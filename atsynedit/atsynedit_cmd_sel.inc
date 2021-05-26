{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_SelectAll: TATCommandResults;
begin
  DoSelect_All;
  Result:= [cResultCaretAny];
  //don't return [cResultCaretBottom] - this scrolls to file end,
  //while Sublime & EmEditor & GEdit keep scroll position after "select all"
end;

function TATSynEdit.DoCommand_SelectInverted: TATCommandResults;
begin
  DoSelect_Inverted;
  Result:= [cResultCaretBottom];
end;

function TATSynEdit.DoCommand_SelectSplitToLines: TATCommandResults;
begin
  DoSelect_SplitSelectionToLines;
  Result:= [cResultCaretBottom];
end;

function TATSynEdit.DoCommand_SelectExtendByLine: TATCommandResults;
begin
  DoSelect_ExtendSelectionByLine;
  Result:= [cResultCaretBottom];
end;

procedure TATSynEdit.DoSelect_ColumnBlock_MoveEndUpDown(var AX, AY: integer; ALineDelta: integer);
var
  bDown: boolean;
  YMax, i: integer;
  Pnt: TPoint;
  Details: TATEditorPosDetails;
begin
  if AY<0 then exit;
  bDown:= ALineDelta>0;
  YMax:= Strings.Count-1;

  if FOptCaretsPrimitiveColumnSelection then
  begin
    AX:= Strings.ColumnPosToCharPos(AY, AX, FTabHelper);
    Pnt:= CaretPosToClientPos(Point(AX, AY));
    Inc(Pnt.Y, ALineDelta*FCharSize.Y);
    Pnt:= ClientPosToCaretPos(Pnt, Details);
    if Pnt.Y<0 then exit;
    AX:= Pnt.X;
    AY:= Pnt.Y;
    AX:= Strings.CharPosToColumnPos(AY, AX, FTabHelper);
  end
  else
  begin
    for i:= 1 to Abs(ALineDelta) do
    begin
      if bDown then
      begin
        Inc(AY);
        if AY>YMax then
        begin
          AY:= YMax;
          Break;
        end;
      end
      else
      begin
        Dec(AY);
        if AY<0 then
        begin
          AY:= 0;
          Break;
        end;
      end;
      AY:= Unfolded_NextLineNumber(AY, bDown);
    end;
  end;
end;

function TATSynEdit.DoCommand_SelectColumnToDirection(ADir: TATEditorSelectColumnDirection): TATCommandResults;
var
  N, NPageLines: integer;
  bDown: boolean;
begin
  Result:= [];
  if Carets.Count=0 then Exit;
  FLastCommandMakesColumnSel:= true;

  //it's needed to Undo to multi-carets, not one caret
  Strings.SetGroupMark;

  if IsSelRectEmpty and (FSelRectEnd.Y<0) then
    if not DoSelect_MultiCaretsLookLikeColumnSelection then
    begin
      DoSelect_NormalSelToColumnSel(FSelRectBegin, FSelRectEnd);
      DoCaretSingleAsIs;
      DoSelect_None;
    end;

  if FSelRectEnd.Y<0 then Exit;

  if ADir in [cDirColumnPageUp, cDirColumnPageDown] then
    NPageLines:= GetPageLines
  else
    NPageLines:= 1;

  bDown:= ADir in [cDirColumnDown, cDirColumnPageDown];
  if not bDown then
    NPageLines:= -NPageLines;

  //make correct FSelRectEnd, w/o making any carets
  case ADir of
    cDirColumnLeft:
      begin
        N:= Strings.ColumnPosToCharPos(FSelRectEnd.Y, FSelRectEnd.X, FTabHelper);
        if N>0 then Dec(N);
        N:= Strings.CharPosToColumnPos(FSelRectEnd.Y, N, FTabHelper);
        FSelRectEnd.X:= N;
        Result:= [cResultKeepColumnSel, cResultCaretLeft];
      end;

    cDirColumnRight:
      begin
        N:= Strings.ColumnPosToCharPos(FSelRectEnd.Y, FSelRectEnd.X, FTabHelper);
        Inc(N);
        N:= Strings.CharPosToColumnPos(FSelRectEnd.Y, N, FTabHelper);
        FSelRectEnd.X:= N;
        Result:= [cResultKeepColumnSel, cResultCaretRight];
      end;

    cDirColumnUp,
    cDirColumnPageUp,
    cDirColumnDown,
    cDirColumnPageDown:
      begin
        DoSelect_ColumnBlock_MoveEndUpDown(FSelRectEnd.X, FSelRectEnd.Y, NPageLines);

        if FSelRectEnd.Y>=FSelRectBegin.Y then
          Result:= [cResultKeepColumnSel, cResultCaretBottom]
        else
          Result:= [cResultKeepColumnSel, cResultCaretTop];
      end;
  end;

  //this makes multi-carets from FSelRectBegin/FSelRectEnd
  //and considers OptCaretsPrimitiveColumnSelection
  DoSelect_ColumnBlock_FromPointsColumns(FSelRectBegin, FSelRectEnd);

  //DebugSelRect; ////debug
end;

function TATSynEdit.DoCommand_SelectColumnToLineEdge(AToEnd: boolean): TATCommandResults;
var
  NPos, i, Y1, Y2: integer;
begin
  Result:= [];
  if Carets.Count=0 then Exit;

  if IsSelRectEmpty or (FSelRectBegin.X<0) then Exit;
  //maybe todo: make column sel if normal sel active here
  //like DoCommand_SelectColumnToDirection

  if not AToEnd then
  begin
    FSelRectEnd.X:= 0;
  end
  else
  begin
    NPos:= 0;
    Y1:= FSelRectBegin.Y;
    Y2:= FSelRectEnd.Y;
    if Y1>Y2 then
      SwapInt(Y1, Y2);
    for i:= Y1 to Y2 do
      if Strings.IsIndexValid(i) then
        NPos:= Max(NPos, FTabHelper.TabsToSpaces_Length(i, Strings.Lines[i], -1));
    FSelRectEnd.X:= NPos;
  end;

  DoSelect_ColumnBlock_FromPointsColumns(FSelRectBegin, FSelRectEnd);
  Result:= [cResultKeepColumnSel, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_RemoveOneCaret(AFirstCaret: boolean): TATCommandResults;
begin
  if Carets.Count>1 then
  begin
    if AFirstCaret then
      Carets.Delete(0)
    else
      Carets.Delete(Carets.Count-1);
    Result:= [cResultCaretAny];
  end
  else
    Result:= [];
end;

function TATSynEdit.DoCommand_MoveSelectionUpDown(ADown: boolean): TATCommandResults;
var
  NFrom, NTo, NLastLine: integer;
  Caret: TATCaretItem;
  St: TATStrings;
  S: atString;
  Pnt: TPoint;
  bLastNoEol: boolean;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  St:= Strings;

  DoCaretSingleAsIs;
  Caret:= Carets[0];
  Caret.GetSelLines(NFrom, NTo);
  if NFrom<0 then
  begin
    NFrom:= Caret.PosY;
    NTo:= NFrom;
  end;

  try
    St.BeginUndoGroup;
    St.EnabledBookmarksUpdate:= false;
    NLastLine:= St.Count-1;

    if ADown then
    //move lines down
    begin
      if NTo>=NLastLine then Exit;
      bLastNoEol:= (NTo+1=NLastLine) and (St.LinesEnds[NLastLine]=cEndNone);

      {
      S:= St.Lines[NTo+1];
      St.LineDelete(NTo+1);
      St.LineInsert(NFrom, S);
      }
      St.LineMove(NTo+1, NFrom);

      if bLastNoEol then
        St.ActionDeleteFakeLineAndFinalEol;

      Inc(Caret.PosY);
      if Caret.EndY>=0 then
        Inc(Caret.EndY);

      //correct caret if out of file
      if Caret.PosY>=St.Count then
      begin
        Pnt:= GetEndOfFilePos;
        Caret.PosX:= Pnt.X;
        Caret.PosY:= Pnt.Y;
      end;
    end
    else
    //move lines up
    begin
      if NFrom<=0 then Exit;
      bLastNoEol:= (NTo=NLastLine) and (St.LinesEnds[NTo]=cEndNone);

      if bLastNoEol then
        St.LinesEnds[NTo]:= St.Endings;

      {
      S:= St.Lines[NFrom-1];
      St.LineDelete(NFrom-1);
      St.LineInsert(NTo, S);
      }
      St.LineMove(NFrom-1, NTo);

      if bLastNoEol then
        St.ActionDeleteFakeLineAndFinalEol;

      Dec(Caret.PosY);
      if Caret.EndY>=0 then
        Dec(Caret.EndY);
    end;

  finally
    St.EndUndoGroup;
    St.EnabledBookmarksUpdate:= true;
  end;

  Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_SelectWords: TATCommandResults;
var
  Item: TATCaretItem;
  i, NOffset1, NOffset2: integer;
  Str: atString;
begin
  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    if not Strings.IsIndexValid(Item.PosY) then Continue;

    Str:= Strings.Lines[Item.PosY];
    SFindWordBounds(Str, Item.PosX, NOffset1, NOffset2, FOptNonWordChars);
    if NOffset1<>NOffset2 then
    begin
      Item.EndY:= Item.PosY;
      Item.EndX:= NOffset1;
      Item.PosX:= NOffset2;
    end;
  end;

  Result:= [cResultCaretAny];
end;

function TATSynEdit.DoCommand_SelectLines: TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
  Str: atString;
begin
  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    if not Strings.IsIndexValid(Item.PosY) then Continue;

    Str:= Strings.Lines[Item.PosY];
    Item.EndY:= Item.PosY;
    Item.EndX:= 0;
    Item.PosX:= Length(Str);
  end;

  Result:= [cResultCaretAny];
end;

function TATSynEdit.DoCommand_ColumnSelectWithoutKey(AValue: boolean): TATCommandResults;
begin
  FOptMouseColumnSelectionWithoutKey:= AValue;
  Result:= [cResultState];
end;

