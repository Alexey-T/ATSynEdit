{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_SelectAll: TATCommandResults;
begin
  DoSelect_All;
  Result:= [TATCommandResult.CaretAny];
  //don't return [TATCommandResult.CaretBottom] - this scrolls to file end,
  //while Sublime & EmEditor & GEdit keep scroll position after "select all"
end;

function TATSynEdit.DoCommand_SelectInverted: TATCommandResults;
begin
  DoSelect_Inverted;
  Result:= [TATCommandResult.CaretBottom];
end;

function TATSynEdit.DoCommand_SelectSplitToLines: TATCommandResults;
begin
  DoSelect_SplitSelectionToLines;
  Result:= [TATCommandResult.CaretBottom];
end;

function TATSynEdit.DoCommand_SelectExtendByLine(AUp: boolean): TATCommandResults;
begin
  DoSelect_ExtendSelectionByLine(AUp);
  if AUp then
    Result:= [TATCommandResult.CaretTop]
  else
    Result:= [TATCommandResult.CaretBottom];
end;

procedure TATSynEdit.DoSelect_ColumnBlock_MoveEndUpDown(var AX, AY: integer; ALineDelta: integer);
var
  St: TATStrings;
  bDown: boolean;
  YMax, i: integer;
  PntText: TPoint;
  PntCoord: TATPoint;
  Details: TATEditorPosDetails;
begin
  if AY<0 then exit;
  St:= Strings;
  bDown:= ALineDelta>0;
  YMax:= St.Count-1;

  if not bDown then
  begin
    if AY=0 then exit;
  end
  else
  begin
    if AY>=YMax then exit;
  end;

  if FOptCaretsPrimitiveColumnSelection then
  begin
    AX:= St.ColumnPosToCharPos(AY, AX, FTabHelper);
    PntCoord:= CaretPosToClientPos(Point(AX, AY));
    Inc(PntCoord.Y, ALineDelta*FCharSize.Y);
    PntText:= ClientPosToCaretPos(PntCoord, Details);
    if PntText.Y<0 then exit;
    AX:= PntText.X;
    AY:= PntText.Y;
    AX:= St.CharPosToColumnPos(AY, AX, FTabHelper);
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
  bDown, bHadSelection: boolean;
begin
  Result:= [];
  if Carets.Count=0 then Exit;
  FLastCommandMakesColumnSel:= true;

  //it's needed to Undo to multi-carets, not one caret
  Strings.SetGroupMark;

  if not IsSelColumn then
    if not DoSelect_MultiCaretsLookLikeColumnSelection then
    begin
      DoSelect_NormalSelToColumnSel(FSelRectBegin, FSelRectEnd);
      DoCaretSingleAsIs;
      DoSelect_None;
    end;

  if FSelRectEnd.Y<0 then Exit;

  if ADir in [TATEditorSelectColumnDirection.PageUp, TATEditorSelectColumnDirection.PageDown] then
    NPageLines:= GetPageLines
  else
    NPageLines:= 1;

  bDown:= ADir in [TATEditorSelectColumnDirection.Down, TATEditorSelectColumnDirection.PageDown];
  if not bDown then
    NPageLines:= -NPageLines;

  bHadSelection:= Carets.IsSelection;

  //make correct FSelRectEnd, w/o making any carets
  case ADir of
    TATEditorSelectColumnDirection.Left:
      begin
        N:= Strings.ColumnPosToCharPos(FSelRectEnd.Y, FSelRectEnd.X, FTabHelper);
        if N>0 then Dec(N);
        N:= Strings.CharPosToColumnPos(FSelRectEnd.Y, N, FTabHelper);
        FSelRectEnd.X:= N;
        Result:= [TATCommandResult.KeepColumnSel, TATCommandResult.CaretLeft];
      end;

    TATEditorSelectColumnDirection.Right:
      begin
        N:= Strings.ColumnPosToCharPos(FSelRectEnd.Y, FSelRectEnd.X, FTabHelper);
        Inc(N);
        N:= Strings.CharPosToColumnPos(FSelRectEnd.Y, N, FTabHelper);
        FSelRectEnd.X:= N;
        Result:= [TATCommandResult.KeepColumnSel, TATCommandResult.CaretRight];
      end;

    TATEditorSelectColumnDirection.Up,
    TATEditorSelectColumnDirection.PageUp,
    TATEditorSelectColumnDirection.Down,
    TATEditorSelectColumnDirection.PageDown:
      begin
        DoSelect_ColumnBlock_MoveEndUpDown(FSelRectEnd.X, FSelRectEnd.Y, NPageLines);

        if FSelRectEnd.Y>=FSelRectBegin.Y then
          Result:= [TATCommandResult.KeepColumnSel, TATCommandResult.CaretBottom]
        else
          Result:= [TATCommandResult.KeepColumnSel, TATCommandResult.CaretTop];
      end;
  end;

  //this makes multi-carets from FSelRectBegin/FSelRectEnd
  //and considers OptCaretsPrimitiveColumnSelection
  DoSelect_ColumnBlock_FromPointsColumns(FSelRectBegin, FSelRectEnd);

  //DoSelect_ColumnBlock_xxxx call for the case "primitive column selection"
  //can make selection for the new caret (part of one long wrapped line).
  //when we do "extend carets line up" and go through long wrapped line.
  //this selection is very bad looking. let's avoid it.
  if ADir in [
      TATEditorSelectColumnDirection.Up,
      TATEditorSelectColumnDirection.PageUp,
      TATEditorSelectColumnDirection.Down,
      TATEditorSelectColumnDirection.PageDown] then
    if OptCaretsPrimitiveColumnSelection and not bHadSelection and Carets.IsSelection then
      DoSelect_None;

  //DebugSelRect; ////debug
end;

function TATSynEdit.DoCommand_SelectColumnToLineEdge(AToEnd: boolean): TATCommandResults;
var
  St: TATStrings;
  NPos, i, Y1, Y2: integer;
begin
  Result:= [];
  if Carets.Count=0 then Exit;

  if not IsSelColumn then Exit;
  //maybe todo: make column sel if normal sel active here
  //like DoCommand_SelectColumnToDirection

  St:= Strings;

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
      if St.IsIndexValid(i) then
        NPos:= Max(NPos, FTabHelper.TabsToSpaces_Length(i, St.Lines[i], -1));
    FSelRectEnd.X:= NPos;
  end;

  DoSelect_ColumnBlock_FromPointsColumns(FSelRectBegin, FSelRectEnd);
  Result:= [TATCommandResult.KeepColumnSel, TATCommandResult.CaretBottom];
end;

function TATSynEdit.DoCommand_SelectFoldingRangeAtCaret: TATCommandResults;
begin
  Result:= [];
  if DoSelect_FoldingRangeAtCaret then
    Result:= [TATCommandResult.CaretAny];
end;

function TATSynEdit.DoCommand_RemoveOneCaret(AFirstCaret: boolean): TATCommandResults;
begin
  if Carets.Count>1 then
  begin
    if AFirstCaret then
      Carets.Delete(0)
    else
      Carets.Delete(Carets.Count-1);
    Result:= [TATCommandResult.CaretAny];
  end
  else
    Result:= [];
end;

function TATSynEdit.DoCommand_MoveSelectionUpDown(ADown: boolean): TATCommandResults;
var
  St: TATStrings;
  Caret: TATCaretItem;
  Pnt: TPoint;
  NFrom, NTo, NLastLine, iCaret, iLine: integer;
  bLastNoEol: boolean;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  if ModeOneLine then Exit;
  St:= Strings;

  DoCaretsDeleteOnSameLines;

  St.BeginUndoGroup;
  St.EnabledBookmarksUpdate:= false;

  try
    for iCaret:= Carets.Count-1 downto 0 do
    begin
      Caret:= Carets[iCaret];
      Caret.GetSelLines(NFrom, NTo);
      if NFrom<0 then
      begin
        NFrom:= Caret.PosY;
        NTo:= NFrom;
      end;

      NLastLine:= St.Count-1;
      if St.IsLastLineFake then
        NLastLine:= Max(0, NLastLine-1); //to fix Undo cannot undo moving line down from fake-like

      if ADown then
      //move lines down
      begin
        if NTo>=NLastLine then Continue;
        bLastNoEol:= (NTo+1=NLastLine) and (St.LinesEnds[NLastLine]=TATLineEnds.None);

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
        if NFrom<=0 then Break;
        bLastNoEol:= (NTo=NLastLine) and (St.LinesEnds[NTo]=TATLineEnds.None);

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

      //make it like in Sublime
      for iLine:= IfThen(ADown, NFrom+1, NFrom) to IfThen(ADown, NTo, NTo-1) do
        St.LinesState[iLine]:= TATLineState.Changed;
    end;
  finally
    St.EndUndoGroup;
    St.EnabledBookmarksUpdate:= true;
  end;

  Result:= [TATCommandResult.Text, TATCommandResult.CaretTop];
end;

function TATSynEdit.DoCommand_SelectWords: TATCommandResults;
var
  St: TATStrings;
  Item: TATCaretItem;
  i, NOffset1, NOffset2: integer;
  Str: atString;
begin
  St:= Strings;
  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    if not St.IsIndexValid(Item.PosY) then Continue;

    Str:= St.Lines[Item.PosY];
    SFindWordBounds(Str, Item.PosX, NOffset1, NOffset2, FOptNonWordChars);
    if NOffset1<>NOffset2 then
    begin
      Item.EndY:= Item.PosY;
      Item.EndX:= NOffset1;
      Item.PosX:= NOffset2;
    end;
  end;

  Result:= [TATCommandResult.CaretAny];
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

  Result:= [TATCommandResult.CaretAny];
end;

function TATSynEdit.DoCommand_ColumnSelectWithoutKey(AValue: boolean): TATCommandResults;
begin
  FOptMouseColumnSelectionWithoutKey:= AValue;
  Result:= [TATCommandResult.State];
end;

