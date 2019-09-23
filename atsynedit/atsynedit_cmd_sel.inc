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

function TATSynEdit.DoCommand_SelectColumn(ADir: TATSelectColumnDirection): TATCommandResults;
var
  PntBegin, PntEnd: TPoint;
  N, NPageLines, i: integer;
begin
  Result:= [];
  if Carets.Count=0 then Exit;

  if IsSelRectEmpty or (FSelRectBegin.X<0) then
    if not DoSelect_MultiCaretsToColumnSel then
    begin
      DoSelect_NormalSelToColumnSel(PntBegin, PntEnd);
      DoCaretSingleAsIs;
      DoSelect_None;
      FSelRectBegin:= PntBegin;
      FSelRectEnd:= PntEnd;
    end;

  if ADir in [cDirColumnPageUp, cDirColumnPageDown] then
    NPageLines:= GetPageLines
  else
    NPageLines:= 1;

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
    cDirColumnPageUp:
      begin
        for i:= 1 to NPageLines do
          if FSelRectEnd.Y>0 then
          begin
            Dec(FSelRectEnd.Y);
            FSelRectEnd.Y:= GetNextUnfoldedLineNumber(FSelRectEnd.Y, false);
          end;

        if FSelRectEnd.Y>=FSelRectBegin.Y then
          Result:= [cResultKeepColumnSel, cResultCaretBottom]
        else
          Result:= [cResultKeepColumnSel, cResultCaretTop];
      end;
    cDirColumnDown,
    cDirColumnPageDown:
      begin
        for i:= 1 to NPageLines do
          if FSelRectEnd.Y<Strings.Count-1 then
          begin
            Inc(FSelRectEnd.Y);
            FSelRectEnd.Y:= GetNextUnfoldedLineNumber(FSelRectEnd.Y, true);
          end;

        if FSelRectEnd.Y>=FSelRectBegin.Y then
          Result:= [cResultKeepColumnSel, cResultCaretBottom]
        else
          Result:= [cResultKeepColumnSel, cResultCaretTop];
      end;
  end;

  DoSelect_ColumnBlock_FromSelRect;
end;

function TATSynEdit.DoCommand_SelectColumnToLineEdge(AToEnd: boolean): TATCommandResults;
var
  NPos, i: integer;
begin
  Result:= [];
  if Carets.Count=0 then Exit;

  if IsSelRectEmpty or (FSelRectBegin.X<0) then Exit;
  //maybe todo: make column sel if normal sel active here
  //like DoCommand_SelectColumn

  if not AToEnd then
  begin
    FSelRectEnd.X:= 0;
  end
  else
  begin
    NPos:= 0;
    for i:= FSelRectBegin.Y to FSelRectEnd.Y do
      if Strings.IsIndexValid(i) then
        NPos:= Max(NPos, FTabHelper.TabsToSpaces_Length(i, Strings.Lines[i], -1));
    FSelRectEnd.X:= NPos;
  end;

  DoSelect_ColumnBlock_FromSelRect;
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
  Str: atString;
  Pnt: TPoint;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  DoCaretSingleAsIs;
  Caret:= Carets[0];
  Caret.GetSelLines(NFrom, NTo);
  if NFrom<0 then
  begin
    NFrom:= Caret.PosY;
    NTo:= NFrom;
  end;

 try
  Strings.EnabledBookmarksUpdate:= false;

  if ADown then
  begin
    NLastLine:= Strings.Count-1;
    if Strings.IsLastLineFake then
      Dec(NLastLine);
    if NTo>=NLastLine then Exit;

    Str:= Strings.Lines[NTo+1];
    Strings.LineDelete(NTo+1);
    Strings.LineInsert(NFrom, Str);

    Inc(Caret.PosY);
    if Caret.EndY>=0 then
      Inc(Caret.EndY);

    //correct caret if out of file
    if Caret.PosY>=Strings.Count then
    begin
      Pnt:= GetEndOfFilePos;
      Caret.PosX:= Pnt.X;
      Caret.PosY:= Pnt.Y;
    end;
  end
  else
  begin
    if NFrom<=0 then Exit;
    Str:= Strings.Lines[NFrom-1];
    Strings.LineDelete(NFrom-1);
    Strings.LineInsert(NTo, Str);

    Dec(Caret.PosY);
    if Caret.EndY>=0 then
      Dec(Caret.EndY);
  end;

 finally
   Strings.EnabledBookmarksUpdate:= true;
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

