{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_SelectAll: TATCommandResults;
begin
  DoSelect_All;
  Result:= [cResultCaretBottom];
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

function TATSynEdit.DoCommand_SelectColumn(ADir: TATDirection): TATCommandResults;
var
  Caret: TATCaretItem;
  N: integer;
begin
  Result:= [];
  if Carets.Count=0 then Exit;
  Caret:= Carets[0];

  if IsSelRectEmpty or (FSelRectBegin.X<0) then
  begin
    DoCaretSingleAsIs;
    DoSelect_None;
    FSelRectBegin.X:= SCharPosToColumnPos(Strings.Lines[Caret.PosY], Caret.PosX, OptTabSize);
    FSelRectBegin.Y:= Caret.PosY;
    FSelRectEnd:= FSelRectBegin;
  end;

  case ADir of
    cDirLeft:
      begin
        N:= SColumnPosToCharPos(Strings.Lines[FSelRectEnd.Y], FSelRectEnd.X, OptTabSize);
        if N>0 then Dec(N);
        N:= SCharPosToColumnPos(Strings.Lines[FSelRectEnd.Y], N, OptTabSize);
        FSelRectEnd.X:= N;
        Result:= [cResultKeepColumnSel, cResultCaretLeft];
      end;
    cDirRight:
      begin
        N:= SColumnPosToCharPos(Strings.Lines[FSelRectEnd.Y], FSelRectEnd.X, OptTabSize);
        Inc(N);
        N:= SCharPosToColumnPos(Strings.Lines[FSelRectEnd.Y], N, OptTabSize);
        FSelRectEnd.X:= N;
        Result:= [cResultKeepColumnSel, cResultCaretRight];
      end;
    cDirUp:
      begin
        if FSelRectEnd.Y>0 then
        begin
          Dec(FSelRectEnd.Y);
          FSelRectEnd.Y:= GetNextUnfoldedLineNumber(FSelRectEnd.Y, false);
        end;
        Result:= [cResultKeepColumnSel, cResultCaretTop];
      end;
    cDirDown:
      begin
        if FSelRectEnd.Y<Strings.Count-1 then
        begin
          Inc(FSelRectEnd.Y);
          FSelRectEnd.Y:= GetNextUnfoldedLineNumber(FSelRectEnd.Y, true);
        end;
        Result:= [cResultKeepColumnSel, cResultCaretBottom];
      end;
  end;

  DoSelect_ColumnBlock(FSelRectBegin, FSelRectEnd);
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
    SFindWordBounds(Str, Item.PosX, NOffset1, NOffset2, FOptWordChars);
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


