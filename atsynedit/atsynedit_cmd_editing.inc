{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCaretsShift(APosX, APosY: integer; AShiftX,
  AShiftY: integer; AShiftBelowX: integer);
var
  iStart, i: integer;
  Item: TATSynCaretItem;
begin
  iStart:= FCarets.IndexOfPosYAvg(APosY);
  if iStart<0 then Exit;

  for i:= iStart to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    //carets below src, apply ShiftY/ShiftBelowX
    if Item.PosY>APosY then
    begin
      if AShiftY=0 then Exit;

      if Item.PosY=APosY+1 then
        Inc(Item.PosX, AShiftBelowX);

      Inc(Item.PosY, AShiftY);
    end
    else
    //carets on same line as src, apply ShiftX
    if Item.PosX>APosX then
    begin
      Inc(Item.PosX, AShiftX);
    end;
    if Item.PosX<0 then Item.PosX:= 0;
    if Item.PosY<0 then Item.PosY:= 0;
  end;
end;

function TATSynEdit.DoCommand_TextInsertTab: TATCommandResults;
var
  Str: atString;
begin
  if FTextTabSpaces then
    Str:= StringOfChar(' ', FTabSize)
  else
    Str:= #9;
  Result:= DoCommand_TextInsertAtCarets(Str, false, FOvrMode);
end;


function TATSynEdit.DoCommand_TextInsertAtCarets(const AText: atString;
  AKeepCaret, AOvrMode: boolean): TATCommandResults;
var
  i: integer;
  Item: TATSynCaretItem;
  Shift, PosAfter: TPoint;
begin
  if ModeReadOnly then Exit;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Strings.TextInsert(Item.PosX, Item.PosY, AText, AOvrMode, Shift, PosAfter);
    if not AKeepCaret then
    begin
      DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);
      Item.PosX:= PosAfter.X;
      Item.PosY:= PosAfter.Y;
    end;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDeleteLeft(ALen: integer): TATCommandResults;
var
  Item: TATSynCaretItem;
  i: integer;
  Shift, PosAfter: TPoint;
begin
  if ModeReadOnly then Exit;

  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    Strings.TextDeleteLeft(Item.PosX, Item.PosY, ALen, Shift, PosAfter);
    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDeleteRight(ALen: integer): TATCommandResults;
var
  Item: TATSynCaretItem;
  i, Len, ShiftBelowX: integer;
  Shift, PosAfter: TPoint;
begin
  if ModeReadOnly then Exit;

  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];

    //offsetX for carets in line[PosY+1]
    ShiftBelowX:= 0;
    Len:= Length(Strings.Lines[Item.PosY]);
    if Item.PosX=Len then
      ShiftBelowX:= Len;

    Strings.TextDeleteRight(Item.PosX, Item.PosY, ALen, Shift, PosAfter);
    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y, ShiftBelowX);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextInsertEol(AKeepCaret: boolean): TATCommandResults;
var
  Item: TATSynCaretItem;
  Shift, PosAfter: TPoint;
  i: integer;
  Str: atString;
begin
  if ModeReadOnly then Exit;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];

    Str:= '';
    if FTextAutoIndent then
    begin
      Str:= Strings.Lines[Item.PosY];
      Str:= Copy(Str, 1, Min(Item.PosX, SGetIndentChars(Str)));
    end;

    Strings.TextInsertEol(Item.PosX, Item.PosY, AKeepCaret, Str, Shift, PosAfter);
    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_TextDeleteCurLine: TATCommandResults;
var
  Item: TATSynCaretItem;
  i: integer;
  Shift, PosAfter: TPoint;
begin
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Strings.TextDeleteCurLine(Item.PosX, Item.PosY, Shift, PosAfter);
    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_TextDuplicateCurLine: TATCommandResults;
var
  Item: TATSynCaretItem;
  i: integer;
  Shift, PosAfter: TPoint;
begin
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Strings.TextDuplicateCurLine(Item.PosX, Item.PosY, Shift, PosAfter);
    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDeleteToLineBegin: TATCommandResults;
var
  Item: TATSynCaretItem;
  Str: atString;
  i: integer;
begin
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Str:= Strings.Lines[Item.PosY];
    if Item.PosX<Length(Str) then
    begin
      Delete(Str, 1, Item.PosX);
      Strings.Lines[Item.PosY]:= Str;
      Item.PosX:= 0;
    end;
  end;

  Result:= [cResultText, cResultCaretLeft];
end;

function TATSynEdit.DoCommand_TextDeleteToLineEnd: TATCommandResults;
var
  Item: TATSynCaretItem;
  Str: atString;
  i: integer;
begin
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Str:= Strings.Lines[Item.PosY];
    if Item.PosX<Length(Str) then
    begin
      Delete(Str, Item.PosX+1, MaxInt);
      Strings.Lines[Item.PosY]:= Str;
    end;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDeleteWord(ANext: boolean): TATCommandResults;
//
//todo- delete duplicate carets on same words before....
var
  Item: TATSynCaretItem;
  Str: atString;
  Shift, PosAfter: TPoint;
  i: integer;
begin
  if ModeReadOnly then Exit;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    if not Strings.IsIndexValid(Item.PosY) then Continue;
    Str:= Strings.Lines[Item.PosY];
    Shift.X:= 0;
    Shift.Y:= 0;
    PosAfter.X:= Item.PosX;
    PosAfter.Y:= Item.PosY;

    //delete to prev line?
    if (Item.PosX=0) and (not ANext) then
    begin
      Strings.TextDeleteLeft(Item.PosX, Item.PosY, 1, Shift, PosAfter);
    end
    else
    //delete to next line?
    if (Item.PosX>=Length(Str)) and ANext then
    begin
      Strings.TextDeleteRight(Item.PosX, Item.PosY, 1, Shift, PosAfter);
    end
    else
    //jump from beyond eol to eol?
    if (Item.PosX>Length(Str)) and (not ANext) then
    begin
      Item.PosX:= Length(Str);
    end
    else
    //delete inside line?
    if (Item.PosX<=Length(Str)) then
    begin
      PosAfter.X:= SFindWordOffset(Str, Item.PosX, ANext, false);
      if PosAfter.X<>Item.PosX then
      begin
        System.Delete(Str, Min(Item.PosX, PosAfter.X)+1, Abs(Item.PosX-PosAfter.X));
        Strings.Lines[Item.PosY]:= Str;
        Shift.X:= -Abs(Item.PosX-PosAfter.X);
        PosAfter.X:= Min(Item.PosX, PosAfter.X);
      end;
    end;

    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);

    if ((Item.PosX<>PosAfter.X) or (Item.PosY<>PosAfter.Y)) and
      (FCarets.IndexOfPosXY(PosAfter.X, PosAfter.Y)>=0) then
    begin
      if FCarets.Count>1 then
        FCarets.Delete(i);
    end
    else
    begin
      Item.PosX:= PosAfter.X;
      Item.PosY:= PosAfter.Y;
    end;
  end;

  if ANext then
    Result:= [cResultText, cResultCaretBottom]
  else
    Result:= [cResultText, cResultCaretTop];
end;


