{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCaretsShift(APosX, APosY: integer; AShiftX,
  AShiftY: integer; AShiftBelowX: integer);
var
  iStart, i: integer;
  Item: TATCaretItem;
begin
  if APosX<0 then Exit;
  if APosY<0 then Exit;

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
    begin
      if Item.PosX>APosX then
        Inc(Item.PosX, AShiftX);
    end;

    //same, but for EndX/EndY
    if Item.EndY>APosY then
    begin
      if Item.EndY=APosY+1 then
        Inc(Item.EndX, AShiftBelowX);

      Inc(Item.EndY, AShiftY);
    end
    else
    begin
      if Item.EndX>APosX then
        Inc(Item.EndX, AShiftX);
    end;

    if Item.PosX<0 then Item.PosX:= 0;
    if Item.PosY<0 then Item.PosY:= 0;
  end;
end;

function TATSynEdit.DoCommand_TextInsertTab: TATCommandResults;
var
  Str: atString;
begin
  if FOptTabSpaces then
    Str:= StringOfChar(' ', FTabSize)
  else
    Str:= #9;
  Result:= DoCommand_TextInsertAtCarets(Str, false, FOver, false);
end;


function TATSynEdit.DoCommand_TextInsertAtCarets(const AText: atString;
  AKeepCaret, AOvrMode, ASelectThen: boolean): TATCommandResults;
var
  i: integer;
  Item: TATCaretItem;
  Shift, PosAfter: TPoint;
  List: TStringList;
  //
  function TextItem(i: integer): atString;
  begin
    if Assigned(List) and (Carets.Count=List.Count) and (i>=0) and (i<List.Count) then
      Result:= UTF8Decode(List[i])
    else
      Result:= AText;
  end;
  //
begin
  Result:= [];
  if ModeReadOnly then Exit;

  if FOptOverwriteSel then
    DoCommand_TextDeleteSelections
  else
    DoSelect_None;

  List:= nil;
  if (Length(AText)>1) and (Carets.Count>1) then
  begin
    List:= TStringList.Create;
    List.Text:= UTF8Encode(AText);
  end;

  try
    for i:= FCarets.Count-1 downto 0 do
    begin
      Item:= FCarets[i];
      Strings.TextInsert(Item.PosX, Item.PosY, TextItem(i), AOvrMode, Shift, PosAfter);
      DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);

      if not AKeepCaret then
      begin
        if ASelectThen then
        begin
          Item.EndX:= Item.PosX;
          Item.EndY:= Item.PosY;
        end
        else
        begin
          Item.EndX:= -1;
          Item.EndY:= -1;
        end;
        Item.PosX:= PosAfter.X;
        Item.PosY:= PosAfter.Y;
      end;
    end;
  finally
    if Assigned(List) then
      FreeAndNil(List);
  end;

  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_TextDeleteLeft(ALen: integer): TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
  Shift, PosAfter: TPoint;
begin
  Result:= [];
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

function TATSynEdit.DoCommand_TextDelete: TATCommandResults;
begin
  if Carets.IsSelection then
    Result:= DoCommand_TextDeleteSelections
  else
    Result:= DoCommand_TextDeleteRight(1);
end;

function TATSynEdit.DoCommand_TextDeleteSelections: TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
  Shift, PosAfter: TPoint;
  AX1, AY1, AX2, AY2: integer;
  bSel: boolean;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  if not Carets.IsSelection then Exit;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= Carets[i];
    Item.GetRange(AX1, AY1, AX2, AY2, bSel);
    if not bSel then Continue;

    Strings.TextDeleteRange(AX1, AY1, AX2, AY2, Shift, PosAfter);
    DoCaretsShift(AX1, AY1, Shift.X, Shift.Y);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
    Item.EndX:= -1;
    Item.EndY:= -1;
  end;

  Result:= [cResultText, cResultCaretTop];
end;


function TATSynEdit.DoCommand_TextDeleteRight(ALen: integer): TATCommandResults;
var
  Item: TATCaretItem;
  i, Len, ShiftBelowX: integer;
  Shift, PosAfter: TPoint;
begin
  Result:= [];
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
  Item: TATCaretItem;
  Shift, PosAfter: TPoint;
  i: integer;
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];

    Str:= '';
    if FOptAutoIndent then
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

function TATSynEdit.DoCommand_TextDeleteLine: TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
  Shift, PosAfter: TPoint;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Strings.TextDeleteLine(Item.PosX, Item.PosY, Shift, PosAfter);
    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_TextDuplicateLine: TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
  Shift, PosAfter: TPoint;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Strings.TextDuplicateLine(Item.PosX, Item.PosY, Shift, PosAfter);
    DoCaretsShift(Item.PosX, Item.PosY, Shift.X, Shift.Y);
    Item.PosX:= PosAfter.X;
    Item.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDeleteToLineBegin: TATCommandResults;
var
  Item: TATCaretItem;
  Str: atString;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Item:= FCarets[i];
    Str:= Strings.Lines[Item.PosY];
    Delete(Str, 1, Item.PosX);
    Strings.Lines[Item.PosY]:= Str;
    Item.PosX:= 0;
  end;

  Result:= [cResultText, cResultCaretLeft];
end;

function TATSynEdit.DoCommand_TextDeleteToLineEnd: TATCommandResults;
var
  Item: TATCaretItem;
  Str: atString;
  i: integer;
begin
  Result:= [];
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
  Item: TATCaretItem;
  Str: atString;
  Shift, PosAfter: TPoint;
  i: integer;
begin
  Result:= [];
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
      PosAfter.X:= SFindWordOffset(Str, Item.PosX, ANext, false, FOptWordChars);
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


