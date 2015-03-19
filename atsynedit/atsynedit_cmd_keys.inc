{$ifdef nnnn}begin end;{$endif}

function TATSynEdit.DoCommand_KeyHome: TATCommandResults;
var
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
    FCarets[i].PosX:= 0;

  Result:= [cResultCaretLeft];
end;

function TATSynEdit.DoCommand_KeyEnd: TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    Item.PosX:= Length(Strings.Lines[Item.PosY]);
  end;

  Result:= [cResultCaretRight];
end;

function TATSynEdit.DoCommand_KeyLeft: TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    if (Item.PosX>0) then
      Dec(Item.PosX)
    else
    if (Item.PosY>0) and not FCaretVirtual then
    begin
      Dec(Item.PosY);
      Item.PosX:= Length(Strings.Lines[Item.PosY]);
    end;
  end;

  Result:= [cResultCaretLeft];
end;

function TATSynEdit.DoCommand_KeyRight: TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    if (Item.PosX<Length(Strings.Lines[Item.PosY])) or FCaretVirtual then
      Inc(Item.PosX)
    else
    if (Item.PosY<Strings.Count-1) then
    begin
      Item.PosX:= 0;
      Inc(Item.PosY);
    end;
  end;

  Result:= [cResultCaretRight];
end;

function TATSynEdit.DoCommand_KeyUpDown(ADown: boolean; ALines: integer): TATCommandResults;
begin
  FCaretSpecPos:= true;
  if FKeyNavigateInWrappedLines then
    Result:= DoCommand_KeyUpDown_Wrapped(ADown, ALines)
  else
    Result:= DoCommand_KeyUpDown_NextLine(ADown, ALines);
end;

function TATSynEdit.DoCommand_KeyUpDown_NextLine(ADown: boolean; ALines: integer): TATCommandResults;
var
  Item: TATCaretItem;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    Item.PosY:= Max(0, Min(Strings.Count-1, Item.PosY + ALines*BoolToPlusMinusOne(ADown)));
  end;

  if ADown then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_KeyUpDown_Wrapped(ADown: boolean; ALines: integer): TATCommandResults;
var
  Item: TATCaretItem;
  Pnt: TPoint;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    Pnt.X:= Item.CoordX;
    Pnt.Y:= Item.CoordY + ALines*FCharSize.Y*BoolToPlusMinusOne(ADown);
    Pnt:= ClientPosToCaretPos(Pnt);
    if Pnt.Y<0 then Continue;
    Item.PosX:= Pnt.X;
    Item.PosY:= Pnt.Y;
  end;

  if ADown then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_GotoTextBegin: TATCommandResults;
begin
  DoCaretSingle(0, 0);

  FScrollHorz.NPos:= 0;
  FScrollVert.NPos:= 0;

  Result:= [cResultCaretTop];
end;

function TATSynEdit.GetLastPos: TPoint;
begin
  Result.X:= 0;
  Result.Y:= 0;
  if Strings.Count>0 then
  begin
    Result.Y:= Strings.Count-1;
    Result.X:= Length(Strings.Lines[Result.Y]);
    if Strings.LinesEnds[Result.Y]<>cLineEndNone then
      Inc(Result.X);
  end;
end;

function TATSynEdit.DoCommand_GotoTextEnd: TATCommandResults;
var
  P: TPoint;
begin
  P:= GetLastPos;
  DoCaretSingle(P.X, P.Y);

  Result:= [cResultCaretBottom];
end;

function TATSynEdit.DoCommand_ScrollVert(ALines: integer): TATCommandResults;
begin
  DoScrollByDelta(0, ALines);
  Result:= [cResultScroll];
end;

function TATSynEdit.DoCommand_GotoWord(ANext: boolean): TATCommandResults;
var
  Item: TATCaretItem;
  Str: atString;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    if not Strings.IsIndexValid(Item.PosY) then Continue;
    Str:= Strings.Lines[Item.PosY];

    //jump to prev line?
    if (Item.PosX=0) and (not ANext) then
    begin
      if Item.PosY>0 then
      begin
        Dec(Item.PosY);
        Item.PosX:= Length(Strings.Lines[Item.PosY]);
      end;
    end
    else
    //jump to next line?
    if (Item.PosX>=Length(Str)) and ANext then
    begin
      if Item.PosY<Strings.Count-1 then
      begin
        Inc(Item.PosY);
        Item.PosX:= SGetIndentChars(Strings.Lines[Item.PosY]);
      end;
    end
    else
    //jump from beyond eol to eol?
    if (Item.PosX>Length(Str)) and (not ANext) then
    begin
      Item.PosX:= Length(Str);
    end
    else
    //jump inside line?
    if (Item.PosX<=Length(Str)) then
    begin
      Item.PosX:= SFindWordOffset(Str, Item.PosX, ANext, true);
    end;
  end;

  if ANext then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_CaretsRemove: TATCommandResults;
begin
  while Carets.Count>1 do
    Carets.Delete(Carets.Count-1);

  Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_SelectAll: TATCommandResults;
begin
  DoSelect_All;
  Result:= [cResultCaretBottom];
end;

