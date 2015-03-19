{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  i: integer;
  Item: TATCaretItem;
  bEnded, bMatch: boolean;
begin
  Result:= false;
  for i:= 0 to Carets.Count-1 do
  begin
    Item:= Carets[i];
    if (Item.EndX<0) or (Item.EndY<0) then Continue;
    if (Item.PosX=Item.EndX) and (Item.PosY=Item.EndY) then Continue;

    bEnded:= Min(Item.PosY, Item.EndY)>AY;
    if bEnded then Exit;

    bMatch:=
      (IsPosSorted(Item.PosX, Item.PosY, AX, AY, true) and IsPosSorted(AX, AY, Item.EndX, Item.EndY, false)) or
      (IsPosSorted(Item.EndX, Item.EndY, AX, AY, true) and IsPosSorted(AX, AY, Item.PosX, Item.PosY, false));

    if bMatch then
      begin Result:= true; Break end;
  end;
end;


procedure TATSynEdit.DoSelect_Word(P: TPoint);
var
  N1, N2: integer;
begin
  if not Strings.IsIndexValid(P.Y) then Exit;
  SFindWordBounds(Strings.Lines[P.Y], P.X, N1, N2);
  if N1<>N2 then
  begin
    DoCaretSingle(P.X, P.Y);
    with Carets[0] do
    begin
      EndY:= P.Y;
      EndX:= N1;
      PosX:= N2;
    end;
  end;
end;

procedure TATSynEdit.DoSelect_Line(P: TPoint);
var
  PLast: TPoint;
begin
  if not Strings.IsIndexValid(P.Y) then Exit;

  DoCaretSingle(P.X, P.Y);
  with Carets[0] do
  begin
    if P.Y<Strings.Count-1 then
    begin
      PosX:= 0;
      PosY:= P.Y+1;
    end
    else
    begin
      PLast:= GetLastPos;
      PosX:= PLast.X;
      PosY:= PLast.Y;
    end;
    EndX:= 0;
    EndY:= P.Y;
  end;
end;

procedure TATSynEdit.DoSelect_All;
var
  P: TPoint;
begin
  P:= GetLastPos;
  DoCaretSingle(P.X, P.Y);
  with Carets[0] do
  begin
    EndX:= 0;
    EndY:= 0;
  end;
end;

procedure TATSynEdit.DoSelect_Lines_ToPoint(ALineFrom: integer; P: TPoint);
var
  CItem: TATCaretItem;
begin
  DoCaretSingle(P.X, P.Y);
  CItem:= Carets[0];

  if P.Y<ALineFrom then
  begin
    CItem.EndX:= 0;
    CItem.EndY:= ALineFrom+1;
  end
  else
  if P.Y>ALineFrom then
  begin
    CItem.EndX:= 0;
    CItem.EndY:= ALineFrom;
  end
  else
  if P.Y=ALineFrom then
  begin
    DoSelect_Line(P);
  end;
end;

