{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  Item: TATCaretItem;
  i, X1, Y1, X2, Y2: integer;
  bSel, bEnded, bMatch: boolean;
begin
  Result:= false;
  for i:= 0 to Carets.Count-1 do
  begin
    Item:= Carets[i];
    Item.GetRange(X1, Y1, X2, Y2, bSel);
    if not bSel then Continue;

    bEnded:= Y1>AY;
    if bEnded then Exit;

    bMatch:= IsPosInRange(AX, AY, X1, Y1, X2, Y2)=cRelateInside;
    if bMatch then
      begin Result:= true; Break end;
  end;
end;


procedure TATSynEdit.DoSelect_Word(P: TPoint);
var
  N1, N2: integer;
begin
  if not Strings.IsIndexValid(P.Y) then Exit;
  SFindWordBounds(Strings.Lines[P.Y], P.X, N1, N2, FOptWordChars);
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

procedure TATSynEdit.DoSelect_CharRange(ACaretIndex: integer; Pnt: TPoint);
begin
  if not Carets.IsIndexValid(ACaretIndex) then Exit;
  Carets[ACaretIndex].SelectToPoint(Pnt.X, Pnt.Y);
end;

procedure TATSynEdit.DoSelect_WordRange(ACaretIndex: integer; P1, P2: TPoint);
begin
  if not Carets.IsIndexValid(ACaretIndex) then Exit;
  if not Strings.IsIndexValid(P1.Y) then Exit;
  if not Strings.IsIndexValid(P2.Y) then Exit;

  if not IsPosSorted(P1.X, P1.Y, P2.X, P2.Y, true) then
  begin
    SwapInt(P1.X, P2.X);
    SwapInt(P1.Y, P2.Y);
  end;

  P1.X:= SFindWordOffset(Strings.Lines[P1.Y], P1.X, false, false, FOptWordChars);
  P2.X:= SFindWordOffset(Strings.Lines[P2.Y], P2.X, true, false, FOptWordChars);

  with Carets[ACaretIndex] do
  begin
    PosX:= P2.X;
    PosY:= P2.Y;
    EndX:= P1.X;
    EndY:= P1.Y;
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

procedure TATSynEdit.DoSelect_None;
var
  i: integer;
begin
  for i:= 0 to Carets.Count-1 do
    with Carets[i] do
    begin
      EndX:= -1;
      EndY:= -1;
    end;
end;

procedure TATSynEdit.GetSelectedLines(ACaretIndex: integer; out AFrom, ATo: integer);
var
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  AFrom:= -1;
  ATo:= -1;
  if not Carets.IsIndexValid(ACaretIndex) then Exit;

  Carets[ACaretIndex].GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then Exit;

  AFrom:= Y1;
  ATo:= Y2;
  //sel ended at line-start?
  if (X2=0) and (Y2>0) then Dec(ATo);
end;


