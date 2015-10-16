{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  NPosLeft,
  NPosRight: integer;
  SLine: atString;
begin
  if not IsSelRectEmpty then
  begin
    if not ((AY>=FSelRect.Top) and (AY<=FSelRect.Bottom)) then exit(False);

    SLine:= Strings.Lines[AY];
    NPosLeft:= SColumnPosToCharPos(SLine, FSelRectColumnLeft, OptTabSize);
    NPosRight:= SColumnPosToCharPos(SLine, FSelRectColumnRight, OptTabSize);
    Result:= (AX>=NPosLeft) and (AX<NPosRight);
  end
  else
    Result:= Carets.IsPosSelected(AX, AY);
end;

function TATSynEdit.IsSelRectEmpty: boolean;
begin
  Result:= EqualRect(FSelRect, cRectEmpty);
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
      PLast:= GetEndOfFilePos;
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
  P:= GetEndOfFilePos;
  DoCaretSingle(P.X, P.Y);
  with Carets[0] do
  begin
    EndX:= 0;
    EndY:= 0;
  end;
end;

procedure TATSynEdit.DoSelect_Inverted;
var
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  XPrev, YPrev: integer;
  i: integer;
  Sel: boolean;
  PosLast: TPoint;
begin
  XPrev:= 0;
  YPrev:= 0;
  NewCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Carets[i].GetRange(X1, Y1, X2, Y2, Sel);
      if not Sel then Continue;

      //add range
      NewCarets.Add(XPrev, YPrev, X1, Y1);
      XPrev:= X2;
      YPrev:= Y2;
    end;

    //add range after last caret
    PosLast:= GetEndOfFilePos;
    NewCarets.Add(XPrev, YPrev, PosLast.X, PosLast.Y);

    DoCaretsAssign(NewCarets);
  finally
    FreeAndNil(NewCarets);
  end;
end;

procedure TATSynEdit.DoSelect_SplitSelectionToLines;
var
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  i, j: integer;
  Sel: boolean;
begin
  NewCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Carets[i].GetRange(X1, Y1, X2, Y2, Sel);
      if not Sel then
      begin
        NewCarets.Add(X1, Y1);
        Continue;
      end;

      if Y1=Y2 then
      begin
        NewCarets.Add(X1, Y1, X2, Y2);
        Continue;
      end;

      //add first part
      if X1<Length(Strings.Lines[Y1]) then
        NewCarets.Add(X1, Y1, Length(Strings.Lines[Y1]), Y1)
      else
        NewCarets.Add(X1, Y1);

      //add middle parts
      for j:= Y1+1 to Y2-1 do
      begin
        if Strings.Lines[j]='' then
          NewCarets.Add(0, j)
        else
          NewCarets.Add(0, j, Length(Strings.Lines[j]), j);
      end;

      //add last part
      NewCarets.Add(0, Y2, X2, Y2);
    end;

    DoCaretsAssign(NewCarets);
  finally
    FreeAndNil(NewCarets);
  end;
end;

procedure TATSynEdit.DoSelect_ExtendSelectionByLine;
var
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  i: integer;
  Sel: boolean;
  PosLast: TPoint;
begin
  NewCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Carets[i].GetRange(X1, Y1, X2, Y2, Sel);
      if not Sel then
        begin X2:= X1; Y2:= Y1; end;

      X1:= 0; //select entire 1st line
      if Y2<Strings.Count-1 then
      begin
        //select till start of next ln
        X2:= 0;
        Y2:= Y2+1;
      end
      else
      begin
        //select till eof
        PosLast:= GetEndOfFilePos;
        X2:= PosLast.X;
        Y2:= PosLast.Y;
      end;

      NewCarets.Add(X1, Y1, X2, Y2);
    end;

    DoCaretsAssign(NewCarets);
  finally
    FreeAndNil(NewCarets);
  end;
end;

procedure TATSynEdit.DoSelect_LineRange(ALineFrom: integer; P: TPoint);
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
  FSelRect:= cRectEmpty;
  FSelRectBegin:= Point(-1, -1);

  for i:= 0 to Carets.Count-1 do
    with Carets[i] do
    begin
      EndX:= -1;
      EndY:= -1;
    end;
end;

procedure TATSynEdit.DoSelect_ColumnBlock(P1, P2: TPoint);
var
  NPosLeft, NPosRight: integer;
  i: integer;
begin
  if P1.Y>P2.Y then
    SwapInt(P1.Y, P2.Y);

  FSelRect.Left:= Min(P1.X, P2.X);
  FSelRect.Right:= Max(P1.X, P2.X);
  FSelRect.Top:= P1.Y;
  FSelRect.Bottom:= P2.Y;

  FSelRectColumnLeft:= SCharPosToColumnPos(Strings.Lines[FSelRect.Top], FSelRect.Left, OptTabSize);
  FSelRectColumnRight:= SCharPosToColumnPos(Strings.Lines[FSelRect.Bottom], FSelRect.Right, OptTabSize);

  for i:= P1.Y to P2.Y do
  begin
    if i=P1.Y then Carets.Clear;
    Carets.Add(0, 0);
    with Carets[Carets.Count-1] do
    begin
      PosX:= SColumnPosToCharPos(Strings.Lines[i], FSelRectColumnRight, OptTabSize);
      PosY:= i;
      EndX:= SColumnPosToCharPos(Strings.Lines[i], FSelRectColumnLeft, OptTabSize);
      EndY:= i;
    end;
  end;
end;

procedure TATSynEdit.DoSelectionDeleteOrReset;
begin
  if FOptOverwriteSel then
    DoCommand_TextDeleteSelection
  else
    DoSelect_None;
end;


