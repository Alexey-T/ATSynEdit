{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  Item: TATCaretItem;
  i, X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  Result:= false;

  if not IsSelRectEmpty then
  begin
    //a) faster, b) works w/o mul-carets
    Result:=
      (AY>=FSelRect.Top) and
      (AY<=FSelRect.Bottom) and
      (AX>=FSelRect.Left) and
      (AX<FSelRect.Right);
  end
  else
  for i:= 0 to Carets.Count-1 do
  begin
    Item:= Carets[i];
    Item.GetRange(X1, Y1, X2, Y2, bSel);
    if not bSel then Continue;

    //carets sorted: can stop
    if Y1>AY then Exit;

    if IsPosInRange(AX, AY, X1, Y1, X2, Y2)=cRelateInside then
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
  i: integer;
begin
  if P1.Y>P2.Y then
    SwapInt(P1.Y, P2.Y);

  FSelRect.Left:= Min(P1.X, P2.X);
  FSelRect.Right:= Max(P1.X, P2.X);
  FSelRect.Top:= P1.Y;
  FSelRect.Bottom:= P2.Y;

  for i:= P1.Y to P2.Y do
  begin
    if i=P1.Y then Carets.Clear;
    Carets.Add(0, 0);
    with Carets[Carets.Count-1] do
    begin
      PosX:= P2.X;
      PosY:= i;
      EndX:= P1.X;
      EndY:= i;
    end;
  end;
end;

procedure TATSynEdit.DoPaintSelectedLineBG(C: TCanvas;
  ACharSize: TPoint;
  const AVisRect: TRect;
  APointLeft: TPoint;
  APointText: TPoint;
  ALineIndex: integer;
  AEolSelected: boolean;
  const AScrollHorz: TATSynScrollInfo);
var
  NLeft, NRight: integer;
begin
  if not IsSelRectEmpty then
  begin
    if (ALineIndex>=FSelRect.Top) and (ALineIndex<=FSelRect.Bottom) then
    begin
      NLeft:= APointLeft.X+ACharSize.X*(FSelRect.Left-AScrollHorz.NPos);
      NRight:= NLeft+ACharSize.X*(FSelRect.Right-FSelRect.Left);
      C.Brush.Color:= FColors.TextSelBG;
      C.FillRect(
        NLeft,
        APointLeft.Y,
        NRight,
        APointLeft.Y+ACharSize.Y);
    end;
  end
  else
  if FOptHiliteSelFull then
    if AEolSelected then
    begin
      C.Brush.Color:= FColors.TextSelBG;
      C.FillRect(
        APointText.X,
        APointText.Y,
        AVisRect.Right,
        APointText.Y+ACharSize.Y);
    end;
end;


function TATSynEdit.IsSelRectEmpty: boolean;
begin
  Result:= EqualRect(FSelRect, cRectEmpty);
end;

function TATSynEdit.DoCommand_SelectColumn(ADir: TATDirection): TATCommandResults;
begin
  Result:= [];
  if Carets.Count=0 then Exit;

  if IsSelRectEmpty or (FSelRectBegin.X<0) then
  begin
    DoCaretSingleAsIs;
    DoSelect_None;
    FSelRectBegin.X:= Carets[0].PosX;
    FSelRectBegin.Y:= Carets[0].PosY;
    FSelRectEnd.X:= FSelRectBegin.X;
    FSelRectEnd.Y:= FSelRectBegin.Y;
  end;

  case ADir of
    cDirLeft:
      begin
        if FSelRectEnd.X>0 then Dec(FSelRectEnd.X);
        Result:= [cResultKeepColumnSel, cResultCaretLeft];
      end;
    cDirRight:
      begin
        Inc(FSelRectEnd.X);
        Result:= [cResultKeepColumnSel, cResultCaretRight];
      end;
    cDirUp:
      begin
        if FSelRectEnd.Y>0 then Dec(FSelRectEnd.Y);
        Result:= [cResultKeepColumnSel, cResultCaretTop];
      end;
    cDirDown:
      begin
        if FSelRectEnd.Y<Strings.Count-1 then Inc(FSelRectEnd.Y);
        Result:= [cResultKeepColumnSel, cResultCaretBottom];
      end;
  end;

  DoSelect_ColumnBlock(FSelRectBegin, FSelRectEnd);
end;



function TATSynEdit.DoCommand_MoveSelectionUpDown(ADown: boolean): TATCommandResults;
var
  NFrom, NTo: integer;
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
    if (NTo>=Strings.Count-1) then Exit;
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
    if (NFrom<=0) then Exit;
    Str:= Strings.Lines[NFrom-1];
    Strings.LineDelete(NFrom-1);
    Strings.LineInsert(NTo, Str);

    Dec(Caret.PosY);
    if Caret.EndY>=0 then
      Dec(Caret.EndY);
  end;

  Result:= [cResultText, cResultCaretTop];
end;


