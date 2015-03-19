{$ifdef nnnn}begin end;{$endif}

procedure TATSynEdit.DoCaretAddToPoint(Pnt: TPoint);
var
  N: integer;
begin
  N:= Carets.IndexOfPosXY(Pnt.X, Pnt.Y);
  if Carets.IsIndexValid(N) then
  begin
    if Carets.Count>1 then
      Carets.Delete(N);
  end
  else
    Carets.Add(Pnt.X, Pnt.Y);
end;

procedure TATSynEdit.DoCaretsColumnToPoint(const PntTo: TPoint);
var
  P, PM1, PM2: TPoint;
begin
  if Carets.Count=0 then Exit;
  with Carets[0] do
    P:= Point(PosX, PosY);

  PM1:= CaretPosToClientPos(P);
  PM2:= CaretPosToClientPos(PntTo);

  //clicked above caret
  if PM2.Y<PM1.Y then
  begin
    P.Y:= PM1.Y;
    PM1.Y:= PM2.Y;
    PM2.Y:= P.Y;
  end;

  Carets.Clear;
  P:= ClientPosToCaretPos(PM1);
  if P.Y>=0 then
    Carets.Add(P.X, P.Y);

  repeat
    Inc(PM1.Y, FCharSize.Y);
    P:= ClientPosToCaretPos(PM1);
    if P.Y>=0 then
      if not Carets.IsLineListed(P.Y) then
        Carets.Add(P.X, P.Y);
  until PM1.Y>=PM2.Y;

  if Carets.Count=0 then
    Carets.Add(PntTo.X, PntTo.Y);
end;

procedure TATSynEdit.DoCaretsSort;
begin
  Carets.Sort;
end;

procedure TATSynEdit.UpdateCaretsCoords(AOnlyLast: boolean = false);
var
  P: TPoint;
  NStart, i: integer;
  Item: TATSynCaretItem;
begin
  if AOnlyLast then
    NStart:= Carets.Count-1
  else
    NStart:= 0;

  for i:= NStart to Carets.Count-1 do
  begin
    Item:= Carets[i];
    P.X:= Item.PosX;
    P.Y:= Item.PosY;
    P:= CaretPosToClientPos(P);
    Item.CoordX:= P.X;
    Item.CoordY:= P.Y;
  end;
end;

function _DoCaretPosToClientPos(
  P: TPoint;
  AWrapInfo: TATSynWrapInfo;
  AStrings: TATStrings;
  ACharSize: TPoint;
  ATabSize: integer;
  const ARect: TRect;
  const AScrollHorz, AScrollVert: TATSynScrollInfo;
  APreferLeftSide: boolean): TPoint;
var
  Item: TATSynWrapItem;
  i, NFromStart: integer;
  Str: atString;
begin
  Result.X:= -1;
  Result.Y:= -1;

  for i:= 0 to AWrapInfo.Count-1 do
  begin
    Item:= AWrapInfo.Items[i];
    if Item.NLineIndex<P.Y then Continue;
    if Item.NLineIndex>P.Y then Break;

    if (P.X<Item.NCharIndex-1) then Continue;
    if (Item.NFinal=cWrapItemMiddle) then
      if (P.X>Item.NCharIndex-1+Item.NLength) or
        ((P.X=Item.NCharIndex-1+Item.NLength) and APreferLeftSide) then
        Continue;

    NFromStart:= P.X+1-Item.NCharIndex;
    Str:= Copy(AStrings.Lines[P.Y], Item.NCharIndex, Min(NFromStart, Item.NLength));

    Result.X:= CanvasTextWidth(Str, ATabSize, ACharSize);
    if NFromStart>Item.NLength then
      Inc(Result.X, (NFromStart-Item.NLength)*ACharSize.X);

    Inc(Result.X, (Item.NIndent-AScrollHorz.NPos)*ACharSize.X);
    Result.Y:= (i-AScrollVert.NPos)*ACharSize.Y;

    Inc(Result.X, ARect.Left);
    Inc(Result.Y, ARect.Top);
    Exit
  end;
end;

function _DoClientPosToCaretPos(
  P: TPoint;
  AWrapInfo: TATSynWrapInfo;
  AStrings: TATStrings;
  ACharSize: TPoint;
  ATabSize: integer;
  const ARect: TRect;
  const AScrollHorz, AScrollVert: TATSynScrollInfo;
  ACaretVirtualPos: boolean): TPoint;
var
  NPixels, NIndex: integer;
  Item: TATSynWrapItem;
  Str: atString;
  AllowVirtual: boolean;
begin
  Result.X:= 0;
  Result.Y:= -1;
  if (ACharSize.X<=0) or (ACharSize.Y<=0) then Exit;

  P.X:= Max(P.X, ARect.Left);
  NIndex:= (P.Y-ARect.Top) div ACharSize.Y + AScrollVert.NPos;

  if NIndex<0 then
  //click above all text
  begin
    Result.X:= 0;
    Result.Y:= 0;
  end
  else
  if not AWrapInfo.IsIndexValid(NIndex) then
  //click below all text
  begin
    NIndex:= AWrapInfo.Count-1;
    if AWrapInfo.IsIndexValid(NIndex) then
    begin
      Item:= AWrapInfo.Items[NIndex];
      Result.Y:= Item.NLineIndex;
      NIndex:= Item.NCharIndex+Item.NLength-1;
      Result.X:= NIndex;
    end;
  end
  else
  //click in text
  begin
    Item:= AWrapInfo.Items[NIndex];
    Result.Y:= Item.NLineIndex;

    Str:= Copy(AStrings.Lines[Result.Y], Item.NCharIndex, Item.NLength);

    AllowVirtual:= ACaretVirtualPos and (Item.NFinal=cWrapItemFinal);
    NPixels:= P.X-ARect.Left + ACharSize.X*(AScrollHorz.NPos-Item.NIndent);
    NIndex:= SFindClickedPosition(Str, NPixels, ACharSize.X, ATabSize, AllowVirtual) + Item.NCharIndex - 2;
    Result.X:= NIndex;
  end;
end;

function TATSynEdit.CaretPosToClientPos(P: TPoint): TPoint;
begin
  Result:= _DoCaretPosToClientPos(P,
    FWrapInfo,
    Strings,
    FCharSize,
    FTabSize,
    FRectMain,
    FScrollHorz,
    FScrollVert,
    FCaretSpecPos
    );
end;

function TATSynEdit.ClientPosToCaretPos(P: TPoint): TPoint;
begin
  Result:= _DoClientPosToCaretPos(P,
    FWrapInfo,
    Strings,
    FCharSize,
    FTabSize,
    FRectMain,
    FScrollHorz,
    FScrollVert,
    FCaretVirtual);
end;

procedure TATSynEdit.SetCaretShape(AValue: TATSynCaretShape);
begin
  if FCaretShape=AValue then Exit;
  DoPaintModeStatic;
  FCaretShape:= AValue;
  DoPaintModeBlinking;
end;

procedure TATSynEdit.SetCaretShapeOvr(AValue: TATSynCaretShape);
begin
  if FCaretShapeOvr=AValue then Exit;
  DoPaintModeStatic;
  FCaretShapeOvr:= AValue;
  DoPaintModeBlinking;
end;

procedure TATSynEdit.DoShowPos(APnt: TPoint; AIndentHorz, AIndentVert: integer);
var
  NIndex, NVisLines, NChars: integer;
begin
  NVisLines:= GetVisibleLines;
  APnt:= CaretPosToClientPos(APnt);
  NIndex:= GetWrapInfoIndex(APnt);
  if NIndex<0 then Exit;

  //for y
  if NIndex<FScrollVert.NPos then
  begin
    FScrollVert.NPos:= Max(0, NIndex-AIndentVert);
    UpdateScrollbars(true);
  end
  else
  if NIndex>FScrollVert.NPos+NVisLines-1 then
  begin
    FScrollVert.NPos:= Max(0, NIndex-NVisLines+1+AIndentVert);
    UpdateScrollbars(true);
  end;

  //for x
  if APnt.X<FRectMain.Left then
  begin
    NChars:= (FRectMain.Left-APnt.X) div FCharSize.X + 1 + AIndentHorz;
    FScrollHorz.NPos:= Max(0, FScrollHorz.NPos-NChars);
    UpdateScrollbars(true);
  end
  else
  if APnt.X>FRectMain.Right-FCharSize.X then
  begin
    NChars:= (APnt.X-FRectMain.Right) div FCharSize.X + 2 + AIndentHorz;
    Inc(FScrollHorz.NPos, NChars);
    UpdateScrollbars(true);
  end;
end;

procedure TATSynEdit.DoShowCaret(AEdge: TATSynCaretEdge);
begin
  DoShowPos(Carets.CaretAtEdge(AEdge), cSizeScrollCaretHorz, cSizeScrollCaretVert);
end;

procedure TATSynEdit.DoGotoPos(APnt: TPoint);
begin
  DoCommandExec(cCommand_CaretsRemove);
  Carets[0].PosX:= APnt.X;
  Carets[0].PosY:= APnt.Y;

  if Strings.LinesHidden[APnt.Y]<>0 then //collapsed
  begin
    Strings.LinesHidden[APnt.Y]:= 0; //uncollapse
    FWrapUpdateNeeded:= true;
    UpdateWrapInfo;
  end;

  DoShowPos(APnt, cSizeScrollGotoHorz, cSizeScrollGotoVert);
  Update;
end;

procedure TATSynEdit.DoCaretsDeleteOnSameLines;
var
  i: integer;
begin
  for i:= Carets.Count-1 downto 1{!} do
  begin
    if Carets[i].PosY=Carets[i-1].PosY then
      Carets.Delete(i);
  end;
end;

procedure TATSynEdit.DoCaretSingleAsIs;
begin
  if Carets.Count=0 then
    Carets.Add(0, 0);

  while Carets.Count>1 do
    Carets.Delete(Carets.Count-1);
end;

procedure TATSynEdit.DoCaretSingle(AX, AY: integer);
begin
  if Carets.Count=0 then
    Carets.Add(AX, AY);

  while Carets.Count>1 do
    Carets.Delete(Carets.Count-1);

  Carets[0].PosX:= AX;
  Carets[0].PosY:= AY;
end;

