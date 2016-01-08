{$ifdef nnnn}begin end;{$endif}


function TATSynEdit.IsLineWithCaret(ALine: integer): boolean;
begin
  Result:= FCarets.IsLineListed(ALine);
end;

function TATSynEdit.IsLinePartWithCaret(ALine: integer; ACoordY: integer): boolean;
var
  i: integer;
  Caret: TATCaretItem;
  Coord: TPoint;
begin
  Result:= false;
  //like Carets.IsLineListed with more code
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if Caret.PosY=ALine then
    begin
      Coord:= CaretPosToClientPos(Point(Caret.PosX, Caret.PosY));
      if Coord.Y=ACoordY then
      begin
        Result:= true;
        Exit
      end;
    end;
  end;
end;


procedure TATSynEdit.DoCaretAddToPoint(AX, AY: integer);
var
  N: integer;
begin
  N:= Carets.IndexOfPosXY(AX, AY);
  if Carets.IsIndexValid(N) then
  begin
    if Carets.Count>1 then
      Carets.Delete(N);
  end
  else
  begin
    Carets.Add(AX, AY);
  end;
end;

procedure TATSynEdit.DoCaretsColumnToPoint(AX, AY: integer);
var
  P, PM1, PM2: TPoint;
  EolPos: boolean;
begin
  DoCaretSingleAsIs;
  with Carets[0] do
    begin P.X:= PosX; P.Y:= PosY; end;

  PM1:= CaretPosToClientPos(P);
  PM2:= CaretPosToClientPos(Point(AX, AY));

  //clicked above caret?
  if PM2.Y<PM1.Y then
    SwapInt(PM1.Y, PM2.Y);

  Carets.Clear;
  P:= ClientPosToCaretPos(PM1, EolPos);
  if P.Y>=0 then
    Carets.Add(P.X, P.Y);

  repeat
    Inc(PM1.Y, FCharSize.Y);
    P:= ClientPosToCaretPos(PM1, EolPos);
    if P.Y>=0 then
      if not Carets.IsLineListed(P.Y) then
        Carets.Add(P.X, P.Y);
  until PM1.Y>=PM2.Y;

  if Carets.Count=0 then
    Carets.Add(AX, AY);
end;

procedure TATSynEdit.DoCaretsSort;
begin
  Carets.Sort;
end;

procedure TATSynEdit.UpdateCaretsCoords(AOnlyLast: boolean = false);
var
  P: TPoint;
  NStart, i: integer;
  Caret: TATCaretItem;
  Marker: TATMarkerItem;
begin
  if AOnlyLast then
    NStart:= Carets.Count-1
  else
    NStart:= 0;

  for i:= NStart to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    P.X:= Caret.PosX;
    P.Y:= Caret.PosY;
    if IsPosFolded(P.X, P.Y) then
    begin
      Caret.CoordX:= -1;
      Caret.CoordY:= -1;
    end
    else
    begin
      P:= CaretPosToClientPos(P);
      Caret.CoordX:= P.X;
      Caret.CoordY:= P.Y;
    end;
  end;

  for i:= 0 to Markers.Count-1 do
  begin
    Marker:= Markers[i];
    P.X:= Marker.PosX;
    P.Y:= Marker.PosY;
    if IsPosFolded(P.X, P.Y) then
    begin
      Marker.CoordX:= -1;
      Marker.CoordY:= -1;
    end
    else
    begin
      P:= CaretPosToClientPos(P);
      Marker.CoordX:= P.X;
      Marker.CoordY:= P.Y;
    end;
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
  NIndex1, NIndex2, i: integer;
  NFromStart: integer;
  Str: atString;
begin
  Result.X:= -1;
  Result.Y:= -1;

  AWrapInfo.FindIndexesOfLineNumber(P.Y, NIndex1, NIndex2);
  if NIndex1<0 then Exit;

  for i:= NIndex1 to NIndex2 do
  begin
    Item:= AWrapInfo.Items[i];

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
  AVirtualPos: boolean;
  out AWrappedEnd: boolean): TPoint;
var
  NPixels, NIndex: integer;
  Item: TATSynWrapItem;
  Str: atString;
  AllowVirtual: boolean;
begin
  AWrappedEnd:= false;
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
      Result.X:= Item.NCharIndex+Item.NLength-1;
    end;
  end
  else
  //click in text
  begin
    Item:= AWrapInfo.Items[NIndex];
    Result.Y:= Item.NLineIndex;

    Str:= Copy(AStrings.Lines[Result.Y], Item.NCharIndex, Item.NLength);

    AllowVirtual:= AVirtualPos and (Item.NFinal=cWrapItemFinal);
    NPixels:= P.X-ARect.Left + ACharSize.X*(AScrollHorz.NPos-Item.NIndent);
    Result.X:= SFindClickedPosition(Str, NPixels, ACharSize.X, ATabSize, AllowVirtual, AWrappedEnd) + Item.NCharIndex - 2;

    if Item.NFinal=cWrapItemFinal then //don't set AWrappedEnd for real eol
      AWrappedEnd:= false;
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
    FCaretSpecPos or FOptCaretPreferLeftSide
    );
end;

function TATSynEdit.ClientPosToCaretPos(P: TPoint; out AEndOfLinePos: boolean): TPoint;
begin
  Result:= _DoClientPosToCaretPos(P,
    FWrapInfo,
    Strings,
    FCharSize,
    FTabSize,
    FRectMain,
    FScrollHorz,
    FScrollVert,
    FCaretVirtual,
    AEndOfLinePos);
end;

procedure TATSynEdit.SetCaretShapeIns(AValue: TATSynCaretShape);
begin
  if FCaretShapeIns=AValue then Exit;
  DoPaintModeStatic;
  FCaretShapeIns:= AValue;
  DoPaintModeBlinking;
end;

procedure TATSynEdit.SetCaretShapeOvr(AValue: TATSynCaretShape);
begin
  if FCaretShapeOvr=AValue then Exit;
  DoPaintModeStatic;
  FCaretShapeOvr:= AValue;
  DoPaintModeBlinking;
end;

procedure TATSynEdit.SetCaretShapeRO(AValue: TATSynCaretShape);
begin
  if FCaretShapeRO=AValue then Exit;
  DoPaintModeStatic;
  FCaretShapeRO:= AValue;
  DoPaintModeBlinking;
end;

procedure TATSynEdit.SetCaretBlinkEnabled(AValue: boolean);
begin
  if FCaretBlinkEnabled=AValue then Exit;
  FCaretBlinkEnabled:= AValue;
  DoPaintModeStatic;
  DoPaintModeBlinking;
end;


procedure TATSynEdit.DoGotoPos(APnt: TPoint; AIndentHorz, AIndentVert: integer);
var
  NIndex, NVisLines, NChars: integer;
begin
  if IsPosFolded(APnt.X, APnt.Y) then Exit;

  NVisLines:= GetVisibleLines;
  APnt:= CaretPosToClientPos(APnt);
  NIndex:= GetWrapInfoIndex(APnt);
  if NIndex<0 then Exit;

  //for y
  //negative AIndentVert: indent always from top
  //positive: from top (goto up) or bottom (goto down)
  if (NIndex<FScrollVert.NPos) then
  begin
    FScrollVert.NPos:= Max(0, NIndex-Abs(AIndentVert));
    UpdateScrollbars;
  end
  else
  if (NIndex>FScrollVert.NPos+NVisLines-1) then
  begin
    if AIndentVert<0 then
      FScrollVert.NPos:= Max(0, NIndex-Abs(AIndentVert))
    else
      FScrollVert.NPos:= Max(0, NIndex-NVisLines+1+Abs(AIndentVert));
    UpdateScrollbars;
  end;

  //for x
  if APnt.X<FRectMain.Left then
  begin
    NChars:= (FRectMain.Left-APnt.X) div FCharSize.X + 1 + AIndentHorz;
    FScrollHorz.NPos:= Max(0, FScrollHorz.NPos-NChars);
    UpdateScrollbars;
  end
  else
  if APnt.X>FRectMain.Right-FCharSize.X then
  begin
    NChars:= (APnt.X-FRectMain.Right) div FCharSize.X + 2 + AIndentHorz;
    Inc(FScrollHorz.NPos, NChars);
    UpdateScrollbars;
  end;

  UpdateCaretsCoords();
end;

procedure TATSynEdit.DoGotoCaret(AEdge: TATCaretEdge);
begin
  DoGotoPos(Carets.CaretAtEdge(AEdge), cScrollIndentCaretHorz, cScrollIndentCaretVert);
end;

procedure TATSynEdit.DoGotoPos_AndUnfold(APnt: TPoint;
  AIndentHorz, AIndentVert: integer);
begin
  if not Strings.IsIndexValid(APnt.Y) then Exit;

  if IsLineFolded(APnt.Y, true) then
  begin
    DoUnfoldLine(APnt.Y);
    Update;
  end;

  DoCaretSingle(APnt.X, APnt.Y);
  DoEventCarets;
  DoGotoPos(APnt, AIndentHorz, AIndentVert);
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
  FSelRect:= cRectEmpty;

  if Carets.Count=0 then
    Carets.Add(0, 0);

  while Carets.Count>1 do
    Carets.Delete(Carets.Count-1);
end;


procedure TATSynEdit.DoCaretSingle(APosX, APosY, AEndX, AEndY: integer; AUseEndXY: boolean);
begin
  FSelRect:= cRectEmpty;

  if Carets.Count=0 then
    Carets.Add(0, 0);

  while Carets.Count>1 do
    Carets.Delete(Carets.Count-1);

  with Carets[0] do
  begin
    PosX:= APosX;
    PosY:= APosY;
    if AUseEndXY then
    begin
      EndX:= AEndX;
      EndY:= AEndY;
    end;
  end;
end;

procedure TATSynEdit.DoCaretSingle(AX, AY: integer; AClearSelection: boolean);
begin
  DoCaretSingle(AX, AY, -1, -1, AClearSelection);
end;

function TATSynEdit.GetCaretSelectionIndex(P: TPoint): integer;
var
  Item: TATCaretItem;
  X1, Y1, X2, Y2, i: integer;
  bSel: boolean;
begin
  Result:= -1;
  for i:= 0 to Carets.Count-1 do
  begin
    Item:= Carets[i];
    Item.GetRange(X1, Y1, X2, Y2, bSel);
    if not bSel then Continue;
    if IsPosInRange(P.X, P.Y, X1, Y1, X2, Y2)=cRelateInside then
    begin
      Result:= i;
      Break
    end;
  end;
end;

function TATSynEdit.DoCaretSwapEdge(AMoveLeft: boolean): boolean;
var
  Item: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  bSel, bAtLeft: boolean;
begin
  Result:= false;
  if Carets.Count<>1 then Exit;

  Item:= Carets[0];
  Item.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then Exit;
  Result:= true;

  bAtLeft:= IsPosSorted(Item.PosX, Item.PosY, Item.EndX, Item.EndY, true);

  //Left/Rt pressed at left/rt side of selection?
  //yes: cancel selection, don't move caret
  if bAtLeft=AMoveLeft then
  begin
    Item.EndX:= -1;
    Item.EndY:= -1;
    Exit
  end;

  //else swap edge
  SwapInt(Item.PosX, Item.EndX);
  SwapInt(Item.PosY, Item.EndY);

  if not FOptKeyLeftRightSwapSelAndSelect then
  begin
    Item.EndX:= -1;
    Item.EndY:= -1;
  end;
end;

function TATSynEdit.GetCaretsArray: TATPointArray;
begin
  if Assigned(FCarets) then
    Result:= FCarets.SaveToArray;
end;

procedure TATSynEdit.SetCaretsArray(const L: TATPointArray);
begin
  if Assigned(FCarets) then
    FCarets.LoadFromArray(L);
end;

procedure TATSynEdit.DoCaretsExtend(ADown: boolean; ALines: integer);
var
  MoreCarets: TATCarets;
  X, Y: integer;
  i, j: integer;
begin
  MoreCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
      with Carets[i] do
      begin
        for j:= 1 to ALines do
        begin
          X:= PosX;
          Y:= PosY+BoolToPlusMinusOne(ADown)*j;
          if (Y<0) or (Y>=Strings.Count) then Break;
          MoreCarets.Add(X, Y);
        end;
      end;

    for i:= 0 to MoreCarets.Count-1 do
      with MoreCarets[i] do
        Carets.Add(PosX, PosY);
    Carets.Sort;
  finally
    FreeAndNil(MoreCarets);
  end;
end;

procedure TATSynEdit.DoCaretsAssign(NewCarets: TATCarets);
begin
  Carets.Clear;
  if NewCarets.Count>0 then
    Carets.Assign(NewCarets)
  else
    DoCaretSingle(0, 0);
end;

function TATSynEdit.IsCaretBlocked: boolean;
begin
  Result:= FCaretStopUnfocused and not Focused;
end;

procedure TATSynEdit.UpdateIncorrectCaretPositions;
begin
  Carets.UpdateIncorrectPositions(Strings.Count-1);
end;


procedure TATSynEdit.DoCaretsShift_CaretItem(Caret: TATCaretItem;
  APosX, APosY, AShiftX, AShiftY, AShiftBelowX: integer);
begin
  //carets below src, apply ShiftY/ShiftBelowX
  if Caret.PosY>APosY then
  begin
    if AShiftY=0 then exit;

    if Caret.PosY=APosY+1 then
      Inc(Caret.PosX, AShiftBelowX);

    Inc(Caret.PosY, AShiftY);
  end
  else
  //carets on same line as src, apply ShiftX
  begin
    if Caret.PosX>APosX then
      Inc(Caret.PosX, AShiftX);
  end;

  //same, but for EndX/EndY
  if Caret.EndY>APosY then
  begin
    if Caret.EndY=APosY+1 then
      Inc(Caret.EndX, AShiftBelowX);

    Inc(Caret.EndY, AShiftY);
  end
  else
  begin
    if Caret.EndX>APosX then
      Inc(Caret.EndX, AShiftX);
  end;

  if Caret.PosX<0 then Caret.PosX:= 0;
  if Caret.PosY<0 then Caret.PosY:= 0;
end;


procedure TATSynEdit.DoCaretsShift_MarkerItem(Mark: TATMarkerItem;
  APosX, APosY, AShiftX, AShiftY, AShiftBelowX: integer;
  APosAfter: TPoint);
begin
  //marker below src, apply ShiftY/ShiftBelowX
  if Mark.PosY>APosY then
  begin
    if AShiftY=0 then exit;

    if Mark.PosY=APosY+1 then
      Inc(Mark.PosX, AShiftBelowX);

    Inc(Mark.PosY, AShiftY);
  end
  else
  //marker on same line as src
  if Mark.PosY=APosY then
  begin
    if Mark.PosX=APosX then
    begin
      Mark.PosX:= APosAfter.X;
      Mark.PosY:= APosAfter.Y;
    end
    else
    if Mark.PosX>=APosX then
      if AShiftY=0 then
        Inc(Mark.PosX, AShiftX)
      else
      begin
        Inc(Mark.PosX, -APosX+APosAfter.X);
        Inc(Mark.PosY, AShiftY);
      end;
  end;

  if Mark.PosX<0 then Mark.PosX:= 0;
  if Mark.PosY<0 then Mark.PosY:= 0;
end;


procedure TATSynEdit.DoCaretsShift(APosX, APosY: integer; AShiftX,
  AShiftY: integer; APosAfter: TPoint; AShiftBelowX: integer);
var
  NStart, i: integer;
  {$ifdef debug_markers_shift}
  S: string;
  {$endif}
begin
  if APosX<0 then Exit;
  if APosY<0 then Exit;

  //adjust carets
  //(optimized, from index NStart, to fast do on 200 carets)
  NStart:= Carets.IndexOfPosYAvg(APosY);
  if NStart>=0 then
    for i:= NStart to Carets.Count-1 do
      DoCaretsShift_CaretItem(Carets[i],
        APosX, APosY, AShiftX, AShiftY, AShiftBelowX);

  {$ifdef debug_markers_shift}
  S:= '';
  for i:= 0 to Markers.Count-1 do
    S:= S+Format('mark[%d] %d %d, ', [i, Markers[i].PosX, Markers[i].PosY]);
  Application.Mainform.Caption:= S+' -- '+Format(
    'pos %d %d, shift %d %d, posafter %d %d',
    [APosX, APosY, AShiftX, AShiftY, APosAfter.X, APosAfter.Y]);
  {$endif}

  //adjust markers
  //(cannot optimize, markers not sorted)
  for i:= 0 to Markers.Count-1 do
    DoCaretsShift_MarkerItem(Markers[i],
      APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);

  for i:= 0 to Attribs.Count-1 do
    DoCaretsShift_MarkerItem(Attribs[i],
      APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);

  for i:= 0 to FMarkedRange.Count-1 do
    DoCaretsShift_MarkerItem(FMarkedRange[i],
      APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);
end;

