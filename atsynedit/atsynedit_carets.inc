{$ifdef nnnn}begin end;{$endif}

function TATSynEdit.IsLineWithCaret(ALine: integer): boolean; inline;
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
        exit(true);
    end;
  end;
end;


procedure TATSynEdit.DoCaretsAddOnColumnBlock(APos1, APos2: TPoint; const ARect: TRect);
var
  i: integer;
begin
  for i:= APos1.Y to APos2.Y do
  begin
    if i=APos1.Y then
      Carets.Clear;
    Carets.Add(
      Strings.ColumnPosToCharPos(i, ARect.Right, FTabHelper),
      i,
      Strings.ColumnPosToCharPos(i, ARect.Left, FTabHelper),
      i
      );
  end
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
  Details: TATPosDetails;
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
  P:= ClientPosToCaretPos(PM1, Details);
  if P.Y>=0 then
    Carets.Add(P.X, P.Y);

  repeat
    Inc(PM1.Y, FCharSize.Y);
    P:= ClientPosToCaretPos(PM1, Details);
    if P.Y>=0 then
      if not Carets.IsLineListed(P.Y) then
        Carets.Add(P.X, P.Y);
  until PM1.Y>=PM2.Y;

  if Carets.Count=0 then
    Carets.Add(AX, AY);
end;

procedure TATSynEdit.DoCaretsSort; inline;
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
    Markers[i]:= Marker;
  end;
end;


function _DoCaretPosToClientPos(
  P: TPoint;
  AWrapInfo: TATSynWrapInfo;
  AStrings: TATStrings;
  AGaps: TATSynGaps;
  ACharSize: TPoint;
  ATabHelper: TATStringTabHelper;
  const ARect: TRect;
  const AScrollHorz, AScrollVert: TATSynScrollInfo;
  APreferLeftSide: boolean): TPoint;
var
  Item: TATSynWrapItem;
  StringItem: PATStringItem;
  NIndex1, NIndex2, NLineIndexTop, NFromStart, NWidth: integer;
  Str: atString;
  bAsciiOnly: boolean;
  i: integer;
begin
  Result.X:= -1;
  Result.Y:= -1;

  AWrapInfo.FindIndexesOfLineNumber(P.Y, NIndex1, NIndex2);
  if NIndex1<0 then Exit;

  //prepare optimization
  AStrings.UpdateItemHasTab(P.Y);
  StringItem:= AStrings.GetItemPtr(P.Y);
  bAsciiOnly:= TATLineHasAscii(StringItem^.Ex.HasAsciiOnly) = cLineAsciiYes;

  for i:= NIndex1 to NIndex2 do
  begin
    Item:= AWrapInfo[i];

    if (P.X<Item.NCharIndex-1) then Continue;
    if (Item.NFinal=cWrapItemMiddle) then
      if (P.X>Item.NCharIndex-1+Item.NLength) or
        ((P.X=Item.NCharIndex-1+Item.NLength) and APreferLeftSide) then
        Continue;

    NFromStart:= P.X+1-Item.NCharIndex;

    if bAsciiOnly then
    begin
      NWidth:= Min(NFromStart, Item.NLength) * ACharSize.X;
    end
    else
    begin
      Str:= AStrings.LineSub(P.Y, Item.NCharIndex, Min(NFromStart, Item.NLength));
      NWidth:= CanvasTextWidth(Str, P.Y, ATabHelper, ACharSize);
    end;

    Result.X:= NWidth;

    if NFromStart>Item.NLength then
      Inc(Result.X, (NFromStart-Item.NLength)*ACharSize.X);

    //TODO: here scrolled part is taken as small chars (*ACharSize.X),
    //while text may have full-width chars, and it gives incorrect result
    Inc(Result.X, (Item.NIndent-AScrollHorz.NPos)*ACharSize.X);
    Result.Y:= (i-AScrollVert.NPos)*ACharSize.Y;

    //consider gaps
    if AWrapInfo.IsIndexValid(AScrollVert.NPos) then
    begin
      NLineIndexTop:= AWrapInfo[AScrollVert.NPos].NLineIndex;
      Inc(Result.Y, AGaps.SizeForLineRange(
        NLineIndexTop, //index of top line
        P.Y-1 //index of line before caret
        ));
    end;

    //coords of corner
    Inc(Result.X, ARect.Left);
    Inc(Result.Y, ARect.Top);
    Exit
  end;
end;


function _DoClientPosToCaretPos(
  P: TPoint;
  AWrapInfo: TATSynWrapInfo;
  AStrings: TATStrings;
  AGaps: TATSynGaps;
  ACharSize: TPoint;
  ATabHelper: TATStringTabHelper;
  const ARect: TRect;
  const AScrollHorz, AScrollVert: TATSynScrollInfo;
  AVirtualPos: boolean;
  out ADetails: TATPosDetails;
  AGapCoordAction: TATGapCoordAction;
  AAfterEofUsesLastLen: boolean): TPoint;
var
  NIndex, NIndexDecrement, NPixels: integer;
  Item: TATSynWrapItem;
  ItemGap: TATSynGapItem;
  Str: atString;
  AllowVirtual, bGapGoesToEnd: boolean;
begin
  Result.X:= 0;
  Result.Y:= -1;
  FillChar(ADetails, SizeOf(ADetails), 0);

  if (ACharSize.X<=0) or (ACharSize.Y<=0) then Exit;
  P.X:= Max(P.X, ARect.Left);

  //find index in WrapInfo for P.Y
  NPixels:= ARect.Top;
  NIndex:= AScrollVert.NPos;
  bGapGoesToEnd:= false;

  //allow to work for arrow-up/ page-up, if caret at top
  if P.Y<ARect.Top then
    if AWrapInfo.IsIndexValid(NIndex) then
    begin
      //start later loop, but from decremented NIndex
      //todo: this not correct if upper (not visible) lines have gaps...
      NIndexDecrement:= Min((ARect.Top-P.Y) div ACharSize.Y + 1, NIndex);
      Dec(NIndex, NIndexDecrement);
      Dec(NPixels, NIndexDecrement*ACharSize.Y);
    end;

  repeat
    if not AWrapInfo.IsIndexValid(NIndex) then
      Break;

    Inc(NPixels, ACharSize.Y);
    if NPixels>P.Y then Break;

    //consider gap
    Item:= AWrapInfo[NIndex];
    if Item.NFinal=cWrapItemFinal then
    begin
      ItemGap:= AGaps.Find(Item.NLineIndex);
      if Assigned(ItemGap) then
      begin
        Inc(NPixels, ItemGap.Size);
        if NPixels>P.Y then
        begin
          ADetails.OnGapItem:= ItemGap;
          ADetails.OnGapPos.Y:= P.Y - (NPixels-ItemGap.Size);
          if Assigned(ItemGap.Bitmap) then
            ADetails.OnGapPos.X:= P.X - GetGapBitmapPosLeft(ARect, ItemGap.Bitmap)
          else
            ADetails.OnGapPos.X:= P.X - ARect.Left;

          case AGapCoordAction of
            cGapCoordToLineEnd:
              bGapGoesToEnd:= true;
            cGapCoordMoveDown:
              begin
                if AWrapInfo.IsIndexValid(NIndex+1) then
                  Inc(NIndex);
              end;
          end;
          Break;
        end;
      end;
    end;

    Inc(NIndex);
  until false;

  //click above all text
  if NIndex<0 then
  begin
    Result.X:= 0;
    Result.Y:= 0;
    Exit
  end;

  //click below all text
  if not AWrapInfo.IsIndexValid(NIndex) then
  begin
    NIndex:= AWrapInfo.Count-1;
    if AAfterEofUsesLastLen then
    begin
      if AWrapInfo.IsIndexValid(NIndex) then
      begin
        Item:= AWrapInfo[NIndex];
        Result.Y:= Item.NLineIndex;
        Result.X:= Item.NCharIndex+Item.NLength-1;
      end;
      Exit
    end;
  end;

  //click in text
  begin
    Item:= AWrapInfo[NIndex];
    Result.Y:= Item.NLineIndex;

    Str:= AStrings.LineSub(
      Result.Y,
      Item.NCharIndex,
      //optimized for huge line
      Min(Item.NLength,
          AScrollHorz.NPos + (ARect.Right-ARect.Left) div ACharSize.X)
      );

    if bGapGoesToEnd then
      Result.X:= Length(Str) + Item.NCharIndex-1
    else
    begin
      AllowVirtual:= AVirtualPos and (Item.NFinal=cWrapItemFinal);
      NPixels:= P.X-ARect.Left + ACharSize.X*(AScrollHorz.NPos-Item.NIndent);
      Result.X:= ATabHelper.FindClickedPosition(
        Result.Y,
        Str,
        NPixels,
        ACharSize.X,
        AllowVirtual,
        ADetails.EndOfWrappedLine)
        + Item.NCharIndex - 2;
    end;

    if Item.NFinal=cWrapItemFinal then //don't set AWrappedEnd for real eol
      ADetails.EndOfWrappedLine:= false;
  end;
end;

function TATSynEdit.CaretPosToClientPos(P: TPoint): TPoint; inline;
begin
  Result:= _DoCaretPosToClientPos(P,
    FWrapInfo,
    Strings,
    Gaps,
    FCharSize,
    FTabHelper,
    FRectMain,
    FScrollHorz,
    FScrollVert,
    FCaretSpecPos or FOptCaretPreferLeftSide
    );
end;

function TATSynEdit.ClientPosToCaretPos(P: TPoint;
  out ADetails: TATPosDetails;
  AGapCoordAction: TATGapCoordAction = cGapCoordToLineEnd): TPoint; inline;
begin
  Result:= _DoClientPosToCaretPos(P,
    FWrapInfo,
    Strings,
    Gaps,
    FCharSize,
    FTabHelper,
    FRectMain,
    FScrollHorz,
    FScrollVert,
    FCaretVirtual or FMouseDownAndColumnSelection,
    ADetails,
    AGapCoordAction,
    IsSelRectEmpty);
end;

procedure TATSynEdit.SetCaretBlinkEnabled(AValue: boolean);
begin
  if FCaretBlinkEnabled=AValue then Exit;
  FCaretBlinkEnabled:= AValue;
  DoPaintModeStatic;
  DoPaintModeBlinking;
end;

procedure TATSynEdit.DoScroll_IndentFromTop(AWrapInfoIndex, AIndentVert: integer); inline;
begin
  FScrollVert.NPos:= Max(0, AWrapInfoIndex-AIndentVert);
  FScrollVert.NPixelOffset:= 0;
end;

procedure TATSynEdit.DoScroll_IndentFromBottom(AWrapInfoIndex, AIndentVert: integer);
//index: original vert scroll pos
//do dec(index), until offset (pixels) from caret to bottom is small enough
var
  NPixels, NPixelsNeeded, NIndex, NCell: integer;
  ItemGap: TATSynGapItem;
  i: integer;
begin
  NIndex:= AWrapInfoIndex;
  if NIndex<=0 then exit;

  NCell:= FCharSize.Y;
  NPixels:= (FRectMain.Bottom-FRectMain.Top)-NCell;
  if NPixels<=0 then exit;
    //this is distance of first visible line, to bottom of control
  NPixelsNeeded:= (AIndentVert+1)*NCell-1;
    //this distance must be reached,
    //can be smaller/bigger, if wrapped lines or gaps

  for i:= 1 to GetVisibleLines do
  begin
    Dec(NIndex);
    if NIndex<0 then
    begin
      FScrollVert.NPos:= 0;
      Exit;
    end;

    Dec(NPixels, NCell);

    //consider gap
    if FWrapInfo[NIndex].NFinal=cWrapItemFinal then
    begin
      ItemGap:= Gaps.Find(FWrapInfo[NIndex].NLineIndex);
      if Assigned(ItemGap) then
        Dec(NPixels, ItemGap.Size);
    end;

    if NPixels<0 then
    begin
      FScrollVert.NPos:= NIndex+1;
      Exit;
    end;

    if NPixels<=NPixelsNeeded then
    begin
      FScrollVert.NPos:= NIndex;
      Exit;
    end;
  end;
end;

procedure TATSynEdit.DoGotoPos(const APos, APosEnd: TPoint;
  AIndentHorz, AIndentVert: integer;
  APlaceCaret, ADoUnfold: boolean;
  AAllowProcessMsg: boolean=true);
var
  NIndex, NChars: integer;
  Coord: TPoint;
  bScrolled: boolean;
begin
  if not Strings.IsIndexValid(APos.Y) then exit;

  //if IsPosFolded(ACaretPos.X, ACaretPos.Y) -better?
  if IsLineFolded(APos.Y, true) then
  begin
    if ADoUnfold then
    begin
      DoUnfoldLine(APos.Y);
      Update;
      if AAllowProcessMsg then
        Application.ProcessMessages;
    end
    else
      exit;
  end;

  if APlaceCaret then
  begin
    DoCaretSingle(APos.X, APos.Y, APosEnd.X, APosEnd.Y);
    DoEventCarets;
  end;

  bScrolled:= false;
  Coord:= CaretPosToClientPos(APos);

  //for y
  //  vert indent <0: always indent from top;
  //             >=0: indent from top (goto up) or bottom (goto down)
  if Coord.Y<FRectMain.Top then
  begin
    NIndex:= FWrapInfo.FindIndexOfCaretPos(APos);
    if NIndex<0 then Exit;
    DoScroll_IndentFromTop(NIndex, Abs(AIndentVert));
    UpdateScrollbars;
    bScrolled:= true;
  end
  else
  if Coord.Y+FCharSize.Y >= FRectMain.Bottom then
  begin
    NIndex:= FWrapInfo.FindIndexOfCaretPos(APos);
    if NIndex<0 then Exit;
    if AIndentVert<0 then
      DoScroll_IndentFromTop(NIndex, Abs(AIndentVert))
    else
      DoScroll_IndentFromBottom(NIndex, Abs(AIndentVert));
    UpdateScrollbars;
    bScrolled:= true;
  end;

  //for x
  if Coord.X<FRectMain.Left then
  begin
    NChars:= (FRectMain.Left-Coord.X) div FCharSize.X + 1 + AIndentHorz;
    FScrollHorz.NPos:= Max(0, FScrollHorz.NPos-NChars);
    UpdateScrollbars;
    bScrolled:= true;
  end
  else
  if Coord.X>FRectMain.Right-FCharSize.X then
  begin
    NChars:= (Coord.X-FRectMain.Right) div FCharSize.X + 2 + AIndentHorz;
    Inc(FScrollHorz.NPos, NChars);
    UpdateScrollbars;
    bScrolled:= true;
  end;

  //if bScrolled then
  //  UpdateCaretsCoords;
  Update;
end;

procedure TATSynEdit.DoGotoCaret(AEdge: TATCaretEdge);
begin
  DoGotoPos(
    Carets.CaretAtEdge(AEdge),
    Point(-1, -1),
    FOptScrollIndentCaretHorz,
    FOptScrollIndentCaretVert,
    false,
    false //dont unfold
    );
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


procedure TATSynEdit.DoCaretSingle(APosX, APosY, AEndX, AEndY: integer);
begin
  FSelRect:= cRectEmpty;
  Carets.Clear;
  Carets.Add(APosX, APosY, AEndX, AEndY);
end;

procedure TATSynEdit.DoCaretSingle(AX, AY: integer; AClearSelection: boolean);
var
  NEndX, NEndY: integer;
  Caret: TATCaretItem;
begin
  NEndX:= -1;
  NEndY:= -1;

  if (not AClearSelection) and (Carets.Count>0) then
  begin
    Caret:= Carets[0];
    NEndX:= Caret.EndX;
    NEndY:= Caret.EndY;
  end;

  DoCaretSingle(AX, AY, NEndX, NEndY);
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
  SetLength(Result, 0);
  if Assigned(FCarets) then
    Result:= FCarets.AsArray;
end;

procedure TATSynEdit.SetCaretsArray(const Ar: TATPointArray);
begin
  if Assigned(FCarets) then
    FCarets.AsArray:= Ar;
end;

procedure TATSynEdit.DoCaretsExtend(ADown: boolean; ALines: integer);
var
  Caret: TATCaretItem;
  MoreCarets: TATCarets;
  X, Y: integer;
  i, j: integer;
begin
  MoreCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Caret:= Carets[i];
      for j:= 1 to ALines do
      begin
        if not Strings.IsIndexValid(Caret.PosY) then Break;
        X:= Strings.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper);
        Y:= Caret.PosY+BoolToPlusMinusOne(ADown)*j;
        if not Strings.IsIndexValid(Y) then Break;
        X:= Strings.ColumnPosToCharPos(Y, X, FTabHelper);
        MoreCarets.Add(X, Y);
      end;
    end;

    for i:= 0 to MoreCarets.Count-1 do
    begin
      Caret:= MoreCarets[i];
      Carets.Add(Caret.PosX, Caret.PosY);
    end;

    //must remove many dups
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

function TATSynEdit.IsCaretBlocked: boolean; inline;
begin
  Result:= FCaretStopUnfocused and not Focused;
end;

procedure TATSynEdit.UpdateIncorrectCaretPositions; inline;
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


procedure TATSynEdit.DoCaretsShift_MarkerItem(
  AMarkerObj: TATMarkers;
  AMarkerIndex: integer;
  APosX, APosY, AShiftX, AShiftY, AShiftBelowX: integer;
  APosAfter: TPoint);
var
  Mark: TATMarkerItem;
begin
  Mark:= AMarkerObj[AMarkerIndex];
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

  AMarkerObj[AMarkerIndex]:= Mark;
end;


procedure TATSynEdit.DoCaretsFixIfInsideFolded;
var
  Caret: TATCaretItem;
  NFoldFrom, i: integer;
  bHidden: boolean;
begin
  for i:= Carets.Count-1 downto 0 do
  begin
    Caret:= Carets[i];
    if Caret.EndY>=0 then Continue;
    if not Strings.IsIndexValid(Caret.PosY) then Continue;

    bHidden:= Strings.LinesHidden[Caret.PosY, FEditorIndex];
    if bHidden then
    begin
      Caret.PosX:= 0;
      Continue;
    end;

    NFoldFrom:= Strings.LinesFoldFrom[Caret.PosY, FEditorIndex];
    if NFoldFrom=0 then
      Continue
    else
      Caret.PosX:= Min(Caret.PosX, NFoldFrom-1);
  end;
end;


procedure TATSynEdit.DoCaretsFixIncorrectPos(AndLimitByLineEnds: boolean);
var
  Caret: TATCaretItem;
  NLast, NLen, i: integer;
begin
  NLast:= Strings.Count-1;
  if NLast<0 then
  begin
    DoCaretSingle(0, 0);
    exit
  end;

  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];

    //fix position
    if not Strings.IsIndexValid(Caret.PosY) then
    begin
      Caret.PosY:= NLast;
      Caret.EndX:= -1;
      Caret.EndY:= -1;
    end;

    if AndLimitByLineEnds then
    begin
      NLen:= Strings.LinesLen[Caret.PosY];
      if Caret.PosX>NLen then
        Caret.PosX:= NLen;
    end;

    //fix selection end
    if Caret.EndY>=0 then
    begin
      if not Strings.IsIndexValid(Caret.EndY) then
        Caret.EndY:= NLast;
      NLen:= Strings.LinesLen[Caret.EndY];
      if Caret.EndX>NLen then
        Caret.EndX:= NLen;
    end;
  end;
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
    DoCaretsShift_MarkerItem(Markers, i,
      APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);

  for i:= 0 to Attribs.Count-1 do
    DoCaretsShift_MarkerItem(Attribs, i,
      APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);

  for i:= 0 to FMarkedRange.Count-1 do
    DoCaretsShift_MarkerItem(FMarkedRange, i,
      APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);
end;


procedure TATSynEdit.DoCaretForceShow; inline;
begin
  Include(FPaintFlags, cPaintUpdateCarets);
  FCaretAllowNextBlink:= true;
end;


function TATSynEdit.DoCaretsKeepOnScreen(AMoveDown: boolean): boolean;
//Forces carets to visible screen area. Gets true if some caret modified.
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  Details: TATPosDetails;
  i: integer;
begin
  Result:= false;

  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if AMoveDown and (Caret.CoordY<=FRectMain.Top+2) then
    begin
      Pnt:= Point(Caret.CoordX, FRectMain.Top);
      Pnt:= ClientPosToCaretPos(Pnt, Details);
      Caret.PosX:= Pnt.X;
      Caret.PosY:= Pnt.Y;
      Result:= true;
    end
    else
    if not AMoveDown and (Caret.CoordY>=FRectMain.Bottom-2*FCharSize.Y-2) then
    begin
      Pnt:= Point(Caret.CoordX, FRectMain.Bottom-FCharSize.Y);
      Pnt:= ClientPosToCaretPos(Pnt, Details);
      Caret.PosX:= Pnt.X;
      Caret.PosY:= Pnt.Y;
      Result:= true;
    end;
  end;
end;


procedure TATSynEdit.DoCaretsApplyShape(var R: TRect; Props: TATCaretProps; W, H: integer);
var
  Y0, DY: integer;
begin
  //width>100% is not needed
  if Props.Width>=0 then
    R.Right:= R.Left + Props.Width
  else
    R.Right:= R.Left - W * Props.Width div 100;

  if Props.Height>=0 then
    R.Top:= R.Bottom - Props.Height
  else
  if Props.Height<-100 then
  begin
    Y0:= (R.Top+R.Bottom) div 2;
    DY:= -H * Props.Height div 200;
    R.Top:= Y0-DY;
    R.Bottom:= Y0+DY;
  end
  else
    R.Top:= R.Bottom + H * Props.Height div 100;
end;


{ TATCaretProps }

procedure TATCaretProps.Assign(Obj: TATCaretProps);
begin
  Width:= Obj.Width;
  Height:= Obj.Height;
  EmptyInside:= Obj.EmptyInside;
end;

