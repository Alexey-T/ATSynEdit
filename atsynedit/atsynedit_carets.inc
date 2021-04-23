{$ifdef nnnn}begin end;{$endif}

function TATSynEdit.IsLineWithCaret(ALine: integer): boolean; inline;
begin
  Result:= FCarets.IsLineWithCaret(ALine);
end;

function TATSynEdit.IsLinePartWithCaret(ALine: integer; ACoordY: integer): boolean;
var
  i: integer;
  Caret: TATCaretItem;
  Coord: TPoint;
begin
  Result:= false;
  //like Carets.IsLineWithCaret with more code
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
  St: TATStrings;
  i: integer;
begin
  St:= Strings;
  for i:= APos1.Y to APos2.Y do
  begin
    if i=APos1.Y then
      Carets.Clear;
    Carets.Add(
      St.ColumnPosToCharPos(i, ARect.Right, FTabHelper),
      i,
      St.ColumnPosToCharPos(i, ARect.Left, FTabHelper),
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
  Details: TATEditorPosDetails;
begin
  //needed to Undo multi-carets to last multi-caret state, before single caret appears
  Strings.SetGroupMark;

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
      if not Carets.IsLineWithCaret(P.Y) then
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
  Marker: PATMarkerItem;
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

  if Assigned(FMarkers) then
  for i:= 0 to FMarkers.Count-1 do
  begin
    Marker:= FMarkers.ItemPtr(i);
    Marker^.CoordX:= -1;
    Marker^.CoordY:= -1;
    Marker^.CoordX2:= -1;
    Marker^.CoordY2:= -1;
    P.X:= Marker^.PosX;
    P.Y:= Marker^.PosY;
    if not IsPosFolded(P.X, P.Y) then
    begin
      P:= CaretPosToClientPos(P);
      Marker^.CoordX:= P.X;
      Marker^.CoordY:= P.Y;
      if Marker^.LineLen<>0 then
      begin
        P.X:= Marker^.PosX+Marker^.LineLen;
        P.Y:= Marker^.PosY;
        P:= CaretPosToClientPos(P);
        Marker^.CoordX2:= P.X;
        Marker^.CoordY2:= P.Y;
      end;
    end;
  end;
end;


function _DoCaretPosToClientPos(
  P: TPoint;
  AWrapInfo: TATWrapInfo;
  AStrings: TATStrings;
  AGaps: TATGaps;
  ACharSize: TPoint;
  ATabHelper: TATStringTabHelper;
  const ARect: TRect;
  const AScrollHorz, AScrollVert: TATEditorScrollInfo;
  APreferLeftSide: boolean): TPoint;
var
  Item: TATWrapItem;
  StringItem: PATStringItem;
  NIndex1, NIndex2, NLineIndexTop, NFromStart, NWidth: integer;
  NScrolledHorzSmooth: integer;
  NScrolledVert: integer;
  NLineCount: integer;
  Str: atString;
  bLineHuge: boolean;
  bAsciiOnly: boolean;
  bAfterEnd: boolean;
  i: integer;
begin
  Result.X:= -1;
  Result.Y:= -1;

  if P.Y<0 then Exit;

  NLineCount:= AStrings.Count;
  if NLineCount=0 then
  begin
    Result.X:= ARect.Left;
    Result.Y:= ARect.Top;
    Exit;
  end;

  //if needed pos after file-end, we
  //1) calculate pos of last char, 2) increase Y by char height
  bAfterEnd:= P.Y>=NLineCount;
  if bAfterEnd then
  begin
    P.Y:= NLineCount-1;
    P.X:= AStrings.LinesLen[P.Y];
  end;

  AWrapInfo.FindIndexesOfLineNumber(P.Y, NIndex1, NIndex2);
  if NIndex1<0 then Exit;

  //prepare optimization
  StringItem:= AStrings.GetItemPtr(P.Y);
  bAsciiOnly:= StringItem^.HasAsciiNoTabs;

  NScrolledHorzSmooth:= AScrollHorz.SmoothPos-AScrollHorz.NPixelOffset;
  NScrolledVert:= Max(0, AScrollVert.NPos);

  for i:= NIndex1 to NIndex2 do
  begin
    Item:= AWrapInfo[i];
    bLineHuge:= Item.NLength>OptMaxLineLenForAccurateCharWidths;

    if (P.X<Item.NCharIndex-1) then Continue;
    if (Item.NFinal=cWrapItemMiddle) then
      if (P.X>Item.NCharIndex-1+Item.NLength) or
        ((P.X=Item.NCharIndex-1+Item.NLength) and APreferLeftSide) then
        Continue;

    NFromStart:= P.X+1-Item.NCharIndex;

    if bAsciiOnly or bLineHuge then
    begin
      NWidth:= Min(NFromStart, Item.NLength) * ACharSize.X;
    end
    else
    begin
      Str:= AStrings.LineSub(P.Y, Item.NCharIndex, Min(NFromStart, Item.NLength));
      NWidth:= CanvasTextWidth(Str, P.Y, ATabHelper, ACharSize.X);
    end;

    Result.X:= NWidth;

    if NFromStart>Item.NLength then
      Inc(Result.X, (NFromStart-Item.NLength)*ACharSize.X);

    Inc(Result.X, Item.NIndent*ACharSize.X - NScrolledHorzSmooth);
    Result.Y:= (i-NScrolledVert)*ACharSize.Y;

    //consider gaps
    if (AGaps.Count>0) and AWrapInfo.IsIndexValid(NScrolledVert) then
    begin
      NLineIndexTop:= AWrapInfo[NScrolledVert].NLineIndex;
      Inc(Result.Y, AGaps.SizeForLineRange(
        NLineIndexTop, //index of top line
        P.Y-1 //index of line before caret
        ));
      //consider top gap
      if AScrollVert.TopGapVisible then
        Inc(Result.Y, AGaps.SizeOfGapTop);
    end;

    //coords of corner
    Inc(Result.X, ARect.Left);
    Inc(Result.Y, ARect.Top);

    if bAfterEnd then
    begin
      Result.X:= ARect.Left;
      Inc(Result.Y, ACharSize.Y);
    end;

    Exit
  end;
end;


function _DoClientPosToCaretPos(
  P: TPoint;
  AWrapInfo: TATWrapInfo;
  AStrings: TATStrings;
  AGaps: TATGaps;
  ACharSize: TPoint;
  ATabHelper: TATStringTabHelper;
  const ARect: TRect;
  const AScrollHorz, AScrollVert: TATEditorScrollInfo;
  AVirtualPos: boolean;
  out ADetails: TATEditorPosDetails;
  AGapCoordAction: TATEditorGapCoordAction;
  AAfterEofUsesLastLen: boolean;
  AEditorIndex: integer): TPoint;
var
  CharOffsets: TATIntFixedArray;
  NLineLenOriginal, NLineLenTrimmed: integer;
  NIndex, NIndexDecrement, NPixels: integer;
  Item: TATWrapItem;
  ItemGap: TATGapItem;
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
  NIndex:= Max(0, AScrollVert.NPos);
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

  //support gap before 1st line
  if (NIndex=0) and AScrollVert.TopGapVisible and (AGaps.Count>0) then
    Inc(NPixels, AGaps.SizeOfGapTop);

  repeat
    if not AWrapInfo.IsIndexValid(NIndex) then
      Break;

    Inc(NPixels, ACharSize.Y);
    if NPixels>P.Y then Break;

    //consider gap
    Item:= AWrapInfo[NIndex];
    if (Item.NFinal=cWrapItemFinal) and (AGaps.Count>0) then
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
        if not AStrings.IsIndexValid(Result.Y) then
          exit(Point(-1, -1));
      end;
      Exit
    end;
  end;

  //click in text
    Item:= AWrapInfo[NIndex];
    Result.Y:= Item.NLineIndex;
    if not AStrings.IsIndexValid(Result.Y) then
      exit(Point(-1, -1));

    NLineLenOriginal:= Item.NLength;
    NLineLenTrimmed:= Min(Item.NLength, AScrollHorz.NPos + ARect.Width div ACharSize.X);

    if NLineLenOriginal>OptMaxLineLenForAccurateCharWidths then
    begin
      Str:= StringOfCharW('N', NLineLenOriginal); //fake to calc CharOffsets only
      ATabHelper.CalcCharOffsets(Item.NLineIndex, Str, CharOffsets, 0);
      Str:= AStrings.LineSub(Item.NLineIndex, Item.NCharIndex, NLineLenTrimmed);
    end
    else
    begin
      Str:= AStrings.LineSub(Item.NLineIndex, Item.NCharIndex, NLineLenTrimmed);
      ATabHelper.CalcCharOffsets(Item.NLineIndex, Str, CharOffsets, 0);
    end;

    if bGapGoesToEnd then
      Result.X:= Length(Str) + Item.NCharIndex-1
    else
    begin
      AllowVirtual:= AVirtualPos and (Item.NFinal=cWrapItemFinal);
      NPixels:= P.X-ARect.Left + ACharSize.X*(AScrollHorz.NPos-Item.NIndent);
      Result.X:= ATabHelper.FindClickedPosition(
        Result.Y,
        Str,
        CharOffsets,
        NPixels,
        ACharSize.X,
        AllowVirtual,
        ADetails.EndOfWrappedLine)
        + Item.NCharIndex - 2;

      //position after end of partially folded line? return line length
      if ADetails.EndOfWrappedLine then
        if AStrings.LinesFoldFrom[Result.Y, AEditorIndex]>0 then
          Result.X:= AStrings.LinesLen[Result.Y];
    end;

    if Item.NFinal=cWrapItemFinal then //don't set AWrappedEnd for real eol
      ADetails.EndOfWrappedLine:= false;
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
  out ADetails: TATEditorPosDetails;
  AGapCoordAction: TATEditorGapCoordAction = cGapCoordToLineEnd): TPoint; inline;
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
    IsSelRectEmpty,
    FEditorIndex);
end;

procedure TATSynEdit.SetCaretBlinkEnabled(AValue: boolean);
begin
  if FCaretBlinkEnabled=AValue then Exit;
  FCaretBlinkEnabled:= AValue;
  TimerBlinkEnable;
end;

procedure TATSynEdit.DoScroll_IndentFromTop(AWrapInfoIndex, AIndentVert: integer); inline;
begin
  AIndentVert:= Min(AIndentVert, GetVisibleLines-1);
  FScrollVert.NPos:= Max(0, AWrapInfoIndex-AIndentVert);
  FScrollVert.NPixelOffset:= 0;
end;

procedure TATSynEdit.DoScroll_IndentFromBottom(AWrapInfoIndex, AIndentVert: integer);
//index: original vert scroll pos
//do dec(index), until offset (pixels) from caret to bottom is small enough
var
  NPixels, NPixelsNeeded, NIndex, NCell: integer;
  ItemGap: TATGapItem;
  i: integer;
begin
  NIndex:= AWrapInfoIndex;
  if NIndex<=0 then exit;

  NCell:= FCharSize.Y;
  NPixels:= FRectMain.Height-NCell;
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
      FScrollVert.SetZero;
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

    //support gap before 1st line
    if (NIndex=0) then
    begin
      ItemGap:= Gaps.Find(-1);
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

function TATSynEdit.DoShowPos(const APos: TPoint; AIndentHorz, AIndentVert: integer; AUnfold, AllowUpdate: boolean): boolean;
var
  Coord: TPoint;
  NIndex, NChars: integer;
begin
  Result:= false;

  //allow big values - make centering vertically
  if Abs(AIndentVert)>=cMaxIndentVert then
    AIndentVert:= GetVisibleLines div 2;

  if AUnfold then
    if IsLineFolded(APos.Y, true) then
      DoUnfoldLine(APos.Y);

  Coord:= CaretPosToClientPos(APos);

  //for y
  //  vert indent <0: always indent from top;
  //             >=0: indent from top (goto up) or bottom (goto down)
  if Coord.Y<FRectMain.Top then
  begin
    NIndex:= FWrapInfo.FindIndexOfCaretPos(APos);
    if NIndex<0 then Exit;
    DoScroll_IndentFromTop(NIndex, Abs(AIndentVert));
    Result:= true;
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
    Result:= true;
  end;

  //for x
  if Coord.X<FRectMain.Left then
  begin
    NChars:= (FRectMain.Left-Coord.X) div FCharSize.X + 1 + AIndentHorz;
    FScrollHorz.NPos:= Max(0, FScrollHorz.NPos-NChars);
    Result:= true;
  end
  else
  if Coord.X>FRectMain.Right-FCharSize.X then
  begin
    NChars:= (Coord.X-FRectMain.Right) div FCharSize.X + 2 + AIndentHorz;
    Inc(FScrollHorz.NPos, NChars);
    Result:= true;
  end;

  if Result then
  begin
    UpdateScrollbars(true);
    if AllowUpdate then
      Update;
  end;
end;

procedure TATSynEdit.DoGotoPos(const APos, APosEnd: TPoint;
  AIndentHorz, AIndentVert: integer;
  APlaceCaret, ADoUnfold: boolean;
  AAllowProcessMsg: boolean=true;
  AAllowUpdate: boolean=true);
var
  changed1, changed2: boolean;
begin
  if not Strings.IsIndexValid(APos.Y) then exit;

  //if IsPosFolded(APos.X, APos.Y) - better?
  if IsLineFolded(APos.Y, true) then
  begin
    if ADoUnfold then
    begin
      DoUnfoldLine(APos.Y);
      if AAllowUpdate then
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
    ActionAddJumpToUndo;
    DoEventCarets;
  end;

  if APosEnd.Y>=0 then
    changed1:= DoShowPos(APosEnd, AIndentHorz, AIndentVert, ADoUnfold, false)
  else
    changed1:= false;
  changed2:= DoShowPos(APos, AIndentHorz, AIndentVert, ADoUnfold, false);

  ////always update scrollbar after all keyboard cmds (PageUp, PageDown, Ctrl+Home)
  if not changed1 and not changed2 then
    UpdateScrollbars(true);

  if AAllowUpdate then
    Update;
end;

procedure TATSynEdit.DoGotoCaret(AEdge: TATCaretEdge; AUndoRedo: boolean=false;
  AAllowProcessMsg: boolean=true; AAllowUpdate: boolean=true);
var
  NIndentVert, NIndentHorz: integer;
begin
  if AUndoRedo then
  begin
    NIndentVert:= FOptUndoIndentVert;
    NIndentHorz:= FOptUndoIndentHorz;
  end
  else
  begin
    NIndentVert:= FOptScrollIndentCaretVert;
    NIndentHorz:= FOptScrollIndentCaretHorz;
  end;

  DoGotoPos(
    Carets.CaretAtEdge(AEdge),
    Point(-1, -1),
    NIndentHorz,
    NIndentVert,
    false,
    false, //dont unfold
    AAllowProcessMsg,
    AAllowUpdate
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

function TATSynEdit.DoCaretSwapEdge(Item: TATCaretItem; AMoveLeft: boolean): boolean;
var
  X1, Y1, X2, Y2: integer;
  bSel, bAtLeft: boolean;
begin
  Result:= false;
  Item.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then Exit;
  Result:= true;

  bAtLeft:= not Item.IsForwardSelection;

  //Left/Rt pressed at left/rt side of selection?
  //yes: cancel selection, don't move caret
  if bAtLeft=AMoveLeft then
  begin
    Item.EndX:= -1;
    Item.EndY:= -1;
    Exit
  end;

  //else swap edge
  Item.SwapSelection;

  if not FOptKeyLeftRightSwapSelAndSelect then
  begin
    Item.EndX:= -1;
    Item.EndY:= -1;
  end;
end;

function TATSynEdit.GetCaretsArray: TATPointArray;
begin
  SetLength(Result{%H-}, 0);
  if Assigned(FCarets) then
    Result:= FCarets.AsArray;
end;

function TATSynEdit.GetMarkersArray: TATInt64Array;
begin
  SetLength(Result{%H-}, 0);
  if Assigned(FMarkers) then
    Result:= FMarkers.AsArray;
end;

procedure TATSynEdit.SetCaretsArray(const Ar: TATPointArray);
begin
  if Assigned(FCarets) then
    FCarets.AsArray:= Ar;
end;

procedure TATSynEdit.SetMarkersArray(const Ar: TATInt64Array);
begin
  InitMarkers; //FMarkers can be nil
  if Assigned(FMarkers) then
    FMarkers.AsArray:= Ar;
end;

procedure TATSynEdit.DoCaretsExtend(ADown: boolean; ALines: integer);
var
  Caret: TATCaretItem;
  MoreCarets: TATCarets;
  St: TATStrings;
  X, Y: integer;
  i, j: integer;
begin
  St:= Strings;

  //it's needed to Undo to multi-carets, not to single caret
  St.SetGroupMark;

  MoreCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Caret:= Carets[i];
      for j:= 1 to ALines do
      begin
        if not St.IsIndexValid(Caret.PosY) then Break;
        X:= St.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper);
        Y:= Caret.PosY+BoolToPlusMinusOne[ADown]*j;
        if not St.IsIndexValid(Y) then Break;
        X:= St.ColumnPosToCharPos(Y, X, FTabHelper);
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
  if Caret.PosY=APosY then
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
  if Caret.EndY=APosY then
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


function TATSynEdit.DoCaretsFixIncorrectPos(AndLimitByLineEnds: boolean): boolean;
var
  Caret: TATCaretItem;
  NLast, NLen, i: integer;
begin
  Result:= false;

  NLast:= Strings.Count-1;
  if NLast<0 then
  begin
    Result:= true;
    DoCaretSingle(0, 0);
    exit
  end;

  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];

    //fix position
    if not Strings.IsIndexValid(Caret.PosY) then
    begin
      Result:= true;
      Caret.PosY:= NLast;
      Caret.EndX:= -1;
      Caret.EndY:= -1;
    end;

    if AndLimitByLineEnds or not FCaretVirtual then
    begin
      NLen:= Strings.LinesLen[Caret.PosY];
      if Caret.PosX>NLen then
      begin
        Result:= true;
        Caret.PosX:= NLen;
      end;
    end;

    //fix selection end
    if Caret.EndY>=0 then
    begin
      if not Strings.IsIndexValid(Caret.EndY) then
      begin
        Result:= true;
        Caret.EndY:= NLast;
      end;
      NLen:= Strings.LinesLen[Caret.EndY];
      if Caret.EndX>NLen then
      begin
        Result:= true;
        Caret.EndX:= NLen;
      end;
    end;
  end;
end;


procedure TATSynEdit.DoCaretsShift(AFromCaret: integer; APosX, APosY: integer;
  AShiftX, AShiftY: integer; APosAfter: TPoint; AShiftBelowX: integer);
//for text change at unknown caret, pass AFromCaret=0
//AShiftBelowX: used in Del key command, it's set to the length of line,
//to shift (to right) carets in the line below
var
  i: integer;
  {$ifdef debug_markers_shift}
  S: string;
  {$endif}
begin
  if APosX<0 then Exit;
  if APosY<0 then Exit;

  //if AFromCaret<0 then
  //  AFromCaret:= Carets.IndexOfPosYAvg(APosY);

  if AFromCaret>=0 then
    for i:= AFromCaret to Carets.Count-1 do
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
  if Assigned(FMarkers) then
    for i:= 0 to FMarkers.Count-1 do
      DoCaretsShift_MarkerItem(FMarkers, i,
        APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);

  if Assigned(FAttribs) then
    for i:= 0 to FAttribs.Count-1 do
      DoCaretsShift_MarkerItem(FAttribs, i,
        APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);

  if Assigned(FMarkedRange) then
    for i:= 0 to FMarkedRange.Count-1 do
      DoCaretsShift_MarkerItem(FMarkedRange, i,
        APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);
end;


procedure TATSynEdit.DoCaretForceShow; inline;
begin
  FCaretAllowNextBlink:= true;
end;


function TATSynEdit.DoCaretsKeepOnScreen(AMoveDown: boolean): boolean;
//Forces carets to visible screen area. Gets true if some caret modified.
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  Details: TATEditorPosDetails;
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


procedure TATSynEdit.DoCaretsApplyShape(var R: TRect; Props: TATCaretShape; W, H: integer);
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


function TATSynEdit.IsCaretOnVisibleRect: boolean;
begin
  //must refresh carets' CoordX/CoordY first, otherwise 1st carets' move out of
  //visible rect will not be detected
  UpdateCaretsCoords;

  Result:= Carets.IsAnyCaretInVisibleRect(FRectMain);
end;

{ TATCaretShape }

procedure TATCaretShape.Assign(Obj: TATCaretShape);
begin
  Width:= Obj.Width;
  Height:= Obj.Height;
  EmptyInside:= Obj.EmptyInside;
end;

