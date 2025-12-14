{$ifdef nnnn}begin end;{$endif}

function TATSynEdit.DoCommand_KeyHome: TATCommandResults;
var
  Caret: TATCaretItem;
  WrapItem: TATWrapItem;
  NLine, NIndent, NWrapped, i: SizeInt;
  NWrapIndex: integer;
  IndentKind: TATLineIndentKind;
begin
  if FCarets.Count=0 then exit([]);

  if ModeOneLine then
  begin
    Caret:= FCarets[0];
    Caret.PosX:= 0;
  end
  else
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    NWrapped:= 0;
    NIndent:= 0;
    NLine:= Caret.PosY;
    if not Strings.IsIndexValid(NLine) then Continue;

    if FOptKeyHomeEndNavigateWrapped and (FWrapMode<>TATEditorWrapMode.ModeOff) then
    begin
      NWrapIndex:= WrapInfo.FindIndexOfCaretPos(Point(Caret.PosX, Caret.PosY));
      if NWrapIndex>=0 then
      begin
        WrapItem:= WrapInfo[NWrapIndex];
        NWrapped:= WrapItem.NCharIndex-1;
      end;
    end;

    if FOptKeyHomeToNonSpace then
      Strings.GetIndentProp(NLine, NIndent, IndentKind);

    if (NWrapped>0) and (Caret.PosX>NWrapped) then
      Caret.PosX:= NWrapped
    else
    if (NIndent>0) and (Caret.PosX>NIndent) then
      Caret.PosX:= NIndent
    else
    if FOptKeyHomeToNonSpace and (Caret.PosX=0) and (NIndent>0) then
      Caret.PosX:= NIndent
    else
      Caret.PosX:= 0;
  end;

  if Caret.PosX=0 then
  begin
    FScrollHorz.NPos:= 0;
    FScrollHorz.NPixelOffset:= 0;
  end;

  Result:= [TATCommandResult.CaretLeft];
end;

function TATSynEdit.DoCommand_KeyEnd: TATCommandResults;
var
  St: TATStrings;
  Caret: TATCaretItem;
  WrapItem: TATWrapItem;
  NLine, NLenTotal, NLen, NWrapped, i: integer;
  NWrapIndex: integer;
begin
  if FCarets.Count=0 then exit([]);
  St:= Strings;

  if ModeOneLine then
  begin
    Caret:= FCarets[0];
    Caret.PosX:= St.LinesLen[0];
  end
  else
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    NLine:= Caret.PosY;
    if not St.IsIndexValid(NLine) then Continue;

    NLenTotal:= St.LinesLen[NLine];
    NLen:= 0;
    NWrapped:= 0;

    if FOptKeyHomeEndNavigateWrapped and (FWrapMode<>TATEditorWrapMode.ModeOff) then
    begin
      NWrapIndex:= WrapInfo.FindIndexOfCaretPos(Point(Caret.PosX, Caret.PosY));
      if NWrapIndex>=0 then
      begin
        WrapItem:= WrapInfo[NWrapIndex];
        NWrapped:= WrapItem.NCharIndex+WrapItem.NLength-1;
        if (NWrapped>0) and (NWrapped<NLenTotal) then
          Dec(NWrapped);
      end;
    end;

    if FOptKeyEndToNonSpace then
      NLen:= St.LineLenWithoutSpace(NLine);

    if (NWrapped>0) and (Caret.PosX<NWrapped) then
      Caret.PosX:= NWrapped
    else
    if (NLen>0) and (Caret.PosX<NLen) then
      Caret.PosX:= NLen
    else
    if FOptKeyEndToNonSpace and (Caret.PosX=NLenTotal) and (NLen<>NLenTotal) then
      Caret.PosX:= NLen
    else
      Caret.PosX:= NLenTotal;
  end;

  Result:= [TATCommandResult.CaretRight];
end;

function TATSynEdit.DoCommand_KeyLeft(ASelCommand: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  bMultiCarets, bKeepSel: boolean;
  i: integer;
begin
  //changed because of issue CudaText#3065
  Result:= [TATCommandResult.CaretLeft, TATCommandResult.CaretLazy];

  bMultiCarets:= FCarets.Count>1;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if not Strings.IsIndexValid(Caret.PosY) then Continue;

    bKeepSel:= not ASelCommand and FOptKeyLeftRightSwapSel and Caret.SwapEdge(true, FOptKeyLeftRightSwapSelAndSelect);

    if bKeepSel then
    begin
      //none
    end
    else
    if (Caret.PosX>0) then
      Dec(Caret.PosX)
    else
    if bMultiCarets and not FOptKeyLeftRightGoToNextLineWithCarets then
    begin
      //none
    end
    else
    if (Caret.PosY>0) and not FCaretVirtual then
    begin
      Dec(Caret.PosY);
      Caret.PosX:= Strings.LinesLen[Caret.PosY];
    end;

    //fixing CudaText #4391
    if not ASelCommand and not bKeepSel then
      Caret.SelectNone
    else
    if not bMultiCarets and Caret.IsSelectionEmpty then
      Caret.SelectNone;
  end;
end;

function TATSynEdit.DoCommand_KeyRight(ASelCommand: boolean): TATCommandResults;
var
  St: TATStrings;
  Caret: TATCaretItem;
  bMultiCarets, bKeepSel: boolean;
  i: integer;
begin
  //changed because of issue CudaText#3065
  Result:= [TATCommandResult.CaretRight, TATCommandResult.CaretLazy];

  St:= Strings;
  bMultiCarets:= FCarets.Count>1;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Caret:= FCarets[i];
    if not St.IsIndexValid(Caret.PosY) then Continue;

    bKeepSel:= not ASelCommand and FOptKeyLeftRightSwapSel and Caret.SwapEdge(false, FOptKeyLeftRightSwapSelAndSelect);

    if bKeepSel then
    begin
      //none
    end
    else
    if (Caret.PosX<St.LinesLen[Caret.PosY]) or FCaretVirtual then
      Inc(Caret.PosX)
    else
    if bMultiCarets and not FOptKeyLeftRightGoToNextLineWithCarets then
    begin
      //none
    end
    else
    if (Caret.PosY<St.Count-1) then
    begin
      Caret.PosX:= 0;
      Inc(Caret.PosY);
    end;

    //fixing CudaText #4391
    if not ASelCommand and not bKeepSel then
      Caret.SelectNone
    else
    if not bMultiCarets and Caret.IsSelectionEmpty then
      Caret.SelectNone;
  end;
end;

function TATSynEdit.DoCommand_KeyUpDown(ADown: boolean; ALines: integer;
  AKeepRelativePos, AWithSel: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  NRelative, iCaret: integer;
  bChange: boolean;
begin
  FCaretSpecPos:= true;

  if AKeepRelativePos then
    NRelative:= LinesFromTop;

  if not AWithSel then
  begin
    //feature like in Sublime Text/ VSCode: with multi-line selection, caret at bottom,
    //pressing Up - moves caret from the _top_ edge of selection.
    bChange:= false;
    if FOptKeyUpDownSwapSel and (ALines=1) then
      for iCaret:= Carets.Count-1 downto 0 do
      begin
        Caret:= Carets[iCaret];
        if Caret.IsSelection and (Caret.IsForwardSelection<>ADown) then
        begin
          Caret.SwapSelection;
          bChange:= true;
        end;
      end;
    if bChange then
      UpdateCaretsCoords(false, false);
    DoSelect_None;
  end;

  //don't check here FWrapMode<>TATEditorWrapMode.ModeOff
  if FOptKeyUpDownNavigateWrapped then
    Result:= DoCommand_KeyUpDown_Wrapped(ADown, ALines)
  else
    Result:= DoCommand_KeyUpDown_NextLine(ADown, ALines);

  if AKeepRelativePos then
    LinesFromTop:= NRelative;
end;

function TATSynEdit.DoCommand_KeyUpDown_NextLine(ADown: boolean; ALines: integer): TATCommandResults;
var
  St: TATStrings;
  Caret: TATCaretItem;
  NCount: integer;
  i, Y: integer;
begin
  St:= Strings;
  NCount:= St.Count;
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    Y:= Caret.PosY;

    repeat
      Y:= Y+ALines*BoolToPlusMinusOne[ADown];
    until not St.IsIndexValid(Y) or not IsLineFolded(Y);

    if not IsLineFolded(Y) then
    begin
      //moved above 1st line
      if Y<0 then
      begin
        Y:= Unfolded_FirstLineNumber;
        if Y=0 then
          if FOptKeyUpDownAllowToEdge then
            Caret.PosX:= 0;
      end;
      //moved below last line
      if Y>=NCount then
      begin
        Y:= Unfolded_LastLineNumber;
        if Y=NCount-1 then
          if FOptKeyUpDownAllowToEdge then
            Caret.PosX:= St.LinesLen[Y];
      end;
      Caret.PosY:= Y;
    end;
  end;

  if ADown then
    Result:= [TATCommandResult.CaretBottom]
  else
    Result:= [TATCommandResult.CaretTop];
end;

function TATSynEdit.DoCommand_KeyUpDown_Wrapped(ADown: boolean; ALines: integer): TATCommandResults;
var
  St: TATStrings;
  Caret: TATCaretItem;
  PosText: TPoint;
  PosCoord, PosCoord2: TATPoint;
  Details: TATEditorPosDetails;
  NWrapIndex, iCaret: integer;
const
  cGapActions: array[boolean] of TATEditorGapCoordAction =
    (TATEditorGapCoordAction.Ignore, TATEditorGapCoordAction.MoveDown);
begin
  St:= Strings;

  //Carets[i].Coord* may be not calculated yet (Carets[i] is out of visible area),
  //but we need Coord* here, so call UpdateCaretsCoords
  UpdateCaretsCoords;

  {
  //fix CudaText #5526; Caret.Coord* are zero, because plugin HiOccur changed the caret
  if FCarets.Count=1 then
  begin
    Caret:= FCarets[0];
    if (Caret.CoordX=0) and (Caret.CoordY=0) then
      UpdateCaretsCoords;
  end;
  }

  for iCaret:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[iCaret];
    PosCoord.X:= Caret.CoordX;
    PosCoord.Y:= Caret.CoordY;

    if IsPosFolded(Caret.PosX, Caret.PosY) then
    begin
      Caret.PosX:= 0;
      Caret.PosY:= Unfolded_NextLineNumber(Caret.PosY, ADown);
      Continue;
    end;

    if FOptKeyUpDownKeepColumn and not FCaretVirtual and (Caret.SavedX>0) then
    begin
      PosCoord.X:= Caret.SavedX;
      //needed for arrow-down on last line; w/o this, arrow-up (2 times) jumps weird
      Caret.SavedX_Pre:= 0;
    end;

    //prevent any action if caret on first line (arrow up) / last line (arrow down)
    //this allows to avoid jump to end of last line (arrow down)
    NWrapIndex:= WrapInfo.FindIndexOfCaretPos(Point(Caret.PosX, Caret.PosY));

    //caret is on 1st line. jump to very begin?
    if (not ADown) and (NWrapIndex=0) then
    begin
      if FOptKeyUpDownAllowToEdge then
        Caret.PosX:= 0;
      Continue;
    end;

    //caret is on last line. jump to very end?
    if ADown and (NWrapIndex=WrapInfo.Count-1) then
    begin
      if FOptKeyUpDownAllowToEdge then
        Caret.PosX:= St.LinesLen[St.Count-1];
      Continue;
    end;

    //increase/decrease PosCoord.Y and calculate pos from it
    PosCoord.Y+= ALines*FCharSize.Y*BoolToPlusMinusOne[ADown];
    PosText:= ClientPosToCaretPos(PosCoord, Details, cGapActions[ADown]);
    if PosText.Y<0 then Continue;

    if Details.EndOfWrappedLine and (PosText.X>0) then
      Dec(PosText.X); //-1 so Up/Down won't jump to EOL pos (caret may paint on next line)

    //PageDown should keep column when we go after document end, CudaText issues #4213 and #4359
    if ADown and (ALines>1) and FCaretVirtual then
      if (PosText.Y=St.Count-1) and (PosText.X=St.LinesLen[PosText.Y]) then
      begin
        PosCoord2:= CaretPosToClientPos(PosText);
        PosCoord2.X:= PosCoord.X;
        PosText:= ClientPosToCaretPos(PosCoord2, Details, TATEditorGapCoordAction.Ignore);
      end;

    Caret.PosX:= PosText.X;
    Caret.PosY:= PosText.Y;
  end;

  if ADown then
    Result:= [TATCommandResult.CaretBottom]
  else
    Result:= [TATCommandResult.CaretTop];
end;

function TATSynEdit.DoCommand_TextBackspace: TATCommandResults;
var
  bColBlock: boolean;
begin
  bColBlock:= not IsSelRectEmpty;

  if bColBlock then
    if FSelRect.Left=FSelRect.Right then
    begin
      DoSelect_None;
      bColBlock:= false;
    end;

  if bColBlock then
    Result:= DoCommand_TextDeleteSelection
  else
    Result:= DoCommand_TextDeleteLeft(1, FOptKeyBackspaceUnindent);
end;

function TATSynEdit.DoCommand_GotoTextBegin: TATCommandResults;
begin
  DoCaretSingle(0, 0, false);
  DoScrollToBeginOrEnd(true);

  Result:= [TATCommandResult.CaretTop];
end;

function TATSynEdit.DoCommand_GotoTextEnd: TATCommandResults;
var
  NLine: integer;
begin
  NLine:= Strings.Count-1;
  if NLine<0 then exit([]);
  DoCaretSingle(Strings.LinesLen[NLine], NLine, false);

  Result:= [TATCommandResult.CaretTop];
end;

function TATSynEdit.DoCommand_ScrollByDelta(ALines, AColumns: integer;
  AKeepCaretOnScreen: boolean): TATCommandResults;
var
  NDirection: TATEditorDirection;
begin
  DoScrollByDelta(AColumns, ALines);
  Result:= [TATCommandResult.Scroll];

  if ALines>0 then
    NDirection:= TATEditorDirection.Down
  else
  if ALines<0 then
    NDirection:= TATEditorDirection.Up
  else
  if AColumns>0 then
    NDirection:= TATEditorDirection.Right
  else
  if AColumns<0 then
    NDirection:= TATEditorDirection.Left
  else
    NDirection:= TATEditorDirection.None;

  if AKeepCaretOnScreen and not Carets.IsSelection then
    if DoCaretsKeepOnScreen(NDirection) then
      Include(Result, TATCommandResult.CaretAny);
end;

function TATSynEdit.DoCommand_ScrollToLeft: TATCommandResults;
begin
  FScrollHorz.SetZero;
  Result:= [TATCommandResult.Scroll];
end;


function TATSynEdit.DoCommand_ScrollToBeginOrEnd(AToBegin: boolean;
  AKeepCaretOnScreen: boolean): TATCommandResults;
var
  St: TATStrings;
begin
  DoScrollToBeginOrEnd(AToBegin);
  Result:= [TATCommandResult.Scroll];

  if AKeepCaretOnScreen then
    if (Carets.Count=1) and (not Carets.IsSelection) then //keeping caret works bad with multi-carets
    begin
      Include(Result, TATCommandResult.CaretAny);
      St:= Strings;
      if AToBegin then
      begin
        DoCaretSingle(0, 0);
      end
      else
      begin
        if FWrapMode=TATEditorWrapMode.ModeOff then
          DoCaretSingle(0, St.Count-1)
        else
          DoCaretSingle(St.LinesLen[St.Count-1], St.Count-1);
      end;
    end;
end;


function TATSynEdit.DoCommand_GotoWord(AJump: TATWordJump; AJumpSimple: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Strs: TATStrings;
  Str: atString;
  bMultiCaret: boolean;
  i: integer;
begin
  Strs:= Strings;
  bMultiCaret:= FCarets.Count>1;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if not Strs.IsIndexValid(Caret.PosY) then Continue;
    Str:= Strs.Lines[Caret.PosY];

    //jump to prev line?
    if (Caret.PosX=0) and (AJump=TATWordJump.Prev) then
    begin
      if not bMultiCaret or FOptKeyLeftRightGoToNextLineWithCarets then
      if Caret.PosY>0 then
      begin
        Dec(Caret.PosY);
        Caret.PosX:= Strs.LinesLen[Caret.PosY];
      end;
    end
    else
    //jump to next line?
    if (Caret.PosX>=Length(Str)) and (AJump in [TATWordJump.Next, TATWordJump.EndOrNext]) then
    begin
      if not bMultiCaret or FOptKeyLeftRightGoToNextLineWithCarets then
      if Caret.PosY<Strs.Count-1 then
      begin
        Inc(Caret.PosY);
        Str:= Strs.Lines[Caret.PosY];
        Caret.PosX:= SGetIndentChars(Str);

        //additional jump to word end
        if AJump=TATWordJump.EndOrNext then
        begin
          Caret.PosX:= SFindWordOffset(Str, Caret.PosX, AJump, true, FOptNonWordChars, AJumpSimple);
        end;
      end;
    end
    else
    //jump from beyond eol to eol?
    if (Caret.PosX>Length(Str)) and (AJump=TATWordJump.Prev) then
    begin
      Caret.PosX:= Length(Str);
    end
    else
    //jump inside line?
    if (Caret.PosX<=Length(Str)) then
    begin
      Caret.PosX:= SFindWordOffset(Str, Caret.PosX, AJump, true, FOptNonWordChars, AJumpSimple);
    end;
  end;

  //TATCommandResult.CaretLazy is to fix CudaText #3065
  if AJump=TATWordJump.Prev then
    Result:= [TATCommandResult.CaretTop, TATCommandResult.CaretLazy]
  else
    Result:= [TATCommandResult.CaretBottom, TATCommandResult.CaretLazy];
end;

function TATSynEdit.DoCommand_GotoLineEdge(ABegin: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if not Strings.IsIndexValid(Caret.PosY) then Continue;

    if ABegin then
      Caret.PosX:= 0
    else
      Caret.PosX:= Strings.LinesLen[Caret.PosY];
  end;

  Result:= [TATCommandResult.CaretTop];
end;


function TATSynEdit.DoCommand_GotoScreenSide(ASide: TATCaretScreenSide): TATCommandResults;
const
  dx = 2;
var
  Details: TATEditorPosDetails;
  Caret: TATCaretItem;
  R: TRect;
  PntText: TPoint;
  PntCoord: TATPoint;
  NCaretX: Int64;
begin
  Result:= [];

  if Carets.Count=0 then
    DoCaretSingle(0, 0);
  Caret:= Carets[0];

  R:= FRectMainVisible;
  OffsetRect(R, -dx, -dx);

  NCaretX:= Caret.CoordX;
  //NCaretX:= R.Left; //also useful, maybe make an option?

  case ASide of
    TATCaretScreenSide.Top:
      PntCoord:= ATPoint(NCaretX, R.Top);
    TATCaretScreenSide.Bottom:
      PntCoord:= ATPoint(NCaretX, R.Bottom);
    TATCaretScreenSide.Middle:
      PntCoord:= ATPoint(NCaretX, (R.Top+R.Bottom) div 2);
  end;

  PntText:= ClientPosToCaretPos(PntCoord, Details);
  if PntText.Y<0 then exit;

  case ASide of
    TATCaretScreenSide.Top:
      begin
        if FOptScrollSmooth then
          if PntText.Y>0 then
            Inc(PntText.Y); //smooth scrolling gives partially visible 1st line
      end;
    TATCaretScreenSide.Bottom:
      begin
        if PntText.Y<Strings.Count-1 then
          Dec(PntText.Y);
      end;
  end;

  DoCaretSingle(PntText.X, PntText.Y);
  Result:= [TATCommandResult.CaretTop];
end;


function TATSynEdit.DoCommand_Cancel(AKeepLastCaret, AKeepSelection: boolean): TATCommandResults;
var
  bChanged: boolean;
  i: integer;
begin
  if AKeepLastCaret and (Carets.Count>0) then
  begin
    bChanged:= false;
    for i:= Carets.Count-2 downto 0 do
    begin
      Carets.Delete(i, false{AWithEvent});
      bChanged:= true;
    end;
    if bChanged then
      Carets.DoChanged;
  end
  else
    DoCaretSingleAsIs;

  if not AKeepSelection then
    DoSelect_None;

  ClearMouseDownVariables;
  UpdateCursor;

  Result:= [TATCommandResult.CaretTop];
end;


function TATSynEdit.DoCommand_CancelKeepFirstVisible: TATCommandResults;
var
  Caret: TATCaretItem;
  NX, NY, i: integer;
begin
  Result:= [];
  if Carets.Count<=1 then exit;

  NX:= -1;
  NY:= -1;

  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if IsPosInVisibleArea(Caret.PosX, Caret.PosY) then
    begin
      NX:= Caret.PosX;
      NY:= Caret.PosY;
      Break;
    end;
  end;

  if Strings.IsIndexValid(NY) then
  begin
    DoCaretSingle(NX, NY);
    Result:= [TATCommandResult.CaretTop];
  end
  else
    //no caret visible on screen? do usual 'Cancel'
    Result:= DoCommand_Cancel(false, false);
end;

