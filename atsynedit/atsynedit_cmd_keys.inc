{$ifdef nnnn}begin end;{$endif}

function TATSynEdit.DoCommand_KeyHome: TATCommandResults;
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  Details: TATEditorPosDetails;
  NLine, NIndent, NWrapped, i: integer;
  IndentKind: TATLineIndentKind;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    NWrapped:= 0;
    NIndent:= 0;
    NLine:= Caret.PosY;
    if not Strings.IsIndexValid(NLine) then Continue;

    if FOptKeyHomeEndNavigateWrapped and (FWrapMode<>cWrapOff) then
    begin
      Pnt.X:= 0;
      Pnt.Y:= Caret.CoordY;
      Pnt:= ClientPosToCaretPos(Pnt, Details);
      NWrapped:= Pnt.X;
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
    FScrollHorz.NPixelOffset:= 0;

  Result:= [cResultCaretLeft];
end;

function TATSynEdit.DoCommand_KeyEnd: TATCommandResults;
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  NLine, NLenTotal, NLen, NWrapped, i: integer;
  Details: TATEditorPosDetails;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    NLine:= Caret.PosY;
    if not Strings.IsIndexValid(NLine) then Continue;

    NLenTotal:= Strings.LinesLen[NLine];
    NLen:= 0;
    NWrapped:= 0;

    if FOptKeyHomeEndNavigateWrapped and (FWrapMode<>cWrapOff) then
    begin
      Pnt.X:= ClientWidth;
      Pnt.Y:= Caret.CoordY;
      Pnt:= ClientPosToCaretPos(Pnt, Details);
      if Pnt.X<NLenTotal then
        NWrapped:= Pnt.X-1;
        //-1 here: need to jump not to last chr but to last-1 chr
        //(like Synwrite; to not blink caret at next part of wrapped line)
    end;

    if FOptKeyEndToNonSpace then
      NLen:= Strings.LineLenWithoutSpace(NLine);

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

  Result:= [cResultCaretRight];
end;

function TATSynEdit.DoCommand_KeyLeft(ASelCommand: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  bMultiCarets: boolean;
  i: integer;
begin
  //changed because of issue CudaText#3065
  Result:= [cResultCaretLeft, cResultCaretLazy];

  bMultiCarets:= FCarets.Count>1;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if not Strings.IsIndexValid(Caret.PosY) then Continue;

    if not ASelCommand and FOptKeyLeftRightSwapSel and DoCaretSwapEdge(Caret, true) then
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
  end;
end;

function TATSynEdit.DoCommand_KeyRight(ASelCommand: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  bMultiCarets: boolean;
  i: integer;
begin
  //changed because of issue CudaText#3065
  Result:= [cResultCaretRight, cResultCaretLazy];

  bMultiCarets:= FCarets.Count>1;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if not Strings.IsIndexValid(Caret.PosY) then Continue;

    if not ASelCommand and FOptKeyLeftRightSwapSel and DoCaretSwapEdge(Caret, false) then
    begin
      //none
    end
    else
    if (Caret.PosX<Strings.LinesLen[Caret.PosY]) or FCaretVirtual then
      Inc(Caret.PosX)
    else
    if bMultiCarets and not FOptKeyLeftRightGoToNextLineWithCarets then
    begin
      //none
    end
    else
    if (Caret.PosY<Strings.Count-1) then
    begin
      Caret.PosX:= 0;
      Inc(Caret.PosY);
    end;
  end;
end;

function TATSynEdit.DoCommand_KeyUpDown(ADown: boolean; ALines: integer;
  AKeepRelativePos: boolean): TATCommandResults;
var
  NRelative: integer;
begin
  FCaretSpecPos:= true;

  if AKeepRelativePos then
    NRelative:= LinesFromTop;

  //don't check here FWrapMode<>cWrapOff
  if FOptKeyUpDownNavigateWrapped then
    Result:= DoCommand_KeyUpDown_Wrapped(ADown, ALines)
  else
    Result:= DoCommand_KeyUpDown_NextLine(ADown, ALines);

  if AKeepRelativePos then
    LinesFromTop:= NRelative;
end;

function TATSynEdit.DoCommand_KeyUpDown_NextLine(ADown: boolean; ALines: integer): TATCommandResults;
var
  Caret: TATCaretItem;
  NCount: integer;
  i, Y: integer;
begin
  NCount:= Strings.Count;
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    Y:= Caret.PosY;

    repeat
      Y:= Y+ALines*BoolToPlusMinusOne[ADown];
    until not Strings.IsIndexValid(Y) or not IsLineFolded(Y);

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
            Caret.PosX:= Strings.LinesLen[Y];
      end;
      Caret.PosY:= Y;
    end;
  end;

  if ADown then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_KeyUpDown_Wrapped(ADown: boolean; ALines: integer): TATCommandResults;
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  Details: TATEditorPosDetails;
  NWrapIndex, i: integer;
const
  cGapActions: array[boolean] of TATEditorGapCoordAction =
    (cGapCoordIgnore, cGapCoordMoveDown);
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];

    if IsPosFolded(Caret.PosX, Caret.PosY) then
    begin
      Caret.PosX:= 0;
      Caret.PosY:= Unfolded_NextLineNumber(Caret.PosY, ADown);
      Continue;
    end;

    Pnt.X:= Caret.CoordX;
    if FOptKeyUpDownKeepColumn and (Caret.SavedX>0) then
    begin
      Pnt.X:= Caret.SavedX;
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
        Caret.PosX:= Strings.LinesLen[Strings.Count-1];
      Continue;
    end;

    //increase/decrease Caret.CoordY and calculate pos from it
    Pnt.Y:= Caret.CoordY + ALines*FCharSize.Y*BoolToPlusMinusOne[ADown];
    Pnt:= ClientPosToCaretPos(Pnt, Details, cGapActions[ADown]);
    if Pnt.Y<0 then Continue;

    if Details.EndOfWrappedLine and (Pnt.X>0) then
      Dec(Pnt.X); //-1 so up/down won't jump to eol pos (caret may paint on next line)

    Caret.PosX:= Pnt.X;
    Caret.PosY:= Pnt.Y;
  end;

  if ADown then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
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

  Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_GotoTextEnd: TATCommandResults;
var
  NLine: integer;
begin
  {
  //worked not reliable, e.g. in CudaText fmConsole.memo
  if FWrapInfo.Count>0 then
  begin
    NLine:= FWrapInfo[FWrapInfo.Count-1].NLineIndex;
    DoCaretSingle(Strings.LinesLen[NLine], NLine, false);
  end;
  }

  NLine:= Strings.Count-1;
  DoCaretSingle(Strings.LinesLen[NLine], NLine, false);

  Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_ScrollByDelta(ALines, AColumns: integer;
  AKeepCaretOnScreen: boolean): TATCommandResults;
begin
  DoScrollByDelta(AColumns, ALines);
  Result:= [cResultScroll];

  if AKeepCaretOnScreen then
    if DoCaretsKeepOnScreen(ALines>0) then
      Include(Result, cResultCaretAny);
end;


function TATSynEdit.DoCommand_ScrollToBeginOrEnd(AToBegin: boolean): TATCommandResults;
begin
  DoScrollToBeginOrEnd(AToBegin);
  Result:= [cResultScroll];
end;


function TATSynEdit.DoCommand_GotoWord(AJump: TATWordJump; AJumpSimple: boolean
  ): TATCommandResults;
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
    if (Caret.PosX=0) and (AJump=cWordjumpToPrev) then
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
    if (Caret.PosX>=Length(Str)) and (AJump in [cWordjumpToNext, cWordjumpToEndOrNext]) then
    begin
      if not bMultiCaret or FOptKeyLeftRightGoToNextLineWithCarets then
      if Caret.PosY<Strs.Count-1 then
      begin
        Inc(Caret.PosY);
        Str:= Strs.Lines[Caret.PosY];
        Caret.PosX:= SGetIndentChars(Str);

        //additional jump to word end
        if AJump=cWordjumpToEndOrNext then
        begin
          Caret.PosX:= SFindWordOffset(Str, Caret.PosX, AJump, true, FOptNonWordChars, AJumpSimple);
        end;
      end;
    end
    else
    //jump from beyond eol to eol?
    if (Caret.PosX>Length(Str)) and (AJump=cWordjumpToPrev) then
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

  //cResultCaretLazy is to fix CudaText #3065
  if AJump=cWordjumpToPrev then
    Result:= [cResultCaretTop, cResultCaretLazy]
  else
    Result:= [cResultCaretBottom, cResultCaretLazy];
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

  Result:= [cResultCaretTop];
end;


function TATSynEdit.DoCommand_GotoScreenSide(ASide: TATCaretScreenSide): TATCommandResults;
const
  dx = 2;
var
  R: TRect;
  P: TPoint;
  Details: TATEditorPosDetails;
begin
  Result:= [];

  R:= FRectMainVisible;
  OffsetRect(R, -dx, -dx);

  case ASide of
    cScreenSideTop:
      P:= Point(R.Left, R.Top);
    cScreenSideBottom:
      P:= Point(R.Left, R.Bottom);
    cScreenSideMiddle:
      P:= Point(R.Left, (R.Top+R.Bottom) div 2);
  end;

  P:= ClientPosToCaretPos(P, Details);
  if P.Y<0 then exit;

  case ASide of
    cScreenSideTop:
      Inc(P.Y); //smooth scrolling gives partially visible 1st line
    cScreenSideBottom:
      Dec(P.Y);
  end;

  DoCaretSingle(P.X, P.Y);
  Result:= [cResultCaretTop];
end;


function TATSynEdit.DoCommand_Cancel: TATCommandResults;
begin
  DoCaretSingleAsIs;
  DoSelect_None;

  FMouseDragDropping:= false;
  FMouseDragDroppingReal:= false;
  UpdateCursor;

  Result:= [cResultCaretTop];
end;


