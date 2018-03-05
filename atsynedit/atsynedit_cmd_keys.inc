{$ifdef nnnn}begin end;{$endif}

function TATSynEdit.DoCommand_KeyHome: TATCommandResults;
var
  i, NIndent, NWrapped: integer;
  Caret: TATCaretItem;
  Pnt: TPoint;
  Details: TATPosDetails;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    NWrapped:= 0;
    NIndent:= 0;

    if FOptKeyHomeEndNavigateWrapped and (FWrapMode<>cWrapOff) then
    begin
      Pnt.X:= 0;
      Pnt.Y:= Caret.CoordY;
      Pnt:= ClientPosToCaretPos(Pnt, Details);
      NWrapped:= Pnt.X;
    end;

    if FOptKeyHomeToNonSpace then
      NIndent:= SGetIndentChars(Strings.Lines[Caret.PosY]);

    if (NWrapped>0) and (Caret.PosX>NWrapped) then
      Caret.PosX:= NWrapped
    else
    if (NIndent>0) and (Caret.PosX>NIndent) then
      Caret.PosX:= NIndent
    else
      Caret.PosX:= 0;
  end;

  Result:= [cResultCaretLeft];
end;

function TATSynEdit.DoCommand_KeyEnd: TATCommandResults;
var
  Caret: TATCaretItem;
  Str: atString;
  Pnt: TPoint;
  i, NLen, NWrapped: integer;
  Details: TATPosDetails;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    Str:= Strings.Lines[Caret.PosY];
    NLen:= 0;
    NWrapped:= 0;

    if FOptKeyHomeEndNavigateWrapped and (FWrapMode<>cWrapOff) then
    begin
      Pnt.X:= ClientWidth;
      Pnt.Y:= Caret.CoordY;
      Pnt:= ClientPosToCaretPos(Pnt, Details);
      if Pnt.X<Length(Str) then
        NWrapped:= Pnt.X-1;
        //-1 here: need to jump not to last chr but to last-1 chr
        //(like Synwrite; to not blink caret at next part of wrapped line)
    end;

    if FOptKeyEndToNonSpace then
      NLen:= SGetNonSpaceLength(Str);

    if (NWrapped>0) and (Caret.PosX<NWrapped) then
      Caret.PosX:= NWrapped
    else
    if (NLen>0) and (Caret.PosX<NLen) then
      Caret.PosX:= NLen
    else
      Caret.PosX:= Length(Str);
  end;

  Result:= [cResultCaretRight];
end;

function TATSynEdit.DoCommand_KeyLeft(ASelCommand: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  i: integer;
begin
  Result:= [cResultCaretLeft];

  if not ASelCommand then
  begin
    if FOptKeyLeftRightSwapSel then
      if DoCaretSwapEdge(true) then Exit;
    DoSelect_None;
  end;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if (Caret.PosX>0) then
      Dec(Caret.PosX)
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
  i: integer;
begin
  Result:= [cResultCaretRight];

  if not ASelCommand then
  begin
    if FOptKeyLeftRightSwapSel then
      if DoCaretSwapEdge(false) then Exit;
    DoSelect_None;
  end;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if (Caret.PosX<Strings.LinesLen[Caret.PosY]) or FCaretVirtual then
      Inc(Caret.PosX)
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
  i, Y: integer;
begin
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    Y:= Caret.PosY;

    repeat
      Y:= Y+ALines*BoolToPlusMinusOne(ADown);
    until not Strings.IsIndexValid(Y) or not IsLineFolded(Y);

    if not IsLineFolded(Y) then
    begin
      if Y<0 then Y:= GetFirstUnfoldedLineNumber;
      if Y>=Strings.Count then Y:= GetLastUnfoldedLineNumber;
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
  Details: TATPosDetails;
  i: integer;
const
  cGapActions: array[boolean] of TATGapCoordAction =
    (cGapCoordIgnore, cGapCoordMoveDown);
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];

    if IsPosFolded(Caret.PosX, Caret.PosY) then
    begin
      Caret.PosX:= 0;
      Caret.PosY:= GetNextUnfoldedLineNumber(Caret.PosY, ADown);
      Continue;
    end;

    Pnt.X:= Caret.CoordX;
    if FOptKeyUpDownKeepColumn and (Caret.SavedX>0) then
      Pnt.X:= Caret.SavedX;

    Pnt.Y:= Caret.CoordY + ALines*FCharSize.Y*BoolToPlusMinusOne(ADown);
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
  FScrollHorz.NPos:= 0;
  FScrollVert.NPos:= 0;

  Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_GotoTextEnd: TATCommandResults;
var
  NLine: integer;
begin
  if FWrapInfo.Count>0 then
  begin
    NLine:= FWrapInfo[FWrapInfo.Count-1].NLineIndex;
    DoCaretSingle(Strings.LinesLen[NLine], NLine, false);
  end;

  Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_ScrollByDelta(ALines, AColumns: integer;
  AKeepCaretOnScreen: boolean): TATCommandResults;
begin
  DoScrollByDelta(AColumns, ALines);
  Result:= [cResultScroll];

  if AKeepCaretOnScreen then
    if DoCaretsKeepOnScreen then
      Include(Result, cResultCaretAny);
end;


function TATSynEdit.DoCommand_GotoWord(AJump: TATWordJump; AJumpSimple: boolean
  ): TATCommandResults;
var
  Caret: TATCaretItem;
  Str: atString;
  i: integer;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    if not Strings.IsIndexValid(Caret.PosY) then Continue;
    Str:= Strings.Lines[Caret.PosY];

    //jump to prev line?
    if (Caret.PosX=0) and (AJump=cWordjumpToPrev) then
    begin
      if Caret.PosY>0 then
      begin
        Dec(Caret.PosY);
        Caret.PosX:= Strings.LinesLen[Caret.PosY];
      end;
    end
    else
    //jump to next line?
    if (Caret.PosX>=Length(Str)) and (AJump in [cWordjumpToNext, cWordjumpToEndOrNext]) then
    begin
      if Caret.PosY<Strings.Count-1 then
      begin
        Inc(Caret.PosY);
        Caret.PosX:= SGetIndentChars(Strings.Lines[Caret.PosY]);

        //additional jump to word end
        if AJump=cWordjumpToEndOrNext then
        begin
          Str:= Strings.Lines[Caret.PosY];
          Caret.PosX:= SFindWordOffset(Str, Caret.PosX, AJump, true, FOptWordChars, AJumpSimple);
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
      Caret.PosX:= SFindWordOffset(Str, Caret.PosX, AJump, true, FOptWordChars, AJumpSimple);
    end;
  end;

  if AJump=cWordjumpToPrev then
    Result:= [cResultCaretTop]
  else
    Result:= [cResultCaretBottom];
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
  P: TPoint;
  Details: TATPosDetails;
begin
  Result:= [];

  case ASide of
    cScreenSideTop:
      P:= Point(FRectMain.Left+dx, FRectMain.Top+dx);
    cScreenSideBottom:
      P:= Point(FRectMain.Left+dx, FRectMain.Bottom-dx);
    cScreenSideMiddle:
      P:= Point(FRectMain.Left+dx, (FRectMain.Top+FRectMain.Bottom) div 2);
  end;

  if P.Y<0 then exit;
  P:= ClientPosToCaretPos(P, Details);
  if P.Y>=0 then
  begin
    DoCaretSingle(P.X, P.Y);
    Result:= [cResultCaretTop];
  end;
end;


function TATSynEdit.DoCommand_Cancel: TATCommandResults;
begin
  DoCaretSingleAsIs;
  DoSelect_None;

  FMouseDragDropping:= false;
  UpdateCursor;

  Result:= [cResultCaretTop];
end;


