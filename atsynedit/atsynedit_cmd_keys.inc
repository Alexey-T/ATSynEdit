{$ifdef nnnn}begin end;{$endif}

function TATSynEdit.DoCommand_KeyHome: TATCommandResults;
var
  i, NIndent, NWrapped: integer;
  Caret: TATCaretItem;
  Pnt: TPoint;
  EolPos: boolean;
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
      Pnt:= ClientPosToCaretPos(Pnt, EolPos);
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
  EolPos: boolean;
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
      Pnt:= ClientPosToCaretPos(Pnt, EolPos);
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
      Caret.PosX:= Length(Strings.Lines[Caret.PosY]);
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
    if (Caret.PosX<Length(Strings.Lines[Caret.PosY])) or FCaretVirtual then
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
    NRelative:= ScrollTopRelative;

  //don't check here FWrapMode<>cWrapOff
  if FOptKeyUpDownNavigateWrapped then
    Result:= DoCommand_KeyUpDown_Wrapped(ADown, ALines)
  else
    Result:= DoCommand_KeyUpDown_NextLine(ADown, ALines);

  if AKeepRelativePos then
    ScrollTopRelative:= NRelative;
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
    until not Strings.IsIndexValid(Y) or not IsLineCollapsedFull(Y);

    if not IsLineCollapsedFull(Y) then
    begin
      if Y<0 then Y:= GetFirstUncollapsedLineNumber;
      if Y>=Strings.Count then Y:= GetLastUncollapsedLineNumber;
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
  i: integer;
  EolPos: boolean;
begin
  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];

    Pnt.X:= Caret.CoordX;
    if FOptKeyUpDownKeepColumn and (Caret.CoordColumn>0) then
      Pnt.X:= Caret.CoordColumn;

    Pnt.Y:= Caret.CoordY + ALines*FCharSize.Y*BoolToPlusMinusOne(ADown);
    Pnt:= ClientPosToCaretPos(Pnt, EolPos);
    if Pnt.Y<0 then Continue;

    if EolPos and (Pnt.X>0) then
      Dec(Pnt.X); //-1 so up/down won't jump to eol pos (caret may paint on next line)

    Caret.PosX:= Pnt.X;
    Caret.PosY:= Pnt.Y;
  end;

  if ADown then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_GotoTextBegin: TATCommandResults;
var
  Item: TATSynWrapItem;
begin
  Item:= FWrapInfo[0];
  if Assigned(Item) then
    DoCaretSingle(0, Item.NLineIndex);

  FScrollHorz.NPos:= 0;
  FScrollVert.NPos:= 0;

  Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_GotoTextEnd: TATCommandResults;
var
  Item: TATSynWrapItem;
begin
  Item:= FWrapInfo[FWrapInfo.Count-1];
  if Assigned(Item) then
    DoCaretSingle(Item.NLength, Item.NLineIndex);

  Result:= [cResultCaretBottom];
end;

function TATSynEdit.DoCommand_ScrollVert(ALines: integer): TATCommandResults;
begin
  DoScrollByDelta(0, ALines);
  Result:= [cResultScroll];
end;


function TATSynEdit.DoCommand_GotoWord(ANext: boolean): TATCommandResults;
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
    if (Caret.PosX=0) and (not ANext) then
    begin
      if Caret.PosY>0 then
      begin
        Dec(Caret.PosY);
        Caret.PosX:= Length(Strings.Lines[Caret.PosY]);
      end;
    end
    else
    //jump to next line?
    if (Caret.PosX>=Length(Str)) and ANext then
    begin
      if Caret.PosY<Strings.Count-1 then
      begin
        Inc(Caret.PosY);
        Caret.PosX:= SGetIndentChars(Strings.Lines[Caret.PosY]);
      end;
    end
    else
    //jump from beyond eol to eol?
    if (Caret.PosX>Length(Str)) and (not ANext) then
    begin
      Caret.PosX:= Length(Str);
    end
    else
    //jump inside line?
    if (Caret.PosX<=Length(Str)) then
    begin
      Caret.PosX:= SFindWordOffset(Str, Caret.PosX, ANext, true, FOptWordChars);
    end;
  end;

  if ANext then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_Cancel: TATCommandResults;
begin
  DoCaretSingleAsIs;
  DoSelect_None;

  FMouseDragDropping:= false;
  UpdateCursor;

  Result:= [cResultCaretTop];
end;


