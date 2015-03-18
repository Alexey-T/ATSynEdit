{$ifdef none}begin end;{$endif}

procedure TATSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Cmd: integer;
  Handled: boolean;
begin
  DoPaintModeStatic;
  try
    Cmd:= GetCommandFromKey(Key, Shift);

    DoEventCommand(Cmd, Handled);
    if not Handled then
      DoCommandExec(Cmd);
  finally
    DoPaintModeBlinking;
  end;
end;

procedure TATSynEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  Str: atString;
begin
  inherited;
  Str:= UTF8Decode(UTF8Key);

  //skip control Ascii chars
  if Length(Str)>0 then
    if AnsiChar(Str[1])<' ' then Exit;

  DoCommandExec(cCommand_TextInsert, Str);
end;

function TATSynEdit.GetCommandFromKey(var Key: Word; Shift: TShiftState): integer;
var
  Str: string;
  Item: TATKeyMappingItem;
  i: integer;
begin
  Result:= 0;

  if (Key=VK_TAB) and (Shift=[]) then
  begin
    Result:= cCommand_KeyTab;
    Key:= 0;
    Exit;
  end;

  if IsEditableTextKey(Key) and ((Shift=[]) or (Shift=[ssShift])) then
    Exit;

  Str:= KeyAndShiftStateToKeyString(Key, Shift);

  for i:= 0 to FKeyMapping.Count-1 do
  begin
    Item:= FKeyMapping[i];
    if IsKeyMappingMatchedItem(Str, Item) then
    begin
      Result:= Item.Cmd;
      Key:= 0;
      Break
    end;
  end;
end;

function TATSynEdit.IsKeyMappingMatchedItem(const Str: string; const Item: TATKeyMappingItem): boolean;
begin
  Result:=
    (Str=Item.Keys1[0]) or
    (Str=Item.Keys2[0]);
end;

procedure TATSynEdit.DoCommandResults(Res: TATCommandResults);
begin
  if cResultText in Res then
  begin
    FWrapUpdateNeeded:= true;
    UpdateWrapInfo;
  end;

  DoCaretsSort;
  UpdateCaretsCoords;

  if cResultCaretLeft in Res then DoShowCaret(cEdgeLeft);
  if cResultCaretRight in Res then DoShowCaret(cEdgeRight);
  if cResultCaretTop in Res then DoShowCaret(cEdgeTop);
  if cResultCaretBottom in Res then DoShowCaret(cEdgeBottom);

  if Res*[cResultCaretAny,
    cResultCaretLeft, cResultCaretRight,
    cResultCaretTop, cResultCaretBottom]<>[] then
    DoEventCarets;

  if cResultText in Res then
    DoEventChange;

  if cResultScroll in Res then
    DoEventScroll;

  if cResultState in Res then
    DoEventState;
end;

procedure TATSynEdit.DoCommandExec(ACmd: integer; const AText: atString = '');
var
  Res: TATCommandResults;
  bSel: boolean;
  Item: TATSynCaretItem;
  i: integer;
begin
  if ACmd<=0 then Exit;
  Res:= [];
  FCaretSpecPos:= false;

  bSel:= ssShift in GetKeyShiftState;
  for i:= 0 to Carets.Count-1 do
  begin
    Item:= Carets[i];
    if not bSel then
    begin
      Item.EndX:= -1;
      Item.EndY:= -1;
    end
    else
    begin
      if Item.EndY<0 then Item.EndY:= Item.PosY;
      if Item.EndX<0 then Item.EndX:= Item.PosX;
    end;
  end;

  case ACmd of
    //most used commands
    cCommand_KeyLeft:             Res:= DoCommand_KeyLeft;
    cCommand_KeyRight:            Res:= DoCommand_KeyRight;
    cCommand_KeyUp:               Res:= DoCommand_KeyUpDown(false, 1);
    cCommand_KeyDown:             Res:= DoCommand_KeyUpDown(true, 1);
    cCommand_KeyHome:             Res:= DoCommand_KeyHome;
    cCommand_KeyEnd:              Res:= DoCommand_KeyEnd;

    cCommand_TextInsert:          Res:= DoCommand_TextInsertAtCarets(AText, false, FOvrMode);
    cCommand_KeyBackspace:        Res:= DoCommand_TextDeleteLeft(1);
    cCommand_KeyDelete:           Res:= DoCommand_TextDeleteRight(1);
    cCommand_KeyTab:              Res:= DoCommand_TextInsertTab;
    cCommand_KeyTabChar:          Res:= DoCommand_TextInsertAtCarets(#9, false, FOvrMode);
    cCommand_KeyEnter:            Res:= DoCommand_TextInsertEol(false);
    cCommand_KeyPageUp:           Res:= DoCommand_KeyUpDown(false, GetVisibleLines-1);
    cCommand_KeyPageDown:         Res:= DoCommand_KeyUpDown(true, GetVisibleLines-1);
    //end of most used

    cCommand_TextDeleteCurLine:        Res:= DoCommand_TextDeleteCurLine;
    cCommand_TextDuplicateCurLine:     Res:= DoCommand_TextDuplicateCurLine;
    cCommand_TextDeleteToLineBegin:    Res:= DoCommand_TextDeleteToLineBegin;
    cCommand_TextDeleteToLineEnd:      Res:= DoCommand_TextDeleteToLineEnd;
    cCommand_TextDeleteWordPrev:       Res:= DoCommand_TextDeleteWord(false);
    cCommand_TextDeleteWordNext:       Res:= DoCommand_TextDeleteWord(true);

    cCommand_SelectAll:                Res:= DoCommand_SelectAll;
    cCommand_GotoTextBegin:            Res:= DoCommand_GotoTextBegin;
    cCommand_GotoTextEnd:              Res:= DoCommand_GotoTextEnd;
    cCommand_GotoWordNext:             Res:= DoCommand_GotoWord(true);
    cCommand_GotoWordPrev:             Res:= DoCommand_GotoWord(false);

    cCommand_ToggleReadOnly:           Res:= DoCommand_ToggleReadOnly;
    cCommand_ToggleOvr:                Res:= DoCommand_ToggleOvr;
    cCommand_ScrollLineUp:             Res:= DoCommand_ScrollLineUpDown(false);
    cCommand_ScrollLineDown:           Res:= DoCommand_ScrollLineUpDown(true);

    cCommand_ClipboardPaste:           Res:= DoCommand_ClipboardPaste(false);
    cCommand_ClipboardPaste_KeepCaret: Res:= DoCommand_ClipboardPaste(true);

    cCommand_CaretsRemove:             Res:= DoCommand_CaretsRemove;
  end;

  DoCommandResults(Res);
end;



