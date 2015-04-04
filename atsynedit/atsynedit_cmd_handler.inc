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
  Shortcut: TShortcut;
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

  Shortcut:= KeyToShortCut(Key, Shift);

  for i:= 0 to FKeyMapping.Count-1 do
  begin
    Item:= FKeyMapping[i];
    if IsKeyMappingMatchedItem(Shortcut, Item) then
    begin
      Result:= Item.Command;
      Key:= 0;
      Break
    end;
  end;
end;

function TATSynEdit.IsKeyMappingMatchedItem(const AKey: TShortcut; const AItem: TATKeyMappingItem): boolean;
begin
  Result:=
    (AKey=AItem.Keys1[0]) or
    (AKey=AItem.Keys2[0]);
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
  begin
    if not (cResultKeepColumnSel in Res) then
      FSelRect:= cRectEmpty;
    DoEventCarets;
  end;

  if cResultText in Res then
  begin
    FSelRect:= cRectEmpty;
    DoEventChange;
  end;

  if cResultScroll in Res then
    DoEventScroll;

  if cResultState in Res then
    DoEventState;
end;

procedure TATSynEdit.DoCommandExec(ACmd: integer; const AText: atString = '');
var
  Res: TATCommandResults;
  Item: TATCaretItem;
  i: integer;
begin
  if ACmd<=0 then Exit;
  Res:= [];
  FCaretSpecPos:= false;

  //---handle special command bits (cCmdNNNNN)
  if ACmd and cCmdSelKeep <> 0 then
    for i:= 0 to Carets.Count-1 do
    begin
      Item:= Carets[i];
      if Item.EndY<0 then Item.EndY:= Item.PosY;
      if Item.EndX<0 then Item.EndX:= Item.PosX;
    end;

  if ACmd and cCmdSelReset <> 0 then
    DoSelect_None;

  if ACmd and cCmdCaret <> 0 then
    Strings.SetGroupMark;

  //debug
  //if FCaretMoved then Beep;
  //--------

  case ACmd of
    //most used commands
    cCommand_KeyLeft:             Res:= DoCommand_KeyLeft(false);
    cCommand_KeyLeft_Sel:         Res:= DoCommand_KeyLeft(true);
    cCommand_KeyRight:            Res:= DoCommand_KeyRight(false);
    cCommand_KeyRight_Sel:        Res:= DoCommand_KeyRight(true);
    cCommand_KeyUp,
    cCommand_KeyUp_Sel:           Res:= DoCommand_KeyUpDown(false, 1);
    cCommand_KeyDown,
    cCommand_KeyDown_Sel:         Res:= DoCommand_KeyUpDown(true, 1);
    cCommand_KeyHome,
    cCommand_KeyHome_Sel:         Res:= DoCommand_KeyHome;
    cCommand_KeyEnd,
    cCommand_KeyEnd_Sel:          Res:= DoCommand_KeyEnd;
    cCommand_KeyPageUp,
    cCommand_KeyPageUp_Sel:       Res:= DoCommand_KeyUpDown(false, GetPageLines);
    cCommand_KeyPageDown,
    cCommand_KeyPageDown_Sel:     Res:= DoCommand_KeyUpDown(true, GetPageLines);

    cCommand_ColSelectLeft:      Res:= DoCommand_SelectColumn(cDirLeft);
    cCommand_ColSelectRight:     Res:= DoCommand_SelectColumn(cDirRight);
    cCommand_ColSelectUp:        Res:= DoCommand_SelectColumn(cDirUp);
    cCommand_ColSelectDown:      Res:= DoCommand_SelectColumn(cDirDown);

    cCommand_TextInsert:          Res:= DoCommand_TextInsertAtCarets(AText, false, FOver, false);
    cCommand_TextInsertTabChar:   Res:= DoCommand_TextInsertAtCarets(#9, false, FOver, false);
    cCommand_KeyBackspace:        Res:= DoCommand_TextDeleteLeft(1);
    cCommand_KeyDelete:           Res:= DoCommand_TextDelete;
    cCommand_KeyTab:              Res:= DoCommand_TextTabulation;
    cCommand_KeyEnter:            Res:= DoCommand_TextInsertEol(false);

    cCommand_Undo:                Res:= DoCommand_Undo;
    cCommand_Redo:                Res:= DoCommand_Redo;
    //end of most used

    cCommand_TextDeleteSelection:      Res:= DoCommand_TextDeleteSelection;
    cCommand_TextDeleteLine:           Res:= DoCommand_TextDeleteLines;
    cCommand_TextDuplicateLine:        Res:= DoCommand_TextDuplicateLine;
    cCommand_TextDeleteToLineBegin:    Res:= DoCommand_TextDeleteToLineBegin;
    cCommand_TextDeleteToLineEnd:      Res:= DoCommand_TextDeleteToLineEnd;
    cCommand_TextDeleteToTextEnd:      Res:= DoCommand_TextDeleteToFileEnd;
    cCommand_TextDeleteWordPrev:       Res:= DoCommand_TextDeleteWord(false);
    cCommand_TextDeleteWordNext:       Res:= DoCommand_TextDeleteWord(true);

    cCommand_SelectAll:                Res:= DoCommand_SelectAll;
    cCommand_SelectWords:              Res:= DoCommand_SelectWords;
    cCommand_SelectLines:              Res:= DoCommand_SelectLines;
    cCommand_SelectNone:               Res:= [cResultCaretAny];

    cCommand_GotoTextBegin,
    cCommand_GotoTextBegin_Sel:        Res:= DoCommand_GotoTextBegin;
    cCommand_GotoTextEnd,
    cCommand_GotoTextEnd_Sel:          Res:= DoCommand_GotoTextEnd;
    cCommand_GotoWordNext,
    cCommand_GotoWordNext_Sel:         Res:= DoCommand_GotoWord(true);
    cCommand_GotoWordPrev,
    cCommand_GotoWordPrev_Sel:         Res:= DoCommand_GotoWord(false);

    cCommand_ToggleReadOnly:           Res:= DoCommand_ToggleReadOnly;
    cCommand_ToggleOverwrite:          Res:= DoCommand_ToggleOverwrite;

    cCommand_TextIndent:               Res:= DoCommand_TextIndentUnindent(true);
    cCommand_TextUnindent:             Res:= DoCommand_TextIndentUnindent(false);

    cCommand_ScrollLineUp:             Res:= DoCommand_ScrollVert(-1);
    cCommand_ScrollLineDown:           Res:= DoCommand_ScrollVert(1);
    cCommand_ScrollToCaretTop:         Res:= [cResultCaretTop];
    cCommand_ScrollToCaretBottom:      Res:= [cResultCaretBottom];
    cCommand_ScrollToCaretLeft:        Res:= [cResultCaretLeft];
    cCommand_ScrollToCaretRight:       Res:= [cResultCaretRight];

    cCommand_ClipboardCopy:            Res:= DoCommand_ClipboardCopy;
    cCommand_ClipboardCopyAdd:         Res:= DoCommand_ClipboardCopy(true);
    cCommand_ClipboardCut:             Res:= DoCommand_ClipboardCut;
    cCommand_ClipboardPaste:           Res:= DoCommand_ClipboardPaste(false, false);
    cCommand_ClipboardPaste_Select:    Res:= DoCommand_ClipboardPaste(false, true);
    cCommand_ClipboardPaste_KeepCaret: Res:= DoCommand_ClipboardPaste(true, false);
    cCommand_ClipboardPaste_Column:          Res:= DoCommand_ClipboardPasteColumnBlock(false);
    cCommand_ClipboardPaste_ColumnKeepCaret: Res:= DoCommand_ClipboardPasteColumnBlock(true);

    cCommand_Cancel:                   Res:= DoCommand_Cancel;

    cCommand_CaretsExtendUpLine:       Res:= DoCommand_CaretsExtend(false, 1);
    cCommand_CaretsExtendUpPage:       Res:= DoCommand_CaretsExtend(false, GetPageLines);
    cCommand_CaretsExtendUpToTop:      Res:= DoCommand_CaretsExtend(false, Strings.Count);
    cCommand_CaretsExtendDownLine:     Res:= DoCommand_CaretsExtend(true, 1);
    cCommand_CaretsExtendDownPage:     Res:= DoCommand_CaretsExtend(true, GetPageLines);
    cCommand_CaretsExtendDownToEnd:    Res:= DoCommand_CaretsExtend(true, Strings.Count);

    cCommand_RepeatTextCommand:        DoCommandExec(FLastTextCmd, FLastTextCmdText);
  end;

  if cResultText in Res then
    if ACmd<>cCommand_RepeatTextCommand then
    begin
      FLastTextCmd:= ACmd;
      FLastTextCmdText:= AText;
    end;

  DoCommandResults(Res);
end;



