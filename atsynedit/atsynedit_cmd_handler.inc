{$ifdef none}begin end;{$endif}

procedure TATSynEdit.DoMenuText;
var
  P: TPoint;
begin
  P:= ClientToScreen(Point(0, 0));
  if Assigned(FMenuText) then
    FMenuText.PopUp(P.X, P.Y)
  else
  begin
    InitMenuStd;
    FMenuStd.PopUp(P.X, P.Y);
  end;
end;

procedure TATSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  NCmd: integer;
begin
  if FLockInput then exit;
  inherited;
  if Key=0 then Exit;

  //special handle for spec keys
  case Key of
    VK_SHIFT,
    VK_CONTROL,
    VK_MENU, //3 usual state keys
    VK_LSHIFT..VK_RMENU: //also 6 state keys
      begin
        //ignore Ctrl/Alt/Shift to not hide caret
        exit;
      end;
    VK_TAB:
      begin
        if not WantTabs then exit;
      end;
    VK_RETURN:
      begin
        if not WantReturns then exit;
      end;
    VK_APPS:
      begin
        DoMenuText;
        exit;
      end;
  end;

  NCmd:= GetCommandFromKey(Key, Shift);
  if NCmd>0 then
  begin
    TimerBlinkDisable;
    DoCommand(NCmd);
    TimerBlinkEnable;
  end;
end;

class function TATSynEdit.CheckInputForNumberOnly(const S: UnicodeString; X: integer; ch: WideChar; AllowNegative: boolean): boolean;
var
  bMinusPresent: boolean;
begin
  Result:= false;
  bMinusPresent:= Pos('-', S)>0;
  case ch of
    '-':
      begin
        if not AllowNegative then exit(false);
        if S='' then
          exit(true);
        if (X=0) and not bMinusPresent then
          exit(true);
        exit(false);
      end;
  '0'..'9':
    begin
      if S='' then
        exit(true);
      if (X=0) and bMinusPresent then
        exit(false);
      exit(true);
    end;
  end;
end;

procedure TATSynEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  Str, StrLine: atString;
  Allow: boolean;
  Caret: TATCaretItem;
begin
  if FLockInput then exit;
  inherited;

  //skip control Ascii chars
  if Ord(UTF8Key[1])<32 then Exit;

  Str:= Utf8Decode(Utf8Key);
  if Str='' then Exit;

  if Assigned(FOnCheckInput) then
  begin
    Allow:= true;
    FOnCheckInput(Self, Str[1], Allow);
    if not Allow then Exit;
  end;

  if FOptInputNumberOnly then
  begin
    if Length(Str)<>1 then exit;
    if Carets.Count<>1 then exit;
    Caret:= Carets[0];
    if not Strings.IsIndexValid(Caret.PosY) then exit;
    StrLine:= Strings.Lines[Caret.PosY];
    if Caret.PosX>Length(StrLine) then exit;
    if not CheckInputForNumberOnly(
      StrLine,
      Caret.PosX,
      Str[1],
      FOptInputNumberAllowNegative) then exit;
  end;

  TimerBlinkDisable;
  DoCommand(cCommand_TextInsert, Str);
  TimerBlinkEnable;

  Utf8Key:= '';
end;

function TATSynEdit.GetCommandFromKey(var Key: Word; Shift: TShiftState): integer;
var
  Shortcut: TShortcut;
  bComboPressing: boolean;
begin
  Result:= 0;

  if (Key=VK_TAB) and (Shift=[]) then
  begin
    Result:= cCommand_KeyTab;
    Key:= 0;
    Exit;
  end;

  //support combo-hotkeys with simple letter
  // https://github.com/Alexey-T/CudaText/issues/2690
  bComboPressing:= (FKeymapHistory.Length>0);
  if not bComboPressing then
    if IsEditorHotkeyForText(Key) and ((Shift=[]) or (Shift=[ssShift])) then
      Exit;

  Shortcut:= KeyToShortCut(Key, Shift);
  Result:= FKeymap.GetCommandFromShortcut(Shortcut, FKeymapHistory);
  if Result>0 then
    Key:= 0;
end;

function TATSynEdit.IsCommandResults_CaretMove(const Res: TATCommandResults): boolean; inline;
begin
  Result:= Res*[
    cResultCaretAny,
    cResultCaretLeft,
    cResultCaretRight,
    cResultCaretTop,
    cResultCaretBottom
    ] <> [];
end;

procedure TATSynEdit.DoCommandResults(ACmd: integer; Res: TATCommandResults);
var
  Caret: TATCaretItem;
  St: TATStrings;
  bTextChanged: boolean;
  bNeedRepaint, bAllowRepaint: boolean;
  bUndoRedo: boolean;
  bAllowCaretScroll: boolean;
begin
  bNeedRepaint:= false;
  bAllowRepaint:= true;
  bTextChanged:= cResultText in Res;
  bUndoRedo:= cResultUndoRedo in Res;

  //if one-line, try to make right edge of text near the edge of control, like Win inputs
  if ModeOneLine then
    if bTextChanged then
    begin
      FScrollHorz.SetZero;
      Include(Res, cResultCaretTop);
    end;

  if bTextChanged or
     (cResultFoldChange in Res) then
  begin
    bNeedRepaint:= true;
    FWrapUpdateNeeded:= true;
    UpdateWrapInfo;
  end;

  St:= Strings;
  if bTextChanged or St.ModifiedRecent then
    St.ActionSaveLastEditionPos; //clears St.ModifiedRecent

  if bTextChanged then
  begin
    bAllowRepaint:= FOptAllowRepaintOnTextChange;
    if FOptMouseHideCursor then
      if PtInRect(FRectMain, ScreenToClient(Mouse.CursorPos)) then
        SetCursor(crNone);
  end;

  case ACmd of
    cCommand_KeyLeft,
    cCommand_KeyLeft_Sel,
    cCommand_CaretsExtendDownLine..cCommand_CaretsExtendUpToTop:
      DoCaretsFixForSurrogatePairs(false);
    cCommand_KeyRight,
    cCommand_KeyRight_Sel:
      DoCaretsFixForSurrogatePairs(true);
  end;

  DoCaretsFixIfInsideFolded;
  DoCaretsSort;

  if IsCommandResults_CaretMove(Res) then
  begin
    if Carets.Count=1 then
      bAllowCaretScroll:= true
    else
    if cResultCaretLazy in Res then
      bAllowCaretScroll:= not IsCaretOnVisibleRect
    else
      bAllowCaretScroll:= true;

    if bAllowCaretScroll then
    begin
      if cResultCaretTop in Res then
        DoGotoCaret(cEdgeTop, bUndoRedo, false, false)
      else
      if cResultCaretBottom in Res then
        DoGotoCaret(cEdgeBottom, bUndoRedo, false, false)
      else
      if cResultCaretLeft in Res then
        DoGotoCaret(cEdgeLeft, bUndoRedo, false, false)
      else
      if cResultCaretRight in Res then
        DoGotoCaret(cEdgeRight, bUndoRedo, false, false);
    end;

    if not (cResultKeepColumnSel in Res) then
      FSelRect:= cRectEmpty;
    DoEventCarets;

    //update pixel coords, so that plugin command
    //  for in in range(N): ed.cmd(cCommand_KeyDown)
    //will move-down N times
    UpdateCaretsCoords;
  end;

  if bTextChanged then
  begin
    if not (cResultKeepColumnSel in Res) then
      FSelRect:= cRectEmpty;
    DoEventChange;
  end;

  if cResultState in Res then
    DoEventState;

  //when caret goes to left/top edge, must reset the NPixelOffset
  if FOptScrollSmooth and (Carets.Count=1) then
  begin
    Caret:= Carets[0];
    if FScrollVert.NPixelOffset>0 then
      if Caret.CoordY=FRectMain.Top then
        FScrollVert.NPixelOffset:= 0;
    if FScrollHorz.NPixelOffset>0 then
      if Caret.CoordX=FRectMain.Left then
        FScrollHorz.NPixelOffset:= 0;
  end;

  if bNeedRepaint then
  begin
    //- don't call Update, because it does unneeded Invalidate (we call Paint below so Invalidate not needed)
    //- Paint call needed, to repaint after "Fold all" (Linux gtk2)
    //and it helps to not skip repaints in big files with EControl lexer
    UpdateCursor;
    Paint;
    if bAllowRepaint then
      Invalidate;
  end
  else
  begin
    if bAllowRepaint then
      Invalidate;
  end;
end;

function TATSynEdit.DoCommandCore(ACmd: integer; const AText: atString): TATCommandResults;
begin
  Result:= [];
  case ACmd of
    //most used commands

    //Shift+arrows must make column selection, if column select option on
    cCommand_KeyLeft:
      Result:= DoCommand_KeyLeft(false);
    cCommand_KeyLeft_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToDirection(cDirColumnLeft)
        else
          Result:= DoCommand_KeyLeft(true);
       end;

    cCommand_KeyRight:
      Result:= DoCommand_KeyRight(false);
    cCommand_KeyRight_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToDirection(cDirColumnRight)
        else
          Result:= DoCommand_KeyRight(true);
      end;

    cCommand_KeyUp:
      Result:= DoCommand_KeyUpDown(false, 1, false);
    cCommand_KeyUp_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToDirection(cDirColumnUp)
        else
          Result:= DoCommand_KeyUpDown(false, 1, false);
        end;

    cCommand_KeyDown:
      Result:= DoCommand_KeyUpDown(true, 1, false);
    cCommand_KeyDown_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToDirection(cDirColumnDown)
        else
          Result:= DoCommand_KeyUpDown(true, 1, false);
      end;

    cCommand_KeyHome:
      Result:= DoCommand_KeyHome;
    cCommand_KeyHome_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToLineEdge(false)
        else
          Result:= DoCommand_KeyHome;
      end;

    cCommand_KeyEnd:
      Result:= DoCommand_KeyEnd;
    cCommand_KeyEnd_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToLineEdge(true)
        else
          Result:= DoCommand_KeyEnd;
      end;

    cCommand_KeyPageUp:
      Result:= DoCommand_KeyUpDown(false, GetPageLines, FOptKeyPageKeepsRelativePos);
    cCommand_KeyPageUp_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToDirection(cDirColumnPageUp)
        else
          Result:= DoCommand_KeyUpDown(false, GetPageLines, FOptKeyPageKeepsRelativePos);
      end;

    cCommand_KeyPageDown:
      Result:= DoCommand_KeyUpDown(true, GetPageLines, FOptKeyPageKeepsRelativePos);
    cCommand_KeyPageDown_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Result:= DoCommand_SelectColumnToDirection(cDirColumnPageDown)
        else
          Result:= DoCommand_KeyUpDown(true, GetPageLines, FOptKeyPageKeepsRelativePos);
      end;

    cCommand_ColSelectLeft:        Result:= DoCommand_SelectColumnToDirection(cDirColumnLeft);
    cCommand_ColSelectRight:       Result:= DoCommand_SelectColumnToDirection(cDirColumnRight);
    cCommand_ColSelectUp:          Result:= DoCommand_SelectColumnToDirection(cDirColumnUp);
    cCommand_ColSelectDown:        Result:= DoCommand_SelectColumnToDirection(cDirColumnDown);
    cCommand_ColSelectPageUp:      Result:= DoCommand_SelectColumnToDirection(cDirColumnPageUp);
    cCommand_ColSelectPageDown:    Result:= DoCommand_SelectColumnToDirection(cDirColumnPageDown);
    cCommand_ColSelectToLineBegin: Result:= DoCommand_SelectColumnToLineEdge(false);
    cCommand_ColSelectToLineEnd:   Result:= DoCommand_SelectColumnToLineEdge(true);

    cCommand_ColSelectWithoutKey_On:
      Result:= DoCommand_ColumnSelectWithoutKey(true);
    cCommand_ColSelectWithoutKey_Off:
      Result:= DoCommand_ColumnSelectWithoutKey(false);
    cCommand_ColSelectWithoutKey_Toggle:
      begin
        OptMouseColumnSelectionWithoutKey:= not OptMouseColumnSelectionWithoutKey;
        Result:= [cResultState];
      end;

    cCommand_RemoveFirstCaret:    Result:= DoCommand_RemoveOneCaret(true);
    cCommand_RemoveLastCaret:     Result:= DoCommand_RemoveOneCaret(false);

    cCommand_TextInsert:          Result:= DoCommand_TextInsertAtCarets(AText, false, FOverwrite, false);
    cCommand_TextInsertTabChar:   Result:= DoCommand_TextInsertAtCarets(#9, false, FOverwrite, false);
    cCommand_KeyBackspace:        Result:= DoCommand_TextBackspace;
    cCommand_KeyDelete:           Result:= DoCommand_TextDelete;
    cCommand_KeyTab:              Result:= DoCommand_TextTabulation;
    cCommand_KeyEnter:            Result:= DoCommand_TextInsertEol(false);
    cCommand_ForceFinalEndOfLine: Result:= DoCommand_ForceFinalEndOfLine;

    cCommand_Undo:                Result:= DoCommand_UndoRedo(true);
    cCommand_Redo:                Result:= DoCommand_UndoRedo(false);
    //end of most used

    cCommand_TextDeleteSelection:      Result:= DoCommand_TextDeleteSelection;
    cCommand_TextDeleteLine:           Result:= DoCommand_TextDeleteLines;
    cCommand_TextDuplicateLine:        Result:= DoCommand_TextDuplicateLine;
    cCommand_TextDeleteToLineBegin:    Result:= DoCommand_TextDeleteToLineBegin;
    cCommand_TextDeleteToLineEnd:      Result:= DoCommand_TextDeleteToLineEnd;
    cCommand_TextDeleteToTextBegin:    Result:= DoCommand_TextDeleteToDocumentBegin;
    cCommand_TextDeleteToTextEnd:      Result:= DoCommand_TextDeleteToDocumentEnd;
    cCommand_TextDeleteWordPrev:       Result:= DoCommand_TextDeleteWord(false);
    cCommand_TextDeleteWordNext:       Result:= DoCommand_TextDeleteWord(true);
    cCommand_TextDeleteWordEntire:     Result:= DoCommand_TextDeleteWordEntire;

    cCommand_SelectAll:                Result:= DoCommand_SelectAll;
    cCommand_SelectNone:               Result:= [cResultCaretAny];
    cCommand_SelectInverted:           Result:= DoCommand_SelectInverted;
    cCommand_SelectSplitToLines:       Result:= DoCommand_SelectSplitToLines;
    cCommand_SelectExtendByLine:       Result:= DoCommand_SelectExtendByLine;
    cCommand_SelectWords:              Result:= DoCommand_SelectWords;
    cCommand_SelectLines:              Result:= DoCommand_SelectLines;

    cCommand_GotoTextBegin,
    cCommand_GotoTextBegin_Sel:        Result:= DoCommand_GotoTextBegin;
    cCommand_GotoTextEnd,
    cCommand_GotoTextEnd_Sel:          Result:= DoCommand_GotoTextEnd;

    cCommand_GotoScreenTop:            Result:= DoCommand_GotoScreenSide(cScreenSideTop);
    cCommand_GotoScreenBottom:         Result:= DoCommand_GotoScreenSide(cScreenSideBottom);
    cCommand_GotoScreenCenter:         Result:= DoCommand_GotoScreenSide(cScreenSideMiddle);

    cCommand_GotoWordNext,
    cCommand_GotoWordNext_Sel:         Result:= DoCommand_GotoWord(cWordjumpToNext);
    cCommand_GotoWordPrev,
    cCommand_GotoWordPrev_Sel:         Result:= DoCommand_GotoWord(cWordjumpToPrev);

    cCommand_GotoWordNext_Simple,
    cCommand_GotoWordNext_Simple_Sel:  Result:= DoCommand_GotoWord(cWordjumpToNext, true);
    cCommand_GotoWordPrev_Simple,
    cCommand_GotoWordPrev_Simple_Sel:  Result:= DoCommand_GotoWord(cWordjumpToPrev, true);

    cCommand_GotoWordEnd,
    cCommand_GotoWordEnd_Sel:          Result:= DoCommand_GotoWord(cWordjumpToEndOrNext);

    cCommand_GotoLineAbsBegin,
    cCommand_GotoLineAbsBegin_Sel:     Result:= DoCommand_GotoLineEdge(true);
    cCommand_GotoLineAbsEnd,
    cCommand_GotoLineAbsEnd_Sel:       Result:= DoCommand_GotoLineEdge(false);

    cCommand_ToggleOverwrite:          Result:= DoCommand_ToggleOverwrite;
    cCommand_ToggleReadOnly:           Result:= DoCommand_ToggleReadOnly;
    cCommand_ToggleWordWrap:           Result:= DoCommand_ToggleWordWrap(false);
    cCommand_ToggleWordWrapAlt:        Result:= DoCommand_ToggleWordWrap(true);
    cCommand_ToggleUnprinted:          Result:= DoCommand_ToggleUnprinted;
    cCommand_ToggleUnprintedSpaces:    Result:= DoCommand_ToggleUnprintedSpaces;
    cCommand_ToggleUnprintedSpacesTrailing: Result:= DoCommand_ToggleUnprintedSpacesTrailing;
    cCommand_ToggleUnprintedEnds:      Result:= DoCommand_ToggleUnprintedEnds;
    cCommand_ToggleUnprintedEndDetails: Result:= DoCommand_ToggleUnprintedEndDetails;
    cCommand_ToggleLineNums:           Result:= DoCommand_ToggleLineNums;
    cCommand_ToggleFolding:            Result:= DoCommand_ToggleFolding;
    cCommand_ToggleRuler:              Result:= DoCommand_ToggleRuler;
    cCommand_ToggleMinimap:            Result:= DoCommand_ToggleMiniMap;
    cCommand_ToggleMicromap:           Result:= DoCommand_ToggleMicroMap;

    cCommand_TextIndent:               Result:= DoCommand_TextIndentUnindent(true);
    cCommand_TextUnindent:             Result:= DoCommand_TextIndentUnindent(false);

    cCommand_ScrollToBegin:            Result:= DoCommand_ScrollToBeginOrEnd(true);
    cCommand_ScrollToEnd:              Result:= DoCommand_ScrollToBeginOrEnd(false);
    cCommand_ScrollLineUp:             Result:= DoCommand_ScrollByDelta(-1, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollLineDown:           Result:= DoCommand_ScrollByDelta(1, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollPageUp:             Result:= DoCommand_ScrollByDelta(-GetPageLines, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollPageDown:           Result:= DoCommand_ScrollByDelta(GetPageLines, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollColumnLeft:         Result:= DoCommand_ScrollByDelta(0, -1, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollColumnRight:        Result:= DoCommand_ScrollByDelta(0, 1, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollToCaretTop:         Result:= [cResultCaretTop];
    cCommand_ScrollToCaretBottom:      Result:= [cResultCaretBottom];
    cCommand_ScrollToCaretLeft:        Result:= [cResultCaretLeft];
    cCommand_ScrollToCaretRight:       Result:= [cResultCaretRight];

    cCommand_Sort_Asc:                 Result:= DoCommand_Sort(cSortActionAsc);
    cCommand_Sort_AscNoCase:           Result:= DoCommand_Sort(cSortActionAscNoCase);
    cCommand_Sort_Desc:                Result:= DoCommand_Sort(cSortActionDesc);
    cCommand_Sort_DescNoCase:          Result:= DoCommand_Sort(cSortActionDescNoCase);

    cCommand_DeleteAllBlanks:          Result:= DoCommand_DeleteAllBlanks;
    cCommand_DeleteAdjacentBlanks:     Result:= DoCommand_DeleteAdjacentBlanks;
    cCommand_DeleteAdjacentDups:       Result:= DoCommand_DeleteAdjacentDups;
    cCommand_DeleteAllDups:            Result:= DoCommand_DeleteAllDups(false);
    cCommand_DeleteAllDupsKeepBlanks:  Result:= DoCommand_DeleteAllDups(true);
    cCommand_ReverseLines:             Result:= DoCommand_ReverseLines;
    cCommand_ShuffleLines:             Result:= DoCommand_ShuffleLines;

    cCommand_ClipboardCopy:            Result:= DoCommand_ClipboardCopy(false, Clipboard);
    cCommand_ClipboardCopyAdd:         Result:= DoCommand_ClipboardCopy(true, Clipboard);
    cCommand_ClipboardCut:             Result:= DoCommand_ClipboardCut(Clipboard);

    //use Clipboard:TClipboard
    cCommand_ClipboardPaste:                 Result:= DoCommand_ClipboardPaste(false, false, Clipboard);
    cCommand_ClipboardPaste_Select:          Result:= DoCommand_ClipboardPaste(false, true, Clipboard);
    cCommand_ClipboardPaste_KeepCaret:       Result:= DoCommand_ClipboardPaste(true, false, Clipboard);
    cCommand_ClipboardPaste_Column:          Result:= DoCommand_ClipboardPasteColumnBlock(false, Clipboard);
    cCommand_ClipboardPaste_ColumnKeepCaret: Result:= DoCommand_ClipboardPasteColumnBlock(true, Clipboard);

    //use PrimarySelection:TClipboard
    cCommand_ClipboardAltPaste:                 Result:= DoCommand_ClipboardPaste(false, false, PrimarySelection);
    cCommand_ClipboardAltPaste_Select:          Result:= DoCommand_ClipboardPaste(false, true, PrimarySelection);
    cCommand_ClipboardAltPaste_KeepCaret:       Result:= DoCommand_ClipboardPaste(true, false, PrimarySelection);
    cCommand_ClipboardAltPaste_Column:          Result:= DoCommand_ClipboardPasteColumnBlock(false, PrimarySelection);
    cCommand_ClipboardAltPaste_ColumnKeepCaret: Result:= DoCommand_ClipboardPasteColumnBlock(true, PrimarySelection);
    //use SecondarySelection:TClipboard
    cCommand_ClipboardAltAltPaste:              Result:= DoCommand_ClipboardPaste(false, false, SecondarySelection);

    cCommand_MoveSelectionUp:          Result:= DoCommand_MoveSelectionUpDown(false);
    cCommand_MoveSelectiondown:        Result:= DoCommand_MoveSelectionUpDown(true);
    cCommand_TextInsertEmptyAbove:     Result:= DoCommand_TextInsertEmptyAboveBelow(false);
    cCommand_TextInsertEmptyBelow:     Result:= DoCommand_TextInsertEmptyAboveBelow(true);

    cCommand_TextCaseLower:            Result:= DoCommand_TextChangeCase(cCaseLower);
    cCommand_TextCaseUpper:            Result:= DoCommand_TextChangeCase(cCaseUpper);
    cCommand_TextCaseTitle:            Result:= DoCommand_TextChangeCase(cCaseTitle);
    cCommand_TextCaseInvert:           Result:= DoCommand_TextChangeCase(cCaseInvert);
    cCommand_TextCaseSentence:         Result:= DoCommand_TextChangeCase(cCaseSentence);

    cCommand_TextTrimSpacesLeft:       Result:= DoCommand_TextTrimSpaces(cTrimLeft);
    cCommand_TextTrimSpacesRight:      Result:= DoCommand_TextTrimSpaces(cTrimRight);
    cCommand_TextTrimSpacesAll:        Result:= DoCommand_TextTrimSpaces(cTrimAll);

    cCommand_FoldAll:                  Result:= DoCommand_FoldAll;
    cCommand_UnfoldAll:                Result:= DoCommand_FoldUnAll;
    cCommand_FoldLevel1..
    cCommand_FoldLevel9:               Result:= DoCommand_FoldLevel(ACmd-cCommand_FoldLevel1);

    cCommand_FoldingFoldAtCurLine:     Result:= DoCommand_FoldRangeAtCurLine(cFoldingFold);
    cCommand_FoldingUnfoldAtCurLine:   Result:= DoCommand_FoldRangeAtCurLine(cFoldingUnfold);
    cCommand_FoldingToggleAtCurLine:   Result:= DoCommand_FoldRangeAtCurLine(cFoldingToggle);
    cCommand_FoldingFoldSelection:     Result:= DoCommand_FoldSelection;

    cCommand_Cancel:                   Result:= DoCommand_Cancel;
    cCommand_CaretsExtendUpLine:       Result:= DoCommand_CaretsExtend(false, 1);
    cCommand_CaretsExtendUpPage:       Result:= DoCommand_CaretsExtend(false, GetPageLines);
    cCommand_CaretsExtendUpToTop:      Result:= DoCommand_CaretsExtend(false, Strings.Count);
    cCommand_CaretsExtendDownLine:     Result:= DoCommand_CaretsExtend(true, 1);
    cCommand_CaretsExtendDownPage:     Result:= DoCommand_CaretsExtend(true, GetPageLines);
    cCommand_CaretsExtendDownToEnd:    Result:= DoCommand_CaretsExtend(true, Strings.Count);

    cCommand_ZoomIn:                   Result:= DoCommand_ScaleDelta(true);
    cCommand_ZoomOut:                  Result:= DoCommand_ScaleDelta(false);
    cCommand_ZoomReset:                Result:= DoCommand_ScaleReset;
    cCommand_RepeatTextCommand:        DoCommand(FLastTextCmd, FLastTextCmdText);
  end;
end;

procedure TATSynEdit.DoCommand(ACmd: integer; const AText: atString = '');
var
  Caret: TATCaretItem;
  Res: TATCommandResults;
  Str: string;
  bTooManyCarets: boolean;
  NCommandCode: integer;
  i: integer;
begin
  if ACmd<=0 then Exit;
  LastCommandChangedLines:= 0;
  FLastCommandMakesColumnSel:= false;
  DoHotspotsExit;

  if AText<>'' then
    Str:= UTF8Encode(AText)
  else
    Str:= '';

  if DoEventCommand(ACmd, Str) then Exit;

  if IsCommandToUndoInOneStep(ACmd) then
    NCommandCode:= ACmd
  else
    NCommandCode:= 0;
  Strings.CommandCode:= NCommandCode;

  FCaretSpecPos:= false;

  //moving must be with visible caret
  DoCaretForceShow;

  //---handle special command bits (cCmdNNNNN)
  if ACmd and cCmdFlag_KeepSel <> 0 then
    for i:= 0 to Carets.Count-1 do
    begin
      Caret:= Carets[i];
      if Caret.EndY<0 then Caret.EndY:= Caret.PosY;
      if Caret.EndX<0 then Caret.EndX:= Caret.PosX;
    end;

  if ACmd and cCmdFlag_ResetSel <> 0 then
    DoSelect_None;

  if ACmd and cCmdFlag_Caret <> 0 then
  begin
    Strings.SetGroupMark;
  end;

  //support CudaText #3084
  Strings.SetNewCommandMark;

  Carets.UpdateMemory(cCaretMem_PrepareX, false);
  FIsRunningCommand:= true;

  bTooManyCarets:= Carets.Count>FOptUndoMaxCarets;
  if bTooManyCarets then
    SetEnabledSlowEvents(false);

  BeginEditing;

  try
    Res:= DoCommandCore(ACmd, AText);
  finally
    EndEditing(cResultText in Res);

    if bTooManyCarets then
      SetEnabledSlowEvents(true);
  end;

  if IsCommandResults_CaretMove(Res) then
    if not FLastCommandMakesColumnSel then
    begin
      ClearSelRectPoints;
    end;

  FLastCommandChangedText2:= FLastCommandChangedText;
  FLastCommandChangedText:= cResultText in Res;
  if FLastCommandChangedText then
  begin
    if ACmd<>cCommand_RepeatTextCommand then
    begin
      FLastTextCmd:= ACmd;
      FLastTextCmdText:= AText;
    end;

    //to solve CudaText issue #3403
    if IsCommandForDelayedParsing(ACmd) then
      StartTimerDelayedParsing;
  end;

  //this logic: to make like in SynWrite, option "Fixed column move".
  //arrows up/down must keep their column, even if sometimes they jump to shorter lines (caret_after_end: off).
  //and special handling for tab-key (for first press of tab-key).
  case ACmd of
    cCommand_KeyTab:
      begin
        if not FLastCommandChangedText2 then
          Carets.UpdateMemory(cCaretMem_SaveX, false);
      end;
    {
    // CudaText issue #1326
    cCommand_ClipboardPaste,
    cCommand_ClipboardPaste_Select,
    cCommand_ClipboardAltPaste,
    cCommand_ClipboardAltAltPaste,
    cCommand_ClipboardAltPaste_Select:
      begin
        Carets.UpdateMemory(cCaretMem_SaveX, false);
      end;
    }
    cCommand_KeyUp,
    cCommand_KeyUp_Sel,
    cCommand_KeyDown,
    cCommand_KeyDown_Sel:
      Carets.UpdateMemory(cCaretMem_SaveX, true);
    else
      Carets.UpdateMemory(cCaretMem_ClearX, false);
  end;

  if NCommandCode<>0 then
    Strings.CommandCode:= 0;
  DoCommandResults(ACmd, Res);
  DoEventCommandAfter(ACmd, AText);
  FIsRunningCommand:= false;
end;


