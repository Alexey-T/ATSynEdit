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
    DoPaintModeStatic;
    DoCommand(NCmd);
    DoPaintModeBlinking;
  end;
end;

procedure TATSynEdit.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  Str: atString;
  Allow: boolean;
begin
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

  DoPaintModeStatic;
  DoCommand(cCommand_TextInsert, Str);
  DoPaintModeBlinking;

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

procedure TATSynEdit.DoCommand(ACmd: integer; const AText: atString = '');
var
  Caret: TATCaretItem;
  Res: TATCommandResults;
  Str: string;
  bAllowUndo: boolean;
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

  Res:= [];
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

  Carets.UpdateSavedX(cUpdateX_Prepare, false);
  FIsRunningCommand:= true;

  bAllowUndo:= Carets.Count<=FOptUndoMaxCarets;
  if not bAllowUndo then
    SetEnabledSlowEvents(false);

  //--------
  case ACmd of
    //most used commands

    //Shift+arrows must make column selection, if column select option on
    cCommand_KeyLeft:
      Res:= DoCommand_KeyLeft(false);
    cCommand_KeyLeft_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToDirection(cDirColumnLeft)
        else
          Res:= DoCommand_KeyLeft(true);
       end;

    cCommand_KeyRight:
      Res:= DoCommand_KeyRight(false);
    cCommand_KeyRight_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToDirection(cDirColumnRight)
        else
          Res:= DoCommand_KeyRight(true);
      end;

    cCommand_KeyUp:
      Res:= DoCommand_KeyUpDown(false, 1, false);
    cCommand_KeyUp_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToDirection(cDirColumnUp)
        else
          Res:= DoCommand_KeyUpDown(false, 1, false);
        end;

    cCommand_KeyDown:
      Res:= DoCommand_KeyUpDown(true, 1, false);
    cCommand_KeyDown_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToDirection(cDirColumnDown)
        else
          Res:= DoCommand_KeyUpDown(true, 1, false);
      end;

    cCommand_KeyHome:
      Res:= DoCommand_KeyHome;
    cCommand_KeyHome_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToLineEdge(false)
        else
          Res:= DoCommand_KeyHome;
      end;

    cCommand_KeyEnd:
      Res:= DoCommand_KeyEnd;
    cCommand_KeyEnd_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToLineEdge(true)
        else
          Res:= DoCommand_KeyEnd;
      end;

    cCommand_KeyPageUp:
      Res:= DoCommand_KeyUpDown(false, GetPageLines, FOptKeyPageKeepsRelativePos);
    cCommand_KeyPageUp_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToDirection(cDirColumnPageUp)
        else
          Res:= DoCommand_KeyUpDown(false, GetPageLines, FOptKeyPageKeepsRelativePos);
      end;

    cCommand_KeyPageDown:
      Res:= DoCommand_KeyUpDown(true, GetPageLines, FOptKeyPageKeepsRelativePos);
    cCommand_KeyPageDown_Sel:
      begin
        if OptMouseColumnSelectionWithoutKey then
          Res:= DoCommand_SelectColumnToDirection(cDirColumnPageDown)
        else
          Res:= DoCommand_KeyUpDown(true, GetPageLines, FOptKeyPageKeepsRelativePos);
      end;

    cCommand_ColSelectLeft:        Res:= DoCommand_SelectColumnToDirection(cDirColumnLeft);
    cCommand_ColSelectRight:       Res:= DoCommand_SelectColumnToDirection(cDirColumnRight);
    cCommand_ColSelectUp:          Res:= DoCommand_SelectColumnToDirection(cDirColumnUp);
    cCommand_ColSelectDown:        Res:= DoCommand_SelectColumnToDirection(cDirColumnDown);
    cCommand_ColSelectPageUp:      Res:= DoCommand_SelectColumnToDirection(cDirColumnPageUp);
    cCommand_ColSelectPageDown:    Res:= DoCommand_SelectColumnToDirection(cDirColumnPageDown);
    cCommand_ColSelectToLineBegin: Res:= DoCommand_SelectColumnToLineEdge(false);
    cCommand_ColSelectToLineEnd:   Res:= DoCommand_SelectColumnToLineEdge(true);

    cCommand_ColSelectWithoutKey_On:
      Res:= DoCommand_ColumnSelectWithoutKey(true);
    cCommand_ColSelectWithoutKey_Off:
      Res:= DoCommand_ColumnSelectWithoutKey(false);
    cCommand_ColSelectWithoutKey_Toggle:
      begin
        OptMouseColumnSelectionWithoutKey:= not OptMouseColumnSelectionWithoutKey;
        Res:= [cResultState];
      end;

    cCommand_RemoveFirstCaret:    Res:= DoCommand_RemoveOneCaret(true);
    cCommand_RemoveLastCaret:     Res:= DoCommand_RemoveOneCaret(false);

    cCommand_TextInsert:          Res:= DoCommand_TextInsertAtCarets(AText, false, FOverwrite, false);
    cCommand_TextInsertTabChar:   Res:= DoCommand_TextInsertAtCarets(#9, false, FOverwrite, false);
    cCommand_KeyBackspace:        Res:= DoCommand_TextBackspace;
    cCommand_KeyDelete:           Res:= DoCommand_TextDelete;
    cCommand_KeyTab:              Res:= DoCommand_TextTabulation;
    cCommand_KeyEnter:            Res:= DoCommand_TextInsertEol(false);
    cCommand_ForceFinalEndOfLine: Res:= DoCommand_ForceFinalEndOfLine;

    cCommand_Undo:                Res:= DoCommand_Undo;
    cCommand_Redo:                Res:= DoCommand_Redo;
    //end of most used

    cCommand_TextDeleteSelection:      Res:= DoCommand_TextDeleteSelection;
    cCommand_TextDeleteLine:           Res:= DoCommand_TextDeleteLines;
    cCommand_TextDuplicateLine:        Res:= DoCommand_TextDuplicateLine;
    cCommand_TextDeleteToLineBegin:    Res:= DoCommand_TextDeleteToLineBegin;
    cCommand_TextDeleteToLineEnd:      Res:= DoCommand_TextDeleteToLineEnd;
    cCommand_TextDeleteToTextBegin:    Res:= DoCommand_TextDeleteToDocumentBegin;
    cCommand_TextDeleteToTextEnd:      Res:= DoCommand_TextDeleteToDocumentEnd;
    cCommand_TextDeleteWordPrev:       Res:= DoCommand_TextDeleteWord(false);
    cCommand_TextDeleteWordNext:       Res:= DoCommand_TextDeleteWord(true);
    cCommand_TextDeleteWordEntire:     Res:= DoCommand_TextDeleteWordEntire;

    cCommand_SelectAll:                Res:= DoCommand_SelectAll;
    cCommand_SelectNone:               Res:= [cResultCaretAny];
    cCommand_SelectInverted:           Res:= DoCommand_SelectInverted;
    cCommand_SelectSplitToLines:       Res:= DoCommand_SelectSplitToLines;
    cCommand_SelectExtendByLine:       Res:= DoCommand_SelectExtendByLine;
    cCommand_SelectWords:              Res:= DoCommand_SelectWords;
    cCommand_SelectLines:              Res:= DoCommand_SelectLines;

    cCommand_GotoTextBegin,
    cCommand_GotoTextBegin_Sel:        Res:= DoCommand_GotoTextBegin;
    cCommand_GotoTextEnd,
    cCommand_GotoTextEnd_Sel:          Res:= DoCommand_GotoTextEnd;

    cCommand_GotoScreenTop:            Res:= DoCommand_GotoScreenSide(cScreenSideTop);
    cCommand_GotoScreenBottom:         Res:= DoCommand_GotoScreenSide(cScreenSideBottom);
    cCommand_GotoScreenCenter:         Res:= DoCommand_GotoScreenSide(cScreenSideMiddle);

    cCommand_GotoWordNext,
    cCommand_GotoWordNext_Sel:         Res:= DoCommand_GotoWord(cWordjumpToNext);
    cCommand_GotoWordPrev,
    cCommand_GotoWordPrev_Sel:         Res:= DoCommand_GotoWord(cWordjumpToPrev);

    cCommand_GotoWordNext_Simple,
    cCommand_GotoWordNext_Simple_Sel:  Res:= DoCommand_GotoWord(cWordjumpToNext, true);
    cCommand_GotoWordPrev_Simple,
    cCommand_GotoWordPrev_Simple_Sel:  Res:= DoCommand_GotoWord(cWordjumpToPrev, true);

    cCommand_GotoWordEnd,
    cCommand_GotoWordEnd_Sel:          Res:= DoCommand_GotoWord(cWordjumpToEndOrNext);

    cCommand_GotoLineAbsBegin,
    cCommand_GotoLineAbsBegin_Sel:     Res:= DoCommand_GotoLineEdge(true);
    cCommand_GotoLineAbsEnd,
    cCommand_GotoLineAbsEnd_Sel:       Res:= DoCommand_GotoLineEdge(false);

    cCommand_ToggleOverwrite:          Res:= DoCommand_ToggleOverwrite;
    cCommand_ToggleReadOnly:           Res:= DoCommand_ToggleReadOnly;
    cCommand_ToggleWordWrap:           Res:= DoCommand_ToggleWordWrap(false);
    cCommand_ToggleWordWrapAlt:        Res:= DoCommand_ToggleWordWrap(true);
    cCommand_ToggleUnprinted:          Res:= DoCommand_ToggleUnprinted;
    cCommand_ToggleUnprintedSpaces:    Res:= DoCommand_ToggleUnprintedSpaces;
    cCommand_ToggleUnprintedSpacesTrailing: Res:= DoCommand_ToggleUnprintedSpacesTrailing;
    cCommand_ToggleUnprintedEnds:      Res:= DoCommand_ToggleUnprintedEnds;
    cCommand_ToggleUnprintedEndDetails: Res:= DoCommand_ToggleUnprintedEndDetails;
    cCommand_ToggleLineNums:           Res:= DoCommand_ToggleLineNums;
    cCommand_ToggleFolding:            Res:= DoCommand_ToggleFolding;
    cCommand_ToggleRuler:              Res:= DoCommand_ToggleRuler;
    cCommand_ToggleMinimap:            Res:= DoCommand_ToggleMiniMap;
    cCommand_ToggleMicromap:           Res:= DoCommand_ToggleMicroMap;

    cCommand_TextIndent:               Res:= DoCommand_TextIndentUnindent(true);
    cCommand_TextUnindent:             Res:= DoCommand_TextIndentUnindent(false);

    cCommand_ScrollToBegin:            Res:= DoCommand_ScrollToBeginOrEnd(true);
    cCommand_ScrollToEnd:              Res:= DoCommand_ScrollToBeginOrEnd(false);
    cCommand_ScrollLineUp:             Res:= DoCommand_ScrollByDelta(-1, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollLineDown:           Res:= DoCommand_ScrollByDelta(1, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollPageUp:             Res:= DoCommand_ScrollByDelta(-GetPageLines, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollPageDown:           Res:= DoCommand_ScrollByDelta(GetPageLines, 0, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollColumnLeft:         Res:= DoCommand_ScrollByDelta(0, -1, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollColumnRight:        Res:= DoCommand_ScrollByDelta(0, 1, FOptScrollLineCommandsKeepCaretOnScreen);
    cCommand_ScrollToCaretTop:         Res:= [cResultCaretTop];
    cCommand_ScrollToCaretBottom:      Res:= [cResultCaretBottom];
    cCommand_ScrollToCaretLeft:        Res:= [cResultCaretLeft];
    cCommand_ScrollToCaretRight:       Res:= [cResultCaretRight];

    cCommand_Sort_Asc:                 Res:= DoCommand_Sort(cSortActionAsc);
    cCommand_Sort_AscNoCase:           Res:= DoCommand_Sort(cSortActionAscNoCase);
    cCommand_Sort_Desc:                Res:= DoCommand_Sort(cSortActionDesc);
    cCommand_Sort_DescNoCase:          Res:= DoCommand_Sort(cSortActionDescNoCase);

    cCommand_DeleteAllBlanks:          Res:= DoCommand_DeleteAllBlanks;
    cCommand_DeleteAdjacentBlanks:     Res:= DoCommand_DeleteAdjacentBlanks;
    cCommand_DeleteAdjacentDups:       Res:= DoCommand_DeleteAdjacentDups;
    cCommand_DeleteAllDups:            Res:= DoCommand_DeleteAllDups(false);
    cCommand_DeleteAllDupsKeepBlanks:  Res:= DoCommand_DeleteAllDups(true);
    cCommand_ReverseLines:             Res:= DoCommand_ReverseLines;
    cCommand_ShuffleLines:             Res:= DoCommand_ShuffleLines;

    cCommand_ClipboardCopy:            Res:= DoCommand_ClipboardCopy(false, Clipboard);
    cCommand_ClipboardCopyAdd:         Res:= DoCommand_ClipboardCopy(true, Clipboard);
    cCommand_ClipboardCut:             Res:= DoCommand_ClipboardCut(Clipboard);

    //use Clipboard:TClipboard
    cCommand_ClipboardPaste:                 Res:= DoCommand_ClipboardPaste(false, false, Clipboard);
    cCommand_ClipboardPaste_Select:          Res:= DoCommand_ClipboardPaste(false, true, Clipboard);
    cCommand_ClipboardPaste_KeepCaret:       Res:= DoCommand_ClipboardPaste(true, false, Clipboard);
    cCommand_ClipboardPaste_Column:          Res:= DoCommand_ClipboardPasteColumnBlock(false, Clipboard);
    cCommand_ClipboardPaste_ColumnKeepCaret: Res:= DoCommand_ClipboardPasteColumnBlock(true, Clipboard);

    //use PrimarySelection:TClipboard
    cCommand_ClipboardAltPaste:                 Res:= DoCommand_ClipboardPaste(false, false, PrimarySelection);
    cCommand_ClipboardAltPaste_Select:          Res:= DoCommand_ClipboardPaste(false, true, PrimarySelection);
    cCommand_ClipboardAltPaste_KeepCaret:       Res:= DoCommand_ClipboardPaste(true, false, PrimarySelection);
    cCommand_ClipboardAltPaste_Column:          Res:= DoCommand_ClipboardPasteColumnBlock(false, PrimarySelection);
    cCommand_ClipboardAltPaste_ColumnKeepCaret: Res:= DoCommand_ClipboardPasteColumnBlock(true, PrimarySelection);
    //use SecondarySelection:TClipboard
    cCommand_ClipboardAltAltPaste:              Res:= DoCommand_ClipboardPaste(false, false, SecondarySelection);

    cCommand_MoveSelectionUp:          Res:= DoCommand_MoveSelectionUpDown(false);
    cCommand_MoveSelectiondown:        Res:= DoCommand_MoveSelectionUpDown(true);
    cCommand_TextInsertEmptyAbove:     Res:= DoCommand_TextInsertEmptyAboveBelow(false);
    cCommand_TextInsertEmptyBelow:     Res:= DoCommand_TextInsertEmptyAboveBelow(true);

    cCommand_TextCaseLower:            Res:= DoCommand_TextChangeCase(cCaseLower);
    cCommand_TextCaseUpper:            Res:= DoCommand_TextChangeCase(cCaseUpper);
    cCommand_TextCaseTitle:            Res:= DoCommand_TextChangeCase(cCaseTitle);
    cCommand_TextCaseInvert:           Res:= DoCommand_TextChangeCase(cCaseInvert);
    cCommand_TextCaseSentence:         Res:= DoCommand_TextChangeCase(cCaseSentence);

    cCommand_TextTrimSpacesLeft:       Res:= DoCommand_TextTrimSpaces(cTrimLeft);
    cCommand_TextTrimSpacesRight:      Res:= DoCommand_TextTrimSpaces(cTrimRight);
    cCommand_TextTrimSpacesAll:        Res:= DoCommand_TextTrimSpaces(cTrimAll);

    cCommand_FoldAll:                  Res:= DoCommand_FoldAll;
    cCommand_UnfoldAll:                Res:= DoCommand_FoldUnAll;
    cCommand_FoldLevel1..
    cCommand_FoldLevel9:               Res:= DoCommand_FoldLevel(ACmd-cCommand_FoldLevel1);

    cCommand_FoldingFoldAtCurLine:     Res:= DoCommand_FoldRangeAtCurLine(cFoldingFold);
    cCommand_FoldingUnfoldAtCurLine:   Res:= DoCommand_FoldRangeAtCurLine(cFoldingUnfold);
    cCommand_FoldingToggleAtCurLine:   Res:= DoCommand_FoldRangeAtCurLine(cFoldingToggle);
    cCommand_FoldingFoldSelection:     Res:= DoCommand_FoldSelection;

    cCommand_Cancel:                   Res:= DoCommand_Cancel;
    cCommand_CaretsExtendUpLine:       Res:= DoCommand_CaretsExtend(false, 1);
    cCommand_CaretsExtendUpPage:       Res:= DoCommand_CaretsExtend(false, GetPageLines);
    cCommand_CaretsExtendUpToTop:      Res:= DoCommand_CaretsExtend(false, Strings.Count);
    cCommand_CaretsExtendDownLine:     Res:= DoCommand_CaretsExtend(true, 1);
    cCommand_CaretsExtendDownPage:     Res:= DoCommand_CaretsExtend(true, GetPageLines);
    cCommand_CaretsExtendDownToEnd:    Res:= DoCommand_CaretsExtend(true, Strings.Count);

    cCommand_ZoomIn:                   Res:= DoCommand_ScaleDelta(true);
    cCommand_ZoomOut:                  Res:= DoCommand_ScaleDelta(false);
    cCommand_ZoomReset:                Res:= DoCommand_ScaleReset;
    cCommand_RepeatTextCommand:        DoCommand(FLastTextCmd, FLastTextCmdText);
  end;

  if not bAllowUndo then
    SetEnabledSlowEvents(true);

  if IsCommandResults_CaretMove(Res) then
    if not FLastCommandMakesColumnSel then
    begin
      ClearSelRectPoints;
    end;

  FLastCommandChangedText2:= FLastCommandChangedText;
  FLastCommandChangedText:= cResultText in Res;
  if FLastCommandChangedText then
    if ACmd<>cCommand_RepeatTextCommand then
    begin
      FLastTextCmd:= ACmd;
      FLastTextCmdText:= AText;
    end;

  //this logic: to make like in SynWrite, option "Fixed column move".
  //arrows up/down must keep their column, even if sometimes they jump to shorter lines (caret_after_end: off).
  //and special handling for tab-key (for first press of tab-key).
  case ACmd of
    cCommand_KeyTab:
      begin
        if not FLastCommandChangedText2 then
          Carets.UpdateSavedX(cUpdateX_Save, false);
      end;
    {
    // https://github.com/Alexey-T/CudaText/issues/1326
    cCommand_ClipboardPaste,
    cCommand_ClipboardPaste_Select,
    cCommand_ClipboardAltPaste,
    cCommand_ClipboardAltAltPaste,
    cCommand_ClipboardAltPaste_Select:
      begin
        Carets.UpdateSavedX(cUpdateX_Save, false);
      end;
    }
    cCommand_KeyUp,
    cCommand_KeyUp_Sel,
    cCommand_KeyDown,
    cCommand_KeyDown_Sel:
      Carets.UpdateSavedX(cUpdateX_Save, true);
    else
      Carets.UpdateSavedX(cUpdateX_Clear, false);
  end;

  DoCommandResults(ACmd, Res);
  DoEventCommandAfter(ACmd, AText);
  FIsRunningCommand:= false;
end;


