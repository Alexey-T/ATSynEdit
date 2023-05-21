{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Keymap_Init;

interface

uses
  Classes,
  ATSynEdit_Keymap,
  ATSynEdit_Commands;

procedure InitKeymapFull(M: TATKeymap);
procedure InitKeymapCombo(M: TATKeymap);

var
  KeymapFull: TATKeymap = nil;
  KeymapCombo: TATKeymap = nil;

implementation

uses
  SysUtils,
  LCLType, LCLProc,
  Dialogs;

procedure InitKeymap_Arrows(M: TATKeyMap);
begin
  M.Add(cCommand_KeyLeft,         'caret char left',               [VK_LEFT], []);
  M.Add(cCommand_KeyLeft_Sel,     'caret char left + select',      [scShift+VK_LEFT], []);
  M.Add(cCommand_KeyRight,        'caret char right',              [VK_RIGHT], []);
  M.Add(cCommand_KeyRight_Sel,    'caret char right + select',     [scShift+VK_RIGHT], []);
  M.Add(cCommand_KeyUp,           'caret line up',                 [VK_UP], []);
  M.Add(cCommand_KeyUp_Sel,       'caret line up + select',        [scShift+VK_UP], []);
  M.Add(cCommand_KeyDown,         'caret line down',               [VK_DOWN], []);
  M.Add(cCommand_KeyDown_Sel,     'caret line down + select',      [scShift+VK_DOWN], []);
end;

procedure InitKeymap_HomeEnd(M: TATKeyMap);
begin
  M.Add(cCommand_KeyHome,         'caret to line begin',           {$ifndef darwin} [VK_HOME],         {$else} [scMeta+VK_LEFT],          {$endif} []);
  M.Add(cCommand_KeyHome_Sel,     'caret to line begin + select',  {$ifndef darwin} [scShift+VK_HOME], {$else} [scMeta+scShift+VK_LEFT],  {$endif} []);
  M.Add(cCommand_KeyEnd,          'caret to line end',             {$ifndef darwin} [VK_END],          {$else} [scMeta+VK_RIGHT],         {$endif} []);
  M.Add(cCommand_KeyEnd_Sel,      'caret to line end + select',    {$ifndef darwin} [scShift+VK_END],  {$else} [scMeta+scShift+VK_RIGHT], {$endif} []);
end;

procedure InitKeymap_PageUpDown(M: TATKeyMap);
begin
  M.Add(cCommand_KeyPageUp,       'caret page up',                 [VK_PRIOR], []);
  M.Add(cCommand_KeyPageUp_Sel,   'caret page up + select',        [scShift+VK_PRIOR], []);
  M.Add(cCommand_KeyPageDown,     'caret page down',               [VK_NEXT], []);
  M.Add(cCommand_KeyPageDown_Sel, 'caret page down + select',      [scShift+VK_NEXT], []);
end;

procedure InitKeymapFull(M: TATKeymap);
begin
  M.Clear;

  InitKeymap_Arrows(M);
  InitKeymap_HomeEnd(M);
  InitKeymap_PageUpDown(M);

  M.Add(cCommand_ColSelectLeft,        'column select: left',          {$ifndef darwin} [scShift+scAlt+VK_LEFT],  {$else} [], {$endif} []);
  M.Add(cCommand_ColSelectRight,       'column select: right',         {$ifndef darwin} [scShift+scAlt+VK_RIGHT], {$else} [], {$endif} []);
  M.Add(cCommand_ColSelectUp,          'column select: up',            {$ifndef darwin} [scShift+scAlt+VK_UP],    {$else} [], {$endif} []);
  M.Add(cCommand_ColSelectDown,        'column select: down',          {$ifndef darwin} [scShift+scAlt+VK_DOWN],  {$else} [], {$endif} []);
  M.Add(cCommand_ColSelectPageUp,      'column select: page up',       {$ifndef darwin} [scShift+scAlt+VK_PRIOR], {$else} [], {$endif} []);
  M.Add(cCommand_ColSelectPageDown,    'column select: page down',     {$ifndef darwin} [scShift+scAlt+VK_NEXT],  {$else} [], {$endif} []);
  M.Add(cCommand_ColSelectToLineBegin, 'column select: to line begin', {$ifndef darwin} [scShift+scAlt+VK_HOME],  {$else} [], {$endif} []);
  M.Add(cCommand_ColSelectToLineEnd,   'column select: to line end',   {$ifndef darwin} [scShift+scAlt+VK_END],   {$else} [], {$endif} []);

  M.Add(cCommand_KeyBackspace, 'delete char left (backspace)', [VK_BACK], [scShift+VK_BACK]);
  M.Add(cCommand_KeyDelete, 'delete char right (delete)', [VK_DELETE], []);
  M.Add(cCommand_KeyEnter, 'insert line-break (enter)', [VK_RETURN], [scShift+VK_RETURN]);
  M.Add(cCommand_KeyTab, 'tabulation key', [], []);
  M.Add(cCommand_TextInsertTabChar, 'insert tab char', [], []);

  M.Add(cCommand_ForceFinalEndOfLine, 'force final end-of-line', [], []);
  M.Add(cCommand_DeleteFinalEndOfLine, 'delete final end-of-line', [], []);

  M.Add(cCommand_TextDeleteLine, 'delete line(s)', [scXControl+VK_Y], []);
  M.Add(cCommand_TextDuplicateLine, 'duplicate line(s)', [scXControl+VK_D], []);

  M.Add(cCommand_GotoTextBegin,      'go to text begin',          {$ifndef darwin} [scXControl+VK_HOME],         [] {$else} [VK_HOME], [scMeta+VK_UP]  {$endif});
  M.Add(cCommand_GotoTextBegin_Sel,  'go to text begin + select', {$ifndef darwin} [scXControl+scShift+VK_HOME], [] {$else} [scShift+VK_HOME], []      {$endif});
  M.Add(cCommand_GotoTextEnd,        'go to text end',            {$ifndef darwin} [scXControl+VK_END],          [] {$else} [VK_END], [scMeta+VK_DOWN] {$endif});
  M.Add(cCommand_GotoTextEnd_Sel,    'go to text end + select',   {$ifndef darwin} [scXControl+scShift+VK_END],  [] {$else} [scShift+VK_END], []       {$endif});

  M.Add(cCommand_GotoWordPrev,       'go to word previous',          {$ifndef darwin} [scXControl+VK_LEFT],         {$else} [scAlt+VK_LEFT],          {$endif} []);
  M.Add(cCommand_GotoWordPrev_Sel,   'go to word previous + select', {$ifndef darwin} [scXControl+scShift+VK_LEFT], {$else} [scAlt+scShift+VK_LEFT],  {$endif} []);
  M.Add(cCommand_GotoWordNext,       'go to word next',              {$ifndef darwin} [],                           {$else} [scAlt+VK_RIGHT],         {$endif} []);
  M.Add(cCommand_GotoWordNext_Sel,   'go to word next + select',     {$ifndef darwin} [],                           {$else} [scAlt+scShift+VK_RIGHT], {$endif} []);

  M.Add(cCommand_GotoWordPrev_Simple,     'go to word previous, simple',          [], []);
  M.Add(cCommand_GotoWordPrev_Simple_Sel, 'go to word previous, simple + select', [], []);
  M.Add(cCommand_GotoWordNext_Simple,     'go to word next, simple',          [], []);
  M.Add(cCommand_GotoWordNext_Simple_Sel, 'go to word next, simple + select', [], []);

  M.Add(cCommand_GotoWordEnd,        'go to word end',            [scXControl+VK_RIGHT], []);
  M.Add(cCommand_GotoWordEnd_Sel,    'go to word end + select',   [scXControl+scShift+VK_RIGHT], []);

  M.Add(cCommand_GotoLineAbsBegin,     'go to line abs. begin',          [], []);
  M.Add(cCommand_GotoLineAbsBegin_Sel, 'go to line abs. begin + select', [], []);
  M.Add(cCommand_GotoLineAbsEnd,       'go to line abs. end',          [], []);
  M.Add(cCommand_GotoLineAbsEnd_Sel,   'go to line abs. end + select', [], []);

  M.Add(cCommand_GotoScreenTop,      'go to screen top', [], []);
  M.Add(cCommand_GotoScreenBottom,   'go to screen bottom', [], []);
  M.Add(cCommand_GotoScreenCenter,   'go to screen center', [], []);

  M.Add(cCommand_SelectAll, 'selection: select all', [scXControl+VK_A], []);
  M.Add(cCommand_TextDeleteSelection, 'selection: delete selected text', [], []);
  M.Add(cCommand_SelectInverted, 'selection: invert selection', [], []);
  M.Add(cCommand_SelectSplitToLines, 'selection: split selection into lines', [], []);
  M.Add(cCommand_SelectExtendByLine, 'selection: expand selection to line, down', [scXControl+VK_L], []);
  M.Add(cCommand_SelectExtendByLineUp, 'selection: expand selection to line, up', [], []);
  M.Add(cCommand_SelectWords, 'selection: select words at carets', [], []);
  M.Add(cCommand_SelectLines, 'selection: select lines at carets', [], []);
  M.Add(cCommand_SelectNone, 'selection: cancel selection', [], []);

  M.Add(cCommand_Cancel, 'selection: cancel carets + selection + drag-drop', [VK_ESCAPE], []);
  M.Add(cCommand_CancelKeepSel, 'selection: cancel carets, but keep first caret/selection', [], []);
  M.Add(cCommand_CancelKeepLast, 'selection: cancel carets, but keep last caret', [], []);
  M.Add(cCommand_CancelKeepLastAndSel, 'selection: cancel carets, but keep last caret/selection', [], []);
  M.Add(cCommand_RemoveFirstCaret, 'selection: remove first caret', [], []);
  M.Add(cCommand_RemoveLastCaret, 'selection: remove last caret', [], []);

  M.Add(cCommand_ToggleOverwrite, 'toggle insert/overwrite mode', [VK_INSERT], []);
  M.Add(cCommand_ToggleReadOnly, 'toggle read-only mode', [], []);
  M.Add(cCommand_ToggleWordWrap, 'toggle word-wrap mode (off/window)', [scXControl+VK_U], []);
  M.Add(cCommand_ToggleWordWrapAlt, 'toggle word-wrap mode (off/window/margin)', [], []);

  M.Add(cCommand_ToggleUnprinted, 'toggle unprinted chars: enable other options', [], []);
  M.Add(cCommand_ToggleUnprintedSpaces, 'toggle unprinted chars: spaces/tabs', [], []);
  M.Add(cCommand_ToggleUnprintedSpacesTrailing, 'toggle unprinted chars: spaces/tabs trailing only', [], []);
  M.Add(cCommand_ToggleUnprintedEnds, 'toggle unprinted chars: ends', [], []);
  M.Add(cCommand_ToggleUnprintedEndDetails, 'toggle unprinted chars: end details', [], []);

  M.Add(cCommand_ToggleLineNums, 'toggle show line numbers', [], []);
  M.Add(cCommand_ToggleFolding, 'toggle show folding bar', [], []);
  M.Add(cCommand_ToggleRuler, 'toggle show ruler', [], []);
  M.Add(cCommand_ToggleMinimap, 'toggle show mini-map', [], []);
  M.Add(cCommand_ToggleMicromap, 'toggle show micro-map', [], []);

  M.Add(cCommand_ColSelectWithoutKey_Toggle, 'column selection without key modifier: toggle', [], []);
  M.Add(cCommand_ColSelectWithoutKey_On, 'column selection without key modifier: on', [], []);
  M.Add(cCommand_ColSelectWithoutKey_Off, 'column selection without key modifier: off', [], []);

  M.Add(cCommand_TextDeleteWordPrev, 'delete word previous', [scXControl+VK_BACK], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word next', [scXControl+VK_DELETE], []);
  M.Add(cCommand_TextDeleteWordEntire, 'delete word at caret', [], []);
  M.Add(cCommand_TextDeleteToLineBegin, 'delete to line begin', [], []);
  M.Add(cCommand_TextDeleteToLineEnd, 'delete to line end', [scXControl+VK_K], []);
  M.Add(cCommand_TextDeleteToTextBegin, 'delete to document begin', [], []);
  M.Add(cCommand_TextDeleteToTextEnd, 'delete to document end', [], []);

  M.Add(cCommand_TextIndent, 'indent selection', [scXControl+VK_I], []);
  M.Add(cCommand_TextUnindent, 'unindent selection', [scXControl+scShift+VK_I], [scShift+VK_TAB]);

  M.Add(cCommand_Undo, 'perform undo', [scXControl+VK_Z], []);
  M.Add(cCommand_Redo, 'perform redo', [scXControl+scShift+VK_Z], []);

  M.Add(cCommand_Sort_Asc,        '(without undo) sort asc.', [], []);
  M.Add(cCommand_Sort_AscNoCase,  '(without undo) sort asc., ignore case', [], []);
  M.Add(cCommand_Sort_Desc,       '(without undo) sort desc.', [], []);
  M.Add(cCommand_Sort_DescNoCase, '(without undo) sort desc., ignore case', [], []);

  M.Add(cCommand_DeleteAllBlanks,      '(without undo) delete all blank lines', [], []);
  M.Add(cCommand_DeleteAdjacentBlanks, '(without undo) delete adjacent blank lines', [], []);
  M.Add(cCommand_DeleteAllDups,        '(without undo) delete all duplicate lines', [], []);
  M.Add(cCommand_DeleteAllDupsKeepBlanks, '(without undo) delete all duplicate lines, keep blanks', [], []);
  M.Add(cCommand_DeleteAdjacentDups,   '(without undo) delete adjacent duplicate lines', [], []);
  M.Add(cCommand_ReverseLines,         '(without undo) reverse lines', [], []);
  M.Add(cCommand_ShuffleLines,         '(without undo) shuffle lines', [], []);

  M.Add(cCommand_ClipboardCopy, 'clipboard: copy', [scXControl+VK_C], [scXControl+VK_INSERT]);
  M.Add(cCommand_ClipboardCopyAdd, 'clipboard: copy/append', [], []);
  M.Add(cCommand_ClipboardCut, 'clipboard: cut', [scXControl+VK_X], [scShift+VK_DELETE]);
  M.Add(cCommand_ClipboardPaste, 'clipboard: paste', [scXControl+VK_V], [scShift+VK_INSERT]);
  M.Add(cCommand_ClipboardPaste_Select, 'clipboard: paste, select', [], []);
  M.Add(cCommand_ClipboardPaste_KeepCaret, 'clipboard: paste, keep caret', [], []);
  M.Add(cCommand_ClipboardPaste_Column, 'clipboard: paste, force column block', [], []);
  M.Add(cCommand_ClipboardPaste_ColumnKeepCaret, 'clipboard: paste, force column block, keep caret', [], []);

  M.Add(cCommand_ClipboardPasteAndIndent, 'clipboard: paste and indent', [], []);
  M.Add(cCommand_ClipboardPasteFromRecents, 'clipboard: paste from history', [], []);
  M.Add(cCommand_ClipboardClearRecents, 'clipboard: clear history', [], []);
  M.Add(cCommand_ClipboardAltPaste, 'clipboard: paste from primary selection (Unix)', [], []);

  M.Add(cCommand_ScrollToBegin, 'scroll to document begin', [], []);
  M.Add(cCommand_ScrollToEnd, 'scroll to document end', [], []);
  M.Add(cCommand_ScrollLineUp, 'scroll line up',     {$ifndef darwin} [scXControl+VK_UP], {$else} [], {$endif} []);
  M.Add(cCommand_ScrollLineDown, 'scroll line down', {$ifndef darwin} [scXControl+VK_DOWN], {$else} [], {$endif} []);
  M.Add(cCommand_ScrollPageUp, 'scroll page up', [], []);
  M.Add(cCommand_ScrollPageDown, 'scroll page down', [], []);
  M.Add(cCommand_ScrollColumnLeft, 'scroll column left', [], []);
  M.Add(cCommand_ScrollColumnRight, 'scroll column right', [], []);
  M.Add(cCommand_ScrollToCaretTop, 'scroll to top caret (of multi-carets)', [], []);
  M.Add(cCommand_ScrollToCaretBottom, 'scroll to bottom caret (of multi-carets)', [], []);
  M.Add(cCommand_ScrollToCaretLeft, 'scroll to left caret (of multi-carets)', [], []);
  M.Add(cCommand_ScrollToCaretRight, 'scroll to right caret (of multi-carets)', [], []);
  M.Add(cCommand_ScrollToLeft, 'scroll to left edge', [], []);

  M.Add(cCommand_MoveSelectionUp, 'move selected lines up', [scAlt+VK_UP], []);
  M.Add(cCommand_MoveSelectionDown, 'move selected lines down', [scAlt+VK_DOWN], []);
  M.Add(cCommand_TextInsertEmptyAbove, 'insert empty line above', [], []);
  M.Add(cCommand_TextInsertEmptyBelow, 'insert empty line below', [], []);

  M.Add(cCommand_CaretsExtendUpLine, 'carets extend: up a line', [], []);
  M.Add(cCommand_CaretsExtendUpPage, 'carets extend: up a page', [], []);
  M.Add(cCommand_CaretsExtendUpToTop, 'carets extend: up to top', [], []);
  M.Add(cCommand_CaretsExtendDownLine, 'carets extend: down a line', [], []);
  M.Add(cCommand_CaretsExtendDownPage, 'carets extend: down a page', [], []);
  M.Add(cCommand_CaretsExtendDownToEnd, 'carets extend: down to end', [], []);

  M.Add(cCommand_ZoomIn, 'current document font size: bigger', [{$ifndef darwin}scCtrl+VK_ADD{$endif}], []);
  M.Add(cCommand_ZoomOut, 'current document font size: smaller', [{$ifndef darwin}scCtrl+VK_SUBTRACT{$endif}], []);
  M.Add(cCommand_ZoomReset, 'current document font size: reset', [], []);

  M.Add(cCommand_TextCaseLower, 'convert case: lower case', [], []);
  M.Add(cCommand_TextCaseUpper, 'convert case: upper case', [], []);
  M.Add(cCommand_TextCaseTitle, 'convert case: title case', [], []);
  M.Add(cCommand_TextCaseInvert, 'convert case: invert case', [], []);
  M.Add(cCommand_TextCaseSentence, 'convert case: sentence case', [], []);

  M.Add(cCommand_TextTrimSpacesLeft, 'trim spaces: left', [], []);
  M.Add(cCommand_TextTrimSpacesRight, 'trim spaces: right', [], []);
  M.Add(cCommand_TextTrimSpacesAll, 'trim spaces: all', [], []);

  M.Add(cCommand_RepeatTextCommand, 'repeat last text command', [], []);

  M.Add(cCommand_FoldAll, 'folding: fold all', [], []);
  M.Add(cCommand_UnfoldAll, 'folding: unfold all', [], []);
  M.Add(cCommand_FoldLevel1, 'folding: fold level 1', [], []);
  M.Add(cCommand_FoldLevel2, 'folding: fold level 2', [], []);
  M.Add(cCommand_FoldLevel3, 'folding: fold level 3', [], []);
  M.Add(cCommand_FoldLevel4, 'folding: fold level 4', [], []);
  M.Add(cCommand_FoldLevel5, 'folding: fold level 5', [], []);
  M.Add(cCommand_FoldLevel6, 'folding: fold level 6', [], []);
  M.Add(cCommand_FoldLevel7, 'folding: fold level 7', [], []);
  M.Add(cCommand_FoldLevel8, 'folding: fold level 8', [], []);
  M.Add(cCommand_FoldLevel9, 'folding: fold level 9', [], []);
  M.Add(cCommand_FoldingFoldAtCurLine, 'folding: fold block at current line', [], []);
  M.Add(cCommand_FoldingUnfoldAtCurLine, 'folding: unfold block at current line', [], []);
  M.Add(cCommand_FoldingToggleAtCurLine, 'folding: toggle block at current line', [], []);
  M.Add(cCommand_FoldingFoldSelection, 'folding: fold selected lines', [], []);
end;

procedure InitKeymapCombo(M: TATKeymap);
begin
  M.Clear;

  InitKeymap_Arrows(M);
  InitKeymap_HomeEnd(M);
  InitKeymap_PageUpDown(M);

  M.Add(cCommand_KeyBackspace, 'delete char left (backspace)', [VK_BACK], []);
  M.Add(cCommand_KeyDelete, 'delete char right (delete)', [VK_DELETE], []);
  M.Add(cCommand_KeyTab, 'tabulation key', [], []);

  M.Add(cCommand_GotoWordPrev,       'go to word left',           [scXControl+VK_LEFT], []);
  M.Add(cCommand_GotoWordPrev_Sel,   'go to word left + select',  [scXControl+scShift+VK_LEFT], []);
  M.Add(cCommand_GotoWordNext,       'go to word right',          [scXControl+VK_RIGHT], []);
  M.Add(cCommand_GotoWordNext_Sel,   'go to word right + select', [scXControl+scShift+VK_RIGHT], []);

  M.Add(cCommand_SelectAll, 'selection: select all', [scXControl+VK_A], []);
  M.Add(cCommand_TextDeleteSelection, 'selection: delete selected text', [], []);

  M.Add(cCommand_ToggleOverwrite, 'toggle insert/overwrite mode', [VK_INSERT], []);

  M.Add(cCommand_TextDeleteWordPrev, 'delete word left', [scXControl+VK_BACK], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word right', [scXControl+VK_DELETE], []);
  M.Add(cCommand_TextDeleteWordEntire, 'delete word', [], []);

  M.Add(cCommand_Undo, 'perform undo', [scXControl+VK_Z], []);
  M.Add(cCommand_Redo, 'perform redo', [scXControl+scShift+VK_Z], []);

  M.Add(cCommand_ClipboardCopy, 'clipboard: copy', [scXControl+VK_C], [scXControl+VK_INSERT]);
  M.Add(cCommand_ClipboardCopyAdd, 'clipboard: copy/append', [], []);
  M.Add(cCommand_ClipboardCut, 'clipboard: cut', [scXControl+VK_X], [scShift+VK_DELETE]);
  M.Add(cCommand_ClipboardPaste, 'clipboard: paste', [scXControl+VK_V], [scShift+VK_INSERT]);

  M.Add(cCommand_ComboboxRecentsMenu, 'combobox: recent items menu', [scAlt+VK_DOWN], [scXControl+VK_DOWN]);
end;


initialization
  KeymapFull:= TATKeymap.Create;
  KeymapCombo:= TATKeymap.Create;
  InitKeymapFull(KeymapFull);
  InitKeymapCombo(KeymapCombo);

finalization
  FreeAndNil(KeymapFull);
  FreeAndNil(KeymapCombo);

end.

