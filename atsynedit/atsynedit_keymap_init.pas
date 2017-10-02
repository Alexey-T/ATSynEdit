{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Keymap_Init;

//{$define test_combo}

interface

uses
  ATSynEdit_Keymap,
  ATSynEdit_Commands;

procedure InitKeymapFull(var M: TATKeymap);
procedure InitKeymapCombo(var M: TATKeymap);

var
  KeymapFull: TATKeymap = nil;
  KeymapCombo: TATKeymap = nil;

implementation

uses
  SysUtils,
  LCLProc,
  Dialogs;

const
  //Mac: instead of Ctrl use Command-key
  cXControl = {$ifdef darwin} 'Meta' {$else} 'Ctrl' {$endif};


procedure InitKeymapFull(var M: TATKeymap);
begin
  M.Clear;

  M.Add(cCommand_KeyLeft,         'caret char left',               ['Left'], []);
  M.Add(cCommand_KeyLeft_Sel,     'caret char left + select',      ['Shift+Left'], []);
  M.Add(cCommand_KeyRight,        'caret char right',              ['Right'], []);
  M.Add(cCommand_KeyRight_Sel,    'caret char right + select',     ['Shift+Right'], []);
  M.Add(cCommand_KeyUp,           'caret line up',                 ['Up'], []);
  M.Add(cCommand_KeyUp_Sel,       'caret line up + select',        ['Shift+Up'], []);
  M.Add(cCommand_KeyDown,         'caret line down',               ['Down'], []);
  M.Add(cCommand_KeyDown_Sel,     'caret line down + select',      ['Shift+Down'], []);
  M.Add(cCommand_KeyHome,         'caret to line start',           ['Home'], []);
  M.Add(cCommand_KeyHome_Sel,     'caret to line start + select',  ['Shift+Home'], []);
  M.Add(cCommand_KeyEnd,          'caret to line end',             ['End'], []);
  M.Add(cCommand_KeyEnd_Sel,      'caret to line end + select',    ['Shift+End'], []);
  M.Add(cCommand_KeyPageUp,       'caret page up',                 ['PgUp'], []);
  M.Add(cCommand_KeyPageUp_Sel,   'caret page up + select',        ['Shift+PgUp'], []);
  M.Add(cCommand_KeyPageDown,     'caret page down',               ['PgDn'], []);
  M.Add(cCommand_KeyPageDown_Sel, 'caret page down + select',      ['Shift+PgDn'], []);

  M.Add(cCommand_ColSelectLeft,        'column select: left',          ['Shift+Alt+Left'], []);
  M.Add(cCommand_ColSelectRight,       'column select: right',         ['Shift+Alt+Right'], []);
  M.Add(cCommand_ColSelectUp,          'column select: up',            ['Shift+Alt+Up'], []);
  M.Add(cCommand_ColSelectDown,        'column select: down',          ['Shift+Alt+Down'], []);
  M.Add(cCommand_ColSelectPageUp,      'column select: page up',       ['Shift+Alt+PgUp'], []);
  M.Add(cCommand_ColSelectPageDown,    'column select: page down',     ['Shift+Alt+PgDn'], []);
  M.Add(cCommand_ColSelectToLineBegin, 'column select: to line begin', ['Shift+Alt+Home'], []);
  M.Add(cCommand_ColSelectToLineEnd,   'column select: to line end',   ['Shift+Alt+End'], []);

  M.Add(cCommand_KeyBackspace, 'delete char left (backspace)', ['Bksp'], []);
  M.Add(cCommand_KeyDelete, 'delete char right (delete)', ['Del'], []);
  M.Add(cCommand_KeyEnter, 'insert line-break (enter)', ['Enter'], []);
  M.Add(cCommand_KeyTab, 'tabulation key', [], []);
  M.Add(cCommand_TextInsertTabChar, 'insert tab char', [], []);

  M.Add(cCommand_ForceFinalEndOfLine, 'force final end-of-line', [], []);

  M.Add(cCommand_TextDeleteLine, 'delete line', [cXControl+'+Y'], []);
  M.Add(cCommand_TextDuplicateLine, 'duplicate line', [cXControl+'+D'], []);

  M.Add(cCommand_GotoTextBegin,      'go to text begin',          [cXControl+'+Home'], []);
  M.Add(cCommand_GotoTextBegin_Sel,  'go to text begin + select', [cXControl+'+Shift+Home'], []);
  M.Add(cCommand_GotoTextEnd,        'go to text end',            [cXControl+'+End'], []);
  M.Add(cCommand_GotoTextEnd_Sel,    'go to text end + select',   [cXControl+'+Shift+End'], []);

  M.Add(cCommand_GotoWordPrev,       'go to word left',           [cXControl+'+Left'], []);
  M.Add(cCommand_GotoWordPrev_Sel,   'go to word left + select',  [cXControl+'+Shift+Left'], []);
  M.Add(cCommand_GotoWordNext,       'go to word right',          [cXControl+'+Right'], []);
  M.Add(cCommand_GotoWordNext_Sel,   'go to word right + select', [cXControl+'+Shift+Right'], []);
  M.Add(cCommand_GotoWordEnd,        'go to word end',            [], []);
  M.Add(cCommand_GotoWordEnd_Sel,    'go to word end + select',   [], []);

  M.Add(cCommand_GotoLineAbsBegin,     'go to line abs. begin',          [], []);
  M.Add(cCommand_GotoLineAbsBegin_Sel, 'go to line abs. begin + select', [], []);
  M.Add(cCommand_GotoLineAbsEnd,       'go to line abs. end',          [], []);
  M.Add(cCommand_GotoLineAbsEnd_Sel,   'go to line abs. end + select', [], []);

  M.Add(cCommand_GotoScreenTop,      'go to screen top', [], []);
  M.Add(cCommand_GotoScreenBottom,   'go to screen bottom', [], []);
  M.Add(cCommand_GotoScreenCenter,   'go to screen center', [], []);

  M.Add(cCommand_SelectAll, 'selection: select all', [cXControl+'+A'], []);
  M.Add(cCommand_TextDeleteSelection, 'selection: delete selected text', [], []);
  M.Add(cCommand_SelectInverted, 'selection: invert selection', [], []);
  M.Add(cCommand_SelectSplitToLines, 'selection: split selection into lines', [], []);
  M.Add(cCommand_SelectExtendByLine, 'selection: extend selection by line', [cXControl+'+L'], []);
  M.Add(cCommand_SelectWords, 'selection: select words at carets', [], []);
  M.Add(cCommand_SelectLines, 'selection: select lines at carets', [], []);
  M.Add(cCommand_SelectNone, 'selection: cancel selection', [], []);
  M.Add(cCommand_Cancel, 'selection: cancel carets, selection, drag-drop', ['Esc'], []);

  M.Add(cCommand_ToggleOverwrite, 'toggle insert/overwrite mode', ['Ins'], []);
  M.Add(cCommand_ToggleReadOnly, 'toggle read-only mode', ['Ctrl+Shift+R'], []);
  M.Add(cCommand_ToggleWordWrap, 'toggle word-wrap mode (off/window)', [cXControl+'+U'], []);
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

  M.Add(cCommand_TextDeleteWordPrev, 'delete word left', [cXControl+'+Bksp'], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word right', [cXControl+'+Del'], []);
  M.Add(cCommand_TextDeleteToLineBegin, 'delete to line start', [], []);
  M.Add(cCommand_TextDeleteToLineEnd, 'delete to line end', [cXControl+'+K'], []);
  M.Add(cCommand_TextDeleteToTextEnd, 'delete to text end', [], []);

  M.Add(cCommand_TextIndent, 'indent selection', [cXControl+'+I'], []);
  M.Add(cCommand_TextUnindent, 'unindent selection', [cXControl+'+Shift+I'], ['Shift+Tab']);

  M.Add(cCommand_Undo, 'perform undo', [cXControl+'+Z'], []);
  M.Add(cCommand_Redo, 'perform redo', [cXControl+'+Shift+Z'], []);

  M.Add(cCommand_ClipboardCopy, 'clipboard: copy', [cXControl+'+C'], [cXControl+'+Ins']);
  M.Add(cCommand_ClipboardCopyAdd, 'clipboard: copy/append', [], []);
  M.Add(cCommand_ClipboardCut, 'clipboard: cut', [cXControl+'+X'], ['Shift+Del']);
  M.Add(cCommand_ClipboardPaste, 'clipboard: paste', [cXControl+'+V'], ['Shift+Ins']);
  M.Add(cCommand_ClipboardPaste_Select, 'clipboard: paste, select', [], []);
  M.Add(cCommand_ClipboardPaste_KeepCaret, 'clipboard: paste, keep caret', [], []);
  M.Add(cCommand_ClipboardPaste_Column, 'clipboard: paste, force column block', [], []);
  M.Add(cCommand_ClipboardPaste_ColumnKeepCaret, 'clipboard: paste, force column block, keep caret', [], []);

  M.Add(cCommand_ClipboardAltPaste, 'clipboard primary (gtk): paste', [], []);
  M.Add(cCommand_ClipboardAltAltPaste, 'clipboard secondary (gtk): paste', [], []);

  M.Add(cCommand_ScrollLineUp, 'scroll line up', [cXControl+'+Up'], []);
  M.Add(cCommand_ScrollLineDown, 'scroll line down', [cXControl+'+Down'], []);
  M.Add(cCommand_ScrollToCaretTop, 'scroll to top caret (of multi-carets)', [], []);
  M.Add(cCommand_ScrollToCaretBottom, 'scroll to bottom caret (of multi-carets)', [], []);
  M.Add(cCommand_ScrollToCaretLeft, 'scroll to left caret (of multi-carets)', [], []);
  M.Add(cCommand_ScrollToCaretRight, 'scroll to right caret (of multi-carets)', [], []);

  M.Add(cCommand_MoveSelectionUp, 'move selected lines up', ['Alt+Up'], []);
  M.Add(cCommand_MoveSelectionDown, 'move selected lines down', ['Alt+Down'], []);
  M.Add(cCommand_TextInsertEmptyAbove, 'insert empty line above', [], []);
  M.Add(cCommand_TextInsertEmptyBelow, 'insert empty line below', [], []);

  M.Add(cCommand_CaretsExtendUpLine, 'carets extend: up a line', [], []);
  M.Add(cCommand_CaretsExtendUpPage, 'carets extend: up a page', [], []);
  M.Add(cCommand_CaretsExtendUpToTop, 'carets extend: up to top', [], []);
  M.Add(cCommand_CaretsExtendDownLine, 'carets extend: down a line', [], []);
  M.Add(cCommand_CaretsExtendDownPage, 'carets extend: down a page', [], []);
  M.Add(cCommand_CaretsExtendDownToEnd, 'carets extend: down to end', [], []);

  {$ifdef test_combo}
  M.Add(cCommand_ZoomIn, 'zoom in', ['Ctrl+B', 'Ctrl+P'], []);
  M.Add(cCommand_ZoomOut, 'zoom out', ['Ctrl+B', 'Ctrl+B', 'Ctrl+M'], []);
  {$else}
  M.Add(cCommand_ZoomIn, 'zoom in', [], []);
  M.Add(cCommand_ZoomOut, 'zoom out', [], []);
  {$endif}

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
end;

procedure InitKeymapCombo(var M: TATKeymap);
begin
  M.Clear;

  M.Add(cCommand_KeyLeft,         'caret char left',               ['Left'], []);
  M.Add(cCommand_KeyLeft_Sel,     'caret char left + select',      ['Shift+Left'], []);
  M.Add(cCommand_KeyRight,        'caret char right',              ['Right'], []);
  M.Add(cCommand_KeyRight_Sel,    'caret char right + select',     ['Shift+Right'], []);
  M.Add(cCommand_KeyHome,         'caret to line start',           ['Home'], []);
  M.Add(cCommand_KeyHome_Sel,     'caret to line start + select',  ['Shift+Home'], []);
  M.Add(cCommand_KeyEnd,          'caret to line end',             ['End'], []);
  M.Add(cCommand_KeyEnd_Sel,      'caret to line end + select',    ['Shift+End'], []);

  M.Add(cCommand_KeyBackspace, 'delete char left (backspace)', ['Bksp'], []);
  M.Add(cCommand_KeyDelete, 'delete char right (delete)', ['Del'], []);
  M.Add(cCommand_KeyEnter, 'insert line-break (enter, ctrl+enter)', ['Enter'], [cXControl+'+Enter']);
  M.Add(cCommand_KeyTab, 'tabulation key', [], []);

  M.Add(cCommand_GotoWordPrev,       'go to word left',           [cXControl+'+Left'], []);
  M.Add(cCommand_GotoWordPrev_Sel,   'go to word left + select',  [cXControl+'+Shift+Left'], []);
  M.Add(cCommand_GotoWordNext,       'go to word right',          [cXControl+'+Right'], []);
  M.Add(cCommand_GotoWordNext_Sel,   'go to word right + select', [cXControl+'+Shift+Right'], []);

  M.Add(cCommand_SelectAll, 'selection: select all', [cXControl+'+A'], []);
  M.Add(cCommand_TextDeleteSelection, 'selection: delete selected text', [], []);

  M.Add(cCommand_ToggleOverwrite, 'toggle insert/overwrite mode', ['Ins'], []);

  M.Add(cCommand_TextDeleteWordPrev, 'delete word left', [cXControl+'+Bksp'], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word right', [cXControl+'+Del'], []);

  M.Add(cCommand_Undo, 'perform undo', [cXControl+'+Z'], []);
  M.Add(cCommand_Redo, 'perform redo', [cXControl+'+Shift+Z'], []);

  M.Add(cCommand_ClipboardCopy, 'clipboard: copy', [cXControl+'+C'], [cXControl+'+Ins']);
  M.Add(cCommand_ClipboardCopyAdd, 'clipboard: copy/append', [], []);
  M.Add(cCommand_ClipboardCut, 'clipboard: cut', [cXControl+'+X'], ['Shift+Del']);
  M.Add(cCommand_ClipboardPaste, 'clipboard: paste', [cXControl+'+V'], ['Shift+Ins']);

  M.Add(cCommand_ComboboxRecentsMenu, 'combobox: recent items menu', ['Alt+Down'], [cXControl+'+Down']);

  M.Add(cCommand_KeyUp,        'blocked: caret line up',   ['Up'], []);
  M.Add(cCommand_KeyDown,      'blocked: caret line down', ['Down'], []);
  M.Add(cCommand_KeyPageUp,    'blocked: caret page up',   ['PgUp'], []);
  M.Add(cCommand_KeyPageDown,  'blocked: caret page down', ['PgDn'], []);
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

