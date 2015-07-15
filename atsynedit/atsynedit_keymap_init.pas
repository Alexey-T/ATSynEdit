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

  M.Add(cCommand_ColSelectLeft,  'column select: left',  ['Shift+Alt+Left'], []);
  M.Add(cCommand_ColSelectRight, 'column select: right', ['Shift+Alt+Right'], []);
  M.Add(cCommand_ColSelectUp,    'column select: up',    ['Shift+Alt+Up'], []);
  M.Add(cCommand_ColSelectDown,  'column select: down',  ['Shift+Alt+Down'], []);

  M.Add(cCommand_KeyBackspace, 'delete char left (backspace)', ['Bksp'], []);
  M.Add(cCommand_KeyDelete, 'delete char right (delete)', ['Del'], []);
  M.Add(cCommand_KeyEnter, 'insert line-break (enter)', ['Enter'], []);
  M.Add(cCommand_KeyTab, 'tabulation key', [], []);
  M.Add(cCommand_TextInsertTabChar, 'insert tab char', [], []);

  M.Add(cCommand_TextDeleteLine, 'delete line', [cXControl+'+Y'], []);
  M.Add(cCommand_TextDuplicateLine, 'duplicate line', [cXControl+'+D'], []);

  M.Add(cCommand_GotoTextBegin,      'goto text begin',          [cXControl+'+Home'], []);
  M.Add(cCommand_GotoTextBegin_Sel,  'goto text begin + select', [cXControl+'+Shift+Home'], []);
  M.Add(cCommand_GotoTextEnd,        'goto text end',            [cXControl+'+End'], []);
  M.Add(cCommand_GotoTextEnd_Sel,    'goto text end + select',   [cXControl+'+Shift+End'], []);
  M.Add(cCommand_GotoWordPrev,       'goto word left',           [cXControl+'+Left'], []);
  M.Add(cCommand_GotoWordPrev_Sel,   'goto word left + select',  [cXControl+'+Shift+Left'], []);
  M.Add(cCommand_GotoWordNext,       'goto word right',          [cXControl+'+Right'], []);
  M.Add(cCommand_GotoWordNext_Sel,   'goto word right + select', [cXControl+'+Shift+Right'], []);

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
  M.Add(cCommand_ToggleReadOnly, 'toggle read-only mode', [cXControl+'+R'], []);
  M.Add(cCommand_ToggleWordWrap, 'toggle word-wrap mode', [cXControl+'+U'], []);
  M.Add(cCommand_ToggleUnprinted, 'toggle show unprinted chars', [], []);
  M.Add(cCommand_ToggleLineNums, 'toggle show line numbers', [], []);
  M.Add(cCommand_ToggleFolding, 'toggle show folding bar', [], []);
  M.Add(cCommand_ToggleRuler, 'toggle show ruler', [], []);
  M.Add(cCommand_ToggleMinimap, 'toggle show minimap', [], []);

  M.Add(cCommand_TextDeleteWordPrev, 'delete word left', [cXControl+'+Bksp'], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word right', [cXControl+'+Del'], []);
  M.Add(cCommand_TextDeleteToLineBegin, 'delete to line start', [], []);
  M.Add(cCommand_TextDeleteToLineEnd, 'delete to line end', [cXControl+'+K'], []);
  M.Add(cCommand_TextDeleteToTextEnd, 'delete to text end', [], []);

  M.Add(cCommand_TextIndent, 'indent selection', [cXControl+'+I'], []);
  M.Add(cCommand_TextUnindent, 'unindent selection', ['Shift+Tab'], []);

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

  M.Add(cCommand_ScrollLineUp, 'scroll line up', [cXControl+'+Up'], []);
  M.Add(cCommand_ScrollLineDown, 'scroll line down', [cXControl+'+Down'], []);
  M.Add(cCommand_ScrollToCaretTop, 'scroll to caret, top', [], []);
  M.Add(cCommand_ScrollToCaretBottom, 'scroll to caret, bottom', [], []);
  M.Add(cCommand_ScrollToCaretLeft, 'scroll to caret, left', [], []);
  M.Add(cCommand_ScrollToCaretRight, 'scroll to caret, right', [], []);

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

  M.Add(cCommand_RepeatTextCommand, 'repeat last text command', [], []);
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
  M.Add(cCommand_KeyEnter, 'insert line-break (enter)', ['Enter'], []);
  M.Add(cCommand_KeyTab, 'tabulation key', [], []);

  M.Add(cCommand_GotoWordPrev,       'goto word left',           [cXControl+'+Left'], []);
  M.Add(cCommand_GotoWordPrev_Sel,   'goto word left + select',  [cXControl+'+Shift+Left'], []);
  M.Add(cCommand_GotoWordNext,       'goto word right',          [cXControl+'+Right'], []);
  M.Add(cCommand_GotoWordNext_Sel,   'goto word right + select', [cXControl+'+Shift+Right'], []);

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

  M.Add(cCommand_RecentsPopup, 'recents popup menu', ['Alt+Down'], [cXControl+'+Down']);
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

