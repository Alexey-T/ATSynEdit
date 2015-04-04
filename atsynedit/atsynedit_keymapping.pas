unit ATSynEdit_Keymapping;

interface

uses
  ATKeyMapping,
  ATSynEdit_Commands;

procedure DoInitDefaultKeymapping(var M: TATKeyMapping);

implementation

uses
  SysUtils,
  LCLProc,
  Dialogs;

const
  //Mac: instead of Ctrl use Command-key
  cXControl = {$ifdef darwin} 'Meta' {$else} 'Ctrl' {$endif};


procedure DoInitDefaultKeymapping(var M: TATKeyMapping);
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

  M.Add(cCommand_SelectAll, 'select all', [cXControl+'+A'], []);
  M.Add(cCommand_SelectWords, 'select words at carets', [], []);
  M.Add(cCommand_SelectLines, 'select lines at carets', [], []);
  M.Add(cCommand_SelectNone, 'cancel selection', [], []);
  M.Add(cCommand_Cancel, 'cancel carets, selection, drag-drop', ['Esc'], []);

  M.Add(cCommand_ToggleOverwrite, 'toggle insert/overwrite mode', ['Ins'], []);
  M.Add(cCommand_ToggleReadOnly, 'toggle read-only mode', [cXControl+'+R'], []);

  M.Add(cCommand_TextDeleteSelection, 'delete selection', [], []);
  M.Add(cCommand_TextDeleteWordPrev, 'delete word left', [cXControl+'+Bksp'], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word right', [cXControl+'+Del'], []);
  M.Add(cCommand_TextDeleteToLineBegin, 'delete to line start', [], []);
  M.Add(cCommand_TextDeleteToLineEnd, 'delete to line end', [cXControl+'+K'], []);
  M.Add(cCommand_TextDeleteToTextEnd, 'delete to text end', [], []);

  M.Add(cCommand_TextIndent, 'indent selection', [cXControl+'+I'], []);
  M.Add(cCommand_TextUnindent, 'unindent selection', [cXControl+'+U'], ['Shift+Tab']);

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

  M.Add(cCommand_CaretsExtendUpLine, 'carets extend: up a line', ['Alt+Up'], []);
  M.Add(cCommand_CaretsExtendUpPage, 'carets extend: up a page', [], []);
  M.Add(cCommand_CaretsExtendUpToTop, 'carets extend: up to top', [], []);
  M.Add(cCommand_CaretsExtendDownLine, 'carets extend: down a line', ['Alt+Down'], []);
  M.Add(cCommand_CaretsExtendDownPage, 'carets extend: down a page', [], []);
  M.Add(cCommand_CaretsExtendDownToEnd, 'carets extend: down to end', [], []);
end;


end.

