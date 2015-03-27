unit ATSynEdit_Keymapping;

interface

uses
  ATKeyMapping, ATSynEdit_Commands;

procedure DoInitDefaultKeymapping(var M: TATKeyMapping);
procedure DoCheckKeyMapping(const M: TATKeyMapping);

implementation

uses
  LCLProc,
  Dialogs;

const
  cXCtrl = {$ifdef darwin} 'Meta' {$else} 'Ctrl' {$endif};
  cXCtrlShift = {$ifdef darwin} 'Shift+Meta' {$else} 'Ctrl+Shift' {$endif};


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
  M.Add(cCommand_KeyPageUp,       'caret page up',                 ['Prior'], []);
  M.Add(cCommand_KeyPageUp_Sel,   'caret page up + select',        ['Shift+Prior'], []);
  M.Add(cCommand_KeyPageDown,     'caret page down',               ['Next'], []);
  M.Add(cCommand_KeyPageDown_Sel, 'caret page down + select',      ['Shift+Next'], []);

  M.Add(cCommand_KeyBackspace, 'delete char left (backspace)', ['Backspace'], []);
  M.Add(cCommand_KeyDelete, 'delete char right (delete)', ['Delete'], []);
  M.Add(cCommand_KeyEnter, 'insert line-break (enter)', ['Return'], []);
  M.Add(cCommand_KeyTab, 'tabulation', [], []);
  M.Add(cCommand_KeyTabChar, 'tabulation: tab-char', [], []);

  M.Add(cCommand_TextDeleteLine, 'delete line', [cXCtrl+'+Y'], []);
  M.Add(cCommand_TextDuplicateLine, 'duplicate line', [cXCtrl+'+D'], []);

  M.Add(cCommand_GotoTextBegin,      'goto text begin',          [cXCtrl+'+Home'], []);
  M.Add(cCommand_GotoTextBegin_Sel,  'goto text begin + select', [cXCtrlShift+'+Home'], []);
  M.Add(cCommand_GotoTextEnd,        'goto text end',            [cXCtrl+'+End'], []);
  M.Add(cCommand_GotoTextEnd_Sel,    'goto text end + select',   [cXCtrlShift+'+End'], []);
  M.Add(cCommand_GotoWordPrev,       'goto word left',           [cXCtrl+'+Left'], []);
  M.Add(cCommand_GotoWordPrev_Sel,   'goto word left + select',  [cXCtrlShift+'+Left'], []);
  M.Add(cCommand_GotoWordNext,       'goto word right',          [cXCtrl+'+Right'], []);
  M.Add(cCommand_GotoWordNext_Sel,   'goto word right + select', [cXCtrlShift+'+Right'], []);

  M.Add(cCommand_SelectAll, 'select all', [cXCtrl+'+A'], []);
  M.Add(cCommand_SelectWords, 'select words at carets', [], []);
  M.Add(cCommand_SelectLines, 'select lines at carets', [], []);
  M.Add(cCommand_SelectNone, 'cancel selection', [], []);
  M.Add(cCommand_Cancel, 'cancel carets, selection, drag-drop', ['Escape'], []);

  M.Add(cCommand_ToggleOverwrite, 'toggle insert/overwrite mode', ['Insert'], []);
  M.Add(cCommand_ToggleReadOnly, 'toggle read-only mode', [cXCtrl+'+R'], []);

  M.Add(cCommand_TextDeleteWordPrev, 'delete word left', [cXCtrl+'+Backspace'], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word right', [cXCtrl+'+Delete'], []);
  M.Add(cCommand_TextDeleteToLineBegin, 'delete to line start', [], []);
  M.Add(cCommand_TextDeleteToLineEnd, 'delete to line end', [cXCtrl+'+K'], []);
  M.Add(cCommand_TextDeleteSelection, 'delete selection', [], []);

  M.Add(cCommand_TextIndent, 'indent selection', [cXCtrl+'+I'], []);
  M.Add(cCommand_TextUnindent, 'unindent selection', [cXCtrl+'+U'], ['Shift+Tab']);

  M.Add(cCommand_ScrollLineUp, 'scroll line up', [cXCtrl+'+Up'], []);
  M.Add(cCommand_ScrollLineDown, 'scroll line down', [cXCtrl+'+Down'], []);
  M.Add(cCommand_ScrollToCaretTop, 'scroll to caret, top', [], []);
  M.Add(cCommand_ScrollToCaretBottom, 'scroll to caret, bottom', [], []);
  M.Add(cCommand_ScrollToCaretLeft, 'scroll to caret, left', [], []);
  M.Add(cCommand_ScrollToCaretRight, 'scroll to caret, right', [], []);

  M.Add(cCommand_ClipboardCopy, 'clipboard: copy', [cXCtrl+'+C'], [cXCtrl+'+Insert']);
  M.Add(cCommand_ClipboardCopyAdd, 'clipboard: copy/add', [], []);
  M.Add(cCommand_ClipboardCut, 'clipboard: cut', [cXCtrl+'+X'], ['Shift+Delete']);
  M.Add(cCommand_ClipboardPaste, 'clipboard: paste', [cXCtrl+'+V'], ['Shift+Insert']);
  M.Add(cCommand_ClipboardPaste_Sel, 'clipboard: paste + select', ['Alt+V'], []);
  M.Add(cCommand_ClipboardPaste_KeepCaret, 'clipboard: paste, keep caret', [], []);
end;

procedure DoCheckKeyMapping(const M: TATKeyMapping);
var
  i: integer;
begin
  for i:= 0 to M.Count-1 do
    with M[i] do
    begin
      //DoCheckText(Keys1[0]);
      //DoCheckText(Keys2[0]);
    end;
end;

end.

