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
  M.Add(cCommand_KeyTabChar, 'tabulation: tab-char', ['Ctrl+I'], []);

  M.Add(cCommand_TextDeleteCurLine, 'delete current line', ['Ctrl+Y'], []);
  M.Add(cCommand_TextDuplicateCurLine, 'duplicate current line', ['Ctrl+D'], []);

  M.Add(cCommand_SelectAll, 'select all', ['Ctrl+A'], []);
  M.Add(cCommand_SelectWords, 'select words at carets', [], []);
  M.Add(cCommand_SelectLines, 'select lines at carets', [], []);

  M.Add(cCommand_GotoTextBegin,      'goto text begin',          ['Ctrl+Home'], []);
  M.Add(cCommand_GotoTextBegin_Sel,  'goto text begin + select', ['Ctrl+Shift+Home'], []);
  M.Add(cCommand_GotoTextEnd,        'goto text end',            ['Ctrl+End'], []);
  M.Add(cCommand_GotoTextEnd_Sel,    'goto text end + select',   ['Ctrl+Shift+End'], []);
  M.Add(cCommand_GotoWordPrev,       'goto word left',           ['Ctrl+Left'], []);
  M.Add(cCommand_GotoWordPrev_Sel,   'goto word left + select',  ['Ctrl+Shift+Left'], []);
  M.Add(cCommand_GotoWordNext,       'goto word right',          ['Ctrl+Right'], []);
  M.Add(cCommand_GotoWordNext_Sel,   'goto word right + select', ['Ctrl+Shift+Right'], []);

  M.Add(cCommand_ToggleOvr, 'toggle insert/overwrite mode', ['Insert'], []);
  M.Add(cCommand_ToggleReadOnly, 'toggle read-only mode', ['Ctrl+R'], []);

  M.Add(cCommand_TextDeleteWordPrev, 'delete word left', ['Ctrl+Backspace'], []);
  M.Add(cCommand_TextDeleteWordNext, 'delete word right', ['Ctrl+Delete'], []);
  M.Add(cCommand_TextDeleteToLineBegin, 'delete to line start', [], []);
  M.Add(cCommand_TextDeleteToLineEnd, 'delete to line end', ['Ctrl+K'], []);

  M.Add(cCommand_ScrollLineUp, 'scroll line up', ['Ctrl+Up'], []);
  M.Add(cCommand_ScrollLineDown, 'scroll line down', ['Ctrl+Down'], []);
  M.Add(cCommand_ScrollToCaretTop, 'scroll to caret, top', [], []);
  M.Add(cCommand_ScrollToCaretBottom, 'scroll to caret, bottom', [], []);
  M.Add(cCommand_ScrollToCaretLeft, 'scroll to caret, left', [], []);
  M.Add(cCommand_ScrollToCaretRight, 'scroll to caret, right', [], []);

  M.Add(cCommand_ClipboardPaste, 'clipboard: paste', ['Ctrl+V'], []);
  M.Add(cCommand_ClipboardPaste_KeepCaret, 'clipboard: paste, keep caret', [], []);

  M.Add(cCommand_CaretsRemove, 'carets: remove multi-carets', ['Escape'], []);
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

