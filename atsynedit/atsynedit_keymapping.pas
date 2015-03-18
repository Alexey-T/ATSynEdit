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

  M.Add(cCommand_KeyLeft, 'caret char left',          ['Left'],  ['Shift+Left']);
  M.Add(cCommand_KeyRight, 'caret char right',        ['Right'], ['Shift+Right']);
  M.Add(cCommand_KeyUp, 'caret line up',              ['Up'],    ['Shift+Up']);
  M.Add(cCommand_KeyDown, 'caret line down',          ['Down'],  ['Shift+Down']);
  M.Add(cCommand_KeyHome, 'caret to line start',      ['Home'],  ['Shift+Home']);
  M.Add(cCommand_KeyEnd, 'caret to line end',         ['End'],   ['Shift+End']);
  M.Add(cCommand_KeyPageUp, 'caret page up',          ['Prior'], ['Shift+Prior']);
  M.Add(cCommand_KeyPageDown, 'caret page down',      ['Next'],  ['Shift+Next']);

  M.Add(cCommand_KeyBackspace, 'delete char left (backspace)', ['Backspace'], []);
  M.Add(cCommand_KeyDelete, 'delete char right (delete)', ['Delete'], []);
  M.Add(cCommand_KeyEnter, 'insert line-break (enter)', ['Return'], []);
  M.Add(cCommand_KeyTab, 'tabulation', [], []);
  M.Add(cCommand_KeyTabChar, 'tabulation: tab-char', ['Ctrl+I'], []);

  M.Add(cCommand_TextDeleteCurLine, 'delete current line', ['Ctrl+Y'], []);
  M.Add(cCommand_TextDuplicateCurLine, 'duplicate current line', ['Ctrl+D'], []);

  M.Add(cCommand_GotoTextBegin, 'goto text begin', ['Ctrl+Home'], ['Ctrl+Shift+Home']);
  M.Add(cCommand_GotoTextEnd, 'goto text end', ['Ctrl+End'], ['Ctrl+Shift+End']);
  M.Add(cCommand_GotoWordNext, 'goto word next', ['Ctrl+Right'], ['Ctrl+Shift+Right']);
  M.Add(cCommand_GotoWordPrev, 'goto word prev', ['Ctrl+Left'], ['Ctrl+Shift+Left']);

  M.Add(cCommand_ToggleOvr, 'toggle insert/overwrite mode', ['Insert'], []);
  M.Add(cCommand_ToggleReadOnly, 'toggle read-only mode', ['Ctrl+R'], []);

  M.Add(cCommand_TextDeleteWordNext, 'delete word next', ['Ctrl+Delete'], []);
  M.Add(cCommand_TextDeleteWordPrev, 'delete word prev', ['Ctrl+Backspace'], []);
  M.Add(cCommand_TextDeleteToLineBegin, 'delete to line start', ['Ctrl+B'], []);
  M.Add(cCommand_TextDeleteToLineEnd, 'delete to line end', ['Ctrl+K'], []);

  M.Add(cCommand_ScrollLineUp, 'scroll line up', ['Ctrl+Up'], []);
  M.Add(cCommand_ScrollLineDown, 'scroll line down', ['Ctrl+Down'], []);

  M.Add(cCommand_ClipboardPaste, 'clipboard: paste', ['Ctrl+V'], []);
  M.Add(cCommand_ClipboardPaste_KeepCaret, 'clipboard: paste, keep caret', [], []);

  M.Add(cCommand_CaretsRemove, 'carets: remove multi-carets', ['Escape'], []);
end;

procedure DoCheckText(const S: string);
begin
  //todo
end;

procedure DoCheckKeyMapping(const M: TATKeyMapping);
var
  i: integer;
begin
  for i:= 0 to M.Count-1 do
    with M[i] do
    begin
      DoCheckText(Keys1[0]);
      DoCheckText(Keys2[0]);
    end;
end;

end.

