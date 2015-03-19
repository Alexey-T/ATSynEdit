unit ATSynEdit_Commands;

{$mode delphi}

interface

const
  cCommand_KeyUp = 100;
  cCommand_KeyDown = 101;
  cCommand_KeyLeft = 102;
  cCommand_KeyRight = 103;
  cCommand_KeyHome = 104;
  cCommand_KeyEnd = 105;

  cCommand_TextInsert = 150;
  cCommand_KeyBackspace = 151;
  cCommand_KeyDelete = 152;
  cCommand_KeyEnter = 153;
  cCommand_KeyTab = 154;
  cCommand_KeyTabChar = 155;
  cCommand_KeyPageUp = 160;
  cCommand_KeyPageDown = 161;

  cCommand_TextDeleteCurLine = 170;
  cCommand_TextDuplicateCurLine = 171;
  cCommand_TextDeleteToLineBegin = 172;
  cCommand_TextDeleteToLineEnd = 173;
  cCommand_TextDeleteWordNext = 174;
  cCommand_TextDeleteWordPrev = 175;

  cCommand_GotoTextBegin = 200;
  cCommand_GotoTextEnd = 201;
  cCommand_GotoWordNext = 202;
  cCommand_GotoWordPrev = 203;

  cCommand_ScrollLineUp = 250;
  cCommand_ScrollLineDown = 251;
  cCommand_SelectAll = 252;
  cCommand_SelectWords = 253;

  cCommand_ToggleReadOnly = 260;
  cCommand_ToggleOvr = 261;

  cCommand_ClipboardPaste = 1000;
  cCommand_ClipboardPaste_KeepCaret = 1001;

  cCommand_CaretsRemove = 2001;

implementation

end.

