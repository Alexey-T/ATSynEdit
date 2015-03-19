unit ATSynEdit_Commands;

{$mode delphi}

interface

const
  cCommandSel = $10000;

const
  cCommand_KeyUp = 100;
  cCommand_KeyDown = 101;
  cCommand_KeyLeft = 102;
  cCommand_KeyRight = 103;
  cCommand_KeyHome = 104;
  cCommand_KeyEnd = 105;
  cCommand_KeyPageUp = 106;
  cCommand_KeyPageDown = 107;

  cCommand_KeyUp_Sel = cCommand_KeyUp or cCommandSel;
  cCommand_KeyDown_Sel = cCommand_KeyDown or cCommandSel;
  cCommand_KeyLeft_Sel = cCommand_KeyLeft or cCommandSel;
  cCommand_KeyRight_Sel = cCommand_KeyRight or cCommandSel;
  cCommand_KeyHome_Sel = cCommand_KeyHome or cCommandSel;
  cCommand_KeyEnd_Sel = cCommand_KeyEnd or cCommandSel;
  cCommand_KeyPageUp_Sel = cCommand_KeyPageUp or cCommandSel;
  cCommand_KeyPageDown_Sel = cCommand_KeyPageDown or cCommandSel;

  cCommand_TextInsert = 150;
  cCommand_KeyBackspace = 151;
  cCommand_KeyDelete = 152;
  cCommand_KeyEnter = 153;
  cCommand_KeyTab = 154;
  cCommand_KeyTabChar = 155;

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

  cCommand_GotoTextBegin_Sel = cCommand_GotoTextBegin or cCommandSel;
  cCommand_GotoTextEnd_Sel = cCommand_GotoTextEnd or cCommandSel;
  cCommand_GotoWordNext_Sel = cCommand_GotoWordNext or cCommandSel;
  cCommand_GotoWordPrev_Sel = cCommand_GotoWordPrev or cCommandSel;

  cCommand_ScrollLineUp = 250;
  cCommand_ScrollLineDown = 251;
  cCommand_ScrollToCaretTop = 252;
  cCommand_ScrollToCaretBottom = 253;

  cCommand_SelectAll = 260;
  cCommand_SelectWords = 261;
  cCommand_SelectLines = 262;

  cCommand_ToggleReadOnly = 300;
  cCommand_ToggleOvr = 301;

  cCommand_ClipboardPaste = 1000;
  cCommand_ClipboardPaste_KeepCaret = 1001;

  cCommand_CaretsRemove = 2001;

implementation

end.

