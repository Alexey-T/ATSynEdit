unit ATSynEdit_Commands;

{$mode delphi}

interface

const
  cSelKeep = $10000; //cmd continues selection (new caret pos makes bigger selection)
  cSelReset = $20000; //before command reset selection

const
  _base_KeyUp = 100;
  _base_KeyDown = 101;
  _base_KeyLeft = 102;
  _base_KeyRight = 103;
  _base_KeyHome = 104;
  _base_KeyEnd = 105;
  _base_KeyPageUp = 106;
  _base_KeyPageDown = 107;

  cCommand_KeyUp           = _base_KeyUp or cSelReset;
  cCommand_KeyDown         = _base_KeyDown or cSelReset;
  cCommand_KeyLeft         = _base_KeyLeft or cSelReset;
  cCommand_KeyRight        = _base_KeyRight or cSelReset;
  cCommand_KeyHome         = _base_KeyHome or cSelReset;
  cCommand_KeyEnd          = _base_KeyEnd or cSelReset;
  cCommand_KeyPageUp       = _base_KeyPageUp or cSelReset;
  cCommand_KeyPageDown     = _base_KeyPageDown or cSelReset;

  cCommand_KeyUp_Sel       = _base_KeyUp or cSelKeep;
  cCommand_KeyDown_Sel     = _base_KeyDown or cSelKeep;
  cCommand_KeyLeft_Sel     = _base_KeyLeft or cSelKeep;
  cCommand_KeyRight_Sel    = _base_KeyRight or cSelKeep;
  cCommand_KeyHome_Sel     = _base_KeyHome or cSelKeep;
  cCommand_KeyEnd_Sel      = _base_KeyEnd or cSelKeep;
  cCommand_KeyPageUp_Sel   = _base_KeyPageUp or cSelKeep;
  cCommand_KeyPageDown_Sel = _base_KeyPageDown or cSelKeep;

  cCommand_TextInsert = 150;
  cCommand_KeyBackspace = 151;
  cCommand_KeyDelete = 152;
  cCommand_KeyEnter = 153;
  cCommand_KeyTab = 154;
  cCommand_KeyTabChar = 155;

  cCommand_TextDeleteLine = 170 or cSelReset;
  cCommand_TextDuplicateLine = 171 or cSelReset;
  cCommand_TextDeleteToLineBegin = 172 or cSelReset;
  cCommand_TextDeleteToLineEnd = 173 or cSelReset;
  cCommand_TextDeleteWordNext = 174 or cSelReset;
  cCommand_TextDeleteWordPrev = 175 or cSelReset;

  _base_GotoTextBegin = 200;
  _base_GotoTextEnd = 201;
  _base_GotoWordNext = 202;
  _base_GotoWordPrev = 203;

  cCommand_GotoTextBegin = _base_GotoTextBegin or cSelReset;
  cCommand_GotoTextEnd = _base_GotoTextEnd or cSelReset;
  cCommand_GotoWordNext = _base_GotoWordNext or cSelReset;
  cCommand_GotoWordPrev = _base_GotoWordPrev or cSelReset;

  cCommand_GotoTextBegin_Sel = _base_GotoTextBegin or cSelKeep;
  cCommand_GotoTextEnd_Sel = _base_GotoTextEnd or cSelKeep;
  cCommand_GotoWordNext_Sel = _base_GotoWordNext or cSelKeep;
  cCommand_GotoWordPrev_Sel = _base_GotoWordPrev or cSelKeep;

  cCommand_ScrollLineUp = 250;
  cCommand_ScrollLineDown = 251;
  cCommand_ScrollToCaretTop = 252;
  cCommand_ScrollToCaretBottom = 253;
  cCommand_ScrollToCaretLeft = 254;
  cCommand_ScrollToCaretRight = 255;

  cCommand_SelectAll = 260 or cSelReset;
  cCommand_SelectWords = 261 or cSelReset;
  cCommand_SelectLines = 262 or cSelReset;

  cCommand_ToggleReadOnly = 300;
  cCommand_ToggleOverwrite = 301;

  cCommand_ClipboardPaste = 1000;
  cCommand_ClipboardPaste_Sel = cCommand_ClipboardPaste or cSelKeep;
  cCommand_ClipboardPaste_KeepCaret = 1001;

  cCommand_CaretsRemove = 2001;

implementation

end.

