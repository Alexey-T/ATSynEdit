unit ATSynEdit_Commands;

{$mode delphi}

interface

const
  cCmdSelKeep  = $10000; //cmd continues selection (new caret pos makes bigger selection)
  cCmdSelReset = $20000; //before command reset selection
  cCmdCaret    = $40000; //command moves caret and makes new undo-group

const
  _base_KeyUp       = 100 or cCmdCaret;
  _base_KeyDown     = 101 or cCmdCaret;
  _base_KeyLeft     = 102 or cCmdCaret;
  _base_KeyRight    = 103 or cCmdCaret;
  _base_KeyHome     = 104 or cCmdCaret;
  _base_KeyEnd      = 105 or cCmdCaret;
  _base_KeyPageUp   = 106 or cCmdCaret;
  _base_KeyPageDown = 107 or cCmdCaret;

  cCommand_KeyUp           = _base_KeyUp or cCmdSelReset;
  cCommand_KeyDown         = _base_KeyDown or cCmdSelReset;
  cCommand_KeyLeft         = _base_KeyLeft; //handles sel
  cCommand_KeyRight        = _base_KeyRight; //handles sel
  cCommand_KeyHome         = _base_KeyHome or cCmdSelReset;
  cCommand_KeyEnd          = _base_KeyEnd or cCmdSelReset;
  cCommand_KeyPageUp       = _base_KeyPageUp or cCmdSelReset;
  cCommand_KeyPageDown     = _base_KeyPageDown or cCmdSelReset;

  cCommand_KeyUp_Sel       = _base_KeyUp or cCmdSelKeep;
  cCommand_KeyDown_Sel     = _base_KeyDown or cCmdSelKeep;
  cCommand_KeyLeft_Sel     = _base_KeyLeft or cCmdSelKeep;
  cCommand_KeyRight_Sel    = _base_KeyRight or cCmdSelKeep;
  cCommand_KeyHome_Sel     = _base_KeyHome or cCmdSelKeep;
  cCommand_KeyEnd_Sel      = _base_KeyEnd or cCmdSelKeep;
  cCommand_KeyPageUp_Sel   = _base_KeyPageUp or cCmdSelKeep;
  cCommand_KeyPageDown_Sel = _base_KeyPageDown or cCmdSelKeep;

  cCommand_TextInsert = 150;
  cCommand_KeyBackspace = 151 or cCmdSelReset;
  cCommand_KeyDelete = 152;
  cCommand_KeyEnter = 153 or cCmdSelReset;
  cCommand_KeyTab = 154;
  cCommand_KeyTabChar = 155;

  cCommand_TextDeleteLine = 170 or cCmdSelReset;
  cCommand_TextDuplicateLine = 171 or cCmdSelReset;
  cCommand_TextDeleteToLineBegin = 172 or cCmdSelReset;
  cCommand_TextDeleteToLineEnd = 173 or cCmdSelReset;
  cCommand_TextDeleteWordNext = 174 or cCmdSelReset;
  cCommand_TextDeleteWordPrev = 175 or cCmdSelReset;
  cCommand_TextDeleteSelection = 180;

  _base_GotoTextBegin = 200 or cCmdCaret;
  _base_GotoTextEnd   = 201 or cCmdCaret;
  _base_GotoWordNext  = 202 or cCmdCaret;
  _base_GotoWordPrev  = 203 or cCmdCaret;

  cCommand_GotoTextBegin = _base_GotoTextBegin or cCmdSelReset;
  cCommand_GotoTextEnd = _base_GotoTextEnd or cCmdSelReset;
  cCommand_GotoWordNext = _base_GotoWordNext or cCmdSelReset;
  cCommand_GotoWordPrev = _base_GotoWordPrev or cCmdSelReset;

  cCommand_GotoTextBegin_Sel = _base_GotoTextBegin or cCmdSelKeep;
  cCommand_GotoTextEnd_Sel = _base_GotoTextEnd or cCmdSelKeep;
  cCommand_GotoWordNext_Sel = _base_GotoWordNext or cCmdSelKeep;
  cCommand_GotoWordPrev_Sel = _base_GotoWordPrev or cCmdSelKeep;

  cCommand_Undo = 235 or cCmdSelReset;
  cCommand_Redo = 236 or cCmdSelReset;

  cCommand_TextIndent = 240;
  cCommand_TextUnindent = 241;

  cCommand_ScrollLineUp = 250;
  cCommand_ScrollLineDown = 251;
  cCommand_ScrollToCaretTop = 252;
  cCommand_ScrollToCaretBottom = 253;
  cCommand_ScrollToCaretLeft = 254;
  cCommand_ScrollToCaretRight = 255;

  cCommand_SelectAll = 260 or cCmdSelReset or cCmdCaret;
  cCommand_SelectWords = 261 or cCmdSelReset or cCmdCaret;
  cCommand_SelectLines = 262 or cCmdSelReset or cCmdCaret;
  cCommand_SelectNone = 263 or cCmdSelReset or cCmdCaret;

  cCommand_ToggleReadOnly = 300;
  cCommand_ToggleOverwrite = 301;

  cCommand_ClipboardPaste = 1000;
  cCommand_ClipboardPaste_Sel = 1001;
  cCommand_ClipboardPaste_KeepCaret = 1002;
  cCommand_ClipboardCopy = 1004;
  cCommand_ClipboardCopyAdd = 1005;
  cCommand_ClipboardCut = 1006;

  cCommand_Cancel = 2001;

implementation

end.

