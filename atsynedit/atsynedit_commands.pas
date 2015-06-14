unit ATSynEdit_Commands;

{$mode objfpc}{$H+}

interface

const
  cCmdSelKeep   = $10000; //cmd continues selection (new caret pos makes bigger selection)
  cCmdSelReset  = $20000; //before command reset selection
  cCmdCaret     = $80000; //cmd moves caret and makes new undo-group

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

  cCommand_ColSelectUp    = 110;
  cCommand_ColSelectDown  = 111;
  cCommand_ColSelectLeft  = 112;
  cCommand_ColSelectRight = 113;

  cCommand_TextInsert = 150;
  cCommand_TextInsertTabChar = 151;
  cCommand_KeyBackspace = 152;
  cCommand_KeyDelete = 153;
  cCommand_KeyEnter = 154;
  cCommand_KeyTab = 155;

  cCommand_TextDeleteSelection = 170;
  cCommand_TextDeleteLine = 171 or cCmdSelReset;
  cCommand_TextDuplicateLine = 172 or cCmdSelReset;
  cCommand_TextDeleteToLineBegin = 173 or cCmdSelReset;
  cCommand_TextDeleteToLineEnd = 174 or cCmdSelReset;
  cCommand_TextDeleteToTextEnd = 175 or cCmdSelReset;
  cCommand_TextDeleteWordNext = 176 or cCmdSelReset;
  cCommand_TextDeleteWordPrev = 177 or cCmdSelReset;

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
  cCommand_SelectNone = 261 or cCmdSelReset or cCmdCaret;
  cCommand_SelectWords = 262 or cCmdSelReset or cCmdCaret;
  cCommand_SelectLines = 263 or cCmdSelReset or cCmdCaret;
  cCommand_SelectInverted = 264 or cCmdCaret;
  cCommand_SelectSplitToLines = 265 or cCmdCaret;
  cCommand_SelectExtendByLine = 266 or cCmdCaret;

  cCommand_MoveSelectionUp = 268 or cCmdCaret;
  cCommand_MoveSelectionDown = 269 or cCmdCaret;
  cCommand_TextInsertEmptyAbove = 270 or cCmdSelReset or cCmdCaret;
  cCommand_TextInsertEmptyBelow = 271 or cCmdSelReset or cCmdCaret;

  cCommand_ToggleOverwrite = 300;
  cCommand_ToggleReadOnly = 301;
  cCommand_ToggleWordWrap = 302;

  cCommand_ClipboardPaste = 1000;
  cCommand_ClipboardPaste_Select = 1001;
  cCommand_ClipboardPaste_KeepCaret = 1002;
  cCommand_ClipboardPaste_Column = 1003 or cCmdSelReset;
  cCommand_ClipboardPaste_ColumnKeepCaret = 1004 or cCmdSelReset;
  cCommand_ClipboardCopy = 1006;
  cCommand_ClipboardCopyAdd = 1007;
  cCommand_ClipboardCut = 1008;

  cCommand_Cancel = 2001;
  cCommand_RepeatTextCommand = 2002;
  cCommand_ZoomIn = 2003;
  cCommand_ZoomOut = 2004;
  cCommand_RecentsPopup = 2005;

  cCommand_CaretsExtendDownLine = 2010;
  cCommand_CaretsExtendDownPage = 2011;
  cCommand_CaretsExtendDownToEnd = 2012;
  cCommand_CaretsExtendUpLine = 2013;
  cCommand_CaretsExtendUpPage = 2014;
  cCommand_CaretsExtendUpToTop = 2015;

implementation

end.

