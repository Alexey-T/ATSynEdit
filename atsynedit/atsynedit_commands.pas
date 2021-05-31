{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Commands;

{$mode objfpc}{$H+}

interface

const
  cCmdFlag_KeepSel   = $10000; //cmd continues selection (new caret pos makes bigger selection)
  cCmdFlag_ResetSel  = $20000; //before command reset selection
  cCmdFlag_Caret     = $80000; //cmd moves caret and makes new undo-group

const
  _base_KeyUp       = 100 or cCmdFlag_Caret;
  _base_KeyDown     = 101 or cCmdFlag_Caret;
  _base_KeyLeft     = 102 or cCmdFlag_Caret;
  _base_KeyRight    = 103 or cCmdFlag_Caret;
  _base_KeyHome     = 104 or cCmdFlag_Caret;
  _base_KeyEnd      = 105 or cCmdFlag_Caret;
  _base_KeyPageUp   = 106 or cCmdFlag_Caret;
  _base_KeyPageDown = 107 or cCmdFlag_Caret;

  cCommand_KeyUp           = _base_KeyUp or cCmdFlag_ResetSel;
  cCommand_KeyDown         = _base_KeyDown or cCmdFlag_ResetSel;
  cCommand_KeyLeft         = _base_KeyLeft; //handles sel
  cCommand_KeyRight        = _base_KeyRight; //handles sel
  cCommand_KeyHome         = _base_KeyHome or cCmdFlag_ResetSel;
  cCommand_KeyEnd          = _base_KeyEnd or cCmdFlag_ResetSel;
  cCommand_KeyPageUp       = _base_KeyPageUp or cCmdFlag_ResetSel;
  cCommand_KeyPageDown     = _base_KeyPageDown or cCmdFlag_ResetSel;

  cCommand_KeyUp_Sel       = _base_KeyUp or cCmdFlag_KeepSel;
  cCommand_KeyDown_Sel     = _base_KeyDown or cCmdFlag_KeepSel;
  cCommand_KeyLeft_Sel     = _base_KeyLeft or cCmdFlag_KeepSel;
  cCommand_KeyRight_Sel    = _base_KeyRight or cCmdFlag_KeepSel;
  cCommand_KeyHome_Sel     = _base_KeyHome or cCmdFlag_KeepSel;
  cCommand_KeyEnd_Sel      = _base_KeyEnd or cCmdFlag_KeepSel;
  cCommand_KeyPageUp_Sel   = _base_KeyPageUp or cCmdFlag_KeepSel;
  cCommand_KeyPageDown_Sel = _base_KeyPageDown or cCmdFlag_KeepSel;

  cCommand_ColSelectUp    = 110;
  cCommand_ColSelectDown  = 111;
  cCommand_ColSelectLeft  = 112;
  cCommand_ColSelectRight = 113;
  cCommand_ColSelectToLineBegin = 114;
  cCommand_ColSelectToLineEnd = 115;
  cCommand_ColSelectPageUp = 116;
  cCommand_ColSelectPageDown = 117;

  cCommand_ColSelectWithoutKey_On = 130;
  cCommand_ColSelectWithoutKey_Off = 131;
  cCommand_ColSelectWithoutKey_Toggle = 132;

  cCommand_RemoveFirstCaret = 135;
  cCommand_RemoveLastCaret = 136;

  cCommand_TextInsert = 150;
  cCommand_TextInsertTabChar = 151;
  cCommand_KeyBackspace = 152;
  cCommand_KeyDelete = 153;
  cCommand_KeyEnter = 154;
  cCommand_KeyTab = 155;
  cCommand_ForceFinalEndOfLine = 160;

  cCommand_TextDeleteSelection = 170;
  cCommand_TextDeleteLine = 171;
  cCommand_TextDuplicateLine = 172;
  cCommand_TextDeleteToLineBegin = 173 or cCmdFlag_ResetSel;
  cCommand_TextDeleteToLineEnd = 174 or cCmdFlag_ResetSel;
  cCommand_TextDeleteToTextEnd = 175 or cCmdFlag_ResetSel;
  cCommand_TextDeleteWordNext = 176 or cCmdFlag_ResetSel;
  cCommand_TextDeleteWordPrev = 177 or cCmdFlag_ResetSel;
  cCommand_TextDeleteToTextBegin = 178 or cCmdFlag_ResetSel;
  cCommand_TextDeleteWordEntire = 179 or cCmdFlag_ResetSel;

  _base_GotoTextBegin = 200 or cCmdFlag_Caret;
  _base_GotoTextEnd   = 201 or cCmdFlag_Caret;
  _base_GotoWordNext  = 202 or cCmdFlag_Caret;
  _base_GotoWordPrev  = 203 or cCmdFlag_Caret;
  _base_GotoWordEnd   = 204 or cCmdFlag_Caret;
  _base_GotoWordNext_Simple = 205 or cCmdFlag_Caret;
  _base_GotoWordPrev_Simple = 206 or cCmdFlag_Caret;

  cCommand_GotoTextBegin = _base_GotoTextBegin or cCmdFlag_ResetSel;
  cCommand_GotoTextEnd = _base_GotoTextEnd or cCmdFlag_ResetSel;
  cCommand_GotoWordNext = _base_GotoWordNext or cCmdFlag_ResetSel;
  cCommand_GotoWordPrev = _base_GotoWordPrev or cCmdFlag_ResetSel;
  cCommand_GotoWordEnd = _base_GotoWordEnd or cCmdFlag_ResetSel;
  cCommand_GotoWordNext_Simple = _base_GotoWordNext_Simple or cCmdFlag_ResetSel;
  cCommand_GotoWordPrev_Simple = _base_GotoWordPrev_Simple or cCmdFlag_ResetSel;

  cCommand_GotoTextBegin_Sel = _base_GotoTextBegin or cCmdFlag_KeepSel;
  cCommand_GotoTextEnd_Sel = _base_GotoTextEnd or cCmdFlag_KeepSel;
  cCommand_GotoWordNext_Sel = _base_GotoWordNext or cCmdFlag_KeepSel;
  cCommand_GotoWordPrev_Sel = _base_GotoWordPrev or cCmdFlag_KeepSel;
  cCommand_GotoWordEnd_Sel = _base_GotoWordEnd or cCmdFlag_KeepSel;
  cCommand_GotoWordNext_Simple_Sel = _base_GotoWordNext_Simple or cCmdFlag_KeepSel;
  cCommand_GotoWordPrev_Simple_Sel = _base_GotoWordPrev_Simple or cCmdFlag_KeepSel;

  _base_GotoLineAbsBegin = 210 or cCmdFlag_Caret;
  _base_GotoLineAbsEnd   = 211 or cCmdFlag_Caret;

  cCommand_GotoLineAbsBegin     = _base_GotoLineAbsBegin or cCmdFlag_ResetSel;
  cCommand_GotoLineAbsBegin_Sel = _base_GotoLineAbsBegin or cCmdFlag_KeepSel;
  cCommand_GotoLineAbsEnd       = _base_GotoLineAbsEnd or cCmdFlag_ResetSel;
  cCommand_GotoLineAbsEnd_Sel   = _base_GotoLineAbsEnd or cCmdFlag_KeepSel;

  cCommand_GotoScreenTop = 215 or cCmdFlag_Caret;
  cCommand_GotoScreenBottom = 216 or cCmdFlag_Caret;
  cCommand_GotoScreenCenter = 217 or cCmdFlag_Caret;

  cCommand_Undo = 235 or cCmdFlag_ResetSel;
  cCommand_Redo = 236 or cCmdFlag_ResetSel;

  cCommand_TextIndent = 240;
  cCommand_TextUnindent = 241;

  cCommand_ScrollPageUp = 248;
  cCommand_ScrollPageDown = 249;
  cCommand_ScrollLineUp = 250;
  cCommand_ScrollLineDown = 251;
  cCommand_ScrollToCaretTop = 252;
  cCommand_ScrollToCaretBottom = 253;
  cCommand_ScrollToCaretLeft = 254;
  cCommand_ScrollToCaretRight = 255;
  cCommand_ScrollColumnLeft = 256;
  cCommand_ScrollColumnRight = 257;
  cCommand_ScrollToBegin = 258;
  cCommand_ScrollToEnd = 259;

  cCommand_SelectAll = 260 or cCmdFlag_ResetSel or cCmdFlag_Caret;
  cCommand_SelectNone = 261 or cCmdFlag_ResetSel or cCmdFlag_Caret;
  cCommand_SelectWords = 262 or cCmdFlag_ResetSel or cCmdFlag_Caret;
  cCommand_SelectLines = 263 or cCmdFlag_ResetSel or cCmdFlag_Caret;
  cCommand_SelectInverted = 264 or cCmdFlag_Caret;
  cCommand_SelectSplitToLines = 265 or cCmdFlag_Caret;
  cCommand_SelectExtendByLine = 266 or cCmdFlag_Caret;

  cCommand_MoveSelectionUp = 268; // or cCmdFlag_Caret;
  cCommand_MoveSelectionDown = 269; // or cCmdFlag_Caret;
  cCommand_TextInsertEmptyAbove = 270 or cCmdFlag_ResetSel or cCmdFlag_Caret;
  cCommand_TextInsertEmptyBelow = 271 or cCmdFlag_ResetSel or cCmdFlag_Caret;

  cCommand_ToggleOverwrite = 300;
  cCommand_ToggleReadOnly = 301;
  cCommand_ToggleWordWrap = 302;

  cCommand_ToggleUnprinted = 303;
  cCommand_ToggleUnprintedSpaces = 304;
  cCommand_ToggleUnprintedEnds = 305;
  cCommand_ToggleUnprintedEndDetails = 306;
  cCommand_ToggleUnprintedSpacesTrailing = 320;

  cCommand_ToggleLineNums = 307;
  cCommand_ToggleFolding = 308;
  cCommand_ToggleRuler = 309;
  cCommand_ToggleMinimap = 310;
  cCommand_ToggleMicromap = 311;
  cCommand_ToggleWordWrapAlt = 312;
  //value 320 busy

  cCommand_Sort_Asc        = 330;
  cCommand_Sort_AscNoCase  = 331;
  cCommand_Sort_Desc       = 332;
  cCommand_Sort_DescNoCase = 333;

  cCommand_DeleteAllBlanks      = 340;
  cCommand_DeleteAdjacentBlanks = 341;
  cCommand_DeleteAdjacentDups   = 342;
  cCommand_DeleteAllDups        = 343;
  cCommand_DeleteAllDupsKeepBlanks = 344;
  cCommand_ReverseLines = 350;
  cCommand_ShuffleLines = 351;

  //first Paste command
  cCommand_ClipboardPaste_Begin = 1000;
  cCommand_ClipboardPaste = 1000;
  cCommand_ClipboardPaste_Select = 1001;
  cCommand_ClipboardPaste_KeepCaret = 1002;
  cCommand_ClipboardPaste_Column = 1003 or cCmdFlag_ResetSel;
  cCommand_ClipboardPaste_ColumnKeepCaret = 1004 or cCmdFlag_ResetSel;
  cCommand_ClipboardCopy = 1006;
  cCommand_ClipboardCopyAdd = 1007;
  cCommand_ClipboardCut = 1008;

  //use PrimarySelection (has meaning in Linux)
  cCommand_ClipboardAltPaste = 1010;
  cCommand_ClipboardAltPaste_Select = 1011;
  cCommand_ClipboardAltPaste_KeepCaret = 1012;
  cCommand_ClipboardAltPaste_Column = 1013 or cCmdFlag_ResetSel;
  cCommand_ClipboardAltPaste_ColumnKeepCaret = 1014 or cCmdFlag_ResetSel;
  //use SecondarySelection (has meaning in Linux)
  cCommand_ClipboardAltAltPaste = 1015;
  //last Paste command
  cCommand_ClipboardPaste_End = 1015;

  cCommand_TextCaseLower = 1020;
  cCommand_TextCaseUpper = 1021;
  cCommand_TextCaseTitle = 1022;
  cCommand_TextCaseInvert = 1023;
  cCommand_TextCaseSentence = 1024;

  cCommand_TextTrimSpacesLeft = 1026;
  cCommand_TextTrimSpacesRight = 1027;
  cCommand_TextTrimSpacesAll = 1028;

  cCommand_UnfoldAll = 1029;
  cCommand_FoldAll = 1030;
  cCommand_FoldLevel1 = 1031;
  cCommand_FoldLevel2 = 1032;
  cCommand_FoldLevel3 = 1033;
  cCommand_FoldLevel4 = 1034;
  cCommand_FoldLevel5 = 1035;
  cCommand_FoldLevel6 = 1036;
  cCommand_FoldLevel7 = 1037;
  cCommand_FoldLevel8 = 1038;
  cCommand_FoldLevel9 = 1039;

  cCommand_FoldingFoldAtCurLine = 1040;
  cCommand_FoldingUnfoldAtCurLine = 1041;
  cCommand_FoldingToggleAtCurLine = 1042;
  cCommand_FoldingFoldSelection = 1043;

  cCommand_Cancel = 2001;
  cCommand_RepeatTextCommand = 2002;
  cCommand_ZoomIn = 2003;
  cCommand_ZoomOut = 2004;
  cCommand_ComboboxRecentsMenu = 2005;
  cCommand_ZoomReset = 2007;

  cCommand_CaretsExtendDownLine = 2010;
  cCommand_CaretsExtendDownPage = 2011;
  cCommand_CaretsExtendDownToEnd = 2012;
  cCommand_CaretsExtendUpLine = 2013;
  cCommand_CaretsExtendUpPage = 2014;
  cCommand_CaretsExtendUpToTop = 2015;

  // custom commands must start later to not conflict
  // with built-in commands in the DoCommand overridden method
  cCommand_FirstUserCode = 4000;

var
  //must be set in application
  cCommand_GotoDefinition: integer = 0;

//all sequental Undo-items which have CommandCode with this value,
//will be undone in single step
function IsCommandToUndoInOneStep(AValue: integer): boolean;

function IsCommandForDelayedParsing(AValue: integer): boolean;

implementation

function IsCommandToUndoInOneStep(AValue: integer): boolean;
begin
  case AValue and not cCmdFlag_ResetSel of
    cCommand_MoveSelectionUp,
    cCommand_MoveSelectionDown,
    cCommand_ClipboardPaste_Begin..
    cCommand_ClipboardPaste_End:
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsCommandForDelayedParsing(AValue: integer): boolean;
//to solve CudaText issue #3403:
//holding hotkey for 'move lines up/down' breaks syntax highlight
begin
  case AValue of
    cCommand_MoveSelectionUp,
    cCommand_MoveSelectionDown:
      Result:= true;
    else
      Result:= false;
  end;
end;

end.

