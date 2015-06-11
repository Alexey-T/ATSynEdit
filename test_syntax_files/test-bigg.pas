{
SynWrite main UI form.
}
{$Q-} //Disable int-checks to avoid integer exception in TabCtrl_GetXRect

unit unMain;

interface

uses
  Windows, SysUtils, Messages, Controls, StdCtrls, ComCtrls, Classes, Menus, Forms,
  ActnList, Dialogs, ImgList, ExtCtrls, Graphics, IniFiles,
  TntForms, TntClasses,
  Gauges,

  {$ifdef SPELL}
  ad3LiveBase,
  ad3Configuration,
  {$endif}

  unFrame,
  unSearch,
  unSearchInList,
  unNumConv,
  unClip,
  unClips,
  unMap,
  unProj,
  unProgress,
  unSR,
  unProcLines,
  unTabSw,

  TB2Item, TB2Dock, TB2Toolbar,
  TB2MDI, TB2ExtItems,

  SpTBXItem, SpTbxMDIMRU, SpTBXDkPanels, SpTBXSkins,
  SpTBXControls, SpTBXTabs, SpTBXEditors,

  ecActns, ecPrint, ecSyntMemo, ecKeyMap, ecPropManager, ecSyntAnal,
  ecSyntTree, ecStrUtils, ecPopupCtrl, ecUnicode,
  ATSyntMemo, //after ecSyntMemo
  ATTabs,
  ATGroups,

  TntDialogs, TntStdCtrls, TntComCtrls,
  DKLang,
  PngImageList,

  unProc,
  unProcHelp,
  unProcEditor,
  ATSynPlugins,
  ATxSProc,
  ecMacroRec,
  ecExtHighlight,
  ecSpell,
  PythonEngine,
  PythonGUIInputOutput;

const
  cImageIndexRoOn = 0;
  cImageIndexRoOff = 1;    
  cImageIndexWrapOn = 2;
  cImageIndexWrapOff = 3;
  cImageIndexSelNormal = 4;
  cImageIndexSelColumn = 5;
  cImageIndexSelLine = 6;
  cImageIndexTabstopMode = 7;

  cImageIndexSaveIcon = 2;
  cImageIndexSaveIconPale = 3;

  cGutterBandSizeFold = 13;
  cGutterBandSizeBm = 16;
  cGutterBandSizeNumSpace = 10; //addition to line-numbers band width
  opMruForPlugin = false; //use recent list for Lister-plugin
  cMaxFilesInFolder = 50; //if more folder in dir, show confirmation for "Syn.exe dir"
  cTabColors = 10; //number of user-defined tab colors
  cFixedWindowItems = 5; //number of fixed items in Window menu
  cMaxTreeLen = 250; //"find in files" result tree: max node length
  cMaxLinesInstantMinimap = 50*1000; //max lines for which OnScroll will update minimap instantly
  cDefaultCursor: array[boolean] of TCursor = (crHourGlass, crDefault);
  SynDefaultSyn = '(default).synw-session';
  cIconsDefault = 'Fugue 24x24';
  cMaxSectionsInInf = 120;
  cMaxLexerLinksInInf = 20;
  cMinProgressFilesize = 120*1024;
  cColorIdxMin = 4; //index in "Recent colors" after "Clear list" and separator

const
  cPyConsoleMaxCount = 1000;
  cPyConsolePrompt = '>>> ';
  cPyConsoleInit = 'print("Python", sys.version)';
  cPyConsoleClear = '-';
  cPyConsolePrint = '=';
  cPyPrefix = 'py:';
  cBinaryPrefix = 'binary:';
  cPyNotInited = 'Python engine not inited';
  cPyTrue = 'True';
  cPyFalse = 'False';
  cPyNone = 'None';
  cPyCommandBase = 5000;
  cPyPluginManager = 'syn_plugin_manager';

type
  TSynDataSubdirId = (
    cSynDataAutocomp,
    cSynDataClips,
    cSynDataColors,
    cSynDataConv,
    cSynDataIcons,
    cSynDataNewDoc,
    cSynDataOutPresets,
    cSynDataSkins,
    cSynDataSnippets,
    cSynDataWebSearch
    );
const
  cSynDataSubdirNames: array[TSynDataSubdirId] of string = (
    'autocomplete',
    'clips',
    'colors',
    'conv',
    'icons',
    'newdoc',
    'outpresets',
    'skins',
    'snippets',
    'websearch'
    );

type
  TSynEditorHistoryItem = (
    cSynHistoryCaret,
    cSynHistoryEnc,
    cSynHistoryWrap,
    cSynHistoryBkmk,
    cSynHistoryFolding,
    cSynHistoryForTemp
    );
  TSynEditorHistoryItems = set of TSynEditorHistoryItem;

const
  cFramePropLexer    = 'lex';
  cFramePropWrap     = 'wrap';
  cFramePropEnc      = 'enc';
  cFramePropSplit    = 'split';
  cFramePropBk       = 'bk';
  cFramePropColor    = 'tabc';
  cFramePropZoom     = 'zoom';
  //these are for both master/slave:
  cFramePropPos      = 'pos';
  cFramePropSel      = 'sel';
  cFramePropFold     = 'fold';

type
  TSynPyEvent = (
    cSynEventOnOpen,
    cSynEventOnSaveAfter,
    cSynEventOnSaveBefore,
    cSynEventOnKey,
    cSynEventOnChange,
    cSynEventOnChangeSlow,
    cSynEventOnSelect,
    cSynEventOnCaretMove,
    cSynEventOnNumber,
    cSynEventOnState,
    cSynEventOnFocus,
    cSynEventOnLexer,
    cSynEventOnComplete,
    cSynEventOnFuncHint,
    cSynEventOnGotoDef,
    cSynEventOnConsole,
    cSynEventOnPanelLog,
    cSynEventOnCompare
    );
  TSynPyEvents = set of TSynPyEvent;

const
  cSynPyEvent: array[TSynPyEvent] of string = (
    'on_open',
    'on_save',
    'on_save_pre',
    'on_key',
    'on_change',
    'on_change_slow',
    'on_select',
    'on_caret_move',
    'on_num',
    'on_state',
    'on_focus',
    'on_lexer',
    'on_complete',
    'on_func_hint',
    'on_goto_def',
    'on_console',
    'on_panel_log',
    'on_compare'
    );

type
  TSynGroupId = (
    cSynGroupCurrent,
    cSynGroupOpposite,
    cSynGroup1,
    cSynGroup2,
    cSynGroup3,
    cSynGroup4,
    cSynGroup5,
    cSynGroup6
    );

type
  TSynBinaryAct = (
    cBinaryDontOpen,
    cBinaryAlwaysOpen,
    cBinaryPrompt
    );

type
  TSynQuickSearchType = (
    cQsNext,
    cQsPrev,
    cQsAgain
    );

type
  TSynAddonType = (
    cAddonTypeNone,
    cAddonTypeBinPlugin,
    cAddonTypePyPlugin,
    cAddonTypeData,
    cAddonTypeLexer,
    cAddonTypeRoot
    );
const
  cSynAddonType: array[TSynAddonType] of string =
    ('', 'plugin', 'py-plugin', 'template', 'lexer', 'root-addon');

type
  TSynEscMode = (
    cEscNothing,
    cEscCloseApp,
    cEscCloseTab,
    cEscCloseTabOrApp,
    cEscMinimizeApp
    );

  TSynBackup = (
    cBakNone,
    cBakAppdata,
    cBakSameDir
    );

  TSynLastDirMode = (
    cLastDirCurrentFile,
    cLastDirRemember,
    cLastDirCustom
    );

  TSynReloadMode = (
    cReloadNone,
    cReloadAuto,
    cReloadAsk
    );

  TSynAutoSaveUnnamed = (
    cAutoSaveIgnore,
    cAutoSavePromptFN,
    cAutoSaveSaveToDir
    );

  TSynRecentColors = (
    cRecColorsAutoHide,
    cRecColorsShow,
    cRecColorsHide
    );

  TSynLineCmd = (
    cLineCmdSortAsc,
    cLineCmdSortDesc,
    cLineCmdSortDialog,
    cLineCmdDedupAll,
    cLineCmdDedupAllAndOrig,
    cLineCmdDedupAdjacent,
    cLineCmdUntab,
    cLineCmdSpacesToTabs,
    cLineCmdSpacesToTabsLead,
    cLineCmdAlignWithSep,
    cLineCmdRemoveBlanks,
    cLineCmdRemoveDupBlanks,
    cLineCmdTrimLead,
    cLineCmdTrimTrail,
    cLineCmdTrimAll,
    cLineCmdRemoveDupSpaces,
    cLineCmdReverse,
    cLineCmdShuffle,
    cLineCmdExtractDupsCase,
    cLineCmdExtractDupsNoCase,
    cLineCmdExtractUniq,
    cLineCmdIndent,
    cLineCmdUnIndent
    );

  TSynCopyNameCmd = (
    cCmdCopyFileName,
    cCmdCopyFullName,
    cCmdCopyFilePath
    );

  TPluginList_Command = array[0..150] of record
    SCaption: Widestring;
    SFilename: string;
    SLexers: string;
    SCmd: string;
  end;

  TPluginList_Event = array[0..70] of record
    SFilename: string;
    SLexers: string;
    SKeycodes: string;
    Events: TSynPyEvents;
  end;

  TPluginList_Findid = array[0..30] of record
    SFilename: string;
    SLexers: string;
  end;

  TPluginList_Acp = TPluginList_FindId;

  TPluginList_Panel = array[0..10] of record
    SCaption: string;
    SFileName: string;
    SState: Widestring;
    FDll: THandle;
    FWindow: THandle;
    FForm: Pointer;
    FSynInit: TSynInit;
    FSynOpenForm: TSynOpenForm;
    FSynCloseForm: TSynCloseForm;
    FSynAction: TSynAction;
  end;

type
  TSynLogPanelProps = record
    RegexStr: string;
    RegexIdLine,
    RegexIdCol,
    RegexIdName: Integer;
    DefFilename: Widestring;
    ZeroBase: boolean;
    Encoding: TSynToolOutputEnc;
  end;

  TSynLogPanelKind = (
    cSynLogOutput,
    cSynLogValidate
    );

  TSynFindInfo = class
    FN, Str: Widestring;
    LineNum, ColNum, Len: integer;
    constructor Create;
  end;

  TSynFindCount = class
    Matches: integer;
  end;

type
  TSynPanelType = (plTypeTree, plTypeClip, plTypeOut);
  TSynFileSort = (sortNone, sortDate, sortDateDesc);
  TSynSelState = (selNone, selSmall, selStream, selColumn, selCarets);
  TSynGotoTree = (tgoNext, tgoPrev, tgoParent, tgoNextBro, tgoPrevBro);
  TSynGotoMode = (goLine, goPrevBk, goNextBk, goNumBk);
  TSynTabOut = (tbOutput, tbFindRes, tbBookmarks, tbValidate, tbPluginsLog, tbConsole);
  TSynTabRight = (tbClipbd, tbMinimap, tbTextClips);
  TSynTabLeft = (tbTree, tbProj, tbTabs, tbPlugin1, tbPlugin2, tbPlugin3, tbPlugin4, tbPlugin5);
  TSynEncOverride = (cp_sr_Def, cp_sr_OEM, cp_sr_UTF8, cp_sr_UTF16);
  TSynUserToolbarId = (synToolbar1, synToolbar2, synToolbar3);
  TSynDock = (sdockTop, sdockLeft, sdockRight, sdockBottom);

type
  TSynFindInFilesError = (
    cFindFilesOk,
    cFindFilesNoFiles,
    cFindFilesNoLines
    );

type
  TSynFindInFilesData = record
    ATextSearch,
    ATextReplace: Widestring;
    ATextCase,
    ATextWords,
    ATextRegex,
    ATextSpec: boolean;
    ADir: Widestring;
    AFnOnly, AToTab, AOutAppend, ACloseAfter: boolean;
    ASortMode: TSynFileSort;
    InOEM, InUTF8, InUTF16: boolean;
  end;

const
  cColorsNum = 68;
type
  TSynColors = array[0..cColorsNum-1] of TColor;

var
  SynCommandlineSessionFN: string;
  SynCommandlineProjectFN: string;

type
  TfmMain = class(TTntForm)
    ActionList: TActionList;
    TBXDockTop: TSpTbxDock;
    tbFile: TSpTbxToolbar;
    acOpen: TAction;
    acSave: TAction;
    acSaveAs: TAction;
    TBXItemBarSaveAs: TSpTBXItem;
    SD: TTntSaveDialog;
    OD: TTntOpenDialog;
    SyntaxManager: TSyntaxManager;
    PropsManager: TPropsManager;
    ecSyntPrinter: TecSyntPrinter;
    PopupEditor: TSpTbxPopupMenu;
    ImgListGutter: TImageList;
    TBXItemCtxCopy: TSpTbxItem;
    TBXSubmenuBarOpen: TSpTBXSubmenuItem;
    SyntKeyMapping: TSyntKeyMapping;
    ecCopy: TecCopy;
    ecCut: TecCut;
    ecPaste: TecPaste;
    ecSelectAll: TecSelectAll;
    ecUndo: TecUndo;
    ecRedo: TecRedo;
    ecIndent: TecIndent;
    ecUnindent: TecUnindent;
    ecLowerCase: TecLowerCase;
    ecUpperCase: TecUpperCase;
    ecToggleCase: TecToggleCase;
    TBXItemCtxCut: TSpTbxItem;
    TBXItemCtxPaste: TSpTbxItem;
    TBXItemCtxSelectAll: TSpTbxItem;
    acSetup: TAction;
    TBXDockLeft: TSpTbxMultiDock;
    TBXSubmenuBarSave: TSpTBXSubmenuItem;
    acExportRTF: TecExportRTFAction;
    acExportHTML: TecExportHTMLAction;
    ecCommentLines: TecCommentLines;
    ecUnCommentLines: TecUnCommentLines;
    ecSortAscending: TAction;
    ecSortDescending: TAction;
    acSetupLexerLib: TAction;
    TimerTick: TTimer;
    PopupStatusEnc: TSpTBXPopupMenu;
    PopupStatusLineEnds: TSpTBXPopupMenu;
    ecReadOnly: TAction;
    tbEdit: TSpTbxToolbar;
    TBXItemBarCopy: TSpTBXItem;
    TBXItemBarCut: TSpTBXItem;
    TBXItemBarPaste: TSpTBXItem;
    TBXItemBarSelAll: TSpTBXItem;
    TBXItemBarUndo: TSpTBXItem;
    TBXItemBarRedo: TSpTBXItem;
    ecClear: TecClear;
    TBXItemBarDelete: TSpTBXItem;
    tbView: TSpTbxToolbar;
    TBXSubmenuBarNPrint: TSpTBXSubmenuItem;
    TBXItemBarComm: TSpTBXItem;
    TBXItemBarUncom: TSpTBXItem;
    TBXSeparatorItem2: TSpTbxSeparatorItem;
    TBXItemBarIndent: TSpTBXItem;
    TBXItemBarUnindent: TSpTBXItem;
    TBXSeparatorItem3: TSpTbxSeparatorItem;
    TBXSeparatorItem4: TSpTbxSeparatorItem;
    TBXSubmenuCase: TSpTbxSubmenuItem;
    TBXItemBarCaseLower: TSpTBXItem;
    TBXItemBarCaseUpper: TSpTBXItem;
    ImgListTree: TImageList;
    plTree: TSpTbxDockablePanel;
    TBXDockBottom: TSpTbxMultiDock;
    TBXDockRight: TSpTbxMultiDock;
    Tree: TSyntaxTreeView;
    ecTitleCase: TAction;
    TBXItemBarCaseTitle: TSpTBXItem;
    ecShowTree: TAction;
    TBXItemBarPTree: TSpTBXItem;
    ecPrintAction: TecPrintAction;
    ecPrinterSetup: TAction;
    PrinterSetupDialog: TPrinterSetupDialog;
    TBXSubmenuBarPrint: TSpTBXSubmenuItem;
    TBXItemBarPrintSet: TSpTBXItem;
    TBXItemBarPreview: TSpTBXItem;
    TBXItemBarPageSet: TSpTBXItem;
    TBXItemEndWin: TSpTbxItem;
    TBXItemEndUn: TSpTbxItem;
    TBXItemEndMac: TSpTbxItem;
    SyntStyles: TSyntStyles;
    ecACP: TAutoCompletePopup;
    ParamCompletion: TParamCompletion;
    TemplatePopup: TTemplatePopup;
    ecFind: TAction;
    TBXSubmenuBarSetup: TSpTBXSubmenuItem;
    TBXSeparatorItem1: TSpTbxSeparatorItem;
    TBXItemBarLexer: TSpTBXItem;
    TBXItemBarLexerLib: TSpTBXItem;
    PopupLexers: TSpTBXPopupMenu;
    ecWrap: TAction;
    ecLineNums: TAction;
    ecFolding: TAction;
    ecNonPrint: TAction;
    acReread: TAction;
    TBXSubmenuBarFind: TSpTBXSubmenuItem;
    TBXItemBarMarks: TSpTBXItem;
    acNewTab: TAction;
    TBXSubmenuBarNew: TSpTBXSubmenuItem;
    TBXItemBarCaseInvert: TSpTBXItem;
    TBXSeparatorItem10: TSpTbxSeparatorItem;
    Status: TSpTbxStatusBar;
    TBXSubmenuSort: TSpTBXSubmenuItem;
    TBXItemBarSortAsc: TSpTBXItem;
    TBXItemBarSortDesc: TSpTBXItem;
    ecCharPopup: TecSelCharPopup;
    TBXItemBarWordPrev: TSpTBXItem;
    TBXSeparatorItem9: TSpTbxSeparatorItem;
    TBXItemBarWordNext: TSpTBXItem;
    TBXSeparatorItem11: TSpTbxSeparatorItem;
    TBXItemBarFNext: TSpTBXItem;
    TBXItemBarFPrev: TSpTBXItem;
    TimerRedraw: TTimer;
    TBXItemCtxDel: TSpTbxItem;
    TBXSeparatorItem14: TSpTbxSeparatorItem;
    TBXItemCtxRedo: TSpTbxItem;
    TBXItemCtxUndo: TSpTbxItem;
    Menu: TSpTbxDock;
    tbMenu: TSpTbxToolbar;
    TBXSubmenuEdit: TSpTBXSubmenuItem;
    TBXSubmenuFile: TSpTBXSubmenuItem;
    TBXItemFOpen: TSpTbxItem;
    acNewWindow: TAction;
    TBXItemFExit: TSpTbxItem;
    TBXItemFSaveAs: TSpTbxItem;
    TBXItemFSave: TSpTbxItem;
    TBXItemFReopen: TSpTbxItem;
    TBXItemFNewWin: TSpTbxItem;
    TBXItemFNew: TSpTbxItem;
    TBXSeparatorItem15: TSpTbxSeparatorItem;
    TBXSubmenuItemFNew: TSpTBXSubmenuItem;
    TBXItemFPreview: TSpTbxItem;
    TBXItemFPrint: TSpTbxItem;
    TBXItemFPageSetup: TSpTbxItem;
    TBXItemFPrinterSetup: TSpTbxItem;
    TBXSubmenuItemExport: TSpTBXSubmenuItem;
    TBXItemFExpRtf: TSpTbxItem;
    TBXItemFExpHtml: TSpTbxItem;
    PanelBg: TPanel;
    TemplateEditor: TSyntaxMemo;
    TBXSubmenuBookmarks: TSpTBXSubmenuItem;
    TBXSubmenuItemBkGoto: TSpTbxSubmenuItem;
    TBXItemB0: TSpTbxItem;
    TBXItemB1: TSpTbxItem;
    TBXItemB2: TSpTbxItem;
    TBXItemB3: TSpTbxItem;
    TBXItemB4: TSpTbxItem;
    TBXItemB5: TSpTbxItem;
    TBXItemB6: TSpTbxItem;
    TBXItemB7: TSpTbxItem;
    TBXItemB8: TSpTbxItem;
    TBXItemB9: TSpTbxItem;
    TBXItemEUndo: TSpTbxItem;
    TBXItemERedo: TSpTbxItem;
    TBXSeparatorItem6: TSpTbxSeparatorItem;
    TBXItemECut: TSpTbxItem;
    TBXItemECopy: TSpTbxItem;
    TBXItemEPaste: TSpTbxItem;
    TBXItemESelectAll: TSpTbxItem;
    TBXItemEDelete: TSpTbxItem;
    TBXSeparatorItem17: TSpTbxSeparatorItem;
    TBXSubmenuItemBkSet: TSpTbxSubmenuItem;
    TBXItemG0: TSpTbxItem;
    TBXItemG1: TSpTbxItem;
    TBXItemG2: TSpTbxItem;
    TBXItemG3: TSpTbxItem;
    TBXItemG4: TSpTbxItem;
    TBXItemG5: TSpTbxItem;
    TBXItemG6: TSpTbxItem;
    TBXItemG7: TSpTbxItem;
    TBXItemG8: TSpTbxItem;
    TBXItemG9: TSpTbxItem;
    TBXSubmenuHelp: TSpTBXSubmenuItem;
    TBXItemHelpAbout: TSpTBXItem;
    TBXSubmenuOptions: TSpTBXSubmenuItem;
    TBXItemOSetup: TSpTbxItem;
    TBXItemOLexer: TSpTbxItem;
    TBXItemOLexerLib: TSpTbxItem;
    TBXSubmenuSearch: TSpTBXSubmenuItem;
    TBXItemSGoto: TSpTbxItem;
    TBXSeparatorItem21: TSpTbxSeparatorItem;
    TBXItemSNext: TSpTbxItem;
    TBXItemSPrev: TSpTbxItem;
    TBXSeparatorItem19: TSpTbxSeparatorItem;
    TBXItemSWordNext: TSpTbxItem;
    TBXItemSWordPrior: TSpTbxItem;
    TBXItemSMarkClear: TSpTbxItem;
    TBXItemEUnindent: TSpTbxItem;
    TBXItemEIndent: TSpTbxItem;
    TBXSubmenuItemSortOps: TSpTBXSubmenuItem;
    TBXItemESortAsc: TSpTbxItem;
    TBXItemESortDesc: TSpTbxItem;
    TBXSubmenuItemCaseOps: TSpTBXSubmenuItem;
    TBXItemECaseUpper: TSpTbxItem;
    TBXItemECaseLower: TSpTbxItem;
    TBXItemECaseTitle: TSpTbxItem;
    TBXItemECaseInvert: TSpTbxItem;
    TBXItemHelpReadmeDir: TSpTBXItem;
    TBXItemONPrint: TSpTbxItem;
    TBXItemONums: TSpTbxItem;
    TBXItemOTree: TSpTbxItem;
    TBXItemOFold: TSpTbxItem;
    TBXItemOWrap: TSpTbxItem;
    TBXItemORO: TSpTbxItem;
    TBXItemSMarkPrev: TSpTbxItem;
    TBXItemSMarkNext: TSpTbxItem;
    TBXItemETable: TSpTbxItem;
    TBXSubmenuLexers: TSpTBXSubmenuItem;
    TBXItemEDup: TSpTbxItem;
    DKLanguageController1: TDKLanguageController;
    TBXItemOTools: TSpTbxItem;
    TBXSeparatorItem22: TSpTbxSeparatorItem;
    TBXItemBkClear: TSpTbxItem;
    TimerHint: TTimer;
    TBXSeparatorItem26: TSpTbxSeparatorItem;
    TBXItemMarkSwap: TSpTbxItem;
    TBXItemMarkColl: TSpTbxItem;
    TBXItemMarkDrop: TSpTbxItem;
    tbQs: TSpTbxToolbar;
    TBXItemFFNext: TSpTbxItem;
    TBXItemFFPrev: TSpTbxItem;
    TBXSubmenuViewToolbars: TSpTBXSubmenuItem;
    TBXItemTQs: TSpTbxItem;
    TBXItemTView: TSpTbxItem;
    TBXItemTEdit: TSpTbxItem;
    TBXItemTFile: TSpTbxItem;
    cbCase: TSpTbxItem;
    edQs: TSpTBXEdit;
    TBControlItem1aa: TTBControlItem;
    TBXItemQs: TSpTbxItem;
    cbWord: TSpTbxItem;
    TBXItemSMarkAll: TSpTbxItem;
    TBXItemHelpTopics: TSpTBXItem;
    TBXItemFClose: TSpTbxItem;
    acClose: TAction;
    PopupTabContext: TSpTBXPopupMenu;
    TBXItemTabCloseOthers: TSpTBXItem;
    TBXItemTabClose: TSpTBXItem;
    TBXSeparatorItem28: TSpTbxSeparatorItem;
    ImageListCloseBtn: TImageList;
    acSaveAll: TAction;
    TBXItemFSaveAll: TSpTbxItem;
    acCloseAll: TAction;
    TBXItemFCloseAll: TSpTbxItem;
    acCloseOthersThisGroup: TAction;
    TBXItemFCloseOth: TSpTbxItem;
    TBXSeparatorItem29: TSpTbxSeparatorItem;
    TBXItemFSesOpen: TSpTbxItem;
    TBXItemFSesSaveAs: TSpTbxItem;
    OD_Session: TTntOpenDialog;
    SD_Session: TTntSaveDialog;
    TBXSubmenuItemFRecents: TSpTbxSubmenuItem;
    TBXSeparatorItem30: TSpTbxSeparatorItem;
    TBXItemFClearRecents: TSpTBXItem;
    TbxSubmenuWindow: TSpTBXSubmenuItem;
    TBXItemETime: TSpTbxItem;
    TBXSubmenuTools: TSpTBXSubmenuItem;
    TBXItemRunOpenDir: TSpTbxItem;
    TBXSubmenuEncodings: TSpTBXSubmenuItem;
    TBXSubmenuLineEnds: TSpTbxSubmenuItem;
    TBXItemEndMWin: TSpTbxItem;
    TBXItemEndMUn: TSpTbxItem;
    TBXItemEndMMac: TSpTbxItem;
    TBXSubmenuEncReread: TSpTBXSubmenuItem;
    TBXSubmenuEncConvert: TSpTBXSubmenuItem;
    TimerLoad: TTimer;
    ecReplace: TAction;
    TBXItemSRep: TSpTbxItem;
    TBXItemSFind: TSpTbxItem;
    acExit: TAction;
    TBXItemFCloseDel: TSpTbxItem;
    acCloseAndDelete: TAction;
    ecReplaceInFiles: TAction;
    TBXItemSRepInFiles: TSpTBXItem;
    TimerSel: TTimer;
    TBXSubmenuCtxMore: TSpTBXSubmenuItem;
    TBXItemCtxCopyUrl: TSpTbxItem;
    TBXItemCtxCopyAppend: TSpTbxItem;
    TBXItemCtxCutAppend: TSpTbxItem;
    TBXSeparatorItem34: TSpTbxSeparatorItem;
    TBXItemCtxCopyHTML: TSpTbxItem;
    TBXItemCtxCopyRTF: TSpTbxItem;
    TBXSeparatorItem35: TSpTbxSeparatorItem;
    TBXItemECutApp: TSpTbxItem;
    TBXItemECopyApp: TSpTbxItem;
    TBXItemSGoBracket: TSpTbxItem;
    plOut: TSpTbxDockablePanel;
    ListOut: TTntListBox;
    TBXItemBarPOut: TSpTBXItem;
    ecShowOut: TAction;
    TBXItemOOut: TSpTbxItem;
    PopupOut: TSpTbxPopupMenu;
    TBXItemOutClear: TSpTBXItem;
    TBXItemOutCopySel: TSpTBXItem;
    TBXItemOutNav: TSpTBXItem;
    TBXSeparatorItem18: TSpTbxSeparatorItem;
    TBXItemOutCopyAll: TSpTBXItem;
    TBXSeparatorItem32: TSpTbxSeparatorItem;
    ImageListStatus: TImageList;
    TBXItemOutDelSel: TSpTBXItem;
    TBXItemOutDelNonparsed: TSpTBXItem;
    PopupFind: TSpTbxPopupMenu;
    TBXItemTreeFindNav: TSpTbxItem;
    TBXSeparatorItem36: TSpTbxSeparatorItem;
    TBXSeparatorItem37: TSpTbxSeparatorItem;
    acSetupLexerStyles: TAction;
    TBXSeparatorItem38: TSpTbxSeparatorItem;
    TBXItemEExtr: TSpTbxItem;
    TBXSeparatorItem39: TSpTbxSeparatorItem;
    TBXItemECaseSent: TSpTbxItem;
    TBXItemBarCaseSent: TSpTBXItem;
    ecSentCase: TAction;
    PopupZoom: TSpTbxPopupMenu;
    TBXItemZSet300: TSpTbxItem;
    TBXItemZSet200: TSpTbxItem;
    TBXItemZSet150: TSpTbxItem;
    TBXItemZSet100: TSpTbxItem;
    TBXItemZSet75: TSpTbxItem;
    TBXItemZSet50: TSpTbxItem;
    TBXItemZSet25: TSpTBXItem;
    TBXItemZOther: TSpTbxItem;
    plClip: TSpTbxDockablePanel;
    ecShowClip: TAction;
    TBXSeparatorItem25: TSpTbxSeparatorItem;
    TBXItemOClip: TSpTbxItem;
    PopupClip: TSpTbxPopupMenu;
    TBXItemClipDeleteAll: TSpTBXItem;
    ecGotoNextFindResult: TAction;
    ecGotoPrevFindResult: TAction;
    TBXItemSResPrev: TSpTbxItem;
    TBXItemSResNext: TSpTbxItem;
    TBXItemESyncEd: TSpTbxItem;
    TBXItemFSesAdd: TSpTbxItem;
    ecFullScr: TAction;
    TBXItemOFullScr: TSpTbxItem;
    TimerBrackets: TTimer;
    TBXSeparatorItem41: TSpTbxSeparatorItem;
    TBXItemTabCopyDir: TSpTBXItem;
    TBXItemTabCopyFull: TSpTBXItem;
    TBXItemTabCopyFN: TSpTBXItem;
    TBXSeparatorItem42: TSpTbxSeparatorItem;
    TBXSeparatorItem44: TSpTbxSeparatorItem;
    TBXItemBarSyncH: TSpTBXItem;
    TBXItemBarSyncV: TSpTBXItem;
    ecSyncScrollH: TAction;
    ecSyncScrollV: TAction;
    TBXItemOShell: TSpTbxItem;
    TBXSubmenuView: TSpTBXSubmenuItem;
    TBXItemOOnTop: TSpTbxItem;
    ecOnTop: TAction;
    TBXItemEFillBlock: TSpTbxItem;
    TBXItemEInsText: TSpTbxItem;
    TBXSeparatorItem23: TSpTbxSeparatorItem;
    TBXItemCtxOpenSel: TSpTbxItem;
    TBXSeparatorItem45: TSpTbxSeparatorItem;
    TBXItemCtxTool12: TSpTBXItem;
    TBXItemCtxTool11: TSpTBXItem;
    TBXItemCtxTool10: TSpTBXItem;
    TBXItemCtxTool9: TSpTBXItem;
    TBXItemCtxTool8: TSpTBXItem;
    TBXItemCtxTool7: TSpTBXItem;
    TBXItemCtxTool6: TSpTBXItem;
    TBXItemCtxTool5: TSpTBXItem;
    TBXItemCtxTool4: TSpTBXItem;
    TBXItemCtxTool3: TSpTBXItem;
    TBXItemCtxTool2: TSpTBXItem;
    TBXItemCtxTool1: TSpTBXItem;
    TBXSeparatorItem13: TSpTbxSeparatorItem;
    TBXSeparatorItem24: TSpTbxSeparatorItem;
    TBXSubmenuAdvanced: TSpTBXSubmenuItem;
    TBXItemORestoreStyles: TSpTbxItem;
    TBXItemCtxCustomize: TSpTbxItem;
    ecToggleFocusTree: TAction;
    ecToggleFocusClip: TAction;
    TBXSeparatorItem46: TSpTbxSeparatorItem;
    TBXSubmenuItemCommentOps: TSpTBXSubmenuItem;
    TBXItemEComm: TSpTbxItem;
    TBXItemEUncomm: TSpTbxItem;
    TBXItemEToggleLineComment: TSpTbxItem;
    ecToggleLineComment: TAction;
    ecToggleFocusOutput: TAction;
    acBackup: TAction;
    ecToggleStreamComment: TAction;
    TBXItemEToggleStreamComment: TSpTbxItem;
    TBXSubmenuItemIndentOps: TSpTBXSubmenuItem;
    TBXSubmenuItemLineOps: TSpTBXSubmenuItem;
    TBXItemEMoveDn: TSpTbxItem;
    TBXItemEMoveUp: TSpTbxItem;
    TBXItemHelpDonate: TSpTBXItem;
    TBXItemEDelLn: TSpTbxItem;
    TBXSeparatorItem47: TSpTbxSeparatorItem;
    TBXSubmenuItemCopyOps: TSpTBXSubmenuItem;
    TBXItemECpDirPath: TSpTbxItem;
    TBXItemECpFullPath: TSpTbxItem;
    TBXItemECpFN: TSpTbxItem;
    ecToggleFocusFindRes: TAction;
    TBXItemBarSpellChk: TSpTBXItem;
    TBXSeparatorItem48: TSpTbxSeparatorItem;
    ecSpellCheck: TAction;
    ecSpellLive: TAction;
    TBXSubmenuBarSpell: TSpTBXSubmenuItem;
    TBXItemVSpellCheck: TSpTbxItem;
    TBXItemVSpellLive: TSpTbxItem;
    TBXSeparatorItem49: TSpTbxSeparatorItem;
    TBXItemEJoin: TSpTbxItem;
    TBXItemESplit: TSpTbxItem;
    TBXSeparatorItem50: TSpTbxSeparatorItem;
    TBXSubmenuMacros: TSpTBXSubmenuItem;
    TBXItemMacroPlay: TSpTbxItem;
    TBXItemMacroStop: TSpTbxItem;
    TBXItemMacroRecord: TSpTbxItem;
    TBXItemMacroCancel: TSpTbxItem;
    TBXSeparatorItem51: TSpTbxSeparatorItem;
    TBXItemMacroDlg: TSpTbxItem;
    acMacroRecord: TecMacroRecord;
    acMacroStop: TecMacroStop;
    acMacroCancel: TecMacroCancel;
    acMacroPlay: TecMacroPlay;
    acMacroDialog: TAction;
    ecMacroRec: TecMacroRecorder;
    acMacro1: TAction;
    acMacro2: TAction;
    acMacro3: TAction;
    acMacro4: TAction;
    acMacro5: TAction;
    acMacro6: TAction;
    acMacro7: TAction;
    acMacro8: TAction;
    acMacro9: TAction;
    PropsManagerKeys: TPropsManager;
    TBXSeparatorItem53: TSpTbxSeparatorItem;
    TBXItemMacro9: TSpTbxItem;
    TBXItemMacro8: TSpTbxItem;
    TBXItemMacro7: TSpTbxItem;
    TBXItemMacro6: TSpTbxItem;
    TBXItemMacro5: TSpTbxItem;
    TBXItemMacro4: TSpTbxItem;
    TBXItemMacro3: TSpTbxItem;
    TBXItemMacro2: TSpTbxItem;
    TBXItemMacro1: TSpTbxItem;
    TBXItemBkNext: TSpTbxItem;
    TBXItemBkPrev: TSpTbxItem;
    TBXItemBkToggle: TSpTbxItem;
    ecBkClearAll: TAction;
    ecBkToggle: TAction;
    ecBkNext: TAction;
    ecBkPrev: TAction;
    ecBkInverse: TAction;
    TBXItemBkInverse: TSpTbxItem;
    ecBkCopy: TAction;
    ecBkCut: TAction;
    ecBkDelete: TAction;
    ecBkDeleteUnmk: TAction;
    ecBkPaste: TAction;
    TBXItemBkDelUnmk: TSpTbxItem;
    TBXItemBkDel: TSpTbxItem;
    TBXItemBkPaste: TSpTbxItem;
    TBXItemBkCut: TSpTbxItem;
    TBXItemBkCopy: TSpTbxItem;
    TBXItemBarGoto: TSpTBXItem;
    ecGoto: TAction;
    TBXSeparatorItem12: TSpTbxSeparatorItem;
    TBXItemVSyncVert: TSpTbxItem;
    TBXItemVSyncHorz: TSpTbxItem;
    ecToggleFocusGroups: TAction;
    TBXSeparatorItem54: TSpTbxSeparatorItem;
    TBXItemECutLine: TSpTbxItem;
    TBXItemECopyLine: TSpTbxItem;
    ListVal: TTntListBox;
    PopupValidate: TSpTbxPopupMenu;
    TBXItemValNav: TSpTbxItem;
    TBXSeparatorItem56: TSpTbxSeparatorItem;
    TBXItemValCopySel: TSpTBXItem;
    TBXItemValCopyAll: TSpTBXItem;
    TBXSeparatorItem57: TSpTbxSeparatorItem;
    TBXItemValClear: TSpTBXItem;
    ecToggleFocusValidate: TAction;
    TBXItemEDedupAdjacent: TSpTbxItem;
    TBXItemBarDedupAdj: TSpTBXItem;
    TBXSubmenuItemSess: TSpTbxSubmenuItem;
    TBXSeparatorItem59: TSpTbxSeparatorItem;
    TBXItemSessClr: TSpTbxItem;
    TBXItemFSesSave: TSpTbxItem;
    TBXItemFSesClose: TSpTbxItem;
    ecRemoveBlanks: TAction;
    TBXItemERemBlanks: TSpTbxItem;
    ecRemoveLines: TAction;
    TBXSubmenuItemBlankOps: TSpTBXSubmenuItem;
    TBXSeparatorItem60: TSpTbxSeparatorItem;
    TBXItemETrimAll: TSpTbxItem;
    TBXItemETrimTrail: TSpTbxItem;
    TBXItemETrimLead: TSpTbxItem;
    ecTrimLead: TAction;
    ecTrimTrail: TAction;
    ecTrimAll: TAction;
    ecRemoveDupSpaces: TAction;
    TBXItemERemDupSp: TSpTbxItem;
    ecTabToSp: TAction;
    ecSpToTab: TAction;
    TBXSeparatorItem61: TSpTbxSeparatorItem;
    TBXItemESpToTab: TSpTbxItem;
    TBXItemETabToSp: TSpTbxItem;
    ecFindClipNext: TAction;
    ecFindClipPrev: TAction;
    TBXSubmenuItemSessions: TSpTBXSubmenuItem;
    ecSplit50_50: TAction;
    ecSplit40_60: TAction;
    ecSplit60_40: TAction;
    ecSplit30_70: TAction;
    ecSplit70_30: TAction;
    ecSplit20_80: TAction;
    ecSplit80_20: TAction;
    TBXItemMacroRepeat: TSpTbxItem;
    acMacroRepeat: TAction;
    TBXSeparatorItem62: TSpTbxSeparatorItem;
    ecRepeatCmd: TAction;
    TBXItemERepeatCmd: TSpTbxItem;
    TBXSepWin: TSpTbxSeparatorItem;
    TBXItemWinFRes: TSpTbxItem;
    TBXItemWinOut: TSpTbxItem;
    TBXItemWinClip: TSpTbxItem;
    TBXItemWinTree: TSpTbxItem;
    TBXItemWinVal: TSpTbxItem;
    TBXSubWin: TSpTbxSubmenuItem;
    TBXItemMarkClear: TSpTbxItem;
    ecToggleFocusMap: TAction;
    TBXItemWinMap: TSpTbxItem;
    ecFindInTree: TAction;
    ecFindInTreeNext: TAction;
    ecFindInTreePrev: TAction;
    ecTreeNext: TAction;
    ecTreePrev: TAction;
    ecReduceBlanks: TAction;
    TBXItemEReduceBlanks: TSpTbxItem;
    ecSplitLeft: TAction;
    ecSplitRight: TAction;
    ecFindNextWithExtend: TAction;
    ecFindPrevWithExtend: TAction;
    ecFindInList: TAction;
    ecFindInListNext: TAction;
    ecFindInListPrev: TAction;
    TBXItemClipFind: TSpTbxItem;
    TBXSeparatorItem64: TSpTbxSeparatorItem;
    TBXSeparatorItem65: TSpTbxSeparatorItem;
    TBXItemTreeFindFind: TSpTbxItem;
    TBXSeparatorItem66: TSpTbxSeparatorItem;
    TBXItemValFind: TSpTbxItem;
    TBXSeparatorItem67: TSpTbxSeparatorItem;
    TBXItemOutFind: TSpTBXItem;
    PopupTree: TSpTbxPopupMenu;
    TBXItemTreeFind: TSpTbxItem;
    TBXItemTreeExpand: TSpTbxItem;
    TBXItemTreeCollapse: TSpTbxItem;
    TimerAutoSave: TTimer;
    TBXItemMacro29: TSpTbxItem;
    TBXItemMacro28: TSpTbxItem;
    TBXItemMacro27: TSpTbxItem;
    TBXItemMacro26: TSpTbxItem;
    TBXItemMacro25: TSpTbxItem;
    TBXItemMacro24: TSpTbxItem;
    TBXItemMacro23: TSpTbxItem;
    TBXItemMacro22: TSpTbxItem;
    TBXItemMacro21: TSpTbxItem;
    TBXItemMacro20: TSpTbxItem;
    TBXItemMacro19: TSpTbxItem;
    TBXItemMacro18: TSpTbxItem;
    TBXItemMacro17: TSpTbxItem;
    TBXItemMacro16: TSpTbxItem;
    TBXItemMacro15: TSpTbxItem;
    TBXItemMacro14: TSpTbxItem;
    TBXItemMacro13: TSpTbxItem;
    TBXItemMacro12: TSpTbxItem;
    TBXItemMacro11: TSpTbxItem;
    TBXItemMacro10: TSpTbxItem;
    TBXItemMacro30: TSpTbxItem;
    acMacro10: TAction;
    acMacro11: TAction;
    acMacro12: TAction;
    acMacro13: TAction;
    acMacro14: TAction;
    acMacro15: TAction;
    acMacro16: TAction;
    acMacro17: TAction;
    acMacro18: TAction;
    acMacro19: TAction;
    acMacro20: TAction;
    acMacro21: TAction;
    acMacro22: TAction;
    acMacro23: TAction;
    acMacro24: TAction;
    acMacro25: TAction;
    acMacro26: TAction;
    acMacro27: TAction;
    acMacro28: TAction;
    acMacro29: TAction;
    acMacro30: TAction;
    TreeFind: TTntTreeView;
    TBXItemTreeFindCopyToTab: TSpTbxItem;
    TBXItemTreeFindClear: TSpTbxItem;
    TBXItemTreeFindCopyToClip: TSpTbxItem;
    TBXItemTreeFindCollapse: TSpTbxItem;
    TBXItemTreeFindExpand: TSpTbxItem;
    TBXSeparatorItem69: TSpTbxSeparatorItem;
    TBXItemTreeFindExpandCur: TSpTbxItem;
    TBXItemCtxFindID: TSpTbxItem;
    ecTreeParent: TAction;
    ecTreeNextBrother: TAction;
    ecTreePrevBrother: TAction;
    TBXItemMarkGoLast: TSpTbxItem;
    TBXItemRunOpenFile: TSpTbxItem;
    TBXItemSSelToken: TSpTbxItem;
    TBXItemTreeFindCopyToClipNode: TSpTbxItem;
    TBXSeparatorItem71: TSpTbxSeparatorItem;
    TBXItemClipCopyToEd: TSpTbxItem;
    TBXItemClipCopyToClip: TSpTbxItem;
    TBXSeparatorItem72: TSpTbxSeparatorItem;
    ecReplaceSelFromClipAll: TAction;
    acRereadOut: TAction;
    ecToggleFocusProject: TAction;
    TBXItemWinProj: TSpTbxItem;
    ecToggleFocusMasterSlave: TAction;
    ecToggleSlave: TAction;
    ecRuler: TAction;
    TBXItemORuler: TSpTbxItem;
    ecSplitViewsVertHorz: TAction;
    ecSplitSlaveVertHorz: TAction;
    ecGotoBk: TAction;
    TBXItemBkGoto: TSpTbxItem;
    TBXSubmenuItemFav: TSpTBXSubmenuItem;
    TBXItemFavAddFile: TSpTbxItem;
    TBXItemFavManage: TSpTbxItem;
    acFavAddFile: TAction;
    acFavManage: TAction;
    TbxSubmenuColors: TSpTBXSubmenuItem;
    ImageListColorRecent: TImageList;
    TBXItemCtxAddColor: TSpTbxItem;
    TBXItemFavAddProj: TSpTbxItem;
    acFavAddProj: TAction;
    TBXSeparatorItem75: TSpTbxSeparatorItem;
    TBXItemTabAddToProj: TSpTBXItem;
    ecToggleFocusClips: TAction;
    TbxItemWinClips: TSpTbxItem;
    PopupClips: TSpTbxPopupMenu;
    TBXItemClipsAddText: TSpTbxItem;
    TBXItemClipsEdit: TSpTbxItem;
    TBXSeparatorItem76: TSpTbxSeparatorItem;
    TBXItemClipsDir: TSpTbxItem;
    OD_Swatch: TOpenDialog;
    SD_Swatch: TSaveDialog;
    TBXTabColor: TSpTbxColorPalette;
    TBXSubmenuTabColor: TSpTbxSubmenuItem;
    TBXItemTabColorMisc: TSpTbxItem;
    TBXSeparatorItem77: TSpTbxSeparatorItem;
    TBXItemTabColorDef: TSpTbxItem;
    ecSmartHl: TAction;
    TBXItemBkDropPortable: TSpTbxItem;
    ecDropPortableBk: TAction;
    TBXSeparatorItem78: TSpTbxSeparatorItem;
    ecGotoPortableBk: TAction;
    TBXItemBkGotoPortable: TSpTbxItem;
    acRename: TAction;
    TBXItemFRename: TSpTbxItem;
    TBXItemRunNumConv: TSpTbxItem;
    ecNumericConverter: TAction;
    ecIndentLike1st: TAction;
    TBXSeparatorItem33: TSpTbxSeparatorItem;
    TBXItemEIndentLike1st: TSpTbxItem;
    TBXSeparatorItem79: TSpTbxSeparatorItem;
    ImageListFtp: TImageList;
    ListPLog: TTntListBox;
    PopupPluginsLog: TSpTbxPopupMenu;
    TBXItemPLogCopySel: TSpTbxItem;
    TBXItemPLogCopyAll: TSpTbxItem;
    TBXSeparatorItem81: TSpTbxSeparatorItem;
    TBXItemPLogDelete: TSpTbxItem;
    TBXItemPLogClear: TSpTbxItem;
    TBXSeparatorItem82: TSpTbxSeparatorItem;
    TBXItemPLogFind: TSpTbxItem;
    TBXItemCtxPasteNoCurChange: TSpTbxItem;
    TBXSeparatorItem80: TSpTbxSeparatorItem;
    PluginACP: TAutoCompletePopup;
    ecCenterLines: TAction;
    TBXSeparatorItem83: TSpTbxSeparatorItem;
    TBXItemECenterLines: TSpTbxItem;
    ListTabs: TTntListView;
    ecToggleFocusTabs: TAction;
    TbxItemWinTabs: TSpTbxItem;
    TBXSubmenuPlugins: TSpTBXSubmenuItem;
    TBXSeparatorItem84: TSpTbxSeparatorItem;
    TBXItemOEditSynPluginsIni: TSpTbxItem;
    TBXItemPLogSaveAs: TSpTbxItem;
    TBXItemTabMoveToWindow: TSpTBXItem;
    TBXItemTabOpenInWindow: TSpTBXItem;
    ecSortDialog: TAction;
    TBXItemESortDialog: TSpTbxItem;
    TBXSeparatorItem86: TSpTbxSeparatorItem;
    TBXItemBarSortDialog: TSpTBXItem;
    TBXItemSSelBrackets: TSpTbxItem;
    PropsManagerPrint: TPropsManager;
    TimerTree: TTimer;
    ecCollapseParent: TAction;
    ecCollapseWithNested: TAction;
    ecSpToTabLeading: TAction;
    TBXItemESpToTabLead: TSpTbxItem;
    ecToggleLineCommentAlt: TAction;
    TBXItemEToggleLineCommentAlt: TSpTbxItem;
    TBXItemCtxPasteToColumn1: TSpTbxItem;
    ecCommandsList: TAction;
    TBXItemHelpCommandList: TSpTBXItem;
    ecProjectList: TAction;
    TbxSubmenuItemCaretsOps: TSpTBXSubmenuItem;
    TBXItemCaretsRemove2: TSpTbxItem;
    TBXItemCaretsRemove1: TSpTbxItem;
    TBXSeparatorItem91: TSpTbxSeparatorItem;
    TBXItemCaretsFromSelClear: TSpTbxItem;
    TBXItemCaretsFromSelRight: TSpTbxItem;
    TBXItemCaretsFromSelLeft: TSpTbxItem;
    TBXSeparatorItem92: TSpTbxSeparatorItem;
    TBXItemCaretsExtDownEnd: TSpTbxItem;
    TBXItemCaretsExtUpEnd: TSpTbxItem;
    TBXItemCaretsExtDownPage: TSpTbxItem;
    TBXItemCaretsExtUpPage: TSpTbxItem;
    TBXItemCaretsExtDownLine: TSpTbxItem;
    TBXItemCaretsExtUpLine: TSpTbxItem;
    TBXSeparatorItem93: TSpTbxSeparatorItem;
    TBXItemCaretsFromMarksClear: TSpTbxItem;
    TBXItemCaretsFromMarksRight: TSpTbxItem;
    TBXItemCaretsFromMarksLeft: TSpTbxItem;
    TBXItemEColumn: TSpTbxItem;
    ecEditColumn: TAction;
    ecDedupAll: TAction;
    ecDedupAdjacent: TAction;
    TBXSeparatorItem94: TSpTbxSeparatorItem;
    TBXItemEDedupAll: TSpTbxItem;
    TBXSeparatorItem85: TSpTbxSeparatorItem;
    TBXItemBarDedupAll: TSpTBXItem;
    TBXSeparatorItem95: TSpTbxSeparatorItem;
    TBXItemEAlignWithSep: TSpTbxItem;
    ecAlignWithSep: TAction;
    TBXItemTabToggleSplit: TSpTBXItem;
    ecToggleShowGroup2: TAction;
    TBXItemSSelExtend: TSpTbxItem;
    TBXItemTreeCollapseAll: TSpTbxItem;
    TBXItemTreeExpandAll: TSpTbxItem;
    TBXItemTreeLevel2: TSpTbxItem;
    TBXItemTreeLevel5: TSpTbxItem;
    TBXItemTreeLevel4: TSpTbxItem;
    TBXItemTreeLevel3: TSpTbxItem;
    TBXSeparatorItem96: TSpTbxSeparatorItem;
    TBXSubmenuTreeLevel: TSpTbxSubmenuItem;
    TBXItemTreeLevel6: TSpTbxItem;
    TBXItemTreeLevel9: TSpTbxItem;
    TBXItemTreeLevel8: TSpTbxItem;
    TBXItemTreeLevel7: TSpTbxItem;
    ecReverseLines: TAction;
    TBXSeparatorItem68: TSpTbxSeparatorItem;
    TBXItemEReverse: TSpTbxItem;
    ecShuffleLines: TAction;
    TBXItemEShuffle: TSpTbxItem;
    tbUser1: TSpTbxToolbar;
    tbUser2: TSpTbxToolbar;
    tbUser3: TSpTbxToolbar;
    TBXSeparatorItem97: TSpTbxSeparatorItem;
    TBXItemTUser3: TSpTbxItem;
    TBXItemTUser2: TSpTbxItem;
    TBXItemTUser1: TSpTbxItem;
    TBXSubmenuToolbars: TSpTbxSubmenuItem;
    TBXItemOToolbar3: TSpTbxItem;
    TBXItemOToolbar2: TSpTbxItem;
    TBXItemOToolbar1: TSpTbxItem;
    ecExtractDupsCase: TAction;
    ecExtractDupsNoCase: TAction;
    TBXSeparatorItem7: TSpTbxSeparatorItem;
    TBXItemEExtractDupNoCase: TSpTbxItem;
    TBXItemEExtractDupCase: TSpTbxItem;
    ecNonPrintOff: TAction;
    ecNonPrintSpaces: TAction;
    ecNonPrintEol: TAction;
    ecNonPrintBoth: TAction;
    TBXSubmenuNonPrint: TSpTBXSubmenuItem;
    TBXSeparatorItem98: TSpTbxSeparatorItem;
    TBXItemONPrintAll: TSpTbxItem;
    TBXItemONPrintEol: TSpTbxItem;
    TBXItemONPrintSpaces: TSpTbxItem;
    PopupUserTB1: TSpTbxPopupMenu;
    TBXItemUserTb1: TSpTbxItem;
    PopupUserTB2: TSpTbxPopupMenu;
    TBXItemUserTB2: TSpTbxItem;
    PopupUserTB3: TSpTbxPopupMenu;
    TBXItemUserTB3: TSpTbxItem;
    TBXItemCtxPasteBkmkLines: TSpTbxItem;
    TBXItemCtxPasteAsColumn: TSpTbxItem;
    TBXSeparatorItem99: TSpTbxSeparatorItem;
    TBXItemONPrintEolDetails: TSpTbxItem;
    ecNonPrintEolDetails: TAction;
    TBXItemOHideItems: TSpTbxItem;
    TBXSeparatorItem100: TSpTbxSeparatorItem;
    TBXItemOEditSynIni: TSpTbxItem;
    acOpenBySelection: TAction;
    ImageListUser1: TPngImageList;
    ImageListUser2: TPngImageList;
    ImageListUser3: TPngImageList;
    TBXDockLeft1: TSpTBXDock;
    TBXDockRight1: TSpTbxDock;
    TBXDockBottom1: TSpTbxDock;
    SplitterLeft: TSpTBXSplitter;
    SplitterRight: TSpTBXSplitter;
    SplitterBottom: TSpTBXSplitter;
    StatusItemLexer: TSpTBXLabelItem;
    StatusItemEnds: TSpTBXLabelItem;
    StatusItemEnc: TSpTBXLabelItem;
    StatusItemCaret: TSpTBXLabelItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    StatusItemChar: TSpTBXLabelItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    StatusItemRO: TSpTBXLabelItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    StatusItemWrap: TSpTBXLabelItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    StatusItemSelMode: TSpTBXLabelItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    StatusItemZoom: TSpTBXLabelItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    SpTBXSeparatorItem11: TSpTBXSeparatorItem;
    StatusItemHint: TSpTBXLabelItem;
    TBXMRUListItemFRecents: TSpTBXMRUListItem;
    TBXMRUListItem_Sess: TSpTBXMRUListItem;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    SpTBXSeparatorItem14: TSpTBXSeparatorItem;
    TBXMRUListItemFNew: TSpTBXMRUListItem;
    TBXItemBarPClip: TSpTBXItem;
    TBXSubmenuMarkers: TSpTBXSubmenuItem;
    TBXSubmenuBkOps: TSpTBXSubmenuItem;
    TBXSubmenuBkPortable: TSpTBXSubmenuItem;
    SpTBXSeparatorItem16: TSpTBXSeparatorItem;
    SpTBXSeparatorItem19: TSpTBXSeparatorItem;
    PopupFoldLevel: TSpTBXPopupMenu;
    SpTBXSeparatorItem23: TSpTBXSeparatorItem;
    TBXItemUnfoldAll: TSpTBXItem;
    TBXItemFoldAll: TSpTBXItem;
    TBXSeparatorItem87: TSpTBXSeparatorItem;
    TBXItemUnfoldLine: TSpTBXItem;
    TBXItemFoldNearestBlock: TSpTBXItem;
    TBXItemFoldSelBlock: TSpTBXItem;
    TBXSeparatorItem89: TSpTBXSeparatorItem;
    TBXItemFoldRangesInSel: TSpTBXItem;
    TBXItemUnfoldRangesInSel: TSpTBXItem;
    TBXSeparatorItem88: TSpTBXSeparatorItem;
    TBXItemFoldParent: TSpTBXItem;
    TBXItemFoldWithNested: TSpTBXItem;
    TBXSubmenuFoldLevel: TSpTBXSubmenuItem;
    TBXItemFoldLevel2: TSpTBXItem;
    TBXItemFoldLevel3: TSpTBXItem;
    TBXItemFoldLevel4: TSpTBXItem;
    TBXItemFoldLevel5: TSpTBXItem;
    TBXItemFoldLevel6: TSpTBXItem;
    TBXItemFoldLevel7: TSpTBXItem;
    TBXItemFoldLevel8: TSpTBXItem;
    TBXItemFoldLevel9: TSpTBXItem;
    TbxItemTabReload: TSpTBXItem;
    TimerMinimap: TTimer;
    SpTBXSeparatorItem18: TSpTBXSeparatorItem;
    TBXSubmenuItemPrint: TSpTBXSubmenuItem;
    TbxItemMenuXX: TSpTBXItem;
    TbxItemMenuX: TSpTBXItem;
    TBXSubmenuItemProjects: TSpTBXSubmenuItem;
    TBXItemProjAddFile: TSpTBXItem;
    TBXItemProjOpen: TSpTBXItem;
    TBXSubmenuItemProjRecents: TSpTBXSubmenuItem;
    TBXMRUListItem_Projects: TSpTBXMRUListItem;
    SpTBXSeparatorItem24: TSpTBXSeparatorItem;
    TBXItemProjRecentClear: TSpTBXItem;
    SpTBXSeparatorItem21: TSpTBXSeparatorItem;
    TBXItemProjGoto: TSpTBXItem;
    TBXItemProjNew: TSpTBXItem;
    TbxItemCtxTool16: TSpTBXItem;
    TbxItemCtxTool15: TSpTBXItem;
    TbxItemCtxTool14: TSpTBXItem;
    TbxItemCtxTool13: TSpTBXItem;
    TBXItemProjAddAllFiles: TSpTBXItem;
    TbxItemProjSave: TSpTBXItem;
    plConsole: TPanel;
    edConsole: TTntComboBox;
    ecToggleFocusConsole: TAction;
    TBXItemWinConsole: TSpTBXItem;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PythonEngine1: TPythonEngine;
    MemoConsole: TTntMemo;
    PythonModule: TPythonModule;
    TbxItemHelpPyDir: TSpTBXItem;
    TbxItemRunSnippets: TSpTBXItem;
    SpTBXSeparatorItem26: TSpTBXSeparatorItem;
    TbxItemRunNewSnippet: TSpTBXItem;
    SD_Snippets: TSaveDialog;
    StatusItemTabsize: TSpTBXLabelItem;
    TbxItemTreeSorted: TSpTBXItem;
    ColorDialogTabs: TColorDialog;
    PopupPanelTitle: TSpTBXPopupMenu;
    TbxItemPanelTitleBar: TSpTBXItem;
    TbxItemPanelTitleShowRight: TSpTBXItem;
    TbxItemPanelTitleShowOut: TSpTBXItem;
    TbxItemPanelTitleShowLeft: TSpTBXItem;
    SpTBXSeparatorItem28: TSpTBXSeparatorItem;
    SpTBXSeparatorItem29: TSpTBXSeparatorItem;
    TbxItemWinSplitV: TSpTBXItem;
    TbxItemWinSplitH: TSpTBXItem;
    TbxItemWinProjPre: TSpTBXItem;
    ecToggleProjPreview: TAction;
    TbxSubmenuCtxPlugins: TSpTBXSubmenuItem;
    TbxSubmenuProjTools: TSpTBXSubmenuItem;
    PopupPreviewEditor: TSpTBXPopupMenu;
    TbxItemPreSelect: TSpTBXItem;
    TbxItemPreCopy: TSpTBXItem;
    SpTBXSeparatorItem30: TSpTBXSeparatorItem;
    TbxItemPreZoomOther: TSpTBXItem;
    TbxItemPreZoom100: TSpTBXItem;
    TbxItemPreZoom75: TSpTBXItem;
    TbxItemPreZoom50: TSpTBXItem;
    TbxItemPreZoom25: TSpTBXItem;
    TbxItemPreEdit: TSpTBXItem;
    SpTBXSeparatorItem31: TSpTBXSeparatorItem;
    TBXSubmenuGroups: TSpTBXSubmenuItem;
    TbxItemGroup2H: TSpTBXItem;
    TbxItemGroup2V: TSpTBXItem;
    TbxItemGroupOne: TSpTBXItem;
    TbxItemGroup3H: TSpTBXItem;
    TbxItemGroup3V: TSpTBXItem;
    TbxItemGroup6Grid: TSpTBXItem;
    TbxItemGroup4Grid: TSpTBXItem;
    TbxItemGroup4V: TSpTBXItem;
    TbxItemGroup4H: TSpTBXItem;
    TbxItemGroup3as1p2: TSpTBXItem;
    TBXSubmenuItemToGroup: TSpTBXSubmenuItem;
    TbxItemToGroupPrev: TSpTBXItem;
    TbxItemToGroupNext: TSpTBXItem;
    TbxItemToGroup6: TSpTBXItem;
    TbxItemToGroup5: TSpTBXItem;
    TbxItemToGroup4: TSpTBXItem;
    TbxItemToGroup3: TSpTBXItem;
    TbxItemToGroup2: TSpTBXItem;
    TbxItemToGroup1: TSpTBXItem;
    SpTBXSeparatorItem20: TSpTBXSeparatorItem;
    TBXItemTabCloseOthersAllGroups: TSpTBXItem;
    acCloseOthersAllGroups: TAction;
    ListBookmarks: TTntListView;
    TbxItemWinBkmk: TSpTBXItem;
    ecToggleFocusBookmarks: TAction;
    ImageListIcons: TPngImageList;
    TbxItemWinFtp: TSpTBXItem;
    TbxItemWinExplorer: TSpTBXItem;
    TBXItemTabCloseRighter: TSpTBXItem;
    TBXItemTabCloseLefter: TSpTBXItem;
    ecReplaceInProject: TAction;
    TbxItemSRepInProject: TSpTBXItem;
    ecPreviewActionNew: TAction;
    acSetupLexerNew: TAction;
    ecPageSetupActionNew: TAction;
    TBXItemECaseRandom: TSpTBXItem;
    TBXItemBarCaseRandom: TSpTBXItem;
    acRestart: TAction;
    TBXItemTreeFindPreview: TSpTBXItem;
    TBXItemEDedupAllOrig: TSpTBXItem;
    ecDedupAllAndOrig: TAction;
    ecExtractUniq: TAction;
    TBXItemEExtractUniq: TSpTBXItem;
    TBXItemBarDedupAndOrig: TSpTBXItem;
    TbxSubmenuWeb: TSpTBXSubmenuItem;
    TbxItemTabSaveAs: TSpTBXItem;
    TbxItemTabSave: TSpTBXItem;
    SpTBXSeparatorItem15: TSpTBXSeparatorItem;
    TBXSubmenuAddons: TSpTBXSubmenuItem;
    TbxItemAddonsUpdate: TSpTBXItem;
    TbxItemAddonsSave: TSpTBXItem;
    TbxItemAddonsEdit: TSpTBXItem;
    TbxItemAddonsRemove: TSpTBXItem;
    TbxItemAddonsInstall: TSpTBXItem;
    TBXSubmenuExtTools: TSpTBXSubmenuItem;
    TbxItemTool16: TSpTBXItem;
    TbxItemTool15: TSpTBXItem;
    TbxItemTool14: TSpTBXItem;
    TbxItemTool13: TSpTBXItem;
    TbxItemTool12: TSpTBXItem;
    TbxItemTool11: TSpTBXItem;
    TbxItemTool10: TSpTBXItem;
    TbxItemTool9: TSpTBXItem;
    TbxItemTool8: TSpTBXItem;
    TbxItemTool7: TSpTBXItem;
    TbxItemTool6: TSpTBXItem;
    TbxItemTool5: TSpTBXItem;
    TbxItemTool4: TSpTBXItem;
    TbxItemTool3: TSpTBXItem;
    TbxItemTool2: TSpTBXItem;
    TbxItemTool1: TSpTBXItem;
    PopupStatusEncConvert: TSpTBXPopupMenu;
    TbxItemAddonsConfig: TSpTBXItem;
    TBXItemClipDeleteSel: TSpTBXItem;
    SpTBXSeparatorItem17: TSpTBXSeparatorItem;
    TBXSubmenuSMarks: TSpTBXSubmenuItem;
    procedure acOpenExecute(Sender: TObject);
    procedure ecTitleCaseExecute(Sender: TObject);
    procedure WindowItemClick(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure MRUClick(Sender: TObject; const S: WideString);
    procedure MRU_SessClick(Sender: TObject; const S: WideString);
    procedure PopupLexersPopup(Sender: TObject);
    procedure acSetupExecute(Sender: TObject);
    procedure TimerTickTimer(Sender: TObject);
    procedure acExportRTFBeforeExecute(Sender: TObject);
    procedure ecReadOnlyExecute(Sender: TObject);
    procedure ButtonOnSelect(Sender: TTBCustomItem; Viewer: TTBItemViewer;
      Selecting: Boolean);
    procedure plTreeResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure plTreeVisibleChanged(Sender: TObject);
    procedure ecShowTreeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ecPrinterSetupExecute(Sender: TObject);
    procedure SetFormat(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamCompletionGetParams(Sender: TObject;
      const FuncName: WideString; Pos: Integer);
    procedure ecACPAfterComplete(Sender: TObject; const Item: WideString);
    procedure ecACPGetAutoCompleteList(Sender: TObject; PosX: TPoint; List,
      Display: TWideStrings);
    procedure ecACPCheckChar(Sender: TObject; C: Word;
      var IsWord: Boolean);
    procedure PopupEditorPopup(Sender: TObject);
    procedure TBXItemCtxCopyUrlClick(Sender: TObject);
    procedure SyntaxManagerChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PluginAcpDefineStartPos(Sender: TObject; var StartPos: TPoint);
    procedure ecACPChange(Sender: TObject);
    procedure ecFindExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TBXItemAbClick(Sender: TObject);
    procedure ecWrapExecute(Sender: TObject);
    procedure ecLineNumsExecute(Sender: TObject);
    procedure ecFoldingExecute(Sender: TObject);
    procedure ecNonPrintExecute(Sender: TObject);
    procedure acRereadExecute(Sender: TObject);
    procedure TBXItemClrClick(Sender: TObject);
    procedure tbViewMove(Sender: TObject);
    procedure ecPrintActionBeforeExecute(Sender: TObject);
    procedure TBXItemBarMarksClick(Sender: TObject);
    procedure ecACPKeyPress(Sender: TObject; var Key: Char);
    procedure ecACPListClick(Sender: TObject);
    procedure acNewTabExecute(Sender: TObject);
    procedure TBXItemBarWordPrevClick(Sender: TObject);
    procedure TBXItemBarWordNextClick(Sender: TObject);
    procedure TBXItemBarFNextClick(Sender: TObject);
    procedure TBXItemBarFPrevClick(Sender: TObject);
    procedure TimerRedrawTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bBk0Click(Sender: TObject);
    procedure TBXSubmenuItemBkGotoPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure bbg0Click(Sender: TObject);
    procedure TBXSubmenuItemBkSetPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure ODShow(Sender: TObject);
    procedure SDShow(Sender: TObject);
    procedure acNewWindowExecute(Sender: TObject);
    procedure TBXItemFExitClick(Sender: TObject);
    procedure ecCharPopupChange(Sender: TObject);
    procedure ecCharPopupShow(Sender: TObject);
    procedure ecACPShow(Sender: TObject);
    procedure TBXItemHelpReadmeDirClick(Sender: TObject);
    procedure TBXSubmenuLineEndsPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXSubmenuEncRereadPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemSMarkNextClick(Sender: TObject);
    procedure TBXItemSMarkPrevClick(Sender: TObject);
    procedure TBXItemETableClick(Sender: TObject);
    procedure TBXSubmenuLexersPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemEDupClick(Sender: TObject);
    procedure TBXItemOToolsClick(Sender: TObject);
    procedure TimerHintTimer(Sender: TObject);
    procedure ecACPCloseUp(Sender: TObject; var Accept: Boolean);
    procedure TBXSubmenuToolsPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemZ0Click(Sender: TObject);
    procedure TBXItemMarkDropClick(Sender: TObject);
    procedure TBXItemMarkCollClick(Sender: TObject);
    procedure TBXItemMarkSwapClick(Sender: TObject);
    procedure TBXItemFFPrevClick(Sender: TObject);
    procedure TBXItemFFNextClick(Sender: TObject);
    procedure cbCaseClick(Sender: TObject);
    procedure TBXItemTFileClick(Sender: TObject);
    procedure TBXItemTEditClick(Sender: TObject);
    procedure TBXItemTViewClick(Sender: TObject);
    procedure TBXItemTQsClick(Sender: TObject);
    procedure tbQsClose(Sender: TObject);
    procedure edQsChange(Sender: TObject);
    procedure TBXItemQsClick(Sender: TObject);
    procedure edQsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbWordClick(Sender: TObject);
    procedure TBXItemSMarkAllClick(Sender: TObject);
    procedure TBXItemHelpTopicsClick(Sender: TObject);
    procedure TBXItemTabCloseClick(Sender: TObject);
    procedure DKLanguageController1LanguageChanged(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acSaveAllExecute(Sender: TObject);
    procedure acCloseAllExecute(Sender: TObject);
    procedure acCloseOthersThisGroupExecute(Sender: TObject);
    procedure TBXItemTabCloseOthersClick(Sender: TObject);
    procedure PopupTabContextPopup(Sender: TObject);
    procedure TBXItemFSesSaveAsClick(Sender: TObject);
    procedure TBXItemFSesOpenClick(Sender: TObject);
    procedure TBXItemFClearRecentsClick(Sender: TObject);
    procedure TbxSubmenuWindowPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure TBXSubmenuEncConvertPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure TBXItemETimeClick(Sender: TObject);
    procedure TBXItemEPasteClick(Sender: TObject);
    procedure TBXItemEDeleteClick(Sender: TObject);
    procedure TBXItemESelectAllClick(Sender: TObject);
    procedure TBXItemECutClick(Sender: TObject);
    procedure TBXItemECopyClick(Sender: TObject);
    procedure TBXItemEUndoClick(Sender: TObject);
    procedure TBXItemRunOpenDirClick(Sender: TObject);
    procedure TimerLoadTimer(Sender: TObject);
    procedure TBXItemTbCloseClick(Sender: TObject);
    procedure ecReplaceExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acCloseAndDeleteExecute(Sender: TObject);
    procedure ecReplaceInFilesExecute(Sender: TObject);
    procedure TimerSelTimer(Sender: TObject);
    procedure TBXItemCtxCopyAppendClick(Sender: TObject);
    procedure TBXItemCtxCutAppendClick(Sender: TObject);
    procedure TBXSubmenuEditPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemSGoBracketClick(Sender: TObject);
    procedure plOutResize(Sender: TObject);
    procedure ecShowOutExecute(Sender: TObject);
    procedure plOutVisibleChanged(Sender: TObject);
    procedure ListOutDblClick(Sender: TObject);
    procedure ListOutKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TBXItemOutClearClick(Sender: TObject);
    procedure TBXItemOutCopySelClick(Sender: TObject);
    procedure TBXItemOutNavClick(Sender: TObject);
    procedure PopupOutPopup(Sender: TObject);
    procedure TBXItemOutCopyAllClick(Sender: TObject);
    procedure ListOutDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure TBXItemOutDelSelClick(Sender: TObject);
    procedure TBXItemOutDelNonparsedClick(Sender: TObject);
    procedure ListOutMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListOutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TBXItemOOOutClick(Sender: TObject);
    procedure TBXItemOOFindClick(Sender: TObject);
    procedure PopupFindPopup(Sender: TObject);
    procedure ecCopyAsRTFExecute(Sender: TObject);
    procedure acSetupLexerStylesExecute(Sender: TObject);
    procedure TBXItemEExtrClick(Sender: TObject);
    procedure PopupStatusEncPopup(Sender: TObject);
    procedure ecSentCaseExecute(Sender: TObject);
    procedure TBXItemZSet25Click(Sender: TObject);
    procedure TBXItemZOtherClick(Sender: TObject);
    procedure ListClipDblClick(Sender: TObject);
    procedure plClipResize(Sender: TObject);
    procedure plClipVisibleChanged(Sender: TObject);
    procedure ecShowClipExecute(Sender: TObject);
    procedure TBXItemClipDeleteAllClick(Sender: TObject);
    procedure ecGotoNextFindResultExecute(Sender: TObject);
    procedure ecGotoPrevFindResultExecute(Sender: TObject);
    procedure TBXItemESyncEdClick(Sender: TObject);
    procedure TBXItemFSesAddClick(Sender: TObject);
    procedure ecFullScrExecute(Sender: TObject);
    procedure edQsExit(Sender: TObject);
    procedure TimerBracketsTimer(Sender: TObject);
    procedure TBXItemTabCopyFNClick(Sender: TObject);
    procedure TBXItemTabCopyFullClick(Sender: TObject);
    procedure TBXItemTabCopyDirClick(Sender: TObject);
    procedure TBXItemSp50Click(Sender: TObject);
    procedure TBXItemTbCloseAllClick(Sender: TObject);
    procedure ecSyncScrollHExecute(Sender: TObject);
    procedure ecSyncScrollVExecute(Sender: TObject);
    procedure TBXItemOShellClick(Sender: TObject);
    procedure ecOnTopExecute(Sender: TObject);
    procedure tbMenuShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure TBXItemEFillBlockClick(Sender: TObject);
    procedure TBXItemEInsTextClick(Sender: TObject);
    procedure TBXItemCtxOpenSelClick(Sender: TObject);
    procedure TBXItemORestoreStylesClick(Sender: TObject);
    procedure TBXItemCtxCustomizeClick(Sender: TObject);
    procedure TreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListClipKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ecToggleFocusTreeExecute(Sender: TObject);
    procedure ecToggleFocusClipExecute(Sender: TObject);
    procedure TBXItemFReopenClick(Sender: TObject);
    procedure ecToggleLineCommentExecute(Sender: TObject);
    procedure ecToggleFocusOutputExecute(Sender: TObject);
    procedure acBackupExecute(Sender: TObject);
    procedure ecToggleStreamCommentExecute(Sender: TObject);
    procedure TBXItemEMoveUpClick(Sender: TObject);
    procedure TBXItemEMoveDnClick(Sender: TObject);
    procedure TBXItemHelpDonateClick(Sender: TObject);
    procedure TBXItemEDelLnClick(Sender: TObject);
    procedure TBXItemECpFNClick(Sender: TObject);
    procedure TBXItemECpFullPathClick(Sender: TObject);
    procedure TBXItemECpDirPathClick(Sender: TObject);
    procedure ecToggleFocusFindResExecute(Sender: TObject);
    procedure TBXItemBarSpellChkClick(Sender: TObject);
    procedure ecSpellCheckExecute(Sender: TObject);
    procedure ecSpellLiveExecute(Sender: TObject);
    procedure TBXItemSpellLiveClick(Sender: TObject);
    procedure acMacroDialogExecute(Sender: TObject);
    procedure acMacro1Execute(Sender: TObject);
    procedure acMacro2Execute(Sender: TObject);
    procedure acMacro3Execute(Sender: TObject);
    procedure acMacro4Execute(Sender: TObject);
    procedure acMacro5Execute(Sender: TObject);
    procedure acMacro6Execute(Sender: TObject);
    procedure acMacro7Execute(Sender: TObject);
    procedure acMacro8Execute(Sender: TObject);
    procedure acMacro9Execute(Sender: TObject);
    procedure TBXItemMacro1Click(Sender: TObject);
    procedure TBXItemMacro2Click(Sender: TObject);
    procedure TBXItemMacro3Click(Sender: TObject);
    procedure TBXItemMacro4Click(Sender: TObject);
    procedure TBXItemMacro5Click(Sender: TObject);
    procedure TBXItemMacro6Click(Sender: TObject);
    procedure TBXItemMacro7Click(Sender: TObject);
    procedure TBXItemMacro8Click(Sender: TObject);
    procedure TBXItemMacro9Click(Sender: TObject);
    procedure TBXSubmenuMacrosPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure ecBkClearAllExecute(Sender: TObject);
    procedure ecBkToggleExecute(Sender: TObject);
    procedure ecBkNextExecute(Sender: TObject);
    procedure ecBkPrevExecute(Sender: TObject);
    procedure ecBkInverseExecute(Sender: TObject);
    procedure ecBkCopyExecute(Sender: TObject);
    procedure ecBkCutExecute(Sender: TObject);
    procedure ecBkDeleteExecute(Sender: TObject);
    procedure ecBkDeleteUnmkExecute(Sender: TObject);
    procedure ecBkPasteExecute(Sender: TObject);
    procedure ecGotoExecute(Sender: TObject);
    procedure ecToggleFocusGroupsExecute(Sender: TObject);
    procedure TBXItemOOValClick(Sender: TObject);
    procedure ListValDblClick(Sender: TObject);
    procedure TBXItemValNavClick(Sender: TObject);
    procedure TBXItemValCopySelClick(Sender: TObject);
    procedure TBXItemValCopyAllClick(Sender: TObject);
    procedure TBXItemValClearClick(Sender: TObject);
    procedure PopupValidatePopup(Sender: TObject);
    procedure ListValKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ecToggleFocusValidateExecute(Sender: TObject);
    procedure TBXItemSessClrClick(Sender: TObject);
    procedure TBXItemFSesSaveClick(Sender: TObject);
    procedure TBXItemFSesCloseClick(Sender: TObject);
    procedure ecRemoveBlanksExecute(Sender: TObject);
    procedure TBXSubmenuItemFNewPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure ecRemoveLinesExecute(Sender: TObject);
    procedure ecTrimLeadExecute(Sender: TObject);
    procedure ecTrimTrailExecute(Sender: TObject);
    procedure ecTrimAllExecute(Sender: TObject);
    procedure ecRemoveDupSpacesExecute(Sender: TObject);
    procedure ecTabToSpExecute(Sender: TObject);
    procedure ecSpToTabExecute(Sender: TObject);
    procedure ecFindClipNextExecute(Sender: TObject);
    procedure ecFindClipPrevExecute(Sender: TObject);
    procedure ecSplit50_50Execute(Sender: TObject);
    procedure ecSplit40_60Execute(Sender: TObject);
    procedure ecSplit60_40Execute(Sender: TObject);
    procedure ecSplit30_70Execute(Sender: TObject);
    procedure ecSplit70_30Execute(Sender: TObject);
    procedure ecSplit20_80Execute(Sender: TObject);
    procedure ecSplit80_20Execute(Sender: TObject);
    procedure acMacroPlayBeforeExecute(Sender: TObject);
    procedure acMacroRepeatExecute(Sender: TObject);
    procedure ecRepeatCmdExecute(Sender: TObject);
    procedure TBXItemRightClipClick(Sender: TObject);
    procedure TBXItemRightMapClick(Sender: TObject);
    procedure ecToggleFocusMapExecute(Sender: TObject);
    procedure ecFindInTreeExecute(Sender: TObject);
    procedure ecFindInTreeNextExecute(Sender: TObject);
    procedure ecFindInTreePrevExecute(Sender: TObject);
    procedure ecTreeNextExecute(Sender: TObject);
    procedure ecTreePrevExecute(Sender: TObject);
    procedure ecReduceBlanksExecute(Sender: TObject);
    procedure ecSplitLeftExecute(Sender: TObject);
    procedure ecSplitRightExecute(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: Char);
    procedure ecFindNextWithExtendExecute(Sender: TObject);
    procedure ecFindPrevWithExtendExecute(Sender: TObject);
    procedure ecFindInListExecute(Sender: TObject);
    procedure ecFindInListNextExecute(Sender: TObject);
    procedure ecFindInListPrevExecute(Sender: TObject);
    procedure TBXItemClipFindClick(Sender: TObject);
    procedure TBXItemValFindClick(Sender: TObject);
    procedure TBXItemOutFindClick(Sender: TObject);
    procedure PopupClipPopup(Sender: TObject);
    procedure TBXItemTreeFindClick(Sender: TObject);
    procedure PopupTreePopup(Sender: TObject);
    procedure TBXItemTreeCollapseClick(Sender: TObject);
    procedure TBXItemTreeExpandClick(Sender: TObject);
    procedure TimerAutoSaveTimer(Sender: TObject);
    procedure acMacro10Execute(Sender: TObject);
    procedure acMacro11Execute(Sender: TObject);
    procedure acMacro12Execute(Sender: TObject);
    procedure acMacro13Execute(Sender: TObject);
    procedure acMacro14Execute(Sender: TObject);
    procedure acMacro15Execute(Sender: TObject);
    procedure acMacro16Execute(Sender: TObject);
    procedure acMacro17Execute(Sender: TObject);
    procedure acMacro18Execute(Sender: TObject);
    procedure acMacro19Execute(Sender: TObject);
    procedure acMacro20Execute(Sender: TObject);
    procedure acMacro21Execute(Sender: TObject);
    procedure acMacro22Execute(Sender: TObject);
    procedure acMacro23Execute(Sender: TObject);
    procedure acMacro24Execute(Sender: TObject);
    procedure acMacro25Execute(Sender: TObject);
    procedure acMacro26Execute(Sender: TObject);
    procedure acMacro27Execute(Sender: TObject);
    procedure acMacro28Execute(Sender: TObject);
    procedure acMacro29Execute(Sender: TObject);
    procedure acMacro30Execute(Sender: TObject);
    procedure TBXItemMacro10Click(Sender: TObject);
    procedure TBXItemMacro11Click(Sender: TObject);
    procedure TBXItemMacro12Click(Sender: TObject);
    procedure TBXItemMacro13Click(Sender: TObject);
    procedure TBXItemMacro14Click(Sender: TObject);
    procedure TBXItemMacro15Click(Sender: TObject);
    procedure TBXItemMacro16Click(Sender: TObject);
    procedure TBXItemMacro17Click(Sender: TObject);
    procedure TBXItemMacro18Click(Sender: TObject);
    procedure TBXItemMacro19Click(Sender: TObject);
    procedure TBXItemMacro20Click(Sender: TObject);
    procedure TBXItemMacro21Click(Sender: TObject);
    procedure TBXItemMacro22Click(Sender: TObject);
    procedure TBXItemMacro23Click(Sender: TObject);
    procedure TBXItemMacro24Click(Sender: TObject);
    procedure TBXItemMacro25Click(Sender: TObject);
    procedure TBXItemMacro26Click(Sender: TObject);
    procedure TBXItemMacro27Click(Sender: TObject);
    procedure TBXItemMacro28Click(Sender: TObject);
    procedure TBXItemMacro29Click(Sender: TObject);
    procedure TBXItemMacro30Click(Sender: TObject);
    procedure TreeFindChange(Sender: TObject; Node: TTreeNode);
    procedure TreeFindDblClick(Sender: TObject);
    procedure TreeFindCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeFindAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TBXItemTreeFindNavClick(Sender: TObject);
    procedure TBXItemTreeFindFindClick(Sender: TObject);
    procedure TBXItemTreeFindCopyToTabClick(Sender: TObject);
    procedure TBXItemTreeFindClearClick(Sender: TObject);
    procedure TBXItemTreeFindCopyToClipClick(Sender: TObject);
    procedure TBXItemTreeFindExpandClick(Sender: TObject);
    procedure TBXItemTreeFindCollapseClick(Sender: TObject);
    procedure TBXItemTreeFindExpandCurClick(Sender: TObject);
    procedure TBXItemCtxFindIDClick(Sender: TObject);
    procedure ecTreeParentExecute(Sender: TObject);
    procedure ecTreeNextBrotherExecute(Sender: TObject);
    procedure ecTreePrevBrotherExecute(Sender: TObject);
    procedure TBXItemRunOpenFileClick(Sender: TObject);
    procedure TBXItemSSelTokenClick(Sender: TObject);
    procedure TBXItemTreeFindCopyToClipNodeClick(Sender: TObject);
    procedure TemplatePopupShow(Sender: TObject);
    procedure TBXItemClipCopyToEdClick(Sender: TObject);
    procedure TBXItemClipCopyToClipClick(Sender: TObject);
    procedure ecReplaceSelFromClipAllExecute(Sender: TObject);
    procedure acRereadOutExecute(Sender: TObject);
    procedure TBXItemLeftTreeClick(Sender: TObject);
    procedure TBXItemLeftProjClick(Sender: TObject);
    procedure ecToggleFocusProjectExecute(Sender: TObject);
    procedure ecToggleFocusMasterSlaveExecute(Sender: TObject);
    procedure ecToggleSlaveExecute(Sender: TObject);
    procedure TBXItemSplitEdHorzClick(Sender: TObject);
    procedure ecRulerExecute(Sender: TObject);
    procedure ecSplitViewsVertHorzExecute(Sender: TObject);
    procedure ecSplitSlaveVertHorzExecute(Sender: TObject);
    procedure ecGotoBkExecute(Sender: TObject);
    procedure TBXItemBkGotoClick(Sender: TObject);
    procedure TBXItemFavAddFileClick(Sender: TObject);
    procedure TBXItemFavManageClick(Sender: TObject);
    procedure acFavManageExecute(Sender: TObject);
    procedure TBXItemCtxAddColorClick(Sender: TObject);
    procedure TbxSubmenuColorsPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemFavAddProjClick(Sender: TObject);
    procedure acFavAddFileExecute(Sender: TObject);
    procedure acFavAddProjExecute(Sender: TObject);
    procedure TBXSubmenuItemFavPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure TBXItemTabAddToProjClick(Sender: TObject);
    procedure TreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TBXItemRightClipsClick(Sender: TObject);
    procedure ecToggleFocusClipsExecute(Sender: TObject);
    procedure TBXItemClipsAddTextClick(Sender: TObject);
    procedure TBXItemClipsEditClick(Sender: TObject);
    procedure TBXItemClipsDirClick(Sender: TObject);
    procedure TBXTabColorChange(Sender: TObject);
    procedure TBXSubmenuTabColorPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemTabColorMiscClick(Sender: TObject);
    procedure TBXItemTabColorDefClick(Sender: TObject);
    procedure ecSmartHlExecute(Sender: TObject);
    procedure TBXItemBkDropPortableClick(Sender: TObject);
    procedure ecDropPortableBkExecute(Sender: TObject);
    procedure ecGotoPortableBkExecute(Sender: TObject);
    procedure acRenameExecute(Sender: TObject);
    procedure TBXItemRunNumConvClick(Sender: TObject);
    procedure ecNumericConverterExecute(Sender: TObject);
    procedure TBXItemEUnindentClick(Sender: TObject);
    procedure ecIndentLike1stExecute(Sender: TObject);
    procedure TBXItemEToggleLineCommentClick(Sender: TObject);
    procedure TBXItemEToggleStreamCommentClick(Sender: TObject);
    procedure TBXItemOOPLogClick(Sender: TObject);
    procedure TBXItemPLogFindClick(Sender: TObject);
    procedure TBXItemPLogClearClick(Sender: TObject);
    procedure TBXItemPLogDeleteClick(Sender: TObject);
    procedure TBXItemPLogCopyAllClick(Sender: TObject);
    procedure TBXItemPLogCopySelClick(Sender: TObject);
    procedure ListPLogKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListPLogDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ecCenterLinesExecute(Sender: TObject);
    procedure TBXItemLeftTabsClick(Sender: TObject);
    procedure ListTabsClick(Sender: TObject);
    procedure ListTabsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListTabsColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListTabsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ecToggleFocusTabsExecute(Sender: TObject);
    procedure TBXItemOEditSynPluginsIniClick(Sender: TObject);
    procedure TBXItemPLogSaveAsClick(Sender: TObject);
    procedure TBXItemTabMoveToWindowClick(Sender: TObject);
    procedure TBXItemTabOpenInWindowClick(Sender: TObject);
    procedure ecEncodeHtmlCharsExecute(Sender: TObject);
    procedure ecSortDialogExecute(Sender: TObject);
    procedure TBXItemSSelBracketsClick(Sender: TObject);
    procedure TimerTreeTimer(Sender: TObject);
    procedure PopupStatusLineEndsPopup(Sender: TObject);
    procedure TBXItemFoldAllClick(Sender: TObject);
    procedure TBXItemUnfoldAllClick(Sender: TObject);
    procedure TBXItemUnfoldLineClick(Sender: TObject);
    procedure TBXItemFoldParentClick(Sender: TObject);
    procedure ecCollapseParentExecute(Sender: TObject);
    procedure ecCollapseWithNestedExecute(Sender: TObject);
    procedure TBXItemFoldWithNestedClick(Sender: TObject);
    procedure TBXItemFoldSelBlockClick(Sender: TObject);
    procedure TBXItemFoldNearestBlockClick(Sender: TObject);
    procedure ecSortAscendingExecute(Sender: TObject);
    procedure ecSortDescendingExecute(Sender: TObject);
    procedure ecSpToTabLeadingExecute(Sender: TObject);
    procedure ecToggleLineCommentAltExecute(Sender: TObject);
    procedure TBXItemEToggleLineCommentAltClick(Sender: TObject);
    procedure TBXSubmenuCtxMorePopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemFoldRangesInSelClick(Sender: TObject);
    procedure TBXItemUnfoldRangesInSelClick(Sender: TObject);
    procedure ecCommandsListExecute(Sender: TObject);
    procedure ecProjectListExecute(Sender: TObject);
    procedure TBXItemCaretsRemove1Click(Sender: TObject);
    procedure TBXItemCaretsRemove2Click(Sender: TObject);
    procedure TBXItemCaretsFromSelLeftClick(Sender: TObject);
    procedure TBXItemCaretsFromSelRightClick(Sender: TObject);
    procedure TBXItemCaretsFromSelClearClick(Sender: TObject);
    procedure TBXItemCaretsExtUpLineClick(Sender: TObject);
    procedure TBXItemCaretsExtDownLineClick(Sender: TObject);
    procedure TBXItemCaretsExtUpPageClick(Sender: TObject);
    procedure TBXItemCaretsExtDownPageClick(Sender: TObject);
    procedure TBXItemCaretsExtUpEndClick(Sender: TObject);
    procedure TBXItemCaretsExtDownEndClick(Sender: TObject);
    procedure TBXItemCaretsFromMarksLeftClick(Sender: TObject);
    procedure TBXItemCaretsFromMarksRightClick(Sender: TObject);
    procedure TBXItemCaretsFromMarksClearClick(Sender: TObject);
    procedure TBXItemEColumnClick(Sender: TObject);
    procedure ecEditColumnExecute(Sender: TObject);
    procedure ecDedupAllExecute(Sender: TObject);
    procedure ecDedupAdjacentExecute(Sender: TObject);
    procedure TBXItemEDedupAllClick(Sender: TObject);
    procedure TBXItemEDedupAdjacentClick(Sender: TObject);
    procedure TBXItemBarDedupAdjClick(Sender: TObject);
    procedure TBXItemBarDedupAllClick(Sender: TObject);
    procedure TBXItemESortDialogClick(Sender: TObject);
    procedure TBXItemESortAscClick(Sender: TObject);
    procedure TBXItemESortDescClick(Sender: TObject);
    procedure TBXItemBarSortDialogClick(Sender: TObject);
    procedure TBXItemBarSortAscClick(Sender: TObject);
    procedure TBXItemBarSortDescClick(Sender: TObject);
    procedure TBXItemBarCaseUpperClick(Sender: TObject);
    procedure TBXItemBarCaseLowerClick(Sender: TObject);
    procedure TBXItemBarCaseTitleClick(Sender: TObject);
    procedure TBXItemBarCaseSentClick(Sender: TObject);
    procedure TBXItemBarCaseInvertClick(Sender: TObject);
    procedure TBXItemECaseUpperClick(Sender: TObject);
    procedure TBXItemECaseLowerClick(Sender: TObject);
    procedure TBXItemECaseTitleClick(Sender: TObject);
    procedure TBXItemECaseSentClick(Sender: TObject);
    procedure TBXItemECaseInvertClick(Sender: TObject);
    procedure TBXItemEAlignWithSepClick(Sender: TObject);
    procedure TBXItemERemBlanksClick(Sender: TObject);
    procedure TBXItemEReduceBlanksClick(Sender: TObject);
    procedure TBXItemETrimLeadClick(Sender: TObject);
    procedure TBXItemETrimTrailClick(Sender: TObject);
    procedure TBXItemETrimAllClick(Sender: TObject);
    procedure TBXItemERemDupSpClick(Sender: TObject);
    procedure TBXItemETabToSpClick(Sender: TObject);
    procedure TBXItemESpToTabClick(Sender: TObject);
    procedure TBXItemESpToTabLeadClick(Sender: TObject);
    procedure TBXItemECenterLinesClick(Sender: TObject);
    procedure ecAlignWithSepExecute(Sender: TObject);
    procedure TBXItemEJoinClick(Sender: TObject);
    procedure TBXItemESplitClick(Sender: TObject);
    procedure TBXItemECopyLineClick(Sender: TObject);
    procedure TBXItemECutLineClick(Sender: TObject);
    procedure TBXItemECopyAppClick(Sender: TObject);
    procedure TBXItemECutAppClick(Sender: TObject);
    procedure TBXItemEIndentClick(Sender: TObject);
    procedure TBXItemEIndentLike1stClick(Sender: TObject);
    procedure TBXItemECommClick(Sender: TObject);
    procedure TBXItemEUncommClick(Sender: TObject);
    procedure TBXItemTabToggleSplitClick(Sender: TObject);
    procedure TBXItemCtxCopyClick(Sender: TObject);
    procedure TBXItemCtxCutClick(Sender: TObject);
    procedure TBXItemCtxPasteClick(Sender: TObject);
    procedure TBXItemCtxDelClick(Sender: TObject);
    procedure TBXItemCtxSelectAllClick(Sender: TObject);
    procedure TBXItemERedoClick(Sender: TObject);
    procedure ecToggleShowGroup2Execute(Sender: TObject);
    procedure PluginACPAfterComplete(Sender: TObject;
      const Item: WideString);
    procedure TBXItemSSelExtendClick(Sender: TObject);
    procedure TBXItemTreeExpandAllClick(Sender: TObject);
    procedure TBXItemTreeCollapseAllClick(Sender: TObject);
    procedure TBXItemTreeLevel2Click(Sender: TObject);
    procedure TBXItemTreeLevel3Click(Sender: TObject);
    procedure TBXItemTreeLevel4Click(Sender: TObject);
    procedure TBXItemTreeLevel5Click(Sender: TObject);
    procedure TBXItemTreeLevel6Click(Sender: TObject);
    procedure TBXItemTreeLevel7Click(Sender: TObject);
    procedure TBXItemTreeLevel8Click(Sender: TObject);
    procedure TBXItemTreeLevel9Click(Sender: TObject);
    procedure ecReverseLinesExecute(Sender: TObject);
    procedure TBXItemEReverseClick(Sender: TObject);
    procedure ecShuffleLinesExecute(Sender: TObject);
    procedure TBXItemEShuffleClick(Sender: TObject);
    procedure TBXItemFoldLevel2Click(Sender: TObject);
    procedure TBXItemFoldLevel3Click(Sender: TObject);
    procedure TBXItemFoldLevel4Click(Sender: TObject);
    procedure TBXItemFoldLevel5Click(Sender: TObject);
    procedure TBXItemFoldLevel6Click(Sender: TObject);
    procedure TBXItemFoldLevel7Click(Sender: TObject);
    procedure TBXItemFoldLevel8Click(Sender: TObject);
    procedure TBXItemFoldLevel9Click(Sender: TObject);
    procedure TBXItemBarCommClick(Sender: TObject);
    procedure TBXItemBarUncomClick(Sender: TObject);
    procedure TBXItemTUser1Click(Sender: TObject);
    procedure TBXItemTUser2Click(Sender: TObject);
    procedure TBXItemTUser3Click(Sender: TObject);
    procedure TBXItemOToolbar1Click(Sender: TObject);
    procedure TBXItemOToolbar2Click(Sender: TObject);
    procedure TBXItemOToolbar3Click(Sender: TObject);
    procedure ecExtractDupsCaseExecute(Sender: TObject);
    procedure ecExtractDupsNoCaseExecute(Sender: TObject);
    procedure TBXItemEExtractDupCaseClick(Sender: TObject);
    procedure TBXItemEExtractDupNoCaseClick(Sender: TObject);
    procedure ecNonPrintSpacesExecute(Sender: TObject);
    procedure ecNonPrintEolExecute(Sender: TObject);
    procedure ecNonPrintBothExecute(Sender: TObject);
    procedure TBXItemCtxPasteAsColumnClick(Sender: TObject);
    procedure TBXItemCtxPasteBkmkLinesClick(Sender: TObject);
    procedure ecNonPrintEolDetailsExecute(Sender: TObject);
    procedure TBXItemOHideItemsClick(Sender: TObject);
    procedure TBXItemOEditSynIniClick(Sender: TObject);
    procedure acOpenBySelectionExecute(Sender: TObject);
    procedure StatusItemCaretClick(Sender: TObject);
    procedure StatusItemEncClick(Sender: TObject);
    procedure StatusItemEndsClick(Sender: TObject);
    procedure StatusItemLexerClick(Sender: TObject);
    procedure StatusItemCharClick(Sender: TObject);
    procedure StatusItemROClick(Sender: TObject);
    procedure StatusItemWrapClick(Sender: TObject);
    procedure StatusItemSelModeClick(Sender: TObject);
    procedure StatusItemZoomClick(Sender: TObject);
    procedure TBXTabColorGetColor(Sender: TObject; ACol, ARow: Integer;
      var Color: TColor; var Name: WideString);
    procedure TBXTabColorCellClick(Sender: TObject; ACol, ARow: Integer;
      var Allow: Boolean);
    procedure StatusItemCaretDrawHint(Sender: TObject;
      AHintBitmap: TBitmap; var AHint: WideString;
      var PaintDefault: Boolean);
    procedure TBXSubmenuItemFRecentsPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXSubmenuItemSessPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXMRUListItemFNewClick(Sender: TObject;
      const Filename: WideString);
    procedure StatusResize(Sender: TObject);
    procedure acSetupLexerLibExecute(Sender: TObject);
    procedure TbxItemTabReloadClick(Sender: TObject);
    procedure TBXSubmenuViewToolbarsPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TimerMinimapTimer(Sender: TObject);
    procedure ecEncodeHtmlChars2Execute(Sender: TObject);
    procedure TbxItemMenuXClick(Sender: TObject);
    procedure TbxItemMenuXXClick(Sender: TObject);
    procedure TBXItemProjOpenClick(Sender: TObject);
    procedure TBXItemProjAddFileClick(Sender: TObject);
    procedure TBXSubmenuItemProjRecentsPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXMRUListItem_ProjectsClick(Sender: TObject;
      const Filename: WideString);
    procedure TBXItemProjRecentClearClick(Sender: TObject);
    procedure TBXItemProjGotoClick(Sender: TObject);
    procedure TBXItemProjNewClick(Sender: TObject);
    procedure PluginACPShow(Sender: TObject);
    procedure plTreeDockChanged(Sender: TObject);
    procedure TBXItemProjAddAllFilesClick(Sender: TObject);
    procedure TbxItemProjSaveClick(Sender: TObject);
    procedure TbxTabConsoleClick(Sender: TObject);
    procedure edConsoleKeyPress(Sender: TObject; var Key: Char);
    procedure ecToggleFocusConsoleExecute(Sender: TObject);
    procedure PythonEngine1BeforeLoad(Sender: TObject);
    procedure edConsoleKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MemoConsoleKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PythonEngine1AfterInit(Sender: TObject);
    procedure PythonModuleInitialization(Sender: TObject);
    procedure PythonGUIInputOutput1SendUniData(Sender: TObject;
      const Data: WideString);
    procedure TbxItemHelpPyDirClick(Sender: TObject);
    procedure PythonGUIInputOutput1ReceiveUniData(Sender: TObject;
      var Data: WideString);
    procedure MemoConsoleDblClick(Sender: TObject);
    procedure TbxItemRunSnippetsClick(Sender: TObject);
    procedure TbxItemRunNewSnippetClick(Sender: TObject);
    procedure TbxItemTreeSortedClick(Sender: TObject);
    procedure TbxItemPanelTitleBarClick(Sender: TObject);
    procedure PopupPanelTitlePopup(Sender: TObject);
    procedure TBXItemMarkGoLastClick(Sender: TObject);
    procedure TBXItemMarkClearClick(Sender: TObject);
    procedure TbxItemWinSplitHClick(Sender: TObject);
    procedure TbxItemWinSplitVClick(Sender: TObject);
    procedure ecToggleProjPreviewExecute(Sender: TObject);
    procedure acExportHTMLBeforeExecute(Sender: TObject);
    procedure TBXSubmenuBookmarksPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TbxItemPreCopyClick(Sender: TObject);
    procedure TbxItemPreSelectClick(Sender: TObject);
    procedure TbxItemPreZoom25Click(Sender: TObject);
    procedure TbxItemPreZoom50Click(Sender: TObject);
    procedure TbxItemPreZoom75Click(Sender: TObject);
    procedure TbxItemPreZoom100Click(Sender: TObject);
    procedure TbxItemPreEditClick(Sender: TObject);
    procedure TbxItemPreZoomOtherClick(Sender: TObject);
    procedure PopupPreviewEditorPopup(Sender: TObject);
    procedure TbxItemGroupOneClick(Sender: TObject);
    procedure TbxItemGroup2HClick(Sender: TObject);
    procedure TbxItemGroup2VClick(Sender: TObject);
    procedure TbxItemGroup3HClick(Sender: TObject);
    procedure TbxItemGroup3VClick(Sender: TObject);
    procedure TbxItemGroup3as1p2Click(Sender: TObject);
    procedure TbxItemGroup4HClick(Sender: TObject);
    procedure TbxItemGroup4VClick(Sender: TObject);
    procedure TbxItemGroup4GridClick(Sender: TObject);
    procedure TbxItemGroup6GridClick(Sender: TObject);
    procedure TBXSubmenuGroupsPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TbxItemToGroup1Click(Sender: TObject);
    procedure TBXSubmenuItemToGroupPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure TBXItemTabCloseOthersAllGroupsClick(Sender: TObject);
    procedure acCloseOthersAllGroupsExecute(Sender: TObject);
    procedure TbxTabBookmarksClick(Sender: TObject);
    procedure ListBookmarksDblClick(Sender: TObject);
    procedure ListBookmarksKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ecToggleFocusBookmarksExecute(Sender: TObject);
    procedure PopupPluginsLogPopup(Sender: TObject);
    procedure TbxItemWinExplorerClick(Sender: TObject);
    procedure TbxItemWinFtpClick(Sender: TObject);
    procedure TBXItemTabCloseRighterClick(Sender: TObject);
    procedure TBXItemTabCloseLefterClick(Sender: TObject);
    procedure ecReplaceInProjectExecute(Sender: TObject);
    procedure ecPreviewActionNewExecute(Sender: TObject);
    procedure TBXItemBarPreviewClick(Sender: TObject);
    procedure TBXItemFPreviewClick(Sender: TObject);
    procedure acSetupLexerNewExecute(Sender: TObject);
    procedure ecPageSetupActionNewExecute(Sender: TObject);
    procedure edQsKeyPress(Sender: TObject; var Key: Char);
    procedure TBXItemECaseRandomClick(Sender: TObject);
    procedure TBXItemBarCaseRandomClick(Sender: TObject);
    procedure acRestartExecute(Sender: TObject);
    procedure TBXItemTreeFindPreviewClick(Sender: TObject);
    procedure TBXItemEDedupAllOrigClick(Sender: TObject);
    procedure ecDedupAllAndOrigExecute(Sender: TObject);
    procedure ecExtractUniqExecute(Sender: TObject);
    procedure TBXItemEExtractUniqClick(Sender: TObject);
    procedure TBXItemBarDedupAndOrigClick(Sender: TObject);
    procedure TbxSubmenuWebPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure WebSearchClick(Sender: TObject);
    procedure TbxItemTabSaveClick(Sender: TObject);
    procedure TbxItemTabSaveAsClick(Sender: TObject);
    procedure TbxItemAddonsInstallClick(Sender: TObject);
    procedure TbxItemAddonsRemoveClick(Sender: TObject);
    procedure TbxItemAddonsEditClick(Sender: TObject);
    procedure TbxItemAddonsSaveClick(Sender: TObject);
    procedure TbxItemAddonsUpdateClick(Sender: TObject);
    procedure PopupStatusEncConvertPopup(Sender: TObject);
    procedure TbxItemAddonsConfigClick(Sender: TObject);
    procedure TBXItemClipDeleteSelClick(Sender: TObject);
    procedure acMacroRecordAfterExecute(Sender: TObject);

  private
    cStatLine,
    cStatCol,
    cStatSelLines,
    cStatSelCols,
    cStatSelChars,
    cStatTLines,
    cStatTChars,
    cStatFSize,
    cStatFDate,
    cStatCarets,
    cStatCaretsTopLn,
    cStatCaretsBotLn: Widestring;

    TabsLeft,
    TabsRight,
    TabsOut: TATTabs;
    TabSwitchers: array[TATGroupsNums] of TTabSwitcher;
    FIcons: string;
    FListSnippets: TList;
    FListLexersSorted: TTntStringList;
    FUserToolbarCommands: TTntStringList;
    FInitialDir: Widestring;
    FInitialKeyCount: Integer;
    FLastUntitled: Integer;
    FLastOnContinueCheck: DWORD;
    FLastCmdId: integer;
    FLastCmdData: string;
    FLastCmdCount: integer;
    FLastCmdPlaying: boolean;
    FLastCmdBreak: boolean;
    FLastMacro: integer;
    FSessionFN: string;
    FProjectIniting: boolean;
    FProjectFreeing: boolean;

    FSpellMenuCaption: Widestring;
    FSpellMenuTag: integer;
    {$ifdef SPELL}
    FSpell: TLiveAddictSpellBase;
    FSpellPos: Integer;
    FSpellChecking: boolean;
    {$endif}

    FPluginsPanel: TPluginList_Panel;
    FPluginsFindid: TPluginList_Findid;
    FPluginsCommand: TPluginList_Command;
    FPluginsEvent: TPluginList_Event;
    FPluginsAcp: TPluginList_Acp;

    FListNewDocs: TTntStringList; //filenames of newdoc-templates
    FListFiles: TTntStringList; //filenames list of mass search/replace operation
    FPanelDrawBusy: boolean;
    FSyncBusy: boolean;
    FSelBlank: boolean; //selection is blank (for Smart Hilite)
    FFullscreen: boolean; //full-screen
    FOnTop: boolean; //always-on-top
    FBoundsRectOld: TRect;

    //forms
    fmNumConv: TfmNumConv;
    fmClip: TfmClip;
    fmClips: TfmClips;
    fmMap: TfmMap;
    fmProj: TfmProj;
    fmSR: TfmSR;

    //original options values
    orig_Tree,
    orig_Clip,
    orig_Out,
    orig_NFold,
    orig_Wrap,
    orig_LNum,
    orig_NPrint,
    orig_Ruler: boolean;
    orig_TabRight: Integer;
    orig_TabLeft: Integer;
    orig_TabOut: Integer;
    orig_TabsSort: integer;
    orig_ListTabsCols: string;
    orig_ListBkmkCols: string;

    //auto-complete lists
    FAcpIntHtml,
    FAcpIntCss,
    FAcpList_Display, //ACP: display list (in form "\s1\...\s2\...")
    FAcpList_Items, //ACP: names with brackets+parameters
    FAcpList_Hints, //ACP: hints (in form "(param1; param2)")
    FAcpList_Desc: //ACP: descriptions (text after "|")
      TStringList;

    FTabLeft,
    FTabOut,
    FTabRight: Integer;
    FTreeRoot: TTntTreeNode; //root tree node of last find result
    FListResFN,              //current filename for mass search/replace operation
    FListResFN_Prev: Widestring; //previous filename for mass search/replace
    FOutItem: Integer; //ListOut item index for right-click
    FOutVisible: boolean; //Visible state for Output panel
    FPrevCaretPos: Integer; //saved caret pos before executing "Select brackets"

    FAcpLexer: string; //ACP list was loaded for this lexer
    FAcpAgain: boolean; //ACP need to show again after closing (for html/css)
    FAcpCss: boolean; //ACP called for CSS lexer
    FAcpHtm: boolean; //ACP called for HTML lexer
    FAcpHtmTags: boolean; //ACP shows html tags, not attribs
    FAcpHtmClosing: boolean; //ACP called for html closing tag </tag>
    FAcpHtmSpaceAdded: boolean; //added space on ACP call

    QuickView: boolean;    //QuickView mode for TotalCmd plugin
    FUpdatePluginsLang: boolean; //need to update plugins' language ASAP
    FNeedRepaint: boolean;   //need full repaint ASAP
    FEnableRepaint: boolean; //enable repaint, it's set after 500ms

    FToolbarMoved: boolean;  //set when toolbars are moved
    FPopupUrl: string;       //current URL for editor popup menu
    FPopupColor: integer;    //current color-id-in-text for editor popup menu
    FCurrPluginAcpStartPos: TPoint; //auto-complete popup position, for cActionSuggestCompletion
    FCurrSelState: TSynSelState; //current selection state (stream, column, carets etc)
    FCurrTheme: string;      //current SpTBX theme
    FCurrDiffScrollY: Integer; //diff between 1st view editor TopLine and 2nd view editor TopLine

    FDialogFFiles_Find,       //last values of "Find in files" dialog fields
    FDialogFFiles_Replace,
    FDialogFFiles_MaskInc,
    FDialogFFiles_MaskExc,
    FDialogFFiles_Dir: Widestring;
    FDialogFFiles_Left,
    FDialogFFiles_Top: Integer;

    FMenuItems: array of  //array of menu-items ids for SynHide.ini
      record Id: string; Item: TComponent; end;
    FMenuItem_Colors_Clear, //menu-items (3) in "Recent colors" menu
    FMenuItem_Colors_Save,
    FMenuItem_Colors_Open: TSpTbxItem;

    FinderPro: TGauge;  //current TGauge for progress showing
    FinderProNum: integer; //previous "NN%" progress value

    //frame related-----------------------------------------
    FCurrentEditor: TSyntaxMemo;
    FClickedFrame: TEditorFrame;
    procedure SetCurrentEditor(Value: TSyntaxMemo);
    function GetFrameCount: integer;
    function GetFrameAllCount: integer;
    function GetFrames(Index: integer): TEditorFrame;
    function GetFramesAll(Index: integer): TEditorFrame;
    procedure SetCurrentFrame(Frame: TEditorFrame);
    function GetCurrentFrame: TEditorFrame;
    function GetCurrentFrameInPages(Pages: TATPages): TEditorFrame;
    procedure FrameSaveState(Sender: TObject);
    function MsgConfirmSaveFrame(Frame: TEditorFrame; CanCancel: boolean=true): TModalResult;
    procedure InitFrameTab(Frame: TEditorFrame);
    function SaveFrame(Frame: TEditorFrame; PromtDialog: Boolean): boolean;
    function OppositeFrame: TEditorFrame;
    function IsFramePropertiesStringForFilename(const fn: Widestring; const Str: string): boolean;
    function FrameGetPropertiesString(F: TEditorFrame): string;
    procedure FrameSetPropertiesString(F: TEditorFrame; const Str: string; EncodingOnly: boolean);
    procedure FocusFrame(Frame: TEditorFrame);
    //frame related------------------------------

    //private methods
    procedure DoDelayedCommandAny(Command: Integer);
    procedure DoDelayedCommandWithClose(Command: Integer);
    function ListTab_FrameIndex: integer;
    procedure SetListTabsColumns(const S: string);
    function GetListTabsColumns: string;
    function GetListBkmkColumns: string;
    procedure SetListBkmkColumns(const S: string);

    //plugins related----------------------------
    procedure DoPlugin_RefreshFiles(const fn: Widestring);
    procedure DoPlugin_RefreshLang;
    procedure DoPlugin_SaveFtpFile(F: TEditorFrame);
    procedure DoPlugin_SetColors(Index: Integer);
    function DoPlugin_OpenPanelByName(const AName: string): Integer;
    function DoPlugin_OpenFavorite(const AFileName: Widestring): boolean;
    procedure DoPlugin_Show(N: Integer);
    
    procedure DoPlugin_LoadPanel(Index: Integer);
    procedure DoPlugin_LoadGotoDef(Index: Integer);
    procedure DoPlugin_LoadAction(
      const AFileName: string;
      const AActionName: Widestring;
      P1, P2, P3, P4: Pointer);
    function DoPlugin_LoadGetString(
      const AFileName: string;
      const AActionName: Widestring): Widestring;

    function DoPlugin_PanelHandleToIndex(AHandle: Pointer): Integer;
    function DoPlugin_PanelFN(Index: Integer): Widestring;
    function DoPlugin_PanelCaption(Index: Integer): Widestring;
    procedure DoPlugin_PanelTabClick(N: Integer);
    procedure DoPlugin_CommandClick(Sender: TObject);
    procedure DoPlugin_AddMenuItem(
      ASubmenu: TSpTbxSubmenuitem;
      const SKey: Widestring; NIndex, NCommandId: Integer);

    procedure DoPlugins_Repaint;
    procedure DoPlugins_Close;
    procedure DoPlugins_Resize;
    procedure DoPlugins_Test;

    procedure DoPlugins_LoadAll;
    procedure DoPlugins_LoadPanels(const fn_plug_ini: string);
    procedure DoPlugins_LoadGotoDef(const fn_plug_ini: string);
    procedure DoPlugins_LoadAutoComplete(const fn_plug_ini: string);
    procedure DoPlugins_LoadCommands(const fn_plug_ini: string);
    procedure DoPlugins_LoadEvents(const fn_plug_ini: string);
    procedure DoPlugins_InitTabs;
    procedure DoPlugins_PreinstallPlugin(const AId, fn_inf: string; AIsPanelPlugin: boolean);
    procedure DoPlugins_PreinstallDefaults;
    //-------------------------------------------
    //
    procedure DoCheckIfBookmarkSetHere(Ed: TSyntaxMemo; NPos: Integer);
    function SGetFrameIndexFromPrefixedStr(const InfoFN: Widestring): Integer;
    function SGetTabPrefix: Widestring;

    procedure DoFindDialog_ReplaceAllInCurrentTab;
    procedure DoFindDialog_ReplaceAllInAllTabs(var AFilesReport: Widestring);
    procedure DoFindDialog_FindNext;
    procedure DoFindDialog_CountAllInCurrentTab;
    procedure DoFindDialog_ReplaceOrSkip(ADoReplace, AGotoNext: boolean);
    procedure DoFindDialog_FindAllInAllTabs;
    procedure DoFindDialog_FindAllInCurrentTab(AWithBkmk, ASelectResults: boolean);

    procedure DoFindInFiles_Dialog(AInProject: boolean);
    function DoFindInFiles_InputData(
      AInProject: boolean;
      AError: TSynFindInFilesError;
      var D: TSynFindInFilesData): TModalResult;
    function DoFindInFiles_GetFileList(
      FListFiles: TTntStringList;
      const SDir, SMaskInc, SMaskExc: Widestring;
      bSubDirs, bNoRO, bNoHidFiles, bNoHidDirs, bNoBinary: boolean;
      ASortMode: TSynFileSort;
      AInProject: boolean): boolean;
    procedure DoFindInFiles_FindAction(
      const ADir: Widestring;
      AOutAppend, InOEM, InUTF8, InUTF16: boolean);
    function DoFindInFiles_ReplaceAction(
      const ADir: Widestring;
      AOutAppend: boolean): boolean;

    procedure DoAddFav(const fn: Widestring);
    procedure NumConvInsert(Sender: TObject; const S: string; Typ: TSynNumType);
    procedure DoGetCommentProps(const Lexer: string;
      UseDefault: boolean;
      var sStart, sEnd: string; var IsMultiLine: boolean);
    function GetTabColors: Widestring;
    procedure SetTabColors(S: Widestring);
    property TabColorsString: Widestring read GetTabColors write SetTabColors;
    procedure DoSetTabColorValue(NColor: TColor);
    procedure DoSetTabColorIndex(NIndex: Integer);
    procedure DoSetTabColorIndex_Current(NIndex: Integer);
    procedure ClipsInsert(Sender: TObject; const AText: Widestring; AIsSnippet: boolean);
    procedure ClipsInsPress(Sender: TObject);
    function IsProgressNeeded(Ed: TSyntaxMemo): boolean;
    function IsProgressStopped(const NDoneSize, NTotalSize: Int64): boolean;
    procedure DoProgressShow(AMode: TProgressType = proFindText);
    procedure DoProgressHide;

    procedure ProjKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ProjOpen(Sender: TObject);
    procedure ProjClose(Sender: TObject);
    procedure ProjGotoFile(Sender: TObject);
    procedure ProjLoadMRU(List: TSynMruList);
    procedure ProjUpdateMRU(List: TSynMruList);
    procedure ProjFileOpen(Sender: TObject; Files: TTntStrings);
    procedure ProjAddEditorFile(Sender: TObject; Files: TTntStrings);
    procedure ProjAddEditorFiles(Sender: TObject; Files: TTntStrings);
    procedure ProjGetLexer(Sender: TObject; Files: TTntStrings);
    procedure ProjGetLexers(Sender: TObject; Files: TTntStrings);
    procedure ProjGetWorkDir(Sender: TObject; Files: TTntStrings);
    procedure ProjGetProjDir(Sender: TObject; Files: TTntStrings);
    procedure ProjSetProjDir(Sender: TObject; Files: TTntStrings);

    function GetRecentColors: string;
    procedure SetRecentColors(const Str: string);
    property RecentColorsStr: string read GetRecentColors write SetRecentColors;
    procedure RecentColorClick(Sender: TObject);
    procedure RecentColorOpen(Sender: TObject);
    procedure RecentColorSave(Sender: TObject);
    procedure DoInitRecentColorsMenu;
    procedure DoDeleteRecentColor(N: Integer);
    function SynBorderStyle: TBorderStyle;
    function SynBorderStyleEditor: TBorderStyle;
    function DoAutoCloseBracket(ch: Widechar): boolean;
    function DoAutoCloseTag: boolean;
    function DoLoadStringFromIni(const fn: string): string;
    procedure DoSaveStringToIni(const fn: string; const Str: string);

    //private UpdateNNN
    procedure UpdateBusyIco;
    procedure UpdateFrameLineEnds(F: TEditorFrame; AFormat: TTextFormat; AManual: boolean);
    procedure UpdateToolbarItemAction(Item: TTBCustomItem; const SCmd: string);
    procedure UpdateNewDocMenu();
    procedure UpdateTreeProps;
    procedure UpdateTitle(Sender: TFrame);
    procedure UpdateAcp(const Lexer: string);
    procedure UpdateTools;
    procedure UpdatePanelOut(n: TSynTabOut);
    procedure UpdatePanelLeft(n: TSynTabLeft);
    procedure UpdatePanelRight(n: TSynTabRight);
    procedure UpdateEncMenu(M: TObject; AConvEnc: boolean = false);
    procedure UpdateBookmarkMenus;
    procedure UpdateShortcuts;
    procedure UpdateLang;
    procedure UpdateSpellLang;
    procedure UpdateEditorCaret(Ed: TSyntaxMemo);
    procedure UpdateEditorNonPrinted(Ed: TSyntaxMemo);
    procedure UpdateLexList;
    procedure UpdateStatusbarLineEnds;
    procedure UpdateStatusbarTabsize;
    procedure UpdateStatusbarEnc(F: TEditorFrame);

    procedure UpdateNewFrame(F: TEditorFrame);
    procedure UpdateFrameEnc(Frame: TEditorFrame);
    procedure UpdateFrameSpell(Frame: TEditorFrame; UpdFlag: boolean = true);
    procedure UpdateFrameZoom(F: TEditorFrame);
    procedure UpdateFrameMicroMap(F: TEditorFrame);
    procedure UpdateClickedFrame;

    procedure UpdateLexer;
    procedure UpdateFormEnabled(En: boolean);
    procedure UpdatePanelOutFromList(List: TWideStringList);
    procedure UpdateRecentsOnClose;
    procedure UpdateColorHint(AClearHint: boolean = true);
    procedure UpdateListTabs;
    procedure UpdateTreeFind_Initial(AStr: Widestring; const ADir: Widestring; AInTabs: boolean = false);
    procedure UpdateTreeFind_Results(AStr: Widestring; const ADir: Widestring; AStopped: boolean; AInTabs: boolean = false);
    procedure UpdateTreeFind_ReplaceResults(const ANodeText: Widestring; ANumFiles, ANumItems: integer; AStopped: boolean);
    procedure UpdateMacroKeynames;

    function SStatusText(Ed: TSyntaxMemo): Widestring;
    function SStatusCharInfo(Ed: TSyntaxMemo): Widestring;
    function SStatusHint(state: TSynSelState): Widestring;

    procedure DoTool_Run(const ATool: TSynTool);
    procedure DoTool_OnClick(Sender: TObject);
    procedure DoTool_HandleOutput(const ft: Widestring; const ATool: TSynTool);
    procedure DoTool_Update(T: TSpTbxItem; Id: integer; ACtxMenu: boolean);
    procedure DoTool_ReplaceMacro(var Str: Widestring; const StrId: string; ViewId: TSynGroupId);
    procedure DoTool_ReplaceFolderMacros(var S: Widestring);
    procedure DoTool_ReplaceAllMacros(var S: Widestring; const Dir: WideString);

    procedure TreeFind_ShowPreview;
    procedure TreeFind_GetItemInfo(var AFilename: Widestring; var ALineNum, AColNum, ALen: Integer);
    function IsProjectEmpty: boolean;
    function IsShowColor(s: string; var NColor, NColorText: TColor): boolean;
    procedure GetTabName(APagesNumber, ATabIndex: Integer; var AName, AFN, ALex: Widestring);
    function GetAcpFN(const LexerName: string): string;
    function GetSpecialHiliteFN(const Id: string): string;
    function GetHtmlListFN: string;
    function GetCssListFN: string;
    function GetHtmlTabbingFN: string;
    function GetLexerComment(const Lexer: string): string;

    function CurrentLexer: string;
    function CurrentLexerForFile: string;
    function DoSnippetTabbing: boolean;
    function DoSmartTagTabbing: boolean;
    procedure DoSmartHilite;
    procedure DoSmartHiliteOnClick;
    procedure DoHandleLastCmd(Command: integer; Data: pointer);

    procedure MsgFound;
    procedure MsgColorBad(const s: string);
    procedure MsgColorOK(const s: string);
    procedure MsgNoSelection;
    procedure MsgNoSelectionForHelp;
    procedure MsgNoRun(const fn: Widestring);
    procedure MsgNoFile(const fn: Widestring);
    procedure MsgNoDir(const fn: Widestring);
    procedure MsgNeedProject;
    procedure MsgEmptyMacro(const s: Widestring);
    procedure MsgDelLines(N: integer);
    procedure MsgDoneLines(N: integer);
    procedure MsgTabbing(const s: Widestring);

    procedure DoCopyFindResultToTab(ALastSearch, AFilesOnly: boolean;
      AToClip: boolean = false);
    procedure DoCopyFindResultToList(ARootNode: TTntTreeNode;
      L: TWideStringList; AFilesOnly: boolean);
    procedure DoCopyFindResultNode;

    function SFindResPrefix(LineNum: integer): Widestring;
    function IsListboxFocused: boolean;
    function IsTreeviewFocused: boolean;
    function CurrentListbox: TCustomListbox;
    function CurrentTreeview: TCustomTreeView;
    procedure DoFrameReloadInt(F: TEditorFrame);
    procedure DoFrameReloadWrapper(F: TEditorFrame);
    function ShowGotoForm(
      var ALine, ACol: integer;
      var AExtSel: boolean;
      var AMode: TSynGotoMode;
      var ABkNum: integer): Boolean;
    //function IsSearchEditFocused: boolean;
    //function IsNumConvEditFocused: boolean;
    //function IsProjPreviewFocused: boolean;
    procedure DoTreeJump(Mode: TSynGotoTree);
    procedure SyncTree;
    procedure SyncMapData;
    procedure SyncMapPos;
    procedure MapClick(Sender: TObject);
    procedure DoClearSearchMarks(Ed: TSyntaxMemo);
    procedure DoFixReplaceCaret(Ed: TSyntaxMemo);
    function FCanUseLexer(const fn: Widestring): boolean;
    procedure SpellCopyClick(Sender: TObject);
    procedure SpellCutClick(Sender: TObject);
    procedure SpellPasteClick(Sender: TObject);
    procedure SpellItemClick(Sender: TObject);
    procedure SpellPopupDoMenu(Sender, Menu: TObject; XPos,
      YPos: Integer; var PopupAction: Integer; var PopupWord: string);
    procedure SpellPopupAddMenuItem(Sender, Menu,
      SubMenu: TObject; Caption: string; Enable, HasChildren: Boolean;
      Tag: Integer; var MenuItem: TObject);
    procedure SpellPopupCreateMenu(Sender: TObject;
      Owner: TComponent; var PopupMenu: TObject);
    procedure SpellDialogShow(Sender: TObject);
    procedure SpellPositionDialog(Sender: TObject);

    function SNewDocName(const fn: Widestring): string;
    procedure DoNewDocClick(Sender: TObject);
    procedure DoNewDocFolderClick(Sender: TObject);
    procedure DoTabIndexClick(n: integer);
    procedure DoRtTabIndexClick(n: integer);

    procedure DoBkDelete(Ed: TSyntaxMemo; DelUnmarked: boolean);
    procedure DoBkNext(Ed: TSyntaxMemo; Next: boolean);
    procedure DoDeleteLine(Ed: TSyntaxMemo; NLine: integer; ForceUndo: boolean = false);
    procedure DoReplaceLine(Ed: TSyntaxMemo; NLine: integer; const S: Widestring; ForceUndo: boolean = false);

    procedure InitGroups;
    procedure InitSpell;
    function DoClipItem: Widestring;
    //procedure DoClipsItemCopy;
    procedure DoClipItemCopy;
    procedure DoClipItemIns;
    procedure DoBackupLexerStyles(ALexer: TSyntAnalyzer);
    procedure DoAcpCss(List, Display: ecUnicode.TWideStrings);
    procedure DoAcpHtm(List, Display: ecUnicode.TWideStrings);
    procedure DoAcpHtmForTag(const STag, SAtr: string; List, Display: ecUnicode.TWideStrings);
    procedure DoAcpFromFile(List, Display: ecUnicode.TWideStrings);
    procedure DoAcpCommand;
    function DoAcpFromPlugins(const AAction: PWideChar): Widestring;
    procedure DoInsertTextDialog;
    procedure DoFillBlock;
    //procedure DoRepaintTBs;
    //procedure DoRepaintTBs2;
    procedure DoSyncScroll(EdSrc: TSyntaxMemo);
    function DoCloseTabs(Id: TATTabCloseId; AForPopupMenu: boolean): boolean;
    procedure DoMoveTabToWindow(Frame: TEditorFrame; AndClose: boolean);
    function LastDir: Widestring;
    function LastDir_UntitledFile: Widestring;
    procedure SaveLastDir(const FN, Filter: Widestring; FilterIndex: integer);
    procedure SaveLastDir_Session(const FN: Widestring);
    procedure SaveLastDir_UntitledFile(const FN: Widestring);
    procedure SetFullscreen(AValue: boolean);
    procedure SetOnTop(V: boolean);
    procedure DoBracketsHilite(Ed: TSyntaxMemo);
    procedure DoListCopy(Sender: TTntListbox);
    procedure DoListCopyAll(Sender: TTntListbox);
    procedure DoHandleKeysInPanels(var Key: Word; Shift: TShiftState);
    function DoNavigate_ListOut(const Str: Widestring): boolean;
    function DoNavigate_ListVal(const Str: Widestring): boolean;
    function IsNavigatableLine(const Str: Widestring): boolean;
    procedure DoNewDoc(const fn: Widestring);
    procedure AppException(Sender: TObject; E: Exception);
    function MsgEncReload: boolean;
    function MsgConfirmFtp: boolean;

    procedure InitSynIniDir;
    procedure LexListClick(Sender: TObject);

    procedure Finder_OnBeforeExecute(Sender: TObject);
    procedure Finder_OnNotFound(Sender: TObject);
    procedure Finder_OnProgress(CurPos, MaxPos: integer);
    procedure Finder_OnContinue(Sender: TObject; var ACanContinue: boolean);
    procedure Finder_OnCanAccept(Sender: TObject;
      StartPos, EndPos: integer; var Accept: Boolean);
    procedure Finder_OnFind_WithResultPane(Sender: TObject;
      StartPos, EndPos: integer; var Accept: Boolean);
    procedure Finder_OnFind_WithBkmk(Sender: TObject;
      StartPos, EndPos: integer; var Accept: Boolean);
    procedure Finder_OnFind_WithResultPaneAndBkmk(Sender: TObject;
      StartPos, EndPos: integer; var Accept: Boolean);

    procedure FinderTree_OnFind(Sender: TObject;
      StartPos, EndPos: integer; Accept: Boolean);
    procedure FinderTree_OnNotFound(Sender: TObject);

    procedure DoFind_ClipboardText(ANext: boolean);
    procedure DoFind_CurrentWord(ANext: boolean);
    procedure DoFind_AndExtendSel(ANext: boolean);
    procedure DoFind_Action(act: TSynSearchAction);
    procedure DoFind_ActionWrapper(act: TSynSearchAction);
    procedure DoFindDialog_OnFocusEditor(Sender: TObject);
    procedure DoFindDialog_OnDockedChanged(Sender: TObject);
    procedure DoFindDialog_OnRepaintNeeded(Sender: TObject);
    procedure DoFindDialog_OnShowStatus(const Msg: Widestring);
    procedure DoFindDialog(AReplaceMode: boolean);
    procedure DoFind_MarkAll(const Str: Widestring);
    procedure DoFind_InFile(const fn: Widestring; InCodepage: TSynEncOverride = cp_sr_Def);
    procedure DoFind_InFrame(F: TEditorFrame;
      AMarkAll: boolean = false;
      AWithBkmk: boolean = false);
    procedure DoReplace_InFile(const fn: Widestring);
    procedure DoReplace_InAllTabs(var nRep, nFiles: integer);
    procedure DoReplace_TabsToSpaces(F: TEditorFrame);
    procedure DoFind_InClipPanel;
    procedure DoFind_InResultsPanel;
    procedure DoFind_InOutputPanel;
    procedure DoFind_InValidatePanel;
    procedure DoFind_InPluginsLog;
    procedure DoFind_CommandFromString(const S: Widestring);

    function IsShortcutOfCmd(sh: TShortcut; cmd: integer): boolean;
    function GetShortcutOfCmd(id: integer): TShortcut;
    function GetShortcutTextOfCmd(id: integer): string;

    procedure DoMacro_Run(n: Integer);
    procedure DoMacro_RecordCommand(Cmd: integer; Data: PWChar);
    function DoMacro_GetCommandId(n: Integer): Integer;
    function DoMacro_GetHotkey(n: integer): TKeyStroke;
    procedure DoMacro_SetHotkey(n: integer; AKey: TKeyStroke);
    function DoMacro_GetName(n: integer): Widestring;
    function DoMacro_GetCommandName(n: integer; AWithKey: boolean = False): Widestring;

    procedure DoOnlineSearch_Filename(const fn: string);
    procedure DoOnlineSearch_Name(const Name: string);

    function GetTheme: string;
    procedure SetIcons(const S: string);
    procedure SetTheme(const S: string);
    procedure LoadTools;
    procedure SaveTools;
    procedure LoadHtmlAndCssLists;
    procedure SetCaretTime(N: Integer);
    function GetCaretTime: Integer;

    function DoConfirmClose: boolean;
    function DoConfirmSaveLexLib: boolean;
    function DoConfirmSaveSession(CanCancel: boolean; ExitCmd: boolean = false): boolean;
    function DoConfirmMaybeBinaryFile(const fn: Widestring): boolean;

    procedure LoadClip;
    procedure LoadProj;
    procedure LoadProjPreview;
    procedure LoadMap;
    procedure LoadClips;
    procedure LoadIni;
    procedure LoadLexLib;
    procedure LoadMacros;
    procedure LoadPrintOptions;
    procedure LoadHideIni;
    procedure SaveOptionsRecent;
    procedure SaveMacros;
    procedure SavePrintOptions;
    procedure SaveLexLib;
    procedure SaveLexLibFilename;
    procedure SaveToolbarsProps;
    function DoReadTotalHistory: Widestring;

    function IsPositionMatchesTokens(Ed: TSyntaxMemo; StartPos, EndPos: Integer;
      OptTokens: TSearchTokens): boolean;

    procedure DoJumpToNextSearchResult(ANext: boolean);
    procedure DoJumpToNextOutputResult(AOutputPanel: boolean; ANext: boolean);
    procedure DoHideMenuItem(const Str: string);
    function IsLexerFindID(const Lex: string): boolean;

    procedure MsgBakEr(const fn: Widestring);
    procedure MsgBakOk(const fn: Widestring);
    procedure DoDateTime;
    procedure DoExtractText;
    procedure DoAcpPopup;
    procedure DoFuncHintPopup;
    function DoCheckUnicodeNeeded(Frame: TEditorFrame): boolean;

    procedure ApplyFrameEncodingAndReload(Frame: TEditorFrame; AEnc: Integer;
      ACanReload: boolean = true);
    procedure MenuitemSetEncoding(Sender: TObject);
    procedure MenuitemConvertEncoding(Sender: TObject);

    procedure ApplyLexerOverrides(F: TEditorFrame; const Lexer: string);
    procedure ApplyPreviewZoom(NValue: Integer);
    procedure DoCheckAutoShowACP(Ed: TSyntaxMemo);
    procedure DoLinesCommand(Cmd: TSynLineCmd);
    procedure DoToggleLineComment(Alt: boolean);
    procedure DoCopyFilenameToClipboard(F: TEditorFrame; Cmd: TSynCopyNameCmd);
    function IsCommandAllowedInMacro(Cmd: Integer): boolean;
    procedure DoTreeLevel(NLevel: Integer);
    procedure DoFoldLevel(NLevel: Integer);
    procedure DoToolbarCommentUncomment(AComment: boolean);
    procedure LoadToolbarProp(Toolbar: TSpTbxToolbar; Ini: TCustomIniFile; const Id: string);
    procedure SaveToolbarProp(Toolbar: TSpTbxToolbar; Ini: TCustomIniFile; const Id: string);
    procedure SavePanelProp(Panel: TSpTbxDockablePanel; Ini: TCustomIniFile; const Id: string);
    procedure LoadPanelProp(Panel: TSpTbxDockablePanel; Ini: TCustomIniFile;
      const Id: string; DefFloating: boolean = false);

    procedure DoToolbar_LoadContent(
      Toolbar: TSpTbxToolbar; Id: string; AutoShow: boolean);
    procedure DoToolbar_LoadProps(
      Ini: TIniFile; ImgList: TPngImageList;
      Toolbar: TObject; Id: string);
    function DoToolbar_Customize(const Id: string): boolean;
    procedure DoToolbar_CustomizeAndReload(Id: TSynUserToolbarId);
    procedure DoToolbar_OnClick(Sender: TObject);
    procedure DoToolbar_RunCommand_PyPlugin(const Str: Widestring);
    procedure DoToolbar_RunCommand_BinaryPlugin(const Str: Widestring);
    procedure DoToolbar_RunCommand_ExtTool(Cmd: Widestring);
    procedure DoToolbar_RunCommand_InternalCmd(Cmd: Widestring);

    function DoShowCmdList(AOnlyStdCommands: boolean = false): Integer;
    function DoShowCmdList_ForTools: string;
    function DoShowCmdHint_ForTools(Cmd: Widestring): Widestring;
    procedure DoEnumExtTools(L: TTntStringList);
    procedure DoEnumPyTools(L: TTntStringList);
    procedure InitMenuItemsList;
    procedure FixMenuBigImageList(Menu: TSpTbxSubmenuItem);
    procedure FixMruBigImageList(Menu: TSpTbxMruListItem);
    procedure FixSplitters;
    function SynFilesFilter: Widestring;
    procedure DoOptionsDialog(tabId: Integer);
    procedure DoTreeFocus;
    procedure DoBookmarksFocus;
    procedure DoGetOppositeEditor(
      const EdSrc: TSyntaxMemo;
      var EdOther: TSyntaxMemo;
      var DiffInTopLines: Integer;
      var EdSrcOnGroup1: boolean);
    procedure LoadAcpFromFile(const fn, Lexer: string);

    procedure DoOpenInBrowser(const fn: Widestring);
    procedure DoOpenBySelection;
    procedure DoOpenCurrentFile;
    procedure DoOpenCurrentDir;
    procedure DoOpenCmdPrompt;

    function SynHiddenOption(const Id: string; Default: integer): Integer;
    procedure DoCopySearchMarks(Ed: TSyntaxMemo);
    procedure DoTextConverter(Ed: TSyntaxMemo; const fn: Widestring; ToBack: boolean);

    procedure ShowProj;
    procedure DoOpenProject; overload;
    procedure DoAddFileToProject;
    procedure DoAddFilesToProject;
    procedure DoFavoriteProjects;
    procedure DoFavoritesDialog(ATab: Integer = -1);
    procedure DoPasteAndSelect;
    procedure DoCopyURL;
    procedure DoOpenURL;
    function UpdateCurrentColorCode(var AColor: Integer): boolean;
    procedure DoAddCurrentColorCodeToRecents;
    procedure DoSaveFolding;
    procedure DoLoadFolding;
    procedure DoOpenLastClosedFile;
    procedure DoPreviewFile(const AFilename: Widestring; AToggle: boolean; ALineNum, AColNum, ALen: Integer);

    procedure ProjPreview(Sender: TObject; const AFilename: Widestring; AToggle: boolean);
    procedure ProjRunTool(const ATool: TSynTool);
    procedure ProjPreviewClose(Sender: TObject);
    procedure ProjPreviewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ProjPreviewButtonClick(Sender: TObject);
    procedure DoCloseTabsOnProjectClosingIfNeeded;
    procedure DoProjectRenameFile(const fn, fn_new: Widestring);
    
    procedure UpdKey_String(Item: TSpTbxItem; const Cmd: Widestring);
    procedure UpdKey(Item: TSpTbxItem; CmdId: integer);
    procedure DoToggleSyncEditing;
    procedure DoZoomEditorInc(AInc: boolean);
    procedure DoZoomEditor(NZoom: Integer);
    procedure DoExtendSelection(Ed: TSyntaxMemo);
    function MsgConfirmOpenSaveSession(AFilesCount: Integer;
      const AFileName: string; ASaveMode: boolean): boolean;

    procedure DoClearFilesHistory;
    procedure DoClearTreeFind;
    procedure DoClearRecentColors;
    procedure DoClearFindDialogStatus;

    //python group
    procedure DoPyCommandPlugin(N: Integer);
    procedure DoPyConsole_EnterCommand(const Str: Widestring);
    procedure DoPyConsole_RepeatCommand;
    function DoPyLoadPlugin(
      const SFilename, SCmd: string): string;
    function DoPyLoadPluginWithParams(
      const SFilename, SCmd: string;
      AEd: TSyntaxMemo;
      const AParams: array of string): Widestring;
    procedure DoPyStringToEvents(const Str: string;
      var AEvents: TSynPyEvents;
      var AKeycodes: string);
    procedure DoPyResetPlugins;

    procedure LoadConsoleHist;
    procedure SaveConsoleHist;
    procedure InitSnippets;
    procedure LoadSnippets;
    procedure ClearSnippets;
    function DoSnippetChoice(const SInitialText: string): integer;
    procedure DoSnippetListDialog(const SInitialText: string);
    procedure DoSnippetNew;
    procedure DoSnippetsReload;
    procedure ApplyPanelTitles;
    procedure DoQuickSearch(AMode: TSynQuickSearchType);
    procedure DoWorkaround_QViewHorzScroll;
    procedure DoWorkaround_FindNext1;
    procedure DoShowHintFilename(const fn: Widestring);
    function DoCheckAutoCorrectCase(Ed: TSyntaxMemo): boolean;
    procedure UpadateFilenameForExport;
    procedure DoConfigTools;
    procedure DoConfigShellOptions;
    procedure DoConfigHideItems;
    procedure DoConfigRestoreStyles;
    procedure DoToggleTabDirs;
    procedure DoInsertUnicodeHexDialog;
    function DoSetPagesAndTabIndex(APageIndex, ATabIndex: Integer): boolean;
    procedure DoListBookmarksNavigate;

    function DoAddTab(Pages: TATPages; AUntitledStr: boolean): TEditorFrame;
    procedure TabAdd(Sender: TObject);
    procedure TabFocus(Sender: TObject);
    procedure TabClose(Sender: TObject; ATabIndex: Integer;
      var ACanClose, ACanContinue: boolean);
    procedure TabPopup(Sender: TObject);
    procedure TabOver(Sender: TObject; ATabIndex: Integer);
    procedure TabMove(Sender: TObject; NFrom, NTo: Integer);

    procedure InitPanelsTabs;
    procedure TabsLeftClick(Sender: TObject);
    procedure TabsRightClick(Sender: TObject);
    procedure TabsOutClick(Sender: TObject);

    function GetUntitledString: Widestring;
    procedure DoAddKeymappingCommand(const ACommand: Integer;
      const ACategory, ACaption, AHotkey: Widestring);

    procedure DoPluginsManager_Install;
    procedure DoPluginsManager_Remove;
    procedure DoPluginsManager_Edit;
    procedure DoPluginsManager_SaveAll;
    procedure DoPluginsManager_Update;
    procedure DoPluginsManager_Config;
    function IsCommandForMacros(Cmd: integer): boolean;
    //end of private

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMCommandAny(var Msg: TMessage); message WM_USER + 1;
    procedure WMCommandWithClose(var Msg: TMessage); message WM_USER + 2;
    //end of protected

  public
    SynDir: string;
    SynIniDir: string;
    SynExe: boolean;
    SynProjectSessionFN: string;

    Groups: TATGroups;
    fmProgress: TfmProgress;
    hLister: HWnd;
    ColorsArray: TSynColors;
    FFontTabs: TFont;
    FFontMenus: TFont;

    SynPyLog: TSynLogPanelKind;
    SynPanelPropsOut,
    SynPanelPropsVal: TSynLogPanelProps;

    SynMruFiles: TSynMruList;
    SynMruSessions: TSynMruList;
    SynMruProjects: TSynMruList;
    SynMruNewdoc: TSynMruList;

    FProjPreview: TSpTBXDockablePanel;
    FProjPreviewFilename: Widestring;
    FProjPreviewEditor: TSyntaxMemo;
    FProjPreviewButton: TSpTbxItem;

    //opt
    opStapleKind: integer;
    opStapleOffset: integer;
    opShowBookmarkColumn: boolean;
    opGroupMode: TATGroupsMode;
    opGroupSplit: Integer;
    opHintScroll: boolean;
    opPyChangeDelay: DWORD;
    opAutoCase: boolean;
    opShowPanelTitles: boolean;
    opTreeSorted: string;
    opUnderlineColored: integer;
    opSyncEditIcon: boolean;
    opWordChars: Widestring;
    opNonPrint,
    opNonPrintSpaces,
    opNonPrintEol,
    opNonPrintEolDetail: boolean;
    opCaretShapeIns,
    opCaretShapeOvr: TATCaretShape;
    opMaxTreeMatches: integer;
    opCaretsEnabled: boolean;
    opCaretsIndicator: integer;
    opCaretsGutterBand: integer;
    opSingleClickURL: boolean;
    opSortMode: TSynSortMode;
    opCopyLineIfNoSel: boolean;
    opLeftRightSelJump: boolean;
    opAutoCloseTags: boolean;
    opAutoCloseBrackets: boolean;
    opAutoCloseBracketsNoEsc: boolean;
    opAutoCloseQuotes1: boolean;
    opAutoCloseQuotes2: boolean;
    opLexersOverride: string;
    opShowRecentColors: TSynRecentColors;
    opShowMenuSizeX: integer;
    opShowMenuSizeY: integer;
    opUnicodeNeeded: integer;
    opTabColors: array[0..Pred(cTabColors)] of integer;
    opClipHook: boolean;
    opColorFtpBlue,
    opColorFtpGreen,
    opColorFtpRed: integer;
    opColorAcpText: integer;
    opColorAcpBg: integer;
    opColorAcpPrefix: integer;
    opColorAcpHintText: integer;
    opColorAcpHintText2: integer;
    opColorCaretsGutter: integer;
    opColorMicromapMarks: integer;
    opColorMicromapMisspelled: integer;
    opColorMicromapBG: integer;
    opColorBkmk: integer;
    opShowWrapMark: boolean;
    opTabEntireColor: boolean;
    opTabDblClickClose: boolean;
    opTabAngle: integer;
    opTabVisible: boolean;
    opTabAtBottom: boolean;
    opTabFolders: boolean; //show folder names before file names on tabs
    opTabNums: boolean; //show numbers on tabs
    opTabXButtons: boolean; //show [x] button on tabs
    opTabPlus: boolean;
    opTabOptionsIndex: integer; //id of active tab in Options dialog
    opTabOptionsLast: integer; //index of last closed tab in Options dialog
    opTabWidthMin,
    opTabWidthMax: integer;
    opTabDragDrop: boolean; //allow D&D of tabs
    opTabsSortMode: integer; //sort mode for Tabs panel
    opTabSwitcher: boolean; //use modern tab switcher (Ctrl+Tab)
    opTipsPanels: boolean;
    opTipsToken: boolean;
    opFollowTail: boolean;
    opFindExpand: boolean; //Expand results tree on progress
    opFindOnTop: boolean; //Find dlg on top
    opFindSuggestSel: boolean; //Suggest selection
    opFindSuggestWord: boolean; //Suggest current word
    opASaveOnTimer,
    opASaveOnFocus,
    opASaveAllFiles: boolean;
    opASaveTimeMin: integer;
    opASaveMaxSizeKb: integer;
    opASaveUnnamed: TSynAutoSaveUnnamed;
    opASaveUnnamedDir: string;
    opMinimapFontSize: integer;
    opMicroMap: boolean;
    opColorMinimapSel: integer;
    opBigSize: integer; //size in Mb for lexer-off
    opBkUndo: boolean;
    opProjPaths: Widestring;
    opHiliteBrackets: boolean;
    opColorOutSelBk,
    opColorOutSelText,
    opColorOutRedText,
    opColorOutRedSelText,
    opColorOutHi: integer;
    opTools: TSynToolList;
      //Note: if need to change max count of tools, also change these places:
      //- in design time create more items in "Run" menu (at top)
      //- in design time create more items in PopupEditor menu (at bottom)
      //- fix procedure TfmMain.UpdateTools
      //
    opStatusText: array[TSynSelState] of string;
    opSpellEn: boolean;
    opSpellExt: string;
    opShowMenuIcons: boolean;
    opHiliteSmart: boolean;
    opHiliteSmartCase: boolean;
    opHiliteSmartWords: boolean;
    opHiliteSmartOnClick: boolean;
    opDateFmt,
    opDateFmtPLog: string;
    opFileBackup: TSynBackup;
    opEsc: TSynEscMode;
    opHistProjectSave,
    opHistProjectLoad: boolean;
    opHistProjectCloseTabs: boolean;
    opHistFilter: integer;
    opHistSessionSave,
    opHistSessionLoad,
    opHistSessionProjSave,
    opHistSessionProjLoad,
    opHistSessionDef: boolean;
    opNewEnc,
    opNewLineEnds: integer;
    opNewLexer: string;
    opMruCheck: boolean; //check MRU on start
    opTabsReplace: boolean; //replace tabs->spaces on reading
    opTemplateTabbing: boolean; //use Tab key for code templates
    opTemplateTabbingExcept: string; //exclution file-ext list for ^^
    opAcpForceText: boolean; //enable to show only words from current file
    opAcpUseSingle: boolean; //auto insert single match
    opAcpChars: string; //additional word chars (lexer specific)
    opAcpHtm: boolean; //Special ACP for HTML
    opAcpCss: boolean; //Special ACP for CSS
    opAcpTabbing: boolean; //Special SmartTagTabbing feature
    opAcpFile: boolean; //ACP from curr file
    opAcpFileChars: integer; //Min word length for ACP from file
    opAcpFileSize: real; //Max file size for ACP from file
    opAcpNum: integer; //Num of chars that starts ACP
    opAcpHintDelay: integer;
    opSingleInstance: boolean; //single instance
    opLang: integer;
    opLexerGroups: boolean;
    opReloadMode: TSynReloadMode;
    opHiliteUrls: boolean;
    opColorLink: integer;
    opKeepCaretOnScreen: boolean;
    opLastDirMode: TSynLastDirMode;
    opLastDirPath,
    opLastDirSession,
    opLastDirProject: Widestring;
    opSaveFindCount,
    opSaveFileCount: integer;
    opSaveWndPos: boolean;
    opSaveEditor: TSynEditorHistoryItems;
    opAskOverwrite: boolean;
    opTextOnly: TSynBinaryAct;
    opShowTitleFull: boolean;
    opShowQsCaptions: boolean;
    opColorTabText,
    opColorTabTextMod,
    opColorTabBgActive,
    opColorTabBgActive2,
    opColorTabBgPassive,
    opColorTabBgPassiveOver,
    opColorTabBorderActive,
    opColorTabBorderPassive: integer;
    opShowCharInfo: boolean;
    opOpenAsOem,
    opOpenAsUtf8: string;

    FLockUpdate: boolean;
    FFinderTotalSize: Int64;
    FFinderDoneSize: Int64;

    Finder: TSynFinderReplacer;
    FinderInTree: TFinderInTree;
    FinderInList: TFinderInList;

    //public methods
    //plugins related
    function PluginAction_TranslatePos(var PosX, PosY, PosAbs: Integer; Direction: Boolean): Integer;
    function PluginAction_ReplaceText(DelLen: Integer; BufPtr: Pointer; BufSize: Integer): Integer;
    function PluginAction_GetProp(id: Integer; Buffer: Pointer; Param: Integer): Integer;
    function PluginAction_SetTopLine(Num: Integer): Integer;
    function PluginAction_GetSel(var Sel: TSynSelection): Integer;
    function PluginAction_SetSel(const Sel: TSynSelection): Integer;
    function PluginAction_SetText(const id: Integer; BufferPtr: Pointer; BufferSize: Integer): Integer;
    function PluginAction_GetText(const id: Integer; BufferPtr: Pointer; var BufferSize: Integer): Integer;
    function PluginAction_GetCaretPos(PtrX, PtrY, PtrAbs: PInteger): Integer;
    function PluginAction_ParseRegex(const SRegex, SStr: Widestring; var Res: TSynRegexArray): Integer;
    function PluginAction_SetCaretPos(AX, AY: Integer): Integer;
    function PluginAction_SuggestCompletion(const Str: PWideChar; NChars: Integer; ShowPopup: boolean): Integer;
    function PluginAction_ControlLog(const AMsg: Widestring; const ACmd: Integer; AColor: TColor; const APluginName: string): Integer;
    function PluginAction_ShowHint(const AMsg: Widestring): Integer;
    function PluginAction_GetMsg(const ADllFN, AMsg: Widestring; AResult: PWideChar): Integer;
    function PluginAction_GetOpenedFN(id: Integer; ptr: PWideChar): Integer;
    function PluginAction_GetProjectFN(id: Integer; ptr: PWideChar): Integer;
    function PluginAction_OpenFtpFile(const fn: Widestring; AInfoPtr: Pointer; AInfoSize: Integer): Integer;
    function PluginAction_OpenFile(const fn: Widestring): Integer;
    function PluginAction_SaveFile(id: Integer; ACanPrompt: boolean): Integer;
    function PluginAction_SetState(Index: Integer; Ptr: PWideChar): Integer;
    function PluginAction(AHandle: Pointer; AName: PWideChar; A1, A2, A3, A4: Pointer): Integer; stdcall;
    function Plugin_FrameById(id: Integer): TEditorFrame;

    function SynClipsDir: string;
    function SynDictDir: string;

    procedure UpdateRO;
    procedure UpdateGutter(F: TEditorFrame; AUpdateCur: boolean = true);
    procedure UpdateQVTree(const fn: Widestring);
    procedure UpdateStatusBar;
    procedure UpdateLexerTo(An: TSyntAnalyzer);
    procedure UpdateOnFrameChanged;
    procedure UpdateListBookmarks;
    procedure UpdateActiveTabColors;
    procedure UpdateMenuDialogBorder(AForm: TForm);

    property InitialKeyCount: Integer read FInitialKeyCount;
    property ListTabsColumns: string read GetListTabsColumns write SetListTabsColumns;
    property ListBkmkColumns: string read GetListBkmkColumns write SetListBkmkColumns;
    property ShowFullScreen: boolean read FFullscreen write SetFullscreen;
    property ShowOnTop: boolean read FOnTop write SetOnTop;

    procedure ApplyTabOptions;
    procedure ApplyTabOptionsTo(ATabs: TATTabs);
    procedure ApplyShowIconsInMenus;
    procedure ApplyCarets;
    procedure ApplyUrlClick;
    procedure ApplyShowRecentColors;
    procedure ApplySpell;
    procedure ApplyProj;
    procedure ApplyFramesOptions;
    procedure ApplyFramesGutters;
    procedure ApplyTips;
    procedure ApplyAutoSave;
    procedure ApplyDefaultFonts;
    procedure ApplyInst;
    procedure ApplyQs;
    procedure ApplyEdOptions;
    procedure ApplyFonts;
    procedure ApplyColors;
    procedure ApplyACP;
    procedure ApplyAcpColors;
    procedure ApplyOut;
    procedure ApplyMinimapProps;
    procedure ApplyBorders;
    procedure ApplyColorsFontsToFrames;

    procedure DoColorsArrayInit(var C: TSynColors);
    procedure DoColorsArrayRead(var C: TSynColors; const StrIni: string);
    function DoColorsArrayAsString(const C: TSynColors): string;
    procedure DoColorsArrayApply(const C: TSynColors; Ed: TSyntaxMemo);

    procedure DoFinderInit(AKeepFlags: boolean = false);
    procedure DoSpellConfig(Sender: TObject);
    procedure DoAutoSave;
    procedure DoBackup(const AFilename: Widestring);
    procedure DoRepaint;
    procedure DoDropFile(const fn: Widestring; IntoProj: boolean = false);
    procedure DoTabSwitch(ANext: boolean; AAllowModernSwitch: boolean = true);
    procedure FocusEditor;
    procedure FocusProj;
    function IsMouseOverProject: boolean;

    constructor CreateParented(hWindow: HWND);
    function DoOpenFile(const AFileName: WideString; const AParams: Widestring = ''): TEditorFrame;
    procedure DoOpenProject(const fn: Widestring); overload;
    procedure DoOpenArchive(const fn, AParams: Widestring);
    function DoOpenArchive_HandleIni(const fn_ini, subdir, section: string; typ: TSynAddonType): boolean;
    procedure DoOpenArchive_HandleIniSections(const fn_inf, subdir: string; typ: TSynAddonType);
    function DoOpenArchive_HandleLexer(const fn_ini, section: string): boolean;
    procedure DoOpenFolder(const dir: Widestring);
    procedure DoOpenFolderDialog;
    procedure DoNewProject;
    procedure DoSaveProject;
    procedure DoUpdateProject;
    procedure SaveOptionsAll;
    procedure DoSaveProjectSession;
    procedure DoOpenProjectSession;
    procedure SaveFrameState(F: TEditorFrame);
    function LoadFrameState(Frame: TEditorFrame; const fn: WideString): boolean;

    procedure DoOpenSession(AFilename: string; AddMode: boolean = false);
    procedure DoCloseSession(PromptToSave: boolean);
    procedure DoSaveSessionToFile(const fn: string);
    procedure DoSaveSession;
    procedure DoSaveSessionAs;
    procedure DoSessionOpenDialog;
    procedure DoSessionAddDialog;

    //event handlers
    procedure SynCaretPosChanged(Sender: TObject);
    procedure SynKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SynExecuteCommand(Sender: TObject; Command: Integer; Data: Pointer; var Handled: Boolean);
    procedure SynScroll(Sender: TObject);
    procedure SynChange(Sender: TObject);
    procedure SynGetTokenHint(Sender: TObject; TokenIndex: Integer; var HintText: String);
    procedure SynSpellCheckerCheckWord(Sender: TObject;
      const AWord: WideString; APos: Integer; var Valid: Boolean);
    procedure SynContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure SynContextGutterPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);

    //frame---------------------------------------------------------------------
    property CurrentEditor: TSyntaxMemo read FCurrentEditor write SetCurrentEditor;
    property FrameCount: integer read GetFrameCount;
    property FrameAllCount: integer read GetFrameAllCount;
    property Frames[Index: integer]: TEditorFrame read GetFrames;
    property FramesAll[Index: integer]: TEditorFrame read GetFramesAll;
    function FrameIndex(F: TEditorFrame): Integer;
    property CurrentFrame: TEditorFrame read GetCurrentFrame write SetCurrentFrame;
    function CreateFrame: TEditorFrame;
    procedure CloseFrame(Frame: TEditorFrame);
    procedure CloseFrameWithCfm(F: TEditorFrame; var ACanClose, ACanContinue: boolean);
    //---------------------------------------------------------------------

    property opCaretTime: integer read GetCaretTime write SetCaretTime;
    property opIcons: string read FIcons write SetIcons;
    property opTheme: string read GetTheme write SetTheme;
    //procedure TestApi;
    function IsPluginWindowActive(var HWnd: THandle): boolean;
    function opMarkDeletedAsModified: boolean;
    procedure DoHint(S: WideString);
    procedure DoHandleQuickSearchEscape;
    function DoHandleEscapeActions: boolean;
    function IsWordChar(ch: WideChar): boolean;
    procedure DoFindId;
    function FrameForFilename(const fn: Widestring): TEditorFrame;
    function DoCheckCommandLineTwo: boolean;
    procedure DoEnumLexers(L: TTntStrings; AlsoDisabled: boolean = false);
    procedure DoEnumFavs(L: TTntStringList);
    procedure DoEnumProjFiles(L: TTntStringList);
    procedure DoEnumIcons(L: TTntStringList);

    //Python public
    procedure DoPyConsole_LogString(const Str: Widestring);
    function DoPyEvent(AEd: TSyntaxMemo; AEvent: TSynPyEvent;
      const AParams: array of string): Widestring;
    procedure DoPyEvent_GetLineNumber(AEd: TSyntaxMemo;
      const ALineNum: Integer; var AResult: string);

    function FrameOfEditor(Ed: TSyntaxMemo): TEditorFrame;
    function BrotherEditor(Ed: TSyntaxMemo): TSyntaxMemo;
    function DoGetProjectFilename(id: Integer): Widestring;
    function CurrentFileName(Id: TSynGroupId): Widestring;
    function CurrentSessionFN: string;
    function CurrentContentFN(Unicode: boolean): Widestring;
    function CurrentSelectionFN(Unicode, Numbered: boolean): Widestring;
    function CurrentProjectFN: Widestring;
    function CurrentProjectSessionFN: string;
    function CurrentProjectMainFN: Widestring;
    function CurrentProjectWorkDir: Widestring;
    function CurrentProjectDir: Widestring;

    function SynIni: string;
    function SynToolbarsIni: string;
    function SynFavIni: string;
    function SynStylesIni: string;
    function SynHistoryStatesIni: string;
    function SynFoldStatesIni: string;
    function SynMacrosIni: string;
    function SynHideIni: string;
    function SynHistoryIni: string;
    function SynPluginsIni: string;
    function SynPluginIni(const SCaption: string): string;
    function SynDataSubdir(Id: TSynDataSubdirId): string;
    function SynSkinsDir: string;
    function SynPyDir: string;
    function SynSnippetsDir: string;
    function SynIconsDir: string;
    function SynSkinFilename(const Name: string): string;
    function SynConverterFilename(const Name: string): string;
    function SynLexersCfg: string;
    function SynLexersExCfg: string;
    function SynLexLib: string;

    function DoGetFavList: Widestring;
    function DoGetSearchPaths: Widestring;
    function DoFindCommand(
      Ed: TSyntaxMemo;
      Act: TSynSearchAction;
      const SText1, SText2: Widestring;
      const Opt: TSearchOptions;
      const Tok: TSearchTokens;
      OptBkmk, OptExtSel: boolean): Integer;
    procedure DoPyUpdateEvents(const APluginName, AEventStr, ALexersStr: string);
    function GetEditorByIndex(APagesIndex, ATabIndex, AMasterIndex: Integer): TSyntaxMemo;
    procedure GetEditorIndexes(Ed: TSyntaxMemo; var AGroupIndex, ATabIndex: Integer);
    procedure DoClearSearchHistory;
    procedure DoSetFrameTabColor(F: TEditorFrame; NColor: TColor);
    function DoAddGutterIcon(const fn: string): Integer;
    function DoGetFrameEncoding(F: TEditorFrame): integer;
    procedure DoSetFrameEncoding(Frame: TEditorFrame; AEnc: Integer);
    procedure DoAddRecentColor(N: Integer);
    //end of public
  end;

var
  SynListerIni: string = ''; //passed from Totalcmd API
  opListerSynDialog: boolean;
  opListerTcHistory: boolean;
  opListerQVReadOnly: boolean;
  opListerQVToolbars,
  opListerQVTree: string;
  opListerTextOnly: integer;
  opListerStartRO: boolean;

function SynStart(ListerWin: HWND; const FileToLoad: WideString): HWND;
procedure SynStop(hWin: HWND);
function IsFileTooBig(const fn: WideString): boolean;

function MsgConfirmBinary(const fn: WideString; H: THandle): boolean;
function MsgConfirmCreate(const fn: Widestring; H: THandle): boolean;
procedure MsgFileTooBig(const fn: Widestring; H: THandle);
procedure MsgCannotCreate(const fn: Widestring; H: THandle);

const
  cSynVer = '6.18.2120';
  cSynPyVer = '1.0.147';

const
  cSynParamRO = '/ro';
  cSynParamSingleInst = '/s';
  cSynParamLineNum = '/n=';
  cSynParamReg = '/reg';
  cSynParamTwo = '/two=';
  cSynParamCmp = '/cmp=';

var
  fmMain: TfmMain = nil;
  _SynActionProc: TSynAction = nil;

implementation

uses
  Clipbrd, Registry, CommCtrl,
  StrUtils, Types, Math, ShellApi,
  Variants,

  TntSystem, TntSysUtils,
  TntClipbrd, TntFileCtrl,
  TntWideStrings,
  TntWideStrUtils,

  ATxFProc,
  ATxColorCodes,
  ATxUnpack,
  ATxImgHint,

  {$ifdef SPELL}
  ad3SpellBase,
  ad3SpellLanguages,
  ad3Ignore,
  {$endif}

  TB2Consts,

  ecExports,
  ecZRegExpr,
  ecCmdConst,
  ecLists,
  cUtils,

  unSaveLex,
  unSetup, unAbout, unEnc, unToolsList, unSRFiles, unExtractStr, unShell, unInsertText,
  unLoadLexStyles, unMacroEdit, unGoto, unCmds,
  unProcTabbin, unGotoBkmk, unFav,
  unMenuCmds, unMenuProj, unMenuSnippets,
  unToolbarProp, unHideItems,
  unProcPy,
  unMainPy,
  unLexerLib, unSnipEd, unSaveTabs, unPrintPreview, unLexerProp,
  unLexerStyles, unPrintSetup;

{$R *.dfm}
{$R Cur.res}

const
  cConverterHtml1 = 'HTML - all entities';
  cConverterHtml2 = 'HTML - entities except brackets';

const
  cSynColorSwatchExt = 'synw-colorstring';
  cSynSnippetExt = 'synw-snippet';
  cSynSessionExt = 'synw-session';

const
  cRegexColorCode = '\#\w{3,6}';
  cRegexColorName = '[a-z]{3,30}';
  cColorNotFound = $AAAAFF;

const
  cThemeWindows = 'Windows';
  cThemeDefault = 'Office XP';
  cThemes: array[0..9] of string = (
    cThemeWindows,
    'Aluminum',
    'Athen',
    'Dream',
    'Eos',
    'Human',
    'Leopard',
    'Office XP',
    'Office 2007 Blue',
    'Office 2007 Silver'
    );

const
  cAcpCharsCss = '-#!@.'; //don't include ':'
  cAcpCharsPhp = '$'; //include '$'
  cAcpCharsHtm = ''; //empty?

const
  cp__UTF8       = -1;
  cp__UTF8_noBOM = -2;
  cp__Unicode    = -3;
  cp__UnicodeBE  = -4;

function SAcpItem(const s1, s2: string): string;
begin
  Result:= '\s1\' + s1 + '\t\\s2\' + s2;
end;

function MsgConfirmBinary(const fn: Widestring; H: THandle): boolean;
begin
  Result:= MsgConfirm(WideFormat(DKLangConstW('MNText'), [WideExtractFileName(fn)]), H);
end;

function MsgConfirmCreate(const fn: Widestring; H: THandle): boolean;
begin
  Result:= MsgConfirm(WideFormat(DKLangConstW('MCre'), [WideExtractFileName(fn)]), H);
end;

function MsgConfirmManyOpen(N: Integer; H: THandle): boolean;
begin
  Result:= MsgConfirm(WideFormat(DKLangConstW('zMOpenFiles'), [N]), H);
end;

procedure MsgFileTooBig(const fn: Widestring; H: THandle);
begin
  MsgError(WideFormat(DKLangConstW('MBig'), [WideExtractFileName(fn)]), H)
end;

procedure MsgCannotCreate(const fn: Widestring; H: THandle);
begin
  MsgError(DKLangConstW('MNCreate') + #13 + fn, H);
end;


const
  cLexerCss = 'CSS';
  cLexerCssList = 'LESS,SASS,SCSS,Sass,Stylus';
  cLexerText = 'Text files';
  cLexerIni = 'Ini files';
  cLexerXML = 'XML';
  cLexerJS = 'JavaScript';
  cLexerNfo = 'NFO files';
  cLexerMake = 'Make files';
  cLexerProperties = 'Properties';

function IsLexerListed(const Lexer, List: string): boolean;
begin
  Result:= IsStringListed(LowerCase(Lexer), LowerCase(List));
end;

function IsLexerHTML(const s: string): boolean;
begin
  Result:= Pos('HTML', s)>0;
end;

function IsLexerPHP(const s: string): boolean;
begin
  Result:= Pos('PHP', s)>0;
end;

function IsLexerPas(const s: string): boolean;
begin
  Result:= Pos('Pascal', s)>0;
end;

function IsLexerCSS(const s: string; CanBeLess: boolean = true): boolean;
begin
  Result:= (s=cLexerCss) or
    (CanBeLess and IsLexerListed(s, cLexerCssList));
end;

function IsLexerJS(const s: string): boolean;
begin
  Result:= s=cLexerJS;
end;

function IsLexerXML(const s: string): boolean;
begin
  Result:= s=cLexerXML;
end;

function IsLexerWithTags(const s: string): boolean;
begin
  Result:= IsLexerHTML(s) or IsLexerXML(s);
end;

function IsLexerWithImages(const s: string): boolean;
begin
  Result:= IsLexerHTML(s) or IsLexerCSS(s);
end;

function IsLexerIni(const s: string): boolean;
begin
  Result:= s=cLexerIni;
end;

function IsLexerNFO(const s: string): boolean;
begin
  Result:= s=cLexerNfo;
end;

function IsLexerMake(const s: string): boolean;
begin
  Result:= s=cLexerMake;
end;

function IsLexerProp(const s: string): boolean;
begin
  Result:= s=cLexerProperties;
end;

function IsLexerWithColors(const s: string): boolean;
begin
  Result:=
    IsLexerCSS(s) or
    IsLexerHTML(s) or
    IsLexerJS(s) or
    IsLexerPHP(s) or
    IsLexerProp(s) or
    IsLexerIni(s);
end;

//-------------------
const
  cLister_itm_wrap    = $FFFC;
  cLister_itm_percent = $FFFE;

type
  TListerPluginInfo = record
    PlugWinProc: Pointer; //callback function of our form
    PlugForm: TfmMain;    //our form
  end;

//----------------------------------------------------------------------------------
procedure TfmMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style:= (WS_CHILD or WS_MAXIMIZE) and not WS_CAPTION and not WS_BORDER;
  Params.WindowClass.cbWndExtra:= SizeOf(Pointer); //4 bytes for address of form
end;

constructor TfmMain.CreateParented(hWindow: HWND);
begin
  inherited CreateParented(hWindow);
  hLister:= hWindow;
  QuickView:= GetParent(hLister) <> 0;
end;

//hook form messages
function HookDestroy(hWin: HWND; Msg, wParam, lParam: LongInt): LongInt; stdcall;
var
  p: ^TListerPluginInfo;
begin
  Result:= 0;
  p:= Pointer(GetWindowLong(hWin, GWL_USERDATA));
  if Msg = WM_DESTROY then
    SynStop(hWin)
  else
  if Msg = WM_SETFOCUS then
    p^.PlugForm.FocusEditor
  else
    Result:= CallWindowProc(p^.PlugWinProc, hWin, Msg, wParam, lParam);
end;

//hook close button of lister window to make 'Cancel' possible
function HookList(hWin: HWND; Msg, wParam, lParam: LongInt): LongInt; stdcall;
var
  p: ^TListerPluginInfo;
begin
  p:= Pointer(GetWindowLong(hWin, GWL_USERDATA));
  if (Msg=WM_ACTIVATE) and (wParam<>0) then
  begin
    p^.PlugForm.FocusEditor;
    Result:= 0;
    Exit;
  end;
  {
  //already done this in FormCloseQuery
  if (Msg = WM_CLOSE) and (not p^.PlugForm.DoConfirmClose) then begin
    Result:= 0;
    Exit;
  end;
  }
  Result:= CallWindowProc(p^.PlugWinProc, hWin, Msg, wParam, lParam);
end;

procedure SynStop(hWin: HWND);
var
  p: ^TListerPluginInfo;
  N: integer;
begin
  N:= GetWindowLong(GetParent(hWin), GWL_USERDATA);
  if N<>0 then
  try
    p:= Pointer(N);
    p^.PlugForm.Close;
    //restore callback function
    SetWindowLong(GetParent(hWin), GWL_WNDPROC, Integer(p^.PlugWinProc));
    Dispose(p);
  except
    on E: Exception do
      MsgExcept('Exception on plugin unhooking', E, 0);
  end;

  N:= GetWindowLong(hWin, GWL_USERDATA);
  if N<>0 then
  try
    p:= Pointer(N);
    //CloseHandle(p^.PlugForm.hMutex);
    Application.RemoveComponent(p^.PlugForm);
    Application.Handle:= 0;
    //restore callback function
    SetWindowLong(p^.PlugForm.Handle, GWL_WNDPROC, Integer(p^.PlugWinProc));
    p^.PlugForm.Free;
  except
    on E: Exception do
      MsgExcept('Exception on plugin closing', E, 0);
  end;
end;

function SynStart(ListerWin: HWND; const FileToLoad: WideString): HWND;
var
  fmMain: TfmMain;
  p: ^TListerPluginInfo;
begin
  Result:= 0;
  try
    fmMain:= TfmMain.CreateParented(ListerWin);
    with fmMain do
    begin
      SynExe:= False;
      InitSynIniDir;

      //synchronize our form and Lister
      //Application.Handle:= ListerWin;
      Application.OnException:= AppException;
      //Application.InsertComponent(fmMain);
      Application.HintHidePause:= 5000;

      //substitution callback function
      New(p);
      SetWindowLong(fmMain.Handle, GWL_USERDATA, Integer(p));
      p^.PlugForm:= fmMain;
      p^.PlugWinProc:= Pointer(SetWindowLong(fmMain.Handle, GWL_WNDPROC, Integer(@HookDestroy)));
      //hook lister close
      New(p);
      SetWindowLong(hLister, GWL_USERDATA, Integer(p));
      p^.PlugForm:= fmMain;
      p^.PlugWinProc:= Pointer(SetWindowLong(hLister, GWL_WNDPROC, Integer(@HookList)));

      Show;
      DoOpenFile(FileToLoad);
      UpdateRO;
      Result:= Handle;

      if not SynExe then
      begin
        DoWorkaround_QViewHorzScroll;
        if QuickView then
          UpdateQVTree(FileToLoad);
      end;
    end; //with fmMain
  except
    on E: Exception do
      MsgExcept('Exception on plugin opening', E, 0);
  end;
end;

procedure TfmMain.AppException(Sender: TObject; E: Exception);
begin
  MsgExcept('Exception in plugin', E, 0);
end;

procedure TfmMain.ecTitleCaseExecute(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smTitleCaseBlock);
end;

function IsFileTooBig(const fn: WideString): boolean;
begin
  Result:= FGetFileSize(fn) >= 200 * 1024 * 1024;
end;

function StringToAddonType(const Str: string): TSynAddonType;
var
  i: TSynAddonType;
begin
  Result:= cAddonTypeNone;
  for i:= Low(TSynAddonType) to High(TSynAddonType) do
    if Str=cSynAddonType[i] then
    begin
      Result:= i;
      Exit
    end;
end;

function TfmMain.SynFilesFilter: Widestring;
begin
  Result:= SyntaxManagerFilesFilter(SyntaxManager, DKLangConstW('filAll'));
end;

procedure TfmMain.acOpenExecute(Sender: TObject);
var
  i: Integer;
  s: Widestring;
begin
  OD.Filter:= SynFilesFilter;
  if (opLastDirMode=cLastDirRemember) and (opHistFilter>0) then
    OD.FilterIndex:= opHistFilter
  else
    OD.FilterIndex:= SFilterNum(OD.Filter);
  OD.FileName:= '';
  OD.InitialDir:= LastDir;

  if not OD.Execute then Exit;

  if (OD.Files.Count=1) and not IsFileExist(OD.FileName) then
  begin
    if not MsgConfirmCreate(OD.FileName, Handle) then Exit;
    with TStringList.Create do
    try
      SaveToFile(OD.FileName);
    finally
      Free
    end;
  end;

  for i:= 0 to OD.Files.Count-1 do
  begin
    S:= OD.Files[i];
    if IsFileTooBig(S) then
      MsgFileTooBig(S, Handle)
    else
    if IsFileArchive(S) or DoConfirmMaybeBinaryFile(S) then
      DoOpenFile(S);
  end;

  //save last dir
  SaveLastDir(OD.FileName, OD.Filter, OD.FilterIndex);

  if CurrentEditor<>nil then
    CurrentEditor.Invalidate;
end;

function TfmMain.DoOpenFile(const AFileName: WideString; const AParams: Widestring = ''): TEditorFrame;
var
  F: TEditorFrame;
begin
  UpdateColorHint;

  if IsFileProject(AFileName) then
  begin
    DoOpenProject(AFileName);
    Result:= nil;
    Exit
  end;

  if IsFileArchive(AFileName) then
  begin
    DoOpenArchive(AFileName, AParams);
    Result:= nil;
    Exit
  end;

  if AFileName = '' then
  begin
    acNewTab.Execute;
    Result:= CurrentFrame;
    Exit
  end;

  //file already opened?
  F:= FrameForFilename(AFileName);
  if F<>nil then
  begin
    Result:= F;
    CurrentFrame:= F;
    Exit;
  end;

  //create new frame and load file
  F:= CurrentFrame;
  if (F <> nil) and (F.FileName = '') and (not F.Modified) then
    Result:= F
  else
    Result:= DoAddTab(Groups.PagesCurrent, false);

  //reset encoding for new frame
  DoSetFrameEncoding(Result, 0);

  if FCanUseLexer(AFileName) then
    Result.EditorMaster.TextSource.SyntaxAnalyzer:= DoFindLexerForFilename(SyntaxManager, AFileName)
  else
    Result.EditorMaster.TextSource.SyntaxAnalyzer:= nil;
  UpdateLexerTo(Result.EditorMaster.TextSource.SyntaxAnalyzer);

  Result.DoStopNotif;
  Result.LoadFile(AFileName);
  Result.TabCaption:= WideExtractFileName(AFileName);
  Result.DoStartNotif;

  UpdateOnFrameChanged;
  UpdateFrameEnc(Result);
  UpdateFrameSpell(Result);
  UpdateFrameZoom(Result);

  //maybe set opened editor R/O for Lister plugin
  if not SynExe then
    UpdateRO;

  if not SynExe then
    BringWindowToTop(hLister);

  UpdateGutter(Result);
  if opTabsReplace and (TemplateEditor.TabMode=tmSpaces) then
    DoReplace_TabsToSpaces(Result);

  DoPyEvent(Result.EditorMaster, cSynEventOnOpen, []);
end;

procedure TfmMain.UpdateFrameEnc(Frame: TEditorFrame);
var
  IsBE: boolean;
begin
  //dont reread files with BOM
  if IsFileWithBOM(Frame.FileName) then Exit;

  if SFileExtensionMatch(Frame.FileName, opOpenAsOem) then
  begin
    ApplyFrameEncodingAndReload(Frame, CP_OEMCP); //Reread as OEM
  end
  else
  if SFileExtensionMatch(Frame.FileName, opOpenAsUtf8) or (opOpenAsUtf8 = '*')
    or IsFileUTF8NoBOM(Frame.FileName)
    or IsFileXmlUTF8(Frame.FileName) then
  begin
    ApplyFrameEncodingAndReload(Frame, cp__UTF8_noBOM); //Reread as UTF8 no BOM
  end
  else
  if IsFileUnicodeNoBOM(Frame.FileName, IsBE) then
  begin
    if IsBE then
      ApplyFrameEncodingAndReload(Frame, cp__UnicodeBE) //UTF-16 BE
    else
      ApplyFrameEncodingAndReload(Frame, cp__Unicode); //UTF-16 LE
  end;
end;

function TfmMain.SaveFrame(Frame: TEditorFrame; PromtDialog: Boolean): boolean;
var
  AUntitled: boolean;
  ALexerName: string;
begin
  Result:= true;
  if Frame=nil then Exit;
  Frame.DoStopNotif;

  if not DoCheckUnicodeNeeded(Frame) then Exit;
  if DoPyEvent(Frame.EditorMaster, cSynEventOnSaveBefore, [])=cPyFalse then Exit;

  AUntitled:= Frame.FileName='';
  if not PromtDialog then
    PromtDialog:= AUntitled;

  if PromtDialog then
  begin
    if AUntitled then
      SD.InitialDir:= LastDir_UntitledFile
    else
      SD.InitialDir:= LastDir;
    SD.Filter:= SynFilesFilter;

    ALexerName:= Frame.CurrentLexer;
    if ALexerName<>'' then
      SD.FilterIndex:= SFilterNameToIdx(SD.Filter, ALexerName)
    else
      SD.FilterIndex:= SFilterNum(SD.Filter);

    if Frame.FileName<>'' then
      SD.FileName:= Frame.FileName
    else
      SD.FileName:= DKLangConstW('newFnLatin');

    //show "Save as" dialog
    Result:= SD.Execute and (SD.FileName<>'');

    if Result then
    begin
      if WideExtractFileExt(SD.FileName)='' then
        SD.FileName:= FFreeFN(
          WideExtractFileName(SD.FileName),
          SFilterIdxToExt(SD.Filter, SD.FilterIndex),
          WideExtractFileDir(SD.FileName));

      Frame.SaveFile(SD.FileName);
      SynMruFiles.AddItem(SD.FileName);
      DoPyEvent(Frame.EditorMaster, cSynEventOnSaveAfter, []);

      //update lexer
      if FCanUseLexer(SD.FileName) then
        Frame.EditorMaster.TextSource.SyntaxAnalyzer:= DoFindLexerForFilename(SyntaxManager, SD.FileName)
      else
        Frame.EditorMaster.TextSource.SyntaxAnalyzer:= nil;
      UpdateLexerTo(Frame.EditorMaster.TextSource.SyntaxAnalyzer);

      //save last dir
      if AUntitled then
        SaveLastDir_UntitledFile(SD.FileName)
      else
        SaveLastDir(SD.FileName, SD.Filter, SD.FilterIndex);
      //spell
      UpdateFrameSpell(Frame);
    end;
  end
  else
  //save silently, without "Save As" dialog
  begin
    //check existance of folder,
    //but always allow saving to Windows folder
    //(needed for "c:\Windows\system32\drivers\etc\hosts" - folder is virtual on Win x64)
    if not IsDirOkForSaving(WideExtractFileDir(Frame.FileName)) then
    begin
      MsgNoDir(WideExtractFileDir(Frame.FileName));
      Exit
    end;

    Frame.SaveFile(Frame.FileName);
    DoPyEvent(Frame.EditorMaster, cSynEventOnSaveAfter, []);

    //save on ftp
    if Frame.IsFtp then
      DoPlugin_SaveFtpFile(Frame);
  end;

  //repaint editor (coz line states not redrawn)
  Frame.EditorMaster.Invalidate;
  Frame.EditorSlave.Invalidate;

  //start notifications
  if IsFileExist(Frame.FileName) then
    Frame.DoStartNotif;

  //send "reread panel" to plugins
  DoPlugin_RefreshFiles(Frame.FileName);

  //need to reload, as line-ends changed?
  if Frame.LineEndsChg then
  begin
    Frame.LineEndsChg:= false;
    DoFrameReloadInt(Frame);
    UpdateFrameEnc(Frame);
  end;

  if AUntitled then
    UpdateListBookmarks;
end;

function TfmMain.GetFrameCount: integer;
begin
  if Assigned(Groups) then
    Result:= Groups.PagesCurrent.Tabs.TabCount
  else
    Result:= 0;
end;

function TfmMain.GetFrameAllCount: integer;
begin
  if Assigned(Groups) then
    Result:= Groups.GetTabTotalCount
  else
    Result:= 0;
end;

function TfmMain.GetFrames(Index: integer): TEditorFrame;
var
  D: TATTabData;
begin
  Result:= nil;
  if Assigned(Groups) then
  begin
    with Groups.PagesCurrent do
      D:= Tabs.GetTabData(Index);
    if D<>nil then
      Result:= D.TabObject as TEditorFrame;
  end;
end;

function TfmMain.GetFramesAll(Index: integer): TEditorFrame;
begin
  if Assigned(Groups) then
    Result:= Groups.GetTabDataOfTotalIndex(Index).TabObject as TEditorFrame
  else
    Result:= nil;
end;


procedure TfmMain.SetCurrentFrame(Frame: TEditorFrame);
var
  NPages, NTab: Integer;
begin
  if Frame=nil then Exit;
  Groups.PagesAndTabIndexOfControl(Frame, NPages, NTab);
  if NTab>=0 then
    Groups.Pages[NPages].Tabs.TabIndex:= NTab;
end;

procedure TfmMain.FocusFrame(Frame: TEditorFrame);
var
  Ed: TSyntaxMemo;
begin
  if Frame=nil then Exit;

  if Frame.IsMasterFocused then
    Ed:= Frame.EditorMaster
  else
    Ed:= Frame.EditorSlave;

  if Frame.Enabled and Frame.Visible then
  begin
    if Ed.Enabled and Ed.Visible and Ed.CanFocus then
      Ed.SetFocus;
  end;
end;

function TfmMain.GetCurrentFrame: TEditorFrame;
begin
  if CurrentEditor<>nil then
    Result:= FrameOfEditor(CurrentEditor)
  else
    Result:= nil;
end;

procedure TfmMain.UpdateEditorNonPrinted(Ed: TSyntaxMemo);
begin
  Ed.NonPrinted.Visible:= opNonPrint;
  Ed.NonPrintedSpaces:= opNonPrintSpaces;
  Ed.NonPrintedEol:= opNonPrintEol;
  Ed.NonPrintedEolDetails:= opNonPrintEolDetail;

  Ed.NonPrinted.SpaceChar:= #183;
  Ed.NonPrinted.TabChar:= #187;
end;

function TfmMain.CreateFrame: TEditorFrame;
begin
  Result:= TEditorFrame.Create(Self);
  Result.Visible:= false; //fix flicker on new-tab
  Result.Name:= '';
  Result.OnTitleChanged:= UpdateTitle;
  Result.OnSaveState:= FrameSaveState;

  Result.EditorMaster.BorderStyle:= SynBorderStyleEditor;
  Result.EditorSlave.BorderStyle:= SynBorderStyleEditor;
  Result.EditorMaster.KeyMapping:= SyntKeyMapping;
  Result.EditorSlave.KeyMapping:= SyntKeyMapping;

  Result.HyperlinkHighlighter.Active:= opHiliteUrls;
  Result.HyperlinkHighlighter.Style.Font.Color:= opColorLink;
  Result.HyperlinkHighlighter.SingleClick:= opSingleClickURL;

  if opHintScroll then
  begin
    with Result.EditorMaster.HintProps do
      ShowHints:= ShowHints+[shScroll];
    with Result.EditorSlave.HintProps do
      ShowHints:= ShowHints+[shScroll];
  end;

  if not opShowBookmarkColumn then
  begin
    Result.EditorMaster.Gutter.Bands[cBandBoommarks].Width:= 0;
    Result.EditorSlave.Gutter.Bands[cBandBoommarks].Width:= 0;
  end;

  case opStapleKind of
    0: Result.EditorMaster.StaplePen.Style:= psSolid;
    1: Result.EditorMaster.StaplePen.Style:= psDot;
    2: Result.EditorMaster.StaplesEnabled:= false;
  end;
  Result.EditorSlave.StaplePen.Style:= Result.EditorMaster.StaplePen.Style;
  Result.EditorSlave.StaplesEnabled:= Result.EditorMaster.StaplesEnabled;

  Result.EditorMaster.StapleOffset:= opStapleOffset;
  Result.EditorSlave.StapleOffset:= opStapleOffset;

  UpdateEditorNonPrinted(Result.EditorMaster);
  UpdateEditorNonPrinted(Result.EditorSlave);

  Result.ShowMap:= opMicroMap;
  Result.CaretsEnabled:= opCaretsEnabled;
  Result.CaretsGutterBand:= opCaretsGutterBand;
  Result.CaretsGutterColor:= opColorCaretsGutter;
  Result.CaretsIndicator:= opCaretsIndicator;

  EditorSetCaretShape(Result.EditorMaster, opCaretShapeIns, true);
  EditorSetCaretShape(Result.EditorSlave, opCaretShapeIns, true);
  EditorSetCaretShape(Result.EditorMaster, opCaretShapeOvr, false);
  EditorSetCaretShape(Result.EditorSlave, opCaretShapeOvr, false);

  PropsManager.Add(Result.EditorMaster);
  PropsManager.Add(Result.EditorSlave);
  DoColorsArrayApply(ColorsArray, Result.EditorMaster);
  DoColorsArrayApply(ColorsArray, Result.EditorSlave);

  Result.EditorMaster.Gutter.LineBreakObj:= IfThen(opShowWrapMark, 0, -1);
  Result.EditorSlave.Gutter.LineBreakObj:= Result.EditorMaster.Gutter.LineBreakObj;

  InitFrameTab(Result);
  UpdateGutter(Result, False);
  CurrentFrame:= Result;
  UpdateNewFrame(Result);
end;

procedure TfmMain.UpdateNewFrame(F: TEditorFrame);
var
  Val: integer;
  Str: string;
begin
  if F=nil then Exit;

  //upd encoding
  if Assigned(fmProj) and (fmProj.FOpts.DefEnc<>0) then
    Val:= Pred(fmProj.FOpts.DefEnc)
  else
    Val:= opNewEnc;

  case Val of
    0: ApplyFrameEncodingAndReload(F, cp_ACP);
    1: ApplyFrameEncodingAndReload(F, cp_OEMCP);
    2: ApplyFrameEncodingAndReload(F, cp__UTF8);
    3: ApplyFrameEncodingAndReload(F, cp__UTF8_noBOM);
    4: ApplyFrameEncodingAndReload(F, cp__Unicode);
    5: ApplyFrameEncodingAndReload(F, cp__UnicodeBE);
  end;

  //upd line ends
  if Assigned(fmProj) and (fmProj.FOpts.DefLineEnds<>0) then
    Val:= Pred(fmProj.FOpts.DefLineEnds)
  else
    Val:= opNewLineEnds;

  case Val of
    0: UpdateFrameLineEnds(F, tfCR_NL, false); //win
    1: UpdateFrameLineEnds(F, tfNL, false); //unix
    2: UpdateFrameLineEnds(F, tfCR, false); //mac
  end;

  //upd lexer
  if Assigned(fmProj) and (fmProj.FOpts.DefLexer<>'') then
    Str:= fmProj.FOpts.DefLexer
  else
    Str:= opNewLexer;

  if Str='' then
    F.EditorMaster.TextSource.SyntaxAnalyzer:= nil
  else
    F.EditorMaster.TextSource.SyntaxAnalyzer:= SyntaxManager.FindAnalyzer(Str);
  UpdateLexerTo(F.EditorMaster.TextSource.SyntaxAnalyzer);

  //other
  F.Modified:= false;
  F.LineEndsChg:= false;
end;

procedure TfmMain.InitFrameTab(Frame: TEditorFrame);
begin
  UpdateFrameSpell(Frame);
end;

procedure TfmMain.CloseFrame(Frame: TEditorFrame);
begin
  if opSaveFileCount>0 then
    SaveFrameState(Frame);

  if Frame.FileName<>'' then
    if not Frame.NotInRecents then
      SynMruFiles.AddItem(Frame.FileName);

  if (Frame.EditorMaster=CurrentEditor) or (Frame.EditorSlave=CurrentEditor) then
    CurrentEditor:= nil;

  Frame.Free;

  //release directory of closed file
  WideSetCurrentDir(FInitialDir);
end;

procedure TfmMain.UpdateTreeProps;
var
  bSorted, bIcons: boolean;
  i: integer;
begin
  //sort tree?
  bSorted:= CurrentFrame.IsTreeSorted;
  if bSorted then
    Tree.SortType:= stText
  else
    Tree.SortType:= stNone;

  //show icons in tree?
  bIcons:= false;
  if (ImgListTree.Count > 0) and (CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer <> nil) then
    with CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer do
      for i:= 0 to BlockRules.Count-1 do
        if (BlockRules[i].TreeItemImage <> -1) or
           (BlockRules[i].TreeGroupImage <> -1) then
        begin
          bIcons:= true;
          Break;
        end;
  if bIcons then
    Tree.Images:= ImgListTree
  else
    Tree.Images:= nil;
end;

procedure TfmMain.UpdateOnFrameChanged;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F<>nil then
  begin
    UpdateLexerTo(F.EditorMaster.TextSource.SyntaxAnalyzer);
    UpdateTitle(F);
    UpdateStatusbar;
    SynScroll(CurrentEditor);
    UpdateTreeProps;
  end
  else
    CurrentEditor:= nil;
end;

procedure TfmMain.UpdateTitle(Sender: TFrame);
const
  cModified: array[boolean] of string = ('', #$95);
var
  F: TEditorFrame;
  s, sWin, sTask, sSess, sRO, sDebug: WideString;
begin
  UpdateListTabs;

  if not SynExe then Exit;
  F:= CurrentFrame;
  if (F=nil) or (F<>Sender) then Exit;

  if (F.FileName<>'') and opShowTitleFull then
    s:= F.FileName
  else
    s:= F.TabCaption;

  if FSessionFN <> '' then
    sSess:= '{' + WideChangeFileExt(WideExtractFileName(FSessionFN), '') + '} '
  else
    sSess:= '';

  if F.EditorMaster.ReadOnly then
    sRO:= ' ' + DKLangConstW('RO')
  else
    sRO:= '';

  {$ifdef TabOrder}
  sDebug:= ' [' + TabSwitchers[1].GetTabList + ' , ' + TabSwitchers[2].GetTabList + ']';
  {$else}
  sDebug:= '';
  {$endif}

  sWin:= sSess + cModified[F.Modified] + s + ' - SynWrite' + sRO + sDebug;
  sTask:= sSess + cModified[F.Modified] + WideExtractFileName(s) + ' - SynWrite';

  TTntForm(Parent).Caption:= sWin;
  TntApplication.Title:= sTask;
  ecSyntPrinter.Title:= F.TabCaption;
end;

procedure TfmMain.SetCurrentEditor(Value: TSyntaxMemo);
var
  i: integer;
  An: TSyntAnalyzer;
begin
  if FCurrentEditor = Value then Exit;
  ecSyntPrinter.SyntMemo:= Value;
  ecCharPopup.SyntMemo:= Value;

  for i:= 0 to ActionList.ActionCount - 1 do
    if (ActionList.Actions[i] is TecBaseMemoAction) then
      (ActionList.Actions[i] as TecBaseMemoAction).SyntMemo:= Value;

  ecACP.SyntMemo:= Value;
  PluginACP.SyntMemo:= Value;
  ParamCompletion.SyntMemo:= Value;
  ecMacroRec.SyntMemo:= Value;

  FCurrentEditor:= Value;

  if FCurrentEditor <> nil then
  begin
    if CurrentFrame<>nil then
      CurrentFrame.HyperlinkHighlighter.Editor:= FCurrentEditor;

    if FCurrentEditor.SyntObj<>nil then
      An:= FCurrentEditor.SyntObj.Owner
    else
      An:= nil;
    UpdateLexerTo(An);

    UpdateStatusBar;
    if not QuickView then
      FocusEditor;
  end;

  //sync maps
  SyncMapData;
  SyncMapPos;

  //sync tree
  SyncTree;
end;

function TfmMain.MsgConfirmSaveFrame(Frame: TEditorFrame; CanCancel: boolean=true): TModalResult;
var
  Str: Widestring;
  Res: Integer;
begin
  Str:= WideFormat(DKLangConstW('MSave'), [Frame.TabCaption]);
  Res:= MsgConfirmYesNoCancel(Str, Handle, CanCancel);
  case Res of
    id_ok: Result:= mrOk;
    id_yes: Result:= mrYes;
    id_no: Result:= mrNo;
    else Result:= mrCancel;
  end;
end;

procedure TfmMain.UpdateStatusBar;
var
  ro, sel2, en_lex: boolean;
  ed: TSyntaxMemo;
  frame: TEditorFrame;
begin
  if FLockUpdate then Exit;

  ed:= CurrentEditor;
  frame:= CurrentFrame;
  if ed=nil then Exit;
  if frame=nil then Exit;

  ro:= ed.ReadOnly;
  sel2:= ed.HaveSelection;
  en_lex:= SyntaxManager.CurrentLexer<>nil;

  TBXSubmenuBarSave.ImageIndex:= IfThen(frame.Modified, cImageIndexSaveIcon, cImageIndexSaveIconPale);

  //Hilite brackets
  TimerBrackets.Enabled:= true;

  ecReadOnly.Checked:= ro;
  ecWrap.Checked:= ed.WordWrap;
  ecLineNums.Checked:= ed.LineNumbers.Visible;
  ecFolding.Checked:= not ed.DisableFolding;
  ecRuler.Checked:= ed.HorzRuler.Visible;
  ecSmartHl.Checked:= opHiliteSmart;
  ecFullScr.Checked:= ShowFullScreen;

  ecNonPrint.Checked:= ed.NonPrinted.Visible;
  ecNonPrintOff.Checked:= not ed.NonPrinted.Visible;
  ecNonPrintSpaces.Checked:= ed.NonPrinted.Visible and ed.NonPrintedSpaces and not ed.NonPrintedEol;
  ecNonPrintEol.Checked:= ed.NonPrinted.Visible and not ed.NonPrintedSpaces and ed.NonPrintedEol;
  ecNonPrintBoth.Checked:= ed.NonPrinted.Visible and ed.NonPrintedSpaces and ed.NonPrintedEol;
  ecNonPrintEolDetails.Checked:= ed.NonPrintedEolDetails;

  ecReplace.Enabled:= not ro;
  ecRepeatCmd.Enabled:= (not ro) and (FLastCmdId>0);

  {
  ecToggleLineCommentAlt.Enabled:= (not ro) and (SyntaxManager.CurrentLexer<>nil) and (SyntaxManager.CurrentLexer.LineComment<>'');
  ecCommentLines.Enabled:= ecToggleLineCommentAlt.Enabled;
  ecUnCommentLines.Enabled:= ecToggleLineCommentAlt.Enabled;
  TBXItemEToggleLineComment.Enabled:= ecToggleLineCommentAlt.Enabled;
  }

  if Assigned(Status) then
  begin
    if ro then
      StatusItemRO.ImageIndex:= cImageIndexRoOn
    else
      StatusItemRO.ImageIndex:= cImageIndexRoOff;

    case ed.SelectModeDefault of
      msColumn: StatusItemSelMode.ImageIndex:= cImageIndexSelColumn;
      msLine: StatusItemSelMode.ImageIndex:= cImageIndexSelLine;
      else StatusItemSelMode.ImageIndex:= cImageIndexSelNormal;
    end;

    if ed.WordWrap then
      StatusItemWrap.ImageIndex:= cImageIndexWrapOn
    else
      StatusItemWrap.ImageIndex:= cImageIndexWrapOff;

    UpdateStatusbarTabsize;
  end;

  UpdateStatusbarEnc(frame);
  UpdateStatusbarLineEnds;

  acSave.Enabled:= not ro;
  acNewTab.Enabled:= not Quickview;
  acClose.Enabled:= not Quickview;
  acOpen.Enabled:= not Quickview;
  acNewWindow.Enabled:= SynExe;
  acSetupLexerNew.Enabled:= en_lex;
  acSetupLexerStyles.Enabled:= en_lex;
  ecFullScr.Enabled:= SynExe;
  ecOnTop.Enabled:= SynExe;
  TBXSubmenuAddons.Enabled:= SynExe;

  ecCopy.Update;
  ecCut.Update;
  ecPaste.Update;
  ecClear.Update;

  ecUndo.Update;
  ecRedo.Update;
  ecCommentLines.Update;
  ecUncommentLines.Update;
  ecIndent.Update;
  ecUnindent.Update;
  ecSortAscending.Update;
  ecSortDescending.Update;

  ecUpperCase.Update;
  ecLowerCase.Update;
  ecToggleCase.Update;
  ecTitleCase.Enabled:= sel2;
  ecSentCase.Enabled:= sel2;
  ecGoto.Enabled:= ed.Lines.Count>0;
  ecRemoveBlanks.Enabled:= not ro;
  ecSortDialog.Enabled:= (ed.Lines.Count>0) and not ro;
  ecSortAscending.Enabled:= ecSortDialog.Enabled;
  ecSortDescending.Enabled:= ecSortDialog.Enabled;
  ecDedupAll.Enabled:= ecSortDialog.Enabled;
  ecDedupAdjacent.Enabled:= ecSortDialog.Enabled;
  TbxSubmenuCase.Enabled:= ecSortDialog.Enabled;

  TBXItemBarComm.Enabled:= (ed.Lines.Count>0) and not ro and en_lex;
  TBXItemBarUncom.Enabled:= TBXItemBarComm.Enabled;

  ecSpellLive.Checked:= Frame.SpellLive;
  ecSyncScrollV.Enabled:= (Groups.PagesVisibleCount=2) and (Groups.Pages2.Tabs.TabCount>0);
  ecSyncScrollH.Enabled:= ecSyncScrollV.Enabled;

  ecPrintAction.Update;
  ecPrinterSetup.Update;

  begin
    StatusItemCaret.Caption:= SStatusText(Ed);
    StatusItemZoom.Caption:= IntToStr(Ed.Zoom) + '%';

    with StatusItemChar do
    begin
      if opShowCharInfo and (Ed.TextLength>0) and (not EditorHasNoCaret(Ed)) then
        Caption:= SStatusCharInfo(Ed)
      else
        Caption:= '';

      Visible:= opShowCharInfo;
    end;
  end;
end;

function TfmMain.SStatusCharInfo(Ed: TSyntaxMemo): Widestring;
var
  ch: Widechar;
  sAnsi: string;
begin
  Result:= '';
  with Ed do
    if TextLength>0 then
    begin
      ch:= Lines.Chars[CaretStrPos+1];
      if Ord(ch)<32 then
        Result:= ''
      else
        Result:= '"' + WideString(ch) + '" ';

      if Lines.TextCoding <> tcAnsi then
      begin
        //Unicode encoding active
        Result:= Result + Format('#%d 0x%x', [Ord(ch), Ord(ch)]);
      end
      else
      begin
        //Some codepage active
        sAnsi:= UnicodeToAnsiCP(WideString(ch), Lines.Codepage);
        if sAnsi='' then sAnsi:= #0;
        Result:= Result + Format('#%d 0x%2.2x', [Ord(sAnsi[1]), Ord(sAnsi[1])]);
      end;
    end;
end;

const
  cExeSuffix: array[boolean] of string = ('', 'Exe');

procedure TfmMain.LoadIni;
var
  ini: TMemIniFile;
  NCount: integer;
  s: Widestring;
  NFlags: LongWord;
  Ok: boolean;
begin
  //get all options from Syn.ini
  ini:= TMemIniFile.Create(SynIni);
  with ini do
  try
    //toolbars props
    if not QuickView then
    begin
      LoadToolbarProp(tbFile, ini, 'File');
      LoadToolbarProp(tbEdit, ini, 'Edit');
      LoadToolbarProp(tbView, ini, 'View');
      LoadToolbarProp(tbQs, ini, 'Qs');

      LoadToolbarProp(tbUser1, ini, 'U1');
      LoadToolbarProp(tbUser2, ini, 'U2');
      LoadToolbarProp(tbUser3, ini, 'U3');

      DoToolbar_LoadContent(tbUser1, '1', false);
      DoToolbar_LoadContent(tbUser2, '2', false);
      DoToolbar_LoadContent(tbUser3, '3', false);
    end
    else
    begin
      tbFile.Visible:= Pos('File', opListerQVToolbars)>0;
      tbEdit.Visible:= Pos('Edit', opListerQVToolbars)>0;
      tbView.Visible:= Pos('View', opListerQVToolbars)>0;
      tbMenu.Visible:= Pos('Menu', opListerQVToolbars)>0;
      tbQS.Visible:=   Pos('QS',   opListerQVToolbars)>0;
      plTree.Visible:= Pos('Tree', opListerQVToolbars)>0;
      plClip.Visible:= Pos('Clip', opListerQVToolbars)>0;
      plOut.Visible:=  Pos('Output', opListerQVToolbars)>0;
      tbUser1.Visible:= Pos('User1', opListerQVToolbars)>0;
      tbUser2.Visible:= Pos('User2', opListerQVToolbars)>0;
      tbUser3.Visible:= Pos('User3', opListerQVToolbars)>0;
    end;

    //auto-save
    opASaveOnTimer:= ReadBool('ASave', 'OnTime', false);
    opASaveOnFocus:= ReadBool('ASave', 'OnFocus', false);
    opASaveTimeMin:= ReadInteger('ASave', 'Time', 5);
    opASaveAllFiles:= ReadBool('ASave', 'AllF', true);
    opASaveMaxSizeKb:= ReadInteger('ASave', 'MaxSize', 0);
    opASaveUnnamed:= TSynAutoSaveUnnamed(ReadInteger('ASave', 'Unnm', 0));
    opASaveUnnamedDir:= ReadString('ASave', 'UnnmDir', '%AppData%\SynWrite\AutoSave');
    ApplyAutoSave;

    //hist
    opLastDirMode:= TSynLastDirMode(ReadInteger('Hist', 'DirVar', Ord(cLastDirRemember)));
    opLastDirPath:= UTF8Decode(ReadString('Hist', 'Dir', ''));
    opLastDirSession:= SExpandFilenameDrive(UTF8Decode(ReadString('Hist', 'DirSess', SynDir)), SynDir);
    opLastDirProject:= SExpandFilenameDrive(UTF8Decode(ReadString('Hist', 'DirProj', SynDir)), SynDir);
    opHistFilter:= ReadInteger('Hist', 'Filter', 0);

    if SynExe then
    begin
      opHistSessionSave:= ReadBool('Hist', 'SessSave', false);
      opHistSessionLoad:= ReadBool('Hist', 'SessLoad', false);
      opHistSessionProjSave:= ReadBool('Hist', 'SessProjSave', false);
      opHistSessionProjLoad:= ReadBool('Hist', 'SessProjLoad', false);
      opHistSessionDef:= ReadBool('Hist', 'SessDef', false);
    end
    else
    begin
      opHistSessionSave:= false;
      opHistSessionLoad:= false;
      opHistSessionProjSave:= false;
      opHistSessionProjLoad:= false;
      opHistSessionDef:= false;
    end;

    opHistProjectSave:= ReadBool('Hist', 'ProjSv', false);
    opHistProjectLoad:= ReadBool('Hist', 'ProjLd', false);
    opHistProjectCloseTabs:= ReadBool('Hist', 'ProjCloseTabs', false);
    opSaveWndPos:= ReadBool('Hist', 'SavePos', true);

    //setup
    opClipHook:= ReadBool('Setup', 'ClipHook', true);
    TemplateEditor.UndoLimit:= ReadInteger('Setup', 'Undo', 5000);
    opHintScroll:= ReadBool('Setup', 'HintScroll', false);
    opShowBookmarkColumn:= ReadBool('Setup', 'ShowBm', true);
    opStapleOffset:= ReadInteger('Setup', 'StapleOffset', 1);
    opStapleKind:= ReadInteger('Setup', 'StapleKind', 0);
    opPyChangeDelay:= ReadInteger('Setup', 'PyChangeDelay', 3000);

    opShowPanelTitles:= ReadBool('View', 'PaneTitle', true);
    ApplyPanelTitles;

    //Options and OptionsEx
    NFlags:= HexStrToLongWord(ReadString('Setup', 'Flags', ''), Ok);
    if Ok then
      TemplateEditor.Options:= TSyntaxMemoOptions(NFlags);

    NFlags:= HexStrToLongWord(ReadString('Setup', 'FlagsEx', ''), Ok);
    if Ok then
      TemplateEditor.OptionsEx:= TSyntaxMemoOptionsEx(LongWord(NFlags));

    //color array
    DoColorsArrayInit(ColorsArray);
    DoColorsArrayRead(ColorsArray, ReadString('View', 'Colors', ''));
    DoColorsArrayApply(ColorsArray, TemplateEditor);

    opTreeSorted:= ReadString('Setup', 'TreeSorted', '');
    opUnderlineColored:= ReadInteger('Setup', 'ColorUnd', 3);
    opSyncEditIcon:= ReadBool('Setup', 'SyncEditIcon', true);

    opNewEnc:= ReadInteger('Setup', 'NEnc', 0);
    opNewLineEnds:= ReadInteger('Setup', 'NLe', 0);
    opNewLexer:= ReadString('Setup', 'NLex', cLexerText);

    opAutoCloseTags:= ReadBool('Setup', 'ACloseTag', false);
    opAutoCloseBrackets:= ReadBool('Setup', 'ACloseBr', false);
    opAutoCloseBracketsNoEsc:= ReadBool('Setup', 'ACloseBrEsc', false);
    opAutoCloseQuotes1:= ReadBool('Setup', 'ACloseQ', false);
    opAutoCloseQuotes2:= ReadBool('Setup', 'ACloseQ2', false);

    opLexerGroups:= ReadBool('Setup', 'LexCat', true);
    opLexersOverride:= ReadString('Setup', 'LexOvr', '');

    if QuickView then
      opTabVisible:= false
    else
      opTabVisible:= ReadBool('Setup', 'TabShow', true);

    opTabWidthMin:= ReadInteger('Setup', 'TabSizeMin', 20);
    opTabWidthMax:= ReadInteger('Setup', 'TabSize', 160);
    opTabEntireColor:= ReadBool('View', 'TabEntire', false);
    opTabDblClickClose:= ReadBool('View', 'TabDblClose', true);
    opTabAngle:= ReadInteger('View', 'TabAngle', 0);
    opTabDragDrop:= true;
    opTabFolders:= ReadBool('View', 'TabDirs', false);
    opTabNums:= ReadBool('View', 'TabNum', false);
    opTabXButtons:= ReadBool('View', 'TabBtn', true);
    opTabPlus:= ReadBool('View', 'TabPlus', true);
    opTabAtBottom:= ReadBool('View', 'TabDown', false);
    opTabSwitcher:= ReadBool('Setup', 'TabSw', true);

    opSingleInstance:= ReadBool('Setup', 'Inst', false);
    ApplyInst;
    opShowQsCaptions:= ReadBool('Setup', 'QsCap', false);
    ApplyQs;
    opHiliteUrls:= ReadBool('Setup', 'Link', true);
    opKeepCaretOnScreen:= false;
    ApplyEdOptions;

    opShowWrapMark:= ReadBool('Setup', 'WrapMk', true);
    opTextOnly:= TSynBinaryAct(ReadInteger('Setup', 'TxOnly', 0));
    opSaveFindCount:= ReadInteger('Setup', 'SaveSRHist', 10);
    opSaveFileCount:= ReadInteger('Setup', 'SaveFrameState', 10);
    byte(opSaveEditor):= ReadInteger('Setup', 'SaveProps', -1);
    opAskOverwrite:= true; //ReadBool('Setup', 'AskRO', true);
    opShowTitleFull:= ReadBool('Setup', 'TitleFull', false);

    //fonts
    StringToFont(TemplateEditor.Font,             ReadString('Fonts', 'Ed', ''));
    StringToFont(TemplateEditor.HorzRuler.Font,   ReadString('Fonts', 'Ruler', ''));
    StringToFont(TemplateEditor.LineNumbers.Font, ReadString('Fonts', 'Nums', ''));
    StringToFont(ListOut.Font,                    ReadString('Fonts', 'Out', ''));
    StringToFont(ecACP.Font,                      ReadString('Fonts', 'Acp', ''));
    StringToFont(Tree.Font,                       ReadString('Fonts', 'Tree', ''));
    StringToFont(MemoConsole.Font,                ReadString('Fonts', 'Con', ''));
    StringToFont(FFontTabs,                       ReadString('Fonts', 'Tabs', ''));
    StringToFont(FFontMenus,                      ReadString('Fonts', 'Menus', ''));
    EdConsole.Font:= MemoConsole.Font;

    //keys
    SyntKeyMapping.UseFirstControlKeys:= ReadBool('Setup', 'KeyComboIgnoreCtrl', true);

    //status props
    opStatusText[selNone]:= ReadString('View', 'StatusNoSel', '{LineNum} : {ColNum} ({TotalLines})');
    opStatusText[selSmall]:= ReadString('View', 'StatusSmallSel', '{LineNum} : {ColNum} ({SelLines}x{SelCols}/{TotalLines})');
    opStatusText[selStream]:= ReadString('View', 'StatusStreamSel', '{LineNum} : {ColNum} ({SelLines}/{TotalLines})');
    opStatusText[selColumn]:= ReadString('View', 'StatusColumnSel', '{LineNum} : {ColNum} ({SelLines}x{SelCols}/{TotalLines})');
    opStatusText[selCarets]:= ReadString('View', 'StatusCarets', '#={Carets} ({TotalLines})');

    with StatusItemCaret do
    begin
      MinWidth:= ReadInteger('View', 'StatusWidth', 155);
      case ReadInteger('View', 'StatusAlign', 0) of
        0: Alignment:= taCenter;
        1: Alignment:= taLeftJustify;
        2: Alignment:= taRightJustify;
      end;
    end;

    opBeep:= ReadBool('Setup', 'Beep', true);
    opUtf8BufferSizeKb:= ReadInteger('Setup', 'Utf8Buffer', 64);
    opShowMenuIcons:= ReadBool('Setup', 'MenuIcon', false);
    ApplyShowIconsInMenus;

    opHiliteSmart:= ReadBool('Setup', 'SmHi', false);
    opHiliteSmartCase:= ReadBool('Setup', 'SmHiCase', false);
    opHiliteSmartWords:= ReadBool('Setup', 'SmHiWords', true);
    opHiliteSmartOnClick:= ReadBool('Setup', 'SmHiClick', false);

    opDateFmt:= ReadString('Setup', 'DateFmt', 'h:mm dd.mm.yyyy');
    opDateFmtPLog:= ReadString('Setup', 'DateFmtP', 'hh:mm');
    opFileBackup:= TSynBackup(ReadInteger('Setup', 'Back', 0));
    opEsc:= TSynEscMode(ReadInteger('Setup', 'Esc' + cExeSuffix[SynExe], Ord(cEscCloseApp)));
    opMruCheck:= ReadBool('Setup', 'MruCheck', false);
    opTabsReplace:= ReadBool('Setup', 'TabSp', false);

    opSpellEn:= ReadBool('Setup', 'SpellEn', false);
    opSpellExt:= ReadString('Setup', 'SpellExt', 'txt,diz');

    opAcpUseSingle:= ReadBool('ACP', 'UseSingl', false);
    opAcpHtm:= ReadBool('ACP', 'Htm', true);
    opAcpCss:= ReadBool('ACP', 'Css', true);
    opAcpTabbing:= ReadBool('ACP', 'Tabbing', true);
    opAcpFile:= ReadBool('ACP', 'File', true);
    opAcpFileChars:= ReadInteger('ACP', 'FChars', 3);
    opAcpFileSize:= ReadFloat('ACP', 'FSize', 2.0);
    opAcpNum:= ReadInteger('ACP', 'Num', 0);
    opAcpHintDelay:= ReadInteger('ACP', 'HintDelay', 1500);
    ecACP.ShowWhenNone:= ReadBool('ACP', 'IfNone', true);
    opTemplateTabbing:= true; //ReadBool('ACP', 'TplTab', true);
    opTemplateTabbingExcept:= ReadString('ACP', 'TplTabEx', 'txt,nfo,diz');
    ParamCompletion.Enabled:= ReadBool('ACP', 'ParamHints', true);

    opReloadMode:= TSynReloadMode(ReadInteger('Setup', 'Notif', Ord(cReloadAsk)));
    ApplyBorders;

    opTipsToken:= ReadBool('Setup', 'Tooltips', true);
    opTipsPanels:= opTipsToken;
    ApplyTips;

    opSingleClickURL:= ReadBool('Setup', 'UrlClick', false);
    ApplyUrlClick;

    opLeftRightSelJump:= ReadBool('Setup', 'LeftRtJump', true);
    opSortMode:= TSynSortMode(ReadInteger('Setup', 'SortM', 0));
    opCopyLineIfNoSel:= ReadBool('Setup', 'CopyLnNoSel', false);
    opShowRecentColors:= TSynRecentColors(ReadInteger('Setup', 'RecColors', 0));
    opUnicodeNeeded:= ReadInteger('Setup', 'UnNeed', 0{don't suggest});
    opFollowTail:= ReadBool('Setup', 'Tail', false);

    S:= ReadString('Setup', 'MenuSize', '');
    opShowMenuSizeX:= StrToIntDef(SGetItem(S), 520);
    opShowMenuSizeY:= StrToIntDef(SGetItem(S), 320);

    Tree.ClickAction:= TSyntaxTreeAction(ReadInteger('Tree', 'Click', Ord(Tree.ClickAction)));
    Tree.Color:= ReadInteger('Tree', 'Color', Tree.Color);
    Tree.AutoSynchronize:= ReadBool('Tree', 'ASync', true);
    Tree.AutoCollapse:= ReadBool('Tree', 'ACollapse', false);
    Tree.AutoExpand:= ReadBool('Tree', 'AExpand', false);
    Tree.UpdateDelay:= ReadInteger('Tree', 'Delay', 1000);

    opFindOffsetTop:= ReadInteger('SR', 'OffY', 6);
    opFindExpand:= ReadBool('SR', 'Expand', false);
    opFindOnTop:= ReadBool('SR', 'ShowOnTop' + cExeSuffix[SynExe], SynExe);
    opFindSuggestSel:= ReadBool('SR', 'SugSel', false);
    opFindSuggestWord:= ReadBool('SR', 'SugWord', false);
    opMaxTreeMatches:= ReadInteger('SR', 'MaxTreeMatches', 100);

    opTabOptionsLast:= ReadInteger('View', 'TabLast', 0);
    opTabsSortMode:= ReadInteger('View', 'TabSort', 0);

    opMinimapFontSize:= ReadInteger('Setup', 'MinimapFont', 1);
    opMicroMap:= ReadBool('View', 'MicroMap', false);
    opShowCurrentColumn:= ReadBool('View', 'CurrCol', false);
    opCaretShapeIns:= TATCaretShape(ReadInteger('View', 'CaretType', Ord(cCrVert1px)));
    opCaretShapeOvr:= TATCaretShape(ReadInteger('View', 'CaretTypeOvr', Ord(cCrFull)));
    opCaretTime:= ReadInteger('View', 'CaretTime', Windows.GetCaretBlinkTime);

    NCount:= ReadInteger('View', 'NPrint', 0+2+4);
    opNonPrint:=          (NCount and 1)<>0;
    opNonPrintSpaces:=    (NCount and 2)<>0;
    opNonPrintEol:=       (NCount and 4)<>0;
    opNonPrintEolDetail:= (NCount and 8)<>0;

    opBigSize:= ReadInteger('Setup', 'BigSize', 4);
    opBkUndo:= ReadBool('Setup', 'BkUndo', false);
    opProjPaths:= UTF8Decode(ReadString('Setup', 'Paths', ''));
    opHiliteBrackets:= ReadBool('Setup', 'BrHi', true);

    opCaretsEnabled:= ReadBool('View', 'CaretsEn', true);
    opCaretsIndicator:= ReadInteger('Setup', 'CaretInd', 2);
    opCaretsGutterBand:= 0;
    ApplyCarets;

    TabColorsString:= ReadString('View', 'TabMisc', '');

    opShowCharInfo:= ReadBool('Setup', 'ChInf', false);
    opLang:= ReadInteger('Setup', 'Lang', 0);
    Status.Visible:= ReadBool('Setup', 'Stat', true);
    //if not QuickView then
    //  Menu.Visible:= ReadBool('Setup', 'Menu' + cExeSuffix[SynExe], true);
    opOpenAsOem:= ReadString('Setup', 'Oem', 'bat,cmd,nfo,diz');
    opOpenAsUtf8:= ReadString('Setup', 'UTF8', '');

    if SynExe or not QuickView then
      opTheme:= ReadString('Setup', 'Theme', cThemeDefault)
	  else
      opTheme:= cThemeWindows;
    opIcons:= ReadString('Setup', 'Icons', cIconsDefault);

    LoadPanelProp(plTree, Ini, 'Tree');
    LoadPanelProp(plOut, Ini, 'Out');
    LoadPanelProp(plClip, Ini, 'Clip');
    FOutVisible:= plOut.Visible;

    FTabLeft:= ReadInteger('plTree', 'Tab', 0);
    FTabRight:= ReadInteger('plClip', 'Tab', 0);
    FTabOut:= ReadInteger('plOut', 'Tab', 0);
    if FTabOut=Ord(tbPluginsLog) then //don't restore last avtive Log panel
      FTabOut:= Ord(tbOutput);

    //opt
    FInitialKeyCount:= SyntKeymapping.Items.Count;
    PropsManager.LoadProps(ini);
    DoKeymappingTruncate(SyntKeyMapping, FInitialKeyCount);

    //force KeepSelMode and FloatMarkers
    with TemplateEditor do
    begin
      Options:= Options + [soKeepSelMode, soFloatMarkers];
      OptionsEx:= OptionsEx + [soKeepSearchMarks];
    end;

    ApplyACP;
    ApplyOut;
    ApplyFonts;

    //save orig opt
    orig_Wrap:= TemplateEditor.WordWrap;
    orig_LNum:= TemplateEditor.LineNumbers.Visible;
    orig_NPrint:= TemplateEditor.NonPrinted.Visible;
    orig_NFold:= TemplateEditor.DisableFolding;
    orig_Ruler:= TemplateEditor.HorzRuler.Visible;
    orig_Tree:= plTree.Visible;
    orig_Out:= {plOut.Visible}FOutVisible;
    orig_Clip:= plClip.Visible;
    orig_TabLeft:= FTabLeft;
    orig_TabRight:= FTabRight;
    orig_TabOut:= FTabOut;
    orig_TabsSort:= opTabsSortMode;
    orig_ListTabsCols:= ListTabsColumns;
    orig_ListBkmkCols:= ListBkmkColumns;
  finally
    Free;
  end;

  opGroupMode:= gmOne;

  if SynExe or opMruForPlugin then
  begin
    Ini:= TMemIniFile.Create(SynHistoryIni);
    with Ini do
    try
      S:= ReadString('Win', 'Groups', '');
      opGroupMode:= TATGroupsMode(StrToIntDef(SGetItem(S), Ord(gmOne)));
      opGroupSplit:= StrToIntDef(SGetItem(S), 50);

      ListTabsColumns:= ReadString('Win', 'ColsTabWidth', '');
      ListBkmkColumns:= ReadString('Win', 'ColsBkmk', '');

      //load recent files
      LoadMruList(SynMruFiles, Ini, 'MRU', opSaveFileCount, opMruCheck);

      //load recent sessions
      LoadMruList(SynMruSessions, Ini, 'MRU_Sess', opSaveFileCount, opMruCheck);

      //load recent project
      if opHistProjectLoad and (SynCommandlineProjectFN='') then
      begin
        LoadProj;
        if Assigned(fmProj) then
        begin
          S:= UTF8Decode(ReadString('MRU', 'Proj', ''));
          if (S<>'') and IsFileExist(S) then
            try
              FProjectIniting:= true;
              fmProj.ProjectFN:= S;
            finally
              FProjectIniting:= false;
            end;
        end;
      end;

      //load recent colors
      RecentColorsStr:= ReadString('Hist', 'Colors', '');
    finally
      Free
    end;
  end;

  //lang
  LangManager.LanguageID:= opLang;
  UpdateLang;
  UpdateShortcuts;
end;

//save only options visible on window
procedure TfmMain.SaveOptionsRecent;
const
  S: array[boolean] of string = ('False', 'True');
var
  Ini: TIniFile;
begin
  Ini:= TIniFile.Create(SynIni);
  with Ini do
  try
    if TemplateEditor.WordWrap <> orig_Wrap then
      WriteString('Template', 'WordWrap', S[TemplateEditor.WordWrap]);
    if TemplateEditor.LineNumbers.Visible <> orig_LNum then
      WriteString('Template', 'LineNumbers.Visible', S[TemplateEditor.LineNumbers.Visible]);
    if TemplateEditor.NonPrinted.Visible <> orig_NPrint then
      WriteString('Template', 'NonPrinted.Visible', S[TemplateEditor.NonPrinted.Visible]);
    if TemplateEditor.DisableFolding <> orig_NFold then
      WriteString('Template', 'DisableFolding', S[TemplateEditor.DisableFolding]);
    if TemplateEditor.HorzRuler.Visible <> orig_Ruler then
      WriteString('Template', 'HorzRuler.Visible', S[TemplateEditor.HorzRuler.Visible]);
    if plTree.Visible <> orig_Tree then
      WriteBool('plTree', 'Vis', plTree.Visible);
    if FOutVisible <> orig_Out then
      WriteBool('plOut', 'Vis', FOutVisible);
    if plClip.Visible <> orig_Clip then
      WriteBool('plClip', 'Vis', plClip.Visible);
    if TabsLeft.TabIndex <> orig_TabLeft then
      WriteInteger('plTree', 'Tab', TabsLeft.TabIndex);
    if TabsRight.TabIndex <> orig_TabRight then
      WriteInteger('plClip', 'Tab', TabsRight.TabIndex);
    if TabsOut.TabIndex <> orig_TabOut then
      WriteInteger('plOut', 'Tab', TabsOut.TabIndex);
    if opTabsSortMode <> orig_TabsSort then
      WriteInteger('View', 'TabSort', opTabsSortMode);

    //save toolbars and panels
    if not ShowFullScreen then
      if FToolbarMoved then
      begin
        SaveToolbarsProps;
        SavePanelProp(plTree, Ini, 'Tree');
        SavePanelProp(plOut, Ini, 'Out');
        SavePanelProp(plClip, Ini, 'Clip');
      end;
  finally
    Free;
  end;

  Ini:= TIniFile.Create(SynHistoryIni);
  with Ini do
  try
    WriteString('Win', 'Groups', Format('%d,%d', [Ord(Groups.Mode), Groups.SplitPos]));

    if ListTabsColumns <> orig_ListTabsCols then
      WriteString('Win', 'ColsTabWidth', ListTabsColumns);
    if ListBkmkColumns <> orig_ListBkmkCols then
      WriteString('Win', 'ColsBkmk', ListBkmkColumns);

    //save Clipbd panel
    if Assigned(fmClips) then
      WriteString('Win', 'Clip', fmClips.Combo.Text);

    //save NumConv panel
    if Assigned(fmNumConv) then
    begin
      WriteInteger('Win', 'NConvX', fmNumConv.Left);
      WriteInteger('Win', 'NConvY', fmNumConv.Top);
    end;

    //save recent colors
    WriteString('Hist', 'Colors', RecentColorsStr);

    if SynExe or opMruForPlugin then
    begin
      //save recent files list
      SaveMruList(SynMruFiles, Ini, 'MRU');

      //save recent session list
      SaveMruList(SynMruSessions, Ini, 'MRU_Sess');

      //save project name
      if Assigned(fmProj) then
        WriteString('MRU', 'Proj', UTF8Encode(fmProj.ProjectFN));
    end;
  finally
    Free;
  end;
end;

//save all
procedure TfmMain.SaveOptionsAll;
var
  f: TIniFile;
begin
  if not SynExe then
    with TIniFile.Create(SynListerIni) do
    try
      WriteInteger('Syn2', 'TxOnly', Ord(opTextOnly));
    finally
      Free;
    end;

 try
  f:= TIniFile.Create(SynIni);
  with f do
  try
    //editor props
    PropsManager.SaveProps(f);

    //view
    WriteBool('View', 'MicroMap', opMicroMap);
    WriteBool('View', 'CurrCol', opShowCurrentColumn);
    WriteInteger('View', 'CaretType', Ord(opCaretShapeIns));
    WriteInteger('View', 'CaretTypeOvr', Ord(opCaretShapeOvr));
    WriteInteger('View', 'CaretTime', opCaretTime);

    WriteInteger('View', 'NPrint',
      Ord(opNonPrint)*1 +
      Ord(opNonPrintSpaces)*2 +
      Ord(opNonPrintEol)*4 +
      Ord(opNonPrintEolDetail)*8);

    //auto-save
    WriteBool('ASave', 'OnTime', opASaveOnTimer);
    WriteBool('ASave', 'OnFocus', opASaveOnFocus);
    WriteInteger('ASave', 'Time', opASaveTimeMin);
    WriteBool('ASave', 'AllF', opASaveAllFiles);
    WriteInteger('ASave', 'MaxSize', opASaveMaxSizeKb);
    WriteInteger('ASave', 'Unnm', Ord(opASaveUnnamed));
    WriteString('ASave', 'UnnmDir', opASaveUnnamedDir);

    //setup
    WriteBool('Setup', 'ClipHook', opClipHook);
    WriteInteger('Setup', 'Undo', TemplateEditor.UndoLimit);
    WriteInteger('Setup', 'BigSize', opBigSize);
    WriteBool('Setup', 'BkUndo', opBkUndo);
    WriteBool('Setup', 'BrHi', opHiliteBrackets);
    WriteBool('Setup', 'ShowBm', opShowBookmarkColumn);
    WriteInteger('Setup', 'StapleKind', opStapleKind);

    WriteString('Setup', 'Paths', UTF8Encode(opProjPaths));
    WriteBool('Setup', 'SpellEn', opSpellEn);
    WriteString('Setup', 'SpellExt', opSpellExt);

    if SynExe then
    begin
      WriteBool('Hist', 'SessSave', opHistSessionSave);
      WriteBool('Hist', 'SessLoad', opHistSessionLoad);
      WriteBool('Hist', 'SessProjSave', opHistSessionProjSave);
      WriteBool('Hist', 'SessProjLoad', opHistSessionProjLoad);
      WriteBool('Hist', 'SessDef', opHistSessionDef);
    end;

    WriteBool('Hist', 'ProjSv', opHistProjectSave);
    WriteBool('Hist', 'ProjLd', opHistProjectLoad);
    WriteBool('Hist', 'ProjCloseTabs', opHistProjectCloseTabs);
    WriteBool('Hist', 'SavePos', opSaveWndPos);

    WriteInteger('Setup', 'NEnc', opNewEnc);
    WriteInteger('Setup', 'NLe', opNewLineEnds);
    WriteString('Setup', 'NLex', opNewLexer);

    WriteBool('Setup', 'ACloseTag', opAutoCloseTags);
    WriteBool('Setup', 'ACloseBr', opAutoCloseBrackets);
    WriteBool('Setup', 'ACloseBrEsc', opAutoCloseBracketsNoEsc);
    WriteBool('Setup', 'ACloseQ', opAutoCloseQuotes1);
    WriteBool('Setup', 'ACloseQ2', opAutoCloseQuotes2);

    WriteBool('Setup', 'TabShow', opTabVisible);
    WriteInteger('Setup', 'TabSizeMin', opTabWidthMin);
    WriteInteger('Setup', 'TabSize', opTabWidthMax);
    WriteBool('Setup', 'TabDnD', opTabDragDrop);
    WriteBool('Setup', 'TabSw', opTabSwitcher);

    WriteString('Setup', 'LexOvr', opLexersOverride);
    WriteBool('Setup', 'Inst', opSingleInstance);
    WriteBool('Setup', 'QsCap', opShowQsCaptions);
    WriteBool('Setup', 'LexCat', opLexerGroups);
    WriteBool('Setup', 'Link', opHiliteUrls);
    WriteBool('Setup', 'WrapMk', opShowWrapMark);
    WriteInteger('Setup', 'TxOnly', Ord(opTextOnly));

    WriteInteger('Setup', 'SaveSRHist', opSaveFindCount);
    WriteInteger('Setup', 'SaveFrameState', opSaveFileCount);
    WriteInteger('Setup', 'SaveProps', byte(opSaveEditor));
    WriteBool('Setup', 'AskRO', opAskOverwrite);
    WriteBool('Setup', 'TitleFull', opShowTitleFull);

    WriteInteger('Hist', 'DirVar', Ord(opLastDirMode));
    if opLastDirMode=cLastDirCustom then
      WriteString('Hist', 'Dir', UTF8Encode(opLastDirPath));

    WriteBool('Setup', 'MenuIcon', opShowMenuIcons);
    WriteBool('Setup', 'Beep', opBeep);

    WriteBool('Setup', 'SmHi', opHiliteSmart);
    WriteBool('Setup', 'SmHiCase', opHiliteSmartCase);
    WriteBool('Setup', 'SmHiWords', opHiliteSmartWords);
    WriteBool('Setup', 'SmHiClick', opHiliteSmartOnClick);

    WriteString('Setup', 'DateFmt', opDateFmt);
    WriteString('Setup', 'DateFmtP', opDateFmtPLog);
    WriteInteger('Setup', 'Back', Ord(opFileBackup));
    WriteInteger('Setup', 'Esc' + cExeSuffix[SynExe], Ord(opEsc));
    WriteBool('Setup', 'MruCheck', opMruCheck);
    WriteBool('Setup', 'TabSp', opTabsReplace);
    WriteInteger('Setup', 'Notif', Ord(opReloadMode));

    WriteBool('Setup', 'LeftRtJump', opLeftRightSelJump);
    WriteBool('Setup', 'Tail', opFollowTail);
    WriteInteger('Setup', 'UnNeed', opUnicodeNeeded);
    WriteInteger('Setup', 'RecColors', Ord(opShowRecentColors));
    WriteBool('Setup', 'CopyLnNoSel', opCopyLineIfNoSel);
    WriteInteger('Setup', 'SortM', Ord(opSortMode));
    WriteBool('Setup', 'UrlClick', opSingleClickURL);
    WriteInteger('Setup', 'ColorUnd', opUnderlineColored);
    WriteString('Setup', 'TreeSorted', opTreeSorted);
    WriteBool('Setup', 'SyncEditIcon', opSyncEditIcon);

    WriteString('Setup', 'Flags', IntToHex(LongWord(TemplateEditor.Options), 8));
    WriteString('Setup', 'FlagsEx', IntToHex(LongWord(TemplateEditor.OptionsEx), 8));

    WriteString('View', 'Colors', DoColorsArrayAsString(ColorsArray));
    WriteBool('View', 'CaretsEn', opCaretsEnabled);

    WriteInteger('Tree', 'Click', Ord(Tree.ClickAction));
    WriteInteger('Tree', 'Color', Tree.Color);
    WriteBool('Tree', 'ASync', Tree.AutoSynchronize);
    WriteBool('Tree', 'ACollapse', Tree.AutoCollapse);
    WriteBool('Tree', 'AExpand', Tree.AutoExpand);
    WriteInteger('Tree', 'Delay', Tree.UpdateDelay);

    //WriteBool('ACP', 'TplTab', opTemplateTabbing);
    WriteString('ACP', 'TplTabEx', opTemplateTabbingExcept);
    WriteBool('ACP', 'UseSingl', opAcpUseSingle);
    WriteBool('ACP', 'Htm', opAcpHtm);
    WriteBool('ACP', 'Css', opAcpCss);
    WriteBool('ACP', 'Tabbing', opAcpTabbing);
    WriteBool('ACP', 'File', opAcpFile);
    WriteInteger('ACP', 'FChars', opAcpFileChars);
    WriteFloat('ACP', 'FSize', opAcpFileSize);
    WriteInteger('ACP', 'Num', opAcpNum);
    WriteInteger('ACP', 'HintDelay', opAcpHintDelay);
    WriteBool('ACP', 'IfNone', ecACP.ShowWhenNone);
    WriteBool('ACP', 'ParamHints', ParamCompletion.Enabled);

    WriteInteger('SR', 'OffY', opFindOffsetTop);
    WriteBool('SR', 'Expand', opFindExpand);
    WriteBool('SR', 'ShowOnTop' + cExeSuffix[SynExe], opFindOnTop);
    WriteBool('SR', 'SugSel', opFindSuggestSel);
    WriteBool('SR', 'SugWord', opFindSuggestWord);
    WriteInteger('SR', 'MaxTreeMatches', opMaxTreeMatches);

    WriteBool('View', 'TabEntire', opTabEntireColor);
    WriteBool('View', 'TabDblClose', opTabDblClickClose);
    WriteInteger('View', 'TabAngle', opTabAngle);
    WriteInteger('View', 'TabLast', opTabOptionsLast);
    WriteBool('View', 'TabDirs', opTabFolders);
    WriteBool('View', 'TabNum', opTabNums);
    WriteBool('View', 'TabBtn', opTabXButtons);
    WriteBool('View', 'TabPlus', opTabPlus);
    WriteBool('View', 'TabDown', opTabAtBottom);
    WriteString('View', 'TabMisc', TabColorsString);

    WriteBool('Setup', 'ChInf', opShowCharInfo);
    WriteInteger('Setup', 'Lang', opLang);
    //if not QuickView then
    //  WriteBool('Setup', 'Menu' + cExeSuffix[SynExe], Menu.Visible);
    WriteBool('Setup', 'Stat', Status.Visible);
    WriteString('Setup', 'Oem', opOpenAsOem);
    WriteString('Setup', 'UTF8', opOpenAsUtf8);
    WriteString('Setup', 'Theme', opTheme);
    WriteString('Setup', 'Icons', opIcons);

    //fonts
    WriteString('Fonts', 'Ed', FontToString(TemplateEditor.Font));
    WriteString('Fonts', 'Ruler', FontToString(TemplateEditor.HorzRuler.Font));
    WriteString('Fonts', 'Nums', FontToString(TemplateEditor.LineNumbers.Font));
    WriteString('Fonts', 'Out', FontToString(ListOut.Font));
    WriteString('Fonts', 'Acp', FontToString(ecACP.Font));
    WriteString('Fonts', 'Tree', FontToString(Tree.Font));
    WriteString('Fonts', 'Con', FontToString(MemoConsole.Font));
    WriteString('Fonts', 'Tabs', FontToString(FFontTabs));
    WriteString('Fonts', 'Menus', FontToString(FFontMenus));
  finally
    Free;
  end;
 except
   MsgError(DKLangConstW('zMCannotSaveIni'), Handle);
 end;
end;

function FAppDataPath: string;
begin
  Result:= SExpandVars('%AppData%\');
end;

procedure TfmMain.InitSynIniDir;
begin
  SynIniDir:= SynDir + 'Settings\';
end;

function TfmMain.SynPluginIni(const SCaption: string): string;
begin
  Result:= SynIniDir + 'SynPlugin' + SCaption + '.ini';
end;

function TfmMain.SynIni: string;
begin
  Result:= SynIniDir + 'Syn.ini';
end;

function TfmMain.SynToolbarsIni: string;
begin
  Result:= SynIniDir + 'SynToolbars.ini';
end;

function TfmMain.SynFavIni: string;
begin
  Result:= SynIniDir + 'SynFav.ini';
end;

function TfmMain.SynStylesIni: string;
begin
  Result:= SynIniDir + 'SynStyles.ini';
end;

function TfmMain.SynHistoryIni: string;
begin
  Result:= SynIniDir + 'SynHistory.ini';
end;

function TfmMain.SynHistoryStatesIni: string;
begin
  Result:= SynIniDir + 'SynHistoryStates.ini';
end;

function TfmMain.SynFoldStatesIni: string;
begin
  Result:= SynIniDir + 'SynFoldStates.ini';
end;

function TfmMain.SynMacrosIni: string;
begin
  Result:= SynIniDir + 'SynMacros.ini';
end;

function TfmMain.SynHideIni: string;
begin
  Result:= SynIniDir + 'SynHide.ini';
end;

function TfmMain.SynPluginsIni: string;
begin
  Result:= SynIniDir + 'SynPlugins.ini';
end;

function TfmMain.SynDataSubdir(Id: TSynDataSubdirId): string;
begin
  Result:= SynDir + 'Data\' + cSynDataSubdirNames[Id];
end;

function TfmMain.SynConverterFilename(const Name: string): string;
begin
  Result:= SynDataSubdir(cSynDataConv) + '\' + Name + '.txt';
end;

function TfmMain.SynLexersCfg: string;
begin
  Result:= SynIniDir + 'Lexers.cfg';
end;

function TfmMain.SynLexersExCfg: string;
begin
  Result:= SynIniDir + 'LexersEx.cfg';
end;

function TfmMain.LoadFrameState(Frame: TEditorFrame; const fn: WideString): boolean;
var
  fnIni: string;
  L: TStringList;
  i: integer;
begin
  Result:= false;
  if (opSaveFileCount=0) then Exit;
  if (fn='') or (Frame=nil) then Exit;
  if not (cSynHistoryForTemp in opSaveEditor) then
    if IsTempFN(fn) then Exit;

  fnIni:= SynHistoryStatesIni;
  if not IsFileExist(fnIni) then Exit;

  L:= TStringList.Create;
  try
    L.LoadFromFile(fnIni);
    for i:= 0 to L.Count-1 do
      if IsFramePropertiesStringForFilename(fn, L[i]) then
      begin
        Frame.FileName:= fn;
        Screen.Cursor:= crHourGlass;
        try
          Frame.EditorMaster.TextSource.Lines.Clear;
          Frame.EditorMaster.TextSource.Lines.SkipSignature:= true;
          FrameSetPropertiesString(Frame, L[i], true); //EncodingOnly=true
          Frame.EditorMaster.TextSource.Lines.LoadFromFile(fn); //uses set encoding
          Frame.EditorMaster.TextSource.Lines.SkipSignature:= false;
          FrameSetPropertiesString(Frame, L[i], false); //EncodingOnly=false
        finally
          Screen.Cursor:= crDefault;
        end;
        Result:= true;
        Break;
      end;
  finally
    FreeAndNil(L);
  end;
end;

procedure TfmMain.SaveFrameState(F: TEditorFrame);
var
  fnIni, sData: string;
  L: TStringList;
  i: integer;
begin
  if (opSaveFileCount=0) then Exit;
  if (F=nil) or (F.FileName='') then Exit;
  if not (cSynHistoryForTemp in opSaveEditor) then
    if IsTempFN(F.FileName) then Exit;

  sData:= FrameGetPropertiesString(F);
  fnIni:= SynHistoryStatesIni;

  L:= TStringList.Create;
  try
    if IsFileExist(fnIni) then
      L.LoadFromFile(fnIni);

    for i:= L.Count-1 downto 0 do
      if IsFramePropertiesStringForFilename(F.FileName, L[i]) then
        L.Delete(i);
    L.Insert(0, sData);

    while L.Count>opSaveFileCount do
      L.Delete(L.Count-1);

    try
      //file may be R/O
      L.SaveToFile(fnIni);
    except
    end;
  finally
    FreeAndNil(L);
  end;
end;

function TfmMain.DoConfirmClose: boolean;
var
  F: TEditorFrame;
  i: integer;
  S: Widestring;
begin
  Result:= false;

  with TfmSaveTabs.Create(Self) do
  try
    bCancel.Enabled:= SynExe;

    for i:= 0 to FrameAllCount-1 do
    begin
      F:= FramesAll[i];
      S:= F.TabCaption;
      if F.FileName<>'' then
        S:= S+'  ('+F.FileName+')';
      if F.Modified then
        List.Items.AddObject(S, F);
    end;

    if List.Items.Count>0 then
      case ShowModal of
        mrCancel:
          Exit;
        mrYes:
          begin
            for i:= 0 to List.Items.Count-1 do
              if List.Checked[i] then
                SaveFrame(List.Items.Objects[i] as TEditorFrame, false);
          end;
      end;
  finally
    Free;
  end;

  if Assigned(fmProj) then
  begin
    if opHistProjectSave then
      fmProj.DoSaveProjectIfNeeded
    else
    begin
      if not fmProj.DoConfirmClose(true) then Exit;
    end;
  end;

  Result:= true;
end;

function TfmMain.SynLexLib: string;
begin
  Result:= SynDir + 'LexLib.lxl';
end;

procedure TfmMain.SaveLexLib;
begin
  SyntaxManager.SaveToFile(SyntaxManager.FileName);
end;

function TfmMain.DoConfirmSaveLexLib: boolean;
begin
  Result:= true;
  if SyntaxManager.Modified then
  begin
    if MsgConfirm(DKLangConstW('MSavLex'), Handle) then
      SaveLexLib;
    SyntaxManager.Modified:= false;
  end;
end;

procedure TfmMain.SynKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Ed=nil then Exit;

  if DoPyEvent(Ed, cSynEventOnKey,
    [IntToStr(Key), '"'+ShiftStateToString(Shift)+'"']) = cPyFalse then
    begin Key:= 0; Exit end;

  if not SynExe then
  if Ed.ReadOnly or (Shift = [ssAlt]) then
  begin
    //Lister: File -> Next (N) or Prev (P)
    if Chr(Lo(Key)) in ['N', 'P'] then
    begin
      PostMessage(hLister, WM_KEYDOWN, Key, 0);
      Key:= 0;
      Exit
    end
    else
    //Lister: Options -> 1..7
    if Chr(Lo(Key)) in ['1'..'7'] then
    begin
      PostMessage(hLister, WM_KEYDOWN, Key, 0);
      Key:= 0;
      Exit
    end;
  end;

  if Ed.ReadOnly and (Key = VK_SPACE) and (Shift = []) then
  begin
    CurrentEditor.ExecCommand(smScrollPageDown);
    Key:= 0;
    Exit
  end;

  //handle Tab key if auto-completion popup is shown
  if not Ed.ReadOnly and (Key = vk_tab) and (Shift = []) then
    if ecACP.Visible or PluginACP.Visible then
    begin
      Key:= 0;
      Exit;
    end;
end;

procedure TfmMain.DoHandleLastCmd(Command: integer; Data: pointer);
var
  IsEdit: boolean;
begin
  IsEdit:= IsCmdEditing(Command);
  if IsEdit and not FLastCmdPlaying then
  begin
    if (FLastCmdId <> Command) or
      (FLastCmdData <> PChar(Data)) or
      FLastCmdBreak then
      FLastCmdCount:= 0
    else
      Inc(FLastCmdCount);
    FLastCmdBreak:= false;
    FLastCmdId:= Command;
    if Data=nil then
      FLastCmdData:= ''
    else
      FLastCmdData:= PChar(Data);
  end;
  if not IsEdit then
    FLastCmdBreak:= true;
end;

procedure TfmMain.DoDelayedCommandAny(Command: Integer);
begin
  PostMessage(Handle, WM_USER + 1, Command, 0);
end;

procedure TfmMain.DoDelayedCommandWithClose(Command: Integer);
begin
  PostMessage(Handle, WM_USER + 2, Command, 0);
end;

procedure EditorSplitLinesWithDialog(Ed: TSyntaxMemo);
var
  s: Widestring;
  n: Integer;
begin
  n:= Ed.RightMargin;
  s:= IntToStr(n);
  if not MsgInput('MRtMargin', s) then Exit;
  n:= StrToIntDef(s, 0);
  if n>=3 then
    EditorSplitLinesByPosition(Ed, n)
  else
    MsgBeep;
end;

procedure TfmMain.SynExecuteCommand(Sender: TObject; Command: Integer;
  Data: Pointer; var Handled: Boolean);
var
  Ed: TSyntaxMemo;
  ch: WideChar;
  p: TPoint;
  n1, n2: integer;
begin
  //remember last edit cmd
  DoHandleLastCmd(Command, Data);

  Handled:= True;
  Ed:= Sender as TSyntaxMemo;

  //debug
  //Application.MainForm.Caption:= Format('cmd %d', [Command]);

  case Command of
    //auto-close tag
    smChar:
      begin
        Handled:= false;
        if Data<>nil then
          ch:= PWideChar(Data)^
        else
          ch:= #0;

        //if current char is 2nd part of key-combination, don't handle it
        //(consider also ecRepeatCmd.Execute context)
        if (SyntKeyMapping.IsHandledCmd(Ed.KeyQueue)<>0) and
          not FLastCmdPlaying and
          not ecMacroRec.Plying then
        begin
          Handled:= true;
          Exit
        end;

        if not IsWordChar(ch) then
          DoCheckAutoCorrectCase(Ed);

        if (ch='>') then
          Handled:= DoAutoCloseTag
        else
        if IsBracketChar(ch) or IsQuoteChar(ch) then
          Handled:= DoAutoCloseBracket(ch)
        else
        if IsWordChar(ch) then
        begin
          DoCheckAutoShowACP(Ed);
        end;
      end;

    smLineBreak:
      begin
        DoCheckAutoCorrectCase(Ed);
        Handled:= false;
      end;

    //case changing
    smUpperCaseBlock:
      EditorChangeBlockCase(Ed, cTextCaseUpper);
    smLowerCaseBlock:
      EditorChangeBlockCase(Ed, cTextCaseLower);
    smToggleCaseBlock:
      EditorChangeBlockCase(Ed, cTextCaseToggle);
    smTitleCaseBlock:
      EditorChangeBlockCase(Ed, cTextCaseTitle);
    sm_SentenceCaseBlock:
      EditorChangeBlockCase(Ed, cTextCaseSent);
    sm_RandomCaseBlock:
      EditorChangeBlockCase(Ed, cTextCaseRandom);

    //indent
    smTab:
      begin
        if Ed.IsTabstopMode then
        begin
          Ed.DoJumpToNextTabstop;
        end
        else
        if Ed.HaveSelection and EditorHasMultilineSelection(Ed) then
        begin
          Ed.ExecCommand(smBlockIndent);
          Exit; //don't record Tab cmd
        end
        else
        if DoSnippetTabbing then //snippets - before SmartTagTabbing
          begin end
        else
        if DoSmartTagTabbing then
          begin end
        else
          Handled:= false;
      end;

    smCopy:
      begin
        if not Ed.HaveSelection and opCopyLineIfNoSel then
          EditorCopyOrCutCurrentLine(Ed, false)
        else
          Handled:= false;
      end;
    smCut:
      begin
        if not Ed.HaveSelection and opCopyLineIfNoSel then
          EditorCopyOrCutCurrentLine(Ed, true)
        else
          Handled:= false;
      end;

    smBlockIndent,
    smBlockUnindent:
      begin
        if Ed.SelLength>0 then //only for stream blocks it works ok
        begin
          if Command=smBlockIndent then
            DoLinesCommand(cLineCmdIndent)
          else
            DoLinesCommand(cLineCmdUnIndent);
        end
        else
          Handled:= false;
      end;

    smScrollUp,
    smScrollDown:
      begin
        Handled:= false;
        if opKeepCaretOnScreen and not EditorHasNoCaret(Ed) then
          EditorKeepCaretOnScreen(Ed);
      end;

    smDuplicateLine:
      EditorDuplicateLine(Ed);

    smGotoLine:
      ecGoto.Execute;

    sm_AutoComplete:
      DoAcpCommand;

    //bkmarks
    sm_BookmarksClear:
      ecBkClearAll.Execute;
    sm_BookmarksToggle:
      ecBkToggle.Execute;
    sm_BookmarksPrev:
      ecBkPrev.Execute;
    sm_BookmarksNext:
      ecBkNext.Execute;
    sm_BookmarksCopy:
      ecBkCopy.Execute;
    sm_BookmarksCut:
      ecBkCut.Execute;
    sm_BookmarksDelete:
      ecBkDelete.Execute;
    sm_BookmarksDeleteUnmarked:
      ecBkDeleteUnmk.Execute;
    sm_BookmarksPaste:
      ecBkPaste.Execute;
    sm_BookmarksInverse:
      ecBkInverse.Execute;

    smPrint:
      ecPrintAction.Execute;
    smPrintPreview:
      ecPreviewActionNew.Execute;
    smPageSetup:
      ecPageSetupActionNew.Execute;
    sm_PrinterSetup:
      ecPrinterSetup.Execute;

    //scroll
    smLeft:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smScrollLeft)
      else
      if opLeftRightSelJump and (Ed.SelLength>0) and not (soPersistentBlocks in Ed.Options) then
      begin
        n1:= Ed.SelStart;
        if Ed.CaretStrPos<>n1 then
          Ed.CaretStrPos:= n1
        else
          Ed.ResetSelection;
      end
      else
        Handled:= False;
     end;

    smRight:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smScrollRight)
      else
      if opLeftRightSelJump and (Ed.SelLength>0) and not (soPersistentBlocks in Ed.Options) then
      begin
        n1:= Ed.SelStart;
        n2:= Ed.SelLength;
        if Ed.CaretStrPos<>n1+n2 then
          Ed.CaretStrPos:= n1+n2
        else
          Ed.ResetSelection;
      end
      else
        Handled:= False;
    end;

    smUp:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smScrollUp)
      else
        Handled:= False;
    end;
    smDown:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smScrollDown)
      else
        Handled:= False;
    end;
    smPageUp:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smScrollPageUp)
      else
        Handled:= False;
    end;
    smPageDown:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smScrollPageDown)
      else
        Handled:= False;
    end;
    //Home key
    smFirstLetter:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smEditorTop{smScrollPageLeft})
      else
        Handled:= false;
    end;
    //End key
    smLastLetter:
    begin
      if EditorHasNoCaret(Ed) then
        Ed.ExecCommand(smEditorBottom{smScrollPageRight})
      else
        Handled:= False;
    end;

    //Shift+
    smSelLeft..smSelDown:
      Handled:= EditorHasNoCaret(Ed);

    //search
    smFindDialog:
      ecFind.Execute;
    smReplaceDialog:
      ecReplace.Execute;
    sm_ReplaceInFiles:
      ecReplaceInFiles.Execute;
    sm_ReplaceInProject:
      ecReplaceInProject.Execute;  

    smFindNext:
    begin
      if IsTreeviewFocused then
        ecFindInTreeNext.Execute
      else
      if IsListboxFocused then
        ecFindInListNext.Execute
      else
      begin
        DoFinderInit(true{KeepFlags});

        if Finder.FindText<>'' then
        begin
          Finder.FindNext;
          if Finder.Matches>0 then
            EditorCheckCaretOverlappedByForm(Finder.Control, fmSR);
        end
        else
          ecFind.Execute;

        DoClearFindDialogStatus;
      end;
    end;

    smFindPrev:
    begin
      if IsTreeviewFocused then
        ecFindInTreePrev.Execute
      else
      if IsListboxFocused then
        ecFindInListPrev.Execute
      else
      begin
        DoFinderInit(true{KeepFlags});

        if Finder.FindText<>'' then
        begin
          Finder.FindPrev;
          if Finder.Matches>0 then
            EditorCheckCaretOverlappedByForm(Finder.Control, fmSR);
        end
        else
          ecFind.Execute;

        DoClearFindDialogStatus;
      end;
    end;

    smFindAll:
    begin
      DoFinderInit(true{KeepFlags});
      if opFindSuggestSel and (Ed.SelLength>0) then
        Finder.FindText:= Ed.SelText
      else
      if opFindSuggestWord then
        Finder.FindText:= Ed.WordAtPos(Ed.CaretPos);
      if Finder.FindText='' then
        ecFind.Execute
      else
      begin
        Finder.Flags:= Finder.Flags+[ftEntireScope];
        Finder.FindAll;
        MsgFound;
        UpdateFrameMicroMap(CurrentFrame);
      end;
      DoClearFindDialogStatus;
    end;

    smReplaceAll:
    begin
      DoFinderInit(true{KeepFlags});
      if Finder.FindText='' then
        ecReplace.Execute
      else
      begin
        Finder.Flags:= Finder.Flags+[ftEntireScope];
        Finder.ReplaceAll;
        MsgFound;
      end;
      DoClearFindDialogStatus;
    end;

    smReplaceNext:
    begin
      DoFinderInit(true{KeepFlags});
      if Finder.FindText='' then
        ecReplace.Execute
      else
      begin
        Finder.Flags:= Finder.Flags-[ftEntireScope];
        DoFixReplaceCaret(Ed);
        if Ed.SelLength>0 then
          Finder.ReplaceAgain;
        Finder.FindAgain;
      end;
      DoClearFindDialogStatus;
    end;

    //find curr word
    smFindCurrentWordNext:
    begin
      DoFinderInit;
      DoFind_CurrentWord(true);
    end;
    smFindCurrentWordPrior:
    begin
      DoFinderInit;
      DoFind_CurrentWord(false);
    end;

    //Lister find
    sm_ListerFindDialog:
      PostMessage(hLister, WM_KEYDOWN, VK_F7, 0);
    sm_ListerFindNext:
      PostMessage(hLister, WM_KEYDOWN, VK_F3, 0);
    sm_ListerFindPrev:
    begin
      PostMessage(hLister, WM_KEYDOWN, VK_SHIFT, 0);
      PostMessage(hLister, WM_KEYDOWN, VK_F3, 0);
    end;

    //clip
    sm_CopyAsHTML:  EditorCopyAsHtml(Ed);
    sm_CopyAsRTF:   EditorCopyAsRtf(Ed);
    sm_CopyAppend:  EditorCopyOrCutAndAppend(Ed, false);
    sm_CutAppend:   EditorCopyOrCutAndAppend(Ed, true);
    sm_CopyLine:    EditorCopyOrCutCurrentLine(Ed, false);
    sm_CutLine:     EditorCopyOrCutCurrentLine(Ed, true);

    //Del key
    smDeleteChar:
    begin
      DoCheckIfBookmarkSetHere(Ed, Ed.CaretStrPos);
      Handled:= false;
    end;
    //BackSpace key
    smDeleteLastChar:
    begin
      DoCheckIfBookmarkSetHere(Ed, Ed.CaretStrPos-1);
      Handled:= false;
    end;

    //Delete lines
    smDeleteLine:
      ecRemoveLines.Execute;

    //Ctrl+End: if pressed on last line, scroll editor up
    smEditorBottom:
    begin
      if (soScrollLastLine in Ed.Options) and
        (Ed.CaretPos.Y = Ed.Lines.Count-1) then
        Ed.TopLine:= Ed.Lines.Count-1
      else
        Handled:= false;
    end;

    //sorting
    smSortAscending:
      ecSortAscending.Execute;
    smSortDescending:
      ecSortDescending.Execute;

    //tools
    sm_OpenBrowserFirefox: DoOpenInBrowser('firefox.exe');
    sm_OpenBrowserIE: DoOpenInBrowser('iexplore.exe');
    sm_OpenBrowserChrome: DoOpenInBrowser('chrome.exe');
    sm_OpenBrowserSafari: DoOpenInBrowser('safari.exe');
    sm_OpenCurrentFile: DoOpenCurrentFile;
    sm_OpenCurrentFolder: DoOpenCurrentDir;
    sm_OpenCmdPrompt: DoOpenCmdPrompt;

    sm_OnlineSearchHTML4: DoOnlineSearch_Name('HTML4');
    sm_OnlineSearchHTML5: DoOnlineSearch_Name('HTML5');
    sm_OnlineSearchGoogle: DoOnlineSearch_Name('Google');
    sm_OnlineSearchPhp: DoOnlineSearch_Name('PHP.net');
    sm_OnlineSearchWikipedia: DoOnlineSearch_Name('Wikipedia (en)');
    sm_OnlineSearchMsdn: DoOnlineSearch_Name('MSDN');

    sm_NumericConverterDialog: ecNumericConverter.Execute;
    sm_SortDialog: ecSortDialog.Execute;
    sm_ToggleLineCommentAlt: ecToggleLineCommentAlt.Execute;

    sm_GotoSelectionStartEnd: EditorJumpSelectionStartEnd(Ed);
    sm_GotoBookmarkDialog: ecGotoBk.Execute;
    sm_ReplaceFromClipAll: ecReplaceSelFromClipAll.Execute;
    sm_RereadOutputPanel: acRereadOut.Execute;
    sm_DropPortableBk: ecDropPortableBk.Execute;
    sm_GotoPortableBk: ecGotoPortableBk.Execute;
    sm_IndentLike1st: ecIndentLike1st.Execute;
    sm_PasteNoCursorChange: EditorPasteNoCaretChange(Ed);
    sm_PasteToColumn1: EditorPasteToFirstColumn(Ed);
    sm_PasteAsColumnBlock: begin if not EditorPasteAsColumnBlock(Ed) then MsgBeep; end;
    sm_CancelSelection: Ed.ResetSelection;
    sm_CenterLines: ecCenterLines.Execute;
    sm_ExtendSelByLine: EditorExtendSelectionByOneLine(Ed);
    sm_SelectBrackets: EditorSelectBrackets(Ed, FPrevCaretPos);
    sm_CollapseParent: ecCollapseParent.Execute;
    sm_CollapseWithNested: ecCollapseWithNested.Execute;
    sm_AlignWithSeparator: ecAlignWithSep.Execute;
    sm_ToggleShowGroup2: ecToggleShowGroup2.Execute;
    sm_SelectionExtend: DoExtendSelection(Ed);
    //sm_SelectionShrink: ecSelShrink.Execute;
    sm_ReverseLines: ecReverseLines.Execute;
    sm_ShuffleLines: ecShuffleLines.Execute;
    sm_DeleteToFileBegin: EditorDeleteToFileBegin(Ed);
    sm_DeleteToFileEnd: EditorDeleteToFileEnd(Ed);

    //blank operations
    sm_RemoveBlanks: ecRemoveBlanks.Execute;
    sm_ReduceBlanks: ecReduceBlanks.Execute;
    sm_TrimLeading: ecTrimLead.Execute;
    sm_TrimTrailing: ecTrimTrail.Execute;
    sm_TrimAll: ecTrimAll.Execute;
    sm_RemoveDupSpaces: ecRemoveDupSpaces.Execute;
    sm_ConvertTabsToSpaces: ecTabToSp.Execute;
    sm_ConvertSpacesToTabsAll: ecSpToTab.Execute;
    sm_ConvertSpacesToTabsLeading: ecSpToTabLeading.Execute;

    sm_GotoNextBlank: begin if not EditorJumpBlankLine(Ed, opFindOffsetTop, true) then MsgBeep; end;
    sm_GotoPrevBlank: begin if not EditorJumpBlankLine(Ed, opFindOffsetTop, false) then MsgBeep; end;
    sm_SelectParagraph: EditorSelectParagraph(Ed);
    sm_SelectToken:         begin if not EditorSelectToken(Ed, false) then MsgBeep; end;
    sm_SelectTokenNoQuotes: begin if not EditorSelectToken(Ed, true) then MsgBeep; end;
    sm_SelectToWordEnd: EditorSelectOrJumpToWordEnd(Ed, true);
    sm_JumpToWordEnd: EditorSelectOrJumpToWordEnd(Ed, false);
    sm_FindNextAndExtendSel: ecFindNextWithExtend.Execute;
    sm_FindPrevAndExtendSel: ecFindPrevWithExtend.Execute;
    sm_FindClipboardNext: ecFindClipNext.Execute;
    sm_FindClipboardPrev: ecFindClipPrev.Execute;
    sm_RepeatLastCommand: ecRepeatCmd.Execute;
    sm_FindCommand: DoFind_CommandFromString(WideString(PWideChar(Data)));
    sm_CommandsList: ecCommandsList.Execute;
    sm_ScrollToSel: EditorScrollToSelection(Ed, opFindOffsetTop);
    sm_ProjectList: ecProjectList.Execute;

    sm_RemoveDupsAll: ecDedupAll.Execute;
    sm_RemoveDupsAllAndOrig: ecDedupAllAndOrig.Execute;
    sm_RemoveDupsAdjacent: ecDedupAdjacent.Execute;
    sm_ExtractDupsCase: ecExtractDupsCase.Execute;
    sm_ExtractDupsNoCase: ecExtractDupsNoCase.Execute;
    sm_ExtractUniqueLines: ecExtractUniq.Execute;

    //macros 1-9
    sm_MacroRepeat: acMacroRepeat.Execute;
    sm_Macro1: acMacro1.Execute;
    sm_Macro2: acMacro2.Execute;
    sm_Macro3: acMacro3.Execute;
    sm_Macro4: acMacro4.Execute;
    sm_Macro5: acMacro5.Execute;
    sm_Macro6: acMacro6.Execute;
    sm_Macro7: acMacro7.Execute;
    sm_Macro8: acMacro8.Execute;
    sm_Macro9: acMacro9.Execute;
    sm_Macro10: acMacro10.Execute;
    sm_Macro11: acMacro11.Execute;
    sm_Macro12: acMacro12.Execute;
    sm_Macro13: acMacro13.Execute;
    sm_Macro14: acMacro14.Execute;
    sm_Macro15: acMacro15.Execute;
    sm_Macro16: acMacro16.Execute;
    sm_Macro17: acMacro17.Execute;
    sm_Macro18: acMacro18.Execute;
    sm_Macro19: acMacro19.Execute;
    sm_Macro20: acMacro20.Execute;
    sm_Macro21: acMacro21.Execute;
    sm_Macro22: acMacro22.Execute;
    sm_Macro23: acMacro23.Execute;
    sm_Macro24: acMacro24.Execute;
    sm_Macro25: acMacro25.Execute;
    sm_Macro26: acMacro26.Execute;
    sm_Macro27: acMacro27.Execute;
    sm_Macro28: acMacro28.Execute;
    sm_Macro29: acMacro29.Execute;
    sm_Macro30: acMacro30.Execute;

    //tabs 0-9
    sm_Tab0: DoTabIndexClick(0);
    sm_Tab1: DoTabIndexClick(1);
    sm_Tab2: DoTabIndexClick(2);
    sm_Tab3: DoTabIndexClick(3);
    sm_Tab4: DoTabIndexClick(4);
    sm_Tab5: DoTabIndexClick(5);
    sm_Tab6: DoTabIndexClick(6);
    sm_Tab7: DoTabIndexClick(7);
    sm_Tab8: DoTabIndexClick(8);
    sm_Tab9: DoTabIndexClick(9);

    sm_TabRt0: DoRtTabIndexClick(0);
    sm_TabRt1: DoRtTabIndexClick(1);
    sm_TabRt2: DoRtTabIndexClick(2);
    sm_TabRt3: DoRtTabIndexClick(3);
    sm_TabRt4: DoRtTabIndexClick(4);
    sm_TabRt5: DoRtTabIndexClick(5);
    sm_TabRt6: DoRtTabIndexClick(6);
    sm_TabRt7: DoRtTabIndexClick(7);
    sm_TabRt8: DoRtTabIndexClick(8);
    sm_TabRt9: DoRtTabIndexClick(9);

    //split
    sm_Split2080: ecSplit20_80.Execute;
    sm_Split3070: ecSplit30_70.Execute;
    sm_Split4060: ecSplit40_60.Execute;
    sm_Split5050: ecSplit50_50.Execute;
    sm_Split6040: ecSplit60_40.Execute;
    sm_Split7030: ecSplit70_30.Execute;
    sm_Split8020: ecSplit80_20.Execute;
    sm_SplitLeft: ecSplitLeft.Execute;
    sm_SplitRight: ecSplitRight.Execute;

    //encoding
    sm_EncodingChange_Ansi: ApplyFrameEncodingAndReload(CurrentFrame, cp_ACP);
    sm_EncodingChange_Oem: ApplyFrameEncodingAndReload(CurrentFrame, cp_OEMCP);
    sm_EncodingChange_Utf8Bom: ApplyFrameEncodingAndReload(CurrentFrame, cp__UTF8);
    sm_EncodingChange_Utf8NoBom: ApplyFrameEncodingAndReload(CurrentFrame, cp__UTF8_noBOM);
    sm_EncodingChange_Utf16LE: ApplyFrameEncodingAndReload(CurrentFrame, cp__Unicode);
    sm_EncodingChange_Utf16BE: ApplyFrameEncodingAndReload(CurrentFrame, cp__UnicodeBE);

    sm_EncodingConvert_Ansi: ApplyFrameEncodingAndReload(CurrentFrame, cp_ACP, false);
    sm_EncodingConvert_Oem: ApplyFrameEncodingAndReload(CurrentFrame, cp_OEMCP, false);
    sm_EncodingConvert_Utf8Bom: ApplyFrameEncodingAndReload(CurrentFrame, cp__UTF8, false);
    sm_EncodingConvert_Utf8NoBom: ApplyFrameEncodingAndReload(CurrentFrame, cp__UTF8_noBOM, false);
    sm_EncodingConvert_Utf16LE: ApplyFrameEncodingAndReload(CurrentFrame, cp__Unicode, false);
    sm_EncodingConvert_Utf16BE: ApplyFrameEncodingAndReload(CurrentFrame, cp__UnicodeBE, false);

    sm_Menu_EncodingsChange:
      begin
        p:= Mouse.CursorPos;
        PopupStatusEnc.Popup(p.x, p.y);
      end;
    sm_Menu_EncodingsConvert:
      begin
        p:= Mouse.CursorPos;
        PopupStatusEncConvert.Popup(p.x, p.y);
      end;
    sm_Menu_Lexers:
      begin
        p:= Mouse.CursorPos;
        PopupLexers.Popup(p.x, p.y);
      end;
    sm_Menu_LineEnds:
      begin
        p:= Mouse.CursorPos;
        PopupStatusLineEnds.Popup(p.x, p.y);
      end;

    sm_QuickSearch:
      begin
        if not tbQs.Visible then
          TbxItemTQsClick(Self);
        tbQs.CurrentDock.Show;
        if Self.Enabled and edQs.CanFocus then
          edQs.SetFocus;
      end;

    sm_InsertDateTime:
      DoDateTime;
    sm_ExtractTextDialog:
      DoExtractText;
    sm_GotoNextFindResult:
      ecGotoNextFindResult.Execute;
    sm_GotoPrevFindResult:
      ecGotoPrevFindResult.Execute;

    sm_ClearFilesHistory:
      DoClearFilesHistory;
    sm_ToggleSyncEditing:
      DoToggleSyncEditing;
    sm_ShowFullScreen:
      ecFullScr.Execute;
    sm_ShowOnTop:
      ecOnTop.Execute;
    sm_SyncScrollHorz:
      ecSyncScrollH.Execute;
    sm_SyncScrollVert:
      ecSyncScrollV.Execute;
    sm_FillBlockDialog:
      DoFillBlock;
    sm_InsertTextDialog:
      DoInsertTextDialog;
    sm_SpellLive:
      ecSpellLive.Execute;
    sm_SpellCheck:
      ecSpellCheck.Execute;
    sm_JoinLines:
      EditorJoinLines(Ed);
    sm_SplitLines:
      EditorSplitLinesWithDialog(Ed);

    smSelCharacter:
      begin
        with ecCharPopup do
          if Visible then
            CloseUp(false)
          else
            Handled:= false;
      end;

    smCommentLines,
    smUncommentLines:
      EditorCommentUncommentLines(Ed, Command = smCommentLines);
    sm_ToggleLineComment:
      ecToggleLineComment.Execute;
    sm_ToggleStreamComment:
      ecToggleStreamComment.Execute;

    //file
    sm_FileNew: acNewTab.Execute;
    sm_FileNewWindow: acNewWindow.Execute;
    sm_FileOpen: acOpen.Execute;
    sm_FileReopen: acReread.Execute;
    sm_FileSave: if acSave.Enabled then acSave.Execute;
    sm_FileSaveAs: if acSaveAs.Enabled then acSaveAs.Execute;
    sm_FileSaveAll: acSaveAll.Execute;
    sm_Fav_AddFile: acFavAddFile.Execute;
    sm_Fav_AddProject: acFavAddProj.Execute;
    sm_Fav_Organize: acFavManage.Execute;

    //tab closing commands are special, they destroy current editor,
    //so need to perform them not in OnExecuteCommand
    sm_FileClose,
    sm_FileCloseAndDelete,
    sm_FileCloseAll,
    sm_FileCloseOthers,
    sm_FileCloseOthersAllGroups,
    sm_FileRenameDialog,
    sm_FileOpenSession,
    sm_FileAddSession,
    sm_FileCloseSession:
      DoDelayedCommandWithClose(Command);

    sm_FileMoveToOtherView:
      Groups.MoveCurrentTabToOpposite;

    sm_FileExit: acExit.Execute;
    sm_FileSaveSession: DoSaveSession;
    sm_FileSaveSessionAs: DoSaveSessionAs;

    sm_FileExportRtf: acExportRTF.Execute;
    sm_FileExportHtml: acExportHTML.Execute;

    sm_OptSetup: acSetup.Execute;
    sm_OptSetupLexer: acSetupLexerNew.Execute;
    sm_OptSetupLexerLib: acSetupLexerLib.Execute;
    sm_OptReadOnly: ecReadOnly.Execute;
    sm_OptShowLeftPanel: ecShowTree.Execute;
    sm_OptShowRightPanel: ecShowClip.Execute;
    sm_OptShowOutputPanel: ecShowOut.Execute;
    sm_OptWrap: ecWrap.Execute;
    sm_OptLineNums: ecLineNums.Execute;
    sm_OptFolding: ecFolding.Execute;
    sm_OptNonPrint: ecNonPrint.Execute;
    sm_OptRuler: ecRuler.Execute;
    sm_ToggleSmartHl: ecSmartHl.Execute;

    sm_ToggleFocusTree: ecToggleFocusTree.Execute;
    sm_ToggleFocusClip: ecToggleFocusClip.Execute;
    sm_ToggleFocusClips: ecToggleFocusClips.Execute;
    sm_ToggleFocusOutput: ecToggleFocusOutput.Execute;
    sm_ToggleFocusFindRes: ecToggleFocusFindRes.Execute;
    sm_ToggleFocusValidate: ecToggleFocusValidate.Execute;
    sm_ToggleFocusMap: ecToggleFocusMap.Execute;
    sm_ToggleFocusProj: ecToggleFocusProject.Execute;
    sm_ToggleFocusTabs: ecToggleFocusTabs.Execute;
    sm_ToggleSlaveView: ecToggleSlave.Execute;
    sm_ToggleFocusMasterSlave: ecToggleFocusMasterSlave.Execute;
    sm_ToggleFocusConsole: ecToggleFocusConsole.Execute;
    sm_ToggleFocusGroups: ecToggleFocusGroups.Execute;
    sm_ToggleFocusBookmarks: ecToggleFocusBookmarks.Execute;

    sm_SplitViewsVertHorz: ecSplitViewsVertHorz.Execute;
    sm_SplitSlaveVertHorz: ecSplitSlaveVertHorz.Execute;
    sm_FileBackup: acBackup.Execute;

    //copy path
    sm_CopyFilename: DoCopyFilenameToClipboard(CurrentFrame, cCmdCopyFileName);
    sm_CopyFullPath: DoCopyFilenameToClipboard(CurrentFrame, cCmdCopyFullName);
    sm_CopyDirPath:  DoCopyFilenameToClipboard(CurrentFrame, cCmdCopyFilePath);

    //move caret
    sm_CaretIncX:  EditorMoveCaretByNChars(Ed, +SynHiddenOption('MovX', 20), 0);
    sm_CaretDecX:  EditorMoveCaretByNChars(Ed, -SynHiddenOption('MovX', 20), 0);
    sm_CaretIncY:  EditorMoveCaretByNChars(Ed, 0, +SynHiddenOption('MovY', 10));
    sm_CaretDecY:  EditorMoveCaretByNChars(Ed, 0, -SynHiddenOption('MovY', 10));

    //misc
    smChangeRangeSide: begin if not EditorJumpRange(Ed) then MsgBeep; end;
    sm_CopySearchMarks: DoCopySearchMarks(Ed);

    //macros
    {
    //disable these! Or Stack-overflow.
    smMacroRecStart: ecMacroRecord1.Execute;
    smMacroRecStop:  ecMacroStop1.Execute;
    smMacroRecCancel: ecMacroCancel1.Execute;
    smMacroPlay:     ecMacroPlay1.Execute;
    }
    sm_MacrosDialog: acMacroDialog.Execute;

    sm_ZoomOriginal: DoZoomEditor(100);
    sm_ZoomIn:       DoZoomEditorInc(true);
    sm_ZoomOut:      DoZoomEditorInc(false);

    sm_OptNonPrintOff: ecNonPrintOff.Execute;
    sm_OptNonPrintSpaces: ecNonPrintSpaces.Execute;
    sm_OptNonPrintEol: ecNonPrintEol.Execute;
    sm_OptNonPrintBoth: ecNonPrintBoth.Execute;
    sm_OptNonPrintEolDetails: ecNonPrintEolDetails.Execute;

    sm_FoldLevel2: DoFoldLevel(2);
    sm_FoldLevel3: DoFoldLevel(3);
    sm_FoldLevel4: DoFoldLevel(4);
    sm_FoldLevel5: DoFoldLevel(5);
    sm_FoldLevel6: DoFoldLevel(6);
    sm_FoldLevel7: DoFoldLevel(7);
    sm_FoldLevel8: DoFoldLevel(8);
    sm_FoldLevel9: DoFoldLevel(9);

    sm_TabColorDefault: DoSetTabColorIndex_Current(0);
    sm_TabColorCustom: DoSetTabColorIndex_Current(-1);
    sm_TabColor1: DoSetTabColorIndex_Current(1);
    sm_TabColor2: DoSetTabColorIndex_Current(2);
    sm_TabColor3: DoSetTabColorIndex_Current(3);
    sm_TabColor4: DoSetTabColorIndex_Current(4);
    sm_TabColor5: DoSetTabColorIndex_Current(5);
    sm_TabColor6: DoSetTabColorIndex_Current(6);
    sm_TabColor7: DoSetTabColorIndex_Current(7);
    sm_TabColor8: DoSetTabColorIndex_Current(8);
    sm_TabColor9: DoSetTabColorIndex_Current(9);
    sm_TabColor10: DoSetTabColorIndex_Current(10);

    sm_HideMenuItemsDialog:       DoConfigHideItems;
    sm_RestoreStylesDialog:       DoConfigRestoreStyles;
    sm_ExternalToolsDialog:       DoConfigTools;
    sm_ExplorerIntegrationDialog: DoConfigShellOptions;

    sm_EditSynIni:                DoOpenFile(SynIni);
    sm_EditSynPluginsIni:         DoOpenFile(SynPluginsIni);
    sm_OpenBySelection:           acOpenBySelection.Execute;
    sm_CustomizeStylesDialog:     acSetupLexerStyles.Execute;
    sm_CustomizeSpellCheckDialog: DoSpellConfig(nil);

    //Options dialog tabs
    sm_OptionsTab_ProgramOpt: DoOptionsDialog(1);
    sm_OptionsTab_Keys: DoOptionsDialog(2);
    sm_OptionsTab_Colors: DoOptionsDialog(3);
    sm_OptionsTab_Fonts: DoOptionsDialog(4);
    sm_OptionsTab_Tabs: DoOptionsDialog(5);
    sm_OptionsTab_Search: DoOptionsDialog(6);
    sm_OptionsTab_TreeMap: DoOptionsDialog(7);
    sm_OptionsTab_EditorView: DoOptionsDialog(9);
    sm_OptionsTab_EditorSelect: DoOptionsDialog(10);
    sm_OptionsTab_EditorCarets: DoOptionsDialog(11);
    sm_OptionsTab_EditorUndo: DoOptionsDialog(12);
    sm_OptionsTab_EditorInsertFmt: DoOptionsDialog(13);
    sm_OptionsTab_EditorOverrides: DoOptionsDialog(14);
    sm_OptionsTab_AutoComplete: DoOptionsDialog(15);
    sm_OptionsTab_SpellChecker: DoOptionsDialog(16);
    sm_OptionsTab_NewOpen: DoOptionsDialog(18);
    sm_OptionsTab_AutoSave: DoOptionsDialog(19);
    sm_OptionsTab_History: DoOptionsDialog(20);
    sm_OptionsTab_FormatsReload: DoOptionsDialog(21);
    sm_OptionsTab_SessionsProject: DoOptionsDialog(22);
    sm_OptionsTab_Paths: DoOptionsDialog(23);

    //tree
    sm_TreeNextNode: ecTreeNext.Execute;
    sm_TreePrevNode: ecTreePrev.Execute;
    sm_TreeParent: ecTreeParent.Execute;
    sm_TreeNextBrother: ecTreeNextBrother.Execute;
    sm_TreePrevBrother: ecTreePrevBrother.Execute;

    sm_TreeCollapseAll: TBXItemTreeCollapseAll.Click;
    sm_TreeExpandAll: TBXItemTreeExpandAll.Click;

    sm_TreeLevel2: DoTreeLevel(2);
    sm_TreeLevel3: DoTreeLevel(3);
    sm_TreeLevel4: DoTreeLevel(4);
    sm_TreeLevel5: DoTreeLevel(5);
    sm_TreeLevel6: DoTreeLevel(6);
    sm_TreeLevel7: DoTreeLevel(7);
    sm_TreeLevel8: DoTreeLevel(8);
    sm_TreeLevel9: DoTreeLevel(9);

    //project
    sm_NewProject: DoNewProject;
    sm_OpenProject: DoOpenProject;
    sm_AddFileToProject: DoAddFileToProject;
    sm_AddFilesToProject: DoAddFilesToProject;
    sm_FavoriteProjects: DoFavoriteProjects;
    sm_SaveProject: DoSaveProject;
    sm_UpdateProject: DoUpdateProject;

    sm_PasteAndSelect: DoPasteAndSelect;
    sm_InsertBlankLineAbove: EditorInsertBlankLineAboveOrBelow(Ed, false);
    sm_InsertBlankLineBelow: EditorInsertBlankLineAboveOrBelow(Ed, true);
    sm_CopyCurrentURL: DoCopyURL;
    sm_OpenCurrentURL: DoOpenURL;
    sm_FindId: DoFindId;
    sm_AddRecentColorCode: DoAddCurrentColorCodeToRecents;
    sm_SaveFolding: DoSaveFolding;
    sm_LoadFolding: DoLoadFolding;
    sm_OpenLastClosedFile: DoOpenLastClosedFile;
    sm_AcpForceTextOn: opAcpForceText:= true;
    sm_AcpForceTextOff: opAcpForceText:= false;

    //select-mode commands
    smNormalSelect: Ed.SelectModeDefault:= msNormal;
    smColumnSelect: Ed.SelectModeDefault:= msColumn;
    smLineSelect: Ed.SelectModeDefault:= msLine;
    smMarkSelStart: EditorMarkSelStart(Ed);
    smMarkSelEnd: EditorMarkSelEnd(Ed);

    sm_ScrollCurrentLineToTop: EditorScrollCurrentLineTo(Ed, cScrollToTop);
    sm_ScrollCurrentLineToBottom: EditorScrollCurrentLineTo(Ed, cScrollToBottom);
    sm_ScrollCurrentLineToMiddle: EditorScrollCurrentLineTo(Ed, cScrollToMiddle);

    sm_NewSnippetDialog: DoSnippetNew;
    sm_SnippetsDialog: DoSnippetListDialog('');

    //sync bookmarks of master/slave editors
    smSetBookmark0..smSetBookmark9:
      begin
        Ed.ToggleBookmark(Command - smSetBookmark0);
        BrotherEditor(Ed).Bookmarks[Command - smSetBookmark0]:= Ed.Bookmarks[Command - smSetBookmark0];
        BrotherEditor(Ed).Invalidate;
        UpdateListBookmarks;
      end;

    //sync markers of master/slave editors
    smDropMarker:
      begin
        BrotherEditor(Ed).DropMarker(Ed.CaretPos);
        //consider snippets
        Ed.MarkersLen.Clear;
        Handled:= false;
      end;
    smCollectMarker:
      begin
        with BrotherEditor(Ed) do
        begin
          if Markers.Count>0 then
            Markers.Delete(Markers.Count-1);
          Invalidate;
        end;
        //consider snippets
        Ed.MarkersLen.Clear;
        Handled:= false;
      end;
    smSwapMarker:
      begin
        with BrotherEditor(Ed) do
        begin
          DropMarker(Ed.CaretPos);
          if Markers.Count>=2 then
            Markers.Delete(Markers.Count-2);
          Invalidate;
        end;
        //consider snippets
        Ed.MarkersLen.Clear;
        Handled:= false;
      end;

    smUndo:
      begin
        if Ed.IsTabstopMode then
        begin
          Ed.MarkersLen.Clear;
          Ed.Markers.Clear;
        end;
        Handled:= false;
      end;

    sm_MarkersClear:
      begin
        EditorClearMarkers(Ed);
        EditorClearMarkers(BrotherEditor(Ed));
      end;
    sm_JumpToLastMarker:
      EditorJumpToLastMarker(Ed);

    sm_HelpFileContents:
      FOpenURL(FHelpFilename, Handle);
    sm_ResetPythonPlugins:
      DoPyResetPlugins;

    sm_GotoNextTab:
      DoTabSwitch(true, false);
    sm_GotoPrevTab:
      DoTabSwitch(false, false);

    sm_GotoNextOutputResult:
      DoJumpToNextOutputResult(not ListVal.Visible, true);
    sm_GotoPrevOutputResult:
      DoJumpToNextOutputResult(not ListVal.Visible, false);

    sm_GotoNextSearchOrOutputResult:
      begin
        if TreeFind.Visible then
          DoJumpToNextSearchResult(true)
        else
          DoJumpToNextOutputResult(not ListVal.Visible, true);
      end;
    sm_GotoPrevSearchOrOutputResult:
      begin
        if TreeFind.Visible then
          DoJumpToNextSearchResult(false)
        else
          DoJumpToNextOutputResult(not ListVal.Visible, false);
      end;

    sm_ToggleShowFoldersOnTabs:
      DoToggleTabDirs;
    sm_InsertUnicodeHexDialog:
      DoInsertUnicodeHexDialog;

    sm_MoveCurrentTabToNextGroup:
      Groups.MoveCurrentTabToNext(true);
    sm_MoveCurrentTabToPrevGroup:
      Groups.MoveCurrentTabToNext(false);

    smDeleteLastWord:
      begin
        //override to make it work at line-start: move to end of previous line
        P:= Ed.CaretPos;
        if P.X=0 then
        begin
          if P.Y>0 then
            Ed.CaretPos:= Point(Ed.Lines.LineLength(P.Y-1), P.Y-1);
        end
        else
          Handled:= false;
      end;

    sm_UpdateIniFile:
      DoUpdateIniFileForNewRelease(SynIni);

    sm_PrintNowAll,
    sm_PrintNowSelection:
      EditorPrint(Ed,
        Command=sm_PrintNowSelection,
        ExtractFileName(FrameOfEditor(Ed).FileName),
        ecSyntPrinter);

    sm_GotoNextModifiedLine:
      if not EditorGotoModifiedLine(Ed, true, false) then MsgBeep;
    sm_GotoPrevModifiedLine:
      if not EditorGotoModifiedLine(Ed, false, false) then MsgBeep;
    sm_GotoNextModifiedOrSavedLine:
      if not EditorGotoModifiedLine(Ed, true, true) then MsgBeep;
    sm_GotoPrevModifiedOrSavedLine:
      if not EditorGotoModifiedLine(Ed, false, true) then MsgBeep;

    sm_OpenEntireFolder:  DoOpenFolderDialog;
    sm_RestartProgram:    acRestart.Execute;

    sm_AddonsManager_Install: DoPluginsManager_Install;
    sm_AddonsManager_Remove: DoPluginsManager_Remove;
    sm_AddonsManager_Edit: DoPluginsManager_Edit;
    sm_AddonsManager_Update: DoPluginsManager_Update;
    sm_AddonsManager_SaveAll: DoPluginsManager_SaveAll;
    sm_AddonsManager_Config: DoPluginsManager_Config;

    //end of commands list

    cPyCommandBase..
    cPyCommandBase+500:
      DoPyCommandPlugin(Command-cPyCommandBase);

    else
      Handled:= false;
  end;

  //workaround for non-recorded commands
  //(EC issue)
  if Handled or IsCommandAllowedInMacro(Command) then
    DoMacro_RecordCommand(Command, Data);
end;

function TfmMain.IsCommandAllowedInMacro(Cmd: Integer): boolean;
begin
  if Cmd=TemplatePopup.CommandID then
    Result:= true
  else
  if (Cmd>=sm_CaretsRemoveLeaveFirst) and
     (Cmd<=sm_CaretsRemoveLeaveFirst+50) then
    Result:= true
  else
    Result:= false;
end;

procedure TfmMain.UpdateLexList;
var
  ACurLexer: string;
  //
  function DoMakeItem(const SName: string; NTag: Integer): TSpTbxItem;
  begin
    Result:= TSpTbxItem.Create(Self);
    if SName<>'' then
      Result.Caption:= SName
    else
      Result.Caption:= DKLangConstW('None');
    Result.Tag:= NTag;
    Result.OnClick:= LexListClick;
    Result.RadioItem:= true;
    Result.Checked:= SName = ACurLexer;
  end;
  //
var
  menu: TSpTbxSubmenuItem;
  s: TStringList;
  i, nTag: integer;
  ch: char;
begin
  PopupLexers.Items.Clear;
  if SyntaxManager.AnalyzerCount=0 then Exit;

  ACurLexer:= CurrentLexerForFile;
  PopupLexers.Items.Add(DoMakeItem('', -1));

  s:= TStringList.Create;
  try
    s.Sorted:= true;
    s.Duplicates:= dupIgnore;
    for i:= 0 to SyntaxManager.AnalyzerCount-1 do
      if not SyntaxManager.Analyzers[i].Internal then
        s.AddObject(SyntaxManager.Analyzers[i].LexerName, TObject(i));

    if opLexerGroups then
    begin
      for ch:= 'A' to 'Z' do
      begin
        menu:= TSpTbxSubmenuItem.Create(Self);
        menu.Caption:= ch;
        PopupLexers.Items.Add(menu);

        for i:= 0 to s.Count-1 do
          if SBegin(UpperCase(s[i]), UpCase(ch)) then
          begin
            nTag:= integer(s.Objects[i]);
            if SyntaxManager.Analyzers[nTag].Internal then Continue;
            menu.Add(DoMakeItem(s[i], nTag));
          end;
        if menu.Count=0 then
          FreeAndNil(menu);
      end;
    end
    else //not opLexerGroups
      for i:= 0 to s.Count-1 do
      begin
        nTag:= integer(s.Objects[i]);
        if SyntaxManager.Analyzers[nTag].Internal then Continue;
        PopupLexers.Items.Add(DoMakeItem(s[i], nTag));
      end;
  finally
    FreeAndNil(s);
  end;
end;

procedure TfmMain.LexListClick(Sender: TObject);
var
  n, n1, n2: integer;
begin
  n:= (Sender as TComponent).Tag;
  if n = -1 then
  begin
    CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer:= nil;
    UpdateLexerTo(nil);
  end
  else
  begin
    n1:= CurrentFrame.EditorMaster.TopLine;
    n2:= CurrentFrame.EditorSlave.TopLine;

    CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer:= SyntaxManager.Analyzers[n];
    UpdateLexerTo(CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer);

    CurrentFrame.EditorMaster.TopLine:= n1;
    CurrentFrame.EditorSlave.TopLine:= n2;
  end;
end;

procedure TfmMain.acSaveExecute(Sender: TObject);
begin
  if CurrentFrame <> nil then
    SaveFrame(CurrentFrame, False);
end;

procedure TfmMain.acSaveAsExecute(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F<>nil then
  begin
    if F.IsFtp then
    begin
      if not MsgConfirmFtp then
        Exit
      else
        F.FreeFtpInfo;
    end;    

    //add to MRU old name
    if F.FileName<>'' then
      SynMruFiles.AddItem(F.FileName);

    //save-as
    SaveFrame(F, True);
    UpdateListBookmarks;
  end;
end;

procedure TfmMain.MRUClick(Sender: TObject; const S: WideString);
begin
  if IsFileExist(S) then
    DoOpenFile(S)
  else
  begin
    MsgNoFile(S);
    SynMruFiles.DeleteItem(S);
  end;
end;

procedure TfmMain.MRU_SessClick(Sender: TObject; const S: WideString);
begin
  if IsFileExist(S) then
  begin
    if not DoConfirmSaveSession(true) then
      Exit;
    DoOpenSession(S);
  end
  else
  begin
    MsgNoFile(S);
    SynMruSessions.DeleteItem(S);
  end;
end;

procedure TfmMain.PopupLexersPopup(Sender: TObject);
begin
  UpdateLexList;
end;

procedure TfmMain.acSetupExecute(Sender: TObject);
begin
  DoOptionsDialog(-1);
end;

procedure TfmMain.DoOptionsDialog(tabId: Integer);
var
  i: Integer;
  L: TTntStringList;
begin
  UpdateMacroKeynames;
  opTabOptionsIndex:= tabId;

  with TfmSetup.Create(Self) do
    try
      fmMain:= Self;
      //center on screen
      Left:= Self.Monitor.Left + (Self.Monitor.Width - Width) div 2;
      Top:= Self.Monitor.Top + (Self.Monitor.Height - Height) div 2;

      //disable some items
      {$ifndef SPELL}
      boxSpellOpt.Visible:= false;
      boxSpellLnk.Visible:= false;
      {$endif}
      cbSavePos.Enabled:= SynExe;
      cbASaveFocus.Enabled:= SynExe;
      cbSessSave.Enabled:= SynExe;
      cbSessLoad.Enabled:= SynExe;
      cbInst.Enabled:= SynExe;
      cbFullTitle.Enabled:= SynExe;

      cbTheme.Items.Clear;
      for i:= Low(cThemes) to High(cThemes) do
        cbTheme.Items.Add(cThemes[i]);

      L:= TTntStringList.Create;
      try
        FFindToList(L, SynSkinsDir, '*.skn', '',
          false{SubDirs}, false, false, false);
        for i:= 0 to L.Count-1 do
          cbTheme.Items.Add('*'+ ChangeFileExt(ExtractFileName(L[i]), ''));
      finally
        FreeAndNil(L);
      end;

      ShowModal;
    finally
      Release;
    end;

  if (not SynExe) and (hLister <> 0) then
    BringWindowToTop(hLister);
  if (not QuickView) then
    FocusEditor;

  //apply lang here
  LangManager.LanguageID:= opLang;
  UpdateLang;

  UpdateShortcuts;
  SyntaxManagerChange(Self);
end;

function TfmMain.DoCloseTabs(Id: TATTabCloseId; AForPopupMenu: boolean): boolean;
begin
  Result:= Groups.CloseTabs(Id, AForPopupMenu);
  UpdateListTabs;
  UpdateListBookmarks;
end;


procedure TfmMain.TimerTickTimer(Sender: TObject);
begin
  //tree update
  if GetCurrentThreadId = MainThreadID then
    CheckSynchronize;

  if StatusItemTabsize.ImageIndex>=0 then
    UpdateStatusbarTabsize;

  if FUpdatePluginsLang then
  begin
    FUpdatePluginsLang:= false;
    DoPlugin_RefreshLang;
  end;

  //repaints
  if FNeedRepaint then
  begin
    FNeedRepaint:= false;

    {$ifndef FixRepaint}
    FixDraw(plOut, true);
    FixDraw(plTree, false);
    FixDraw(plClip, false);
    {$endif}

    //Repaint TBs
    tbMenu.Invalidate;
    tbView.Invalidate;
    tbQS.Invalidate;
    edQs.Invalidate;
    Status.Invalidate;

    //Repaint editor
    if CurrentEditor<>nil then
      FixDraw(CurrentEditor, true);
  end;

  if CurrentFrame<>nil then
    CurrentFrame.DoChangeTick;
end;

procedure TfmMain.acExportRTFBeforeExecute(Sender: TObject);
begin
  if CurrentEditor.HaveSelection then
    acExportRTF.ExportType:= etSelection
  else
    acExportRTF.ExportType:= etAllText;

  UpadateFilenameForExport;
end;

procedure TfmMain.ecReadOnlyExecute(Sender: TObject);
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if DoPyEvent(Ed, cSynEventOnState, [cSynPropRO]) = cPyFalse then Exit;
  Ed.ReadOnly:= not Ed.ReadOnly;

  UpdateStatusbar;
  UpdateEditorCaret(Ed);
  UpdateTitle(CurrentFrame);
end;

//fix for missing EC's cursor hiding
procedure TfmMain.UpdateEditorCaret(Ed: TSyntaxMemo);
begin
  if QuickView then Exit;
  if not Ed.ReadOnly then Exit;
  with TEdit.Create(Ed) do
  try
    Parent:= Ed;
    Left:= 0;
    Top:= 0;
    if Self.Enabled and CanFocus then
      SetFocus;
  finally
    Free;
  end;
  FocusEditor;
end;

procedure TfmMain.UpdateGutter(F: TEditorFrame; AUpdateCur: boolean = true);
var
  NBandNums: integer;
begin
  NBandNums:= F.EditorMaster.LineNumbers.Band;

  if F.EditorMaster.DisableFolding then
  begin
    F.EditorMaster.Gutter.Bands[cBandFolding].Width:= 0;
    F.EditorSlave.Gutter.Bands[cBandFolding].Width:= 0;
  end
  else
  begin
    F.EditorMaster.Gutter.Bands[cBandFolding].Width:= cGutterBandSizeFold;
    F.EditorSlave.Gutter.Bands[cBandFolding].Width:= cGutterBandSizeFold;
  end;

  if F.EditorMaster.LineNumbers.Visible then
  begin
    //none
  end
  else
  begin
    F.EditorMaster.Gutter.Bands[NBandNums].Width:= 0;
    F.EditorSlave.Gutter.Bands[NBandNums].Width:= 0;
  end;

  if AUpdateCur then
    UpdateEditorCaret(F.EditorMaster);
end;

procedure TfmMain.ButtonOnSelect(Sender: TTBCustomItem; Viewer: TTBItemViewer; Selecting: Boolean);
begin
  if Sender is TSpTbxItem then
    DoHint((Sender as TSpTbxitem).Hint)
  else
    DoHint(Sender.Hint);
end;

procedure TfmMain.SynScroll(Sender: TObject);
var
  Ed: TSyntaxMemo;
  N: integer;
begin
  Ed:= Sender as TSyntaxMemo;

  //Send info to Lister
  if not SynExe then
    with Ed do
    begin
      if Lines.Count = 0 then
        N:= 0
      else
        N:= (TopLine * 100) div Lines.Count;
      PostMessage(hLister, WM_COMMAND, MAKELONG(N, cLister_itm_percent), Handle);
    end;

  //sync scroll views
  if not FSyncBusy then
  try
    FSyncBusy:= true;
    DoSyncScroll(Ed);
  finally
    FSyncBusy:= false;
  end;

  //update map
  if Ed.Lines.Count<=cMaxLinesInstantMinimap then
    SyncMapPos
  else
  begin
    TimerMinimap.Enabled:= false;
    TimerMinimap.Enabled:= true;
  end;
end;

procedure TfmMain.plTreeResize(Sender: TObject);
begin
  {$ifndef FixRepaint}
  plTree.Invalidate;
  tbTabsLeft.Invalidate;
  {$endif}

  DoPlugins_Resize;

  tbViewMove(Self);
end;

procedure TfmMain.Finder_OnBeforeExecute(Sender: TObject);
begin
  Finder.Control:= CurrentEditor;
  DoHint('');
end;

procedure TfmMain.Finder_OnNotFound(Sender: TObject);
begin
  DoProgressHide;
  DoHint(WideFormat(DKLangConstW('MNFound2'), [Finder.FindText]));
  MsgBeep;
end;

function TfmMain.SynBorderStyle: TBorderStyle;
begin
  {if opShowBorders then
    Result:= bsSingle
  else}
    Result:= bsNone;
end;

function TfmMain.SynBorderStyleEditor: TBorderStyle;
begin
  Result:= bsNone;
end;

procedure TfmMain.ApplyBorders;
begin
  Tree.BorderStyle:= SynBorderStyle;
  ListTabs.BorderStyle:= SynBorderStyle;
  ListOut.BorderStyle:= SynBorderStyle;
  ListVal.BorderStyle:= SynBorderStyle;
  ListPLog.BorderStyle:= SynBorderStyle;
  TreeFind.BorderStyle:= SynBorderStyle;
  MemoConsole.BorderStyle:= SynBorderStyle;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  FixSplitters;
  ApplyDefaultFonts;

  //scale sizes for 150% DPI
  if PixelsPerInch<>96 then
  begin
    Font.Size:= ScaleFontSize(Font.Size, Self);
    with Status do
      Height:= ScaleFontSize(Height, Self);
  end;

  {$ifndef SPELL}
  TbxItemTbSpellLive.Enabled:= false;
  TBXItemVSpellLive.Enabled:= false;
  TBXItemVSpellCheck.Enabled:= false;
  {$endif}

  if SynExe then
    if ParamStr(1)=cSynParamReg then
    begin
      LoadLexLib;
      TBXItemOShellClick(Self);
      Application.Terminate;
    end;

  if SynExe then
  begin
    SetForegroundWindow(Application.Handle); //For focusing taskbar button
    Application.BringToFront;               //For bringing to front
  end;

  TbxItemORo.Visible:= not SynExe;
  TbxItemOShell.Enabled:= SynExe;
  TBXItemOEditSynPluginsIni.Enabled:= SynExe;
  TbxItemCtxCustomize.Visible:= QuickView;
  TbxItemWinExplorer.Enabled:= SynExe;
  TbxItemWinFtp.Enabled:= SynExe;
  TBXItemWinConsole.Enabled:= SynExe;

  //init main
  LoadIni;
  PropsManager.UpdateAll;
  LoadLexLib;
  LoadMacros;
  LoadClip;
  LoadHideIni;
  LoadConsoleHist;
  LoadProjPreview;

  //init proj tree
  ApplyProj;

  //init spell-checker
  InitSpell;
  UpdateSpellLang;

  //Py fields
  PyExeDir:= ExcludeTrailingPathDelimiter(SynDir);
  PyIniDir:= ExcludeTrailingPathDelimiter(SynIniDir);

  //init objects
  InitPanelsTabs;
  InitGroups;
  DoPlugins_InitTabs; //after InitPanelsTabs
  
  TabsLeft.TabIndex:= FTabLeft;
  TabsRight.TabIndex:= FTabRight;
  TabsOut.TabIndex:= FTabOut;
end;

procedure TfmMain.LoadLexLib;
var
  fn_std, fn: string;
begin
  fn_std:= SynLexLib;

  with TIniFile.Create(SynIni) do
  try
    fn:= ReadString('Setup', 'LexLib', '');
  finally
    Free
  end;

  if fn='' then
    fn:= fn_std
  else
  begin
    if (ExtractFileDir(fn)='') then
      fn:= ExtractFilePath(fn_std) + fn
    else
      fn:= fn_std;
  end;

  if IsFileExist(fn) then
    SyntaxManager.LoadFromFile(fn);
end;

procedure TfmMain.DoFind_CurrentWord(ANext: boolean);
var
  NStart, NEnd, NMaxLen: integer;
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Ed=nil then Exit;
  with Ed do
  begin
    if SelLength>0 then
    begin
      //search for selection
      NMaxLen:= SynHiddenOption('MaxWordLen', 100);
      Finder.FindText:= EditorGetSelTextLimited(Ed, NMaxLen);
      NStart:= SelStart;
      NEnd:= NStart+SelLength;
    end
    else
    begin
      //search for curr word
      WordRangeAtPos(CaretPos, NStart, NEnd);
      if NEnd > NStart then
        Finder.FindText:= WordAtPos(CaretPos)
      else
        Exit;
    end;

    //set needed flags
    Finder.Flags:= Finder.Flags-[ftRegex];
    Finder.Flags:= Finder.Flags+[ftWrapSearch];

    //make sure FindNext won't try to find regex
    if Assigned(fmSR) then
      fmSR.OpRe:= false;

    //search
    if ANext then
    begin
      CaretStrPos:= NEnd;
      SetSelection(NStart, NEnd-NStart, true);
      Finder.FindNext;
    end
    else
    begin
      CaretStrPos:= NStart;
      SetSelection(NStart, NEnd-NStart, true);
      Finder.FindPrev;
    end;
  end;
end;

procedure TfmMain.plTreeVisibleChanged(Sender: TObject);
begin
  FixSplitters;
  ecShowTree.Checked:= plTree.Visible;

  if plTree.Visible then //ZD
    RedrawWindow(plTree.Handle, nil, 0,
      RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME); //ZD
end;

procedure TfmMain.ecShowTreeExecute(Sender: TObject);
begin
  with plTree do
    Visible:= not Visible;
  if not plTree.Visible then
    FocusEditor;

  {$ifndef FixRepaint}
  DoRepaintTBs;
  {$endif}
end;

const
  cDefColors: array[0..Pred(cTabColors)] of TColor =
    ($0000FF, $007FFF, $00FFFF, $00FF00, $007F00,
     $FFFFFF, $FFFF7F, $FF0000, $FF7F7F, $7F7F7F);

procedure TfmMain.FormCreate(Sender: TObject);
var
  i: integer;
  Cur: HIcon;
begin
  SynExe:= true;
  SynDir:= ExtractFilePath(GetModuleName(HInstance));
  SynDirForHelpFiles:= SynDir + 'Readme';
  InitSynIniDir;

  EditorSynLexersCfg:= SynLexersCfg;
  EditorSynLexersExCfg:= SynLexersExCfg;

  SynMruFiles:= TSynMruList.Create;
  SynMruSessions:= TSynMruList.Create;
  SynMruProjects:= TSynMruList.Create;
  SynMruNewdoc:= TSynMruList.Create;

  //make panels font non-bold
  plTree.Options.RightAlignSpacer.FontSettings.Style:= [];
  plClip.Options.RightAlignSpacer.FontSettings.Style:= [];
  plOut.Options.RightAlignSpacer.FontSettings.Style:= [];

  InitMenuItemsList;
  LangManager.ScanForLangFiles(SynDir + 'Lang', '*.lng', False);
  OnBackupLexerStyles:= DoBackupLexerStyles;
  FileNameConverterImageToBmp:= SynDir + 'Tools\ImageToBmp.exe';

  OD_Swatch.DefaultExt:= cSynColorSwatchExt;
  OD_Swatch.Filter:= Format('*.%s|*.%s', [cSynColorSwatchExt, cSynColorSwatchExt]);
  SD_Swatch.DefaultExt:= cSynColorSwatchExt;
  SD_Swatch.Filter:= OD_Swatch.Filter;
  SD_Snippets.DefaultExt:= cSynSnippetExt;
  SD_Snippets.Filter:= Format('*.%s|*.%s', [cSynSnippetExt, cSynSnippetExt]);
  OD_Session.DefaultExt:= cSynSessionExt;
  OD_Session.Filter:= 'Sessions|*.'+cSynSessionExt+';*.syn';
  SD_Session.DefaultExt:= cSynSessionExt;
  SD_Session.Filter:= 'Sessions|*.'+cSynSessionExt;

  ListOut.Align:= alClient;
  ListVal.Align:= alClient;
  TreeFind.Align:= alClient;
  Tree.Align:= alClient;
  ListPLog.Align:= alClient;
  ListTabs.Align:= alClient;
  ListBookmarks.Align:= alClient;
  plConsole.Align:= alClient;

  //init colors
  Move(cDefColors, opTabColors, SizeOf(opTabColors));

  //init plugins
  FillChar(FPluginsPanel, Sizeof(FPluginsPanel), 0);
  FillChar(FPluginsFindid, Sizeof(FPluginsFindid), 0);
  FillChar(FPluginsCommand, Sizeof(FPluginsCommand), 0);
  FillChar(FPluginsAcp, Sizeof(FPluginsAcp), 0);

  FInitialDir:= 'C:\'; //used on file closing
  FLastUntitled:= 0;
  FLastCmdId:= 0;
  FLastCmdData:= '';
  FLastCmdCount:= 0;
  FLastCmdPlaying:= false;
  FLastCmdBreak:= false;
  FLastMacro:= -1;

  FPanelDrawBusy:= false;
  FSyncBusy:= false;
  FListNewDocs:= TTntStringList.Create;
  FListFiles:= TTntStringList.Create;
  FListLexersSorted:= TTntStringList.Create;
  FListSnippets:= nil;
  FFontTabs:= TFont.Create;
  FFontTabs.Assign(ToolbarFont);
  FFontMenus:= TFont.Create;
  FFontMenus.Assign(ToolbarFont);

  fmNumConv:= nil;
  fmClip:= nil;
  fmClips:= nil;
  fmMap:= nil;
  fmProj:= nil;
  fmProgress:= nil;
  fmSR:= nil;
  FProjPreview:= nil;
  FProjPreviewEditor:= nil;
  FProjPreviewFilename:= '';

  for i:= Low(TabSwitchers) to High(TabSwitchers) do
  begin
    TabSwitchers[i]:= TTabSwitcher.Create(i);
    TabSwitchers[i].OnGetTab:= GetTabName;
  end;

  opAcpForceText:= false;
  FFullscreen:= false;
  FOnTop:= false;
  FLockUpdate:= false;
  FToolbarMoved:= false;
  FNeedRepaint:= true;
  FEnableRepaint:= false;
  FUpdatePluginsLang:= false;

  //ACP
  FAcpList_Display:= TStringList.Create;
  FAcpList_Items:= TStringList.Create;
  FAcpList_Hints:= TStringList.Create;
  FAcpList_Desc:= TStringList.Create;
  FAcpIntHtml:= TStringList.Create;
  FAcpIntCss:= TStringList.Create;

  //finder
  FinderPro:= nil;
  FinderInTree:= nil;
  FinderInList:= nil;
  Finder:= TSynFinderReplacer.Create(Self);
  Finder.OnBeforeExecute:= Finder_OnBeforeExecute;
  Finder.OnNotFound:= Finder_OnNotFound;
  Finder.SkipInit:= False;
  Finder.OnProgress:= Finder_OnProgress;

  //clear actions hints (Sepa's code)
  with ActionList do
    for i:= 0 to ActionCount-1 do
       (Actions[i] as TCustomAction).ShortCut:= 0;

  //new cursor for editor's gutter
  Cur:= LoadCursor(HInstance, 'GCURSOR');
  if Cur>0 then
    Screen.Cursors[1]:= Cur;

  //others
  FUserToolbarCommands:= TTntStringList.Create;
  //FTempFilenames:= TTntStringList.Create;
end;

procedure TfmMain.ecPrinterSetupExecute(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TfmMain.SetFormat(Sender: TObject);
var
  fmt: TTextFormat;
begin
  fmt:= TTextFormat((Sender as TComponent).Tag);
  UpdateFrameLineEnds(CurrentFrame, fmt, true);
end;


procedure TfmMain.UpdateFrameLineEnds(F: TEditorFrame; AFormat: TTextFormat; AManual: boolean);
begin
  if F<>nil then
    if AFormat<>F.EditorMaster.TextSource.Lines.TextFormat then
    begin
      F.EditorMaster.TextSource.Lines.TextFormat:= AFormat;
      if AManual then
      begin
        F.Modified:= true;
        F.LineEndsChg:= true;
      end;
      UpdateStatusbarLineEnds;
    end;
end;

procedure TfmMain.UpdateStatusbarLineEnds;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F<>nil then
    case F.EditorMaster.TextSource.Lines.TextFormat of
      tfCR: StatusItemEnds.Caption:= 'Mac';
      tfNL: StatusItemEnds.Caption:= 'Unix';
      else StatusItemEnds.Caption:= 'Win';
    end;
end;

procedure TfmMain.UpdateStatusbarTabsize;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Ed<>nil then
  begin
    if Ed.IsTabstopMode then
    begin
      StatusItemTabsize.ImageIndex:= cImageIndexTabstopMode;
      StatusItemTabsize.Caption:= '';
    end
    else
    begin
      StatusItemTabsize.ImageIndex:= -1;
      StatusItemTabsize.Caption:=
        IntToStr(EditorTabSize(Ed)) +
        IfThen(Ed.TabMode=tmSpaces, '_');
    end;
  end;
end;

function TfmMain.GetAcpFN(const LexerName: string): string;
begin
  Result:= LexerName;

  //for 'PL/SQL'
  SReplaceAll(Result, '/', '_');
  //for 'PHP_'
  if (Result<>'') and (Result[Length(Result)]='_') then
    Delete(Result, Length(Result), 1);

  Result:= SynDataSubdir(cSynDataAutocomp) + '\' + Result + '.acp';
end;

function TfmMain.GetSpecialHiliteFN(const Id: string): string;
begin
  Result:= SynDataSubdir(cSynDataAutocomp) + '\' + Id + '.ini';
end;

function TfmMain.GetHtmlListFN: string;
begin
  Result:= GetSpecialHiliteFN('Html_List');
end;

function TfmMain.GetCssListFN: string;
begin
  Result:= GetSpecialHiliteFN('Css_List');
end;

function TfmMain.GetHtmlTabbingFN: string;
begin
  Result:= GetSpecialHiliteFN('Html_Tabbing');
end;

procedure TfmMain.LoadHtmlAndCssLists;
var
  fn: string;
begin
  if FAcpIntHtml.Count=0 then
  begin
    fn:= GetHtmlListFN;
    if IsFileExist(fn) then
      FAcpIntHtml.LoadFromFile(fn);
  end;

  if FAcpIntCss.Count=0 then
  begin
    fn:= GetCssListFN;
    if IsFileExist(fn) then
      FAcpIntCss.LoadFromFile(fn);
  end;
end;

procedure TfmMain.UpdateAcp(const Lexer: string);
var
  fn: Widestring;
begin
  FAcpLexer:= Lexer;
  FAcpAgain:= false;

  if not Assigned(FAcpList_Display) then Exit;
  FAcpList_Display.Clear;
  FAcpList_Items.Clear;
  FAcpList_Hints.Clear;
  FAcpList_Desc.Clear;

  if Lexer='' then Exit;

  //load default ACP file
  fn:= GetAcpFN(Lexer);
  if IsFileExist(fn) then
  begin
    opAcpChars:= '';
    LoadAcpFromFile(fn, Lexer);
  end;

  //load user ACP file (specified in project)
  if Assigned(fmProj) then
  begin
    fn:= fmProj.GetUserVarValue('AC['+Lexer+']');
    if fn<>'' then
    begin
      if ExtractFilePath(fn)='' then
        fn:= ExtractFilePath(fmProj.ProjectFN)+fn;
      if IsFileExist(fn) then
        LoadAcpFromFile(fn, Lexer);
    end;
  end;
end;


procedure TfmMain.LoadAcpFromFile(const fn, Lexer: string);
  //
  function AcpItem(const s1, s2, s3, s4: string): string;
  begin
    Result:= '\s1\' + s1 + '\t\\s2\' + s2 + '\t\\s0\' + s3 + '\s3\ '+ s4;
  end;
  //
var
  List: TStringList;
  s, SType, SId, SPar, SHint: string;
  i: Integer;
  IsPas, IsBracketSep: boolean;
begin
  IsPas:= IsLexerPas(Lexer);
  IsBracketSep:= true;

  List:= TStringList.Create;
  try
    List.LoadFromFile(fn);
    for i:= 0 to List.Count-1 do
    begin
      s:= List[i];
      if s='' then
        Continue;
      if s[1]='#' then
      begin
        SParseString_AcpControlLine(s, opAcpChars, IsBracketSep);
        Continue;
      end;
      SParseString_AcpStd(s, IsBracketSep, SType, SId, SPar, SHint);
      if SId<>'' then
      begin
        FAcpList_Items.Add(SId + IfThen(Pos('(', SPar)=1, '('));
        FAcpList_Display.Add(AcpItem(SType, SId, SPar, ''{SHint}));
        FAcpList_Desc.Add(SHint);

        if IsPas and (Pos('):', SPar)>0) then
        begin
          SDeleteFrom(SPar, '):');
          SPar:= SPar+')';
        end;
        FAcpList_Hints.Add(SPar);
      end;
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  TimerTick.Enabled:= false;

  FreeAndNil(SynMruFiles);
  FreeAndNil(SynMruSessions);
  FreeAndNil(SynMruProjects);
  FreeAndNil(SynMruNewdoc);

  //FreeAndNil(FTempFilenames);
  FreeAndNil(FUserToolbarCommands);

  for i:= Low(TabSwitchers) to High(TabSwitchers) do
    FreeAndNil(TabSwitchers[i]);

  FreeAndNil(FAcpList_Display);
  FreeAndNil(FAcpList_Items);
  FreeAndNil(FAcpList_Hints);
  FreeAndNil(FAcpList_Desc);
  FreeAndNil(FAcpIntHtml);
  FreeAndNil(FAcpIntCss);

  FreeAndNil(Finder);
  FreeAndNil(FListFiles);
  FreeAndNil(FListNewDocs);
  FreeAndNil(FListLexersSorted);
  FreeAndNil(FFontTabs);
  FreeAndNil(FFontMenus);

  if Assigned(FListSnippets) then
  begin
    ClearSnippets;
    FreeAndNil(FListSnippets);
  end;
end;

function SAcpItemToId(const S: Widestring): Widestring;
begin
  Result:= S;
  //strip last "(" from id
  if (Result<>'') and (Result[Length(Result)]='(') then
    SetLength(Result, Length(Result)-1);
end;

procedure TfmMain.ParamCompletionGetParams(Sender: TObject;
  const FuncName: WideString; Pos: Integer);
var
  SText, S: Widestring;
  i: integer;
begin
  ParamCompletion.Items.Clear;

  SText:= DoAcpFromPlugins(cActionGetFunctionHint);
  if SText<>'' then
  begin
    ParamCompletion.Items.SetText(PWChar(SText));
  end
  else
  begin
    SText:= LowerCase(FuncName);
    for i:= 0 to FAcpList_Items.Count-1 do
    begin
      S:= LowerCase(FAcpList_Items[i]);
      S:= SAcpItemToId(S);
      if SText=S then
        if FAcpList_Hints[i]<>'' then
          ParamCompletion.Items.Add(FAcpList_Hints[i]);
    end;
  end;
end;

procedure TfmMain.SynGetTokenHint(Sender: TObject; TokenIndex: Integer; var HintText: String);
var
  Ed: TSyntaxMemo;
  Frame: TEditorFrame;
  s, sFilename, sResult: ecString;
  NColor1, NColor2: TColor;
  i, n: integer;
begin
  if not opTipsToken then
    begin HintText:= ''; Exit end;

  Ed:= Sender as TSyntaxMemo;
  Frame:= FrameOfEditor(Ed);
  sFileName:= Frame.FileName;

  //no lexer active
  if Ed.SyntObj=nil then
    begin HintText:= ''; Exit end;

  s:= EditorTokenString(Ed, TokenIndex);

  if IsImageHint(s, sFileName, sResult) then
  begin
    HintText:= sResult;
    DoHint(sResult);
    Exit
  end;

  if IsShowColor(s, NColor1, NColor2) then
  begin
    HintText:= cColorPrefix + IntToStr(NColor1) + IfThen(NColor2<>clNone, ','+IntToStr(NColor2));
    Exit
  end;

  {
  //test mouse over editor
  if not PtInRect(Ed.BoundsRect, Ed.Parent.ScreenToClient(Mouse.CursorPos)) then
  begin
    HintText:= '';
    Exit;
  end;
  }

  try
    s:= EditorTokenFullString(Ed, TokenIndex, Pos('.', opAcpChars)>0);

    //show for strings their ACP hint/description
    n:= -1;
    for i:= 0 to FAcpList_Items.Count-1 do
      if SAcpItemToId(FAcpList_Items[i]) = s then
        begin n:= i; Break; end;

    if n>=0 then
      if FAcpList_Hints[n]<>'' then
        HintText:= Trim(
          '\s4\'+SAcpItemToId(FAcpList_Items[n])+' '+
          '\s5\'+FAcpList_Hints[n]+sLineBreak+
          '\s6\'+FAcpList_Desc[n]);
  except
    on E: Exception do
    begin
      HintText:= '';
      DoHint('Exception on reading hint: '+E.Message);
    end;
  end;
end;

procedure TfmMain.ecACPAfterComplete(Sender: TObject;
  const Item: WideString);
var
  Ed: TSyntaxMemo;
  QuoteChar: WideChar;
begin
  Ed:= CurrentEditor;

  //close tag
  if FAcpHtmClosing then
    Ed.InsertText('>')
  else
  //handle Ctrl+Enter pressing in ACP list
  if FAcpHtmTags and IsCtrlPressed then
    with Ed do
      if (CaretStrPos>0) and (Lines.Chars[CaretStrPos]=' ') then
      begin
        CaretStrPos:= CaretStrPos-1;
        DeleteText(1);
        InsertText('>');
        FAcpAgain:= false;
      end;

  //add quote char after parameter value
  if FAcpHtm and not FAcpHtmTags then
  begin
    if EditorCaretAfterUnclosedQuote(Ed, QuoteChar) then
      Ed.InsertText(QuoteChar);
  end;

  //if CSS completion done, delete ":" after caret
  //(it may left from old text, and we inserted ":" anyway)
  if FAcpCss then
    with Ed do
    begin
      if (Lines.Chars[CaretStrPos+1]=':') then
        DeleteText(1);
    end;

  if FAcpAgain then
    DoAcpPopup //show ACP again
  else
    DoFuncHintPopup; //show func hint
end;

function _IsWordChar_Css(ch: WideChar): boolean;
begin
  Result:= ecStrUtils.IsWordChar(ch) or (Pos(ch, '-.') > 0);
end;

procedure TfmMain.DoAcpCss(List, Display: ecUnicode.TWideStrings);
var
  SProp: string;
  AcpStr, Str: Widestring;
  i: Integer;
begin
  if FAcpIntCss.Count=0 then
    begin MsgBeep; Exit end;

  opAcpChars:= cAcpCharsCss;
  FAcpCss:= true;

  //is there any css property before caret and before ":"?
  EditorGetCssTag(CurrentEditor, SProp);
  //show popup again not always
  FAcpAgain:= SProp='';

  //css property found,
  //return all values of property
  if SProp<>'' then
  begin
    AcpStr:= FAcpIntCss.Values[SProp];
    if AcpStr<>'' then
    repeat
      Str:= SGetItem(AcpStr, ',');
      if Str='' then Break;
      List.Add(Str);
      Display.Add(SAcpItem(SProp+' value', Str)); //show prop name in purple column
    until false;
  end
  //css property not found,
  //return all properties
  else
  begin
    for i:= 0 to FAcpIntCss.Count-1 do
    begin
      Str:= FAcpIntCss.Names[i];
      if Str<>'' then
      begin
        List.Add(Str+': ');
        Display.Add(SAcpItem('css', Str));
      end;
    end;
  end;
end;

procedure TfmMain.DoAcpHtm(List, Display: ecUnicode.TWideStrings);
var
  STag, SAtr: string;
begin
  if FAcpIntHtml.Count=0 then
    begin MsgBeep; Exit end;

  //get current HTML tag and attrib
  EditorGetHtmlTag(CurrentEditor, STag, SAtr);

  //are we inside CSS part style="...."?
  if SAtr='style' then
    DoAcpCss(List, Display)
  else
    DoAcpHtmForTag(STag, SAtr, List, Display);
end;

procedure TfmMain.DoAcpHtmForTag(const STag, SAtr: string;
  List, Display: ecUnicode.TWideStrings);
var
  AcpStr, str: string;
  AtrList: TStringList;
  NTag, j: integer;
  AddBr: boolean;
begin
  FAcpHtm:= true;
  opAcpChars:= cAcpCharsHtm;
  AddBr:= EditorNeedsHtmlOpeningBracket(CurrentEditor);
  AcpStr:= '';

  if STag<>'' then
  begin
    NTag:= FAcpIntHtml.IndexOfName(STag);
    if NTag>=0 then
      AcpStr:= FAcpIntHtml[NTag];

    if AcpStr<>'' then
    begin
      AtrList:= TStringList.Create;
      try
        SParseString_AcpHtml(AcpStr, SAtr, AtrList);
        if SAtr='' then
        //-----------
        //return list of all attribs of tag
        begin
          for j:= 0 to AtrList.Count-1 do
          begin
            str:= AtrList[j];
            List.Add(str+'="'); //with HTML quote
            Display.Add(SAcpItem('attrib', str));
          end;
          FAcpAgain:= true;
          Exit;
        end
        else
        //-----------
        //return list of values of attrib
        begin
          for j:= 0 to AtrList.Count-1 do
          begin
            str:= AtrList[j];
            List.Add(str);
            Display.Add(SAcpItem('value', str));
          end;
          FAcpAgain:= false;
          Exit;
        end;
      finally
        FreeAndNil(AtrList);
      end;
    end; //if AcpStr<>''
  end //if STag<>''
  else
  //------------------
  //return list of tags
  begin
    //is it closing tag?
    FAcpHtmClosing:= false;
    with CurrentEditor do
      if TextLength>0 then
      begin
        j:= CaretStrPos;
        while IsWordChar(Lines.Chars[j]) do Dec(j);
        FAcpHtmClosing:= Lines.Chars[j]='/';
      end;

    if FAcpHtmClosing then
    begin
      //return list of closing tags
      for j:= 0 to FAcpIntHtml.Count-1 do
      begin
        str:= FAcpIntHtml.Names[j];
        if str<>'' then
        begin
          List.Add(str);
          Display.Add(SAcpItem('tag', '</'+str+'>'));
        end;
      end;
      FAcpAgain:= false;
      FAcpHtmTags:= true;
    end
    else
    begin
      //return list of opening tags
      for j:= 0 to FAcpIntHtml.Count-1 do
      begin
        str:= FAcpIntHtml.Names[j];
        if str<>'' then
        begin
          List.Add(IfThen(AddBr, '<') + str + ' ');
          Display.Add(SAcpItem('tag', '<'+str+'>'));
        end;
      end;
      FAcpAgain:= true;
      FAcpHtmTags:= true;
    end;
  end;
end;

procedure TfmMain.ecACPGetAutoCompleteList(Sender: TObject; PosX: TPoint;
  List, Display: ecUnicode.TWideStrings);
var
  Lexer: string;
begin
  List.Clear;
  Display.Clear;

  FAcpCss:= false;
  FAcpHtm:= false;
  FAcpHtmTags:= false;
  FAcpHtmClosing:= false;
  Lexer:= CurrentLexer;

  if opAcpForceText then
  begin
    DoAcpFromFile(List, Display);
    Exit
  end;

  if (opAcpHtm and IsLexerHTML(Lexer)) or
     (opAcpCss and IsLexerCSS(Lexer, false)) then
    begin
      LoadHtmlAndCssLists;
      if IsLexerCSS(Lexer) then
        DoAcpCss(List, Display)
      else
        DoAcpHtm(List, Display);
      DoAcpFromFile(List, Display);
      Exit
    end;

  ParamCompletion.CloseUp(False); //or Stack overflow

  if FAcpList_Items.Count>0 then
  begin
    List.Assign(FAcpList_Items);
    Display.Assign(FAcpList_Display);
    FAcpAgain:= false;
  end;

  //get words from file
  DoAcpFromFile(List, Display);
end;

procedure TfmMain.ecACPCheckChar(Sender: TObject; C: Word;
  var IsWord: Boolean);
begin
  IsWord:= IsWord or
    (Pos(Chr(c), '_' + opAcpChars) > 0);
end;

procedure TfmMain.PopupEditorPopup(Sender: TObject);
var
  Ed: TSyntaxMemo;
  s: Widestring;
begin
  Ed:= CurrentEditor;
  if Ed.TextLength=0 then
  begin
    TBXItemCtxCopyUrl.Enabled:= false;
    TBXItemCtxOpenSel.Enabled:= false;
    TBXItemCtxAddColor.Enabled:= false;
    TBXItemCtxFindID.Enabled:= false;
    TBXItemCtxCopyAppend.Enabled:= false;
    TBXItemCtxCutAppend.Enabled:= false;
    TBXItemCtxCopyHTML.Enabled:= false;
    TBXItemCtxCopyRTF.Enabled:= false;
    Exit;
  end;

  //update all
  TBXItemCtxCopyAppend.Enabled:= Ed.HaveSelection;
  TBXItemCtxCutAppend.Enabled:= Ed.HaveSelection;
  TBXItemCtxCopyHTML.Enabled:= Ed.HaveSelection;
  TBXItemCtxCopyRTF.Enabled:= Ed.HaveSelection;
  TBXItemCtxPaste.Enabled:= Clipboard.HasFormat(cf_text);
  TBXItemCtxFindID.Enabled:= (CurrentFrame.FileName<>'') and IsLexerFindID(CurrentLexer);

  //update "Copy URL"
  {
  //old code (caret pos wasn't corrected for RClick)
  p:= Ed.ScreenToClient(Mouse.CursorPos);
  p:= Ed.MouseToCaret(p.x, p.y);
  p:= CurrentFrame.HyperlinkHighlighter.HltRangeBndAt(Ed.CaretPosToStrPos(p));
  }
  FPopupUrl:= CurrentFrame.SUrlAt(Ed.CaretPos);
  TBXItemCtxCopyUrl.Enabled:= FPopupUrl<>'';

  //update "Add to recent colors"
  TbxItemCtxAddColor.Enabled:= UpdateCurrentColorCode(FPopupColor);

  //update "Open selection"
  s:= EditorSelectionForGotoCommand(Ed);
  TbxItemCtxOpenSel.Caption:= WideFormat(DKLangConstW('opf'), [s]);
  TbxItemCtxOpenSel.Enabled:= s<>'';

  //update External Tools items
  UpdateTools;
end;

procedure TfmMain.TBXItemCtxCopyUrlClick(Sender: TObject);
begin
  Clipboard.AsText:= FPopupUrl;
end;

(*
function TfmMain.EscapedAcpChars: string;
const
  Ch = '.#()[]{}<>^$*+-?|:';
var
  i:Integer;
begin
  Result:= '\w';
  for i:= 1 to Length(opAcpChars) do
    if Pos(opAcpChars[i], Ch)>0 then
      Result:= Result + '\' + opAcpChars[i]
    else
      Result:= Result + opAcpChars[i];
end;
*)

procedure TfmMain.SyntaxManagerChange(Sender: TObject);
var
  en: boolean;
  Lexer: string;
begin
  UpdateTools;
  acSetupLexerStyles.Enabled:= SyntaxManager.CurrentLexer<>nil;
  acSetupLexerNew.Enabled:= acSetupLexerStyles.Enabled;
  
  Lexer:= '';
  opWordChars:= '';

  if SyntaxManager.CurrentLexer<>nil then
  begin
    Lexer:= SyntaxManager.CurrentLexer.LexerName;

    //some overrides for few lexers
    if IsLexerCSS(Lexer) then
    begin
      opAcpChars:= cAcpCharsCss;
      opWordChars:= cAcpCharsCss;
    end
    else
    if IsLexerHTML(Lexer) then
    begin
      opAcpChars:= cAcpCharsHtm;
      opWordChars:= cAcpCharsHtm;
    end
    else
    if IsLexerPHP(Lexer) then
    begin
      opAcpChars:= cAcpCharsPhp;
      opWordChars:= cAcpCharsPhp;
    end;

    //load external ACP file
    UpdateAcp(Lexer);
  end
  else
  begin
    UpdateAcp('');
    opAcpChars:= '.'; //default ACP chars for curr-file
  end;

  //update status hint
  en:= SyntaxManager.AnalyzerCount>0;
  TbxSubmenuLexers.Enabled:= en;
  StatusItemLexer.Enabled:= en;

  if not en then
  begin
    StatusItemLexer.Caption:= DKLangConstW('None');
  end
  else
  begin
    if CurrentFrame<>nil then
      StatusItemLexer.Caption:= CurrentFrame.CurrentLexer;
    if StatusItemLexer.Caption='' then
      StatusItemLexer.Caption:= DKLangConstW('None');
  end;

  //Update ACP regex
  //(special for HTML, auto for others)
  if IsLexerHTML(Lexer) then
    ecACP.StartExpr:= '\<'
  else
    ecACP.StartExpr:= '';

  ApplyLexerOverrides(CurrentFrame, Lexer);
  UpdateStatusbarTabsize;

  DoPyEvent(CurrentEditor, cSynEventOnLexer, []);
end;


procedure TfmMain.ApplyLexerOverrides(F: TEditorFrame; const Lexer: string);
  //here we override editor options:
  //a) overrides for "Lexers overrides" option
  //b) need to reduce LineSpacing for NFO files
  //c) need to set TabMode=tabs for Make files
var
  ATabStop, ATabMode, AWrap, AMargin, ASpacing, AOptFill,
  AOptWordChars, AKeepBlanks, AAutoCase, AIndent, ATabColor: string;
begin
  if F=nil then Exit;
  with F do
  begin
    if not SGetLexerOverride(opLexersOverride, Lexer,
      ATabStop, ATabMode, AWrap, AMargin, ASpacing, AOptFill,
      AOptWordChars, AKeepBlanks, AAutoCase, AIndent, ATabColor) then
    begin
      EditorMaster.TabList.AsString:= TemplateEditor.TabList.AsString;
      EditorSlave.TabList.AsString:= TemplateEditor.TabList.AsString;

      EditorMaster.TabMode:= TemplateEditor.TabMode;
      EditorSlave.TabMode:= TemplateEditor.TabMode;

      EditorMaster.LineSpacing:= TemplateEditor.LineSpacing;
      EditorSlave.LineSpacing:= TemplateEditor.LineSpacing;
    end
    else
    begin
      //1) override TabStops
      if ATabStop<>'' then
      begin
        EditorMaster.TabList.AsString:= ATabStop;
        EditorSlave.TabList.AsString:= EditorMaster.TabList.AsString;
      end;  

      //2) override TabMode
      if ATabMode<>'' then
      begin
        case StrToIntDef(ATabMode, 1) of
          0: EditorMaster.TabMode:= tmSpaces;
          1: EditorMaster.TabMode:= tmTabChar;
          2: EditorMaster.TabMode:= tmSmartTab;
        end;
        EditorSlave.TabMode:= EditorMaster.TabMode;
      end;

      //3) override "word wrap"
      if AWrap<>'' then
      begin
        case StrToIntDef(AWrap, 0) of
          0: //wrap off
            EditorMaster.WordWrap:= false;
          1: //wrap by window edge
            begin
              EditorMaster.WordWrap:= true;
              EditorMaster.Options:= EditorMaster.Options - [soBreakOnRightMargin];
            end;
          2: //wrap by right margin
            begin
              EditorMaster.WordWrap:= true;
              EditorMaster.Options:= EditorMaster.Options + [soBreakOnRightMargin];
            end;
        end;
        EditorSlave.WordWrap:= EditorMaster.WordWrap;
        EditorSlave.Options:= EditorMaster.Options;
      end;

      //4) override "Right margin"
      if AMargin<>'' then
      begin
        EditorMaster.RightMargin:= StrToIntDef(AMargin, 80);
        EditorSlave.RightMargin:= EditorMaster.RightMargin;
      end;  

      //5) override "Line spacing"
      if ASpacing<>'' then
      begin
        EditorMaster.LineSpacing:= StrToIntDef(ASpacing, 1);
        EditorSlave.LineSpacing:= EditorMaster.LineSpacing;
      end;  

      //6) override "Optimal fill"
      if AOptFill<>'' then
      begin
        if Bool(StrToIntDef(AOptFill, 0)) then
          begin
            EditorMaster.Options:= EditorMaster.Options + [soOptimalFill];
            EditorSlave.Options:= EditorSlave.Options + [soOptimalFill];
          end
        else
          begin
            EditorMaster.Options:= EditorMaster.Options - [soOptimalFill];
            EditorSlave.Options:= EditorSlave.Options - [soOptimalFill];
          end;
      end;

      //7) override "Word chars"
      opWordChars:= AOptWordChars;

      //8) override "Keep trailing blanks"
      if AKeepBlanks<>'' then
      begin
        if Bool(StrToIntDef(AKeepBlanks, 0)) then
        begin
          EditorMaster.Options:= EditorMaster.Options + [soKeepTrailingBlanks];
          EditorSlave.Options:= EditorSlave.Options + [soKeepTrailingBlanks];
        end
        else
        begin
          EditorMaster.Options:= EditorMaster.Options - [soKeepTrailingBlanks];
          EditorSlave.Options:= EditorSlave.Options - [soKeepTrailingBlanks];
        end;
      end;

      //9) override "Auto-correct case"
      if AAutoCase<>'' then
        opAutoCase:= AAutoCase='1';

      //10) override "Block indent"
      if AIndent<>'' then
      begin
        EditorMaster.BlockIndent:= StrToIntDef(AIndent, 4);
        EditorSlave.BlockIndent:= EditorMaster.BlockIndent;
      end;

      //11) override "Tab color"
      if ATabColor<>'' then
        DoSetFrameTabColor(F, StringToColor(ATabColor));
    end;

    //overrides for "NFO files"
    if IsLexerNFO(Lexer) then
    begin
      EditorMaster.LineSpacing:= 0;
      EditorSlave.LineSpacing:= 0;
    end;

    //overrides for "Make files"
    if IsLexerMake(Lexer) then
    begin
      EditorMaster.TabMode:= tmTabChar;
      EditorSlave.TabMode:= tmTabChar;
    end;
  end;
end;


(*
procedure TfmMain.DoRepaintTBs;
begin
  {$ifndef FixRepaint}
  tbMenu.Invalidate;
  tbFile.Invalidate;
  tbEdit.Invalidate;
  tbView.Invalidate;
  tbQs.Invalidate;
  edQs.Invalidate;

  tbUser1.Invalidate;
  tbUser2.Invalidate;
  tbUser3.Invalidate;

  plTree.Invalidate;
  plOut.Invalidate;
  plClip.Invalidate;

  Tree.Invalidate;
  TreeFind.Invalidate;
  ListOut.Invalidate;
  ListVal.Invalidate;
  ListPLog.Invalidate;
  ListTabs.Invalidate;
  MemoConsole.Invalidate;

  if Assigned(fmProj) then
    fmProj.TreeProj.Invalidate;
  if Assigned(fmClip) then
    fmClip.ListClip.Invalidate;
  if Assigned(fmClips) then
    fmClips.List.Invalidate;

  tbTabsLeft.Invalidate;
  tbTabsOut.Invalidate;
  tbTabsRight.Invalidate;
  {$endif}

  DoPlugins_Repaint;
end;
*)

(*
procedure TfmMain.DoRepaintTBs2;
begin
  {$ifndef FixRepaint}
  if Assigned(fmClip) then
    FixDraw(fmClip.ListClip, true);

  if Assigned(fmMap) then
    FixDraw(fmMap.edMap, true);

  FixDraw(Tree);

  if CurrentEditor<>nil then
    FixDraw(CurrentEditor, true);
  {$endif}
end;
*)

procedure TfmMain.FormResize(Sender: TObject);
begin
  {$ifndef FixRepaint}
  DoRepaintTBs;
  {$endif}
  SyncMapPos;
end;

procedure TfmMain.ecACPChange(Sender: TObject);
var
  i: integer;
  s: string;
const
  sep: TSysCharSet = ['.',' ',#9,'-'];
begin
  if opAcpUseSingle then
    with ecACP do
      if ListBox.Items.Count=1 then
      begin
        CloseUp(true);
        Exit
      end;

  //update ACP hint
  ecACP.ToolHint.Text:='';
  i:= ecACP.ListBox.ItemIndex;
  if (i>=0) and (i<ecACP.Items.Count) then
  begin
    s:= ecACP.GetListItem(i);
    i:= FAcpList_Items.IndexOf(s);
    if (i>=0) and
      (i<FAcpList_Hints.Count) and
      (i<FAcpList_Desc.Count) then
      ecACP.ToolHint.Text:= Trim(
        '\s4\' + SAcpItemToId(s) + ' ' +
        '\s5\' + WrapText(FAcpList_Hints[i], sLineBreak+'\s5\', sep, 70) + sLineBreak +
        '\s6\' + WrapText(FAcpList_Desc[i], sLineBreak+'\s6\', sep, 65));
  end;
end;

procedure TfmMain.MenuitemSetEncoding(Sender: TObject);
begin
  if Sender is TSpTbxItem then
    if not (Sender as TSpTbxItem).Checked then
      ApplyFrameEncodingAndReload(CurrentFrame, (Sender as TComponent).Tag);
end;

procedure TfmMain.MenuitemConvertEncoding(Sender: TObject);
begin
  if Sender is TSpTbxItem then
    if not (Sender as TSpTbxItem).Checked then
    begin
      ApplyFrameEncodingAndReload(CurrentFrame, (Sender as TComponent).Tag, false{ACanReload});
      CurrentFrame.Modified:= true;
    end;
end;

procedure TfmMain.DoSetFrameEncoding(Frame: TEditorFrame; AEnc: Integer);
begin
  if Frame<>nil then
  with Frame do
  begin
    SkipBom:= False;
    case AEnc of
    cp__UTF8:
      begin
        EditorMaster.TextSource.Lines.CodePage:= 0;
        EditorMaster.TextSource.Lines.TextCoding:= tcUTF8;
      end;
    cp__UTF8_noBOM:
      begin
        SkipBom:= True;
        EditorMaster.TextSource.Lines.CodePage:= 0;
        EditorMaster.TextSource.Lines.TextCoding:= tcUTF8;
      end;
    cp__Unicode:
      begin
        EditorMaster.TextSource.Lines.CodePage:= 0;
        EditorMaster.TextSource.Lines.TextCoding:= tcUnicode;
      end;
    cp__UnicodeBE:
      begin
        EditorMaster.TextSource.Lines.CodePage:= 0;
        EditorMaster.TextSource.Lines.TextCoding:= tcSwapUnicode;
      end;
    else
      begin
        EditorMaster.TextSource.Lines.TextCoding:= tcANSI;
        EditorMaster.TextSource.Lines.CodePage:= AEnc;
      end;
    end;
  end;
end;


procedure TfmMain.ApplyFrameEncodingAndReload(Frame: TEditorFrame; AEnc: Integer;
  ACanReload: boolean = true);
begin
  if Frame<>nil then
    with Frame do
    begin
      DoSetFrameEncoding(Frame, AEnc);

      if ACanReload and (FileName <> '') then
        if (not Modified) or MsgEncReload then
        begin
          if IsFileWithBOM(FileName) then
            MsgWarn(WideFormat(DKLangConstW('cpBOM'), [WideExtractFileName(FileName)]), Handle);
          Modified:= False;
          EditorMaster.TextSource.Lines.SkipSignature:= True;
          DoFrameReloadInt(Frame);
          EditorMaster.TextSource.Lines.SkipSignature:= False;
         end;

      //if not ACanReload then
      //  EditorSetModified(Frame.EditorMaster);
    end;

  UpdateStatusBar;
end;

function TfmMain.MsgConfirmFtp: boolean;
begin
  Result:= MsgConfirm(DKLangConstW('zMFtpOp'), Handle);
end;

function TfmMain.MsgEncReload: boolean;
begin
  with TfmEnc.Create(Self) do
  try
    Left:= Self.Monitor.Left + (Self.Monitor.Width - Width) div 2;
    Top:= Self.Monitor.Top + (Self.Monitor.Height - Height) div 2;
    Result:= ShowModal = mrYes;
  finally
    Release;
  end;
end;

procedure TfmMain.UpdateStatusbarEnc(F: TEditorFrame);
begin
  if F<>nil then
  with F do
  case EditorMaster.TextSource.Lines.TextCoding of
    tcAnsi:
    begin
      case EditorMaster.TextSource.Lines.Codepage of
      CP_ACP:
        StatusItemEnc.Caption:= 'ANSI';
      CP_OEMCP:
        StatusItemEnc.Caption:= 'OEM';
      CP_MACCP:
        StatusItemEnc.Caption:= 'Mac';
      else
        StatusItemEnc.Caption:= IntToStr(EditorMaster.TextSource.Lines.Codepage);
      end;
    end;
    tcUnicode:
      StatusItemEnc.Caption:= 'UTF-16';
    tcSwapUnicode:
      StatusItemEnc.Caption:= 'UTF-16 BE';
    tcUTF8:
    begin
      if SkipBom then
        StatusItemEnc.Caption:= DKLangConstW('cpUTF8no')
      else
        StatusItemEnc.Caption:= 'UTF-8';
    end;
  end;
end;


procedure TfmMain.DoFinderInit(AKeepFlags: boolean = false);
var
  IsSpec, IsSel,
  IsCase, IsWords,
  IsRe, IsRe_s, //IsRe_m,
  IsForw, IsWrap, IsSkipCol: boolean;
  SText: Widestring;
begin
  IsCase:= false;
  IsWords:= false;

  if Assigned(fmSR) then
  begin
    SText:= '';
    IsSel:= fmSR.OpInSel;
    IsForw:= not fmSR.OpBack;
    IsRe:= fmSR.OpRe;
    IsRe_s:= fmSR.OpReDot;
    if AKeepFlags then
    begin
      IsCase:= fmSR.OpCase;
      IsWords:= fmSR.OpWords;
    end;
    IsWrap:= fmSR.OpWrap;
    IsSpec:= fmSR.OpSpec;
    IsSkipCol:= fmSR.cbSkipCol.Checked;
  end
  else
  with TIniFile.Create(SynHistoryIni) do
  try
    SText:= DoReadTotalHistory;
    IsSel:= false; //not saved
    IsForw:= ReadBool('Search', 'Forw', true);
    IsRe:= ReadBool('Search', 'RegExp', false);
    IsRe_s:= ReadBool('Search', 'RegExpS', false);
    //IsRe_m:= ReadBool('Search', 'RegExpM', true);
    if AKeepFlags then
    begin
      IsCase:= ReadBool('Search', 'Case', false);
      IsWords:= ReadBool('Search', 'Words', false);
    end;
    IsWrap:= ReadBool('Search', 'Wrap', false);
    IsSpec:= ReadBool('Search', 'Spec', false);
    IsSkipCol:= ReadBool('Search', 'SkipCol', false);
  finally
    Free;
  end;

  //restore text only when a) it empty, b) ini string not empty
  if (Finder.FindText='') and (SText<>'') then
    Finder.FindText:= SText;

  //restore flags, but not all of them
  //(as Case/Words opts are configurable in QSearch)
  Finder.Tokens:= tokensAll;
  Finder.Flags:= [];
  if IsSel then Finder.Flags:= Finder.Flags + [ftSelectedText];
  if not IsForw then Finder.Flags:= Finder.Flags + [ftBackward];
  if IsRe then Finder.Flags:= Finder.Flags + [ftRegex];
  if IsRe_s then Finder.Flags:= Finder.Flags + [ftRegex_s];
  if AKeepFlags then
  begin
    if IsCase then Finder.Flags:= Finder.Flags + [ftCaseSens];
    if IsWords then Finder.Flags:= Finder.Flags + [ftWholeWords];
  end;
  if IsWrap then Finder.Flags:= Finder.Flags + [ftWrapSearch];
  if IsSkipCol then Finder.Flags:= Finder.Flags + [ftSkipCollapsed];
  if not IsRE and IsSpec then
    Finder.FindText:= SDecodeSpecChars(Finder.FindText);
end;


function TfmMain.DoReadTotalHistory: Widestring;
var
  fnTC: string;
  SA: Ansistring;
begin
  fnTC:= SExpandVars('%Commander_ini%');
  if opListerTcHistory and (not SynExe) and SExpanded(fnTC) then
  begin
    //handle RedirectSection
    FixTcIni(fnTC, 'SearchText');

    //read TC ini
    with TIniFile.Create(fnTC) do
    try
      SA:= ReadString('SearchText', '0', '');
      if Pos(cUtfSign, SA)=1 then
      begin
        Delete(SA, 1, Length(cUtfSign));
        Result:= UTF8Decode(SA);
      end
      else
        Result:= SA;
    finally
      Free;
    end;
  end
  else
    with TIniFile.Create(SynIni) do
    try
      Result:= UTF8Decode(ReadString('SearchText', '0' , ''));
    finally
      Free;
    end;
end;


procedure TfmMain.ecFindExecute(Sender: TObject);
begin
  if IsTreeviewFocused then
    ecFindInTree.Execute
  else
  if IsListboxFocused then
    ecFindInList.Execute
  else
    DoFindDialog(false);
end;

procedure TfmMain.ecReplaceExecute(Sender: TObject);
begin
  DoFindDialog(true);
end;

procedure TfmMain.DoFindDialog(AReplaceMode: boolean);
var
  Ed: TSyntaxMemo;
  Ctl: TWinControl;
begin
  Ed:= CurrentEditor;

  if fmSR=nil then
  begin
    fmSR:= TfmSR.Create(Self);
    with fmSR do
    begin
      SRHistTC:= opListerTcHistory and not SynExe;
      SRCount:= opSaveFindCount;
      SRIni:= SynHistoryIni;
      SRIniS:= SynHistoryIni;
      SRProc:= Self.DoFind_ActionWrapper;
      OnFocusEditor:= Self.DoFindDialog_OnFocusEditor;
      OnDockedChanged:= Self.DoFindDialog_OnDockedChanged;
      OnShowStatus:= Self.DoFindDialog_OnShowStatus;
      OnRepaintNeeded:= Self.DoFindDialog_OnRepaintNeeded;
      cbTokens.Enabled:= SyntaxManager.AnalyzerCount>0;
      LoadIni;

      if ShowOnTop then
        SetFormOnTop(fmSR.Handle, ShowOnTop);
      if not ShowOnTop then
      begin
        //Seems it OK sets ShowOnTop style, only for app
        if opFindOnTop then
          FormStyle:= fsStayOnTop
        else
          FormStyle:= fsNormal;
      end;
    end;
  end;

  with fmSR do
  begin
    SR_SuggestedSel:= '';
    SR_SuggestedSelEn:= Ed.HaveSelection;
    SR_SuggestedSelScope:= SR_SuggestedSelEn and EditorHasMultilineSelection(Ed);

    Sh_FindNext:= GetShortcutOfCmd(smFindNext);
    Sh_FindMode:= GetShortcutOfCmd(smFindDialog);
    Sh_ReplaceMode:= GetShortcutOfCmd(smReplaceDialog);

    if (not SR_SuggestedSelScope){careful} then
    begin
      if opFindSuggestSel and (Ed.SelLength>0) then
        SR_SuggestedSel:= Ed.SelText
      else
      if opFindSuggestWord then
        SR_SuggestedSel:= Ed.WordAtPos(Ed.CaretPos);
    end;

    OpInSel:= SR_SuggestedSelScope;

    if SR_SuggestedSel<>'' then
    begin
      DoCopyToEdit(ed1, OpSpec, OpRe, SR_SuggestedSel);
      ed1Memo.Text:= ed1.Text;
    end;
    ed1Change(Self);

    IsReplace:= AReplaceMode;

    Show;
    if Visible and Enabled then
    begin
      if IsMultiline then Ctl:= ed1Memo else Ctl:= ed1;
      if Ctl.Enabled and Ctl.Visible and Ctl.CanFocus then
        Ctl.SetFocus;
    end;
  end;
end;

  function TfmMain.IsProgressNeeded(Ed: TSyntaxMemo): boolean;
  begin
    Result:= false; ////Ed.TextLength >= cMinProgressFilesize;
      //showing causes bug: replace in 2 tabs, tabs with size>MinProgressFilesize,
      //and replaced only in active tab
  end;

  function TfmMain.IsProgressStopped(const NDoneSize, NTotalSize: Int64): boolean;
  begin
    Result:= false;
    try
      if NTotalSize<=0 then
        raise Exception.Create('NTotalSize<=0');
      if Assigned(fmProgress) then
      begin
        fmProgress.Pro.Progress:= NDoneSize * 100 div NTotalSize;
        Application.ProcessMessages;
        Result:= StopFind;
      end;
    except
      on E: Exception do
        MsgExcept('Error on msg processing', E, Handle);
    end;
  end;

  procedure TfmMain.UpdateFormEnabled(En: boolean);
  begin
    Groups.Enabled:= En;
    plTree.Enabled:= En;
    plClip.Enabled:= En;
    plOut.Enabled:= En;

    Menu.Enabled:= En;
    TbxDockTop.Enabled:= En;
    TbxDockLeft.Enabled:= En;
    TbxDockLeft1.Enabled:= En;
    TbxDockRight.Enabled:= En;
    TbxDockRight1.Enabled:= En;
    TbxDockBottom.Enabled:= En;
    TbxDockBottom1.Enabled:= En;

    if Assigned(fmSR) then
      fmSR.IsEnabled:= En;

    if Assigned(fmNumConv) then
      fmNumConv.Enabled:= En;

    Screen.Cursor:= cDefaultCursor[En];
  end;

  procedure TfmMain.DoProgressShow(AMode: TProgressType);
  begin
    UpdateFormEnabled(false);

    if not Assigned(fmProgress) then
    begin
      fmProgress:= TfmProgress.Create(Self);
      fmProgress.ParentWindow:= Status.Handle; //"Parent:= Status" doesn't work
      fmProgress.BorderStyle:= bsNone;
      fmProgress.Align:= alClient;
    end;
    fmProgress.SetStatusMode(true);
    fmProgress.SetMode(AMode);
    fmProgress.Show;
    with fmProgress.bCan do
      if CanFocus then SetFocus;
    //DoRepaint; //no need

    FinderPro:= fmProgress.Pro;
    StopFind:= false;
    Application.ProcessMessages;
  end;

  procedure TfmMain.DoProgressHide;
  begin
    UpdateFormEnabled(true);

    if Assigned(fmProgress) then
    begin
      fmProgress.Hide;
      DoRepaint; //needed anyway, even if controls not resized
    end;

    if Assigned(fmSR) and fmSR.Visible and fmSR.Enabled and fmSR.CanFocus then
      fmSR.SetFocus
    else
    if not edQs.Focused then
      FocusEditor;
  end;

procedure TfmMain.DoFind_ActionWrapper(act: TSynSearchAction);
var
  Ed: TSyntaxMemo;
  oldStart, oldLength: integer;
begin
  Ed:= CurrentEditor;
  FinderPro:= nil;
  FinderProNum:= 0;
  oldStart:= Ed.SelStart;
  oldLength:= Ed.SelLength;

  if IsProgressNeeded(Ed) then
    DoProgressShow;

  try
    DoFind_Action(act);
  finally
    FinderPro:= nil;
    FinderProNum:= 0;
    DoProgressHide;
  end;

  //record find action in macro
  if Assigned(fmSR) then
    DoMacro_RecordCommand(
      sm_FindCommand,
      PWChar(WriteFindOptions(act, fmSR.TextOptions, fmSR.Text1, fmSR.Text2)));

  //extend selection
  if act in [cfActionFindNext] then
  if Assigned(fmSR) and fmSR.OpExtSel then
    EditorExtendSelectionByPosition(Ed,
      oldStart, oldLength,
      Ed.SelStart, Ed.SelLength);

  //set "From caret" dialog option
  if act in [cfActionFindNext, cfActionSkip, cfActionReplaceNext] then
    if Finder.Matches>0 then
      if Assigned(fmSR) then
        fmSR.SetFromCaret;
end;


procedure TfmMain.DoFindDialog_ReplaceOrSkip(ADoReplace, AGotoNext: boolean);
var
  Ok, OkReplaced: boolean;
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  Finder.Flags:= Finder.Flags-[ftPromtOnReplace];

  //move to sel start
  if (Ed.SelLength>0) then
    DoFixReplaceCaret(Ed);

  //replace only when sel present
  OkReplaced:= false;
  Ok:= (Ed.SelLength>0) or (Finder.Matches>0);
  if Ok then
    if ADoReplace then
    begin
      //Bug1: RepAgain doesn't replace 1st match if caret
      //already at match and match_len =0 (search for '^')
      Finder.ReplaceAgain;
      OkReplaced:= Finder.Matches>0;
    end;

  //sel next match
  if AGotoNext then
  begin
    Finder.FindAgain;

    //workaround for Bug1
    DoWorkaround_FindNext1;

    //final actions
    Ok:= Finder.Matches>0;
    if Ok and (Ed.SelLength>0) then
      DoFixReplaceCaret(Ed);
    if Ok then
      EditorCheckCaretOverlappedByForm(Finder.Control, fmSR);
  end
  else
    Ok:= false;

  //show message in Find dialog status
  if not OkReplaced then
  begin
    if Ok then
      fmSR.ShowStatus(DKLangConstW('zMResRepNoFoundYes'))
    else
      fmSR.ShowStatus(DKLangConstW('zMResRepNoFoundNo'));
  end
  else
  begin
    if Ok then
      fmSR.ShowStatus(DKLangConstW('zMResRepYesFoundYes'))
    else
      fmSR.ShowStatus(DKLangConstW('zMResRepYesFoundNo'));
  end;
end;

procedure TfmMain.DoFindDialog_FindNext;
var
  Ok: boolean;
begin
  Finder.FindAgain;
  Ok:= Finder.Matches>0;
  if Ok then
    EditorCheckCaretOverlappedByForm(Finder.Control, fmSR);
  if Ok then
    fmSR.ShowStatus(DKLangConstW('zMResFound'))
  else
    fmSR.ShowStatus(DKLangConstW('zMResFoundNo'));
end;

procedure TfmMain.DoFindDialog_CountAllInCurrentTab;
begin
  Finder.CountAll;
  fmSR.ShowError(Finder.Matches=0);
end;

procedure TfmMain.DoFindDialog_ReplaceAllInCurrentTab;
var
  Ok, OkSel: boolean;
  SText: Widestring;
  Sel: TSynSelSave;
  OldScrollPosY: integer;
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  Ok:= ftPromtOnReplace in Finder.Flags;
  OkSel:= ftSelectedText in Finder.Flags;
  if Ok then
  begin
    fmSR.Hide;
    SText:= fmSR.Text1;
  end;
  if OkSel then
    EditorSaveSel(Ed, Sel);
  OldScrollPosY:= Ed.ScrollPosY;
  //
  Finder.ReplaceAll;
  //
  Ed.ScrollPosY:= OldScrollPosY;
  if OkSel then
    EditorRestoreSel(Ed, Sel);
  fmSR.ShowError(Finder.Matches=0);
  if Ok then
  begin
    fmSR.Show;
    fmSR.Text1:= SText;
  end;
end;

procedure TfmMain.DoFindDialog_ReplaceAllInAllTabs(var AFilesReport: Widestring);
var
  nMatches, nFiles: Integer;
begin
  if FrameAllCount<2 then
    MsgWarn(DKLangConstW('fnMul'), Handle)
  else
  begin
    DoReplace_InAllTabs(nMatches, nFiles);
    Finder.Matches:= nMatches;
    fmSR.ShowError(Finder.Matches=0);
    AFilesReport:= WideFormat(DKLangConstW('fn_f'), [nFiles]);
  end;
end;

procedure TfmMain.DoFind_Action(act: TSynSearchAction);
var
  SMsg, SMsgFiles: Widestring;
  SMsgRegex: string;
begin
  SMsg:= '';
  SMsgFiles:= '';

  with fmSR do
  begin
    if Text1='' then
      begin MsgBeep(true); Exit end;

    Finder.Flags:= [];
    if OpCase then Finder.Flags:= Finder.Flags + [ftCaseSens];
    if OpWords then Finder.Flags:= Finder.Flags + [ftWholeWords];
    if OpRe then Finder.Flags:= Finder.Flags + [ftRegex];
    if OpReDot then Finder.Flags:= Finder.Flags + [ftRegex_s];
    if OpInSel then Finder.Flags:= Finder.Flags + [ftSelectedText];
    if OpBack then Finder.Flags:= Finder.Flags + [ftBackward];
    if OpCfm then Finder.Flags:= Finder.Flags + [ftPromtOnReplace];
    if OpWrap then Finder.Flags:= Finder.Flags + [ftWrapSearch];
    if cbSkipCol.Checked then Finder.Flags:= Finder.Flags + [ftSkipCollapsed];

    //handle "From caret" specially: ignore it for "Replace all" actions
    if (not OpFromCaret) or
      (act in cSearchIngoreFromCaret) then
      Finder.Flags:= Finder.Flags + [ftEntireScope];

    Finder.Tokens:= TSearchTokens(cbTokens.ItemIndex);
    Finder.FindText:= Text1;
    Finder.ReplaceText:= Text2;
    if OpSpec then
    begin
      Finder.FindText:= SDecodeSpecChars(Finder.FindText);
      Finder.ReplaceText:= SDecodeSpecChars(Finder.ReplaceText);
    end;

    Finder.OnCanAccept:= Finder_OnCanAccept;
  end;

  //check regex valid
  if ftRegex in Finder.Flags then
    if not IsRegexValid(Finder.FindText, SMsgRegex) then
    begin
      fmSR.ShowStatus(DKLangConstW('zMRegexInvalid')+': '+SMsgRegex);
      MsgBeep;
      Exit
    end;

  case act of
    cfActionFindNext:
      DoFindDialog_FindNext;
    //
    cfActionReplaceNext:
      //don't jump to next match, if Ctrl is pressed (feature)
      DoFindDialog_ReplaceOrSkip(true, not IsCtrlPressed);
    //
    cfActionSkip:
      DoFindDialog_ReplaceOrSkip(false, true);
    //
    cfActionFindAll:
      DoFindDialog_FindAllInCurrentTab(
        fmSR.OpBkmkAll,
        fmSR.OpSelectAll);
    //
    cfActionCount:
      DoFindDialog_CountAllInCurrentTab;
    //
    cfActionFindInTabs:
      DoFindDialog_FindAllInAllTabs;
    //
    cfActionReplaceAll:
      DoFindDialog_ReplaceAllInCurrentTab;
    //
    cfActionReplaceAllInAll:
      DoFindDialog_ReplaceAllInAllTabs(SMsgFiles);
  end;

  Finder.OnCanAccept:= nil;
  if Finder.Matches>0 then
  begin
    SMsg:= WideFormat(DKLangConstW('Found'), [Finder.Matches]) + ' ' + SMsgFiles;
    DoHint(SMsg);
    if act in [cfActionCount, cfActionFindAll, cfActionReplaceAll, cfActionReplaceAllInAll] then
      fmSR.ShowStatus(SMsg);
  end;
end;

//Reason of this:
//1- Lister plugin doesnt handle some keys
//2- QuickSearch edit focused doesnt handle keys
procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: integer;
  S: string;
begin
  //code below needed only for Lister plugin
  if SynExe then Exit;
  if Assigned(fmSR) then
    if fmSR.ed1.Focused or fmSR.ed2.Focused or fmSR.ed1Memo.Focused or fmSR.ed2Memo.Focused then Exit;

  //Ctrl+Space
  if (Key = vk_space) and (Shift = [ssCtrl]) then Exit;

  //PgUp/PgDn on ACP list shown
  if (Key = vk_prior) or (Key = vk_next) then
    if ecACP.ListBox.Visible then Exit;

  //Clip keys
  if fmClip.ListClip.Focused then
  begin
    if (Key = Ord('X')) and (Shift = [ssCtrl]) then Exit;
    if (Key = Ord('C')) and (Shift = [ssCtrl]) then Exit;
    if (Key = vk_delete) and (Shift = [ssShift]) then Exit;
  end;

  //Clips keys
  if Assigned(fmClips) and fmClips.ListNames.Focused then
  begin
    if (Key = vk_insert) and (Shift = []) then Exit;
    if (Key = vk_delete) and (Shift = [ssShift]) then Exit;
    if (Key = Ord('C')) and (Shift = [ssCtrl]) then Exit;
  end;

  //QSearch keys
  if edQs.Focused then
  begin
    if (Key = Ord('V')) and (Shift = [ssCtrl]) then Exit;
    if (Key = Ord('X')) and (Shift = [ssCtrl]) then Exit;
    if (Key = Ord('C')) and (Shift = [ssCtrl]) then Exit;
    if (Key = Ord('A')) and (Shift = [ssCtrl]) then Exit;
    if (Key = Ord('Z')) and (Shift = [ssCtrl]) then Exit;
    if (Key = Ord('Y')) and (Shift = [ssCtrl]) then Exit;
    if (key = vk_insert) then Exit;
    if (Key = vk_delete) then Exit;
  end;

  //Esc
  if not QuickView then
    if (Key = VK_ESCAPE) and (Shift = []) then
    begin
      //Esc in QSearch
      if edQs.Focused then
        DoHandleQuickSearchEscape
      else
      //Esc in panels
      if (CurrentEditor<>nil) and (not CurrentEditor.Focused) then
        FocusEditor
      else
        //Esc in editor
        case opEsc of
          cEscNothing:
            Exit;
          cEscCloseApp:
            acExit.Execute;
          cEscCloseTab:
            acClose.Execute;
          cEscCloseTabOrApp:
            begin
              if FrameAllCount=1 then
                acExit.Execute
              else
                acClose.Execute;
            end;
          cEscMinimizeApp:
            begin
              if SynExe then
                Application.Minimize
              else
                SendMessage(hLister, WM_SYSCOMMAND, SC_MINIMIZE, 0);
            end;
        end;
      Key:= 0;
      Exit
    end;

  //Ctrl+Tab, Ctrl+Shift+Tab
  if (Key = vk_tab) and ((Shift = [ssCtrl]) or (Shift = [ssCtrl, ssShift])) then
  begin
    DoTabSwitch(Shift = [ssCtrl]);
    Key:= 0;
    Exit
  end;

  //QView Ctrl-Q
  if QuickView and (Shift = [ssCtrl]) and (Key = Ord('Q')) then
  begin
    PostMessage(hLister, wm_keydown, vk_control, 0);
    PostMessage(hLister, wm_keydown, Key, 0);
    PostMessage(hLister, wm_keyup, Key, 0);
    PostMessage(hLister, wm_keyup, vk_control, 0);
    Key:= 0;
    Exit
  end;

  //QSearch needs:
  if ((Shift = []) and (Key = VK_delete)) or
   ((Shift = []) and (Key = vk_back)) or
   (((Shift = []) or (Shift = [ssShift])) and
    ((Key = vk_left) or (Key = vk_right) or (Key = vk_up) or (Key = vk_down)
     or (Key = vk_home) or (Key = vk_end) or (Key = vk_return))) or
   ((Shift = []) and (Key >= Ord('A')) and (Key <= Ord('Z')))
   then
     Exit;

  //Handle configured keys
  S:= ShortcutToText(Shortcut(Key, Shift));
  with SyntKeyMapping do
    for i:= 0 to Items.Count-1 do
      with Items[i] do
        if ((KeyStrokes.Count > 0) and (KeyStrokes[0].AsString = S)) or
          ((KeyStrokes.Count > 1) and (KeyStrokes[1].AsString = S)) then
        begin
          DoDelayedCommandAny(Command);
          Key:= 0;
          Exit
        end;

  //Handle tools
  for i:= Low(opTools) to High(opTools) do
   with opTools[i] do
     if (ToolCaption<>'') and (ToolCommand<>'') and (S=ToolKeys) and
       ((ToolLexer='') or (CurrentFrame.CurrentLexer=ToolLexer)) then
    begin
      DoTool_Run(opTools[i]);
      Key:= 0;
      Exit
    end;
end;

procedure TfmMain.TBXItemAbClick(Sender: TObject);
begin
  with TfmAbout.Create(Self) do
    try
      LabelWVersion.Caption:= cSynVer;
      Left:= Self.Monitor.Left + (Self.Monitor.Width - Width) div 2;
      Top:= Self.Monitor.Top + (Self.Monitor.Height - Height) div 2;
      ShowModal;
    finally
      Free
    end;
end;

procedure TfmMain.ecWrapExecute(Sender: TObject);
var
  Ed: TSyntaxMemo;
  NPos: Integer;
begin
  Ed:= CurrentEditor;
  if DoPyEvent(Ed, cSynEventOnState, [cSynPropWrap]) = cPyFalse then Exit;

  with Ed do
  begin
    NPos:= TopLine;
    WordWrap:= not WordWrap;
    TopLine:= NPos;
    if not WordWrap then
      ExecCommand(smScrollAbsLeft);
  end;

  PostMessage(hLister, WM_COMMAND, MAKELONG(Ord(Ed.WordWrap), cLister_itm_wrap), Handle);
  UpdateStatusbar;
end;

procedure TfmMain.ecLineNumsExecute(Sender: TObject);
begin
  if DoPyEvent(CurrentEditor, cSynEventOnState, [cSynPropNums]) = cPyFalse then Exit;

  with CurrentFrame do
  begin
    EditorMaster.LineNumbers.Visible:= not EditorMaster.LineNumbers.Visible;
    EditorSlave.LineNumbers.Visible:= EditorMaster.LineNumbers.Visible;
    TemplateEditor.LineNumbers.Visible:= EditorMaster.LineNumbers.Visible;
    UpdateGutterWidth(EditorMaster);
    UpdateGutterWidth(EditorSlave);
  end;

  UpdateGutter(CurrentFrame);
  UpdateStatusbar;
end;

procedure TfmMain.ecFoldingExecute(Sender: TObject);
begin
  if DoPyEvent(CurrentEditor, cSynEventOnState, [cSynPropFolding]) = cPyFalse then Exit;

  with CurrentFrame do
  begin
    EditorMaster.DisableFolding:= not EditorMaster.DisableFolding;
    EditorSlave.DisableFolding:= EditorMaster.DisableFolding;
  end;

  UpdateGutter(CurrentFrame);
  UpdateStatusbar;
end;

procedure TfmMain.ecNonPrintExecute(Sender: TObject);
begin
  with CurrentEditor do
    NonPrinted.Visible:= not NonPrinted.Visible;
  UpdateStatusbar;
end;

procedure TfmMain.acRereadExecute(Sender: TObject);
begin
  DoFrameReloadWrapper(CurrentFrame);
end;

procedure TfmMain.DoFrameReloadWrapper(F: TEditorFrame);
begin
  if (F=nil) or (F.FileName='') then
  begin
    MsgBeep;
    Exit
  end;

  if not IsFileExist(F.FileName) then
  begin
    MsgNoFile(F.FileName);
    Exit
  end;

  DoFrameReloadInt(F);
  UpdateFrameEnc(F); //calls DoFrameReloadInt

  F.EditorMaster.Lines.ResetLineStates;
  F.EditorSlave.Lines.ResetLineStates;

  F.EditorMaster.ResetSearchMarks;
  F.EditorSlave.ResetSearchMarks;
  UpdateFrameMicroMap(F);
end;

procedure TfmMain.DoClearSearchMarks(Ed: TSyntaxMemo);
begin
  Ed.ResetSearchMarks;
  UpdateFrameMicroMap(FrameOfEditor(Ed));
end;

procedure TfmMain.DoFrameReloadInt(F: TEditorFrame);
var
  p1, p2: integer;
begin
  if (F<>nil) and (F.FileName<>'') then
  begin
    if F.Modified then
      case MsgConfirmSaveFrame(F, True) of
        mrCancel:
          Exit;
        mrYes:
          if not SaveFrame(F, False) then Exit;
      end;

    p1:= F.EditorMaster.TopLine;
    p2:= F.EditorSlave.TopLine;
    F.EditorMaster.TextSource.Lines.LoadFromFile(F.FileName);
    F.EditorMaster.TopLine:= p1;
    F.EditorSlave.TopLine:= p2;

    F.Modified:= False;
    UpdateStatusbar;
  end;
end;

procedure TfmMain.TBXItemClrClick(Sender: TObject);
begin
  DoClearFilesHistory;
end;

procedure TfmMain.DoClearFilesHistory;
begin
  FDelete(SynHistoryStatesIni);
  SynMruFiles.Items.Clear;
  with TIniFile.Create(SynHistoryIni) do
  try
    EraseSection('MRU');
  finally
    Free;
  end;
end;

procedure TfmMain.tbViewMove(Sender: TObject);
begin
  if Visible then
  begin
    if not TimerLoad.Enabled then
      FToolbarMoved:= True; //set flag only after first delay
    FormResize(Self);
  end;
end;

procedure TfmMain.SetIcons(const S: string);
var
  dir, fn: string;
begin
  dir:= SynIconsDir;
  fn:= dir+'\'+S+'.tar';

  if not IsFileExist(fn) then
  begin
    fn:= dir+'\'+cIconsDefault+'.tar';
    if not IsFileExist(fn) then Exit;
  end;  

  FIcons:= S;
  DoIconSet_LoadFromTar(ImageListIcons, fn);

  tbFile.Refresh;
  tbEdit.Refresh;
  tbView.Refresh;
  tbQs.Refresh;
end;

procedure TfmMain.ApplyShowIconsInMenus;
begin
  if opShowMenuIcons then
    PopupEditor.Images:= ImageListIcons
  else
    PopupEditor.Images:= nil;
end;

procedure TfmMain.ecPrintActionBeforeExecute(Sender: TObject);
begin
  LoadPrintOptions;
  with ecSyntPrinter do
  begin
    SyntMemo:= CurrentEditor;
    PrintSelection:= CurrentEditor.HaveSelection;
  end;
  with ecPrintAction do
  begin
    PrintDialog:= nil;
    SyntPrinter:= ecSyntPrinter;
    SyntMemo:= nil; //runtime breaks this
  end;
end;

procedure TfmMain.TBXItemBarMarksClick(Sender: TObject);
begin
  with CurrentFrame do
  begin
    DoClearSearchMarks(EditorMaster);
    DoClearSearchMarks(EditorSlave);
  end;
end;

procedure TfmMain.ecACPKeyPress(Sender: TObject; var Key: Char);
const
  cNonWord = '<>/\|.,;: +*=#()[]{}%''"?!@%&';
begin
  //these chars close window
  if Pos(Key, cNonWord) > 0 then
    if Pos(Key, opAcpChars) = 0 then
      ecACP.CloseUp(False);
  //closing tag
  if Key = '/' then
    DoAcpPopup;
end;

procedure TfmMain.DoAcpPopup;
begin
  DoDelayedCommandAny(ecACP.CommandID{650});
end;

procedure TfmMain.DoFuncHintPopup;
begin
  DoDelayedCommandAny(ParamCompletion.CommandID{652});
end;

procedure TfmMain.ecACPListClick;
begin
  ecACP.CloseUp(True); //2nd click appears, 2 ids inserted
end;

procedure TfmMain.acNewTabExecute(Sender: TObject);
begin
  if Assigned(Groups) then
    DoAddTab(Groups.PagesCurrent, true);
end;

procedure TfmMain.DoHint(S: WideString);
var
  SPrefix: Widestring;
begin
  if ecMacroRec.Recording then
    SPrefix:= '['+DKLangConstW('statusmsg_macro')+'] '
  {$ifdef SPELL}
  else
  if FSpellChecking then
    SPrefix:= '['+DKLangConstW('statusmsg_spell')+'] '
  {$endif}
  else
    SPrefix:= '';

  SDeleteFromW(S, #10);
  SDeleteFromW(S, #13);
  StatusItemHint.Caption:= SPrefix+S;

  TimerHint.Enabled:= false;
  TimerHint.Enabled:= true;
end;

procedure TfmMain.TBXItemBarWordPrevClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smFindCurrentWordPrior);
end;

procedure TfmMain.TBXItemBarWordNextClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smFindCurrentWordNext);
end;

procedure TfmMain.TBXItemBarFNextClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smFindNext);
end;

procedure TfmMain.TBXItemBarFPrevClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smFindPrev);
end;

procedure TfmMain.TimerRedrawTimer(Sender: TObject);
begin
  TimerRedraw.Enabled:= False;
  FEnableRepaint:= True;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  Action:= caFree;
  UpdateRecentsOnClose;

  //was in FormCloseConfirm before
  for i:= 0 to FrameAllCount-1 do
    SaveFrameState(FramesAll[i]);

  //save ini
  if not QuickView then
  try
    SaveOptionsRecent;
    SaveConsoleHist;
  except
    MsgError(DKLangConstW('zMCannotSaveIni'), Handle);
  end;

  //close Spell dialog
  {$ifdef SPELL}
  if Assigned(FSpell) and Assigned(FSpell.DialogForm) then
    FSpell.DialogForm.Close;
  {$endif}

  //unset clip hook
  if Assigned(fmClip) then
    fmClip.Close;
  //clear find results
  DoClearTreeFind;
  //close plugins
  DoPlugins_Close;

  //close proj and proj-preview
  FProjectFreeing:= true;
  ProjPreviewClose(Self);
  if Assigned(fmProj) then
  begin
    ProjClose(Self);
    fmProj.Close;
  end;
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(fmProgress) and fmProgress.Visible then
  begin
    MsgBeep;
    CanClose:= false;
  end
  else
    CanClose:=
      DoConfirmClose and
      DoConfirmSaveSession(true, true);
end;

procedure TfmMain.WMCommandAny(var Msg: TMessage);
begin
  if CurrentEditor<>nil then
    CurrentEditor.ExecCommand(Msg.wParam);
end;

procedure TfmMain.bBk0Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smGotoBookmark0 + (Sender as TComponent).Tag);
  EditorCenterPos(CurrentEditor, true{GotoMode}, opFindOffsetTop);
end;

procedure TfmMain.TBXSubmenuItemBkGotoPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  UpdateBookmarkMenus;
end;


procedure TfmMain.UpdateBookmarkMenus;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  with Ed do
  begin
    TbxItemB0.Enabled:= Bookmarks[0] <> -1;
    TbxItemB1.Enabled:= Bookmarks[1] <> -1;
    TbxItemB2.Enabled:= Bookmarks[2] <> -1;
    TbxItemB3.Enabled:= Bookmarks[3] <> -1;
    TbxItemB4.Enabled:= Bookmarks[4] <> -1;
    TbxItemB5.Enabled:= Bookmarks[5] <> -1;
    TbxItemB6.Enabled:= Bookmarks[6] <> -1;
    TbxItemB7.Enabled:= Bookmarks[7] <> -1;
    TbxItemB8.Enabled:= Bookmarks[8] <> -1;
    TbxItemB9.Enabled:= Bookmarks[9] <> -1;

    TbxItemB0.Caption:= EditorGetBookmarkDesc(Ed, 0);
    TbxItemB1.Caption:= EditorGetBookmarkDesc(Ed, 1);
    TbxItemB2.Caption:= EditorGetBookmarkDesc(Ed, 2);
    TbxItemB3.Caption:= EditorGetBookmarkDesc(Ed, 3);
    TbxItemB4.Caption:= EditorGetBookmarkDesc(Ed, 4);
    TbxItemB5.Caption:= EditorGetBookmarkDesc(Ed, 5);
    TbxItemB6.Caption:= EditorGetBookmarkDesc(Ed, 6);
    TbxItemB7.Caption:= EditorGetBookmarkDesc(Ed, 7);
    TbxItemB8.Caption:= EditorGetBookmarkDesc(Ed, 8);
    TbxItemB9.Caption:= EditorGetBookmarkDesc(Ed, 9);

    TbxItemG0.Checked:= Bookmarks[0] <> -1;
    TbxItemG1.Checked:= Bookmarks[1] <> -1;
    TbxItemG2.Checked:= Bookmarks[2] <> -1;
    TbxItemG3.Checked:= Bookmarks[3] <> -1;
    TbxItemG4.Checked:= Bookmarks[4] <> -1;
    TbxItemG5.Checked:= Bookmarks[5] <> -1;
    TbxItemG6.Checked:= Bookmarks[6] <> -1;
    TbxItemG7.Checked:= Bookmarks[7] <> -1;
    TbxItemG8.Checked:= Bookmarks[8] <> -1;
    TbxItemG9.Checked:= Bookmarks[9] <> -1;
  end;
end;

procedure TfmMain.bbg0Click(Sender: TObject);
var
  N: Integer;
begin
  N:= (Sender as TComponent).Tag;
  CurrentEditor.ExecCommand(smSetBookmark0 + N);
end;

procedure TfmMain.TBXSubmenuItemBkSetPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  UpdateBookmarkMenus;
end;


procedure TfmMain.ODShow(Sender: TObject);
begin
  DoCenterForm(OD.Handle, Self);
end;

procedure TfmMain.SDShow(Sender: TObject);
begin
  DoCenterForm(SD.Handle, Self);
end;

procedure TfmMain.acNewWindowExecute(Sender: TObject);
begin
  if SynExe then
    FExecute(Application.ExeName, '', '', 0);
end;

procedure TfmMain.TBXItemFExitClick(Sender: TObject);
begin
  if DoCloseTabs(tabCloseAll, false) then
    acExit.Execute;
end;

procedure TfmMain.ecCharPopupChange(Sender: TObject);
begin
  with ecCharPopup do
    ToolHint.Text:= WideFormat(DKLangConstW('MSel'),
      [Byte(SelChar), Byte(SelChar)]);
end;

procedure TfmMain.ecCharPopupShow(Sender: TObject);
var
  Ed: TSyntaxMemo;
  ch: WideChar;
  s: string;
begin
  Ed:= CurrentEditor;
  with ecCharPopup do
  begin
    Font:= Ed.Font;
    if Ed.TextLength>0 then
      if Ed.Lines.TextCoding=tcAnsi then
      begin
        ch:= Ed.Lines.Chars[Ed.CaretStrPos+1];
        s:= UnicodeToAnsiCP(ch, CurrentFrame.EditorMaster.TextSource.Lines.Codepage);
        if s<>'' then SelChar:= s[1];
      end
      else
      begin
        ch:= Ed.Lines.Chars[Ed.CaretStrPos+1];
        SelChar:= Char(ch);
      end;
  end;
end;

procedure TfmMain.ecACPShow(Sender: TObject);
begin
  //if only single item matches the text, then insert this item w/o dialog
  if opAcpUseSingle then
    with ecACP do
      if ListBox.Items.Count=1 then
      begin
        CloseUp(true);
        Exit
      end;

  //CSS: insert space if caret is after ":" char
  if FAcpCss then
    with CurrentEditor do
      if Lines.Chars[CaretStrPos]=':' then
      begin
        InsertText(' ');
        DoAcpPopup;
      end;

  //Work-around for small problem:
  //when ACP called with caret on ID, this ID replaced then with the chosed ID.
  //Need to insert some char before text ID.
  with CurrentEditor do
    if (CaretStrPos<TextLength) then
      if IsWordChar(Lines.Chars[CaretStrPos+1]) then
      begin
        InsertText(' ');
        CaretStrPos:= CaretStrPos-1;
        DoAcpPopup;
        FAcpHtmSpaceAdded:= true;
      end;
end;

procedure TfmMain.FrameSaveState(Sender: TObject);
begin
  SaveFrameState(Sender as TEditorFrame);
end;

procedure TfmMain.SaveLexLibFilename;
var
  fn: string;
begin
  fn:= SyntaxManager.FileName;
  if UpperCase(ExtractFileDir(fn)) = UpperCase(ExtractFileDir(SynLexLib)) then
    fn:= ExtractFileName(fn);

  with TIniFile.Create(SynIni) do
  try
    WriteString('Setup', 'LexLib', fn);
  finally
    Free
  end;
end;

  //1st shortcut for command
  function TfmMain.GetShortcutOfCmd(id: integer): TShortcut;
  var
    c: TecCommandItem;
  begin
    Result:= 0;
    c:= SyntKeyMapping.CommandByID(id);
    if Assigned(c) and
      (c.KeyStrokes.Count>0) and
      (c.KeyStrokes[0].KeyDefs.Count>0) then
      Result:= c.KeyStrokes[0].KeyDefs[0].Shortcut;
  end;

  //text representation of command's shortcut
  function TfmMain.GetShortcutTextOfCmd(id: integer): string;
  var
    c: TecCommandItem;
  begin
    c:= SyntKeyMapping.CommandByID(id);
    if Assigned(c) and (c.KeyStrokes.Count>0) then
      Result:= c.KeyStrokes[0].AsString
    else
      Result:= '';
  end;

  //is shortcut assigned to cmd (1st or 2nd shortcut)
  function TfmMain.IsShortcutOfCmd(sh: TShortcut; cmd: integer): boolean;
  var
    c: TecCommandItem;
  begin
    Result:= false;
    if sh=0 then Exit;
    c:= SyntKeyMapping.CommandByID(cmd);
    if Assigned(c) then
      Result:=
        ((c.KeyStrokes.Count>0) and
         (c.KeyStrokes[0].KeyDefs.Count = 1) and
         (c.KeyStrokes[0].KeyDefs[0].Shortcut = sh)) or
        ((c.KeyStrokes.Count>1) and
         (c.KeyStrokes[1].KeyDefs.Count = 1) and
         (c.KeyStrokes[1].KeyDefs[0].Shortcut = sh));
  end;

  function TfmMain.DoMacro_GetCommandId(n: Integer): Integer;
  begin
    if n<9 then
      Result:= sm_Macro1+n
    else
      Result:= sm_Macro10+n-9;
  end;

  function TfmMain.DoMacro_GetHotkey(n: integer): TKeyStroke;
  var
    id: Integer;
    cmd: TecCommandItem;
  begin
    id:= DoMacro_GetCommandId(n-1);
    cmd:= SyntKeyMapping.CommandByID(id);
    if cmd.KeyStrokes.Count>0 then
    begin
      Result:= TKeyStroke.Create(nil);
      Result.Assign(cmd.KeyStrokes[0]);
    end
    else
      Result:= nil;
  end;

  procedure TfmMain.DoMacro_SetHotkey(n: integer; AKey: TKeyStroke);
  var
    id: Integer;
    cmd: TecCommandItem;
  begin
    id:= DoMacro_GetCommandId(n-1);
    cmd:= SyntKeyMapping.CommandByID(id);
    if Assigned(cmd) then
    begin
      cmd.KeyStrokes.Clear;
      if AKey<>nil then
        cmd.KeyStrokes.Add.Assign(AKey);
    end;
  end;

//set menuitem caption, so that it shows "shortcut" text after a tab char
procedure TfmMain.UpdKey_String(Item: TSpTbxItem; const Cmd: Widestring);
var
  S: Widestring;
begin
  S:= Item.Caption;
  SDeleteFromW(S, #9);
  if Cmd<>'' then
    S:= S + #9 + Cmd;
  Item.Caption:= S;
end;

//set menuitem caption, using Command id
procedure TfmMain.UpdKey(Item: TSpTbxItem; CmdId: integer);
begin
  UpdKey_String(Item, GetShortcutTextOfCmd(CmdId));
end;

procedure TfmMain.UpdateShortcuts;
begin
  plTree.Options.CloseButton.Hint:= GetShortcutTextOfCmd(sm_OptShowLeftPanel);
  plClip.Options.CloseButton.Hint:= GetShortcutTextOfCmd(sm_OptShowRightPanel);
  plOut.Options.CloseButton.Hint:= GetShortcutTextOfCmd(sm_OptShowOutputPanel);

  UpdKey(TBXItemHelpTopics, sm_HelpFileContents);
  UpdKey(TbxItemRunSnippets, sm_SnippetsDialog);
  UpdKey(TbxItemRunNewSnippet, sm_NewSnippetDialog);

  UpdKey(TBXItemONPrintSpaces, sm_OptNonPrintSpaces);
  UpdKey(TBXItemONPrintEol, sm_OptNonPrintEol);
  UpdKey(TBXItemONPrintAll, sm_OptNonPrintBoth);
  UpdKey(TBXItemONPrintEolDetails, sm_OptNonPrintEolDetails);

  UpdKey(TBXItemEExtractDupCase, sm_ExtractDupsCase);
  UpdKey(TBXItemEExtractDupNoCase, sm_ExtractDupsNoCase);
  UpdKey(TBXItemEReverse, sm_ReverseLines);
  UpdKey(TBXItemEShuffle, sm_ShuffleLines);
  UpdKey(TBXItemSSelExtend, sm_SelectionExtend);

  //multi-carets
  UpdKey(TbxItemCaretsRemove1, sm_CaretsRemoveLeaveFirst);
  UpdKey(TbxItemCaretsRemove2, sm_CaretsRemoveLeaveLast);
  UpdKey(TbxItemCaretsFromSelLeft, sm_CaretsFromSelLeft);
  UpdKey(TbxItemCaretsFromSelRight, sm_CaretsFromSelRight);
  UpdKey(TbxItemCaretsFromSelClear, sm_CaretsFromSelClear);
  UpdKey(TbxItemCaretsFromMarksLeft, sm_CaretsFromMarksLeft);
  UpdKey(TbxItemCaretsFromMarksRight, sm_CaretsFromMarksRight);
  UpdKey(TbxItemCaretsFromMarksClear, sm_CaretsFromMarksClear);
  UpdKey(TbxItemCaretsExtUpLine, sm_CaretsExtendUpLine);
  UpdKey(TbxItemCaretsExtDownLine, sm_CaretsExtendDownLine);
  UpdKey(TbxItemCaretsExtUpPage, sm_CaretsExtendUpPage);
  UpdKey(TbxItemCaretsExtDownPage, sm_CaretsExtendDownPage);
  UpdKey(TbxItemCaretsExtUpEnd, sm_CaretsExtendUpEnd);
  UpdKey(TbxItemCaretsExtDownEnd, sm_CaretsExtendDownEnd);

  //folding
  UpdKey(TbxItemFoldAll, smFullCollapse);
  UpdKey(TbxItemUnFoldAll, smFullExpand);
  UpdKey(TbxItemFoldNearestBlock, smToggleCollapseNearest);
  UpdKey(TbxItemFoldSelBlock, smCollapseSelection);
  UpdKey(TbxItemFoldRangesInSel, smInSelCollapse);
  UpdKey(TbxItemUnFoldRangesInSel, smInSelExpand);

  //win
  UpdKey(TbxItemWinTree, sm_ToggleFocusTree);
  UpdKey(TbxItemWinClip, sm_ToggleFocusClip);
  UpdKey(TbxItemWinClips, sm_ToggleFocusClips);
  UpdKey(TbxItemWinOut, sm_ToggleFocusOutput);
  UpdKey(TbxItemWinFRes, sm_ToggleFocusFindRes);
  UpdKey(TbxItemWinVal, sm_ToggleFocusValidate);
  UpdKey(TbxItemWinMap, sm_ToggleFocusMap);
  UpdKey(TbxItemWinProj, sm_ToggleFocusProj);
  UpdKey(TbxItemWinTabs, sm_ToggleFocusTabs);
  UpdKey(TbxItemWinConsole, sm_ToggleFocusConsole);
  UpdKey(TbxItemWinBkmk, sm_ToggleFocusBookmarks);

  //sort
  UpdKey(TBXItemEDedupAll, sm_RemoveDupsAll);
  UpdKey(TBXItemEDedupAdjacent, sm_RemoveDupsAdjacent);
  UpdKey(TbxItemESortDialog, sm_SortDialog);
  UpdKey(tbxItemESortAsc, smSortAscending);
  UpdKey(tbxItemESortDesc, smSortDescending);

  //blank ops
  UpdKey(TBXItemEAlignWithSep, sm_AlignWithSeparator);
  UpdKey(TbxItemECenterLines, sm_CenterLines);
  UpdKey(TbxItemETabToSp, sm_ConvertTabsToSpaces);
  UpdKey(TbxItemESpToTab, sm_ConvertSpacesToTabsAll);
  UpdKey(TBXItemERemDupSp, sm_RemoveDupSpaces);
  UpdKey(TbxItemERemBlanks, sm_RemoveBlanks);
  UpdKey(TbxItemEReduceBlanks, sm_ReduceBlanks);
  UpdKey(TbxItemETrimLead, sm_TrimLeading);
  UpdKey(TbxItemETrimTrail, sm_TrimTrailing);
  UpdKey(TbxItemETrimAll, sm_TrimAll);
  UpdKey(TbxItemEIndentLike1st, sm_IndentLike1st);

  UpdKey(TbxItemMarkClear, sm_MarkersClear);
  UpdKey(TbxItemMarkGoLast, sm_JumpToLastMarker);

  UpdKey(TbxItemHelpCommandList, sm_CommandsList);
  UpdKey(TbxItemERepeatCmd, sm_RepeatLastCommand);
  UpdKey(TbxItemSSelToken, sm_SelectToken);

  //macro
  UpdKey(TbxItemMacroRepeat, sm_MacroRepeat);
  UpdKey(TbxItemMacroRecord, smMacroRecStart);
  UpdKey(TbxItemMacroStop, smMacroRecStop);
  UpdKey(TbxItemMacroCancel, smMacroRecCancel);
  UpdKey(TbxItemMacroPlay, smMacroPlay);
  UpdKey(TbxItemMacroDlg, sm_MacrosDialog);

  //view
  UpdKey(TbxItemVSyncHorz, sm_SyncScrollHorz);
  UpdKey(TbxItemVSyncVert, sm_SyncScrollVert);
  UpdKey(TbxItemVSpellLive, sm_SpellLive);
  UpdKey(TbxItemVSpellCheck, sm_SpellCheck);

  //edit
  UpdKey(TbxItemEJoin, sm_JoinLines);
  UpdKey(TbxItemESplit, sm_SplitLines);
  UpdKey(TbxItemECpFN, sm_CopyFilename);
  UpdKey(TbxItemECpFullPath, sm_CopyFullPath);
  UpdKey(TbxItemECpDirPath, sm_CopyDirPath);
  UpdKey(TbxItemEMoveUp, smMoveLinesUp);
  UpdKey(TbxItemEMoveDn, smMoveLinesDown);
  UpdKey(TbxItemEToggleStreamComment, sm_ToggleStreamComment);
  UpdKey(TbxItemEToggleLineComment, sm_ToggleLineComment);
  UpdKey(TbxItemEToggleLineCommentAlt, sm_ToggleLineCommentAlt);

  UpdKey(TBXItemEInsText, sm_InsertTextDialog);
  UpdKey(TBXItemEFillBlock, sm_FillBlockDialog);
  UpdKey(TBXItemOOnTop, sm_ShowOnTop);
  UpdKey(TBXItemOFullScr, sm_ShowFullScreen);

  UpdKey(TBXItemESyncEd, sm_ToggleSyncEditing);
  UpdKey(TbxItemEExtr, sm_ExtractTextDialog);
  UpdKey(TbxItemETime, sm_InsertDateTime);

  UpdKey(TBXItemFExit, sm_FileExit);
  UpdKey(TBXItemFClearRecents, sm_ClearFilesHistory);
  UpdKey(TbxItemFClose, sm_FileClose);
  UpdKey(TbxItemFCloseDel, sm_FileCloseAndDelete);
  UpdKey(TbxItemFCloseAll, sm_FileCloseAll);
  UpdKey(TbxItemFCloseOth, sm_FileCloseOthers);
  UpdKey(TbxItemFSaveAll, sm_FileSaveAll);
  UpdKey(TbxItemFRename, sm_FileRenameDialog);
  //
  UpdKey(TbxItemSResNext, sm_GotoNextFindResult);
  UpdKey(TbxItemSResPrev, sm_GotoPrevFindResult);

  UpdKey(TBXItemSMarkAll, smFindAll);
  UpdKey(tbxItemSMarkNext, smSearchMarkNext);
  UpdKey(tbxItemSMarkPrev, smSearchMarkPrev);
  UpdKey(tbxItemSMarkClear, smSearchMarkReset);

  UpdKey(TBXItemQs, sm_QuickSearch);
  UpdKey(tbxItemMarkDrop, smDropMarker);
  UpdKey(tbxItemMarkColl, smCollectMarker);
  UpdKey(tbxItemMarkSwap, smSwapMarker);

  UpdKey(TbxItemRunOpenFile, sm_OpenCurrentFile);
  UpdKey(TbxItemRunOpenDir, sm_OpenCurrentFolder);
  UpdKey(TbxItemRunNumConv, sm_NumericConverterDialog);

  //file
  UpdKey(tbxItemFPrint, smPrint);
  UpdKey(tbxItemFPreview, smPrintPreview);
  UpdKey(tbxItemFNew, sm_FileNew);
  UpdKey(tbxItemFNewWin, sm_FileNewWindow);
  UpdKey(tbxItemFOpen, sm_FileOpen);
  UpdKey(tbxItemFReopen, sm_FileReopen);
  UpdKey(tbxItemFSave, sm_FileSave);
  UpdKey(tbxItemFSaveAs, sm_FileSaveAs);

  UpdKey(tbxItemFavAddFile, sm_Fav_AddFile);
  UpdKey(tbxItemFavAddProj, sm_Fav_AddProject);
  UpdKey(tbxItemFavManage, sm_Fav_Organize);

  UpdKey(TBXItemFSesOpen, sm_FileOpenSession);
  UpdKey(TBXItemFSesSave, sm_FileSaveSession);
  UpdKey(TBXItemFSesSaveAs, sm_FileSaveSessionAs);
  UpdKey(TBXItemFSesAdd, sm_FileAddSession);
  UpdKey(TBXItemFSesClose, sm_FileCloseSession);

  UpdKey(tbxItemFExpHtml, sm_FileExportHtml);
  UpdKey(tbxItemFExpRtf, sm_FileExportRtf);
  UpdKey(tbxItemFPageSetup, smPageSetup);
  UpdKey(tbxItemFPrinterSetup, sm_PrinterSetup);

  //edit
  UpdKey(tbxItemEUndo, smUndo);
  UpdKey(tbxItemERedo, smRedo);
  UpdKey(tbxItemECut, smCut);
  UpdKey(tbxItemECopy, smCopy);
  UpdKey(tbxItemEPaste, smPaste);
  UpdKey(tbxItemEDelete, smClearSelection);
  UpdKey(tbxItemESelectAll, smSelectAll);

  UpdKey(tbxItemECopyApp, sm_CopyAppend);
  UpdKey(tbxItemECutApp, sm_CutAppend);
  UpdKey(tbxItemECopyLine, sm_CopyLine);
  UpdKey(tbxItemECutLine, sm_CutLine);

  UpdKey(tbxItemEDup, smDuplicateLine);
  UpdKey(tbxItemEDelLn, smDeleteLine);
  UpdKey(tbxItemETable, smSelCharacter);
  UpdKey(tbxItemEComm, smCommentLines);
  UpdKey(tbxItemEUncomm, smUncommentLines);
  UpdKey(tbxItemEIndent, smBlockIndent);
  UpdKey(tbxItemEUnindent, smBlockUnindent);
  UpdKey(tbxItemECaseUpper, smUpperCaseBlock);
  UpdKey(tbxItemECaseLower, smLowerCaseBlock);
  UpdKey(tbxItemECaseTitle, smTitleCaseBlock);
  UpdKey(tbxItemECaseInvert, smToggleCaseBlock);
  UpdKey(tbxItemECaseSent, sm_SentenceCaseBlock);

  //bookmk
  UpdKey(tbxItemG0, smSetBookmark0);
  UpdKey(tbxItemG1, smSetBookmark1);
  UpdKey(tbxItemG2, smSetBookmark2);
  UpdKey(tbxItemG3, smSetBookmark3);
  UpdKey(tbxItemG4, smSetBookmark4);
  UpdKey(tbxItemG5, smSetBookmark5);
  UpdKey(tbxItemG6, smSetBookmark6);
  UpdKey(tbxItemG7, smSetBookmark7);
  UpdKey(tbxItemG8, smSetBookmark8);
  UpdKey(tbxItemG9, smSetBookmark9);

  UpdKey(tbxItemB0, smGotoBookmark0);
  UpdKey(tbxItemB1, smGotoBookmark1);
  UpdKey(tbxItemB2, smGotoBookmark2);
  UpdKey(tbxItemB3, smGotoBookmark3);
  UpdKey(tbxItemB4, smGotoBookmark4);
  UpdKey(tbxItemB5, smGotoBookmark5);
  UpdKey(tbxItemB6, smGotoBookmark6);
  UpdKey(tbxItemB7, smGotoBookmark7);
  UpdKey(tbxItemB8, smGotoBookmark8);
  UpdKey(tbxItemB9, smGotoBookmark9);

  UpdKey(TBXItemBkGotoPortable, sm_GotoPortableBk);
  UpdKey(TBXItemBkDropPortable, sm_DropPortableBk);
  UpdKey(TbxItemBkGoto, sm_GotoBookmarkDialog);
  UpdKey(tbxItemBkClear, sm_BookmarksClear);
  UpdKey(tbxItemBkToggle, sm_BookmarksToggle);
  UpdKey(tbxItemBkNext, sm_BookmarksNext);
  UpdKey(tbxItemBkPrev, sm_BookmarksPrev);
  UpdKey(tbxItemBkCopy, sm_BookmarksCopy);
  UpdKey(tbxItemBkCut, sm_BookmarksCut);
  UpdKey(tbxItemBkDel, sm_BookmarksDelete);
  UpdKey(tbxItemBkDelUnmk, sm_BookmarksDeleteUnmarked);
  UpdKey(tbxItemBkPaste, sm_BookmarksPaste);
  UpdKey(tbxItemBkInverse, sm_BookmarksInverse);

  //opt
  UpdKey(tbxItemOSetup, sm_OptSetup);
  UpdKey(tbxItemOLexer, sm_OptSetupLexer);
  UpdKey(tbxItemOLexerLib, sm_OptSetupLexerLib);
  //search
  UpdKey(tbxItemSRep, smReplaceDialog);
  UpdKey(tbxItemSRepInFiles, sm_ReplaceInFiles);
  UpdKey(tbxItemSRepInProject, sm_ReplaceInProject);
  UpdKey(tbxItemSFind, smFindDialog);
  UpdKey(tbxItemSNext, smFindNext);
  UpdKey(tbxItemSPrev, smFindPrev);
  UpdKey(tbxItemSWordNext, smFindCurrentWordNext);
  UpdKey(tbxItemSWordPrior, smFindCurrentWordPrior);
  UpdKey(tbxItemSGoto, smGotoLine);
  UpdKey(TBXItemSGoBracket, smChangeRangeSide);
  UpdKey(TbxItemSSelBrackets, sm_SelectBrackets);
  //
  UpdKey(tbxItemORO, sm_OptReadOnly);
  UpdKey(tbxItemOTree, sm_OptShowLeftPanel);
  UpdKey(tbxItemOOut, sm_OptShowOutputPanel);
  UpdKey(tbxItemOClip, sm_OptShowRightPanel);
  UpdKey(tbxItemOFold, sm_OptFolding);
  UpdKey(tbxItemOWrap, sm_OptWrap);
  UpdKey(tbxItemONums, sm_OptLineNums);
  UpdKey(tbxItemONPrint, sm_OptNonPrint);
  UpdKey(tbxItemORuler, sm_OptRuler);

  //=========popup menus
  //main popup
  UpdKey(TbxItemCtxUndo, smUndo);
  UpdKey(TbxItemCtxRedo, smRedo);
  UpdKey(TbxItemCtxCut, smCut);
  UpdKey(TbxItemCtxCopy, smCopy);
  UpdKey(TbxItemCtxPaste, smPaste);
  UpdKey(TbxItemCtxDel, smClearSelection);
  UpdKey(TbxItemCtxSelectAll, smSelectAll);
  UpdKey(TbxItemCtxCustomize, sm_OptSetup);
  UpdKey(TbxItemCtxCopyAppend, sm_CopyAppend);
  UpdKey(TbxItemCtxCutAppend, sm_CutAppend);
  UpdKey(TbxItemCtxCopyHTML, sm_CopyAsHTML);
  UpdKey(TbxItemCtxCopyRTF, sm_CopyAsRTF);
  UpdKey(TbxItemCtxPasteNoCurChange, sm_PasteNoCursorChange);
  UpdKey(TBXItemCtxPasteToColumn1, sm_PasteToColumn1);
  UpdKey(TBXItemCtxPasteAsColumn, sm_PasteAsColumnBlock);
  UpdKey(TBXItemCtxPasteBkmkLines, sm_BookmarksPaste);

  //clip popup menu
  UpdKey(TBXItemClipFind, smFindDialog);
  UpdKey_String(TBXItemClipCopyToEd, 'Enter');
  UpdKey_String(TBXItemClipCopyToClip, 'Ctrl+C');
  UpdKey_String(TBXItemClipDeleteSel, 'Delete');
  UpdKey_String(TBXItemClipDeleteAll, 'Shift+Delete');

  //clips popup menu
  UpdKey_String(TBXItemClipsAddText, 'Insert');

  //output popup menu
  UpdKey_String(TBXItemOutNav, 'Space');
  UpdKey_String(TBXItemOutClear, 'Delete');
  UpdKey(TBXItemOutFind, smFindDialog);

  //find results popup menu
  UpdKey_String(TBXItemTreeFindNav, 'Enter');
  UpdKey_String(TBXItemTreeFindPreview, 'Space');
  UpdKey_String(TBXItemTreeFindCopyToClipNode, 'Ctrl+C');
  UpdKey_String(TBXItemTreeFindClear, 'Delete');
  UpdKey(TBXItemTreeFindFind, smFindDialog);

  //validate popup menu
  UpdKey_String(TBXItemValNav, 'Space');
  //UpdKey_String(TBXItemValCopySel, 'Ctrl+C');
  UpdKey_String(TBXItemValClear, 'Delete');
  UpdKey(TBXItemValFind, smFindDialog);

  //tree popup menu
  UpdKey(TBXItemTreeFind, smFindDialog);
end;

procedure TfmMain.TBXItemHelpReadmeDirClick(Sender: TObject);
begin
  FOpenURL(SynDir + 'Readme', Handle);
end;

procedure TfmMain.TBXItemHelpTopicsClick(Sender: TObject);
begin
  FOpenURL(FHelpFilename, Handle);
end;

procedure TfmMain.TBXSubmenuEncRereadPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  UpdateEncMenu(TBXSubmenuEncReread);
end;

procedure TfmMain.TBXItemSMarkNextClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSearchMarkNext);
end;

procedure TfmMain.TBXItemSMarkPrevClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSearchMarkPrev);
end;

procedure TfmMain.TBXItemETableClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSelCharacter);
end;

procedure TfmMain.TBXSubmenuLexersPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  UpdateLexList;
  TbxSubmenuLexers.LinkSubitems:= PopupLexers.Items;
end;

procedure TfmMain.TBXItemEDupClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smDuplicateLine);
end;

function TfmMain.GetTheme: string;
begin
  Result:= FCurrTheme;
end;

procedure TfmMain.SetTheme(const S: string);
begin
  FCurrTheme:= S;
  if S=cThemeWindows then
    SkinManager.SetToDefaultSkin
  else
  if Pos('*', S)=1 then
  begin
    SkinManager.SetToDefaultSkin;
    SkinManager.LoadFromFile(SynSkinFilename(S));
  end
  else
    SkinManager.SetSkin(S);
end;

procedure TfmMain.LoadTools;
begin
  DoTool_LoadList(opTools, SynIni, 'Tool');
  UpdateTools;
end;

procedure TfmMain.DoTool_Update(T: TSpTbxItem; Id: integer; ACtxMenu: boolean);
var
  Frame: TEditorFrame;
  An: TSyntAnalyzer;
begin
  Frame:= CurrentFrame;
  if Frame=nil then Exit;
  An:= Frame.EditorMaster.TextSource.SyntaxAnalyzer;

  with opTools[Id] do
   if not ACtxMenu or ToolContextItem then
   begin
     T.Tag:= Id;
     T.Caption:= ToolCaption;
     T.Enabled:= (ToolCaption<>'') and (ToolCommand<>'') and
       ((ToolLexer='') or ((An<>nil) and (An.LexerName=ToolLexer)));
     if T.Enabled and not ACtxMenu then
       T.ShortCut:= TextToShortcut(ToolKeys)
     else
       T.ShortCut:= 0;
     T.Visible:= T.Enabled;
     T.Hint:= ToolCommand;
     T.OnClick:= DoTool_OnClick;
     T.OnSelect:= ButtonOnSelect;
   end
   else
   begin
     T.Visible:= false;
     T.ShortCut:= 0;
   end;
end;

procedure TfmMain.UpdateTools;
begin
  DoTool_Update(TbxItemTool1, 1, false);
  DoTool_Update(TbxItemTool2, 2, false);
  DoTool_Update(TbxItemTool3, 3, false);
  DoTool_Update(TbxItemTool4, 4, false);
  DoTool_Update(TbxItemTool5, 5, false);
  DoTool_Update(TbxItemTool6, 6, false);
  DoTool_Update(TbxItemTool7, 7, false);
  DoTool_Update(TbxItemTool8, 8, false);
  DoTool_Update(TbxItemTool9, 9, false);
  DoTool_Update(TbxItemTool10, 10, false);
  DoTool_Update(TbxItemTool11, 11, false);
  DoTool_Update(TbxItemTool12, 12, false);
  DoTool_Update(TbxItemTool13, 13, false);
  DoTool_Update(TbxItemTool14, 14, false);
  DoTool_Update(TbxItemTool15, 15, false);
  DoTool_Update(TbxItemTool16, 16, false);

  DoTool_Update(TbxItemCtxTool1, 1, true);
  DoTool_Update(TbxItemCtxTool2, 2, true);
  DoTool_Update(TbxItemCtxTool3, 3, true);
  DoTool_Update(TbxItemCtxTool4, 4, true);
  DoTool_Update(TbxItemCtxTool5, 5, true);
  DoTool_Update(TbxItemCtxTool6, 6, true);
  DoTool_Update(TbxItemCtxTool7, 7, true);
  DoTool_Update(TbxItemCtxTool8, 8, true);
  DoTool_Update(TbxItemCtxTool9, 9, true);
  DoTool_Update(TbxItemCtxTool10, 10, true);
  DoTool_Update(TbxItemCtxTool11, 11, true);
  DoTool_Update(TbxItemCtxTool12, 12, true);
  DoTool_Update(TbxItemCtxTool13, 13, true);
  DoTool_Update(TbxItemCtxTool14, 14, true);
  DoTool_Update(TbxItemCtxTool15, 15, true);
  DoTool_Update(TbxItemCtxTool16, 16, true);
end;

procedure TfmMain.SaveTools;
begin
  DoTool_SaveList(opTools, SynIni, 'Tool');
  UpdateTools;
end;

procedure TfmMain.TBXItemOToolsClick(Sender: TObject);
begin
  DoConfigTools;
end;

procedure TfmMain.DoConfigTools;
var
  L: TTntStringList;
begin
  L:= TTntStringList.Create;
  try
    L.Sorted:= true;
    L.Duplicates:= dupIgnore;
    DoEnumLexers(L);

    if DoTool_ConfigList(opTools, Self, L, true,
      CurrentFrame.CurrentLexer,
      SynDataSubdir(cSynDataOutPresets),
      '') then
    begin
      Application.ProcessMessages;
      SaveTools;
      ListOut.Invalidate;
    end;
  finally
    FreeAndNil(L);
  end;
end;

procedure TfmMain.TimerHintTimer(Sender: TObject);
begin
  TimerHint.Enabled:= false;
  StatusItemHint.Caption:= '';
end;

procedure TfmMain.DKLanguageController1LanguageChanged(Sender: TObject);
begin
  UpdateLang;
end;

procedure TfmMain.UpdateLang;
begin
  UpdateTitle(CurrentFrame);
  UpdateStatusBar;

  if SyntaxManager.CurrentLexer = nil then
    StatusItemLexer.Caption:= DKLangConstW('None');

  //Project items:
  SMsgProjNew:= DKLangConstW('unnamed');

  //TBX toolbars:
  tbQS.ChevronHint:= DKLangConstW('MoreB');
  tbFile.ChevronHint:= tbQS.ChevronHint;
  tbEdit.ChevronHint:= tbQS.ChevronHint;
  tbView.ChevronHint:= tbQS.ChevronHint;
  //tbTabsLeft.ChevronHint:= tbQS.ChevronHint;
  //tbTabsRight.ChevronHint:= tbQS.ChevronHint;
  //tbTabsOut.ChevronHint:= tbQS.ChevronHint;
  tbUser1.ChevronHint:= tbQS.ChevronHint;
  tbUser2.ChevronHint:= tbQS.ChevronHint;
  tbUser3.ChevronHint:= tbQS.ChevronHint;
  if Assigned(fmProj) then
    fmProj.tbProject.ChevronHint:= tbQS.ChevronHint;

  //Statusbar items:
  cStatLine:=     ' '+DKLangConstW('stat_line')+' ';
  cStatCol:=      ' '+DKLangConstW('stat_col')+' ';
  cStatSelLines:= ' '+DKLangConstW('stat_sellines')+' ';
  cStatSelCols:=  ' '+DKLangConstW('stat_selcols')+' ';
  cStatSelChars:= ' '+DKLangConstW('stat_selchars')+' ';
  cStatTLines:=   ' '+DKLangConstW('stat_tlines')+' ';
  cStatTChars:=   ' '+DKLangConstW('stat_tchars')+' ';
  cStatFSize:=    ' '+DKLangConstW('stat_fsize')+' ';
  cStatFDate:=    ' '+DKLangConstW('stat_fdate')+' ';
  cStatCarets:=   ' '+DKLangConstW('stat_carets')+' ';
  cStatCaretsTopLn:= ' '+DKLangConstW('stat_carets_top')+' ';
  cStatCaretsBotLn:= ' '+DKLangConstW('stat_carets_btm')+' ';

  FUpdatePluginsLang:= true;
end;

procedure TfmMain.UpdateSpellLang;
begin
  {$ifdef SPELL}
  if Assigned(FSpell) then
    case LangManager.LanguageID of
      1029: FSpell.UILanguage:= ltCzech;
      1030: FSpell.UILanguage:= ltDanish;
      1031: FSpell.UILanguage:= ltGerman;
      1036: FSpell.UILanguage:= ltFrench;
      1040: FSpell.UILanguage:= ltItalian;
      1043: FSpell.UILanguage:= ltDutch;
      1045: FSpell.UILanguage:= ltPolish;
      1046: FSpell.UILanguage:= ltBrPort;
      1049: FSpell.UILanguage:= ltRussian;
      1053: FSpell.UILanguage:= ltSwedish;
      3082: FSpell.UILanguage:= ltSpanish;
      else FSpell.UILanguage:= ltEnglish;
    end;
  {$endif}
end;

procedure TfmMain.ecACPCloseUp(Sender: TObject; var Accept: Boolean);
begin
  if not Accept then
  begin
    //delete space if added on ACP call
    if FAcpHtm and FAcpHtmSpaceAdded then
      with CurrentEditor do
        if Lines.Chars[CaretStrPos+1] = ' ' then
          DeleteText(1);
  end;

  FAcpHtmSpaceAdded:= false;
end;


procedure TfmMain.DoTool_ReplaceMacro(var Str: Widestring; const StrId: string; ViewId: TSynGroupId);
  //
  function SMacro(const MacroName: string): string;
  begin
    Result:= '{'+MacroName+StrId+'}';
  end;
var
  fn: Widestring;
begin
  fn:= CurrentFileName(ViewId);
  SReplaceW(Str, SMacro('FileName'), fn);
  SReplaceW(Str, SMacro('FileNameOnly'), WideExtractFileName(fn));
  SReplaceW(Str, SMacro('FileNameNoExt'), WideChangeFileExt(WideExtractFileName(fn), ''));
  SReplaceW(Str, SMacro('FileDir'), WideExtractFileDir(fn));
  SReplaceW(Str, SMacro('FileExt'), Copy(WideExtractFileExt(fn), 2, MaxInt));
end;

procedure TfmMain.DoTool_ReplaceFolderMacros(var S: Widestring);
begin
  SReplaceW(S, '{FileDir}', WideExtractFileDir(CurrentFrame.FileName));
  SReplaceW(S, '{SynDir}', ExtractFileDir(SynDir));
  SReplaceW(S, '{SynIniDir}', ExtractFileDir(SynIni));
  SReplaceW(S, '{SynDrive}', ExtractFileDrive(SynDir));
  SReplaceW(S, '{ProjectDir}', CurrentProjectDir);
  SReplaceW(S, '{ProjectWorkDir}', CurrentProjectWorkDir);
  SReplaceW(S, '{ProjectMainFileDir}', WideExtractFileDir(CurrentProjectMainFN));
end;


procedure TfmMain.DoTool_ReplaceAllMacros(var S: Widestring; const Dir: WideString);
var
  fn, SValue: Widestring;
  p: TPoint;
begin
  p:= CurrentEditor.CaretPos;
  //
  DoTool_ReplaceMacro(S, '', cSynGroupCurrent);
  DoTool_ReplaceMacro(S, '2', cSynGroupOpposite);
  DoTool_ReplaceMacro(S, 'N1', cSynGroup1);
  DoTool_ReplaceMacro(S, 'N2', cSynGroup2);
  DoTool_ReplaceMacro(S, 'N3', cSynGroup3);
  DoTool_ReplaceMacro(S, 'N4', cSynGroup4);
  DoTool_ReplaceMacro(S, 'N5', cSynGroup5);
  DoTool_ReplaceMacro(S, 'N6', cSynGroup6);
  //
  SReplaceW(S, '{ProjectDir}', CurrentProjectDir);
  SReplaceW(S, '{ProjectWorkDir}', CurrentProjectWorkDir);
  SReplaceW(S, '{ProjectMainFileName}', CurrentProjectMainFN);
  SReplaceW(S, '{ProjectMainFileDir}', WideExtractFileDir(CurrentProjectMainFN));
  //
  SReplaceW(S, '{CurrentWord}', CurrentEditor.WordAtPos(p));
  SReplaceW(S, '{CurrentLineNum}', IntToStr(p.Y+1));
  SReplaceW(S, '{CurrentColumnNum}', IntToStr(p.X+1));
  if (p.Y >= 0) and (p.Y < CurrentEditor.Lines.Count) then
    SReplaceW(S, '{CurrentLine}', CurrentEditor.Lines[p.Y]);
  //
  SValue:= SReplaceAllEols(CurrentEditor.SelText, ' ');
  SReplaceW(S, '{SelectedText}', SValue);

  if Pos('{SelectionFileName}', S)>0 then
    SReplaceW(S, '{SelectionFileName}', CurrentSelectionFN(true, false));
  if Pos('{SelectionFileNameAnsi}', S)>0 then
    SReplaceW(S, '{SelectionFileNameAnsi}', CurrentSelectionFN(false, false));

  if Pos('{SelectionFileNameNum}', S)>0 then
    SReplaceW(S, '{SelectionFileNameNum}', CurrentSelectionFN(true, true));
  if Pos('{SelectionFileNameAnsiNum}', S)>0 then
    SReplaceW(S, '{SelectionFileNameAnsiNum}', CurrentSelectionFN(false, true));

  if Pos('{SelectedTextForWeb}', S)>0 then
    SReplaceW(S, '{SelectedTextForWeb}', EditorSelectedTextForWeb(CurrentEditor));
  //
  if Pos('{ContentFileName}', S)>0 then
    SReplaceW(S, '{ContentFileName}', CurrentContentFN(true));
  if Pos('{ContentFileNameAnsi}', S)>0 then
    SReplaceW(S, '{ContentFileNameAnsi}', CurrentContentFN(false));
  //
  SReplaceW(S, '{SynDir}', ExtractFileDir(SynDir));
  SReplaceW(S, '{SynIniDir}', ExtractFileDir(SynIni));
  SReplaceW(S, '{SynDrive}', ExtractFileDrive(SynDir));
  //
  while Pos('{Interactive}', S)>0 do
  begin
    fn:= '';
    if not DoInputString(DKLangConstW('cmdInt'), fn, SynHistoryIni, 'ExtToolParam') then
      raise Exception.Create('Param input cancelled');
    SReplaceW(S, '{Interactive}', fn);
  end;
  //
  while Pos('{InteractiveFile}', S)>0 do
  begin
    fn:= '';
    if not WidePromptForFileName(fn, '', '',
      DKLangConstW('cmdIFile'), dir) then
      raise Exception.Create('Filename input cancelled');
    SReplaceW(S, '{InteractiveFile}', fn);
  end;
  //
  while Pos('{InteractiveDir}', S)>0 do
  begin
    fn:= dir;
    if not WideSelectDirectory(
      DKLangConstW('cmdIDir'), '', fn) then
      raise Exception.Create('Dir name input cancelled');
    SReplaceW(S, '{InteractiveDir}', fn);
  end;
  //
  //user variables (from project)
  if Assigned(fmProj) then
    fmProj.ReplaceUserVars(S, '', SValue);
end;

procedure TfmMain.DoTool_Run(const ATool: TSynTool);
  //
var
  ft, fcmd, fpar, frun, fexe, fdir,
  SCurWord: Widestring;
begin
  if CurrentFrame=nil then Exit;
  with ATool do
  begin
    //check correctness of tool params
    if (Pos('{File', ToolParams)>0) and (CurrentFrame.FileName='') then
      begin MsgWarn(DKLangConstW('NSaved'), Handle); Exit end;

    if (Pos('{Select', ToolParams)>0) and not CurrentEditor.HaveSelection then
      begin MsgNoSelection; Exit end;

    if (Pos('{ProjectWorkDir}', ToolParams)>0) and (CurrentProjectWorkDir='') then
      begin MsgEmptyMacro('{ProjectWorkDir}'); Exit end;
    if (Pos('{ProjectMainFileName}', ToolParams)>0) and (CurrentProjectMainFN='') then
      begin MsgEmptyMacro('{ProjectMainFileName}'); Exit end;
    if (Pos('{ProjectMainFileDir}', ToolParams)>0) and (CurrentProjectMainFN='') then
      begin MsgEmptyMacro('{ProjectMainFileDir}'); Exit end;

    if ToolCommand = '' then
      begin MsgBeep; Exit end;

    //expand some macros in "File name", "Initial dir" fields
    fexe:= ToolCommand;
    fdir:= ToolDir;
    DoTool_ReplaceFolderMacros(fexe);
    DoTool_ReplaceFolderMacros(fdir);

    //save files if needed
    case ToolSaveMode of
      svCurrent:
        begin
          if CurrentFrame.Modified then
            acSave.Execute;
        end;
      svAll:
        acSaveAll.Execute;
    end;

    //CHM file
    if SFileExtensionMatch(fexe, 'chm') then
    begin
      if not IsFileExist(fexe) then
        begin MsgNoFile(fexe); Exit end;

      if CurrentEditor.SelLength>0 then
        SCurWord:= CurrentEditor.SelText
      else
        SCurWord:= CurrentEditor.WordAtPos(CurrentEditor.CaretPos);
      SDeleteFromW(SCurWord, #13);
      SDeleteFromW(SCurWord, #10);
      if Trim(SCurWord)='' then
        begin MsgNoSelectionForHelp; Exit end;

      frun:= SynDir + 'Tools\HtmlHelpView.exe';
      if not IsFileExist(frun) then
        begin MsgNoFile(frun); Exit end;

      FExecute(frun,
        '"'+fexe+'" "'+SCurWord+'"',
        '', Handle);
      Exit;
    end;

    //HLP file
    if SFileExtensionMatch(fexe, 'hlp') then
    begin
      if not IsFileExist(fexe) then
        begin MsgNoFile(fexe); Exit end;

      if CurrentEditor.SelLength>0 then
        SCurWord:= CurrentEditor.SelText
      else
        SCurWord:= CurrentEditor.WordAtPos(CurrentEditor.CaretPos);
      SDeleteFromW(SCurWord, #13);
      SDeleteFromW(SCurWord, #10);
      if Trim(SCurWord)='' then
        begin MsgNoSelectionForHelp; Exit end;

      Application.HelpSystem.Hook(
        Longint(Handle),
        string(fexe),
        HELP_PARTIALKEY,
        Integer(PChar(string(SCurWord))));
      Exit;
    end;

    if not ToolOutCapture then
    //don't handle output
    begin
      fdir:= SExpandVars(fdir);
      if fdir='' then
        fdir:= SExtractFileDir(CurrentFrame.FileName);

      try
        frun:= SExpandVars(fexe);
        DoTool_ReplaceAllMacros(frun, fdir);
      except
        Exit
      end;

      try
        fpar:= SExpandVars(ToolParams);
        DoTool_ReplaceAllMacros(fpar, fdir);
      except
        Exit
      end;

      if not FExecute(frun, fpar, fdir, Handle) then
        begin MsgNoRun(frun); Exit end;
    end
    else
    //handle output by running cmd.exe
    begin
      ft:= FGetTempFilenameDeleted();
      if IsFileExist(ft) then
        begin MsgError('Cannot delete temp file: '#13+ft, Handle); Exit end;

      fdir:= SExpandVars(fdir);
      if fdir='' then
        fdir:= SExtractFileDir(CurrentFrame.FileName);
      if fdir='' then
        fdir:= FTempDir;

      try
        frun:= SExpandVars(fexe);
        DoTool_ReplaceAllMacros(frun, fdir);
      except
        Exit
      end;

      try
        fpar:= SExpandVars(ToolParams);
        DoTool_ReplaceAllMacros(fpar, fdir);
      except
        Exit
      end;

      fcmd:= WideFormat('cmd.exe /c""%s" %s >"%s" 2>&1"', [frun, fpar, ft]);

      Screen.Cursor:= crHourGlass;
      try
        FDelete(ft);
        if FExecProcess(fcmd, fdir, sw_hide, true{Wait})=exCannotRun then
          begin MsgNoRun(frun); Exit end;
      finally
        Screen.Cursor:= crDefault;
      end;

      DoTool_HandleOutput(ft, ATool);
    end;
  end;
end;

procedure TfmMain.DoTool_OnClick(Sender: TObject);
var
  N: Integer;
begin
  N:= (Sender as TComponent).Tag;
  if (N>=Low(opTools)) and (N<=High(opTools)) then
    DoTool_Run(opTools[N]);
end;

procedure TfmMain.TBXSubmenuToolsPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  TBXItemRunOpenDir.Enabled:= CurrentFrame.Filename <> '';
  TBXItemRunOpenFile.Enabled:= TBXItemRunOpenDir.Enabled;
  UpdateTools;
end;

procedure TfmMain.TBXItemZ0Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ZoomOriginal);
end;

procedure TfmMain.TBXItemMarkDropClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smDropMarker);
end;

procedure TfmMain.TBXItemMarkCollClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smCollectMarker);
end;

procedure TfmMain.TBXItemMarkSwapClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSwapMarker);
end;

procedure TfmMain.TBXItemTFileClick(Sender: TObject);
begin
  with tbFile do
    Visible:= not Visible;
  UpdateStatusBar;
  SaveToolbarsProps;
end;

procedure TfmMain.TBXItemTEditClick(Sender: TObject);
begin
  with tbEdit do
    Visible:= not Visible;
  UpdateStatusBar;
  SaveToolbarsProps;
end;

procedure TfmMain.TBXItemTViewClick(Sender: TObject);
begin
  with tbView do
    Visible:= not Visible;
  UpdateStatusBar;
  SaveToolbarsProps;
end;

procedure TfmMain.TBXItemTQsClick(Sender: TObject);
begin
  with tbQs do
    Visible:= not Visible;
  if not tbQs.Visible then
    FocusEditor;
  UpdateStatusBar;
  SaveToolbarsProps;
end;

procedure TfmMain.SaveToolbarsProps;
var
  Ini: TIniFile;
begin
  if ShowFullScreen then Exit;
  Ini:= TIniFile.Create(SynIni);
  try
    SaveToolbarProp(tbFile, Ini, 'File');
    SaveToolbarProp(tbEdit, Ini, 'Edit');
    SaveToolbarProp(tbView, Ini, 'View');
    SaveToolbarProp(tbQs, Ini, 'Qs');
    SaveToolbarProp(tbUser1, Ini, 'U1');
    SaveToolbarProp(tbUser2, Ini, 'U2');
    SaveToolbarProp(tbUser3, Ini, 'U3');
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfmMain.tbQsClose(Sender: TObject);
begin
  UpdateStatusBar;
  SaveToolbarsProps;
end;

procedure TfmMain.DoQuickSearch(AMode: TSynQuickSearchType);
var
  bBeep, bFound: boolean;
begin
  if edQs.Text<>'' then
  begin
    Finder.FindText:= edQs.Text;
    Finder.Flags:= [ftWrapSearch];
    if cbCase.Checked then
      Finder.Flags:= Finder.Flags + [ftCaseSens];
    if cbWord.Checked then
      Finder.Flags:= Finder.Flags + [ftWholeWords];

    bBeep:= opBeep;
    opBeep:= false;
    try
      case AMode of
        cQsNext: bFound:= Finder.FindNext;
        cQsPrev: bFound:= Finder.FindPrev;
        cQsAgain: bFound:= Finder.FindFirst;
        else bFound:= false;
      end;
    finally
      opBeep:= bBeep;
    end;

    if not bFound then
      edQs.Color:= cColorNotFound
    else
      edQs.Color:= clWindow;
  end
  else
    edQs.Color:= clWindow;
end;

procedure TfmMain.TBXItemFFPrevClick(Sender: TObject);
begin
  DoQuickSearch(cQsPrev);
end;

procedure TfmMain.TBXItemFFNextClick(Sender: TObject);
begin
  DoQuickSearch(cQsNext);
end;

procedure TfmMain.cbCaseClick(Sender: TObject);
begin
  cbCase.Checked:= not cbCase.Checked;
  edQsChange(Self);
end;

procedure TfmMain.edQsChange(Sender: TObject);
begin
  TBXItemFFNext.Enabled:= edQs.Text <> '';
  TBXItemFFPrev.Enabled:= TBXItemFFNext.Enabled;
  DoQuickSearch(cQsAgain);
end;

procedure TfmMain.TBXItemQsClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_QuickSearch);
end;

procedure TfmMain.edQsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=vk_return) and (Shift=[]) then
  begin
    TbxItemFFNextClick(Self);
    Key:= 0;
    Exit
  end;
  if (Key=vk_return) and (Shift=[ssShift]) then
  begin
    TbxItemFFPrevClick(Self);
    Key:= 0;
    Exit
  end;
end;

procedure TfmMain.ApplyQs;
begin
  //find next/prev
  if opShowQsCaptions then
    TbxItemFFNext.DisplayMode:= nbdmImageAndText
  else
    TbxItemFFNext.DisplayMode:= nbdmDefault;
  TbxItemFFPrev.DisplayMode:= TbxItemFFNext.DisplayMode;

  //case/words
  if opShowQsCaptions then
    cbCase.DisplayMode:= nbdmTextOnly
  else
    cbCase.DisplayMode:= nbdmDefault;
  cbWord.DisplayMode:= cbCase.DisplayMode;
end;

procedure TfmMain.cbWordClick(Sender: TObject);
begin
  cbWord.Checked:= not cbWord.Checked;
  edQsChange(Self);
end;

procedure TfmMain.TBXItemSMarkAllClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smFindAll);
end;

//wrap around CloseFrame, ask for saving etc
procedure TfmMain.CloseFrameWithCfm(F: TEditorFrame;
  var ACanClose, ACanContinue: boolean);
begin
  if QuickView then Exit;
  if F=nil then Exit;

  if F.Modified and F.IsAlertEnabled then
    case MsgConfirmSaveFrame(F, ACanContinue) of
      mrYes:
      begin
        SaveFrame(F, False);
        ACanClose:= true;
        ACanContinue:= true;
        F.Modified:= false; //needed if selected Cancel in SaveDialog
      end;
      mrNo:
      begin
        ACanClose:= true;
        ACanContinue:= true;
        F.Modified:= false;
      end;
      mrCancel:
      begin
        ACanClose:= false;
        ACanContinue:= false;
      end;
    end;

  if ACanClose then
    CloseFrame(F);

  UpdateColorHint;
  UpdateListBookmarks;
end;

procedure TfmMain.TBXItemTabCloseClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseCurrent, true);
end;

function TfmMain.SNewDocName(const fn: Widestring): string;
var
  an: TSyntAnalyzer;
  s: string;
begin
  Result:= ChangeFileExt(ExtractFileName(fn), '');
  SDeleteFrom(Result, '_'); //"_UTF8" suffix may exist

  s:= '?';
  an:= DoFindLexerForFilename(SyntaxManager, fn);
  if an<>nil then s:= an.LexerName;

  Result:= s + #9 + Result;
end;

procedure TfmMain.DoNewDoc(const fn: Widestring);
var
  Enc: Integer;
  Ini: TIniFile;
  Ed: TSyntaxMemo;
begin
  if not IsFileExist(fn) then Exit;

  //add template to MRU list
  Ini:= TIniFile.Create(SynHistoryIni);
  try
    SynMruNewdoc.AddItem(fn);
    SaveMruList(SynMruNewdoc, Ini, 'MRU_Newdoc');
  finally
    FreeAndNil(Ini);
  end;

  //calculate encoding, consider "_UTF8" filename suffix
  Enc:= CP_ACP;
  if Pos('_UTF8', ExtractFileName(fn))>0 then
    Enc:= cp__UTF8_noBOM;

  //need new tab?
  if CurrentEditor=nil then Exit;
  if (CurrentFrame.FileName<>'') or (CurrentEditor.Lines.Count>0) then
    acNewTab.Execute;

  //load template file
  Ed:= CurrentEditor;
  Ed.LoadFromFile(fn);
  Ed.Modified:= true;

  Ed.TextSource.SyntaxAnalyzer:= DoFindLexerForFilename(SyntaxManager, fn);
  UpdateLexerTo(Ed.TextSource.SyntaxAnalyzer);

  //apply encoding
  ApplyFrameEncodingAndReload(CurrentFrame, Enc);
end;

//tab X button rect
procedure TabCtrl_GetXRect(H: THandle; TabIndex: Integer; var R: TRect);
const
  BtnSize = 15; //X button size in SpTbx
begin
  TabCtrl_GetItemRect(H, TabIndex, R);
  Dec(R.Right, 4);
  R.Left:= R.Right - BtnSize;
  R.Top:= (R.Bottom + R.Top - BtnSize) div 2 + 1;
  R.Bottom:= R.Top + BtnSize;
end;

procedure TfmMain.acCloseExecute(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseCurrent, false);
end;

//show filename in statusbar, truncated if needed
procedure TfmMain.DoShowHintFilename(const fn: Widestring);
var
  bmp: TBitmap;
  size: integer;
begin
  //width of last panel
  size:= Status.ClientWidth - 8 - Status.View.Find(StatusItemHint).BoundsRect.Left;

  bmp:= TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(Status.Font);
    DoHint(WideMinimizeName(fn, bmp.Canvas, size));
  finally
    FreeAndNil(bmp);
  end;
end;

procedure TfmMain.acSaveAllExecute(Sender: TObject);
var
  i: Integer;
  F: TEditorFrame;
begin
  for i:= 0 to FrameAllCount-1 do
  begin
    F:= FramesAll[i];
    if F.Modified then
    begin
      SaveFrame(F, False);
      F.Modified:= False;
    end;
  end;
end;

procedure TfmMain.acCloseAllExecute(Sender: TObject);
begin
  DoCloseTabs(tabCloseAll, false);
end;

procedure TfmMain.acCloseOthersThisGroupExecute(Sender: TObject);
begin
  DoCloseTabs(tabCloseOthersThisPage, false);
end;

procedure TfmMain.acCloseOthersAllGroupsExecute(Sender: TObject);
begin
  DoCloseTabs(tabCloseOthersAllPages, false);
end;

procedure TfmMain.TBXItemTabCloseOthersClick(Sender: TObject);
begin
  DoCloseTabs(tabCloseOthersThisPage, true);
end;

procedure TfmMain.TBXItemTabCloseOthersAllGroupsClick(Sender: TObject);
begin
  DoCloseTabs(tabCloseOthersAllPages, true);
end;

procedure TfmMain.UpdateClickedFrame;
var
  N, NPages, NTab: Integer;
begin
  FClickedFrame:= nil;

  //handle right-click over Tabs panel
  if ListTabs.Visible and plTree.Visible then
    if PtInControl(ListTabs, Mouse.CursorPos) then
    begin
      N:= ListTab_FrameIndex;
      if N>=0 then
      begin
        FClickedFrame:= FramesAll[N];

        //set PopupPages/PopupTabIndex, so context menu will work on Tabs panel
        Groups.PagesAndTabIndexOfControl(FClickedFrame, NPages, NTab);
        if NPages>0 then
          Groups.PopupPages:= Groups.Pages[NPages];
        Groups.PopupTabIndex:= NTab;
      end;
      Exit;
    end;

  //handle right-click over tabs  
  with Groups do
    if (PopupPages<>nil) and (PopupTabIndex>=0) then
      FClickedFrame:= PopupPages.Tabs.GetTabData(PopupTabIndex).TabObject as TEditorFrame;
end;

procedure TfmMain.PopupTabContextPopup(Sender: TObject);
var
  en_all, en_named, enProj, enWinMove, enWinOpen: boolean;
  F: TEditorFrame;
begin
  UpdateClickedFrame;
  F:= FClickedFrame;

  en_all:= F<>nil;
  en_named:= en_all and (F.FileName<>'');
  enProj:= en_all and Assigned(fmProj) and plTree.Visible;
  enWinOpen:= en_all and SynExe and not opSingleInstance;
  enWinMove:= en_all and enWinOpen and not F.Modified and (FrameAllCount>1);

  TBXSubmenuItemToGroup.Enabled:= en_all and (Groups.PopupTabIndex>=0);
  TBXSubmenuTabColor.Enabled:= en_all;

  TBXItemTabSave.Enabled:= en_all;
  TBXItemTabSaveAs.Enabled:= en_all;
  TBXItemTabClose.Enabled:= en_all;
  TBXItemTabCloseOthers.Enabled:= en_all and (FrameAllCount>1);
  TBXItemTabCloseOthersAllGroups.Enabled:= TBXItemTabCloseOthers.Enabled and (Groups.PagesVisibleCount>1);
  TBXItemTabCloseLefter.Enabled:= TBXItemTabCloseOthers.Enabled;
  TBXItemTabCloseRighter.Enabled:= TBXItemTabCloseOthers.Enabled;

  TBXItemTabReload.Enabled:= en_all;
  TBXItemTabToggleSplit.Enabled:= en_all;
  TBXItemTabToggleSplit.Checked:= en_all and F.IsSplitted;

  TBXItemTabCopyFN.Enabled:= en_named;
  TBXItemTabCopyFull.Enabled:= en_named;
  TBXItemTabCopyDir.Enabled:= en_named;

  TBXItemTabMoveToWindow.Enabled:= en_named and enWinMove;
  TBXItemTabOpenInWindow.Enabled:= en_named and enWinOpen;
  TBXItemTabAddToProj.Enabled:= en_named and enProj;
end;

procedure TfmMain.TBXItemFSesSaveAsClick(Sender: TObject);
begin
  DoSaveSessionAs;
end;

procedure TfmMain.DoSaveSessionAs;
var
  i: Integer;
begin
  for i:= 0 to FrameCount-1 do
    if Frames[i].FileName = '' then
    begin
      if MsgConfirm(DKLangConstW('MEmp'), Handle) then Break else Exit;
    end;

  with SD_Session do
  begin
    InitialDir:= opLastDirSession;
    //suggest foldername of curr. file as session name
    if CurrentFrame.FileName<>'' then
      FileName:= WideChangeFileExt(WideExtractFileName(WideExtractFileDir(CurrentFrame.FileName)), '')
    else
      FileName:= '';
    if Execute then
    begin
      SaveLastDir_Session(FileName);
      DoSaveSessionToFile(FileName);
      SynMruSessions.AddItem(FileName);
      UpdateTitle(CurrentFrame);
    end;
  end;
end;

procedure TfmMain.DoSaveSessionToFile(const fn: string);
const
  cSess = 'sess';
var
  i, Num: Integer;
  F: TEditorFrame;
  Str, SSec: string;
begin
  FSessionFN:= fn;

  //session dir may not exist, for portable install
  Str:= SExtractFileDir(fn);
  if not IsDirExist(Str) then
    CreateDir(Str);

  try
    with TIniFile.Create(fn) do
    try
      WriteInteger(cSess, 'gr_mode', Ord(Groups.Mode));
      WriteInteger(cSess, 'gr_act', Groups.PagesIndexOf(Groups.PagesCurrent));
      WriteInteger(cSess, 'split', Groups.SplitPos);

      Str:= '';
      for i:= Low(Groups.Pages) to High(Groups.Pages) do
        Str:= Str+IntToStr(Groups.Pages[i].Tabs.TabIndex)+',';
      WriteString(cSess, 'tab_act', Str);

      Num:= -1;
      for i:= 0 to FrameAllCount-1 do
      begin
        F:= FramesAll[i];
        if F.FileName='' then Continue; //skip untitled tabs
        if F.IsFtp then Continue; //skip ftp tabs

        Inc(Num); //start with 0
        SSec:= 'f'+IntToStr(Num);

        WriteInteger(SSec, 'gr', Groups.PagesIndexOf(F.Parent as TATPages));
        WriteString(SSec, 'fn', UTF8Encode(SCollapseFilenameWithDot(F.FileName, SExtractFileDir(fn))));
        WriteString(SSec, 'top', Format('%d,%d', [F.EditorMaster.TopLine, F.EditorSlave.TopLine]));
        WriteString(SSec, 'caret', Format('%d,%d', [F.EditorMaster.CaretStrPos, F.EditorSlave.CaretStrPos]));
        WriteString(SSec, 'wrap', Format('%d,%d', [Ord(F.EditorMaster.WordWrap), Ord(F.EditorSlave.WordWrap)]));
        WriteString(SSec, 'prop', Format('%d,%d,%d,%d,', [
                                    Ord(F.EditorMaster.TextSource.ReadOnly),
                                    Ord(F.EditorMaster.LineNumbers.Visible),
                                    Ord(not F.EditorMaster.DisableFolding),
                                    Ord(F.EditorMaster.SelectModeDefault)
                                    ]));
        WriteString(SSec, 'color', ColorToString(F.TabColor));
        WriteString(SSec, 'colmark', F.EditorMaster.ColMarkersString);
        WriteString(SSec, 'folded', EditorGetCollapsedRanges(F.EditorMaster)+';'+
                                    EditorGetCollapsedRanges(F.EditorSlave));
      end;

      WriteInteger(cSess, 'tabs', Num+1);
    finally
      Free;
    end;
  except
    //don't show msg, for R/O ini folder
  end;
end;

procedure TfmMain.TBXItemFSesOpenClick(Sender: TObject);
begin
  DoSessionOpenDialog;
end;


procedure TfmMain.DoSessionOpenDialog;
begin
  if not DoConfirmSaveSession(true) then
    Exit;
  with OD_Session do
  begin
    InitialDir:= opLastDirSession;
    if Execute then
    begin
      //confirm creation of new session
      if not IsFileExist(FileName) then
      begin
        if not MsgConfirmCreate(FileName, Handle) then
          Exit;
        with TStringList.Create do
        try
          SaveToFile(FileName);
        finally
          Free
        end;
      end;

      SaveLastDir_Session(FileName);
      DoOpenSession(FileName);

      if ExtractFileExt(FileName)='.syn' then
        FileName:= ChangeFileExt(FileName, '.'+cSynSessionExt);
      SynMruSessions.AddItem(FileName);
    end;
  end;
end;

procedure TfmMain.DoOpenSession(AFilename: string; AddMode: boolean = false);
const
  cSess = 'sess';
var
  F: TEditorFrame;
  SSec, AConvName: string;
  Str, SFilename, SDir: Widestring;
  NTabs, NGroup, i: Integer;
  Nums: array[TATGroupsNums] of Integer;
begin
  //support prev session format (using Py script)
  if ExtractFileExt(AFilename)='.syn' then
  begin
    AConvName:= ChangeFileExt(AFilename, '.'+cSynSessionExt);
    if not FileExists(AConvName) then
    begin
      Py_ConvertSessionToNewFormat(AFilename, AConvName);
      if not FileExists(AConvName) then
      begin
        MsgError('Cannot convert session file to new format', Handle);
        Exit
      end;
    end;

    //rename old .syn file
    FFileMove(AFilename, AFilename+'.bak');

    AFilename:= AConvName;
  end;

  //get session dir, w/o last slash
  SDir:= WideExcludeTrailingBackslash(WideExtractFileDir(AFilename));

  with TMemIniFile.Create(AFilename) do
  try
    NTabs:= ReadInteger(cSess, 'tabs', 1);
    if not MsgConfirmOpenSaveSession(NTabs, AFilename, false) then
      Exit;

    if not AddMode then
    begin
      if not DoCloseTabs(tabCloseAll, false) then Exit;
      FSessionFN:= AFilename; //remember fname: after close-all
    end;

    DoControlLock(Self); //fix flicker
    FLockUpdate:= true;

    try
      Groups.Mode:= TATGroupsMode(ReadInteger(cSess, 'gr_mode', 1));
      Groups.SplitPos:= ReadInteger(cSess, 'split', 50);

      for i:= 0 to NTabs-1 do
      begin
        SSec:= 'f'+IntToStr(i);

        SFilename:= UTF8Decode(ReadString(SSec, 'fn', ''));
        if SFilename='' then Continue;

        //filename stored with ".\"
        if SBegin(SFilename, '.\') then
          SReplaceW(SFilename, '.', SDir)
        else
        //filename stored without path
        if WideExtractFileDir(SFilename) = '' then
          SFilename:= SDir + '\' + SFilename;
        //test filename
        if not IsFileExist(SFilename) then Continue;

        NGroup:= ReadInteger(SSec, 'gr', 1);
        if (NGroup>=1) and (NGroup<=Groups.PagesVisibleCount) then
          Groups.PagesCurrent:= Groups.Pages[NGroup];

        F:= DoOpenFile(SFilename);
        if F=nil then Continue;
        F.NotInRecents:= true;

        if cSynHistoryCaret in opSaveEditor then
        begin
          Str:= ReadString(SSec, 'top', '');
          F.EditorMaster.TopLine:= StrToIntDef(SGetItem(Str), 0);
          F.EditorSlave.TopLine:= StrToIntDef(SGetItem(Str), 0);

          Str:= ReadString(SSec, 'caret', '');
          F.EditorMaster.CaretStrPos:= StrToIntDef(SGetItem(Str), 0);
          F.EditorSlave.CaretStrPos:= StrToIntDef(SGetItem(Str), 0);
        end;

        if cSynHistoryWrap in opSaveEditor then
        begin
          Str:= ReadString(SSec, 'wrap', '');
          F.EditorMaster.WordWrap:= Bool(StrToIntDef(SGetItem(Str), 0));
          F.EditorSlave.WordWrap:= Bool(StrToIntDef(SGetItem(Str), 0));
        end;  

        Str:= ReadString(SSec, 'prop', '');
        F.EditorMaster.TextSource.ReadOnly:= Bool(StrToIntDef(SGetItem(Str), 0));
        F.EditorMaster.LineNumbers.Visible:= Bool(StrToIntDef(SGetItem(Str), 0));
        F.EditorMaster.DisableFolding:= not Bool(StrToIntDef(SGetItem(Str), 0));
        F.EditorMaster.SelectModeDefault:= TSyntSelectionMode(StrToIntDef(SGetItem(Str), 0));
        UpdateGutter(F); //apply folding

        DoSetFrameTabColor(F, StringToColor(ReadString(SSec, 'color', ColorToString(clNone))));
        F.EditorMaster.ColMarkersString:= ReadString(SSec, 'colmark', '');

        if cSynHistoryFolding in opSaveEditor then
        begin
          Str:= ReadString(SSec, 'folded', '');
          F.CollapsedString1:= SGetItem(Str, ';');
          F.CollapsedString2:= SGetItem(Str, ';');
        end;  
      end;

      //restore TabIndex'es and PageIndex
      //
      NGroup:= ReadInteger(cSess, 'gr_act', 1);
      //
      Str:= ReadString(cSess, 'tab_act', '');
      for i:= Low(Groups.Pages) to High(Groups.Pages) do
      begin
        Nums[i]:= StrToIntDef(SGetItem(Str), 0);
        DoSetPagesAndTabIndex(i, Nums[i]);
      end;
      i:= NGroup;
      DoSetPagesAndTabIndex(i, Nums[i]);

    finally
      FLockUpdate:= false;
      DoControlUnlock(Self);
    end;
  finally
    Free;
  end;

  UpdateStatusBar;
  DoRepaint;
end;

procedure TfmMain.TBXItemFClearRecentsClick(Sender: TObject);
begin
  TBXItemClrClick(Self);
end;


procedure TfmMain.TbxSubmenuWindowPopup(Sender: TTBCustomItem; FromLink: Boolean);
  //
  function _Sh(i: Integer): string;
  begin
    {
    //this is wrong: sm_TabN call left-tabs, while menuitems call FramesAll[i]
    if (i>=0) and (i<=9) then
      Result:= #9 + GetShortcutTextOfCmd(sm_Tab0+i)
    else
    }
      Result:= '';
  end;
  //
  function _AccPrefix(i: Integer): string;
  begin
    if i < 9 then
      Result:= Format('&%d   ', [i+1])
    else
    if i < 10 + Ord('Z') - Ord('A') then
      Result:= Format('&%s   ', [Chr(i-9 + Ord('A'))])
    else
      Result:= '';
  end;
  //
var
  mi: TSpTbxItem;
  Frame: TEditorFrame;
  i: integer;
begin
  with CurrentFrame do
  begin
    TbxItemWinSplitH.Checked:= IsSplitted and SplitHorz;
    TbxItemWinSplitV.Checked:= IsSplitted and not SplitHorz;
  end;

  //clear previous items (with Tag>0)
  with TbxSubmenuWindow do
    for i:= Count-1 downto 0 do
      if Items[i].Tag>0 then
        Items[i].Free;

  //add items for all frames
  for i:= 0 to FrameAllCount-1 do
  begin
    Frame:= FramesAll[i];
    mi:= TSpTbxItem.Create(Self);
    mi.Caption:= _AccPrefix(i) + Frame.TabCaption + _Sh(i);
    mi.RadioItem:= true;
    mi.Checked:= Frame.Visible;
    mi.Hint:= Frame.FileName;
    mi.Tag:= i+1;
    mi.OnClick:= WindowItemClick;
    mi.OnSelect:= ButtonOnSelect;
    TbxSubmenuWindow.Add(mi);
  end;

  //move lower Window items to end
  for i:= 1 to cFixedWindowItems do
    with TbxSubmenuWindow do
      Move(0, Count-1);
end;

procedure TfmMain.WindowItemClick(Sender: TObject);
var
  N: integer;
begin
  N:= (Sender as TComponent).Tag - 1;
  CurrentFrame:= FramesAll[N];
end;

procedure TfmMain.DoTabIndexClick(n: integer);
begin
  if not DoSetPagesAndTabIndex(1, n) then
    MsgBeep;
end;

procedure TfmMain.DoRtTabIndexClick(n: integer);
begin
  if not DoSetPagesAndTabIndex(2, n) then
    MsgBeep;
end;

procedure TfmMain.TBXSubmenuEncConvertPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  UpdateEncMenu(TBXSubmenuEncConvert, True{AConvEnc});
end;

procedure TfmMain.TBXItemETimeClick(Sender: TObject);
begin
  DoDateTime;
end;

procedure TfmMain.DoDateTime;
begin
  with CurrentEditor do
    if not ReadOnly then
      InsertText(FormatDateTime(opDateFmt, Now));
end;


procedure TfmMain.TBXItemEPasteClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smPaste);
end;

procedure TfmMain.TBXItemEDeleteClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smClearSelection);
end;

procedure TfmMain.TBXItemESelectAllClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSelectAll);
end;

procedure TfmMain.TBXItemECutClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smCut);
end;

procedure TfmMain.TBXItemECopyClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smCopy);
end;

procedure TfmMain.TBXItemEUndoClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smUndo);
end;

procedure TfmMain.TBXItemERedoClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smRedo);
end;

procedure TfmMain.DoReplace_TabsToSpaces(F: TEditorFrame);
//slow! don't use F.EditorMaster.UnTabText;
var
  L: TTntStringList;
begin
  L:= TTntStringList.Create;
  try
    L.Text:= F.EditorMaster.Text;
    DoListCommand_Untab(L, EditorTabSize(F.EditorMaster));
    F.EditorMaster.Text:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;

procedure TfmMain.DoOnlineSearch_Name(const Name: string);
var
  fn: string;
begin
  fn:= SynDataSubdir(cSynDataWebSearch)+'\'+Name+'.ini';
  if FileExists(fn) then
    DoOnlineSearch_Filename(fn)
  else
    MsgNoFile(fn);  
end;

procedure TfmMain.DoOnlineSearch_Filename(const fn: string);
const
  cMacroWord = '{word}';
  cMacroSel = '{sel}';
var
  SWeb, S: Widestring;
begin
  with TIniFile.Create(fn) do
  try
    SWeb:= UTF8Decode(ReadString('info', 'web', ''));
    if SWeb='' then Exit;
  finally
    Free
  end;

  if Pos(cMacroWord, SWeb)>0 then
  begin
    with CurrentEditor do
      S:= WideTrim(WordAtPos(CaretPos));
    if S='' then
      begin MsgNoSelection; Exit; end;
    SReplaceW(SWeb, cMacroWord, S);
  end;

  if Pos(cMacroSel, SWeb)>0 then
  begin
    S:= EditorSelectedTextForWeb(CurrentEditor);
    if S='' then
      begin MsgNoSelection; Exit end;
    SReplaceW(SWeb, cMacroSel, S);
  end;

  FOpenURL(SWeb, Handle);
end;


procedure TfmMain.DoOpenInBrowser(const fn: Widestring);
begin
  if CurrentFrame.FileName <> '' then
  begin
    if CurrentFrame.Modified then
      acSave.Execute;
    if not FExecute(fn, '"' + CurrentFrame.FileName + '"', '', Handle) then
      MsgNoRun(fn);
  end;
end;

procedure TfmMain.TBXItemRunOpenFileClick(Sender: TObject);
begin
  DoOpenCurrentFile;
end;

procedure TfmMain.DoOpenCurrentFile;
begin
  if CurrentFrame.FileName<>'' then
  begin
    if CurrentFrame.Modified then
      SaveFrame(CurrentFrame, false);
    FOpenURL(CurrentFrame.FileName, Handle);
  end
  else
    MsgBeep;
end;

procedure TfmMain.TBXItemRunOpenDirClick(Sender: TObject);
begin
  DoOpenCurrentDir;
end;

procedure TfmMain.DoOpenCurrentDir;
begin
  if CurrentFrame.FileName<>'' then
    FOpenURL(WideExtractFileDir(CurrentFrame.FileName), Handle);
end;

procedure TfmMain.DoOpenCmdPrompt;
var
  SDir: Widestring;
begin
  if CurrentFrame.FileName<>'' then
    SDir:= WideExtractFileDir(CurrentFrame.FileName)
  else
    SDir:= GetCurrentDir;
  FExecute('cmd.exe', '', SDir, Handle);
end;

procedure TfmMain.TimerLoadTimer(Sender: TObject);
begin
  TimerLoad.Enabled:= false;
  LoadTools;
end;

procedure TfmMain.TBXItemTbCloseClick(Sender: TObject);
begin
  acClose.Execute;
end;


procedure TfmMain.acExitExecute(Sender: TObject);
begin
  if SynExe then
    Application.MainForm.Close
  else
    PostMessage(hLister, WM_CLOSE, 0, 0);
end;

procedure TfmMain.DoReplace_InAllTabs(var nRep, nFiles: integer);
var
  F: TEditorFrame;
  i: Integer;
begin
  Finder.Flags:= Finder.Flags-[ftSelectedText]+[ftEntireScope];
  Finder.OnNotFound:= nil;
  F:= CurrentFrame;
  nRep:= 0;
  nFiles:= 0;
  for i:= 0 to FrameAllCount-1 do
  begin
    CurrentFrame:= FramesAll[i];
    Finder.ReplaceAll;
    if Finder.Matches>0 then
    begin
      Inc(nFiles);
      Inc(nRep, Finder.Matches);
    end;
  end;
  CurrentFrame:= F;
  Finder.OnNotFound:= Finder_OnNotFound;
end;

procedure TfmMain.acCloseAndDeleteExecute(Sender: TObject);
var
  fn: Widestring;
begin
  fn:= CurrentFrame.FileName;
  if fn='' then Exit;
  if MsgConfirm(DKLangConstW('mdel'), Handle) then
  begin
    //close
    acClose.Execute;
    //delete file
    if not FDeleteToRecycle(Handle, fn, true) then
      MsgError(WideFormat(DKLangConstW('mdeln'), [WideExtractFileName(fn)]), Handle);
    //delete proj item
    if Assigned(fmProj) then
      fmProj.DoRemoveFile(fn);

    SynMruFiles.DeleteItem(fn);
    DoPlugin_RefreshFiles(fn);
  end;
end;

procedure TfmMain.WMCommandWithClose(var Msg: TMessage);
begin
  case Msg.WParam of
    sm_FileClose:
      acClose.Execute;
    sm_FileCloseAndDelete:
      acCloseAndDelete.Execute;
    sm_FileCloseAll:
      acCloseAll.Execute;
    sm_FileCloseOthers:
      acCloseOthersThisGroup.Execute;
    sm_FileCloseOthersAllGroups:
      acCloseOthersAllGroups.Execute;
    sm_FileRenameDialog:
      acRename.Execute;
    sm_FileOpenSession:
      DoSessionOpenDialog;
    sm_FileAddSession:
      DoSessionAddDialog;
    sm_FileCloseSession:
      DoCloseSession(true);
  end;
  Msg.Result:= 1;
end;

//Finder must be set-up
procedure TfmMain.DoReplace_InFile(const fn: Widestring);
var
  Attr: Dword;
const
  fa = (file_attribute_readonly or file_attribute_hidden or file_attribute_system);
begin
  try
    TemplateEditor.LoadFromFile(fn);
  except
    on E: Exception do
    begin
      MsgExcept('Error on loading file'#13+fn, E, Handle);
      Exit;
    end;
  end;

  try
    try
      Finder.ReplaceAll;
    except
      on E: Exception do
      begin
        MsgExcept('Error on replacing in file'#13+fn, E, Handle);
        Exit;
      end;
    end;

    if Finder.Matches > 0 then
    try
      //clear R/O
      Attr:= GetFileAttributesW(PWChar(fn));
      if (Attr and fa) <> 0 then
        SetFileAttributesW(PWChar(fn), Attr and not fa);
      //save
      TemplateEditor.SaveToFile(fn);
      //return R/O back
      if (Attr and fa) <> 0 then
        SetFileAttributesW(PWChar(fn), Attr);
    except
      on E: Exception do
        MsgExcept('Error on saving file'#13+fn, E, Handle);
    end;
  finally
    TemplateEditor.Lines.Clear;
  end;
end;

procedure TfmMain.DoFind_InFile(const fn: Widestring; InCodepage: TSynEncOverride = cp_sr_Def);
var
  Op: TSyntaxMemoOptions;
  OpWrap: boolean;
  //
  procedure ClearEditor;
  begin
    TemplateEditor.Lines.Clear;
    TemplateEditor.Options:= Op;
    TemplateEditor.WordWrap:= OpWrap;
  end;
begin
  //set "Fixed ln height" option to speed-up editor
  Op:= TemplateEditor.Options;
  OpWrap:= TemplateEditor.WordWrap;
  TemplateEditor.Options:= TemplateEditor.Options + [soFixedLineHeight];
  TemplateEditor.WordWrap:= false;

  case InCodepage of
    cp_sr_OEM:
    begin
      TemplateEditor.Lines.SkipSignature:= true;
      TemplateEditor.Lines.TextCoding:= tcAnsi;
      TemplateEditor.Lines.CodePage:= CP_OEMCP;
    end;
    cp_sr_UTF8:
    begin
      TemplateEditor.Lines.SkipSignature:= true;
      TemplateEditor.Lines.TextCoding:= tcUTF8;
      TemplateEditor.Lines.CodePage:= 0;
    end;
    cp_sr_UTF16:
    begin
      TemplateEditor.Lines.SkipSignature:= true;
      TemplateEditor.Lines.TextCoding:= tcUnicode;
      TemplateEditor.Lines.CodePage:= 0;
    end;
    else
    begin
      TemplateEditor.Lines.SkipSignature:= false;
      TemplateEditor.Lines.TextCoding:= tcAnsi;
      TemplateEditor.Lines.CodePage:= CP_ACP;
    end;
  end;

  try
    FListResFN:= fn;
    TemplateEditor.LoadFromFile(fn);
  except
    on E: Exception do
    begin
      MsgExcept('Error on loading file'#13+fn, E, Handle);
      ClearEditor;
      Exit;
    end;
  end;

  Finder.OnFind:= Finder_OnFind_WithResultPane;
  Finder.OnCanAccept:= nil;
  Finder.OnContinue:= Finder_OnContinue;
  FLastOnContinueCheck:= 0;

  try
    try
      Finder.CountAll;
    finally
      Finder.OnFind:= nil;
      Finder.OnCanAccept:= nil;
      Finder.OnContinue:= nil;
      ClearEditor;
    end;
  except
    on E: Exception do
      MsgExcept('Error on counting matches in file'#13+fn, E, Handle);
  end;
end;

procedure TfmMain.ecReplaceInFilesExecute(Sender: TObject);
begin
  DoFindInFiles_Dialog(false);
end;

procedure TfmMain.ecReplaceInProjectExecute(Sender: TObject);
begin
  if IsProjectEmpty then
    MsgNeedProject
  else
    DoFindInFiles_Dialog(true);
end;

function TfmMain.DoFindInFiles_InputData(
  AInProject: boolean;
  AError: TSynFindInFilesError;
  var D: TSynFindInFilesData): TModalResult;
var
  SMsgRegex: string;
begin
  //get text from "Find" dialog
  if Assigned(fmSR) and fmSR.Visible then
  begin
    D.ATextSearch:= fmSR.Text1;
    D.ATextReplace:= fmSR.Text2;
    if fmSR.Visible and not fmSR.IsDocked then
      fmSR.Hide;
  end
  else
  begin
    D.ATextSearch:= '';
    D.ATextReplace:= '';
  end;

  with TfmSRFiles.Create(Self) do
  try
    SR_IniDir:= Self.SynIniDir;
    SR_InProject:= AInProject;
    SR_CurrentDir:= SExtractFileDir(CurrentFrame.FileName);
    SR_CurrentFile:= SExtractFileName(CurrentFrame.FileName);
    SR_Count:= opSaveFindCount;
    SR_Ini:= SynHistoryIni;
    SR_Ini_S:= SynHistoryIni;
    SR_SuggestedSel:= '';
    SR_SuggestedFind:= D.ATextSearch;
    SR_SuggestedReplace:= D.ATextReplace;
    FKeyGotoFind:= GetShortcutOfCmd(smFindDialog);
    FKeyGotoReplace:= GetShortcutOfCmd(smReplaceDialog);

    case AError of
      cFindFilesOk:
        begin
          with CurrentEditor do
          begin
            if opFindSuggestSel and (SelLength>0) then
              SR_SuggestedSel:= SelText
            else
            if opFindSuggestWord then
              SR_SuggestedSel:= WordAtPos(CaretPos);
          end;
        end;
      cFindFilesNoFiles:
        DoMessage(DKLangConstW('MNFoundNoFiles'));
      cFindFilesNoLines:
        DoMessage(DKLangConstW('MNFoundNoLines'));
    end;

    //use last values of fields
    SR_LastLeft:= FDialogFFiles_Left;
    SR_LastTop:= FDialogFFiles_Top;
    SR_LastFind:= FDialogFFiles_Find;
    SR_LastReplace:= FDialogFFiles_Replace;
    SR_LastMaskInc:= FDialogFFiles_MaskInc;
    SR_LastMaskExc:= FDialogFFiles_MaskExc;
    SR_LastDir:= FDialogFFiles_Dir;

    //center form
    Left:= Self.Monitor.Left + (Self.Monitor.Width - Width) div 2;
    Top:= Self.Monitor.Top + (Self.Monitor.Height - Height) div 2;

    repeat
      Result:= ShowModal;

      D.ATextSearch:= ed1.Text;
      D.ATextReplace:= ed2.Text;
      D.ATextCase:= cbCase.Checked;
      D.ATextWords:= cbWords.Checked;
      D.ATextRegex:= cbRe.Checked;
      D.ATextSpec:= cbSpec.Checked;
      D.ADir:= edDir.Text;
      D.AFnOnly:= cbFnOnly.Checked;
      D.AToTab:= cbOutTab.Checked;
      D.AOutAppend:= cbOutAppend.Checked;
      D.ACloseAfter:= cbCloseAfter.Checked;
      D.ASortMode:= TSynFileSort(edSort.ItemIndex);
      D.InOEM:= cbInOEM.Checked;
      D.InUTF8:= cbInUTF8.Checked;
      D.InUTF16:= cbInUTF16.Checked;

      case Result of
        mrCancel,
        resGotoFind,
        resGotoRep:
          Exit
      end;

      //if dir not exists, goto ShowModal
      if not AInProject then
      begin
        edDir.Text:= WideExcludeTrailingBackslash(edDir.Text);
        if not IsDirExist(edDir.Text) then
        begin
          DoMessage(DKLangConstW('MNFoundFold'));
          Continue
        end;
      end;

      if cbRE.Checked and not IsRegexValid(ed1.Text, SMsgRegex) then
      begin
        DoMessage(DKLangConstW('zMRegexInvalid'));
        Continue
      end;

      Break;
    until false;

    if AInProject then
      D.ADir:= DKLangConstW('zMProjectDir')
    else
      D.ADir:= edDir.Text;

    //confirm mass replace
    if Result=resReplaceAll then
      if not MsgConfirm(
        WideFormat(DKLangConstW('zMCfmMassReplace'), [D.ADir, edFileInc.Text]),
        Self.Handle) then
      begin
        Result:= mrCancel;
        Exit
      end;

    //save last dialog field values
    FDialogFFiles_Find:= ed1.Text;
    FDialogFFiles_Replace:= ed2.Text;
    FDialogFFiles_MaskInc:= edFileInc.Text;
    FDialogFFiles_MaskExc:= edFileExc.Text;
    FDialogFFiles_Dir:= edDir.Text;
    FDialogFFiles_Left:= Left;
    FDialogFFiles_Top:= Top;

    //find files to StringList
    if not DoFindInFiles_GetFileList(FListFiles,
      edDir.Text, edFileInc.Text, edFileExc.Text,
      cbSubDir.Checked, cbNoRO.Checked, cbNoHid.Checked, cbNoHid2.Checked, cbNoBin.Checked,
      D.ASortMode, AInProject) then
      begin
        Result:= mrCancel;
        Exit
      end;
  finally
    Free;
  end;
end;


procedure TfmMain.DoFindInFiles_Dialog(AInProject: boolean);
var
  PrevFlags: TSearchOptions;
  PrevTokens: TSearchTokens;
  PrevText1, PrevText2: Widestring;
  PrevEvent1, PrevEvent2, PrevEvent3: TNotifyEvent;
  PrevEvent4: TOnFindEvent;
  //-------------
  procedure _SaveFinder;
  begin
    PrevFlags:= Finder.Flags;
    PrevTokens:= Finder.Tokens;
    PrevText1:= Finder.FindText;
    PrevText2:= Finder.ReplaceText;
    PrevEvent1:= Finder.OnAfterExecute;
    PrevEvent2:= Finder.OnBeforeExecute;
    PrevEvent3:= Finder.OnNotFound;
    PrevEvent4:= Finder.OnCanAccept;
  end;
  //-------------
  procedure _RestoreFinder;
  begin
    Finder.Control:= CurrentEditor;
    Finder.Flags:= PrevFlags;
    Finder.Tokens:= PrevTokens;
    Finder.FindText:= ''; //don't restore find-text
    Finder.ReplaceText:= '';
    Finder.OnAfterExecute:= PrevEvent1;
    Finder.OnBeforeExecute:= PrevEvent2;
    Finder.OnNotFound:= PrevEvent3;
    Finder.OnCanAccept:= PrevEvent4;
  end;
  //--------
  procedure _SetFinder(const D: TSynFindInFilesData);
  begin
    Finder.Control:= TemplateEditor;
    Finder.FindText:= D.ATextSearch;
    Finder.ReplaceText:= D.ATextReplace;
    Finder.Tokens:= tokensAll;
    Finder.Flags:= [ftEntireScope];
    if D.ATextCase then Finder.Flags:= Finder.Flags + [ftCaseSens];
    if D.ATextWords then Finder.Flags:= Finder.Flags + [ftWholeWords];
    if D.ATextRegex then Finder.Flags:= Finder.Flags + [ftRegex];
    if D.ATextSpec then
    begin
      Finder.FindText:= SDecodeSpecChars(Finder.FindText);
      Finder.ReplaceText:= SDecodeSpecChars(Finder.ReplaceText);
    end;

    Finder.OnAfterExecute:= nil;
    Finder.OnBeforeExecute:= nil;
    Finder.OnNotFound:= nil;
    Finder.OnCanAccept:= nil;
  end;
  //---------
var
  D: TSynFindInFilesData;
  AError: TSynFindInFilesError;
  ARes: TModalResult;
  ANeedFocusResult: boolean;
label
  _Show;
begin
  _SaveFinder;

  ANeedFocusResult:= false;
  AError:= cFindFilesOk;
  _Show:
  ARes:= DoFindInFiles_InputData(AInProject, AError, D);
  AError:= cFindFilesOk;

  case ARes of
    mrCancel:
      begin
        _RestoreFinder;
        Exit
      end;
    resGotoFind:
      begin
        _RestoreFinder;
        ecFind.Execute;
        if Assigned(fmSR) then
        begin
          fmSR.Text1:= D.ATextSearch;
          fmSR.Text2:= D.ATextReplace;
        end;
        Application.ProcessMessages;
        Exit
      end;
    resGotoRep:
      begin
        _RestoreFinder;
        ecReplace.Execute;
        if Assigned(fmSR) then
        begin
          fmSR.Text1:= D.ATextSearch;
          fmSR.Text2:= D.ATextReplace;
        end;
        Application.ProcessMessages;
        Exit
      end;
  end;

  //set Finder and start work
  _SetFinder(D);

  //--------------------------
  //"Find in files" work
  if ARes=resFindAll then
    if FListFiles.Count = 0 then
      AError:= cFindFilesNoFiles
    else
    begin
      DoFindInFiles_FindAction(D.ADir, D.AOutAppend, D.InOEM, D.InUTF8, D.InUTF16);

      //show "Find in files" report in Output pane
      if FTreeRoot=nil then
        raise Exception.Create('TreeRoot nil');

      if not StopFind then
      if FTreeRoot.GetFirstChild=nil then
      begin
        UpdateTreeFind_Results(Finder.FindText, D.ADir, false);
        AError:= cFindFilesNoLines;
      end
      else
      begin
        UpdateTreeFind_Results(Finder.FindText, D.ADir, false);
        if D.AToTab then
        begin
          DoCopyFindResultToTab(true, D.AFnOnly);
        end
        else
        begin
          ANeedFocusResult:= true;
          TabsOut.TabIndex:= Ord(tbFindRes);
          plOut.Show;
        end;
      end;
    end;

  //---------------------------
  //"Replace in files" work
  if ARes=resReplaceAll then
    if FListFiles.Count = 0 then
      AError:= cFindFilesNoFiles
    else
    begin
      if not DoFindInFiles_ReplaceAction(D.ADir, D.AOutAppend) then
        AError:= cFindFilesNoLines
      else
      begin
        FTreeRoot.Expand(false);
        TreeFind.Selected:= FTreeRoot;
        if D.AToTab then
        begin
          DoCopyFindResultToTab(true, true{AFnOnly=true for replace});
        end
        else
        begin
          ANeedFocusResult:= true;
          TabsOut.TabIndex:= Ord(tbFindRes);
          plOut.Show;
        end;
      end;
    end;

  //search work is finished
  if (AError<>cFindFilesOk) or (not D.ACloseAfter) then
  begin
    DoProgressHide;
    goto _Show;
  end;

  //finalize
  DoProgressHide;
  StopFind:= false;
  _RestoreFinder;

  if ANeedFocusResult then
  begin
    if Self.Enabled and TreeFind.CanFocus then
      TreeFind.SetFocus;
  end
  else
    FocusEditor;
end;

function TfmMain.DoFindInFiles_ReplaceAction(
  const ADir: Widestring;
  AOutAppend: boolean): boolean;
var
  ANodeText: Widestring;
  ACountFiles,
  ACountMatches: Integer;
  i: Integer;
begin
  Result:= true;

  //init FTreeRoot, show Output pane
  if not AOutAppend then
    DoClearTreeFind;
  ANodeText:= WideFormat(DKLangConstW('O_fnode_r'),
      [Finder.FindText, Finder.ReplaceText, ADir]);
  FTreeRoot:= TreeFind.Items.Add(nil, ANodeText);

  TabsOut.TabIndex:= Ord(tbFindRes);
  plOut.Show;

  ACountFiles:= 0;
  ACountMatches:= 0;

  fmProgress.SetMode(proFindText);
  Application.ProcessMessages;
  try
    for i:= 0 to FListFiles.Count-1 do
    begin
      try
        DoReplace_InFile(FListFiles[i]);
      except
        on E: Exception do
          MsgExcept('Error on replacing in file'#13+FListFiles[i], E, Handle);
      end;

      try
        if Finder.Matches>0 then
        begin
          Inc(ACountFiles);
          Inc(ACountMatches, Finder.Matches);
          //update TreeFind
          UpdateTreeFind_ReplaceResults(ANodeText, ACountFiles, ACountMatches, false);
          FTreeRoot.Expand(false);

          TreeFind.Selected:= TreeFind.Items.AddChild(FTreeRoot,
            FListFiles[i] + Format(' (%d)', [Finder.Matches]));
            //todo: make adding with prefix+str, using FinderFindWithResPane
        end;
      except
        on E: Exception do
          MsgExcept('Error on adding result'#13+FListFiles[i], E, Handle);
      end;

      //if "Replace in files" stopped
      if IsProgressStopped(i+1, FListFiles.Count) then
      begin
        DoProgressHide;
        UpdateTreeFind_ReplaceResults(ANodeText, ACountFiles, ACountMatches, true);
        Break;
      end;
    end;
  except
    on E: Exception do
    begin
      DoProgressHide;
      MsgExcept('Error on replacing in files', E, Handle);
      Exit;
    end;
  end;

  Result:= ACountMatches<>0;
end;

procedure TfmMain.DoFindInFiles_FindAction(
  const ADir: Widestring;
  AOutAppend, InOEM, InUTF8, InUTF16: boolean);
var
  NTotalSize, NDoneSize: Int64;
  AFn: Widestring;
  ThisInUTF8: boolean;
  i: Integer;
begin
  FListResFN:= '';
  FListResFN_Prev:= '';

  //init TreeRoot, show Output pane
  if not AOutAppend then
    DoClearTreeFind;
  UpdateTreeFind_Initial(Finder.FindText, ADir);
  TabsOut.TabIndex:= Ord(tbFindRes);
  plOut.Show;

  fmProgress.SetMode(proFindText);
  Application.ProcessMessages;
  try
    NTotalSize:= 0;
    NDoneSize:= 0;
    for i:= 0 to FListFiles.Count-1 do
      Inc(NTotalSize, DWORD(FListFiles.Objects[i]));
    if NTotalSize = 0 then
      NTotalSize:= 1;

    FFinderTotalSize:= NTotalSize;
    FFinderDoneSize:= NDoneSize;

    for i:= 0 to FListFiles.Count-1 do
    begin
      try
        //show filename on progress form
        if Assigned(fmProgress) then
        begin
          AFn:= FListFiles[i];
          Delete(AFn, 1, Length(ADir)+1);
          AFn:= WideMinimizeName(AFn, fmProgress.Canvas, Self.ClientWidth - fmProgress.labFilename.Left);
          fmProgress.labFilename.Caption:= AFn;
        end;

        //first search in auto-detected encoding
        ThisInUTF8:= IsFileUTF8NoBOM(FListFiles[i]);
        if ThisInUTF8 then
          DoFind_InFile(FListFiles[i], cp_sr_UTF8)
        else
          DoFind_InFile(FListFiles[i]);

        //additional searches in OEM/UTF8/UTF16
        if not IsFileWithBOM(FListFiles[i]) then
        begin
          if InOEM then
            DoFind_InFile(FListFiles[i], cp_sr_OEM);
          if InUTF8 and not ThisInUTF8 then
            DoFind_InFile(FListFiles[i], cp_sr_UTF8);
          if InUTF16 then
            DoFind_InFile(FListFiles[i], cp_sr_UTF16);
        end;
      except
        on E: Exception do
          MsgExcept('Error on finding in file'#13+FListFiles[i], E, Handle);
      end;

      Inc(NDoneSize, DWORD(FListFiles.Objects[i]));
      FFinderDoneSize:= NDoneSize;
      if IsProgressStopped(NDoneSize, NTotalSize) then
        Break;
    end;

    FFinderTotalSize:= 0;
    FFinderDoneSize:= 0;
  except
    on E: Exception do
    begin
      DoProgressHide;
      MsgExcept('Error on finding in files', E, Handle);
      Exit
    end;
  end;

  //if "Find in files" stopped
  if StopFind then
  begin
    UpdateTreeFind_Results(Finder.FindText, ADir, true);
    Exit
  end;
end;

function TfmMain.DoFindInFiles_GetFileList(
  FListFiles: TTntStringList;
  const SDir, SMaskInc, SMaskExc: Widestring;
  bSubDirs, bNoRO, bNoHidFiles, bNoHidDirs, bNoBinary: boolean;
  ASortMode: TSynFileSort;
  AInProject: boolean): boolean;
var
  N, i: Integer;
begin
  Result:= true;
  FListFiles.Clear;
  DoProgressShow(proFindFiles);

  if AInProject then
  begin
    DoEnumProjFiles(FListFiles);
    if SMaskInc<>'' then
      for i:= FListFiles.Count-1 downto 0 do
        if not FFilenameMatchesMaskList(FListFiles[i], SMaskInc, false) then
          FListFiles.Delete(i);
  end
  else
  try
    FFindToList(FListFiles, sDir, sMaskInc, sMaskExc,
      bSubDirs, bNoRO, bNoHidFiles, bNoHidDirs);
    if StopFind then
    begin
      DoProgressHide;
      Result:= false;
      Exit
    end;
  except
    on E: Exception do
    begin
      MsgExcept('Error on searching for files', E, Handle);
      DoProgressHide;
      Result:= false;
      Exit;
    end;
  end;

  //exclude binary files
  if bNoBinary then
  try
    fmProgress.SetMode(proExclBinary);
    N:= FListFiles.Count;
    for i:= N-1 downto 0 do
    begin
      if not IsFileText(FListFiles[i]) then
        FListFiles.Delete(i);
      if IsProgressStopped(N-i, N) then
      begin
        DoProgressHide;
        Result:= false;
        Exit
      end;
    end;
  except
    on E: Exception do
    begin
      MsgExcept('Error on excluding binary files', E, Handle);
      DoProgressHide;
      Result:= false;
      Exit;
    end;
  end;

  case ASortMode of
    sortDate:
      FListFiles.CustomSort(CompareListDate);
    sortDateDesc:
      FListFiles.CustomSort(CompareListDateDesc);
  end;
end;

procedure TfmMain.DoFind_MarkAll(const Str: Widestring);
var
  PrevFlags: TSearchOptions;
  PrevText1, PrevText2: Widestring;
  PrevEvent1, PrevEvent2, PrevEvent3: TNotifyEvent;
begin
  //save finder
  PrevFlags:= Finder.Flags;
  PrevText1:= Finder.FindText;
  PrevText2:= Finder.ReplaceText;
  PrevEvent1:= Finder.OnAfterExecute;
  PrevEvent2:= Finder.OnBeforeExecute;
  PrevEvent3:= Finder.OnNotFound;

  //set finder
  Finder.Control:= CurrentEditor;
  Finder.FindText:= Str;
  Finder.ReplaceText:= '';
  Finder.Flags:= [ftEntireScope];
  if opHiliteSmartWords then
    Finder.Flags:= Finder.Flags + [ftWholeWords];
  if opHiliteSmartCase then
    Finder.Flags:= Finder.Flags + [ftCaseSens];

  Finder.OnAfterExecute:= nil;
  Finder.OnBeforeExecute:= nil;
  Finder.OnNotFound:= nil;

  //find
  Finder.FindAll;
  MsgFound;

  //hide search marks, if only one result found
  if Finder.Matches=1 then
    CurrentEditor.ResetSearchMarks;

  UpdateFrameMicroMap(CurrentFrame);

  //restore finder
  Finder.Flags:= PrevFlags;
  Finder.FindText:= PrevText1;
  Finder.ReplaceText:= PrevText2;
  Finder.OnAfterExecute:= PrevEvent1;
  Finder.OnBeforeExecute:= PrevEvent2;
  Finder.OnNotFound:= PrevEvent3;
end;

procedure TfmMain.DoSmartHilite;
var
  Ed: TSyntaxMemo;
  s: Widestring;
  i: Integer;
begin
  Ed:= CurrentEditor;
  if Ed=nil then Exit;
  if FSelBlank and (Ed.SelLength=0) then Exit;
  Ed.ResetSearchMarks;
  s:= Ed.SelText;
  FSelBlank:= Ed.SelLength=0;

  if opHiliteSmartWords then
  begin
    //Selection must be a word
    for i:= 1 to Length(s) do
      if not IsWordChar(s[i]) then Exit;
  end;

  DoFind_MarkAll(s);
end;

procedure TfmMain.TimerSelTimer(Sender: TObject);
begin
  TimerSel.Enabled:= false;
  if opHiliteSmart then
    DoSmartHilite;
end;

procedure TfmMain.TimerBracketsTimer(Sender: TObject);
begin
  TimerBrackets.Enabled:= false;
  DoBracketsHilite(CurrentEditor);
end;

procedure TfmMain.TBXItemCtxCopyAppendClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CopyAppend);
end;

procedure TfmMain.TBXItemCtxCutAppendClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CutAppend);
end;

procedure TfmMain.TBXSubmenuEditPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  en, ro: boolean;
begin
  en:= CurrentEditor.HaveSelection;
  ro:= CurrentEditor.ReadOnly;

  //Need enabled items for S/R dialog:
  //TBXItemECut.Enabled:= en and not ro;
  //TBXItemECopy.Enabled:= en;
  //TBXItemEDelete.Enabled:= en and not ro;
  //TBXItemEPaste.Enabled:= not ro;

  TBXItemEUndo.Enabled:= not ro;
  TBXItemERedo.Enabled:= not ro;
  TBXItemEComm.Enabled:= not ro;
  TBXItemEUncomm.Enabled:= not ro;
  TBXItemEIndent.Enabled:= not ro;
  TBXItemEUnindent.Enabled:= not ro;
  TbxItemEMoveUp.Enabled:= not ro;
  TbxItemEMoveDn.Enabled:= not ro;
  TBXItemEDup.Enabled:= not ro;
  TBXItemEDelLn.Enabled:= not ro;
  TBXItemETable.Enabled:= not ro;
  TBXItemETime.Enabled:= not ro;
  TBXItemEJoin.Enabled:= en and not ro;
  TBXItemESplit.Enabled:= not ro;

  TBXItemEFillBlock.Enabled:= en and not ro;
  TBXItemEColumn.Enabled:= en and not ro;
  TBXItemEInsText.Enabled:= not ro;
  TbxItemEToggleLineComment.Enabled:= not ro;
  TbxItemEToggleStreamComment.Enabled:= not ro;
end;

procedure TfmMain.TBXItemSGoBracketClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smChangeRangeSide);
end;

procedure TfmMain.plOutResize(Sender: TObject);
begin
  {$ifndef FixRepaint}
  plOut.Invalidate;
  {$endif}
  tbViewMove(Self);
end;

procedure TfmMain.ecShowOutExecute(Sender: TObject);
begin
  with plOut do
  begin
    Visible:= not Visible;
    FOutVisible:= Visible;
    if not Visible then
      FocusEditor;
  end;

  {$ifndef FixRepaint}
  DoRepaintTBs;
  DoRepaintTBs2;
  {$endif}
end;

procedure TfmMain.plOutVisibleChanged(Sender: TObject);
begin
  FixSplitters;

  ecShowOut.Checked:= plOut.Visible;
  if not plOut.Visible then //Apply when X icon pressed
  begin
    FOutVisible:= false;
    DoRepaint;
  end;

  SyncMapPos;
end;

procedure TfmMain.ListOutDblClick(Sender: TObject);
begin
  with ListOut do
   if (ItemIndex>=0) and (ItemIndex<Items.Count) then
     DoNavigate_ListOut(Items[ItemIndex]);
end;

function TfmMain.DoNavigate_ListOut(const Str: Widestring): boolean;
var
  fn: Widestring;
  nLine, nCol: Integer;
begin
  Result:= false;
  if Str='' then Exit;

  fn:= SynPanelPropsOut.DefFilename;
  SParseOut(Str,
    SynPanelPropsOut.RegexStr,
    SynPanelPropsOut.RegexIdName,
    SynPanelPropsOut.RegexIdLine,
    SynPanelPropsOut.RegexIdCol,
    SynPanelPropsOut.ZeroBase,
    fn, nLine, nCol);

  if fn='' then Exit;
  if nLine<1 then Exit;
  if nCol<1 then nCol:= 1;

  Result:= true;

  //correct fn
  if (SExtractFilePath(fn)='') and (CurrentFrame.FileName<>'') then
    fn:= SExtractFilePath(CurrentFrame.FileName)+fn;
  if not IsFileExist(fn) then
    begin MsgError(WideFormat(DKLangConstW('O_fne'), [fn]), Handle); Exit end;

  DoOpenFile(fn);
  FocusEditor;
  CurrentEditor.CaretPos:= Point(nCol-1, nLine-1);
end;

procedure TfmMain.DoHandleKeysInPanels(var Key: Word; Shift: TShiftState);
var
  sh: TShortcut;
  i: integer;
begin
  sh:= Shortcut(Key, Shift);
  if sh=0 then Exit;

  //Cmd list
  if IsShortcutOfCmd(sh, sm_CommandsList) then
  begin
    ecCommandsList.Execute;
    Key:= 0;
    Exit
  end;

  //Find-in-files
  if IsShortcutOfCmd(sh, sm_ReplaceInFiles) then
  begin
    ecReplaceInFiles.Execute;
    Key:= 0;
    Exit
  end;

  if IsShortcutOfCmd(sh, sm_ReplaceInProject) then
  begin
    ecReplaceInProject.Execute;
    Key:= 0;
    Exit
  end;

  //Toggle panels
  if IsShortcutOfCmd(sh, sm_OptShowLeftPanel) then
  begin
    ecShowTree.Execute;
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_OptShowOutputPanel) then
  begin
    ecShowOut.Execute;
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_OptShowRightPanel) then
  begin
    ecShowClip.Execute;
    Key:= 0;
    Exit
  end;

  //Next/prev search/output result - 6 commands
  if IsShortcutOfCmd(sh, sm_GotoNextFindResult) then
  begin
    ecGotoNextFindResult.Execute;
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_GotoPrevFindResult) then
  begin
    ecGotoPrevFindResult.Execute;
    Key:= 0;
    Exit
  end;

  if IsShortcutOfCmd(sh, sm_GotoNextOutputResult) then
  begin
    CurrentEditor.ExecCommand(sm_GotoNextOutputResult);
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_GotoPrevOutputResult) then
  begin
    CurrentEditor.ExecCommand(sm_GotoPrevOutputResult);
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_GotoNextSearchOrOutputResult) then
  begin
    CurrentEditor.ExecCommand(sm_GotoNextSearchOrOutputResult);
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_GotoPrevSearchOrOutputResult) then
  begin
    CurrentEditor.ExecCommand(sm_GotoPrevSearchOrOutputResult);
    Key:= 0;
    Exit
  end;

  //Focus commands
  if IsShortcutOfCmd(sh, sm_ToggleFocusTree) then
  begin
    ecToggleFocusTree.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusBookmarks) then
  begin
    ecToggleFocusBookmarks.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusValidate) then
  begin
    ecToggleFocusValidate.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusProj) then
  begin
    ecToggleFocusProject.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusMap) then
  begin
    ecToggleFocusMap.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusClip) then
  begin
    ecToggleFocusClip.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusClips) then
  begin
    ecToggleFocusClips.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusFindRes) then
  begin
    ecToggleFocusFindRes.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusOutput) then
  begin
    ecToggleFocusOutput.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusTabs) then
  begin
    ecToggleFocusTabs.Execute;
    Key:= 0;
    Exit
  end;
  //
  if IsShortcutOfCmd(sh, sm_ToggleFocusConsole) then
  begin
    ecToggleFocusConsole.Execute;
    Key:= 0;
    Exit
  end;

  //Tree next/prev/parent/next-brother/prev-brother
  if IsShortcutOfCmd(sh, sm_TreeNextNode) then
  begin
    ecTreeNext.Execute;
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_TreePrevNode) then
  begin
    ecTreePrev.Execute;
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_TreeParent) then
  begin
    ecTreeParent.Execute;
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_TreePrevBrother) then
  begin
    ecTreePrevBrother.Execute;
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(sh, sm_TreeNextBrother) then
  begin
    ecTreeNextBrother.Execute;
    Key:= 0;
    Exit
  end;

  //sm_Tab0..sm_Tab9
  for i:= 0 to 9 do
    if IsShortcutOfCmd(sh, sm_Tab0+i) then
    begin
      DoTabIndexClick(i);
      Key:= 0;
      Exit
    end;

  //sm_TreeLevel2..sm_TreeLevel9
  for i:= 2 to 9 do
    if IsShortcutOfCmd(sh, sm_TreeLevel2+i-2) then
    begin
      DoTreeLevel(i);
      Key:= 0;
      Exit
    end;
end;

procedure TfmMain.ListOutKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key=vk_space) or (Key=vk_return)) and (Shift=[]) then
  begin
    ListOutDblClick(Self);
    Key:= 0;
    Exit
  end;
  if (Key=vk_delete) and (Shift=[]) then
  begin
    TbxItemOutClearClick(Self);
    Key:= 0;
    Exit
  end;
  if (Key=Ord('C')) and (Shift=[ssCtrl]) then
  begin
    TbxItemOutCopySelClick(Self);
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(Shortcut(Key, Shift), smFindDialog) then
  begin
    TBXItemOutFindClick(Self);
    Key:= 0;
    Exit
  end;
  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.TBXItemOutClearClick(Sender: TObject);
begin
  ListOut.Clear;
  FocusEditor;
end;

procedure TfmMain.TBXItemOutCopySelClick(Sender: TObject);
begin
  DoListCopy(ListOut);
end;

procedure TfmMain.DoListCopy(Sender: TTntListbox);
var i:Integer;
  S: Widestring;
begin
  with Sender do
   if SelCount>0 then
   begin
     S:= '';
     for i:= 0 to Items.Count-1 do
       if Selected[i] then
         S:= S+Items[i]+sLineBreak;
     if SelCount=1 then
       S:= SDelLastCR(S);
     TntClipboard.AsWideText:= S;
   end;
end;

procedure TfmMain.TBXItemOutNavClick(Sender: TObject);
begin
  with ListOut do
   if FOutItem>=0 then
    DoNavigate_ListOut(Items[FOutItem]);
end;

procedure TfmMain.PopupOutPopup(Sender: TObject);
begin
  with ListOut do
  begin
    TbxItemOutNav.Enabled:= (FOutItem>=0) and (FOutItem<=Items.Count-1);
    TbxItemOutCopySel.Enabled:= SelCount>0;
    TbxItemOutDelSel.Enabled:= SelCount>0;
    TbxItemOutDelNonparsed.Enabled:= Items.Count>0;
    TbxItemOutClear.Enabled:= Items.Count>0;
    TbxItemOutCopyAll.Enabled:= Items.Count>0;
    TbxItemOutFind.Enabled:= Items.Count>0;
  end;
end;

procedure TfmMain.TBXItemOutCopyAllClick(Sender: TObject);
begin
  DoListCopyAll(ListOut);
end;

procedure TfmMain.DoListCopyAll(Sender: TTntListbox);
begin
  with Sender do
   if Items.Count>0 then
    TntClipboard.AsWideText:= Items.Text;
end;

procedure TfmMain.ApplyACP;
begin
  with ecACP do
  begin
    ItemHeight:= FontHeightToItemHeight(Font);
    Height:= ItemHeight * (DropDownCount+1);
  end;

  PluginACP.Font:= ecACP.Font;
  PluginACP.ItemHeight:= ecACP.ItemHeight;
  PluginACP.Height:= ecACP.Height;
  PluginACP.DropDownCount:= ecACP.DropDownCount;

  TemplatePopup.Font:= ecACP.Font;
  TemplatePopup.ListBox.Font:= ecACP.Font;
  TemplatePopup.ListBox.ItemHeight:= ecACP.ItemHeight;
  TemplatePopup.DropDownCount:= ecACP.DropDownCount;

  ApplyAcpColors;
end;

procedure TfmMain.ApplyOut;
begin
  ListVal.Color:= ListOut.Color;
  ListVal.Font:= ListOut.Font;

  ListTabs.Color:= Tree.Color;
  ListTabs.Font:= Tree.Font;

  ListBookmarks.Color:= Tree.Color;
  ListBookmarks.Font:= Tree.Font;

  TreeFind.Color:= ListOut.Color;
  TreeFind.Font:= ListOut.Font;

  ListPLog.Color:= ListOut.Color;
  ListPLog.Font:= ListOut.Font;

  MemoConsole.Color:= ListOut.Color;
  edConsole.Color:= ListOut.Color;

  ListOut.ItemHeight:= FontHeightToItemHeight(ListOut.Font);
  ListVal.ItemHeight:= ListOut.ItemHeight;
  ListPLog.ItemHeight:= ListOut.ItemHeight;

  ListOut.Invalidate;
  ListVal.Invalidate;
  TreeFind.Invalidate;
  ListPLog.Invalidate;
  ListTabs.Invalidate;
  MemoConsole.Invalidate;

  if Assigned(fmProj) then
  begin
    fmProj.TreeProj.Font:= Tree.Font;
    fmProj.TreeProj.Color:= Tree.Color;
    fmProj.TreeProj.Invalidate;
  end;

  if Assigned(fmClip) then
  begin
    fmClip.ListClip.Color:= ListOut.Color;
    fmClip.ListClip.Font:= ListOut.Font;
    fmClip.ListClip.ItemHeight:= ListOut.ItemHeight;
    fmClip.FColorSel:= opColorOutSelText;
    fmClip.FColorSelBk:= opColorOutSelBk;
    fmClip.ListClip.Invalidate;
  end;

  if Assigned(fmClips) then
  begin
    fmClips.ListNames.Color:= ListOut.Color;
    fmClips.ListNames.Font:= ListOut.Font;
    fmClips.ListNames.ItemHeight:= ListOut.ItemHeight;
    fmClips.Combo.Color:= ListOut.Color;
    fmClips.Combo.Font:= ListOut.Font;
    fmClips.ListNames.Invalidate;
  end;
end;

function TfmMain.IsNavigatableLine(const Str: Widestring): boolean;
var
  fn: Widestring;
  nLine, nCol: Integer;
  LogProps: ^TSynLogPanelProps;
begin
  if ListVal.Visible then
    LogProps:= @SynPanelPropsVal
  else
    LogProps:= @SynPanelPropsOut;

  fn:= LogProps.DefFilename;
  SParseOut(Str,
    LogProps.RegexStr,
    LogProps.RegexIdName,
    LogProps.RegexIdLine,
    LogProps.RegexIdCol,
    LogProps.ZeroBase,
    fn, nLine, nCol);

  Result:= (fn<>'') and (nLine>0);
end;

procedure TfmMain.ListOutDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var c: TColor;
begin
  with Control as TTntListbox do
  begin
    if odSelected in State then
      Canvas.Brush.Color:= opColorOutSelBk
    else
      Canvas.Brush.Color:= Color;
    Canvas.FillRect(Rect);

    if IsNavigatableLine(Items[Index]) then
    begin
      if odSelected in State then
        c:= opColorOutRedSelText
      else
        c:= opColorOutRedText
    end
    else
    begin
      if odSelected in State then
        c:= opColorOutSelText
      else
        c:= Font.Color;
    end;

    Canvas.Font.Color:= c;
    ecTextOut(Canvas, Rect.Left+1, Rect.Top, Items[Index]);
  end;
end;

procedure TfmMain.TBXItemOutDelSelClick(Sender: TObject);
begin
  ListOut.DeleteSelected;
end;

procedure TfmMain.TBXItemOutDelNonparsedClick(Sender: TObject);
var
  i: Integer;
begin
  with ListOut do
  begin
    Items.BeginUpdate;
    try
      for i:= Count-1 downto 0 do
        if not IsNavigatableLine(Items[i]) then
         Items.Delete(i);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TfmMain.ListOutMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then
  with Sender as TTntListbox do
  begin
    FOutItem:= ItemAtPos(Point(x, y), true);
    ItemIndex:= FOutItem;
  end;
end;

procedure TfmMain.ListOutMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  n: Integer;
  S: Widestring;
begin
  if not (Sender is TTntListbox) then
    raise Exception.Create('Sender isn''t Listbox');
  with Sender as TTntListBox do
  begin
    n:= ItemAtPos(Point(x, y), true);
    if n>=0 then
    begin
      S:= Items[n];
      if ecTextExtent(Canvas, S).cx >= ClientWidth then
      begin
        Hint:= S;
        Application.ActivateHint(ClientToScreen(Point(x, y)));
      end
      else
      begin
        Hint:= '';
        Application.HideHint;
      end;
    end;
  end;
end;

procedure TfmMain.Finder_OnContinue(Sender: TObject; var ACanContinue: boolean);
var
  Tick: DWORD;
begin
  //allow to stop every 2 sec
  Tick:= GetTickCount;
  if Tick-FLastOnContinueCheck >= 2*1000 then
  begin
    FLastOnContinueCheck:= Tick;
    Application.ProcessMessages;
  end;
  ACanContinue:= not StopFind;
end;

procedure TfmMain.Finder_OnFind_WithResultPaneAndBkmk(Sender: TObject;
      StartPos, EndPos: integer; var Accept: Boolean);
begin
  Finder_OnFind_WithResultPane(Sender, StartPos, EndPos, Accept);
  Finder_OnFind_WithBkmk(Sender, StartPos, EndPos, Accept);
end;

procedure TfmMain.Finder_OnFind_WithResultPane(Sender: TObject;
      StartPos, EndPos: integer; var Accept: Boolean);
var
  Ed: TCustomSyntaxMemo;
  p: TPoint;
  ColNum, LineNum: integer;
  S: Widestring;
  Node, NodeFile: TTntTreeNode;
  Info: TSynFindInfo;
begin
  //find NodeFile: note with filename
  Node:= TreeFind.Items[TreeFind.Items.Count-1];
  if Node=FTreeRoot then
    NodeFile:= TreeFind.Items.AddChildObject(Node, FListResFN, TSynFindCount.Create)
  else
  begin
    if Assigned(Node.Data) and (TObject(Node.Data) is TSynFindInfo) and (TSynFindInfo(Node.Data).FN<>FListResFN) then
      NodeFile:= TreeFind.Items.AddChildObject(FTreeRoot, FListResFN, TSynFindCount.Create)
    else
      NodeFile:= Node.Parent;
  end;

  //store Finder.Matches into (NodeFile.Data).Matches
  if (TObject(NodeFile.Data) is TSynFindCount) then
    TSynFindCount(NodeFile.Data).Matches:= Finder.Matches;

  if (opMaxTreeMatches>0) then
    if (Finder.Matches>opMaxTreeMatches) then Exit;

  //get info about match and store it into Info
  Ed:= (Sender as TSynFinder).Control;
  p:= Ed.StrPosToCaretPos(StartPos);
  if not ((p.y >= 0) and (p.y < Ed.Lines.Count)) then Exit;
  ColNum:= p.X;
  LineNum:= p.Y;

  S:= Ed.Lines[LineNum];
  SReplaceAllW(S, #9, ' '); //replace tabs with 1 space (to not break BG hiliting) in Treeview

  Info:= TSynFindInfo.Create;
  Info.FN:= FListResFN;
  Info.Str:= S;
  Info.LineNum:= LineNum; //LineNum - 0-based
  Info.ColNum:= ColNum-1; //ColNum - 1-based
  Info.Len:= EndPos-StartPos;

  //add node under NodeFile
  if (opMaxTreeMatches>0) and
    (Finder.Matches=opMaxTreeMatches) then
    TreeFind.Items.AddChildObject(NodeFile, '...', Info)
  else
    TreeFind.Items.AddChildObject(NodeFile,
      SFindResPrefix(LineNum) + Copy(S, 1, cMaxTreeLen), Info);

  //scroll to last file, update
  FTreeRoot.Expand(false);
  if opFindExpand then
    TreeFind.Selected:= TreeFind.Items[TreeFind.Items.Count-1]
  else
    TreeFind.Selected:= NodeFile;
  Application.ProcessMessages;
  //Sleep(250); //debug
end;

procedure TfmMain.Finder_OnFind_WithBkmk(Sender: TObject;
  StartPos, EndPos: integer; var Accept: Boolean);
var
  Ed: TCustomSyntaxMemo;
  LineNum: integer;
begin
  Ed:= (Sender as TSynFinder).Control;
  LineNum:= Ed.StrPosToCaretPos(StartPos).Y;
  if (LineNum>=0) and (LineNum<Ed.Lines.Count) then
    if Ed.BookmarkForLine(LineNum)<0 then
      CurrentFrame.DoBkToggle(Ed, LineNum);
end;

procedure TfmMain.UpdatePanelOut(n: TSynTabOut);
begin
  ListOut.Visible:= n=tbOutput;
  ListVal.Visible:= n=tbValidate;
  TreeFind.Visible:= n=tbFindRes;
  ListPLog.Visible:= n=tbPluginsLog;
  plConsole.Visible:= n=tbConsole;
  ListBookmarks.Visible:= n=tbBookmarks;

  if ListBookmarks.Visible then
    UpdateListBookmarks;
end;

procedure TfmMain.UpdatePanelLeft(n: TSynTabLeft);
var
  IsTree, IsProj, IsTabs: boolean;
  i: Integer;
begin
  //is it plugin tab?
  if n>=tbPlugin1 then
  begin
    i:= Ord(n)-Ord(tbPlugin1);
    DoPlugin_PanelTabClick(i);
    Exit
  end;

  IsTree:= n=tbTree;
  IsProj:= n=tbProj;
  IsTabs:= n=tbTabs;

  Tree.Visible:= IsTree;
  SyncTree;

  ListTabs.Visible:= IsTabs;

  if IsProj then
    LoadProj;
  if Assigned(fmProj) then
    fmProj.Visible:= IsProj;

  DoPlugin_Show(-1);
end;

procedure TfmMain.UpdatePanelRight(n: TSynTabRight);
var
  IsMap, IsClip, IsClips: boolean;
begin
  IsMap:= n=tbMinimap;
  IsClip:= n=tbClipbd;
  IsClips:= n=tbTextClips;

  if IsMap then
    LoadMap;
  if IsClips then
    LoadClips;

  if Assigned(fmClip) then
    fmClip.Visible:= IsClip;

  if Assigned(fmClips) then
    fmClips.Visible:= IsClips;

  if Assigned(fmMap) then
  begin
    fmMap.Visible:= IsMap;
    SyncMapData;
    SyncMapPos;
  end;
end;

procedure TfmMain.TBXItemOOOutClick(Sender: TObject);
begin
  TabsOut.TabIndex:= Ord(tbOutput);
end;

procedure TfmMain.TBXItemOOFindClick(Sender: TObject);
begin
  TabsOut.TabIndex:= Ord(tbFindRes);
end;

procedure TfmMain.PopupFindPopup(Sender: TObject);
var
  IsSel, IsItems: boolean;
begin
  IsSel:= TreeFind.Selected<>nil;
  IsItems:= TreeFind.Items.Count>0;
  
  TBXItemTreeFindNav.Enabled:= IsSel;
  TBXItemTreeFindPreview.Enabled:= IsSel;
  TBXItemTreeFindCopyToTab.Enabled:= IsSel;
  TBXItemTreeFindCopyToClip.Enabled:= IsSel;
  TBXItemTreeFindCopyToClipNode.Enabled:= IsSel;
  TBXItemTreeFindClear.Enabled:= IsItems;
  TBXItemTreeFindFind.Enabled:= IsItems;
  TBXItemTreeFindExpand.Enabled:= IsItems;
  TBXItemTreeFindExpandCur.Enabled:= IsSel;
  TBXItemTreeFindCollapse.Enabled:= IsItems;
end;

procedure TfmMain.ecCopyAsRTFExecute(Sender: TObject);
var Exp: TRTFSyntExport;
begin
  Exp:= TRTFSyntExport.Create(nil);
  try
   Exp.SyntMemo:= CurrentEditor;
   Exp.ExportType:= etSelection;
   Exp.SaveToClipboard;
  finally
   Exp.Free;
  end;
end;

procedure TfmMain.acSetupLexerStylesExecute(Sender: TObject);
var
  An: TSyntAnalyzer;
begin
  An:= SyntaxManager.CurrentLexer;
  if DoLexerStylesDialog(An) then
  begin
    SaveLexLib;
    SyntaxManager.Modified:= false;
  end;
end;

procedure TfmMain.TBXItemEExtrClick(Sender: TObject);
begin
  DoExtractText;
end;

procedure TfmMain.DoExtractText;
begin
  with TfmExtract.Create(Self) do
  try
    FSynIni:= Self.SynHistoryIni;
    SRCount:= opSaveFindCount;
    Memo:= Self.CurrentEditor;
    case ShowModal of
      mrYes:
      begin
        acNewTab.Execute;
        Frames[FrameCount-1].EditorMaster.Text:= List.Items.Text;
      end;
    end;
  finally
    Release;
  end;
end;

procedure TfmMain.PopupStatusEncPopup(Sender: TObject);
begin
  UpdateEncMenu(PopupStatusEnc);
end;

procedure TfmMain.PopupStatusEncConvertPopup(Sender: TObject);
begin
  UpdateEncMenu(PopupStatusEncConvert, true);
end;

procedure TfmMain.UpdateEncMenu(M: TObject; AConvEnc: boolean = false);
  procedure Add(const S: Widestring; Tag: Integer);
  var
    Item: TTbCustomItem;
    MI: TSpTbxItem;
  begin
    if S = '-' then
      Item:= TSpTbxSeparatorItem.Create(Self)
    else
    begin
      MI:= TSpTbxItem.Create(Self);
      Item:= MI;
      MI.Caption:= S;
      MI.Tag:= Tag;
      if AConvEnc then
        MI.OnClick:= MenuitemConvertEncoding
      else
        MI.OnClick:= MenuitemSetEncoding;
      MI.RadioItem:= true;
      MI.Checked:= DoGetFrameEncoding(CurrentFrame) = Tag;
    end;

    if M is TSpTbxPopupMenu then
      (M as TSpTbxPopupMenu).Items.Add(Item)
    else
    if M is TSpTbxSubmenuItem then
      (M as TSpTbxSubmenuItem).Add(Item);
  end;

  function EncOK(n: integer): boolean;
  const
    p: AnsiString = 'pppp';
  begin
    Result:= MultiByteToWideChar(
      n, 0,
      PAnsiChar(p), Length(p),
      nil, 0) > 0;
  end;

  procedure Add2(M: TSpTbxSubmenuItem; const S: Widestring; Tag: Integer;
    IsUnicode: boolean = false);
  var
    MI: TSpTbxItem;
  begin
    if S = '-' then
      M.Add(TSpTbxSeparatorItem.Create(Self))
    else
    begin
      MI:= TSpTbxItem.Create(Self);
      MI.Caption:= S;
      MI.Tag:= Tag;
      if Tag > 0 then
        MI.Hint:= WideFormat(DKLangConstW('cpN'), [Tag]);
      MI.OnSelect:= ButtonOnSelect;
      if AConvEnc then
        MI.OnClick:= MenuitemConvertEncoding
      else
        MI.OnClick:= MenuitemSetEncoding;
      MI.RadioItem:= true;
      MI.Checked:= DoGetFrameEncoding(CurrentFrame) = Tag;
      if not IsUnicode then
        MI.Enabled:= EncOK(Tag);
      M.Add(MI);
    end;
  end;

  function AddSub(const s: Widestring): TSpTbxSubmenuItem;
  begin
    Result:= TSpTbxSubmenuItem.Create(Self);
    Result.Caption:= DoGetLocalizedEncodingName(S);
    if M is TSpTbxPopupMenu then
      (M as TSpTbxPopupMenu).Items.Add(Result)
    else
    if M is TSpTbxSubmenuItem then
      (M as TSpTbxSubmenuItem).Add(Result);
  end;

  procedure AddUni;
  var MSub: TSpTbxSubmenuItem;
  begin
    MSub:= AddSub(DKLangConstW('cpUnicode'));
    Add2(MSub, 'UTF-16', cp__Unicode, true);
    Add2(MSub, 'UTF-16 BE', cp__UnicodeBE, true);
    Add2(MSub, 'UTF-8', cp__UTF8, true);
    Add2(MSub, DKLangConstW('cpUTF8no'), cp__UTF8_noBOM, true);
    Add2(MSub, 'UTF-7', CP_UTF7, true);
  end;
  //----------------------------------
var
  SS, SK: TStringlist;
  Ini: TMemIniFile;
  i, j: Integer;
  MSub: TSpTbxSubmenuItem;
  Main: boolean;
begin
  if M is TSpTbxPopupMenu then
    (M as TSpTbxPopupMenu).Items.Clear
  else
  if M is TSpTbxSubmenuItem then
    (M as TSpTbxSubmenuItem).Clear;

  Main:= M is TSpTbxPopupMenu;
  if not Main then
  begin
    Add('ANSI (Windows)', CP_ACP);
    Add('OEM (DOS)', CP_OEMCP);
    Add('Mac', CP_MACCP);
    Add('-', 0);
    AddUni;
    Add('-', 0);
  end;

  //Read Enc.cfg
  SS:= TStringList.create;
  SK:= TStringlist.create;
  Ini:= TMemIniFile.Create(SynIniDir + 'Enc.cfg');
  try
    Ini.ReadSections(SS);
    for i:= 0 to SS.Count-1 do
    begin
      MSub:= AddSub(SS[i]);
      Ini.ReadSection(SS[i], SK);
      for j:= 0 to SK.Count-1 do
        Add2(MSub, SK[j],
          StrToInt(Ini.ReadString(SS[i], SK[j], '')) );
    end;
  finally
    FreeAndNil(SS);
    FreeAndNil(SK);
    FreeAndNil(Ini);
  end;

  if Main then
  begin
    Add('-', 0);
    AddUni;
    Add('-', 0);
    Add('Mac', CP_MACCP);
    Add('OEM (DOS)', CP_OEMCP);
    Add('ANSI (Windows)', CP_ACP);
  end;
end;

function TfmMain.DoGetFrameEncoding(F: TEditorFrame): integer;
begin
  case F.EditorMaster.TextSource.Lines.TextCoding of
    tcUTF8:
      Result:= IfThen(F.SkipBom, cp__UTF8_noBOM, cp__UTF8);
    tcUnicode:
      Result:= cp__Unicode;
    tcSwapUnicode:
      Result:= cp__UnicodeBE;
    tcAnsi:
      Result:= F.EditorMaster.TextSource.Lines.Codepage;
    else
      Result:= cp_ACP;
  end;
end;


procedure TfmMain.ecSentCaseExecute(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SentenceCaseBlock);
end;

procedure TfmMain.DoZoomEditor(NZoom: Integer);
begin
  CurrentEditor.Zoom:= NZoom;
  StatusItemZoom.Caption:= IntToStr(CurrentEditor.Zoom) + '%';
end;

procedure TfmMain.TBXItemZSet25Click(Sender: TObject);
begin
  DoZoomEditor((Sender as TComponent).Tag);
end;

procedure TfmMain.TBXItemZOtherClick(Sender: TObject);
var
  s: Widestring;
  n: Integer;
begin
  S:= '';
  if MsgInput('zMZoomPrompt', S) then
  begin
    n:= StrToIntDef(S, 0);
    if (n >= 20) and (n <= 500) then
      DoZoomEditor(n);
  end;
end;

procedure TfmMain.ListClipDblClick(Sender: TObject);
begin
  DoClipItemIns;
end;

procedure TfmMain.LoadClip;
begin
  if Assigned(fmClip) then Exit;
  fmClip:= TfmClip.Create(Self);
  with fmClip do
  begin
    Parent:= plClip;
    Align:= alClient;
    BorderStyle:= bsNone;
    ListClip.OnDblClick:= ListClipDblClick;
    ListClip.OnMouseMove:= ListOutMouseMove;
    ListClip.PopupMenu:= PopupClip;
    ListClip.OnKeyDown:= ListClipKeyDown;
    ListClip.BorderStyle:= SynBorderStyle;
    ApplyOut;
    Show;
    if opClipHook then
      InitHook;
  end;
end;

function TfmMain.SynClipsDir: string;
begin
  Result:= SynDataSubdir(cSynDataClips);
end;

function TfmMain.SynDictDir: string;
begin
  Result:= SynDir + 'Dictionaries';
end;

procedure TfmMain.LoadClips;
var
  S: string;
begin
  if Assigned(fmClips) then Exit;
  fmClips:= TfmClips.Create(Self);
  with fmClips do
  begin
    Parent:= plClip;
    Align:= alClient;
    BorderStyle:= bsNone;
    OnClipInsert:= ClipsInsert;
    OnInsPress:= ClipsInsPress;
    ListNames.OnMouseMove:= ListOutMouseMove;
    ListNames.PopupMenu:= PopupClips;
    ListNames.BorderStyle:= SynBorderStyle;
    ApplyOut;

    //load clips
    InitClips(SynClipsDir);
    //restore recent clip
    with TIniFile.Create(SynHistoryIni) do
    try
      s:= ReadString('Win', 'Clip', '');
      if s<>'' then
      begin
        Combo.ItemIndex:= Combo.Items.IndexOf(s);
        ComboChange(Self);
      end;
    finally
      Free
    end;

    Show;
  end;
end;

procedure TfmMain.LoadProj;
begin
  if not Assigned(fmProj) then
  begin
    fmProj:= TfmProj.Create(Self);
    with fmProj do
    begin
      Parent:= plTree;
      Align:= alClient;
      BorderStyle:= bsNone;
      //
      FDirToolsPresets:= SynDataSubdir(cSynDataOutPresets);
      TreeProj.BorderStyle:= SynBorderStyle;
      TreeProj.Font.Assign(Tree.Font);
      TreeProj.Color:= Tree.Color;
      tbProject.ChevronHint:= tbQS.ChevronHint;
      //
      OnPreview:= ProjPreview;
      OnFileOpen:= ProjFileOpen;
      OnRunTool:= ProjRunTool;
      OnAddEditorFile:= ProjAddEditorFile;
      OnAddEditorFilesAll:= ProjAddEditorFiles;
      OnGetLexer:= ProjGetLexer;
      OnGetLexers:= ProjGetLexers;
      OnGetWorkDir:= ProjGetWorkDir;
      OnGetProjDir:= ProjGetProjDir;
      OnSetProjDir:= ProjSetProjDir;
      OnGotoProjFile:= ProjGotoFile;
      OnLoadMRU:= ProjLoadMRU;
      OnUpdateMRU:= ProjUpdateMRU;
      OnProjectOpen:= ProjOpen;
      OnProjectClose:= ProjClose;
      TreeProj.OnKeyDown:= ProjKeyDown;
      //
      Show;
      //
      Self.TbxSubmenuProjTools.LinkSubitems:= TBXItemProjProp;
    end;
  end;
end;

procedure TfmMain.LoadProjPreview;
var
  Ini: TIniFile;
begin
  if not Assigned(FProjPreview) then
  begin
    FProjPreview:= TSpTbxDockablePanel.Create(Self);
    with FProjPreview do
    begin
      Parent:= plTree; //TBXDockRight;
      Options.RightAlignSpacer.FontSettings.Style:= []; //make font non-bold
      ShowCaptionWhenDocked:= opShowPanelTitles;
      DockMode:= dmCanFloat;
      HideWhenInactive:= false;
      OnClose:= ProjPreviewClose;

      ClientWidth:= 400;
      ClientHeight:= 300;
      FloatingClientWidth:= 600;
      FloatingClientHeight:= 400;
      FloatingPosition:= Point(200, 50);
      Floating:= true;

      FProjPreviewEditor:= TSyntaxMemo.Create(Self);
      with FProjPreviewEditor do
      begin
        Parent:= FProjPreview;
        Align:= alClient;
        BorderStyle:= bsNone;
        ReadOnly:= true;
        Gutter.Visible:= false;
        PopupMenu:= PopupPreviewEditor;
        Options:= Options + [soAlwaysShowCaret] - [soScrollLastLine];
        ShowRightMargin:= false;
        Lines.Clear;
        OnKeyDown:= ProjPreviewKeyDown;
      end;

      FProjPreviewButton:= TSpTbxItem.Create(Self);
      with FProjPreviewButton do
      begin
        Caption:= 'Open in editor';
        OnClick:= ProjPreviewButtonClick;
      end;
      FProjPreview.Items.Insert(1, FProjPreviewButton);

      Ini:= TIniFile.Create(SynIni);
      try
        LoadPanelProp(FProjPreview, Ini, 'Pre', true{DefFloating});
      finally
        FreeAndNil(Ini);
      end;

      Ini:= TIniFile.Create(SynHistoryIni);
      try
        FProjPreviewEditor.Zoom:= Ini.ReadInteger('Win', 'PreviewZoom', 100);
      finally
        FreeAndNil(Ini);
      end;

      Hide;
    end;
  end;
end;

procedure TfmMain.ApplyPreviewZoom(NValue: Integer);
var
  Ini: TIniFile;
begin
  if Assigned(FProjPreviewEditor) then
  begin
    FProjPreviewEditor.Zoom:= NValue;

    Ini:= TIniFile.Create(SynHistoryIni);
    try
      Ini.WriteInteger('Win', 'PreviewZoom', NValue);
    finally
      FreeAndNil(Ini);
    end;
  end;
end;

procedure TfmMain.LoadMap;
begin
  if Assigned(fmMap) then Exit;
  fmMap:= TfmMap.Create(Self);
  with fmMap do
  begin
    Parent:= plClip;
    Align:= alClient;
    BorderStyle:= bsNone;
    OnMapClick:= MapClick;
    edMap.BorderStyle:= SynBorderStyle;
    ApplyMinimapProps;
    Show;
  end;
end;

procedure TfmMain.plClipResize(Sender: TObject);
begin
  {$ifndef FixRepaint}
  plClip.Invalidate;
  if Assigned(fmClip) then
    fmClip.ListClip.Invalidate;
  {$endif}
  tbViewMove(Self);
end;

procedure TfmMain.plClipVisibleChanged(Sender: TObject);
begin
  FixSplitters;
  ecShowClip.Checked:= plClip.Visible;

  SyncMapData;
  SyncMapPos;
end;

procedure TfmMain.ecShowClipExecute(Sender: TObject);
begin
  with plClip do
    Visible:= not Visible;
  if not plClip.Visible then
    FocusEditor;

  {$ifndef FixRepaint}
  DoRepaintTBs;
  DoRepaintTBs2;
  {$endif}
end;

procedure TfmMain.TBXItemClipDeleteSelClick(Sender: TObject);
begin
  fmClip.DoDeleteSelected;
end;

procedure TfmMain.TBXItemClipDeleteAllClick(Sender: TObject);
begin
  fmClip.DoDeleteAll;
end;

procedure TfmMain.ecGotoNextFindResultExecute(Sender: TObject);
begin
  DoJumpToNextSearchResult(true);
end;

procedure TfmMain.ecGotoPrevFindResultExecute(Sender: TObject);
begin
  DoJumpToNextSearchResult(false);
end;

procedure TfmMain.DoJumpToNextSearchResult(ANext: boolean);
var
  Node: TTntTreeNode;
begin
  with TreeFind do
  begin
    if Items.Count=0 then Exit;
    if Selected=nil then
      Selected:= Items[0];

    repeat
      if ANext then
        Node:= Selected.GetNext
      else
        Node:= Selected.GetPrev;
      if Node=nil then
        begin MsgBeep; Exit end;

      //allow to stop only on leaf nodes
      if Node.GetFirstChild<>nil then
      begin
        if ANext then
          Node:= Node.GetNext
        else
          Node:= Node.GetPrev;
      end;
      if Node=nil then
        begin MsgBeep; Exit end;

      Selected:= Node;
    until Selected.Data<>nil;

    TreeFindDblClick(Self);
  end;
end;

procedure TfmMain.DoJumpToNextOutputResult(AOutputPanel: boolean; ANext: boolean);
var
  List: TTntListbox;
  N, i: Integer;
  ok: boolean;
begin
  if AOutputPanel then
    List:= ListOut
  else
    List:= ListVal;

  //previous index (will be increased or decreased)
  if ANext then
    N:= -1
  else
    N:= List.Items.Count;

  //consider listbox selection
  for i:= 0 to List.Items.Count-1 do
    if List.Selected[i] then
      begin N:= i; Break end;

  repeat
    if ANext then Inc(N) else Dec(N);
    if not ((N>=0) and (N<List.Items.Count)) then
      begin MsgBeep; Exit end;

    List.ItemIndex:= N;
    for i:= 0 to List.Items.Count-1 do
      List.Selected[i]:= i=N;

    if AOutputPanel then
      ok:= DoNavigate_ListOut(List.Items[N])
    else
      ok:= DoNavigate_ListVal(List.Items[N]);
    if ok then Exit;
  until false;

  MsgBeep;
end;

procedure TfmMain.TBXItemESyncEdClick(Sender: TObject);
begin
  DoToggleSyncEditing;
end;

procedure TfmMain.DoToggleSyncEditing;
begin
  if not EditorToggleSyncEditing(CurrentEditor) then
    MsgBeep;
end;


procedure TfmMain.DoBracketsHilite(Ed: TSyntaxMemo);
var
  n1, n2: Integer;
begin
  if Ed<>nil then
    with Ed do
    begin
      if not opHiliteBrackets then
      begin
        //Invalidate needed to prevent bug: "Current line hiliting" leaves on multiple lines,
        //with SelectModeDefault=msColumn
        Invalidate;
        Exit;
      end;

      if HaveSelection then Exit;
      EditorFindBrackets(Ed, n1, n2);
      if n2<0 then Exit;

      Ed.BracketsHilited:= true;
      SearchMarks.Clear;
      SearchMarks.Add(ecLists.TRange.Create(n1, n1+1));
      SearchMarks.Add(ecLists.TRange.Create(n2, n2+1));
      Invalidate;
    end;
end;

procedure TfmMain.TBXItemFSesAddClick(Sender: TObject);
begin
  DoSessionAddDialog;
end;

procedure TfmMain.DoSessionAddDialog;
begin
  {//No need for "Add session":
  if not DoConfirmSaveSession(true) then
    Exit;}
  with OD_Session do
  begin
    InitialDir:= opLastDirSession;
    if Execute then
    begin
      SaveLastDir_Session(FileName);
      DoOpenSession(FileName, True);
      SynMruSessions.AddItem(FileName);
    end;
  end;
end;

procedure TfmMain.MsgBakEr(const fn: Widestring);
begin
  MsgError(WideFormat(DKLangConstW('MBakEr'), [fn]), Handle);
end;

procedure TfmMain.MsgBakOk(const fn: Widestring);
begin
  MsgInfo(WideFormat(DKLangConstW('MBakOk'), [fn]), Handle);
end;

procedure TfmMain.DoBackup(const AFilename: Widestring);
var
  Dest: Widestring;
const
  ROMask = FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM;
begin
  case opFileBackup of
    cBakAppdata:
      begin
      Dest:= FAppDataPath + 'SynWrite\Backup';
      WideForceDirectories(Dest);
      Dest:= Dest+'\'+WideExtractFileName(AFileName);
      end;
    cBakSameDir:
      Dest:= AFileName+'.bak';
    else
      Exit;
  end;
  if IsFileExist(AFileName) and (FGetFileSize(AFileName)>0) then
  begin
    //clear RO/H/S attr
    if IsFileExist(Dest) then
      SetFileAttributesW(PWChar(Dest),
        GetFileAttributesW(PWChar(Dest)) and not ROMask);

    if not FFileCopy(AFileName, Dest) then
      MsgBakEr(Dest);
  end;
end;

function TfmMain.LastDir: Widestring;
begin
  case opLastDirMode of
    cLastDirCurrentFile:
    begin
      if (CurrentFrame<>nil) and (CurrentFrame.FileName<>'') then
        Result:= WideExtractFileDir(CurrentFrame.FileName)
      else Result:= opLastDirPath
    end
    else
      Result:= opLastDirPath;
  end;

  if (Result<>'') and not IsDirExist(Result) then
  begin
    DoHint(DKLangConstW('MNFoundFold')+': '+Result);
    MsgBeep;
    Result:= 'C:\';
  end;
end;

function TfmMain.LastDir_UntitledFile: Widestring;
begin
  with TIniFile.Create(SynHistoryIni) do
  try
    Result:= UTF8Decode(ReadString('Hist', 'DirUntitled', ''));
  finally
    Free
  end;

  if Result='' then
    Result:= opLastDirPath;
  if (Result<>'') and not IsDirExist(Result) then
  begin
    DoHint(DKLangConstW('MNFoundFold')+': '+Result);
    MsgBeep;
    Result:= 'C:\';
  end;
end;

procedure TfmMain.SaveLastDir_UntitledFile(const FN: Widestring);
begin
  with TIniFile.Create(SynHistoryIni) do
  try
    WriteString('Hist', 'DirUntitled', UTF8Encode(WideExtractFileDir(FN)));
  finally
    Free
  end;
end;

procedure TfmMain.SaveLastDir(const FN, Filter: Widestring; FilterIndex: integer);
begin
  if opLastDirMode<>cLastDirRemember then Exit;

  opLastDirPath:= WideExtractFileDir(FN);
  opHistFilter:= FilterIndex;
  if FilterIndex >= SFilterNum(Filter) then
    opHistFilter:= 0;

  with TIniFile.Create(SynIni) do
  try
    WriteString('Hist', 'Dir', UTF8Encode(opLastDirPath));
    WriteInteger('Hist', 'Filter', opHistFilter);
  finally
    Free;
  end;
end;

procedure TfmMain.SaveLastDir_Session(const FN: Widestring);
var
  S: Widestring;
begin
  opLastDirSession:= WideExtractFileDir(FN);
  S:= SCollapseFilenameDrive(opLastDirSession, SynDir);
  //
  with TIniFile.Create(SynIni) do
  try
    WriteString('Hist', 'DirSess', UTF8Encode(S));
  finally
    Free;
  end;
end;

procedure TfmMain.ApplyEdOptions;
var
  i, N: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    with FramesAll[i] do
    begin
      //apply "Show wrap mark"
      EditorMaster.Gutter.LineBreakObj:= IfThen(opShowWrapMark, 0, -1);
      EditorSlave.Gutter.LineBreakObj:= EditorMaster.Gutter.LineBreakObj;

      //apply non-printed
      UpdateEditorNonPrinted(EditorMaster);
      UpdateEditorNonPrinted(EditorSlave);

      //apply caret time
      N:= TemplateEditor.Caret.Insert.BlinkTime;
      EditorMaster.Caret.Insert.BlinkTime:= N;
      EditorMaster.Caret.Overwrite.BlinkTime:= N;
      EditorSlave.Caret.Insert.BlinkTime:= N;
      EditorSlave.Caret.Overwrite.BlinkTime:= N;
    end;
end;

procedure TfmMain.ApplyFonts;
begin
  TemplateEditor.HorzRuler.Height:=
    5 + Round(Abs(TemplateEditor.HorzRuler.Font.Height) * TemplateEditor.Zoom / 100);

  if Assigned(fmProj) then
  begin
    fmProj.TreeProj.Font:= Tree.Font;
    fmProj.TreeProj.Color:= Tree.Color;
  end;

  ToolbarFont.Assign(FFontMenus);
  DoRepaint;

  FFontSnippetsEditor.Assign(TemplateEditor.Font);

  ApplyColorsFontsToFrames;
  ApplyTabOptions;
end;

procedure TfmMain.ApplyColorsFontsToFrames;
var
  i: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    with FramesAll[i] do
    begin
      DoColorsArrayApply(ColorsArray, EditorMaster);
      DoColorsArrayApply(ColorsArray, EditorSlave);
    end;
end;

procedure TfmMain.ApplyColors;
var
  i: Integer;
begin
  ListOut.Invalidate;
  TreeFind.Invalidate;
  ListVal.Invalidate;
  ListPLog.Invalidate;
  MemoConsole.Invalidate;

  ApplyTabOptions;
  Groups.Invalidate;
  TabsLeft.Invalidate;
  TabsRight.Invalidate;
  TabsOut.Invalidate;

  ApplyFramesOptions;
  ApplyAcpColors;

  if Assigned(fmClip) then
  begin
    fmClip.ListClip.Color:= ListOut.Color;
    fmClip.FColorSel:= opColorOutSelText;
    fmClip.FColorSelBk:= opColorOutSelBk;
    fmClip.ListClip.Invalidate;
  end;

  if Assigned(fmProj) then
    fmProj.TreeProj.Color:= Tree.Color;

  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if FForm<>nil then
        DoPlugin_SetColors(i);
end;

procedure TfmMain.ApplyAcpColors;
begin
  ecACP.BgColor:= opColorAcpBg;
  PluginACP.BgColor:= opColorAcpBg;
  TemplatePopup.BgColor:= opColorAcpBg;

  ecACP.Font.Color:= opColorAcpText;
  PluginACP.Font.Color:= opColorAcpText;
  TemplatePopup.Font.Color:= opColorAcpText;

  //ecACP uses SyntStyles
  SyntStyles.Styles[0].Font.Color:= opColorAcpPrefix;
  SyntStyles.Styles[1].Font.Color:= opColorAcpText;
  SyntStyles.Styles[3].Font.Color:= opColorAcpHintText;
  SyntStyles.Styles[4].Font.Color:= opColorAcpHintText;
  SyntStyles.Styles[5].Font.Color:= opColorAcpHintText2;
end;

procedure TfmMain.ecFullScrExecute(Sender: TObject);
begin
  ShowFullScreen:= not ShowFullScreen;
  UpdateStatusbar;
end;

procedure TfmMain.SetOnTop(V: boolean);
begin
  if not SynExe then Exit;
  if FOnTop <> V then
  begin
    FOnTop:= V;
    SetFormOnTop(Application.MainForm.Handle, V);
    if Assigned(fmSR) then
      SetFormOnTop(fmSR.Handle, V);
  end;
end;

procedure TfmMain.SetFullscreen(AValue: boolean);
begin
  if FFullscreen <> AValue then
  begin
    FFullscreen:= AValue;
    SetFormStyle(Application.MainForm, not AValue);
    if AValue then
    begin
      FBoundsRectOld:= Application.MainForm.BoundsRect;
      Application.MainForm.BoundsRect:= Monitor.BoundsRect;
    end
    else
    begin
      Application.MainForm.BoundsRect:= FBoundsRectOld;
    end;
    TBXDockTop.Visible:= not AValue;
    DoRepaint;
  end;
end;

procedure TfmMain.edQsExit(Sender: TObject);
begin
  if ShowFullScreen then
    if tbQs.CurrentDock= TBXDockTop then
    begin
      TBXDockTop.Hide;
      DoRepaint;
    end;
end;

procedure TfmMain.DoRepaint;
begin
  FNeedRepaint:= true;
end;

procedure TfmMain.FocusEditor;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Self.Enabled then
    if (Ed<>nil) and Ed.CanFocus then
      Ed.SetFocus;
end;

procedure TfmMain.DoClearSearchHistory;
begin
  with TIniFile.Create(SynHistoryIni) do
  try
    EraseSection('SearchText');
    EraseSection('ReplaceHist');
    EraseSection('FilesIncHist');
    EraseSection('FilesExcHist');
    EraseSection('FilesDirHist');
    EraseSection('ExtractHist');
    EraseSection('CharCode');
  finally
    Free;
  end;

  if Assigned(fmSR) then
    with fmSR do
    begin
      ed1.Items.Clear;
      ed2.Items.Clear;
      Text1:= '';
      Text2:= '';
    end;
end;

procedure TfmMain.DoCopyFilenameToClipboard(F: TEditorFrame; Cmd: TSynCopyNameCmd);
begin
  if (F<>nil) and (F.FileName<>'') then
    case Cmd of
      cCmdCopyFileName: TntClipboard.AsWideText:= WideExtractFileName(F.FileName);
      cCmdCopyFullName: TntClipboard.AsWideText:= F.FileName;
      cCmdCopyFilePath: TntClipboard.AsWideText:= WideExtractFileDir(F.FileName);
    end;
end;

procedure TfmMain.TBXItemTabCopyFNClick(Sender: TObject);
begin
  DoCopyFilenameToClipboard(FClickedFrame, cCmdCopyFileName);
end;

procedure TfmMain.TBXItemTabCopyFullClick(Sender: TObject);
begin
  DoCopyFilenameToClipboard(FClickedFrame, cCmdCopyFullName);
end;

procedure TfmMain.TBXItemTabCopyDirClick(Sender: TObject);
begin
  DoCopyFilenameToClipboard(FClickedFrame, cCmdCopyFilePath);
end;

procedure TfmMain.TBXItemSp50Click(Sender: TObject);
begin
  ecSplit50_50.Execute;
end;

procedure TfmMain.TBXItemTbCloseAllClick(Sender: TObject);
begin
  acCloseAll.Execute;
end;

procedure TfmMain.ecSplitViewsVertHorzExecute(Sender: TObject);
begin
  case Groups.Mode of
    gm2Horz: Groups.Mode:= gm2Vert;
    gm2Vert: Groups.Mode:= gm2Horz;
  end;
end;

procedure TfmMain.ecSyncScrollHExecute(Sender: TObject);
begin
  with ecSyncScrollH do
    Checked:= not Checked;
end;

procedure TfmMain.ecSyncScrollVExecute(Sender: TObject);
var
  EdOther: TSyntaxMemo;
  View1st: boolean;
begin
  with ecSyncScrollV do
  begin
    Checked:= not Checked;
    if Checked then
      DoGetOppositeEditor(CurrentEditor, EdOther, FCurrDiffScrollY, View1st);
  end;
end;

procedure TfmMain.DoGetOppositeEditor(
  const EdSrc: TSyntaxMemo;
  var EdOther: TSyntaxMemo;
  var DiffInTopLines: Integer;
  var EdSrcOnGroup1: boolean);
var
  F: TEditorFrame;
begin
  EdOther:= nil;

  EdSrcOnGroup1:= (FrameOfEditor(EdSrc).Parent as TATPages) = Groups.Pages1;
  if EdSrcOnGroup1 then
    F:= GetCurrentFrameInPages(Groups.Pages2)
  else
    F:= GetCurrentFrameInPages(Groups.Pages1);

  if F<>nil then
  begin
    EdOther:= F.EditorMaster;
    DiffInTopLines:= EdOther.TopLine - EdSrc.TopLine;
    if not EdSrcOnGroup1 then
      DiffInTopLines:= -DiffInTopLines;
  end;
end;

procedure TfmMain.DoSyncScroll(EdSrc: TSyntaxMemo);
var
  EdOther: TSyntaxMemo;
  DiffY: Integer;
  View1st: boolean;
begin
  if not (ecSyncScrollV.Checked or ecSyncScrollH.Checked) then Exit;
  if EdSrc=nil then Exit;

  DoGetOppositeEditor(EdSrc, EdOther, DiffY, View1st);
  if EdOther=nil then Exit;
  if EdOther.Lines.Count=0 then Exit;

  DiffY:= FCurrDiffScrollY;
  if not View1st then
    DiffY:= -DiffY;

  if ecSyncScrollV.Checked then
    EdOther.TopLine:= EdSrc.TopLine + DiffY;
  if ecSyncScrollH.Checked then
    EdOther.ScrollPosX:= EdSrc.ScrollPosX;
end;

procedure TfmMain.TBXItemOShellClick(Sender: TObject);
begin
  DoConfigShellOptions;
end;

procedure TfmMain.DoConfigShellOptions;
var
  i: Integer;
begin
  with TfmShell.Create(nil) do
  try
    FLex.Clear;
    for i:= 0 to SyntaxManager.AnalyzerCount-1 do
      with SyntaxManager.Analyzers[i] do
        if (not Internal) and (Extentions <> '') then
          FLex.Add(LexerName + '=' + Extentions);
    ShowModal;
  finally
    Release
  end;
end;

procedure TfmMain.UpdateRO;
  procedure RO;
  begin
    if not ecReadOnly.Checked then
      ecReadOnly.Execute;
  end;
  procedure NoRO;
  begin
    if ecReadOnly.Checked then
      ecReadOnly.Execute;
  end;
var
  s: string;
  i, N: Integer;
begin
  if not SynExe then
  begin
    if not QuickView then
      if opListerStartRO then RO;
    if QuickView then
      if opListerQVReadOnly then RO else NoRO;
  end
  else
  begin
    //consider cmdline param
    for i:= 1 to ParamCount do
    begin
      s:= ParamStr(i);
      if s=cSynParamRO then
      begin
        RO;
        Continue
      end;
      if SBegin(s, cSynParamLineNum) then
      begin
        Delete(s, 1, Length(cSynParamLineNum));
        N:= StrToIntDef(s, -1)-1;
        if N>=0 then
          CurrentEditor.CaretPos:= Point(0, N);
      end;
    end;
  end;
end;

procedure TfmMain.ecOnTopExecute(Sender: TObject);
begin
  ShowOnTop:= not ShowOnTop;
  ecOnTop.Checked:= ShowOnTop;
end;

procedure TfmMain.tbMenuShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  if Assigned(fmSR) and fmSR.Active then
  begin
    Handled:= fmSR.IsShortCut(Msg);
    Exit
  end;
end;

procedure TfmMain.TBXItemEFillBlockClick(Sender: TObject);
begin
  DoFillBlock;
end;

procedure TfmMain.DoFillBlock;
begin
end;

//this function deprecated.
(*
procedure TfmMain.DoFillBlock;
var
  Ed: TSyntaxMemo;
  sData: Widestring;
  bKeep: boolean;
begin

  Ed:= CurrentEditor;
  if Ed.ReadOnly then Exit;
  if not Ed.HaveSelection then Exit;

  if Ed.SelectMode <> msColumn then
  begin
    MsgWarn(DKLangConstW('vert'), Handle);
    Exit
  end;

  with TfmFillBlock.Create(Self) do
  try
    if ShowModal<>mrOk then Exit;
    sData:= edLine.Text;
    bKeep:= cbKeepWidth.Checked;
  finally
    Free
  end;

  EditorFillBlockRect(Ed, sData, bKeep);
end;
*)

procedure TfmMain.TBXItemEInsTextClick(Sender: TObject);
begin
  DoInsertTextDialog;
end;

procedure TfmMain.DoInsertTextDialog;
var
  Ed: TSyntaxMemo;
  F: TEditorFrame;
  Data: TSynEditorInsertData;
begin
  Ed:= CurrentEditor;
  if Ed.ReadOnly then Exit;
  F:= FrameOfEditor(Ed);
  FillChar(Data, SizeOf(Data), 0);

  with TfmInsTxt.Create(nil) do
  try
    SIni:= Self.SynHistoryIni;
    SetMode(EditorHasMultilineSelection(Ed));
    if ShowModal<>mrOk then Exit;

    Data.SText1:= ed1.Text;
    Data.SText2:= ed2.Text;
    if bText.Checked then Data.InsMode:= mTxt else
     if bNumber.Checked then Data.InsMode:= mNum else
      Data.InsMode:= mBul;
    if bAtCol.Checked then Data.InsPos:= pCol else
     if bAfterSp.Checked then Data.InsPos:= pAfterSp else
      Data.InsPos:= pAfterStr;
    Data.InsCol:= edCol.Value;
    Data.InsStrAfter:= edStrAfter.Text;
    Data.SkipEmpty:= cbSkip.Checked;
    Data.NStart:= edStart.Value;
    Data.NDigits:= edDigits.Value;
    Data.NTail:= edTail.Text;
    Data.NBegin:= edBegin.Text;
    Data.NCounter:= edCounter.Value;
  finally
    Free
  end;

  F.LockMapUpdate:= true;
  FLockUpdate:= true;
  UpdateFormEnabled(false);
  try
    EditorInsertTextData(Ed, Data, DoHint);
    DoHint('');
  finally
    UpdateFormEnabled(true);
    FLockUpdate:= false;
    F.LockMapUpdate:= false;
  end;
end;

function DoFindFileInSubdirs(
  const sel: Widestring;
  dir: Widestring;
  const def_ext: Widestring): Widestring;
var
  fn: Widestring;
begin
  Result:= '';
  if (sel='') or (dir='') then Exit;

  //WithPath:= WideExtractFileDir(sel) <> '';
  //SubDirs:= (dir[Length(dir)] = '*');
  dir:= STrimFolderName(dir);

  //try exact name in dir
  fn:= dir + '\' + WideExtractFileName(sel);
  if IsFileExist(fn) then
    begin Result:= fn; Exit end;

  //try name plus default_ext in dir
  if (WideExtractFileExt(fn) = '') and (def_ext <> '') then
    fn:= fn + '.' + def_ext;
  if IsFileExist(fn) then
    begin Result:= fn; Exit end;

  (*
  //try recursive search from dir (if * at end)
  if SubDirs and not WithPath then
  begin
    fn:= sel;
    FFindInSubdirs(fn, dir, Result);
    if IsFileExist(Result) then Exit;

    if (WideExtractFileExt(sel) = '') and (def_ext <> '') then
    begin
      fn:= sel + '.' + def_ext;
      FFindInSubdirs(fn, dir, Result);
      if IsFileExist(Result) then Exit;
    end;
  end;
  *)
end;

procedure TfmMain.TBXItemCtxOpenSelClick(Sender: TObject);
begin
  acOpenBySelection.Execute;
end;

procedure TfmMain.DoOpenBySelection;
var
  sel, fn, s, dir, ext: Widestring;
  n, LnNum: integer;
  Dirs, Dirs2: TTntStringList;
begin
  LnNum:= 0;
  sel:= CurrentEditor.SelText;
  if (sel='') or (Pos(#13, sel)>0) or (Pos(#10, sel)>0) then
    begin MsgBeep; Exit end;

  //parse text "filename(LineNum)"
  if sel[Length(sel)]=')' then
  begin
    n:= Length(sel);
    while (n>0) and (sel[n]<>'(') do Dec(n);
    if n=0 then
      begin MsgBeep; Exit end;
    s:= Copy(sel, n, MaxInt);
    s:= Copy(s, 2, Length(s)-2);
    LnNum:= StrToIntDef(s, 1);
    Delete(sel, n, MaxInt);
    sel:= Trim(sel);
    if sel='' then
      begin MsgBeep; Exit end;
  end;

  //ext - 1st extension of lexer
  if CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer<>nil then
    ext:= CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer.Extentions
  else
    ext:= '';
  SDeleteFromW(ext, ' ');

  //try filename in current dir
  fn:= sel;
  if WideExtractFileDir(fn)='' then
    if CurrentFrame.FileName<>'' then
      fn:= WideExtractFilePath(CurrentFrame.FileName)+fn
    else
      fn:= WideGetCurrentDir+'\'+fn;

  //try filename + def_ext in current dir
  if not IsFileExist(fn) then
    if ext<>'' then
      fn:= fn + '.' + ext;

  //find filename in Project paths
  if not IsFileExist(fn) then
  try
    Dirs:= TTntStringList.Create;
    Dirs2:= TTntStringList.Create;

    SStringToList(opProjPaths, Dirs);
    if Assigned(fmProj) and (fmProj.FOpts.SearchDirs<>'') then
    begin
      SStringToList(fmProj.FOpts.SearchDirs, Dirs2);
      Dirs.AddStrings(Dirs2);
    end;

    //a. search in matched folder first
    fn:= '';
    for n:= 0 to Dirs.Count-1 do
    begin
      dir:= STrimFolderName(Dirs[n]);
      if Pos(
        LowerCase(dir + '\'),
        LowerCase(WideExtractFilePath(CurrentFrame.FileName))) > 0 then
      begin
        //Msg('m'#13+dir);
        fn:= DoFindFileInSubdirs(sel, Dirs[n], ext);
        if IsFileExist(fn) then
        begin
          Dirs.Delete(n);
          Break;
        end;
      end;
    end;

    //b. search in other project folders
    if fn = '' then
      for n:= 0 to Dirs.Count-1 do
      begin
        fn:= DoFindFileInSubdirs(sel, Dirs[n], ext);
        if IsFileExist(fn) then Break;
      end;
  finally
    FreeAndNil(Dirs);
    FreeAndNil(Dirs2);
  end;

  //stop trying, show msg
  if not IsFileExist(fn) then
    MsgNoFile(WideExtractFileName(sel))
  else
  begin
    //open with warning about non-text content
    if IsFileText(fn) or MsgConfirmBinary(fn, Handle) then
    begin
      DoOpenFile(fn);
      if LnNum > 0 then
        CurrentEditor.CaretPos:= Point(0, LnNum-1);
    end;
  end;
end;

procedure TfmMain.Finder_OnProgress(CurPos, MaxPos: integer);
var
  N: Int64;
begin
  if Assigned(FinderPro) then
  begin
    if MaxPos=0 then
      Inc(MaxPos);

    if FFinderTotalSize>0 then
      N:= Int64(FFinderDoneSize + CurPos) * 100 div FFinderTotalSize
    else
      N:= Int64(CurPos) * 100 div MaxPos;

    //don't check "if FinderProNum<>N", this slows down
    FinderProNum:= N;
    FinderPro.Progress:= N;
    {
    //
    //Need EndUpdate/BeginUpdate if locked
    b:= CurrentEditor.UpdateCount>0;
    if b then
      CurrentEditor.EndUpdate;
      }
    Application.ProcessMessages;
    {
    if b then
      CurrentEditor.BeginUpdate;
      }
  end;
end;

procedure TfmMain.DoAcpFromFile(List, Display: ecUnicode.TWideStrings);
const
  cNonWordChars = '''"';
var
  S, SWord: Widestring;
  LL: TTntStringList;
  i, NCaret: Integer;
  IsWord, AtCaret: boolean;
begin
  if not opAcpFile then
    Exit;

  S:= CurrentEditor.Text;
  i:= Round(opAcpFileSize * (1024*1024));
  if Length(S) > i then
    SetLength(S, i);

  LL:= TTntStringList.Create;
  LL.Sorted:= true;
  LL.Duplicates:= dupIgnore;
  SWord:= '';
  NCaret:= CurrentEditor.CaretStrPos;
  AtCaret:= false;

  try
    for i:= 1 to Length(S)+1 {Len+1} do
    begin
      //if i mod 10 = 0 then
      //  MsgAcpFile('Searching '+IntToStr(i*100 div Length(s))+'%');

      if i<=Length(S) then
      begin
        IsWord:= IsWordChar(S[i]) or
          (Pos(S[i], opAcpChars)>0) or
          (S[i]='%') or
          ((S[i]='.') and (i<Length(S)) and IsWordChar(S[i+1]));
        if Pos(S[i], cNonWordChars)>0 then
          IsWord:= false;  
      end
      else
        IsWord:= false;

      //trailing ':' can't be wordchar (':' is wordchar for CSS)
      if (i>1) and (S[i]=':') and IsWordChar(S[i-1]) then
        IsWord:= false;

      if not IsWord then
      begin
        if Length(SWord) >= opAcpFileChars then
          if not AtCaret then
            LL.Add(SWord);
        SWord:= '';
        AtCaret:= false;
      end
      else
      begin
        SWord:= SWord + S[i];
        if i=NCaret then
          AtCaret:= true;
      end;
    end;

    S:= SAcpItem(DKLangConstW('typed'), ''); //beginning of Display string
    for i:= 0 to LL.Count-1 do
    begin
      //if List.IndexOf(LL[i])<0 then //<-- don't use, it freezes on large file!!
      List.Add(LL[i]);
      Display.Add(S + LL[i]);
    end;
  finally
    FreeAndNil(LL);
    DoHint('');
  end;
end;

procedure TfmMain.DoBackupLexerStyles(ALexer: TSyntAnalyzer);
begin
  SaveLexerStylesToFile(ALexer, SynStylesIni);
end;

procedure TfmMain.TBXItemORestoreStylesClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RestoreStylesDialog);
end;


procedure TfmMain.DoConfigRestoreStyles;
  //------
  function FindAn(const S: string): TSyntAnalyzer;
  var i: Integer;
  begin
    Result:= nil;
    with SyntaxManager do
      for i:= 0 to AnalyzerCount-1 do
        if Analyzers[i].LexerName = S {case-sens!} then
          begin Result:= Analyzers[i]; Exit end;
  end;
var
  i: Integer;
  An: TSyntAnalyzer;
  S: string;
begin
  with TfmLoadLex.Create(nil) do
  try
    SIniStyles:= Self.SynStylesIni;
    if ShowModal=mrOk then
    begin
      for i:= 0 to List.Count-1 do
        if List.Checked[i] then
        begin
          An:= FindAn(List.Items[i]);
          if An<>nil then
          begin
            LoadLexerStylesFromFile(An, SynStylesIni);
            S:= S + List.Items[i] + #13;
          end
          else
            MsgWarn(WideFormat(DKLangConstW('MNLex'), [List.Items[i]]), Handle);
        end;
      SaveLexLib;
      //MsgInfo(S);
    end;
  finally
    Release
  end;
end;

procedure TfmMain.TBXItemCtxCustomizeClick(Sender: TObject);
begin
  acSetup.Execute;
end;

type
  TTreeCrack = class(TCustomSyntaxTreeview);

procedure TfmMain.TreeKeyPress(Sender: TObject; var Key: Char);
begin
  //disable beep on Enter/Esc
  if Key=#13 then Key:= #0;
  if Key=#27 then Key:= #0;
end;

procedure TfmMain.TreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sh: TShortcut;
begin
  //Enter in tree
  if (Key=vk_return) and (Shift=[]) then
  begin
    TTreeCrack(Tree).DblClick;
    Key:= 0;
    Exit
  end;

  sh:= Shortcut(Key, Shift);
  if sh=0 then Exit;

  //Ctrl+F in tree
  if IsShortcutOfCmd(sh, smFindDialog) then
  begin
    ecFindInTree.Execute;
    Key:= 0;
    Exit;
  end;
  //F3 in tree
  if IsShortcutOfCmd(sh, smFindNext) then
  begin
    ecFindInTreeNext.Execute;
    Key:= 0;
    Exit;
  end;
  //Ctrl+F3 in tree
  if IsShortcutOfCmd(sh, smFindPrev) then
  begin
    ecFindInTreePrev.Execute;
    Key:= 0;
    Exit;
  end;

  DoHandleKeysInPanels(Key, Shift);
end;

function TfmMain.DoClipItem: Widestring;
begin
  Result:= '';
  if Assigned(fmClip) then
  with fmClip do
    if ListClip.ItemIndex >= 0 then
      Result:= ItemAt(ListClip.ItemIndex);
end;

procedure TfmMain.DoClipItemCopy;
var
  s: Widestring;
begin
  s:= DoClipItem;
  if s<>'' then
    TntClipboard.AsWideText:= s;
end;

procedure TfmMain.DoClipItemIns;
var
  s: Widestring;
begin
  s:= DoClipItem;
  if s<>'' then
    with CurrentEditor do
    begin
      //Need separate action in Undo list (after typing text)
      //EditorSetModified(CurrentEditor);
      InsertText(s);
    end;
end;

procedure TfmMain.ListClipKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Enter in clip
  if (Key=vk_return) and (Shift=[]) then
  begin
    DoClipItemIns;
    Key:= 0;
    Exit
  end;
  //Ctrl+C in clip
  if (Key=Ord('C')) and (Shift=[ssCtrl]) then
  begin
    DoClipItemCopy;
    Key:= 0;
    Exit
  end;
  //Ctrl+F in clip
  if IsShortcutOfCmd(Shortcut(Key, Shift), smFindDialog) then
  begin
    DoFind_InClipPanel;
    Key:= 0;
    Exit
  end;
  //Del in clip
  if (Key=VK_DELETE) and (Shift=[]) then
  begin
    fmClip.DoDeleteSelected;
    Key:= 0;
    Exit
  end;
  //Shift+Del in clip
  if (Key=VK_DELETE) and (Shift=[ssShift]) then
  begin
    fmClip.DoDeleteAll;
    Key:= 0;
    Exit
  end;
  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.DoTreeFocus;
begin
  TabsLeft.TabIndex:= Ord(tbTree);
  if Self.Enabled and Tree.CanFocus then
    Tree.SetFocus;
end;

procedure TfmMain.DoBookmarksFocus;
begin
  TabsOut.TabIndex:= Ord(tbBookmarks);
  if Self.Enabled and ListBookmarks.CanFocus then
    ListBookmarks.SetFocus;
end;

procedure TfmMain.ecToggleFocusTreeExecute(Sender: TObject);
begin
  if not plTree.Visible then
  begin
    ecShowTree.Execute;
    DoTreeFocus;
  end
  else
  if Tree.Focused then
    FocusEditor
  else
    DoTreeFocus;
end;

procedure TfmMain.ecToggleFocusClipExecute(Sender: TObject);
begin
  if not plClip.Visible then
  begin
    ecShowClip.Execute;
    TabsRight.TabIndex:= Ord(tbClipbd);
    if fmClip.ListClip.CanFocus then
      fmClip.ListClip.SetFocus;
  end
  else
  if fmClip.ListClip.Focused then
    FocusEditor
  else
  begin
    TabsRight.TabIndex:= Ord(tbClipbd);
    if fmClip.ListClip.CanFocus then
      fmClip.ListClip.SetFocus
  end;
end;

procedure TfmMain.UpdateQVTree;
begin
  if opListerQVTree<>'' then
    plTree.Visible:= SFileExtensionMatch(fn, opListerQVTree);
end;

procedure TfmMain.UpdateLexer;
var
  Lexer: string;
begin
  Lexer:= CurrentLexer;
  if Lexer<>FAcpLexer then
    UpdateAcp(Lexer);
end;

procedure TfmMain.TBXItemFReopenClick(Sender: TObject);
begin
  acReread.Execute;
end;

procedure TfmMain.ecToggleLineCommentExecute(Sender: TObject);
begin
  DoToggleLineComment(false);
end;

procedure TfmMain.DoToggleLineComment(Alt: boolean);
var
  An: TSyntAnalyzer;
  Ed: TSyntaxMemo;
  sCom: Widestring;
  nLine1, nLine2, n: Integer;
  NeedUncomm: boolean;
begin
  Ed:= CurrentEditor;
  An:= EditorCurrentAnalyzerForPos(Ed, Ed.CaretStrPos);

  //get comment chars
  if An<>nil then
    sCom:= An.LineComment
  else
    sCom:= '';
  if sCom='' then
    begin MsgBeep; Exit; end;

  //get NeedUncomm
  with Ed do
  begin
    if SelLength=0 then
      n:= CaretPos.Y
    else
      n:= StrPosToCaretPos(SelStart).Y;

    if (n>=0) and (n<Lines.Count) then
      NeedUncomm:= SBegin(WideTrim(Lines[n]), sCom)
    else
      begin MsgBeep; Exit; end;
  end;

  //work for: selection is small, 1-line, so better to unselect
  //(to move caret down after toggling comment)
  if Ed.SelLength>0 then
  begin
    Ed.GetSelectedLines(nLine1, nLine2);
    if nLine1=nLine2 then
    begin
      Ed.CaretStrPos:= Ed.SelStart;
      Ed.ResetSelection;
    end;
  end;

  //toggle comment
  if NeedUncomm then
    ecUncommentLines.Execute
  else
  if Alt then
    EditorCommentLinesAlt(Ed, sCom)
  else
    ecCommentLines.Execute;
end;

procedure TfmMain.ecToggleFocusOutputExecute(Sender: TObject);
begin
  if not plOut.Visible then
  begin
    ecShowOut.Execute;
    TabsOut.TabIndex:= Ord(tbOutput);
    if Self.Enabled and ListOut.CanFocus then
      ListOut.SetFocus;
  end
  else
  if ListOut.Focused then
    FocusEditor
  else
  begin
    TabsOut.TabIndex:= Ord(tbOutput);
    if Self.Enabled and ListOut.CanFocus then
      ListOut.SetFocus
  end;
end;

procedure TfmMain.acBackupExecute(Sender: TObject);
var Dest:Widestring;
begin
  with CurrentFrame do
    if (FileName<>'') and (FGetFileSize(FileName)>0) then
    begin
      Dest:= FileName+'.bak';
      if not FFileCopy(FileName, Dest) then
        MsgBakEr(Dest)
      else
        MsgBakOk(Dest)
    end
    else
      MsgBeep;
end;

procedure TfmMain.DoGetCommentProps(const Lexer: string;
  UseDefault: boolean;
  var sStart, sEnd: string; var IsMultiLine: boolean);
const
  DefStart = '(*';
  DefEnd = '*)';
var
  n: Integer;
  s: string;
begin
  sStart:= '';
  sEnd:= '';
  IsMultiLine:= false;

  if (Lexer='') and UseDefault then
    begin sStart:= DefStart; sEnd:= DefEnd; Exit end;

  if UseDefault then
  begin
    sStart:= GetLexerComment(Lexer);
    if sStart<>'' then Exit;
  end;

  s:= DoReadLexersCfg('Comments', Lexer);
  if s='' then
  begin
    IsMultiLine:= true;
    s:= DoReadLexersCfg('CommentsForLines', Lexer);
  end;

  n:= Pos(',', s);
  if (s='') or (n=0) then
  begin
    if UseDefault then
      begin sStart:= DefStart; sEnd:= DefEnd; end
    else
      MsgWarn(WideFormat(DKLangConstW('MNCmt'), [Lexer]), Handle);
    Exit;
  end;

  sStart:= Copy(s, 1, n-1);
  sEnd:= Copy(s, n+1, MaxInt);
end;

procedure TfmMain.ecToggleStreamCommentExecute(Sender: TObject);
var
  Ed: TSyntaxMemo;
  s1, s2, Lexer: string;
  MLine: boolean;
begin
  Ed:= CurrentEditor;
  Lexer:= CurrentLexer;
  if Lexer='' then
    begin MsgBeep; Exit end;

  if Ed.ReadOnly then Exit;
  if Ed.SelLength=0 then
    begin MsgNoSelection; Exit end;

  DoGetCommentProps(Lexer, false, s1, s2, MLine);
  if s1<>'' then
    EditorToggleStreamComment(Ed, s1, s2, MLine);
end;

procedure TfmMain.TBXItemEMoveUpClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smMoveLinesUp);
end;

procedure TfmMain.TBXItemEMoveDnClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smMoveLinesDown);
end;

procedure TfmMain.TBXItemEDelLnClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smDeleteLine);
end;

procedure TfmMain.TBXItemHelpDonateClick(Sender: TObject);
begin
  SynHelpTopic(helpDonate, Handle);
end;

procedure TfmMain.TBXItemECpFNClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CopyFilename);
end;

procedure TfmMain.TBXItemECpFullPathClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CopyFullPath);
end;

procedure TfmMain.TBXItemECpDirPathClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CopyDirPath);
end;

procedure TfmMain.ecToggleFocusFindResExecute(Sender: TObject);
begin
  if not plOut.Visible then
  begin
    ecShowOut.Execute;
    TabsOut.TabIndex:= Ord(tbFindRes);
    if Self.Enabled and TreeFind.CanFocus then
      TreeFind.SetFocus;
  end
  else
  if TreeFind.Focused then
    FocusEditor
  else
  begin
    TabsOut.TabIndex:= Ord(tbFindRes);
    if Self.Enabled and TreeFind.CanFocus then
      TreeFind.SetFocus
  end;
end;

procedure TfmMain.InitSpell;
begin
  {$ifdef SPELL}
  FSpellPos:= -1;
  FSpellChecking:= false;
  FSpell:= TLiveAddictSpellBase.Create(Self);
  with FSpell do
  begin
    ConfigAvailableOptions:= [soUpcase, soNumbers, soAbbreviations, soPrimaryOnly, soDUalCaps];
    ConfigFilename:= SynIniDir + 'SynSpell.ini'; //dupped, maybe Addict can't get it only here?
    ConfigStorage:= csFile; //must be upper
    ConfigDefaultMain.Add('American.adm');
    ConfigDefaultActiveCustom:= 'Default.adu';
    ConfigDictionaryDir.Clear;
    ConfigDictionaryDir.Add(ExtractFileDir(SynIniDir));
    ConfigDictionaryDir.Add(SynDir + 'Dictionaries');
    ConfigFilename:= SynIniDir + 'SynSpell.ini'; //dupped
    SuggestionsLearningDict:= SynIniDir + 'SynSpellLearn.adl';

    DialogInitialPos:= ipUserDefined;
    EndMessage:= emNever;
    UILanguageFontControls.Name:= Self.Font.Name;
    UILanguageFontText.Name:= Self.Font.Name;
    UILanguageUseFonts:= true;

    OnPopupCreateMenu:= SpellPopupCreateMenu;
    OnPopupAddMenuItem:= SpellPopupAddMenuItem;
    OnPopupDoMenu:= SpellPopupDoMenu;
    OnSpellDialogShow:= SpellDialogShow;
    OnPositionDialog:= SpellPositionDialog;
  end;
  {$endif}
end;

procedure TfmMain.DoSpellConfig(Sender: TObject);
begin
  {$ifdef SPELL}
  if Assigned(FSpell) then
    FSpell.Setup;
  {$endif}
end;

procedure TfmMain.UpdateFrameSpell(Frame: TEditorFrame; UpdFlag: boolean = true);
begin
  {$ifdef SPELL}
  if Assigned(FSpell) then
  begin
    if UpdFlag then
      Frame.SpellLive:= opSpellEn and
        ((Frame.FileName = '') or SFileExtensionMatch(Frame.FileName, opSpellExt));
    ecSpellLive.Checked:= Frame.SpellLive;
  end;
  {$endif}
end;

procedure TfmMain.TBXItemBarSpellChkClick(Sender: TObject);
begin
  ecSpellCheck.Execute;
end;

procedure TfmMain.ecSpellCheckExecute(Sender: TObject);
var
  F: TEditorFrame;
  Ed: TSyntaxMemo;
  NStart, NEnd: Integer;
  S, SPrev: string; //Addict is not Unicode aware
  AMap, ASpellLiveBefore: boolean;
begin
  {$ifdef SPELL}
  F:= CurrentFrame;
  if F=nil then Exit;
  if not Assigned(FSpell) then Exit;
  if FSpellChecking then Exit;

  Ed:= F.EditorMaster;
  AMap:= F.ShowMap;
  F.ShowMap:= false;

  //need to set SpellLive
  ASpellLiveBefore:= F.SpellLive;
  if not ASpellLiveBefore then
  begin
    F.SpellLive:= true;
    F.ecSpellChecker.Analyze(false{Background});
  end;

  FSpellPos:= -1;
  FSpellChecking:= true;
  UpdateBusyIco;

  try
    repeat
      if FSpell.DialogForm<>nil then
        FSpell.DialogForm.Close;

      //get next misspelled word
      FSpellPos:= F.DoSpellContinue(FSpellPos+1);
      if FSpellPos<0 then
      begin
        Ed.ResetSelection;
        F.ecSpellChecker.Active:= false;
        F.ecSpellChecker.Active:= true;
        F.SpellLive:= ASpellLiveBefore;
        MsgInfo(DKLangConstW('zMSpellDone'), Handle);
        Exit
      end;

      NStart:= FSpellPos;
      NEnd:= NStart + EditorGetWordLengthForSpellCheck(Ed, NStart);

      if NEnd<=NStart then
        begin MsgBeep; Continue end;
      S:= Copy(Ed.Lines.FText, NStart+1, NEnd-NStart);
      Ed.SetSelection(NStart, NEnd-NStart);

      //process messages
      if (Ed.CaretPos.Y+1) mod 500 = 0 then
        Application.ProcessMessages;
      if Application.Terminated then Exit;

      //show spell dialog
      ////if not FSpell.CheckWord(S) then

      SPrev:= S;
      FSpell.CheckString(S);
      if FSpell.CheckCanceled then
      begin
        Ed.ResetSelection;
        Exit;
      end;
      if S<>SPrev then
        Ed.ReplaceText(NStart, NEnd-NStart, S);
    until false;
  finally
    FSpellChecking:= false;
    UpdateBusyIco;
    F.ShowMap:= AMap;
    if AMap then
      F.DoSyncMicromap;
  end;
  {$endif}
end;

procedure TfmMain.ecSpellLiveExecute(Sender: TObject);
begin
  with CurrentFrame do
    SpellLive:= not SpellLive;
  UpdateFrameSpell(CurrentFrame, false);
end;

procedure TfmMain.TBXItemSpellLiveClick(Sender: TObject);
begin
  ecSpellLive.Execute;
end;

function TfmMain.IsCommandForMacros(Cmd: integer): boolean;
begin
  Result:=
    ((Cmd>=sm_Macro1) and (Cmd<=sm_Macro9)) or
    ((Cmd>=sm_Macro10) and (Cmd<=sm_Macro30)); 
end;

procedure TfmMain.acMacroDialogExecute(Sender: TObject);
var
  keys: TMacroKeysArray;
  idx, i, j: Integer;
  BusyKeys: TStringList;
begin
  for i:= Low(keys) to High(keys) do
    keys[i]:= DoMacro_GetHotkey(i);

  //list of keys which give warning on pressing ok if used in dlg
  BusyKeys:= TStringList.Create;
  for i:= 0 to SyntKeyMapping.Items.Count-1 do
    with SyntKeyMapping.Items[i] do
      if not IsCommandForMacros(Command) then
        for j:= 0 to KeyStrokes.Count-1 do
          BusyKeys.Add(KeyStrokes.Items[j].AsString);

  if DoMacroEditDialog(ecMacroRec, keys, idx, BusyKeys) then
  begin
    if idx >= 0 then
      FLastMacro:= idx;
    for i:= Low(keys) to High(keys) do
      DoMacro_SetHotkey(i, keys[i]);

    UpdateShortcuts;
    SaveMacros;
  end;

  for i:= Low(keys) to High(keys) do
    FreeAndNil(keys[i]);

  FreeAndNil(BusyKeys);
end;

procedure TfmMain.LoadMacros;
var fn: string;
begin
  fn:= SynMacrosIni;
  if FileExists(fn) then
    ecMacroRec.LoadFromFile(fn);
end;

procedure TfmMain.SaveMacros;
begin
  ecMacroRec.SaveToFile(SynMacrosIni);
  PropsManagerKeys.IniFileName:= SynIni;
  PropsManagerKeys.SaveProps;
end;

procedure TfmMain.DoMacro_Run(n: Integer);
begin
  with ecMacroRec do
    if (n>=0) and (n<Count) then
    begin
      FLastMacro:= n;
      Play(n);
    end;
end;

procedure TfmMain.acMacro1Execute(Sender: TObject);
begin
  DoMacro_Run(0);
end;

procedure TfmMain.acMacro2Execute(Sender: TObject);
begin
  DoMacro_Run(1);
end;

procedure TfmMain.acMacro3Execute(Sender: TObject);
begin
  DoMacro_Run(2);
end;

procedure TfmMain.acMacro4Execute(Sender: TObject);
begin
  DoMacro_Run(3);
end;

procedure TfmMain.acMacro5Execute(Sender: TObject);
begin
  DoMacro_Run(4);
end;

procedure TfmMain.acMacro6Execute(Sender: TObject);
begin
  DoMacro_Run(5);
end;

procedure TfmMain.acMacro7Execute(Sender: TObject);
begin
  DoMacro_Run(6);
end;

procedure TfmMain.acMacro8Execute(Sender: TObject);
begin
  DoMacro_Run(7);
end;

procedure TfmMain.acMacro9Execute(Sender: TObject);
begin
  DoMacro_Run(8);
end;

procedure TfmMain.TBXItemMacro1Click(Sender: TObject);
begin
  acMacro1.Execute;
end;

procedure TfmMain.TBXItemMacro2Click(Sender: TObject);
begin
  acMacro2.Execute;
end;

procedure TfmMain.TBXItemMacro3Click(Sender: TObject);
begin
  acMacro3.Execute;
end;

procedure TfmMain.TBXItemMacro4Click(Sender: TObject);
begin
  acMacro4.Execute;
end;

procedure TfmMain.TBXItemMacro5Click(Sender: TObject);
begin
  acMacro5.Execute;
end;

procedure TfmMain.TBXItemMacro6Click(Sender: TObject);
begin
  acMacro6.Execute;
end;

procedure TfmMain.TBXItemMacro7Click(Sender: TObject);
begin
  acMacro7.Execute;
end;

procedure TfmMain.TBXItemMacro8Click(Sender: TObject);
begin
  acMacro8.Execute;
end;

procedure TfmMain.TBXItemMacro9Click(Sender: TObject);
begin
  acMacro9.Execute;
end;

  function TfmMain.DoMacro_GetName(n: integer): Widestring;
  begin
    with ecMacroRec do
      if (n>=0) and (n<Count) and (Macros[n].Name<>'') then
        Result:= '"'+Macros[n].Name+'"'
      else
        Result:= '#'+IntToStr(n+1);
  end;

  function TfmMain.DoMacro_GetCommandName(n: integer; AWithKey: boolean = False): Widestring;
  begin
    Result:= WideFormat(DKLangConstW('macItem'), [DoMacro_GetName(n)]);
    if AWithKey then
      Result:= Result + #9 + GetShortcutTextOfCmd(DoMacro_GetCommandId(n));
  end;

procedure TfmMain.TBXSubmenuMacrosPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  n: Integer;
  en: boolean;
begin
  n:= ecMacroRec.Count;

  en:= (FLastMacro>=0) and (FLastMacro<n);
  TbxItemMacroRepeat.Enabled:= en;
  TbxItemMacroRepeat.Caption:= DKLangConstW('macRep');
  if en then TbxItemMacroRepeat.Caption:=
    TbxItemMacroRepeat.Caption + ' ' + DoMacro_GetName(FLastMacro);

  TbxItemMacro1.Visible:= n>=1;
  TbxItemMacro2.Visible:= n>=2;
  TbxItemMacro3.Visible:= n>=3;
  TbxItemMacro4.Visible:= n>=4;
  TbxItemMacro5.Visible:= n>=5;
  TbxItemMacro6.Visible:= n>=6;
  TbxItemMacro7.Visible:= n>=7;
  TbxItemMacro8.Visible:= n>=8;
  TbxItemMacro9.Visible:= n>=9;
  TbxItemMacro10.Visible:= n>=10;
  TbxItemMacro11.Visible:= n>=11;
  TbxItemMacro12.Visible:= n>=12;
  TbxItemMacro13.Visible:= n>=13;
  TbxItemMacro14.Visible:= n>=14;
  TbxItemMacro15.Visible:= n>=15;
  TbxItemMacro16.Visible:= n>=16;
  TbxItemMacro17.Visible:= n>=17;
  TbxItemMacro18.Visible:= n>=18;
  TbxItemMacro19.Visible:= n>=19;
  TbxItemMacro20.Visible:= n>=20;
  TbxItemMacro21.Visible:= n>=21;
  TbxItemMacro22.Visible:= n>=22;
  TbxItemMacro23.Visible:= n>=23;
  TbxItemMacro24.Visible:= n>=24;
  TbxItemMacro25.Visible:= n>=25;
  TbxItemMacro26.Visible:= n>=26;
  TbxItemMacro27.Visible:= n>=27;
  TbxItemMacro28.Visible:= n>=28;
  TbxItemMacro29.Visible:= n>=29;
  TbxItemMacro30.Visible:= n>=30;

  TbxItemMacro1.Caption:= DoMacro_GetCommandName(0, true);
  TbxItemMacro2.Caption:= DoMacro_GetCommandName(1, true);
  TbxItemMacro3.Caption:= DoMacro_GetCommandName(2, true);
  TbxItemMacro4.Caption:= DoMacro_GetCommandName(3, true);
  TbxItemMacro5.Caption:= DoMacro_GetCommandName(4, true);
  TbxItemMacro6.Caption:= DoMacro_GetCommandName(5, true);
  TbxItemMacro7.Caption:= DoMacro_GetCommandName(6, true);
  TbxItemMacro8.Caption:= DoMacro_GetCommandName(7, true);
  TbxItemMacro9.Caption:= DoMacro_GetCommandName(8, true);
  TbxItemMacro10.Caption:= DoMacro_GetCommandName(9, true);
  TbxItemMacro11.Caption:= DoMacro_GetCommandName(10, true);
  TbxItemMacro12.Caption:= DoMacro_GetCommandName(11, true);
  TbxItemMacro13.Caption:= DoMacro_GetCommandName(12, true);
  TbxItemMacro14.Caption:= DoMacro_GetCommandName(13, true);
  TbxItemMacro15.Caption:= DoMacro_GetCommandName(14, true);
  TbxItemMacro16.Caption:= DoMacro_GetCommandName(15, true);
  TbxItemMacro17.Caption:= DoMacro_GetCommandName(16, true);
  TbxItemMacro18.Caption:= DoMacro_GetCommandName(17, true);
  TbxItemMacro19.Caption:= DoMacro_GetCommandName(18, true);
  TbxItemMacro20.Caption:= DoMacro_GetCommandName(19, true);
  TbxItemMacro21.Caption:= DoMacro_GetCommandName(20, true);
  TbxItemMacro22.Caption:= DoMacro_GetCommandName(21, true);
  TbxItemMacro23.Caption:= DoMacro_GetCommandName(22, true);
  TbxItemMacro24.Caption:= DoMacro_GetCommandName(23, true);
  TbxItemMacro25.Caption:= DoMacro_GetCommandName(24, true);
  TbxItemMacro26.Caption:= DoMacro_GetCommandName(25, true);
  TbxItemMacro27.Caption:= DoMacro_GetCommandName(26, true);
  TbxItemMacro28.Caption:= DoMacro_GetCommandName(27, true);
  TbxItemMacro29.Caption:= DoMacro_GetCommandName(28, true);
  TbxItemMacro30.Caption:= DoMacro_GetCommandName(29, true);
end;


procedure TfmMain.ecBkClearAllExecute(Sender: TObject);
begin
  if MsgConfirm(DKLangConstW('MBk'), Handle) then
  begin
    EditorClearBookmarks(CurrentFrame.EditorMaster);
    EditorClearBookmarks(CurrentFrame.EditorSlave);
    UpdateStatusbar;
    UpdateListBookmarks;
  end;
end;

procedure TfmMain.ecBkToggleExecute(Sender: TObject);
begin
  if CurrentFrame<>nil then
    with CurrentFrame do
      DoBkToggle(CurrentEditor, CurrentEditor.CurrentLine);
  UpdateStatusbar;
  UpdateListBookmarks;
end;

procedure TfmMain.ecBkNextExecute(Sender: TObject);
begin
  DoBkNext(CurrentEditor, true);
end;

procedure TfmMain.ecBkPrevExecute(Sender: TObject);
begin
  DoBkNext(CurrentEditor, false);
end;

procedure TfmMain.DoBkNext(Ed: TSyntaxMemo; Next: boolean);
var
  i, n, nPos: Integer;
  L: TList;
begin
  nPos:= Ed.CaretStrPos;
  L:= TList.Create;
  try
    Screen.Cursor:= crHourGlass;
    try
      EditorGetBookmarksAsSortedList(Ed, L);
    finally
      Screen.Cursor:= crDefault;
    end;

    //find bkmark which is near nPos
    n:= -1;
    if Next then
    begin
      for i:= 0 to L.Count-1 do
        if Ed.Bookmarks[Integer(L[i])] > nPos then
          begin n:= Integer(L[i]); Break end;
    end
    else
    begin
      for i:= L.Count-1 downto 0 do
        if Ed.Bookmarks[Integer(L[i])] < nPos then
          begin n:= Integer(L[i]); Break end;
    end;
    //we're after last bkmark
    if n<0 then
      if L.Count=0 then
      begin
        MsgBeep;
        Exit
      end
      else
      begin
        if Next then
          n:= Integer(L[0])
        else
          n:= Integer(L[L.Count-1]);
      end;
  finally
    FreeAndNil(L);
  end;

  Ed.GotoBookmark(n);
  EditorCenterPos(Ed, true{GotoMode}, opFindOffsetTop);
  //Msg(Inttostr(n));
end;

procedure TfmMain.ecBkInverseExecute(Sender: TObject);
var
  N, i: Integer;
begin
  N:= CurrentEditor.Lines.Count;
  if N=0 then Exit;

  DoProgressShow(proBookmarks);

  with CurrentFrame do
  begin
    EditorMaster.BeginUpdate;
    EditorSlave.BeginUpdate;
    try
      for i:= 0 to N-1 do
      begin
        DoBkToggle(EditorMaster, i);
        if IsProgressStopped(i+1, N) then
          Break;
      end;
    finally
      EditorMaster.EndUpdate;
      EditorSlave.EndUpdate;
    end;
  end;

  UpdateStatusbar;
  UpdateListBookmarks;
  DoProgressHide;
end;

procedure TfmMain.ecBkCopyExecute(Sender: TObject);
var
  List: TWideStringList;
  i: Integer;
begin
  List:= TWideStringList.Create;
  try
    with CurrentEditor do
      for i:= 0 to Lines.Count-1 do
        if BookmarkForLine(i)>=0 then
          List.Add(Lines[i]);

    if List.Count>0 then
      TntClipboard.AsWideText:= List.Text
    else
      MsgBeep;
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmMain.ecBkCutExecute(Sender: TObject);
begin
  ecBkCopy.Execute;
  ecBkDelete.Execute;
end;

procedure TfmMain.ecBkDeleteExecute(Sender: TObject);
begin
  DoBkDelete(CurrentEditor, false);
end;

procedure TfmMain.ecBkDeleteUnmkExecute(Sender: TObject);
begin
  DoBkDelete(CurrentEditor, true);
end;

procedure TfmMain.DoBkDelete(ed: TSyntaxMemo; DelUnmarked: boolean);
var
  N, i, bk, NDel: Integer;
begin
  N:= ed.Lines.Count;
  if N=0 then Exit;

  DoProgressShow(proBookmarks);
  NDel:= 0;

  with ed do
  begin
    BeginUpdate;
    for i:= N-1 downto 0 do
    begin
      if IsProgressStopped(N-i, N) then
        Break;

      bk:= BookmarkForLine(i);
      if (bk>=0) xor DelUnmarked then
      begin
        if bk>=0 then
          Bookmarks[bk]:= -1;
        DoDeleteLine(ed, i);
        Inc(NDel);
      end;
    end;
    EndUpdate;
  end;

  MsgDelLines(NDel);
  DoProgressHide;
  UpdateStatusbar;
  UpdateListBookmarks;
end;

procedure TfmMain.DoDeleteLine(Ed: TSyntaxMemo; NLine: integer;
  ForceUndo: boolean = false);
begin
  EditorDeleteLine(Ed, NLine, opBkUndo or ForceUndo);
end;

procedure TfmMain.DoReplaceLine(Ed: TSyntaxMemo; NLine: integer;
  const S: ecString; ForceUndo: boolean = false);
begin
  EditorReplaceLine(Ed, NLine, S, opBkUndo or ForceUndo);
end;

procedure TfmMain.ecBkPasteExecute(Sender: TObject);
var
  s: Widestring;
  N, i, bk: Integer;
begin
  s:= TntClipboard.AsWideText;
  if s='' then
    begin MsgBeep; Exit end;

  N:= CurrentEditor.Lines.Count;
  if N=0 then Exit;

  DoProgressShow(proBookmarks);

  with CurrentEditor do
  begin
    BeginUpdate;
    for i:= N-1 downto 0 do
    begin
      if IsProgressStopped(N-i, N) then
        Break;

      bk:= BookmarkForLine(i);
      if bk>=0 then
      begin
        DoReplaceLine(CurrentEditor, i, s);
        Bookmarks[bk]:= CaretPosToStrPos(Point(0, i));
      end;
    end;
    EndUpdate;
  end;

  DoProgressHide;
  UpdateStatusbar;
  UpdateListBookmarks;
end;

procedure TfmMain.ecGotoExecute(Sender: TObject);
var
  Ed: TSyntaxMemo;
  oldSelStart, oldSelLength,
  n, m: Integer;
  AExt: boolean;
  AMode: TSynGotoMode;
  ABkNum: integer;
  Pnt: TPoint;
begin
  Ed:= CurrentEditor;

  Pnt:= Ed.LinesPosToLog(Ed.CaretPos);
  n:= Pnt.Y+1;
  m:= Pnt.X+1;
  oldSelStart:= Ed.SelStart;
  oldSelLength:= Ed.SelLength;
  AMode:= goLine;

  if ShowGotoForm(n, m, AExt, AMode, ABkNum) then
  begin
    case AMode of
      goLine:
        begin
          Pnt:= Point(m-1, n-1);
          DoMacro_RecordCommand(smGotoXY, @Pnt);
          Ed.CaretPos:= Ed.LogToLinesPos(Pnt);
        end;
      goPrevBk:
        ecBkPrev.Execute;
      goNextBk:
        ecBkNext.Execute;
      goNumBk:
        Ed.ExecCommand(smGotoBookmark0+ABkNum);
    end;

    if AExt then
      EditorExtendSelectionByPosition(Ed,
        oldSelStart, oldSelLength,
        Ed.CaretStrPos, 0);

    EditorCenterPos(Ed, true{GotoMode}, opFindOffsetTop);
    FocusEditor;
  end;
end;

procedure TfmMain.ecToggleFocusGroupsExecute(Sender: TObject);
begin
  Groups.PagesSetNext(true);
end;

procedure TfmMain.TBXItemOOValClick(Sender: TObject);
begin
  TabsOut.TabIndex:= Ord(tbValidate);
end;

procedure TfmMain.ListValDblClick(Sender: TObject);
begin
  with ListVal do
   if (ItemIndex>=0) and (ItemIndex<Items.Count) then
     DoNavigate_ListVal(Items[ItemIndex]);
end;

function TfmMain.DoNavigate_ListVal(const Str: Widestring): boolean;
var
  fn: Widestring;
  nLine, nCol: Integer;
begin
  Result:= false;
  if Str='' then Exit;

  fn:= SynPanelPropsVal.DefFilename;
  SParseOut(Str,
    SynPanelPropsVal.RegexStr,
    SynPanelPropsVal.RegexIdName,
    SynPanelPropsVal.RegexIdLine,
    SynPanelPropsVal.RegexIdCol,
    SynPanelPropsVal.ZeroBase,
    fn, nLine, nCol);

  if fn='' then Exit;
  if nLine<1 then Exit;
  if nCol<1 then nCol:= 1;

  Result:= true;
  if not IsFileExist(fn) then Exit;
  DoOpenFile(fn);
  CurrentEditor.CaretPos:= Point(nCol-1, nLine-1);
  FocusEditor;
end;

procedure TfmMain.TBXItemValNavClick(Sender: TObject);
begin
  ListValDblClick(ListVal);
end;

procedure TfmMain.TBXItemValCopySelClick(Sender: TObject);
begin
  DoListCopy(ListVal);
end;

procedure TfmMain.TBXItemValCopyAllClick(Sender: TObject);
begin
  DoListCopyAll(ListVal);
end;

procedure TfmMain.TBXItemValClearClick(Sender: TObject);
begin
  ListVal.Items.Clear;
  FocusEditor;
  UpdateStatusbar;
end;

procedure TfmMain.PopupValidatePopup(Sender: TObject);
begin
  with ListVal do
  begin
    TbxItemValNav.Enabled:= (ItemIndex>=0) and (ItemIndex<=Items.Count-1);
    TbxItemValCopySel.Enabled:= SelCount>0;
    TbxItemValCopyAll.Enabled:= Items.Count>0;
    TbxItemValClear.Enabled:= Items.Count>0;
    TbxItemValFind.Enabled:= Items.Count>0;
  end;
end;

procedure TfmMain.ListValKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key=vk_space) or (Key=vk_return)) and (Shift=[]) then
  begin
    ListValDblClick(Self);
    Key:= 0;
    Exit
  end;
  if (Key=vk_delete) and (Shift=[]) then
  begin
    TbxItemValClearClick(Self);
    Key:= 0;
    Exit
  end;
  if (Key=Ord('C')) and (Shift=[ssCtrl]) then
  begin
    TbxItemValCopySelClick(Self);
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(Shortcut(Key, Shift), smFindDialog) then
  begin
    TBXItemValFindClick(Self);
    Key:= 0;
    Exit
  end;
  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.ecToggleFocusValidateExecute(Sender: TObject);
begin
  if not plOut.Visible then
  begin
    ecShowOut.Execute;
    TabsOut.TabIndex:= Ord(tbValidate);
    if Self.Enabled and ListVal.CanFocus then
      ListVal.SetFocus;
  end
  else
  if ListVal.Focused then
    FocusEditor
  else
  begin
    TabsOut.TabIndex:= Ord(tbValidate);
    if Self.Enabled and ListVal.CanFocus then
      ListVal.SetFocus
  end;
end;

procedure TfmMain.ecReduceBlanksExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdRemoveDupBlanks);
end;


procedure TfmMain.TBXItemSessClrClick(Sender: TObject);
begin
  SynMruSessions.Items.Clear;
  with TIniFile.Create(SynHistoryIni) do
  try
    EraseSection('MRU_Sess');
  finally
    Free;
  end;
end;

procedure TfmMain.TBXItemFSesSaveClick(Sender: TObject);
begin
  DoSaveSession;
end;

procedure TfmMain.DoSaveSession;
begin
  if FSessionFN<>'' then
    DoSaveSessionToFile(FSessionFN)
  else
    DoSaveSessionAs;
end;

procedure TfmMain.TBXItemFSesCloseClick(Sender: TObject);
begin
  DoCloseSession(true);
end;

procedure TfmMain.DoCloseSession(PromptToSave: boolean);
begin
  if PromptToSave then
    if not DoConfirmSaveSession(true) then
      Exit;
  FSessionFN:= '';
  UpdateTitle(CurrentFrame);
end;

function TfmMain.DoConfirmSaveSession(CanCancel: boolean; ExitCmd: boolean = false): boolean;
var
  sName, fn: WideString;
begin
  Result:= true;

  //don't show messages if all tabs closed
  if (FrameAllCount=1) and
    (Frames[0].FileName='') and
    (not Frames[0].Modified) then
  begin
    Exit
  end;  

  if ExitCmd and opHistSessionSave then
  begin
    //save last session (named of default)
    if (FSessionFN='') or opHistSessionDef then
      fn:= SynIniDir + SynDefaultSyn //default
    else
      fn:= FSessionFN; //named
    DoSaveSessionToFile(fn);
    SynMruSessions.AddItem(fn);
    Exit
  end;

  //save last named session
  if FSessionFN='' then Exit;
  if opHistSessionSave then
  begin
    DoSaveSessionToFile(FSessionFN);
    SynMruSessions.AddItem(FSessionFN);
    Exit
  end;

  //ask to save current named session
  sName:= WideChangeFileExt(WideExtractFileName(FSessionFN), '');

  case MsgConfirmYesNoCancel(
         WideFormat(DKLangConstW('MSessSav'), [sName]),
         Handle, CanCancel) of
    id_yes:
      begin
        DoSaveSessionToFile(FSessionFN);
        SynMruSessions.AddItem(FSessionFN);
        Result:= true;
      end;
    id_no:
      Result:= true;
    else
      Result:= false;
  end;
end;

procedure TfmMain.ecRemoveBlanksExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdRemoveBlanks);
end;

{
http://delphi.about.com/cs/adptips2004/a/bltip0304_3.htm
}
procedure TfmMain.TBXSubmenuItemFNewPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  UpdateNewDocMenu;
end;

procedure TfmMain.UpdateNewDocMenu();
var
  i: Integer;
  mi: TSpTbxItem;
  miSub: TSpTbxSubmenuItem;
  Names: TStringList;
  ch: char;
  NeedSub: boolean;
  Ini: TIniFile;
const
  chSet = ['A'..'Z', '?'];
begin
  FListNewDocs.Clear;
  FFindToList(FListNewDocs,
    SynDataSubdir(cSynDataNewDoc),
    '*.*',
    '',
    false{SubDirs},
    false, false, false);

  Names:= TStringList.Create;
  for i:= 0 to FListNewDocs.Count-1 do
    Names.AddObject(SNewDocName(FListNewDocs[i]), Pointer(i));
  Names.Sort;

  //fill MRU items
  Ini:= TIniFile.Create(SynHistoryIni);
  try
    LoadMruList(SynMruNewdoc, Ini, 'MRU_Newdoc', 5{MaxCount}, true);
  finally
    FreeAndNil(Ini);
  end;

  with TBXMRUListItemFNew do
  begin
    Clear;
    for i:= SynMruNewdoc.Items.Count-1 downto 0 do
      MRUAdd(SynMruNewdoc.Items[i]);
  end;

  //del items
  with TbxSubmenuItemFNew do
    for i:= Count-1 downto 0 do
      if Items[i].Tag>0 then
        Items[i].Free;

  //add items
  for ch:= Low(ch) to High(ch) do
    if ch in chSet then
    begin
      NeedSub:= false;
      for i:= 0 to Names.Count-1 do
        if UpCase(Names[i][1]) = ch then
          begin NeedSub:= true; Break end;
      if not NeedSub then Continue;

      //add submenu
      miSub:= TSpTbxSubmenuItem.Create(Self);
      miSub.Caption:= ch;
      miSub.Tag:= 1;

      TbxSubmenuItemFNew.Add(miSub);
      //fill submenu
      for i:= 0 to Names.Count-1 do
        if UpCase(Names[i][1]) = ch then
        begin
          mi:= TSpTbxItem.Create(Self);
          mi.OnClick:= DoNewDocClick;
          mi.Caption:= Names[i];
          mi.Tag:= Integer(Names.Objects[i]) + 1;
          miSub.Add(mi);
        end;
    end;

  //add "Browse folder"
  TbxSubmenuItemFNew.Add(TSpTbxSeparatorItem.Create(Self));
  mi:= TSpTbxItem.Create(Self);
  mi.OnClick:= DoNewDocFolderClick;
  mi.Caption:= DKLangConstW('OpNew');
  mi.Tag:= 1;
  TbxSubmenuItemFNew.Add(mi);

  //fix too big menuitems height
  FixMruBigImageList(TBXMRUListItemFNew);
  FixMenuBigImageList(TbxSubmenuItemFNew);

  FreeAndNil(Names);
end;

procedure TfmMain.FixMenuBigImageList(Menu: TSpTbxSubmenuItem);
var
  i: Integer;
begin
  with Menu do
    for i:= 0 to Count-1 do
      Items[i].Images:= ImageListStatus;
end;

procedure TfmMain.FixMruBigImageList(Menu: TSpTbxMruListItem);
var
  i: Integer;
begin
  with Menu do
    for i:= 0 to Count-1 do
      Items[i].Images:= ImageListStatus;
end;

procedure TfmMain.DoNewDocFolderClick(Sender: TObject);
begin
  FOpenURL(SynDataSubdir(cSynDataNewDoc), Handle);
end;

procedure TfmMain.DoNewDocClick(Sender: TObject);
var
  n: Integer;
begin
  n:= (Sender as TComponent).Tag - 1;
  if (n>=0) and (n<=FListNewDocs.Count-1) then
    DoNewDoc(FListNewDocs[n])
  else
    MsgBeep(true);
end;

procedure TfmMain.ecRemoveLinesExecute(Sender: TObject);
var
  NDel: integer;
begin
  NDel:= EditorDeleteSelectedLines(CurrentEditor);
  MsgDelLines(NDel);
end;

procedure TfmMain.ecTrimLeadExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdTrimLead);
end;

procedure TfmMain.ecTrimTrailExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdTrimTrail);
end;

procedure TfmMain.ecTrimAllExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdTrimAll);
end;

procedure TfmMain.ecRemoveDupSpacesExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdRemoveDupSpaces);
end;

procedure TfmMain.ecTabToSpExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdUntab);
  {
  //code from EC demo:
  with CurrentEditor do
    if (SelectMode in [msLine, msNormal]) and HaveSelection then
      UnTabText(SelStart + 1, SelStart + SelLength)
    else
      UnTabText;
      }
end;

procedure TfmMain.ecSpToTabExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdSpacesToTabs);
end;

procedure TfmMain.DoFind_ClipboardText(ANext: boolean);
var
  s: Widestring;
begin
  s:= DoClipItem;
  if s='' then
    s:= TntClipboard.AsWideText;
  if s='' then
    begin MsgBeep; Exit end;

  SDeleteFromW(s, #13);
  SDeleteFromW(s, #10);

  DoHint('');
  Finder.FindText:= s;
  Finder.Flags:= Finder.Flags-[ftRegex];
  Finder.Flags:= Finder.Flags+[ftWrapSearch];

  if ANext then
    Finder.FindNext
  else
    Finder.FindPrev;
end;

procedure TfmMain.ecFindClipNextExecute(Sender: TObject);
begin
  DoFind_ClipboardText(true);
end;

procedure TfmMain.ecFindClipPrevExecute(Sender: TObject);
begin
  DoFind_ClipboardText(false);
end;

type
  TMacroHk = class(TecMacroRecorder);

//Add Cmd to currently recorded macro (if not done by EC)
procedure TfmMain.DoMacro_RecordCommand(Cmd: integer; Data: PWChar);
var
  h: boolean;
begin
  if ecMacroRec.Recording then
    TMacroHk(ecMacroRec).BeforeCommand(Cmd, Data, h);
end;

procedure TfmMain.ecSplit50_50Execute(Sender: TObject);
begin
  Groups.SplitPos:= 50;
end;

procedure TfmMain.ecSplit40_60Execute(Sender: TObject);
begin
  Groups.SplitPos:= 40;
end;

procedure TfmMain.ecSplit60_40Execute(Sender: TObject);
begin
  Groups.SplitPos:= 60;
end;

procedure TfmMain.ecSplit30_70Execute(Sender: TObject);
begin
  Groups.SplitPos:= 30;
end;

procedure TfmMain.ecSplit70_30Execute(Sender: TObject);
begin
  Groups.SplitPos:= 70;
end;

procedure TfmMain.ecSplit20_80Execute(Sender: TObject);
begin
  Groups.SplitPos:= 20;
end;

procedure TfmMain.ecSplit80_20Execute(Sender: TObject);
begin
  Groups.SplitPos:= 80;
end;

{
http://www.addictive-software.com/addict3/other-downloads.htm
Menu demo
}
procedure TfmMain.SpellPopupCreateMenu(Sender: TObject;
  Owner: TComponent; var PopupMenu: TObject);
var
  AMenu: TSpTbxPopupMenu;
  AMenuItem: TSpTbxItem;
begin
  AMenu:= TSpTbxPopupMenu.Create(Owner);
	AMenu.AutoHotkeys:= maManual;
	PopupMenu:= AMenu;

  //info item
	AMenuItem:= TSpTbxItem.Create(AMenu);
	AMenuItem.Caption:= DKLangConstW('zMSpell');
  AMenuItem.Enabled:= false;
	AMenu.Items.Add(AMenuItem);
  //cut
	AMenuItem:= TSpTbxItem.Create(AMenu);
	AMenuItem.Caption:= TbxItemCtxCut.Caption;
  AMenuItem.Enabled:= CurrentEditor.HaveSelection;
  AMenuItem.OnClick:= SpellCutClick;
	AMenu.Items.Add(AMenuItem);
  //copy
	AMenuItem:= TSpTbxItem.Create(AMenu);
	AMenuItem.Caption:= TbxItemCtxCopy.Caption;
  AMenuItem.Enabled:= CurrentEditor.HaveSelection;
  AMenuItem.OnClick:= SpellCopyClick;
	AMenu.Items.Add(AMenuItem);
  //paste
	AMenuItem:= TSpTbxItem.Create(AMenu);
	AMenuItem.Caption:= TbxItemCtxPaste.Caption;
  AMenuItem.OnClick:= SpellPasteClick;
	AMenu.Items.Add(AMenuItem);
  //sep
  AMenu.Items.Add(TSpTbxSeparatorItem.Create(AMenu));
end;

//same Menu demo
procedure TfmMain.SpellPopupDoMenu(Sender, Menu: TObject;
  XPos, YPos: Integer; var PopupAction: Integer; var PopupWord: string);
begin
  TSpTbxPopupMenu(Menu).Popup(XPos, YPos);
  Application.ProcessMessages;

  PopupWord:= FSpellMenuCaption;
  PopupAction:= FSpellMenuTag;
end;


procedure TfmMain.SpellItemClick(Sender: TObject);
begin
  FSpellMenuCaption:= (Sender as TSpTbxItem).Caption;
  FSpellMenuTag:= (Sender as TComponent).Tag;
end;

procedure TfmMain.SpellCopyClick(Sender: TObject);
begin
  ecCopy.Execute;
end;

procedure TfmMain.SpellCutClick(Sender: TObject);
begin
  ecCut.Execute;
end;

procedure TfmMain.SpellPasteClick(Sender: TObject);
begin
  ecPaste.Execute;
end;

procedure TfmMain.SpellPopupAddMenuItem(
  Sender, Menu, SubMenu: TObject;
  Caption: string; Enable, HasChildren: Boolean;
  Tag: Integer; var MenuItem: TObject);
var
  vMenuItem: TTBCustomItem;
begin
  FSpellMenuTag:= 0;
  if HasChildren then
  begin
    vMenuItem:= TSpTbxItem.Create(TComponent(Menu));
    TSpTbxPopupMenu (Menu).Items.Add (vMenuItem);
    (vMenuItem as TSpTbxItem).Caption:= Caption;
    vMenuItem.Enabled:= Enabled;
    vMenuItem.Tag:= Tag;
    MenuItem:= vMenuItem;
  end
  else
  begin
    if Assigned (SubMenu) then
    begin
      vMenuItem:= TSpTbxItem.Create(TComponent(SubMenu));
      TSpTbxItem (SubMenu).Add(vMenuItem);
    end
    else
    begin
      if Caption='-' then
        vMenuItem:= TSpTbxSeparatorItem.Create(TComponent(Menu))
      else
        vMenuItem:= TSpTbxItem.Create(TComponent(Menu));
      TSpTbxPopupMenu(Menu).Items.Add(vMenuItem);
    end;

    if (vMenuItem is TSpTbxItem) then
      (vMenuItem as TSpTbxItem).Caption:= Caption;
    vMenuItem.Enabled:= Enabled;
    if (Tag > 0) then
    begin
      vMenuItem.Tag:= Tag;
      vMenuItem.OnClick:= SpellItemClick;
    end;
    MenuItem:= vMenuItem;
  end;
end;


procedure TfmMain.acMacroPlayBeforeExecute(Sender: TObject);
begin
  FLastMacro:= ecMacroRec.Count-1;
end;

procedure TfmMain.acMacroRepeatExecute(Sender: TObject);
begin
  DoMacro_Run(FLastMacro);
end;

procedure TfmMain.ecRepeatCmdExecute(Sender: TObject);
var i: integer;
begin
  if FLastCmdId > 0 then
  begin
    FLastCmdPlaying:= true;
    for i:= 0 to FLastCmdCount{=Count-1} do
      CurrentEditor.ExecCommand(FLastCmdId, PChar(FLastCmdData));
    FLastCmdPlaying:= false;
  end;
end;

function TfmMain.FCanUseLexer(const fn: Widestring): boolean;
begin
  Result:= FGetFileSize(fn) <= opBigSize * 1024 * 1024;
end;

procedure TfmMain.DoFixReplaceCaret(Ed: TSyntaxMemo);
var
  n, nf: Integer;
begin
  with Ed do
  begin
    n:= SelStart;
    nf:= SelLength;
    if ftBackward in Finder.Flags then
      CaretStrPos:= n+nf
    else
      CaretStrPos:= n;
    SetSelection(n, nf, true);
  end;
end;

procedure TfmMain.TBXItemRightClipClick(Sender: TObject);
begin
  TabsRight.TabIndex:= Ord(tbClipbd);
end;

procedure TfmMain.TBXItemRightMapClick(Sender: TObject);
begin
  TabsRight.TabIndex:= Ord(tbMinimap);
end;

procedure TfmMain.MapClick(Sender: TObject);
var
  Ed: TSyntaxMemo;
  P: TPoint;
begin
  Ed:= CurrentEditor;
  if Ed<>nil then
  begin
    P:= (Sender as TSyntaxMemo).CaretPos;

    EditorUncollapseLine(Ed, P.Y);
    //Ed.CaretPos:= P; //don't move caret
    Ed.TopLine:= P.Y - Ed.VisibleLines div 2;
  end;
end;

procedure TfmMain.ApplyMinimapProps;
begin
  if Assigned(fmMap) then
    with fmMap do
    begin
      SetMapFontSize(opMinimapFontSize);
      SetMapColor(opColorMinimapSel);
    end;
end;

procedure TfmMain.SyncMapData;
begin
  if plClip.Visible then
    if Assigned(fmMap) and fmMap.Visible then
      fmMap.SyncMapData(CurrentEditor);
end;

procedure TfmMain.SyncMapPos;
begin
  if plClip.Visible then
    if Assigned(fmMap) and fmMap.Visible then
      fmMap.SyncMapPos(CurrentEditor);
end;

procedure TfmMain.SynChange(Sender: TObject);
begin
  SyncMapPos;
end;

procedure TfmMain.ecToggleFocusMapExecute(Sender: TObject);
begin
  if not plClip.Visible then
  begin
    ecShowClip.Execute;
    TabsRight.TabIndex:= Ord(tbMinimap);
    if Assigned(fmMap) and fmMap.edMap.CanFocus then
      fmMap.edMap.SetFocus;
  end
  else
  if Assigned(fmMap) and fmMap.edMap.Focused then
    FocusEditor
  else
  begin
    TabsRight.TabIndex:= Ord(tbMinimap);
    if Assigned(fmMap) and fmMap.edMap.CanFocus then
      fmMap.edMap.SetFocus
  end;
end;

procedure TfmMain.FinderTree_OnFind;
begin
  if Sender is TFinderInTree then
    with Sender as TFinderInTree do
      if Control.CanFocus then
        Control.SetFocus;
end;

procedure TfmMain.FinderTree_OnNotFound(Sender: TObject);
var
  s: Widestring;
begin
  if Sender is TFinderInTree then
    s:= (Sender as TFinderInTree).FindText
  else
  if Sender is TFinderInList then
    s:= (Sender as TFinderInList).FindText
  else
    s:= '';
  s:= WideFormat(DKLangConstW('MNFound2'), [s]);
  DoHint(s);
  MsgBeep;
end;

procedure TfmMain.ecFindInTreeExecute(Sender: TObject);
begin
  if Assigned(fmSR) and fmSR.Visible and not fmSR.IsDocked then
    fmSR.Hide;
  if not Assigned(FinderInTree) then
    FinderInTree:= TFinderInTree.Create(Self);
  with FinderInTree do
  begin
    IniFN:= Self.SynHistoryIni;
    if Assigned(fmSR) then
      SearchText:= fmSR.Text1;
    Control:= Self.CurrentTreeview;
    OnFind:= Self.FinderTree_OnFind;
    OnNotFound:= Self.FinderTree_OnNotFound;
    Execute;
  end;
end;

procedure TfmMain.ecFindInTreeNextExecute(Sender: TObject);
begin
  if not Assigned(FinderInTree) or
    (FinderInTree.FindText='') then
    begin ecFindInTree.Execute; Exit end;
  FinderInTree.FindNext;
end;

procedure TfmMain.ecFindInTreePrevExecute(Sender: TObject);
begin
  if not Assigned(FinderInTree) or
    (FinderInTree.FindText='') then
    begin ecFindInTree.Execute; Exit end;
  FinderInTree.FindPrev;
end;

procedure TfmMain.DoTreeJump(Mode: TSynGotoTree);
var
  tn, tn2: TTreeNode;
begin
  with Tree do
    if Selected<>nil then
    begin
      case Mode of
        tgoNext:
          tn:= Selected.GetNext;
        tgoPrev:
          tn:= Selected.GetPrev;
        tgoParent:
          tn:= Selected.Parent;
        tgoNextBro:
          begin
            tn:= Selected.GetNextSibling;
            tn2:= Selected;
            if tn=nil then
              repeat
                tn2:= tn2.Parent;
                if tn2=nil then Break;
                tn:= tn2.GetNextSibling;
                if tn<>nil then Break;
              until false;
          end;
        tgoPrevBro:
          begin
            tn:= Selected.GetPrevSibling;
            if tn=nil then
              tn:= Selected.Parent;
          end;
        else tn:= nil;
      end;
      if tn<>nil then
      begin
        Selected:= tn;
        Tree.MemoCaretUpdate(True);
      end
      else
        MsgBeep;
    end;
end;

procedure TfmMain.ecTreeNextExecute(Sender: TObject);
begin
  DoTreeJump(tgoNext);
end;

procedure TfmMain.ecTreePrevExecute(Sender: TObject);
begin
  DoTreeJump(tgoPrev);
end;

{
function TfmMain.IsSearchEditFocused: boolean;
begin
  Result:= Assigned(fmSR) and
    fmSR.Visible and
    fmSR.Enabled and
    (fmSR.ed1.Focused or fmSR.ed2.Focused);
end;
}

{
function TfmMain.IsNumConvEditFocused: boolean;
begin
  Result:= Assigned(fmNumConv) and
    fmNumConv.Visible and
    fmNumConv.Enabled and
    (fmNumConv.ActiveControl is TTntEdit) and
    fmNumConv.ActiveControl.Focused;
end;
}

procedure TfmMain.ecSplitLeftExecute(Sender: TObject);
begin
  Groups.SplitPosDecrease;
end;

procedure TfmMain.ecSplitRightExecute(Sender: TObject);
begin
  Groups.SplitPosIncrease;
end;

function TfmMain.ShowGotoForm(
  var ALine, ACol: integer;
  var AExtSel: boolean;
  var AMode: TSynGotoMode;
  var ABkNum: integer): Boolean;
var
  i: integer;
  en: boolean;
  s: string;
begin
  with TfmGoto.Create(nil) do
   try
     PanelPos.Visible:= AMode=goLine;
     PanelBookmk.Visible:= not PanelPos.Visible;
     case AMode of
       goPrevBk: cbPrev.Checked:= true;
       goNextBk: cbNext.Checked:= true;
       goNumBk: cbNum.Checked:= true;
     end;

     edLine.Text:= IntToStr(ALine);
     edCol.Text:= IntToStr(ACol);
     FMaxLine:= CurrentEditor.Lines.Count;

     //init list of numbered bookmarks
     edNum.Items.Clear;
     for i:= 0 to 9 do
     begin
       edNum.Items.Add(EditorGetBookmarkDesc(CurrentEditor, i));
       FBookSet[i]:= CurrentEditor.Bookmarks[i]>=0;
     end;

     //numbered bookmarks enabled?
     en:= false;
     edNum.ItemIndex:= 0;
     for i:= 0 to 9 do
       if FBookSet[i] then
         begin en:= true; edNum.ItemIndex:= i; Break end;
     cbNum.Enabled:= en;
     edNum.Enabled:= en;

     //any bookmarks enabled?
     en:= CurrentEditor.BookmarkObj.Count>0;
     cbPrev.Enabled:= en;
     cbNext.Enabled:= en;
     labBookmk.Enabled:= en;

     Result:= ShowModal = mrOk;
     if Result then
     begin
       if PanelPos.Visible then AMode:= goLine else
       if cbPrev.Checked then AMode:= goPrevBk else
       if cbNext.Checked then AMode:= goNextBk else
       if cbNum.Checked then AMode:= goNumBk;

       //calc line number
       //limit by [1, Count]
       s:= edLine.Text;
       s:= Trim(s);
       if (s<>'') and (char(s[1]) in ['+', '-']) then
         ALine:= ALine+StrToIntDef(s, 0)
       else
         ALine:= StrToIntDef(s, ALine);
       ALine:= Min2(Max2(ALine, 1), CurrentEditor.Lines.Count);

       //calc column number
       //limit by [1, inf]
       s:= edCol.Text;
       s:= Trim(s);
       if (s<>'') and (char(s[1]) in ['+', '-']) then
         ACol:= ACol+StrToIntDef(s, 0)
       else
         ACol:= StrToIntDef(s, ACol);
       ACol:= Max2(ACol, 1);

       AExtSel:= cbExtSel.Checked;
       ABkNum:= edNum.ItemIndex;
     end;
   finally
     Free;
   end;
end;


procedure TfmMain.DoFind_AndExtendSel(ANext: boolean);
var
  Ed: TSyntaxMemo;
  oldStart, oldLength: Integer;
begin
  DoFinderInit;
  if Finder.FindText='' then
  begin
    ecFind.Execute;
    Exit
  end;

  Ed:= CurrentEditor;
  oldStart:= Ed.SelStart;
  oldLength:= Ed.SelLength;
  if ANext then
    Finder.FindNext
  else
    Finder.FindPrev;
  if Finder.Matches>0 then
    EditorExtendSelectionByPosition(Ed,
      oldStart, oldLength,
      Ed.SelStart, Ed.SelLength);
end;

procedure TfmMain.ecFindNextWithExtendExecute(Sender: TObject);
begin
  DoFind_AndExtendSel(true);
end;

procedure TfmMain.ecFindPrevWithExtendExecute(Sender: TObject);
begin
  DoFind_AndExtendSel(false);
end;

procedure TfmMain.UpdateFrameZoom(F: TEditorFrame);
begin
  //update ruler height
  with F do
  begin
    EditorMasterZoom(EditorMaster);
    EditorMasterZoom(EditorSlave);
  end;
end;

procedure TfmMain.ecFindInListExecute(Sender: TObject);
begin
  if Assigned(fmSR) and fmSR.Visible and not fmSR.IsDocked then
    fmSR.Hide;
  if not Assigned(FinderInList) then
    FinderInList:= TFinderInList.Create(Self);
  with FinderInList do
  begin
    IniFN:= Self.SynHistoryIni;
    if Assigned(fmSR) then
      SearchText:= fmSR.Text1;
    Control:= CurrentListbox;
    OnNotFound:= Self.FinderTree_OnNotFound;
    Execute;
  end;
end;

procedure TfmMain.ecFindInListNextExecute(Sender: TObject);
begin
  if not Assigned(FinderInList) or
    (FinderInList.FindText='') then
    begin ecFindInList.Execute; Exit end;
  FinderInList.Control:= CurrentListbox;
  FinderInList.FindNext;
end;

procedure TfmMain.ecFindInListPrevExecute(Sender: TObject);
begin
  if not Assigned(FinderInList) or
    (FinderInList.FindText='') then
    begin ecFindInList.Execute; Exit end;
  FinderInList.Control:= CurrentListbox;
  FinderInList.FindPrev;
end;

function TfmMain.CurrentListbox: TCustomListbox;
begin
  if ListOut.Focused then Result:= ListOut
  else
  if ListVal.Focused then Result:= ListVal
  else
  if ListPLog.Focused then Result:= ListPLog
  else
  if Assigned(fmClip) and fmClip.Visible and fmClip.ListClip.Focused then
    Result:= fmClip.ListClip
  else
    Result:= nil;
end;

function TfmMain.CurrentTreeview: TCustomTreeView;
begin
  if Tree.Focused then Result:= Tree
  else
  if TreeFind.Focused then Result:= TreeFind
  else
    Result:= nil;
end;

function TfmMain.IsListboxFocused: boolean;
begin
  Result:= CurrentListbox <> nil;
end;

function TfmMain.IsTreeviewFocused: boolean;
begin
  Result:= CurrentTreeview <> nil;
end;

procedure TfmMain.TBXItemClipFindClick(Sender: TObject);
begin
  DoFind_InClipPanel;
end;

procedure TfmMain.DoFind_InClipPanel;
begin
  if Assigned(fmClip) then
    with fmClip.ListClip do
      if CanFocus then
      begin
        SetFocus;
        ecFindInList.Execute;
      end;
end;

procedure TfmMain.TBXItemValFindClick(Sender: TObject);
begin
  DoFind_InValidatePanel;
end;

procedure TfmMain.DoFind_InValidatePanel;
begin
  with ListVal do
    if CanFocus then
    begin
      SetFocus;
      ecFindInList.Execute;
    end;
end;

procedure TfmMain.TBXItemOutFindClick(Sender: TObject);
begin
  DoFind_InOutputPanel;
end;

procedure TfmMain.DoFind_InOutputPanel;
begin
  with ListOut do
    if CanFocus then
    begin
      SetFocus;
      ecFindInList.Execute;
    end;
end;

procedure TfmMain.PopupClipPopup(Sender: TObject);
begin
  with fmClip.ListClip do
  begin
    TbxItemClipCopyToEd.Enabled:= ItemIndex>=0;
    TbxItemClipCopyToClip.Enabled:= ItemIndex>=0;
    TbxItemClipDeleteSel.Enabled:= ItemIndex>=0;
    TbxItemClipDeleteAll.Enabled:= Items.Count>0;
    TbxItemClipFind.Enabled:= Items.Count>0;
  end;
end;

procedure TfmMain.TBXItemTreeFindClick(Sender: TObject);
begin
  with Tree do
    if CanFocus then
    begin
      SetFocus;
      ecFindInTree.Execute;
    end;
end;

procedure TfmMain.PopupTreePopup(Sender: TObject);
var
  en: boolean;
begin
  with Tree do
  begin
    en:= Items.Count>0;
    TbxItemTreeCollapse.Enabled:= en;
    TbxItemTreeCollapseAll.Enabled:= en;
    TbxItemTreeExpand.Enabled:= en;
    TbxItemTreeExpandAll.Enabled:= en;
    TBXSubmenuTreeLevel.Enabled:= en;
    TbxItemTreeFind.Enabled:= en;
    TbxItemTreeSorted.Checked:= SortType <> stNone;
  end;
end;

procedure TfmMain.TBXItemTreeCollapseClick(Sender: TObject);
begin
  with Tree do
    if Selected<>nil then
      Selected.Collapse(true);
end;

procedure TfmMain.TBXItemTreeExpandClick(Sender: TObject);
begin
  with Tree do
    if Selected<>nil then
    begin
      Selected.Expand(true);
      Selected.MakeVisible;
    end;
end;

procedure TfmMain.ApplyAutoSave;
begin
  with TimerAutoSave do
  begin
    Enabled:= opASaveOnTimer;
    Interval:= 60 * 1000 * Max2(opASaveTimeMin, 1);
  end;
end;

procedure TfmMain.TimerAutoSaveTimer(Sender: TObject);
begin
  DoAutoSave;
end;

procedure TfmMain.DoAutoSave;
  //
  function SizeOk(F: TEditorFrame): boolean;
  begin
    Result:=
      (opASaveMaxSizeKb = 0) or
      (F.EditorMaster.TextSource.Lines.TextLength <= opASaveMaxSizeKb * 1024);
  end;
  //
  function FrameOk(F: TEditorFrame): boolean;
  begin
    Result:= (F<>nil) and
      F.Modified and
      ((F.FileName<>'') or (opASaveUnnamed<>cAutoSaveIgnore)) and
      SizeOk(F);
  end;
  //
  procedure DoSave(F: TEditorFrame);
  var
    dir, ext, fn: Widestring;
  begin
    if F.FileName<>'' then
      SaveFrame(F, false)
    else
    if opASaveUnnamed=cAutoSaveIgnore then
      Exit
    else
    if opASaveUnnamed=cAutoSavePromptFN then
      SaveFrame(F, true)
    else
    if opASaveUnnamed=cAutoSaveSaveToDir then
    begin
      //get save dir
      dir:= SExpandVars(opASaveUnnamedDir);
      SReplaceW(dir, '%AppData%\', FAppDataPath); //force %AppData% expand
      if not IsDirExist(dir) then
        ForceDirectories(dir);

      //get save extention
      if F.EditorMaster.TextSource.SyntaxAnalyzer<>nil then
        ext:= F.EditorMaster.TextSource.SyntaxAnalyzer.Extentions
      else
        ext:= 'txt';
      SDeleteFromW(ext, ' ');

      //save
      fn:= FFreeFN(DKLangConstW('newFnLatin'), ext, dir);
      //msg('"'+fn+'"');
      F.FileName:= fn;
      SaveFrame(F, false);
      UpdateTitle(F);
      SynMruFiles.AddItem(fn);
    end;
  end;
var
  i: integer;
  F: TEditorFrame;
begin
  if opASaveAllFiles then
  begin
    for i:= 0 to FrameAllCount-1 do
    begin
      F:= FramesAll[i];
      if FrameOk(F) then
        DoSave(F);
    end;
  end
  else
  begin
    F:= CurrentFrame;
    if FrameOk(F) then
      DoSave(F);
  end;
end;

procedure TfmMain.UpdateMacroKeynames;
  procedure KeyN(cmd: integer; const NewName: string);
  var i: integer;
  begin
    with SyntKeyMapping do
      for i:= 0 to Items.Count-1 do
        if Items[i].Command=cmd then
        begin
          Items[i].DisplayName:= NewName;
          Exit
        end;
  end;
begin
  KeyN(sm_Macro1, DoMacro_GetCommandName(0));
  KeyN(sm_Macro2, DoMacro_GetCommandName(1));
  KeyN(sm_Macro3, DoMacro_GetCommandName(2));
  KeyN(sm_Macro4, DoMacro_GetCommandName(3));
  KeyN(sm_Macro5, DoMacro_GetCommandName(4));
  KeyN(sm_Macro6, DoMacro_GetCommandName(5));
  KeyN(sm_Macro7, DoMacro_GetCommandName(6));
  KeyN(sm_Macro8, DoMacro_GetCommandName(7));
  KeyN(sm_Macro9, DoMacro_GetCommandName(8));
  KeyN(sm_Macro10, DoMacro_GetCommandName(9));
  KeyN(sm_Macro11, DoMacro_GetCommandName(10));
  KeyN(sm_Macro12, DoMacro_GetCommandName(11));
  KeyN(sm_Macro13, DoMacro_GetCommandName(12));
  KeyN(sm_Macro14, DoMacro_GetCommandName(13));
  KeyN(sm_Macro15, DoMacro_GetCommandName(14));
  KeyN(sm_Macro16, DoMacro_GetCommandName(15));
  KeyN(sm_Macro17, DoMacro_GetCommandName(16));
  KeyN(sm_Macro18, DoMacro_GetCommandName(17));
  KeyN(sm_Macro19, DoMacro_GetCommandName(18));
  KeyN(sm_Macro20, DoMacro_GetCommandName(19));
  KeyN(sm_Macro21, DoMacro_GetCommandName(20));
  KeyN(sm_Macro22, DoMacro_GetCommandName(21));
  KeyN(sm_Macro23, DoMacro_GetCommandName(22));
  KeyN(sm_Macro24, DoMacro_GetCommandName(23));
  KeyN(sm_Macro25, DoMacro_GetCommandName(24));
  KeyN(sm_Macro26, DoMacro_GetCommandName(25));
  KeyN(sm_Macro27, DoMacro_GetCommandName(26));
  KeyN(sm_Macro28, DoMacro_GetCommandName(27));
  KeyN(sm_Macro29, DoMacro_GetCommandName(28));
  KeyN(sm_Macro30, DoMacro_GetCommandName(29));
end;

procedure TfmMain.acMacro10Execute(Sender: TObject);
begin
  DoMacro_Run(9);
end;

procedure TfmMain.acMacro11Execute(Sender: TObject);
begin
  DoMacro_Run(10);
end;

procedure TfmMain.acMacro12Execute(Sender: TObject);
begin
  DoMacro_Run(11);
end;

procedure TfmMain.acMacro13Execute(Sender: TObject);
begin
  DoMacro_Run(12);
end;

procedure TfmMain.acMacro14Execute(Sender: TObject);
begin
  DoMacro_Run(13);
end;

procedure TfmMain.acMacro15Execute(Sender: TObject);
begin
  DoMacro_Run(14);
end;

procedure TfmMain.acMacro16Execute(Sender: TObject);
begin
  DoMacro_Run(15);
end;

procedure TfmMain.acMacro17Execute(Sender: TObject);
begin
  DoMacro_Run(16);
end;

procedure TfmMain.acMacro18Execute(Sender: TObject);
begin
  DoMacro_Run(17);
end;

procedure TfmMain.acMacro19Execute(Sender: TObject);
begin
  DoMacro_Run(18);
end;

procedure TfmMain.acMacro20Execute(Sender: TObject);
begin
  DoMacro_Run(19);
end;

procedure TfmMain.acMacro21Execute(Sender: TObject);
begin
  DoMacro_Run(20);
end;

procedure TfmMain.acMacro22Execute(Sender: TObject);
begin
  DoMacro_Run(21);
end;

procedure TfmMain.acMacro23Execute(Sender: TObject);
begin
  DoMacro_Run(22);
end;

procedure TfmMain.acMacro24Execute(Sender: TObject);
begin
  DoMacro_Run(23);
end;

procedure TfmMain.acMacro25Execute(Sender: TObject);
begin
  DoMacro_Run(24);
end;

procedure TfmMain.acMacro26Execute(Sender: TObject);
begin
  DoMacro_Run(25);
end;

procedure TfmMain.acMacro27Execute(Sender: TObject);
begin
  DoMacro_Run(26);
end;

procedure TfmMain.acMacro28Execute(Sender: TObject);
begin
  DoMacro_Run(27);
end;

procedure TfmMain.acMacro29Execute(Sender: TObject);
begin
  DoMacro_Run(28);
end;

procedure TfmMain.acMacro30Execute(Sender: TObject);
begin
  DoMacro_Run(29);
end;

procedure TfmMain.TBXItemMacro10Click(Sender: TObject);
begin
  acMacro10.Execute;
end;

procedure TfmMain.TBXItemMacro11Click(Sender: TObject);
begin
  acMacro11.Execute;
end;

procedure TfmMain.TBXItemMacro12Click(Sender: TObject);
begin
  acMacro12.Execute;
end;

procedure TfmMain.TBXItemMacro13Click(Sender: TObject);
begin
  acMacro13.Execute;
end;

procedure TfmMain.TBXItemMacro14Click(Sender: TObject);
begin
  acMacro14.Execute;
end;

procedure TfmMain.TBXItemMacro15Click(Sender: TObject);
begin
  acMacro15.Execute;
end;

procedure TfmMain.TBXItemMacro16Click(Sender: TObject);
begin
  acMacro16.Execute;
end;

procedure TfmMain.TBXItemMacro17Click(Sender: TObject);
begin
  acMacro17.Execute;
end;

procedure TfmMain.TBXItemMacro18Click(Sender: TObject);
begin
  acMacro18.Execute;
end;

procedure TfmMain.TBXItemMacro19Click(Sender: TObject);
begin
  acMacro19.Execute;
end;

procedure TfmMain.TBXItemMacro20Click(Sender: TObject);
begin
  acMacro20.Execute;
end;

procedure TfmMain.TBXItemMacro21Click(Sender: TObject);
begin
  acMacro21.Execute;
end;

procedure TfmMain.TBXItemMacro22Click(Sender: TObject);
begin
  acMacro22.Execute;
end;

procedure TfmMain.TBXItemMacro23Click(Sender: TObject);
begin
  acMacro23.Execute;
end;

procedure TfmMain.TBXItemMacro24Click(Sender: TObject);
begin
  acMacro24.Execute;
end;

procedure TfmMain.TBXItemMacro25Click(Sender: TObject);
begin
  acMacro25.Execute;
end;

procedure TfmMain.TBXItemMacro26Click(Sender: TObject);
begin
  acMacro26.Execute;
end;

procedure TfmMain.TBXItemMacro27Click(Sender: TObject);
begin
  acMacro27.Execute;
end;

procedure TfmMain.TBXItemMacro28Click(Sender: TObject);
begin
  acMacro28.Execute;
end;

procedure TfmMain.TBXItemMacro29Click(Sender: TObject);
begin
  acMacro29.Execute;
end;

procedure TfmMain.TBXItemMacro30Click(Sender: TObject);
begin
  acMacro30.Execute;
end;

constructor TSynFindInfo.Create;
begin
  FN:= '';
  Str:= '';
  LineNum:= 0;
  ColNum:= 0;
  Len:= 0;
end;

procedure TfmMain.UpdateTreeFind_Results(
  AStr: Widestring; const ADir: Widestring;
  AStopped: boolean; AInTabs: boolean = false);
  //-------------------
  function STreeText(SEnd: Widestring): Widestring;
  var
    SStart: Widestring;
  begin
    if AStopped then
      SEnd:= SEnd + ', ' + DKLangConstW('O_intr');
    if not AInTabs then
      SStart:= WideFormat(DKLangConstW('O_fnode'), [AStr, ADir])
    else
      SStart:= WideFormat(DKLangConstW('O_ftabs'), [AStr]);
    Result:= SStart + ' (' + SEnd + ')';
  end;
  //-------------------
  function UpdateTreeCounter(NodeFile: TTntTreeNode): integer;
  var
    //Node: TTntTreeNode;
    Obj: TObject;
  begin
    {
    Result:= 1;
    Node:= NodeFile.GetFirstChild;
    if Node<>nil then
    repeat
      Node:= NodeFile.GetNextChild(Node);
      if Node=nil then Break;
      Inc(Result);
    until false;
    }
    Result:= 0;
    Obj:= TObject(NodeFile.Data);
    if Obj is TSynFindCount then
      Result:= (Obj as TSynFindCount).Matches + 1;
    NodeFile.Text:= NodeFile.Text+ Format(' (%d)', [Result]);
  end;
var
  Node: TTntTreeNode;
  NFiles, NItems: integer;
  SEnd: Widestring;
begin
  AStr:= SReplaceAllEols(AStr, '¶');

  NFiles:= 0;
  NItems:= 0;
  if FTreeRoot=nil then
    raise Exception.Create('TreeRoot nil');
  Node:= FTreeRoot.GetFirstChild;
  if Node=nil then
  begin
    SEnd:= DKLangConstW('O_nores');
    FTreeRoot.Text:= STreeText(SEnd);
    Exit
  end;
  repeat
    Inc(NFiles);
    Inc(NItems, UpdateTreeCounter(Node));
    Node:= FTreeRoot.GetNextChild(Node);
    if Node=nil then Break;
  until false;

  //report
  SEnd:= WideFormat(DKLangConstW('O_stat'), [NItems, NFiles]);
  FTreeRoot.Text:= STreeText(SEnd);
  TreeFind.Selected:= FTreeRoot;
  FTreeRoot.Expand(false);
end;

procedure TfmMain.UpdateTreeFind_Initial(AStr: Widestring; const ADir: Widestring; AInTabs: boolean = false);
begin
  AStr:= SReplaceAllEols(AStr, '¶');

  if not AInTabs then
    FTreeRoot:= TreeFind.Items.Add(nil,
      WideFormat(DKLangConstW('O_fnode'), [AStr, ADir]) + '...')
  else
    FTreeRoot:= TreeFind.Items.Add(nil,
      WideFormat(DKLangConstW('O_ftabs'), [AStr]) + '...');

  TreeFind.Selected:= FTreeRoot;
  FTreeRoot.Expand(false);
end;

procedure TfmMain.TreeFind_ShowPreview;
begin
  if Assigned(FProjPreview) then
    with FProjPreview do
    begin
      Visible:= not Visible;
      if Visible then
        TreeFindChange(nil, nil);
    end;
end;

procedure TfmMain.TreeFindChange(Sender: TObject; Node: TTreeNode);
var
  fn: Widestring;
  LineNum, ColNum, Len: Integer;
begin
  if Assigned(FProjPreview) and FProjPreview.Visible then
  begin
    TreeFind_GetItemInfo(fn, LineNum, ColNum, Len);
    if IsFileExist(fn) then
      DoPreviewFile(fn, false, LineNum, ColNum, Len);
  end;  
end;

procedure TfmMain.TreeFindDblClick(Sender: TObject);
var
  fn: Widestring;
  LineNum, ColNum, Len, n: Integer;
begin
  TreeFind_GetItemInfo(fn, LineNum, ColNum, Len);

  if IsFileExist(fn) then
    DoOpenFile(fn)
  else
  begin
    n:= SGetFrameIndexFromPrefixedStr(fn);
    if (n>=0) and (n<FrameAllCount) then
      CurrentFrame:= FramesAll[n]
    else
      Exit;
  end;

  with CurrentEditor do
    SetSelection(CaretPosToStrPos(Point(ColNum, LineNum)), Len);
    
  EditorCenterPos(CurrentEditor, false, opFindOffsetTop);
  FocusEditor;
end;

procedure TfmMain.TreeFind_GetItemInfo(var AFilename: Widestring; var ALineNum, AColNum, ALen: Integer);
var
  Obj: TObject;
  Info: TSynFindInfo;
  n: integer;
begin
  AFilename:= '';
  ALineNum:= 0;
  AColNum:= 0;
  ALen:= 0; 

  if TreeFind.Selected=nil then Exit;
  Obj:= TObject(TreeFind.Selected.Data);
  if Obj is TSynFindInfo then
  begin
    Info:= Obj as TSynFindInfo;
    AFilename:= Info.FN;
    ALineNum:= Info.LineNum;
    AColNum:= Info.ColNum;
    ALen:= Info.Len;
  end
  else
  begin
    //maybe clicked on Replace result: "filename (NN)"
    AFilename:= TreeFind.Selected.Text;
    if (AFilename='') or (AFilename[Length(AFilename)]<>')') then Exit;
    n:= Length(AFilename);
    while (n>0) and (AFilename[n]<>' ') do Dec(n);
    if n=0 then Exit;
    Delete(AFilename, n, MaxInt);
  end;
end;

procedure TfmMain.TreeFindCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  DefaultDraw:= true;
end;

procedure TfmMain.TreeFindAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  r: TRect;
  Obj: TObject;
  Info: TSynFindInfo;
  sInf, s: Widestring;
  n: Integer;
begin
  Obj:= TObject(Node.Data);
  if Obj is TSynFindInfo then
    Info:= Obj as TSynFindInfo
  else
    Exit;

  if Stage<>cdPostPaint then Exit;
  DefaultDraw:= true;
  R:= Node.DisplayRect(true);
  Inc(R.Left, 2); // <---------dx
  Inc(R.Top, 1); //<--------dy

  Sender.Canvas.Font.Assign(TreeFind.Font);
  Sender.Canvas.Font.Color:= opColorOutRedText;
  Sender.Canvas.Brush.Color:= opColorOutHi;

  sInf:= SFindResPrefix({Info.FN,} Info.LineNum);
  s:= Copy(TTntTreeNode(Node).Text, 1, Length(sInf)+Info.ColNum);
  n:= ecTextExtent(Sender.Canvas, s).cx;
  s:= Copy(TTntTreeNode(Node).Text, Length(sInf)+Info.ColNum+1, Info.Len);
  ecTextOut(Sender.Canvas, R.Left+n, R.Top, s);
end;

function TfmMain.SFindResPrefix(LineNum: integer): Widestring;
begin
  Result:= WideFormat('(%d): ', [LineNum+1]);
end;

procedure TfmMain.TreeFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Enter, Space
  if (Key=vk_return) and (Shift=[]) then
  begin
    TreeFindDblClick(Sender);
    Key:= 0;
    Exit
  end;
  if (Key=vk_space) and (Shift=[]) then
  begin
    TreeFind_ShowPreview;
    Key:= 0;
    Exit
  end;
  //Ctrl+F
  if IsShortcutOfCmd(Shortcut(Key, Shift), smFindDialog) then
  begin
    TBXItemTreeFindFindClick(Self);
    Key:= 0;
    Exit
  end;
  //Del, Ctrl+Del
  if (Key=vk_delete) and ((Shift=[]) or (Shift=[ssCtrl])) then
  begin
    TbxItemTreeFindClearClick(Sender);
    Key:= 0;
    Exit
  end;
  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.UpdateTreeFind_ReplaceResults(const ANodeText: Widestring; ANumFiles, ANumItems: integer; AStopped: boolean);
var
  SEnd: Widestring;
begin
  if FTreeRoot=nil then
    raise Exception.Create('TreeRoot nil');

  if ANumItems=0 then
    SEnd:= DKLangConstW('O_nores')
  else
    SEnd:= WideFormat(DKLangConstW('O_stat'), [ANumItems, ANumFiles]);
  if AStopped then
    SEnd:= SEnd + ', ' + DKLangConstW('O_intr');

  FTreeRoot.Text:= ANodeText + ' ('+SEnd+')';
end;

procedure TfmMain.TBXItemTreeFindNavClick(Sender: TObject);
begin
  TreeFindDblClick(Self);
end;

procedure TfmMain.TBXItemTreeFindFindClick(Sender: TObject);
begin
  DoFind_InResultsPanel;
end;

procedure TfmMain.DoFind_InResultsPanel;
begin
  with TreeFind do
    if CanFocus then
    begin
      SetFocus;
      ecFindInTree.Execute;
    end;
end;

procedure TfmMain.TBXItemTreeFindCopyToTabClick(Sender: TObject);
begin
  DoCopyFindResultToTab(false, false);
end;

procedure TfmMain.DoCopyFindResultToTab(ALastSearch, AFilesOnly: boolean;
  AToClip: boolean = false);
var
  Node: TTntTreeNode;
  L: TWideStringList;
  S: Widestring;
  F: TEditorFrame;
begin
  if ALastSearch then
  begin
    Node:= FTreeRoot;
    if Node=nil then
      raise Exception.Create('TreeRoot nil');
  end
  else
  begin
    Node:= TreeFind.Selected;
    if Node=nil then Exit;
    if Node.Parent<>nil then //goto file level
      Node:= Node.Parent;
    if Node.Parent<>nil then //goto root level
      Node:= Node.Parent;
  end;

  L:= TWideStringList.Create;
  try
    DoCopyFindResultToList(Node, L, AFilesOnly);
    if AToClip then
    begin
      S:= L.Text;
      SReplaceZeroesW(S);
      TntClipboard.AsWideText:= S;
    end
    else
    begin
      F:= DoAddTab(Groups.PagesCurrent, true);
      F.EditorMaster.Lines.AddStrings(L);
    end;
  finally
    FreeAndNil(L);
  end;
end;

//copy tree from selected node
procedure TfmMain.DoCopyFindResultNode;
var
  Node: TTntTreeNode;
  L: TWideStringList;
  S: Widestring;
begin
  Node:= TreeFind.Selected;
  if Node=nil then Exit;

  L:= TWideStringList.Create;
  try
    DoCopyFindResultToList(Node, L, true);
    S:= L.Text;
  finally
    FreeAndNil(L);
  end;

  SReplaceZeroesW(S);
  TntClipboard.AsWideText:= S;
end;

procedure TfmMain.DoCopyFindResultToList(ARootNode: TTntTreeNode;
  L: TWideStringList; AFilesOnly: boolean);
var
  Node, Node2: TTntTreeNode;
  Info: TSynFindInfo;
  n: integer;
begin
  L.Clear;
  L.Add(ARootNode.Text);
  L.Add('');
  Node:= ARootNode.GetFirstChild;
  if Node=nil then Exit;

  repeat
    if AFilesOnly then
      L.Add(Node.Text)
    else
    begin
      Node2:= Node.GetFirstChild;
      if Node2<>nil then
        repeat
          if TObject(Node2.Data) is TSynFindInfo then
            Info:= TSynFindInfo(Node2.Data)
          else
            Info:= nil;

          if Info<>nil then
          begin
            n:= Pos('): ', Node2.Text);
            if n>0 then
              L.Add(
                Info.FN + '(' + IntToStr(Info.LineNum+1) + '): ' +
                Info.Str);
          end;
          Node2:= Node.GetNextChild(Node2);
        until Node2=nil;
    end;
    Node:= ARootNode.GetNextChild(Node);
  until Node=nil;
end;

procedure TfmMain.TBXItemTreeFindClearClick(Sender: TObject);
begin
  DoClearTreeFind;
end;

procedure TfmMain.DoClearTreeFind;
var
  i: integer;
  Node: TTntTreeNode;
begin
  for i:= TreeFind.Items.Count-1 downto 0 do
  begin
    Node:= TreeFind.Items[i];
    if Node.Data<>nil then
    begin
      TObject(Node.Data).Free;
      Node.Data:= nil;
    end;
  end;
  TreeFind.Items.Clear;
end;

procedure TfmMain.TBXItemTreeFindCopyToClipClick(Sender: TObject);
begin
  DoCopyFindResultToTab(false, false, true{AToClip});
end;

procedure TfmMain.TBXItemTreeFindExpandClick(Sender: TObject);
var
  Node: TTntTreeNode;
begin
  Node:= TreeFind.Selected;
  TreeFind.FullExpand;
  if Node<>nil then
    Node.MakeVisible;
end;

procedure TfmMain.TBXItemTreeFindCollapseClick(Sender: TObject);
begin
  TreeFind.FullCollapse;
end;

{
procedure TfmMain.MsgAcpFile(const s: Widestring);
begin
  SHint[-1]:= '[ACP from file] ' + s;
  //Application.ProcessMessages; //don't leave in release!
end;
}

procedure TfmMain.MsgNoRun(const fn: Widestring);
begin
  MsgError(WideFormat(DKLangConstW('MRun'), [fn]), Handle);
end;

procedure TfmMain.MsgNoSelection;
begin
  MsgWarn(DKLangConstW('MNSel2'), Handle);
end;

procedure TfmMain.MsgNoSelectionForHelp;
begin
  MsgWarn(DKLangConstW('MNSel'), Handle);
end;

procedure TfmMain.MsgNoFile(const fn: Widestring);
begin
  MsgError(DKLangConstW('MNFound')+#13+fn, Handle);
end;

procedure TfmMain.MsgNoDir(const fn: Widestring);
begin
  MsgWarn(DKLangConstW('MNFoundFold')+#13+fn, Handle);
end;

procedure TfmMain.MsgEmptyMacro(const s: Widestring);
begin
  MsgWarn(WideFormat(DKLangConstW('zMNoMacro'), [s]), Handle);
end;

procedure TfmMain.MsgDelLines(N: integer);
begin
  DoHint(WideFormat(DKLangConstW('zDelLn'), [N]));
end;

procedure TfmMain.MsgDoneLines(N: integer);
begin
  DoHint(WideFormat(DKLangConstW('zDoneLn'), [N]));
end;

procedure TfmMain.MsgTabbing(const s: Widestring);
begin
  DoHint('[SmartTagTabbing] ' + s);
end;

procedure TfmMain.MsgNeedProject;
begin
  MsgWarn(DKLangConstW('zMProjEmpty'), Handle);
end;

procedure TfmMain.TBXItemTreeFindExpandCurClick(Sender: TObject);
begin
  with TreeFind do
    if Selected<>nil then
      Selected.Expand(true);
end;

procedure TfmMain.ApplyTips;
begin
  Tree.ToolTips:= opTipsPanels;
  TreeFind.ToolTips:= opTipsPanels;
  ListOut.ShowHint:= opTipsPanels;
  ListVal.ShowHint:= opTipsPanels;
  ListPLog.ShowHint:= opTipsPanels;
  MemoConsole.ShowHint:= opTipsPanels;
  if Assigned(fmClip) then
    fmClip.ListClip.ShowHint:= opTipsPanels;
end;


function CurrentLexerForLine(Ed: TSyntaxMemo; NLine: integer): string;
var
  Pos: integer;
begin
  if Assigned(Ed) then
  begin
    Pos:= Ed.CaretPosToStrPos(Point(0, NLine));
    Result:= EditorCurrentLexerForPos(Ed, Pos);
  end
  else
    Result:= '';
end;

function TfmMain.CurrentLexer: string;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Assigned(Ed) then
    Result:= EditorCurrentLexerForPos(Ed, Ed.CaretStrPos)
  else
    Result:= '';
end;

function TfmMain.CurrentLexerForFile: string;
begin
  Result:= '';
  if SyntaxManager.CurrentLexer<>nil then
    Result:= SyntaxManager.CurrentLexer.LexerName;
end;

function TfmMain.GetLexerComment(const Lexer: string): string;
var
  An: TSyntAnalyzer;
begin
  Result:= '';
  An:= SyntaxManager.FindAnalyzer(Lexer);
  if An<>nil then
    Result:= An.LineComment;
end;

function TfmMain.DoSmartTagTabbing: boolean;
var
  Ed: TSyntaxMemo;
  s, sTag, sTagExp, sIndent: Widestring;
  NTagSt, NTagLen, NCaret, NPos: integer;
  SelSt, SelLen: integer;
  fn: string;
  DoInitTab: boolean;
begin
  Result:= false;
  if not opAcpTabbing then Exit;
  if not IsLexerHTML(CurrentLexer) then Exit;

  Ed:= CurrentEditor;
  if Ed.ReadOnly then Exit;
  if Ed.CaretPos.Y >= Ed.Lines.Count then Exit;
  s:= Ed.Lines[Ed.CaretPos.Y];

  //process tabbing inside <tagName ......>
  NCaret:= Ed.CaretPos.X + 1;
  SGetTagBounds(s, NCaret, NTagSt, NTagLen);
  if (NTagSt>0) and (NTagLen>0) then
  begin
    sTag:= Copy(s, NTagSt, NTagLen);
    SelSt:= NCaret - NTagSt + 1;
    SelLen:= Ed.SelLength;

    SGetTagTabbing(sTag, SelSt, SelLen);
    if SelSt > 0 then
    begin
      Ed.CaretPos:= Point(NTagSt + SelSt - 2, Ed.CaretPos.Y);
      Ed.SetSelection(Ed.CaretStrPos, SelLen, true);
      MsgTabbing(DKLangConstW('ztagMovedSelection'));
    end
    else
    begin
      Ed.CaretPos:= Point(NTagSt + NTagLen - 1, Ed.CaretPos.Y);
      Ed.ResetSelection;
      MsgTabbing(DKLangConstW('ztagMovedOut'));
    end;
    Result:= true;
  end
  else
  begin
    //process tagName expanding to <tagName ......>
    SGetTagStart(s, NCaret, NTagSt);
    if NTagSt=0 then Exit;
    sTag:= Copy(s, NTagSt, NCaret-NTagSt);

    //read tag expansion from Html_Tabbing.ini
    fn:= GetHtmlTabbingFN;
    if not IsFileExist(fn) then
    begin
      MsgTabbing('File not found: "'+fn+'"');
      MsgBeep;
      Result:= true;
      Exit;
    end;

    with TIniFile.Create(fn) do
    try
      sTagExp:= ReadString('HTML', sTag, '');
    finally
      Free
    end;

    if sTagExp='' then
    begin
      MsgTabbing(DKLangConstW('ztagTagUnknown')+' "'+sTag+'"');
      MsgBeep;
      Result:= true;
      Exit
    end;

    //insert expanded tag
    DoInitTab:=
      (Pos('|', sTagExp) = 0) and
      (Pos(' ', sTagExp) > 0);

    //replace '\n'
    sIndent:= EditorIndentStringForPos(Ed, Point(NTagSt-1, Ed.CaretPos.Y));
    SReplaceAllW(sTagExp, '\n', EditorEOL(Ed) + sIndent);

    //replace '|', save 1st '|' position
    NCaret:= Pos('|', sTagExp);
    if NCaret=0 then NCaret:= 1;
    SReplaceAllW(sTagExp, '|', '');

    Ed.BeginUpdate;
    try
      Ed.CaretPos:= Point(NTagSt-1, Ed.CaretPos.Y);
      NPos:= Ed.CaretStrPos;
      Ed.DeleteText(Length(sTag));
      Ed.InsertText(sTagExp);
      if DoInitTab then
      begin
        //do TagTabbing 2 times to select inner range
        SelSt:= 0;
        SelLen:= 0;
        SGetTagTabbing(sTagExp, SelSt, SelLen);
        SGetTagTabbing(sTagExp, SelSt, SelLen);
        Ed.CaretStrPos:= NPos + SelSt - 1;
        Ed.SetSelection(
          Ed.CaretStrPos, SelLen);
        MsgTabbing(DKLangConstW('ztagExpandAndMove'));
      end
      else
      begin
        //NCaret holds '|' position in sTagExp
        Ed.CaretStrPos:= NPos + NCaret - 1;
        MsgTabbing(DKLangConstW('ztagExpand'));
      end;
    finally
      Ed.EndUpdate;
    end;
    Result:= true;
  end;
end;

procedure TfmMain.ecTreeParentExecute(Sender: TObject);
begin
  DoTreeJump(tgoParent);
end;

procedure TfmMain.ecTreeNextBrotherExecute(Sender: TObject);
begin
  DoTreeJump(tgoNextBro);
end;

procedure TfmMain.ecTreePrevBrotherExecute(Sender: TObject);
begin
  DoTreeJump(tgoPrevBro);
end;

procedure TfmMain.TBXItemSSelTokenClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SelectToken);
end;

procedure TfmMain.TBXItemTreeFindCopyToClipNodeClick(Sender: TObject);
begin
  //if root selected (Parent=nil), do "Copy search",
  //else do "Copy node"
  if (TreeFind.Selected<>nil) and (TreeFind.Selected.Parent=nil) then
    TBXItemTreeFindCopyToClipClick(Self)
  else
    DoCopyFindResultNode;
end;

procedure TfmMain.DoTabSwitch(ANext: boolean; AAllowModernSwitch: boolean = true);
var
  NTabs: integer;
  Switcher: TTabSwitcher;
begin
  NTabs:= FrameCount;
  if NTabs<=1 then Exit;

  if opTabSwitcher and (NTabs>2) and AAllowModernSwitch then
  begin
    Switcher:= TabSwitchers[Groups.PagesIndexOf(Groups.PagesCurrent)];
    NTabs:= Switcher.TabSwitch(ANext, Application.MainForm);
    if NTabs>=0 then
      Groups.PagesCurrent.Tabs.TabIndex:= NTabs;
  end
  else
    Groups.PagesCurrent.Tabs.SwitchTab(ANext);
end;

procedure TfmMain.UpdateListTabs;
var
  F, FCurrent: TEditorFrame;
  i: Integer;
begin
  FCurrent:= CurrentFrame;

  with ListTabs do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for i:= 0 to FrameAllCount-1 do
        with Items.Add do
        begin
          F:= FramesAll[i];
          if F=FCurrent then
            ListTabs.Selected:= Items[i];

          Caption:= '';
          SubItems.Add(F.TabCaption);
          SubItems.Add(F.FileName);
          SubItems.Add(IntToStr(i));
          ImageIndex:= IfThen(F.Modified, 0, -1);
        end;
    finally
      Items.EndUpdate;
    end;

    if Selected<>nil then
      Selected.MakeVisible(false);
  end;
end;


procedure TfmMain.GetTabName(
  APagesNumber, ATabIndex: Integer;
  var AName, AFN, ALex: Widestring);
var
  F: TEditorFrame;
  D: TATTabData;
begin
  D:= Groups.Pages[APagesNumber].Tabs.GetTabData(ATabIndex);
  if D<>nil then
  begin
    F:= D.TabObject as TEditorFrame;
    AName:= WideFormat('[%d] ', [ATabIndex+1]) + F.TabCaption;
    AFN:= F.FileName;
    if AFN='' then
      AFN:= F.TabCaption;
    ALex:= F.CurrentLexer;
  end
  else
  begin
    AName:= WideFormat('[%d]', [ATabIndex+1]);
    AFN:= WideFormat('(index=%d, FrameCount=%d)', [ATabIndex, FrameAllCount]);
    ALex:= '';
  end;
end;

procedure TfmMain.TemplatePopupShow(Sender: TObject);
begin
  if opAcpUseSingle then
    if TemplatePopup.Listbox.Items.Count=1 then
      TemplatePopup.CloseUp(true);
end;

function TfmMain.DoSnippetTabbing: boolean;
var
  Ed: TSyntaxMemo;
  StrId, StrLexer, StrSelText: Widestring;
  NSnipIndex, i: Integer;
begin
  Result:= false;
  Ed:= CurrentEditor;
  if not opTemplateTabbing then Exit;
  if SFileExtensionMatch(CurrentFrame.FileName, opTemplateTabbingExcept) then Exit;
  if Ed.ReadOnly then Exit;

  StrLexer:= CurrentLexer;
  StrId:= EditorGetWordBeforeCaret(Ed, true);
  if StrId='' then Exit;

  InitSnippets;
  NSnipIndex:= -1;
  for i:= 0 to FListSnippets.Count-1 do
  begin
    with TSynSnippetClass(FListSnippets[i]) do
      if ((Info.Lexers = '') or IsLexerListed(StrLexer, Info.Lexers)) and
        (StrId = Info.Id) then
        if NSnipIndex>=0 then //id exists N times?
        begin
          DoSnippetListDialog(StrId);
          Result:= true;
          Exit
        end
        else
          NSnipIndex:= i;
  end;

  //single snippet found
  if NSnipIndex>=0 then
  begin
    Ed.BeginUpdate;
    try
      StrSelText:= Ed.SelText;
      Ed.ClearSelection;
      Ed.CaretStrPos:= Ed.CaretStrPos - Length(StrId);
      Ed.DeleteText(Length(StrId));
      EditorInsertSnippet(Ed,
        TSynSnippetClass(FListSnippets[NSnipIndex]).Info.Text,
        StrSelText,
        FrameOfEditor(Ed).FileName);
    finally
      Ed.EndUpdate;
    end;
    Result:= true;
  end;
end;

procedure TfmMain.MsgColorBad;
begin
  DoHint(DKLangConstW('MColorBad')+' "'+s+'"');
end;

procedure TfmMain.MsgColorOK;
begin
  DoHint(WideFormat(DKLangConstW('MColorOk'), ['#'+s]));
end;


function TfmMain.IsShowColor(s: string;
  var NColor, NColorText: TColor): boolean;
var
  SColor, SColorText: ecString;
  Sel: Widestring;
begin
  Result:= false;
  NColor:= clNone;
  NColorText:= clNone;
  UpdateColorHint(false);

  if s='' then Exit;
  if not IsLexerWithColors(CurrentLexer) then Exit;

  //show selected color first
  sel:= CurrentEditor.SelText;

  //show color for HTML color names (Blue, Red..)
  if IsStringRegex(sel, cRegexColorName) then
  begin
    s:= SGetColorNameValue(sel);
    if s='' then Exit;
    Delete(s, 1, 1);

    if not IsHexColorString(s) then
      begin MsgColorBad(s); Exit end;
    NColor:= SHtmlCodeToColor(s);

    MsgColorOK(s);
    Result:= true;
    Exit;
  end;

  //show color for selected color code #AABBCC
  if IsStringRegex(sel, cRegexColorCode) then
  begin
    Delete(sel, 1, 1);
    s:= sel;

    if not IsHexColorString(s) then
      begin MsgColorBad(s); Exit end;
    NColor:= SHtmlCodeToColor(s);

    MsgColorOK(s);
    Result:= true;
    Exit;
  end;

  //parse HTML <div style="color: #AAA; background-color: #BBB;">
  SColorText:= SFindRegex(s, '(?<=background-color\:\s*\#)\w{3,6}');
  SColor:= SFindRegex(s, '(?<=[^-]color\:\s*\#)\w{3,6}');
  if (SColor<>'') and (SColorText<>'') then
  begin
    //Msg(SColor+' '+SColorText);
    if not IsHexColorString(SColor) then
      begin MsgColorBad(SColor); Exit end;
    if not IsHexColorString(SColorText) then
      begin MsgColorBad(SColorText); Exit end;
    NColor:= SHtmlCodeToColor(SColor);
    NColorText:= SHtmlCodeToColor(SColorText);

    MsgColorOK(SColor+ ' / #' +SColorText);
    Result:= true;
    Exit
  end
  else
  if (SColor<>'') then
    s:= '#'+SColor
  else
  if (SColorText<>'') then
    s:= '#'+SColorText;

  //delete quotes
  if (s[1]='"') and (s[Length(s)]='"') then
  begin
    s:= Copy(s, 2, Length(s)-2);
    if s='' then Exit;
  end;
  if (s[1]='''') and (s[Length(s)]='''') then
  begin
    s:= Copy(s, 2, Length(s)-2);
    if s='' then Exit;
  end;

  //is string #AABBCC ?
  if s[1]<>'#' then Exit;
  Delete(s, 1, 1);
  if s='' then Exit;

  if not IsHexColorString(s) then
    begin MsgColorBad(s); Exit end;
  NColor:= SHtmlCodeToColor(s);

  MsgColorOK(s);
  Result:= true;
end;


procedure TfmMain.UpdateColorHint(AClearHint: boolean = true);
begin
  if AClearHint then
    DoHint('');
end;

procedure TfmMain.ApplyShowRecentColors;
begin
  case opShowRecentColors of
    cRecColorsAutoHide:
      TbxSubmenuColors.Visible:= ImageListColorRecent.Count>1;
    cRecColorsShow:
      TbxSubmenuColors.Visible:= true;
    cRecColorsHide:
      TbxSubmenuColors.Visible:= false;
  end;
end;

procedure TfmMain.DoClearRecentColors;
begin
  TbxSubmenuColors.Clear;

  //leave only 1st color ico
  with ImageListColorRecent do
    while Count>1 do
      Delete(Count-1);

  ApplyShowRecentColors;
end;

procedure TfmMain.DoAddRecentColor(N: Integer);
var
  Item: TSpTbxItem;
begin
  DoInitRecentColorsMenu;
  DoAddColorToImageList(ImageListColorRecent, N);
  ApplyShowRecentColors;

  //delete old menuitem with same color
  DoDeleteRecentColor(N);

  //add new menuitem
  Item:= TSpTbxItem.Create(Self);
  Item.Caption:= SColorToHtmlCode(N);
  Item.Tag:= N;
  Item.OnClick:= RecentColorClick;
  Item.Images:= ImageListColorRecent;
  Item.ImageIndex:= Item.Images.Count-1;
  TbxSubmenuColors.Insert(cColorIdxMin, Item);
end;

procedure TfmMain.DoDeleteRecentColor(N: Integer);
var
  i: Integer;
begin
  with TbxSubmenuColors do
    for i:= cColorIdxMin to Count-1 do
      if Items[i].Tag=N then
      begin
        Items[i].Free;
        Break
      end;
end;


procedure TfmMain.RecentColorClick(Sender: TObject);
var
  Code: Integer;
begin
  Code:= TComponent(Sender).Tag;
  if Code=-1 then
    DoClearRecentColors
  else
  if IsCtrlPressed then
    DoDeleteRecentColor(Code)
  else
  begin
    EditorInsertColorCode(CurrentEditor, Code);
    DoAddRecentColor(Code);
  end;
end;

procedure TfmMain.DoTool_HandleOutput(const ft: Widestring; const ATool: TSynTool);
var
  List: TWideStringList;
  AType: TSynOutputType;
  N1, N2: Integer;
begin
  ListOut.Items.Clear;
  if not (IsFileExist(ft) and (FGetFileSize(ft)>0)) then
  begin
    DoHint(WideFormat(DKLangConstW('MRun0'), [ATool.ToolCaption]));
    MsgBeep;
    Exit
  end;

  List:= TWideStringList.Create;
  with ATool do
  try
    List.LoadFromFile(ft);
    FDelete(ft);
    FixListOutput(List, ToolNoTags, IsLexerPas(ToolLexer), ToolOutEncoding,
      EditorTabExpansion(CurrentEditor));

    AType:= OutputTypeStrToType(ToolOutType);
    if AType=outReplaceSelOrDoc then
      if CurrentEditor.SelLength=0 then
        AType:= outReplaceDoc
      else
        AType:= outReplaceSel;

    case AType of
      outIgnore:
        begin end;

      outToPanel:
        begin
          SynPanelPropsOut.DefFilename:= CurrentFrame.FileName;
          SynPanelPropsOut.RegexStr:= ATool.ToolOutRegex;
          SynPanelPropsOut.RegexIdName:= ATool.ToolOutNum_fn;
          SynPanelPropsOut.RegexIdLine:= ATool.ToolOutNum_line;
          SynPanelPropsOut.RegexIdCol:= ATool.ToolOutNum_col;
          SynPanelPropsOut.Encoding:= ATool.ToolOutEncoding;
          SynPanelPropsOut.ZeroBase:= false;

          UpdatePanelOutFromList(List);
          TabsOut.TabIndex:= Ord(tbOutput);
          plOut.Show;
        end;

      outToNewDoc:
        begin
          acNewTab.Execute;
          CurrentEditor.Lines.AddStrings(List);
          //EditorSetModified(CurrentEditor);
        end;

      outReplaceSel:
        begin
          if CurrentEditor.ReadOnly then
            MsgBeep
          else
          begin
            CurrentEditor.BeginUpdate;
            try
              CurrentEditor.ClearSelection;
              CurrentEditor.InsertText(SDelLastCR(List.Text));
            finally
              CurrentEditor.EndUpdate;
            end;
          end;
        end;

      outReplaceDoc:
        begin
          if CurrentEditor.ReadOnly then
            MsgBeep
          else
          begin
            CurrentEditor.BeginUpdate;
            try
              N1:= CurrentEditor.TopLine;
              N2:= CurrentEditor.CaretPos.Y;

              CurrentEditor.SelectAll;
              CurrentEditor.ClearSelection;
              CurrentEditor.InsertText(List.Text);

              CurrentEditor.TopLine:= N1;
              CurrentEditor.CaretPos:= Point(0, N2);
            finally
              CurrentEditor.EndUpdate;
            end;
          end;
        end;

      outReplaceWord:
        begin
          if CurrentEditor.ReadOnly then
            MsgBeep
          else
          begin
            CurrentEditor.BeginUpdate;
            try
              if CurrentEditor.WordAtPos(CurrentEditor.CaretPos)<>'' then
                CurrentEditor.SelectWord;
              CurrentEditor.InsertText(Trim(SDelLastCR(List.Text)));
            finally
              CurrentEditor.EndUpdate;
            end;
          end;
        end;

      outToClip:
        begin
          TntClipboard.AsWideText:= List.Text;
        end;

      outInsAtLineStart:
        begin
          if CurrentEditor.ReadOnly then
            MsgBeep
          else
            with CurrentEditor do
            begin
              ResetSelection;
              CaretPos:= Point(0, CurrentLine);
              InsertText(List.Text);
            end;
        end;

      else
        raise Exception.Create('Unknown tool type: '+ToolOutType);
    end;
  finally
    FreeAndNil(List);
  end;
end;


function TfmMain.SStatusText(Ed: TSyntaxMemo): Widestring;
var
  Frame: TEditorFrame;
  state: TSynSelState;
  p1, p2: TPoint;
  NLine, NCol,
  NSelLines, NSelCols, NSelChars: integer;
  NCarets, NTopLine, NBottomLine: integer;
  NTime: TFileTime;
  NSize: Int64;
begin
  Result:= '';
  if Ed=nil then Exit;

  Frame:= FrameOfEditor(Ed);
  NCarets:= Frame.CaretsCount;
  Frame.CaretsProps(NTopLine, NBottomLine);

  with Ed do
  begin
    state:= selNone;
    NLine:= CaretPos.Y+1;
    NCol:= LinesPosToLog(CaretPos).X+1;
    NSelLines:= 0;
    NSelCols:= 0;
    NSelChars:= 0;

    if NCarets>1 then
      state:= selCarets
    else
    if HaveSelection then
    begin
      if SelectMode<>msColumn then
      begin
        state:= selStream;
        p1:= StrPosToCaretPos(SelStart);
        p2:= StrPosToCaretPos(SelStart+SelLength);
        NSelChars:= SelLength;
        NSelLines:= p2.Y-p1.Y+1;
        if NSelLines=1 then
          NSelCols:= p2.X-p1.X;
        if p2.X=0 then Dec(NSelLines);
        if NSelLines=1 then
          state:= selSmall;
      end
      else
      begin
        state:= selColumn;
        NSelLines:= SelRect.Bottom-SelRect.Top+1;
        NSelCols:= SelRect.Right-SelRect.Left;
        NSelChars:= NSelLines*NSelCols;
      end;
    end;
  end;

  FCurrSelState:= state;
  Result:= opStatusText[state];

  FillChar(NTime, SizeOf(NTime), 0);
  NSize:= 0;
  if Frame.FileName<>'' then
    if Pos('{File', Result)>0 then
      FGetFileInfo(Frame.FileName, NSize, NTime);

  SReplaceAllW(Result, '{LineNum}', IntToStr(NLine));
  SReplaceAllW(Result, '{ColNum}', IntToStr(NCol));
  SReplaceAllW(Result, '{SelLines}', IntToStr(NSelLines));
  SReplaceAllW(Result, '{SelCols}', IntToStr(NSelCols));
  SReplaceAllW(Result, '{SelChars}', IntToStr(NSelChars));

  SReplaceAllW(Result, '{TotalLines}', IntToStr(Ed.Lines.Count));
  if Pos('{TotalChars}', Result)>0 then
    SReplaceAllW(Result, '{TotalChars}', IntToStr(Ed.Lines.TextLength));

  SReplaceAllW(Result, '{Carets}', IntToStr(NCarets));
  SReplaceAllW(Result, '{CaretsTopLine}', IntToStr(NTopLine+1));
  SReplaceAllW(Result, '{CaretsBottomLine}', IntToStr(NBottomLine+1));

  if Frame.FileName<>'' then
  begin
    //if Pos('{FileSize}', Result)>0 then
    //  SReplaceAllW(Result, '{FileSize}', FormatSize(NSize, true));
    if Pos('{FileDate}', Result)>0 then
      SReplaceAllW(Result, '{FileDate}', FormatFileTime(NTime));
    if Pos('{FileDate2}', Result)>0 then
      SReplaceAllW(Result, '{FileDate2}', FormatFileTimeAlt(NTime));
    if Pos('{FileDateOp}', Result)>0 then
      SReplaceAllW(Result, '{FileDateOp}', FormatFileTimeFmt(NTime, opDateFmt));
  end
  else
  begin
    //SReplaceAllW(Result, '{FileSize}', '?');
    SReplaceAllW(Result, '{FileDate}', '?');
    SReplaceAllW(Result, '{FileDate2}', '?');
    SReplaceAllW(Result, '{FileDateOp}', '?');
  end;
end;

function TfmMain.SStatusHint(state: TSynSelState): Widestring;
begin
  Result:= opStatusText[state];

  SReplaceAllW(Result, '{LineNum}', cStatLine);
  SReplaceAllW(Result, '{ColNum}', cStatCol);
  SReplaceAllW(Result, '{SelLines}', cStatSelLines);
  SReplaceAllW(Result, '{SelCols}', cStatSelCols);
  SReplaceAllW(Result, '{SelChars}', cStatSelChars);
  SReplaceAllW(Result, '{TotalLines}', cStatTLines);
  SReplaceAllW(Result, '{TotalChars}', cStatTChars);

  SReplaceAllW(Result, '{FileDate}', cStatFDate);
  SReplaceAllW(Result, '{FileDate2}', cStatFDate);
  SReplaceAllW(Result, '{FileDateOp}', cStatFDate);

  SReplaceAllW(Result, '{Carets}', cStatCarets);
  SReplaceAllW(Result, '{CaretsTopLine}', cStatCaretsTopLn);
  SReplaceAllW(Result, '{CaretsBottomLine}', cStatCaretsBotLn);

  //del dup spaces
  SReplaceAllW(Result, '  ', ' ');
end;

procedure TfmMain.TBXItemClipCopyToEdClick(Sender: TObject);
begin
  DoClipItemIns;
end;

procedure TfmMain.TBXItemClipCopyToClipClick(Sender: TObject);
begin
  DoClipItemCopy;
end;

procedure TfmMain.ecReplaceSelFromClipAllExecute(Sender: TObject);
var
  Ed: TSyntaxMemo;
  S: Widestring;
  NLine: Integer;
begin
  Ed:= CurrentEditor;
  if Ed=nil then Exit;
  if Ed.ReadOnly then Exit;
  if Ed.SelLength=0 then
    begin MsgBeep; Exit end;

  S:= TntClipboard.AsWideText;
  if S='' then
    begin MsgBeep; Exit end;

  //set finder
  Finder.FindText:= Ed.SelText;
  Finder.ReplaceText:= S;
  Finder.Flags:= Finder.Flags
    - [ftBackward, ftSelectedText, ftRegex, ftPromtOnReplace]
    + [ftEntireScope];

  //replace
  NLine:= Ed.TopLine;
  Finder.ReplaceAll;
  Ed.TopLine:= NLine;

  MsgFound;
end;

procedure TfmMain.MsgFound;
var
  n: Integer;
begin
  n:= Finder.Matches;
  if n>0 then
    DoHint(WideFormat(DKLangConstW('Found'), [n]));
end;

procedure TfmMain.UpdatePanelOutFromList(List: TWideStringList);
var
  i: integer;
begin
  ListOut.Items.BeginUpdate;
  try
    ListOut.Items.Clear;
    for i:= 0 to List.Count-1 do
      ListOut.Items.Add(List[i]);
    FixListboxHorzScrollbar(ListOut);
  finally
    ListOut.Items.EndUpdate;
  end;
end;

procedure TfmMain.acRereadOutExecute(Sender: TObject);
var
  ft: Widestring;
  List: TWideStringList;
begin
  ft:= FGetTempFilenameIndexed(0);
  if not (IsFileExist(ft) and (FGetFileSize(ft)>0)) then
    begin MsgNoFile(ft); Exit end;

  List:= TWideStringList.Create;
  try
    List.LoadFromFile(ft);
    FixListOutput(List, false{NoTags}, false{NoDups},
      SynPanelPropsOut.Encoding,
      EditorTabExpansion(CurrentEditor));
    UpdatePanelOutFromList(List);
    TabsOut.TabIndex:= Ord(tbOutput);
    plOut.Show;
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmMain.TBXItemLeftTreeClick(Sender: TObject);
begin
  TabsLeft.TabIndex:= Ord(tbTree);
end;

procedure TfmMain.TBXItemLeftProjClick(Sender: TObject);
begin
  TabsLeft.TabIndex:= Ord(tbProj);
end;

procedure TfmMain.ecToggleFocusProjectExecute(Sender: TObject);
begin
  if not plTree.Visible then
  begin
    ecShowTree.Execute;
    TabsLeft.TabIndex:= Ord(tbProj);
    if Assigned(fmProj) then
      if fmProj.TreeProj.CanFocus then
        fmProj.TreeProj.SetFocus
  end
  else
  if Assigned(fmProj) and fmProj.TreeProj.Focused then
    FocusEditor
  else
  begin
    TabsLeft.TabIndex:= Ord(tbProj);
    if Assigned(fmProj) then
      if fmProj.TreeProj.CanFocus then
        fmProj.TreeProj.SetFocus
  end;
end;

function TfmMain.DoAutoCloseTag: boolean;
var
  Err: string;
begin
  Result:= false;
  if opAutoCloseTags and IsLexerWithTags(CurrentLexer) then
  begin
    Result:= EditorAutoCloseTag(CurrentEditor, Err);
    if not Result then
      DoHint(Err);
  end;
end;

function TfmMain.DoAutoCloseBracket(ch: Widechar): boolean;
begin
  Result:= EditorAutoCloseBracket(CurrentEditor, ch,
    opAutoCloseBrackets,
    opAutoCloseQuotes1,
    opAutoCloseQuotes2,
    opAutoCloseBracketsNoEsc);
end;

procedure TfmMain.ecToggleFocusMasterSlaveExecute(Sender: TObject);
begin
  with CurrentFrame do
  begin
    if not IsSplitted then
      SplitPos:= 50.0;
    if EditorMaster.Focused then
    begin
      if Self.Enabled and EditorSlave.CanFocus then
        EditorSlave.SetFocus;
    end
    else
    begin
      if Self.Enabled and EditorMaster.CanFocus then
        EditorMaster.SetFocus;
    end;
  end;
end;

procedure TfmMain.ecToggleSlaveExecute(Sender: TObject);
begin
  CurrentFrame.ToggleSplitted;
end;

procedure TfmMain.TBXItemSplitEdHorzClick(Sender: TObject);
begin
  with CurrentFrame do
    SplitHorz:= not SplitHorz;
end;

procedure TfmMain.ecRulerExecute(Sender: TObject);
begin
  if DoPyEvent(CurrentEditor, cSynEventOnState, [cSynPropRuler]) = cPyFalse then Exit;

  with CurrentEditor do
  begin
    HorzRuler.Visible:= not HorzRuler.Visible;
  end;
  UpdateStatusbar;
end;

procedure TfmMain.UpdateRecentsOnClose;
var
  i: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    with FramesAll[i] do
      if FileName<>'' then
        if not NotInRecents then
          SynMruFiles.AddItem(FileName);
end;


procedure TfmMain.ecSplitSlaveVertHorzExecute(Sender: TObject);
begin
  with CurrentFrame do
    SplitHorz:= not SplitHorz;
end;

procedure TfmMain.ecGotoBkExecute(Sender: TObject);
var
  i, nPos: Integer;
  L: TList;
  ed: TSyntaxMemo;
begin
  ed:= CurrentEditor;
  L:= TList.Create;

  try
    //create bookmarks list
    Screen.Cursor:= crHourGlass;
    try
      EditorGetBookmarksAsSortedList(ed, L);
      if L.Count=0 then
        begin MsgBeep; Exit end;
    finally
      Screen.Cursor:= crDefault;
    end;

    with TfmGotoBkmk.Create(nil) do
    try
      FIniFN:= Self.SynHistoryIni;

      //fill form
      List.Font.Assign(ed.Font);
      List.Items.Clear;
      for i:= 0 to L.Count-1 do
        List.Items.Add(EditorGetBookmarkDesc(Ed, Integer(L[i]), 60, true, true));

      //select curr bookmk
      List.ItemIndex:= 0;
      nPos:= ed.CaretStrPos;
      for i:= L.Count-1 downto 0 do
        if ed.Bookmarks[Integer(L[i])] <= nPos then
          begin List.ItemIndex:= i; Break end;
      List.Selected[List.ItemIndex]:= true;

      if ShowModal=mrOk then
      begin
        i:= List.ItemIndex;
        if i>=0 then
        begin
          //Shift pressed?
          if GetKeyState(vk_shift)<0 then
            EditorSelectToPosition(ed, ed.Bookmarks[Integer(L[i])])
          else
            ed.GotoBookmark(Integer(L[i]));

          EditorCenterPos(ed, true{GotoMode}, opFindOffsetTop);
        end;
      end;
    finally
      Free
    end;
  finally
    FreeAndNil(L);
  end;
end;

procedure TfmMain.ecGotoPortableBkExecute(Sender: TObject);
var
  CmtBegin, CmtEnd,
  CmtBegin1, CmtBegin2: string;
  StripBkmk: boolean;
  //
  function BkStr(const S: Widestring; NLine: Integer): Widestring;
  var
    n: Integer;
  begin
    Result:= S;
    SReplaceAllW(Result, #9, EditorTabExpansion(CurrentEditor));

    try
      if not StripBkmk then Exit;

      n:= Pos(CmtBegin, Result);
      if n=0 then Exit;
      Delete(Result, 1, n+Length(CmtBegin)+4);

      if CmtEnd<>'' then
      begin
        n:= Pos(CmtEnd, Result);
        if n=0 then Exit;
        Delete(Result, n, MaxInt);
      end;

      Result:= WideTrim(Result);
    finally
      Result:= IntToStr(NLine+1)+':  '+Result;
    end;
  end;
  //
  function IsBk(const s: Widestring): boolean;
  var
    n: Integer;
  begin
    if CmtBegin='' then
      begin Result:= false; Exit end;
    n:= Pos(CmtBegin1, s);
    if n=0 then
      n:= Pos(CmtBegin2, s);
    Result:= n>0;
    if Result and (CmtEnd<>'') then
      Result:= PosEx(CmtEnd, s, n)>0;
  end;
  //
var
  i, nPos: Integer;
  L: TList;
  L_Str: TTntStringList;
  ed: TSyntaxMemo;
  MLine: boolean;
  SLastLexer, SLexer: string;
begin
  ed:= CurrentEditor;
  L:= TList.Create;
  L_Str:= TTntStringList.Create;

  CmtBegin:= '';
  CmtEnd:= '';
  CmtBegin1:= '';
  CmtBegin2:= '';
  SLastLexer:= '?';
  StripBkmk:= Bool(SynHiddenOption('BkStrip', 0));

  try
    //create bookmarks list
    Screen.Cursor:= crHourGlass;
    try
      for i:= 0 to ed.Lines.Count-1 do
      begin
        SLexer:= CurrentLexerForLine(ed, i);
        if SLexer<>SLastLexer then
        begin
          SLastLexer:= SLexer;
          DoGetCommentProps(SLexer, true, CmtBegin, CmtEnd, MLine);
          CmtBegin1:= CmtBegin+'NOTE';
          CmtBegin2:= CmtBegin+'TODO';
        end;

        if IsBk(ed.Lines[i]) then
        begin
          L.Add(Pointer(i));
          L_Str.Add(BkStr(ed.Lines[i], i));
        end;
      end;
    finally
      Screen.Cursor:= crDefault;
    end;

    if L.Count=0 then
      begin DoHint(DKlangConstW('zMNoBookmk')); MsgBeep; Exit end;

    with TfmGotoBkmk.Create(nil) do
    try
      FIniFN:= Self.SynHistoryIni;

      //fill form
      List.Font.Assign(ed.Font);
      List.Items.Clear;
      List.Items.AddStrings(L_Str);

      //select curr bookmk
      List.ItemIndex:= 0;
      nPos:= ed.CurrentLine;
      for i:= L.Count-1 downto 0 do
        if Integer(L[i]) <= nPos then
          begin List.ItemIndex:= i; Break end;
      List.Selected[List.ItemIndex]:= true;

      if ShowModal=mrOk then
      begin
        i:= List.ItemIndex;
        if i>=0 then
        begin
          //Shift pressed?
          if GetKeyState(vk_shift)<0 then
            EditorSelectToPosition(ed, ed.CaretPosToStrPos(Point(0, Integer(L[i]))))
          else
            ed.CaretPos:= Point(0, Integer(L[i]));

          EditorCenterPos(ed, true{GotoMode}, opFindOffsetTop);
        end;
      end;
    finally
      Free
    end;
  finally
    FreeAndNil(L_Str);
    FreeAndNil(L);
  end;
end;


procedure TfmMain.TBXItemBkGotoClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_GotoBookmarkDialog);
end;

procedure TfmMain.TBXItemFavAddFileClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_Fav_AddFile);
end;

procedure TfmMain.TBXItemFavManageClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_Fav_Organize);
end;

procedure TfmMain.DoAddFav(const fn: Widestring);
var
  fn_ini: string;
  L: TTntStringList;
begin
  if fn='' then
    begin MsgBeep; Exit end;
  fn_ini:= SynFavIni;

  L:= TTntStringList.Create;
  try
    if FileExists(fn_ini) then
      L.LoadFromFile(fn_ini);
    if L.IndexOf(fn)<0 then
    begin
      L.Add(fn);
      L.SaveToFile(fn_ini);
    end;
  finally
    FreeAndNil(L);
  end;

  DoHint(WideFormat(DKLangConstW('zMFavAdded'), [fn]));
end;

procedure TfmMain.acFavManageExecute(Sender: TObject);
begin
  DoFavoritesDialog;
end;

procedure TfmMain.DoFavoritesDialog(ATab: Integer = -1);
begin
  with TfmFav.Create(nil) do
  try
    FIniFN:= SynFavIni;
    FOptFN:= SynHistoryIni;
    FFavTab:= ATab;
    
    if ShowModal=mrOk then
    begin
      if FCurrentFileName<>'' then
        if IsFileExist(FCurrentFileName) then
        begin
          if IsFileProject(FCurrentFileName) then
            DoOpenProject(FCurrentFileName)
          else
            DoOpenFile(FCurrentFileName);
        end
        else
        if not DoPlugin_OpenFavorite(FCurrentFileName) then
          MsgNoFile(FCurrentFileName);
    end;
  finally
    Free
  end;
end;

procedure TfmMain.ApplyFramesOptions;
var
  i, N: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    with FramesAll[i] do
    begin
      EditorMaster.HintProps.DelayBefore:= opAcpHintDelay;
      EditorSlave.HintProps.DelayBefore:= opAcpHintDelay;

      N:= IfThen(opShowBookmarkColumn, cGutterBandSizeBm, 0);
      EditorMaster.Gutter.Bands[cBandBoommarks].Width:= N;
      EditorSlave.Gutter.Bands[cBandBoommarks].Width:= N;

      HyperlinkHighlighter.Style.Font.Color:= opColorLink;
      ShowMap:= opMicroMap;

      DoTitleChanged;
    end;
end;

procedure TfmMain.ApplyFramesGutters;
var
  i: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    UpdateGutter(FramesAll[i]);
end;

procedure TfmMain.TBXItemCtxAddColorClick(Sender: TObject);
begin
  DoAddRecentColor(FPopupColor);
end;

function TfmMain.GetRecentColors: string;
var
  i: Integer;
begin
  Result:= '';
  with TbxSubmenuColors do
    for i:= Count-1 downto cColorIdxMin do
      Result:= Result+ SColorToHtmlCode(Items[i].Tag)+',';
end;

procedure TfmMain.SetRecentColors(const Str: string);
var
  S, SItem: Widestring;
  Code: Integer;
begin
  DoClearRecentColors;
  S:= Str;
  repeat
    SItem:= SGetItem(S); //#AABBCC
    Delete(SItem, 1, 1); //delete #
    if SItem='' then Break;
    //Msg(SItem);
    try
      Code:= SHtmlCodeToColor(SItem);
      DoAddRecentColor(Code);
    except
      Continue
    end;
  until false;
end;

procedure TfmMain.DoInitRecentColorsMenu;
var
  Item: TSpTbxItem;
  ItemSep: TSpTbxSeparatorItem;
begin
  //add menu items and separator
  with TbxSubmenuColors do
    if Count=0 then
    begin
      Item:= TSpTbxItem.Create(Self);
      Item.Caption:= 'Clear list';
      Item.Tag:= -1;
      Item.OnClick:= RecentColorClick;
      Add(Item);
      FMenuItem_Colors_Clear:= Item;

      Item:= TSpTbxItem.Create(Self);
      Item.Caption:= 'Open...';
      Item.Tag:= -1;
      Item.OnClick:= RecentColorOpen;
      Add(Item);
      FMenuItem_Colors_Open:= Item;

      Item:= TSpTbxItem.Create(Self);
      Item.Caption:= 'Save as...';
      Item.Tag:= -1;
      Item.OnClick:= RecentColorSave;
      Add(Item);
      FMenuItem_Colors_Save:= Item;

      ItemSep:= TSpTbxSeparatorItem.Create(Self);
      Add(ItemSep);
    end;
end;

procedure TfmMain.TbxSubmenuColorsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  en: boolean;
begin
  DoInitRecentColorsMenu;
  if TbxSubmenuColors.Count>0 then
    if FMenuItem_Colors_Clear<>nil then
    begin
      FMenuItem_Colors_Clear.Caption:= SStripFromTab(TBXItemFClearRecents.Caption);
      FMenuItem_Colors_Save.Caption:= SStripFromTab(TBXItemFSaveAs.Caption);
      FMenuItem_Colors_Open.Caption:= SStripFromTab(TBXItemFOpen.Caption);

      en:= ImageListColorRecent.Count>1;
      FMenuItem_Colors_Save.Enabled:= en;
      FMenuItem_Colors_Clear.Enabled:= en;
    end;
end;

function TfmMain.IsMouseOverProject: boolean;
begin
  if Assigned(fmProj) and fmProj.Visible and plTree.Visible then
    Result:= IsMouseOverControl(fmProj)
  else
    Result:= false;
end;

procedure TfmMain.DoDropFile(const fn: Widestring; IntoProj: boolean = false);
var
  i: Integer;
begin
  //drop item to Project tree
  if IntoProj then
    fmProj.DoDropItem(fn)
  else
  //open file in editor
  if IsDirExist(fn) then
    DoOpenFolder(fn)
  else
  if not IsFileExist(fn) then
  begin
    MsgBeep;
    Exit
  end
  else
  if not IsFileText(fn) and not MsgConfirmBinary(fn, Handle) then
    Exit
  else
  begin
    for i:= Low(Groups.Pages) to High(Groups.Pages) do
      if IsMouseOverControl(Groups.Pages[i]) then
      begin
        //issue: if pages1 is empty (one empty tab), pages2 empty, then
        //dropped on pages1 all times (even with mouse on pages2). maybe leave it.
        //
        Groups.PagesCurrent:= Groups.Pages[i];
        DoOpenFile(fn);
        Break
      end;
  end;
end;

procedure TfmMain.ProjAddEditorFile(Sender: TObject; Files: TTntStrings);
begin
  with CurrentFrame do
    if FileName<>'' then
      if not IsFtp then
        Files.Add(FileName);
end;

procedure TfmMain.ProjAddEditorFiles(Sender: TObject; Files: TTntStrings);
var
  i: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    with FramesAll[i] do
      if FileName<>'' then
        if not IsFtp then
          Files.Add(FileName);
end;

procedure TfmMain.ProjFileOpen(Sender: TObject; Files: TTntStrings);
var
  i: Integer;
  fn: Widestring;
  Bads: TTntStringList;
const
  cCnt = 10; //ask additional confirmation when many files opened
begin
  if (Files.Count>cCnt) then
    if not MsgConfirmManyOpen(Files.Count, Handle) then Exit;

  Bads:= TTntStringList.Create;
  try
    for i:= 0 to Files.Count-1 do
    begin
      fn:= Files[i];
      if IsFileExist(fn) then
      begin
        if IsFileText(fn) or MsgConfirmBinary(fn, Handle) then
        begin
          DoOpenFile(fn);
          FocusEditor;
        end;
      end
      else
        Bads.Add(fn);
    end;

    if Bads.Count>0 then
    begin
      if Bads.Count>cCnt then
      begin
        while Bads.Count>cCnt do
          Bads.Delete(Bads.Count-1);
        Bads.Add('...');
      end;
      MsgNoFile(Bads.Text);
    end;
  finally
    FreeAndNil(Bads);
  end;
end;

procedure TfmMain.ProjGetLexers(Sender: TObject; Files: TTntStrings);
begin
  Files.Add(' '+DKLangConstW('None')); //needed for proj too
  DoEnumLexers(Files);
end;

procedure TfmMain.ProjGetLexer(Sender: TObject; Files: TTntStrings);
begin
  Files.Add(CurrentLexerForFile);
end;

procedure TfmMain.TBXItemFavAddProjClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_Fav_AddProject);
end;

procedure TfmMain.acFavAddFileExecute(Sender: TObject);
begin
  DoAddFav(CurrentFrame.FileName);
end;

procedure TfmMain.acFavAddProjExecute(Sender: TObject);
begin
  DoAddFav(CurrentProjectFN);
end;

function TfmMain.CurrentProjectFN: Widestring;
begin
  Result:= '';
  if Assigned(fmProj) then
    Result:= fmProj.ProjectFN;
end;

procedure TfmMain.DoOpenProject(const fn: Widestring);
begin
  if IsFileExist(fn) then
  begin
    ecToggleFocusProject.Execute;
    if Assigned(fmProj) then
      fmProj.ProjectFN:= fn;
  end
  else
    MsgBeep;
end;

procedure TfmMain.TBXSubmenuItemFavPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  TBXItemFavAddFile.Enabled:= CurrentFrame.FileName<>'';
  TBXItemFavAddProj.Enabled:= CurrentProjectFN<>'';
end;

procedure TfmMain.ProjGetWorkDir(Sender: TObject; Files: TTntStrings);
begin
  Files.Add(LastDir);
end;

procedure TfmMain.TBXItemTabAddToProjClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FClickedFrame;
  if (F<>nil) and (F.FileName<>'') then
    if not F.IsFtp then
      if Assigned(fmProj) then
        fmProj.DoDropItem(F.FileName);
end;

function TfmMain.CurrentProjectMainFN: Widestring;
begin
  Result:= '';
  if Assigned(fmProj) then
    Result:= fmProj.FOpts.MainFN;
end;

function TfmMain.CurrentProjectDir: Widestring;
begin
  Result:= '';
  if Assigned(fmProj) then
    Result:= WideExtractFileDir(fmProj.ProjectFN);
end;

function TfmMain.CurrentProjectWorkDir: Widestring;
begin
  Result:= '';
  if Assigned(fmProj) then
    Result:= fmProj.FOpts.WorkDir;
end;

procedure TfmMain.TreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Node: TTreeNode;
begin
  Node:= Tree.GetNodeAt(MousePos.X, MousePos.Y);
  if Node<>nil then
    Tree.Selected:= Node;
end;

procedure TfmMain.ApplyProj;
//needed to apply color/font and "Untitled" str to project pane
begin
  if Assigned(fmProj) then
  begin
    ApplyFonts;
    fmProj.UpdateTitle;
  end;
end;

procedure TfmMain.TBXItemRightClipsClick(Sender: TObject);
begin
  TabsRight.TabIndex:= Ord(tbTextClips);
end;

procedure TfmMain.ClipsInsert(Sender: TObject; const AText: Widestring; AIsSnippet: boolean);
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Ed.ReadOnly then Exit;
  if AIsSnippet then
    EditorInsertSnippet(Ed, AText, Ed.SelText, FrameOfEditor(Ed).FileName)
  else
    Ed.InsertText(AText);
  FocusEditor;
end;

procedure TfmMain.ecToggleFocusClipsExecute(Sender: TObject);
begin
  if not plClip.Visible then
  begin
    ecShowClip.Execute;
    TabsRight.TabIndex:= Ord(tbTextClips);
    if Assigned(fmClips) then
      if fmClips.ListNames.CanFocus then
        fmClips.ListNames.SetFocus;
  end
  else
  if Assigned(fmClips) and fmClips.ListNames.Focused then
    FocusEditor
  else
  begin
    TabsRight.TabIndex:= Ord(tbTextClips);
    if Assigned(fmClips) then
      if fmClips.ListNames.CanFocus then
        fmClips.ListNames.SetFocus
  end;
end;

procedure TfmMain.TBXItemClipsAddTextClick(Sender: TObject);
const
  cName = 'snippet';
var
  S: Widestring;
begin
  S:= SReplaceAllEols(CurrentEditor.SelText, '\n');

  if Pos('=', S)>0 then
    S:= cName+'='+S;

  if not MsgInput('zClipEnter', S) then Exit;
  if WideTrim(S)='' then Exit;

  if Assigned(fmClips) then
    fmClips.DoAddClip(S);
end;

procedure TfmMain.TBXItemClipsEditClick(Sender: TObject);
begin
  if Assigned(fmClips) then
    DoOpenFile(fmClips.CurrentClipFN);
end;

procedure TfmMain.TBXItemClipsDirClick(Sender: TObject);
var
  dir: Widestring;
begin
  if Assigned(fmClips) then
  begin
    //dir:= WideExtractFileDir(fmClips.GetCurrentClipFN);
    dir:= SynClipsDir;
    FOpenURL(dir, Handle);
  end;
end;

procedure TfmMain.RecentColorOpen(Sender: TObject);
begin
  with OD_Swatch do
  begin
    InitialDir:= LastDir;
    FileName:= '';
    if Execute then
      RecentColorsStr:= DoLoadStringFromIni(FileName);
  end;
end;

function TfmMain.DoLoadStringFromIni(const fn: string): string;
begin
  with TIniFile.Create(fn) do
  try
    Result:= ReadString('ini', 'str', '');
  finally
    Free
  end;
end;

procedure TfmMain.DoSaveStringToIni(const fn: string; const Str: string);
begin
  with TIniFile.Create(fn) do
  try
    WriteString('ini', 'str', Str);
  finally
    Free
  end;
end;


procedure TfmMain.RecentColorSave(Sender: TObject);
begin
  with SD_Swatch do
  begin
    InitialDir:= LastDir;
    FileName:= '';
    if Execute then
      DoSaveStringToIni(FileName, RecentColorsStr);
  end;
end;

procedure TfmMain.ProjGetProjDir(Sender: TObject; Files: TTntStrings);
begin
  Files.Add(opLastDirProject);
end;

procedure TfmMain.ProjSetProjDir(Sender: TObject; Files: TTntStrings);
var
  S: Widestring;
begin
  if Files.Count>0 then
    opLastDirProject:= Files[0];
  S:= SCollapseFilenameDrive(opLastDirProject, SynDir);

  with TIniFile.Create(SynIni) do
  try
    WriteString('Hist', 'DirProj', UTF8Encode(S));
  finally
    Free
  end;
end;

{
procedure TfmMain.DoClipsItemCopy;
var
  s: Widestring;
begin
  s:= fmClips.GetCurrentClip;
  if s<>'' then
    TntClipboard.AsWideText:= s;
end;
}

procedure TfmMain.ApplyInst;
begin
  TbxItemFNewWin.Enabled:= SynExe and not opSingleInstance;
end;

procedure TfmMain.ApplyDefaultFonts;
const
  cc = 'Consolas';
begin
  if Screen.Fonts.IndexOf(cc)>=0 then
  begin
    TemplateEditor.Font.Name:= cc;
    TemplateEditor.LineNumbers.Font.Name:= cc;
    TemplateEditor.HorzRuler.Font.Name:= cc;
    MemoConsole.Font.Name:= cc;
    edConsole.Font.Name:= cc;
  end;
end;

procedure TfmMain.TBXTabColorChange(Sender: TObject);
begin
  DoSetTabColorValue(TbxTabColor.Color);
end;

procedure TfmMain.DoSetFrameTabColor(F: TEditorFrame; NColor: TColor);
var
  NPages, NTab: Integer;
  D: TATTabData;
begin
  Groups.PagesAndTabIndexOfControl(F, NPages, NTab);
  if (NPages<0) or (NTab<0) then Exit;
  D:= Groups.Pages[NPages].Tabs.GetTabData(NTab);
  if D=nil then Exit;

  F.TabColor:= NColor;
  D.TabColor:= NColor;
  Groups.Invalidate;
end;

procedure TfmMain.DoSetTabColorValue(NColor: TColor);
begin
  DoSetFrameTabColor(FClickedFrame, NColor);
end;

procedure TfmMain.DoSetTabColorIndex_Current(NIndex: Integer);
begin
  FClickedFrame:= CurrentFrame;
  DoSetTabColorIndex(NIndex);
end;

procedure TfmMain.DoSetTabColorIndex(NIndex: Integer);
var
  NColor: TColor;
begin
  case NIndex of
    0:
      NColor:= clNone;
    -1:
      begin
        with ColorDialogTabs do
        begin
          if Execute then
            NColor:= Color
          else
            Exit;
        end;
      end;
    1..10:
      NColor:= opTabColors[NIndex-1];
    else
      raise Exception.Create('Unknown tab color index');
  end;

  DoSetFrameTabColor(FClickedFrame, NColor);
end;

procedure TfmMain.TBXSubmenuTabColorPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  F: TEditorFrame;
begin
  F:= FClickedFrame;
  if F<>nil then
  begin
    TbxTabColor.Color:= F.TabColor;
    TbxItemTabColorDef.Checked:= F.TabColor=clNone;
  end;
end;

procedure TfmMain.TBXItemTabColorMiscClick(Sender: TObject);
begin
  with ColorDialogTabs do
    if Execute then
      DoSetTabColorValue(Color);
end;

procedure TfmMain.TBXItemTabColorDefClick(Sender: TObject);
begin
  DoSetTabColorValue(clNone);
end;

function TfmMain.GetTabColors: Widestring;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to High(opTabColors) do
    Result:= Result+ SColorToHtmlCode(opTabColors[i])+',';
end;

procedure TfmMain.SetTabColors(S: Widestring);
var
  SItem: Widestring;
  i: Integer;
begin
  if S='' then Exit;
  for i:= 0 to High(opTabColors) do
  begin
    SItem:= SGetItem(S);
    Delete(SItem, 1, 1);
    if SItem='' then Break;
    opTabColors[i]:= SHtmlCodeToColor(SItem);
  end;
end;

procedure TfmMain.ClipsInsPress;
begin
  TBXItemClipsAddText.Click;
end;

function TfmMain.CurrentSessionFN: string;
begin
  Result:= FSessionFN;
end;

function TfmMain.CurrentContentFN(Unicode: boolean): Widestring;
var
  S, Ext: Widestring;
begin
  Ext:= WideExtractFileExt(CurrentFrame.FileName);
  if Ext='' then
    Ext:= '.txt';

  Result:= FTempDir + '\SynwText' + Ext;
  FDelete(Result);

  with CurrentEditor do
    S:= Lines.FText;

  if Unicode then
    with TTntStringList.Create do
    try
      Text:= S;
      SaveToFile(Result);
    finally
      Free
    end
  else
    with TStringList.Create do
    try
      Text:= S;
      SaveToFile(Result);
    finally
      Free
    end;
end;


function TfmMain.CurrentSelectionFN(Unicode, Numbered: boolean): Widestring;
var
  S: Widestring;
  i: integer;
begin
  if not Numbered then
  begin
    Result:= FTempDir + '\SynwSel.txt';
    FDelete(Result);
  end
  else
  begin
    for i:= 1 to 1000 do
    begin
      Result:= FTempDir + Format('\SynwSel_%d.txt', [i]);
      if not IsFileExist(Result) then Break;
    end;
  end;

  with CurrentEditor do
    if HaveSelection then
      S:= SelText
    else
      S:= Lines.FText;

  if Unicode then
    with TTntStringList.Create do
    try
      Text:= S;
      SaveToFile(Result);
    finally
      Free
    end
  else
    with TStringList.Create do
    try
      Text:= S;
      SaveToFile(Result);
    finally
      Free
    end;
end;

procedure TfmMain.ecSmartHlExecute(Sender: TObject);
begin
  opHiliteSmart:= not opHiliteSmart;
  UpdateStatusBar;
end;

procedure TfmMain.TBXItemBkDropPortableClick(Sender: TObject);
begin
  ecDropPortableBk.Execute;
end;

procedure TfmMain.ecDropPortableBkExecute(Sender: TObject);
var
  S, SPadding: Widestring;
  s1, s2: string;
  MLine: boolean;
begin
  if CurrentEditor.ReadOnly then Exit;

  DoGetCommentProps(CurrentLexer, true, s1, s2, MLine);
  if s1='' then Exit;

  if not MsgInput('zMBkName', S) then Exit;

  with CurrentEditor do
  begin
    SPadding:= ''; //StringOfChar(' ', 2);
    InsertText(SPadding+s1+'NOTE: '+s+' '+s2);
  end;
end;


procedure TfmMain.acRenameExecute(Sender: TObject);
var
  F: TEditorFrame;
  fn, fn_new, sName: Widestring;
  NColor: TColor;
begin
  F:= CurrentFrame;
  fn:= F.FileName;
  if fn='' then
    begin MsgBeep; Exit end;
  NColor:= F.TabColor;

  if F.IsFtp then
    if not MsgConfirmFtp then
      Exit
    else
      F.FreeFtpInfo;

  repeat
    sName:= WideExtractFileName(fn);
    if not DoInputFilename(DKLangConstW('zMRename'), sName) then Exit;

    fn_new:= WideExtractFilePath(fn) + sName;

    //allow to rename to non-existent name
    if not IsFileExist(fn_new) then
      Break
    else
    //allow to only change filename case
    if WideUpperCase(fn_new) = WideUpperCase(fn) then
      Break
    else
    //don't allow to overwrite existing file
      MsgBeep;
  until false;

  acClose.Execute;
  if not MoveFileW(PWChar(fn), PWChar(fn_new)) then
  begin
    MsgRenameError(fn, fn_new, Handle);
    DoOpenFile(fn);
  end
  else
  begin
    //rename successful
    SynMruFiles.DeleteItem(fn);
    DoOpenFile(fn_new);
    DoPlugin_RefreshFiles(fn_new);
    DoProjectRenameFile(fn, fn_new);
  end;

  if NColor<>clNone then
    DoSetFrameTabColor(CurrentFrame, NColor);
end;

procedure TfmMain.ApplySpell;
var
  i: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    UpdateFrameSpell(FramesAll[i]);
end;


function TfmMain.OppositeFrame: TEditorFrame;
begin
  case Groups.PagesIndexOf(Groups.PagesCurrent) of
    1: Result:= GetCurrentFrameInPages(Groups.Pages2);
    else Result:= GetCurrentFrameInPages(Groups.Pages1);
  end;
end;

function TfmMain.CurrentFileName(Id: TSynGroupId): Widestring;
var
  F: TEditorFrame;
begin
  Result:= '';
  case Id of
    cSynGroupCurrent: F:= CurrentFrame;
    cSynGroupOpposite: F:= OppositeFrame;
    cSynGroup1: F:= GetCurrentFrameInPages(Groups.Pages1);
    cSynGroup2: F:= GetCurrentFrameInPages(Groups.Pages2);
    cSynGroup3: F:= GetCurrentFrameInPages(Groups.Pages3);
    cSynGroup4: F:= GetCurrentFrameInPages(Groups.Pages4);
    cSynGroup5: F:= GetCurrentFrameInPages(Groups.Pages5);
    cSynGroup6: F:= GetCurrentFrameInPages(Groups.Pages6);
    else F:= nil;
  end;
  if F<>nil then
    Result:= F.FileName;
end;

procedure TfmMain.TBXItemRunNumConvClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_NumericConverterDialog);
end;

procedure TfmMain.ecNumericConverterExecute(Sender: TObject);
begin
  if not Assigned(fmNumConv) then
  begin
    fmNumConv:= TfmNumConv.Create(Self);
    fmNumConv.FormStyle:= fsStayOnTop;
    fmNumConv.OnInsert:= NumConvInsert;

    with TIniFile.Create(SynHistoryIni) do
    try
      fmNumConv.Left:= ReadInteger('Win', 'NConvX', 400);
      fmNumConv.Top:= ReadInteger('Win', 'NConvY', 300);
    finally
      Free
    end;
  end;
  fmNumConv.Show;
end;

procedure TfmMain.NumConvInsert(Sender: TObject; const S: string; Typ: TSynNumType);
var
  SCode: string;
begin
  SCode:= S;
  if Typ=numHex then
    SCode:= EditorFormatHexCode(CurrentEditor, S);
  CurrentEditor.InsertText(SCode);
end;

procedure TfmMain.TBXItemEUnindentClick(Sender: TObject);
begin
  if Assigned(fmNumConv) and (fmNumConv.ActiveControl.Focused) then
  begin
    fmNumConv.SelNext;
  end
  else
    CurrentEditor.ExecCommand(smBlockUnindent);
end;

procedure TfmMain.ecIndentLike1stExecute(Sender: TObject);
var
  ed: TSyntaxMemo;
  i, Ln1, Ln2: Integer;
  s, Ind_Old, Ind_New: ecString;
begin
  ed:= CurrentEditor;
  with ed do
    if ReadOnly or (Lines.Count=0) then Exit;
  if ed.SelLength=0 then
    begin MsgNoSelection; Exit end;

  EditorGetSelLines(ed, Ln1, Ln2);
  if Ln2=Ln1 then
    begin MsgWarn(DKLangConstW('zMSelMulLine'), Handle); Exit end;

  with ed do
  begin
    Ind_Old:= SIndentOf(Lines[Ln1]);

    BeginUpdate;
    DoProgressShow;

    try
      for i:= Ln1+1 to Ln2 do
      begin
        if IsProgressStopped(i-Ln1+1, Ln2-Ln1+1) then
          Break;

        s:= Lines[i];
        Ind_New:= SIndentOf(s);
        if Ind_New<>Ind_Old then
        begin
          s:= Ind_Old + Copy(s, Length(Ind_New)+1, MaxInt);
          DoReplaceLine(ed, i, s, true{ForceUndo});
        end;
      end;
    finally
      DoProgressHide;
      EndUpdate;
    end;
  end;

  FocusEditor;
end;

procedure TfmMain.TBXItemEToggleLineCommentClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ToggleLineComment);
end;

procedure TfmMain.TBXItemEToggleStreamCommentClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ToggleStreamComment);
end;

procedure TfmMain.DoPlugins_PreinstallDefaults;
var
  DirExe: string;
begin
  DirExe:= ExtractFileDir(Application.ExeName);
  //Panel plugins
  //DoPlugins_PreinstallPlugin('Explorer', DirExe+'\Plugins\Explorer\install.inf', true);
  DoPlugins_PreinstallPlugin('SynFTP', DirExe+'\Plugins\SynFTP\install.inf', true);
  //Command plugins
  DoPlugins_PreinstallPlugin('Color Picker', DirExe+'\Py\syn_color_picker\install.inf', false);
  DoPlugins_PreinstallPlugin('HTML Tidy\Menu', DirExe+'\Py\syn_html_tidy\install.inf', false);
  DoPlugins_PreinstallPlugin('Make Plugin', DirExe+'\Py\syn_make_plugin\install.inf', false);
end;

procedure TfmMain.DoPlugins_PreinstallPlugin(const AId, fn_inf: string; AIsPanelPlugin: boolean);
var
  s_section, s_title: string;
  n_type: TSynAddonType;
begin
  if not FileExists(fn_inf) then Exit;
  if AIsPanelPlugin then s_section:= 'Panels' else s_section:= 'Commands';

  with TIniFile.Create(SynPluginsIni) do
  try
    if ReadString(s_section, AId, '')<>'' then Exit
  finally
    Free
  end;

  with TIniFile.Create(fn_inf) do
  try
    s_title:= ReadString('info', 'title', '');
    n_type:= StringToAddonType(ReadString('info', 'type', ''));
  finally
    Free
  end;

  if n_type=cAddonTypeNone then Exit;
  MsgInfo('Preinstalling plugin: '+s_title, Handle);
  DoOpenArchive_HandleIniSections(fn_inf,
    ExtractFileName(ExtractFileDir(fn_inf)),
    n_type);
end;



procedure TfmMain.DoPlugins_LoadAll;
var
  fn_plug_ini: string;
begin
  DoPlugins_PreinstallDefaults;

  fn_plug_ini:= SynPluginsIni;
  if not IsFileExist(fn_plug_ini) then
    Exit;

  DoPlugins_LoadPanels(fn_plug_ini);
  DoPlugins_LoadGotoDef(fn_plug_ini);
  DoPlugins_LoadAutoComplete(fn_plug_ini);
  DoPlugins_LoadCommands(fn_plug_ini);
  DoPlugins_LoadEvents(fn_plug_ini);

  //debug
  //DoPlugins_Test;
end;

procedure TfmMain.DoPlugins_InitTabs;
var
  i: Integer;
begin
  //disable plugins in Lister
  if not SynExe then Exit;

  DoPlugins_LoadAll;

  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if SCaption<>'' then
        TabsLeft.AddTab(-1, SCaption);
end;

function TfmMain.DoPlugin_PanelFN(Index: Integer): Widestring;
begin
  Result:= '';
  if Index<0 then Exit;
  Result:= ExtractFileName(FPluginsPanel[Index].SFileName);
end;

function TfmMain.DoPlugin_PanelCaption(Index: Integer): Widestring;
begin
  Result:= '';
  if Index<0 then Exit;
  Result:= FPluginsPanel[Index].SCaption;
  if FPluginsPanel[Index].SState<>'' then
    Result:= Result + ' - ' + FPluginsPanel[Index].SState;
end;

procedure TfmMain.DoPlugin_PanelTabClick(N: Integer);
begin
  if not ((N>=Low(FPluginsPanel)) and (N<=High(FPluginsPanel))) then Exit;
  if FPluginsPanel[N].SCaption='' then Exit;

  Tree.Visible:= false;
  Tree.SyntaxMemo:= nil;
  if Assigned(fmProj) then
    fmProj.Visible:= false;

  DoPlugin_LoadPanel(N);
  DoPlugin_Show(N);
end;

procedure TfmMain.DoPlugin_LoadPanel(Index: Integer);
var
  AParent: THandle;
  AIni: Widestring;
begin
  with FPluginsPanel[Index] do
  begin
    //already loaded?
    if FWindow<>0 then Exit;

    if not IsFileExist(SFileName) then
    begin
      MsgNoFile(SFileName);
      Exit;
    end;

    FDll:= LoadLibrary(PChar(string(SFileName)));
    if FDll=0 then
    begin
      MsgError('Can''t load dll:'#13+SFileName, Handle);
      Exit
    end;

    FSynInit:= GetProcAddress(FDll, 'SynInit');
    if @FSynInit=nil then
    begin
      MsgError('Can''t find SynInit'#13+SFileName, Handle);
      Exit
    end;

    FSynOpenForm:= GetProcAddress(FDll, 'SynOpenForm');
    if @FSynOpenForm=nil then
    begin
      MsgError('Can''t find SynOpenForm'#13+SFileName, Handle);
      Exit
    end;

    FSynCloseForm:= GetProcAddress(FDll, 'SynCloseForm');
    if @FSynCloseForm=nil then
    begin
      MsgError('Can''t find SynCloseForm'#13+SFileName, Handle);
      Exit
    end;

    FSynAction:= GetProcAddress(FDll, 'SynAction');
    if @FSynAction=nil then
    begin
      MsgError('Can''t find SynAction'#13+SFileName, Handle);
      Exit
    end;

    AIni:= SynPluginIni(SCaption);
    FSynInit(PWChar(AIni), @_SynActionProc);
    //MsgInfo(AIni);

    AParent:= plTree.Handle;
    FForm:= FSynOpenForm(AParent, FWindow);
    Windows.SetParent(FWindow, AParent);
  end;

  DoPlugin_SetColors(Index);
  DoPlugins_Resize;
end;

procedure TfmMain.DoPlugins_Resize;
var
  X, Y, XSize, YSize, i: Integer;
begin
  X:= 0;
  Y:= IfThen(opShowPanelTitles, plTree.CaptionPanelSize.Y, 0);
  XSize:= plTree.ClientWidth;
  YSize:= plTree.ClientHeight - Y;
  if Assigned(TabsLeft) then
    Dec(YSize, TabsLeft.Height);

  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if FWindow<>0 then
        SetWindowPos(FWindow, 0, X, Y, XSize, YSize, 0);
end;

procedure TfmMain.DoPlugins_Close;
var
  i: Integer;
begin
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if (FForm<>nil) and Assigned(FSynCloseForm) then
        FSynCloseForm(FForm);
end;

procedure TfmMain.DoPlugin_Show(N: Integer);
var
  i: Integer;
begin
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if FWindow<>0 then
        ShowWindow(FWindow, IfThen(i=N, sw_show, sw_hide));
end;

function TfmMain.DoPlugin_OpenFavorite(const AFileName: Widestring): boolean;
var
  N: Integer;
  AName: string;
  ADir: Widestring;
begin
  N:= Pos('::', AFileName);
  Result:= N>0;
  if not Result then Exit;

  AName:= Copy(AFileName, 1, N-1);
  ADir:= Copy(AFileName, N+2, MaxInt);

  N:= DoPlugin_OpenPanelByName(AName);
  if N>=0 then
  begin
     with FPluginsPanel[N] do
       if (FForm<>nil) and Assigned(FSynAction) then
         FSynAction(FForm, cActionNavigateToFile, PWChar(ADir), nil, nil, nil);
  end;
end;

function TfmMain.DoPlugin_OpenPanelByName(const AName: string): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    if FPluginsPanel[i].SCaption=AName then
    begin
      if not plTree.Visible then
        ecShowTree.Execute;

      Result:= i;
      TabsLeft.TabIndex:= Ord(tbPlugin1)+i;
      Exit
    end;
end;

function TfmMain.PluginAction_OpenFile(const fn: Widestring): Integer;
begin
  if fn='' then
  begin
    Result:= cSynError;
    Exit;
  end;

  if DoConfirmMaybeBinaryFile(fn) then
  begin
    DoOpenFile(fn);
    Result:= cSynOK;
  end
  else
    Result:= cSynError;
end;

function TfmMain.PluginAction_OpenFtpFile(const fn: Widestring; AInfoPtr: Pointer; AInfoSize: Integer): Integer;
begin
  Result:= PluginAction_OpenFile(fn);
  if Result=cSynOK then
    CurrentFrame.SetFtpInfo(AInfoPtr, AInfoSize);
end;

function TfmMain.PluginAction_GetOpenedFN(id: Integer; ptr: PWideChar): Integer;
var
  F: TEditorFrame;
begin
  F:= Plugin_FrameById(id);
  if (F<>nil) then //return OK for unnamed tab
  begin
    Result:= cSynOK;
    if F.FileName='' then
      ptr^:= #0
    else
      lstrcpynW(ptr, PWChar(F.FileName), cSynMaxPath);
  end
  else
    Result:= cSynError;
end;

function TfmMain.PluginAction_SetState(Index: Integer; Ptr: PWideChar): Integer;
begin
  Result:= cSynOK;
  if Index<0 then Exit;

  FPluginsPanel[Index].SState:= Widestring(Ptr);
  if TabsLeft.TabIndex=Ord(tbPlugin1)+Index then
    plTree.Caption:= DoPlugin_PanelCaption(Index);
end;

function TfmMain.PluginAction_GetProjectFN(id: Integer; ptr: PWideChar): Integer;
var
  fn: Widestring;
begin
  Result:= cSynError;
  if (fmProj=nil) then Exit;

  if (id=cSynIdCurrentFile) then
  begin
    fn:= fmProj.FOpts.MainFN;
    lstrcpynW(ptr, PWChar(fn), cSynMaxPath);
    Result:= cSynOK;
    Exit
  end;

  if IsProjectEmpty or (id<0) or (id>=fmProj.TreeProj.Items.Count) then
    Exit;

  fn:= fmProj.GetFN(fmProj.TreeProj.Items[id]);
  lstrcpynW(ptr, PWChar(fn), cSynMaxPath);
  Result:= cSynOK;
end;


function TfmMain.Plugin_FrameById(id: Integer): TEditorFrame;
begin
  if (id=cSynIdCurrentFile) then
    Result:= CurrentFrame
  else
  if (id=cSynIdOppositeFile) then
    Result:= OppositeFrame
  else
  if (id>=0) and (id<FrameAllCount) then
    Result:= FramesAll[id]
  else
    Result:= nil;
end;

function TfmMain.PluginAction_SaveFile(id: Integer; ACanPrompt: boolean): Integer;
var
  F: TEditorFrame;
begin
  F:= Plugin_FrameById(id);
  if (F<>nil) and ((F.FileName<>'') or ACanPrompt) then
  begin
    SaveFrame(F, False);
    Result:= cSynOK;
  end
  else
    Result:= cSynError;
end;

{
function TfmMain.PluginFilename(AHandle: Pointer): string;
var
  i: Integer;
begin
  Result:= '';
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
    begin
      if FForm=nil then Break;
      if FForm=AHandle then
        begin Result:= SFileName; Break end;
    end;
end;
}

function TfmMain.PluginAction_GetMsg(const ADllFN, AMsg: Widestring; AResult: PWideChar): Integer;
  //
  function GetFN(const fn_dll, Suffix: string): string;
  begin
    Result:= ChangeFileExt(fn_dll, '.'+Suffix+'.lng');
  end;
  //
var
  fn_lng, fn_en_lng: string;
  S: Widestring;
begin
  fn_lng:= GetFN(ADllFN, FHelpLangSuffix);
  fn_en_lng:= GetFN(ADllFN, 'En');

  S:= DoReadLangMsg(fn_lng, fn_en_lng, AMsg);

  lstrcpynW(AResult, PWChar(S), cSynMaxMsg);
  Result:= cSynOK;
end;

function TfmMain.DoPlugin_PanelHandleToIndex(AHandle: Pointer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    if FPluginsPanel[i].FForm = AHandle then
      begin Result:= i; Exit end;
end;

function TfmMain.PluginAction(AHandle: Pointer; AName: PWideChar; A1, A2, A3, A4: Pointer): Integer; stdcall;
var
  act: Widestring;
begin
  act:= AName;

  //---------------------
  if (act=cActionGetText) then
  begin
    Result:= PluginAction_GetText(Integer(A1), A2, PInteger(A3)^);
    Exit;
  end;

  //---------------------
  if (act=cActionSetText) then
  begin
    Result:= PluginAction_SetText(Integer(A1), A2, Integer(A3));
    Exit;
  end;

  //---------------------
  if (act=cActionGetMsg) then
  begin
    Result:= PluginAction_GetMsg(PWChar(A1), PWChar(A2), PWChar(A3));
    Exit;
  end;

  //---------------------
  if (act=cActionGetCaretPos) then
  begin
    Result:= PluginAction_GetCaretPos(A1, A2, A3);
    Exit;
  end;

  //---------------------
  if (act=cActionSetCaretPos) then
  begin
    Result:= PluginAction_SetCaretPos(Integer(A1), Integer(A2));
    Exit;
  end;

  //---------------------
  if (act=cActionTranslatePos) then
  begin
    Result:= PluginAction_TranslatePos(PInteger(A1)^, PInteger(A2)^, PInteger(A3)^, Bool(A4));
    Exit;
  end;

  //---------------------
  if (act=cActionGetSelection) then
  begin
    Result:= PluginAction_GetSel(PSynSelection(A1)^);
    Exit;
  end;

  //---------------------
  if (act=cActionSetSelection) then
  begin
    Result:= PluginAction_SetSel(PSynSelection(A1)^);
    Exit;
  end;

  //---------------------
  if (act=cActionSetTopLine) then
  begin
    Result:= PluginAction_SetTopLine(Integer(A1));
    Exit;
  end;

  //---------------------
  if (act=cActionReplaceText) then
  begin
    Result:= PluginAction_ReplaceText(Integer(A1), A2, Integer(A3));
    Exit;
  end;

  //---------------------
  if (act=cActionSuggestCompletion) then
  begin
    Result:= PluginAction_SuggestCompletion(PWChar(A1), Integer(A2), Bool(A3));
    Exit;
  end;

  //---------------------
  if (act=cActionParseRegex) then
  begin
    Result:= PluginAction_ParseRegex(PWChar(A1), PWChar(A2), PSynRegexArray(A3)^);
    Exit;
  end;

  //---------------------
  if (act=cActionOpenFile) then
  begin
    Result:= PluginAction_OpenFile(PWChar(A1));
    Exit;
  end;

  //---------------------
  if (act=cActionSaveFile) then
  begin
    Result:= PluginAction_SaveFile(Integer(A1), Bool(A2));
    Exit;
  end;

  //---------------------
  if (act=cActionOpenFtpFile) then
  begin
    Result:= PluginAction_OpenFtpFile(PWChar(A1), A2, Integer(A3));
    Exit
  end;

  //---------------------
  if (act=cActionControlLog) then
  begin
    Result:= PluginAction_ControlLog(PWChar(A1), Integer(A2), Integer(A3),
      DoPlugin_PanelFN(DoPlugin_PanelHandleToIndex(AHandle)));
    Exit
  end;

  //---------------------
  if (act=cActionGetProperty) then
  begin
    Result:= PluginAction_GetProp(Integer(A1), A2, Integer(A3));
    Exit;
  end;

  //---------------------
  if (act=cActionShowHint) then
  begin
    Result:= PluginAction_ShowHint(PWChar(A1));
    Exit
  end;

  //---------------------
  if (act=cActionGetOpenedFileName) then
  begin
    Result:= PluginAction_GetOpenedFN(Integer(A2), A1);
    Exit;
  end;

  //---------------------
  if (act=cActionGetProjectFileName) then
  begin
    Result:= PluginAction_GetProjectFN(Integer(A2), A1);
    Exit;
  end;

  //---------------------
  if (act=cActionAddToFavorites) then
  begin
    DoAddFav(PWChar(A1));
    Result:= cSynOK;
    Exit
  end;

  //---------------------
  if (act=cActionSetState) then
  begin
    Result:= PluginAction_SetState(DoPlugin_PanelHandleToIndex(AHandle), A1);
    Exit
  end;

  //---------------------
  Result:= cSynBadCmd;
end;


procedure TfmMain.DoPlugin_RefreshFiles(const fn: Widestring);
var
  i: Integer;
begin
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if (FForm<>nil) and Assigned(FSynAction) then
        FSynAction(FForm, cActionRefreshFileList, PWChar(fn), nil, nil, nil);
end;

procedure TfmMain.DoPlugin_RefreshLang;
var
  i: Integer;
begin
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if (FForm<>nil) and Assigned(FSynAction) then
        FSynAction(FForm, cActionUpdateLang, nil, nil, nil, nil);
end;

procedure TfmMain.DoPlugins_Repaint;
var
  i: Integer;
begin
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if (FForm<>nil) and Assigned(FSynAction) then
        FSynAction(FForm, cActionRepaint, nil, nil, nil, nil);
end;

procedure TfmMain.DoPlugin_SaveFtpFile(F: TEditorFrame);
var
  i: Integer;
begin
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if (FForm<>nil) and Assigned(FSynAction) then
        FSynAction(FForm, cActionSaveFtpFile,
          PWideChar(Widestring(F.FileName)),
          F.FtpInfoPtr, Pointer(F.FtpInfoSize), nil);
end;

procedure TfmMain.DoPlugin_SetColors(Index: Integer);
begin
  if Index>=0 then
    with FPluginsPanel[Index] do
      if (FForm<>nil) and Assigned(FSynAction) then
      begin
        FSynAction(FForm, cActionSetColor, Pointer(cColorId_Text), Pointer(ColorToRGB(Tree.Font.Color)), nil, nil);
        FSynAction(FForm, cActionSetColor, Pointer(cColorId_Back), Pointer(ColorToRGB(Tree.Color)), nil, nil);
        //
        FSynAction(FForm, cActionSetColor, Pointer(cColorId_LogNormal), Pointer(ColorToRGB(opColorFtpBlue)), nil, nil);
        FSynAction(FForm, cActionSetColor, Pointer(cColorId_LogServer), Pointer(ColorToRGB(opColorFtpGreen)), nil, nil);
        FSynAction(FForm, cActionSetColor, Pointer(cColorId_LogError), Pointer(ColorToRGB(opColorFtpRed)), nil, nil);
      end;
end;


procedure TfmMain.TBXItemOOPLogClick(Sender: TObject);
begin
  TabsOut.TabIndex:= Ord(tbPluginsLog);
end;

function TfmMain.PluginAction_ShowHint(const AMsg: Widestring): Integer;
begin
  DoHint(AMsg);
  Result:= cSynOK;
end;

function TfmMain.PluginAction_ControlLog(const AMsg: Widestring;
  const ACmd: Integer; AColor: TColor; const APluginName: string): Integer;
var
  S: Widestring;
  D: TATTabData;
begin
  Result:= cSynOK;
  case ACmd of
    cSynLogCmdHide:
      begin
        TabsOut.TabIndex:= Ord(tbOutput);
      end;

    cSynLogCmdShow:
      begin
        TabsOut.TabIndex:= Ord(tbPluginsLog);
        plOut.Show;
      end;

    cSynLogCmdAddLine:
      begin
        if opDateFmtPLog<>'' then
          S:= FormatDateTime(opDateFmtPLog, Now) + ' '
        else
          S:= '';

        ListPLog.Items.BeginUpdate;
        try
          ListPLog.Items.AddObject(S + AMsg, Pointer(AColor));
          ListPLog.ItemIndex:= ListPLog.Items.Count-1;
        finally
          ListPLog.Items.EndUpdate;
        end;

        DoPyEvent(CurrentEditor, cSynEventOnPanelLog, [
          SWideStringToPythonString(APluginName),
          SWideStringToPythonString(AMsg),
          SWideStringToPythonString(S)
          ]);
      end;

    cSynLogCmdClear:
      ListPLog.Items.Clear;

    cSynLogCmdSetCaption:
      begin
        D:= TabsOut.GetTabData(Ord(tbPluginsLog));
        if D<>nil then
        begin
          D.TabCaption:= AMsg;
          TabsOut.Invalidate;
        end;
      end;
    else
      Result:= cSynBadCmd;
  end;
end;


procedure TfmMain.TBXItemPLogFindClick(Sender: TObject);
begin
  DoFind_InPluginsLog;
end;

procedure TfmMain.DoFind_InPluginsLog;
begin
  with ListPLog do
    if CanFocus then
    begin
      SetFocus;
      ecFindInList.Execute;
    end;
end;

procedure TfmMain.TBXItemPLogClearClick(Sender: TObject);
begin
  ListPLog.Clear;
  FocusEditor;
end;

procedure TfmMain.TBXItemPLogDeleteClick(Sender: TObject);
begin
  ListPLog.DeleteSelected;
end;

procedure TfmMain.TBXItemPLogCopyAllClick(Sender: TObject);
begin
  DoListCopyAll(ListPLog);
end;

procedure TfmMain.TBXItemPLogCopySelClick(Sender: TObject);
begin
  DoListCopy(ListPLog);
end;

procedure TfmMain.ListPLogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=vk_delete) and (Shift=[]) then
  begin
    TbxItemPLogClearClick(Self);
    Key:= 0;
    Exit
  end;
  if (Key=Ord('C')) and (Shift=[ssCtrl]) then
  begin
    TbxItemPLogCopySelClick(Self);
    Key:= 0;
    Exit
  end;
  if IsShortcutOfCmd(Shortcut(Key, Shift), smFindDialog) then
  begin
    TBXItemPLogFindClick(Self);
    Key:= 0;
    Exit
  end;
  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.ListPLogDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var c: TColor;
begin
  with Control as TTntListbox do
  begin
    if odSelected in State then
      Canvas.Brush.Color:= opColorOutSelBk
    else
      Canvas.Brush.Color:= Color;
    Canvas.FillRect(Rect);

    if odSelected in State then
      c:= opColorOutSelText
    else
      c:= TColor(Items.Objects[Index]);

    Canvas.Font.Color:= c;
    ecTextOut(Canvas, Rect.Left+1, Rect.Top, Items[Index]);
  end;
end;

//--------------------
function TfmMain.SGetTabPrefix: Widestring;
begin
  Result:= DKLangConstW('Tab') + ':';
end;

procedure TfmMain.DoFindDialog_FindAllInAllTabs;
var
  NTotalSize, NDoneSize: Int64;
  ADir: Widestring;
  F: TEditorFrame;
  i: Integer;
begin
  FListFiles.Clear;
  for i:= 0 to FrameAllCount-1 do
    FListFiles.AddObject('',
      Pointer(FramesAll[i].EditorMaster.TextLength));

  FListResFN:= '';
  FListResFN_Prev:= '';
  ADir:= '';

  //init TreeRoot, show pane
  UpdateTreeFind_Initial(Finder.FindText, ADir, true);
  TabsOut.TabIndex:= Ord(tbFindRes);
  plOut.Show;

  DoProgressShow(proFindText);
  try
    NTotalSize:= 0;
    NDoneSize:= 0;
    for i:= 0 to FListFiles.Count-1 do
      Inc(NTotalSize, DWORD(FListFiles.Objects[i]));
    if NTotalSize = 0 then
      NTotalSize:= 1;

    FFinderTotalSize:= NTotalSize;
    FFinderDoneSize:= NDoneSize;

    for i:= 0 to FListFiles.Count-1 do
    begin
      F:= FramesAll[i];
      FListResFN:= SGetTabPrefix + IntToStr(i+1) + '[' + F.TabCaption + ']';

      DoFind_InFrame(F);

      Inc(NDoneSize, DWORD(FListFiles.Objects[i]));
      FFinderDoneSize:= NDoneSize;
      if IsProgressStopped(NDoneSize, NTotalSize) then
        Break;
    end;

    FFinderTotalSize:= 0;
    FFinderDoneSize:= 0;
  except
    on E: Exception do
    begin
      MsgExcept('Error on searching in tabs', E, Handle);
      DoProgressHide;
      Exit;
    end;
  end;

  if StopFind then
  begin
    StopFind:= false;
    UpdateTreeFind_Results(Finder.FindText, ADir, true, true);
    Exit
  end;

  if FTreeRoot=nil then
    raise Exception.Create('TreeRoot nil');
  if FTreeRoot.GetFirstChild=nil then
  begin
    UpdateTreeFind_Results(Finder.FindText, ADir, false, true);
  end
  else
  begin
    UpdateTreeFind_Results(Finder.FindText, ADir, false, true);
    TabsOut.TabIndex:= Ord(tbFindRes);
    plOut.Show;
  end;
end;

procedure TfmMain.DoFind_InFrame(F: TEditorFrame;
  AMarkAll: boolean = false;
  AWithBkmk: boolean = false);
begin
  FLastOnContinueCheck:= 0;
  Finder.OnBeforeExecute:= nil;
  Finder.OnNotFound:= nil;
  Finder.OnCanAccept:= Finder_OnCanAccept;
  Finder.OnContinue:= Finder_OnContinue;
  if AWithBkmk then
    Finder.OnFind:= Finder_OnFind_WithResultPaneAndBkmk
  else
    Finder.OnFind:= Finder_OnFind_WithResultPane;

  try
    Finder.Control:= F.EditorMaster;
    if AMarkAll then
      Finder.FindAll
    else
      Finder.CountAll;
  finally
    Finder.OnBeforeExecute:= Finder_OnBeforeExecute;
    Finder.OnNotFound:= Finder_OnNotFound;
    Finder.OnFind:= nil;
    Finder.OnCanAccept:= nil;
    Finder.OnContinue:= nil;
  end;
end;

function TfmMain.SGetFrameIndexFromPrefixedStr(const InfoFN: Widestring): Integer;
var
  s: Widestring;
begin
  Result:= -1;
  s:= SGetTabPrefix; //"Tab#"
  if SBegin(InfoFN, s) then
  begin
    s:= Copy(InfoFN, Length(s)+1, MaxInt);
    SDeleteFromW(s, '['); //delete trailing part with filename
    Result:= Pred(StrToIntDef(s, -1));
  end;
end;

function TfmMain.PluginAction_SuggestCompletion(
  const Str: PWideChar;
  NChars: Integer;
  ShowPopup: boolean): Integer;
var
  P: TPoint;
  L: ecUnicode.TWideStrings;
  i: Integer;
  S, S_id, S_type, S_param: Widestring;
begin
  Result:= cSynOK;

  PluginACP.Items.Clear;
  PluginACP.DisplayItems.Clear;

  L:= TWideStringList.Create;
  try
    L.SetText(Str);
    if L.Count=0 then
      begin Result:= cSynError; Exit; end;
    for i:= 0 to L.Count-1 do
    begin
      S:= L[i];
      if Pos('|', S)>0 then
      begin
        S_id:= SGetItem(S, '|');
        S_type:= SGetItem(S, '|');
        S_param:= SGetItem(S, '|');
      end
      else
      begin
        S_id:= S;
        S_type:= '';
        S_param:= '';
      end;
      if S_id='' then Continue;
      S:= IfThen(Pos('(', S_param)>0, '('); //insert text with "(" if params not empty
      PluginACP.Items.Add(S_id+S);
      PluginACP.DisplayItems.Add(WideFormat('\s1\%s\t\\s2\%s\t\\s0\%s', [S_type, S_id, S_param]));
    end;
  finally
    FreeAndNil(L);
  end;

  with CurrentEditor do
  begin
    P:= CaretPos;
    if (TextLength=0) or (P.X=0) then
      begin Result:= cSynError; Exit end;

    if NChars>P.X then
      NChars:= P.X;
    Dec(P.X, NChars);

    if not ShowPopup then
    begin
      ReplaceText(CaretPosToStrPos(P), NChars, L[0]);
    end
    else
    begin
      FCurrPluginAcpStartPos:= P;
      PluginACP.Execute;
    end;
  end;
end;

procedure TfmMain.PluginAcpDefineStartPos(Sender: TObject;
  var StartPos: TPoint);
begin
  StartPos:= FCurrPluginAcpStartPos;
end;

function TfmMain.PluginAction_GetCaretPos(PtrX, PtrY, PtrAbs: PInteger): Integer;
var
  Ed: TSyntaxMemo;
  P: TPoint;
begin
  Ed:= CurrentEditor;
  P:= Ed.CaretPos;
  if PtrX<>nil then PtrX^:= P.X;
  if PtrY<>nil then PtrY^:= P.Y;
  if PtrAbs<>nil then PtrAbs^:= Ed.CaretStrPos;
  Result:= cSynOK;
end;

function TfmMain.PluginAction_SetCaretPos(AX, AY: Integer): Integer;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if (AX>=0) and (AY>=0) and (AY<Ed.Lines.Count) then
  begin
    Ed.CaretPos:= Point(AX, AY);
    Result:= cSynOK;
  end
  else
  if (AX<0) and (AY>=0) then
  begin
    Ed.CaretStrPos:= AY;
    Result:= cSynOK;
  end
  else
    Result:= cSynError;
end;


function TfmMain.PluginAction_ParseRegex(const SRegex, SStr: Widestring; var Res: TSynRegexArray): Integer;
var
  ResL: TSynStrArray;
  i: Integer;
begin
  SParseRegexArray(SStr, SRegex, ResL);
  for i:= Low(Res) to High(Res) do
  begin
    FillChar(Res[i]^, cSynMaxPath, 0);
    lstrcpynw(PWideChar(Res[i]), PWideChar(ResL[i]), cSynMaxPath);
  end;
  Result:= cSynOK;
end;

procedure TfmMain.ecCenterLinesExecute(Sender: TObject);
begin
  EditorCenterSelectedLines(CurrentEditor);
end;

procedure TfmMain.TBXItemLeftTabsClick(Sender: TObject);
begin
  TabsLeft.TabIndex:= Ord(tbTabs);
end;

function TfmMain.ListTab_FrameIndex: integer;
begin
  Result:= -1;
  if ListTabs.Selected=nil then Exit;
  if ListTabs.Selected.SubItems.Count<2 then Exit;
  Result:= StrToIntDef(ListTabs.Selected.SubItems[2], -1);
end;

procedure TfmMain.ListTabsClick(Sender: TObject);
var
  N: Integer;
begin
  N:= ListTab_FrameIndex;
  if (N>=0) and (N<FrameAllCount) then
  begin
    CurrentFrame:= FramesAll[N];

    if ListTabs.CanFocus then
      ListTabs.SetFocus;
    FocusEditor;
  end;
end;

procedure TfmMain.ListTabsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.ListTabsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  opTabsSortMode:= Column.Index;
  UpdateListTabs;
end;

procedure TfmMain.ListTabsCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  fn1, fn2,
  cap1, cap2: Widestring;
begin
  cap1:= Item1.SubItems[0];
  cap2:= Item2.SubItems[0];
  
  fn1:= Item1.SubItems[1];
  fn2:= Item2.SubItems[1];

  case opTabsSortMode of
    0: Compare:= 0;
    1: Compare:= lstrcmpw(PWChar(cap1), PWChar(cap2));
    2: Compare:= lstrcmpw(PWChar(fn1), PWChar(fn2));
    else
      raise Exception.Create('Unknown ListTabs sort mode')
  end;
end;

procedure TfmMain.ecToggleFocusTabsExecute(Sender: TObject);
begin
  if not plTree.Visible then
  begin
    ecShowTree.Execute;
    TabsLeft.TabIndex:= Ord(tbTabs);
    if ListTabs.CanFocus then
      ListTabs.SetFocus
  end
  else
  if ListTabs.Focused then
    FocusEditor
  else
  begin
    TabsLeft.TabIndex:= Ord(tbTabs);
    if ListTabs.CanFocus then
      ListTabs.SetFocus
  end;
end;

procedure TfmMain.LoadHideIni;
var
  L: TStringList;
  fn: string;
  i: Integer;
begin
  fn:= SynHideIni;
  if not IsFileExist(fn) then Exit;

  L:= TStringList.Create;
  try
    L.LoadFromFile(fn);
    for i:= 0 to L.Count-1 do
      DoHideMenuItem(L[i]);
  finally
    FreeAndNil(L)
  end;
end;

procedure TfmMain.DoHideMenuItem(const Str: string);
var
  id, s1, sIndex: Widestring;
  NIndex, i: integer;
  Item: TComponent;
begin
  if Trim(Str)='' then Exit;
  s1:= Str;
  id:= SGetItem(s1, ' ');
  if id='' then Exit;
  SIndex:= SGetItem(s1, ' ');
  NIndex:= StrToIntDef(SIndex, -1);

  for i:= Low(FMenuItems) to High(FMenuItems) do
    if FMenuItems[i].Id = id then
    begin
      Item:= FMenuItems[i].Item;

      if Item is TSpTbxSubmenuItem then
      begin
        if NIndex<0 then
          (Item as TSpTbxSubmenuItem).Visible:= false
        else
        if NIndex<(Item as TSpTbxSubmenuItem).Count then
          (Item as TSpTbxSubmenuItem).Items[NIndex].Visible:= false
        else
          MsgError('[SynHide.ini] Bad index: '+Str, Handle);
      end
      else
      if Item is TSpTbxItem then
      begin
        (Item as TSpTbxItem).Visible:= false;
      end
      else
      if Item is TSpTbxPopupMenu then
      begin
        if (NIndex>=0) and (NIndex<(Item as TSpTbxPopupMenu).Items.Count) then
          (Item as TSpTbxPopupMenu).Items[NIndex].Visible:= false
        else
          MsgError('[SynHide.ini] Bad index: '+Str, Handle);
      end
      else
      if Item is TSpTbxToolbar then
      begin
        if (NIndex>=0) and (NIndex<(Item as TSpTbxToolbar).Items.Count) then
          (Item as TSpTbxToolbar).Items[NIndex].Visible:= false
        else
          MsgError('[SynHide.ini] Bad index: '+Str, Handle);
      end
      else
        MsgError('[SynHide.ini] Unknown item type: '+Str, Handle);
      Exit;
    end;

  MsgError('[SynHide.ini] Unknown item id: '+Str, Handle);
end;

function TfmMain.IsLexerFindID(const Lex: string): boolean;
var
  i: Integer;
begin
  Result:= false;
  for i:= Low(FPluginsFindid) to High(FPluginsFindid) do
    with FPluginsFindid[i] do
      if IsLexerListed(Lex, SLexers) then
      begin
        Result:= true;
        Exit
      end;
end;

procedure TfmMain.TBXItemCtxFindIDClick(Sender: TObject);
begin
  DoFindId;
end;

procedure TfmMain.DoFindId;
var
  i: Integer;
begin
  if DoPyEvent(CurrentEditor, cSynEventOnGotoDef, []) = cPyTrue then
  begin
    CurrentEditor.ResetSelection; //reset selection caused by Ctrl+Alt+click
    Exit;
  end;

  for i:= Low(FPluginsFindid) to High(FPluginsFindid) do
    with FPluginsFindid[i] do
      if IsLexerListed(CurrentLexer, SLexers) then
      begin
        DoPlugin_LoadGotoDef(i);
        CurrentEditor.ResetSelection; //reset selection caused by Ctrl+Alt+click
        Exit
      end;

  //no find-id plugins found
  DoHint(DKLangConstW('zMFindIdNone'));
end;

function TfmMain.DoAcpFromPlugins(const AAction: PWideChar): Widestring;
var
  i: Integer;
begin
  if AAction=cActionGetAutoComplete then
  begin
    Result:= DoPyEvent(CurrentEditor, cSynEventOnComplete, []);
    if Result=cPyTrue then Exit;
  end;

  if AAction=cActionGetFunctionHint then
  begin
    Result:= DoPyEvent(CurrentEditor, cSynEventOnFuncHint, []);
    if Result<>'' then Exit;
  end;

  Result:= '';
  for i:= Low(FPluginsAcp) to High(FPluginsAcp) do
    with FPluginsAcp[i] do
      if IsLexerListed(CurrentLexer, SLexers) then
      begin
        DoHint(DKLangConstW('zMTryAcp')+' '+ExtractFileName(SFileName));

        //auto-completion dll plugin?
        Result:= DoPlugin_LoadGetString(SFilename, AAction);
        DoHint('');
        if Result<>'' then Exit;
      end;
end;


procedure TfmMain.DoPlugin_LoadGotoDef(Index: Integer);
begin
  DoPlugin_LoadAction(
    FPluginsFindid[Index].SFilename,
    cActionFindID,
    nil, nil, nil, nil);
end;

procedure TfmMain.DoPlugin_LoadAction(
  const AFileName: string;
  const AActionName: Widestring;
  P1, P2, P3, P4: Pointer);
var
  FDll: THandle;
  FSynInit: TSynInit;
  FSynAction: TSynAction;
  AIni: Widestring;
begin
  if not IsFileExist(AFileName) then
  begin
    MsgNoFile(AFileName);
    Exit;
  end;

  FDll:= LoadLibrary(PChar(string(AFileName)));
  if FDll=0 then
  begin
    MsgError('Can''t load dll:'#13+AFileName, Handle);
    Exit
  end;

  FSynInit:= GetProcAddress(FDll, 'SynInit');
  if @FSynInit=nil then
  begin
    MsgError('Can''t find SynInit'#13+AFileName, Handle);
    Exit
  end;

  FSynAction:= GetProcAddress(FDll, 'SynAction');
  if @FSynAction=nil then
  begin
    MsgError('Can''t find SynAction'#13+AFileName, Handle);
    Exit
  end;

  AIni:= SynPluginIni(ChangeFileExt(ExtractFileName(AFileName), ''));
  FSynInit(PWChar(AIni), @_SynActionProc);
  FSynAction(nil, PWChar(AActionName), P1, P2, P3, P4);
  FreeLibrary(FDll);
end;

function TfmMain.DoPlugin_LoadGetString(
  const AFileName: string;
  const AActionName: Widestring): Widestring;
var
  FDll: THandle;
  FSynInit: TSynInit;
  FSynAction: TSynAction;
  AIni: Widestring;
  P1, P2, P3, P4: Pointer;
  AText: array[0..Pred(8*1024)] of WideChar;
begin
  Result:= '';

  if not IsFileExist(AFileName) then
  begin
    MsgNoFile(AFileName);
    Exit;
  end;

  FDll:= LoadLibrary(PChar(string(AFileName)));
  if FDll=0 then
  begin
    MsgError('Can''t load dll:'#13+AFileName, Handle);
    Exit
  end;

  FSynInit:= GetProcAddress(FDll, 'SynInit');
  if @FSynInit=nil then
  begin
    MsgError('Can''t find SynInit'#13+AFileName, Handle);
    Exit
  end;

  FSynAction:= GetProcAddress(FDll, 'SynAction');
  if @FSynAction=nil then
  begin
    MsgError('Can''t find SynAction'#13+AFileName, Handle);
    Exit
  end;

  FillChar(AText, SizeOf(AText), 0);
  P1:= @AText;
  P2:= Pointer(SizeOf(AText) div 2 - 1);
  P3:= nil;
  P4:= nil;

  try
    AIni:= SynPluginIni(ChangeFileExt(ExtractFileName(AFileName), ''));
    FSynInit(PWChar(AIni), @_SynActionProc);
    if FSynAction(nil, PWChar(AActionName), P1, P2, P3, P4) = cSynOK then
      Result:= Widestring(AText);
  finally
    FreeLibrary(FDll);
  end;
end;


procedure TfmMain.DoPlugin_CommandClick(Sender: TObject);
var
  N: Integer;
begin
  N:= (Sender as TComponent).Tag;
  DoPyCommandPlugin(N);
end;

procedure TfmMain.DoPyCommandPlugin(N: Integer);
begin
  if (N>=Low(FPluginsCommand)) and (N<=High(FPluginsCommand)) then
  with FPluginsCommand[N] do
  begin
    if (SFileName='') then
      begin MsgBeep; Exit; end;

    if (SLexers<>'') and not IsLexerListed(CurrentLexer, SLexers) then
    begin
      DoHint(WideFormat(DKLangConstW('zMNoneLexer'), [SLexers]));
      MsgBeep;
      Exit;
    end;

    //MsgInfo(SFileName+#13+SCmd+#13);
    if SBegin(SFilename, cPyPrefix) then
    begin
      //Python command plugin
      DoPyLoadPlugin(
        SFilename,
        SCmd);
    end
    else
    begin
      //DLL command plugin
      DoPlugin_LoadAction(
        SFilename,
        cActionMenuCommand,
        PWChar(WideString(SCmd)),
        nil,
        nil,
        nil);
    end;
  end;
end;

procedure TfmMain.DoPlugin_AddMenuItem(
  ASubmenu: TSpTbxSubmenuitem;
  const SKey: Widestring;
  NIndex, NCommandId: Integer);
var
  ItemSub: TSpTbxSubmenuItem;
  Item: TSpTbxItem;
  S, CapMenu, CapItem: Widestring;
  N, i: Integer;
begin
  with ASubmenu do
  begin
    Enabled:= true;

    S:= SKey;
    CapMenu:= SGetItem(S, '\');
    CapItem:= SGetItem(S, '\');

    N:= -1;
    for i:= 0 to Count-1 do
      if Items[i].Caption=CapMenu then
        begin N:= i; Break end;

    if N<0 then
    begin
      if CapItem='' then
        ItemSub:= ASubmenu
      else
      begin
        ItemSub:= TSpTbxSubmenuItem.Create(Self);
        ItemSub.Caption:= CapMenu;
        Add(ItemSub);
      end;
    end
    else
    begin
      if (Items[N] is TSpTbxSubmenuItem) then
        ItemSub:= (Items[N] as TSpTbxSubmenuItem)
      else
        begin MsgBeep; Exit end;
    end;

    //separater menu id begins with "-"
    if SBegin(CapItem, '-') then
    begin
      ItemSub.Add(TSpTbxSeparatorItem.Create(Self));
    end
    else
    begin
      Item:= TSpTbxItem.Create(Self);
      if CapItem='' then
        Item.Caption:= CapMenu
      else
        Item.Caption:= CapItem;
      Item.Tag:= NIndex;
      Item.OnClick:= DoPlugin_CommandClick;
      if NCommandId>0 then
        UpdKey(Item, NCommandId);
      ItemSub.Add(Item);
    end;
  end;
end;

procedure TfmMain.DoFindDialog_OnFocusEditor(Sender: TObject);
begin
  FocusEditor;
end;

procedure TfmMain.DoFindDialog_OnDockedChanged(Sender: TObject);
begin
  if fmSR.IsDocked then
  begin
    fmSR.BorderStyle:= bsNone;
    fmSR.Parent:= PanelBg;
    fmSR.Align:= alBottom;
  end
  else
  begin
    fmSR.Parent:= nil;
    fmSR.Align:= alNone;
    fmSR.BorderStyle:= bsDialog;
  end;
end;

procedure TfmMain.DoFindDialog_OnRepaintNeeded(Sender: TObject);
begin
  //DoRepaint doesn't help
end;

procedure TfmMain.DoFindDialog_OnShowStatus(const Msg: Widestring);
begin
  DoHint(Msg);
end;


procedure TfmMain.TBXItemOEditSynPluginsIniClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_EditSynPluginsIni);
end;

function TfmMain.PluginAction_GetText(const id: Integer; BufferPtr: Pointer; var BufferSize: Integer): Integer;
var
  s, sCR: Widestring;
  NeededSize: Integer;
  Ed: TSyntaxMemo;
  i, Ln1, Ln2: Integer;
begin
  Ed:= CurrentEditor;
  s:= '';
  sCR:= EditorEOL(Ed);

  if (BufferPtr=nil) or (BufferSize<=0) then
  begin
    Result:= cSynError;
    Exit
  end;

  case id of
    cSynIdAllText:
      s:= Ed.Lines.FText;
    cSynIdSelectedText:
      s:= Ed.SelText;
    cSynIdSelectedLines:
      begin
        if Ed.SelLength>0 then
        begin
          EditorGetSelLines(Ed, Ln1, Ln2);
          for i:= Ln1 to Ln2 do
            s:= s+Ed.Lines[i]+sCR;
        end;
      end;
    cSynIdCurrentLine:
      begin
        i:= Ed.CaretPos.Y;
        if (i>=0) and (i<Ed.Lines.Count) then
          s:= Ed.Lines[i]
        else
          begin Result:= cSynError; Exit end;
      end;
    cSynIdSearchPaths:
      begin
        s:= DoGetSearchPaths;
      end;
    cSynIdFavoritesText:
      begin
        s:= DoGetFavList;
      end;
    else
      begin
        if (id>=0) and (id<Ed.Lines.Count) then
          s:= Ed.Lines[id]
        else
          begin Result:= cSynError; Exit end;
      end;
  end;

  NeededSize:= Length(s)+1;
  if BufferSize<NeededSize then
  begin
    BufferSize:= NeededSize;
    Result:= cSynSmallBuffer;
    Exit
  end;

  FillChar(BufferPtr^, BufferSize, 0);
  lstrcpynW(BufferPtr, PWChar(s), NeededSize);
  BufferSize:= Length(s);
  Result:= cSynOK;
end;

function TfmMain.DoGetSearchPaths: Widestring;
begin
  Result:= opProjPaths;
  if Assigned(fmProj) then
    Result:= Result + fmProj.FOpts.SearchDirs;
end;

function TfmMain.PluginAction_SetText(const id: Integer;
  BufferPtr: Pointer; BufferSize: Integer): Integer;
var
  s: Widestring;
  Ed: TSyntaxMemo;
  i: Integer;
  //Ln1, Ln2: Integer;
begin
  Ed:= CurrentEditor;
  s:= '';

  if (BufferPtr=nil) or (BufferSize<=0) then
  begin
    Result:= cSynError;
    Exit
  end;

  s:= SBufferToString(BufferPtr, BufferSize);

  case id of
    cSynIdAllText:
      Ed.ReplaceText(0, Ed.TextLength, s);
    cSynIdSelectedText:
      Ed.ReplaceText(Ed.SelStart, Ed.SelLength, s);
    {
    //not supported for SetText
    cSynIdSelectedLines:
    begin
      if Ed.SelLength>0 then
      begin
        EditorGetSelLines(Ed, Ln1, Ln2);
        for i:= Ln2 downto Ln1 do
          DoDeleteLine(Ed, i, true);
        dd
      end;
    end;
    }
    cSynIdCurrentLine:
    begin
      i:= Ed.CaretPos.Y;
      if (i>=0) and (i<Ed.Lines.Count) then
        DoReplaceLine(Ed, i, s, true{ForceUndo})
      else
        begin Result:= cSynError; Exit end;
    end;
    else
    begin
      if (id>=0) and (id<Ed.Lines.Count) then
        DoReplaceLine(Ed, id, s, true{ForceUndo})
      else
        begin Result:= cSynError; Exit end;
    end;
  end;

  Result:= cSynOK;
end;

function TfmMain.PluginAction_GetSel(var Sel: TSynSelection): Integer;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  Sel.SelStart:= Ed.SelStart;
  Sel.SelLength:= Ed.SelLength;
  Sel.SelRect:= Ed.SelRect;
  Result:= cSynOK;
end;

function TfmMain.PluginAction_SetSel(const Sel: TSynSelection): Integer;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Sel.SelLength<0 then
    Ed.ResetSelection
  else
  if Sel.SelLength>0 then
    Ed.SetSelection(Sel.SelStart, Sel.SelLength)
  else
    Ed.SelRect:= Sel.SelRect;
  Result:= cSynOK;
end;

function TfmMain.PluginAction_SetTopLine(Num: Integer): Integer;
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if (Num>=0) and (Num<Ed.Lines.Count) then
  begin
    Ed.TopLine:= Num;
    Result:= cSynOK;
  end
  else
    Result:= cSynError;
end;

function TfmMain.PluginAction_GetProp(id: Integer; Buffer: Pointer; Param: Integer): Integer;
  //
  procedure SetNum(N: Integer);
  begin
    PInteger(Buffer)^:= N;
  end;
  procedure SetStr(const S: Widestring);
  begin
    lstrcpynW(Buffer, PWChar(S), cSynMaxPath);
  end;
var
  Ed: TSyntaxMemo;
begin
  Result:= cSynOK;
  Ed:= CurrentEditor;
  case id of
    cSynPropLinesCount: SetNum(Ed.Lines.Count);
    cSynPropTopLine: SetNum(Ed.TopLine);
    cSynPropReadOnly: SetNum(Ord(Ed.ReadOnly));
    cSynPropLexer: SetStr(CurrentLexer);
    cSynPropLexerForFile: SetStr(CurrentLexerForFile);
    cSynPropLineEnds: SetStr(EditorEOL(Ed));
    cSynPropRightMargin: SetNum(Ed.RightMargin);
    cSynPropTabSpaces: SetNum(Ord(Ed.TabMode=tmSpaces));
    cSynPropTabSize: SetNum(Ed.TabList[0]);
    cSynPropWordWrap: SetNum(Ord(Ed.WordWrap));
    cSynPropCodeFolding: SetNum(Ord(not Ed.DisableFolding));
    cSynPropNonPrintable: SetNum(Ord(Ed.NonPrinted.Visible));
    cSynPropLineNumbers: SetNum(Ord(Ed.LineNumbers.Visible));
    cSynPropTextLength: SetNum(Ed.TextLength);
    cSynPropLineLength:
      begin
        if (Param>=0) and (Param<Ed.Lines.Count) then
          SetNum(Ed.Lines.LineLength(Param))
        else
          Result:= cSynError;
      end;
    cSynPropLineSpace:
      begin
        if (Param>=0) and (Param<Ed.Lines.Count) then
          SetNum(Ed.Lines.LineSpace(Param))
        else
          Result:= cSynError;
      end;
    else
      Result:= cSynError;
  end;
end;

function TfmMain.PluginAction_ReplaceText(DelLen: Integer; BufPtr: Pointer;
  BufSize: Integer): Integer;
var
  Ed: TSyntaxMemo;
  s: Widestring;
begin
  Ed:= CurrentEditor;
  if Ed.ReadOnly or (DelLen<0) or (BufSize<0) then
  begin
    Result:= cSynError;
    Exit
  end;

  s:= SBufferToString(BufPtr, BufSize);
  Ed.ReplaceText(Ed.CaretStrPos, DelLen, s);
  Result:= cSynOK;
end;

function TfmMain.PluginAction_TranslatePos(var PosX, PosY, PosAbs: Integer;
  Direction: Boolean): Integer;
var
  Ed: TSyntaxMemo;
  P: TPoint;
begin
  Ed:= CurrentEditor;
  if Direction then
  begin
    //offset -> col/line
    P:= Ed.StrPosToCaretPos(PosAbs);
    PosX:= P.X;
    PosY:= P.Y;
  end
  else
  begin
    PosAbs:= Ed.CaretPosToStrPos(Point(PosX, PosY));
  end;
  Result:= cSynOK;
end;

procedure TfmMain.TBXItemPLogSaveAsClick(Sender: TObject);
var
  fn: string;
begin
  with TSaveDialog.Create(Self) do
  try
    Options:= Options+[ofOverwritePrompt];
    Filter:= '*.log;*.txt|*.log;*.txt';
    InitialDir:= '';
    FileName:= 'Log';
    if Execute then
    begin
      if ExtractFileExt(FileName)='' then
        fn:= FFreeFN(FileName, 'log', InitialDir)
      else
        fn:= FileName;
      ListPLog.Items.SaveToFile(fn);
    end;
  finally
    Free
  end;
end;

procedure TfmMain.TBXItemTabMoveToWindowClick(Sender: TObject);
begin
  DoMoveTabToWindow(FClickedFrame, true);
end;

procedure TfmMain.TBXItemTabOpenInWindowClick(Sender: TObject);
begin
  DoMoveTabToWindow(FClickedFrame, false);
end;

procedure TfmMain.DoMoveTabToWindow(Frame: TEditorFrame; AndClose: boolean);
begin
  if not SynExe then
    begin MsgBeep; Exit; end;

  if Frame.FileName='' then
    begin MsgBeep; Exit; end;
  if opSingleInstance then
    begin MsgBeep; Exit end;

  if not FExecute(Application.ExeName, '"'+Frame.FileName+'"', '', Handle) then
    begin MsgBeep; Exit end;

  if AndClose then
    Groups.CloseTabs(tabCloseCurrent, true);
end;


function TfmMain.BrotherEditor(Ed: TSyntaxMemo): TSyntaxMemo;
var
  F: TEditorFrame;
begin
  F:= FrameOfEditor(Ed);
  if Ed=F.EditorMaster then
    Result:= F.EditorSlave
  else
    Result:= F.EditorMaster;
end;

procedure TfmMain.DoCheckIfBookmarkSetHere(Ed: TSyntaxMemo; NPos: Integer);
var
  NNew, i: Integer;
begin
  if not ((NPos>=0) and (NPos<Ed.TextLength)) then Exit;
  with Ed.BookmarkObj do
    for i:= 0 to Count-1 do
      if Items[i].Position = NPos then
      begin
        NNew:= NPos+1;
        Items[i].Position:= NNew;
        //need to change Slave's bookmarks too
        with BrotherEditor(Ed).BookmarkObj do
          Items[i].Position:= NNew;
        Exit
      end;
end;

procedure TfmMain.UpdateFrameMicroMap(F: TEditorFrame);
begin
  F.DoSyncMicromap;
end;

procedure TfmMain.ProjLoadMRU(List: TSynMruList);
var
  Ini: TIniFile;
begin
  Ini:= TIniFile.Create(SynHistoryIni);
  try
    LoadMruList(List, Ini, 'MRU_Proj', opSaveFileCount, opMruCheck);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfmMain.ProjUpdateMRU(List: TSynMruList);
var
  Ini: TIniFile;
begin
  Ini:= TIniFile.Create(SynHistoryIni);
  try
    SaveMruList(List, Ini, 'MRU_Proj');
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfmMain.ecSortDialogExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdSortDialog);
end;

procedure TfmMain.DoLinesCommand(Cmd: TSynLineCmd);
var
  Ed: TSyntaxMemo;
  Ln1, Ln2: Integer;
  Pos1, Pos2, i: Integer;
  Col1, Col2: Integer;
  L: TTntStringList;
  S, Sep: Widestring;
  ok: boolean;
begin
  Ed:= CurrentEditor;
  if not Ed.HaveSelection then
  begin
    Ln1:= 0;
    Ln2:= Ed.Lines.Count-1;
  end
  else
    EditorGetSelLines(Ed, Ln1, Ln2);

  //if (Ln2-Ln1)<1 then
  //  begin MsgNoSelection; Exit end;

  Pos1:= Ed.CaretPosToStrPos(Point(0, Ln1));
  if Ln2>=Ed.Lines.Count-1 then
    Pos2:= Ed.TextLength
  else
    Pos2:= Ed.CaretPosToStrPos(Point(0, Ln2+1));

  Col1:= -1;
  Col2:= -1;
  if Ed.HaveSelection and (Ed.SelectMode=msColumn) then
  begin
    Col1:= Ed.SelRect.Left+1;
    Col2:= Ed.SelRect.Right;
  end;

  L:= TTntStringList.Create;
  try
    for i:= Ln1 to Ln2 do
      L.Add(Ed.Lines[i]);

    case Cmd of
      cLineCmdSortAsc,
      cLineCmdSortDesc:
        ok:= DoListCommand_Sort(L, opSortMode, Cmd=cLineCmdSortAsc, false{AShowDlg}, Col1, Col2);
      cLineCmdSortDialog:
        ok:= DoListCommand_Sort(L, opSortMode, true, true{AShowDlg}, Col1, Col2);

      cLineCmdDedupAll:
        begin
          i:= DoListCommand_Deduplicate(L, cLineDedupAll);
          ok:= i>0;
          MsgDelLines(i);
        end;
      cLineCmdDedupAllAndOrig:
        begin
          i:= DoListCommand_Deduplicate(L, cLineDedupAllAndOrig);
          ok:= i>0;
          MsgDelLines(i);
        end;
      cLineCmdDedupAdjacent:
        begin
          i:= DoListCommand_Deduplicate(L, cLineDedupAdjacent);
          ok:= i>0;
          MsgDelLines(i);
        end;

      cLineCmdTrimLead:
        begin
          i:= DoListCommand_Trim(L, cTrimLead);
          ok:= i>0;
          MsgDoneLines(i);
        end;
      cLineCmdTrimTrail:
        begin
          i:= DoListCommand_Trim(L, cTrimTrail);
          ok:= i>0;
          MsgDoneLines(i);
        end;
      cLineCmdTrimAll:
        begin
          i:= DoListCommand_Trim(L, cTrimAll);
          ok:= i>0;
          MsgDoneLines(i);
        end;
      cLineCmdRemoveDupSpaces:
        begin
          i:= DoListCommand_Trim(L, cTrimDups);
          ok:= i>0;
          MsgDoneLines(i);
        end;

      cLineCmdReverse:
        begin
          ok:= DoListCommand_Reverse(L);
        end;
      cLineCmdShuffle:
        begin
          ok:= DoListCommand_Shuffle(L);
        end;

      cLineCmdUntab:
        begin
          i:= DoListCommand_Untab(L, EditorTabSize(Ed));
          ok:= i>0;
          MsgDoneLines(i);
        end;

      cLineCmdIndent:
        ok:= DoListCommand_Indent(L,
               EditorTabSize(Ed),
               Ed.BlockIndent,
               soOptimalFill in Ed.Options
               );
      cLineCmdUnIndent:
        ok:= DoListCommand_UnIndent(L,
               EditorTabSize(Ed),
               Ed.BlockIndent,
               soOptimalFill in Ed.Options,
               soUnindentKeepAlign in Ed.Options
               );

      cLineCmdSpacesToTabs:
        begin
          ok:= DoListCommand_Unspace(L, EditorTabSize(Ed), false);
        end;
      cLineCmdSpacesToTabsLead:
        begin
          ok:= DoListCommand_Unspace(L, EditorTabSize(Ed), true);
        end;

      cLineCmdRemoveBlanks:
        begin
          i:= DoListCommand_RemoveBlanks(L);
          ok:= i>0;
          MsgDelLines(i);
        end;
      cLineCmdRemoveDupBlanks:
        begin
          i:= DoListCommand_RemoveDupBlanks(L);
          ok:= i>0;
          MsgDelLines(i);
        end;

      cLineCmdExtractDupsCase,
      cLineCmdExtractDupsNoCase,
      cLineCmdExtractUniq:
        begin
          case Cmd of
            cLineCmdExtractDupsCase:
              i:= DoListCommand_ExtractDups(L, true);
            cLineCmdExtractDupsNoCase:
              i:= DoListCommand_ExtractDups(L, false);
            cLineCmdExtractUniq:
              i:= DoListCommand_ExtractUniq(L);
            else
              i:= 0;
          end;
          ok:= false;
          if i>0 then
          begin
            acNewTab.Execute;
            CurrentEditor.InsertText(L.Text);
          end;
          MsgDoneLines(i);
        end;

      cLineCmdAlignWithSep:
        begin
          //read separator from ini
          with TIniFile.Create(SynHistoryIni) do
          try
            Sep:= UTF8Decode(ReadString('Win', 'AlignSep', '='));
            ok:= DoInputString(DKLangConstW('zMEnterSep'), Sep) and (Sep<>'');
            if ok then
              WriteString('Win', 'AlignSep', UTF8Encode(Sep));
          finally
            Free
          end;
          //do alignment
          if ok then
          begin
            i:= DoListCommand_AlignWithSep(L, Sep, EditorTabSize(Ed){, soOptimalFill in Ed.Options});
            ok:= i>0;
            MsgDoneLines(i);
          end;
        end;
      else
        ok:= false;
    end;

    //are lines processed
    if not ok then
      begin MsgDoneLines(0); MsgBeep; Exit end;

    //get resulting string
    S:= L.Text;
    FixLineEnds(S, Ed.Lines.TextFormat);
    FixLineEnds_AtEnd(S, Ed);
  finally
    FreeAndNil(L);
  end;

  //insert string into editor
  Ed.BeginUpdate;
  try
    Ed.CaretStrPos:= Pos1; //needed! otherwise ReplaceText will leave trailing blanks after small block.
    Ed.ReplaceText(Pos1, Pos2-Pos1, S);

    //restore selection Ln1...Ln2
    Ed.SetSelection(Pos1, Length(S));
  finally
    Ed.EndUpdate;
  end;
end;

procedure TfmMain.TBXItemSSelBracketsClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SelectBrackets);
end;

function TfmMain.FrameOfEditor(Ed: TSyntaxMemo): TEditorFrame;
begin
  if Ed.Owner is TEditorFrame then
    Result:= Ed.Owner as TEditorFrame
  else
    raise Exception.Create('Unknown owner of editor');
end;

procedure TfmMain.LoadPrintOptions;
begin
  PropsManagerPrint.IniFileName:= SynIni;
  PropsManagerPrint.LoadProps;

  with TIniFile.Create(SynIni) do
  try
    StringToFont(ecSyntPrinter.FontText, ReadString('Fonts', 'P_Text', ''));
    StringToFont(ecSyntPrinter.FontFooter, ReadString('Fonts', 'P_Footer', ''));
    StringToFont(ecSyntPrinter.FontHeader, ReadString('Fonts', 'P_Header', ''));
    StringToFont(ecSyntPrinter.FontLineNumders, ReadString('Fonts', 'P_Nums', ''));
  finally
    Free;
  end;
end;

procedure TfmMain.SavePrintOptions;
begin
  PropsManagerPrint.IniFileName:= SynIni;
  PropsManagerPrint.SaveProps;

  with TIniFile.Create(SynIni) do
  try
    WriteString('Fonts', 'P_Text', FontToString(ecSyntPrinter.FontText));
    WriteString('Fonts', 'P_Footer', FontToString(ecSyntPrinter.FontFooter));
    WriteString('Fonts', 'P_Header', FontToString(ecSyntPrinter.FontHeader));
    WriteString('Fonts', 'P_Nums', FontToString(ecSyntPrinter.FontLineNumders));
  finally
    Free;
  end;
end;


{
procedure TfmMain.TestApi;
var
  Ed: TSyntaxMemo;
  i: Integer;
  S: string;
begin
  Ed:= CurrentEditor;
  for i:= 0 to Ed.Lines.Count-1 do
    if IsEditorLineCollapsed(Ed, i) then
      S:= S+inttostr(i)+' ';
  Showmessage(s);
end;
}

procedure TfmMain.Finder_OnCanAccept(Sender: TObject;
  StartPos, EndPos: integer; var Accept: Boolean);
var
  Ed: TSyntaxMemo;
  Flags: TSearchOptions;
  Tokens: TSearchTokens;
  p: TPoint;
begin
  Ed:= (Sender as TSynFinder).Control as TSyntaxMemo;
  Flags:= (Sender as TSynFinder).Flags;
  Tokens:= (Sender as TSynFinder).Tokens;

  if ftSkipCollapsed in Flags then
  begin
    p:= Ed.StrPosToCaretPos(StartPos);
    if IsEditorLineCollapsed(Ed, p.y) then
    begin
      Accept:= false;
      Exit
    end;
  end;

  if Tokens<>tokensAll then
  begin
    Accept:= IsPositionMatchesTokens(Ed, StartPos, EndPos, Tokens);
  end;
end;

function TfmMain.IsPositionMatchesTokens(Ed: TSyntaxMemo;
  StartPos, EndPos: Integer; OptTokens: TSearchTokens): boolean;
var
  IsCmt, IsStr: boolean;
begin
  Result:= true;
  if OptTokens=tokensAll then Exit;

  EditorGetTokenType(Ed, StartPos, EndPos, IsCmt, IsStr);
  case OptTokens of
    tokensCmt:
      Result:= IsCmt;
    tokensStr:
      Result:= IsStr;
    tokensCmtStr:
      Result:= IsCmt or IsStr;
    tokensExceptCmtStr:
      Result:= not (IsCmt or IsStr);
  end;
end;

procedure TfmMain.SyncTree;
var
  Ed: TSyntaxMemo;
begin
  //is master<->slave editor switched?
  //then do fast switch.
  Ed:= CurrentEditor;
  if (Ed<>nil) and (Tree.SyntaxMemo<>nil) and (Tree.SyntaxMemo.Parent = Ed.Parent) then
  begin
    Tree.SyntaxMemo:= Ed;
    Exit;
  end;

  //clear tree and sync it after 1s delay
  Tree.SyntaxMemo:= nil;
  if Tree.Visible then
  begin
    TimerTree.Enabled:= false;
    TimerTree.Enabled:= true;
  end;
end;

procedure TfmMain.TimerTreeTimer(Sender: TObject);
begin
  TimerTree.Enabled:= false;
  if Tree.Visible then
    Tree.SyntaxMemo:= CurrentEditor
  else
    Tree.SyntaxMemo:= nil;
end;

function TfmMain.DoCheckUnicodeNeeded(Frame: TEditorFrame): boolean;
  //
  function Cfm(const SEnc: Widestring): integer;
  begin
    Result:= MsgConfirmYesNoCancel(
      WideFormat(DKLangConstW('zMUniNeed'), [SEnc]),
      Handle, true);
  end;
  //
begin
  Result:= true;
  if opUnicodeNeeded=0 then Exit;
  if Frame.EditorMaster.TextSource.Lines.TextCoding<>tcAnsi then Exit;
  if not IsTextUnicode(Frame.EditorMaster.Lines.FText) then Exit;

  case opUnicodeNeeded of
    1:
        case Cfm('UTF-8') of
          id_yes: ApplyFrameEncodingAndReload(Frame, cp__UTF8, False{ACanReload});
          id_cancel: Result:= false;
        end;
    2:
        case Cfm(DKLangConstW('cpUTF8no')) of
          id_yes: ApplyFrameEncodingAndReload(Frame, cp__UTF8_noBOM, False{ACanReload});
          id_cancel: Result:= false;
        end;
    3:
        case Cfm('UTF-16') of
          id_yes: ApplyFrameEncodingAndReload(Frame, cp__Unicode, False{ACanReload});
          id_cancel: Result:= false;
        end;
    4:
        case Cfm('UTF-16 BE') of
          id_yes: ApplyFrameEncodingAndReload(Frame, cp__UnicodeBE, False{ACanReload});
          id_cancel: Result:= false;
        end;
    5:
      ApplyFrameEncodingAndReload(Frame, cp__UTF8, False{ACanReload});
    6:
      ApplyFrameEncodingAndReload(Frame, cp__UTF8_noBOM, False{ACanReload});
    7:
      ApplyFrameEncodingAndReload(Frame, cp__Unicode, False{ACanReload});
    8:
      ApplyFrameEncodingAndReload(Frame, cp__UnicodeBE, False{ACanReload});
  end;
end;


procedure TfmMain.PopupStatusLineEndsPopup(Sender: TObject);
begin
  case CurrentFrame.EditorMaster.TextSource.Lines.TextFormat of
    tfCR: TbxItemEndMac.Checked:= true;
    tfNL: TbxItemEndUn.Checked:= true;
    else TbxItemEndWin.Checked:= true;
  end;
end;

procedure TfmMain.TBXSubmenuLineEndsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  case CurrentFrame.EditorMaster.TextSource.Lines.TextFormat of
    tfCR: TbxItemEndMMac.Checked:= true;
    tfNL: TbxItemEndMUn.Checked:= true;
    else TbxItemEndMWin.Checked:= true;
  end;
end;

procedure TfmMain.SynSpellCheckerCheckWord(Sender: TObject;
  const AWord: WideString; APos: Integer; var Valid: Boolean);
  //
  //
var
  F: TEditorFrame;
  Ed: TSyntaxMemo;
  En: boolean;
begin
  {$ifdef SPELL}
  F:= Sender as TEditorFrame;
  Ed:= F.EditorMaster;
  En:= F.SpellLive;
  Inc(APos);

  if En and IsPositionMatchesTokens(Ed, APos, APos+1, tokensCmtStr) then
    Valid:= FSpell.CheckWord(AWord) or F.IsUrlAtPosition(APos)
  else
    Valid:= true;
  {$else}
  Valid:= true;
  {$endif}
end;

procedure TfmMain.SynContextGutterPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  Ed: TSyntaxMemo;
begin
  Handled:= false;
  Ed:= Sender as TSyntaxMemo;
  if (not Ed.Gutter.Visible) or (Ed.Gutter.Bands[cBandFolding].Width=0) then Exit;

  Handled:= (MousePos.X >= 0) and (MousePos.X < Ed.Gutter.Width);

  if Handled then
    with Mouse.CursorPos do
      PopupFoldLevel.Popup(X, Y);
end;

procedure TfmMain.SynContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  F: TEditorFrame;
  Ed: TSyntaxMemo;
  AWord: Widestring;
  S: Ansistring;
  NStart, NEnd: Integer;
begin
  Handled:= false;
  F:= Sender as TEditorFrame;
  Ed:= F.EditorMaster;

  if not F.IsEditorPosMisspelled(Ed.CaretStrPos) then Exit;
  Handled:= true;

  {$ifdef SPELL}
  AWord:= Ed.WordAtPos(Ed.CaretPos);
  MousePos:= Ed.ClientToScreen(MousePos);
  S:= AWord;
  if FSpell.ShowPopupMenu(Sender,
    [spAdd, spIgnoreAll, spReplace], MousePos.X, MousePos.Y, S) = spReplace then
  begin
    Ed.WordRangeAtPos(Ed.CaretPos, NStart, NEnd);
    Ed.ReplaceText(NStart, NEnd-NStart, S);
  end;
  {$endif}
end;

procedure TfmMain.DoCheckAutoShowACP(Ed: TSyntaxMemo);
var
  N, NLen, i: Integer;
begin
  if (opAcpNum=0) or (ecACP.StartExpr<>'') then Exit;
  if ecACP.Visible then Exit;

  N:= Ed.CaretStrPos;
  if N>Length(Ed.Lines.FText) then Exit;
  //don't do Autocomplete if next char is wordchar
  if IsWordChar(Ed.Lines.Chars[N+1]) then Exit;

  //NLen is length of word before caret
  NLen:= 1;
  i:= N+1;
  repeat
    if (i<=1) or not IsWordChar(Ed.Lines.Chars[i-1]) then Break;
    Inc(NLen);
    Dec(i);
    if NLen>=opAcpNum then
    begin
      DoAcpPopup;
      Break
    end;
  until false;
end;

procedure TfmMain.SpellDialogShow(Sender: TObject);
begin
  {$ifdef SPELL}
  //not needed
  {$endif}
end;

procedure TfmMain.SpellPositionDialog(Sender: TObject);
var
  F: TForm;
begin
  {$ifdef SPELL}
  if Assigned(FSpell) and Assigned(FSpell.DialogForm) then
  begin
    F:= FSpell.DialogForm;
    F.Left:= (Screen.Width - F.Width) div 2;
    F.Top:= (Screen.Height - F.Height) div 2;
    EditorCheckCaretOverlappedByForm(CurrentFrame.EditorMaster, F);
  end;
  {$endif}
end;

procedure TfmMain.TBXItemFoldAllClick(Sender: TObject);
begin
  CurrentEditor.FullCollapse();
end;

procedure TfmMain.TBXItemUnfoldAllClick(Sender: TObject);
begin
  CurrentEditor.FullExpand();
end;

procedure TfmMain.TBXItemUnfoldLineClick(Sender: TObject);
begin
  with CurrentEditor do
    ToggleCollapseChildren(CaretPos.Y);
end;

function TfmMain.IsPluginWindowActive(var HWnd: THandle): boolean;
var
  i: Integer;
begin
  Result:= false;
  HWnd:= 0;
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
      if (FDll<>0) and (FWindow<>0) and IsChild(FWindow, Windows.GetFocus) then
      begin
        HWnd:= FWindow;
        Result:= true;
        Exit
      end;
end;

procedure TfmMain.ecCollapseParentExecute(Sender: TObject);
begin
  MsgBeep;
  //EditorCollapseParentRange(CurrentEditor, CurrentEditor.CaretStrPos);
end;

procedure TfmMain.ecCollapseWithNestedExecute(Sender: TObject);
begin
  MsgBeep;
  //EditorCollapseWithNested(CurrentEditor, CurrentEditor.CaretPos.Y);
end;

procedure TfmMain.TBXItemFoldParentClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CollapseParent);
end;

procedure TfmMain.TBXItemFoldWithNestedClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CollapseWithNested);
end;

procedure TfmMain.TBXItemFoldSelBlockClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smCollapseSelection);
end;

procedure TfmMain.TBXItemFoldNearestBlockClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smToggleCollapseNearest);
end;

procedure TfmMain.DoFind_CommandFromString(const S: Widestring);
var
  Act: TSynSearchAction;
  SText1, SText2: Widestring;
  Opt: TSearchOptions;
  Tok: TSearchTokens;
  OptBkmk, OptExtSel: boolean;
begin
  ReadFindOptions(S, Act, SText1, SText2, Opt, Tok, OptBkmk, OptExtSel);
  DoFindCommand(CurrentEditor, Act, SText1, SText2, Opt, Tok, OptBkmk, OptExtSel);
end;

function TfmMain.DoFindCommand(
  Ed: TSyntaxMemo;
  Act: TSynSearchAction;
  const SText1, SText2: Widestring;
  const Opt: TSearchOptions;
  const Tok: TSearchTokens;
  OptBkmk, OptExtSel: boolean): Integer;
var
  _OptPrev: TSearchOptions;
  _TokPrev: TSearchTokens;
  _PrevSelStart, _PrevSelLength: Integer;
  _BeforePrev: TNotifyEvent;
  Ok: boolean;
begin
  _OptPrev:= Finder.Flags;
  _TokPrev:= Finder.Tokens;
  _BeforePrev:= Finder.OnBeforeExecute;

  Finder.FindText:= SText1;
  Finder.ReplaceText:= SText2;
  Finder.Flags:= Opt;
  Finder.Tokens:= Tok;
  Finder.OnBeforeExecute:= nil;
  Finder.OnCanAccept:= Finder_OnCanAccept;
  Finder.Control:= Ed;

  if OptBkmk then
    Finder.OnFind:= Finder_OnFind_WithBkmk
  else
    Finder.OnFind:= nil;

  _PrevSelStart:= Finder.Control.SelStart;
  _PrevSelLength:= Finder.Control.SelLength;

  case Act of
    cfActionFindNext: Finder.FindAgain;
    cfActionFindAll: Finder.FindAll;
    cfActionFindInTabs: MsgError('Command "Find in all tabs" not supported in macros yet', Handle);
    cfActionCount: Finder.CountAll;
    cfActionSkip: Finder.FindAgain;
    cfActionReplaceNext: Finder.ReplaceAgain;
    cfActionReplaceAll: Finder.ReplaceAll;
    cfActionReplaceAllInAll: MsgError('Command "Replace in all tabs" not supported in macros yet', Handle);
  end;
  Result:= Finder.Matches;

  //restote Finder
  Finder.OnBeforeExecute:= _BeforePrev;
  Finder.OnCanAccept:= nil;
  Finder.OnFind:= nil;
  Finder.Flags:= _OptPrev;
  Finder.Tokens:= _TokPrev;

  //post-find actions
  Ok:= Finder.Matches>0;
  if Ok then
  begin
    if OptExtSel and (Act in [cfActionFindNext]) then
      EditorExtendSelectionByPosition(CurrentEditor,
        _PrevSelStart, _PrevSelLength,
        CurrentEditor.CaretStrPos, 0);

    if Assigned(fmSR) then
      EditorCheckCaretOverlappedByForm(Finder.Control, fmSR);
  end
  else
  begin
    //stop macro if not found
    ecMacroRec.AtFileEnd:= true;
    ecMacroRec.StopPlayback:= true;
  end;
end;

procedure TfmMain.ecSortAscendingExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdSortAsc);
end;

procedure TfmMain.ecSortDescendingExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdSortDesc);
end;

procedure TfmMain.ecSpToTabLeadingExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdSpacesToTabsLead);
end;

procedure TfmMain.TBXItemEToggleLineCommentAltClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ToggleLineCommentAlt);
end;

procedure TfmMain.ecToggleLineCommentAltExecute(Sender: TObject);
begin
  DoToggleLineComment(true);
end;

procedure TfmMain.TBXSubmenuCtxMorePopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  clip: boolean;
begin
  clip:= Clipboard.HasFormat(cf_text);
  TBXItemCtxPasteNoCurChange.Enabled:= clip;
  TBXItemCtxPasteToColumn1.Enabled:= clip;
end;

procedure TfmMain.TBXItemFoldRangesInSelClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smInSelCollapse);
end;

procedure TfmMain.TBXItemUnfoldRangesInSelClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smInSelExpand);
end;

procedure TfmMain.ecCommandsListExecute(Sender: TObject);
var
  Cmd: Integer;
begin
  Cmd:= DoShowCmdList;
  if Cmd<=0 then Exit;

  //lexer selected?
  if Cmd>=cLexListBase then
  begin
    Dec(Cmd, cLexListBase);
    Cmd:= Integer(FListLexersSorted.Objects[Cmd]);
    if (Cmd>=0) and (Cmd<SyntaxManager.AnalyzerCount) then
    begin
      CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer:= SyntaxManager.Analyzers[Cmd];
      UpdateLexerTo(CurrentFrame.EditorMaster.TextSource.SyntaxAnalyzer);
    end;
  end
  else
  //usual command selected
    CurrentEditor.ExecCommand(Cmd);
end;


function TfmMain.DoShowCmdList_ForTools: string;
var
  Cmd: Integer;
begin
  Result:= '';
  Cmd:= DoShowCmdList(true{OnlyStdCommands});
  if Cmd>0 then
    Result:= 'cm:'+IntToStr(Cmd);
end;

function TfmMain.DoShowCmdList(AOnlyStdCommands: boolean = false): Integer;
var
  Form: TfmMenuCmds;
  i: Integer;
begin
  Result:= 0;

  Form:= TfmMenuCmds.Create(Self);
  with Form do
  try
    UpdateMenuDialogBorder(Form);
    UpdateMacroKeynames;

    Caption:= DKLangConstW('zMCmdList');

    //1) add commands
    KeysList.Assign(SyntKeyMapping);

    if AOnlyStdCommands then
      DoKeymappingTruncate(KeysList, FInitialKeyCount)
    else
    begin
      //2) add lexers
      FListLexersSorted.Clear;
      FListLexersSorted.Sorted:= true;
      for i:= 0 to SyntaxManager.AnalyzerCount-1 do
        if not SyntaxManager.Analyzers[i].Internal then
          FListLexersSorted.AddObject(SyntaxManager.Analyzers[i].LexerName, Pointer(i));
      for i:= 0 to FListLexersSorted.Count-1 do
        LexList.Add('Lexer: ' + FListLexersSorted[i]);
    end;    

    FIniFN:= Self.SynHistoryIni;
    FColorSel:= opColorOutSelText;
    FColorSelBk:= opColorOutSelBk;

    if ShowModal=mrOk then
    begin
      if List.ItemIndex>=0 then
        Result:= Integer(List.Items.Objects[List.ItemIndex]);
    end;
  finally
    Free;
  end;
end;

procedure TfmMain.ecProjectListExecute(Sender: TObject);
var
  fn: Widestring;
  Files: TTntStringList;
  Form: TfmMenuProj;
  i: Integer;
begin
  if IsProjectEmpty then
  begin
    MsgNeedProject;
    Exit
  end;

  Files:= TTntStringList.Create;

  try
    for i:= 0 to fmProj.TreeProj.Items.Count-1 do
    begin
      fn:= fmProj.GetFN(fmProj.TreeProj.Items[i]);
      if fn<>'' then
        Files.Add(fn);
    end;
    if Files.Count=0 then
    begin
      MsgNeedProject;
      Exit
    end;

    fn:= '';
    Form:= TfmMenuProj.Create(Self);
    with Form do
    try
      UpdateMenuDialogBorder(Form);
      Caption:= DKLangConstW('zMProjList');

      FIniFN:= Self.SynHistoryIni;
      fmProj:= Self.fmProj;
      FListFiles:= Files;
      FColorSel:= opColorOutSelText;
      FColorSelBk:= opColorOutSelBk;

      if ShowModal=mrOk then
      begin
        if List.ItemIndex>=0 then
          fn:= Files[Integer(List.Items.Objects[List.ItemIndex])];
      end;
    finally
      Free;
    end;
  finally
    FreeAndNil(Files);
  end;

  if fn<>'' then
    if IsFileExist(fn) then
    begin
      if IsFileText(fn) or MsgConfirmBinary(fn, Handle) then
        DoOpenFile(fn);
    end;
end;

procedure TfmMain.ApplyUrlClick;
var
  i: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
  begin
    FramesAll[i].HyperlinkHighlighter.Active:= opHiliteUrls;
    FramesAll[i].HyperlinkHighlighter.SingleClick:= opSingleClickURL;
  end;
end;

procedure TfmMain.ApplyCarets;
var
  i: Integer;
begin
  for i:= 0 to FrameAllCount-1 do
    with FramesAll[i] do
    begin
      CaretsEnabled:= opCaretsEnabled;
      CaretsGutterBand:= opCaretsGutterBand;
      CaretsGutterColor:= opColorCaretsGutter;
      CaretsIndicator:= opCaretsIndicator;

      EditorSetCaretShape(EditorMaster, opCaretShapeIns, true);
      EditorSetCaretShape(EditorSlave, opCaretShapeIns, true);
      EditorSetCaretShape(EditorMaster, opCaretShapeOvr, false);
      EditorSetCaretShape(EditorSlave, opCaretShapeOvr, false);
    end;
end;

procedure TfmMain.TBXItemCaretsRemove1Click(Sender: TObject);
begin
  if not DoHandleEscapeActions then
    CurrentEditor.ExecCommand(sm_CaretsRemoveLeaveFirst);
end;

procedure TfmMain.TBXItemCaretsRemove2Click(Sender: TObject);
begin
  if not DoHandleEscapeActions then
    CurrentEditor.ExecCommand(sm_CaretsRemoveLeaveLast);
end;

procedure TfmMain.TBXItemCaretsFromSelLeftClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsFromSelLeft);
end;

procedure TfmMain.TBXItemCaretsFromSelRightClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsFromSelRight);
end;

procedure TfmMain.TBXItemCaretsFromSelClearClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsFromSelClear);
end;

procedure TfmMain.TBXItemCaretsExtUpLineClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsExtendUpLine);
end;

procedure TfmMain.TBXItemCaretsExtDownLineClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsExtendDownLine);
end;

procedure TfmMain.TBXItemCaretsExtUpPageClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsExtendUpPage);
end;

procedure TfmMain.TBXItemCaretsExtDownPageClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsExtendDownPage);
end;

procedure TfmMain.TBXItemCaretsExtUpEndClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsExtendUpEnd);
end;

procedure TfmMain.TBXItemCaretsExtDownEndClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsExtendDownEnd);
end;

procedure TfmMain.TBXItemCaretsFromMarksLeftClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsFromMarksLeft);
end;

procedure TfmMain.TBXItemCaretsFromMarksRightClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsFromMarksRight);
end;

procedure TfmMain.TBXItemCaretsFromMarksClearClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CaretsFromMarksClear);
end;

procedure TfmMain.TBXItemEColumnClick(Sender: TObject);
begin
  ecEditColumn.Execute;
end;

procedure TfmMain.ecEditColumnExecute(Sender: TObject);
begin
  with CurrentEditor do
    if HaveSelection then
      if SelectMode <> msColumn then
        MsgWarn(DKLangConstW('vert'), Handle)
      else
        ExecCommand(sm_CaretsFromSelLeft);
end;

procedure TfmMain.ecDedupAllExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdDedupAll);
end;

procedure TfmMain.ecDedupAllAndOrigExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdDedupAllAndOrig);
end;

procedure TfmMain.ecDedupAdjacentExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdDedupAdjacent);
end;

procedure TfmMain.TBXItemEDedupAllClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveDupsAll);
end;

procedure TfmMain.TBXItemEDedupAllOrigClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveDupsAllAndOrig);
end;

procedure TfmMain.TBXItemEDedupAdjacentClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveDupsAdjacent);
end;

procedure TfmMain.TBXItemBarDedupAdjClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveDupsAdjacent);
end;

procedure TfmMain.TBXItemBarDedupAllClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveDupsAll);
end;

procedure TfmMain.TBXItemBarDedupAndOrigClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveDupsAllAndOrig);
end;

procedure TfmMain.TBXItemESortDialogClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SortDialog);
end;

procedure TfmMain.TBXItemESortAscClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSortAscending);
end;

procedure TfmMain.TBXItemESortDescClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSortDescending);
end;

procedure TfmMain.TBXItemBarSortDialogClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SortDialog);
end;

procedure TfmMain.TBXItemBarSortAscClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSortAscending);
end;

procedure TfmMain.TBXItemBarSortDescClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSortDescending);
end;

procedure TfmMain.TBXItemBarCaseUpperClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smUpperCaseBlock);
end;

procedure TfmMain.TBXItemBarCaseLowerClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smLowerCaseBlock);
end;

procedure TfmMain.TBXItemBarCaseTitleClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smTitleCaseBlock);
end;

procedure TfmMain.TBXItemBarCaseInvertClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smToggleCaseBlock);
end;

procedure TfmMain.TBXItemBarCaseSentClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SentenceCaseBlock);
end;

procedure TfmMain.TBXItemBarCaseRandomClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RandomCaseBlock);
end;

procedure TfmMain.TBXItemECaseUpperClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smUpperCaseBlock);
end;

procedure TfmMain.TBXItemECaseLowerClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smLowerCaseBlock);
end;

procedure TfmMain.TBXItemECaseTitleClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smTitleCaseBlock);
end;

procedure TfmMain.TBXItemECaseInvertClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smToggleCaseBlock);
end;

procedure TfmMain.TBXItemECaseSentClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SentenceCaseBlock);
end;

procedure TfmMain.TBXItemECaseRandomClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RandomCaseBlock);
end;

procedure TfmMain.TBXItemEAlignWithSepClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AlignWithSeparator);
end;

procedure TfmMain.TBXItemERemBlanksClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveBlanks);
end;

procedure TfmMain.TBXItemEReduceBlanksClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ReduceBlanks);
end;

procedure TfmMain.TBXItemETrimLeadClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_TrimLeading);
end;

procedure TfmMain.TBXItemETrimTrailClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_TrimTrailing);
end;

procedure TfmMain.TBXItemETrimAllClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_TrimAll);
end;

procedure TfmMain.TBXItemERemDupSpClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_RemoveDupSpaces);
end;

procedure TfmMain.TBXItemETabToSpClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ConvertTabsToSpaces);
end;

procedure TfmMain.TBXItemESpToTabClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ConvertSpacesToTabsAll);
end;

procedure TfmMain.TBXItemESpToTabLeadClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ConvertSpacesToTabsLeading);
end;

procedure TfmMain.TBXItemECenterLinesClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CenterLines);
end;

procedure TfmMain.ecAlignWithSepExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdAlignWithSep);
end;

procedure TfmMain.TBXItemEJoinClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_JoinLines);
end;

procedure TfmMain.TBXItemESplitClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SplitLines);
end;

procedure TfmMain.TBXItemECopyLineClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CopyLine);
end;

procedure TfmMain.TBXItemECutLineClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CutLine);
end;

procedure TfmMain.TBXItemECopyAppClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CopyAppend);
end;

procedure TfmMain.TBXItemECutAppClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_CutAppend);
end;

procedure TfmMain.TBXItemEIndentClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smBlockIndent);
end;

procedure TfmMain.TBXItemEIndentLike1stClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_IndentLike1st);
end;

procedure TfmMain.TBXItemECommClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smCommentLines);
end;

procedure TfmMain.TBXItemEUncommClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smUncommentLines);
end;

procedure TfmMain.TBXItemTabToggleSplitClick(Sender: TObject);
begin
  if Assigned(FClickedFrame) then
    FClickedFrame.ToggleSplitted;
end;

procedure TfmMain.TBXItemCtxCopyClick(Sender: TObject);
begin
  with CurrentEditor do
    if not HaveSelection then
    begin
      if opCopyLineIfNoSel then
        ExecCommand(sm_CopyLine)
      else
        MsgBeep;
    end
    else
      ExecCommand(smCopy);
end;

procedure TfmMain.TBXItemCtxPasteClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smPaste);
end;

procedure TfmMain.TBXItemCtxDelClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smClearSelection);
end;

procedure TfmMain.TBXItemCtxSelectAllClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smSelectAll);
end;

procedure TfmMain.TBXItemCtxCutClick(Sender: TObject);
begin
  with CurrentEditor do
    if not HaveSelection then
    begin
      if opCopyLineIfNoSel then
        ExecCommand(sm_CutLine)
      else
        MsgBeep;
    end
    else
      ExecCommand(smCut);
end;

procedure TfmMain.ecToggleShowGroup2Execute(Sender: TObject);
begin
  case Groups.Mode of
    gmOne: Groups.Mode:= gm2Horz;
    gm2Horz,
    gm2Vert: Groups.Mode:= gmOne;
  end;
end;

procedure TfmMain.DoAcpCommand;
var
  Ed: TSyntaxMemo;
  SText: Widestring;
begin
  PluginACP.Items.Clear;
  PluginACP.DisplayItems.Clear;

  if not opAcpForceText then
    SText:= DoAcpFromPlugins(cActionGetAutoComplete)
  else
    SText:= '';

  //Python plugin?
  //it must show popup by itself.
  if (SText=cPyNone) then //plugin didn't handle
    SText:= '';
  if (SText=cPyTrue) then //plugin did handle
    Exit;

  //binary plugin?
  //it must return completions here.
  if (SText<>'') then
  begin
    Ed:= CurrentEditor;
    PluginAction_SuggestCompletion(PWChar(SText), EditorWordLength(Ed), true);
    Exit
  end;

  //usual completion from name.acp file
  DoAcpPopup;
end;

procedure TfmMain.PluginACPAfterComplete(Sender: TObject;
  const Item: WideString);
begin
  //need to force parameter hint, it doesn't appear auto on plugin ACP
  if ParamCompletion.Enabled then
    ParamCompletion.Execute;
end;

procedure TfmMain.DoExtendSelection(Ed: TSyntaxMemo);
var
  Lex, Err: string;
begin
  if Ed.SyntObj=nil then
  begin
    DoHint('Extend selection: no lexer active');
    MsgBeep;
    Exit
  end;

  //we have two variants of code: for usual code (Pascal/C/PHP/etc) and for HTML/XML.
  //HTML/XML case is special, need precise jumps considering "<" and ">".
  Lex:= CurrentLexer;
  if IsLexerHTML(Lex) or IsLexerXML(Lex) then
  begin
    EditorExtendSelectionByLexer_HTML(Ed);
  end
  else
  begin
    EditorExtendSelectionByLexer_All(Ed, Err);
    if Err<>'' then
      begin DoHint(Err); MsgBeep; end;
  end
end;

procedure TfmMain.TBXItemSSelExtendClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SelectionExtend);
end;

procedure TfmMain.TBXItemTreeExpandAllClick(Sender: TObject);
begin
  DoTreeFocus;
  Tree.FullExpand;
  if Tree.Items.Count>0 then
  begin
    Tree.Selected:= Tree.Items[0];
    Tree.Selected.MakeVisible;
  end;
end;

procedure TfmMain.TBXItemTreeCollapseAllClick(Sender: TObject);
begin
  DoTreeFocus;
  Tree.FullCollapse;
end;

procedure TfmMain.DoTreeLevel(NLevel: Integer);
var
  i: Integer;
begin
  DoTreeFocus;
  with Tree do
  begin
    Items.BeginUpdate;
    try
      FullExpand;
      if Items.Count>0 then
      begin
        for i:= 0 to Items.Count-1 do
          if Items[i].Level >= NLevel then
            Items[i].Collapse(true);
        Selected:= Items[0];
        Selected.MakeVisible;
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TfmMain.TBXItemTreeLevel2Click(Sender: TObject);
begin
  DoTreeLevel(2);
end;

procedure TfmMain.TBXItemTreeLevel3Click(Sender: TObject);
begin
  DoTreeLevel(3);
end;

procedure TfmMain.TBXItemTreeLevel4Click(Sender: TObject);
begin
  DoTreeLevel(4);
end;

procedure TfmMain.TBXItemTreeLevel5Click(Sender: TObject);
begin
  DoTreeLevel(5);
end;

procedure TfmMain.TBXItemTreeLevel6Click(Sender: TObject);
begin
  DoTreeLevel(6);
end;

procedure TfmMain.TBXItemTreeLevel7Click(Sender: TObject);
begin
  DoTreeLevel(7);
end;

procedure TfmMain.TBXItemTreeLevel8Click(Sender: TObject);
begin
  DoTreeLevel(8);
end;

procedure TfmMain.TBXItemTreeLevel9Click(Sender: TObject);
begin
  DoTreeLevel(9);
end;

procedure TfmMain.ecReverseLinesExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdReverse);
end;

procedure TfmMain.TBXItemEReverseClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ReverseLines);
end;

procedure TfmMain.ecShuffleLinesExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdShuffle);
end;

procedure TfmMain.TBXItemEShuffleClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ShuffleLines);
end;

procedure TfmMain.DoFoldLevel(NLevel: Integer);
var
  Ed: TSyntaxMemo;
begin
  Ed:= CurrentEditor;
  if Ed.SyntObj=nil then
    begin DoHint('Fold level: no lexer active'); MsgBeep; Exit end;
  EditorFoldLevel(Ed, NLevel);
end;


procedure TfmMain.TBXItemFoldLevel2Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel2);
end;

procedure TfmMain.TBXItemFoldLevel3Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel3);
end;

procedure TfmMain.TBXItemFoldLevel4Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel4);
end;

procedure TfmMain.TBXItemFoldLevel5Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel5);
end;

procedure TfmMain.TBXItemFoldLevel6Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel6);
end;

procedure TfmMain.TBXItemFoldLevel7Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel7);
end;

procedure TfmMain.TBXItemFoldLevel8Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel8);
end;

procedure TfmMain.TBXItemFoldLevel9Click(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FoldLevel9);
end;

procedure TfmMain.TBXItemBarCommClick(Sender: TObject);
begin
  DoToolbarCommentUncomment(true);
end;

procedure TfmMain.TBXItemBarUncomClick(Sender: TObject);
begin
  DoToolbarCommentUncomment(false);
end;

procedure TfmMain.DoToolbarCommentUncomment(AComment: boolean);
var
  Lex: string;
  Cmd: Integer;
begin
  //this code is to call "Toggle stream comment" for HTML and CSS.
  //on other lexs it should call default commands "Comment lines"/"Uncomment lines"
  Lex:= CurrentLexer;
  if IsLexerHTML(Lex) or IsLexerCSS(Lex) then
    CurrentEditor.ExecCommand(sm_ToggleStreamComment)
  else
  begin
    Cmd:= IfThen(AComment, smCommentLines, smUncommentLines);
    if ecCommentLines.Enabled then
      CurrentEditor.ExecCommand(Cmd)
    else
      MsgBeep;
  end;
end;

{
function TfmMain.DockTypeToName(Typ: TSynDock): string;
begin
  case Typ of
    sdockTop: Result:= TbxDockTop.Name;
    sdockLeft: Result:= TbxDockLeft.Name;
    sdockRight: Result:= TbxDockRight.Name;
    sdockBottom: Result:= TbxDockBottom.Name;
    else raise Exception.Create('Unknown dock type');
  end;
end;
}

procedure TfmMain.LoadToolbarProp(
  Toolbar: TSpTbxToolbar;
  Ini: TCustomIniFile;
  const Id: string);
var
  ADockStr: string;
  ADock: TTBDock;
begin
  Toolbar.Visible:= Ini.ReadBool('Menu', 'tb'+Id+'Vis', Toolbar.Visible);

  if Toolbar.CurrentDock<>nil then
    ADockStr:= Toolbar.CurrentDock.Name
  else
    ADockStr:= '';
  ADockStr:= Ini.ReadString('Menu', 'tb'+Id, ADockStr);

  try
    ADock:= Self.FindComponent(ADockStr) as TTBDock;
    //work for incorrect ini setting (cannot place toolbar on MultiDock)
    if ADock=TbxDockLeft then ADock:= TbxDockLeft1 else
     if ADock=TbxDockRight then ADock:= TbxDockRight1 else
      if ADock=TbxDockBottom then ADock:= TbxDockBottom1;

    Toolbar.CurrentDock:= ADock;
    Toolbar.DockPos:= Ini.ReadInteger('Menu', 'tb'+Id+'Pos', Toolbar.DockPos);
    Toolbar.DockRow:= Ini.ReadInteger('Menu', 'tb'+Id+'Row', Toolbar.DockRow);
  except
    MsgWarn(WideFormat('Cannot dock toolbar "%s" on "%s"', [Id, ADockStr]), Handle);
  end;
end;

procedure TfmMain.SaveToolbarProp(
  Toolbar: TSpTbxToolbar;
  Ini: TCustomIniFile;
  const Id: string);
begin
  Ini.WriteBool('Menu', 'tb'+Id+'Vis', Toolbar.Visible);
  if Toolbar.CurrentDock<>nil then
    Ini.WriteString('Menu', 'tb'+Id, Toolbar.CurrentDock.Name);
  Ini.WriteInteger('Menu', 'tb'+Id+'Pos', Toolbar.DockPos);
  Ini.WriteInteger('Menu', 'tb'+Id+'Row', Toolbar.DockRow);
end;

procedure TfmMain.SavePanelProp(
  Panel: TSpTbxDockablePanel;
  Ini: TCustomIniFile;
  const Id: string);
begin
  try
    with Panel do
    begin
      Ini.WriteBool('pl'+Id, 'Vis', Visible);
      if CurrentDock<>nil then
        Ini.WriteString('pl'+Id, 'Dock', CurrentDock.Name);

      Ini.WriteInteger('pl'+Id, 'DW', Width);
      Ini.WriteInteger('pl'+Id, 'DH', Height);
      Ini.WriteInteger('pl'+Id, 'DPos', DockPos);
      Ini.WriteInteger('pl'+Id, 'DRow', DockRow);
      Ini.WriteBool('pl'+Id, 'Fl', Floating);
      Ini.WriteInteger('pl'+Id, 'FlW', FloatingClientWidth);
      Ini.WriteInteger('pl'+Id, 'FlH', FloatingClientHeight);
      Ini.WriteInteger('pl'+Id, 'FlX', FloatingPosition.X);
      Ini.WriteInteger('pl'+Id, 'FlY', FloatingPosition.Y);
    end;
  except
  end;
end;

procedure TfmMain.LoadPanelProp(
  Panel: TSpTbxDockablePanel;
  Ini: TCustomIniFile;
  const Id: string;
  DefFloating: boolean = false);
var
  p: TPoint;
  ADockStr: string;
  ADock: TTBDock;
begin
  with Panel do
  begin
    if not QuickView then
    begin
      Visible:= Ini.ReadBool('pl'+Id, 'Vis', Visible);
      if Assigned(Panel.OnVisibleChanged) then
        Panel.OnVisibleChanged(Self);
    end;

    if CurrentDock<>nil then
      ADockStr:= CurrentDock.Name
    else
      ADockStr:= '';
    ADockStr:= Ini.ReadString('pl'+Id, 'Dock', ADockStr);

    try
      ADock:= Self.FindComponent(ADockStr) as TTBDock;
      CurrentDock:= ADock;
    except
      MsgWarn(WideFormat('Cannot dock panel "%s" on "%s"', [Id, ADockStr]), Handle);
    end;

    DockPos:= Ini.ReadInteger('pl'+Id, 'DPos', DockPos);
    DockRow:= Ini.ReadInteger('pl'+Id, 'DRow', DockRow);

    Width:= Ini.ReadInteger('pl'+Id, 'DW', Width);
    Height:= Ini.ReadInteger('pl'+Id, 'DH', Height);
    Floating:= Ini.ReadBool('pl'+Id, 'Fl', DefFloating);
    FloatingClientWidth:= Ini.ReadInteger('pl'+Id, 'FlW', FloatingClientWidth);
    FloatingClientHeight:= Ini.ReadInteger('pl'+Id, 'FlH', FloatingClientHeight);
    p.X:= Ini.ReadInteger('pl'+Id, 'FlX', FloatingPosition.X);
    p.Y:= Ini.ReadInteger('pl'+Id, 'FlY', FloatingPosition.Y);
    FloatingPosition:= p;
  end;
end;

procedure TfmMain.TBXItemTUser1Click(Sender: TObject);
begin
  with tbUser1 do
    Visible:= not Visible;
  UpdateStatusbar;
  SaveToolbarsProps;
end;

procedure TfmMain.TBXItemTUser2Click(Sender: TObject);
begin
  with tbUser2 do
    Visible:= not Visible;
  UpdateStatusbar;
  SaveToolbarsProps;
end;

procedure TfmMain.TBXItemTUser3Click(Sender: TObject);
begin
  with tbUser3 do
    Visible:= not Visible;
  UpdateStatusbar;
  SaveToolbarsProps;
end;

procedure TfmMain.DoToolbar_CustomizeAndReload(Id: TSynUserToolbarId);
var
  Toolbar: TSpTbxToolbar;
  ToolbarId: string;
begin
  case Id of
    synToolbar1: begin Toolbar:= tbUser1; ToolbarId:= '1'; end;
    synToolbar2: begin Toolbar:= tbUser2; ToolbarId:= '2'; end;
    synToolbar3: begin Toolbar:= tbUser3; ToolbarId:= '3'; end;
    else
      raise Exception.Create('Unknown toolbar id: '+IntToStr(Ord(Id)));
  end;

  if DoToolbar_Customize(ToolbarId) then
  begin
    DoToolbar_LoadContent(Toolbar, ToolbarId, true);
    Toolbar.Refresh;
    UpdateStatusbar;
    DoRepaint;
  end;
end;

procedure TfmMain.TBXItemOToolbar1Click(Sender: TObject);
begin
  DoToolbar_CustomizeAndReload(synToolbar1);
end;

procedure TfmMain.TBXItemOToolbar2Click(Sender: TObject);
begin
  DoToolbar_CustomizeAndReload(synToolbar2);
end;

procedure TfmMain.TBXItemOToolbar3Click(Sender: TObject);
begin
  DoToolbar_CustomizeAndReload(synToolbar3);
end;

function TfmMain.DoToolbar_Customize(const Id: string): boolean;
var
  Dir: string;
begin
  with TIniFile.Create(SynHistoryIni) do
  try
    Dir:= ReadString('Win', 'ImagesDir', '');
    Result:= DoShowToolbarProp(
      SynToolbarsIni,
      Id,
      DoShowCmdList_ForTools,
      DoShowCmdHint_ForTools,
      DoEnumExtTools,
      DoEnumPyTools,
      0, 0,
      Dir);
    WriteString('Win', 'ImagesDir', Dir);
  finally
    Free
  end;
end;

function TfmMain.DoShowCmdHint_ForTools(Cmd: Widestring): Widestring;
var
  N, i: Integer;
begin
  Result:= '';
  if Cmd='-' then Exit;

  if SBegin(Cmd, 'cm:') then
  begin
    SDeleteToW(Cmd, ':');
    N:= StrToIntDef(Cmd, 0);
    if N=0 then Exit;

    for i:= 0 to SyntKeyMapping.Items.Count-1 do
      with SyntKeyMapping.Items[i] do
        if Command = N then
        begin
          Result:= Category + ': ' + DisplayName;
          Exit
        end;
  end
  else
    Result:= Cmd;
end;

procedure TfmMain.DoToolbar_LoadContent(Toolbar: TSpTbxToolbar;
  Id: string; AutoShow: boolean);
var
  Ini: TIniFile;
  ImgList: TPngImageList;
begin
  Ini:= TIniFile.Create(SynToolbarsIni);
  try
    ImgList:= Toolbar.Images as TPngImageList;
    ImgList.Width:= Ini.ReadInteger(Id, 'ix', 32);
    ImgList.Height:= Ini.ReadInteger(Id, 'iy', 32);

    ImgList.BeginUpdate;
    try
      Toolbar.Items.Clear;
      DoToolbar_LoadProps(Ini, ImgList, Toolbar, Id);
    finally
      ImgList.EndUpdate;
    end;
  finally
    FreeAndNil(Ini);
  end;

  if Toolbar.Items.Count=0 then
    Toolbar.Visible:= false
  else
  if AutoShow then
  begin
    Toolbar.Visible:= true;
    SaveToolbarsProps;
  end;
end;

procedure TfmMain.DoToolbar_LoadProps(
  Ini: TIniFile; ImgList: TPngImageList;
  Toolbar: TObject; Id: string);
var
  Item: TTbCustomItem;
  SCmd, SHint, SIcoFN: Widestring;
  IcoLoaded, IsSubmenu, IsSep: boolean;
  i: Integer;
begin
  for i:= 0 to High(TToolbarProps) do
  begin
    SCmd:= UTF8Decode(Ini.ReadString(Id, IntToStr(i)+'c', ''));
    SHint:= UTF8Decode(Ini.ReadString(Id, IntToStr(i)+'h', ''));
    SIcoFN:= UTF8Decode(Ini.ReadString(Id, IntToStr(i)+'i', ''));
    SReplaceAllW(SIcoFN, '{ini}', ExtractFileDir(SynIni));

    if (SCmd='') and (SHint='') and (SIcoFN='') then Break;
    IcoLoaded:= false;
    IsSep:= SCmd='-';
    IsSubmenu:= SBegin(SCmd, 'm:');

    if IsSep then
    //create separator
    begin
      Item:= TSpTbxSeparatorItem.Create(Self);
    end
    else
    //create submenu
    if IsSubmenu then
    begin
      Item:= TSpTbxSubmenuItem.Create(Self);
      if SCmd='m:{recent}' then
      begin
        Item.LinkSubitems:= TBXSubmenuItemFRecents;
      end
      else
      if SCmd='m:{new}' then
      begin
        Item.LinkSubitems:= TBXSubmenuItemFNew;
      end
      else
      if SCmd='m:{sess}' then
      begin
        Item.LinkSubitems:= TBXSubmenuItemSess;
      end
      else
      if SCmd='m:{colors}' then
      begin
        Item.LinkSubitems:= TbxSubmenuColors;
      end
      else
      if SCmd='m:{enc-chg}' then
      begin
        Item.LinkSubitems:= TBXSubmenuEncReread;
      end
      else
      if SCmd='m:{enc-conv}' then
      begin
        Item.LinkSubitems:= TBXSubmenuEncConvert;
      end
      else
      if SCmd='m:{folding}' then
      begin
        Item.LinkSubitems:= PopupFoldLevel.Items;
      end
      else
      if SCmd='m:{foldlevel}' then
      begin
        Item.LinkSubitems:= TBXSubmenuFoldLevel;
      end
      else
      if SCmd='m:{nonprint}' then
      begin
        Item.LinkSubitems:= TBXSubmenuNonPrint;
      end
      else
      if SCmd='m:{projects}' then
      begin
        Item.LinkSubitems:= TBXSubmenuItemProjRecents;
      end
      else
      if SCmd='m:{plugins}' then
      begin
        Item.LinkSubitems:= TBXSubmenuPlugins;
      end
      else
      if SCmd='m:{projtools}' then
      begin
        Item.LinkSubitems:= TbxSubmenuProjTools;
      end
      else
      if SCmd='m:{case}' then
      begin
        Item.LinkSubitems:= TBXSubmenuItemCaseOps;
      end
      else
      if SCmd='m:{blank}' then
      begin
        Item.LinkSubitems:= TBXSubmenuItemBlankOps;
      end
      else
      if SCmd='m:{cmt}' then
      begin
        Item.LinkSubitems:= TBXSubmenuItemCommentOps;
      end
      else
      if SCmd='m:{carets}' then
      begin
        Item.LinkSubitems:= TbxSubmenuItemCaretsOps;
      end
      else
      if SCmd='m:{websearch}' then
      begin
        Item.LinkSubitems:= TbxSubmenuWeb;
      end
      else
      if SCmd='m:{addman}' then
      begin
        Item.LinkSubitems:= TbxSubmenuAddons;
      end
      else
      begin
        FUserToolbarCommands.Add(SCmd);
        Item.Tag:= FUserToolbarCommands.Count-1;
      end;
      Item.Images:= ImgList; //inherit ImageList
      Item.Options:= [tboDropdownArrow];
      Item.OnSelect:= ButtonOnSelect;
      IcoLoaded:= LoadPngIconEx(ImgList, SIcoFN);
    end
    else
    //create usual command item
    begin
      Item:= TSpTbxItem.Create(Self);
      FUserToolbarCommands.Add(SCmd);
      Item.Enabled:= SCmd<>'';
      Item.Tag:= FUserToolbarCommands.Count-1;
      Item.OnClick:= DoToolbar_OnClick;
      Item.OnSelect:= ButtonOnSelect;
      IcoLoaded:= LoadPngIconEx(ImgList, SIcoFN);

      //add Action to "options" buttons, so toggling will check/uncheck these buttons
      UpdateToolbarItemAction(Item, SCmd);
    end;  

    //handle "*" at end of hint
    if (SHint<>'') and (SHint[Length(SHint)]='*') then
    begin
      SetLength(SHint, Length(SHint)-1);
      Item.DisplayMode:= nbdmImageAndText;
    end;
    //set caption and hint last
    Item.Hint:= SHint;
    if Item is TSpTbxItem then
      (Item as TSpTbxItem).Caption:= SHint;

    //now add ready button to toolbar or submenu
    if Toolbar is TSpTbxToolbar then
    begin
      (Toolbar as TSpTbxToolbar).Items.Add(Item);
      if IcoLoaded then
        (Toolbar as TSpTbxToolbar).Items[(Toolbar as TSpTbxToolbar).Items.Count-1].ImageIndex:= ImgList.PngImages.Count-1;
    end
    else
    if Toolbar is TSpTbxSubmenuItem then
    begin
      (Toolbar as TSpTbxSubmenuItem).Add(Item);
      if IcoLoaded then
        (Toolbar as TSpTbxSubmenuItem).Items[(Toolbar as TSpTbxSubmenuItem).Count-1].ImageIndex:= ImgList.PngImages.Count-1;
    end;

    //load submenu contents
    if IsSubmenu then
      DoToolbar_LoadProps(Ini, ImgList, Item, SCmd);
  end;
end;

procedure TfmMain.DoToolbar_OnClick(Sender: TObject);
var
  Cmd: Widestring;
  NCmd: Integer;
begin
  NCmd:= (Sender as TSpTbxItem).Tag;
  if not ((NCmd>=0) and (NCmd<FUserToolbarCommands.Count)) then
    begin MsgBeep; Exit end;
  Cmd:= FUserToolbarCommands[NCmd];

  if SBegin(Cmd, 'cm:') then
    DoToolbar_RunCommand_InternalCmd(Cmd)
  else
  if SBegin(Cmd, 'ext:') then
    DoToolbar_RunCommand_ExtTool(Cmd)
  else
  if SBegin(Cmd, cPyPrefix) then
    DoToolbar_RunCommand_PyPlugin(Cmd)
  else
  if SBegin(Cmd, cBinaryPrefix) then
    DoToolbar_RunCommand_BinaryPlugin(Cmd);
end;

procedure TfmMain.DoToolbar_RunCommand_InternalCmd(Cmd: Widestring);
var
  NCmd: Integer;
begin
  SDeleteToW(Cmd, ':');
  NCmd:= StrToIntDef(Cmd, 0);
  if NCmd>0 then
    CurrentEditor.ExecCommand(NCmd);
end;

procedure TfmMain.DoToolbar_RunCommand_ExtTool(Cmd: Widestring);
var
  i: Integer;
begin
  SDeleteToW(Cmd, ':');
  for i:= Low(opTools) to High(opTools) do
    if opTools[i].ToolCaption=Cmd then
    begin
      DoTool_Run(opTools[i]);
      Exit
    end;
  MsgError(WideFormat(DKLangConstW('MRun'), [Cmd]), Handle);
end;

procedure TfmMain.DoToolbar_RunCommand_PyPlugin(const Str: Widestring);
var
  PyCmd, PyFile: Widestring;
begin
  PyCmd:= Str;
  PyFile:= SGetItem(PyCmd, '/');
  DoPyLoadPlugin(PyFile, PyCmd);
end;

procedure TfmMain.DoToolbar_RunCommand_BinaryPlugin(const Str: Widestring);
var
  BinCmd, BinFile: Widestring;
begin
  BinCmd:= Str;
  BinFile:= SGetItem(BinCmd, '/');
  SDeleteToW(BinFile, ':');
  DoPlugin_LoadAction(BinFile, cActionMenuCommand, PWChar(Widestring(BinCmd)), nil, nil, nil);
end;


procedure TfmMain.DoEnumExtTools(L: TTntStringList);
var
  i: Integer;
begin
  for i:= Low(opTools) to High(opTools) do
    with opTools[i] do
      if ToolCaption<>'' then
        L.Add(ToolCaption);
end;

procedure TfmMain.DoEnumPyTools(L: TTntStringList);
var
  i: Integer;
  S: Widestring;
begin
  for i:= Low(FPluginsCommand) to High(FPluginsCommand) do
    with FPluginsCommand[i] do
      if SFileName<>'' then
        if not SBegin(ExtractFileName(SCaption), '-') then
        begin
          S:= SFileName + '/' + SCmd;
          if not SBegin(S, cPyPrefix) then
            S:= cBinaryPrefix+S;
          L.Add(S);
        end;
end;

procedure TfmMain.DoEnumIcons(L: TTntStringList);
var
  i: Integer;
begin
  FFindToList(L, SynIconsDir, '*.tar', '',
    false{SubDirs}, false, false, false);

  for i:= 0 to L.Count-1 do
    L[i]:= ChangeFileExt(ExtractFileName(L[i]), '');
  L.Sort;
end;

procedure TfmMain.ecExtractDupsCaseExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdExtractDupsCase);
end;

procedure TfmMain.ecExtractDupsNoCaseExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdExtractDupsNoCase);
end;

procedure TfmMain.ecExtractUniqExecute(Sender: TObject);
begin
  DoLinesCommand(cLineCmdExtractUniq);
end;

procedure TfmMain.TBXItemEExtractDupCaseClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ExtractDupsCase);
end;

procedure TfmMain.TBXItemEExtractDupNoCaseClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ExtractDupsNoCase);
end;

procedure TfmMain.TBXItemEExtractUniqClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ExtractUniqueLines);
end;


procedure TfmMain.ecNonPrintSpacesExecute(Sender: TObject);
begin
  with CurrentEditor do
  begin
    NonPrinted.Visible:= true;
    NonPrintedSpaces:= true;
    NonPrintedEol:= false;
    Invalidate;
  end;
  UpdateStatusBar;
end;

procedure TfmMain.ecNonPrintEolExecute(Sender: TObject);
begin
  with CurrentEditor do
  begin
    NonPrinted.Visible:= true;
    NonPrintedSpaces:= false;
    NonPrintedEol:= true;
    Invalidate;
  end;
  UpdateStatusBar;
end;

procedure TfmMain.ecNonPrintBothExecute(Sender: TObject);
begin
  with CurrentEditor do
  begin
    NonPrinted.Visible:= true;
    NonPrintedSpaces:= true;
    NonPrintedEol:= true;
    Invalidate;
  end;
  UpdateStatusBar;
end;

procedure TfmMain.TBXItemCtxPasteAsColumnClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_PasteAsColumnBlock);
end;

procedure TfmMain.TBXItemCtxPasteBkmkLinesClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_BookmarksPaste);
end;

procedure TfmMain.ecNonPrintEolDetailsExecute(Sender: TObject);
begin
  with CurrentEditor do
  begin
    NonPrintedEolDetails:= not NonPrintedEolDetails;
    Invalidate;
  end;
  UpdateStatusBar;
end;

procedure TfmMain.InitMenuItemsList;
begin
  SetLength(FMenuItems, 16+1);
  //
  with FMenuItems[0] do begin Id:= 'file'; Item:= TbxSubmenuFile; end;
  with FMenuItems[1] do begin Id:= 'edit'; Item:= TbxSubmenuEdit; end;
  with FMenuItems[2] do begin Id:= 'search'; Item:= TbxSubmenuSearch; end;
  with FMenuItems[3] do begin Id:= 'bookmarks'; Item:= TbxSubmenuBookmarks; end;
  with FMenuItems[4] do begin Id:= 'tools'; Item:= TbxSubmenuTools; end;
  with FMenuItems[5] do begin Id:= 'macros'; Item:= TbxSubmenuMacros; end;
  with FMenuItems[6] do begin Id:= 'options'; Item:= TbxSubmenuOptions; end;
  with FMenuItems[7] do begin Id:= 'view'; Item:= TbxSubmenuView; end;
  with FMenuItems[8] do begin Id:= 'window'; Item:= TbxSubmenuWindow; end;
  with FMenuItems[9] do begin Id:= 'help'; Item:= TbxSubmenuHelp; end;
  //
  with FMenuItems[10] do begin Id:= 'g'; Item:= TBXSubmenuGroups; end;
  with FMenuItems[11] do begin Id:= 'x'; Item:= TbxItemMenuX; end;
  with FMenuItems[12] do begin Id:= 'xx'; Item:= TbxItemMenuXX; end;
  //
  with FMenuItems[13] do begin Id:= 'toolbar-file'; Item:= tbFile; end;
  with FMenuItems[14] do begin Id:= 'toolbar-edit'; Item:= tbEdit; end;
  with FMenuItems[15] do begin Id:= 'toolbar-view'; Item:= tbView; end;
  with FMenuItems[16] do begin Id:= 'context'; Item:= PopupEditor; end;
end;

procedure TfmMain.TBXItemOHideItemsClick(Sender: TObject);
begin
  DoConfigHideItems;
end;

procedure TfmMain.DoConfigHideItems;
const
  cSep: Widestring = '———';
  cSub: Widestring = '  >>';
var
  i, j: Integer;
  Item: TComponent;
  Id, S: Widestring;
begin
  with TfmHideItems.Create(nil) do
  try
    FIniFN:= SynHideIni;
    for i:= Low(FMenuItems) to High(FMenUItems) do
    begin
      Item:= FMenuItems[i].Item;
      Id:= FMenuItems[i].Id;

      //is it toolbar?
      if Item is TSpTbxToolbar then
      begin
        for j:= 0 to (Item as TSpTbxToolbar).Items.Count-1 do
        begin
          if (Item as TSpTbxToolbar).Items[j] is TSpTbxSeparatorItem then
            S:= cSep
          else
          begin
            S:= ((Item as TSpTbxToolbar).Items[j] as TSpTbxItem).Caption;
            S:= SStripFromTab(S);
            if (Item as TSpTbxToolbar).Items[j] is TSpTbxSubmenuItem then
              S:= S+cSub;
          end;
          List.Items.Add('['+id+' '+IntToStr(j)+']  '+S);
        end;
      end
      else
      //is it context menu?
      if Item is TSpTbxPopupMenu then
      begin
        for j:= 0 to 14{max index is of "More..." item} do
        begin
          if (Item as TSpTbxPopupMenu).Items[j] is TSpTbxSeparatorItem then
            S:= cSep
          else
          begin
            S:= ((Item as TSpTbxPopupMenu).Items[j] as TSpTbxItem).Caption;
            SDeleteFromW(S, #9);
            if (Item as TSpTbxPopupMenu).Items[j] is TSpTbxSubmenuItem then
              S:= S+cSub;
          end;
          List.Items.Add('['+id+' '+IntToStr(j)+']  '+S);
        end;
      end
      else
      if Item is TSpTbxSubmenuItem then
      begin
        //menu item
        S:= (Item as TSpTbxSubmenuItem).Caption;
        SReplaceAllW(S, '&', '');
        List.Items.Add('['+id+']  '+S);
        //subitems (not for "window" item)
        if id<>'window' then
        for j:= 0 to (Item as TSpTbxSubmenuItem).Count-1 do
        begin
          if (Item as TSpTbxSubmenuItem).Items[j] is TSpTbxSeparatorItem then
            S:= cSep
          else
          begin
            S:= ((Item as TSpTbxSubmenuItem).Items[j] as TSpTbxItem).Caption;
            S:= SStripFromTab(S);
            SReplaceAllW(S, '&', '');
            if (Item as TSpTbxSubmenuItem).Items[j] is TSpTbxSubmenuItem then
              S:= S+cSub;
          end;
          List.Items.Add('    '+'['+id+' '+IntToStr(j)+']  '+S);
        end;
      end
      else
      if Item is TSpTbxItem then
      begin
        List.Items.Add('['+Id+']');
      end
      else
      begin
        MsgError('Unknown item type: '+Id, Handle);
        Exit
      end;
      //separator
      List.Items.Add('');
    end;

    if ShowModal=mrOk then
      begin end;
  finally
    Free
  end;
end;

procedure TfmMain.TBXItemOEditSynIniClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_EditSynIni);
end;

procedure TfmMain.acOpenBySelectionExecute(Sender: TObject);
begin
  DoOpenBySelection;
end;

function TfmMain.opMarkDeletedAsModified: boolean;
begin
  Result:= Bool(SynHiddenOption('MarkDeletedModified', 1));
end;

procedure TfmMain.ApplyTabOptions;
var
  i: Integer;
begin
  if not Assigned(Groups) then Exit;

  Groups.SetTabFont(FFontTabs);
  Groups.SetTabOption(tabOptionFontSize, FFontTabs.Size);
  
  Groups.SetTabOption(tabColorText, opColorTabText);
  Groups.SetTabOption(tabColorTextModified, opColorTabTextMod);
  Groups.SetTabOption(tabColorBgActive, opColorTabBgActive);
  Groups.SetTabOption(tabColorBgPassive, opColorTabBgPassive);
  Groups.SetTabOption(tabColorBgPassiveOver, opColorTabBgPassiveOver);
  Groups.SetTabOption(tabColorBorderActive, opColorTabBorderActive);
  Groups.SetTabOption(tabColorBorderPassive, opColorTabBorderPassive);

  Groups.SetTabOption(tabOptionAngle, opTabAngle);
  Groups.SetTabOption(tabOptionShowTabs, Ord(opTabVisible));
  Groups.SetTabOption(tabOptionShowXButtons, Ord(opTabXButtons));
  Groups.SetTabOption(tabOptionShowPlus, Ord(opTabPlus));
  Groups.SetTabOption(tabOptionShowNums, Ord(opTabNums));
  Groups.SetTabOption(tabOptionShowEntireColor, Ord(opTabEntireColor));
  Groups.SetTabOption(tabOptionDoubleClickClose, Ord(opTabDblClickClose));
  Groups.SetTabOption(tabOptionBottomTabs, Ord(opTabAtBottom));
  Groups.SetTabOption(tabOptionDragDrop, Ord(opTabDragDrop));
  Groups.SetTabOption(tabOptionWidthMin, Ord(opTabWidthMin));
  Groups.SetTabOption(tabOptionWidthMax, Ord(opTabWidthMax));

  for i:= 0 to FrameAllCount-1 do
    with FramesAll[i] do
    begin
      DoTitleChanged;
    end;

  ApplyTabOptionsTo(TabsLeft);
  ApplyTabOptionsTo(TabsRight);
  ApplyTabOptionsTo(TabsOut);
end;

procedure TfmMain.FixSplitters;
begin
  SplitterBottom.Top:= 0;
  SplitterLeft.Left:= ClientWidth;
  SplitterRight.Left:= 0;

  TbxDockLeft1.Left:= SplitterLeft.Left+4;
  TbxDockRight1.Left:= 0;

  SplitterBottom.Visible:= TbxDockBottom.Height>0;
  SplitterLeft.Visible:= TbxDockLeft.Width>0;
  SplitterRight.Visible:= TbxDockRight.Width>0;
end;

procedure TfmMain.StatusItemCaretClick(Sender: TObject);
begin
  ecGoto.Execute;
end;

procedure TfmMain.StatusItemEncClick(Sender: TObject);
begin
  with Mouse.CursorPos do
    PopupStatusEnc.Popup(X, Y);
end;

procedure TfmMain.StatusItemEndsClick(Sender: TObject);
begin
  with Mouse.CursorPos do
    PopupStatusLineEnds.Popup(X, Y);
end;

procedure TfmMain.StatusItemLexerClick(Sender: TObject);
begin
  with Mouse.CursorPos do
    PopupLexers.Popup(X, Y);
end;

procedure TfmMain.StatusItemCharClick(Sender: TObject);
begin
   with CurrentEditor do
     if not ReadOnly then
       ExecCommand(smSelCharacter);
end;

procedure TfmMain.StatusItemROClick(Sender: TObject);
begin
  ecReadOnly.Execute;
end;

procedure TfmMain.StatusItemWrapClick(Sender: TObject);
begin
  ecWrap.Execute;
end;

procedure TfmMain.StatusItemSelModeClick(Sender: TObject);
begin
  with CurrentEditor do
    case SelectModeDefault of
      msNone,
      msNormal: SelectModeDefault:= msColumn;
      msColumn: SelectModeDefault:= msLine;
      msLine: SelectModeDefault:= msNormal;
    end;
end;

procedure TfmMain.StatusItemZoomClick(Sender: TObject);
begin
  with Mouse.CursorPos do
    PopupZoom.Popup(X, Y);
end;

procedure TfmMain.TBXTabColorGetColor(Sender: TObject; ACol, ARow: Integer;
  var Color: TColor; var Name: WideString);
begin
  Color:= opTabColors[ACol + ARow * 5];
  Name:= SColorToHtmlCode(Color);
end;

procedure TfmMain.TBXTabColorCellClick(Sender: TObject; ACol,
  ARow: Integer; var Allow: Boolean);
begin
  DoSetTabColorIndex(ACol + ARow * 5 + 1);
end;

procedure TfmMain.StatusItemCaretDrawHint(Sender: TObject;
  AHintBitmap: TBitmap; var AHint: WideString; var PaintDefault: Boolean);
begin
  //update statusbar hint
  AHint:= SStatusHint(FCurrSelState);
  ecTextOut(AHintBitmap.Canvas, 2, 2, AHint);
end;

procedure TfmMain.TBXSubmenuItemFRecentsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  i: Integer;
begin
  with TBXMRUListItemFRecents do
  begin
    Clear;
    for i:= SynMruFiles.Items.Count-1 downto 0 do
      MRUAdd(SynMruFiles.Items[i]);
  end;

  //fix too big menuitems height
  FixMruBigImageList(TBXMRUListItemFRecents);
  FixMenuBigImageList(TbxSubmenuItemFRecents);
end;

procedure TfmMain.TBXSubmenuItemSessPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  i: Integer;
begin
  with TBXMRUListItem_Sess do
  begin
    Clear;
    for i:= SynMruSessions.Items.Count-1 downto 0 do
      MRUAdd(SynMruSessions.Items[i]);
  end;
end;

function TfmMain.SynSkinsDir: string;
begin
  Result:= SynDataSubdir(cSynDataSkins);
end;

function TfmMain.SynSnippetsDir: string;
begin
  Result:= SynDataSubdir(cSynDataSnippets);
end;

function TfmMain.SynIconsDir: string;
begin
  Result:= SynDataSubdir(cSynDataIcons);
end;

function TfmMain.SynPyDir: string;
begin
  //at the time of this call, SynDir not yet inited
  Result:= ExtractFilePath(ParamStr(0)) + 'Py';
end;

function TfmMain.SynSkinFilename(const Name: string): string;
begin
  Result:= SynSkinsDir + '\' + Copy(Name, 2, MaxInt) + '.skn';
end;

procedure TfmMain.DoColorsArrayApply(const C: TSynColors; Ed: TSyntaxMemo);
begin
  //fonts first (colors will override)
  Ed.Font:= TemplateEditor.Font;
  Ed.HorzRuler.Font:= TemplateEditor.HorzRuler.Font;
  Ed.LineNumbers.Font:= TemplateEditor.LineNumbers.Font;

  //colors array
  Ed.Font.Color:= C[0];
  Ed.Color:= C[1];
  Ed.DefaultStyles.CurrentLine.BgColor:= C[2];
  Ed.LineNumbers.Font.Color:= C[3];
  Ed.LineNumbers.UnderColor:= C[3];
  Ed.Gutter.Bands[1].Color:= C[4];
  Ed.Gutter.Bands[0].Color:= C[4];
  Ed.CollapseBreakColor:= C[5];
  Ed.Gutter.CollapsePen.Color:= C[6];
  Ed.Gutter.Bands[3].Color:= C[7];
  //Ed.Gutter.SeparatorColor:= C[8];
  Ed.DefaultStyles.SelectioMark.Font.Color:= C[9];
  Ed.DefaultStyles.SelectioMark.BgColor:= C[10];
  Ed.RightMarginColor:= C[11];
  Ed.HintProps.Font.Color:= C[12];
  Ed.HintProps.Color:= C[13];
  Ed.NonPrinted.Color:= C[14];
  Ed.StaplePen.Color:= C[15];
  //Ed.Gutter.Bands[1].GradientRight:= C[16];
  //Ed.Gutter.Bands[2].Color:= C[16];
  Tree.Font.Color:= C[17];
  Tree.Color:= C[18];
  Ed.HorzRuler.Font.Color:= C[19];
  Ed.HorzRuler.Color:= C[20];
  opColorTabBgPassive:= C[21];
  opColorTabBgActive:= C[22];
  opColorTabBgActive2:= C[23];
  Ed.DefaultStyles.SearchMark.Font.Color:= C[24];
  Ed.DefaultStyles.SearchMark.BgColor:= C[25];
  ListOut.Font.Color:= C[26];
  ListOut.Color:= C[27];
  TreeFind.Font.Color:= ListOut.Font.Color;
  TreeFind.Color:= ListOut.Color;
  opColorOutSelText:= C[28];
  opColorOutSelBk:= C[29];
  opColorOutRedText:= C[30];
  opColorOutRedSelText:= C[31];
  opColorOutHi:= C[32];
  //opColorBracket:= C[33];
  //opColorBracketBg:= C[34];
  opColorLink:= C[35];
  //opColorSplitViews:= C[36];
  Ed.DefaultStyles.CurrentLine.Font.Color:= C[37];
  Ed.LineStateDisplay.ModifiedColor:= C[38];
  Ed.LineStateDisplay.NewColor:= C[39];
  Ed.LineStateDisplay.SavedColor:= C[40];
  Ed.LineStateDisplay.UnchangedColor:= C[41];
  opColorTabText:= C[42];
  //opColorTabFontUnsav:= C[43];
  opColorBkmk:= C[44];
  opColorMinimapSel:= C[45];
  Ed.DefaultStyles.CollapseMark.Font.Color:= C[46];
  //opColorSplitSlave:= C[47];
  Ed.Gutter.Bands[2].Color:= C[48];
  opColorNonPrintedBG:= C[49];
  Ed.DefaultStyles.CollapseMark.BgColor:= C[50];
  Ed.SyncEditing.SyncRangeStyle.BgColor:= C[51];
  opColorMicromapMarks:= C[52];
  opColorCaretsGutter:= C[53];
  opColorAcpText:= C[54];
  opColorAcpBg:= C[55];
  opColorTabBgPassiveOver:= C[56];
  opColorTabBorderActive:= C[57];
  opColorTabBorderPassive:= C[58];
  opColorTabTextMod:= C[59];
  opColorFtpBlue:= C[60];
  opColorFtpGreen:= C[61];
  opColorFtpRed:= C[62];
  opColorMicromapBG:= C[63];
  opColorMicromapMisspelled:= C[64];
  opColorAcpPrefix:= C[65];
  opColorAcpHintText:= C[66];
  opColorAcpHintText2:= C[67];

  //options
  Ed.Options:= TemplateEditor.Options;
  Ed.OptionsEx:= TemplateEditor.OptionsEx;
  Ed.UndoLimit:= TemplateEditor.UndoLimit;

  Ed.Caret.Insert.BlinkTime:= TemplateEditor.Caret.Insert.BlinkTime;
  Ed.Caret.Overwrite.BlinkTime:= TemplateEditor.Caret.Insert.BlinkTime;
end;

procedure TfmMain.DoColorsArrayInit(var C: TSynColors);
begin
  opColorAcpBg:= clWindow;
  opColorAcpText:= clBlack;
  opColorAcpPrefix:= clPurple;
  opColorAcpHintText:= clBlack;
  opColorAcpHintText2:= clGray;

  opColorCaretsGutter:= clLtGray;
  opColorBkmk:= RGB(200, 240, 200);
  opColorNonPrintedBG:= clSilver;

  opColorTabText:= clBlack;
  opColorTabTextMod:= $a00000;
  opColorTabBgActive:= $f0f0f0;
  opColorTabBgActive2:= opColorTabBgActive;
  opColorTabBgPassive:= $d8d8d8;
  opColorTabBgPassiveOver:= clLtGray;
  opColorTabBorderActive:= clLtGray;
  opColorTabBorderPassive:= clLtGray;

  opColorOutSelBk:= clDkGray;
  opColorOutSelText:= clHighlighttext;
  opColorOutRedText:= clNavy;
  opColorOutRedSelText:= clYellow;
  opColorOutHi:= clSkyBlue;

  opColorLink:= clBlue;
  opColorMinimapSel:= clSkyBlue;
  opColorMicromapBG:= $e0e0e0;
  opColorMicromapMarks:= clGreen;
  opColorMicromapMisspelled:= clRed;

  opColorFtpBlue:= RGB(0, 0, 200);
  opColorFtpGreen:= RGB(0, 200, 0);
  opColorFtpRed:= clRed;

  C[0]:= TemplateEditor.Font.Color;
  C[1]:= TemplateEditor.Color;
  C[2]:= TemplateEditor.DefaultStyles.CurrentLine.BgColor;
  C[3]:= TemplateEditor.LineNumbers.Font.Color;
  C[4]:= TemplateEditor.Gutter.Bands[1].Color;
  C[5]:= TemplateEditor.CollapseBreakColor;
  C[6]:= TemplateEditor.Gutter.CollapsePen.Color;
  C[7]:= TemplateEditor.Gutter.Bands[3].Color;
  //C[8]:= TemplateEditor.Gutter.SeparatorColor;
  C[9]:= TemplateEditor.DefaultStyles.SelectioMark.Font.Color;
  C[10]:= TemplateEditor.DefaultStyles.SelectioMark.BgColor;
  C[11]:= TemplateEditor.RightMarginColor;
  C[12]:= TemplateEditor.HintProps.Font.Color;
  C[13]:= TemplateEditor.HintProps.Color;
  C[14]:= TemplateEditor.NonPrinted.Color;
  C[15]:= TemplateEditor.StaplePen.Color;
  //C[16]:= TemplateEditor.Gutter.Bands[1].GradientRight;
  C[17]:= Tree.Font.Color;
  C[18]:= Tree.Color;
  C[19]:= TemplateEditor.HorzRuler.Font.Color;
  C[20]:= TemplateEditor.HorzRuler.Color;
  C[21]:= opColorTabBgPassive;
  C[22]:= opColorTabBgActive;
  C[23]:= opColorTabBgActive2;
  C[24]:= TemplateEditor.DefaultStyles.SearchMark.Font.Color;
  C[25]:= TemplateEditor.DefaultStyles.SearchMark.BgColor;
  C[26]:= ListOut.Font.Color;
  C[27]:= ListOut.Color;
  C[28]:= opColorOutSelText;
  C[29]:= opColorOutSelBk;
  C[30]:= opColorOutRedText;
  C[31]:= opColorOutRedSelText;
  C[32]:= opColorOutHi;
  //C[33]:= opColorBracket;
  //C[34]:= opColorBracketBg;
  C[35]:= opColorLink;
  //C[36]:= opColorSplitViews;
  C[37]:= TemplateEditor.DefaultStyles.CurrentLine.Font.Color;
  C[38]:= TemplateEditor.LineStateDisplay.ModifiedColor;
  C[39]:= TemplateEditor.LineStateDisplay.NewColor;
  C[40]:= TemplateEditor.LineStateDisplay.SavedColor;
  C[41]:= TemplateEditor.LineStateDisplay.UnchangedColor;
  C[42]:= opColorTabText;
  //C[43]:= opColorTabFontUnsav;
  C[44]:= opColorBkmk;
  C[45]:= opColorMinimapSel;
  C[46]:= TemplateEditor.DefaultStyles.CollapseMark.Font.Color;
  //C[47]:= opColorSplitSlave;
  C[48]:= TemplateEditor.Gutter.Bands[2].Color;
  C[49]:= opColorNonPrintedBG;
  C[50]:= TemplateEditor.DefaultStyles.CollapseMark.BgColor;
  C[51]:= TemplateEditor.SyncEditing.SyncRangeStyle.BgColor;
  C[52]:= opColorMicromapMarks;
  C[53]:= opColorCaretsGutter;
  C[54]:= opColorAcpText;
  C[55]:= opColorAcpBg;
  C[56]:= opColorTabBgPassiveOver;
  C[57]:= opColorTabBorderActive;
  C[58]:= opColorTabBorderPassive;
  C[59]:= opColorTabTextMod;
  C[60]:= opColorFtpBlue;
  C[61]:= opColorFtpGreen;
  C[62]:= opColorFtpRed;
  C[63]:= opColorMicromapBG;
  C[64]:= opColorMicromapMisspelled;
  C[65]:= opColorAcpPrefix;
  C[66]:= opColorAcpHintText;
  C[67]:= opColorAcpHintText2;
end;

procedure TfmMain.DoHandleQuickSearchEscape;
begin
  FocusEditor;
end;

function TfmMain.DoHandleEscapeActions: boolean;
begin
  Result:= false;
  if GetKeyState(vk_escape)<0 then
  begin
    if edQS.Focused then
    begin
      DoHandleQuickSearchEscape;
      Result:= true;
    end
    else
    if ecACP.Visible then
    begin
      ecACP.CloseUp(false);
      Result:= true;
    end;
  end;
end;


procedure TfmMain.TBXMRUListItemFNewClick(Sender: TObject;
  const Filename: WideString);
begin
  DoNewDoc(Filename);
end;

procedure TfmMain.StatusResize(Sender: TObject);
begin
  Status.InvalidateBackground();
end;

procedure TfmMain.DoEnumFavs(L: TTntStringList);
var
  fn: string;
begin
  L.Clear;
  fn:= SynFavIni;
  if IsFileExist(fn) then
    L.LoadFromFile(fn);
end;

function TfmMain.DoGetFavList: Widestring;
var
  L: TTntStringList;
begin
  Result:= '';
  L:= TTntStringList.Create;
  try
    DoEnumFavs(L);
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;

procedure TfmMain.acSetupLexerLibExecute(Sender: TObject);
begin
  DoLexerLibraryDialog(SyntaxManager, ImgListTree);
  SaveLexLibFilename;
  DoConfirmSaveLexLib;
end;

procedure TfmMain.TbxItemTabReloadClick(Sender: TObject);
begin
  DoFrameReloadWrapper(FClickedFrame);
end;

function TfmMain.SynHiddenOption(const Id: string; Default: integer): Integer;
begin
  with TIniFile.Create(SynIni) do
  try
    Result:= ReadInteger('Setup', Id, Default);
  finally
    Free
  end;
end;

(*
procedure TfmMain.DoRememberTempFile(const fn: Widestring);
begin
  if FTempFilenames.IndexOf(fn)<0 then
    FTempFilenames.Add(fn);
end;

procedure TfmMain.DoDeleteTempFiles;
var
  i: Integer;
begin
  for i:= FTempFilenames.Count-1 downto 0 do
  begin
    FDelete(FTempFilenames[i]);
    FTempFilenames.Delete(i);
  end;
end;
*)

procedure TfmMain.TBXSubmenuViewToolbarsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  TbxItemTFile.Checked:= tbFile.Visible;
  TbxItemTEdit.Checked:= tbEdit.Visible;
  TbxItemTView.Checked:= tbView.Visible;
  TbxItemTQs.Checked:= tbQs.Visible;

  TbxItemTUser1.Checked:= tbUser1.Visible;
  TbxItemTUser2.Checked:= tbUser2.Visible;
  TbxItemTUser3.Checked:= tbUser3.Visible;
  TbxItemTUser1.Visible:= tbUser1.Items.Count>0;
  TbxItemTUser2.Visible:= tbUser2.Items.Count>0;
  TbxItemTUser3.Visible:= tbUser3.Items.Count>0;
end;

procedure TfmMain.DoCopySearchMarks(Ed: TSyntaxMemo);
var
  L: TTntStringList;
begin
  L:= TTntStringList.Create;
  try
    EditorSearchMarksToList(Ed, L);
    if L.Count=0 then
      MsgBeep
    else
      TntClipboard.AsWideText:= L.Text;
  finally
    FreeAndNil(L)
  end;
end;

procedure TfmMain.TimerMinimapTimer(Sender: TObject);
begin
  TimerMinimap.Enabled:= false;
  SyncMapPos;
end;


procedure TfmMain.DoTextConverter(Ed: TSyntaxMemo; const fn: Widestring; ToBack: boolean);
var
  SFrom, STo: Widestring;
  NStart, NLen: Integer;
  ToAll: boolean;
begin
  if Ed.ReadOnly then Exit;
  if not IsFileExist(fn) then
    begin MsgNoFile(fn); Exit end;

  ToAll:= Ed.SelLength=0;
  if ToAll then
  begin
    SFrom:= Ed.Lines.FText;
    NStart:= 0;
    NLen:= Length(SFrom);
  end
  else
  begin
    SFrom:= Ed.SelText;
    NStart:= Ed.SelStart;
    NLen:= Ed.SelLength;
  end;

  STo:= SDecodeUsingFileTable(SFrom, fn, ToBack);
  if STo=SFrom then
    begin MsgDoneLines(0); MsgBeep; Exit end;

  Ed.ReplaceText(NStart, NLen, STo);
  if not ToAll then
    Ed.SetSelection(NStart, Length(STo));
end;

procedure TfmMain.ecEncodeHtmlCharsExecute(Sender: TObject);
begin
  DoTextConverter(CurrentEditor, SynConverterFilename(cConverterHtml1), false);
end;

procedure TfmMain.ecEncodeHtmlChars2Execute(Sender: TObject);
begin
  DoTextConverter(CurrentEditor, SynConverterFilename(cConverterHtml2), false);
end;

procedure TfmMain.ProjGotoFile(Sender: TObject);
begin
  ecProjectList.Execute;
end;

procedure TfmMain.TbxItemMenuXClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FileClose);
end;

procedure TfmMain.TbxItemMenuXXClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_FileCloseAll);
end;

function TfmMain.IsWordChar(ch: WideChar): boolean;
begin
  //count '#' as wordchar for HTML color codes
  Result:= ecStrUtils.IsWordChar(ch) or (Pos(ch, opWordChars+'#')>0);
end;

procedure TfmMain.DoFavoriteProjects;
begin
  DoFavoritesDialog(cSynFavTabProjects);
end;

procedure TfmMain.ShowProj;
begin
  if not (Assigned(fmProj) and fmProj.TreeProj.Focused) then
  begin
    //focus project
    ecToggleFocusProject.Execute;
    //repaint project
    plTree.Invalidate;
    if Assigned(fmProj) then
      fmProj.TreeProj.Invalidate;
    Application.ProcessMessages;
  end;
end;

procedure TfmMain.DoOpenProject;
begin
  ShowProj;
  if Assigned(fmProj) then
    fmProj.DoOpenProject;
end;

procedure TfmMain.DoAddFileToProject;
begin
  ShowProj;
  if Assigned(fmProj) then
    fmProj.DoAddEditorFiles(false);
end;

procedure TfmMain.DoAddFilesToProject;
begin
  ShowProj;
  if Assigned(fmProj) then
    fmProj.DoAddEditorFiles(true);
end;

procedure TfmMain.DoNewProject;
begin
  ShowProj;
  if Assigned(fmProj) then
    fmProj.DoNewProject;
end;

procedure TfmMain.DoSaveProject;
begin
  if Assigned(fmProj) then
    fmProj.DoSaveProject;
end;

procedure TfmMain.DoUpdateProject;
begin
  if Assigned(fmProj) then
    fmProj.DoUpdateProject;
end;

procedure TfmMain.TBXItemProjOpenClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_OpenProject);
end;

procedure TfmMain.TBXItemProjAddFileClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddFileToProject);
end;

procedure TfmMain.TBXItemProjAddAllFilesClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddFilesToProject);
end;

procedure TfmMain.TBXSubmenuItemProjRecentsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  Ini: TIniFile;
  i: Integer;
begin
  Ini:= TIniFile.Create(SynHistoryIni);
  try
    LoadMruList(SynMruProjects, Ini, 'MRU_Proj', opSaveFileCount, opMruCheck);
  finally
    FreeAndNil(Ini);
  end;

  with TBXMRUListItem_Projects do
  begin
    Clear;
    for i:= SynMruProjects.Items.Count-1 downto 0 do
      MRUAdd(SynMruProjects.Items[i]);
  end;
end;

procedure TfmMain.TBXMRUListItem_ProjectsClick(Sender: TObject;
  const Filename: WideString);
begin
  if IsFileExist(Filename) then
    DoOpenProject(Filename)
  else
  begin
    MsgNoFile(Filename);
    SynMruProjects.DeleteItem(Filename);
  end;
end;

procedure TfmMain.TBXItemProjRecentClearClick(Sender: TObject);
begin
  SynMruProjects.Items.Clear;
  with TIniFile.Create(SynHistoryIni) do
  try
    EraseSection('MRU_Proj');
  finally
    Free;
  end;
end;

procedure TfmMain.TBXItemProjGotoClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_ProjectList);
end;

procedure TfmMain.TBXItemProjNewClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_NewProject);
end;

procedure TfmMain.SynCaretPosChanged(Sender: TObject);
var
  Ed: TSyntaxMemo;
  NeedDraw: boolean;
begin
  Ed:= CurrentEditor;
  if Ed=nil then Exit;
  NeedDraw:= false;

  ATSyntMemo.TSyntaxMemo(Ed).DoUpdateMargins;

  if Ed.BracketsHilited then
  begin
    Ed.BracketsHilited:= false;
    Ed.SearchMarks.Clear;
    NeedDraw:= true;
  end;

  if NeedDraw then
    Ed.Invalidate;

  DoSmartHiliteOnClick;

  UpdateLexer;
  UpdateStatusBar;
end;

procedure TfmMain.DoSmartHiliteOnClick;
var
  Ed: TSyntaxMemo;
  NPos: Integer;
  S: Widestring;
  F: TEditorFrame;
  MCarets: boolean;
begin
  Ed:= CurrentEditor;

  //don't allow to work for many carets
  F:= FrameOfEditor(Ed);
  MCarets:= (F<>nil) and (F.CaretsCount>1);
  if MCarets and opHiliteSmartOnClick then
  begin
    DoClearSearchMarks(Ed);
    Exit
  end;

  if opHiliteSmartOnClick then
  begin
    DoClearSearchMarks(Ed);
    NPos:= Ed.CaretStrPos;
    if not Ed.HaveSelection then
	  if IsWordChar(Ed.Lines.Chars[NPos+1]) then
	  begin
      S:= Ed.WordAtPos(Ed.CaretPos);
      if S<>'' then
        DoFind_MarkAll(S);
	  end;
  end;
end;

procedure TfmMain.DoPasteAndSelect;
begin
  EditorPasteAndSelect(CurrentEditor);
end;

procedure TfmMain.DoCopyURL;
var
  S: Widestring;
begin
  S:= CurrentFrame.SUrlAt(CurrentEditor.CaretPos);
  if S<>'' then
    Clipboard.AsText:= S
  else
    MsgBeep;
end;

procedure TfmMain.DoOpenURL;
const
  cMaxLen = 1000;
var
  S: Widestring;
begin
  S:= CurrentFrame.SUrlAt(CurrentEditor.CaretPos);
  if S<>'' then
    FOpenURL(S, Handle)
  else
  begin
    S:= Trim(EditorGetSelTextLimited(CurrentEditor, cMaxLen));
    if S='' then
      begin MsgBeep; Exit end;
    if Pos('://', S)=0 then
      S:= 'http://' + S;
    FOpenURL(S, Handle);
  end;
end;

//updates TbxItemCtxAddColor.Enabled
function TfmMain.UpdateCurrentColorCode(var AColor: Integer): boolean;
var
  Ed: TSyntaxMemo;
  s: Widestring;
  nStart, nEnd: Integer;
begin
  Ed:= CurrentEditor;
  s:= Ed.SelText;
  if (s<>'') and IsStringRegex(s, cRegexColorCode) then
  begin
    Delete(s, 1, 1);
    Result:= IsHexColorString(s);
    if Result then
      AColor:= SHtmlCodeToColor(s);
  end
  else
  if (s<>'') and IsStringRegex(s, cRegexColorName) then
  begin
    s:= SGetColorNameValue(s);
    Result:= s<>'';
    if Result then
    begin
      Delete(s, 1, 1);
      Result:= IsHexColorString(s);
      if Result then
        AColor:= SHtmlCodeToColor(s);
    end;
  end
  else
  begin
    EditorGetColorCodeRange(Ed, nStart, nEnd, AColor);
    Result:= nEnd>nStart;
  end;
end;

procedure TfmMain.DoAddCurrentColorCodeToRecents;
var
  nColor: Integer;
begin
  if UpdateCurrentColorCode(nColor) then
    DoAddRecentColor(nColor)
  else
    MsgBeep;
end;

procedure TfmMain.DoSaveFolding;
var
  Ed: TSyntaxMemo;
  F: TEditorFrame;
  SFold: string;
begin
  Ed:= CurrentEditor;
  F:= CurrentFrame;

  if F.FileName='' then
  begin
    DoHint('Cannot save folding for unnamed tab');
    MsgBeep;
    Exit
  end;

  if Ed.DisableFolding then
  begin
    DoHint('Cannot save folding when it''s disabled');
    MsgBeep;
    Exit
  end;

  SFold:= EditorGetCollapsedRanges(Ed);
  if SFold='' then
  begin
    DoHint('Cannot save empty folding state');
    MsgBeep;
    Exit
  end;

  with TIniFile.Create(SynFoldStatesIni) do
  try
    WriteString('Fold', UTF8Encode(F.FileName), SFold);
  finally
    Free
  end;

  DoHint('Folding saved to file');
end;

procedure TfmMain.DoLoadFolding;
var
  Ed: TSyntaxMemo;
  F: TEditorFrame;
  SFold: string;
begin
  Ed:= CurrentEditor;
  F:= CurrentFrame;

  if F.FileName='' then
  begin
    DoHint('Cannot load folding for unnamed tab');
    MsgBeep;
    Exit
  end;

  if Ed.DisableFolding then
    ecFolding.Execute;

  with TIniFile.Create(SynFoldStatesIni) do
  try
    SFold:= ReadString('Fold', UTF8Encode(F.FileName), '');
  finally
    Free
  end;

  if SFold='' then
  begin
    DoHint('Cannot load empty folding state');
    MsgBeep;
    Exit
  end;

  EditorSetCollapsedRanges(Ed, SFold);
  DoHint('Folding loaded from file');
end;

procedure TfmMain.PluginACPShow(Sender: TObject);
begin
  if opAcpUseSingle then
    with PluginACP do
      if ListBox.Items.Count=1 then
      begin
        CloseUp(true);
        Exit
      end;
end;

function TfmMain.FrameForFilename(const fn: Widestring): TEditorFrame;
var
  i: Integer;
  F: TEditorFrame;
begin
  Result:= nil;
  for i:= 0 to FrameAllCount-1 do
  begin
    F:= FramesAll[i];
    if F.IsTheFile(fn) then
    begin
      Result:= F;
      Exit;
    end;
  end;
end;

procedure TfmMain.DoOpenLastClosedFile;
var
  fn: Widestring;
  i: Integer;
begin
  fn:= '';
  with SynMruFiles do
    for i:= 0 to Items.Count-1 do
      if FrameForFilename(Items[i])=nil then
      begin
        fn:= Items[i];
        Break
      end;

  if fn<>'' then
  begin
    if IsFileExist(fn) then
      DoOpenFile(fn)
    else
      MsgNoFile(fn);
  end
  else
    MsgBeep();
end;

procedure TfmMain.ProjPreview(Sender: TObject; const AFilename: Widestring;
  AToggle: boolean);
begin
  DoPreviewFile(AFilename, AToggle, 0, 0, 0);
end;

procedure TfmMain.DoPreviewFile(const AFilename: Widestring;
  AToggle: boolean; ALineNum, AColNum, ALen: Integer);
var
  Ed: TSyntaxMemo;
begin
  FProjPreviewFilename:= '';
  if Assigned(FProjPreview) then
    with FProjPreview do
    begin
      if AToggle then
        Visible:= not Visible;

      FProjPreview.Caption:= DKLangConstW('MPre')+': '+WideExtractFilename(AFilename);
      if Assigned(FProjPreviewButton) then
        FProjPreviewButton.Caption:= '[ ' + DKLangConstW('MPreButton') + ' ]';

      Ed:= FProjPreviewEditor;
      Ed.Lines.Clear;
      if not Visible then Exit;
      if CurrentEditor=nil then Exit;
      if not IsFileExist(AFilename) then Exit;

      //sync preview-editor options
      //don't use Ed.Assign(CurrentEditor), it gives error on exit
      Ed.Font.Assign(CurrentEditor.Font);
      Ed.Color:= CurrentEditor.Color;
      Ed.RightMargin:= CurrentEditor.RightMargin;
      Ed.Options:= CurrentEditor.Options;
      Ed.OptionsEx:= CurrentEditor.OptionsEx;
      Ed.StaplesEnabled:= CurrentEditor.StaplesEnabled;
      Ed.StapleOffset:= CurrentEditor.StapleOffset;
      Ed.StaplePen.Assign(CurrentEditor.StaplePen);

      //detect UTF8-no-bom
      if IsFileUTF8NoBOM(AFilename) then
      begin
        Ed.Lines.SkipSignature:= true;
        Ed.Lines.TextCoding:= tcUTF8;
        Ed.Lines.CodePage:= 0;
      end;

      Screen.Cursor:= crHourGlass;
      try
        Ed.LoadFromFile(AFilename);
        Ed.TopLine:= ALineNum - opFindOffsetTop;
        Ed.SetSelection(Ed.CaretPosToStrPos(Point(AColNum, ALineNum)), ALen);
        Ed.SyntaxAnalyzer:= DoFindLexerForFilename(SyntaxManager, AFilename);
      finally
        Screen.Cursor:= crDefault;
      end;

      FProjPreviewFilename:= AFilename;
    end;
end;

procedure TfmMain.ProjPreviewClose(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini:= TIniFile.Create(SynIni);
  try
    if Assigned(FProjPreview) then
      SavePanelProp(FProjPreview, Ini, 'Pre');
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfmMain.FocusProj;
begin
  if Assigned(fmProj) and fmProj.TreeProj.CanFocus then
    fmProj.TreeProj.SetFocus;
end;

procedure TfmMain.ProjPreviewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=vk_escape) and (Shift=[]) then
  begin
    FProjPreview.Visible:= false;
    FocusProj;
    Key:= 0;
    Exit
  end;
  {
  if (Key=vk_space) and (Shift=[]) then
  begin
    ProjPreviewButtonClick(Self);
    Key:= 0;
    Exit
  end;
  }
end;

{
function TfmMain.IsProjPreviewFocused: boolean;
begin
  Result:=
    Assigned(FProjPreview) and FProjPreview.Visible and
    Assigned(FProjPreviewEditor) and FProjPreviewEditor.Focused;
end;
}

procedure TfmMain.ProjPreviewButtonClick(Sender: TObject);
var
  fn: Widestring;
begin
  fn:= FProjPreviewFilename;
  if (fn<>'') and IsFileExist(fn) then
  begin
    FProjPreview.Hide;
    DoOpenFile(fn);
    FocusEditor;

    CurrentEditor.CaretPos:= FProjPreviewEditor.CaretPos;
    EditorCenterPos(CurrentEditor, true, opFindOffsetTop);
  end
  else
    MsgBeep;
end;

function TfmMain.DoCheckCommandLineTwo: boolean;
var
  i: Integer;
  S, SName1, SName2,
  SLine1, SLine2, SCol1, SCol2, SDelta: Widestring;
  NLine1, NLine2, NCol1, NCol2, NDelta: Integer;
  IsParamTwo, IsParamCmp: boolean;
  F: TEditorFrame;
begin
  Result:= false;
  for i:= 1 to WideParamCount do
  begin
    S:= WideParamStr(i);
    IsParamTwo:= SBegin(S, cSynParamTwo);
    IsParamCmp:= SBegin(S, cSynParamCmp);

    if IsParamTwo then
    begin
      Result:= true;
      SDeleteToW(S, '=');

      SName1:= SGetItem(S, '|');
      SName2:= SGetItem(S, '|');
      SLine1:= SGetItem(S, '|');
      SLine2:= SGetItem(S, '|');
      SCol1:= SGetItem(S, '|');
      SCol2:= SGetItem(S, '|');
      SDelta:= SGetItem(S, '|');

      NLine1:= StrToIntDef(SLine1, 1)-1;
      NLine2:= StrToIntDef(SLine2, 1)-1;
      NCol1:= StrToIntDef(SCol1, 1)-1;
      NCol2:= StrToIntDef(SCol2, 1)-1;
      NDelta:= StrToIntDef(SDelta, 0);

      if not IsFileExist(SName1) then
        begin MsgNoFile(SName1); Exit end;
      //if not IsFileExist(SName2) then
      //  begin MsgNoFile(SName2); Exit end;

      //close all tabs
      if not DoCloseTabs(tabCloseAll, false) then Exit;
      Groups.Mode:= gmOne;

      //open 1st file
      F:= DoOpenFile(SName1);
      F.EditorMaster.TopLine:= NLine1 - NDelta;
      F.EditorMaster.CaretPos:= Point(NCol1, NLine1);

      //open 2nd file
      if IsFileExist(SName2) then
      begin
        F:= DoOpenFile(SName2);
        F.EditorMaster.TopLine:= NLine2 - NDelta;
        F.EditorMaster.CaretPos:= Point(NCol2, NLine2);
      end
      else
      begin
        acNewTab.Execute;
        F:= CurrentFrame;
        F.EditorMaster.Lines.Text:= DKLangConstW('MNFound')+' '+SName2;
        F.Modified:= true;
      end;

      //move last file to group2
      with Groups do
      begin
        Mode:= gm2Horz;
        MoveTab(Pages1, Pages1.Tabs.TabCount-1, Pages2, -1, false);
      end;
    end;

    if IsParamCmp then
    begin
      Result:= true;
      SDeleteToW(S, '=');

      SName1:= SGetItem(S, '|');
      SName2:= SGetItem(S, '|');

      if not IsFileExist(SName1) then
        begin MsgNoFile(SName1); Exit end;
      if not IsFileExist(SName2) then
        begin MsgNoFile(SName2); Exit end;

      //close all tabs
      if not DoCloseTabs(tabCloseAll, false) then Exit;
      Groups.Mode:= gmOne;

      DoPyEvent(CurrentEditor, cSynEventOnCompare,
        [SWideStringToPythonString(SName1),
         SWideStringToPythonString(SName2)]);
    end;
  end;
end;


procedure TfmMain.plTreeDockChanged(Sender: TObject);
begin
  //fix splitter position, it may be too right after
  //docking the tree panel to left
  SplitterLeft.Left:= 10;
end;

function TfmMain.CurrentProjectSessionFN: string;
begin
  Result:= CurrentProjectFN;
  if Result<>'' then
    Result:= ChangeFileExt(Result, '.'+cSynSessionExt);
end;

procedure TfmMain.DoSaveProjectSession;
var
  fn: string;
begin
  fn:= CurrentProjectSessionFN;
  if fn<>'' then
  begin
    DoSaveSessionToFile(fn);
    SynMruSessions.AddItem(fn);
  end;
end;

procedure TfmMain.DoOpenProjectSession;
var
  fn: string;
begin
  fn:= CurrentProjectSessionFN;
  if fn<>'' then
  begin
    DoOpenSession(fn);
  end;
end;

procedure TfmMain.ProjOpen;
var
  fn: string;
begin
  if not opHistSessionProjLoad then Exit;

  fn:= CurrentProjectSessionFN;
  if (fn<>'') and IsFileExist(fn) then
  begin
    if FProjectIniting then
    begin
      //if we are called from LoadIni, lexer lib isn't yet loaded,
      //so bad to load session yet. So we save session name to SynProjectSessionFN
      //to open later, in TfmEx.FormShow
      //MsgInfo('open sess delayed '+fn, Handle);
      SynProjectSessionFN:= fn;
    end
    else
    begin
      //usual project opening, load session now
      //MsgInfo('open sess '+fn, Handle);
      DoOpenSession(fn);
    end;
  end;
end;

procedure TfmMain.DoCloseTabsOnProjectClosingIfNeeded;
begin
  //make sure we don't close all tabs during form OnClose (this gives AV)
  if not FProjectFreeing then
    if opHistProjectCloseTabs then
      acCloseAll.Execute;
end;

procedure TfmMain.ProjClose;
var
  fn: string;
begin
  if not opHistSessionProjSave then
  begin
    DoCloseTabsOnProjectClosingIfNeeded;
    Exit;
  end;

  fn:= CurrentProjectSessionFN;
  if fn<>'' then
  begin
    //MsgInfo('save sess '+fn, Handle);
    DoSaveSessionToFile(fn);
    DoCloseSession(false);
    DoCloseTabsOnProjectClosingIfNeeded;
  end;
end;

procedure TfmMain.DoProjectRenameFile(const fn, fn_new: Widestring);
begin
  if Assigned(fmProj) then
    fmProj.DoRenameFile(fn, fn_new);
end;

procedure TfmMain.ProjKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  sh: TShortcut;
begin
  sh:= Shortcut(Key, Shift);
  if sh=0 then Exit;

  //configured shortcut for "Go to project file"
  if IsShortcutOfCmd(sh, sm_ProjectList) then
  begin
    ProjGotoFile(Self);
    Key:= 0;
    Exit
  end;
  //F2 - rename folder
  if (Key=VK_F2) and (Shift=[]) then
  begin
    fmProj.DoRename;
    Key:= 0;
    Exit
  end;
  //F5 - refresh project
  if (Key=VK_F5) and (Shift=[]) then
  begin
    fmProj.DoRefresh;
    Key:= 0;
    Exit
  end;
  //Del
  if (Key=VK_delete) and (Shift=[]) and not fmProj.TreeProj.IsEditing then
  begin
    fmProj.DoRemove;
    Key:= 0;
    Exit
  end;
  //Ins
  if (Key=VK_insert) and (Shift=[]) and not fmProj.TreeProj.IsEditing then
  begin
    fmProj.DoAddFiles;
    Key:= 0;
    Exit
  end;
  //Space - toggle preview pane
  if (Key=VK_space) and (Shift=[]) and not fmProj.TreeProj.IsEditing then
  begin
    fmProj.DoPreview(true);
    Key:= 0;
    Exit
  end;
  //Ctrl+Space - toggle "show paths"
  if (Key=VK_space) and (Shift=[ssCtrl]) then
  begin
    fmProj.DoToggleShowPaths;
    Key:= 0;
    Exit
  end;
  //Enter - open selected files
  if (Key=vk_return) and (Shift=[]) then
  begin
    if not fmProj.IsDirSelected then
      fmProj.DoOpenFiles;
    Key:= 0;
    Exit
  end;

  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.DoZoomEditorInc(AInc: boolean);
const
  cDelta = 10; //increase by N%
begin
  DoZoomEditor(CurrentEditor.Zoom + cDelta * IfThen(AInc, 1, -1));
end;

procedure TfmMain.TbxItemProjSaveClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SaveProject);
end;


function TfmMain.FrameGetPropertiesString(F: TEditorFrame): string;
  //
  procedure Add(var res: string; const id, val: string);
  begin
    res:= res + id + '=' + val + ';';
  end;
  //
  procedure AddEd(var res: string; Ed: TSyntaxMemo; const id: string);
  var
    p: TPoint;
  begin
    p:= Ed.CaretPos;
    Add(res, cFramePropPos+id, Format('%d,%d,%d,', [p.X, p.Y, Ed.TopLine]));
    if cSynHistoryCaret in opSaveEditor then
      Add(res, cFramePropSel+id, EditorGetSelCoordAsString(Ed));
    if cSynHistoryFolding in opSaveEditor then
      Add(res, cFramePropFold+id, EditorGetCollapsedRanges(Ed));
  end;
  //
begin
  Result:= '';
  if F.FileName<>'' then
  begin
    Result:= Utf8Encode(F.FileName) + ';';

    if cSynHistoryEnc in opSaveEditor then
      Add(Result, cFramePropEnc, IntToStr(DoGetFrameEncoding(F)));
    Add(Result, cFramePropLexer, F.CurrentLexer);
    Add(Result, cFramePropWrap, IntToStr(Ord(F.EditorMaster.WordWrap)));
    Add(Result, cFramePropSplit, IntToStr(Ord(F.SplitHorz)) + ',' + IntToStr(Round(F.SplitPos)));

    AddEd(Result, F.EditorMaster, '1');
    AddEd(Result, F.EditorSlave, '2');

    if cSynHistoryBkmk in opSaveEditor then
      Add(Result, cFramePropBk, EditorGetBookmarksAsString(F.EditorMaster));
    if F.TabColor<>clNone then
      Add(Result, cFramePropColor, IntToStr(F.TabColor));
    if F.EditorMaster.Zoom<>100 then
      Add(Result, cFramePropZoom, IntToStr(F.EditorMaster.Zoom));
  end;
end;

function TfmMain.IsFramePropertiesStringForFilename(const fn: Widestring; const Str: string): boolean;
var
  N: Integer;
  fnData: Widestring;
begin
  Result:= false;
  if fn='' then Exit;

  N:= Pos(';', Str);
  if N=0 then Exit;
  fnData:= Utf8Decode(Copy(Str, 1, N-1));

  Result:= WStrIComp(PWChar(fnData), PWChar(fn)) = 0;
    //WideUpperCase(fnData)=WideUpperCase(fn);
end;

procedure TfmMain.FrameSetPropertiesString(F: TEditorFrame; const Str: string; EncodingOnly: boolean);
var
  S, SItem, SId, SVal: Widestring;
  Ed: TSyntaxMemo;
  pnt: TPoint;
  EdIndex, NVal{, NVal2}: Integer;
  //Analyzer: TSyntAnalyzer;
begin
  (*
  Analyzer:= SyntaxManager.AnalyzerForFile(F.FileName);
  F.EditorMaster.TextSource.SyntaxAnalyzer:= nil;
  *)

  F.EditorMaster.BeginUpdate;
  F.EditorSlave.BeginUpdate;
  try
    S:= Str;
    SItem:= SGetItem(S, ';'); //delete filename
    repeat
      SItem:= SGetItem(S, ';');
      if SItem='' then Break;
      SId:= SGetItem(SItem, '=');
      SVal:= SItem;
      if Length(SId)<2 then Continue;
      EdIndex:= StrToIntDef(SId[Length(SId)], 0);

      if EdIndex=2 then
        Ed:= F.EditorSlave
      else
        Ed:= F.EditorMaster;

      if IsDigitChar(SId[Length(SId)]) then
        SetLength(SId, Length(SId)-1);

      if EncodingOnly then
      begin
        //apply encoding field, don't touch others
        if (SId=cFramePropEnc) and (cSynHistoryEnc in opSaveEditor) then
          begin
            NVal:= StrToIntDef(SVal, 0);
            DoSetFrameEncoding(F, NVal);
          end
        else
          Continue;
      end;

      if SId=cFramePropLexer then
        begin
          //Analyzer:= SyntaxManager.FindAnalyzer(SVal);
          F.EditorMaster.TextSource.SyntaxAnalyzer:= SyntaxManager.FindAnalyzer(SVal);
        end
      else
      if (SId=cFramePropWrap) and (cSynHistoryWrap in opSaveEditor) then
        begin
          F.EditorMaster.WordWrap:= Bool(StrToIntDef(SVal, 0));
          F.EditorSlave.WordWrap:= F.EditorMaster.WordWrap;
        end
      else
      if SId=cFramePropSplit then
        begin
          F.SplitHorz:= Bool(StrToIntDef(SGetItem(SVal), 1));
          F.SplitPos:= StrToIntDef(SGetItem(SVal), 50);
        end
      else
      if (SId=cFramePropBk) and (cSynHistoryBkmk in opSaveEditor) then
        begin
          EditorSetBookmarksAsString(F.EditorMaster, SVal);
          EditorSetBookmarksAsString(F.EditorSlave, SVal);
          UpdateListBookmarks;
        end
      else
      if SId=cFramePropColor then
        begin
          if SVal<>'' then
            DoSetFrameTabColor(F, StrToIntDef(SVal, clNone));
        end
      else
      if SId=cFramePropZoom then
        begin
          F.EditorMaster.Zoom:= StrToIntDef(SVal, 100);
          F.EditorSlave.Zoom:= F.EditorMaster.Zoom;
        end
      else
      if (SId=cFramePropPos) and (cSynHistoryCaret in opSaveEditor) then
        begin
          pnt.X:= StrToIntDef(SGetItem(SVal), 0);
          pnt.Y:= StrToIntDef(SGetItem(SVal), 0);
          Ed.CaretPos:= pnt;
          Ed.TopLine:= StrToIntDef(SGetItem(SVal), 0);
        end
      else
      if (SId=cFramePropSel) and (cSynHistoryCaret in opSaveEditor) then
        begin
          EditorSetSelCoordAsString(Ed, SVal);
        end
      else
      if (SId=cFramePropFold) and (cSynHistoryFolding in opSaveEditor) then
        begin
          //this works, if lexer analisys already finished
          EditorSetCollapsedRanges(Ed, SVal);
          //this works if not
          if EdIndex=1 then
            F.CollapsedString1:= SVal
          else
            F.CollapsedString2:= SVal;
        end;
    until false;
  finally
    F.EditorMaster.EndUpdate;
    F.EditorSlave.EndUpdate;
  end;

  (*
  //now repaint and set lexer - this is long operation (5s on unMain.pas, if caret at end)
  Application.ProcessMessages;
  NVal:= F.EditorMaster.TopLine;
  NVal2:= F.EditorSlave.TopLine;
  if FCanUseLexer(F.FileName) then
    F.EditorMaster.TextSource.SyntaxAnalyzer:= Analyzer;
  F.EditorMaster.TopLine:= NVal;
  F.EditorSlave.TopLine:= NVal2;
  *)
end;

function TfmMain.MsgConfirmOpenSaveSession(
  AFilesCount: Integer;
  const AFileName: string;
  ASaveMode: boolean): boolean;
const
  cMsg: array[boolean] of string = ('zMCfmSessionLoad', 'zMCfmSessionSave');
var
  NOpt: Integer;
  SName, SMsg: Widestring;
begin
  NOpt:= SynHiddenOption('SessionMaxFiles', 0);
  SName:= ChangeFileExt(ExtractFileName(AFileName), '');
  SMsg:= WideFormat(DKLangConstW(cMsg[ASaveMode]), [SName, AFilesCount]);

  if (NOpt=0) or (AFilesCount<=NOpt) then
    Result:= true
  else
    Result:= MsgConfirm(SMsg, Handle);
end;

function TfmMain.DoOpenArchive_HandleLexer(const fn_ini, section: string): boolean;
var
  s_file, dir: string;
  s_links: array[1..cMaxLexerLinksInInf] of string;
  fn_lexer, fn_acp, fn_acp_target: string;
  An, AnLink: TSyntAnalyzer;
  i: Integer;
begin
  dir:= ExtractFileDir(fn_ini);

  with TIniFile.Create(fn_ini) do
  try
    s_file:= ReadString(section, 'file', '');
    for i:= Low(s_links) to High(s_links) do
      s_links[i]:= ReadString(section, 'link'+IntToStr(i), '');
  finally
    Free
  end;

  Result:= s_file<>'';
  if not Result then Exit;

  fn_lexer:= dir + '\' + s_file + '.lcf';
  fn_acp:= dir + '\' + s_file + '.acp';
  fn_acp_target:= GetAcpFN(s_file);

  if FileExists(fn_lexer) then
  begin
    An:= SyntaxManager.FindAnalyzer(s_file);
    if An=nil then
      An:= SyntaxManager.AddAnalyzer;
    An.LoadFromFile(fn_lexer);

    for i:= Low(s_links) to High(s_links) do
      if s_links[i]<>'' then
      begin
        AnLink:= SyntaxManager.FindAnalyzer(s_links[i]);
        if AnLink=nil then
          MsgError('Cannot find linked lexer: '+s_links[i], Handle)
        else
          if An.SubAnalyzers.Count>=i then
            An.SubAnalyzers[i-1].SyntAnalyzer:= AnLink;
      end;

    SaveLexLib;
    FDelete(fn_lexer);
  end;

  if FileExists(fn_acp) then
  begin
    FFileCopy(fn_acp, fn_acp_target);
    FDelete(fn_acp);
  end;
end;

procedure TfmMain.DoOpenArchive_HandleIniSections(const fn_inf, subdir: string;
  typ: TSynAddonType);
var
  i: Integer;
begin
  DoOpenArchive_HandleIni(fn_inf, subdir, 'ini', typ);
  for i:= 1 to cMaxSectionsInInf do
    if not DoOpenArchive_HandleIni(fn_inf, subdir, 'ini'+IntToStr(i), typ) then
      Break
end;

function TfmMain.DoOpenArchive_HandleIni(const fn_ini, subdir, section: string; typ: TSynAddonType): boolean;
var
  s_section, s_id, s_file, s_params, s_value: string;
begin
  with TIniFile.Create(fn_ini) do
  try
    s_section:= ReadString(section, 'section', '');
    s_id:= ReadString(section, 'id', '');
    s_file:= ReadString(section, 'file', '');
    s_params:= ReadString(section, 'params', '');
  finally
    Free
  end;

  Result:= s_section<>'';
  if not Result then Exit;

  if (s_id='') then
  begin
    MsgError('Section in inf-file is incorrect', Handle);
    Exit
  end;

  case typ of
    cAddonTypeBinPlugin:
      s_value:= subdir + '\' + s_file + ';' + s_params;
    cAddonTypePyPlugin:
      s_value:= cPyPrefix + subdir + ';' + s_params;
    else
      Exit;
  end;

  with TIniFile.Create(SynPluginsIni) do
  try
    WriteString(s_section, s_id, s_value);
  finally
    Free
  end;

  ///debug
  //MsgInfo(Format('Write key: [%s] %s=%s', [s_section, s_id, '.....']), Handle);
end;

procedure TfmMain.DoOpenArchive(const fn, AParams: Widestring);
const
  cInf = 'install.inf';
  cVInf = 'v.inf';
var
  fn_inf, dir_to: string;
  s_title, s_type, s_desc, s_ver, s_subdir: string;
  s_msg: Widestring;
  n_type: TSynAddonType;
  i: integer;
  AllowConfirm: boolean;
  VersionStr: string;
begin
  AllowConfirm:= Pos('/s', AParams)=0;
  VersionStr:= '';
  i:= Pos('/v', AParams);
  if i>0 then
    VersionStr:= Copy(AParams, i+2, MaxInt);

  dir_to:= FTempDir;
  fn_inf:= dir_to + '\' + cInf;
  FDelete(fn_inf);

  if not FUnpackSingle(fn, dir_to, cInf, false{asAdmin}) then
  begin
    MsgNoFile('Unzip.exe / Unrar.exe');
    Exit
  end;

  if not FileExists(fn_inf) then
  begin
    MsgError(DKLangConstW('zMInstallNoInf'), Handle);
    Exit
  end;

  with TIniFile.Create(fn_inf) do
  try
    s_title:= ReadString('info', 'title', '');
    s_type:= ReadString('info', 'type', '');
    s_desc:= ReadString('info', 'desc', '');
    s_ver:= ReadString('info', 'ver', '');
    s_subdir:= ReadString('info', 'subdir', '');
  finally
    Free
  end;

  n_type:= StringToAddonType(s_type);

  //don't show "template", show nicer "data/dir"
  if n_type=cAddonTypeData then
    s_type:= 'data/'+s_subdir;

  if (s_title='') then
  begin
    MsgError('Invalid field in inf-file: title', Handle);
    Exit
  end;
  if (s_subdir='') or (Pos('\', s_subdir)>0) or (Pos('/', s_subdir)>0) then
  begin
    MsgError('Invalid field in inf-file: subdir', Handle);
    Exit
  end;
  if (n_type = cAddonTypeNone) then
  begin
    MsgError('Invalid field in inf-file: type', Handle);
    Exit
  end;

  //confirm installation
  s_msg:= DKLangConstW('zMInstallThis') + #13#13 +
          DKLangConstW('zMInstallName') + ': ' + s_title + #13 +
          DKLangConstW('zMInstallType') + ': ' + s_type + #13;
  if s_desc<>'' then
    s_msg:= s_msg + DKLangConstW('zMInstallDesc') + ': ' + s_desc + #13;
  if s_ver<>'' then
    s_msg:= s_msg + DKLangConstW('zMInstallVer') + ': ' + s_ver + #13;
  s_msg:= s_msg + #13 + DKLangConstW('zMInstallYesNo');

  if AllowConfirm then
    if not MsgConfirm(s_msg, Handle, true) then Exit;

  case n_type of
    cAddonTypeBinPlugin:
      dir_to:= SynDir + 'Plugins\' + s_subdir;
    cAddonTypePyPlugin:
      dir_to:= SynPyDir + '\' + s_subdir;
    cAddonTypeData:
      dir_to:= SynDir + 'Data\' + s_subdir;
    cAddonTypeRoot:
      dir_to:= ExcludeTrailingPathDelimiter(SynDir);
    cAddonTypeLexer:
      dir_to:= FTempDir+'\Synwrite_lexer';
    else
      dir_to:= '?';
  end;

  //new inf filename
  fn_inf:= dir_to + '\' + cInf;

  if not FUnpackAll(fn, dir_to, IsElevationNeededForFolder(dir_to)) or
    not FileExists(fn_inf) then
  begin
    MsgError(DKLangConstW('zMInstallCantUnpack'), Handle);
    Exit
  end;

  if n_type in [cAddonTypeBinPlugin, cAddonTypePyPlugin] then
  begin
    DoRemovePluginsIniLines(SynPluginsIni, s_subdir, n_type=cAddonTypeBinPlugin);
    DoOpenArchive_HandleIniSections(fn_inf, s_subdir, n_type);
  end;

  if n_type=cAddonTypeLexer then
  begin
    for i:= 1 to cMaxSectionsInInf do
      if not DoOpenArchive_HandleLexer(fn_inf, 'lexer'+IntToStr(i)) then
        Break
  end;

  if n_type in [cAddonTypeBinPlugin, cAddonTypePyPlugin] then
  begin
    //plugin finalizing:
    //store version to v.inf
    if VersionStr<>'' then
      with TStringList.Create do
      try
        Add(VersionStr);
        SaveToFile(dir_to + '\' + cVInf);
      finally
        Free
      end;
  end
  else
  begin
    //non-plugin finalizing:
    //delete install.inf
    FDelete(fn_inf);
  end;

  //report results
  if AllowConfirm then
    if n_type=cAddonTypeLexer then
    begin
      s_msg:= DKLangConstW('zMInstallLexerOk');
      MsgInfo(s_msg, Handle);
    end
    else
    begin
      s_msg:= WideFormat(DKLangConstW('zMInstallOk'), [dir_to]);
      if MsgConfirm(s_msg, Handle, true{IsQuestion}) then
        acRestart.Execute;
    end;
end;

procedure TfmMain.TbxTabConsoleClick(Sender: TObject);
begin
  TabsOut.TabIndex:= Ord(tbConsole);
end;

procedure TfmMain.DoPyConsole_EnterCommand(const Str: Widestring);
var
  SNew: Widestring;
  Handled: boolean;
begin
  DoPyConsole_LogString(cPyConsolePrompt + Str);

  Handled:= DoPyEvent(CurrentEditor, cSynEventOnConsole,
    [SWideStringToPythonString(Str)]) = cPyTrue;

  if not Handled then
  try
    with GetPythonEngine do
    begin
      SNew:= Str;
      if SBegin(SNew, cPyConsolePrint) then
      begin
        Delete(SNew, 1, Length(cPyConsolePrint));
        SNew:= 'print('+SNew+')'
      end
      else
      if IsWordString(SNew, true) then
        SNew:= 'print('+SNew+')';

      ExecString(UTF8Encode(SNew));

      //code fails on entering "import sys"
      {
      Obj:= EvalString(UTF8Encode(Str));
      if Obj<>Py_None then
        DoPyConsole_LogString(PyObjectAsString(Obj));
      }
    end;
  except
    MsgBeep(true);
  end;

  edConsole.Text:= Str;
  ComboUpdate(edConsole, opSaveFindCount);
  edConsole.Text:= '';
  if edConsole.CanFocus then
    edConsole.SetFocus;
end;

procedure TfmMain.edConsoleKeyPress(Sender: TObject; var Key: Char);
var
  Str: Widestring;
begin
  Str:= edConsole.Text;
  if (Key=#13) then
  begin
    if Str=cPyConsoleClear then
    begin
      MemoConsole.Lines.Clear;
      edConsole.Text:= '';
    end
    else
      DoPyConsole_EnterCommand(Str);
    Key:= #0;
    Exit
  end;
end;

procedure TfmMain.ecToggleFocusConsoleExecute(Sender: TObject);
begin
  if not plOut.Visible then
  begin
    ecShowOut.Execute;
    TabsOut.TabIndex:= Ord(tbConsole);
    if Self.Enabled and edConsole.CanFocus then
      edConsole.SetFocus;
  end
  else
  if edConsole.Focused or MemoConsole.Focused then
    FocusEditor
  else
  begin
    TabsOut.TabIndex:= Ord(tbConsole);
    if Self.Enabled and edConsole.CanFocus then
      edConsole.SetFocus;
  end;
end;

procedure TfmMain.PythonEngine1BeforeLoad(Sender: TObject);
begin
  //note: for Lister plugin DllPath will be wrong, but it's ok:
  //even with correct DllPath Python won't load in Lister plugin
  //(cannot find MSVCRT DLLs).
  //note: need to set FatalAbort = FatalMsgDlg = False.
  //note: don't check SynExe here (not yet inited).
  with Sender as TPythonEngine do
  begin
    DllPath:= ExtractFilePath(ParamStr(0));
    InitScript.Add(cPyConsoleInit);
  end;
end;

procedure TfmMain.edConsoleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.DoPyConsole_RepeatCommand;
var
  S: Widestring;
begin
  S:= MemoConsole.Lines[MemoConsole.CaretPos.Y];
  if SBegin(S, cPyConsolePrompt) then
  begin
    Delete(S, 1, Length(cPyConsolePrompt));
    DoPyConsole_EnterCommand(S);
  end
  else
    MsgBeep;
end;

procedure TfmMain.MemoConsoleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Enter should repeat command already entered in memoConsole
  if (Key=13) and (Shift=[]) then
  begin
    DoPyConsole_RepeatCommand;
    Key:= 0;
    Exit
  end;

  DoHandleKeysInPanels(Key, Shift);
end;

procedure TfmMain.PythonEngine1AfterInit(Sender: TObject);
var
  SDir, S1, S2, S3: string;
begin
  SDir:= ExtractFilePath(Application.ExeName);
  S1:= SDir + 'DLLs';
  S2:= SDir + 'python33.zip';
  S3:= SDir + 'Py';
  Py_SetSysPath([S1, S2, S3]);
end;


//{$I unMainPy.pas}

procedure TfmMain.PythonModuleInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do
  begin
    AddMethod('msg_box', Py_msg_box, '');
    AddMethod('msg_status', Py_msg_status, '');
    AddMethod('dlg_input', Py_dlg_input, '');
    AddMethod('dlg_input_ex', Py_dlg_input_ex, '');
    AddMethod('dlg_input_memo', Py_dlg_input_memo, '');
    AddMethod('dlg_menu', Py_dlg_menu, '');
    AddMethod('dlg_snippet', Py_dlg_snippet, '');
    AddMethod('dlg_checklist', Py_dlg_checklist, '');
    AddMethod('dlg_file', Py_dlg_file, '');
    AddMethod('dlg_folder', Py_dlg_folder, '');

    AddMethod('app_version', Py_app_version, '');
    AddMethod('app_api_version', Py_app_api_version, '');
    AddMethod('app_exe_dir', Py_app_exe_dir, '');
    AddMethod('app_ini_dir', Py_app_ini_dir, '');
    AddMethod('app_log', Py_app_log, '');
    AddMethod('app_proc', Py_app_proc, '');
    AddMethod('lexer_proc', Py_lexer_proc, '');
    AddMethod('ed_handles', Py_ed_handles, '');

    AddMethod('ini_read', Py_ini_read, '');
    AddMethod('ini_write', Py_ini_write, '');

    AddMethod('file_open', Py_file_open, '');
    AddMethod('file_save', Py_file_save, '');
    AddMethod('file_get_name', Py_file_get_name, '');
    AddMethod('text_local', Py_text_local, '');
    AddMethod('text_convert', Py_text_convert, '');
    AddMethod('regex_parse', Py_regex_parse, '');

    AddMethod('get_app_prop', Py_get_app_prop, '');
    AddMethod('set_app_prop', Py_set_app_prop, '');

    AddMethod('ed_get_staple', Py_ed_get_staple, '');
    AddMethod('ed_get_bk', Py_ed_get_bk, '');
    AddMethod('ed_set_bk', Py_ed_set_bk, '');
    AddMethod('ed_get_sync_ranges', Py_ed_get_sync_ranges, '');
    AddMethod('ed_add_sync_range', Py_ed_add_sync_range, '');
    AddMethod('ed_focus', Py_ed_focus, '');
    AddMethod('ed_complete', Py_ed_complete, '');
    AddMethod('ed_get_split', Py_ed_get_split, '');
    AddMethod('ed_set_split', Py_ed_set_split, '');
    AddMethod('ed_set_attr', Py_ed_set_attr, '');
    AddMethod('ed_get_attr', Py_ed_get_attr, '');

    AddMethod('ed_get_text_all', Py_ed_get_text_all, '');
    AddMethod('ed_get_text_sel', Py_ed_get_text_sel, '');
    AddMethod('ed_get_text_line', Py_ed_get_text_line, '');
    AddMethod('ed_get_text_len', Py_ed_get_text_len, '');
    AddMethod('ed_get_text_substr', Py_ed_get_text_substr, '');
    AddMethod('ed_get_indent', Py_ed_get_indent, '');

    AddMethod('ed_get_caret_xy', Py_ed_get_caret_xy, '');
    AddMethod('ed_get_caret_pos', Py_ed_get_caret_pos, '');
    AddMethod('ed_set_caret_xy', Py_ed_set_caret_xy, '');
    AddMethod('ed_set_caret_pos', Py_ed_set_caret_pos, '');
    AddMethod('ed_add_caret_xy', Py_ed_add_caret_xy, '');
    AddMethod('ed_add_mark', Py_ed_add_mark, '');

    AddMethod('ed_pos_xy', Py_ed_pos_xy, '');
    AddMethod('ed_xy_pos', Py_ed_xy_pos, '');
    AddMethod('ed_log_xy', Py_ed_log_xy, '');
    AddMethod('ed_xy_log', Py_ed_xy_log, '');

    AddMethod('ed_get_line_count', Py_ed_get_line_count, '');
    AddMethod('ed_get_line_prop', Py_ed_get_line_prop, '');
    AddMethod('ed_get_carets', Py_ed_get_carets, '');
    AddMethod('ed_get_marks', Py_ed_get_marks, '');
    AddMethod('ed_get_prop', Py_ed_get_prop, '');
    AddMethod('ed_set_prop', Py_ed_set_prop_wrapper, '');
    AddMethod('ed_get_filename', Py_ed_get_filename, '');
    AddMethod('ed_get_alerts', Py_ed_get_alerts, '');
    AddMethod('ed_set_alerts', Py_ed_set_alerts, '');
    AddMethod('ed_get_tabcolor', Py_ed_get_tabcolor, '');
    AddMethod('ed_set_tabcolor', Py_ed_set_tabcolor, '');
    AddMethod('ed_get_indexes', Py_ed_get_indexes, '');
    AddMethod('ed_get_enc', Py_ed_get_enc, '');
    AddMethod('ed_set_enc', Py_ed_set_enc, '');

    AddMethod('ed_get_sel_mode', Py_ed_get_sel_mode, '');
    AddMethod('ed_get_sel_lines', Py_ed_get_sel_lines, '');
    AddMethod('ed_get_sel', Py_ed_get_sel, '');
    AddMethod('ed_get_sel_rect', Py_ed_get_sel_rect, '');
    AddMethod('ed_set_sel', Py_ed_set_sel, '');
    AddMethod('ed_set_sel_rect', Py_ed_set_sel_rect, '');

    AddMethod('ed_replace', Py_ed_replace, '');
    AddMethod('ed_insert', Py_ed_insert, '');
    AddMethod('ed_insert_snippet', Py_ed_insert_snippet, '');
    AddMethod('ed_set_text_all', Py_ed_set_text_all, '');
    AddMethod('ed_set_text_line', Py_ed_set_text_line, '');
    AddMethod('ed_get_word', Py_ed_get_word, '');

    AddMethod('ed_cmd', Py_ed_cmd, '');
    AddMethod('ed_lock', Py_ed_lock, '');
    AddMethod('ed_unlock', Py_ed_unlock, '');
    AddMethod('ed_find', Py_ed_find, '');
  end;
end;

procedure TfmMain.PythonGUIInputOutput1SendUniData(Sender: TObject;
  const Data: WideString);
begin
  DoPyConsole_LogString(Data);
end;

function TfmMain.DoPyLoadPlugin(const SFilename, SCmd: string): string;
var
  SId: string;
begin
  SId:= SFilename;
  if SBegin(SId, cPyPrefix) then
    Delete(SId, 1, Length(cPyPrefix));

  if not GetPythonEngine.Initialized then
  begin
    DoPyConsole_LogString(cPyNotInited);
    Exit
  end;

  Result:= Py_RunPlugin_Command(SId, SCmd);
end;

function TfmMain.DoPyLoadPluginWithParams(
  const SFilename, SCmd: string;
  AEd: TSyntaxMemo;
  const AParams: array of string): Widestring;
var
  SId: string;
begin
  SId:= SFilename;
  if SBegin(SId, cPyPrefix) then
    Delete(SId, 1, Length(cPyPrefix));

  if not GetPythonEngine.Initialized then
  begin
    DoPyConsole_LogString(cPyNotInited);
    Exit
  end;

  Result:= Py_RunPlugin_Event(SId, SCmd, AEd, AParams);
end;


procedure TfmMain.DoPyConsole_LogString(const Str: Widestring);
begin
  with MemoConsole do
  begin
    Lines.Add(Str);
    while Lines.Count > cPyConsoleMaxCount do
      Lines.Delete(0);
  end;

  MemoScrollToBottom(MemoConsole);
end;

procedure TfmMain.TbxItemHelpPyDirClick(Sender: TObject);
begin
  FOpenURL(SynPyDir, Handle);
end;

procedure TfmMain.PythonGUIInputOutput1ReceiveUniData(Sender: TObject;
  var Data: WideString);
begin
  Data:= '';
  if DoInputString('Python prompt:', Data) then begin end;
end;

procedure TfmMain.MemoConsoleDblClick(Sender: TObject);
begin
  DoPyConsole_RepeatCommand;
end;

procedure TfmMain.LoadConsoleHist;
begin
  ComboLoadFromFile(edConsole, SynHistoryIni, 'Console', false);
end;

procedure TfmMain.SaveConsoleHist;
begin
  ComboSaveToFile(edConsole, SynHistoryIni, 'Console');
end;

procedure TfmMain.InitSnippets;
begin
  if FListSnippets=nil then
  begin
    FListSnippets:= TList.Create;
    LoadSnippets;
  end;
end;

procedure TfmMain.LoadSnippets;
var
  Files: TTntStringList;
  InfoRec: TSynSnippetInfo;
  InfoClass: TSynSnippetClass;
  i: Integer;
begin
  ClearSnippets;

  Files:= TTntStringList.Create;
  try
    FFindToList(Files,
      SynSnippetsDir,
      '*.'+cSynSnippetExt, '',
      true{SubDirs},
      false, false, false,
      false{EnableProcMsg});

    for i:= 0 to Files.Count-1 do
      if DoLoadSnippetFromFile(Files[i], InfoRec) then
      begin
        InfoClass:= TSynSnippetClass.Create;
        InfoClass.Info.Filename:= InfoRec.Filename;
        InfoClass.Info.Name:= InfoRec.Name;
        InfoClass.Info.Id:= InfoRec.Id;
        InfoClass.Info.Lexers:= InfoRec.Lexers;
        InfoClass.Info.Text:= InfoRec.Text;
        FListSnippets.Add(InfoClass);
      end;
  finally
    FreeAndNil(Files);
  end;
end;

procedure TfmMain.ClearSnippets;
var
  i: Integer;
begin
  if Assigned(FListSnippets) then
  begin
    for i:= FListSnippets.Count-1 downto 0 do
    begin
      TObject(FListSnippets[i]).Free;
      FListSnippets[i]:= nil;
    end;
    FListSnippets.Clear;
  end;
end;

function TfmMain.DoSnippetChoice(const SInitialText: string): integer;
var
  IsModified: boolean;
  Form: TfmMenuSnippets;
begin
  Result:= -1;
  InitSnippets;

  Form:= TfmMenuSnippets.Create(Self);
  with Form do
  try
    UpdateMenuDialogBorder(Form);
    Caption:= DKLangConstW('zMSnippetList');

    Edit.Text:= SInitialText;
    MemoText.Font.Assign(CurrentEditor.Font);

    FInfoList:= Self.FListSnippets;
    FCurrentLexer:= Self.CurrentLexer;

    FIniFN:= Self.SynHistoryIni;
    FColorSel:= opColorOutSelText;
    FColorSelBk:= opColorOutSelBk;

    if ShowModal=mrOk then
      if List.ItemIndex>=0 then
        Result:= Integer(List.Items.Objects[List.ItemIndex]);

    IsModified:= FModified;
  finally
    Free
  end;

  if IsModified then
    LoadSnippets;
end;

procedure TfmMain.DoSnippetListDialog(const SInitialText: string);
var
  Ed: TSyntaxMemo;
  Index: Integer;
  SSelText, SSnippetText: Widestring;
begin
  Ed:= CurrentEditor;
  if Ed.ReadOnly then Exit;

  Index:= DoSnippetChoice(SInitialText);
  if Index>=0 then
  begin
    SSelText:= Ed.SelText;
    SSnippetText:= TSynSnippetClass(FListSnippets[Index]).Info.Text;

    Ed.BeginUpdate;
    try
      Ed.ClearSelection;
      if SInitialText<>'' then
      begin
        Ed.CaretStrPos:= Ed.CaretStrPos - Length(SInitialText);
        Ed.DeleteText(Length(SInitialText));
      end;
      EditorInsertSnippet(Ed, SSnippetText, SSelText, FrameOfEditor(Ed).FileName);
    finally
      Ed.EndUpdate;
    end;
  end;
end;

procedure TfmMain.DoSnippetNew;
var
  AInfo: TSynSnippetInfo;
  ADir: string;
begin
  DoClearSnippet(AInfo);
  AInfo.Lexers:= CurrentLexer;
  ADir:= SynSnippetsDir;

  if DoSnippetEditorDialog(AInfo) then
  begin
    CreateDir(ADir);
    if not DirectoryExists(ADir) then
      begin MsgNoDir(ADir); Exit end;

    with SD_Snippets do
    begin
      InitialDir:= ADir;
      FileName:= AInfo.Name;
      if Execute then
      begin
        DoSaveSnippetToFile(FileName, AInfo);
        DoSnippetsReload;
      end;
    end;
  end;
end;

procedure TfmMain.DoSnippetsReload;
begin
  if FListSnippets<>nil then
    LoadSnippets;
end;

procedure TfmMain.TbxItemRunSnippetsClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_SnippetsDialog);
end;

procedure TfmMain.TbxItemRunNewSnippetClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_NewSnippetDialog);
end;


procedure TfmMain.TbxItemTreeSortedClick(Sender: TObject);
begin
  with CurrentFrame do
  begin
    IsTreeSorted:= not IsTreeSorted;
    if IsTreeSorted then
      Tree.SortType:= stText
    else
      Tree.SortType:= stNone;
  end;

  Tree.UpdateTree;
end;

procedure TfmMain.ApplyPanelTitles;
var
  en: boolean;
begin
  en:= opShowPanelTitles;
  plTree.ShowCaptionWhenDocked:= en;
  plOut.ShowCaptionWhenDocked:= en;
  plClip.ShowCaptionWhenDocked:= en;
  if Assigned(FProjPreview) then
    FProjPreview.ShowCaptionWhenDocked:= en;
  DoPlugins_Resize;
end;

procedure TfmMain.TbxItemPanelTitleBarClick(Sender: TObject);
begin
  opShowPanelTitles:= not opShowPanelTitles;
  ApplyPanelTitles;

  with TIniFile.Create(SynIni) do
  try
    WriteBool('View', 'PaneTitle', opShowPanelTitles);
  finally
    Free
  end;
end;

procedure TfmMain.PopupPanelTitlePopup(Sender: TObject);
begin
  TbxItemPanelTitleBar.Checked:= opShowPanelTitles;
end;

function MainPyEditor(H: Integer): TSyntaxMemo;
var
  nTab: Integer;
begin
  Result:= nil;
  if Assigned(fmMain) then
  begin
    case H of
      0:
        Result:= fmMain.CurrentEditor;
      1:
        Result:= fmMain.BrotherEditor(fmMain.CurrentEditor);
      2:
        Result:= fmMain.OppositeFrame.EditorMaster;
      3:
        Result:= fmMain.OppositeFrame.EditorSlave;
      cPyEditorHandleMin..
      cPyEditorHandleMax:
        begin
          nTab:= H-cPyEditorHandleMin;
          if (nTab>=0) and (nTab<fmMain.FrameAllCount) then
            Result:= fmMain.FramesAll[nTab].EditorMaster
          else
            Result:= nil;
        end;
      else
        Result:= TSyntaxMemo(Pointer(H));
    end;
  end;
end;

procedure TfmMain.DoWorkaround_QViewHorzScroll;
begin
  //fix incorrect ScrollPosX>0
  if CurrentFrame<>nil then
    with CurrentFrame do
    begin
      EditorMaster.ScrollPosX:= 0;
      EditorSlave.ScrollPosX:= 0;
    end;
end;

procedure TfmMain.DoWorkaround_FindNext1;
begin
  if (Finder.Matches>0) and (Finder.MatchLen=0) and (not Finder.IsSpecialCase1) then
    with CurrentEditor do
      if CaretStrPos>0 then
        CaretStrPos:= CaretStrPos-1;
end;

function TfmMain.DoCheckAutoCorrectCase(Ed: TSyntaxMemo): boolean;
var
  NCaret, NLen, i: Integer;
  SId, SAcpId: string;
begin
  Result:= false;
  if not opAutoCase then Exit;

  NCaret:= Ed.CaretStrPos;
  if not IsWordChar(Ed.Lines.Chars[NCaret]) then Exit;

  SId:= EditorGetWordBeforeCaret(Ed, false);
  NLen:= Length(SId);

  for i:= 0 to FAcpList_Items.Count-1 do
  begin
    SAcpId:= FAcpList_Items[i];

    if not IsWordChar(WideChar(SAcpId[Length(SAcpId)])) then
      SetLength(SAcpId, Length(SAcpId)-1);

    if StrIComp(PChar(SAcpId), PChar(SId))=0 then
    begin
      if (SAcpId<>SId) and
        IsPositionMatchesTokens(Ed, NCaret-1, NCaret, tokensExceptCmtStr) then
      begin
        Ed.ReplaceText(NCaret-NLen, NLen, SAcpId);
        DoHint('Id: ' + SAcpId);
        Result:= true;
      end;
      Break;
    end;
  end;
end;

procedure TfmMain.DoOpenFolder(const dir: Widestring);
var
  L: TTntStringList;
  i, NCount: Integer;
  fn: Widestring;
begin
  L:= TTntStringList.Create;
  try
    FFindToList(L, dir, '*', '',
      true{SubDir},
      false{NoRO}, false{NoHidFiles}, true{NoHidFolders});

    //exclude binary files
    NCount:= L.Count;
    for i:= L.Count-1 downto 0 do
    begin
      Application.Title:= Format('filter %d / %d', [NCount-i, NCount]);
      Application.ProcessMessages;
      if Application.Terminated then Exit;

      fn:= L[i];
      if not IsFileText(fn) or IsFileTooBig(fn) then
        L.Delete(i);
    end;

    //confirm
    if L.Count > cMaxFilesInFolder then
      if not MsgConfirm(
        WideFormat(DKLangConstW('zMCfmOpenFolder'), [dir, L.Count]),
        Handle) then Exit;

    //open left files
    for i:= 0 to L.Count-1 do
    begin
      Application.Title:= Format('... %d / %d', [i+1, L.Count]);
      Application.ProcessMessages;
      if Application.Terminated then Exit;

      fn:= L[i];
      DoOpenFile(fn);
    end;
  finally
    FreeAndNil(L);
  end;
end;

procedure TfmMain.DoEnumLexers(L: TTntStrings; AlsoDisabled: boolean = false);
var
  i: Integer;
begin
  with SyntaxManager do
    for i:= 0 to AnalyzerCount-1 do
      if AlsoDisabled or not Analyzers[i].Internal then
        L.Add(Analyzers[i].LexerName);
end;

procedure TfmMain.TBXItemMarkGoLastClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_JumpToLastMarker);
end;

procedure TfmMain.TBXItemMarkClearClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_MarkersClear);
end;

procedure TfmMain.TbxItemWinSplitHClick(Sender: TObject);
begin
  with CurrentFrame do
  begin
    if not IsSplitted then
    begin
      SplitHorz:= true;
      SplitPos:= 50;
    end
    else
    if not SplitHorz then
      SplitHorz:= true
    else
      SplitPos:= 0;
  end;
end;

procedure TfmMain.TbxItemWinSplitVClick(Sender: TObject);
begin
  with CurrentFrame do
  begin
    if not IsSplitted then
    begin
      SplitHorz:= false;
      SplitPos:= 50;
    end
    else
    if SplitHorz then
      SplitHorz:= false
    else
      SplitPos:= 0;
  end;
end;

function TfmMain.FrameIndex(F: TEditorFrame): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to FrameAllCount-1 do
    if F=FramesAll[i] then
    begin
      Result:= i;
      Break
    end;
end;

procedure TfmMain.DoFindDialog_FindAllInCurrentTab(AWithBkmk, ASelectResults: boolean);
var
  ADir: Widestring;
  AFrame: TEditorFrame;
  AFrameIndex: Integer;
begin
  CurrentEditor.ResetSearchMarks;
  CurrentEditor.RemoveCarets;

  ADir:= '';
  AFrame:= CurrentFrame;
  AFrameIndex:= FrameIndex(AFrame);

  //init TreeRoot, show pane
  UpdateTreeFind_Initial(Finder.FindText, ADir, true);
  TabsOut.TabIndex:= Ord(tbFindRes);
  plOut.Show;

  FListFiles.Clear;
  FListResFN_Prev:= '';
  FListResFN:= SGetTabPrefix + IntToStr(AFrameIndex+1) + '[' + AFrame.TabCaption + ']';

  try
    DoFind_InFrame(AFrame, true, AWithBkmk);
  except
    on E: Exception do
    begin
      MsgExcept('Error on searching in tab', E, Handle);
      DoProgressHide;
      Exit;
    end;
  end;

  //update states
  if Assigned(fmSR) then
    fmSR.ShowError(Finder.Matches=0);
  UpdateStatusbar; //needed as bookmarks appear
  UpdateFrameMicroMap(AFrame);

  //update "Search Results" pane
  if FTreeRoot=nil then
    raise Exception.Create('TreeRoot nil');
  UpdateTreeFind_Results(Finder.FindText, ADir, false, true);
  TabsOut.TabIndex:= Ord(tbFindRes);
  plOut.Show;

  if ASelectResults then
    CurrentEditor.ExecCommand(smCaretsFromMarksRight);
end;

procedure TfmMain.DoPyStringToEvents(const Str: string;
  var AEvents: TSynPyEvents;
  var AKeycodes: string);
var
  SText, SItem: Widestring;
  ev: TSynPyEvent;
begin
  AEvents:= [];
  AKeycodes:= '';
  SText:= Str;

  repeat
    SItem:= SGetItem(SText);
    if SItem='' then Break;

    if SItem[1]='k' then
    begin
      Delete(SItem, 1, 1);
      AKeyCodes:= AKeyCodes+SItem+',';
      Continue;
    end;

    for ev:= Low(TSynPyEvent) to High(TSynPyEvent) do
      if SItem=cSynPyEvent[ev] then
      begin
        Include(AEvents, ev);
        Break
      end;
  until false;
end;

function TfmMain.DoPyEvent(AEd: TSyntaxMemo; AEvent: TSynPyEvent;
  const AParams: array of string): Widestring;
const
  cTrueResultStopsThese = [cSynEventOnComplete, cSynEventOnFuncHint];  
var
  SCurLexer: string;
  i: Integer;
begin
  //empty string result means "no handlers for event"
  Result:= '';
  SCurLexer:= CurrentLexerForFile;

  for i:= Low(FPluginsEvent) to High(FPluginsEvent) do
    with FPluginsEvent[i] do
    begin
      if (SFilename='') then Break;
      if (AEvent in Events) then
        if (SLexers='') or IsLexerListed(SCurLexer, SLexers) then
        begin
          //check that OnKey event is called for supported keys
          if (AEvent=cSynEventOnKey) then
            if Length(AParams)>=1 then
              if (SKeycodes<>'') and not IsStringListed(AParams[0], SKeycodes) then
                Continue;

          //call Python
          Result:= DoPyLoadPluginWithParams(SFilename, cSynPyEvent[AEvent], AEd, AParams);

          //True for some events means "stop"
          if Result=cPyTrue then
            if AEvent in cTrueResultStopsThese then Exit;

          //False means "stop", other results ignored
          if Result=cPyFalse then Exit;
        end;
    end;
end;

procedure TfmMain.DoPyEvent_GetLineNumber(
  AEd: TSyntaxMemo;
  const ALineNum: Integer;
  var AResult: string);
var
  AEvent: TSynPyEvent;
  i: Integer;
begin
  AEvent:= cSynEventOnNumber;

  for i:= Low(FPluginsEvent) to High(FPluginsEvent) do
    with FPluginsEvent[i] do
    begin
      if (SFilename='') then Break;
      if (AEvent in Events) then
      //no check for lexer name, to speed-up
      begin
        AResult:= DoPyLoadPluginWithParams(SFilename, cSynPyEvent[AEvent], AEd, [IntToStr(ALineNum)]);
        Exit;
      end;
    end;
end;


procedure TfmMain.DoPlugins_LoadEvents(const fn_plug_ini: string);
var
  ListSec: TStringList;
  NIndex, i: Integer;
  sKey, sValue, sValue2, sValue3, sValue4, sValue5: Widestring;
begin
  //clear Event list
  for i:= Low(FPluginsEvent) to High(FPluginsEvent) do
    with FPluginsEvent[i] do
    begin
      SFileName:= '';
      SLexers:= '';
      Events:= [];
    end;

  //load section "Events"
  ListSec:= TStringList.Create;
  with TIniFile.Create(fn_plug_ini) do
  try
    ReadSectionValues('Events', ListSec);
  finally
    Free
  end;

  //parse section "Events"
  try
    NIndex:= Low(FPluginsEvent);
    for i:= 0 to ListSec.Count-1 do
    begin
      SGetKeyAndValues(ListSec[i], sKey, sValue, sValue2, sValue3, sValue4, sValue5);
      if (sKey='') or (sValue='') then Continue;

      if NIndex<=High(FPluginsEvent) then
      begin
        FPluginsEvent[NIndex].SFileName:= sValue;
        DoPyStringToEvents(sValue2,
          FPluginsEvent[NIndex].Events,
          FPluginsEvent[NIndex].SKeycodes);
        FPluginsEvent[NIndex].SLexers:= sValue3;
        Inc(NIndex);
      end;
    end;
  finally
    FreeAndNil(ListSec);
  end;
end;


procedure TfmMain.DoPlugins_LoadCommands(const fn_plug_ini: string);
var
  ListSec: TStringList;
  NIndex, NCommandId, i: Integer;
  sKey, sValue, sValue2, sValueLexers, sValueHotkey, sValueFlags: Widestring;
begin
  //clear Command list
  for i:= Low(FPluginsCommand) to High(FPluginsCommand) do
    with FPluginsCommand[i] do
    begin
      SCaption:= '';
      SFileName:= '';
      SLexers:= '';
      SCmd:= '';
    end;

  TBXSubmenuPlugins.Clear;
  TBXSubmenuCtxPlugins.Clear;

  //load section "Commands"
  ListSec:= TStringList.Create;
  with TIniFile.Create(fn_plug_ini) do
  try
    ReadSectionValues('Commands', ListSec);
  finally
    Free
  end;

  //parse section "Commands"
  try
    NIndex:= Low(FPluginsCommand);
    for i:= 0 to ListSec.Count-1 do
    begin
      SGetKeyAndValues(ListSec[i], sKey, sValue, sValue2, sValueLexers, sValueHotkey, sValueFlags);

      if (sValue=cPyPrefix+cPyPluginManager) then Continue; //don't add Plugin Manager here
      if (sKey='') or (sValue='') then Continue;
      //field sValueFlags is for future

      if NIndex<=High(FPluginsCommand) then
      begin
        if SBegin(sValue, cPyPrefix) or SBegin(sValue, '-') then
          FPluginsCommand[NIndex].SFileName:= sValue
        else
          FPluginsCommand[NIndex].SFileName:= SynDir + 'Plugins\' + sValue;
        FPluginsCommand[NIndex].SCmd:= sValue2;
        FPluginsCommand[NIndex].SLexers:= sValueLexers;
        FPluginsCommand[NIndex].SCaption:= sKey;
        NCommandId:= cPyCommandBase+NIndex;

        //1) add to keymapping
        DoAddKeymappingCommand(NCommandId, 'Plugin', sKey, sValueHotkey);

        //2) add to main-menu
        DoPlugin_AddMenuItem(TBXSubmenuPlugins, sKey, NIndex, NCommandId);

        //3) add to context menu (if enabled)
        if Pos('-', sValueFlags)=0 then
        begin
          TBXSubmenuCtxPlugins.Visible:= true;
          DoPlugin_AddMenuItem(TBXSubmenuCtxPlugins, sKey, NIndex, NCommandId);
        end;

        Inc(NIndex);
      end;
    end;
  finally
    FreeAndNil(ListSec);
  end;

  DoSortMenu(TbxSubmenuPlugins);
  DoSortMenu(TBXSubmenuCtxPlugins);
end;

procedure TfmMain.DoPlugins_LoadAutoComplete(const fn_plug_ini: string);
var
  ListSec: TStringList;
  NIndex, i: Integer;
  sKey, sValue, sValue2, sValue3, sValue4, sValue5: Widestring;
begin
  //clear ACP list
  for i:= Low(FPluginsAcp) to High(FPluginsAcp) do
    with FPluginsAcp[i] do
    begin
      SLexers:= '';
      SFileName:= '';
    end;

  //load section "Complete"
  ListSec:= TStringList.Create;
  with TIniFile.Create(fn_plug_ini) do
  try
    ReadSectionValues('Complete', ListSec);
  finally
    Free
  end;

  //parse section "Complete"
  try
    NIndex:= Low(FPluginsAcp);
    for i:= 0 to ListSec.Count-1 do
    begin
      SGetKeyAndValues(ListSec[i], sKey, sValue, sValue2, sValue3, sValue4, sValue5);
      if (sKey='') or (sValue='') then Continue;

      if NIndex<=High(FPluginsAcp) then
      begin
        if SBegin(sValue, cPyPrefix) then
          FPluginsAcp[NIndex].SFileName:= sValue
        else
          FPluginsAcp[NIndex].SFileName:= SynDir + 'Plugins\' + sValue;
        FPluginsAcp[NIndex].SLexers:= sValue2;
        Inc(NIndex);
      end;
    end;
  finally
    FreeAndNil(ListSec);
  end;
end;

procedure TfmMain.DoPlugins_LoadGotoDef(const fn_plug_ini: string);
var
  ListSec: TStringList;
  NIndex, i: Integer;
  sKey, sValue, sValue2, sValue3, sValue4, sValue5: Widestring;
begin
  //clear FindID list
  for i:= Low(FPluginsFindid) to High(FPluginsFindid) do
    with FPluginsFindid[i] do
    begin
      SLexers:= '';
      SFileName:= '';
    end;

  //load section "FindID"
  ListSec:= TStringList.Create;
  with TIniFile.Create(fn_plug_ini) do
  try
    ReadSectionValues('FindID', ListSec);
  finally
    Free
  end;

  //parse section "FindID"
  try
    NIndex:= Low(FPluginsFindid);
    for i:= 0 to ListSec.Count-1 do
    begin
      SGetKeyAndValues(ListSec[i], sKey, sValue, sValue2, sValue3, sValue4, sValue5);
      if (sKey='') or (sValue='') then Continue;

      if NIndex<=High(FPluginsFindid) then
      begin
        if SBegin(sValue, cPyPrefix) then
          FPluginsFindid[NIndex].SFileName:= sValue
        else
          FPluginsFindid[NIndex].SFileName:= SynDir + 'Plugins\' + sValue;
        FPluginsFindid[NIndex].SLexers:= sValue2;
        Inc(NIndex);
      end;
    end;
  finally
    FreeAndNil(ListSec);
  end;
end;

procedure TfmMain.DoPlugins_LoadPanels(const fn_plug_ini: string);
var
  ListSec: TStringList;
  NIndex, i: Integer;
  sKey, sValue, sValue2, sValue3, sValue4, sValue5: Widestring;
begin
  //clear Panels list
  for i:= Low(FPluginsPanel) to High(FPluginsPanel) do
    with FPluginsPanel[i] do
    begin
      SCaption:= '';
      SFileName:= '';
    end;

  //load section "Panels"
  ListSec:= TStringList.Create;
  with TIniFile.Create(fn_plug_ini) do
  try
    ReadSectionValues('Panels', ListSec);
  finally
    Free
  end;

  //parse section "Panels"
  try
    NIndex:= Low(FPluginsPanel);
    for i:= 0 to ListSec.Count-1 do
    begin
      SGetKeyAndValues(ListSec[i], sKey, sValue, sValue2, sValue3, sValue4, sValue5);
      if (sKey='') or (sValue='') then Continue;
      if (sKey='SynFTP') then sKey:= 'FTP'; //show simplier title

      if NIndex<=High(FPluginsPanel) then
      begin
        FPluginsPanel[NIndex].SCaption:= sKey;
        FPluginsPanel[NIndex].SFileName:= SynDir + 'Plugins\' + sValue;
        Inc(NIndex);
      end;
    end;
  finally
    FreeAndNil(ListSec);
  end;
end;

procedure TfmMain.DoPlugins_Test;
var
  i: Integer;
  sValue: string;
begin
  //test output
  sValue:= '';
  for i:= 0 to 5 do
    with FPluginsEvent[i] do
      if SFilename<>'' then
        sValue:= sValue+SFilename+#13+SLexers+#13+SKeycodes+#13{+IfThen(cSynEventOnSave in Events, 'on_save')};
  MsgInfo(sValue, Handle);
end;

procedure TfmMain.DoClearFindDialogStatus;
begin
  if Assigned(fmSR) then
    fmSR.ShowStatus('');
end;

procedure TfmMain.DoPyResetPlugins;
var
  fn, Cmd: string;
  L: TStringList;
begin
  fn:= SynPyDir + '\sw_reset_plugins.py';
  if not FileExists(fn) then
    begin MsgNoFile(fn); Exit end;

  L:= TStringList.Create;
  try
    L.LoadFromFile(fn);
    GetPythonEngine.ExecStrings(L);
    Cmd:= Format('_reset_plugins(r"%s")', [SynPyDir]);
    GetPythonEngine.ExecString(Cmd);
  finally
    FreeAndNil(L)
  end;
end;


procedure TfmMain.ecToggleProjPreviewExecute(Sender: TObject);
begin
  if Assigned(FProjPreview) then
  begin
    if Assigned(fmProj) then
      fmProj.DoPreview(true{Toggle})
    else
      DoPreviewFile('', true{Toggle}, 0, 0, 0);
  end;
end;

procedure TfmMain.UpdateLexerTo(An: TSyntAnalyzer);
begin
  SyntaxManager.CurrentLexer:= An;
  SyntaxManagerChange(Self);
end;

procedure TfmMain.acExportHTMLBeforeExecute(Sender: TObject);
begin
  UpadateFilenameForExport;
end;

procedure TfmMain.UpadateFilenameForExport;
begin
  //variable in ecActns.pas
  ecActns.ecExportsBaseFilename:= ChangeFileExt(ExtractFileName(CurrentFrame.FileName), '');
end;

function TfmMain.DoGetProjectFilename(id: Integer): Widestring;
begin
  Result:= '';
  if Assigned(fmProj) then
    if (id>=0) and (id<fmProj.TreeProj.Items.Count) then
      Result:= fmProj.GetFN(fmProj.TreeProj.Items[id]);
end;

procedure TfmMain.ProjRunTool(const ATool: TSynTool);
begin
  DoTool_Run(ATool);
end;

procedure TfmMain.TBXSubmenuBookmarksPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  bk, ro: boolean;
begin
  ro:= CurrentEditor.ReadOnly;
  bk:= CurrentEditor.BookmarkObj.Count>0;

  TBXItemBkClear.Enabled:= bk;
  TBXItemBkNext.Enabled:= bk;
  TBXItemBkPrev.Enabled:= bk;
  TBXItemBkCopy.Enabled:= bk;
  TBXItemBkCut.Enabled:= bk and not ro;
  TBXItemBkDel.Enabled:= bk and not ro;
  TBXItemBkDelUnmk.Enabled:= not ro;
  TBXItemBkPaste.Enabled:= bk and not ro;
  TBXItemBkGoto.Enabled:= bk;
end;



procedure TfmMain.DoToggleTabDirs;
begin
  opTabFolders:= not opTabFolders;
  ApplyTabOptions;
end;

procedure TfmMain.TbxItemPreCopyClick(Sender: TObject);
begin
  if Assigned(FProjPreviewEditor) then
    FProjPreviewEditor.CopyToClipboard;
end;

procedure TfmMain.TbxItemPreSelectClick(Sender: TObject);
begin
  if Assigned(FProjPreviewEditor) then
    FProjPreviewEditor.SelectAll;
end;


procedure TfmMain.TbxItemPreZoom25Click(Sender: TObject);
begin
  ApplyPreviewZoom(25);
end;

procedure TfmMain.TbxItemPreZoom50Click(Sender: TObject);
begin
  ApplyPreviewZoom(50);
end;

procedure TfmMain.TbxItemPreZoom75Click(Sender: TObject);
begin
  ApplyPreviewZoom(75);
end;

procedure TfmMain.TbxItemPreZoom100Click(Sender: TObject);
begin
  ApplyPreviewZoom(100);
end;

procedure TfmMain.TbxItemPreEditClick(Sender: TObject);
begin
  ProjPreviewButtonClick(Self);
end;

procedure TfmMain.TbxItemPreZoomOtherClick(Sender: TObject);
var
  Str: Widestring;
begin
  if Assigned(FProjPreviewEditor) then
  begin
    Str:= IntToStr(FProjPreviewEditor.Zoom);
    if DoInputString(DKLangConstW('zMZoomPrompt'), Str) then
      ApplyPreviewZoom(StrToIntDef(Str, FProjPreviewEditor.Zoom));
  end;
end;

procedure TfmMain.PopupPreviewEditorPopup(Sender: TObject);
var
  Ed: TSyntaxMemo;
begin
  //captions
  TbxItemPreCopy.Caption:= TBXItemECopy.Caption;
  TbxItemPreSelect.Caption:= TBXItemESelectAll.Caption;
  UpdKey_String(TbxItemPreCopy, ''); //remove text "Ctrl+C"
  UpdKey_String(TbxItemPreSelect, ''); //remove text "Ctrl+A"

  TbxItemPreZoom25.Caption:= '25%';
  TbxItemPreZoom50.Caption:= '50%';
  TbxItemPreZoom75.Caption:= '75%';
  TbxItemPreZoom100.Caption:= '100%';
  TbxItemPreZoomOther.Caption:= TBXItemZOther.Caption;
  TbxItemPreEdit.Caption:= DKLangConstW('MPreButton');

  //checked states
  Ed:= FProjPreviewEditor;
  if Assigned(Ed) then
  begin
    TbxItemPreCopy.Enabled:= Ed.HaveSelection;
    TbxItemPreEdit.Enabled:= FProjPreviewFilename<>'';

    TbxItemPreZoom25.Checked:= Ed.Zoom=25;
    TbxItemPreZoom50.Checked:= Ed.Zoom=50;
    TbxItemPreZoom75.Checked:= Ed.Zoom=75;
    TbxItemPreZoom100.Checked:= Ed.Zoom=100;
    TbxItemPreZoomOther.Checked:= not (
      TbxItemPreZoom25.Checked or
      TbxItemPreZoom50.Checked or
      TbxItemPreZoom75.Checked or
      TbxItemPreZoom100.Checked );
  end;
end;

procedure TfmMain.DoInsertUnicodeHexDialog;
var
  Str: Widestring;
  Num: LongWord;
begin
  if CurrentEditor.ReadOnly then Exit;
  if not DoInputCharCode(Str, Num, SynHistoryIni) then Exit;
  CurrentEditor.InsertText(Str);
  DoHint(WideFormat(DKLangConstW('zMInputUnicodeHex'),
    [Str, IntToHex(Num, 4)]));
end;

procedure TfmMain.InitGroups;
begin
  Groups:= TATGroups.Create(Self);
  Groups.Parent:= PanelBg;
  Groups.Align:= alClient;

  Groups.OnTabAdd:= TabAdd;
  Groups.OnTabFocus:= TabFocus;
  Groups.OnTabClose:= TabClose;
  Groups.OnTabPopup:= TabPopup;
  Groups.OnTabOver:= TabOver;
  Groups.OnTabMove:= TabMove;

  ApplyTabOptions;
  Groups.Mode:= opGroupMode;
  Groups.SplitPos:= opGroupSplit;
end;

function TfmMain.DoAddTab(Pages: TATPages; AUntitledStr: boolean): TEditorFrame;
begin
  Result:= CreateFrame;
  Result.Parent:= Self;
  Result.Visible:= false;

  UpdateEditorCaret(Result.EditorMaster);
  UpdateEditorCaret(Result.EditorSlave);
  UpdateColorHint;
  UpdateFrameZoom(Result);

  Pages.AddTab(Result, '?', false);
  if AUntitledStr then
    Result.TabCaption:= GetUntitledString;
end;

procedure TfmMain.TabAdd(Sender: TObject);
begin
  DoAddTab((Sender as TATTabs).Parent as TATPages, true);
end;

procedure TfmMain.TabFocus(Sender: TObject);
var
  ATabs: TATTabs;
  APages: TATPages;
  D: TATTabData;
  NPages: Integer;
begin
  ATabs:= Sender as TATTabs;
  APages:= ATabs.Parent as TATPages;

  D:= ATabs.GetTabData(ATabs.TabIndex);
  if D<>nil then
    if D.TabObject<>nil then
    begin
      if QuickView then
      begin
        //don't focus! but set CurrentEditor.
        CurrentEditor:= (D.TabObject as TEditorFrame).EditorMaster;
      end
      else
        FocusFrame(D.TabObject as TEditorFrame);

      UpdateOnFrameChanged;

      NPages:= Groups.PagesIndexOf(APages);
      if NPages>=Low(TabSwitchers) then
        TabSwitchers[NPages].UpdateTabList(ATabs.TabIndex, -1, -1);
    end;
end;

procedure TfmMain.TabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
var
  ATabs: TATTabs;
  D: TATTabData;
  F: TEditorFrame;
  NotAllowed: boolean;
begin
  ATabs:= Sender as TATTabs;
  D:= ATabs.GetTabData(ATabIndex);
  if D<>nil then
  begin
    F:= D.TabObject as TEditorFrame;

    //don't close last tab if it's empty and not modified (for group1)
    NotAllowed:= (ATabIndex=0) and (ATabs.TabCount=1) and
      (not F.Modified) and (F.EditorMaster.TextLength=0) and
      not (ATabs.Parent as TATPages).EnabledEmpty;
      
    if NotAllowed then  
      ACanClose:= false
    else
      CloseFrameWithCfm(F, ACanClose, ACanContinue);
  end;
end;

procedure TfmMain.TabPopup(Sender: TObject);
var
  P: TPoint;
begin
  P:= Mouse.CursorPos;
  PopupTabContext.Popup(P.X, P.Y);
end;

procedure TfmMain.TbxItemGroupOneClick(Sender: TObject);
begin
  Groups.Mode:= gmOne;
end;

procedure TfmMain.TbxItemGroup2HClick(Sender: TObject);
begin
  Groups.Mode:= gm2Vert;
end;

procedure TfmMain.TbxItemGroup2VClick(Sender: TObject);
begin
  Groups.Mode:= gm2Horz;
end;

procedure TfmMain.TbxItemGroup3HClick(Sender: TObject);
begin
  Groups.Mode:= gm3Vert;
end;

procedure TfmMain.TbxItemGroup3VClick(Sender: TObject);
begin
  Groups.Mode:= gm3Horz;
end;

procedure TfmMain.TbxItemGroup3as1p2Click(Sender: TObject);
begin
  Groups.Mode:= gm3Plus;
end;

procedure TfmMain.TbxItemGroup4HClick(Sender: TObject);
begin
  Groups.Mode:= gm4Vert;
end;

procedure TfmMain.TbxItemGroup4VClick(Sender: TObject);
begin
  Groups.Mode:= gm4Horz;
end;

procedure TfmMain.TbxItemGroup4GridClick(Sender: TObject);
begin
  Groups.Mode:= gm4Grid;
end;

procedure TfmMain.TbxItemGroup6GridClick(Sender: TObject);
begin
  Groups.Mode:= gm6Grid;
end;

procedure TfmMain.TBXSubmenuGroupsPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  TbxItemGroupOne.Checked:= Groups.Mode=gmOne;
  TbxItemGroup2H.Checked:= Groups.Mode=gm2Vert;
  TbxItemGroup2V.Checked:= Groups.Mode=gm2Horz;
  TbxItemGroup3H.Checked:= Groups.Mode=gm3Vert;
  TbxItemGroup3V.Checked:= Groups.Mode=gm3Horz;
  TbxItemGroup3as1p2.Checked:= Groups.Mode=gm3Plus;
  TbxItemGroup4H.Checked:= Groups.Mode=gm4Vert;
  TbxItemGroup4V.Checked:= Groups.Mode=gm4Horz;
  TbxItemGroup4Grid.Checked:= Groups.Mode=gm4Grid;
  TbxItemGroup6Grid.Checked:= Groups.Mode=gm6Grid;
end;

procedure TfmMain.TbxItemToGroup1Click(Sender: TObject);
var
  N: Integer;
begin
  N:= (Sender as TComponent).Tag;
  if N>0 then
  begin
    if (N=2) and (Groups.Mode=gmOne) then
      Groups.Mode:= gm2Horz;
    Groups.MoveTab(Groups.PopupPages, Groups.PopupTabIndex, Groups.Pages[N], -1, false);
  end
  else
  if (N=-1) then
    Groups.MovePopupTabToNext(true)
  else
  if (N=-2) then
    Groups.MovePopupTabToNext(false);
end;

procedure TfmMain.TBXSubmenuItemToGroupPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  Cnt, N: Integer;
begin
  Cnt:= Groups.PagesVisibleCount; //visible groups
  N:= Groups.PagesIndexOf(Groups.PopupPages); //current group

  TbxItemToGroup1.Enabled:= (Cnt>=2) and (N<>1);
  TbxItemToGroup2.Enabled:= {(Cnt>=2) and} (N<>2);
  TbxItemToGroup3.Enabled:= (Cnt>=3) and (N<>3);
  TbxItemToGroup4.Enabled:= (Cnt>=4) and (N<>4);
  TbxItemToGroup5.Enabled:= (Cnt>=5) and (N<>5);
  TbxItemToGroup6.Enabled:= (Cnt>=6) and (N<>6);
  TbxItemToGroupNext.Enabled:= Cnt>=2;
  TbxItemToGroupPrev.Enabled:= Cnt>=2;
end;

function TfmMain.DoSetPagesAndTabIndex(APageIndex, ATabIndex: Integer): boolean;
begin
  Result:= Groups.SetPagesAndTabIndex(APageIndex, ATabIndex);
end;


function TfmMain.GetCurrentFrameInPages(Pages: TATPages): TEditorFrame;
var
  D: TATTabData;
begin
  Result:= nil;
  if Pages<>nil then
    with Pages do
      if Tabs.TabCount>0 then
      begin
        D:= Tabs.GetTabData(Tabs.TabIndex);
        if D<>nil then
          Result:= D.TabObject as TEditorFrame;
      end;
end;


procedure TfmMain.TabOver(Sender: TObject; ATabIndex: Integer);
var
  D: TATTabData;
begin
  if ATabIndex>=0 then //discard -1, -2
    with (Sender as TATTabs) do
    begin
      D:= GetTabData(ATabIndex);
      if D<>nil then
        DoShowHintFilename((D.TabObject as TEditorFrame).FileName);
    end
  else
    DoHint('');
end;

procedure TfmMain.DoColorsArrayRead(var C: TSynColors; const StrIni: string);
var
  i: Integer;
  Str: Widestring;
begin
  Str:= StrIni;
  for i:= Low(C) to High(C) do
    C[i]:= StrToIntDef(SGetItem(Str), C[i]);
end;

function TfmMain.DoColorsArrayAsString(const C: TSynColors): string;
var
  i: Integer;
begin
  Result:= '';
  for i:= Low(C) to High(C) do
    Result:= Result+IntToStr(C[i])+',';
end;

procedure TfmMain.UpdateActiveTabColors;
var
  i: Integer;
begin
  for i:= Low(Groups.Pages) to High(Groups.Pages) do
    with Groups.Pages[i] do
    begin
      Tabs.ColorTabActive:= IfThen(
        Groups.Pages[i]=Groups.PagesCurrent,
        opColorTabBgActive,
        opColorTabBgActive2);
      Tabs.Invalidate;
    end;
end;

procedure TfmMain.TbxTabBookmarksClick(Sender: TObject);
begin
  TabsOut.TabIndex:= Ord(tbBookmarks);
end;

procedure TfmMain.UpdateListBookmarks;
const
  cMaxLen = 100;
var
  L: TList;
  F: TEditorFrame;
  Ed: TSyntaxMemo;
  i, j, NLine: Integer;
  bm: TBookmark;
  SDesc: Widestring;
begin
  if not ListBookmarks.Visible then Exit;
  ListBookmarks.Items.BeginUpdate;
  ListBookmarks.Items.Clear;

  L:= TList.Create;
  try
    for j:= 0 to FrameAllCount-1 do
    begin
      F:= FramesAll[j];
      if F=nil then Continue;
      Ed:= F.EditorMaster;
      if Ed=nil then Continue;

      L.Clear;
      EditorGetBookmarksAsSortedList_Ex(Ed, L);

      for i:= 0 to L.Count-1 do
      begin
        bm:= TBookmark(L[i]);
        if bm=nil then Continue;
        NLine:= Ed.StrPosToCaretPos(bm.Position).Y;

        SDesc:= '';
        if (NLine>=0) and (NLine<Ed.Lines.Count) then
        begin
          SDesc:= Ed.Lines[NLine];
          SDesc:= Copy(SDesc, 1, cMaxLen);
          SReplaceAllW(SDesc, #9, '    '{not exact len});
        end;

        with ListBookmarks.Items.Add do
        begin
          ImageIndex:= bm.ImageIndex;
          if F.FileName<>'' then
            Caption:= F.FileName
          else
            Caption:= F.TabCaption;
          SubItems.Add(IntToStr(NLine+1));
          SubItems.Add(SDesc);
        end;
      end;
    end;
  finally
    FreeAndNil(L);
  end;

  ListBookmarks.Items.EndUpdate;
end;

procedure TfmMain.DoListBookmarksNavigate;
var
  fn: Widestring;
  F: TEditorFrame;
  Num, i: Integer;
begin
  with ListBookmarks do
    if Selected<>nil then
    begin
      fn:= Selected.Caption;
      Num:= StrToIntDef(Selected.SubItems[0], -1);
      if Num<0 then Exit;

      //goto named tab
      if IsFileExist(fn) then
      begin
        F:= DoOpenFile(fn);
        if F<>nil then
          F.EditorMaster.CaretPos:= Point(0, Num-1);
      end
      else
      //goto untitled tab
      if SBegin(fn, DKLangConstW('unnamed')) then //more check, only untitled
        for i:= 0 to FrameAllCount-1 do
        begin
          F:= FramesAll[i];
          if (F.FileName='') and (F.TabCaption=fn) then
          begin
            CurrentFrame:= F;
            F.EditorMaster.CaretPos:= Point(0, Num-1);
            Break
          end;
        end;
    end;
end;

procedure TfmMain.ListBookmarksDblClick(Sender: TObject);
begin
  DoListBookmarksNavigate;
end;

procedure TfmMain.ListBookmarksKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=vk_space) and (Shift=[]) then
  begin
    DoListBookmarksNavigate;
    Key:= 0;
    Exit
  end;  
end;

procedure TfmMain.ecToggleFocusBookmarksExecute(Sender: TObject);
begin
  if not plOut.Visible then
  begin
    ecShowOut.Execute;
    DoBookmarksFocus;
  end
  else
  if ListBookmarks.Focused then
    FocusEditor
  else
    DoBookmarksFocus;
end;

procedure TfmMain.InitPanelsTabs;
begin
  TabsLeft:= TATTabs.Create(Self);
  TabsLeft.Parent:= plTree;
  TabsLeft.OnTabClick:= TabsLeftClick;

  TabsRight:= TATTabs.Create(Self);
  TabsRight.Parent:= plClip;
  TabsRight.OnTabClick:= TabsRightClick;

  TabsOut:= TATTabs.Create(Self);
  TabsOut.Parent:= plOut;
  TabsOut.OnTabClick:= TabsOutClick;

  TabsLeft.PopupMenu:= PopupPanelTitle;
  TabsRight.PopupMenu:= PopupPanelTitle;
  TabsOut.PopupMenu:= PopupPanelTitle;

  TabsLeft.AddTab(-1, DKLangConstW('cap_Tree'));
  TabsLeft.AddTab(-1, DKLangConstW('cap_Proj'));
  TabsLeft.AddTab(-1, DKLangConstW('cap_Tabs'));

  TabsRight.AddTab(-1, DKLangConstW('cap_Clip'));
  TabsRight.AddTab(-1, DKLangConstW('cap_Minimap'));
  TabsRight.AddTab(-1, DKLangConstW('cap_Clips'));

  TabsOut.AddTab(-1, DKLangConstW('cap_Out'));
  TabsOut.AddTab(-1, DKLangConstW('cap_FRes'));
  TabsOut.AddTab(-1, DKLangConstW('cap_Bk'));
  TabsOut.AddTab(-1, DKLangConstW('cap_Valid'));
  TabsOut.AddTab(-1, 'Log');
  TabsOut.AddTab(-1, DKLangConstW('cap_Con'));
end;

procedure TfmMain.ApplyTabOptionsTo(ATabs: TATTabs);
var
  RefTabs: TATTabs;
begin
  RefTabs:= Groups.Pages1.Tabs;
  ATabs.Align:= alBottom;
  ATabs.Font:= FFontTabs;
  ATabs.OnTabDrawBefore:= RefTabs.OnTabDrawBefore;

  ATabs.TabBottom:= true;
  ATabs.TabShowPlus:= false;
  ATabs.TabShowMenu:= false;
  ATabs.TabShowClose:= tbShowNone;
  ATabs.TabMiddleClickClose:= false;
  ATabs.TabDragEnabled:= false;

  ATabs.TabAngle:= RefTabs.TabAngle;
  ATabs.TabIndentTop:= 0;
  ATabs.TabIndentLeft:= 3;
  ATabs.TabIndentInit:= 0;
  ATabs.TabIndentText:= 3;
  ATabs.TabHeight:= RefTabs.TabHeight-2;
  ATabs.Height:= ATabs.TabHeight+1;

  ATabs.Color:= RefTabs.Color;
  ATabs.ColorBg:= RefTabs.ColorBg;
  ATabs.ColorBorderActive:= RefTabs.ColorBorderActive;
  ATabs.ColorBorderPassive:= RefTabs.ColorBorderPassive;
  ATabs.ColorTabActive:= RefTabs.ColorTabActive;
  ATabs.ColorTabPassive:= RefTabs.ColorTabPassive;
  ATabs.ColorTabOver:= RefTabs.ColorTabOver;

  ATabs.Invalidate;
end;

procedure TfmMain.TabsLeftClick(Sender: TObject);
var
  Index: Integer;
  D: TATTabData;
begin
  Index:= TabsLeft.TabIndex;
  UpdatePanelLeft(TSynTabLeft(Index));

  if Index<Ord(tbPlugin1) then
  begin
    D:= TabsLeft.GetTabData(Index);
    if D<>nil then
      plTree.Caption:= D.TabCaption;
  end
  else
    plTree.Caption:= DoPlugin_PanelCaption(Index-Ord(tbPlugin1));
end;

procedure TfmMain.TabsRightClick(Sender: TObject);
var
  D: TATTabData;
begin
  UpdatePanelRight(TSynTabRight(TabsRight.TabIndex));

  D:= TabsRight.GetTabData(TabsRight.TabIndex);
  if D<>nil then
    plClip.Caption:= D.TabCaption;
end;

procedure TfmMain.TabsOutClick(Sender: TObject);
var
  D: TATTabData;
begin
  UpdatePanelOut(TSynTabOut(TabsOut.TabIndex));

  D:= TabsOut.GetTabData(TabsOut.TabIndex);
  if D<>nil then
    plOut.Caption:= D.TabCaption;
end;

procedure TfmMain.PopupPluginsLogPopup(Sender: TObject);
begin
  with ListPLog do
  begin
    TBXItemPLogCopySel.Enabled:= SelCount>0;
    TBXItemPLogCopyAll.Enabled:= Items.Count>0;
    TBXItemPLogDelete.Enabled:= Items.Count>0;
    TBXItemPLogClear.Enabled:= Items.Count>0;
    TBXItemPLogFind.Enabled:= Items.Count>0;
    TBXItemPLogSaveAs.Enabled:= Items.Count>0;
  end;
end;

procedure TfmMain.TbxItemWinExplorerClick(Sender: TObject);
begin
  if DoPlugin_OpenPanelByName('Explorer')<0 then
    MsgBeep;
end;

procedure TfmMain.TbxItemWinFtpClick(Sender: TObject);
begin
  if DoPlugin_OpenPanelByName('FTP')<0 then
    MsgBeep;
end;

procedure TfmMain.TBXItemTabCloseRighterClick(Sender: TObject);
begin
  DoCloseTabs(tabCloseRighterThisPage, true);
end;

procedure TfmMain.TBXItemTabCloseLefterClick(Sender: TObject);
begin
  DoCloseTabs(tabCloseLefterThisPage, true);
end;

procedure TfmMain.TabMove(Sender: TObject; NFrom, NTo: Integer);
var
  ATabs: TATTabs;
  Ctl: TControl;
  NPages: Integer;
  Sw: TTabSwitcher;
begin
  ATabs:= Sender as TATTabs;
  Ctl:= ATabs.Parent;
  if Ctl is TATPages then
    NPages:= Groups.PagesIndexOf(Ctl as TATPages)
  else
    begin MsgBeep; Exit end;

  if NPages>=Low(TabSwitchers) then
    Sw:= TabSwitchers[NPages]
  else
    begin MsgBeep; Exit end;

  if NFrom=-1 then
  begin
    Sw.UpdateTabList(-1, ATabs.TabCount-1, -1);
    Sw.MoveTabInList(ATabs.TabCount-1, NTo);
  end
  else
  if NTo=-1 then
    Sw.UpdateTabList(-1, -1, NFrom)
  else
    Sw.MoveTabInList(NFrom, NTo);
end;

procedure TfmMain.DoPyUpdateEvents(const APluginName, AEventStr, ALexersStr: string);
var
  i, N: Integer;
begin
  //find index of plugin (get first empty index if not listed)
  N:= -1;
  for i:= Low(FPluginsEvent) to High(FPluginsEvent) do
    with FPluginsEvent[i] do
      if (SFilename=APluginName) or (SFilename='') then
        begin N:= i; Break end;
  if N<0 then Exit;

  //update record
  with FPluginsEvent[N] do
  begin
    if SFilename='' then
      SFilename:= APluginName;
    DoPyStringToEvents(AEventStr, Events, SKeycodes);
    SLexers:= ALexersStr;
  end;
end;

function TfmMain.GetUntitledString: Widestring;
begin
  Result:= DKLangConstW('unnamed')+IntToStr(FLastUntitled);
  Inc(FLastUntitled);
end;

function TfmMain.GetEditorByIndex(APagesIndex, ATabIndex, AMasterIndex: Integer): TSyntaxMemo;
var
  F: TEditorFrame;
  D: TATTabData;
  ATabs: TATTabs;
begin
  Result:= nil;

  if APagesIndex=-1 then
    APagesIndex:= Groups.PagesIndexOf(Groups.PagesCurrent);
  if APagesIndex=-1 then
    Exit;

  ATabs:= Groups.Pages[APagesIndex].Tabs;
  if ATabIndex=-1 then
    ATabIndex:= ATabs.TabIndex;

  D:= ATabs.GetTabData(ATabIndex);
  if D<>nil then
  begin
    F:= D.TabObject as TEditorFrame;
    case AMasterIndex of
      1: Result:= F.EditorMaster;
      2: Result:= F.EditorSlave;
      else
        begin
          if F.IsMasterFocused then
            Result:= F.EditorMaster
          else
            Result:= F.EditorSlave;
        end;
    end;
  end;
end;

procedure TfmMain.GetEditorIndexes(Ed: TSyntaxMemo;
  var AGroupIndex, ATabIndex: Integer);
begin
  Groups.PagesAndTabIndexOfControl(FrameOfEditor(Ed), AGroupIndex, ATabIndex);
end;


procedure TfmMain.DoAddKeymappingCommand(const ACommand: Integer;
  const ACategory, ACaption, AHotkey: Widestring);
var
  S, SItem: Widestring;
begin
  SyntKeyMapping.Add(ACommand, ACategory, '', ACaption);
  with SyntKeyMapping do
    with Items[Items.Count-1] do
    begin
      S:= AHotkey;
      with KeyStrokes.Add.KeyDefs do
      repeat
        SItem:= SGetItem(S, '|');
        if SItem='' then Break;
        Add.ShortCut:= TextToShortCut(SItem);
      until false;  
    end;  
end;

function TfmMain.DoConfirmMaybeBinaryFile(const fn: Widestring): boolean;
begin
  if (opTextOnly<>cBinaryAlwaysOpen) and (not IsFileText(fn)) then
    Result:= MsgConfirmBinary(fn, Handle)
  else
    Result:= true;
end;


procedure TfmMain.DoEnumProjFiles(L: TTntStringList);
var
  i: Integer;
  fn: Widestring;
begin
  L.Clear;
  if Assigned(fmProj) then
    for i:= 0 to fmProj.TreeProj.Items.Count-1 do
    begin
      fn:= fmProj.GetFN(fmProj.TreeProj.Items[i]);
      if fn<>'' then
        L.Add(fn);
    end;
end;

procedure TfmMain.ecPreviewActionNewExecute(Sender: TObject);
begin
  LoadPrintOptions;
  with ecSyntPrinter do
  begin
    SyntMemo:= CurrentEditor;
    PrintSelection:= CurrentEditor.HaveSelection;
  end;

  DoEditorPrintPreview(ecSyntPrinter, CurrentFrame.TabCaption);
  SavePrintOptions;
end;

procedure TfmMain.TBXItemBarPreviewClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smPrintPreview);
end;

procedure TfmMain.TBXItemFPreviewClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(smPrintPreview);
end;

procedure TfmMain.acSetupLexerNewExecute(Sender: TObject);
begin
  if DoLexerPropDialog(SyntaxManager.CurrentLexer, ImgListTree) then
  begin
    SyntaxManager.Modified:= true;
    DoConfirmSaveLexLib;
  end;
end;

function TfmMain.IsProjectEmpty: boolean;
begin
  Result:= (fmProj=nil) or (fmProj.TreeProj.Items.Count<=1);
end;

procedure TfmMain.ecPageSetupActionNewExecute(Sender: TObject);
begin
  LoadPrintOptions;
  if DoConfigPrinterPage(ecSyntPrinter) then
    SavePrintOptions;
end;

procedure TfmMain.edQsKeyPress(Sender: TObject; var Key: Char);
begin
  //disable ding with Esc
  if (Key=#27) then Key:= #0;
end;

procedure TfmMain.UpdateToolbarItemAction(Item: TTBCustomItem; const SCmd: string);
var
  S: Widestring;
begin
  S:= SCmd;
  SDeleteToW(S, ':');
  case StrToIntDef(S, 0) of
    sm_OptReadOnly:        Item.Action:= ecReadOnly;
    sm_OptWrap:            Item.Action:= ecWrap;
    sm_OptShowLeftPanel:   Item.Action:= ecShowTree;
    sm_OptShowOutputPanel: Item.Action:= ecShowOut;
    sm_OptShowRightPanel:  Item.Action:= ecShowClip;
    sm_ShowFullScreen:     Item.Action:= ecFullScr;
    sm_OptRuler:           Item.Action:= ecRuler;
    sm_OptLineNums:        Item.Action:= ecLineNums;
    sm_OptFolding:         Item.Action:= ecFolding;

    sm_OptNonPrint:       Item.Action:= ecNonPrint;
    sm_OptNonPrintOff:    Item.Action:= ecNonPrintOff;
    sm_OptNonPrintSpaces: Item.Action:= ecNonPrintSpaces;
    sm_OptNonPrintEol:    Item.Action:= ecNonPrintEol;
    sm_OptNonPrintBoth:   Item.Action:= ecNonPrintBoth;
    sm_OptNonPrintEolDetails: Item.Action:= ecNonPrintEolDetails;

    sm_ToggleSmartHl:   Item.Action:= ecSmartHl;
    sm_ShowOnTop:       Item.Action:= ecOnTop;
    sm_SpellLive:       Item.Action:= ecSpellLive;
    sm_SyncScrollHorz:  Item.Action:= ecSyncScrollH;
    sm_SyncScrollVert:  Item.Action:= ecSyncScrollV;
  end;
end;


function TfmMain.GetCaretTime: Integer;
begin
  Result:= TemplateEditor.Caret.Insert.BlinkTime;
end;

procedure TfmMain.SetCaretTime(N: Integer);
begin
  TemplateEditor.Caret.Insert.BlinkTime:= N;
  ApplyEdOptions;
end;

procedure TfmMain.DoOpenFolderDialog;
var
  dir: Widestring;
begin
  dir:= '';
  if WideSelectDirectory('', '', dir) then
    DoOpenFolder(dir);
end;

procedure TfmMain.acRestartExecute(Sender: TObject);
var
  fn: string;
begin
  if not SynExe then Exit;
  fn:= ExtractFilePath(ParamStr(0))+'SynHelper.exe';
  if not FileExists(fn) then
    begin MsgNoFile(fn); Exit end;
    
  FExecute(fn, Format('restart %d', [Handle]), '', 0);
  acExit.Execute;
end;

procedure TfmMain.TBXItemTreeFindPreviewClick(Sender: TObject);
begin
  TreeFind_ShowPreview;
end;

procedure TfmMain.TbxSubmenuWebPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  LFiles: TTntStringList;
  i: Integer;
  MI: TSpTbxItem;
begin
  TbxSubmenuWeb.Clear;

  LFiles:= TTNtStringList.Create;
  try
    FFindToList(LFiles, SynDataSubdir(cSynDataWebSearch), '*.ini', '', false, false, false, false);
    for i:= 0 to LFiles.Count-1 do
    begin
      MI:= TSpTbxItem.Create(Self);
      MI.Caption:= WideChangeFileExt(WideExtractFileName(LFiles[i]), '');
      MI.OnClick:= WebSearchClick;
      TbxSubmenuWeb.Add(MI);
    end;
  finally
    FreeAndNil(LFiles);
  end;
end;

procedure TfmMain.WebSearchClick(Sender: TObject);
begin
  DoOnlineSearch_Name((Sender as TSpTbxItem).Caption);
end;

procedure TfmMain.TbxItemTabSaveClick(Sender: TObject);
begin
  if Assigned(FClickedFrame) then
    SaveFrame(FClickedFrame, false);
end;

procedure TfmMain.TbxItemTabSaveAsClick(Sender: TObject);
begin
  if Assigned(FClickedFrame) then
    SaveFrame(FClickedFrame, true);
end;

procedure TfmMain.DoPluginsManager_Install;
begin
  DoPyLoadPlugin(cPyPluginManager, 'menu_install');
end;

procedure TfmMain.DoPluginsManager_Remove;
begin
  DoPyLoadPlugin(cPyPluginManager, 'menu_remove');
end;

procedure TfmMain.DoPluginsManager_Edit;
begin
  DoPyLoadPlugin(cPyPluginManager, 'menu_edit');
end;

procedure TfmMain.DoPluginsManager_SaveAll;
begin
  DoPyLoadPlugin(cPyPluginManager, 'download_all');
end;

procedure TfmMain.DoPluginsManager_Update;
begin
  DoPyLoadPlugin(cPyPluginManager, 'update');
end;

procedure TfmMain.DoPluginsManager_Config;
begin
  DoPyLoadPlugin(cPyPluginManager, 'config');
end;

procedure TfmMain.TbxItemAddonsInstallClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddonsManager_Install);
end;

procedure TfmMain.TbxItemAddonsRemoveClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddonsManager_Remove);
end;

procedure TfmMain.TbxItemAddonsEditClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddonsManager_Edit);
end;

procedure TfmMain.TbxItemAddonsSaveClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddonsManager_SaveAll);
end;

procedure TfmMain.TbxItemAddonsUpdateClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddonsManager_Update);
end;

procedure TfmMain.TbxItemAddonsConfigClick(Sender: TObject);
begin
  CurrentEditor.ExecCommand(sm_AddonsManager_Config);
end;


function TfmMain.DoAddGutterIcon(const fn: string): Integer;
var
  Bmp: TBitmap;
begin
  Result:= -1;
  if not FileExists(fn) then Exit;

  Bmp:= TBitmap.Create;
  try
    try
      Bmp.LoadFromFile(fn);
      Bmp.Transparent:= true;
      Result:= ImgListGutter.AddMasked(Bmp, Bmp.TransparentColor);
    except
      Exit
    end;
  finally
    FreeAndNil(Bmp);
  end;
end;


function TfmMain.GetListBkmkColumns: string;
begin
  Result:= GetListviewColumnsAsString(ListBookmarks);
end;

function TfmMain.GetListTabsColumns: string;
begin
  Result:= GetListviewColumnsAsString(ListTabs);
end;

procedure TfmMain.SetListTabsColumns(const S: string);
begin
  SetListviewColumnsFromString(ListTabs, S);
end;

procedure TfmMain.SetListBkmkColumns(const S: string);
begin
  SetListviewColumnsFromString(ListBookmarks, S);
end;


procedure TfmMain.UpdateBusyIco;
begin
  DoHint('');
end;

procedure TfmMain.acMacroRecordAfterExecute(Sender: TObject);
begin
  UpdateBusyIco;
end;

procedure TfmMain.UpdateMenuDialogBorder(AForm: TForm);
var
  Frame: TEditorFrame;
  P: TPoint;
begin
  Frame:= CurrentFrame;
  P.Y:= 0;
  P.X:= Frame.Width div 2 - opShowMenuSizeX div 2;
  P:= Frame.ClientToScreen(P);

  AForm.BorderStyle:= bsNone;
  AForm.Left:= P.X;
  AForm.Top:= P.Y;
  AForm.Height:= Min(Frame.Height, opShowMenuSizeY);
  AForm.Width:= opShowMenuSizeX;

  //consider curr window size
  AForm.Left:= Min(AForm.Left, Application.MainForm.Left+Application.MainForm.Width-AForm.Width);
  AForm.Left:= Max(Max(AForm.Left, Application.MainForm.Left), 0);
end;

initialization
  unProcPy.PyEditor:= MainPyEditor;

end.

