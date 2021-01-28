{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

{$I atsynedit_defines.inc}

unit ATSynEdit;

interface

uses
  {$ifdef Windows}
  Windows, Messages,
  ATSynEdit_Adapter_IME,
  {$endif}
  InterfaceBase,
  Classes, SysUtils, Graphics,
  Controls, ExtCtrls, Menus, Forms, Clipbrd,
  LMessages, LCLType, LCLVersion,
  LazUTF8,
  EncConv,
  {$ifdef use_bg}
  BGRABitmap,
  BGRABitmapTypes,
  {$endif}
  ATStringProc,
  ATStringProc_Separator,
  ATStrings,
  ATStringProc_WordJump,
  ATCanvasPrimitives,
  ATSynEdit_CharSizer,
  ATSynEdit_RegExpr,
  ATSynEdit_Colors,
  ATSynEdit_Keymap,
  ATSynEdit_LineParts,
  ATSynEdit_CanvasProc,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Gutter,
  ATSynEdit_Gutter_Decor,
  ATSynEdit_WrapInfo,
  ATSynEdit_Bookmarks,
  ATSynEdit_Ranges,
  ATSynEdit_DimRanges,
  ATSynEdit_Gaps,
  ATSynEdit_Hotspots,
  ATSynEdit_Micromap,
  ATSynEdit_Adapters,
  ATSynEdit_Adapter_Cache,
  ATSynEdit_LinkCache,
  ATSynEdit_FGL,
  ATScrollBar;


type
  TATTokenKind = (
    atkOther,
    atkComment,
    atkString
    );

  TATSynEditScrollStyle = (
    aessHide,
    aessShow,
    aessAuto
    );

  TATMiddleClickAction = (
    mcaNone,
    mcaScrolling,
    mcaPaste,
    mcaGotoDefinition
    );

  TATPosDetails = record
    EndOfWrappedLine: boolean;
    OnGapItem: TATGapItem;
    OnGapPos: TPoint;
  end;

  TATMouseDoubleClickAction = (
    cMouseDblClickNone,
    cMouseDblClickSelectWordChars,
    cMouseDblClickSelectAnyChars,
    cMouseDblClickSelectEntireLine
    );

  TATMouseActionId = (
    cMouseActionNone,
    cMouseActionClickSimple,
    cMouseActionClickRight,
    cMouseActionClickAndSelNormalBlock,
    cMouseActionClickAndSelVerticalBlock,
    cMouseActionClickMiddle,
    cMouseActionMakeCaret,
    cMouseActionMakeCaretsColumn
    );

  TATMouseActionRecord = record
    MouseState: TShiftState;
    MouseActionId: TATMouseActionId;
  end;

  TATMouseActions = array of TATMouseActionRecord;

  TATDirection = (
    cDirNone,
    cDirLeft,
    cDirRight,
    cDirUp,
    cDirDown
    );

  TATRulerNumeration = (
    cRulerNumeration_0_10_20,
    cRulerNumeration_1_11_21,
    cRulerNumeration_1_10_20
    );

  TATSelectColumnDirection = (
    cDirColumnLeft,
    cDirColumnRight,
    cDirColumnUp,
    cDirColumnDown,
    cDirColumnPageUp,
    cDirColumnPageDown
    );

  TATScrollbarsArrowsKind = (
    cScrollArrowsNormal,
    cScrollArrowsHidden,
    cScrollArrowsAbove,
    cScrollArrowsBelow,
    cScrollArrowsCorner
    );

  TATCaseConvert = (
    cCaseLower,
    cCaseUpper,
    cCaseTitle,
    cCaseInvert,
    cCaseSentence
    );

  TATCommandResult = (
    cResultText,             //Text was changed.
    cResultFoldChange,       //Folding range(s) were changed or folded/unfolded.
    cResultCaretAny,         //Caret(s) pos/selection was changed. Don't scroll to caret.
    cResultCaretLeft,        //Caret(s) pos/selection was changed. Scroll to the most left caret.
    cResultCaretTop,         //Caret(s) pos/selection was changed. Scroll to the first caret.
    cResultCaretRight,       //Caret(s) pos/selection was changed. Scroll to the most right caret.
    cResultCaretBottom,      //Caret(s) pos/selection was changed. Scroll to the last caret.
    cResultCaretLazy,  //Additional to CaretLeft/CaretRight/CaretTop/CaretBottom, scrolls only if no carets are left in visible area.
    cResultKeepColumnSel,    //Restore previous column selection, if command changed it.
    cResultScroll,           //Some scrolling was made.
    cResultUndoRedo,         //Undo or Redo action was made.
    cResultState             //Some properties of editor were changed (e.g. word-wrap state).
    );
  TATCommandResults = set of TATCommandResult;

  TATGapCoordAction = (
    cGapCoordIgnore,
    cGapCoordToLineEnd,
    cGapCoordMoveDown
    );

  TATGutterIconsKind = (
    cGutterIconsPlusMinus,
    cGutterIconsTriangles
    );

  TATPasteCaret = (
    cPasteCaretNoChange,
    cPasteCaretLeftBottom,
    cPasteCaretRightBottom,
    cPasteCaretRightTop,
    cPasteCaretColumnLeft,
    cPasteCaretColumnRight
    );

  TATFoldStyle = ( //affects folding of blocks without "text hint" passed from adapter
    cFoldHereWithDots, //show "..." from fold-pos
    cFoldHereWithTruncatedText, //show truncated line instead of "..."
    cFoldFromEndOfLine, //looks like Lazarus: show "..." after line, bad with 2 blocks starting at the same line
    cFoldFromEndOfLineAlways, //same, even if HintText not empty
    cFoldFromNextLine //looks like SynWrite: don't show "...", show separator line
    );

  TATFoldRangeCmd = (
    cFoldingFold,
    cFoldingUnfold,
    cFoldingToggle
    );

  TATStapleEdge = (
    cStapleEdgeNone,
    cStapleEdgeAngle,
    cStapleEdgeLine
    );

type
  TATAutoIndentKind = (
    cIndentAsPrevLine,
    cIndentSpacesOnly,
    cIndentTabsAndSpaces,
    cIndentTabsOnly,
    cIndentToOpeningBracket
    );

  TATPageUpDownSize = (
    cPageSizeFull,
    cPageSizeFullMinus1,
    cPageSizeHalf
    );

  TATSynWrapMode = (
    cWrapOff,
    cWrapOn,
    cWrapAtMargin,
    cWrapAtWindowOrMargin
    );

  TATSynNumbersStyle = (
    cNumbersAll,
    cNumbersNone,
    cNumbersEach10th,
    cNumbersEach5th,
    cNumbersRelative
    );

  TATSynPaintFlag = (
    cPaintUpdateBitmap,
    cPaintUpdateCaretsCoords
    );
  TATSynPaintFlags = set of TATSynPaintFlag;

  { TATSynScrollInfo }

  TATSynScrollInfo = record
    Vertical: boolean;
    NMax: integer;
    NPage: integer;
    NPos: integer;
    NPosLast: integer;
    NPixelOffset: integer;
    SmoothCharSize: integer;
    SmoothMax: integer;
    SmoothPage: integer;
    SmoothPos: integer;
    SmoothPosLast: integer;
    procedure Clear;
    procedure SetZero; inline;
    procedure SetLast; inline;
    function TopGapVisible: boolean; inline;
    function TotalOffset: integer; inline;
    class operator =(const A, B: TATSynScrollInfo): boolean;
  end;

type
  { TATCaretProps }

  TATCaretProps = class
  public
    //Value>=0: in pixels
    //Value<0: in percents
    //Value<-100: caret is bigger than cell and overlaps nearest cells
    Width: integer;
    Height: integer;
    EmptyInside: boolean;
    procedure Assign(Obj: TATCaretProps);
  end;

const
  cMaxIndentVert = 100;
  cInitUndoLimit = 5000;
  cInitUndoMaxCarets = cInitUndoLimit;
  cInitUndoIndentVert = 15;
  cInitUndoIndentHorz = 20;
  cInitScrollbarHorzAddSpace = 2;
  cInitIdleInterval = 0; //1000; //0 dont fire OnIdle, faster
  cInitTextOffsetFromLine = {$ifdef windows} 0 {$else} 1 {$endif};
  cInitCaretsPrimitiveColumnSelection = true;
  cInitCaretsMultiToColumnSel = true;
  cInitWrapMode = cWrapOff;
  cInitWrapEnabledForMaxLines = 60*1000;
  cInitSpacingText = 1;
  cInitCaretBlinkTime = 600;
  cInitTimerAutoScroll = 100;
  cInitTimerNiceScroll = 100;
  cInitMinimapVisible = false;
  cInitMinimapSelColorChange = 6; //how much minimap sel-rect is darker, in %
  cInitMinimapTooltipVisible = true;
  cInitMinimapTooltipLinesCount = 6;
  cInitMinimapTooltipWidthPercents = 60;
  cInitMicromapVisible = false;
  cInitShowMouseSelFrame = true;
  cInitMarginRight = 80;
  cInitTabSize = 8;
  cInitMinimapWidth = 160;
  cInitNumbersStyle = cNumbersEach5th;
  cInitNumbersIndentPercents = 60;
  cInitBitmapWidth = 1000;
  cInitBitmapHeight = 800;
  cInitGutterPlusSize = 4;
  cInitMarkerSize = 30;
  cInitFoldStyle = cFoldHereWithTruncatedText;
  cInitFoldUnderlineOffset = 3;
  cInitFoldTooltipVisible = true;
  cInitFoldTooltipLineCount = 15;
  cInitFoldTooltipWidthPercents = 80;
  cInitMaxLineLenToTokenize = 4000;
  cInitMinLineLenToCalcURL = 4;
  cInitMaxLineLenToCalcURL = 300;
  cInitStapleHiliteAlpha = 180;
  cInitZebraAlphaBlend = 235;

  cGutterBands = 6;
  cGutterSizeBm = 16;
  cGutterSizeNum = 10;
  cGutterSizeFold = 14;
  cGutterSizeState = 3;
  cGutterSizeSep = 1;
  cGutterSizeEmpty = 2;

const
  cFoldedLenOfEmptyHint = 50;
  cFoldedMarkIndentInner = 2; //indent inside [...] folded-mark
  cFoldedMarkIndentOuter = 2; //indent before [...] folded-mark
  cSpeedScrollAutoHorz: integer = 10; //auto-scroll (drag out of control): speed x
  cSpeedScrollAutoVert: integer = 1; //... speed y
  cSpeedScrollNice: integer = 3;
  cSizeGutterFoldLineDx = 3;
  cSizeRulerHeightPercents = 120;
  cSizeRulerMarkSmall = 3;
  cSizeRulerMarkBig = 7;
  cSizeRulerMarkCaret = 1;
  cSizeIndentTooltipX = 5;
  cSizeIndentTooltipY = 1;
  cMinFontSize = 6;
  cMinTabSize = 1;
  cMaxTabSize = 64;
  cMinMinimapWidth = 30;
  cMaxCharsForOutput = 1000; //don't paint more chars in line
  cMinWrapColumn = 20; //too small width won't give smaller wrap-column
  cMinWrapColumnAbs = 4; //absolute min of wrap-column (leave n chars on line anyway)
  cMinMarginRt = 20;
  cMinCaretTime = 300;
  cMaxCaretTime = 2000;
  cMinCharsAfterAnyIndent = 20; //if indent is too big, leave 20 chrs in wrapped-parts anyway
  cMaxLinesForOldWrapUpdate = 100; //if less lines, force old wrapinfo update (fast)
  cHintScrollDx = 5;
  cHintBookmarkDx = 6;
  cHintBookmarkDy = 16;
  cUrlMarkerTag = -100;

  cUrlRegex_Email = '\b(mailto:)?\w[\w\-\.]*@\w[\w\-\.]*\.\w{2,}\b';
  cUrlRegex_WebBegin = 'https?://|ftp://|magnet:\?|www\.|ftp\.';
  cUrlRegex_WebSite = '\w[\w\-\.@]*(:\d+)?'; // @ for password; :\d+ is port
  cUrlRegex_WebAnchor = '(\#[\w\-]*)?';
  cUrlRegex_WebParams = '(\?[^<>''"\s]+)?';
  cUrlRegex_Web =
    '\b(' + cUrlRegex_WebBegin + ')'
    + cUrlRegex_WebSite
    + '(/[~\w\.\-\+\/%]*)?' //folders
    + cUrlRegex_WebParams
    + cUrlRegex_WebAnchor;
  cUrlRegexInitial = cUrlRegex_Email + '|' + cUrlRegex_Web;

  cTextEditorLocked: string = 'Wait...';
  cTextHintScrollPrefix: string = 'Line';

  cStrMenuitemFoldAll: string = 'Fold all';
  cStrMenuitemUnfoldAll: string = 'Unfold all';
  cStrMenuitemFoldLevel: string = 'Fold level';
  cStrMenuitemCut: string = 'Cut';
  cStrMenuitemCopy: string = 'Copy';
  cStrMenuitemPaste: string = 'Paste';
  cStrMenuitemDelete: string = 'Delete';
  cStrMenuitemSelectAll: string = 'Select all';
  cStrMenuitemUndo: string = 'Undo';
  cStrMenuitemRedo: string = 'Redo';

var
  cRectEmpty: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  ATClipboardColumnFormat: TClipboardFormat = 0; //must be inited
  ATClipboardColumnSignature: integer = $1000;

type
  TATSynEditClickEvent = procedure(Sender: TObject; var AHandled: boolean) of object;
  TATSynEditClickMoveCaretEvent = procedure(Sender: TObject; APrevPnt, ANewPnt: TPoint) of object;
  TATSynEditClickGapEvent = procedure(Sender: TObject; AGapItem: TATGapItem; APos: TPoint) of object;
  TATSynEditCommandEvent = procedure(Sender: TObject; ACommand: integer; const AText: string; var AHandled: boolean) of object;
  TATSynEditCommandAfterEvent = procedure(Sender: TObject; ACommand: integer; const AText: string) of object;
  TATSynEditClickGutterEvent = procedure(Sender: TObject; ABand: integer; ALineNum: integer) of object;
  TATSynEditClickMicromapEvent = procedure(Sender: TObject; AX, AY: integer) of object;
  TATSynEditClickLinkEvent = procedure(Sender: TObject; const ALink: string) of object;
  TATSynEditDrawBookmarkEvent = procedure(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect) of object;
  TATSynEditDrawRectEvent = procedure(Sender: TObject; C: TCanvas; const ARect: TRect) of object;
  TATSynEditDrawGapEvent = procedure(Sender: TObject; C: TCanvas; const ARect: TRect; AGap: TATGapItem) of object;
  TATSynEditCalcBookmarkColorEvent = procedure(Sender: TObject; ABookmarkKind: integer; var AColor: TColor) of object;
  TATSynEditCalcStapleEvent = procedure(Sender: TObject; ALine, AIndent: integer; var AStapleColor: TColor) of object;
  TATSynEditCalcHiliteEvent = procedure(Sender: TObject; var AParts: TATLineParts;
    ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor) of object;
  TATSynEditPasteEvent = procedure(Sender: TObject; var AHandled: boolean;
    AKeepCaret, ASelectThen: boolean) of object;
  TATSynEditHotspotEvent = procedure(Sender: TObject; AHotspotIndex: integer) of object;
  TATSynEditCheckInputEvent = procedure(Sender: TObject; AChar: WideChar; var AllowInput: boolean) of object;

type
  { TATFoldedMark }

  TATFoldedMark = record
  public
    Coord: TRect;
    LineFrom, LineTo: integer;
    procedure Init(const ACoord: TRect; ALineFrom, ALineTo: integer);
    procedure InitNone;
    function IsInited: boolean;
    class operator =(const a, b: TATFoldedMark): boolean;
  end;

  { TATFoldedMarks }

  TATFoldedMarks = class(specialize TFPGList<TATFoldedMark>)
  public
    function FindByCoord(ACoord: TPoint): TATFoldedMark;
  end;

  TATEditorTempOptions = record
    FontSize: integer;
    WrapMode: TATSynWrapMode;
    ShowMinimap: boolean;
    ShowMicromap: boolean;
    ShowRuler: boolean;
    ShowNumbers: boolean;
    ShowFolding: boolean;
    ShowUnprinted: boolean;
    UnprintedSpaces: boolean;
    UnprintedSpacesTrail: boolean;
    UnprintedSpacesInSel: boolean;
    UnprintedEnds: boolean;
    UnprintedEndsDetails: boolean;
  end;

type
  { TATSynEdit }

  TATSynEdit = class(TCustomControl)
  private
    FFontItalic: TFont;
    FFontBold: TFont;
    FFontBoldItalic: TFont;
    FTimersEnabled: boolean;
    FTimerIdle: TTimer;
    FTimerBlink: TTimer;
    FTimerScroll: TTimer;
    FTimerNiceScroll: TTimer;
    FPaintFlags: TATSynPaintFlags;
    FPaintLocked: integer;
    FBitmap: TBitmap;
    FKeymap: TATKeymap;
    FKeymapHistory: TATKeyArray;
    //FBrotherEditor: TATSynEdit;
    FWantTabs: boolean;
    FWantReturns: boolean;
    FEditorIndex: integer;
    FMarginRight: integer;
    FMarginList: array of integer;
    FStringsInt: TATStrings;
    FStringsExternal: TATStrings;
    FTabHelper: TATStringTabHelper;
    FAdapterHilite: TATAdapterHilite;
    FAdapterCache: TATAdapterHiliteCache;
    FAdapterIME: TATAdapterIME;
    FFold: TATSynRanges;
    FFoldImageList: TImageList;
    FFoldStyle: TATFoldStyle;
    FFoldUnderlineOffset: integer;
    FFoldEnabled: boolean;
    FFoldTooltipVisible: boolean;
    FFoldTooltipWidthPercents: integer;
    FFoldTooltipLineCount: integer;
    FCursorText: TCursor;
    FCursorColumnSel: TCursor;
    FCursorGutterBookmark: TCursor;
    FCursorGutterNumbers: TCursor;
    FCursorMinimap: TCursor;
    FCursorMicromap: TCursor;
    FTextOffset: TPoint;
    FTextHint: string;
    FTextHintFontStyle: TFontStyles;
    FTextHintCenter: boolean;
    FSel: TATCaretSelections;
    FSelRect: TRect;
    FSelRectBegin: TPoint;
    FSelRectEnd: TPoint;
    FCarets: TATCarets;
    FCaretShowEnabled: boolean;
    FCaretShown: boolean;
    FCaretBlinkEnabled: boolean;
    FCaretBlinkTime: integer;
    FCaretPropsNormal: TATCaretProps;
    FCaretPropsOverwrite: TATCaretProps;
    FCaretPropsReadonly: TATCaretProps;
    FCaretVirtual: boolean;
    FCaretSpecPos: boolean;
    FCaretStopUnfocused: boolean;
    FCaretHideUnfocused: boolean;
    FCaretAllowNextBlink: boolean;
    FIsEntered: boolean;
    FMarkers: TATMarkers;
    FAttribs: TATMarkers;
    FMarkedRange: TATMarkers;
    FDimRanges: TATDimRanges;
    FHotspots: TATHotspots;
    FRegexLinks: TRegExpr;
    FLinkCache: TATLinkCache;
    FFileName: string;
    FMenuStd: TPopupMenu;
    FMenuText: TPopupMenu;
    FMenuGutterBm: TPopupMenu;
    FMenuGutterNum: TPopupMenu;
    FMenuGutterFold: TPopupMenu;
    FMenuGutterFoldStd: TPopupMenu;
    FMenuMinimap: TPopupMenu;
    FMenuMicromap: TPopupMenu;
    FMenuRuler: TPopupMenu;
    MenuitemTextCut: TMenuItem;
    MenuitemTextCopy: TMenuItem;
    MenuitemTextPaste: TMenuItem;
    MenuitemTextDelete: TMenuItem;
    MenuitemTextSelAll: TMenuItem;
    MenuitemTextUndo: TMenuItem;
    MenuitemTextRedo: TMenuItem;
    FOverwrite: boolean;
    FHintWnd: THintWindow;
    FMouseDownCoordOriginal: TPoint;
    FMouseDownCoord: TPoint;
    FMouseDragCoord: TPoint;
    FMouseDownPnt: TPoint;
    FMouseDownGutterLineNumber: integer;
    FMouseDownOnMinimap: boolean;
    FMouseDownDouble: boolean;
    FMouseDownWithCtrl: boolean;
    FMouseDownWithAlt: boolean;
    FMouseDownWithShift: boolean;
    FMouseNiceScrollPos: TPoint;
    FMouseDragDropping: boolean;
    FMouseDragDroppingReal: boolean;
    FMouseDragMinimap: boolean;
    FMouseDragMinimapDelta: integer;
    FMouseDragMinimapSelHeight: integer;
    FMouseDownAndColumnSelection: boolean;
    FMouseAutoScroll: TATDirection;
    FMouseActions: TATMouseActions;
    FLastHotspot: integer;
    FLastTextCmd: integer;
    FLastTextCmdText: atString;
    FLastCommandChangedText: boolean;
    FLastCommandChangedText2: boolean;
    FLastCommandMakesColumnSel: boolean;
    FLastLineOfSlowEvents: integer;
    FLineTopTodo: integer;
    FIsCaretShapeChangedFromAPI: boolean;
    FIsReadOnlyChanged: boolean;
    FIsReadOnlyAutodetected: boolean;
    FIsRunningCommand: boolean;
    FCursorOnMinimap: boolean;
    FCursorOnGutter: boolean;
    FOnCheckInput: TATSynEditCheckInputEvent;
    FOnBeforeCalcHilite: TNotifyEvent;
    FOnClickDbl,
    FOnClickTriple,
    FOnClickMiddle: TATSynEditClickEvent;
    FOnClickMoveCaret: TATSynEditClickMoveCaretEvent;
    FOnClickGap: TATSynEditClickGapEvent;
    FOnClickEndSelect: TATSynEditClickMoveCaretEvent;
    FOnClickLink: TATSynEditClickLinkEvent;
    FOnIdle: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnChangeState: TNotifyEvent;
    FOnChangeCaretPos: TNotifyEvent;
    FOnChangeModified: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FOnClickGutter: TATSynEditClickGutterEvent;
    FOnClickMicromap: TATSynEditClickMicromapEvent;
    FOnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent;
    FOnDrawGap: TATSynEditDrawGapEvent;
    FOnDrawLine: TATSynEditDrawLineEvent;
    FOnDrawMicromap: TATSynEditDrawRectEvent;
    FOnDrawEditor: TATSynEditDrawRectEvent;
    FOnDrawRuler: TATSynEditDrawRectEvent;
    FOnCommand: TATSynEditCommandEvent;
    FOnCommandAfter: TATSynEditCommandAfterEvent;
    FOnCalcHilite: TATSynEditCalcHiliteEvent;
    FOnCalcStaple: TATSynEditCalcStapleEvent;
    FOnCalcBookmarkColor: TATSynEditCalcBookmarkColorEvent;
    FOnCalcTabSize: TATStringTabCalcEvent;
    FOnPaste: TATSynEditPasteEvent;
    FOnHotspotEnter: TATSynEditHotspotEvent;
    FOnHotspotExit: TATSynEditHotspotEvent;
    FWrapInfo: TATWrapInfo;
    FWrapTemps: TATWrapItems;
    FWrapColumn: integer;
    FWrapMode: TATSynWrapMode;
    FWrapUpdateNeeded: boolean;
    FWrapIndented: boolean;
    FWrapAddSpace: integer;
    FWrapEnabledForMaxLines: integer;
    FUnprintedVisible,
    FUnprintedSpaces,
    FUnprintedSpacesTrailing,
    FUnprintedSpacesBothEnds,
    FUnprintedSpacesOnlyInSelection,
    FUnprintedEof,
    FUnprintedEnds,
    FUnprintedEndsDetails: boolean;
    FPrevVisibleColumns: integer;
    FPrevModified: boolean;
    FCharSize: TPoint;
    FCharSizeMinimap: TPoint;
    FCharSpacingText: TPoint;
    FTabSize: integer;
    FGutter: TATGutter;
    FGutterDecor: TATGutterDecor;
    FGutterDecorImages: TImageList;
    FGutterBandBookmarks: integer;
    FGutterBandNumbers: integer;
    FGutterBandStates: integer;
    FGutterBandFolding: integer;
    FGutterBandSeparator: integer;
    FGutterBandEmpty: integer;
    FGutterBandDecor: integer;
    FColors: TATSynEditColors;
    FColorFont: TColor;
    FColorBG: TColor;
    FRulerHeight: integer;
    FNumbersIndent: integer;
    FRectMain,
    FRectMainVisible,
    FRectMinimap,
    FRectMicromap,
    FRectGutter,
    FRectRuler: TRect;
    FClientW: integer; //saved on Paint, to avoid calling Controls.ClientWidth/ClientHeight
    FClientH: integer;
    FLineBottom: integer;
    FLineParts: TATLineParts; //size is huge, so not local var
    FLineOtherParts: TATLineParts; //size is huge, so not local var
    FScrollVert,
    FScrollHorz,
    FScrollVertMinimap,
    FScrollHorzMinimap: TATSynScrollInfo;
    FScrollbarVert,
    FScrollbarHorz: TATScrollbar;
    FScrollbarLock: boolean;
    FPrevHorz,
    FPrevVert: TATSynScrollInfo;
    FMinimapWidth: integer;
    FMinimapCharWidth: integer;
    FMinimapCustomScale: integer;
    FMinimapVisible: boolean;
    FMinimapShowSelBorder: boolean;
    FMinimapShowSelAlways: boolean;
    FMinimapSelColorChange: integer;
    FMinimapAtLeft: boolean;
    FMinimapTooltipVisible: boolean;
    FMinimapTooltipLinesCount: integer;
    FMinimapTooltipWidthPercents: integer;
    FMinimapTooltip: TPanel;
    FMinimapCachedPainting: boolean;
    FMinimapHiliteLinesWithSelection: boolean;
    FMicromap: TATMicromap;
    FMicromapVisible: boolean;
    FMicromapScaleDiv: integer;
    FFoldedMarkList: TATFoldedMarks;
    FFoldedMarkCurrent: TATFoldedMark;
    FFoldedMarkTooltip: TPanel;
    FPaintCounter: integer;
    FPaintStarted: boolean;
    {$ifdef debug_fps}
    FTickMinimap: QWord;
    FTickAll: QWord;
    FMissMain: integer;
    FMissMinimap: integer;
    {$endif}
    FShowOsBarVert: boolean;
    FShowOsBarHorz: boolean;
    {$ifdef use_bg}
    FFastBmp: TBGRABitmap;
    {$endif}

    //these options are implemented in CudaText, they are dummy here
    FOptAutoCloseBrackets: string;
    FOptAutocompleteAutoshowCharCount: integer;
    FOptAutocompleteTriggerChars: string;
    FOptAutocompleteCommitChars: string;
    FOptAutocompleteCloseChars: string;
    FOptAutocompleteAddOpeningBracket: boolean;
    FOptAutocompleteUpDownAtEdge: integer;
    FOptAutocompleteCommitIfSingleItem: boolean;

    //options
    FOptScaleFont: integer;
    FOptIdleInterval: integer;
    FOptPasteAtEndMakesFinalEmptyLine: boolean;
    FOptPasteMultilineTextSpreadsToCarets: boolean;
    FOptMaxLineLenToTokenize: integer;
    FOptMinLineLenToCalcURL: integer;
    FOptMaxLineLenToCalcURL: integer;
    FOptMaxLinesToCountUnindent: integer;
    FOptScrollStyleVert: TATSynEditScrollStyle;
    FOptScrollStyleHorz: TATSynEditScrollStyle;
    FOptScrollSmooth: boolean;
    FOptScrollIndentCaretHorz: integer; //offsets for caret-moving: if caret goes out of control
    FOptScrollIndentCaretVert: integer; //must be 0, >0 gives jumps on move-down
    FOptUndoLimit: integer;
    FOptUndoIndentVert: integer;
    FOptUndoIndentHorz: integer;
    FOptScrollbarsNew: boolean;
    FOptScrollbarHorizontalAddSpace: integer;
    FOptScrollLineCommandsKeepCaretOnScreen: boolean;
    FOptShowFontLigatures: boolean;
    FOptShowURLs: boolean;
    FOptShowURLsRegex: string;
    FOptShowDragDropMarker: boolean;
    FOptStapleStyle: TATLineStyle;
    FOptStapleIndent: integer;
    FOptStapleWidthPercent: integer;
    FOptStapleHiliteActive: boolean;
    FOptStapleHiliteActiveAlpha: integer;
    FOptStapleEdge1: TATStapleEdge;
    FOptStapleEdge2: TATStapleEdge;
    FOptStapleIndentConsidersEnd: boolean;
    FOptMouseEnableAll: boolean;
    FOptMouseEnableNormalSelection: boolean;
    FOptMouseEnableColumnSelection: boolean;
    FOptMouseColumnSelectionWithoutKey: boolean;
    FOptCaretsPrimitiveColumnSelection: boolean;
    FOptCaretsAddedToColumnSelection: boolean;
    FOptCaretPreferLeftSide: boolean;
    FOptCaretPosAfterPasteColumn: TATPasteCaret;
    FOptCaretFixAfterRangeFolded: boolean;
    FOptCaretsMultiToColumnSel: boolean;
    FOptMarkersSize: integer;
    FOptShowScrollHint: boolean;
    FOptTextCenteringCharWidth: integer;
    FOptTextOffsetLeft: integer;
    FOptTextOffsetTop: integer;
    FOptTextOffsetFromLine: integer;
    FOptSavingForceFinalEol: boolean;
    FOptSavingTrimSpaces: boolean;
    FOptSavingTrimFinalEmptyLines: boolean;
    FOptUndoGrouped: boolean;
    FOptUndoMaxCarets: integer;
    FOptIndentSize: integer;
    FOptIndentKeepsAlign: boolean;
    FOptIndentMakesWholeLinesSelection: boolean;
    FOptBorderWidth: integer;
    FOptBorderWidthFocused: integer;
    FOptBorderFocusedActive: boolean;
    FOptRulerVisible: boolean;
    FOptRulerNumeration: TATRulerNumeration;
    FOptRulerHeightPercents: integer;
    FOptRulerFontSizePercents: integer;
    FOptRulerMarkSizeCaret: integer;
    FOptRulerMarkSizeSmall: integer;
    FOptRulerMarkSizeBig: integer;
    FOptRulerTopIndentPercents: integer;
    FOptGutterVisible: boolean;
    FOptGutterPlusSize: integer;
    FOptGutterShowFoldAlways: boolean;
    FOptGutterShowFoldLines: boolean;
    FOptGutterShowFoldLinesAll: boolean;
    FOptGutterShowFoldLinesForCaret: boolean;
    FOptGutterIcons: TATGutterIconsKind;
    FOptNumbersAutosize: boolean;
    FOptNumbersAlignment: TAlignment;
    FOptNumbersStyle: TATSynNumbersStyle;
    FOptNumbersShowFirst: boolean;
    FOptNumbersShowCarets: boolean;
    FOptNumbersSkippedChar: string;
    FOptNumbersIndentPercents: integer;
    FOptNonWordChars: atString;
    FOptAutoIndent: boolean;
    FOptAutoIndentKind: TATAutoIndentKind;
    FOptAutoIndentBetterBracketsCurly: boolean;
    FOptAutoIndentBetterBracketsRound: boolean;
    FOptAutoIndentBetterBracketsSquare: boolean;
    FOptAutoIndentRegexRule: string;
    FOptTabSpaces: boolean;
    FOptLastLineOnTop: boolean;
    FOptOverwriteSel: boolean;
    FOptOverwriteAllowedOnPaste: boolean;
    FOptKeyBackspaceUnindent: boolean;
    FOptKeyBackspaceGoesToPrevLine: boolean;
    FOptKeyPageKeepsRelativePos: boolean;
    FOptKeyUpDownNavigateWrapped: boolean;
    FOptKeyHomeEndNavigateWrapped: boolean;
    FOptKeyUpDownKeepColumn: boolean;
    FOptCopyLinesIfNoSel: boolean;
    FOptCutLinesIfNoSel: boolean;
    FOptCopyColumnBlockAlignedBySpaces: boolean;
    FOptShowFullSel: boolean;
    FOptShowFullHilite: boolean;
    FOptShowCurLine: boolean;
    FOptShowCurLineMinimal: boolean;
    FOptShowCurLineOnlyFocused: boolean;
    FOptShowCurColumn: boolean;
    FOptShowMouseSelFrame: boolean;
    FOptMouseHideCursor: boolean;
    FOptMouseClickOpensURL: boolean;
    FOptMouseClickNumberSelectsLine: boolean;
    FOptMouseClickNumberSelectsLineWithEOL: boolean;
    FOptMouse2ClickAction: TATMouseDoubleClickAction;
    FOptMouse2ClickOpensURL: boolean;
    FOptMouse2ClickDragSelectsWords: boolean;
    FOptMouse3ClickSelectsLine: boolean;
    FOptMouseDragDrop: boolean;
    FOptMouseDragDropCopying: boolean;
    FOptMouseDragDropCopyingWithState: TShiftStateEnum;
    FOptMouseRightClickMovesCaret: boolean;
    FOptMouseMiddleClickAction: TATMiddleClickAction;
    FOptMouseWheelScrollVert: boolean;
    FOptMouseWheelScrollHorz: boolean;
    FOptMouseWheelScrollVertSpeed: integer;
    FOptMouseWheelScrollHorzSpeed: integer;
    FOptMouseWheelScrollHorzWithState: TShiftStateEnum;
    FOptMouseWheelZooms: boolean;
    FOptMouseWheelZoomsWithState: TShiftStateEnum;
    FOptKeyPageUpDownSize: TATPageUpDownSize;
    FOptKeyLeftRightGoToNextLineWithCarets: boolean;
    FOptKeyLeftRightSwapSel: boolean;
    FOptKeyLeftRightSwapSelAndSelect: boolean;
    FOptKeyHomeToNonSpace: boolean;
    FOptKeyEndToNonSpace: boolean;
    FOptKeyTabIndents: boolean;
    FOptKeyTabIndentsVerticalBlock: boolean;
    FOptShowIndentLines: boolean;
    FOptShowGutterCaretBG: boolean;
    FOptAllowRepaintOnTextChange: boolean;
    FOptAllowReadOnly: boolean;
    FOptZebraActive: boolean;
    FOptZebraStep: integer;
    FOptZebraAlphaBlend: byte;

    //
    procedure ClearSelRectPoints;
    procedure ClearMouseDownVariables;
    procedure DebugSelRect;
    function DoCalcLineLen(ALineIndex: integer): integer;
    function GetAttribs: TATMarkers;
    procedure GetClientSizes(out W, H: integer);
    function GetMarkers: TATMarkers;
    function GetDimRanges: TATDimRanges;
    function GetHotspots: TATHotspots;
    function GetGutterDecor: TATGutterDecor;
    function IsCaretOnVisibleRect: boolean;
    procedure SetOptShowURLsRegex(const AValue: string);
    procedure SetShowOsBarVert(AValue: boolean);
    procedure SetShowOsBarHorz(AValue: boolean);
    procedure DebugFindWrapIndex;
    function DoCalcIndentCharsFromPrevLines(AX, AY: integer): integer;
    procedure DoCalcPosColor(AX, AY: integer; var AColor: TColor);
    procedure DoCalcLineEntireColor(ALine: integer; AUseColorOfCurrentLine: boolean; out AColor: TColor; out
      AColorForced: boolean; AHiliteLineWithSelection: boolean);
    procedure DoCaretsApplyShape(var R: TRect; Props: TATCaretProps; W, H: integer);
    procedure DoCaretsAddOnColumnBlock(APos1, APos2: TPoint; const ARect: TRect);
    procedure DoCaretsFixForSurrogatePairs(AMoveRight: boolean);
    function DoCaretsKeepOnScreen(AMoveDown: boolean): boolean;
    procedure DoCaretsOnChanged(Sender: TObject);
    procedure DoCaretsAssign(NewCarets: TATCarets);
    procedure DoCaretsShift_CaretItem(Caret: TATCaretItem; APosX, APosY, AShiftX,
      AShiftY, AShiftBelowX: integer);
    procedure DoCaretsShift_MarkerItem(AMarkerObj: TATMarkers;
      AMarkerIndex: integer; APosX, APosY, AShiftX, AShiftY,
      AShiftBelowX: integer; APosAfter: TPoint);
    procedure DoDropText(AndDeleteSelection: boolean);
    procedure DoFoldbarClick(ALine: integer);
    function DoGetFoldedMarkLinesCount(ALine: integer): integer;
    procedure DoHandleRightClick(X, Y: integer);
    function DoHandleClickEvent(AEvent: TATSynEditClickEvent): boolean;
    procedure DoHotspotsExit;
    procedure DoHintShow;
    procedure DoHintHide;
    procedure DoHintShowForBookmark(ALine: integer);
    procedure DoMenuGutterFold_AddDynamicItems(Menu: TPopupMenu);
    procedure DoMenuGutterFold;
    procedure DoMenuText;
    procedure DoMinimapClick(APosY: integer);
    procedure DoMinimapDrag(APosY: integer);
    procedure DoStringsOnChange(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: integer);
    procedure DoStringsOnProgress(Sender: TObject);
    procedure DoScroll_IndentFromBottom(AWrapInfoIndex, AIndentVert: integer);
    procedure DoScroll_IndentFromTop(AWrapInfoIndex, AIndentVert: integer); inline;
    procedure DoSelectionDeleteColumnBlock;
    function DoSelect_MultiCaretsLookLikeColumnSelection: boolean;
    procedure DoSelect_NormalSelToColumnSel(out ABegin, AEnd: TPoint);
    function _IsFocused: boolean;
    function GetEncodingName: string;
    procedure SetEncodingName(const AName: string);
    function GetGaps: TATGaps;
    function GetLastCommandChangedLines: integer;
    function GetMinimapActualHeight: integer;
    function GetMinimapSelTop: integer;
    function GetMinimap_DraggedPosToWrapIndex(APosY: integer): integer;
    function GetMinimap_ClickedPosToWrapIndex(APosY: integer): integer;
    function GetOptTextOffsetTop: integer;
    function GetRedoAsString: string;
    function GetUndoAsString: string;
    function IsFoldLineNeededBeforeWrapitem(N: integer): boolean;
    function IsRepaintNeededOnEnterOrExit: boolean;
    procedure MenuFoldFoldAllClick(Sender: TObject);
    procedure MenuFoldLevelClick(Sender: TObject);
    procedure MenuFoldUnfoldAllClick(Sender: TObject);
    procedure MenuFoldPlusMinusClick(Sender: TObject);
    procedure MinimapTooltipPaint(Sender: TObject);
    procedure FoldedMarkTooltipPaint(Sender: TObject);
    procedure FoldedMarkMouseEnter(Sender: TObject);
    procedure OnNewScrollbarHorzChanged(Sender: TObject);
    procedure OnNewScrollbarVertChanged(Sender: TObject);
    procedure DoPartCalc_CreateNew(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer; AColorBG: TColor);
    procedure DoPartCalc_ApplySelectionOver(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer);
    procedure DoPartCalc_ApplyAttribsOver(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer; AColorBG: TColor);
    function GetAutoIndentString(APosX, APosY: integer; AUseIndentRegexRule: boolean): atString;
    function GetFoldedMarkText(ALine: integer): string;
    function GetModified: boolean;
    function Unfolded_NextLineNumber(ALine: integer; ADown: boolean): integer;
    function Unfolded_FirstLineNumber: integer;
    function Unfolded_LastLineNumber: integer;
    function GetOneLine: boolean;
    function GetRedoCount: integer;
    function GetLinesFromTop: integer;
    function GetText: atString;
    function GetUndoAfterSave: boolean;
    function GetUndoCount: integer;
    procedure InitAttribs;
    procedure InitMarkers;
    procedure InitHotspots;
    procedure InitDimRanges;
    procedure InitGutterDecor;
    procedure InitMarkedRange;
    procedure InitMinimapTooltip;
    procedure InitFoldedMarkList;
    procedure InitFoldedMarkTooltip;
    procedure InitFoldImageList;
    procedure InitMenuStd;
    function IsLinePartWithCaret(ALine: integer; ACoordY: integer): boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuStdPopup(Sender: TObject);
    procedure DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer;
      AItems: TATWrapItems; AConsiderFolding: boolean);
    procedure DoCalcLineHilite(const AData: TATWrapItem;
      out AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
      AColorBG: TColor; AColorForced: boolean; var AColorAfter: TColor;
      AMainText: boolean);
    function DoScaleFont(AValue: integer): integer;
    //select
    procedure DoSelectionDeleteOrReset;
    procedure DoSelect_ExtendSelectionByLine;
    procedure DoSelect_CharRange(ACaretIndex: integer; Pnt: TPoint);
    procedure DoSelect_WordRange(ACaretIndex: integer; P1, P2: TPoint);
    procedure DoSelect_ByDoubleClick(AllowOnlyWordChars: boolean);
    procedure DoSelect_Line_ByClick;
    procedure DoSelect_ColumnBlock_MoveEndUpDown(var AX, AY: integer; ALineDelta: integer);
    function TempSel_IsSelection: boolean; inline;
    function TempSel_IsMultiline: boolean; inline;
    function TempSel_IsLineWithSelection(ALine: integer): boolean; inline;
    function TempSel_IsLineAllSelected(ALine: integer): boolean; inline;
    function TempSel_IsPosSelected(AX, AY: integer): boolean; inline;
    function TempSel_IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection; inline;
    procedure TempSel_GetRangesInLineAfterPoint(AX, AY: integer; out ARanges: TATSimpleRangeArray); inline;
    //paint
    procedure PaintEx(ALineNumber: integer);
    function DoPaint(AFlags: TATSynPaintFlags; ALineFrom: integer): boolean;
    procedure DoPaintBorder(C: TCanvas; AColor: TColor; AWidth: integer);
    procedure DoPaintAllTo(C: TCanvas; AFlags: TATSynPaintFlags; ALineFrom: integer);
    procedure DoPaintMainTo(C: TCanvas; ALineFrom: integer);
    procedure DoPaintNiceScroll(C: TCanvas);
    procedure DoPaintLineNumber(C: TCanvas; ALineIndex, ACoordTop: integer; ABand: TATGutterItem);
    procedure DoPaintMarginLineTo(C: TCanvas; AX, AWidth: integer; AColor: TColor);
    procedure DoPaintRulerTo(C: TCanvas);
    procedure DoPaintRulerCaretMark(C: TCanvas; ACaretX: integer);
    procedure DoPaintFPS(C: TCanvas);
    procedure DoPaintTextTo(C: TCanvas; const ARect: TRect;
      const ACharSize: TPoint; AWithGutter, AMainText: boolean;
      var AScrollHorz, AScrollVert: TATSynScrollInfo; ALineFrom: integer);
    procedure DoPaintTextFragmentTo(C: TCanvas; const ARect: TRect; ALineFrom,
      ALineTo: integer; AConsiderWrapInfo: boolean; AColorBG, AColorBorder: TColor);
    procedure DoPaintLineIndent(C: TCanvas; const ARect: TRect; ACharSize: TPoint;
      ACoordY: integer; AIndentSize: integer; AColorBG: TColor;
      AScrollPos: integer; AIndentLines: boolean);
    {$ifdef use_bg}
    procedure DoPaintMinimapSelTo_BGRA(C: TBGRABitmap);
    {$else}
    procedure DoPaintMinimapSelTo(C: TCanvas);
    {$endif}
    procedure DoPaintMinimapTo(C: TCanvas);
    procedure DoPaintMicromapTo(C: TCanvas);
    procedure DoPaintMarginsTo(C: TCanvas);
    procedure DoPaintGapTo(C: TCanvas; const ARect: TRect; AGap: TATGapItem);
    procedure DoPaintFoldedMark(C: TCanvas;
      APosX, APosY, ACoordX, ACoordY: integer;
      const AMarkText: string);
    procedure DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
    procedure DoPaintModeStatic;
    procedure DoPaintModeBlinking;
    procedure DoPaintSelectedLineBG(C: TCanvas; ACharSize: TPoint;
      const AVisRect: TRect; APointLeft, APointText: TPoint;
      const AWrapItem: TATWrapItem; ALineWidth: integer;
      const AScrollHorz: TATSynScrollInfo);
    procedure DoPaintMarkersTo(C: TCanvas);
    procedure DoPaintMarkerOfDragDrop(C: TCanvas);
    procedure DoPaintGutterPlusMinus(C: TCanvas; AX, AY: integer; APlus: boolean;
      ALineColor: TColor);
    procedure DoPaintGutterFolding(C: TCanvas; AWrapItemIndex: integer; ACoordX1,
      ACoordX2, ACoordY1, ACoordY2: integer);
    procedure DoPaintGutterDecor(C: TCanvas; ALine: integer; const ARect: TRect);
    procedure DoPaintGutterBandBG(C: TCanvas; ABand: integer; AColor: TColor; AY1,
      AY2: integer; AEntireHeight: boolean);
    procedure DoPaintLockedWarning(C: TCanvas);
    procedure DoPaintStaple(C: TCanvas; const R: TRect; AColor: TColor);
    procedure DoPaintStaples(C: TCanvas; const ARect: TRect; ACharSize: TPoint;
      const AScrollHorz: TATSynScrollInfo);
    procedure DoPaintTextHintTo(C: TCanvas);
    procedure DoPaintMouseSelFrame(C: TCanvas);
    //carets
    procedure DoCaretsExtend(ADown: boolean; ALines: integer);
    function GetCaretManyAllowed: boolean;
    function GetCaretSelectionIndex(P: TPoint): integer;
    function DoCaretSwapEdge(Item: TATCaretItem; AMoveLeft: boolean): boolean;
    procedure DoCaretsSort;
    //events
    procedure DoEventBeforeCalcHilite;
    procedure DoEventClickMicromap(AX, AY: integer);
    procedure DoEventClickGutter(ABandIndex, ALineNumber: integer);
    function DoEventCommand(ACommand: integer; const AText: string): boolean;
    procedure DoEventDrawBookmarkIcon(C: TCanvas; ALineNumber: integer; const ARect: TRect);
    procedure DoEventCommandAfter(ACommand: integer; const AText: string);
    //
    function GetCharSpacingY: integer;
    function GetEndOfFilePos: TPoint;
    function GetMarginString: string;
    function GetReadOnly: boolean;
    function GetLineTop: integer;
    function GetColumnLeft: integer;
    function GetTextForClipboard: string;
    function GetStrings: TATStrings;
    function GetMouseNiceScroll: boolean;
    procedure SetEnabledSlowEvents(AValue: boolean);
    procedure SetCaretBlinkEnabled(AValue: boolean);
    procedure SetFoldEnabled(AValue: boolean);
    procedure SetFontBold(AValue: TFont);
    procedure SetFontBoldItalic(AValue: TFont);
    procedure SetFontItalic(AValue: TFont);
    procedure SetLastCommandChangedLines(AValue: integer);
    procedure SetModified(AValue: boolean);
    procedure SetMouseNiceScroll(AValue: boolean);
    procedure SetCaretManyAllowed(AValue: boolean);
    procedure SetCaretBlinkTime(AValue: integer);
    procedure SetCharSpacingY(AValue: integer);
    procedure SetMarginString(const AValue: string);
    procedure SetMicromapVisible(AValue: boolean);
    procedure SetMinimapVisible(AValue: boolean);
    procedure SetOneLine(AValue: boolean);
    procedure SetReadOnly(AValue: boolean);
    procedure SetLineTop(AValue: integer);
    procedure SetColumnLeft(AValue: integer);
    procedure SetLinesFromTop(AValue: integer);
    procedure SetRedoAsString(const AValue: string);
    procedure SetStrings(Obj: TATStrings);
    procedure GetRectMain(out R: TRect);
    procedure GetRectMinimap(out R: TRect);
    procedure GetRectMinimapSel(out R: TRect);
    procedure GetRectMicromap(out R: TRect);
    procedure GetRectGutter(out R: TRect);
    procedure GetRectRuler(out R: TRect);
    function GetTextOffset: TPoint;
    function GetPageLines: integer;
    function GetMinimapScrollPos: integer;
    procedure SetTabSize(AValue: integer);
    procedure SetTabSpaces(AValue: boolean);
    procedure SetText(const AValue: atString);
    procedure SetUndoAfterSave(AValue: boolean);
    procedure SetUndoAsString(const AValue: string);
    procedure SetUndoLimit(AValue: integer);
    procedure SetWrapMode(AValue: TATSynWrapMode);
    procedure SetWrapIndented(AValue: boolean);
    procedure UpdateScrollbarVert;
    procedure UpdateScrollbarHorz;
    procedure UpdateSelRectFromPoints(const P1, P2: TPoint);
    procedure UpdateAdapterCacheSize;
    procedure UpdateInitialVars(C: TCanvas);
    procedure UpdateLinksAttribs;
    function UpdateLinksRegexObject: boolean;
    procedure UpdateTabHelper;
    procedure UpdateCursor;
    procedure UpdateGutterAutosize;
    procedure UpdateMinimapAutosize;
    procedure UpdateMinimapTooltip;
    procedure UpdateFoldedMarkTooltip;
    procedure UpdateClientSizes;
    function DoFormatLineNumber(N: integer): string;
    function UpdateScrollInfoFromMessage(var Info: TATSynScrollInfo; const Msg: TLMScroll): boolean;
    procedure UpdateCaretsCoords(AOnlyLast: boolean = false);
    function GetCharSize(C: TCanvas; ACharSpacing: TPoint): TPoint;
    function GetScrollbarVisible(bVertical: boolean): boolean;
    procedure SetMarginRight(AValue: integer);

    //timers
    procedure TimerIdleTick(Sender: TObject);
    procedure TimerBlinkTick(Sender: TObject);
    procedure TimerScrollTick(Sender: TObject);
    procedure TimerNiceScrollTick(Sender: TObject);

    //carets
    procedure DoCaretAddToPoint(AX, AY: integer);
    procedure DoCaretsColumnToPoint(AX, AY: integer);
    procedure DoCaretsDeleteOnSameLines;

    //editing
    function IsCommandResults_CaretMove(const Res: TATCommandResults): boolean;
    procedure DoCommandResults(ACmd: integer; Res: TATCommandResults);
    function DoCommand_TextInsertAtCarets(const AText: atString; AKeepCaret,
      AOvrMode, ASelectThen: boolean): TATCommandResults;
    function DoCommand_ColumnSelectWithoutKey(AValue: boolean): TATCommandResults;
    function DoCommand_FoldLevel(ALevel: integer): TATCommandResults;
    function DoCommand_FoldAll: TATCommandResults;
    function DoCommand_FoldUnAll: TATCommandResults;
    function DoCommand_FoldRangeAtCurLine(ACommand: TATFoldRangeCmd): TATCommandResults;
    function DoCommand_FoldSelection: TATCommandResults;
    function DoCommand_TextTrimSpaces(AMode: TATTrimSpaces): TATCommandResults;
    function DoCommand_TextChangeCase(AMode: TATCaseConvert): TATCommandResults;
    function DoCommand_ScaleDelta(AIncrease: boolean): TATCommandResults;
    function DoCommand_ScaleReset: TATCommandResults;
    function DoCommand_MoveSelectionUpDown(ADown: boolean): TATCommandResults;
    function DoCommand_TextInsertEmptyAboveBelow(ADown: boolean): TATCommandResults;
    function DoCommand_SelectColumnToDirection(ADir: TATSelectColumnDirection): TATCommandResults;
    function DoCommand_SelectColumnToLineEdge(AToEnd: boolean): TATCommandResults;
    function DoCommand_RemoveOneCaret(AFirstCaret: boolean): TATCommandResults;
    function DoCommand_TextInsertColumnBlockOnce(const AText: atString; AKeepCaret: boolean): TATCommandResults;
    function DoCommand_CaretsExtend(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_Undo: TATCommandResults;
    function DoCommand_Redo: TATCommandResults;
    function DoCommand_TextIndentUnindent(ARight: boolean): TATCommandResults;
    function DoCommand_TextIndentUnindent_StreamBlock(ARight: boolean): TATCommandResults;
    procedure DoCommand_TextIndentUnindent_StreamBlock_OneCaret(ARight: boolean; Caret: TATCaretItem);
    function DoCommand_TextIndentUnindent_ColumnBlock(ARight: boolean): TATCommandResults;
    function DoCommand_SelectWords: TATCommandResults;
    function DoCommand_SelectLines: TATCommandResults;
    function DoCommand_SelectAll: TATCommandResults;
    function DoCommand_SelectInverted: TATCommandResults;
    function DoCommand_SelectSplitToLines: TATCommandResults;
    function DoCommand_SelectExtendByLine: TATCommandResults;
    function DoCommand_Cancel: TATCommandResults;
    function DoCommand_ToggleReadOnly: TATCommandResults;
    function DoCommand_ToggleOverwrite: TATCommandResults;
    function DoCommand_ToggleWordWrap(AltOrder: boolean): TATCommandResults;
    function DoCommand_ToggleUnprinted: TATCommandResults;
    function DoCommand_ToggleUnprintedSpaces: TATCommandResults;
    function DoCommand_ToggleUnprintedSpacesTrailing: TATCommandResults;
    function DoCommand_ToggleUnprintedEnds: TATCommandResults;
    function DoCommand_ToggleUnprintedEndDetails: TATCommandResults;
    function DoCommand_ToggleLineNums: TATCommandResults;
    function DoCommand_ToggleFolding: TATCommandResults;
    function DoCommand_ToggleRuler: TATCommandResults;
    function DoCommand_ToggleMiniMap: TATCommandResults;
    function DoCommand_ToggleMicroMap: TATCommandResults;
    function DoCommand_GotoWord(AJump: TATWordJump; AJumpSimple: boolean=false): TATCommandResults;
    function DoCommand_GotoLineEdge(ABegin: boolean): TATCommandResults;
    function DoCommand_GotoScreenSide(ASide: TATCaretScreenSide): TATCommandResults;
    function DoCommand_ScrollToBeginOrEnd(AToBegin: boolean): TATCommandResults;
    function DoCommand_ScrollByDelta(ALines, AColumns: integer; AKeepCaretOnScreen: boolean): TATCommandResults;
    function DoCommand_TextInsertTabSpacesAtCarets(AOvrMode: boolean): TATCommandResults;
    function DoCommand_TextTabulation: TATCommandResults;
    function DoCommand_KeyHome: TATCommandResults;
    function DoCommand_KeyEnd: TATCommandResults;
    function DoCommand_KeyLeft(ASelCommand: boolean): TATCommandResults;
    function DoCommand_KeyRight(ASelCommand: boolean): TATCommandResults;
    function DoCommand_KeyUpDown(ADown: boolean; ALines: integer; AKeepRelativePos: boolean): TATCommandResults;
    function DoCommand_KeyUpDown_NextLine(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_KeyUpDown_Wrapped(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_TextBackspace: TATCommandResults;
    function DoCommand_TextDelete: TATCommandResults;
    function DoCommand_TextDeleteSelection: TATCommandResults;
    function DoCommand_TextDeleteLeft(ALen: integer; AAllowUnindent: boolean): TATCommandResults;
    function DoCommand_TextDeleteRight(ALen: integer): TATCommandResults;
    function DoCommand_TextInsertEol(AKeepCaret: boolean): TATCommandResults;
    function DoCommand_ForceFinalEndOfLine: TATCommandResults;
    function DoCommand_TextDeleteLines: TATCommandResults;
    function DoCommand_TextDuplicateLine: TATCommandResults;
    function DoCommand_TextDeleteToLineBegin: TATCommandResults;
    function DoCommand_TextDeleteToLineEnd: TATCommandResults;
    function DoCommand_TextDeleteWord(ANext: boolean): TATCommandResults;
    function DoCommand_TextDeleteWordEntire: TATCommandResults;
    function DoCommand_TextDeleteToDocumentBegin: TATCommandResults;
    function DoCommand_TextDeleteToDocumentEnd: TATCommandResults;
    function DoCommand_GotoTextBegin: TATCommandResults;
    function DoCommand_GotoTextEnd: TATCommandResults;
    function DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean;
      AClipboardObject: TClipboard): TATCommandResults;
    function DoCommand_ClipboardPasteColumnBlock(AKeepCaret: boolean;
      AClipboardObject: TClipboard): TATCommandResults;
    function DoCommand_ClipboardCopy(Append: boolean;
      AClipboardObject: TClipboard): TATCommandResults;
    function DoCommand_ClipboardCut(
      AClipboardObject: TClipboard): TATCommandResults;
    function DoCommand_Sort(AAction: TATStringsSortAction): TATCommandResults;
    function DoCommand_DeleteAllBlanks: TATCommandResults;
    function DoCommand_DeleteAdjacentBlanks: TATCommandResults;
    function DoCommand_DeleteAdjacentDups: TATCommandResults;
    function DoCommand_DeleteAllDups(AKeepBlanks: boolean): TATCommandResults;
    function DoCommand_ReverseLines: TATCommandResults;
    function DoCommand_ShuffleLines: TATCommandResults;
    //
    function GetCommandFromKey(var Key: Word; Shift: TShiftState): integer;
    function DoMouseWheelAction(Shift: TShiftState; AUp, AForceHorz: boolean): boolean;
    function GetCaretsArray: TATPointArray;
    function GetMarkersArray: TATInt64Array;
    procedure SetCaretsArray(const Ar: TATPointArray);
    procedure SetMarkersArray(const Ar: TATInt64Array);
    property MouseNiceScroll: boolean read GetMouseNiceScroll write SetMouseNiceScroll;
    property ShowOsBarVert: boolean read FShowOsBarVert write SetShowOsBarVert;
    property ShowOsBarHorz: boolean read FShowOsBarHorz write SetShowOsBarHorz;

  public
    TagString: string; //to store plugin specific data in CudaText
    InitialOptions: TATEditorTempOptions;

    //overrides
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property ClientWidth: integer read FClientW;
    property ClientHeight: integer read FClientH;
    //updates
    procedure Invalidate; override;
    procedure InvalidateHilitingCache;
    procedure InvalidateHilitingCache(ALineIndex: integer);
    procedure Update(AUpdateWrapInfo: boolean = false; AUpdateCaretsCoords: boolean = true); reintroduce;
    procedure UpdateWrapInfo(AForceUpdate: boolean=false);
    procedure UpdateFoldedFromLinesHidden;
    procedure UpdateScrollInfoFromSmoothPos(var AInfo: TATSynScrollInfo; APos: integer);
    procedure UpdateFoldLineIndexer;
    function UpdateScrollbars(AdjustSmoothPos: boolean): boolean;
    procedure DoEventCarets; virtual;
    procedure DoEventScroll; virtual;
    procedure DoEventChange(AllowOnChange: boolean=true); virtual;
    procedure DoEventState; virtual;
    procedure TimersStart;
    procedure TimersStop;
    //complex props
    property Strings: TATStrings read GetStrings write SetStrings;
    property Fold: TATSynRanges read FFold;
    property Carets: TATCarets read FCarets;
    property Markers: TATMarkers read GetMarkers;
    property Attribs: TATMarkers read GetAttribs;
    property Micromap: TATMicromap read FMicromap;
    property DimRanges: TATDimRanges read GetDimRanges;
    property Hotspots: TATHotspots read GetHotspots;
    property Gaps: TATGaps read GetGaps;
    property Keymap: TATKeymap read FKeymap write FKeymap;
    property MouseMap: TATMouseActions read FMouseActions write FMouseActions;
    property TabHelper: TATStringTabHelper read FTabHelper;
    property WrapInfo: TATWrapInfo read FWrapInfo;
    property ScrollVert: TATSynScrollInfo read FScrollVert write FScrollVert;
    property ScrollHorz: TATSynScrollInfo read FScrollHorz write FScrollHorz;
    //property BrotherEditor: TATSynEdit read FBrotherEditor write FBrotherEditor;
    property CaretPropsNormal: TATCaretProps read FCaretPropsNormal;
    property CaretPropsOverwrite: TATCaretProps read FCaretPropsOverwrite;
    property CaretPropsReadonly: TATCaretProps read FCaretPropsReadonly;
    //common
    property EncodingName: string read GetEncodingName write SetEncodingName;
    property Modified: boolean read GetModified write SetModified;
    property AdapterForHilite: TATAdapterHilite read FAdapterHilite write FAdapterHilite;
    property AdapterIME: TATAdapterIME read FAdapterIME write FAdapterIME;
    property EditorIndex: integer read FEditorIndex write FEditorIndex;
    property LineTop: integer read GetLineTop write SetLineTop;
    property LineBottom: integer read FLineBottom;
    property LinesFromTop: integer read GetLinesFromTop write SetLinesFromTop;
    property ColumnLeft: integer read GetColumnLeft write SetColumnLeft;
    property ModeOverwrite: boolean read FOverwrite write FOverwrite;
    property ModeReadOnly: boolean read GetReadOnly write SetReadOnly;
    property ModeOneLine: boolean read GetOneLine write SetOneLine;
    property UndoCount: integer read GetUndoCount;
    property RedoCount: integer read GetRedoCount;
    property UndoAsString: string read GetUndoAsString write SetUndoAsString;
    property RedoAsString: string read GetRedoAsString write SetRedoAsString;
    property Text: atString read GetText write SetText;
    property SelRect: TRect read FSelRect;
    function IsSelRectEmpty: boolean;
    function IsPosSelected(AX, AY: integer): boolean;
    function IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
    function IsPosFolded(AX, AY: integer): boolean;
    function IsLineFolded(ALine: integer; ADetectPartialFold: boolean = false): boolean;
    function IsCharWord(ch: Widechar): boolean;
    property TextCharSize: TPoint read FCharSize;
    procedure DoUnfoldLine(ALine: integer);
    property RectMain: TRect read FRectMain;
    property RectGutter: TRect read FRectGutter;
    property RectMinimap: TRect read FRectMinimap;
    property RectMicromap: TRect read FRectMicromap;
    property RectRuler: TRect read FRectRuler;
    function IndentString: string;
    function RectMicromapMark(AColumn, ALineFrom, ALineTo: integer): TRect;
    //gutter
    property Gutter: TATGutter read FGutter;
    property GutterDecor: TATGutterDecor read GetGutterDecor;
    property GutterBandBookmarks: integer read FGutterBandBookmarks write FGutterBandBookmarks;
    property GutterBandNumbers: integer read FGutterBandNumbers write FGutterBandNumbers;
    property GutterBandStates: integer read FGutterBandStates write FGutterBandStates;
    property GutterBandFolding: integer read FGutterBandFolding write FGutterBandFolding;
    property GutterBandSeparator: integer read FGutterBandSeparator write FGutterBandSeparator;
    property GutterBandEmpty: integer read FGutterBandEmpty write FGutterBandEmpty;
    property GutterBandDecor: integer read FGutterBandDecor write FGutterBandDecor;
    //files
    property FileName: string read FFileName write FFileName;
    procedure LoadFromFile(const AFilename: string; AKeepScroll: boolean=false); virtual;
    procedure SaveToFile(const AFilename: string); virtual;
    //cmd
    procedure TextInsertAtCarets(const AText: atString; AKeepCaret,
      AOvrMode, ASelectThen: boolean);
    //carets
    procedure DoCaretSingle(APosX, APosY, AEndX, AEndY: integer);
    procedure DoCaretSingle(AX, AY: integer; AClearSelection: boolean = true);
    procedure DoCaretSingleAsIs;
    function DoCaretsFixIncorrectPos(AndLimitByLineEnds: boolean): boolean;
    procedure DoCaretsFixIfInsideFolded;
    procedure DoCaretsShift(AFromCaret: integer; APosX, APosY: integer; AShiftX, AShiftY: integer;
      APosAfter: TPoint; AShiftBelowX: integer = 0);
    procedure DoCaretForceShow;
    function CaretPosToClientPos(P: TPoint): TPoint;
    function ClientPosToCaretPos(P: TPoint;
      out ADetails: TATPosDetails;
      AGapCoordAction: TATGapCoordAction=cGapCoordToLineEnd): TPoint;
    function IsLineWithCaret(ALine: integer): boolean;
    //goto
    function DoShowPos(const APos: TPoint; AIndentHorz, AIndentVert: integer; AUnfold, AllowUpdate: boolean): boolean;
    procedure DoGotoPos(const APos, APosEnd: TPoint;
      AIndentHorz, AIndentVert: integer;
      APlaceCaret, ADoUnfold: boolean;
      AAllowProcessMsg: boolean=true;
      AAllowUpdate: boolean=true);
    procedure DoGotoCaret(AEdge: TATCaretEdge; AUndoRedo: boolean=false;
      AAllowProcessMsg: boolean= true; AAllowUpdate: boolean= true);
    //bookmarks
    procedure BookmarkSetForLineEx(ALine, ABmKind: integer;
      const AHint: string; AAutoDelete: TATBookmarkAutoDelete; AShowInList: boolean; const ATag: Int64;
      ABookmarksObj: TATBookmarks);
    procedure BookmarkSetForLine(ALine, ABmKind: integer;
      const AHint: string; AAutoDelete: TATBookmarkAutoDelete; AShowInList: boolean; const ATag: Int64);
    procedure BookmarkSetForLine_2(ALine, ABmKind: integer;
      const AHint: string; AAutoDelete: TATBookmarkAutoDelete; AShowInList: boolean; const ATag: Int64);
    procedure BookmarkToggleForLine(ALine, ABmKind: integer;
      const AHint: string; AAutoDelete: TATBookmarkAutoDelete; AShowInList: boolean; const ATag: Int64);
    procedure BookmarkDeleteForLineEx(ALine: integer; ABookmarksObj: TATBookmarks);
    procedure BookmarkDeleteForLine(ALine: integer);
    procedure BookmarkDeleteForLine_2(ALine: integer);
    function BookmarkDeleteByTagEx(const ATag: Int64; ABookmarksObj: TATBookmarks): boolean;
    function BookmarkDeleteByTag(const ATag: Int64): boolean;
    function BookmarkDeleteByTag_2(const ATag: Int64): boolean;
    procedure BookmarkDeleteAll;
    procedure BookmarkDeleteAll_2;
    procedure BookmarkInvertAll;
    procedure BookmarkGotoNext(ANext: boolean; AIndentHorz, AIndentVert: integer; AOnlyShownInList: boolean);
    procedure BookmarkCopyMarkedLines;
    procedure BookmarkDeleteMarkedLines;
    procedure BookmarkPlaceBookmarksOnCarets;
    procedure BookmarkPlaceCaretsOnBookmarks;
    //fold
    procedure DoRangeFold(ARangeIndex: integer);
    procedure DoRangeUnfold(ARangeIndex: integer);
    procedure DoRangeHideLines(ALineFrom, ALineTo: integer); inline;
    procedure DoFoldForLevel(ALevel: integer);
    procedure DoFoldForLevelEx(ALevel: integer; AOuterRange: integer);
    procedure DoFoldUnfoldRangeAtCurLine(AOp: TATFoldRangeCmd);
    //markers
    procedure MarkerClearAll;
    procedure MarkerDrop;
    procedure MarkerGotoLast(AndDelete: boolean; AIndentHorz, AIndentVert: integer);
    procedure MarkerSwap;
    procedure MarkerSelectToCaret;
    procedure MarkerDeleteToCaret;
    //menu
    property PopupTextDefault: TPopupMenu read FMenuStd;
    property PopupText: TPopupMenu read FMenuText write FMenuText;
    property PopupGutterBm: TPopupMenu read FMenuGutterBm write FMenuGutterBm;
    property PopupGutterNum: TPopupMenu read FMenuGutterNum write FMenuGutterNum;
    property PopupGutterFold: TPopupMenu read FMenuGutterFold write FMenuGutterFold;
    property PopupMinimap: TPopupMenu read FMenuMinimap write FMenuMinimap;
    property PopupMicromap: TPopupMenu read FMenuMicromap write FMenuMicromap;
    property PopupRuler: TPopupMenu read FMenuRuler write FMenuRuler;
    //misc
    function GetVisibleLines: integer;
    function GetVisibleColumns: integer;
    function GetVisibleLinesMinimap: integer;
    procedure DoCommand(ACmd: integer; const AText: atString = ''); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsLocked: boolean;
    function TextSelected: atString;
    function TextSelectedEx(ACaret: TATCaretItem): atString;
    function TextCurrentWord: atString;
    property LastCommandChangedLines: integer read GetLastCommandChangedLines write SetLastCommandChangedLines;
    property IsRunningCommand: boolean read FIsRunningCommand;
    property IsReadOnlyChanged: boolean read FIsReadOnlyChanged write FIsReadOnlyChanged;
    property IsReadOnlyAutodetected: boolean read FIsReadOnlyAutodetected write FIsReadOnlyAutodetected;
    property IsCaretShapeChangedFromAPI: boolean read FIsCaretShapeChangedFromAPI write FIsCaretShapeChangedFromAPI;
    procedure DoSelect_All;
    procedure DoSelect_None;
    procedure DoSelect_Inverted;
    procedure DoSelect_SplitSelectionToLines;
    procedure DoSelect_Line(APos: TPoint);
    procedure DoSelect_CharGroupAtPos(P: TPoint; AddCaret, AllowOnlyWordChars: boolean);
    procedure DoSelect_LineRange(ALineFrom: integer; APosTo: TPoint);
    procedure DoSelect_ClearColumnBlock;
    procedure DoSelect_ColumnBlock_FromPoints(P1Char, P2Char: TPoint;
      AUpdateSelRectPoints: boolean=true);
    procedure DoSelect_ColumnBlock_FromPointsColumns(P1, P2: TPoint);
    procedure DoSelect_ColumnBlock_Primitive(P1, P2: TPoint);
    procedure DoScrollToBeginOrEnd(AToBegin: boolean);
    procedure DoScrollByDelta(ADeltaX, ADeltaY: integer);
    procedure DoScrollByDeltaInPixels(ADeltaX, ADeltaY: integer);
    procedure DoScaleFontDelta(AInc: boolean);
    function DoCalcLineHiliteEx(ALineIndex: integer; var AParts: TATLineParts;
      AColorBG: TColor; out AColorAfter: TColor): boolean;
    procedure DoSetMarkedLines(ALine1, ALine2: integer);
    procedure DoGetMarkedLines(out ALine1, ALine2: integer);
    function DoGetLinkAtPos(AX, AY: integer): atString;
    function DoGetGapRect(AIndex: integer; out ARect: TRect): boolean;
    procedure DoConvertIndentation(ASpacesToTabs: boolean);
    procedure DoConvertTabsToSpaces;

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseLeave; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos{%H-}: TPoint): boolean; override;
    function DoMouseWheelHorz(Shift: TShiftState; WheelDelta: integer; MousePos{%H-}: TPoint): boolean; {$IF LCL_FULLVERSION >= 1090000} override; {$ENDIF}

    procedure DblClick; override;
    procedure TripleClick; override;
    function DoGetTextString: atString; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    //messages
    //procedure WMGetDlgCode(var Msg: TLMNoParams); message LM_GETDLGCODE;
    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;

    {$ifdef windows}
    procedure WMIME_Request(var Msg: TMessage); message WM_IME_REQUEST;
    procedure WMIME_Notify(var Msg: TMessage); message WM_IME_NOTIFY;
    procedure WMIME_StartComposition(var Msg:TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMIME_Composition(var Msg:TMessage); message WM_IME_COMPOSITION;
    procedure WMIME_EndComposition(var Msg:TMessage); message WM_IME_ENDCOMPOSITION;
    {$endif}

  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property Constraints;
    property DoubleBuffered;
    property DragMode;
    property DragKind;
    property Enabled;
    property Font;
    property FontItalic: TFont read FFontItalic write SetFontItalic;
    property FontBold: TFont read FFontBold write SetFontBold;
    property FontBoldItalic: TFont read FFontBoldItalic write SetFontBoldItalic;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    //events std
    property OnContextPopup;
    property OnDragOver;
    property OnDragDrop;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnUTF8KeyPress;
    //events new
    property OnClickDouble: TATSynEditClickEvent read FOnClickDbl write FOnClickDbl;
    property OnClickTriple: TATSynEditClickEvent read FOnClickTriple write FOnClickTriple;
    property OnClickMiddle: TATSynEditClickEvent read FOnClickMiddle write FOnClickMiddle;
    property OnClickGutter: TATSynEditClickGutterEvent read FOnClickGutter write FOnClickGutter;
    property OnClickMicromap: TATSynEditClickMicromapEvent read FOnClickMicromap write FOnClickMicromap;
    property OnClickMoveCaret: TATSynEditClickMoveCaretEvent read FOnClickMoveCaret write FOnClickMoveCaret;
    property OnClickEndSelect: TATSynEditClickMoveCaretEvent read FOnClickEndSelect write FOnClickEndSelect;
    property OnClickGap: TATSynEditClickGapEvent read FOnClickGap write FOnClickGap;
    property OnClickLink: TATSynEditClickLinkEvent read FOnClickLink write FOnClickLink;
    property OnCheckInput: TATSynEditCheckInputEvent read FOnCheckInput write FOnCheckInput;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeModified: TNotifyEvent read FOnChangeModified write FOnChangeModified;
    property OnChangeState: TNotifyEvent read FOnChangeState write FOnChangeState;
    property OnChangeCaretPos: TNotifyEvent read FOnChangeCaretPos write FOnChangeCaretPos;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnCommand: TATSynEditCommandEvent read FOnCommand write FOnCommand;
    property OnCommandAfter: TATSynEditCommandAfterEvent read FOnCommandAfter write FOnCommandAfter;
    property OnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent read FOnDrawBookmarkIcon write FOnDrawBookmarkIcon;
    property OnDrawLine: TATSynEditDrawLineEvent read FOnDrawLine write FOnDrawLine;
    property OnDrawGap: TATSynEditDrawGapEvent read FOnDrawGap write FOnDrawGap;
    property OnDrawMicromap: TATSynEditDrawRectEvent read FOnDrawMicromap write FOnDrawMicromap;
    property OnDrawEditor: TATSynEditDrawRectEvent read FOnDrawEditor write FOnDrawEditor;
    property OnDrawRuler: TATSynEditDrawRectEvent read FOnDrawRuler write FOnDrawRuler;
    property OnCalcHilite: TATSynEditCalcHiliteEvent read FOnCalcHilite write FOnCalcHilite;
    property OnCalcStaple: TATSynEditCalcStapleEvent read FOnCalcStaple write FOnCalcStaple;
    property OnCalcTabSize: TATStringTabCalcEvent read FOnCalcTabSize write FOnCalcTabSize;
    property OnCalcBookmarkColor: TATSynEditCalcBookmarkColorEvent read FOnCalcBookmarkColor write FOnCalcBookmarkColor;
    property OnBeforeCalcHilite: TNotifyEvent read FOnBeforeCalcHilite write FOnBeforeCalcHilite;
    property OnPaste: TATSynEditPasteEvent read FOnPaste write FOnPaste;
    property OnHotspotEnter: TATSynEditHotspotEvent read FOnHotspotEnter write FOnHotspotEnter;
    property OnHotspotExit: TATSynEditHotspotEvent read FOnHotspotExit write FOnHotspotExit;

    //misc
    property CursorText: TCursor read FCursorText write FCursorText default crIBeam;
    property CursorColumnSel: TCursor read FCursorColumnSel write FCursorColumnSel default crCross;
    property CursorGutterBookmark: TCursor read FCursorGutterBookmark write FCursorGutterBookmark default crHandPoint;
    property CursorGutterNumbers: TCursor read FCursorGutterNumbers write FCursorGutterNumbers default crDefault;
    property CursorMinimap: TCursor read FCursorMinimap write FCursorMinimap default crDefault;
    property CursorMicromap: TCursor read FCursorMicromap write FCursorMicromap default crDefault;
    property Colors: TATSynEditColors read FColors write FColors stored false;
    property ImagesGutterDecor: TImageList read FGutterDecorImages write FGutterDecorImages;
    property WantTabs: boolean read FWantTabs write FWantTabs default true;
    property WantReturns: boolean read FWantReturns write FWantReturns default true;

    //options
    property OptAutoCloseBrackets: string read FOptAutoCloseBrackets write FOptAutoCloseBrackets stored false;
    property OptAutocompleteAutoshowCharCount: integer read FOptAutocompleteAutoshowCharCount write FOptAutocompleteAutoshowCharCount default 0;
    property OptAutocompleteTriggerChars: string read FOptAutocompleteTriggerChars write FOptAutocompleteTriggerChars stored false;
    property OptAutocompleteCommitChars: string read FOptAutocompleteCommitChars write FOptAutocompleteCommitChars stored false;
    property OptAutocompleteCloseChars: string read FOptAutocompleteCloseChars write FOptAutocompleteCloseChars stored false;
    property OptAutocompleteAddOpeningBracket: boolean read FOptAutocompleteAddOpeningBracket write FOptAutocompleteAddOpeningBracket default true;
    property OptAutocompleteUpDownAtEdge: integer read FOptAutocompleteUpDownAtEdge write FOptAutocompleteUpDownAtEdge default 1;
    property OptAutocompleteCommitIfSingleItem: boolean read FOptAutocompleteCommitIfSingleItem write FOptAutocompleteCommitIfSingleItem default false;

    property OptScaleFont: integer read FOptScaleFont write FOptScaleFont default 0;
    property OptIdleInterval: integer read FOptIdleInterval write FOptIdleInterval default cInitIdleInterval;
    property OptTabSpaces: boolean read FOptTabSpaces write SetTabSpaces default false;
    property OptTabSize: integer read FTabSize write SetTabSize default cInitTabSize;
    property OptNonWordChars: atString read FOptNonWordChars write FOptNonWordChars stored false;
    property OptFoldStyle: TATFoldStyle read FFoldStyle write FFoldStyle default cInitFoldStyle;
    property OptFoldEnabled: boolean read FFoldEnabled write SetFoldEnabled default true;
    property OptFoldUnderlineOffset: integer read FFoldUnderlineOffset write FFoldUnderlineOffset default cInitFoldUnderlineOffset;
    property OptFoldTooltipVisible: boolean read FFoldTooltipVisible write FFoldTooltipVisible default cInitFoldTooltipVisible;
    property OldFoldTooltipWidthPercents: integer read FFoldTooltipWidthPercents write FFoldTooltipWidthPercents default cInitFoldTooltipWidthPercents;
    property OptFoldTooltipLineCount: integer read FFoldTooltipLineCount write FFoldTooltipLineCount default cInitFoldTooltipLineCount;
    property OptTextHint: string read FTextHint write FTextHint;
    property OptTextHintFontStyle: TFontStyles read FTextHintFontStyle write FTextHintFontStyle default [fsItalic];
    property OptTextHintCenter: boolean read FTextHintCenter write FTextHintCenter default false;
    property OptTextCenteringCharWidth: integer read FOptTextCenteringCharWidth write FOptTextCenteringCharWidth default 0;
    property OptTextOffsetLeft: integer read FOptTextOffsetLeft write FOptTextOffsetLeft default 0;
    property OptTextOffsetTop: integer read GetOptTextOffsetTop write FOptTextOffsetTop default 0;
    property OptTextOffsetFromLine: integer read FOptTextOffsetFromLine write FOptTextOffsetFromLine default cInitTextOffsetFromLine;
    property OptAutoIndent: boolean read FOptAutoIndent write FOptAutoIndent default true;
    property OptAutoIndentKind: TATAutoIndentKind read FOptAutoIndentKind write FOptAutoIndentKind default cIndentAsPrevLine;
    property OptAutoIndentBetterBracketsCurly: boolean read FOptAutoIndentBetterBracketsCurly write FOptAutoIndentBetterBracketsCurly default true;
    property OptAutoIndentBetterBracketsRound: boolean read FOptAutoIndentBetterBracketsRound write FOptAutoIndentBetterBracketsRound default false;
    property OptAutoIndentBetterBracketsSquare: boolean read FOptAutoIndentBetterBracketsSquare write FOptAutoIndentBetterBracketsSquare default false;
    property OptAutoIndentRegexRule: string read FOptAutoIndentRegexRule write FOptAutoIndentRegexRule;
    property OptCopyLinesIfNoSel: boolean read FOptCopyLinesIfNoSel write FOptCopyLinesIfNoSel default true;
    property OptCutLinesIfNoSel: boolean read FOptCutLinesIfNoSel write FOptCutLinesIfNoSel default false;
    property OptCopyColumnBlockAlignedBySpaces: boolean read FOptCopyColumnBlockAlignedBySpaces write FOptCopyColumnBlockAlignedBySpaces default true;
    property OptLastLineOnTop: boolean read FOptLastLineOnTop write FOptLastLineOnTop default false;
    property OptOverwriteSel: boolean read FOptOverwriteSel write FOptOverwriteSel default true;
    property OptOverwriteAllowedOnPaste: boolean read FOptOverwriteAllowedOnPaste write FOptOverwriteAllowedOnPaste default false;
    property OptScrollStyleHorz: TATSynEditScrollStyle read FOptScrollStyleHorz write FOptScrollStyleHorz default aessAuto;
    property OptScrollStyleVert: TATSynEditScrollStyle read FOptScrollStyleVert write FOptScrollStyleVert default aessShow;
    property OptScrollSmooth: boolean read FOptScrollSmooth write FOptScrollSmooth default true;
    property OptScrollIndentCaretHorz: integer read FOptScrollIndentCaretHorz write FOptScrollIndentCaretHorz default 10;
    property OptScrollIndentCaretVert: integer read FOptScrollIndentCaretVert write FOptScrollIndentCaretVert default 0;
    property OptUndoIndentVert: integer read FOptUndoIndentVert write FOptUndoIndentVert default cInitUndoIndentVert;
    property OptUndoIndentHorz: integer read FOptUndoIndentHorz write FOptUndoIndentHorz default cInitUndoIndentHorz;
    property OptScrollbarsNew: boolean read FOptScrollbarsNew write FOptScrollbarsNew default false;
    property OptScrollbarHorizontalAddSpace: integer read FOptScrollbarHorizontalAddSpace write FOptScrollbarHorizontalAddSpace default cInitScrollbarHorzAddSpace;
    property OptScrollLineCommandsKeepCaretOnScreen: boolean read FOptScrollLineCommandsKeepCaretOnScreen write FOptScrollLineCommandsKeepCaretOnScreen default true;

    property OptShowFontLigatures: boolean read FOptShowFontLigatures write FOptShowFontLigatures default true;
    property OptShowURLs: boolean read FOptShowURLs write FOptShowURLs default true;
    property OptShowURLsRegex: string read FOptShowURLsRegex write SetOptShowURLsRegex stored false;
    property OptShowDragDropMarker: boolean read FOptShowDragDropMarker write FOptShowDragDropMarker default true;
    property OptMaxLineLenToTokenize: integer read FOptMaxLineLenToTokenize write FOptMaxLineLenToTokenize default cInitMaxLineLenToTokenize;
    property OptMinLineLenToCalcURL: integer read FOptMinLineLenToCalcURL write FOptMinLineLenToCalcURL default cInitMinLineLenToCalcURL;
    property OptMaxLineLenToCalcURL: integer read FOptMaxLineLenToCalcURL write FOptMaxLineLenToCalcURL default cInitMaxLineLenToCalcURL;
    property OptMaxLinesToCountUnindent: integer read FOptMaxLinesToCountUnindent write FOptMaxLinesToCountUnindent default 100;
    property OptStapleStyle: TATLineStyle read FOptStapleStyle write FOptStapleStyle default cLineStyleSolid;
    property OptStapleIndent: integer read FOptStapleIndent write FOptStapleIndent default -1;
    property OptStapleWidthPercent: integer read FOptStapleWidthPercent write FOptStapleWidthPercent default 100;
    property OptStapleHiliteActive: boolean read FOptStapleHiliteActive write FOptStapleHiliteActive default true;
    property OptStapleHiliteActiveAlpha: integer read FOptStapleHiliteActiveAlpha write FOptStapleHiliteActiveAlpha default cInitStapleHiliteAlpha;
    property OptStapleEdge1: TATStapleEdge read FOptStapleEdge1 write FOptStapleEdge1 default cStapleEdgeAngle;
    property OptStapleEdge2: TATStapleEdge read FOptStapleEdge2 write FOptStapleEdge2 default cStapleEdgeAngle;
    property OptStapleIndentConsidersEnd: boolean read FOptStapleIndentConsidersEnd write FOptStapleIndentConsidersEnd default false;
    property OptShowFullWidthForSelection: boolean read FOptShowFullSel write FOptShowFullSel default false;
    property OptShowFullWidthForSyntaxHilite: boolean read FOptShowFullHilite write FOptShowFullHilite default true;
    property OptShowCurLine: boolean read FOptShowCurLine write FOptShowCurLine default false;
    property OptShowCurLineMinimal: boolean read FOptShowCurLineMinimal write FOptShowCurLineMinimal default true;
    property OptShowCurLineOnlyFocused: boolean read FOptShowCurLineOnlyFocused write FOptShowCurLineOnlyFocused default false;
    property OptShowCurColumn: boolean read FOptShowCurColumn write FOptShowCurColumn default false;
    property OptShowScrollHint: boolean read FOptShowScrollHint write FOptShowScrollHint default false;
    property OptShowMouseSelFrame: boolean read FOptShowMouseSelFrame write FOptShowMouseSelFrame default cInitShowMouseSelFrame;
    property OptCaretManyAllowed: boolean read GetCaretManyAllowed write SetCaretManyAllowed default true;
    property OptCaretVirtual: boolean read FCaretVirtual write FCaretVirtual default true;
    property OptCaretBlinkTime: integer read FCaretBlinkTime write SetCaretBlinkTime default cInitCaretBlinkTime;
    property OptCaretBlinkEnabled: boolean read FCaretBlinkEnabled write SetCaretBlinkEnabled default true;
    property OptCaretStopUnfocused: boolean read FCaretStopUnfocused write FCaretStopUnfocused default true;
    property OptCaretHideUnfocused: boolean read FCaretHideUnfocused write FCaretHideUnfocused default true;
    property OptCaretPreferLeftSide: boolean read FOptCaretPreferLeftSide write FOptCaretPreferLeftSide default true;
    property OptCaretPosAfterPasteColumn: TATPasteCaret read FOptCaretPosAfterPasteColumn write FOptCaretPosAfterPasteColumn default cPasteCaretColumnRight;
    property OptCaretsPrimitiveColumnSelection: boolean read FOptCaretsPrimitiveColumnSelection write FOptCaretsPrimitiveColumnSelection default cInitCaretsPrimitiveColumnSelection;
    property OptCaretsAddedToColumnSelection: boolean read FOptCaretsAddedToColumnSelection write FOptCaretsAddedToColumnSelection default true;
    property OptCaretFixAfterRangeFolded: boolean read FOptCaretFixAfterRangeFolded write FOptCaretFixAfterRangeFolded default true;
    property OptCaretsMultiToColumnSel: boolean read FOptCaretsMultiToColumnSel write FOptCaretsMultiToColumnSel default cInitCaretsMultiToColumnSel;
    property OptMarkersSize: integer read FOptMarkersSize write FOptMarkersSize default cInitMarkerSize;
    property OptGutterVisible: boolean read FOptGutterVisible write FOptGutterVisible default true;
    property OptGutterPlusSize: integer read FOptGutterPlusSize write FOptGutterPlusSize default cInitGutterPlusSize;
    property OptGutterShowFoldAlways: boolean read FOptGutterShowFoldAlways write FOptGutterShowFoldAlways default true;
    property OptGutterShowFoldLines: boolean read FOptGutterShowFoldLines write FOptGutterShowFoldLines default true;
    property OptGutterShowFoldLinesAll: boolean read FOptGutterShowFoldLinesAll write FOptGutterShowFoldLinesAll default false;
    property OptGutterShowFoldLinesForCaret: boolean read FOptGutterShowFoldLinesForCaret write FOptGutterShowFoldLinesForCaret default true;
    property OptGutterIcons: TATGutterIconsKind read FOptGutterIcons write FOptGutterIcons default cGutterIconsPlusMinus;
    property OptBorderWidth: integer read FOptBorderWidth write FOptBorderWidth default 0;
    property OptBorderWidthFocused: integer read FOptBorderWidthFocused write FOptBorderWidthFocused default 0;
    property OptBorderFocusedActive: boolean read FOptBorderFocusedActive write FOptBorderFocusedActive default false;
    property OptRulerVisible: boolean read FOptRulerVisible write FOptRulerVisible default true;
    property OptRulerNumeration: TATRulerNumeration read FOptRulerNumeration write FOptRulerNumeration default cRulerNumeration_0_10_20;
    property OptRulerHeightPercents: integer read FOptRulerHeightPercents write FOptRulerHeightPercents default cSizeRulerHeightPercents;
    property OptRulerFontSizePercents: integer read FOptRulerFontSizePercents write FOptRulerFontSizePercents default 80;
    property OptRulerMarkSizeCaret: integer read FOptRulerMarkSizeCaret write FOptRulerMarkSizeCaret default cSizeRulerMarkCaret;
    property OptRulerMarkSizeSmall: integer read FOptRulerMarkSizeSmall write FOptRulerMarkSizeSmall default cSizeRulerMarkSmall;
    property OptRulerMarkSizeBig: integer read FOptRulerMarkSizeBig write FOptRulerMarkSizeBig default cSizeRulerMarkBig;
    property OptRulerTopIndentPercents: integer read FOptRulerTopIndentPercents write FOptRulerTopIndentPercents default 0;
    property OptMinimapCustomScale: integer read FMinimapCustomScale write FMinimapCustomScale default 0;
    property OptMinimapVisible: boolean read FMinimapVisible write SetMinimapVisible default cInitMinimapVisible;
    property OptMinimapCharWidth: integer read FMinimapCharWidth write FMinimapCharWidth default 0;
    property OptMinimapShowSelBorder: boolean read FMinimapShowSelBorder write FMinimapShowSelBorder default false;
    property OptMinimapShowSelAlways: boolean read FMinimapShowSelAlways write FMinimapShowSelAlways default true;
    property OptMinimapSelColorChange: integer read FMinimapSelColorChange write FMinimapSelColorChange default cInitMinimapSelColorChange;
    property OptMinimapAtLeft: boolean read FMinimapAtLeft write FMinimapAtLeft default false;
    property OptMinimapTooltipVisible: boolean read FMinimapTooltipVisible write FMinimapTooltipVisible default cInitMinimapTooltipVisible;
    property OptMinimapTooltipLinesCount: integer read FMinimapTooltipLinesCount write FMinimapTooltipLinesCount default cInitMinimapTooltipLinesCount;
    property OptMinimapTooltipWidthPercents: integer read FMinimapTooltipWidthPercents write FMinimapTooltipWidthPercents default cInitMinimapTooltipWidthPercents;
    property OptMinimapCachedPainting: boolean read FMinimapCachedPainting write FMinimapCachedPainting default true;
    property OptMinimapHiliteLinesWithSelection: boolean read FMinimapHiliteLinesWithSelection write FMinimapHiliteLinesWithSelection default true;
    property OptMicromapVisible: boolean read FMicromapVisible write SetMicromapVisible default cInitMicromapVisible;
    property OptCharSpacingY: integer read GetCharSpacingY write SetCharSpacingY default cInitSpacingText;
    property OptWrapMode: TATSynWrapMode read FWrapMode write SetWrapMode default cInitWrapMode;
    property OptWrapIndented: boolean read FWrapIndented write SetWrapIndented default true;
    property OptWrapAddSpace: integer read FWrapAddSpace write FWrapAddSpace default 1;
    property OptWrapEnabledForMaxLines: integer read FWrapEnabledForMaxLines write FWrapEnabledForMaxLines default cInitWrapEnabledForMaxLines;
    property OptMarginRight: integer read FMarginRight write SetMarginRight default cInitMarginRight;
    property OptMarginString: string read GetMarginString write SetMarginString;
    property OptNumbersAutosize: boolean read FOptNumbersAutosize write FOptNumbersAutosize default true;
    property OptNumbersAlignment: TAlignment read FOptNumbersAlignment write FOptNumbersAlignment default taRightJustify;
    property OptNumbersStyle: TATSynNumbersStyle read FOptNumbersStyle write FOptNumbersStyle default cInitNumbersStyle;
    property OptNumbersShowFirst: boolean read FOptNumbersShowFirst write FOptNumbersShowFirst default true;
    property OptNumbersShowCarets: boolean read FOptNumbersShowCarets write FOptNumbersShowCarets default false;
    property OptNumbersSkippedChar: string read FOptNumbersSkippedChar write FOptNumbersSkippedChar;
    property OptNumbersIndentPercents: integer read FOptNumbersIndentPercents write FOptNumbersIndentPercents default cInitNumbersIndentPercents;
    property OptUnprintedVisible: boolean read FUnprintedVisible write FUnprintedVisible default true;
    property OptUnprintedSpaces: boolean read FUnprintedSpaces write FUnprintedSpaces default true;
    property OptUnprintedSpacesTrailing: boolean read FUnprintedSpacesTrailing write FUnprintedSpacesTrailing default false;
    property OptUnprintedSpacesBothEnds: boolean read FUnprintedSpacesBothEnds write FUnprintedSpacesBothEnds default false;
    property OptUnprintedSpacesOnlyInSelection: boolean read FUnprintedSpacesOnlyInSelection write FUnprintedSpacesOnlyInSelection default false;
    property OptUnprintedEnds: boolean read FUnprintedEnds write FUnprintedEnds default true;
    property OptUnprintedEndsDetails: boolean read FUnprintedEndsDetails write FUnprintedEndsDetails default true;
    property OptUnprintedEof: boolean read FUnprintedEof write FUnprintedEof default true;
    property OptMouseEnableAll: boolean read FOptMouseEnableAll write FOptMouseEnableAll default true;
    property OptMouseEnableNormalSelection: boolean read FOptMouseEnableNormalSelection write FOptMouseEnableNormalSelection default true;
    property OptMouseEnableColumnSelection: boolean read FOptMouseEnableColumnSelection write FOptMouseEnableColumnSelection default true;
    property OptMouseHideCursorOnType: boolean read FOptMouseHideCursor write FOptMouseHideCursor default false;
    property OptMouseClickOpensURL: boolean read FOptMouseClickOpensURL write FOptMouseClickOpensURL default false;
    property OptMouseClickNumberSelectsLine: boolean read FOptMouseClickNumberSelectsLine write FOptMouseClickNumberSelectsLine default true;
    property OptMouseClickNumberSelectsLineWithEOL: boolean read FOptMouseClickNumberSelectsLineWithEOL write FOptMouseClickNumberSelectsLineWithEOL default true;
    property OptMouse2ClickAction: TATMouseDoubleClickAction read FOptMouse2ClickAction write FOptMouse2ClickAction default cMouseDblClickSelectAnyChars;
    property OptMouse2ClickOpensURL: boolean read FOptMouse2ClickOpensURL write FOptMouse2ClickOpensURL default true;
    property OptMouse2ClickDragSelectsWords: boolean read FOptMouse2ClickDragSelectsWords write FOptMouse2ClickDragSelectsWords default true;
    property OptMouse3ClickSelectsLine: boolean read FOptMouse3ClickSelectsLine write FOptMouse3ClickSelectsLine default true;
    property OptMouseDragDrop: boolean read FOptMouseDragDrop write FOptMouseDragDrop default true;
    property OptMouseDragDropCopying: boolean read FOptMouseDragDropCopying write FOptMouseDragDropCopying default true;
    property OptMouseDragDropCopyingWithState: TShiftStateEnum read FOptMouseDragDropCopyingWithState write FOptMouseDragDropCopyingWithState default ssModifier;
    property OptMouseMiddleClickAction: TATMiddleClickAction read FOptMouseMiddleClickAction write FOptMouseMiddleClickAction default mcaScrolling;
    property OptMouseRightClickMovesCaret: boolean read FOptMouseRightClickMovesCaret write FOptMouseRightClickMovesCaret default false;
    property OptMouseWheelScrollVert: boolean read FOptMouseWheelScrollVert write FOptMouseWheelScrollVert default true;
    property OptMouseWheelScrollVertSpeed: integer read FOptMouseWheelScrollVertSpeed write FOptMouseWheelScrollVertSpeed default 3;
    property OptMouseWheelScrollHorz: boolean read FOptMouseWheelScrollHorz write FOptMouseWheelScrollHorz default true;
    property OptMouseWheelScrollHorzSpeed: integer read FOptMouseWheelScrollHorzSpeed write FOptMouseWheelScrollHorzSpeed default 10;
    property OptMouseWheelScrollHorzWithState: TShiftStateEnum read FOptMouseWheelScrollHorzWithState write FOptMouseWheelScrollHorzWithState default ssShift;
    property OptMouseWheelZooms: boolean read FOptMouseWheelZooms write FOptMouseWheelZooms default true;
    property OptMouseWheelZoomsWithState: TShiftStateEnum read FOptMouseWheelZoomsWithState write FOptMouseWheelZoomsWithState default ssModifier;
    property OptMouseColumnSelectionWithoutKey: boolean read FOptMouseColumnSelectionWithoutKey write FOptMouseColumnSelectionWithoutKey default false;
    property OptKeyBackspaceUnindent: boolean read FOptKeyBackspaceUnindent write FOptKeyBackspaceUnindent default true;
    property OptKeyBackspaceGoesToPrevLine: boolean read FOptKeyBackspaceGoesToPrevLine write FOptKeyBackspaceGoesToPrevLine default true;
    property OptKeyPageKeepsRelativePos: boolean read FOptKeyPageKeepsRelativePos write FOptKeyPageKeepsRelativePos default true;
    property OptKeyUpDownNavigateWrapped: boolean read FOptKeyUpDownNavigateWrapped write FOptKeyUpDownNavigateWrapped default true;
    property OptKeyUpDownKeepColumn: boolean read FOptKeyUpDownKeepColumn write FOptKeyUpDownKeepColumn default true;
    property OptKeyHomeEndNavigateWrapped: boolean read FOptKeyHomeEndNavigateWrapped write FOptKeyHomeEndNavigateWrapped default true;
    property OptKeyPageUpDownSize: TATPageUpDownSize read FOptKeyPageUpDownSize write FOptKeyPageUpDownSize default cPageSizeFullMinus1;
    property OptKeyLeftRightGoToNextLineWithCarets: boolean read FOptKeyLeftRightGoToNextLineWithCarets write FOptKeyLeftRightGoToNextLineWithCarets default true;
    property OptKeyLeftRightSwapSel: boolean read FOptKeyLeftRightSwapSel write FOptKeyLeftRightSwapSel default true;
    property OptKeyLeftRightSwapSelAndSelect: boolean read FOptKeyLeftRightSwapSelAndSelect write FOptKeyLeftRightSwapSelAndSelect default false;
    property OptKeyHomeToNonSpace: boolean read FOptKeyHomeToNonSpace write FOptKeyHomeToNonSpace default true;
    property OptKeyEndToNonSpace: boolean read FOptKeyEndToNonSpace write FOptKeyEndToNonSpace default true;
    property OptKeyTabIndents: boolean read FOptKeyTabIndents write FOptKeyTabIndents default true;
    property OptKeyTabIndentsVerticalBlock: boolean read FOptKeyTabIndentsVerticalBlock write FOptKeyTabIndentsVerticalBlock default false;
    property OptIndentSize: integer read FOptIndentSize write FOptIndentSize default 2;
             // N>0: use N spaces
             // N<0: use N tabs
             // N=0: calc indent from OptTabSize/OptTabSpaces
    property OptIndentKeepsAlign: boolean read FOptIndentKeepsAlign write FOptIndentKeepsAlign default true;
    property OptIndentMakesWholeLinesSelection: boolean read FOptIndentMakesWholeLinesSelection write FOptIndentMakesWholeLinesSelection default false;
    property OptShowIndentLines: boolean read FOptShowIndentLines write FOptShowIndentLines default true;
    property OptShowGutterCaretBG: boolean read FOptShowGutterCaretBG write FOptShowGutterCaretBG default true;
    property OptAllowRepaintOnTextChange: boolean read FOptAllowRepaintOnTextChange write FOptAllowRepaintOnTextChange default true;
    property OptAllowReadOnly: boolean read FOptAllowReadOnly write FOptAllowReadOnly default true;
    property OptUndoLimit: integer read FOptUndoLimit write SetUndoLimit default cInitUndoLimit;
    property OptUndoGrouped: boolean read FOptUndoGrouped write FOptUndoGrouped default true;
    property OptUndoAfterSave: boolean read GetUndoAfterSave write SetUndoAfterSave default true;
    property OptUndoMaxCarets: integer read FOptUndoMaxCarets write FOptUndoMaxCarets default cInitUndoMaxCarets;
    property OptSavingForceFinalEol: boolean read FOptSavingForceFinalEol write FOptSavingForceFinalEol default false;
    property OptSavingTrimSpaces: boolean read FOptSavingTrimSpaces write FOptSavingTrimSpaces default false;
    property OptSavingTrimFinalEmptyLines: boolean read FOptSavingTrimFinalEmptyLines write FOptSavingTrimFinalEmptyLines default false;
    property OptPasteAtEndMakesFinalEmptyLine: boolean read FOptPasteAtEndMakesFinalEmptyLine write FOptPasteAtEndMakesFinalEmptyLine default true;
    property OptPasteMultilineTextSpreadsToCarets: boolean read FOptPasteMultilineTextSpreadsToCarets write FOptPasteMultilineTextSpreadsToCarets default true;
    property OptZebraActive: boolean read FOptZebraActive write FOptZebraActive default false;
    property OptZebraStep: integer read FOptZebraStep write FOptZebraStep default 2;
    property OptZebraAlphaBlend: byte read FOptZebraAlphaBlend write FOptZebraAlphaBlend default cInitZebraAlphaBlend;
  end;

var
  //better to have as global bar (for many editors)
  OptMouseDragDropFocusesTargetEditor: boolean = true;

const
  cEncNameUtf8_WithBom = 'UTF-8 with BOM';
  cEncNameUtf8_NoBom = 'UTF-8';
  cEncNameUtf16LE_WithBom = 'UTF-16 LE with BOM';
  cEncNameUtf16LE_NoBom = 'UTF-16 LE';
  cEncNameUtf16BE_WithBom = 'UTF-16 BE with BOM';
  cEncNameUtf16BE_NoBom = 'UTF-16 BE';
  cEncNameUtf32LE_WithBom = 'UTF-32 LE with BOM';
  cEncNameUtf32LE_NoBom = 'UTF-32 LE';
  cEncNameUtf32BE_WithBom = 'UTF-32 BE with BOM';
  cEncNameUtf32BE_NoBom = 'UTF-32 BE';

function EditorLinkIsEmail(const S: string): boolean;
procedure EditorOpenLink(const S: string);

implementation

uses
  LCLIntf,
  LCLProc,
  Dialogs,
  Types,
  Math,
  ATSynEdit_Commands,
  ATSynEdit_Keymap_Init;

{$I atsynedit_proc.inc}

{ TATSynEdit }

procedure TATSynEdit.DoPaintRulerTo(C: TCanvas);
var
  NX, NPrevFontSize, NRulerStart, NOutput,
  NTopIndent, NMarkHeight, i: integer;
  NCharW: integer;
  Str: string;
begin
  NPrevFontSize:= C.Font.Size;
  NRulerStart:= FScrollHorz.NPos;
  NX:= FRectMain.Left;
  NTopIndent:= FOptRulerTopIndentPercents*FCharSize.Y div 100;

  C.Font.Name:= Font.Name;
  C.Font.Size:= DoScaleFont(Font.Size) * FOptRulerFontSizePercents div 100;
  C.Font.Color:= Colors.RulerFont;
  C.Pen.Color:= Colors.RulerFont;
  C.Brush.Color:= Colors.RulerBG;

  C.FillRect(FRectRuler);
  CanvasLineHorz(C, FRectRuler.Left, FRectRuler.Bottom-1, FRectRuler.Right);

  NCharW:= FCharSize.X * FOptRulerFontSizePercents div 100;

  for i:= NRulerStart to NRulerStart+GetVisibleColumns+1 do
  begin
    case FOptRulerNumeration of
      cRulerNumeration_0_10_20:
        begin
          NOutput:= i;
          if (i mod 10 = 0) then
          begin
            Str:= IntToStr(NOutput);
            CanvasTextOutSimplest(C, NX - NCharW*Length(Str) div 2, NTopIndent, Str);
          end;
        end;
      cRulerNumeration_1_11_21:
        begin
          NOutput:= i;
          if (i mod 10 = 0) then
          begin
            Str:= IntToStr(NOutput+1{!});
            CanvasTextOutSimplest(C, NX - NCharW*Length(Str) div 2, NTopIndent, Str);
          end;
        end;
      cRulerNumeration_1_10_20:
        begin
          NOutput:= i+1;
          if (NOutput=1) or (NOutput mod 10 = 0) then
          begin
            Str:= IntToStr(NOutput);
            CanvasTextOutSimplest(C, NX - NCharW*Length(Str) div 2, NTopIndent, Str);
          end;
        end;
    end;

    if NOutput mod 5 = 0 then
      NMarkHeight:= EditorScale(FOptRulerMarkSizeBig)
    else
      NMarkHeight:= EditorScale(FOptRulerMarkSizeSmall);

    CanvasLineVert(C, NX, FRectRuler.Bottom-1-NMarkHeight, FRectRuler.Bottom-1);

    Inc(NX, FCharSize.X);
  end;

  C.Font.Size:= NPrevFontSize;
end;


procedure TATSynEdit.DoPaintRulerCaretMark(C: TCanvas; ACaretX: integer);
begin
  if (ACaretX>=FRectRuler.Left) and (ACaretX<FRectRuler.Right) then
    CanvasPaintTriangleDown(C,
      Colors.RulerFont,
      Point(ACaretX, FRectRuler.Top+FOptRulerMarkSizeCaret),
      EditorScale(FOptRulerMarkSizeCaret)
      );
end;

procedure TATSynEdit.UpdateGutterAutosize;
var
  Str: string;
begin
  Str:= IntToStr(Max(10, Strings.Count));
  FGutter[FGutterBandNumbers].Size:=
    Length(Str)*FCharSize.X + 2*FNumbersIndent;
  FGutter.Update;
end;

procedure TATSynEdit.UpdateMinimapAutosize;
{
  Minimap must give same cnt of small chars, as rest width gives for normal chars.
  This gives:
    MapSize / CharWidth_small = (ClientWidth - MapSize) / CharWidth_big
    MapSize = (ClientWidth * CharWidth_small) / (CharWidth_big+CharWidth_small)
}
var
  CharSmall, CharBig: integer;
begin
  CharBig:= FCharSize.X;
  CharSmall:= FCharSizeMinimap.X;

  if FMinimapCharWidth=0 then
  begin
    FMinimapWidth:= ClientWidth-FTextOffset.X;
    if FMicromapVisible then
      Dec(FMinimapWidth, FRectMicromap.Width);
    FMinimapWidth:= FMinimapWidth * CharSmall div (CharSmall+CharBig);
  end
  else
    FMinimapWidth:= CharSmall*FMinimapCharWidth;

  FMinimapWidth:= Max(cMinMinimapWidth, FMinimapWidth);
end;

function TATSynEdit.DoFormatLineNumber(N: integer): string;
var
  NCurLine: integer;
begin
  if FOptNumbersStyle=cNumbersRelative then
  begin
    if Carets.Count=0 then
      exit(IntToStr(N));
    NCurLine:= Carets[0].PosY+1;
    if N=NCurLine then
      Result:= IntToStr(N)
    else
      Result:= IntToStr(N-NCurLine);
    exit
  end;

  if FOptNumbersShowCarets then
    if IsLineWithCaret(N-1) then
      Exit(IntToStr(N));

  if FOptNumbersShowFirst then
    if N=1 then
      Exit(IntToStr(N));

  case FOptNumbersStyle of
    cNumbersAll:
      Result:= IntToStr(N);
    cNumbersNone:
      Result:= FOptNumbersSkippedChar;
    cNumbersEach10th:
      begin
        if (N mod 10 = 0) then
          Result:= IntToStr(N)
        else
        if (N mod 5) = 0 then
          Result:= '-'
        else
          Result:= FOptNumbersSkippedChar;
      end;
    cNumbersEach5th:
      begin
        if (N mod 5 = 0) then
          Result:= IntToStr(N)
        else
          Result:= FOptNumbersSkippedChar;
      end;
  end;
end;

function TATSynEdit.GetScrollbarVisible(bVertical: boolean): boolean;
const
  cKind: array[boolean] of integer = (SB_HORZ, SB_VERT);
var
  si: TScrollInfo;
begin
  FillChar(si{%H-}, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;
  GetScrollInfo(Handle, cKind[bVertical], si);
  Result:= Longword(si.nMax) > Longword(si.nPage);
end;

procedure TATSynEdit.SetMarginRight(AValue: integer);
begin
  if AValue=FMarginRight then Exit;
  FMarginRight:= Max(AValue, cMinMarginRt);
  if FWrapMode in [cWrapAtMargin, cWrapAtWindowOrMargin] then
    FWrapUpdateNeeded:= true;
  Update;
end;

procedure TATSynEdit.UpdateWrapInfo(AForceUpdate: boolean);
var
  CurStrings: TATStrings;
  ListNums: TATIntegerList;
  UseCachedUpdate: boolean;
  bConsiderFolding: boolean;
  NNewVisibleColumns: integer;
  NIndentMaximal: integer;
  NLine, NIndexFrom, NIndexTo: integer;
  i, j: integer;
begin
  //method can be called before 1st paint,
  //so TCanvas.TextWidth (TATSynEdit.GetCharSize) will give exception "Control has no parent window"
  //example: CudaText has user.json with "wrap_mode":1

  //2021.01.29:
  //check "if not HandleAllocated" stops the work, when passive file-tabs are
  //trying to restore Ed.LineTop.
  // https://github.com/Alexey-T/CudaText/issues/3112
  //to fix this issue, let's not Exit "if not HandleAllocated",
  //but handle this in GetCharSize(), via GetDC(0)

  if not HandleAllocated then
    if FWrapMode<>cWrapOff then
      exit;

  //must init FRect* if called before first paint (wrapped items need it)
  if FRectMain.Width=0 then
    UpdateInitialVars(Canvas);

  GlobalCharSizer.Init(Font.Name, DoScaleFont(Font.Size));

  //virtual mode allows faster usage of WrapInfo
  CurStrings:= Strings;
  FWrapInfo.StringsObj:= CurStrings;
  FWrapInfo.VirtualMode:=
    (FWrapMode=cWrapOff) and
    (Fold.Count=0) and
    (CurStrings.Count>2);
  if FWrapInfo.VirtualMode then exit;

  bConsiderFolding:= Fold.Count>0;
  NNewVisibleColumns:= GetVisibleColumns;
  NIndentMaximal:= Max(2, NNewVisibleColumns-cMinCharsAfterAnyIndent); //don't do too big NIndent

  if AForceUpdate then
    FWrapUpdateNeeded:= true
  else
  if (not FWrapUpdateNeeded) and
    (FWrapMode=cWrapOn) and
    (FPrevVisibleColumns<>NNewVisibleColumns) then
    FWrapUpdateNeeded:= true;

  if not FWrapUpdateNeeded then Exit;
  FWrapUpdateNeeded:= false;
  FPrevVisibleColumns:= NNewVisibleColumns;

  InvalidateHilitingCache;

  case FWrapMode of
    cWrapOff:
      FWrapColumn:= 0;
    cWrapOn:
      FWrapColumn:= Max(cMinWrapColumn, NNewVisibleColumns-FWrapAddSpace);
    cWrapAtMargin:
      FWrapColumn:= Max(cMinWrapColumn, FMarginRight);
    cWrapAtWindowOrMargin:
      FWrapColumn:= Max(cMinWrapColumn, Min(NNewVisibleColumns-FWrapAddSpace, FMarginRight));
  end;

  UseCachedUpdate:=
    (FWrapInfo.Count>0) and
    (CurStrings.Count>cMaxLinesForOldWrapUpdate) and
    (not CurStrings.ListUpdatesHard) and
    (CurStrings.ListUpdates.Count>0);
  //UseCachedUpdate:= false;////to disable

  FWrapTemps.Clear;

  if not UseCachedUpdate then
  begin
    FWrapInfo.Clear;
    FWrapInfo.SetCapacity(CurStrings.Count);
    for i:= 0 to CurStrings.Count-1 do
    begin
      DoCalcWrapInfos(i, NIndentMaximal, FWrapTemps, bConsiderFolding);
      for j:= 0 to FWrapTemps.Count-1 do
        FWrapInfo.Add(FWrapTemps[j]);
    end;
    FWrapTemps.Clear;
  end
  else
  begin
    //cached WrapInfo update - calculate info only for changed lines (Strings.ListUpdates)
    //and insert results into WrapInfo
    ListNums:= TATIntegerList.Create;
    try
      ListNums.Assign(CurStrings.ListUpdates);

      for i:= 0 to ListNums.Count-1 do
      begin
        NLine:= ListNums[i];
        DoCalcWrapInfos(NLine, NIndentMaximal, FWrapTemps, bConsiderFolding);
        if FWrapTemps.Count=0 then Continue;

        FWrapInfo.FindIndexesOfLineNumber(NLine, NIndexFrom, NIndexTo);
        if NIndexFrom<0 then
        begin
          //Showmessage('Cant find wrap-index for line '+Inttostr(NLine));
          Continue;
        end;

        //slow for 100carets, 1M lines, so made method in which
        //we can optimize it (instead of del/ins do assign)
        FWrapInfo.ReplaceItems(NIndexFrom, NIndexTo, FWrapTemps);
      end;
      FWrapTemps.Clear;
    finally
      FreeAndNil(ListNums);
    end;
  end;

  CurStrings.ListUpdates.Clear;
  CurStrings.ListUpdatesHard:= false;

  {$ifdef debug_findwrapindex}
  DebugFindWrapIndex;
  {$endif}
end;


procedure _CalcWrapInfos(
  AStrings: TATStrings;
  ATabHelper: TATStringTabHelper;
  AEditorIndex: integer;
  AWrapColumn: integer;
  AWrapIndented: boolean;
  AVisibleColumns: integer;
  const ANonWordChars: atString;
  ALineIndex: integer;
  AIndentMaximal: integer;
  AItems: TATWrapItems;
  AConsiderFolding: boolean);
var
  WrapItem: TATWrapItem;
  NPartOffset, NLen, NIndent, NVisColumns: integer;
  NFoldFrom: integer;
  FinalState: TATWrapItemFinal;
  bInitialItem: boolean;
  StrPart: atString;
begin
  AItems.Clear;

  //line folded entirely?
  if AConsiderFolding then
    if AStrings.LinesHidden[ALineIndex, AEditorIndex] then Exit;

  NLen:= AStrings.LinesLen[ALineIndex];

  if NLen=0 then
  begin
    WrapItem.Init(ALineIndex, 1, 0, 0, cWrapItemFinal, true);
    AItems.Add(WrapItem);
    Exit;
  end;

  //consider fold, before wordwrap
  if AConsiderFolding then
  begin
    //line folded partially?
    NFoldFrom:= AStrings.LinesFoldFrom[ALineIndex, AEditorIndex];
    if NFoldFrom>0 then
    begin
      WrapItem.Init(ALineIndex, 1, Min(NLen, NFoldFrom-1), 0, cWrapItemCollapsed, true);
      AItems.Add(WrapItem);
      Exit;
    end;
  end;

  //line not wrapped?
  if (AWrapColumn<cMinWrapColumnAbs) then
  begin
    WrapItem.Init(ALineIndex, 1, NLen, 0, cWrapItemFinal, true);
    AItems.Add(WrapItem);
    Exit;
  end;

  NVisColumns:= Max(AVisibleColumns, cMinWrapColumnAbs);
  NPartOffset:= 1;
  NIndent:= 0;
  bInitialItem:= true;

  repeat
    StrPart:= AStrings.LineSub(ALineIndex, NPartOffset, NVisColumns);
    if StrPart='' then Break;

    NLen:= ATabHelper.FindWordWrapOffset(
      ALineIndex,
      //very slow to calc for entire line (eg len=70K),
      //calc for first NVisColumns chars
      StrPart,
      Max(AWrapColumn-NIndent, cMinWrapColumnAbs),
      ANonWordChars,
      AWrapIndented);

    if NLen>=Length(StrPart) then
      FinalState:= cWrapItemFinal
    else
      FinalState:= cWrapItemMiddle;

    WrapItem.Init(ALineIndex, NPartOffset, NLen, NIndent, FinalState, bInitialItem);
    AItems.Add(WrapItem);
    bInitialItem:= false;

    if AWrapIndented then
      if NPartOffset=1 then
      begin
        NIndent:= ATabHelper.GetIndentExpanded(ALineIndex, StrPart);
        NIndent:= Min(NIndent, AIndentMaximal);
      end;

    Inc(NPartOffset, NLen);
  until false;
end;


procedure TATSynEdit.DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer; AItems: TATWrapItems;
  AConsiderFolding: boolean);
begin
  _CalcWrapInfos(
    Strings,
    FTabHelper,
    FEditorIndex,
    FWrapColumn,
    FWrapIndented,
    GetVisibleColumns,
    FOptNonWordChars,
    ALine,
    AIndentMaximal,
    AItems,
    AConsiderFolding);
end;


function TATSynEdit.GetVisibleLines: integer; inline;
begin
  Result:= FRectMainVisible.Height div FCharSize.Y;
end;

function TATSynEdit.GetVisibleColumns: integer; inline;
begin
  Result:= FRectMainVisible.Width div FCharSize.X;
end;

function TATSynEdit.GetVisibleLinesMinimap: integer; inline;
begin
  Result:= FRectMinimap.Height div FCharSizeMinimap.Y - 1;
end;

function TATSynEdit.GetMinimapScrollPos: integer;
begin
  Result:=
    Int64(Max(0, FScrollVert.NPos)) *
    Max(0, FScrollVert.NMax-GetVisibleLinesMinimap) div
    Max(1, FScrollVert.NMax-FScrollVert.NPage);
end;

procedure TATSynEdit.SetTabSize(AValue: integer);
begin
  if FTabSize=AValue then Exit;
  FTabSize:= Min(cMaxTabSize, Max(cMinTabSize, AValue));
  FWrapUpdateNeeded:= true;
  FTabHelper.TabSize:= FTabSize;
end;

procedure TATSynEdit.SetTabSpaces(AValue: boolean);
begin
  if FOptTabSpaces=AValue then Exit;
  FOptTabSpaces:= AValue;
  FTabHelper.TabSpaces:= AValue;
end;

procedure TATSynEdit.SetText(const AValue: atString);
begin
  Strings.LoadFromString(AValue);
  DoCaretSingle(0, 0);
  Update(true);
end;

procedure TATSynEdit.SetWrapMode(AValue: TATSynWrapMode);
var
  NLine: integer;
  Caret: TATCaretItem;
begin
  if FWrapMode=AValue then Exit;

  //disable setting wrap=on for too big files
  if FWrapMode=cWrapOff then
    if Strings.Count>=FWrapEnabledForMaxLines then exit;

  NLine:= LineTop;
  FWrapMode:= AValue;

  FWrapUpdateNeeded:= true;
  UpdateWrapInfo; //helps to solve https://github.com/Alexey-T/CudaText/issues/2879
                  //FWrapUpdateNeeded:=true and Update() is not enough

  if FWrapMode<>cWrapOff then
    FScrollHorz.SetZero;

  Update;
  LineTop:= NLine;

  //when very long line has caret at end, and we toggle wordwrap, let's scroll to new caret pos
  if FWrapMode=cWrapOff then
    if Carets.Count=1 then
    begin
      Caret:= Carets[0];
      if Caret.PosX>0 then
        DoShowPos(
          Point(Caret.PosX, Caret.PosY),
          FOptScrollIndentCaretHorz,
          FOptScrollIndentCaretVert,
          true,
          true);
      end;
end;

procedure TATSynEdit.SetWrapIndented(AValue: boolean);
begin
  if FWrapIndented=AValue then Exit;
  FWrapIndented:=AValue;
  if FWrapMode<>cWrapOff then
    FWrapUpdateNeeded:= true;
end;

function TATSynEdit.UpdateScrollbars(AdjustSmoothPos: boolean): boolean;
//returns True is scrollbars visibility was changed
var
  bVert1, bVert2, bHorz1, bHorz2: boolean;
  bVertOur1, bVertOur2, bHorzOur1, bHorzOur2: boolean;
  bChangedBarsOs, bChangedBarsOur: boolean;
  NPos, NLineIndex, NGapPos, NGapAll: integer;
begin
  Result:= false;

  NGapAll:= 0;
  NGapPos:= 0;

  //consider Gaps for vertical scrollbar
  if Gaps.Count>0 then
  begin
    if AdjustSmoothPos then
    begin
      NLineIndex:= 0;
      NPos:= Max(0, FScrollVert.NPos);
      if FWrapInfo.IsIndexValid(NPos) then
        NLineIndex:= FWrapInfo.Data[NPos].NLineIndex;
      NGapPos:= Gaps.SizeForLineRange(-1, NLineIndex-1);
    end;

    NGapAll:= Gaps.SizeForAll;
  end;

  with FScrollVert do
  begin
    NPage:= Max(1, GetVisibleLines)-1;
    NMax:= Max(0, FWrapInfo.Count-1); //must be 0 for single line text
    if FOptLastLineOnTop then
      Inc(NMax, NPage);
    NPosLast:= Max(0, NMax-NPage);

    SmoothCharSize:= FCharSize.Y;
    SmoothMax:= NMax*SmoothCharSize + NGapAll;
    SmoothPage:= NPage*SmoothCharSize;
    SmoothPosLast:= Max(0, SmoothMax - SmoothPage);
    if AdjustSmoothPos then
      SmoothPos:= TotalOffset + NGapPos;
  end;

  with FScrollHorz do
  begin
    NPage:= Max(1, GetVisibleColumns);
    //NMax is calculated in DoPaintTextTo
    //hide horz bar for word-wrap:
    if FWrapMode=cWrapOn then
      NMax:= NPage;
    NPosLast:= Max(0, NMax-NPage);

    SmoothCharSize:= FCharSize.X;
    SmoothMax:= NMax*SmoothCharSize;
    SmoothPage:= NPage*SmoothCharSize;
    SmoothPosLast:= Max(0, SmoothMax - SmoothPage);
    if AdjustSmoothPos then
      SmoothPos:= TotalOffset;
  end;

  bVert1:= ShowOsBarVert;
  bHorz1:= ShowOsBarHorz;
  bVertOur1:= FScrollbarVert.Visible;
  bHorzOur1:= FScrollbarHorz.Visible;

  UpdateScrollbarVert;
  UpdateScrollbarHorz;

  bVert2:= ShowOsBarVert;
  bHorz2:= ShowOsBarHorz;
  bVertOur2:= FScrollbarVert.Visible;
  bHorzOur2:= FScrollbarHorz.Visible;

  bChangedBarsOs:= (bVert1<>bVert2) or (bHorz1<>bHorz2);
  bChangedBarsOur:= (bVertOur1<>bVertOur2) or (bHorzOur1<>bHorzOur2);

  Result:= bChangedBarsOs or bChangedBarsOur;
  if Result then
    UpdateClientSizes;

  if (FPrevHorz<>FScrollHorz) or
    (FPrevVert<>FScrollVert) then
  begin
    FPrevHorz:= FScrollHorz;
    FPrevVert:= FScrollVert;
    DoEventScroll;
  end;
end;

procedure TATSynEdit.UpdateScrollbarVert;
var
  NeedBar: boolean;
  si: TScrollInfo;
begin
  case FOptScrollStyleVert of
    aessHide:
      NeedBar:= false;
    aessShow:
      NeedBar:= true;
    aessAuto:
      NeedBar:= (FScrollVert.SmoothPos>0) or (FScrollVert.NMax>FScrollVert.NPage);
  end;

  FScrollbarVert.Visible:= NeedBar and FOptScrollbarsNew;
  ShowOsBarVert:= NeedBar and not FOptScrollbarsNew;

  if FScrollbarVert.Visible then
  begin
    FScrollbarLock:= true;
    FScrollbarVert.Min:= 0;
    FScrollbarVert.Max:= FScrollVert.SmoothMax;
    FScrollbarVert.SmallChange:= FScrollVert.SmoothCharSize;
    FScrollbarVert.PageSize:= FScrollVert.SmoothPage;
    FScrollbarVert.Position:= FScrollVert.SmoothPos;
    FScrollbarVert.Update;
    FScrollbarLock:= false;
  end;

  if ShowOsBarVert then
  begin
    FillChar(si{%H-}, SizeOf(si), 0);
    si.cbSize:= SizeOf(si);
    si.fMask:= SIF_ALL; //or SIF_DISABLENOSCROLL; //todo -- DisableNoScroll doesnt work(Win)
    si.nMin:= 0;
    si.nMax:= FScrollVert.SmoothMax;
    si.nPage:= FScrollVert.SmoothPage;
    //if FOptScrollbarsNew then
    //  si.nPage:= si.nMax+1;
    si.nPos:= FScrollVert.SmoothPos;
    SetScrollInfo(Handle, SB_VERT, si, True);
  end;

  {$ifdef debug_scroll}
  Writeln(Format('ATSynEdit SetScrollInfo: SB_VERT, nMin=%d, nMax=%d, nPage=%d, nPos=%d',
    [FScrollVert.NMin, FScrollVert.NMax, FScrollVert.NPage, FScrollVert.NPos]));
  {$endif}
end;

procedure TATSynEdit.UpdateScrollbarHorz;
var
  NeedBar: boolean;
  si: TScrollInfo;
begin
  case FOptScrollStyleHorz of
    aessHide:
      NeedBar:= false;
    aessShow:
      NeedBar:= true;
    aessAuto:
      NeedBar:= (FScrollHorz.SmoothPos>0) or (FScrollHorz.NMax>FScrollHorz.NPage);
  end;

  FScrollbarHorz.Visible:= NeedBar and FOptScrollbarsNew;
  ShowOsBarHorz:= NeedBar and not FOptScrollbarsNew;

  if FScrollbarHorz.Visible then
  begin
    FScrollbarLock:= true;
    FScrollbarHorz.Min:= 0;
    FScrollbarHorz.Max:= FScrollHorz.SmoothMax;
    FScrollbarHorz.SmallChange:= FScrollHorz.SmoothCharSize;
    FScrollbarHorz.PageSize:= FScrollHorz.SmoothPage;
    FScrollbarHorz.Position:= FScrollHorz.SmoothPos;
    FScrollbarHorz.Update;
    if FScrollbarVert.Visible then
      FScrollbarHorz.IndentCorner:= 100
    else
      FScrollbarHorz.IndentCorner:= 0;
    FScrollbarLock:= false;
  end;

  if ShowOsBarHorz then
  begin
    FillChar(si{%H-}, SizeOf(si), 0);
    si.cbSize:= SizeOf(si);
    si.fMask:= SIF_ALL; //or SIF_DISABLENOSCROLL; don't work
    si.nMin:= 0;
    si.nMax:= FScrollHorz.SmoothMax;
    si.nPage:= FScrollHorz.SmoothPage;
    //if FOptScrollbarsNew or FOptScrollbarHorizontalHidden then
    //  si.nPage:= si.nMax+1;
    si.nPos:= FScrollHorz.SmoothPos;
    SetScrollInfo(Handle, SB_HORZ, si, True);
  end;

  {$ifdef debug_scroll}
  Writeln(Format('ATSynEdit SetScrollInfo: SB_HORZ, nMin=%d, nMax=%d, nPage=%d, nPos=%d',
    [FScrollHorz.NMin, FScrollHorz.NMax, FScrollHorz.NPage, FScrollHorz.NPos]));
  {$endif}
end;

procedure TATSynEdit.GetRectMain(out R: TRect);
begin
  R.Left:= FRectGutter.Left + FTextOffset.X;
  R.Top:= FTextOffset.Y;
  R.Right:= ClientWidth
    - IfThen(FMinimapVisible and not FMinimapAtLeft, FMinimapWidth)
    - IfThen(FMicromapVisible, FRectMicromap.Width);
  R.Bottom:= ClientHeight;

  FRectMainVisible:= R;

  if FOptScrollSmooth then
  begin
    Dec(R.Left, FScrollHorz.NPixelOffset);
    Dec(R.Top, FScrollVert.NPixelOffset);
  end;
end;

procedure TATSynEdit.GetRectMinimap(out R: TRect);
begin
  if not FMinimapVisible then
  begin
    R:= cRectEmpty;
    exit
  end;

  if FMinimapAtLeft then
    R.Left:= 0
  else
    R.Left:= ClientWidth-FMinimapWidth-IfThen(FMicromapVisible, FRectMicromap.Width);

  R.Right:= R.Left+FMinimapWidth;
  R.Top:= 0;
  R.Bottom:= ClientHeight;
end;

procedure TATSynEdit.GetRectMinimapSel(out R: TRect);
begin
  R.Left:= FRectMinimap.Left;
  R.Right:= FRectMinimap.Right;
  R.Top:= GetMinimapSelTop;
  R.Bottom:= Min(
    R.Top + (GetVisibleLines+1)*FCharSizeMinimap.Y,
    FRectMinimap.Bottom
    );
end;

procedure TATSynEdit.GetRectMicromap(out R: TRect);
var
  NSize: integer;
begin
  NSize:= FMicromap.UpdateSizes(EditorScale(FCharSize.X));

  if not FMicromapVisible then
  begin
    R:= cRectEmpty;
    exit
  end;

  R.Top:= 0;
  R.Bottom:= ClientHeight;
  R.Right:= ClientWidth;
  R.Left:= R.Right-NSize;

  FMicromap.UpdateCoords(R.Left);
  FMicromapScaleDiv:= Max(1, Strings.Count);
end;

procedure TATSynEdit.GetRectGutter(out R: TRect);
begin
  R.Left:= IfThen(FMinimapVisible and FMinimapAtLeft, FMinimapWidth);
  R.Top:= IfThen(FOptRulerVisible, FRulerHeight);
  R.Right:= R.Left + FGutter.Width;
  R.Bottom:= ClientHeight;

  if not FOptGutterVisible then
  begin
    R.Right:= R.Left;
    R.Bottom:= R.Top;
    exit
  end;

  Gutter.GutterLeft:= R.Left;
  Gutter.Update;
end;

procedure TATSynEdit.GetRectRuler(out R: TRect);
begin
  if not FOptRulerVisible then
  begin
    R:= cRectEmpty;
    exit
  end;

  R.Left:= FRectGutter.Left;
  R.Right:= FRectMain.Right;
  R.Top:= 0;
  R.Bottom:= R.Top + FRulerHeight;
end;

procedure TATSynEdit.UpdateClientSizes;
begin
  GetClientSizes(FClientW, FClientH);
end;

procedure TATSynEdit.UpdateInitialVars(C: TCanvas);
begin
  UpdateClientSizes;

  C.Font.Name:= Font.Name;
  C.Font.Size:= DoScaleFont(Font.Size);

  FCharSize:= GetCharSize(C, FCharSpacingText);

  if FMinimapCustomScale<100 then
  begin
    FCharSizeMinimap.X:= EditorScale(1);
    FCharSizeMinimap.Y:= EditorScale(2);
  end
  else
  begin
    FCharSizeMinimap.X:= 1 * FMinimapCustomScale div 100;
    FCharSizeMinimap.Y:= 2 * FMinimapCustomScale div 100;
  end;

  FNumbersIndent:= FCharSize.X * FOptNumbersIndentPercents div 100;
  FRulerHeight:= FCharSize.Y * FOptRulerHeightPercents div 100;

  if FOptGutterVisible and FOptNumbersAutosize then
    UpdateGutterAutosize;
  if FMinimapVisible then
    UpdateMinimapAutosize;

  FTextOffset:= GetTextOffset; //after gutter autosize
  GetRectMicromap(FRectMicromap);
  GetRectMinimap(FRectMinimap); //after FMicromap
  GetRectGutter(FRectGutter);
  GetRectMain(FRectMain); //after gutter/minimap/FMicromap
  GetRectRuler(FRectRuler); //after main
end;

procedure TATSynEdit.DoPaintMainTo(C: TCanvas; ALineFrom: integer);
begin
  if csLoading in ComponentState then Exit;

  UpdateInitialVars(C);

  C.Brush.Color:= FColorBG;
  C.FillRect(ClientRect); //avoid FClientW here to fill entire area

  UpdateAdapterCacheSize;
  UpdateWrapInfo;

  UpdateLinksAttribs;
  DoPaintTextTo(C, FRectMain, FCharSize, FOptGutterVisible, true, FScrollHorz, FScrollVert, ALineFrom);
  DoPaintMarginsTo(C);
  DoPaintNiceScroll(C);

  if FOptRulerVisible then
  begin
    DoPaintRulerTo(C);
    if Assigned(FOnDrawRuler) then
      FOnDrawRuler(Self, C, FRectRuler);
  end;

  if Assigned(FOnDrawEditor) then
    FOnDrawEditor(Self, C, FRectMain);

  if FMinimapVisible then
    DoPaintMinimapTo(C);
  if FMicromapVisible then
    DoPaintMicromapTo(C);

  if FOptBorderFocusedActive and FIsEntered then
    DoPaintBorder(C, Colors.BorderLineFocused, FOptBorderWidthFocused)
  else
    DoPaintBorder(C, Colors.BorderLine, FOptBorderWidth);

  if FOptShowMouseSelFrame then
    if FMouseDragCoord.X>=0 then
      DoPaintMouseSelFrame(C);
end;

procedure TATSynEdit.DoPaintMouseSelFrame(C: TCanvas);
var
  X1, X2, Y1, Y2: integer;
begin
  if not FOptMouseEnableNormalSelection then exit;

  X1:= FMouseDownCoord.X - FScrollHorz.TotalOffset;
  X2:= FMouseDragCoord.X;
  Y1:= FMouseDownCoord.Y - FScrollVert.TotalOffset;
  Y2:= FMouseDragCoord.Y;

  C.DrawFocusRect(Rect(
    Max(-1, Min(X1, X2)),
    Max(-1, Min(Y1, Y2)),
    Min(Width+1, Max(X1, X2)),
    Min(Height+1, Max(Y1, Y2))
    ));
end;

procedure TATSynEdit.DoPaintBorder(C: TCanvas; AColor: TColor; AWidth: integer);
var
  W, H, i: integer;
begin
  if AWidth<1 then exit;
  C.Pen.Color:= AColor;
  W:= ClientWidth;
  H:= ClientHeight;
  for i:= 0 to AWidth-1 do
    C.Frame(i, i, W-i, H-i);
end;

function TATSynEdit.GetCharSize(C: TCanvas; ACharSpacing: TPoint): TPoint;
const
  Sample: string = 'N'; //char 'M' is not ok, it's too wide for var-width fonts
var
  Size: TSize;
  TempC: TCanvas;
begin
  if C.HandleAllocated then
  begin
    Size:= C.TextExtent(Sample);
  end
  else
  begin
    TempC:= TCanvas.Create;
    try
      TempC.Handle:= GetDC(0);
      TempC.Font.Assign(Self.Font);
      Size:= TempC.TextExtent(Sample);
    finally
      FreeAndNil(TempC);
    end;
  end;

  Result.X:= Max(1, Size.cx + ACharSpacing.X);
  Result.Y:= Max(1, Size.cy + ACharSpacing.Y);
end;

procedure TATSynEdit.DoPaintGutterBandBG(C: TCanvas; ABand: integer; AColor: TColor;
  AY1, AY2: integer; AEntireHeight: boolean);
var
  X1, X2: integer;
begin
  with FGutter[ABand] do
  begin
    X1:= Left;
    X2:= Right;
  end;

  if not AEntireHeight then
  begin
    C.Brush.Color:= AColor;
    C.FillRect(X1, AY1, X2, AY2);
  end
  else
  begin
    C.Brush.Color:= AColor;
    C.FillRect(X1, FRectGutter.Top, X2, FRectGutter.Bottom);
  end;
end;

procedure TATSynEdit.DoPaintTextTo(C: TCanvas;
  const ARect: TRect;
  const ACharSize: TPoint;
  AWithGutter, AMainText: boolean;
  var AScrollHorz, AScrollVert: TATSynScrollInfo;
  ALineFrom: integer);
var
  NCoordTop, NCoordSep: integer;
  //
  procedure FillOneLine(AFillColor: TColor; ARectLeft: integer);
  begin
    {$ifdef use_bg}
    if AMainText then
    begin
      C.Brush.Color:= AFillColor;
      C.FillRect(ARectLeft, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y);
    end
    else
      FFastBmp.FillRect(
        ARectLeft - FRectMinimap.Left,
        NCoordTop - FRectMinimap.Top,
        FRectMinimap.Width,
        NCoordTop - FRectMinimap.Top + ACharSize.Y,
        AFillColor);
    {$else}
    C.Brush.Color:= AFillColor;
    C.FillRect(ARectLeft, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y);
    {$endif}
  end;
  //
var
  NWrapIndex, NWrapIndexDummy, NLinesIndex, NLineLen, NCount: integer;
  NOutputCharsSkipped, NOutputCellPercentsSkipped: integer;
  NOutputStrWidth, NOutputMaximalChars: integer;
  WrapItem: TATWrapItem;
  GapItem: TATGapItem;
  Band: TATGutterItem;
  StringItem: PATStringItem;
  NColorEntire, NColorAfter: TColor;
  NColorOfStates: array[TATLineState] of TColor;
  NDimValue, NBandDecor: integer;
  StrOutput: atString;
  CurrPoint, CurrPointText, CoordAfterText: TPoint;
  LineSeparator: TATLineSeparator;
  LineState: TATLineState;
  bLineWithCaret, bLineEolSelected, bLineColorForced, bLineHuge: boolean;
  Event: TATSynEditDrawLineEvent;
  TextOutProps: TATCanvasTextOutProps;
  bCachedMinimap, bUseColorOfCurrentLine: boolean;
  bHiliteLinesWithSelection: boolean;
  bUseSetPixel: boolean;
  NSubPos, NSubLen: integer;
  bTrimmedNonSpaces: boolean;
begin
  bHiliteLinesWithSelection:= not AMainText and FMinimapHiliteLinesWithSelection;
  bUseSetPixel:=
    {$ifndef windows} DoubleBuffered and {$endif}
    (ACharSize.X=1);

  NColorOfStates[cLineStateNone]:= -1;
  NColorOfStates[cLineStateChanged]:= Colors.StateChanged;
  NColorOfStates[cLineStateAdded]:= Colors.StateAdded;
  NColorOfStates[cLineStateSaved]:= Colors.StateSaved;

  //wrap turned off can cause bad scrollpos, fix it
  with AScrollVert do
    NPos:= Min(NPos, NPosLast);

  {$ifdef use_bg}
  if AMainText then
  {$endif}
  begin
    C.Brush.Color:= FColorBG;
    C.FillRect(ARect);
  end;

  if AMainText then
  begin
    if Assigned(FFoldedMarkList) then
      FFoldedMarkList.Clear;
  end;

  if AWithGutter then
  begin
    C.Brush.Color:= Colors.GutterBG;
    C.FillRect(FRectGutter);

    //paint some bands, for full height coloring
    if FGutter[FGutterBandFolding].Visible then
      DoPaintGutterBandBG(C, FGutterBandFolding, Colors.GutterFoldBG, -1, -1, true);
    if FGutter[FGutterBandSeparator].Visible then
      DoPaintGutterBandBG(C, FGutterBandSeparator, Colors.GutterSeparatorBG, -1, -1, true);
    if FGutter[FGutterBandEmpty].Visible then
      DoPaintGutterBandBG(C, FGutterBandEmpty, FColorBG, -1, -1, true);
  end;

  if AMainText and (FTextHint<>'') then
  begin
    NCount:= Strings.Count;
    if (NCount=0) or ((NCount=1) and (Strings.LinesLen[0]=0)) then
    begin
      DoPaintTextHintTo(C);
      Exit
    end;
  end;

  {$ifndef fix_horzscroll}
  AScrollHorz.NMax:= 1;
  {$endif}

  NCoordTop:= ARect.Top;

  if ALineFrom>=0 then
  begin
    FWrapInfo.FindIndexesOfLineNumber(ALineFrom, NWrapIndex, NWrapIndexDummy);
    AScrollVert.NPos:= NWrapIndex;
  end
  else
  begin
    NWrapIndex:= Max(0, AScrollVert.NPos);
  end;

  DoEventBeforeCalcHilite;

  bCachedMinimap:=
    not AMainText and
    FMinimapCachedPainting and
    Assigned(FAdapterHilite) and
    not TempSel_IsMultiline;

  repeat
    if NCoordTop>ARect.Bottom then Break;

    if not FWrapInfo.IsIndexValid(NWrapIndex) then
    begin
      //paint end-of-file arrow
      if AMainText then
        if NWrapIndex>=0 then
          if OptUnprintedVisible and OptUnprintedEof then
            if OptUnprintedEndsDetails then
              DoPaintUnprintedEolText(C,
                'EOF',
                ARect.Left,
                NCoordTop,
                Colors.UnprintedFont,
                Colors.UnprintedBG)
            else
              CanvasArrowHorz(C,
                Rect(ARect.Left, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y),
                Colors.UnprintedFont, OptUnprintedEofCharLength*ACharSize.X,
                false,
                OptUnprintedTabPointerScale);
      Break;
    end;

    WrapItem:= FWrapInfo[NWrapIndex];
    NLinesIndex:= WrapItem.NLineIndex;
    if not Strings.IsIndexValid(NLinesIndex) then Break;

    //support Gap before the 1st line
    if AMainText then
      if (NWrapIndex=0) and AScrollVert.TopGapVisible and (Gaps.SizeOfGapTop>0) then
      begin
        GapItem:= Gaps.Find(-1);
        if Assigned(GapItem) then
        begin
          DoPaintGapTo(C, Rect(ARect.Left, NCoordTop, ARect.Right, NCoordTop+GapItem.Size), GapItem);
          NCoordTop+= GapItem.Size;
        end;
      end;

    {$ifdef atsynedit_cache}
    //speedup painting minimap:
    //if line parts cached, paint them now
    NColorAfter:= clNone;
    if bCachedMinimap then
      if (WrapItem.NLength>0) and
        FAdapterCache.Get(WrapItem.NLineIndex, WrapItem.NCharIndex, FLineParts, NColorAfter) then
        begin
          DoCalcLineEntireColor(
            WrapItem.NLineIndex,
            false,
            NColorEntire,
            bLineColorForced,
            bHiliteLinesWithSelection);

          DoPartSetColorBG(FLineParts, NColorEntire, bLineColorForced);
          if bLineColorForced then
            NColorAfter:= NColorEntire;

          if NColorAfter<>clNone then
            FillOneLine(NColorAfter, ARect.Left);

          CurrPointText:= Point(
            Int64(ARect.Left) + (Int64(WrapItem.NIndent)-AScrollHorz.NPos)*ACharSize.X,
            NCoordTop);

          CanvasTextOutMinimap(
            {$ifdef use_bg} FFastBmp, {$else} C, {$endif}
            ARect,
            CurrPointText.X {$ifdef use_bg} - FRectMinimap.Left {$endif},
            CurrPointText.Y {$ifdef use_bg} - FRectminimap.Top {$endif},
            ACharSize,
            FTabSize,
            FLineParts,
            FColorBG,
            NColorAfter,
            Strings.LineSub(
              WrapItem.NLineIndex,
              WrapItem.NCharIndex,
              GetVisibleColumns), //optimize for huge lines
            bUseSetPixel
            );

          //end of painting line
          Inc(NCoordTop, ACharSize.Y);
          Inc(NWrapIndex);
          Continue;
        end
        else
        begin
          {$ifdef debug_fps}
          if WrapItem.NLength>0 then
            Inc(FMissMinimap);
          {$endif}
        end;
    {$endif}

    //don't update FLineBottom if minimap paints
    if AMainText then
    begin
      FLineBottom:= NLinesIndex;

      if IsFoldLineNeededBeforeWrapitem(NWrapIndex) then
      begin
        NCoordSep:= NCoordTop-1;
        C.Pen.Color:= Colors.CollapseLine;
        CanvasLineHorz(C,
          ARect.Left+FFoldUnderlineOffset,
          NCoordSep,
          ARect.Right-FFoldUnderlineOffset
          );
      end;
    end;

    //prepare line
    NOutputCharsSkipped:= 0;
    NOutputCellPercentsSkipped:= 0;
    NOutputStrWidth:= 0;

    CurrPoint.X:= ARect.Left;
    CurrPoint.Y:= NCoordTop;
    CurrPointText.X:= CurrPoint.X
                      + WrapItem.NIndent*ACharSize.X
                      - AScrollHorz.SmoothPos
                      + AScrollHorz.NPixelOffset;
    CurrPointText.Y:= CurrPoint.Y;

    bTrimmedNonSpaces:= false;
    if AMainText then
    begin
      NLineLen:= Strings.LinesLen[NLinesIndex];
      bLineHuge:= NLineLen>OptMaxLineLenForAccurateCharWidths;
      if not bLineHuge then
      begin
        //little slow for huge lines
        NSubPos:= WrapItem.NCharIndex;
        NSubLen:= Min(WrapItem.NLength, GetVisibleColumns+AScrollHorz.NPos+1+6);
          //+1 because of NPixelOffset
          //+6 because of HTML color underlines
        StrOutput:= Strings.LineSub(NLinesIndex, NSubPos, NSubLen);

        if FUnprintedSpacesTrailing then
          bTrimmedNonSpaces:= NSubPos+NSubLen <= Strings.LineLenWithoutSpace(NLinesIndex);

        if WrapItem.bInitial then
        begin
          //very slow for huge lines
          FTabHelper.FindOutputSkipOffset(
            NLinesIndex,
            StrOutput,
            AScrollHorz.NPos,
            NOutputCharsSkipped,
            NOutputCellPercentsSkipped);
          Delete(StrOutput, 1, NOutputCharsSkipped);
        end;
      end
      else
      begin
        //work faster for huge lines (but not accurate horiz scrollbar)
        NOutputCharsSkipped:= AScrollHorz.NPos;
        NOutputCellPercentsSkipped:= NOutputCharsSkipped*100;

        NSubPos:= WrapItem.NCharIndex + NOutputCharsSkipped;
        NSubLen:= Min(WrapItem.NLength, GetVisibleColumns+AScrollHorz.NPos+1+6);
          //+1 because of NPixelOffset
          //+6 because of HTML color underlines
        StrOutput:= Strings.LineSub(NLinesIndex, NSubPos, NSubLen);

        if FUnprintedSpacesTrailing then
          bTrimmedNonSpaces:= NSubPos+NSubLen <= Strings.LineLenWithoutSpace(NLinesIndex);
      end;

      Inc(CurrPointText.X, NOutputCellPercentsSkipped * ACharSize.X div 100);

      if Length(StrOutput)>cMaxCharsForOutput then
        SetLength(StrOutput, cMaxCharsForOutput);
    end
    else
    begin
      //work very fast for minimap, take LineSub from start
      StrOutput:= Strings.LineSub(
        NLinesIndex,
        1,
        Min(WrapItem.NLength, GetVisibleColumns)
        );
    end;

    LineSeparator:= Strings.LinesSeparator[NLinesIndex];
    bLineWithCaret:= AMainText and IsLineWithCaret(NLinesIndex);
    bLineEolSelected:= AMainText and IsPosSelected(WrapItem.NCharIndex-1+WrapItem.NLength, WrapItem.NLineIndex);

    if AMainText then
    begin
      //horz scrollbar max: is calculated here, to make variable horz bar
      //vert scrollbar max: is calculated in UpdateScrollbars
      if bLineHuge then
        NOutputMaximalChars:= NLineLen //approximate, it don't consider CJK chars, but OK for huge lines
      else
      begin
        StringItem:= Strings.GetItemPtr(NLinesIndex);
        if StringItem^.HasAsciiNoTabs then
          NOutputMaximalChars:= StringItem^.CharLen
        else
          NOutputMaximalChars:= CanvasTextWidth(
            StringItem^.Line,
            NLinesIndex,
            FTabHelper,
            1 //pass CharWidth=1px
            );
      end;
      AScrollHorz.NMax:= Max(AScrollHorz.NMax, NOutputMaximalChars + FOptScrollbarHorizontalAddSpace);
    end;

    C.Brush.Color:= FColorBG;
    C.Font.Color:= FColorFont;

    bUseColorOfCurrentLine:= false;
    if bLineWithCaret then
      if FOptShowCurLine and (not FOptShowCurLineOnlyFocused or FIsEntered) then
      begin
        if FOptShowCurLineMinimal then
          bUseColorOfCurrentLine:= IsLinePartWithCaret(NLinesIndex, NCoordTop)
        else
          bUseColorOfCurrentLine:= true;
      end;

    DoCalcLineEntireColor(
      NLinesIndex,
      bUseColorOfCurrentLine,
      NColorEntire,
      bLineColorForced,
      bHiliteLinesWithSelection);

    if AMainText and FOptZebraActive then
      if (NLinesIndex+1) mod FOptZebraStep = 0 then
        NColorEntire:= ColorBlend(NColorEntire, FColorFont, FOptZebraAlphaBlend);

    FillOneLine(NColorEntire, ARect.Left);

    //paint line
    if StrOutput<>'' then
    begin
      if WrapItem.NIndent>0 then
      begin
        NColorAfter:= FColorBG;
        DoCalcPosColor(WrapItem.NCharIndex, NLinesIndex, NColorAfter);
        DoPaintLineIndent(C, ARect, ACharSize,
          NCoordTop, WrapItem.NIndent,
          NColorAfter,
          AScrollHorz.NPos, AMainText and FOptShowIndentLines);
      end;

      NColorAfter:= clNone;
      DoCalcLineHilite(WrapItem, FLineParts{%H-},
        NOutputCharsSkipped, cMaxCharsForOutput,
        NColorEntire, bLineColorForced,
        NColorAfter, AMainText);

      if FLineParts[0].Offset<0 then
      begin
        //some bug in making parts! to fix!
        //raise Exception.Create('Program bug in text renderer, report to author!');
        C.Font.Color:= clRed;
        C.TextOut(CurrPointText.X, CurrPointText.Y, 'Program bug in text renderer, report to author!');
        Break;
      end;

      //apply DimRanges
      if Assigned(FDimRanges) then
      begin
        NDimValue:= FDimRanges.GetDimValue(WrapItem.NLineIndex, -1);
        if NDimValue>0 then //-1: no ranges found, 0: no effect
          DoPartsDim(FLineParts, NDimValue, FColorBG);
      end;

      //adapter may return ColorAfterEol, paint it
      if FOptShowFullHilite then
        if NColorAfter<>clNone then
          FillOneLine(NColorAfter, CurrPointText.X);

      if AWithGutter then
        Event:= FOnDrawLine
      else
        Event:= nil;

      if OptUnprintedReplaceSpec then
        SRemoveAsciiControlChars(StrOutput, WideChar(OptUnprintedReplaceSpecToCode));

      if AMainText then
      begin
        TextOutProps.Editor:= Self;
        TextOutProps.SuperFast:= bLineHuge;
        TextOutProps.TabHelper:= FTabHelper;
        TextOutProps.LineIndex:= NLinesIndex;
        TextOutProps.CharIndexInLine:= WrapItem.NCharIndex;
        TextOutProps.CharSize:= ACharSize;
        TextOutProps.CharsSkipped:= NOutputCellPercentsSkipped div 100;
        TextOutProps.TrimmedTrailingNonSpaces:= bTrimmedNonSpaces;
        TextOutProps.DrawEvent:= Event;
        TextOutProps.ControlWidth:= ClientWidth+ACharSize.X*2;
        TextOutProps.TextOffsetFromLine:= FOptTextOffsetFromLine;

        TextOutProps.ShowUnprinted:= AMainText and FUnprintedVisible and FUnprintedSpaces;
        TextOutProps.ShowUnprintedSpacesTrailing:= FUnprintedSpacesTrailing;
        TextOutProps.ShowUnprintedSpacesBothEnds:= FUnprintedSpacesBothEnds;
        TextOutProps.ShowUnprintedSpacesOnlyInSelection:= FUnprintedSpacesOnlyInSelection;
        TextOutProps.DetectIsPosSelected:= @IsPosSelected;

        TextOutProps.ShowFontLigatures:= FOptShowFontLigatures and (not bLineWithCaret);
        TextOutProps.ColorNormalFont:= Colors.TextFont;
        TextOutProps.ColorUnprintedFont:= Colors.UnprintedFont;
        TextOutProps.ColorUnprintedHexFont:= Colors.UnprintedHexFont;

        TextOutProps.FontNormal_Name:= Font.Name;
        TextOutProps.FontNormal_Size:= DoScaleFont(Font.Size);

        TextOutProps.FontItalic_Name:= FontItalic.Name;
        TextOutProps.FontItalic_Size:= DoScaleFont(FontItalic.Size);

        TextOutProps.FontBold_Name:= FontBold.Name;
        TextOutProps.FontBold_Size:= DoScaleFont(FontBold.Size);

        TextOutProps.FontBoldItalic_Name:= FontBoldItalic.Name;
        TextOutProps.FontBoldItalic_Size:= DoScaleFont(FontBoldItalic.Size);

        CanvasTextOut(C,
          CurrPointText.X,
          CurrPointText.Y,
          StrOutput,
          @FLineParts,
          NOutputStrWidth,
          TextOutProps
          );

        //paint selection bg, after applying ColorAfterEol
        DoPaintSelectedLineBG(C, ACharSize, ARect,
          CurrPoint,
          CurrPointText,
          WrapItem,
          NOutputStrWidth,
          AScrollHorz);
      end
      else
      if StrOutput<>'' then
        CanvasTextOutMinimap(
          {$ifdef use_bg} FFastBmp, {$else} C, {$endif}
          ARect,
          CurrPointText.X {$ifdef use_bg} - FRectMinimap.Left {$endif},
          CurrPointText.Y {$ifdef use_bg} - FRectminimap.Top {$endif},
          ACharSize,
          FTabSize,
          FLineParts,
          FColorBG,
          NColorAfter,
          Strings.LineSub(
            WrapItem.NLineIndex,
            WrapItem.NCharIndex,
            GetVisibleColumns), //optimize for huge lines
          bUseSetPixel
          );

      //restore after textout
      C.Font.Style:= Font.Style;
    end
    else
    //paint empty line bg
    begin
      if FOptShowFullHilite then
      begin
        NColorAfter:= clNone;
        DoCalcPosColor(0, NLinesIndex, NColorAfter);
        if NColorAfter<>clNone then
          FillOneLine(NColorAfter, ARect.Left);
      end;

      DoPaintSelectedLineBG(C, ACharSize, ARect,
        CurrPoint,
        CurrPointText,
        WrapItem,
        0,
        AScrollHorz);
    end;

    if AMainText then
    begin
      CoordAfterText.X:= CurrPointText.X+NOutputStrWidth;
      CoordAfterText.Y:= CurrPointText.Y;

      if WrapItem.NFinal=cWrapItemFinal then
      begin
        //for OptShowFullWidthForSelection=false paint eol bg
        if bLineEolSelected then
        begin
          C.Brush.Color:= Colors.TextSelBG;
          C.FillRect(
            CoordAfterText.X,
            CoordAfterText.Y,
            CoordAfterText.X+ACharSize.X,
            CoordAfterText.Y+ACharSize.Y);
        end;

        //paint eol mark
        if FUnprintedVisible and FUnprintedEnds then
        begin
          if OptUnprintedEndsDetails then
            DoPaintUnprintedEolText(C,
              cLineEndNiceNames[Strings.LinesEnds[WrapItem.NLineIndex]],
              CoordAfterText.X,
              CoordAfterText.Y,
              Colors.UnprintedFont,
              Colors.UnprintedBG)
          else
            DoPaintUnprintedEolArrow(C,
              CoordAfterText.X,
              CoordAfterText.Y,
              ACharSize,
              Colors.UnprintedFont);
        end;
      end
      else
      begin
        //paint wrapped-line-part mark
        if FUnprintedVisible and FUnprintedEnds then
          DoPaintUnprintedWrapMark(C,
            CoordAfterText.X,
            CoordAfterText.Y,
            ACharSize,
            Colors.UnprintedFont);
      end;

      //draw collapsed-mark
      if WrapItem.NFinal=cWrapItemCollapsed then
        DoPaintFoldedMark(C,
          Strings.LinesFoldFrom[NLinesIndex, FEditorIndex]-1,
          NLinesIndex,
          CoordAfterText.X,
          CoordAfterText.Y,
          GetFoldedMarkText(NLinesIndex));
    end;

    //draw separators
    if LineSeparator<>cLineSepNone then
    begin
      if LineSeparator=cLineSepTop then
        NCoordSep:= NCoordTop
      else
        NCoordSep:= NCoordTop+ACharSize.Y-1;
      C.Pen.Color:= Colors.BlockSepLine;
      CanvasLineHorz(C, ARect.Left, NCoordSep, ARect.Right);
    end;

    //draw gutter
    if AWithGutter then
    begin
      //paint area over scrolled text
      C.Brush.Color:= Colors.GutterBG;
      C.FillRect(FRectGutter.Left, NCoordTop, FRectGutter.Right, NCoordTop+ACharSize.Y);

      //gutter band: number
      Band:= FGutter[FGutterBandNumbers];
      if Band.Visible then
      begin
        if bLineWithCaret and FOptShowGutterCaretBG then
        begin
          DoPaintGutterBandBG(C, FGutterBandNumbers, Colors.GutterCaretBG, NCoordTop, NCoordTop+ACharSize.Y, false);
          C.Font.Color:= Colors.GutterCaretFont;
        end
        else
          C.Font.Color:= Colors.GutterFont;

        if WrapItem.bInitial then
          DoPaintLineNumber(C, NLinesIndex, NCoordTop, Band);
      end;

      //gutter decor
      NBandDecor:= FGutterBandDecor;
      if NBandDecor<0 then
        NBandDecor:= FGutterBandBookmarks;

      Band:= FGutter[NBandDecor];
      if Band.Visible then
        if WrapItem.bInitial then
          DoPaintGutterDecor(C, NLinesIndex,
            Rect(
              Band.Left,
              NCoordTop,
              Band.Right,
              NCoordTop+ACharSize.Y
              ));

      //gutter band: bookmark
      Band:= FGutter[FGutterBandBookmarks];
      if Band.Visible then
        if WrapItem.bInitial then
        begin
          if Strings.Bookmarks.Find(NLinesIndex)>=0 then
            DoEventDrawBookmarkIcon(C, NLinesIndex,
              Rect(
                Band.Left,
                NCoordTop,
                Band.Right,
                NCoordTop+ACharSize.Y
                ));
        end;

      //gutter band: fold
      Band:= FGutter[FGutterBandFolding];
      if Band.Visible then
      begin
        DoPaintGutterBandBG(C, FGutterBandFolding, Colors.GutterFoldBG, NCoordTop, NCoordTop+ACharSize.Y, false);
        DoPaintGutterFolding(C,
          NWrapIndex,
          Band.Left,
          Band.Right,
          NCoordTop,
          NCoordTop+ACharSize.Y
          );
      end;

      //gutter band: state
      Band:= FGutter[FGutterBandStates];
      if Band.Visible then
      begin
        LineState:= Strings.LinesState[NLinesIndex];
        if LineState<>cLineStateNone then
          DoPaintGutterBandBG(C,
            FGutterBandStates,
            NColorOfStates[LineState],
            NCoordTop,
            NCoordTop+ACharSize.Y,
            false);
      end;

      //gutter band: separator
      if FGutter[FGutterBandSeparator].Visible then
        DoPaintGutterBandBG(C, FGutterBandSeparator, Colors.GutterSeparatorBG, NCoordTop, NCoordTop+ACharSize.Y, false);
      //gutter band: empty indent
      if FGutter[FGutterBandEmpty].Visible then
        DoPaintGutterBandBG(C, FGutterBandEmpty, FColorBG, NCoordTop, NCoordTop+ACharSize.Y, false);
    end;

    //end of painting line
    Inc(NCoordTop, ACharSize.Y);
    Inc(NWrapIndex);

    //consider gap (not for minimap)
    if AMainText and (WrapItem.NFinal=cWrapItemFinal) then
    begin
      GapItem:= Gaps.Find(NLinesIndex);
      if Assigned(GapItem) then
      begin
        DoPaintGapTo(C, Rect(ARect.Left, NCoordTop, ARect.Right, NCoordTop+GapItem.Size), GapItem);
        NCoordTop+= GapItem.Size;
      end;
    end;
  until false;

  //staples
  if AMainText then
    DoPaintStaples(C, ARect, ACharSize, AScrollHorz);
end;

function TATSynEdit.GetMinimapSelTop: integer;
begin
  Result:= FRectMinimap.Top + (Max(0, FScrollVert.NPos)-FScrollVertMinimap.NPos)*FCharSizeMinimap.Y;
end;

function TATSynEdit.GetMinimapActualHeight: integer;
begin
  Result:=
    Max(2, Min(
      FRectMinimap.Height,
      FWrapInfo.Count*FCharSizeMinimap.Y
      ));
end;

function TATSynEdit.GetMinimap_DraggedPosToWrapIndex(APosY: integer): integer;
var
  NCount, NScrollPos, NScrollMax, NScrollMax2: integer;
begin
  NCount:= FWrapInfo.Count;
  NScrollPos:= Max(0, APosY-FMouseDragMinimapDelta);

  //for big files
  NScrollMax:= Max(0, FRectMinimap.Height-FMouseDragMinimapSelHeight);

  //for small files: minimap drag must not be until bottom
  NScrollMax2:= NCount*FCharSizeMinimap.Y;
  if not FOptLastLineOnTop then
    NScrollMax2:= Max(0, NScrollMax2-FMouseDragMinimapSelHeight);

  if NScrollMax>NScrollMax2 then
    NScrollMax:= NScrollMax2;

  if NScrollMax>0 then
  begin
    Result:= Int64(FScrollVert.NPosLast) * NScrollPos div NScrollMax;
    Result:= Min(NCount-1, Result);
  end
  else
    Result:= 0;
end;

function TATSynEdit.GetMinimap_ClickedPosToWrapIndex(APosY: integer): integer;
begin
  Result:= (APosY-FRectMinimap.Top) div FCharSizeMinimap.Y + FScrollVertMinimap.NPos;
  if not FWrapInfo.IsIndexValid(Result) then
    Result:= -1;
end;

function TATSynEdit.GetOptTextOffsetTop: integer;
begin
  if ModeOneLine then
    Result:= (ClientHeight - TextCharSize.Y) div 2
  else
    Result:= FOptTextOffsetTop;
end;

function _IsColorDark(N: TColor): boolean;
const
  cMax = $60;
begin
  Result:= (Red(N)<cMax) and (Green(N)<cMax) and (Blue(N)<cMax);
end;

{$ifdef use_bg}
procedure TATSynEdit.DoPaintMinimapSelTo_BGRA(C: TBGRABitmap);
var
  R: TRect;
  rColor: TBGRAPixel;
begin
  if FMinimapShowSelAlways or FCursorOnMinimap then
  begin
    GetRectMinimapSel(R);
    OffsetRect(R, -FRectMinimap.Left, -FRectMinimap.Top);

    // https://forum.lazarus.freepascal.org/index.php/topic,51383.msg377195.html#msg377195
    if _IsColorDark(Colors.TextBG) then
      rColor.FromRGB(255, 255, 255, FMinimapSelColorChange*255 div 100)
    else
      rColor.FromRGB(0, 0, 0, FMinimapSelColorChange*255 div 100);

    C.FillRect(R, rColor, dmDrawWithTransparency);

    if FMinimapShowSelBorder then
    begin
      rColor.FromColor(Colors.MinimapBorder);
      C.Rectangle(R, rColor);
    end;
  end;

  if Colors.MinimapBorder<>clNone then
  begin
    rColor.FromColor(Colors.MinimapBorder);
    C.DrawVertLine(0, 0, FRectMinimap.Height, rColor);
  end;
end;
{$else}
procedure TATSynEdit.DoPaintMinimapSelTo(C: TCanvas);
var
  R: TRect;
begin
  if FMinimapShowSelAlways or FCursorOnMinimap then
  begin
    GetRectMinimapSel(R);
    //if IntersectRect(R, R, FRectMinimap) then
    begin
      CanvasInvertRect(C, R, Colors.MinimapSelBG);
      if FMinimapShowSelBorder then
      begin
        C.Pen.Color:= Colors.MinimapBorder;
        C.Brush.Style:= bsClear;
        C.Rectangle(R);
        C.Brush.Style:= bsSolid;
      end;
    end;
  end;

  if Colors.MinimapBorder<>clNone then
  begin
    C.Pen.Color:= Colors.MinimapBorder;
    CanvasLineVert(C, FRectMinimap.Left-1, FRectMinimap.Top, FRectMinimap.Bottom);
  end;
end;
{$endif}

procedure TATSynEdit.DoPaintMinimapTo(C: TCanvas);
begin
  {$ifdef debug_fps}
  FTickMinimap:= GetTickCount64;
  {$endif}

  FScrollHorzMinimap.Clear;
  FScrollVertMinimap.Clear;

  FScrollVertMinimap.NPos:= GetMinimapScrollPos;
  FScrollVertMinimap.NPosLast:= MaxInt div 2;

  {$ifdef use_bg}
  FFastBmp.SetSize(FRectMinimap.Width, FRectMinimap.Height);
  FFastBmp.Fill(FColorBG);
  {$endif}

  DoPaintTextTo(C, FRectMinimap, FCharSizeMinimap, false, false, FScrollHorzMinimap, FScrollVertMinimap, -1);

  {$ifdef use_bg}
  DoPaintMinimapSelTo_BGRA(FFastBmp);
  {$else}
  DoPaintMinimapSelTo(C);
  {$endif}

  {$ifdef use_bg}
  FFastBmp.Draw(C, FRectMinimap.Left, FRectMinimap.Top);
  {$endif}

  {$ifdef debug_fps}
  FTickMinimap:= GetTickCount64-FTickMinimap;
  {$endif}
end;

procedure TATSynEdit.DoPaintMicromapTo(C: TCanvas);
begin
  if Assigned(FOnDrawMicromap) then
    FOnDrawMicromap(Self, C, FRectMicromap)
  else
  begin
    C.Brush.Color:= clCream;
    C.FillRect(FRectMicromap);
  end;
end;


procedure TATSynEdit.DoPaintGapTo(C: TCanvas; const ARect: TRect; AGap: TATGapItem);
var
  RHere, RBmp: TRect;
  NColor: TColor;
begin
  NColor:= AGap.Color;
  if NColor<>clNone then
  begin
    C.Brush.Color:= NColor;
    C.FillRect(ARect);
  end;

  if AGap.Bitmap<>nil then
  begin
    RBmp:= Rect(0, 0, AGap.Bitmap.Width, AGap.Bitmap.Height);
    //RHere is rect of bitmap's size, located at center of ARect
    RHere.Left:= GetGapBitmapPosLeft(ARect, AGap.Bitmap);
    RHere.Top:= (ARect.Top+ARect.Bottom-RBmp.Bottom) div 2;
    RHere.Right:= RHere.Left + RBmp.Right;
    RHere.Bottom:= RHere.Top + RBmp.Bottom;
    C.CopyRect(RHere, AGap.Bitmap.Canvas, RBmp);
  end
  else
  if Assigned(FOnDrawGap) then
    FOnDrawGap(Self, C, ARect, AGap);
end;

procedure TATSynEdit.DoPaintMarginLineTo(C: TCanvas; AX, AWidth: integer; AColor: TColor);
var
  XFrom, XTo, X: integer;
begin
  if (AX>=FRectMain.Left) and (AX<FRectMain.Right) then
  begin
    C.Pen.Color:= AColor;
    CanvasLineVert2(C, AX, FRectMain.Top, FRectMain.Bottom, false, AWidth);
  end;
end;

procedure TATSynEdit.DoPaintMarginsTo(C: TCanvas);
  //
  function PosX(NMargin: integer): integer; inline;
  begin
    Result:= FRectMain.Left + FCharSize.X*(NMargin-FScrollHorz.NPos);
  end;
var
  NWidth, i: integer;
begin
  NWidth:= EditorScale(1);
  if FMarginRight>1 then
    DoPaintMarginLineTo(C, PosX(FMarginRight), NWidth, Colors.MarginRight);
  for i:= 0 to Length(FMarginList)-1 do
    DoPaintMarginLineTo(C, PosX(FMarginList[i]), NWidth, Colors.MarginUser);
end;


procedure TATSynEdit.DoPaintFoldedMark(C: TCanvas;
  APosX, APosY, ACoordX, ACoordY: integer;
  const AMarkText: string);
var
  NWidth: integer;
  Str: string;
  RectMark: TRect;
  FoldMark: TATFoldedMark;
begin
  Str:= AMarkText;

  SDeleteFrom(Str, #10);
    //e.g. Diff lexer gives collapsed-string with EOL (several lines)

  Str:= FTabHelper.TabsToSpaces(APosY, UTF8Decode(Str));
    //expand tabs too

  if APosX>0 then
    Inc(ACoordX, cFoldedMarkIndentOuter);

  //set colors:
  //if 1st chars selected, then use selection-color
  if IsPosSelected(APosX, APosY) then
  begin
    C.Font.Color:= Colors.TextSelFont;
    C.Brush.Color:= Colors.TextSelBG;
  end
  else
  begin
    C.Font.Color:= Colors.CollapseMarkFont;
    C.Brush.Color:= Colors.CollapseMarkBG;
  end;

  //paint text
  C.TextOut(
    ACoordX+cFoldedMarkIndentInner,
    ACoordY+FOptTextOffsetFromLine,
    Str);
  NWidth:= C.TextWidth(Str) + 2*cFoldedMarkIndentInner;

  //paint frame
  RectMark:= Rect(ACoordX, ACoordY, ACoordX+NWidth, ACoordY+FCharSize.Y);
  C.Pen.Color:= Colors.CollapseMarkBorder;
  C.Brush.Style:= bsClear;
  C.Rectangle(RectMark);
  C.Brush.Style:= bsSolid;

  if FFoldTooltipVisible then
  begin
    FoldMark.Init(
      RectMark,
      APosY,
      APosY + DoGetFoldedMarkLinesCount(APosY) -1
      );

    InitFoldedMarkList;
    FFoldedMarkList.Add(FoldMark);
  end;
end;

function TATSynEdit.GetCharSpacingY: integer;
begin
  Result:= FCharSpacingText.Y;
end;

function TATSynEdit.GetMarginString: string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Length(FMarginList)-1 do
    Result+= IntToStr(FMarginList[i]) + ' ';
  Result:= Trim(Result);
end;

function TATSynEdit.GetReadOnly: boolean;
begin
  Result:= Strings.ReadOnly;
end;

function TATSynEdit.GetLineTop: integer;
var
  N: integer;
begin
  Result:= 0;
  N:= Max(0, FScrollVert.NPos);
  if FWrapInfo.IsIndexValid(N) then
    Result:= FWrapInfo[N].NLineIndex;
end;

function TATSynEdit.GetColumnLeft: integer;
begin
  Result:= FScrollHorz.NPos;
end;

constructor TATSynEdit.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;

  //GlobalCharSizer should be created after MainForm is inited
  if not Assigned(GlobalCharSizer) then
    GlobalCharSizer:= TATCharSizer.Create(AOwner);

  if not Assigned(cBitmapNiceScroll) then
    InitEditorResources;

  Caption:= '';
  ControlStyle:= ControlStyle+[csOpaque, csDoubleClicks, csTripleClicks];
  DoubleBuffered:= EditorDoubleBufferedNeeded;
  BorderStyle:= bsNone;
  TabStop:= true;

  Width:= 300;
  Height:= 250;
  Font.Name:= 'Courier New';
  Font.Size:= 9;

  FFontItalic:= TFont.Create;
  FFontItalic.Name:= '';
  FFontBold:= TFont.Create;
  FFontBold.Name:= '';
  FFontBoldItalic:= TFont.Create;
  FFontBoldItalic.Name:= '';

  FScrollbarVert:= TATScrollbar.Create(Self);
  FScrollbarVert.Hide;
  FScrollbarVert.Parent:= Self;
  FScrollbarVert.Align:= alRight;
  FScrollbarVert.Kind:= sbVertical;
  FScrollbarVert.Cursor:= crArrow;
  FScrollbarVert.Width:= ATScrollbarTheme.InitialSize;
  FScrollbarVert.Update;
  FScrollbarVert.OnChange:= @OnNewScrollbarVertChanged;

  FScrollbarHorz:= TATScrollbar.Create(Self);
  FScrollbarHorz.Hide;
  FScrollbarHorz.Parent:= Self;
  FScrollbarHorz.Align:= alBottom;
  FScrollbarHorz.Kind:= sbHorizontal;
  FScrollbarHorz.Cursor:= crArrow;
  FScrollbarHorz.Height:= ATScrollbarTheme.InitialSize;
  FScrollbarHorz.IndentCorner:= 100;
  FScrollbarHorz.Update;
  FScrollbarHorz.OnChange:= @OnNewScrollbarHorzChanged;

  FCaretPropsNormal:= TATCaretProps.Create;
  FCaretPropsOverwrite:= TATCaretProps.Create;
  FCaretPropsReadonly:= TATCaretProps.Create;

  FCaretPropsNormal.Width:= 2;
  FCaretPropsNormal.Height:= -100;
  FCaretPropsOverwrite.Width:= -100;
  FCaretPropsOverwrite.Height:= -100;
  FCaretPropsReadonly.Width:= -100;
  FCaretPropsReadonly.Height:= 2;

  FWantTabs:= true;
  FWantReturns:= true;
  FCharSize:= Point(4, 4); //not nul
  FEditorIndex:= 0;

  FCarets:= TATCarets.Create;
  FCarets.Add(0, 0);
  FCarets.OnCaretChanged:= @DoCaretsOnChanged;

  FCaretShowEnabled:= false; //code sets it On in DoEnter
  FCaretShown:= false;
  FCaretBlinkEnabled:= true;
  FCaretVirtual:= true;
  FCaretSpecPos:= false;
  FCaretStopUnfocused:= true;
  FCaretHideUnfocused:= true;

  FTabHelper:= TATStringTabHelper.Create;
  FMarkers:= nil;
  FAttribs:= nil;
  FMarkedRange:= nil;
  FDimRanges:= nil;
  FHotspots:= nil;
  FAdapterCache:= TATAdapterHiliteCache.Create;
  FLinkCache:= TATLinkCache.Create;

  {$ifdef use_bg}
  FFastBmp:= TBGRABitmap.Create;
  {$endif}

  {$ifdef windows}
  FAdapterIME:= TATAdapterIMEStandard.Create;
  {$endif}

  FPaintLocked:= 0;
  FPaintFlags:= [cPaintUpdateBitmap];

  FColors:= TATSynEditColors.Create;
  InitDefaultColors(FColors);
  InitEditorMouseActions(FMouseActions);

  FCursorText:= crIBeam;
  FCursorColumnSel:= crCross;
  FCursorGutterBookmark:= crHandPoint;
  FCursorGutterNumbers:= crDefault;
  FCursorMinimap:= crDefault;
  FCursorMicromap:= crDefault;

  FTimerIdle:= TTimer.Create(Self);
  FTimerIdle.Enabled:= false;
  FTimerIdle.OnTimer:=@TimerIdleTick;

  FTimerBlink:= TTimer.Create(Self);
  SetCaretBlinkTime(cInitCaretBlinkTime);
  FTimerBlink.OnTimer:= @TimerBlinkTick;
  FTimerBlink.Enabled:= false; //true;

  FTimerScroll:= TTimer.Create(Self);
  FTimerScroll.Interval:= cInitTimerAutoScroll;
  FTimerScroll.OnTimer:= @TimerScrollTick;
  FTimerScroll.Enabled:= false;

  FTimerNiceScroll:= TTimer.Create(Self);
  FTimerNiceScroll.Interval:= cInitTimerNiceScroll;
  FTimerNiceScroll.OnTimer:= @TimerNiceScrollTick;
  FTimerNiceScroll.Enabled:= false;

  FBitmap:= Graphics.TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= cInitBitmapWidth;
  FBitmap.Height:= cInitBitmapHeight;

  FOptUndoLimit:= cInitUndoLimit;
  FStringsExternal:= nil;
  FStringsInt:= TATStrings.Create(FOptUndoLimit);
  FStringsInt.OnGetCaretsArray:= @GetCaretsArray;
  FStringsInt.OnGetMarkersArray:= @GetMarkersArray;
  FStringsInt.OnSetCaretsArray:= @SetCaretsArray;
  FStringsInt.OnSetMarkersArray:= @SetMarkersArray;
  FStringsInt.OnProgress:= @DoStringsOnProgress;
  FStringsInt.OnChange:= @DoStringsOnChange;

  FFold:= TATSynRanges.Create;
  FFoldStyle:= cInitFoldStyle;
  FFoldEnabled:= true;
  FFoldUnderlineOffset:= cInitFoldUnderlineOffset;
  FFoldTooltipVisible:= cInitFoldTooltipVisible;
  FFoldTooltipWidthPercents:= cInitFoldTooltipWidthPercents;
  FFoldTooltipLineCount:= cInitFoldTooltipLineCount;

  FWrapInfo:= TATWrapInfo.Create;
  FWrapInfo.StringsObj:= FStringsInt;

  FWrapTemps:= TATWrapItems.Create;
  FWrapUpdateNeeded:= true;
  FWrapMode:= cInitWrapMode;
  FWrapColumn:= cInitMarginRight;
  FWrapIndented:= true;
  FWrapAddSpace:= 1;
  FWrapEnabledForMaxLines:= cInitWrapEnabledForMaxLines;

  FMicromap:= TATMicromap.Create;

  FOverwrite:= false;
  FTabSize:= cInitTabSize;
  FMarginRight:= cInitMarginRight;
  SetLength(FMarginList, 0);
  FFoldedMarkList:= nil;
  FOptIdleInterval:= cInitIdleInterval;

  FOptAutoCloseBrackets:= '([{';
  FOptAutocompleteAutoshowCharCount:= 0;
  FOptAutocompleteTriggerChars:= '';
  FOptAutocompleteCommitChars:= ' ,;/\''"';
  FOptAutocompleteCloseChars:= '<>()[]{}=';
  FOptAutocompleteAddOpeningBracket:= true;
  FOptAutocompleteUpDownAtEdge:= 1; //cudWrap

  FShowOsBarVert:= false;
  FShowOsBarHorz:= false;

  FUnprintedVisible:= true;
  FUnprintedSpaces:= true;
  FUnprintedSpacesTrailing:= false;
  FUnprintedSpacesBothEnds:= false;
  FUnprintedSpacesOnlyInSelection:= false;
  FUnprintedEnds:= true;
  FUnprintedEndsDetails:= true;
  FUnprintedEof:= true;

  FTextHint:= '';
  FTextHintFontStyle:= [fsItalic];
  FTextHintCenter:= false;

  FGutter:= TATGutter.Create;
  FGutterDecor:= nil;

  FOptGutterVisible:= true;
  FOptGutterPlusSize:= cInitGutterPlusSize;
  FOptGutterShowFoldAlways:= true;
  FOptGutterShowFoldLines:= true;
  FOptGutterShowFoldLinesAll:= false;
  FOptGutterShowFoldLinesForCaret:= true;
  FOptGutterIcons:= cGutterIconsPlusMinus;

  FGutterBandBookmarks:= 0;
  FGutterBandNumbers:= 1;
  FGutterBandStates:= 2;
  FGutterBandFolding:= 3;
  FGutterBandSeparator:= 4;
  FGutterBandEmpty:= 5;
  FGutterBandDecor:= -1;

  for i:= 1 to cGutterBands do
    FGutter.Add(10);
  FGutter[FGutterBandBookmarks].Size:= cGutterSizeBm;
  FGutter[FGutterBandBookmarks].Scaled:= true;
  FGutter[FGutterBandNumbers].Size:= cGutterSizeNum;
  FGutter[FGutterBandStates].Size:= cGutterSizeState;
  FGutter[FGutterBandStates].Scaled:= true;
  FGutter[FGutterBandFolding].Size:= cGutterSizeFold;
  FGutter[FGutterBandFolding].Scaled:= true;
  FGutter[FGutterBandSeparator].Size:= cGutterSizeSep;
  FGutter[FGutterBandEmpty].Size:= cGutterSizeEmpty;
  FGutter[FGutterBandSeparator].Visible:= false;
  FGutter.Update;

  FOptNumbersAutosize:= true;
  FOptNumbersAlignment:= taRightJustify;
  FOptNumbersStyle:= cInitNumbersStyle;
  FOptNumbersShowFirst:= true;
  FOptNumbersShowCarets:= false;
  FOptNumbersSkippedChar:= '.';
  FOptNumbersIndentPercents:= cInitNumbersIndentPercents;

  FOptBorderWidth:= 0;
  FOptBorderWidthFocused:= 0;
  FOptBorderFocusedActive:= false;

  FOptRulerVisible:= true;
  FOptRulerNumeration:= cRulerNumeration_0_10_20;
  FOptRulerHeightPercents:= cSizeRulerHeightPercents;
  FOptRulerMarkSizeCaret:= cSizeRulerMarkCaret;
  FOptRulerMarkSizeSmall:= cSizeRulerMarkSmall;
  FOptRulerMarkSizeBig:= cSizeRulerMarkBig;
  FOptRulerFontSizePercents:= 80;
  FOptRulerTopIndentPercents:= 0;

  FMinimapWidth:= cInitMinimapWidth;
  FMinimapCharWidth:= 0;
  FMinimapCustomScale:= 0;
  FMinimapVisible:= cInitMinimapVisible;
  FMinimapShowSelBorder:= false;
  FMinimapShowSelAlways:= true;
  FMinimapSelColorChange:= cInitMinimapSelColorChange;
  FMinimapAtLeft:= false;
  FMinimapTooltipVisible:= cInitMinimapTooltipVisible;
  FMinimapTooltipLinesCount:= cInitMinimapTooltipLinesCount;
  FMinimapTooltipWidthPercents:= cInitMinimapTooltipWidthPercents;
  FMinimapCachedPainting:= true;
  FMinimapHiliteLinesWithSelection:= true;

  FMicromapVisible:= cInitMicromapVisible;

  FCharSpacingText:= Point(0, cInitSpacingText);
  FCharSizeMinimap:= Point(1, 2);

  FOptScrollStyleHorz:= aessAuto;
  FOptScrollStyleVert:= aessShow;
  FOptScrollSmooth:= true;
  FOptScrollIndentCaretHorz:= 10;
  FOptScrollIndentCaretVert:= 0;
  FOptUndoIndentVert:= cInitUndoIndentVert;
  FOptUndoIndentHorz:= cInitUndoIndentHorz;
  FOptScrollbarsNew:= false;
  FOptScrollbarHorizontalAddSpace:= cInitScrollbarHorzAddSpace;
  FOptScrollLineCommandsKeepCaretOnScreen:= true;

  FOptShowFontLigatures:= true;
  FOptShowURLs:= true;
  FOptShowURLsRegex:= cUrlRegexInitial;
  FOptShowDragDropMarker:= true;

  FOptUndoMaxCarets:= cInitUndoMaxCarets;
  FOptMaxLineLenToTokenize:= cInitMaxLineLenToTokenize;
  FOptMinLineLenToCalcURL:= cInitMinLineLenToCalcURL;
  FOptMaxLineLenToCalcURL:= cInitMaxLineLenToCalcURL;
  FOptMaxLinesToCountUnindent:= 100;

  FOptStapleStyle:= cLineStyleSolid;
  FOptStapleIndent:= -1;
  FOptStapleWidthPercent:= 100;
  FOptStapleHiliteActive:= true;
  FOptStapleHiliteActiveAlpha:= cInitStapleHiliteAlpha;
  FOptStapleEdge1:= cStapleEdgeAngle;
  FOptStapleEdge2:= cStapleEdgeAngle;
  FOptStapleIndentConsidersEnd:= false;

  FOptTextCenteringCharWidth:= 0;
  FOptTextOffsetLeft:= 0;
  FOptTextOffsetTop:= 0;
  FOptTextOffsetFromLine:= cInitTextOffsetFromLine;
  FOptAllowRepaintOnTextChange:= true;
  FOptAllowReadOnly:= true;

  FOptKeyBackspaceUnindent:= true;
  FOptKeyBackspaceGoesToPrevLine:= true;
  FOptKeyPageKeepsRelativePos:= true;
  FOptKeyUpDownNavigateWrapped:= true;
  FOptKeyHomeEndNavigateWrapped:= true;
  FOptKeyUpDownKeepColumn:= true;

  FOptOverwriteAllowedOnPaste:= false;
  FOptNonWordChars:= cDefaultNonWordChars;
  FOptAutoIndent:= true;
  FOptAutoIndentKind:= cIndentAsPrevLine;
  FOptAutoIndentBetterBracketsCurly:= true;
  FOptAutoIndentBetterBracketsRound:= false;
  FOptAutoIndentBetterBracketsSquare:= false;
  FOptAutoIndentRegexRule:= '';
  FOptTabSpaces:= false;

  FOptLastLineOnTop:= false;
  FOptOverwriteSel:= true;
  FOptMouseDragDrop:= true;
  FOptMouseDragDropCopying:= true;
  FOptMouseDragDropCopyingWithState:= ssModifier;
  FOptMouseMiddleClickAction:= mcaScrolling;
  FOptMouseHideCursor:= false;

  FOptMouseClickOpensURL:= false;
  FOptMouseClickNumberSelectsLine:= true;
  FOptMouseClickNumberSelectsLineWithEOL:= true;
  FOptMouse2ClickAction:= cMouseDblClickSelectAnyChars;
  FOptMouse2ClickOpensURL:= true;
  FOptMouse2ClickDragSelectsWords:= true;
  FOptMouse3ClickSelectsLine:= true;

  FOptMouseRightClickMovesCaret:= false;
  FOptMouseWheelScrollVert:= true;
  FOptMouseWheelScrollVertSpeed:= 3;
  FOptMouseWheelScrollHorz:= true;
  FOptMouseWheelScrollHorzSpeed:= 10;
  FOptMouseWheelScrollHorzWithState:= ssShift;
  FOptMouseWheelZooms:= true;
  FOptMouseWheelZoomsWithState:= ssModifier;

  FOptCopyLinesIfNoSel:= true;
  FOptCutLinesIfNoSel:= false;
  FOptCopyColumnBlockAlignedBySpaces:= true;
  FOptShowFullSel:= false;
  FOptShowFullHilite:= true;
  FOptShowCurLine:= false;
  FOptShowCurLineMinimal:= true;
  FOptShowCurLineOnlyFocused:= false;
  FOptShowCurColumn:= false;
  FOptShowMouseSelFrame:= cInitShowMouseSelFrame;

  FOptKeyPageUpDownSize:= cPageSizeFullMinus1;
  FOptKeyLeftRightGoToNextLineWithCarets:= true;
  FOptKeyLeftRightSwapSel:= true;
  FOptKeyLeftRightSwapSelAndSelect:= false;
  FOptKeyHomeToNonSpace:= true;
  FOptKeyEndToNonSpace:= true;
  FOptKeyTabIndents:= true;
  FOptKeyTabIndentsVerticalBlock:= false;

  FOptShowIndentLines:= true;
  FOptShowGutterCaretBG:= true;
  FOptIndentSize:= 2;
  FOptIndentKeepsAlign:= true;
  FOptIndentMakesWholeLinesSelection:= false;
  FOptUndoGrouped:= true;
  FOptSavingForceFinalEol:= false;
  FOptSavingTrimSpaces:= false;
  FOptShowScrollHint:= false;
  FOptCaretPreferLeftSide:= true;
  FOptCaretPosAfterPasteColumn:= cPasteCaretColumnRight;
  FOptCaretsAddedToColumnSelection:= true;
  FOptCaretFixAfterRangeFolded:= true;
  FOptCaretsPrimitiveColumnSelection:= cInitCaretsPrimitiveColumnSelection;
  FOptCaretsMultiToColumnSel:= cInitCaretsMultiToColumnSel;
  FOptMarkersSize:= cInitMarkerSize;
  FOptMouseEnableAll:= true;
  FOptMouseEnableNormalSelection:= true;
  FOptMouseEnableColumnSelection:= true;
  FOptPasteAtEndMakesFinalEmptyLine:= true;
  FOptPasteMultilineTextSpreadsToCarets:= true;
  FOptZebraActive:= false;
  FOptZebraStep:= 2;
  FOptZebraAlphaBlend:= cInitZebraAlphaBlend;

  ClearMouseDownVariables;
  FMouseNiceScrollPos:= Point(0, 0);

  FSelRect:= cRectEmpty;
  ClearSelRectPoints;
  FCursorOnMinimap:= false;
  FCursorOnGutter:= false;
  FLastTextCmd:= 0;
  FLastTextCmdText:= '';
  FLastCommandChangedText:= false;
  FLastHotspot:= -1;

  FScrollVert.Clear;
  FScrollHorz.Clear;
  FScrollVert.Vertical:= true;
  FScrollHorz.Vertical:= false;
  FScrollVertMinimap.Vertical:= true;
  FScrollHorzMinimap.Vertical:= false;

  FKeymap:= KeymapFull;
  FHintWnd:= nil;

  FMenuStd:= nil;
  FMenuText:= nil;
  FMenuGutterBm:= nil;
  FMenuGutterNum:= nil;
  FMenuGutterFold:= nil;
  FMenuGutterFoldStd:= nil;
  FMenuMinimap:= nil;
  FMenuMicromap:= nil;
  FMenuRuler:= nil;

  //must call UpdateTabHelper also before first Paint
  UpdateTabHelper;
  //must call before first paint
  UpdateLinksRegexObject;
end;

destructor TATSynEdit.Destroy;
begin
  FAdapterHilite:= nil;
  if Assigned(FRegexLinks) then
    FreeAndNil(FRegexLinks);
  if Assigned(FAdapterIME) then
    FreeAndNil(FAdapterIME);
  TimersStop;
  if Assigned(FHintWnd) then
    FreeAndNil(FHintWnd);
  if Assigned(FMenuStd) then
    FreeAndNil(FMenuStd);
  DoPaintModeStatic;
  if Assigned(FFoldedMarkList) then
  begin
    FFoldedMarkList.Clear;
    FreeAndNil(FFoldedMarkList);
  end;
  {$ifdef use_bg}
  FreeAndNil(FFastBmp);
  {$endif}
  FreeAndNil(FMicromap);
  FreeAndNil(FFold);
  FreeAndNil(FTimerNiceScroll);
  FreeAndNil(FTimerScroll);
  FreeAndNil(FTimerBlink);
  FreeAndNil(FCarets);
  if Assigned(FHotspots) then
    FreeAndNil(FHotspots);
  if Assigned(FDimRanges) then
    FreeAndNil(FDimRanges);
  if Assigned(FMarkedRange) then
    FreeAndNil(FMarkedRange);
  if Assigned(FMarkers) then
    FreeAndNil(FMarkers);
  FreeAndNil(FTabHelper);
  if Assigned(FAttribs) then
    FreeAndNil(FAttribs);
  FreeAndNil(FGutter);
  FreeAndNil(FWrapTemps);
  FreeAndNil(FWrapInfo);
  FreeAndNil(FStringsInt);
  if Assigned(FGutterDecor) then
    FreeAndNil(FGutterDecor);
  FreeAndNil(FBitmap);
  FreeAndNil(FColors);
  FreeAndNil(FLinkCache);
  FreeAndNil(FAdapterCache);
  FreeAndNil(FFontItalic);
  FreeAndNil(FFontBold);
  FreeAndNil(FFontBoldItalic);
  FreeAndNil(FCaretPropsNormal);
  FreeAndNil(FCaretPropsOverwrite);
  FreeAndNil(FCaretPropsReadonly);
  inherited;
end;

procedure TATSynEdit.Update(
  AUpdateWrapInfo: boolean = false;
  AUpdateCaretsCoords: boolean = true);
begin
  UpdateCursor;

  if AUpdateWrapInfo then
  begin
    FWrapUpdateNeeded:= true;
    ////UpdateWrapInfo; //performed later
  end;
  if AUpdateCaretsCoords then
    Include(FPaintFlags, cPaintUpdateCaretsCoords);
  Invalidate;
end;

procedure TATSynEdit.SetFocus;
begin
  LCLIntf.SetFocus(Handle);
end;

procedure TATSynEdit.GetClientSizes(out W, H: integer);
var
  R: TRect;
begin
  R:= inherited ClientRect;

  W:= R.Width;
  //if FScrollbarVert.Visible then
  if FOptScrollbarsNew then //better check this instead of FScrollbarVert.Visible
    Dec(W, FScrollbarVert.Width);
  if W<1 then W:= 1;

  H:= R.Height;
  if FScrollbarHorz.Visible then
    Dec(H, FScrollbarHorz.Height);
  if H<1 then H:= 1;
end;

procedure TATSynEdit.LoadFromFile(const AFilename: string; AKeepScroll: boolean=false);
begin
  DoPaintModeStatic;

  FCarets.Clear;
  FCarets.Add(0, 0);

  Strings.Clear;
  FWrapInfo.Clear;
  FWrapUpdateNeeded:= true;

  if not AKeepScroll then
  begin
    FScrollHorz.Clear;
    FScrollVert.Clear;
  end;

  BeginUpdate;
  try
    FFileName:= '';
    Strings.LoadFromFile(AFilename);
    FFileName:= AFileName;
  finally
    EndUpdate;
  end;

  Update;
  DoPaintModeBlinking;
  DoEventChange(false); //calling OnChange makes almost no sense on opening file
  //DoEventCarets; //calling OnChangeCaretPos makes little sense on opening file
end;

procedure TATSynEdit.SaveToFile(const AFilename: string);
var
  Change1, Change2, Change3: boolean;
begin
  Change1:= false;
  Change2:= false;
  Change3:= false;

  if FOptSavingForceFinalEol then
    Change1:= Strings.ActionEnsureFinalEol;

  if FOptSavingTrimSpaces then
  begin
    Change2:= Strings.ActionTrimSpaces(cTrimRight);
    //caret may be after end-of-line, so fix it
    if not OptCaretVirtual then
      DoCaretsFixIncorrectPos(true);
  end;

  if FOptSavingTrimFinalEmptyLines then
  begin
    Change3:= Strings.ActionTrimFinalEmptyLines;
    if Change3 then
      DoCaretsFixIncorrectPos(false);
  end;

  if Change1 or Change2 or Change3 then
  begin
    Update(true);
    DoEventChange;
  end;

  Strings.SaveToFile(AFilename);
  FFileName:= AFilename;
  Modified:= false;
end;


function TATSynEdit.GetStrings: TATStrings;
begin
  if Assigned(FStringsExternal) then
    Result:= FStringsExternal
  else
    Result:= FStringsInt;
end;

procedure TATSynEdit.SetCaretBlinkTime(AValue: integer);
begin
  AValue:= Max(AValue, cMinCaretTime);
  AValue:= Min(AValue, cMaxCaretTime);
  FCaretBlinkTime:= AValue;
  FTimerBlink.Interval:= AValue;
end;

procedure TATSynEdit.SetCharSpacingY(AValue: integer);
begin
  if FCharSpacingText.Y=AValue then Exit;
  FCharSpacingText.Y:= AValue;
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetMarginString(const AValue: string);
var
  Sep: TATStringSeparator;
  N: integer;
begin
  SetLength(FMarginList, 0);
  Sep.Init(AValue, ' ');
  repeat
    if not Sep.GetItemInt(N, 0) then Break;
    if N<2 then Continue;
    SetLength(FMarginList, Length(FMarginList)+1);
    FMarginList[Length(FMarginList)-1]:= N;
  until false;
end;

procedure TATSynEdit.SetMicromapVisible(AValue: boolean);
begin
  if FMicromapVisible=AValue then Exit;
  FMicromapVisible:= AValue;
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetMinimapVisible(AValue: boolean);
begin
  if FMinimapVisible=AValue then Exit;
  FMinimapVisible:= AValue;
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetOneLine(AValue: boolean);
var
  Str: TATStrings;
begin
  Carets.OneLine:= AValue;
  Str:= Strings;
  Str.OneLine:= AValue;

  if AValue then
  begin
    OptGutterVisible:= false;
    OptRulerVisible:= false;
    OptMinimapVisible:= false;
    //OptMicromapVisible:= false;
    OptCaretVirtual:= false;
    OptCaretManyAllowed:= false;
    OptUnprintedVisible:= false;
    OptWrapMode:= cWrapOff;
    OptScrollStyleHorz:= aessHide;
    OptScrollStyleVert:= aessHide;
    OptAllowReadOnly:= false;
    OptMouseMiddleClickAction:= mcaNone;
    OptMouseDragDrop:= false;
    OptMarginRight:= 1000;
    OptUndoLimit:= 200;

    DoCaretSingle(0, 0);

    while Str.Count>1 do
      Str.LineDelete(Str.Count-1, false, false, false);
  end;
end;

procedure TATSynEdit.SetReadOnly(AValue: boolean);
begin
  if not FOptAllowReadOnly then Exit;
  Strings.ReadOnly:= AValue;
end;

procedure TATSynEdit.SetLineTop(AValue: integer);
var
  NFrom, NTo, i: integer;
begin
  if not HandleAllocated then
  begin
    FLineTopTodo:= AValue;
    exit;
  end;

  if AValue<=0 then
  begin
    FScrollVert.SetZero;
    Update;
    Exit
  end;

  //first make sure WrapInfo is filled with data;
  //then we can read WrapInfo and calc scroll pos;
  //this is required for restoring LineTop for n tabs, on opening CudaText.
  UpdateWrapInfo;

  if FWrapInfo.Count=0 then
  begin
    FScrollVert.SetZero;
    Update;
    Exit
  end;

  //find exact match
  FWrapInfo.FindIndexesOfLineNumber(AValue, NFrom, NTo);
  if NFrom>=0 then
  begin
    FScrollVert.NPos:= NFrom;
    UpdateScrollbars(true);
    Update;
    Exit
  end;

  //find approx match
  for i:= 0 to FWrapInfo.Count-1 do
    with FWrapInfo[i] do
      if NLineIndex>=AValue then
      begin
        FScrollVert.NPos:= i;
        UpdateScrollbars(true);
        Update;
        Exit
      end;
end;

procedure TATSynEdit.SetColumnLeft(AValue: integer);
begin
  FScrollHorz.NPos:= AValue;
  UpdateScrollbars(true);
  Update;
end;

procedure TATSynEdit.SetLinesFromTop(AValue: integer);
begin
  with FScrollVert do
    NPos:= Max(0, NPos + (GetLinesFromTop - AValue));
end;

procedure TATSynEdit.SetRedoAsString(const AValue: string);
begin
  Strings.RedoAsString:= AValue;
end;

procedure TATSynEdit.SetStrings(Obj: TATStrings);
begin
  FStringsExternal:= Obj;
end;

function TATSynEdit.GetTextOffset: TPoint;
var
  NGutterWidth: integer;
begin
  if FOptGutterVisible then
    NGutterWidth:= Gutter.Width
  else
    NGutterWidth:= 0;

  if FOptTextCenteringCharWidth>0 then
    Result.X:= Max(0, (ClientWidth - NGutterWidth -
                       FOptTextCenteringCharWidth * FCharSize.X) div 2)
  else
    Result.X:= OptTextOffsetLeft;

  Inc(Result.X, NGutterWidth);

  Result.Y:= OptTextOffsetTop;
  if FOptRulerVisible then
    Inc(Result.Y, FRulerHeight);
end;

function TATSynEdit.GetPageLines: integer;
begin
  case FOptKeyPageUpDownSize of
    cPageSizeFull: Result:= GetVisibleLines;
    cPageSizeFullMinus1: Result:= GetVisibleLines-1;
    cPageSizeHalf: Result:= GetVisibleLines div 2;
    else
      raise Exception.Create('Unknown pagesize');
  end;
end;

procedure TATSynEdit.UpdateAdapterCacheSize;
var
  N: integer;
begin
  if FMinimapVisible then
    N:= Max(GetVisibleLines, GetVisibleLinesMinimap)+1
  else
    N:= GetVisibleLines+1;
  FAdapterCache.MaxSize:= N;
end;

procedure TATSynEdit.DoPaintAllTo(C: TCanvas; AFlags: TATSynPaintFlags; ALineFrom: integer);
var
  NCaretX, NWidth: integer;
begin
  if Enabled then
  begin
    FColorFont:= Colors.TextFont;
    FColorBG:= Colors.TextBG;
  end
  else
  begin
    FColorFont:= Colors.TextDisabledFont;
    FColorBG:= Colors.TextDisabledBG;
  end;

  Inc(FPaintCounter);
  FCaretShown:= false;
  Carets.GetSelections(FSel);

  DoPaintMainTo(C, ALineFrom);

  if cPaintUpdateCaretsCoords in AFlags then
    UpdateCaretsCoords;

  if Carets.Count>0 then
  begin
    NCaretX:= Carets[0].CoordX;
    NWidth:= EditorScale(1);
    if FOptShowCurColumn then
      DoPaintMarginLineTo(C, NCaretX, NWidth, Colors.MarginCaret);
    if FOptRulerVisible and (FOptRulerMarkSizeCaret>0) then
      DoPaintRulerCaretMark(C, NCaretX);
  end;

  DoPaintMarkersTo(C);
end;

function TATSynEdit.DoPaint(AFlags: TATSynPaintFlags; ALineFrom: integer): boolean;
//gets True if one of scrollbars changed Visible state
begin
  UpdateTabHelper;

  if DoubleBuffered then
  begin
    if Assigned(FBitmap) then
      if cPaintUpdateBitmap in AFlags then
      begin
        FBitmap.BeginUpdate(true);
        DoPaintAllTo(FBitmap.Canvas, AFlags, ALineFrom);
        FBitmap.EndUpdate();
      end;
  end
  else
    DoPaintAllTo(Canvas, AFlags, ALineFrom);

  Result:= UpdateScrollbars(false);
end;

procedure TATSynEdit.DoPaintLockedWarning(C: TCanvas);
const
  cBitmapX = 20;
  cBitmapY = 20;
  cRectX = 85;
  cRectY = 40;
  cRectWidth = 300;
  cRectHeight = 10;
var
  NValue: integer;
  Bmp: TGraphic;
begin
  C.Brush.Color:= Colors.TextBG;
  C.FillRect(ClientRect);

  if Strings.ProgressKind<>cStringsProgressSaving then
    Bmp:= cBitmapWait
  else
    Bmp:= cBitmapSaving;
  C.Draw(cBitmapX, cBitmapY, Bmp);

  NValue:= Strings.ProgressValue;
  if NValue>0 then
  begin
    C.Pen.Color:= Colors.TextSelBG;
    C.Brush.Color:= Colors.TextSelBG;
    C.FrameRect(
      cRectX,
      cRectY,
      cRectX + cRectWidth,
      cRectY + cRectHeight
      );
    C.FillRect(
      cRectX,
      cRectY,
      cRectX + cRectWidth * NValue div 100,
      cRectY + cRectHeight
      );
  end;
end;


procedure TATSynEdit.Paint;
var
  NLine: integer;
begin
  if not HandleAllocated then exit;

  if not FPaintStarted then
  begin
    FPaintStarted:= true;
    if FLineTopTodo>0 then
    begin
      NLine:= FLineTopTodo;
      FLineTopTodo:= 0;
      SetLineTop(NLine);
      exit;
    end;
  end;

  PaintEx(-1);
end;

procedure TATSynEdit.PaintEx(ALineNumber: integer);
var
  R: TRect;
begin
  if IsLocked then
  begin
    DoPaintLockedWarning(Canvas);
    Exit
  end;

  if DoubleBuffered then
    if not Assigned(FBitmap) then exit;

  {$ifdef debug_fps}
  FTickAll:= GetTickCount64;
  FMissMain:= 0;
  FMissMinimap:= 0;
  {$endif}

  //if scrollbars shown, paint again
  if DoPaint(FPaintFlags, ALineNumber) then
    DoPaint(FPaintFlags, ALineNumber);
  Exclude(FPaintFlags, cPaintUpdateBitmap);

    if DoubleBuffered then
    //buf mode: timer tick don't give painting of whole bitmap
    //(cPaintUpdateBitmap off)
    begin
      DoPaintCarets(FBitmap.Canvas, true);
    end
    else
    //non-buf mode: timer tick clears whole canvas first.
    //we already painted bitmap above,
    //and now we invert carets or dont invert (use FCaretAllowNextBlink)
    begin
      if not FCaretBlinkEnabled or FCaretAllowNextBlink then
        DoPaintCarets(Canvas, true);
    end;

  if DoubleBuffered then
  begin
    //single place where we flush bitmap to canvas
    R:= Canvas.ClipRect;
    Canvas.CopyRect(R, FBitmap.Canvas, R);
  end;

  DoPaintMarkerOfDragDrop(Canvas);

  {$ifdef debug_fps}
  FTickAll:= GetTickCount64-FTickAll;
  DoPaintFPS(Canvas);
  {$endif}
end;

procedure TATSynEdit.Resize;
const
  cStep = 200; //resize bitmap by N pixels step
var
  SizeX, SizeY: integer;
begin
  inherited;

  if DoubleBuffered then
    if Assigned(FBitmap) then
    begin
      SizeX:= (Width div cStep + 1)*cStep;
      SizeY:= (Height div cStep + 1)*cStep;
      if (SizeX>FBitmap.Width) or (SizeY>FBitmap.Height) then
      begin
        FBitmap.SetSize(SizeX, SizeY);
        FBitmap.FreeImage; //recommended, else seen black bitmap on bigsize
      end;
    end;

  if FWrapMode in [cWrapOn, cWrapAtWindowOrMargin] then
  begin
    FWrapUpdateNeeded:= true;
    Include(FPaintFlags, cPaintUpdateCaretsCoords);
  end;

  if not FPaintStarted then exit;

  {
  Include(FPaintFlags, cPaintUpdateBitmap);
  PaintEx(LineTop);
  }
  Invalidate;
end;

procedure TATSynEdit.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  InitMenuStd;
  inherited;
  if not Handled then
  begin
    DoHandleRightClick(MousePos.X, MousePos.Y);
    Handled:= true;
  end;
end;

procedure TATSynEdit.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
  //needed to remove flickering on resize and mouse-over
  Msg.Result:= 1;
end;

procedure TATSynEdit.DoHintShow;
var
  S: string;
  P: TPoint;
  R: TRect;
begin
  if csDesigning in ComponentState then Exit;
  if not FOptShowScrollHint then Exit;

  if FHintWnd=nil then
    FHintWnd:= THintWindow.Create(Self);

  S:= cTextHintScrollPrefix+' '+IntToStr(LineTop+1);
  R:= FHintWnd.CalcHintRect(500, S, nil);

  P:= ClientToScreen(Point(ClientWidth-R.Width, 0));
  OffsetRect(R, P.X, P.Y);
  OffsetRect(R, -cHintScrollDx, cHintScrollDx);

  FHintWnd.ActivateHint(R, S);
  FHintWnd.Invalidate; //for Win
end;

procedure TATSynEdit.DoHintShowForBookmark(ALine: integer);
var
  S: string;
  P: TPoint;
  R: TRect;
  NIndex: integer;
begin
  if csDesigning in ComponentState then Exit;

  if FHintWnd=nil then
    FHintWnd:= THintWindow.Create(Self);

  NIndex:= Strings.Bookmarks.Find(ALine);
  if NIndex<0 then exit;

  S:= Strings.Bookmarks[NIndex].Data.Hint;
  if S='' then
    begin DoHintHide; exit end;

  R:= FHintWnd.CalcHintRect(500, S, nil);

  P:= Mouse.CursorPos;
  OffsetRect(R, P.X+cHintBookmarkDx, P.Y+cHintBookmarkDy);

  FHintWnd.ActivateHint(R, S);
  FHintWnd.Invalidate; //for Win
end;


procedure TATSynEdit.DoHintHide;
begin
  if Assigned(FHintWnd) then
    FHintWnd.Hide;
end;

procedure TATSynEdit.UpdateScrollInfoFromSmoothPos(var AInfo: TATSynScrollInfo; APos: integer);
//Note: for vertical bar, NPos=-1 means than we are before the first line, over top gap
var
  NPos, NPixels, NLineIndex, NCharSize: integer;
  NSizeGapTop, NSizeGap0: integer;
  bConsiderGaps: boolean;
begin
  AInfo.SmoothPos:= APos;
  bConsiderGaps:= AInfo.Vertical and (Gaps.Count>0);

  if APos<=0 then
  begin
    AInfo.SetZero;
    if bConsiderGaps then
      if Gaps.SizeOfGapTop>0 then
        AInfo.NPos:= -1;
    exit
  end;

  if APos>=AInfo.SmoothPosLast then
  begin
    AInfo.SetLast;
    exit
  end;

  if bConsiderGaps then
  begin
    //for position before line=0
    NSizeGapTop:= Gaps.SizeOfGapTop;
    NSizeGap0:= Gaps.SizeOfGap0;

    if NSizeGapTop>0 then
      if APos<NSizeGapTop then
      begin
        AInfo.NPos:= -1;
        AInfo.NPixelOffset:= APos;
        exit;
      end;

    //for position before line=1
    //(other positions are calculated ok later)
    if NSizeGap0>0 then
      if APos<NSizeGapTop+AInfo.SmoothCharSize+NSizeGap0 then
      begin
        AInfo.NPos:= 0;
        AInfo.NPixelOffset:= APos-NSizeGapTop;
        exit;
      end;
  end;

  NCharSize:= AInfo.SmoothCharSize;
  AInfo.NPos:= Min(APos div NCharSize, AInfo.NMax);
  AInfo.NPixelOffset:= APos mod NCharSize;

  //consider Gaps for vert scrolling
  if bConsiderGaps then
  begin
    NPos:= Min(AInfo.NPos, WrapInfo.Count-1);
    NPixels:= AInfo.NPixelOffset;

    repeat
      NLineIndex:= FWrapInfo.Data[NPos].NLineIndex - 1;
      NPixels:= APos - NPos*NCharSize - Gaps.SizeForLineRange(-1, NLineIndex);
      if NPos=0 then Break;
      if NLineIndex=0 then Break;
      if NPixels>=0 then Break;
      Dec(NPos);
    until false;

    AInfo.NPos:= NPos;
    AInfo.NPixelOffset:= NPixels
  end;
end;

function TATSynEdit.UpdateScrollInfoFromMessage(var Info: TATSynScrollInfo; const Msg: TLMScroll): boolean;
begin
  if Info.NMax<Info.NPage then
  begin
    Info.Clear;
    Exit(true);
  end;

  case Msg.ScrollCode of
    SB_TOP:
      begin
        UpdateScrollInfoFromSmoothPos(Info, 0);
      end;

    SB_BOTTOM:
      begin
        UpdateScrollInfoFromSmoothPos(Info, Info.SmoothPosLast);
      end;

    SB_LINEUP:
      begin
        UpdateScrollInfoFromSmoothPos(Info, Info.SmoothPos-Info.SmoothCharSize);
      end;

    SB_LINEDOWN:
      begin
        UpdateScrollInfoFromSmoothPos(Info, Info.SmoothPos+Info.SmoothCharSize);
      end;

    SB_PAGEUP:
      begin
        UpdateScrollInfoFromSmoothPos(Info, Info.SmoothPos-Info.SmoothPage);
      end;

    SB_PAGEDOWN:
      begin
        UpdateScrollInfoFromSmoothPos(Info, Info.SmoothPos+Info.SmoothPage);
      end;

    SB_THUMBPOSITION:
      begin
        //must ignore message with Msg.Msg set: LM_VSCROLL, LM_HSCROLL;
        //we get it on macOS during window resize, not expected! moves v-scroll pos to 0.
        if Msg.Msg=0 then
          UpdateScrollInfoFromSmoothPos(Info, Msg.Pos);
      end;

    SB_THUMBTRACK:
      begin
        UpdateScrollInfoFromSmoothPos(Info, Msg.Pos);
        if Info.Vertical then
          DoHintShow;
      end;

    SB_ENDSCROLL:
      DoHintHide;
  end;

  //correct value (if -1)
  if Info.SmoothPos>Info.SmoothPosLast then
    UpdateScrollInfoFromSmoothPos(Info, Info.SmoothPosLast)
  else
  if Info.SmoothPos<0 then
    UpdateScrollInfoFromSmoothPos(Info, 0);

  Result:= Msg.ScrollCode<>SB_THUMBTRACK;
end;

procedure TATSynEdit.WMVScroll(var Msg: TLMVScroll);
begin
  Include(FPaintFlags, cPaintUpdateCaretsCoords);
  UpdateScrollInfoFromMessage(FScrollVert, Msg);
  Invalidate;
end;

{$ifdef windows}
procedure TATSynEdit.WMIME_Request(var Msg: TMessage);
begin
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeRequest(Self, Msg);
end;

procedure TATSynEdit.WMIME_Notify(var Msg: TMessage);
begin
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeNotify(Self, Msg);
end;

procedure TATSynEdit.WMIME_StartComposition(var Msg: TMessage);
begin
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeStartComposition(Self, Msg);
end;

procedure TATSynEdit.WMIME_Composition(var Msg: TMessage);
begin
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeComposition(Self, Msg);
end;

procedure TATSynEdit.WMIME_EndComposition(var Msg: TMessage);
begin
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeEndComposition(Self, Msg);
end;
{$endif}

procedure TATSynEdit.WMHScroll(var Msg: TLMHScroll);
begin
  Include(FPaintFlags, cPaintUpdateCaretsCoords);
  UpdateScrollInfoFromMessage(FScrollHorz, Msg);
  Invalidate;
end;

procedure TATSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PCaret: TPoint;
  PosDetails: TATPosDetails;
  Index: integer;
  ActionId: TATMouseActionId;
  bClickOnSelection: boolean;
  R: TRect;
begin
  if not OptMouseEnableAll then exit;
  inherited;
  SetFocus;
  DoCaretForceShow;

  FMouseDownCoordOriginal.X:= X;
  FMouseDownCoordOriginal.Y:= Y;
  FMouseDownCoord.X:= X + FScrollHorz.TotalOffset;
  FMouseDownCoord.Y:= Y + FScrollVert.TotalOffset;
  FMouseDownWithCtrl:= ssXControl in Shift;
  FMouseDownWithAlt:= ssAlt in Shift;
  FMouseDownWithShift:= ssShift in Shift;

  if FMinimapVisible and PtInRect(FRectMinimap, Point(X, Y)) then
  begin
    GetRectMinimapSel(R);
    FMouseDownOnMinimap:= true;
    FMouseDragMinimapSelHeight:= R.Height;
    if PtInRect(R, Point(X, Y)) then
    begin
      FMouseDragMinimap:= true;
      FMouseDragMinimapDelta:= Y-R.Top;
    end;
    Exit;
  end;

  PCaret:= ClientPosToCaretPos(Point(X, Y), PosDetails);
  FCaretSpecPos:= false;
  FMouseDownOnMinimap:= false;
  FMouseDownGutterLineNumber:= -1;
  FMouseDragDropping:= false;
  FMouseDragDroppingReal:= false;
  FMouseDragMinimap:= false;
  ActionId:= EditorMouseActionId(FMouseActions, Shift);

  ClearSelRectPoints; //SelRect points will be set in MouseMove

  if Assigned(FAdapterIME) then
    FAdapterIME.Stop(Self, false);

  if MouseNiceScroll then
  begin
    MouseNiceScroll:= false;
    Exit
  end;

  if PtInRect(FRectMain, Point(X, Y)) then
  begin
    FMouseDownPnt:= PCaret;
    bClickOnSelection:= GetCaretSelectionIndex(FMouseDownPnt)>=0;

    if Shift=[ssMiddle] then
      if DoHandleClickEvent(FOnClickMiddle) then Exit;

    //Ctrl+click on selection must not be ignored, but must start drag-drop with copying
    if ActionId=cMouseActionMakeCaret then
      if bClickOnSelection then
        ActionId:= cMouseActionClickSimple;

    if ActionId=cMouseActionClickMiddle then
    begin
      case FOptMouseMiddleClickAction of
        mcaScrolling:
          begin
            FMouseNiceScrollPos:= Point(X, Y);
            MouseNiceScroll:= true;
          end;
        mcaPaste:
          begin
            //don't set caret pos here, user needs to press middle-btn on any place to paste
            DoCommand(cCommand_ClipboardAltPaste); //uses PrimarySelection:TClipboard
          end;
        mcaGotoDefinition:
          begin
            if cCommand_GotoDefinition>0 then
            begin
              DoCaretSingle(PCaret.X, PCaret.Y);
              DoCommand(cCommand_GotoDefinition);
            end;
          end;
      end;
      Exit
    end;

    if ActionId=cMouseActionClickSimple then
    begin
      Strings.ActionAddJumpToUndo;
      Strings.SetGroupMark;

      FSelRect:= cRectEmpty;
      DoCaretSingleAsIs;

      if Assigned(PosDetails.OnGapItem) then
      begin
        if Assigned(FOnClickGap) then
          FOnClickGap(Self, PosDetails.OnGapItem, PosDetails.OnGapPos);
      end;

      if FOptMouseDragDrop and bClickOnSelection and not ModeReadOnly then
      begin
        //DragMode must be dmManual, drag started by code
        FMouseDragDropping:= true;
      end
      else
      begin
        if Assigned(FOnClickMoveCaret) then
          FOnClickMoveCaret(Self, Point(Carets[0].PosX, Carets[0].PosY), FMouseDownPnt);

        DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
        DoSelect_None;
      end;
    end;

    if ActionId=cMouseActionClickAndSelNormalBlock then
    begin
      FSelRect:= cRectEmpty;
      DoCaretSingleAsIs;
      Carets[0].SelectToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
    end;

    if ActionId=cMouseActionClickAndSelVerticalBlock then
    begin
      FSelRect:= cRectEmpty;
      DoCaretSingleAsIs;
      with Carets[0] do
        DoSelect_ColumnBlock_FromPoints(
          Point(PosX, PosY),
          FMouseDownPnt
          );
    end;

    if ActionId=cMouseActionMakeCaret then
    begin
      FSelRect:= cRectEmpty;
      DoCaretAddToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
    end;

    if ActionId=cMouseActionMakeCaretsColumn then
    begin
      FSelRect:= cRectEmpty;
      DoCaretsColumnToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
    end;

    if ActionId=cMouseActionClickRight then
    begin
      if FOptMouseRightClickMovesCaret then
        if not bClickOnSelection then
        begin
          DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
          DoSelect_None;
          Invalidate;
        end;
    end;
  end;

  if FOptGutterVisible and PtInRect(FRectGutter, Point(X, Y)) then
  begin
    if ActionId=cMouseActionClickSimple then
    begin
      Index:= FGutter.IndexAt(X);
      if Index=FGutterBandNumbers then
      begin
        if FOptMouseClickNumberSelectsLine then
        begin
          FSelRect:= cRectEmpty;
          FMouseDownGutterLineNumber:= PCaret.Y;
          DoSelect_Line(PCaret);
        end;
      end
      else
      if Index=FGutterBandFolding then
      begin
        DoFoldbarClick(PCaret.Y);
      end
      else
        //click on other bands- event
        DoEventClickGutter(FGutter.IndexAt(X), PCaret.Y);
    end;
  end;

  if FMicromapVisible and PtInRect(FRectMicromap, Point(X, Y)) then
    if ActionId=cMouseActionClickSimple then
    begin
      DoEventClickMicromap(X-FRectMicromap.Left, Y-FRectMicromap.Top);
      Exit
    end;

  //don't fire OnChangeCaretPos on right click
  if Button=mbRight then
    if not FOptMouseRightClickMovesCaret then
      exit;

  DoCaretsSort;
  DoEventCarets;
  Update;
end;

procedure TATSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  bCopySelection: boolean;
  Str: atString;
  Caret: TATCaretItem;
  PosDetails: TATPosDetails;
  PntCaret: TPoint;
begin
  if not OptMouseEnableAll then exit;
  inherited;

  if FOptShowMouseSelFrame then
    if FMouseDragCoord.X>=0 then
    begin
      FMouseDragCoord:= Point(-1, -1);
      Invalidate;
    end;

  if PtInRect(FRectMinimap, Point(X, Y)) then
  begin
    if FMouseDownOnMinimap then
    begin
      FMouseDownOnMinimap:= false;
      if not FMouseDragMinimap then
        DoMinimapClick(Y);
    end;
    Exit
  end;

  if PtInRect(ClientRect, Point(X, Y)) then
  if FMouseDragDropping then
  begin
    //drag-drop really started
    if FMouseDragDroppingReal then
    begin
      Strings.BeginUndoGroup;
      try
        bCopySelection:=
          FOptMouseDragDropCopying and
          (FOptMouseDragDropCopyingWithState in Shift);
        DoDropText(not bCopySelection);
      finally
        Strings.EndUndoGroup;
        Update;
      end;
    end
    else
    //mouse released w/o drag-drop
    begin
      PntCaret:= ClientPosToCaretPos(Point(X, Y), PosDetails);
      DoCaretSingle(PntCaret.X, PntCaret.Y);
      DoEventCarets;
      Update;
    end;
    FMouseDragDropping:= false;
    FMouseDragDroppingReal:= false;
  end;

  if FOptMouseClickOpensURL then
    if not FMouseDownDouble and not FMouseDragDropping and (Button=mbLeft) then
      if Carets.Count=1 then
      begin
        Caret:= Carets[0];
        Str:= DoGetLinkAtPos(Caret.PosX, Caret.PosY);
        if Str<>'' then
          if Assigned(FOnClickLink) then
            FOnClickLink(Self, Str);
      end;

  ClearMouseDownVariables;

  if Carets.Count=1 then
    with Carets[0] do
    begin
      //mouse-up after selection made
      if EndY>=0 then
      begin
        if Assigned(FOnClickEndSelect) then
          FOnClickEndSelect(Self, Point(EndX, EndY), Point(PosX, PosY));
      end
      //else: simple mouse click
    end;
end;

procedure TATSynEdit.ClearSelRectPoints;
begin
  FLastCommandMakesColumnSel:= false;
  FSelRectBegin:= Point(-1, -1);
  FSelRectEnd:= Point(-1, -1);
end;

procedure TATSynEdit.ClearMouseDownVariables;
begin
  FMouseDownCoordOriginal:= Point(-1, -1);
  FMouseDownCoord:= Point(-1, -1);
  FMouseDownPnt:= Point(-1, -1);
  FMouseDownGutterLineNumber:= -1;
  FMouseDownDouble:= false;
  FMouseDownAndColumnSelection:= false;
  FMouseDownOnMinimap:= false;
  FMouseDownWithCtrl:= false;
  FMouseDownWithAlt:= false;
  FMouseDownWithShift:= false;
  FMouseDragDropping:= false;
  FMouseDragDroppingReal:= false;
  FMouseDragMinimap:= false;
  FTimerScroll.Enabled:= false;
end;

procedure TATSynEdit.DoHandleRightClick(X, Y: integer);
var
  Index: integer;
begin
  if PtInRect(FRectMain, Point(X, Y)) then
  begin
    if Assigned(FMenuText) then
      FMenuText.PopUp
    else
    begin
      InitMenuStd;
      FMenuStd.PopUp;
    end;
  end
  else
  if FOptGutterVisible and PtInRect(FRectGutter, Point(X, Y)) then
  begin
    Index:= FGutter.IndexAt(X);
    if Index=FGutterBandBookmarks then
      if Assigned(FMenuGutterBm) then FMenuGutterBm.PopUp;
    if Index=FGutterBandNumbers then
      if Assigned(FMenuGutterNum) then FMenuGutterNum.PopUp;
    if Index=FGutterBandFolding then
      if Assigned(FMenuGutterFold) then FMenuGutterFold.PopUp else DoMenuGutterFold;
  end
  else
  if FMinimapVisible and PtInRect(FRectMinimap, Point(X, Y)) then
  begin
    if Assigned(FMenuMinimap) then FMenuMinimap.PopUp;
  end
  else
  if FMicromapVisible and PtInRect(FRectMicromap, Point(X, Y)) then
  begin
    if Assigned(FMenuMicromap) then FMenuMicromap.PopUp;
  end
  else
  if FOptRulerVisible and PtInRect(FRectRuler, Point(X, Y)) then
  begin
    if Assigned(FMenuRuler) then FMenuRuler.PopUp;
  end;
end;

procedure TATSynEdit.UpdateCursor;
var
  P: TPoint;
  RectBm, RectNums: TRect;
begin
  if MouseNiceScroll then Exit;
  P:= ScreenToClient(Mouse.CursorPos);

  RectBm.Left:= FGutter[FGutterBandBookmarks].Left;
  RectBm.Right:= FGutter[FGutterBandBookmarks].Right;
  RectBm.Top:= FRectMain.Top;
  RectBm.Bottom:= FRectMain.Bottom;

  RectNums.Left:= FGutter[FGutterBandNumbers].Left;
  RectNums.Right:= FGutter[FGutterBandNumbers].Right;
  RectNums.Top:= FRectMain.Top;
  RectNums.Bottom:= FRectMain.Bottom;

  //if FMouseDragDropping then
  //  Cursor:= crDrag
  //else
  //if FMouseDragMinimap then
  //  Cursor:= crSizeNS
  //else
  if PtInRect(FRectMain, P) then
  begin
    if FMouseDownAndColumnSelection then
      Cursor:= FCursorColumnSel
    else
      Cursor:= FCursorText;
  end
  else
  if PtInRect(RectBm, P) then
    Cursor:= FCursorGutterBookmark
  else
  if PtInRect(RectNums, P) then
    Cursor:= FCursorGutterNumbers
  else
  if PtInRect(FRectMinimap, P) then
    Cursor:= FCursorMinimap
  else
  if PtInRect(FRectMicromap, P) then
    Cursor:= FCursorMicromap
  else
    Cursor:= crDefault;
end;


procedure _LimitPointByRect(var P: TPoint; const R: TRect); inline;
begin
  if P.X<R.Left+1 then P.X:= R.Left+1;
  if P.X>R.Right then P.X:= R.Right;
  if P.Y<R.Top+1 then P.Y:= R.Top+1;
  if P.Y>R.Bottom then P.Y:= R.Bottom;
end;

procedure TATSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  cMovedDeltaPx = 5;
var
  P: TPoint;
  RectNums, RectBookmk: TRect;
  bOnMain, bOnMinimap, bOnMicromap,
  bOnGutter, bOnGutterNumbers, bOnGutterBookmk,
  bSelecting, bSelectingGutterNumbers: boolean;
  bMovedMinimal: boolean;
  Details: TATPosDetails;
  nIndex: integer;
  Caret: TATCaretItem;
begin
  if not OptMouseEnableAll then exit;
  inherited;

  P:= Point(X, Y);
  UpdateCursor;

  bMovedMinimal:= IsPointsDiffByDelta(P, FMouseDownCoordOriginal, cMovedDeltaPx);

  bSelecting:= (not FMouseDragDropping) and (FMouseDownPnt.X>=0);
  bSelectingGutterNumbers:= FMouseDownGutterLineNumber>=0;

  if bSelecting then
  begin
    FMouseDragCoord:= P;
    _LimitPointByRect(FMouseDragCoord, FRectMainVisible);
  end
  else
    FMouseDragCoord:= Point(-1, -1);

  bOnMain:= PtInRect(FRectMain, P);
  bOnMinimap:= FMinimapVisible and PtInRect(FRectMinimap, P);
  bOnMicromap:= FMicromapVisible and PtInRect(FRectMicromap, P);
  bOnGutter:= FOptGutterVisible and PtInRect(FRectGutter, P);
  bOnGutterNumbers:= false;
  bOnGutterBookmk:= false;

  //detect cursor on minimap
  if FMinimapVisible then
  begin
    if not FMinimapShowSelAlways then
      if bOnMinimap<>FCursorOnMinimap then
        Invalidate;
    FCursorOnMinimap:= bOnMinimap;
  end;

  if bOnMinimap and FMinimapTooltipVisible then
  begin
    InitMinimapTooltip;
    FMinimapTooltip.Show;
    UpdateMinimapTooltip;
  end
  else
  begin
    if Assigned(FMinimapTooltip) then
      FMinimapTooltip.Hide;
  end;

  //detect cursor on gutter
  if FOptGutterVisible then
  begin
    if not FOptGutterShowFoldAlways then
      if bOnGutter<>FCursorOnGutter then
        Invalidate;
    FCursorOnGutter:= bOnGutter;
  end;

  RectNums:= Rect(0, 0, 0, 0);
  RectBookmk:= Rect(0, 0, 0, 0);
  if FOptGutterVisible then
  begin
    if FGutter[FGutterBandNumbers].Visible then
    begin
      RectNums.Left:= FGutter[FGutterBandNumbers].Left;
      RectNums.Right:= FGutter[FGutterBandNumbers].Right;
      RectNums.Top:= FRectMain.Top;
      RectNums.Bottom:= FRectMain.Bottom;

      bOnGutterNumbers:= bOnGutter and PtInRect(RectNums, P);
    end;

    if FGutter[FGutterBandBookmarks].Visible then
    begin
      RectBookmk.Left:= FGutter[FGutterBandBookmarks].Left;
      RectBookmk.Right:= FGutter[FGutterBandBookmarks].Right;
      RectBookmk.Top:= FRectMain.Top;
      RectBookmk.Bottom:= FRectMain.Bottom;

      bOnGutterBookmk:= bOnGutter and PtInRect(RectBookmk, P);
    end;
  end;

  //detect cursor on folded marks
  if FFoldTooltipVisible and Assigned(FFoldedMarkList) then
  begin
    FFoldedMarkCurrent:= FFoldedMarkList.FindByCoord(Point(X, Y));
    UpdateFoldedMarkTooltip;
  end;

  //show/hide bookmark hint
  if bOnGutterBookmk and not bSelecting then
  begin
    P:= ClientPosToCaretPos(Point(X, Y), Details);
    DoHintShowForBookmark(P.Y);
  end
  else
    DoHintHide;

  //mouse dragged on minimap
  //handle this before starting FTimerScroll (CudaText issues 2941, 2944)
  if FMouseDragMinimap then
  begin
    if bMovedMinimal then
      if Shift=[ssLeft] then
        DoMinimapDrag(Y);
    Exit
  end;

  //start scroll timer
  FTimerScroll.Enabled:=
    FOptMouseEnableAll and
    FOptMouseEnableNormalSelection and
    FOptMouseEnableColumnSelection and
    (ssLeft in Shift) and
    (not bOnMain) and
    //auto-scroll must not work when cursor is over minimap/micromap
    (not bOnMinimap) and
    (not bOnMicromap) and
    //auto-scroll must not work when cursor is over line-numbers
    //but can work if over other gutter columns
    (not bOnGutterNumbers);

  FMouseAutoScroll:= cDirNone;
  if (P.Y<FRectMain.Top) and (not ModeOneLine) then
    FMouseAutoScroll:= cDirUp else
  if (P.Y>=FRectMain.Bottom) and (not ModeOneLine) then
    FMouseAutoScroll:= cDirDown else
  if (P.X<FRectMain.Left) then
    FMouseAutoScroll:= cDirLeft else
  if (P.X>=FRectMain.Right) then
    FMouseAutoScroll:= cDirRight;

  //mouse dragged on gutter numbers (only if drag started on gutter numbers)
  if bSelectingGutterNumbers then
    if bOnGutterNumbers then
    begin
      if Shift=[ssLeft] then
      begin
        P:= ClientPosToCaretPos(P, Details);
        if (P.Y>=0) and (P.X>=0) then
        begin
          DoSelect_LineRange(FMouseDownGutterLineNumber, P);
          DoCaretsSort;
          DoEventCarets;
          Invalidate;
        end;
      end;
    Exit
  end;

  //mouse drag-drop just begins
  if bOnMain and FMouseDragDropping then
  begin
    if not FMouseDragDroppingReal and
      IsPointsDiffByDelta(Point(X, Y), FMouseDownCoordOriginal, Mouse.DragThreshold) then
    begin
      FMouseDragDroppingReal:= true;
      BeginDrag(true);
    end;
    exit;
  end;

  //mouse just moved on text
  if bOnMain and (FMouseDownPnt.X<0) then
    begin
      if Shift*[ssLeft, ssRight]=[] then
        if Assigned(FHotspots) and (FHotspots.Count>0) then
        begin
          P:= ClientPosToCaretPos(P, Details);
          if P.Y>=0 then
          begin
            nIndex:= FHotspots.FindByPos(P.X, P.Y);
            if nIndex<>FLastHotspot then
            begin
              if FLastHotspot>=0 then
                if Assigned(FOnHotspotExit) then
                  FOnHotspotExit(Self, FLastHotspot);

              if nIndex>=0 then
                if Assigned(FOnHotspotEnter) then
                  FOnHotspotEnter(Self, nIndex);

              FLastHotspot:= nIndex;
            end;
          end;
        end;
      Exit
    end;

  //mouse dragged to select block
  if bSelecting then
    if bOnMain or bOnGutter then
    begin
      if ssLeft in Shift then
        if Carets.Count>0 then
        begin
          P:= ClientPosToCaretPos(P, Details);
          //Application.MainForm.Caption:= Format('MouseDownPnt %d:%d, CurPnt %d:%d',
            //[FMouseDownPnt.Y, FMouseDownPnt.X, P.Y, P.X]);
          if (P.Y<0) then Exit;

          //mouse not moved at last by char?
          if (FMouseDownPnt.X=P.X) and (FMouseDownPnt.Y=P.Y) then
          begin
            //remove selection from current caret
            nIndex:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
            if Carets.IsIndexValid(nIndex) then
            begin
              Caret:= Carets[nIndex];
              Caret.PosX:= P.X;
              Caret.PosY:= P.Y;
              Caret.EndX:= -1;
              Caret.EndY:= -1;
            end;
            Invalidate;
          end
          else
          begin
            //drag w/out button pressed: single selection
            //ssShift is allowed to select by Shift+MouseWheel
            if not FMouseDownWithCtrl and not FMouseDownWithAlt then
            begin
              if FOptMouseEnableColumnSelection and FOptMouseColumnSelectionWithoutKey then
              begin
                //column selection
                FMouseDownAndColumnSelection:= true;
                DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
                DoSelect_None;
                DoSelect_ColumnBlock_FromPoints(FMouseDownPnt, P);
              end
              else
              if FOptMouseEnableNormalSelection then
              begin
                //normal selection
                DoCaretSingleAsIs;
                if FMouseDownDouble and FOptMouse2ClickDragSelectsWords then
                  DoSelect_WordRange(0, FMouseDownPnt, P)
                else
                  DoSelect_CharRange(0, P);
              end;
            end;

            //drag with Ctrl pressed: add selection
            if (ssLeft in Shift) and
              FMouseDownWithCtrl and
              not FMouseDownWithAlt and
              not FMouseDownWithShift then
            begin
              nIndex:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
              DoSelect_CharRange(nIndex, P);
            end;

            //drag with Alt pressed: column selection
            if FOptMouseEnableColumnSelection then
              if (ssLeft in Shift) and
                not FMouseDownWithCtrl and
                FMouseDownWithAlt and
                not FMouseDownWithShift then
              begin
                FMouseDownAndColumnSelection:= true;
                DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
                DoSelect_None;
                DoSelect_ColumnBlock_FromPoints(FMouseDownPnt, P);
              end;

            DoCaretsSort;
            DoEventCarets;
            Invalidate;
          end;
        end;
      Exit;
    end;
end;

procedure TATSynEdit.MouseLeave;
begin
  if not OptMouseEnableAll then exit;
  inherited;
  DoHintHide;
  DoHotspotsExit;
  if Assigned(FMinimapTooltip) then
    FMinimapTooltip.Hide;
end;

function TATSynEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  if not OptMouseEnableAll then exit(false);
  Result:= DoMouseWheelAction(Shift, WheelDelta>0, false);
end;

function TATSynEdit.DoMouseWheelHorz(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  if not OptMouseEnableAll then exit(false);
  Result:= DoMouseWheelAction([], WheelDelta<0, true);
end;

type
  TATMouseWheelMode = (
    aWheelModeNormal,
    aWheelModeHoriz,
    aWheelModeZoom
    );

function TATSynEdit.DoMouseWheelAction(Shift: TShiftState; AUp, AForceHorz: boolean): boolean;
var
  Mode: TATMouseWheelMode;
  NSpeedX, NSpeedY: integer;
  Pnt: TPoint;
begin
  Result:= false;
  if not OptMouseEnableAll then exit;

  //hide all temporary windows
  DoHotspotsExit;
  if Assigned(FFoldedMarkTooltip) then
    FFoldedMarkTooltip.Hide;
  if Assigned(FMinimapTooltip) then
    FMinimapTooltip.Hide;

  if AForceHorz then
    Mode:= aWheelModeHoriz
  else
  if (Shift=[FOptMouseWheelZoomsWithState]) then
    Mode:= aWheelModeZoom
  else
  // -[ssLeft] to ignore pressed mouse button
  if (Shift-[ssLeft]=[FOptMouseWheelScrollHorzWithState]) then
    Mode:= aWheelModeHoriz
  else
  // -[ssLeft] to ignore pressed mouse button
  if (Shift-[ssLeft]=[]) then
    Mode:= aWheelModeNormal
  else
    exit;

  NSpeedX:= FOptMouseWheelScrollHorzSpeed;
  NSpeedY:= FOptMouseWheelScrollVertSpeed;
  if AUp then NSpeedX:= -NSpeedX;
  if AUp then NSpeedY:= -NSpeedY;

  case Mode of
    aWheelModeNormal:
      begin
        if (not ModeOneLine) and FOptMouseWheelScrollVert then
        begin
          //w/o this handler wheel works only with OS scrollbars, need with new scrollbars too
          DoScrollByDeltaInPixels(0, NSpeedY*FCharSize.Y);
          Invalidate;
          Result:= true;
        end;
      end;

    aWheelModeHoriz:
      begin
        if (not ModeOneLine) and FOptMouseWheelScrollHorz then
        begin
          DoScrollByDelta(NSpeedX, 0);
          Invalidate;
          Result:= true;
        end;
      end;

    aWheelModeZoom:
      begin
        if FOptMouseWheelZooms then
        begin
          DoScaleFontDelta(AUp);
          Result:= true;
        end;
      end;
  end;

  if ssLeft in Shift then
  begin
    Pnt:= ScreenToClient(Mouse.CursorPos);
    MouseMove(Shift, Pnt.X, Pnt.Y);
  end;
end;

function TATSynEdit.DoHandleClickEvent(AEvent: TATSynEditClickEvent): boolean;
begin
  Result:= false;
  if Assigned(AEvent) then
    AEvent(Self, Result);
end;

procedure TATSynEdit.DblClick;
var
  Caret: TATCaretItem;
  SLink: atString;
begin
  if not OptMouseEnableAll then exit;
  inherited;

  if DoHandleClickEvent(FOnClickDbl) then Exit;

  if FOptMouse2ClickOpensURL then
    if Carets.Count>0 then
    begin
      Caret:= Carets[0];
      SLink:= DoGetLinkAtPos(Caret.PosX, Caret.PosY);
      if SLink<>'' then
      begin
        if Assigned(FOnClickLink) then
          FOnClickLink(Self, SLink);
        DoEventCarets;
        exit
      end;
    end;

  case FOptMouse2ClickAction of
    cMouseDblClickSelectEntireLine:
      begin
        DoSelect_Line_ByClick;
      end;
    cMouseDblClickSelectWordChars:
      begin
        FMouseDownDouble:= true;
        DoSelect_ByDoubleClick(true);
      end;
    cMouseDblClickSelectAnyChars:
      begin
        FMouseDownDouble:= true;
        DoSelect_ByDoubleClick(false);
      end;
  end;

  DoEventCarets;
end;

procedure TATSynEdit.TripleClick;
begin
  if not OptMouseEnableAll then exit;
  inherited;

  if DoHandleClickEvent(FOnClickTriple) then Exit;

  if FOptMouse3ClickSelectsLine then
    DoSelect_Line_ByClick;
end;


procedure TATSynEdit.DoSelect_ByDoubleClick(AllowOnlyWordChars: boolean);
begin
  if not Strings.IsIndexValid(FMouseDownPnt.Y) then Exit;
  DoSelect_CharGroupAtPos(FMouseDownPnt, EditorIsPressedCtrl, AllowOnlyWordChars);
  Invalidate;
end;

function TATSynEdit.GetCaretManyAllowed: boolean;
begin
  Result:= Carets.ManyAllowed;
end;

procedure TATSynEdit.SetCaretManyAllowed(AValue: boolean);
begin
  Carets.ManyAllowed:= AValue;
  if not AValue then
    DoCaretSingleAsIs;
end;


procedure TATSynEdit.DoSelect_Line_ByClick;
var
  P: TPoint;
  Details: TATPosDetails;
begin
  P:= ScreenToClient(Mouse.CursorPos);
  if PtInRect(FRectMain, P) then
  begin
    P:= ClientPosToCaretPos(P, Details);
    if P.Y<0 then Exit;
    DoSelect_Line(P);
    Invalidate;
  end;
end;

procedure TATSynEdit.Invalidate;
begin
  Include(FPaintFlags, cPaintUpdateBitmap);
  inherited;
end;

procedure TATSynEdit.InvalidateHilitingCache;
begin
  FAdapterCache.Clear;
end;

procedure TATSynEdit.InvalidateHilitingCache(ALineIndex: integer);
begin
  if Assigned(FAdapterCache) then
    FAdapterCache.DeleteForLine(ALineIndex);
end;

function TATSynEdit._IsFocused: boolean;
//this method is to speedup focused check (TControl.Focused prop is slower)
var
  C: TControl;
begin
  Result:= false;
  if not FIsEntered then exit;
  if not Application.Active then exit;

  C:= Self;
  while Assigned(C.Parent) do
    C:= C.Parent;
  if C is TForm then
    if not (C as TForm).Active then exit;

  Result:= true;
end;

procedure TATSynEdit.TimerBlinkTick(Sender: TObject);
begin
  if not FCaretShowEnabled then exit;

  if FCaretStopUnfocused and not _IsFocused then
    if FCaretShown then
      exit;

  if not DoubleBuffered then
    FCaretAllowNextBlink:= not FCaretAllowNextBlink;

  inherited Invalidate;
end;

procedure TATSynEdit.TimerScrollTick(Sender: TObject);
var
  nIndex: integer;
  PClient, PCaret: TPoint;
  Details: TATPosDetails;
begin
  PClient:= ScreenToClient(Mouse.CursorPos);
  PClient.X:= Max(FRectMain.Left, PClient.X);
  PClient.Y:= Max(FRectMain.Top, PClient.Y);
  PClient.X:= Min(FRectMain.Right, PClient.X);
  PClient.Y:= Min(FRectMain.Bottom, PClient.Y);

  case FMouseAutoScroll of
    cDirUp:    DoScrollByDelta(0, -cSpeedScrollAutoVert);
    cDirDown:  DoScrollByDelta(0, cSpeedScrollAutoVert);
    cDirLeft:  DoScrollByDelta(-cSpeedScrollAutoHorz, 0);
    cDirRight: DoScrollByDelta(cSpeedScrollAutoHorz, 0);
    else Exit;
  end;

  PCaret:= ClientPosToCaretPos(PClient, Details);

  if (PCaret.X>=0) and (PCaret.Y>=0) then
  begin
    if FMouseDownGutterLineNumber>=0 then
    begin
      DoSelect_LineRange(FMouseDownGutterLineNumber, PCaret);
    end
    else
    if IsSelRectEmpty then
    begin
      nIndex:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
      if nIndex>=0 then
        Carets[nIndex].SelectToPoint(PCaret.X, PCaret.Y);
    end
    else
    begin
      DoSelect_ColumnBlock_FromPoints(FMouseDownPnt, PCaret);
    end;
  end;

  DoCaretsSort;
  DoEventCarets;
  //DoEventScroll;
  Invalidate;
end;

procedure TATSynEdit.TimerNiceScrollTick(Sender: TObject);
var
  Pnt: TPoint;
  Dx, Dy: integer;
  Dir: TATDirection;
begin
  Pnt:= ScreenToClient(Mouse.CursorPos);
  if not PtInRect(FRectMain, Pnt) then Exit;

  //delta in pixels
  Dx:= Pnt.X-FMouseNiceScrollPos.X;
  Dy:= Pnt.Y-FMouseNiceScrollPos.Y;

  if (Abs(Dx)<=cBitmapNiceScroll.Height div 2) and
    (Abs(Dy)<=cBitmapNiceScroll.Height div 2) then
    begin
      Cursor:= crNiceScrollNone;
      Exit;
    end;

  if (Dy<0) and (Abs(Dy)>Abs(Dx)) then Dir:= cDirUp else
    if (Dy>0) and (Abs(Dy)>Abs(Dx)) then Dir:= cDirDown else
      if Dx<0 then Dir:= cDirLeft else
        Dir:= cDirRight;

  case Dir of
    cDirLeft:  Cursor:= crNiceScrollLeft;
    cDirRight: Cursor:= crNiceScrollRight;
    cDirUp:    Cursor:= crNiceScrollUp;
    cDirDown:  Cursor:= crNiceScrollDown;
  end;

  //delta in pixels
  Dx:= Sign(Dx)*((Abs(Dx)-cBitmapNiceScroll.Height div 2) + 1) div cSpeedScrollNice;
  Dy:= Sign(Dy)*((Abs(Dy)-cBitmapNiceScroll.Height div 2) + 1) div cSpeedScrollNice;

  if Dir in [cDirLeft, cDirRight] then
    DoScrollByDeltaInPixels(Dx, 0)
  else
    DoScrollByDeltaInPixels(0, Dy);

  Invalidate;
end;

procedure TATSynEdit.DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
var
  R: TRect;
  i: integer;
  Item: TATCaretItem;
  CaretProps: TATCaretProps;
  NCaretColor: TColor;
begin
  if not FCaretShowEnabled then exit;

  if ModeReadOnly then
    CaretProps:= FCaretPropsReadonly
  else
  if ModeOverwrite then
    CaretProps:= FCaretPropsOverwrite
  else
    CaretProps:= FCaretPropsNormal;

  NCaretColor:= Colors.Caret;
  { //block was needed when we didn't have OptCaretHideUnfocused
  if (not FCaretStopUnfocused) or _IsFocused then
    NCaretColor:= Colors.Caret
  else
    //I cannot find proper color of NCaretColor, to make unfocused carets invisible,
    //tried several combinations: Colors.TextBG with Colors.TextFont with 'xor'.
    //at least value 'Colors.TextBG xor Colors.TextFont' gives PALE caret color
    //on many CudaText themes (default and dark themes).
    NCaretColor:= Colors.TextBG xor Colors.TextFont;
    }

  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    R.Left:= Item.CoordX;
    R.Top:= Item.CoordY;
    R.Right:= R.Left+FCharSize.X;
    R.Bottom:= R.Top+FCharSize.Y;

    //check caret is visible (IntersectRect is slower)
    if R.Right<=FRectMain.Left then Continue;
    if R.Bottom<=FRectMain.Top then Continue;
    if R.Left>=FRectMain.Right then Continue;
    if R.Top>=FRectMain.Bottom then Continue;

    DoCaretsApplyShape(R, CaretProps, FCharSize.X, FCharSize.Y);

    if FCaretBlinkEnabled then
    begin
      FCaretShown:= not FCaretShown;
      CanvasInvertRect(C, R, NCaretColor);
      //if shape FrameFull, invert inner area
      if CaretProps.EmptyInside then
        CanvasInvertRect(C, Rect(R.Left+1, R.Top+1, R.Right-1, R.Bottom-1), NCaretColor);
    end
    else
    begin
      FCaretShown:= true;
      //paint non-blinking caret simpler
      C.Brush.Color:= NCaretColor;
      C.FillRect(R);
    end;

    if AWithInvalidate then
      if not (csCustomPaint in ControlState) then //disable during Paint
        InvalidateRect(Handle, @R, false);
  end;
end;

procedure TATSynEdit.DoPaintMarkerOfDragDrop(C: TCanvas);
var
  Details: TATPosDetails;
  P: TPoint;
  R: TRect;
begin
  if not FOptShowDragDropMarker then exit;
  if not (FMouseDragDropping and FMouseDragDroppingReal) then exit;

  P:= ClientPosToCaretPos(ScreenToClient(Mouse.CursorPos), Details);
  if P.Y<0 then exit;
  P:= CaretPosToClientPos(P);
  if P.Y<0 then exit;
  if not PtInRect(FRectMain, P) then exit;

  R.Left:= P.X-1;
  R.Right:= P.X+1; //2 pixels width
  R.Top:= P.Y;
  R.Bottom:= P.Y+FCharSize.Y; //100% height

  C.Brush.Color:= Colors.Markers;
  C.FillRect(R);

  //InvalidateRect(Handle, @R, false);
end;

procedure TATSynEdit.DoPaintModeStatic;
begin
  FTimerBlink.Enabled:= false;
end;

procedure TATSynEdit.DoPaintModeBlinking;
begin
  FTimerBlink.Enabled:= false;
  FTimerBlink.Enabled:= FTimersEnabled and FCaretBlinkEnabled;
end;


procedure TATSynEdit.DoPaintLineIndent(C: TCanvas;
  const ARect: TRect;
  ACharSize: TPoint;
  ACoordY: integer;
  AIndentSize: integer;
  AColorBG: TColor;
  AScrollPos: integer;
  AIndentLines: boolean);
var
  i: integer;
  RBack: TRect;
begin
  if AIndentSize=0 then Exit;

  RBack:= Rect(0, 0, AIndentSize*ACharSize.X, ACharSize.Y);
  OffsetRect(RBack, ARect.Left-AScrollPos*ACharSize.X, ACoordY);

  C.Brush.Color:= AColorBG;
  C.FillRect(RBack);

  if AIndentLines then
    for i:= 0 to AIndentSize-1 do
      if i mod FTabSize = 0 then
        CanvasLine_DottedVertAlt(C,
          Colors.IndentVertLines,
          ARect.Left + (i-AScrollPos)*ACharSize.X,
          ACoordY,
          ACoordY+ACharSize.Y);
end;

procedure TATSynEdit.DoPaintSelectedLineBG(C: TCanvas;
  ACharSize: TPoint;
  const AVisRect: TRect;
  APointLeft, APointText: TPoint;
  const AWrapItem: TATWrapItem;
  ALineWidth: integer;
  const AScrollHorz: TATSynScrollInfo);
var
  NLineIndex, NPartXAfter: integer;
  NLeft, NRight, i: integer;
  Ranges: TATSimpleRangeArray;
  RangeFrom, RangeTo: integer;
begin
  NLineIndex:= AWrapItem.NLineIndex;

  if not IsSelRectEmpty then
  begin
   //avoid weird look when empty area is filled in word-wrap mode
   if FWrapMode=cWrapOff then
    if (NLineIndex>=FSelRect.Top) and (NLineIndex<=FSelRect.Bottom) then
    begin
      NLeft:= APointLeft.X+ACharSize.X*(FSelRect.Left-AScrollHorz.NPos);
      NRight:= NLeft+ACharSize.X*FSelRect.Width;
      NLeft:= Max(NLeft, APointText.X+ALineWidth);
      if (NLeft<NRight) then
      begin
        C.Brush.Color:= Colors.TextSelBG;
        C.FillRect(
          NLeft,
          APointLeft.Y,
          NRight,
          APointLeft.Y+ACharSize.Y);
      end;
    end;
  end
  else
  begin
    if not FOptShowFullSel then exit;
    NPartXAfter:= AWrapItem.NCharIndex-1+AWrapItem.NLength;

    //here we calculate ranges (XFrom, XTo) where selection(s) overlap current line,
    //and then paint fillrect for them
    TempSel_GetRangesInLineAfterPoint(NPartXAfter, NLineIndex, Ranges);

    for i:= 0 to Length(Ranges)-1 do
    begin
      RangeFrom:= Ranges[i].NFrom;
      RangeTo:= Ranges[i].NTo;

      //don't paint tail for cases
      //1) OptShowFullSel=false
      //2) middle WrapItem
      if RangeFrom>NPartXAfter then
        if (AWrapItem.NFinal=cWrapItemMiddle) then
          Continue;

      NLeft:= APointText.X + ALineWidth + (RangeFrom-NPartXAfter)*ACharSize.X;
      if RangeTo=MaxInt then
        NRight:= AVisRect.Right
      else
        NRight:= NLeft+(RangeTo-RangeFrom)*ACharSize.X;

      C.Brush.Color:= Colors.TextSelBG;
      C.FillRect(
        Max(AVisRect.Left, NLeft),
        APointText.Y,
        Min(AVisRect.Right, NRight),
        APointText.Y+ACharSize.Y
        );
    end;
  {
  if FOptShowFullSel then
    if AEolSelected then
    begin
      C.Brush.Color:= Colors.TextSelBG;
      C.FillRect(
        APointText.X,
        APointText.Y,
        AVisRect.Right,
        APointText.Y+ACharSize.Y);
    end;
    }
  end;
end;

procedure TATSynEdit.DoPaintNiceScroll(C: TCanvas);
begin
  if MouseNiceScroll then
    C.Draw(
      FMouseNiceScrollPos.X - cBitmapNiceScroll.Height div 2,
      FMouseNiceScrollPos.Y - cBitmapNiceScroll.Height div 2,
      cBitmapNiceScroll);
end;

procedure TATSynEdit.DoPaintLineNumber(C: TCanvas; ALineIndex, ACoordTop: integer; ABand: TATGutterItem);
var
  Str: string;
  Size: integer;
  CoordX: integer;
begin
  Str:= DoFormatLineNumber(ALineIndex+1);

  //C.Font.Name:= Font.Name;
  //C.Font.Size:= DoScaleFont(Font.Size);
  //Size:= C.TextWidth(Str);
  Size:= FCharSize.X*Length(Str);

  case FOptNumbersAlignment of
    taLeftJustify:
      CoordX:= ABand.Left + FNumbersIndent;
    taRightJustify:
      CoordX:= ABand.Right - Size - FNumbersIndent;
    taCenter:
      CoordX:= (ABand.Left + ABand.Right - Size) div 2;
  end;

  CanvasTextOutSimplest(C, CoordX, ACoordTop, Str);
end;


function TATSynEdit.DoEventCommand(ACommand: integer; const AText: string): boolean; inline;
begin
  Result:= false;
  if Assigned(FOnCommand) then
    FOnCommand(Self, ACommand, AText, Result);
end;

procedure TATSynEdit.DoEventCommandAfter(ACommand: integer; const AText: string); inline;
begin
  if Assigned(FOnCommandAfter) then
    FOnCommandAfter(Self, ACommand, AText);
end;


procedure TATSynEdit.DoEventCarets;
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorCaretMove(Self);

  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self);
end;

procedure TATSynEdit.DoEventScroll;
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorScroll(Self);

  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATSynEdit.DoEventChange(AllowOnChange: boolean=true);
begin
  FLinkCache.Clear;

  if Assigned(FAdapterHilite) then
  begin
    InvalidateHilitingCache;
    FAdapterHilite.OnEditorChange(Self);
  end;

  if AllowOnChange then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);

    if FPrevModified<>Modified then
    begin
      FPrevModified:= Modified;
      if Assigned(FOnChangeModified) then
        FOnChangeModified(Self);
    end;
  end;

  //fire OnIdle after pause after change
  if FOptIdleInterval>0 then
  begin
    FTimerIdle.Enabled:= false;
    FTimerIdle.Interval:= FOptIdleInterval;
    FTimerIdle.Enabled:= true;
  end;
end;

procedure TATSynEdit.DoEventState;
begin
  if Assigned(FOnChangeState) then
    FOnChangeState(Self);
end;

procedure TATSynEdit.DoEventClickGutter(ABandIndex, ALineNumber: integer); inline;
begin
  if Assigned(FOnClickGutter) then
    FOnClickGutter(Self, ABandIndex, ALineNumber);
end;

procedure TATSynEdit.DoEventClickMicromap(AX, AY: integer); inline;
begin
  if Assigned(FOnClickMicromap) then
    FOnClickMicromap(Self, AX, AY);
end;

procedure TATSynEdit.DoEventDrawBookmarkIcon(C: TCanvas; ALineNumber: integer; const ARect: TRect); inline;
begin
  if Assigned(FOnDrawBookmarkIcon) then
    FOnDrawBookmarkIcon(Self, C, ALineNumber, ARect);
end;

procedure TATSynEdit.DoEventBeforeCalcHilite; inline;
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorBeforeCalcHilite(Self);

  if Assigned(FOnBeforeCalcHilite) then
    FOnBeforeCalcHilite(Self);
end;


procedure TATSynEdit.DoScrollToBeginOrEnd(AToBegin: boolean);
begin
  FScrollHorz.SetZero;
  if AToBegin then
    FScrollVert.SetZero
  else
    FScrollVert.SetLast;

  FScrollHorz.NPixelOffset:= 0;
  FScrollVert.NPixelOffset:= 0;

  UpdateScrollbars(true);
end;

procedure TATSynEdit.DoScrollByDelta(ADeltaX, ADeltaY: integer);
//
  procedure _Delta(var AInfo: TATSynScrollInfo; ADelta: integer); inline;
  begin
    if ADelta=0 then exit;
    with AInfo do
    begin
      NPos:= Max(0, Min(NPosLast, NPos+ADelta));
      if (NPos=0) or (NPos>=NPosLast) then
        NPixelOffset:= 0;
    end;
  end;
//
begin
  _Delta(FScrollHorz, ADeltaX);
  _Delta(FScrollVert, ADeltaY);
  UpdateScrollbars(true);
end;

procedure TATSynEdit.DoScrollByDeltaInPixels(ADeltaX, ADeltaY: integer);
//
  procedure _Delta(var AInfo: TATSynScrollInfo; ADelta: integer); inline;
  begin
    if ADelta=0 then exit;
    UpdateScrollInfoFromSmoothPos(AInfo,
      Min(AInfo.SmoothPosLast, AInfo.SmoothPos + ADelta));
  end;
//
begin
  _Delta(FScrollHorz, ADeltaX);
  _Delta(FScrollVert, ADeltaY);
end;

procedure TATSynEdit.MenuClick(Sender: TObject);
var
  Cmd: integer;
begin
  Cmd:= (Sender as TMenuItem).Tag;
  if Cmd>0 then
  begin
    DoCommand(Cmd);
    Invalidate;
  end;
end;

procedure TATSynEdit.MenuStdPopup(Sender: TObject);
var
  i: integer;
begin
  MenuitemTextCut.Caption:= cStrMenuitemCut;
  MenuitemTextCopy.Caption:= cStrMenuitemCopy;
  MenuitemTextPaste.Caption:= cStrMenuitemPaste;
  MenuitemTextDelete.Caption:= cStrMenuitemDelete;
  MenuitemTextSelAll.Caption:= cStrMenuitemSelectAll;
  MenuitemTextUndo.Caption:= cStrMenuitemUndo;
  MenuitemTextRedo.Caption:= cStrMenuitemRedo;

  for i:= 0 to FMenuStd.Items.Count-1 do
    with FMenuStd.Items[i] do
    begin
      if Assigned(FKeymap) then
        ShortCut:= FKeymap.GetShortcutFromCommand(Tag);

      //separator items: hide if read-only, nicer menu
      if Caption='-' then
        Visible:= not ModeReadOnly;

      case Tag of
        cCommand_ClipboardCut:
          begin
            Enabled:= not ModeReadOnly;
            Visible:= not ModeReadOnly;
          end;
        cCommand_ClipboardPaste:
          begin
            Enabled:= not ModeReadOnly and Clipboard.HasFormat(CF_Text);
            Visible:= not ModeReadOnly;
          end;
        cCommand_TextDeleteSelection:
          begin
            Enabled:= not ModeReadOnly and Carets.IsSelection;
            Visible:= not ModeReadOnly;
          end;
        cCommand_Undo:
          begin
            Enabled:= not ModeReadOnly and (UndoCount>0);
            Visible:= not ModeReadOnly;
          end;
        cCommand_Redo:
          begin
            Enabled:= not ModeReadOnly and (RedoCount>0);
            Visible:= not ModeReadOnly;
          end;
      end;
    end;
end;

procedure TATSynEdit.InitMenuStd;
  //
  function Add(const SName: string; Cmd: integer): TMenuItem; inline;
  var
    MI: TMenuItem;
  begin
    MI:= TMenuItem.Create(FMenuStd);
    MI.Caption:= SName;
    MI.Tag:= Cmd;
    MI.OnClick:= @MenuClick;
    Result:= MI;
    FMenuStd.Items.Add(MI);
  end;
  //
begin
  if FMenuStd=nil then
  begin
    FMenuStd:= TPopupMenu.Create(Self);
    FMenuStd.OnPopup:= @MenuStdPopup;

    MenuitemTextUndo:= Add('Undo', cCommand_Undo);
    MenuitemTextRedo:= Add('Redo', cCommand_Redo);
    Add('-', 0);
    MenuitemTextCut:= Add('Cut', cCommand_ClipboardCut);
    MenuitemTextCopy:= Add('Copy', cCommand_ClipboardCopy);
    MenuitemTextPaste:= Add('Paste', cCommand_ClipboardPaste);
    MenuitemTextDelete:= Add('Delete', cCommand_TextDeleteSelection);
    Add('-', 0);
    MenuitemTextSelAll:= Add('Select all', cCommand_SelectAll);
  end;
end;

//drop selection of 1st caret into mouse-pos
procedure TATSynEdit.DoDropText(AndDeleteSelection: boolean);
var
  P, PosAfter, Shift: TPoint;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
  Str: atString;
  Relation: TATPosRelation;
  Details: TATPosDetails;
begin
  if Carets.Count<>1 then Exit; //allow only 1 caret
  Carets[0].GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then Exit;

  DoSelect_None;

  //calc insert-pos
  P:= ScreenToClient(Mouse.CursorPos);
  P:= ClientPosToCaretPos(P, Details);
  if P.Y<0 then exit;

  //can't drop into selection
  Relation:= IsPosInRange(P.X, P.Y, X1, Y1, X2, Y2);
  if Relation=cRelateInside then exit;

  Str:= Strings.TextSubstring(X1, Y1, X2, Y2);
  if Str='' then exit;

  //insert before selection?
  if Relation=cRelateBefore then
  begin
    if AndDeleteSelection then
      Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
    Strings.TextInsert(P.X, P.Y, Str, false, Shift, PosAfter);

    //select moved text
    DoCaretSingle(PosAfter.X, PosAfter.Y, P.X, P.Y);
  end
  else
  begin
    Strings.TextInsert(P.X, P.Y, Str, false, Shift, PosAfter);

    //select moved text
    DoCaretSingle(PosAfter.X, PosAfter.Y, P.X, P.Y);

    if AndDeleteSelection then
    begin
      Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
      DoCaretsShift(0, X1, Y1, Shift.X, Shift.Y, PosAfter);
    end;
  end;

  Update(true);
  DoEventCarets;
  DoEventChange;
end;

function TATSynEdit.IndentString: string;
begin
  if FOptTabSpaces then
    Result:= StringOfChar(' ', FTabSize)
  else
    Result:= #9;
end;

function TATSynEdit.GetAutoIndentString(APosX, APosY: integer; AUseIndentRegexRule: boolean): atString;
var
  StrPrev, StrIndent: atString;
  NChars, NSpaces: integer;
  MatchPos, MatchLen: integer;
  bAddIndent: boolean;
begin
  Result:= '';
  if not FOptAutoIndent then Exit;
  if not Strings.IsIndexValid(APosY) then Exit;

  StrPrev:= Strings.LineSub(APosY, 1, APosX);
  if StrPrev='' then exit;
  NChars:= SGetIndentChars(StrPrev); //count of chars in indent

  bAddIndent:=
    AUseIndentRegexRule and
    (FOptAutoIndentRegexRule<>'') and
    SFindRegexMatch(StrPrev, FOptAutoIndentRegexRule, MatchPos, MatchLen);

  StrIndent:= Copy(StrPrev, 1, NChars);
  NSpaces:= Length(FTabHelper.TabsToSpaces(APosY, StrIndent));

  case FOptAutoIndentKind of
    cIndentAsPrevLine:
      Result:= StrIndent;
    cIndentSpacesOnly:
      Result:= StringOfChar(' ', NSpaces);
    cIndentTabsOnly:
      Result:= StringOfChar(#9, NSpaces div FTabSize);
    cIndentTabsAndSpaces:
      Result:= StringOfChar(#9, NSpaces div FTabSize) + StringOfChar(' ', NSpaces mod FTabSize);
    cIndentToOpeningBracket:
      begin
        //indent like in prev line + spaces up to opening bracket
        NSpaces:= SGetIndentCharsToOpeningBracket(StrPrev);
        Result:= StrIndent + StringOfChar(' ', NSpaces-Length(StrIndent));
      end;
  end;

  if bAddIndent then
    Result:= Result+IndentString;
end;

function TATSynEdit.GetModified: boolean;
begin
  Result:= Strings.Modified;
end;

procedure TATSynEdit.SetModified(AValue: boolean);
begin
  Strings.Modified:= AValue;
  if AValue then
    DoEventChange
  else
    FPrevModified:= false;
end;

function TATSynEdit.GetOneLine: boolean;
begin
  Result:= Strings.OneLine;
end;

function TATSynEdit.GetRedoCount: integer;
begin
  Result:= Strings.RedoCount;
end;

function TATSynEdit.GetLinesFromTop: integer;
var
  P: TPoint;
begin
  if Carets.Count=0 then
    begin Result:= 0; Exit end;
  with Carets[0] do
    P:= Point(PosX, PosY);
  P:= CaretPosToClientPos(P);
  Result:= (P.Y-FRectMain.Top) div FCharSize.Y;
end;

function TATSynEdit.GetText: atString;
begin
  Result:= DoGetTextString;
end;

function TATSynEdit.DoGetTextString: atString;
begin
  //TATEdit overrides it
  Result:= Strings.TextString_Unicode;
end;

function TATSynEdit.IsRepaintNeededOnEnterOrExit: boolean; inline;
begin
  Result:=
    FOptShowCurLineOnlyFocused or
    FOptBorderFocusedActive or
    FCaretStopUnfocused;
end;

procedure TATSynEdit.DoEnter;
begin
  inherited;
  FIsEntered:= true;
  if FCaretHideUnfocused then
    FCaretShowEnabled:= true;
  if IsRepaintNeededOnEnterOrExit then
    Invalidate;
  TimersStart;
end;

procedure TATSynEdit.DoExit;
begin
  inherited;
  FIsEntered:= false;
  if FCaretHideUnfocused then
    FCaretShowEnabled:= false;
  if IsRepaintNeededOnEnterOrExit then
    Invalidate;
  TimersStop;
end;

procedure TATSynEdit.TimersStart;
//TimersStart/Stop are added to minimize count of running timers
begin
  FTimersEnabled:= true;
  if Assigned(FTimerBlink) then
    FTimerBlink.Enabled:= FTimersEnabled and FCaretBlinkEnabled;
end;

procedure TATSynEdit.TimersStop;
begin
  FTimersEnabled:= false;
  if Assigned(FTimerBlink) then FTimerBlink.Enabled:= false;
  if Assigned(FTimerIdle) then FTimerIdle.Enabled:= false;
  if Assigned(FTimerScroll) then FTimerScroll.Enabled:= false;
  if Assigned(FTimerNiceScroll) then FTimerNiceScroll.Enabled:= false;
end;

procedure TATSynEdit.DoMinimapClick(APosY: integer);
var
  NItem: integer;
begin
  NItem:= GetMinimap_ClickedPosToWrapIndex(APosY);
  if NItem>=0 then
  begin
    NItem:= Max(0, NItem - GetVisibleLines div 2);
    FScrollVert.NPos:= Min(NItem, FScrollVert.NMax);
    UpdateScrollbars(true);
    Update;
    UpdateMinimapTooltip;
  end;
end;

procedure TATSynEdit.DoMinimapDrag(APosY: integer);
begin
  FScrollVert.NPos:= GetMinimap_DraggedPosToWrapIndex(APosY);
  UpdateScrollbars(true);
  Update;
end;

function TATSynEdit.GetUndoAsString: string;
begin
  Result:= Strings.UndoAsString;
end;

function TATSynEdit.GetRedoAsString: string;
begin
  Result:= Strings.RedoAsString;
end;

procedure TATSynEdit.SetUndoLimit(AValue: integer);
begin
  FOptUndoLimit:= Max(0, AValue);
  Strings.UndoLimit:= FOptUndoLimit;
end;

function TATSynEdit.GetUndoAfterSave: boolean;
begin
  Result:= Strings.UndoAfterSave;
end;

function TATSynEdit.GetUndoCount: integer;
begin
  Result:= Strings.UndoCount;
end;

procedure TATSynEdit.SetUndoAfterSave(AValue: boolean);
begin
  Strings.UndoAfterSave:= AValue;
end;

procedure TATSynEdit.SetUndoAsString(const AValue: string);
begin
  Strings.UndoAsString:= AValue;
end;

procedure TATSynEdit.DoScaleFontDelta(AInc: boolean);
const
  cMinScale = 60;
  cStep = 10;
//var
//  NTop: integer;
begin
  if FOptScaleFont=0 then
  begin
    FOptScaleFont:= EditorScaleFontPercents;
    if FOptScaleFont=0 then
      FOptScaleFont:= EditorScalePercents;
  end;

  if not AInc then
    if FOptScaleFont<=cMinScale then Exit;

  //NTop:= LineTop;
  FOptScaleFont:= FOptScaleFont+cStep*BoolToPlusMinusOne[AInc];
  //LineTop:= NTop;
  Update;
end;

procedure TATSynEdit.BeginUpdate; inline;
begin
  Inc(FPaintLocked);
  Invalidate;
end;

procedure TATSynEdit.EndUpdate; inline;
begin
  Dec(FPaintLocked);
  if FPaintLocked<0 then
    FPaintLocked:= 0;
  if FPaintLocked=0 then
    Invalidate;
end;

function TATSynEdit.IsLocked: boolean; inline;
begin
  Result:= FPaintLocked>0;
end;

function TATSynEdit.TextSelectedEx(ACaret: TATCaretItem): atString;
var
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  Result:= '';
  ACaret.GetRange(X1, Y1, X2, Y2, bSel);
  if bSel then
    Result:= Strings.TextSubstring(X1, Y1, X2, Y2);
end;

function TATSynEdit.TextSelected: atString;
begin
  if Carets.Count>0 then
    Result:= TextSelectedEx(Carets[0])
  else
    Result:= '';
end;

function TATSynEdit.TextCurrentWord: atString;
var
  Str: atString;
  Caret: TATCaretItem;
  N1, N2: integer;
begin
  Result:= '';
  if Carets.Count=0 then Exit;
  Caret:= Carets[0];
  Str:= Strings.Lines[Caret.PosY];
  SFindWordBounds(Str, Caret.PosX, N1, N2, OptNonWordChars);
  if N2>N1 then
    Result:= Copy(Str, N1+1, N2-N1);
end;

function TATSynEdit.GetMouseNiceScroll: boolean;
begin
  Result:= FTimerNiceScroll.Enabled;
end;

procedure TATSynEdit.SetEnabledSlowEvents(AValue: boolean);
var
  St: TATStrings;
begin
  St:= Strings;
  if not AValue then
  begin
    St.DoClearUndo(true);
    St.EnabledChangeEvents:= false;
    if Carets.Count>0 then
    begin
      FLastLineOfSlowEvents:= Carets[0].FirstTouchedLine;
      if not St.IsIndexValid(FLastLineOfSlowEvents) then
        FLastLineOfSlowEvents:= -1;
    end;
  end
  else
  begin
    St.DoClearUndo(false);
    St.EnabledChangeEvents:= true;
    if St.IsIndexValid(FLastLineOfSlowEvents) then
    begin
      St.DoEventLog(FLastLineOfSlowEvents);
      St.DoEventChange(cLineChangeEdited, FLastLineOfSlowEvents, 1);
      FLastLineOfSlowEvents:= -1;
    end;
  end;
end;

procedure TATSynEdit.SetMouseNiceScroll(AValue: boolean);
begin
  FTimerNiceScroll.Enabled:= AValue;
  if not AValue then
    UpdateCursor;
  Invalidate;
end;

function TATSynEdit.GetEndOfFilePos: TPoint;
begin
  Result.X:= 0;
  Result.Y:= 0;
  if Strings.Count>0 then
  begin
    Result.Y:= Strings.Count-1;
    Result.X:= Strings.LinesLen[Result.Y];
    if Strings.LinesEnds[Result.Y]<>cEndNone then
      Inc(Result.X);
  end;
end;

procedure TATSynEdit.DoPaintGutterFolding(C: TCanvas;
  AWrapItemIndex: integer;
  ACoordX1, ACoordX2, ACoordY1, ACoordY2: integer);
var
  CoordXCenter, CoordYCenter: integer;
  IsLineUp, IsLineDown: boolean;
  //
  procedure DrawUp; inline;
  begin
    if IsLineUp then
      CanvasLineVert(C,
        CoordXCenter,
        ACoordY1,
        CoordYCenter
        );
  end;
  procedure DrawDown; inline;
  begin
    if IsLineDown then
      CanvasLineVert(C,
        CoordXCenter,
        CoordYCenter,
        ACoordY2
        );
  end;
  //
var
  State: (cFoldbarNone, cFoldbarBegin, cFoldbarEnd, cFoldbarMiddle);
  NLineIndex: integer;
  WrapItem: TATWrapItem;
  Rng: PATSynRange;
  NIndexOfCurrentRng, NIndexOfCaretRng: integer;
  Caret: TATCaretItem;
  IsPlus: boolean;
  bHiliteLines: boolean;
  NColorLine, NColorPlus: TColor;
begin
  if not FOptGutterShowFoldAlways then
    if not FCursorOnGutter then Exit;

  WrapItem:= FWrapInfo[AWrapItemIndex];
  NLineIndex:= WrapItem.NLineIndex;

  //find deepest range which includes caret pos
  NIndexOfCaretRng:= -1;
  if FOptGutterShowFoldLinesForCaret then
    if Carets.Count>0 then
    begin
      Caret:= Carets[0];
      NIndexOfCaretRng:= FFold.FindDeepestRangeContainingLine(Caret.PosY, false);
    end;

  NIndexOfCurrentRng:= FFold.FindDeepestRangeContainingLine(NLineIndex, false);
  if NIndexOfCurrentRng<0 then exit;
  bHiliteLines:= NIndexOfCurrentRng=NIndexOfCaretRng;

  //calc state
  State:= cFoldbarNone;
  IsPlus:= false;
  IsLineUp:= false;
  IsLineDown:= false;

  Rng:= Fold.ItemPtr(NIndexOfCurrentRng);
  if Rng^.Y<NLineIndex then IsLineUp:= true;
  if Rng^.Y2>NLineIndex then IsLineDown:= true;
  if Rng^.Y=NLineIndex then
  begin
    State:= cFoldbarBegin;
    //don't override found [+], 2 blocks can start at same pos
    if not IsPlus then IsPlus:= Rng^.Folded;
  end;
  if Rng^.Y2=NLineIndex then
    if State<>cFoldbarBegin then
      State:= cFoldbarEnd;

  //correct state for wrapped line
  if State=cFoldbarBegin then
    if not WrapItem.bInitial then
      State:= cFoldbarMiddle;

  //correct state for wrapped line
  if State=cFoldbarEnd then
    if WrapItem.NFinal=cWrapItemMiddle then
      State:= cFoldbarMiddle;

  if bHiliteLines then
    NColorPlus:= Colors.GutterFoldLine2
  else
    NColorPlus:= Colors.GutterFoldLine;

  if FOptGutterShowFoldLines then
    NColorLine:= NColorPlus
  else
    NColorLine:= Colors.GutterFoldBG;
  C.Pen.Color:= NColorLine;

  CoordXCenter:= (ACoordX1+ACoordX2) div 2;
  CoordYCenter:= (ACoordY1+ACoordY2) div 2;

  case State of
    cFoldbarBegin:
      begin
        if FOptGutterShowFoldLinesAll then
        begin
          DrawUp;
          DrawDown;
        end;

        if not IsPlus then
          DrawDown;

        DoPaintGutterPlusMinus(C,
          CoordXCenter, CoordYCenter, IsPlus, NColorPlus);
      end;
    cFoldbarEnd:
      begin
        if FOptGutterShowFoldLinesAll then
        begin
          DrawUp;
          DrawDown;
        end;

        Dec(ACoordY2, cSizeGutterFoldLineDx);
        CanvasLineVert(C,
          CoordXCenter,
          ACoordY1,
          ACoordY2
          );
        CanvasLineHorz(C,
          CoordXCenter,
          ACoordY2,
          CoordXCenter + EditorScale(FOptGutterPlusSize)
          );
      end;
    cFoldbarMiddle:
      begin
        CanvasLineVert(C,
          CoordXCenter,
          ACoordY1,
          ACoordY2
          );
      end;
    else
      begin
        DrawUp;
        DrawDown;
      end;
  end;
end;

procedure TATSynEdit.DoPaintGutterDecor(C: TCanvas; ALine: integer; const ARect: TRect);
var
  Decor: TATGutterDecorItem;
  Style, StylePrev: TFontStyles;
  N: integer;
  Ext: TSize;
begin
  if FGutterDecor=nil then exit;
  N:= FGutterDecor.Find(ALine);
  if N<0 then exit;
  Decor:= FGutterDecor[N];

  if Decor.Data.Text<>'' then
  begin
    C.Font.Color:= Decor.Data.TextColor;
    Style:= [];
    if Decor.Data.TextBold then
      Include(Style, fsBold);
    if Decor.Data.TextItalic then
      Include(Style, fsItalic);
    StylePrev:= C.Font.Style;
    C.Font.Style:= Style;

    Ext:= C.TextExtent(Decor.Data.Text);
    C.Brush.Color:= Colors.GutterBG;
    C.TextOut(
      (ARect.Left+ARect.Right-Ext.cx) div 2,
      (ARect.Top+ARect.Bottom-Ext.cy) div 2,
      Decor.Data.Text
      );
    C.Font.Style:= StylePrev;
  end
  else
  if Assigned(FGutterDecorImages) then
  begin
    N:= Decor.Data.ImageIndex;
    if (N>=0) and (N<FGutterDecorImages.Count) then
      FGutterDecorImages.Draw(C,
        (ARect.Left+ARect.Right-FGutterDecorImages.Width) div 2,
        (ARect.Top+ARect.Bottom-FGutterDecorImages.Height) div 2,
        N
        );
  end;
end;

procedure TATSynEdit.DoPaintTextHintTo(C: TCanvas);
var
  Size: TSize;
  Pos: TPoint;
begin
  C.Brush.Color:= FColorBG;
  C.Font.Color:= Colors.TextHintFont;
  C.Font.Style:= FTextHintFontStyle;

  Size:= C.TextExtent(FTextHint);
  if FTextHintCenter then
  begin
    Pos:= CenterPoint(FRectMain);
    Dec(Pos.X, Size.cx div 2);
    Dec(Pos.Y, Size.cy div 2);
  end
  else
  begin
    Pos:= FTextOffset;
  end;

  C.TextOut(Pos.X, Pos.Y, FTextHint);
end;


procedure TATSynEdit.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  case Message.CharCode of
    VK_RETURN: Message.Result:= Ord(WantReturns);
    VK_TAB: Message.Result:= Ord(WantTabs);
    VK_LEFT,
    VK_RIGHT,
    VK_UP,
    VK_DOWN: Message.Result:= 1;
    else inherited;
  end;
end;


procedure TATSynEdit.DoPaintStaple(C: TCanvas; const R: TRect; AColor: TColor);
var
  X1, Y1, X2, Y2: integer;
begin
  if FOptStapleStyle=cLineStyleNone then Exit;

  if FOptStapleEdge1=cStapleEdgeAngle then
    CanvasLineEx(C, AColor, FOptStapleStyle, R.Left, R.Top, R.Right, R.Top, false);

  X1:= R.Left;
  Y1:= R.Top;
  X2:= R.Left;
  Y2:= R.Bottom;
  if FOptStapleEdge1=cStapleEdgeNone then
    Inc(Y1, FCharSize.Y);
  if FOptStapleEdge2=cStapleEdgeNone then
    Dec(Y2, FCharSize.Y);

  CanvasLineEx(C, AColor, FOptStapleStyle, X1, Y1, X2, Y2, false);

  if FOptStapleEdge2=cStapleEdgeAngle then
    CanvasLineEx(C, AColor, FOptStapleStyle, R.Left, R.Bottom, R.Right, R.Bottom, true);
end;

procedure TATSynEdit.DoPaintStaples(C: TCanvas; const ARect: TRect;
  ACharSize: TPoint; const AScrollHorz: TATSynScrollInfo);
var
  nLineFrom, nLineTo, nRangeDeepest, nMaxHeight: integer;
  nIndent, nIndentBegin, nIndentEnd: integer;
  Indexes: TATIntArray;
  Range: PATSynRange;
  P1, P2: TPoint;
  RSt: TRect;
  NColor, NColorNormal, NColorActive: TColor;
  i: integer;
begin
  if FOptStapleStyle=cLineStyleNone then Exit;
  if not FFold.HasStaples then Exit;

  nLineFrom:= LineTop;
  nLineTo:= LineBottom;
  nMaxHeight:= FRectMain.Height+2;
  nRangeDeepest:= -1;

  Indexes:= FFold.FindRangesWithStaples(nLineFrom, nLineTo);

  //currently find active range for first caret only
  if FOptStapleHiliteActive then
    if Carets.Count>0 then
      nRangeDeepest:= FFold.FindDeepestRangeContainingLine(Carets[0].PosY, true);

  NColorNormal:= Colors.BlockStaple;
  NColorActive:= Colors.BlockStapleForCaret;
  if NColorActive=clNone then
    NColorActive:= ColorBlend(NColorNormal, FColorFont, FOptStapleHiliteActiveAlpha);

  for i:= 0 to High(Indexes) do
  begin
    Range:= Fold.ItemPtr(Indexes[i]);
    {
    //FindRangesWithStaples does it:
    if not Range^.Staple then Continue;
    if Range^.Folded then Continue;
    }

    if IsLineFolded(Range^.Y, true) then Continue;
    if IsLineFolded(Range^.Y2, true) then Continue;

    P1:= CaretPosToClientPos(Point(0, Range^.Y));
    P2:= CaretPosToClientPos(Point(0, Range^.Y2));
    if (P1.Y<FRectMain.Top) and (Range^.Y>=nLineFrom) then Continue;
    if (P2.Y<FRectMain.Top) and (Range^.Y2>=nLineFrom) then Continue;

    nIndentBegin:= FTabHelper.GetIndentExpanded(Range^.Y, Strings.Lines[Range^.Y]);

    if FOptStapleIndentConsidersEnd then
    begin
      nIndentEnd:= FTabHelper.GetIndentExpanded(Range^.Y2, Strings.Lines[Range^.Y2]);
      nIndent:= Min(nIndentBegin, nIndentEnd);
    end
    else
      nIndent:= nIndentBegin;

    Inc(P1.X, nIndent*ACharSize.X);
    Inc(P2.X, nIndent*ACharSize.X);

    RSt.Left:= P1.X + FOptStapleIndent;
    RSt.Top:= P1.Y;
    RSt.Right:= RSt.Left+ (ACharSize.X * FOptStapleWidthPercent div 100);
    RSt.Bottom:= P2.Y + ACharSize.Y-1;

    if (RSt.Left>=ARect.Left) and
      (RSt.Left<ARect.Right) then
    begin
      //don't use too big coords, some OS'es truncate lines painted with big coords
      RSt.Top:= Max(RSt.Top, -2);
      RSt.Bottom:= Min(RSt.Bottom, nMaxHeight);

      if Indexes[i]=nRangeDeepest then
        NColor:= NColorActive
      else
        NColor:= NColorNormal;

      if Assigned(FOnCalcStaple) then
        FOnCalcStaple(Self, Range^.Y, NIndent, NColor);

      DoPaintStaple(C, RSt, NColor);
    end;
  end;
end;


function TATSynEdit.IsCharWord(ch: Widechar): boolean;
begin
  Result:= ATStringProc.IsCharWord(ch, OptNonWordChars);
end;

function TATSynEdit.GetGaps: TATGaps;
begin
  Result:= Strings.Gaps;
end;

function TATSynEdit.GetLastCommandChangedLines: integer;
begin
  Result:= Strings.LastCommandChangedLines;
end;

procedure TATSynEdit.SetLastCommandChangedLines(AValue: integer);
begin
  Strings.LastCommandChangedLines:= AValue;
end;

procedure TATSynEdit.DoPaintMarkersTo(C: TCanvas);
var
  Mark: TATMarkerItem;
  Pnt: TPoint;
  NMarkSize, NLineW: integer;
  iMark: integer;
  R: TRect;
begin
  if FMarkers=nil then exit;

  NMarkSize:= Max(1, FCharSize.Y * FOptMarkersSize div (100*2));
  NLineW:= NMarkSize;

  for iMark:= 0 to FMarkers.Count-1 do
  begin
    Mark:= FMarkers[iMark];
    if Mark.CoordX<0 then Continue;
    if Mark.CoordY<0 then Continue;

    Pnt.X:= Mark.CoordX;
    Pnt.Y:= Mark.CoordY+FCharSize.Y;

    if PtInRect(FRectMain, Pnt) then
      CanvasPaintTriangleUp(C, Colors.Markers, Pnt, NMarkSize);

    if (Mark.LineLen<>0) and (Mark.CoordY=Mark.CoordY2) then
    begin
      R.Left:= Min(Pnt.X, Mark.CoordX2);
      R.Right:= Max(Pnt.X, Mark.CoordX2)+1;
      R.Bottom:= Pnt.Y+NMarkSize+1;
      R.Top:= R.Bottom-NLineW;

      //avoid painting part of the line over minimap/gutter
      R.Left:= Max(R.Left, FRectMain.Left);
      R.Right:= Min(R.Right, FRectMain.Right);

      C.Brush.Color:= Colors.Markers;
      C.FillRect(R);
    end;
  end;
end;

procedure TATSynEdit.DoPaintGutterPlusMinus(C: TCanvas; AX, AY: integer;
  APlus: boolean; ALineColor: TColor);
begin
  case OptGutterIcons of
    cGutterIconsPlusMinus:
      begin
        CanvasPaintPlusMinus(C,
          ALineColor,
          Colors.GutterFoldBG,
          Point(AX, AY),
          EditorScale(FOptGutterPlusSize),
          APlus);
      end;
    cGutterIconsTriangles:
      begin
        if APlus then
          CanvasPaintTriangleRight(C,
            ALineColor,
            Point(AX, AY),
            EditorScale(FOptGutterPlusSize div 2))
        else
          CanvasPaintTriangleDown(C,
            ALineColor,
            Point(AX, AY),
            EditorScale(FOptGutterPlusSize div 2))
      end;
  end;
end;


procedure TATSynEdit.DoSetMarkedLines(ALine1, ALine2: integer);
begin
  InitMarkedRange;
  FMarkedRange.Clear;
  if (ALine1>=0) and (ALine2>=ALine1) then
  begin
    FMarkedRange.Add(0, ALine1);
    FMarkedRange.Add(0, ALine2);
  end;
  InvalidateHilitingCache;
end;

procedure TATSynEdit.DoGetMarkedLines(out ALine1, ALine2: integer);
begin
  ALine1:= -1;
  ALine2:= -1;
  if Assigned(FMarkedRange) then
    if FMarkedRange.Count=2 then
    begin
      ALine1:= FMarkedRange.Items[0].PosY;
      ALine2:= FMarkedRange.Items[1].PosY;
    end;
end;


procedure TATSynEdit.UpdateLinksAttribs;
var
  AtrObj: TATLinePartClass;
  NLineStart, NLineEnd, NLineLen: integer;
  MatchPos, MatchLen, iLine: integer;
  LinkArrayPtr: PATLinkArray;
  LinkArray: TATLinkArray;
  LinkIndex: integer;
  NRegexRuns: integer;
begin
  if ModeOneLine then
    exit;

  if not OptShowURLs then
  begin
    if Assigned(FAttribs) then
      FAttribs.DeleteWithTag(cUrlMarkerTag);
    exit;
  end;

  NLineStart:= LineTop;
  NLineEnd:= NLineStart+GetVisibleLines;

  InitAttribs;
  FAttribs.DeleteWithTag(cUrlMarkerTag);

  FLinkCache.DeleteDataOutOfRange(NLineStart, NLineEnd);
  NRegexRuns:= 0;

  for iLine:= NLineStart to NLineEnd do
  begin
    if not Strings.IsIndexValid(iLine) then Break;
    NLineLen:= Strings.LinesLen[iLine];

    if NLineLen<FOptMinLineLenToCalcURL then Continue;
    if NLineLen>FOptMaxLineLenToCalcURL then Continue;

    LinkArrayPtr:= FLinkCache.FindData(iLine);
    if LinkArrayPtr=nil then
    begin
      Assert(Assigned(FRegexLinks), 'FRegexLinks not inited');
      FRegexLinks.InputString:= Strings.Lines[iLine];

      LinkIndex:= 0;
      FillChar(LinkArray, SizeOf(LinkArray), 0);
      MatchPos:= 0;
      MatchLen:= 0;
      Inc(NRegexRuns);

      while FRegexLinks.ExecPos(MatchPos+MatchLen+1) do
      begin
        MatchPos:= FRegexLinks.MatchPos[0];
        MatchLen:= FRegexLinks.MatchLen[0];
        LinkArray[LinkIndex].NFrom:= MatchPos;
        LinkArray[LinkIndex].NLen:= MatchLen;
        Inc(LinkIndex);
        if LinkIndex>High(LinkArray) then Break;
        Inc(NRegexRuns);
      end;

      FLinkCache.AddData(iLine, LinkArray);
      LinkArrayPtr:= @LinkArray;
    end;

    for LinkIndex:= 0 to High(LinkArray) do
    begin
      MatchLen:= LinkArrayPtr^[LinkIndex].NLen;
      if MatchLen=0 then Break;
      MatchPos:= LinkArrayPtr^[LinkIndex].NFrom;

      AtrObj:= TATLinePartClass.Create;
      AtrObj.Data.ColorFont:= Colors.Links;
      AtrObj.Data.ColorBG:= clNone;
      AtrObj.Data.ColorBorder:= Colors.Links;
      AtrObj.Data.BorderDown:= cLineStyleSolid;

      FAttribs.Add(
        MatchPos-1,
        iLine,
        cUrlMarkerTag,
        MatchLen,
        0,
        AtrObj
        );
    end;
  end;

  ////debug
  //Application.MainForm.Caption:= 'runs:'+IntToStr(NRegexRuns)+' '+FLinkCache.DebugText;
end;


function TATSynEdit.DoGetLinkAtPos(AX, AY: integer): atString;
var
  MatchPos, MatchLen: integer;
begin
  Result:= '';
  if not Strings.IsIndexValid(AY) then exit;

  Assert(Assigned(FRegexLinks), 'FRegexLinks not inited');
  FRegexLinks.InputString:= Strings.Lines[AY];
  MatchPos:= 0;
  MatchLen:= 0;

  while FRegexLinks.ExecPos(MatchPos+MatchLen+1) do
  begin
    MatchPos:= FRegexLinks.MatchPos[0]-1;
    MatchLen:= FRegexLinks.MatchLen[0];
    if MatchPos>AX then
      Break;
    if (MatchPos<=AX) and (MatchPos+MatchLen>AX) then
      exit(FRegexLinks.Match[0]);
  end;
end;


procedure TATSynEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:=
    FOptMouseDragDrop and
    (not ModeReadOnly) and
    (not ModeOneLine) and
    (Source is TATSynEdit) and
    ((Source as TATSynEdit).TextSelected<>'');
end;

procedure TATSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  SText: atString;
  Pnt: TPoint;
  Details: TATPosDetails;
begin
  if not (Source is TATSynEdit) then exit;
  if (Source=Self) then exit;
  SText:= (Source as TATSynedit).TextSelected;
  if SText='' then exit;

  Pnt:= ClientPosToCaretPos(Point(X, Y), Details);
  if Strings.IsIndexValid(Pnt.Y) then
  begin
    DoCaretSingle(Pnt.X, Pnt.Y);
    DoCommand(cCommand_TextInsert, SText);
    if OptMouseDragDropFocusesTargetEditor then
      SetFocus;

    //Ctrl not pressed: delete block from src
    if FOptMouseDragDropCopyingWithState in GetKeyShiftState then
      (Source as TATSynedit).DoCommand(cCommand_TextDeleteSelection);
  end;
end;

procedure TATSynEdit.OnNewScrollbarVertChanged(Sender: TObject);
var
  Msg: TLMVScroll;
begin
  if FScrollbarLock then exit;
  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  Msg.ScrollCode:= SB_THUMBPOSITION;
  Msg.Pos:= FScrollbarVert.Position;
  WMVScroll(Msg);

  //show scroll hint
  DoHintShow;
end;

procedure TATSynEdit.OnNewScrollbarHorzChanged(Sender: TObject);
var
  Msg: TLMHScroll;
begin
  if FScrollbarLock then exit;
  FillChar({%H-}Msg, SizeOf(Msg), 0);
  Msg.ScrollCode:= SB_THUMBPOSITION;
  Msg.Pos:= FScrollbarHorz.Position;
  WMHScroll(Msg);
end;

procedure TATSynEdit.DoCaretsOnChanged(Sender: TObject);
begin
  if Strings.ModifiedRecent then
    if Assigned(FCarets) and (FCarets.Count>0) then
      Strings.DoSaveLastEditPos;
      //it clears ModifiedRecent
end;

procedure TATSynEdit.TimerIdleTick(Sender: TObject);
begin
  FTimerIdle.Enabled:= false;
  if Assigned(FOnIdle) then
    FOnIdle(Self);
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorIdle(Self);
end;

procedure TATSynEdit.DoStringsOnChange(Sender: TObject; AChange: TATLineChangeKind; ALine,
  AItemCount: integer);
begin
  Fold.Update(AChange, ALine, AItemCount);

  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorChangeEx(Self, AChange, ALine, AItemCount);
end;

procedure TATSynEdit.DoStringsOnProgress(Sender: TObject);
begin
  Invalidate;
  Application.ProcessMessages;
  //auto paints "wait... N%"
end;

procedure TATSynEdit.DoHotspotsExit;
begin
  if FLastHotspot>=0 then
  begin
    if Assigned(FOnHotspotExit) then
      FOnHotspotExit(Self, FLastHotspot);
    FLastHotspot:= -1;
  end;
end;


procedure TATSynEdit.MinimapTooltipPaint(Sender: TObject);
var
  C: TCanvas;
  RectAll: TRect;
  Pnt: TPoint;
  NWrapIndex, NLineCenter, NLineTop, NLineBottom: integer;
begin
  C:= FMinimapTooltip.Canvas;
  RectAll:= Rect(0, 0, FMinimapTooltip.Width, FMinimapTooltip.Height);
  Pnt:= ScreenToClient(Mouse.CursorPos);

  C.Pen.Color:= Colors.MinimapTooltipBorder;
  C.Brush.Color:= Colors.MinimapTooltipBG;
  C.Rectangle(RectAll);

  NWrapIndex:= GetMinimap_ClickedPosToWrapIndex(Pnt.Y);
  FMinimapTooltip.Visible:= FMinimapTooltipVisible and (NWrapIndex>=0);
  if NWrapIndex<0 then exit;
  NLineCenter:= FWrapInfo[NWrapIndex].NLineIndex;
  NLineTop:= Max(0, NLineCenter - FMinimapTooltipLinesCount div 2);
  NLineBottom:= Min(NLineTop + FMinimapTooltipLinesCount-1, Strings.Count-1);

  DoPaintTextFragmentTo(C, RectAll,
    NLineTop,
    NLineBottom,
    true,
    Colors.MinimapTooltipBG,
    Colors.MinimapTooltipBorder
    );
end;

procedure TATSynEdit.DoPaintTextFragmentTo(C: TCanvas;
  const ARect: TRect;
  ALineFrom, ALineTo: integer;
  AConsiderWrapInfo: boolean;
  AColorBG, AColorBorder: TColor);
var
  NOutputStrWidth: integer;
  NLine, NWrapIndex: integer;
  NColorAfter: TColor;
  WrapItem: TATWrapItem;
  TextOutProps: TATCanvasTextOutProps;
begin
  C.Brush.Color:= AColorBG;
  C.FillRect(ARect);

  FillChar(TextOutProps{%H-}, SizeOf(TextOutProps), 0);

  TextOutProps.Editor:= Self;
  TextOutProps.SuperFast:= false;
  TextOutProps.TabHelper:= FTabHelper;
  TextOutProps.CharSize:= FCharSize;
  TextOutProps.CharsSkipped:= 0;
  TextOutProps.DrawEvent:= nil;
  TextOutProps.ControlWidth:= ARect.Width;
  TextOutProps.TextOffsetFromLine:= FOptTextOffsetFromLine;

  TextOutProps.ShowUnprinted:= FUnprintedVisible and FUnprintedSpaces;
  TextOutProps.ShowUnprintedSpacesTrailing:= FUnprintedSpacesTrailing;
  TextOutProps.ShowUnprintedSpacesBothEnds:= FUnprintedSpacesBothEnds;
  TextOutProps.ShowUnprintedSpacesOnlyInSelection:= FUnprintedSpacesOnlyInSelection;
  TextOutProps.DetectIsPosSelected:= @IsPosSelected;

  TextOutProps.ShowFontLigatures:= FOptShowFontLigatures;
  TextOutProps.ColorNormalFont:= Colors.TextFont;
  TextOutProps.ColorUnprintedFont:= Colors.UnprintedFont;
  TextOutProps.ColorUnprintedHexFont:= Colors.UnprintedHexFont;

  TextOutProps.FontNormal_Name:= Font.Name;
  TextOutProps.FontNormal_Size:= DoScaleFont(Font.Size);

  TextOutProps.FontItalic_Name:= FontItalic.Name;
  TextOutProps.FontItalic_Size:= DoScaleFont(FontItalic.Size);

  TextOutProps.FontBold_Name:= FontBold.Name;
  TextOutProps.FontBold_Size:= DoScaleFont(FontBold.Size);

  TextOutProps.FontBoldItalic_Name:= FontBoldItalic.Name;
  TextOutProps.FontBoldItalic_Size:= DoScaleFont(FontBoldItalic.Size);

  if AConsiderWrapInfo then
    NWrapIndex:= WrapInfo.FindIndexOfCaretPos(Point(0, ALineFrom));

  for NLine:= ALineFrom to ALineTo do
  begin
    NColorAfter:= clNone;
    if AConsiderWrapInfo then
    begin
      if NWrapIndex<0 then Break;
      WrapItem:= WrapInfo[NWrapIndex];
      Inc(NWrapIndex);
    end
    else
    begin
      if not Strings.IsIndexValid(NLine) then Break;
      FillChar(WrapItem, SizeOf(WrapItem), 0);
      WrapItem.NLineIndex:= NLine;
      WrapItem.NCharIndex:= 1;
      WrapItem.NLength:= Strings.LinesLen[NLine];
    end;

    DoCalcLineHilite(WrapItem, FLineParts{%H-},
      0, cMaxCharsForOutput,
      AColorBG,
      false,
      NColorAfter,
      true);

    TextOutProps.LineIndex:= WrapItem.NLineIndex;
    TextOutProps.CharIndexInLine:= WrapItem.NCharIndex;
    CanvasTextOut(C,
      cSizeIndentTooltipX,
      cSizeIndentTooltipY + FCharSize.Y*(NLine-ALineFrom),
      Strings.LineSub(
        WrapItem.NLineIndex,
        WrapItem.NCharIndex,
        GetVisibleColumns),
      @FLineParts,
      NOutputStrWidth,
      TextOutProps
      )
   end;

  C.Brush.Color:= AColorBorder;
  C.FrameRect(ARect);
end;


procedure TATSynEdit.UpdateMinimapTooltip;
var
  Pnt: TPoint;
begin
  if FMinimapTooltip=nil then exit;
  if not FMinimapTooltip.Visible then exit;
  Pnt:= ScreenToClient(Mouse.CursorPos);

  FMinimapTooltip.Width:= FRectMain.Width * FMinimapTooltipWidthPercents div 100;
  if FMinimapAtLeft then
    FMinimapTooltip.Left:= FRectMinimap.Right + 1
  else
    FMinimapTooltip.Left:= FRectMinimap.Left - FMinimapTooltip.Width - 1;
  FMinimapTooltip.Height:= FMinimapTooltipLinesCount*FCharSize.Y + 2;
  FMinimapTooltip.Top:= Max(0, Min(ClientHeight-FMinimapTooltip.Height,
    Pnt.Y - Trunc(FCharSize.Y*FMinimapTooltipLinesCount/2)
    ));

  FMinimapTooltip.Invalidate;
end;


procedure TATSynEdit.UpdateFoldedMarkTooltip;
begin
  if (not FFoldTooltipVisible) or not FFoldedMarkCurrent.IsInited then
  begin
    if Assigned(FFoldedMarkTooltip) then
      FFoldedMarkTooltip.Hide;
    exit
  end;

  InitFoldedMarkTooltip;

  FFoldedMarkTooltip.Width:= FRectMain.Width * FFoldTooltipWidthPercents div 100;
  FFoldedMarkTooltip.Height:= (FFoldedMarkCurrent.LineTo-FFoldedMarkCurrent.LineFrom+1) * FCharSize.Y + 2;
  FFoldedMarkTooltip.Left:= Min(
    FRectMain.Right - FFoldedMarkTooltip.Width - 1,
    FFoldedMarkCurrent.Coord.Left);
  FFoldedMarkTooltip.Top:=
    FFoldedMarkCurrent.Coord.Top + FCharSize.Y;

  //no space for on bottom? show on top
  if FFoldedMarkTooltip.Top + FFoldedMarkTooltip.Height > FRectMain.Bottom then
    if FFoldedMarkCurrent.Coord.Top - FFoldedMarkTooltip.Height >= FRectMain.Top then
      FFoldedMarkTooltip.Top:= FFoldedMarkCurrent.Coord.Top - FFoldedMarkTooltip.Height;

  FFoldedMarkTooltip.Show;
  FFoldedMarkTooltip.Invalidate;
end;

procedure TATSynEdit.FoldedMarkTooltipPaint(Sender: TObject);
begin
  if FFoldedMarkCurrent.IsInited then
    DoPaintTextFragmentTo(
      FFoldedMarkTooltip.Canvas,
      Rect(0, 0, FFoldedMarkTooltip.Width, FFoldedMarkTooltip.Height),
      FFoldedMarkCurrent.LineFrom,
      FFoldedMarkCurrent.LineTo,
      false, //to paint fully folded lines, must be False
      Colors.MinimapTooltipBG,
      Colors.MinimapTooltipBorder
      );
end;

procedure TATSynEdit.FoldedMarkMouseEnter(Sender: TObject);
begin
  if Assigned(FFoldedMarkTooltip) then
    FFoldedMarkTooltip.Hide;
end;

function TATSynEdit.DoGetFoldedMarkLinesCount(ALine: integer): integer;
var
  i: integer;
begin
  Result:= 1;
  for i:= ALine+1 to Min(ALine+FFoldTooltipLineCount-1, Strings.Count-1) do
    if Strings.LinesHidden[i, FEditorIndex] then
      Inc(Result)
    else
      Break;
end;


function TATSynEdit.DoGetGapRect(AIndex: integer; out ARect: TRect): boolean;
var
  GapItem: TATGapItem;
  Pnt: TPoint;
begin
  Result:= false;
  ARect:= Rect(0, 0, 0, 0);

  if not ((AIndex>=0) and (AIndex<Gaps.Count)) then exit;

  GapItem:= Gaps.Items[AIndex];
  Pnt:= CaretPosToClientPos(Point(0, GapItem.LineIndex+1));
  if Pnt.Y<GapItem.Size then exit;

  ARect.Left:= FRectMain.Left;
  ARect.Right:= FRectMain.Right;
  ARect.Top:= Pnt.Y - GapItem.Size;
  ARect.Bottom:= Pnt.Y;
  Result:= true;
end;

procedure TATSynEdit.SetFontItalic(AValue: TFont);
begin
  FFontItalic.Assign(AValue);
end;

procedure TATSynEdit.SetFontBold(AValue: TFont);
begin
  FFontBold.Assign(AValue);
end;

procedure TATSynEdit.SetFontBoldItalic(AValue: TFont);
begin
  FFontBoldItalic.Assign(AValue);
end;

procedure TATSynEdit.UpdateTabHelper;
begin
  FTabHelper.TabSpaces:= OptTabSpaces;
  FTabHelper.TabSize:= OptTabSize;
  FTabHelper.IndentSize:= OptIndentSize;
  FTabHelper.SenderObj:= Self;
  FTabHelper.OnCalcTabSize:= FOnCalcTabSize;
  FTabHelper.OnCalcLineLen:= @DoCalcLineLen;
end;

{$ifdef debug_fps}
procedure TATSynEdit.DoPaintFPS(C: TCanvas);
var
  NFps: integer;
  //NFpsMap: integer;
  S: string;
begin
  if FTickAll<1 then
    FTickAll:= 1;
  if FTickMinimap<1 then
    FTickMinimap:= 1;
  NFps:= 1000 div FTickAll div 5 * 5;
  //NFpsMap:= 1000 div FTickMinimap;

  C.Font.Name:= 'Arial';
  C.Font.Color:= clRed;
  C.Font.Size:= 8;
  C.Brush.Color:= clCream;

  S:= Format('#%d, fps %d', [FPaintCounter, NFps]);
  CanvasTextOutSimplest(C, FClientW-100, 5, S);

  S:= Format('miss %d+%d', [FMissMain-FMissMinimap, FMissMinimap]);
  CanvasTextOutSimplest(C, FClientW-100, 20, S);
end;
{$else}
procedure TATSynEdit.DoPaintFPS(C: TCanvas);
begin
end;
{$endif}


function TATSynEdit.GetEncodingName: string;
var
  Str: TATStrings;
begin
  Str:= Strings;
  case Str.Encoding of
    cEncAnsi:
      begin
        Result:= cEncConvNames[Str.EncodingCodepage];
      end;
    cEncUTF8:
      begin
        if Str.SaveSignUtf8 then
          Result:= cEncNameUtf8_WithBom
        else
          Result:= cEncNameUtf8_NoBom;
      end;
    cEncWideLE:
      begin
        if Str.SaveSignWide then
          Result:= cEncNameUtf16LE_WithBom
        else
          Result:= cEncNameUtf16LE_NoBom;
      end;
    cEncWideBE:
      begin
        if Str.SaveSignWide then
          Result:= cEncNameUtf16BE_WithBom
        else
          Result:= cEncNameUtf16BE_NoBom;
      end;
    cEnc32LE:
      begin
        if Str.SaveSignWide then
          Result:= cEncNameUtf32LE_WithBom
        else
          Result:= cEncNameUtf32LE_NoBom;
      end;
    cEnc32BE:
      begin
        if Str.SaveSignWide then
          Result:= cEncNameUtf32BE_WithBom
        else
          Result:= cEncNameUtf32BE_NoBom;
      end;
  end;
end;

procedure TATSynEdit.SetEncodingName(const AName: string);
var
  Str: TATStrings;
begin
  if AName='' then exit;
  if SameText(AName, GetEncodingName) then exit;
  Str:= Strings;

  if SameText(AName, cEncNameUtf8_WithBom) then begin Str.Encoding:= cEncUTF8; Str.SaveSignUtf8:= true; end else
  if SameText(AName, cEncNameUtf8_NoBom) then begin Str.Encoding:= cEncUTF8; Str.SaveSignUtf8:= false; end else
  if SameText(AName, cEncNameUtf16LE_WithBom) then begin Str.Encoding:= cEncWideLE; Str.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf16LE_NoBom) then begin Str.Encoding:= cEncWideLE; Str.SaveSignWide:= false; end else
  if SameText(AName, cEncNameUtf16BE_WithBom) then begin Str.Encoding:= cEncWideBE; Str.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf16BE_NoBom) then begin Str.Encoding:= cEncWideBE; Str.SaveSignWide:= false; end else
  if SameText(AName, cEncNameUtf32LE_WithBom) then begin Str.Encoding:= cEnc32LE; Str.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf32LE_NoBom) then begin Str.Encoding:= cEnc32LE; Str.SaveSignWide:= false; end else
  if SameText(AName, cEncNameUtf32BE_WithBom) then begin Str.Encoding:= cEnc32BE; Str.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf32BE_NoBom) then begin Str.Encoding:= cEnc32BE; Str.SaveSignWide:= false; end else
  begin
    Str.Encoding:= cEncAnsi;
    Str.EncodingCodepage:= EncConvFindEncoding(LowerCase(AName));
  end;
end;

procedure TATSynEdit.TextInsertAtCarets(const AText: atString; AKeepCaret,
  AOvrMode, ASelectThen: boolean);
var
  Res: TATCommandResults;
begin
  Res:= DoCommand_TextInsertAtCarets(AText, AKeepCaret, AOvrMode, ASelectThen);
  DoCommandResults(0, Res);
end;

procedure TATSynEdit.DoCaretsFixForSurrogatePairs(AMoveRight: boolean);
//this is used to prevert caret stopping
//a) inside surrogate pair (2 codes which make one glyph)
//b) on accent (combining) char; we skip _all_ chars (Unicode allows several accent chars)
var
  Caret: TATCaretItem;
  ch: WideChar;
  i: integer;
begin
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if Caret.PosX<=0 then Continue;
    if not Strings.IsIndexValid(Caret.PosY) then Continue;
    ch:= Strings.LineCharAt(Caret.PosY, Caret.PosX+1);
    if ch=#0 then Continue;

    if IsCharSurrogateLow(ch) then
    begin
      Caret.PosX:= Caret.PosX+BoolToPlusMinusOne[AMoveRight];
      Continue;
    end;

    while IsCharAccent(ch) do
    begin
      Caret.PosX:= Caret.PosX+BoolToPlusMinusOne[AMoveRight];
      ch:= Strings.LineCharAt(Caret.PosY, Caret.PosX+1);
    end;
  end;
end;

function TATSynEdit.RectMicromapMark(AColumn, ALineFrom, ALineTo: integer): TRect;
const
  cMinHeight = 2;
var
  H: integer;
begin
  if FMicromap.IsIndexValid(AColumn) then
  begin
    H:= FRectMicromap.Height;

    if ALineFrom>=0 then
      Result.Top:= FRectMicromap.Top + Int64(ALineFrom) * H div FMicromapScaleDiv
    else
      Result.Top:= FRectMicromap.Top;

    if ALineTo>=0 then
      Result.Bottom:= Max(Result.Top+cMinHeight,
                 FRectMicromap.Top + Int64(ALineTo+1) * H div FMicromapScaleDiv)
    else
      Result.Bottom:= FRectMicromap.Bottom;

    with FMicromap.Columns[AColumn] do
    begin
      Result.Left:= NLeft;
      Result.Right:= NRight;
    end;
  end
  else
    Result:= cRectEmpty;
end;

procedure TATSynEdit.SetShowOsBarVert(AValue: boolean);
begin
  if FShowOsBarVert=AValue then Exit;
  FShowOsBarVert:= AValue;
  ShowScrollBar(Handle, SB_Vert, AValue);
end;

procedure TATSynEdit.SetShowOsBarHorz(AValue: boolean);
begin
  if FShowOsBarHorz=AValue then Exit;
  FShowOsBarHorz:= AValue;
  ShowScrollBar(Handle, SB_Horz, AValue);
end;

procedure TATSynEdit.InitMinimapTooltip;
begin
  if FMinimapTooltip=nil then
  begin
    FMinimapTooltip:= TPanel.Create(Self);
    FMinimapTooltip.Hide;
    FMinimapTooltip.Width:= 15;
    FMinimapTooltip.Height:= 15;
    FMinimapTooltip.Parent:= Self;
    FMinimapTooltip.BorderStyle:= bsNone;
    FMinimapTooltip.OnPaint:= @MinimapTooltipPaint;
  end;
end;

procedure TATSynEdit.InitFoldedMarkList;
begin
  if FFoldedMarkList=nil then
    FFoldedMarkList:= TATFoldedMarks.Create;
end;

procedure TATSynEdit.InitFoldedMarkTooltip;
begin
  if FFoldedMarkTooltip=nil then
  begin
    FFoldedMarkTooltip:= TPanel.Create(Self);
    FFoldedMarkTooltip.Hide;
    FFoldedMarkTooltip.Width:= 15;
    FFoldedMarkTooltip.Height:= 15;
    FFoldedMarkTooltip.Parent:= Self;
    FFoldedMarkTooltip.BorderStyle:= bsNone;
    FFoldedMarkTooltip.OnPaint:= @FoldedMarkTooltipPaint;
    FFoldedMarkTooltip.OnMouseEnter:=@FoldedMarkMouseEnter;
  end;
end;

procedure TATSynEdit.InitAttribs;
begin
  if FAttribs=nil then
  begin
    FAttribs:= TATMarkers.Create;
    FAttribs.Sorted:= true;
    FAttribs.Duplicates:= true; //CudaText plugins need it
  end;
end;

procedure TATSynEdit.InitMarkers;
begin
  if FMarkers=nil then
  begin
    FMarkers:= TATMarkers.Create;
    FMarkers.Sorted:= false;
  end;
end;

procedure TATSynEdit.InitHotspots;
begin
  if FHotspots=nil then
    FHotspots:= TATHotspots.Create;
end;

procedure TATSynEdit.InitDimRanges;
begin
  if FDimRanges=nil then
    FDimRanges:= TATDimRanges.Create;
end;

procedure TATSynEdit.InitGutterDecor;
begin
  if FGutterDecor=nil then
    FGutterDecor:= TATGutterDecor.Create;
end;

function TATSynEdit.GetAttribs: TATMarkers;
begin
  InitAttribs;
  Result:= FAttribs;
end;

function TATSynEdit.DoCalcLineLen(ALineIndex: integer): integer;
begin
  Result:= Strings.LinesLen[ALineIndex];
end;

function TATSynEdit.GetDimRanges: TATDimRanges;
begin
  InitDimRanges;
  Result:= FDimRanges;
end;

function TATSynEdit.GetHotspots: TATHotspots;
begin
  InitHotspots;
  Result:= FHotspots;
end;

function TATSynEdit.GetMarkers: TATMarkers;
begin
  InitMarkers;
  Result:= FMarkers;
end;

function TATSynEdit.GetGutterDecor: TATGutterDecor;
begin
  InitGutterDecor;
  Result:= FGutterDecor;
end;

procedure TATSynEdit.SetOptShowURLsRegex(const AValue: string);
begin
  if FOptShowURLsRegex=AValue then Exit;
  FOptShowURLsRegex:= AValue;
  UpdateLinksRegexObject;
end;

procedure TATSynEdit.InitMarkedRange;
begin
  if FMarkedRange=nil then
  begin
    FMarkedRange:= TATMarkers.Create;
    FMarkedRange.Sorted:= true;
  end;
end;

function TATSynEdit.DoScaleFont(AValue: integer): integer;
begin
  if FOptScaleFont>0 then
    Result:= AValue * FOptScaleFont div 100
  else
    Result:= EditorScaleFont(AValue);
end;

function TATSynEdit.UpdateLinksRegexObject: boolean;
begin
  Result:= false;

  if FRegexLinks=nil then
    FRegexLinks:= TRegExpr.Create;

  try
    //FRegexLinks.UseUnicodeWordDetection:= false; //faster for links
    FRegexLinks.ModifierS:= false;
    FRegexLinks.ModifierM:= false; //M not needed
    FRegexLinks.ModifierI:= false; //I not needed to find links
    FRegexLinks.Expression:= FOptShowURLsRegex;
    FRegexLinks.Compile;

    Result:= true;
  except
    exit;
  end;
end;

{$I atsynedit_carets.inc}
{$I atsynedit_hilite.inc}
{$I atsynedit_sel.inc}
{$I atsynedit_fold.inc}
{$I atsynedit_debug.inc}

{$R res/nicescroll1.res}
{$R res/nicescroll2.res}
{$R res/foldbar.res}
{$R res/editor_hourglass.res}

{$I atsynedit_cmd_handler.inc}
{$I atsynedit_cmd_keys.inc}
{$I atsynedit_cmd_sel.inc}
{$I atsynedit_cmd_editing.inc}
{$I atsynedit_cmd_clipboard.inc}
{$I atsynedit_cmd_misc.inc}
{$I atsynedit_cmd_bookmark.inc}
{$I atsynedit_cmd_markers.inc}


initialization
  ATClipboardColumnFormat:= RegisterClipboardFormat('Application/X-ATSynEdit-Block');

  RegExprModifierS:= False;
  RegExprModifierM:= True;
  RegExprUsePairedBreak:= False;
  RegExprReplaceLineBreak:= #10;

finalization
  FreeEditorResources;

end.

