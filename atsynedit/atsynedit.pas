{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}

{$I atsynedit_defines.inc}

unit ATSynEdit;

interface

uses
  {$ifdef Windows}
  Windows,
  ATSynEdit_Adapter_WindowsIME,
  {$endif}
  {$ifdef LCLGTK2}
  ATSynEdit_Adapter_gtk2IME,
  {$endif}
  Messages, //for Win32 and macOS
  InterfaceBase,
  Classes, SysUtils, Graphics,
  Controls, ExtCtrls, Menus, Forms, Clipbrd,
  syncobjs, gdeque,
  LMessages, LCLType, LCLVersion,
  LazUTF8,
  EncConv,
  BGRABitmap,
  BGRABitmapTypes,
  ATStringProc,
  ATStringProc_Separator,
  ATStrings,
  ATStringProc_WordJump,
  ATCanvasPrimitives,
  ATSynEdit_Globals,
  ATSynEdit_CharSizer,
  {$ifdef USE_FPC_REGEXPR}
  RegExpr,
  {$else}
  ATSynEdit_RegExpr,
  {$endif}
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
  ATSynEdit_LinkCache,
  ATSynEdit_FGL,
  ATSynEdit_ClipRecents,
  ATScrollBar;

type
  TATPoint64 = record
    X, Y: Int64
  end;

  TATRect64 = record
    Left, Top, Right, Bottom: Int64;
  end;

type
  TATCommandInvoke = (
    Internal,
    InternalIME,
    Hotkey,
    HotkeyChar,
    MenuContext,
    MenuMain,
    MenuAPI,
    AppInternal,
    AppPalette,
    AppToolbar,
    AppSidebar,
    AppCharMap,
    AppDragDrop,
    AppAPI
    );

const
  cEditorCommandInvoke: array[TATCommandInvoke] of string = (
    'int',
    'int_ime',
    'key',
    'key_char',
    'menu_ctx',
    'menu_main',
    'menu_api',
    'app_int',
    'app_pal',
    'app_toolbar',
    'app_sidebar',
    'app_charmap',
    'app_dragdrop',
    'app_api'
    );

type
  TATCommandLogItem = record
  public
    ItemInvoke: TATCommandInvoke;
    ItemCode: integer;
    //don't use 'string' here! it gives crashes on freeing of editor objects
    ItemText: string[220];
  end;

  { TATCommandLog }

  TATCommandLog = class(specialize TDeque<TATCommandLogItem>)
  public
    MaxCount: integer;
    constructor Create;
    procedure Add(ACode: integer; AInvoke: TATCommandInvoke; const AText: string);
  end;

  TATEditorWheelRecordKind = (Vert, Horz, Zoom);
  TATEditorWheelRecord = record
    Kind: TATEditorWheelRecordKind;
    Delta: integer;
  end;

type
  TATTokenKind = (
    Other,
    Comment,
    Str
    );

  TATEditorScrollbarStyle = (
    Hide,
    Show,
    Auto
    );

  TATEditorMiddleClickAction = (
    None,
    Scrolling,
    Paste,
    GotoDefinition
    );

  TATEditorPosDetails = record
    EndOfWrappedLine: boolean; //coordinate was after the end of wrapped line-part (but not after the final line-part)
    EndOfPartiallyFoldedLine: boolean; //coordinate was after beginning of folded-block, for partially folded line
    BelowAllText: boolean; //coordinate was below all existing lines (so the ending text pos was returned)
    OnGapItem: TATGapItem; //if coordinate is on a gap, here it TATGapItem object
    OnGapPos: TPoint; //if coordinate is on a gap, here are gap-related coordinates
  end;

  TATEditorDoubleClickAction = (
    None,
    SelectWordChars,
    SelectAnyChars,
    SelectEntireLine
    );

  TATEditorMouseAction = (
    None,
    ClickSimple,
    ClickRight,
    ClickAndSelNormalBlock,
    ClickAndSelVerticalBlock,
    ClickMiddle,
    MakeCaret,
    MakeCaretsColumn
    );

  TATEditorMouseActionRecord = record
    MouseState: TShiftState;
    MouseActionId: TATEditorMouseAction;
  end;

  TATEditorMouseActions = array of TATEditorMouseActionRecord;

  TATFoldBarState = (
    None,
    SBegin,
    SEnd,
    SMiddle
    );

  TATFoldBarProps = record
    State: TATFoldBarState;
    IsPlus: boolean;
    IsLineUp: boolean;
    IsLineDown: boolean;
    HiliteLines: boolean;
  end;

  TATFoldBarPropsArray = array of TATFoldBarProps;

  TATEditorDirection = (
    None,
    Left,
    Right,
    Up,
    Down
    );

  TATEditorRulerNumeration = (
    Num_0_10_20,
    Num_1_11_21,
    Num_1_10_20
    );

  TATEditorSelectColumnDirection = (
    Left,
    Right,
    Up,
    Down,
    PageUp,
    PageDown
    );

  TATEditorScrollbarsArrowsKind = (
    Normal,
    Hidden,
    Above,
    Below,
    Corner
    );

  TATEditorCaseConvert = (
    Lower,
    Upper,
    Title,
    Invert,
    Sentence
    );

  TATEditorActionIfFolded = (
    Ignore,
    Unfold,
    DoExit
    );

  TATCommandResult = (
    Text,             //Text was changed.
    FoldChange,       //Folding range(s) were changed or folded/unfolded.
    CaretAny,         //Caret(s) pos/selection was changed. Don't scroll to caret.
    CaretLeft,        //Caret(s) pos/selection was changed. Scroll to the most left caret.
    CaretTop,         //Caret(s) pos/selection was changed. Scroll to the first caret.
    CaretRight,       //Caret(s) pos/selection was changed. Scroll to the most right caret.
    CaretBottom,      //Caret(s) pos/selection was changed. Scroll to the last caret.
    CaretLazy,  //Additional to CaretLeft/CaretRight/CaretTop/CaretBottom, scrolls only if no carets are left in visible area.
    KeepColumnSel,    //Restore previous column selection, if command changed it.
    Scroll,           //Some scrolling was made.
    UndoRedo,         //Undo or Redo action was made.
    State             //Some properties of editor were changed (e.g. word-wrap state).
    );
  TATCommandResults = set of TATCommandResult;

  TATEditorGapCoordAction = (
    Ignore,
    ToLineEnd,
    MoveDown
    );

  TATEditorGutterIcons = (
    PlusMinus,
    Triangles
    );

  TATEditorPasteCaret = (
    NoChange,
    LeftBottom,
    RightBottom,
    RightTop,
    ColumnLeft,
    ColumnRight
    );

  TATEditorFoldStyle = (
    HereWithDots, //show [...] from fold-pos
    HereWithTruncatedText, //show truncated line instead of [...]
    FromEndOfLine, //looks like Lazarus: show [...] after line, but it's bad when 2 blocks start at the same line
    FromEndOfLineAlways, //always show [...] after line end
    FromNextLine //don't show [...], show dashed underline "- - - -"
    );

const
  cEditorFoldStylesUnfoldOnClick = [
    TATEditorFoldStyle.HereWithDots,
    TATEditorFoldStyle.HereWithTruncatedText,
    TATEditorFoldStyle.FromEndOfLine
    ];

type
  TATEditorFoldRangeCommand = (
    Fold,
    Unfold,
    Toggle
    );

  TATEditorStapleEdge = (
    None,
    Angle,
    Line
    );

type
  TATEditorAutoIndentKind = (
    AsPrevLine,
    SpacesOnly,
    TabsAndSpaces,
    TabsOnly,
    ToOpeningBracket
    );

  TATEditorPageDownSize = (
    Full,
    FullMinus1,
    Half
    );

  TATEditorWrapMode = (
    ModeOff,
    ModeOn,
    AtWindowOrMargin
    );

  TATEditorNumbersStyle = (
    All,
    None,
    Each10th,
    Each5th,
    Relative
    );

  TATEditorInternalFlag = (
    Bitmap, //flag "bitmap should be repainted"
    ScrolledHorz, //flag "horizontal scroll _position_ is changed"
    RepaintNeeded, //last paint changes some state, so repainting is needed
    ScrollEventNeeded, //last paint detected that OnScroll event must be fired
    Resize
    );
  TATEditorInternalFlags = set of TATEditorInternalFlag;

  TATEditorModifiedOption = (
    WordWrap,
    MinimapVisible,
    MicromapVisible,
    RulerVisible,
    GutterBookmarks,
    GutterNumbers,
    GutterFolding,
    GutterLineStates,
    LastLineOnTop,
    ReadOnly,
    ReadOnlyIsDetected,
    CaretShapeNormal,
    CaretShapeOverwrite,
    CaretShapeReadonly,
    UnprintedVisible,
    UnprintedSpaces,
    UnprintedTrailingOnly,
    UnprintedEnds,
    UnprintedEndDetails,
    UnprintedWraps,
    SavingTrimSpaces,
    SavingTrimFinalEmptyLines,
    SavingForceFinalEol
    );
  TATEditorModifiedOptions = set of TATEditorModifiedOption;

type
  { TATEditorScrollInfo }

  TATEditorScrollInfo = record
  private
    NPosInternal: Int64;
    procedure SetNPos(const AValue: Int64);
  public
    Vertical: boolean;
    NMax: Int64;
    NPage: Int64;
    NPosLast: Int64;
    NPixelOffset: Int64;
    CharSizeScaled: Int64; //char width/height, multiplied by ATEditorCharXScale
    SmoothMax: Int64;
    SmoothPage: Int64;
    SmoothPos: Int64;
    SmoothPosLast: Int64;
    property NPos: Int64 //property is only for debugging
      read NPosInternal
      write NPosInternal;
      //write SetNPos;
    procedure Clear;
    procedure SetZero; inline;
    procedure SetLast; inline;
    function TopGapVisible: boolean; inline;
    function TotalOffset: Int64;
    class operator =(const A, B: TATEditorScrollInfo): boolean;
  end;

type
  { TATCaretShape }

  TATCaretShape = class
  public
    //Value>=0: in pixels
    //Value<0: in percents
    //Value<-100: caret is bigger than cell and overlaps nearest cells
    Width: integer;
    Height: integer;
    EmptyInside: boolean;
    function IsNarrow: boolean;
    procedure Assign(Obj: TATCaretShape);
  end;

type
  { TMinimapThread }

  TATMinimapThread = class(TThread)
  public
    Editor: TObject;
  protected
    procedure Execute; override;
  end;

var
  cRectEmpty: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

type
  TATSynEditClickEvent = procedure(Sender: TObject; var AHandled: boolean) of object;
  TATSynEditClickMoveCaretEvent = procedure(Sender: TObject; APrevPnt, ANewPnt: TPoint) of object;
  TATSynEditClickGapEvent = procedure(Sender: TObject; AGapItem: TATGapItem; APos: TPoint) of object;
  TATSynEditCommandEvent = procedure(Sender: TObject; ACommand: integer; AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean) of object;
  TATSynEditCommandAfterEvent = procedure(Sender: TObject; ACommand: integer; const AText: string) of object;
  TATSynEditClickGutterEvent = procedure(Sender: TObject; ABand: integer; ALineNum: integer; var AHandled: boolean) of object;
  TATSynEditClickMicromapEvent = procedure(Sender: TObject; AX, AY: integer) of object;
  TATSynEditClickLinkEvent = procedure(Sender: TObject; const ALink: string) of object;
  TATSynEditChangeDetailedEvent = procedure(Sender: TObject; APos, APosEnd, AShift, APosAfter: TPoint) of object;
  TATSynEditDrawBookmarkEvent = procedure(Sender: TObject; C: TCanvas; ALineIndex, ABookmarkIndex: integer; const ARect: TRect; var AHandled: boolean) of object;
  TATSynEditDrawRectEvent = procedure(Sender: TObject; C: TCanvas; const ARect: TRect) of object;
  TATSynEditDrawRulerEvent = procedure(Sender: TObject; C: TCanvas; const ARect: TRect; var AHandled: boolean) of object;
  TATSynEditDrawGapEvent = procedure(Sender: TObject; C: TCanvas; const ARect: TRect; AGap: TATGapItem) of object;
  TATSynEditCalcBookmarkColorEvent = procedure(Sender: TObject; ABookmarkKind: integer; var AColor: TColor) of object;
  TATSynEditCalcStapleEvent = procedure(Sender: TObject; ALine, AIndent: integer; var AStapleColor: TColor) of object;
  TATSynEditCalcHiliteEvent = procedure(Sender: TObject; var AParts: TATLineParts;
    ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor) of object;
  TATSynEditPasteEvent = procedure(Sender: TObject; var AHandled: boolean;
    AKeepCaret, ASelectThen: boolean) of object;
  TATSynEditHotspotEvent = procedure(Sender: TObject; AHotspotIndex: integer) of object;
  TATSynEditCheckInputEvent = procedure(Sender: TObject; AChar: WideChar; var AllowInput: boolean) of object;
  TATSynEditGetTokenEvent = function(Sender: TObject; AX, AY: integer): TATTokenKind of object;
  TATSynEditBeforeOrAfterHiliteEvent = procedure(Sender: TObject; AMainText: boolean) of object;
  TATSynEditEnabledUndoRedoChanged = procedure(Sender: TObject; AUndoEnabled, ARedoEnabled: boolean) of object;

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

type
  TATTimingRec = record
    TimeAll,
    TimeMinimap,
    TimeTextout: integer;
  end;
  TATTimingQueue = specialize TDeque<TATTimingRec>;

  TATEditorChosenBackColor = (
    None,
    CurrentLineBG,
    BookmarkBG,
    MarkedRangeBG,
    DefaultBG
    );

type
  { TATSynEdit }

  TATSynEdit = class(TCustomControl)
  public const
    cInitTextOffsetLeft = 0;
    cInitTextOffsetTop = 2;
    cInitAutoPairForMultiCarets = true;
    cInitInputNumberAllowNegative = true;
    cInitMaskChar = '*';
    cInitGapBitmapAlignment = taCenter;
    cInitGapBitmapIndent = 0;
    cInitScrollAnimationSteps = 4;
    cInitScrollAnimationSleep = 0;
    cInitUndoLimit = 5000;
    cInitUndoMaxCarets = 4000;
    cInitUndoIndentVert = 15;
    cInitUndoIndentHorz = 20;
    cInitUndoPause = 300;
    cInitUndoPause2 = 1000;
    cInitUndoPauseHighlightLine = true;
    cInitUndoForCaretJump = true;
    cInitMicromapShowForMinCount = 2;
    cInitScrollbarHorzAddSpace = 2;
    cInitIdleInterval = 0; //0: don't fire OnIdle, faster
    cInitCaretsPrimitiveColumnSelection = true;
    cInitCaretsMultiToColumnSel = true;
    cInitBorderVisible = true;
    cInitBorderWidth = 1;
    cInitBorderWidthFocused = 1;
    cInitBorderWidthWithColor = 3;
    cInitRulerNumeration = TATEditorRulerNumeration.Num_0_10_20;
    cInitRulerHeightPercents = 100;
    cInitRulerFontSizePercents = 80;
    cInitRulerMarkCaret = 1;
    cInitRulerMarkSmall = 3;
    cInitRulerMarkBig = 7;
    cInitWrapMode = TATEditorWrapMode.ModeOff;
    cInitWrapEnabledForMaxLines = 60*1000;
    cInitSpacingTop = 0;
    cInitSpacingBottom = 1;
    cInitCaretBlinkTime = 600;
    cInitMinimapVisible = false;
    cInitMinimapSelColorChange = 6; //how much minimap sel-rect is darker, in %
    cInitMinimapTooltipVisible = true;
    cInitMinimapTooltipHeight = 6;
    cInitMinimapTooltipWidthPercents = 60;
    cInitMicromapVisible = false;
    cInitMicromapScalePerColumn = 50;
    cInitMicromapOnScrollbar = false;
    cInitMicromapBookmarks = false;
    cInitShowMouseSelFrame = true;
    cInitMarginRight = 80;
    cInitTabSize = 8;
    cInitNumbersStyle = TATEditorNumbersStyle.Each5th;
    cInitNumbersIndentPercents = 60;
    cInitBitmapWidth = 1000;
    cInitBitmapHeight = 800;
    cInitGutterPlusSize = 4;
    cInitGutterWidthBookmarks = 16;
    cInitGutterWidthNumbers = 10;
    cInitGutterWidthFolding = 14;
    cInitGutterWidthSeparator = 1;
    cInitGutterWidthEmpty = 2;
    cInitGutterWidthLineStates = 3;
    cInitMarkerSize = 30;
    cInitFoldStyle = TATEditorFoldStyle.HereWithTruncatedText;
    cInitFoldUnderlineOffset = 3;
    cInitFoldTooltipVisible = true;
    cInitFoldTooltipLineCount = 15;
    cInitFoldTooltipWidthPercents = 80;
    cInitMinLineLenToCalcURL = 4;
    cInitMaxLineLenToCalcURL = 5000; //AliExpress has URLs of len 520, but for text files with long paragraphs we need bigger limit
    cInitDragDropMarkerWidth = 4;
    cInitStapleHiliteAlpha = 180;
    cInitZebraAlphaBlend = 235;
    cInitDimUnfocusedBack = 0;
    cInitShowFoldedMarkWithSelectionBG = true;

    cUrlRegex_Email = '\b(mailto:)?\w[\w\-\+\.]*@\w[\w\-\.]*\.\w{2,}\b';
    cUrlRegex_WebBegin = '\b(https?://|ftp://)';
    cUrlRegex_WebSite = '\w[\w\-\.@]*(:\d+)?'; // @ for password; :\d+ is port
    cUrlRegex_WebAnchor = '(\#[\w\-\./%:!]*)?';
    cUrlRegex_WebParams = '(\?[^\s"''<>]*[\w/\-\+\*=])?';
    cUrlRegex_WebFolder =
      '[~\w\.\-\+/%@!%:&\*=\|,;\$\[\]]' + //always allowed chars
      '|\(.*?\)' + //brackets pair
      //'|\[.*?\]' + //brackets pair
      '|\{.*?\}';  //brackets pair
      //'|([''"`](?![\x20\x09\x0A\x0D.,;:]))'; //chars allowed only if not followed by a delimiter

    cUrlRegex_Web =
      cUrlRegex_WebBegin
      + cUrlRegex_WebSite
      + '(/(' + cUrlRegex_WebFolder + ')*)?'
      + cUrlRegex_WebParams
      + cUrlRegex_WebAnchor;
    cUrlRegexInitial = cUrlRegex_Email + '|' + cUrlRegex_Web;

  private
    FFontProportional: boolean;
    FFontItalic: TFont;
    FFontBold: TFont;
    FFontBoldItalic: TFont;
    FTimersEnabled: boolean;
    FTimerIdle: TTimer;
    FTimerBlink: TTimer;
    FTimerScroll: TTimer;
    FTimerNiceScroll: TTimer;
    FTimerDelayedParsing: TTimer;
    FTimerFlicker: TTimer;
    FPaintFlags: TATEditorInternalFlags;
    FPaintLocked: integer;
    FBitmap: TBitmap;
    FKeymap: TATKeymap;
    FKeymapHistory: TATKeyArray;
    FActivationTime: QWord;
    FParentFrameObject: TCustomFrame;
    FWantTabs: boolean;
    FWantReturns: boolean;
    FEditorIndex: integer;
    FMarginRight: integer;
    FMarginList: array of integer;
    FStringsInt: TATStrings;
    FStringsExternal: TATStrings;
    FTabHelper: TATStringTabHelper;
    FAdapterHilite: TATAdapterHilite;
    FAdapterIME: TATAdapterIME;
    FCharSizer: TATCharSizer;
    FFold: TATFoldRanges;
    FFoldImageList: TImageList;
    FFoldStyle: TATEditorFoldStyle;
    FFoldUnderlineOffset: integer;
    FFoldEnabled: boolean;
    FFoldTooltipVisible: boolean;
    FFoldTooltipWidthPercents: integer;
    FFoldTooltipLineCount: integer;
    FFoldIconForMinimalRange: integer;
    FCursorText: TCursor;
    FCursorColumnSel: TCursor;
    FCursorGutterBookmark: TCursor;
    FCursorGutterNumbers: TCursor;
    FCursorMinimap: TCursor;
    FCursorMicromap: TCursor;
    FTextOffset: TPoint;
    FSpacingTopEdge: integer; //offset of entire text-rect from top edge
    FSpacingTopEdge1: integer; //same as above, maybe with 1-2px adjustment
    FSpacingTop: integer; //offset of text from line's top
    FSpacingBottom: integer; //offset of text's bottom from line's bottom
    FTextHint: string;
    FTextHintFontStyle: TFontStyles;
    FTextHintCenter: boolean;
    FSel: TATCaretSelections;
    FSelRect: TRect;
    FSelRectBegin: TPoint;
    FSelRectEnd: TPoint;
    FVisibleColumns: integer;
    FCommandLog: TATCommandLog;
    FCarets: TATCarets;
    FCaretShowEnabled: boolean;
    FCaretShown: boolean;
    FCaretBlinkEnabled: boolean;
    FCaretBlinkTime: integer;
    FCaretShapeNormal: TATCaretShape;
    FCaretShapeOverwrite: TATCaretShape;
    FCaretShapeReadonly: TATCaretShape;
    FCaretVirtual: boolean;
    FCaretSpecPos: boolean;
    FCaretStopUnfocused: boolean;
    FCaretHideUnfocused: boolean;
    FCaretAllowNextBlink: boolean;
    FCaretDistanceFromTop: integer;
    FCaretDistanceFromBottom: integer;
    FIsEntered: boolean;
    FIsIniting: boolean;
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
    FMouseDownCoordOriginal: TATPoint;
    FMouseDownCoord: TATPoint;
    FMouseDragCoord: TATPoint;
    FMouseDownPnt: TPoint;
    FMouseDownPnt_ColumnSelOrigin: TPoint;
    FMouseDownAndColumnSelection: boolean;
    FMouseDownGutterLineNumber: integer;
    FMouseDownOnEditingArea: boolean;
    FMouseDownOnMinimap: boolean;
    FMouseDownDouble: boolean;
    FMouseDownDouble_SelBegin: TPoint;
    FMouseDownDouble_SelEnd: TPoint;
    FMouseDownWithCtrl: boolean;
    FMouseDownWithAlt: boolean;
    FMouseDownWithShift: boolean;
    FMouseNiceScrollPos: TATPoint;
    FMouseDragDropping: boolean;
    FMouseDragDroppingReal: boolean;
    FMouseDragMinimap: boolean;
    FMouseDragMinimapDelta: integer;
    FMouseDragMinimapSelHeight: integer;
    FMouseDragHandlerDisabled: boolean;
    FMouseRightClickOnGutterIsHandled: boolean;
    FMouseAutoScrollDirection: TATEditorDirection;
    FMouseActions: TATEditorMouseActions;
    FLastControlWidth: integer;
    FLastControlHeight: integer;
    FLastHotspot: integer;
    FLastTextCmd: integer;
    FLastTextCmdText: atString;
    FLastCommand: integer;
    FLastCommandChangedText: boolean;
    FLastCommandChangedText2: boolean;
    FLastCommandMakesColumnSel: boolean;
    FLastCommandDelayedParsingOnLine: integer;
    FLastLineOfSlowEvents: integer;
    FLastUndoTick: QWord;
    FLastUndoPaused: boolean;
    FLastEnabledUndo: boolean;
    FLastEnabledRedo: boolean;
    FLastCaretY: integer;
    FLineTopTodo: integer;
    FIsRunningCommand: boolean;
    FCursorOnMinimap: boolean;
    FCursorOnGutter: boolean;
    FDropMarker_TextPos: TPoint;
    FDropMarker_Coord: TATPoint;
    FAdapterIsDataReady: boolean;
    FTimingQueue: TATTimingQueue;
    FOnClickDbl,
    FOnClickTriple,
    FOnClickMiddle: TATSynEditClickEvent;
    FOnClickMoveCaret: TATSynEditClickMoveCaretEvent;
    FOnClickGap: TATSynEditClickGapEvent;
    FOnClickEndSelect: TATSynEditClickMoveCaretEvent;
    FOnClickLink: TATSynEditClickLinkEvent;
    FOnIdle: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnChangeDetailed: TATSynEditChangeDetailedEvent;
    FOnChangeLog: TATStringsChangeLogEvent;
    FOnChangeCaretPos: TNotifyEvent;
    FOnChangeState: TNotifyEvent;
    FOnChangeZoom: TNotifyEvent;
    FOnChangeModified: TNotifyEvent;
    FOnChangeBookmarks: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FOnClickGutter: TATSynEditClickGutterEvent;
    FOnClickMicromap: TATSynEditClickMicromapEvent;
    FOnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent;
    FOnDrawGap: TATSynEditDrawGapEvent;
    FOnDrawLine: TATSynEditDrawLineEvent;
    FOnDrawMicromap: TATSynEditDrawRectEvent;
    FOnDrawEditor: TATSynEditDrawRectEvent;
    FOnDrawRuler: TATSynEditDrawRulerEvent;
    FOnCommand: TATSynEditCommandEvent;
    FOnCommandAfter: TATSynEditCommandAfterEvent;
    FOnCommandKeyEnter: TNotifyEvent;
    FOnCalcCaretsCoords: TNotifyEvent;
    FOnCalcHilite: TATSynEditCalcHiliteEvent;
    FOnCalcStaple: TATSynEditCalcStapleEvent;
    FOnCalcBookmarkColor: TATSynEditCalcBookmarkColorEvent;
    FOnCalcTabSize: TATStringTabCalcEvent;
    FOnPaste: TATSynEditPasteEvent;
    FOnHotspotEnter: TATSynEditHotspotEvent;
    FOnHotspotExit: TATSynEditHotspotEvent;
    FOnCheckInput: TATSynEditCheckInputEvent;
    FOnBeforeCalcHilite: TATSynEditBeforeOrAfterHiliteEvent;
    FOnAfterCalcHilite: TATSynEditBeforeOrAfterHiliteEvent;
    FOnGetToken: TATSynEditGetTokenEvent;
    FOnEnabledUndoRedoChanged: TATSynEditEnabledUndoRedoChanged;
    FWrapInfo: TATWrapInfo;
    FWrapTemps: TATWrapItems;
    FWrapMode: TATEditorWrapMode;
    FWrapUpdateNeeded: boolean;
    FWrapIndented: boolean;
    FWrapAddSpace: integer;
    FWrapEnabledForMaxLines: integer;
    FUnprintedVisible,
    FUnprintedSpaces,
    FUnprintedSpacesTrailing,
    FUnprintedSpacesBothEnds,
    FUnprintedSpacesOnlyInSelection,
    FUnprintedSpacesAlsoInSelection,
    FUnprintedForceTabs,
    FUnprintedEof,
    FUnprintedEnds,
    FUnprintedEndsDetails,
    FUnprintedWraps: boolean;
    FPrevModified: boolean;
    FPrevFont: record
      FontName: string;
      FontSize: integer;
      SpacingTop: integer;
      SpacingBottom: integer;
      OptScaleFont: integer;
      GlobalScale: integer;
      GlobalScaleFont: integer;
    end;
    FPrevCaret: record
      PosX,
      PosY,
      EndX,
      EndY: integer;
    end;
    FCharSize: TATEditorCharSize;
    FCharSizeMinimap: TATEditorCharSize;
    FGutter: TATGutter;
    FGutterDecor: TATGutterDecor;
    FGutterDecorImages: TImageList;
    FGutterDecorAlignment: TAlignment;
    FGutterBandBookmarks: integer;
    FGutterBandNumbers: integer;
    FGutterBandStates: integer;
    FGutterBandFolding: integer;
    FGutterBandSeparator: integer;
    FGutterBandEmpty: integer;
    FGutterBandDecor: integer;
    FColorFont: TColor;
    FColorBG: TColor;
    FColorGutterBG: TColor;
    FColorGutterFoldBG: TColor;
    FColorRulerBG: TColor;
    FColorCollapseMarkBG: TColor;
    FRulerHeight: integer;
    FNumbersIndent: integer;
    FRectMain,
    FRectMainVisible,
    FRectMinimap,
    FRectMinimapTooltip,
    FRectMicromap,
    FRectGutter,
    FRectGutterBm,
    FRectGutterNums,
    FRectRuler: TRect;
    FClientW: integer; //saved on Paint, to avoid calling Controls.ClientWidth/ClientHeight
    FClientH: integer;
    FLineBottom: integer;
    FParts: TATLineParts; //this is used in DoPaintLine
    FPartsMinimap: TATLineParts; //this is used by DoPaintMinimapLine, in thread
    FPartsSel: array[boolean] of TATLineParts; //this is used in DoPartCalc_ApplySelectionOver
    FScrollVert,
    FScrollHorz,
    FScrollVertMinimap,
    FScrollHorzMinimap: TATEditorScrollInfo;
    FScrollbarVert,
    FScrollbarHorz: TATScrollbar;
    FScrollbarLock: boolean;
    FPrevHorz,
    FPrevVert: TATEditorScrollInfo;
    FMinimapWidth: integer;
    FMinimapCharWidth: integer;
    FMinimapCustomScale: integer;
    FMinimapVisible: boolean;
    FMinimapShowSelBorder: boolean;
    FMinimapShowSelAlways: boolean;
    FMinimapSelColorChange: integer;
    FMinimapAtLeft: boolean;
    FMinimapTooltipVisible: boolean;
    FMinimapTooltipEnabled: boolean;
    FMinimapTooltipBitmap: TBitmap;
    FMinimapTooltipHeight: integer;
    FMinimapTooltipWidthPercents: integer;
    FMinimapTooltipFontSize: integer;
    FMinimapHiliteLinesWithSelection: boolean;
    FMinimapDragImmediately: boolean;
    FMicromap: TATMicromap;
    FMicromapVisible: boolean;
    FMicromapScalePerColumn: integer;
    FMicromapOnScrollbar: boolean;
    FMicromapLineStates: boolean;
    FMicromapSelections: boolean;
    FMicromapBookmarks: boolean;
    FMicromapShowForMinCount: integer;
    FFoldedMarkList: TATFoldedMarks;
    FFoldedMarkCurrent: TATFoldedMark;
    FFoldedMarkTooltip: TPanel;
    FPaintCounter: integer;
    FPaintStarted: boolean;
    FPaintWorking: boolean;
    FTickMinimap: QWord;
    FTickAll: QWord;
    FShowOsBarVert: boolean;
    FShowOsBarHorz: boolean;
    FMinimapBmp: TBGRABitmap;
    FMinimapThread: TATMinimapThread;
    FEventMapStart: TSimpleEvent; //fired when need to start MinimapThread work
    FEventMapDone: TSimpleEvent; //fired by MinimapThread, when it's work done
    FColorOfStates: array[TATLineState] of TColor;
    FFoldingAsStringTodo: string;

    //these options are implemented in CudaText, they are dummy here
    FOptThemed: boolean;
    FOptAutoPairForMultiCarets: boolean;
    FOptAutoPairChars: string;
    FOptAutoPair_DisableCharDoubling: boolean;
    FOptAutocompleteSymbolsAllowedBeforeCaret: string;
    FOptAutocompleteAutoshowCharCount: integer;
    FOptAutocompleteTriggerChars: string;
    FOptAutocompleteCommitChars: string;
    FOptAutocompleteCommitOnEnter: boolean;
    FOptAutocompleteCloseChars: string;
    FOptAutocompleteAddOpeningBracket: boolean;
    FOptAutocompleteUpDownAtEdge: integer;
    FOptAutocompleteCommitIfSingleItem: boolean;

    //options
    FOptForceSeparateCharSizer: boolean;
    FOptGapBitmapAlignment: TAlignment;
    FOptGapBitmapIndent: integer;
    FOptFlickerReducingPause: integer;
    FOptInputNumberOnly: boolean;
    FOptInputNumberAllowNegative: boolean;
    FOptMaskChar: WideChar;
    FOptMaskCharUsed: boolean;
    FOptScrollAnimationSteps: integer;
    FOptScrollAnimationSleep: integer;
    FOptScaleFont: integer;
    FOptIdleInterval: integer;
    FOptPasteAtEndMakesFinalEmptyLine: boolean;
    FOptPasteMultilineTextSpreadsToCarets: boolean;
    FOptPasteWithEolAtLineStart: boolean;
    FOptMinLineLenToCalcURL: integer;
    FOptMaxLineLenToCalcURL: integer;
    FOptMaxLinesToCountUnindent: integer;
    FOptScrollStyleVert: TATEditorScrollbarStyle;
    FOptScrollStyleHorz: TATEditorScrollbarStyle;
    FOptScrollSmooth: boolean;
    FOptScrollIndentCaretHorz: integer; //offsets for caret-moving: if caret goes out of control
    FOptScrollIndentCaretVert: integer; //must be 0, >0 gives jumps on move-down
    FOptUndoLimit: integer;
    FOptUndoGrouped: boolean;
    FOptUndoMaxCarets: integer;
    FOptUndoIndentVert: integer;
    FOptUndoIndentHorz: integer;
    FOptUndoPause: integer;
    FOptUndoPause2: integer;
    FOptUndoPauseHighlightLine: boolean;
    FOptUndoForCaretJump: boolean;
    FOptScrollbarsNew: boolean;
    FOptScrollbarHorizontalAddSpace: integer;
    FOptScrollLineCommandsKeepCaretOnScreen: boolean;
    FOptShowFontLigatures: boolean;
    FOptShowURLs: boolean;
    FOptShowURLsRegex: string;
    FOptShowDragDropMarker: boolean;
    FOptShowDragDropMarkerWidth: integer;
    FOptShowFoldedMarkWithSelectionBG: boolean;
    FOptStapleStyle: TATLineStyle;
    FOptStapleIndent: integer;
    FOptStapleWidthPercent: integer;
    FOptStapleHiliteActive: boolean;
    FOptStapleHiliteActiveAlpha: integer;
    FOptStapleEdge1: TATEditorStapleEdge;
    FOptStapleEdge2: TATEditorStapleEdge;
    FOptStapleIndentConsidersEnd: boolean;
    FOptCaretsPrimitiveColumnSelection: boolean;
    FOptCaretsAddedToColumnSelection: boolean;
    FOptCaretPreferLeftSide: boolean;
    FOptCaretPosAfterPasteColumn: TATEditorPasteCaret;
    FOptCaretFixAfterRangeFolded: boolean;
    FOptCaretsMultiToColumnSel: boolean;
    FOptCaretProximityVert: integer;
    FOptMarkersSize: integer;
    FOptShowScrollHint: boolean;
    FOptTextDuplicationMovesCaretDown: boolean;
    FOptTextCenteringCharWidth: integer;
    FOptTextOffsetLeft: integer;
    FOptTextOffsetTop: integer;
    FOptSavingForceFinalEol: boolean;
    FOptSavingTrimSpaces: boolean;
    FOptSavingTrimFinalEmptyLines: boolean;
    FOptIndentSize: integer;
    FOptIndentKeepsAlign: boolean;
    FOptIndentMakesWholeLinesSelection: boolean;
    FOptTrimLineOnPressingEnter: boolean;
    FOptBorderVisible: boolean;
    FOptBorderWidth: integer;
    FOptBorderWidthFocused: integer;
    FOptBorderWidthWithColor: integer;
    FOptBorderFocusedActive: boolean;
    FOptBorderRounded: boolean;
    FOptBorderColor: TColor;
    FOptCornerFontName: string;
    FOptCornerFontSize: integer;
    FOptCornerText: string;
    FOptCornerColorFont: TColor;
    FOptCornerColorBack: TColor;
    FOptCornerColorBorder: TColor;
    FOptCorner2FontName: string;
    FOptCorner2FontSize: integer;
    FOptCorner2Text: string;
    FOptCorner2ColorFont: TColor;
    FOptCorner2ColorBack: TColor;
    FOptCorner2ColorBorder: TColor;
    FOptRulerVisible: boolean;
    FOptRulerNumeration: TATEditorRulerNumeration;
    FOptRulerHeightPercents: integer;
    FOptRulerFontSizePercents: integer;
    FOptRulerMarkSizeCaret: integer;
    FOptRulerMarkSizeSmall: integer;
    FOptRulerMarkSizeBig: integer;
    FOptRulerMarkForAllCarets: boolean;
    FOptRulerTopIndentPercents: integer;
    FOptRulerText: string;
    FOptGutterVisible: boolean;
    FOptGutterPlusSize: integer;
    FOptGutterShowFoldAlways: boolean;
    FOptGutterShowFoldLines: boolean;
    FOptGutterShowFoldLinesAll: boolean;
    FOptGutterShowFoldLinesForCaret: boolean;
    FOptGutterShowBracketDecor: boolean;
    FOptGutterWidthBookmarks: integer;
    FOptGutterWidthNumbers: integer;
    FOptGutterWidthFolding: integer;
    FOptGutterWidthSeparator: integer;
    FOptGutterWidthEmpty: integer;
    FOptGutterWidthLineStates: integer;
    FOptGutterIcons: TATEditorGutterIcons;
    FOptNumbersAutosize: boolean;
    FOptNumbersAlignment: TAlignment;
    FOptNumbersStyle: TATEditorNumbersStyle;
    FOptNumbersShowFirst: boolean;
    FOptNumbersShowCarets: boolean;
    FOptNumbersIndentPercents: integer;
    FOptNonWordChars: atString;
    FOptAutoIndent: boolean;
    FOptAutoIndentKind: TATEditorAutoIndentKind;
    FOptAutoIndentBetterBracketsCurly: boolean;
    FOptAutoIndentBetterBracketsRound: boolean;
    FOptAutoIndentBetterBracketsSquare: boolean;
    FOptAutoIndentRegexRule: string;
    FOptTabSize: integer;
    FOptTabSpaces: boolean;
    FOptTabSmart: boolean;
    FOptLastLineOnTop: boolean;
    FOptOverwriteSel: boolean;
    FOptOverwriteAllowedOnPaste: boolean;
    FOptKeyBackspaceUnindent: boolean;
    FOptKeyBackspaceGoesToPrevLine: boolean;
    FOptKeyPageKeepsRelativePos: boolean;
    FOptKeyPageUpDownSize: TATEditorPageDownSize;
    FOptKeyUpDownNavigateWrapped: boolean;
    FOptKeyUpDownAllowToEdge: boolean;
    FOptKeyUpDownKeepColumn: boolean;
    FOptKeyLeftRightGoToNextLineWithCarets: boolean;
    FOptKeyLeftRightSwapSel: boolean;
    FOptKeyLeftRightSwapSelAndSelect: boolean;
    FOptKeyEndToNonSpace: boolean;
    FOptKeyHomeToNonSpace: boolean;
    FOptKeyHomeEndNavigateWrapped: boolean;
    FOptKeyTabIndents: boolean;
    FOptKeyTabIndentsVerticalBlock: boolean;
    FOptCopyLinesIfNoSel: boolean;
    FOptCutLinesIfNoSel: boolean;
    FOptCopyColumnBlockAlignedBySpaces: boolean;
    FOptShowFullSel: boolean;
    FOptShowFullHilite: boolean;
    FOptShowCurLine: boolean;
    FOptShowCurLineMinimal: boolean;
    FOptShowCurLineOnlyFocused: boolean;
    FOptShowCurLineIfWithoutSel: boolean;
    FOptShowCurColumn: boolean;
    FOptKeepSelFontColor: boolean;
    FOptShowMouseSelFrame: boolean;
    FOptMouseEnableAll: boolean;
    FOptMouseEnableNormalSelection: boolean;
    FOptMouseEnableColumnSelection: boolean;
    FOptMouseColumnSelectionWithoutKey: boolean;
    FOptMouseHideCursor: boolean;
    FOptMouseClickOpensURL: boolean;
    FOptMouseClickNumberSelectsLine: boolean;
    FOptMouseClickNumberSelectsLineWithEOL: boolean;
    FOptMouseClickNumberSelectsFoldedRange: boolean;
    FOptMouse2ClickAction: TATEditorDoubleClickAction;
    FOptMouse2ClickOpensURL: boolean;
    FOptMouse2ClickDragSelectsWords: boolean;
    FOptMouse2ClickOnFoldMarkSelectsFoldedLines: boolean;
    FOptMouse3ClickSelectsLine: boolean;
    FOptMouseDragDrop: boolean;
    FOptMouseDragDropCopying: boolean;
    FOptMouseDragDropCopyingWithState: TShiftStateEnum;
    FOptMouseRightClickMovesCaret: boolean;
    FOptMouseMiddleClickAction: TATEditorMiddleClickAction;
    FOptMouseWheelScrollVert: boolean;
    FOptMouseWheelScrollHorz: boolean;
    FOptMouseWheelScrollVertSpeed: integer;
    FOptMouseWheelScrollHorzSpeed: integer;
    FOptMouseWheelScrollHorzWithState: TShiftStateEnum;
    FOptMouseWheelZooms: boolean;
    FOptMouseWheelZoomsWithState: TShiftStateEnum;
    FOptShowIndentLines: boolean;
    FOptShowGutterCaretBG: boolean;
    FOptAllowRepaintOnTextChange: boolean;
    FOptAllowReadOnly: boolean;
    FOptZebraActive: boolean;
    FOptZebraStep: integer;
    FOptZebraAlphaBlend: byte;
    FOptDimUnfocusedBack: integer;

    //
    function DoCalcFoldDeepestRangeContainingCaret: integer;
    function DoCalcForegroundFromAttribs(AX, AY: integer; var AColor: TColor;
      var AFontStyles: TFontStyles): boolean;
    function DoCalcFoldProps(AWrapItemIndex, AFoldRangeWithCaret: integer; out AProps: TATFoldBarProps): boolean;
    class function CheckInputForNumberOnly(const S: UnicodeString; X: integer;
      ch: WideChar; AllowNegative: boolean): boolean;
    procedure ClearSelRectPoints;
    procedure ClearMouseDownVariables;
    procedure DebugSelRect;
    function DoCalcLineLen(ALineIndex: SizeInt): SizeInt;
    procedure DoChangeBookmarks;
    procedure DoHandleWheelRecord(const ARec: TATEditorWheelRecord);
    procedure DoStringsOnUnfoldLine(Sender: TObject; ALine: SizeInt);
    function FindLineNextNonspaceBegin(const ALine: UnicodeString; AFromOffset: integer): integer;
    function FindLineNextNonspaceEnd(const ALine: UnicodeString; AFromOffset: integer): integer;
    function FindUnpairedBracketBackward(const ALine: UnicodeString;
      ALineIndex, AColumn: integer; out ABracketChar: char): integer;
    function GetLineIndentInSpaces(ALine: integer): integer;
    function GetLineIndentInPixels(ALine: integer; const ACharSize: TATEditorCharSize): integer;
    procedure InitClipboardExData(out Data: TATEditorClipboardExData);
    procedure FlushEditingChangeEx(AChange: TATLineChangeKind; ALine, AItemCount: SizeInt);
    procedure FlushEditingChangeLog(ALine: SizeInt);
    function GetActualDragDropIsCopying: boolean;
    function GetIndentString: UnicodeString;
    function GetActualProximityVert: integer;
    function GetAttribs: TATMarkers;
    procedure GetClientSizes(out W, H: integer);
    function GetFoldingAsString: string;
    function GetMarkers: TATMarkers;
    function GetDimRanges: TATDimRanges;
    function GetHotspots: TATHotspots;
    function GetGutterDecor: TATGutterDecor;
    procedure InitLengthArray(out Lens: TATIntArray);
    procedure CalcCaretDistanceFromEdges(ACommand: integer;
      out ALinesFromTop, ALinesFromBottom: integer);
    function IsCoordInFoldedMark(AX, AY: integer): boolean;
    procedure MenuitemClipboardRecentsClick(Sender: TObject);
    procedure SetEditorIndex(AValue: integer);
    function GetUndoForMarkers: boolean;
    procedure SetUndoForMarkers(AValue: boolean);
    function GetUndoForAttribs: boolean;
    procedure SetUndoForAttribs(AValue: boolean);
    procedure SetOptScaleFont(AValue: integer);
    procedure UpdateClipboardRecents(const AText: string);
    procedure UpdateGapForms(ABeforePaint: boolean);
    procedure UpdateAndWait(AUpdateWrapInfo: boolean; APause: integer);
    procedure SetFoldingAsString(const AValue: string);
    procedure SetOptShowURLsRegex(const AValue: string);
    procedure SetShowOsBarVert(AValue: boolean);
    procedure SetShowOsBarHorz(AValue: boolean);
    procedure DebugFindWrapIndex;
    function DoCalcIndentCharsFromPrevLines(AX, AY: integer): integer;
    procedure DoCalcPosColor(AX, AY: integer; var AColor: TColor; AMainText: boolean);
    procedure DoCalcLineEntireColor(ALine: integer;
      AUseColorOfCurrentLine: boolean;
      AUseColorOfCurrentLine2: boolean;
      out AColor: TColor; out AColorForced: boolean;
      out AChosenEnum: TATEditorChosenBackColor;
      AHiliteLineWithSelection: boolean);
    procedure DoCaretsApplyShape(var R: TRect; Props: TATCaretShape; W, H: integer);
    function DoCaretApplyProximityToVertEdge(ACaretPos: TPoint; ACaretCoordY: Int64;
      AProximity, AIndentVert: integer): boolean;
    function DoCaretApplyProximityToHorzEdge(ACaretCoordX: Int64;
      AProximity, AIndentHorz: integer): boolean;
    procedure DoCaretsAddOnColumnBlock(APos1, APos2: TPoint; const ARect: TRect);
    procedure DoCaretsFixForSurrogatePairs(AMoveRight: boolean);
    function DoCaretsKeepOnScreen(ADirection: TATEditorDirection): boolean;
    procedure DoCaretsAssign(NewCarets: TATCarets);
    procedure DoDropText(AndDeleteSelection: boolean);
    procedure DoMarkAllRangesUnfolded;
    procedure DoFoldbarClick_LineIndex(ALine: integer);
    procedure DoFoldbarClick_RangeIndex(ARangeIndex: integer);
    procedure DoHandleRightClick(X, Y: integer);
    function DoHandleClickEvent(AEvent: TATSynEditClickEvent): boolean;
    procedure DoHotspotsExit;
    function DoGetTokenKind(AX, AY: integer): TATTokenKind;
    procedure DoHintShowForScrolling;
    procedure DoHintHide;
    procedure DoHintShowForBookmark(ALine: integer);
    procedure DoMenuGutterFold_AddDynamicItems(Menu: TPopupMenu);
    procedure DoMenuGutterFold;
    procedure DoMenuContextFromKeyboard;
    procedure DoMinimapClick(APosY: integer);
    procedure DoMinimapDrag(APosY: integer);
    procedure DoStringsOnChangeLog(Sender: TObject; ALine: SizeInt);
    procedure DoStringsOnProgress(Sender: TObject; var ACancel: boolean);
    procedure DoStringsOnUndoAfter(Sender: TObject; AX, AY: SizeInt);
    procedure DoStringsOnUndoBefore(Sender: TObject; AX, AY: SizeInt);
    procedure DoScroll_SetPos(var AScrollInfo: TATEditorScrollInfo; APos: integer);
    procedure DoScroll_LineTop(ALine: integer; AUpdate: boolean);
    function DoScroll_IndentFromBottom(AWrapInfoIndex, AIndentVert: integer): boolean;
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
    function IsFoldingUnderlineNeededForWrapitem(AWrapIndex: integer): boolean;
    function IsRepaintNeededOnEnterOrExit: boolean;
    procedure MenuFoldFoldAllClick(Sender: TObject);
    procedure MenuFoldLevelClick(Sender: TObject);
    procedure MenuFoldUnfoldAllClick(Sender: TObject);
    procedure MenuFoldPlusMinusClick(Sender: TObject);
    procedure FoldedMarkTooltipPaint(Sender: TObject);
    procedure FoldedMarkMouseEnter(Sender: TObject);
    procedure OnNewScrollbarHorzChanged(Sender: TObject);
    procedure OnNewScrollbarVertChanged(Sender: TObject);
    procedure DoPartCalc_CreateNew(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer; AColorBG: TColor; AWithSelection: boolean);
    procedure DoPartCalc_ApplySelectionOver(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer; AMainText: boolean);
    procedure DoPartCalc_ApplyAttribsOver(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer; AColorBG: TColor; AMainText: boolean);
    function GetAutoIndentString(APosX, APosY: integer; AUseIndentRegexRule, AForceIndent: boolean): atString;
    function GetFoldedMarkText(APosX, APosY: integer): string;
    function GetModified: boolean;
    function GetModifiedBookmarks: boolean;
    procedure SetModifiedBookmarks(AValue: boolean);
    function Unfolded_NextLineNumber(ALine: integer; ADown: boolean): integer;
    function Unfolded_FirstLineNumber: integer;
    function Unfolded_LastLineNumber: integer;
    function GetOneLine: boolean;
    function GetRedoCount: integer;
    function GetLinesFromTop: integer;
    function GetText: UnicodeString;
    function GetUndoAfterSave: boolean;
    function GetUndoCount: integer;
    procedure InitAttribs;
    procedure InitMarkers;
    procedure InitHotspots;
    procedure InitDimRanges;
    procedure InitGutterDecor;
    procedure InitMarkedRange;
    procedure InitFoldedMarkList;
    procedure InitFoldedMarkTooltip;
    procedure InitFoldImageList;
    procedure InitMenuStd;
    procedure InitTimerScroll;
    procedure InitTimerNiceScroll;
    procedure StartTimerDelayedParsing;
    function IsWrapItemWithCaret(constref AWrapItem: TATWrapItem): boolean;
    procedure MenuStdClick(Sender: TObject);
    procedure MenuStdPopup(Sender: TObject);
    procedure DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer;
      AItems: TATWrapItems; AConsiderFolding: boolean);
    procedure DoCalcLineHilite(const AData: TATWrapItem;
      var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
      AColorBG: TColor; AColorForced: boolean; var AColorAfter: TColor;
      AMainText, AWithSelection: boolean);
    function DoScaleFont(AValue: integer): integer;
    //select
    procedure DoSelectionDeleteOrReset;
    procedure DoSelect_ExtendSelectionByLine(AUp: boolean);
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
    procedure DoPaint(ALineFrom: integer);
    procedure DoPaintBorders(C: TCanvas);
    procedure DoPaintBorder(C: TCanvas; AColor: TColor; ABorderWidth: integer; ABorderRounded: boolean);
    procedure DoPaintAll(C: TCanvas; ALineFrom: integer);
    procedure DoPaintMain(C: TCanvas; ALineFrom: integer);
    procedure DoPaintLine(C: TCanvas;
      const ARectLine: TRect;
      const ACharSize: TATEditorCharSize;
      var AScrollHorz: TATEditorScrollInfo;
      const AWrapIndex: integer;
      var ATempParts: TATLineParts);
    procedure DoPaintMinimapLine(ARectLine: TRect;
      const ACharSize: TATEditorCharSize;
      var AScrollHorz: TATEditorScrollInfo;
      const AWrapIndex: integer;
      var ATempParts: TATLineParts);
    procedure DoPaintGutterOfLine(C: TCanvas;
      ARect: TRect;
      const ACharSize: TATEditorCharSize;
      AWrapIndex, AFoldRangeWithCaret: integer);
    procedure DoPaintGutterBookmarkStdIcon(C: TCanvas; ARect: TRect);
    procedure DoPaintNiceScroll(C: TCanvas);
    procedure DoPaintGutterNumber(C: TCanvas; ALineIndex, ACoordTop: integer; ABand: TATGutterItem);
    procedure DoPaintMarginLineTo(C: TCanvas; AX: Int64; AWidth: integer; AColor: TColor);
    procedure DoPaintRuler(C: TCanvas);
    procedure DoPaintRulerCaretMark(C: TCanvas; ACaretX: Int64);
    procedure DoPaintRulerCaretMarks(C: TCanvas);
    procedure DoPaintTiming(C: TCanvas);
    procedure DoPaintText(C: TCanvas;
      const ARect: TRect;
      const ACharSize: TATEditorCharSize;
      AWithGutter: boolean;
      var AScrollHorz, AScrollVert: TATEditorScrollInfo;
      AWrapIndex: integer);
    procedure DoPaintTextFragment(C: TCanvas;
      const ARect: TRect;
      ALineFrom: integer;
      AConsiderWrapInfo: boolean;
      AColorBG, AColorBorder: TColor;
      AFontSize: integer;
      AWithSelection: boolean);
    procedure DoPaintLineIndent(C: TCanvas;
      const ARect: TRect;
      const ACharSize: TATEditorCharSize;
      ACoordY: integer;
      AIndentSize: integer;
      AColorBG: TColor;
      AScrollPos: integer;
      AShowIndentLines: boolean);
    procedure DoPaintMinimapAllToBGRABitmap;
    procedure DoPaintMinimapTextToBGRABitmap(
      const ARect: TRect;
      const ACharSize: TATEditorCharSize;
      var AScrollHorz, AScrollVert: TATEditorScrollInfo);
    procedure DoPaintMinimapSelToBGRABitmap;
    procedure DoPaintMinimapTooltip(C: TCanvas);
    procedure DoPaintMicromap(C: TCanvas);
    procedure DoPaintMargins(C: TCanvas);
    procedure DoPaintGap(C: TCanvas; const ARect: TRect; AGap: TATGapItem);
    procedure DoPaintFoldedMark(C: TCanvas;
      APosX, APosY, ACoordX, ACoordY: integer;
      const AMarkText: string);
    procedure DoPaintFoldingUnderline(C: TCanvas;
      ALine: integer;
      const ARectLine: TRect;
      const ACharSize: TATEditorCharSize;
      const AScrollHorz: TATEditorScrollInfo;
      AOutputTextWidth: integer);
    procedure DoPaintCaretShape(C: TCanvas; ARect: TRect; ACaret: TATCaretItem;
      ACaretShape: TATCaretShape);
    procedure DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
    procedure DoPaintCornerText_RightBottom(C: TCanvas; const AText, AFontName: string;
      AFontSize: integer; AColorFont, AColorBack, AColorBorder: TColor);
    procedure DoPaintCornerText_RightTop(C: TCanvas; const AText, AFontName: string;
      AFontSize: integer; AColorFont, AColorBack, AColorBorder: TColor);
    procedure TimerBlinkDisable;
    procedure TimerBlinkEnable;
    procedure DoPaintSelectedLineBG(C: TCanvas;
      const ACharSize: TATEditorCharSize;
      const AVisRect: TRect;
      APointLeft, APointText: TPoint;
      const AWrapItem: TATWrapItem;
      ALineWidth: integer;
      const AScrollHorz: TATEditorScrollInfo);
    procedure DoPaintMarkersTo(C: TCanvas);
    procedure DoPaintMarkerOfDragDrop(C: TCanvas);
    procedure DoPaintGutterPlusMinus(C: TCanvas; AX, AY: integer; APlus: boolean;
      ALineColor: TColor);
    procedure DoPaintGutterFolding(C: TCanvas;
      AWrapItemIndex, AFoldRangeWithCaret: integer;
      ACoord1, ACoord2: TPoint);
    procedure DoPaintGutterDecor(C: TCanvas; ALine: integer; const ARect: TRect;
      out AIconPainted: boolean);
    procedure DoPaintGutterBandBG(C: TCanvas; AColor: TColor; AX1, AY1, AX2,
      AY2: integer; AEntireHeight: boolean);
    procedure DoPaintLockedWarning(C: TCanvas);
    procedure DoPaintStaple(C: TCanvas; const R: TRect; AColor: TColor);
    procedure DoPaintStaples(C: TCanvas;
      const ARect: TRect;
      const ACharSize: TATEditorCharSize;
      const AScrollHorz: TATEditorScrollInfo);
    procedure DoPaintTextHintTo(C: TCanvas);
    procedure DoPaintMouseSelFrame(C: TCanvas);
    //carets
    procedure DoCaretsExtend(ADown: boolean; ALines: integer);
    function GetCaretManyAllowed: boolean;
    //events
    procedure DoEventBeforeCalcHilite(AMainText: boolean);
    procedure DoEventAfterCalcHilite(AMainText: boolean);
    procedure DoEventClickMicromap(AX, AY: integer);
    procedure DoEventClickGutter(ABandIndex, ALineNumber: integer; var AHandled: boolean);
    function DoEventCommand(ACommand: integer; AInvoke: TATCommandInvoke; const AText: string): boolean;
    procedure DoEventDrawBookmarkIcon(C: TCanvas; ALineIndex, ABookmarkIndex: integer;
      const ARect: TRect; var AHandled: boolean);
    procedure DoEventCommandAfter(ACommand: integer; const AText: string);
    procedure DoEventEnabledUndoRedoChanged;
    //
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
    procedure SetSpacingTop(AValue: integer);
    procedure SetSpacingBottom(AValue: integer);
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
    procedure GetRectGutterNumbers(out R: TRect);
    procedure GetRectGutterBookmarks(out R: TRect);
    function GetTextOffset: TPoint;
    function GetPageLines: integer;
    function GetMinimapScrollPos: integer;
    procedure SetTabSize(AValue: integer);
    procedure SetTabSpaces(AValue: boolean);
    procedure SetText(const AValue: UnicodeString);
    procedure SetUndoAfterSave(AValue: boolean);
    procedure SetUndoAsString(const AValue: string);
    procedure SetUndoLimit(AValue: integer);
    procedure SetWrapMode(AValue: TATEditorWrapMode);
    procedure SetWrapIndented(AValue: boolean);
    procedure UpdateGutterBandIndexes;
    procedure UpdateScrollbarVert;
    procedure UpdateScrollbarHorz;
    procedure UpdateScrollbarsOfMinimap;
    procedure UpdateScrollInfoVertPartial(out APage, AMax: Int64);
    procedure UpdateSelRectFromPoints(const P1, P2: TPoint);
    procedure UpdateInitialVars(C: TCanvas);
    procedure UpdateLinksAttribs(ALineFrom: integer);
    function UpdateLinksRegexObject: boolean;
    procedure UpdateTabHelper;
    procedure UpdateCursor;
    procedure UpdateGutterColumns;
    procedure UpdateMinimapAutosize;
    procedure UpdateFoldedMarkTooltip;
    procedure UpdateClientSizes;
    function DoFormatLineNumber(N: integer): string;
    function UpdateScrollInfoFromMessage(var AInfo: TATEditorScrollInfo; const AMsg: TLMScroll): boolean;
    procedure UpdateCaretsCoords(AOnlyLast: boolean=false; ASkipInvisible: boolean=false);
    procedure UpdateMarkersCoords;
    procedure UpdateCharSize(var ACharSize: TATEditorCharSize; C: TCanvas);
    function GetScrollbarVisible(bVertical: boolean): boolean;
    procedure SetMarginRight(AValue: integer);

    //timers
    procedure TimerIdleTick(Sender: TObject);
    procedure TimerBlinkTick(Sender: TObject);
    procedure TimerScrollTick(Sender: TObject);
    procedure TimerNiceScrollTick(Sender: TObject);
    procedure TimerDelayedParsingTick(Sender: TObject);
    procedure TimerFlickerTick(Sender: TObject);

    //carets
    procedure DoCaretAddToPoint(AX, AY: integer);
    procedure DoCaretsColumnToPoint(AX, AY: integer);
    procedure DoCaretsDeleteOnSameLines;

    //editing
    function IsCommandResults_CaretMove(Res: TATCommandResults): boolean;
    function DoCommandCore(ACmd: integer; const AText: atString): TATCommandResults;
    procedure DoCommandResults(ACmd: integer; Res: TATCommandResults);
    function DoCommand_TextInsertAtCarets(const AText: atString; AKeepCaret,
      AOvrMode, ASelectThen, AInsertAtLineStarts: boolean): TATCommandResults;
    function DoCommand_ColumnSelectWithoutKey(AValue: boolean): TATCommandResults;
    function DoCommand_FoldLevel(ALevel: integer): TATCommandResults;
    function DoCommand_FoldAll: TATCommandResults;
    function DoCommand_FoldUnAll: TATCommandResults;
    function DoCommand_FoldRangeAtCurLine(ACommand: TATEditorFoldRangeCommand): TATCommandResults;
    function DoCommand_FoldSelection: TATCommandResults;
    function DoCommand_TextTrimSpaces(AMode: TATTrimSpaces): TATCommandResults;
    function DoCommand_TextChangeCase(AMode: TATEditorCaseConvert): TATCommandResults;
    function DoCommand_ScaleDelta(AIncrease: boolean): TATCommandResults;
    function DoCommand_ScaleReset: TATCommandResults;
    function DoCommand_MoveSelectionUpDown(ADown: boolean): TATCommandResults;
    function DoCommand_TextInsertEmptyAboveBelow(ADown: boolean): TATCommandResults;
    function DoCommand_SelectColumnToDirection(ADir: TATEditorSelectColumnDirection): TATCommandResults;
    function DoCommand_SelectColumnToLineEdge(AToEnd: boolean): TATCommandResults;
    function DoCommand_SelectFoldingRangeAtCaret: TATCommandResults;
    function DoCommand_RemoveOneCaret(AFirstCaret: boolean): TATCommandResults;
    function DoCommand_TextInsertColumnBlockOnce(const AText: string; AKeepCaret: boolean): TATCommandResults;
    function DoCommand_CaretsExtend(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_UndoRedo(AUndo: boolean): TATCommandResults;
    function DoCommand_TextIndentUnindent(ARight: boolean): TATCommandResults;
    function DoCommand_TextIndentUnindent_StreamBlock(ARight: boolean): TATCommandResults;
    procedure DoCommand_TextIndentUnindent_StreamBlock_OneCaret(ARight: boolean;
      Caret: TATCaretItem; out ATextChanged: boolean);
    function DoCommand_TextIndentUnindent_ColumnBlock(ARight: boolean): TATCommandResults;
    function DoCommand_SelectWords: TATCommandResults;
    function DoCommand_SelectLines: TATCommandResults;
    function DoCommand_SelectAll: TATCommandResults;
    function DoCommand_SelectInverted: TATCommandResults;
    function DoCommand_SelectSplitToLines: TATCommandResults;
    function DoCommand_SelectExtendByLine(AUp: boolean): TATCommandResults;
    function DoCommand_Cancel(AKeepLastCaret, AKeepSelection: boolean): TATCommandResults;
    function DoCommand_CancelKeepFirstVisible: TATCommandResults;
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
    function DoCommand_ScrollToBeginOrEnd(AToBegin: boolean; AKeepCaretOnScreen: boolean): TATCommandResults;
    function DoCommand_ScrollByDelta(ALines, AColumns: integer; AKeepCaretOnScreen: boolean): TATCommandResults;
    function DoCommand_ScrollToLeft: TATCommandResults;
    function DoCommand_TextInsertTabSpacesAtCarets(AOvrMode: boolean): TATCommandResults;
    function DoCommand_TextTabulation: TATCommandResults;
    function DoCommand_TextInsertTabSmart: TATCommandResults;
    function DoCommand_KeyHome: TATCommandResults;
    function DoCommand_KeyEnd: TATCommandResults;
    function DoCommand_KeyLeft(ASelCommand: boolean): TATCommandResults;
    function DoCommand_KeyRight(ASelCommand: boolean): TATCommandResults;
    function DoCommand_KeyUpDown(ADown: boolean; ALines: integer; AKeepRelativePos, AWithSel: boolean): TATCommandResults;
    function DoCommand_KeyUpDown_NextLine(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_KeyUpDown_Wrapped(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_TextBackspace: TATCommandResults;
    function DoCommand_TextDelete: TATCommandResults;
    function DoCommand_TextDeleteSelection(ASkipTouchingSel: boolean=false): TATCommandResults;
    function DoCommand_TextDeleteLeft(ADeleteLen: integer; AAllowUnindent: boolean): TATCommandResults;
    function DoCommand_TextDeleteRight(ADeleteLen: integer): TATCommandResults;
    function DoCommand_TextInsertEol(AKeepCaret: boolean): TATCommandResults;
    function DoCommand_ForceFinalEndOfLine: TATCommandResults;
    function DoCommand_DeleteFinalEndOfLine: TATCommandResults;
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
      AClipboardHasColumnBlock: boolean; const AClipboardText: string): TATCommandResults;
    function DoCommand_ClipboardPasteColumnBlock(AKeepCaret: boolean;
      const AClipboardText: string): TATCommandResults;
    function DoCommand_ClipboardPasteAndIndent: TATCommandResults;
    function DoCommand_ClipboardPasteFromRecents: TATCommandResults;
    function DoCommand_ClipboardClearRecents: TATCommandResults;
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
    function DoMouseWheelAction(Shift: TShiftState; AWheelDelta: integer;
      AForceHorz: boolean): boolean;
    property MouseNiceScroll: boolean read GetMouseNiceScroll write SetMouseNiceScroll;
    property ShowOsBarVert: boolean read FShowOsBarVert write SetShowOsBarVert;
    property ShowOsBarHorz: boolean read FShowOsBarHorz write SetShowOsBarHorz;
    procedure InvalidateEx(AForceRepaint, AForceOnScroll: boolean);

  public
    Colors: TATEditorColors;
    TagString: string; //to store plugin specific data in CudaText
    ModifiedOptions: TATEditorModifiedOptions;

    //overrides
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property ClientWidth: integer read FClientW;
    property ClientHeight: integer read FClientH;
    //updates
    procedure Invalidate; override;
    procedure Update(AUpdateWrapInfo: boolean=false; AForceRepaint: boolean=false; AForceOnScroll: boolean=false; AUpdateTempSel: boolean=false); reintroduce;
    procedure UpdateWrapInfo(AForceUpdate: boolean=false; AAllowCachedUpdate: boolean=true);
    procedure UpdateFoldedFromLinesHidden;
    procedure UpdateScrollInfoFromSmoothPos(var AInfo: TATEditorScrollInfo; const APos: Int64);
    function UpdateScrollbars(AdjustSmoothPos: boolean): boolean;
    procedure UpdateCaretsAndMarkersOnEditing(AFromCaret: integer; APos, APosEnd, AShift, APosAfter: TPoint);
    procedure UpdateMarkersOnDeleting(AX1, AY1, AX2, AY2: integer);
    //events
    procedure DoEventCarets; virtual;
    procedure DoEventScroll; virtual;
    procedure DoEventChange(ALineIndex: integer=-1; AllowOnChange: boolean=true); virtual;
    procedure DoEventChangeModified;
    procedure DoEventState; virtual;
    procedure DoEventZoom;
    procedure TimersStart;
    procedure TimersStop;
    //complex objects
    property Strings: TATStrings read GetStrings write SetStrings;
    property Fold: TATFoldRanges read FFold;
    property Carets: TATCarets read FCarets;
    property CaretsSel: TATCaretSelections read FSel;
    property CaretShowEnabled: boolean read FCaretShowEnabled write FCaretShowEnabled;
    property Markers: TATMarkers read GetMarkers;
    property Attribs: TATMarkers read GetAttribs;
    property Micromap: TATMicromap read FMicromap;
    property DimRanges: TATDimRanges read GetDimRanges;
    property Hotspots: TATHotspots read GetHotspots;
    property Gaps: TATGaps read GetGaps;
    property Keymap: TATKeymap read FKeymap write FKeymap;
    property CommandLog: TATCommandLog read FCommandLog;
    property MouseActions: TATEditorMouseActions read FMouseActions write FMouseActions;
    property TabHelper: TATStringTabHelper read FTabHelper;
    property WrapInfo: TATWrapInfo read FWrapInfo;
    //common
    property ScrollVert: TATEditorScrollInfo read FScrollVert write FScrollVert;
    property ScrollHorz: TATEditorScrollInfo read FScrollHorz write FScrollHorz;
    property ScrollbarVert: TATScrollbar read FScrollbarVert;
    property ScrollbarHorz: TATScrollbar read FScrollbarHorz;
    property ParentFrameObject: TCustomFrame read FParentFrameObject write FParentFrameObject; //for CudaText; not the same as editor's Parent
    property CaretShapeNormal: TATCaretShape read FCaretShapeNormal;
    property CaretShapeOverwrite: TATCaretShape read FCaretShapeOverwrite;
    property CaretShapeReadonly: TATCaretShape read FCaretShapeReadonly;
    function IsCaretOnVisibleRect: boolean;
    property ModifiedBookmarks: boolean read GetModifiedBookmarks write SetModifiedBookmarks;
    property FontProportional: boolean read FFontProportional;
    property EncodingName: string read GetEncodingName write SetEncodingName;
    property Modified: boolean read GetModified write SetModified;
    property AdapterForHilite: TATAdapterHilite read FAdapterHilite write FAdapterHilite;
    property AdapterIME: TATAdapterIME read FAdapterIME write FAdapterIME;
    property EditorIndex: integer read FEditorIndex write SetEditorIndex;
    property ActivationTime: QWord read FActivationTime write FActivationTime;
    property LineTop: integer read GetLineTop write SetLineTop;
    property LineTopTodo: integer read FLineTopTodo write FLineTopTodo; //e.g. needed for CudaText to override prev value of FLineTopTodo which was set by LineTop:=N
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
    procedure DoStringsOnChangeEx(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: SizeInt);
    procedure ActionAddJumpToUndo;
    property Text: UnicodeString read GetText write SetText;
    property SelRect: TRect read FSelRect;
    function IsEmpty: boolean;
    function IsSelRectEmpty: boolean;
    function IsSelColumn: boolean;
    function IsPosSelected(AX, AY: integer): boolean;
    function IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
    function IsPosFolded(AX, AY: integer): boolean;
    function IsPosInVisibleArea(AX, AY: integer): boolean;
    function IsLineFolded(ALine: integer; ADetectPartialFold: boolean = false): boolean;
    function IsCharWord(ch: Widechar): boolean;
    function IsLexerNormal: boolean;
    function IsLexerNone: boolean;
    property TextCharSize: TATEditorCharSize read FCharSize;
    property RectMain: TRect read FRectMain;
    property RectGutter: TRect read FRectGutter;
    property RectMinimap: TRect read FRectMinimap;
    property RectMicromap: TRect read FRectMicromap;
    property RectRuler: TRect read FRectRuler;
    property OptTextOffsetLeft: integer read FOptTextOffsetLeft write FOptTextOffsetLeft;
    property OptTextOffsetTop: integer read GetOptTextOffsetTop write FOptTextOffsetTop;
    //gutter
    property Gutter: TATGutter read FGutter;
    property GutterDecor: TATGutterDecor read GetGutterDecor;
    property GutterDecorAlignment: TAlignment read FGutterDecorAlignment write FGutterDecorAlignment;
    //files
    property FileName: string read FFileName write FFileName;
    procedure LoadFromFile(const AFilename: string; AOptions: TATLoadStreamOptions); virtual;
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
    procedure DoCaretForceShow;
    function CaretPosToClientPos(const APos: TPoint): TATPoint;
    function ClientPosToCaretPos(APos: TATPoint;
      out ADetails: TATEditorPosDetails;
      AGapCoordAction: TATEditorGapCoordAction=TATEditorGapCoordAction.ToLineEnd;
      AColumnSelectionAllowsVirtualPos: boolean=false): TPoint;
    function IsLineWithCaret(ALine: integer; ADisableSelected: boolean=false): boolean;
    function OffsetToCaretPos(const APos: integer): TPoint;
    function CaretPosToOffset(const ACaret: TPoint): integer;
    function GetCaretsArray: TATPointPairArray;
    function GetMarkersArray: TATMarkerMarkerArray;
    function GetAttribsArray: TATMarkerAttribArray;
    procedure SetCaretsArray(const Ar: TATPointPairArray);
    procedure SetMarkersArray(const Ar: TATMarkerMarkerArray);
    procedure SetAttribsArray(const Ar: TATMarkerAttribArray);
    //goto
    function DoShowPos(const APos: TPoint;
      AIndentHorz, AIndentVert: integer;
      AllowUnfold,
      AllowUpdate,
      AllowProximity: boolean): boolean;
    procedure DoGotoPos(const APos, APosEnd: TPoint;
      AIndentHorz, AIndentVert: integer;
      APlaceCaret: boolean;
      ADoUnfold: TATEditorActionIfFolded;
      AAllowProcessMsg: boolean=true;
      AAllowUpdate: boolean=true;
      AAllowProximity: boolean=true);
    procedure DoGotoCaret(AEdge: TATCaretEdge;
      AUndoRedo: boolean=false;
      AAllowProcessMsg: boolean=true;
      AAllowUpdate: boolean=true;
      AAllowProximity: boolean=true);
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
    procedure BookmarkDeleteAll(AWithEvent: boolean=true);
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
    procedure DoRangeHideLines(ALineFrom, ALineTo: integer);
    procedure DoFoldForLevel(ALevel: integer);
    procedure DoFoldForLevelEx(ALevel: integer; AOuterRange: integer);
    function DoFoldUnfoldRangeAtCurLine(AOp: TATEditorFoldRangeCommand): boolean;
    property FoldingAsString: string read GetFoldingAsString write SetFoldingAsString;
    property FoldingAsStringTodo: string read FFoldingAsStringTodo write FFoldingAsStringTodo;
    procedure DoUnfoldLine(ALine: integer);
    procedure DoUnfoldLinesWithCarets;
    procedure DoMarkLinesVisible(ALineFrom, ALineTo: integer);
    //markers
    procedure MarkerClearAll;
    procedure MarkerDrop;
    procedure MarkerGotoLast(AndDelete: boolean; AIndentHorz, AIndentVert: integer);
    procedure MarkerSwap;
    procedure MarkerSelectToCaret;
    procedure MarkerDeleteToCaret;
    procedure AttribClearAll;
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
    procedure DoCommand(ACmd: integer; AInvoke: TATCommandInvoke; const AText: atString = ''); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure BeginEditing;
    procedure EndEditing(ATextChanged: boolean);
    procedure DoHideAllTooltips;
    function IsLocked: boolean;
    function IsIndentBasedFolding: boolean;
    function TextSelected: atString;
    function TextSelectedEx(ACaret: TATCaretItem): atString;
    function TextCurrentWord: atString;
    //LastCommandChangedLines: count of lines changed by last call of Strings.ActionTrimSpaces
    property LastCommandChangedLines: integer read GetLastCommandChangedLines write SetLastCommandChangedLines;
    property IsIniting: boolean read FIsIniting write FIsIniting;
    property IsRunningCommand: boolean read FIsRunningCommand;
    procedure DoSelect_All;
    procedure DoSelect_None;
    procedure DoSelect_Inverted;
    procedure DoSelect_SplitSelectionToLines;
    procedure DoSelect_Line(APos: TPoint);
    procedure DoSelect_CharGroupAtPos(P: TPoint; AddCaret, AllowOnlyWordChars: boolean);
    procedure DoSelect_LineRange(ALineFrom: integer; APosTo: TPoint);
    procedure DoSelect_LinesByFoldedMark(ALineFrom, ALineTo: integer);
    function DoSelect_FoldingRangeStartingAtLine(ACaret: TATCaretItem;
      ALine: integer; ACaretToEndOfSel: boolean): boolean;
    function DoSelect_FoldingRangeAtCaret: boolean;
    procedure DoSelect_ClearColumnBlock;
    procedure DoSelect_ColumnBlock_FromPoints(APosChar1, APosChar2: TPoint;
      AUpdateSelRectPoints: boolean=true);
    procedure DoSelect_ColumnBlock_FromPointsColumns(P1, P2: TPoint);
    procedure DoSelect_ColumnBlock_Primitive(ACaretPos1, ACaretPos2: TPoint);
    procedure DoScrollToBeginOrEnd(AToBegin: boolean);
    procedure DoScrollByDelta(ADeltaX, ADeltaY: integer);
    procedure DoScrollByDeltaInPixels(ADeltaX, ADeltaY: integer);
    procedure DoScaleFontDelta(AInc: boolean; AllowUpdate: boolean);
    function DoCalcLineHiliteEx(ALineIndex: integer; var AParts: TATLineParts;
      AColorBG: TColor; out AColorAfter: TColor; AWithSelection: boolean): boolean;
    procedure DoSetMarkedLines(ALine1, ALine2: integer);
    function DoGetMarkedLines(out ALine1, ALine2: integer): boolean;
    function DoGetLinkAtPos(AX, AY: integer): atString;
    function DoGetGapRect(AIndex: integer; out ARect: TRect): boolean;

  protected
    IsRepaintEnabled: boolean;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
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
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;

    {$ifdef windows}
    procedure WMIME_Request(var Msg: TMessage); message WM_IME_REQUEST;
    procedure WMIME_Notify(var Msg: TMessage); message WM_IME_NOTIFY;
    procedure WMIME_StartComposition(var Msg:TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMIME_Composition(var Msg:TMessage); message WM_IME_COMPOSITION;
    procedure WMIME_EndComposition(var Msg:TMessage); message WM_IME_ENDCOMPOSITION;
    {$endif}

    {$ifdef LCLGTK2}
    //{$ifdef GTK2_IME_CODE}
    procedure WM_GTK_IM_COMPOSITION(var Msg: TLMessage); message LM_IM_COMPOSITION;
    //{$endif}
    {$endif}

    {$ifdef LCLCOCOA}
    procedure COCOA_IM_COMPOSITION(var Message: TLMessage); message LM_IM_COMPOSITION;
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
    property OnChangeDetailed: TATSynEditChangeDetailedEvent read FOnChangeDetailed write FOnChangeDetailed;
    property OnChangeLog: TATStringsChangeLogEvent read FOnChangeLog write FOnChangeLog;
    property OnChangeModified: TNotifyEvent read FOnChangeModified write FOnChangeModified;
    property OnChangeCaretPos: TNotifyEvent read FOnChangeCaretPos write FOnChangeCaretPos;
    property OnChangeState: TNotifyEvent read FOnChangeState write FOnChangeState;
    property OnChangeZoom: TNotifyEvent read FOnChangeZoom write FOnChangeZoom;
    property OnChangeBookmarks: TNotifyEvent read FOnChangeBookmarks write FOnChangeBookmarks;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnCommand: TATSynEditCommandEvent read FOnCommand write FOnCommand;
    property OnCommandAfter: TATSynEditCommandAfterEvent read FOnCommandAfter write FOnCommandAfter;
    property OnCommandKeyEnter: TNotifyEvent read FOnCommandKeyEnter write FOnCommandKeyEnter;
    property OnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent read FOnDrawBookmarkIcon write FOnDrawBookmarkIcon;
    property OnDrawLine: TATSynEditDrawLineEvent read FOnDrawLine write FOnDrawLine;
    property OnDrawGap: TATSynEditDrawGapEvent read FOnDrawGap write FOnDrawGap;
    property OnDrawMicromap: TATSynEditDrawRectEvent read FOnDrawMicromap write FOnDrawMicromap;
    property OnDrawEditor: TATSynEditDrawRectEvent read FOnDrawEditor write FOnDrawEditor;
    property OnDrawRuler: TATSynEditDrawRulerEvent read FOnDrawRuler write FOnDrawRuler;
    property OnCalcCaretsCoords: TNotifyEvent read FOnCalcCaretsCoords write FOnCalcCaretsCoords;
    property OnCalcHilite: TATSynEditCalcHiliteEvent read FOnCalcHilite write FOnCalcHilite;
    property OnCalcStaple: TATSynEditCalcStapleEvent read FOnCalcStaple write FOnCalcStaple;
    property OnCalcTabSize: TATStringTabCalcEvent read FOnCalcTabSize write FOnCalcTabSize;
    property OnCalcBookmarkColor: TATSynEditCalcBookmarkColorEvent read FOnCalcBookmarkColor write FOnCalcBookmarkColor;
    property OnBeforeCalcHilite: TATSynEditBeforeOrAfterHiliteEvent read FOnBeforeCalcHilite write FOnBeforeCalcHilite;
    property OnAfterCalcHilite: TATSynEditBeforeOrAfterHiliteEvent read FOnAfterCalcHilite write FOnAfterCalcHilite;
    property OnPaint;
    property OnPaste: TATSynEditPasteEvent read FOnPaste write FOnPaste;
    property OnHotspotEnter: TATSynEditHotspotEvent read FOnHotspotEnter write FOnHotspotEnter;
    property OnHotspotExit: TATSynEditHotspotEvent read FOnHotspotExit write FOnHotspotExit;
    property OnGetToken: TATSynEditGetTokenEvent read FOnGetToken write FOnGetToken;
    property OnEnabledUndoRedoChanged: TATSynEditEnabledUndoRedoChanged read FOnEnabledUndoRedoChanged write FOnEnabledUndoRedoChanged;

    //misc
    property CursorText: TCursor read FCursorText write FCursorText default crIBeam;
    property CursorColumnSel: TCursor read FCursorColumnSel write FCursorColumnSel default crCross;
    property CursorGutterBookmark: TCursor read FCursorGutterBookmark write FCursorGutterBookmark default crHandPoint;
    property CursorGutterNumbers: TCursor read FCursorGutterNumbers write FCursorGutterNumbers default crDefault;
    property CursorMinimap: TCursor read FCursorMinimap write FCursorMinimap default crDefault;
    property CursorMicromap: TCursor read FCursorMicromap write FCursorMicromap default crDefault;
    property ImagesGutterDecor: TImageList read FGutterDecorImages write FGutterDecorImages;
    property WantTabs: boolean read FWantTabs write FWantTabs default true;
    property WantReturns: boolean read FWantReturns write FWantReturns default true;

    //options
    property OptThemed: boolean read FOptThemed write FOptThemed default false;
    property OptAutoPairForMultiCarets: boolean read FOptAutoPairForMultiCarets write FOptAutoPairForMultiCarets default cInitAutoPairForMultiCarets;
    property OptAutoPairChars: string read FOptAutoPairChars write FOptAutoPairChars stored false;
    property OptAutoPair_DisableCharDoubling: boolean read FOptAutoPair_DisableCharDoubling write FOptAutoPair_DisableCharDoubling default false;
    property OptAutocompleteSymbolsAllowedBeforeCaret: string read FOptAutocompleteSymbolsAllowedBeforeCaret write FOptAutocompleteSymbolsAllowedBeforeCaret stored false;
    property OptAutocompleteAutoshowCharCount: integer read FOptAutocompleteAutoshowCharCount write FOptAutocompleteAutoshowCharCount default 0;
    property OptAutocompleteTriggerChars: string read FOptAutocompleteTriggerChars write FOptAutocompleteTriggerChars stored false;
    property OptAutocompleteCommitChars: string read FOptAutocompleteCommitChars write FOptAutocompleteCommitChars stored false;
    property OptAutocompleteCommitOnEnter: boolean read FOptAutocompleteCommitOnEnter write FOptAutocompleteCommitOnEnter stored false;
    property OptAutocompleteCloseChars: string read FOptAutocompleteCloseChars write FOptAutocompleteCloseChars stored false;
    property OptAutocompleteAddOpeningBracket: boolean read FOptAutocompleteAddOpeningBracket write FOptAutocompleteAddOpeningBracket default true;
    property OptAutocompleteUpDownAtEdge: integer read FOptAutocompleteUpDownAtEdge write FOptAutocompleteUpDownAtEdge default 1;
    property OptAutocompleteCommitIfSingleItem: boolean read FOptAutocompleteCommitIfSingleItem write FOptAutocompleteCommitIfSingleItem default false;

    property OptForceSeparateCharSizer: boolean read FOptForceSeparateCharSizer write FOptForceSeparateCharSizer default false;
    property OptGapBitmapAlignment: TAlignment read FOptGapBitmapAlignment write FOptGapBitmapAlignment default cInitGapBitmapAlignment;
    property OptGapBitmapIndent: integer read FOptGapBitmapIndent write FOptGapBitmapIndent default cInitGapBitmapIndent;
    property OptFlickerReducingPause: integer read FOptFlickerReducingPause write FOptFlickerReducingPause default 0;
    property OptInputNumberOnly: boolean read FOptInputNumberOnly write FOptInputNumberOnly default false;
    property OptInputNumberAllowNegative: boolean read FOptInputNumberAllowNegative write FOptInputNumberAllowNegative default cInitInputNumberAllowNegative;
    property OptMaskChar: WideChar read FOptMaskChar write FOptMaskChar default cInitMaskChar;
    property OptMaskCharUsed: boolean read FOptMaskCharUsed write FOptMaskCharUsed default false;
    property OptScrollAnimationSteps: integer read FOptScrollAnimationSteps write FOptScrollAnimationSteps default cInitScrollAnimationSteps;
    property OptScrollAnimationSleep: integer read FOptScrollAnimationSleep write FOptScrollAnimationSleep default cInitScrollAnimationSleep;
    property OptScaleFont: integer read FOptScaleFont write SetOptScaleFont default 0;
    property OptIdleInterval: integer read FOptIdleInterval write FOptIdleInterval default cInitIdleInterval;
    property OptTabSpaces: boolean read FOptTabSpaces write SetTabSpaces default false;
    property OptTabSize: integer read FOptTabSize write SetTabSize default cInitTabSize;
    property OptTabSmart: boolean read FOptTabSmart write FOptTabSmart default false;
    property OptNonWordChars: atString read FOptNonWordChars write FOptNonWordChars stored false;
    property OptFoldStyle: TATEditorFoldStyle read FFoldStyle write FFoldStyle default cInitFoldStyle;
    property OptFoldEnabled: boolean read FFoldEnabled write SetFoldEnabled default true;
    property OptFoldUnderlineOffset: integer read FFoldUnderlineOffset write FFoldUnderlineOffset default cInitFoldUnderlineOffset;
    property OptFoldTooltipVisible: boolean read FFoldTooltipVisible write FFoldTooltipVisible default cInitFoldTooltipVisible;
    property OldFoldTooltipWidthPercents: integer read FFoldTooltipWidthPercents write FFoldTooltipWidthPercents default cInitFoldTooltipWidthPercents;
    property OptFoldTooltipLineCount: integer read FFoldTooltipLineCount write FFoldTooltipLineCount default cInitFoldTooltipLineCount;
    property OptFoldIconForMinimalRangeHeight: integer read FFoldIconForMinimalRange write FFoldIconForMinimalRange default 0;
    property OptTextHint: string read FTextHint write FTextHint;
    property OptTextHintFontStyle: TFontStyles read FTextHintFontStyle write FTextHintFontStyle default [fsItalic];
    property OptTextHintCenter: boolean read FTextHintCenter write FTextHintCenter default false;
    property OptTextDuplicationMovesCaretDown: boolean read FOptTextDuplicationMovesCaretDown write FOptTextDuplicationMovesCaretDown default true;
    property OptTextCenteringCharWidth: integer read FOptTextCenteringCharWidth write FOptTextCenteringCharWidth default 0;
    property OptAutoIndent: boolean read FOptAutoIndent write FOptAutoIndent default true;
    property OptAutoIndentKind: TATEditorAutoIndentKind read FOptAutoIndentKind write FOptAutoIndentKind default TATEditorAutoIndentKind.AsPrevLine;
    property OptAutoIndentBetterBracketsCurly: boolean read FOptAutoIndentBetterBracketsCurly write FOptAutoIndentBetterBracketsCurly default true;
    property OptAutoIndentBetterBracketsRound: boolean read FOptAutoIndentBetterBracketsRound write FOptAutoIndentBetterBracketsRound default false;
    property OptAutoIndentBetterBracketsSquare: boolean read FOptAutoIndentBetterBracketsSquare write FOptAutoIndentBetterBracketsSquare default true;
    property OptAutoIndentRegexRule: string read FOptAutoIndentRegexRule write FOptAutoIndentRegexRule;
    property OptCopyLinesIfNoSel: boolean read FOptCopyLinesIfNoSel write FOptCopyLinesIfNoSel default true;
    property OptCutLinesIfNoSel: boolean read FOptCutLinesIfNoSel write FOptCutLinesIfNoSel default false;
    property OptCopyColumnBlockAlignedBySpaces: boolean read FOptCopyColumnBlockAlignedBySpaces write FOptCopyColumnBlockAlignedBySpaces default true;
    property OptLastLineOnTop: boolean read FOptLastLineOnTop write FOptLastLineOnTop default false;
    property OptOverwriteSel: boolean read FOptOverwriteSel write FOptOverwriteSel default true;
    property OptOverwriteAllowedOnPaste: boolean read FOptOverwriteAllowedOnPaste write FOptOverwriteAllowedOnPaste default false;
    property OptScrollStyleHorz: TATEditorScrollbarStyle read FOptScrollStyleHorz write FOptScrollStyleHorz default TATEditorScrollbarStyle.Auto;
    property OptScrollStyleVert: TATEditorScrollbarStyle read FOptScrollStyleVert write FOptScrollStyleVert default TATEditorScrollbarStyle.Show;
    property OptScrollSmooth: boolean read FOptScrollSmooth write FOptScrollSmooth default true;
    property OptScrollIndentCaretHorz: integer read FOptScrollIndentCaretHorz write FOptScrollIndentCaretHorz default 10;
    property OptScrollIndentCaretVert: integer read FOptScrollIndentCaretVert write FOptScrollIndentCaretVert default 0;
    property OptScrollbarsNew: boolean read FOptScrollbarsNew write FOptScrollbarsNew default false;
    property OptScrollbarHorizontalAddSpace: integer read FOptScrollbarHorizontalAddSpace write FOptScrollbarHorizontalAddSpace default cInitScrollbarHorzAddSpace;
    property OptScrollLineCommandsKeepCaretOnScreen: boolean read FOptScrollLineCommandsKeepCaretOnScreen write FOptScrollLineCommandsKeepCaretOnScreen default true;

    property OptShowFontLigatures: boolean read FOptShowFontLigatures write FOptShowFontLigatures default true;
    property OptShowURLs: boolean read FOptShowURLs write FOptShowURLs default true;
    property OptShowURLsRegex: string read FOptShowURLsRegex write SetOptShowURLsRegex stored false;
    property OptShowDragDropMarker: boolean read FOptShowDragDropMarker write FOptShowDragDropMarker default true;
    property OptShowDragDropMarkerWidth: integer read FOptShowDragDropMarkerWidth write FOptShowDragDropMarkerWidth default cInitDragDropMarkerWidth;
    property OptShowFoldedMarkWithSelectionBG: boolean read FOptShowFoldedMarkWithSelectionBG write FOptShowFoldedMarkWithSelectionBG default cInitShowFoldedMarkWithSelectionBG;
    property OptMinLineLenToCalcURL: integer read FOptMinLineLenToCalcURL write FOptMinLineLenToCalcURL default cInitMinLineLenToCalcURL;
    property OptMaxLineLenToCalcURL: integer read FOptMaxLineLenToCalcURL write FOptMaxLineLenToCalcURL default cInitMaxLineLenToCalcURL;
    property OptMaxLinesToCountUnindent: integer read FOptMaxLinesToCountUnindent write FOptMaxLinesToCountUnindent default 100;
    property OptStapleStyle: TATLineStyle read FOptStapleStyle write FOptStapleStyle default TATLineStyle.Solid;
    property OptStapleIndent: integer read FOptStapleIndent write FOptStapleIndent default -1;
    property OptStapleWidthPercent: integer read FOptStapleWidthPercent write FOptStapleWidthPercent default 100;
    property OptStapleHiliteActive: boolean read FOptStapleHiliteActive write FOptStapleHiliteActive default true;
    property OptStapleHiliteActiveAlpha: integer read FOptStapleHiliteActiveAlpha write FOptStapleHiliteActiveAlpha default cInitStapleHiliteAlpha;
    property OptStapleEdge1: TATEditorStapleEdge read FOptStapleEdge1 write FOptStapleEdge1 default TATEditorStapleEdge.Angle;
    property OptStapleEdge2: TATEditorStapleEdge read FOptStapleEdge2 write FOptStapleEdge2 default TATEditorStapleEdge.Angle;
    property OptStapleIndentConsidersEnd: boolean read FOptStapleIndentConsidersEnd write FOptStapleIndentConsidersEnd default false;
    property OptShowFullWidthForSelection: boolean read FOptShowFullSel write FOptShowFullSel default false;
    property OptShowFullWidthForSyntaxHilite: boolean read FOptShowFullHilite write FOptShowFullHilite default true;
    property OptShowCurLine: boolean read FOptShowCurLine write FOptShowCurLine default false;
    property OptShowCurLineMinimal: boolean read FOptShowCurLineMinimal write FOptShowCurLineMinimal default true;
    property OptShowCurLineOnlyFocused: boolean read FOptShowCurLineOnlyFocused write FOptShowCurLineOnlyFocused default false;
    property OptShowCurLineIfWithoutSel: boolean read FOptShowCurLineIfWithoutSel write FOptShowCurLineIfWithoutSel default true;
    property OptShowCurColumn: boolean read FOptShowCurColumn write FOptShowCurColumn default false;
    property OptKeepSelFontColor: boolean read FOptKeepSelFontColor write FOptKeepSelFontColor default false;
    property OptShowScrollHint: boolean read FOptShowScrollHint write FOptShowScrollHint default false;
    property OptShowMouseSelFrame: boolean read FOptShowMouseSelFrame write FOptShowMouseSelFrame default cInitShowMouseSelFrame;
    property OptCaretManyAllowed: boolean read GetCaretManyAllowed write SetCaretManyAllowed default true;
    property OptCaretVirtual: boolean read FCaretVirtual write FCaretVirtual default true;
    property OptCaretBlinkTime: integer read FCaretBlinkTime write SetCaretBlinkTime default cInitCaretBlinkTime;
    property OptCaretBlinkEnabled: boolean read FCaretBlinkEnabled write SetCaretBlinkEnabled default true;
    property OptCaretStopUnfocused: boolean read FCaretStopUnfocused write FCaretStopUnfocused default true;
    property OptCaretHideUnfocused: boolean read FCaretHideUnfocused write FCaretHideUnfocused default true;
    property OptCaretPreferLeftSide: boolean read FOptCaretPreferLeftSide write FOptCaretPreferLeftSide default true;
    property OptCaretPosAfterPasteColumn: TATEditorPasteCaret read FOptCaretPosAfterPasteColumn write FOptCaretPosAfterPasteColumn default TATEditorPasteCaret.ColumnRight;
    property OptCaretsPrimitiveColumnSelection: boolean read FOptCaretsPrimitiveColumnSelection write FOptCaretsPrimitiveColumnSelection default cInitCaretsPrimitiveColumnSelection;
    property OptCaretsAddedToColumnSelection: boolean read FOptCaretsAddedToColumnSelection write FOptCaretsAddedToColumnSelection default true;
    property OptCaretFixAfterRangeFolded: boolean read FOptCaretFixAfterRangeFolded write FOptCaretFixAfterRangeFolded default true;
    property OptCaretsMultiToColumnSel: boolean read FOptCaretsMultiToColumnSel write FOptCaretsMultiToColumnSel default cInitCaretsMultiToColumnSel;
    property OptCaretProximityVert: integer read FOptCaretProximityVert write FOptCaretProximityVert default 0;
    property OptMarkersSize: integer read FOptMarkersSize write FOptMarkersSize default cInitMarkerSize;
    property OptGutterVisible: boolean read FOptGutterVisible write FOptGutterVisible default true;
    property OptGutterPlusSize: integer read FOptGutterPlusSize write FOptGutterPlusSize default cInitGutterPlusSize;
    property OptGutterShowFoldAlways: boolean read FOptGutterShowFoldAlways write FOptGutterShowFoldAlways default true;
    property OptGutterShowFoldLines: boolean read FOptGutterShowFoldLines write FOptGutterShowFoldLines default true;
    property OptGutterShowFoldLinesAll: boolean read FOptGutterShowFoldLinesAll write FOptGutterShowFoldLinesAll default false;
    property OptGutterShowFoldLinesForCaret: boolean read FOptGutterShowFoldLinesForCaret write FOptGutterShowFoldLinesForCaret default true;
    property OptGutterShowBracketDecor: boolean read FOptGutterShowBracketDecor write FOptGutterShowBracketDecor default true;
    property OptGutterWidthBookmarks: integer read FOptGutterWidthBookmarks write FOptGutterWidthBookmarks default cInitGutterWidthBookmarks;
    property OptGutterWidthNumbers: integer read FOptGutterWidthNumbers write FOptGutterWidthNumbers default cInitGutterWidthNumbers;
    property OptGutterWidthFolding: integer read FOptGutterWidthFolding write FOptGutterWidthFolding default cInitGutterWidthFolding;
    property OptGutterWidthSeparator: integer read FOptGutterWidthSeparator write FOptGutterWidthSeparator default cInitGutterWidthSeparator;
    property OptGutterWidthEmpty: integer read FOptGutterWidthEmpty write FOptGutterWidthEmpty default cInitGutterWidthEmpty;
    property OptGutterWidthLineStates: integer read FOptGutterWidthLineStates write FOptGutterWidthLineStates default cInitGutterWidthLineStates;
    property OptGutterIcons: TATEditorGutterIcons read FOptGutterIcons write FOptGutterIcons default TATEditorGutterIcons.PlusMinus;
    property OptBorderVisible: boolean read FOptBorderVisible write FOptBorderVisible default cInitBorderVisible;
    property OptBorderWidth: integer read FOptBorderWidth write FOptBorderWidth default cInitBorderWidth;
    property OptBorderWidthFocused: integer read FOptBorderWidthFocused write FOptBorderWidthFocused default cInitBorderWidthFocused;
    property OptBorderWidthWithColor: integer read FOptBorderWidthWithColor write FOptBorderWidthWithColor default cInitBorderWidthWithColor;
    property OptBorderRounded: boolean read FOptBorderRounded write FOptBorderRounded default false;
    property OptBorderFocusedActive: boolean read FOptBorderFocusedActive write FOptBorderFocusedActive default false;
    property OptBorderColor: TColor read FOptBorderColor write FOptBorderColor default clNone;
    property OptCornerFontName: string read FOptCornerFontName write FOptCornerFontName;
    property OptCornerFontSize: integer read FOptCornerFontSize write FOptCornerFontSize default 0;
    property OptCornerText: string read FOptCornerText write FOptCornerText;
    property OptCornerColorFont: TColor read FOptCornerColorFont write FOptCornerColorFont default clBlack;
    property OptCornerColorBack: TColor read FOptCornerColorBack write FOptCornerColorBack default clWhite;
    property OptCornerColorBorder: TColor read FOptCornerColorBorder write FOptCornerColorBorder default clNone;
    property OptCorner2FontName: string read FOptCorner2FontName write FOptCorner2FontName;
    property OptCorner2FontSize: integer read FOptCorner2FontSize write FOptCorner2FontSize default 0;
    property OptCorner2Text: string read FOptCorner2Text write FOptCorner2Text;
    property OptCorner2ColorFont: TColor read FOptCorner2ColorFont write FOptCorner2ColorFont default clBlack;
    property OptCorner2ColorBack: TColor read FOptCorner2ColorBack write FOptCorner2ColorBack default clWhite;
    property OptCorner2ColorBorder: TColor read FOptCorner2ColorBorder write FOptCorner2ColorBorder default clNone;
    property OptRulerVisible: boolean read FOptRulerVisible write FOptRulerVisible default true;
    property OptRulerNumeration: TATEditorRulerNumeration read FOptRulerNumeration write FOptRulerNumeration default cInitRulerNumeration;
    property OptRulerHeightPercents: integer read FOptRulerHeightPercents write FOptRulerHeightPercents default cInitRulerHeightPercents;
    property OptRulerFontSizePercents: integer read FOptRulerFontSizePercents write FOptRulerFontSizePercents default cInitRulerFontSizePercents;
    property OptRulerMarkSizeCaret: integer read FOptRulerMarkSizeCaret write FOptRulerMarkSizeCaret default cInitRulerMarkCaret;
    property OptRulerMarkSizeSmall: integer read FOptRulerMarkSizeSmall write FOptRulerMarkSizeSmall default cInitRulerMarkSmall;
    property OptRulerMarkSizeBig: integer read FOptRulerMarkSizeBig write FOptRulerMarkSizeBig default cInitRulerMarkBig;
    property OptRulerMarkForAllCarets: boolean read FOptRulerMarkForAllCarets write FOptRulerMarkForAllCarets default false;
    property OptRulerTopIndentPercents: integer read FOptRulerTopIndentPercents write FOptRulerTopIndentPercents default 0;
    property OptRulerText: string read FOptRulerText write FOptRulerText;
    property OptMinimapCustomScale: integer read FMinimapCustomScale write FMinimapCustomScale default 0;
    property OptMinimapVisible: boolean read FMinimapVisible write SetMinimapVisible default cInitMinimapVisible;
    property OptMinimapCharWidth: integer read FMinimapCharWidth write FMinimapCharWidth default 0;
    property OptMinimapShowSelBorder: boolean read FMinimapShowSelBorder write FMinimapShowSelBorder default false;
    property OptMinimapShowSelAlways: boolean read FMinimapShowSelAlways write FMinimapShowSelAlways default true;
    property OptMinimapSelColorChange: integer read FMinimapSelColorChange write FMinimapSelColorChange default cInitMinimapSelColorChange;
    property OptMinimapAtLeft: boolean read FMinimapAtLeft write FMinimapAtLeft default false;
    property OptMinimapTooltipVisible: boolean read FMinimapTooltipVisible write FMinimapTooltipVisible default cInitMinimapTooltipVisible;
    property OptMinimapTooltipHeight: integer read FMinimapTooltipHeight write FMinimapTooltipHeight default cInitMinimapTooltipHeight;
    property OptMinimapTooltipWidthPercents: integer read FMinimapTooltipWidthPercents write FMinimapTooltipWidthPercents default cInitMinimapTooltipWidthPercents;
    property OptMinimapTooltipFontSize: integer read FMinimapTooltipFontSize write FMinimapTooltipFontSize default 0;
    property OptMinimapHiliteLinesWithSelection: boolean read FMinimapHiliteLinesWithSelection write FMinimapHiliteLinesWithSelection default true;
    property OptMinimapDragImmediately: boolean read FMinimapDragImmediately write FMinimapDragImmediately default false;
    property OptMicromapVisible: boolean read FMicromapVisible write SetMicromapVisible default cInitMicromapVisible;
    property OptMicromapScalePerColumn: integer read FMicromapScalePerColumn write FMicromapScalePerColumn default cInitMicromapScalePerColumn;
    property OptMicromapOnScrollbar: boolean read FMicromapOnScrollbar write FMicromapOnScrollbar default cInitMicromapOnScrollbar;
    property OptMicromapLineStates: boolean read FMicromapLineStates write FMicromapLineStates default true;
    property OptMicromapSelections: boolean read FMicromapSelections write FMicromapSelections default true;
    property OptMicromapBookmarks: boolean read FMicromapBookmarks write FMicromapBookmarks default cInitMicromapBookmarks;
    property OptMicromapShowForMinCount: integer read FMicromapShowForMinCount write FMicromapShowForMinCount default cInitMicromapShowForMinCount;
    property OptSpacingTop: integer read FSpacingTop write SetSpacingTop default cInitSpacingTop;
    property OptSpacingBottom: integer read FSpacingBottom write SetSpacingBottom default cInitSpacingBottom;
    property OptWrapMode: TATEditorWrapMode read FWrapMode write SetWrapMode default cInitWrapMode;
    property OptWrapIndented: boolean read FWrapIndented write SetWrapIndented default true;
    property OptWrapAddSpace: integer read FWrapAddSpace write FWrapAddSpace default 1;
    property OptWrapEnabledForMaxLines: integer read FWrapEnabledForMaxLines write FWrapEnabledForMaxLines default cInitWrapEnabledForMaxLines;
    property OptMarginRight: integer read FMarginRight write SetMarginRight default cInitMarginRight;
    property OptMarginString: string read GetMarginString write SetMarginString;
    property OptNumbersAutosize: boolean read FOptNumbersAutosize write FOptNumbersAutosize default true;
    property OptNumbersAlignment: TAlignment read FOptNumbersAlignment write FOptNumbersAlignment default taRightJustify;
    property OptNumbersStyle: TATEditorNumbersStyle read FOptNumbersStyle write FOptNumbersStyle default cInitNumbersStyle;
    property OptNumbersShowFirst: boolean read FOptNumbersShowFirst write FOptNumbersShowFirst default true;
    property OptNumbersShowCarets: boolean read FOptNumbersShowCarets write FOptNumbersShowCarets default false;
    property OptNumbersIndentPercents: integer read FOptNumbersIndentPercents write FOptNumbersIndentPercents default cInitNumbersIndentPercents;
    property OptUnprintedVisible: boolean read FUnprintedVisible write FUnprintedVisible default true;
    property OptUnprintedSpaces: boolean read FUnprintedSpaces write FUnprintedSpaces default true;
    property OptUnprintedSpacesTrailing: boolean read FUnprintedSpacesTrailing write FUnprintedSpacesTrailing default false;
    property OptUnprintedSpacesBothEnds: boolean read FUnprintedSpacesBothEnds write FUnprintedSpacesBothEnds default false;
    property OptUnprintedSpacesOnlyInSelection: boolean read FUnprintedSpacesOnlyInSelection write FUnprintedSpacesOnlyInSelection default false;
    property OptUnprintedSpacesAlsoInSelection: boolean read FUnprintedSpacesAlsoInSelection write FUnprintedSpacesAlsoInSelection default false;
    property OptUnprintedForceTabs: boolean read FUnprintedForceTabs write FUnprintedForceTabs default false;
    property OptUnprintedEnds: boolean read FUnprintedEnds write FUnprintedEnds default true;
    property OptUnprintedEndsDetails: boolean read FUnprintedEndsDetails write FUnprintedEndsDetails default true;
    property OptUnprintedWraps: boolean read FUnprintedWraps write FUnprintedWraps default false;
    property OptUnprintedEof: boolean read FUnprintedEof write FUnprintedEof default true;
    property OptMouseEnableAll: boolean read FOptMouseEnableAll write FOptMouseEnableAll default true;
    property OptMouseEnableNormalSelection: boolean read FOptMouseEnableNormalSelection write FOptMouseEnableNormalSelection default true;
    property OptMouseEnableColumnSelection: boolean read FOptMouseEnableColumnSelection write FOptMouseEnableColumnSelection default true;
    property OptMouseHideCursorOnType: boolean read FOptMouseHideCursor write FOptMouseHideCursor default false;
    property OptMouseClickOpensURL: boolean read FOptMouseClickOpensURL write FOptMouseClickOpensURL default false;
    property OptMouseClickNumberSelectsLine: boolean read FOptMouseClickNumberSelectsLine write FOptMouseClickNumberSelectsLine default true;
    property OptMouseClickNumberSelectsLineWithEOL: boolean read FOptMouseClickNumberSelectsLineWithEOL write FOptMouseClickNumberSelectsLineWithEOL default true;
    property OptMouseClickNumberSelectsFoldedRange: boolean read FOptMouseClickNumberSelectsFoldedRange write FOptMouseClickNumberSelectsFoldedRange default true;
    property OptMouse2ClickAction: TATEditorDoubleClickAction read FOptMouse2ClickAction write FOptMouse2ClickAction default TATEditorDoubleClickAction.SelectAnyChars;
    property OptMouse2ClickOpensURL: boolean read FOptMouse2ClickOpensURL write FOptMouse2ClickOpensURL default true;
    property OptMouse2ClickOnFoldMarkSelectsFoldedLines: boolean read FOptMouse2ClickOnFoldMarkSelectsFoldedLines write FOptMouse2ClickOnFoldMarkSelectsFoldedLines default true;
    property OptMouse2ClickDragSelectsWords: boolean read FOptMouse2ClickDragSelectsWords write FOptMouse2ClickDragSelectsWords default true;
    property OptMouse3ClickSelectsLine: boolean read FOptMouse3ClickSelectsLine write FOptMouse3ClickSelectsLine default true;
    property OptMouseDragDrop: boolean read FOptMouseDragDrop write FOptMouseDragDrop default true;
    property OptMouseDragDropCopying: boolean read FOptMouseDragDropCopying write FOptMouseDragDropCopying default true;
    property OptMouseDragDropCopyingWithState: TShiftStateEnum read FOptMouseDragDropCopyingWithState write FOptMouseDragDropCopyingWithState default ssModifier;
    property OptMouseMiddleClickAction: TATEditorMiddleClickAction read FOptMouseMiddleClickAction write FOptMouseMiddleClickAction default TATEditorMiddleClickAction.Scrolling;
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
    property OptKeyUpDownAllowToEdge: boolean read FOptKeyUpDownAllowToEdge write FOptKeyUpDownAllowToEdge default false;
    property OptKeyUpDownKeepColumn: boolean read FOptKeyUpDownKeepColumn write FOptKeyUpDownKeepColumn default true;
    property OptKeyHomeEndNavigateWrapped: boolean read FOptKeyHomeEndNavigateWrapped write FOptKeyHomeEndNavigateWrapped default true;
    property OptKeyPageUpDownSize: TATEditorPageDownSize read FOptKeyPageUpDownSize write FOptKeyPageUpDownSize default TATEditorPageDownSize.FullMinus1;
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
    property OptTrimLineOnPressingEnter: boolean read FOptTrimLineOnPressingEnter write FOptTrimLineOnPressingEnter default true;
    property OptShowIndentLines: boolean read FOptShowIndentLines write FOptShowIndentLines default true;
    property OptShowGutterCaretBG: boolean read FOptShowGutterCaretBG write FOptShowGutterCaretBG default true;
    property OptAllowRepaintOnTextChange: boolean read FOptAllowRepaintOnTextChange write FOptAllowRepaintOnTextChange default true;
    property OptAllowReadOnly: boolean read FOptAllowReadOnly write FOptAllowReadOnly default true;
    property OptUndoLimit: integer read FOptUndoLimit write SetUndoLimit default cInitUndoLimit;
    property OptUndoGrouped: boolean read FOptUndoGrouped write FOptUndoGrouped default true;
    property OptUndoAfterSave: boolean read GetUndoAfterSave write SetUndoAfterSave default true;
    property OptUndoMaxCarets: integer read FOptUndoMaxCarets write FOptUndoMaxCarets default cInitUndoMaxCarets;
    property OptUndoIndentVert: integer read FOptUndoIndentVert write FOptUndoIndentVert default cInitUndoIndentVert;
    property OptUndoIndentHorz: integer read FOptUndoIndentHorz write FOptUndoIndentHorz default cInitUndoIndentHorz;
    property OptUndoPause: integer read FOptUndoPause write FOptUndoPause default cInitUndoPause;
    property OptUndoPause2: integer read FOptUndoPause2 write FOptUndoPause2 default cInitUndoPause2;
    property OptUndoPauseHighlightLine: boolean read FOptUndoPauseHighlightLine write FOptUndoPauseHighlightLine default cInitUndoPauseHighlightLine;
    property OptUndoForCaretJump: boolean read FOptUndoForCaretJump write FOptUndoForCaretJump default cInitUndoForCaretJump;
    property OptUndoForMarkers: boolean read GetUndoForMarkers write SetUndoForMarkers default true;
    property OptUndoForAttribs: boolean read GetUndoForAttribs write SetUndoForAttribs default true;
    property OptSavingForceFinalEol: boolean read FOptSavingForceFinalEol write FOptSavingForceFinalEol default false;
    property OptSavingTrimSpaces: boolean read FOptSavingTrimSpaces write FOptSavingTrimSpaces default false;
    property OptSavingTrimFinalEmptyLines: boolean read FOptSavingTrimFinalEmptyLines write FOptSavingTrimFinalEmptyLines default false;
    property OptPasteAtEndMakesFinalEmptyLine: boolean read FOptPasteAtEndMakesFinalEmptyLine write FOptPasteAtEndMakesFinalEmptyLine default true;
    property OptPasteMultilineTextSpreadsToCarets: boolean read FOptPasteMultilineTextSpreadsToCarets write FOptPasteMultilineTextSpreadsToCarets default true;
    property OptPasteWithEolAtLineStart: boolean read FOptPasteWithEolAtLineStart write FOptPasteWithEolAtLineStart default true;
    property OptZebraActive: boolean read FOptZebraActive write FOptZebraActive default false;
    property OptZebraStep: integer read FOptZebraStep write FOptZebraStep default 2;
    property OptZebraAlphaBlend: byte read FOptZebraAlphaBlend write FOptZebraAlphaBlend default cInitZebraAlphaBlend;
    property OptDimUnfocusedBack: integer read FOptDimUnfocusedBack write FOptDimUnfocusedBack default cInitDimUnfocusedBack;
  end;

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

function EditorLinkIsEmail(S: string): boolean;
procedure EditorOpenLink(const S: string);

procedure InitEditorMouseActions(out M: TATEditorMouseActions; ANoCtrlClickForCaret: boolean);

function HandleMouseDownToHandleExtraMouseButtons(Ctl: TCustomControl; Button: TMouseButton; Shift: TShiftState): boolean;

implementation

uses
  LCLIntf,
  LCLProc,
  Dialogs,
  Types,
  Math,
  StrUtils,
  {$ifdef LCLGTK2}
  Gtk2Globals,
  {$endif}
  {$ifdef LCLCOCOA}
  CocoaPrivate,
  CocoaFullControlEdit, //for ICocoaImeControl
  {$endif}
  ATStringProc_TextBuffer,
  ATSynEdit_Commands,
  ATSynEdit_Keymap_Init;

{$I atsynedit_proc.inc}

{$ifdef LCLCOCOA}
//unit was changed to an INC file because of problems with IDE under macOS
{$I atsynedit_cocoaime.inc}
{$endif}

{ TATMinimapThread }

procedure TATMinimapThread.Execute;
var
  Ed: TATSynEdit;
begin
  Ed:= TATSynEdit(Editor);
  repeat
    if Terminated then exit;
    if Ed.FEventMapStart.WaitFor(1000)=wrSignaled then
    begin
      Ed.FEventMapStart.ResetEvent;
      Ed.DoPaintMinimapAllToBGRABitmap;
      Ed.FEventMapDone.SetEvent;
    end;
  until false;
end;

{ TATSynEdit }

procedure TATSynEdit.DoPaintRuler(C: TCanvas);
var
  NCoordX, NPrevFontSize, NRulerStart, NOutput,
  NTopIndent, NMarkHeight, i: integer;
  NCharWidthScaled: integer;
  Str: string;
begin
  NPrevFontSize:= C.Font.Size;
  NRulerStart:= FScrollHorz.NPos;
  NTopIndent:= FOptRulerTopIndentPercents*FCharSize.Y div 100;

  C.Font.Name:= Font.Name;
  C.Font.Size:= DoScaleFont(Font.Size) * FOptRulerFontSizePercents div 100;
  C.Font.Color:= Colors.RulerFont;
  C.Pen.Color:= Colors.RulerFont;
  C.Brush.Color:= FColorRulerBG;

  C.FillRect(FRectRuler);

  NCharWidthScaled:= FCharSize.XScaled * FOptRulerFontSizePercents div 100;

  for i:= NRulerStart to NRulerStart+FVisibleColumns+1 do
  begin
    NCoordX:= FRectMain.Left + (i-NRulerStart) * FCharSize.XScaled div ATEditorCharXScale;

    case FOptRulerNumeration of
      TATEditorRulerNumeration.Num_0_10_20:
        begin
          NOutput:= i;
          if (i mod 10 = 0) then
          begin
            Str:= IntToStr(NOutput);
            CanvasTextOutSimplest(C, NCoordX - NCharWidthScaled*Length(Str) div 2 div ATEditorCharXScale, NTopIndent, Str);
          end;
        end;
      TATEditorRulerNumeration.Num_1_11_21:
        begin
          NOutput:= i;
          if (i mod 10 = 0) then
          begin
            Str:= IntToStr(NOutput+1{!});
            CanvasTextOutSimplest(C, NCoordX - NCharWidthScaled*Length(Str) div 2 div ATEditorCharXScale, NTopIndent, Str);
          end;
        end;
      TATEditorRulerNumeration.Num_1_10_20:
        begin
          NOutput:= i+1;
          if (NOutput=1) or (NOutput mod 10 = 0) then
          begin
            Str:= IntToStr(NOutput);
            CanvasTextOutSimplest(C, NCoordX - NCharWidthScaled*Length(Str) div 2 div ATEditorCharXScale, NTopIndent, Str);
          end;
        end;
    end;

    if NOutput mod 5 = 0 then
      NMarkHeight:= ATEditorScale(FOptRulerMarkSizeBig)
    else
      NMarkHeight:= ATEditorScale(FOptRulerMarkSizeSmall);

    CanvasLineVert(C, NCoordX, FRectRuler.Bottom-1-NMarkHeight, FRectRuler.Bottom-1);
  end;

  CanvasLineHorz(C, FRectRuler.Left, FRectRuler.Bottom-1, FRectRuler.Right);

  C.Font.Size:= NPrevFontSize;
end;


procedure TATSynEdit.DoPaintRulerCaretMark(C: TCanvas; ACaretX: Int64);
begin
  if (ACaretX>=FRectRuler.Left) and (ACaretX<FRectRuler.Right) then
    CanvasPaintTriangleDown(C,
      Colors.RulerFont,
      Point(ACaretX, FRectRuler.Top+ATEditorScale(FOptRulerMarkSizeCaret)),
      ATEditorScale(FOptRulerMarkSizeCaret)
      );
end;

procedure TATSynEdit.DoPaintRulerCaretMarks(C: TCanvas);
var
  NCount, i: integer;
begin
  if FOptRulerVisible and (FOptRulerMarkSizeCaret>0) then
  begin
    if FOptRulerMarkForAllCarets then
      NCount:= Carets.Count
    else
      NCount:= 1;

    for i:= 0 to NCount-1 do
      DoPaintRulerCaretMark(C, Carets[i].CoordX);
  end;
end;

procedure TATSynEdit.UpdateGutterColumns;
var
  NCnt, NLen, NBandIndex: integer;
begin
  if FOptNumbersAutosize then
  begin
    NCnt:= Strings.Count;

    if NCnt>=1000000000 then NLen:= 10
    else
    if NCnt>=100000000 then NLen:= 9
    else
    if NCnt>=10000000 then NLen:= 8
    else
    if NCnt>=1000000 then NLen:= 7
    else
    if NCnt>=100000 then NLen:= 6
    else
    if NCnt>=10000 then NLen:= 5
    else
    if NCnt>=1000 then NLen:= 4
    else
    if NCnt>=100 then NLen:= 3
    else
      NLen:= 2;

    if FOptNumbersStyle=TATEditorNumbersStyle.Relative then //add space for '-'
      Inc(NLen);

    NBandIndex:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers);
    if NBandIndex>=0 then
      FGutter[NBandIndex].Size:= NLen*FCharSize.XScaled div ATEditorCharXScale + 2*FNumbersIndent;
  end
  else
  begin
    NBandIndex:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers);
    if NBandIndex>=0 then
      FGutter[NBandIndex].Size:= FOptGutterWidthNumbers;
  end;

  NBandIndex:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks);
  if NBandIndex>=0 then
    FGutter[NBandIndex].Size:= FOptGutterWidthBookmarks;

  NBandIndex:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagFolding);
  if NBandIndex>=0 then
    FGutter[NBandIndex].Size:= FOptGutterWidthFolding;

  NBandIndex:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagSeparator);
  if NBandIndex>=0 then
    FGutter[NBandIndex].Size:= FOptGutterWidthSeparator;

  NBandIndex:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagEmpty);
  if NBandIndex>=0 then
    FGutter[NBandIndex].Size:= FOptGutterWidthEmpty;

  NBandIndex:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagLineStates);
  if NBandIndex>=0 then
    FGutter[NBandIndex].Size:= FOptGutterWidthLineStates;

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
  CharBig:= FCharSize.XScaled div ATEditorCharXScale;
  CharSmall:= FCharSizeMinimap.XScaled div ATEditorCharXScale;

  if FMinimapCharWidth=0 then
  begin
    FMinimapWidth:= ClientWidth-FTextOffset.X;
    if FMicromapVisible and not FMicromapOnScrollbar then
      Dec(FMinimapWidth, FRectMicromap.Width);
    FMinimapWidth:= FMinimapWidth * CharSmall div (CharSmall+CharBig);
  end
  else
    FMinimapWidth:= CharSmall*FMinimapCharWidth;

  FMinimapWidth:= Max(ATEditorOptions.MinMinimapWidth, FMinimapWidth);
end;

function TATSynEdit.DoFormatLineNumber(N: integer): string;
var
  NCurLine: integer;
begin
  if FOptNumbersStyle=TATEditorNumbersStyle.Relative then
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
    TATEditorNumbersStyle.All:
      Result:= IntToStr(N);
    TATEditorNumbersStyle.None:
      Result:= '.';
    TATEditorNumbersStyle.Each10th:
      begin
        if (N mod 10 = 0) then
          Result:= IntToStr(N)
        else
        if (N mod 5) = 0 then
          Result:= '-'
        else
          Result:= '.';
      end;
    TATEditorNumbersStyle.Each5th:
      begin
        if (N mod 5 = 0) then
          Result:= IntToStr(N)
        else
          Result:= '.';
      end;
  end;
end;

function TATSynEdit.GetScrollbarVisible(bVertical: boolean): boolean;
const
  cKind: array[boolean] of integer = (SB_HORZ, SB_VERT);
var
  si: TScrollInfo;
begin
  si:= Default(TScrollInfo);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;
  GetScrollInfo(Handle, cKind[bVertical], si);
  Result:= Longword(si.nMax) > Longword(si.nPage);
end;

procedure TATSynEdit.SetMarginRight(AValue: integer);
begin
  if AValue=FMarginRight then Exit;
  FMarginRight:= AValue;
  if AValue>=0 then
    FMarginRight:= Max(FMarginRight, ATEditorOptions.MinMarginRt);
  if FWrapMode=TATEditorWrapMode.AtWindowOrMargin then
    FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.UpdateWrapInfo(AForceUpdate: boolean; AAllowCachedUpdate: boolean=true);
var
  CurStrings: TATStrings;
  ListNums: TATIntegerList;
  TempWrapItem: TATWrapItem;
  bUseCachedUpdate: boolean;
  bConsiderFolding: boolean;
  NNewVisibleColumns: integer;
  NIndentMaximal: integer;
  NLine, NLinesCount, NIndexFrom, NIndexTo: integer;
  i, j: integer;
begin
  //method can be called before 1st paint,
  //so TCanvas.TextWidth (TATSynEdit.UpdateCharSize) will give exception "Control has no parent window"
  //example: CudaText has user.json with "wrap_mode":1

  //2021.01.29:
  //check "if not HandleAllocated" stops the work, when passive file-tabs are
  //trying to restore Ed.LineTop.
  // https://github.com/Alexey-T/CudaText/issues/3112
  //to fix this issue, let's not Exit "if not HandleAllocated",
  //but handle this in UpdateCharSize(), via GetDC(0)

  if not HandleAllocated then
    if FWrapMode<>TATEditorWrapMode.ModeOff then
      exit;

  //must init FRect* if called before first paint (wrapped items need it)
  if FRectMain.Width=0 then
    UpdateInitialVars(Canvas);

  FCharSizer.Init(
    Font.Name,
    DoScaleFont(Font.Size),
    FOptTabSize,
    FFontProportional
    );

  //virtual mode allows faster usage of WrapInfo
  CurStrings:= Strings;
  NLinesCount:= CurStrings.Count;
  FWrapInfo.StringsObj:= CurStrings;
  FWrapInfo.VirtualMode:=
    (FWrapMode=TATEditorWrapMode.ModeOff) and
    (Fold.Count=0) and
    (NLinesCount>2);
  if FWrapInfo.VirtualMode then exit;

  bConsiderFolding:= Fold.Count>0;
  NNewVisibleColumns:= GetVisibleColumns;
  NIndentMaximal:= Max(2, NNewVisibleColumns-ATEditorOptions.MinCharsAfterAnyIndent); //don't do too big NIndent

  if AForceUpdate then
    FWrapUpdateNeeded:= true
  else
  if (not FWrapUpdateNeeded) and
    (FWrapMode<>TATEditorWrapMode.ModeOff) and
    (FWrapInfo.VisibleColumns<>NNewVisibleColumns) then
    FWrapUpdateNeeded:= true;

  if not FWrapUpdateNeeded then Exit;
  FWrapUpdateNeeded:= false;
  FWrapInfo.VisibleColumns:= NNewVisibleColumns;

  case FWrapMode of
    TATEditorWrapMode.ModeOff:
      FWrapInfo.WrapColumn:= 0;
    TATEditorWrapMode.ModeOn:
      FWrapInfo.WrapColumn:= Max(ATEditorOptions.MinWrapColumn, NNewVisibleColumns-FWrapAddSpace);
    TATEditorWrapMode.AtWindowOrMargin:
      begin
        if FMarginRight>=0 then
          FWrapInfo.WrapColumn:= Max(ATEditorOptions.MinWrapColumn, Min(NNewVisibleColumns-FWrapAddSpace, FMarginRight))
        else
          FWrapInfo.WrapColumn:= Max(ATEditorOptions.MinWrapColumn, NNewVisibleColumns+FMarginRight);
      end;
  end;

  bUseCachedUpdate:=
    AAllowCachedUpdate and
    (FWrapInfo.Count>0) and
    (FWrapInfo.StringsPrevCount>=0) and
    (NLinesCount>ATEditorOptions.MaxLinesForOldWrapUpdate) and
    CurStrings.EnableCachedWrapinfoUpdate and
    (CurStrings.IndexesOfEditedLines.Count>0);
  //bUseCachedUpdate:= false; ////debug

  FWrapTemps.Clear;

  if not bUseCachedUpdate then
  begin
    FWrapInfo.Clear;
    FWrapInfo.SetCapacity(NLinesCount);
    for i:= 0 to NLinesCount-1 do
    begin
      DoCalcWrapInfos(i, NIndentMaximal, FWrapTemps, bConsiderFolding);
      for j:= 0 to FWrapTemps.Count-1 do
        FWrapInfo.Add(FWrapTemps[j]);
    end;
    FWrapTemps.Clear;
  end
  else
  begin
    //cached WrapInfo update - calculate info only for changed lines (Strings.IndexesOfEditedLines)
    //and insert results into WrapInfo

    //after changes in 2024.01 (action=Add: don't reset EnableCachedWrapinfoUpdate),
    //we can have trailing empty line(s) not indexed in WrapInfo,
    //so add wrap-items for them
    if FWrapInfo.StringsPrevCount>=0 then
      for i:= FWrapInfo.StringsPrevCount to CurStrings.Count-1 do
      begin
        TempWrapItem.Init(i, 1, CurStrings.LinesLen[i], 0, TATWrapItemFinal.Final, true);
        FWrapInfo.Add(TempWrapItem);
      end;

    ListNums:= TATIntegerList.Create;
    try
      ListNums.Assign(CurStrings.IndexesOfEditedLines);
      for i:= 0 to ListNums.Count-1 do
      begin
        NLine:= ListNums[i];
        //check index to avoid EListError
        if not CurStrings.IsIndexValid(NLine) then Continue;

        DoCalcWrapInfos(NLine, NIndentMaximal, FWrapTemps, bConsiderFolding);
        if FWrapTemps.Count=0 then Continue;

        FWrapInfo.FindIndexesOfLineNumber(NLine, NIndexFrom, NIndexTo);
        if NIndexFrom>=0 then
        begin
          //slow for 100 carets, 1M lines, so made method in which
          //we can optimize it (instead of Delete/Insert do Assign)
          FWrapInfo.ReplaceItems(NIndexFrom, NIndexTo, FWrapTemps);
        end
        else
        begin
          //ShowMessage('Cant find wrap-index for line '+IntToStr(NLine));
        end;
      end;

      FWrapTemps.Clear;
    finally
      FreeAndNil(ListNums);
    end;
  end;

  FWrapInfo.StringsPrevCount:= CurStrings.Count;
  CurStrings.IndexesOfEditedLines.Clear;
  CurStrings.EnableCachedWrapinfoUpdate:= true;

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
  AConsiderFolding: boolean;
  AFontProportional: boolean);
var
  WrapItem: TATWrapItem;
  WrapItemPtr: PATWrapItem;
  NLineLen, NPartLen, NFoldFrom: integer;
  NPartOffset, NIndent, NVisColumns: integer;
  bInitialItem: boolean;
  StrPart: UnicodeString;
begin
  AItems.Clear;

  //line folded entirely?
  if AConsiderFolding then
    if AStrings.LinesHidden[ALineIndex, AEditorIndex] then Exit;

  NLineLen:= AStrings.LinesLen[ALineIndex];

  if NLineLen=0 then
  begin
    WrapItem.Init(ALineIndex, 1, 0, 0, TATWrapItemFinal.Final, true);
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
      WrapItem.Init(ALineIndex, 1, Min(NLineLen, NFoldFrom-1), 0, TATWrapItemFinal.Collapsed, true);
      AItems.Add(WrapItem);
      Exit;
    end;
  end;

  //line not wrapped?
  if (AWrapColumn<ATEditorOptions.MinWrapColumnAbs) then
  begin
    WrapItem.Init(ALineIndex, 1, NLineLen, 0, TATWrapItemFinal.Final, true);
    AItems.Add(WrapItem);
    Exit;
  end;

  NVisColumns:= Max(AVisibleColumns, ATEditorOptions.MinWrapColumnAbs);
  NPartOffset:= 1;
  NIndent:= 0;
  bInitialItem:= true;

  repeat
    if AFontProportional then
      StrPart:= AStrings.LineSub(ALineIndex, NPartOffset, ATEditorOptions.MaxVisibleColumns)
    else
      StrPart:= AStrings.LineSub(ALineIndex, NPartOffset, NVisColumns);

    if StrPart='' then
    begin
      if not bInitialItem then
      begin
        WrapItemPtr:= AItems._GetItemPtr(AItems.Count-1);
        WrapItemPtr^.NFinal:= TATWrapItemFinal.Final;
      end;
      Break;
    end;

    NPartLen:= ATabHelper.FindWordWrapOffset(
      ALineIndex,
      //very slow to calc for entire line (eg len=70K),
      //calc for first NVisColumns chars
      StrPart,
      Max(AWrapColumn-NIndent, ATEditorOptions.MinWrapColumnAbs),
      ANonWordChars,
      AWrapIndented
      );

    WrapItem.Init(ALineIndex, NPartOffset, NPartLen, NIndent, TATWrapItemFinal.Middle, bInitialItem);
    AItems.Add(WrapItem);
    bInitialItem:= false;

    if AWrapIndented then
      if NPartOffset=1 then
      begin
        NIndent:= ATabHelper.GetIndentExpanded(ALineIndex, StrPart);
        NIndent:= Min(NIndent, AIndentMaximal);
      end;

    Inc(NPartOffset, NPartLen);
  until false;
end;


procedure TATSynEdit.DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer; AItems: TATWrapItems;
  AConsiderFolding: boolean);
begin
  _CalcWrapInfos(
    Strings,
    FTabHelper,
    FEditorIndex,
    FWrapInfo.WrapColumn,
    FWrapIndented,
    GetVisibleColumns,
    FOptNonWordChars,
    ALine,
    AIndentMaximal,
    AItems,
    AConsiderFolding,
    FFontProportional);
end;


function TATSynEdit.GetVisibleLines: integer;
begin
  Result:= FRectMainVisible.Height div FCharSize.Y;
end;

function TATSynEdit.GetVisibleColumns: integer;
begin
  Result:= FRectMainVisible.Width * ATEditorCharXScale div FCharSize.XScaled;
end;

function TATSynEdit.GetVisibleLinesMinimap: integer;
begin
  Result:= FRectMinimap.Height div FCharSizeMinimap.Y - 1;
end;

function TATSynEdit.GetActualDragDropIsCopying: boolean;
begin
  Result:= FOptMouseDragDropCopying and
    (FOptMouseDragDropCopyingWithState in GetKeyShiftState);
end;

function TATSynEdit.GetActualProximityVert: integer;
begin
  Result:= FOptCaretProximityVert;
  if Result>0 then
    Result:= Min(Min(Result, 10), GetVisibleLines div 2 - 1)
end;

function TATSynEdit.GetMinimapScrollPos: integer;
var
  NPos: Int64;
  NScrollMax, NScrollPage: Int64;
begin
  NPos:= Max(0, FScrollVert.NPos);

  //avoid using FScrollVert.NMax and .NPage, because vert-scrollbar
  //may be not inited yet; CudaText #4566
  UpdateScrollInfoVertPartial(NScrollPage, NScrollMax);

  Result:=
    Int64(NPos) *
    Max(0, NScrollMax-GetVisibleLinesMinimap) div
    Max(1, NScrollMax-NScrollPage);
end;

procedure TATSynEdit.SetTabSize(AValue: integer);
begin
  if FOptTabSize=AValue then Exit;
  FOptTabSize:= Min(ATEditorOptions.MaxTabSize, Max(ATEditorOptions.MinTabSize, AValue));
  FWrapUpdateNeeded:= true;
  FTabHelper.TabSize:= FOptTabSize;
end;

procedure TATSynEdit.SetTabSpaces(AValue: boolean);
begin
  if FOptTabSpaces=AValue then Exit;
  FOptTabSpaces:= AValue;
  FTabHelper.TabSpaces:= AValue;
end;

procedure TATSynEdit.SetText(const AValue: UnicodeString);
begin
  DoCaretSingle(0, 0);
  BookmarkDeleteAll;
  if Assigned(FMarkers) then
    FMarkers.Clear;
  if Assigned(FAttribs) then
    FAttribs.Clear;
  if Assigned(FLinkCache) then
    FLinkCache.Clear;

  Strings.LoadFromString(UTF8Encode(AValue));
  UpdateWrapInfo(true, false{important});
  Update;
end;

procedure TATSynEdit.SetWrapMode(AValue: TATEditorWrapMode);
var
  NLine: integer;
  Caret: TATCaretItem;
begin
  if FWrapMode=AValue then Exit;

  //disable setting wrap=on for too big files
  if FWrapMode=TATEditorWrapMode.ModeOff then
    if Strings.Count>FWrapEnabledForMaxLines then exit;

  NLine:= LineTop;
  FWrapMode:= AValue;

  FWrapUpdateNeeded:= true;
  UpdateWrapInfo; //helps to solve https://github.com/Alexey-T/CudaText/issues/2879
                  //FWrapUpdateNeeded:=true and Update() is not enough

  if FWrapMode<>TATEditorWrapMode.ModeOff then
    FScrollHorz.SetZero;

  Update;
  LineTop:= NLine;

  //when very long line has caret at end, and we toggle wordwrap, let's scroll to new caret pos
  if FWrapMode=TATEditorWrapMode.ModeOff then
    if Carets.Count=1 then
    begin
      Caret:= Carets[0];
      if Caret.PosX>0 then
        DoShowPos(
          Point(Caret.PosX, Caret.PosY),
          FOptScrollIndentCaretHorz,
          FOptScrollIndentCaretVert,
          true,
          true,
          false);
      end;
end;

procedure TATSynEdit.SetWrapIndented(AValue: boolean);
begin
  if FWrapIndented=AValue then Exit;
  FWrapIndented:=AValue;
  if FWrapMode<>TATEditorWrapMode.ModeOff then
    FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.UpdateScrollInfoVertPartial(out APage, AMax: Int64);
begin
  APage:= Max(1, GetVisibleLines)-1;
  AMax:= Max(0, FWrapInfo.Count-1); //must be 0 for single line text
  if FOptLastLineOnTop then
    Inc(AMax, APage);
end;

function TATSynEdit.UpdateScrollbars(AdjustSmoothPos: boolean): boolean;
//returns True is scrollbars visibility was changed
var
  bVert1, bVert2, bHorz1, bHorz2: boolean;
  bVertOur1, bVertOur2, bHorzOur1, bHorzOur2: boolean;
  bChangedBarsOs, bChangedBarsOur: boolean;
  bScrolled: boolean;
  NPos, NLineIndex, NGapPos, NGapAll: integer;
  CharSizeScaled_Prev: integer;
begin
  Result:= false;

  if ModeOneLine then
  begin
    FScrollbarVert.Hide;
    FScrollbarHorz.Hide;
    //don't exit, we still need calculation of FScrollHorz fields
  end;

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

  if not ModeOneLine then
  with FScrollVert do
  begin
    UpdateScrollInfoVertPartial(NPage, NMax);
    NPosLast:= Max(0, NMax-NPage);
    if NPos>NPosLast then
    begin
      NPos:= NPosLast; //fix CudaText #4737
      Include(FPaintFlags, TATEditorInternalFlag.RepaintNeeded); //fix CudaText #5755
    end;

    CharSizeScaled_Prev:= CharSizeScaled;
    CharSizeScaled:= FCharSize.Y * ATEditorCharXScale;
    SmoothMax:= NMax * CharSizeScaled div ATEditorCharXScale + NGapAll;
    SmoothPage:= NPage * CharSizeScaled div ATEditorCharXScale;
    SmoothPosLast:= Max(0, SmoothMax - SmoothPage);
    if AdjustSmoothPos then
      SmoothPos:= TotalOffset + NGapPos
    else
    //fix CudaText #4342
    if CharSizeScaled_Prev<>CharSizeScaled then
      SmoothPos:= SmoothPos*CharSizeScaled div CharSizeScaled_Prev;
  end;

  with FScrollHorz do
  begin
    NPage:= Max(1, GetVisibleColumns);
    //NMax is calculated in DoPaintText
    //hide horz bar for word-wrap:
    if FWrapMode<>TATEditorWrapMode.ModeOff then
    begin
      NPos:= 0;
      SmoothPos:= 0;
      NMax:= NPage;
    end;
    NPosLast:= Max(0, NMax-NPage);

    CharSizeScaled_Prev:= CharSizeScaled;
    CharSizeScaled:= FCharSize.XScaled;
    SmoothMax:= NMax * CharSizeScaled div ATEditorCharXScale;
    SmoothPage:= NPage * CharSizeScaled div ATEditorCharXScale;
    SmoothPosLast:= Max(0, SmoothMax - SmoothPage);
    if AdjustSmoothPos then
      SmoothPos:= TotalOffset
    else
    //fix CudaText #4342
    if CharSizeScaled_Prev<>CharSizeScaled then
      SmoothPos:= SmoothPos*CharSizeScaled div CharSizeScaled_Prev;
  end;

  //don't need further code for OneLine
  if ModeOneLine then exit;

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
    if FScrollHorz.NPos<>FPrevHorz.NPos then
      Include(FPaintFlags, TATEditorInternalFlag.ScrolledHorz);

    bScrolled:=
      (FPrevHorz.SmoothPos<>FScrollHorz.SmoothPos) or
      (FPrevVert.SmoothPos<>FScrollVert.SmoothPos) or
      (FPrevVert.SmoothPage<>FScrollVert.SmoothPage); //this must react to window maximize/restore
    if bScrolled then
      Include(FPaintFlags, TATEditorInternalFlag.ScrollEventNeeded);

    FPrevHorz:= FScrollHorz;
    FPrevVert:= FScrollVert;
  end;
end;

procedure TATSynEdit.UpdateScrollbarVert;
var
  NeedBar: boolean;
  si: TScrollInfo;
begin
  case FOptScrollStyleVert of
    TATEditorScrollbarStyle.Hide:
      NeedBar:= false;
    TATEditorScrollbarStyle.Show:
      NeedBar:= true;
    TATEditorScrollbarStyle.Auto:
      NeedBar:= (FScrollVert.SmoothPos>0) or (FScrollVert.NMax>FScrollVert.NPage);
  end;

  FScrollbarVert.Visible:= NeedBar and FOptScrollbarsNew;
  ShowOsBarVert:= NeedBar and not FOptScrollbarsNew;

  if FScrollbarVert.Visible then
  begin
    FScrollbarLock:= true;

    FScrollbarVert.Min:= 0;
    FScrollbarVert.Max:= FScrollVert.SmoothMax;
    FScrollbarVert.SmallChange:= FScrollVert.CharSizeScaled div ATEditorCharXScale;
    FScrollbarVert.PageSize:= FScrollVert.SmoothPage;
    FScrollbarVert.Position:= FScrollVert.SmoothPos;

    FScrollbarVert.Update;
    FScrollbarLock:= false;
  end;

  if ShowOsBarVert then
  begin
    si:= Default(TScrollInfo);
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
    TATEditorScrollbarStyle.Hide:
      NeedBar:= false;
    TATEditorScrollbarStyle.Show:
      NeedBar:= true;
    TATEditorScrollbarStyle.Auto:
      NeedBar:= (FScrollHorz.SmoothPos>0) or (FScrollHorz.NMax>FScrollHorz.NPage);
  end;

  FScrollbarHorz.Visible:= NeedBar and FOptScrollbarsNew;
  ShowOsBarHorz:= NeedBar and not FOptScrollbarsNew;

  if FScrollbarHorz.Visible then
  begin
    FScrollbarLock:= true;
    FScrollbarHorz.Min:= 0;
    FScrollbarHorz.Max:= FScrollHorz.SmoothMax;
    FScrollbarHorz.SmallChange:= FScrollHorz.CharSizeScaled div ATEditorCharXScale;
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
    si:= Default(TScrollInfo);
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
    - IfThen(FMicromapVisible and not FMicromapOnScrollbar, FRectMicromap.Width);
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
  begin
    R.Left:= 0;
    R.Right:= R.Left+FMinimapWidth;
  end
  else
  begin
    R.Right:= ClientWidth;
    if FMicromapVisible and not FMicromapOnScrollbar then
      Dec(R.Right, FRectMicromap.Width);
    R.Left:= R.Right-FMinimapWidth;
  end;

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
  NWidth: integer;
begin
  if FMicromapOnScrollbar and FOptScrollbarsNew and Assigned(FScrollbarVert) then
    NWidth:= FScrollbarVert.Width - 2*ATScrollbarTheme.BorderSize
  else
    NWidth:= Length(FMicromap.Columns) * FCharSize.XScaled div ATEditorCharXScale * FMicromapScalePerColumn div 100;

  FMicromap.UpdateWidth(NWidth);

  if not FMicromapVisible or FMicromapOnScrollbar then
  begin
    R:= cRectEmpty;
  end
  else
  begin
    R.Top:= 0;
    R.Bottom:= ClientHeight;
    R.Right:= ClientWidth;
    R.Left:= R.Right-NWidth;
  end;
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

  Gutter.GutterCoordLeft:= R.Left;
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

procedure TATSynEdit.GetRectGutterNumbers(out R: TRect);
var
  NBand: integer;
begin
  NBand:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers);
  if FOptGutterVisible and FGutter[NBand].Visible then
  begin
    R.Left:= FGutter[NBand].Left;
    R.Right:= FGutter[NBand].Right;
    R.Top:= FRectGutter.Top;
    R.Bottom:= FRectGutter.Bottom;
  end
  else
    R:= cRectEmpty;
end;

procedure TATSynEdit.GetRectGutterBookmarks(out R: TRect);
var
  NBand: integer;
begin
  NBand:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks);
  if FOptGutterVisible and FGutter[NBand].Visible then
  begin
    R.Left:= FGutter[NBand].Left;
    R.Right:= FGutter[NBand].Right;
    R.Top:= FRectGutter.Top;
    R.Bottom:= FRectGutter.Bottom;
  end
  else
    R:= cRectEmpty;
end;


procedure TATSynEdit.UpdateClientSizes;
begin
  GetClientSizes(FClientW, FClientH);
end;

procedure TATSynEdit.UpdateInitialVars(C: TCanvas);
begin
  UpdateClientSizes;
  UpdateGutterBandIndexes;

  C.Font.Name:= Font.Name;
  C.Font.Size:= DoScaleFont(Font.Size);

  if FOptForceSeparateCharSizer and (FCharSizer=GlobalCharSizer) then
  begin
    FCharSizer:= TATCharSizer.Create(Self);
    FTabHelper.CharSizer:= FCharSizer;
  end;
  UpdateCharSize(FCharSize, C);

  if FSpacingBottom<0 then
    FSpacingTopEdge:= FSpacingBottom
  else
    FSpacingTopEdge:= 0;
  FSpacingTopEdge1:= FSpacingTopEdge; //"-1" gives artifacts on gutter bands

  if FMinimapCustomScale<=0 then
  begin
    FCharSizeMinimap.XScaled:= Max(1, ATEditorScale(1) * ATEditorCharXScale);
    FCharSizeMinimap.Y:=       Max(2, ATEditorScale(2));
  end
  else
  if FMinimapCustomScale<100 then
  begin
    FCharSizeMinimap.XScaled:= ATEditorCharXScale;
    FCharSizeMinimap.Y:=       1;
  end
  else
  begin
    FCharSizeMinimap.XScaled:= Max(1,     FMinimapCustomScale div 100 * ATEditorCharXScale);
    FCharSizeMinimap.Y:=       Max(2, 2 * FMinimapCustomScale div 100);
  end;

  FNumbersIndent:= FCharSize.XScaled * FOptNumbersIndentPercents div 100 div ATEditorCharXScale;

  if FOptRulerText='' then
    FRulerHeight:= FCharSize.Y * FOptRulerHeightPercents div 100
  else
    FRulerHeight:= FCharSize.Y * (SFindCharCount(FOptRulerText, #10)+1);

  if FOptGutterVisible then
    UpdateGutterColumns;

  FTextOffset:= GetTextOffset; //after gutter autosize

  if FMinimapVisible then
    UpdateMinimapAutosize; //after FTextOffset

  GetRectMicromap(FRectMicromap);
  GetRectMinimap(FRectMinimap); //after micromap
  GetRectGutter(FRectGutter);
  GetRectMain(FRectMain); //after gutter/minimap/micromap
  GetRectRuler(FRectRuler); //after main
  GetRectGutterBookmarks(FRectGutterBm); //after gutter
  GetRectGutterNumbers(FRectGutterNums); //after gutter

  FRectMinimapTooltip:= Rect(0, 0, 0, 0);
end;

procedure TATSynEdit.DoPaintBorders(C: TCanvas);
begin
  //border for e.g. macro-recording
  if (FOptBorderColor<>clNone) and (FOptBorderWidthWithColor>0) then
    DoPaintBorder(C, FOptBorderColor, FOptBorderWidthWithColor, false)
  else
  //border for 'focused' state (dark blue by default)
  if FOptBorderFocusedActive and FIsEntered and (FOptBorderWidthFocused>0) then
    DoPaintBorder(C, Colors.BorderLineFocused, FOptBorderWidthFocused, FOptBorderRounded)
  else
  //normal border
  if FOptBorderVisible and (FOptBorderWidth>0) then
    DoPaintBorder(C, Colors.BorderLine, FOptBorderWidth, FOptBorderRounded);
end;

procedure TATSynEdit.DoPaintCornerText_RightBottom(C: TCanvas;
  const AText, AFontName: string;
  AFontSize: integer;
  AColorFont, AColorBack, AColorBorder: TColor);
var
  Sep: TATStringSeparator;
  TextSize: Types.TSize;
  SItem: string;
  NTop: integer;
  bOneLine: boolean;
const
  cBrushStyles: array[boolean] of TBrushStyle = (bsClear, bsSolid);
begin
  if (AText='') or (AColorFont=clNone) then exit;

  if AFontName<>'' then
    C.Font.Name:= AFontName
  else
    C.Font.Name:= Self.Font.Name;
  if AFontSize>0 then
    C.Font.Size:= AFontSize
  else
    C.Font.Size:= Self.Font.Size;

  C.Font.Color:= AColorFont;
  C.Brush.Color:= AColorBack;
  C.Brush.Style:= cBrushStyles[AColorBack<>clNone];

  bOneLine:= Pos(#10, AText)=0;

  Sep.Init(AText, #10);
  NTop:= ClientHeight;
  while Sep.GetItemStr(SItem) do
  begin
    TextSize:= C.TextExtent(SItem);
    Dec(NTop, TextSize.cy);
  end;

  Sep.Init(AText, #10);
  while Sep.GetItemStr(SItem) do
  begin
    if not bOneLine then
      TextSize:= C.TextExtent(SItem);
    C.TextOut(
      ClientWidth-TextSize.cx,
      NTop,
      SItem
      );
    if AColorBorder<>clNone then
    begin
      C.Pen.Color:= AColorBorder;
      C.Frame(
        ClientWidth-TextSize.cx,
        NTop,
        ClientWidth,
        NTop+TextSize.cy
        );
    end;
    Inc(NTop, TextSize.cy);
  end;
  C.Brush.Style:= bsSolid;
end;

procedure TATSynEdit.DoPaintCornerText_RightTop(C: TCanvas;
  const AText, AFontName: string;
  AFontSize: integer;
  AColorFont, AColorBack, AColorBorder: TColor);
var
  Sep: TATStringSeparator;
  TextSize: Types.TSize;
  SItem: string;
  NTop: integer;
const
  cBrushStyles: array[boolean] of TBrushStyle = (bsClear, bsSolid);
begin
  if (AText='') or (AColorFont=clNone) then exit;

  if AFontName<>'' then
    C.Font.Name:= AFontName
  else
    C.Font.Name:= Self.Font.Name;
  if AFontSize>0 then
    C.Font.Size:= AFontSize
  else
    C.Font.Size:= Self.Font.Size;
  C.Font.Color:= AColorFont;
  C.Brush.Color:= AColorBack;
  C.Brush.Style:= cBrushStyles[AColorBack<>clNone];

  Sep.Init(AText, #10);
  NTop:= 0;
  while Sep.GetItemStr(SItem) do
  begin
    TextSize:= C.TextExtent(SItem);
    C.TextOut(
      ClientWidth-TextSize.cx,
      NTop,
      SItem
      );
    if AColorBorder<>clNone then
    begin
      C.Pen.Color:= AColorBorder;
      C.Frame(
        ClientWidth-TextSize.cx,
        NTop,
        ClientWidth,
        NTop+TextSize.cy
        );
    end;
    Inc(NTop, TextSize.cy);
  end;
  C.Brush.Style:= bsSolid;
end;

procedure TATSynEdit.DoPaintMain(C: TCanvas; ALineFrom: integer);
var
  NWrapIndex, NWrapIndexDummy: integer;
  bRulerHandled: boolean;
begin
  C.Brush.Color:= FColorBG;
  C.FillRect(0, 0, Width, Height); //avoid FClientW here to fill entire area

  //update WrapInfo before MinimapThread start
  UpdateWrapInfo;

  //calc FScrollVert.NPos before MinimapThread start
  if ALineFrom>=0 then
  begin
    FWrapInfo.FindIndexesOfLineNumber(ALineFrom, NWrapIndex, NWrapIndexDummy);
    DoScroll_SetPos(FScrollVert, NWrapIndex);
  end
  else
  begin
    NWrapIndex:= Max(0, Min(FScrollVert.NPos, FScrollVert.NPosLast));
  end;

  if FMinimapVisible then
  begin
    {$ifdef map_th}
    if not Assigned(FMinimapThread) then
    begin
      FEventMapStart:= TSimpleEvent.Create;
      FEventMapDone:= TSimpleEvent.Create;
      FMinimapThread:= TATMinimapThread.Create(true);
      FMinimapThread.FreeOnTerminate:= false;
      FMinimapThread.Editor:= Self;
      FMinimapThread.Start;
    end;
    FEventMapStart.SetEvent;
    {$else}
    DoPaintMinimapAllToBGRABitmap;
    {$endif}
  end;

  UpdateLinksAttribs(ALineFrom);
  DoPaintText(C, FRectMain, FCharSize, FOptGutterVisible, FScrollHorz, FScrollVert, NWrapIndex);
  DoPaintMargins(C);
  DoPaintNiceScroll(C);

  if FOptRulerVisible then
  begin
    bRulerHandled:= false;
    if Assigned(FOnDrawRuler) then
      FOnDrawRuler(Self, C, FRectRuler, bRulerHandled);
    if not bRulerHandled then
      DoPaintRuler(C);
  end;

  if Assigned(FOnDrawEditor) then
    FOnDrawEditor(Self, C, FRectMain);

  if FMicromapVisible and not FMicromapOnScrollbar then
    DoPaintMicromap(C);

  //force selection-frame during column selection
  if FOptShowMouseSelFrame or FMouseDownAndColumnSelection then
    if FMouseDragCoord.X>=0 then
      DoPaintMouseSelFrame(C);

  if FMinimapVisible then
  begin
    if FMinimapTooltipVisible and FMinimapTooltipEnabled then
      DoPaintMinimapTooltip(C);

    {$ifdef map_th}
    if FEventMapDone.WaitFor(ATEditorOptions.MaxMinimapThreadWaiting)=wrSignaled then
    begin
      FEventMapDone.ResetEvent;
      FMinimapBmp.Draw(C, FRectMinimap.Left, FRectMinimap.Top);
    end;
    {$else}
    FMinimapBmp.Draw(C, FRectMinimap.Left, FRectMinimap.Top);
    {$endif}
  end;

  DoPaintBorders(C);

  DoPaintCornerText_RightBottom(C,
    FOptCornerText,
    FOptCornerFontName,
    FOptCornerFontSize,
    FOptCornerColorFont,
    FOptCornerColorBack,
    FOptCornerColorBorder
    );

  DoPaintCornerText_RightTop(C,
    FOptCorner2Text,
    FOptCorner2FontName,
    FOptCorner2FontSize,
    FOptCorner2ColorFont,
    FOptCorner2ColorBack,
    FOptCorner2ColorBorder
    );

  if EditorParserExceptionMessage<>'' then
  begin
    C.Font.Color:= clRed;
    C.Brush.Color:= clWhite;
    CanvasTextOutSimplest(C, 4, 4, EditorParserExceptionMessage);
  end;
end;

procedure TATSynEdit.DoPaintMouseSelFrame(C: TCanvas);
const
  cMinW = 8; //minimal width/height of the frame in pixels
  cMinH = 6;
var
  X1, X2, Y1, Y2: integer;
  XX1, XX2, YY1, YY2: integer;
begin
  if not FOptMouseEnableNormalSelection then exit;

  if FMouseDownCoord.Y<0 then exit;

  X1:= FMouseDownCoord.X - FScrollHorz.TotalOffset;
  X2:= FMouseDragCoord.X;
  Y1:= FMouseDownCoord.Y - FScrollVert.TotalOffset;
  Y2:= FMouseDragCoord.Y;

  XX1:= Max(-1, Min(X1, X2));
  YY1:= Max(-1, Min(Y1, Y2));
  XX2:= Min(Width+1, Max(X1, X2));
  YY2:= Min(Height+1, Max(Y1, Y2));

  if XX1<0 then exit;
  if XX2-XX1<cMinW then exit;
  if YY2-YY1<cMinH then exit;

  //avoid TCanvas.DrawFocusRect(), sometimes it's painted bad on Qt5
  C.Pen.Color:= ColorBlendHalf(Colors.TextFont, Colors.TextBG);
  CanvasLineHorz(C, XX1, YY1, XX2, true);
  CanvasLineHorz(C, XX1, YY2, XX2, true);
  CanvasLineVert(C, XX1, YY1, YY2, true);
  CanvasLineVert(C, XX2, YY1, YY2, true);
end;

procedure TATSynEdit.DoPaintBorder(C: TCanvas; AColor: TColor;
  ABorderWidth: integer; ABorderRounded: boolean);
var
  NColorBG, NColorFore: TColor;
  X, Y, W, H, i: integer;
begin
  if ABorderWidth<1 then exit;
  C.Pen.Color:= AColor;

  X:= 0;
  Y:= 0;
  W:= ClientWidth;
  H:= ClientHeight;

  for i:= 0 to ABorderWidth-1 do
    C.Frame(X+i, Y+i, X+W-i, Y+H-i);

  if ABorderRounded and (ABorderWidth=1) then
  begin
    NColorBG:= ColorToRGB(Colors.BorderParentBG);
    NColorFore:= ColorToRGB(Colors.TextBG);

    //left top+bottom corners, with one color
    CanvasPaintRoundedCorners(C,
      Rect(X, Y, X+W, Y+H),
      [acckLeftTop, acckLeftBottom],
      NColorBG, AColor, NColorFore);

    if ModeOneLine and FMicromapVisible then
      NColorFore:= Colors.ComboboxArrowBG;

    //right top+bottom corners, maybe with different color
    CanvasPaintRoundedCorners(C,
      Rect(X, Y, X+W, Y+H),
      [acckRightTop, acckRightBottom],
      NColorBG, AColor, NColorFore);
  end;
end;

procedure TATSynEdit.UpdateCharSize(var ACharSize: TATEditorCharSize; C: TCanvas);
  //
  procedure UpdateFontProportional(TempC: TCanvas);
  begin
    FFontProportional:=
      TempC.TextWidth('.')<TempC.TextWidth('N');
  end;
  //
var
  SampleStrLen: integer;
  SampleStr: string;
  Size: TSize;
begin
  //user told that caching helps here, on low-spec PC
  if (FPrevFont.FontName=Self.Font.Name) and
    (FPrevFont.FontSize=Self.Font.Size) and
    (FPrevFont.SpacingTop=FSpacingTop) and
    (FPrevFont.SpacingBottom=FSpacingBottom) and
    (FPrevFont.OptScaleFont=FOptScaleFont) and
    (FPrevFont.GlobalScale=ATEditorScalePercents) and
    (FPrevFont.GlobalScaleFont=ATEditorScaleFontPercents) then exit;

  FPrevFont.FontName:= Self.Font.Name;
  FPrevFont.FontSize:= Self.Font.Size;
  FPrevFont.SpacingTop:= FSpacingTop;
  FPrevFont.SpacingBottom:= FSpacingBottom;
  FPrevFont.OptScaleFont:= FOptScaleFont;
  FPrevFont.GlobalScale:= ATEditorScalePercents;
  FPrevFont.GlobalScaleFont:= ATEditorScaleFontPercents;

  if ATEditorOptions.PreciseCalculationOfCharWidth then
    SampleStrLen:= 64
  else
    SampleStrLen:= 1;

  SampleStr:= StringOfChar(ATEditorOptions.SampleChar, SampleStrLen);

  if C.HandleAllocated then
  begin
    Size:= C.TextExtent(SampleStr);
    UpdateFontProportional(C);
  end
  else
  begin
    FBitmap.Canvas.Font.Name:= Self.Font.Name;
    FBitmap.Canvas.Font.Size:= DoScaleFont(Self.Font.Size);
    Size:= FBitmap.Canvas.TextExtent(SampleStr);
    UpdateFontProportional(FBitmap.Canvas);
  end;

  ACharSize.XScaled:= Max(1, Size.cx) * ATEditorCharXScale div SampleStrLen;

  ACharSize.Y:= Max(1, Size.cy + FSpacingTop + FSpacingBottom);

  if FFontProportional then
    ACharSize.XSpacePercents:= FCharSizer.GetSpaceWidth
  else
    ACharSize.XSpacePercents:= 100;
end;

procedure TATSynEdit.DoPaintGutterBandBG(C: TCanvas; AColor: TColor;
  AX1, AY1, AX2, AY2: integer; AEntireHeight: boolean);
begin
  if not AEntireHeight then
  begin
    C.Brush.Color:= AColor;
    C.FillRect(AX1, AY1, AX2, AY2);
  end
  else
  begin
    C.Brush.Color:= AColor;
    C.FillRect(AX1, FRectGutter.Top, AX2, FRectGutter.Bottom);
  end;
end;


procedure TATSynEdit.DoPaintText(C: TCanvas;
  const ARect: TRect;
  const ACharSize: TATEditorCharSize;
  AWithGutter: boolean;
  var AScrollHorz, AScrollVert: TATEditorScrollInfo;
  AWrapIndex: integer);
var
  RectLine: TRect;
  GapItemTop, GapItemCur: TATGapItem;
  GutterItem: TATGutterItem;
  WrapItem: TATWrapItem;
  NLineCount, NFoldRangeWithCaret: integer;
begin
  //wrap turned off can cause bad scrollpos, fix it
  with AScrollVert do
    NPos:= Min(NPos, NPosLast);

  C.Brush.Color:= FColorBG;
  C.FillRect(ARect);

  if Assigned(FFoldedMarkList) then
    FFoldedMarkList.Clear;

  if AWithGutter then
  begin
    FColorOfStates[TATLineState.None]:= -1;
    FColorOfStates[TATLineState.Changed]:= Colors.StateChanged;
    FColorOfStates[TATLineState.Added]:= Colors.StateAdded;
    FColorOfStates[TATLineState.Saved]:= Colors.StateSaved;

    C.Brush.Color:= FColorGutterBG;
    C.FillRect(FRectGutter);

    //paint some bands, for full height coloring
    GutterItem:= FGutter[FGutterBandFolding];
    if GutterItem.Visible then
      DoPaintGutterBandBG(C,
        FColorGutterFoldBG,
        GutterItem.Left,
        -1,
        GutterItem.Right,
        -1,
        true);

    GutterItem:= FGutter[FGutterBandSeparator];
    if GutterItem.Visible then
      DoPaintGutterBandBG(C,
        Colors.GutterSeparatorBG,
        GutterItem.Left,
        -1,
        GutterItem.Right,
        -1,
        true);

    GutterItem:= FGutter[FGutterBandEmpty];
    if GutterItem.Visible then
      DoPaintGutterBandBG(C,
        FColorBG,
        GutterItem.Left,
        -1,
        GutterItem.Right,
        -1,
        true);
  end;

  if (FTextHint<>'') then
  begin
    NLineCount:= Strings.Count;
    if (NLineCount=0) or ((NLineCount=1) and (Strings.LinesLen[0]=0)) then
    begin
      DoPaintTextHintTo(C);
      Exit
    end;
  end;

  {$ifndef fix_horzscroll}
  AScrollHorz.NMax:= 1;
  {$endif}

  RectLine.Left:= ARect.Left;
  RectLine.Right:= ARect.Right;
  RectLine.Top:= 0;
  RectLine.Bottom:= ARect.Top;

  if AWithGutter then
    NFoldRangeWithCaret:= DoCalcFoldDeepestRangeContainingCaret
  else
    NFoldRangeWithCaret:= -1;

  DoEventBeforeCalcHilite(true);
  try
   repeat
    RectLine.Top:= RectLine.Bottom;
    RectLine.Bottom:= RectLine.Top+ACharSize.Y;
    if RectLine.Top>ARect.Bottom then Break;

    if not FWrapInfo.IsIndexValid(AWrapIndex) then
    begin
      //paint end-of-file arrow
      if AWrapIndex>=0 then
        if OptUnprintedVisible and OptUnprintedEof then
          if OptUnprintedEndsDetails then
            DoPaintUnprintedSymbols(C,
              TATLineEndsSymbols.EOF,
              ARect.Left,
              RectLine.Top,
              ACharSize,
              Colors.UnprintedFont,
              Colors.UnprintedBG)
          else
            CanvasArrowHorz(C,
              RectLine,
              Colors.UnprintedFont,
              ATEditorOptions.UnprintedEofCharLength*ACharSize.XScaled div ATEditorCharXScale,
              false,
              ATEditorOptions.UnprintedTabPointerScale);
      Break;
    end;

    WrapItem:= FWrapInfo[AWrapIndex];
    GapItemTop:= nil;
    GapItemCur:= nil;

    //consider gap before 1st line
    if (AWrapIndex=0) and AScrollVert.TopGapVisible and (Gaps.SizeOfGapTop>0) then
    begin
      GapItemTop:= Gaps.Find(-1);
      if Assigned(GapItemTop) then
        Inc(RectLine.Bottom, GapItemTop.Size);
    end;

    //consider gap for this line
    if WrapItem.NFinal=TATWrapItemFinal.Final then
    begin
      GapItemCur:= Gaps.Find(WrapItem.NLineIndex);
      if Assigned(GapItemCur) then
        Inc(RectLine.Bottom, GapItemCur.Size);
    end;

    //paint gap before 1st line
    if Assigned(GapItemTop) then
    begin
      DoPaintGap(C,
        Rect(
          RectLine.Left,
          RectLine.Top,
          RectLine.Right,
          RectLine.Top+GapItemTop.Size),
        GapItemTop);
      Inc(RectLine.Top, GapItemTop.Size);
    end;

    DoPaintLine(C,
      RectLine,
      ACharSize,
      AScrollHorz,
      AWrapIndex,
      FParts);

    //paint gap after line
    if Assigned(GapItemCur) then
      DoPaintGap(C,
        Rect(
          RectLine.Left,
          RectLine.Top+ACharSize.Y,
          RectLine.Right,
          RectLine.Top+ACharSize.Y+GapItemCur.Size),
        GapItemCur);

    if AWithGutter then
      DoPaintGutterOfLine(C,
        RectLine,
        ACharSize,
        AWrapIndex,
        NFoldRangeWithCaret
        );

    //update LineBottom as index of last painted line
    FLineBottom:= WrapItem.NLineIndex;

    Inc(AWrapIndex);
   until false;
  finally
    DoEventAfterCalcHilite(true);
  end;

  //block staples
  DoPaintStaples(C, ARect, ACharSize, AScrollHorz);
end;

procedure TATSynEdit.DoPaintMinimapTextToBGRABitmap(
  const ARect: TRect;
  const ACharSize: TATEditorCharSize;
  var AScrollHorz, AScrollVert: TATEditorScrollInfo);
var
  RectLine: TRect;
  NWrapIndex: integer;
begin
  FMinimapBmp.SetSize(ARect.Width, ARect.Height);
  FMinimapBmp.Fill(FColorBG);

  //wrap turned off can cause bad scrollpos, fix it
  with AScrollVert do
    NPos:= Min(NPos, NPosLast);

  NWrapIndex:= Max(0, AScrollVert.NPos);

  RectLine.Left:= ARect.Left;
  RectLine.Right:= ARect.Right;
  RectLine.Top:= 0;
  RectLine.Bottom:= ARect.Top;

  DoEventBeforeCalcHilite(false);
  try
    repeat
      RectLine.Top:= RectLine.Bottom;
      RectLine.Bottom:= RectLine.Top+ACharSize.Y;
      if RectLine.Top>ARect.Bottom then Break;

      if not FWrapInfo.IsIndexValid(NWrapIndex) then
        Break;

      DoPaintMinimapLine(RectLine, ACharSize, AScrollHorz, NWrapIndex, FPartsMinimap);

      Inc(NWrapIndex);
    until false;
  finally
    DoEventAfterCalcHilite(false);
  end;
end;


procedure TATSynEdit.DoPaintLine(C: TCanvas;
  const ARectLine: TRect;
  const ACharSize: TATEditorCharSize;
  var AScrollHorz: TATEditorScrollInfo;
  const AWrapIndex: integer;
  var ATempParts: TATLineParts);
  //
  procedure FillOneLine(AFillColor: TColor);
  begin
    C.Brush.Style:= bsSolid;
    C.Brush.Color:= AFillColor;
    C.FillRect(
      ARectLine.Left,
      ARectLine.Top+FSpacingTopEdge1,
      ARectLine.Right,
      ARectLine.Bottom+FSpacingTopEdge1
      );
  end;
  //
var
  St: TATStrings;
  NLinesIndex, NColumnIndex, NLineLen, NCount: integer;
  NOutputCharsSkipped: Int64;
  NOutputStrWidth, NOutputMaximalChars: Int64;
  NOutputCellPercentsSkipped: Int64;
  NCoordTempX: integer;
  NCoordSep: Int64;
  WrapItem: TATWrapItem;
  StringItem: PATStringItem;
  NColorEntire, NColorAfter, NColorIndent: TColor;
  NDimValue: integer;
  ChosenBackColorEnum: TATEditorChosenBackColor;
  StrOutput: atString;
  CurrPoint, CurrPointText, CoordAfterText: TPoint;
  LineSeparator: TATLineSeparator;
  bLineWithCaret, bLineEolSelected, bLineColorForced, bLineHuge: boolean;
  Event: TATSynEditDrawLineEvent;
  TextOutProps: TATCanvasTextOutProps;
  NSubPos, NSubLen: integer;
  bHiliteLinesWithSelection: boolean;
  bTrimmedNonSpaces: boolean;
  bUseColorOfCurrentLine,
  bUseColorOfCurrentLine2: boolean;
begin
  St:= Strings;
  bHiliteLinesWithSelection:= false;

  WrapItem:= FWrapInfo[AWrapIndex];
  NLinesIndex:= WrapItem.NLineIndex;
  if not St.IsIndexValid(NLinesIndex) then Exit;

  //prepare line
  NOutputCharsSkipped:= 0;
  NOutputCellPercentsSkipped:= 0;
  NOutputStrWidth:= 0;

  CurrPoint.X:= ARectLine.Left;
  CurrPoint.Y:= ARectLine.Top;
  CurrPointText.X:= Int64(CurrPoint.X)
                    + Int64(WrapItem.NIndent)*ACharSize.XScaled*ACharSize.XSpacePercents div ATEditorCharXScale div 100
                    - AScrollHorz.SmoothPos
                    + AScrollHorz.NPixelOffset;
  CurrPointText.Y:= CurrPoint.Y;
  Inc(CurrPointText.Y, FSpacingTopEdge1);

  bTrimmedNonSpaces:= false;

  NLineLen:= St.LinesLen[NLinesIndex];
  bLineHuge:= WrapItem.NLength>cMaxFixedArray;

  if not bLineHuge then
  begin
    //little slow for huge lines
    NSubPos:= WrapItem.NCharIndex;
    if FFontProportional then
      NSubLen:= Min(WrapItem.NLength, ATEditorOptions.MaxVisibleColumns)
    else
      NSubLen:= Min(WrapItem.NLength, FVisibleColumns+AScrollHorz.NPos+1+6);
      //+1 because of NPixelOffset
      //+6 because of HTML color underlines
    StrOutput:= St.LineSub(NLinesIndex, NSubPos, NSubLen);

    if FUnprintedSpacesTrailing then
      bTrimmedNonSpaces:= NSubPos+NSubLen <= St.LineLenWithoutSpace(NLinesIndex);

    if WrapItem.bInitial then
    begin
      //very slow for huge lines
      FTabHelper.FindOutputSkipOffset(
        NLinesIndex,
        StrOutput,
        AScrollHorz.SmoothPos,
        NOutputCharsSkipped,
        NOutputCellPercentsSkipped);
      Delete(StrOutput, 1, NOutputCharsSkipped);
    end;
  end
  else
  begin
    //work faster for huge lines (but not accurate horiz scrollbar)
    NOutputCharsSkipped:= AScrollHorz.SmoothPos * ATEditorCharXScale div ACharSize.XScaled;
    NOutputCellPercentsSkipped:= NOutputCharsSkipped*100;

    NSubPos:= WrapItem.NCharIndex + NOutputCharsSkipped;
    if FFontProportional then
      NSubLen:= Min(WrapItem.NLength, ATEditorOptions.MaxVisibleColumns)
    else
      NSubLen:= Min(WrapItem.NLength, FVisibleColumns+1+6);
      //+1 because of NPixelOffset
      //+6 because of HTML color underlines
    StrOutput:= St.LineSub(NLinesIndex, NSubPos, NSubLen);

    if FUnprintedSpacesTrailing then
      bTrimmedNonSpaces:= NSubPos+NSubLen <= St.LineLenWithoutSpace(NLinesIndex);
  end;

  Inc(CurrPointText.X, NOutputCellPercentsSkipped * ACharSize.XScaled div ATEditorCharXScale div 100);

  if Length(StrOutput)>ATEditorOptions.MaxCharsForOutput then
    SetLength(StrOutput, ATEditorOptions.MaxCharsForOutput);

  if FOptMaskCharUsed then
    StrOutput:= StringOfCharW(FOptMaskChar, Length(StrOutput));

  LineSeparator:= St.LinesSeparator[NLinesIndex];
  bLineWithCaret:= IsLineWithCaret(NLinesIndex, FOptShowCurLineIfWithoutSel);
  bLineEolSelected:= IsPosSelected(WrapItem.NCharIndex-1+WrapItem.NLength, WrapItem.NLineIndex);

  //horz scrollbar max: is calculated here, to make variable horz bar
  //vert scrollbar max: is calculated in UpdateScrollbars
  case FWrapMode of
    TATEditorWrapMode.ModeOn:
      AScrollHorz.NMax:= GetVisibleColumns;
    TATEditorWrapMode.AtWindowOrMargin:
      begin
        if FMarginRight>=0 then
          AScrollHorz.NMax:= Min(GetVisibleColumns, FMarginRight)
        else
          AScrollHorz.NMax:= GetVisibleColumns;
      end
    else
      begin
        //avoid these calculations for huge line length=40M in wrapped mode, because
        //getter of StringItem^.Line is slow
        if bLineHuge then
          NOutputMaximalChars:= NLineLen //approximate, it don't consider CJK chars, but OK for huge lines
        else
        begin
          StringItem:= St.GetItemPtr(NLinesIndex);
          if not FFontProportional and StringItem^.HasAsciiNoTabs then
            NOutputMaximalChars:= StringItem^.CharLen
          else
            NOutputMaximalChars:= CanvasTextWidth(
              StringItem^.Line, //Line getter is very slow for huge lines
              NLinesIndex,
              FTabHelper,
              1, //pass CharWidth=1px
              FFontProportional
              );
        end;
        AScrollHorz.NMax:= Max(
          AScrollHorz.NMax,
          NOutputMaximalChars + FOptScrollbarHorizontalAddSpace);
      end;
  end;

  C.Brush.Color:= FColorBG;
  C.Font.Name:= Font.Name;
  C.Font.Size:= DoScaleFont(Font.Size);
  C.Font.Color:= FColorFont;

  bUseColorOfCurrentLine:= false;
  bUseColorOfCurrentLine2:= false;
  if bLineWithCaret then
    if FOptShowCurLine and (not FOptShowCurLineOnlyFocused or FIsEntered) then
    begin
      bUseColorOfCurrentLine2:= not IsWrapItemWithCaret(WrapItem);
      if FOptShowCurLineMinimal then
        bUseColorOfCurrentLine:= not bUseColorOfCurrentLine2
      else
        bUseColorOfCurrentLine:= true;
    end;

  DoCalcLineEntireColor(
    NLinesIndex,
    bUseColorOfCurrentLine,
    bUseColorOfCurrentLine2,
    NColorEntire,
    bLineColorForced,
    ChosenBackColorEnum,
    bHiliteLinesWithSelection
    );

  if FOptZebraActive then
    if (NLinesIndex+1) mod FOptZebraStep = 0 then
      NColorEntire:= ColorBlend(NColorEntire, FColorFont, FOptZebraAlphaBlend);

  FillOneLine(NColorEntire{, ARectLine.Left});

  //paint line
  if StrOutput<>'' then
  begin
    NColorAfter:= clNone;

    DoCalcLineHilite(
      WrapItem,
      ATempParts{%H-},
      NOutputCharsSkipped,
      ATEditorOptions.MaxCharsForOutput,
      NColorEntire,
      bLineColorForced,
      NColorAfter,
      true,
      true
      );

    if WrapItem.NIndent>0 then
    begin
      {
      //before (2025.02):
      NColorIndent:= FColorBG;
      DoCalcPosColor(WrapItem.NCharIndex, NLinesIndex, NColorIndent, true);
      }
      //after:
      NColorIndent:= ATempParts[0].ColorBG;

      DoPaintLineIndent(C, ARectLine, ACharSize,
        ARectLine.Top, WrapItem.NIndent,
        NColorIndent,
        AScrollHorz.NPos, FOptShowIndentLines);
    end;

    if ATempParts[0].Offset<0 then
    begin
      //some bug in making parts! to fix!
      //raise Exception.Create('Program bug in text renderer, report to author!');
      C.Font.Color:= clRed;
      C.TextOut(CurrPointText.X, CurrPointText.Y, 'Program bug in text renderer, report to author!');
      Exit;
    end;

    //apply DimRanges
    if Assigned(FDimRanges) then
    begin
      NDimValue:= FDimRanges.GetDimValue(WrapItem.NLineIndex, -1);
      if NDimValue>0 then //-1: no ranges found, 0: no effect
        DoPartsDim(ATempParts, NDimValue, FColorBG);
    end;

    //adapter may return ColorAfterEol, paint it
    if FOptShowFullHilite then
      if NColorAfter<>clNone then
        FillOneLine(NColorAfter{, CurrPointText.X});

    Event:= FOnDrawLine;

    if ATEditorOptions.UnprintedReplaceSpec then
      SRemoveAsciiControlChars(StrOutput, WideChar(ATEditorOptions.UnprintedReplaceSpecToCode));

    //truncate text to not paint over screen
    if FFontProportional then
      NCount:= ATEditorOptions.MaxVisibleColumns
    else
      NCount:= ARectLine.Width * ATEditorCharXScale div ACharSize.XScaled + 2;
    if Length(StrOutput)>NCount then
      SetLength(StrOutput, NCount);

      TextOutProps.Editor:= Self;
      TextOutProps.HasAsciiNoTabs:= not FFontProportional and St.LinesHasAsciiNoTabs[NLinesIndex];
      TextOutProps.SuperFast:= bLineHuge;
      TextOutProps.TabHelper:= FTabHelper;
      TextOutProps.LineIndex:= NLinesIndex;
      TextOutProps.CharIndexInLine:= WrapItem.NCharIndex;
      TextOutProps.CharSize:= ACharSize;
      TextOutProps.CharsSkipped:= NOutputCellPercentsSkipped div 100;
      TextOutProps.TrimmedTrailingNonSpaces:= bTrimmedNonSpaces;
      TextOutProps.DrawEvent:= Event;
      TextOutProps.ControlWidth:= ClientWidth+ACharSize.XScaled div ATEditorCharXScale * 2;
      TextOutProps.SpacingTopEdge:= FSpacingTopEdge;
      TextOutProps.SpacingTop:= FSpacingTop;

      TextOutProps.ShowUnprinted:= FUnprintedVisible and FUnprintedSpaces;
      TextOutProps.ShowUnprintedSpacesTrailing:= FUnprintedSpacesTrailing;
      TextOutProps.ShowUnprintedSpacesBothEnds:= FUnprintedSpacesBothEnds;
      TextOutProps.ShowUnprintedSpacesOnlyInSelection:= FUnprintedSpacesOnlyInSelection {and TempSel_IsSelection}; //careful, CudaText #4541
      TextOutProps.ShowUnprintedSpacesAlsoInSelection:= not FUnprintedSpacesOnlyInSelection and FUnprintedSpacesAlsoInSelection and TempSel_IsSelection;
      TextOutProps.ShowUnprintedForceTabs:= FUnprintedForceTabs;
      TextOutProps.DetectIsPosSelected:= @IsPosSelected;

      TextOutProps.ShowFontLigatures:= FOptShowFontLigatures and
                                       (ATEditorOptions.EnableLigaturesOnLineWithCaret or not bLineWithCaret);
      TextOutProps.ColorNormalFont:= Colors.TextFont;
      TextOutProps.ColorUnprintedFont:= Colors.UnprintedFont;
      TextOutProps.ColorUnprintedHexFont:= Colors.UnprintedHexFont;

      TextOutProps.FontProportional:= FFontProportional;

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
        @ATempParts,
        NOutputStrWidth,
        TextOutProps
        );

      //paint selection bg, after applying ColorAfterEol
      DoPaintSelectedLineBG(C, ACharSize, ARectLine,
        CurrPoint,
        CurrPointText,
        WrapItem,
        NOutputStrWidth,
        AScrollHorz);

    //restore after textout
    C.Font.Style:= Font.Style;
  end
  else
  //paint empty line bg
  begin
    if FOptShowFullHilite and
      (ChosenBackColorEnum<>TATEditorChosenBackColor.MarkedRangeBG) then
    begin
      NColorAfter:= clNone;
      //visible StrOutput is empty, but the line itself may be not empty (because of horz scroll)
      DoCalcPosColor(NLineLen, NLinesIndex, NColorAfter, true);
      if NColorAfter<>clNone then
        FillOneLine(NColorAfter{, ARectLine.Left});
    end;

    DoPaintSelectedLineBG(C, ACharSize, ARectLine,
      CurrPoint,
      CurrPointText,
      WrapItem,
      0,
      AScrollHorz);
  end;

  CoordAfterText.X:= CurrPointText.X+NOutputStrWidth;
  CoordAfterText.Y:= CurrPointText.Y;

  if WrapItem.NFinal=TATWrapItemFinal.Final then
  begin
    //for OptShowFullWidthForSelection=false paint eol bg
    if bLineEolSelected then
    begin
      if ATEditorOptions.RenderSpaceBgAtLineEOL then
      begin
        C.Brush.Color:= Colors.TextSelBG;
        C.FillRect(
          CoordAfterText.X,
          CoordAfterText.Y,
          CoordAfterText.X+ACharSize.XScaled div ATEditorCharXScale,
          CoordAfterText.Y+ACharSize.Y);
      end
      else
      if NLineLen=0 then
      begin
        C.Brush.Color:= Colors.TextSelBG;
        C.FillRect(
          CoordAfterText.X,
          CoordAfterText.Y,
          CoordAfterText.X+ACharSize.XScaled div ATEditorCharXScale * ATEditorOptions.RenderSpaceBgAtLineEOL_WidthOnEmpty div 100,
          CoordAfterText.Y+ACharSize.Y);
      end
    end;

    //paint EOL mark
    if FUnprintedVisible and FUnprintedEnds then
      //should UnprintedSpacesOnlyInSelection affect EOL marks? if yes, we need the following "if":
      if (not FUnprintedSpacesOnlyInSelection) or bLineEolSelected then
      begin
        if OptUnprintedEndsDetails then
          DoPaintUnprintedSymbols(C,
            cLineEndsToSymbols[St.LinesEnds[WrapItem.NLineIndex]],
            CoordAfterText.X,
            CoordAfterText.Y,
            ACharSize,
            Colors.UnprintedFont,
            Colors.UnprintedBG)
        else
          DoPaintUnprintedEndSymbol(C,
            CoordAfterText.X,
            CoordAfterText.Y,
            ACharSize,
            Colors.UnprintedFont,
            Colors.TextBG);
      end;
  end
  else
  begin
    //paint wrapped-line-part mark
    if FUnprintedVisible and FUnprintedWraps and
      (not FUnprintedSpacesOnlyInSelection or bLineEolSelected) then
      begin
        if ATEditorOptions.UnprintedWrapArrowAtEdge then
          NCoordTempX:= ARectLine.Right - ACharSize.XScaled div ATEditorCharXScale - 1
        else
          NCoordTempX:= CoordAfterText.X;
        DoPaintUnprintedWrapMark(C,
          NCoordTempX,
          CoordAfterText.Y,
          ACharSize,
          Colors.UnprintedFont);
      end;
  end;

  //draw collapsed-mark
  if WrapItem.NFinal=TATWrapItemFinal.Collapsed then
  begin
    NColumnIndex:= St.LinesFoldFrom[NLinesIndex, FEditorIndex]-1;
    DoPaintFoldedMark(C,
      NColumnIndex,
      NLinesIndex,
      CoordAfterText.X,
      CoordAfterText.Y,
      GetFoldedMarkText(NColumnIndex, NLinesIndex)
      );
  end;

  //draw separators
  if (LineSeparator<>TATLineSeparator.None) then
  begin
    if LineSeparator=TATLineSeparator.Top then
      NCoordSep:= ARectLine.Top
    else
      NCoordSep:= ARectLine.Top+ACharSize.Y-1;
    C.Pen.Color:= Colors.BlockSepLine;
    CanvasLineHorz(C, ARectLine.Left, NCoordSep, ARectLine.Right);
  end;

  //draw folding line '- - - -'
  if (WrapItem.NFinal=TATWrapItemFinal.Final) and IsFoldingUnderlineNeededForWrapitem(AWrapIndex) then
  begin
    if WrapItem.NIndent>0 then
      Inc(NOutputStrWidth, WrapItem.NIndent*ACharSize.XScaled*ACharSize.XSpacePercents div ATEditorCharXScale div 100);

    DoPaintFoldingUnderline(C,
      WrapItem.NLineIndex,
      ARectLine,
      ACharSize,
      AScrollHorz,
      NOutputStrWidth
      );
  end;
end;


function TATSynEdit.GetLineIndentInSpaces(ALine: integer): integer;
var
  StringItem: PATStringItem;
  LineIndentKind: TATLineIndentKind;
  NChars: SizeInt;
  Str: atString;
begin
  StringItem:= Strings.GetItemPtr(ALine);
  StringItem^.GetIndentProp(NChars, LineIndentKind);
  if NChars>0 then
  begin
    Str:= StringItem^.LineSub(1, NChars);
    Result:= FTabHelper.GetIndentExpanded(ALine, Str);
  end
  else
    Result:= 0;
end;

function TATSynEdit.GetLineIndentInPixels(ALine: integer; const ACharSize: TATEditorCharSize): integer;
begin
  Result:= GetLineIndentInSpaces(ALine)
    * ACharSize.XScaled*ACharSize.XSpacePercents div ATEditorCharXScale div 100;
end;

procedure TATSynEdit.DoPaintFoldingUnderline(C: TCanvas;
  ALine: integer;
  const ARectLine: TRect;
  const ACharSize: TATEditorCharSize;
  const AScrollHorz: TATEditorScrollInfo;
  AOutputTextWidth: integer);
var
  NCoordTop, NCoordLeft, NCoordRight: integer;
  NOutputTextStart, NLineWidth, NDashLen, NEmptyLen, i: integer;
begin
  NCoordTop:= ARectLine.Top+ACharSize.Y-1;

  case ATEditorOptions.FoldedUnderlineSize of
    TATEditorFoldedUnderlineSize.BeginToWindowEnd:
      begin
        NCoordLeft:= ARectLine.Left+FFoldUnderlineOffset;
        NCoordRight:= ARectLine.Right-FFoldUnderlineOffset;
      end;
    TATEditorFoldedUnderlineSize.BeginToLineEnd:
      begin
        NCoordLeft:= ARectLine.Left+FFoldUnderlineOffset;
        NCoordRight:= Min(ARectLine.Right-FFoldUnderlineOffset, ARectLine.Left+AOutputTextWidth);
      end;
    TATEditorFoldedUnderlineSize.IndentToLineEnd:
      begin
        NOutputTextStart:= GetLineIndentInPixels(ALine, ACharSize)-AScrollHorz.SmoothPos;
        NCoordLeft:= Max(ARectLine.Left+FFoldUnderlineOffset, ARectLine.Left+NOutputTextStart);
        NCoordRight:= Min(ARectLine.Right-FFoldUnderlineOffset, ARectLine.Left+AOutputTextWidth);
      end;
    TATEditorFoldedUnderlineSize.BeginToMargin:
      begin
        NCoordLeft:= ARectLine.Left+FFoldUnderlineOffset;
        NCoordRight:= Min(
          ARectLine.Right - FFoldUnderlineOffset,
          ARectLine.Left +
            Max(
              IfThen(FMarginRight>=0, FMarginRight, GetVisibleColumns+FMarginRight),
              ATEditorOptions.MinMarginRt
              )
            * ACharSize.XScaled div ATEditorCharXScale
            - AScrollHorz.SmoothPos
          );
      end;
  end;

  NLineWidth:= Max(1, DoScaleFont(1));

  case ATEditorOptions.FoldedUnderlineStyle of
    TATEditorFoldedUnderlineStyle.Dashed:
      begin
        NDashLen:= DoScaleFont(ATEditorOptions.DashedLine_DashLen);
        NEmptyLen:= DoScaleFont(ATEditorOptions.DashedLine_EmptyLen);
        for i:= 0 to NLineWidth-1 do
          CanvasLineHorz_Dashed(C,
            Colors.CollapseLine,
            NCoordLeft,
            NCoordTop-i,
            NCoordRight,
            NDashLen,
            NEmptyLen
            );
      end;
    TATEditorFoldedUnderlineStyle.Solid:
      begin
        for i:= 0 to NLineWidth-1 do
          CanvasLine(C,
            Point(NCoordLeft, NCoordTop-i),
            Point(NCoordRight, NCoordTop-i),
            Colors.CollapseLine
            );
      end;
    TATEditorFoldedUnderlineStyle.Dotted:
      begin
        CanvasLine_Dotted(C,
          Colors.CollapseLine,
          NCoordLeft,
          NCoordTop,
          NCoordRight,
          NCoordTop
          );
      end;
  end;
end;

procedure TATSynEdit.DoPaintMinimapLine(
  ARectLine: TRect;
  const ACharSize: TATEditorCharSize;
  var AScrollHorz: TATEditorScrollInfo;
  const AWrapIndex: integer;
  var ATempParts: TATLineParts);
  //
  procedure FillOneLine(AFillColor: TColor; ARectLeft: integer);
  begin
    FMinimapBmp.FillRect(
      ARectLeft - FRectMinimap.Left,
      ARectLine.Top - FRectMinimap.Top,
      FRectMinimap.Width,
      ARectLine.Top - FRectMinimap.Top + ACharSize.Y,
      AFillColor);
  end;
  //
var
  St: TATStrings;
  NLinesIndex: integer;
  NOutputCharsSkipped: integer;
  WrapItem: TATWrapItem;
  NColorEntire, NColorAfter: TColor;
  CurrPoint, CurrPointText: TPoint;
  ChosenBackColorEnum: TATEditorChosenBackColor;
  bLineColorForced: boolean;
  bUseSetPixel: boolean;
  bUseColorOfCurrentLine,
  bUseColorOfCurrentLine2: boolean;
begin
  St:= Strings;
  bUseSetPixel:=
    {$IF Defined(LCLWin32)} {$else} DoubleBuffered and {$endif}
    (ACharSize.XScaled div ATEditorCharXScale = 1);

  if not FWrapInfo.IsIndexValid(AWrapIndex) then Exit; //e.g. main thread updated WrapInfo
  WrapItem:= FWrapInfo[AWrapIndex];
  NLinesIndex:= WrapItem.NLineIndex;
  if not St.IsIndexValid(NLinesIndex) then Exit;

  //prepare line
  NOutputCharsSkipped:= 0;

  CurrPoint.X:= ARectLine.Left;
  CurrPoint.Y:= ARectLine.Top;
  CurrPointText.X:= Int64(CurrPoint.X)
                    + Int64(WrapItem.NIndent)*ACharSize.XScaled div ATEditorCharXScale
                    - AScrollHorz.SmoothPos
                    + AScrollHorz.NPixelOffset;
  CurrPointText.Y:= CurrPoint.Y;

  bUseColorOfCurrentLine:= false;
  bUseColorOfCurrentLine2:= false;

  DoCalcLineEntireColor(
    NLinesIndex,
    bUseColorOfCurrentLine,
    bUseColorOfCurrentLine2,
    NColorEntire,
    bLineColorForced,
    ChosenBackColorEnum,
    FMinimapHiliteLinesWithSelection
    );

  FillOneLine(NColorEntire, ARectLine.Left);

  //paint line
  if WrapItem.NLength>0 then
  begin
    NColorAfter:= clNone;

    if GetTickCount64-FTickMinimap>=ATEditorOptions.MinimapColoringTime then
    begin
      ATempParts:= Default(TATLineParts);
      ATempParts[0].Offset:= 0;
      ATempParts[0].Len:= WrapItem.NLength;
      ATempParts[0].ColorBG:= NColorEntire;
      ATempParts[0].ColorFont:= FColorFont;
    end
    else
    DoCalcLineHilite(
      WrapItem,
      ATempParts{%H-},
      NOutputCharsSkipped,
      ATEditorOptions.MaxCharsForOutput,
      NColorEntire,
      bLineColorForced,
      NColorAfter,
      false,
      true
      );

    //adapter may return ColorAfterEol, paint it
    if FOptShowFullHilite then
      if NColorAfter<>clNone then
        FillOneLine(NColorAfter, CurrPointText.X);

    {
    //truncate text to not paint over screen
    NMaxStringLen:= ARectLine.Width div ACharSize.XScaled div ATEditorCharXScale + 2;
    if Length(StrOutput)>NMaxStringLen then
      SetLength(StrOutput, NMaxStringLen);
      }

    CanvasTextOutMinimap(
        FMinimapBmp,
        ARectLine,
        CurrPointText.X - FRectMinimap.Left,
        CurrPointText.Y - FRectminimap.Top,
        ACharSize,
        FOptTabSize,
        ATempParts,
        FColorBG,
        NColorAfter,
        St.LineSub(
          WrapItem.NLineIndex,
          WrapItem.NCharIndex,
          FVisibleColumns), //optimize for huge lines
        bUseSetPixel
        );
  end
  else
  //paint empty line bg
  begin
    if FOptShowFullHilite then
    begin
      NColorAfter:= clNone;
      DoCalcPosColor(0, NLinesIndex, NColorAfter, false);
      if NColorAfter<>clNone then
        FillOneLine(NColorAfter, ARectLine.Left);
    end;
  end;
end;


procedure TATSynEdit.DoPaintGutterBookmarkStdIcon(C: TCanvas; ARect: TRect);
var
  dx: integer;
begin
  C.Brush.Color:= Colors.BookmarkIcon;
  C.Pen.Color:= Colors.BookmarkIcon;
  inc(ARect.Top, 1);
  inc(ARect.Left, 4);
  dx:= ARect.Height div 2-1;
  C.Polygon([
    Point(ARect.Left, ARect.Top),
    Point(ARect.Left+dx, ARect.Top+dx),
    Point(ARect.Left, ARect.Top+2*dx)
    ]);
end;


procedure TATSynEdit.DoPaintGutterOfLine(C: TCanvas;
  ARect: TRect;
  const ACharSize: TATEditorCharSize;
  AWrapIndex, AFoldRangeWithCaret: integer);
var
  St: TATStrings;
  WrapItem: TATWrapItem;
  LineState: TATLineState;
  GutterItem: TATGutterItem;
  bLineWithCaret, bHandled, bIconPainted: boolean;
  NLinesIndex, NBandDecor, NBookmarkIndex, NShift2ndIcon: integer;
  TempRect: TRect;
begin
  St:= Strings;
  WrapItem:= FWrapInfo[AWrapIndex];
  NLinesIndex:= WrapItem.NLineIndex;
  if not St.IsIndexValid(NLinesIndex) then exit;
  bLineWithCaret:= IsLineWithCaret(NLinesIndex);
  bIconPainted:= false;

  Inc(ARect.Top, FSpacingTopEdge);

  //paint area over scrolled text
  C.Brush.Color:= FColorGutterBG;
  C.FillRect(FRectGutter.Left, ARect.Top, FRectGutter.Right, ARect.Bottom);

  //gutter band: number
  GutterItem:= FGutter[FGutterBandNumbers];
  if GutterItem.Visible then
  begin
    if bLineWithCaret and FOptShowGutterCaretBG then
    begin
      DoPaintGutterBandBG(C,
        Colors.GutterCaretBG,
        GutterItem.Left,
        ARect.Top,
        GutterItem.Right,
        ARect.Bottom,
        false);
      C.Font.Color:= Colors.GutterCaretFont;
    end
    else
      C.Font.Color:= Colors.GutterFont;

    if WrapItem.bInitial then
      DoPaintGutterNumber(C, NLinesIndex, ARect.Top, GutterItem);
  end;

  //gutter decor
  NBandDecor:= FGutterBandDecor;
  if NBandDecor<0 then
    NBandDecor:= FGutterBandBookmarks;

  GutterItem:= FGutter[NBandDecor];
  if GutterItem.Visible then
    if WrapItem.bInitial then
      DoPaintGutterDecor(C,
        NLinesIndex,
        Rect(
          GutterItem.Left,
          ARect.Top,
          GutterItem.Right,
          ARect.Bottom
          ),
        bIconPainted
        );

  //gutter band: bookmark
  GutterItem:= FGutter[FGutterBandBookmarks];
  if GutterItem.Visible then
    if WrapItem.bInitial then
    begin
      NBookmarkIndex:= St.Bookmarks.Find(NLinesIndex);
      if NBookmarkIndex>=0 then
      begin
        TempRect:= Rect(
          GutterItem.Left,
          ARect.Top,
          GutterItem.Right,
          ARect.Bottom
          );

        //if decor's icon was painted on this place before, shift current bookmark's icon to the right
        if bIconPainted then
        begin
          if Assigned(FGutterDecorImages) then
            NShift2ndIcon:= FGutterDecorImages.Width * ATEditorOptions.GutterShift2ndIconRightByPercents div 100
          else
            NShift2ndIcon:= 16 div 2;
          NShift2ndIcon:= DoScaleFont(NShift2ndIcon);
          Inc(TempRect.Left, NShift2ndIcon);
          Inc(TempRect.Right, NShift2ndIcon);
        end;

        bHandled:= false;
        DoEventDrawBookmarkIcon(
          C,
          NLinesIndex,
          NBookmarkIndex,
          TempRect,
          bHandled);
        if not bHandled then
          DoPaintGutterBookmarkStdIcon(C, TempRect);
      end;
    end;

  //gutter band: fold
  GutterItem:= FGutter[FGutterBandFolding];
  if GutterItem.Visible then
  begin
    DoPaintGutterBandBG(C,
      FColorGutterFoldBG,
      GutterItem.Left,
      ARect.Top,
      GutterItem.Right,
      ARect.Bottom,
      false);
    DoPaintGutterFolding(C,
      AWrapIndex,
      AFoldRangeWithCaret,
      Point(GutterItem.Left, ARect.Top),
      Point(GutterItem.Right, ARect.Bottom)
      );
  end;

  //gutter band: line state
  GutterItem:= FGutter[FGutterBandStates];
  if GutterItem.Visible then
  begin
    LineState:= St.LinesState[NLinesIndex];
    if LineState<>TATLineState.None then
      DoPaintGutterBandBG(C,
        FColorOfStates[LineState],
        GutterItem.Left,
        ARect.Top,
        GutterItem.Right,
        ARect.Bottom,
        false);
  end;

  //gutter band: separator
  GutterItem:= FGutter[FGutterBandSeparator];
  if GutterItem.Visible then
    DoPaintGutterBandBG(C,
      Colors.GutterSeparatorBG,
      GutterItem.Left,
      ARect.Top,
      GutterItem.Right,
      ARect.Bottom,
      false);

  //gutter band: empty indent
  GutterItem:= FGutter[FGutterBandEmpty];
  if GutterItem.Visible then
    DoPaintGutterBandBG(C,
      FColorBG,
      GutterItem.Left,
      ARect.Top,
      GutterItem.Right,
      ARect.Bottom,
      false);
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
  if Result>FWrapInfo.Count-1 then
    Result:= FWrapInfo.Count-1
  else
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

procedure TATSynEdit.DoPaintMinimapSelToBGRABitmap;
var
  C: TBGRABitmap;
  R: TRect;
  rColor: TBGRAPixel;
  nAlphaValue: integer;
  NPos: integer;
begin
  C:= FMinimapBmp;
  if FMinimapShowSelAlways or FCursorOnMinimap then
  begin
    GetRectMinimapSel(R);
    OffsetRect(R, -FRectMinimap.Left, -FRectMinimap.Top);

    //border must be more visible, not overlap vertical line
    if FMinimapAtLeft then
      Dec(R.Right)
    else
      Inc(R.Left);

    // https://forum.lazarus.freepascal.org/index.php/topic,51383.msg377195.html#msg377195
    nAlphaValue:= FMinimapSelColorChange*255 div 100;
    if _IsColorDark(FColorBG) then
      rColor.FromRGB(255, 255, 255, nAlphaValue)
    else
      rColor.FromRGB(0, 0, 0, nAlphaValue);

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
    if FMinimapAtLeft then
      NPos:= FRectMinimap.Right-1
    else
      NPos:= 0;
    C.DrawVertLine(NPos, 0, FRectMinimap.Height, rColor);
  end;
end;

procedure TATSynEdit.UpdateScrollbarsOfMinimap;
begin
  FScrollHorzMinimap.Clear;
  FScrollVertMinimap.Clear;

  FScrollVertMinimap.NPos:= GetMinimapScrollPos;
  FScrollVertMinimap.NPosLast:= MaxInt div 2;
end;

procedure TATSynEdit.DoPaintMinimapAllToBGRABitmap;
begin
  //avoid too often minimap repainting
  if not FAdapterIsDataReady then exit;
  FTickMinimap:= GetTickCount64;

  UpdateScrollbarsOfMinimap;

  DoPaintMinimapTextToBGRABitmap(FRectMinimap, FCharSizeMinimap, FScrollHorzMinimap, FScrollVertMinimap);
  DoPaintMinimapSelToBGRABitmap;

  if ATEditorOptions.DebugTiming then
    FTickMinimap:= GetTickCount64-FTickMinimap;
end;

procedure TATSynEdit.DoPaintMicromap(C: TCanvas);
begin
  if Assigned(FOnDrawMicromap) then
    FOnDrawMicromap(Self, C, FRectMicromap)
  else
  begin
    C.Brush.Color:= clCream;
    C.Brush.Style:= bsSolid;
    C.FillRect(FRectMicromap);
  end;
end;


procedure TATSynEdit.DoPaintGap(C: TCanvas; const ARect: TRect; AGap: TATGapItem);
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

  if Assigned(AGap.Bitmap) then
  begin
    RBmp:= Rect(0, 0, AGap.Bitmap.Width, AGap.Bitmap.Height);
    //RHere is rect of bitmap's size
    case FOptGapBitmapAlignment of
      taCenter:
        RHere.Left:= GetGapBitmapPosLeft(ARect, AGap.Bitmap.Width);
      taLeftJustify:
        RHere.Left:= ARect.Left +
                     Int64(FOptGapBitmapIndent)*FCharSize.XScaled*FCharSize.XSpacePercents div ATEditorCharXScale div 100;
      taRightJustify:
        RHere.Left:= Max(ARect.Left, ARect.Right-AGap.Bitmap.Width);
    end;
    RHere.Top:= (ARect.Top+ARect.Bottom-RBmp.Bottom) div 2;
    RHere.Right:= RHere.Left + RBmp.Right;
    RHere.Bottom:= RHere.Top + RBmp.Bottom;
    C.CopyRect(RHere, AGap.Bitmap.Canvas, RBmp);
  end
  else
  if Assigned(AGap.Form) then
  begin
    AGap.Form.BorderStyle:= bsNone;
    AGap.Form.Parent:= Self;

    //RHere is rect of form, it is stretched by width to RectMain
    RHere.Left:= RectMain.Left;
    RHere.Right:= RectMain.Right;
    RHere.Top:= ARect.Top;
    RHere.Bottom:= RHere.Top + AGap.Size;

    AGap.Form.BoundsRect:= RHere;
    AGap.FormVisible:= true;
  end
  else
  if Assigned(FOnDrawGap) then
    FOnDrawGap(Self, C, ARect, AGap);
end;

procedure TATSynEdit.DoPaintMarginLineTo(C: TCanvas; AX: Int64; AWidth: integer; AColor: TColor);
begin
  if (AX>=FRectMain.Left) and (AX<FRectMain.Right) then
  begin
    C.Pen.Color:= AColor;
    CanvasLineVert2(C, AX, FRectMain.Top, FRectMain.Bottom, false, AWidth);
  end;
end;

procedure TATSynEdit.DoPaintMargins(C: TCanvas);
  //
  function PosX(NMargin: integer): integer; inline;
  begin
    Result:= FRectMain.Left + FCharSize.XScaled *(NMargin-FScrollHorz.NPos) div ATEditorCharXScale;
  end;
var
  NWidth, NX, iMargin: integer;
begin
  NWidth:= ATEditorScale(1);

  if FMarginRight>=0 then
    NX:= FMarginRight
  else
    NX:= GetVisibleColumns+FMarginRight;
  NX:= Max(NX, ATEditorOptions.MinMarginRt);
  DoPaintMarginLineTo(C, PosX(NX), NWidth, Colors.MarginRight);

  for iMargin:= 0 to Length(FMarginList)-1 do
    DoPaintMarginLineTo(C, PosX(FMarginList[iMargin]), NWidth, Colors.MarginUser);
end;


procedure TATSynEdit.DoPaintFoldedMark(C: TCanvas;
  APosX, APosY, ACoordX, ACoordY: integer;
  const AMarkText: string);
var
  Str: string;
  RectMark: TRect;
  FoldMark: TATFoldedMark;
  NWidth, NRangeIndex, NLastFoldedLine: integer;
begin
  Str:= AMarkText;

  SDeleteFrom(Str, #10);
    //e.g. Diff lexer gives collapsed-string with EOL (several lines)

  Str:= FTabHelper.TabsToSpaces(APosY, UTF8Decode(Str));
    //expand tabs too

  if APosX>0 then
    Inc(ACoordX, ATEditorOptions.FoldedMarkIndentOuter);

  //set colors:
  //if 1st chars selected, then use selection-color
  if IsPosSelected(APosX, APosY) and FOptShowFoldedMarkWithSelectionBG then
  begin
    if not FOptKeepSelFontColor then
      C.Font.Color:= Colors.TextSelFont
    else
      C.Font.Color:= Colors.TextFont;
    C.Brush.Color:= Colors.TextSelBG;
  end
  else
  begin
    C.Font.Color:= Colors.CollapseMarkFont;
    C.Brush.Color:= FColorCollapseMarkBG;
  end;

  //paint text
  if not FOptShowFoldedMarkWithSelectionBG then
    C.Brush.Style:= bsClear;

  C.TextOut(
    ACoordX+ATEditorOptions.FoldedMarkIndentInner,
    ACoordY+FSpacingTopEdge+FSpacingTop,
    Str);
  NWidth:= C.TextWidth(Str) + 2*ATEditorOptions.FoldedMarkIndentInner;

  //paint frame
  RectMark:= Rect(ACoordX, ACoordY, ACoordX+NWidth, ACoordY+FCharSize.Y);
  C.Pen.Color:= Colors.CollapseMarkBorder;
  C.Brush.Style:= bsClear;
  C.Rectangle(RectMark);
  C.Brush.Style:= bsSolid;

  NRangeIndex:= FFold.FindRangeWithPlusAtLine(APosY);
  if NRangeIndex>=0 then
  begin
    NLastFoldedLine:= FFold.ItemPtr(NRangeIndex)^.Y2;
    InitFoldedMarkList;
    FoldMark.Init(RectMark, APosY, NLastFoldedLine);
    FFoldedMarkList.Add(FoldMark);
  end;
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
  if FLineTopTodo>0 then
    exit(FLineTopTodo);
  Result:= 0;
  if Assigned(FWrapInfo) and (FWrapInfo.Count>0) then
  begin
    N:= Max(0, FScrollVert.NPos);
    if FWrapInfo.IsIndexValid(N) then
      Result:= FWrapInfo[N].NLineIndex;
  end;
end;

function TATSynEdit.GetColumnLeft: integer;
begin
  Result:= FScrollHorz.NPos;
end;

constructor TATSynEdit.Create(AOwner: TComponent);
begin
  inherited;

  if ATEditorOptions.UseGlobalCharSizer then
  begin
    //GlobalCharSizer should be created after MainForm is inited
    if GlobalCharSizer=nil then
      GlobalCharSizer:= TATCharSizer.Create(AOwner);
    FCharSizer:= GlobalCharSizer;
  end
  else
    FCharSizer:= TATCharSizer.Create(Self);

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

  FCaretShapeNormal:= TATCaretShape.Create;
  FCaretShapeOverwrite:= TATCaretShape.Create;
  FCaretShapeReadonly:= TATCaretShape.Create;

  FCaretShapeNormal.Width:= 2;
  FCaretShapeNormal.Height:= -100;
  FCaretShapeOverwrite.Width:= -100;
  FCaretShapeOverwrite.Height:= -100;
  FCaretShapeReadonly.Width:= -100;
  FCaretShapeReadonly.Height:= 2;

  FWantTabs:= true;
  FWantReturns:= true;
  FCharSize.XScaled:= 4 * ATEditorCharXScale;
  FCharSize.Y:= 4;
  FEditorIndex:= 0;

  FCommandLog:= TATCommandLog.Create;

  FCarets:= TATCarets.Create;
  FCarets.Add(0, 0);

  FCaretShowEnabled:= false; //code sets it On in DoEnter
  FCaretShown:= false;
  FCaretBlinkEnabled:= true;
  FCaretVirtual:= true;
  FCaretSpecPos:= false;
  FCaretStopUnfocused:= true;
  FCaretHideUnfocused:= true;

  FTabHelper:= TATStringTabHelper.Create(FCharSizer);
  FMarkers:= nil;
  FAttribs:= nil;
  FMarkedRange:= nil;
  FDimRanges:= nil;
  FHotspots:= nil;
  FLinkCache:= TATLinkCache.Create;

  FMinimapBmp:= TBGRABitmap.Create;

  {$ifdef windows}
  FAdapterIME:= TATAdapterWindowsIME.Create;
  {$endif}

  {$ifdef LCLCOCOA}
  FAdapterIME:= TATAdapterCocoaIME.Create(self);
  {$endif}

  {$ifdef LCLGTK2}
  FAdapterIME:= TATAdapterGTK2IME.Create;
  {$endif}

  FPaintLocked:= 0;
  FPaintFlags:= [TATEditorInternalFlag.Bitmap];

  Colors.Init;
  InitEditorMouseActions(FMouseActions, false);

  FCursorText:= crIBeam;
  FCursorColumnSel:= crCross;
  FCursorGutterBookmark:= crHandPoint;
  FCursorGutterNumbers:= crDefault;
  FCursorMinimap:= crDefault;
  FCursorMicromap:= crDefault;

  FTimerDelayedParsing:= TTimer.Create(Self);
  FTimerDelayedParsing.Enabled:= false;
  FTimerDelayedParsing.Interval:= 300;
  FTimerDelayedParsing.OnTimer:= @TimerDelayedParsingTick;

  FTimerBlink:= TTimer.Create(Self);
  FTimerBlink.Enabled:= false;
  SetCaretBlinkTime(cInitCaretBlinkTime);
  FTimerBlink.OnTimer:= @TimerBlinkTick;

  FTimerFlicker:= TTimer.Create(Self);
  FTimerFlicker.Enabled:= false;
  FTimerFlicker.OnTimer:= @TimerFlickerTick;

  FBitmap:= Graphics.TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.SetSize(cInitBitmapWidth, cInitBitmapHeight);

  FOptUndoLimit:= cInitUndoLimit;
  FOptUndoIndentVert:= cInitUndoIndentVert;
  FOptUndoIndentHorz:= cInitUndoIndentHorz;
  FOptUndoMaxCarets:= cInitUndoMaxCarets;
  FOptUndoGrouped:= true;
  FOptUndoPause:= cInitUndoPause;
  FOptUndoPause2:= cInitUndoPause2;
  FOptUndoPauseHighlightLine:= cInitUndoPauseHighlightLine;
  FOptUndoForCaretJump:= cInitUndoForCaretJump;

  FStringsExternal:= nil;
  FStringsInt:= TATStrings.Create(FOptUndoLimit);
  FStringsInt.OnGetCaretsArray:= @GetCaretsArray;
  FStringsInt.OnGetMarkersArray:= @GetMarkersArray;
  FStringsInt.OnGetAttribsArray:= @GetAttribsArray;
  FStringsInt.OnSetCaretsArray:= @SetCaretsArray;
  FStringsInt.OnSetMarkersArray:= @SetMarkersArray;
  FStringsInt.OnSetAttribsArray:= @SetAttribsArray;
  FStringsInt.OnProgress:= @DoStringsOnProgress;
  FStringsInt.OnChangeEx:= @DoStringsOnChangeEx;
  FStringsInt.OnChangeLog:= @DoStringsOnChangeLog;
  FStringsInt.OnUndoBefore:= @DoStringsOnUndoBefore;
  FStringsInt.OnUndoAfter:= @DoStringsOnUndoAfter;
  FStringsInt.OnUnfoldLine:= @DoStringsOnUnfoldLine;

  FFold:= TATFoldRanges.Create;
  FFoldStyle:= cInitFoldStyle;
  FFoldEnabled:= true;
  FFoldUnderlineOffset:= cInitFoldUnderlineOffset;
  FFoldTooltipVisible:= cInitFoldTooltipVisible;
  FFoldTooltipWidthPercents:= cInitFoldTooltipWidthPercents;
  FFoldTooltipLineCount:= cInitFoldTooltipLineCount;

  FWrapInfo:= TATWrapInfo.Create;
  FWrapInfo.StringsObj:= FStringsInt;
  FWrapInfo.WrapColumn:= cInitMarginRight;

  FWrapTemps:= TATWrapItems.Create;
  FWrapUpdateNeeded:= true;
  FWrapMode:= cInitWrapMode;
  FWrapIndented:= true;
  FWrapAddSpace:= 1;
  FWrapEnabledForMaxLines:= cInitWrapEnabledForMaxLines;

  FMicromap:= TATMicromap.Create;
  FMicromapVisible:= cInitMicromapVisible;
  FMicromapScalePerColumn:= cInitMicromapScalePerColumn;
  FMicromapOnScrollbar:= cInitMicromapOnScrollbar;
  FMicromapLineStates:= true;
  FMicromapSelections:= true;
  FMicromapBookmarks:= cInitMicromapBookmarks;
  FMicromapShowForMinCount:= cInitMicromapShowForMinCount;

  FOverwrite:= false;
  FMarginRight:= cInitMarginRight;
  FMarginList:= nil;
  FFoldedMarkList:= nil;

  FPrevCaret.PosX:= 0;
  FPrevCaret.PosY:= 0;
  FPrevCaret.EndX:= -1;
  FPrevCaret.EndY:= -1;

  FOptGapBitmapAlignment:= cInitGapBitmapAlignment;
  FOptGapBitmapIndent:= cInitGapBitmapIndent;
  FOptFlickerReducingPause:= 0;
  FOptInputNumberOnly:= false;
  FOptInputNumberAllowNegative:= cInitInputNumberAllowNegative;
  FOptMaskChar:= cInitMaskChar;
  FOptMaskCharUsed:= false;
  FOptScrollAnimationSteps:= cInitScrollAnimationSteps;
  FOptScrollAnimationSleep:= cInitScrollAnimationSleep;
  FOptIdleInterval:= cInitIdleInterval;

  FOptAutoPairForMultiCarets:= cInitAutoPairForMultiCarets;
  FOptAutoPairChars:= '([{';
  FOptAutoPair_DisableCharDoubling:= false;

  FOptAutocompleteSymbolsAllowedBeforeCaret:= '.:>''"'; //why such default: see ATSynEdit_Cmp_Form.pas
  FOptAutocompleteAutoshowCharCount:= 0;
  FOptAutocompleteTriggerChars:= '';
  FOptAutocompleteCommitChars:= ' ,;/\''"';
  FOptAutocompleteCommitOnEnter:= true;
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
  FUnprintedWraps:= false;
  FUnprintedEof:= true;

  FTextHint:= '';
  FTextHintFontStyle:= [fsItalic];
  FTextHintCenter:= false;

  FGutter:= TATGutter.Create;
  FGutter.OnScale:= @DoScaleFont;
  FGutterDecor:= nil;

  FOptGutterVisible:= true;
  FOptGutterPlusSize:= cInitGutterPlusSize;
  FOptGutterShowFoldAlways:= true;
  FOptGutterShowFoldLines:= true;
  FOptGutterShowFoldLinesAll:= false;
  FOptGutterShowFoldLinesForCaret:= true;
  FOptGutterShowBracketDecor:= true;
  FOptGutterWidthBookmarks:= cInitGutterWidthBookmarks;
  FOptGutterWidthNumbers:= cInitGutterWidthNumbers;
  FOptGutterWidthFolding:= cInitGutterWidthFolding;
  FOptGutterWidthSeparator:= cInitGutterWidthSeparator;
  FOptGutterWidthEmpty:= cInitGutterWidthEmpty;
  FOptGutterWidthLineStates:= cInitGutterWidthLineStates;
  FOptGutterIcons:= TATEditorGutterIcons.PlusMinus;

  FGutterDecorAlignment:= taLeftJustify;
  FGutterBandDecor:= -1;

  FGutter.Add(-1, cInitGutterWidthBookmarks,  ATEditorOptions.GutterTagBookmarks,  true, true);
  FGutter.Add(-1, cInitGutterWidthNumbers,    ATEditorOptions.GutterTagNumbers,    false, true);
  FGutter.Add(-1, cInitGutterWidthLineStates, ATEditorOptions.GutterTagLineStates, true, true);
  FGutter.Add(-1, cInitGutterWidthFolding,    ATEditorOptions.GutterTagFolding,    true, true);
  FGutter.Add(-1, cInitGutterWidthSeparator,  ATEditorOptions.GutterTagSeparator,  false, false);
  FGutter.Add(-1, cInitGutterWidthEmpty,      ATEditorOptions.GutterTagEmpty,      false, true);
  UpdateGutterBandIndexes;

  FOptNumbersAutosize:= true;
  FOptNumbersAlignment:= taRightJustify;
  FOptNumbersStyle:= cInitNumbersStyle;
  FOptNumbersShowFirst:= true;
  FOptNumbersShowCarets:= false;
  FOptNumbersIndentPercents:= cInitNumbersIndentPercents;

  FOptBorderVisible:= cInitBorderVisible;
  FOptBorderWidth:= cInitBorderWidth;
  FOptBorderWidthFocused:= cInitBorderWidthFocused;
  FOptBorderWidthWithColor:= cInitBorderWidthWithColor;
  FOptBorderFocusedActive:= false;
  FOptBorderColor:= clNone;

  FOptCornerText:= '';
  FOptCornerColorFont:= clBlack;
  FOptCornerColorBack:= clWhite;
  FOptCornerColorBorder:= clNone;
  FOptCorner2Text:= '';
  FOptCorner2ColorFont:= clBlack;
  FOptCorner2ColorBack:= clWhite;
  FOptCorner2ColorBorder:= clNone;

  FOptRulerVisible:= true;
  FOptRulerNumeration:= cInitRulerNumeration;
  FOptRulerHeightPercents:= cInitRulerHeightPercents;
  FOptRulerMarkSizeCaret:= cInitRulerMarkCaret;
  FOptRulerMarkSizeSmall:= cInitRulerMarkSmall;
  FOptRulerMarkSizeBig:= cInitRulerMarkBig;
  FOptRulerMarkForAllCarets:= false;
  FOptRulerFontSizePercents:= cInitRulerFontSizePercents;
  FOptRulerTopIndentPercents:= 0;

  FMinimapWidth:= 150;
  FMinimapCharWidth:= 0;
  FMinimapCustomScale:= 0;
  FMinimapVisible:= cInitMinimapVisible;
  FMinimapShowSelBorder:= false;
  FMinimapShowSelAlways:= true;
  FMinimapSelColorChange:= cInitMinimapSelColorChange;
  FMinimapAtLeft:= false;
  FMinimapTooltipVisible:= cInitMinimapTooltipVisible;
  FMinimapTooltipHeight:= cInitMinimapTooltipHeight;
  FMinimapTooltipWidthPercents:= cInitMinimapTooltipWidthPercents;
  FMinimapTooltipFontSize:= 0;
  FMinimapHiliteLinesWithSelection:= true;

  FSpacingTop:= cInitSpacingTop;
  FSpacingBottom:= cInitSpacingBottom;
  FCharSizeMinimap.XScaled:= 1 * ATEditorCharXScale;
  FCharSizeMinimap.XSpacePercents:= 100;
  FCharSizeMinimap.Y:= 2;

  FOptScrollStyleHorz:= TATEditorScrollbarStyle.Auto;
  FOptScrollStyleVert:= TATEditorScrollbarStyle.Show;
  FOptScrollSmooth:= true;
  FOptScrollIndentCaretHorz:= 10;
  FOptScrollIndentCaretVert:= 0;

  FOptScrollbarsNew:= false;
  FOptScrollbarHorizontalAddSpace:= cInitScrollbarHorzAddSpace;
  FOptScrollLineCommandsKeepCaretOnScreen:= true;

  FOptShowFontLigatures:= true;
  FOptShowURLs:= true;
  FOptShowURLsRegex:= cUrlRegexInitial;
  FOptShowDragDropMarker:= true;
  FOptShowDragDropMarkerWidth:= cInitDragDropMarkerWidth;
  FOptShowFoldedMarkWithSelectionBG:= cInitShowFoldedMarkWithSelectionBG;

  FOptMinLineLenToCalcURL:= cInitMinLineLenToCalcURL;
  FOptMaxLineLenToCalcURL:= cInitMaxLineLenToCalcURL;
  FOptMaxLinesToCountUnindent:= 100;

  FOptStapleStyle:= TATLineStyle.Solid;
  FOptStapleIndent:= -1;
  FOptStapleWidthPercent:= 100;
  FOptStapleHiliteActive:= true;
  FOptStapleHiliteActiveAlpha:= cInitStapleHiliteAlpha;
  FOptStapleEdge1:= TATEditorStapleEdge.Angle;
  FOptStapleEdge2:= TATEditorStapleEdge.Angle;
  FOptStapleIndentConsidersEnd:= false;

  FOptTextDuplicationMovesCaretDown:= true;
  FOptTextCenteringCharWidth:= 0;
  FOptTextOffsetLeft:= cInitTextOffsetLeft;
  FOptTextOffsetTop:= cInitTextOffsetTop;
  FOptAllowRepaintOnTextChange:= true;
  FOptAllowReadOnly:= true;

  FOptKeyBackspaceUnindent:= true;
  FOptKeyBackspaceGoesToPrevLine:= true;
  FOptKeyPageKeepsRelativePos:= true;
  FOptKeyUpDownNavigateWrapped:= true;
  FOptKeyUpDownAllowToEdge:= false;
  FOptKeyHomeEndNavigateWrapped:= true;
  FOptKeyUpDownKeepColumn:= true;

  FOptOverwriteAllowedOnPaste:= false;
  FOptNonWordChars:= ATEditorOptions.DefaultNonWordChars;
  FOptAutoIndent:= true;
  FOptAutoIndentKind:= TATEditorAutoIndentKind.AsPrevLine;
  FOptAutoIndentBetterBracketsCurly:= true;
  FOptAutoIndentBetterBracketsRound:= false;
  FOptAutoIndentBetterBracketsSquare:= true;
  FOptAutoIndentRegexRule:= '';

  FOptTabSize:= cInitTabSize;
  FOptTabSpaces:= false;
  FOptTabSmart:= false;

  FOptLastLineOnTop:= false;
  FOptOverwriteSel:= true;
  FOptMouseDragDrop:= true;
  FOptMouseDragDropCopying:= true;
  FOptMouseDragDropCopyingWithState:= ssModifier;
  FOptMouseMiddleClickAction:= TATEditorMiddleClickAction.Scrolling;
  FOptMouseHideCursor:= false;

  FOptMouseClickOpensURL:= false;
  FOptMouseClickNumberSelectsLine:= true;
  FOptMouseClickNumberSelectsLineWithEOL:= true;
  FOptMouseClickNumberSelectsFoldedRange:= true;
  FOptMouse2ClickAction:= TATEditorDoubleClickAction.SelectAnyChars;
  FOptMouse2ClickOpensURL:= true;
  FOptMouse2ClickOnFoldMarkSelectsFoldedLines:= true;
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
  FOptShowCurLineIfWithoutSel:= true;
  FOptShowCurColumn:= false;
  FOptShowMouseSelFrame:= cInitShowMouseSelFrame;

  FOptKeyPageUpDownSize:= TATEditorPageDownSize.FullMinus1;
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
  FOptTrimLineOnPressingEnter:= true;
  FOptSavingForceFinalEol:= false;
  FOptSavingTrimSpaces:= false;
  FOptShowScrollHint:= false;
  FOptCaretPreferLeftSide:= true;
  FOptCaretPosAfterPasteColumn:= TATEditorPasteCaret.ColumnRight;
  FOptCaretsAddedToColumnSelection:= true;
  FOptCaretFixAfterRangeFolded:= true;
  FOptCaretsPrimitiveColumnSelection:= cInitCaretsPrimitiveColumnSelection;
  FOptCaretsMultiToColumnSel:= cInitCaretsMultiToColumnSel;
  FOptCaretProximityVert:= 0;
  FOptMarkersSize:= cInitMarkerSize;
  FOptMouseEnableAll:= true;
  FOptMouseEnableNormalSelection:= true;
  FOptMouseEnableColumnSelection:= true;
  FOptPasteAtEndMakesFinalEmptyLine:= true;
  FOptPasteMultilineTextSpreadsToCarets:= true;
  FOptPasteWithEolAtLineStart:= true;
  FOptZebraActive:= false;
  FOptZebraStep:= 2;
  FOptZebraAlphaBlend:= cInitZebraAlphaBlend;
  FOptDimUnfocusedBack:= cInitDimUnfocusedBack;

  ClearMouseDownVariables;
  FMouseDownPnt_ColumnSelOrigin:= Point(-1, -1);
  FMouseNiceScrollPos:= ATPoint(0, 0);

  FSelRect:= cRectEmpty;
  ClearSelRectPoints;
  FCursorOnMinimap:= false;
  FCursorOnGutter:= false;
  FLastTextCmd:= 0;
  FLastTextCmdText:= '';
  FLastCommandChangedText:= false;
  FLastCommandDelayedParsingOnLine:= MaxInt;
  FLastHotspot:= -1;
  FLastCaretY:= -1;

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

  //allow Invalidate to work now
  IsRepaintEnabled:= true;
end;

destructor TATSynEdit.Destroy;
begin
  if Assigned(FTimingQueue) then
    FreeAndNil(FTimingQueue);
  if Assigned(FMinimapThread) then
  begin
    FMinimapThread.Terminate;
    FEventMapStart.SetEvent;
    if not FMinimapThread.Finished then
      FMinimapThread.WaitFor;
    FreeAndNil(FMinimapThread);
  end;
  if Assigned(FEventMapStart) then
    FreeAndNil(FEventMapStart);
  if Assigned(FEventMapDone) then
    FreeAndNil(FEventMapDone);
  FAdapterHilite:= nil;
  if Assigned(FMinimapTooltipBitmap) then
    FreeAndNil(FMinimapTooltipBitmap);
  if Assigned(FRegexLinks) then
    FreeAndNil(FRegexLinks);
  if Assigned(FAdapterIME) then
    FreeAndNil(FAdapterIME);
  TimersStop;
  if Assigned(FHintWnd) then
    FreeAndNil(FHintWnd);
  if Assigned(FMenuStd) then
    FreeAndNil(FMenuStd);
  TimerBlinkDisable;
  if Assigned(FFoldedMarkList) then
  begin
    FFoldedMarkList.Clear;
    FreeAndNil(FFoldedMarkList);
  end;
  FreeAndNil(FMinimapBmp);
  FreeAndNil(FMicromap);
  FreeAndNil(FFold);
  FreeAndNil(FTimerFlicker);
  FreeAndNil(FTimerDelayedParsing);
  if Assigned(FTimerNiceScroll) then
    FreeAndNil(FTimerNiceScroll);
  if Assigned(FTimerScroll) then
    FreeAndNil(FTimerScroll);
  FreeAndNil(FTimerBlink);
  FreeAndNil(FCarets);
  FreeAndNil(FCommandLog);
  if Assigned(FHotspots) then
    FreeAndNil(FHotspots);
  if Assigned(FDimRanges) then
    FreeAndNil(FDimRanges);
  if Assigned(FMarkedRange) then
    FreeAndNil(FMarkedRange);
  if Assigned(FMarkers) then
    FreeAndNil(FMarkers);
  FreeAndNil(FTabHelper);
  if Assigned(FCharSizer) and (FCharSizer<>GlobalCharSizer) then
    FreeAndNil(FCharSizer);
  if Assigned(FAttribs) then
    FreeAndNil(FAttribs);
  FreeAndNil(FGutter);
  FreeAndNil(FWrapTemps);
  FreeAndNil(FWrapInfo);
  FreeAndNil(FStringsInt);
  if Assigned(FGutterDecor) then
    FreeAndNil(FGutterDecor);
  FreeAndNil(FBitmap);
  FreeAndNil(FLinkCache);
  FreeAndNil(FFontItalic);
  FreeAndNil(FFontBold);
  FreeAndNil(FFontBoldItalic);
  FreeAndNil(FCaretShapeNormal);
  FreeAndNil(FCaretShapeOverwrite);
  FreeAndNil(FCaretShapeReadonly);
  inherited;
end;

procedure TATSynEdit.Update(AUpdateWrapInfo: boolean=false; AForceRepaint: boolean=false; AForceOnScroll: boolean=false; AUpdateTempSel: boolean=false);
begin
  if not IsRepaintEnabled then exit;

  if AUpdateWrapInfo then
    FWrapUpdateNeeded:= true;

  //do it here to allow apps to remove carets and call Update + CalcLineHiliteEx to get LineParts w/o selections
  if AUpdateTempSel then
    Carets.GetSelections(FSel);

  InvalidateEx(AForceRepaint, AForceOnScroll);
end;

procedure TATSynEdit.SetFocus;
begin
  if HandleAllocated then
    LCLIntf.SetFocus(Handle);

  {$ifdef darwin}
  //why we don't enable this on Windows: it will give regression in CudaText:
  //click on code-tree filter, then click on main editor: now both editor + filter have blinking caret

  //DoEnter is not called, so caret don't blink in ui-tab opened by Command+N
  DoEnter;
  {$endif}
end;

procedure TATSynEdit.GetClientSizes(out W, H: integer);
begin
  W:= Width;
  H:= Height;
  if ModeOneLine then exit;

  if FOptScrollbarsNew then //better check this instead of FScrollbarVert.Visible
  begin
    Dec(W, FScrollbarVert.Width);
    if FScrollbarHorz.Visible then
      Dec(H, FScrollbarHorz.Height);
  end
  else
  begin
    W:= inherited ClientWidth;
    H:= inherited ClientHeight;
  end;

  if W<1 then W:= 1;
  if H<1 then H:= 1;
end;

procedure TATSynEdit.LoadFromFile(const AFilename: string; AOptions: TATLoadStreamOptions);
begin
  if not FileExists(AFilename) then exit;
  TimerBlinkDisable;

  FCarets.Clear;
  FCarets.Add(0, 0);

  Strings.Clear(false{AWithEvent});
  FWrapInfo.Clear;
  FWrapUpdateNeeded:= true;

  if not (TATLoadStreamOption.KeepScroll in AOptions) then
  begin
    FScrollHorz.Clear;
    FScrollVert.Clear;
  end;

  BeginUpdate;
  try
    FFileName:= '';
    Strings.LoadFromFile(AFilename, AOptions);
    FFileName:= AFileName;
  finally
    EndUpdate;

    ModifiedOptions:= [];
    FLastHotspot:= -1;
    FLastCaretY:= -1;

    Update;
    TimerBlinkEnable;
  end;

  DoEventChange(0, false{AllowOnChange}); //calling OnChange makes almost no sense on opening file
end;

procedure TATSynEdit.SaveToFile(const AFilename: string);
var
  St: TATStrings;
  bChange1, bChange2, bChange3: boolean;
  NPrevUndoLimit: integer;
begin
  St:= Strings;
  bChange1:= false;
  bChange2:= false;
  bChange3:= false;

  {
  decrease chance of memory-overflow (by Undo items), which may happen when doing
  trim-trailing-spaces with lot of lines with trailing spaces
  }
  NPrevUndoLimit:= St.UndoLimit;
  if not St.UndoAfterSave then
    St.UndoLimit:= 0;

  if FOptSavingForceFinalEol then
    bChange1:= St.ActionEnsureFinalEol;

  if FOptSavingTrimSpaces then
  begin
    bChange2:= St.ActionTrimSpaces(TATTrimSpaces.Right);
    //caret may be after end-of-line, so fix it
    if not OptCaretVirtual then
      DoCaretsFixIncorrectPos(true);
  end;

  if FOptSavingTrimFinalEmptyLines then
  begin
    bChange3:= St.ActionTrimFinalEmptyLines;
    if bChange3 then
      DoCaretsFixIncorrectPos(false);
  end;

  if bChange1 or bChange2 or bChange3 then
  begin
    Update(true);
    DoEventChange;
  end;

  St.SaveToFile(AFilename);
  if not St.UndoAfterSave then
    St.UndoLimit:= NPrevUndoLimit;

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
  AValue:= Max(AValue, ATEditorOptions.MinCaretTime);
  AValue:= Min(AValue, ATEditorOptions.MaxCaretTime);
  FCaretBlinkTime:= AValue;
  FTimerBlink.Interval:= AValue;
end;

procedure TATSynEdit.SetSpacingTop(AValue: integer);
begin
  if FSpacingTop=AValue then Exit;
  FSpacingTop:= AValue;
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetSpacingBottom(AValue: integer);
begin
  if FSpacingBottom=AValue then Exit;
  FSpacingBottom:= AValue;
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetMarginString(const AValue: string);
var
  Sep: TATStringSeparator;
  N: integer;
begin
  FMarginList:= nil;
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
  if not FMicromapOnScrollbar then
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
  St: TATStrings;
begin
  Carets.OneLine:= AValue;
  St:= Strings;
  St.OneLine:= AValue;

  if AValue then
  begin
    OptGutterVisible:= false;
    OptRulerVisible:= false;
    OptMinimapVisible:= false;
    OptCaretVirtual:= false;
    OptCaretManyAllowed:= false;
    OptUnprintedVisible:= false;
    OptWrapMode:= TATEditorWrapMode.ModeOff;
    OptScrollStyleHorz:= TATEditorScrollbarStyle.Hide;
    OptScrollStyleVert:= TATEditorScrollbarStyle.Hide;
    //OptMouseDragDrop:= false;
    OptMarginRight:= 1000;
    OptUndoLimit:= 200;

    DoCaretSingle(0, 0);

    while St.Count>1 do
      St.LineDelete(St.Count-1, false, false, false);
  end
  else
  begin
    OptScrollStyleHorz:= TATEditorScrollbarStyle.Auto;
    OptScrollStyleVert:= TATEditorScrollbarStyle.Show;
  end;
end;

procedure TATSynEdit.SetReadOnly(AValue: boolean);
begin
  if not FOptAllowReadOnly then Exit;
  Strings.ReadOnly:= AValue;
end;

procedure TATSynEdit.SetLineTop(AValue: integer);
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

  DoScroll_LineTop(AValue, true);
end;

procedure TATSynEdit.DoScroll_SetPos(var AScrollInfo: TATEditorScrollInfo; APos: integer);
begin
  //note: don't limit value by AScrollInfo.NPosLast, this gives regression: all files open at top
  AScrollInfo.NPos:= APos;
  //must update other info in AScrollInfo
  UpdateScrollbars(true);
end;

procedure TATSynEdit.DoScroll_LineTop(ALine: integer; AUpdate: boolean);
var
  NFrom, NTo, i: integer;
begin
  if FWrapInfo=nil then exit;

  if (ALine<=0) or (FWrapInfo.Count=0) then
  begin
    FScrollVert.SetZero;
    if AUpdate then Update;
    Exit
  end;

  //find exact match
  FWrapInfo.FindIndexesOfLineNumber(ALine, NFrom, NTo);
  if NFrom>=0 then
  begin
    DoScroll_SetPos(FScrollVert, NFrom);
    if AUpdate then Update;
    Exit
  end;

  //find approx match
  for i:= 0 to FWrapInfo.Count-1 do
    with FWrapInfo[i] do
      if NLineIndex>=ALine then
      begin
        DoScroll_SetPos(FScrollVert, i);
        if AUpdate then Update;
        Exit
      end;
end;

procedure TATSynEdit.SetColumnLeft(AValue: integer);
begin
  DoScroll_SetPos(FScrollHorz, AValue);
  Update;
end;

procedure TATSynEdit.SetLinesFromTop(AValue: integer);
begin
  with FScrollVert do
    NPos:= Max(0, Min(NPosLast, NPos + (GetLinesFromTop - AValue)));
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
  if ModeOneLine then
  begin
    Result.X:= OptTextOffsetLeft;
    Result.Y:= OptTextOffsetTop;
    exit;
  end;

  if FOptGutterVisible then
    NGutterWidth:= Gutter.Width
  else
    NGutterWidth:= 0;

  if FOptTextCenteringCharWidth>0 then
    Result.X:= Max(0, (ClientWidth - NGutterWidth -
                       FOptTextCenteringCharWidth * FCharSize.XScaled div ATEditorCharXScale) div 2)
  else
    Result.X:= OptTextOffsetLeft;

  Inc(Result.X, NGutterWidth);

  Result.Y:= OptTextOffsetTop;

  if FSpacingBottom<0 then
    Result.Y:= Max(Result.Y, -FSpacingBottom*2); //*2 is needed to not clip the first line

  if FOptRulerVisible then
    Inc(Result.Y, FRulerHeight);
end;

function TATSynEdit.GetPageLines: integer;
begin
  case FOptKeyPageUpDownSize of
    TATEditorPageDownSize.Full:
      Result:= GetVisibleLines;
    TATEditorPageDownSize.FullMinus1:
      Result:= GetVisibleLines-1;
    TATEditorPageDownSize.Half:
      Result:= GetVisibleLines div 2;
  end;
end;


procedure TATSynEdit.DoPaintAll(C: TCanvas; ALineFrom: integer);
var
  NColorOther: TColor;
  NBlend: integer;
begin
  UpdateInitialVars(C);

  FColorFont:= Colors.TextFont;
  FColorBG:= Colors.TextBG;
  FColorGutterBG:= Colors.GutterBG;
  FColorGutterFoldBG:= Colors.GutterFoldBG;
  FColorRulerBG:= Colors.RulerBG;
  FColorCollapseMarkBG:= Colors.CollapseMarkBG;

  if Enabled then
  begin
    if FOptDimUnfocusedBack<>0 then
      if not _IsFocused then
      begin
        if FOptDimUnfocusedBack>0 then
          NColorOther:= clBlack
        else
          NColorOther:= clWhite;
        NBlend:= Abs(FOptDimUnfocusedBack);

        FColorBG:= ColorBlend(NColorOther, FColorBG, NBlend);
        FColorGutterBG:= ColorBlend(NColorOther, FColorGutterBG, NBlend);
        FColorGutterFoldBG:= ColorBlend(NColorOther, FColorGutterFoldBG, NBlend);
        FColorRulerBG:= ColorBlend(NColorOther, FColorRulerBG, NBlend);
        FColorCollapseMarkBG:= ColorBlend(NColorOther, FColorCollapseMarkBG, NBlend);
      end;
  end
  else
  begin
    FColorFont:= Colors.TextDisabledFont;
    FColorBG:= Colors.TextDisabledBG;
  end;

  Inc(FPaintCounter);
  FVisibleColumns:= GetVisibleColumns;
  FCaretShown:= false;
  Carets.GetSelections(FSel);

  if Assigned(FAdapterHilite) then
    FAdapterIsDataReady:= FAdapterHilite.IsDataReady
  else
    FAdapterIsDataReady:= true;

  UpdateGapForms(true);
  DoPaintMain(C, ALineFrom);
  UpdateGapForms(false);
  UpdateCaretsCoords(false, true);

  if Carets.Count>0 then
  begin
    //we paint line only for 1st caret
    //no need to paint for all carets,
    //it will be bad for e.g. 100 carets
    if FOptShowCurColumn then
      DoPaintMarginLineTo(C, Carets[0].CoordX, ATEditorScale(1), Colors.MarginCaret);

    DoPaintRulerCaretMarks(C);
  end;

  UpdateMarkersCoords;
  DoPaintMarkersTo(C);
  DoPaintMarkerOfDragDrop(C);
end;


procedure TATSynEdit.DoPaint(ALineFrom: integer);
begin
  Exclude(FPaintFlags, TATEditorInternalFlag.RepaintNeeded);
  if csLoading in ComponentState then exit;
  if csDestroying in ComponentState then exit;

  UpdateTabHelper;

  if DoubleBuffered then
  begin
    if Assigned(FBitmap) then
      if TATEditorInternalFlag.Bitmap in FPaintFlags then
      begin
        FBitmap.BeginUpdate(true);
        try
          DoPaintAll(FBitmap.Canvas, ALineFrom);
        finally
          FBitmap.EndUpdate();
        end;
      end;
  end
  else
    DoPaintAll(Canvas, ALineFrom);

  if UpdateScrollbars(false) then
    Include(FPaintFlags, TATEditorInternalFlag.RepaintNeeded);
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
  C.FillRect(Rect(0, 0, Width, Height));

  if Strings.ProgressKind<>TATStringsProgressKind.Saving then
    Bmp:= ATEditorBitmaps.BitmapWait
  else
    Bmp:= ATEditorBitmaps.BitmapSaving;
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

  if Assigned(OnPaint) then
    OnPaint(Self);

  FPaintWorking:= true;
  try
    if TATEditorInternalFlag.Resize in FPaintFlags then
    begin
      Exclude(FPaintFlags, TATEditorInternalFlag.Resize);
      if DoubleBuffered then
        if Assigned(FBitmap) then
          BitmapResizeBySteps(FBitmap, Width, Height);
    end;

    //-1 means "auto detect from FScrollVert"
    NLine:= -1;
    //if FLineTopTodo is set, use this var (usually on the first painting) and clear the var
    if FLineTopTodo>0 then
    begin
      NLine:= FLineTopTodo;
      FLineTopTodo:= 0;
    end;

    FPaintStarted:= true;
    PaintEx(NLine);
  finally
    FPaintWorking:= false;
    ATEditorOptions.EditorWasPaintedOnce:= true;
  end;

  if TATEditorInternalFlag.ScrollEventNeeded in FPaintFlags then
  begin
    Exclude(FPaintFlags, TATEditorInternalFlag.ScrollEventNeeded);
    DoEventScroll;
  end;

  DoEventEnabledUndoRedoChanged;
end;


procedure TATSynEdit.DoEventEnabledUndoRedoChanged;
var
  St: TATStrings;
  bEnabledUndo, bEnabledRedo: boolean;
begin
  if Assigned(FOnEnabledUndoRedoChanged) then
  begin
    St:= Strings;
    bEnabledUndo:= (not St.ReadOnly) and (not St.UndoEmpty);
    bEnabledRedo:= (not St.ReadOnly) and (not St.RedoEmpty);
    if (bEnabledUndo<>FLastEnabledUndo) or
       (bEnabledRedo<>FLastEnabledRedo) then
    begin
      FLastEnabledUndo:= bEnabledUndo;
      FLastEnabledRedo:= bEnabledRedo;
      FOnEnabledUndoRedoChanged(Self, bEnabledUndo, bEnabledRedo);
    end;
  end;
end;

function TATSynEdit.IsLexerNormal: boolean;
var
  S: string;
begin
  if FAdapterHilite=nil then
    exit(false);
  S:= FAdapterHilite.GetLexerName;
  Result:= (S<>'') and (S[Length(S)]<>'^');
  //suffix of lite lexers in CudaText = ' ^'
end;

function TATSynEdit.IsLexerNone: boolean;
var
  S: string;
begin
  if FAdapterHilite=nil then
    exit(true);
  S:= FAdapterHilite.GetLexerName;
  Result:= S='';
end;


procedure TATSynEdit.SetEditorIndex(AValue: integer);
begin
  if FEditorIndex=AValue then Exit;
  FEditorIndex:= AValue;
  FWrapInfo.EditorIndex:= AValue;
end;

procedure TATSynEdit.PaintEx(ALineNumber: integer);
begin
  //experimental, reduce flickering on typing in Markdown
  FOptAllowRepaintOnTextChange:= not IsLexerNormal;

  if IsLocked then
  begin
    DoPaintLockedWarning(Canvas);
    Exit
  end;

  if ATEditorOptions.DebugTiming then
  begin
    FTickAll:= GetTickCount64;
    FTickMinimap:= 0;
    FTickTextout:= 0;
  end;

  DoPaint(ALineNumber);

  //block is folded at the end? deleted something at the end? repaint needed
  if not FOptLastLineOnTop then
    if FScrollVert.NPos>FScrollVert.NPosLast then
      Include(FPaintFlags, TATEditorInternalFlag.RepaintNeeded);

  if TATEditorInternalFlag.RepaintNeeded in FPaintFlags then
    DoPaint(ALineNumber);
  Exclude(FPaintFlags, TATEditorInternalFlag.RepaintNeeded);
  Exclude(FPaintFlags, TATEditorInternalFlag.Bitmap);

  if DoubleBuffered then
  //buf mode: timer tick don't give painting of whole bitmap
  //(cIntFlagBitmap off)
  begin
    if not Assigned(FBitmap) then exit;
    DoPaintCarets(FBitmap.Canvas, true);
    Canvas.Draw(0, 0, FBitmap);
  end
  else
  //non-buf mode: timer tick clears whole canvas first.
  //we already painted bitmap above,
  //and now we invert carets or dont invert (use FCaretAllowNextBlink)
  begin
    if not FCaretBlinkEnabled or FCaretAllowNextBlink then
      DoPaintCarets(Canvas, true);
  end;

  if ATEditorOptions.DebugTiming then
  begin
    FTickAll:= GetTickCount64-FTickAll;
    DoPaintTiming(Canvas);
  end;
end;

procedure TATSynEdit.Resize;
begin
  inherited;
  if not IsRepaintEnabled then exit;

  //avoid setting FLineTopTodo, which breaks the v-scroll-pos, if huge line is wrapped
  //and v-scroll-pos is in the middle of this line
  if (Width=FLastControlWidth) and
    (Height=FLastControlHeight) then exit;
  FLastControlWidth:= Width;
  FLastControlHeight:= Height;

  FLineTopTodo:= GetLineTop;

  Include(FPaintFlags, TATEditorInternalFlag.Resize);
  if FWrapMode<>TATEditorWrapMode.ModeOff then
    FWrapUpdateNeeded:= true;

  if not FPaintStarted then exit;
  Invalidate;
end;

procedure TATSynEdit.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if not IsEnabled then //prevent popup menu if form is disabled, for CudaText plugins on Qt5
  begin
    Handled:= true;
    exit;
  end;

  if FOptGutterVisible and PtInRect(FRectGutter, MousePos) then
    if FMouseRightClickOnGutterIsHandled then
    begin
      Handled:= true;
      exit;
    end;

  InitMenuStd;
  inherited;
  if not Handled then
  begin
    DoHandleRightClick(MousePos.X, MousePos.Y);
    Handled:= true;
  end;
end;

procedure TATSynEdit.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
var
  R: TRect;
begin
  //to avoid flickering with white on app startup
  {$ifdef windows}
  if (not ATEditorOptions.EditorWasPaintedOnce) and (Msg.DC<>0) then
  begin
    Brush.Color:= Colors.TextBG;
    R.Left:= 0;
    R.Top:= 0;
    R.Width:= Width;
    R.Height:= Height;
    Windows.FillRect(Msg.DC, R, Brush.Reference.Handle);
  end;
  {$endif}

  //to remove flickering on resize and mouse-over
  Msg.Result:= 1;
end;

procedure TATSynEdit.DoHintShowForScrolling;
var
  S: string;
  P: TPoint;
  R: TRect;
begin
  if csDesigning in ComponentState then Exit;
  if not FOptShowScrollHint then Exit;

  if FHintWnd=nil then
    FHintWnd:= THintWindow.Create(Self);
  FHintWnd.Font:= Screen.HintFont;

  S:= ATEditorOptions.TextHintScrollPrefix+' '+IntToStr(LineTop+1);
  R:= FHintWnd.CalcHintRect(500, S, nil);

  P:= ClientToScreen(Point(ClientWidth-R.Width, 0));
  OffsetRect(R, P.X, P.Y);
  OffsetRect(R, -ATEditorOptions.HintScrollDx, ATEditorOptions.HintScrollDx);

  FHintWnd.ActivateHint(R, S);
  FHintWnd.Invalidate; //for Win
end;

procedure TATSynEdit.DoHintShowForBookmark(ALine: integer);
var
  St: TATStrings;
  S: string;
  P: TPoint;
  R: TRect;
begin
  if csDesigning in ComponentState then Exit;

  if FHintWnd=nil then
    FHintWnd:= THintWindow.Create(Self);
  FHintWnd.Font:= Screen.HintFont;

  S:= '';
  St:= Strings;

  //get hint of bookmark
  if Assigned(St.Bookmarks) then
    S:= St.Bookmarks.FindHintForLine(ALine);

  //get hint of decor
  if S='' then
    if Assigned(St.GutterDecor1) then
      S:= St.GutterDecor1.FindHintForLine(ALine);

  if S='' then
  begin
    DoHintHide;
    exit
  end;

  R:= FHintWnd.CalcHintRect(ATEditorOptions.HintBookmarkMaxWidth, S, nil);

  P:= Mouse.CursorPos;
  OffsetRect(R, P.X+ATEditorOptions.HintBookmarkDx, P.Y+ATEditorOptions.HintBookmarkDy);

  FHintWnd.ActivateHint(R, S);
  FHintWnd.Invalidate; //for Win
end;


procedure TATSynEdit.DoHintHide;
begin
  if Assigned(FHintWnd) then
    FHintWnd.Hide;
end;

procedure _UpdateScrollInfoFromSmoothPos(
  var AInfo: TATEditorScrollInfo;
  const APos: Int64;
  AWrapInfo: TATWrapInfo;
  AGaps: TATGaps);
//Note: for vertical bar, NPos=-1 means than we are before the first line, over top gap
var
  NPos, NPixels, NLineIndex: Int64;
  NSizeGapTop, NSizeGap0: Int64;
  bConsiderGaps: boolean;
begin
  AInfo.SmoothPos:= APos;
  bConsiderGaps:= AInfo.Vertical and (AGaps.Count>0);

  if APos<=0 then
  begin
    AInfo.SetZero;
    if bConsiderGaps then
      if AGaps.SizeOfGapTop>0 then
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
    NSizeGapTop:= AGaps.SizeOfGapTop;
    NSizeGap0:= AGaps.SizeOfGap0;

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
      if APos<NSizeGapTop+AInfo.CharSizeScaled div ATEditorCharXScale + NSizeGap0 then
      begin
        AInfo.NPos:= 0;
        AInfo.NPixelOffset:= APos-NSizeGapTop;
        exit;
      end;
  end;

  AInfo.NPos:= Min(APos * ATEditorCharXScale div AInfo.CharSizeScaled, AInfo.NMax);
  AInfo.NPixelOffset:= APos mod (AInfo.CharSizeScaled div ATEditorCharXScale);

  //consider Gaps for vert scrolling
  if bConsiderGaps then
  begin
    NPos:= Min(AInfo.NPos, AWrapInfo.Count-1);
    NPixels:= AInfo.NPixelOffset;

    repeat
      NLineIndex:= AWrapInfo.Data[NPos].NLineIndex - 1;
      NPixels:= APos - NPos* AInfo.CharSizeScaled div ATEditorCharXScale - AGaps.SizeForLineRange(-1, NLineIndex);
      if NPos=0 then Break;
      if NLineIndex=0 then Break;
      if NPixels>=0 then Break;
      Dec(NPos);
    until false;

    AInfo.NPos:= NPos;
    AInfo.NPixelOffset:= NPixels
  end;
end;

procedure TATSynEdit.UpdateScrollInfoFromSmoothPos(var AInfo: TATEditorScrollInfo; const APos: Int64);
begin
  _UpdateScrollInfoFromSmoothPos(AInfo, APos, WrapInfo, Gaps);
end;

function TATSynEdit.UpdateScrollInfoFromMessage(var AInfo: TATEditorScrollInfo; const AMsg: TLMScroll): boolean;
begin
  if AInfo.NMax<AInfo.NPage then
  begin
    AInfo.Clear;
    Exit(true);
  end;

  case AMsg.ScrollCode of
    SB_TOP:
      begin
        UpdateScrollInfoFromSmoothPos(AInfo, 0);
      end;

    SB_BOTTOM:
      begin
        UpdateScrollInfoFromSmoothPos(AInfo, AInfo.SmoothPosLast);
      end;

    SB_LINEUP:
      begin
        UpdateScrollInfoFromSmoothPos(AInfo, AInfo.SmoothPos-AInfo.CharSizeScaled div ATEditorCharXScale);
      end;

    SB_LINEDOWN:
      begin
        UpdateScrollInfoFromSmoothPos(AInfo, AInfo.SmoothPos+AInfo.CharSizeScaled div ATEditorCharXScale);
      end;

    SB_PAGEUP:
      begin
        UpdateScrollInfoFromSmoothPos(AInfo, AInfo.SmoothPos-AInfo.SmoothPage);
      end;

    SB_PAGEDOWN:
      begin
        UpdateScrollInfoFromSmoothPos(AInfo, AInfo.SmoothPos+AInfo.SmoothPage);
      end;

    SB_THUMBPOSITION:
      begin
        ////in 2016.12:
        ////must ignore message with AMsg.Msg set: LM_VSCROLL, LM_HSCROLL;
        ////we get it on macOS during window resize, not expected! moves v-scroll pos to 0.
        ////so added line:
        //if AMsg.Msg=0 then

        ////in 2022.07:
        ////line "if ..." removed, it causes bug on macOS 11: CudaText #4258
        UpdateScrollInfoFromSmoothPos(AInfo, AMsg.Pos);
      end;

    SB_THUMBTRACK:
      begin
        UpdateScrollInfoFromSmoothPos(AInfo, AMsg.Pos);
        if AInfo.Vertical then
          DoHintShowForScrolling;
      end;

    SB_ENDSCROLL:
      DoHintHide;
  end;

  //correct value (if -1)
  if AInfo.SmoothPos>AInfo.SmoothPosLast then
    UpdateScrollInfoFromSmoothPos(AInfo, AInfo.SmoothPosLast)
  else
  if AInfo.SmoothPos<0 then
    UpdateScrollInfoFromSmoothPos(AInfo, 0);

  Result:= AMsg.ScrollCode<>SB_THUMBTRACK;
end;

procedure TATSynEdit.WMVScroll(var Msg: TLMVScroll);
begin
  UpdateScrollInfoFromMessage(FScrollVert, Msg);
  InvalidateEx(true, true);
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

{$ifdef LCLCOCOA}
procedure TATSynEdit.COCOA_IM_COMPOSITION(var Message: TMessage);
begin
  Message.Result := PtrInt(FAdapterIME);
end;
{$endif}

procedure TATSynEdit.WMHScroll(var Msg: TLMHScroll);
begin
  UpdateScrollInfoFromMessage(FScrollHorz, Msg);
  InvalidateEx(true, true);
end;

procedure TATSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PosCoord: TATPoint;
  PosTextClicked: TPoint;
  PosDetails: TATEditorPosDetails;
  ActionId: TATEditorMouseAction;
  bClickOnSelection, bClickHandled, bUnfoldClickedPos: boolean;
  bOldColumnSelection, bClickToSelectColumn: boolean;
  NGutterIndex, NRangeIndex, NLineRangeEnd: integer;
  RectMinimapSel: TRect;
  Caret: TATCaretItem;
begin
  if not OptMouseEnableAll then exit;
  inherited;
  SetFocus;
  DoCaretForceShow;
  FMouseRightClickOnGutterIsHandled:= false;

  //support OptTextCenteringCharWidth which makes empty area on the left of FRectMain
  if (FOptTextCenteringCharWidth>0) and
    (X<FRectMain.Left) and
    (X>=FRectGutter.Right) then
    X:= FRectMain.Left;

  PosCoord:= ATPoint(X, Y);

  if Button=mbRight then
  begin
    PosTextClicked:= ClientPosToCaretPos(PosCoord, PosDetails);

    if FOptGutterVisible and ATPointInRect(FRectGutter, PosCoord) then
    begin
      NGutterIndex:= FGutter.FindIndexAtCoordX(PosCoord.X);
      bClickHandled:= false;
      DoEventClickGutter(NGutterIndex, PosTextClicked.Y, bClickHandled);
      if bClickHandled then
      begin
        FMouseRightClickOnGutterIsHandled:= true;
        exit;
      end;
    end;

    if FOptMouseRightClickMovesCaret then
    begin
      bClickOnSelection:= Carets.FindCaretContainingPos(PosTextClicked.X, PosTextClicked.Y)>=0;
      if not bClickOnSelection then //click over selection must never reset that selection, like in Notepad++
        if Strings.IsIndexValid(PosTextClicked.Y) then
        begin
          DoCaretSingle(PosTextClicked.X, PosTextClicked.Y);
          DoSelect_None;
          Invalidate;
         end;
    end;

    //right click during mouse selection: cancel the selection
    MouseUp(mbLeft, [], X, Y);
    exit;
  end;

  //handle Button=mbExtra1 and =mbExtra2: they must perform 'Browser Backward'/'Browser Forward'
  if HandleMouseDownToHandleExtraMouseButtons(Self, Button, Shift) then exit;

  FMouseDownCoordOriginal.X:= X;
  FMouseDownCoordOriginal.Y:= Y;
  FMouseDownCoord.X:= X + FScrollHorz.TotalOffset;
  FMouseDownCoord.Y:= Y + FScrollVert.TotalOffset;
  FMouseDownWithCtrl:= ssXControl in Shift;
  FMouseDownWithAlt:= ssAlt in Shift;
  FMouseDownWithShift:= ssShift in Shift;
  ActionId:= EditorMouseActionId(FMouseActions, Shift);

  if FMinimapVisible and ATPointInRect(FRectMinimap, PosCoord) then
  begin
    GetRectMinimapSel(RectMinimapSel);
    FMouseDownOnMinimap:= true;
    FMouseDragMinimapSelHeight:= RectMinimapSel.Height;
    if FMinimapDragImmediately then
    begin
      FCursorOnMinimap:= true;
      FMouseDragMinimap:= true;
      FMouseDragMinimapDelta:= FMouseDragMinimapSelHeight div 2;
      FMouseDownOnMinimap:= false;

      ////discussed at CudaText issue #4928, now minimap click scrolls to the clicked place
      if (Button=mbMiddle) or ((Button=mbLeft) and (ssShift in Shift)) then
        DoMinimapDrag(Y)
      else
      begin
        DoMinimapClick(Y);

        //important, so GetMinimapSelTop will return updated value
        UpdateScrollbars(true);
        UpdateScrollbarsOfMinimap;

        {
        //this block moves Mouse.CursorPos, which don't work in Wayland session
        FMouseDragMinimapDelta:= FMouseDragMinimapSelHeight div 2;
        FMouseDragHandlerDisabled:= true;
        Mouse.CursorPos:= ClientToScreen(Point(X, GetMinimapSelTop+FMouseDragMinimapSelHeight div 2));
        }

        //this block don't move Mouse.CursorPos
        FMouseDragMinimapDelta:= Y-GetMinimapSelTop;
        //see also bugreport: https://github.com/Alexey-T/CudaText/issues/5074#issuecomment-1547852513
      end;
    end
    else
    if ATPointInRect(RectMinimapSel, PosCoord) then
    begin
      FMouseDragMinimap:= true;
      FMouseDragMinimapDelta:= Y-RectMinimapSel.Top;
    end;
    Exit;
  end;

  //before PosTextClicked:=ClientPosToCaretPos need some preparing to affect
  //GenericClientPosToCaretPos params, for Alt+Shift+click (select column block from old origin / from caret);
  //CudaText issue #5757
  bClickToSelectColumn:= ActionId=TATEditorMouseAction.ClickAndSelVerticalBlock;
  if bClickToSelectColumn then
  begin
    bOldColumnSelection:= FMouseDownAndColumnSelection;
    FMouseDownAndColumnSelection:= true; //to set AVirtualPos=True in GenericClientPosToCaretPos
  end;

  PosTextClicked:= ClientPosToCaretPos(PosCoord,
    PosDetails,
    TATEditorGapCoordAction.ToLineEnd,
    bClickToSelectColumn
    );

  if bClickToSelectColumn then
  begin
    FMouseDownAndColumnSelection:= bOldColumnSelection;
  end;

  FCaretSpecPos:= false;
  FMouseDownOnEditingArea:= false;
  FMouseDownOnMinimap:= false;
  FMouseDownGutterLineNumber:= -1;
  FMouseDragDropping:= false;
  FMouseDragDroppingReal:= false;
  FMouseDragMinimap:= false;
  bClickOnSelection:= false;

  ClearSelRectPoints; //SelRect points will be set in MouseMove

  if Assigned(FAdapterIME) then
    FAdapterIME.Stop(Self, false);

  if MouseNiceScroll then
  begin
    MouseNiceScroll:= false;
    Exit
  end;

  if ATPointInRect(FRectMain, PosCoord) then
  begin
    FMouseDownOnEditingArea:= true;
    FMouseDownPnt:= PosTextClicked;
    bClickOnSelection:= Carets.FindCaretContainingPos(FMouseDownPnt.X, FMouseDownPnt.Y)>=0;

    if (FMouseDownWithAlt and not FMouseDownWithCtrl) or FOptMouseColumnSelectionWithoutKey then
      //note: hardcoded Alt key! but hard to rewrite via ActionId
    begin
      if not FMouseDownWithShift then
        FMouseDownPnt_ColumnSelOrigin:= FMouseDownPnt
      else
      if (FMouseDownPnt_ColumnSelOrigin.Y=-1) and (Carets.Count>0) then
        FMouseDownPnt_ColumnSelOrigin:= Point(Carets[0].PosX, Carets[0].PosY);
    end
    else
      FMouseDownPnt_ColumnSelOrigin:= Point(-1, -1);

    if Shift=[ssMiddle] then
      if DoHandleClickEvent(FOnClickMiddle) then Exit;

    //Ctrl+click on selection must not be ignored, but must start drag-drop with copying
    if ActionId=TATEditorMouseAction.MakeCaret then
      if bClickOnSelection then
        ActionId:= TATEditorMouseAction.ClickSimple;

    case ActionId of
      TATEditorMouseAction.ClickMiddle:
        begin
          case FOptMouseMiddleClickAction of
            TATEditorMiddleClickAction.Scrolling:
              begin
                if not ModeOneLine then
                begin
                  FMouseNiceScrollPos:= PosCoord;
                  MouseNiceScroll:= true;
                end;
              end;
            TATEditorMiddleClickAction.Paste:
              begin
                //set caret to the clicked position, like Kate and VSCode do:
                DoCaretSingle(PosTextClicked.X, PosTextClicked.Y);
                DoCommand(cCommand_ClipboardAltPaste, TATCommandInvoke.Internal); //uses PrimarySelection:TClipboard
              end;
            TATEditorMiddleClickAction.GotoDefinition:
              begin
                if (not ModeOneLine) and (cCommand_GotoDefinition>0) then
                begin
                  DoCaretSingle(PosTextClicked.X, PosTextClicked.Y);
                  DoCommand(cCommand_GotoDefinition, TATCommandInvoke.Internal);
                end;
              end;
          end;
          Exit
        end;

      TATEditorMouseAction.ClickSimple:
        begin
          ActionAddJumpToUndo;
          Strings.SetGroupMark;

          FSelRect:= cRectEmpty;
          DoCaretSingleAsIs;

          if Assigned(PosDetails.OnGapItem) then
          begin
            if Assigned(FOnClickGap) then
              FOnClickGap(Self, PosDetails.OnGapItem, PosDetails.OnGapPos);
          end;

          if FOptMouseDragDrop and bClickOnSelection then
          begin
            //DragMode must be dmManual, drag started by code
            FMouseDragDropping:= true;
          end
          else
          begin
            //avoid OnChangeCaretPos if click does nothing
            if Carets.Count=1 then
            begin
              Caret:= Carets[0];
              if (not Caret.IsSelection) and
                (Caret.PosX=FMouseDownPnt.X) and
                (Caret.PosY=FMouseDownPnt.Y) then
                exit;
            end;

            DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);

            bUnfoldClickedPos:= (FFoldStyle in cEditorFoldStylesUnfoldOnClick)
              //ignore click on fold-mark, because we handle double-click on it (select entire range)
              and not IsCoordInFoldedMark(X, Y);

            DoShowPos(
              FMouseDownPnt,
              FOptScrollIndentCaretHorz,
              FOptScrollIndentCaretVert,
              bUnfoldClickedPos,
              false,
              false //False is to fix CudaText issue #5139
              );
            DoSelect_None;

            if Assigned(FOnClickMoveCaret) then
              FOnClickMoveCaret(Self, Point(Carets[0].PosX, Carets[0].PosY), FMouseDownPnt);
          end;
        end;

      TATEditorMouseAction.ClickAndSelNormalBlock:
        begin
          FSelRect:= cRectEmpty;
          DoCaretSingleAsIs;
          //see CudaText issue #5221
          Carets[0].SelectToPoint_ByShiftClick(FMouseDownPnt.X, FMouseDownPnt.Y);
        end;

      TATEditorMouseAction.ClickAndSelVerticalBlock:
        begin
          if FOptMouseEnableColumnSelection then
          begin
            FSelRect:= cRectEmpty;
            DoCaretSingleAsIs;
            if FMouseDownPnt_ColumnSelOrigin.Y>=0 then
              DoSelect_ColumnBlock_FromPoints(
                FMouseDownPnt_ColumnSelOrigin,
                FMouseDownPnt
                )
            else
              DoSelect_ColumnBlock_FromPoints(
                Point(Carets[0].PosX, Carets[0].PosY),
                FMouseDownPnt
                );
          end;
        end;

      TATEditorMouseAction.MakeCaret:
        begin
          FSelRect:= cRectEmpty;
          DoCaretAddToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
        end;

      TATEditorMouseAction.MakeCaretsColumn:
        begin
          FSelRect:= cRectEmpty;
          DoCaretsColumnToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
        end;
    end; //case ActionId of
  end;

  if FOptGutterVisible and ATPointInRect(FRectGutter, PosCoord) then
  begin
    NGutterIndex:= FGutter.FindIndexAtCoordX(PosCoord.X);

    bClickHandled:= false;
    DoEventClickGutter(NGutterIndex, PosTextClicked.Y, bClickHandled);
    if bClickHandled then exit;

    //Shift+click on gutter numbers
    if ActionId=TATEditorMouseAction.ClickAndSelNormalBlock then
    begin
      NGutterIndex:= FGutter.FindIndexAtCoordX(X);
      if NGutterIndex=FGutterBandNumbers then
        if Carets.Count>0 then
        begin
          Caret:= Carets[0];
          PosTextClicked:= ClientPosToCaretPos(PosCoord, PosDetails);
          if Strings.IsIndexValid(PosTextClicked.Y) then
          begin
            //adjust caret, if one whole line is selected (by previous click on gutter number)
            if (Caret.PosX=0) and (Caret.EndX=0) and (Caret.EndY=Caret.PosY-1) then
            begin
              if (PosTextClicked.Y>=Caret.PosY) then
                Caret.Change(0, Caret.PosY-1, -1, -1);
            end
            else
            if (Caret.PosX=Strings.LinesLen[Caret.PosY]) and (Caret.EndX=0) and (Caret.EndY=Caret.PosY) then
            begin
              if (PosTextClicked.Y>=Caret.PosY) then
                Caret.Change(0, Caret.PosY, -1, -1);
            end;

            DoSelect_LineRange(PosTextClicked.Y, Point(Caret.PosX, Caret.PosY));
          end;
        end;
    end;

    //simple click on gutter numbers / foldbar
    if ActionId=TATEditorMouseAction.ClickSimple then
    begin
      if NGutterIndex=FGutterBandNumbers then
      begin
        if FOptMouseClickNumberSelectsLine then
        begin
          FSelRect:= cRectEmpty;
          FMouseDownGutterLineNumber:= PosTextClicked.Y;

          //click on gutter line number for 1st line of folded range? select the entire range
          if FOptMouseClickNumberSelectsFoldedRange then
            NRangeIndex:= FFold.FindRangeWithPlusAtLine(PosTextClicked.Y)
          else
            NRangeIndex:= -1;

          if (NRangeIndex>=0) and FFold.ItemPtr(NRangeIndex)^.Folded then
          begin
            NLineRangeEnd:= FFold.ItemPtr(NRangeIndex)^.Y2;
            if Strings.IsIndexValid(NLineRangeEnd+1) then
              DoSelect_LineRange(PosTextClicked.Y, Point(0, NLineRangeEnd+1))
            else
              DoSelect_LineRange(PosTextClicked.Y, Point(Strings.LinesLen[NLineRangeEnd], NLineRangeEnd));
          end
          else
            DoSelect_Line(PosTextClicked);
        end;
      end
      else
      if NGutterIndex=FGutterBandFolding then
      begin
        DoFoldbarClick_LineIndex(PosTextClicked.Y);
      end;
    end;
  end;

  if FMicromapVisible and not FMicromapOnScrollbar and ATPointInRect(FRectMicromap, PosCoord) then
    if ActionId=TATEditorMouseAction.ClickSimple then
    begin
      DoEventClickMicromap(X-FRectMicromap.Left, Y-FRectMicromap.Top);
      Exit
    end;

  //don't fire OnChangeCaretPos on right click
  if Button=mbRight then
    if not FOptMouseRightClickMovesCaret then
      exit;

  Carets.Sort;
  DoEventCarets;
  Update;
end;

procedure TATSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Str: atString;
  Caret: TATCaretItem;
  PosDetails: TATEditorPosDetails;
  PosCoord: TATPoint;
  PosTextClicked: TPoint;
  bMovedMinimal: boolean;
begin
  if not OptMouseEnableAll then exit;
  inherited;
  PosCoord:= ATPoint(X, Y);

  if FOptShowMouseSelFrame or FMouseDownAndColumnSelection then
    if FMouseDragCoord.X>=0 then
    begin
      bMovedMinimal:= IsPointsDiffByDelta(PosCoord, FMouseDownCoordOriginal, ATEditorOptions.MouseMoveSmallDelta);
      if bMovedMinimal then
        Invalidate;
    end;

  if ATPointInRect(FRectMinimap, PosCoord) then
  begin
    if FMouseDownOnMinimap then
    begin
      FMouseDownOnMinimap:= false;
      if not FMouseDragMinimap then
        DoMinimapClick(Y);
      FMouseDragMinimap:= false;
    end;
    ClearMouseDownVariables;
    Exit
  end;

  if ATPointInRect(ClientRect, PosCoord) and FMouseDragDropping and IsVisible then
    //check IsVisible because we may drag-drop to another editor control
  begin
    //drag-drop really started
    if FMouseDragDroppingReal then
    begin
      Strings.BeginUndoGroup;
      try
        DoDropText(not GetActualDragDropIsCopying);
      finally
        Strings.EndUndoGroup;
        Update;
      end;
    end
    else
    //mouse released w/o drag-drop
    begin
      PosTextClicked:= ClientPosToCaretPos(PosCoord, PosDetails);
      DoCaretSingle(PosTextClicked.X, PosTextClicked.Y);
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
  FMouseDownCoordOriginal:= ATPoint(-1, -1);
  FMouseDownCoord:= ATPoint(-1, -1);
  FMouseDownPnt:= Point(-1, -1);
  FMouseDownGutterLineNumber:= -1;
  FMouseDownDouble:= false;
  FMouseDownDouble_SelBegin:= Point(-1, -1);
  FMouseDownDouble_SelEnd:= Point(-1, -1);
  FMouseDownAndColumnSelection:= false;
  FMouseDownOnEditingArea:= false;
  FMouseDownOnMinimap:= false;
  FMouseDownWithCtrl:= false;
  FMouseDownWithAlt:= false;
  FMouseDownWithShift:= false;
  FMouseDragDropping:= false;
  FMouseDragDroppingReal:= false;
  FMouseDragMinimap:= false;
  FMouseDragCoord:= ATPoint(-1, -1);
  FMouseRightClickOnGutterIsHandled:= false;

  if Assigned(FTimerScroll) then
    FTimerScroll.Enabled:= false;
end;

procedure TATSynEdit.DoHandleRightClick(X, Y: integer);
var
  PosCoord: TPoint;
  NGutterIndex: integer;
begin
  PosCoord:= Point(X, Y);
  if PtInRect(FRectMain, PosCoord) then
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
  if FOptGutterVisible and PtInRect(FRectGutter, PosCoord) then
  begin
    NGutterIndex:= FGutter.FindIndexAtCoordX(X);
    if NGutterIndex=FGutterBandBookmarks then
      if Assigned(FMenuGutterBm) then FMenuGutterBm.PopUp;
    if NGutterIndex=FGutterBandNumbers then
      if Assigned(FMenuGutterNum) then FMenuGutterNum.PopUp;
    if NGutterIndex=FGutterBandFolding then
      if Assigned(FMenuGutterFold) then FMenuGutterFold.PopUp else DoMenuGutterFold;
  end
  else
  if FMinimapVisible and PtInRect(FRectMinimap, PosCoord) then
  begin
    if Assigned(FMenuMinimap) then FMenuMinimap.PopUp;
  end
  else
  if FMicromapVisible and not FMicromapOnScrollbar and PtInRect(FRectMicromap, PosCoord) then
  begin
    if Assigned(FMenuMicromap) then FMenuMicromap.PopUp;
  end
  else
  if FOptRulerVisible and PtInRect(FRectRuler, PosCoord) then
  begin
    if Assigned(FMenuRuler) then FMenuRuler.PopUp;
  end;
end;

procedure TATSynEdit.UpdateCursor;
var
  PntClient: TPoint;
begin
  if MouseNiceScroll then Exit;
  PntClient:= ScreenToClient(Mouse.CursorPos);
  if not PtInRect(ClientRect, PntClient) then exit;

  //if FMouseDragDropping and FMouseDragDroppingReal then
  if DragManager.IsDragging then
  begin
    //don't check here PtInRect(FRectMain, P), to have ok cursor
    //when dragging to another editor
    if ModeReadOnly then
      DragCursor:= crNoDrop
    else
    if GetActualDragDropIsCopying then
      DragCursor:= crMultiDrag
    else
      DragCursor:= crDrag;
    Cursor:= DragCursor;
  end
  else
  if PtInRect(FRectMain, PntClient) then
  begin
    if FMouseDownAndColumnSelection then
      Cursor:= FCursorColumnSel
    else
      Cursor:= FCursorText;
  end
  else
  if PtInRect(FRectGutterBm, PntClient) then
  begin
    if FMouseDownPnt.Y<0 then
      Cursor:= FCursorGutterBookmark;
  end
  else
  if PtInRect(FRectGutterNums, PntClient) then
  begin
    if FMouseDownPnt.Y<0 then
      Cursor:= FCursorGutterNumbers;
  end
  else
  if PtInRect(FRectMinimap, PntClient) then
    Cursor:= FCursorMinimap
  else
  if PtInRect(FRectMicromap, PntClient) then
    Cursor:= FCursorMicromap
  else
    Cursor:= crDefault;
end;


procedure _LimitPointByRect(var P: TATPoint; const R: TRect); inline;
begin
  if P.X<R.Left+1 then P.X:= R.Left+1;
  if P.X>R.Right then P.X:= R.Right;
  if P.Y<R.Top+1 then P.Y:= R.Top+1;
  if P.Y>R.Bottom then P.Y:= R.Bottom;
end;


procedure TATSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  PntCoord: TATPoint;
  PntText: TPoint;
  RectMainCopy: TRect;
  bOnMain, bOnMinimap, bOnMicromap,
  bOnGutter, bOnGutterNumbers, bOnGutterBookmk,
  bSelecting, bSelectingGutterNumbers: boolean;
  bSelectAdding, bSelectColumnLeft, bSelectColumnMiddle: boolean;
  bSelectShiftExpanding: boolean;
  bMovedMinimal: boolean;
  bUpdateForMinimap: boolean;
  bStartTimerScroll: boolean;
  bPntTextInited: boolean = false;
  nIndexHotspot, nIndexCaret: integer;
  {$ifdef windows}
  PntScreen: TPoint;
  nScreenDelta: integer;
  {$endif}
  Caret: TATCaretItem;
  //
  procedure UpdatePntText;
  var
    Details: TATEditorPosDetails;
  begin
    if not bPntTextInited then
    begin
      bPntTextInited:= true;
      PntText:= ClientPosToCaretPos(
        ATPoint(X, Y),
        Details,
        TATEditorGapCoordAction.ToLineEnd, //default value
        true //true: for CudaText issue #5551
        );
    end;
  end;
  //
begin
  if not OptMouseEnableAll then exit;
  inherited;

  PntCoord:= ATPoint(X, Y);
  PntText:= Point(-1, -1);
  UpdateCursor;

  //RectMainCopy is to handle auto-scrolling in CudaText Distraction-Free mode,
  //when FRectMain occupies the very bottom (and very left) of the screen
  RectMainCopy:= FRectMain;
  InflateRect(RectMainCopy, -2, -2);

  {$ifdef windows}
  //workaround for Lazarus Win32 bug: editor has too big height in CudaText distraction-free mode
  // https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40143
  PntScreen:= ClientToScreen(Point(0, 0));
  nScreenDelta:= PntScreen.Y+ClientHeight-Screen.DesktopRect.Bottom+3;
  if nScreenDelta>0 then
    Dec(RectMainCopy.Bottom, nScreenDelta);
  {$endif}

  bMovedMinimal:= IsPointsDiffByDelta(PntCoord, FMouseDownCoordOriginal, ATEditorOptions.MouseMoveSmallDelta);

  bSelecting:= (not FMouseDragDropping) and (FMouseDownPnt.X>=0);
  bSelectingGutterNumbers:= FMouseDownGutterLineNumber>=0;

  bSelectAdding:= bSelecting and (ssLeft in Shift) and FMouseDownWithCtrl and not FMouseDownWithAlt and not FMouseDownWithShift;
  bSelectColumnLeft:= bSelecting and (ssLeft in Shift) and not FMouseDownWithCtrl and FMouseDownWithAlt and not FMouseDownWithShift;
  bSelectColumnMiddle:= bSelecting and (ssMiddle in Shift) and FMouseDownWithCtrl and not FMouseDownWithAlt and not FMouseDownWithShift;
  bSelectShiftExpanding:= bSelecting and (ssLeft in Shift) and not FMouseDownWithCtrl and not FMouseDownWithAlt and FMouseDownWithShift;

  if bSelecting then
  begin
    FMouseDragCoord:= PntCoord;
    _LimitPointByRect(FMouseDragCoord, FRectMainVisible);
  end
  else
    FMouseDragCoord:= ATPoint(-1, -1);

  bOnMain:= ATPointInRect(RectMainCopy, PntCoord);
  bOnMinimap:= FMinimapVisible and ATPointInRect(FRectMinimap, PntCoord);
  bOnMicromap:= FMicromapVisible and not FMicromapOnScrollbar and ATPointInRect(FRectMicromap, PntCoord);
  bOnGutter:= FOptGutterVisible and ATPointInRect(FRectGutter, PntCoord);
  bOnGutterNumbers:= bOnGutter and ATPointInRect(FRectGutterNums, PntCoord);
  bOnGutterBookmk:= bOnGutter and ATPointInRect(FRectGutterBm, PntCoord);

  //detect cursor on minimap
  if FMinimapVisible then
  begin
    bUpdateForMinimap:= false;
    if bOnMinimap<>FCursorOnMinimap then
      bUpdateForMinimap:= true;
    FCursorOnMinimap:= bOnMinimap;
    if FMinimapTooltipVisible and bOnMinimap and (Shift*[ssLeft, ssMiddle]=[]) then
    begin
      FMinimapTooltipEnabled:= true;
      bUpdateForMinimap:= true;
    end
    else
      FMinimapTooltipEnabled:= false;
    if bUpdateForMinimap then
      Update;

    //mouse dragged on minimap
    //handle this before starting FTimerScroll (CudaText issues 2941, 2944)
    if FMouseDragMinimap then
    begin
      if bMovedMinimal then
        if (Shift=[ssMiddle]) or (Shift-[ssShift]=[ssLeft]) then
          if FMouseDragHandlerDisabled then
            FMouseDragHandlerDisabled:= false
          else
            DoMinimapDrag(Y);
      Exit
    end;
  end;

  //detect cursor on gutter
  if FOptGutterVisible then
  begin
    if not FOptGutterShowFoldAlways then
      if bOnGutter<>FCursorOnGutter then
        Invalidate;
    FCursorOnGutter:= bOnGutter;
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
    UpdatePntText;
    DoHintShowForBookmark(PntText.Y);
  end
  else
    DoHintHide;

  bStartTimerScroll:=
    FOptMouseEnableAll and
    (FOptMouseEnableNormalSelection or FOptMouseEnableColumnSelection) and
    (ssLeft in Shift) and
    (not bOnMain) and
    //auto-scroll must not work when cursor is over minimap/micromap
    (not bOnMinimap) and
    (not bOnMicromap) and
    ((not bOnGutter) or bOnGutterBookmk);
                        //cursor on bookmarks-column? allow. this gutter column can be the leftmost UI element.

  if bStartTimerScroll then
    InitTimerScroll;
  if Assigned(FTimerScroll) then
    FTimerScroll.Enabled:= bStartTimerScroll;

  FMouseAutoScrollDirection:= TATEditorDirection.None;
  if (Y<RectMainCopy.Top) and (not ModeOneLine) then
    FMouseAutoScrollDirection:= TATEditorDirection.Up else
  if (Y>=RectMainCopy.Bottom) and (not ModeOneLine) then
    FMouseAutoScrollDirection:= TATEditorDirection.Down else
  if (X<RectMainCopy.Left) then
    FMouseAutoScrollDirection:= TATEditorDirection.Left else
  if (X>=RectMainCopy.Right) then
    FMouseAutoScrollDirection:= TATEditorDirection.Right;

  //mouse dragged on gutter numbers (only if drag started on gutter numbers)
  if bSelectingGutterNumbers then
    if bOnGutterNumbers then
    begin
      if Shift=[ssLeft] then
      begin
        UpdatePntText;
        if (PntText.Y>=0) and (PntText.X>=0) then
        begin
          DoSelect_LineRange(FMouseDownGutterLineNumber, PntText);
          Carets.Sort;
          DoEventCarets;
          Invalidate;
        end;
      end;
    Exit
  end;

  //mouse drag-drop just begins
  if FMouseDragDropping then
  begin
    if not FMouseDragDroppingReal and
      IsPointsDiffByDelta(ATPoint(X, Y), FMouseDownCoordOriginal, Mouse.DragThreshold) then
    begin
      FMouseDragDroppingReal:= true;
      BeginDrag(true);
    end
    else
    begin
      UpdatePntText;
      if PntText.Y>=0 then
      begin
        if PntText<>FDropMarker_TextPos then
        begin
          FDropMarker_TextPos:= PntText;
          FDropMarker_Coord:= CaretPosToClientPos(FDropMarker_TextPos);
          Invalidate; //Invalidate is needed even if nothing changed, just to paint drop-marker
        end;
      end;
    end;
    exit;
  end;

  //mouse just moved on text
  if bOnMain and (FMouseDownPnt.X<0) then
    begin
      if Shift*[ssLeft, ssRight]=[] then
        if Assigned(FHotspots) and (FHotspots.Count>0) then
        begin
          UpdatePntText;
          if PntText.Y>=0 then
          begin
            nIndexHotspot:= FHotspots.FindByPos(PntText.X, PntText.Y);
            if nIndexHotspot<>FLastHotspot then
            begin
              if FLastHotspot>=0 then
                if Assigned(FOnHotspotExit) then
                  FOnHotspotExit(Self, FLastHotspot);

              if nIndexHotspot>=0 then
                if Assigned(FOnHotspotEnter) then
                  FOnHotspotEnter(Self, nIndexHotspot);

              FLastHotspot:= nIndexHotspot;
            end;
          end;
        end;
      Exit
    end;

  //mouse dragged to select block
  if bSelecting then
    if bOnMain or bOnGutter or FMouseDownOnEditingArea then
    begin
      if (ssLeft in Shift) or bSelectColumnMiddle then
        if Carets.Count>0 then
        begin
          UpdatePntText;
          //Application.MainForm.Caption:= Format('MouseDownPnt %d:%d, CurPnt %d:%d',
            //[FMouseDownPnt.Y, FMouseDownPnt.X, PntText.Y, PntText.X]);
          if (PntText.Y<0) then Exit;

          //mouse not moved at least by char?
          if (FMouseDownPnt.X=PntText.X) and (FMouseDownPnt.Y=PntText.Y) and
            not bSelectShiftExpanding //check it to allow expanding of selection by Shift+[mouse drag]
          then
          begin
            //remove selection from current caret
            nIndexCaret:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
            if Carets.IsIndexValid(nIndexCaret) then
            begin
              Caret:= Carets[nIndexCaret];
              Caret.PosX:= PntText.X;
              Caret.PosY:= PntText.Y;
              Caret.EndX:= -1;
              Caret.EndY:= -1;
            end;
            Invalidate;
          end
          else
          begin
            //drag without Ctrl/Alt pressed: single selection
            //ssShift is allowed to select by Shift+MouseWheel
            if not FMouseDownWithCtrl and not FMouseDownWithAlt then
            begin
              if FOptMouseEnableColumnSelection and FOptMouseColumnSelectionWithoutKey then
              begin
                //column selection
                FMouseDownAndColumnSelection:= true;
                DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
                DoSelect_None;
                DoSelect_ColumnBlock_FromPoints(FMouseDownPnt, PntText);
              end
              else
              if FOptMouseEnableNormalSelection then
              begin
                //normal selection
                if not bSelectShiftExpanding then //check it to allow expanding of selection by Shift+[mouse drag]
                  DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
                //writeln('DoCaretSingle: '+inttostr(FMouseDownPnt.X)+':'+inttostr(FMouseDownPnt.Y));

                if FMouseDownDouble and FOptMouse2ClickDragSelectsWords then
                begin
                  if FMouseDownDouble_SelBegin<>FMouseDownPnt then
                  begin
                    if IsPosSorted(
                      FMouseDownDouble_SelBegin.X,
                      FMouseDownDouble_SelBegin.Y,
                      PntText.X,
                      PntText.Y,
                      false
                      ) then
                      DoSelect_WordRange(0, PntText, FMouseDownDouble_SelBegin)
                    else
                    if IsPosSorted(
                      PntText.X,
                      PntText.Y,
                      FMouseDownDouble_SelEnd.X,
                      FMouseDownDouble_SelEnd.Y,
                      false
                      ) then
                      DoSelect_WordRange(0, PntText, FMouseDownDouble_SelEnd);
                  end
                  else
                    DoSelect_WordRange(0, PntText, FMouseDownPnt);
                end
                else
                begin
                  Carets[0].SelectToPoint(PntText.X, PntText.Y);
                  //writeln('Carets[0].SelToPnt: '+inttostr(PntText.X)+':'+inttostr(PntText.Y));
                end;
              end;
            end;

            //drag with Ctrl pressed: add selection
            if bSelectAdding then
            begin
              nIndexCaret:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
              if Carets.IsIndexValid(nIndexCaret) then
                Carets[nIndexCaret].SelectToPoint(PntText.X, PntText.Y);
            end;

            //drag with Alt pressed: column selection
            //middle button drag with Ctrl pressed: the same
            if FOptMouseEnableColumnSelection then
              if bSelectColumnLeft or bSelectColumnMiddle then
              begin
                FMouseDownAndColumnSelection:= true;
                DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
                DoSelect_None;
                DoSelect_ColumnBlock_FromPoints(FMouseDownPnt, PntText);
              end;

            Carets.Sort;
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
  DoHideAllTooltips;
end;

function TATSynEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  if not OptMouseEnableAll then exit(false);
  inherited;
  Result:= DoMouseWheelAction(Shift, WheelDelta, false)
end;

function TATSynEdit.DoMouseWheelHorz(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  if not OptMouseEnableAll then exit(false);

  Result:= DoMouseWheelAction([], -WheelDelta, true);
end;

type
  TATMouseWheelMode = (
    Normal,
    Horiz,
    Zoom
    );

procedure TATSynEdit.DoHideAllTooltips;
var
  bUpdate: boolean;
begin
  bUpdate:= FMinimapTooltipEnabled;

  DoHintHide;
  DoHotspotsExit;
  if Assigned(FFoldedMarkTooltip) then
    FFoldedMarkTooltip.Hide;
  FMinimapTooltipEnabled:= false;

  if bUpdate then
    Update;
end;

function TATSynEdit.DoMouseWheelAction(Shift: TShiftState;
  AWheelDelta: integer; AForceHorz: boolean): boolean;
var
  WheelRecord: TATEditorWheelRecord;
  Mode: TATMouseWheelMode;
  Pnt: TPoint;
begin
  Result:= false;
  if not OptMouseEnableAll then exit;
  if ModeOneLine then exit;
  DoHideAllTooltips;

  if AForceHorz then
    Mode:= TATMouseWheelMode.Horiz
  else
  if (Shift=[FOptMouseWheelZoomsWithState]) then
    Mode:= TATMouseWheelMode.Zoom
  else
  // -[ssLeft] to ignore pressed mouse button
  if (Shift-[ssLeft]=[FOptMouseWheelScrollHorzWithState]) then
    Mode:= TATMouseWheelMode.Horiz
  else
  // -[ssLeft] to ignore pressed mouse button
  if (Shift-[ssLeft]=[]) then
    Mode:= TATMouseWheelMode.Normal
  else
    exit;

  WheelRecord:= Default(TATEditorWheelRecord);

  case Mode of
    TATMouseWheelMode.Normal:
      begin
        if FOptMouseWheelScrollVert then
        begin
          WheelRecord.Kind:= TATEditorWheelRecordKind.Vert;
          WheelRecord.Delta:= AWheelDelta;
          DoHandleWheelRecord(WheelRecord);
          Update;
        end;
        Result:= true;
      end;

    TATMouseWheelMode.Horiz:
      begin
        if FOptMouseWheelScrollHorz then
        begin
          WheelRecord.Kind:= TATEditorWheelRecordKind.Horz;
          WheelRecord.Delta:= AWheelDelta;
          DoHandleWheelRecord(WheelRecord);
          Update;
        end;
        Result:= true;
      end;

    TATMouseWheelMode.Zoom:
      begin
        if FOptMouseWheelZooms then
        begin
          WheelRecord.Kind:= TATEditorWheelRecordKind.Zoom;
          WheelRecord.Delta:= AWheelDelta;
          DoHandleWheelRecord(WheelRecord);
          Update;
        end;
        Result:= true;
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
  FoldMark: TATFoldedMark;
  MousePnt: TPoint;
begin
  if not OptMouseEnableAll then exit;
  inherited;

  if DoHandleClickEvent(FOnClickDbl) then Exit;

  if FOptMouse2ClickOnFoldMarkSelectsFoldedLines then
    if Assigned(FFoldedMarkList) and (FFoldedMarkList.Count>0) then
    begin
      MousePnt:= ScreenToClient(Mouse.CursorPos);
      FoldMark:= FFoldedMarkList.FindByCoord(MousePnt);
      if FoldMark.IsInited then
      begin
        DoSelect_LinesByFoldedMark(FoldMark.LineFrom, FoldMark.LineTo);
        exit;
      end;
    end;

  if FOptMouse2ClickOpensURL and FOptShowURLs and Assigned(FOnClickLink) then
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
    TATEditorDoubleClickAction.SelectEntireLine:
      begin
        DoSelect_Line_ByClick;
      end;
    TATEditorDoubleClickAction.SelectWordChars:
      begin
        FMouseDownDouble:= true;
        DoSelect_ByDoubleClick(true);
      end;
    TATEditorDoubleClickAction.SelectAnyChars:
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
var
  Caret: TATCaretItem;
begin
  if not Strings.IsIndexValid(FMouseDownPnt.Y) then Exit;
  DoSelect_CharGroupAtPos(FMouseDownPnt, EditorIsPressedCtrl, AllowOnlyWordChars);
  Invalidate;

  if Carets.Count=1 then
  begin
    Caret:= Carets[0];
    FMouseDownDouble_SelBegin:= Caret.GetLeftEdge;
    FMouseDownDouble_SelEnd:= Caret.GetRightEdge;
  end;
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
  P0: TPoint;
  P: TATPoint;
  Details: TATEditorPosDetails;
begin
  P0:= ScreenToClient(Mouse.CursorPos);
  P:= ATPoint(P0.X, P0.Y);
  if ATPointInRect(FRectMain, P) then
  begin
    P0:= ClientPosToCaretPos(P, Details);
    if P0.Y<0 then Exit;
    DoSelect_Line(P0);
    Invalidate;
  end;
end;

(*
function TATSynEdit.IsInvalidateAllowed: boolean;
begin
  exit(true);

  {
  //solve CudaText issue #3461
  //but this gives random hangings on editing, e.g. CudaText #3475, also AT sees hangings on macOS
  if Assigned(AdapterForHilite) then
    Result:= AdapterForHilite.IsParsedAtLeastPartially
  else
    Result:= true;
    }

  { //debug
  if not Result then
    if Assigned(Application) and Assigned(Application.MainForm) then
      Application.MainForm.Caption:= 'skip invalidate: '+TimeToStr(Now)+', lexer: '+AdapterForHilite.GetLexerName;
    }
end;
*)

procedure TATSynEdit.Invalidate;
begin
  InvalidateEx(false, false);
end;

procedure TATSynEdit.InvalidateEx(AForceRepaint, AForceOnScroll: boolean);
begin
  if not IsRepaintEnabled then exit;
  //if not IsInvalidateAllowed then exit;

  if Assigned(AdapterForHilite) and
    AdapterForHilite.ImplementsDataReady and
    not AForceRepaint and
    not (TATEditorInternalFlag.Resize in FPaintFlags) then
  begin
    if FOptFlickerReducingPause>=1000 then
      //value 1000 is the special value of CudaText option "renderer_anti_flicker"
    begin
      if not AdapterForHilite.IsDataReadyPartially then
      begin
        exit;
      end;
    end
    else
    if FOptFlickerReducingPause>0 then
    begin
      FTimerFlicker.Enabled:= false;
      FTimerFlicker.Interval:= FOptFlickerReducingPause;
      FTimerFlicker.Enabled:= FTimersEnabled;
      exit;
    end;
  end;

  Include(FPaintFlags, TATEditorInternalFlag.Bitmap);
  inherited Invalidate;

  if AForceOnScroll then
    Include(FPaintFlags, TATEditorInternalFlag.ScrollEventNeeded);
end;

function TATSynEdit._IsFocused: boolean;
//this method is to speedup focused check (TControl.Focused prop is slower)
var
  C: TControl;
begin
  Result:= false;
  if not FIsEntered then
  begin
    //WriteLn('not focused 1');
    exit;
  end;
  if not IsLibrary and not Application.Active then
  begin
    //WriteLn('not focused 2');
    exit;
  end;

  C:= Self;
  while Assigned(C.Parent) do
    C:= C.Parent;

  //added IFNDEF because of CudaText issue #4350
  {$ifndef LCLGTK2}
  if C is TForm then
    if not (C as TForm).Active then
    begin
      //WriteLn('not focused 3');
      exit;
    end;
  {$endif}

  Result:= true;
end;

procedure TATSynEdit.TimerBlinkTick(Sender: TObject);
var
  bFocused: boolean;
begin
  if not FCaretShowEnabled then exit;
  if not IsLibrary then
    if not Application.Active then exit;

  if FCaretStopUnfocused then
  begin
    bFocused:= _IsFocused;
    if not bFocused then
      if FCaretShown then
        exit;
  end;

  if not DoubleBuffered then
    FCaretAllowNextBlink:= not FCaretAllowNextBlink;

  DoPaintCarets(Canvas, true);
end;

procedure TATSynEdit.TimerScrollTick(Sender: TObject);
var
  nIndexCaret: integer;
  P0: TPoint;
  PClient: TATPoint;
  PCaret: TPoint;
  Details: TATEditorPosDetails;
begin
  P0:= ScreenToClient(Mouse.CursorPos);
  PClient.X:= P0.X;
  PClient.Y:= P0.Y;
  PClient.X:= Max(FRectMain.Left, PClient.X);
  PClient.Y:= Max(FRectMain.Top, PClient.Y);
  PClient.X:= Min(FRectMain.Right, PClient.X);
  PClient.Y:= Min(FRectMain.Bottom, PClient.Y);

  case FMouseAutoScrollDirection of
    TATEditorDirection.Up:
      DoScrollByDelta(0, -ATEditorOptions.SpeedScrollAutoVert);
    TATEditorDirection.Down:
      DoScrollByDelta(0, ATEditorOptions.SpeedScrollAutoVert);
    TATEditorDirection.Left:
      DoScrollByDelta(-ATEditorOptions.SpeedScrollAutoHorz, 0);
    TATEditorDirection.Right:
      DoScrollByDelta(ATEditorOptions.SpeedScrollAutoHorz, 0);
    else
      Exit;
  end;

  PCaret:= ClientPosToCaretPos(PClient,
    Details,
    TATEditorGapCoordAction.ToLineEnd,
    true //to avoid selection to smaller X during column selection, CudaText issue #5756
    );

  if (PCaret.X>=0) and (PCaret.Y>=0) then
  begin
    if FMouseDownGutterLineNumber>=0 then
    begin
      DoSelect_LineRange(FMouseDownGutterLineNumber, PCaret);
    end
    else
    if IsSelRectEmpty and not FMouseDownAndColumnSelection then
    begin
      nIndexCaret:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
      if nIndexCaret>=0 then
        Carets[nIndexCaret].SelectToPoint(PCaret.X, PCaret.Y);
    end
    else
    begin
      DoSelect_ColumnBlock_FromPoints(FMouseDownPnt, PCaret);
    end;
  end;

  Carets.Sort;
  DoEventCarets;
  Invalidate;
end;

procedure TATSynEdit.TimerNiceScrollTick(Sender: TObject);
var
  Pnt: TPoint;
  Dx, Dy: integer;
  Dir: TATEditorDirection;
  NBitmapSize: integer;
begin
  Pnt:= ScreenToClient(Mouse.CursorPos);
  if not PtInRect(FRectMain, Pnt) then Exit;

  ATEditorBitmaps.InitCursorsForNiceScroll;

  //delta in pixels
  Dx:= Pnt.X-FMouseNiceScrollPos.X;
  Dy:= Pnt.Y-FMouseNiceScrollPos.Y;

  NBitmapSize:= ATEditorBitmaps.BitmapNiceScroll.Width;
  if (Abs(Dx)<=NBitmapSize div 2) and
    (Abs(Dy)<=NBitmapSize div 2) then
    begin
      Cursor:= crNiceScrollNone;
      Exit;
    end;

  if (Dy<0) and (Abs(Dy)>Abs(Dx)) then Dir:= TATEditorDirection.Up else
    if (Dy>0) and (Abs(Dy)>Abs(Dx)) then Dir:= TATEditorDirection.Down else
      if Dx<0 then Dir:= TATEditorDirection.Left else
        Dir:= TATEditorDirection.Right;

  case Dir of
    TATEditorDirection.Left:
      Cursor:= crNiceScrollLeft;
    TATEditorDirection.Right:
      Cursor:= crNiceScrollRight;
    TATEditorDirection.Up:
      Cursor:= crNiceScrollUp;
    TATEditorDirection.Down:
      Cursor:= crNiceScrollDown;
  end;

  //delta in pixels
  Dx:= Sign(Dx)*((Abs(Dx)-NBitmapSize div 2) + 1) div ATEditorOptions.SpeedScrollNice;
  Dy:= Sign(Dy)*((Abs(Dy)-NBitmapSize div 2) + 1) div ATEditorOptions.SpeedScrollNice;

  if Dir in [TATEditorDirection.Left, TATEditorDirection.Right] then
    DoScrollByDeltaInPixels(Dx, 0)
  else
    DoScrollByDeltaInPixels(0, Dy);

  Invalidate;
end;


procedure TATSynEdit.DoPaintCaretShape(C: TCanvas; ARect: TRect;
  ACaret: TATCaretItem; ACaretShape: TATCaretShape);
var
  NCoordX, NCoordY: Int64;
begin
  if not FCaretBlinkEnabled and ACaretShape.IsNarrow then
  begin
    C.Brush.Color:= (not (Colors.Caret xor Colors.TextBG)) and $ffffff;
    C.FillRect(ARect);
    exit;
  end;

  if ACaretShape.EmptyInside then
  begin
    CanvasInvertRectEmptyInside(C, ARect, Colors.Caret);
    exit;
  end;

  CanvasInvertRect(C, ARect, Colors.Caret);

  if ATEditorOptions.CaretTextOverInvertedRect and not ACaretShape.IsNarrow then
  begin
    if (ACaret.CharAtCaret<>#0) and (ACaret.CharColor<>clNone) and not IsCharUnicodeSpace(ACaret.CharAtCaret) then
    begin
      C.Font.Color:= ACaret.CharColor;
      C.Font.Style:= ACaret.CharStyles;
      C.Brush.Style:= bsClear;
      NCoordX:= ACaret.CoordX;
      NCoordY:= ACaret.CoordY;
      if FSpacingBottom<0 then
        Inc(NCoordY, FSpacingBottom);
      CanvasTextOutSimplest(C, NCoordX, NCoordY, UnicodeString(ACaret.CharAtCaret));
    end;
  end;
end;

procedure TATSynEdit.DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
var
  Caret: TATCaretItem;
  CaretShape: TATCaretShape;
  R, RectCaretOld: TRect;
  NCharWidth: integer;
  iCaret: integer;
begin
  if csLoading in ComponentState then exit;
  if csDestroying in ComponentState then exit;
  if not FCaretShowEnabled then exit;

  //disable InvalidateRect during Paint
  if (csCustomPaint in ControlState) then
    AWithInvalidate:= false;
  //if not IsInvalidateAllowed then
  //  AWithInvalidate:= false;

  if ModeReadOnly then
    CaretShape:= FCaretShapeReadonly
  else
  if ModeOverwrite then
    CaretShape:= FCaretShapeOverwrite
  else
    CaretShape:= FCaretShapeNormal;

  if FCaretBlinkEnabled then
    FCaretShown:= not FCaretShown
  else
    FCaretShown:= true;

  NCharWidth:= FCharSize.XScaled div ATEditorCharXScale;

  for iCaret:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[iCaret];
    if Caret.CoordX=-1 then Continue;

    //check caret is visible (IntersectRect is slower)
    if Caret.CoordX>=FRectMain.Right then Continue;
    if Caret.CoordY>=FRectMain.Bottom then Continue;
    if Caret.CoordX+NCharWidth<=FRectMain.Left then Continue;
    if Caret.CoordY+FCharSize.Y<=FRectMain.Top then Continue;

    R.Left:= Caret.CoordX;
    R.Top:= Caret.CoordY;
    R.Right:= R.Left+NCharWidth;
    R.Bottom:= R.Top+FCharSize.Y;

    if FMinimapTooltipVisible and FMinimapTooltipEnabled and (FRectMinimapTooltip.Width>0) then
      if (R.Left>=FRectMinimapTooltip.Left) and
        (R.Right<=FRectMinimapTooltip.Right) and
        (R.Top>=FRectMinimapTooltip.Top) and
        (R.Bottom<=FRectMinimapTooltip.Bottom) then
        Continue;

    DoCaretsApplyShape(R, CaretShape, NCharWidth, FCharSize.Y);

    if FCaretBlinkEnabled then
    begin
      //this block is to solve 'ghost caret on typing', CudaText issue #3167
      //check (Caret.OldRect<>R) is to fix CudaText issue #5054
      if not FCaretShown and (Caret.OldRect<>R) then
      begin
        RectCaretOld:= Caret.OldRect;
        if RectCaretOld.Width>0 then
        begin
          CanvasInvertRect(C, RectCaretOld, Colors.Caret);
          if AWithInvalidate then
          begin
            {$ifdef darwin}
            //CudaText issue #4250
            InflateRect(RectCaretOld, 1, 1);
            {$endif}
            InvalidateRect(Handle, @RectCaretOld, false);
          end;
        end;
      end;

      DoPaintCaretShape(C, R, Caret, CaretShape);
    end
    else
    begin
      DoPaintCaretShape(C, R, Caret, CaretShape);
    end;

    Caret.OldRect:= R;

    if AWithInvalidate then
    begin
      {$ifdef darwin}
      //CudaText issue #4250
      InflateRect(R, 1, 1);
      {$endif}
      InvalidateRect(Handle, @R, false);
    end;
  end;
end;


procedure TATSynEdit.DoPaintMarkerOfDragDrop(C: TCanvas);
var
  NMarkWidth: integer;
  PntCoord: TATPoint;
  R: TRect;
begin
  if not FOptShowDragDropMarker then exit;

  //if not FMouseDragDropping or not FMouseDragDroppingReal then exit;
  ////drag-drop from another control may be active
  if not DragManager.IsDragging then exit;

  PntCoord:= FDropMarker_Coord;
  if PntCoord.Y<0 then exit;
  if not ATPointInRect(FRectMain, PntCoord) then exit;

  NMarkWidth:= ATEditorScale(FOptShowDragDropMarkerWidth);
  R.Left:= PntCoord.X - NMarkWidth div 2;
  R.Right:= R.Left + NMarkWidth;
  R.Top:= PntCoord.Y;
  R.Bottom:= R.Top + FCharSize.Y; //100% height

  C.Brush.Color:= Colors.DragDropMarker;
  C.FillRect(R);
end;

procedure TATSynEdit.TimerBlinkDisable;
begin
  if ATEditorOptions.UsePaintStatic then
    FTimerBlink.Enabled:= false;
end;

procedure TATSynEdit.TimerBlinkEnable;
begin
  if ATEditorOptions.UsePaintStatic then
  begin
    FTimerBlink.Enabled:= false;
    FTimerBlink.Enabled:= FTimersEnabled and FCaretBlinkEnabled;
  end;
end;


procedure TATSynEdit.DoPaintLineIndent(C: TCanvas;
  const ARect: TRect;
  const ACharSize: TATEditorCharSize;
  ACoordY: integer;
  AIndentSize: integer;
  AColorBG: TColor;
  AScrollPos: integer;
  AShowIndentLines: boolean);
var
  i: integer;
  RBack: TRect;
begin
  if AIndentSize=0 then Exit;

  RBack:= Rect(0, 0, AIndentSize*ACharSize.XScaled div ATEditorCharXScale, ACharSize.Y);
  OffsetRect(RBack, ARect.Left-AScrollPos*ACharSize.XScaled div ATEditorCharXScale, ACoordY);

  C.Brush.Color:= AColorBG;
  C.FillRect(RBack);

  if AShowIndentLines then
    for i:= 0 to AIndentSize-1 do
      if i mod FOptTabSize = 0 then
        CanvasLine_DottedVertAlt(C,
          Colors.IndentVertLines,
          ARect.Left + (i-AScrollPos)*ACharSize.XScaled*ACharSize.XSpacePercents div ATEditorCharXScale div 100,
          ACoordY,
          ACoordY+ACharSize.Y);
end;

procedure TATSynEdit.DoPaintSelectedLineBG(C: TCanvas;
  const ACharSize: TATEditorCharSize;
  const AVisRect: TRect;
  APointLeft, APointText: TPoint;
  const AWrapItem: TATWrapItem;
  ALineWidth: integer;
  const AScrollHorz: TATEditorScrollInfo);
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
   if FWrapMode=TATEditorWrapMode.ModeOff then
    if (NLineIndex>=FSelRect.Top) and (NLineIndex<=FSelRect.Bottom) then
    begin
      NLeft:= APointLeft.X+ACharSize.XScaled*(FSelRect.Left-AScrollHorz.NPos) div ATEditorCharXScale;
      NRight:= NLeft+ACharSize.XScaled*FSelRect.Width div ATEditorCharXScale;
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
        if (AWrapItem.NFinal=TATWrapItemFinal.Middle) then
          Continue;

      NLeft:= APointText.X + ALineWidth + (RangeFrom-NPartXAfter)*ACharSize.XScaled div ATEditorCharXScale;
      if RangeTo=MaxInt then
        NRight:= AVisRect.Right
      else
        NRight:= NLeft+(RangeTo-RangeFrom)*ACharSize.XScaled div ATEditorCharXScale;

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
var
  NBitmapSize: integer;
begin
  NBitmapSize:= ATEditorBitmaps.BitmapNiceScroll.Width;
  if MouseNiceScroll then
    C.Draw(
      FMouseNiceScrollPos.X - NBitmapSize div 2,
      FMouseNiceScrollPos.Y - NBitmapSize div 2,
      ATEditorBitmaps.BitmapNiceScroll);
end;

procedure TATSynEdit.DoPaintGutterNumber(C: TCanvas; ALineIndex, ACoordTop: integer; ABand: TATGutterItem);
  //
  //painting of text mark '-' or '.' is slower, so paint mark by FillRect()
  procedure PaintDash(W, H: integer);
  var
    P: TPoint;
  begin
    P.Y:= ACoordTop + FCharSize.Y div 2 - ATEditorScale(1);

    case FOptNumbersAlignment of
      taLeftJustify:
        P.X:= ABand.Left + FNumbersIndent;
      taRightJustify:
        P.X:= ABand.Right - FNumbersIndent - FCharSize.XScaled div ATEditorCharXScale div 2;
      taCenter:
        P.X:= (ABand.Left+ABand.Right) div 2;
    end;

    C.Brush.Color:= C.Font.Color;
    C.Brush.Style:= bsSolid;
    C.FillRect(
      P.X - W div 2,
      P.Y,
      P.X - W div 2 + W,
      P.Y + H
      );
  end;
  //
var
  SText: string;
  P: TPoint;
  NNumberWidth, NLineWidthPx: integer;
begin
  SText:= DoFormatLineNumber(ALineIndex+1);

  case SText of
    '':
      exit;

    '.':
      begin
        NLineWidthPx:= DoScaleFont(2);
        PaintDash(
          NLineWidthPx,
          NLineWidthPx
          );
      end;

    '-':
      begin
        NLineWidthPx:= DoScaleFont(2);
        PaintDash(
          FCharSize.XScaled div ATEditorCharXScale,
          NLineWidthPx
          );
      end;

    else
      begin
        NNumberWidth:= FCharSize.XScaled * Length(SText) div ATEditorCharXScale;

        P.Y:= ACoordTop;

        case FOptNumbersAlignment of
          taLeftJustify:
            P.X:= ABand.Left + FNumbersIndent;
          taRightJustify:
            P.X:= ABand.Right - NNumberWidth - FNumbersIndent;
          taCenter:
            P.X:= (ABand.Left + ABand.Right - NNumberWidth) div 2;
        end;

        C.Brush.Style:= bsClear;
        CanvasTextOutSimplest(C,
          P.X,
          P.Y+FSpacingTopEdge+FSpacingTop,
          SText
          );
      end;
  end;
end;


function TATSynEdit.DoEventCommand(ACommand: integer;
  AInvoke: TATCommandInvoke; const AText: string): boolean;
begin
  Result:= false;
  if Assigned(FOnCommand) then
    FOnCommand(Self, ACommand, AInvoke, AText, Result);
end;

procedure TATSynEdit.DoEventCommandAfter(ACommand: integer; const AText: string);
begin
  if Assigned(FOnCommandAfter) then
    FOnCommandAfter(Self, ACommand, AText);
end;


procedure TATSynEdit.DoEventCarets;
var
  SClip: string;
  Caret: TATCaretItem;
begin
  if Carets.Count=1 then
  begin
    Caret:= Carets[0];

    //OptAutoPair_DisableCharDoubling is to support feature of VSCode:
    //after typing paired brackets e.g. '()', app disables typing ')' over ')';
    //but if caret left orig line -> app don't disable typing ')' over ')'
    if Caret.PosY<>FPrevCaret.PosY then
      FOptAutoPair_DisableCharDoubling:= false;

    if (FPrevCaret.PosX=Caret.PosX) and
      (FPrevCaret.PosY=Caret.PosY) and
      (FPrevCaret.EndX=Caret.EndX) and
      (FPrevCaret.EndY=Caret.EndY) then exit;
    FPrevCaret.PosX:= Caret.PosX;
    FPrevCaret.PosY:= Caret.PosY;
    FPrevCaret.EndX:= Caret.EndX;
    FPrevCaret.EndY:= Caret.EndY;
  end
  else
  begin
    FPrevCaret.PosX:= -1;
    FPrevCaret.PosY:= -1;
    FPrevCaret.EndX:= -1;
    FPrevCaret.EndY:= -1;
  end;

  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorCaretMove(Self);

  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self);

  {
  //event is not needed
  if Assigned(FOnChangeCaretLine) then
    if Carets.Count=1 then
    begin
      NCaretY:= Carets[0].PosY;
      if NCaretY<>FLastCaretY then
      begin
        FLastCaretY:= NCaretY;
        FOnChangeCaretLine(Self);
      end;
    end;
  }
  if ATEditorOptions.AutoCopyToClipboard or
    ATEditorOptions.AutoCopyToPrimarySel then
    if (Carets.Count=1) and Carets.IsSelection then
    begin
      SClip:= GetTextForClipboard;
      if Length(SClip)<=ATEditorOptions.AutoCopyMaxTextSize then
      begin
        if ATEditorOptions.AutoCopyToClipboard then
          SClipboardCopy(SClip);
        if ATEditorOptions.AutoCopyToPrimarySel then
          SClipboardCopy(SClip, PrimarySelection);
      end;
    end;
end;

procedure TATSynEdit.DoEventScroll;
begin
  //horizontal scroll must clear CaretItem.SavedX values
  if not FCaretVirtual then
    if TATEditorInternalFlag.ScrolledHorz in FPaintFlags then
    begin
      Exclude(FPaintFlags, TATEditorInternalFlag.ScrolledHorz);
      Carets.UpdateMemory(TATCaretMemoryAction.ClearX, false);
    end;

  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorScroll(Self);

  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATSynEdit.DoEventChangeModified;
begin
  FPrevModified:= Modified;
  if Assigned(FOnChangeModified) then
    FOnChangeModified(Self);
end;

procedure TATSynEdit.DoEventChange(ALineIndex: integer; AllowOnChange: boolean);
var
  HandlerChangeLog: TATStringsChangeLogEvent;
begin
  FLinkCache.Clear;

  if Assigned(FAdapterHilite) then
  begin
    FAdapterHilite.OnEditorChange(Self);

    if ALineIndex>=0 then
    begin
      HandlerChangeLog:= Strings.OnChangeLog;
      if Assigned(HandlerChangeLog) then
        HandlerChangeLog(nil, ALineIndex);
    end;
  end;

  if AllowOnChange then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);

    if FPrevModified<>Modified then
      DoEventChangeModified;
  end;

  //fire OnIdle after pause after change
  if FOptIdleInterval>0 then
  begin
    if FTimerIdle=nil then
    begin
      FTimerIdle:= TTimer.Create(Self);
      FTimerIdle.Enabled:= false;
      FTimerIdle.OnTimer:= @TimerIdleTick;
    end;
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

procedure TATSynEdit.DoEventZoom;
begin
  if Assigned(FOnChangeZoom) then
    FOnChangeZoom(Self);
end;

procedure TATSynEdit.DoEventClickGutter(ABandIndex, ALineNumber: integer; var AHandled: boolean);
begin
  if Assigned(FOnClickGutter) then
    FOnClickGutter(Self, ABandIndex, ALineNumber, AHandled);
end;

procedure TATSynEdit.DoEventClickMicromap(AX, AY: integer);
begin
  if Assigned(FOnClickMicromap) then
    FOnClickMicromap(Self, AX, AY);
end;

procedure TATSynEdit.DoEventDrawBookmarkIcon(C: TCanvas; ALineIndex, ABookmarkIndex: integer;
  const ARect: TRect; var AHandled: boolean);
begin
  if Assigned(FOnDrawBookmarkIcon) then
    FOnDrawBookmarkIcon(Self, C, ALineIndex, ABookmarkIndex, ARect, AHandled);
end;

procedure TATSynEdit.DoEventBeforeCalcHilite(AMainText: boolean);
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorBeforeCalcHilite(Self, AMainText);

  if Assigned(FOnBeforeCalcHilite) then
    FOnBeforeCalcHilite(Self, AMainText);
end;

procedure TATSynEdit.DoEventAfterCalcHilite(AMainText: boolean);
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorAfterCalcHilite(Self, AMainText);

  if Assigned(FOnAfterCalcHilite) then
    FOnAfterCalcHilite(Self, AMainText);
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
  procedure _Delta(var AInfo: TATEditorScrollInfo; ADelta: integer);
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
  procedure _Delta(var AInfo: TATEditorScrollInfo; ADelta: integer);
  begin
    if ADelta=0 then exit;
    UpdateScrollInfoFromSmoothPos(AInfo,
      Min(AInfo.SmoothPosLast, AInfo.SmoothPos+ADelta));
  end;
//
begin
  _Delta(FScrollHorz, ADeltaX);
  _Delta(FScrollVert, ADeltaY);
end;

procedure TATSynEdit.MenuStdClick(Sender: TObject);
var
  Cmd: integer;
begin
  Cmd:= (Sender as TMenuItem).Tag;
  if Cmd>0 then
  begin
    DoCommand(Cmd, TATCommandInvoke.MenuContext);
    Invalidate;
  end;
end;

procedure TATSynEdit.MenuStdPopup(Sender: TObject);
var
  i: integer;
begin
  MenuitemTextCut.Caption:= ATEditorOptions.TextMenuitemCut;
  MenuitemTextCopy.Caption:= ATEditorOptions.TextMenuitemCopy;
  MenuitemTextPaste.Caption:= ATEditorOptions.TextMenuitemPaste;
  MenuitemTextDelete.Caption:= ATEditorOptions.TextMenuitemDelete;
  MenuitemTextSelAll.Caption:= ATEditorOptions.TextMenuitemSelectAll;
  MenuitemTextUndo.Caption:= ATEditorOptions.TextMenuitemUndo;
  MenuitemTextRedo.Caption:= ATEditorOptions.TextMenuitemRedo;

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
    MI.OnClick:= @MenuStdClick;
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

procedure TATSynEdit.InitTimerScroll;
begin
  if FTimerScroll=nil then
  begin
    FTimerScroll:= TTimer.Create(Self);
    FTimerScroll.Enabled:= false;
    FTimerScroll.Interval:= ATEditorOptions.TimerIntervalAutoScroll;
    FTimerScroll.OnTimer:= @TimerScrollTick;
  end;
end;

procedure TATSynEdit.InitTimerNiceScroll;
begin
  if FTimerNiceScroll=nil then
  begin
    FTimerNiceScroll:= TTimer.Create(Self);
    FTimerNiceScroll.Enabled:= false;
    FTimerNiceScroll.Interval:= ATEditorOptions.TimerIntervalNiceScroll;
    FTimerNiceScroll.OnTimer:= @TimerNiceScrollTick;
  end;
end;

//drop selection of 1st caret into mouse-pos
procedure TATSynEdit.DoDropText(AndDeleteSelection: boolean);
var
  St: TATStrings;
  Str: atString;
  P, PosAfter, Shift: TPoint;
  PntCoord: TATPoint;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
  Relation: TATPosRelation;
  Details: TATEditorPosDetails;
begin
  if ModeReadOnly then exit;
  St:= Strings;
  if Carets.Count<>1 then Exit; //allow only 1 caret
  Carets[0].GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then Exit;

  DoSelect_None;

  //calc insert-pos
  P:= ScreenToClient(Mouse.CursorPos);
  PntCoord.X:= P.X;
  PntCoord.Y:= P.Y;
  P:= ClientPosToCaretPos(PntCoord, Details);
  if P.Y<0 then exit;

  //can't drop into selection
  Relation:= IsPosInRange(P.X, P.Y, X1, Y1, X2, Y2);
  if Relation=TATPosRelation.Inside then exit;

  Str:= St.TextSubstring(X1, Y1, X2, Y2);
  if Str='' then exit;
  BeginEditing;

  //insert before selection?
  if Relation=TATPosRelation.Before then
  begin
    if AndDeleteSelection then
      St.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
    St.TextInsert(P.X, P.Y, Str, false, Shift, PosAfter);

    //select moved text
    DoCaretSingle(PosAfter.X, PosAfter.Y, P.X, P.Y);
  end
  else
  begin
    St.TextInsert(P.X, P.Y, Str, false, Shift, PosAfter);

    //select moved text
    DoCaretSingle(PosAfter.X, PosAfter.Y, P.X, P.Y);

    if AndDeleteSelection then
    begin
      St.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
      UpdateCaretsAndMarkersOnEditing(0,
        Point(X1, Y1),
        Point(X2, Y2),
        Shift,
        PosAfter);
    end;
  end;

  DoEventCarets;

  EndEditing(true);
  {
  NChangedLine:= St.EditingTopLine; //Min(Y1, P.Y)
  DoEventChange(NChangedLine);
    //with DoEventChange(ALineIndex=-1), we have broken syntax highlight,
    //after drag-drop from huge line, to the lower position of the same huge line,
    //e.g. in 100K HTML file with huge line
  }

  Update(true);
end;

function TATSynEdit.GetIndentString: UnicodeString;
begin
  if FOptTabSpaces then
    Result:= StringOfCharW(' ', FOptTabSize)
  else
    Result:= #9;
end;

function TATSynEdit.GetAutoIndentString(APosX, APosY: integer;
  AUseIndentRegexRule, AForceIndent: boolean): atString;
var
  StrPrev, StrIndent: atString;
  NChars, NSpaces: integer;
  MatchPos, MatchLen: SizeInt;
  bAddIndent: boolean;
begin
  Result:= '';
  if not FOptAutoIndent then Exit;
  if not Strings.IsIndexValid(APosY) then Exit;

  StrPrev:= Strings.LineSub(APosY, 1, APosX);
  if StrPrev='' then exit;
  NChars:= SGetIndentChars(StrPrev); //count of chars in indent

  if AForceIndent then
    bAddIndent:= true
  else
    bAddIndent:=
    (Length(StrPrev)<ATEditorOptions.MaxLineLenToCalculateAutoIndent) and //avoid running regex on huge line, it gives strange AV in atsynedit_regexpr.pas:MatchPrim
    AUseIndentRegexRule and
    (FOptAutoIndentRegexRule<>'') and
    SFindRegexMatch(StrPrev, FOptAutoIndentRegexRule{%H-}, MatchPos, MatchLen);

  StrIndent:= Copy(StrPrev, 1, NChars);
  NSpaces:= Length(FTabHelper.TabsToSpaces(APosY, StrIndent));

  case FOptAutoIndentKind of
    TATEditorAutoIndentKind.AsPrevLine:
      Result:= StrIndent;
    TATEditorAutoIndentKind.SpacesOnly:
      Result:= StringOfCharW(' ', NSpaces);
    TATEditorAutoIndentKind.TabsOnly:
      Result:= StringOfCharW(#9, NSpaces div FOptTabSize);
    TATEditorAutoIndentKind.TabsAndSpaces:
      Result:= StringOfCharW(#9, NSpaces div FOptTabSize) + StringOfCharW(' ', NSpaces mod FOptTabSize);
    TATEditorAutoIndentKind.ToOpeningBracket:
      begin
        //indent like in prev line + spaces up to opening bracket
        NSpaces:= SGetIndentCharsToOpeningBracket(StrPrev);
        Result:= StrIndent + StringOfCharW(' ', NSpaces-Length(StrIndent));
      end;
  end;

  if bAddIndent then
    Result:= Result+GetIndentString;
end;

function TATSynEdit.GetModified: boolean;
begin
  Result:= Strings.Modified;
end;

function TATSynEdit.GetModifiedBookmarks: boolean;
begin
  Result:= Strings.Bookmarks.Modified;
end;

procedure TATSynEdit.SetModifiedBookmarks(AValue: boolean);
begin
  Strings.Bookmarks.Modified:= AValue;
end;

procedure TATSynEdit.SetModified(AValue: boolean);
var
  bChange: boolean;
begin
  bChange:= (Strings.Modified<>AValue) or (FPrevModified<>AValue);
  Strings.Modified:= AValue;
  FPrevModified:= AValue;
  if bChange and not FIsIniting then
    DoEventChangeModified;
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
  PntCoord: TATPoint;
begin
  if Carets.Count=0 then
    begin Result:= 0; Exit end;
  with Carets[0] do
    P:= Point(PosX, PosY);
  PntCoord:= CaretPosToClientPos(P);
  Result:= (PntCoord.Y-FRectMain.Top) div FCharSize.Y;
end;

function TATSynEdit.GetText: UnicodeString;
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
  FActivationTime:= GetTickCount64;
  if FCaretHideUnfocused then
    FCaretShowEnabled:= true;
  if IsRepaintNeededOnEnterOrExit then
    Invalidate;
  TimersStart;
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeEnter(Self);
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
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeExit(Self);
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

  if Assigned(FTimerBlink) then
    FTimerBlink.Enabled:= false;

  if Assigned(FTimerIdle) then
    FTimerIdle.Enabled:= false;

  if Assigned(FTimerScroll) then
    FTimerScroll.Enabled:= false;

  if Assigned(FTimerNiceScroll) then
    FTimerNiceScroll.Enabled:= false;

  if Assigned(FTimerDelayedParsing) then
    FTimerDelayedParsing.Enabled:= false;

  if Assigned(FTimerFlicker) then
    FTimerFlicker.Enabled:= false;
end;

procedure TATSynEdit.DoMinimapClick(APosY: integer);
var
  NItem: integer;
begin
  NItem:= GetMinimap_ClickedPosToWrapIndex(APosY);
  if NItem>=0 then
  begin
    NItem:= Max(0, NItem - GetVisibleLines div 2);
    DoScroll_SetPos(FScrollVert, Min(NItem, FScrollVert.NMax));
    Update;
  end;
end;

procedure TATSynEdit.DoMinimapDrag(APosY: integer);
var
  NPos: integer;
begin
  NPos:= GetMinimap_DraggedPosToWrapIndex(APosY);
  DoScroll_SetPos(FScrollVert, NPos);
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

procedure TATSynEdit.DoScaleFontDelta(AInc: boolean; AllowUpdate: boolean);
const
  cMinScale = 60;
  cStep = 10;
var
  NScale: integer;
begin
  NScale:= OptScaleFont;
  if NScale=0 then
  begin
    NScale:= ATEditorScaleFontPercents;
    if NScale=0 then
      NScale:= ATEditorScalePercents;
  end;

  if not AInc then
    if NScale<=cMinScale then Exit;

  //use OptScaleFont setter, not FOptScaleFont
  OptScaleFont:= NScale+cStep*BoolToPlusMinusOne[AInc];

  if AllowUpdate then
    Update;
end;

procedure TATSynEdit.BeginUpdate;
begin
  Inc(FPaintLocked);
  Invalidate;
end;

procedure TATSynEdit.EndUpdate;
begin
  if FPaintLocked<=0 then
    FPaintLocked:= 0
  else
  begin
    Dec(FPaintLocked);
    if FPaintLocked=0 then
      Invalidate;
  end;
end;

function TATSynEdit.IsLocked: boolean;
begin
  Result:= FPaintLocked>0;
end;

function TATSynEdit.IsIndentBasedFolding: boolean;
begin
  Result:= Assigned(AdapterForHilite) and AdapterForHilite.IsIndentBasedFolding;
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
  Result:= Assigned(FTimerNiceScroll) and FTimerNiceScroll.Enabled;
end;

procedure TATSynEdit.SetEnabledSlowEvents(AValue: boolean);
var
  St: TATStrings;
begin
  St:= Strings;
  if not AValue then
  begin
    St.ClearUndo(true);
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
    St.ClearUndo(false);
    St.EnabledChangeEvents:= true;
    if St.IsIndexValid(FLastLineOfSlowEvents) then
    begin
      St.DoEventLog(FLastLineOfSlowEvents);
      St.DoEventChange(TATLineChangeKind.Edited, FLastLineOfSlowEvents, 1);
      FLastLineOfSlowEvents:= -1;
    end;
  end;
end;

procedure TATSynEdit.SetMouseNiceScroll(AValue: boolean);
begin
  if AValue then
    InitTimerNiceScroll;
  if Assigned(FTimerNiceScroll) then
    FTimerNiceScroll.Enabled:= AValue;
  if not AValue then
    UpdateCursor;
  Invalidate;
end;

function TATSynEdit.GetEndOfFilePos: TPoint;
var
  St: TATStrings;
begin
  St:= Strings;
  if St.Count>0 then
  begin
    Result.Y:= St.Count-1;
    Result.X:= St.LinesLen[Result.Y];
    if St.LinesEnds[Result.Y]<>TATLineEnds.None then
      Inc(Result.X);
  end
  else
  begin
    Result.X:= 0;
    Result.Y:= 0;
  end;
end;


function TATSynEdit.DoCalcFoldDeepestRangeContainingCaret: integer;
var
  Caret: TATCaretItem;
begin
  Result:= -1;
  if FOptGutterShowFoldLinesForCaret then
    if Carets.Count>0 then
    begin
      Caret:= Carets[0];
      if Strings.IsIndexValid(Caret.PosY) then
        Result:= FFold.FindDeepestRangeContainingLine(Caret.PosY, false, FFoldIconForMinimalRange);
    end;
end;

function TATSynEdit.DoCalcFoldProps(AWrapItemIndex, AFoldRangeWithCaret: integer; out AProps: TATFoldBarProps): boolean;
var
  WrapItem: TATWrapItem;
  Rng: PATFoldRange;
  NLineIndex: integer;
  NIndexOfCurrentRng, NIndexOfLineRng: integer;
begin
  Result:= false;
  AProps:= Default(TATFoldBarProps);

  WrapItem:= FWrapInfo[AWrapItemIndex];
  NLineIndex:= WrapItem.NLineIndex;

  NIndexOfCurrentRng:= FFold.FindDeepestRangeContainingLine(NLineIndex, false, FFoldIconForMinimalRange);
  NIndexOfLineRng:= FFold.FindRangeWithPlusAtLine_ViaIndexer(NLineIndex);

  if NIndexOfCurrentRng<0 then exit;
  AProps.HiliteLines:= NIndexOfCurrentRng=AFoldRangeWithCaret;

  Rng:= Fold.ItemPtr(NIndexOfCurrentRng);

  if Rng^.Y<NLineIndex then
    AProps.IsLineUp:= true;

  if Rng^.Y2>NLineIndex then
    AProps.IsLineDown:= true;

  if Rng^.Y=NLineIndex then
  begin
    AProps.State:= TATFoldBarState.SBegin;
    {
    //don't override found [+], 2 blocks can start at same pos
    if not AProps.IsPlus then
      AProps.IsPlus:= Rng^.Folded;
    }
    if NIndexOfLineRng>=0 then
      AProps.IsPlus:= FFold.ItemPtr(NIndexOfLineRng)^.Folded
    else
      AProps.IsPlus:= Rng^.Folded;
  end;

  if Rng^.Y2=NLineIndex then
    if AProps.State<>TATFoldBarState.SBegin then
      AProps.State:= TATFoldBarState.SEnd;

  //correct state for wrapped line
  if AProps.State=TATFoldBarState.SBegin then
    if not WrapItem.bInitial then
      AProps.State:= TATFoldBarState.SMiddle;

  //correct state for wrapped line
  if AProps.State=TATFoldBarState.SEnd then
    if WrapItem.NFinal=TATWrapItemFinal.Middle then
      AProps.State:= TATFoldBarState.SMiddle;

  Result:= true;
end;

procedure TATSynEdit.DoPaintGutterFolding(C: TCanvas;
  AWrapItemIndex, AFoldRangeWithCaret: integer;
  ACoord1, ACoord2: TPoint);
var
  CoordXCenter, CoordYCenter: integer;
  Props: TATFoldBarProps;
  //
  procedure DrawUp; inline;
  begin
    if Props.IsLineUp then
      CanvasLineVert(C,
        CoordXCenter,
        ACoord1.Y,
        CoordYCenter
        );
  end;
  procedure DrawDown; inline;
  begin
    if Props.IsLineDown then
      CanvasLineVert(C,
        CoordXCenter,
        CoordYCenter,
        ACoord2.Y+1
        );
  end;
  //
var
  NColorLine, NColorPlus: TColor;
  bOk: boolean;
  OldPenWidth: integer;
begin
  if not FOptGutterShowFoldAlways then
    if not FCursorOnGutter then exit;

  //fixing CudaText issue #4285, needed only for macOS
  C.AntialiasingMode:= amOff;

  bOk:= DoCalcFoldProps(AWrapItemIndex, AFoldRangeWithCaret, Props);
  if not bOk then exit;

  if Props.HiliteLines then
    NColorPlus:= Colors.GutterFoldLine2
  else
    NColorPlus:= Colors.GutterFoldLine;

  if FOptGutterShowFoldLines then
    NColorLine:= NColorPlus
  else
    NColorLine:= FColorGutterFoldBG;
  C.Pen.Color:= NColorLine;

  OldPenWidth:= C.Pen.Width;
  C.Pen.Width:= DoScaleFont(1);
  C.Pen.EndCap:= pecFlat;

  CoordXCenter:= (ACoord1.X+ACoord2.X) div 2;
  CoordYCenter:= (ACoord1.Y+ACoord2.Y) div 2;

  case Props.State of
    TATFoldBarState.SBegin:
      begin
        if FOptGutterShowFoldLinesAll then
          DrawUp;

        if FOptGutterShowFoldLinesAll or not Props.IsPlus then
          DrawDown;

        DoPaintGutterPlusMinus(C,
          CoordXCenter, CoordYCenter, Props.IsPlus, NColorPlus);
      end;

    TATFoldBarState.SEnd:
      begin
        if FOptGutterShowFoldLinesAll then
        begin
          DrawUp;
          DrawDown;
        end;

        Dec(ACoord2.Y, ATEditorOptions.SizeGutterFoldLineDx);
        CanvasLineVert(C,
          CoordXCenter,
          ACoord1.Y,
          ACoord2.Y
          );
        CanvasLineHorz(C,
          CoordXCenter,
          ACoord2.Y,
          CoordXCenter + DoScaleFont(FOptGutterPlusSize)
          );
      end;

    TATFoldBarState.SMiddle:
      begin
        CanvasLineVert(C,
          CoordXCenter,
          ACoord1.Y,
          ACoord2.Y
          );
      end;

    else
      begin
        DrawUp;
        DrawDown;
      end;
  end;

  C.Pen.Width:= OldPenWidth;
end;

procedure TATSynEdit.DoPaintGutterDecor(C: TCanvas; ALine: integer; const ARect: TRect;
  out AIconPainted: boolean);
  //
  procedure PaintDecorItem(var Decor: TATGutterDecorItem);
  var
    Style, StylePrev: TFontStyles;
    Ext: TSize;
    NLeftPos, NImageIndex, NImageCount: integer;
    bPaintIcon: boolean;
    DecorText: string;
  begin
    NImageIndex:= Decor.Data.ImageIndex;
    NImageCount:= 0;
    if Assigned(FGutterDecorImages) then
      NImageCount:= FGutterDecorImages.Count;
    bPaintIcon:= (NImageIndex>=0) and (NImageIndex<NImageCount);

    //paint decor text
    DecorText:= Decor.Data.TextCaption;
    if DecorText<>'' then
    begin
      C.Font.Color:= Decor.Data.TextColor;
      Style:= [];
      if Decor.Data.TextBold then
        Include(Style, fsBold);
      if Decor.Data.TextItalic then
        Include(Style, fsItalic);
      StylePrev:= C.Font.Style;
      C.Font.Style:= Style;

      Ext:= C.TextExtent(DecorText);
      C.Brush.Color:= FColorGutterBG;

      case FGutterDecorAlignment of
        taCenter:
          NLeftPos:= (ARect.Left+ARect.Right-Ext.cx) div 2;
        taLeftJustify:
          NLeftPos:= ARect.Left;
        taRightJustify:
          NLeftPos:= ARect.Right-Ext.cx;
      end;

      C.Brush.Style:= bsClear;
      C.TextOut(
        NLeftPos,
        (ARect.Top+ARect.Bottom-Ext.cy) div 2,
        DecorText
        );
      C.Font.Style:= StylePrev;
      C.Brush.Style:= bsSolid;
    end
    else
    //paint decor icon
    if bPaintIcon then
    begin
      case FGutterDecorAlignment of
        taCenter:
          NLeftPos:= (ARect.Left+ARect.Right-FGutterDecorImages.Width) div 2;
        taLeftJustify:
          NLeftPos:= ARect.Left;
        taRightJustify:
          NLeftPos:= ARect.Right-FGutterDecorImages.Width;
      end;

      FGutterDecorImages.Draw(C,
          NLeftPos,
          (ARect.Top+ARect.Bottom-FGutterDecorImages.Height) div 2,
          NImageIndex
          );
    end
    else
    //fill cell background
    if Decor.Data.TextColor<>clNone then
    begin
      C.Brush.Style:= bsSolid;
      C.Brush.Color:= Decor.Data.TextColor;
      C.FillRect(ARect);
    end;
  end;
  //
var
  Decor: PATGutterDecorItem;
  NItem: integer;
begin
  AIconPainted:= false;
  if FGutterDecor=nil then exit;
  NItem:= FGutterDecor.Find(ALine);
  if NItem<0 then exit;

  //paint first found item
  Decor:= FGutterDecor.ItemPtr(NItem);
  PaintDecorItem(Decor^);
  AIconPainted:= not Decor^.IsBackgroundFill;

  //paint next item, if first one is background-filler
  if Decor^.IsBackgroundFill and FGutterDecor.IsIndexValid(NItem+1) then
  begin
    Decor:= FGutterDecor.ItemPtr(NItem+1);
    if Decor^.Data.LineNum=ALine then
    begin
      PaintDecorItem(Decor^);
      AIconPainted:= true;
    end;
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

  C.Brush.Style:= bsClear;
  C.TextOut(Pos.X, Pos.Y, FTextHint);
end;


procedure TATSynEdit.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  case Message.CharCode of
    VK_RETURN:
      Message.Result:= Ord(WantReturns);
    VK_TAB:
      Message.Result:= Ord(WantTabs);
    VK_LEFT,
    VK_RIGHT,
    VK_UP,
    VK_DOWN:
      Message.Result:= 1;
    else
      inherited;
  end;
end;

procedure TATSynEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if Assigned(FAdapterIME) then
    FAdapterIME.ImeKillFocus(Self);
end;

{$ifdef LCLGTK2}
//{$ifdef GTK2_IME_CODE}
procedure TATSynEdit.WM_GTK_IM_COMPOSITION(var Msg: TLMessage);
begin
  if Assigned(FAdapterIME) then
    FAdapterIME.GTK2IMComposition(Self, Msg);
end;
//{$endif}
{$endif}

procedure TATSynEdit.DoPaintStaple(C: TCanvas; const R: TRect; AColor: TColor);
begin
  if FOptStapleEdge1=TATEditorStapleEdge.Angle then
    CanvasLineEx(C, AColor, FOptStapleStyle, R.Left, R.Top, R.Right, R.Top, false);

  CanvasLineEx(C, AColor, FOptStapleStyle, R.Left, R.Top, R.Left, R.Bottom, false);

  if FOptStapleEdge2=TATEditorStapleEdge.Angle then
    CanvasLineEx(C, AColor, FOptStapleStyle, R.Left, R.Bottom, R.Right, R.Bottom, true);
end;

procedure TATSynEdit.DoPaintStaples(C: TCanvas;
  const ARect: TRect;
  const ACharSize: TATEditorCharSize;
  const AScrollHorz: TATEditorScrollInfo);
var
  St: TATStrings;
  nLineFrom, nLineTo, nRangeDeepest, nMaxHeight: integer;
  nIndent, nIndentBegin, nIndentEnd: integer;
  nSpaceShift: integer;
  Indexes: TATIntArray;
  Range: PATFoldRange;
  P1, P2: TATPoint;
  RectStaple: TRect;
  NColor, NColorNormal, NColorActive: TColor;
  bIndentBasedFolding: boolean;
  i: integer;
begin
  if FOptStapleStyle=TATLineStyle.None then Exit;
  if not FFold.HasStaples then Exit;

  St:= Strings;
  nLineFrom:= LineTop;
  nLineTo:= LineBottom;
  nMaxHeight:= FRectMain.Bottom+2; //not FRectMain.Height, bad with ruler on
  nRangeDeepest:= -1;

  Indexes:= FFold.FindRangesWithStaples(nLineFrom, nLineTo);

  //currently find active range for first caret only
  if FOptStapleHiliteActive then
    if Carets.Count>0 then
      nRangeDeepest:= FFold.FindDeepestRangeContainingLine(Carets[0].PosY, true, FFoldIconForMinimalRange);

  NColorNormal:= Colors.BlockStaple;
  NColorActive:= Colors.BlockStapleForCaret;
  if NColorActive=clNone then
    NColorActive:= ColorBlend(NColorNormal, FColorFont, FOptStapleHiliteActiveAlpha);

  bIndentBasedFolding:= IsIndentBasedFolding;

  for i:= 0 to High(Indexes) do
  begin
    Range:= Fold.ItemPtr(Indexes[i]);
    {
    //FindRangesWithStaples does it:
    if not Range^.Staple then Continue;
    if Range^.Folded then Continue;
    }

    if not St.IsIndexValid(Range^.Y) then Continue;
    if not St.IsIndexValid(Range^.Y2) then Continue;

    if IsLineFolded(Range^.Y, true) then Continue;
    if IsLineFolded(Range^.Y2, true) then Continue;

    P1:= CaretPosToClientPos(Point(0, Range^.Y));
    P2:= CaretPosToClientPos(Point(0, Range^.Y2));
    if (P1.Y<FRectMain.Top) and (Range^.Y>=nLineFrom) then Continue;
    if (P2.Y<FRectMain.Top) and (Range^.Y2>=nLineFrom) then Continue;

    nIndentBegin:= FTabHelper.GetIndentExpanded(Range^.Y, St.Lines[Range^.Y]);

    if FOptStapleIndentConsidersEnd then
    begin
      nIndentEnd:= FTabHelper.GetIndentExpanded(Range^.Y2, St.Lines[Range^.Y2]);
      nIndent:= Min(nIndentBegin, nIndentEnd);
    end
    else
      nIndent:= nIndentBegin;

    nSpaceShift:= Int64(nIndent)*ACharSize.XSpacePercents*ACharSize.XScaled div ATEditorCharXScale div 100;
    Inc(P1.X, nSpaceShift);
    Inc(P2.X, nSpaceShift);

    RectStaple.Left:= P1.X + FOptStapleIndent;
    RectStaple.Top:= P1.Y;

    if (RectStaple.Left>=ARect.Left) and
      (RectStaple.Left<ARect.Right) then
    begin
      RectStaple.Right:= RectStaple.Left+ (ACharSize.XScaled * FOptStapleWidthPercent div ATEditorCharXScale div 100);
      RectStaple.Bottom:= P2.Y + ACharSize.Y-1;

      if FOptStapleEdge1=TATEditorStapleEdge.None then
        Inc(RectStaple.Top, FCharSize.Y);

      if FOptStapleEdge2=TATEditorStapleEdge.None then
        if not bIndentBasedFolding then
          Dec(RectStaple.Bottom, FCharSize.Y);

      //don't use too big coords, some OS'es truncate lines painted with big coords
      RectStaple.Top:= Max(RectStaple.Top, -2);
      RectStaple.Bottom:= Min(RectStaple.Bottom, nMaxHeight);

      if Indexes[i]=nRangeDeepest then
        NColor:= NColorActive
      else
        NColor:= NColorNormal;

      if Assigned(FOnCalcStaple) then
        FOnCalcStaple(Self, Range^.Y, NIndent, NColor);

      DoPaintStaple(C, RectStaple, NColor);
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
  PntCoord: TATPoint;
  PntShort: TPoint;
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

    PntCoord.X:= Mark.CoordX;
    PntCoord.Y:= Mark.CoordY+FCharSize.Y;

    if ATPointInRect(FRectMain, PntCoord) then
    begin
      PntShort.X:= PntCoord.X;
      PntShort.Y:= PntCoord.Y;
      CanvasPaintTriangleUp(C, Colors.Markers, PntShort, NMarkSize);

      if (Mark.LineLen<>0) and (Mark.CoordY=Mark.CoordY2) then
      begin
        R.Left:= Min(PntShort.X, Mark.CoordX2);
        R.Right:= Max(PntShort.X, Mark.CoordX2)+1;
        R.Bottom:= PntShort.Y+NMarkSize+1;
        R.Top:= R.Bottom-NLineW;

        //avoid painting part of the line over minimap/gutter
        R.Left:= Max(R.Left, FRectMain.Left);
        R.Right:= Min(R.Right, FRectMain.Right);

        C.Brush.Color:= Colors.Markers;
        C.FillRect(R);
      end;
    end;
  end;
end;

procedure TATSynEdit.DoPaintGutterPlusMinus(C: TCanvas; AX, AY: integer;
  APlus: boolean; ALineColor: TColor);
begin
  Inc(AY, FSpacingTopEdge);

  case OptGutterIcons of
    TATEditorGutterIcons.PlusMinus:
      begin
        CanvasPaintPlusMinus(C,
          ALineColor,
          FColorGutterFoldBG,
          Point(AX, AY),
          DoScaleFont(FOptGutterPlusSize),
          DoScaleFont(1),
          APlus);
      end;
    TATEditorGutterIcons.Triangles:
      begin
        if APlus then
          CanvasPaintTriangleRight(C,
            ALineColor,
            Point(AX, AY),
            DoScaleFont(FOptGutterPlusSize div 2))
        else
          CanvasPaintTriangleDown(C,
            ALineColor,
            Point(AX, AY),
            DoScaleFont(FOptGutterPlusSize div 2))
      end;
  end;
end;


procedure TATSynEdit.DoSetMarkedLines(ALine1, ALine2: integer);
begin
  InitMarkedRange;
  FMarkedRange.Clear;
  if (ALine1>=0) and (ALine2>=ALine1) then
  begin
    FMarkedRange.Add(
      Point(0, ALine1),
      Point(0, 0),
      TATMarkerTags.Init(0, 0)
      );
    FMarkedRange.Add(
      Point(0, ALine2),
      Point(0, 0),
      TATMarkerTags.Init(0, 0)
      );
  end;
end;

function TATSynEdit.DoGetMarkedLines(out ALine1, ALine2: integer): boolean;
begin
  Result:= Assigned(FMarkedRange) and (FMarkedRange.Count=2);
  if Result then
  begin
    ALine1:= FMarkedRange.Items[0].PosY;
    ALine2:= FMarkedRange.Items[1].PosY;
  end
  else
  begin
    ALine1:= -1;
    ALine2:= -1;
  end;
end;


procedure TATSynEdit.UpdateLinksAttribs(ALineFrom: integer);
{
Method takes 0-1 ms, on maximized window, on notebook HP Pavilion g6,
so no need to optimize it more
}
var
  St: TATStrings;
  NLineStart, NLineEnd, NLineLen: integer;
  MatchPos, MatchLen, iLine: integer;
  LinePart: TATLinePart;
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
      if FAttribs.DeleteWithTag(ATEditorOptions.UrlMarkerTag) then
        Update;
    exit;
  end;

  if FRegexLinks=nil then exit;
  {$ifndef USE_FPC_REGEXPR}
  if not FRegexLinks.IsCompiled then exit;
  {$endif}

  St:= Strings;
  if ALineFrom>=0 then
    NLineStart:= ALineFrom
  else
    NLineStart:= LineTop;
  NLineEnd:= NLineStart+GetVisibleLines;

  InitAttribs;
  FAttribs.DeleteWithTag(ATEditorOptions.UrlMarkerTag);
  InitLinePart(LinePart);

  FLinkCache.DeleteDataOutOfRange(NLineStart, NLineEnd);
  NRegexRuns:= 0;

  for iLine:= NLineStart to NLineEnd do
  begin
    if not St.IsIndexValid(iLine) then Break;
    NLineLen:= St.LinesLen[iLine];

    if NLineLen<FOptMinLineLenToCalcURL then Continue;
    if NLineLen>FOptMaxLineLenToCalcURL then Continue;

    LinkArrayPtr:= FLinkCache.FindData(iLine);
    if LinkArrayPtr=nil then
    begin
      Assert(Assigned(FRegexLinks), 'FRegexLinks not inited');
      FRegexLinks.InputString:= St.Lines[iLine]+#10; //add #10 to handle it in regex

      LinkIndex:= 0;
      LinkArray:= Default(TATLinkArray);
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

      LinePart.ColorFont:= Colors.Links;
      LinePart.ColorBG:= clNone;
      LinePart.ColorBorder:= Colors.Links;
      LinePart.BorderDown:= TATLineStyle.Solid;

      FAttribs.Add(
        Point(MatchPos-1, iLine),
        Point(MatchLen, 0),
        TATMarkerTags.Init(ATEditorOptions.UrlMarkerTag, 0),
        @LinePart
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

  if FRegexLinks=nil then exit;
  {$ifndef USE_FPC_REGEXPR}
  if not FRegexLinks.IsCompiled then exit;
  {$endif}

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
var
  Cur: TCursor;
  EdOther: TATSynEdit;
begin
  Sleep(30); //avoid big CPU load, as method is called often

  if (Source is TATSynEdit) and (Source<>Self) then
  begin
    EdOther:= TATSynEdit(Source);
    if EdOther.OptMouseDragDrop then
    begin
      //for drag to another control, we reverse Ctrl-pressed state
      if GetActualDragDropIsCopying then
        Cur:= crDrag
      else
        Cur:= crMultiDrag;
      EdOther.DragCursor:= Cur;
      EdOther.Cursor:= Cur;
    end;
  end;

  Accept:=
    FOptMouseDragDrop and
    (not ModeReadOnly) and
    (Source is TATSynEdit) and
    (TATSynEdit(Source).Carets.Count=1) and
    (TATSynEdit(Source).Carets[0].IsSelection) and
    (not ModeOneLine or not TATSynEdit(Source).Carets[0].IsMultilineSelection);

  if Accept then
  begin
    Update;
  end;
end;

procedure TATSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  SText: atString;
  Pnt: TPoint;
  PntCoord: TATPoint;
  Details: TATEditorPosDetails;
begin
  if not (Source is TATSynEdit) then exit;

  //this check means: method runs only on drop from another editor
  if (Source=Self) then exit;

  SText:= TATSynEdit(Source).TextSelected;
  if SText='' then exit;

  PntCoord:= ATPoint(X, Y);
  Pnt:= ClientPosToCaretPos(PntCoord, Details);
  if Strings.IsIndexValid(Pnt.Y) then
  begin
    DoCaretSingle(Pnt.X, Pnt.Y);
    DoCommand(cCommand_TextInsert, TATCommandInvoke.Internal, SText);
    if ATEditorOptions.MouseDragDropFocusesTargetEditor then
      SetFocus;

    //Ctrl is pressed: delete block from src
    //note: it's opposite to the drag-drop in the single document
    if GetActualDragDropIsCopying then
      TATSynEdit(Source).DoCommand(cCommand_TextDeleteSelection, TATCommandInvoke.Internal);
  end;
end;

procedure TATSynEdit.OnNewScrollbarVertChanged(Sender: TObject);
//don't call WMVScroll with SB_THUMBPOSITION, because it is limited by LongInt,
//and we need to support big Int64 values
begin
  if FScrollbarLock then exit;
  UpdateScrollInfoFromSmoothPos(FScrollVert, FScrollbarVert.Position);
  InvalidateEx(true, true);
  //show scroll hint
  DoHintShowForScrolling;
end;

procedure TATSynEdit.OnNewScrollbarHorzChanged(Sender: TObject);
//don't call WMHScroll with SB_THUMBPOSITION, because it is limited by LongInt,
//and we need to support big Int64 values
begin
  if FScrollbarLock then exit;
  UpdateScrollInfoFromSmoothPos(FScrollHorz, FScrollbarHorz.Position);
  InvalidateEx(true, true);
end;

procedure TATSynEdit.TimerIdleTick(Sender: TObject);
begin
  FTimerIdle.Enabled:= false;
  if Assigned(FOnIdle) then
    FOnIdle(Self);
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorIdle(Self);
end;

procedure TATSynEdit.DoStringsOnChangeEx(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: SizeInt);
//we are called inside BeginEditing/EndEditing - just remember top edited line
var
  St: TATStrings;
begin
  St:= Strings;

  if Fold.Count>0 then
  begin
    Fold.Update(AChange, ALine, AItemCount);
    Fold.ClearLineIndexer(St.Count, true);
    Fold.UpdateLineIndexer;
  end;

  if not St.EditingActive then
    FlushEditingChangeEx(AChange, ALine, AItemCount);
end;

procedure TATSynEdit.DoStringsOnChangeLog(Sender: TObject; ALine: SizeInt);
var
  St: TATStrings;
begin
  St:= Strings;
  if not St.EditingActive then
    FlushEditingChangeLog(ALine);
end;

procedure TATSynEdit.FlushEditingChangeEx(AChange: TATLineChangeKind; ALine, AItemCount: SizeInt);
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorChangeEx(Self, AChange, ALine, AItemCount);
end;

procedure TATSynEdit.FlushEditingChangeLog(ALine: SizeInt);
begin
  if Assigned(FOnChangeLog) then
    FOnChangeLog(Self, ALine);
end;

procedure TATSynEdit.StartTimerDelayedParsing;
begin
  FTimerDelayedParsing.Enabled:= false;
  FTimerDelayedParsing.Enabled:= FTimersEnabled;

  if Carets.Count>0 then
    FLastCommandDelayedParsingOnLine:= Min(
      FLastCommandDelayedParsingOnLine,
      Max(0, Carets[0].FirstTouchedLine-1)
      );
end;

procedure TATSynEdit.TimerDelayedParsingTick(Sender: TObject);
//to solve CudaText issue #3403
//const
//  c: integer=0;
begin
  FTimerDelayedParsing.Enabled:= false;
  if Assigned(FOnChangeLog) then
    FOnChangeLog(Self, FLastCommandDelayedParsingOnLine);
  {
  //debug
  inc(c);
  Application.MainForm.Caption:= 'delayed parse: '+inttostr(c)+': '+inttostr(FLastCommandDelayedParsingOnLine);
  }
  FLastCommandDelayedParsingOnLine:= MaxInt;
end;

procedure TATSynEdit.TimerFlickerTick(Sender: TObject);
begin
  FTimerFlicker.Enabled:= false;
  Include(FPaintFlags, TATEditorInternalFlag.Bitmap);
  inherited Invalidate;
end;

procedure TATSynEdit.DoStringsOnProgress(Sender: TObject; var ACancel: boolean);
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

function TATSynEdit.DoGetTokenKind(AX, AY: integer): TATTokenKind;
begin
  if Assigned(FOnGetToken) then
    Result:= FOnGetToken(Self, AX, AY)
  else
    Result:= TATTokenKind.Other;
end;

procedure TATSynEdit.DoPaintTextFragment(C: TCanvas;
  const ARect: TRect;
  ALineFrom: integer;
  AConsiderWrapInfo: boolean;
  AColorBG, AColorBorder: TColor;
  AFontSize: integer;
  AWithSelection: boolean);
var
  St: TATStrings;
  NOutputStrWidth: Int64;
  NLine, NWrapIndex: integer;
  NVisibleColumns: integer;
  NTextX, NTextY, NFontSize: integer;
  NColorAfter: TColor;
  WrapItem: TATWrapItem;
  TextOutProps: TATCanvasTextOutProps;
  SText: UnicodeString;
  ChSize: TSize;
begin
  St:= Strings;
  C.Brush.Color:= AColorBG;
  C.FillRect(ARect);

  TextOutProps:= Default(TATCanvasTextOutProps);

  TextOutProps.Editor:= Self;
  TextOutProps.TabHelper:= FTabHelper;
  TextOutProps.CharsSkipped:= 0;
  TextOutProps.DrawEvent:= nil;
  TextOutProps.ControlWidth:= ARect.Width;
  TextOutProps.SpacingTopEdge:= FSpacingTopEdge;
  TextOutProps.SpacingTop:= FSpacingTop;

  TextOutProps.ShowUnprinted:= FUnprintedVisible and FUnprintedSpaces;
  TextOutProps.ShowUnprintedSpacesTrailing:= FUnprintedSpacesTrailing;
  TextOutProps.ShowUnprintedSpacesBothEnds:= FUnprintedSpacesBothEnds;
  TextOutProps.ShowUnprintedSpacesOnlyInSelection:= FUnprintedSpacesOnlyInSelection and TempSel_IsSelection;
  TextOutProps.ShowUnprintedSpacesAlsoInSelection:= not FUnprintedSpacesOnlyInSelection and FUnprintedSpacesAlsoInSelection and TempSel_IsSelection;
  TextOutProps.ShowUnprintedForceTabs:= FUnprintedForceTabs;
  TextOutProps.DetectIsPosSelected:= @IsPosSelected;

  TextOutProps.ShowFontLigatures:= FOptShowFontLigatures;
  TextOutProps.ColorNormalFont:= Colors.TextFont;
  TextOutProps.ColorUnprintedFont:= Colors.UnprintedFont;
  TextOutProps.ColorUnprintedHexFont:= Colors.UnprintedHexFont;

  TextOutProps.FontProportional:= FFontProportional;

  if AFontSize<=2 then
  begin
    TextOutProps.FontNormal_Name:= Font.Name;
    TextOutProps.FontNormal_Size:= DoScaleFont(Font.Size);

    TextOutProps.FontItalic_Name:= FontItalic.Name;
    TextOutProps.FontItalic_Size:= DoScaleFont(FontItalic.Size);

    TextOutProps.FontBold_Name:= FontBold.Name;
    TextOutProps.FontBold_Size:= DoScaleFont(FontBold.Size);

    TextOutProps.FontBoldItalic_Name:= FontBoldItalic.Name;
    TextOutProps.FontBoldItalic_Size:= DoScaleFont(FontBoldItalic.Size);

    TextOutProps.CharSize:= FCharSize;
  end
  else
  begin
    NFontSize:= DoScaleFont(AFontSize);

    TextOutProps.FontNormal_Name:= Font.Name;
    TextOutProps.FontNormal_Size:= NFontSize;

    TextOutProps.FontItalic_Name:= FontItalic.Name;
    TextOutProps.FontItalic_Size:= NFontSize;

    TextOutProps.FontBold_Name:= FontBold.Name;
    TextOutProps.FontBold_Size:= NFontSize;

    TextOutProps.FontBoldItalic_Name:= FontBoldItalic.Name;
    TextOutProps.FontBoldItalic_Size:= NFontSize;

    FBitmap.Canvas.Font.Size:= NFontSize;
    ChSize:= FBitmap.Canvas.TextExtent('0');
    TextOutProps.CharSize.XScaled:= Max(1, ChSize.cx) * ATEditorCharXScale;
    TextOutProps.CharSize.Y:= Max(1, ChSize.cy);
    TextOutProps.CharSize.XSpacePercents:= 100;
  end;

  if AConsiderWrapInfo then
    NWrapIndex:= WrapInfo.FindIndexOfCaretPos(Point(0, ALineFrom));

  if FFontProportional then
    NVisibleColumns:= ATEditorOptions.MaxVisibleColumns
  else
    NVisibleColumns:= GetVisibleColumns;

  for NLine:= ALineFrom to St.Count-1 do
  begin
    if not St.IsIndexValid(NLine) then Break;
    NColorAfter:= clNone;
    if AConsiderWrapInfo then
    begin
      if NWrapIndex<0 then Break;
      WrapItem:= WrapInfo[NWrapIndex];
      Inc(NWrapIndex);
    end
    else
    begin
      WrapItem:= Default(TATWrapItem);
      WrapItem.NLineIndex:= NLine;
      WrapItem.NCharIndex:= 1;
      WrapItem.NLength:= St.LinesLen[NLine];
    end;

    DoCalcLineHilite(
      WrapItem,
      FParts{%H-},
      0,
      ATEditorOptions.MaxCharsForOutput,
      AColorBG,
      false,
      NColorAfter,
      true,
      AWithSelection
      );

    SText:= St.LineSub(
        WrapItem.NLineIndex,
        WrapItem.NCharIndex,
        NVisibleColumns);

    if FOptMaskCharUsed then
      SText:= StringOfCharW(FOptMaskChar, Length(SText));

    TextOutProps.HasAsciiNoTabs:= not FFontProportional and St.LinesHasAsciiNoTabs[WrapItem.NLineIndex];
    TextOutProps.SuperFast:= false;
    TextOutProps.LineIndex:= WrapItem.NLineIndex;
    TextOutProps.CharIndexInLine:= WrapItem.NCharIndex;

    NTextX:= ATEditorOptions.SizeIndentTooltipX + WrapItem.NIndent*TextOutProps.CharSize.XScaled*FCharSize.XSpacePercents div ATEditorCharXScale div 100;
    NTextY:= ATEditorOptions.SizeIndentTooltipY + TextOutProps.CharSize.Y*(NLine-ALineFrom);
    if NTextY>=ARect.Bottom then
      Break;

    CanvasTextOut(C,
      NTextX,
      NTextY,
      SText,
      @FParts,
      NOutputStrWidth,
      TextOutProps
      )
   end;

  C.Brush.Color:= AColorBorder;
  C.FrameRect(ARect);
end;


procedure TATSynEdit.DoPaintMinimapTooltip(C: TCanvas);
var
  C_Bmp: TCanvas;
  RectAll: TRect;
  Pnt: TPoint;
  NWrapIndex, NLineCenter, NLineTop: integer;
  NPanelLeft, NPanelTop, NPanelWidth, NPanelHeight: integer;
begin
  Pnt:= ScreenToClient(Mouse.CursorPos);

  //workaround for LCL bug on Windows: MouseLeave is not called with _fast_ mouse moving
  if not PtInRect(FRectMinimap, Pnt) then exit;

  NPanelWidth:= FRectMain.Width * FMinimapTooltipWidthPercents div 100;
  if FMinimapAtLeft then
    NPanelLeft:= FRectMinimap.Right + 1
  else
    NPanelLeft:= FRectMinimap.Left - NPanelWidth - 1;
  NPanelHeight:= FMinimapTooltipHeight*FCharSize.Y + 2;
  NPanelTop:= Max(0, Min(Int64(ClientHeight-NPanelHeight),
    Pnt.Y - FCharSize.Y*FMinimapTooltipHeight div 2 ));

  if FMinimapTooltipBitmap=nil then
    FMinimapTooltipBitmap:= TBitmap.Create;
  FMinimapTooltipBitmap.SetSize(NPanelWidth, NPanelHeight);

  RectAll:= Rect(0, 0, NPanelWidth, NPanelHeight);
  C_Bmp:= FMinimapTooltipBitmap.Canvas;
  C_Bmp.Pen.Color:= Colors.MinimapTooltipBorder;
  C_Bmp.Brush.Color:= Colors.MinimapTooltipBG;
  C_Bmp.Rectangle(RectAll);

  NWrapIndex:= GetMinimap_ClickedPosToWrapIndex(Pnt.Y);
  if NWrapIndex<0 then exit;
  NLineCenter:= FWrapInfo[NWrapIndex].NLineIndex;
  NLineTop:= Max(0, NLineCenter - FMinimapTooltipHeight div 2);
  //NLineBottom:= Min(NLineTop + FMinimapTooltipHeight-1, Strings.Count-1);

  DoPaintTextFragment(C_Bmp, RectAll,
    NLineTop,
    //NLineBottom,
    true,
    Colors.MinimapTooltipBG,
    Colors.MinimapTooltipBorder,
    FMinimapTooltipFontSize,
    true
    );

  C.Draw(NPanelLeft, NPanelTop, FMinimapTooltipBitmap);

  FRectMinimapTooltip:= Rect(
    NPanelLeft,
    NPanelTop,
    NPanelLeft+NPanelWidth,
    NPanelTop+NPanelHeight
    );
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
  FFoldedMarkTooltip.Height:= Min(FFoldTooltipLineCount, FFoldedMarkCurrent.LineTo-FFoldedMarkCurrent.LineFrom+1) * FCharSize.Y + 1;
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
    DoPaintTextFragment(
      FFoldedMarkTooltip.Canvas,
      Rect(0, 0, FFoldedMarkTooltip.Width, FFoldedMarkTooltip.Height),
      FFoldedMarkCurrent.LineFrom,
      false, //to paint fully folded lines, must be False
      Colors.MinimapTooltipBG,
      Colors.MinimapTooltipBorder,
      0,
      true
      );
end;

procedure TATSynEdit.FoldedMarkMouseEnter(Sender: TObject);
begin
  if Assigned(FFoldedMarkTooltip) then
    FFoldedMarkTooltip.Hide;
end;


function TATSynEdit.DoGetGapRect(AIndex: integer; out ARect: TRect): boolean;
var
  GapItem: TATGapItem;
  Pnt: TATPoint;
begin
  Result:= false;
  ARect:= Rect(0, 0, 0, 0);

  if not Gaps.IsIndexValid(AIndex) then exit;
  GapItem:= Gaps.Items[AIndex];
  Pnt:= CaretPosToClientPos(Point(0, GapItem.LineIndex+1));

  ARect.Left:= FRectMain.Left;
  ARect.Right:= FRectMain.Right;
  ARect.Top:= Pnt.Y - GapItem.Size;
  ARect.Bottom:= Pnt.Y;

  //gap can be scrolled away: return False
  if ARect.Bottom<=FRectMain.Top then exit;
  if ARect.Top>=FRectMain.Bottom then exit;

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
  FTabHelper.CharSize:= FCharSize;
  FTabHelper.FontProportional:= FFontProportional;
  FTabHelper.SenderObj:= Self;
  FTabHelper.OnCalcTabSize:= FOnCalcTabSize;
  FTabHelper.OnCalcLineLen:= @DoCalcLineLen;
end;

procedure TATSynEdit.DoPaintTiming(C: TCanvas);
var
  RPlot: TRect;
  Rec, RecPrev: TATTimingRec;
  NRectBottom, i: integer;
  S: string;
begin
  if ModeOneLine then exit;
  if GetVisibleLines<ATTimingIndicator.MinEditorLines then exit;

  if not Assigned(FTimingQueue) then
    FTimingQueue:= TATTimingQueue.Create;

  while FTimingQueue.Size()>ATTimingIndicator.PlotWidth do
    FTimingQueue.PopBack();

  Rec.TimeAll:= FTickAll;
  Rec.TimeMinimap:= FTickMinimap;
  Rec.TimeTextout:= FTickTextout;
  FTimingQueue.PushFront(Rec);

  C.Font.Name:= Font.Name;
  C.Font.Color:= ATTimingIndicator.FontColor;
  C.Font.Size:= ATTimingIndicator.FontSize;
  C.Brush.Color:= ATTimingIndicator.FontBackColor;

  NRectBottom:= ClientHeight-1;

  RPlot.Left:= 0;
  RPlot.Right:= RPlot.Left+ATTimingIndicator.PlotWidth;
  RPlot.Bottom:= NRectBottom;
  RPlot.Top:= RPlot.Bottom-ATTimingIndicator.PlotHeight;

  C.Pen.Color:= ATTimingIndicator.LinesHorizColor;
  for i:= 0 to ATTimingIndicator.PlotHeight div 10 do
    C.Line(RPlot.Left, RPlot.Bottom-i*10, RPlot.Right, RPlot.Bottom-i*10);

  for i:= 1 to FTimingQueue.Size-1 do
  begin
    Rec:= FTimingQueue.Items[i];
    RecPrev:= FTimingQueue.Items[i-1];
    C.Pen.Color:= ATTimingIndicator.LinesPlotColorTextout;
    C.Line(
      RPlot.Left+i-1,
      RPlot.Bottom-Rec.TimeTextout,
      RPlot.Left+i,
      RPlot.Bottom-RecPrev.TimeTextout
      );
    C.Pen.Color:= ATTimingIndicator.LinesPlotColorMinimap;
    C.Line(
      RPlot.Left+i-1,
      RPlot.Bottom-Rec.TimeMinimap,
      RPlot.Left+i,
      RPlot.Bottom-RecPrev.TimeMinimap
      );
    C.Pen.Color:= ATTimingIndicator.LinesPlotColorAll;
    C.Line(
      RPlot.Left+i-1,
      RPlot.Bottom-Rec.TimeAll,
      RPlot.Left+i,
      RPlot.Bottom-RecPrev.TimeAll
      );
  end;

  S:= Format('#%03d, %d ms', [FPaintCounter, FTickAll]);
  if FMinimapVisible then
    S+= Format(', mmap %d ms', [FTickMinimap]);
  CanvasTextOutSimplest(C, RPlot.Right+3, ClientHeight - ATTimingIndicator.FontSize * 18 div 10, S);
end;


function TATSynEdit.GetEncodingName: string;
var
  St: TATStrings;
begin
  St:= Strings;
  case St.Encoding of
    TATFileEncoding.ANSI:
      begin
        Result:= cEncConvNames[St.EncodingCodepage];
      end;
    TATFileEncoding.UTF8:
      begin
        if St.SaveSignUtf8 then
          Result:= cEncNameUtf8_WithBom
        else
          Result:= cEncNameUtf8_NoBom;
      end;
    TATFileEncoding.UTF16LE:
      begin
        if St.SaveSignWide then
          Result:= cEncNameUtf16LE_WithBom
        else
          Result:= cEncNameUtf16LE_NoBom;
      end;
    TATFileEncoding.UTF16BE:
      begin
        if St.SaveSignWide then
          Result:= cEncNameUtf16BE_WithBom
        else
          Result:= cEncNameUtf16BE_NoBom;
      end;
    TATFileEncoding.UTF32LE:
      begin
        if St.SaveSignWide then
          Result:= cEncNameUtf32LE_WithBom
        else
          Result:= cEncNameUtf32LE_NoBom;
      end;
    TATFileEncoding.UTF32BE:
      begin
        if St.SaveSignWide then
          Result:= cEncNameUtf32BE_WithBom
        else
          Result:= cEncNameUtf32BE_NoBom;
      end;
  end;
end;

procedure TATSynEdit.SetEncodingName(const AName: string);
var
  St: TATStrings;
begin
  if AName='' then exit;
  if SameText(AName, GetEncodingName) then exit;
  St:= Strings;

  if SameText(AName, cEncNameUtf8_WithBom) then begin St.Encoding:= TATFileEncoding.UTF8; St.SaveSignUtf8:= true; end else
  if SameText(AName, cEncNameUtf8_NoBom) then begin St.Encoding:= TATFileEncoding.UTF8; St.SaveSignUtf8:= false; end else
  if SameText(AName, cEncNameUtf16LE_WithBom) then begin St.Encoding:= TATFileEncoding.UTF16LE; St.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf16LE_NoBom) then begin St.Encoding:= TATFileEncoding.UTF16LE; St.SaveSignWide:= false; end else
  if SameText(AName, cEncNameUtf16BE_WithBom) then begin St.Encoding:= TATFileEncoding.UTF16BE; St.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf16BE_NoBom) then begin St.Encoding:= TATFileEncoding.UTF16BE; St.SaveSignWide:= false; end else
  if SameText(AName, cEncNameUtf32LE_WithBom) then begin St.Encoding:= TATFileEncoding.UTF32LE; St.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf32LE_NoBom) then begin St.Encoding:= TATFileEncoding.UTF32LE; St.SaveSignWide:= false; end else
  if SameText(AName, cEncNameUtf32BE_WithBom) then begin St.Encoding:= TATFileEncoding.UTF32BE; St.SaveSignWide:= true; end else
  if SameText(AName, cEncNameUtf32BE_NoBom) then begin St.Encoding:= TATFileEncoding.UTF32BE; St.SaveSignWide:= false; end else
  begin
    St.Encoding:= TATFileEncoding.ANSI;
    St.EncodingCodepage:= EncConvFindEncoding(LowerCase(AName), eidCP1252);
  end;
end;

procedure TATSynEdit.TextInsertAtCarets(const AText: atString; AKeepCaret,
  AOvrMode, ASelectThen: boolean);
var
  Res: TATCommandResults;
begin
  Res:= DoCommand_TextInsertAtCarets(AText, AKeepCaret, AOvrMode, ASelectThen, false);
  DoCommandResults(0, Res);
end;

procedure TATSynEdit.DoCaretsFixForSurrogatePairs(AMoveRight: boolean);
//this is used to prevert caret stopping
//a) inside surrogate pair (2 codes which make one glyph)
//b) on accent (combining) char; we skip _all_ chars (Unicode allows several accent chars)
var
  St: TATStrings;
  Caret: TATCaretItem;
  ch: WideChar;
  i: integer;
begin
  St:= Strings;
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if Caret.PosX<=0 then Continue;
    if not St.IsIndexValid(Caret.PosY) then Continue;
    ch:= St.LineCharAt(Caret.PosY, Caret.PosX+1);
    if ch=#0 then Continue;

    if IsCharSurrogateLow(ch) then
    begin
      Caret.PosX:= Caret.PosX+BoolToPlusMinusOne[AMoveRight];
      Continue;
    end;

    {
    //now we don't need to adjust caret pos for accent chars,
    //they have separate char-cell like letters
    while IsCharAccent(ch) do
    begin
      Caret.PosX:= Caret.PosX+BoolToPlusMinusOne[AMoveRight];
      ch:= St.LineCharAt(Caret.PosY, Caret.PosX+1);
    end;
    }
  end;
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

function TATSynEdit.DoCalcLineLen(ALineIndex: SizeInt): SizeInt;
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
    Result:= ATEditorScaleFont(AValue);
end;

function TATSynEdit.UpdateLinksRegexObject: boolean;
begin
  Result:= false;
  if FOptShowURLsRegex='' then exit;

  if FRegexLinks=nil then
    FRegexLinks:= TRegExpr.Create;

  try
    FRegexLinks.ModifierS:= false;
    FRegexLinks.ModifierM:= false; //M not needed
    FRegexLinks.ModifierI:= false; //I not needed to find links
    FRegexLinks.Expression:= FOptShowURLsRegex{%H-};
    FRegexLinks.Compile;
    Result:= true;
  except
  end;
end;


function TATSynEdit.GetFoldingAsString: string;
var
  L: TStringList;
  i: integer;
  R: PATFoldRange;
begin
  Result:= '';
  L:= TStringList.Create;
  try
    L.LineBreak:= ',';
    for i:= 0 to Fold.Count-1 do
    begin
      R:= Fold.ItemPtr(i);
      if R^.Folded then
        L.Add(IntToStr(R^.Y));
    end;
    Result:= L.Text;
  finally
    L.Free;
  end;
end;

procedure TATSynEdit.SetFoldingAsString(const AValue: string);
var
  St: TATStrings;
  Sep: TATStringSeparator;
  NLineTop, NLine, NRange: integer;
  bChange: boolean;
begin
  DoCommand_FoldUnAll;
  bChange:= FWrapUpdateNeeded;
  NLineTop:= LineTop;

  St:= Strings;
  Sep.Init(AValue);
  repeat
    if not Sep.GetItemInt(NLine, -1) then Break;

    if not St.IsIndexValid(NLine) then Continue;

    NRange:= Fold.FindRangeWithPlusAtLine(NLine);
    if NRange<0 then Continue;

    //if not Fold.ItemPtr(NRange)^.Folded then
    begin
      bChange:= true;
      DoRangeFold(NRange);
    end;
  until false;

  if bChange then
  begin
    if FScrollHorz.NPos>0 then
    begin
      //fix changed horz scroll, CudaText issue #1439
      FScrollHorz.SetZero;

      //SetZero may scroll view out of caret
      DoGotoCaret(TATCaretEdge.Top);
    end;

    //keep LineTop! CudaText issue #3055
    LineTop:= NLineTop;

    Update;
  end;
end;

procedure TATSynEdit.UpdateAndWait(AUpdateWrapInfo: boolean; APause: integer);
begin
  Update(AUpdateWrapInfo);
  Paint;
  Application.ProcessMessages;
  Sleep(APause);
end;

function TATSynEdit.IsPosInVisibleArea(AX, AY: integer): boolean;
var
  Pnt: TATPoint;
  NTop, NCount: integer;
begin
  NTop:= LineTop;
  if AY<NTop then
    exit(false);

  NCount:= Strings.Count;
  if NCount<=1 then
    exit(true);

  if OptWrapMode=TATEditorWrapMode.ModeOff then
  begin
    Result:= AY<=NTop+GetVisibleLines;
  end
  else
  begin
    if AY>LineBottom then
      exit(false);

    Pnt:= CaretPosToClientPos(Point(AX, AY));
    if Pnt.Y=-1 then
      exit(true);

    Result:= ATPointInRect(FRectMainVisible, Pnt);
  end;
end;

procedure TATSynEdit.DoStringsOnUndoBefore(Sender: TObject; AX, AY: SizeInt);
var
  OldOption: boolean;
  Tick: QWord;
begin
  FLastUndoPaused:= false;

  if ModeOneLine then exit;
  if FOptUndoPause<=0 then exit;
  if Carets.Count>1 then exit;
  if AY<0 then exit;
  if AY>=Strings.Count then exit; //must have for the case: big file; Ctrl+A, Del; Undo
  if IsPosInVisibleArea(AX, AY) then exit;

  Tick:= GetTickCount64;
  if FLastUndoTick>0 then
    if Tick-FLastUndoTick<FOptUndoPause2 then
      exit;

  FLastUndoPaused:= true;
  FLastUndoTick:= Tick;

  if FOptUndoPauseHighlightLine then
  begin
    OldOption:= OptShowCurLine;
    OptShowCurLine:= true;
  end;

  DoGotoPos(
    Point(AX, AY),
    Point(-1, -1),
    FOptUndoIndentHorz,
    FOptUndoIndentVert,
    true,
    TATEditorActionIfFolded.Unfold,
    false,
    false);
  { //not good
  DoShowPos(
    Point(0, ALine),
    FOptUndoIndentHorz,
    FOptUndoIndentVert,
    true,
    true);
    }

  UpdateAndWait(true, FOptUndoPause);

  if FOptUndoPauseHighlightLine then
    OptShowCurLine:= OldOption;
end;

procedure TATSynEdit.DoStringsOnUndoAfter(Sender: TObject; AX, AY: SizeInt);
var
  OldOption: boolean;
begin
  if not FLastUndoPaused then exit;
  {
  if ModeOneLine then exit;
  if FOptUndoPause<=0 then exit;
  if AY<0 then exit;
  if AY>=Strings.Count then exit; //must have for the case: big file; Ctrl+A, Del; Undo
  if IsPosInVisibleArea(AX, AY) then exit;
  }

  if FOptUndoPauseHighlightLine then
  begin
    OldOption:= OptShowCurLine;
    OptShowCurLine:= true;
  end;

  UpdateAndWait(true, FOptUndoPause);

  if FOptUndoPauseHighlightLine then
    OptShowCurLine:= OldOption;
end;

procedure TATSynEdit.DoStringsOnUnfoldLine(Sender: TObject; ALine: SizeInt);
var
  N: integer;
begin
  N:= Fold.FindRangeWithPlusAtLine(ALine);
  if N>=0 then
    if Fold.ItemPtr(N)^.Folded then
    begin
      DoRangeUnfold(N);
      UpdateWrapInfo(true); //without this, WrapInfo is not updated somehow
    end;
end;


procedure TATSynEdit.ActionAddJumpToUndo;
var
  St: TATStrings;
begin
  St:= Strings;
  if FOptUndoForCaretJump then
  begin
    St.SetGroupMark; //solve CudaText #3269
    St.ActionAddJumpToUndo(St.CaretsAfterLastEdition);
    //ActionAddJumpToUndo(GetCaretsArray); //bad, parameter is needed only for another array
  end;
end;

function TATSynEdit.IsEmpty: boolean;
var
  St: TATStrings;
  NCount: integer;
begin
  St:= Strings;
  NCount:= St.Count;
  Result:= (NCount=0) or ((NCount=1) and (St.LinesLen[0]=0));
end;

procedure TATSynEdit.BeginEditing;
var
  St: TATStrings;
begin
  St:= Strings;
  St.EditingActive:= true;
  St.EditingTopLine:= -1;
end;

procedure TATSynEdit.EndEditing(ATextChanged: boolean);
var
  St: TATStrings;
begin
  St:= Strings;
  St.EditingActive:= false;
  if ATextChanged then
    if St.EditingTopLine>=0 then
    begin
      //FlushEditingChangeEx(cLineChangeEdited, FEditingTopLine, 1); //not needed
      FlushEditingChangeLog(St.EditingTopLine);
    end;
end;

procedure TATSynEdit.UpdateGapForms(ABeforePaint: boolean);
var
  Gap: TATGapItem;
  i: integer;
begin
  if ABeforePaint then
  begin
    for i:= 0 to Gaps.Count-1 do
    begin
      Gap:= Gaps[i];
      if Assigned(Gap.Form) then
        Gap.FormVisible:= false;
    end;
  end
  else
  begin
    for i:= 0 to Gaps.Count-1 do
    begin
      Gap:= Gaps[i];
      if Assigned(Gap.Form) then
        Gap.Form.Visible:= Gap.FormVisible;
    end;
  end;
end;

procedure TATSynEdit.SetOptScaleFont(AValue: integer);
begin
  if FOptScaleFont=AValue then Exit;
  FOptScaleFont:=AValue;
  UpdateInitialVars(Canvas);
end;


procedure TATSynEdit.DoHandleWheelRecord(const ARec: TATEditorWheelRecord);
begin
  case ARec.Kind of
    TATEditorWheelRecordKind.Vert:
      begin
        //w/o this handler wheel works only with OS scrollbars, need with new scrollbars too
        DoScrollByDeltaInPixels(
          0,
          FCharSize.Y * -FOptMouseWheelScrollVertSpeed * ARec.Delta div 120
          );
      end;

    TATEditorWheelRecordKind.Horz:
      begin
        DoScrollByDelta(
          -FOptMouseWheelScrollHorzSpeed * ARec.Delta div 120,
          0
          );
      end;

    TATEditorWheelRecordKind.Zoom:
      begin
        DoScaleFontDelta(ARec.Delta>0, false);
        DoEventZoom;
      end;
  end;
end;

procedure TATSynEdit.UpdateGutterBandIndexes;
begin
  FGutterBandBookmarks:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks);
  FGutterBandNumbers:=   FGutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers);
  FGutterBandStates:=    FGutter.FindIndexByTag(ATEditorOptions.GutterTagLineStates);
  FGutterBandFolding:=   FGutter.FindIndexByTag(ATEditorOptions.GutterTagFolding);
  FGutterBandSeparator:= FGutter.FindIndexByTag(ATEditorOptions.GutterTagSeparator);
  FGutterBandEmpty:=     FGutter.FindIndexByTag(ATEditorOptions.GutterTagEmpty);
end;

type
  TCustomControlHack = class(TCustomControl);

function HandleMouseDownToHandleExtraMouseButtons(Ctl: TCustomControl; Button: TMouseButton; Shift: TShiftState): boolean;
var
  NewKey: word;
begin
  case Button of
    mbExtra1:
      begin
        NewKey:= VK_BROWSER_BACK;
        TCustomControlHack(Ctl).KeyDown(NewKey, Shift);
        Result:= true;
      end;
    mbExtra2:
      begin
        NewKey:= VK_BROWSER_FORWARD;
        TCustomControlHack(Ctl).KeyDown(NewKey, Shift);
        Result:= true;
      end;
    else
      Result:= false;
  end;
end;

function TATSynEdit.GetUndoForMarkers: boolean;
begin
  Result:= Assigned(FStringsInt.OnGetMarkersArray);
end;

procedure TATSynEdit.SetUndoForMarkers(AValue: boolean);
begin
  if AValue then
  begin
    FStringsInt.OnGetMarkersArray:= @GetMarkersArray;
    FStringsInt.OnSetMarkersArray:= @SetMarkersArray;
  end
  else
  begin
    FStringsInt.OnGetMarkersArray:= nil;
    FStringsInt.OnSetMarkersArray:= nil;
  end;
end;

function TATSynEdit.GetUndoForAttribs: boolean;
begin
  Result:= Assigned(FStringsInt.OnGetAttribsArray);
end;

procedure TATSynEdit.SetUndoForAttribs(AValue: boolean);
begin
  if AValue then
  begin
    FStringsInt.OnGetAttribsArray:= @GetAttribsArray;
    FStringsInt.OnSetAttribsArray:= @SetAttribsArray;
  end
  else
  begin
    FStringsInt.OnGetAttribsArray:= nil;
    FStringsInt.OnSetAttribsArray:= nil;
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

  RegExprModifierS:= False;
  RegExprModifierM:= True;
  {$ifndef USE_FPC_REGEXPR}
  RegExprUsePairedBreak:= False;
  RegExprReplaceLineBreak:= #10;
  {$endif}

end.
