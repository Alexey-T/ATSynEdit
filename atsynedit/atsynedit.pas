unit ATSynEdit;

{$mode delphi}
//{$define beep_wrapinfo}
//{$define debug_findwrapindex}
//{$define beep_cached_update}

interface

uses
  Classes, SysUtils, Graphics,
  Controls, ExtCtrls, Menus,
  LMessages, LCLType,
  ATStringProc, ATStrings, ATCanvasProc, ATGutter,
  ATCarets, ATKeyMapping, ATSynEdit_WrapInfo;

type
  TATDirection = (
    cDirLeft,
    cDirRight,
    cDirUp,
    cDirDown
    );

  TATCommandResult = (
    cResultText,
    cResultCaretAny,
    cResultCaretLeft,
    cResultCaretTop,
    cResultCaretRight,
    cResultCaretBottom,
    cResultKeepColumnSel,
    cResultScroll,
    cResultState
    );
  TATCommandResults = set of TATCommandResult;

type
  TATSynEditColors = class
  public
    TextFont,
    TextBG,
    TextSelFont,
    TextSelBG,
    Caret,
    GutterFont,
    GutterBG,
    GutterCaretBG,
    CurLineBG,
    MarginRight,
    MarginCaret,
    MarginUser,
    IndentLines,
    RulerFont,
    RulerBG,
    CollapseLine,
    CollapseFont,
    UnprintedFont,
    UnprintedBG,
    UnprintedHexFont,
    MinimapBorder,
    MinimapSelBG,
    MicromapBG,
    StateChanged,
    StateAdded,
    StateSaved,
    LockedBG: TColor;
  end;

  TATAutoIndentKind = (
    cIndentAsIs,
    cIndentSpaces,
    cIndentTabsSpaces,
    cIndentTabsOnly
    );

  TATPageUpDownSize = (
    cPageSizeFull,
    cPageSizeFullMinus1,
    cPageSizeHalf
    );

  TATAutoScroll = (
    cScrollNone,
    cScrollUp,
    cScrollDown,
    cScrollLeft,
    cScrollRight
    );

  TATSynWrapMode = (
    cWrapOff,
    cWrapOn,
    cWrapAtMargin
    );

  TATSynNumbersStyle = (
    cNumbersAll,
    cNumbersNone,
    cNumbersEach10th,
    cNumbersEach5th,
    cNumbersForCarets
    );

  TATSynPaintFlag = (
    cPaintUpdateBitmap,
    cPaintUpdateCaretsCoords,
    cPaintUpdateScrollbars
    );
  TATSynPaintFlags = set of TATSynPaintFlag;

  TATSynScrollInfo = record
    NMin,
    NMax,
    NPage,
    NPos: integer;
  end;

  TATSynCaretShape = (
    cCaretShapeFull,
    cCaretShapeVert1px,
    cCaretShapeVert2px,
    cCaretShapeVert3px,
    cCaretShapeVert4px,
    cCaretShapeVert10percent,
    cCaretShapeVert20percent,
    cCaretShapeVert30percent,
    cCaretShapeVert40percent,
    cCaretShapeVert50percent,
    cCaretShapeHorz1px,
    cCaretShapeHorz2px,
    cCaretShapeHorz3px,
    cCaretShapeHorz4px,
    cCaretShapeHorz10percent,
    cCaretShapeHorz20percent,
    cCaretShapeHorz30percent,
    cCaretShapeHorz40percent,
    cCaretShapeHorz50percent
    );

const
  cInitCaretShape = cCaretShapeVert2px;
  cInitSpacingText = 1;
  cInitSpacingMinimap = -1;
  cInitTimerBlink = 600;
  cInitTimerScroll = 80;
  cInitMinimapVisible = false;
  cInitMicromapVisible = false;
  cInitMarginRight = 80;
  cInitTabSize = 10;
  cInitMicromapWidth = 30;
  cInitMinimapWidth = 160;
  cInitMinimapFontSize = 2;
  cInitNumbersStyle = cNumbersEach5th;
  cInitBitmapWidth = 1000;
  cInitBitmapHeight = 800;

  cGutterBands = 5;
  cSizeGutterBandBm = 16;
  cSizeGutterBandNum = 10;
  cSizeGutterBandFold = 0;
  cSizeGutterBandState = 3;
  cSizeGutterBandEmpty = 2;
  cCollapseMarkIndent = 3;
  cScrollKeepHorz = 1; //n chars allow handy clicking after eol
  cScrollIndentCaretHorz = 10;
  cScrollIndentCaretVert = 0; //mustbe 0, 1 gives jumps 2 lines on moving down
  cScrollIndentGotoHorz = 10;
  cScrollIndentGotoVert = 3;
  cScrollAutoHorz = 10; //scroll N chrs each timer tick
  cScrollAutoVert = 1;
  cResizeBitmapStep = 100;
  cOffsetTextTop = 1;
  cSizeGutterNumOffsetLeft = 5;
  cSizeGutterNumOffsetRight = 4;
  cSizeRulerHeight = 19;
  cSizeRulerMarkSmall = 3;
  cSizeRulerMarkBig = 7;
  cMinFontSize = 6;
  cMinTabSize = 1;
  cMaxTabSize = 16;
  cMaxCharsForOutput = 400;
  cMinMinimapWidth = 30;
  cMinWrapColumn = 20;
  cMinWrapColumnAbs = 4;
  cMinMarginRt = 20;
  cMinCaretTime = 300;
  cMaxCaretTime = 2000;
  cMinCharsAfterAnyIndent = 20;
  cMaxLinesForOldWrapUpdate = 100;

var
  cRectEmpty: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  cClipFormatId: integer = 0; //must be inited
  cClipSignatureColumn: integer = $1000;

type
  TATSynEditCommandEvent = procedure(Sender: TObject; ACommand: integer; var AHandled: boolean) of object;
  TATSynEditClickGutterEvent = procedure(Sender: TObject; ABand: integer; ALineNum: integer) of object;
  TATSynEditDrawBookmarkEvent = procedure(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect) of object;


type
  { TATSynEdit }

  TATSynEdit = class(TCustomControl)
  private
    FTimerBlink: TTimer;
    FTimerScroll: TTimer;
    FPaintStatic: boolean;
    FPaintFlags: TATSynPaintFlags;
    FPaintLocked: integer;
    FBitmap: TBitmap;
    FKeyMapping: TATKeyMapping;
    FMarginRight: integer;
    FMarginList: TList;
    FStringsInt,
    FStringsExt: TATStrings;
    FTextOffset: TPoint;
    FSelRect: TRect;
    FSelRectBegin,
    FSelRectEnd: TPoint;
    FCarets: TATCarets;
    FCaretShape,
    FCaretShapeOvr: TATSynCaretShape;
    FCaretShown: boolean;
    FCaretVirtual: boolean;
    FCaretSpecPos: boolean;
    FCaretStopUnfocused: boolean;
    FMenu: TPopupMenu;
    FOver: boolean;
    FMouseDownPnt: TPoint;
    FMouseDownNumber: integer;
    FMouseDownDouble: boolean;
    FMouseDragging: boolean;
    FMouseAutoScroll: TATAutoScroll;
    FOnCaretMoved: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnScrolled: TNotifyEvent;
    FOnClickGutter: TATSynEditClickGutterEvent;
    FOnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent;
    FOnDrawLine: TATSynEditDrawLineEvent;
    FOnStateChanged: TNotifyEvent;
    FOnCommand: TATSynEditCommandEvent;
    FWrapInfo: TATSynWrapInfo;
    FWrapColumn: integer;
    FWrapMode: TATSynWrapMode;
    FWrapUpdateNeeded: boolean;
    FWrapIndented: boolean;
    FUnprintedVisible,
    FUnprintedSpaces,
    FUnprintedEnds,
    FUnprintedEndsDetails,
    FUnprintedReplaceSpec: boolean;
    FPrevVisibleColumns: integer;
    FCharSize: TPoint;
    FCharSizeMinimap: TPoint;
    FCharSpacingText,
    FCharSpacingMinimap: TPoint;
    FTabSize: integer;
    FGutter: TATGutter;
    FGutterBandBm,
    FGutterBandNum,
    FGutterBandState,
    FGutterBandFold,
    FGutterBandEmpty: integer;
    FColors: TATSynEditColors;
    FRectMain,
    FRectMinimap,
    FRectMicromap,
    FRectGutter,
    FRectRuler: TRect;
    FScrollVert,
    FScrollHorz: TATSynScrollInfo;
    FScrollVertMinimap,
    FScrollHorzMinimap: TATSynScrollInfo;
    FPrevHorz,
    FPrevVert: TATSynScrollInfo;
    FMinimapWidth: integer;
    FMinimapFontSize: integer;
    FMinimapVisible: boolean;
    FMicromapWidth: integer;
    FMicromapVisible: boolean;
    FOptUndoGrouped: boolean;
    FOptIndentSize: integer;
    FOptIndentKeepsAlign: boolean;
    FOptRulerVisible: boolean;
    FOptRulerSize: integer;
    FOptRulerFontSize: integer;
    FOptRulerMarkSizeSmall: integer;
    FOptRulerMarkSizeBig: integer;
    FOptGutterVisible: boolean;
    FOptNumbersStyle: TATSynNumbersStyle;
    FOptWordChars: atString;
    FOptAutoIndent: boolean;
    FOptAutoIndentKind: TATAutoIndentKind;
    FOptTabSpaces: boolean;
    FOptLastLineOnTop: boolean;
    FOptOverwriteSel: boolean;
    FOptUseOverOnPaste: boolean;
    FOptKeyNavigateWrapped: boolean;
    FOptCopyLinesIfNoSel: boolean;
    FOptCutLinesIfNoSel: boolean;
    FOptHiliteSelFull: boolean;
    FOptShowCurLine: boolean;
    FOptShowCurColumn: boolean;
    FOptMouse2ClickSelectsLine: boolean;
    FOptMouse3ClickSelectsLine: boolean;
    FOptMouse2ClickDragSelectsWords: boolean;
    FOptMouseDragDrop: boolean;
    FOptMouseRightClickMovesCaret: boolean;
    FOptMouseGutterClickSelectsLine: boolean;
    FOptKeyPageUpDownSize: TATPageUpDownSize;
    FOptKeyLeftRightSwapSel: boolean;
    FOptKeyHomeToNonSpace: boolean;
    FOptKeyEndToNonSpace: boolean;
    FOptKeyTabIndents: boolean;
    FOptShowIndentLines: boolean;
    FOptShowGutterCaretBG: boolean;
    //
    procedure DebugFindWrapIndex;
    procedure DoCaretsExtend(ADown: boolean; ALines: integer);
    procedure DoDropText;
    procedure DoFindWrapIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
    procedure DoMinimapClick(APosY: integer);
    procedure DoPaintSelectedLineBG(C: TCanvas; ACharSize: TPoint;
      const AVisRect: TRect; APointLeft: TPoint; APointText: TPoint;
      ALineIndex: integer; AEolSelected: boolean;
      const AScrollHorz: TATSynScrollInfo);
    function GetAutoIndentString(APosX, APosY: integer): atString;
    function GetRedoCount: integer;
    procedure GetSelectedLines(ACaretIndex: integer; out AFrom, ATo: integer);
    function GetUndoAfterSave: boolean;
    function GetUndoCount: integer;
    function GetUndoLimit: integer;
    procedure DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer; AItems: TList);
    procedure InitColors;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure DoCalcLineHilite(const AItem: TATSynWrapItem; var AParts: TATLineParts;
      ACharsSkipped, ACharsMax: integer; AColorBG: TColor);
    procedure DoInitDefaultPopupMenu;
    //select
    procedure DoSelect_ColumnBlock(P1, P2: TPoint);
    procedure DoSelect_CharRange(ACaretIndex: integer; Pnt: TPoint);
    procedure DoSelect_WordRange(ACaretIndex: integer; P1, P2: TPoint);
    procedure DoSelect_Word_ByClick;
    procedure DoSelect_Line_ByClick;
    procedure DoSelect_None;
    //paint
    function DoPaint(AFlags: TATSynPaintFlags): boolean;
    procedure DoPaintMarginLineTo(C: TCanvas; AX: integer; AColor: TColor);
    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintRulerTo(C: TCanvas);
    procedure DoPaintTextTo(C: TCanvas;
      const ARect: TRect;
      const ACharSize: TPoint;
      AWithGutter, AWithUnprintable: boolean;
      var AScrollHorz, AScrollVert: TATSynScrollInfo);
    procedure DoPaintMinimapSelTo(C: TCanvas);
    procedure DoPaintMinimapTo(C: TCanvas);
    procedure DoPaintMicromapTo(C: TCanvas);
    procedure DoPaintMarginsTo(C: TCanvas);
    procedure DoPaintCollapseMark(C: TCanvas;
      const Str: atString;
      ALineStart: TPoint;
      AScrollPos: integer);
    procedure DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
    procedure DoPaintModeStatic;
    procedure DoPaintModeBlinking;
    //carets
    function GetCaretManyAllowed: boolean;
    function GetCaretSelectionIndex(P: TPoint): integer;
    function GetCaretTime: integer;
    function DoCaretSwapEdge(AMoveLeft: boolean): boolean;
    procedure DoCaretsSort;
    procedure DoCaretSingle(AX, AY: integer);
    procedure DoCaretSingleAsIs;
    //events
    procedure DoEventCarets;
    procedure DoEventScroll;
    procedure DoEventChange;
    procedure DoEventState;
    procedure DoEventClickGutter(ABandIndex, ALineNumber: integer);
    procedure DoEventCommand(ACommand: integer; out AHandled: boolean);
    procedure DoEventDrawBookmarkIcon(C: TCanvas; ALineNumber: integer; const ARect: TRect);
    //
    function GetCharSpacingX: integer;
    function GetCharSpacingY: integer;
    function GetLastPos: TPoint;
    function GetMarginString: string;
    function GetReadOnly: boolean;
    function GetScrollTop: integer;
    function GetTextForClipboard: string;
    function GetWrapInfoIndex(AMousePos: TPoint): integer;
    function GetStrings: TATStrings;
    function IsKeyMappingMatchedItem(const AKey: TShortcut;
      const AItem: TATKeyMappingItem): boolean;
    function IsPosSelected(AX, AY: integer): boolean;
    procedure SetCaretManyAllowed(AValue: boolean);
    procedure SetCaretTime(AValue: integer);
    procedure SetCaretShape(AValue: TATSynCaretShape);
    procedure SetCaretShapeOvr(AValue: TATSynCaretShape);
    procedure SetCharSpacingX(AValue: integer);
    procedure SetCharSpacingY(AValue: integer);
    procedure SetMarginString(AValue: string);
    procedure SetMicromapVisible(AValue: boolean);
    procedure SetMinimapVisible(AValue: boolean);
    procedure SetOver(AValue: boolean);
    procedure SetReadOnly(AValue: boolean);
    procedure SetScrollTop(AValue: integer);
    procedure SetStrings(Obj: TATStrings);
    function GetRectMain: TRect;
    function GetRectMinimap: TRect;
    function GetRectMicromap: TRect;
    function GetRectGutter: TRect;
    function GetRectRuler: TRect;
    function GetTextOffset: TPoint;
    function GetGutterNumbersWidth: integer;
    function GetPageLines: integer;
    function GetVisibleLines: integer;
    function GetVisibleColumns: integer;
    function GetVisibleLinesMinimap: integer;
    function GetMinimapScrollPos: integer;
    procedure SetTabSize(AValue: integer);
    procedure SetUndoAfterSave(AValue: boolean);
    procedure SetUndoLimit(AValue: integer);
    procedure SetWrapMode(AValue: TATSynWrapMode);
    procedure SetWrapIndented(AValue: boolean);
    procedure UpdateCursor;
    procedure UpdateGutterAutosize;
    procedure UpdateMinimapAutosize(C: TCanvas);
    function DoFormatLineNumber(N: integer): atString;
    procedure UpdateWrapInfo;
    function UpdateScrollbars: boolean;
    procedure UpdateScrollbarVert;
    procedure UpdateScrollbarHorz;
    procedure UpdateCaretsCoords(AOnlyLast: boolean = false);
    function GetCharSize(C: TCanvas; ACharSpacing: TPoint): TPoint;
    function GetScrollbarVisible(bVertical: boolean): boolean;
    procedure SetMarginRight(AValue: integer);
    procedure TimerBlinkTick(Sender: TObject);
    procedure TimerScrollTick(Sender: TObject);

    //carets
    procedure DoCaretAddToPoint(AX, AY: integer);
    procedure DoCaretsColumnToPoint(AX, AY: integer);
    procedure DoCaretsShift(APosX, APosY: integer; AShiftX, AShiftY: integer; AShiftBelowX: integer = 0);
    procedure DoCaretsDeleteOnSameLines;

    //editing
    procedure DoCommandResults(Res: TATCommandResults);
    function DoCommand_SelectColumn(ADir: TATDirection): TATCommandResults;
    function DoCommand_TextInsertColumnBlockOnce(const AText: atString; AKeepCaret: boolean): TATCommandResults;
    function DoCommand_CaretsExtend(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_Undo: TATCommandResults;
    function DoCommand_Redo: TATCommandResults;
    function DoCommand_TextIndentUnindent(ARight: boolean): TATCommandResults;
    function DoCommand_SelectWords: TATCommandResults;
    function DoCommand_SelectLines: TATCommandResults;
    function DoCommand_SelectAll: TATCommandResults;
    function DoCommand_Cancel: TATCommandResults;
    function DoCommand_ToggleReadOnly: TATCommandResults;
    function DoCommand_ToggleOverwrite: TATCommandResults;
    function DoCommand_GotoWord(ANext: boolean): TATCommandResults;
    function DoCommand_ScrollVert(ALines: integer): TATCommandResults;
    function DoCommand_TextInsertAtCarets(const AText: atString; AKeepCaret,
      AOvrMode, ASelectThen: boolean): TATCommandResults;
    function DoCommand_TextTabulation: TATCommandResults;
    function DoCommand_KeyHome: TATCommandResults;
    function DoCommand_KeyEnd: TATCommandResults;
    function DoCommand_KeyLeft(ASelCommand: boolean): TATCommandResults;
    function DoCommand_KeyRight(ASelCommand: boolean): TATCommandResults;
    function DoCommand_KeyUpDown(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_KeyUpDown_NextLine(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_KeyUpDown_Wrapped(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_TextDelete: TATCommandResults;
    function DoCommand_TextDeleteSelection: TATCommandResults;
    function DoCommand_TextDeleteLeft(ALen: integer): TATCommandResults;
    function DoCommand_TextDeleteRight(ALen: integer): TATCommandResults;
    function DoCommand_TextInsertEol(AKeepCaret: boolean): TATCommandResults;
    function DoCommand_TextDeleteLines: TATCommandResults;
    function DoCommand_TextDuplicateLine: TATCommandResults;
    function DoCommand_TextDeleteToLineBegin: TATCommandResults;
    function DoCommand_TextDeleteToLineEnd: TATCommandResults;
    function DoCommand_TextDeleteWord(ANext: boolean): TATCommandResults;
    function DoCommand_TextDeleteToFileEnd: TATCommandResults;
    function DoCommand_GotoTextBegin: TATCommandResults;
    function DoCommand_GotoTextEnd: TATCommandResults;
    function DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean): TATCommandResults;
    function DoCommand_ClipboardPasteColumnBlock(AKeepCaret: boolean): TATCommandResults;
    function DoCommand_ClipboardCopy(Append: boolean = false): TATCommandResults;
    function DoCommand_ClipboardCut: TATCommandResults;
    //
    function GetCommandFromKey(var Key: Word; Shift: TShiftState): integer;
    function DoMouseWheelAction(Shift: TShiftState; AUp: boolean): boolean;

  public
    //std
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    procedure Invalidate; override;
    procedure Update(
      AUpdateWrapInfo: boolean = false;
      AUpdateCaretsCoords: boolean = true); reintroduce;
    //text
    property Strings: TATStrings read GetStrings write SetStrings;
    property KeyMapping: TATKeyMapping read FKeyMapping;
    property ScrollTop: integer read GetScrollTop write SetScrollTop;
    property ModeOver: boolean read FOver write SetOver;
    property ModeReadOnly: boolean read GetReadOnly write SetReadOnly;
    property UndoCount: integer read GetUndoCount;
    property RedoCount: integer read GetRedoCount;
    property SelRect: TRect read FSelRect;
    //gutter
    property Gutter: TATGutter read FGutter;
    property GutterBandBm: integer read FGutterBandBm write FGutterBandBm;
    property GutterBandNum: integer read FGutterBandNum write FGutterBandNum;
    property GutterBandState: integer read FGutterBandState write FGutterBandState;
    property GutterBandFold: integer read FGutterBandFold write FGutterBandFold;
    property GutterBandEmpty: integer read FGutterBandEmpty write FGutterBandEmpty;
    //files
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    //carets
    function CaretPosToClientPos(P: TPoint): TPoint;
    function ClientPosToCaretPos(P: TPoint): TPoint;
    property Carets: TATCarets read FCarets;
    function IsLineWithCaret(ALine: integer): boolean;
    procedure DoShowPos(APnt: TPoint; AIndentHorz, AIndentVert: integer);
    procedure DoShowCaret(AEdge: TATCaretEdge);
    procedure DoGotoPos(APnt: TPoint);
    function GetCaretsArray: TPointArray;
    procedure SetCaretsArray(const L: TPointArray);
    //misc
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DoSelect_All;
    procedure DoSelect_Line(P: TPoint);
    procedure DoSelect_Word(P: TPoint);
    procedure DoSelect_LineRange(ALineFrom: integer; P: TPoint);
    procedure DoFoldLines(ALineFrom, ALineTo, ACharPosFrom: integer; AFold: boolean);
    procedure DoCommandExec(ACmd: integer; const AText: atString = '');
    procedure DoScrollByDelta(Dx, Dy: integer);
    procedure DoSizeChange(AInc: boolean);

  protected
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos{%H-}: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos{%H-}: TPoint): boolean; override;
    procedure DblClick; override;
    procedure TripleClick; override;
    //messages
    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  published
    //events
    property OnCaretMoved: TNotifyEvent read FOnCaretMoved write FOnCaretMoved;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnScrolled: TNotifyEvent read FOnScrolled write FOnScrolled;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnCommand: TATSynEditCommandEvent read FOnCommand write FOnCommand;
    property OnClickGutter: TATSynEditClickGutterEvent read FOnClickGutter write FOnClickGutter;
    property OnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent read FOnDrawBookmarkIcon write FOnDrawBookmarkIcon;
    property OnDrawLine: TATSynEditDrawLineEvent read FOnDrawLine write FOnDrawLine;

    //options
    property Colors: TATSynEditColors read FColors;
    property OptTabSpaces: boolean read FOptTabSpaces write FOptTabSpaces;
    property OptTabSize: integer read FTabSize write SetTabSize;
    property OptWordChars: atString read FOptWordChars write FOptWordChars;
    property OptAutoIndent: boolean read FOptAutoIndent write FOptAutoIndent;
    property OptAutoIndentKind: TATAutoIndentKind read FOptAutoIndentKind write FOptAutoIndentKind;
    property OptCopyLinesIfNoSel: boolean read FOptCopyLinesIfNoSel write FOptCopyLinesIfNoSel;
    property OptCutLinesIfNoSel: boolean read FOptCutLinesIfNoSel write FOptCutLinesIfNoSel;
    property OptLastLineOnTop: boolean read FOptLastLineOnTop write FOptLastLineOnTop;
    property OptOverwriteSel: boolean read FOptOverwriteSel write FOptOverwriteSel;
    property OptHiliteSelFull: boolean read FOptHiliteSelFull write FOptHiliteSelFull;
    property OptUseOverOnPaste: boolean read FOptUseOverOnPaste write FOptUseOverOnPaste;
    property OptShowCurLine: boolean read FOptShowCurLine write FOptShowCurLine;
    property OptShowCurColumn: boolean read FOptShowCurColumn write FOptShowCurColumn;
    property OptCaretManyAllowed: boolean read GetCaretManyAllowed write SetCaretManyAllowed;
    property OptCaretVirtual: boolean read FCaretVirtual write FCaretVirtual;
    property OptCaretShape: TATSynCaretShape read FCaretShape write SetCaretShape;
    property OptCaretShapeOvr: TATSynCaretShape read FCaretShapeOvr write SetCaretShapeOvr;
    property OptCaretTime: integer read GetCaretTime write SetCaretTime;
    property OptCaretStopUnfocused: boolean read FCaretStopUnfocused write FCaretStopUnfocused;
    property OptGutterVisible: boolean read FOptGutterVisible write FOptGutterVisible;
    property OptRulerVisible: boolean read FOptRulerVisible write FOptRulerVisible;
    property OptRulerSize: integer read FOptRulerSize write FOptRulerSize;
    property OptRulerFontSize: integer read FOptRulerFontSize write FOptRulerFontSize;
    property OptRulerMarkSizeSmall: integer read FOptRulerMarkSizeSmall write FOptRulerMarkSizeSmall;
    property OptRulerMarkSizeBig: integer read FOptRulerMarkSizeBig write FOptRulerMarkSizeBig;
    property OptMinimapVisible: boolean read FMinimapVisible write SetMinimapVisible;
    property OptMicromapVisible: boolean read FMicromapVisible write SetMicromapVisible;
    property OptCharSpacingX: integer read GetCharSpacingX write SetCharSpacingX;
    property OptCharSpacingY: integer read GetCharSpacingY write SetCharSpacingY;
    property OptWrapMode: TATSynWrapMode read FWrapMode write SetWrapMode;
    property OptWrapIndented: boolean read FWrapIndented write SetWrapIndented;
    property OptMarginRight: integer read FMarginRight write SetMarginRight;
    property OptMarginString: string read GetMarginString write SetMarginString;
    property OptNumbersStyle: TATSynNumbersStyle read FOptNumbersStyle write FOptNumbersStyle;
    property OptUnprintedVisible: boolean read FUnprintedVisible write FUnprintedVisible;
    property OptUnprintedSpaces: boolean read FUnprintedSpaces write FUnprintedSpaces;
    property OptUnprintedEnds: boolean read FUnprintedEnds write FUnprintedEnds;
    property OptUnprintedEndsDetails: boolean read FUnprintedEndsDetails write FUnprintedEndsDetails;
    property OptUnprintedReplaceSpec: boolean read FUnprintedReplaceSpec write FUnprintedReplaceSpec;
    property OptMouse2ClickSelectsLine: boolean read FOptMouse2ClickSelectsLine write FOptMouse2ClickSelectsLine;
    property OptMouse3ClickSelectsLine: boolean read FOptMouse3ClickSelectsLine write FOptMouse3ClickSelectsLine;
    property OptMouse2ClickDragSelectsWords: boolean read FOptMouse2ClickDragSelectsWords write FOptMouse2ClickDragSelectsWords;
    property OptMouseDragDrop: boolean read FOptMouseDragDrop write FOptMouseDragDrop;
    property OptMouseRightClickMovesCaret: boolean read FOptMouseRightClickMovesCaret write FOptMouseRightClickMovesCaret;
    property OptMouseGutterClickSelectsLine: boolean read FOptMouseGutterClickSelectsLine write FOptMouseGutterClickSelectsLine;
    property OptKeyNavigateWrapped: boolean read FOptKeyNavigateWrapped write FOptKeyNavigateWrapped;
    property OptKeyPageUpDownSize: TATPageUpDownSize read FOptKeyPageUpDownSize write FOptKeyPageUpDownSize;
    property OptKeyLeftRightSwapSel: boolean read FOptKeyLeftRightSwapSel write FOptKeyLeftRightSwapSel;
    property OptKeyHomeToNonSpace: boolean read FOptKeyHomeToNonSpace write FOptKeyHomeToNonSpace;
    property OptKeyEndToNonSpace: boolean read FOptKeyEndToNonSpace write FOptKeyEndToNonSpace;
    property OptKeyTabIndents: boolean read FOptKeyTabIndents write FOptKeyTabIndents;
    property OptIndentSize: integer read FOptIndentSize write FOptIndentSize;
    property OptIndentKeepsAlign: boolean read FOptIndentKeepsAlign write FOptIndentKeepsAlign;
    property OptShowIndentLines: boolean read FOptShowIndentLines write FOptShowIndentLines;
    property OptShowGutterCaretBG: boolean read FOptShowGutterCaretBG write FOptShowGutterCaretBG;
    property OptUndoLimit: integer read GetUndoLimit write SetUndoLimit;
    property OptUndoGrouped: boolean read FOptUndoGrouped write FOptUndoGrouped;
    property OptUndoAfterSave: boolean read GetUndoAfterSave write SetUndoAfterSave;
  end;

implementation

uses
  LCLIntf,
  LCLProc,
  Forms,
  Dialogs,
  Types,
  Math,
  Clipbrd,
  ATSynEdit_Commands,
  ATSynEdit_KeyMapping,
  ATStringProc_WordJump;

{$I atsynedit_proc.inc}


{ TATSynEdit }

procedure TATSynEdit.DoPaintRulerTo(C: TCanvas);
var
  NX, NSize, NPrevSize, NRulerStart, i: integer;
  Str: string;
begin
  NPrevSize:= C.Font.Size;
  NRulerStart:= FScrollHorz.NPos;

  C.Font.Size:= FOptRulerFontSize;
  C.Font.Color:= FColors.RulerFont;
  C.Pen.Color:= FColors.RulerFont;
  C.Brush.Color:= FColors.RulerBG;

  C.FillRect(FRectRuler);
  C.Line(FRectRuler.Left, FRectRuler.Bottom-1,
         FRectRuler.Right, FRectRuler.Bottom-1);

  for i:= NRulerStart to NRulerStart+GetVisibleColumns+1 do
  begin
    NX:= FTextOffset.X+(i-NRulerStart)*FCharSize.X;
    NSize:= IfThen(i mod 5 = 0, FOptRulerMarkSizeBig, FOptRulerMarkSizeSmall);
    C.Line(NX, FRectRuler.Bottom-1,
           NX, FRectRuler.Bottom-1-NSize);

    if i mod 10 = 0 then
    begin
      Str:= IntToStr(i);
      C.TextOut(NX - C.TextWidth(Str) div 2, 0, Str);
    end;
  end;

  C.Font.Size:= NPrevSize;
end;

procedure TATSynEdit.UpdateGutterAutosize;
begin
  FGutter[FGutterBandNum].Size:= GetGutterNumbersWidth;
  FGutter.Update;
end;

procedure TATSynEdit.UpdateMinimapAutosize(C: TCanvas);
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
  C.Font.Size:= FMinimapFontSize;
  CharSmall:= CanvasFontSizes(C).cx;
  C.Font.Size:= Font.Size;

  FMinimapWidth:= (ClientWidth - IfThen(FMicromapVisible, FMicromapWidth) - FTextOffset.X)*CharSmall div (CharSmall+CharBig);
  FMinimapWidth:= Max(cMinMinimapWidth, FMinimapWidth);
end;

function TATSynEdit.DoFormatLineNumber(N: integer): atString;
begin
  case FOptNumbersStyle of
    cNumbersAll:
      Result:= IntToStr(N);
    cNumbersNone:
      Result:= '.';
    cNumbersEach10th:
      begin
        if (N=1) or (N mod 10 = 0) then
          Result:= IntToStr(N)
        else
        if (N mod 5) = 0 then
          Result:= '-'
        else
          Result:= '.';
      end;
    cNumbersEach5th:
      begin
        if (N=1) or (N mod 5 = 0) then
          Result:= IntToStr(N)
        else
          Result:= '.';
      end;
    cNumbersForCarets:
      begin
        if IsLineWithCaret(N-1) then
          Result:= IntToStr(N)
        else
          Result:= '.';
      end;
    else
      raise Exception.Create('Unknown nums-style');
  end;
end;

function TATSynEdit.IsLineWithCaret(ALine: integer): boolean;
begin
  Result:= FCarets.IsLineListed(ALine);
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
  if FWrapMode=cWrapAtMargin then
    FWrapUpdateNeeded:= true;
  Update;
end;

procedure TATSynEdit.UpdateWrapInfo;
var
  NNewVisibleColumns: integer;
  NIndentMaximal: integer;
  Items: TList;
  ListNums: TList;
  i, j: integer;
  NLine, NIndexFrom, NIndexTo: integer;
  UseCachedUpdate: boolean;
begin
  NNewVisibleColumns:= GetVisibleColumns;
  NIndentMaximal:= Max(2, NNewVisibleColumns-cMinCharsAfterAnyIndent); //don't do too big NIndent

  if (not FWrapUpdateNeeded) and
    (FWrapMode=cWrapOn) and
    (FPrevVisibleColumns<>NNewVisibleColumns) then
    FWrapUpdateNeeded:= true;

  if not FWrapUpdateNeeded then Exit;
  FWrapUpdateNeeded:= false;
  FPrevVisibleColumns:= NNewVisibleColumns;

  {$ifdef beep_wrapinfo}
  Beep;
  {$endif}

  case FWrapMode of
    cWrapOff:
      FWrapColumn:= 0;
    cWrapOn:
      FWrapColumn:= Max(cMinWrapColumn, NNewVisibleColumns-cScrollKeepHorz);
    cWrapAtMargin:
      FWrapColumn:= Max(cMinWrapColumn, FMarginRight);
  end;

  UseCachedUpdate:=
    (FWrapInfo.Count>0) and
    (Strings.Count>cMaxLinesForOldWrapUpdate) and
    (not Strings.ListUpdatesHard) and
    (Strings.ListUpdates.Count>0);
  //UseCachedUpdate:= false;////to disable

  Items:= TList.Create;
  ListNums:= TList.Create;

  try
    if not UseCachedUpdate then
    begin
      FWrapInfo.Clear;
      FWrapInfo.SetCapacity(Strings.Count);
      for i:= 0 to Strings.Count-1 do
      begin
        DoCalcWrapInfos(i, NIndentMaximal, Items);
        for j:= 0 to Items.Count-1 do
          FWrapInfo.Add(TATSynWrapItem(Items[j]));
      end;
    end
    else
    begin
      {$ifdef beep_cached_update}
      Beep;
      {$endif}

      ListNums.Assign(Strings.ListUpdates);

      for i:= 0 to ListNums.Count-1 do
      begin
        NLine:= NativeInt{%H-}(ListNums[i]);
        DoCalcWrapInfos(NLine, NIndentMaximal, Items);
        if Items.Count=0 then Continue;

        DoFindWrapIndexesOfLineNumber(NLine, NIndexFrom, NIndexTo);
        if NIndexFrom<0 then
        begin
          Showmessage('Cant find wrap-index for line '+Inttostr(NLine));
          Continue;
        end;

        //slow for 100carets, 1M lines, so made method in which
        //we can optimize it (instead of del/ins do assign)
        FWrapInfo.ReplaceItems(NIndexFrom, NIndexTo, Items);
      end;
    end;
  finally
    FreeAndNil(ListNums);
    FreeAndNil(Items);
  end;

  Strings.ListUpdates.Clear;
  Strings.ListUpdatesHard:= false;

  {$ifdef debug_findwrapindex}
  DebugFindWrapIndex;
  {$endif}
end;


procedure TATSynEdit.DoFindWrapIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
begin
  FWrapInfo.FindIndexesOfLineNumber(ALineNum, AFrom, ATo);
end;

procedure TATSynEdit.DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer; AItems: TList);
var
  NHiddenIndex, NOffset, NLen, NIndent: integer;
  NFinal: TATSynWrapFinal;
  Str: atString;
begin
  AItems.Clear;

  NHiddenIndex:= Strings.LinesHidden[ALine];
  if NHiddenIndex<0 then Exit;

  Str:= Strings.Lines[ALine];
  NLen:= Length(Str);

  //line collapsed partially?
  if NHiddenIndex>0 then
  begin
    AItems.Add(TATSynWrapItem.Create(ALine, 1, Min(NLen, NHiddenIndex-1), 0, cWrapItemCollapsed));
    Exit;
  end;

  //wrap not needed?
  if (FWrapColumn<cMinWrapColumnAbs) then
  begin
    AItems.Add(TATSynWrapItem.Create(ALine, 1, NLen, 0, cWrapItemFinal));
    Exit;
  end;

  NOffset:= 1;
  NIndent:= 0;

  repeat
    NLen:= SFindWordWrapOffset(Str, Max(FWrapColumn-NIndent, cMinWrapColumnAbs), FTabSize, FOptWordChars);
    if NLen>=Length(Str) then
      NFinal:= cWrapItemFinal
    else
      NFinal:= cWrapItemMiddle;
    AItems.Add(TATSynWrapItem.Create(ALine, NOffset, NLen, NIndent, NFinal));

    if FWrapIndented then
      if NOffset=1 then
      begin
        NIndent:= SGetIndentExpanded(Str, FTabSize);
        NIndent:= Min(NIndent, AIndentMaximal);
      end;

    Inc(NOffset, NLen);
    Delete(Str, 1, NLen);
  until Str='';
end;

procedure TATSynEdit.DebugFindWrapIndex;
var
  i, j, n1, n2: integer;
begin
  for i:= 0 to Strings.Count-1 do
  begin
    FWrapInfo.FindIndexesOfLineNumber(i, n1, n2);
    if n1<0 then
    begin
      Application.MainForm.caption:= format('fail findindex: %d', [i]);
      Exit
    end;
    for j:= n1 to n2 do
      if FWrapInfo.Items[j].NLineIndex<>i then
      begin
        Application.MainForm.caption:= format('fail findindex: %d', [i]);
        Exit
      end;
  end;
  Application.MainForm.caption:= 'ok findindex';
end;

function TATSynEdit.GetVisibleLines: integer;
begin
  Result:= (FRectMain.Bottom-FRectMain.Top) div FCharSize.Y;
end;

function TATSynEdit.GetVisibleColumns: integer;
begin
  Result:= (FRectMain.Right-FRectMain.Left) div FCharSize.X;
end;

function TATSynEdit.GetVisibleLinesMinimap: integer;
begin
  Result:= (FRectMinimap.Bottom-FRectMinimap.Top) div FCharSizeMinimap.Y - 1;
end;

function TATSynEdit.GetMinimapScrollPos: integer;
begin
  Result:=
    Int64(FScrollVert.NPos) *
    Max(0, FScrollVert.NMax-GetVisibleLinesMinimap) div
    Max(1, FScrollVert.NMax-FScrollVert.NPage);
end;

procedure TATSynEdit.SetTabSize(AValue: integer);
begin
  if FTabSize=AValue then Exit;
  FTabSize:= Min(cMaxTabSize, Max(cMinTabSize, AValue));
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetWrapMode(AValue: TATSynWrapMode);
begin
  if FWrapMode=AValue then Exit;
  FWrapMode:= AValue;
  FWrapUpdateNeeded:= true;

  if FWrapMode<>cWrapOff then
    FScrollHorz.NPos:= 0;
end;

procedure TATSynEdit.SetWrapIndented(AValue: boolean);
begin
  if FWrapIndented=AValue then Exit;
  FWrapIndented:=AValue;
  if FWrapMode<>cWrapOff then
    FWrapUpdateNeeded:= true;
end;

function TATSynEdit.UpdateScrollbars: boolean;
var
  bVert1, bVert2,
  bHorz1, bHorz2: boolean;
begin
  Result:= false;

  with FScrollVert do
  begin
    NPage:= Max(1, GetVisibleLines)-1;
    NMin:= 0;
    if FOptLastLineOnTop then
      NMax:= Max(1, FWrapInfo.Count+NPage-1)
    else
      NMax:= Max(1, FWrapInfo.Count-1);
  end;

  with FScrollHorz do
  begin
    NPage:= Max(1, GetVisibleColumns);
    NMin:= 0;
    //NMax calculated in DoPaintTextTo
    //hide horz bar for word-wrap:
    if FWrapMode=cWrapOn then
      NMax:= NPage;
  end;

  bVert1:= GetScrollbarVisible(true);
  bHorz1:= GetScrollbarVisible(false);
  UpdateScrollbarVert;
  UpdateScrollbarHorz;
  bVert2:= GetScrollbarVisible(true);
  bHorz2:= GetScrollbarVisible(false);
  Result:= (bVert1<>bVert2) or (bHorz1<>bHorz2);

  if not IsEqualScrollInfo(FPrevHorz, FScrollHorz) or
    not IsEqualScrollInfo(FPrevVert, FScrollVert) then
  begin
    FPrevHorz:= FScrollHorz;
    FPrevVert:= FScrollVert;
    DoEventScroll;
  end;
end;

procedure TATSynEdit.UpdateScrollbarVert;
var
  si: TScrollInfo;
begin
  FillChar(si{%H-}, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;// or SIF_DISABLENOSCROLL; //todo -- DisableNoScroll doesnt work(Win)
  si.nMin:= FScrollVert.NMin;
  si.nMax:= FScrollVert.NMax;
  si.nPage:= FScrollVert.NPage;
  si.nPos:= FScrollVert.NPos;
  SetScrollInfo(Handle, SB_VERT, si, True);
end;

procedure TATSynEdit.UpdateScrollbarHorz;
var
  si: TScrollInfo;
begin
  FillChar(si{%H-}, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;// or SIF_DISABLENOSCROLL;
  si.nMin:= FScrollHorz.NMin;
  si.nMax:= FScrollHorz.NMax;
  si.nPage:= FScrollHorz.NPage;
  si.nPos:= FScrollHorz.NPos;
  SetScrollInfo(Handle, SB_HORZ, si, True);
end;

function TATSynEdit.GetRectMain: TRect;
begin
  Result.Left:= FTextOffset.X;
  Result.Top:= FTextOffset.Y;
  Result.Right:= ClientWidth
    - IfThen(FMinimapVisible, FMinimapWidth)
    - IfThen(FMicromapVisible, FMicromapWidth);
  Result.Bottom:= ClientHeight;
end;

function TATSynEdit.GetRectMinimap: TRect;
begin
  if FMinimapVisible then
  begin
    Result.Left:= ClientWidth-FMinimapWidth-IfThen(FMicromapVisible, FMicromapWidth);
    Result.Top:= 0;
    Result.Right:= Result.Left+FMinimapWidth;
    Result.Bottom:= ClientHeight;
  end
  else
    Result:= cRectEmpty;
end;

function TATSynEdit.GetRectMicromap: TRect;
begin
  if FMicromapVisible then
  begin
    Result.Left:= ClientWidth-FMicromapWidth;
    Result.Top:= 0;
    Result.Right:= ClientWidth;
    Result.Bottom:= ClientHeight;
  end
  else
    Result:= cRectEmpty;
end;

function TATSynEdit.GetRectGutter: TRect;
begin
  if FOptGutterVisible then
  begin
    Result.Left:= 0;
    Result.Top:= IfThen(FOptRulerVisible, FOptRulerSize);
    Result.Right:= FGutter.Width;
    Result.Bottom:= ClientHeight;
  end
  else
    Result:= cRectEmpty;
end;

function TATSynEdit.GetRectRuler: TRect;
begin
  if FOptRulerVisible then
  begin
    Result.Left:= 0;
    Result.Right:= FRectMain.Right;
    Result.Top:= 0;
    Result.Bottom:= Result.Top+FOptRulerSize;
  end
  else
    Result:= cRectEmpty;
end;

procedure TATSynEdit.DoPaintTo(C: TCanvas);
begin
  C.Brush.Color:= FColors.TextBG;
  C.FillRect(ClientRect);

  C.Font.Assign(Font);
  FCharSize:= GetCharSize(C, FCharSpacingText);
  FCharSizeMinimap:= Point(8, 8);

  if FOptGutterVisible then UpdateGutterAutosize;
  if FMinimapVisible then UpdateMinimapAutosize(C);

  FTextOffset:= GetTextOffset; //after gutter autosize
  FRectMinimap:= GetRectMinimap;
  FRectMicromap:= GetRectMicromap;
  FRectGutter:= GetRectGutter;
  FRectMain:= GetRectMain; //after gutter/minimap
  FRectRuler:= GetRectRuler; //after main

  UpdateWrapInfo;

  if FOptRulerVisible then
    DoPaintRulerTo(C);
  DoPaintTextTo(C, FRectMain, FCharSize, FOptGutterVisible, FUnprintedVisible, FScrollHorz, FScrollVert);
  DoPaintMarginsTo(C);

  if FMinimapVisible then
    DoPaintMinimapTo(C);
  if FMicromapVisible then
    DoPaintMicromapTo(C);
end;

function TATSynEdit.GetCharSize(C: TCanvas; ACharSpacing: TPoint): TPoint;
var
  Size: TSize;
begin
  Size:= CanvasFontSizes(C);
  Result.X:= Max(1, Size.cx + ACharSpacing.X);
  Result.Y:= Max(1, Size.cy + ACharSpacing.Y);
end;

procedure TATSynEdit.DoPaintTextTo(C: TCanvas;
  const ARect: TRect;
  const ACharSize: TPoint;
  AWithGutter, AWithUnprintable: boolean;
  var AScrollHorz, AScrollVert: TATSynScrollInfo);
var
  NGutterBmX1, NGutterBmX2,
  NGutterNumsX1, NGutterNumsX2,
  //NGutterFoldX1, NGutterFoldX2,
  NGutterEmptyX1, NGutterEmptyX2,
  NGutterStateX1, NGutterStateX2,
  NCoordTop, NCoordLeftNums: integer;
  NWrapIndex, NLinesIndex: integer;
  NOutputCharsSkipped, NOutputStrWidth: integer;
  NOutputSpacesSkipped: real;
  WrapItem: TATSynWrapItem;
  BmKind: integer;
  BmColor: TColor;
  Str, StrOut, StrOutUncut: atString;
  CurrPoint, CurrPointText: TPoint;
  LineWithCaret, LineEolSelected: boolean;
  Parts: TATLineParts;
  Event: TATSynEditDrawLineEvent;
  i: integer;
  //
  procedure DoPaintState(ATop: integer; AColor: TColor);
  begin
    C.Brush.Color:= AColor;
    C.FillRect(NGutterStateX1, ATop, NGutterStateX2, ATop+ACharSize.Y);
  end;
  //
begin
  C.Brush.Color:= FColors.TextBG;
  C.FillRect(ARect);

  if AWithGutter then
  begin
    with FGutter[FGutterBandBm] do begin NGutterBmX1:= Left; NGutterBmX2:= Right; end;
    with FGutter[FGutterBandNum] do begin NGutterNumsX1:= Left; NGutterNumsX2:= Right; end;
    with FGutter[FGutterBandState] do begin NGutterStateX1:= Left; NGutterStateX2:= Right; end;
    //with FGutter[FGutterBandFold] do begin NGutterFoldX1:= Left; NGutterFoldX2:= Right; end;
    with FGutter[FGutterBandEmpty] do begin NGutterEmptyX1:= Left; NGutterEmptyX2:= Right; end;

    C.Brush.Color:= FColors.GutterBG;
    C.FillRect(FRectGutter);

    if FGutter[FGutterBandEmpty].Visible then
    begin
      C.Brush.Color:= FColors.TextBG;
      C.FillRect(NGutterEmptyX1, FRectGutter.Top, NGutterEmptyX2, FRectGutter.Bottom);
      C.Brush.Color:= FColors.GutterBG;
    end;
  end;

  NCoordTop:= ARect.Top;
  NWrapIndex:= AScrollVert.NPos;
  AScrollHorz.NMax:= 1;

  repeat
    if NCoordTop>ARect.Bottom then Break;
    if not FWrapInfo.IsIndexValid(NWrapIndex) then Break;

    WrapItem:= FWrapInfo.Items[NWrapIndex];
    NLinesIndex:= WrapItem.NLineIndex;
    if not Strings.IsIndexValid(NLinesIndex) then Break;

    if FWrapInfo.IsItemAfterCollapsed(NWrapIndex) then
    begin
      C.Pen.Color:= FColors.CollapseLine;
      C.Line(ARect.Left, NCoordTop-1, ARect.Right, NCoordTop-1);
    end;

    //prepare line
    Str:= Strings.Lines[NLinesIndex];
    Str:= Copy(Str, WrapItem.NCharIndex, WrapItem.NLength);

    LineWithCaret:= IsLineWithCaret(NLinesIndex);
    LineEolSelected:= IsPosSelected(WrapItem.NCharIndex-1+WrapItem.NLength, WrapItem.NLineIndex);

    StrOut:= Str;
    StrOutUncut:= StrOut;
    AScrollHorz.NMax:= Max(AScrollHorz.NMax,
      Round(CanvasTextSpaces(StrOutUncut, FTabSize)) + cScrollKeepHorz);

    CurrPoint.X:= ARect.Left;
    CurrPoint.Y:= NCoordTop;

    C.Brush.Color:= FColors.TextBG;
    C.Font.Color:= FColors.TextFont;

    //draw bookmark bg
    BmColor:= clNone;
    BmKind:= Strings.LinesBm[NLinesIndex];
    if BmKind<>0 then
      BmColor:= Strings.LinesBmColor[NLinesIndex];
    if FOptShowCurLine and LineWithCaret then
      BmColor:= FColors.CurLineBG;

    if BmColor<>clNone then
    begin
      C.Brush.Color:= BmColor;
      C.FillRect(ARect.Left, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y);
    end;

    if AWithUnprintable then
      if FOptShowIndentLines then
      begin
        for i:= 0 to WrapItem.NIndent-1 do
          if i mod FTabSize = 0 then
            CanvasDottedVertLine(C, ARect.Left + (i-AScrollHorz.NPos)*ACharSize.X, NCoordTop, NCoordTop+ACharSize.Y, FColors.IndentLines);
      end;

    CurrPointText:= Point(
      Int64(CurrPoint.X) + (Int64(WrapItem.NIndent)-AScrollHorz.NPos)*ACharSize.X,
      CurrPoint.Y);
    NOutputStrWidth:= 0;

    //draw line
    if StrOut<>'' then
    begin
      NOutputCharsSkipped:= 0;
      NOutputSpacesSkipped:= 0;
      if FWrapInfo.IsItemInitial(NWrapIndex) then
      begin
        SFindOutputSkipOffset(StrOut, FTabSize, AScrollHorz.NPos, NOutputCharsSkipped, NOutputSpacesSkipped);
        Delete(StrOut, 1, NOutputCharsSkipped);
        Delete(StrOut, cMaxCharsForOutput, MaxInt);
        Inc(CurrPointText.X, Trunc(NOutputSpacesSkipped*ACharSize.X));
      end;

      DoPaintSelectedLineBG(C, ACharSize, ARect,
        CurrPoint,
        CurrPointText,
        NLinesIndex,
        LineEolSelected,
        AScrollHorz);

      DoCalcLineHilite(WrapItem, Parts{%H-},
        NOutputCharsSkipped, cMaxCharsForOutput,
        IfThen(BmColor<>clNone, BmColor, FColors.TextBG));

      if AWithGutter then
        Event:= FOnDrawLine
      else
        Event:= nil;

      CanvasTextOut(C,
        CurrPointText.X,
        CurrPointText.Y,
        StrOut,
        FTabSize,
        ACharSize,
        FUnprintedReplaceSpec,
        AWithUnprintable and FUnprintedSpaces,
        FColors.UnprintedFont,
        FColors.UnprintedHexFont,
        NOutputStrWidth,
        Trunc(NOutputSpacesSkipped), //todo:
          //needed number of chars of all chars counted as 1.0,
          //while NOutputSpacesSkipped is with cjk counted as 1.7
        @Parts,
        Event
        );
    end
    else
    begin
      DoPaintSelectedLineBG(C, ACharSize, ARect,
        CurrPoint,
        CurrPointText,
        NLinesIndex,
        LineEolSelected,
        AScrollHorz);
    end;

    if WrapItem.NFinal=cWrapItemFinal then
    begin
      //for OptHiliteSelFull=false paint eol bg
      if LineEolSelected then
      begin
        C.Brush.Color:= FColors.TextSelBG;
        C.FillRect(
          CurrPointText.X+NOutputStrWidth,
          CurrPointText.Y,
          CurrPointText.X+NOutputStrWidth+ACharSize.X,
          CurrPointText.Y+ACharSize.Y);
      end;

      //paint eol mark
      if AWithUnprintable then
        if FUnprintedEnds then
          DoPaintUnprintedEol(C,
            cLineEndNiceNames[Strings.LinesEnds[WrapItem.NLineIndex]],
            Point(
              CurrPointText.X+NOutputStrWidth,
              CurrPointText.Y),
            ACharSize,
            FColors.UnprintedFont,
            FColors.UnprintedBG,
            FUnprintedEndsDetails);
    end;

    //draw collapsed-mark
    if WrapItem.NFinal=cWrapItemCollapsed then
      if AWithUnprintable then
        DoPaintCollapseMark(C, Str, CurrPoint, AScrollHorz.NPos);

    //draw gutter
    if AWithGutter then
    begin
      C.Brush.Color:= FColors.GutterBG;
      C.FillRect(FRectGutter.Left, NCoordTop, FRectGutter.Right, NCoordTop+ACharSize.Y);

      //gutter band: number
      if FGutter[FGutterBandNum].Visible then
      begin
        if LineWithCaret and FOptShowGutterCaretBG then
        begin
          C.Brush.Color:= FColors.GutterCaretBG;
          C.FillRect(NGutterNumsX1, NCoordTop, NGutterNumsX2, NCoordTop+ACharSize.Y);
        end;

        if FWrapInfo.IsItemInitial(NWrapIndex) then
        begin
          C.Font.Color:= FColors.GutterFont;
          C.FillRect(NGutterNumsX1, NCoordTop, NGutterNumsX2, NCoordTop+ACharSize.Y);

          Str:= DoFormatLineNumber(NLinesIndex+1);
          NCoordLeftNums:= NGutterNumsX2 - C.TextWidth(Str) - cSizeGutterNumOffsetRight;
          C.TextOut(NCoordLeftNums, NCoordTop, Str);
        end;
      end;

      //gutter band: bookmark
      if FGutter[FGutterBandBm].Visible then
        if FWrapInfo.IsItemInitial(NWrapIndex) then
        begin
          if Strings.LinesBm[NLinesIndex]<>0 then
            DoEventDrawBookmarkIcon(C, NLinesIndex, Rect(NGutterBmX1, NCoordTop, NGutterBmX2, NCoordTop+ACharSize.Y));
        end;

      //gutter band: state
      if FGutter[FGutterBandState].Visible then
      begin
        case Strings.LinesState[WrapItem.NLineIndex] of
          cLineStateChanged: DoPaintState(NCoordTop, FColors.StateChanged);
          cLineStateAdded: DoPaintState(NCoordTop, FColors.StateAdded);
          cLineStateSaved: DoPaintState(NCoordTop, FColors.StateSaved);
        end;
      end;

      //gutter band: empty indent
      if FGutter[FGutterBandEmpty].Visible then
      begin
        C.Brush.Color:= FColors.TextBG;
        C.FillRect(NGutterEmptyX1, NCoordTop, NGutterEmptyX2, NCoordTop+ACharSize.Y);
      end;
    end;

    //end of painting line
    Inc(NCoordTop, ACharSize.Y);
    Inc(NWrapIndex);
  until false;
end;

procedure TATSynEdit.DoPaintMinimapSelTo(C: TCanvas);
var
  R: TRect;
begin
  R.Left:= FRectMinimap.Left;
  R.Right:= FRectMinimap.Right;
  R.Top:= FRectMinimap.Top + (FScrollVert.NPos-FScrollVertMinimap.NPos)*FCharSizeMinimap.Y;
  R.Bottom:= R.Top + (FScrollVert.NPage+1)*FCharSizeMinimap.Y;

  if IntersectRect(R, R, FRectMinimap) then
    CanvasInvertRect(C, R, FColors.MinimapSelBG);
end;

procedure TATSynEdit.DoPaintMinimapTo(C: TCanvas);
begin
  DoClearScrollInfo(FScrollHorzMinimap);
  DoClearScrollInfo(FScrollVertMinimap);

  C.Font.Size:= FMinimapFontSize;
  FCharSizeMinimap:= GetCharSize(C, FCharSpacingMinimap);
  FScrollVertMinimap.NPos:= GetMinimapScrollPos;
  DoPaintTextTo(C, FRectMinimap, FCharSizeMinimap, false, false, FScrollHorzMinimap, FScrollVertMinimap);
  C.Font.Size:= Font.Size;

  DoPaintMinimapSelTo(C);

  if FColors.MinimapBorder<>clNone then
  begin
    C.Pen.Color:= FColors.MinimapBorder;
    C.Line(FRectMinimap.Left-1, FRectMinimap.Top,
           FRectMinimap.Left-1, FRectMinimap.Bottom);
  end;
end;

procedure TATSynEdit.DoPaintMicromapTo(C: TCanvas);
begin
  C.Brush.Color:= FColors.MicromapBG;
  C.FillRect(FRectMicromap);
end;

procedure TATSynEdit.DoPaintMarginLineTo(C: TCanvas; AX: integer; AColor: TColor);
begin
  if (AX>=FRectMain.Left) and (AX<FRectMain.Right) then
  begin
    C.Pen.Color:= AColor;
    C.Line(AX, FRectMain.Top,
           AX, FRectMain.Bottom);
  end;
end;

procedure TATSynEdit.DoPaintMarginsTo(C: TCanvas);
  //
  function PosX(NMargin: integer): integer;
  begin
    Result:= FRectMain.Left + FCharSize.X*(NMargin-FScrollHorz.NPos);
  end;
var
  i: integer;
begin
  if FMarginRight>1 then
    DoPaintMarginLineTo(C, PosX(FMarginRight), FColors.MarginRight);
  for i:= 0 to FMarginList.Count-1 do
    DoPaintMarginLineTo(C, PosX(NativeInt{%H-}(FMarginList[i])), FColors.MarginUser);
end;


procedure TATSynEdit.DoPaintCollapseMark(C: TCanvas; const Str: atString;
  ALineStart: TPoint; AScrollPos: integer);
var
  SMark: atString;
  NSize: integer;
begin
  Dec(ALineStart.X, FCharSize.X*AScrollPos);
  Inc(ALineStart.X, CanvasTextWidth(Str, FTabSize, FCharSize));
  Inc(ALineStart.X, cCollapseMarkIndent);

  //paint text
  C.Font.Color:= FColors.CollapseFont;
  C.Brush.Color:= FColors.TextBG;

  SMark:= '...';
  C.TextOut(
    ALineStart.X+cCollapseMarkIndent,
    ALineStart.Y,
    SMark);
  NSize:= C.TextWidth(SMark) + 2*cCollapseMarkIndent;

  //paint frame
  C.Pen.Color:= FColors.CollapseFont;
  C.Brush.Style:= bsClear;
  C.Rectangle(ALineStart.X, ALineStart.Y, ALineStart.X+NSize, ALineStart.Y+FCharSize.Y);
end;

function TATSynEdit.GetCharSpacingX: integer;
begin
  Result:= FCharSpacingText.X;
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
  for i:= 0 to FMarginList.Count-1 do
    Result:= Result+IntToStr(NativeInt{%H-}(FMarginList[i]))+' ';
end;

function TATSynEdit.GetReadOnly: boolean;
begin
  Result:= Strings.ReadOnly;
end;

function TATSynEdit.GetScrollTop: integer;
var
  N: integer;
begin
  Result:= 0;
  N:= FScrollVert.NPos;
  if not FWrapInfo.IsIndexValid(N) then Exit;
  Result:= FWrapInfo.Items[N].NLineIndex;
end;

constructor TATSynEdit.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;

  Caption:= '';
  ControlStyle:= ControlStyle+[csOpaque, csDoubleClicks, csTripleClicks];
  BorderStyle:= bsNone;
  TabStop:= true;

  Width:= 300;
  Height:= 200;
  Font.Name:= 'Courier New';
  Font.Size:= {$ifndef darwin} 9 {$else} 14 {$endif};

  FCarets:= TATCarets.Create;
  FCarets.Add(0, 0);
  FCaretShown:= false;
  FCaretShape:= cInitCaretShape;
  FCaretShapeOvr:= cCaretShapeFull;
  FCaretVirtual:= true;
  FCaretSpecPos:= false;
  FCaretStopUnfocused:= true;

  FPaintLocked:= 0;
  FPaintStatic:= false;
  FPaintFlags:= [cPaintUpdateBitmap, cPaintUpdateScrollbars];

  FColors:= TATSynEditColors.Create;
  InitColors;

  FTimerBlink:= TTimer.Create(Self);
  FTimerBlink.Interval:= cInitTimerBlink;
  FTimerBlink.OnTimer:= TimerBlinkTick;
  FTimerBlink.Enabled:= true;

  FTimerScroll:= TTimer.Create(Self);
  FTimerScroll.Interval:= cInitTimerScroll;
  FTimerScroll.OnTimer:= TimerScrollTick;
  FTimerScroll.Enabled:= false;

  FBitmap:= Graphics.TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= cInitBitmapWidth;
  FBitmap.Height:= cInitBitmapHeight;

  FStringsExt:= nil;
  FStringsInt:= TATStrings.Create;
  FStringsInt.OnGetCaretsArray:= GetCaretsArray;
  FStringsInt.OnSetCaretsArray:= SetCaretsArray;

  FWrapInfo:= TATSynWrapInfo.Create;
  FWrapUpdateNeeded:= true;
  FWrapMode:= cWrapOn;
  FWrapColumn:= cInitMarginRight;
  FWrapIndented:= true;

  FOver:= false;
  FTabSize:= cInitTabSize;
  FMarginRight:= cInitMarginRight;
  FMarginList:= TList.Create;

  FUnprintedVisible:= true;
  FUnprintedSpaces:= true;
  FUnprintedEnds:= true;
  FUnprintedEndsDetails:= true;
  FUnprintedReplaceSpec:= true;

  FGutter:= TATGutter.Create;
  FOptGutterVisible:= true;

  FGutterBandBm:= 0;
  FGutterBandNum:= 1;
  FGutterBandFold:= 2;
  FGutterBandState:= 3;
  FGutterBandEmpty:= 4;

  for i:= 1 to cGutterBands do FGutter.Add(10);
  FGutter[FGutterBandBm].Size:= cSizeGutterBandBm;
  FGutter[FGutterBandNum].Size:= cSizeGutterBandNum;
  FGutter[FGutterBandState].Size:= cSizeGutterBandState;
  FGutter[FGutterBandFold].Size:= cSizeGutterBandFold;
  FGutter[FGutterBandEmpty].Size:= cSizeGutterBandEmpty;
  FGutter.Update;

  FOptNumbersStyle:= cInitNumbersStyle;
  FOptRulerSize:= cSizeRulerHeight;
  FOptRulerMarkSizeSmall:= cSizeRulerMarkSmall;
  FOptRulerMarkSizeBig:= cSizeRulerMarkBig;
  FOptRulerFontSize:= 8;
  FOptRulerVisible:= true;

  FMinimapWidth:= cInitMinimapWidth;
  FMinimapFontSize:= cInitMinimapFontSize;
  FMinimapVisible:= cInitMinimapVisible;
  FMicromapWidth:= cInitMicromapWidth;
  FMicromapVisible:= cInitMicromapVisible;

  FCharSpacingText:= Point(0, cInitSpacingText);
  FCharSpacingMinimap:= Point(0, cInitSpacingMinimap);

  FOptKeyNavigateWrapped:= true;
  FOptUseOverOnPaste:= false;
  FOptWordChars:= '';
  FOptAutoIndent:= true;
  FOptAutoIndentKind:= cIndentAsIs;
  FOptTabSpaces:= false;
  FOptLastLineOnTop:= false;
  FOptOverwriteSel:= true;
  FOptMouseDragDrop:= true;
  FOptMouse2ClickSelectsLine:= false;
  FOptMouse3ClickSelectsLine:= true;
  FOptMouse2ClickDragSelectsWords:= true;
  FOptMouseRightClickMovesCaret:= false;
  FOptMouseGutterClickSelectsLine:= true;
  FOptCopyLinesIfNoSel:= true;
  FOptCutLinesIfNoSel:= false;
  FOptHiliteSelFull:= false;
  FOptShowCurLine:= false;
  FOptShowCurColumn:= false;
  FOptKeyPageUpDownSize:= cPageSizeFullMinus1;
  FOptKeyLeftRightSwapSel:= true;
  FOptKeyHomeToNonSpace:= true;
  FOptKeyEndToNonSpace:= true;
  FOptKeyTabIndents:= true;
  FOptShowIndentLines:= true;
  FOptShowGutterCaretBG:= true;
  FOptIndentSize:= 2;
  FOptIndentKeepsAlign:= true;
  FOptUndoGrouped:= true;

  FMouseDownPnt:= Point(-1, -1);
  FMouseDownNumber:= -1;
  FMouseDownDouble:= false;
  FMouseDragging:= false;
  FSelRect:= cRectEmpty;

  DoClearScrollInfo(FScrollHorz);
  DoClearScrollInfo(FScrollVert);

  FKeyMapping:= TATKeyMapping.Create;
  FMenu:= TPopupMenu.Create(Self);

  DoInitDefaultKeymapping(FKeyMapping);
  DoInitDefaultPopupMenu;

  //UpdateCaretsCoords;
  //DoPaintModeBlinking;
end;

destructor TATSynEdit.Destroy;
begin
  DoPaintModeStatic;
  FreeAndNil(FTimerScroll);
  FreeAndNil(FTimerBlink);
  FreeAndNil(FKeyMapping);
  FreeAndNil(FCarets);
  FreeAndNil(FGutter);
  FreeAndNil(FMarginList);
  FreeAndNil(FWrapInfo);
  FreeAndNil(FStringsInt);
  FreeAndNil(FBitmap);
  FreeAndNil(FColors);
  inherited;
end;

procedure TATSynEdit.Update(
  AUpdateWrapInfo: boolean = false;
  AUpdateCaretsCoords: boolean = true);
begin
  UpdateCursor;
  if AUpdateWrapInfo then
    FWrapUpdateNeeded:= true;
  Include(FPaintFlags, cPaintUpdateScrollbars);
  if AUpdateCaretsCoords then
    Include(FPaintFlags, cPaintUpdateCaretsCoords);
  Invalidate;
end;

procedure TATSynEdit.SetFocus;
begin
  LCLIntf.SetFocus(Handle);
end;

procedure TATSynEdit.LoadFromFile(const AFilename: string);
begin
  DoPaintModeStatic;

  FCarets.Clear;
  FCarets.Add(0, 0);

  Strings.Clear;
  FWrapInfo.Clear;
  FWrapUpdateNeeded:= true;

  DoClearScrollInfo(FScrollHorz);
  DoClearScrollInfo(FScrollVert);

  BeginUpdate;
  try
    Strings.LoadFromFile(AFilename);
  finally
    EndUpdate;
  end;

  Update;
  DoPaintModeBlinking;
  DoEventChange;
  DoEventCarets;
end;

procedure TATSynEdit.SaveToFile(const AFilename: string);
begin
  Strings.SaveToFile(AFilename);
end;

procedure TATSynEdit.DoFoldLines(ALineFrom, ALineTo, ACharPosFrom: integer; AFold: boolean);
var
  i: integer;
begin
  if AFold then
  begin
    Strings.LinesHidden[ALineFrom]:= ACharPosFrom;
    for i:= ALineFrom+1 to ALineTo do
      Strings.LinesHidden[i]:= -1;
  end
  else
  begin
    for i:= ALineFrom to ALineTo do
      Strings.LinesHidden[i]:= 0;
  end;

  FWrapUpdateNeeded:= true;
end;

function TATSynEdit.GetStrings: TATStrings;
begin
  if Assigned(FStringsExt) then
    Result:= FStringsExt
  else
    Result:= FStringsInt;
end;

procedure TATSynEdit.SetCaretTime(AValue: integer);
begin
  AValue:= Max(AValue, cMinCaretTime);
  AValue:= Min(AValue, cMaxCaretTime);
  FTimerBlink.Interval:= AValue;
end;

procedure TATSynEdit.SetCharSpacingX(AValue: integer);
begin
  if FCharSpacingText.X=AValue then Exit;
  FCharSpacingText.X:= AValue;
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetCharSpacingY(AValue: integer);
begin
  if FCharSpacingText.Y=AValue then Exit;
  FCharSpacingText.Y:= AValue;
  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.SetMarginString(AValue: string);
var
  S: string;
  N: integer;
begin
  FMarginList.Clear;
  repeat
    S:= SGetItem(AValue, ' ');
    if S='' then Break;
    N:= StrToIntDef(S, 0);
    if N<2 then Continue;
    FMarginList.Add(pointer{%H-}(N));
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

procedure TATSynEdit.SetOver(AValue: boolean);
begin
  if FOver= AValue then Exit;
  FOver:= AValue;
end;

procedure TATSynEdit.SetReadOnly(AValue: boolean);
begin
  Strings.ReadOnly:= AValue;
end;

procedure TATSynEdit.SetScrollTop(AValue: integer);
var
  NFrom, NTo, i: integer;
begin
  //find exact match
  DoFindWrapIndexesOfLineNumber(AValue, NFrom, NTo);
  if NFrom>=0 then
  begin
    FScrollVert.NPos:= NFrom;
    Exit
  end;

  //find approx match
  for i:= 0 to FWrapInfo.Count-1 do
    with FWrapInfo.Items[i] do
      if NLineIndex>=AValue then
      begin
        FScrollVert.NPos:= i;
        Exit
      end;
end;

procedure TATSynEdit.SetStrings(Obj: TATStrings);
begin
  FStringsExt:= Obj;
end;

function TATSynEdit.GetTextOffset: TPoint;
begin
  Result.X:= 0;
  if FOptGutterVisible then
    Inc(Result.X, FGutter.Width);

  Result.Y:= cOffsetTextTop;
  if FOptRulerVisible then
    Inc(Result.Y, FOptRulerSize);
end;

function TATSynEdit.GetGutterNumbersWidth: integer;
var
  Str: atString;
begin
  Str:= IntToStr(Max(10, Strings.Count));
  Result:=
    Length(Str)*FCharSize.X+
    cSizeGutterNumOffsetLeft+
    cSizeGutterNumOffsetRight;
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

function TATSynEdit.DoPaint(AFlags: TATSynPaintFlags): boolean;
var
  ARect: TRect;
begin
  Result:= false;
  if not Assigned(FBitmap) then Exit;

  if cPaintUpdateBitmap in AFlags then
  begin
    DoPaintTo(FBitmap.Canvas);

    if cPaintUpdateCaretsCoords in AFlags then
    begin
      UpdateCaretsCoords;
      if FOptShowCurColumn and (Carets.Count>0) then
        DoPaintMarginLineTo(FBitmap.Canvas, Carets[0].CoordX, FColors.MarginCaret);
    end;

    DoPaintCarets(FBitmap.Canvas, false);
  end;

  ARect:= Canvas.ClipRect;
  Canvas.CopyRect(ARect, FBitmap.Canvas, ARect);

  if cPaintUpdateScrollbars in AFlags then
    Result:= UpdateScrollbars;
end;


procedure TATSynEdit.Paint;
begin
  if FPaintLocked>0 then
  begin
    Canvas.Brush.Color:= FColors.LockedBG;
    Canvas.FillRect(ClientRect);
    Canvas.Font.Assign(Self.Font);
    Canvas.TextOut(4, 4, '...');
    Exit;
  end;

  //if scrollbars shown, paint again
  if DoPaint(FPaintFlags) then
    DoPaint(FPaintFlags);

  Exclude(FPaintFlags, cPaintUpdateBitmap);
end;

procedure TATSynEdit.DoOnResize;
begin
  inherited;

  if Assigned(FBitmap) then
  begin
    FBitmap.Width:= Max(FBitmap.Width, ((Width div cResizeBitmapStep)+1)*cResizeBitmapStep);
    FBitmap.Height:= Max(FBitmap.Height, ((Height div cResizeBitmapStep)+1)*cResizeBitmapStep);
  end;

  Invalidate;
end;

//needed to remove flickering on resize and mouse-over
procedure TATSynEdit.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
  Msg.Result:= 1;
end;

function UpdateScrollInfoFromMessage(const Msg: TLMScroll; var Info: TATSynScrollInfo): boolean;
var
  AMax: integer;
begin
  AMax:= Max(0, Info.NMax-Info.NPage)+1;

  case Msg.ScrollCode of
    SB_TOP:        Info.NPos:= Info.NMin;
    SB_BOTTOM:     Info.NPos:= AMax;

    SB_LINEUP:     Info.NPos:= Max(Info.NPos-1, Info.NMin);
    SB_LINEDOWN:   Info.NPos:= Min(Info.NPos+1, AMax);

    SB_PAGEUP:     Info.NPos:= Max(Info.NPos-Info.NPage, Info.NMin);
    SB_PAGEDOWN:   Info.NPos:= Min(Info.NPos+Info.NPage, AMax);

    SB_THUMBPOSITION,
    SB_THUMBTRACK: Info.NPos:= Msg.Pos;
  end;

  Result:= Msg.ScrollCode<>SB_THUMBTRACK;
end;

procedure TATSynEdit.WMVScroll(var Msg: TLMVScroll);
begin
  Include(FPaintFlags, cPaintUpdateCaretsCoords);
  UpdateScrollInfoFromMessage(Msg, FScrollVert);
  Invalidate;
end;

procedure TATSynEdit.WMHScroll(var Msg: TLMHScroll);
begin
  Include(FPaintFlags, cPaintUpdateCaretsCoords);
  UpdateScrollInfoFromMessage(Msg, FScrollHorz);
  Invalidate;
end;


procedure TATSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PCaret: TPoint;
begin
  inherited;
  SetFocus;

  PCaret:= ClientPosToCaretPos(Point(X, Y));
  FCaretSpecPos:= false;
  FMouseDownNumber:= -1;
  FMouseDragging:= false;

  if PtInRect(FRectMinimap, Point(X, Y)) then
  if Shift=[ssLeft] then
  begin
    DoMinimapClick(Y);
    Exit
  end;

  if PtInRect(FRectMain, Point(X, Y)) then
  begin
    FMouseDownPnt:= PCaret;

    if Shift=[ssLeft] then
    begin
      FSelRect:= cRectEmpty;
      Strings.SetGroupMark;
      DoCaretSingleAsIs;

      if FOptMouseDragDrop and (GetCaretSelectionIndex(FMouseDownPnt)>=0) and not ModeReadOnly then
      begin
        FMouseDragging:= true;
        UpdateCursor;
      end
      else
      begin
        DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
        DoSelect_None;
      end;
    end;

    if Shift=[ssLeft, ssShift] then
    begin
      FSelRect:= cRectEmpty;
      DoCaretSingleAsIs;
      Carets[0].SelectToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
    end;

    if Shift=[ssLeft, ssXControl] then
    begin
      FSelRect:= cRectEmpty;
      DoCaretAddToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
    end;

    if Shift=[ssLeft, ssXControl, ssShift] then
    begin
      FSelRect:= cRectEmpty;
      DoCaretsColumnToPoint(FMouseDownPnt.X, FMouseDownPnt.Y);
    end;

    if Shift=[ssRight] then
    begin
      if FOptMouseRightClickMovesCaret then
        if GetCaretSelectionIndex(FMouseDownPnt)<0 then
        begin
          DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
          DoSelect_None;
        end;
    end;
  end;

  if PtInRect(FRectGutter, Point(X, Y)) then
  if Shift=[ssLeft] then
  begin
    //handle click on numbers here
    if FOptMouseGutterClickSelectsLine and
      (X>=FGutter[FGutterBandNum].Left) and
      (X<FGutter[FGutterBandNum].Right) then
    begin
      FSelRect:= cRectEmpty;
      FMouseDownNumber:= PCaret.Y;
      DoSelect_Line(PCaret);
    end
    else
      //on other bands- event
      DoEventClickGutter(FGutter.IndexAt(X), PCaret.Y);
  end;

  DoCaretsSort;
  DoEventCarets;
  Update;
end;

procedure TATSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDragging then
  begin
    DoDropText;
    Update;
  end;

  FMouseDownPnt:= Point(-1, -1);
  FMouseDownNumber:= -1;
  FMouseDownDouble:= false;
  FMouseDragging:= false;

  FTimerScroll.Enabled:= false;
end;

procedure TATSynEdit.UpdateCursor;
var
  P: TPoint;
  RectBm: TRect;
begin
  P:= ScreenToClient(Mouse.CursorPos);

  RectBm.Left:= FGutter[FGutterBandBm].Left;
  RectBm.Right:= FGutter[FGutterBandBm].Right;
  RectBm.Top:= FRectMain.Top;
  RectBm.Bottom:= FRectMain.Bottom;

  if FMouseDragging then
    Cursor:= crDrag
  else
  if PtInRect(FRectMain, P) then
    Cursor:= crIBeam
  else
  if PtInRect(RectBm, P) then
    Cursor:= crHandPoint
  else
    Cursor:= crDefault;
end;

procedure TATSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  RectNums: TRect;
  nIndex: integer;
begin
  inherited;

  P:= Point(X, Y);
  UpdateCursor;

  RectNums.Left:= FGutter[FGutterBandNum].Left;
  RectNums.Right:= FGutter[FGutterBandNum].Right;
  RectNums.Top:= FRectMain.Top;
  RectNums.Bottom:= FRectMain.Bottom;

  //start scroll timer
  FTimerScroll.Enabled:= (ssLeft in Shift) and (not PtInRect(FRectMain, P));
  FMouseAutoScroll:= cScrollNone;
  if P.Y<FRectMain.Top then FMouseAutoScroll:= cScrollUp else
  if P.Y>=FRectMain.Bottom then FMouseAutoScroll:= cScrollDown else
  if P.X<FRectMain.Left then FMouseAutoScroll:= cScrollLeft else
  if P.X>=FRectMain.Right then FMouseAutoScroll:= cScrollRight;

  //mouse dragged on numbers
  if PtInRect(RectNums, P) then
    if Shift=[ssLeft] then
    begin
      P:= ClientPosToCaretPos(P);
      if (P.Y>=0) and (P.X>=0) then
        if FMouseDownNumber>=0 then
        begin
          DoSelect_LineRange(FMouseDownNumber, P);
          DoCaretsSort;
          DoEventCarets;
          Update;
        end;
    end;

  //mouse dragged on text
  if not FMouseDragging then
  if PtInRect(FRectMain, P) then
  begin
    if ssLeft in Shift then
      if Carets.Count>0 then
      begin
        P:= ClientPosToCaretPos(P);
        if P.Y>=0 then
        begin
          //drag w/out button pressed: single selection
          if [ssXControl, ssShift, ssAlt]*Shift=[] then
          begin
            DoCaretSingleAsIs;
            if FMouseDownDouble and FOptMouse2ClickDragSelectsWords then
              DoSelect_WordRange(0, FMouseDownPnt, P)
            else
              DoSelect_CharRange(0, P);
          end;

          //drag with Ctrl pressed: add selection
          if Shift=[ssXControl, ssLeft] then
          begin
            nIndex:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
            DoSelect_CharRange(nIndex, P);
          end;

          //drag with Alt pressed
          if Shift=[ssAlt, ssLeft] then
          begin
            DoCaretSingle(FMouseDownPnt.X, FMouseDownPnt.Y);
            DoSelect_None;
            DoSelect_ColumnBlock(FMouseDownPnt, P);
          end;

          DoCaretsSort;
          DoEventCarets;
          Update;
        end;
      end;
  end;
end;

function TATSynEdit.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  Result:= DoMouseWheelAction(Shift, false);
end;

function TATSynEdit.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
begin
  Result:= DoMouseWheelAction(Shift, true);
end;

function TATSynEdit.DoMouseWheelAction(Shift: TShiftState; AUp: boolean): boolean;
begin
  if Shift=[ssCtrl] then
  begin
    DoSizeChange(AUp);
    Result:= true;
  end
  else
    Result:= false;
end;


procedure TATSynEdit.DblClick;
begin
  inherited;

  if FOptMouse2ClickSelectsLine then
    DoSelect_Line_ByClick
  else
  begin
    FMouseDownDouble:= true;
    DoSelect_Word_ByClick;
  end;
end;

procedure TATSynEdit.TripleClick;
begin
  inherited;

  if FOptMouse3ClickSelectsLine then
    DoSelect_Line_ByClick;
end;


procedure TATSynEdit.DoSelect_Word_ByClick;
var
  P: TPoint;
begin
  P:= ScreenToClient(Mouse.CursorPos);
  if PtInRect(FRectMain, P) then
  begin
    P:= ClientPosToCaretPos(P);
    if P.Y<0 then Exit;
    DoSelect_Word(P);
    Update;
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
  P: TPoint;
begin
  P:= ScreenToClient(Mouse.CursorPos);
  if PtInRect(FRectMain, P) then
  begin
    P:= ClientPosToCaretPos(P);
    if P.Y<0 then Exit;
    DoSelect_Line(P);
    Update;
  end;
end;


procedure TATSynEdit.Invalidate;
begin
  Include(FPaintFlags, cPaintUpdateBitmap);
  inherited;
end;

procedure TATSynEdit.TimerBlinkTick(Sender: TObject);
begin
  if FCaretStopUnfocused then
    if not Focused then
    begin
      if FCaretShown then
      begin
        FCaretShown:= not FCaretShown;
        DoPaintCarets(FBitmap.Canvas, true);
      end;
      Exit;
    end;

  FCaretShown:= not FCaretShown;
  DoPaintCarets(FBitmap.Canvas, true);
end;

procedure TATSynEdit.TimerScrollTick(Sender: TObject);
var
  nIndex: integer;
  PClient, PCaret: TPoint;
begin
  PClient:= ScreenToClient(Mouse.CursorPos);
  PClient.X:= Max(FRectMain.Left, PClient.X);
  PClient.Y:= Max(FRectMain.Top, PClient.Y);
  PClient.X:= Min(FRectMain.Right, PClient.X);
  PClient.Y:= Min(FRectMain.Bottom, PClient.Y);

  case FMouseAutoScroll of
    cScrollUp:    DoScrollByDelta(0, -cScrollAutoVert);
    cScrollDown:  DoScrollByDelta(0, cScrollAutoVert);
    cScrollLeft:  DoScrollByDelta(-cScrollAutoHorz, 0);
    cScrollRight: DoScrollByDelta(cScrollAutoHorz, 0);
    else Exit;
  end;

  PCaret:= ClientPosToCaretPos(PClient);
  if (PCaret.X>=0) and (PCaret.Y>=0) then
    if FMouseDownNumber>=0 then
    begin
      DoSelect_LineRange(FMouseDownNumber, PCaret);
    end
    else
    begin
      nIndex:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
      if nIndex>=0 then
        Carets[nIndex].SelectToPoint(PCaret.X, PCaret.Y);
    end;

  DoCaretsSort;
  DoEventCarets;
  DoEventScroll;
  Update;
end;

procedure TATSynEdit.DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
var
  R: TRect;
  i: integer;
  Item: TATCaretItem;
  Shape: TATSynCaretShape;
begin
  if ModeOver then
    Shape:= FCaretShapeOvr
  else
    Shape:= FCaretShape;

  for i:= 0 to FCarets.Count-1 do
  begin
    Item:= FCarets[i];
    R.Left:= Item.CoordX;
    R.Top:= Item.CoordY;
    R.Right:= R.Left+FCharSize.X;
    R.Bottom:= R.Top+FCharSize.Y;

    //caret not visible
    if R.Left<0 then Continue;
    if R.Top<0 then Continue;

    case Shape of
      cCaretShapeVert1px:       begin R.Right:= R.Left+1; end;
      cCaretShapeVert2px:       begin R.Right:= R.Left+2; end;
      cCaretShapeVert3px:       begin R.Right:= R.Left+3; end;
      cCaretShapeVert4px:       begin R.Right:= R.Left+4; end;
      cCaretShapeVert10percent: begin R.Right:= R.Left+Trunc(FCharSize.X*0.10); end;
      cCaretShapeVert20percent: begin R.Right:= R.Left+Trunc(FCharSize.X*0.20); end;
      cCaretShapeVert30percent: begin R.Right:= R.Left+Trunc(FCharSize.X*0.30); end;
      cCaretShapeVert40percent: begin R.Right:= R.Left+Trunc(FCharSize.X*0.40); end;
      cCaretShapeVert50percent: begin R.Right:= R.Left+FCharSize.X div 2; end;
      cCaretShapeHorz1px:       begin R.Top:= R.Bottom-1; end;
      cCaretShapeHorz2px:       begin R.Top:= R.Bottom-2; end;
      cCaretShapeHorz3px:       begin R.Top:= R.Bottom-3; end;
      cCaretShapeHorz4px:       begin R.Top:= R.Bottom-4; end;
      cCaretShapeHorz10percent: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.10); end;
      cCaretShapeHorz20percent: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.20); end;
      cCaretShapeHorz30percent: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.30); end;
      cCaretShapeHorz40percent: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.40); end;
      cCaretShapeHorz50percent: begin R.Top:= R.Bottom-FCharSize.Y div 2; end;
    end;

    if IntersectRect(R, R, FRectMain) then
    begin
      CanvasInvertRect(C, R, FColors.Caret);
      if AWithInvalidate then
        InvalidateRect(Handle, @R, false);
    end;
  end;
end;

procedure TATSynEdit.DoPaintModeStatic;
begin
  FPaintStatic:= true;
  FTimerBlink.Enabled:= false;
  FCaretShown:= false;
  Invalidate;
end;

procedure TATSynEdit.DoPaintModeBlinking;
begin
  FPaintStatic:= false;
  if Assigned(FTimerBlink) then
  begin
    FTimerBlink.Enabled:= false;
    FTimerBlink.Enabled:= true;
  end;
end;

procedure TATSynEdit.DoEventCommand(ACommand: integer; out AHandled: boolean);
begin
  AHandled:= false;
  if Assigned(FOnCommand) then
    FOnCommand(Self, ACommand, AHandled);
end;

function TATSynEdit.GetCaretTime: integer;
begin
  Result:= FTimerBlink.Interval;
end;

function TATSynEdit.GetWrapInfoIndex(AMousePos: TPoint): integer;
var
  NPixels: integer;
begin
  Result:= -1;
  NPixels:= AMousePos.Y - FRectMain.Top;
  Result:= FScrollVert.NPos + NPixels div FCharSize.Y;
  if not FWrapInfo.IsIndexValid(Result) then
    Result:= -1;
end;

procedure TATSynEdit.DoEventCarets;
begin
  if Assigned(FOnCaretMoved) then
    FOnCaretMoved(Self);
end;

procedure TATSynEdit.DoEventScroll;
begin
  if Assigned(FOnScrolled) then
    FOnScrolled(Self);
end;

procedure TATSynEdit.DoEventChange;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TATSynEdit.DoEventState;
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TATSynEdit.DoEventClickGutter(ABandIndex, ALineNumber: integer);
begin
  if Assigned(FOnClickGutter) then
    FOnClickGutter(Self, ABandIndex, ALineNumber);
end;

procedure TATSynEdit.DoEventDrawBookmarkIcon(C: TCanvas; ALineNumber: integer; const ARect: TRect);
begin
  if Assigned(FOnDrawBookmarkIcon) then
    FOnDrawBookmarkIcon(Self, C, ALineNumber, ARect);
end;

procedure TATSynEdit.DoScrollByDelta(Dx, Dy: integer);
begin
  with FScrollHorz do
    NPos:= Max(0, Min(NMax-NPage, NPos+Dx));
  with FScrollVert do
    NPos:= Max(0, Min(NMax-NPage, NPos+Dy));
end;

procedure TATSynEdit.MenuClick(Sender: TObject);
var
  Cmd: integer;
begin
  Cmd:= (Sender as TMenuItem).Tag;
  if Cmd>0 then
  begin
    DoCommandExec(Cmd);
    Update;
  end;
end;

procedure TATSynEdit.MenuPopup(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to FMenu.Items.Count-1 do
    with FMenu.Items[i] do
      case Tag of
        cCommand_ClipboardCut: Enabled:= not ModeReadOnly;
        cCommand_ClipboardPaste: Enabled:= not ModeReadOnly and Clipboard.HasFormat(CF_Text);
        cCommand_TextDeleteSelection: Enabled:= not ModeReadOnly and Carets.IsSelection;
        cCommand_Undo: Enabled:= not ModeReadOnly and (UndoCount>0);
        cCommand_Redo: Enabled:= not ModeReadOnly and (RedoCount>0);
      end;
end;

procedure TATSynEdit.DoInitDefaultPopupMenu;
  //
  procedure Add(const SName: string; Cmd: integer);
  var
    MI: TMenuItem;
  begin
    MI:= TMenuItem.Create(FMenu);
    MI.Caption:= SName;
    MI.ShortCut:= FKeyMapping.GetShortcutFromCommand(Cmd);
    MI.Tag:= Cmd;
    MI.OnClick:= MenuClick;
    FMenu.Items.Add(MI);
  end;
  //
begin
  FMenu.OnPopup:= MenuPopup;
  PopupMenu:= FMenu;

  Add('Undo', cCommand_Undo);
  Add('Redo', cCommand_Redo);
  Add('-', 0);
  Add('Cut', cCommand_ClipboardCut);
  Add('Copy', cCommand_ClipboardCopy);
  Add('Paste', cCommand_ClipboardPaste);
  Add('Delete', cCommand_TextDeleteSelection);
  Add('-', 0);
  Add('Select all', cCommand_SelectAll);
end;


//drop selection of 1st caret into mouse-pos
procedure TATSynEdit.DoDropText;
var
  P, PosAfter, Shift: TPoint;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
  Str: atString;
  Relation: TATPosRelation;
begin
  if Carets.Count<>1 then Exit; //allow only 1 caret
  Carets[0].GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then Exit;

  DoSelect_None;

  //calc insert-pos
  P:= ScreenToClient(Mouse.CursorPos);
  P:= ClientPosToCaretPos(P);
  if P.Y<0 then
    begin Beep; Exit end;

  //can't drop into selection
  Relation:= IsPosInRange(P.X, P.Y, X1, Y1, X2, Y2);
  if Relation=cRelateInside then
    begin Beep; Exit end;

  Str:= Strings.TextSubstring(X1, Y1, X2, Y2);
  if Str='' then
    begin Beep; Exit end;

  //insert before selection?
  if Relation=cRelateBefore then
  begin
    Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
    Strings.TextInsert(P.X, P.Y, Str, false, Shift, PosAfter);
    DoCaretSingle(P.X, P.Y);
  end
  else
  begin
    Strings.TextInsert(P.X, P.Y, Str, false, Shift, PosAfter);
    Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
    DoCaretSingle(P.X, P.Y);
    DoCaretsShift(X1, Y1, Shift.X, Shift.Y);
  end;

  Update(true);
end;

function TATSynEdit.GetAutoIndentString(APosX, APosY: integer): atString;
var
  Str: atString;
  NChars, NSpaces: integer;
begin
  if not FOptAutoIndent then
    begin Result:= ''; Exit end;

  Str:= Strings.Lines[APosY];
  NChars:= SGetIndentChars(Str); //count of chars in indent
  NChars:= Min(APosX, NChars); //limit indent by x-pos

  Str:= Copy(Str, 1, NChars);
  NSpaces:= Length(STabsToSpaces(Str, FTabSize));

  case FOptAutoIndentKind of
    cIndentAsIs:
      Result:= Str;
    cIndentSpaces:
      Result:= StringOfChar(' ', NSpaces);
    cIndentTabsOnly:
      Result:= StringOfChar(#9, NSpaces div FTabSize);
    cIndentTabsSpaces:
      Result:= StringOfChar(#9, NSpaces div FTabSize) + StringOfChar(' ', NSpaces mod FTabSize);
    else
      raise Exception.Create('Unknown autoindent-kind');
  end;
end;

function TATSynEdit.GetRedoCount: integer;
begin
  Result:= Strings.RedoCount;
end;

procedure TATSynEdit.DoMinimapClick(APosY: integer);
var
  N: integer;
begin
  N:= (APosY-FRectMinimap.Top) div FCharSizeMinimap.Y + FScrollVertMinimap.NPos;
  if FWrapInfo.IsIndexValid(N) then
  begin
    N:= FWrapInfo[N].NLineIndex;
    DoGotoPos(Point(0, N));
  end;
end;

procedure TATSynEdit.InitColors;
begin
  FColors.TextBG:= clWhite;
  FColors.TextFont:= clBlack;
  FColors.TextSelFont:= clHighlightText;
  FColors.TextSelBG:= clHighlight;
  FColors.Caret:= clBlack;
  FColors.GutterFont:= clGray;
  FColors.GutterBG:= $e0e0e0;
  FColors.GutterCaretBG:= $c8c8c8;
  FColors.CurLineBG:= $e0f0f0;
  FColors.RulerBG:= FColors.GutterBG;
  FColors.RulerFont:= clGray;
  FColors.CollapseLine:= clNavy;
  FColors.CollapseFont:= clBlue;
  FColors.MarginRight:= clLtGray;
  FColors.MarginCaret:= clLime;
  FColors.MarginUser:= clYellow;
  FColors.IndentLines:= clMedGray;
  FColors.UnprintedFont:= $5050f0;
  FColors.UnprintedBG:= $e0e0e0;
  FColors.UnprintedHexFont:= clMedGray;
  FColors.MinimapBorder:= clLtGray;
  FColors.MinimapSelBG:= $eeeeee;
  FColors.MicromapBG:= clCream;
  FColors.StateChanged:= $00f0f0;
  FColors.StateAdded:= $20c020;
  FColors.StateSaved:= clMedGray;
  FColors.LockedBG:= $e0e0e0;
end;

function TATSynEdit.GetUndoLimit: integer;
begin
  Result:= Strings.UndoLimit;
end;

procedure TATSynEdit.SetUndoLimit(AValue: integer);
begin
  Strings.UndoLimit:= AValue;
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

procedure TATSynEdit.DoSizeChange(AInc: boolean);
var
  NTop: integer;
begin
  if not AInc then
    if Font.Size<=cMinFontSize then Exit;

  NTop:= ScrollTop;
  Font.Size:= Font.Size+BoolToPlusMinusOne(AInc);
  Update;
  Application.ProcessMessages; //needed to apply Scrolltop

  ScrollTop:= NTop;
  Update;
end;

procedure TATSynEdit.BeginUpdate;
begin
  Inc(FPaintLocked);
  Invalidate;
end;

procedure TATSynEdit.EndUpdate;
begin
  Dec(FPaintLocked);
  if FPaintLocked=0 then
    Invalidate;
end;

{$I atsynedit_carets.inc}
{$I atsynedit_hilite.inc}
{$I atsynedit_sel.inc}

{$I atsynedit_cmd_handler.inc}
{$I atsynedit_cmd_keys.inc}
{$I atsynedit_cmd_editing.inc}
{$I atsynedit_cmd_clipboard.inc}
{$I atsynedit_cmd_misc.inc}


initialization
  cClipFormatId:= RegisterClipboardFormat('Application/X-Laz-ATSynEdit-Block');

end.

