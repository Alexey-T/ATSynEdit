unit ATSynEdit;

{$mode delphi}
//{$define beep_wrapinfo}

interface

uses
  Classes, SysUtils, Graphics,
  Controls, ExtCtrls,
  LMessages, LCLType,
  {$ifdef windows}
  Messages,
  {$endif}
  ATStringProc, ATStrings, ATCanvasProc, ATGutter,
  ATSynCarets, ATKeyMapping;

type
  TATCommandResult = (
    cResultText,
    cResultCaretAny,
    cResultCaretLeft,
    cResultCaretTop,
    cResultCaretRight,
    cResultCaretBottom,
    cResultScroll,
    cResultState
    );
  TATCommandResults = set of TATCommandResult;

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

  { TATSynWrapItem }

  TATSynWrapFinal = (cWrapItemFinal, cWrapItemCollapsed, cWrapItemMiddle);
  TATSynWrapItem = class
    NLineIndex,
    NCharIndex,
    NLength,
    NIndent: integer;
    NFinal: TATSynWrapFinal;
  end;

  { TATSynWrapInfo }

  TATSynWrapInfo = class
  private
    FList: TList;
    function GetItem(N: integer): TATSynWrapItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    function IsItemInitial(N: integer): boolean;
    function IsItemAfterCollapsed(N: integer): boolean;
    property Items[N: integer]: TATSynWrapItem read GetItem;
    procedure Add(AIndex, AOffset, ALen, AIndent: integer; AFinal: TATSynWrapFinal);
  end;

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
  cInitNumbersFontSize = 8;
  cInitNumbersStyle = cNumbersAll;
  cInitBitmapWidth = 1000;
  cInitBitmapHeight = 800;

  cInitColorCaret = clBlack;
  cInitColorTextBG = clWhite;
  cInitColorTextFont = clBlack;
  cInitColorGutterFont = clGray;
  cInitColorGutterBG = $e0e0e0;
  cInitColorGutterCaretBG = $c8c8c8;
  cInitColorCurLineBG = $e0f0f0;
  cInitColorRulerMark = clGray;
  cInitColorCollapsedLine = clNavy;
  cInitColorCollapsedText = clBlue;
  cInitColorMargins = clLtGray;
  cInitColorUnprintedFont = $5050f0;
  cInitColorUnprintedBG = $e0e0e0;
  cInitColorMicromapBG = clCream;
  cInitColorMinimapBorder = clLtGray;
  cInitColorMinimapSel = $eeeeee;
  cInitColorStateChanged = $00f0f0;
  cInitColorStateAdded = $20c020;
  cInitColorStateSaved = clNavy;

  cGutterBands = 5;
  cSizeGutterBandBm = 15;
  cSizeGutterBandNum = 10;
  cSizeGutterBandFold = 0;
  cSizeGutterBandState = 3;
  cSizeGutterBandEmpty = 2;
  cUnprintedFontOffsetX = 3;
  cUnprintedFontOffsetY = 2;
  cUnprintedFontScale = 0.80;
  cUnprintedDotMinSize = 2;
  cUnprintedDotScale = 6; //divider of line height
  cUnprintedEndScale = 3; //divider of line height
  cSizeScrollCaretHorz = 10;
  cSizeScrollCaretVert = 0;
  cSizeScrollGotoHorz = 10;
  cSizeScrollGotoVert = 3;
  cSizeCollapseMarkIndent = 3;
  cSizeScrollHorzAuto = 10; //chars to scroll by drag out of right/left
  cSizeScrollVertAuto = 1;
  cSizeScrollHorzKeep = 1; //n chars allow handy clicking after eol
  cSizeBitmapStep = 100;
  cOffsetTextLeft = 0; //needs fixing bookmark bg paint if >0
  cOffsetTextTop = 1;
  cSizeGutterNumOffsetLeft = 5;
  cSizeGutterNumOffsetRight = 4;
  cSizeRulerHeight = 19;
  cSizeRulerMarkSmall = 3;
  cSizeRulerMarkBig = 6;
  cMaxCharsForOutput = 400;
  cMinTabSize = 1;
  cMaxTabSize = 16;
  cMinMinimapWidth = 30;
  cMinWrapColumn = 20;
  cMinMarginRt = 20;
  cMinCaretTime = 300;
  cMaxCaretTime = 2000;

  { TATGutter }

type
  TATSynEditCommandEvent = procedure(Sender: TObject; ACommand: integer; var AHandled: boolean) of object;
  TATSynEditClickGutter = procedure(Sender: TObject; ABand: integer; ALineNum: integer) of object;
  TATSynEditDrawBookmarkIcon = procedure(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect) of object;

  { TATSynEdit }

type
  TATSynEdit = class(TCustomControl)
  private
    FTimerBlink: TTimer;
    FTimerScroll: TTimer;
    FPaintStatic: boolean;
    FPaintFlags: TATSynPaintFlags;
    FBitmap: TBitmap;
    FKeyMapping: TATKeyMapping;
    FMarginRight: integer;
    FMarginList: TList;
    FStringsInt,
    FStringsExt: TATStrings;
    FTextOffset: TPoint;
    FCarets: TATCarets;
    FCaretShape,
    FCaretShapeOvr: TATSynCaretShape;
    FCaretShown: boolean;
    FCaretVirtual: boolean;
    FCaretSpecPos: boolean;
    FCaretMoveByRtClick: boolean;
    FOver: boolean;
    FUseOverOnPaste: boolean;
    FMouseDownPnt: TPoint;
    FMouseDownNumber: integer;
    FMouseAutoScroll: TATAutoScroll;
    FKeyNavigateInWrappedLines: boolean;
    FOnCaretMoved: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnScrolled: TNotifyEvent;
    FOnClickGutter: TATSynEditClickGutter;
    FOnDrawBookmarkIcon: TATSynEditDrawBookmarkIcon;
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
    FTabSize: integer;
    FGutter: TATGutter;
    FGutterBandBm,
    FGutterBandNum,
    FGutterBandState,
    FGutterBandFold,
    FGutterBandEmpty: integer;
    FColorTextFont: TColor;
    FColorTextBG: TColor;
    FColorTextSel: TColor;
    FColorTextSelBG: TColor;
    FColorCaret: TColor;
    FColorGutterFont: TColor;
    FColorGutterBG: TColor;
    FColorGutterCaretBG: TColor;
    FColorCurLineBG: TColor;
    FColorMargins: TColor;
    FColorRulerMark: TColor;
    FColorRulerBG: TColor;
    FColorCollapsedLine: TColor;
    FColorCollapsedText: TColor;
    FColorUnprintedFont: TColor;
    FColorUnprintedBG: TColor;
    FColorMinimapBorder: TColor;
    FColorMinimapSel: TColor;
    FColorStateChanged: TColor;
    FColorStateAdded: TColor;
    FColorStateSaved: TColor;
    FRectMain,
    FRectMinimap,
    FRectMicromap,
    FRectGutter,
    FRectRuler: TRect;
    FRulerHeight: integer;
    FRulerVisible: boolean;
    FRulerFontSize: integer;
    FRulerMarkSizeSmall: integer;
    FRulerMarkSizeBig: integer;
    FGutterVisible: boolean;
    FNumbersStyle: TATSynNumbersStyle;
    FNumbersFontSize: integer;
    FMinimapWidth: integer;
    FMinimapFontSize: integer;
    FMinimapVisible: boolean;
    FMicromapWidth: integer;
    FMicromapVisible: boolean;
    FCharSpacingText,
    FCharSpacingMinimap: TPoint;
    FScrollVert,
    FScrollHorz: TATSynScrollInfo;
    FScrollVertMinimap,
    FScrollHorzMinimap: TATSynScrollInfo;
    FPrevHorz,
    FPrevVert: TATSynScrollInfo;
    FTextAutoIndent: boolean;
    FTextTabSpaces: boolean;
    FShowCurLine: boolean;
    FShowCurColumn: boolean;
    //
    procedure DoCalcLineHilite(const AItem: TATSynWrapItem; var AParts: TATLineParts;
      ACharsSkipped, ACharsMax: integer; AColorBG: TColor);
    procedure DoCaretSingle(AX, AY: integer);
    procedure DoCaretSingleAsIs;
    procedure DoPaint(AFlags: TATSynPaintFlags);
    procedure DoPaintMarginLineTo(C: TCanvas; AX: integer);
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
    procedure DoPaintUnprintedChars(C: TCanvas;
      const Str: atString;
      ALineStart: TPoint;
      const AVisibleRect: TRect;
      ACharSize: TPoint;
      AOffsetX: integer);
    procedure DoPaintUnprintedEol(C: TCanvas;
      const AItem: TATSynWrapItem;
      ALineStart: TPoint;
      AOffsetX: integer);
    procedure DoPaintUnprintedSpace(C: TCanvas; const ARect: TRect; AScale: integer);
    procedure DoPaintUnprintedTabulation(C: TCanvas; const ARect: TRect);
    procedure DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
    procedure DoPaintModeStatic;
    procedure DoPaintModeBlinking;
    procedure DoCaretsSort;
    procedure DoEventCarets;
    procedure DoEventScroll;
    procedure DoEventChange;
    procedure DoEventState;
    procedure DoEventClickGutter(ABandIndex, ALineNumber: integer);
    procedure DoEventCommand(ACommand: integer; out AHandled: boolean);
    procedure DoEventDrawBookmarkIcon(C: TCanvas; ALineNumber: integer; const ARect: TRect);
    function GetCaretsTime: integer;
    function GetCharSpacingX: integer;
    function GetCharSpacingY: integer;
    function GetLastPos: TPoint;
    function GetMarginString: string;
    function GetReadOnly: boolean;
    function GetScrollTop: integer;
    function GetWrapInfoIndex(AMousePos: TPoint): integer;
    function GetStrings: TATStrings;
    function IsKeyMappingMatchedItem(const Str: string;
      const Item: TATKeyMappingItem): boolean;
    function IsPosSelected(AX, AY: integer): boolean;
    procedure SetCaretsTime(AValue: integer);
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
    function GetGutterNumbersColumnWidth: integer;
    function GetVisibleLines: integer;
    function GetVisibleColumns: integer;
    function GetVisibleLinesMinimap: integer;
    function GetMinimapScrollPos: integer;
    procedure SetTabSize(AValue: integer);
    procedure SetWrapMode(AValue: TATSynWrapMode);
    procedure SetWrapIndented(AValue: boolean);
    procedure UpdateGutterAutosize;
    procedure UpdateMinimapAutosize(C: TCanvas);
    function DoFormatLineNumber(N: integer): atString;
    procedure UpdateWrapInfo;
    procedure UpdateScrollbars(AllowRepeat: boolean = true);
    procedure UpdateScrollbarVert;
    procedure UpdateScrollbarHorz;
    procedure UpdateCaretsCoords(AOnlyLast: boolean = false);
    function GetCharSize(C: TCanvas; ACharSpacing: TPoint): TPoint;
    function GetScrollbarVisible(bVertical: boolean): boolean;
    procedure SetMarginRight(AValue: integer);
    procedure TimerBlinkTick(Sender: TObject);
    procedure TimerScrollTick(Sender: TObject);

    //carets
    procedure DoCaretAddToPoint(Pnt: TPoint);
    procedure DoCaretsColumnToPoint(const PntTo: TPoint);
    procedure DoCaretsShift(APosX, APosY: integer; AShiftX, AShiftY: integer; AShiftBelowX: integer = 0);
    procedure DoCaretsDeleteOnSameLines;

    //editing
    procedure DoCommandResults(Res: TATCommandResults);
    function DoCommand_SelectWords: TATCommandResults;
    function DoCommand_SelectLines: TATCommandResults;
    function DoCommand_SelectAll: TATCommandResults;
    function DoCommand_CaretsRemove: TATCommandResults;
    function DoCommand_TextDeleteWord(ANext: boolean): TATCommandResults;
    function DoCommand_ToggleReadOnly: TATCommandResults;
    function DoCommand_ToggleOver: TATCommandResults;
    function DoCommand_GotoWord(ANext: boolean): TATCommandResults;
    function DoCommand_ScrollToCaret(AEdge: TATCaretEdge): TATCommandResults;
    function DoCommand_ScrollVert(ALines: integer): TATCommandResults;
    function DoCommand_TextInsertAtCarets(const AText: atString; AKeepCaret, AOvrMode: boolean): TATCommandResults;
    function DoCommand_TextInsertTab: TATCommandResults;
    function DoCommand_KeyHome: TATCommandResults;
    function DoCommand_KeyEnd: TATCommandResults;
    function DoCommand_KeyLeft: TATCommandResults;
    function DoCommand_KeyRight: TATCommandResults;
    function DoCommand_KeyUpDown(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_KeyUpDown_NextLine(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_KeyUpDown_Wrapped(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_TextDeleteLeft(ALen: integer): TATCommandResults;
    function DoCommand_TextDeleteRight(ALen: integer): TATCommandResults;
    function DoCommand_TextInsertEol(AKeepCaret: boolean): TATCommandResults;
    function DoCommand_TextDeleteCurLine: TATCommandResults;
    function DoCommand_TextDuplicateCurLine: TATCommandResults;
    function DoCommand_TextDeleteToLineBegin: TATCommandResults;
    function DoCommand_TextDeleteToLineEnd: TATCommandResults;
    function DoCommand_GotoTextBegin: TATCommandResults;
    function DoCommand_GotoTextEnd: TATCommandResults;
    function DoCommand_ClipboardPaste(AKeepCaret: boolean): TATCommandResults;
    //
    function GetCommandFromKey(var Key: Word; Shift: TShiftState): integer;

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
    //misc
    procedure DoSelect_All;
    procedure DoSelect_Line(P: TPoint);
    procedure DoSelect_Word(P: TPoint);
    procedure DoSelect_Lines_ToPoint(ALineFrom: integer; P: TPoint);
    procedure DoFoldLines(ALineFrom, ALineTo, ACharPosFrom: integer; AFold: boolean);
    procedure DoCommandExec(ACmd: integer; const AText: atString = '');
    procedure DoScrollByDelta(Dx, Dy: integer);

  protected
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DblClick; override;
    procedure TripleClick; override;
    //messages
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  published
    //events
    property OnCaretMoved: TNotifyEvent read FOnCaretMoved write FOnCaretMoved;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnScrolled: TNotifyEvent read FOnScrolled write FOnScrolled;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnCommand: TATSynEditCommandEvent read FOnCommand write FOnCommand;
    property OnClickGutter: TATSynEditClickGutter read FOnClickGutter write FOnClickGutter;
    property OnDrawBookmarkIcon: TATSynEditDrawBookmarkIcon read FOnDrawBookmarkIcon write FOnDrawBookmarkIcon;

    //options
    property OptAutoIndent: boolean read FTextAutoIndent write FTextAutoIndent;
    property OptTabSpaces: boolean read FTextTabSpaces write FTextTabSpaces;
    property OptTabSize: integer read FTabSize write SetTabSize;
    property OptUseOverOnPaste: boolean read FUseOverOnPaste write FUseOverOnPaste;
    property OptShowCurLine: boolean read FShowCurLine write FShowCurLine;
    property OptShowCurColumn: boolean read FShowCurColumn write FShowCurColumn;
    property OptCaretVirtual: boolean read FCaretVirtual write FCaretVirtual;
    property OptCaretShape: TATSynCaretShape read FCaretShape write SetCaretShape;
    property OptCaretShapeOvr: TATSynCaretShape read FCaretShapeOvr write SetCaretShapeOvr;
    property OptCaretsTime: integer read GetCaretsTime write SetCaretsTime;
    property OptGutterVisible: boolean read FGutterVisible write FGutterVisible;
    property OptRulerVisible: boolean read FRulerVisible write FRulerVisible;
    property OptMinimapVisible: boolean read FMinimapVisible write SetMinimapVisible;
    property OptMicromapVisible: boolean read FMicromapVisible write SetMicromapVisible;
    property OptCharSpacingX: integer read GetCharSpacingX write SetCharSpacingX;
    property OptCharSpacingY: integer read GetCharSpacingY write SetCharSpacingY;
    property OptWrapMode: TATSynWrapMode read FWrapMode write SetWrapMode;
    property OptWrapIndented: boolean read FWrapIndented write SetWrapIndented;
    property OptMarginRight: integer read FMarginRight write SetMarginRight;
    property OptMarginString: string read GetMarginString write SetMarginString;
    property OptNumbersStyle: TATSynNumbersStyle read FNumbersStyle write FNumbersStyle;
    property OptUnprintedVisible: boolean read FUnprintedVisible write FUnprintedVisible;
    property OptUnprintedSpaces: boolean read FUnprintedSpaces write FUnprintedSpaces;
    property OptUnprintedEnds: boolean read FUnprintedEnds write FUnprintedEnds;
    property OptUnprintedEndsDetails: boolean read FUnprintedEndsDetails write FUnprintedEndsDetails;
    property OptUnprintedReplaceSpec: boolean read FUnprintedReplaceSpec write FUnprintedReplaceSpec;
  end;

implementation

uses
  LCLIntf,
  LCLProc,
  Forms,
  Dialogs,
  Types,
  Menus,
  Math,
  Clipbrd,
  ATSynEdit_Commands,
  ATSynEdit_KeyMapping,
  ATStringProc_WordJump;

{$I atsynedit_proc.inc}

{ TATSynWrapInfo }

function TATSynWrapInfo.GetItem(N: integer): TATSynWrapItem;
begin
  if IsIndexValid(N) then
    Result:= TATSynWrapItem(FList[N])
  else
    Result:= nil;
end;

constructor TATSynWrapInfo.Create;
begin
  FList:= TList.Create;
end;

destructor TATSynWrapInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynWrapInfo.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    TObject(FList[i]).Free;
    FList.Delete(i);
  end;
end;

function TATSynWrapInfo.Count: integer;
begin
  Result:= FList.Count;
end;

function TATSynWrapInfo.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATSynWrapInfo.IsItemInitial(N: integer): boolean;
begin
  if IsIndexValid(N) then
  begin
    if N=0 then
      Result:= true
    else
      Result:= Items[N].NLineIndex<>Items[N-1].NLineIndex;
  end
  else
    Result:= true;
end;

function TATSynWrapInfo.IsItemAfterCollapsed(N: integer): boolean;
begin
  if IsIndexValid(N) and (N>0) then
  begin
    Result:= Items[N].NLineIndex-Items[N-1].NLineIndex > 1;
  end
  else
    Result:= false;
end;

procedure TATSynWrapInfo.Add(AIndex, AOffset, ALen, AIndent: integer; AFinal: TATSynWrapFinal);
var
  Item: TATSynWrapItem;
begin
  Item:= TATSynWrapItem.Create;
  Item.NLineIndex:= AIndex;
  Item.NCharIndex:= AOffset;
  Item.NLength:= ALen;
  Item.NIndent:= AIndent;
  Item.NFinal:= AFinal;
  FList.Add(Item);
end;


{ TATSynEdit }

procedure TATSynEdit.DoPaintRulerTo(C: TCanvas);
const
  ATabSize = 1; //tab not needed in ruler
var
  NX, NSize, NPrevSize, NRulerStart, i: integer;
  ACharSize: TPoint;
  Str: string;
begin
  NPrevSize:= C.Font.Size;
  NRulerStart:= FScrollHorz.NPos;

  C.Font.Size:= FRulerFontSize;
  ACharSize:= GetCharSize(C, Point(0, 0));

  C.Font.Color:= FColorRulerMark;
  C.Pen.Color:= FColorRulerMark;
  C.Brush.Color:= FColorRulerBG;

  C.FillRect(FRectRuler);
  C.MoveTo(FRectRuler.Left, FRectRuler.Bottom);
  C.LineTo(FRectRuler.Right, FRectRuler.Bottom);

  for i:= NRulerStart to NRulerStart+GetVisibleColumns+1 do
  begin
    NX:= FTextOffset.X+(i-NRulerStart)*FCharSize.X;
    if i mod 5 = 0 then
      NSize:= FRulerMarkSizeBig
    else
      NSize:= FRulerMarkSizeSmall;
    C.MoveTo(NX, FRulerHeight-1);
    C.LineTo(NX, FRulerHeight-1-NSize);

    if i mod 10 = 0 then
    begin
      Str:= IntToStr(i);
      Dec(NX, CanvasTextWidth(Str, ATabSize, ACharSize) div 2);
      CanvasTextOut(C, NX, 0, Str, ATabSize, ACharSize, false);
    end;
  end;

  C.Font.Size:= NPrevSize;
end;

procedure TATSynEdit.UpdateGutterAutosize;
begin
  FGutter[FGutterBandNum].Size:= GetGutterNumbersColumnWidth;
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
  case FNumbersStyle of
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
  NNewVisibleColumns, NOffset, NLen, NIndent, NHiddenIndex, i: integer;
  NFinal: TATSynWrapFinal;
  Str: atString;
begin
  NNewVisibleColumns:= GetVisibleColumns;
  if (not FWrapUpdateNeeded) and
    (FWrapMode=cWrapOn) and
    (FPrevVisibleColumns<>NNewVisibleColumns) then
    FWrapUpdateNeeded:= true;

  if not FWrapUpdateNeeded then Exit;
  FWrapUpdateNeeded:= false;
  FPrevVisibleColumns:= NNewVisibleColumns;

  FWrapInfo.Clear;

  {$ifdef beep_wrapinfo}
  SysUtils.Beep;
  {$endif}

  case FWrapMode of
    cWrapOff:
      FWrapColumn:= 0;
    cWrapOn:
      FWrapColumn:= Max(cMinWrapColumn, NNewVisibleColumns-cSizeScrollHorzKeep);
    cWrapAtMargin:
      FWrapColumn:= Max(cMinWrapColumn, FMarginRight);
  end;

  for i:= 0 to Strings.Count-1 do
  begin
    NHiddenIndex:= Strings.LinesHidden[i];
    if NHiddenIndex<0 then Continue;

    Str:= Strings.Lines[i];
    NLen:= Length(Str);

    //line collapsed partially?
    if NHiddenIndex>0 then
    begin
      FWrapInfo.Add(i, 1, Min(NLen, NHiddenIndex-1), 0, cWrapItemCollapsed);
      Continue;
    end;

    //wrap not needed?
    if (FWrapColumn<=1) then
    begin
      FWrapInfo.Add(i, 1, NLen, 0, cWrapItemFinal);
      Continue
    end;

    NOffset:= 1;
    NIndent:= 0;

    repeat
      NLen:= SFindWordWrapPosition(Str, FWrapColumn-NIndent, FTabSize);
      if NLen>=Length(Str) then
        NFinal:= cWrapItemFinal
      else
        NFinal:= cWrapItemMiddle;
      FWrapInfo.Add(i, NOffset, NLen, NIndent, NFinal);

      if FWrapIndented then
        if NOffset=1 then
          NIndent:= SGetIndentExpanded(Str, FTabSize);

      Inc(NOffset, NLen);
      Delete(Str, 1, NLen);

      if (str<>'') and (nlen<=0) then showmessage('aa');
    until Str='';
  end;
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

procedure TATSynEdit.UpdateScrollbars(AllowRepeat: boolean = true);
var
  bVert1, bVert2,
  bHorz1, bHorz2: boolean;
begin
  with FScrollVert do
  begin
    NPage:= Max(1, GetVisibleLines)-1;
    NMin:= 0;
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

  //recalculate scrollbars, if they shown just now
  if AllowRepeat then
    if (not bVert1 and bVert2) or
      (not bHorz1 and bHorz2) then
      begin
        //SysUtils.Beep;
        UpdateScrollbars(false);
      end;

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
  si.fMask:= SIF_ALL;// or SIF_DISABLENOSCROLL;
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
    Result:= Rect(0, 0, 0, 0);
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
    Result:= Rect(0, 0, 0, 0);
end;

function TATSynEdit.GetRectGutter: TRect;
begin
  if FGutterVisible then
  begin
    Result.Left:= 0;
    Result.Top:= IfThen(FRulerVisible, FTextOffset.Y);
    Result.Right:= FGutter.Width;
    Result.Bottom:= ClientHeight;
  end
  else
    Result:= Rect(0, 0, 0, 0);
end;

function TATSynEdit.GetRectRuler: TRect;
begin
  if FRulerVisible then
  begin
    Result.Left:= 0;
    Result.Right:= FRectMain.Right;
    Result.Top:= 0;
    Result.Bottom:= Result.Top+FRulerHeight;
  end
  else
    Result:= Rect(0, 0, 0, 0);
end;

procedure TATSynEdit.DoPaintTo(C: TCanvas);
begin
  C.Brush.Color:= FColorTextBG;
  C.FillRect(ClientRect);

  C.Font.Assign(Font);
  FCharSize:= GetCharSize(C, FCharSpacingText);
  FCharSizeMinimap:= Point(8, 8);

  if FGutterVisible then UpdateGutterAutosize;
  if FMinimapVisible then UpdateMinimapAutosize(C);

  FTextOffset:= GetTextOffset; //after gutter autosize
  FRectMinimap:= GetRectMinimap;
  FRectMicromap:= GetRectMicromap;
  FRectGutter:= GetRectGutter;
  FRectMain:= GetRectMain; //after gutter/minimap
  FRectRuler:= GetRectRuler; //after main

  UpdateWrapInfo;

  if FRulerVisible then
    DoPaintRulerTo(C);
  DoPaintTextTo(C, FRectMain, FCharSize, FGutterVisible, FUnprintedVisible, FScrollHorz, FScrollVert);
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
  NGutterFoldX1, NGutterFoldX2,
  NGutterEmptyX1, NGutterEmptyX2,
  NGutterStateX1, NGutterStateX2,
  NCoordTop, NCoordLeftNums: integer;
  NWrapIndex, NLinesIndex: integer;
  NOutputCharsSkipped: integer;
  NOutputSpacesSkipped: real;
  WrapItem: TATSynWrapItem;
  BmKind: TATLineBookmark;
  BmColor: TColor;
  Str, StrOut, StrOutUncut: atString;
  CurrPoint: TPoint;
  LineWithCaret: boolean;
  Parts: TATLineParts;
  //
  procedure DoPaintState(ATop: integer; AColor: TColor);
  begin
    C.Brush.Color:= AColor;
    C.FillRect(NGutterStateX1, ATop, NGutterStateX2, ATop+ACharSize.Y);
  end;
  //
begin
  C.Brush.Color:= FColorTextBG;
  C.FillRect(ARect);

  if AWithGutter then
  begin
    with FGutter[FGutterBandBm] do begin NGutterBmX1:= Left; NGutterBmX2:= Right; end;
    with FGutter[FGutterBandNum] do begin NGutterNumsX1:= Left; NGutterNumsX2:= Right; end;
    with FGutter[FGutterBandState] do begin NGutterStateX1:= Left; NGutterStateX2:= Right; end;
    with FGutter[FGutterBandFold] do begin NGutterFoldX1:= Left; NGutterFoldX2:= Right; end;
    with FGutter[FGutterBandEmpty] do begin NGutterEmptyX1:= Left; NGutterEmptyX2:= Right; end;

    C.Brush.Color:= FColorGutterBG;
    C.FillRect(FRectGutter);

    if FGutter[FGutterBandEmpty].Visible then
    begin
      C.Brush.Color:= FColorTextBG;
      C.FillRect(NGutterEmptyX1, FRectGutter.Top, NGutterEmptyX2, FRectGutter.Bottom);
      C.Brush.Color:= FColorGutterBG;
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
      C.Pen.Color:= FColorCollapsedLine;
      C.MoveTo(ARect.Left, NCoordTop-1);
      C.LineTo(ARect.Right, NCoordTop-1);
    end;

    //prepare line
    Str:= Strings.Lines[NLinesIndex];
    Str:= Copy(Str, WrapItem.NCharIndex, WrapItem.NLength);
    LineWithCaret:= IsLineWithCaret(NLinesIndex);

    StrOut:= StringOfChar(' ', WrapItem.NIndent) + Str;
    StrOutUncut:= StrOut;
    AScrollHorz.NMax:= Max(AScrollHorz.NMax,
      Round(CanvasTextSpaces(StrOutUncut, FTabSize)) + cSizeScrollHorzKeep);

    CurrPoint.X:= ARect.Left;
    CurrPoint.Y:= NCoordTop;

    C.Brush.Color:= FColorTextBG;
    C.Font.Color:= FColorTextFont;

    //draw bookmark bg
    BmColor:= clNone;
    BmKind:= Strings.LinesBm[NLinesIndex];
    if BmKind<>cBmNone then
      BmColor:= Strings.LinesBmColor[NLinesIndex];
    if FShowCurLine and LineWithCaret then
      BmColor:= FColorCurLineBG;

    if BmColor<>clNone then
    begin
      C.Brush.Color:= BmColor;
      C.FillRect(ARect.Left, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y);
    end;

    if WrapItem.NFinal=cWrapItemFinal then
      StrOut:= StrOut+' '; //to hilite eol

    //draw line
    if StrOut<>'' then
    begin
      SFindOutputSkipPosition(StrOut, FTabSize, AScrollHorz.NPos, NOutputCharsSkipped, NOutputSpacesSkipped);
      Delete(StrOut, 1, NOutputCharsSkipped);
      Delete(StrOut, cMaxCharsForOutput, MaxInt);

      DoCalcLineHilite(WrapItem, Parts{%H-},
        NOutputCharsSkipped, cMaxCharsForOutput,
        IfThen(BmColor<>clNone, BmColor, FColorTextBG));

      CanvasTextOut(C,
        CurrPoint.X - AScrollHorz.NPos*ACharSize.X + Trunc(NOutputSpacesSkipped*ACharSize.X),
        CurrPoint.Y,
        StrOut,
        FTabSize,
        ACharSize,
        FUnprintedReplaceSpec,
        Trunc(NOutputSpacesSkipped), //todo:
          //needed number of chars of all chars counted as 1.0,
          //while NOutputSpacesSkipped is with cjk counted as 1.7
        @Parts
        );

      //draw unprintable spaces/tabs
      if AWithUnprintable then
        if FUnprintedSpaces then
          DoPaintUnprintedChars(C, Str, CurrPoint, ARect, ACharSize,
            (WrapItem.NIndent - AScrollHorz.NPos)*ACharSize.X);
    end;

    //draw collapsed-mark
    if AWithUnprintable then
      if WrapItem.NFinal=cWrapItemCollapsed then
        DoPaintCollapseMark(C, Str, CurrPoint, AScrollHorz.NPos);

    //draw unprintable eol
    if AWithUnprintable then
      if FUnprintedEnds then
        DoPaintUnprintedEol(C, WrapItem, CurrPoint,
          CanvasTextWidth(StrOutUncut, FTabSize, ACharSize) - AScrollHorz.NPos*ACharSize.X);

    //draw gutter
    if AWithGutter then
    begin
      C.Brush.Color:= FColorGutterBG;
      C.FillRect(FRectGutter.Left, NCoordTop, FRectGutter.Right, NCoordTop+ACharSize.Y);

      //gutter band: number
      if FGutter[FGutterBandNum].Visible then
      begin
        if LineWithCaret then
        begin
          C.Brush.Color:= FColorGutterCaretBG;
          C.FillRect(NGutterNumsX1, NCoordTop, NGutterNumsX2, NCoordTop+ACharSize.Y);
        end;

        if FWrapInfo.IsItemInitial(NWrapIndex) then
        begin
          C.Font.Color:= FColorGutterFont;
          C.FillRect(NGutterNumsX1, NCoordTop, NGutterNumsX2, NCoordTop+ACharSize.Y);

          Str:= DoFormatLineNumber(NLinesIndex+1);
          NCoordLeftNums:= NGutterNumsX2 - CanvasTextWidth(Str, FTabSize, ACharSize) - cSizeGutterNumOffsetRight;
          CanvasTextOut(C, NCoordLeftNums, NCoordTop, Str, FTabSize, ACharSize, false);
        end;
      end;

      //gutter band: bookmark
      if FGutter[FGutterBandBm].Visible then
        if FWrapInfo.IsItemInitial(NWrapIndex) then
        begin
          if Strings.LinesBm[NLinesIndex]<>cBmNone then
            DoEventDrawBookmarkIcon(C, NLinesIndex, Rect(NGutterBmX1, NCoordTop, NGutterBmX2, NCoordTop+ACharSize.Y));
        end;

      //gutter band: state
      if FGutter[FGutterBandState].Visible then
      begin
        case Strings.LinesState[WrapItem.NLineIndex] of
          cLineStateChanged: DoPaintState(NCoordTop, FColorStateChanged);
          cLineStateAdded: DoPaintState(NCoordTop, FColorStateAdded);
          cLineStateSaved: DoPaintState(NCoordTop, FColorStateSaved);
        end;
      end;

      //gutter band: empty indent
      if FGutter[FGutterBandEmpty].Visible then
      begin
        C.Brush.Color:= FColorTextBG;
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
    CanvasInvertRect(C, R, FColorMinimapSel);
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

  if FColorMinimapBorder<>clNone then
  begin
    C.Pen.Color:= FColorMinimapBorder;
    C.MoveTo(FRectMinimap.Left-1, FRectMinimap.Top);
    C.LineTo(FRectMinimap.Left-1, FRectMinimap.Bottom);
  end;
end;

procedure TATSynEdit.DoPaintMicromapTo(C: TCanvas);
begin
  C.Brush.Color:= cInitColorMicromapBG;
  C.FillRect(FRectMicromap);
end;

procedure TATSynEdit.DoPaintMarginLineTo(C: TCanvas; AX: integer);
begin
  if (AX>=FRectMain.Left) and (AX<FRectMain.Right) then
  begin
    C.Pen.Color:= FColorMargins;
    C.MoveTo(AX, FRectMain.Top);
    C.LineTo(AX, FRectMain.Bottom);
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
    DoPaintMarginLineTo(C, PosX(FMarginRight));
  for i:= 0 to FMarginList.Count-1 do
    DoPaintMarginLineTo(C, PosX(integer(FMarginList[i])));
end;


procedure TATSynEdit.DoPaintCollapseMark(C: TCanvas; const Str: atString;
  ALineStart: TPoint; AScrollPos: integer);
var
  SMark: atString;
  NSize: integer;
  ACharSize: TPoint;
begin
  Dec(ALineStart.X, FCharSize.X*AScrollPos);
  Inc(ALineStart.X, CanvasTextWidth(Str, FTabSize, FCharSize));
  Inc(ALineStart.X, cSizeCollapseMarkIndent);

  //paint text
  C.Font.Color:= FColorCollapsedText;
  C.Brush.Color:= FColorTextBG;

  SMark:= '...';
  ACharSize:= GetCharSize(C, Point(0, 0));
  CanvasTextOut(C, ALineStart.X+cSizeCollapseMarkIndent, ALineStart.Y, SMark, FTabSize, ACharSize, false);
  NSize:= CanvasTextWidth(SMark, FTabSize, ACharSize) + 2*cSizeCollapseMarkIndent;

  //paint frame
  C.Pen.Color:= FColorCollapsedText;
  C.Brush.Style:= bsClear;
  C.Rectangle(ALineStart.X, ALineStart.Y, ALineStart.X+NSize, ALineStart.Y+FCharSize.Y);
end;

procedure TATSynEdit.DoPaintUnprintedChars(C: TCanvas;
  const Str: atString;
  ALineStart: TPoint;
  const AVisibleRect: TRect;
  ACharSize: TPoint;
  AOffsetX: integer);
var
  List: array of real;
  R: TRect;
  i: integer;
begin
  if Str='' then Exit;
  Inc(ALineStart.X, AOffsetX);

  SetLength(List, Length(Str));
  SCalcCharOffsets(Str, List, FTabSize);

  for i:= 1 to Length(Str) do
    if (Str[i]=' ') or (Str[i]=#9) then
    begin
      R.Left:= ALineStart.X;
      R.Right:= ALineStart.X;
      if i>1 then
        Inc(R.Left, Round(ACharSize.X*List[i-2]));
      Inc(R.Right, Round(ACharSize.X*List[i-1]));

      R.Top:= ALineStart.Y;
      R.Bottom:= R.Top+ACharSize.Y;

      if R.Right<AVisibleRect.Left then Continue;
      if R.Left>AVisibleRect.Right then Continue;

      if Str[i]=' ' then
        DoPaintUnprintedSpace(C, R, cUnprintedDotScale)
      else
        DoPaintUnprintedTabulation(C, R);
    end;
end;

procedure TATSynEdit.DoPaintUnprintedEol(C: TCanvas;
  const AItem: TATSynWrapItem;
  ALineStart: TPoint;
  AOffsetX: integer);
var
  NPosX, NPosY, NPrevSize: integer;
  Eol: TATLineEnds;
  StrEol: atString;
  ACharSize: TPoint;
begin
  if AItem.NFinal<>cWrapItemFinal then Exit;

  Eol:= Strings.LinesEnds[AItem.NLineIndex];
  StrEol:= cLineEndNiceNames[Eol];
  if StrEol='' then Exit;

  NPosX:= ALineStart.X + AOffsetX;
  NPosY:= ALineStart.Y;

  if FUnprintedEndsDetails then
  begin
    NPrevSize:= C.Font.Size;
    C.Font.Size:= Trunc(C.Font.Size*cUnprintedFontScale);
    ACharSize:= GetCharSize(C, Point(0, 0));

    C.Font.Color:= FColorUnprintedFont;
    C.Brush.Color:= FColorUnprintedBG;

    Inc(NPosX, cUnprintedFontOffsetX);
    Inc(NPosY, cUnprintedFontOffsetY);
    CanvasTextOut(C, NPosX, NPosY, StrEol, FTabSize, ACharSize, false);

    C.Font.Size:= NPrevSize;
  end
  else
  begin
    DoPaintUnprintedSpace(C, Rect(NPosX, NPosY, NPosX+FCharSize.X, NPosY+FCharSize.Y), cUnprintedEndScale);
  end;
end;

procedure TATSynEdit.DoPaintUnprintedSpace(C: TCanvas; const ARect: TRect; AScale: integer);
var
  R: TRect;
  NSize: integer;
begin
  NSize:= Max(cUnprintedDotMinSize, (ARect.Bottom-ARect.Top) div AScale);
  R.Left:= (ARect.Left+ARect.Right) div 2 - NSize div 2;
  R.Top:= (ARect.Top+ARect.Bottom) div 2 - NSize div 2;
  R.Right:= R.Left + NSize;
  R.Bottom:= R.Top + NSize;
  C.Pen.Color:= FColorUnprintedFont;
  C.Brush.Color:= FColorUnprintedFont;
  C.FillRect(R);
end;

procedure TATSynEdit.DoPaintUnprintedTabulation(C: TCanvas; const ARect: TRect);
const
  cInd = 1; //offset left/rt
  cDiv = 4; //part 1/N of size-y
var
  X, X2, Y, Dx: integer;
begin
  X:= ARect.Right-cInd;
  X2:= ARect.Left+cInd;
  Y:= (ARect.Top+ARect.Bottom) div 2;
  Dx:= (ARect.Bottom-ARect.Top) div cDiv;
  C.Pen.Color:= FColorUnprintedFont;
  C.MoveTo(X, Y);
  C.LineTo(X2, Y);
  C.MoveTo(X, Y);
  C.LineTo(X-Dx, Y-Dx);
  C.MoveTo(X, Y);
  C.LineTo(X-Dx, Y+Dx);
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
    Result:= Result+IntToStr(integer(FMarginList[i]))+' ';
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
  Font.Size:= 9;

  FCarets:= TATCarets.Create;
  FCarets.Add(0, 0);
  FCaretShown:= false;
  FCaretShape:= cInitCaretShape;
  FCaretShapeOvr:= cCaretShapeFull;
  FCaretVirtual:= true;
  FCaretSpecPos:= false;
  FCaretMoveByRtClick:= true;

  FPaintStatic:= false;
  FPaintFlags:= [cPaintUpdateBitmap, cPaintUpdateScrollbars];

  FKeyNavigateInWrappedLines:= true;
  FOver:= false;
  FUseOverOnPaste:= false;

  FColorTextBG:= cInitColorTextBG;
  FColorTextFont:= cInitColorTextFont;
  FColorTextSel:= clHighlightText;
  FColorTextSelBG:= clHighlight;
  FColorCaret:= cInitColorCaret;
  FColorGutterFont:= cInitColorGutterFont;
  FColorGutterBG:= cInitColorGutterBG;
  FColorGutterCaretBG:= cInitColorGutterCaretBG;
  FColorCurLineBG:= cInitColorCurLineBG;
  FColorRulerBG:= cInitColorGutterBG;
  FColorRulerMark:= cInitColorRulerMark;
  FColorCollapsedLine:= cInitColorCollapsedLine;
  FColorCollapsedText:= cInitColorCollapsedText;
  FColorMargins:= cInitColorMargins;
  FColorUnprintedFont:= cInitColorUnprintedFont;
  FColorUnprintedBG:= cInitColorUnprintedBG;
  FColorMinimapBorder:= cInitColorMinimapBorder;
  FColorMinimapSel:= cInitColorMinimapSel;
  FColorStateChanged:= cInitColorStateChanged;
  FColorStateAdded:= cInitColorStateAdded;
  FColorStateSaved:= cInitColorStateSaved;

  FTimerBlink:= TTimer.Create(Self);
  FTimerBlink.Interval:= cInitTimerBlink;
  FTimerBlink.OnTimer:= TimerBlinkTick;

  FTimerScroll:= TTimer.Create(Self);
  FTimerScroll.Interval:= cInitTimerScroll;
  FTimerScroll.OnTimer:= TimerScrollTick;
  FTimerScroll.Enabled:= false;

  FBitmap:= Graphics.TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= cInitBitmapWidth;
  FBitmap.Height:= cInitBitmapHeight;

  FStringsInt:= TATStrings.Create;
  FStringsExt:= nil;

  FWrapInfo:= TATSynWrapInfo.Create;
  FWrapUpdateNeeded:= true;
  FWrapMode:= cWrapOn;
  FWrapColumn:= cInitMarginRight;
  FWrapIndented:= true;

  FTabSize:= cInitTabSize;
  FMarginRight:= cInitMarginRight;
  FMarginList:= TList.Create;

  FUnprintedVisible:= true;
  FUnprintedSpaces:= true;
  FUnprintedEnds:= true;
  FUnprintedEndsDetails:= true;
  FUnprintedReplaceSpec:= true;

  FGutter:= TATGutter.Create;
  FGutterVisible:= true;

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

  FRulerHeight:= cSizeRulerHeight;
  FRulerMarkSizeSmall:= cSizeRulerMarkSmall;
  FRulerMarkSizeBig:= cSizeRulerMarkBig;
  FRulerFontSize:= 8;
  FRulerVisible:= true;

  FNumbersFontSize:= cInitNumbersFontSize;
  FNumbersStyle:= cInitNumbersStyle;

  FMinimapWidth:= cInitMinimapWidth;
  FMinimapFontSize:= cInitMinimapFontSize;
  FMinimapVisible:= cInitMinimapVisible;
  FMicromapWidth:= cInitMicromapWidth;
  FMicromapVisible:= cInitMicromapVisible;

  FCharSpacingText:= Point(0, cInitSpacingText);
  FCharSpacingMinimap:= Point(0, cInitSpacingMinimap);

  FTextAutoIndent:= true;
  FTextTabSpaces:= false;
  FShowCurLine:= false;
  FShowCurColumn:= false;

  DoClearScrollInfo(FScrollHorz);
  DoClearScrollInfo(FScrollVert);

  FKeyMapping:= TATKeyMapping.Create;
  DoInitDefaultKeymapping(FKeyMapping);
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
  inherited;
end;

procedure TATSynEdit.Update(
  AUpdateWrapInfo: boolean = false;
  AUpdateCaretsCoords: boolean = true);
begin
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

  Strings.LoadFromFile(AFilename);

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

procedure TATSynEdit.SetCaretsTime(AValue: integer);
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
    FMarginList.Add(Pointer(N));
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
  i: integer;
begin
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
  Result.X:= cOffsetTextLeft;
  if FGutterVisible then
    Inc(Result.X, FGutter.Width);

  Result.Y:= cOffsetTextTop;
  if FRulerVisible then
    Inc(Result.Y, FRulerHeight);
end;

function TATSynEdit.GetGutterNumbersColumnWidth: integer;
var
  Str: atString;
begin
  Str:= IntToStr(Max(10, Strings.Count));
  Result:=
    Length(Str)*FCharSize.X+
    cSizeGutterNumOffsetLeft+
    cSizeGutterNumOffsetRight;
end;

procedure TATSynEdit.DoPaint(AFlags: TATSynPaintFlags);
var
  ARect: TRect;
begin
  if not Assigned(FBitmap) then Exit;

  if cPaintUpdateBitmap in AFlags then
  begin
    DoPaintTo(FBitmap.Canvas);

    if cPaintUpdateCaretsCoords in AFlags then
    begin
      UpdateCaretsCoords;
      if FShowCurColumn and (Carets.Count>0) then
        DoPaintMarginLineTo(FBitmap.Canvas, Carets[0].CoordX);
    end;

    DoPaintCarets(FBitmap.Canvas, false);
  end;

  ARect:= Canvas.ClipRect;
  Canvas.CopyRect(ARect, FBitmap.Canvas, ARect);

  if cPaintUpdateScrollbars in AFlags then
    UpdateScrollbars;
end;


procedure TATSynEdit.Paint;
begin
  DoPaint(FPaintFlags);
  Exclude(FPaintFlags, cPaintUpdateBitmap);
end;

procedure TATSynEdit.DoOnResize;
begin
  inherited;

  if Assigned(FBitmap) then
  begin
    FBitmap.Width:= Max(FBitmap.Width, ((Width div cSizeBitmapStep)+1)*cSizeBitmapStep);
    FBitmap.Height:= Max(FBitmap.Height, ((Height div cSizeBitmapStep)+1)*cSizeBitmapStep);
  end;

  Invalidate;
end;

{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATSynEdit.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

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
{
not needed so deled:
if not UpdateScrollInfoFromMessage(Msg, FScrollVert) then
  Exclude(FPaintFlags, cPaintUpdateScrollbars);
}
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
  FCaretSpecPos:= false;

  PCaret:= ClientPosToCaretPos(Point(X, Y));
  FMouseDownNumber:= -1;

  if PtInRect(FRectMain, Point(X, Y)) then
  begin
    FMouseDownPnt:= PCaret;

    if Shift=[ssLeft] then
    begin
      FCarets.Clear;
      FCarets.Add(FMouseDownPnt.X, FMouseDownPnt.Y);
    end;

    if Shift=[ssLeft, ssCtrl] then
    begin
      DoCaretAddToPoint(FMouseDownPnt);
    end;

    if Shift=[ssLeft, ssCtrl, ssShift] then
    begin
      DoCaretsColumnToPoint(FMouseDownPnt);
    end;

    if Shift=[ssRight] then
    begin
      if FCaretMoveByRtClick then
      begin
        FCarets.Clear;
        FCarets.Add(FMouseDownPnt.X, FMouseDownPnt.Y);
      end;
    end;
  end;

  if PtInRect(FRectGutter, Point(X, Y)) then
  begin
    //handle click on numbers here
    if (X>=FGutter[FGutterBandNum].Left) and
      (X<FGutter[FGutterBandNum].Right) then
    begin
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

  FMouseDownPnt:= Point(-1, -1);
  FMouseDownNumber:= -1;

  FTimerScroll.Enabled:= false;
end;

procedure TATSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  RectBm, RectNums: TRect;
  nIndex: integer;
begin
  inherited;

  P:= Point(X, Y);

  RectBm.Left:= FGutter[FGutterBandBm].Left;
  RectBm.Right:= FGutter[FGutterBandBm].Right;
  RectBm.Top:= FRectMain.Top;
  RectBm.Bottom:= FRectMain.Bottom;

  RectNums.Left:= FGutter[FGutterBandNum].Left;
  RectNums.Right:= FGutter[FGutterBandNum].Right;
  RectNums.Top:= FRectMain.Top;
  RectNums.Bottom:= FRectMain.Bottom;

  //update cursor
  if PtInRect(FRectMain, P) then
    Cursor:= crIBeam
  else
  if PtInRect(RectBm, P) then
    Cursor:= crHandPoint
  else
    Cursor:= crDefault;

  //start scroll timer
  FTimerScroll.Enabled:= (ssLeft in Shift) and (not PtInRect(FRectMain, P));
  FMouseAutoScroll:= cScrollNone;
  if P.Y<FRectMain.Top then FMouseAutoScroll:= cScrollUp else
  if P.Y>=FRectMain.Bottom then FMouseAutoScroll:= cScrollDown else
  if P.X<FRectMain.Left then FMouseAutoScroll:= cScrollLeft else
  if P.X>=FRectMain.Right then FMouseAutoScroll:= cScrollRight;

  //mouse dragged on numbers
  if PtInRect(RectNums, P) then
    if ssLeft in Shift then
    begin
      P:= ClientPosToCaretPos(P);
      if (P.Y>=0) and (P.X>=0) then
        if FMouseDownNumber>=0 then
        begin
          DoSelect_Lines_ToPoint(FMouseDownNumber, P);
          DoCaretsSort;
          DoEventCarets;
          Update;
        end;
    end;

  //mouse dragged on text
  if PtInRect(FRectMain, P) then
  begin
    if ssLeft in Shift then
      if Carets.Count>0 then
      begin
        P:= ClientPosToCaretPos(P);
        if P.Y>=0 then
        begin
          //drag w/out button pressed: single selection
          if [ssCtrl, ssShift, ssAlt]*Shift=[] then
          begin
            DoCaretSingleAsIs;
            Carets.SelectToPoint(0, P.X, P.Y);
          end;

          //drag with Ctrl pressed: add selection
          if [ssCtrl, ssShift, ssAlt]*Shift=[ssCtrl] then
          begin
            nIndex:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
            if nIndex>=0 then
              Carets.SelectToPoint(nIndex, P.X, P.Y);
          end;

          DoCaretsSort;
          DoEventCarets;
          Update;
        end;
      end;
  end;
end;

procedure TATSynEdit.DblClick;
var
  P: TPoint;
begin
  inherited;

  P:= ScreenToClient(Mouse.CursorPos);
  if PtInRect(FRectMain, P) then
  begin
    P:= ClientPosToCaretPos(P);
    if P.Y<0 then Exit;
    DoSelect_Word(P);
    Update;
  end;
end;

procedure TATSynEdit.TripleClick;
var
  P: TPoint;
begin
  inherited;

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

  case FMouseAutoScroll of
    cScrollUp:
      begin
        PClient.Y:= FRectMain.Top;
        DoScrollByDelta(0, -cSizeScrollVertAuto);
      end;
    cScrollDown:
      begin
        PClient.Y:= FRectMain.Bottom;
        DoScrollByDelta(0, cSizeScrollVertAuto);
      end;
    cScrollLeft:
      begin
        PClient.X:= FRectMain.Left;
        DoScrollByDelta(-cSizeScrollHorzAuto, 0);
      end;
    cScrollRight:
      begin
        PClient.X:= FRectMain.Right;
        DoScrollByDelta(cSizeScrollHorzAuto, 0);
      end;
    else
      Exit;
  end;

  PCaret:= ClientPosToCaretPos(PClient);
  if (PCaret.X>=0) and (PCaret.Y>=0) then
  begin
    nIndex:= Carets.IndexOfPosXY(FMouseDownPnt.X, FMouseDownPnt.Y, true);
    if nIndex>=0 then
      Carets.SelectToPoint(nIndex, PCaret.X, PCaret.Y);
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
      CanvasInvertRect(C, R, FColorCaret);
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

function TATSynEdit.GetCaretsTime: integer;
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



{$I atsynedit_carets.inc}
{$I atsynedit_hilite.inc}
{$I atsynedit_sel.inc}

{$I atsynedit_cmd_handler.inc}
{$I atsynedit_cmd_keys.inc}
{$I atsynedit_cmd_editing.inc}
{$I atsynedit_cmd_clipboard.inc}
{$I atsynedit_cmd_misc.inc}

end.

