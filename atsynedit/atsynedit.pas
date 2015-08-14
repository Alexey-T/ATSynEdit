{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

{$mode objfpc}{$H+}

//{$define beep_wrapinfo}
//{$define debug_findwrapindex}
//{$define beep_cached_update}
//{$define test_foldlist}
//{$define allow_proc_msg}

unit ATSynEdit;

interface

uses
  Classes, SysUtils, Graphics,
  Controls, ExtCtrls, Menus, Forms,
  LMessages, LCLType,
  ATStringProc,
  ATStrings,
  ATSynEdit_Keymap,
  ATSynEdit_CanvasProc,
  ATSynEdit_Carets,
  ATSynEdit_Gutter,
  ATSynEdit_WrapInfo,
  ATSynEdit_Ranges,
  ATSynEdit_Adapters;

type
  TATDirection = (
    cDirNone,
    cDirLeft,
    cDirRight,
    cDirUp,
    cDirDown
    );

  TATCaseConvert = (
    cCaseLower,
    cCaseUpper,
    cCaseTitle,
    cCaseInvert,
    cCaseSentence
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

  TATFoldStyle = ( //affects folding of blocks without "text hint" passed from adapter
    cFoldHereWithDots, //show "..." from fold-pos
    cFoldHereWithTruncatedText, //show truncated line instead of "..."
    cFoldFromEndOfLine, //looks like Lazarus: show "..." after line, bad with 2 blocks starting at the same line
    cFoldFromNextLine //looks like SynWrite: don't show "...", show separator line
    );

type
  TATSynEditColors = class(TPersistent)
  private
    FTextFont,
    FTextBG,
    FTextDisabledFont,
    FTextDisabledBG,
    FTextSelFont,
    FTextSelBG,
    FCaret,
    FGutterFont,
    FGutterBG,
    FGutterCaretBG,
    FGutterPlusBorder,
    FGutterPlusBG,
    FGutterFoldLine,
    FGutterFoldBG,
    FGutterSeparatorBG,
    FCurrentLineBG,
    FMarginRight,
    FMarginCaret,
    FMarginUser,
    FIndentVertLines,
    FRulerFont,
    FRulerBG,
    FCollapseLine,
    FCollapseMarkFont,
    FCollapseMarkBG,
    FUnprintedFont,
    FUnprintedBG,
    FUnprintedHexFont,
    FMinimapBorder,
    FMinimapSelBG,
    FStateChanged,
    FStateAdded,
    FStateSaved,
    FTextHintFont,
    FBlockStaple,
    FBlockSepLine,
    FLockedBG,
    FComboboxArrow,
    FComboboxArrowBG: TColor;
  published
    property TextFont: TColor read FTextFont write FTextFont;
    property TextBG: TColor read FTextBG write FTextBG;
    property TextDisabledFont: TColor read FTextDisabledFont write FTextDisabledFont;
    property TextDisabledBG: TColor read FTextDisabledBG write FTextDisabledBG;
    property TextSelFont: TColor read FTextSelFont write FTextSelFont;
    property TextSelBG: TColor read FTextSelBG write FTextSelBG;
    property Caret: TColor read FCaret write FCaret;
    property GutterFont: TColor read FGutterFont write FGutterFont;
    property GutterBG: TColor read FGutterBG write FGutterBG;
    property GutterCaretBG: TColor read FGutterCaretBG write FGutterCaretBG;
    property GutterPlusBorder: TColor read FGutterPlusBorder write FGutterPlusBorder;
    property GutterPlusBG: TColor read FGutterPlusBG write FGutterPlusBG;
    property GutterFoldLine: TColor read FGutterFoldLine write FGutterFoldLine;
    property GutterFoldBG: TColor read FGutterFoldBG write FGutterFoldBG;
    property GutterSeparatorBG: TColor read FGutterSeparatorBG write FGutterSeparatorBG;
    property CurrentLineBG: TColor read FCurrentLineBG write FCurrentLineBG;
    property MarginRight: TColor read FMarginRight write FMarginRight;
    property MarginCaret: TColor read FMarginCaret write FMarginCaret;
    property MarginUser: TColor read FMarginUser write FMarginUser;
    property IndentVertLines: TColor read FIndentVertLines write FIndentVertLines;
    property RulerFont: TColor read FRulerFont write FRulerFont;
    property RulerBG: TColor read FRulerBG write FRulerBG;
    property CollapseLine: TColor read FCollapseLine write FCollapseLine;
    property CollapseMarkFont: TColor read FCollapseMarkFont write FCollapseMarkFont;
    property CollapseMarkBG: TColor read FCollapseMarkBG write FCollapseMarkBG;
    property UnprintedFont: TColor read FUnprintedFont write FUnprintedFont;
    property UnprintedBG: TColor read FUnprintedBG write FUnprintedBG;
    property UnprintedHexFont: TColor read FUnprintedHexFont write FUnprintedHexFont;
    property MinimapBorder: TColor read FMinimapBorder write FMinimapBorder;
    property MinimapSelBG: TColor read FMinimapSelBG write FMinimapSelBG;
    property StateChanged: TColor read FStateChanged write FStateChanged;
    property StateAdded: TColor read FStateAdded write FStateAdded;
    property StateSaved: TColor read FStateSaved write FStateSaved;
    property BlockStaple: TColor read FBlockStaple write FBlockStaple;
    property BlockSepLine: TColor read FBlockSepLine write FBlockSepLine;
    property LockedBG: TColor read FLockedBG write FLockedBG;
    property TextHintFont: TColor read FTextHintFont write FTextHintFont;
    property ComboboxArrow: TColor read FComboboxArrow write FComboboxArrow;
    property ComboboxArrowBG: TColor read FComboboxArrowBG write FComboboxArrowBG;
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

  TATSynWrapMode = (
    cWrapOff,
    cWrapOn,
    cWrapAtMargin
    );

  TATSynNumbersStyle = (
    cNumbersAll,
    cNumbersNone,
    cNumbersEach10th,
    cNumbersEach5th
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
    NPos,
    NPosLast: integer;
  end;

  TATSynCaretShape = (
    cCaretShapeFull,
    cCaretShapeVertPixels1,
    cCaretShapeVertPixels2,
    cCaretShapeVertPixels3,
    cCaretShapeVertPixels4,
    cCaretShapeVertPercents10,
    cCaretShapeVertPercents15,
    cCaretShapeVertPercents20,
    cCaretShapeVertPercents25,
    cCaretShapeVertPercents30,
    cCaretShapeVertPercents35,
    cCaretShapeVertPercents40,
    cCaretShapeVertPercents50,
    cCaretShapeHorzPixels1,
    cCaretShapeHorzPixels2,
    cCaretShapeHorzPixels3,
    cCaretShapeHorzPixels4,
    cCaretShapeHorzPixels5,
    cCaretShapeHorzPercents10,
    cCaretShapeHorzPercents15,
    cCaretShapeHorzPercents20,
    cCaretShapeHorzPercents25,
    cCaretShapeHorzPercents30,
    cCaretShapeHorzPercents35,
    cCaretShapeHorzPercents40,
    cCaretShapeHorzPercents50,
    cCaretShapeFrameFull
    );

const
  cInitCaretShapeIns = cCaretShapeVertPixels1;
  cInitCaretShapeOvr = cCaretShapeFull;
  cInitCaretShapeRO = cCaretShapeHorzPixels1;
  cInitTextOffsetFromLine = {$ifdef windows} 0 {$else} 1 {$endif};
  cInitSpacingText = 1;
  cInitTimerBlink = 600;
  cInitTimerAutoScroll = 80;
  cInitTimerNiceScroll = 200;
  cInitMinimapVisible = false;
  cInitMicromapVisible = false;
  cInitMarginRight = 80;
  cInitTabSize = 8;
  cInitMicromapWidth = 30;
  cInitMinimapWidth = 160;
  cInitNumbersStyle = cNumbersEach5th;
  cInitBitmapWidth = 1000;
  cInitBitmapHeight = 800;
  cInitGutterPlusSize = 4;
  cInitFoldStyle = cFoldHereWithTruncatedText;

  cGutterBands = 6;
  cGutterSizeBm = 16;
  cGutterSizeNum = 10;
  cGutterSizeFold = 14;
  cGutterSizeState = 3;
  cGutterSizeSep = 1;
  cGutterSizeEmpty = 2;

const
  cFoldedLenOfEmptyHint = 50;
  cFoldedMarkIndentInner = 2;
  cFoldedMarkIndentOuter = 0;
  cScrollKeepHorz = 1; //keep char, allow handy clicking after eol of longest line
  cScrollIndentCaretHorz = 10; //offsets for caret-moving: if caret goes out of control
  cScrollIndentCaretVert = 0; //must be 0, >0 gives jumps on move-down
  cScrollIndentGotoHorz = 10; //offsets for "goto" command: if caret goes out of control
  cScrollIndentGotoVert = 3;
  cSpeedScrollAutoHorz = 10; //auto-scroll (drag out of control): speed x
  cSpeedScrollAutoVert = 1; //... speed y
  cSpeedScrollNiceHorz = 4; //browser-scroll (middle-click): speed x
  cSpeedScrollNiceVert = 1; //... speed y
  cResizeBitmapStep = 200; //resize bitmap by N pixels step
  cSizeGutterFoldLineDx = 3;
  cSizeRulerHeight = 20;
  cSizeRulerMarkSmall = 3;
  cSizeRulerMarkBig = 7;
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
  cHintScrollPrefix: string = 'Line';
  cHintScrollDx = 5;

var
  cRectEmpty: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  cClipFormatId: integer = 0; //must be inited
  cClipSignatureColumn: integer = $1000;

type
  TATSynEditClickEvent = procedure(Sender: TObject; var AHandled: boolean) of object;
  TATSynEditCommandEvent = procedure(Sender: TObject; ACommand: integer; var AHandled: boolean) of object;
  TATSynEditClickGutterEvent = procedure(Sender: TObject; ABand: integer; ALineNum: integer) of object;
  TATSynEditClickMicromapEvent = procedure(Sender: TObject; AX, AY: integer) of object;
  TATSynEditDrawBookmarkEvent = procedure(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect) of object;
  TATSynEditDrawRectEvent = procedure(Sender: TObject; C: TCanvas; const ARect: TRect) of object;
  TATSynEditCalcStapleEvent = procedure(Sender: TObject; ALine, AIndent: integer; var AStapleColor: TColor) of object;
  TATSynEditCalcHiliteEvent = procedure(Sender: TObject; var AParts: TATLineParts;
    ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor) of object;


type
  { TATSynEdit }

  TATSynEdit = class(TCustomControl)
  private
    FTimerBlink: TTimer;
    FTimerScroll: TTimer;
    FTimerNiceScroll: TTimer;
    FPaintStatic: boolean;
    FPaintFlags: TATSynPaintFlags;
    FPaintLocked: integer;
    FBitmap: TBitmap;
    FKeymap: TATKeymap;
    FWantTabs: boolean;
    FEditorIndex: integer;
    FMarginRight: integer;
    FMarginList: TList;
    FStringsInt,
    FStringsExternal: TATStrings;
    FAdapterHilite: TATAdapterHilite;
    FFold: TATSynRanges;
    FFoldStyle: TATFoldStyle;
    FCursorText,
    FCursorBm: TCursor;
    FTextOffset: TPoint;
    FTextLocked: string;
    FTextHint: string;
    FTextHintFontStyle: TFontStyles;
    FTextHintCenter: boolean;
    FSelRect: TRect;
    FSelRectBegin,
    FSelRectEnd: TPoint;
    FCarets: TATCarets;
    FCaretBlinkEnabled: boolean;
    FCaretShapeIns,
    FCaretShapeOvr,
    FCaretShapeRO: TATSynCaretShape;
    FCaretShown: boolean;
    FCaretVirtual: boolean;
    FCaretSpecPos: boolean;
    FCaretStopUnfocused: boolean;
    FMenuStd,
    FMenuText,
    FMenuGutterBm,
    FMenuGutterNum,
    FMenuGutterFold,
    FMenuGutterFoldStd,
    FMenuMinimap,
    FMenuMicromap,
    FMenuRuler: TPopupMenu;
    FOverwrite: boolean;
    FHintWnd: THintWindow;
    FMouseDownPnt: TPoint;
    FMouseDownNumber: integer;
    FMouseDownDouble: boolean;
    FMouseDownRight: boolean;
    FMouseNiceScrollPos: TPoint;
    FMouseDragDropping: boolean;
    FMouseAutoScroll: TATDirection;
    FLastTextCmd: integer;
    FLastTextCmdText: atString;
    FCursorOnMinimap: boolean;
    FCursorOnGutter: boolean;
    FOnClickDbl,
    FOnClickTriple,
    FOnClickMiddle: TATSynEditClickEvent;
    FOnChangeCaretPos: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FOnClickGutter: TATSynEditClickGutterEvent;
    FOnClickMicromap: TATSynEditClickMicromapEvent;
    FOnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent;
    FOnDrawLine: TATSynEditDrawLineEvent;
    FOnDrawMicromap: TATSynEditDrawRectEvent;
    FOnDrawEditor: TATSynEditDrawRectEvent;
    FOnDrawRuler: TATSynEditDrawRectEvent;
    FOnChangeState: TNotifyEvent;
    FOnCommand: TATSynEditCommandEvent;
    FOnCalcHilite: TATSynEditCalcHiliteEvent;
    FOnCalcStaple: TATSynEditCalcStapleEvent;
    FWrapInfo: TATSynWrapInfo;
    //FWrapProgress: integer;
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
    FCharSpacingText: TPoint;
    FTabSize: integer;
    FGutter: TATGutter;
    FGutterBandBm,
    FGutterBandNum,
    FGutterBandState,
    FGutterBandFold,
    FGutterBandSep,
    FGutterBandEmpty: integer;
    FColors: TATSynEditColors;
    FRectMain,
    FRectMinimap,
    FRectMicromap,
    FRectGutter,
    FRectRuler: TRect;
    FLineBottom: integer;
    FScrollVert,
    FScrollHorz: TATSynScrollInfo;
    FScrollVertMinimap,
    FScrollHorzMinimap: TATSynScrollInfo;
    FPrevHorz,
    FPrevVert: TATSynScrollInfo;
    FMinimapWidth: integer;
    FMinimapCharWidth: integer;
    FMinimapVisible: boolean;
    FMinimapShowSelBorder: boolean;
    FMinimapShowSelAlways: boolean;
    FMicromapWidth: integer;
    FMicromapVisible: boolean;
    FOptShowStapleStyle: TATLineStyle;
    FOptShowStapleIndent: integer;
    FOptShowStapleWidthPercent: integer;
    FOptMouseEnableNormalSelection: boolean;
    FOptMouseEnableColumnSelection: boolean;
    FOptMouseDownForPopup: boolean;
    FOptCaretPreferLeftSide: boolean;
    FOptShowScrollHint: boolean;
    FOptTextOffsetTop: integer;
    FOptTextOffsetFromLine: integer;
    FOptSavingForceFinalEol: boolean;
    FOptSavingTrimSpaces: boolean;
    FOptUndoGrouped: boolean;
    FOptIndentSize: integer;
    FOptIndentKeepsAlign: boolean;
    FOptRulerVisible: boolean;
    FOptRulerSize: integer;
    FOptRulerFontSize: integer;
    FOptRulerMarkSizeSmall: integer;
    FOptRulerMarkSizeBig: integer;
    FOptRulerTextIndent: integer;
    FOptGutterVisible: boolean;
    FOptGutterPlusSize: integer;
    FOptGutterShowFoldAlways: boolean;
    FOptGutterShowFoldLines: boolean;
    FOptGutterShowFoldLinesAll: boolean;
    FOptNumbersAutosize: boolean;
    FOptNumbersAlignment: TAlignment;
    FOptNumbersFontSize: integer;
    FOptNumbersStyle: TATSynNumbersStyle;
    FOptNumbersShowFirst,
    FOptNumbersShowCarets: boolean;
    FOptNumbersSkippedChar: atString;
    FOptNumbersIndentLeft,
    FOptNumbersIndentRight: integer;
    FOptWordChars: atString;
    FOptAutoIndent: boolean;
    FOptAutoIndentKind: TATAutoIndentKind;
    FOptTabSpaces: boolean;
    FOptLastLineOnTop: boolean;
    FOptOverwriteSel: boolean;
    FOptOverwriteAllowedOnPaste: boolean;
    FOptKeyBackspaceUnindent: boolean;
    FOptKeyPageKeepsRelativePos: boolean;
    FOptKeyUpDownNavigateWrapped: boolean;
    FOptKeyHomeEndNavigateWrapped: boolean;
    FOptKeyUpDownKeepColumn: boolean;
    FOptCopyLinesIfNoSel: boolean;
    FOptCutLinesIfNoSel: boolean;
    FOptShowFullSel: boolean;
    FOptShowFullHilite: boolean;
    FOptShowCurLine: boolean;
    FOptShowCurLineMinimal: boolean;
    FOptShowCurColumn: boolean;
    FOptMouseHideCursor: boolean;
    FOptMouse2ClickSelectsLine: boolean;
    FOptMouse3ClickSelectsLine: boolean;
    FOptMouse2ClickDragSelectsWords: boolean;
    FOptMouseDragDrop: boolean;
    FOptMouseRightClickMovesCaret: boolean;
    FOptMouseGutterClickSelectsLine: boolean;
    FOptMouseNiceScroll: boolean;
    FOptKeyPageUpDownSize: TATPageUpDownSize;
    FOptKeyLeftRightSwapSel: boolean;
    FOptKeyLeftRightSwapSelAndSelect: boolean;
    FOptKeyHomeToNonSpace: boolean;
    FOptKeyEndToNonSpace: boolean;
    FOptKeyTabIndents: boolean;
    FOptShowIndentLines: boolean;
    FOptShowGutterCaretBG: boolean;
    FOptAllowScrollbars: boolean;
    FOptAllowZooming: boolean;
    FOptAllowReadOnly: boolean;
    //
    procedure DebugFindWrapIndex;
    procedure DoCalcPosColor(AX, AY: integer; var AColor: TColor);
    procedure DoCalcLineEntireColor(ALine: integer; ACoordTop: integer;
      ALineWithCaret: boolean; out AColor: TColor; out AColorForced: boolean);
    procedure DoCaretsAssign(NewCarets: TATCarets);
    procedure DoDropText;
    procedure DoFoldbarClick(ALine: integer);
    procedure DoHandleRightClick(X, Y: integer);
    function DoHandleClickEvent(AEvent: TATSynEditClickEvent): boolean;
    procedure DoHintShow;
    procedure DoHintHide;
    procedure DoMenuGutterFold;
    procedure DoMinimapClick(APosY: integer);
    function GetColorTextBG: TColor;
    function GetColorTextFont: TColor;
    function IsFoldLineNeededBeforeWrapitem(N: integer): boolean;
    procedure MenuFoldStdClick(Sender: TObject);
    procedure PaintEx(ALine: integer);
    procedure DoPaintGutterFolding(C: TCanvas; AWrapItemIndex: integer; ACoordX1,
      ACoordX2, ACoordY1, ACoordY2: integer);
    procedure DoPaintGutterBandBG(C: TCanvas; ABand: integer; AColor: TColor; ATop,
      ABottom: integer);
    procedure DoPaintLockedWarning(C: TCanvas);
    procedure DoPaintStaple(C: TCanvas; const R: TRect; AColor: TColor);
    procedure DoPaintStaples(C: TCanvas; const ARect: TRect; ACharSize: TPoint;
      const AScrollHorz: TATSynScrollInfo);
    procedure DoPaintTextHintTo(C: TCanvas);
    procedure DoPartCalc_ApplyOver(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer);
    procedure DoPartCalc_CreateNew(var AParts: TATLineParts; AOffsetMax,
      ALineIndex, ACharIndex: integer; AColorBG: TColor);
    procedure DoUnfoldLine(ALine: integer);
    function GetAutoIndentString(APosX, APosY: integer): atString;
    function GetFirstUnfoldedLineNumber: integer;
    function GetFoldedMarkText(ALine: integer): string;
    function GetLastUnfoldedLineNumber: integer;
    function GetModified: boolean;
    function GetNextUnfoldedLineNumber(ALine: integer; ADown: boolean): integer;
    function GetOneLine: boolean;
    function GetRedoCount: integer;
    function GetLinesFromTop: integer;
    function GetText: atString;
    function GetUndoAfterSave: boolean;
    function GetUndoCount: integer;
    function GetUndoLimit: integer;
    procedure DoInitPopupMenu;
    function IsCaretBlocked: boolean;
    function IsLineFolded(ALine: integer; ADetectPartialFold: boolean = false): boolean;
    function IsLineFoldedFull(ALine: integer): boolean;
    function IsLinePartWithCaret(ALine: integer; ACoordY: integer): boolean;
    procedure MenuClick(Sender: TObject);
    procedure MenuPopup(Sender: TObject);
    procedure DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer; AItems: TList);
    procedure DoCalcLineHilite(const AItem: TATSynWrapItem;
      var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor; AColorForced: boolean; var AColorAfter: TColor);
    //select
    procedure DoSelectionDeleteOrReset;
    procedure DoSelect_ExtendSelectionByLine;
    procedure DoSelect_ColumnBlock(P1, P2: TPoint);
    procedure DoSelect_CharRange(ACaretIndex: integer; Pnt: TPoint);
    procedure DoSelect_WordRange(ACaretIndex: integer; P1, P2: TPoint);
    procedure DoSelect_Word_ByClick;
    procedure DoSelect_Line_ByClick;
    procedure DoSelect_None;
    //paint
    function DoPaint(AFlags: TATSynPaintFlags; ALineFrom: integer): boolean;
    procedure DoPaintNiceScroll(C: TCanvas);
    procedure DoPaintMarginLineTo(C: TCanvas; AX: integer; AColor: TColor);
    procedure DoPaintTo(C: TCanvas; ALineFrom: integer);
    procedure DoPaintRulerTo(C: TCanvas);
    procedure DoPaintTextTo(C: TCanvas; const ARect: TRect;
      const ACharSize: TPoint; AWithGutter, AMainText: boolean;
      var AScrollHorz, AScrollVert: TATSynScrollInfo; ALineFrom: integer);
    procedure DoPaintLineIndent(C: TCanvas; const ARect: TRect; ACharSize: TPoint;
      ACoordY: integer; AIndentSize: integer; AColorBG: TColor;
      AScrollPos: integer; AIndentLines: boolean);
    procedure DoPaintMinimapSelTo(C: TCanvas);
    procedure DoPaintMinimapTo(C: TCanvas);
    procedure DoPaintMicromapTo(C: TCanvas);
    procedure DoPaintMarginsTo(C: TCanvas);
    procedure DoPaintFoldedMark(C: TCanvas; ACoord: TPoint; const AMarkText: string);
    procedure DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
    procedure DoPaintModeStatic;
    procedure DoPaintModeBlinking;
    procedure DoPaintSelectedLineBG(C: TCanvas; ACharSize: TPoint;
      const AVisRect: TRect; APointLeft: TPoint; APointText: TPoint;
      ALineIndex: integer; AEolSelected: boolean;
      const AScrollHorz: TATSynScrollInfo);
    //carets
    procedure DoCaretsExtend(ADown: boolean; ALines: integer);
    function GetCaretManyAllowed: boolean;
    function GetCaretSelectionIndex(P: TPoint): integer;
    function GetCaretBlinkTime: integer;
    function DoCaretSwapEdge(AMoveLeft: boolean): boolean;
    procedure DoCaretsSort;
    //events
    procedure DoEventClickMicromap(AX, AY: integer);
    procedure DoEventClickGutter(ABandIndex, ALineNumber: integer);
    function DoEventCommand(ACommand: integer): boolean;
    procedure DoEventDrawBookmarkIcon(C: TCanvas; ALineNumber: integer; const ARect: TRect);
    //
    function GetCharSpacingX: integer;
    function GetCharSpacingY: integer;
    function GetEndOfFilePos: TPoint;
    function GetMarginString: string;
    function GetReadOnly: boolean;
    function GetLineTop: integer;
    function GetTextForClipboard: string;
    function GetWrapInfoIndex(AMousePos: TPoint): integer;
    function GetStrings: TATStrings;
    function GetMouseNiceScroll: boolean;
    procedure SetCaretShapeRO(AValue: TATSynCaretShape);
    procedure SetCaretBlinkEnabled(AValue: boolean);
    procedure SetMouseNiceScroll(AValue: boolean);
    procedure SetCaretManyAllowed(AValue: boolean);
    procedure SetCaretBlinkTime(AValue: integer);
    procedure SetCaretShapeIns(AValue: TATSynCaretShape);
    procedure SetCaretShapeOvr(AValue: TATSynCaretShape);
    procedure SetCharSpacingX(AValue: integer);
    procedure SetCharSpacingY(AValue: integer);
    procedure SetMarginString(AValue: string);
    procedure SetMicromapVisible(AValue: boolean);
    procedure SetMinimapVisible(AValue: boolean);
    procedure SetOneLine(AValue: boolean);
    procedure SetReadOnly(AValue: boolean);
    procedure SetLineTop(AValue: integer);
    procedure SetLinesFromTop(AValue: integer);
    procedure SetStrings(Obj: TATStrings);
    function GetRectMain: TRect;
    function GetRectMinimap: TRect;
    function GetRectMicromap: TRect;
    function GetRectGutter: TRect;
    function GetRectRuler: TRect;
    function GetTextOffset: TPoint;
    function GetGutterNumbersWidth(C: TCanvas): integer;
    function GetPageLines: integer;
    function GetVisibleLines: integer;
    function GetVisibleColumns: integer;
    function GetVisibleLinesMinimap: integer;
    function GetMinimapScrollPos: integer;
    procedure SetTabSize(AValue: integer);
    procedure SetText(AValue: atString);
    procedure SetUndoAfterSave(AValue: boolean);
    procedure SetUndoLimit(AValue: integer);
    procedure SetWrapMode(AValue: TATSynWrapMode);
    procedure SetWrapIndented(AValue: boolean);
    procedure UpdateCursor;
    procedure UpdateGutterAutosize(C: TCanvas);
    procedure UpdateMinimapAutosize(C: TCanvas);
    function DoFormatLineNumber(N: integer): atString;
    function UpdateScrollInfoFromMessage(const Msg: TLMScroll;
      var Info: TATSynScrollInfo): boolean;
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
    procedure TimerNiceScrollTick(Sender: TObject);

    //carets
    procedure DoCaretAddToPoint(AX, AY: integer);
    procedure DoCaretsColumnToPoint(AX, AY: integer);
    procedure DoCaretsShift(APosX, APosY: integer; AShiftX, AShiftY: integer; AShiftBelowX: integer = 0);
    procedure DoCaretsDeleteOnSameLines;

    //editing
    procedure DoCommandResults(Res: TATCommandResults);
    function DoCommand_TextChangeCase(AMode: TATCaseConvert): TATCommandResults;
    function DoCommand_SizeChange(AIncrease: boolean): TATCommandResults;
    function DoCommand_MoveSelectionUpDown(ADown: boolean): TATCommandResults;
    function DoCommand_TextInsertEmptyAboveBelow(ADown: boolean): TATCommandResults;
    function DoCommand_SelectColumn(ADir: TATDirection): TATCommandResults;
    function DoCommand_TextInsertColumnBlockOnce(const AText: atString; AKeepCaret: boolean): TATCommandResults;
    function DoCommand_CaretsExtend(ADown: boolean; ALines: integer): TATCommandResults;
    function DoCommand_Undo: TATCommandResults;
    function DoCommand_Redo: TATCommandResults;
    function DoCommand_TextIndentUnindent(ARight: boolean): TATCommandResults;
    function DoCommand_SelectWords: TATCommandResults;
    function DoCommand_SelectLines: TATCommandResults;
    function DoCommand_SelectAll: TATCommandResults;
    function DoCommand_SelectInverted: TATCommandResults;
    function DoCommand_SelectSplitToLines: TATCommandResults;
    function DoCommand_SelectExtendByLine: TATCommandResults;
    function DoCommand_Cancel: TATCommandResults;
    function DoCommand_ToggleReadOnly: TATCommandResults;
    function DoCommand_ToggleOverwrite: TATCommandResults;
    function DoCommand_ToggleWordWrap: TATCommandResults;
    function DoCommand_ToggleUnprinted: TATCommandResults;
    function DoCommand_ToggleUnprintedSpaces: TATCommandResults;
    function DoCommand_ToggleUnprintedEnds: TATCommandResults;
    function DoCommand_ToggleUnprintedEndDetails: TATCommandResults;
    function DoCommand_ToggleLineNums: TATCommandResults;
    function DoCommand_ToggleFolding: TATCommandResults;
    function DoCommand_ToggleRuler: TATCommandResults;
    function DoCommand_ToggleMinimap: TATCommandResults;
    function DoCommand_GotoWord(ANext: boolean): TATCommandResults;
    function DoCommand_ScrollVert(ALines: integer): TATCommandResults;
    function DoCommand_TextInsertAtCarets(const AText: atString; AKeepCaret,
      AOvrMode, ASelectThen: boolean): TATCommandResults;
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
    function GetCaretsArray: TATPointArray;
    procedure SetCaretsArray(const L: TATPointArray);
    property MouseNiceScroll: boolean read GetMouseNiceScroll write SetMouseNiceScroll;
    procedure DoDebugInitFoldList;

  public
    //overrides
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    //updates
    procedure Invalidate; override;
    procedure Update(AUpdateWrapInfo: boolean = false; AUpdateCaretsCoords: boolean = true); reintroduce;
    procedure UpdateIncorrectCaretPositions;
    procedure UpdateFoldedFromLinesHidden;
    procedure DoEventCarets;
    procedure DoEventScroll;
    procedure DoEventChange;
    procedure DoEventState;
    //general
    property Strings: TATStrings read GetStrings write SetStrings;
    property Fold: TATSynRanges read FFold;
    property Keymap: TATKeymap read FKeymap write FKeymap;
    property Modified: boolean read GetModified;
    property AdapterHilite: TATAdapterHilite read FAdapterHilite write FAdapterHilite;
    property EditorIndex: integer read FEditorIndex write FEditorIndex;
    property LineTop: integer read GetLineTop write SetLineTop;
    property LineBottom: integer read FLineBottom;
    property LinesFromTop: integer read GetLinesFromTop write SetLinesFromTop;
    property ModeOverwrite: boolean read FOverwrite write FOverwrite;
    property ModeReadOnly: boolean read GetReadOnly write SetReadOnly;
    property ModeOneLine: boolean read GetOneLine write SetOneLine;
    property UndoCount: integer read GetUndoCount;
    property RedoCount: integer read GetRedoCount;
    property Text: atString read GetText write SetText;
    property SelRect: TRect read FSelRect;
    function IsSelRectEmpty: boolean;
    function IsPosSelected(AX, AY: integer): boolean;
    function IsPosFolded(AX, AY: integer): boolean;
    function IsCharWord(ch: Widechar): boolean;
    //gutter
    property Gutter: TATGutter read FGutter;
    property GutterBandBm: integer read FGutterBandBm write FGutterBandBm;
    property GutterBandNum: integer read FGutterBandNum write FGutterBandNum;
    property GutterBandState: integer read FGutterBandState write FGutterBandState;
    property GutterBandFold: integer read FGutterBandFold write FGutterBandFold;
    property GutterBandSep: integer read FGutterBandSep write FGutterBandSep;
    property GutterBandEmpty: integer read FGutterBandEmpty write FGutterBandEmpty;
    //files
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    //carets
    procedure DoCaretSingle(AX, AY: integer);
    procedure DoCaretSingleAsIs;
    function CaretPosToClientPos(P: TPoint): TPoint;
    function ClientPosToCaretPos(P: TPoint; out AEndOfLinePos: boolean): TPoint;
    property Carets: TATCarets read FCarets;
    function IsLineWithCaret(ALine: integer): boolean;
    procedure DoGotoPos(APnt: TPoint; AIndentHorz, AIndentVert: integer);
    procedure DoGotoCaret(AEdge: TATCaretEdge);
    procedure DoGotoPosEx(APnt: TPoint);
    //misc
    procedure DoCommand(ACmd: integer; const AText: atString = ''); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function TextSelected: atString;
    function TextCurrentWord: atString;
    procedure DoSelect_All;
    procedure DoSelect_Inverted;
    procedure DoSelect_SplitSelectionToLines;
    procedure DoSelect_Line(P: TPoint);
    procedure DoSelect_Word(P: TPoint);
    procedure DoSelect_LineRange(ALineFrom: integer; P: TPoint);
    procedure DoRangeFold(ARange: TATSynRange);
    procedure DoRangeUnfold(ARange: TATSynRange);
    procedure DoScrollByDelta(Dx, Dy: integer);
    procedure DoSizeChange(AInc: boolean);
    procedure DoCommentSelectionLines(Act: TATCommentAction;
      const AComment: atString);

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
    function DoGetTextString: atString; virtual;
    //messages
    procedure WMGetDlgCode(var Msg: TLMNoParams); message LM_GETDLGCODE;
    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    //menu
    property PopupText: TPopupMenu read FMenuText write FMenuText;
    property PopupGutterBm: TPopupMenu read FMenuGutterBm write FMenuGutterBm;
    property PopupGutterNum: TPopupMenu read FMenuGutterNum write FMenuGutterNum;
    property PopupGutterFold: TPopupMenu read FMenuGutterFold write FMenuGutterFold;
    property PopupMinimap: TPopupMenu read FMenuMinimap write FMenuMinimap;
    property PopupMicromap: TPopupMenu read FMenuMicromap write FMenuMicromap;
    property PopupRuler: TPopupMenu read FMenuRuler write FMenuRuler;
    //events std
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
    property OnUTF8KeyPress;
    //events new
    property OnClickDouble: TATSynEditClickEvent read FOnClickDbl write FOnClickDbl;
    property OnClickTriple: TATSynEditClickEvent read FOnClickTriple write FOnClickTriple;
    property OnClickMiddle: TATSynEditClickEvent read FOnClickMiddle write FOnClickMiddle;
    property OnClickGutter: TATSynEditClickGutterEvent read FOnClickGutter write FOnClickGutter;
    property OnClickMicromap: TATSynEditClickMicromapEvent read FOnClickMicromap write FOnClickMicromap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeState: TNotifyEvent read FOnChangeState write FOnChangeState;
    property OnChangeCaretPos: TNotifyEvent read FOnChangeCaretPos write FOnChangeCaretPos;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnCommand: TATSynEditCommandEvent read FOnCommand write FOnCommand;
    property OnDrawBookmarkIcon: TATSynEditDrawBookmarkEvent read FOnDrawBookmarkIcon write FOnDrawBookmarkIcon;
    property OnDrawLine: TATSynEditDrawLineEvent read FOnDrawLine write FOnDrawLine;
    property OnDrawMicromap: TATSynEditDrawRectEvent read FOnDrawMicromap write FOnDrawMicromap;
    property OnDrawEditor: TATSynEditDrawRectEvent read FOnDrawEditor write FOnDrawEditor;
    property OnDrawRuler: TATSynEditDrawRectEvent read FOnDrawRuler write FOnDrawRuler;
    property OnCalcHilite: TATSynEditCalcHiliteEvent read FOnCalcHilite write FOnCalcHilite;
    property OnCalcStaple: TATSynEditCalcStapleEvent read FOnCalcStaple write FOnCalcStaple;

    //misc
    property CursorText: TCursor read FCursorText write FCursorText;
    property CursorBm: TCursor read FCursorBm write FCursorBm;
    property Colors: TATSynEditColors read FColors write FColors;
    property WantTabs: boolean read FWantTabs write FWantTabs;

    //options
    property OptTabSpaces: boolean read FOptTabSpaces write FOptTabSpaces;
    property OptTabSize: integer read FTabSize write SetTabSize;
    property OptWordChars: atString read FOptWordChars write FOptWordChars;
    property OptFoldStyle: TATFoldStyle read FFoldStyle write FFoldStyle;
    property OptTextLocked: string read FTextLocked write FTextLocked;
    property OptTextHint: string read FTextHint write FTextHint;
    property OptTextHintFontStyle: TFontStyles read FTextHintFontStyle write FTextHintFontStyle;
    property OptTextHintCenter: boolean read FTextHintCenter write FTextHintCenter;
    property OptTextOffsetTop: integer read FOptTextOffsetTop write FOptTextOffsetTop;
    property OptTextOffsetFromLine: integer read FOptTextOffsetFromLine write FOptTextOffsetFromLine;
    property OptAutoIndent: boolean read FOptAutoIndent write FOptAutoIndent;
    property OptAutoIndentKind: TATAutoIndentKind read FOptAutoIndentKind write FOptAutoIndentKind;
    property OptCopyLinesIfNoSel: boolean read FOptCopyLinesIfNoSel write FOptCopyLinesIfNoSel;
    property OptCutLinesIfNoSel: boolean read FOptCutLinesIfNoSel write FOptCutLinesIfNoSel;
    property OptLastLineOnTop: boolean read FOptLastLineOnTop write FOptLastLineOnTop;
    property OptOverwriteSel: boolean read FOptOverwriteSel write FOptOverwriteSel;
    property OptOverwriteAllowedOnPaste: boolean read FOptOverwriteAllowedOnPaste write FOptOverwriteAllowedOnPaste;
    property OptShowStapleStyle: TATLineStyle read FOptShowStapleStyle write FOptShowStapleStyle;
    property OptShowStapleIndent: integer read FOptShowStapleIndent write FOptShowStapleIndent;
    property OptShowStapleWidthPercent: integer read FOptShowStapleWidthPercent write FOptShowStapleWidthPercent;
    property OptShowFullSel: boolean read FOptShowFullSel write FOptShowFullSel;
    property OptShowFullHilite: boolean read FOptShowFullHilite write FOptShowFullHilite;
    property OptShowCurLine: boolean read FOptShowCurLine write FOptShowCurLine;
    property OptShowCurLineMinimal: boolean read FOptShowCurLineMinimal write FOptShowCurLineMinimal;
    property OptShowScrollHint: boolean read FOptShowScrollHint write FOptShowScrollHint;
    property OptShowCurColumn: boolean read FOptShowCurColumn write FOptShowCurColumn;
    property OptCaretManyAllowed: boolean read GetCaretManyAllowed write SetCaretManyAllowed;
    property OptCaretVirtual: boolean read FCaretVirtual write FCaretVirtual;
    property OptCaretShape: TATSynCaretShape read FCaretShapeIns write SetCaretShapeIns;
    property OptCaretShapeOvr: TATSynCaretShape read FCaretShapeOvr write SetCaretShapeOvr;
    property OptCaretShapeRO: TATSynCaretShape read FCaretShapeRO write SetCaretShapeRO;
    property OptCaretBlinkTime: integer read GetCaretBlinkTime write SetCaretBlinkTime;
    property OptCaretBlinkEnabled: boolean read FCaretBlinkEnabled write SetCaretBlinkEnabled;
    property OptCaretStopUnfocused: boolean read FCaretStopUnfocused write FCaretStopUnfocused;
    property OptCaretPreferLeftSide: boolean read FOptCaretPreferLeftSide write FOptCaretPreferLeftSide;
    property OptGutterVisible: boolean read FOptGutterVisible write FOptGutterVisible;
    property OptGutterPlusSize: integer read FOptGutterPlusSize write FOptGutterPlusSize;
    property OptGutterShowFoldAlways: boolean read FOptGutterShowFoldAlways write FOptGutterShowFoldAlways;
    property OptGutterShowFoldLines: boolean read FOptGutterShowFoldLines write FOptGutterShowFoldLines;
    property OptGutterShowFoldLinesAll: boolean read FOptGutterShowFoldLinesAll write FOptGutterShowFoldLinesAll;
    property OptRulerVisible: boolean read FOptRulerVisible write FOptRulerVisible;
    property OptRulerSize: integer read FOptRulerSize write FOptRulerSize;
    property OptRulerFontSize: integer read FOptRulerFontSize write FOptRulerFontSize;
    property OptRulerMarkSizeSmall: integer read FOptRulerMarkSizeSmall write FOptRulerMarkSizeSmall;
    property OptRulerMarkSizeBig: integer read FOptRulerMarkSizeBig write FOptRulerMarkSizeBig;
    property OptRulerTextIndent: integer read FOptRulerTextIndent write FOptRulerTextIndent;
    property OptMinimapVisible: boolean read FMinimapVisible write SetMinimapVisible;
    property OptMinimapCharWidth: integer read FMinimapCharWidth write FMinimapCharWidth;
    property OptMinimapShowSelBorder: boolean read FMinimapShowSelBorder write FMinimapShowSelBorder;
    property OptMinimapShowSelAlways: boolean read FMinimapShowSelAlways write FMinimapShowSelAlways;
    property OptMicromapVisible: boolean read FMicromapVisible write SetMicromapVisible;
    property OptMicromapWidth: integer read FMicromapWidth write FMicromapWidth;
    property OptCharSpacingX: integer read GetCharSpacingX write SetCharSpacingX;
    property OptCharSpacingY: integer read GetCharSpacingY write SetCharSpacingY;
    property OptWrapMode: TATSynWrapMode read FWrapMode write SetWrapMode;
    property OptWrapIndented: boolean read FWrapIndented write SetWrapIndented;
    property OptMarginRight: integer read FMarginRight write SetMarginRight;
    property OptMarginString: string read GetMarginString write SetMarginString;
    property OptNumbersAutosize: boolean read FOptNumbersAutosize write FOptNumbersAutosize;
    property OptNumbersAlignment: TAlignment read FOptNumbersAlignment write FOptNumbersAlignment;
    property OptNumbersFontSize: integer read FOptNumbersFontSize write FOptNumbersFontSize;
    property OptNumbersStyle: TATSynNumbersStyle read FOptNumbersStyle write FOptNumbersStyle;
    property OptNumbersShowFirst: boolean read FOptNumbersShowFirst write FOptNumbersShowFirst;
    property OptNumbersShowCarets: boolean read FOptNumbersShowCarets write FOptNumbersShowCarets;
    property OptNumbersSkippedChar: atString read FOptNumbersSkippedChar write FOptNumbersSkippedChar;
    property OptNumbersIndentLeft: integer read FOptNumbersIndentLeft write FOptNumbersIndentLeft;
    property OptNumbersIndentRight: integer read FOptNumbersIndentRight write FOptNumbersIndentRight;
    property OptUnprintedVisible: boolean read FUnprintedVisible write FUnprintedVisible;
    property OptUnprintedSpaces: boolean read FUnprintedSpaces write FUnprintedSpaces;
    property OptUnprintedEnds: boolean read FUnprintedEnds write FUnprintedEnds;
    property OptUnprintedEndsDetails: boolean read FUnprintedEndsDetails write FUnprintedEndsDetails;
    property OptUnprintedReplaceSpec: boolean read FUnprintedReplaceSpec write FUnprintedReplaceSpec;
    property OptMouseEnableNormalSelection: boolean read FOptMouseEnableNormalSelection write FOptMouseEnableNormalSelection;
    property OptMouseEnableColumnSelection: boolean read FOptMouseEnableColumnSelection write FOptMouseEnableColumnSelection;
    property OptMouseDownForPopup: boolean read FOptMouseDownForPopup write FOptMouseDownForPopup;
    property OptMouseHideCursorOnType: boolean read FOptMouseHideCursor write FOptMouseHideCursor;
    property OptMouse2ClickSelectsLine: boolean read FOptMouse2ClickSelectsLine write FOptMouse2ClickSelectsLine;
    property OptMouse3ClickSelectsLine: boolean read FOptMouse3ClickSelectsLine write FOptMouse3ClickSelectsLine;
    property OptMouse2ClickDragSelectsWords: boolean read FOptMouse2ClickDragSelectsWords write FOptMouse2ClickDragSelectsWords;
    property OptMouseDragDrop: boolean read FOptMouseDragDrop write FOptMouseDragDrop;
    property OptMouseNiceScroll: boolean read FOptMouseNiceScroll write FOptMouseNiceScroll;
    property OptMouseRightClickMovesCaret: boolean read FOptMouseRightClickMovesCaret write FOptMouseRightClickMovesCaret;
    property OptMouseGutterClickSelectsLine: boolean read FOptMouseGutterClickSelectsLine write FOptMouseGutterClickSelectsLine;
    property OptKeyBackspaceUnindent: boolean read FOptKeyBackspaceUnindent write FOptKeyBackspaceUnindent;
    property OptKeyPageKeepsRelativePos: boolean read FOptKeyPageKeepsRelativePos write FOptKeyPageKeepsRelativePos;
    property OptKeyUpDownNavigateWrapped: boolean read FOptKeyUpDownNavigateWrapped write FOptKeyUpDownNavigateWrapped;
    property OptKeyUpDownKeepColumn: boolean read FOptKeyUpDownKeepColumn write FOptKeyUpDownKeepColumn;
    property OptKeyHomeEndNavigateWrapped: boolean read FOptKeyHomeEndNavigateWrapped write FOptKeyHomeEndNavigateWrapped;
    property OptKeyPageUpDownSize: TATPageUpDownSize read FOptKeyPageUpDownSize write FOptKeyPageUpDownSize;
    property OptKeyLeftRightSwapSel: boolean read FOptKeyLeftRightSwapSel write FOptKeyLeftRightSwapSel;
    property OptKeyLeftRightSwapSelAndSelect: boolean read FOptKeyLeftRightSwapSelAndSelect write FOptKeyLeftRightSwapSelAndSelect;
    property OptKeyHomeToNonSpace: boolean read FOptKeyHomeToNonSpace write FOptKeyHomeToNonSpace;
    property OptKeyEndToNonSpace: boolean read FOptKeyEndToNonSpace write FOptKeyEndToNonSpace;
    property OptKeyTabIndents: boolean read FOptKeyTabIndents write FOptKeyTabIndents;
    property OptIndentSize: integer read FOptIndentSize write FOptIndentSize;
    property OptIndentKeepsAlign: boolean read FOptIndentKeepsAlign write FOptIndentKeepsAlign;
    property OptShowIndentLines: boolean read FOptShowIndentLines write FOptShowIndentLines;
    property OptShowGutterCaretBG: boolean read FOptShowGutterCaretBG write FOptShowGutterCaretBG;
    property OptAllowScrollbars: boolean read FOptAllowScrollbars write FOptAllowScrollbars;
    property OptAllowZooming: boolean read FOptAllowZooming write FOptAllowZooming;
    property OptAllowReadOnly: boolean read FOptAllowReadOnly write FOptAllowReadOnly;
    property OptUndoLimit: integer read GetUndoLimit write SetUndoLimit;
    property OptUndoGrouped: boolean read FOptUndoGrouped write FOptUndoGrouped;
    property OptUndoAfterSave: boolean read GetUndoAfterSave write SetUndoAfterSave;
    property OptSavingForceFinalEol: boolean read FOptSavingForceFinalEol write FOptSavingForceFinalEol;
    property OptSavingTrimSpaces: boolean read FOptSavingTrimSpaces write FOptSavingTrimSpaces;
  end;


implementation

uses
  LCLIntf,
  LCLProc,
  Dialogs,
  Types,
  Math,
  Clipbrd,
  ATSynEdit_Commands,
  ATSynEdit_Keymap_Init,
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

  if FOptRulerFontSize<>0 then
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
      C.TextOut(NX - C.TextWidth(Str) div 2, FOptRulerTextIndent, Str);
    end;
  end;

  C.Font.Size:= NPrevSize;
end;

procedure TATSynEdit.UpdateGutterAutosize(C: TCanvas);
begin
  FGutter[FGutterBandNum].Size:= GetGutterNumbersWidth(C);
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
  CharSmall:= FCharSizeMinimap.X;

  if FMinimapCharWidth=0 then
  begin
    FMinimapWidth:= (ClientWidth - IfThen(FMicromapVisible, FMicromapWidth) - FTextOffset.X)
      * CharSmall div (CharSmall+CharBig);
  end
  else
    FMinimapWidth:= CharSmall*FMinimapCharWidth;

  FMinimapWidth:= Max(cMinMinimapWidth, FMinimapWidth);
end;

function TATSynEdit.DoFormatLineNumber(N: integer): atString;
begin
  if FOptNumbersShowCarets then
    if IsLineWithCaret(N-1) then
    begin
      Result:= IntToStr(N);
      Exit
    end;

  if FOptNumbersShowFirst then
    if N=1 then
    begin
      Result:= IntToStr(N);
      Exit
    end;

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
    else
      raise Exception.Create('Unknown num-style');
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
  //FWrapProgress:= 0;
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

        FWrapInfo.FindIndexesOfLineNumber(NLine, NIndexFrom, NIndexTo);
        if NIndexFrom<0 then
        begin
          //Showmessage('Cant find wrap-index for line '+Inttostr(NLine));
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


procedure TATSynEdit.DoCalcWrapInfos(ALine: integer; AIndentMaximal: integer; AItems: TList);
var
  NHiddenIndex, NOffset, NLen, NIndent: integer;
  NFinal: TATSynWrapFinal;
  Str: atString;
begin
  AItems.Clear;

  NHiddenIndex:= Strings.LinesHidden[ALine, FEditorIndex];
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
    NLen:= SFindWordWrapOffset(Str, Max(FWrapColumn-NIndent, cMinWrapColumnAbs), FTabSize, FOptWordChars, FWrapIndented);
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

procedure TATSynEdit.SetText(AValue: atString);
begin
  Strings.LoadFromString(AValue);
  DoCaretSingle(0, 0);
  Update(true);
end;

procedure TATSynEdit.SetWrapMode(AValue: TATSynWrapMode);
var
  NLine: integer;
begin
  if FWrapMode=AValue then Exit;

  NLine:= LineTop;

  FWrapMode:= AValue;
  FWrapUpdateNeeded:= true;

  if FWrapMode<>cWrapOff then
    FScrollHorz.NPos:= 0;

  Invalidate;
  AppProcessMessages;
  LineTop:= NLine;
  Invalidate;
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
    NPosLast:= Max(NMin, NMax-NPage);
  end;

  with FScrollHorz do
  begin
    NPage:= Max(1, GetVisibleColumns);
    NMin:= 0;
    //NMax calculated in DoPaintTextTo
    //hide horz bar for word-wrap:
    if FWrapMode=cWrapOn then
      NMax:= NPage;
    NPosLast:= Max(NMin, NMax-NPage);
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
  if not FOptAllowScrollbars then Exit;

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
  if not FOptAllowScrollbars then Exit;

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

procedure TATSynEdit.DoPaintTo(C: TCanvas; ALineFrom: integer);
begin
  if csLoading in ComponentState then Exit;

  C.Brush.Color:= GetColorTextBG;
  C.FillRect(ClientRect);

  C.Font.Assign(Font);
  FCharSize:= GetCharSize(C, FCharSpacingText);

  if FOptGutterVisible and FOptNumbersAutosize then
    UpdateGutterAutosize(C);
  if FMinimapVisible then
    UpdateMinimapAutosize(C);

  FTextOffset:= GetTextOffset; //after gutter autosize
  FRectMinimap:= GetRectMinimap;
  FRectMicromap:= GetRectMicromap;
  FRectGutter:= GetRectGutter;
  FRectMain:= GetRectMain; //after gutter/minimap
  FRectRuler:= GetRectRuler; //after main

  UpdateWrapInfo;

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
end;

function TATSynEdit.GetCharSize(C: TCanvas; ACharSpacing: TPoint): TPoint;
var
  Size: TPoint;
begin
  Size:= CanvasFontSizes(C);
  Result.X:= Max(1, Size.X + ACharSpacing.X);
  Result.Y:= Max(1, Size.Y + ACharSpacing.Y);
end;

procedure TATSynEdit.DoPaintGutterBandBG(C: TCanvas; ABand: integer; AColor: TColor; ATop, ABottom: integer);
var
  X1, X2: integer;
begin
  with FGutter[ABand] do
    begin X1:= Left; X2:= Right; end;
  C.Brush.Color:= AColor;
  if ATop>=0 then
    C.FillRect(X1, ATop, X2, ABottom)
  else
    C.FillRect(X1, FRectGutter.Top, X2, FRectGutter.Bottom)
end;

procedure TATSynEdit.DoPaintTextTo(C: TCanvas;
  const ARect: TRect;
  const ACharSize: TPoint;
  AWithGutter, AMainText: boolean;
  var AScrollHorz, AScrollVert: TATSynScrollInfo;
  ALineFrom: integer);
var
  NCoordTop, NCoordSep: integer;
  NWrapIndex, NLinesIndex: integer;
  NOutputCharsSkipped, NOutputStrWidth: integer;
  NOutputSpacesSkipped: real;
  WrapItem: TATSynWrapItem;
  NColorEntire, NColorAfter: TColor;
  Str, StrOut, StrOutUncut: atString;
  CurrPoint, CurrPointText, CoordAfterText, CoordNums: TPoint;
  LineSeparator: TATLineSeparator;
  LineWithCaret, LineEolSelected, LineColorForced: boolean;
  Parts: TATLineParts;
  Event: TATSynEditDrawLineEvent;
  StrSize: TSize;
  //
  procedure DoPaintGutterBandState(ATop: integer; AColor: TColor);
  begin
    DoPaintGutterBandBG(C, FGutterBandState, AColor, ATop, ATop+ACharSize.Y);
  end;
  //
begin
  //wrap turned off can cause bad scrollpos, fix it
  with AScrollVert do
    NPos:= Min(NPos, NPosLast);

  C.Brush.Color:= GetColorTextBG;
  C.FillRect(ARect);

  if AWithGutter then
  begin
    C.Brush.Color:= FColors.GutterBG;
    C.FillRect(FRectGutter);

    //paint some bands, for full height coloring
    if FGutter[FGutterBandFold].Visible then
      DoPaintGutterBandBG(C, FGutterBandFold, FColors.GutterFoldBG, -1, -1);
    if FGutter[FGutterBandSep].Visible then
      DoPaintGutterBandBG(C, FGutterBandSep, FColors.GutterSeparatorBG, -1, -1);
    if FGutter[FGutterBandEmpty].Visible then
      DoPaintGutterBandBG(C, FGutterBandEmpty, GetColorTextBG, -1, -1);
  end;

  if AMainText and (FTextHint<>'') then
    if (Strings.Count=0) or ((Strings.Count=1) and (Strings.Lines[0]='')) then
    begin
      DoPaintTextHintTo(C);
      Exit
    end;

  AScrollHorz.NMax:= 1;
  NCoordTop:= ARect.Top;

  if ALineFrom>=0 then
  begin
    FWrapInfo.FindIndexesOfLineNumber(ALineFrom, NWrapIndex, NColorAfter{dummy});
    AScrollVert.NPos:= NWrapIndex;
  end
  else
    NWrapIndex:= AScrollVert.NPos;

  repeat
    if NCoordTop>ARect.Bottom then Break;
    if not FWrapInfo.IsIndexValid(NWrapIndex) then Break;

    WrapItem:= FWrapInfo.Items[NWrapIndex];
    NLinesIndex:= WrapItem.NLineIndex;
    if not Strings.IsIndexValid(NLinesIndex) then Break;
    FLineBottom:= NLinesIndex;

    if IsFoldLineNeededBeforeWrapitem(NWrapIndex) then
    begin
      NCoordSep:= NCoordTop-1;
      C.Pen.Color:= Colors.CollapseLine;
      C.Line(ARect.Left, NCoordSep, ARect.Right, NCoordSep);
    end;

    //prepare line
    Str:= Strings.Lines[NLinesIndex];
    Str:= Copy(Str, WrapItem.NCharIndex, WrapItem.NLength);

    LineSeparator:= Strings.LinesSeparator[NLinesIndex];
    LineWithCaret:= IsLineWithCaret(NLinesIndex);
    LineEolSelected:= IsPosSelected(WrapItem.NCharIndex-1+WrapItem.NLength, WrapItem.NLineIndex);

    StrOut:= Str;
    StrOutUncut:= StrOut;
    AScrollHorz.NMax:= Max(AScrollHorz.NMax,
      Round(CanvasTextSpaces(StrOutUncut, FTabSize)) + cScrollKeepHorz);

    CurrPoint.X:= ARect.Left;
    CurrPoint.Y:= NCoordTop;

    C.Brush.Color:= GetColorTextBG;
    C.Font.Color:= GetColorTextFont;

    DoCalcLineEntireColor(NLinesIndex, NCoordTop, LineWithCaret, NColorEntire, LineColorForced);
    C.Brush.Color:= NColorEntire;
    C.FillRect(ARect.Left, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y);

    CurrPointText:= Point(
      Int64(CurrPoint.X) + (Int64(WrapItem.NIndent)-AScrollHorz.NPos)*ACharSize.X,
      CurrPoint.Y);
    NOutputStrWidth:= 0;

    //paint line
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

      if WrapItem.NIndent>0 then
      begin
        NColorAfter:= GetColorTextBG;
        DoCalcPosColor(WrapItem.NCharIndex, NLinesIndex, NColorAfter);
        DoPaintLineIndent(C, ARect, ACharSize,
          NCoordTop, WrapItem.NIndent,
          NColorAfter,
          AScrollHorz.NPos, AMainText and FOptShowIndentLines);
      end;

      NColorAfter:= clNone;
      DoCalcLineHilite(WrapItem, Parts{%H-},
        NOutputCharsSkipped, cMaxCharsForOutput,
        NColorEntire, LineColorForced,
        NColorAfter);

      //adapter may return ColorAfterEol, paint it
      if FOptShowFullHilite then
        if NColorAfter<>clNone then
        begin
          C.Brush.Color:= NColorAfter;
          C.FillRect(CurrPointText.X, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y);
        end;

      //paint selection bg, after applying ColorAfterEol
      DoPaintSelectedLineBG(C, ACharSize, ARect,
        CurrPoint,
        CurrPointText,
        NLinesIndex,
        LineEolSelected,
        AScrollHorz);

      if AWithGutter then
        Event:= FOnDrawLine
      else
        Event:= nil;

      if FUnprintedReplaceSpec then
        StrOut:= SRemoveAsciiControlChars(StrOut);

      if AMainText then
        CanvasTextOut(C,
          CurrPointText.X,
          CurrPointText.Y,
          StrOut,
          FTabSize,
          ACharSize,
          AMainText,
          AMainText and FUnprintedVisible and FUnprintedSpaces,
          FColors.UnprintedFont,
          FColors.UnprintedHexFont,
          NOutputStrWidth,
          Trunc(NOutputSpacesSkipped), //todo:
            //needed number of chars of all chars counted as 1.0,
            //while NOutputSpacesSkipped is with cjk counted as 1.7
          @Parts,
          Event,
          FOptTextOffsetFromLine
          )
      else
        CanvasTextOutMinimap(C,
          StrOut,
          CurrPointText,
          FCharSizeMinimap,
          FTabSize,
          @Parts
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
        begin
          C.Brush.Color:= NColorAfter;
          C.FillRect(ARect.Left, NCoordTop, ARect.Right, NCoordTop+ACharSize.Y);
        end;
      end;

      DoPaintSelectedLineBG(C, ACharSize, ARect,
        CurrPoint,
        CurrPointText,
        NLinesIndex,
        LineEolSelected,
        AScrollHorz);
    end;

    CoordAfterText:= Point(
      CurrPointText.X+NOutputStrWidth,
      CurrPointText.Y);

    if WrapItem.NFinal=cWrapItemFinal then
    begin
      //for OptShowFullSel=false paint eol bg
      if LineEolSelected then
      begin
        C.Brush.Color:= FColors.TextSelBG;
        C.FillRect(
          CoordAfterText.X,
          CoordAfterText.Y,
          CoordAfterText.X+ACharSize.X,
          CoordAfterText.Y+ACharSize.Y);
      end;

      //paint eol mark
      if AMainText and FUnprintedVisible and FUnprintedEnds then
        DoPaintUnprintedEol(C,
          cLineEndNiceNames[Strings.LinesEnds[WrapItem.NLineIndex]],
          CoordAfterText,
          ACharSize,
          FColors.UnprintedFont,
          FColors.UnprintedBG,
          FUnprintedEndsDetails);
    end;

    //draw collapsed-mark
    if AMainText then
      if WrapItem.NFinal=cWrapItemCollapsed then
        DoPaintFoldedMark(C, CoordAfterText, GetFoldedMarkText(NLinesIndex));

    //draw separators
    if LineSeparator<>cLineSepNone then
    begin
      if LineSeparator=cLineSepTop then
        NCoordSep:= NCoordTop
      else
        NCoordSep:= NCoordTop+ACharSize.Y-1;
      C.Pen.Color:= Colors.BlockSepLine;
      C.Line(ARect.Left, NCoordSep, ARect.Right, NCoordSep);
    end;

    //draw gutter
    if AWithGutter then
    begin
      //paint area over scrolled text
      C.Brush.Color:= FColors.GutterBG;
      C.FillRect(FRectGutter.Left, NCoordTop, FRectGutter.Right, NCoordTop+ACharSize.Y);

      //gutter band: number
      if FGutter[FGutterBandNum].Visible then
      begin
        if LineWithCaret and FOptShowGutterCaretBG then
          DoPaintGutterBandBG(C, FGutterBandNum, FColors.GutterCaretBG, NCoordTop, NCoordTop+ACharSize.Y);

        if FWrapInfo.IsItemInitial(NWrapIndex) then
        begin
          C.Font.Color:= FColors.GutterFont;
          if FOptNumbersFontSize<>0 then
            C.Font.Size:= FOptNumbersFontSize;

          Str:= DoFormatLineNumber(NLinesIndex+1);
          StrSize:= C.TextExtent(Str);

          case FOptNumbersAlignment of
            taLeftJustify: CoordNums.X:= FGutter[FGutterBandNum].Left + FOptNumbersIndentLeft;
            taRightJustify: CoordNums.X:= FGutter[FGutterBandNum].Right - StrSize.cx - FOptNumbersIndentRight;
            taCenter: CoordNums.X:= (FGutter[FGutterBandNum].Left + FGutter[FGutterBandNum].Right - StrSize.cx) div 2;
          end;

          if FOptNumbersFontSize=0 then
            CoordNums.Y:= NCoordTop
          else
            CoordNums.Y:= NCoordTop + ACharSize.y div 2 - StrSize.cy div 2;

          C.TextOut(CoordNums.X, CoordNums.Y, Str);
          C.Font.Size:= Font.Size;
        end;
      end;

      //gutter band: bookmark
      if FGutter[FGutterBandBm].Visible then
        if FWrapInfo.IsItemInitial(NWrapIndex) then
        begin
          if Strings.LinesBm[NLinesIndex]<>0 then
            DoEventDrawBookmarkIcon(C, NLinesIndex,
              Rect(
                FGutter[FGutterBandBm].Left,
                NCoordTop,
                FGutter[FGutterBandBm].Right,
                NCoordTop+ACharSize.Y
                ));
        end;

      //gutter band: fold
      if FGutter[FGutterBandFold].Visible then
      begin
        DoPaintGutterBandBG(C, FGutterBandFold, FColors.GutterFoldBG, NCoordTop, NCoordTop+ACharSize.Y);
        DoPaintGutterFolding(C,
          NWrapIndex,
          FGutter[FGutterBandFold].Left,
          FGutter[FGutterBandFold].Right,
          NCoordTop,
          NCoordTop+ACharSize.Y
          );
      end;

      //gutter band: state
      if FGutter[FGutterBandState].Visible then
      begin
        case Strings.LinesState[NLinesIndex] of
          cLineStateChanged: DoPaintGutterBandState(NCoordTop, FColors.StateChanged);
          cLineStateAdded: DoPaintGutterBandState(NCoordTop, FColors.StateAdded);
          cLineStateSaved: DoPaintGutterBandState(NCoordTop, FColors.StateSaved);
        end;
      end;

      //gutter band: separator
      if FGutter[FGutterBandSep].Visible then
        DoPaintGutterBandBG(C, FGutterBandSep, FColors.GutterSeparatorBG, NCoordTop, NCoordTop+ACharSize.Y);
      //gutter band: empty indent
      if FGutter[FGutterBandEmpty].Visible then
        DoPaintGutterBandBG(C, FGutterBandEmpty, GetColorTextBG, NCoordTop, NCoordTop+ACharSize.Y);
    end;

    //end of painting line
    Inc(NCoordTop, ACharSize.Y);
    Inc(NWrapIndex);
  until false;

  //staples
  if AMainText then
    DoPaintStaples(C, ARect, ACharSize, AScrollHorz);
end;

procedure TATSynEdit.DoPaintMinimapSelTo(C: TCanvas);
var
  R: TRect;
begin
  if not FMinimapShowSelAlways then
    if not FCursorOnMinimap then Exit;

  R.Left:= FRectMinimap.Left;
  R.Right:= FRectMinimap.Right;
  R.Top:= FRectMinimap.Top + (FScrollVert.NPos-FScrollVertMinimap.NPos)*FCharSizeMinimap.Y;
  R.Bottom:= R.Top + (FScrollVert.NPage+1)*FCharSizeMinimap.Y;

  if IntersectRect(R, R, FRectMinimap) then
  begin
    CanvasInvertRect(C, R, FColors.MinimapSelBG);
    if FMinimapShowSelBorder then
    begin
      C.Pen.Color:= FColors.MinimapBorder;
      C.Brush.Style:= bsClear;
      C.Rectangle(R);
      C.Brush.Style:= bsSolid;
    end;
  end;
end;

procedure TATSynEdit.DoPaintMinimapTo(C: TCanvas);
begin
  DoClearScrollInfo(FScrollHorzMinimap);
  DoClearScrollInfo(FScrollVertMinimap);

  FScrollVertMinimap.NPos:= GetMinimapScrollPos;
  FScrollVertMinimap.NPosLast:= MaxInt div 2;
  DoPaintTextTo(C, FRectMinimap, FCharSizeMinimap, false, false, FScrollHorzMinimap, FScrollVertMinimap, -1);

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
  if Assigned(FOnDrawMicromap) then
    FOnDrawMicromap(Self, C, FRectMicromap)
  else
  begin
    C.Brush.Color:= clCream;
    C.FillRect(FRectMicromap);
  end;
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


procedure TATSynEdit.DoPaintFoldedMark(C: TCanvas;
  ACoord: TPoint; const AMarkText: string);
var
  NWidth: integer;
  Str: string;
begin
  Str:= STabsToSpaces(AMarkText, FTabSize);
  Inc(ACoord.X, cFoldedMarkIndentOuter);

  //paint bg
  C.Font.Color:= FColors.CollapseMarkFont;
  C.Brush.Color:= FColors.CollapseMarkBG;

  //paint text
  C.TextOut(
    ACoord.X+cFoldedMarkIndentInner,
    ACoord.Y+FOptTextOffsetFromLine,
    Str);
  NWidth:= C.TextWidth(Str) + 2*cFoldedMarkIndentInner;

  //paint frame
  C.Pen.Color:= FColors.CollapseMarkFont;
  C.Brush.Style:= bsClear;
  C.Rectangle(ACoord.X, ACoord.Y, ACoord.X+NWidth, ACoord.Y+FCharSize.Y);
  C.Brush.Style:= bsSolid;
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

function TATSynEdit.GetLineTop: integer;
var
  Item: TATSynWrapItem;
begin
  Result:= 0;
  Item:= FWrapInfo.Items[FScrollVert.NPos];
  if Assigned(Item) then
    Result:= Item.NLineIndex;
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

  FWantTabs:= true;
  FCharSize:= Point(4, 4); //not nul
  FEditorIndex:= 0;

  FCarets:= TATCarets.Create;
  FCarets.Add(0, 0);
  FCaretBlinkEnabled:= true;
  FCaretShown:= false;
  FCaretShapeIns:= cInitCaretShapeIns;
  FCaretShapeOvr:= cInitCaretShapeOvr;
  FCaretShapeRO:= cInitCaretShapeRO;
  FCaretVirtual:= true;
  FCaretSpecPos:= false;
  FCaretStopUnfocused:= true;

  FPaintLocked:= 0;
  FPaintStatic:= false;
  FPaintFlags:= [cPaintUpdateBitmap, cPaintUpdateScrollbars];

  FColors:= TATSynEditColors.Create;
  InitDefaultColors(FColors);

  FCursorText:= crIBeam;
  FCursorBm:= crHandPoint;

  FTimerBlink:= TTimer.Create(Self);
  FTimerBlink.Interval:= cInitTimerBlink;
  FTimerBlink.OnTimer:= @TimerBlinkTick;
  FTimerBlink.Enabled:= true;

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

  FStringsExternal:= nil;
  FStringsInt:= TATStrings.Create;
  FStringsInt.OnGetCaretsArray:= @GetCaretsArray;
  FStringsInt.OnSetCaretsArray:= @SetCaretsArray;

  FFold:= TATSynRanges.Create;
  FFoldStyle:= cInitFoldStyle;

  FWrapInfo:= TATSynWrapInfo.Create;
  FWrapInfo.OnCheckLineCollapsed:= @IsLineFoldedFull;
  FWrapUpdateNeeded:= true;
  FWrapMode:= cWrapOn;
  FWrapColumn:= cInitMarginRight;
  FWrapIndented:= true;
  //FWrapProgress:= 0;

  FOverwrite:= false;
  FTabSize:= cInitTabSize;
  FMarginRight:= cInitMarginRight;
  FMarginList:= TList.Create;

  FUnprintedVisible:= true;
  FUnprintedSpaces:= true;
  FUnprintedEnds:= true;
  FUnprintedEndsDetails:= true;
  FUnprintedReplaceSpec:= true;

  FTextLocked:= 'wait...';
  FTextHint:= '';
  FTextHintFontStyle:= [fsItalic];
  FTextHintCenter:= false;

  FGutter:= TATGutter.Create;
  FOptGutterVisible:= true;
  FOptGutterPlusSize:= cInitGutterPlusSize;
  FOptGutterShowFoldAlways:= true;
  FOptGutterShowFoldLines:= true;
  FOptGutterShowFoldLinesAll:= false;

  FGutterBandBm:= 0;
  FGutterBandNum:= 1;
  FGutterBandState:= 2;
  FGutterBandFold:= 3;
  FGutterBandSep:= 4;
  FGutterBandEmpty:= 5;

  for i:= 1 to cGutterBands do
    FGutter.Add(10);
  FGutter[FGutterBandBm].Size:= cGutterSizeBm;
  FGutter[FGutterBandNum].Size:= cGutterSizeNum;
  FGutter[FGutterBandState].Size:= cGutterSizeState;
  FGutter[FGutterBandFold].Size:= cGutterSizeFold;
  FGutter[FGutterBandSep].Size:= cGutterSizeSep;
  FGutter[FGutterBandEmpty].Size:= cGutterSizeEmpty;
  FGutter[FGutterBandSep].Visible:= false;
  FGutter.Update;

  FOptNumbersAutosize:= true;
  FOptNumbersAlignment:= taRightJustify;
  FOptNumbersFontSize:= 0;
  FOptNumbersStyle:= cInitNumbersStyle;
  FOptNumbersShowFirst:= true;
  FOptNumbersShowCarets:= false;
  FOptNumbersSkippedChar:= '.';
  FOptNumbersIndentLeft:= 5;
  FOptNumbersIndentRight:= 5;

  FOptRulerVisible:= true;
  FOptRulerSize:= cSizeRulerHeight;
  FOptRulerMarkSizeSmall:= cSizeRulerMarkSmall;
  FOptRulerMarkSizeBig:= cSizeRulerMarkBig;
  FOptRulerFontSize:= 8;
  FOptRulerTextIndent:= 0;

  FMinimapWidth:= cInitMinimapWidth;
  FMinimapCharWidth:= 0;
  FMinimapVisible:= cInitMinimapVisible;
  FMinimapShowSelBorder:= false;
  FMinimapShowSelAlways:= true;
  FMicromapWidth:= cInitMicromapWidth;
  FMicromapVisible:= cInitMicromapVisible;

  FCharSpacingText:= Point(0, cInitSpacingText);
  FCharSizeMinimap:= Point(1, 2);

  FOptShowStapleStyle:= cLineStyleSolid;
  FOptShowStapleIndent:= -1;
  FOptShowStapleWidthPercent:= 100;

  FOptTextOffsetTop:= 0;
  FOptTextOffsetFromLine:= cInitTextOffsetFromLine;
  FOptAllowScrollbars:= true;
  FOptAllowZooming:= true;
  FOptAllowReadOnly:= true;
  FOptKeyBackspaceUnindent:= true;
  FOptKeyPageKeepsRelativePos:= true;
  FOptKeyUpDownNavigateWrapped:= true;
  FOptKeyHomeEndNavigateWrapped:= true;
  FOptKeyUpDownKeepColumn:= true;
  FOptOverwriteAllowedOnPaste:= false;
  FOptWordChars:= '';
  FOptAutoIndent:= true;
  FOptAutoIndentKind:= cIndentAsIs;
  FOptTabSpaces:= false;

  FOptLastLineOnTop:= false;
  FOptOverwriteSel:= true;
  FOptMouseDragDrop:= true;
  FOptMouseNiceScroll:= true;
  FOptMouseHideCursor:= false;
  FOptMouse2ClickSelectsLine:= false;
  FOptMouse3ClickSelectsLine:= true;
  FOptMouse2ClickDragSelectsWords:= true;
  FOptMouseRightClickMovesCaret:= false;
  FOptMouseGutterClickSelectsLine:= true;
  FOptCopyLinesIfNoSel:= true;
  FOptCutLinesIfNoSel:= false;
  FOptShowFullSel:= false;
  FOptShowFullHilite:= true;
  FOptShowCurLine:= false;
  FOptShowCurLineMinimal:= true;
  FOptShowCurColumn:= false;
  FOptKeyPageUpDownSize:= cPageSizeFullMinus1;
  FOptKeyLeftRightSwapSel:= true;
  FOptKeyLeftRightSwapSelAndSelect:= false;
  FOptKeyHomeToNonSpace:= true;
  FOptKeyEndToNonSpace:= true;
  FOptKeyTabIndents:= true;
  FOptShowIndentLines:= true;
  FOptShowGutterCaretBG:= true;
  FOptIndentSize:= 2;
  FOptIndentKeepsAlign:= true;
  FOptUndoGrouped:= true;
  FOptSavingForceFinalEol:= false;
  FOptSavingTrimSpaces:= false;
  FOptShowScrollHint:= false;
  FOptCaretPreferLeftSide:= true;
  FOptMouseDownForPopup:= false;
  FOptMouseEnableNormalSelection:= true;
  FOptMouseEnableColumnSelection:= true;

  FMouseDownPnt:= Point(-1, -1);
  FMouseDownNumber:= -1;
  FMouseDownDouble:= false;
  FMouseDownRight:= false;
  FMouseDragDropping:= false;
  FMouseNiceScrollPos:= Point(0, 0);

  FSelRect:= cRectEmpty;
  FCursorOnMinimap:= false;
  FCursorOnGutter:= false;
  FLastTextCmd:= 0;
  FLastTextCmdText:= '';

  DoClearScrollInfo(FScrollHorz);
  DoClearScrollInfo(FScrollVert);

  FKeymap:= KeymapFull;
  FHintWnd:= THintWindow.Create(Self);

  FMenuStd:= TPopupMenu.Create(Self);
  FMenuText:= nil;
  FMenuGutterBm:= nil;
  FMenuGutterNum:= nil;
  FMenuGutterFold:= nil;
  FMenuGutterFoldStd:= nil;
  FMenuMinimap:= nil;
  FMenuMicromap:= nil;
  FMenuRuler:= nil;

  DoInitPopupMenu;
end;

destructor TATSynEdit.Destroy;
begin
  FreeAndNil(FHintWnd);
  FreeAndNil(FMenuStd);
  DoPaintModeStatic;
  FreeAndNil(FFold);
  FreeAndNil(FTimerNiceScroll);
  FreeAndNil(FTimerScroll);
  FreeAndNil(FTimerBlink);
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
    //show "wait" text
    Strings.Clear;
    Update(true);
    AppProcessMessages;

  try
    Strings.LoadFromFile(AFilename);
  finally
    EndUpdate;
  end;

  Update;
  DoPaintModeBlinking;
  DoEventChange;
  DoEventCarets;

  {$ifdef test_foldlist}
  DoDebugInitFoldList;
  {$endif}
end;

procedure TATSynEdit.SaveToFile(const AFilename: string);
var
  Change1, Change2: boolean;
begin
  Change1:= false;
  Change2:= false;

  if FOptSavingForceFinalEol then
    Change1:= Strings.ActionEnsureFinalEol;
  if FOptSavingTrimSpaces then
    Change2:= Strings.ActionTrimTrailSpaces;
  if Change1 or Change2 then
    Update(true);

  Strings.SaveToFile(AFilename);
  DoEventState; //modified
  Update;
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

procedure TATSynEdit.SetOneLine(AValue: boolean);
begin
  Strings.OneLine:= AValue;
  Carets.OneLine:= AValue;
  if AValue then
  begin
    OptGutterVisible:= false;
    OptRulerVisible:= false;
    OptMinimapVisible:= false;
    OptMicromapVisible:= false;
    OptCaretVirtual:= false;
    OptCaretManyAllowed:= false;
    OptUnprintedVisible:= false;
    OptWrapMode:= cWrapOff;
    OptAllowScrollbars:= false;
    OptAllowZooming:= false;
    OptAllowReadOnly:= false;
    OptMouseNiceScroll:= false;
    OptMouseDragDrop:= false;
    OptMarginRight:= 1000;
    OptUndoLimit:= 200;
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
  //find exact match
  FWrapInfo.FindIndexesOfLineNumber(AValue, NFrom, NTo);
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

procedure TATSynEdit.SetLinesFromTop(AValue: integer);
begin
  with FScrollVert do
    NPos:= Max(0, NPos + (GetLinesFromTop - AValue));
end;

procedure TATSynEdit.SetStrings(Obj: TATStrings);
begin
  FStringsExternal:= Obj;
end;

function TATSynEdit.GetTextOffset: TPoint;
begin
  Result.X:= 0;
  if FOptGutterVisible then
    Inc(Result.X, FGutter.Width);

  Result.Y:= FOptTextOffsetTop;
  if FOptRulerVisible then
    Inc(Result.Y, FOptRulerSize);
end;

function TATSynEdit.GetGutterNumbersWidth(C: TCanvas): integer;
var
  Str: atString;
  CharSize: integer;
begin
  if FOptNumbersFontSize<>0 then
    C.Font.Size:= FOptNumbersFontSize;
  CharSize:= C.TextWidth('0');

  Str:= IntToStr(Max(10, Strings.Count));
  Result:=
    Length(Str)*CharSize+
    FOptNumbersIndentLeft+
    FOptNumbersIndentRight;

  if FOptNumbersFontSize<>0 then
    C.Font.Size:= Font.Size;
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

function TATSynEdit.DoPaint(AFlags: TATSynPaintFlags; ALineFrom: integer): boolean;
var
  ARect: TRect;
begin
  Result:= false;
  if not Assigned(FBitmap) then Exit;

  if cPaintUpdateBitmap in AFlags then
  begin
    DoPaintTo(FBitmap.Canvas, ALineFrom);

    if cPaintUpdateCaretsCoords in AFlags then
    begin
      UpdateCaretsCoords;
      if FOptShowCurColumn and (Carets.Count>0) then
        DoPaintMarginLineTo(FBitmap.Canvas, Carets[0].CoordX, FColors.MarginCaret);
    end;

    FCaretShown:= false;
    DoPaintCarets(FBitmap.Canvas, false);
  end;

  ARect:= Canvas.ClipRect;
  Canvas.CopyRect(ARect, FBitmap.Canvas, ARect);

  if cPaintUpdateScrollbars in AFlags then
    Result:= UpdateScrollbars;
end;

procedure TATSynEdit.DoPaintLockedWarning(C: TCanvas);
var
  Str: string;
begin
  C.Brush.Color:= Colors.LockedBG;
  C.FillRect(ClientRect);
  C.Font.Assign(Self.Font);
  Str:= FTextLocked;
  C.TextOut(10, 10, Str);
end;


procedure TATSynEdit.Paint;
begin
  PaintEx(-1);
end;

procedure TATSynEdit.PaintEx(ALine: integer);
begin
  if FPaintLocked>0 then
  begin
    DoPaintLockedWarning(Canvas);
    Exit
  end;

  //if scrollbars shown, paint again
  if DoPaint(FPaintFlags, ALine) then
    DoPaint(FPaintFlags, ALine);

  Exclude(FPaintFlags, cPaintUpdateBitmap);
end;

procedure TATSynEdit.DoOnResize;
var
  SizeX, SizeY: integer;
begin
  inherited;

  if Assigned(FBitmap) then
  begin
    SizeX:= (Width div cResizeBitmapStep + 1)*cResizeBitmapStep;
    SizeY:= (Height div cResizeBitmapStep + 1)*cResizeBitmapStep;
    if (SizeX>FBitmap.Width) or (SizeY>FBitmap.Height) then
    begin
      FBitmap.SetSize(SizeX, SizeY);
      FBitmap.FreeImage; //recommended, else seen black bitmap on bigsize
    end;
  end;

  FWrapUpdateNeeded:= true;
  Include(FPaintFlags, cPaintUpdateScrollbars);
  Include(FPaintFlags, cPaintUpdateCaretsCoords);
  Include(FPaintFlags, cPaintUpdateBitmap);
  PaintEx(LineTop);
end;

//needed to remove flickering on resize and mouse-over
procedure TATSynEdit.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
  Msg.Result:= 1;
end;

procedure TATSynEdit.DoHintShow;
var
  S: string;
  P: TPoint;
  R: TRect;
begin
  if not FOptShowScrollHint then Exit;

  S:= cHintScrollPrefix+' '+IntToStr(LineTop+1);
  R:= FHintWnd.CalcHintRect(500, S, nil);

  P:= ClientToScreen(Point(ClientWidth-(R.Right-R.Left), 0));
  OffsetRect(R, P.X, P.Y);
  OffsetRect(R, -cHintScrollDx, cHintScrollDx);

  FHintWnd.ActivateHint(R, S);
  FHintWnd.Invalidate; //for Win
end;

procedure TATSynEdit.DoHintHide;
begin
  if Assigned(FHintWnd) then
    FHintWnd.Hide;
end;

function TATSynEdit.UpdateScrollInfoFromMessage(const Msg: TLMScroll; var Info: TATSynScrollInfo): boolean;
begin
  case Msg.ScrollCode of
    SB_TOP:        Info.NPos:= Info.NMin;
    SB_BOTTOM:     Info.NPos:= Info.NPosLast;

    SB_LINEUP:     Info.NPos:= Max(Info.NPos-1, Info.NMin);
    SB_LINEDOWN:   Info.NPos:= Min(Info.NPos+1, Info.NPosLast);

    SB_PAGEUP:     Info.NPos:= Max(Info.NPos-Info.NPage, Info.NMin);
    SB_PAGEDOWN:   Info.NPos:= Min(Info.NPos+Info.NPage, Info.NPosLast);

    SB_THUMBPOSITION: Info.NPos:= Msg.Pos;
    SB_THUMBTRACK:
      begin
        Info.NPos:= Msg.Pos;
        if @Info=@FScrollVert then DoHintShow;
      end;
    SB_ENDSCROLL:
      DoHintHide;
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
  EolPos: boolean;
  Index: integer;
begin
  inherited;
  SetFocus;

  PCaret:= ClientPosToCaretPos(Point(X, Y), EolPos);
  FCaretSpecPos:= false;
  FMouseDownNumber:= -1;
  FMouseDragDropping:= false;
  FMouseDownRight:= Shift=[ssRight];

  if MouseNiceScroll then
  begin
    MouseNiceScroll:= false;
    Exit
  end;

  if FMinimapVisible and PtInRect(FRectMinimap, Point(X, Y)) then
    if Shift=[ssLeft] then
    begin
      DoMinimapClick(Y);
      Exit
    end;

  if PtInRect(FRectMain, Point(X, Y)) then
  begin
    FMouseDownPnt:= PCaret;

    if Shift=[ssMiddle] then
    begin
      if DoHandleClickEvent(FOnClickMiddle) then Exit;
      if FOptMouseNiceScroll then
      begin
        FMouseNiceScrollPos:= Point(X, Y);
        MouseNiceScroll:= true;
      end;
      Exit
    end;

    if Shift=[ssLeft] then
    begin
      FSelRect:= cRectEmpty;
      Strings.SetGroupMark;
      DoCaretSingleAsIs;

      if FOptMouseDragDrop and (GetCaretSelectionIndex(FMouseDownPnt)>=0) and not ModeReadOnly then
      begin
        FMouseDragDropping:= true;
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
          Invalidate;
        end;
    end;
  end;

  if Shift=[ssRight] then
    if FOptMouseDownForPopup then
      DoHandleRightClick(X, Y);

  if FOptGutterVisible and PtInRect(FRectGutter, Point(X, Y)) then
  begin
    if Shift=[ssLeft] then
    begin
      Index:= FGutter.IndexAt(X);
      if Index=FGutterBandNum then
      begin
        if FOptMouseGutterClickSelectsLine then
        begin
          FSelRect:= cRectEmpty;
          FMouseDownNumber:= PCaret.Y;
          DoSelect_Line(PCaret);
        end;
      end
      else
      if Index=FGutterBandFold then
      begin
        DoFoldbarClick(PCaret.Y);
      end
      else
        //click on other bands- event
        DoEventClickGutter(FGutter.IndexAt(X), PCaret.Y);
    end;
  end;

  if FMicromapVisible and PtInRect(FRectMicromap, Point(X, Y)) then
    if Shift=[ssLeft] then
    begin
      DoEventClickMicromap(X-FRectMicromap.Left, Y-FRectMicromap.Top);
      Exit
    end;

  DoCaretsSort;
  DoEventCarets;
  Update;
end;

procedure TATSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDragDropping then
  begin
    DoDropText;
    Update;
  end;

  FMouseDownPnt:= Point(-1, -1);
  FMouseDownNumber:= -1;
  FMouseDownDouble:= false;
  FMouseDragDropping:= false;
  FTimerScroll.Enabled:= false;

  //popup menu
  if FMouseDownRight then
  begin
    FMouseDownRight:= false;
    if not FOptMouseDownForPopup then
      DoHandleRightClick(X, Y);
  end;
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
      FMenuStd.PopUp;
  end
  else
  if FOptGutterVisible and PtInRect(FRectGutter, Point(X, Y)) then
  begin
    Index:= FGutter.IndexAt(X);
    if Index=FGutterBandBm then
      if Assigned(FMenuGutterBm) then FMenuGutterBm.PopUp;
    if Index=FGutterBandNum then
      if Assigned(FMenuGutterNum) then FMenuGutterNum.PopUp;
    if Index=FGutterBandFold then
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
  RectBm: TRect;
begin
  if MouseNiceScroll then Exit;
  P:= ScreenToClient(Mouse.CursorPos);

  RectBm.Left:= FGutter[FGutterBandBm].Left;
  RectBm.Right:= FGutter[FGutterBandBm].Right;
  RectBm.Top:= FRectMain.Top;
  RectBm.Bottom:= FRectMain.Bottom;

  if FMouseDragDropping then
    Cursor:= crDrag
  else
  if PtInRect(FRectMain, P) then
    Cursor:= FCursorText
  else
  if PtInRect(RectBm, P) then
    Cursor:= FCursorBm
  else
    Cursor:= crDefault;
end;

procedure TATSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  RectNums: TRect;
  nIndex: integer;
  bOnMinimap, bOnGutter, bEolPos: boolean;
begin
  inherited;

  P:= Point(X, Y);
  UpdateCursor;

  //detect cursor on minimap
  if FMinimapVisible then
  begin
    bOnMinimap:= PtInRect(FRectMinimap, P);
    if not FMinimapShowSelAlways then
      if bOnMinimap<>FCursorOnMinimap then
        Invalidate;
    FCursorOnMinimap:= bOnMinimap;
  end;

  //detect cursor on gutter
  if FOptGutterVisible then
  begin
    bOnGutter:= PtInRect(FRectGutter, P);
    if not FOptGutterShowFoldAlways then
      if bOnGutter<>FCursorOnGutter then
        Invalidate;
    FCursorOnGutter:= bOnGutter;
  end;

  //numbers
  RectNums.Left:= FGutter[FGutterBandNum].Left;
  RectNums.Right:= FGutter[FGutterBandNum].Right;
  RectNums.Top:= FRectMain.Top;
  RectNums.Bottom:= FRectMain.Bottom;

  //start scroll timer
  FTimerScroll.Enabled:=
    (ssLeft in Shift) and
    (not PtInRect(ClientRect, P) or FCursorOnGutter);
  FMouseAutoScroll:= cDirNone;
  if P.Y<FRectMain.Top then FMouseAutoScroll:= cDirUp else
  if P.Y>=FRectMain.Bottom then FMouseAutoScroll:= cDirDown else
  if P.X<FRectMain.Left then FMouseAutoScroll:= cDirLeft else
  if P.X>=FRectMain.Right then FMouseAutoScroll:= cDirRight;

  //mouse dragged on numbers
  if PtInRect(RectNums, P) then
  begin
    if Shift=[ssLeft] then
    begin
      P:= ClientPosToCaretPos(P, bEolPos);
      if (P.Y>=0) and (P.X>=0) then
        if FMouseDownNumber>=0 then
        begin
          DoSelect_LineRange(FMouseDownNumber, P);
          DoCaretsSort;
          DoEventCarets;
          Update;
        end;
    end;
    Exit
  end;

  //mouse dragged on text
  if (not FMouseDragDropping) and (FMouseDownPnt.X>=0) then
    if PtInRect(FRectMain, P) then
    begin
      if ssLeft in Shift then
        if Carets.Count>0 then
        begin
          P:= ClientPosToCaretPos(P, bEolPos);
          if P.Y>=0 then
          begin
            //drag w/out button pressed: single selection
            if FOptMouseEnableNormalSelection then
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
            if FOptMouseEnableColumnSelection then
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
      Exit;
    end;

  //mouse dragged on minimap
  if PtInRect(FRectMinimap, P) then
  begin
    if Shift=[ssLeft] then
    begin
      DoMinimapClick(Y);
    end;
    Exit
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

function TATSynEdit.DoHandleClickEvent(AEvent: TATSynEditClickEvent): boolean;
begin
  Result:= false;
  if Assigned(AEvent) then
    AEvent(Self, Result);
end;

procedure TATSynEdit.DblClick;
begin
  inherited;

  if DoHandleClickEvent(FOnClickDbl) then Exit;

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

  if DoHandleClickEvent(FOnClickTriple) then Exit;

  if FOptMouse3ClickSelectsLine then
    DoSelect_Line_ByClick;
end;


procedure TATSynEdit.DoSelect_Word_ByClick;
var
  P: TPoint;
  EolPos: boolean;
begin
  P:= ScreenToClient(Mouse.CursorPos);
  if PtInRect(FRectMain, P) then
  begin
    P:= ClientPosToCaretPos(P, EolPos);
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
  EolPos: boolean;
begin
  P:= ScreenToClient(Mouse.CursorPos);
  if PtInRect(FRectMain, P) then
  begin
    P:= ClientPosToCaretPos(P, EolPos);
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
  DoPaintCarets(FBitmap.Canvas, true);
end;

procedure TATSynEdit.TimerScrollTick(Sender: TObject);
var
  nIndex: integer;
  PClient, PCaret: TPoint;
  EolPos: boolean;
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

  PCaret:= ClientPosToCaretPos(PClient, EolPos);
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

  if (Abs(Dx)<=cBitmapNiceScrollRadius) and
    (Abs(Dy)<=cBitmapNiceScrollRadius) then
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

  //delta in chars
  Dx:= Sign(Dx)*((Abs(Dx)-cBitmapNiceScrollRadius) div FCharSize.X + 1)*cSpeedScrollNiceHorz;
  Dy:= Sign(Dy)*((Abs(Dy)-cBitmapNiceScrollRadius) div FCharSize.Y + 1)*cSpeedScrollNiceVert;

  if Dir in [cDirLeft, cDirRight] then
    DoScrollByDelta(Dx, 0)
  else
    DoScrollByDelta(0, Dy);

  Invalidate;
end;

procedure TATSynEdit.DoPaintCarets(C: TCanvas; AWithInvalidate: boolean);
var
  R: TRect;
  i: integer;
  Item: TATCaretItem;
  Shape: TATSynCaretShape;
begin
  if IsCaretBlocked then
  begin
    if not FCaretShown then Exit;
  end;
  FCaretShown:= not FCaretShown;

  if ModeReadOnly then
    Shape:= FCaretShapeRO
  else
  if ModeOverwrite then
    Shape:= FCaretShapeOvr
  else
    Shape:= FCaretShapeIns;

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
      cCaretShapeVertPixels1: begin R.Right:= R.Left+1; end;
      cCaretShapeVertPixels2: begin R.Right:= R.Left+2; end;
      cCaretShapeVertPixels3: begin R.Right:= R.Left+3; end;
      cCaretShapeVertPixels4: begin R.Right:= R.Left+4; end;
      cCaretShapeVertPercents10: begin R.Right:= R.Left+Trunc(FCharSize.X*0.10); end;
      cCaretShapeVertPercents15: begin R.Right:= R.Left+Trunc(FCharSize.X*0.15); end;
      cCaretShapeVertPercents20: begin R.Right:= R.Left+Trunc(FCharSize.X*0.20); end;
      cCaretShapeVertPercents25: begin R.Right:= R.Left+Trunc(FCharSize.X*0.25); end;
      cCaretShapeVertPercents30: begin R.Right:= R.Left+Trunc(FCharSize.X*0.30); end;
      cCaretShapeVertPercents35: begin R.Right:= R.Left+Trunc(FCharSize.X*0.35); end;
      cCaretShapeVertPercents40: begin R.Right:= R.Left+Trunc(FCharSize.X*0.40); end;
      cCaretShapeVertPercents50: begin R.Right:= R.Left+FCharSize.X div 2; end;
      cCaretShapeHorzPixels1: begin R.Top:= R.Bottom-1; end;
      cCaretShapeHorzPixels2: begin R.Top:= R.Bottom-2; end;
      cCaretShapeHorzPixels3: begin R.Top:= R.Bottom-3; end;
      cCaretShapeHorzPixels4: begin R.Top:= R.Bottom-4; end;
      cCaretShapeHorzPixels5: begin R.Top:= R.Bottom-5; end;
      cCaretShapeHorzPercents10: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.10); end;
      cCaretShapeHorzPercents15: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.15); end;
      cCaretShapeHorzPercents20: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.20); end;
      cCaretShapeHorzPercents25: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.25); end;
      cCaretShapeHorzPercents30: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.30); end;
      cCaretShapeHorzPercents35: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.35); end;
      cCaretShapeHorzPercents40: begin R.Top:= R.Bottom-Trunc(FCharSize.Y*0.40); end;
      cCaretShapeHorzPercents50: begin R.Top:= R.Bottom-FCharSize.Y div 2; end;
    end;

    if IntersectRect(R, R, FRectMain) then
    begin
      CanvasInvertRect(C, R, FColors.Caret);

      //frame-shape: invert second time inner area
      if Shape=cCaretShapeFrameFull then
        CanvasInvertRect(C, Rect(R.Left+1, R.Top+1, R.Right-1, R.Bottom-1), FColors.Caret);

      if AWithInvalidate then
        InvalidateRect(Handle, @R, false);
    end;
  end;
end;

procedure TATSynEdit.DoPaintModeStatic;
begin
  FPaintStatic:= true;
  FTimerBlink.Enabled:= false;
  Invalidate;
end;

procedure TATSynEdit.DoPaintModeBlinking;
begin
  FPaintStatic:= false;
  if Assigned(FTimerBlink) then
  begin
    FTimerBlink.Enabled:= false;
    FTimerBlink.Enabled:= FCaretBlinkEnabled;
  end;
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
        CanvasDottedVertLine_Alt(C,
          FColors.IndentVertLines,
          ARect.Left + (i-AScrollPos)*ACharSize.X,
          ACoordY,
          ACoordY+ACharSize.Y);
end;

procedure TATSynEdit.DoPaintSelectedLineBG(C: TCanvas;
  ACharSize: TPoint;
  const AVisRect: TRect;
  APointLeft: TPoint;
  APointText: TPoint;
  ALineIndex: integer;
  AEolSelected: boolean;
  const AScrollHorz: TATSynScrollInfo);
var
  NLeft, NRight: integer;
begin
  if not IsSelRectEmpty then
  begin
    if (ALineIndex>=FSelRect.Top) and (ALineIndex<=FSelRect.Bottom) then
    begin
      NLeft:= APointLeft.X+ACharSize.X*(FSelRect.Left-AScrollHorz.NPos);
      NRight:= NLeft+ACharSize.X*(FSelRect.Right-FSelRect.Left);
      C.Brush.Color:= FColors.TextSelBG;
      C.FillRect(
        NLeft,
        APointLeft.Y,
        NRight,
        APointLeft.Y+ACharSize.Y);
    end;
  end
  else
  if FOptShowFullSel then
    if AEolSelected then
    begin
      C.Brush.Color:= FColors.TextSelBG;
      C.FillRect(
        APointText.X,
        APointText.Y,
        AVisRect.Right,
        APointText.Y+ACharSize.Y);
    end;
end;

procedure TATSynEdit.DoPaintNiceScroll(C: TCanvas);
begin
  if MouseNiceScroll then
    C.Draw(
      FMouseNiceScrollPos.X - cBitmapNiceScrollRadius,
      FMouseNiceScrollPos.Y - cBitmapNiceScrollRadius,
      cBitmapNiceScroll);
end;


function TATSynEdit.DoEventCommand(ACommand: integer): boolean;
begin
  Result:= false;
  if Assigned(FOnCommand) then
    FOnCommand(Self, ACommand, Result);
end;

function TATSynEdit.GetCaretBlinkTime: integer;
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
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorCaretMove(Self);

  if Assigned(FOnChangeCaretPos) then
    FOnChangeCaretPos(Self);
end;

procedure TATSynEdit.DoEventScroll;
begin
  //if Assigned(FAdapterHilite) then
  //  FAdapterHilite.OnEditorScroll(Self);

  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATSynEdit.DoEventChange;
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorChange(Self);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TATSynEdit.DoEventState;
begin
  if Assigned(FOnChangeState) then
    FOnChangeState(Self);
end;

procedure TATSynEdit.DoEventClickGutter(ABandIndex, ALineNumber: integer);
begin
  if Assigned(FOnClickGutter) then
    FOnClickGutter(Self, ABandIndex, ALineNumber);
end;

procedure TATSynEdit.DoEventClickMicromap(AX, AY: integer);
begin
  if Assigned(FOnClickMicromap) then
    FOnClickMicromap(Self, AX, AY);
end;

procedure TATSynEdit.DoEventDrawBookmarkIcon(C: TCanvas; ALineNumber: integer; const ARect: TRect);
begin
  if Assigned(FOnDrawBookmarkIcon) then
    FOnDrawBookmarkIcon(Self, C, ALineNumber, ARect);
end;

procedure TATSynEdit.DoScrollByDelta(Dx, Dy: integer);
begin
  with FScrollHorz do
    NPos:= Max(0, Min(NPosLast, NPos+Dx));
  with FScrollVert do
    NPos:= Max(0, Min(NPosLast, NPos+Dy));
end;

procedure TATSynEdit.MenuClick(Sender: TObject);
var
  Cmd: integer;
begin
  Cmd:= (Sender as TMenuItem).Tag;
  if Cmd>0 then
  begin
    DoCommand(Cmd);
    Update;
  end;
end;

procedure TATSynEdit.MenuPopup(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to FMenuStd.Items.Count-1 do
    with FMenuStd.Items[i] do
    begin
      if Assigned(FKeymap) then
        ShortCut:= FKeymap.GetShortcutFromCommand(Tag);

      case Tag of
        cCommand_ClipboardCut: Enabled:= not ModeReadOnly;
        cCommand_ClipboardPaste: Enabled:= not ModeReadOnly and Clipboard.HasFormat(CF_Text);
        cCommand_TextDeleteSelection: Enabled:= not ModeReadOnly and Carets.IsSelection;
        cCommand_Undo: Enabled:= not ModeReadOnly and (UndoCount>0);
        cCommand_Redo: Enabled:= not ModeReadOnly and (RedoCount>0);
      end;
    end;
end;

procedure TATSynEdit.DoInitPopupMenu;
  //
  procedure Add(const SName: string; Cmd: integer);
  var
    MI: TMenuItem;
  begin
    MI:= TMenuItem.Create(FMenuStd);
    MI.Caption:= SName;
    MI.Tag:= Cmd;
    MI.OnClick:= @MenuClick;
    FMenuStd.Items.Add(MI);
  end;
  //
begin
  FMenuStd.OnPopup:= @MenuPopup;

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
  bSel, bEolPos: boolean;
  Str: atString;
  Relation: TATPosRelation;
begin
  if Carets.Count<>1 then Exit; //allow only 1 caret
  Carets[0].GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then Exit;

  DoSelect_None;

  //calc insert-pos
  P:= ScreenToClient(Mouse.CursorPos);
  P:= ClientPosToCaretPos(P, bEolPos);
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

    //select moved text
    DoCaretSingle(0, 0);
    with Carets[0] do
    begin
      PosX:= PosAfter.X;
      PosY:= PosAfter.Y;
      EndX:= P.X;
      EndY:= P.Y;
    end;
  end
  else
  begin
    Strings.TextInsert(P.X, P.Y, Str, false, Shift, PosAfter);

    //select moved text
    DoCaretSingle(0, 0);
    with Carets[0] do
    begin
      PosX:= PosAfter.X;
      PosY:= PosAfter.Y;
      EndX:= P.X;
      EndY:= P.Y;
    end;

    Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
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

function TATSynEdit.GetModified: boolean;
begin
  Result:= Strings.Modified;
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
  Result:= Strings.TextString;
end;

procedure TATSynEdit.DoMinimapClick(APosY: integer);
var
  NItem: integer;
begin
  NItem:= (APosY-FRectMinimap.Top) div FCharSizeMinimap.Y + FScrollVertMinimap.NPos;
  if FWrapInfo.IsIndexValid(NItem) then
  begin
    NItem:= Max(0, NItem - GetVisibleLines div 2);
    FScrollVert.NPos:= Min(NItem, FScrollVert.NMax);
    Update;
  end;
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
  if not FOptAllowZooming then Exit;
  if not AInc then
    if Font.Size<=cMinFontSize then Exit;

  NTop:= LineTop;
  Font.Size:= Font.Size+BoolToPlusMinusOne(AInc);
  Update;
  AppProcessMessages;

  LineTop:= NTop;
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

function TATSynEdit.TextSelected: atString;
var
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  Result:= '';
  if Carets.Count=0 then Exit;
  Carets[0].GetRange(X1, Y1, X2, Y2, bSel);
  if bSel then
    Result:= Strings.TextSubstring(X1, Y1, X2, Y2);
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
  SFindWordBounds(Str, Caret.PosX, N1, N2, OptWordChars);
  if N2>N1 then
    Result:= Copy(Str, N1+1, N2-N1);
end;

function TATSynEdit.GetMouseNiceScroll: boolean;
begin
  Result:= FTimerNiceScroll.Enabled;
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
    Result.X:= Length(Strings.Lines[Result.Y]);
    if Strings.LinesEnds[Result.Y]<>cEndNone then
      Inc(Result.X);
  end;
end;

procedure TATSynEdit.DoPaintGutterFolding(C: TCanvas;
  AWrapItemIndex: integer;
  ACoordX1, ACoordX2, ACoordY1, ACoordY2: integer);
var
  List: TATIntArray;
  State: (cFoldbarNone, cFoldbarBegin, cFoldbarEnd, cFoldbarMiddle);
  CoordXM, CoordYM: integer;
  WrapItem: TATSynWrapItem;
  LineIndex: integer;
  IsPlus, IsLineUp, IsLineDown: boolean;
  i: integer;
  //
  procedure DrawUp;
  begin
    if IsLineUp then
      C.Line(
        CoordXM,
        ACoordY1,
        CoordXM,
        CoordYM
        );
  end;
  procedure DrawDown;
  begin
    if IsLineDown then
      C.Line(
        CoordXM,
        CoordYM,
        CoordXM,
        ACoordY2
        );
  end;
  //
begin
  if not FOptGutterShowFoldAlways then
    if not FCursorOnGutter then Exit;

  WrapItem:= FWrapInfo[AWrapItemIndex];
  LineIndex:= WrapItem.NLineIndex;

  List:= FFold.FindRangesContainingLines(LineIndex, LineIndex, nil,
    false{OnlyFolded}, false{TopLevelOnly}, cRngHasAllLines);
  if Length(List)=0 then Exit;

  //calc state
  State:= cFoldbarNone;
  IsPlus:= false;
  IsLineUp:= false;
  IsLineDown:= false;

  for i:= Low(List) to High(List) do
    with FFold[List[i]] do
    begin
      if Y<LineIndex then IsLineUp:= true;
      if Y2>LineIndex then IsLineDown:= true;
      if Y=LineIndex then
      begin
        State:= cFoldbarBegin;
        //don't override found [+], 2 blocks can start at same pos
        if not IsPlus then IsPlus:= Folded;
      end;
      if Y2=LineIndex then
        if State<>cFoldbarBegin then
          State:= cFoldbarEnd;
    end;

  //correct state for wrapped line
  if State=cFoldbarBegin then
    if not FWrapInfo.IsItemInitial(AWrapItemIndex) then
      State:= cFoldbarMiddle;

  //correct state for wrapped line
  if State=cFoldbarEnd then
    if WrapItem.NFinal=cWrapItemMiddle then
      State:= cFoldbarMiddle;

  C.Pen.Color:= IfThen(FOptGutterShowFoldLines,
    FColors.GutterFoldLine,
    FColors.GutterFoldBG);

  CoordXM:= (ACoordX1+ACoordX2) div 2;
  CoordYM:= (ACoordY1+ACoordY2) div 2;

  case State of
    cFoldbarBegin:
      begin
        if FOptGutterShowFoldLinesAll then
          begin DrawUp; DrawDown; end;

        if not IsPlus then
          DrawDown;

        CanvasPaintPlusMinus(C,
          FColors.GutterPlusBorder,
          FColors.GutterPlusBG,
          Point(CoordXM, CoordYM),
          FOptGutterPlusSize,
          IsPlus);
      end;
    cFoldbarEnd:
      begin
        if FOptGutterShowFoldLinesAll then
          begin DrawUp; DrawDown; end;

        Dec(ACoordY2, cSizeGutterFoldLineDx);
        C.Line(
          CoordXM,
          ACoordY1,
          CoordXM,
          ACoordY2
          );
        C.Line(
          CoordXM,
          ACoordY2,
          CoordXM + FOptGutterPlusSize,
          ACoordY2
          );
      end;
    cFoldbarMiddle:
      begin
        C.Line(
          CoordXM,
          ACoordY1,
          CoordXM,
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

procedure TATSynEdit.DoPaintTextHintTo(C: TCanvas);
var
  Size: TSize;
  Pos: TPoint;
begin
  C.Brush.Color:= GetColorTextBG;
  C.Font.Color:= FColors.TextHintFont;
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


procedure TATSynEdit.WMGetDlgCode(var Msg: TLMNoParams);
begin
  inherited;
  Msg.Result:= DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTALLKEYS;
  if FWantTabs and (GetKeyState(VK_CONTROL) >= 0) then
    Msg.Result:= Msg.Result or DLGC_WANTTAB;
end;

procedure TATSynEdit.DoPaintStaple(C: TCanvas; const R: TRect; AColor: TColor);
begin
  if FOptShowStapleStyle=cLineStyleNone then Exit;
  CanvasLineEx(C, AColor, FOptShowStapleStyle, Point(R.Left, R.Top), Point(R.Right, R.Top), false);
  CanvasLineEx(C, AColor, FOptShowStapleStyle, Point(R.Left, R.Top), Point(R.Left, R.Bottom), false);
  CanvasLineEx(C, AColor, FOptShowStapleStyle, Point(R.Left, R.Bottom), Point(R.Right, R.Bottom), true);
end;

procedure TATSynEdit.DoPaintStaples(C: TCanvas; const ARect: TRect;
  ACharSize: TPoint; const AScrollHorz: TATSynScrollInfo);
var
  nLineFrom, nLineTo, nIndent: integer;
  Indexes: TATIntArray;
  Range: TATSynRange;
  P1, P2: TPoint;
  i: integer;
  RSt: TRect;
  NColor: TColor;
begin
  if FOptShowStapleStyle=cLineStyleNone then Exit;
  nLineFrom:= LineTop;
  nLineTo:= LineBottom;
  Indexes:= FFold.FindRangesContainingLines(nLineFrom, nLineTo, nil,
    false{OnlyFolded}, false{TopLevelOnly}, cRngHasAnyOfLines);

  //c.font.color:= clblue;
  //c.textout(arect.right-150, arect.top, format('staples vis %d', [length(indexes)]));

  for i:= 0 to High(Indexes) do
  begin
    Range:= FFold[Indexes[i]];
    if not Range.Staple then Continue;
    if Range.Folded then Continue;

    if IsLineFolded(Range.Y, true) then Continue;
    if IsLineFolded(Range.Y2, true) then Continue;

    P1:= CaretPosToClientPos(Point(0, Range.Y));
    P2:= CaretPosToClientPos(Point(0, Range.Y2));
    if (P1.Y<0) and (Range.Y>=nLineFrom) then Continue;
    if (P2.Y<0) and (Range.Y2>=nLineFrom) then Continue;

    NIndent:= SGetIndentExpanded(Strings.Lines[Range.Y], FTabSize);
    Inc(P1.X, NIndent*ACharSize.X);
    Inc(P2.X, NIndent*ACharSize.X);

    RSt.Left:= P1.X + FOptShowStapleIndent;
    RSt.Top:= P1.Y;
    RSt.Right:= RSt.Left+ (ACharSize.X * FOptShowStapleWidthPercent div 100);
    RSt.Bottom:= P2.Y + ACharSize.Y-1;

    if (RSt.Left>=ARect.Left) and
      (RSt.Left<ARect.Right) then
    begin
      NColor:= Colors.BlockStaple;
      if Assigned(FOnCalcStaple) then
        FOnCalcStaple(Self, Range.Y, NIndent, NColor);
      DoPaintStaple(C, RSt, NColor);
    end;
  end;
end;


function TATSynEdit.IsCharWord(ch: Widechar): boolean;
begin
  Result:= ATStringProc.IsCharWord(ch, OptWordChars);
end;

function TATSynEdit.GetColorTextBG: TColor;
begin
  if Enabled then
    Result:= Colors.TextBG
  else
    Result:= Colors.TextDisabledBG;
end;

function TATSynEdit.GetColorTextFont: TColor;
begin
  if Enabled then
    Result:= Colors.TextFont
  else
    Result:= Colors.TextDisabledFont;
end;


{$I atsynedit_carets.inc}
{$I atsynedit_hilite.inc}
{$I atsynedit_sel.inc}
{$I atsynedit_fold.inc}
{$I atsynedit_debug.inc}
{$R res/nicescroll.res}
{$R res/foldbar.res}

{$I atsynedit_cmd_handler.inc}
{$I atsynedit_cmd_keys.inc}
{$I atsynedit_cmd_sel.inc}
{$I atsynedit_cmd_editing.inc}
{$I atsynedit_cmd_clipboard.inc}
{$I atsynedit_cmd_misc.inc}


initialization
  InitClipboardFormat;
  InitResourcesNicescroll;

finalization
  FreeResources;

end.

