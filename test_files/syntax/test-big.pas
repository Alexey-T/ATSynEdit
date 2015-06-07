{ *************************************************************************** }
{                                                                             }
{ EControl Syntax Editor SDK                                                  }
{                                                                             }
{ Copyright (c) 2004 - 2008 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{                                                                             }
{ *************************************************************************** }
{$I ecSyntEdit.INC}

unit ecSyntMemo;

interface

uses Windows, Messages, Classes, Controls, Graphics, StdCtrls, Forms, Menus, ImgList,
     ExtCtrls, ecStrUtils, ecSyntAnal, ecKeyMap, ecLists, Imm,
     {$IFDEF EC_DOTNET}Types, Variants, System.Text, {$ENDIF}
     ActiveX, ecOleDrag, ecMemoStrings, ecComplRend, ecEmbObj;

(*$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropTarget)' *)

type
  TCharGr = (cgSp, cgEOL, cgSymb, cgWord); //AT

const
  // Flags to GetStyleList
  slSubLexer     = $0001;
  slTextRange    = $0002;
  slToken        = $0004;
  slDynoRange    = $0008;
  slUserRange    = $0010;
  slEventHandle  = $0020;
  slSearchMarks  = $0040;

  slAllStyles    = $00FF;
  slPrintable    = $0037;
  slExported     = $0037;

  slStatic       = $0015;
  slDynoProc     = $006A;

const
  // Resolution of vertical scrollbar
  MaxLinesResolution = $7FFF;

type

  TCustomSyntaxMemo = class;
  TecTextMargin = class;

  {$IFDEF EC_VCL5}
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  {$ENDIF}

  TTabMode = (tmTabChar, tmSpaces, tmDialog, tmSmartTab);
  TIncSearchState = (isStop, isStart, isStrChange);

  TGetGutterImageEvent = procedure(Sender: TObject; const Line: integer;
                         List: TList) of object;
  TGetLineHighlightEvent = procedure(Sender: TObject; Line: integer; var Selected: Boolean;
                         var bgColor, frColor: TColor) of object;
  TGutterClickEvent = procedure(Sender: TObject; Line: integer;
                 Buton: TMouseButton; Shift: TShiftState; XY: TPoint) of object;
  TGetTokenHintEvent = procedure(Sender: TObject; TokenIndex: integer;
                 var HintText: string) of object;
  TAfterInsertChar = procedure(Sender: TObject; C: ecChar; APos: integer) of object;

  TDrawTokenEvent = procedure(Sender: TObject; Rect: TRect; TextPos: TPoint;
                              CharCount: integer) of object;
  TLineDrawEvent = procedure(Sender: TObject; Rect: TRect; Line: integer) of object;
  TAnimateEvent = procedure(Sender: TObject; Pt: TPoint;  Size: integer) of object;
  TIncSearchChangeEvent = procedure(Sender: TObject; State: TIncSearchState) of object;
  TBeforeDrawStaple = procedure(Sender: TObject; Canvas: TCanvas; Staple: TBlockStaple; Level: integer) of object;
  TCanInsertEvent = procedure(Sender: TObject; Position: integer; var AllowInsert: boolean) of object;
  TCanDeleteEvent = procedure(Sender: TObject; Position, Count: integer; var AllowDelete: boolean) of object;
  TDragMarginEvent = procedure(Sender: TObject; Margin: TecTextMargin; var Pos: integer) of object;
  TSyntShowHintEvent = procedure(Sender: TObject; const HintStr: string; var HintObj: THintWindow) of object; //AT2
  TGetGutterBandColor = procedure(Sender: TObject; NBand: Integer; NLine: Integer; var NColor: TColor) of object; //AT

  TSyntMemoPlugin = class(TComponent)
  private
    FSyntMemo: TCustomSyntaxMemo;
    procedure SetSyntMemo(const Value: TCustomSyntaxMemo);
  protected
    property SyntMemo: TCustomSyntaxMemo read FSyntMemo write SetSyntMemo;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
  end;

  TCollapsedRange = class(TSortedItem)
  private
    FLine: integer;
    FPos: integer;        // Start of text range
    FEnd: integer;        // End of text range
    FLineCount: integer;  // Collapsed lines count
    FValide: Boolean;     // if False => request refresh
    FUser: Boolean;       // Is a independent custom range
    FColText: ecString;   // Text icon
  protected
    function GetKey: integer; override;
    procedure CopyFrom(Other: TCollapsedRange);
    constructor CreateEmpty;
  public
    constructor Create(ALine, APos, AEnd, ALineCount: integer);
    property StartPos: integer read FPos;
    property EndPos: integer read FEnd;
    property Line: integer read FLine;
    property LineCount: integer read FLineCount;
    property ColText: ecString read FColText;
    property IsUser: Boolean read FUser;
  end;

  TCollapsedRanges = class(TPersistent)
  private
    FList: TSortedList;
    FDiaps: TRangeList;
    procedure UpdateDiaps;
    function GetCount: integer;
    function GetItem(Index: integer): TCollapsedRange;
    function CorrectCollapsed(APos, Count, LineCountChanged: integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Clear;   // FullExpand
    procedure Add(Range: TCollapsedRange);
    procedure Delete(Index: integer);

    function GetCollapsed(aLine: integer): TCollapsedRange;
    function GetCollapsedIndex(aLine: integer): integer;
    function IsLineVisible(Line: integer): Boolean;

    property Count: integer read GetCount;
    property Ranges[Index: integer]: TCollapsedRange read GetItem; default;
  end;

  TOnCheckLine = procedure(Sender: TObject; Line: integer; var Show: Boolean) of object;
  TOnGutterObjectClick = procedure(Sender: TObject; Line: integer; Shift: TShiftState) of object;

  TCustomGutterObject = class(TCollectionItem)
  private
    FMargin: integer;
    FLine: integer;
    FImageIndex: TImageIndex;
    FHint: string;
    FImageList: TCustomImageList;
    FSelInvertColors: Boolean;
    FBgColor: TColor;
    FForeColor: TColor;
    FOnCheckLine: TOnCheckLine;
    FBand: integer;
    FPopupMenu: TPopupMenu;
    FBounds: TRect;                   // Bounds of the image (for internal use)
    FOnClick: TOnGutterObjectClick;
    FTag: integer;
    FName: ecString;
    FCursor: TCursor;
    FSubLine: integer;
    procedure SetBgColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SeTCustomImageList(const Value: TCustomImageList);
    procedure SetLine(const Value: integer);
    procedure SetMargin(const Value: integer);
    procedure SetSelInvertColors(const Value: Boolean);
    procedure SetBand(const Value: integer);
    procedure Init;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure DoFreeNotification(Cmp: TComponent);
    procedure SetCursor(const Value: TCursor);
    procedure SetSubLine(const Value: integer);
  protected
    function CheckLine(ALine: integer): Boolean; virtual;
    property Line: integer read FLine write SetLine default -1;
    property SubLine: integer read FSubLine write SetSubLine default -1;
    property OnCheckLine: TOnCheckLine read FOnCheckLine write FOnCheckLine;
    property OnClick: TOnGutterObjectClick read FOnClick write FOnClick;
  public
    constructor CreateObj(ALine: integer; AImageIndex: TImageIndex);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageList: TCustomImageList read FImageList write SeTCustomImageList;
    property Hint: string read FHint write SetHint;
    property Margin: integer read FMargin write SetMargin default -1;
    property Band: integer read FBand write SetBand default -1;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    // Line highlight
    property ForeColor: TColor read FForeColor write SetForeColor default clNone;
    property BgColor: TColor read FBgColor write SetBgColor default clNone;
    property SelInvertColors: Boolean read FSelInvertColors write SetSelInvertColors default False;
    property Tag: integer read FTag write FTag;
    property Name: ecString read FName write FName;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
  end;

  TGutterObject = class(TCustomGutterObject)
  published
    property Line;
    property SubLine;
    property OnCheckLine;
    property OnClick;
  end;

  TGutterObjects = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TGutterObject;
    procedure SetItem(Index: integer; const Value: TGutterObject);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TGutterObject;
    property Items[Index: integer]: TGutterObject read GetItem write SetItem; default;
  end;

  TTabList = class(TPersistent)
  private
    FList: TList;
    FOwner: TCustomSyntaxMemo;
    function GetAsString: string;
    function GetCount: integer;
    function GetItems(Index: integer): integer;
    procedure SetAsString(const Value: string);
    procedure SetItems(Index: integer; const Value: integer);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;

    procedure Add(NewTab: integer);
    procedure Delete(Index: integer);
    function NextTab(pos: integer; AllowZeroTab: Boolean = False): integer;
    property Count: integer read GetCount;
    property Items[Index: integer]: integer read GetItems write SetItems; default;
    property Owner: TCustomSyntaxMemo read FOwner;
  published
    property AsString: string read GetAsString write SetAsString;
  end;

  TNonPrinted = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FVisible: Boolean;
    FUseFont: Boolean;
    FSpaceChar: Char;
    FTabChar: Char;
    FLineBreakChar: Char;
    FFont: TFont;
    FSoftLineBreakChar: Char;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetLineBreakChar(const Value: Char);
    procedure SetSpaceChar(const Value: Char);
    procedure SetTabChar(const Value: Char);
    procedure SetUseFont(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    function GetColor: TColor;
    procedure SetSoftLineBreakChar(const Value: Char);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Owner: TCustomSyntaxMemo read FOwner;
  published
    property Visible: Boolean read FVisible write SetVisible default False;
    property Color: TColor read GetColor write SetColor default clSilver;
    property UseFont: Boolean read FUseFont write SetUseFont default False;
    property Font: TFont read FFont write SetFont;
    property TabChar: Char read FTabChar write SetTabChar default '›';
    property SpaceChar: Char read FSpaceChar write SetSpaceChar default '·';
    property LineBreakChar: Char read FLineBreakChar write SetLineBreakChar default '¶';
    property SoftLineBreakChar: Char read FSoftLineBreakChar write SetSoftLineBreakChar default '¬';
  end;

  TGetLineNumberStringEvent = procedure(Sender: TObject; Line: integer;
                                    var NumberStr: string) of object;

  TLineNumberingStyle = (lsDefault, lsBDS, lsEach5, lsOnlyCurent,
     lsUser1, lsUser2, lsUser3);

  TWordNavigation = (wnWordStart, wnWordEdge, wnWordSpaceEdge, wnEclipse);

  TLineNumbers = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FVisible: Boolean;
    FUnderColor: TColor;
    FFont: TFont;
    FMargin: integer;
    FAlignment: TAlignment;
    FVertAlignment: TVertAlignment;
    FBand: integer;
    FFirstLineNumber: integer;
    FNumberingStart: integer;
    FNumberingEnd: integer;
    FNumberingStyle: TLineNumberingStyle;
    FAutoSize: Boolean;
    procedure Update;
    procedure UpdateBandWidth;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetVisible(const Value: Boolean);
    procedure FontChanged(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetMargin(const Value: integer);
    procedure SetVertAlignment(const Value: TVertAlignment);
    procedure SetBand(const Value: integer);
    procedure SetFirstLineNumber(const Value: integer);
    procedure SetNumberingEnd(const Value: integer);
    procedure SetNumberingStart(const Value: integer);
    procedure SetNumberingStyle(const Value: TLineNumberingStyle);
    procedure SetAutoSize(const Value: Boolean);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetNumberString(Line: integer): string;
    property Owner: TCustomSyntaxMemo read FOwner;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property UnderColor: TColor read FUnderColor write SetColor default clNone;
    property Margin: integer read FMargin write SetMargin default 2;
    property Alignment: TAlignment read FAlignment write SetAlignment default taRightJustify;

    property VertAlignment: TVertAlignment read FVertAlignment write SetVertAlignment default vaCenter;
    property Font: TFont read FFont write SetFont;
    property Band: integer read FBand write SetBand;
    property FirstLineNumber: integer read FFirstLineNumber write SetFirstLineNumber default 1;
    property NumberingStart: integer read FNumberingStart write SetNumberingStart default -1;
    property NumberingEnd: integer read FNumberingEnd write SetNumberingEnd default -1;
    property NumberingStyle: TLineNumberingStyle read FNumberingStyle write SetNumberingStyle default lsDefault;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
  end;

  TGutterBand = class(TCollectionItem)
  private
    FWidth: integer;
    FRightBound: TColor;
    FColor: TColor;
    FLeftBound: TColor;
    FGradient: Boolean;
    FGradientRight: TColor;
    FCursor: TCursor;
    FMouseMoveCaret: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetLeftBound(const Value: TColor);
    procedure SetRightBound(const Value: TColor);
    procedure SetWidth(const Value: integer);
    procedure SetGradient(const Value: Boolean);
    procedure SetGradientRight(const Value: TColor);
    procedure SetCursor(const Value: TCursor);
    procedure SetMouseMoveCaret(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Width: integer read FWidth write SetWidth;
    property Color: TColor read FColor write SetColor default clNone;
    property LeftBound: TColor read FLeftBound write SetLeftBound default clNone;
    property RightBound: TColor read FRightBound write SetRightBound default clNone;
    property Gradient: Boolean read FGradient write SetGradient default False;
    property GradientRight: TColor read FGradientRight write SetGradientRight default clWhite;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property MouseMoveCaret: Boolean read FMouseMoveCaret write SetMouseMoveCaret default True;
  end;

  TGutterBands = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TGutterBand;
    procedure SetItem(Index: integer; const Value: TGutterBand);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TGutterBand;
    property Items[Index: integer]: TGutterBand read GetItem write SetItem; default;
  end;

  TGutter = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FVisible: Boolean;
    FWidth: integer;
    FExpandButtons: TBitmap;
    FColor: TColor;
    FImages: TCustomImageList;
    FBands: TGutterBands;
    FObjects: TGutterObjects;
    FExpBtnBand: integer;
    FShowSeparator: Boolean;
    FShowCollapseLine: Boolean;
    FCollapsePen: TPen;
    FBuffer: TBitmap;
    FPopupMenu: TPopupMenu;
    FSeparatorColor: TColor;
    FAutoSize: Boolean;
    FCursor: TCursor;
    FMouseMoveCaret: Boolean;
    FLineBreakObj: integer;
    procedure SetColor(const Value: TColor);
    procedure SetExpandButtons(const Value: TBitmap);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: integer);
    procedure SetBands(const Value: TGutterBands);
    procedure SetObjects(const Value: TGutterObjects);
    procedure SetExpBtnBand(const Value: integer);
    procedure SetShowSeparator(const Value: Boolean);
    procedure SetCollapsePen(const Value: TPen);
    procedure SetShowCollapseLine(const Value: Boolean);
    function  GetDoubleBufered: Boolean;
    procedure SetDoubleBufered(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetCursor(const Value: TCursor);
    procedure SetMouseMoveCaret(const Value: Boolean);
    procedure SetLineBreakObj(const Value: integer);
  protected
    function GetOwner: TPersistent; override;
    function RecalcWidth: Boolean;
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBandBound(BandIndex: integer): TPoint;
    function BandAt(X: integer): integer;
    function ExpBtnRect(Y1, Y2: integer): TRect;
    function MouseMoveCaretAt(X: integer): Boolean;
    property Owner: TCustomSyntaxMemo read FOwner;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: integer read FWidth write SetWidth default 30;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Images: TCustomImageList read FImages write SetImages;
    property ExpandButtons: TBitmap read FExpandButtons write SetExpandButtons;
    property Bands: TGutterBands read FBands write SetBands;
    property Objects: TGutterObjects read FObjects write SetObjects;
    property ExpBtnBand: integer read FExpBtnBand write SetExpBtnBand;
    property ShowSeparator: Boolean read FShowSeparator write SetShowSeparator default True;
    property CollapsePen: TPen read FCollapsePen write SetCollapsePen;
    property ShowCollapseLine: Boolean read FShowCollapseLine write SetShowCollapseLine default True;
    property DoubleBufered: Boolean read GetDoubleBufered write SetDoubleBufered default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default clGray;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Cursor: TCursor read FCursor write SetCursor default crArrow;
    property MouseMoveCaret: Boolean read FMouseMoveCaret write SetMouseMoveCaret default False;
    property LineBreakObj: integer read FLineBreakObj write SetLineBreakObj default -1;
  end;

  TSyntShowHint = (shScroll, shCollapsed, shGutter, shTokens);
  TSyntShowHints = set of TSyntShowHint;

  THintProps = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FTimeGutter: integer;
    FTimeTokens: integer;
    FDelayBefore: integer;
    FTimeCollapsed: integer;
    FShowHints: TSyntShowHints;
    FCollapsedLines: integer;
    FFormated: Boolean;
    FImages: TCustomImageList;
    FStyles: TSyntStyles;
    FShowFirstLine: Boolean;
    function GetColor: TColor;
    function GetFont: TFont;
    procedure SetColor(const Value: TColor);
    procedure SetDelayBefore(const Value: integer);
    procedure SetFont(const Value: TFont);
    procedure SetShowHints(const Value: TSyntShowHints);
    procedure SetTimeCollapsed(const Value: integer);
    procedure SetTimeGutter(const Value: integer);
    procedure SetTimeTokens(const Value: integer);
    procedure SetCollapsedLines(const Value: integer);
    procedure SetFormated(const Value: Boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetStyles(const Value: TSyntStyles);
    procedure SetShowFirstLine(const Value: Boolean);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    procedure Assign(Source: TPersistent); override;

    procedure SetHintLines(StartLine, EndLine: integer);
  published
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor;
    property ShowHints: TSyntShowHints read FShowHints write SetShowHints;
    property DelayBefore: integer read FDelayBefore write SetDelayBefore default 500;
    property TimeCollapsed: integer read FTimeCollapsed write SetTimeCollapsed default 10000;
    property TimeGutter: integer read FTimeGutter write SetTimeGutter default 5000;
    property TimeTokens: integer read FTimeTokens write SetTimeTokens default 10000;
    property CollapsedLines: integer read FCollapsedLines write SetCollapsedLines default 20;
    property Styles: TSyntStyles read FStyles write SetStyles;
    property Images: TCustomImageList read FImages write SetImages;
    property Formated: Boolean read FFormated write SetFormated default False;
    property ShowFirstLine: Boolean read FShowFirstLine write SetShowFirstLine default False;
  end;

  TSyntHintWindow = class(THintWindow)
  protected
    FirstLine, LastLine: integer;
    FLineInfos: TList;
    FComplRender: TComplexRender;
    procedure Paint; override;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT; //!!
  public
    destructor Destroy; override;
    function CalcHintRect(MaxWidth: Integer; const AHint: String;
      AData: ecPointer): TRect; override;
    // For lines rendering
    procedure SetLines(First, Last: integer);
    // Resets custom formatting
    procedure ResetLines;
  end;

  TUserRange = class(TCollectionItem)
  private
    FStartPos, FEndPos: integer;
    FStartLine, FEndLine: integer;
    FStyle: string;
    FLineBreaks: TLineBreakBound;
    FCollapsable: Boolean;
    FLineBreakColor: TColor;
    FHighlightLines: Boolean;
    FTag: integer;
    FCollapseIcon: string;
    procedure SetCollapsable(const Value: Boolean);
    procedure SetEndPos(const Value: integer);
    procedure SetLineBreaks(const Value: TLineBreakBound);
    procedure SetStartPos(const Value: integer);
    procedure SetStyle(const Value: string);
    procedure SetLineBreakColor(const Value: TColor);
    procedure SetEndLine(const Value: integer);
    procedure SetStartLine(const Value: integer);
    procedure SetHighlightLines(const Value: Boolean);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetFlags: integer;
    procedure SetFlags(const Value: integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(const AStartPos, AEndPos: integer);

    property AsString: string read GetAsString write SetAsString;
    property Flags: integer read GetFlags write SetFlags;
  published
    property StartPos: integer read FStartPos write SetStartPos default 0;
    property EndPos: integer read FEndPos write SetEndPos default 0;
    property StartLine: integer read FStartLine write SetStartLine stored False;
    property EndLine: integer read FEndLine write SetEndLine stored False;
    property Style: string read FStyle write SetStyle;
    property LineBreaks: TLineBreakBound read FLineBreaks write SetLineBreaks default [];
    property LineBreakColor: TColor read FLineBreakColor write SetLineBreakColor default clBlue;
    property Collapsable: Boolean read FCollapsable write SetCollapsable default False;
    property HighlightLines: Boolean read FHighlightLines write SetHighlightLines;
    property Tag: integer read FTag write FTag;
    property CollapseIcon: string read FCollapseIcon write FCollapseIcon;
  end;

  TUserRanges = class(TCollection)
  private
    FOwner: TCustomSyntaxMemo;
    FActive: Boolean;
    FFormatted: TList;
    FCollapsible: TList;
    FWithSeparators: TList;
    procedure SetActive(const Value: Boolean);
    function GetItem(Index: integer): TUserRange;
  protected
    procedure Update(Item: TCollectionItem); override;
    function FindStyle(const StyleName: string): TSyntaxFormat;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;

    function Add: TUserRange;
    function AddRange(AStartPos, AEndPos: integer; ACollapsable: Boolean = False;
              AStyle: string = ''; ALineBreaks: TLineBreakBound = []): TUserRange;

    function GetLineState(Line: integer): integer;
    function GetCollapsible(Line: integer): TUserRange;
    function GetLineBreak(Line: integer): TColor;
    function ApplyStyles(Canvas: TCanvas; Pos: integer): integer; // --
    function GetRangeStyles(Pos: integer; List: TStyleEntries): integer;
    procedure GetLineHighlight(Line: integer; var InvSel: Boolean; var bgColor, frColor: TColor);
    function RangeAtPos(Pos: integer): TUserRange;
    function RangeAtSel(SelStart, SelLength: integer): TUserRange;

    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    property Items[Index: integer]: TUserRange read GetItem; default;
  published
    property Active: Boolean read FActive write SetActive default True;
  end;

  TBookmark = class(TCustomGutterObject)
  private
    FBmIndex: integer;
    FPosition: integer;
    FModified: Boolean;
    FAllowDelete: Boolean;
    procedure SetPosition(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;

    property BmIndex: integer read FBmIndex write FBmIndex;
    property Position: integer read FPosition write SetPosition;
    property Modified: Boolean read FModified write FModified;
    property AllowDelete: Boolean read FAllowDelete write FAllowDelete;
  end;

  TBookmarks = class(TCollection)
  private
    FOwner: TCustomSyntaxMemo;
    function GetItem(Index: integer): TBookmark;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    function Add: TBookmark;
    property Items[Index: integer]: TBookmark read GetItem; default;
  end;

  TMarker = class
  private
    FScrollPos: TPoint;
    FCaretPos: TPoint;
    FPos: integer;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    procedure CopyFrom(Other: TMarker);

    property AsString: string read GetAsString write SetAsString;
    property ScrollPos: TPoint read FScrollPos;
    property CaretPos: TPoint read FCaretPos;
    property Position: integer read FPos;
  end;

  TAnimationType = (atNone, atCircle, atRect, atRoundRect, atEllipse, atCustom);

  TSyntAnimation = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FTimer: TTimer;
    FPoint: TPoint;
    FSize: integer;
    FHaveDrawn: Boolean;
    FEnabled: Boolean;
    FRadius: integer;
    FStep: integer;
    FAnimType: TAnimationType;
    FMarkerAnim: TAnimationType;
    FBookmarkAnim: TAnimationType;
    procedure DrawAnim;
    procedure DoTimer(Sender: TObject);
    function GetInterval: integer;
    procedure SetInterval(const Value: integer);
    procedure SetStep(const Value: integer);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Animate(Pt: TPoint; AnimType: TAnimationType);
    procedure Reset;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Interval: integer read GetInterval write SetInterval default 10;
    property Radius: integer read FRadius write FRadius default 100;
    property Step: integer read FStep write SetStep default 10;
    property MarkerAnim: TAnimationType read FMarkerAnim write FMarkerAnim default atCircle;
    property BookmarkAnim: TAnimationType read FBookmarkAnim write FBookmarkAnim default atRect;
  end;

  // New in v2.02 (Default styles holder)
  TDefaultStyles = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FSearchMark: TSyntaxFormat;
    FSelectioMark: TSyntaxFormat;
    FCurrentLine: TSyntaxFormat;
    FCollapseMark: TSyntaxFormat;
    procedure SetCurrentLine(const Value: TSyntaxFormat);
    procedure SetSearchMark(const Value: TSyntaxFormat);
    procedure SetSelectioMark(const Value: TSyntaxFormat);
    procedure StyleChanged(Sender: TObject);
    procedure SetCollapseMark(const Value: TSyntaxFormat);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property SelectioMark: TSyntaxFormat read FSelectioMark write SetSelectioMark;
    property SearchMark: TSyntaxFormat read FSearchMark write SetSearchMark;
    property CurrentLine: TSyntaxFormat read FCurrentLine write SetCurrentLine;
    property CollapseMark: TSyntaxFormat read FCollapseMark write SetCollapseMark;
  end;

  // v2.15
  TLineInfo = class
    FWordBreaks: TList;  // List of word-break positions
    FHeights: TList;     // List of single line heights
    FWidths: TList;      // List of single line widths
    FHidden: Boolean;    // All line text is hidden
    FInvalid: Boolean;
    FManualHidden: Boolean; // Line was hidden by setting Hidden property
  private
    function GetCount: integer;
    function GetTotalHeight: integer;
    function GetHeights(Index: integer): integer;
    function GetWordBreak(Index: integer): integer;
    procedure SetHeights(Index: integer; const Value: integer);
    procedure SetWordBreak(Index: integer; const Value: integer);
    function GetHidden: Boolean;
    procedure SetHidden(const Value: Boolean);
    function GetTotalWidth: integer;
    function GetWidths(Index: integer): integer;
    procedure SetWidths(Index: integer; const Value: integer);
  protected
    property Invalid: Boolean read FInvalid;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddWordBreak(APos: integer);
    function SubLineForPos(Pos: integer): integer;
    procedure Clear;

    property Height: integer read GetTotalHeight;
    property LineCount: integer read GetCount;
    property Heights[Index: integer]: integer read GetHeights write SetHeights;
    property Widths[Index: integer]: integer read GetWidths write SetWidths;
    // WordBreaks[LineCount - 1] = 0x7FFFFFFF
    property WordBreaks[Index: integer]: integer read GetWordBreak write SetWordBreak;
    // After seting Hidden to True you should call TSyntaxMemo.CalcLineCount manually
    property Hidden: Boolean read GetHidden write SetHidden;
    property Width: integer read GetTotalWidth;
  end;

  //v2.15
  TLineStateDisplay = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FBand: integer;
    FNewColor: TColor;
    FModifiedColor: TColor;
    FUnchangedColor: TColor;
    FSavedColor: TColor;
    procedure SetBand(const Value: integer);
    procedure SetModifiedColor(const Value: TColor);
    procedure SetNewColor(const Value: TColor);
    procedure SetSavedColor(const Value: TColor);
    procedure SetUnchangedColor(const Value: TColor);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    procedure Assign(Source: TPersistent); override;
  published
    property Band: integer read FBand write SetBand default -1;
    property UnchangedColor: TColor read FUnchangedColor write SetUnchangedColor default clNone;
    property ModifiedColor: TColor read FModifiedColor write SetModifiedColor default clYellow;
    property NewColor: TColor read FNewColor write SetNewColor default clGreen;
    property SavedColor: TColor read FSavedColor write SetSavedColor default clNavy;
  end;

  TCanSyncEditEvent = procedure(Sender: TObject; SyncList: TRangeList;
                           var CanSynchronize: Boolean) of object;

  // Synchronized editing (v2.20)
  TSyntSyncEdit = class(TPersistent)
  private
    FOwner: TCustomSyntaxMemo;
    FSyncList: TRangeList;
    FInactiveWordsStyle: TSyntaxFormat;
    FActiveWordsStyle: TSyntaxFormat;
    FSyncRangeStyle: TSyntaxFormat;
    FActiveCoord: TPoint;
    FEnabled: Boolean;
    FIgnoreCase: Boolean;
    procedure SetActiveWordsStyle(const Value: TSyntaxFormat);
    procedure SetInactiveWordsStyle(const Value: TSyntaxFormat);
    procedure SetSyncRangeStyle(const Value: TSyntaxFormat);
    procedure SetEnabled(const Value: Boolean);
    function GetCount: integer;
    function GetSyncRange(Index: integer): TRange;
  protected
    procedure TextChanged(Pos, Count: integer);
    function GetStyleList(CurPos: integer; List: TStyleEntries): integer; // Formatting
    function CaretPosChanged: Boolean;
    function GetSyncPositions(Pos, Count: integer): TList;
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function AddRange(AStart, AEnd: integer): Boolean;
    function AddCurSelection: Boolean;
    procedure Validate;

    function RangeEndAtLine(Line: integer): integer;
    procedure Delete(Index: integer);
    function SyncRangeAt(Pos: integer): integer;
    property SyncRanges[Index: integer]: TRange read GetSyncRange; default;
    property Count: integer read GetCount;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property SyncRangeStyle: TSyntaxFormat read FSyncRangeStyle write SetSyncRangeStyle;
    property ActiveWordsStyle: TSyntaxFormat read FActiveWordsStyle write SetActiveWordsStyle;
    property InactiveWordsStyle: TSyntaxFormat read FInactiveWordsStyle write SetInactiveWordsStyle;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase default false;
  end;

  TFillStyle = (fstNone, fstHorzGradient, fstVertGradient, fstCenter, fstTile,
                fstStretch);

  TecBackGround = class(TPersistent)
  private
    FBitmap: TBitmap;
    FGradColor: TColor;
    FFillStyle: TFillStyle;
    FOnChange: TNotifyEvent;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetFillStyle(const Value: TFillStyle);
    procedure SetGradColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOnChange: TNotifyEvent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure FillRect(Canvas: TCanvas; const R: TRect; DefColor: TColor);
    function IsDefault: Boolean;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property FillStyle: TFillStyle read FFillStyle write SetFillStyle default fstNone;
    property GradColor: TColor read FGradColor write SetGradColor default clBlue;
  end;

  TSyntSelectionMode = (msNone, msNormal, msColumn, msLine);
  TCollapseStyle = (csLineSeparator, csRegionName, csNameWhenDefined, csEndEllipsis);

  TLineIterator = procedure(Line: integer; Info: TLineInfo;
                            var Param1, Param2: integer; var Stop: Boolean) of object;

  TSetBookmarkEvent = procedure(Snder: TObject; Bookmark: TBookmark;
                                var Accept: Boolean) of object;
  TGetStyleEntryEvent = procedure(Sender: TObject; CurPos: integer;
                      StyleList: TStyleEntries; var NextPos: integer) of object;
  TGetMouseSelMode = procedure(Sender: TObject; Shift: TShiftState;
                      var SelMode: TSyntSelectionMode) of object;
  TBookmarkDeleteEvent = procedure(Sender: TObject; Bookmark: TBookmark;
                       var AllowDelete: Boolean) of object;
  TecOleBeginDragText = procedure(Sender: TObject; var DragText: WideString; var OnlyCopy: Boolean) of object;
  TGetLineStyleEvent = procedure(Sender: TObject; Line: integer; var Style: TSyntaxFormat) of object;
  TGetRightMarginEvent = procedure(Sender: TObject; Line: integer; var RMargin: integer; RightMarginPen: TPen) of object;
  TBeforeInsertEvent = procedure(Sender: TObject; var Pos: integer; var S: ecString; var Accept: Boolean) of object;
  TBeforeDeleteEvent = procedure(Sender: TObject; var Pos, Count: integer; var Accept: Boolean) of object;
  TAfterCalcLineInfo = procedure(Sender: TObject; LineInfo: TLineInfo; Line: integer) of object;

  TSyntaxMemoOption = (soOverwriteBlocks, soPersistentBlocks, soEnableBlockSel,
                       soDoubleClickLine, soKeepCaretInText, soCopyAsRTF,
                       soHideSelection, soHideDynamic, soAutoIndentMode,
                       soBackUnindent, soGroupUndo, soGroupRedo,
                       soFixedLineHeight, soDragText, soCallapseEmptyLines,
                       soAutoSelect, soKeepTrailingBlanks, soFloatMarkers,
                       soUndoAfterSave, soDisableSelection, soAlwaysShowCaret,
                       soDrawCurLineFocus, soHideCursorOnType, soScrollLastLine,
                       soGreedySelect, soKeepSelMode, soSmartCaret,
                       soBreakOnRightMargin,  soOptimalFill, soFixedColumnMove,
                       soVariableHorzScrollBar, soUnindentKeepAlign);
  TSyntaxMemoOptionEx = (soSmartPaste, soUseCaseFormat, soAutoFormat, soKeepSearchMarks,
                         soExtractAnsiParts, soCorrectNonPrinted, soVirtualCaretPos,
                         soUnlimitedCaretPos, soNormalSelToLineEnd, soRightClickMoveCaret,
                         soDisableAutoClose, soAllowZeroTab, soNotSuppressAltNNNN,
                         eoShowCaretWhenUnfocused, soKeepCaretPaste,
                         soAllowSelectByWords, soSimplifiedUndo//AT
                         );

  TSyntaxMemoOptions = set of TSyntaxMemoOption;
  TSyntaxMemoOptionsEx = set of TSyntaxMemoOptionEx;

  TSyntMemoPrintOptions = set of (mpWordWrap, mpLineHighlight, mpBlockHighlight,
                       mpBackColor, {mpLineNumbers, }mpHideCollapsed, mpBlockStaples);

  TMonoFontMode = (mfAuto, mfFixed, mfVariable);

  TSaveStateOptions = set of (ssoAutoSave, ssoAutoLoad, ssoCaretPos, ssoSelection,
                       ssoUserRanges, ssoWordWrap, ssoNonPrinted, ssoDisableFolding,
                       ssoScrollPos, ssoMarkers, ssoBookmarks);

  TScrollAlignment = (saNone, saNearestEdge, saMinEdge, saCenter, saMaxEdge);
  
  // Constants for ProcessLine
  TProcessLineOper = (   // Param.x       Param.y        Result.x         Result.y
    ptDrawing,           // First subline Last subline   -                -
    ptLineHeight,        // -             -              -                -
    ptMouseToText,       // x pos         y offset       in line pos      -
    ptTextToMouse,       // In line pos   -              x pos            offset from line top
    ptTestOverCollapsed, // x pos         y offset       test result (bool)
    ptTestOverImage);    // x pos         y offset       in-text image index

  TecMemoStateStorage = class(TComponent)
  protected
    FCaretPos: TPoint;
    FScrollPos: TPoint;
    FSelMode: TSyntSelectionMode;
    FSelBlock: TRect;
    FSelRange: TPoint;
    FReplaceMode: Boolean;
    FWordWrap: Boolean;
    FDisableFolding: Boolean;
    FUserRanges: TUserRanges;
    FBookmarks: TBookmarks;
    FColRanges: TCollapsedRanges;
    FMarkers: TList;
    FModified: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CopyMarkers(Src, Dest: TList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Bookmarks: TBookmarks read FBookmarks write FBookmarks;
    property CaretPos: TPoint read FCaretPos write FCaretPos;
    property ColRanges: TCollapsedRanges read FColRanges write FColRanges;
    property DisableFolding: Boolean read FDisableFolding write FDisableFolding;
    property Markers: TList read FMarkers write FMarkers;
    property Modified: Boolean read FModified write FModified;
    property ReplaceMode: Boolean read FReplaceMode write FReplaceMode;
    property ScrollPos: TPoint read FScrollPos write FScrollPos;
    property SelBlock: TRect read FSelBlock write FSelBlock;
    property SelMode: TSyntSelectionMode read FSelMode write FSelMode;
    property SelRange: TPoint read FSelRange write FSelRange;
    property UserRanges: TUserRanges read FUserRanges write FUserRanges;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
  end;

  TSelRgnProc = function(StartPos, EndPos: integer; var Data: TSyntaxFormat): Boolean of object;

  TecTextAttributes = class
  private
    FOwner: TCustomSyntaxMemo;
    function ApplyStyle_Int(StartPos, EndPos: integer; var Data: TSyntaxFormat): Boolean;
    function GetSelStyle_Int(StartPos, EndPos: integer; var Data: TSyntaxFormat): Boolean;
    function ClearFormat_Int(StartPos, EndPos: integer; var Data: TSyntaxFormat): Boolean;

    procedure ApplyStyle(Stl: TSyntaxFormat);
    function CreateEmptyStyle: TSyntaxFormat;
    function IterateSelection(Proc: TSelRgnProc; var Data: TSyntaxFormat): Boolean;
    function GetFontStyle(const Index: integer): Boolean;
    procedure SetFontStyle(const Index: integer; const Value: Boolean);
    function GetBgColor: TColor;
    function GetCharset: TFontCharset;
    function GetColor: TColor;
    function GetFontName: TFontName;
    function GetFontSize: integer;
    procedure SetBgColor(const Value: TColor);
    procedure SetCharset(const Value: TFontCharset);
    procedure SetColor(const Value: TColor);
    procedure SetFontName(const Value: TFontName);
    procedure SetFontSize(const Value: integer);
    function GetHidden: Boolean;
    function GetReadOnly: Boolean;
    procedure SetHidden(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;
  public
    constructor Create(AOwner: TCustomSyntaxMemo);

    function CanFormat: Boolean;
    procedure ClearSelFormat;
    procedure ClearFormat;
    function GetSelStyle: TSyntaxFormat;

    property Bold: Boolean index 0 read GetFontStyle write SetFontStyle;
    property Italic: Boolean index 1 read GetFontStyle write SetFontStyle;
    property Underline: Boolean index 2 read GetFontStyle write SetFontStyle;
    property StrikeOut: Boolean index 3 read GetFontStyle write SetFontStyle;

    property Name: TFontName read GetFontName write SetFontName;
    property Size: integer read GetFontSize write SetFontSize;
    property Charset: TFontCharset read GetCharset write SetCharset;

    property Color: TColor read GetColor write SetColor;
    property BgColor: TColor read GetBgColor write SetBgColor;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Hidden: Boolean read GetHidden write SetHidden;

    property Enabled: Boolean read GetEnabled write SetEnabled default True;
  end;

  TecHorzRuler = class(TPersistent)
  private
    FLabelInterval: integer;
    FMinorTicksInterval: integer;
    FMajorTicksLength: integer;
    FMinorTicksLength: integer;
    FMajorTicksInterval: integer;
    FColor: TColor;
    FBackGround: TecBackGround;
    FFont: TFont;
    FHeight: integer;
    FOwner: TCustomSyntaxMemo;
    FOnChange: TNotifyEvent;
    FSeparatorColor: TColor;
    FCurPos: integer;
    FShowCurrentPos: Boolean;
    FMajorTicksColor: TColor;
    FMinorTicksColor: TColor;
    FVisible: Boolean;
    procedure SetBackGround(const Value: TecBackGround);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetLabelInterval(const Value: integer);
    procedure SetMajorTicksInterval(const Value: integer);
    procedure SetMajorTicksLength(const Value: integer);
    procedure SetMinorTicksInterval(const Value: integer);
    procedure SetMinorTicksLength(const Value: integer);
    procedure RulerChanged(Sender: TObject);
    procedure SetHeight(const Value: integer);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetShowCurrentPos(const Value: Boolean);
    procedure SetMajorTicksColor(const Value: TColor);
    procedure SetMinorTicksColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure UpdateCurrent;
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas);
    procedure PaintTo(Canvas: TCanvas);
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clNone;
    property BackGround: TecBackGround read FBackGround write SetBackGround;
    property MinorTicksLength: integer read FMinorTicksLength write SetMinorTicksLength default 2;
    property MajorTicksLength: integer read FMajorTicksLength write SetMajorTicksLength default 5;
    property MinorTicksInterval: integer read FMinorTicksInterval write SetMinorTicksInterval default 1;
    property MajorTicksInterval: integer read FMajorTicksInterval write SetMajorTicksInterval default 5;
    property MinorTicksColor: TColor read FMinorTicksColor write SetMinorTicksColor default clBlack;
    property MajorTicksColor: TColor read FMajorTicksColor write SetMajorTicksColor default clBlack;
    property LabelInterval: integer read FLabelInterval write SetLabelInterval default 10;
    property Height: integer read FHeight write SetHeight default 17;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default clGray;
    property ShowCurrentPos: Boolean read FShowCurrentPos write SetShowCurrentPos default True;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  TecTextMargin = class(TCollectionItem)
  private
    FAllowDrag: Boolean;
    FVisible: Boolean;
    FPosition: integer;
    FHint: string;
    FPen: TPen;
    FRulerMark: Boolean;
    procedure SetAllowDrag(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetPen(const Value: TPen);
    procedure SetPosition(Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure PenChnaged(Sender: TObject);
    procedure SetRulerMark(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Pen: TPen read FPen write SetPen;
    property Hint: string read FHint write SetHint;
    property AllowDrag: Boolean read FAllowDrag write SetAllowDrag default True;
    property Position: integer read FPosition write SetPosition default 80;
    property RulerMark: Boolean read FRulerMark write SetRulerMark;
  end;

  TecTextMargins = class(TOwnedCollection)
  private
    FAbsolutePos: Boolean;
    FAllowDrag: Boolean;
    FDragWithCtrl: Boolean;
    function GetItem(Index: integer): TecTextMargin;
    procedure SetItem(Index: integer; const Value: TecTextMargin);
    procedure SetAbsolutePos(const Value: Boolean);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TecTextMargin;
    property Items[Index: integer]: TecTextMargin read GetItem write SetItem; default;
  published
    property AbsolutePos: Boolean read FAbsolutePos write SetAbsolutePos default False;
    property AllowDrag: Boolean read FAllowDrag write FAllowDrag default True;
    property DragWithCtrl: Boolean read FDragWithCtrl write FDragWithCtrl default True;
  end;

  TecCaretShape = class(TPersistent)
  private
    FIsGray: Boolean;
    FHeight: integer;
    FWidth: integer;
    FBitmap: TBitmap;
    FColor: TColor;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FBlinkTime: Cardinal;
    FStretch: Boolean;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetColor(const Value: TColor);
    procedure SetHeight(const Value: integer);
    procedure SetIsGray(const Value: Boolean);
    procedure SetWidth(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure BitmapChanged(Sender: TObject);
    procedure SetBlinkTime(const Value: Cardinal);
    procedure SetStretch(const Value: Boolean);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight default 100;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Stretch: Boolean read FStretch write SetStretch default True;
    property Color: TColor read FColor write SetColor default clDefault;
    property IsGray: Boolean read FIsGray write SetIsGray default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property BlinkTime: Cardinal read FBlinkTime write SetBlinkTime default 0;
  end;

  TecCaret = class(TPersistent)
  private
    FReadOnly: TecCaretShape;
    FInsert: TecCaretShape;
    FOverwrite: TecCaretShape;
    FOwner: TCustomSyntaxMemo;
    FBitmap: TBitmap;
    FIsShown: Boolean;
    FSize: TSize;
    FCurShape: TecCaretShape;
    FPosition: TPoint;
    FYOffset: integer;
    FOldBlinkTime: integer;
    FVisible: Boolean;
    FUseCustom: Boolean;
    FCustom: TecCaretShape;
    procedure SetInsert(const Value: TecCaretShape);
    procedure SetOverwrite(const Value: TecCaretShape);
    procedure SetReadOnly(const Value: TecCaretShape);
    procedure ShapeChanged(sender: TObject);
    procedure SetVisible(const Value: Boolean);
    procedure SetCustom(const Value: TecCaretShape);
    procedure SetUseCustom(const Value: Boolean);
  public
    constructor Create(AOwner: TCustomSyntaxMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Show(Pos: TPoint);
    procedure Hide;
    procedure Update;
  published
    property Insert: TecCaretShape read FInsert write SetInsert;
    property Overwrite: TecCaretShape read FOverwrite write SetOverwrite;
    property ReadOnly: TecCaretShape read FReadOnly write SetReadOnly;
    property Custom: TecCaretShape read FCustom write SetCustom;
    property Visible: Boolean read FVisible write SetVisible default True;
    property UseCustom: Boolean read FUseCustom write SetUseCustom default False;
  end;

  TCustomSyntaxMemo = class(TCustomControl, {$IFNDEF EC_DOTNET}IUnknown,{$ENDIF}
                            IecTextClient, IecSyntClient, IDropTarget, IDropSource)
  private
    FNonPrintedSpaces: boolean; //AT
    FNonPrintedEol: boolean; //AT
    FNonPrintedEolDetails: boolean; //AT
    FDoScrollOrg: TPoint;//AT
    FDoScroll: boolean;//AT
    FTimerScroll: TTimer;//AT
    FWordNavigation: TWordNavigation;
    FLines: TSyntMemoStrings;
    FLeftTopPos: TPoint;
    FSelStart: integer;
    FSelLength: integer;
    FCaretPos: TPoint;
    FDragging: Boolean;
    FDragText: Boolean;
    FDragSelWord: Boolean;
    FDragPos: integer;
    FScrollTimer: TTimer;
    FSyntRanges: TClientSyntAnalyzer;
    FReplaceMode: Boolean;
    FOnCaretPosChanged: TNotifyEvent;
    FTabMode: TTabMode;
    FOnGetGutterImage: TGetGutterImageEvent;
    FCursor: TCursor;
    FVertAlignment: TVertAlignment;
    FShiftDown: Boolean;
    FBookmarks: TBookmarks;
    FBlock: TRect;
    FDragPosCaret: TPoint;
    FCollapsed: TCollapsedRanges;
    FOnLineHighLight: TGetLineHighlightEvent;
//    FModified: Boolean;
    FScrollBars: TScrollStyle;
    FSyntClients: TList;
    FLineInfos: TList;
    FTextSource: TSyntTextSource;      // contains reference to gutter infos for the current line
    FTabList: TTabList;
    FNonPrinted: TNonPrinted;
    FLineNumbers: TLineNumbers;
    FGutter: TGutter;
    FHint,
    FHintBak: THintWindow; //AT2
    FHintTimer: TTimer;
    FTextMargin: integer;
    FOnGutterClick: TGutterClickEvent;
    FOnGetGutterBandColor: TGetGutterBandColor; //AT
    FOnGetTokenHint: TGetTokenHintEvent;
    FOnShowHint: TSyntShowHintEvent; //AT2
    FOnInsertChar: TAfterInsertChar;
    FHintProps: THintProps;
    FKeyMapping: TSyntKeyMapping;
    FBlockIndent: integer;
//    FGroupIndex: integer;   // index of group undo
    FFirstKey: TShortCut;
    FOptions: TSyntaxMemoOptions;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FCharCase: TEditCharCase;
    FOnGetLineNumberStr: TGetLineNumberStringEvent;
    FCollpseLevel: integer;
    FSearchMarks: TRangeList;
    FMarkers: TList;
    FHiddenStyle: Boolean;
    FOnDrawToken: TDrawTokenEvent;
    FOnBeforeLineDraw: TLineDrawEvent;
    FOnAfterLineDraw: TLineDrawEvent;
    FOnSetBookmark: TSetBookmarkEvent;
    FUserRanges: TUserRanges;
    FUserStyles: TSyntStyles;
    FStaplePen: TPen;
    FStapleOffset: integer;
    FStaplesEnabled: boolean; //AT
    FDragStaple: TBlockStaple;
    FAnimation: TSyntAnimation;
    FOnAnimate: TAnimateEvent;
    FWordWrap: Boolean;
    FPrintOptions: TSyntMemoPrintOptions;
    FSelectMode: TSyntSelectionMode;
    FSelectModeDefault: TSyntSelectionMode; //AT 
    FExternCaretChange: Boolean;
    FIncSearch: Boolean;
    FIncSearchPos: integer;
    FIncSearchBack: Boolean;
    FIncSearchStr: ecString;
    FIncSearchChange: TIncSearchChangeEvent;
    FIncSearchIgnoreCase: Boolean;
    FLineSpacing: integer;
    FOnTextChanged: TTextChangedEvent;
    FOnGetStyleEntry: TGetStyleEntryEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FFlatScrollBars: Boolean;
    FDefaultStyles: TDefaultStyles;
    FCollapseStyle: TCollapseStyle;
    FCollapseBreakColor: TColor;
    FOnFinishAnalysis: TNotifyEvent;

    FLinesDesc: TList;
    FDefaultLineDesc: TLineInfo;
    FTopSubLine: integer; // additional sub-line of the topline
    FLineCount: integer;  // saved line count
    FLineStateDisplay: TLineStateDisplay;
    FDefaultPopup: Boolean;
    FDefPopup: TPopupMenu;
    FOnGetMouseSelMode: TGetMouseSelMode;
    FSyncEditing: TSyntSyncEdit;
    FOnCanSyncEdit: TCanSyncEditEvent;
    FOnExecuteCommand: TExecuteCommandEvent;
    FDisableFolding: Boolean;

    FMonoFont: Boolean;
    FDefExt: TSize;
    FMonoFontMode: TMonoFontMode;
    FLastDblClick: LongWord;
    FOnSelectionChanged: TNotifyEvent;
    FBackGround: TecBackGround;

    FSkipHltClear: Boolean;
    FOnDeleteBookmark: TBookmarkDeleteEvent;
    FKeyNavigate: Boolean;
    FLineSpacingBefore: integer;
    FLineSpacingAfter: integer;
    {$IFDEF EC_UNICODE_IN_ANSI}
    FUniChar: WideChar;
    FScanCode: LongWord;
    {$ENDIF}
    FSaveState: TSaveStateOptions;
    FOnScroll: TNotifyEvent;
    // OLE Drag support
    FDragTypeOLE: Boolean;
    FValidOle: Boolean;
    FOnOleDragEnter: TecOleDragEvent;
    FOnOleDragOver: TecOleDragEvent;
    FOnOleDrop: TecOleDragEvent;
    FDataObj: IDataObject;
    FPrnTarget: IDropTarget;
    FColPos: TPoint;
    FOnOleGiveFeedback: TecOleGiveFeedback;
    FMultiLine: Boolean;
    FOnOleBeginDrag: TecOleBeginDragText;
    FOnCheckChar: TCheckCharEvent;
    FKeepedXPos: integer;
    FOnGetLineStyle: TGetLineStyleEvent;
    FMarkedSelStart: integer;
    FOnGetRightMargin: TGetRightMarginEvent;
    FEmbObjects: TecEmbeddedObjects;
    FSelectedEmbObj: integer;
    FKeyQueue: TKeyQueue;
    FOptionsEx: TSyntaxMemoOptionsEx;
    FBorderStyle: TBorderStyle;
    FMargin: TRect;

    FScrollDelit: TPoint;
    FCurScroll: TScrollStyle;
    FZoom: integer;
    FOnZoom: TNotifyEvent;
    FSelAttributes: TecTextAttributes;
    FOnBeforeDelete: TBeforeDeleteEvent;
    FOnBeforeInsert: TBeforeInsertEvent;
    FOnEnableCommand: TExecuteCommandEvent;
    FHorzRuler: TecHorzRuler;
    FTextMargins: TecTextMargins;
    FOnBeforeDrawStaple: TBeforeDrawStaple;
    FCaret: TecCaret;
    FOnCanDelete: TCanDeleteEvent;
    FOnCanInsert: TCanInsertEvent;
    FHintSense: TRect;
    FTransparent: Boolean;
    FOnDragTextMargin: TDragMarginEvent;
    FAlignment: TAlignment;
    FOnAfterCalcLineInfo: TAfterCalcLineInfo;
    FMaximalLinesWidth: integer; //ZD
    function GetAbout: string;
    procedure SetAbout(const Value: string);
    procedure WMEraseBkgnd(var Message: TWMErasebkgnd); message WM_ERASEBKGND;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
//    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  {$IFDEF EC_UNICODE_IN_ANSI}
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
  {$ENDIF}
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMIMEStartComp(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure UpdateIMEWindow(IMEContext: HIMC);
    procedure RecalcIMEWindow;
    procedure SetCaretPos(Value: TPoint);
    procedure DoDragScroll(Sender: TObject);
    procedure DoTimerScroll(Sender: TObject);//AT
    procedure DrawScroll;//AT
    procedure SetLines(const Value: TSyntMemoStrings);
    procedure UpdateCaretPos;
    procedure AddUndo(op: TTextOperation; const aInsTxt, aDelTxt: ecString; aPos: integer; MovedCaret: Boolean = True);
    procedure SetUndoLimit(const Value: integer);
    function GetUndoLimit: integer;
    procedure SetSyntRanges(const Value: TSyntAnalyzer);
    procedure SetTabMode(const Value: TTabMode);
    function LineLength(Index: integer): integer;
    procedure SetCursor(const Value: TCursor);
    function GetSyntRanges: TSyntAnalyzer;
    function GetBookmarkObj(Index: integer): TBookmark;
    function GetBookmark(Index: integer): integer;
    procedure SetBookmark(Index: integer; const Value: integer);
    procedure SetReplaceMode(const Value: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure DrawGutterLine(y1, y2, Line, LineColState: integer);
    procedure UpdateCollapsed;
    function GetLines: TSyntMemoStrings;
    procedure SetTextSource(const Value: TSyntTextSource);
    procedure SetTabList(const Value: TTabList);
    procedure SetNonPrinted(const Value: TNonPrinted);
    procedure SetLineNumbers(const Value: TLineNumbers);
    procedure SetGutter(const Value: TGutter);
    procedure FillGutterBackground(Line, y1, y2: integer; Canvas: TCanvas);
    procedure SetTextMargin(const Value: integer);
    procedure LineBound(Line: integer; var Y1, Y2: integer; Exact: Boolean = False);
    procedure ClearLineInfos;
    procedure GetLineInfos(Line: integer);
    procedure SetHintProps(const Value: THintProps);
    procedure SetKeyMapping(const Value: TSyntKeyMapping);
    procedure SetOptions(const Value: TSyntaxMemoOptions);
    function GetCanUndo: Boolean;
    function GetModified: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: ecString;
    procedure SetModified(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelText(const Value: ecString);
    procedure SetCharCase(const Value: TEditCharCase);
    function GetCanPaste: Boolean;
    procedure SetLineSelection(sStart, sLength: integer; re_draw: Boolean = True);
    function GetCaretStrPos: integer;
    procedure SetCaretStrPos(const Value: integer);
    procedure SetUserRanges(const Value: TUserRanges);
    procedure SetUserStyles(const Value: TSyntStyles);
    procedure SetStaplePen(const Value: TPen);
    procedure SetStapleOffset(const Value: integer);
    function GetTextLength: integer;
    procedure SetAnimation(const Value: TSyntAnimation);
    function GetLineRect(Line: integer): TRect;
    procedure SetWordWrap(const Value: Boolean);
    procedure SetSelectMode(const Value: TSyntSelectionMode);
    procedure SetSelectModeDefault(const Value: TSyntSelectionMode); //AT
    procedure SetIncSearchStr(const Value: ecString);
    procedure SetIncSearchBack(const Value: Boolean);
    procedure SetIncSearchIgnoreCase(const Value: Boolean);
    procedure SetLineSpacing(const Value: integer);
    function GetSyntObj: TClientSyntAnalyzer;
    procedure SetFlatScrollBars(const Value: Boolean);
    procedure SetDefaultStyles(const Value: TDefaultStyles);
    procedure ShowCollapsedHint(ALine: integer; const HintSense: TRect);
    function CollapsedTextIconHint(X, Y: integer): Boolean;
    procedure SetCollapseBreakColor(const Value: TColor);
    procedure SetCollapseStyle(const Value: TCollapseStyle);
    function GetColRangeTextAtPos(Line: integer; var IconPos: integer): string;
    function InTextImageHint(X, Y: integer): Boolean;
    procedure SetLineStateDisplay(const Value: TLineStateDisplay);
    procedure StdCmdPopupExec(Sender: TObject);
    // from line processing
    function GetBlockStaples(Line: integer): TList;
    function GetMarkersFroLine(Line, StartPos, EndPos: integer): TList;
    // scrolling support
    function GetScrollPosX: integer;
    procedure SetScrollPosX(Value: integer);
    function GetTopLine: integer;
    procedure SetTopLine(Value: integer);
    function GetLogTopLine: integer;
    procedure SetLogTopLine( Line: integer);
    procedure UpdateLineInfosAfterEdit(EditPos: TPoint; LineChange: integer);
    procedure CountHeight(Line: integer; Info: TLineInfo; var Param1,
      Param2: integer; var Stop: Boolean);
    procedure CountLines(Line: integer; Info: TLineInfo; var Param1,
      Param2: integer; var Stop: Boolean);
    procedure CountLogLines(Line: integer; Info: TLineInfo; var Param1,
      Param2: integer; var Stop: Boolean);
    procedure IsLineAtPos(Line: integer; Info: TLineInfo; var Param1,
      Param2: integer; var Stop: Boolean);
    procedure ScanLog(Line: integer; Info: TLineInfo; var Param1,
      Param2: integer; var Stop: Boolean);
    procedure VisDetect(Line: integer; Info: TLineInfo; var Param1,
      Param2: integer; var Stop: Boolean);
    procedure CountLinesWidth(Line: integer; Info: TLineInfo; var Param1,
      Param2: integer; var Stop: Boolean);
    procedure SetSyncEditing(const Value: TSyntSyncEdit);
    function SyncInsDelOper(var Pos: integer; Count: integer; S: ecString): Boolean;
    procedure ClearLineState(Line: integer);
    procedure SetDisableFolding(const Value: Boolean);
    procedure UpdateMonoFontFlag;
    procedure SetMonoFontMode(const Value: TMonoFontMode);
    function HasReadOnly(Styles: TStyleEntries): boolean;
    function Get_Text: ecString;
    function IntGetLineInfo(Line: integer): TLineInfo;
    procedure SetBackGround(const Value: TecBackGround);
    procedure BgChanged(Sender: TObject);
    procedure SetSelRect(const Value: TRect);
    procedure SetLineSpacingAfter(const Value: integer);
    procedure SetLineSpacingBefore(const Value: integer);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetCharset(const Value: TFontCharset);
    function GetCharset: TFontCharset;
    function GetReadOnly: Boolean;
    function GetParentOLETarget: IDropTarget;
    function IsOverRightMargin(X: Integer; OnlyVisible: Boolean = True): TecTextMargin;
    procedure SetKeyQueue(const Value: TKeyQueue);
    procedure SetOptionsEx(const Value: TSyntaxMemoOptionsEx);
    function GetClientCount: integer;
    function GetClients(Index: integer): TObject;
    procedure SetZoom(const Value: integer);
    procedure SetSelAttributes(const Value: TecTextAttributes);
    procedure StartTextSelection(pt: TPoint; Shift: TShiftState; selWords: Boolean = false);
    function GetCurrentLine: integer;
    procedure SetCurrentLine(const Value: integer);
    function GetInsAddSpaces(const cp: TPoint): ecString;
    procedure SetHorzRuler(const Value: TecHorzRuler);
    procedure SetTextMargins(const Value: TecTextMargins);
    function GetRightMargin: integer;
    function GetRightMarginColor: TColor;
    function GetShowRightMargin: Boolean;
    procedure SetShowRightMargin(const Value: Boolean);
    procedure SetRightMargin(const Value: integer);
    procedure SetRightMarginColor(const Value: TColor);
    procedure SetCaret(const Value: TecCaret);
    procedure SafeDeleteText(Ranges: TList);
    function EMCaretToREPos(const P: TPoint): TPoint;
    function EMLineIndex(LogLine: integer): integer;
    function EMREPosToCaret(const P: TPoint): TPoint;
    function EMLineLength(CharPos: integer): integer;
    function EMLineFromChar(CharPos: integer): integer;
//    function EMGetLine(LogLine: integer): ecString;
    procedure EMGetLineInt(LogLine: integer; var sp, ep: integer);
    function EMGetLineCount: integer;
    function NextTab(LogK, InTextPos: integer): integer;
    function AllowZeroTabAfter(C: ecChar): Boolean;
    procedure SetTransparent(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    function DoMouseWheelUpDown(Up: boolean; Shift: TShiftState): Boolean;//AT
  protected
    FUpdateCount: integer;
    FDraggedTextMargin: TecTextMargin;
    //AT
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;//AT
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;//AT
    // Style management
    procedure ApplyStyles(Canvas: TCanvas; List: TStyleEntries; OnlyDyno: Boolean);
    procedure ApplyDefStyle(Canvas: TCanvas; Pos: integer);
    procedure ApplyStyle(Canvas: TCanvas; Style: TSyntaxFormat);
    function SetCanvasAtPos(Canvas: TCanvas; CurPos: integer): integer;
    procedure UpdateCursor;
    function GetModifiedChanged: TNotifyEvent;
    procedure SetModifiedChanged(const Value: TNotifyEvent);
  protected
    // IDropSource
    procedure DoInsertChar(Char: ecChar; StrPos: Integer); virtual;
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; {$IFNDEF EC_DOTNET}stdcall;{$ENDIF}
    function GiveFeedback(dwEffect: Longint): HResult; {$IFNDEF EC_DOTNET}stdcall;{$ENDIF}
    // IDropTarget
    function DragEnter({$IFNDEF EC_DOTNET}const{$ENDIF} DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; {$IFNDEF EC_DOTNET}stdcall;{$ENDIF}
    function IDropTarget.DragOver = IDropTarget_DragOver;
    function IDropTarget_DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; {$IFNDEF EC_DOTNET}stdcall;{$ENDIF}
    function DragLeave: HResult; {$IFNDEF EC_DOTNET}stdcall;{$ENDIF}
    function Drop({$IFNDEF EC_DOTNET}const{$ENDIF} DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; {$IFNDEF EC_DOTNET}stdcall;{$ENDIF}
    // IecTextClient
    procedure TextChanged(Sender: TObject; Pos, Count, LineChange: integer); dynamic;
    // IecSyntClient
    procedure FormatChanged;
    procedure Finished;
  protected
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure GutterClick(Line: integer; Button: TMouseButton; Shift: TShiftState; XY: TPoint); dynamic;
    procedure ExpandTabs(var S: ecString; CurPos, LogK: integer);
    function CurrentFontHeight(Canvas: TCanvas): integer;
    function SingleUndo: integer;
    function SingleRedo: integer;
    procedure IntInsertText(APos: integer; s: ecString); dynamic;
    procedure IntDeleteText(APos: integer; Count: integer); dynamic;
    procedure Change; dynamic;
    function CaretPosChanged: Boolean; dynamic;
    procedure AnalyzeToPos(APos: integer); dynamic;
    // Hint support routines
    procedure HintTimer(Sender: TObject);
    procedure DoShowHint(const p: TPoint); dynamic;
    procedure ShowHintWnd(const Text: string; FHintRect: TRect; ResetTime: integer); overload;
    procedure ShowHintWnd(const Text: string; APos: TPoint; ResetTime: integer; AllowMoveLeft: Boolean = False); overload;
    procedure ShowHintWnd(const Text: string; const HintSense: TRect); overload;
    procedure GetTokenHint(TokenIndex: integer; var HintText: string); dynamic;

    procedure DrawMarker(Canvas: TCanvas; x, y: integer); dynamic;
    procedure IntSetSelection(sStart, sLength: integer; re_draw: Boolean = True);
    procedure DrawStaple(Canvas: TCanvas; X, Y1, Y2: integer; AFirstLine, ALastLine: Boolean; Level: integer; Staple: TBlockStaple); virtual;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    function GetCursor(x, y: integer; Shift: TShiftState): TCursor; dynamic;
    function GetMouseSelMode(Shift: TShiftState): TSyntSelectionMode; dynamic;
    function CreateStdPopup: TPopupMenu;
    function DoBeforeInsertText(var Pos: integer; var S: ecString): Boolean; dynamic;
    function DoBeforeDeleteText(var Pos, Count: integer): Boolean; dynamic;
    procedure DoDragMargin(Margin: TecTextMargin; Pos: integer); dynamic;
    function PaintNoTextSpace(Canvas: TCanvas; const R: TRect; Line: integer): Boolean; virtual;
    function GetLineInfo(Line: integer): TLineInfo; virtual;

    // Scolling utils
    function IterateVisibleLines(Proc: TLineIterator; FirstLine: integer;
      var Param1, Param2: integer; DefinedLine: Boolean = False; Grow: Boolean = True): integer;
    //procedure CollapsedChanged;
    function SourceTopLinePos: integer;

    procedure SelectionChanged; dynamic;
    procedure IntSetCaretPos(const cp: TPoint);
    function GetLineNumberStr(Line: integer): string; dynamic;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DoEndDrag(Target: TObject; X: Integer; Y: Integer);override;
    procedure DoScroll; dynamic;
    procedure GetGutterImage(Line: integer; List: TList); dynamic;
    procedure DragCanceled; override;

    function IsWordChar(C: ecChar): Boolean; virtual;
    function IsWordEdge(c1, c2: ecChar): boolean; virtual;
    procedure ReaderSkip(Reader: TReader);
    procedure DefineProperties(Filer: TFiler); override;

    function DoAutoFormat: Boolean;
    procedure Set_Text(const Value: ecString); virtual;
    procedure SetBorderStyle(const Value: TBorderStyle); virtual;
    procedure UpdateMargin;
    function CalcMargins: TRect; virtual;
    procedure SetCurs(cur: TCursor);
    procedure AdjustScrollBar;
    property Margin: TRect read FMargin; //AT was in private

    property About: string read GetAbout write SetAbout stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RefreshSyntax;
    procedure ResetHint;

    function IsOverStaple(X, Y: Integer): TBlockStaple; //AT, was in private
    procedure CollapsedChanged; //AT
    //AT
    property NonPrintedSpaces: boolean read FNonPrintedSpaces write FNonPrintedSpaces; //AT
    property NonPrintedEol: boolean read FNonPrintedEol write FNonPrintedEol; //AT
    property NonPrintedEolDetails: boolean read FNonPrintedEolDetails write FNonPrintedEolDetails; //AT
    function VisibleLinesWidth: integer; //ZD - made it public because it is needed when centering text

    // Line processor
    function ProcessLine(Canvas: TCanvas; LineInfo: TLineInfo;
                         Line: integer; ProcType: TProcessLineOper;
                         R: TRect; Param: TPoint;
                         FromPos: integer = 0; ToPos: integer = 0): TPoint;

    // Main command processor
    procedure ExecCommand(Command: integer; Data: ecPointer = nil); virtual;
    function IsCommandEnabled(Command: integer; Data: ecPointer = nil): Boolean; virtual;

    function SCharGr(ch: ecChar): TCharGr; //AT
    procedure DoWordJump(ANext: boolean); //AT
    function DoWordJumpPos(NFromPos: Integer; ANext: boolean): Integer; //AT
    property UpdateCount: integer read FUpdateCount; //AT

    // Standard edit action support
    function UpdateAction(Action: TBasicAction): boolean; override;
    function ExecuteAction(Action: TBasicAction): boolean; override;

    // For syntax clients
    procedure AddClient(Client: TObject);
    procedure RemoveClient(Client: TObject);

    // Must be extracted
    procedure ExportToRtf(const FileName: string);
    procedure ExportToHTML(const FileName: string);

    // Text operations
    procedure BeginUpdate;
    procedure EndUpdate(doRefresh: Boolean = True);
    procedure InsertText(s: ecString);
    procedure DeleteText(Count: integer);
    procedure MoveText(toCaretPos: TPoint; Copying: Boolean);
    function ReplaceText(Pos, Count: integer; RepStr: ecString): Boolean;
    function InsertTextBlock(Block: TecStrings; CarPos: TPoint): integer;
    procedure UpdateEditor; virtual;
    procedure ConvertTabs(AStart, AEnd: integer);
    function SkipHidden(X, Line: integer; Grow: Boolean): TPoint;
    procedure InsertNewLine(Indent: integer; DoNotMoveCaret, SoftBreak: Boolean);

    // Undo/redo operations
    procedure Undo;
    procedure Redo;
    procedure ClearUndo;
    procedure ClearRedo;
    function CanRedo: Boolean;

    // Standard edit methods
    procedure Clear;
    procedure ClearSelection;
    procedure ResetSelection; // unselect
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard(AForceColumnBlock: boolean = false);
    function CanInsertText(Pos: integer): Boolean; virtual;
    function CanDeleteText(Pos, Count: integer): Boolean; virtual;
    function CanClearSelection: Boolean;

    procedure SelectAll;
    function HaveSelection: Boolean;
    procedure SetSelection(sStart, sLength: integer; DoNotMoveCaret: Boolean = False);
    procedure SelectLine(Line: integer);
    procedure SelectLines(FirstLine, LastLine: integer);

    // OLE Drag&Drop
    function BeginOLEDrag: Boolean;

    // Coordinate convertions
    function MouseToCaret(X, Y: integer): TPoint;
    function CaretToMouse(cX, cY: integer): TPoint;
    function CaretPosToStrPos(const p: TPoint): integer;
    function StrPosToCaretPos(p: integer): TPoint;
    function LinesPosToLog(Pos: TPoint): TPoint;
    function LinesPosToLogX(const S: ecString; XPos: integer): integer;
    function LogToLinesPos(const Pos: TPoint): TPoint;
    function CaretInText(const Pos: TPoint): Boolean;
    function SingleLineHeight(Line: integer): integer;
    function LineHeight(Line: integer): integer; overload;
    function LineHeight(cp: TPoint): integer; overload;
    function LineAtPos(var Y: integer): integer;
    function VisibleLines: integer;
    function VisibleCols: integer;
    function DefTextExt: TSize;
    function DefLineHeight: integer;
    function ScrollText(XScrollPos, YScrollPos, YSubLine: integer): Boolean;
    function TextArea: TRect;
    procedure SupressCaretChange;

    // Search results marks
    procedure SetSearchMark(sStart, sLength: integer; DoAdd: Boolean = False);
    procedure ResetSearchMarks;

    // Collapse support routines
    function IsLineCollapsed(Line: integer): integer;
    function ToggleCollapse(Line: integer): Boolean;
    procedure CollapseRange(Range: TTextRange; Update: Boolean = True);
    procedure CollapseUserRange(Range: TUserRange; Update: Boolean = True);
    procedure FullExpand(AStartPos: integer = 0; AEndPos: integer = -1);
    procedure FullCollapse(AStartPos: integer = 0; AEndPos: integer = -1);
    procedure CollapseLines(StartPos, EndPos: integer);
    procedure CollapseNearest(Line: integer);
    function ShowLine(aLine: integer): Boolean;
    function ShowLines(FirstLine, LastLine: integer): Boolean;
    function ToggleCollapseChildren(Line: integer): Boolean;

    // Utils function
    function WordAtPos(p: TPoint): ecString;
    procedure WordRangeAtPos(Pos: TPoint; var wStart, wEnd: integer);
    procedure SelectWord;
    procedure IndentLines(sl, el, Count, XPos: integer; BreakAlign: Boolean);
    procedure RemoveTrailingBlanks(WithUndo: Boolean);
    function FormatAtPos(aPos: integer): TSyntaxFormat;
    procedure ClearCustObject;
    function GetStyleList(CurPos: integer; List: TStyleEntries; Flags: integer; IncludeLine: Boolean): integer; virtual;
    procedure InvalidateTextRange(StartPos, EndPos: integer; UpdateHeights: Boolean);
    procedure InvalidateLines(sLine, eLine: integer; UpdateHeights: Boolean);
    procedure InvalidateGutter;
    function TokenAtPos(Pos: TPoint): integer;
    function IsOverCollapsedTextIcon(X, Y: integer): Boolean;
    procedure ResetLineHeights(DoClear: Boolean = False; FromLine: integer = 0; ToLine: integer = -1);
    function GetSelectedLines(var FirstLine, LastLine: integer): integer;
    procedure SortLines(Ascending, CaseSensitive: Boolean; FromLine: integer = 0; ToLine: integer = -1; LogXPos: integer = 0);
    procedure SortSelection(Ascending, CaseSensitive: Boolean);
    function CanSortSelection: Boolean;
    function PosInSelection(aPos: integer): Boolean; overload;
    function PosInSelection(aCaretPos: TPoint): Boolean; overload;
    function ClientOfType(AClass: TClass): TObject;
    function GutterObjectAt(const XY: TPoint): TCustomGutterObject;
    function CanMoveSelLines(MoveUp: Boolean): Boolean;
    procedure MoveSelLines(MoveUp: Boolean);
    procedure DuplicateLine(Line: integer);
    function UnTabText(FromPos: integer = 0; ToPos: integer = 0): integer;

    // Exec command helpers
    procedure ShiftSelection(Count: integer; BreakAlign: Boolean);
    function JumpToMatchDelim: Boolean;
    procedure LineComments(CommentLines: Boolean; FirstLine: integer = -1; LastLine: integer = -1);
    function NextWord(pt: TPoint): TPoint;
    function PrevWord(pt: TPoint): TPoint;
    function ChangeCase(Pos, Count: integer; Oper: TChangeCase): Boolean;
    procedure SelChangeCase(Oper: TChangeCase);
    function GetIndentString(Len: integer; OnlySpaces: Boolean): ecString;
    function StringIndent(const S: ecString): integer;
    function FirstLetter(const s: ecString): integer;
    function LastLetter(const s: ecString): integer;

    // Aligning lines
    procedure AlignTokens(FromLine, ToLine: integer);
    procedure AlignTokensSel;
    procedure AlignLines(const Tokens: TStringList; ByFirst: Boolean; FromLine, ToLine: integer);
    procedure AlignSelectedLines(const Tokens: string; ByFirst: Boolean = False);

    // File operations
    procedure LoadFromFile(const FileName: Widestring);
    procedure SaveToFile(const FileName: WideString);
    procedure LoadStateFromFile(const FileName: string);
    procedure SaveStateToFile(const FileName: string);

    // Bookmarks
    procedure ClearBookmarks;
    procedure ToggleBookmark(Index: integer);
    procedure GotoBookmark(Index: integer);
    function BookmarkForLine(Line: integer): integer;

    // Markers
    function CreateMarker(cp: TPoint): TMarker;
    procedure DropMarker(Pos: TPoint);
    function CollectMarker: Boolean;
    function SwapMarker: Boolean;
    procedure AnimateCaret(AnimType: TAnimationType);
    procedure GotoMarker(Marker: TMarker);

    // User Ranges
    function CurrentUserRange: TUserRange;
    function CreateUserRange(ACollapsable: Boolean = False; AStyle: string = '';
                              ALineBreaks: TLineBreakBound = []): TUserRange;

    // Incremental search
    procedure IncSearchStart(FromPos: integer = -1);
    procedure IncSearchStop;

    // Scrolling (logical line numbers, count)
    procedure LogToLine(LogLine: integer; var Line, SubLine: integer);
    function LineToLog(Line, SubLine: integer): integer;
    function CalcLineCount(Exact: Boolean = False): integer;
    function ScrollCaret: Boolean; overload;
    function SkipInvisible(Line: integer; Grow: Boolean = True): integer;
    function ScrollCaret(Cp: TPoint; VertAlign: TScrollAlignment; HorzAlign: TScrollAlignment): Boolean; overload;

    property CaretPos: TPoint read FCaretPos write SetCaretPos;
    property CaretStrPos: integer read GetCaretStrPos write SetCaretStrPos;
    property TopLine: integer read GetTopLine write SetTopLine;
    property ScrollPosY: integer read GetLogTopLine write SetLogTopLine;
    property ScrollPosX: integer read GetScrollPosX write SetScrollPosX;
    property LineInfos[Index: integer]: TLineInfo read GetLineInfo;

    property Bookmarks[Index: integer]: integer read GetBookmark write SetBookmark;
    property Collapsed: TCollapsedRanges read FCollapsed;
    property SyntObj: TClientSyntAnalyzer read GetSyntObj;
    property BookmarkObj: TBookmarks read FBookmarks;

    property CanUndo: Boolean read GetCanUndo;
    property CanPaste: Boolean read GetCanPaste;
    property Modified: Boolean read GetModified write SetModified;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: ecString read GetSelText write SetSelText;
    property SelRect: TRect read FBlock write SetSelRect;

    property SelStartMarked: Integer read FMarkedSelStart write FMarkedSelStart; //AT
    property DragPos: Integer read FDragPos write FDragPos; //AT

    property TextLength: integer read GetTextLength;
    property SelectMode: TSyntSelectionMode read FSelectMode write SetSelectMode;
    property SelectModeDefault: TSyntSelectionMode read FSelectModeDefault write SetSelectModeDefault;
    property IncSearchStr: ecString read FIncSearchStr write SetIncSearchStr;
    property IncSearchBack: Boolean read FIncSearchBack write SetIncSearchBack default False;
    property IncSearchIgnoreCase: Boolean read FIncSearchIgnoreCase write SetIncSearchIgnoreCase default True;
    property Markers: TList read FMarkers;
    property CurrentLine: integer read GetCurrentLine write SetCurrentLine;
    property SearchMarks: TRangeList read FSearchMarks;
    // Used only when (Canvas <> Self.Canvas)
    property PrintOptions: TSyntMemoPrintOptions read FPrintOptions write FPrintOptions;

//    property Text;
    property Text: ecString read Get_Text write Set_Text;
    property Font;
    property Color;
    property Canvas;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
    // Formatting
    property SelAttributes: TecTextAttributes read FSelAttributes write SetSelAttributes;
  public
    property Lines: TSyntMemoStrings read GetLines write SetLines;
    property SyntaxAnalyzer: TSyntAnalyzer read GetSyntRanges write SetSyntRanges;
    property TextSource: TSyntTextSource read FTextSource write SetTextSource;
    property TabList: TTabList read FTabList write SetTabList;
    property NonPrinted: TNonPrinted read FNonPrinted write SetNonPrinted;
    property LineNumbers: TLineNumbers read FLineNumbers write SetLineNumbers;
    property Gutter: TGutter read FGutter write SetGutter;
    property HintProps: THintProps read FHintProps write SetHintProps;
    property KeyMapping: TSyntKeyMapping read FKeyMapping write SetKeyMapping;
    property UserRanges: TUserRanges read FUserRanges write SetUserRanges;
    property UserStyles: TSyntStyles read FUserStyles write SetUserStyles;
    property Animation: TSyntAnimation read FAnimation write SetAnimation;

    property ShowRightMargin: Boolean read GetShowRightMargin write SetShowRightMargin stored False;
    property RightMarginColor: TColor read GetRightMarginColor write SetRightMarginColor stored False;
    property RightMargin: integer read GetRightMargin write SetRightMargin stored False;

    property ReplaceMode: Boolean read FReplaceMode write SetReplaceMode default False;
    property UndoLimit: integer read GetUndoLimit write SetUndoLimit default 1000;
    property TabMode: TTabMode read FTabMode write SetTabMode default tmSpaces;
    property Cursor: TCursor read FCursor write SetCursor default crIBeam;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property TextMargin: integer read FTextMargin write SetTextMargin default 2;
    property BlockIndent: integer read FBlockIndent write FBlockIndent default 2;
    property CollapseLevel: integer read FCollpseLevel write FCollpseLevel default -1;
    property StaplePen: TPen read FStaplePen write SetStaplePen;
    property StapleOffset: integer read FStapleOffset write SetStapleOffset default 2;
    property StaplesEnabled: boolean read FStaplesEnabled write FStaplesEnabled;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property FlatScrollBars: Boolean read FFlatScrollBars write SetFlatScrollBars default False; // v2.02
    property DefaultStyles: TDefaultStyles read FDefaultStyles write SetDefaultStyles; // v2.02
    property CollapseBreakColor: TColor read FCollapseBreakColor write SetCollapseBreakColor default clSilver; // v2.02
    property CollapseStyle: TCollapseStyle read FCollapseStyle write SetCollapseStyle default csRegionName; // v2.02
    property LineStateDisplay: TLineStateDisplay read FLineStateDisplay write SetLineStateDisplay; //v2.15
    property DefaultPopup: Boolean read FDefaultPopup write FDefaultPopup default True;  //v2.20
    property SyncEditing: TSyntSyncEdit read FSyncEditing write SetSyncEditing; // v2.20
    property DisableFolding: Boolean read FDisableFolding write SetDisableFolding default False;
    property MonoFontMode: TMonoFontMode read FMonoFontMode write SetMonoFontMode default mfVariable;
    property BackGround: TecBackGround read FBackGround write SetBackGround;
    property DragTypeOLE: Boolean read FDragTypeOLE write FDragTypeOLE default False;

    property LineSpacing: integer read FLineSpacing write SetLineSpacing default 1;
    property LineSpacingBefore: integer read FLineSpacingBefore write SetLineSpacingBefore default 0;
    property LineSpacingAfter: integer read FLineSpacingAfter write SetLineSpacingAfter default 0;

    property Options: TSyntaxMemoOptions read FOptions write SetOptions
        default [soOverwriteBlocks, soEnableBlockSel, soAutoIndentMode,
                 soBackUnindent, soGroupUndo, soHideSelection, soHideDynamic, soDragText,
                 soScrollLastLine];
    property OptionsEx: TSyntaxMemoOptionsEx read FOptionsEx write SetOptionsEx
        default [soSmartPaste, soCorrectNonPrinted, soRightClickMoveCaret];

    property SaveState: TSaveStateOptions read FSaveState write FSaveState default [];

    property ClientCount: integer read GetClientCount;
    property Clients[Index: integer]: TObject read GetClients;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default True;
    property Charset: TFontCharset read GetCharset write SetCharset default DEFAULT_CHARSET;
    property KeyQueue: TKeyQueue read FKeyQueue write SetKeyQueue; // v2.30
    property Zoom: integer read FZoom write SetZoom default 100;
    property HorzRuler: TecHorzRuler read FHorzRuler write SetHorzRuler;
    property TextMargins: TecTextMargins read FTextMargins write SetTextMargins;
    property Caret: TecCaret read FCaret write SetCaret; // 2.60
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property WordNavigation: TWordNavigation read FWordNavigation write FWordNavigation default wnWordStart;

    property OnCaretPosChanged: TNotifyEvent read FOnCaretPosChanged write FOnCaretPosChanged;
    property OnGetGutterImage: TGetGutterImageEvent read FOnGetGutterImage write FOnGetGutterImage;
    property OnLineHighLight: TGetLineHighlightEvent read FOnLineHighLight write FOnLineHighLight;
    property OnGutterClick: TGutterClickEvent read FOnGutterClick write FOnGutterClick;
    property OnGetGutterBandColor: TGetGutterBandColor read FOnGetGutterBandColor write FOnGetGutterBandColor; //AT
    property OnGetTokenHint: TGetTokenHintEvent read FOnGetTokenHint write FOnGetTokenHint;
    property OnShowHint: TSyntShowHintEvent read FOnShowHint write FOnShowHint; //AT2
    property OnInsertChar: TAfterInsertChar read FOnInsertChar write FOnInsertChar;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetLineNumberStr: TGetLineNumberStringEvent read FOnGetLineNumberStr write FOnGetLineNumberStr;
    property OnDrawToken: TDrawTokenEvent read FOnDrawToken write FOnDrawToken;
    property OnBeforeLineDraw: TLineDrawEvent read FOnBeforeLineDraw write FOnBeforeLineDraw;
    property OnAfterLineDraw: TLineDrawEvent read FOnAfterLineDraw write FOnAfterLineDraw;
    property OnSetBookmark: TSetBookmarkEvent read FOnSetBookmark write FOnSetBookmark;
    property OnAnimate: TAnimateEvent read FOnAnimate write FOnAnimate;
    property OnIncSearchChange: TIncSearchChangeEvent read FIncSearchChange write FIncSearchChange;
    property OnTextChanged: TTextChangedEvent read FOnTextChanged write FOnTextChanged;
    property OnGetStyleEntry: TGetStyleEntryEvent read FOnGetStyleEntry write FOnGetStyleEntry;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnFinishAnalysis: TNotifyEvent read FOnFinishAnalysis write FOnFinishAnalysis;
    property OnGetMouseSelMode: TGetMouseSelMode read FOnGetMouseSelMode write FOnGetMouseSelMode;
    property OnCanSyncEdit: TCanSyncEditEvent read FOnCanSyncEdit write FOnCanSyncEdit; //v2.20
    property OnExecuteCommand: TExecuteCommandEvent read FOnExecuteCommand write FOnExecuteCommand;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnDeleteBookmark: TBookmarkDeleteEvent read FOnDeleteBookmark write FOnDeleteBookmark;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnOleDragEnter: TecOleDragEvent read FOnOleDragEnter write FOnOleDragEnter;
    property OnOleDragOver: TecOleDragEvent read FOnOleDragOver write FOnOleDragOver;
    property OnOleDrop: TecOleDragEvent read FOnOleDrop write FOnOleDrop;
    property OnOleGiveFeedback: TecOleGiveFeedback read FOnOleGiveFeedback write FOnOleGiveFeedback;
    property OnOleBeginDrag: TecOleBeginDragText read FOnOleBeginDrag write FOnOleBeginDrag;
    property OnCheckChar: TCheckCharEvent read FOnCheckChar write FOnCheckChar;
    property OnGetLineStyle: TGetLineStyleEvent read FOnGetLineStyle write FOnGetLineStyle;
    property OnGetRightMargin: TGetRightMarginEvent read FOnGetRightMargin write FOnGetRightMargin;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
    property OnModifiedChanged: TNotifyEvent read GetModifiedChanged write SetModifiedChanged;
    property OnBeforeInsert: TBeforeInsertEvent read FOnBeforeInsert write FOnBeforeInsert;
    property OnBeforeDelete: TBeforeDeleteEvent read FOnBeforeDelete write FOnBeforeDelete;
    property OnEnableCommand: TExecuteCommandEvent read FOnEnableCommand write FOnEnableCommand;
    property OnBeforeDrawStaple: TBeforeDrawStaple read FOnBeforeDrawStaple write FOnBeforeDrawStaple;
    property OnCanInsert: TCanInsertEvent read FOnCanInsert write FOnCanInsert; // v2.70
    property OnCanDelete: TCanDeleteEvent read FOnCanDelete write FOnCanDelete; // v2.70
    property OnDragTextMargin: TDragMarginEvent read FOnDragTextMargin write FOnDragTextMargin; // v2.70
    property OnAfterCalcLineInfo: TAfterCalcLineInfo read FOnAfterCalcLineInfo write FOnAfterCalcLineInfo; // 3.00
  end;

  TSyntaxMemo = class(TCustomSyntaxMemo)
  published
    property NonPrintedSpaces; //AT
    property NonPrintedEol; //AT
    property NonPrintedEolDetails; //AT
    property SelectModeDefault; //AT
    property Lines;
    property SyntaxAnalyzer;
    property TextSource;
    property TabList;
    property NonPrinted;
    property LineNumbers;
    property Gutter;
    property HintProps;
    property KeyMapping;
    property UserRanges;
    property UserStyles;
    property Animation;

    property ReplaceMode;
    property UndoLimit;
    property TabMode;
    property Cursor;
    property ScrollBars;
    property TextMargin;
    property BlockIndent;
    property CollapseLevel;
    property StaplePen;
    property StapleOffset;
    property WordWrap;
    property IncSearchBack;
    property IncSearchIgnoreCase;
    property FlatScrollBars;
    property CollapseBreakColor;
    property CollapseStyle;
    property DefaultStyles;
    property LineStateDisplay;
    property DefaultPopup;
    property SyncEditing;
    property DisableFolding;
    property MonoFontMode;
    property BackGround;
    property DragTypeOLE;

    property LineSpacing;
    property LineSpacingBefore;
    property LineSpacingAfter;

    property Options;
    property OptionsEx;
    property SaveState;

    property BorderStyle;
    property CharCase;
    property ReadOnly;
    property DoubleBuffered default False; // 2.27
    property MultiLine; // 2.27
    property Charset; // 2.27
    property KeyQueue; // 2.30
    property Zoom;  // 2.32
    property HorzRuler; // 2.50
    property TextMargins; // 2.50
    property Caret; // 2.60
    property Transparent; // 3.00
    property OnAfterCalcLineInfo; // 3.00
    property Alignment;
    property WordNavigation; // 3.10

    // Old style Right Margin properties.
    property ShowRightMargin;
    property RightMarginColor;
    property RightMargin;

    property Anchors;
    property Align;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind; //??
    property DragMode; //??
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property About;

    property OnCaretPosChanged;
    property OnGetGutterImage;

    property OnLineHighLight;
    property OnGutterClick;
    property OnGetTokenHint;
    property OnInsertChar;
    property OnChange;
    property OnGetLineNumberStr;
    property OnDrawToken;
    property OnBeforeLineDraw;
    property OnAfterLineDraw;
    property OnSetBookmark;
    property OnAnimate;
    property OnIncSearchChange;
    property OnTextChanged;
    property OnGetStyleEntry;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnFinishAnalysis;
    property OnGetMouseSelMode;
    property OnCanSyncEdit;
    property OnExecuteCommand;
    property OnSelectionChanged;
    property OnDeleteBookmark;
    property OnScroll;
    property OnOleDragEnter;
    property OnOleDragOver;
    property OnOleDrop;
    property OnOleGiveFeedback;
    property OnOleBeginDrag;
    property OnCheckChar;
    property OnGetLineStyle;
    property OnZoom;
    property OnModifiedChanged;
    property OnBeforeInsert;
    property OnBeforeDelete;
    property OnEnableCommand;
    property OnBeforeDrawStaple;
    property OnCanInsert;
    property OnCanDelete;
    property OnDragTextMargin;

    // Inherited events
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Change_Case(var S: ecString; CurPos: integer; StlList: TStyleEntries);

var
  opColorNonPrintedBG: TColor = clSilver;

implementation

{$R expbtns.res}

uses Math, SysUtils, Clipbrd, Dialogs, Printers, Contnrs, StdActns, FlatSB,
     inifiles,
     ecCmdConst, ecExports, ecGoLineDlg {$IFNDEF EC_DOTNET}, RichEdit{$ENDIF},
     ecSyntDlg, ecPrint, ecPreviewFm{$IFDEF EC_VCL7_UP}, Themes {$ENDIF};

const
  SListIndexError = 'List index out of bounds (%d)';

{
var t1, t2: DWORD;

procedure __1; begin
 t1:= GetTickCount;
end;
procedure __2; begin
 t2:= GetTickCount;
 if (t2-t1)>500 then messagebeep(0);
end;
}

type

  // Single singronized selection
  TSyncRecord = class(TRange)
  private
    FList: TList; // List of TRangeList
    procedure ExtractWrods(const Text: ecString; IgnoreCase: Boolean);
    function GetCount: integer;
    function GetItem(Index: integer): TRangeList;
  public
    constructor Create(AStart, AEnd: integer; const AText: ecString; IgnoreCase: Boolean);
    destructor Destroy; override;
    procedure Delete(Index: integer);

    property Count: integer read GetCount;
    property Items[Index: integer]: TRangeList read GetItem; default;
  end;

  TSyncWord = class(TRange)
  end;

procedure GradientFill(Canvas: TCanvas; const R: TRect; c1, c2: TColor; Vertical: Boolean);
var bmp: TBitmap;
    p1, p2: array[0..2] of integer;
    c: TColor;
    w, i, j: integer;
    dc, destDc: HDC;
begin
  if not RectVisible(Canvas.Handle, R) or
     (R.Left = R.Right) or
     (R.Bottom = R.Top) then Exit;

  c1 := ColorToRGB(c1);
  c2 := ColorToRGB(c2);
  for i := 0 to 2 do
   begin
    p1[i] := (c1 shr (i * 8)) and $FF;
    p2[i] := (c2 shr (i * 8)) and $FF - p1[i];
   end;

  bmp := TBitmap.Create;
  try
   if Vertical then
    begin
     bmp.Width := 1;
     w := R.Bottom - R.Top;
     bmp.Height := w;
    end else
    begin
     bmp.Height := 1;
     w := R.Right - R.Left;
     bmp.Width := w;
    end;
   dc := bmp.Canvas.Handle;
   for i := 0 to w do
    begin
     c := 0;
     for j := 0 to 2 do
       c := c or ((p1[j] + p2[j]*i div w)  shl (j * 8));
     if Vertical then
       SetPixel(dc, 0, i, c)
     else
       SetPixel(dc, i, 0, c);
    end;
   // drawing
   destDc := Canvas.Handle;
   if Vertical then
    for i := R.Left to R.Right do
     BitBlt(destDc, i, R.Top, 1, w, dc, 0, 0, SRCCOPY)
   else
    for i := R.Top to R.Bottom do
     BitBlt(destDc, R.Left, i, w, 1, dc, 0, 0, SRCCOPY);
  finally
    bmp.Free;
  end;
end;

{ TLineInfo }

constructor TLineInfo.Create;
begin
  inherited;
  FWordBreaks := nil;
  FInvalid := False;
  FHeights := TList.Create;
  FWidths := TList.Create;
end;

destructor TLineInfo.Destroy;
begin
  FreeAndNil(FWordBreaks);
  FreeAndNil(FHeights);
  FreeAndNil(FWidths);
  inherited;
end;

procedure TLineInfo.Clear;
begin
  FreeAndNil(FWordBreaks);
  FHeights.Clear;
  FWidths.Clear;
end;

procedure TLineInfo.AddWordBreak(APos: integer);
begin
  if FWordBreaks = nil then
    FWordBreaks := TList.Create;
  FWordBreaks.Add(TObject(APos));
end;

function TLineInfo.GetCount: integer;
begin
  if FWordBreaks <> nil then Result := FWordBreaks.Count + 1
   else Result := 1;
end;

function TLineInfo.GetHeights(Index: integer): integer;
begin
  if Index < FHeights.Count then
    Result := integer(FHeights[Index])
  else
    Result := 0;
end;

function TLineInfo.GetTotalHeight: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to FHeights.Count - 1 do
   Result := Result + integer(FHeights[i]);
end;

function TLineInfo.GetWordBreak(Index: integer): integer;
begin
  if FWordBreaks = nil then
    Result := -1 //0
  else if Index < FWordBreaks.Count then
    Result := integer(FWordBreaks[Index])
  else
    Result := $7FFFFFFF;
end;

procedure TLineInfo.SetHeights(Index: integer; const Value: integer);
begin
  if Index >= FHeights.Count then
   FHeights.Count := Index + 1;
  FHeights[Index] := TObject(Value);
end;

function TLineInfo.SubLineForPos(Pos: integer): integer;
var i: integer;
begin
  for i := 0 to LineCount - 1 do
   if Pos < WordBreaks[i] then
    begin
     Result := i;
     Exit;
    end;
  Result := LineCount - 1;
end;

procedure TLineInfo.SetWordBreak(Index: integer; const Value: integer);
begin
  if FWordBreaks = nil then
    begin
      if Index = 0 then
       begin
         FWordBreaks := TList.Create;
         FWordBreaks.Add(TObject(Value));
       end else
       raise Exception.Create('Invalid wordbreak setting');
    end else
  if Index < FWordBreaks.Count then
    FWordBreaks[Index] := TObject(Value)
  else if Index = FWordBreaks.Count then
    FWordBreaks.Add(TObject(Value))
  else
    raise Exception.Create('Invalid wordbreak setting');
end;

function TLineInfo.GetHidden: Boolean;
begin
  Result := FHidden or FManualHidden;
end;

procedure TLineInfo.SetHidden(const Value: Boolean);
begin
  FManualHidden := Value;
end;

function TLineInfo.GetTotalWidth: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to FWidths.Count - 1 do
    if Widths[i] > Result then
      Result := Widths[i];
end;

function TLineInfo.GetWidths(Index: integer): integer;
begin
  if Index < FWidths.Count then
    Result := integer(FWidths[Index])
  else
    Result := 0;
end;

procedure TLineInfo.SetWidths(Index: integer; const Value: integer);
begin
  if Index >= FWidths.Count then
    FWidths.Count := Index + 1;
  FWidths[Index] := TObject(Value);
end;

{ TLineStateDisplay }

procedure TLineStateDisplay.Assign(Source: TPersistent);
begin
  if Source is TLineStateDisplay then
   with TLineStateDisplay(Source) do
    begin
      Self.FBand := FBand;
      Self.FModifiedColor := FModifiedColor;
      Self.FNewColor := FNewColor;
      Self.FUnchangedColor := FUnchangedColor;
      Self.FSavedColor := FSavedColor;
    end;
end;

constructor TLineStateDisplay.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
  FBand := -1;
  FModifiedColor := clYellow;
  FNewColor := clGreen;
  FUnchangedColor := clNone;
  FSavedColor := clNavy;
end;

procedure TLineStateDisplay.SetBand(const Value: integer);
begin
  FBand := Value;
  FOwner.InvalidateGutter;
end;

procedure TLineStateDisplay.SetModifiedColor(const Value: TColor);
begin
  FModifiedColor := Value;
  FOwner.InvalidateGutter;
end;

procedure TLineStateDisplay.SetNewColor(const Value: TColor);
begin
  FNewColor := Value;
  FOwner.InvalidateGutter;
end;

procedure TLineStateDisplay.SetSavedColor(const Value: TColor);
begin
  FSavedColor := Value;
  FOwner.InvalidateGutter;
end;

procedure TLineStateDisplay.SetUnchangedColor(const Value: TColor);
begin
  FUnchangedColor := Value;
  FOwner.InvalidateGutter;
end;

{ TCollapsedRange }

procedure TCollapsedRange.CopyFrom(Other: TCollapsedRange);
begin
  FLine := Other.FLine;
  FPos := Other.FPos;
  FEnd := Other.FEnd;
  FLineCount := Other.FLineCount;
  FValide := Other.FValide;
  FUser := Other.FUser;
  FColText := Other.FColText;
end;

constructor TCollapsedRange.Create(ALine, APos, AEnd, ALineCount: integer);
begin
  inherited Create;
  FLine := ALine;
  FPos := APos;
  FEnd := AEnd;
  FLineCount := ALineCount;
  FValide := True;
end;

constructor TCollapsedRange.CreateEmpty;
begin
  inherited Create;
end;

function TCollapsedRange.GetKey: integer;
begin
  Result := FLine;
end;

{ TCollapsedRanges }

constructor TCollapsedRanges.Create;//(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
//  FOwner := AOwner;
  FList := TSortedList.Create(True);
  FDiaps := TRangeList.Create;
end;

destructor TCollapsedRanges.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FDiaps);
  inherited;
end;

procedure TCollapsedRanges.Add(Range: TCollapsedRange);
begin
  FList.Add(Range);
  FDiaps.Add(TRange.Create(Range.FLine + 1, Range.FLine + Range.FLineCount));
end;

procedure TCollapsedRanges.Clear;
begin
  FList.Clear;
  FDiaps.Clear;
end;

procedure TCollapsedRanges.UpdateDiaps;
var i: integer;
begin
  FDiaps.Clear;
  for i := 0 to Count - 1 do
   with Ranges[i] do
    FDiaps.Add(TRange.Create(FLine + 1, FLine + FLineCount));
end;

function TCollapsedRanges.GetCount: integer;
begin
  Result := FList.Count;
end;

function TCollapsedRanges.GetItem(Index: integer): TCollapsedRange;
begin
  Result := TCollapsedRange(FList[Index]);
end;

function TCollapsedRanges.GetCollapsed(aLine: integer): TCollapsedRange;
begin
  Result := TCollapsedRange(FList.GetAt(aLine));
end;

function TCollapsedRanges.CorrectCollapsed(APos, Count: integer; LineCountChanged: integer): Boolean;
var i: integer;
begin
  Result := False;
  for i := FList.Count - 1 downto 0 do
   with GetItem(i) do
    if FEnd >= APos then
     begin
      FValide := False;
      if (FPos >= APos) and (FPos >= APos - Count) then
       begin
        FPos := FPos + Count;
        Inc(FLine, LineCountChanged);
       end else
//       if LineCountChanged <> 0 then
       begin
         FList.Delete(i);
         Result := True;
       end;
       Result := Result or (LineCountChanged <> 0);
     end;
  if Result then
    UpdateDiaps;
end;

procedure TCollapsedRanges.Delete(Index: integer);
begin
  FList.Delete(Index);
  UpdateDiaps;
end;

function TCollapsedRanges.GetCollapsedIndex(aLine: integer): integer;
begin
  Result := FList.GetIndexAt(aLine);
end;

function TCollapsedRanges.IsLineVisible(Line: integer): Boolean;
begin
  Result := FDiaps.RangeAt(Line) = -1;
end;

procedure TCollapsedRanges.Assign(Source: TPersistent);
var src: TCollapsedRanges;
    rng: TCollapsedRange;
    i: integer;
begin
  if Source is TCollapsedRanges then
    begin
      Clear;
      src := TCollapsedRanges(Source);
      for i := 0 to src.Count - 1 do
       begin
        rng := TCollapsedRange.CreateEmpty;
        rng.CopyFrom(src[i]);
        Add(rng);
       end;
    end else
  inherited;
end;

{ TCustomGutterObject }

constructor TCustomGutterObject.CreateObj(ALine: integer; AImageIndex: TImageIndex);
begin
  inherited Create(nil);
  Init;
  FLine := ALine;
  FImageIndex := AImageIndex;
end;

constructor TCustomGutterObject.Create(Collection: TCollection);
begin
  inherited;
  Init;
end;

procedure TCustomGutterObject.Init;
begin
  FBand := -1;
  FLine := -1;
  FSubLine := -1;
  FImageIndex := -1;
  FMargin := -1;
  FImageList := nil;
  FHint := '';
  FForeColor := clNone;
  FBgColor := clNone;
  FSelInvertColors := False;
  FCursor := crDefault;
end;

procedure TCustomGutterObject.Assign(Source: TPersistent);
var src: TCustomGutterObject;
begin
  if Source is TCustomGutterObject then
   begin
     src := Source as TCustomGutterObject;
     FLine := src.FLine;
     FImageIndex := src.FImageIndex;
     FMargin := src.FMargin;
     FImageList := src.FImageList;
     FHint := src.FHint;
     FForeColor := src.FForeColor;
     FBgColor := src.FBgColor;
     FSelInvertColors := src.FSelInvertColors;
     FName := src.FName;
     FCursor := src.FCursor;
   end;
end;

procedure TCustomGutterObject.SetBgColor(const Value: TColor);
begin
  if FBgColor <> Value then
    begin
      FBgColor := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then
    begin
      FForeColor := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SetHint(const Value: string);
begin
  if FHint <> Value then
    begin
      FHint := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
    begin
      FImageIndex := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SeTCustomImageList(const Value: TCustomImageList);
begin
  if FImageList <> Value then
   begin
    FImageList := Value;
    DoFreeNotification(FImageList);
    Changed(False);
   end;
end;

procedure TCustomGutterObject.SetLine(const Value: integer);
begin
  if FLine <> Value then
    begin
      FLine := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SetMargin(const Value: integer);
begin
  if FMargin <> Value then
    begin
      FMargin := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SetSelInvertColors(const Value: Boolean);
begin
  if FSelInvertColors <> Value then
    begin
      FSelInvertColors := Value;
      Changed(False);
    end;
end;

function TCustomGutterObject.CheckLine(ALine: integer): Boolean;
begin
  Result := ALine = FLine;
  if Assigned(FOnCheckLine) then
   FOnCheckLine(Self, ALine, Result);
end;

procedure TCustomGutterObject.SetBand(const Value: integer);
begin
  if FBand <> Value then
    begin
      FBand := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SetPopupMenu(const Value: TPopupMenu);
begin
  if Assigned(Collection) then
   begin
    DoFreeNotification(Value);
    FPopupMenu := Value;
   end;
end;

{$IFNDEF EC_VCL6_UP}
type TPersHack = class(TPersistent);
{$ENDIF}

procedure TCustomGutterObject.DoFreeNotification(Cmp: TComponent);
var own: TObject;
begin
  if (Cmp = nil) or (Collection = nil) then Exit;
{$IFNDEF EC_VCL6_UP}
  own := TPersHack(Collection).GetOwner;
{$ELSE}
  own := Collection.Owner;
{$ENDIF}
  if own <> nil then
    if own is TGutter then
     Cmp.FreeNotification((own as TGutter).FOwner) else
    if own is TComponent then
     Cmp.FreeNotification(TComponent(own));
end;

destructor TCustomGutterObject.Destroy;
var own: TPersistent;
begin
  if Collection <> nil then
   begin
  {$IFNDEF EC_VCL6_UP}
    own := TPersHack(Collection).GetOwner;
  {$ELSE}
    own := Collection.Owner;
  {$ENDIF}
    if own <> nil then
      if own is TGutter then
        own := TGutter(own).Owner else
      if not (own is TCustomSyntaxMemo) then
        own := nil;
    if own <> nil then
      with TCustomSyntaxMemo(own) do
        if (FLineInfos <> nil) and not (csDestroying in ComponentState) then
          FLineInfos.Remove(Self);
   end;
  inherited;
end;

procedure TCustomGutterObject.SetCursor(const Value: TCursor);
begin
  if FCursor <> Value then
    begin
      FCursor := Value;
      Changed(False);
    end;
end;

procedure TCustomGutterObject.SetSubLine(const Value: integer);
begin
  if FSubLine <> Value then
    begin
      FSubLine := Value;
      Changed(False);
    end;
end;

{ TGutterObjects }

function TGutterObjects.Add: TGutterObject;
begin
  Result := (inherited Add) as TGutterObject
end;

function TGutterObjects.GetItem(Index: integer): TGutterObject;
begin
  Result := (inherited Items[Index]) as TGutterObject;
end;

procedure TGutterObjects.SetItem(Index: integer; const Value: TGutterObject);
begin
  Items[Index].Assign(Value);
end;

procedure TGutterObjects.Update(Item: TCollectionItem);
begin
  with (GetOwner as TGutter).Owner do
   if csDesigning in ComponentState then
    Invalidate;
end;

{ TTabList }

constructor TTabList.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FList := TList.Create;
  FOwner := AOwner;
end;

destructor TTabList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TTabList.Add(NewTab: integer);
var i: integer;
begin
  Assert(FList<>nil);
  if FList.IndexOf(ecPointer(NewTab)) <> -1 then
    raise Exception.Create('Duplicate tab position');
  for i := 0 to FList.Count - 1 do
   if NewTab < integer(FList[i]) then
    begin
      FList.Insert(i, ecPointer(NewTab));
      Exit;
    end;
  FList.Add(ecPointer(NewTab));
end;

procedure TTabList.Assign(Source: TPersistent);
var i: integer;
begin
  if Source is TTabList then
  begin
   FList.Clear;
   with Source as TTabList do
    for i := 0 to FList.Count - 1 do
     Self.FList.Add(FList[i]);
  end;
end;

function TTabList.GetAsString: string;
var i: integer;
begin
  Result := '';
  for i := 0 to FList.Count - 1 do
   begin
     if i <> 0 then Result := Result + ' ';
     Result := Result + IntToStr(integer(FList[i]));
   end;
end;

function TTabList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TTabList.GetItems(Index: integer): integer;
begin
  Result := integer(FList[Index]);
end;

procedure TTabList.SetAsString(const Value: string);
var st, i, L: integer;
begin
  FList.Clear;
  i := 1;
  L := Length(Value);
  while i <= L do
   begin
     while not IsIdentDigitChar(Value[i]) do
      begin
       Inc(i);
       if i > L then Exit;
      end;
     st := i;
     Inc(i);
     while (i <= L) and IsIdentDigitChar(Value[i]) do Inc(i);
     Add(StrToInt(Copy(Value, st, i - st)));
   end;
  FOwner.UpdateEditor;
end;

procedure TTabList.SetItems(Index: integer; const Value: integer);
begin
  Delete(Index);
  Add(Value);
end;

procedure TTabList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

// for tab char in logical logPos return next char pos (relatively to 0)
function TTabList.NextTab(pos: integer; AllowZeroTab: Boolean): integer;
var i, dx: integer;
begin
  for i := 0 to FList.Count - 1 do
   if (integer(FList[i]) > pos) or (AllowZeroTab and (integer(FList[i]) = pos)) then
    begin
     Result := integer(FList[i]);
     Exit;
    end;
  if FList.Count = 0 then Result := pos + 1 else
   begin
     Result := integer(FList.Last);
     if FList.Count = 1 then dx := Result
      else dx := Result - integer(FList[FList.Count - 2]);
     if dx <= 1 then dx := 1;
     if AllowZeroTab and ((pos - Result) mod dx = 0) then
       Result := pos
     else
       Result := Result + ((pos - Result) div dx + 1) * dx;
   end;
end;

procedure TTabList.Clear;
begin
  FList.Clear;
end;

{ TNonPrinted }

constructor TNonPrinted.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
  FSpaceChar := '·';
  FTabChar := '›';
  FLineBreakChar := '¶';
  FSoftLineBreakChar := '¬';
  FFont := TFont.Create;
  FFont.Color := clSilver;
end;

destructor TNonPrinted.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TNonPrinted.Assign(Source: TPersistent);
var src: TNonPrinted;
begin
  if Source is TNonPrinted then
   begin
     src := Source as TNonPrinted;
     FSpaceChar := src.SpaceChar;
     FTabChar := src.TabChar;
     FLineBreakChar := src.LineBreakChar;
     FSoftLineBreakChar := src.SoftLineBreakChar;
     FFont.Assign(src.Font);
     FVisible := src.Visible;
     FUseFont := src.UseFont;
   end;
end;

procedure TNonPrinted.SetColor(const Value: TColor);
begin
  FFont.Color := Value;
  if Visible then FOwner.Invalidate;
end;

procedure TNonPrinted.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if Visible and UseFont then FOwner.Invalidate;
end;

procedure TNonPrinted.SetLineBreakChar(const Value: Char);
begin
  FLineBreakChar := Value;
  if Visible then FOwner.Invalidate;
end;

procedure TNonPrinted.SetSpaceChar(const Value: Char);
begin
  FSpaceChar := Value;
  if Visible then FOwner.Invalidate;
end;

procedure TNonPrinted.SetTabChar(const Value: Char);
begin
  FTabChar := Value;
  if Visible then FOwner.Invalidate;
end;

procedure TNonPrinted.SetUseFont(const Value: Boolean);
begin
  FUseFont := Value;
  if Visible then FOwner.Invalidate;
end;

procedure TNonPrinted.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
   begin
    FVisible := Value;
    FOwner.ResetLineHeights;
    FOwner.UpdateEditor;
   end;
end;

function TNonPrinted.GetColor: TColor;
begin
  Result := FFont.Color;
end;

procedure TNonPrinted.SetSoftLineBreakChar(const Value: Char);
begin
  FSoftLineBreakChar := Value;
  if Visible then FOwner.Invalidate;
end;

{ TLineNumbers }

constructor TLineNumbers.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 7;
  FFont.OnChange := FontChanged;
  FVisible := True;
  FUnderColor := clNone;
  FMargin := 2;
  FAlignment := taRightJustify;
  FVertAlignment := vaCenter;
  FBand := -1;
  FFirstLineNumber := 1;
  FNumberingStart := -1;
  FNumberingEnd := -1;
end;

destructor TLineNumbers.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TLineNumbers.Assign(Source: TPersistent);
var src: TLineNumbers;
begin
  if Source is TLineNumbers then
   begin
     src := Source as TLineNumbers;
     FVisible := src.Visible;
     FUnderColor := src.UnderColor;
     FFont.Assign(src.Font);
     FMargin := src.FMargin;
     FAlignment := src.FAlignment;
     FVertAlignment := src.FVertAlignment;
     FBand := src.FBand;
     FFirstLineNumber := src.FFirstLineNumber;
     FNumberingStart := src.FNumberingStart;
     FNumberingEnd := src.FNumberingEnd;
     FNumberingStyle := src.FNumberingStyle;
     FAutoSize := src.FAutoSize;
     UpdateBandWidth;
     Update;
   end;
end;

procedure TLineNumbers.SetColor(const Value: TColor);
begin
  if FUnderColor <> Value then
    begin
      FUnderColor := Value;
      Update;
    end;
end;

procedure TLineNumbers.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TLineNumbers.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      FVisible := Value;
      UpdateBandWidth;
      FOwner.InvalidateGutter;
    end;
end;

procedure TLineNumbers.FontChanged(Sender: TObject);
begin
  UpdateBandWidth;
  Update;
end;

procedure TLineNumbers.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
    begin
      FAlignment := Value;
      Update;
    end;
end;

procedure TLineNumbers.SetMargin(const Value: integer);
begin
  if FMargin <> Value then
    begin
      FMargin := Value;
      Update;
    end;
end;

procedure TLineNumbers.SetVertAlignment(const Value: TVertAlignment);
begin
  if FVertAlignment <> Value then
    begin
      FVertAlignment := Value;
      Update;
    end;
end;

procedure TLineNumbers.SetBand(const Value: integer);
begin
  if FBand <> Value then
    begin
      FBand := Value;
      UpdateBandWidth;
      Update;
    end;
end;

function TLineNumbers.GetNumberString(Line: integer): string;
var ln: integer;
begin
  if (FNumberingStart > 0) and (Line + 1 < FNumberingStart) or
     (FNumberingEnd > 0) and (Line + 1 > FNumberingEnd) then
   begin
     Result := '';
     Exit;
   end;
  ln := Line + FFirstLineNumber;
  if FNumberingStart > 0 then Dec(ln, FNumberingStart - 1);

  case FNumberingStyle of
    lsBDS:
      begin
        if (Ln mod 10 = 0) or
           (FOwner.Collapsed.GetCollapsedIndex(Line) <> -1) or
           (Line = FOwner.CaretPos.Y) then
          Result := IntToStr(ln)
        else
        if Ln mod 5 = 0 then
          Result := '-'
        else
          Result := '.';
      end;
    lsEach5:
      begin
        if (Ln mod 5 = 0) or
           (FOwner.Collapsed.GetCollapsedIndex(Line) <> -1) or
           (Line = FOwner.CaretPos.Y) then
          Result := IntToStr(Ln)
        else
          Result := '.';
      end;
    lsOnlyCurent:
        if Line = FOwner.CaretPos.Y then
          Result := IntToStr(ln)
        else
          Result := '';
    else Result := IntToStr(ln);
  end;
end;

procedure TLineNumbers.SetFirstLineNumber(const Value: integer);
begin
  FFirstLineNumber := Value;
  Update;
end;

procedure TLineNumbers.SetNumberingEnd(const Value: integer);
begin
  FNumberingEnd := Value;
  Update;
end;

procedure TLineNumbers.SetNumberingStart(const Value: integer);
begin
  FNumberingStart := Value;
  Update;
end;

procedure TLineNumbers.Update;
begin
  if Visible then
    FOwner.InvalidateGutter;
end;

procedure TLineNumbers.SetNumberingStyle(const Value: TLineNumberingStyle);
begin
  if FNumberingStyle <> Value then
   begin
    FNumberingStyle := Value;
    Update;
   end;
end;

procedure TLineNumbers.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
    begin
      FAutoSize := Value;
      UpdateBandWidth;
    end;
end;

procedure TLineNumbers.UpdateBandWidth;
var N: integer;
    DC: HDC;
    sz: TSize;
begin
  if FAutoSize and FVisible then
    begin
      N := Length(IntToStr(FOwner.Lines.Count));
      DC := GetDC(0);
      SelectObject(DC, FFont.Handle);
      GetTextExtentPoint32(DC, '8', 1, sz);
      ReleaseDC(0, DC);
      sz.cx := sz.cx * N + FMargin + 2; //AT

      if FBand = -1 then FOwner.Gutter.Width := sz.cx else
      if (FBand >= 0) and (FBand < FOwner.Gutter.Bands.Count) then
        FOwner.Gutter.Bands[FBand].Width := sz.cx;
    end;
end;

{ TGutterBand }

constructor TGutterBand.Create(Collection: TCollection);
begin
  FColor := clNone;
  FLeftBound := clNone;
  FRightBound := clNone;
  FGradientRight := clWhite;
  FCursor := crDefault;
  FMouseMoveCaret := True;
  inherited;
end;

procedure TGutterBand.Assign(Source: TPersistent);
var src: TGutterBand;
begin
  if Source is TGutterBand then
   begin
     src := Source as TGutterBand;
     FColor := src.FColor;
     FLeftBound := src.FLeftBound;
     FRightBound := src.FRightBound;
     FWidth := src.FWidth;
     FGradient := src.FGradient;
     FGradientRight := src.FGradientRight;
     FCursor := src.FCursor;
     FMouseMoveCaret := src.FMouseMoveCaret;
   end;
end;

procedure TGutterBand.SetColor(const Value: TColor);
begin
  if FColor <> Value then
    begin
      FColor := Value;
      Changed(False);
    end;
end;

procedure TGutterBand.SetLeftBound(const Value: TColor);
begin
  if FLeftBound <> Value then
    begin
      FLeftBound := Value;
      Changed(False);
    end;
end;

procedure TGutterBand.SetRightBound(const Value: TColor);
begin
  if FRightBound <> Value then
    begin
      FRightBound := Value;
      Changed(False);
    end;
end;

procedure TGutterBand.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
    begin
      FWidth := Value;
      Changed(False);
    end;
end;

procedure TGutterBand.SetGradient(const Value: Boolean);
begin
  if FGradient <> Value then
    begin
      FGradient := Value;
      Changed(False);
    end;
end;

procedure TGutterBand.SetGradientRight(const Value: TColor);
begin
  if FGradientRight <> Value then
    begin
      FGradientRight := Value;
      Changed(False);
    end;
end;

procedure TGutterBand.SetCursor(const Value: TCursor);
begin
  if FCursor <> Value then
    begin
      FCursor := Value;
      Changed(False);
    end;
end;

procedure TGutterBand.SetMouseMoveCaret(const Value: Boolean);
begin
  if FMouseMoveCaret <> Value then
    begin
      FMouseMoveCaret := Value;
      Changed(False);
    end;
end;

{ TGutterBands }

function TGutterBands.Add: TGutterBand;
begin
  Result := (inherited Add) as TGutterBand;
end;

function TGutterBands.GetItem(Index: integer): TGutterBand;
begin
  Result := (inherited Items[Index]) as TGutterBand;
end;

procedure TGutterBands.SetItem(Index: integer; const Value: TGutterBand);
begin
  (inherited Items[Index]).Assign(Value);
end;

procedure TGutterBands.Update(Item: TCollectionItem);
begin
  with GetOwner as TGutter do
   if not RecalcWidth then
    Owner.InvalidateGutter;
end;

{ TGutter }

constructor TGutter.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
  FBands := TGutterBands.Create(Self, TGutterBand);
  FExpandButtons := TBitmap.Create;
  FExpandButtons.LoadFromResourceName({$IFDEF EC_DOTNET}Application.Handle{$ELSE}HInstance{$ENDIF}, 'STDEXPANDBUTTONS');
  FVisible := True;
  FWidth := 30;
  FColor := clBtnFace;
  FObjects := TGutterObjects.Create(Self, TGutterObject);
  FShowSeparator := True;
  FExpBtnBand := -1;
  FCollapsePen := TPen.Create;
  FCollapsePen.Color := clGray;
  FShowCollapseLine := True;
  FBuffer := TBitmap.Create;
  FSeparatorColor := clGray;
  FCursor := crArrow;
  FLineBreakObj := -1;
end;

destructor TGutter.Destroy;
begin
  FExpandButtons.Free;
  FBands.Free;
  FObjects.Free;
  FCollapsePen.Free;
  if Assigned(FBuffer) then FBuffer.Free;
  inherited;
end;

procedure TGutter.Assign(Source: TPersistent);
var src: TGutter;
begin
  if Source is TGutter then
   begin
     src := Source as TGutter;
     FWidth := src.Width;
     FColor := src.Color;
     FVisible := src.Visible;
     FExpandButtons.Assign( src.ExpandButtons);
     Images := src.Images;
     FBands.Assign(src.Bands);
     FObjects.Assign(src.Objects);
     FExpBtnBand := src.ExpBtnBand;
     FShowSeparator := src.ShowSeparator;
     FCollapsePen.Assign(src.CollapsePen);
     FShowCollapseLine := src.ShowCollapseLine;
     FAutoSize := src.FAutoSize;
     DoubleBufered := src.DoubleBufered;
     FSeparatorColor := src.FSeparatorColor;
     FCursor := src.FCursor;
     FMouseMoveCaret := src.FMouseMoveCaret;
   end;
end;

procedure TGutter.SetColor(const Value: TColor);
begin
  FColor := Value;
  FOwner.InvalidateGutter;
end;

procedure TGutter.SetExpandButtons(const Value: TBitmap);
begin
  FExpandButtons.Assign(Value);
  FOwner.InvalidateGutter;
end;

procedure TGutter.SetImages(const Value: TCustomImageList);
begin
  if ChangeComponentReference(FOwner, Value, TComponent(FImages)) then
    FOwner.InvalidateGutter;
end;

procedure TGutter.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      FVisible := Value;
      FOwner.UpdateMargin;
    end;
end;

procedure TGutter.SetWidth(const Value: integer);
begin
  if FAutoSize then
    RecalcWidth
  else
    if FWidth <> Value then
      begin
        FWidth := Value;
        FOwner.UpdateMargin;
      end;
end;

procedure TGutter.SetBands(const Value: TGutterBands);
begin
  FBands.Assign(Value);
  Owner.InvalidateGutter;
end;

function TGutter.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGutter.SetObjects(const Value: TGutterObjects);
begin
  FObjects.Assign(Value);
  Owner.Invalidate;
end;

procedure TGutter.SetExpBtnBand(const Value: integer);
begin
  FExpBtnBand := Value;
  Owner.InvalidateGutter;
end;

procedure TGutter.SetShowSeparator(const Value: Boolean);
begin
  if FShowSeparator <> Value then
    begin
      FShowSeparator := Value;
      Owner.UpdateMargin;
    end;
end;

function TGutter.GetBandBound(BandIndex: integer): TPoint;
var i: integer;
begin
  Result.X := 0;
  if (BandIndex >= 0) and (BandIndex < Bands.Count) then
   begin
    for i := 0 to BandIndex - 1 do
     Inc(Result.X, Bands[i].Width);
    Result.Y := Result.X + Bands[BandIndex].Width;
   end else Result.Y := Width;
end;

function TGutter.BandAt(X: integer): integer;
var i: integer;
begin
  for i := 0 to Bands.Count - 1 do
    begin
      Dec(X, Bands[i].Width);
      if X < 0 then
        begin
          Result := i;
          Exit;
        end;
    end;
  Result := -1;
end;

function TGutter.ExpBtnRect(Y1, Y2: integer): TRect;
var ts: TSize;
    pn: TPoint;
begin
  if ExpandButtons.Empty then
   begin
    Owner.Canvas.Font := Owner.Font;
    ts :=  Owner.Canvas.TextExtent('+');
   end else
   begin
    ts.cx := ExpandButtons.Width div 2;
    ts.cy := ExpandButtons.Height;
   end;
   pn := GetBandBound(ExpBtnBand);
   Result := Bounds(pn.Y - ts.cx - 1, (Y2 + Y1 - ts.cy) div 2, ts.cx, ts.cy);
end;

procedure TGutter.SetCollapsePen(const Value: TPen);
begin
  FCollapsePen.Assign(Value);
  FOwner.InvalidateGutter;
end;

procedure TGutter.SetShowCollapseLine(const Value: Boolean);
begin
  FShowCollapseLine := Value;
  FOwner.InvalidateGutter;
end;

function TGutter.GetDoubleBufered: Boolean;
begin
  Result := Assigned(FBuffer);
end;

procedure TGutter.SetDoubleBufered(const Value: Boolean);
begin
  if Value <> GetDoubleBufered then
   if Value then FBuffer := TBitmap.Create
    else FreeAndNil(FBuffer);
end;

procedure TGutter.SetPopupMenu(const Value: TPopupMenu);
begin
  ChangeComponentReference(FOwner, Value, TComponent(FPopupMenu));
end;

procedure TGutter.SetSeparatorColor(const Value: TColor);
begin
  if FSeparatorColor <> Value then
    begin
      FSeparatorColor := Value;
      if FShowSeparator then
        FOwner.Invalidate;
    end;
end;

procedure TGutter.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
   begin
    FAutoSize := Value;
    RecalcWidth;
   end;
end;

function TGutter.RecalcWidth: Boolean;
var nw, i: integer;
begin
  if FAutoSize then
   begin
    nw := 0;
    for i := 0 to Bands.Count - 1 do
     Inc(nw, Bands[i].Width);
    Result := Width <> nw;
    if Result then
      begin
        FWidth := nw;
        FOwner.UpdateMargin;
      end;
   end else Result := False;
end;

procedure TGutter.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
end;

procedure TGutter.SetMouseMoveCaret(const Value: Boolean);
begin
  FMouseMoveCaret := Value;
end;

function TGutter.MouseMoveCaretAt(X: integer): Boolean;
var i: integer;
begin
  Result := MouseMoveCaret;
  if Result then
    begin
      i := BandAt(X);
      Result := (i <> -1) and Bands[i].MouseMoveCaret;
    end;
end;

procedure TGutter.SetLineBreakObj(const Value: integer);
begin
  if FLineBreakObj <> Value then
    begin
      FLineBreakObj := Value;
      FOwner.InvalidateGutter;
    end;
end;

{ THintProps }

constructor THintProps.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
  FTimeGutter := 5000;
  FTimeTokens := 10000;
  FDelayBefore := 500;
  FTimeCollapsed := 10000;
  FCollapsedLines := 20;
  ShowHints := [shScroll, shCollapsed, shGutter, shTokens];
end;

procedure THintProps.Assign(Source: TPersistent);
var src: THintProps;
begin
  if Source is THintProps then
   begin
    src := Source as THintProps;
    Font := src.Font;
    Color := src.Color;
    ShowHints := src.ShowHints;
    DelayBefore := src.DelayBefore;
    TimeCollapsed := src.TimeCollapsed;
    TimeGutter := src.TimeGutter;
    TimeTokens := src.TimeTokens;
   end;
end;

function THintProps.GetColor: TColor;
begin
  Result := FOwner.FHint.Color;
end;

function THintProps.GetFont: TFont;
begin
  Result := FOwner.FHint.Font;
end;

procedure THintProps.SetColor(const Value: TColor);
begin
  FOwner.FHint.Color := Value;
end;

procedure THintProps.SetDelayBefore(const Value: integer);
begin
  if Value <= 0 then
   raise Exception.Create('Time must be greater 0');
  FDelayBefore := Value;
end;

procedure THintProps.SetFont(const Value: TFont);
begin
  FOwner.FHint.Font := Value;
end;

procedure THintProps.SetShowHints(const Value: TSyntShowHints);
begin
  FShowHints := Value;
end;

procedure THintProps.SetTimeCollapsed(const Value: integer);
begin
  if Value <= 0 then
   raise Exception.Create('Time must be greater 0');
  FTimeCollapsed := Value;
end;

procedure THintProps.SetTimeGutter(const Value: integer);
begin
  if Value <= 0 then
   raise Exception.Create('Time must be greater 0');
  FTimeGutter := Value;
end;

procedure THintProps.SetTimeTokens(const Value: integer);
begin
  if Value <= 0 then
   raise Exception.Create('Time must be greater 0');
  FTimeTokens := Value;
end;

procedure THintProps.SetCollapsedLines(const Value: integer);
begin
  if Value <= 0 then
   raise Exception.Create('Time must be greater 0');
  FCollapsedLines := Value;
end;

procedure THintProps.SetFormated(const Value: Boolean);
begin
  FFormated := Value;
end;

procedure THintProps.SetImages(const Value: TCustomImageList);
begin
  ChangeComponentReference(FOwner, Value, TComponent(FImages));
end;

procedure THintProps.SetStyles(const Value: TSyntStyles);
begin
  ChangeComponentReference(FOwner, Value, TComponent(FStyles));
end;

procedure THintProps.SetShowFirstLine(const Value: Boolean);
begin
  FShowFirstLine := Value;
end;

procedure THintProps.SetHintLines(StartLine, EndLine: integer);
begin
  if FOwner.FHint is TSyntHintWindow then 
    (FOwner.FHint as TSyntHintWindow).SetLines(StartLine, EndLine);
end;

{ TSyntHintWindow }

function TSyntHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: String; AData: ecPointer): TRect;
const
   SideOffset = 6;
var i, w: integer;
    Info: TLineInfo;
    M: TCustomSyntaxMemo;
begin
  M := TCustomSyntaxMemo(Owner);

  if MaxWidth <= 0 then MaxWidth := M.ClientWidth - M.FMargin.Left - M.FMargin.Right;
  Dec(MaxWidth, SideOffset);
  Result := Rect(0, 0, MaxWidth, 0);

  if FLineInfos <> nil then
   begin
     FLineInfos.Clear;
     Result.Right := 0;
     M.PrintOptions := [mpLineHighlight, mpBlockHighlight, mpBackColor];
     for i := FirstLine to LastLine do
      if (i >= 0) and (i < M.Lines.Count) then
       begin
         Info := TLineInfo.Create;
         M.ProcessLine(Canvas, Info, i, ptLineHeight, Rect(0, 0, MaxWidth, 0), Point(0, 0));
         FLineInfos.Add(Info);
         if Info.LineCount = 0 then Continue;
         if Info.Width > Result.Right then
          if Info.Width < MaxWidth then Result.Right := Info.Width
           else Result.Right := MaxWidth;
         Inc(Result.Bottom, Info.Heights[0]);
       end;
   end else
  if M.HintProps.Formated then
    begin
     FreeAndNil(FComplRender);
     FComplRender := TComplexRender.Create;
     FComplRender.Canvas := Canvas;
     FComplRender.Images := M.HintProps.Images;
     if M.HintProps.Styles <> nil then
       FComplRender.Styles := M.HintProps.Styles.Styles;
     FComplRender.Font := Font;
     FComplRender.ColumnSpace := 8;
     FComplRender.Items.Text := AHint;
     w := FComplRender.Width;
     if Result.Right > w then
       Result.Right := w;
     Result.Bottom := FComplRender.Height;
    end else
    begin
      Canvas.Font := Font;
      DrawText(Canvas.Handle, ecPChar(AHint), -1, Result, DT_CALCRECT or DT_LEFT or DT_EXPANDTABS or
        DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
    end;

    Inc(Result.Right, SideOffset);
end;

destructor TSyntHintWindow.Destroy;
begin
  FreeAndNil(FLineInfos);
  FreeAndNil(FComplRender);
  inherited;
end;

procedure TSyntHintWindow.Paint;
var R: TRect;
    i: integer;
    Info: TLineInfo;
begin
  R := ClientRect;
  Inc(R.Left, 1);
//  Inc(R.Top, 0);
  if FLineInfos <> nil then
   with TCustomSyntaxMemo(Owner) do
     begin
       PrintOptions := [mpLineHighlight, mpBlockHighlight, mpBackColor];
       for i := 0 to Self.FLineInfos.Count - 1 do
       begin
         Info := TLineInfo(Self.FLineInfos[i]);
         if Info.LineCount > 0 then
          begin
            R.Bottom := R.Top + Info.Heights[0];
            ProcessLine(Self.Canvas, Info, i + FirstLine,
              ptDrawing, R, Point(0, 0));
            Inc(R.Top, Info.Heights[0]);
          end;
       end;
     end else
  if FComplRender <> nil then
   begin
    for i := 0 to FComplRender.VisibleCount - 1 do
      begin
        Canvas.Brush.Color := Color;
        R.Bottom := R.Top + FComplRender.ItemHeight(i);
        FComplRender.DrawItem(i, R, False);
        R.Top := R.Bottom;
      end;
   end else
   begin
    Canvas.Font := Font;
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, ecPChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or DT_EXPANDTABS or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
   end;
end;

procedure TSyntHintWindow.ResetLines;
begin
  FreeAndNil(FLineInfos);
  FreeAndNil(FComplRender);
end;

procedure TSyntHintWindow.SetLines(First, Last: integer);
begin
  FirstLine := First;
  LastLine  := Last;
  if FLineInfos <> nil then FLineInfos.Clear
   else FLineInfos := TObjectList.Create;
end;

procedure TSyntHintWindow.WMNCPaint(var Message: TMessage); //!!
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    Rectangle(DC,0,0,Width, Height);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

{ TUserRange }

procedure TUserRange.Assign(Source: TPersistent);
begin
  if Source is TUserRange then
   with Source as TUserRange do
    begin
      Self.LineBreaks := FLineBreaks;
      Self.Collapsable := FCollapsable;
      Self.Style := FStyle;
      Self.StartPos := StartPos;
      Self.EndPos := EndPos;
    end;
end;

constructor TUserRange.Create(Collection: TCollection);
begin
  inherited;
  FLineBreakColor := clBlue;
end;

destructor TUserRange.Destroy;
begin
  with TUserRanges(Collection) do
   begin
     Assert(Assigned(FFormatted));
     if (FFormatted.IndexOf(Self) <> -1) and (FOwner <> nil) then
      with FOwner do
       if not (csDestroying in ComponentState) then
        ResetLineHeights(False, FStartLine);
     FFormatted.Remove(Self);
     FCollapsible.Remove(Self);
     FWithSeparators.Remove(Self);
   end;
  inherited;
end;

function TUserRange.GetAsString: string;
begin
  Result := IntToStr(StartPos) + ';' +
            IntToStr(EndPos) + ';' +
            IntToStr(Flags) + ';' +
            {$IFDEF EC_VCL6_UP}
            AnsiQuotedStr(Style, '"') + ';' +
            {$ELSE}
            Style + ';' +
            {$ENDIF}
            IntToStr(LineBreakColor) + ';' +
            IntToStr(Tag);
end;

procedure TUserRange.SetAsString(const Value: string);
var st: TzStringList;
begin
  st := TzStringList.Create;
  try
    st.Delimiter := ';';
    st.DelimitedText := Value;
    {$IFDEF EC_VCL6_UP}
    st.QuoteChar := '"';
    {$ENDIF}
    StartPos := StrToInt(st[0]);
    EndPos := StrToInt(st[1]);
    Flags := StrToInt(st[2]);
    Style := st[3];
    LineBreakColor := StrToInt(st[4]);
    Tag := StrToInt(st[5]);
  except
  end;
  st.Free;
end;

procedure TUserRange.SetCollapsable(const Value: Boolean);
begin
  if FCollapsable <> Value then
   with TUserRanges(Collection) do
   begin
     if FCollapsable then FCollapsible.Remove(Self)
                     else FCollapsible.Add(Self);
     FCollapsable := Value;
     Self.Changed(False);
   end;
end;

procedure TUserRange.SetEndLine(const Value: integer);
begin
  with Collection as TUserRanges do
   if FOwner <> nil then
    if (Value >= 0) and (Value < FOwner.Lines.Count) then
     EndPos := FOwner.CaretPosToStrPos(Point(0, Value + 1)); // fix
end;

procedure TUserRange.SetEndPos(const Value: integer);
begin
  FEndPos := Value;
  with Collection as TUserRanges do
   if FOwner <> nil then
     with FOwner.StrPosToCaretPos(FEndPos) do
      if (X = 0) and (Y > 0) then  // fix
        FEndLine := Y - 1
      else
        FEndLine := Y;
  Changed(False);
end;

procedure TUserRange.SetHighlightLines(const Value: Boolean);
begin
  FHighlightLines := Value;
  Changed(False);
end;

procedure TUserRange.SetLineBreakColor(const Value: TColor);
begin
  FLineBreakColor := Value;
  Changed(False);
end;

procedure TUserRange.SetLineBreaks(const Value: TLineBreakBound);
begin
  if FLineBreaks <> Value then
   with TUserRanges(Collection) do
   begin
     if FLineBreaks <> [] then FWithSeparators.Remove(Self);
     FLineBreaks := Value;
     if FLineBreaks <> [] then FWithSeparators.Add(Self);
     Self.Changed(False);
   end;
end;

procedure TUserRange.SetStartLine(const Value: integer);
begin
  with (Collection as TUserRanges) do
   if FOwner <> nil then
    StartPos := FOwner.CaretPosToStrPos(Point(0, Value));
end;

procedure TUserRange.SetStartPos(const Value: integer);
begin
  FStartPos := Value;
  with (Collection as TUserRanges) do
   if FOwner <> nil then
    FStartLine := FOwner.StrPosToCaretPos(FStartPos).Y;
  Changed(False);
end;

procedure TUserRange.SetStyle(const Value: string);
begin
  if FStyle <> Value then
   with TUserRanges(Collection) do
   begin
     if FStyle <> '' then FFormatted.Remove(Self);
     FStyle := Value;
     if FStyle <> '' then FFormatted.Add(Self);
     with TUserRanges(Collection)  do
      if FOwner <> nil then
        FOwner.ResetLineHeights(False, FStartLine);
     Self.Changed(False);
   end;
end;

function TUserRange.GetFlags: integer;
begin
  Result := 0;
  if lbTop in LineBreaks then Inc(Result);
  if lbBottom in LineBreaks then Inc(Result, 2);
  if Collapsable then Inc(Result, 4);
  if HighlightLines then Inc(Result, 8);
end;

procedure TUserRange.SetFlags(const Value: integer);
var lb: TLineBreakBound;
begin
  lb := [];
  if (Value and 1) <> 0 then Include(lb, lbTop);
  if (Value and 2) <> 0 then Include(lb, lbBottom);
  LineBreaks := lb;
  Collapsable := (Value and 4) <> 0;
  HighlightLines := (Value and 8) <> 0;
  Changed(False);
end;

procedure TUserRange.SetBounds(const AStartPos, AEndPos: integer);
begin
  if (StartPos <> AStartPos) or (EndPos <> AEndPos) then
    begin
      Collection.BeginUpdate;
      try
        StartPos := AStartPos;
        EndPos := AEndPos;
      finally
        Collection.EndUpdate;
      end;
    end;
end;

{ TUserRanges }

function TUserRanges.Add: TUserRange;
begin
  Result := TUserRange(inherited Add);
end;

function TUserRanges.AddRange(AStartPos, AEndPos: integer;
  ACollapsable: Boolean; AStyle: string; ALineBreaks: TLineBreakBound): TUserRange;
begin
  Result := Add;
  Result.StartPos := AStartPos;
  Result.EndPos := AEndPos;
  Result.Collapsable := ACollapsable;
  Result.LineBreaks := ALineBreaks;
  Result.Style := AStyle;
end;

function TUserRanges.ApplyStyles(Canvas: TCanvas; Pos: integer): integer;
var i: integer;
    st: TSyntaxFormat;
begin
  Result := $7FFFFFFF;
  if FActive and (FOwner <> nil) then
  for i := 0 to FFormatted.Count - 1 do
   with TUserRange(FFormatted[i]) do
    begin
      if StartPos <= Pos then
       begin
        if EndPos > Pos then
         begin
           st := FindStyle(FStyle);
           if st <> nil then
            begin
{              if st.BgColor <> clNone then
                FOwner.FStyleLevel := 2;}
              FOwner.ApplyStyle(Canvas, st);
              if EndPos < Result then Result := EndPos;
            end;
         end;
       end else
        if StartPos < Result then Result := StartPos;
    end;
   if Result = $7FFFFFFF then Result := -1;
end;

constructor TUserRanges.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create(TUserRange);
  FOwner := AOwner;
  FActive := True;
  FFormatted := TList.Create;
  FCollapsible := TList.Create;
  FWithSeparators := TList.Create;
end;

destructor TUserRanges.Destroy;
begin
  FFormatted.Free;
  FCollapsible.Free;
  FWithSeparators.Free;
  inherited;
end;

function TUserRanges.FindStyle(const StyleName: string): TSyntaxFormat;
begin
  Result := nil;
  if FOwner = nil then Exit;
  if Assigned(FOwner.UserStyles) then
    Result := TSyntaxFormat(FOwner.UserStyles.Styles.ItemByName(StyleName));
  if (Result = nil) and Assigned(FOwner.SyntObj) then
   Result := TSyntaxFormat(FOwner.SyntObj.Owner.Formats.ItemByName(StyleName));
end;

function TUserRanges.GetCollapsible(Line: integer): TUserRange;
var i: integer;
begin
  if FActive then
  for i := 0 to FCollapsible.Count - 1 do
   with TUserRange(FCollapsible[i]) do
    if (Line = FStartLine) and (FEndLine > FStartLine) then
     begin
       Result := TUserRange(FCollapsible[i]);
       Exit;
     end;
  Result := nil;
end;

function TUserRanges.GetItem(Index: integer): TUserRange;
begin
  Result := TUserRange(inherited Items[Index]);
end;

function TUserRanges.GetLineBreak(Line: integer): TColor;
var i: integer;
begin
  if FActive then
  for i := 0 to FWithSeparators.Count - 1 do
   with TUserRange(FWithSeparators[i]) do
      if (Line = FStartLine) and (lbTop in FLineBreaks) or
         (Line = FEndLine + 1) and (lbBottom in FLineBreaks) then
          begin
            Result := FLineBreakColor;
            Exit;
          end;
  Result := clNone;
end;

procedure TUserRanges.GetLineHighlight(Line: integer; var InvSel: Boolean;
  var bgColor, frColor: TColor);
var i: integer;
    st: TSyntaxFormat;
begin
  if FActive then
  for i := 0 to FFormatted.Count - 1 do
   with TUserRange(FFormatted[i]) do
    if HighlightLines and (StartLine <= Line) and (EndLine >= Line) then
    begin
      st := FindStyle(Style);
      if (st <> nil) and st.Enabled then
       begin
        if st.BgColor <> clNone then bgColor := st.BgColor;
        if (st.FormatType <> ftBackGround) and (st.Font.Color <> clNone) then
          frColor := st.Font.Color;
        InvSel := True;
       end;
    end;
end;

function TUserRanges.GetLineState(Line: integer): integer;
var i, t: integer;
begin
  Result := csOutCollapse;
  if FActive then
  for i := 0 to FCollapsible.Count - 1 do
   with TUserRange(FCollapsible[i]) do
    if FEndLine > FStartLine then
     begin
       if Line = FStartLine then t := csCollapsible else
        if Line = FEndLine then t := csCollapseEnd else
         if (Line > FStartLine) and (Line < FEndLine) then t := csInCollapse
          else t := csOutCollapse;
       if t > Result then Result := t;
     end;
end;

function TUserRanges.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TUserRanges.GetRangeStyles(Pos: integer; List: TStyleEntries): integer;
var i: integer;
    st: TSyntaxFormat;
begin
  Result := -1;
  if FActive then
  for i := FFormatted.Count - 1 downto 0 do
   with TUserRange(FFormatted[i]) do
    if StartPos > Pos then
      begin
        if (Result = -1) or (Result > StartPos) then
          Result := StartPos;
//        Exit    // fix v2.02
      end else
     if (Pos >= StartPos) and (Pos < EndPos) then
      begin
       st := FindStyle(FStyle);
       if st <> nil then
        begin
         List.Add(st, StartPos, EndPos);
         if (EndPos < Result) or (Result = -1) then
           Result := EndPos;
        end;
      end;
end;

function TUserRanges.RangeAtPos(Pos: integer): TUserRange;
var i, m: integer;
begin
  m := -1;
  Result := nil;
  if FActive then
  for i := 0 to Count - 1 do
   with Items[i] do
    if (StartPos <= Pos) and (EndPos > Pos) and (StartPos > m) then
     begin
       m := StartPos;
       Result := Items[i];
     end;
end;

function TUserRanges.RangeAtSel(SelStart, SelLength: integer): TUserRange;
var i: integer;
begin
  if FActive then
  for i := 0 to Count - 1 do
   with Items[i] do
    if (StartPos = SelStart) and (EndPos = SelStart + SelLength) then
     begin
       Result := Items[i];
       Exit;
     end;
  Result := nil;
end;

procedure TUserRanges.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if (Count > 0) and (FOwner <> nil) then
    FOwner.FormatChanged;
end;

procedure TUserRanges.Update(Item: TCollectionItem);
begin
  inherited;
  if (FOwner <> nil) and (UpdateCount = 0) then //(csDesigning in FOwner.ComponentState) then
     FOwner.Invalidate;
end;

procedure TUserRanges.LoadFromFile(const FileName: string);
const Section: string = 'USER_RANGES';
var ini: TIniFile;
    st: TStringList;
    i: integer;
begin
  ini := TIniFile.Create(FileName);
  st := TStringList.Create;
  BeginUpdate;
  try
    ini.ReadSection(Section, st);
    Clear;
    for i := 0 to st.Count - 1 do
      Add.AsString := ini.ReadString(Section, st[i], '');
  finally
    EndUpdate;
    ini.Free;
    st.Free;
  end;
end;

procedure TUserRanges.SaveToFile(const FileName: string);
const Section: string = 'USER_RANGES';
var ini: TIniFile;
    i: integer;
begin
  ini := TIniFile.Create(FileName);
  try
    ini.EraseSection(Section);
    for i := 0 to Count - 1 do
      ini.WriteString(Section, IntToStr(i), Items[i].AsString);
  finally
    ini.Free;
  end;
end;

{ TBookmark }

procedure TBookmark.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TBookmark then
   with Source as TBookmark do
    begin
     Self.FBmIndex := FBmIndex;
     Self.FPosition := FPosition;
     Self.FModified := FModified;
     Self.FAllowDelete := FAllowDelete;
    end;
end;

procedure TBookmark.SetPosition(const Value: integer);
begin
  if FPosition <> Value then
   begin
    FPosition := Value;
    FModified := True;
   end;
  with TBookmarks(Collection) do
   if FOwner <> nil then
    FLine := FOwner.StrPosToCaretPos(FPosition).Y;
end;

{ TBookmarks }

function TBookmarks.Add: TBookmark;
begin
  Result := TBookmark(inherited Add);
end;

constructor TBookmarks.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create(TBookmark);
  FOwner := AOwner;
end;

function TBookmarks.GetItem(Index: integer): TBookmark;
begin
  Result := TBookmark(inherited Items[Index])
end;

function TBookmarks.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TMarker }

procedure TMarker.CopyFrom(Other: TMarker);
begin
  FScrollPos := Other.FScrollPos;
  FCaretPos := Other.FCaretPos;
  FPos := Other.FPos;
end;

function TMarker.GetAsString: string;
begin
  Result := Format('%d;%d;%d;%d;%d', [FPos, FCaretPos.X, FCaretPos.Y, FScrollPos.X, FScrollPos.Y]);
end;

procedure TMarker.SetAsString(const Value: string);
var st: TzStringList;
begin
  st := TzStringList.Create;
  try
    st.Delimiter := ';';
    st.DelimitedText := Value;
    FPos := StrToInt(st[0]);
    FCaretPos.X := StrToInt(st[1]);
    FCaretPos.Y := StrToInt(st[2]);
    FScrollPos.X := StrToInt(st[3]);
    FScrollPos.Y := StrToInt(st[4]);
  finally
    st.Free;
  end;
end;

{ TSyntAnimation }

procedure TSyntAnimation.Animate(Pt: TPoint; AnimType: TAnimationType);
begin
  if not FEnabled or (AnimType = atNone) then Exit;
  Reset;
  FTimer.Enabled := True;
  FPoint := Pt;
  FSize := FRadius;
  FHaveDrawn := False;
  FAnimType := AnimType;
end;

procedure TSyntAnimation.Assign(Source: TPersistent);
var src: TSyntAnimation;
begin
 if Source is TSyntAnimation then
  begin
    src := Source as TSyntAnimation;
    MarkerAnim := src.MarkerAnim;
    BookmarkAnim := src.BookmarkAnim;
    Enabled := src.Enabled;
    Interval := src.Interval;
    Radius := src.Radius;
    Step := src.Step;
  end;
end;

constructor TSyntAnimation.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FTimer := TTimer.Create(AOwner);
  FTimer.Interval := 10;
  FTimer.OnTimer := DoTimer;
  FOwner := AOwner;
  FEnabled := True;
  FRadius := 100;
  FStep := 10;
  FMarkerAnim := atCircle;
  FBookmarkAnim := atRect;
end;

destructor TSyntAnimation.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TSyntAnimation.DoTimer(Sender: TObject);
begin
  if FHaveDrawn then DrawAnim;
  Dec(FSize, FStep);
  if FSize <= 0 then FTimer.Enabled := False
   else DrawAnim;
  FHaveDrawn := FTimer.Enabled;
end;

procedure TSyntAnimation.DrawAnim;
var R: TRect;
begin
  with FOwner.Canvas do
   begin
    Pen.Mode := pmNotXor;
    Brush.Style := bsClear;
    Pen.Color := clBlack;
    R := Rect(FPoint.X - FSize, FPoint.Y - FSize, FPoint.X + FSize, FPoint.Y + FSize);
    case FAnimType of
     atCircle: Ellipse(R);
     atRect:   Rectangle(R);
     atRoundRect: RoundRect(R.Left, R.Top, R.Right, R.Bottom, FSize div 2, FSize div 2);
     atEllipse: Ellipse(R.Left, R.Top + FSize div 2, R.Right, R.Bottom - FSize div 2);
     atCustom: if Assigned(FOwner.OnAnimate) then FOwner.OnAnimate(FOwner, FPoint, FSize);
    end;
    Brush.Style := bsSolid;
    Pen.Mode := pmCopy;
   end;
end;

function TSyntAnimation.GetInterval: integer;
begin
  Result := FTimer.Interval;
end;

procedure TSyntAnimation.Reset;
begin
  if FHaveDrawn then
   begin
     FTimer.Enabled := False;
     DrawAnim;
   end;
end;

procedure TSyntAnimation.SetInterval(const Value: integer);
begin
  FTimer.Interval := Value;
end;

procedure TSyntAnimation.SetStep(const Value: integer);
begin
  if Value > 0 then
    FStep := Value;
end;

{ TDefaultStyles }

procedure TDefaultStyles.Assign(Source: TPersistent);
begin
  if Source is TDefaultStyles then
   with Source as TDefaultStyles do
    begin
     Self.FSearchMark.Assign(FSearchMark);
     Self.FSelectioMark.Assign(FSelectioMark);
     Self.FCurrentLine.Assign(FCurrentLine);
     Self.FCollapseMark.Assign(FCollapseMark);
    end;
end;

constructor TDefaultStyles.Create(AOwner: TCustomSyntaxMemo);
var i: integer;
begin
  inherited Create;

  FOwner := AOwner;

  FCurrentLine := TSyntaxFormat.Create(nil);
  FCurrentLine.FormatType := ftBackGround;
  FCurrentLine.BgColor := clNone;
  FCurrentLine.OnChange := StyleChanged;
  FCurrentLine.Enabled := False;

  FSearchMark := TSyntaxFormat.Create(nil);
  FSearchMark.FormatType := ftColor;
  FSearchMark.Font.Color := clWhite;
  FSearchMark.BgColor := clBlack;
  FSearchMark.OnChange := StyleChanged;

  FSelectioMark := TSyntaxFormat.Create(nil);
  FSelectioMark.FormatType := ftColor;
  FSelectioMark.Font.Color := clHighlightText;
  FSelectioMark.BgColor := clHighlight;
  FSelectioMark.OnChange := StyleChanged;

  FCollapseMark := TSyntaxFormat.Create(nil);
  FCollapseMark.FormatType := ftColor;
  FCollapseMark.Font.Color := clSilver;
//  FCollapseMark.BgColor := clWhite;
  for i := 0 to 3 do
   begin
     FCollapseMark.BorderColors[i] := clSilver;
     FCollapseMark.BorderTypes[i] := blSolid;
   end;
  FCollapseMark.OnChange := StyleChanged;
end;

destructor TDefaultStyles.Destroy;
begin
  FSearchMark.Free;
  FSelectioMark.Free;
  FCurrentLine.Free;
  FCollapseMark.Free;
  inherited;
end;

procedure TDefaultStyles.SetCollapseMark(const Value: TSyntaxFormat);
begin
  FCollapseMark.Assign(Value);
  StyleChanged(FCollapseMark);
end;

procedure TDefaultStyles.SetCurrentLine(const Value: TSyntaxFormat);
begin
  FCurrentLine.Assign(Value);
  StyleChanged(FCurrentLine);
end;

procedure TDefaultStyles.SetSearchMark(const Value: TSyntaxFormat);
begin
  FSearchMark.Assign(Value);
  StyleChanged(FSearchMark);
end;

procedure TDefaultStyles.SetSelectioMark(const Value: TSyntaxFormat);
begin
  FSelectioMark.Assign(Value);
  StyleChanged(FSelectioMark);
end;

procedure TDefaultStyles.StyleChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;


{ TCustomSyntaxMemo }

constructor TCustomSyntaxMemo.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque{$IFDEF EC_VCL7_UP}, csNeedsBorderPaint{$ENDIF}];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  ParentColor := False;
  Color := clWindow;
  Ctl3D := True;
  FBorderStyle := bsSingle;
  Cursor := crIBeam;
  DragCursor := crDrag;
  FMultiLine := True;
  FLinesDesc := TObjectList.Create;
  FDefaultLineDesc := TLineInfo.Create;
  FLines := TSyntMemoStrings.Create;
  FLines.OnChange := TextChanged;
  FGutter := TGutter.Create(Self);
  FNonPrinted := TNonPrinted.Create(Self);
  FLineNumbers := TLineNumbers.Create(Self);
  FTabList := TTabList.Create(Self);
  FStaplePen := TPen.Create;
  FStaplePen.Color := clGray;
  FAnimation := TSyntAnimation.Create(Self);
  FTabList.Add(4);
  Font.Name := 'Courier New';
  Font.Size := 10;

  //AT
  FNonPrintedSpaces := True;
  FNonPrintedEol := True;
  FNonPrintedEolDetails := False;

  //AT
  FTimerScroll:= TTimer.Create(Self);
  FTimerScroll.OnTimer:= DoTimerScroll;
  FTimerScroll.Enabled:= false;
  FTimerScroll.Interval:= 200;

  FScrollTimer := TTimer.Create(nil);
  FScrollTimer.OnTimer := DoDragScroll;
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 100;
  FSearchMarks := TRangeList.Create;
  FBookmarks := TBookmarks.Create(Self);
  FMarkers := TObjectList.Create;
  FCollapsed:= TCollapsedRanges.Create;
  FUserRanges := TUserRanges.Create(Self);
  FDefaultStyles := TDefaultStyles.Create(Self);
  FLineStateDisplay := TLineStateDisplay.Create(Self);
  FBackGround := TecBackGround.Create(BgChanged);
  FKeyQueue := TKeyQueue.Create;

  FHorzRuler := TecHorzRuler.Create(Self);
  FHorzRuler.OnChange := BgChanged;

  FTextMargin := 2;
  FTabMode := tmSpaces;
  FMargin := Rect(0, 0, 0, 0);
  FScrollDelit := Point(1, 1);
  ClearBookmarks;
//  AutoSelect := False;
//  AutoSize := False;
  Brush.Style := bsClear;
  FScrollBars := ssBoth;
  FSyntClients := TList.Create;
  FLineInfos := TList.Create;
  FSyncEditing := TSyntSyncEdit.Create(Self);

  FHintTimer := TTimer.Create(Self);
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := HintTimer;
  FHint := TSyntHintWindow.Create(Self);
  FHint.Color := $00C6FFFF;
  FHintProps := THintProps.Create(Self);
  FHint.FreeNotification(Self);
  FHintBak := FHint;

  FBlockIndent := 2;
//  FGroupIndex := -1;
  FFirstKey := 0;
  FCollpseLevel := -1;

  FOptions := [soOverwriteBlocks, soEnableBlockSel, soAutoIndentMode, soBackUnindent,
               soGroupUndo, soHideSelection, soHideDynamic, soDragText, soScrollLastLine];
  FOptionsEx := [soSmartPaste, soCorrectNonPrinted, soRightClickMoveCaret];
  FStapleOffset := 2;
  FStaplesEnabled := true;
  FIncSearchIgnoreCase := True;
  FCollapseBreakColor := clSilver;
  FCollapseStyle := csRegionName;
  FDefaultPopup := True;
  FDefPopup := nil;
  FLineSpacing := 1;
  ResetLineHeights;
  FKeepedXPos := -1;
  FMarkedSelStart := -1;
  FSelectedEmbObj := -1;
  FZoom := 100;

  FSelAttributes := TecTextAttributes.Create(Self);
  FTextMargins := TecTextMargins.Create(Self);
  FTextMargins.Add.Pen.Color := clSilver;

  FCaret := TecCaret.Create(Self);
  FMonoFontMode := mfVariable;

  FMaximalLinesWidth := 0; //ZD
//  UpdateMargin;
end;

procedure TCustomSyntaxMemo.CreateWnd;
begin
  inherited;
  UpdateMargin;
  ResetLineHeights;
  if FFlatScrollBars then
    InitializeFlatSB(Handle); //!!
  AdjustScrollBar;
  FValidOle := RegisterDragDrop(Handle, Self) = S_OK;
end;

procedure TCustomSyntaxMemo.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
//  Transparency: array[Boolean] of DWORD = (0, WS_EX_TRANSPARENT);
begin
  inherited;
  with Params do
  begin
    Caption := nil;
    Style := Style or BorderStyles[BorderStyle] or ScrollBar[FScrollBars];
    if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
//      if FTransparent then
//        Style := Style and not WS_CLIPCHILDREN;
//      AddBiDiModeExStyle(ExStyle);
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;// or Transparency[FTransparent];
    end;
  end;
end;

procedure TCustomSyntaxMemo.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

destructor TCustomSyntaxMemo.Destroy;
begin
  FreeAndNil(FBackGround);
  FreeAndNil(FSyntRanges);
  FreeAndNil(FScrollTimer);
  FreeAndNil(FCollapsed);
  FreeAndNil(FLineInfos);
  FreeAndNil(FTabList);
  FreeAndNil(FNonPrinted);
  FreeAndNil(FLineNumbers);
  FreeAndNil(FGutter);
  FreeAndNil(FHintTimer);
  FreeAndNil(FHint);
  FreeAndNil(FHintProps);
  FreeAndNil(FSearchMarks);
  FreeAndNil(FBookmarks);
  FreeAndNil(FMarkers);
  FreeAndNil(FUserRanges);
  FreeAndNil(FStaplePen);
  FreeAndNil(FAnimation);
  FreeAndNil(FDefaultStyles);
  FreeAndNil(FLinesDesc);
  FreeAndNil(FDefaultLineDesc);
  FreeAndNil(FLineStateDisplay);
  FreeAndNil(FSyncEditing);
  FreeAndNil(FKeyQueue);
  FreeAndNil(FDefPopup);
  FreeAndNil(FSelAttributes);
  FreeAndNil(FHorzRuler);
  FreeAndNil(FTextMargins);
  FreeAndNil(FCaret);
  FreeAndNil(FSyntClients);

  inherited;
  FreeAndNil(FLines);
end;

procedure TCustomSyntaxMemo.DestroyWnd;
begin
  RevokeDragDrop(Handle);
  if FFlatScrollBars then
    UninitializeFlatSB(Handle); //!!
  inherited;
end;

type
  TComponentAccess = class(TComponent);

procedure TCustomSyntaxMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
var i: integer;
begin
  inherited;
  if (Operation <> opRemove) or (AComponent = nil) or
     (AComponent is THintAction) or (FGutter = nil) or
     (FHintProps = nil) then Exit;

  Assert(Assigned(FSyntClients));
  FSyntClients.Remove(AComponent);

  if AComponent is THintWindow then
  begin
       if AComponent = FHint then FHint := nil;
       Exit;
  end;

  if AComponent is TSyntAnalyzer then
  begin
       if (AComponent = GetSyntRanges) and Assigned(FSyntRanges) then begin
            FSyntRanges.Free;
            FSyntRanges := nil;
            if not (csDestroying in ComponentState) then
              Invalidate;
       end;
       Exit;
  end;

  if AComponent is TCustomImageList then
    begin
     for i := 0 to Gutter.Objects.Count - 1 do
       if Gutter.Objects[i].ImageList = AComponent then
         Gutter.Objects[i].ImageList := nil;

     for i := 0 to FBookmarks.Count - 1 do
       if FBookmarks[i].ImageList = AComponent then
         FBookmarks[i].ImageList := nil;

     if AComponent = FGutter.Images then
       FGutter.FImages := nil;

     if FHintProps.FImages = AComponent then
       FHintProps.Images := nil;
    end;

  if AComponent is TSyntStyles then
    begin
      if AComponent = FUserStyles then FUserStyles := nil;
      if AComponent = FHintProps.FStyles then FHintProps.Styles := nil;
    end;

  if AComponent is TSyntTextSource then
  begin
    if AComponent = FTextSource then
        TextSource := nil;
    Exit;
  end;

  if AComponent is TSyntKeyMapping then
  begin
       if AComponent = FKeyMapping then FKeyMapping := nil;
       Exit;
  end;

  if AComponent is TPopupMenu then
  begin
      if FGutter.PopupMenu = AComponent then
        FGutter.PopupMenu := nil;
      for i := 0 to FGutter.Objects.Count - 1 do
            if Gutter.Objects[i].PopupMenu = AComponent then
                 Gutter.Objects[i].PopupMenu := nil;
  end;
end;

//==============================================================================
//   Style application routines
//==============================================================================
// Extracts styles list at the specified position
// Result - next position of style change
function TCustomSyntaxMemo.GetStyleList(CurPos: integer; List: TStyleEntries; Flags: integer; IncludeLine: Boolean): integer;
var i, res, tmp, Line: integer;
    stl: TSyntaxFormat;
    iObj: IecExternalFormatter;
   procedure CheckRes(NextFmt: integer);
   begin
    if (NextFmt > CurPos) and (NextFmt < res) then res := NextFmt;
   end;
  procedure AddLineStl;
  begin
    if Assigned(stl) then
      begin
        tmp := CaretPosToStrPos(Point(0, Line + 1));
        List.Add(stl, CaretPosToStrPos(Point(0, Line)), tmp);
        CheckRes(tmp);
      end;
  end;
begin
//  Result := CurPos + 1;
//  Exit;

  res := Lines.TextLength;

  if Assigned(SyntObj) then
   begin
    // Text range styles
    if (slTextRange and Flags) <> 0 then
      CheckRes(SyntObj.GetRangeStyles(CurPos, List, ((slDynoRange and Flags) <> 0)));
    // Token style
    if (slToken and Flags) <> 0 then
      CheckRes(SyntObj.GetTokenStyle(CurPos, List));
   end;
  // User ranges styles
  if (slUserRange and Flags) <> 0 then
    CheckRes(FUserRanges.GetRangeStyles(CurPos, List));

  // Custom styles
  if Assigned(FOnGetStyleEntry) and ((slEventHandle and Flags) <> 0) then
   begin
    i := -1;
    FOnGetStyleEntry(Self, CurPos, List, i);
    CheckRes(i);
   end;
  // Search mark style
  if (slSearchMarks and Flags) <> 0 then
   for i := 0 to FSearchMarks.Count - 1 do
    with FSearchMarks[i] do
     if StartPos > CurPos then CheckRes(StartPos) else
     if (StartPos <= CurPos) and (EndPos > CurPos) then
       begin
         if Assigned(SyntObj) and Assigned(SyntObj.Owner.SearchMatch) then
           stl := SyntObj.Owner.SearchMatch else stl := FDefaultStyles.FSearchMark;
         List.Add(stl, StartPos, EndPos, True);
         CheckRes(EndPos);
         Break;
       end;
  // Execute editor plug-in
  Assert(Assigned(FSyntClients));
  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecExternalFormatter, iObj) then
     CheckRes(iObj.GetStyleList(CurPos, List));
  if Assigned(FTextSource) and Supports(FTextSource, IecExternalFormatter, iObj) then
    CheckRes(iObj.GetStyleList(CurPos, List));

  CheckRes(FSyncEditing.GetStyleList(CurPos, List));

  if IncludeLine then
    begin
      Line := StrPosToCaretPos(CurPos).Y;
      stl := nil;
      if Line = FCaretPos.Y then
        begin
          if Assigned(SyntObj) and Assigned(SyntObj.Owner.CurrentLine) then
            stl := SyntObj.Owner.CurrentLine
          else
            stl := FDefaultStyles.CurrentLine;
          AddLineStl;
        end;
      stl := nil;
      if Assigned(FOnGetLineStyle) then
        begin
          FOnGetLineStyle(Self, Line, stl);
          AddLineStl;
        end;
    end;

  Result := res;
end;

procedure TCustomSyntaxMemo.ApplyStyles(Canvas: TCanvas; List: TStyleEntries;
  OnlyDyno: Boolean);
var i: integer;
begin
  for i := 0 to List.Count - 1 do
   with List[i] do
    if OnlyDyno xor not IsDynoStyle then
      ApplyStyle(Canvas, Style);
  if FHiddenStyle and (Canvas = Self.Canvas) and not OnlyDyno and FNonPrinted.Visible then
   begin
    FHiddenStyle := False;
    if FNonPrinted.UseFont then Canvas.Font := FNonPrinted.Font
     else if FNonPrinted.Color <> clNone then
      Canvas.Font.Color := FNonPrinted.Color;
   end;
end;

procedure TCustomSyntaxMemo.ApplyDefStyle(Canvas: TCanvas; Pos: integer);
var Synt: TSyntAnalyzer;
begin
  // Memo default
  Canvas.Brush.Color := Color;
  Canvas.Font := Font;
  FHiddenStyle := False;
  FVertAlignment := vaCenter;
  // Lexer's default style
  if Assigned(SyntObj) then
    begin
      Synt := SyntObj.AnalyzerAtPos(Pos);
      if Assigned(Synt) and Assigned(Synt.DefStyle) then
        ApplyStyle(Canvas, Synt.DefStyle);
    end;
end;

procedure TCustomSyntaxMemo.ApplyStyle(Canvas: TCanvas; Style: TSyntaxFormat);
begin
  if not Style.Enabled then Exit;

  if ffHidden in Style.FormatFlags then
    FHiddenStyle := Style.Hidden;

  if (Style.FormatType in [ftCustomFont, ftFontAttr]) and
     (ffVertAlign in Style.FormatFlags) then
    FVertAlignment := Style.VertAlignment;

  Style.ApplyTo(Canvas, not FMonoFont);
end;

function TCustomSyntaxMemo.SetCanvasAtPos(Canvas: TCanvas;
  CurPos: integer): integer;
var List: TStyleEntries;
    Flags: integer;
begin
  List := TStyleEntries.Create;
  try
    if Canvas = Self.Canvas then Flags := slAllStyles
      else Flags := slPrintable;
    Result := GetStyleList(CurPos, List, Flags, True);
    if Result = -1 then Result := TextLength;
    ApplyDefStyle(Canvas, CurPos);
    ApplyStyles(Canvas, List, False);
    ApplyStyles(Canvas, List, True);
  finally
    List.Free;
  end;
end;
//==============================================================================
//   Drawing routines
//==============================================================================
{$define OldExt}
{$ifdef OldExt}
function TCustomSyntaxMemo.DefTextExt: TSize;
var DC: HDC;
    SaveFont: HFont;
    Metrics: TTextMetric;
begin
  if FMonoFont then Result := FDefExt else
    begin
      DC := GetDC(0);
      try
        if Assigned(SyntObj) and (SyntObj.Owner.DefStyle <> nil) then
         SaveFont := SelectObject(DC, SyntObj.Owner.DefStyle.Font.Handle)
        else
         SaveFont := SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, SaveFont);
      finally
        ReleaseDC(0, DC);
      end;
      Result.cx := Metrics.tmAveCharWidth;
      Result.cy := Metrics.tmHeight;
    end;

  if FZoom <> 100 then
    begin
      Result.cx := Trunc(Result.cx * FZoom / 100);
      Result.cy := Trunc(Result.cy * FZoom / 100);
    end;
  if Result.cx = 0 then
    raise Exception.Create('Internal error: DefTextExt.cx = 0')
end;
{$else}
function TCustomSyntaxMemo.DefTextExt: TSize;
var DC: HDC;
    SaveFont: HFont;
    F: TFont;
    Metrics: TTextMetric;
    tmp: integer;
begin
    if FMonoFont then
    begin
      Result := FDefExt;
    end
    else
    begin
        DC := GetDC(0);
        try
            if Assigned(SyntObj) and (SyntObj.Owner.DefStyle <> nil) then
                F := SyntObj.Owner.DefStyle.Font
            else
                F := Font;
            {if FZoom <> 100 then} begin//AT
                tmp := F.Size;
                F.PixelsPerInch := MulDiv(PixelPerInch.X, FZoom, 100);
                F.Size := tmp;
            end;
            SaveFont := SelectObject(DC, F.Handle);
            GetTextMetrics(DC, Metrics);
            SelectObject(DC, SaveFont);
        finally
            ReleaseDC(0, DC);
        end;
        Result.cx := Metrics.tmAveCharWidth;
        Result.cy := Metrics.tmHeight;
    end;
    if Result.cx = 0 then
        raise Exception.Create('Internal error: DefTextExt.cx = 0')
end;
{$endif}

function TCustomSyntaxMemo.CurrentFontHeight(Canvas: TCanvas): integer;
begin
  if FMonoFont and (Canvas = Self.Canvas) or not HandleAllocated then
    Result := FDefExt.cy
  else
    Result := ecTextExtent(Canvas, 'W').cy;
end;

procedure TCustomSyntaxMemo.WMERASEBKGND(var Message: TWMErasebkgnd);
begin
   Message.Result := 1;
end;

procedure TCustomSyntaxMemo.DrawMarker(Canvas: TCanvas; x, y: integer);
var c: TColor;
const MSize = 4;
begin
  with Canvas do
   begin
    c := Brush.Color;
    Brush.Color := clRed;
    Pen.Color := clBlack;
    Polygon([Point(x - MSize, y), Point(x + MSize, y), Point(x, y - MSize)]);
    Brush.Color := c;
   end;
end;

procedure TCustomSyntaxMemo.DrawStaple(Canvas: TCanvas; X, Y1, Y2: integer; AFirstLine,
  ALastLine: Boolean; Level: integer; Staple: TBlockStaple);
var dx1, dx, dy: integer;
    d: double;
begin
//  if (FStapleOffset < 0) and (AFirstLine or ALastLine) then Exit;
  if Staple.Rule.UseCustomPen then
    Canvas.Pen := Staple.Rule.Pen
  else
    Canvas.Pen := FStaplePen;

  if Assigned(FOnBeforeDrawStaple) then
    FOnBeforeDrawStaple(Self, Canvas, Staple, Level);

  dx := 10;
  dy := 1;
  dx1 := FStapleOffset;
  if (Canvas <> Self.Canvas) or (FZoom <> 100) then
    begin
      d := Canvas.Font.PixelsPerInch / PixelPerInch.X;
      dx := Round(dx * d);
      dy := Round(dy * d);
      dx1 := Round(dx1 * d);
      Canvas.Pen.Width := Round(Canvas.Pen.Width * d);
    end;

  X := X - dx1;
  Canvas.MoveTo(X, Y1);
  Canvas.LineTo(X, Y2);
  Y2 := Y2 - dy;
  if AFirstLine then
   begin
    Canvas.MoveTo(x, y1);
    Canvas.LineTo(x + dx, y1);
   end;
  if ALastLine then
   begin
    Canvas.MoveTo(x, Y2);
    Canvas.LineTo(x + dx, Y2);
   end;
end;

function TCustomSyntaxMemo.GetColRangeTextAtPos(Line: integer; var IconPos: integer): string;
var p: TCollapsedRange;
    Ln: integer;
begin
  Result := '';
  if CollapseStyle = csLineSeparator then Exit;
  p := FCollapsed.GetCollapsed(Line);
  if p = nil then Exit;

  if CollapseStyle = csEndEllipsis then
    begin
      Result := '...';
      IconPos := p.FPos;
      Ln := StrPosToCaretPos(IconPos).Y;
      IconPos := CaretPosToStrPos(Point(0, Ln)) + Lines.LineLength(Ln);
      Exit;
    end;
  Result := p.FColText;

  if Result = '' then
   if CollapseStyle = csNameWhenDefined then Exit
    else Result := '...';

  IconPos := p.FPos;
end;

//==============================================================================
//==============================================================================
//   Line processing
//==============================================================================
//==============================================================================

// Block staples for the Line
function TCustomSyntaxMemo.GetAbout: string;
begin
  Result := 'Version ' + ecCmdConst.ecSyntaxEditorVersion;
end;

function TCustomSyntaxMemo.GetBlockStaples(Line: integer): TList;
begin
  if not Assigned(SyntObj) then Result := nil else
    begin
      Result := TList.Create;
      SyntObj.GetStaples(Line, Result);
      if Result.Count = 0 then
        FreeAndNil(Result);
    end;
end;

// Markers in the Line
function TCustomSyntaxMemo.GetMarkersFroLine(Line, StartPos, EndPos: integer): TList;
var i: integer;
begin
  Result := nil;
  for i := 0 to FMarkers.Count - 1 do
   with TMarker(FMarkers[i]) do
    if (soFloatMarkers in FOptions) and (Position >= StartPos) and (Position <= EndPos) or
       not (soFloatMarkers in FOptions) and (CaretPos.Y = Line) then
       begin
         if Result = nil then Result := TList.Create;
         Result.Add(FMarkers[i]);
       end;
end;

type

  TLineHighlightAttr = class
    bg, fr: TColor;
    InvSel: Boolean;
  public
    constructor Create;
    procedure Apply(Canvas: TCanvas; InSel: Boolean);
  end;


constructor TLineHighlightAttr.Create;
begin
  inherited Create;
  bg := clNone;
  fr := clNone;
  InvSel := False;
end;

procedure TLineHighlightAttr.Apply(Canvas: TCanvas; InSel: Boolean);
begin
  if not InSel then
   begin
     if bg <> clNone then Canvas.Brush.Color := bg;
     if fr <> clNone then  Canvas.Font.Color := fr;
   end else
   if InvSel then
    begin
     if fr <> clNone then Canvas.Brush.Color := fr;
     if bg <> clNone then  Canvas.Font.Color := bg;
    end;
end;

{$IFDEF EC_UNICODE}
  {$DEFINE EC_RTL}
{$ENDIF}

procedure Change_Case(var S: ecString; CurPos: integer; StlList: TStyleEntries);
var i, j: integer;
    c: ecChar;
begin
  for i := 0 to StlList.Count - 1 do
   case StlList[i].Style.ChangeCase of
     ccUpper: for j := 1 to Length(S) do
               S[j] := ecUpCase(S[j]);
     ccLower: for j := 1 to Length(S) do
               S[j] := ecLoCase(S[j]);
     ccToggle:for j := 1 to Length(S) do
                begin
                  c := ecUpCase(S[j]);
                  if c = S[j] then S[j] := ecLoCase(c)
                    else S[j] := c;
                end;
     ccTitle: if (StlList[i].StartPos = CurPos) and (Length(S) > 0) then
                S[1] := ecUpCase(S[1]);
   end;
end;

function TCustomSyntaxMemo.ProcessLine(Canvas: TCanvas; LineInfo: TLineInfo;
                                       Line: integer; ProcType: TProcessLineOper;
                                       R: TRect; Param: TPoint;
                                       FromPos, ToPos: integer): TPoint;
var i, x, y, k, CurPos, BasePos, Sl, dK,
    sLeft, sRight, LineNum, LogK, FStapleLevel, Pixel_Per_Inch, WordBreakPos: integer;
    BlockLine, ShowSel, ShowDyn, LineEnd, IsPrint, AWordWrap, TransparentBack,
    BackGnd, IsAnsi, IsFixedHgt: Boolean;
    sv_bg_color: TColor;

    StlList: TStyleEntries;
    FCurStaples, FCurMarkers, ImgList: TList;
    HA1, HA2: TLineHighlightAttr;
    FLineStyle: TSyntaxFormat;
    cS: ecString;
    // Re-design
    ElemLen: integer;    // Element length
    ElemSize: TSize;     // Element Size
    ElemStr: ecString;   // Element string (with expanded tabs)
    LnHeight: integer;   // Current line height
    UnchangedWB: Boolean;// Indicates that word-breaks are unchanged
    {$IFDEF EC_RTL} // RTL support code
    RTLregions: array of TPoint;
    InRtl: Boolean;
    RTLRegIdx, RTLStartIdx, RTLStart, RTLWidth: integer;
    RTLStr: WideString;
    ResCP: GCP_RESULTSW;
    {$ENDIF}
    ClipR: TRect;


  procedure FreeTempObj;
  begin
    FreeAndNil(FCurStaples);
    FreeAndNil(FCurMarkers);
    FreeAndNil(ImgList);
    FreeAndNil(StlList);
    FreeAndNil(HA1);
    FreeAndNil(HA2);
  end;

  function GetElemStr(CurPos, LogK: integer; eLen: integer; CheckAnsi: Boolean = False): ecString;
  var i: integer;
      AnsiMode: Byte;
  begin
    Result := Lines.SubString(CurPos + 1, eLen);
    eLen := Length(Result);
    if eLen = 0 then Exit;

    if CheckAnsi and (soExtractAnsiParts in FOptionsEx) then
      begin
        AnsiMode := 0;
        for i := 1 to eLen do
          begin
            IsAnsi := (Ord(Result[i]) <= 127);
            if IsAnsi and (AnsiMode = 2) or
               not IsAnsi and (AnsiMode = 1) then
              begin
                eLen := i - 1;
                Result := Copy(Result, 1, eLen);
                Break;
              end;
            if IsAnsi then AnsiMode := 1
                      else AnsiMode := 2;
          end;
        IsAnsi := AnsiMode < 2;
        ElemLen := eLen;
      end;

    ExpandTabs(Result, CurPos, LogK);

    if soCorrectNonPrinted in FOptionsEx then
      for i := Length(Result) downto 1 do
        if (Ord(Result[i]) < 32) and (Ord(Result[i]) >= 28) then
          Delete(Result, i, 1);

    ChangeStrCase(Result, TChangeCase(CharCase));
    Change_Case(Result, CurPos, StlList);
  end;

  function GetChar(K: integer): ecChar;
  begin
    Result := Lines.Chars[BasePos + K];
  end;

  function TextSize(const S: ecString): TSize;
  begin
    if FMonoFont and not IsPrint or not HandleAllocated then
      begin
        if S = '' then
          begin
            Result.cx := 0;
            Result.cy := 0;
          end else
          begin
            Result := DefTextExt;
            Result.cx := Result.cx * Length(S);
          end;
      end else
        Result := ecTextExtent(Canvas, S);
  end;

  procedure GetElement;
  begin
    ElemStr := GetElemStr(CurPos, LogK, ElemLen, True);
    if (not IsAnsi or not (soExtractAnsiParts in FOptionsEx)) and (Charset <> DEFAULT_CHARSET) and (Canvas.Font.Charset <> Charset) then
      Canvas.Font.Charset := Charset;
    ElemSize := TextSize(ElemStr);
  end;

  procedure SetElemLength(eLen: integer);
  begin
    if eLen > 0 then
      ElemLen := eLen
    else
      ElemLen := 1;
  end;

  function BaseLine: integer;
  begin
    Result := y + LnHeight;
  end;

  procedure FillRectEx(Rect: TRect);
  begin
    if IsPrint and (Canvas.Brush.Color = clWhite) then Exit;
    if sv_bg_color = Canvas.Brush.Color then
     begin
        Rect.Left := Rect.Left + 6;
        if Rect.Left > Rect.Right then
          begin
            sv_bg_color := clNone;
            Exit;
          end;
     end;
    Canvas.FillRect(Rect);
    sv_bg_color := clNone;
  end;

  function IsTransparent: Boolean;
  begin
    Result := TransparentBack and (IsPrint or (Canvas.Brush.Color = Color));
    if Result then
      Canvas.Brush.Style := bsClear;
  end;

  procedure DrawTagStr(const S: ecString; xp: integer);
  var ty, hgt: integer;
      Rt, Rtmp: TRect;
      {$IFDEF EC_RTL}
      rgn_sv: HRGN;
      {$ENDIF}
  begin
    IsTransparent;
    {$IFDEF EC_RTL}
    if InRtl then
      xp := RTLStart + RTLWidth - (xp - RTLStart) - ElemSize.cx;
    {$ENDIF}
    Rt := Rect(xp, Y, xp + ElemSize.cx, BaseLine);
//    if RectVisible(Canvas.Handle, Rt) then
    if IntersectRect(Rtmp, ClipR, Rt) then
    begin
{      if FMonoFont and (Canvas = Self.Canvas) then
       begin
         ecTextOut(Canvas, Rt.Left, Rt.Top, S);
       end else
       begin}
         hgt := LnHeight;
         Dec(hgt, FLineSpacing);
         if LineNum = LineInfo.LineCount - 1 then
           Dec(hgt, FLineSpacingAfter);
         ty := Y;
         if LineNum = 0 then
          begin
           Inc(ty, FLineSpacingBefore);
           Dec(hgt, FLineSpacingBefore);
          end;
         case FVertAlignment of
//           vaTop:     ty := Y;
           vaBottom:  ty := ty + hgt - ElemSize.cy;
           vaCenter:  ty := ty + (hgt - ElemSize.cy) div 2;
         end;
         if Canvas.Brush.Style <> bsClear then
           begin
             Canvas.FillRect(Rt);
             Canvas.Brush.Style := bsClear;
           end;
{         if Canvas.Brush.Style <> bsClear then
           if ty + ElemSize.cy - 1 < Rt.Bottom then
            begin
             Rt.Top := ty + ElemSize.cy - 1;
             Canvas.FillRect(Rt);
            end;
           if ty > Y then
            begin
             Rt.Top := Y;
             Rt.Bottom := ty;
             Canvas.FillRect(Rt);
            end;}
         {$IFDEF EC_RTL}
         if InRtl then
          begin
           rgn_sv := CreateRectRgn(0,0,0,0);
           GetClipRgn(Canvas.Handle, rgn_sv);
           IntersectClipRect(Canvas.Handle, xp, Y, xp + ElemSize.cx, BaseLine);
           ecTextOut(Canvas, RTLStart, ty, RTLStr);
           SelectClipRgn(Canvas.Handle, rgn_sv);
           DeleteObject(rgn_sv);
          end
         else
         {$ENDIF}
         ecTextOut(Canvas, Rt.Left, ty, S);
//       end;
    end;
  end;

  procedure DrawElem;
  var w: integer;
      S: ecString;
  begin
    S := ElemStr;

    w := 0;
    if (Length(S) > 0) and (S[1] = ' ') and (sv_bg_color <> clNone) and
       (sv_bg_color = Canvas.Brush.Color) {$IFDEF EC_RTL}and not InRtl{$ENDIF} then
     begin
       w := Canvas.TextWidth(' ');
       FillRectEx(Rect(X, Y, X + w, BaseLine));
       ElemSize.cx := ElemSize.cx - w;
       Delete(S, 1, 1);
     end;
    if Length(S) > 0 then
      begin
        // + Uncommented in v 2.32
        if not BackGnd and (S[Length(S)] <> ' ') and not IsPrint and
           ((k + ElemLen >= Sl - 1) or (GetChar(k + ElemLen + 1) = ' '))
           and not TransparentBack then
         begin
           sv_bg_color := Canvas.Brush.Color;
           Canvas.FillRect(Rect(X + ElemSize.cx + w, Y, X + ElemSize.cx + w + 6, BaseLine));
         end else sv_bg_color := clNone;
        // - Uncommented in v 2.32
        DrawTagStr(S, X + w);
      end;
    ElemSize.cx := ElemSize.cx + w;
  end;

  procedure DrawBounds(SPos, EPos: integer; Rt: TRect; LineEnd: Boolean);
  var i, dw, x, ls: integer;
      SmartBound, Valid: Boolean;
      ps, pe, ps_xy, pe_xy: TPoint;
  begin
    for i := 0 to StlList.Count - 1 do
     with TStyleEntry(StlList[i]) do
      if Assigned(Style) and Style.HasBorder then
      begin
        SmartBound := Style.MultiLineBorder;
        // Left border
        if (Style.BorderTypeLeft <> blNone) and (StartPos = SPos) or
           SmartBound and (Rt.Left = R.Left) then
         begin
          Canvas.Pen.Color := Style.BorderColorLeft;
          dw := GetBorderLineWidth(Style.BorderTypeLeft) div 2;
          DrawBorder(Canvas, Style.BorderTypeLeft, Point(Rt.Left + dw, Rt.Top), Point(Rt.Left + dw, Rt.Bottom));
         end;
        // right border
        if (Style.BorderTypeRight <> blNone) and (EndPos = EPos) or
           SmartBound and (Rt.Right = R.Right) then
         begin
          Canvas.Pen.Color := Style.BorderColorRight;
          dw := (GetBorderLineWidth(Style.BorderTypeRight) + 1) div 2;
          DrawBorder(Canvas, Style.BorderTypeRight, Point(Rt.Right - dw, Rt.Top), Point(Rt.Right - dw, Rt.Bottom));
         end;

        if (Style.BorderTypeTop = blNone) and
           (Style.BorderTypeBottom = blNone) or
           (EPos < EndPos) and not LineEnd then Exit;

        ps := StrPosToCaretPos(StartPos);
        pe := StrPosToCaretPos(EndPos);
        ps_xy := CaretToMouse(ps.X, ps.Y);
        pe_xy := CaretToMouse(pe.X, pe.Y);
        SmartBound := SmartBound and (ps_xy.Y < pe_xy.Y);

        // Bottom
        if Style.BorderTypeBottom <> blNone then
         begin
          if (ps_xy.Y = pe_xy.Y) or (Rt.Top = ps_xy.Y) then x := ps_xy.X
           else x := FMargin.Left;
          Valid := True;
          if SmartBound then
           begin
             if pe_xy.Y = Rt.Bottom then
              begin
                x := pe_xy.X;
                if ps_xy.Y + LineHeight(ps) = pe_xy.Y then
                  x := Max(x, ps_xy.X);
              end else
                Valid := pe_xy.Y + LineHeight(pe) = Rt.Bottom;
           end;
          if Valid then
           begin
            Canvas.Pen.Color := Style.BorderColorBottom;
            dw := (GetBorderLineWidth(Style.BorderTypeBottom) + 1) div 2;
            DrawBorder(Canvas, Style.BorderTypeBottom, Point(x, Rt.Bottom - dw), Point(Rt.Right, Rt.Bottom - dw));
           end;
         end;

        // Top
        if Style.BorderTypeTop <> blNone then
         begin
          x := Rt.Right;
          if ps_xy.Y = Rt.Top then ls := ps_xy.X
           else ls := FMargin.Left;
          Valid := True;
          if SmartBound then
           begin
             if ps_xy.Y + LineHeight(ps) = Rt.Top then
                x := Min(x, ps_xy.X)
             else
                Valid := ps_xy.Y = Rt.Top;
           end;
          if Valid then
           begin
            Canvas.Pen.Color := Style.BorderColorTop;
            dw := GetBorderLineWidth(Style.BorderTypeTop) div 2;
            DrawBorder(Canvas, Style.BorderTypeTop, Point(ls, Rt.Top + dw), Point(x, Rt.Top + dw));
           end;
         end;
      end;
  end;

  procedure GetLineHighlight;
  var i: integer;
      gi: TGutterObject;
  begin
    if IsPrint and not (mpLineHighlight in FPrintOptions) then Exit;

    if Assigned(SyntObj) then
      SyntObj.GetLineHighlight(Line, HA1.InvSel, HA1.bg, HA1.fr, ShowDyn);
    FUserRanges.GetLineHighlight(Line, HA1.InvSel, HA1.bg, HA1.fr);
    for i := 0 to FLineInfos.Count - 1 do
     begin
      gi := TGutterObject(FLineInfos[i]);
      if (gi.BgColor <> clNone) or (gi.ForeColor <> clNone) then
       begin
        if gi.BgColor <> clNone then HA2.bg := gi.BgColor;
        if gi.ForeColor <> clNone then HA2.fr := gi.ForeColor;
        HA2.InvSel := gi.SelInvertColors;
       end;
     end;
    if Assigned(FOnLineHighLight) then
      FOnLineHighLight(Self, Line, HA2.InvSel, HA2.bg, HA2.fr);
  end;

  procedure DrawMarkers;
  var i, cx: integer;
      isFloat: Boolean;
  begin
    isFloat := soFloatMarkers in FOptions;
    for i := 0 to FCurMarkers.Count - 1 do
     with TMarker(FCurMarkers[i]) do
      begin
       if isFloat then cx := Position - BasePos
        else cx := CaretPos.X;
       if (cx >= k) and (LineEnd or (cx < k + ElemLen)) then
        begin
         if cx = k then cx := X else
          if LineEnd then cx := X + max(DefTextExt.cx * ( cx - Sl ), 0)
            else cx := X + ecTextExtent(Canvas, GetElemStr(CurPos, LogK, cx - k)).cx;
         DrawMarker(Canvas, cx, y + LnHeight);
        end;
      end;
  end;

  procedure DrawStaple(Staple: TBlockStaple; sLine, eLine: integer);
  var x: integer;
  begin
     x := Staple.XPos + R.Left;
     if (x < 0) or (y <> R.Top) or
        (Line = sLine) and (FCollapsed.GetCollapsed(Line) <> nil) then Exit;
     Self.DrawStaple(Canvas, x, y, BaseLine, Line = sLine, Line = eLine, FStapleLevel, Staple);
  end;

  procedure UpdateStaple(Staple: TBlockStaple);
  var p1, p2, sLine, eLine: integer;
      function LineStart(Line: integer): integer;
      begin
        Result := StringIndent(Lines[Line]);
      end;
  begin
    with Staple do
     begin
      SyntObj.GetStapleLines(Staple, sLine, eLine);
      p1 := LineStart(sLine);
      p2 := LineStart(eLine);
      SetCanvasAtPos(Canvas, CaretPosToStrPos(Point(sLine, 0)));
      XPos := Canvas.TextWidth(' ') * Min(p1, p2);
//      if ScrollPosX > 0 then
//        XPos := XPos + ScrollPosX * DefTextExt.cx;
     end;
  end;

  procedure DrawStaples;
  var i, sLine, eLine: integer;
      Staple: TBlockStaple;
  begin
    i := 0;
    while i < FCurStaples.Count do
      begin
        Staple := TBlockStaple(FCurStaples[i]);
        if Staple.XPos = -1 then UpdateStaple(Staple);
        if (LineEnd or (Staple.XPos + R.Left < X + ElemSize.cx)) then
         begin
           SyntObj.GetStapleLines(Staple, sLine, eLine);
           if (sLine < eLine) and (Line >= sLine) and (Line <= eLine) and
              ((Staple.XPos + R.Left - FStapleOffset <= X) or IsSpaceChar(GetChar(K + 1))) then
             DrawStaple(Staple, sLine, eLine);
           FCurStaples.Delete(i);
           Inc(FStapleLevel);
         end else
           Inc(I);
      end;
  end;

  procedure DrawNonPrinted;
  var i, tx, lk, t: integer;
      C: ecChar;
      s, sEnds: ecString; //AT
      HiliteBG: boolean; //AT
      Offset, HeightY: Integer; //AT
  const
    cDx = 2;
    cDy = 2; //indents for CR/LF mark
  begin
    //AT
    if FSelLength = 0 then
      HiliteBG := false
    else
    begin
      Offset := CaretPosToStrPos(Point(0, Line)) + Lines.LineLength(Line);
      HiliteBG := (Offset >= FSelStart) and (Offset < FSelStart + FSelLength);
    end;  
    if not FNonPrinted.Visible and (not LineEnd or not HiliteBG) then Exit;

     //Draw non printed
     Canvas.Brush.Style := bsClear;
     if FNonPrinted.UseFont then Canvas.Font := FNonPrinted.Font
      else Canvas.Font.Color := FNonPrinted.Color;
     HeightY := DefTextExt.cy + FLineSpacing;
     if LineEnd then
      begin
        i := Lines.LineSpace(Line) - Lines.LineLength(Line);
        if (Line < Lines.Count) and (i > 0) then
        if not FNonPrinted.Visible or not (FNonPrintedEol and FNonPrintedEolDetails) then
        begin
          if not FNonPrinted.Visible or not FNonPrintedEol then
            sEnds := ' ' //show space at line-ends
          else
          if (i = 1) and (Lines.TextFormat in [tfCR_NL, tfDefault]) then
            sEnds := FNonPrinted.SoftLineBreakChar
          else
            sEnds := FNonPrinted.LineBreakChar;
          if HiliteBG then
          begin
            Canvas.Brush.Color := DefaultStyles.SelectioMark.BgColor;
            Canvas.FillRect(Rect(X, Y, X + Canvas.TextWidth(sEnds), Y + HeightY));
          end;
          Canvas.TextOut(X, Y, sEnds);
        end
        else
        begin
          Canvas.Font.Size := Canvas.Font.Size - cDy;
          s := Lines.LineEndStr(Line);
          for i := 1 to Length(s) do
          begin
            case s[i] of
              #13: sEnds := 'CR';
              #10: sEnds := 'LF';
              else sEnds := '?';
            end;

            if HiliteBG then
            begin
              Canvas.Brush.Color := DefaultStyles.SelectioMark.BgColor;
              Canvas.FillRect(Rect(X, Y, X + Canvas.TextWidth(sEnds) + cDx*2, Y + HeightY));
            end;

            Inc(X, cDx);
            if opColorNonPrintedBG <> clNone then
              Canvas.Brush.Color:= opColorNonPrintedBG;
            Canvas.TextOut(X, Y + cDy div 2, sEnds);
            Inc(X, Canvas.TextWidth(sEnds));
          end;
        end;
      end
     else
       if FNonPrintedSpaces then //AT
       //below is old MZ's code
       begin
         tx := X;
         lk := LogK;
         for i := 1 to ElemLen do
           begin
             C := GetChar(k + i);
             if C = #9 then
               begin
                 t := NextTab(lk, BasePos + K + i);
                 if t > lk then
                   begin
                     Canvas.TextOut(tX, Y, FNonPrinted.TabChar);
                     Inc(tx, Canvas.TextWidth(' ') * (t - lk));
                     lk := t;
                   end;
               end else
               begin
                 if C = ' ' then
                   begin
                     Canvas.TextOut(tX, Y, FNonPrinted.SpaceChar);
                   end;
                 Inc(tx, ecTextExtent(Canvas, C).cx);
                 Inc(lk);
               end;
           end;
       end;
     Canvas.Brush.Style := bsSolid;
     Canvas.Font := Font;
  end;

  procedure SetMarkedFormat;
  var Stl: TSyntaxFormat;
  begin
   if Assigned(SyntObj) and Assigned(SyntObj.Owner.MarkedBlock) then
     Stl := SyntObj.Owner.MarkedBlock
   else
     Stl := FDefaultStyles.SelectioMark;

   ApplyStyle(Canvas, Stl);
   if BlockLine then
     StlList.Add(Stl, sLeft + BasePos, sRight + BasePos)
   else
     StlList.Add(Stl, FSelStart, FSelStart + FSelLength);
   HA1.Apply(Canvas, True);
   HA2.Apply(Canvas, True);
  end;

  procedure RemoveNewStyles;
  var i: integer;
  begin
    for i := StlList.Count - 1 downto 0 do
     if StlList[i].StartPos = CurPos then
      StlList.Delete(i);
  end;

  function PrepareCanvas(RemoveNew: Boolean = False): integer;
  var Flags, i, tmp: integer;
      CurLn: Boolean;
      CurLnStl: TSyntaxFormat;
  begin
    StlList.Clear;
    if IsPrint then
      begin
        if not (mpBlockHighlight in FPrintOptions) then
         Flags := slPrintable - slSubLexer - slTextRange
        else Flags := slPrintable;
      end else
       if ShowDyn then Flags := slAllStyles
         else Flags := slAllStyles - slDynoRange;

    Result := GetStyleList(CurPos, StlList, Flags, False);

    if RemoveNew then RemoveNewStyles;

    if (ProcType = ptLineHeight) and FMonoFont and (Canvas = Self.Canvas) then
     begin
       FHiddenStyle := False;
       for i := 0 to StlList.Count - 1 do
        with TStyleEntry(StlList[i]) do
         if (Style <> nil) and Style.Hidden then
          begin
           FHiddenStyle := True;
           Exit;
          end;
       Exit;
     end;

    ApplyDefStyle(Canvas, CurPos);
    if BackGnd then
      Canvas.Brush.Color := clNone;
    if ProcType = ptDrawing then HA1.Apply(Canvas, False);
    ApplyStyles(Canvas, StlList, False);

    CurLn := not IsPrint and (Line = FCaretPos.Y);
    CurLnStl := nil;  // to avoid warning
    if CurLn then
      if Assigned(SyntObj) and Assigned(SyntObj.Owner.CurrentLine) then
        CurLnStl := SyntObj.Owner.CurrentLine
      else
        CurLnStl := FDefaultStyles.CurrentLine;
    // Current line style
    if Assigned(FLineStyle) then ApplyStyle(Canvas, FLineStyle);
    if CurLn then ApplyStyle(Canvas, CurLnStl);
    if ProcType = ptDrawing then HA2.Apply(Canvas, False); // set line highlighting (only for drawing)
    ApplyStyles(Canvas, StlList, True);
    if CurLn then
      StlList.Add(CurLnStl, BasePos, BasePos + Sl + Length(sLineBreak));
    if Assigned(FLineStyle) then
      StlList.Add(FLineStyle, BasePos, BasePos + Sl + Length(sLineBreak));
    if BackGnd and (Canvas.Brush.Color = clNone) then
      Canvas.Brush.Style := bSClear;

//    if (Charset <> DEFAULT_CHARSET) and (Charset <> Canvas.Font.Charset) then
//      Canvas.Font.Charset := Charset;

    // Check selection
    if not LineEnd and not FHiddenStyle and ShowSel and (ProcType <> ptLineHeight) and not RemoveNew then
      begin
       if (k >= sLeft) and (k < sRight) then SetMarkedFormat;
       tmp := Result - BasePos;  // next format pos in line
       if (k < sLeft) and (tmp > sLeft) then
         Result := BasePos + sLeft else
       if (tmp > sRight) and (k < sRight) then
         Result := BasePos + sRight;
      end;

    if not IsPrint and (Canvas.Font.PixelsPerInch <> Pixel_Per_Inch) then
      begin
        tmp := Canvas.Font.Size;
        Canvas.Font.PixelsPerInch := Pixel_Per_Inch;
        Canvas.Font.Size := tmp;
      end;

    if IsPrint and (Canvas.Brush.Color = Color) then
      Canvas.Brush.Style := bsClear;
  end;

  //AT
  function IsWordChar2(ch: Widechar): boolean;
  begin
    //count '()[].,?!' as word-chars too, for word-wrap calculation,
    //to look like Notepad++
    Result := not ecStrUtils.IsSpaceChar(ch);
  end;

  function DetremineWordBreak: integer;
  var NextPos, i, lk, l: integer;
      BreakWord: Boolean;
      C: ecChar;
  begin
    BreakWord := X = R.Left;
    NextPos := X;
    Result := k;
    lk := LogK;
    for i := 1 to ElemLen do
      begin
        C := GetChar(k + i);
        // Position of the next character
        if C = #9 then
          begin
            l := Length(GetElemStr(CurPos + i - 1, lk, 1));
            Inc(NextPos, TextSize(' ').cx*l);
            Inc(lk, l);
          end else
          begin
            Inc(NextPos, TextSize(C).cx);
            Inc(lk);
          end;

        //AT: IsWordChar -> IsWordChar2 (count '()[]' as wordchars too)
        if not IsSpaceChar(C) then
         begin
          if ((i > 1) and not IsWordChar2(GetChar(k + i - 1)) or
             not IsWordChar2(C) or BreakWord) and
            (Result < k + i - 1) then
             Result := k + i - 1;
          if not IsWordChar2(C) then BreakWord := False;

          if NextPos >= WordBreakPos then Exit;
         end else
         begin
           BreakWord := False;
           Result := k + i;
         end;
      end;
    Result := -1;
  end;

 {$IFDEF EC_RTL}
  procedure DetectRTLRegions;
  var I, K: integer;
     procedure SkipDigits;
     begin
       while (I <= SL) and
             (IsSpaceChar(GetChar(i)) or
             (GetChar(i) >= '0') and (GetChar(i) <= '9') or
             (GetChar(i) = '.') or
             (GetChar(i) = ',') or
             (GetChar(i) = '"')) do
         Inc(I);
     end;
  begin
    SetLength(RTLregions, 0);
    I := 1;
    K := -1;
    SkipDigits;
    while I <= Sl do
      begin
        if IsRightToLefChar(GetChar(i)) then
          begin
            Inc(K);
            SetLength(RTLregions, K + 1);
            RTLregions[K].X := I - 1;
            while I <= Sl do
              begin
                Inc(I);
                SkipDigits;
                if I > Sl then
                  RTLregions[K].Y := Sl else
                if not IsRightToLefChar(GetChar(i)) then
                  begin
                    RTLregions[K].Y := I - 1;
                    Break;
                  end;
              end;
          end;
        Inc(I);
      end;
  end;
  {$ENDIF}

  procedure GetLnHieghtFromFont;
  var tmp: integer;
  begin
    if not IsFixedHgt then
      begin
        tmp := CurrentFontHeight(Canvas);
        if LnHeight < tmp then
          LnHeight := tmp;
      end;
  end;

  procedure SetLineStart;
  var w, lw: integer;
      pntR: TRect;
  begin
    if (ProcType = ptLineHeight) or (FAlignment = taLeftJustify) then
      X := R.Left else
    begin
      if AWordWrap then
        w := WordBreakPos
      else
        w := R.Right;
      lw := LineInfo.Widths[LineNum];
      if FAlignment = taRightJustify then
        X := w - lw - FTextMargin
      else
        X := (w - lw - R.Left) div 2 + R.Left;
      if (ProcType = ptDrawing) and not IsTransparent and (X > R.Left) then
        begin
          pntR := Rect(R.Left, Y, X, BaseLine);
          PrepareCanvas;
          if not PaintNoTextSpace(Canvas, pntR, Line) then
            FillRectEx(pntR);
        end;
    end;
  end;

var nf, Wrap_k, SpaceWidth, tmp, ColIconPos: integer;
    ColRangeText: string;
    R1: TRect;
begin
  Result.x := 0;
  Result.y := 0;
//  EmbObj := -1;
  if ProcType = ptTestOverImage then Result.X := -1;
  if Line >= Lines.Count then
    Exit;  // only for valid line

  if not HandleAllocated then Exit;

  Pixel_Per_Inch := Round(PixelPerInch.X * FZoom/ 100);
  if HandleAllocated then
    ClipR := Canvas.ClipRect
  else
    SetRectEmpty(ClipR);

  // Selection and dyno flags
  IsPrint := Canvas <> Self.Canvas;
  ShowSel := not IsPrint and (ProcType <> ptLineHeight);
  ShowDyn := (Focused or not (soHideDynamic in FOptions)) and Assigned(SyntObj) and ShowSel;
  ShowSel := (Focused or not (soHideSelection in FOptions)) and ShowSel;
  BlockLine := ShowSel and (SelectMode = msColumn) and (Line >= FBlock.Top) and (Line <= FBlock.Bottom) and
               (FBlock.Right > FBlock.Left);
  TransparentBack := IsPrint and not (mpBackColor in FPrintOptions) or FTransparent;
  if IsPrint then AWordWrap := mpWordWrap in FPrintOptions
             else AWordWrap := FWordWrap;
  BackGnd := not IsPrint and (not FBackGround.IsDefault or DoubleBuffered);
  IsFixedHgt := (soFixedLineHeight in FOptions) and not IsPrint;

  // Word break position
  WordBreakPos := 0;
  if AWordWrap then
   if IsPrint then WordBreakPos := R.Right else
   if soBreakOnRightMargin in FOptions then
     begin
       nf := RightMargin;
       if Assigned(FOnGetRightMargin) then
         FOnGetRightMargin(Self, Line, nf, Canvas.Pen);
       if nf <= 0 then
         AWordWrap := False
       else
         if FTextMargins.AbsolutePos then
           WordBreakPos := nf  + R.Left + 1
         else
           WordBreakPos := nf * DefTextExt.cx  + R.Left + 1;
     end
   else
     WordBreakPos := ClientWidth - FMargin.Left - FMargin.Right + R.Left - DefTextExt.cx div 2; //ZD
     // Decreased by DefTextExt.cx div 2 pixels that adds a little space between the right side of text
     // and vertical scroll bar to allow placing of caret with the mouse on the end.
     //ZD

  FLineStyle := nil;
  if Assigned(FOnGetLineStyle) then
    FOnGetLineStyle(Self, Line, FLineStyle);

  // Get line string
  CurPos := Lines.LineIndex(Line) - 1;           // line pos
  Sl := Lines.LineLength(Line);
  if FromPos >= Sl then
    begin
      Inc(CurPos, Sl);
      Sl := 0;
    end else
  if (FromPos > 0) or (ToPos > 0) then
    begin
      if ToPos = 0 then ToPos := Sl
       else ToPos := ToPos - FromPos;
      Inc(CurPos, FromPos);
      Sl := ToPos;
    end;
  BasePos := CurPos;
 {$IFDEF EC_RTL}
 // Determination RTL regions
  InRtl := False;
  RTLRegIdx := 0;
  RTlStartIdx := 0;
  DetectRTLRegions;
 {$ENDIF}

  // Init loop variables
  FStapleLevel := 0;
  k := 0;                                           // actual pos in line
  LogK := 0;                                        // Logical pos in line
  LineNum := 0;                                     // number of logical line (wordwrap)
  LineEnd := False;
  sv_bg_color := clNone;
  Y := R.Top;

  // Selection range
  if BlockLine then
    begin // column selection
      sLeft := LogToLinesPos(Point(FBlock.Left, Line)).X;
      sRight := LogToLinesPos(Point(FBlock.Right, Line)).X;
    end else
  if ShowSel and (SelectMode <> msColumn) and (FSelLength > 0) and
     (FSelStart + FSelLength > BasePos) and (FSelStart <= BasePos + Sl) then
    begin // Normal selection
      if FSelStart > BasePos then sLeft := FSelStart - BasePos else sLeft := 0;
     sRight := FSelStart + FSelLength - BasePos;
    end else
    begin // No selection
      ShowSel := False;
      sLeft := 0;
      sRight := 0;
    end;

  // Get collapsed region text icon
  if IsPrint or FDisableFolding then ColRangeText := ''
   else ColRangeText := GetColRangeTextAtPos(Line, ColIconPos);

  //AT
  if Pos(#9, ColRangeText)>0 then
    ColRangeText:= StringReplace(ColRangeText, #9, StringOfChar(' ', TabList[0]), [rfReplaceAll]);

  if ColRangeText <> '' then
   begin
    // Set new length
    ColIconPos := ColIconPos - BasePos;
    if (ColIconPos >= 0) and (ColIconPos < Sl) then
      Sl := ColIconPos;
   end;

  // Selecting objects associated with the line (used only while drawing)
  FCurStaples := nil;
  FCurMarkers := nil;
  HA1 := TLineHighlightAttr.Create;
  HA2 := TLineHighlightAttr.Create;
  if (ProcType = ptDrawing) and (FZoom >= 75) then //AT
   begin
     if not IsPrint or (mpBlockStaples in FPrintOptions) then
	   if FStaplesEnabled then //AT
       FCurStaples := GetBlockStaples(Line);
     if not IsPrint then
       FCurMarkers := GetMarkersFroLine(Line, BasePos, BasePos + Sl);
     GetLineHighlight;
   end;
//  if Assigned(FEmbObjects) then
//    EmbObj := FEmbObjects.PriorAt(BasePos, True);
  if Assigned(FEmbObjects) then
    ImgList := FEmbObjects.GetObjectsInRange(BasePos, BasePos + Sl)
  else
    ImgList := nil;
  StlList := TStyleEntries.Create;

try

  UnchangedWB := True;
  case ProcType of
//    ptLineHeight:
//      LineInfo.FWidth := 0;
    ptDrawing:
      if (Param.X > 0) and (Param.X < LineInfo.LineCount) then
      begin
        k := LineInfo.WordBreaks[Param.X - 1];
        LineNum := Param.X;
      end;
    ptTextToMouse:
      if not Assigned(ImgList) then
      for i := 0 to LineInfo.LineCount - 2 do
        if LineInfo.WordBreaks[i] < Param.X then
        begin
          Inc(LineNum);
          k := LineInfo.WordBreaks[i];
          Inc(Y, LineInfo.Heights[i]);
        end else Break;
    ptTestOverCollapsed,
    ptTestOverImage,
    ptMouseToText:
      if not Assigned(ImgList) then
      for i := 0 to LineInfo.LineCount - 2 do
       if Y + LineInfo.Heights[i] <= Param.Y then
       begin
         k := LineInfo.WordBreaks[i];
         Inc(Y, LineInfo.Heights[i]);
         Inc(LineNum);
       end else Break;
  end;
  CurPos := BasePos + k;
  if ProcType <> ptLineHeight then LnHeight := LineInfo.Heights[LineNum]  else
  if IsFixedHgt then LnHeight := DefTextExt.cy else LnHeight := 0;

  SetLineStart; // starting point

  // line content processing
  nf := CurPos;
  while k < Sl do
   begin
//     if nf <= CurPos then
       nf := PrepareCanvas;

     // limiting with end of line
     if nf - CurPos >= Sl - k then SetElemLength(Sl - k)
      else SetElemLength(nf - CurPos);
       
     //MZ: 
     //bug in rendering functions (for long strings)
     //if ElemLen > 256 then ElemLen := 256;
     
     //AT: it's needed (e.g. lines longer than 4700 are invisible) but with bigger value, e.g. 512:
     if ElemLen > 512 then ElemLen := 512;

    {$IFDEF EC_RTL}
    if RTLRegIdx < Length(RTLregions) then begin
            if k = RTLregions[RTLRegIdx].Y then begin
                Inc(RTLRegIdx);
                x := RTLStart + RTLWidth;
            end;
            if RTLRegIdx < Length(RTLregions) then begin
                InRtl := (k >= RTLregions[RTLRegIdx].x) and (k < RTLregions[RTLRegIdx].y);
                if InRtl then begin
                    if k + ElemLen > RTLregions[RTLRegIdx].y then
                        ElemLen := RTLregions[RTLRegIdx].y - k;
                end else begin
                    if k + ElemLen > RTLregions[RTLRegIdx].x then
                        ElemLen := RTLregions[RTLRegIdx].x - k;
                end;
                if k = RTLregions[RTLRegIdx].X then begin // initializing RTL section
                    RTLStart := X;
                    RTLStartIdx := RTLregions[RTLRegIdx].X;
                    RTLStr := Lines.SubString(BasePos + RTLStartIdx + 1,
                                RTLregions[RTLRegIdx].Y - RTLStartIdx);
                    RTLWidth := ecTextExtent(Canvas, RTLStr).cx;
                end;
            end else
                InRtl := False;
    end;
    {$ENDIF}

     if not FHiddenStyle then
      begin

       // *** In-text images processing
       // Set next position to the nearest image
       if ImgList <> nil then
        for i := ImgList.Count - 1 downto 0 do
         with TecCustomEmbeddedObject(ImgList[i]) do
          if Position = CurPos then
           begin
             // draw images
              ElemSize := GetSize(Self, Canvas);
              case ProcType of
                 ptTextToMouse:
                   begin
                     Dec(Param.X);
                     if k >= Param.X then
                       begin
                         Result.X := X;
                         if k = Param.X then
                           Inc(Result.X, ElemSize.cx);
                         Result.Y := Y;
                         Exit;
                       end;
                   end;
                 ptMouseToText:
                   if (x + ElemSize.cx > Param.X) and (Param.Y < y + LnHeight) then
                     begin
                       Inc(Result.X, k);
                       if Param.X - X > ElemSize.cx div 2 then
                         Inc(Result.X);
                       Exit;
                     end else
                       Inc(Result.X);
                 ptLineHeight:
                     if not IsFixedHgt then
                       if LnHeight < ElemSize.cy then
                         LnHeight := ElemSize.cy;
                 ptDrawing:
                     begin
//                       if not (efOpaque in Flags) then
                         Canvas.FillRect(Bounds(x, y, ElemSize.cx, LnHeight));
                       Draw(Self, Canvas, Bounds(x, y + (LnHeight - ElemSize.cy) div 2, ElemSize.cx, ElemSize.cy));
                     end;
                 ptTestOverImage:
                     if PtInRect(Bounds(x, y + (LnHeight - ElemSize.cy) div 2, ElemSize.cx, ElemSize.cy), Param) then
                      begin
                       Result.X := Index;
                       Exit;
                      end;
              end;
              Inc(x, ElemSize.cx);
              ImgList.Delete(i);
           end else
          if (Position > CurPos) and (Position < CurPos + ElemLen) then
            SetElemLength(Position - CurPos);
       // *** End of in-text images processing

       // *** Word wrap processing
       if AWordWrap then
         begin
           if ProcType = ptLineHeight then
            begin
              // Word wrap calculation
              GetLnHieghtFromFont;
              {$IFDEF EC_RTL}
              if InRTL then
                Wrap_k := -1
              else
              {$ENDIF}
                Wrap_k := DetremineWordBreak;
              if Wrap_k <> -1 then
               begin
                // Bug fixing
                if (LineNum > 1) and (Wrap_k = LineInfo.WordBreaks[LineNum - 1]) then
                  Inc(Wrap_k);
                // - Bug fixing
                ElemLen := Wrap_k - k;
                GetElement;
                Inc(X, ElemSize.cx);
                if k + ElemLen >= Wrap_k then
                  begin
                    if UnchangedWB then
                       UnchangedWB := LineInfo.WordBreaks[LineNum] = Wrap_k;
                    if not UnchangedWB then
                       LineInfo.WordBreaks[LineNum] := Wrap_k;

                    if LineNum = 0 then
                      Inc(LnHeight, FLineSpacingBefore);
                    Inc(LnHeight, FLineSpacing);
                    if LnHeight < 0 then
                      LnHeight := 0;
                    LineInfo.Heights[LineNum] := LnHeight;
                    LineInfo.Widths[LineNum] := X - R.Left;
                    Inc(LineNum);
                    SetLineStart;
//                    if X - R.Left > LineInfo.FWidth then
//                      LineInfo.FWidth := X - R.Left;
                    if UnchangedWB and (LineNum > 5) then Exit;
                    if IsFixedHgt then LnHeight := DefTextExt.cy
                                  else LnHeight := 0;
                  end;
               end else
               begin
                 GetElement;
                 Inc(X, ElemSize.cx);
               end;
              Inc(k, ElemLen);
              Inc(LogK, Length(ElemStr));
              Inc(CurPos, ElemLen);
              Continue;
            end
           else
            begin
              // Word wrapped line processing
              Wrap_k := LineInfo.WordBreaks[LineNum];
              if (Wrap_k > 0) and (Wrap_k < k + ElemLen) then
               begin
                if Wrap_k > k then SetElemLength(Wrap_k - k) else
                if (Wrap_k <= k) {and (X <> R.Left)} then
                  begin
                    case ProcType of
                      ptDrawing:
                        begin
                          PrepareCanvas(True);
                          if (k > sLeft) and (k < sRight) then SetMarkedFormat;
                          if not IsTransparent then Canvas.FillRect(Rect(x, y, R.Right, y + LnHeight));
                          DrawBounds(CurPos, CurPos, Rect(x, y, R.Right, y + LnHeight), True);
                          if y + LnHeight > ClipR.Bottom then
                            Exit;
                          if (Param.Y >= 0) and (Param.Y = LineNum) then Exit;
                          nf := 0;
                        end;
                      ptMouseToText:
                        if Param.Y < y + LnHeight then
                          begin
                            Inc(Result.X, k - 1); // !!!
                            Exit;
                          end;
                      ptTestOverCollapsed,
                      ptTestOverImage:
                        if Param.Y < y + LnHeight then
                          Exit;
                    end;
                    X := R.Left;
                    Inc(y, LineInfo.Heights[LineNum]);
                    Inc(LineNum);
                    LnHeight := LineInfo.Heights[LineNum];
                    SetLineStart;
                    FreeAndNil(FCurStaples);
                    Continue;
                  end;
              end;
            end;
         end;
       // *** End of word wrap processing

//       if (ProcType = ptTextToMouse) and (k + ElemLen > Param.X) then
//         SetElemLength(Param.X - k);

       if Assigned(FCurStaples) then
         begin
           if not IsSpaceChar(GetChar(K + 1)) then
             begin
               for i := FCurStaples.Count - 1 downto 0 do
                 if TBlockStaple(FCurStaples[i]).XPos - FStapleOffset + R.Left > X then
                   FCurStaples.Delete(i);
             end
           else
             begin
               i := 1;
               while (i < ElemLen) and IsSpaceChar(GetChar(K + i + 1)) do Inc(i);
               ElemLen := i;
             end;
         end;
       // *** Visible element processing

       // Size of the element
       GetElement;
       // visible tag processing
       case ProcType of
         ptTextToMouse:
           {$IFDEF EC_RTL}
           if InRTL then
            begin
             if RTLStartIdx + Length(RTLStr) > Param.X then
              begin
               FillChar(ResCP, sizeof(ResCP), 0);
               ResCP.lStructSize := sizeof(ResCP);
               tmp := Length(RTLStr) * sizeof(integer);
               ResCP.lpCaretPos := GetMemory(tmp);
               ResCP.lpDx := GetMemory(tmp);
//               ResCP.lpOrder := GetMemory(tmp);
//               ResCP.lpClass := GetMemory(Length(RTLStr));
               ResCP.nGlyphs := Length(RTLStr);
               FillChar(ResCP.lpCaretPos^, tmp, 0);
               FillChar(ResCP.lpDx^, tmp, 0);
//               FillChar(ResCP.lpOrder^, tmp, 0);
//               FillChar(ResCP.lpClass^, Length(RTLStr), 0);
               GetCharacterPlacementW(Canvas.Handle, PWideChar(RTLStr),
                 Length(RTLStr) + 1, 1000, ResCP, GCP_REORDER {or GCP_GLYPHSHAPE or GCP_DISPLAYZWG});
               Result.X := RTLStart + PIntegerArray(ResCP.lpCaretPos)^[Param.X - RTLStartIdx];
               FreeMemory(ResCP.lpCaretPos);
               FreeMemory(ResCP.lpDx);
//               FreeMemory(ResCP.lpOrder);
               Result.Y := y;
               Exit;
              end;
            end else
           {$ENDIF}
           // -- start of caret behaviour finxing
           if k + ElemLen = Param.X then
            begin
             if (LineNum < LineInfo.LineCount - 1) and
                (LineInfo.WordBreaks[LineNum] = Param.X) then
               begin
                 Result.X := R.Left;
                 Result.Y := Y + LineInfo.Heights[LineNum];
               end else
                 Result := Point(x + ElemSize.cx, Y);
             Exit;
            end else
           // -- end of caret behaviour finxing
           if k + ElemLen > Param.X then
           begin
             Result.X := x + TextSize(GetElemStr(CurPos, LogK, Param.X - k)).cx;
             Result.Y := y;
             Exit;
           end;
         ptMouseToText:
           {$IFDEF EC_RTL}
           if InRTL then
            begin
             if (x + RTLWidth > Param.X) and (Param.Y < y + LnHeight) then
              begin
               FillChar(ResCP, sizeof(ResCP), 0);
               ResCP.lStructSize := sizeof(ResCP);
               tmp := Length(RTLStr) * sizeof(integer);
               ResCP.lpCaretPos := GetMemory(tmp);
//               ResCP.lpDx := GetMemory(tmp);
               ResCP.lpOrder := GetMemory(tmp);
//               ResCP.lpClass := GetMemory(Length(RTLStr));
               ResCP.nGlyphs := Length(RTLStr);
               FillChar(ResCP.lpCaretPos^, tmp, 0);
//               FillChar(ResCP.lpDx^, tmp, 0);
               FillChar(ResCP.lpOrder^, tmp, 0);
//               FillChar(ResCP.lpClass^, Length(RTLStr), 0);
               GetCharacterPlacementW(Canvas.Handle, PWideChar(RTLStr),
                 Length(RTLStr) + 1, 1000, ResCP, GCP_REORDER {or GCP_GLYPHSHAPE or GCP_DISPLAYZWG});
               Result.X := 0;
               tmp := 1000;
               for i := 0 to Length(RTLStr)- 1 do
                 begin
                   k := abs(RTLStart + PIntegerArray(ResCP.lpCaretPos)^[i] - Param.X);
                   if k < tmp then
                    begin
                     Result.X := i;
                     tmp := k;
                    end;
                 end;
               Result.X := RTLStartIdx + Result.X;//PIntegerArray(ResCP.lpOrder)^[Result.X];
               FreeMemory(ResCP.lpCaretPos);
//               FreeMemory(ResCP.lpDx);
               FreeMemory(ResCP.lpOrder);
//               FreeMemory(ResCP.lpClass);
               Result.Y := y;
               Exit;
              end;
            end else
           {$ENDIF}
           if (x + ElemSize.cx > Param.X) and (Param.Y < y + LnHeight) then
            begin
              while k < Sl do
               begin
                 {$IFDEF EC_MBCS}
                 if (k < Sl - 1) and (Lines.CharType(BasePos + K + 2) = mbTrailByte) then
                   begin
                     dK := 2;
                     cS := Lines.SubString(BasePos + K + 1, 2);
                   end else
                 {$ENDIF}
                   begin
                     cS := GetChar(k + 1);
                     dK := 1;
                   end;
                 if cS = #9 then
                   begin
                     tmp := Length(GetElemStr(BasePos + K, LogK, 1));
                     Inc(LogK, tmp);
                     tmp := TextSize(' ').cx * tmp;
                   end
                 else
                   begin
                     tmp := TextSize(cS).cx;
                     Inc(LogK, dK);
                   end;
                 Inc(X, tmp);
                 if x >= Param.X then
                  begin
                   if x - Param.X < tmp div 2 then Inc(k, dK);
                   Break;
                  end;
                 Inc(k, dk);
               end;
              Inc(Result.X, k);
              Exit;
            end;
         ptLineHeight:
            begin
              if not AWordWrap and FMonoFont
                 and not (soVariableHorzScrollBar in FOptions) // I need to know line width
                 and (Canvas = Self.Canvas) then
               begin
                LnHeight := FDefExt.cy;
                Break;
               end;

              GetLnHieghtFromFont;
            end;
         ptDrawing:
            begin
              DrawElem;
              DrawBounds(CurPos, CurPos + ElemLen, Rect(x, Y, x + ElemSize.cx, BaseLine), False);
              if Assigned(FCurMarkers) then DrawMarkers;
              if Assigned(FCurStaples) then DrawStaples;
              if {FNonPrinted.Visible and} not IsPrint then DrawNonPrinted; //AT
              if Assigned(FOnDrawToken) then
                FOnDrawToken(Self, Rect(X, y, X + ElemSize.cx, y + LnHeight), Point(k, Line), ElemLen);
            end;
       end;
       inc(x, ElemSize.cx);
       if (x > ClipR.Right) and (ProcType = ptDrawing) and not AWordWrap then
        begin
          DrawBounds(CurPos, CurPos, Rect(x, Y, x, BaseLine), True);
          Exit;
        end;
       if Assigned(FCurStaples) and not IsSpaceChar(GetChar(K + 1)) then
         FreeAndNil(FCurStaples);
       // *** End of visible element processing

      end { end of not hidden processing}
     else
      if (ProcType = ptTextToMouse) and (k + ElemLen >= Param.X) then
        begin
          Result.X := x;
          Result.Y := y;
          Exit;
        end;

     inc(k, ElemLen);
     Inc(LogK, Length(ElemStr));
     inc(CurPos, ElemLen);
   end;
  LineEnd := True;
  StlList.Clear;

  // end of line content processing

  // Draw collapsed mark text
  if ColRangeText <> '' then
    begin
      ShowDyn := False;
      PrepareCanvas;
      StlList.Clear;
      if Assigned(SyntObj) and Assigned(SyntObj.Owner.CollapseStyle) then
        ApplyStyle(Canvas, SyntObj.Owner.CollapseStyle)
      else
        ApplyStyle(Canvas, FDefaultStyles.CollapseMark);
      StlList.Add(FDefaultStyles.CollapseMark, CurPos, CurPos + 1);
      if not BlockLine and (FSelStart <= CurPos) and (FSelStart + FSelLength > CurPos) then
        SetMarkedFormat;
      IsTransparent;
      ElemSize := ecTextExtent(Canvas, ColRangeText);
      case ProcType of
        ptMouseToText:
          begin
            Inc(Result.X, Sl);
            Exit;
          end;
        ptTestOverCollapsed:
           begin
             if (Param.X >= x) and (Param.X - x < ElemSize.cx) then
               Result := Point(ElemSize.cx, BaseLine - Y)
             else
               Result := Point(0, 0);
             Exit;
           end;
        ptLineHeight:
          begin
            GetLnHieghtFromFont;
            Inc(X, ElemSize.cx);
          end;
        ptDrawing:
          begin
            DrawTagStr(ColRangeText, x);
            DrawBounds(CurPos, CurPos + 1, Rect(x, Y, x + ElemSize.cx, BaseLine), False);
            Inc(x, ElemSize.cx);
            sv_bg_color := clNone;
          end;
      end;
      CurPos := BasePos + Sl;
      StlList.Clear;
    end;

  // Fill selected region in column selection mode
  PrepareCanvas;
  if HandleAllocated then
    begin
      SpaceWidth := Canvas.TextWidth(' ');
      if SpaceWidth = 0 then
        SpaceWidth := 16;
    end
  else
    SpaceWidth := 16;
  tmp := x;
  if BlockLine and (Sl < sRight) and (ProcType = ptDrawing) then
  begin
    if k <= sLeft then// FBlock.Left then
     begin
      PrepareCanvas;
      if not IsTransparent then
        FillRectEx(Rect(tmp, y, tmp + (sLeft - k) * SpaceWidth, BaseLine));
      inc(tmp, (sLeft - k) * SpaceWidth);
      k := sLeft;
     end;
    if k < sRight then// FBlock.Right then
     begin
      PrepareCanvas;
      SetMarkedFormat;
      if not IsTransparent then
        FillRectEx(Rect(tmp, y, tmp + (sRight - k) * SpaceWidth, BaseLine));
      inc(tmp, (sRight - k) * SpaceWidth);
     end;
  end;

  PrepareCanvas;
  if (sl < sRight) and not BlockLine and
     (not (soNormalSelToLineEnd in FOptionsEx) or not (SelectMode in [msNormal, msNone])) then
    SetMarkedFormat;

  // **** Line end of line processing  ****
  case ProcType of
    ptTextToMouse: begin
                     if ImgList <> nil then
                      for i := 0 to ImgList.Count - 1 do
                       with TecCustomEmbeddedObject(ImgList[i]).GetSize(Self, Canvas) do
                         begin
                           if Param.X > Sl then
                             begin
                               Inc(x, cx);
                               Dec(Param.X);
                             end else
                               Break;
                         end;
                     Result.X := x + (Param.X - Sl) * SpaceWidth;
                     Result.Y := y;
                   end;
    ptMouseToText: begin
                     Inc(Result.X, Sl);
                     if ImgList <> nil then
                      for i := 0 to ImgList.Count - 1 do
                       with TecCustomEmbeddedObject(ImgList[i]).GetSize(Self, Canvas) do
                         begin
                           Inc(x, cx);
                           if Param.X > x then
                             Inc(Result.X)
                           else
                             Exit;
                         end;
                     Inc(Result.X, (Param.X - x) div SpaceWidth);
                   end;
    ptLineHeight:  begin
//                     LineInfo.AddWordBreak(Sl);

                     if Sl = 0 then
                       begin
                         if FHiddenStyle then
                           LineInfo.FHidden := True
                         else
                           LnHeight := CurrentFontHeight(Canvas);
                       end
                     else
                       LineInfo.FHidden := (LineNum = 0) and (LnHeight = 0);

                     if ImgList <> nil then
                      for i := ImgList.Count - 1 downto 0 do
                       with TecCustomEmbeddedObject(ImgList[i]).GetSize(Self, Canvas) do
                         begin
                           if (LnHeight < cy) and not IsFixedHgt then
                             LnHeight := cy;
                           Inc(x, cx);
                         end;

                     if LineNum = 0 then Inc(LnHeight, FLineSpacingBefore);
                     Inc(LnHeight, FLineSpacingAfter);
                     Inc(LnHeight, FLineSpacing);
                     if LnHeight < 0 then LnHeight := 0;
                     LineInfo.Heights[LineNum] := LnHeight;
                     LineInfo.Widths[LineNum] := X - R.Left;
//                     LineInfo.FWidth := X - R.Left;
                     if LineInfo.LineCount > LineNum + 1 then
                      for i := LineInfo.LineCount - 2 to LineNum do
                       LineInfo.FWordBreaks.Delete(i);
                   end;
    ptDrawing:     begin
                     R1 := Rect(tmp, y, R.Right, BaseLine);
                     if not IsTransparent and not PaintNoTextSpace(Canvas, R1, Line) then
                         FillRectEx(R1);
                     DrawBounds(CurPos, CurPos + Length(sLineBreak), R1, True);
                     if Assigned(FCurMarkers) then DrawMarkers;
                     if Assigned(FCurStaples) then DrawStaples;
                     if ImgList <> nil then
                      for i := ImgList.Count - 1 downto 0 do
                       with TecCustomEmbeddedObject(ImgList[i]), GetSize(Self, Canvas) do
                         begin
                           // draw images
                           Canvas.FillRect(Bounds(x, y, cx, LnHeight));
                           Draw(Self, Canvas, Bounds(x, y + (LnHeight - cy) div 2, cx, cy));
                           Inc(x, cx);
                         end;
                     if {FNonPrinted.Visible and} not IsPrint then
                       DrawNonPrinted; //AT
                   end;
    ptTestOverImage:begin
                     if ImgList <> nil then
                      for i := ImgList.Count - 1 downto 0 do
//                       with TInTextImage(ImgList[i]) do
                       with TecCustomEmbeddedObject(ImgList[i]), GetSize(Self, Canvas) do
                         begin
                           // draw images
//                           if PtInRect(Bounds(x, y, FInTexTCustomImageList.Width, LnHeight), Param) then
                           if PtInRect(Bounds(x, y, cx, LnHeight), Param) then
                             Result.X := Index;
                           Inc(x, cx);
                         end;
                    end;
  end;
finally
  FreeTempObj;
end;
end;
//==============================================================================
//==============================================================================
//   End of line processing
//==============================================================================

function TCustomSyntaxMemo.PaintNoTextSpace(Canvas: TCanvas; const R: TRect; Line: integer): Boolean;
begin
  Result := False;
end;

//==============================================================================
// Fill gutter
procedure TCustomSyntaxMemo.FillGutterBackground(Line, y1, y2: integer; Canvas: TCanvas);
var x, x1, i: integer;
    NColor: TColor; //AT
{    c1, c2: TColor;
 function GetGradColor(c1, c2: TColor; x, N: integer): TColor;
 var b1, b2, b, i: Byte;
 begin
   Result := 0;
   for i := 0 to 2 do
    begin
     b1 := c1 shr (8 * i) and $FF;
     b2 := c2 shr (8 * i) and $FF;
     b := b1 + (b2 - b1)*x div N;
     Result := Result or (b shl (8 * i));
    end;
 end;}
begin
  x := 0;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  for i := 0 to Gutter.Bands.Count - 1 do
   if Gutter.Bands[i].Width > 0 then
    begin
     x1 := min(x + Gutter.Bands[i].Width, FGutter.Width);
     if x >= x1 then Exit;
     if Gutter.Bands[i].Color <> clNone then  Canvas.Brush.Color := Gutter.Bands[i].Color
                                        else  Canvas.Brush.Color := FGutter.Color;
                                        
     if Assigned(FOnGetGutterBandColor) then
     begin
       NColor := Canvas.Brush.Color;
       FOnGetGutterBandColor(Self, i, Line, NColor);
       if NColor <> clNone then
         Canvas.Brush.Color := NColor;
     end;                                     
                                        
     if (FLineStateDisplay.Band = i) and (Line <> -1) then
       case Lines.LineState[Line] of
         lsNormal:
           if FLineStateDisplay.UnchangedColor <> clNone then
             Canvas.Brush.Color := FLineStateDisplay.UnchangedColor;
         lsModified:
           if FLineStateDisplay.ModifiedColor <> clNone then
             Canvas.Brush.Color := FLineStateDisplay.ModifiedColor;
         lsNew:
           if FLineStateDisplay.NewColor <> clNone then
             Canvas.Brush.Color := FLineStateDisplay.NewColor;
         lsSaved:
           if FLineStateDisplay.SavedColor <> clNone then
             Canvas.Brush.Color := FLineStateDisplay.SavedColor;
       end;
     if Gutter.Bands[i].Gradient then
      begin
//       c1 := ColorToRGB(Canvas.Brush.Color);
//       c2 := ColorToRGB(Gutter.Bands[i].GradientRight);
       GradientFill(Canvas, Rect(x, Y1, x1, Y2), Canvas.Brush.Color, Gutter.Bands[i].GradientRight, False);
{       Canvas.Pen.Width := 1;
       Canvas.Pen.Style := psSolid;
       for j := 0 to x1 - x do
        begin
         Canvas.Pen.Color := GetGradColor(c1, c2, j, x1 - x);
         Canvas.MoveTo(x + j, Y1);
         Canvas.LineTo(x + j, Y2);
        end;}
      end else
       Canvas.FillRect(Rect(x, Y1, x1, Y2));

     if Gutter.Bands[i].LeftBound <> clNone then
       begin
        Canvas.Pen.Color := Gutter.Bands[i].LeftBound;
        Canvas.MoveTo(x, Y1);
        Canvas.LineTo(x, Y2);
       end;
     if Gutter.Bands[i].RightBound <> clNone then
       begin
        Canvas.Pen.Color := Gutter.Bands[i].RightBound;
        Canvas.MoveTo(x1 - 1, Y1);
        Canvas.LineTo(x1 - 1, Y2);
       end;
     inc(x, Gutter.Bands[i].Width);
     if x >= FGutter.Width then Exit;
    end;
  Canvas.Brush.Color := FGutter.Color;
  Canvas.FillRect(Rect(x, Y1, FGutter.Width, Y2));
end;

procedure TCustomSyntaxMemo.DrawGutterLine(y1, y2, Line, LineColState: integer);   // Draw Gutter
var i, j, w, h, YTop, Y: integer;
    gi: TCustomGutterObject;
    IL: TCustomImageList;
    S: string;
    pn: TPoint;
    ts: TSize;
    R: TRect;
    Cvs: TCanvas;
    Ln: TLineInfo;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);

  procedure DrawLineNumber(WasImages: Boolean);
  begin
   if not FLineNumbers.Visible then Exit;
   cvs.Font := FLineNumbers.Font;
   if WasImages then
    if FLineNumbers.UnderColor <> clNone then cvs.Font.Color := FLineNumbers.UnderColor
     else Exit;
   s := GetLineNumberStr(Line);
   if s = '' then Exit;
   ts := cvs.TextExtent(S);
   pn := FGutter.GetBandBound(FLineNumbers.Band);
   case FLineNumbers.Alignment of
     taLeftJustify:  pn.X := pn.X + FLineNumbers.Margin;
     taRightJustify: pn.X := pn.Y - FLineNumbers.Margin - ts.cx;
     taCenter:       pn.X := (pn.Y + pn.X - ts.cx) div 2;
   end;
   case FLineNumbers.VertAlignment of
     vaTop:     pn.y := Y1;
     vaBottom:  pn.y := Y2 - ts.cy;
     else       pn.y := (Y2 + Y1 - ts.cy) div 2;
   end;
   cvs.TextOut(pn.X, pn.Y, S);
  end;

var buf, HasImg: Boolean;
begin
  YTop := Y1;
  buf := FGutter.DoubleBufered and not DoubleBuffered;
  if buf then
   begin
    Y2 := Y2 - Y1;
    Y1 := 0;
    FGutter.FBuffer.Width := FGutter.Width;
    FGutter.FBuffer.Height := Y2;
    cvs := FGutter.FBuffer.Canvas;
   end else
    cvs := Canvas;

  FillGutterBackground(Line, Y1, Y2, cvs);
  cvs.Brush.Style := bsClear;
  // Check for gutter images
  HasImg := LineColState >= 0;
  if not HasImg then
   for i := 0 to FLineInfos.Count - 1 do
    if TCustomGutterObject(FLineInfos[i]).ImageIndex <> -1 then
      begin
        HasImg := True;
        Break;
      end;
  DrawLineNumber(HasImg);

  // Draw gutter images
  for i := 0 to FLineInfos.Count - 1 do
   begin
    gi := TCustomGutterObject(FLineInfos[i]);
    if gi.ImageList <> nil then IL := gi.ImageList
     else IL := FGutter.Images;
    if Assigned(IL) and not IsRectEmpty(gi.FBounds) then
      if gi.SubLine = -2 then
        begin
          Ln := GetLineInfo(Line);
          h := gi.FBounds.Bottom - gi.FBounds.Top;
          Y := Y1;
          for j := 1 to Ln.LineCount - 1 do
            begin
              Inc(Y, Ln.Heights[j - 1]);
              IL.Draw(cvs, gi.FBounds.Left, Y + (Ln.Heights[j] - h) div 2, gi.ImageIndex);
            end;
        end
      else
        IL.Draw(cvs, gi.FBounds.Left, gi.FBounds.Top - YTop + Y1, gi.ImageIndex);
   end;

  // Draw tree icon
  if (LineColState > csOutCollapse) and (FGutter.ShowCollapseLine) then
    begin
     R := FGutter.ExpBtnRect(Y1, Y2);
     cvs.Pen := FGutter.CollapsePen;
     R.Left := (R.Left + R.Right) div 2;
     if (LineColState >= 0) and ((Line = 0) or (IsLineCollapsed(Line - 1) = csOutCollapse))
      then cvs.MoveTo(R.Left, (Y1 + Y2) div 2)
      else cvs.MoveTo(R.Left, Y1);
     cvs.LineTo(R.Left, Y2);
     if LineColState = -1 then
      begin
       cvs.MoveTo(R.Left, Y2 - cvs.Pen.Width);
       cvs.LineTo(R.Right, Y2 - cvs.Pen.Width);
      end;
    end;
  if LineColState >= csCollapsible then
   begin
     if FGutter.ExpandButtons.Empty then
      begin
       if LineColState = 0 then S := '-' else S := '+';
       with FGutter.ExpBtnRect(Y1, Y2) do
         cvs.TextOut(Left, Top, S);
      end else
      begin
       R := FGutter.ExpBtnRect(Y1, Y2);
       w := R.Right - R.Left;
       h := R.Bottom - R.Top;
       cvs.BrushCopy(R, FGutter.ExpandButtons, Bounds(LineColState*w, 0, w, h),
          FGutter.ExpandButtons.Canvas.Pixels[0, h - 1]);
      end;
   end;

  if buf then Canvas.Draw(0, YTop, FGutter.FBuffer)
   else Canvas.Brush.Style := bsSolid;
end;

procedure TCustomSyntaxMemo.GetGutterImage(Line: integer; List: TList);
begin
  if Assigned(FOnGetGutterImage) then
    FOnGetGutterImage(Self, Line, FLineInfos);
end;

procedure TCustomSyntaxMemo.GetLineInfos(Line: integer);
var i, j, Y1, Y2, _Y1, _Y2: integer;
    IL: TCustomImageList;
    gi: TCustomGutterObject;
    pn: TPoint;
    Ln: TLineInfo;
begin
  ClearLineInfos;
  Ln := GetLineInfo(Line);
  if (Ln.LineCount > 1) and (FGutter.LineBreakObj >= 0) and (FGutter.LineBreakObj < FGutter.Objects.Count) then
    begin
      gi := FGutter.Objects[FGutter.LineBreakObj];
      gi.SubLine := -2;
      FLineInfos.Add(gi);
    end;

  for i := 0 to Gutter.Objects.Count - 1 do
   if Gutter.Objects[i].CheckLine(Line) then
    FLineInfos.Add(Gutter.Objects[i]);
  for i := 0 to FBookmarks.Count - 1 do
   if FBookmarks[i].Line = Line then
    FLineInfos.Add(FBookmarks[i]);
  GetGutterImage(Line, FLineInfos);


  LineBound(Line, Y1, Y2);
  for i := 0 to FLineInfos.Count - 1 do
   begin
    gi := TCustomGutterObject(FLineInfos[i]);
    if gi.ImageList <> nil then IL := gi.ImageList
     else IL := FGutter.Images;
    if Assigned(IL) and (gi.ImageIndex >= 0) and (gi.ImageIndex < IL.Count) then
     begin
      pn := FGutter.GetBandBound(gi.Band);
      if gi.Margin = -1 then
        begin // Automatic positioning within single band
         Inc(pn.X, 1);
         for j := i + 1 to FLineInfos.Count - 1 do
          with TCustomGutterObject(FLineInfos[j]) do
           if (Band = gi.Band) and
              (Margin = -1) and
              ((SubLine = gi.SubLine) or (gi.SubLine = -2) and (SubLine > 0))
              then
            if ImageList <> nil then Inc(pn.X, ImageList.Width + 1) else
             if FGutter.Images <> nil then Inc(pn.X, FGutter.Images.Width + 1);
        end
       else Inc(pn.X, gi.Margin);

      if (gi.SubLine >= 0) and (gi.SubLine < Ln.LineCount) then
        begin
          _Y1 := Y1;
          for j := 0 to gi.SubLine - 1 do
            Inc(_Y1, Ln.Heights[j]);
          _Y2 := _Y1 + Ln.Heights[gi.SubLine];
        end else
        begin
          _Y1 := Y1;
          _Y2 := Y2;
        end;

      gi.FBounds := Bounds(pn.X, (_Y2 + _Y1 - IL.Height) div 2, IL.Width, IL.Height)
     end else SetRectEmpty(gi.FBounds);
   end;
end;

procedure TCustomSyntaxMemo.ClearLineInfos;
var i: integer;
begin
  for i := 0 to FLineInfos.Count - 1 do
   if (TCustomGutterObject(FLineInfos[i]).Collection = nil) and
      not (TObject(FLineInfos[i]) is TBookmark) then
    TObject(FLineInfos[i]).Free;
  FLineInfos.Clear;
end;

procedure TCustomSyntaxMemo.Paint;
var Line, RM, cr, LineColState, tmp, i, subfirst, DefCharWidth: integer;
    Clip, R, LnOffs: TRect;
    lb: TLineBreakRange;
    bg: TColor;
    NeedGutter: Boolean;
    FTempRgn: HRGN;
    Info: TLineInfo;

  procedure DrawRightMargin(y1, y2: integer);
  var RM: integer;
      i: integer;
  begin
    SetBrushOrgEx(Canvas.Handle, 0, 0, nil);
    for i := 0 to FTextMargins.Count - 1 do
      if FTextMargins[i].Visible then
        begin
          RM := FTextMargins[i].Position;
          Canvas.Pen := FTextMargins[i].Pen;
          if Assigned(FOnGetRightMargin) then
            FOnGetRightMargin(FTextMargins[i], Line, RM, Canvas.Pen);
          if FTextMargins.AbsolutePos then
            RM := FMargin.Left + RM - ScrollPosX * DefCharWidth
          else
            RM := FMargin.Left + (RM - ScrollPosX) * DefCharWidth;
          Canvas.MoveTo(RM, y1);
          Canvas.LineTo(RM, y2);
        end;
  end;

  procedure DrawBreakLine(Y: integer; Color: TColor; Pen: TPen = nil);
  begin
    if Assigned(Pen) then
      Canvas.Pen := Pen
    else
      begin
        Canvas.Pen.Color := Color;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Width := 1;
      end;
    Canvas.MoveTo(FMargin.Left, Y);
    Canvas.LineTo(ClientWidth - FMargin.Right, Y);
  end;

  {$IFNDEF EC_REG}
  procedure DrawSharewareLabel;
  var R: TRect;
  const lb: string = 'NOT REGISTERED'#13#10'www.econtrol.ru';
  begin
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 8;
    Canvas.Font.Color := clNavy;
    Canvas.Font.Style := [];
    Canvas.Brush.Color := clInfoBk;
    Canvas.Brush.Style := bsSolid;
    SetRectEmpty(R);
    DrawText(Canvas.Handle, ecPChar(lb), -1, R, DT_CALCRECT or DT_CENTER);
    OffsetRect(R, ClientWidth - R.Right - 2, ClientHeight - R.Bottom - 2);
    DrawText(Canvas.Handle, ecPChar(lb), -1, R, DT_CENTER);
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
  end;
  {$ENDIF}

  procedure InvBgControl(Ctl: TWinControl; R: TRect);
  var i: integer;
  begin
    if Ctl = Self then Exit;
    for i := 0 to Ctl.ControlCount - 1 do
      if Ctl.Controls[i] is TWinControl then
        InvBgControl(Ctl.Controls[i] as TWinControl, R);
    MapWindowPoints(Handle, Ctl.Handle, R, 2);
    InvalidateRect(Ctl.Handle, @R, True);
    Ctl.Update;
  end;

var SaveDC, TmpDC: HDC;
begin
//  if FUpdateCount <> 0 then
//    Exit;
//AT
  if FUpdateCount <> 0 then
  begin
    Canvas.Brush.Color:= Self.Color;
    Canvas.FillRect(ClientRect);
    Canvas.Font.Name:= 'Tahoma';
    Canvas.Font.Size:= 9;
    Canvas.Font.Color:= Self.Font.Color;
    Canvas.TextOut(10, 10, 'Not ready...');
    Exit;
  end;

  Clip := Canvas.ClipRect;

  if FTransparent then
    begin
      R := Rect(FMargin.Left - FTextMargin, FMargin.Top, ClientWidth - FMargin.Right, ClientHeight - FMargin.Bottom);
      IntersectRect(R, Clip, R);

      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOREDRAW or SWP_NOSENDCHANGING or SWP_NOMOVE or SWP_NOSIZE);
      InvBgControl(Parent, R);
//      MapWindowPoints(Handle, Parent.Handle, R, 2);
//      InvalidateRect(Parent.Handle, @R, True);
//      Parent.Update;
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOREDRAW or SWP_NOSENDCHANGING or SWP_NOMOVE or SWP_NOSIZE);
      // Get new handle to remove predefined clipping
      SaveDC := Canvas.Handle;
      Canvas.Handle := GetDC(Handle);
      IntersectClipRect(Canvas.Handle, Clip.Left, Clip.Top, Clip.Right, Clip.Bottom);
    end
  else SaveDC := 0;

  FAnimation.Reset;
  {$IFNDEF EC_REG}
  DrawShareWareLabel;
  {$ENDIF}

  // Смещение текста относительно начала
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(FMargin.Left - FTextMargin, FMargin.Top, FMargin.Left, ClientHeight - FMargin.Bottom));
  if FGutter.Visible and FGutter.ShowSeparator then
   begin
    // Left border of gutter
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := FGutter.SeparatorColor;
    RM := FMargin.Left - FTextMargin - 1;
    Canvas.MoveTo(RM, FMargin.Top);
    Canvas.LineTo(RM, ClientHeight - FMargin.Bottom);
    dec(RM);
    Canvas.Pen.Color := Color;
    Canvas.MoveTo(RM, FMargin.Top);
    Canvas.LineTo(RM, ClientHeight - FMargin.Bottom);
   end;
  if (FHorzRuler.Height > 0) and (FHorzRuler.Visible) and (Clip.Top < FHorzRuler.Height) then
    FHorzRuler.Paint(Canvas);
  NeedGutter := Clip.Left <= FMargin.Left;
  IntersectClipRect(Canvas.Handle, FMargin.Left, FMargin.Top,
                    ClientWidth - FMargin.Right, ClientHeight - FMargin.Bottom);

  FTempRgn := CreateRectRgn(0,0,0,0);
  cr := GetClipRgn(Canvas.Handle, FTempRgn);
  if not FBackGround.IsDefault or DoubleBuffered then
   begin
    R := Rect(FMargin.Left, FMargin.Top, ClientWidth - FMargin.Right, ClientHeight - FMargin.Bottom);
    IntersectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    ApplyDefStyle(Canvas, -1);
    FBackGround.FillRect(Canvas, R, Canvas.Brush.Color);
   end;
try
  DefCharWidth := DefTextExt.cx;
  R := Rect(FMargin.Left - ScrollPosX * DefCharWidth, FMargin.Top, Clip.Right, FMargin.Top);

  Line := SkipInvisible( TopLine );
  while (R.Top < Clip.Bottom) and (Line < Lines.Count) and ((Line = 0) or FMultiLine) do
   begin
     AnalyzeToPos(Lines.LineIndex(Line) + Lines.LineSpace(Line) - 1); // 2.27
     Info := GetLineInfo(Line);
     if not Info.Hidden then
       begin
         if (Line = TopLine) and (FTopSubLine > 0) and (FTopSubLine < Info.LineCount) then
           subfirst := FTopSubLine
         else
           subfirst := 0;
         R.Bottom := R.Top;
         for i := subfirst to Info.LineCount - 1 do
           Inc(R.Bottom, Info.Heights[i]);

         if (R.Bottom > Clip.Top) and (R.Bottom > R.Top) then
         begin
          // Get gutter objects
          GetLineInfos(Line);
          // Get collapse state
          LineColState := IsLineCollapsed(Line);
          if (Line = Lines.Count - 1) and (LineColState = csInCollapse) then
            LineColState := csCollapseEnd;
          // Get line separator
          if Assigned(SyntObj) then lb := SyntObj.GetLineBreak(Line) else lb := nil;

          SetRectEmpty(LnOffs);

          if (LineColState = 1) and (GetColRangeTextAtPos(Line, tmp) = '') then // v2.02
           begin
             DrawBreakLine(R.Bottom - 1, CollapseBreakColor);  // v2.02
             LnOffs.Bottom := 1;
           end
          else
          if Assigned(lb) then
           begin
             if lb.Rule.UseCustomPen then
               begin
                 DrawBreakLine(R.Top, 0, lb.Rule.Pen);
                 LnOffs.Top := 1;
               end else
             if Assigned(lb.Rule.Style) and (lb.Rule.Style.BgColor <> clNone) then
               begin
                 DrawBreakLine(R.Top, lb.Rule.Style.BgColor);
                 LnOffs.Top := 1;
               end;
           end else
           begin
             bg := FUserRanges.GetLineBreak(Line);
             if bg <> clNone then
              begin
               DrawBreakLine(R.Top, bg);
               LnOffs.Top := 1;
              end;
           end;

          IntersectClipRect(Canvas.Handle, R.Left + LnOffs.Left, R.Top + LnOffs.Top, R.Right + LnOffs.Right, R.Bottom - LnOffs.Bottom);
          if Assigned(FOnBeforeLineDraw) then FOnBeforeLineDraw(Self, R, Line);
          ProcessLine(Canvas, Info, Line, ptDrawing, R, Point(subfirst, -1));
          if Assigned(FOnAfterLineDraw) then FOnAfterLineDraw(Self, R, Line);

          SelectClipRgn(Canvas.Handle, 0);
          if FGutter.Visible and NeedGutter then
           begin
             IntersectClipRect(Canvas.Handle, 0, R.Top, FGutter.Width, R.Bottom);
             DrawGutterLine(R.Top, R.Bottom{Min(R.Bottom, ClientHeight - FMargin.Bottom)}, Line, LineColState);
           end;

          case cr of
            0: SelectClipRgn(Canvas.Handle, 0);
            1: SelectClipRgn(Canvas.Handle, FTempRgn);
          end;

          if (Line = FCaretPos.Y) and (soDrawCurLineFocus in FOptions) and Focused then
             DrawFocusRect(Canvas.Handle, Rect(R.Left, R.Top, ClientWidth - FMargin.Right, R.Bottom));

          DrawRightMargin(R.Top, Min(R.Bottom, ClientHeight - FMargin.Bottom));
          ClearLineInfos;
         end;
         R.Top := R.Bottom;
       end;
     Line := SkipInvisible(Line + 1);
   end;
  if FBackGround.IsDefault and not DoubleBuffered then
   begin
    if Line >= Lines.Count then
       Canvas.Brush.Color := Color;
    if not PaintNoTextSpace(Canvas, Rect(R.Left, R.Top, ClientWidth - FMargin.Right, Clip.Bottom), -1) then
      if not Transparent then
        Canvas.FillRect(Rect(Clip.Left, R.Top, Clip.Right, Clip.Bottom));
   end;
  DrawRightMargin(R.Top, Clip.Bottom);
  // Fill gutter
  SelectClipRGN(Canvas.Handle, 0);
  if FGutter.Visible and NeedGutter then
    FillGutterBackground(-1, R.Top, Clip.Bottom, Canvas);
except
end;
DeleteObject(FTempRgn);
if SaveDC <> 0 then
  begin
    TmpDC := Canvas.Handle;
    Canvas.Handle := SaveDC;
    ReleaseDC(Handle, TmpDC);
  end;
{$IFDEF EC_DOTNET}
 {$IFDEF EC_VCL9}  // In Delphi 2005 there is a problem with disposing system resource
  System.GC.Collect(2);
 {$ENDIF}
{$ENDIF}
  //AT
  DrawScroll;
end;

procedure TCustomSyntaxMemo.InvalidateGutter;
var R: TRect;
begin
  if FGutter.Visible and HandleAllocated then
   begin
     R := Rect(0, 0, FMargin.Left, ClientHeight);
     InvalidateRect(Handle, {$IFNDEF EC_DOTNET}@{$ENDIF}R, False);
   end;
end;

//==============================================================================
//   Coordinate conversion
//==============================================================================

function TCustomSyntaxMemo.CaretPosToStrPos(const p: TPoint): integer;
begin
  if Assigned(FTextSource) then
    Result := FTextSource.CaretPosToStrPos(p)
  else
    Result := Lines.CaretToStr(p);
end;

function TCustomSyntaxMemo.StrPosToCaretPos(p: integer): TPoint;
begin
  if Assigned(FTextSource) then
    Result := FTextSource.StrPosToCaretPos(p)
  else
    Result := Lines.StrToCaret(p);
end;

procedure TCustomSyntaxMemo.SupressCaretChange;
begin
  FExternCaretChange := True;
end;

function TCustomSyntaxMemo.LineLength(Index: integer): integer;
begin
  if Assigned(FTextSource) then
    Result := FTextSource.LineLength(Index)
  else
    Result := Lines.LineLength(Index);
end;

function TCustomSyntaxMemo.AllowZeroTabAfter(C: ecChar): Boolean;
begin
  Result := (C <> #9) and not IsLineBreakChar(C);
end;

function TCustomSyntaxMemo.NextTab(LogK, InTextPos: integer): integer;
begin
  Result := TabList.NextTab(LogK, (soAllowZeroTab in FOptionsEx) and (InTextPos > 1) and AllowZeroTabAfter(Lines.Chars[InTextPos - 1]));
end;

procedure TCustomSyntaxMemo.ExpandTabs(var S: ecString; CurPos, LogK: integer);
var i, x, ki: integer;
begin
  i := 1;
  ki := 1;
  while i <= Length(S) do
    begin
      if S[i] = #9 then
        begin
          x := NextTab(LogK, CurPos + ki) - LogK;
          Delete(S, i, 1);
          inc(LogK, x);
          if x > 0 then
            Insert(StringOfChar(' ', x), S, i);
          inc(i, x);
        end else
        begin
          inc(LogK);
          inc(i);
        end;
      Inc(ki);
    end;
end;

// Position in line => logical position (due to tabs)
function TCustomSyntaxMemo.LinesPosToLogX(const S: ecString; XPos: integer): integer;
var i, N: integer;
begin
  Result := 0;
  N := min(XPos, Length(S));
  for i := 1 to N do
    if S[i] = #9 then Result := TabList.NextTab(Result, (soAllowZeroTab in FOptionsEx) and (i > 1) and AllowZeroTabAfter(S[i - 1]))
                 else inc(Result);
  if XPos > Length(S) then
    Inc(Result, XPos - Length(S));
end;

function TCustomSyntaxMemo.LinesPosToLog(Pos: TPoint): TPoint;
begin
  if (Pos.Y >= 0) and (Pos.Y < Lines.Count) then
    Result.X := LinesPosToLogX(Lines[Pos.Y], Pos.X)
  else
    Result.X := Pos.X;
  Result.Y := Pos.Y;
end;

// Position in line <= logical position (due to tabs)
function TCustomSyntaxMemo.LogToLinesPos(const Pos: TPoint): TPoint;
var S: ecString;
    p: integer;
begin
  Result := Pos;
  if (Pos.Y >= 0) and (Pos.Y < Lines.Count) then
    begin
      S := Lines[Pos.Y];
      Result.X := 0;
      p := 0;
      while Result.X < Length(S) do
        begin
          if S[Result.X + 1] <> #9 then inc(p)
            else p := TabList.NextTab(p, (soAllowZeroTab in FOptionsEx) and (Result.X > 0) and AllowZeroTabAfter(S[Result.X]));
          if p > Pos.X then Exit;
          inc(Result.X);
        end;
      inc(Result.X, Pos.X - p);
    end;
end;

procedure TCustomSyntaxMemo.WMKillFocus(var Message: TWMKillFocus);
begin
  if not (eoShowCaretWhenUnfocused in FOptionsEx) then
    FCaret.Hide
  else
    FCaret.FIsShown := False;
  ResetHint;
  IncSearchStop;
  if [soHideSelection, soHideDynamic] * FOptions <> [] then Invalidate;
  inherited;
end;

procedure TCustomSyntaxMemo.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if [soHideSelection, soHideDynamic] * FOptions <> [] then Invalidate;
  if soAutoSelect in FOptions then SelectAll;
  UpdateCaretPos;
end;

procedure TCustomSyntaxMemo.UpdateCaretPos;
var p: TPoint;
begin
  if (FUpdateCount <> 0) or not HandleAllocated then Exit;
  if FCaretPos.Y < TopLine then FCaret.Hide else begin
      p := MouseToCaret(FMargin.Left, ClientHeight);
      if FCaretPos.Y > p.Y then FCaret.Hide else begin
          p := CaretToMouse(FCaretPos.X, FCaretPos.Y);
          if p.X < FMargin.Left then FCaret.Hide
            else FCaret.Show(p);
      end;
    end;
end;

//==============================================================================
//  Mouse & Keyboard handlers
//==============================================================================
procedure TCustomSyntaxMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var p, Y1, Y2, ti, i: integer;
    pt: TPoint;
    R : TRect;
    iObj: IecSyntMemoPlugIn;
    b: Boolean;
begin
  //AT
  if FDoScroll then
  begin
    FDoScroll:= false;
    FTimerScroll.Enabled:= false;
    Cursor:= crIBeam;
    Invalidate;
    Exit
  end;
  if Button = mbMiddle then
  begin
    FDoScroll:= true;
    FDoScrollOrg:= Point(X, Y);
    FTimerScroll.Enabled:= true;
    Cursor:= crSizeNS;
    Invalidate;
    Exit
  end;

  if not Focused then
   begin
     SetFocus;
     if not Focused then
      Windows.SetFocus(Handle);  // Fixing ID32
   end;
  IncSearchStop;
  ResetHint;

  pt := MouseToCaret(X, Y);
  if FHorzRuler.Visible and (Y < FHorzRuler.Height) then
    begin
      FDraggedTextMargin := IsOverRightMargin(X, False);
      if Assigned(FDraggedTextMargin) and
         (not FDraggedTextMargin.RulerMark or
          not FDraggedTextMargin.AllowDrag or
          not FTextMargins.AllowDrag) then
        FDraggedTextMargin := nil
      else
        Exit;
    end else
  if X < FMargin.Left then
   begin
    LineBound(pt.y, Y1, Y2);
    R := FGutter.ExpBtnRect(Y1, Y2);
    b := PtInRect(R, Point(X, Y));
    if b then
      if ssCtrl in Shift then
        b := ToggleCollapseChildren(pt.Y)
      else
        b := ToggleCollapse(pt.Y);
    if not b then
      GutterClick(pt.Y, Button, Shift, Point(X, Y));
    Exit;
   end;

  if ((ssDouble in Shift) or (ssCtrl in Shift)) and (Button = mbLeft) and
     IsOverCollapsedTextIcon(X, Y) then
   begin
     ToggleCollapse(pt.Y);
     Exit;
   end;

  p := CaretPosToStrPos(pt);
  if Assigned(SyntObj) then
   begin
    ti := SyntObj.TokenAtPos(p);
    if (ti <> -1) and (SyntObj.Tags[ti].Rule is TTokenRule) and
       Assigned(TTokenRule(SyntObj.Tags[ti].Rule).OnMouseClick) then
      TTokenRule(SyntObj.Tags[ti].Rule).OnMouseClick(Self, ti, Button, Shift);
   end;

  if (soRightClickMoveCaret in FOptionsEx) and (Button = mbRight) and not PosInSelection(pt) then
    IntSetCaretPos(pt);

  inherited;

  if Button = mbLeft then
    begin
      // Двойной щелчок мышкой
      if ssDouble in Shift then // Выбираем слово
       begin
        if GetTickCount - FLastDblClick < GetDoubleClickTime then
          begin // 4 click
            Exclude(Shift, ssDouble);
            FLastDblClick := 0;
          end else
          begin // 2 click
            FLastDblClick := GetTickCount;
            if not (soDoubleClickLine in FOptions) then
             begin
              CaretPos := pt;
              SelectWord;
              StartTextSelection(pt, Shift, true);

              //AT- added for fixing issue with double-click
              if not (soAllowSelectByWords in OptionsEx) then
                FDragging := False;
             end
            else
             if pt.Y < Lines.Count then // Выбираем строку
               SelectLine(pt.Y);
            Exit;
          end
       end else
      if GetTickCount - FLastDblClick < GetDoubleClickTime then // 3 click
        if not (soDoubleClickLine in FOptions) then
         begin
          SelectLine(pt.Y);
//          FLastDblClick := 0;
          Exit;
         end;

      if ssCtrl in Shift then
       begin
        FDragStaple := IsOverStaple(X, Y);
        if FDragStaple <> nil then  Exit;
       end;

      if (ssCtrl in Shift) or not FTextMargins.DragWithCtrl then
        begin
          FDraggedTextMargin := IsOverRightMargin(X);
          if Assigned(FDraggedTextMargin) then
            if FDraggedTextMargin.AllowDrag and FTextMargins.AllowDrag then
              Exit
            else
              FDraggedTextMargin := nil;
        end;

      FDragText := (soDragText in FOptions) and not (ssShift in Shift) and
                   PosInSelection(pt) {and not ReadOnly};
                   
      if not FDragText then
        StartTextSelection(pt, Shift)
      else
        begin
         FScrollTimer.Enabled := True;
         //AT
         //bug: DragTypeOLE happens for normal block dragging!
         {
         if FValidOle and FDragTypeOLE then
         begin
           BeginOLEDrag;
           MessageBeep(0); 
         end  
         else
         }
           BeginDrag(True);
        end;
    end;

  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecSyntMemoPlugIn, iObj) then
    if iObj.HandleMouse(Button, Shift, X, Y) then Exit;
end;

procedure TCustomSyntaxMemo.StartTextSelection(pt: TPoint; Shift: TShiftState; selWords: Boolean);
begin
  FDragging := True;
  FDragSelWord := selWords and (soAllowSelectByWords in OptionsEx); //AT
  FScrollTimer.Enabled := True;
  if selWords then //AT
  begin
    FDragPosCaret := SkipHidden(pt.X, pt.Y, False);
    FDragPos := CaretPosToStrPos(pt);
    Exit;
  end;
  if not (ssShift in Shift) then
    begin
     ResetSelection;
     FDragPosCaret := SkipHidden(pt.X, pt.Y, False);
     FDragPos := CaretPosToStrPos(pt);
    end;

  SelectMode := GetMouseSelMode(Shift);
  case SelectMode of
    msColumn: ExecCommand(smColSelGotoXY, {$IFNDEF EC_DOTNET}@{$ENDIF}pt);
    msNone:   ExecCommand(smGotoXY, {$IFNDEF EC_DOTNET}@{$ENDIF}pt);
    msLine:   begin end; //AT - fix for non-activated Line sel mode
    else      ExecCommand(smSelGotoXY, {$IFNDEF EC_DOTNET}@{$ENDIF}pt);
  end;
end;

function TCustomSyntaxMemo.IsOverStaple(X, Y: Integer): TBlockStaple;
var i, xp, Line: integer;
    List: TList;
begin
  Result := nil;
  if not Assigned(SyntObj) or (Lines.Count = 0) then Exit;
  Line := MouseToCaret(X, Y).Y;
  if (Line < 0) or (Line >= Lines.Count) then Exit;
  xp := X - FMargin.Left;
  if ScrollPosX > 0 then
   xp := xp + ScrollPosX * DefTextExt.cx;

  List := TList.Create;
  try
    SyntObj.GetStaples(Line, List);
    for i := 0 to List.Count - 1 do
     with TBlockStaple(List[i]) do
      if (XPos <> -1) and
         (xp <= XPos - FStapleOffset + 1) and
         (xp >= XPos - FStapleOffset - 1) then
       begin
         Result := TBlockStaple(List[i]);
         Exit;
       end;
  finally
    List.Free;
  end;
end;

function TCustomSyntaxMemo.IsOverRightMargin(X: Integer; OnlyVisible: Boolean): TecTextMargin;
  function TestPosition(p: integer): Boolean;
  begin
    if FTextMargins.AbsolutePos then
      Result := abs(FMargin.Left + p - ScrollPosX * DefTextExt.cx - X) < 3
    else
      Result := abs(FMargin.Left + (p - ScrollPosX) * DefTextExt.cx - X) < 3;
  end;
var i: integer;
begin
  for i := FTextMargins.Count - 1 downto 0 do
    if (FTextMargins[i].Visible or not OnlyVisible) and TestPosition(FTextMargins[i].Position) then
      begin
        Result := FTextMargins[i];
        Exit;
      end;
  Result := nil;
end;

procedure TCustomSyntaxMemo.UpdateCursor;
var p: TPoint;
    Shift: TShiftState;
begin
  try
    p := ScreenToClient(Mouse.CursorPos);
    if PtInRect(ClientRect, p) then
      begin
        Shift := [];
        if GetKeyState(VK_MENU) < 0 then Include(Shift, ssAlt);
        if GetKeyState(VK_SHIFT) < 0 then Include(Shift, ssShift);
        if GetKeyState(VK_CONTROL) < 0 then Include(Shift, ssCtrl);
        MouseMove(Shift, p.X, p.Y);
      end;
  except
  end;
end;

procedure TCustomSyntaxMemo.SetCurs(cur: TCursor);
begin
  Windows.SetCursor(Screen.Cursors[cur]);
  inherited Cursor := cur;
end;

procedure TCustomSyntaxMemo.DoDragMargin(Margin: TecTextMargin; Pos: integer);
begin
  if Assigned(FOnDragTextMargin) then
    FOnDragTextMargin(Self, Margin, Pos);
  Margin.Position := Pos;
end;

//procedure TCustomSyntaxMemo.TextMarginAtPos(X)

procedure TCustomSyntaxMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var C: TCursor;
    i, p: integer;
    go: TCustomGutterObject;
    tm: TecTextMargin;

  function CanDragMargin: Boolean;
  begin
    tm := IsOverRightMargin(X);
    Result := Assigned(tm) and tm.AllowDrag and FTextMargins.AllowDrag and
             ((ssCtrl in Shift) or not FTextMargins.DragWithCtrl);
  end;
begin
  inherited;
  if FDragging then
    begin
      if PtInRect(TextArea, Point(x, y)) then
        IntSetCaretPos(MouseToCaret(X, Y)) else
      if Gutter.MouseMoveCaretAt(X) then
        IntSetCaretPos(MouseToCaret(FMargin.Left, Y));
    end else
  if Assigned(FDraggedTextMargin) then
    begin
      if FTextMargins.AbsolutePos then
        p := ScrollPosX * DefTextExt.cx + X - FMargin.Left
      else
        p := ScrollPosX + Round((X - FMargin.Left) / DefTextExt.cx);
      DoDragMargin(FDraggedTextMargin, Max(0, p));
    end else
  if FDragStaple <> nil then SetCurs(crHSplit) else
  if not FDragText then
    begin
      if FHorzRuler.Visible and (Y < FHorzRuler.Height) and (TextMargins.Count > 0) then
        begin
          tm := IsOverRightMargin(X, False);
          if Assigned(tm) and tm.RulerMark and tm.AllowDrag and FTextMargins.AllowDrag then
            C := crHSplit
          else
            C := crDefault;
        end else
      if X < FMargin.Left then
        begin
          go := GutterObjectAt(Point(X, Y));
          if Assigned(go) then
            C := go.Cursor
          else
            begin
              C := FGutter.Cursor;
              i := Gutter.BandAt(X);
              if i <> -1 then
                C := Gutter.Bands[i].Cursor;
            end;
        end else
      if not FReadOnly and (IsOverStaple(X, Y) <> nil) and (ssCtrl in Shift)
         or CanDragMargin
        then C := crHSplit
        else C := GetCursor(X, Y, Shift);
     SetCurs(C);
     // Test hint
     if not PtInRect(FHintSense, Point(x, y)) then
      begin
       ResetHint;
       FHintTimer.Enabled := True;
      end;
   end;
end;

procedure TCustomSyntaxMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Indent, sLine, eLine: integer;
//    p: TPoint;
begin
  inherited;
  ResetHint;
  FDragging := False;
  FDragSelWord := False;
  FDraggedTextMargin := nil;
  FScrollTimer.Enabled := False;

  //AT
  //turn off auto-scroll, only if we have dragging
  if (Button = mbMiddle) and
    FDoScroll and
    (Sqr(X - FDoScrollOrg.X) + Sqr(Y - FDoScrollOrg.Y) >= 25) then
  begin
    FDoScroll:= false;
    FTimerScroll.Enabled:= false;
    Cursor:= crIBeam;
    Invalidate;
    Exit
  end;

  if FDragStaple <> nil then
   begin
     Indent := DefTextExt.cx;
     Indent := Round((X - FMargin.Left - FDragStaple.XPos) / Indent + ScrollPosX);
     SyntObj.GetStapleLines(FDragStaple, sLine, eLine);
     IndentLines(sLine, eLine, Indent, 0, False);
     FDragStaple := nil;
   end;
   
(*  if FDragText then
   begin
     FDragText := False;
     p := MouseToCaret(X, Y);
     if PosInSelection(p) then
      begin
       ResetSelection;
       IntSetCaretPos(p);
      end
{       begin
         SelectMode := msNormal;
         FDragPos := CaretStrPos;
         IntSetSelection(FDragPos, 0);
       end}
     else
       MoveText(p , ssCtrl in Shift);
   end;                               *)
end;

procedure TCustomSyntaxMemo.DoDragScroll(Sender: TObject);
const ScrollDist = 30;
var p: TPoint;
    bx, by, bxin, byin : Boolean;
begin
  if {$IFDEF EC_VCL6_UP}
       not Mouse.IsDragging
     {$ELSE}
      (Mouse.Capture = 0) // Delphi 5
     {$ENDIF}
      and not FDragging then
    begin
      FScrollTimer.Enabled := False;
      Exit;
    end;
  p := ScreenToClient(Mouse.CursorPos);
  if FDragText then
    begin
      with TextArea do
       begin
         bxin := (p.X >= Left) and (p.X <= Right);
         byin := (p.Y >= Top) and (p.Y <= Bottom);
         bx := (p.X < Left) and (Left - p.X < ScrollDist) or
               (p.X > Right) and (p.X - Right < ScrollDist);
         by := (p.Y < Top) and (Top - p.Y < ScrollDist) or
               (p.Y > Bottom) and (p.Y - Bottom < ScrollDist);
         if bx and (by or byin) or by and (bx or bxin) then
           IntSetCaretPos(MouseToCaret(p.X, p.Y));
       end;
    end else
  if not PtInRect(TextArea, p) then
    IntSetCaretPos(MouseToCaret(p.X, p.Y));
end;

// Returns gutter object, but without objects defined by event.
function TCustomSyntaxMemo.GutterObjectAt(const XY: TPoint): TCustomGutterObject;
var i, L, Y: integer;
begin
  Result := nil;
  if XY.X < FGutter.Width then
    begin
      Y := XY.Y;
      L := LineAtPos(Y);
      GetLineInfos(L);
      try
        for i := 0 to FLineInfos.Count - 1 do
          begin
            Result := TCustomGutterObject(FLineInfos[i]);
            if not IsRectEmpty(Result.FBounds) and PtInRect(Result.FBounds, XY) then
              begin
                if not Assigned(Result.Collection) then
                  Result := nil;
                Exit;
              end;
          end;
      finally
        ClearLineInfos;
      end;
    end;
end;

procedure TCustomSyntaxMemo.GutterClick(Line: integer; Button: TMouseButton; Shift: TShiftState; XY: TPoint);
var i: integer;
    go: TCustomGutterObject;
begin
  GetLineInfos(Line);
  try
    for i := 0 to FLineInfos.Count - 1 do
     begin
      go := TCustomGutterObject(FLineInfos[i]);
      if not IsRectEmpty(go.FBounds) and PtInRect(go.FBounds, XY) then
       begin
        // OnClick processing
        if Button = mbLeft then
         begin
          if Assigned(go.OnClick) then
            begin
              go.OnClick(go, Line, Shift);
              Exit;
            end
         end else
        // Popup menu processing
        if Button = mbRight then
         if Assigned(go.PopupMenu) then
          with ClientToScreen(XY) do
           begin
            go.PopupMenu.Popup(X, Y);
            Exit;
           end;
       end;
    end;

    if Assigned(FGutter.PopupMenu) and (Button = mbRight) then
      with ClientToScreen(XY) do FGutter.PopupMenu.Popup(X, Y)
    else
      begin
        if Assigned(FOnGutterClick) then
          FOnGutterClick(Self, Line, Button, Shift, XY);
        if (Button = mbLeft) and Gutter.MouseMoveCaretAt(XY.X) then
          StartTextSelection(MouseToCaret(FMargin.Left, XY.Y), Shift);
      end;

  finally
    ClearLineInfos;
  end;
end;

procedure TCustomSyntaxMemo.SetCaretPos(Value: TPoint);
var p, t, sl, el, pstrt, pend: integer;
    need_upd: integer;
  procedure InvalidateLines(L1, L2: integer);
  var R: TRect;
      Y1, Y2: integer;
  begin
    if L2 < L1 then
      begin
        Y1 := L2;
        L2 := L1;
        L1 := Y1;
      end;
    R.Left := 0;
    R.Right := ClientWidth;
    if L1 <= TopLine then R.Top := 0 else
      begin
        LineBound(L1, Y1, Y2);
        R.Top := Y1;
      end;
    if L2 > TopLine + VisibleLines then R.Bottom := ClientHeight - FMargin.Bottom else
      begin
        if (L2 > L1) or (L1 <= TopLine) then
          LineBound(L2, Y1, Y2);
        R.Bottom := Y2;
      end;
    InvalidateRect(Handle, {$IFNDEF EC_DOTNET}@{$ENDIF}R, False);
  end;
begin
  // Limit caret position
  need_upd := 0;

  if Value.X < 0 then Value.X := 0;
  if Value.Y < 0 then Value.Y := 0;

  if not (soVirtualCaretPos in FOptionsEx) then
    begin
      if Lines.Count = 0 then Value := Point(0, 0);
      if Value.Y >= Lines.Count then
       if Lines.Count > 0 then
         Value.Y := Lines.Count - 1;
    end;

  if not FMultiLine then
    Value.Y := 0;
  if soKeepCaretInText in FOptions then
   begin
     FKeepedXPos := -1;
     if (SelectMode <> msColumn) or FDragText or
        not FShiftDown and not FDragging or (soDisableSelection in FOptions) then
     if Value.Y < Lines.Count then Value.X := min(Value.X, LineLength(Value.Y))
      else Value.X := 0;
   end;

  Value := SkipHidden(Value.X, Value.Y, True);

  if (FCaretPos.X = Value.X) and (FCaretPos.Y = Value.Y) then
   begin
    ScrollCaret;
    if not FDragText and not (FShiftDown or FDragging) and FKeyNavigate
       and not (soPersistentBlocks in FOptions) then
      ResetSelection;
    Exit;
   end;

  // Full update required for dynamic highlighting
  if Assigned(SyntObj) and SyntObj.SetCurPos(CaretPosToStrPos(Value), Value.X > LineLength(Value.Y)) then
    need_upd := 3 else
  if (FCaretPos.Y <> Value.Y) and ((soDrawCurLineFocus in FOptions) or
     Assigned(SyntObj) and Assigned(SyntObj.Owner.CurrentLine) or
     DefaultStyles.CurrentLine.Enabled) then
    need_upd := 1;

  sl := FCaretPos.Y;
  el := Value.Y;

  FCaretPos := Value;
  ScrollCaret;

  // Change selection
  p := CaretStrPos;
  if not FDragText then
   if (FShiftDown or FDragging) and not (soDisableSelection in FOptions) then
     begin
      if need_upd < 2 then need_upd := 2;
      if SelectMode = msColumn then
       with FBlock do
        begin
         sl := Top;
         el := Bottom;
         FSelLength := 0;
         TopLeft := LinesPosToLog(FDragPosCaret);
         if Top > Lines.Count - 1 then
           Top := Max(Lines.Count - 1, 0);
         BottomRight := LinesPosToLog(FCaretPos);
         if Bottom > Lines.Count - 1 then
           Bottom := Max(Lines.Count - 1, 0);
         if Left > Right then
          begin t := Right; Right := Left;  Left := t; end;
         if Top > Bottom then
          begin t := Bottom; Bottom := Top; Top := t; end;
         if soGreedySelect in FOptions then
          Right := Right + 1;
         if Top < sl then sl := Top;
         if Bottom > el then el := Bottom;
         SelectionChanged;
       end
       else
        if SelectMode = msLine then SetLineSelection(min(p, FDragPos), abs(FDragPos - p), False)
         else
         begin
            pstrt := min(p, FDragPos);
            pend := max(p, FDragPos);
            if FDragSelWord then begin
                while (pstrt > 0) and not IsWordEdge(Lines.Chars[pstrt], Lines.Chars[pstrt + 1]) do dec(pstrt);
                while (pend + 1 < TextLength) and not IsWordEdge(Lines.Chars[pend], Lines.Chars[pend+1]) do inc(pend);
            end;
            IntSetSelection(pstrt, pend - pstrt, False);
         end;
     end
    else
     begin
       FDragPos := p;
       FDragPosCaret := FCaretPos;
       if FSelLength = 0 then FSelStart := p;
       if not (soPersistentBlocks in FOptions) and HaveSelection then
        begin
         ResetSelection;
         need_upd := 0;
         if SelectMode <> msColumn then FSelStart := p;
        end;
     end;

  if CaretPosChanged then need_upd := 3;
  if (FSearchMarks.Count > 0) and not (soKeepSearchMarks in FOptionsEx) then
    begin
      ResetSearchMarks;
      need_upd := 0;
    end;

  if HandleAllocated then
    begin
      case need_upd of
        1: begin
             InvalidateLines(sl, sl);
             if el <> sl then
               InvalidateLines(el, el);
           end;
        2: InvalidateLines(el, sl);
        3: Invalidate;
       else
        if (sl <> el) and LineNumbers.Visible then
          InvalidateGutter;
      end;
      UpdateCaretPos;
    end;
end;

procedure TCustomSyntaxMemo.IntSetSelection(sStart, sLength: integer; re_draw: Boolean);
var L: integer;
begin
  //AT
  //need to enable "normal" set selection for "columns" mode for 2 cases:
  //a) select all, b) select current word by dbl-click
  if SelectMode=msColumn then
    if ((sStart=0) and (sLength=TextLength)) or
      (StrPosToCaretPos(sStart).Y = StrPosToCaretPos(sStart+sLength).Y) then
    begin
      SelectMode := msNormal;
    end;

  L := Lines.TextLength;
  if sStart > L then sStart := L;
  if sLength + sStart > L then sLength := L - sStart;
  if soDisableSelection in FOptions then sLength := 0;
  if (sStart = FSelStart) and (sLength = FSelLength) then Exit;

  re_draw := re_draw and ((sLength > 0) or (FSelLength > 0));
  FSelStart := sStart;
  FSelLength := sLength;
  if re_draw then Invalidate;
  SelectionChanged;
end;

procedure TCustomSyntaxMemo.SetSelection(sStart, sLength: integer; DoNotMoveCaret: Boolean);
begin
  if not DoNotMoveCaret then
  begin
      CaretStrPos := sStart + sLength;
      FDragPos := sStart;
  end;
  if sLength < 0 then begin
      sStart := sStart + sLength;
      sLength := -sLength; 
  end;
  IntSetSelection(sStart, sLength);
end;

function TCustomSyntaxMemo.PosInSelection(aPos: integer): Boolean;
begin
  if SelectMode = msColumn then
    Result := PosInSelection(StrPosToCaretPos(aPos))
  else
    Result := (aPos >= FSelStart) and (aPos < FSelStart + FSelLength);
end;

function TCustomSyntaxMemo.PosInSelection(aCaretPos: TPoint): Boolean;
var sLeft, sRight:integer;
begin
  if SelectMode = msColumn then
   begin
     Result := (aCaretPos.Y >= FBlock.Top) and (aCaretPos.Y <= FBlock.Bottom);
     if Result then
      begin
       sLeft := LogToLinesPos(Point(FBlock.Left, aCaretPos.Y)).X;
       sRight := LogToLinesPos(Point(FBlock.Right, aCaretPos.Y)).X;
       Result := (aCaretPos.X >= sLeft) and (aCaretPos.X < sRight);
      end;
   end
  else
    Result := PosInSelection(CaretPosToStrPos(aCaretPos));
end;

procedure TCustomSyntaxMemo.SelectLine(Line: integer);
begin
  SelectLines(Line, Line);
end;

procedure TCustomSyntaxMemo.SelectLines(FirstLine, LastLine: integer);
var sp, ep: integer;
begin
  if LastLine > Lines.Count then
    LastLine := Lines.Count - 1;
  if LastLine < 0 then
    Exit;

  ShowLines(FirstLine, LastLine);
  CaretPos := Point(0, FirstLine);
  sp := CaretStrPos;
  ep := CaretPosToStrPos(Point(0, LastLine)) + Lines.LineSpace(LastLine);
  IntSetSelection(sp, ep - sp);
end;

procedure TCustomSyntaxMemo.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTMESSAGE;
  if FTabMode <> tmDialog then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

{$IFDEF EC_UNICODE_IN_ANSI}
procedure TCustomSyntaxMemo.WMKeyDown(var Message: TWMKey);
begin
  FScanCode := (Message.KeyData shr 16) and $FF;
  inherited;
end;
{$ENDIF}

procedure TCustomSyntaxMemo.KeyDown(var Key: Word; Shift: TShiftState);
var sc: TShortCut;
    cmd, i: integer;
    msg: tagMsg;
    iObj: IecSyntMemoPlugIn;

  {$IFDEF EC_UNICODE_IN_ANSI}
  function UnicodeKey(Key: Word): WideChar;
  var pKeyBuffer : TKeyboardState;
      x: LongInt;
      Buff: array[0..10] of WideChar;
  begin
    GetKeyboardState(pKeyBuffer);
    x := ToUnicode(Key, FScanCode, pKeyBuffer, Buff, 10, 0);
    if x >= 1 then Result := Buff[0] else
        begin
          Result := #0; //WideChar(Key);
          FScanCode := 0;          //aa: added to prevent . with alt-tab
        end;
  end;

  function AsciiKey(Key: Word): Integer;
  var pKeyBuffer : TKeyboardState;
      ScanCode, x: LongInt;
      Buff: array[0..2] of Char;
  begin
    GetKeyboardState(pKeyBuffer);
    ScanCode := 0;
    x := ToAsciiEx(Key, ScanCode, pKeyBuffer, Buff, 0, 0);
    Result := Key;
    if x = 1 then Result := Integer(Buff[0]);
    if x = 2 then Result := Integer(Buff[1]);
  end;
  {$ENDIF}

begin
  inherited;
  if Key = 0 then Exit;
  {$IFDEF EC_UNICODE_IN_ANSI}
  if not (ssAlt in Shift) then   // To handle ALT+#### combinations
    begin
      AsciiKey(Key); // somehow enables composed characters ("e -> ?)
      FUniChar := UnicodeKey(Key);
    end;
  {$ENDIF}
  if Key <> 0 then
  if FIncSearch then
    begin
      if Key = VK_BACK then
        begin
          if Length(FIncSearchStr) > 0 then
            IncSearchStr := Copy(FIncSearchStr, 1, Length(FIncSearchStr) - 1);
        end else
      if (Key <= VK_DELETE) and (Key <> VK_SPACE) and (Key <> VK_SHIFT) then IncSearchStop;
    end else
  if (Key = VK_SHIFT) or (Key = VK_CONTROL) or (Key = VK_MENU) then
    begin
      UpdateCursor;
    end else
    begin
     sc := Key;
     if ssShift in Shift then sc := sc or scShift;
     if ssCtrl in Shift then sc := sc or scCtrl;
     if ssAlt in Shift then sc := sc or scAlt;
     FKeyQueue.Add(sc);
     // Execute editor plug-in
     for i := 0 to FSyntClients.Count - 1 do
      if Supports(TObject(FSyntClients[i]), IecSyntMemoPlugIn, iObj) then
       if iObj.HandleKeyStroke(sc) then
        begin
          PeekMessage(msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
          Exit;
        end;
     // Select editor command
     if Assigned(FKeyMapping) then cmd := FKeyMapping.IsHandledCmd(FKeyQueue)
                              else cmd := DefaultKeyMapping.IsHandledCmd(FKeyQueue);
     if cmd <> 0 then
       begin
         ExecCommand(cmd);
         PeekMessage(msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
       end;
    end;
end;

procedure TCustomSyntaxMemo.KeyUp(var Key: Word; Shift: TShiftState);
var msg: tagMsg;
begin
  // Go around (Alt+Home, ...) key down
  // Process Alt+XXX only when NumLock is ON
  if (Key = VK_MENU) and ((GetKeyState(VK_NUMLOCK) and $7FFF) = 0)
     and not (soNotSuppressAltNNNN in FOptionsEx) then
    PeekMessage(msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
  if (Key = VK_SHIFT) or (Key = VK_CONTROL) or (Key = VK_MENU) then
    UpdateCursor;
  try
    inherited;
  except
  end;
end;

procedure TCustomSyntaxMemo.KeyPress(var Key: Char);
var WC: ecChar;
begin
  inherited;
  {$IFDEF EC_UNICODE_IN_ANSI}
  if (Key = #63) and (FUniChar <> #0) then WC := FUniChar
   else WC := ecKeyChar(Key);
  FUniChar := #0;
  {$ELSE}
  WC := Key;
  {$ENDIF}
  if FIncSearch then
  begin
   IncSearchStr := IncSearchStr + WC;
   Exit;
  end;
  ExecCommand(smChar, {$IFNDEF EC_DOTNET}@{$ENDIF}WC);
  if soHideCursorOnType in FOptions then
   with ScreenToClient(Mouse.CursorPos) do
    if (X >= FMargin.Left) and (X < ClientWidth - FMargin.Right) and
       (Y >= FMargin.Top) and (Y < ClientHeight - FMargin.Bottom) then
     Windows.SetCursor(0);
end;

procedure TCustomSyntaxMemo.CNKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode <> VK_DELETE then
    inherited;
end;

procedure TCustomSyntaxMemo.RecalcIMEWindow;
var IMEContext: HIMC;
begin
  IMEContext := ImmGetContext(Handle);
  if IMEContext <> 0 then
  try
    UpdateIMEWindow(IMEContext);
  finally
    ImmReleaseContext(Handle, IMEContext);
  end;
end;

// Set IME window position
procedure TCustomSyntaxMemo.UpdateIMEWindow(IMEContext: HIMC);
var CForm: TCompositionForm;
    RightBdr, WBpos: integer;
begin
  with CForm do
  begin
  //  if (CaretPos.Y = 0) and (CaretPos.X = 0) then Exit; //aa: prevent IME input juming to wrong pos
    dwStyle := CFS_RECT;//CFS_FORCE_POSITION;
    ptCurrentPos := CaretToMouse(CaretPos.X, CaretPos.Y);
    RightBdr := ClientWidth - FMargin.Right;
    if WordWrap and (soBreakOnRightMargin in Options) then
     begin
       WBpos := RightMargin * DefTextExt.cx + FMargin.Left;
       if WBpos < RightBdr then
         RightBdr := WBpos;
     end;
    rcArea := Rect(FMargin.Left, FMargin.Top, RightBdr + 1, ClientHeight - FMargin.Bottom);
  end;
  ImmSetCompositionWindow(IMEContext, {$IFNDEF EC_DOTNET}@{$ENDIF}CForm);
end;

procedure TCustomSyntaxMemo.WMImeComposition(var Msg: TMessage);
var IMEContext: HIMC;
    p: WideString;
    {$IFNDEF EC_UNICODE}
    S: AnsiString;
    {$ENDIF}
    FImeCount: integer;
   {$IFDEF EC_DOTNET}
    SBld: StringBuilder;
   {$ENDIF}
begin
  if (Msg.LParam and GCS_RESULTSTR) <> 0 then
  begin
    IMEContext := ImmGetContext(Handle);
    try
      FImeCount := ImmGetCompositionStringW(IMEContext, GCS_RESULTSTR, nil, 0) div 2;
      if FImeCount > 0 then
       begin
        SetLength(p, FImeCount);
        {$IFDEF EC_DOTNET}
        ImmGetCompositionStringW(IMEContext, GCS_RESULTSTR, SBld, FImeCount*2);
        p := SBld.ToString;
        {$ELSE}
        ImmGetCompositionStringW(IMEContext, GCS_RESULTSTR, PWideChar(p), FImeCount*2);
        {$ENDIF}

        {$IFDEF EC_UNICODE}
        ExecCommand(smString, PecChar(p));
        {$ELSE}
        S := UnicodeToAnsi(p, CharSet);
        ExecCommand(smString, PecChar(S));
        {$ENDIF}
        UpdateIMEWindow(IMEContext);
       end;
    finally
      ImmReleaseContext(Handle, IMEContext);
    end;
  end;
  inherited;
end;

procedure TCustomSyntaxMemo.WMIMEStartComp(var Message: TMessage);
var H: HIMC;
    LFont: TLogFont;
begin
  H := ImmGetContext(Handle);
  if H <> 0 then
  begin
    UpdateIMEWindow(H);
    // Set IME window font (context sensitive)
    SetCanvasAtPos(Canvas, CaretStrPos - 1);
    GetObject(Canvas.Font.Handle, SizeOf(TLogFont), {$IFNDEF EC_DOTNET}@{$ENDIF}LFont);
    ImmSetCompositionFont(H, {$IFNDEF EC_DOTNET}@{$ENDIF}LFont);
    ImmReleaseContext(Handle, H);
  end;
  inherited;
end;
{.$ENDIF}

//==============================================================================
//   Text modification
//==============================================================================
{
procedure TCustomSyntaxMemo.EndTextUpdate(ChangedLine: integer);
begin
  FDragging := False;
  UpdateEditor;
  CallIdleAnalysis;
end;}

function TCustomSyntaxMemo.HasReadOnly(Styles: TStyleEntries): boolean;
var i: integer;
begin
  Result := False;
  if (SyntObj <> nil) and (SyntObj.Owner.DefStyle <> nil) then
   with SyntObj.Owner.DefStyle do
    if ffReadOnly in FormatFlags then
     Result := ReadOnly;

  for i := 0 to Styles.Count - 1 do
   with Styles[i] do
    if ffReadOnly in Style.FormatFlags then
     Result := Style.ReadOnly;
end;

function TCustomSyntaxMemo.CanInsertText(Pos: integer): Boolean;
var Styles: TStyleEntries;
begin
    if ReadOnly then begin
        Result := False;
        Exit;
    end;

    //if not (soNoReadOnly in FOptionsEx) then begin //AT
        if Pos = 0 then Result := True else begin
            Styles := TStyleEntries.Create;
            try
                GetStyleList(Pos, Styles, slAllStyles, False);
                Result := not HasReadOnly(Styles);
                if not Result then begin
                    Styles.Clear;
                    GetStyleList(Pos - 1, Styles, slAllStyles, False);
                    Result := not HasReadOnly(Styles);
                end;
            finally
                Styles.Free;
            end;
        end;

    if Assigned(FOnCanInsert) then
        FOnCanInsert(Self, Pos, Result);
end;

function TCustomSyntaxMemo.CanDeleteText(Pos, Count: integer): Boolean;
var Styles: TStyleEntries;
    Next, tmp: integer;
begin
  Result := False;
  if ReadOnly then
    Exit;

  if Count = 0 then
   begin
    Result := True;
    Exit;
   end;

  Styles := TStyleEntries.Create;
  try
    Next := Pos;
    if Count > 0 then
      while Next < Pos + Count do
       begin
         tmp := GetStyleList(Next, Styles, slPrintable, False);
         if not HasReadOnly(Styles) then
          begin
           Result := True;
           Break;
          end;
         if tmp < Next then Break;
         Next := tmp;
         Styles.Clear;
       end;
  finally
    Styles.Free;
  end;
  if Assigned(FOnCanDelete) then
    FOnCanDelete(Self, Pos, Count, Result);
end;

// Deletion text withourt deleting text with "Read-only" flag
procedure TCustomSyntaxMemo.SafeDeleteText(Ranges: TList);
var i: integer;
    Styles: TStyleEntries;
    ResRanges: TRangeList;

  procedure ProcessSingleRange(StartPos, EndPos: integer);
  var Next, Cur, sp: integer;
      RO_state: Boolean;
  begin
    Cur := StartPos;
    sp := StartPos;
    RO_state := False;
    while Cur < EndPos do
     begin
       Next := GetStyleList(Cur, Styles, slPrintable, True);
       if HasReadOnly(Styles) then
         begin
           if not RO_State and (sp < Cur) then
             ResRanges.Add(TRange.Create(sp, Cur));
           RO_state := True;
         end else
         begin
           if RO_state then
             sp := Cur;
           RO_state := False;
         end;
       if Next <= Cur then Break; // no formatting
       Cur := Next;
       Styles.Clear;
     end;
    if not RO_state and (sp < EndPos) then
      ResRanges.Add(TRange.Create(sp, EndPos));
  end;

begin
  Styles := TStyleEntries.Create;
  ResRanges := TRangeList.Create;
  try
    for i := 0 to Ranges.Count - 1 do
      with TRange(Ranges[i]) do
        ProcessSingleRange(StartPos, EndPos);
    for i := ResRanges.Count - 1 downto 0 do
      with ResRanges[i] do
        begin
          FCaretPos := StrPosToCaretPos(StartPos);
          DeleteText(EndPos - StartPos);
        end;
  finally
    Styles.Free;
    ResRanges.Free;
  end;
end;

procedure TCustomSyntaxMemo.BeginUpdate;
begin
  if FUpdateCount = 0 then
    Lines.BeginUndoGroup;
  Inc(FUpdateCount);
end;

procedure TCustomSyntaxMemo.EndUpdate(doRefresh: Boolean);
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
   begin
    FUpdateCount := 0;
    Lines.EndUndoGroup;
    if doRefresh then
      begin
        FSyncEditing.Validate;
        UpdateEditor;
      end;
   end;
end;

procedure TCustomSyntaxMemo.UpdateEditor;
begin
  if FUpdateCount = 0 then
   begin
    AdjustScrollBar;
    UpdateCaretPos;
    Invalidate;
   end;
end;

function TCustomSyntaxMemo.CanClearSelection: Boolean;
var i, sLeft, sRight, N: integer;
begin
  Result := False;
  if not HaveSelection then Exit;
  if SelectMode = msColumn then
    begin
     for i := FBlock.Top to FBlock.Bottom do
     begin
      sLeft := LogToLinesPos(Point(FBlock.Left, i)).X;
      sRight := LogToLinesPos(Point(FBlock.Right, i)).X;
      N := LineLength(i);
      if N > sLeft then
        if CanDeleteText(CaretPosToStrPos(Point(sLeft, i)), min(N, sRight) - sLeft) then
          begin
            Result := True;
            Exit;
          end;
     end;
   end else
   if not CanDeleteText(FSelStart, FSelLength) then Exit;
  Result := True;
end;

procedure TCustomSyntaxMemo.IntDeleteText(APos, Count: integer);
begin
//  FIgnoreUpdate := True;
//  try
   FExternCaretChange := False;
   Lines.DeleteTxt(APos, Count);
//  finally
//   FIgnoreUpdate := False;
//  end;
end;

procedure TCustomSyntaxMemo.IntInsertText(APos: integer; s: ecString);
begin
//  FIgnoreUpdate := True;
//  try
   FExternCaretChange := False;
   Lines.InsertTxt(s, APos);
//  finally
//   FIgnoreUpdate := False;
//  end;
end;

// Synchronized inset/delete
function TCustomSyntaxMemo.SyncInsDelOper(var Pos: integer; Count: integer; S: ecString): Boolean;
var i, sync_pos, ch: integer;
    sync_list: TList;
    Del: ecString;
    isDel: Boolean;
begin
  if Count >= 0 then
    Count := Length(S);

  ch := Count;
  if Count < 0 then Inc(ch, Length(S));
  sync_list := FSyncEditing.GetSyncPositions(Pos, Count);
  Result := sync_list <> nil;
  if Result then
    try
     isDel := Count < 0;
     if isDel then Count := -Count;
     if Assigned(SyntObj) and (sync_list.Count > 0) then
       SyntObj.ChangedAtPos(integer(sync_list[0]));
     for i := sync_list.Count - 1 downto 0 do
       begin
         sync_pos := integer(sync_list[i]);
         if sync_pos < Pos then Inc(Pos, ch);
         if isDel then
           begin
             if DoBeforeDeleteText(sync_pos, Count) then
               begin
                 Del := Lines.SubString(sync_pos + 1, Count);
                 AddUndo(toDelete, '', Del, sync_pos, False);
                 IntDeleteText(sync_pos, Count);
               end;
           end;
         if (Length(S) > 0) and DoBeforeInsertText(sync_pos, S) then
           begin
             AddUndo(toInsert, S, '', sync_pos);
             IntInsertText(sync_pos, S);
           end;
       end;
    finally
      sync_list.Free;
    end
end;

procedure TCustomSyntaxMemo.DeleteText(Count: integer);
var aPos: integer;
    RightDel: Boolean;
    cp: TPoint;
  function CheckBreakPos(Pos: integer): Boolean;
  begin
   Result := (Pos < Lines.TextLength) and (Lines.Chars[Pos + 1] = #10) and
             (Pos > 0) and (Lines.Chars[Pos] = #13);
  end;

begin
  cp := FCaretPos;
  Inc(cp.X, Count);
  // Delete single object when there are no selection
  if Assigned(FEmbObjects) then
    begin
      if Count = 1 then
        aPos := FEmbObjects.ObjectAtCaret(CaretPos) else
      if Count = -1 then
        aPos := FEmbObjects.ObjectAtCaret(Point(FCaretPos.X - 1, FCaretPos.Y)) else
        aPos := -1;
      if aPos <> -1 then
        begin
          BeginUpdate;
          try
            FEmbObjects.Delete(aPos);
            ResetLineHeights(True, CaretPos.Y, CaretPos.Y);
            if Count < 0 then
              CaretPos := Point(FCaretPos.X + Count, FCaretPos.Y);
            Exit;
          finally
            EndUpdate;
          end;
        end;
    end;

  aPos := CaretStrPos;
  RightDel := Count > 0;
  if Count < 0 then
   begin
    if aPos = 0 then Exit;
    Count := -Count;
    if aPos < Count then
     begin
      Count := aPos;
      aPos := 0;
     end else Dec(aPos, Count);
   end;
  Count := min(Count, Lines.TextLength - aPos);
  if Count <= 0 then Exit;

  {$IFDEF EC_MBCS}
  if Lines.CharType(aPos + 1) = mbTrailByte then
    begin
      Dec(aPos);
      Inc(Count);
    end;
  if Lines.CharType(aPos + Count + 1) = mbTrailByte then
    Inc(Count);
  {$ENDIF}

  if not CanDeleteText(aPos, Count) then Exit;

  BeginUpdate;
  try
    if CheckBreakPos(Count + aPos) then Inc(Count);
    if CheckBreakPos(aPos) then begin   Inc(Count); Dec(aPos); end;

    if not SyncInsDelOper(aPos, -Count, '') and DoBeforeDeleteText(aPos, Count) then
      begin
       AddUndo(toDelete, '', Lines.SubString(aPos + 1, Count), aPos, not RightDel);
       IntDeleteText(aPos, Count);
      end;
    if not FExternCaretChange then
      CaretStrPos := aPos;
  finally
    EndUpdate;
  end;
end;

function TCustomSyntaxMemo.GetInsAddSpaces(const cp: TPoint): ecString;
var i, N: integer;
begin
  Result := '';
  if Lines.Count = 0 then N := 1
    else N := Lines.Count;

  if (cp.Y >= N) or
     (cp.X > LineLength(cp.Y)) then
    begin
      if cp.Y >= N then
        begin
          for i := N to cp.Y do
            Result := Result + sLineBreak;
          Result := Result + StringOfChar(' ', cp.X);
        end else
          Result := StringOfChar(' ', cp.X - LineLength(cp.Y));
    end;
end;

procedure TCustomSyntaxMemo.InsertText(s: ecString);
var aPos, i: integer;
    extStr: ecString;
begin
  if HaveSelection and not CanClearSelection or
     not CanInsertText(CaretStrPos) then
    Exit;

  if not FMultiLine then
    for i := 1 to Length(s) do
      if IsLineBreakChar(S[i]) then
        S[i] := ' ';

  BeginUpdate;
  try
    if HaveSelection and (soOverwriteBlocks in FOptions) then
        ClearSelection;
    if s <> sLineBreak then
      begin
        extStr := GetInsAddSpaces(FCaretPos);
        S := extStr + S;
      end
    else
      begin
        extStr := '';
        if (FCaretPos.Y >= Lines.Count) and (soVirtualCaretPos in FOptionsEx) then
          begin
            CaretPos := Point(0, FCaretPos.Y + 1);
            Exit;
          end;
      end;
    aPos := CaretStrPos;

    if (extStr <> '') or not SyncInsDelOper(aPos, 0, S) and DoBeforeInsertText(APos, S) then
     begin
        AddUndo(toInsert, S, '', aPos);
        IntInsertText(aPos, S);
     end;


    inc(aPos, Length(s));
    if not FExternCaretChange then
      CaretStrPos := aPos;
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.MoveText(toCaretPos: TPoint; Copying: Boolean);
var ip, ToPos, sLeft, sRight: integer;
    s: ecString;
    st: TecStringList;
    extStr: ecString;
    R: TRect;
begin
  if PosInSelection(toCaretPos) or
     (toCaretPos.Y < 0) or
     ((toCaretPos.Y >= Lines.Count) and not (soVirtualCaretPos in FOptionsEx)) or
     not HaveSelection or
     Copying and not CanClearSelection or
     not CanInsertText(CaretPosToStrPos(toCaretPos))
  then Exit;

  if (soKeepCaretInText in FOptions) and
     (toCaretPos.X > LineLength(toCaretPos.Y)) then
    toCaretPos.X := LineLength(toCaretPos.Y);

  BeginUpdate;
  try
    if SelectMode <> msColumn then
      begin
       extStr := GetInsAddSpaces(toCaretPos);
       ToPos := CaretPosToStrPos(toCaretPos);
       s := Lines.SubString(FSelStart + 1, FSelLength);
//       ip := toCaretPos.X - LineLength(toCaretPos.Y);
       if not Copying and DoBeforeDeleteText(FSelStart, FSelLength) then
        begin
          if ToPos > FSelStart then dec(ToPos, FSelLength);
          AddUndo(toDelete, '', s, FSelStart);
          IntDeleteText(FSelStart, FSelLength);
          FSelLength := 0;
        end;
//       if ip > 0 then s := StringOfChar(' ', ip) + s else ip := 0;
       S := extStr + S;
       if DoBeforeInsertText(ToPos, S) then
         begin
           AddUndo(toInsert, S, '', ToPos);
           IntInsertText(ToPos, S);
           ip := Length(extStr);
           Inc(ToPos, ip);
           SetSelection(ToPos, Length(s) - ip);
         end;
      end
    else
      begin
        if not Copying and (toCaretPos.Y >= FBlock.Top) and
            (toCaretPos.Y <= FBlock.Bottom) then
         begin
          sLeft := LogToLinesPos(Point(FBlock.Left, toCaretPos.Y)).X;
          sRight := LogToLinesPos(Point(FBlock.Right, toCaretPos.Y)).X;
          if toCaretPos.X > sRight then
            toCaretPos.X := toCaretPos.X - sRight - sLeft;
         end;
        R.Top := toCaretPos.Y;
        R.Bottom := R.Top + (FBlock.Bottom - FBlock.Top);
        R.Right := LogToLinesPos(Point(FBlock.Right, FBlock.Top)).X -
                   LogToLinesPos(Point(FBlock.Left, FBlock.Top)).X;

        st := TecStringList.Create;
        try
          st.Text := GetSelText;
          if not Copying then
            ClearSelection;
          InsertTextBlock(st, toCaretPos);
          CaretPos := Point(toCaretPos.X + Length(st[st.Count - 1]), toCaretPos.Y + st.Count - 1);
          R.Left := LinesPosToLog(toCaretPos).X;
          R.Right := LinesPosToLog(Point(toCaretPos.X + R.Right, toCaretPos.Y)).X;
          SelRect := R;
        finally
          st.Free;
        end;
      end;
  finally
    EndUpdate;
  end;
end;

function TCustomSyntaxMemo.ReplaceText(Pos, Count: integer; RepStr: ecString): Boolean;
var S: ecString;
begin
  Result := CanDeleteText(Pos, Count);
  if not Result then Exit;

  BeginUpdate;
  try
    if not SyncInsDelOper(Pos, -Count, RepStr) then
     begin
      S := Lines.SubString(Pos + 1, Count);
      if (Count > 0) and DoBeforeDeleteText(Pos, Count) then
        IntDeleteText(Pos, Count);
      if DoBeforeInsertText(Pos, RepStr) then
        begin
          IntInsertText(Pos, RepStr);
          AddUndo(toInsert, RepStr, S, Pos, False);
        end else
          AddUndo(toDelete, '', S, Pos, False); // in case when insert was forbidden
     end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.ConvertTabs(AStart, AEnd: integer);
var i, x: integer;
begin
  BeginUpdate;
  try
    if AEnd = -1 then AEnd := Lines.TextLength;
    for i := AEnd downto AStart do
     if Lines.Chars[i] = #9 then
      begin
        x := LinesPosToLog(StrPosToCaretPos(i)).X;
        x := NextTab(x, i) - x;
        ReplaceText(i - 1, 1, StringOfChar(' ', x));
      end;
  finally
    EndUpdate;
  end;
end;

function TCustomSyntaxMemo.InsertTextBlock(Block: TecStrings;
  CarPos: TPoint): integer;
var ins_text: ecString;
    i: integer;
begin
  BeginUpdate;
  try
    Result := 0;
    for i := 0 to Block.Count - 1 do
      begin
       ins_text := Block[i];
       if CarPos.Y >= Lines.Count then
        begin
         ins_text := GetInsAddSpaces(CarPos) {sLineBreak + StringOfChar(' ', CarPos.X)} + ins_text;
         if Lines.IsNullEnd then ins_text := ins_text + sLineBreak;
         Result := Lines.TextLength;
        end else
        begin
         ins_text := StringOfChar(' ', CarPos.X - LineLength(CarPos.Y)) + ins_text;
         Result := CaretPosToStrPos(CarPos);
        end;
       if CanInsertText(Result) and DoBeforeInsertText(Result, ins_text) then
         begin
           IntInsertText(Result, ins_text);
           AddUndo(toInsert, ins_text, '', Result);
           Inc(Result, Length(ins_text));
         end;
       inc(CarPos.Y);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.ClearSelection;
var i, N, p, sLeft, sRight: integer;
    R: TRect;
    List: TList;
begin
  if not CanClearSelection then Exit;
  BeginUpdate;
  List := TObjectList.Create;
  try
    if SelectMode = msColumn then
     begin
       R := FBlock;
       for i := R.Bottom downto R.Top do
       begin
        sLeft := LogToLinesPos(Point(R.Left, i)).X;
        sRight := LogToLinesPos(Point(R.Right, i)).X;
        N := LineLength(i);
        if N > sLeft then
         begin
           p := CaretPosToStrPos(Point(sLeft, i));
           List.Add(TRange.Create(p, p + min(N, sRight) - sLeft));
//          FCaretPos := Point(sLeft, i);
//          DeleteText(min(N, sRight) - sLeft);
         end;
       end;
     end else
     begin
       List.Add(TRange.Create(FSelStart, FSelStart + FSelLength));
//       FCaretPos := StrPosToCaretPos(FSelStart);
//       DeleteText(FSelLength);
     end;
    SafeDeleteText(List);
    ResetSelection;
  finally
    List.Free;
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.ResetSelection;
var b: Boolean;
begin
  if SelectMode = msColumn then
    b := not IsRectEmpty(FBlock)
  else
    b := FSelLength > 0;
  FSelLength := 0;
  SetRectEmpty(FBlock);
  FSelStart := CaretStrPos;
  FDragPos := FSelStart;
  if b then
   begin
    SelectionChanged;
    Invalidate;
   end;
end;

//==============================================================================
//   Selected block opeartions (clear, copy, cut, paste)
//==============================================================================

function TCustomSyntaxMemo.HaveSelection: Boolean;
begin
  if SelectMode = msColumn then
    with FBlock do Result := (Left < Right) and (Top <= Bottom)
  else
    Result := FSelLength > 0;
end;

procedure TCustomSyntaxMemo.Clear;
begin
  BeginUpdate;
  try
    Lines.Clear;
    ClearCustObject;
    CaretPos := Point(0,0);
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.CopyToClipboard;
var Exporter: TPlainTextSyntExport;
begin
  if HaveSelection then
   begin
     if soCopyAsRTF in FOptions then Exporter := TRTFSyntExport.Create(nil)
      else Exporter := TPlainTextSyntExport.Create(nil);
     try
       Exporter.SyntMemo := Self;
       Exporter.ExportSelection;
       Exporter.SaveToClipboard;
     finally
       Exporter.Free
     end;
   end;
end;

procedure TCustomSyntaxMemo.CutToClipboard;
begin
  CopyToClipboard;
  ClearSelection;
end;

procedure TCustomSyntaxMemo.PasteFromClipboard(AForceColumnBlock: boolean = false);
var ins_text: ecString;
    pt: TPoint;
    st: TecStringList;
    CurPos, EndPos: integer;
    PrevPos: TPoint;
begin
  BeginUpdate;
  try
    if Clipboard.HasFormat(CF_TEXT) then
      begin
        PrevPos := CaretPos; //AT
        if soSmartPaste in FOptionsEx then
          ins_text := GetClipboardTextEx(Charset)
        else
          ins_text := GetClipboardText(Charset);

        //AT
        //patch for considering Lines ends
        case Lines.TextFormat of
          tfCR: ReplaceStr(ins_text, #13#10, #13);
          tfNL: ReplaceStr(ins_text, #13#10, #10);
        end;

        CurPos := CaretStrPos;
        if (ins_text = '') or not CanInsertText(CurPos) then Exit;
        if (GetClipboardBlockType <> 2) and (not AForceColumnBlock) then //AT added AForceColumnBlock
        begin
          InsertText(ins_text);
          //AT commented
          //if TabMode <> tmTabChar then
          //  ConvertTabs(CurPos + 1, Length(ins_text));
        end
        else
          begin
           st := TecStringList.Create;
           st.Text := ins_text;
           try
            if HaveSelection and (soOverwriteBlocks in FOptions) then ClearSelection;
            pt := Point(CaretPos.X + (FBlock.Right - FBlock.Left), CaretPos.Y + st.Count - 1);
            EndPos := InsertTextBlock(st, CaretPos);
            CaretPos := pt;
            //AT commented
            //if TabMode <> tmTabChar then
            //  ConvertTabs(CurPos + 1, EndPos + 1);
           finally
            st.Free;
           end;
          end;

        //AT
        if soKeepCaretPaste in FOptionsEx then
          CaretPos := PrevPos;
      end;

    //AT
    {
    else
    if Assigned(FEmbObjects) then
      if FEmbObjects.Paste(Self) then
        ResetLineHeights(True, CaretPos.Y, CaretPos.Y);
        }
  finally
    EndUpdate;
  end;
end;

function TCustomSyntaxMemo.GetCanPaste: Boolean;
begin
  try
    Result := Clipboard.HasFormat(CF_TEXT);
    if not Result and Assigned(FEmbObjects) then
      Result := FEmbObjects.CanPaste;
    if Result then
     if HaveSelection then
      Result := CanClearSelection
     else
      Result := CanInsertText(CaretStrPos);
  except
    Result := False;
  end;
end;

procedure TCustomSyntaxMemo.SelectAll;
begin
  IntSetSelection(0, Lines.TextLength);
end;

function TCustomSyntaxMemo.ExecuteAction(Action: TBasicAction): boolean;
begin // perform support of standard actions
  if Action is TEditAction then
  begin
    Result := True;
    if Action is TEditCut then  CutToClipboard
     else if Action is TEditCopy then CopyToClipboard
      else if Action is TEditPaste then PasteFromClipboard
       else if Action is TEditDelete then ClearSelection
        else if Action is TEditUndo then Undo
         else if Action is TEditSelectAll then SelectAll;
  end else Result := inherited ExecuteAction(Action);
end;

function TCustomSyntaxMemo.UpdateAction(Action: TBasicAction): boolean;
begin // perform support of standard actions
  Result := (Action is TEditAction) and Focused;
  if Result then
  begin
    if Action is TEditCopy then
      TEditAction(Action).Enabled := HaveSelection
    else if (Action is TEditCut) or (Action is TEditDelete) then
      TEditAction(Action).Enabled := HaveSelection and not ReadOnly
    else if Action is TEditPaste then
      TEditAction(Action).Enabled := CanPaste
    else if Action is TEditUndo then
      TEditAction(Action).Enabled := CanUndo
    else if Action is TEditSelectAll then
      TEditAction(Action).Enabled := Lines.Count > 0
    else
      Result := False;
  end;
  if not Result then
    Result := inherited UpdateAction(Action);
end;

//==============================================================================
//   Undo & Redo
//==============================================================================

function TCustomSyntaxMemo.GetCanUndo: Boolean;
begin
  with Lines do Result := UndoList.Count - RedoCount > 0;
end;

function TCustomSyntaxMemo.CanRedo: Boolean;
begin
  Result := Lines.RedoCount > 0;
end;

procedure TCustomSyntaxMemo.ClearUndo;
begin
  Lines.ClearUndo;
end;

procedure TCustomSyntaxMemo.ClearRedo;
begin
  Lines.ClearRedo;
end;

procedure TCustomSyntaxMemo.SetUndoLimit(const Value: integer);
begin
  Lines.UndoLimit := Value;
end;

procedure TCustomSyntaxMemo.AddUndo(op: TTextOperation; const aInsTxt, aDelTxt: ecString;
  aPos: integer; MovedCaret: Boolean);
begin
  Lines.AddUndo(op, aInsTxt, aDelTxt, aPos, MovedCaret);
end;

function IsGroup(U1, U2: TUndoRec): Boolean;
{$IFNDEF EC_VCL6_UP}
 function IfThen(Cond: Boolean; CaseTrue, CaseFalse: integer): integer;
 begin
   if Cond then Result := CaseTrue else Result := CaseFalse;
 end;
{$ENDIF}
begin
  Result := (U1.Operation = U2.Operation) and
            ((U1.Operation = toDelete) and (U1.Position = U2.Position + IFThen(U2.CaretChanged, Length(U1.DelText), 0)) or
             (U1.Operation = toInsert) and (U1.Position = U2.Position - IFThen(U2.CaretChanged, Length(U1.InsText), 0)));
end;

function TCustomSyntaxMemo.SingleUndo: integer;
begin
  with TUndoRec(Lines.UndoList[Lines.UndoList.Count - Lines.RedoCount - 1]) do
  begin
    Lines.RedoCount := Lines.RedoCount + 1;
    Result := Position;
    case Operation of
      toInsert: begin
                 IntDeleteText(Position, Length(InsText));
                 IntInsertText(Position, DelText);
                 if CaretChanged then
                   inc(Result, Length(DelText));
                end;
      toDelete: begin
                 IntInsertText(Position, DelText);
                 if CaretChanged then
                   inc(Result, Length(DelText));
                end;
    end;
  end;
end;

procedure TCustomSyntaxMemo.Undo;
var aPos, N, uPos: integer;
    isGrp: Boolean;
begin
    if not CanUndo then Exit;

    BeginUpdate;
    try
        if not (soSimplifiedUndo in FOptionsEx) then
        begin
          aPos := CaretStrPos;
          uPos := TUndoRec(Lines.UndoList[Lines.UndoList.Count - Lines.RedoCount - 1]).PostPos(TextLength);
          if uPos <> aPos then begin
              CaretStrPos := uPos;
              Exit;
          end;
        end;

        with Lines do
        repeat
            N := UndoList.Count - RedoCount - 1;
            isGrp := (N > 0) and
                ((TUndoRec(UndoList[N]).GroupIndex = TUndoRec(UndoList[N - 1]).GroupIndex) or
                 (soGroupUndo in FOptions) and IsGroup(TUndoRec(UndoList[N - 1]), TUndoRec(UndoList[N])));
            if N >= 0 then
                aPos := SingleUndo;
        until not isGrp;

        CaretStrPos := aPos;
    finally
        EndUpdate;
    end;
end;

function TCustomSyntaxMemo.SingleRedo: integer;
begin
  with TUndoRec(Lines.UndoList[Lines.UndoList.Count - Lines.RedoCount]) do
   begin
    Lines.RedoCount := Lines.RedoCount - 1;
    Result := Position;
    case Operation of
      toInsert: begin
                 IntDeleteText(Position, Length(DelText));
                 IntInsertText(Position, InsText);
                 if CaretChanged then Inc(Result, Length(InsText));
                end;
      toDelete: IntDeleteText(Position, Length(DelText));
    end;
   end;
end;


procedure TCustomSyntaxMemo.Redo;
var aPos, N, uPos: integer;
    isGrp: Boolean;
begin
    if not CanRedo then Exit;

    BeginUpdate;
    try
        aPos := CaretStrPos;
        uPos := TUndoRec(Lines.UndoList[Lines.UndoList.Count - Lines.RedoCount]).Position;
        if uPos <> aPos then begin
            CaretStrPos := uPos;
            Exit;
        end;

        with Lines do
        repeat
            N := UndoList.Count - RedoCount;
            isGrp := (RedoCount > 1) and
                ((TUndoRec(UndoList[N]).GroupIndex = TUndoRec(UndoList[N + 1]).GroupIndex) or
                 (soGroupRedo in FOptions) and IsGroup(TUndoRec(UndoList[N]), TUndoRec(UndoList[N + 1])));
            if RedoCount > 0 then
                aPos := SingleRedo;
        until not isGrp;
        CaretStrPos := aPos;
    finally
        EndUpdate;
    end;
end;

// ===========================================================================
//  properties handlers
// ===========================================================================

procedure TCustomSyntaxMemo.SetLines(const Value: TSyntMemoStrings);
begin
  Lines.Assign(Value);
end;

procedure TCustomSyntaxMemo.SetReplaceMode(const Value: Boolean);
begin
  if FReplaceMode <> Value then
    begin
      FReplaceMode := Value;
      FCaret.Update;
      Change;
    end;
end;

procedure TCustomSyntaxMemo.SetSyntRanges(const Value: TSyntAnalyzer);
begin
  if GetSyntRanges = Value then Exit;
  if Assigned(FSyntRanges) then
   FSyntRanges.Free;
  if Assigned(Value) then
    FSyntRanges := Value.AddClient(Self, GetLines)
  else
    FSyntRanges := nil;

  if Assigned(FSyntRanges) then
    FSyntRanges.Owner.FreeNotification(Self);

  UpdateMonoFontFlag;

  BeginUpdate;
  try
    ResetLineHeights(True);
    FullExpand;
    if not Assigned(FSyntRanges) then Finished;
  finally
    EndUpdate;
  end;
//  Invalidate;
//  if Assigned(FSyntRanges) then CallIdleAnalysis
//   else Finished;
end;

function TCustomSyntaxMemo.GetSyntRanges: TSyntAnalyzer;
begin
  if Assigned(FSyntRanges) then
   Result := FSyntRanges.Owner
  else
   Result := nil;
end;

procedure TCustomSyntaxMemo.SetTabMode(const Value: TTabMode);
begin
  if FTabMode <> Value then
    FTabMode := Value;
end;

function TCustomSyntaxMemo.CalcMargins: TRect;
begin
  SetRectEmpty(Result);
  Result.Left := FTextMargin;
  if FGutter.Visible and (FGutter.Width > 0) then
    begin
      if FGutter.ShowSeparator then
        Result.Left := Result.Left + 2;
      Result.Left := Result.Left + FGutter.Width;
    end;
  if FHorzRuler.Visible then
    Result.Top := FHorzRuler.Height;
end;

procedure TCustomSyntaxMemo.UpdateMargin;
begin
  if not HandleAllocated then
    SetRectEmpty(FMargin)
  else
    FMargin := CalcMargins;
  Invalidate;
end;

procedure TCustomSyntaxMemo.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  inherited Cursor := Value;
end;

function TCustomSyntaxMemo.FormatAtPos(aPos: integer): TSyntaxFormat;
var tmp: integer;
begin
  Result := nil;
  if Assigned(SyntObj) then
   begin
    tmp := SyntObj.TokenAtPos(aPos);
    if tmp <> -1 then
     with SyntObj.Tags[tmp] do
      if Rule <> nil then
       Result := Rule.Style;
   end
end;
// ===========================================================================
//  Bookmarks
// ===========================================================================

procedure TCustomSyntaxMemo.ClearBookmarks;
begin
  FBookmarks.Clear;
end;

function TCustomSyntaxMemo.GetBookmarkObj(Index: integer): TBookmark;
var i: integer;
begin
  for i := 0 to FBookmarks.Count - 1 do
   if FBookmarks[i].BmIndex = Index then
    begin
     Result := FBookmarks[i];
     Exit;
    end;
  Result := nil;
end;

function TCustomSyntaxMemo.GetBookmark(Index: integer): integer;
var bm: TBookmark;
begin
  bm := GetBookmarkObj(Index);
  if bm <> nil then Result := bm.Position
   else Result := -1;
end;

procedure TCustomSyntaxMemo.SetBookmark(Index: integer; const Value: integer);
var bm: TBookmark;
    Accept: Boolean;
begin
  bm := GetBookmarkObj(Index);

  if Value = -1 then
    begin
      if bm <> nil then
        bm.Free;
      Exit;
    end;

  if bm <> nil then bm.Position := Value
   else
    begin
     bm := FBookmarks.Add;
     bm.FBmIndex := Index;
     bm.Position := Value;
     bm.AllowDelete := True;
     Accept := True;
     if Assigned(FOnSetBookmark) then FOnSetBookmark(Self, bm, Accept);
     if not Accept then
      begin
        bm.Free;
        Exit;
      end;
    end;
  Invalidate;
end;

procedure TCustomSyntaxMemo.GotoBookmark(Index: integer);
var bm: TBookmark;
begin
  bm := GetBookmarkObj(Index);
  if bm <> nil then
   begin
    CaretStrPos := bm.Position;
    AnimateCaret(FAnimation.BookmarkAnim);
   end;
end;

procedure TCustomSyntaxMemo.ToggleBookmark(Index: integer);
var bm: TBookmark;
    AllowDelBmk : boolean;
begin
  bm := GetBookmarkObj(Index);
  if bm = nil then SetBookmark(Index, CaretStrPos) else
    if CaretPos.Y = bm.Line then
      begin
        AllowDelBmk := True; //bm.AllowDelete;
        if Assigned(FOnDeleteBookmark) then
          FOnDeleteBookmark(Self, bm, AllowDelBmk);
        if AllowDelBmk then bm.Free
          else bm.Position := CaretStrPos;
      end
     else bm.Position := CaretStrPos;
  Invalidate;
end;

function TCustomSyntaxMemo.BookmarkForLine(Line: integer): integer;
var i: integer;
begin
  for i := 0 to FBookmarks.Count - 1 do
   if FBookmarks[i].Line = Line then
    begin
     Result := FBookmarks[i].BmIndex;
     Exit;
    end;
  Result := -1;
end;

// ===========================================================================
//  Collapse routines
// ===========================================================================
procedure TCustomSyntaxMemo.ClearLineState(Line: integer);
var Info: TLineInfo;
begin
  if FLinesDesc.Count > Line then
   begin
    Info := IntGetLineInfo(Line);
    if Info <> nil then
      Dec(FLineCount, Info.LineCount - 1);
    FLinesDesc[Line] := nil;
   end;
end;

procedure TCustomSyntaxMemo.CollapseRange(Range: TTextRange; Update: Boolean);
var aStart, aEnd: integer;
    bnd: TPoint;
    pCol: TCollapsedRange;
begin
  bnd := SyntObj.GetColRangeBound(Range);
  aStart := StrPosToCaretPos(bnd.X).Y;
  with StrPosToCaretPos(bnd.Y) do
   if X > 0 then   aEnd := Y
   else aEnd := Y - 1;
  if aStart >= aEnd then Exit;
  if soCallapseEmptyLines in FOptions then
   while (aEnd < Lines.Count - 1) and (Lines.IsLineEmpty(aEnd + 1)) do
    inc(aEnd);
  pCol := TCollapsedRange.Create(aStart, bnd.X, bnd.Y, aEnd - aStart + 1);
  pCol.FColText := SyntObj.GetCollapsedText(Range);
  FCollapsed.Add(pCol);
  ClearLineState(aStart);
  if Update then
    CollapsedChanged; //UpdateEditor;
end;

// 1 - collapsed
// 0 - not collapsed, start line of collapsible range
// -1 - end line of the collapsable range
// -2 - in the collapsable lines range
// -3 - can not be hidden and is not start of the collapsible range
function TCustomSyntaxMemo.IsLineCollapsed(Line: integer): integer;
var p: TCollapsedRange;
    t: integer;
begin
  Result := csOutCollapse;
  if (Line >= 0) and (Line < Lines.Count) and not FDisableFolding then
   begin
    p := FCollapsed.GetCollapsed(Line);
    if p <> nil then  Result := csCollapsed
     else
      begin
       if Assigned(SyntObj) then
        Result := SyntObj.GetLineState(Line);
       t := FUserRanges.GetLineState(Line);
       if t > Result then Result := t;
      end;
   end;
end;

procedure TCustomSyntaxMemo.CollapseUserRange(Range: TUserRange;
  Update: Boolean);
var aEnd: integer;
    cr: TCollapsedRange;
begin
  aEnd := Range.FEndLine;
  if soCallapseEmptyLines in FOptions then
   while (aEnd < Lines.Count - 1) and (Lines.IsLineEmpty(aEnd + 1)) do
    inc(aEnd);
  cr := TCollapsedRange.Create(Range.FStartLine, Range.StartPos, Range.EndPos, aEnd - Range.FStartLine + 1);
  cr.FColText := Range.CollapseIcon;
  FCollapsed.Add(cr);
  ClearLineState(Range.FStartLine);
  if Update then
    CollapsedChanged;
end;

function TCustomSyntaxMemo.ToggleCollapse(Line: integer): Boolean;
var p: integer;
    gr: TTextRange;
    ur: TUserRange;
begin
  Result := (Line >= 0) and (Line < Lines.Count);
  if Result then
    begin
      ClearLineState(Line);
      p := FCollapsed.GetCollapsedIndex(Line);
      if p <> -1 then
        begin
          FCollapsed.Delete(p);
          CollapsedChanged;
        end
      else // Expand line
        begin
          if Assigned(SyntObj) then
          begin
            gr := SyntObj.GetRangeAtLine(Line);
            if gr <> nil then
            begin
              CollapseRange(gr);
              Exit;
            end;
          end;
          ur := UserRanges.GetCollapsible(Line);
          if ur <> nil then CollapseUserRange(ur)
            else Result := False;
        end;
   end;
end;

function TCustomSyntaxMemo.ToggleCollapseChildren(Line: integer): Boolean;
var p: integer;
    gr: TTextRange;
    ur: TUserRange;
begin
  Result := (Line >= 0) and (Line < Lines.Count);
  if Result then
    begin
      ClearLineState(Line);
      p := FCollapsed.GetCollapsedIndex(Line);
      if p <> -1 then
        FullExpand(CaretPosToStrPos(Point(0, Line)),
                   CaretPosToStrPos(Point(0, Line + Collapsed[p].LineCount)))
      else
        begin
          if Assigned(SyntObj) then
          begin
            gr := SyntObj.GetRangeAtLine(Line);
            if (gr <> nil) and (gr.EndIdx <> -1) then
            begin
              FullCollapse(CaretPosToStrPos(Point(0, Line + 1)),
                           SyntObj.Tags[gr.EndIdx].StartPos);
              Exit;
            end;
          end;
          ur := UserRanges.GetCollapsible(Line);
          if ur <> nil then
            begin
              FullCollapse(CaretPosToStrPos(Point(0, Line + 1)),
                           ur.EndPos);
            end else Result := False;
        end;
   end;
end;

procedure TCustomSyntaxMemo.UpdateCollapsed;
var i, aStart, aEnd: integer;
    gr: TTextRange;
    ur: TUserRange;
    bnd: TPoint;
    bChanged: Boolean;
begin
  bChanged := False;
  for i := FCollapsed.Count - 1 downto 0 do
  with FCollapsed[i] do
    if not FValide and not FUser and Assigned(SyntObj) then
    begin
      ur := UserRanges.RangeAtPos(FPos);
      gr := SyntObj.RangeAtPos(FPos);
      if (gr = nil) and (ur = nil) then
      begin
        FCollapsed.Delete(i);
        bChanged := True;
      end else
      begin
        if gr <> nil then
          bnd := SyntObj.GetRangeBound(gr)
        else
          bnd := Point(ur.StartPos, ur.EndPos);
        bChanged := True;
        if bnd.X = bnd.Y then Continue;
        aStart := StrPosToCaretPos(bnd.X).Y;
        aEnd := StrPosToCaretPos(bnd.Y).Y;
        if aStart >= aEnd then
          FCollapsed.Delete(i)
        else
        begin
          FValide := True;
          FLine := aStart;
          FEnd := bnd.Y;
          FLineCount := aEnd - aStart + 1;
        end;
      end;
    end;
  if bChanged then
    CollapsedChanged;
  UpdateEditor;
end;


procedure TCustomSyntaxMemo.FullExpand(AStartPos: integer; AEndPos: integer);
var i: integer;
begin
  if FCollapsed.Count > 0 then
   begin
    if (AStartPos = 0) and (AEndPos = -1) then
      FCollapsed.Clear
    else
     begin
      if AEndPos = -1 then AEndPos := TextLength;
      for i := FCollapsed.Count - 1 downto 0 do
       with FCollapsed[i] do
        begin
         if (StartPos >= AStartPos) and (StartPos < AEndPos) then
           FCollapsed.Delete(i);
        end;
     end;
    CollapsedChanged; //UpdateEditor;
   end;
end;

procedure TCustomSyntaxMemo.FullCollapse(AStartPos: integer; AEndPos: integer);
var i : integer;
    isFull: Boolean;

begin
  isFull := (AStartPos = 0) and (AEndPos = -1);
  if AEndPos = -1 then AEndPos := TextLength;

  if Assigned(SyntObj) and not SyntObj.IsFinished then
    SyntObj.Analyze;

  if isFull then
   begin
    FCollapsed.Clear;
    if Assigned(SyntObj) then
     begin
      for i := 0 to SyntObj.RangeCount - 1 do
       if (not SyntObj.Ranges[i].Rule.NotCollapsed) and (SyntObj.Ranges[i].EndIdx <> -1) and
          ((FCollpseLevel = -1) or (SyntObj.Ranges[i].Level = FCollpseLevel)) then
        CollapseRange(SyntObj.Ranges[i], False);
     end;
    for i := 0 to UserRanges.Count - 1 do
     if UserRanges[i].FStartLine < UserRanges[i].FEndLine then
      CollapseUserRange(UserRanges[i], False);
   end else
   begin
    FullExpand(AStartPos, AEndPos);
    if Assigned(SyntObj) then
     with SyntObj do
      for i := 0 to RangeCount - 1 do
       if (not Ranges[i].Rule.NotCollapsed) and (Ranges[i].EndIdx <> -1) and
         ((FCollpseLevel = -1) or (Ranges[i].Level = FCollpseLevel)) and
         (Tags[Ranges[i].StartIdx].StartPos >= AStartPos) and
         (Tags[Ranges[i].EndIdx].EndPos <= AEndPos) then
        CollapseRange(Ranges[i], False);

    for i := 0 to UserRanges.Count - 1 do
     with UserRanges[i] do
       if (StartLine < EndLine) and (StartPos >= AStartPos) and
          (EndPos <= AEndPos) then
        CollapseUserRange(UserRanges[i], False);
   end;

  CollapsedChanged;
end;

procedure TCustomSyntaxMemo.CollapseLines(StartPos, EndPos: integer);
var cRange: TCollapsedRange;
    EndLine, StartLine: integer;
begin
  StartLine := StrPosToCaretPos(StartPos).Y;
  EndLine := StrPosToCaretPos(EndPos).Y;
  if (EndLine > StartLine) and (EndLine < Lines.Count) and (StartLine >= 0) then
   begin
     cRange := TCollapsedRange.Create(StartLine, StartPos, EndPos, EndLine - StartLine + 1);
     cRange.FUser := True;
     FCollapsed.Add(cRange);
     CollapsedChanged;
  end;
end;

procedure TCustomSyntaxMemo.CollapseNearest(Line: integer);
var fnd: TTextRange;
begin
  if Assigned(SyntObj) then
   begin
     if Line < 0 then Line := 0 else
       if Line >= Lines.Count then Line := TextLength
         else Line := Lines.LineIndex(Line) + Lines.LineLength(Line) - 1;
     fnd := SyntObj.GetNearestColRange(Line);
     if fnd <> nil then
       CollapseRange(fnd);
   end;
end;

// ===========================================================================
//  Other
// ===========================================================================
{
function TCustomSyntaxMemo.TokenPosAtMouse(X, Y: integer): TPoint;
var p, i: integer;
begin
  Result := MouseToCaret(X, y);
  p := CaretPosToStrPos(Result);
  if Assigned(SyntObj) then
   with SyntObj do
   for i := 0 to TagCount - 1 do
    if Tags[i].StartPos <= p then
     begin
      if Tags[i].EndPos > p then
        begin
         Result := Tags[i].TagPos;
         Exit;
        end;
     end else Break;
  Result := Point(0,0);
end;}

procedure TCustomSyntaxMemo.WndProc(var Message: TMessage);
{$IFNDEF EC_DOTNET}
var p: TPoint;
    s: string;
    i: integer;
    sp, ep: integer;
{$ENDIF}
begin
  with Message do
  if (Msg = CM_CANCELMODE) and (WParam = 0) then
   begin
    // pass CM_CANCELMODE to window procedure (to hendle pop-up controls)
    SendMessage(Handle, Msg, 1, LParam);
    Exit;
   end;

  if Message.Msg = WM_IME_CHAR then
    Exit;

  if (Message.Msg = WM_PAINT) and (FUpdateCount <> 0) then
   begin
    Dispatch(Message);
    Exit;
   end;

  inherited; //AT
  (* //AT
{$IFNDEF EC_DOTNET}
//  if HandleAllocated then
  with Message do
    case Msg of
      WM_GETTEXTLENGTH: Result := Lines.TextLength;
      WM_SETTEXT:       begin
                          FCaretPos := Point(0,0);
                          Lines.Text := string(PChar(LParam));
                        end;
      WM_GETTEXT:       begin
                          if Lines = nil then PC := '' else
                          {$IFDEF EC_UNICODE}
                            begin
                              s := Lines.FText;
                              PC := PChar(s);
                            end;
                          {$ELSE}
                            PC := PChar(Lines.Text);
                          {$ENDIF}
                          PC := '--';
                          Result := StrLen(StrLCopy(PChar(LParam), PC, WParam - 1));
                        end;
      else inherited;
    end;
{$ENDIF}
    *)

  // Messages for emulation of edit control
  with Message do
  case Msg of
    CM_MOUSELEAVE: ResetHint;
    WM_KEYUP{WM_KEYDOWN}: ResetHint;
    WM_COPY:   CopyToClipboard;
    WM_CUT:    CutToClipboard;
    WM_PASTE:  PasteFromClipboard;
    WM_UNDO:   Undo;
    WM_CLEAR:  ClearSelection;
    WM_CAPTURECHANGED: ResetHint;
  // Standard edit messages
{$IFNDEF EC_DOTNET}
    EM_CANUNDO: Result := Integer(CanUndo);
    EM_CANREDO: Result := Integer(CanRedo);
    EM_CHARFROMPOS:
      begin
        if IsBadWritePtr(Pointer(LParam), 8) then
          begin // edit controls
            p := MouseToCaret(LParamLo, LParamHi);
            ResultLo := CaretPosToStrPos(p);
            ResultHi := p.Y;
          end else
          begin // rich edit controls
            with PPoint(LParam)^ do
              Result := CaretPosToStrPos(MouseToCaret(X, Y));
          end;
      end;
    EM_EMPTYUNDOBUFFER: ClearUndo;
    EM_GETFIRSTVISIBLELINE:  Result := ScrollPosY;
    EM_GETLINE:
      begin
        EMGetLineInt(WParam, sp, ep);
        Result := ep - sp;
        if Result > PWord(LParam)^ then
          Result := PWord(LParam)^;
        S := Lines.SubString(sp + 1, Result);
        if Result > 0 then
          Move(s[1], PChar(LParam)^, Result {$IFDEF EC_STRING_UNICODE}*2{$ENDIF});
      end;
    EM_GETLINECOUNT: Result := EMGetLineCount; //Result := Lines.Count;
    EM_GETMODIFY: Result := integer(Modified);
    EM_GETSEL: begin
                 if WParam <> 0 then PDWORD(WParam)^ := SelStart;
                 if LParam <> 0 then PDWORD(LParam)^ := SelStart + SelLength;
                 if SelStart + SelLength > $FFFF then
                   Result := -1
                 else
                  begin
                   ResultLo := SelStart;
                   ResultHi := SelStart + SelLength;
                  end;
               end;
    EM_LINEFROMCHAR: Result := EMLineFromChar(WParam);
    EM_LINEINDEX: Result := EMLineIndex(WParam);
    EM_LINELENGTH: Result := EMLineLength(WParam);
    EM_LINESCROLL: begin
                    ScrollPosX := ScrollPosX + WParam;
                    ScrollPosY := ScrollPosY + LParam;
                    Result := 1;
                   end;
    EM_POSFROMCHAR:begin
                    if IsBadWritePtr(Pointer(WParam), 8) then
                      begin // Rich Edit v2.0
                        p := StrPosToCaretPos(WParam);
                        with CaretToMouse(p.x, p.y) do
                          Result := MakeLResult(Word(x), Word(y));
                      end else
                      begin  // Rich Edit v1.0, v3.0
                        p := StrPosToCaretPos(LParam);
                        p := CaretToMouse(p.x, p.y); //MZ
                        if p.X < FGutter.Width then p.X := FGutter.Width;
                        PPoint(WParam)^ := p;
                      end;
                   end;
    EM_REPLACESEL: InsertText(PChar(LParam));
    EM_SETMODIFY:  Modified := WParam <> 0;
    EM_SETREADONLY: ReadOnly := WParam <> 0;
    EM_SETSEL:  if WParam = -1 then SelLength := 0 else
                 if (WParam = 0) and (LParam = -1) then SelectAll
                   else SetSelection(WParam, LParam - WParam);
    EM_SETTABSTOPS: begin
                      TabList.Clear;
                      for i := 0 to WParam do
                        TabList.Add(PDWORD(LParam + 4*i)^);
                      Result := -1;
                    end;
    EM_UNDO:        begin
                      Result := integer(CanUndo);
                      Undo;
                    end;
    EM_REDO:        begin
                      Result := integer(CanRedo);
                      Redo;
                    end;
    EM_EXLINEFROMCHAR: Result := EMLineFromChar(LParam);
    EM_EXGETSEL:    if LParam <> 0 then
                      begin
                        PLongInt(LParam)^ := SelStart;
                        PLongInt(LParam + sizeof(LongInt))^ := SelStart + SelLength;
                      end;
    EM_EXSETSEL:   if LParam <> 0 then
                      begin
                        i := PLongInt(LParam)^;
                        SetSelection(i, PLongInt(LParam + sizeof(LongInt))^ - i);
                        Result := EMLineFromChar(i);
                      end;
    EM_SCROLLCARET:ScrollCaret;
    EM_SCROLL:
      begin
        Result := 1 shl 16; // TRUE at high word
        sp := ScrollPosY;
        case WParam of
          SB_LINEDOWN: ExecCommand(smScrollDown);
          SB_LINEUP: ExecCommand(smScrollUp);
          SB_PAGEDOWN: ExecCommand(smScrollPageDown);
          SB_PAGEUP: ExecCommand(smScrollPageUp);
          else Result := 0;
        end;
        Result := Result + abs(ScrollPosY - sp);
      end;
{$ENDIF}
  end;
end;

function TCustomSyntaxMemo.EMCaretToREPos(const P: TPoint): TPoint;
var li: TLineInfo;
    subLine, i: integer;
begin
  li := GetLineInfo(P.Y);
  subLine := 0;
  while (subLine < li.LineCount - 1) and (li.WordBreaks[subLine] < P.X) do
    Inc(subLine);
  if subLine > 0 then
    Result.X := P.X - li.WordBreaks[subLine - 1]
  else
    Result.X := P.X;

  Result.Y := subLine;
  for i := 0 to P.Y - 1 do
    Inc(Result.Y, GetLineInfo(i).LineCount); //MZ
//  Result.Y := LineToLog(P.Y, subLine);
end;

function TCustomSyntaxMemo.EMREPosToCaret(const P: TPoint): TPoint;
var li: TLineInfo;
    subLine: integer;
begin
  LogToLine(P.Y, Result.Y, subLine);
  li := GetLineInfo(Result.Y);
  if subLine > 0 then
    Result.X := P.X + li.WordBreaks[subLine - 1]
  else
    Result.X := P.X;
end;

function TCustomSyntaxMemo.EMLineIndex(LogLine: integer): integer;
begin
  if (Lines.Count = 0) or (LogLine >= FLineCount) then Result := -1 else
    begin
      if LogLine = -1 then
        LogLine := EMCaretToREPos(CaretPos).Y;
      Result := CaretPosToStrPos( EMREPosToCaret(Point(0, LogLine)) );
    end;
end;

procedure TCustomSyntaxMemo.EMGetLineInt(LogLine: integer; var sp, ep: integer);
var Ln, sL, ls: integer;
    li: TLineInfo;
begin
  LogToLine(LogLine, Ln, sL);
  if Ln > Lines.Count then
    begin
      sp := TextLength;
      ep := sp;
      Exit;
    end;

  li := GetLineInfo(Ln);

  if sL > 0 then
    sp := li.WordBreaks[sL - 1]
  else
    sp := 0;

  if sL < li.LineCount - 1 then
    ep := li.WordBreaks[sL]
  else
    ep := LineLength(Ln);

  if ep < sp then
    ep := sp;

  ls := Lines.LineIndex(Ln) - 1;
  Inc(ep, ls);
  Inc(sp, ls);
end;

function TCustomSyntaxMemo.EMLineLength(CharPos: integer): integer;
var sp, ep, tmp : integer;
begin
  if CharPos = -1 then
    begin
      EMGetLineInt(EMLineFromChar(SelStart), sp, tmp);
      EMGetLineInt(EMLineFromChar(SelStart + SelLength), tmp, ep);
      Result := ep - sp - SelLength;
    end else
  if (CharPos < 0) or (CharPos >= TextLength)  then
    Result := 0
  else
    begin
      EMGetLineInt(EMLineFromChar(CharPos), sp, ep);
      Result := ep - sp;
    end;
end;

function TCustomSyntaxMemo.EMLineFromChar(CharPos: integer): integer;
var p: TPoint;
begin
  if CharPos = -1 then p := CaretPos
    else p := StrPosToCaretPos(CharPos);
  Result := EMCaretToREPos(p).Y;
end;

procedure TCustomSyntaxMemo.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;
{
function TCustomSyntaxMemo.EMGetLine(LogLine: integer): ecString;
var sp, ep: integer;
begin
  EMGetLineInt(LogLine, sp, ep);
  Result := Lines.SubString(sp, ep - sp);
end;}

function TCustomSyntaxMemo.EMGetLineCount: integer;
begin
  if Assigned(FSyntRanges) then
    FSyntRanges.CompleteAnalysis;
  Result := EMCaretToREPos(StrPosToCaretPos(TextLength)).Y + 1;
end;
// =========================== Analyze routines =================================

procedure TCustomSyntaxMemo.AddClient(Client: TObject);
begin
  if (FSyntClients.IndexOf(Client) = -1) and Assigned(Client) then
   begin
    FSyntClients.Add(Client);
    if Client is TComponent then
      (Client as TComponent).FreeNotification(Self);
   end;
end;

procedure TCustomSyntaxMemo.RemoveClient(Client: TObject);
begin
  FSyntClients.Remove(Client);
end;

// After analyzing full text
procedure TCustomSyntaxMemo.Finished;
var i: integer;
    iObj: IecSyntClient;
begin
  if csDestroying in ComponentState then Exit;
  ResetLineHeights;
  UpdateCollapsed;
  Invalidate;

  if DoAutoFormat then Exit;

  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecSyntClient, iObj) then
     iObj.Finished;
  if Assigned(FOnFinishAnalysis) then
    FOnFinishAnalysis(Self);
end;

procedure TCustomSyntaxMemo.FormatChanged;
var i: integer;
    iObj: IecSyntClient;
begin
  UpdateMonoFontFlag;
  ResetLineHeights(True);
  Invalidate;
  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecSyntClient, iObj) then
    iObj.FormatChanged;
end;

function TCustomSyntaxMemo.CaretPosChanged: Boolean;
var i: integer;
    iObj: IecSyntMemoClient;
begin
  SupressCaretChange;
  Result := FSyncEditing.CaretPosChanged;
  FHorzRuler.UpdateCurrent;
  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecSyntMemoClient, iObj) then
    Result := Result or iObj.CaretPosChanged;
  if Assigned(FOnCaretPosChanged) then FOnCaretPosChanged(Self);
end;

type
  TRangeAccess = class(TRange);

procedure TCustomSyntaxMemo.TextChanged(Sender: TObject; Pos, Count, LineChange: integer);
var AllowDelBmk: Boolean;
    i, Line, tmp: integer;
    iObj: IecTextClient;
    cp: TPoint;
  function UpdatePos(aPos: integer): integer;
  begin
    Result := aPos;
    if Result > Pos then
      Result := max(Result + Count, Pos);
  end;
begin
  FMarkedSelStart := -1;
  BeginUpdate;
  try
    if Pos = -1 then // Unknown text change
     begin
      FSelLength := 0;
      FSelStart := 0;
      FLeftTopPos := Point(0, 0);
      UpdateMonoFontFlag;
      RefreshSyntax;
      CaretPos := CaretPos;  // Reset caret position
     end else
     begin
      cp := StrPosToCaretPos(Pos);
      Line := cp.Y;
      if Assigned(FSyntRanges) then
        FSyntRanges.TextChanged(Pos, Count, Line, LineChange);
      // Change selection when Persistent Blocks (otherwise reset it)
      if not (soPersistentBlocks in FOptions) or (Lines.Count = 0) then  ResetSelection else
        if SelectMode <> msColumn then
         begin
           tmp := FSelStart + FSelLength;
           FSelStart := UpdatePos(FSelStart);
           FSelLength := UpdatePos(tmp) - FSelStart;
           if FSelLength < 0 then FSelLength := 0;
         end else
        if (LineChange <> 0) and (StrPosToCaretPos(Pos).Y <= FBlock.Top) then
         begin
           FBlock.Top := Min(FBlock.Top + LineChange, Lines.Count - 1);
           FBlock.Bottom := Min(FBlock.Bottom + LineChange, Lines.Count - 1);
         end;
      // Correct bookmarks
      for i := FBookmarks.Count - 1 downto 0 do
       with FBookmarks[i] do
        begin
         if AllowDelete and (Count < 0) and
            (Pos - Position <= 0) and (Pos - Position > Count) then
          begin
            AllowDelBmk := True;
            if Assigned(FOnDeleteBookmark) then
              FOnDeleteBookmark(Self, FBookmarks[i], AllowDelBmk);
            if AllowDelBmk then
             begin
              Free;
              Continue;
             end;
          end;
         Position := UpdatePos(Position);
        end;
      // Correct markes
      for i := 0 to FMarkers.Count - 1 do
       with TMarker(FMarkers[i]) do
         FPos := UpdatePos(FPos);
      // Correct collapsed
      FCollapsed.CorrectCollapsed(Pos, Count, LineChange);
      // Correct user ranges
      for i := UserRanges.Count - 1 downto 0 do
       with UserRanges[i] do
        begin
          StartPos := UpdatePos(StartPos);
          EndPos := UpdatePos(EndPos);
          if EndPos <= StartPos then UserRanges[i].Free; // remove empty range
        end;
      // Reset line heights
      UpdateLineInfosAfterEdit(cp, LineChange);
  {    if not FIgnoreUpdate then
       begin
        UpdateEditor;
        CallIdleAnalysis;
       end;}
      if FLeftTopPos.Y >= Lines.Count then
        FLeftTopPos.Y := Lines.Count - 1;
     end;

    if FSearchMarks.Count > 0 then
      if soKeepSearchMarks in FOptionsEx then
        begin
          for i := FSearchMarks.Count - 1 downto 0 do
            with TRangeAccess(FSearchMarks[i]) do
              begin
                FStartPos := UpdatePos(StartPos);
                FEndPos := UpdatePos(EndPos);
                if EndPos <= StartPos then FSearchMarks.Delete(i); // remove empty range
              end;
        end else ResetSearchMarks;

    FSyncEditing.TextChanged(Pos, Count);
    for i := FSyntClients.Count - 1 downto 0 do
     if Supports(TObject(FSyntClients[i]), IecTextClient, iObj) then
      iObj.TextChanged(Sender, Pos, Count, LineChange);
    if Assigned(FOnTextChanged) then
      FOnTextChanged(Self, Pos, Count, LineChange);

    if (Pos = -1) or (LineChange <> 0) then
      LineNumbers.UpdateBandWidth;

    Change;
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.Assign(Source: TPersistent);
var src: TCustomSyntaxMemo;
begin
  if Source is TCustomSyntaxMemo then
   begin
    src := Source as TCustomSyntaxMemo;

    ReplaceMode := src.ReplaceMode;
    UndoLimit := src.UndoLimit;
    SyntaxAnalyzer := src.SyntaxAnalyzer;
    TabList := src.TabList;
    TabMode := src.TabMode;
    FLineNumbers := src.FLineNumbers;
    Gutter := src.Gutter;
    ShowRightMargin := src.ShowRightMargin;
    RightMargin := src.RightMargin;
    RightMarginColor := src.RightMarginColor;
    NonPrinted := src.NonPrinted;
    ScrollBars := src.ScrollBars;
    Options := src.Options;

    Cursor := src.Cursor;
    Color := src.Color;
    ALign := src.Align;
    Font := src.Font;
   end else inherited;
end;

procedure TCustomSyntaxMemo.RemoveTrailingBlanks(WithUndo: Boolean);
var i, N, cnt, p: integer;
    s: ecString;
begin
  if ReadOnly or (Lines.Count = 0) then Exit;
  BeginUpdate;
  try
    for i := Lines.Count - 1 downto 0 do
     begin
       s := Lines[i];
       N := Length(s);
       cnt := 0;
       //while (cnt < N) and {IsSpaceChar(s[N - cnt])}(s[N - cnt]=' ') do
       while (cnt < N) and ((s[N - cnt]=' ') or (s[N - cnt]=#9)) do //AT
        Inc(cnt);
       if cnt <> 0 then
        begin
          p := Lines.LineIndex(i) + N - cnt;
          if WithUndo then
           AddUndo(toDelete, '', Lines.SubString(p, cnt), p - 1);
          IntDeleteText(p - 1, cnt);
        end;
     end;
    if not WithUndo then ClearUndo;
  finally
    EndUpdate;
  end;

end;

// =============================================================================
// Export functions
// =============================================================================

procedure TCustomSyntaxMemo.ExportToRtf(const FileName: string);
var Exp: TRTFSyntExport;
begin
  Exp := TRTFSyntExport.Create(nil);
  try
   Exp.SyntMemo := Self;
   Exp.ExportType := etAllText;
   Exp.SaveToFile(FileName);
  finally
   Exp.Free;
  end;
end;

procedure TCustomSyntaxMemo.ExportToHTML(const FileName: string);
var Exp: THTMLSyntExport;
begin
  Exp := THTMLSyntExport.Create(nil);
  try
   Exp.SyntMemo := Self;
   Exp.ExportType := etAllText;
   Exp.SaveToFile(FileName);
  finally
   Exp.Free;
  end;
end;

// =============================================================================
// Some Properties Handlers
// =============================================================================

function TCustomSyntaxMemo.GetLines: TSyntMemoStrings;
begin
  if FTextSource = nil
  then
    Result := FLines
  else
    Result := FTextSource.Lines;
end;

procedure TCustomSyntaxMemo.SetTextSource(const Value: TSyntTextSource);
var OldModified: Boolean;
begin
  if FTextSource = Value then Exit;
  OldModified := Modified;
  if FTextSource <> nil then
    FTextSource.RemoveClient(Self);
  if Assigned(Value) and not Assigned(Value.Lines.OnModifiedChanged) and Assigned(FLines.OnModifiedChanged) then
    Value.Lines.OnModifiedChanged := FLines.OnModifiedChanged;
  FTextSource := Value;
  if FTextSource is TecEmbeddedObjects then
    FEmbObjects := TecEmbeddedObjects(FTextSource)
  else
    FEmbObjects := nil;
  if FTextSource <> nil then
   begin
     FTextSource.AddClient(Self);
     FTextSource.FreeNotification(Self);
   end;
  FSkipHltClear := True;
  try
    TextChanged(nil, -1, 0, 0);
  finally
    FSkipHltClear := False;
  end;
  CaretPos := Point(0, 0);
  Invalidate;
  if (OldModified <> Modified) and Assigned(OnModifiedChanged) then
    FLines.OnModifiedChanged(Self);
end;

procedure TCustomSyntaxMemo.SetTabList(const Value: TTabList);
begin
  FTabList.Assign(Value);
  UpdateEditor;
end;

procedure TCustomSyntaxMemo.SetNonPrinted(const Value: TNonPrinted);
begin
  FNonPrinted.Assign(Value);
  Invalidate;
end;

procedure TCustomSyntaxMemo.SetLineNumbers(const Value: TLineNumbers);
begin
  FLineNumbers.Assign(Value);
  Invalidate;
end;

procedure TCustomSyntaxMemo.SetGutter(const Value: TGutter);
begin
  FGutter.Assign(Value);
  Invalidate;
end;

function TCustomSyntaxMemo.GetModified: Boolean;
begin
  Result := Lines.Modified;
end;

function TCustomSyntaxMemo.GetModifiedChanged: TNotifyEvent;
begin
  Result := FLines.OnModifiedChanged;
end;

function TCustomSyntaxMemo.GetSelLength: Integer;
begin
  Result := FSelLength;
end;

function TCustomSyntaxMemo.GetSelStart: Integer;
begin
  Result := FSelStart;
end;

function TCustomSyntaxMemo.GetSelText: ecString;
var i, N, sLeft, sRight: integer;
begin
  Result := '';
  if HaveSelection then
  if SelectMode = msColumn then
    for i := FBlock.Top to FBlock.Bottom do
     begin
      sLeft := LogToLinesPos(Point(FBlock.Left, i)).X;
      sRight := LogToLinesPos(Point(FBlock.Right, i)).X;
      N := LineLength(i);
      if N > sLeft then
        Result := Result + Lines.SubString(CaretPosToStrPos(Point(sLeft, i)) + 1, min(N, sRight) - sLeft);
      Result := Result + sLineBreak;
     end
  else
   Result := Lines.SubString(FSelStart + 1, FSelLength);
end;

procedure TCustomSyntaxMemo.SetModified(const Value: Boolean);
begin
  if not Value then
    Lines.SaveLineStates; //AT
  if Lines.Modified <> Value then
    begin
      if not Value and (soUndoAfterSave in FOptions) then
        Lines.SaveUndoState
      else
        Lines.Modified := Value;
    end;
end;

procedure TCustomSyntaxMemo.SetModifiedChanged(const Value: TNotifyEvent);
begin
  FLines.OnModifiedChanged := Value;
  if Assigned(FTextSource) then
    Lines.OnModifiedChanged := Value;
end;

procedure TCustomSyntaxMemo.SetReadOnly(const Value: Boolean);
begin
  if Value = GetReadonly then Exit;
  if FTextSource <> nil then
    FTextSource.ReadOnly := Value
  else
    FReadOnly := Value;
  FCaret.Update;
end;

procedure TCustomSyntaxMemo.SetSelLength(const Value: Integer);
begin
  SetSelection(SelStart, Value);
end;

procedure TCustomSyntaxMemo.SetSelStart(const Value: Integer);
begin
  SetSelection(Value, SelLength);
end;

procedure TCustomSyntaxMemo.SetSelText(const Value: ecString);
begin
  InsertText(Value);
  IntSetSelection(CaretStrPos - Length(Value), Length(Value));
end;

procedure TCustomSyntaxMemo.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomSyntaxMemo.SetCharCase(const Value: TEditCharCase);
begin
  FCharCase := Value;
  Invalidate;
end;

procedure TCustomSyntaxMemo.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

// =============================================================================
// Hint management routines
// =============================================================================

procedure TCustomSyntaxMemo.ResetHint;
begin
 if not Assigned(FHintTimer) then Exit;

 if FHint is TSyntHintWindow then //AT2
   (FHint as TSyntHintWindow).ResetLines;

 FHintTimer.Tag := 0;
 FHintTimer.Interval := FHintProps.DelayBefore;
 FHintTimer.Enabled := False;
 with FHint do
  if HandleAllocated and IsWindowVisible(Handle) then
    ShowWindow(Handle, SW_HIDE);
 SetRectEmpty(FHintSense);

 //AT2
 if FHint<>FHintBak then
 begin
   FreeAndNil(FHint);
   FHint := FHintBak;
 end;
end;

procedure TCustomSyntaxMemo.ShowHintWnd(const Text: string; FHintRect: TRect; ResetTime: integer);
begin
  //AT2
  if Assigned(FOnShowHint) then
    FOnShowHint(Self, Text, FHint);

  FHint.ActivateHint(FHintRect, Text);
  FHint.Update;
  FHintTimer.Enabled := False;
  FHintTimer.Enabled := True;
  FHintTimer.Interval:= ResetTime;
  FHintTimer.Tag     := 1;
end;

procedure TCustomSyntaxMemo.ShowHintWnd(const Text: string; APos: TPoint; ResetTime: integer; AllowMoveLeft: Boolean);
var FHintRect: TRect;
    MaxW: integer;
begin
  APos := ClientToScreen(APos);
  if AllowMoveLeft then
    MaxW := Screen.DesktopWidth
  else
    MaxW := Screen.DesktopWidth - APos.X;
  Dec(MaxW, 5);
  FHintRect := FHint.CalcHintRect(MaxW, Text, nil);
  OffsetRect(FHintRect, APos.X, APos.Y);
  if FHintRect.Right > Screen.DesktopWidth - 5 then
    OffsetRect(FHintRect, FHintRect.Right - Screen.DesktopWidth - 5, 0);
  ShowHintWnd(Text, FHintRect, ResetTime);
end;

procedure TCustomSyntaxMemo.ShowHintWnd(const Text: string; const HintSense: TRect);
begin
  FHintSense := HintSense;
  ShowHintWnd(Text, Point(HintSense.Left, HintSense.Bottom), FHintProps.TimeGutter, True);
end;

procedure TCustomSyntaxMemo.ShowCollapsedHint(ALine: integer; const HintSense: TRect);
var first_line, Y1, Y2: integer;
    txt: string;
begin
  with FCollapsed.GetCollapsed(ALine) do
   begin
    if FHintProps.ShowFirstLine then
      first_line := ALine
    else
      first_line := ALine + 1;
    if FHint is TSyntHintWindow then //AT2
      (FHint as TSyntHintWindow).SetLines(first_line, ALine + min(LineCount - 1, FHintProps.CollapsedLines));
   end;
  txt :='';
  LineBound(ALine, Y1, Y2);
  FHintSense := HintSense;
  ShowHintWnd(txt, Point(FMargin.Left, Y2 - 1), FHintProps.TimeCollapsed)
end;

function TCustomSyntaxMemo.CollapsedTextIconHint(X, Y: integer): Boolean;
var Line, ColIconPos: integer;
    sz: TPoint;
    R: TRect;
begin
  Result := False;
  if CollapseStyle = csLineSeparator then Exit;
  Line := LineAtPos(Y);
  ColIconPos := 0;
  if GetColRangeTextAtPos(Line, ColIconPos) <> '' then
    begin
      sz := ProcessLine(Canvas, GetLineInfo(Line), Line, ptTestOverCollapsed, GetLineRect(Line), Point(X, Y));
      with StrPosToCaretPos(ColIconPos) do
        R.TopLeft := CaretToMouse(X, Y);
      R.Right := R.Left + sz.X;
      R.Bottom := R.Top + sz.Y;
      if (X >= R.Left) and (X <= R.Right) then
        begin
          ShowCollapsedHint(Line, R);
          Result := True;
        end;
    end;
end;

function TCustomSyntaxMemo.InTextImageHint(X, Y: integer): Boolean;
var Line: integer;
    p: TPoint;
    img: TecCustomEmbeddedObject;
begin
  Result := False;
  if FEmbObjects = nil then Exit;
  Line := LineAtPos(Y);
  if (Line > 0) or (Line > Lines.Count - 1) then Exit;
  p := ProcessLine(Canvas, GetLineInfo(Line), Line, ptTestOverImage, GetLineRect(Line), Point(X, Y));
  if p.X <> -1 then
   begin
     img := FEmbObjects[p.X];
     if img.Hint <> '' then
      begin
        with StrPosToCaretPos(img.Position) do
          FHintSense.BottomRight := CaretToMouse(X, Y);
        FHintSense.Left := FHintSense.Right - img.GetSize(Self, Canvas).cx;
        FHintSense.Top := FHintSense.Bottom;
        FHintSense.Bottom := FHintSense.Top + LineHeight(Point(p.X, Line));//SingleLineHeight(Line);
        ShowHintWnd(img.Hint, FHintSense);
        Result := True;
      end;
   end;
end;

procedure TCustomSyntaxMemo.DoShowHint(const p: TPoint);
var cp, pHint, pn, sp, ep: TPoint;
    i, j, Y1, Y2, TokenIdx, H, Y: integer;
    Text: string;
    R: TRect;
    gi: TCustomGutterObject;
    b: Boolean;
    te: TecTextMargin;
    Ln: TLineInfo;
begin
  cp := MouseToCaret(p.X, p.Y);
  LineBound(cp.y, Y1, Y2);

  // Gutter
  if p.X < FGutter.Width then
    begin
     R := FGutter.ExpBtnRect(Y1, Y2);
     if PtInRect(R, p) and (IsLineCollapsed(cp.Y) = 1) and
        (shCollapsed in FHintProps.ShowHints) then
      begin // Hint for collapsed lines
        ShowCollapsedHint(cp.Y, R);
      end else
      if shGutter in FHintProps.ShowHints then
      begin // Hint for gutter images
        GetLineInfos(cp.Y);
        try
          for i := 0 to FLineInfos.Count - 1 do
           begin
            gi := TCustomGutterObject(FLineInfos[i]);
            if not IsRectEmpty(gi.FBounds) and (gi.Hint <> '') then
             begin
              if gi.SubLine = -2 then
                begin
                  FHintSense := gi.FBounds;
                  H := FHintSense.Bottom - FHintSense.Top;
                  Ln := GetLineInfo(cp.Y);
                  Y := Y1;
                  b := False;
                  for j := 1 to Ln.LineCount - 1 do
                    begin
                      Inc(Y, Ln.Heights[j - 1]);
                      FHintSense.Top := Y + (Ln.Heights[j] - H) div 2;
                      FHintSense.Bottom := FHintSense.Top + H;
                      b := PtInRect(FHintSense, p);
                      if b then break;
                    end;
                end
              else
                begin
                  b := PtInRect(gi.FBounds, p);
                  FHintSense := gi.FBounds;
                end;
              if b then
                begin
                  pHint := ScreenToClient(Mouse.CursorPos);
                  Inc(pHint.Y, 16);
                  ShowHintWnd(gi.Hint, pHint, FHintProps.TimeGutter, True);
                  Break;
                end;
             end;
           end;
        finally
          ClearLineInfos;
        end;
      end;
     Exit;
    end;

    with ScreenToClient(Mouse.CursorPos) do
      begin
        if CollapsedTextIconHint(X, Y) or
           InTextImageHint(X, Y)
        then Exit;

        te := IsOverRightMargin(X);
        if  Assigned(te) and (te.Hint <> '') then
          begin
            FHintSense := Bounds(X - 5, Y - 5, 10, 10);
            ShowHintWnd(te.Hint, FHintSense.BottomRight, FHintProps.TimeGutter);
            Exit;
          end;
      end;

    if Assigned(SyntObj) and
       (cp.X >= 0) and (cp.Y < Lines.Count) and (cp.X < LineLength(cp.Y)) and
       (shTokens in FHintProps.ShowHints) then // Text
    begin
      // Hints for tokens
      TokenIdx := SyntObj.TokenAtPos(CaretPosToStrPos(cp));
      if TokenIdx <> -1 then
       begin
        Text := '';
        GetTokenHint(TokenIdx, Text);
        if Text <> '' then
         begin
          sp := StrPosToCaretPos(SyntObj.Tags[TokenIdx].StartPos);
          ep := StrPosToCaretPos(SyntObj.Tags[TokenIdx].EndPos);
          R.TopLeft := CaretToMouse(sp.x, sp.y);
          R.BottomRight := CaretToMouse(ep.x, ep.y);
          b := R.Bottom > R.Top;  // Multiline token
          if b then
           begin
            R.Left := FMargin.Left;
            R.Right := ClientWidth - FMargin.Right;
           end;
          Inc(R.Bottom, LineHeight(ep));//SingleLineHeight(cp.Y));
          FHintSense := R;
          if b then
            begin
              pn := ScreenToClient(Mouse.CursorPos);
              Inc(pn.Y, 16);
            end
          else
            pn := Point(R.Left, R.Bottom);
          ShowHintWnd(Text, pn, FHintProps.TimeTokens, True);
         end;
       end;
    end;
end;

procedure TCustomSyntaxMemo.HintTimer(Sender: TObject);
begin
  UpdateCaretPos;
  if FHintTimer.Tag = 1 then
    begin
      ResetHint;
      FHintTimer.Enabled := False;
      Exit;
    end;
  FHintTimer.Enabled := False;
  DoShowHint(ScreenToClient(Mouse.CursorPos));
end;

procedure TCustomSyntaxMemo.SetTextMargin(const Value: integer);
begin
  if FTextMargin <> Value then
    begin
      FTextMargin := Value;
      UpdateMargin;
    end;
end;

procedure TCustomSyntaxMemo.GetTokenHint(TokenIndex: integer;
  var HintText: string);
begin
  if Assigned(FOnGetTokenHint) then
    FOnGetTokenHint(Self, TokenIndex, HintText);
end;

procedure TCustomSyntaxMemo.SetHintProps(const Value: THintProps);
begin
  FHintProps.Assign(Value);
end;

procedure TCustomSyntaxMemo.SetKeyMapping(const Value: TSyntKeyMapping);
begin
  ChangeComponentReference(Self, Value, TComponent(FKeyMapping));
end;

procedure TCustomSyntaxMemo.SetOptions(const Value: TSyntaxMemoOptions);
var dif: TSyntaxMemoOptions;
begin
  dif := (FOptions + Value) - (FOptions * Value);
  FOptions := Value;
  if not (soEnableBlockSel in FOptions) and (SelectMode = msColumn) then
   SelectMode := msNone;
  if (soBreakOnRightMargin in dif) and FWordWrap then
   begin
    ResetLineHeights(True);
    UpdateEditor;
   end else
  if (dif * [soHideSelection, soHideDynamic] <> []) and Focused then Invalidate;
  if soKeepCaretInText in dif then CaretPos := CaretPos;
  if soScrollLastLine in dif then ScrollPosY := ScrollPosY;
end;

//==============================================================================
//                         Command processor
//==============================================================================

function IsSignChar(Ch: WideChar): boolean;//AT
begin
  Result := Pos(Ch, '-+!@#%^&*=/\|^&.,:;?') > 0;
end;

function IsHexChar(Ch: WideChar): boolean;//AT
begin
  Result := Pos(Ch, '1234567890abcdefABCDEF') > 0;
end;

procedure TCustomSyntaxMemo.WordRangeAtPos(Pos: TPoint; var wStart, wEnd: integer);
var s: ecString;
    k1, k2: integer;
begin
  if Lines.Count = 0 then Exit;
  if Pos.Y < Lines.Count then  s := Lines[Pos.Y]
   else s := '';
  k1 := min(Pos.X + 1, Length(S));

  //AT
  //fix for selecting "-----" or "+++++" as word
  if (k1 > 0) and (k1 <= Length(s)) and IsSignChar(s[k1]) then
  begin
    while (k1 > 1) and IsSignChar(s[k1-1]) do Dec(k1);
    wStart := CaretPosToStrPos(Point(k1-1, Pos.Y));
    while (k1 < Length(s)) and IsSignChar(s[k1+1]) do Inc(k1);
    wEnd := CaretPosToStrPos(Point(k1, Pos.Y));
    Exit;
  end;

  while (k1 > 1) do                    // !!!!!!!!!!!!!!
   if IsWordChar(s[k1 - 1]) then
     dec(k1) else Break;
  if k1 = 0 then
   begin
    wStart := CaretPosToStrPos(Point(0, Pos.Y));
    wEnd := wStart;
   end else
   begin
    k2 := k1;
    while (k2 > 0) and (k2 <= Length(S)) and IsWordChar(S[k2]) do
      inc(k2);
    wStart := CaretPosToStrPos(Point(k1 - 1, Pos.Y));
    wEnd := CaretPosToStrPos(Point(k2 - 1, Pos.Y));

    //AT
    //addition to select color token #AABBCC (len=6) or #AAA (len=3)
    if (k1>1) and IsHexChar(s[k1]) and (s[k1-1]='#') then
      if ((k2-k1)=6) or ((k2-k1)=3) then
        Dec(wStart);
   end;
end;

function TCustomSyntaxMemo.WordAtPos(p: TPoint): ecString;
var k1, k2: integer;
begin
  WordRangeAtPos(p, k1, k2);
  Result := Lines.SubString(k1 + 1, K2 - k1);
end;

procedure TCustomSyntaxMemo.SelectWord;
  //make new version of IsSpaceChar to handle only space/tab
  //(but not EOL chars, like Notepad++)
  function IsSpaceChar(ch: Widechar): boolean;
  begin
    Result:= (ch=' ') or (ch=#9);
  end;
var
  wEnd, wStart: integer;
begin
  //handle case when caret is on space
  //(like Notepad++ does)
  wStart:= CaretStrPos;
  if (wStart>=0) and (wStart<Length(Lines.FText)) then
    if IsSpaceChar(Lines.FText[wStart+1]) then
    begin
      wEnd:= wStart;
      while (wStart>0) and IsSpaceChar(Lines.FText[wStart]) do Dec(wStart);
      while (wEnd<Length(Lines.FText)) and IsSpaceChar(Lines.FText[wEnd+1]) do Inc(wEnd);
      SetSelection(wStart, wEnd-wStart);
      Exit;
    end;

  WordRangeAtPos(CaretPos, wStart, wEnd);
  if wEnd = wStart then
   begin
    CaretStrPos := wEnd + 1;
    SelLength := 0;
   end else
//   begin
//    CaretStrPos := wEnd;
    SetSelection(wStart, wEnd - wStart);
//   end;
end;

procedure TCustomSyntaxMemo.IndentLines(sl, el, Count, XPos: integer;
  BreakAlign: Boolean);
var i, j, n, pos, x, new_len: integer;
    s: ecString;
    FixPos: array[0..2] of TPoint;
begin
  if (el >= Lines.Count) or (sl >= Lines.Count) or (Count = 0) then Exit;

  if (Count < 0) and not BreakAlign then
   for i := sl to el do
    begin
     s := Lines[i];
     x := LinesPosToLogX(S, XPos);
     if x >= Length(S) then Continue;
     S := Copy(S, x + 1, Length(S) - x);
     n := FirstLetter(s);
     if n < Length(s) then
       begin
         if XPos = 0 then
           n := LinesPosToLogX(S, n);
         if n < -Count then
           begin
             Count := -n;
             if Count = 0 then Exit;
           end;
       end;
    end;

  FixPos[0] := CaretPos;
  if SelectMode = msColumn then
    begin
      FixPos[1] := {LogToLinesPos}(FBlock.TopLeft);
      FixPos[2] := {LogToLinesPos}(FBlock.BottomRight);
    end else
    begin
      FixPos[1] := StrPosToCaretPos(SelStart);
      FixPos[2] := StrPosToCaretPos(SelStart + SelLength);
    end;

  BeginUpdate;
  try
  for i := sl to el do
   begin
     s := Lines[i];
     x := LinesPosToLogX(S, XPos);
     if x >= Length(S) then Continue;
     S := Copy(S, x + 1, Length(S) - x);
     n := FirstLetter(s);
     if n < Length(s) then // skip empty lines
      begin
       if XPos = 0 then
         new_len := LinesPosToLogX(S, n) + Count
       else
         new_len := n + Count;
       if new_len < 0 then new_len := 0;
       S := GetIndentString(new_len, XPos > 0);

       pos := CaretPosToStrPos(Point(x, i));
       ReplaceText(pos, n, S);
       new_len := Length(S) - n;

       if SelectMode <> msColumn then
       for j := 0 to 2 do
        if FixPos[j].Y = I then
          begin
           FixPos[j].X := FixPos[j].X + new_len;
           if FixPos[j].X < 0 then
             FixPos[j].X := 0;
          end;
      end;
   end;
  CaretPos := FixPos[0];
  if SelectMode = msColumn then
   begin
     FBlock.TopLeft := {LinesPosToLog}(FixPos[1]);
     FBlock.BottomRight := {LinesPosToLog}(FixPos[2]);
   end else
   begin
     n := CaretPosToStrPos(FixPos[1]);
     IntSetSelection(n, CaretPosToStrPos(FixPos[2]) - n);
   end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.ShiftSelection(Count: integer; BreakAlign: Boolean);
var sl, el, x: integer;
begin
  if ReadOnly then Exit;
  GetSelectedLines(sl, el);
  If SelectMode = msColumn then x := FBlock.Left else x := 0;
  IndentLines(sl, el, Count, x, BreakAlign);
end;

function TCustomSyntaxMemo.IsWordEdge(c1, c2: ecChar): boolean;
    function CharType(c: ecChar): integer;
    begin
        if IsWordChar(c) then Result := 0
            else if IsSpaceChar(c) then Result := 1
                else Result := 2;
    end;
begin
    case WordNavigation of
    wnWordStart:    Result := not IsWordChar(c1) and IsWordChar(c2) or (c2 = #10);
    wnWordEdge:     Result := IsWordChar(c1) <> IsWordChar(c2);
    wnWordSpaceEdge:Result := CharType(c1) <> CharType(c2);
    else // wnEclipse
        Result := (CharType(c1) <> CharType(c2)) or
            (IsWordChar(c1) and (ecLoCase(c2) <> c2) and (ecUpCase(c1) <> c1));
    end;
end;

function TCustomSyntaxMemo.PrevWord(pt: TPoint): TPoint;
var s: ecString;
begin
  if pt.Y >= Lines.Count then
  begin
    if Lines.Count = 0 then Result := Point(0,0)
     else Result := Point(LineLength(Lines.Count - 1), Lines.Count - 1);
  end else
  begin
    Result := pt;
    dec(Result.X);
    s := Lines[Result.Y];
    while (s = '') and (Result.Y > 0) do
    begin
      dec(Result.Y);
      s := Lines[Result.Y];
      Result.X := Length(s);
    end;

    if Result.X >= Length(s) then
    begin
      Result.X := Length(s);
      if (Result.X = 0) or IsWordEdge(s[Result.X], #10) then
        Exit;
      dec(Result.X);
    end;

    while Result.X > 0 do
      if IsWordEdge(s[Result.X], s[Result.X + 1]) then
        Exit
      else
        dec(Result.X);
    if (Result.X < 0) or not IsWordEdge(#10, s[1]) and (Result.Y > 0) then
      Result := Point(LineLength(pt.Y - 1), pt.Y - 1);
   end;
end;

function TCustomSyntaxMemo.NextWord(pt: TPoint): TPoint;
var s: ecString;
begin
  if pt.Y >= Lines.Count then
  begin
    if Lines.Count = 0 then Result := Point(0,0)
      else Result := Point(LineLength(Lines.Count - 1), Lines.Count - 1);
  end else
  begin
    Result := pt;
    inc(Result.X);
    if pt.X >= LineLength(pt.Y) then
    begin
      if pt.Y >= Lines.Count - 1 then
        Exit;
      Result := Point(0, pt.Y + 1);
      s := Lines[Result.Y];
      while (s = '') and (Result.Y < Lines.Count - 1) do begin
        inc(Result.Y);
        s := Lines[Result.Y];
      end;
      if (s = '') or IsWordEdge(#10, s[1]) then
        Exit;
      inc(Result.X);
    end else
      s := Lines[Result.Y];
    while Result.X < Length(s) do
      if IsWordEdge(s[Result.X], s[Result.X + 1]) then
        Exit
      else
        Inc(Result.X);
   end;
end;

function TCustomSyntaxMemo.ChangeCase(Pos, Count: integer; Oper: TChangeCase): Boolean;
var s, s1: ecString;
    i: integer;
    c: ecChar;
begin
  s := Lines.SubString(Pos + 1, Count);
  s1 := s;
  case Oper of
    ccUpper:
      for i := 1 to Length(s) do
        s[i] := ecUpCase(s[i]);
    ccLower:
      for i := 1 to Length(s) do
        s[i] := ecLoCase(s[i]);
    ccToggle:
      for i := 1 to Length(s) do  // Toggle
        begin
          c := ecUpCase(s[i]);
          if c = s[i] then s[i] := ecLoCase(c)
            else s[i] := c;
        end;
    ccTitle:
      begin     // Title
        for i := 1 to Length(s) do
         if IsAlphaChar(s[i]) then
          if (Pos + i - 1 = 0) or
             not IsAlphaChar(Lines.Chars[Pos + i - 1]) then
            s[i] := ecUpCase(s[i])
          else
            s[i] := ecLoCase(s[i]);
      end;
  end;
  Result := s <> s1;
  if Result then
    ReplaceText(Pos, Count, s);
end;

procedure TCustomSyntaxMemo.SelChangeCase(Oper: TChangeCase);
  procedure BlockCase(Oper: TChangeCase);
  var i, sLeft, sRight, cnt: integer;
      R: TRect;
  begin
    BeginUpdate;
    try
      R := FBlock;
      for i := R.Top to R.Bottom do
       begin
         sLeft := LogToLinesPos(Point(R.Left, i)).X;
         sRight := LogToLinesPos(Point(R.Right, i)).X;
         cnt := min(LineLength(i), sRight) - sLeft;
         if cnt > 0 then
           begin
            sLeft := CaretPosToStrPos(Point(sLeft, i));
            ChangeCase(sLeft, cnt, Oper);
           end;
       end;
    finally
      EndUpdate;
    end;
  end;
begin
  if not HaveSelection then
    ChangeCase(CaretStrPos, 1, Oper)
  else if SelectMode <> msColumn then
    ChangeCase(FSelStart, FSelLength, Oper)
  else
    BlockCase(Oper);
end;

function TCustomSyntaxMemo.GetIndentString(Len: integer; OnlySpaces: Boolean): ecString;
var pos, n_pos: integer;
begin
  if (soOptimalFill in FOptions) and not OnlySpaces and (TabMode <> tmSpaces) then
   begin
     Result := '';
     pos := 0;
     repeat
       n_pos := TabList.NextTab(pos, False);
       if n_pos <= Len then Result := Result + #9
        else
         begin
           if pos < Len then
             Result := Result + StringOfChar(' ', Len - pos);
           Exit;
         end;
       pos := n_pos;
     until False;
   end else
  Result := StringOfChar(' ', Len);
end;

function TCustomSyntaxMemo.StringIndent(const S: ecString): integer;
var i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
   if S[i] = #9 then
     Result := TabList.NextTab(Result, (soAllowZeroTab in FOptionsEx) and (i > 1) and AllowZeroTabAfter(S[i - 1]))
   else if S[i] = ' ' then
     Inc(Result)
   else
     Exit;
  Result := -1;
end;

function TCustomSyntaxMemo.FirstLetter(const s: ecString): integer;
var i: integer;
begin
  for i := 1 to Length(s) do
   if not IsSpaceChar(s[i]) then
    begin
     Result := i - 1;
     Exit;
    end;
  Result := 0;
end;

function TCustomSyntaxMemo.LastLetter(const s: ecString): integer;
var i: integer;
begin
  for i := Length(s) downto 1 do
   if not IsSpaceChar(s[i]) then
    begin
     Result := i;
     Exit;
    end;
  Result := 0;
end;

procedure TCustomSyntaxMemo.ExecCommand(Command: integer; Data: ecPointer);

  function GetIndent(StartLine: integer; LessThan: integer): integer;
  var i, ind: integer;
  begin
    if Lines.Count > 0 then
    for i := StartLine downto 0 do
     begin
      ind := StringIndent(Lines[i]);
      if (ind <> -1) and (ind < LessThan) then
       begin
        Result := ind;
        Exit;
       end;
     end;
     Result := 0;
  end;

  // Char categories:
  // 0 - line break character
  // 1 - space character
  // 2 - symbol
  // 3 - word character
  function CharCategory(Pos: integer): BYTE;
  var C: ecChar;
  begin
   C := Lines.Chars[Pos];
   if IsWordChar(C) then Result := 3 else
    if IsLineBreakChar(C) then Result := 0 else
     if IsSpaceChar(C) then Result := 1
       else Result := 2;
  end;

  function CountToNext(Pos: integer): integer;
  var Cs: BYTE;
  begin
    Inc(Pos);
    if Pos >= TextLength then
      begin
        Result := 0;
        Exit;
      end;
    Result := Pos;
    Cs := CharCategory(Pos);
    Inc(Pos);
    if Cs > 1 then
      begin
        while (Pos <= TextLength) and (CharCategory(Pos) = Cs) do
          Inc(Pos);
        while (Pos <= TextLength) and (CharCategory(Pos) = 1) do
          Inc(Pos);
      end else
        while (Pos <= TextLength) and (CharCategory(Pos) <= 1) do
          Inc(Pos); // Initial spaces
    Result := Pos - Result;
  end;

  function CountToPrev(Pos: integer): integer;
  begin
    if Pos = 0 then Result := 0 else
      begin
        Result := Pos;
        while (Pos > 0) and (CharCategory(Pos) in [1,2]) do
          Dec(Pos);
        while (Pos > 0) and (CharCategory(Pos) = 3) do
          Dec(Pos);
        Result := Result - Pos;
      end;
  end;

  // x - logical position
  function GetSmartPos(x, Line: integer): integer;
  var x1, i: integer;
      s: ecString;
  begin
    while Line > 0 do
     begin
      Dec(Line);
      x1 := LogToLinesPos(Point(x, Line)).X + 2;
      s := Lines[Line];
      for i := x1 to Length(s) do
       if (Ord(s[i - 1]) <= $20) and (Ord(s[i]) > $20) then
        begin
         Result := LinesPosToLog(Point(i - 1, Line)).X;
         Exit;
        end;
     end;
    Result := TabList.NextTab(x, False);
  end;

  function PageUp: integer;
  var Y: integer;
  begin
    Y := CaretToMouse(0, TopLine).Y;
    Y := Y - (ClientHeight - FMargin.Top - FMargin.Bottom - DefTextExt.cy);
    Result := LineAtPos( Y );
  end;

  function PageDown: integer;
  var Y: integer;
  begin
    Y := CaretToMouse(0, TopLine).Y;
    Y := Y + (ClientHeight - FMargin.Top - FMargin.Bottom - DefTextExt.cy);
    Result := LineAtPos( Y );
  end;

var FSvKeepPos: integer;

  function FixedColumnPos: TPoint;
  var lInfo: TLineInfo;
  begin
    if (soFixedColumnMove in FOptions) and
       (CaretPos.Y = FColPos.Y) and
       (CaretPos.X >= FColPos.X) then
      Result := FColPos
    else
      Result := FCaretPos;

    if (soKeepCaretInText in Options) then
      begin
        if FWordWrap then
          begin
            lInfo := GetLineInfo(Result.Y);
            if (lInfo.LineCount > 1) and (Result.X >= lInfo.WordBreaks[0]) then
              Exit;
          end;

        if FKeepedXPos = -1 then
          FSvKeepPos := LinesPosToLog(Point(Result.X, FCaretPos.Y)).X
        else
          begin
            Result := LogToLinesPos(Point(FKeepedXPos, Result.Y));
            FSvKeepPos := FKeepedXPos;
          end;
      end;
  end;

  function SmartCaretCmd: Boolean;
  var p: TPoint;
      h, i: integer;
      Info: TLineInfo;
  begin
   Result := False;
   if (Command = smUp) or (Command = smDown) then
    begin
      Result := True;
      p := FixedColumnPos;
      h := LineHeight(p);
      p := CaretToMouse(p.X, p.Y);
      Inc(p.X, 2);
      case Command of
       smUp:   Dec(p.Y, h div 2);
       smDown: Inc(p.Y, h div 2 + h);
      end;
      CaretPos := MouseToCaret(p.X, p.y);
    end else
   if FWordWrap then
    begin
      Info := GetLineInfo(FCaretPos.Y);
      if Info.LineCount > 1 then
      case Command of
       smFirstLetter, smLineStart, smLastLetter, smLineEnd:
        begin
          h := 0;
          for i := Info.LineCount - 2 downto 0 do
           if Info.WordBreaks[i] <= FCaretPos.X then
            begin
              h := i + 1;
              Break;
             end;
          if h = 0 then p.X := 0 else p.X := Info.WordBreaks[h - 1];
          if h >= Info.LineCount - 1 then p.Y := LineLength(FCaretPos.Y) else p.Y := Info.WordBreaks[h] - 1;
         case Command of
          smFirstLetter, smLineStart: CaretPos := Point(p.X, FCaretPos.Y);
          smLastLetter, smLineEnd:    CaretPos := Point(p.Y, FCaretPos.Y);
         end;
         Result := True;
        end
       else Result := False;
    end;
    end;
  end;

var x, n, i, ws, we, nV: integer;
    S: ecString;
    cp: TPoint;
    DataChar: ecChar;
    Handled: Boolean;
    iHook: IecCommandHook;
    searchDlg: TSyntFindDialog;
begin
  if Assigned(FOnExecuteCommand) then
   begin
    Handled := False;
    FOnExecuteCommand(Self, Command, Data, Handled);
    if Handled then
      Exit;
   end;

  if Assigned(FKeyMapping) and FKeyMapping.ExecuteCommand(Self, Command, Data) then
    Exit;

  Handled := False;
  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecCommandHook, iHook) then
    begin
     iHook.BeforeCommand(Command, Data, Handled);
     if Handled then
       Exit;
    end;

  if (Command < 300) and (Command > 0) then
   begin
    if Command >  smColSelection then
      begin
        SelectMode := msColumn;
        Dec(Command, smColSelection);
        FShiftDown := True;
      end else
    if Command > smSelection then
      begin
        {
        if SelectMode in [msNone, msColumn] then
          SelectMode := msNormal;
          }
        SelectMode := SelectModeDefault;
          
        Dec(Command, smSelection);
        FShiftDown := True;
      end else
    {
    //AT - commented
    if not (soKeepSelMode in FOptions) then
      begin
        SelectMode := msNone;
        FShiftDown := False;
      end
    }
    //AT - added instead
    if True then
      begin
        FShiftDown := False;
      end
    else
      FShiftDown := SelectMode <> msNone;

    FKeyNavigate := Command <> smGotoXY;
    try
     FSvKeepPos := -1;
     if not (soSmartCaret in FOptions) or not SmartCaretCmd then
     case Command of
      smLeft,
      smLeftUp:      if not (soKeepCaretInText in FOptions) and (Command <> smLeftUp) or
                        (FCaretPos.X > 0) then
                       CaretPos := SkipHidden(FCaretPos.X - 1, FCaretPos.Y, False)
                     else
                       CaretPos := SkipHidden(LineLength(FCaretPos.Y - 1), FCaretPos.Y - 1, False);
      smRight:       if not (soKeepCaretInText in FOptions) or (FCaretPos.X < LineLength(FCaretPos.Y)) then
                       CaretPos := SkipHidden(FCaretPos.X + 1, FCaretPos.Y, True)
                     else
                       if FCaretPos.Y < Lines.Count - 1 then
                         CaretPos := SkipHidden(0, FCaretPos.Y + 1, True);
      smUp:          begin
                       cp := FixedColumnPos;
                       CaretPos := SkipHidden(LogToLinesPos(Point(LinesPosToLog(cp).X, cp.Y - 1)).X, cp.Y - 1, False)
                     end;
      smDown:        begin
                       cp := FixedColumnPos;
                       CaretPos := SkipHidden(LogToLinesPos(Point(LinesPosToLog(cp).X, cp.Y + 1)).X, cp.Y + 1, True)
                     end;
      {
      smWordLeft:    with PrevWord(FCaretPos) do
                       CaretPos := SkipHidden(X, Y, False);
      smWordRight:   with NextWord(FCaretPos) do
                       CaretPos := SkipHidden(X, Y, True);
      }
      smWordLeft: DoWordJump(False);//AT
      smWordRight: DoWordJump(True);//AT

      smLineStart:   CaretPos := SkipHidden(0, FCaretPos.Y, False);
      smLineEnd:     CaretPos := SkipHidden(LineLength(FCaretPos.Y), FCaretPos.Y, True);
      smPageUp:      begin
                       x := ScrollPosY;
                       cp := CaretToMouse(CaretPos.X, CaretPos.Y);
                       nV := VisibleLines - 1; //AT
                       ScrollPosY := ScrollPosY - nV;
                       if x = ScrollPosY then
                         CaretPos := SkipHidden(FCaretPos.X, FCaretPos.Y - nV, False)
                       else
                         CaretPos := MouseToCaret(cp.X, cp.Y);
                       Update; //AT
                     end;
      smPageDown:    begin
                       x := ScrollPosY;
                       cp := CaretToMouse(CaretPos.X, CaretPos.Y);
                       nV := VisibleLines - 1; //AT
                       ScrollPosY := ScrollPosY + nV;
                       if x = ScrollPosY then
                         CaretPos := SkipHidden(FCaretPos.X, FCaretPos.Y + nV, True)
                       else
                         CaretPos := MouseToCaret(cp.X, cp.Y);
                       Update; //AT
                     end;
      smPageLeft:    CaretPos := SkipHidden(FCaretPos.X + VisibleCols, FCaretPos.Y, True);
      smPageRight:   CaretPos := SkipHidden(max(FCaretPos.X - VisibleCols, 0), FCaretPos.Y, False);
      smPageTop:     CaretPos := SkipHidden(FCaretPos.X, TopLine, False);
      smPageBottom:  CaretPos := SkipHidden(FCaretPos.X, TopLine + VisibleLines - 2, True); //AT "-2"
      smEditorTop:   CaretPos := Point(0, 0);
      smEditorBottom:CaretStrPos := Lines.TextLength;
      smGotoXY:      if Data <> nil then
                       CaretPos := {$IFDEF EC_DOTNET}TPoint(Data){$ELSE}PPoint(Data)^{$ENDIF};
      smFirstLetter: if FCaretPos.Y < Lines.Count then
                     begin
                       x := FirstLetter(Lines[FCaretPos.Y]);
                       if x = FCaretPos.X then x := 0; //AT
                       CaretPos := SkipHidden(x, FCaretPos.Y, False);
                     end;
      smLastLetter:  if FCaretPos.Y < Lines.Count then
                     begin
                       x := LastLetter(Lines[FCaretPos.Y]);
                       if (x = FCaretPos.X) or (x = 0) then x := LineLength(FCaretPos.Y); //AT
                       CaretPos := SkipHidden(x, FCaretPos.Y, True);
                     end;
     end;
    finally
      FShiftDown := False;
      FKeyNavigate := False;
    end;
    FColPos := CaretPos;
    if FSvKeepPos <> -1 then
      FKeepedXPos := FSvKeepPos;
   end else
   case Command of
    smScrollUp:   ScrollPosY := ScrollPosY - 1;
    smScrollDown: ScrollPosY := ScrollPosY + 1;
    smScrollLeft: ScrollPosX := ScrollPosX - 1;
    smScrollRight:ScrollPosX := ScrollPosX + 1;

    smScrollPageUp:   ScrollPosY := ScrollPosY - VisibleLines;
    smScrollPageDown: ScrollPosY := ScrollPosY + VisibleLines;
    smScrollPageLeft: ScrollPosX := ScrollPosX - VisibleCols;
    smScrollPageRight:ScrollPosX := ScrollPosX + VisibleCols;

    smScrollAbsUp:    ScrollPosY := 0;
    smScrollAbsDown:  if Lines.Count = 0 then ScrollPosY := 0
                       else ScrollPosY := LineToLog(Lines.Count - 1, GetLineInfo(Lines.Count - 1).LineCount - 1);
    smScrollAbsLeft:  ScrollPosX := 0;
    smScrollAbsRight: ScrollPosX := 800;

    smCopy:            CopyToClipboard;
    smCut:             CutToClipboard;
    smPaste:           PasteFromClipboard;
    smUndo:            Undo;
    smRedo:            Redo;
    smSelectAll:       SelectAll;
    smClearSelection:  ClearSelection;
    smCopyAsRTF:       if soCopyAsRTF in Options then CopyToClipboard
                        else
                          begin
                           FOptions := FOptions - [soCopyAsRTF];
                           CopyToClipboard;
                           FOptions := FOptions + [soCopyAsRTF];
                          end;

    smDeleteChar:
        if HaveSelection and (soOverwriteBlocks in FOptions) then ClearSelection else
          if (FCaretPos.Y < Lines.Count) and
             (FCaretPos.X < LineLength(FCaretPos.Y)) then DeleteText(1) else
           begin
             InsertText(StringOfChar(' ', FCaretPos.X - LineLength(FCaretPos.Y)));
             DeleteText(1);
           end;
    smDeleteLastChar:
        if HaveSelection and (soOverwriteBlocks in FOptions) then ClearSelection else
          if FCaretPos.X = 0 then DeleteText(-1) else
           begin
            if (FCaretPos.Y < Lines.Count) and (FCaretPos.X <= LineLength(FCaretPos.Y)) then
             begin
               S := Lines[FCaretPos.Y];
               n := LinesPosToLog(FCaretPos).X;
               x := StringIndent(S);
               if x = -1 then x := n;// FCaretPos.X;
               if (soBackUnindent in FOptions) and ({FCaretPos.X}n = x) then
                begin
                 n := LogToLinesPos(Point(GetIndent(FCaretPos.Y - 1, x), FCaretPos.Y)).X;
                 if n < FCaretPos.X then
                   DeleteText(n - FCaretPos.X);
                end
               else
                 DeleteText(-1);
             end else CaretPos := Point(FCaretPos.X - 1, FCaretPos.Y);
           end;
    smDeleteLine:
        if Lines.Count > FCaretPos.Y then
          begin
           CaretPos := Point(0, FCaretPos.Y);
           DeleteText(Lines.LineSpace(FCaretPos.Y));
          end;
    smDeleteWord:     DeleteText(CountToNext(CaretStrPos));
    smDeleteLastWord: DeleteText(-CountToPrev(CaretStrPos));
    smDeleteBOL:      DeleteText(-min(LineLength(FCaretPos.Y), FCaretPos.X));
    smDeleteEOL:   if FCaretPos.X < LineLength(FCaretPos.Y) then
                      DeleteText(LineLength(FCaretPos.Y) - FCaretPos.X);
    smClearAll:
        begin
         CaretPos := Point(0,0);
         DeleteText(Lines.TextLength);
        end;

    smChar:
       if Data <> nil then
         begin
          DataChar := PecChar(Data){$IFNDEF EC_DOTNET}^{$ENDIF};
          if DataChar >= ' ' then
          begin
           if Lines.Count > CaretPos.Y then x := LineLength(CaretPos.Y)
                                       else x := 0;
           if (CaretPos.X < x) and FReplaceMode and (FSelLength = 0)
             then
              begin
               if ReplaceText(CaretStrPos, 1, DataChar) then//ReplaceChar(DataChar)
                 CaretStrPos := CaretStrPos + 1;
              end
             else InsertText(DataChar);
           DoInsertChar(DataChar, CaretStrPos - 1);
          end;
         end;
    smString:
       if Data <> nil then InsertText(PecChar(Data));

    smLineBreakSoft,
    smLineBreak:
        if soAutoIndentMode in FOptions then
          InsertNewLine(GetIndent(FCaretPos.Y, LinesPosToLog(FCaretPos).X + 1), False, Command = smLineBreakSoft)
        else
          InsertNewLine(0, False, Command = smLineBreakSoft);
    smInsertLine:
        InsertNewLine(0, True, False);
    smBlockIndent:   ShiftSelection(FBlockIndent, not (soUnindentKeepAlign in FOptions));
    smBlockUnindent: ShiftSelection(-FBlockIndent, not (soUnindentKeepAlign in FOptions));
    smTab:
        case TabMode of
          tmSpaces,
          tmSmartTab:
           begin
             x := LinesPosToLog(FCaretPos).X;
             if TabMode = tmSpaces then n := TabList.NextTab(x, False)
               else n := GetSmartPos(x, FCaretPos.Y);
//             if (FCaretPos.X < LineLength(FCaretPos.Y)) or
//                (FCaretPos.X = LineLength(FCaretPos.Y)) and
//                (soKeepCaretInText in FOptions)
//             then
             InsertText(StringOfChar(' ', n - x))
//             else
//                CaretPos := Point(n, FCaretPos.Y);
           end;
          tmTabChar: InsertText(#9);
        end;
    smTabChar: InsertText(#9);

    smUpperCase,
    smLowerCase,
    smToggleCase,
    smTitleCase:
        if HaveSelection then
          SelChangeCase(TChangeCase(Command - smUpperCase + 1))
        else
          begin
           WordRangeAtPos(CaretPos, x, n);
           if n > x then
             ChangeCase(x, n - x, TChangeCase(Command - smUpperCase + 1));
          end;
    smUpperCaseBlock,
    smLowerCaseBlock,
    smToggleCaseBlock,
    smTitleCaseBlock: SelChangeCase(TChangeCase(Command - smUpperCaseBlock + 1));

    smInsertMode:    ReplaceMode := False;
    smOverwriteMode: ReplaceMode := True;
    smToggleMode:    ReplaceMode := not ReplaceMode;

    smNormalSelect: SelectMode := msNormal;
    smColumnSelect: SelectMode := msColumn;
    smLineSelect:   SelectMode := msLine;
    smResetSelection: ResetSelection;

    smGotoBookmark0..smGotoBookmark9: GotoBookmark(command - smGotoBookmark0);
    smSetBookmark0..smSetBookmark9:   ToggleBookmark(command - smSetBookmark0);
    smDropMarker:   if Data = nil then DropMarker(CaretPos)
                      else DropMarker({$IFDEF EC_DOTNET}TPoint(Data){$ELSE}PPoint(Data)^{$ENDIF});
    smCollectMarker:CollectMarker;
    smSwapMarker:   SwapMarker;
    smChangeRangeSide: JumpToMatchDelim;

    smToggleCollapse:  ToggleCollapse(FCaretPos.Y);
    smCollapse:        CollapseNearest(FCaretPos.Y);
    smExpand:          if IsLineCollapsed(FCaretPos.Y) = 1 then ToggleCollapse(FCaretPos.Y);
    smFullCollapse:    FullCollapse;
    smFullExpand:      FullExpand;
    smCollapseSelection: CollapseLines(FSelStart, FSelStart + FSelLength);
    smToggleCollapseNearest:
                        if IsLineCollapsed(CaretPos.Y) in [csCollapsible,csCollapsed] then
                          ToggleCollapse(CaretPos.Y)
                        else
                          CollapseNearest(CaretPos.Y);

    smToggleNonPrinted:NonPrinted.Visible := not NonPrinted.Visible;
    smToggleWordWrap:  WordWrap := not WordWrap;
    smToggleFolding:   DisableFolding := not DisableFolding;
    smToggleLineNumbersVisible : LineNumbers.Visible := not Linenumbers.Visible;

    smIncrementalSearch: IncSearchStart;
    smCommentLines,
    smUncommentLines:   LineComments(Command = smCommentLines);
    smInSelCollapse:    FullCollapse(SelStart, SelStart + SelLength);
    smInSelExpand:      FullExpand(SelStart, SelStart + SelLength);
    smSortAscending:    SortSelection(True, False);
    smSortDescending:   SortSelection(False, False);
    smGotoLine:         begin
                          n := CaretPos.Y + 1;
                          if SelectLineNumber(n, Lines.Count) then
                            CaretPos := Point(0, n - 1);
                          if not Focused then
                            SetFocus;
                        end;
    smMoveLinesUp:     MoveSelLines(True);
    smMoveLinesDown:   MoveSelLines(False);
    smDuplicateLine:   DuplicateLine(CaretPos.Y);

    smMarkSelStart:     FMarkedSelStart := CaretStrPos;
    smMarkSelEnd:       if FMarkedSelStart <> -1 then
                          begin
                            n := CaretStrPos;
                            if FMarkedSelStart > n then
                              begin
                                i := n;
                                n := FMarkedSelStart;
                              end else
                                i := FMarkedSelStart;
                            SetSelection(i, n - i);
                          end;
    smAlignTokens:     AlignTokensSel;
    else
      if IsCommandEnabled(Command) then
        case Command of
          smFindDialog,
          smFindNext,
          smFindPrev,
          smFindAll,
          smFindFirst,
          smFindLast,
          smSearchAgain,
          smReplaceDialog,
          smReplaceNext,
          smReplacePrev,
          smReplaceAll,
          smReplaceFirst,
          smReplaceLast,
          smReplaceAgain:
            begin
              if Command >= smReplaceDialog then
                searchDlg := TSyntReplaceDialog(ClientOfType(TSyntReplaceDialog))
              else
                searchDlg := TSyntFindDialog(ClientOfType(TSyntFindDialog));
              case Command of
                smFindDialog,
                smReplaceDialog: searchDlg.Execute;
                smFindNext,
                smReplaceNext:   searchDlg.FindNext;
                smFindPrev,
                smReplacePrev:   searchDlg.FindPrev;
                smFindAll,
                smReplaceAll:    searchDlg.FindAll;
                smFindFirst,
                smReplaceFirst:  searchDlg.FindFirst;
                smFindLast,
                smReplaceLast:   searchDlg.FindLast;
                smReplaceAgain,
                smSearchAgain:   searchDlg.SearchAgain;
              end;
            end;
          smFindCurrentWordNext,
          smFindCurrentWordPrior:
            begin
              searchDlg := TSyntFindDialog(ClientOfType(TSyntFindDialog));
              if HaveSelection and (SelectMode <> msColumn) then
                begin
                  ws := SelStart;
                  we := ws + SelLength;
                end else
                begin
                  WordRangeAtPos(CaretPos, ws, we);
                end;
              searchDlg.FindText := Lines.SubString(ws + 1, we - ws);
              if Command = smFindCurrentWordNext then
                begin
                  CaretStrPos := we;
                  searchDlg.FindNext;
                end
              else
                begin
                  CaretStrPos := ws;
                  searchDlg.FindPrev;
                end
            end;

          smCopyPasteFileEnd:
            begin
              CopyToClipboard;
              ResetSelection;
              CaretStrPos := TextLength;
              PasteFromClipboard;
            end;
          smCopyPasteFileStart:
            begin
              CopyToClipboard;
              ResetSelection;
              CaretStrPos := 0;
              PasteFromClipboard;
            end;
          smCutPasteFileEnd:
            begin
              CutToClipboard;
              ResetSelection;
              CaretStrPos := TextLength;
              PasteFromClipboard;
            end;
          smCutPasteFileStart:
            begin
              CutToClipboard;
              ResetSelection;
              CaretStrPos := 0;
              PasteFromClipboard;
            end;
          smCopyPasteAbove:
            begin
              CopyToClipboard;
              if SelectMode = msColumn then
                CaretStrPos := CaretPosToStrPos(FBlock.TopLeft)
              else
                CaretStrPos := SelStart;
              ResetSelection;
              PasteFromClipboard;
            end;
          smCopyPasteBelow:
            begin
              CopyToClipboard;
              if SelectMode = msColumn then
                CaretStrPos := CaretPosToStrPos(Point(FBlock.Left, FBlock.Bottom + 1))
              else
                CaretStrPos := SelStart + SelLength;
              ResetSelection;
              PasteFromClipboard;
            end;
          smPrint,
          smPrintSelection:
            with TecSyntPrinter(ClientOfType(TecSyntPrinter)) do
              begin
                PrintSelection := Command = smPrintSelection;
                Print;
              end;
          smPrintPreview: Preview(TecSyntPrinter(ClientOfType(TecSyntPrinter)));
          smPageSetup: TecSyntPrinter(ClientOfType(TecSyntPrinter)).PageSetup;
          smSearchMarkReset: ResetSearchMarks;
          smSearchMarkNext: if FSearchMarks.Count > 0 then
                              begin
                                x := CaretStrPos;
                                n := FSearchMarks.NextAt(x);
                                if n = -1 then n := 0;
                                if (x >= FSearchMarks[n].StartPos) and (x < FSearchMarks[n].EndPos) then
                                  Inc(n);
                                if n >= FSearchMarks.Count then n := 0;
                                CaretStrPos := FSearchMarks[n].StartPos;
                              end;
          smSearchMarkPrev: if FSearchMarks.Count > 0 then
                              begin
                                x := CaretStrPos;
                                n := FSearchMarks.PriorAt(x);
                                if n = -1 then n := FSearchMarks.Count - 1;
                                if (x >= FSearchMarks[n].StartPos) and (x < FSearchMarks[n].EndPos) then
                                  Dec(n);
                                if n < 0 then n := FSearchMarks.Count - 1;
                                CaretStrPos := FSearchMarks[n].StartPos;
                              end;
        end;
   end;

  Handled := False;
  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecCommandHook, iHook) then
    begin
     iHook.AfterCommand(Command, Data, Handled);
     if Handled then
       Exit;
    end;
end;

function TCustomSyntaxMemo.IsCommandEnabled(Command: integer; Data: ecPointer): Boolean;
var x1, x2, i: integer;
    iHook: IecCommandHook;
    searchDlg: TSyntFindDialog;
begin
  if (Command < 300) and (Command > 0) then
   Command := Command mod 100;

  case Command of
      smLeft:        if soKeepCaretInText in FOptions then Result := (FCaretPos.X > 0) or (FCaretPos.Y > 0)
                                                      else Result := (FCaretPos.X > 0);
      smUp:          Result := FCaretPos.Y > 0;
      smDown:        Result := FCaretPos.Y < Lines.Count - 1;
      smLineStart:   Result := FCaretPos.X > 0;
      smLineEnd:     Result := FCaretPos.X < LineLength(FCaretPos.Y) - 1;
      smPageUp:      Result := FCaretPos.Y > 0;
      smPageDown:    Result := FCaretPos.Y < Lines.Count - 1;
      smPageLeft:    Result := FCaretPos.X > 0;
      smPageTop:     Result := FCaretPos.X > 0;
      smPageBottom:  Result := FCaretPos.Y < Lines.Count - 1;
      smEditorTop:   Result := FCaretPos.X > 0;
      smEditorBottom:Result := FCaretPos.Y < Lines.Count - 1;
      smGotoXY:      Result := Data <> nil;

      smScrollAbsUp,
      smScrollPageUp,
      smScrollUp:    Result := ScrollPosY > 0;
      smScrollAbsDown,
      smScrollPageDown,
      smScrollDown:  Result := ScrollPosY < FLineCount - 2;
      smScrollAbsLeft,
      smScrollPageLeft,
      smScrollLeft:  Result := ScrollPosX > 0;

      smCopyAsRTF,
      smCopy:        Result := HaveSelection;
      smCut:         Result := HaveSelection and not ReadOnly and CanClearSelection;
      smPaste:       Result := CanPaste;
      smUndo:        Result := CanUndo;
      smRedo:        Result := CanRedo;
      smSelectAll:   Result := Lines.Count > 0;
      smClearSelection: Result := HaveSelection and not ReadOnly and CanClearSelection;

      smDeleteLastChar: Result := not ReadOnly and ((FCaretPos.X > 0) or (FCaretPos.Y > 0) or HaveSelection);
      smDeleteChar,
      smDeleteLine,
      smDeleteWord,
      smDeleteLastWord,
      smDeleteBOL,
      smDeleteEOL,
      smClearAll:  Result := not ReadOnly and (Lines.Count > 0);

      smChar,
      smString:    Result := (Data <> nil) and not ReadOnly and CanInsertText(CaretStrPos);

      smLineBreak,
      smInsertLine,
      smTab,
      smTabChar:       Result := not ReadOnly and CanInsertText(CaretStrPos);

      smBlockIndent,
      smBlockUnindent: Result := not ReadOnly and (Lines.Count > 0);

      smUpperCase,
      smLowerCase,
      smToggleCase,
      smTitleCase:
        begin
         WordRangeAtPos(CaretPos, x1, x2);
         Result := (x2 > x1) and not ReadOnly;
        end;

      smUpperCaseBlock,
      smLowerCaseBlock,
      smTitleCaseBlock, //AT
      smToggleCaseBlock: Result := HaveSelection and not ReadOnly;//AT

      smInsertMode:    Result := not ReadOnly and ReplaceMode;
      smOverwriteMode: Result := not ReadOnly and not ReplaceMode;
      smToggleMode:    Result := not ReadOnly;

      smResetSelection:Result := HaveSelection;
      smColumnSelect:  Result := soEnableBlockSel in FOptions;

      smGotoBookmark0..smGotoBookmark9: Result := Bookmarks[Command - smGotoBookmark0] <> -1;
      smSwapMarker,
      smCollectMarker: Result := FMarkers.Count > 0;

      smToggleCollapse:  Result := IsLineCollapsed(CaretPos.Y) >= 0;
      smCollapse:        begin
                           x1 := IsLineCollapsed(FCaretPos.Y);
                           Result := (x1 = 0) or (x1 = -1) or (x1 = -2);
                         end;
      smExpand:          Result := IsLineCollapsed(FCaretPos.Y) = 1;
      smFullCollapse:    Result := Assigned(SyntObj) {and (SyntObj.CollapsibleCount > 0)} or
                                   (FUserRanges.FCollapsible.Count > 0);
      smFullExpand:      Result := FCollapsed.Count > 0;
      smCollapseSelection:Result := (SelectMode <> msColumn) and (FSelLength > 0) and
                    (StrPosToCaretPos(FSelStart).Y < StrPosToCaretPos(FSelStart + FSelLength).Y);
      smToggleCollapseNearest:
                         Result := IsLineCollapsed(FCaretPos.Y) <> csOutCollapse;
      smCommentLines,
      smUncommentLines:  Result := (SyntObj <> nil) {and (SyntObj.Owner.LineComment <> '') --AT} and not ReadOnly;
      smInSelCollapse:   Result := (SelLength > 0) and (SelectMode <> msColumn);
      smInSelExpand:     Result := (SelLength > 0) and (SelectMode <> msColumn) and (FCollapsed.Count > 0);

      smMacroRecStart,
      smMacroRecStop,
      smMacroRecCancel,
      smMacroPlay:       Result := False;
      smSortAscending,
      smSortDescending:  Result := CanSortSelection;
      smGotoLine:        Result := Lines.Count > 0; //AT
      smMoveLinesUp:     Result := CanMoveSelLines(True);
      smMoveLinesDown:   Result := CanMoveSelLines(False);
      smDuplicateLine:   Result := CaretPos.Y < Lines.Count;

      smFindCurrentWordNext,
      smFindCurrentWordPrior:
        Result := (ClientOfType(TSyntFindDialog) <> nil) and
                  (HaveSelection and (SelectMode <> msColumn) or (WordAtPos(CaretPos) <> ''));
      smFindDialog:      Result := ClientOfType(TSyntFindDialog) <> nil;
      smFindNext,
      smFindPrev,
      smFindAll,
      smFindFirst,
      smFindLast,
      smSearchAgain: begin
                       searchDlg := TSyntFindDialog(ClientOfType(TSyntFindDialog));
                       Result := Assigned(searchDlg) and (searchDlg.FindText <> '');
                     end;

      smReplaceDialog:   Result := ClientOfType(TSyntReplaceDialog) <> nil;
      smReplaceNext,
      smReplacePrev,
      smReplaceAll,
      smReplaceFirst,
      smReplaceLast,
      smReplaceAgain:begin
                       searchDlg := TSyntReplaceDialog(ClientOfType(TSyntReplaceDialog));
                       Result := Assigned(searchDlg) and (searchDlg.FindText <> '') and not ReadOnly;
                     end;

      smCopyPasteFileEnd,
      smCopyPasteFileStart,
      smCutPasteFileEnd,
      smCutPasteFileStart,
      smCopyPasteAbove,
      smCopyPasteBelow:    Result := HaveSelection and not ReadOnly;
      smAlignTokens:       Result := HaveSelection and not ReadOnly;
      smPrint:             Result := (ClientOfType(TecSyntPrinter) <> nil) and (TextLength > 0);
      smPrintSelection:    Result := (ClientOfType(TecSyntPrinter) <> nil) and HaveSelection;
      smPageSetup,
      smPrintPreview:      Result := (ClientOfType(TecSyntPrinter) <> nil);

      smSearchMarkReset,
      smSearchMarkNext,
      smSearchMarkPrev:    Result := FSearchMarks.Count > 0;
    else Result := True;
  end;

  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecCommandHook, iHook) then
     iHook.IsCommandEnabled(Command, Data, Result);

  if Assigned(FOnEnableCommand) then
    FOnEnableCommand(Self, Command, Data, Result);
end;

function TCustomSyntaxMemo.ClientOfType(AClass: TClass): TObject;
var i: integer;
begin
  Result := nil;
  for i := 0 to FSyntClients.Count - 1 do
    if TObject(FSyntClients[i]).ClassType = AClass then
      begin
        Result := TObject(FSyntClients[i]);
        Exit;
      end else
    if TObject(FSyntClients[i]).InheritsFrom(AClass) then
      Result := TObject(FSyntClients[i]);
end;

procedure TCustomSyntaxMemo.LoadFromFile(const FileName: Widestring);
begin
  try
    Lines.LoadFromFile(FileName);
  finally
    ClearCustObject;
    Modified := False;
    CaretPos := Point(0,0);
  end;
  if (ssoAutoLoad in FSaveState) and not SameText(ExtractFileExt(FileName), '.esi') then
    LoadStateFromFile(ChangeFileExt(FileName, '.esi'));
end;

procedure TCustomSyntaxMemo.SaveToFile(const FileName: WideString);
begin
  if not (soKeepTrailingBlanks in FOptions) then
    RemoveTrailingBlanks(soUndoAfterSave in FOptions);
//  else if not (soUndoAfterSave in FOptions) then ClearUndo;
  Lines.SaveToFile(FileName);
  Modified := False;
//  if soUndoAfterSave in FOptions then
//    Lines.SaveUndoState
//  else
//    Modified := False;

  if (ssoAutoSave in FSaveState) and not SameText(ExtractFileExt(FileName), '.esi') then
    SaveStateToFile(ChangeFileExt(FileName, '.esi'));

  Change;
end;

procedure TCustomSyntaxMemo.LoadStateFromFile(const FileName: string);
var ini: TIniFile;
    St: TStringList;
    i: integer;
    marker: TMarker;
begin
  // Restore state
  if not FileExists(FileName) then Exit;

  ini := TIniFile.Create(FileName);
  BeginUpdate;
  try
    if ssoUserRanges in FSaveState then
      FUserRanges.LoadFromFile(FileName);
    if ssoCaretPos in FSaveState then
      begin
        CaretPos := Point(
          ini.ReadInteger('Editor', 'CaretPosX', CaretPos.X),
          ini.ReadInteger('Editor', 'CaretPosY', CaretPos.Y));
      end;
    if ssoSelection in FSaveState then
      begin
        SelectMode := TSyntSelectionMode(ini.ReadInteger('Editor', 'SelectMode', integer(SelectMode)));
        if SelectMode = msColumn then
         with FBlock do
          begin
           Left := ini.ReadInteger('Editor', 'SelRect_Left', Left);
           Top := ini.ReadInteger('Editor', 'SelRect_Top', Top);
           Right := ini.ReadInteger('Editor', 'SelRect_Right', Right);
           Bottom := ini.ReadInteger('Editor', 'SelRect_Bottom', Bottom);
          end
         else
          begin
           FSelStart := ini.ReadInteger('Editor', 'SelStart', FSelStart);
           FSelLength := ini.ReadInteger('Editor', 'SelLength', FSelLength);
          end;
      end;
    if ssoWordWrap in FSaveState then
      WordWrap := ini.ReadBool('Editor', 'WordWrap', WordWrap);
    if ssoNonPrinted in FSaveState then
      FNonPrinted.Visible := ini.ReadBool('Editor', 'NonPrinted', FNonPrinted.Visible);
    if ssoScrollPos in FSaveState then
     begin
      TopLine := ini.ReadInteger('Editor', 'TopLine', TopLine);
      ScrollPosX := ini.ReadInteger('Editor', 'ScrollPosX', ScrollPosX);
      if WordWrap then
        ScrollPosY := ini.ReadInteger('Editor', 'ScrollPosY', ScrollPosY);
     end;
    if ssoDisableFolding in FSaveState then
      DisableFolding := ini.ReadBool('Editor', 'DisableFolding', DisableFolding);
    if ssoMarkers in FSaveState then
      begin
        St := TStringList.Create;
        try
          ini.ReadSection('Markers', St);
          FMarkers.Clear;
          for i := 0 to St.Count - 1 do
            begin
              marker := TMarker.Create;
              try
                marker.AsString := ini.ReadString('Markers', IntToStr(i), '');
                FMarkers.Add(marker);
              except
                marker.Free;
              end;
            end;
        finally
          St.Free;
        end;
      end;
    if ssoBookmarks in FSaveState then
      for i := 0 to 9 do
        Bookmarks[i] := ini.ReadInteger('Bookmarks', 'Bookmark' + IntToStr(i), -1);
  except
    Application.HandleException(Self);
  end;
  EndUpdate;
  ini.Free;
end;

procedure TCustomSyntaxMemo.SaveStateToFile(const FileName: string);
var ini: TIniFile;
    i: integer;
begin
  ini := TIniFile.Create(FileName);
  try
    if ssoUserRanges in FSaveState then
      FUserRanges.SaveToFile(FileName);
    ini.EraseSection('Editor');
    if ssoCaretPos in FSaveState then
      begin
        ini.WriteInteger('Editor', 'CaretPosX', CaretPos.X);
        ini.WriteInteger('Editor', 'CaretPosY', CaretPos.Y);
      end;
    if ssoSelection in FSaveState then
      begin
        ini.WriteInteger('Editor', 'SelectMode', integer(SelectMode));
        if FSelectMode = msColumn then
         with FBlock do
          begin
           ini.WriteInteger('Editor', 'SelRect_Left', Left);
           ini.WriteInteger('Editor', 'SelRect_Top', Top);
           ini.WriteInteger('Editor', 'SelRect_Right', Right);
           ini.WriteInteger('Editor', 'SelRect_Bottom', Bottom);
          end
         else
          begin
           ini.WriteInteger('Editor', 'SelStart', SelStart);
           ini.WriteInteger('Editor', 'SelLength', SelLength);
          end;
      end;
    if ssoWordWrap in FSaveState then
      ini.WriteBool('Editor', 'WordWrap', WordWrap);
    if ssoNonPrinted in FSaveState then
      ini.WriteBool('Editor', 'NonPrinted', FNonPrinted.Visible);
    if ssoScrollPos in FSaveState then
     begin
      ini.WriteInteger('Editor', 'TopLine', TopLine);
      ini.WriteInteger('Editor', 'ScrollPosY', ScrollPosY);
      ini.WriteInteger('Editor', 'ScrollPosX', ScrollPosX);
     end;
    if ssoDisableFolding in FSaveState then
      ini.WriteBool('DisableFolding', 'DisableFolding', DisableFolding);
    if ssoMarkers in FSaveState then
      begin
        ini.EraseSection('Markers');
        for i := 0 to FMarkers.Count - 1 do
          ini.WriteString('Markers', IntToStr(i), TMarker(FMarkers[i]).AsString);
      end;
    if ssoBookmarks in FSaveState then
      for i := 0 to 9 do
        ini.WriteInteger('Bookmarks', 'Bookmark' + IntToStr(i), Bookmarks[i]);
   except
     Application.HandleException(Self);
   end;
   ini.Free;
end;

function TCustomSyntaxMemo.GetCaretStrPos: integer;
begin
  Result := CaretPosToStrPos(FCaretPos);
end;

procedure TCustomSyntaxMemo.SetCaretStrPos(const Value: integer);
begin
  CaretPos := StrPosToCaretPos(Value);
end;

// Search marks

procedure TCustomSyntaxMemo.ResetSearchMarks;
begin
  if FSearchMarks.Count > 0 then
   begin
     BeginUpdate;
     try
       FSearchMarks.Clear;
     finally
       EndUpdate;
     end;
   end;
end;

procedure TCustomSyntaxMemo.SetSearchMark(sStart, sLength: integer; DoAdd: Boolean);
begin
  if sLength <= 0 then Exit;
  if not DoAdd then
   FSearchMarks.Clear;
  FSearchMarks.Add(TRange.Create(sStart, sStart + sLength));
  if not DoAdd then
   begin
     Invalidate;
     UpdateCaretPos;
   end;
end;

// Markers

function TCustomSyntaxMemo.CreateMarker(cp: TPoint): TMarker;
begin
  Result := TMarker.Create;
  Result.FScrollPos := FLeftTopPos;
  Result.FCaretPos := cp;
  Result.FPos := CaretPosToStrPos(cp);
  FMarkers.Add(Result);
end;

procedure TCustomSyntaxMemo.GotoMarker(Marker: TMarker);
begin
  ScrollPosX := Marker.FScrollPos.X;
  ScrollPosY := Marker.FScrollPos.Y;
  if soFloatMarkers in FOptions then CaretStrPos := Marker.Position
   else CaretPos := Marker.FCaretPos;
end;

function TCustomSyntaxMemo.CollectMarker: Boolean;
begin
  Result := FMarkers.Count > 0;
  if Result then
   begin
     GotoMarker(TMarker(FMarkers.Last));
     FMarkers.Delete(FMarkers.Count - 1);
     Invalidate;
     AnimateCaret(FAnimation.MarkerAnim);
   end;
end;

procedure TCustomSyntaxMemo.DropMarker(Pos: TPoint);
begin
  CreateMarker(Pos);
  Invalidate;
  AnimateCaret(FAnimation.MarkerAnim);
end;

function TCustomSyntaxMemo.SwapMarker: Boolean;
begin
  Result := FMarkers.Count > 0;
  if Result then
   begin
     CreateMarker(CaretPos);
     GotoMarker(TMarker(FMarkers[FMarkers.Count - 2]));
     FMarkers.Delete(FMarkers.Count - 2);
     Invalidate;
     AnimateCaret(FAnimation.MarkerAnim);
   end;
end;

procedure TCustomSyntaxMemo.AnimateCaret(AnimType: TAnimationType);
var p: TPoint;
begin
  p := CaretToMouse(FCaretPos.X, FCaretPos.Y);
  p.Y := p.Y + LineHeight(FCaretPos) {SingleLineHeight(FCaretPos.Y)} - 1;
  FAnimation.Animate(p, AnimType);
end;

function TCustomSyntaxMemo.JumpToMatchDelim: Boolean;
var p, i: integer;
begin
  Result := False;
  if not Assigned(SyntObj) then Exit;
  p := CaretStrPos;
  with SyntObj do
  for i := 0 to RangeCount - 1 do
   if Ranges[i].EndIdx <> -1 then // make sure that is a closed range
   if (Tags[Ranges[i].StartIdx].StartPos <= p) and (Tags[Ranges[i].StartIdx].EndPos >= p) then
    begin
      CaretStrPos := Tags[Ranges[i].EndIdx].StartPos;
      Result := True;
      Exit;
    end else
   if (Tags[Ranges[i].EndIdx].StartPos <= p) and (Tags[Ranges[i].EndIdx].EndPos >= p) then
    begin
      CaretStrPos := Tags[Ranges[i].StartIdx].StartPos;
      Result := True;
      Exit;
    end;
end;

procedure TCustomSyntaxMemo.SetUserRanges(const Value: TUserRanges);
begin
  FUserRanges.Assign(Value);
end;

function TCustomSyntaxMemo.CurrentUserRange: TUserRange;
begin
  if SelLength > 0 then Result := UserRanges.RangeAtSel(SelStart, SelLength)
   else Result := UserRanges.RangeAtPos(CaretStrPos);
end;

procedure TCustomSyntaxMemo.ClearCustObject;
begin
  FCollapsed.Clear;
  FMarkers.Clear;
  FBookmarks.Clear;
  FUserRanges.Clear;
  ClearUndo;
end;

function TCustomSyntaxMemo.CreateUserRange(ACollapsable: Boolean;
  AStyle: string; ALineBreaks: TLineBreakBound): TUserRange;
begin
  if SelLength > 0 then
     Result := UserRanges.AddRange(SelStart, SelStart + SelLength, ACollapsable, AStyle, ALineBreaks)
  else Result := nil;
end;

procedure TCustomSyntaxMemo.SetUserStyles(const Value: TSyntStyles);
begin
  if ChangeComponentReference(Self, Value, TComponent(FUserStyles)) then
    Invalidate;
end;

procedure TCustomSyntaxMemo.SetStaplePen(const Value: TPen);
begin
  FStaplePen.Assign(Value);
  Invalidate;
end;

procedure TCustomSyntaxMemo.SetStapleOffset(const Value: integer);
begin
  FStapleOffset := Value;
  Invalidate;
end;

function TCustomSyntaxMemo.GetTextLength: integer;
begin
  Result := Lines.TextLength;
end;

procedure TCustomSyntaxMemo.SetAbout(const Value: string);
begin
end;

procedure TCustomSyntaxMemo.SetAnimation(const Value: TSyntAnimation);
begin
  FAnimation.Assign(Value);
end;

procedure TCustomSyntaxMemo.RefreshSyntax;
begin
  BeginUpdate;
  try
    if Assigned(SyntObj) and not FSkipHltClear then SyntObj.Clear;
    FLinesDesc.Clear;
    ResetLineHeights(True);
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.SetWordWrap(const Value: Boolean);
var //ZD
  si: TScrollInfo; //ZD
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    //ZD start
    if FWordWrap then
      FLeftTopPos.X := 0
    else begin
      si.cbSize := sizeof(TScrollInfo);
      si.fMask := SIF_ALL;
      si.nMin := 0;
      si.nMax := 2;
      si.nPos := 0;
      si.nPage := 1;
      if FFlatScrollBars then begin
        FlatSB_SetScrollInfo(Handle, SB_HORZ, si, True);
        FlatSB_ShowScrollBar(Handle, SB_HORZ, True);
      end else begin
        SetScrollInfo(Handle, SB_HORZ, si, True);
        ShowScrollBar(Handle, SB_HORZ, True);
      end;
    end;
    // These lines are needed because horizontal scroll bar cannot be made visible
    // after it is hidden in Word Wrap mode which is caused by SIF_DISABLENOSCROLL
    //ZD end
    ResetLineHeights(True);
    UpdateEditor;
    ScrollCaret;
  end;
end;

procedure TCustomSyntaxMemo.SetLineSelection(sStart, sLength: integer; re_draw: Boolean);
var ss, sl: integer;
    p: TPoint;
begin
  p := StrPosToCaretPos(sStart);
  ss := CaretPosToStrPos(Point(0, p.Y));
  p := StrPosToCaretPos(sStart + sLength);
  sl := CaretPosToStrPos(Point(0, p.Y));
  if (soGreedySelect in FOptions) and (p.Y < Lines.Count) then
    Inc(sl, Lines.LineSpace(p.Y));
  IntSetSelection(ss, sl-ss, re_Draw);
end;

procedure TCustomSyntaxMemo.SetSelectModeDefault(const Value: TSyntSelectionMode);
begin
  if FSelectModeDefault <> Value then
  begin
    FSelectModeDefault := Value;
    if FSelectModeDefault = msNone then
    begin
      FSelectModeDefault := msNormal;
    end;
    SelectionChanged;
  end;
end;

procedure TCustomSyntaxMemo.SetSelectMode(const Value: TSyntSelectionMode);
begin
  if FSelectMode <> Value then
   begin
     if not (soEnableBlockSel in FOptions) and (Value = msColumn) then Exit;
     if Value = msColumn then // bug fixing ("persistent blocks" did not work)
       FSelLength := 0;
     FSelectMode := Value;
     case FSelectMode of
       msColumn:
         if soGreedySelect in FOptions then
           FBlock := Rect(FCaretPos.X, FCaretPos.Y, FCaretPos.X + 1, FCaretPos.Y);
       msLine:
         SetLineSelection(CaretStrPos, 0);
     end;
     Invalidate;
     SelectionChanged;
   end;
end;

//==============================================================================
//    Incremental Search
//==============================================================================

procedure TCustomSyntaxMemo.IncSearchStart(FromPos: integer);
begin
  if not HandleAllocated then Exit;
  if not Focused then
   Windows.SetFocus(Handle);
  FIncSearch := True;
  FIncSearchStr := '';
  if FromPos = -1 then FIncSearchPos := CaretStrPos
   else
    begin
      FIncSearchPos := FromPos;
      if FIncSearchPos >= TextLength then FIncSearchPos := TextLength - 1;
      if FIncSearchPos < 0 then FIncSearchPos := 0;
    end;
  if Assigned(FIncSearchChange) then
    FIncSearchChange(Self, isStart);
end;

procedure TCustomSyntaxMemo.SetIncSearchStr(const Value: ecString);
var sp: integer;
begin
  if FIncSearch then
   begin
     sp := SearchText(Lines.FText, Value, FIncSearchPos, FIncSearchBack, FIncSearchIgnoreCase);
     if sp > 0 then
      begin
       FIncSearchStr := Value;
       if FIncSearchBack then
        CaretStrPos := sp - 1
       else
        CaretStrPos := sp - 1 + Length(Value);
       SetSearchMark(sp - 1, Length(Value));
       if Assigned(FIncSearchChange) then
          FIncSearchChange(Self, isStrChange);
      end
   end;
end;

procedure TCustomSyntaxMemo.IncSearchStop;
begin
  if FIncSearch then
   begin
     FIncSearch := False;
     if Assigned(FIncSearchChange) then
       FIncSearchChange(Self, isStop);
   end;
end;

procedure TCustomSyntaxMemo.SetIncSearchBack(const Value: Boolean);
begin
  if FIncSearchBack <> Value then
   begin
    FIncSearchBack := Value;
    IncSearchStop;
   end;
end;

procedure TCustomSyntaxMemo.SetIncSearchIgnoreCase(const Value: Boolean);
begin
  if FIncSearchIgnoreCase <> Value then
   begin
    FIncSearchIgnoreCase := Value;
    IncSearchStop;
   end;
end;

procedure TCustomSyntaxMemo.SetLineSpacing(const Value: integer);
begin
  if (FLineSpacing <> Value) {and (Value >= 0)} then
   begin
    FLineSpacing := Value;
    ResetLineHeights;
   end;
end;

function TCustomSyntaxMemo.GetSyntObj: TClientSyntAnalyzer;
begin
  if Assigned(FSyntRanges) then Result := FSyntRanges else
   if Assigned(FTextSource) then Result := FTextSource.SyntObj
    else Result := nil;
end;

function TCustomSyntaxMemo.SkipHidden(X, Line: integer; Grow: Boolean): TPoint;
var len, p, i, t: integer;
    pCol: TCollapsedRange;
begin
  Result.X := X;
  Result.Y := SkipInvisible(Line, grow);
  if Result.Y >= Lines.Count then
    begin
      if soVirtualCaretPos in FOptionsEx then
        Exit
      else
        Result.Y := Lines.Count - 1;
    end;
  if Result.Y < 0 then Result.Y := 0;

  if (CollapseStyle <> csLineSeparator) and
     (GetColRangeTextAtPos(Result.Y, t) <> '') then  // v2.02
    begin
       t := t - Lines.LineIndex(Result.Y) + 1;
       if t < Result.X then
         if Grow then
          begin
           Result.X := 0;
           pCol := FCollapsed.GetCollapsed(Result.Y);
           if pCol <> nil then
            begin
             Result.Y := pCol.Line + pCol.LineCount;
             if Result.Y >= Lines.Count then Result.Y := Lines.Count - 1;
            end;
          end
         else
           Result.X := t;
    end;

  len := Lines.LineLength(Result.Y);
  if (X >= len) or (X = 0) then Exit;

  p := CaretPosToStrPos(Result);
  if Grow then
    for i := Result.X to len do
     begin
      SetCanvasAtPos(Canvas, p);
      if not FHiddenStyle then Break;
      Inc(p);
      Inc(Result.X);
     end
  else
    for i := Result.X downto 0 do
     begin
      SetCanvasAtPos(Canvas, p);
      if not FHiddenStyle then Break;
      Dec(p);
      Dec(Result.X);
     end;

  {$IFDEF EC_MBCS}
   if Lines.CharType(p + 1) = mbTrailByte then
     if Grow then
       Inc(Result.X)
     else
       Dec(Result.X);
  {$ENDIF}
end;

procedure TCustomSyntaxMemo.InvalidateTextRange(StartPos, EndPos: integer;
  UpdateHeights: Boolean);
var sLine, eLine: integer;
begin
  if EndPos > StartPos then
   begin
    sLine := StrPosToCaretPos(StartPos).Y;
    eLine := StrPosToCaretPos(EndPos).Y;
    InvalidateLines(sLine, eLine, UpdateHeights);
   end;
end;

procedure TCustomSyntaxMemo.InvalidateLines(sLine, eLine: integer; UpdateHeights: Boolean);
var y: integer;
    InvRect: TRect;
begin
  If UpdateHeights then
    ResetLineHeights(False, sLine, eLine);
  y := CaretToMouse(0, sLine).Y;
  if y < ClientHeight then
    begin
      InvRect := Rect(FMargin.Left, y, ClientWidth - FMargin.Right, ClientHeight - FMargin.Bottom);
      InvalidateRect(Handle, {$IFNDEF EC_DOTNET}@{$ENDIF}InvRect, False);
    end;
end;

procedure TCustomSyntaxMemo.CMMouseEnter(var Message: TMessage);
begin
  MouseEnter;
end;

procedure TCustomSyntaxMemo.CMMouseLeave(var Message: TMessage);
begin
  ResetHint;
  MouseLeave;
end;

procedure TCustomSyntaxMemo.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomSyntaxMemo.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

function TCustomSyntaxMemo.TokenAtPos(Pos: TPoint): integer;
begin
  if PtInRect(TextArea, Pos) and Assigned(SyntObj) then
   Result := SyntObj.TokenAtPos(CaretPosToStrPos(MouseToCaret(Pos.X, Pos.Y)))
  else
   Result := -1;
end;

procedure TCustomSyntaxMemo.SetFlatScrollBars(const Value: Boolean);
begin
  if FFlatScrollBars <> Value then
   begin
     FFlatScrollBars := Value;
     if HandleAllocated then
       begin
        if not FFlatScrollBars then
           UninitializeFlatSB(Handle)
        else
           InitializeFlatSB(Handle);
       end;
   end;
end;

procedure TCustomSyntaxMemo.SetDefaultStyles(const Value: TDefaultStyles);
begin
  FDefaultStyles.Assign(Value);
end;

procedure TCustomSyntaxMemo.Loaded;
begin
  inherited;
  ResetLineHeights(True);  // fix v2.02
  Lines.ResetLineStates;
  if Assigned(SyntObj) then
    SyntObj.Clear;
end;

procedure TCustomSyntaxMemo.SetCollapseBreakColor(const Value: TColor);
begin
  if FCollapseBreakColor <> Value then
   begin
    FCollapseBreakColor := Value;
    Invalidate;
   end;
end;

procedure TCustomSyntaxMemo.SetCollapseStyle(const Value: TCollapseStyle);
begin
  if FCollapseStyle <> Value then
   begin
    FCollapseStyle := Value;
    Invalidate;
   end;
end;

procedure TCustomSyntaxMemo.AnalyzeToPos(APos: integer);
var iObj: IecExternalFormatter;
    i: integer;
begin
  if SyntObj <> nil then
    SyntObj.TryAppend(APos);//AppendToPos(APos);
  // Execute editor plug-in
  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecExternalFormatter, iObj) then
     iObj.AnalyzeToPos(APos);
end;

//==============================================================================
//  Scroll section
//==============================================================================

function TCustomSyntaxMemo.VisibleCols: integer;
var
  W: Integer; //AT
begin
  W:= DefTextExt.cx;
  if W = 0 then
    Result := 0
  else
   begin
    Result := Floor((ClientWidth - FMargin.Left - FMargin.Right)/W);
    if Result < 0 then Result := 0;
   end;
end;

function TCustomSyntaxMemo.TextArea: TRect;
begin
  SetRect(Result, FMargin.Left, FMargin.Top,
                  ClientWidth - FMargin.Right,
                  ClientHeight - FMargin.Bottom);
end;

procedure TCustomSyntaxMemo.AdjustScrollBar;
var
  si: TScrollInfo;

  procedure ShowScroll(wBar: integer; bShow: Boolean);
  begin
    if FFlatScrollBars then
      FlatSB_ShowScrollBar(Handle, wBar, bShow)
    else
      ShowScrollBar(Handle, wBar, bShow);
  end;

  procedure Set_ScrollPos(nBar, nPos: Integer);
  begin
    if FFlatScrollBars then
      FlatSB_SetScrollPos(Handle, nBar, nPos, False)
    else
      SetScrollPos(Handle, nBar, nPos, False);
  end;

  procedure SetScrInfo(BarFlag: Integer);
  begin
    if FFlatScrollBars then
      FlatSB_SetScrollInfo(Handle, BarFlag, si, True)
    else
      SetScrollInfo(Handle, BarFlag, si, True);
  end;

  function DefineDelit: integer;
  begin
    Result := Ceil(si.nMax / MaxLinesResolution);
    if Result < 1 then Result := 1;
    if Result > 1 then
      begin
        si.nMax := Round(si.nMax / Result);
        si.nPage := Round(si.nPage / Result);
        if si.nPage < 1 then
          si.nPage := 1;
        si.nPos := Round(si.nPos / Result);
      end;
  end;

var
  VV: integer;
  bShowVert, bShowHorz: Boolean;
begin
  if FUpdateCount <> 0 then Exit;
  if HandleAllocated then
   begin
    si.cbSize := sizeof(TScrollInfo);
    si.fMask := SIF_ALL or SIF_DISABLENOSCROLL; //AT
    si.nMin := 0;

    case FCurScroll of
      ssVertical:
        if not WordWrap then
        begin
          Set_ScrollPos(SB_VERT, Round(ScrollPosY / FScrollDelit.Y));
          Exit;
        end;
      ssHorizontal:
        begin
          Set_ScrollPos(SB_HORZ, Round(ScrollPosX / FScrollDelit.X));
          Exit;
        end;
    end;

    bShowVert:= FScrollBars in [ssBoth, ssVertical];
    bShowHorz:= FScrollBars in [ssBoth, ssHorizontal];

    // Vertical Scroll Bar
    if bShowVert and FMultiLine and (Lines.Count > 0) then
     begin
      VV:= VisibleLines;
      if VV > 0 then //AT
        si.nPage := VV
      else
        si.nPage := 0;
      si.nMax := FLineCount;
      if (soScrollLastLine in FOptions) then begin //ZD
        if (si.nPage > 0) then
          Inc(si.nMax, si.nPage - 1)
      end else if ScrollPosY > FLineCount - integer(si.nPage) + 1 then //ZD
        ScrollPosY := FLineCount - integer(si.nPage) + 1; //ZD
        // There is a bug when text has small number of lines and vertical size of editor
        // is even smaler, when scroll bar is at the bottom after vertical enlargement
        // the scroll bar disapears, but the top of the text remains outside of the editor.
        //ZD

      si.nPos := ScrollPosY;
      FScrollDelit.Y := DefineDelit;
      if soScrollLastLine in FOptions then
        Dec(si.nMax);
    end
    else
    begin
      //AT disable scrollbar
      si.nMax := 1;
      si.nPos := 0;
      si.nPage := 2;
    end;

    SetScrInfo(SB_VERT);
    ShowScroll(SB_VERT, bShowVert);

    // Horizontal Scroll Bar
    if bShowHorz then
    begin
      si.nPage := VisibleCols;
      si.nPos := ScrollPosX;

      if FWordWrap then
      begin
        //ZD start
        VV := (ClientWidth - FMargin.Right - FMargin.Left) div DefTextExt.cx - 1;
        //lines taken back to hide horizontal scroll bar in Word Wrap mode
        bShowHorz := (soBreakOnRightMargin in FOptions) and (RightMargin > 0) and
             ((si.nPos > 0) or
             (VV < RightMargin));
        if bShowHorz then begin
          si.nMax := RightMargin;
          VV := si.nMax - VV;
          if si.nPos > VV then begin
            ScrollPosX := VV;
            si.nPos := VV;
          end;
        end else
          si.nMax := 0;
      end
      else begin
        VV := VisibleLinesWidth;
        if soVariableHorzScrollBar in FOptions then
        begin
          //bShowHorz := (VV >=  ClientWidth - FMargin.Left - FMargin.Right - DefTextExt.cx div 2) or (ScrollPosX <> 0);
          //if bShowHorz then
            //AT - orig method of calculating si.nMax is wrong
            si.nMax := Max(
              si.nPos + integer(si.nPage) - 1,
              VV div DefTextExt.cx); //ZD
            //AT
        end
        else begin
          if FMaximalLinesWidth < VV then FMaximalLinesWidth := VV;
          si.nMax := FMaximalLinesWidth div DefTextExt.cx;
        end;
      end;
      //ZD end

      FScrollDelit.X := DefineDelit;
    end;

    SetScrInfo(SB_HORZ);
    ShowScroll(SB_HORZ, bShowHorz);

    //RecalcIMEWindow; //AT commented, too slow
   end;
end;

procedure TCustomSyntaxMemo.Resize;
begin
  inherited;
  if FWordWrap and not (soBreakOnRightMargin in FOptions) then ResetLineHeights(True);
  if soScrollLastLine in FOptions then AdjustScrollBar
   else ScrollPosY := ScrollPosY; // ScrollWnd(LeftTopPos);
  AdjustScrollBar;
  UpdateCaretPos;
  RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE); //ZD
  // This line fixes problem with disapeared scroll bars when toggling
  // right or bottom panels.
  //ZD
end;

procedure TCustomSyntaxMemo.WMVScroll(var Message: TWMVScroll);
var FHintRect: TRect;
    S: string;
{    procedure CorrectPos;
    var si: TScrollInfo;
        mx: integer;
    begin
      si.fMask := SIF_ALL;
      if not GetScrollInfo(Handle, SB_VERT, si) then Exit;
      mx := FLineCount;
      if FScrollDelit.Y > 1 then
        si.nPage := si.nPage * FScrollDelit.Y;
      if (soScrollLastLine in FOptions) and (si.nPage > 0) then
        Inc(mx, si.nPage);
      if FScrollDelit.Y > 1 then
        mx := Round(mx / FScrollDelit.Y);
      if mx > si.nMax then
        Message.Pos := Round(Message.Pos * mx / si.nMax);
    end;}
begin
    case Message.ScrollCode of
      SB_LINEUP:    ExecCommand(smScrollUp);
      SB_PAGEUP:    ExecCommand(smScrollPageUp);
      SB_TOP:       ExecCommand(smScrollAbsUp);
      SB_LINEDOWN:  ExecCommand(smScrollDown);
      SB_PAGEDOWN:  ExecCommand(smScrollPageDown);
      SB_BOTTOM:    ExecCommand(smScrollAbsDown);
      SB_ENDSCROLL: begin
                      FCurScroll := ssNone;
                      AdjustScrollBar;
                    end;
      SB_THUMBTRACK:
       begin
{        if not FFlatScrollBars then
          CorrectPos;}
        FCurScroll := ssVertical;
        ScrollPosY := Message.Pos * FScrollDelit.Y;
        if shScroll in FHintProps.ShowHints then
         begin
           S := 'Line: ' + IntToStr(TopLine + 1);
           FHintRect := FHint.CalcHintRect(100, S, nil);
           OffsetRect(FHintRect, ClientToScreen(Point(ClientWidth, 0)).x -  FHintRect.Right, Mouse.CursorPos.y);
           ShowHintWnd(S, FHintRect, 0);
         end;

        if FWordWrap then //ZD
          RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE); //ZD
          // This line fixes bug when scrolling to the end of file which has many
          // lines that are much longer than that is displayed in the editor, i.e.
          // number of logical lines is much larger than the number of real lines.
          //ZD
       end;
     end;
    Message.Result := 0;

    Update;//AT
end;

procedure TCustomSyntaxMemo.WMHScroll(var Message: TWMHScroll);
  var //ZD
    si: TScrollInfo; //ZD
begin
  case Message.ScrollCode of
    //ZD start
    SB_LINEDOWN:
      begin
        si.fMask := SIF_ALL;
        if GetScrollInfo(Handle, SB_HORZ, si)
        and (ScrollPosX + 1 > si.nMax - integer(si.nPage) + 1) then
          ScrollPosX := si.nMax - integer(si.nPage) + 1
          // The right arrow button caused unlimited scroll to the right.
        else
          ExecCommand(smScrollRight);
      end;
    SB_PAGEDOWN:
      begin
        si.fMask := SIF_ALL;
        if GetScrollInfo(Handle, SB_HORZ, si)
        and (ScrollPosX + VisibleCols > si.nMax - integer(si.nPage) + 1) then
          ScrollPosX := si.nMax - integer(si.nPage) + 1
          // Click on the scroll area on the right side of thumb caused scrolling
          // that was to far after the text.
        else
          ExecCommand(smScrollPageRight);
      end;
    //ZD end
    SB_BOTTOM:    ExecCommand(smScrollAbsRight);
    SB_LINEUP:    ExecCommand(smScrollLeft);
    SB_PAGEUP:    ExecCommand(smScrollPageLeft);
    SB_TOP:       ExecCommand(smScrollAbsLeft);
    SB_ENDSCROLL: begin
                    FCurScroll := ssNone;
                    AdjustScrollBar;
                  end;
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FCurScroll := ssHorizontal;
        ScrollPosX := Message.Pos * FScrollDelit.X;
      end;
   end;
  Message.Result := 0;
end;
//==============================================================================
//==============================================================================
//==============================================================================
function TCustomSyntaxMemo.IntGetLineInfo(Line: integer): TLineInfo;
begin
  Result := nil;
  {$IFDEF EC_DOTNET}
  if (Line < FLinesDesc.Count) then
   begin
    if (FLinesDesc[Line] <> nil) and (FLinesDesc[Line] is TLineInfo) then
//       (LongWord(FLinesDesc[Line]) > LongWord(High(TLineState))) then
      Result := TLineInfo(FLinesDesc[Line]);
   end;
  {$ELSE}
   if (Line < FLinesDesc.Count) and (LongWord(FLinesDesc[Line]) > LongWord(High(TLineState))) then
      Result := TLineInfo(FLinesDesc[Line]);
  {$ENDIF}
end;

function TCustomSyntaxMemo.GetLineInfo(Line: integer): TLineInfo;
var re_calc: Boolean;
    init_count, res_count, W: integer;
begin
  // Default Info (for empty editor)
  if (Line < 0) or (Line >= Lines.Count) then
   begin
     Result := FDefaultLineDesc;
     Result.Heights[0] := DefTextExt.cy;
     Exit;
   end;

  // Insure valid descs array
  if Lines.Count > FLinesDesc.Count then
    begin
      FLinesDesc.Count := Lines.Count;
      CalcLineCount;
    end;

  if FLinesDesc[Line] = nil then
    begin
      Result := TLineInfo.Create;
      {$IFDEF EC_VCL5}
      FLinesDesc[Line] := nil;
      {$ENDIF}
      FLinesDesc[Line] := Result;  // Save result in array
      re_calc := True;
    end else
    begin
      Result := TLineInfo(FLinesDesc[Line]);
      re_calc := Result.Invalid;
    end;

  if re_calc then
    begin
      if Result.Hidden then init_count := 0
                       else init_count := Result.LineCount;
      // Calculates line size and word breakes
//      if Result.Invalid then Result.Clear;
      AnalyzeToPos(CaretPosToStrPos(Point(0, Line + 1)));
      if HandleAllocated then
        W := ClientWidth
      else
        W := Width;
      ProcessLine(Canvas, Result, Line, ptLineHeight,
        Rect(FMargin.Left - ScrollPosX * DefTextExt.cx, 0, W, 0), Point(0, 0));
      Result.FInvalid := False;
      if Assigned(FOnAfterCalcLineInfo) then
        FOnAfterCalcLineInfo(Self, Result, Line);
      // Updating number of lines (logical)
      if Result.Hidden then res_count := 0
                       else res_count := Result.LineCount;
      if init_count <> res_count then
       begin
        Inc(FLineCount, res_count - init_count);
        AdjustScrollBar;
       end;
      // Logical line count is changed
      if Line = FCaretPos.Y then
        IntSetCaretPos(SkipHidden(FCaretPos.X, Line, False));
    end;
end;

procedure TCustomSyntaxMemo.UpdateLineInfosAfterEdit(EditPos: TPoint;
  LineChange: integer);
var i, Line, tmp: integer;
    Info: TLineInfo;
begin
  Line := EditPos.Y;
  if LineChange < 0 then
   begin
    if EditPos.X = 0 then tmp := Line else tmp:= Line + 1;
    for i := tmp - LineChange - 1 downto tmp do
     if i < FLinesDesc.Count then
      FLinesDesc.Delete(i);
   end else
   if LineChange > 0 then
    for i := 1 to LineChange do
      if Line + i < FLinesDesc.Count then FLinesDesc.Insert(Line + i, nil)
       else FLinesDesc.Add(nil);
  Info := IntGetLineInfo(Line);
  if Info <> nil then
   with Info do
    begin
     Dec(Self.FLineCount, LineCount - 1);
     FInvalid := True;
     Clear;
    end;
  Inc(FLineCount, LineChange);
end;

procedure TCustomSyntaxMemo.ResetLineHeights(DoClear: Boolean; FromLine, ToLine: integer);
var i, e: integer;
    Info: TLineInfo;
begin
  if Lines.Count <> FLinesDesc.Count then
   begin
    FLinesDesc.Count := Lines.Count;
    if not DoClear then
      CalcLineCount;
   end;

  if (ToLine = -1) or (ToLine >= Lines.Count) then e := FLinesDesc.Count - 1
   else e := ToLine;

  for i := FromLine to e do
   begin
    Info := IntGetLineInfo(i);
    if Info <> nil then
     begin
       Info.FInvalid := True;
       if DoClear then
         Info.Clear;
     end;
   end;

  if DoClear then
    CalcLineCount;

//  UpdateObjectsPos;
end;

function TCustomSyntaxMemo.IterateVisibleLines(Proc: TLineIterator;
  FirstLine: integer; var Param1, Param2: integer; DefinedLine: Boolean;
  Grow: Boolean): integer;
var i, j, idx, scan: integer;
  function ProcessLine(Line: integer): Boolean;
  var Info: TLineInfo;
  begin
    Result := False;
    Info := IntGetLineInfo(Line);
    if DefinedLine and ((Info = nil) or Info.Invalid) then
      Info := GetLineInfo(Line);
    if (Info <> nil) and Info.Hidden then
      Exit;
    Proc(Line, Info, Param1, Param2, Result);
    if Result then
      begin
        IterateVisibleLines := Line;
        Exit;
      end;
  end;
begin
  if (FirstLine < 0) or (FirstLine >= Lines.Count) then
     begin
       Result := 0;
       Exit;
     end;
  scan := FirstLine;
  idx := FCollapsed.FDiaps.PriorAt(FirstLine);

  if Grow then
   begin
    if not FDisableFolding then
     begin
      if (idx <> -1) and (FCollapsed.FDiaps[idx].EndPos > FirstLine) then
       scan := FCollapsed.FDiaps[idx].EndPos;
      Inc(idx);
      for i := idx to FCollapsed.FDiaps.Count - 1 do
       begin
        for j := scan to FCollapsed.FDiaps[i].StartPos - 1 do
         if ProcessLine(j) then Exit;
        scan := FCollapsed.FDiaps[i].EndPos;
       end;
     end;
    for j := scan to Lines.Count - 1 do
     if ProcessLine(j) then Exit;
    Result := Lines.Count;
   end else
   begin
    if not FDisableFolding then
     begin
      if (idx <> -1) and (FCollapsed.FDiaps[idx].EndPos > FirstLine) then
       begin
         scan := FCollapsed.FDiaps[idx].StartPos - 1;
         Dec(idx);
       end;
      for i := idx downto 0 do
       begin
        for j := scan downto FCollapsed.FDiaps[i].EndPos do
         if ProcessLine(j) then Exit;
        scan := FCollapsed.FDiaps[i].StartPos - 1;
       end;
     end;
    for j := scan downto 0 do
     if ProcessLine(j) then Exit;
    Result := -1;
   end;
end;

procedure TCustomSyntaxMemo.CountLines(Line: integer; Info: TLineInfo;
   var Param1, Param2: integer; var Stop: Boolean);
begin
 if Info = nil then Inc(Param1)
  else Inc(Param1, Info.LineCount);
end;

procedure TCustomSyntaxMemo.ScanLog(Line: integer; Info: TLineInfo;
   var Param1, Param2: integer; var Stop: Boolean);
begin
 if Info = nil then Dec(Param1)
  else Dec(Param1, Info.LineCount);
 Stop := Param1 < 0;
end;

procedure TCustomSyntaxMemo.CountHeight(Line: integer; Info: TLineInfo;
   var Param1, Param2: integer; var Stop: Boolean);
begin
 if Line > Param2 then Stop := True else
  if (Info = nil) or (Info.Height = 0) then Inc(Param1, DefTextExt.cy)
   else Inc(Param1, Info.Height);
end;

procedure TCustomSyntaxMemo.IsLineAtPos(Line: integer; Info: TLineInfo;
   var Param1, Param2: integer; var Stop: Boolean);
var H: integer;
begin
 if (Info = nil) or (Info.Height = 0) then H := DefTextExt.cy
   else H := Info.Height;
 if Param1 < H then Stop := True
  else Dec(Param1, H);
end;

procedure TCustomSyntaxMemo.CountLogLines(Line: integer; Info: TLineInfo;
   var Param1, Param2: integer; var Stop: Boolean);
var i: integer;
  procedure ProcLine(H: integer);
  begin
    if H = 0 then Exit;
    if H > Param1 then Stop := True
     else
      begin
       Dec(Param1, H);
       Inc(Param2);
      end;
  end;
begin
 if Info = nil then ProcLine(DefTextExt.cy) else
 for i := Info.LineCount - 1 downto 0 do
  begin
    ProcLine(Info.Heights[i]);
    if Stop then Exit;
  end;
end;

procedure TCustomSyntaxMemo.CountLinesWidth(Line: integer; Info: TLineInfo;
   var Param1, Param2: integer; var Stop: Boolean);
begin
 if Info <> nil then
   begin
     Dec(Param1, Info.Height);
     if Param2 < Info.Width then
       Param2 := Info.Width;
     Stop := Param1 <= 0;
   end;
end;

var NULL: integer = 0; // For not used parameters

procedure TCustomSyntaxMemo.VisDetect(Line: integer; Info: TLineInfo; var Param1, Param2: integer; var Stop: Boolean);
begin
  Stop := True;
end;

function TCustomSyntaxMemo.SkipInvisible(Line: integer; Grow: Boolean): integer;
begin
  if (soVirtualCaretPos in FOptionsEx) and (Line >= Lines.Count) then
    begin
      Result := Line;
      Exit;
    end;

  Result := IterateVisibleLines(VisDetect, Line, NULL, NULL, True, Grow);
  if Grow then
   begin
    if Result < Line then Result := Line
   end else
    if Result > Line then Result := Line;
end;

function TCustomSyntaxMemo.CalcLineCount(Exact: Boolean): integer;
begin
  Result := 0;
  IterateVisibleLines(CountLines, 0, Result, NULL, Exact, True);
  if (FLineCount <> Result) then
   begin
     FLineCount := Result;
     AdjustScrollBar;
   end;
end;

function TCustomSyntaxMemo.GetLogTopLine: integer;
begin
  Result := LineToLog(TopLine, FTopSubLine);
end;

procedure TCustomSyntaxMemo.SetLogTopLine(Line: integer);
var YScroll, YSubLine: integer;
begin
    LogToLine(Line, YScroll, YSubLine);
    ScrollText(FLeftTopPos.X, YScroll, YSubLine);
end;

function TCustomSyntaxMemo.LineToLog(Line, SubLine: integer): integer;
begin
  with GetLineInfo(Line) do
   if SubLine < LineCount then Result := SubLine
    else Result := 0;
  IterateVisibleLines(CountLines, Line - 1, Result, NULL, False, False);
end;

procedure TCustomSyntaxMemo.LogToLine(LogLine: integer; var Line,
  SubLine: integer);
var Info: TLineInfo;
begin
  if LogLine < 0 then LogLine := 0;
  Line := IterateVisibleLines(ScanLog, 0, LogLine, NULL, False, True);
  Info := IntGetLineInfo(Line);
  if Info <> nil then
     SubLine := Info.LineCount + LogLine
  else
     SubLine := 0;
end;

function TCustomSyntaxMemo.GetTopLine: integer;
begin
  Result := FLeftTopPos.Y;
end;

procedure TCustomSyntaxMemo.SetTopLine(Value: integer);
begin
  if Value > Lines.Count then Value := Lines.Count;
  if Value < 0 then Value := 0;
  if Value <> GetTopLine then
   ScrollPosY := LineToLog(Value, 0);
end;

function TCustomSyntaxMemo.GetScrollPosX: integer;
begin
  Result := FLeftTopPos.X;
end;

procedure TCustomSyntaxMemo.SetScrollPosX(Value: integer);
begin
  if Value < 0 then Value := 0;
  if GetScrollPosX <> Value then
   begin
     ScrollText(Value, FLeftTopPos.Y, FTopSubLine);
{    FLeftTopPos.X := Value;
    UpdateEditor;
    DoScroll;}
   end;
end;

procedure TCustomSyntaxMemo.CollapsedChanged;
begin
  CalcLineCount;
  IntSetCaretPos(SkipHidden(FCaretPos.X, FCaretPos.Y, False));
//  UpdateObjectsPos;
  UpdateEditor;
end;

function TCustomSyntaxMemo.ScrollCaret: Boolean;
begin
  Result := ScrollCaret(FCaretPos, saNearestEdge, saNearestEdge);
end;

function TCustomSyntaxMemo.ScrollCaret(Cp: TPoint; VertAlign, HorzAlign: TScrollAlignment): Boolean;
var tWidth, tHeight, LnHeight: integer;
    ALeftTopPosY, ALeftTopPosX, ATopSubLine: integer;
    p: TPoint;

  procedure ScrollDown(H: integer);
  begin
    if ALeftTopPosY > Lines.Count then // virtual position
      begin
        if Lines.Count = 0 then
          begin
            Dec(H, ALeftTopPosY * DefLineHeight);
            ALeftTopPosY := 0;
            ATopSubLine := 0;
          end else
          begin
            Dec(H, (ALeftTopPosY - Lines.Count + 1) * DefLineHeight);
            ALeftTopPosY := Lines.Count - 1;
            ATopSubLine := GetLineInfo(ALeftTopPosY).LineCount - 1;
          end;
        if H <= 0 then
          begin
            // Caret is not visible
            if (Cp.X = FCaretPos.X) and (Cp.Y = FCaretPos.Y) and not (soUnlimitedCaretPos in FOptionsEx) then
              CaretPos := Point(FCaretPos.X, FCaretPos.Y + H div DefLineHeight - 1);
            Exit;
          end;
      end;

    with GetLineInfo(ALeftTopPosY) do
     begin
      while ATopSubLine > 0 do
       begin
        Dec(H, Heights[ATopSubLine]);
        if H <= 0 then Exit;
        Dec(ATopSubLine);
       end;
      Dec(H, Heights[0]);
     end;
    if H <= 0 then Exit;
    ALeftTopPosY := IterateVisibleLines(IsLineAtPos, ALeftTopPosY - 1, H, NULL, True, False);
    if ALeftTopPosY < 0 then
     begin
      ALeftTopPosY := 0;
      ATopSubLine := 0;
     end else
     with GetLineInfo(ALeftTopPosY) do
     begin
      ATopSubLine := LineCount - 1;
      while (ATopSubLine > 0) and (H > 0) do
       begin
        Dec(H, Heights[ATopSubLine]);
        if H <= 0 then Exit;
        Dec(ATopSubLine);
       end;
     end;
  end;

begin
  Result := False;
  if not HandleAllocated then
    Exit;

  if not (soVirtualCaretPos in FOptionsEx) then
    begin
      if Cp.Y > Lines.Count  then
        if Lines.Count = 0 then Exit
          else Cp.Y := Lines.Count - 1;
    end;

  // Expand collapsed blocks
  if ShowLine(Cp.Y) then
    CollapsedChanged;

  // Current window position of the caret
  p := CaretToMouse(Cp.X, Cp.Y);
  p.X := p.X - FMargin.Left;
  p.Y := p.Y - FMargin.Top;
  tWidth := ClientWidth - FMargin.Right - FMargin.Left;
  tHeight := ClientHeight - FMargin.Top - FMargin.Bottom;
  LnHeight := LineHeight(Cp);

  // Make caret visible
  ALeftTopPosX := FLeftTopPos.X;
  if not FWordWrap or (soBreakOnRightMargin in FOptions) then
    begin
      if HorzAlign = saNearestEdge then
        if p.X < 0 then HorzAlign := saMinEdge else
          if p.X > tWidth then HorzAlign := saMaxEdge
            else HorzAlign := saNone;

      case HorzAlign of
        saMinEdge: ALeftTopPosX := FLeftTopPos.X + Floor(p.X / DefTextExt.cx);
        saCenter:  ALeftTopPosX := FLeftTopPos.X + Ceil((p.X - tWidth/2)/ DefTextExt.cx);
        saMaxEdge: ALeftTopPosX := FLeftTopPos.X + Ceil((p.X - tWidth)/ DefTextExt.cx);
      end;

      if ALeftTopPosX < 0 then
        ALeftTopPosX := 0;
   end;

  ALeftTopPosY := Cp.Y;
  ATopSubLine := GetLineInfo(Cp.Y).SubLineForPos(Cp.X);

  if VertAlign = saNearestEdge then
    if p.Y < 0 then VertAlign := saMinEdge else
      if p.Y > tHeight - LnHeight then VertAlign := saMaxEdge
        else VertAlign := saNone;

  case VertAlign of
    saMinEdge: ScrollDown(0);
    saCenter:  ScrollDown((tHeight - LnHeight) div 2);
    saMaxEdge: ScrollDown(tHeight - LnHeight);
    saNone:    begin
                 ALeftTopPosY := FLeftTopPos.Y;
                 ATopSubLine := FTopSubLine;
               end;
  end;

  Result := ScrollText(ALeftTopPosX, ALeftTopPosY, ATopSubLine);
{  Result := (FLeftTopPos.Y <> ALeftTopPosY) or
            (FLeftTopPos.X <> ALeftTopPosX) or
            (FTopSubLine <> ATopSubLine);

  if Result then
    begin
      FLeftTopPos.X := ALeftTopPosX;
      FLeftTopPos.Y := ALeftTopPosY;
      FTopSubLine := ATopSubLine;
      DoScroll;
      UpdateEditor;
    end;}
end;

// line rect (without top position)
function TCustomSyntaxMemo.GetLineRect(Line: integer): TRect;
begin
  Result.Left := FMargin.Left - FLeftTopPos.X * DefTextExt.cx; // starting point
  Result.Right := ClientWidth;
  Result.Top := 0;
  Result.Bottom := GetLineInfo(Line).Height; //SingleLineHeight(Line);
end;

function TCustomSyntaxMemo.SourceTopLinePos: integer;
var i: integer;
begin
  Result := FMargin.Top;
  if FTopSubLine > 0 then
   with GetLineInfo(TopLine) do
    for i := FTopSubLine - 1 downto 0 do
     Dec(Result, Heights[i]);
end;

// Положение каретки => координаты окна
function TCustomSyntaxMemo.CaretToMouse(cX, cY: integer): TPoint;
var p: TPoint;
    Y2: integer;
begin
  LineBound(cY, Result.Y, Y2);
  if (soVirtualCaretPos in FOptionsEx) and (cY >= Lines.Count) then
    begin
      Inc(Result.Y, (cY - Lines.Count) * DefLineHeight);
      Result.X := (cX - FLeftTopPos.X) * DefTextExt.cX + FMargin.Left;
      Exit;
    end;
  p := ProcessLine(Canvas, GetLineInfo(cy), cY, ptTextToMouse, GetLineRect(cY), Point(cX, 0));
  Result.x := p.X;
  if Result.x = 0 then Result.x := FMargin.Left;
  Inc(Result.Y, p.Y);
end;

procedure TCustomSyntaxMemo.LineBound(Line: integer; var Y1, Y2: integer; Exact: Boolean);
var L, Y: integer;
begin
  Y := 0;
  Y1 := SourceTopLinePos;
  if Line < FLeftTopPos.Y then
   begin
     L := FLeftTopPos.Y - 1;
     IterateVisibleLines(CountHeight, Line, Y, L, Exact);
     Dec(Y1, Y);
   end else
  if Line > FLeftTopPos.Y then
   begin
     L := Line - 1;
     IterateVisibleLines(CountHeight, FLeftTopPos.Y, Y, L, Exact);
     Inc(Y1, Y);
   end;
  Y2 := Y1 + GetLineInfo(Line).Height;
end;

// Line number from Y mouse position
function TCustomSyntaxMemo.LineAtPos(var Y: integer): integer;
var dH: integer;
begin
  Dec(Y, SourceTopLinePos);
  if Y >= 0 then
    begin
      Result := IterateVisibleLines(IsLineAtPos, FLeftTopPos.Y, Y, NULL, False, True);
      if (soVirtualCaretPos in FOptionsEx) and (Result >= Lines.Count - 1) then
        begin
          dH := DefLineHeight;
          if Y > dH then
            begin
              Result := Result + Y div dH;
              Y := Y mod dH;
            end;
        end;
    end
  else
    begin
     Y := -Y;
     Result := IterateVisibleLines(IsLineAtPos, FLeftTopPos.Y - 1, Y, NULL, False, False);
     if Result >= FLeftTopPos.Y then
       Y := 0 else
     if Result < Lines.Count then
       Y := GetLineInfo(Result).Height - Y;
    end;
end;

// Coordinates in window => caret position
function TCustomSyntaxMemo.MouseToCaret(X, Y: integer): TPoint;
begin
  Result.Y := LineAtPos(Y);
  if Result.Y < 0 then Result := Point(0, 0) else
   if Result.Y > Lines.Count - 1 then
     begin
       if soVirtualCaretPos in FOptionsEx then
         Result.X := (X - FMargin.Left) div DefTextExt.cx + FLeftTopPos.X
       else
         Result := StrPosToCaretPos(Lines.TextLength);
     end
    else Result.X := ProcessLine(Canvas, GetLineInfo(Result.Y), Result.Y,
                                ptMouseToText, GetLineRect(Result.Y), Point(X, Y)).X
end;

function TCustomSyntaxMemo.IsOverCollapsedTextIcon(X, Y: integer): Boolean;
var Line: integer;
begin
  Result := False;
  if FCollapseStyle = csLineSeparator then Exit;
  Line := LineAtPos(Y);
  if (Line < 0) or (Line > Lines.Count - 1) then Exit;
  if ProcessLine(Canvas, GetLineInfo(Line), Line, ptTestOverCollapsed, GetLineRect(Line), Point(X, Y)).X > 0 then
    Result := True;
end;

function TCustomSyntaxMemo.LineHeight(Line: integer): integer;
begin
  if not FCollapsed.IsLineVisible(Line) then Result := 0 else
   with GetLineInfo(Line) do
    if Visible then Result := Height
     else Result := 0;
end;

function TCustomSyntaxMemo.LineHeight(cp: TPoint): integer;
begin
  if not FCollapsed.IsLineVisible(cp.Y) then Result := 0 else
   with GetLineInfo(cp.Y) do
    if Visible then Result := Heights[SubLineForPos(cp.X)]
     else Result := 0;
end;

function TCustomSyntaxMemo.SingleLineHeight(Line: integer): integer;
begin
  if Lines.Count = 0 then Result := DefTextExt.cy else
  with GetLineInfo(Line) do
   if Visible then Result := Heights[0]
    else Result := 0;
end;

function TCustomSyntaxMemo.VisibleLines: integer;
var CurHeight: integer;
begin
  Result := 0;
  CurHeight := ClientHeight;
  if CurHeight = 0 then
    CurHeight := Height;
  CurHeight := CurHeight - FMargin.Top - FMargin.Bottom;
  IterateVisibleLines(CountLogLines, FLeftTopPos.Y, CurHeight, Result, True);
  Inc(Result, Round(CurHeight / DefTextExt.cy));
end;

function TCustomSyntaxMemo.VisibleLinesWidth: integer;
var CurHeight: integer;
begin
  Result := 0;
  CurHeight := Height - FMargin.Top - FMargin.Bottom;
  IterateVisibleLines(CountLinesWidth, FLeftTopPos.Y, CurHeight, Result, True);
end;

function TCustomSyntaxMemo.ShowLine(aLine: integer): Boolean;
begin
  Result := ShowLines(aLine, aLine);
end;

function TCustomSyntaxMemo.ShowLines(FirstLine, LastLine: integer): Boolean;
var i, j: integer;
    Info: TLineInfo;
begin
  Result := False;
  if FDisableFolding then Exit;
  if LastLine = -1 then
    LastLine := Lines.Count - 1;

  with FCollapsed do
  for i := FCollapsed.Count - 1 downto 0 do
   with Ranges[i] do
    if (LastLine > Line) and (FirstLine < Line + LineCount) then
    begin
      FList.Delete(i);
      for j := Line + LineCount downto Line do
       begin
        Info := IntGetLineInfo(Line);
        if Info <> nil then
         with Info do
          begin
           FInvalid := True;
           Clear;
          end;
       end;
      Result := True;
    end;
  if Result then
    begin
      FCollapsed.UpdateDiaps;
      CalcLineCount;
      AdjustScrollBar;
    end;
end;

procedure TCustomSyntaxMemo.SetLineStateDisplay(const Value: TLineStateDisplay);
begin
  FLineStateDisplay.Assign(Value);
end;

function TCustomSyntaxMemo.CreateStdPopup: TPopupMenu;
var mi: TMenuItem;
    KeyMap: TSyntKeyMapping;
  procedure AddSeparator;
  begin
    mi := TMenuItem.Create(Self);
    mi.Caption := '-';
    CreateStdPopup.Items.Add(mi);
  end;
  procedure AddCommand(cmd: integer);
  var cObj: TecCommandItem;
  begin
    cObj := KeyMap.CommandByID(cmd);
    if cObj = nil then Exit;
    mi := TMenuItem.Create(Self);
    mi.Caption := cObj.Caption;
    mi.Tag := cmd;
    mi.Enabled := IsCommandEnabled(cmd);
    mi.OnClick := StdCmdPopupExec;
    CreateStdPopup.Items.Add(mi);
  end;
begin
  if Assigned(FKeyMapping) then
    KeyMap := FKeyMapping
  else
    KeyMap := DefaultKeyMapping;

  Result := TPopupMenu.Create(Self);
  AddCommand(smUndo);
  AddCommand(smRedo);
  AddSeparator;
  AddCommand(smCut);
  AddCommand(smCopy);
  AddCommand(smPaste);
  AddCommand(smClearSelection);
  AddSeparator;
  AddCommand(smSelectAll);
end;

procedure TCustomSyntaxMemo.StdCmdPopupExec(Sender: TObject);
begin
  ExecCommand((Sender as TComponent).Tag);
end;

function TCustomSyntaxMemo.GetCursor(x, y: integer;
  Shift: TShiftState): TCursor;
var i: integer;
    iObj: IecSyntMemoPlugIn;
begin
  if IsOverCollapsedTextIcon(X, Y) then Result := crHandPoint else
   if HaveSelection and (soDragText in FOptions) and not ReadOnly and
      PosInSelection(CaretPosToStrPos(MouseToCaret(X, Y))) then Result := crDefault
   else Result := FCursor;

  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecSyntMemoPlugIn, iObj) then
    iObj.UpdateCursor(X, Y, Shift, Result);
end;

function TCustomSyntaxMemo.GetMouseSelMode(Shift: TShiftState): TSyntSelectionMode;
begin
  //AT - changed ":= FSelectMode" to ":= FSelectModeDefault"
  if (Shift * [ssAlt, ssShift, ssCtrl] = []) and (soKeepSelMode in FOptions) then
    Result := FSelectModeDefault
  else
    begin
      Result := msNone;
      if ssAlt in Shift then
       if ssCtrl in Shift then Result := msLine
        else Result := msColumn;
    end;

  if Assigned(FOnGetMouseSelMode) then
    FOnGetMouseSelMode(Self, Shift, Result);
end;

procedure TCustomSyntaxMemo.SetSyncEditing(const Value: TSyntSyncEdit);
begin
  FSyncEditing.Assign(Value);
end;

procedure TCustomSyntaxMemo.LineComments(CommentLines: Boolean;
  FirstLine: integer; LastLine: integer);
var cs, S: ecString;
    i, com_pos: integer;
    OldCP: TPoint;
   function IsEmpty(const st: ecString): Boolean;
   var i: integer;
   begin
     for i := 1 to Length(st) do
      if not IsSpaceChar(st[i]) then
       begin
         Result := False;
         Exit;
       end;
     Result := True;
   end;

var
  An: TSyntAnalyzer;
begin
  //AT- used comment for lexer-under-caret, not global lexer
  {
  if (SyntObj <> nil) then
    cs := SyntObj.Owner.LineComment
  else Exit;
  }
  cs := '';
  An := nil;
  if SyntObj<>nil then
    An := SyntObj.AnalyzerAtPos(CaretStrPos);
  if An<>nil then
    cs := An.LineComment;
  if cs = '' then Exit;

  if FirstLine = -1 then
    GetSelectedLines(FirstLine, LastLine);
  if LastLine = -1 then
    LastLine := Lines.Count - 1;
  if FirstLine > LastLine then Exit;
  OldCP := CaretPos;
  BeginUpdate;
  ResetSelection;
  try
    for i := LastLine downto FirstLine do
     begin
       if i < Lines.Count then S := Lines[i]
         else S := '';
       com_pos := Pos(cs, S);
       if CommentLines then
        begin
{          if S = '' then
            Continue;
          if (com_pos > 0) and IsEmpty(Copy(S, 1, com_pos - 1)) then
            Continue;}
          FCaretPos := Point(0, i);
          InsertText(cs);
        end else
         if com_pos > 0 then
          begin
            FCaretPos := Point(com_pos - 1, i);
            DeleteText(Length(cs));
          end;
     end;
    CaretPos := OldCP;
  finally
    EndUpdate;
  end;
end;

//AT: changed some ifs
function TCustomSyntaxMemo.GetSelectedLines(var FirstLine, LastLine: integer): integer;
begin
  if HaveSelection and (SelectMode = msColumn) then
   begin
     FirstLine := FBlock.Top;
     LastLine := FBlock.Bottom;
   end else
  if FSelLength > 0 then 
  begin
     FirstLine := StrPosToCaretPos(FSelStart).Y;
     with StrPosToCaretPos(FSelStart + FSelLength) do
      if (X = 0) and (Y > 0) then
       LastLine := Y - 1
      else
       LastLine := Y;
     if LastLine < FirstLine then
       LastLine := FirstLine;
  end
  else
  begin
    FirstLine := FCaretPos.Y;
    LastLine := FirstLine;
  end;
  Result := LastLine - FirstLine + 1;
end;

procedure TCustomSyntaxMemo.SetDisableFolding(const Value: Boolean);
begin
  if FDisableFolding = Value then Exit;
  FDisableFolding := Value;
  BeginUpdate;
  try
    if FCollapsed.Count > 0 then
      ResetLineHeights(True);
    if not FDisableFolding then
      IntSetCaretPos(SkipHidden(CaretPos.X, CaretPos.Y, False));
  finally
    EndUpdate;
  end;
end;

// Determines whether mono font may be used
procedure TCustomSyntaxMemo.UpdateMonoFontFlag;
var CheckedLexers: TList;

  function CheckStyle(stl: TSyntaxFormat): Boolean;
  begin
   Result := (stl = nil) or
             (not stl.Enabled) or
             (stl.FormatType <> ftCustomFont) or
             (stl.Font.Size = Font.Size) and
             (stl.Font.Name = Font.Name) and
             (stl.Font.Pitch in [fpFixed, fpDefault]);
  end;

  function CheckRules(Rules: TSyntCollection): Boolean;
  var i: integer;
  begin
    Result := True;
    if not Rules.ItemClass.InheritsFrom(TRuleCollectionItem) then Exit;
    for i := 0 to Rules.Count - 1 do
     with TRuleCollectionItem(Rules[i]) do
      if Enabled and not CheckStyle(Style) then
       begin
        Result := False;
        Exit;
       end;
  end;

  function CheckLexer(Lexer: TSyntAnalyzer): Boolean;
  var i: integer;
  begin

    Result := CheckStyle(Lexer.MarkedBlock) and
              CheckStyle(Lexer.SearchMatch) and
              CheckStyle(Lexer.CurrentLine) and
              CheckStyle(Lexer.DefStyle) and
              CheckRules(Lexer.TokenRules) and
              CheckRules(Lexer.BlockRules) and
              CheckRules(Lexer.SubAnalyzers);

    CheckedLexers.Add(Lexer);
    if Result then
     for i := 0 to Lexer.SubAnalyzers.Count - 1 do
      with Lexer.SubAnalyzers[i] do
       if Enabled and (SyntAnalyzer <> nil) and
          (CheckedLexers.IndexOf(SyntAnalyzer) = -1) then
         begin
           Result := CheckLexer(SyntAnalyzer);
           if not Result then Exit;
         end;
  end;

var is_mono: Boolean;
    metrics: TTextMetric;
    tmp: integer;
    i: integer;
begin
  if not HandleAllocated then Exit;
  if FMonoFontMode = mfFixed then
    Font.Pitch := fpFixed
  else
    Font.Pitch := fpDefault;

  is_mono := (FMonoFontMode <> mfVariable) and {(Charset = DEFAULT_CHARSET) and }(FZoom = 100);
  if is_mono then
   begin
    Canvas.Font := Font;
    Canvas.Font.Charset := Charset;
    GetTextMetrics(Canvas.Handle, metrics);
    is_mono := (metrics.tmPitchAndFamily and TMPF_FIXED_PITCH) = 0;
    if is_mono and (FMonoFontMode = mfAuto) then
     begin
//               (Font.Pitch in [fpFixed, fpDefault]) and
       is_mono :=
             CheckStyle(SyncEditing.InactiveWordsStyle) and
             CheckStyle(SyncEditing.ActiveWordsStyle) and
             CheckStyle(SyncEditing.SyncRangeStyle) and
             CheckStyle(DefaultStyles.SearchMark) and
             CheckStyle(DefaultStyles.SelectioMark) and
             CheckStyle(DefaultStyles.CurrentLine) and
             CheckStyle(DefaultStyles.CollapseMark);

       if is_mono and Assigned(FUserStyles) then
         for I := 0 to FUserStyles.Styles.Count - 1 do
           is_mono := is_mono and CheckStyle(FUserStyles.Styles[I]);

        if is_mono and (SyntObj <> nil) then
         begin
          CheckedLexers := TList.Create;
          try
            is_mono := CheckLexer(SyntObj.Owner);
          finally
            CheckedLexers.Free;
          end;
         end;
     end;
   end;
  if FMonoFont <> is_mono then
   begin
     if is_mono then
       begin
         if Canvas.Font.PixelsPerInch <> PixelPerInch.X then
           begin
             tmp := Canvas.Font.Size;
             Canvas.Font.PixelsPerInch := PixelPerInch.X;
             Canvas.Font.Size := tmp;
           end;
         FDefExt := Canvas.TextExtent('W');
       end;
     FMonoFont := is_mono;
     ResetLineHeights;
   end;
end;

procedure TCustomSyntaxMemo.CMFontChanged(var Message: TMessage);
begin
  FMonoFont := False;
  UpdateMonoFontFlag;
  ResetLineHeights;
  Invalidate;
end;

procedure TCustomSyntaxMemo.SetMonoFontMode(const Value: TMonoFontMode);
begin
  FMonoFontMode := Value;
  UpdateMonoFontFlag;
end;

function TCustomSyntaxMemo.Get_Text: ecString;
begin
  Result := Lines.Text;
end;

procedure TCustomSyntaxMemo.Set_Text(const Value: ecString);
begin
  Lines.Text := Value;
end;

procedure TCustomSyntaxMemo.SelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TCustomSyntaxMemo.SetBackGround(const Value: TecBackGround);
begin
  FBackGround.Assign(Value);
  Invalidate;
end;

procedure TCustomSyntaxMemo.BgChanged(Sender: TObject);
begin
  if FBackGround.FillStyle <> fstNone then
    DoubleBuffered := True;
  Invalidate;
end;

procedure TCustomSyntaxMemo.IntSetCaretPos(const cp: TPoint);
begin
  if (FCaretPos.X <> cp.X) or (FCaretPos.Y <> cp.Y) then
    CaretPos := SkipHidden(cp.X, cp.Y, False);
end;

function TCustomSyntaxMemo.GetLineNumberStr(Line: integer): string;
begin
  Result := FLineNumbers.GetNumberString(Line);
  if Assigned(FOnGetLineNumberStr) then FOnGetLineNumberStr(Self, Line, Result);
end;

procedure TCustomSyntaxMemo.SetSelRect(const Value: TRect);
var t: integer;
begin
  BeginUpdate;
  try
    FBlock := Value;
    if FBlock.Right < FBlock.Left then
      begin
        t := FBlock.Right;
        FBlock.Right := FBlock.Left;
        FBlock.Left := t;
      end;
    if FBlock.Bottom < FBlock.Top then
      begin
        t := FBlock.Bottom;
        FBlock.Bottom := FBlock.Top;
        FBlock.Top := t;
      end;
    SelectMode := msColumn;
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.SetLineSpacingAfter(const Value: integer);
begin
  if (FLineSpacingAfter <> Value) {and (Value >= 0)} then
   begin
    FLineSpacingAfter := Value;
    ResetLineHeights;
   end;
end;

procedure TCustomSyntaxMemo.SetLineSpacingBefore(const Value: integer);
begin
  if (FLineSpacingBefore <> Value) {and (Value >= 0)} then
   begin
    FLineSpacingBefore := Value;
    ResetLineHeights;
   end;
end;

function StringListCompareStringsDesc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  {$IFDEF EC_VCL6_UP}
  if List.CaseSensitive then
    Result := AnsiCompareStr(List[Index2], List[Index1])
  else
  {$ENDIF}
    Result := AnsiCompareText(List[Index2], List[Index1]);
end;

procedure TCustomSyntaxMemo.SortLines(Ascending, CaseSensitive: Boolean; FromLine,
  ToLine, LogXPos: integer);
var List: TecStringList;
    i, spos, cnt: integer;
    S: ecString;
begin
  if ToLine = -1 then ToLine := Lines.Count - 1;
  if (Lines.Count < 2) or
     (FromLine < 0) or (FromLine >= Lines.Count) or
     (ToLine - FromLine < 2)
     then Exit;

  List := TecStringList.Create;
  try
    for i := FromLine to ToLine do
      begin
        if LogXPos = 0 then spos := 0
          else spos := LogToLinesPos(Point(LogXPos, i)).X;
        if spos < LineLength(i) then
         begin
          S := Lines[i];
          S := Copy(S, spos + 1, Length(S) - spos);
         end
        else
          S := '';
        List.AddObject(S, TObject(i));
      end;
    {$IFDEF EC_VCL6_UP}
    List.CaseSensitive := CaseSensitive;
    {$ENDIF}
    {$IFDEF EC_UNICODE_IN_ANSI}
    List.Ascending := Ascending;
    List.Sort;
    {$ELSE}
    if Ascending then
      List.Sort
    else
      List.CustomSort(StringListCompareStringsDesc);
    {$ENDIF}
    S := '';
    for i := 0 to List.Count - 1 do
     begin
      spos := integer(List.Objects[i]);
      S := S + Lines.SubString(Lines.LineIndex(spos), Lines.LineSpace(spos));
      if Lines.LineSpace(spos) = Lines.LineLength(spos) then
        S := S + sLineBreak;
     end;
    spos := Lines.LineIndex(FromLine) - 1;
    cnt := Lines.LineIndex(ToLine) - 1 + Lines.LineSpace(ToLine) - spos;
    BeginUpdate;
    try
      ResetSelection;
      FCaretPos := Point(0, FromLine);
      DeleteText(cnt);
      InsertText(S);
    finally
      EndUpdate;
    end;
  finally
    List.Free;
  end;
end;

procedure TCustomSyntaxMemo.SortSelection(Ascending, CaseSensitive: Boolean);
var FromLine, ToLine, XPos: integer;
    SaveSel: TRect;
begin
  GetSelectedLines(FromLine, ToLine);
  if SelectMode = msColumn then
    begin
       XPos := FBlock.Left;
       SaveSel := FBlock;
    end else
       XPos := 0;
  SortLines(Ascending, CaseSensitive, FromLine, ToLine, XPos);
  if SelectMode = msColumn then
    FBlock := SaveSel
  else
   begin
    XPos := Lines.LineIndex(FromLine) - 1;
    IntSetSelection(XPos, CaretPosToStrPos(Point(0, ToLine + 1)) - XPos);
   end;
end;

function TCustomSyntaxMemo.CanSortSelection: Boolean;
var FromLine, ToLine: integer;
begin
  Result := not ReadOnly and HaveSelection;
  if not Result then Exit;
  GetSelectedLines(FromLine, ToLine);
  Result := (ToLine  - FromLine > 1) and
            CanDeleteText(Lines.LineIndex(FromLine) - 1,
            Lines.LineIndex(ToLine) + Lines.LineSpace(ToLine) - Lines.LineIndex(FromLine));
end;

function TCustomSyntaxMemo.CanMoveSelLines(MoveUp: Boolean): Boolean;
var FromLine, ToLine, Ln: integer;
begin
  GetSelectedLines(FromLine, ToLine);
  if MoveUp then
    Ln := FromLine - 1
  else
    Ln := ToLine + 1;

  Result := (Ln >= 0) and (Ln < Lines.Count) and
            CanDeleteText(Lines.LineIndex(Ln) - 1, Lines.LineSpace(Ln));
end;

procedure TCustomSyntaxMemo.MoveSelLines(MoveUp: Boolean);
var FromLine, ToLine, Ln, tp: integer;
    S: ecString;
    KeepSel: Boolean;
begin
  if not CanMoveSelLines(MoveUp) then Exit;

  GetSelectedLines(FromLine, ToLine);
  if MoveUp then
    begin
      Ln := FromLine - 1;
      tp := CaretPosToStrPos(Point(0, ToLine + 1)) - Lines.LineSpace(Ln);
    end
  else
    begin
      Ln := ToLine + 1;
      tp := CaretPosToStrPos(Point(0, FromLine));
    end;

  with Lines do
    S := Lines.SubString(LineIndex(Ln), LineSpace(Ln));
  if Lines.IsNullEnd then
    if MoveUp and (ToLine = Lines.Count - 1) then
      S := sLineBreak + S else
    if Ln = Lines.Count - 1 then
      S := S + sLineBreak;

  BeginUpdate;
  try
    KeepSel := not (soPersistentBlocks in FOptions);
    if KeepSel then
      Include(FOptions, soPersistentBlocks);
    // Delete text
    ReplaceText(Lines.LineIndex(Ln) - 1, Lines.LineSpace(Ln), '');
    // Inserting to new location
    ReplaceText(tp, 0, S);
    if MoveUp then tp := -1 else tp := 1;
    CaretPos := SkipHidden(FCaretPos.X, FCaretPos.Y + tp, not MoveUp);
    if KeepSel then
      Exclude(FOptions, soPersistentBlocks);
    if SelectMode <> msColumn then
      with StrPosToCaretPos(FSelStart) do
        if (X = 0) and (Y = FromLine) then
          begin
            tp := CaretPosToStrPos(Point(0, FromLine + 1));
            IntSetSelection(tp, FSelLength - (tp - FSelStart));
          end;
  finally
    EndUpdate;
  end;
end;

// ===========================================================================
//  OLE Drag & Drop support
// ===========================================================================
function TCustomSyntaxMemo.GetParentOLETarget: IDropTarget;
var prn: TWinControl;
begin
  prn := Parent;
  Result := nil;
  while prn <> nil do
    begin
      if Supports(prn, IDropTarget, Result) then Exit;
      prn := prn.Parent;
    end;
end;

function TCustomSyntaxMemo.DragEnter({$IFNDEF EC_DOTNET}const{$ENDIF} DataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var Handled: Boolean;
begin
  Result := S_OK;
  FDataObj := DataObj;
  if Assigned(FOnOleDragEnter) then
    begin
      Handled := False;
      FOnOleDragEnter(Self, DataObj, grfKeyState, pt, dwEffect, Handled);
      if Handled then
        Exit;
    end;
  if GetFormatInfo(DataObj, CF_TEXT) or GetFormatInfo(DataObj, CF_UNICODETEXT) then
  begin
    if (grfKeyState and MK_CONTROL) <> 0 then
      dwEffect := DROPEFFECT_COPY
    else
      dwEffect := DROPEFFECT_MOVE;
    if not Focused then
      Windows.SetFocus(Handle);
  end
  else
  begin
    FDataObj := nil;
    FPrnTarget := GetParentOLETarget;
    if FPrnTarget <> nil then
      begin
        Result := FPrnTarget.DragEnter(DataObj, grfKeyState, pt, dwEffect);
        if Result <> S_OK then
          FPrnTarget := nil;
      end else
      begin
        dwEffect := DROPEFFECT_NONE;
        Result := E_INVALIDARG; // All next processing will be aborted
      end;
  end;
end;

function TCustomSyntaxMemo.DragLeave: HResult;
begin
  if FPrnTarget <> nil then
    begin
      Result := FPrnTarget.DragLeave;
      FPrnTarget := nil;
    end else
    begin
      Result := S_OK;
      FDataObj := nil;
    end;
end;

function TCustomSyntaxMemo.IDropTarget_DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
var p: TPoint;
    Handled: Boolean;
begin
  if FPrnTarget <> nil then
    begin
      Result := FPrnTarget.DragOver(grfKeyState, pt, dwEffect);
      Exit;
    end;

  Result := S_OK;
  if Assigned(FOnOleDragOver) then
    begin
      Handled := False;
      FOnOleDragOver(Self, FDataObj, grfKeyState, pt, dwEffect, Handled);
      if Handled then
        Exit;
    end;

  p := ScreenToClient(pt);
  CaretPos := MouseToCaret(p.X, p.Y);
  if not CanInsertText(CaretStrPos){ or FDragText and PosInSelection(CaretPos)} then
    dwEffect := DROPEFFECT_NONE
  else if (grfKeyState and MK_CONTROL) <> 0 then
    dwEffect := DROPEFFECT_COPY
  else
    dwEffect := DROPEFFECT_MOVE;
end;

function TCustomSyntaxMemo.Drop({$IFNDEF EC_DOTNET}const{$ENDIF} DataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var FFormat: TFormatEtc;
    AText: ecString;
    Medium : TStgMedium;
    cp: TPoint;
    BT: Byte;
    St: TecStringList;
    Handled: Boolean;
begin
  if FPrnTarget <> nil then
    begin
      Result := FPrnTarget.Drop(DataObj, grfKeyState, pt, dwEffect);
      Exit;
    end;

  Result := S_OK;
  FDataObj := nil;

  if Assigned(FOnOleDrop) then
    begin
      Handled := False;
      FOnOleDrop(Self, DataObj, grfKeyState, pt, dwEffect, Handled);
      if Handled then
        Exit;
    end;

  pt := ScreenToClient(pt);
  cp := MouseToCaret(pt.X, pt.Y);

  if (grfKeyState and MK_CONTROL) <> 0 then
    dwEffect := DROPEFFECT_COPY
  else
    dwEffect := DROPEFFECT_MOVE;

  if FDragText then // Self dragging
    begin
      if PosInSelection(cp) then ResetSelection
       else MoveText(cp, (grfKeyState and MK_CONTROL) <> 0);
      FDragText := False;
      Exit;
    end;

  BT := TEXT_BLOCK_LINEAR;
  if GetFormatInfo(DataObj, CF_BLOCKTYPE, FFormat) and
     Succeeded(DataObj.GetData(FFormat, Medium)) and
     (Medium.hGlobal <> 0) then
   begin
     BT := BlockTypeFromHandle(Medium.hGlobal);
     ReleaseStgMedium(Medium);
   end;

  CaretPos := cp;
  if CanInsertText(CaretStrPos) then
    begin
      AText := GetDragText(DataObj, CharSet);
      if AText <> '' then
        begin
          ResetSelection;
          if BT = TEXT_BLOCK_RECTANGLE then
           begin
            St := TecStringList.Create;
            St.Text := AText;
            try
              InsertTextBlock(St, CaretPos);
            finally
              St.Free;
            end;
           end else
            InsertText(AText);
        end;
   end;
end;

function TCustomSyntaxMemo.BeginOLEDrag: Boolean;
var DataObject: IDataObject;
    DataObj: TecDataObject;
    dwEffect: integer;
    S: AnsiString;
    W: WideString;
    Data: ecString;
    BT: {$IFDEF EC_DOTNET}TBytes{$ELSE}Byte{$ENDIF};
    Exporter: TPlainTextSyntExport;
    CopyOnly: Boolean;
begin
  if not (soDragText in FOptions) or not FValidOle then
    begin
      Result := False;
      Exit;
    end;

  DataObj := TecDataObject.Create;
  {$IFDEF EC_UNICODE}
  W := SelText;
  S := UnicodeToAnsi(W, CharSet);
  {$ELSE}
  S := SelText;
  W := AnsiToUnicode(S, CharSet);
  {$ENDIF}
  CopyOnly := False;
  if Assigned(FOnOleBeginDrag) then
    begin
      FOnOleBeginDrag(Self, W, CopyOnly);
      S := UnicodeToAnsi(W, CharSet);
    end;

  {$IFDEF EC_DOTNET}
  DataObj.SetFormatData(CF_TEXT, BytesOf(S + #0), Length(S) + 1);
  DataObj.SetFormatData(CF_UNICODETEXT, BytesOf(W + #0), Length(W)*2 + 2);
  SetLength(BT, 1);
  if SelectMode = msColumn then
    BT[0] := TEXT_BLOCK_RECTANGLE
  else
    BT[0] := TEXT_BLOCK_LINEAR;
  DataObj.SetFormatData(CF_BLOCKTYPE, BT, 1);
  {$ELSE}
  DataObj.SetFormatData(CF_TEXT, PAnsiChar(S)^, Length(S) + 1);
  DataObj.SetFormatData(CF_UNICODETEXT, PWideChar(W)^, Length(W)*2 + 2);
  if SelectMode = msColumn then
    BT := TEXT_BLOCK_RECTANGLE
  else
    BT := TEXT_BLOCK_LINEAR;
  DataObj.SetFormatData(CF_BLOCKTYPE, BT, 1);
  {$ENDIF}
  if soCopyAsRTF in FOptions then
   begin
     Exporter := TRTFSyntExport.Create(nil);
     try
       Exporter.SyntMemo := Self;
       Exporter.ExportSelection;
       Data := Exporter.ExportData;
  {$IFDEF EC_DOTNET}
       DataObj.SetFormatData(ecStrUtils.CF_RTF, BytesOf(S + #0), Length(S) + 1);
  {$ELSE}
       DataObj.SetFormatData(ecStrUtils.CF_RTF, PecChar(Data)^, Length(Data) + 1);
  {$ENDIF}
     finally
       Exporter.Free;
     end;
   end;

  {$IFDEF EC_DOTNET}
  DataObject := DataObj as IDataObject;
  {$ELSE}
  DataObj.GetInterface(IDataObject, DataObject);
  {$ENDIF}
  FDragText := True;
  Result := DoDragDrop(DataObject, Self,
      DROPEFFECT_COPY or DROPEFFECT_MOVE, dweffect) = DRAGDROP_S_DROP;
  if Result and
     not CopyOnly and
     FDragText and  // FDragText reseted in Drop, so text was moved using MoveText
     (dwEffect = DROPEFFECT_MOVE) then
    ClearSelection; // Clear selection if user moves text to another control
  FDragText := False;
end;

procedure TCustomSyntaxMemo.DragDrop(Source: TObject; X, Y: Integer);
var cp: TPoint;
begin
  inherited;
  if (Source is TCustomSyntaxMemo) and
      TCustomSyntaxMemo(Source).HaveSelection then
    begin
      if Source = Self then
        begin
          cp := MouseToCaret(X, Y);
          if PosInSelection(cp) then ResetSelection
           else MoveText(cp, (GetKeyState(VK_CONTROL) and not $7FFF) <> 0);
        end else
        begin
          TCustomSyntaxMemo(Source).CopyToClipboard;
          PasteFromClipboard;
        end;
    end;
end;

procedure TCustomSyntaxMemo.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TCustomSyntaxMemo) and
            TCustomSyntaxMemo(Source).HaveSelection;

  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);

  if Accept then
    begin
      if Source <> Self then
        ResetSelection;
      CaretPos := MouseToCaret(X, Y);
      if not Focused then
        SetFocus;
    end;
end;

procedure TCustomSyntaxMemo.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  FDragText := False;
end;

procedure TCustomSyntaxMemo.DoScroll;
var i: integer;
    iObj: IecSyntMemoPlugIn2;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
  for i := 0 to FSyntClients.Count - 1 do
   if Supports(TObject(FSyntClients[i]), IecSyntMemoPlugIn2, iObj) then
    iObj.ScrollPosChanged;
  if Assigned(FTextSource) and Supports(FTextSource, IecSyntMemoPlugIn2, iObj) then
    iObj.ScrollPosChanged;

  //AT
  DrawScroll;
  Invalidate;
end;

function TCustomSyntaxMemo.GiveFeedback(dwEffect: Integer): HResult;
var Handled: Boolean;
begin
  if Assigned(FOnOleGiveFeedback) then
    begin
      Handled := False;
      FOnOleGiveFeedback(Self, dwEffect, Handled);
      if Handled then
        begin
          Result := S_OK;
          Exit;
        end;
    end;
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

function TCustomSyntaxMemo.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Integer): HResult;
begin
  Result := S_OK;
  If fEscapePressed then Result := DRAGDROP_S_CANCEL else
    if ((grfKeyState and MK_LBUTTON) = 0) then
      Result := DRAGDROP_S_DROP;
end;

procedure TCustomSyntaxMemo.SetMultiLine(const Value: Boolean);
begin
  if FMultiLine = Value then Exit;
  FMultiLine := Value;
  BeginUpdate;
  try
    TopLine := 0;
    CaretPos := Point(CaretPos.X, 0);
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.SetCharset(const Value: TFontCharset);
begin
  if Charset <> Value then
    begin
      Lines.Charset := Value;
      Font.Charset := Value;
      ResetLineHeights;
      UpdateMonoFontFlag;
      Invalidate;
    end;
end;

function TCustomSyntaxMemo.GetCharset: TFontCharset;
begin
  Result := Lines.Charset;
end;

function TCustomSyntaxMemo.GetReadOnly: Boolean;
begin
  if FTextSource <> nil then
    Result := FTextSource.ReadOnly
  else
    Result := FReadOnly;
end;

procedure TCustomSyntaxMemo.DragCanceled;
begin
  inherited;
  ResetSelection;
end;

function TCustomSyntaxMemo.IsWordChar(C: ecChar): Boolean;
begin
  Result := ecStrUtils.IsWordChar(C);
  if Assigned(FOnCheckChar) then
    FOnCheckChar(Self, Ord(C), Result);
end;

// =============================================================================
// Embedded objects processing
// =============================================================================

procedure TCustomSyntaxMemo.SetKeyQueue(const Value: TKeyQueue);
begin
  FKeyQueue.Assign(Value);
end;

procedure TCustomSyntaxMemo.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('InTextImages', ReaderSkip, nil, False);
  Filer.DefineProperty('InTexTCustomImageList', ReaderSkip, nil, False);
end;

{$IFDEF EC_VCL5}
type
  TCompReader = class(TReader)
    procedure SkipValue;
  end;

procedure TCompReader.SkipValue;
  procedure SkipBytes(Count: Integer);
  var
    Bytes: array[0..255] of Char;
  begin
    while Count > 0 do
      if Count > SizeOf(Bytes) then
      begin
        Read(Bytes, SizeOf(Bytes));
        Dec(Count, SizeOf(Bytes));
      end
      else
      begin
        Read(Bytes, Count);
        Count := 0;
      end;
  end;

  procedure SkipList;
  begin
    while not EndOfList do SkipValue;
    ReadListEnd;
  end;

  procedure SkipBinary(BytesPerUnit: Integer);
  var
    Count: Longint;
  begin
    Read(Count, SizeOf(Count));
    SkipBytes(Count * BytesPerUnit);
  end;

  procedure SkipCollection;
  begin
    while not EndOfList do
    begin
      if NextValue in [vaInt8, vaInt16, vaInt32] then SkipValue;
      SkipBytes(1);
      while not EndOfList do SkipProperty;
      ReadListEnd;
    end;
    ReadListEnd;
  end;
begin
  case ReadValue of
    vaNull: { no value field, just an identifier };
    vaList: SkipList;
    vaInt8: SkipBytes(SizeOf(Byte));
    vaInt16: SkipBytes(SizeOf(Word));
    vaInt32: SkipBytes(SizeOf(LongInt));
    vaExtended: SkipBytes(SizeOf(Extended));
    vaString, vaIdent: ReadStr;
    vaFalse, vaTrue: { no value field, just an identifier };
    vaBinary: SkipBinary(1);
    vaSet: SkipSetBody;
    vaLString: SkipBinary(1);
    vaCollection: SkipCollection;
    vaSingle: SkipBytes(Sizeof(Single));
    vaCurrency: SkipBytes(SizeOf(Currency));
    vaDate: SkipBytes(Sizeof(TDateTime));
    vaWString: SkipBinary(Sizeof(WideChar));
    vaInt64: SkipBytes(Sizeof(Int64));
  end;
end;
{$ENDIF}

procedure TCustomSyntaxMemo.ReaderSkip(Reader: TReader);
begin
  {$IFDEF EC_VCL5}TCompReader{$ENDIF}(Reader).SkipValue;
end;

procedure TCustomSyntaxMemo.SetOptionsEx(const Value: TSyntaxMemoOptionsEx);
begin
  if FOptionsEx <> Value then
    begin
      FOptionsEx := Value;
      ResetLineHeights;
      Invalidate;
    end;
end;

function TCustomSyntaxMemo.GetClientCount: integer;
begin
  Result := FSyntClients.Count;
end;

function TCustomSyntaxMemo.GetClients(Index: integer): TObject;
begin
  Result := TObject(FSyntClients[Index]);
end;

procedure TCustomSyntaxMemo.AlignTokens(FromLine, ToLine: integer);
var i, idx, ls, le, k: integer;
    W: TList;
    St: TecStringList;
    LineStr, S: ecString;
    Indent: integer;

  procedure AddWidth(N: integer);
  begin
    if ls + N > le then N := le - ls;
    if k = W.Count then
      W.Add(TObject(N))
    else
      if integer(W[k]) < N then
        W[k] := TObject(N);
    Inc(k);
    Inc(ls, N);
  end;

begin
  if not Assigned(SyntObj) or ReadOnly then Exit;
  if FromLine < 0 then FromLine := 0;
  if ToLine > Lines.Count - 1 then ToLine := Lines.Count - 1;
  if ToLine = FromLine then Exit;
  Indent := -1;
  W := TList.Create;
  try
    for i := FromLine to ToLine do
      begin
        ls := Lines.LineIndex(i) - 1;
        le := ls + Lines.LineLength(i);
        idx := SyntObj.NextTokenAt(ls);
        k := 0;
        if idx <> -1 then
          begin
            // Multiline tokens are not handled
            if SyntObj.Tags[idx].StartPos < ls then Exit;
            if Indent = -1 then Indent := SyntObj.Tags[idx].StartPos - ls;
            // Column width determination
            while (idx < SyntObj.TagCount) and (SyntObj.Tags[idx].StartPos < le) do
              begin
                AddWidth(SyntObj.Tags[idx].EndPos - SyntObj.Tags[idx].StartPos);
                Inc(idx);
              end;
          end;
      end;
    if Indent = -1 then Exit; // There no lines to align
    St := TecStringList.Create;
    try
      for i := FromLine to ToLine do
        begin
          ls := Lines.LineIndex(i) - 1;
          le := ls + Lines.LineLength(i);
          idx := SyntObj.NextTokenAt(ls);
          k := 0;
          LineStr := GetIndentString(Indent, False);
          if idx <> -1 then
            while (idx < SyntObj.TagCount) and (SyntObj.Tags[idx].StartPos < le) do
              begin
                S := SyntObj.TagStr[idx];
                LineStr := LineStr + S;
                if K < W.Count - 1 then
                  LineStr := LineStr + StringOfChar(' ', integer(W[k]) - Length(S) + 1)
                else
                  Break;
                Inc(K);
                Inc(idx);
              end;
          St.Add(LineStr);
        end;
      ls := Lines.LineIndex(FromLine) - 1;
      le := Lines.LineIndex(ToLine) - 1 + Lines.LineSpace(ToLine);
      ReplaceText(ls, le - ls, St.Text);
    finally
      St.Free;
    end;
  finally
    W.Free;
  end;
end;

procedure TCustomSyntaxMemo.AlignTokensSel;
var sl, el: integer;
begin
  if ReadOnly then Exit;
  if GetSelectedLines(sl, el) > 1 then
    AlignTokens(sl, el);
end;

function TCustomSyntaxMemo.ScrollText(XScrollPos, YScrollPos, YSubLine: integer): Boolean;
var XAmount, Y1, Y2, i, Ln, MaxLine: integer;
    R: TRect;
begin
    if YScrollPos < 0 then YScrollPos := 0;
    Result := False;
    if XScrollPos <> ScrollPosX then begin
        Result := True;
        XAmount := (ScrollPosX - XScrollPos) * DefTextExt.cx;
        FLeftTopPos.X := XScrollPos;
        R := TextArea;
        R.Top := 0;
        if soDrawCurLineFocus in Options then
            InflateRect(R, -1, 0);
        if HandleAllocated then
            if (FBackGround.FillStyle = fstNone) and
                ((FHorzRuler.Height = 0) or (FHorzRuler.BackGround.FillStyle = fstNone))
                and not FTransparent then
                ScrollWindow(Handle, XAmount, 0, {$IFNDEF EC_DOTNET}@{$ENDIF}R, {$IFNDEF EC_DOTNET}@{$ENDIF}R)
            else
                Invalidate;
    end;
    if (YScrollPos <> FLeftTopPos.Y) or (YSubLine <> FTopSubLine) then begin
        if not (soScrollLastLine in FOptions) then begin
            Ln := LineToLog(YScrollPos, YSubLine);
            MaxLine := FLineCount - VisibleLines + 1;
            if Ln > MaxLine then
                LogToLine(MaxLine, YScrollPos, YSubLine);
        end;
        if YScrollPos >= Lines.Count then begin
            YScrollPos := Lines.Count - 1;
            if YScrollPos < 0 then begin
                YScrollPos := 0;
                YSubLine := 0;
            end else
                YSubLine := GetLineInfo(YScrollPos).LineCount - 1;
        end;

        if abs(YScrollPos - FLeftTopPos.Y) > 20 then begin
            FLeftTopPos.Y := YScrollPos;
            FTopSubLine := YSubLine;
            Invalidate;
        end else begin
            LineBound(YScrollPos, Y1, Y2, True);
            with GetLineInfo(YScrollPos) do
                for i := 0 to Min(YSubLine, LineCount) - 1 do
                    Inc(Y1, Heights[i]);
            FLeftTopPos.Y := YScrollPos;
            FTopSubLine := YSubLine;
            R := ClientRect;
            R.Top := R.Top + FMargin.Top;
            R.Bottom := R.Bottom - FMargin.Bottom;
            if HandleAllocated then
                if FTransparent or FDoScroll then Invalidate else begin
                    ScrollWindow(Handle, 0, -(Y1 - FMargin.Top), {$IFNDEF EC_DOTNET}@{$ENDIF}R, {$IFNDEF EC_DOTNET}@{$ENDIF}R);
                    InvalidateGutter;
                end;
        end;
        Result := True;
    end;
    if Result then begin
        AdjustScrollBar;
        UpdateCaretPos;
        DoScroll;
    end;
end;

procedure TCustomSyntaxMemo.SetZoom(const Value: integer);
begin
  if (Value > 10) and (Value < 1000) and (Value <> FZoom) then
    begin
      FZoom := Value;
      UpdateMonoFontFlag;
      ResetLineHeights(True);
      UpdateEditor;
      if Assigned(SyntObj) then
        SyntObj.ResetStaples;
      if Assigned(FOnZoom) then
        FOnZoom(Self);
    end;
end;

procedure TCustomSyntaxMemo.SetSelAttributes(
  const Value: TecTextAttributes);
begin
// no assignment is supported
end;

type
  TFormatRange = class(TRange)
    RefStyle: TSyntaxFormat;
  end;

function TCustomSyntaxMemo.DoAutoFormat: Boolean;
var CL: TSortedList;
    i: integer;

  function IsFormattingStyle(Stl: TSyntaxFormat): Boolean;
  begin
    Result := Assigned(Stl) and (Stl.ChangeCase <> ccNone);
  end;

  procedure AddRange(StartPos, EndPos: integer; Stl: TSyntaxFormat);
  var R: TFormatRange;
  begin
    R := TFormatRange.Create(StartPos, EndPos);
    R.RefStyle := Stl;
    CL.Add(R);
  end;

begin
  Result := False;
  if (soAutoFormat in FOptionsEx) and Assigned(SyntObj) then
    with SyntObj do
      begin
        CL := TSortedList.Create(True);
        try
          // Formatted tags
          for i := 0 to TagCount - 1 do
            if IsFormattingStyle(Tags[i].Style) then
              AddRange(Tags[i].StartPos, Tags[i].EndPos, Tags[i].Style);

          // formatted ranges
          for i := 0 to RangeCount - 1 do
            with Ranges[i] do
              if Assigned(Rule) and (EndIdx <> -1) and IsFormattingStyle(Rule.Style) then
                AddRange(Tags[StartIdx].StartPos, Tags[EndIdx].EndPos, Rule.Style);

          // formatted sub-lexers
          for i := 0 to SubLexerRangeCount - 1 do
            with SubLexerRanges[i] do
              if Assigned(Rule) and IsFormattingStyle(Rule.Style) then
                AddRange(StartPos, EndPos, Rule.Style);

          // perform formatting
          for i := CL.Count - 1 downto 0 do
            with TFormatRange(CL[i]) do
              Result := Result or ChangeCase(StartPos, EndPos - StartPos, RefStyle.ChangeCase);
        finally
          CL.Free;
        end;
      end;
end;

function TCustomSyntaxMemo.GetCurrentLine: integer;
begin
  Result := FCaretPos.Y;
end;

procedure TCustomSyntaxMemo.SetCurrentLine(const Value: integer);
begin
  if (Value >= 0) and (Value < Lines.Count) then
    CaretPos := Point(FCaretPos.X, Value);
end;

function TCustomSyntaxMemo.DefLineHeight: integer;
begin
  Result := DefTextExt.cy + FLineSpacing + FLineSpacingAfter + FLineSpacingAfter;
end;

procedure TCustomSyntaxMemo.DuplicateLine(Line: integer);
begin
  if (Line >= 0) and (Line < Lines.Count) then
    ReplaceText(Lines.LineIndex(Line) - 1, 0,
      Lines.SubString(Lines.LineIndex(Line), Lines.LineSpace(Line)));
end;

function TCustomSyntaxMemo.DoBeforeDeleteText(var Pos,
  Count: integer): Boolean;
begin
  Result := CanDeleteText(Pos, Count);
  if Assigned(FOnBeforeDelete) then
    FOnBeforeDelete(Self, Pos, Count, Result);
end;

function TCustomSyntaxMemo.DoBeforeInsertText(var Pos: integer;
  var S: ecString): Boolean;
begin
  Result := CanInsertText(Pos);
  if Assigned(FOnBeforeInsert) then
    FOnBeforeInsert(Self, Pos, S, Result);
end;

function TCustomSyntaxMemo.CaretInText(const Pos: TPoint): Boolean;
begin
  Result := (Pos.Y >= 0) and (Pos.Y < Lines.Count) and
            (Pos.X >= 0) and (Pos.X <= Lines.LineLength(Pos.Y));
end;

procedure TCustomSyntaxMemo.AlignLines(const Tokens: TStringList;
  ByFirst: Boolean; FromLine, ToLine: integer);
var ColWidth: integer;
    i, k, dx: integer;

  function NextPos(const Line: ecString): integer;
  var i, k: integer;
  begin
    Result := 0;
    for i := 0 to Tokens.Count - 1 do
      begin
        k := ecPosEx(Tokens[i], Line);
        if k > Result then
          Result := k;
      end;
  end;

  procedure UpdateColumnsFromLine(const Line: ecString);
  var k: integer;
  begin
    k := NextPos(Line);
    if k > 0 then
      k := LinesPosToLogX(Line, k);
    if ColWidth < k then
      ColWidth := k;
  end;

begin
  ColWidth := 0;
  if ByFirst then
    UpdateColumnsFromLine(Lines[FromLine])
  else
    for i := FromLine to ToLine do
      UpdateColumnsFromLine(Lines[i]);
  if ColWidth > 0 then
    begin
      BeginUpdate;
      try
        for i := FromLine to ToLine do
          begin
            k := NextPos(Lines[i]);
            if k > 0 then
              begin
                dx := ColWidth - LinesPosToLogX(Lines[i], k);
                if dx > 0 then
                  ReplaceText(CaretPosToStrPos(Point(k - 1, i)), 0, StringOfChar(' ', dx));
              end;
          end;
      finally
        EndUpdate;
      end;
    end;
end;

procedure TCustomSyntaxMemo.AlignSelectedLines(const Tokens: string;
  ByFirst: Boolean);
var sl, el: integer;
    St: TStringList;
begin
  if GetSelectedLines(sl, el) > 1 then
    begin
      St := TStringList.Create;
      try
        St.Text := Tokens;
        AlignLines(St, ByFirst, sl, el);
      finally
        St.Free;
      end;
    end;
end;

procedure TCustomSyntaxMemo.SetHorzRuler(const Value: TecHorzRuler);
begin
  FHorzRuler.Assign(Value);
end;

procedure TCustomSyntaxMemo.SetTextMargins(const Value: TecTextMargins);
begin
  FTextMargins.Assign(Value);
end;

// =============================================================================
//  Old style Right Margin support (properties: RightMargin, RightMarginColor, ShowRightMargin)
// =============================================================================
function TCustomSyntaxMemo.GetRightMargin: integer;
begin
  if FTextMargins.Count > 0 then
    Result := FTextMargins[0].Position
  else
    Result := 0;
end;

function TCustomSyntaxMemo.GetRightMarginColor: TColor;
begin
  if FTextMargins.Count > 0 then
    Result := FTextMargins[0].Pen.Color
  else
    Result := clBlack;
end;

function TCustomSyntaxMemo.GetShowRightMargin: Boolean;
begin
  if FTextMargins.Count > 0 then
    Result := FTextMargins[0].Visible
  else
    Result := False;
end;

procedure TCustomSyntaxMemo.SetShowRightMargin(const Value: Boolean);
begin
  if FTextMargins.Count > 0 then
    FTextMargins[0].Visible := Value;
end;

procedure TCustomSyntaxMemo.SetRightMargin(const Value: integer);
begin
  if FTextMargins.Count > 0 then
    FTextMargins[0].Position := Value;
end;

procedure TCustomSyntaxMemo.SetRightMarginColor(const Value: TColor);
begin
  if FTextMargins.Count > 0 then
    FTextMargins[0].Pen.Color := Value;
end;

procedure TCustomSyntaxMemo.InsertNewLine(Indent: integer; DoNotMoveCaret, SoftBreak: Boolean);
var S, S1: ecString;
    X, i, sp, ind, j, max_ind, tg: integer;
    cp: TPoint;
    R: TTextRange;
    first_en: Boolean;
    lBreak: string;
begin
  if not FMultiLine then Exit;

  x := CaretStrPos;
  S := '';

  case Lines.TextFormat of
  tfCR: lBreak := #$D;
  tfNL: lBreak := #$A;
  else if SoftBreak then
         lBreak := #$A
       else
         lBreak := #$D#$A;
  end;

  // Auto-closing opened ranges
  first_en := True;
  max_ind := 0;
  if not HaveSelection and (SyntObj <> nil) and not (soDisableAutoClose in FOptionsEx) then
    with SyntObj do
      begin
        tg := TokenAtPos(x);
        i := OpenCount - 1;
        if (tg = -1) or (Tags[tg].EndPos = x) or (Tags[tg].StartPos = x) then
        while i >= 0 do
          begin
            R := Opened[i];
            if (R.Rule.AutoCloseMode <> acmDisabled) and
               (Tags[R.StartIdx].StartPos < x) and
               (R.Rule.AutoCloseText <> '') then
              begin
                if first_en and not IsFinished then
                  begin
                    first_en := False;
                    CompleteAnalysis;
                    if i >= OpenCount - 1 then
                      i := OpenCount - 1;
                    Continue;
                  end;

                if R.Rule.AutoCloseMode = acmCloseNearest then
                  for j := R.Index + 1 to RangeCount - 1 do
                    if Tags[Ranges[j].StartIdx].StartPos >= X then Break else
                      if (Ranges[j].Rule = R.Rule) or
                         (R.Rule.BlockEndCond <> nil) and (R.Rule.BlockEndCond = Ranges[j].Rule.BlockEndCond) then
                        R := Ranges[j];

                cp := StrPosToCaretPos(Tags[R.StartIdx].StartPos);
                if cp.Y < Lines.Count then
                  begin
                    ind := StringIndent(Lines[cp.Y]);
                    if ind = -1 then ind := 0;
                    if ind > max_ind then max_ind := ind;
                    S := S + lBreak + GetAutoCloseText(R, GetIndentString(ind, False)); //AT
                  end;
              end;
            Dec(i);
          end;
      end;

  if S <> '' then
    begin
      Inc(max_ind, FBlockIndent);
      S1 := GetInsAddSpaces(FCaretPos) + lBreak;
      if max_ind > 0 then
        S1 := S1 + GetIndentString(max_ind, False);
      sp := x + Length(S1);
      ReplaceText(x, 0, S1 + S);
      if not DoNotMoveCaret then
        CaretStrPos := sp;
    end else
    begin
      if DoNotMoveCaret then
        cp := CaretPos;
      if Indent > 0 then
        InsertText(lBreak + GetIndentString(Indent, False))
      else
        InsertText(lBreak);
      if DoNotMoveCaret then
        CaretPos := cp;
    end;
end;

procedure TCustomSyntaxMemo.SetCaret(const Value: TecCaret);
begin
  FCaret.Assign(Value);
end;

procedure TCustomSyntaxMemo.DoInsertChar(Char: ecChar; StrPos: Integer);
begin
  if Assigned(FOnInsertChar) then
    FOnInsertChar(Self, Char, StrPos);
end;

function TCustomSyntaxMemo.UnTabText(FromPos, ToPos: integer): integer;
var i, n: integer;
    p: TPoint;
begin
  if FromPos < 1 then FromPos := 1;
  if ToPos < FromPos then ToPos := TextLength;

  Result := 0;
  BeginUpdate;
  try
    for i := ToPos downto FromPos do
      if Lines.Chars[i] = #9 then
        begin
          Inc(Result);
          p := StrPosToCaretPos(i);
          n := LinesPosToLog(p).X;
          P.X := P.X - 1;
          n := n - LinesPosToLog(p).X;
          if n < 1 then n := 1;
          ReplaceText(i - 1, 1, StringOfChar(' ', n))
        end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomSyntaxMemo.WMContextMenu(var Message: TWMContextMenu);
begin
  if DefaultPopup and (PopupMenu = nil) then
   begin
     if FDefPopup <> nil then
       FDefPopup.Free;
     FDefPopup := CreateStdPopup;
     FDefPopup.Popup(Message.XPos, Message.YPos);
   end else
     inherited;
end;

procedure TCustomSyntaxMemo.WMCancelMode(var Message: TWMCancelMode);
begin
  ResetHint;
  FDragging := False;
  FDraggedTextMargin := nil;
  FScrollTimer.Enabled := False;
  FDragStaple := nil;
  CancelDrag;
  inherited;
end;

procedure TCustomSyntaxMemo.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
    begin
      FTransparent := Value;
{      ParentBackground := Value;}
      if Value then
        ControlStyle := ControlStyle - [csOpaque]
      else
        ControlStyle := ControlStyle + [csOpaque];
      RecreateWnd;
    end;
end;
{
procedure TCustomSyntaxMemo.WMNCPaint(var Message: TWMNCPaint);
begin
  if FTransparent then
    begin
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_TRANSPARENT);
      inherited;
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT);
    end else
      inherited;
end;}

procedure TCustomSyntaxMemo.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
    begin
      FAlignment := Value;
      Invalidate;
    end;
end;

function TCustomSyntaxMemo.DoMouseWheelUpDown(Up: boolean; Shift: TShiftState): Boolean;
const
  cMultiplyX = 10;
var
  Delta: Integer;
begin
  Delta := Mouse.WheelScrollLines;
  if Up then
    Delta := -Delta;
  if ssShift in Shift then
    ScrollPosX := ScrollPosX + Delta*cMultiplyX
  else
    ScrollPosY := ScrollPosY + Delta;
  ResetHint;
  Result := true;
end;

function TCustomSyntaxMemo.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := DoMouseWheelUpDown(True, Shift);
end;

function TCustomSyntaxMemo.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := DoMouseWheelUpDown(False, Shift);
end;

{ TSyncRecord }

constructor TSyncRecord.Create(AStart, AEnd: integer;
  const AText: ecString; IgnoreCase: Boolean);
begin
  if AStart < 0 then AStart := 0;
  if AEnd > Length(AText) then AEnd := Length(AText);
  inherited Create(AStart, AEnd);
  FList := TObjectList.Create;
  ExtractWrods(AText, IgnoreCase);
end;

procedure TSyncRecord.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

destructor TSyncRecord.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

// Extracting the same words in the selection and creating lists
procedure TSyncRecord.ExtractWrods(const Text: ecString; IgnoreCase: Boolean);
var N, ws, we, i: integer;
{$IFDEF EC_UNICODE}
    strList: TecStringList;
{$ELSE}
    strList: TzStringList;
{$ENDIF}
    S: ecString;
    R: TRangeList;
  procedure Scan(var Pos: integer; WordChar: Boolean);
  begin
   while (Pos < N) and (WordChar xor not IsWordChar(Text[Pos + 1])) do
    Inc(Pos);
  end;
begin
  N := Length(Text);
  ws := StartPos;
  if (ws > 0) and IsWordChar(Text[ws]) then
     Scan(ws, True);
  Scan(ws, False);

{$IFDEF EC_UNICODE}
  strList := TecStringList.Create;
{$ELSE}
  strList := TzStringList.Create;
{$ENDIF}
  strList.Sorted := True;
  strList.CaseSensitive := not IgnoreCase;
  try
   while ws < EndPos do
    begin
      we := ws;
      Scan(we, True);
      if we > EndPos then Break;
      S := Copy(Text, ws + 1, we - ws);
      i := strList.IndexOf(S);
      if i = -1 then
       begin
        R := TRangeList.Create(False);
        R.Add(TSyncWord.Create(ws, we + 1));
        strList.AddObject(S, R);
       end else
        TRangeList(strList.Objects[i]).Add(TSyncWord.Create(ws, we + 1));
      ws := we;
      Scan(ws, False);
    end;
  finally
    for i := 0 to strList.Count - 1 do
     if TRangeList(strList.Objects[i]).Count > 1 then
      FList.Add(strList.Objects[i])
     else
      strList.Objects[i].Free;
    strList.Free;
  end;
end;

function TSyncRecord.GetCount: integer;
begin
  Result := FList.Count;
end;

function TSyncRecord.GetItem(Index: integer): TRangeList;
begin
  Result := TRangeList(FList[Index]);
end;

{ TSyntSyncEdit }

constructor TSyntSyncEdit.Create(AOwner: TCustomSyntaxMemo);
var i: integer;
begin
  inherited Create;
  FOwner := AOwner;
  FSyncRangeStyle := TSyntaxFormat.Create(nil);
  FSyncRangeStyle.FormatType := ftBackGround;
  FSyncRangeStyle.BgColor := $00E0FFE0;
  FActiveWordsStyle := TSyntaxFormat.Create(nil);
  FActiveWordsStyle.FormatType := ftBackGround;
  for i := 0 to 3 do
   begin
     FActiveWordsStyle.BorderColors[i] := clBlue;
     FActiveWordsStyle.BorderTypes[i] := blSolid;
   end;
  FInactiveWordsStyle := TSyntaxFormat.Create(nil);
  FInactiveWordsStyle.FormatType := ftBackGround;
  FInactiveWordsStyle.BorderColorBottom := clBtnFace;
  FInactiveWordsStyle.BorderTypeBottom := blSolid;
  FActiveCoord.X := -1;
  FEnabled := True;
end;

destructor TSyntSyncEdit.Destroy;
begin
  Clear;
  FInactiveWordsStyle.Free;
  FActiveWordsStyle.Free;
  FSyncRangeStyle.Free;
  inherited;
end;

procedure TSyntSyncEdit.Clear;
begin
  if FSyncList <> nil then
   begin
     FSyncList.Free;
     FSyncList := nil;
   end;
  FActiveCoord.X := -1;
end;

function TSyntSyncEdit.GetStyleList(CurPos: integer; List: TStyleEntries): integer;
var idx, i, j: integer;
    R: TSyncRecord;
    L: TRange;
begin
  Result := -1;
  if FSyncList = nil then Exit;

  idx := FSyncList.NextAt(CurPos);
  if idx = -1 then Exit;
  R := TSyncRecord(FSyncList[idx]);
  if R.StartPos > CurPos then
   begin
    Result := R.StartPos;
    Exit;
   end;
  List.Add(FSyncRangeStyle, R.StartPos, R.EndPos);
  Result := R.EndPos;
  for i := 0 to R.Count - 1 do
   begin
    j := R[i].NextAt(CurPos);
    if j = -1 then Continue;
    if R[i][j].EndPos - 1 <= CurPos then
     begin
      Inc(j);
      if j >= R[i].Count then Continue;
     end;
    L := R[i][j];
    if L.StartPos > CurPos then
      begin
       if Result > L.StartPos then
         Result := L.StartPos;
      end else
      begin
       if (FActiveCoord.X = idx) and (FActiveCoord.Y = i) then
         List.Add(FActiveWordsStyle, L.StartPos, L.EndPos - 1)
       else
         List.Add(FInactiveWordsStyle, L.StartPos, L.EndPos - 1);
       if Result > L.EndPos - 1 then
         Result := L.EndPos - 1;
      end;
   end;
end;

procedure TSyntSyncEdit.TextChanged(Pos, Count: integer);
var i, j, k, idx, idx_wl: integer;
    R: TSyncRecord;
    W: TSyncWord;
begin
  if (FSyncList = nil) or (Count = 0) then Exit;
  if Pos = -1 then
   begin
    Clear;
    Exit;
   end;
  idx := FSyncList.NextAt(Pos);
  if idx = -1 then Exit;

  // In the syncchonized range
  if FSyncList[idx].StartPos <= Pos then
   begin
     R := TSyncRecord(FSyncList[idx]);
     // Delete
     if (Count < 0) and (Pos - Count > R.EndPos) then
      begin
       while (idx < FSyncList.Count) and
             (Pos - Count > FSyncList[idx].StartPos) do
         FSyncList.Delete(idx);
      end else
      begin
       Inc(R.FEndPos, Count);
       for i := R.Count - 1 downto 0 do
        begin
          idx_wl := R[i].NextAt(Pos);
          if idx_wl = -1 then Continue;
          // Possibly edited word
          W := TSyncWord(R[i][idx_wl]);
          if (Count < 0) and
             ((Pos < W.EndPos - 1) and (Pos - Count > W.EndPos - 1) or
              (idx_wl < R[i].Count - 1) and (R[i][idx_wl + 1].StartPos <= Pos - Count) or
              (Pos < W.StartPos) and (Pos - Count > W.StartPos)) then
            begin
             R.Delete(i);
            end else
            begin
              if W.StartPos <= Pos then
                begin
                  Inc(W.FEndPos, Count);
                  Inc(idx_wl);
                end;

              for j := idx_wl to R[i].Count - 1 do
               begin
                 W := TSyncWord(R[i][j]);
                 Inc(W.FStartPos, Count);
                 Inc(W.FEndPos, Count);
               end;
            end;
        end;
       if R.Count = 0 then FSyncList.Delete(idx)
        else Inc(idx);
      end;
   end;
  for i := idx to FSyncList.Count - 1 do
   begin
    R := TSyncRecord(FSyncList[idx]);
    Inc(R.FStartPos, Count);
    Inc(R.FEndPos, Count);
    for j := 0 to R.Count - 1 do
     for k := 0 to R[j].Count - 1 do
      begin
       W := TSyncWord(R[j][k]);
       Inc(W.FStartPos, Count);
       Inc(W.FEndPos, Count);
      end;
   end;
end;

function TSyntSyncEdit.GetSyncPositions(Pos, Count: integer): TList;
var i, j, idx, idx_wl, offs: integer;
    R: TSyncRecord;
    W: TSyncWord;
begin
  Result := nil;
  if (FSyncList = nil) or (Count = 0) then Exit;
  idx := FSyncList.NextAt(Pos);
  if (idx <> -1) and (FSyncList[idx].StartPos <= Pos) then
   begin
     R := TSyncRecord(FSyncList[idx]);
     for i := R.Count - 1 downto 0 do
      begin
        idx_wl := R[i].NextAt(Pos);
        if idx_wl = -1 then Continue;
        // Possibly edited word
        W := TSyncWord(R[i][idx_wl]);
        if W.StartPos <= Pos then
          begin
            if (Count < 0) and
               ((Pos < W.EndPos - 1) and (Pos - Count > W.EndPos - 1) or
                (idx_wl < R[i].Count - 1) and (R[i][idx_wl + 1].StartPos <= Pos - Count)) then
               Exit;
            Result := TList.Create;
            offs := Pos - W.StartPos;
            for j := 0 to R[i].Count - 1 do
             Result.Add(TObject(R[i][j].StartPos + offs));
            Exit;
          end;
      end;
   end;
end;

procedure TSyntSyncEdit.SetActiveWordsStyle(const Value: TSyntaxFormat);
begin
  FActiveWordsStyle.Assign(Value);
end;

procedure TSyntSyncEdit.SetInactiveWordsStyle(const Value: TSyntaxFormat);
begin
  FInactiveWordsStyle.Assign(Value);
end;

procedure TSyntSyncEdit.SetSyncRangeStyle(const Value: TSyntaxFormat);
begin
  FSyncRangeStyle.Assign(Value);
end;

function TSyntSyncEdit.AddRange(AStart, AEnd: integer): Boolean;
var R: TSyncRecord;
    CanSync: Boolean;
    i: integer;
begin
  Result := False;
  if FEnabled and (AStart < AEnd) then
    begin
      R := TSyncRecord.Create(AStart, AEnd, FOwner.Lines.FText, IgnoreCase);
      if Assigned(FOwner.OnCanSyncEdit) then
       for i := R.Count - 1 downto 0 do
        begin
         CanSync := True;
         FOwner.OnCanSyncEdit(FOwner, R[i], CanSync);
         if not CanSync then
           R.Delete(i);
        end;
      if R.Count = 0 then
       begin
        R.Free;
        Exit;
       end;
      if FSyncList = nil then
        FSyncList := TRangeList.Create(False);
      FSyncList.DeleteIntersected(AStart, AEnd);
      FSyncList.Add(R);
      CaretPosChanged;
      Result := True;
    end;
end;

function TSyntSyncEdit.CaretPosChanged: Boolean;
var cp, i: integer;
    R: TSyncRecord;
    Old: TPoint;
begin
  Result := False;
  if FSyncList = nil then Exit;
  Old := FActiveCoord;
  FActiveCoord.X := -1;
  try
    cp := FOwner.CaretStrPos;
    FActiveCoord.X := FSyncList.RangeAt(cp);
    if FActiveCoord.X <> -1 then
     begin
      R := TSyncRecord(FSyncList[FActiveCoord.X]);
      for i := 0 to R.Count - 1 do
       if R[i].RangeAt(cp) <> -1 then
        begin
          FActiveCoord.Y := i;
          Exit;
        end;
      FActiveCoord.Y := -1;
     end;
  finally
    Result := (FActiveCoord.X <> old.X) or (FActiveCoord.Y <> old.Y);
  end;
end;

function TSyntSyncEdit.AddCurSelection: Boolean;
begin
  with FOwner do
   begin
     Result := (SelLength > 0) and Self.AddRange(SelStart, SelStart + SelLength);
     if Result then
      begin
       ResetSelection;
       Invalidate;
      end;
   end;
end;

procedure TSyntSyncEdit.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
    begin
      FEnabled := Value;
      if not Enabled then
        Clear;
    end;
end;

procedure TSyntSyncEdit.Assign(Source: TPersistent);
var src: TSyntSyncEdit;
begin
  if Source is TSyntSyncEdit then
    begin
      Clear;
      src := TSyntSyncEdit(Source);
      FInactiveWordsStyle.Assign(src.InactiveWordsStyle);
      FActiveWordsStyle.Assign(src.ActiveWordsStyle);
      FSyncRangeStyle.Assign(src.SyncRangeStyle);
      FEnabled := src.Enabled;
    end;
end;

procedure TSyntSyncEdit.Validate;
var i, j, k, wl: integer;
begin
  if FSyncList <> nil then
   for i := FSyncList.Count - 1 downto 0 do
    with TSyncRecord(FSyncList[i]) do
     begin
       for j := Count - 1 downto 0 do
        begin
          wl := Items[j][0].Size;
          for k := 1 to Items[j].Count - 1 do
           if Items[j][k].Size <> wl then
            begin
              Delete(j);
              Break;
            end;
        end;
       if Count = 0 then
        FSyncList.Delete(i);
     end;
end;

function TSyntSyncEdit.GetCount: integer;
begin
  if FSyncList = nil then Result := 0
   else Result := FSyncList.Count;
end;

function TSyntSyncEdit.GetSyncRange(Index: integer): TRange;
begin
  if FSyncList = nil then
    raise EListError.CreateFmt(SListIndexError, [Index])
  else
    Result := FSyncList[Index];
end;

function TSyntSyncEdit.SyncRangeAt(Pos: integer): integer;
begin
  if FSyncList = nil then
    Result := -1
  else
    Result := FSyncList.RangeAt(Pos);
end;

procedure TSyntSyncEdit.Delete(Index: integer);
begin
  if FSyncList = nil then
    raise EListError.CreateFmt(SListIndexError, [Index])
  else
    FSyncList.Delete(Index);
  FOwner.Invalidate;
end;

function TSyntSyncEdit.RangeEndAtLine(Line: integer): integer;
var ls, le, i: integer;
begin
  Result := -1;
  if FSyncList <> nil then
   begin
    ls := FOwner.CaretPosToStrPos(Point(0, Line));
    le := ls + FOwner.Lines.LineSpace(Line);
    for i := 0 to Count - 1 do
     with FSyncList[i] do
      if (EndPos > ls) and (EndPos <= le) then
       begin
        Result := i;
        Exit;
       end;
   end;
end;

{ TStateStorage }

constructor TecMemoStateStorage.Create(AOwner: TComponent);
begin
  inherited;
  FUserRanges := TUserRanges.Create(nil);
  FBookmarks := TBookmarks.Create(nil);
  FColRanges := TCollapsedRanges.Create;
  FMarkers := TObjectList.Create;
end;

destructor TecMemoStateStorage.Destroy;
begin
  FUserRanges.Free;
  FBookmarks.Free;
  FColRanges.Free;
  FMarkers.Free;
  inherited;
end;

procedure TecMemoStateStorage.Assign(Source: TPersistent);
var Memo: TCustomSyntaxMemo;
    Src: TecMemoStateStorage;
begin
  // Saving settings
  if Source is TCustomSyntaxMemo then
    begin
      Memo := TCustomSyntaxMemo(Source);
      FCaretPos := Memo.CaretPos;
      FScrollPos := Point(Memo.ScrollPosX, Memo.ScrollPosY);
      FSelMode := Memo.SelectMode;
      FSelBlock := Memo.SelRect;
      FSelRange := Point(Memo.SelStart, Memo.SelLength);
      FReplaceMode := Memo.ReplaceMode;
      FWordWrap := Memo.WordWrap;
      FDisableFolding := Memo.DisableFolding;
      FUserRanges.Assign(Memo.UserRanges);
      FBookmarks.Assign(Memo.BookmarkObj);
      FColRanges.Assign(Memo.Collapsed);
      CopyMarkers(Memo.Markers, FMarkers);
      FModified := Memo.Modified;
    end else
  // Copy from other state storage
  if Source is TecMemoStateStorage then
    begin
      Src := TecMemoStateStorage(Source);
      FCaretPos := Src.FCaretPos;
      FScrollPos := Src.FScrollPos;
      FSelMode := Src.FSelMode;
      FSelBlock := Src.FSelBlock;
      FSelRange := Src.FSelRange;
      FReplaceMode := Src.FReplaceMode;
      FWordWrap := Src.FWordWrap;
      FModified := Src.Modified;
      FUserRanges.Assign(Src.FUserRanges);
      FBookmarks.Assign(Src.FBookmarks);
      FColRanges.Assign(Src.FColRanges);
    end
  else inherited;
end;

procedure TecMemoStateStorage.AssignTo(Dest: TPersistent);
var Memo: TCustomSyntaxMemo;
begin
  // Loading state
  if Dest is TCustomSyntaxMemo then
    begin
      Memo := TCustomSyntaxMemo(Dest);
      Memo.BeginUpdate;
      try
        Memo.CaretPos := FCaretPos;
        Memo.ReplaceMode := FReplaceMode;
        Memo.WordWrap := FWordWrap;
        Memo.UserRanges.Assign(FUserRanges);
        Memo.BookmarkObj.Assign(FBookmarks);
        CopyMarkers(FMarkers, Memo.Markers);
        Memo.Collapsed.Assign(FColRanges);
        Memo.ScrollPosX := FScrollPos.X;
        Memo.ScrollPosY := FScrollPos.Y;
        Memo.SelectMode := FSelMode;
        Memo.DisableFolding := FDisableFolding;
        Memo.Modified := FModified;
        if Memo.SelectMode = msColumn then
          Memo.FBlock := FSelBlock
        else
          Memo.IntSetSelection(FSelRange.X, FSelRange.Y);
      finally
        Memo.EndUpdate;
      end;
    end
  else inherited;
end;

procedure TecMemoStateStorage.CopyMarkers(Src, Dest: TList);
var i: integer;
    m: TMarker;
begin
  Dest.Clear;
  for i := 0 to Src.Count - 1 do
   begin
    m := TMarker.Create;
    m.CopyFrom(TMarker(Src[i]));
    Dest.Add(m);
   end;
end;

{ TecBackGround }

constructor TecBackGround.Create(AOnChange: TNotifyEvent);
begin
  inherited Create;
  FOnChange := AOnChange;
  FBitmap := TBitmap.Create;
  FGradColor := clBlue;
end;

destructor TecBackGround.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TecBackGround.Assign(Source: TPersistent);
begin
  if Source is TecBackGround then
   with Source as TecBackGround do
   begin
     Self.FGradColor := FGradColor;
     Self.FBitmap.Assign(FBitmap);
     Self.FFillStyle := FFillStyle;
     Self.Changed;
   end else inherited;
end;

procedure TecBackGround.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  Changed;
end;

procedure TecBackGround.SetFillStyle(const Value: TFillStyle);
begin
  if FFillStyle <> Value then
   begin
    FFillStyle := Value;
    Changed;
   end;
end;

procedure TecBackGround.SetGradColor(const Value: TColor);
begin
  if FGradColor <> Value then
   begin
    FGradColor := Value;
    Changed;
   end;
end;

procedure TecBackGround.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TecBackGround.FillRect(Canvas: TCanvas; const R: TRect; DefColor: TColor);
var x, y: integer;
begin
  case FFillStyle of
   fstCenter,
   fstNone: begin
              Canvas.Brush.Color := DefColor;
              Canvas.Brush.Style := bsSolid;
              Canvas.FillRect(R);
              if FFillStyle = fstCenter then
                Canvas.Draw((R.Left + R.Right - FBitmap.Width) div 2,
                            (R.Top + R.Bottom - FBitmap.Height) div 2,
                            FBitmap);
            end;
   fstHorzGradient:
              GradientFill(Canvas, R, DefColor, GradColor, False);
   fstVertGradient:
              GradientFill(Canvas, R, DefColor, GradColor, True);
   fstTile: if not FBitmap.Empty then
             begin
              y := R.Top;
              while y < R.Bottom do
               begin
                x := R.Left;
                while x < R.Right do
                 begin
                  Canvas.Draw(x, y, FBitmap);
                  Inc(x, FBitmap.Width);
                 end;
                Inc(y, FBitmap.Height);
               end;
             end;
   fstStretch:
           Canvas.StretchDraw(R, FBitmap);
  end;
end;

function TecBackGround.IsDefault: Boolean;
begin
  Result := True;
  case FFillStyle of
    fstHorzGradient,
    fstVertGradient: Result := False;
    fstCenter,
    fstTile,
    fstStretch:      Result := FBitmap.Empty;
  end;
end;

{ TSyntMemoPlugin }

destructor TSyntMemoPlugin.Destroy;
begin
  if Assigned(FSyntMemo) then
    SetSyntMemo(nil);
  inherited;
end;

procedure TSyntMemoPlugin.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSyntMemo) then
   FSyntMemo := nil;
end;

procedure TSyntMemoPlugin.SetSyntMemo(const Value: TCustomSyntaxMemo);
begin
  if FSyntMemo <> Value then
    begin
      if FSyntMemo <> nil then
        begin
          FSyntMemo.RemoveClient(Self);
          FSyntMemo.RemoveFreeNotification(Self);
        end;
      FSyntMemo := Value;
      if FSyntMemo <> nil then
        begin
          FSyntMemo.FreeNotification(Self);
          FSyntMemo.AddClient(Self);
        end;
   end;
end;

{ TecTextAttributes }

constructor TecTextAttributes.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TecTextAttributes.CanFormat: Boolean;
begin
  Result := Enabled;
end;

function TecTextAttributes.CreateEmptyStyle: TSyntaxFormat;
begin
  Result := TSyntaxFormat.Create(nil);
  Result.FormatType := ftCustomFont;
  Result.Font.Color := clNone;
  Result.FormatFlags := [];
end;

function TecTextAttributes.GetFontStyle(const Index: integer): Boolean;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if Stl = nil then Result := False else
    try
      if (Stl.FormatType in [ftColor, ftBackground]) or
         not (TFormatFlag(Index) in Stl.FormatFlags) then
        Result := False
      else
        Result := TFontStyle(Index) in Stl.Font.Style;
    finally
      Stl.Free;
    end;
end;

procedure TecTextAttributes.SetFontStyle(const Index: integer;
  const Value: Boolean);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;

  Stl := CreateEmptyStyle;
  try
    Stl.FormatFlags := [TFormatFlag(Index)];
    if Value then
      Stl.Font.Style := [TFontStyle(Index)]
    else
      Stl.Font.Style := [];

    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

function TecTextAttributes.IterateSelection(Proc: TSelRgnProc;
  var Data: TSyntaxFormat): Boolean;
var i, sR, sL: integer;
begin
  with FOwner do
  if not HaveSelection then
    begin
      WordRangeAtPos(CaretPos, sL, sR);
      Result := (sR > sL) and Proc(sL, sR, Data);
    end else
  if SelectMode = msColumn then
    begin
      Result := False;
      for i := SelRect.Top to SelRect.Bottom do
        begin
          sL := LogToLinesPos(Point(SelRect.Left, i)).X;
          sR := LogToLinesPos(Point(SelRect.Right, i)).X - sL;
          if sR > 0 then
            begin
              Result := Proc(sL, sL + sR, Data);
              if not Result then
                Exit;
            end;
        end;
    end else
    begin
      sL := SelStart;
      sR := sL + SelLength;
      Result := Proc(sL, sR, Data);
    end;
end;

function TecTextAttributes.ApplyStyle_Int(StartPos, EndPos: integer;
  var Data: TSyntaxFormat): Boolean;
begin
  with FOwner do
  if EndPos > StartPos then
    begin
      TecEmbeddedObjects(TextSource).FormatRange(StartPos, EndPos, Data);
      ResetLineHeights(False, StrPosToCaretPos(StartPos).Y, StrPosToCaretPos(EndPos).Y);
    end;
  Result := True;
end;

procedure TecTextAttributes.ApplyStyle(Stl: TSyntaxFormat);
begin
  if CanFormat and IterateSelection(ApplyStyle_Int, Stl) then
    FOwner.Invalidate;
end;

function TecTextAttributes.GetSelStyle_Int(StartPos, EndPos: integer;
  var Data: TSyntaxFormat): Boolean;
begin
  TecEmbeddedObjects(FOwner.TextSource).RangeStyle(StartPos, EndPos, Data);
  Result := Assigned(Data);
end;

function TecTextAttributes.GetSelStyle: TSyntaxFormat;
begin
  Result := nil;
  if CanFormat then
    IterateSelection(GetSelStyle_Int, Result);
end;

procedure TecTextAttributes.ClearFormat;
begin
  if CanFormat then
    begin
      TecEmbeddedObjects(FOwner.TextSource).ClearFormatting;
      FOwner.ResetLineHeights;
      FOwner.Invalidate;
    end;
end;

function TecTextAttributes.ClearFormat_Int(StartPos, EndPos: integer;
  var Data: TSyntaxFormat): Boolean;
begin
  with FOwner do
    begin
      TecEmbeddedObjects(TextSource).ClearFormatting(StartPos, EndPos);
      ResetLineHeights(False, StrPosToCaretPos(StartPos).Y, StrPosToCaretPos(EndPos).Y);
    end;
  Result := True;
end;

procedure TecTextAttributes.ClearSelFormat;
var Data: TSyntaxFormat;
begin
  if CanFormat then
    begin
      IterateSelection(ClearFormat_Int, Data);
      FOwner.Invalidate;
    end;
end;

function TecTextAttributes.GetBgColor: TColor;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if Stl = nil then Result := clNone else
    try
      Result := Stl.BgColor;
    finally
      Stl.Free;
    end;
end;

function TecTextAttributes.GetCharset: TFontCharset;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if (Stl = nil) or
     (Stl.FormatType <> ftCustomFont) or
     not (ffFontCharset in Stl.FormatFlags) then Result := DEFAULT_CHARSET else
    try
      Result := Stl.Font.Charset;
    finally
      Stl.Free;
    end;
end;

function TecTextAttributes.GetColor: TColor;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if (Stl = nil) or (Stl.FormatType = ftBackGround) then Result := clNone else
    try
      Result := Stl.Font.Color;
    finally
      Stl.Free;
    end;
end;

function TecTextAttributes.GetFontName: TFontName;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if (Stl = nil) or
     (Stl.FormatType <> ftCustomFont) or
     not (ffFontName in Stl.FormatFlags) then Result := '' else
    try
      Result := Stl.Font.Name;
    finally
      Stl.Free;
    end;
end;

function TecTextAttributes.GetFontSize: integer;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if (Stl = nil) or
     (Stl.FormatType <> ftCustomFont) or
     not (ffFontSize in Stl.FormatFlags) then Result := 0 else
    try
      Result := Stl.Font.Size;
    finally
      Stl.Free;
    end;
end;

procedure TecTextAttributes.SetBgColor(const Value: TColor);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;
  Stl := CreateEmptyStyle;
  try
    Stl.BgColor := Value;
    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

procedure TecTextAttributes.SetCharset(const Value: TFontCharset);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;
  Stl := CreateEmptyStyle;
  try
    Stl.Font.Charset := Value;
    Stl.FormatFlags := [ffFontCharset];
    Stl.FormatType := ftCustomFont;
    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

procedure TecTextAttributes.SetColor(const Value: TColor);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;
  Stl := CreateEmptyStyle;
  try
    Stl.Font.Color := Value;
    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

procedure TecTextAttributes.SetFontName(const Value: TFontName);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;
  Stl := CreateEmptyStyle;
  try
    Stl.Font.Name := Value;
    Stl.FormatFlags := [ffFontName];
    Stl.FormatType := ftCustomFont;
    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

procedure TecTextAttributes.SetFontSize(const Value: integer);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;
  Stl := CreateEmptyStyle;
  try
    Stl.Font.Size := Value;
    Stl.FormatFlags := [ffFontSize];
    Stl.FormatType := ftCustomFont;
    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

function TecTextAttributes.GetHidden: Boolean;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if (Stl = nil) or
     not (ffHidden in Stl.FormatFlags) then Result := False else
    try
      Result := Stl.Hidden;
    finally
      Stl.Free;
    end;
end;

function TecTextAttributes.GetReadOnly: Boolean;
var Stl: TSyntaxFormat;
begin
  Stl := GetSelStyle;
  if (Stl = nil) or
     not (ffReadOnly in Stl.FormatFlags) then Result := False else
    try
      Result := Stl.ReadOnly;
    finally
      Stl.Free;
    end;
end;

procedure TecTextAttributes.SetHidden(const Value: Boolean);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;
  Stl := CreateEmptyStyle;
  try
    Stl.Hidden := Value;
    Stl.FormatFlags := [ffHidden];
    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

procedure TecTextAttributes.SetReadOnly(const Value: Boolean);
var Stl: TSyntaxFormat;
begin
  if not CanFormat then Exit;
  Stl := CreateEmptyStyle;
  try
    Stl.ReadOnly := Value;
    Stl.FormatFlags := [ffReadOnly];
    ApplyStyle(Stl);
  finally
    Stl.Free;
  end;
end;

procedure TecTextAttributes.SetEnabled(const Value: Boolean);
begin
  if Assigned(FOwner) then//AT
  if Assigned(FOwner.TextSource) and
     (FOwner.TextSource is TecEmbeddedObjects) then
    TecEmbeddedObjects(FOwner.TextSource).FormatEnabled := Value;
end;

function TecTextAttributes.GetEnabled: Boolean;
begin
  if Assigned(FOwner.TextSource) and
     (FOwner.TextSource is TecEmbeddedObjects) then
    Result := TecEmbeddedObjects(FOwner.TextSource).FormatEnabled
  else
    Result := False;
end;

{ TecHorzRuler }

constructor TecHorzRuler.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
  FBackGround := TecBackGround.Create(RulerChanged);
  FFont := TFont.Create;
  FFont.OnChange := RulerChanged;
  FColor := clNone;
  FSeparatorColor := clGray;
  FLabelInterval := 10;
  FMinorTicksInterval := 1;
  FMajorTicksInterval := 5;
  FMajorTicksLength := 5;
  FMinorTicksLength := 2;
  FHeight := 17;
  FShowCurrentPos := True;
  FMinorTicksColor := clBlack;
  FMajorTicksColor := clBlack;
end;

destructor TecHorzRuler.Destroy;
begin
  FreeAndNil(FBackGround);
  FreeAndNil(FFont);
  inherited;
end;

procedure TecHorzRuler.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TecHorzRuler.Paint(Canvas: TCanvas);
var bmp: TBitmap;
begin
  if (FHeight = 0) or (FOwner.ClientWidth = 0) or not FVisible then
    Exit;

  if not FOwner.DoubleBuffered then
    begin
      bmp := TBitmap.Create;
      try
        bmp.Width := FOwner.ClientWidth;
        bmp.Height := FHeight;
        PaintTo(bmp.Canvas);
        Canvas.Draw(0, 0, bmp);
      finally
        bmp.Free;
      end;
    end else
      PaintTo(Canvas);
end;

procedure TecHorzRuler.PaintTo(Canvas: TCanvas);
var R: TRect;
    DX, X, nX, tick_len, Tmp, i: integer;
    sz: TSize;
    S: string;
begin
  R := FOwner.ClientRect;
  R.Bottom := FHeight;
  with Canvas do
    begin
      // Backgound
      if FColor = clNone then
        Brush.Color := FOwner.Color
      else
        Brush.Color := FColor;
      FBackGround.FillRect(Canvas, R, Brush.Color);
      R.Bottom := R.Bottom - 1;
      // Separator
      if FSeparatorColor <> clNone then
        begin
          Pen.Color := FSeparatorColor;
          Pen.Width := 1;
          Pen.Style := psSolid;
          MoveTo(R.Left, R.Bottom);
          LineTo(R.Right, R.Bottom);
          R.Bottom := R.Bottom - 1;
        end;
      // Ticks
      R.Left := FOwner.FMargin.Left;
      R.Right := R.Right - FOwner.FMargin.Right;
      nX := FOwner.ScrollPosX;
      DX := FOwner.DefTextExt.cx;
      if Dx = 0 then
        Exit; // Incorrect situation
      X := R.Left;
      Pen.Width := 1;
      Pen.Color := clBlack;
      while X <= R.Right do
        begin
          tick_len := 0;
          if (FMajorTicksInterval > 0) and (Nx mod FMajorTicksInterval = 0) and (tick_len < FMajorTicksLength) then
            begin
              tick_len := FMajorTicksLength;
              Pen.Color := FMajorTicksColor;
            end;
          if (FMinorTicksInterval > 0) and (Nx mod FMinorTicksInterval = 0) and (tick_len < FMinorTicksLength) then
            begin
              tick_len := FMinorTicksLength;
              Pen.Color := FMinorTicksColor;
            end;
          if tick_len > 0 then
            begin
              MoveTo(X, R.Bottom);
              LineTo(X, R.Bottom - tick_len);
            end;
          Inc(nX);
          Inc(X, DX);
        end;
      // Text margin marks
      nX := FOwner.ScrollPosX;
      for i := 0 to FOwner.TextMargins.Count - 1 do
        if FOwner.TextMargins[i].RulerMark then
          begin
            if FOwner.TextMargins.AbsolutePos then
              X := R.Left + FOwner.TextMargins[i].Position - nX * DX
            else
              X := R.Left + (FOwner.TextMargins[i].Position - nX) * DX;
            if X < R.Right + 1 then
              begin
                Pen := FOwner.TextMargins[i].Pen;
                Pen.Width := 1;
                MoveTo(X - 1, R.Top);
                LineTo(X - 1, R.Bottom +1);
                MoveTo(X + 1, R.Top);
                LineTo(X + 1, R.Bottom +1);
              end;
          end;
      // Labels
      if FLabelInterval > 0 then
        begin
          Font := Self.Font;
          Brush.Style := bsClear;
          R.Bottom := R.Bottom - 2;
          X := R.Left;
          nX := FOwner.ScrollPosX;
          if (nX mod FLabelInterval) > 0 then
            begin
              Tmp := FLabelInterval * (nX div FLabelInterval + 1);
              X := X + (Tmp - nX) * Dx;
              nX := Tmp;
            end;
          while X <= R.Right do
            begin
              S := IntToStr(nX);
              sz := TextExtent(S);
              TextOut(X - sz.cx div 2, R.Bottom - sz.cy, S);
              Inc(nX, FLabelInterval);
              Inc(X, DX * FLabelInterval);
            end;
        end;
      // Current position
      if FShowCurrentPos then
        begin
          nX := FOwner.ScrollPosX;
          if FCurPos >= nX then
            begin
              X := R.Left + (FCurPos - nX) * DX;
              if X < R.Right then
                begin
                  Pen.Width := 1;
                  Pen.Color := clBlack;
                  Brush.Color := clBlack;
                  Brush.Style := bsSolid;
                  FillRect(Bounds(x - 3, R.Top, 7, 1));
                  MoveTo(X, R.Top + 1);
                  LineTo(X, R.Top + 5);
                end;
            end;
        end;
    end;
end;

procedure TecHorzRuler.RulerChanged(Sender: TObject);
begin
  Changed;
end;

procedure TecHorzRuler.SetBackGround(const Value: TecBackGround);
begin
  FBackGround.Assign(Value);
end;

procedure TecHorzRuler.SetColor(const Value: TColor);
begin
  if FColor <> Value then
    begin
      FColor := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TecHorzRuler.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
    begin
      FHeight := Value;
      FOwner.UpdateMargin;
      UpdateCurrent;
    end;
end;

procedure TecHorzRuler.SetLabelInterval(const Value: integer);
begin
  if FLabelInterval <> Value then
    begin
      FLabelInterval := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetMajorTicksInterval(const Value: integer);
begin
  if FMajorTicksInterval <> Value then
    begin
      FMajorTicksInterval := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetMajorTicksLength(const Value: integer);
begin
  if FMajorTicksLength <> Value then
    begin
      FMajorTicksLength := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetMinorTicksInterval(const Value: integer);
begin
  if FMinorTicksInterval <> Value then
    begin
      FMinorTicksInterval := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetMinorTicksLength(const Value: integer);
begin
  if FMinorTicksLength <> Value then
    begin
      FMinorTicksLength := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetSeparatorColor(const Value: TColor);
begin
  if FSeparatorColor <> Value then
    begin
      FSeparatorColor := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetShowCurrentPos(const Value: Boolean);
begin
  if FShowCurrentPos <> Value then
    begin
      FShowCurrentPos := Value;
      if FShowCurrentPos then
        UpdateCurrent
      else
        FCurPos := -1;
      Changed;
    end;
end;

procedure TecHorzRuler.SetMajorTicksColor(const Value: TColor);
begin
  if FMajorTicksColor <> Value then
    begin
      FMajorTicksColor := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.SetMinorTicksColor(const Value: TColor);
begin
  if FMinorTicksColor <> Value then
    begin
      FMinorTicksColor := Value;
      Changed;
    end;
end;

procedure TecHorzRuler.UpdateCurrent;
var newPos: integer;
    R: TRect;
begin
  if FShowCurrentPos and (FHeight > 0) and FVisible then
    begin
      newPos := FOwner.LinesPosToLog(FOwner.CaretPos).X;
      if newPos <> FCurPos then
        begin
          FCurPos := newPos;
          if FOwner.HandleAllocated then
            begin
              with FOwner do
                R := Rect(FMargin.Left, 0, ClientWidth - FMargin.Right, FMargin.Top);
                InvalidateRect(FOwner.Handle, {$IFNDEF EC_DOTNET}@{$ENDIF}R, True);
            end;
        end;
    end;
end;

procedure TecHorzRuler.Assign(Source: TPersistent);
begin
  if Source is TecHorzRuler then
    with Source As TecHorzRuler do
      begin
        Self.Font := Font;
        Self.Color := Color;
        Self.BackGround := BackGround;
        Self.MinorTicksLength := MinorTicksLength;
        Self.MajorTicksLength := MajorTicksLength;
        Self.MinorTicksInterval := MinorTicksInterval;
        Self.MajorTicksInterval := MajorTicksInterval;
        Self.MinorTicksColor := MinorTicksColor;
        Self.MajorTicksColor := MajorTicksColor;
        Self.LabelInterval := LabelInterval;
        Self.Height := Height;
        Self.SeparatorColor := SeparatorColor;
        Self.ShowCurrentPos := ShowCurrentPos;
      end
  else
    inherited;

end;

procedure TecHorzRuler.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      FVisible := Value;
      FOwner.UpdateMargin;
      UpdateCurrent;
    end;
end;

{ TecTextMargin }

procedure TecTextMargin.Assign(Source: TPersistent);
begin
  if Source is TecTextMargin then
    with Source as TecTextMargin do
      begin
        Self.FAllowDrag := FAllowDrag;
        Self.FVisible := FVisible;
        Self.FPosition := FPosition;
        Self.FHint := FHint;
        Self.FPen.Assign(FPen);
        Changed(False);
      end
  else
    inherited;
end;

constructor TecTextMargin.Create(Collection: TCollection);
begin
  inherited;
  FPen := TPen.Create;
  FPen.OnChange := PenChnaged;
  FVisible := True;
  FAllowDrag := True;
  FPosition := 80;
end;

destructor TecTextMargin.Destroy;
begin
  FreeAndNil(FPen);
  inherited;
end;

procedure TecTextMargin.PenChnaged(Sender: TObject);
begin
  Changed(False);
end;

procedure TecTextMargin.SetAllowDrag(const Value: Boolean);
begin
  if FAllowDrag <> Value then
    begin
      FAllowDrag := Value;
      Changed(False);
    end;
end;

procedure TecTextMargin.SetHint(const Value: string);
begin
  if FHint <> Value then
    begin
      FHint := Value;
      Changed(False);
    end;
end;

procedure TecTextMargin.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TecTextMargin.SetPosition(Value: integer);
begin
  if Value < 0 then
    Value := 0;
  if FPosition <> Value then
    begin
      FPosition := Value;
      if (Index = 0) and Assigned(Collection) then
        with TCustomSyntaxMemo(TecTextMargins(Collection).GetOwner) do
          ResetLineHeights(True);
      Changed(False);
    end;
end;

procedure TecTextMargin.SetRulerMark(const Value: Boolean);
begin
  if FRulerMark <> Value then
    begin
      FRulerMark := Value;
      Changed(False);
    end;
end;

procedure TecTextMargin.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      FVisible := Value;
      Changed(False);
    end;
end;

{ TecTextMargins }

function TecTextMargins.Add: TecTextMargin;
begin
  Result := TecTextMargin(inherited Add);
end;

constructor TecTextMargins.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TecTextMargin);
  FAllowDrag := True;
  FDragWithCtrl := True;
  FAbsolutePos := False;
end;

function TecTextMargins.GetItem(Index: integer): TecTextMargin;
begin
  Result := TecTextMargin(inherited GetItem(Index));
end;

procedure TecTextMargins.SetAbsolutePos(const Value: Boolean);
begin
  if FAbsolutePos <> Value then
    begin
      FAbsolutePos := Value;
      TCustomSyntaxMemo(GetOwner).Invalidate;
    end;
end;

procedure TecTextMargins.SetItem(Index: integer;
  const Value: TecTextMargin);
begin
  inherited SetItem(Index, Value);
end;

procedure TecTextMargins.Update(Item: TCollectionItem);
begin
  TCustomSyntaxMemo(GetOwner).Invalidate;
end;

{ TecCaretShape }

constructor TecCaretShape.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FColor := clDefault;
  FWidth := -2; // absolute
  FHeight := 100; // percent
  FIsGray := False;
  FVisible := True;
  FBlinkTime := 0;
  FStretch := True;
end;

destructor TecCaretShape.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TecCaretShape.Assign(Source: TPersistent);
begin
  if Source is TecCaretShape then
    with TecCaretShape(Source) do
      begin
        Self.FWidth := Width;
        Self.FHeight := Height;
        Self.FBitmap.Assign(Bitmap);
        Self.FColor := Color;
        Self.FIsGray := IsGray;
        Self.FVisible := Visible;
        Self.FStretch := Stretch;
        Self.Changed;
      end else
  inherited;
end;

procedure TecCaretShape.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TecCaretShape.SetColor(const Value: TColor);
begin
  if FColor <> Value then
    begin
      FColor := Value;
      Changed;
    end;
end;

procedure TecCaretShape.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
    begin
      if Value > 100 then
        raise Exception.Create('Caret height should be less 100%');
      FHeight := Value;
      Changed;
    end;
end;

procedure TecCaretShape.SetIsGray(const Value: Boolean);
begin
  if FIsGray <> Value then
    begin
      FIsGray := Value;
      Changed;
    end;
end;

procedure TecCaretShape.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
    begin
      if Value > 100 then
        raise Exception.Create('Caret width should be less 100%');
      FWidth := Value;
      Changed;
    end;
end;

procedure TecCaretShape.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      FVisible := Value;
      Changed;
    end;
end;

procedure TecCaretShape.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TecCaretShape.BitmapChanged(Sender: TObject);
begin
  Changed;
end;

procedure TecCaretShape.SetBlinkTime(const Value: Cardinal);
begin
  if FBlinkTime <> Value then
    begin
      FBlinkTime := Value;
      Changed;
    end;
end;

procedure TecCaretShape.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
    begin
      FStretch := Value;
      if not FBitmap.Empty then
        Changed;
    end;
end;

{ TecCaret }

constructor TecCaret.Create(AOwner: TCustomSyntaxMemo);
begin
  inherited Create;
  FOwner := AOwner;
  FReadOnly := TecCaretShape.Create;
  FReadOnly.OnChange := ShapeChanged;
  FReadOnly.Width := -1;
  FInsert := TecCaretShape.Create;
  FInsert.OnChange := ShapeChanged;
  FInsert.Width := -2;
  FOverwrite := TecCaretShape.Create;
  FOverwrite.OnChange := ShapeChanged;
  FOverwrite.Width := 100;
  FCustom := TecCaretShape.Create;
  FCustom.OnChange := ShapeChanged;
  FOldBlinkTime := -1;
  FVisible := True;
  FUseCustom := False;
end;

destructor TecCaret.Destroy;
begin
  FreeAndNil(FReadOnly);
  FreeAndNil(FInsert);
  FreeAndNil(FOverwrite);
  FreeAndNil(FCustom);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TecCaret.ShapeChanged(Sender: TObject);
begin
  if Sender = FCurShape then
    begin
      FCurShape := nil;
      Update;
    end;
end;

procedure TecCaret.Assign(Source: TPersistent);
begin
  if Source is TecCaret then
    with TecCaret(Source) do
      begin
        Self.Insert := Insert;
        Self.Overwrite := Overwrite;
        Self.ReadOnly := ReadOnly;
        Self.Custom := Custom;
        Self.Visible := Visible;
        Self.UseCustom := UseCustom;
      end else
  inherited;
end;

procedure TecCaret.SetInsert(const Value: TecCaretShape);
begin
  FInsert.Assign(Value);
end;

procedure TecCaret.SetOverwrite(const Value: TecCaretShape);
begin
  FOverwrite.Assign(Value);
end;

procedure TecCaret.SetReadOnly(const Value: TecCaretShape);
begin
  FReadOnly.Assign(Value);
end;

procedure TecCaret.Hide;
begin
  if FIsShown then
    begin
      DestroyCaret;
      FIsShown := False;
      FCurShape := nil;
      if FOldBlinkTime > -1 then
        begin
          SetCaretBlinkTime(FOldBlinkTime);
          FOldBlinkTime := -1;
        end;
    end;
end;

procedure TecCaret.Update;
begin
//  if FIsShown then
  Show(FPosition);
end;

procedure TecCaret.Show(Pos: TPoint);
var NewShape: TecCaretShape;
    NewSize: TSize;
    hBmp, P, FOldOffset: integer;
    R: TRect;

  procedure CreateBitmap;
  begin
    FBitmap := TBitmap.Create;
    FBitmap.Width := FSize.cx;
    FBitmap.Height := FSize.cy;
    hBmp := FBitmap.Handle;
  end;

begin
  FPosition := Pos;
  if FUseCustom then
    begin
      NewShape := FCustom;
    end else
  if FOwner.ReadOnly then
    begin
      if soAlwaysShowCaret in FOwner.Options then
        NewShape := FReadOnly
      else
        NewShape := nil;
    end else
  with FOwner do
    if ReplaceMode and (FCaretPos.Y < Lines.Count) and (FCaretPos.X < LineLength(FCaretPos.Y)) then
      NewShape := FOverwrite
    else
      NewShape := FInsert;

  if not Assigned(NewShape) or not NewShape.Visible or not FVisible or
     not FOwner.HandleAllocated or (csDesigning in FOwner.ComponentState) or
     not FOwner.Focused then
    Hide else
  begin
    if NewShape.Width < 0 then
      NewSize.cx := -NewShape.Width
    else
      begin
        with FOwner do
          if (FCaretPos.Y < Lines.Count) and (FCaretPos.X < LineLength(FCaretPos.Y)) then
            begin
              P := CaretStrPos;
              SetCanvasAtPos(Canvas, P);
              P := ecTextExtent(Canvas, Lines.FText[P + 1]).cx;
            end else P := DefTextExt.cx;
        NewSize.cx := P * NewShape.Width div 100;
      end;

    FOldOffset := FYOffset;
    FYOffset := 0;
    if NewShape.Height < 0 then
      NewSize.cy := -NewShape.Height
    else
      begin
        P := FOwner.LineHeight(FOwner.FCaretPos);
        if P = 0 then P := FOwner.DefTextExt.cy;
        NewSize.cy := P * NewShape.Height div 100;
        FYOffset := P - NewSize.cy;
      end;

    if NewSize.cx = 0 then NewSize.cx := 1;
    if NewSize.cy = 0 then NewSize.cy := 1;

    if not FIsShown or (NewShape <> FCurShape) or
       (NewSize.cx <> FSize.cx) or (NewSize.cy <> FSize.cy) then
      begin
        if Assigned(FBitmap) then
          begin // Erase previous bitmap shape (fix of Win32 behavior)
            DestroyCaret;
            R := Bounds(FPosition.X, FPosition.Y + FOldOffset, FSize.cx, FSize.cy);
            InvalidateRect(FOwner.Handle, {$IFNDEF EC_DOTNET}@{$ENDIF}R, True);
          end;

        FSize := NewSize;
        FCurShape := NewShape;
        FreeAndNil(FBitmap);
        if not NewShape.Bitmap.Empty then
          begin
            CreateBitmap;
            FBitmap.Canvas.CopyMode := SRCINVERT;
            if NewShape.Stretch then
              FBitmap.Canvas.StretchDraw(Rect(0, 0, FBitmap.Width, FBitmap.Height), NewShape.Bitmap)
            else
              FBitmap.Canvas.Draw(0, 0, NewShape.Bitmap);
          end else
        if NewShape.Color <> clDefault then
          begin
            CreateBitmap;
            FBitmap.Canvas.Brush.Color := ColorToRGB(NewShape.Color) xor ColorToRGB(FOwner.Color);
            FBitmap.Canvas.Brush.Style := bsSolid;
            FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
          end else
        if NewShape.IsGray then
          hBmp := 1
        else
          hBmp := 0;

        CreateCaret(FOwner.Handle, hBmp, FSize.cx, FSize.cy);
        ShowCaret(FOwner.Handle);
        FIsShown := True;
      end;

    if NewShape.BlinkTime > 0 then
      begin
        if (FOldBlinkTime = -1) or (GetCaretBlinkTime <> NewShape.BlinkTime) then
          begin
            if FOldBlinkTime = -1 then
              FOldBlinkTime := GetCaretBlinkTime;
            SetCaretBlinkTime(NewShape.BlinkTime);
          end;
      end else
      begin
        if FOldBlinkTime <> -1 then
          begin
            SetCaretBlinkTime(FOldBlinkTime);
            FOldBlinkTime := -1;
          end;
      end;

    SetCaretPos(Pos.X, Pos.Y + FYOffset);
  end;
end;

procedure TecCaret.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      FVisible := Value;
      Update;
    end;
end;

procedure TecCaret.SetCustom(const Value: TecCaretShape);
begin
  FCustom.Assign(Value);
end;

procedure TecCaret.SetUseCustom(const Value: Boolean);
begin
  if FUseCustom <> Value then
    begin
      FUseCustom := Value;
      Update;
    end;
end;

  //AT
  procedure TCustomSyntaxMemo.DrawScroll;
  var R: TRect; p: TPoint;
  const c = 16; d = 4;
  begin
    if not FDoScroll then Exit;
    p:= FDoScrollOrg;
    Canvas.Brush.Color:= clWhite;
    Canvas.Pen.Color:= clDkGray;
    R:= Rect(p.x - c, p.y - c, p.x + c, p.y + c);
    Canvas.Ellipse(R);
    Canvas.Brush.Color:= clDkGray;
    Canvas.Polygon([Point(p.x, R.Top + d),
      Point(p.x + c - d-4, p.y - d),
      Point(p.x - c + d+4, p.y - d)]);
    Canvas.Polygon([Point(p.x, R.Bottom - d),
      Point(p.x + c - d-4, p.y + d),
      Point(p.x - c + d+4, p.y + d)]);
    Canvas.Ellipse(Rect(p.x - d div 2, p.y - d div 2,
      p.x + d div 2, p.y + d div 2));
  end;

//AT
procedure TCustomSyntaxMemo.DoTimerScroll(Sender: TObject);
var d: Integer; p: TPoint;
begin
  if FDoScroll then
  begin
    p:= ScreenToClient(Mouse.CursorPos);
    if Abs(p.Y - FDoScrollOrg.y) < 16 then Exit;
    d:= (p.Y - FDoScrollOrg.y) div 16;
    ScrollPosY:= ScrollPosY + d;
  end;
end;

//AT
const
  sCharsSp = ' '#9;
  sCharsEOL = #10#13;
  sCharsSymb = '!"#%&''()[]{}<>*+-/=,.:;?\^`|~‚„…‹›‘’“”–—¦«»­±'; //no chars '@' and '$'

function TCustomSyntaxMemo.SCharGr(ch: ecChar): TCharGr;
begin
  if IsWordChar(ch) then Result:= cgWord else
   if Pos(ch, sCharsSp)>0 then Result:= cgSp else
    if Pos(ch, sCharsEOL)>0 then Result:= cgEOL else
     if Pos(ch, sCharsSymb)>0 then Result:= cgSymb else
      Result:= cgWord;
end;

procedure TCustomSyntaxMemo.DoWordJump(ANext: boolean);
begin
  CaretStrPos:= DoWordJumpPos(CaretStrPos, ANext);
end;

function TCustomSyntaxMemo.DoWordJumpPos(NFromPos: Integer; ANext: boolean): Integer;
var
  s: ecString;
  n: Integer;
  //------------
  procedure Next;
  var gr: TCharGr;
  begin
    if not ((n>=0) and (n<Length(s))) then Exit;
    gr:= SCharGr(s[n+1]);
    repeat Inc(n)
    until
      (n>=Length(s)) or (SCharGr(s[n+1])<>gr);
  end;
  //------------
  procedure Home;
  var gr: TCharGr;
  begin
    if not ((n>0) and (n<=Length(s))) then Exit;
    gr:= SCharGr(s[n+1]);
    while (n>0) and (SCharGr(s[n])=gr) do
      Dec(n);
  end;
begin
  s:= Lines.FText;
  n:= NFromPos;
  if ANext then
  begin
    Next;
    if (n<Length(s)) and (SCharGr(s[n+1])= cgSp) then
      Next;
  end
  else
  begin
    //if we at word middle, jump to word start
    if (n>0) and (SCharGr(s[n])=SCharGr(s[n+1])) then
      Home
    else
    begin
      //jump lefter, then jump to prev word start
      if (n>0) then
        begin Dec(n); Home end;
      if (n>0) and (SCharGr(s[n+1])= cgSp) then
        begin Dec(n); Home end;
    end
  end;
  Result:= n;
end;

function TCustomSyntaxMemo.GetUndoLimit: integer;
begin
  Result := Lines.UndoLimit;
end;

end.

