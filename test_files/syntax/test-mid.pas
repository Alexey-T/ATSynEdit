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
