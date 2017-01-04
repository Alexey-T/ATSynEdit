{
ATScrollBar for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0 or LGPL

Features:
- fully supports owner-draw of all elements (arrows, backgnd, thumb, corner empty area)
- prop: border size
- prop: arrow mark size
- prop: size of corner empty area (for additional controls maybe)
- prop: kind of arrows (normal, both above, both below, no arrows)

Mouse usage:
- click and holding mouse on arrows
- click and holding mouse on page-up (area above thumb) / page-down (area below thumb)
- dragging of thumb
}

unit ATSynEdit_ScrollBar;

{$mode delphi}

interface

{$ifndef FPC}
{$define windows}
{$endif}

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls, Forms;

type
  TATScrollElemType = (
    aseArrowUp,
    aseArrowDown,
    aseArrowLeft,
    aseArrowRight,
    aseScrollThumbV,
    aseScrollThumbH,
    aseScrollAreaH,
    aseScrollAreaV,
    aseScrolledAreaH,
    aseScrolledAreaV,
    aseCorner
    );

type
  TATScrollArrowsKind = (
    asaArrowsNormal,
    asaArrowsBelow,
    asaArrowsAbove,
    asaArrowsHidden
    );

type
  TATScrollDrawEvent = procedure (Sender: TObject; AType: TATScrollElemType;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

type
  TATScrollbarTheme = record
    ColorBG: TColor;
    ColorBorder: TColor;
    ColorThumbBorder: TColor;
    ColorThumbFill: TColor;
    ColorArrowBorder: TColor;
    ColorArrowFill: TColor;
    ColorArrowSign: TColor;
    ColorScrolled: TColor;
  end;
var
  ATScrollbarTheme: TATScrollbarTheme;

type

  { TATScroll }

  TATScroll = class(TCustomPanel)
  private
    FKind: TScrollBarKind;
    FKindArrows: TATScrollArrowsKind;
    FIndentBorder: Integer;
    FIndentCorner: Integer;
    FIndentArrow: Integer;
    FIndentArrLonger: Integer;
    FTimerDelay: Integer;

    FPos,
    FMin,
    FMax,
    FPage: Integer;

    //internal
    FRectMain: TRect; //area for scrolling
    FRectArrUp: TRect; //area for up or left arrow
    FRectArrDown: TRect; //area for down or right arrow
    FRectThumb: TRect; //area for scroll-thumb
    FRectCorner: TRect;
    FRectPageUp: TRect;
    FRectPageDown: TRect;

    FBitmap: TBitmap;
    FTimer: TTimer;
    FOnChange: TNotifyEvent;
    FOnOwnerDraw: TATScrollDrawEvent;

    //drag-drop
    FMouseDown: boolean;
    FMouseDragOffset: Integer;
    FMouseDownOnUp,
    FMouseDownOnDown,
    FMouseDownOnThumb,
    FMouseDownOnPageUp,
    FMouseDownOnPageDown: boolean;

    procedure DoPaintArrow(C: TCanvas; const R: TRect; Typ: TATScrollElemType);
    procedure DoPaintThumb(C: TCanvas);
    procedure DoPaintBack(C: TCanvas);
    procedure DoPaintBackScrolled(C: TCanvas);
    procedure DoPaintTo(C: TCanvas);

    procedure DoPaintStd_Corner(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Back(C: TCanvas; const R: TRect);
    procedure DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Arrow(C: TCanvas; R: TRect; Typ: TATScrollElemType);
    procedure DoPaintStd_Thumb(C: TCanvas; const R: TRect);

    function IsHorz: boolean;
    function MouseToPos(X, Y: Integer): Integer;
    procedure DoUpdateThumbRect;
    procedure DoUpdateCornerRect;
    procedure DoUpdatePosOnDrag(X, Y: Integer);
    procedure DoScrollBy(NDelta: Integer);
    function GetPxAtScroll(APos: Integer): Integer;

    procedure TimerTimer(Sender: TObject);
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetKindArrows(AValue: TATScrollArrowsKind);
    procedure SetPos(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPage(Value: Integer);
    function DoDrawEvent(AType: TATScrollElemType;
      ACanvas: TCanvas; const ARect: TRect): boolean;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    property Position: Integer read FPos write SetPos;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property PageSize: Integer read FPage write SetPage;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property DoubleBuffered;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property KindArrows: TATScrollArrowsKind read FKindArrows write SetKindArrows default asaArrowsNormal;
    property IndentBorder: Integer read FIndentBorder write FIndentBorder default 1;
    property IndentCorner: Integer read FIndentCorner write FIndentCorner default 0;
    property IndentArrow: Integer read FIndentArrow write FIndentArrow default 3;
    property IndentArrLonger: Integer read FIndentArrLonger write FIndentArrLonger default 0;
    property TimerDelay: Integer read FTimerDelay write FTimerDelay default 80;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnOwnerDraw: TATScrollDrawEvent read FOnOwnerDraw write FOnOwnerDraw;
    property OnContextPopup;
    property OnResize;
  end;

implementation

uses
  SysUtils, Math;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_NO;
  {$else}
  Result:= false;
  {$endif}
end;

{ TATScroll }

constructor TATScroll.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];
  DoubleBuffered:= IsDoubleBufferedNeeded;
  Width:= 200;
  Height:= 20;

  FKind:= sbHorizontal;
  FKindArrows:= asaArrowsNormal;
  FIndentBorder:= 1;
  FIndentCorner:= 0;
  FIndentArrow:= 3;
  FIndentArrLonger:= 0;

  FMin:= 0;
  FMax:= 100;
  FPage:= 20;

  Color:= ATScrollbarTheme.ColorBG;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FTimerDelay:= 80;
  FTimer:= TTimer.Create(Self);
  FTimer.Enabled:= false;
  FTimer.Interval:= FTimerDelay;
  FTimer.OnTimer:= TimerTimer;

  FMouseDown:= false;
  FMouseDragOffset:= 0;
end;

destructor TATScroll.Destroy;
begin
  FTimer.Enabled:= false;
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATScroll.Paint;
begin
  if DoubleBuffered then
  begin
    if Assigned(FBitmap) then
    begin
      DoPaintTo(FBitmap.Canvas);
      Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
    end;
  end
  else
    DoPaintTo(Canvas);
end;

procedure TATScroll.DoPaintTo(C: TCanvas);
var
  FSize: Integer;
begin
  FRectMain:= ClientRect;
  FRectArrUp:= Rect(0, 0, 0, 0);
  FRectArrDown:= Rect(0, 0, 0, 0);

  DoUpdateCornerRect;
  if not IsRectEmpty(FRectCorner) then
    if DoDrawEvent(aseCorner, C, FRectCorner) then
      DoPaintStd_Corner(C, FRectCorner);

  C.Brush.Color:= ATScrollbarTheme.ColorBorder;
  C.FillRect(FRectMain);

  InflateRect(FRectMain, -FIndentBorder, -FIndentBorder);

  if IsHorz then
  begin
    //horz kind
    FSize:= Math.Min(FRectMain.Bottom-FRectMain.Top, (FRectMain.Right-FRectMain.Left) div 2);
    Inc(FSize, FIndentArrLonger);
    case FKindArrows of
      asaArrowsNormal:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Left+FSize, FRectMain.Bottom);
          FRectArrDown:= Rect(FRectMain.Right-FSize, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
          Inc(FRectMain.Left, FSize);
          Dec(FRectMain.Right, FSize);
        end;
      asaArrowsBelow:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Left+FSize, FRectMain.Bottom);
          FRectArrDown:= Rect(FRectMain.Left+FSize, FRectMain.Top, FRectMain.Left+2*FSize, FRectMain.Bottom);
          Inc(FRectMain.Left, 2*FSize);
        end;
      asaArrowsAbove:
        begin
          FRectArrDown:= Rect(FRectMain.Right-FSize, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
          FRectArrUp:= Rect(FRectMain.Right-2*FSize, FRectMain.Top, FRectMain.Right-FSize, FRectMain.Bottom);
          Dec(FRectMain.Right, 2*FSize);
        end;
    end;
    DoPaintArrow(C, FRectArrUp, aseArrowLeft);
    DoPaintArrow(C, FRectArrDown, aseArrowRight);
  end
  else
  begin
    //vertical kind
    FSize:= Math.Min(FRectMain.Right-FRectMain.Left, (FRectMain.Bottom-FRectMain.Top) div 2);
    Inc(FSize, FIndentArrLonger);
    case FKindArrows of
      asaArrowsNormal:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectMain.Top+FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Bottom-FSize, FRectMain.Right, FRectMain.Bottom);
          Inc(FRectMain.Top, FSize);
          Dec(FRectMain.Bottom, FSize);
        end;
      asaArrowsBelow:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Bottom-2*FSize, FRectMain.Right, FRectMain.Bottom-FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Bottom-FSize, FRectMain.Right, FRectMain.Bottom);
          Dec(FRectMain.Bottom, 2*FSize);
        end;
      asaArrowsAbove:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectMain.Top+FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Top+FSize, FRectMain.Right, FRectMain.Top+2*FSize);
          Inc(FRectMain.Top, 2*FSize);
        end;
    end;
    DoPaintArrow(C, FRectArrUp, aseArrowUp);
    DoPaintArrow(C, FRectArrDown, aseArrowDown);
  end;

  DoPaintBack(C);
  DoUpdateThumbRect;
  DoPaintBackScrolled(C);
  DoPaintThumb(C);
end;

procedure TATScroll.DoPaintBack(C: TCanvas);
var
  Typ: TATScrollElemType;
begin
  if IsHorz then Typ:= aseScrollAreaH else Typ:= aseScrollAreaV;
  if DoDrawEvent(Typ, C, FRectMain) then
    DoPaintStd_Back(C, FRectMain);
end;

procedure TATScroll.DoPaintBackScrolled(C: TCanvas);
var
  Typ: TATScrollElemType;
begin
  if IsHorz then Typ:= aseScrolledAreaH else Typ:= aseScrolledAreaV;

  if FMouseDown and FMouseDownOnPageUp then
    if DoDrawEvent(Typ, C, FRectPageUp) then
      DoPaintStd_BackScrolled(C, FRectPageUp);

  if FMouseDown and FMouseDownOnPageDown then
    if DoDrawEvent(Typ, C, FRectPageDown) then
      DoPaintStd_BackScrolled(C, FRectPageDown);
end;


procedure TATScroll.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseDown:= Button=mbLeft;
  FMouseDownOnThumb:= PtInRect(FRectThumb, Point(X, Y));
  FMouseDownOnUp:= PtInRect(FRectArrUp, Point(X, Y));
  FMouseDownOnDown:= PtInRect(FRectArrDown, Point(X, Y));
  FMouseDownOnPageUp:= PtInRect(FRectPageUp, Point(X, Y));
  FMouseDownOnPageDown:= PtInRect(FRectPageDown, Point(X, Y));

  if IsHorz then
    FMouseDragOffset:= X-FRectThumb.Left
  else
    FMouseDragOffset:= Y-FRectThumb.Top;

  FTimer.Enabled:= FMouseDown and
    (FMouseDownOnUp or
     FMouseDownOnDown or
     FMouseDownOnPageUp or
     FMouseDownOnPageDown);
end;

procedure TATScroll.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseDown:= false;
  FMouseDownOnThumb:= false;
  FTimer.Enabled:= false;
  Invalidate;
end;

procedure TATScroll.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    //little complicated to speed up
    if IsHorz then
    begin
      FBitmap.Width:= Math.Max(FBitmap.Width, Width);
      FBitmap.Height:= Height;
    end
    else
    begin
      FBitmap.Width:= Width;
      FBitmap.Height:= Math.Max(FBitmap.Height, Height);
    end;
  end;
end;


{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATScroll.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATScroll.Click;
begin
  inherited;
end;

function TATScroll.DoDrawEvent(AType: TATScrollElemType;
  ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnOwnerDraw) then
    FOnOwnerDraw(Self, AType, ACanvas, ARect, Result);
end;

procedure TATScroll.SetKind(AValue: TScrollBarKind);
begin
  if AValue=FKind then Exit;
  FKind:= AValue;
  Invalidate;
end;

procedure TATScroll.SetKindArrows(AValue: TATScrollArrowsKind);
begin
  if FKindArrows=AValue then Exit;
  FKindArrows:= AValue;
  Invalidate;
end;

procedure TATScroll.DoPaintArrow(C: TCanvas; const R: TRect;
  Typ: TATScrollElemType);
begin
  if IsRectEmpty(R) then exit;
  if DoDrawEvent(Typ, C, R) then
    DoPaintStd_Arrow(C, R, Typ);
end;    

procedure TATScroll.DoPaintStd_Arrow(C: TCanvas; R: TRect;
  Typ: TATScrollElemType);
var
  P, P1, P2, P3: TPoint;
  cc: Integer;
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ATScrollbarTheme.ColorArrowBorder;
  C.FillRect(R);

  InflateRect(R, -1, -1);
  C.Brush.Color:= ATScrollbarTheme.ColorArrowFill;
  C.FillRect(R);

  P:= CenterPoint(R);
  cc:= FIndentArrow;

  case Typ of
    aseArrowUp:
      begin
        P1:= Point(P.X-cc, P.Y+cc div 2);
        P2:= Point(P.X+cc, P.Y+cc div 2);
        P3:= Point(P.X, P.Y-cc+cc div 2);
      end;
    aseArrowDown:
      begin
        P1:= Point(P.X-cc, P.Y-cc div 2);
        P2:= Point(P.X+cc, P.Y-cc div 2);
        P3:= Point(P.X, P.Y+cc-cc div 2);
      end;
    aseArrowLeft:
      begin
        P1:= Point(P.X+cc div 2, P.Y-cc);
        P2:= Point(P.X+cc div 2, P.Y+cc);
        P3:= Point(P.X-cc+cc div 2, P.Y);
      end;
    aseArrowRight:
      begin
        P1:= Point(P.X-cc div 2    -1, P.Y-cc);
        P2:= Point(P.X-cc div 2    -1, P.Y+cc);
        P3:= Point(P.X+cc-cc div 2 -1, P.Y);
      end;
    else
      Exit;
 end;     

  C.Brush.Color:= ATScrollbarTheme.ColorArrowSign;
  C.Pen.Color:= ATScrollbarTheme.ColorArrowSign;
  C.Polygon([P1, P2, P3]);
end;

function TATScroll.IsHorz: boolean;
begin
  Result:= FKind=sbHorizontal;
end;

function TATScroll.GetPxAtScroll(APos: Integer): Integer;
var
  N0, NLen: Integer;
begin
  if IsHorz then
  begin
    N0:= FRectMain.Left;
    NLen:= FRectMain.Right-FRectMain.Left
  end
  else
  begin
    N0:= FRectMain.Top;
    NLen:= FRectMain.Bottom-FRectMain.Top;
  end;
  Result:= N0 + (APos-FMin) * NLen div Math.Max(1, FMax-FMin);
end;

procedure TATScroll.DoUpdateThumbRect;
const
  cMinView = 10;
var
  R: TRect;
begin
  FRectThumb:= Rect(0, 0, 0, 0);
  FRectPageUp:= Rect(0, 0, 0, 0);
  FRectPageDown:= Rect(0, 0, 0, 0);

  if IsHorz then
  begin
    if FRectMain.Right-FRectMain.Left<cMinView then Exit;
    R.Top:= FRectMain.Top;
    R.Bottom:= FRectMain.Bottom;
    R.Left:= GetPxAtScroll(FPos);
    R.Left:= Math.Min(R.Left, FRectMain.Right-cMinView);
    R.Right:= GetPxAtScroll(FPos+FPage);
    R.Right:= Math.Max(R.Right, R.Left+cMinView);
    R.Right:= Math.Min(R.Right, FRectMain.Right);
  end
  else
  begin
    if FRectMain.Bottom-FRectMain.Top<cMinView then Exit;
    R.Left:= FRectMain.Left;
    R.Right:= FRectMain.Right;
    R.Top:= GetPxAtScroll(FPos);
    R.Top:= Math.Min(R.Top, FRectMain.Bottom-cMinView);
    R.Bottom:= GetPxAtScroll(FPos+FPage);
    R.Bottom:= Math.Max(R.Bottom, R.Top+cMinView);
    R.Bottom:= Math.Min(R.Bottom, FRectMain.Bottom);
  end;
  FRectThumb:= R;

  if IsHorz then
  begin
    FRectPageUp:= Rect(FRectMain.Left, FRectMain.Top, FRectThumb.Left, FRectMain.Bottom);
    FRectPageDown:= Rect(FRectThumb.Right, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
  end
  else
  begin
    FRectPageUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectThumb.Top);
    FRectPageDown:= Rect(FRectMain.Left, FRectThumb.Bottom, FRectMain.Right, FRectMain.Bottom);
  end;
end;

procedure TATScroll.DoPaintThumb(C: TCanvas);
var
  Typ: TATScrollElemType;
begin
  if IsRectEmpty(FRectThumb) then Exit;
  if IsHorz then
    Typ:= aseScrollThumbH
  else
    Typ:= aseScrollThumbV;

  if DoDrawEvent(Typ, C, FRectThumb) then
    DoPaintStd_Thumb(C, FRectThumb);
end;

procedure TATScroll.DoPaintStd_Thumb(C: TCanvas; const R: TRect);
const
  cMinMark = 20; //minimial size of thumb, after which thumb disappears
  cMarkOf = 4; //offset from thumb edge to "|||" lines
var
  P: TPoint;
begin
  C.Brush.Color:= ATScrollbarTheme.ColorThumbFill;
  C.Pen.Color:= ATScrollbarTheme.ColorThumbBorder;
  C.Rectangle(R);

  P:= CenterPoint(R);
  if IsHorz then
  begin
    if (R.Right-R.Left)>cMinMark then
    begin
      C.MoveTo(P.X  , R.Top+cMarkOf);
      C.LineTo(P.X  , R.Bottom-cMarkOf);
      C.MoveTo(P.X-2, R.Top+cMarkOf);
      C.LineTo(P.X-2, R.Bottom-cMarkOf);
      C.MoveTo(P.X+2, R.Top+cMarkOf);
      C.LineTo(P.X+2, R.Bottom-cMarkOf);
    end;
  end
  else
  begin
    if (R.Bottom-R.Top)>cMinMark then
    begin
      C.MoveTo(R.Left+cMarkOf, P.Y);
      C.LineTo(R.Right-cMarkOf, P.Y);
      C.MoveTo(R.Left+cMarkOf, P.Y-2);
      C.LineTo(R.Right-cMarkOf, P.Y-2);
      C.MoveTo(R.Left+cMarkOf, P.Y+2);
      C.LineTo(R.Right-cMarkOf, P.Y+2);
    end;
  end;
end;


procedure TATScroll.SetMax(Value: Integer);
begin
  if FMax<>Value then
  begin
    FMax:= Value;
    FPos:= Math.Min(FPos, FMax);
    Invalidate;
  end;
end;

procedure TATScroll.SetMin(Value: Integer);
begin
  if FMin<>Value then
  begin
    FMin:= Value;
    FPos:= Math.Max(FPos, FMin);
    Invalidate;
  end;
end;

procedure TATScroll.SetPage(Value: Integer);
begin
  if FPage<>Value then
  begin
    FPage:= Value;
    Invalidate;
  end;
end;

procedure TATScroll.SetPos(Value: Integer);
begin
  Value:= Math.Min(Value, FMax);
  Value:= Math.Max(Value, FMin);
  if FPos<>Value then
  begin
    FPos:= Value;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TATScroll.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDownOnThumb then
  begin
    DoUpdatePosOnDrag(X, Y);
    Exit
  end;
end;

function TATScroll.MouseToPos(X, Y: Integer): Integer;
begin
  if IsHorz then
    Result:= FMin + (X-FRectMain.Left) * (FMax-FMin) div Math.Max(FRectMain.Right-FRectMain.Left, 1)
  else
    Result:= FMin + (Y-FRectMain.Top) * (FMax-FMin) div Math.Max(FRectMain.Bottom-FRectMain.Top, 1);
end;

procedure TATScroll.DoUpdatePosOnDrag(X, Y: Integer);
var
  N: Integer;
begin
  N:= MouseToPos(
    X-FMouseDragOffset,
    Y-FMouseDragOffset);
  N:= Math.Max(N, FMin);
  N:= Math.Min(N, FMax-FPage);
  SetPos(N);
end;

procedure TATScroll.DoScrollBy(NDelta: Integer);
var
  N: Integer;
begin
  N:= FPos;
  Inc(N, NDelta);
  if (NDelta>0) then
    N:= Math.Min(N, FMax-FPage);
  SetPos(N);
end;

procedure TATScroll.TimerTimer(Sender: TObject);
var
  P: TPoint;
begin
  P:= Mouse.CursorPos;
  P:= ScreenToClient(P);

  if FMouseDownOnDown and PtInRect(FRectArrDown, P) then
    DoScrollBy(1)
  else
  if FMouseDownOnUp and PtInRect(FRectArrUp, P) then
    DoScrollBy(-1)
  else
  if FMouseDownOnPageDown and PtInRect(FRectPageDown, P) then
    DoScrollBy(FPage)  
  else
  if FMouseDownOnPageUp and PtInRect(FRectPageUp, P) then
    DoScrollBy(-FPage);
end;

procedure TATScroll.DoPaintStd_Corner(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ATScrollbarTheme.ColorBG;
  C.FillRect(R);
end;

procedure TATScroll.DoPaintStd_Back(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ATScrollbarTheme.ColorBG;
  C.FillRect(R);
end;

procedure TATScroll.DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ATScrollbarTheme.ColorScrolled;
  C.FillRect(R);
end;

procedure TATScroll.DoUpdateCornerRect;
begin
  FRectCorner:= Rect(0, 0, 0, 0);
  if IsHorz then
  begin
    if FIndentCorner>0 then
    begin
      FRectCorner:= Rect(ClientWidth-FIndentCorner, 0, ClientWidth, ClientHeight);
      Dec(FRectMain.Right, FIndentCorner);
    end
    else
    if FIndentCorner<0 then
    begin
      FRectCorner:= Rect(0, 0, Abs(FIndentCorner), ClientHeight);
      Inc(FRectMain.Left, Abs(FIndentCorner));
    end;
  end
  else
  begin
    if FIndentCorner>0 then
    begin
      FRectCorner:= Rect(0, ClientHeight-FIndentCorner, ClientWidth, ClientHeight);
      Dec(FRectMain.Bottom, FIndentCorner);
    end
    else
    if FIndentCorner<0 then
    begin
      FRectCorner:= Rect(0, 0, ClientWidth, Abs(FIndentCorner));
      Inc(FRectMain.Top, Abs(FIndentCorner));
    end;
  end;
end;

initialization
  ATScrollbarTheme.ColorBG:= $d0d0d0;
  ATScrollbarTheme.ColorBorder:= clLtGray;
  ATScrollbarTheme.ColorThumbBorder:= $808080;
  ATScrollbarTheme.ColorThumbFill:= $c0c0c0;
  ATScrollbarTheme.ColorArrowBorder:= $808080;
  ATScrollbarTheme.ColorArrowFill:= $c0c0c0;
  ATScrollbarTheme.ColorArrowSign:= $404040;
  ATScrollbarTheme.ColorScrolled:= $d0b0b0;

end.
