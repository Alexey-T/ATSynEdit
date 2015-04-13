unit ATButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Buttons;

type
  { TATSimpleButton }

  TATSimpleButton = class(TCustomControl)
  protected
    FPressed,
    FOver,
    FChecked,
    FCheckable: boolean;
    FCaption: string;
    FOnClick: TNotifyEvent;
    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    ColorFont,
    ColorBgPassive,
    ColorBgOver,
    ColorBgChecked,
    ColorBorderPassive,
    ColorBorderOver: TColor;
    constructor Create(AOwner: TComponent); override;
    property Caption: string read FCaption write FCaption;
    property Checked: boolean read FChecked;
    property Checkable: boolean read FCheckable write FCheckable;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses Math, Types;

{ TATSimpleButton }

procedure TATSimpleButton.Paint;
var
  r: TRect;
  p: TPoint;
  bPressed: boolean;
begin
  inherited;

  r:= ClientRect;
  Canvas.Brush.Color:=
    IfThen(FChecked, ColorBgChecked,
      IfThen(FOver, ColorBgOver, ColorBgPassive));
  Canvas.FillRect(r);

  bPressed:= FPressed or FChecked;
  if bPressed then
    InflateRect(r, -1, -1);

  Canvas.Pen.Color:= IfThen(FOver, ColorBorderOver, ColorBorderPassive);
  Canvas.Rectangle(r);

  p.x:= (ClientWidth - Canvas.TextWidth(FCaption)) div 2;
  p.y:= (ClientHeight - Canvas.TextHeight('N')) div 2 + IfThen(bPressed, 1);
  Canvas.Font.Color:= ColorFont;
  Canvas.TextOut(p.x, p.y, FCaption);
end;

procedure TATSimpleButton.MouseEnter;
begin
  inherited;
  FOver:= true;
  Invalidate;
end;

procedure TATSimpleButton.MouseLeave;
begin
  inherited MouseLeave;
  FOver:= false;
  Invalidate;
end;

procedure TATSimpleButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Shift=[ssLeft] then
    FPressed:= true;
  Invalidate;
end;

procedure TATSimpleButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FPressed then
  begin
    if Assigned(FOnClick) then
      FOnClick(Self);
    if FCheckable then
      FChecked:= not FChecked;
  end;

  FPressed:= false;
  Invalidate;
end;

constructor TATSimpleButton.Create(AOwner: TComponent);
begin
  inherited;

  FPressed:= false;
  FOver:= false;
  FChecked:= false;
  FCheckable:= false;

  ColorFont:= $404040;
  ColorBgPassive:= $e0e0e0;
  ColorBgOver:= $e0e0e0;
  ColorBgChecked:= $b0b0b0;
  ColorBorderPassive:= clGray;
  ColorBorderOver:= $d0d0d0;
end;

end.

