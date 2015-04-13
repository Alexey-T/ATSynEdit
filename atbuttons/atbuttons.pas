{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0
}

unit ATButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type
  TATButtonTheme = record
    FontName: string;
    FontSize: integer;
    FontStyles: TFontStyles;
    ColorFont,
    ColorBgPassive,
    ColorBgOver,
    ColorBgChecked,
    ColorBorderPassive,
    ColorBorderOver: TColor;
    MouseoverBorderWidth: integer;
    PressedBorderWidth: integer;
    PressedCaptionShiftY: integer;
    PressedCaptionShiftX: integer;
  end;

var
  ATButtonTheme: TATButtonTheme;

type
  { TATSimpleButton }

  TATSimpleButton = class(TCustomControl)
  private
    FPressed,
    FOver,
    FChecked,
    FCheckable: boolean;
    FCaption: string;
    FOnClick: TNotifyEvent;
    procedure SetCaption(AValue: string);
    procedure SetChecked(AValue: boolean);
  protected
    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Checked: boolean read FChecked write SetChecked;
    property Checkable: boolean read FCheckable write FCheckable;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses Math, Types;

{ TATSimpleButton }

procedure TATSimpleButton.SetChecked(AValue: boolean);
begin
  if FChecked= AValue then Exit;
  FChecked:= AValue;
  Invalidate;
end;

procedure TATSimpleButton.SetCaption(AValue: string);
begin
  if FCaption= AValue then Exit;
  FCaption:= AValue;
  Invalidate;
end;

procedure TATSimpleButton.Paint;
var
  r: TRect;
  p: TPoint;
  size, i: integer;
begin
  inherited;

  //----draw bg
  r:= ClientRect;
  Canvas.Brush.Color:=
    IfThen(FChecked, ATButtonTheme.ColorBgChecked,
      IfThen(FOver, ATButtonTheme.ColorBgOver, ATButtonTheme.ColorBgPassive));
  Canvas.FillRect(r);

  //----draw border
  Canvas.Brush.Style:= bsClear;

  Canvas.Pen.Color:= IfThen(FOver, ATButtonTheme.ColorBorderOver, ATButtonTheme.ColorBorderPassive);
  Canvas.Rectangle(r);

  size:= 1;
  if FPressed then size:= ATButtonTheme.PressedBorderWidth else
  if FOver then size:= ATButtonTheme.MouseoverBorderWidth;

  for i:= 1 to size-1 do
  begin
    InflateRect(r, -1, -1);
    Canvas.Rectangle(r);
  end;

  Canvas.Brush.Style:= bsSolid;

  //----draw caption
  Canvas.Font.Name:= ATButtonTheme.FontName;
  Canvas.Font.Color:= ATButtonTheme.ColorFont;
  Canvas.Font.Size:= ATButtonTheme.FontSize;
  Canvas.Font.Style:= ATButtonTheme.FontStyles;

  p.x:= (ClientWidth - Canvas.TextWidth(FCaption)) div 2 +
    IfThen(FPressed, ATButtonTheme.PressedCaptionShiftX);
  p.y:= (ClientHeight - Canvas.TextHeight(FCaption)) div 2 +
    IfThen(FPressed, ATButtonTheme.PressedCaptionShiftY);
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

  ControlStyle:= ControlStyle
    +[csOpaque, csNoFocus]
    -[csDoubleClicks, csTripleClicks];

  Width:= 100;
  Height:= 25;

  FCaption:= 'Btn';
  FPressed:= false;
  FOver:= false;
  FChecked:= false;
  FCheckable:= false;
  FOnClick:= nil;
end;

initialization

  with ATButtonTheme do
  begin
    FontName:= 'default';
    FontSize:= 10;
    FontStyles:= [];
    ColorFont:= $303030;
    ColorBgPassive:= $e0e0e0;
    ColorBgOver:= $e0e0e0;
    ColorBgChecked:= $b0b0b0;
    ColorBorderPassive:= $a0a0a0;
    ColorBorderOver:= $d0d0d0;
    MouseoverBorderWidth:= 1;
    PressedBorderWidth:= 2;
    PressedCaptionShiftX:= 0;
    PressedCaptionShiftY:= 1;
  end;

end.

