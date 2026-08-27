{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CanvasProc_InvRect;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Types;
  
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
procedure CanvasInvertRectEmptyInside(C: TCanvas; const R: TRect; AColor: TColor);

implementation

uses
  {$ifdef LCLGtk3}
  Gtk3Objects,
  Cairo,
  {$endif}
  Classes;

procedure CanvasInvertRect_ByPixels(C: TCanvas; const R: TRect; AColor: TColor);
var
  NValue: Longint;
  i, j: integer;
begin
  NValue:= not AColor and $ffffff;
  for j:= R.Top to R.Bottom-1 do
    for i:= R.Left to R.Right-1 do
      C.Pixels[i, j]:= C.Pixels[i, j] xor NValue;
end;

{$ifdef LCLGtk3}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  cr: Pcairo_t;
  rgb: LongInt;
  cRed, cGreen, cBlue: Double;
begin
  if not Assigned(C) or (C.Handle = 0) then Exit;

  cr := Cairo.pcairo_t(TGtk3DeviceContext(C.Handle).pcr);
  if cr = nil then Exit;

  rgb := ColorToRGB(AColor);
  cRed   := (rgb and $FF) / 255;
  cGreen := ((rgb shr 8) and $FF) / 255;
  cBlue  := ((rgb shr 16) and $FF) / 255;

  cairo_save(cr);
  cairo_set_operator(cr, CAIRO_OPERATOR_DIFFERENCE);
  cairo_set_source_rgb(cr, cRed, cGreen, cBlue);
  cairo_rectangle(cr, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  cairo_fill(cr);
  cairo_restore(cr);
end;

procedure CanvasInvertRectEmptyInside(C: TCanvas; const R: TRect; AColor: TColor);
var
  cr: Pcairo_t;
  rgb: LongInt;
  cRed, cGreen, cBlue: Double;
begin
  if not Assigned(C) or (C.Handle = 0) then Exit;

  cr := Cairo.Pcairo_t(TGtk3DeviceContext(C.Handle).pcr);
  if cr = nil then Exit;

  rgb := ColorToRGB(AColor);
  cRed   := (rgb and $FF) / 255;
  cGreen := ((rgb shr 8) and $FF) / 255;
  cBlue  := ((rgb shr 16) and $FF) / 255;

  cairo_save(cr);
  cairo_set_operator(cr, CAIRO_OPERATOR_DIFFERENCE);
  cairo_set_source_rgb(cr, cRed, cGreen, cBlue);
  // offset 0.5 aligns line to grid
  cairo_set_line_width(cr, 1.0);
  cairo_rectangle(cr, R.Left + 0.5, R.Top + 0.5,
                  R.Right - R.Left - 1, R.Bottom - R.Top - 1);
  cairo_stroke(cr);
  cairo_restore(cr);
end;
{$else}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  X: integer;
  OldAntialias: TAntialiasingMode;
  OldMode: TPenMode;
  OldStyle: TPenStyle;
  OldWidth: integer;
  {$ifdef FPC}
  OldEndCap: TPenEndCap;
  {$endif}
begin
  if ATCanvasPrimitives_InvertByPixels then
  begin
    CanvasInvertRect_ByPixels(C, R, AColor);
    exit;
  end;

  OldAntialias:= C.AntialiasingMode;
  OldMode:= C.Pen.Mode;
  OldStyle:= C.Pen.Style;
  OldWidth:= C.Pen.Width;

  X:= (R.Left+R.Right) div 2;
  C.Pen.Mode:= {$if defined(LCLCocoa)} pmNot {$else} pmXor {$endif};
  C.Pen.Style:= psSolid;
  C.Pen.Color:= AColor;
  C.AntialiasingMode:= amOff;

  {$ifdef FPC}
  OldEndCap:= C.Pen.EndCap;
  C.Pen.EndCap:= pecFlat;
  {$endif}

  C.Pen.Width:= R.Width;

  C.MoveTo(X, R.Top);
  C.LineTo(X, R.Bottom);

  {$ifdef FPC}
  C.Pen.EndCap:= OldEndCap;
  {$endif}
  C.Pen.Width:= OldWidth;
  C.Pen.Style:= OldStyle;
  C.Pen.Mode:= OldMode;
  C.AntialiasingMode:= OldAntialias;
  C.Rectangle(0, 0, 0, 0); //apply pen
end;

procedure CanvasInvertRectEmptyInside(C: TCanvas; const R: TRect; AColor: TColor);
var
  {$ifdef FPC}
  OldAntialias: TAntialiasingMode;
  {$endif}
  OldPenMode: TPenMode;
  OldPenStyle: TPenStyle;
  OldPenWidth: integer;
  OldBrushStyle: TBrushStyle;
begin
  if ATCanvasPrimitives_InvertByPixels then
  begin
    CanvasInvertRect_ByPixels(C, Rect(R.Left, R.Top, R.Right, R.Top+1), AColor);
    CanvasInvertRect_ByPixels(C, Rect(R.Left, R.Top+1, R.Left+1, R.Bottom-1), AColor);
    CanvasInvertRect_ByPixels(C, Rect(R.Right-1, R.Top+1, R.Right, R.Bottom-1), AColor);
    CanvasInvertRect_ByPixels(C, Rect(R.Left, R.Bottom-1, R.Right, R.Bottom), AColor);
    exit;
  end;

  {$ifdef FPC}
  OldAntialias:= C.AntialiasingMode;
  {$endif}
  OldPenMode:= C.Pen.Mode;
  OldPenStyle:= C.Pen.Style;
  OldPenWidth:= C.Pen.Width;
  OldBrushStyle:= C.Brush.Style;

  C.Pen.Mode:= {$ifdef darwin} pmNot {$else} pmXor {$endif};
  C.Pen.Style:= psSolid;
  C.Pen.Color:= AColor;
  {$ifdef FPC}
  C.AntialiasingMode:= amOff;
  {$endif}
  C.Pen.Width:= 1;
  C.Brush.Style:= bsClear;

  C.Rectangle(R);

  C.Brush.Style:= OldBrushStyle;
  C.Pen.Width:= OldPenWidth;
  C.Pen.Style:= OldPenStyle;
  C.Pen.Mode:= OldPenMode;
  {$ifdef FPC}
  C.AntialiasingMode:= OldAntialias;
  {$endif}
  C.Rectangle(0, 0, 0, 0); //apply pen
end;
{$endif}


end.
