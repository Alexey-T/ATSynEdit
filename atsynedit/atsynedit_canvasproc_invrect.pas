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
  {$ifdef LCLGtk2}
  Gdk2,
  Gtk2Def,
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

  cr := pcairo_t(TGtk3DeviceContext(C.Handle).pcr);
  if cr = nil then Exit;

  rgb := AColor; //ColorToRGB(AColor);
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

  cr := Pcairo_t(TGtk3DeviceContext(C.Handle).pcr);
  if cr = nil then Exit;

  rgb := AColor; //ColorToRGB(AColor);
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
{$endif}

{$ifdef LCLGtk2}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  DC: TGtkDeviceContext;
  gc: PGdkGC;
  drawable: PGdkDrawable;
  col: TGdkColor;
  colormap: PGdkColormap;
  W, H: Integer;
begin
  if not Assigned(C) or (C.Handle = 0) then Exit;

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  if (W <= 0) or (H <= 0) then Exit;

  DC := TGtkDeviceContext(C.Handle);
  drawable := DC.Drawable;
  gc := DC.GC;
  if (drawable = nil) or (gc = nil) then Exit;

  // Use WHITE for XOR to get pure inversion: result = ~background
  col.red   := $FFFF;
  col.green := $FFFF;
  col.blue  := $FFFF;

  // Allocate color through colormap for correct pixel value on any visual
  colormap := gdk_drawable_get_colormap(drawable);
  if colormap <> nil then
    gdk_colormap_alloc_color(colormap, @col, False, True);

  gdk_gc_set_function(gc, GDK_XOR);
  gdk_gc_set_foreground(gc, @col);

  gdk_draw_rectangle(drawable, gc, 1, R.Left, R.Top, W, H);

  gdk_gc_set_function(gc, GDK_COPY);
end;

procedure CanvasInvertRectEmptyInside(C: TCanvas; const R: TRect; AColor: TColor);
var
  DC: TGtkDeviceContext;
  gc: PGdkGC;
  drawable: PGdkDrawable;
  col: TGdkColor;
  colormap: PGdkColormap;
  W, H: Integer;
begin
  if not Assigned(C) or (C.Handle = 0) then Exit;

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  if (W <= 0) or (H <= 0) then Exit;

  DC := TGtkDeviceContext(C.Handle);
  drawable := DC.Drawable;
  gc := DC.GC;
  if (drawable = nil) or (gc = nil) then Exit;

  // Use WHITE for XOR to get pure inversion: result = ~background
  col.red   := $FFFF;
  col.green := $FFFF;
  col.blue  := $FFFF;

  // Allocate color through colormap for correct pixel value on any visual
  colormap := gdk_drawable_get_colormap(drawable);
  if colormap <> nil then
    gdk_colormap_alloc_color(colormap, @col, False, True);

  gdk_gc_set_function(gc, GDK_XOR);
  gdk_gc_set_foreground(gc, @col);

  // filled = 0 → only the outline, 1-pixel wide by default
  // Use W-1, H-1 so the stroke lands exactly within R (GDK's outline
  // rectangle includes both endpoints, unlike the filled variant)
  gdk_draw_rectangle(drawable, gc, 0, R.Left, R.Top, W - 1, H - 1);

  gdk_gc_set_function(gc, GDK_COPY);
end;
{$endif}

{$if not defined(LCLGtk2) and not defined(LCLGtk3)}
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
{$endif}

{$if not defined(LCLGtk2) and not defined(LCLGtk3)}
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
  {
  if ATCanvasPrimitives_InvertByPixels then
  begin
    CanvasInvertRect_ByPixels(C, Rect(R.Left, R.Top, R.Right, R.Top+1), AColor);
    CanvasInvertRect_ByPixels(C, Rect(R.Left, R.Top+1, R.Left+1, R.Bottom-1), AColor);
    CanvasInvertRect_ByPixels(C, Rect(R.Right-1, R.Top+1, R.Right, R.Bottom-1), AColor);
    CanvasInvertRect_ByPixels(C, Rect(R.Left, R.Bottom-1, R.Right, R.Bottom), AColor);
    exit;
  end;
  }

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
