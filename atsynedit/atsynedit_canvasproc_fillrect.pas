{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CanvasProc_FillRect;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Types;  
  
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);

implementation

uses
  {$ifdef LCLWin32}
  Windows,
  {$endif}
  {$ifdef LCLGtk2}
  Gdk2,
  Gtk2Def,
  {$endif}
  {$ifdef LCLGtk3}
  Gtk3Objects,
  Cairo,
  {$endif}
  LCLType;

{$ifdef LCLGtk2}
{$define HAS_F}
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  DC: TGtkDeviceContext;
  drawable: PGdkDrawable;
  temp_gc: PGdkGC;
  col: TGdkColor;
  W, H: Integer;
begin
  if (C.Handle = 0) then Exit;

  W := R.Width;
  H := R.Height;
  if (W <= 0) or (H <= 0) then Exit;

  DC := TGtkDeviceContext(C.Handle);
  drawable := DC.Drawable;
  if drawable = nil then Exit;
  temp_gc := DC.GC;
  if temp_gc = nil then Exit;

  // GDK2 uses 16-bit color components
  // multiply by $101 (257) - to scale 0..255 -> 0..65535
  col.red   := (AColor and $FF) * $101;
  col.green := ((AColor shr 8) and $FF) * $101;
  col.blue  := ((AColor shr 16) and $FF) * $101;

  gdk_gc_set_rgb_fg_color(temp_gc, @col);

  gdk_draw_rectangle(drawable, temp_gc, 1, R.Left, R.Top, W, H);
end;
{$endif}

{$ifdef LCLGtk3}
{$define HAS_F}
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  Cr: Pcairo_t;
begin
  if (C.Handle = 0) then Exit;

  cr := pcairo_t(TGtk3DeviceContext(C.Handle).pcr);
  if cr = nil then Exit;

  cairo_set_source_rgb(Cr,
    (AColor and $FF) / 255.0,
    ((AColor shr 8) and $FF) / 255.0,
    ((AColor shr 16) and $FF) / 255.0);

  cairo_rectangle(Cr, R.Left, R.Top, R.Width, R.Height);
  cairo_fill(Cr);
end;
{$endif}

{$ifdef LCLWin32}
{$define HAS_F}
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  DC: HDC;
  OldBrush: HGDIOBJ;
  OldBrushColor: COLORREF;
  W, H: Integer;
begin
  if (C.Handle = 0) then Exit;

  W := R.Width;
  H := R.Height;
  if (W <= 0) or (H <= 0) then Exit;

  DC := HDC(C.Handle);

  // using stock brush DC_BRUSH is faster than do everytime CreateSolidBrush/DeleteObject.
  OldBrush := SelectObject(DC, GetStockObject(DC_BRUSH));
  if OldBrush = 0 then Exit;

  try
    OldBrushColor := SetDCBrushColor(DC, AColor);

    PatBlt(DC, R.Left, R.Top, W, H, PATCOPY);

    SetDCBrushColor(DC, OldBrushColor);
  finally
    SelectObject(DC, OldBrush);
  end;
end;
{$endif}

{$ifndef HAS_F}
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);
begin
  C.Brush.Style:= bsSolid;
  C.Brush.Color:= AColor;
  C.FillRect(R);
end;
{$endif}

end.
