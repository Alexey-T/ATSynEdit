////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.Cairo.pas
//  * Purpose   : Speeding up text output with the Cairo library
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2025.
//  * Version   : 2.0.14
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/FWHexView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/FWHexView
//  ****************************************************************************
//

{
License: MPL 2.0 or LGPL
}

unit ATSynEdit_CanvasProc_Cairo;

{$mode Delphi}{$H+}

interface

uses
  LCLType,
  LCLIntf,
  Types,
  SysUtils,
  Graphics;

  procedure CairoTextOut(ACanvas: TCanvas; AX, AY: Integer; AStr: PChar);
  function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
    ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;

implementation

uses
  gdk2,
  Gtk2Def,
  Cairo,
  Math;

type
  ECairoException = class(Exception);

  TCairoColor = record
    R, G, B, A: Double;
  end;

  PCairoClusterArray = ^TCairoClusterArray;
  TCairoClusterArray = array[0..0] of cairo_text_cluster_t;

function cairo_create_context(DC: HDC): pcairo_t;
var
  Ctx: TGtkDeviceContext;
  Matrix: cairo_matrix_t;
begin
  Ctx := TGtkDeviceContext(DC);
  Result := gdk_cairo_create(Ctx.Drawable);
  if Result = nil then
    raise ECairoException.Create('Cannot create cairo context');
  if Ctx.WindowExt <> Ctx.ViewPortExt then
  begin
    Matrix.xx := Ctx.ViewPortExt.X / Ctx.WindowExt.X;
    Matrix.yy := Ctx.ViewPortExt.Y / Ctx.WindowExt.Y;
    Matrix.x0 := Ctx.ViewPortOrg.X;
    Matrix.y0 := Ctx.ViewPortOrg.Y;
    Matrix.yx := 0;
    Matrix.xy := 0;
    cairo_set_matrix(Result, @Matrix);
  end;
end;

procedure cairo_set_font(ACairo: pcairo_t; AFont: TFont);
var
  ADefFont: TFontData;
  LSlant: cairo_font_slant_t;
  LWeight: cairo_font_weight_t;
begin
  if fsItalic in AFont.Style then
    LSlant := CAIRO_FONT_SLANT_ITALIC
  else
    LSlant := CAIRO_FONT_SLANT_NORMAL;

  if fsBold in AFont.Style then
    LWeight := CAIRO_FONT_WEIGHT_BOLD
  else
    LWeight := CAIRO_FONT_WEIGHT_NORMAL;

  ADefFont := GetFontData(GetStockObject(DEFAULT_GUI_FONT));

  if AFont.IsDefault then
    cairo_select_font_face(ACairo, PChar(string(ADefFont.Name)), LSlant, LWeight)
  else
    cairo_select_font_face(ACairo, PChar(AFont.Name), LSlant, LWeight);

  if AFont.Height = 0 then
    cairo_set_font_size(ACairo, ADefFont.Height)
  else
    cairo_set_font_size(ACairo, Abs(AFont.Height));
end;

function cairo_get_color(AColor: TColor): TCairoColor;
begin
  AColor := ColorToRGB(AColor);
  Result.R := GetRValue(AColor) / 255;
  Result.G := GetGValue(AColor) / 255;
  Result.B := GetBValue(AColor) / 255;
  Result.A := 1.0;
end;

procedure cairo_set_source_color(ACairo: pcairo_t; const AColor: TCairoColor);
begin
  cairo_set_source_rgba(ACairo, AColor.R, AColor.G, AColor.B, AColor.A);
end;

function cairo_font_baseline(AFont: Pcairo_scaled_font_t): Integer;
var
  extents: cairo_font_extents_t;
begin
  cairo_scaled_font_extents(AFont, @extents);
  Result := Ceil(extents.height - extents.descent);
end;

procedure CairoTextOut(ACanvas: TCanvas; AX, AY: Integer; AStr: PChar);
var
  ct: pcairo_t;
  sfont: Pcairo_scaled_font_t;
  x, y: Integer;
begin
  ct := cairo_create_context(ACanvas.Handle);
  try
    cairo_set_font(ct, ACanvas.Font);
    cairo_set_source_color(ct, cairo_get_color(ACanvas.Font.Color));
    sfont := cairo_get_scaled_font(ct);
    x := AX;
    y := AY + cairo_font_baseline(sfont);
    {
    if Flags and DT_NOCLIP = 0 then
    begin
      cairo_rectangle(ct, ARect.Left, ARect.Top, ARect.Width + 1, ARect.Height);
      cairo_clip(ct);
    end;
    }
    cairo_move_to(ct, x, y);
    cairo_show_text(ct, AStr);
  finally
    cairo_destroy(ct);
  end;
end;

function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
  ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ct: pcairo_t;
  AFont: Pcairo_scaled_font_t;
  glyph, glyphs: Pcairo_glyph_t;
  num_glyphs: LongInt;
  clusters: PCairoClusterArray;
  num_clusters: LongInt;
  cluster_flags: cairo_text_cluster_flags_t;
  I, A: Integer;
begin
  ct := cairo_create_context(ACanvas.Handle);
  try
    cairo_set_font(ct, ACanvas.Font);
    AFont := cairo_get_scaled_font(ct);
    num_glyphs := 0;
    num_clusters := 0;
    clusters := nil;
    glyphs := nil;
    Result := cairo_scaled_font_text_to_glyphs(AFont, X, Y + cairo_font_baseline(AFont),
      Str, Count, @glyphs, @num_glyphs, @clusters, @num_clusters, @cluster_flags) = CAIRO_STATUS_SUCCESS;
    if not Result then Exit;
    if Dx <> nil then
    begin
      glyph := glyphs;
      for I := 0 to num_clusters - 1 do
        for A := 0 to clusters^[I].num_glyphs - 1 do
        begin
          glyph^.x := X;
          Inc(X, Dx^);
          Inc(glyph);
          Inc(Dx);
        end;
    end;
    if ARect <> nil then
    begin
      if ACanvas.Brush.Style = bsSolid then
        ACanvas.FillRect(ARect^);
      if Options and ETO_CLIPPED <> 0 then
      begin
        cairo_rectangle(ct, ARect^.Left, ARect^.Top, ARect^.Width + 1, ARect^.Height);
        cairo_clip(ct)
      end;
    end;
    cairo_set_source_color(ct, cairo_get_color(ACanvas.Font.Color));
    cairo_glyph_path(ct, glyphs, num_glyphs);
    cairo_fill_preserve(ct);
    cairo_glyph_free(glyphs);
    cairo_text_cluster_free(pcairo_text_cluster_t(clusters));
  finally
    cairo_destroy(ct);
  end;
end;

end.

