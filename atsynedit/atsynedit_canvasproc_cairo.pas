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

implementation

uses
  gdk2,
  Gtk2Def,
  Cairo,
  glib2,
  Math;

var
  CairoColors: array [0..255] of Double;

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
  Result.R := CairoColors[GetRValue(AColor)];
  Result.G := CairoColors[GetGValue(AColor)];
  Result.B := CairoColors[GetBValue(AColor)];
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

type
  TCairoContext = record
    Context: pcairo_t;
    Font: Pcairo_scaled_font_t;
    Size: Integer;
    Glyphs: Pcairo_glyph_t;
    GlyphsLen: LongInt;
    Clusters: PCairoClusterArray;
    ClustersLen: LongInt;
    InvalidGlyphsPresent: Boolean;
  end;

procedure ReleaseCairoContext(var AContext: TCairoContext);
begin
  cairo_glyph_free(AContext.Glyphs);
  cairo_text_cluster_free(pcairo_text_cluster_t(AContext.Clusters));
  cairo_destroy(AContext.Context);
end;

function CreateCairoContext(ACanvas: TCanvas; Str: PChar;
  Count, X, Y: Integer; out AContext: TCairoContext): Boolean;
var
  cluster_flags: cairo_text_cluster_flags_t;
  glyphs: Pcairo_glyph_t;
  I, A: Integer;
begin
  AContext := Default(TCairoContext);
  AContext.Context := cairo_create_context(ACanvas.Handle);
  cairo_set_font(AContext.Context, ACanvas.Font);
  cairo_set_source_color(AContext.Context, cairo_get_color(ACanvas.Font.Color));
  AContext.Font := cairo_get_scaled_font(AContext.Context);
  AContext.Size := ACanvas.Font.Size;
  Result := cairo_scaled_font_text_to_glyphs(AContext.Font,
    X, Y + cairo_font_baseline(AContext.Font),
    Str, Count, @AContext.Glyphs, @AContext.GlyphsLen, @AContext.Clusters,
    @AContext.ClustersLen, @cluster_flags) = CAIRO_STATUS_SUCCESS;
  if not Result then
  begin
    ReleaseCairoContext(AContext);
    Exit;
  end;
  glyphs := AContext.Glyphs;
  for I := 0 to AContext.ClustersLen - 1 do
    for A := 0 to AContext.Clusters^[I].num_glyphs - 1 do
    begin
      if glyphs^.index = 0 then
      begin
        AContext.InvalidGlyphsPresent := True;
        Break;
      end;
      Inc(glyphs);
    end;
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


var
  I: Integer;

initialization

  for I := 0 to 255 do
    CairoColors[I] := I / 255;

end.

