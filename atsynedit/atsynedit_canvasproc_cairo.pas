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

  function CairoDrawText(ACanvas: TCanvas; const Str: string;
    var ARect: TRect; Flags: Cardinal): Integer;
  function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
    ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;

  function CairoDrawTextOld(ACanvas: TCanvas; const Str: string;
    var ARect: TRect; Flags: Cardinal): Integer;
  function CairoExtTextOutOld(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
    ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;

implementation

uses
  gdk2,
  Gtk2Def,
  Cairo,
  pango,
  pangocairo,
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

procedure DrawPangoGlyph(const ct: TCairoContext; Str: PChar; X: Integer;
  CalcRect: PRect; Dx: PInteger);
var
  layout: PPangoLayout;
  font_descriptions: PPangoFontDescription;
  glyphs, start: Pcairo_glyph_t;
  I, A, Len, BytesLen, GlyphOffset: Integer;
  CurrentValid, GlyphValid: Boolean;

  procedure DrawPart;
  var
    pX, pY: LongInt;
    CurrentGlyphX: Double;
  begin
    if Dx = nil then
      CurrentGlyphX := glyphs^.x
    else
      CurrentGlyphX := X;
    if CalcRect = nil then
    begin
      if start^.index > 0 then
        cairo_show_glyphs(ct.Context, start, Len)
      else
      begin
        cairo_move_to(ct.Context, start^.x, start^.y);
        pango_layout_set_text(layout, Str, BytesLen);
        pango_layout_get_pixel_size(layout, @pX, @pY);
        pango_cairo_show_layout_line(ct.Context, pango_layout_get_line(layout, 0));
        GlyphOffset := pX - Ceil(CurrentGlyphX - start^.x);
      end;
    end
    else
      if start^.index = 0 then
      begin
        pango_layout_set_text(layout, Str, BytesLen);
        pango_layout_get_pixel_size(layout, @pX, @pY);
        CalcRect^.Height := pY;
        Inc(CalcRect^.Right, pX - Ceil(CurrentGlyphX - start^.x) + 1);
      end;
    start := glyphs;
    Inc(Str, BytesLen);
    BytesLen := 0;
    Len := 0;
  end;

begin
  layout := pango_cairo_create_layout(ct.Context);
  font_descriptions := pango_font_description_new;
  pango_font_description_set_size(font_descriptions, PANGO_SCALE * ct.Size);
  pango_layout_set_font_description(layout, font_descriptions);
  pango_font_description_free(font_descriptions);

  glyphs := ct.Glyphs;
  start := glyphs;
  BytesLen := 0;
  Len := 0;
  GlyphOffset := 0;
  CurrentValid := glyphs^.index > 0;
  for I := 0 to ct.ClustersLen - 1 do
  begin
    for A := 0 to ct.Clusters^[I].num_glyphs - 1 do
    begin
      GlyphValid := glyphs^.index > 0;
      if GlyphValid <> CurrentValid then
      begin
        DrawPart;
        CurrentValid := GlyphValid;
      end;
      if Dx <> nil then
      begin
        glyphs^.x := X;
        Inc(X, Dx^);
        Inc(Dx);
      end;
      glyphs^.x := glyphs^.x + GlyphOffset;
      Inc(glyphs);
    end;
    Inc(BytesLen, ct.Clusters^[I].num_bytes);
    Inc(Len);
  end;

  DrawPart;

  g_object_unref(layout);
end;

function CairoDrawText(ACanvas: TCanvas; const Str: string; var ARect: TRect;
  Flags: Cardinal): Integer;
var
  ct: TCairoContext;
  textents: cairo_text_extents_t;
  x, y, awidth: Integer;
begin
  Result := 0;
  if not CreateCairoContext(ACanvas, PChar(Str), -1, ARect.Left, ARect.Top, ct) then Exit;
  try
    cairo_scaled_font_extents(ct.Font, @textents);
    Result := Ceil(textents.height);

    cairo_text_extents(ct.Context, PChar(Str), @textents);
    if Flags and DT_CALCRECT <> 0 then
    begin
      ARect.Width := Ceil(textents.width);
      ARect.Height := Max(Result, Ceil(textents.height));
      if ct.InvalidGlyphsPresent then
        DrawPangoGlyph(ct, PChar(Str), ARect.Left, @ARect, nil);
      Exit;
    end;

    if Flags and DT_NOCLIP = 0 then
    begin
      cairo_rectangle(ct.Context, ARect.Left, ARect.Top, ARect.Width + 1, ARect.Height);
      cairo_clip(ct.Context);
    end;

    x := ARect.Left;
    y := ARect.Top + cairo_font_baseline(ct.Font);
    awidth := Ceil(textents.width);
    if Flags and DT_CENTER <> 0 then
      x := ARect.Left + (ARect.Width - awidth) div 2;
    if Flags and DT_RIGHT <> 0 then
      x := ARect.Right - awidth;

    if ACanvas.Brush.Style = bsSolid then
    begin
      ARect.Left := Max(X, ARect.Left);
      ACanvas.FillRect(ARect);
    end;

    if ct.InvalidGlyphsPresent then
      DrawPangoGlyph(ct, PChar(Str), x, nil, nil)
    else
    begin
      cairo_move_to(ct.Context, x, y);
      cairo_show_text(ct.Context, PChar(Str));
    end;

  finally
    ReleaseCairoContext(ct);
  end;
end;

function CairoExtTextOut(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
  ARect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ct: TCairoContext;
  glyphs: Pcairo_glyph_t;
  I, A: Integer;
begin
  Result := CreateCairoContext(ACanvas, Str, Count, X, Y, ct);
  if not Result then Exit;
  try
    if ARect <> nil then
    begin
      if ACanvas.Brush.Style = bsSolid then
        ACanvas.FillRect(ARect^);
      if Options and ETO_CLIPPED <> 0 then
      begin
        cairo_rectangle(ct.Context, ARect^.Left, ARect^.Top, ARect^.Width + 1, ARect^.Height);
        cairo_clip(ct.Context)
      end;
    end;

    if ct.InvalidGlyphsPresent then
    begin
      DrawPangoGlyph(ct, PChar(Str), X, nil, Dx);
      Exit;
    end;

    if Dx = nil then
    begin
      cairo_move_to(ct.Context, x, y + cairo_font_baseline(ct.Font));
      cairo_show_text(ct.Context, Str);
      Exit;
    end;

    glyphs := ct.Glyphs;
    for I := 0 to ct.ClustersLen - 1 do
      for A := 0 to ct.Clusters^[I].num_glyphs - 1 do
      begin
        glyphs^.x := X;
        Inc(X, Dx^);
        Inc(glyphs);
        Inc(Dx);
      end;

    cairo_show_glyphs(ct.Context, ct.Glyphs, ct.GlyphsLen);

  finally
    ReleaseCairoContext(ct);
  end;
end;

function CairoDrawTextOld(ACanvas: TCanvas; const Str: string;
  var ARect: TRect; Flags: Cardinal): Integer;
var
  ct: pcairo_t;
  sfont: Pcairo_scaled_font_t;
  fextents: cairo_font_extents_t;
  textents: cairo_text_extents_t;
  x, y, awidth: Integer;
begin
  Result := 0;
  ct := cairo_create_context(ACanvas.Handle);
  try
    cairo_set_font(ct, ACanvas.Font);
    cairo_set_source_color(ct, cairo_get_color(ACanvas.Font.Color));
    sfont := cairo_get_scaled_font(ct);
    cairo_scaled_font_extents(sfont, @fextents);
    Result := Ceil(fextents.height);
    x := ARect.Left;
    y := ARect.Top + cairo_font_baseline(sfont);
    if Flags and DT_NOCLIP = 0 then
    begin
      cairo_rectangle(ct, ARect.Left, ARect.Top, ARect.Width + 1, ARect.Height);
      cairo_clip(ct);
    end;
    cairo_text_extents(ct, PChar(Str), @textents);
    if Flags and DT_CALCRECT <> 0 then
    begin
      ARect.Width := Ceil(textents.width);
      ARect.Height := Max(Result, Ceil(textents.height));
      Exit;
    end;
    awidth := Ceil(textents.width);
    if Flags and DT_CENTER <> 0 then
      x := ARect.Left + (ARect.Width - awidth) div 2;
    if Flags and DT_RIGHT <> 0 then
      x := ARect.Right - awidth;
    if ACanvas.Brush.Style = bsSolid then
    begin
      ARect.Left := Max(X, ARect.Left);
      ACanvas.FillRect(ARect);
    end;
    cairo_move_to(ct, x, y);
    cairo_show_text(ct, PChar(Str));
  finally
    cairo_destroy(ct);
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

function CairoExtTextOutOld(ACanvas: TCanvas; X, Y: Integer; Options: Longint;
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

var
  I: Integer;

initialization

  for I := 0 to 255 do
    CairoColors[I] := I / 255;

end.

