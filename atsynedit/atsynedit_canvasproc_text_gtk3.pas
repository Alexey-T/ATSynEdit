{
Copyright (c) 2026 Alexey Torgashin (uvviewsoft.com)
License: MPL 2.0 or LGPL
}

unit ATSynEdit_CanvasProc_Text_Gtk3;

{$mode Delphi}{$H+}

interface

uses
  Graphics;

procedure NativeTextOut(ACanvas: TCanvas; AX, AY: Integer; const AStr: string);

implementation

uses
  SysUtils,
  LCLType,
  LCLIntf,
  Gtk3Objects, // Contains TGtk3DeviceContext
  Cairo,
  Lazcairo1,
  Math;

var
  CairoColors: array [0..255] of Double;

type
  ECairoException = class(Exception);

  TCairoColor = record
    R, G, B, A: Double;
  end;

var
  // Global variables used to cache font state.
  // The color/source cannot be cached because LCL may change it between calls,
  // for example when FillRect paints the selection background.
  LastFontName: string = '';
  LastFontSize: Integer = 0;
  LastFontBold: Boolean = False;
  LastFontItalic: Boolean = False;
  LastBaseline: Integer = 0;
  LastCt: pcairo_t = nil;

procedure NativeTextOut(ACanvas: TCanvas; AX, AY: Integer; const AStr: string);
var
  ct: pcairo_t;
  Ctx: TGtk3DeviceContext;
  IsBold, IsItalic: Boolean;
  LSlant: Tcairo_font_slant_t;
  LWeight: Tcairo_font_weight_t;
  ADefFont: TFontData;
  AColor: TColor;
  C: TCairoColor;
  x, y: Integer;
  extents: cairo_font_extents_t;
  FontChanged: Boolean;
  OwnsContext: Boolean;
begin
  Ctx := TGtk3DeviceContext(ACanvas.Handle);
  OwnsContext := False;

  // Optimization 1: reuse the existing LCL Cairo context instead of creating a new one.
  // This avoids expensive cairo_create/cairo_destroy calls for every text chunk.
  ct := Ctx.pcr;
  if ct = nil then
  begin
    if Ctx.CairoSurface <> nil then
    begin
      ct := cairo_create(Ctx.CairoSurface);
      OwnsContext := True;
    end
    else
      raise ECairoException.Create('Cannot get cairo context');
  end;

  try
    IsBold := fsBold in ACanvas.Font.Style;
    IsItalic := fsItalic in ACanvas.Font.Style;

    // Optimization 2: cache font parameters.
    // LCL does not modify the Cairo toy-text font face inside pcr,
    // so caching the font is safe here.
    FontChanged := (ACanvas.Font.Name <> LastFontName) or
                   (ACanvas.Font.Height <> LastFontSize) or
                   (IsBold <> LastFontBold) or
                   (IsItalic <> LastFontItalic) or
                   (ct <> LastCt);

    if FontChanged then
    begin
      LastCt := ct;

      if IsItalic then
        LSlant := CAIRO_FONT_SLANT_ITALIC
      else
        LSlant := CAIRO_FONT_SLANT_NORMAL;

      if IsBold then
        LWeight := CAIRO_FONT_WEIGHT_BOLD
      else
        LWeight := CAIRO_FONT_WEIGHT_NORMAL;

      if ACanvas.Font.IsDefault then
      begin
        ADefFont := GetFontData(GetStockObject(DEFAULT_GUI_FONT));
        cairo_select_font_face(ct, PChar(string(ADefFont.Name)), LSlant, LWeight);
        if ACanvas.Font.Height = 0 then
          cairo_set_font_size(ct, ADefFont.Height)
        else
          cairo_set_font_size(ct, Abs(ACanvas.Font.Height));
        LastFontName := string(ADefFont.Name);
      end
      else
      begin
        cairo_select_font_face(ct, PChar(ACanvas.Font.Name), LSlant, LWeight);
        if ACanvas.Font.Height = 0 then
        begin
          ADefFont := GetFontData(GetStockObject(DEFAULT_GUI_FONT));
          cairo_set_font_size(ct, ADefFont.Height);
        end
        else
          cairo_set_font_size(ct, Abs(ACanvas.Font.Height));
        LastFontName := ACanvas.Font.Name;
      end;

      LastFontSize := ACanvas.Font.Height;
      LastFontBold := IsBold;
      LastFontItalic := IsItalic;

      // Calculate and cache the font baseline.
      cairo_font_extents(ct, @extents);
      LastBaseline := Ceil(extents.height - extents.descent);
    end;

    // Regression fix: the color/source must always be set before drawing.
    // LCL GTK3 changes the source in pcr during operations such as FillRect,
    // for example when painting the selection background.
    // If this call were skipped, text could be painted with the background color.
    AColor := ACanvas.Font.Color;
    C.R := CairoColors[GetRValue(AColor)];
    C.G := CairoColors[GetGValue(AColor)];
    C.B := CairoColors[GetBValue(AColor)];
    C.A := 1.0;
    cairo_set_source_rgba(ct, C.R, C.G, C.B, C.A);

    x := AX;
    y := AY + LastBaseline;

    cairo_move_to(ct, x, y);
    cairo_show_text(ct, PChar(AStr));
  finally
    if OwnsContext then
      cairo_destroy(ct);
  end;
end;

var
  I: Integer;

initialization
  for I := 0 to 255 do
    CairoColors[I] := I / 255;

end.
