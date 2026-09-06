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
  Forms,
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
  LastFontName: string = '';
  LastFontSize: Integer = 0;
  LastFontBold: Boolean = False;
  LastFontItalic: Boolean = False;
  LastBaseline: Integer = 0;
  LastCt: pcairo_t = nil;
  LastFontMatrix: cairo_matrix_t; // initialised below

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
  CurFontMatrix: cairo_matrix_t;
begin
  Ctx := TGtk3DeviceContext(ACanvas.Handle);
  OwnsContext := False;

  // Optimization 1: reuse the existing LCL Cairo context instead of creating a new one.
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

  {
  WriteLn(Format('[DBG] Font.Name=%s Height=%d Size=%d PixelsPerInch=%d ct=%p',
    [ACanvas.Font.Name, ACanvas.Font.Height, ACanvas.Font.Size,
     Screen.PixelsPerInch, ct]));
  }

  try
    IsBold := fsBold in ACanvas.Font.Style;
    IsItalic := fsItalic in ACanvas.Font.Style;

    // Optimization 2: cache font parameters.
    // We invalidate the cache when any canvas font property changes
    // OR the cairo_t pointer changes OR the Cairo font matrix on the
    // context has been overwritten externally (see fix #2 above).
    FontChanged := (ACanvas.Font.Name <> LastFontName) or
                   (ACanvas.Font.Height <> LastFontSize) or
                   (IsBold <> LastFontBold) or
                   (IsItalic <> LastFontItalic) or
                   (ct <> LastCt);

    // Detect external modification of the Cairo font matrix.
    // LCL GTK3 may call pango_cairo_update_layout between our calls
    // (e.g. when painting gutter text or selection via Pango), which
    // internally calls cairo_set_font_matrix and silently overwrites
    // the toy-text font size we set.  Without this check the cache
    // believes the font is still valid and text is drawn at Pango's
    // (typically smaller) size — visible after window resize when the
    // paint order or Pango font setup changes.
    if not FontChanged then
    begin
      cairo_get_font_matrix(ct, @CurFontMatrix);
      if (Abs(CurFontMatrix.xx - LastFontMatrix.xx) > 0.01) or
         (Abs(CurFontMatrix.yx - LastFontMatrix.yx) > 0.01) or
         (Abs(CurFontMatrix.xy - LastFontMatrix.xy) > 0.01) or
         (Abs(CurFontMatrix.yy - LastFontMatrix.yy) > 0.01) then
        FontChanged := True;
    end;

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
          cairo_set_font_size(ct, Abs(ADefFont.Height))          // FIX #1: was ADefFont.Height (negative!)
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
          cairo_set_font_size(ct, Abs(ADefFont.Height));         // FIX #1: was ADefFont.Height (negative!)
        end
        else
          cairo_set_font_size(ct, Abs(ACanvas.Font.Height));
        LastFontName := ACanvas.Font.Name;
      end;

      LastFontSize := ACanvas.Font.Height;
      LastFontBold := IsBold;
      LastFontItalic := IsItalic;

      // Save the font matrix we just set so we can detect external
      // overwrites on subsequent calls (fix #2).
      cairo_get_font_matrix(ct, @LastFontMatrix);

      // Calculate and cache the font baseline.
      cairo_font_extents(ct, @extents);
      LastBaseline := Ceil(extents.height - extents.descent);
    end;

    // Regression fix: the color/source must always be set before drawing.
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
  // Initialise to identity so the first call never matches stale garbage.
  LastFontMatrix.xx := 1.0;
  LastFontMatrix.yx := 0.0;
  LastFontMatrix.xy := 0.0;
  LastFontMatrix.yy := 1.0;
  LastFontMatrix.x0 := 0.0;
  LastFontMatrix.y0 := 0.0;

end.
