{
Copyright (c) 2026 Alexey Torgashin (uvviewsoft.com)
License: MPL 2.0 or LGPL
}

unit ATSynEdit_CanvasProc_Text_Gtk2;

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
  Types,
  gdk2,
  Gtk2Def,
  Cairo,
  glib2,
  Math;

var
  CairoColors: array [0..255] of Double;

type
  ECairoException = class(Exception);

  // Cache key for font state and context matrix.
  TFontKey = record
    Name: string;
    Height: Integer;
    Bold: Boolean;
    Italic: Boolean;
    mxx, mxy, myx, myy, mx0, my0: Double;
  end;

  // Cached scaled font and baseline.
  TCachedFont = record
    Key: TFontKey;
    SFont: Pcairo_scaled_font_t;
    Baseline: Integer;
  end;

const
  MaxCachedFonts = 64;

var
  CachedFonts: array[0..MaxCachedFonts - 1] of TCachedFont;
  CachedCount: Integer;

  DefaultFontInited: Boolean;
  DefaultFontName: string;
  DefaultFontHeight: Integer;

function cairo_create_context(DC: HDC): pcairo_t;
var
  Ctx: TGtkDeviceContext;
  Matrix: cairo_matrix_t;
begin
  Ctx := TGtkDeviceContext(DC);
  Result := gdk_cairo_create(Ctx.Drawable);

  if Result = nil then
    raise ECairoException.Create('Cannot create cairo context');

  // Preserve LCL mapping if it is active.
  if (Ctx.WindowExt.X <> 0) and (Ctx.WindowExt.Y <> 0) and
     (Ctx.WindowExt <> Ctx.ViewPortExt) then
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

procedure EnsureDefaultFont;
var
  ADefFont: TFontData;
begin
  if not DefaultFontInited then
  begin
    ADefFont := GetFontData(GetStockObject(DEFAULT_GUI_FONT));
    DefaultFontName := string(ADefFont.Name);
    DefaultFontHeight := Abs(ADefFont.Height);

    if DefaultFontHeight <= 0 then
      DefaultFontHeight := 10;

    DefaultFontInited := True;
  end;
end;

procedure MakeFontKey(AFont: TFont; const Matrix: cairo_matrix_t; out Key: TFontKey);
begin
  Key.Bold := fsBold in AFont.Style;
  Key.Italic := fsItalic in AFont.Style;

  if AFont.IsDefault or (AFont.Name = '') then
  begin
    EnsureDefaultFont;
    Key.Name := DefaultFontName;
  end
  else
    Key.Name := AFont.Name;

  if AFont.Height = 0 then
  begin
    EnsureDefaultFont;
    Key.Height := DefaultFontHeight;
  end
  else
    Key.Height := Abs(AFont.Height);

  if Key.Height <= 0 then
    Key.Height := 10;

  Key.mxx := Matrix.xx;
  Key.mxy := Matrix.xy;
  Key.myx := Matrix.yx;
  Key.myy := Matrix.yy;
  Key.mx0 := Matrix.x0;
  Key.my0 := Matrix.y0;
end;

function SameFontKey(const A, B: TFontKey): Boolean;
begin
  Result :=
    (A.Height = B.Height) and
    (A.Bold = B.Bold) and
    (A.Italic = B.Italic) and
    (CompareText(A.Name, B.Name) = 0) and
    (A.mxx = B.mxx) and
    (A.mxy = B.mxy) and
    (A.myx = B.myx) and
    (A.myy = B.myy) and
    (A.mx0 = B.mx0) and
    (A.my0 = B.my0);
end;

function FindCachedFont(const Key: TFontKey): Integer;
var
  i: Integer;
begin
  for i := 0 to CachedCount - 1 do
    if SameFontKey(CachedFonts[i].Key, Key) then
      Exit(i);

  Result := -1;
end;

function GetScaledFontBaseline(AFont: Pcairo_scaled_font_t): Integer;
var
  extents: cairo_font_extents_t;
begin
  cairo_scaled_font_extents(AFont, @extents);
  Result := Ceil(extents.height - extents.descent);
end;

procedure ApplyFontToContext(ct: pcairo_t; const Key: TFontKey);
var
  LSlant: cairo_font_slant_t;
  LWeight: cairo_font_weight_t;
begin
  if Key.Italic then
    LSlant := CAIRO_FONT_SLANT_ITALIC
  else
    LSlant := CAIRO_FONT_SLANT_NORMAL;

  if Key.Bold then
    LWeight := CAIRO_FONT_WEIGHT_BOLD
  else
    LWeight := CAIRO_FONT_WEIGHT_NORMAL;

  cairo_select_font_face(ct, PChar(Key.Name), LSlant, LWeight);
  cairo_set_font_size(ct, Key.Height);
end;

procedure AddCachedFont(const Key: TFontKey; SFont: Pcairo_scaled_font_t;
  Baseline: Integer);
begin
  if CachedCount >= MaxCachedFonts then
    Exit;

  CachedFonts[CachedCount].Key := Key;
  CachedFonts[CachedCount].SFont := cairo_scaled_font_reference(SFont);
  CachedFonts[CachedCount].Baseline := Baseline;

  Inc(CachedCount);
end;

procedure SetSourceColor(ct: pcairo_t; AColor: TColor);
var
  RGBColor: TColor;
begin
  RGBColor := ColorToRGB(AColor);

  cairo_set_source_rgba(
    ct,
    CairoColors[GetRValue(RGBColor)],
    CairoColors[GetGValue(RGBColor)],
    CairoColors[GetBValue(RGBColor)],
    1.0
  );
end;

procedure NativeTextOut(ACanvas: TCanvas; AX, AY: Integer; const AStr: string);
var
  ct: pcairo_t;
  m: cairo_matrix_t;
  Key: TFontKey;
  idx: Integer;
  sfont: Pcairo_scaled_font_t;
  baseline: Integer;
begin
  ct := cairo_create_context(ACanvas.Handle);
  try
    cairo_get_matrix(ct, @m);
    MakeFontKey(ACanvas.Font, m, Key);

    idx := FindCachedFont(Key);

    if idx >= 0 then
    begin
      // Fast path: reuse already created scaled font.
      cairo_set_scaled_font(ct, CachedFonts[idx].SFont);
      baseline := CachedFonts[idx].Baseline;
    end
    else
    begin
      // Slow path: create font state for this context and cache it.
      ApplyFontToContext(ct, Key);

      sfont := cairo_get_scaled_font(ct);
      baseline := GetScaledFontBaseline(sfont);

      AddCachedFont(Key, sfont, baseline);
    end;

    // Color must be set every time because a new Cairo context is used.
    SetSourceColor(ct, ACanvas.Font.Color);

    cairo_move_to(ct, AX, AY + baseline);
    cairo_show_text(ct, PChar(AStr));
  finally
    cairo_destroy(ct);
  end;
end;

var
  I: Integer;

initialization
  for I := 0 to 255 do
    CairoColors[I] := I / 255;

finalization
  for I := 0 to CachedCount - 1 do
    cairo_scaled_font_destroy(CachedFonts[I].SFont);

end.
