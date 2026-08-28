unit ATSynEdit_CanvasProc_Text_Qt5;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

procedure NativeTextOut(ACanvas: TCanvas; AX, AY: Integer; const AStr: WideString);

implementation

uses
  Classes, SysUtils, Types,
  Qt5, qtobjects;

type
  ECairoException = class(Exception);

  QColor = record
    R, G, B, A: Byte;
  end;
  PQColor = ^QColor;

  TFontKey = record
    Name: string;
    Size: Integer;
    Bold: Boolean;
    Italic: Boolean;
    Underline: Boolean;
    StrikeOut: Boolean;
  end;

  TCachedFont = record
    Key: TFontKey;
    Font: QFontH;
    Metrics: QFontMetricsH;
    Ascent: Integer;
  end;

const
  MaxCachedFonts = 16;

var
  CachedFonts: array[0..MaxCachedFonts - 1] of TCachedFont;
  CachedCount: Integer = 0;
  DefaultFontName: string;
  DefaultFontSize: Integer;
  DefaultFontInited: Boolean = False;
  i: Integer;

procedure EnsureDefaultFont;
var
  DefFont: QFontH;
  Family: WideString;
begin
  if not DefaultFontInited then
  begin
    DefFont := nil;
    QApplication_font(DefFont);
    QFont_family(DefFont, @Family);
    DefaultFontName := UTF8Encode(Family);
    DefaultFontSize := QFont_pointSize(DefFont);
    if DefaultFontSize <= 0 then
      DefaultFontSize := 10;
    QFont_destroy(DefFont);
    DefaultFontInited := True;
  end;
end;

procedure MakeFontKey(AFont: TFont; out Key: TFontKey);
begin
  Key.Bold := fsBold in AFont.Style;
  Key.Italic := fsItalic in AFont.Style;
  Key.Underline := fsUnderline in AFont.Style;
  Key.StrikeOut := fsStrikeOut in AFont.Style;

  if (AFont.Name = '') or AFont.IsDefault then
  begin
    EnsureDefaultFont;
    Key.Name := DefaultFontName;
  end
  else
    Key.Name := AFont.Name;

  if AFont.Size <= 0 then
  begin
    EnsureDefaultFont;
    Key.Size := DefaultFontSize;
  end
  else
    Key.Size := AFont.Size;

  if Key.Size <= 0 then
    Key.Size := 10;
end;

function SameFontKey(const A, B: TFontKey): Boolean;
begin
  Result :=
    (A.Size = B.Size) and
    (A.Bold = B.Bold) and
    (A.Italic = B.Italic) and
    (A.Underline = B.Underline) and
    (A.StrikeOut = B.StrikeOut) and
    (CompareText(A.Name, B.Name) = 0);
end;

function FindCachedFont(const Key: TFontKey): Integer;
var
  j: Integer;
begin
  for j := 0 to CachedCount - 1 do
    if SameFontKey(CachedFonts[j].Key, Key) then
      Exit(j);
  Result := -1;
end;

{ Создаёт QFont, применяет к QPainter, возвращает живые объекты для кэша.
  ВАЖНО: Font и Metrics НЕ уничтожаются здесь — они передаются в кэш. }
procedure CreateFontForContext(Painter: QPainterH; const Key: TFontKey;
  out Font: QFontH; out Metrics: QFontMetricsH; out Ascent: Integer);
var
  Weight: Integer;
  WS: WideString;
begin
  Font := QFont_Create();

  WS := Key.Name;
  QFont_setFamily(Font, @WS);
  QFont_setPointSize(Font, Key.Size);

  if Key.Bold then
    Weight := 75  // QFont::Bold
  else
    Weight := 50; // QFont::Normal
  QFont_setWeight(Font, Weight);

  QFont_setItalic(Font, Key.Italic);
  QFont_setUnderline(Font, Key.Underline);
  QFont_setStrikeOut(Font, Key.StrikeOut);

  QPainter_setFont(Painter, Font);

  Metrics := QFontMetrics_Create(Font);
  Ascent := QFontMetrics_ascent(Metrics);
end;

procedure AddCachedFont(const Key: TFontKey; Font: QFontH;
  Metrics: QFontMetricsH; Ascent: Integer);
begin
  if CachedCount >= MaxCachedFonts then
    Exit;

  CachedFonts[CachedCount].Key := Key;
  CachedFonts[CachedCount].Font := Font;
  CachedFonts[CachedCount].Metrics := Metrics;
  CachedFonts[CachedCount].Ascent := Ascent;
  Inc(CachedCount);
end;

procedure SetPenColor(Painter: QPainterH; AColor: TColor);
var
  RGBColor: TColor;
  Color: QColor;
begin
  RGBColor := ColorToRGB(AColor);
  Color.R := Red(RGBColor);
  Color.G := Green(RGBColor);
  Color.B := Blue(RGBColor);
  Color.A := 255;
  QPainter_setPen(Painter, @Color);
end;

procedure NativeTextOut(ACanvas: TCanvas; AX, AY: Integer; const AStr: WideString);
var
  QtDC: TQtDeviceContext;
  Painter: QPainterH;
  Key: TFontKey;
  idx: Integer;
  Metrics: QFontMetricsH;
  Ascent: Integer;
  WS: WideString;
  NewFont: QFontH;
begin
  if not ACanvas.HandleAllocated then exit;
  QtDC := TQtDeviceContext(ACanvas.Handle);
  Painter := QtDC.Widget;
  if not Assigned(Painter) then
    raise ECairoException.Create('Invalid QPainter handle in HDC');

  MakeFontKey(ACanvas.Font, Key);

  idx := FindCachedFont(Key);

  if idx >= 0 then
  begin
    { Быстрый путь: шрифт уже в кэше }
    QPainter_setFont(Painter, CachedFonts[idx].Font);
    Ascent := CachedFonts[idx].Ascent;
  end
  else
  begin
    { Медленный путь: создаём шрифт и сохраняем в кэш ЖИВЫМ }
    CreateFontForContext(Painter, Key, NewFont, Metrics, Ascent);
    AddCachedFont(Key, NewFont, Metrics, Ascent);
  end;

  SetPenColor(Painter, ACanvas.Font.Color);

  WS := AStr;
  QPainter_drawText(Painter, AX, AY + Ascent, @WS);
end;

initialization
  CachedCount := 0;

finalization
  for i := 0 to CachedCount - 1 do
  begin
    if Assigned(CachedFonts[i].Metrics) then
      QFontMetrics_destroy(CachedFonts[i].Metrics);
    if Assigned(CachedFonts[i].Font) then
      QFont_destroy(CachedFonts[i].Font);
  end;
  CachedCount := 0;

end.
