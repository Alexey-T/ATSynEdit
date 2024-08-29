unit multitextout_gtk2;

{$mode ObjFPC}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils,
  Types, LCLType,
  Gtk2Int;


type
  TMultiTextRec = record
    X, Y: Longint;
    Str: string;
    Dx: PInteger;
  end;

  TMultiTextArray = array of TMultiTextRec;

type
  TGtk2WidgetSetHelper = class helper for TGtk2WidgetSet
    procedure MultiTextOut(DC: HDC; constref Texts: TMultiTextArray);
  end;


implementation

uses
  LazUTF8,
  Gtk2Def, Gtk2Proc, Gdk2, pango;

procedure TGtk2WidgetSetHelper.MultiTextOut(DC: HDC; constref Texts: TMultiTextArray);
var
  DevCtx: TGtkDeviceContext absolute DC;
  DCOrigin: TPoint;
  Foreground, BackgroundColor: PGDKColor;

  procedure DoTextOut(X, Y: Integer; Str: Pchar; CurCount: Integer; Dx: PInteger);
  var
    CurStr: PChar;
    CurDx: PInteger;
    CurScreenX: LongInt;
    CharLen: LongInt;
    AFont: PGDIObject;
  begin
    AFont := DevCtx.GetFont;
    if (Dx <> nil) then
    begin
      CurStr := Str;
      CurDx := Dx;
      CurScreenX := X;
      while CurCount > 0 do
      begin
        CharLen := UTF8CodepointSize(CurStr);

        //DevCtx.DrawTextWithColors(CurStr, CharLen, CurScreenX, Y, Foreground, BackgroundColor);
        pango_layout_set_text(AFont^.GDIFontObject, CurStr, CharLen);
        gdk_draw_layout_with_colors(DevCtx.Drawable, DevCtx.GC, CurScreenX, Y, AFont^.GDIFontObject, Foreground, BackgroundColor);

        inc(CurScreenX, CurDx^);
        inc(CurDx);
        inc(CurStr, CharLen);
        dec(CurCount, CharLen);
      end;
    end
    else
    begin
      //DevCtx.DrawTextWithColors(Str, CurCount, X, Y, Foreground, BackgroundColor);
      pango_layout_set_text(AFont^.GDIFontObject, Str, CurCount);
      gdk_draw_layout_with_colors(DevCtx.Drawable, DevCtx.GC, X, Y, AFont^.GDIFontObject, Foreground, BackgroundColor);
    end;
  end;

var
  i: integer;
begin
  if not Gtk2WidgetSet.IsValidDC(DC) then Exit;
  if DevCtx.GC <> nil then; // create GC
  BackgroundColor := nil;
  DCOrigin := DevCtx.Offset;

  UpdateDCTextMetric(DevCtx);
  DevCtx.SelectedColors := dcscCustom;

  if (DevCtx.BkMode = OPAQUE) then
  begin
    AllocGDIColor(DC, @DevCtx.CurrentBackColor);
    BackGroundColor := @DevCtx.CurrentBackColor.Color;
  end;

  EnsureGCColor(DC, dccCurrentTextColor, True, False);
  Foreground := nil; //StyleForegroundColor(CurrentTextColor.ColorRef, nil);

  for i := 0 to High(Texts) do
    DoTextOut(
      Texts[i].X + DCOrigin.X,
      Texts[i].Y + DCOrigin.Y,
      PChar(Texts[i].Str),
      Length(Texts[i].Str),
      Texts[i].Dx
      );
end;

end.

