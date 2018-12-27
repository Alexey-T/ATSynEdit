{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CharSizer;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, Graphics, Dialogs,
  UnicodeData,
  LCLType, LCLIntf;

type
  { TATCharSizer }

  TATCharSizer = class
  private
    FontName: string;
    FontSize: integer;
    Canvas: TCanvas;
    Sizes: packed array[word] of word;
    SizeAvg: integer;
    function GetCharWidth_FromCache(ch: widechar): integer;
  public
    procedure Init(const AFontName: string; AFontSize: integer; ACanvas: TCanvas);
    function GetCharWidth(ch: widechar): integer;
  end;

var
  GlobalCharSizer: TATCharSizer;

var
  OptCharSizeProportional: boolean = true;
  OptCharSizeWideAllowed: boolean = true;
  OptCharScaleFullWidth: word = 190; //width of fullsize chars (CJK and others) in percents

  OptCharFillWidth_Range1_begin: word = $180;
  OptCharFillWidth_Range1_end: word = $24F;
  OptCharFillWidth_Range2_begin: word = $1100;
  OptCharFillWidth_Range2_end: word = $FFFF;
  OptCharFillWidth_Range3_begin: word = 0;
  OptCharFillWidth_Range3_end: word = 0;

  OptCharScaleHex_Small: word = 300; //width of hex show: "xNN"
  OptCharScaleHex_Big: word = 500; //width of hex show: "xNNNN"
  OptUnprintedReplaceSpec: boolean = false;
  OptUnprintedReplaceSpecToCode: integer = 164; //char 164 is small circle
  OptHexChars: UnicodeString = ''; //show these chars as "<NNNN>"
  OptHexCharsDefault: UnicodeString = ''; //recommended default for OptHexChars

function IsCharAsciiControl(ch: widechar): boolean;
function IsCharAccent(ch: widechar): boolean;
function IsCharHex(ch: widechar): boolean;


implementation

function IsCharAsciiControl(ch: widechar): boolean;
begin
  Result:= (ch<>#9) and (Ord(ch)<$20);
end;

function IsCharFullWidth(ch: widechar): boolean;
begin
  Result:= false;
  if ch=#$2026 then exit; //unicode dots

  if (Ord(ch)>=OptCharFillWidth_Range1_begin) and
     (Ord(ch)<=OptCharFillWidth_Range1_end) then exit(true);

  if (Ord(ch)>=OptCharFillWidth_Range2_begin) and
     (Ord(ch)<=OptCharFillWidth_Range2_end) then exit(true);

  if (Ord(ch)>=OptCharFillWidth_Range3_begin) and
     (Ord(ch)<=OptCharFillWidth_Range3_end) then exit(true);
end;

{
http://unicode.org/reports/tr9/#Directional_Formatting_Characters
https://en.wikipedia.org/wiki/Whitespace_character#Unicode
}
function IsCharHex(ch: widechar): boolean;
begin
  if ch=#9 then exit(false);
  if ch<#$20 then exit(true);
  if ch<#128 then exit(false);

  if ch=#$85 then exit(true); //white space
  if (ch>=#$D800) and (ch<=#$DFFF) then exit(true); //surrogate utf-16 chars
  if (ch>=#$2000) and (ch<=#$200F) then exit(true); //white spaces + specials
  if (ch>=#$2028) and (ch<=#$202F) then exit(true); //white spaces + specials
  if (ch>=#$2066) and (ch<=#$2069) then exit(true);
  if ch=#$061C then exit(true);
  if ch=#$FEFF then exit(true);

  Result:= Pos(ch, OptHexChars)>0;
end;

{
http://en.wikipedia.org/wiki/Combining_character
Combining Diacritical Marks (0300–036F), since version 1.0, with modifications in subsequent versions down to 4.1
Combining Diacritical Marks Extended (1AB0–1AFF), version 7.0
Combining Diacritical Marks Supplement (1DC0–1DFF), versions 4.1 to 5.2
Combining Diacritical Marks for Symbols (20D0–20FF), since version 1.0, with modifications in subsequent versions down to 5.1
Combining Half Marks (FE20–FE2F), versions 1.0, updates in 5.2
}
{
http://www.unicode.org/charts/PDF/U0E80.pdf
cannot render them ok anyway as accents:
0EB1, 0EB4..0EBC, 0EC8..0ECD
}
function IsCharAccent(ch: widechar): boolean;
begin
  case GetProps(Ord(ch))^.Category of
    UGC_NonSpacingMark .. UGC_EnclosingMark:
      Result:= true;
    else
      Result:= false;
  end;
end;


{ TATCharSizer }

procedure TATCharSizer.Init(const AFontName: string; AFontSize: integer; ACanvas: TCanvas);
begin
  if (FontName<>AFontName) or (FontSize<>AFontSize) then
  begin
    FontName:= AFontName;
    FontSize:= AFontSize;
    FillChar(Sizes, SizeOf(Sizes), 0);
  end;
  Canvas:= ACanvas;
  Canvas.Font.Name:= AFontName;
  Canvas.Font.Size:= AFontSize;
  Canvas.Font.Style:= [];
  SizeAvg:= Canvas.TextWidth('M');
end;

{$ifdef windows}
function _CharWidth(C: TCanvas; ch: Widechar): integer; inline;
var
  Size: TSize;
begin
  Windows.GetTextExtentPointW(C.Handle, @ch, 1, Size);
  Result:= Size.cx;
end;
{$else}
function _CharWidth(C: TCanvas; ch: Widechar): integer; inline;
begin
  Result:= C.TextWidth(UTF8Encode(WideString(ch)));
end;
{$endif}

function TATCharSizer.GetCharWidth_FromCache(ch: Widechar): integer;
begin
  Result:= Sizes[Ord(ch)];
  if Result=0 then
  begin
    if Canvas=nil then
    begin
      ShowMessage('Program error: CharSize.Init was not called');
      exit(8); //some char width
    end;
    Result:= _CharWidth(Canvas, ch) * 100 div SizeAvg;
    Sizes[Ord(ch)]:= Result;
  end;
end;

function TATCharSizer.GetCharWidth(ch: widechar): integer;
begin
  Result:= 100;

  if OptUnprintedReplaceSpec and IsCharAsciiControl(ch) then
    exit;

  if IsCharHex(ch) then
  begin
    if Ord(ch)<$100 then
      exit(OptCharScaleHex_Small)
    else
      exit(OptCharScaleHex_Big);
  end;

  if OptCharSizeProportional then
    if Ord(ch)>=128 then
      exit(GetCharWidth_FromCache(ch));

  if OptCharSizeWideAllowed and IsCharFullWidth(ch) then
    exit(OptCharScaleFullWidth);
end;


initialization

  GlobalCharSizer:= TATCharSizer.Create;

finalization

  FreeAndNil(GlobalCharSizer);

end.

