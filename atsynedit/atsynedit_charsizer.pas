{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CharSizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
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

{
http://unicode.org/reports/tr9/#Directional_Formatting_Characters
Implicit Directional Formatting Characters 	LRM, RLM, ALM
Explicit Directional Embedding and Override Formatting Characters 	LRE, RLE, LRO, RLO, PDF
Explicit Directional Isolate Formatting Characters 	LRI, RLI, FSI, PDI
}
function IsCharHex(ch: widechar): boolean;
begin
  if ch=#9 then exit(false);
  if Ord(ch)<$20 then exit(true);
  if Ord(ch)<128 then exit(false);

  //codes for surrogate utf16 chars
  if (Ord(ch)>=$D800) and (Ord(ch)<=$DFFF) then exit(true);

  if (ch>=#$202A) and (ch<=#$202E) then exit(true);
  if (ch>=#$2066) and (ch<=#$2069) then exit(true);
  if ch=#$200E then exit(true);
  if ch=#$200F then exit(true);
  if ch=#$061C then exit(true);

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

function TATCharSizer.GetCharWidth_FromCache(ch: Widechar): integer;
begin
  Result:= Sizes[Ord(ch)];
  if Result=0 then
  begin
    Result:= Canvas.TextWidth(UTF8Encode(UnicodeString(ch))) * 100 div SizeAvg;
    Sizes[Ord(ch)]:= Result;
  end;
end;

function TATCharSizer.GetCharWidth(ch: widechar): integer;
begin
  Result:= 100;

  if Ord(ch)>=128 then
    exit(GetCharWidth_FromCache(ch));

  if OptUnprintedReplaceSpec and IsCharAsciiControl(ch) then
    exit;

  if IsCharHex(ch) then
  begin
    if Ord(ch)<$100 then
      exit(OptCharScaleHex_Small)
    else
      exit(OptCharScaleHex_Big);
  end;
end;


initialization

  GlobalCharSizer:= TATCharSizer.Create;

finalization

  FreeAndNil(GlobalCharSizer);

end.

