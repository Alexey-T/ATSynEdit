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
  Math,
  UnicodeData,
  LCLType, LCLIntf;

type
  { TATCharSizer }

  TATCharSizer = class
  private
    const SaveScale=2; //max stored char width (in %) is 255*SaveScale
  private
    FontName: string;
    FontSize: integer;
    Bitmap: TBitmap;
    SizeAvg: integer;
    Sizes: packed array[word] of byte; //width of WideChars, divided by SizeAvg, divided by SaveScale
    function GetCharWidth_FromCache(ch: widechar): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(const AFontName: string; AFontSize: integer);
    function GetCharWidth(ch: widechar): integer;
    function GetStrWidth(const S: WideString): integer;
  end;

var
  GlobalCharSizer: TATCharSizer = nil;
  //must be created after MainForm is inited, e.g. in TATSynEdit.Create

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

function IsCharAsciiControl(ch: widechar): boolean; inline;
function IsCharAccent(ch: widechar): boolean;
function IsCharHex(ch: widechar): boolean;


implementation

function IsCharAsciiControl(ch: widechar): boolean; inline;
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

  //these are Emojis
  //if IsCharSurrogate(ch) then exit(true);

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

{$ifdef windows}
function _WidestrWidth(C: TCanvas; const S: WideString): integer; inline;
var
  Size: TSize;
begin
  Windows.GetTextExtentPointW(C.Handle, PWChar(S), Length(S), Size);
  Result:= Size.cx;
end;
{$else}
function _WidestrWidth(C: TCanvas; const S: WideString): integer; inline;
begin
  Result:= C.TextWidth(UTF8Encode(S));
end;
{$endif}

{ TATCharSizer }

procedure TATCharSizer.Init(const AFontName: string; AFontSize: integer);
begin
  if (FontName<>AFontName) or (FontSize<>AFontSize) then
  begin
    FontName:= AFontName;
    FontSize:= AFontSize;
    FillChar(Sizes, SizeOf(Sizes), 0);
  end;
  Bitmap.Canvas.Font.Name:= AFontName;
  Bitmap.Canvas.Font.Size:= AFontSize;
  Bitmap.Canvas.Font.Style:= [];
  SizeAvg:= Bitmap.Canvas.TextWidth('M');
end;

function TATCharSizer.GetCharWidth_FromCache(ch: widechar): integer;
begin
  Result:= Sizes[Ord(ch)] * SaveScale;
  if Result=0 then
  begin
    Result:= _WidestrWidth(Bitmap.Canvas, WideString(ch)) * 100 div SizeAvg;
    Sizes[Ord(ch)]:= Math.Min(255, Result div SaveScale);
  end;
end;

constructor TATCharSizer.Create;
begin
  Bitmap:= TBitmap.Create;
  Bitmap.Width:= 50;
  Bitmap.Height:= 20;
end;

destructor TATCharSizer.Destroy;
begin
  FreeAndNil(Bitmap);
  inherited;
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

function TATCharSizer.GetStrWidth(const S: WideString): integer;
begin
  Result:= _WidestrWidth(Bitmap.Canvas, S) * 100 div SizeAvg;
end;


finalization

  if Assigned(GlobalCharSizer) then
    FreeAndNil(GlobalCharSizer);

end.

