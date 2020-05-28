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
  Classes, SysUtils, Graphics,
  Forms,
  ExtCtrls,
  Math,
  LCLType, LCLIntf;

type
  { TATCharSizer }

  TATCharSizer = class
  private
    const SaveScale=2; //max stored char width (in %) is 255*SaveScale
  private
    FontName: string;
    FontSize: integer;
    SizeAvg: integer;
    FPanel: TPanel;
    Sizes: packed array[word] of byte; //width of WideChars, divided by SizeAvg, divided by SaveScale
    function GetCharWidth_FromCache(ch: WideChar): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(const AFontName: string; AFontSize: integer);
    function GetCharWidth(ch: WideChar): integer;
    //function GetStrWidth(const S: WideString): integer;
  end;

var
  GlobalCharSizer: TATCharSizer = nil;
  //must be created after MainForm is inited, e.g. in TATSynEdit.Create

var
  OptCharSizeProportional: boolean = true;
  OptCharScaleFullWidth: word = 190; //width of fullsize chars (CJK and others) in percents

const
  OptCharScaleHex_Small = 300; //width of hex show: "xNN"
  OptCharScaleHex_Big = 500; //width of hex show: "xNNNN"

var
  OptUnprintedReplaceSpec: boolean = false;
  OptUnprintedReplaceSpecToCode: integer = 164; //char 164 is small circle
  OptHexChars: UnicodeString = ''; //show these chars as "<NNNN>"
  OptHexCharsDefault: UnicodeString = ''; //recommended default for OptHexChars

function IsCharAsciiControl(ch: WideChar): boolean; inline;
function IsCharAccent(ch: WideChar): boolean; inline;
function IsCharHexDisplayed(ch: WideChar): boolean;


implementation

var
  FixedSizes: packed array[word] of byte;
const
  _norm = 1;
  _full = 2;
  _comb = 3;

procedure InitFixedSizes;
var
  i: word;
begin
  FillChar(FixedSizes, SizeOf(FixedSizes), 0);

  //Basic Latin
  //Latin-1 Supplement
  //Latin Extended-A
  for i:= $20 to $24F do
    FixedSizes[i]:= _norm;

  //some Latin are full-width
  for i:= $1C4 to $1CC do
    FixedSizes[i]:= _full;
  FixedSizes[$1F1]:= _full;
  FixedSizes[$1F2]:= _full;
  FixedSizes[$1F3]:= _full;
  FixedSizes[$24A]:= _full;

  //Cyrillic: full-width
  // https://en.wikipedia.org/wiki/Cyrillic_script_in_Unicode
  for i:= $400 to $4FF do
    FixedSizes[i]:= _full;
  for i:= $500 to $52F do
    FixedSizes[i]:= _full;
  for i:= $2DE0 to $2DFF do
    FixedSizes[i]:= _full;
  for i:= $A640 to $A69F do
    FixedSizes[i]:= _full;
  for i:= $1C80 to $1C8F do
    FixedSizes[i]:= _full;
  for i:= $1D2B to $1D78 do
    FixedSizes[i]:= _full;

  //Basic Russian: normal width
  // https://www.unicode.org/charts/PDF/U0400.pdf
  for i:= $410 to $451 do //a..ya A..YA yo
    FixedSizes[i]:= _norm;
  FixedSizes[$401]:= _norm; //YO

  //German
  // https://resources.german.lsa.umich.edu/schreiben/unicode/
  FixedSizes[$00DF]:= _norm;
  FixedSizes[$00E4]:= _norm;
  FixedSizes[$00F6]:= _norm;
  FixedSizes[$00FC]:= _norm;
  FixedSizes[$00C4]:= _norm;
  FixedSizes[$00D6]:= _norm;
  FixedSizes[$00DC]:= _norm;

  //Greek and Coptic
  // https://unicode.org/charts/PDF/U0370.pdf
  for i:= $370 to $3ff do
    FixedSizes[i]:= _norm;

  FixedSizes[$2026]:= _full; //unicode dots

  //Chinese
  // http://www.unicode.org/versions/Unicode5.0.0/ch12.pdf#G12159
  for i:= $3400 to $9FFF do
    FixedSizes[i]:= _full;
  for i:= $F900 to $FAFF do
    FixedSizes[i]:= _full;

  // http://www.rikai.com/library/kanjitables/kanji_codes.unicode.shtml
  //Japanese punctuation, Hiragana, Katakana
  for i:= $3000 to $30ff do
    FixedSizes[i]:= _full;
  //_full-width Roman
  for i:= $ff00 to $ff5f do
    FixedSizes[i]:= _full;
  //half-width Katakana
  for i:= $ff60 to $ff9f do
    FixedSizes[i]:= _norm;
  for i:= $ffa0 to $ffef do
    FixedSizes[i]:= _full;

  //Arabic
  // https://en.wikipedia.org/wiki/Arabic_script_in_Unicode
  for i:= $0600 to $06FF do
    FixedSizes[i]:= _full;
  for i:= $0750 to $077F do
    FixedSizes[i]:= _full;
  for i:= $08A0 to $08FF do
    FixedSizes[i]:= _full;
  for i:= $FB50 to $FDFF do
    FixedSizes[i]:= _full;
  for i:= $FE70 to $FEFF do
    FixedSizes[i]:= _full;

  //Hebrew
  // https://en.wikipedia.org/wiki/Hebrew_(Unicode_block)
  for i:= $0590 to $05FF do
    FixedSizes[i]:= _full;

  //combining chars
  // https://en.wikipedia.org/wiki/Combining_character
  for i:=$0300 to $036F do
    FixedSizes[i]:= _comb;
  for i:=$1AB0 to $1AFF do
    FixedSizes[i]:= _comb;
  for i:=$1DC0 to $1DFF do
    FixedSizes[i]:= _comb;
  for i:=$20D0 to $20FF do
    FixedSizes[i]:= _comb;
  for i:=$FE20 to $FE2F do
    FixedSizes[i]:= _comb;

  FixedSizes[$3099]:= _comb;
  FixedSizes[$309A]:= _comb;
  FixedSizes[$032A]:= _comb;
  for i:= $0346 to $034F do
    FixedSizes[i]:= _comb;
  for i:= $0363 to $036F do
    FixedSizes[i]:= _comb;

end;

function IsCharAsciiControl(ch: WideChar): boolean; inline;
begin
  Result:= (ch<>#9) and (Ord(ch)<$20);
end;

{
http://unicode.org/reports/tr9/#Directional_Formatting_Characters
https://en.wikipedia.org/wiki/Whitespace_character#Unicode
}
function IsCharHexDisplayed(ch: WideChar): boolean;
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

function IsCharAccent(ch: WideChar): boolean;
begin
  Result:= FixedSizes[Ord(ch)]=_comb;
end;

{$ifdef windows}
function _WidestrWidth(C: TCanvas; S: WideChar): integer; inline;
var
  Size: TSize;
begin
  Windows.GetTextExtentPointW(C.Handle, @S, 1{Len}, Size);
  Result:= Size.cx;
end;
{$else}
function _WidestrWidth(C: TCanvas; S: WideChar): integer; inline;
begin
  Result:= C.TextWidth(WideString(S));
  //debug
  //Write('#'+IntToHex(Ord(S),2)+'"'+S+'" ');
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

  FPanel.Parent:= Application.MainForm;
  FPanel.Canvas.Font.Name:= AFontName;
  FPanel.Canvas.Font.Size:= AFontSize;
  FPanel.Canvas.Font.Style:= [];

  SizeAvg:= FPanel.Canvas.TextWidth('N');
end;

function TATCharSizer.GetCharWidth_FromCache(ch: WideChar): integer;
begin
  Result:= Sizes[Ord(ch)] * SaveScale;
  if Result=0 then
  begin
    Result:= _WidestrWidth(FPanel.Canvas, ch) * 100 div SizeAvg;
    Sizes[Ord(ch)]:= Math.Min(255, Result div SaveScale);
  end;
end;

constructor TATCharSizer.Create;
begin
  FPanel:= TPanel.Create(nil);
  FPanel.Name:= 'AppSizerPanel';
  FPanel.Visible:= false;
  FPanel.SetBounds(0, 0, 50, 20);
end;

destructor TATCharSizer.Destroy;
begin
  FreeAndNil(FPanel);
  inherited;
end;

function TATCharSizer.GetCharWidth(ch: WideChar): integer;
var
  n: word absolute ch;
  size: integer;
begin
  Result:= 100;

  size:= FixedSizes[n];
  if size=_norm then exit;
  if size=_full then exit(OptCharScaleFullWidth);

  if OptUnprintedReplaceSpec and IsCharAsciiControl(ch) then
    exit;

  if IsCharHexDisplayed(ch) then
  begin
    if n<$100 then
      exit(OptCharScaleHex_Small)
    else
      exit(OptCharScaleHex_Big);
  end;

  if OptCharSizeProportional then
    if n>=128 then
      exit(GetCharWidth_FromCache(ch));

  //for other codes, use full-width size
  Result:= OptCharScaleFullWidth;
end;

{
function TATCharSizer.GetStrWidth(const S: WideString): integer;
begin
  Result:= _WidestrWidth(FPanel.Canvas, S) * 100 div SizeAvg;
end;
}

initialization

  InitFixedSizes;

finalization

  if Assigned(GlobalCharSizer) then
    FreeAndNil(GlobalCharSizer);

end.

