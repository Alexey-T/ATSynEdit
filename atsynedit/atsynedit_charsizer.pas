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

function IsCharAsciiControl(ch: WideChar): boolean; inline;
function IsCharAccent(ch: WideChar): boolean; inline;
function IsCharUnicodeSpace(ch: WideChar): boolean; inline;
function IsCharHexDisplayed(ch: WideChar): boolean; inline;
function IsCharUnusualWidth(ch: WideChar): boolean; inline;
function IsStringWithUnusualWidthChars(const S: UnicodeString): boolean;


implementation

var
  FixedSizes: packed array[word] of byte;

const
  _norm = 1;
  _full = 2;
  _space = 3;
  _comb = 4;
  _hexshow = 5;

procedure InitFixedSizes;
var
  i: word;
begin
  FillChar(FixedSizes, SizeOf(FixedSizes), 0);

  //Basic Latin
  //Latin-1 Supplement
  //Latin Extended-A
  for i:= $0 to $24F do
    FixedSizes[i]:= _norm;

  //IPA Extensions
  for i:= $250 to $2AF do
    FixedSizes[i]:= _norm;

  //some Latin are full-width
  for i:= $1C4 to $1CC do
    FixedSizes[i]:= _full;
  FixedSizes[$1F1]:= _full;
  FixedSizes[$1F2]:= _full;
  FixedSizes[$1F3]:= _full;
  FixedSizes[$24A]:= _full;

  //Cyrillic
  // https://www.compart.com/en/unicode/block/U+0400
  for i:= $400 to $408 do
    FixedSizes[i]:= _norm;
  for i:= $409 to $40B do
    FixedSizes[i]:= _full;
  for i:= $40C to $458 do //Russian is here
    FixedSizes[i]:= _norm;
  for i:= $459 to $45A do
    FixedSizes[i]:= _full;
  for i:= $45B to $45F do
    FixedSizes[i]:= _norm;
  for i:= $460 to $461 do
    FixedSizes[i]:= _full;
  for i:= $462 to $4FF do
    FixedSizes[i]:= _norm;

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

  //Syriac...Khmer..general puctuation
  for i:= $700 to $206F do
    FixedSizes[i]:= _full;

  for i:= $2010 to $2027 do
    FixedSizes[i]:= _norm;
  for i:= $2030 to $203A do
    FixedSizes[i]:= _norm;
  FixedSizes[$20AC]:= _norm;
  FixedSizes[$2122]:= _norm;

  //combining chars
  // https://en.wikipedia.org/wiki/Combining_character
  for i:=$0300 to $036F do
    FixedSizes[i]:= _comb;
  for i:=$483 to $489 do
    FixedSizes[i]:= _comb;
  for i:=$1AB0 to $1AFF do
    FixedSizes[i]:= _comb;
  for i:=$1DC0 to $1DFF do
    FixedSizes[i]:= _comb;
  for i:=$20D0 to $20FF do
    FixedSizes[i]:= _comb;
  for i:=$FE20 to $FE2F do
    FixedSizes[i]:= _comb;

  // https://www.compart.com/en/unicode/block/U+0E80
  FixedSizes[$eb1]:= _comb;
  FixedSizes[$eb4]:= _comb;
  for i:= $eb4 to $ebc do
    FixedSizes[i]:= _comb;
  for i:= $ec8 to $ecd do
    FixedSizes[i]:= _comb;

  FixedSizes[$3099]:= _comb;
  FixedSizes[$309A]:= _comb;

  //hex display
  // http://unicode.org/reports/tr9/#Directional_Formatting_Characters
  // https://en.wikipedia.org/wiki/Whitespace_character#Unicode
  for i:= 0 to $20-1 do //ascii control chars
    if i<>9 then
      FixedSizes[i]:= _hexshow;
  for i:= $2000 to $200F do //white spaces + specials
    FixedSizes[i]:= _hexshow;
  for i:= $2028 to $202F do  //white spaces + specials
    FixedSizes[i]:= _hexshow;
  for i:= $2066 to $2069 do
    FixedSizes[i]:= _hexshow;

  FixedSizes[$85]:= _hexshow; //white space
  FixedSizes[$061C]:= _hexshow;
  FixedSizes[$FEFF]:= _hexshow;

  //space chars
  FixedSizes[$9]:= _space;
  FixedSizes[$20]:= _space;
  FixedSizes[$A0]:= _space; //no-break space, NBSP, often used on macOS
  FixedSizes[$1680]:= _space; //white space
  FixedSizes[$2007]:= _space; //figure space
  FixedSizes[$200B]:= _space; //zero width space https://en.wikipedia.org/wiki/Zero-width_space
  FixedSizes[$202F]:= _space; //narrow no-break space
  FixedSizes[$205F]:= _space; //white space
  FixedSizes[$2060]:= _space; //white space
  FixedSizes[$3000]:= _space; //CJK white space

end;

function IsCharAsciiControl(ch: WideChar): boolean; inline;
begin
  Result:= (ch<>#9) and (Ord(ch)<$20);
end;

function IsCharHexDisplayed(ch: WideChar): boolean;
begin
  Result:= FixedSizes[Ord(ch)]=_hexshow;
end;

function IsCharAccent(ch: WideChar): boolean;
begin
  Result:= FixedSizes[Ord(ch)]=_comb;
end;

function IsCharUnicodeSpace(ch: WideChar): boolean;
begin
  Result:= FixedSizes[Ord(ch)]=_space;
end;

function IsCharUnusualWidth(ch: WideChar): boolean;
begin
  //tab-char must have unusual width (to render it with Dx offset)
  if Ord(ch)<32 then
    exit(true);

  case FixedSizes[Ord(ch)] of
    _norm, _space:
      Result:= false
    else
      Result:= true;
  end;
end;

function IsStringWithUnusualWidthChars(const S: UnicodeString): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if IsCharUnusualWidth(S[i]) then
      exit(true);
  Result:= false;
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
begin
  Result:= 100;

  case FixedSizes[n] of
    _norm: exit;
    _full: exit(OptCharScaleFullWidth);
    _space: exit;
    _comb: exit(0);
    _hexshow:
      begin
        if n<$100 then
          exit(OptCharScaleHex_Small)
        else
          exit(OptCharScaleHex_Big);
      end;
  end;

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

