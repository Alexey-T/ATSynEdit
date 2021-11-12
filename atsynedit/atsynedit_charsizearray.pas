{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CharSizeArray;

{$mode objfpc}{$H+}

interface

var
  FixedSizes: packed array[word] of byte;

const
  uw_normal = 1;    //chars with usual width
  uw_fullwidth = 2; //full-width chars
  uw_space = 3;     //whitespaces
  uw_combined = 4;  //combined chars (accents)
  uw_hexshow = 5;   //show these chars in hex form like "xNNNN"

implementation

procedure InitFixedSizes;
var
  i: word;
begin
  FillChar(FixedSizes, SizeOf(FixedSizes), 0);

  //Basic Latin
  //Latin-1 Supplement
  //Latin Extended-A
  for i:= $0 to $24F do
    FixedSizes[i]:= uw_normal;

  //IPA Extensions
  for i:= $250 to $2D7 do
    FixedSizes[i]:= uw_normal;

  //some Latin are full-width
  for i:= $1C4 to $1CC do
    FixedSizes[i]:= uw_fullwidth;
  FixedSizes[$1F1]:= uw_fullwidth;
  FixedSizes[$1F2]:= uw_fullwidth;
  FixedSizes[$1F3]:= uw_fullwidth;
  FixedSizes[$24A]:= uw_fullwidth;

  //Cyrillic
  // https://www.compart.com/en/unicode/block/U+0400
  for i:= $400 to $408 do
    FixedSizes[i]:= uw_normal;
  for i:= $409 to $40B do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $40C to $458 do //Russian is here
    FixedSizes[i]:= uw_normal;
  for i:= $459 to $45A do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $45B to $45F do
    FixedSizes[i]:= uw_normal;
  for i:= $460 to $461 do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $462 to $4FF do
    FixedSizes[i]:= uw_normal;

  for i:= $500 to $52F do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $2DE0 to $2DFF do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $A640 to $A69F do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $1C80 to $1C8F do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $1D2B to $1D78 do
    FixedSizes[i]:= uw_fullwidth;

  //German
  // https://resources.german.lsa.umich.edu/schreiben/unicode/
  FixedSizes[$00DF]:= uw_normal;
  FixedSizes[$00E4]:= uw_normal;
  FixedSizes[$00F6]:= uw_normal;
  FixedSizes[$00FC]:= uw_normal;
  FixedSizes[$00C4]:= uw_normal;
  FixedSizes[$00D6]:= uw_normal;
  FixedSizes[$00DC]:= uw_normal;

  //Greek and Coptic
  // https://unicode.org/charts/PDF/U0370.pdf
  for i:= $370 to $3ff do
    FixedSizes[i]:= uw_normal;

  //Chinese
  // http://www.unicode.org/versions/Unicode5.0.0/ch12.pdf#G12159
  for i:= $3400 to $9FFF do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $F900 to $FAFF do
    FixedSizes[i]:= uw_fullwidth;

  // http://www.rikai.com/library/kanjitables/kanji_codes.unicode.shtml
  //Japanese punctuation, Hiragana, Katakana
  for i:= $3000 to $30ff do
    FixedSizes[i]:= uw_fullwidth;
  //full-width Roman
  for i:= $ff00 to $ff5f do
    FixedSizes[i]:= uw_fullwidth;
  //half-width Katakana
  for i:= $ff60 to $ff9f do
    FixedSizes[i]:= uw_normal;
  for i:= $ffa0 to $ffef do
    FixedSizes[i]:= uw_fullwidth;

  //Arabic
  // https://en.wikipedia.org/wiki/Arabic_script_in_Unicode
  for i:= $0600 to $06FF do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $0750 to $077F do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $08A0 to $08FF do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $FB50 to $FDFF do
    FixedSizes[i]:= uw_fullwidth;
  for i:= $FE70 to $FEFF do
    FixedSizes[i]:= uw_fullwidth;

  //Hebrew
  // https://en.wikipedia.org/wiki/Hebrew_(Unicode_block)
  for i:= $0590 to $05FF do
    FixedSizes[i]:= uw_fullwidth;

  //Syriac...Khmer..general puctuation
  for i:= $700 to $206F do
    FixedSizes[i]:= uw_fullwidth;

  for i:= $2010 to $2027 do
    FixedSizes[i]:= uw_normal;
  for i:= $2030 to $203A do
    FixedSizes[i]:= uw_normal;
  //"Superscripts and Subscripts" block which goes from 0x2070 to 0x209F
  for i:= $2070 to $209F do
    FixedSizes[i]:= uw_normal;

  FixedSizes[$2026]:= uw_fullwidth; //ellipsis char should be full-width
  FixedSizes[$20AC]:= uw_normal;
  FixedSizes[$2122]:= uw_normal;

  //math operators
  for i:= $2200 to $22FF do
    FixedSizes[i]:= uw_fullwidth;

  //"Miscellaneous Symbols" block which goes from 0x2600 to 0x26FF
  for i:= $2600 to $26FF do
    FixedSizes[i]:= uw_fullwidth;

  //combining chars
  // https://en.wikipedia.org/wiki/Combining_character
  for i:=$0300 to $036F do
    FixedSizes[i]:= uw_combined;
  for i:=$483 to $489 do
    FixedSizes[i]:= uw_combined;
  for i:=$1AB0 to $1AFF do
    FixedSizes[i]:= uw_combined;
  for i:=$1DC0 to $1DFF do
    FixedSizes[i]:= uw_combined;
  for i:=$20D0 to $20FF do
    FixedSizes[i]:= uw_combined;
  for i:=$FE20 to $FE2F do
    FixedSizes[i]:= uw_combined;

  // https://www.compart.com/en/unicode/block/U+0E80
  FixedSizes[$eb1]:= uw_combined;
  FixedSizes[$eb4]:= uw_combined;
  for i:= $eb4 to $ebc do
    FixedSizes[i]:= uw_combined;
  for i:= $ec8 to $ecd do
    FixedSizes[i]:= uw_combined;

  FixedSizes[$3099]:= uw_combined;
  FixedSizes[$309A]:= uw_combined;

  //hex display
  // http://unicode.org/reports/tr9/#Directional_Formatting_Characters
  // https://en.wikipedia.org/wiki/Whitespace_character#Unicode
  for i:= 0 to $20-1 do //ascii control chars
    if i<>9 then
      FixedSizes[i]:= uw_hexshow;

  //for Unicode BiDi chars attack: https://www.opennet.ru/opennews/art.shtml?num=56083
  for i:= $2000 to $200F do //white spaces + specials
    FixedSizes[i]:= uw_hexshow;
  for i:= $2028 to $202F do  //white spaces + specials
    FixedSizes[i]:= uw_hexshow;
  for i:= $2066 to $2069 do
    FixedSizes[i]:= uw_hexshow;
  FixedSizes[$061C]:= uw_hexshow;

  FixedSizes[$85]:= uw_hexshow; //white space
  FixedSizes[$3164]:= uw_hexshow; //for Unicode attack: https://www.opennet.ru/opennews/art.shtml?num=56131
  FixedSizes[$01C3]:= uw_hexshow; //for Unicode attack: https://www.opennet.ru/opennews/art.shtml?num=56131
  FixedSizes[$FEFF]:= uw_hexshow;

  //line-break chars
  FixedSizes[$0A]:= uw_space;
  FixedSizes[$0D]:= uw_space;

  //space chars
  FixedSizes[$9]:= uw_space;
  FixedSizes[$20]:= uw_space;
  FixedSizes[$A0]:= uw_space; //no-break space, NBSP, often used on macOS
  FixedSizes[$1680]:= uw_space; //white space
  FixedSizes[$2007]:= uw_space; //figure space
  FixedSizes[$200B]:= uw_space; //zero width space https://en.wikipedia.org/wiki/Zero-width_space
  FixedSizes[$202F]:= uw_space; //narrow no-break space
  FixedSizes[$205F]:= uw_space; //white space
  FixedSizes[$2060]:= uw_space; //white space
  FixedSizes[$3000]:= uw_space; //CJK white space

end;

initialization

  InitFixedSizes;

end.

