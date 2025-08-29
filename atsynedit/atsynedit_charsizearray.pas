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

procedure Fill(IndexFrom, IndexTo: word; Value: byte); inline;
begin
  FillChar(FixedSizes[IndexFrom], IndexTo-IndexFrom+1, Value);
end;

procedure InitFixedSizes;
begin
  //Basic Latin
  //Latin-1 Supplement
  //Latin Extended-A
  Fill($20, $24F, uw_normal);

  //IPA Extensions
  Fill($250, $2D7, uw_normal);

  //some Latin are full-width
  Fill($1C4, $1CC, uw_fullwidth);
  FixedSizes[$1F1]:= uw_fullwidth;
  FixedSizes[$1F2]:= uw_fullwidth;
  FixedSizes[$1F3]:= uw_fullwidth;
  FixedSizes[$24A]:= uw_fullwidth;

  //Cyrillic, https://www.compart.com/en/unicode/block/U+0400
  Fill($400, $45A, uw_normal);
  Fill($45B, $45F, uw_normal);
  Fill($460, $461, uw_fullwidth);
  Fill($462, $4FF, uw_normal);

  Fill($500, $52F, uw_fullwidth);
  Fill($2DE0, $2DFF, uw_fullwidth);
  Fill($A640, $A69F, uw_fullwidth);
  Fill($1C80, $1C8F, uw_fullwidth);
  Fill($1D2B, $1D78, uw_fullwidth);

  //German, https://resources.german.lsa.umich.edu/schreiben/unicode/
  FixedSizes[$00DF]:= uw_normal;
  FixedSizes[$00E4]:= uw_normal;
  FixedSizes[$00F6]:= uw_normal;
  FixedSizes[$00FC]:= uw_normal;
  FixedSizes[$00C4]:= uw_normal;
  FixedSizes[$00D6]:= uw_normal;
  FixedSizes[$00DC]:= uw_normal;

  //Greek and Coptic, https://unicode.org/charts/PDF/U0370.pdf
  Fill($370, $3ff, uw_normal);

  //Chinese, http://www.unicode.org/versions/Unicode5.0.0/ch12.pdf#G12159
  Fill($3400, $9FFF, uw_fullwidth);
  Fill($F900, $FAFF, uw_fullwidth);

  //Japanese punctuation, Hiragana, Katakana, http://www.rikai.com/library/kanjitables/kanji_codes.unicode.shtml
  Fill($3000, $30ff, uw_fullwidth);
  //full-width Roman
  Fill($ff00, $ff5f, uw_fullwidth);
  //half-width Katakana
  Fill($ff60, $ff9f, uw_normal);
  Fill($ffa0, $ffef, uw_fullwidth);

  //Hebrew, https://en.wikipedia.org/wiki/Hebrew_(Unicode_block)
  Fill($0590, $05FF, uw_fullwidth);
  //Arabic, https://en.wikipedia.org/wiki/Arabic_script_in_Unicode
  Fill($0600, $06FF, uw_fullwidth);
  //Syriac
  Fill($0700, $074F, uw_fullwidth);
  Fill($0750, $077F, uw_fullwidth);
  Fill($08A0, $08FF, uw_fullwidth);
  //Khmer
  Fill($1780, $17FF, uw_fullwidth);
  Fill($FB50, $FDFF, uw_fullwidth);
  Fill($FE70, $FEFF, uw_fullwidth);

  {
  //Georgian, better to have the autodetect
  Fill($10A0, $10FF, uw_normal);
  //one Georgian char is fullwidth
  FixedSizes[$10DA]:= uw_fullwidth;
  }

  //Latin extended additional
  Fill($1EA0, $1EF9, uw_normal);

  //Greek extended
  Fill($1F00, $1FFF, uw_normal);

  //General punctuation range, https://www.unicodepedia.com/groups/general-punctuation/
  Fill($2010, $203A, uw_normal);
  Fill($203B, $206F, uw_fullwidth);
  FixedSizes[$203C]:= uw_normal;
  FixedSizes[$2044]:= uw_normal;

  //"Superscripts and Subscripts" block
  Fill($2070, $209F, uw_normal);

  FixedSizes[$20AC]:= uw_normal;
  FixedSizes[$2122]:= uw_normal;

  //math operators
  { //let's not force widths of math chars - see CudaText issue #3873, where user asked to force them
  Fill($2200, $22FF, uw_fullwidth);
  FixedSizes[$2200]:= uw_normal;
  FixedSizes[$2203]:= uw_normal;
  FixedSizes[$2205]:= uw_normal;
  FixedSizes[$2208]:= uw_normal;
  FixedSizes[$2209]:= uw_normal;
  FixedSizes[$221a]:= uw_normal;
  FixedSizes[$2227]:= uw_normal;
  FixedSizes[$2228]:= uw_normal;
  FixedSizes[$222A]:= uw_normal;
  FixedSizes[$2248]:= uw_normal;
  FixedSizes[$2260]:= uw_normal;
  FixedSizes[$2264]:= uw_normal;
  FixedSizes[$2265]:= uw_normal;
  FixedSizes[$2282]:= uw_normal;
  FixedSizes[$2283]:= uw_normal;
  FixedSizes[$2286]:= uw_normal;
  FixedSizes[$2287]:= uw_normal;
  FixedSizes[$22C2]:= uw_normal;
  }

  //"Miscellaneous Symbols" block which goes from 0x2600 to 0x26FF
  Fill($2600, $265F, uw_fullwidth);
  Fill($2668, $26FF, uw_fullwidth);

  //------------------------------------
  //combining chars
  // https://en.wikipedia.org/wiki/Combining_character
  Fill($0300, $036F, uw_combined);
  Fill($483, $489, uw_combined);
  Fill($1AB0, $1AFF, uw_combined);
  Fill($1DC0, $1DFF, uw_combined);
  Fill($20D0, $20FF, uw_combined);
  Fill($FE20, $FE2F, uw_combined);

  // https://www.compart.com/en/unicode/block/U+0E80
  FixedSizes[$eb1]:= uw_combined;
  FixedSizes[$eb4]:= uw_combined;
  Fill($eb4, $ebc, uw_combined);
  Fill($ec8, $ecd, uw_combined);

  FixedSizes[$3099]:= uw_combined;
  FixedSizes[$309A]:= uw_combined;

  //------------------------------------
  //hex display
  // http://unicode.org/reports/tr9/#Directional_Formatting_Characters
  // https://en.wikipedia.org/wiki/Whitespace_character#Unicode
  Fill(0, $20-1, uw_hexshow);
  FixedSizes[9]:= uw_space;

  //for Unicode BiDi chars attack: https://www.opennet.ru/opennews/art.shtml?num=56083
  Fill($2000, $200F, uw_hexshow);
  Fill($2028, $202F, uw_hexshow);
  Fill($2066, $2069, uw_hexshow);
  FixedSizes[$061C]:= uw_hexshow;

  FixedSizes[$85]:= uw_hexshow; //white space
  FixedSizes[$AD]:= uw_hexshow; //Sublime shows it as hex code
  FixedSizes[$3164]:= uw_hexshow; //for Unicode attack: https://www.opennet.ru/opennews/art.shtml?num=56131
  FixedSizes[$01C3]:= uw_hexshow; //for Unicode attack: https://www.opennet.ru/opennews/art.shtml?num=56131
  FixedSizes[$FEFF]:= uw_hexshow;

  //------------------------------------
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

