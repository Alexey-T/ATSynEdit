{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_LineParts;

{$mode objfpc}{$H+}
{$ScopedEnums on}

interface

uses
  SysUtils, Graphics;

type
  TATLineStyle = (
    None,
    Solid,
    Dash,
    Solid2px,
    Dotted,
    Rounded,
    Wave
    );

const
  afsFontBold = 1;
  afsFontItalic = 2;
  afsFontCrossed = 4;

type
  //bitpacked: SizeOf=24 bytes
  TATLinePart = bitpacked record
    Offset: Longint; //4 bytes, 2 bytes are not enough (app will crash on line length 120K, with wrap=off)
    Len: Longint; //Word is enough, but Longint makes SizeOf=24 bytes, better
    case Boolean of
      false: (
        ColorFont, ColorBG, ColorBorder: TColor;
        FontStyles: byte;
        Dummy: byte; //to align SizeOf to 24 bytes
        BorderUp, BorderDown, BorderLeft, BorderRight: TATLineStyle;
        );
      true:
        ( QWord1, QWord2: QWord );
    end;
  PATLinePart = ^TATLinePart;

procedure InitLinePart(out Part: TATLinePart);

const
  cMaxLineParts = 230;
type
  TATLineParts = array[0..cMaxLineParts-1] of TATLinePart;
  PATLineParts = ^TATLineParts;


function DoPartsHaveSameStyles(var A, B: TATLinePart): boolean; inline;
procedure DoPartFind(var P: TATLineParts; APos: integer; out AIndex, AOffsetLeft: integer);
function DoPartInsert(var AParts: TATLineParts; var APart: TATLinePart; AKeepFontStyles, AMainText: boolean): boolean;
procedure DoPartSetColorBG(var P: TATLineParts; AColor: TColor; AForceColor: boolean);

function DoPartsGetCount(var P: TATLineParts): integer;
function DoPartsGetLastCachedChar(var P: TATLineParts): integer;
function DoPartsShow(var P: TATLineParts): string;
procedure DoPartsDim(var P: TATLineParts; ADimLevel255: integer; AColorBG: TColor);
procedure DoPartsCutFromOffset(var P: TATLineParts; AOffset: integer);

//IME inline composition support
function DoPartsInsertGap(var P: TATLineParts; APos, ALen: integer;
  ADefaultFontColor, ADefaultBG: TColor): boolean;
procedure DoPartsMarkImeInline(var P: TATLineParts; APos, ALen: integer;
  const AAttrs: array of byte;
  AColorUnderline, AColorTargetBG, AColorTargetFont: TColor; AMainText: boolean);

function ColorBlend(c1, c2: Longint; A: Longint): Longint;
function ColorBlendHalf(c1, c2: Longint): Longint;

function ConvertFontStylesToInteger(Styles: TFontStyles): integer;
function ConvertIntegerToFontStyles(Value: integer): TFontStyles;

implementation

function ColorBlend(c1, c2: Longint; A: Longint): Longint;
//blend level: 0..255
var
  r, g, b, v1, v2: byte;
begin
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= Byte(A * (v1 - v2) shr 8 + v2); //typecast is required, to avoid range-error in $R+
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= Byte(A * (v1 - v2) shr 8 + v2);
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= Byte(A * (v1 - v2) shr 8 + v2);
  Result := Longint(b shl 16) + (g shl 8) + r;
end;

function ColorBlendHalf(c1, c2: Longint): Longint;
var
  r, g, b, v1, v2: byte;
begin
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= Byte((v1+v2) shr 1);
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= Byte((v1+v2) shr 1);
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= Byte((v1+v2) shr 1);
  Result := Longint(b shl 16) + (g shl 8) + r;
end;

procedure InitLinePart(out Part: TATLinePart);
begin
  Part:= Default(TATLinePart);
  Part.ColorBG:= clNone;
  Part.ColorFont:= clNone;
  Part.ColorBorder:= clNone;
end;

function DoPartsHaveSameStyles(var A, B: TATLinePart): boolean;
begin
  Result:=
    (A.QWord1=B.QWord1) and
    (A.QWord2=B.QWord2);
end;

procedure DoPartFind(var P: TATLineParts; APos: integer; out AIndex,
  AOffsetLeft: integer);
var
  iStart, iEnd, i: integer;
begin
  AIndex:= -1;
  AOffsetLeft:= 0;

  for i:= Low(P) to High(P)-1 do
  begin
    if P[i].Len=0 then
    begin
      //pos after last part?
      if i>Low(P) then
        if APos>=P[i-1].Offset+P[i-1].Len then
          AIndex:= i;
      Break;
    end;

    iStart:= P[i].Offset;
    iEnd:= iStart+P[i].Len;

    //pos at part begin?
    if (APos=iStart) then
    begin
      AIndex:= i;
      Break
    end;

    //pos at part middle?
    if (APos>=iStart) and (APos<iEnd) then
    begin
      AIndex:= i;
      AOffsetLeft:= APos-iStart;
      Break
    end;
  end;
end;


function DoPartsGetTotalLen(constref AParts: TATLineParts): integer;
var
  N: integer;
begin
  N:= 0;
  while (N<=High(AParts)) and (AParts[N].Len>0) do Inc(N);
  if N=0 then
    Result:= 0
  else
    Result:= AParts[N-1].Offset+AParts[N-1].Len;
end;

function DoPartsGetCount(var P: TATLineParts): integer;
//func considers case when some middle part has Len=0
begin
  Result:= High(P)+1;
  while (Result>0) and (P[Result-1].Len=0) do
    Dec(Result);
end;

function DoPartsGetLastCachedChar(var P: TATLineParts): integer;
var
  N: integer;
begin
  N:= DoPartsGetCount(P);
  if N<=0 then
    Result:= 0
  else
    Result:= P[N-1].Offset+P[N-1].Len;
end;


var
  //size is huge, so it's global var
  //local var decreases paint speed by ~5 msec per paint
  //boolean index is to avoid conflict between main thread / minimap thread; use AMainText here
  ResultParts: array[boolean] of TATLineParts;

function DoPartInsert(var AParts: TATLineParts; var APart: TATLinePart;
  AKeepFontStyles, AMainText: boolean): boolean;
var
  ResultPartIndex: integer;
  //
  procedure AddPart(constref P: TATLinePart); inline;
  begin
    if P.Len>0 then
      if ResultPartIndex<High(TATLineParts) then
      begin
        Move(P, ResultParts[AMainText][ResultPartIndex], SizeOf(TATLinePart));
        Inc(ResultPartIndex);
      end;
  end;
  //
  procedure FixPartLen(var P: TATLinePart; NOffsetEnd: integer); inline;
  begin
    if P.Len>NOffsetEnd-P.Offset then
      P.Len:= NOffsetEnd-P.Offset;
  end;
  //
var
  PartSelBegin, PartSelEnd: TATLinePart;
  ColorFontLeft, ColorFontRight: TColor;
  nIndex1, nIndex2,
  nOffset1, nOffset2, nOffsetLimit,
  newLen2, newOffset2: integer;
  i: integer;
begin
  Result:= false;

  //if editor scrolled to right, passed parts have Offset<0,
  //shrink such parts
  if (APart.Offset<0) and (APart.Offset+APart.Len>0) then
  begin
    Inc(APart.Len, APart.Offset);
    APart.Offset:= 0;
  end;

  DoPartFind(AParts, APart.Offset, nIndex1, nOffset1);
  DoPartFind(AParts, APart.Offset+APart.Len, nIndex2, nOffset2);
  if nIndex1<0 then Exit;
  if nIndex2<0 then Exit;

  //if ColorBG=clNone, use ColorBG of previous part at that position
  //tested on URLs in JS inside HTML
  if APart.ColorBG=clNone then
    APart.ColorBG:= AParts[nIndex1].ColorBG; //clYellow;

  if APart.ColorFont<>clNone then
  begin
    ColorFontLeft:= APart.ColorFont;
    ColorFontRight:= APart.ColorFont;
  end
  else
  begin
    ColorFontLeft:= AParts[nIndex1].ColorFont;
    ColorFontRight:= AParts[nIndex2].ColorFont;
  end;

  //these 2 parts are for edges of selection
  PartSelBegin:= Default(TATLinePart);
  PartSelEnd:= Default(TATLinePart);

  PartSelBegin.ColorFont:= ColorFontLeft;
  PartSelBegin.ColorBG:= APart.ColorBG;

  PartSelBegin.Offset:= AParts[nIndex1].Offset+nOffset1;
  PartSelBegin.Len:= AParts[nIndex1].Len-nOffset1;

  PartSelBegin.FontStyles:= AParts[nIndex1].FontStyles;
  PartSelBegin.BorderDown:= AParts[nIndex1].BorderDown;
  PartSelBegin.BorderLeft:= AParts[nIndex1].BorderLeft;
  PartSelBegin.BorderRight:= AParts[nIndex1].BorderRight;
  PartSelBegin.BorderUp:= AParts[nIndex1].BorderUp;
  PartSelBegin.ColorBorder:= AParts[nIndex1].ColorBorder;

  PartSelEnd.ColorFont:= ColorFontRight;
  PartSelEnd.ColorBG:= APart.ColorBG;
  PartSelEnd.Offset:= AParts[nIndex2].Offset;
  PartSelEnd.Len:= nOffset2;
  PartSelEnd.FontStyles:= AParts[nIndex2].FontStyles;
  PartSelEnd.BorderDown:= AParts[nIndex2].BorderDown;
  PartSelEnd.BorderLeft:= AParts[nIndex2].BorderLeft;
  PartSelEnd.BorderRight:= AParts[nIndex2].BorderRight;
  PartSelEnd.BorderUp:= AParts[nIndex2].BorderUp;
  PartSelEnd.ColorBorder:= AParts[nIndex2].ColorBorder;

  with AParts[nIndex2] do
  begin
    newLen2:= Len-nOffset2;
    newOffset2:= Offset+nOffset2;
  end;

  ResultParts[AMainText]:= Default(TATLineParts);
  ResultPartIndex:= 0;

  //add parts before selection
  for i:= 0 to nIndex1-1 do
    AddPart(AParts[i]);
  if nOffset1>0 then
  begin
    FixPartLen(AParts[nIndex1], APart.Offset);
    AddPart(AParts[nIndex1]);
  end;

  //add middle (one APart of many parts)
  if not AKeepFontStyles then
  begin
    if APart.ColorFont=clNone then //fix CudaText issue #3571, #3574
      APart.ColorFont:= ColorFontLeft;
    AddPart(APart);
  end
  else
  begin
    nOffsetLimit:= APart.Offset+APart.Len;
    FixPartLen(PartSelBegin, nOffsetLimit);
    AddPart(PartSelBegin);

    for i:= nIndex1+1 to nIndex2-1 do
    begin
      AParts[i].ColorFont:= ColorFontLeft;
      AParts[i].ColorBG:= APart.ColorBG;
      FixPartLen(AParts[i], nOffsetLimit);
      AddPart(AParts[i]);
    end;

    if nIndex1<nIndex2 then
    begin
      FixPartLen(PartSelEnd, nOffsetLimit);
      AddPart(PartSelEnd);
    end;
  end;

  //add parts after selection
  if nOffset2>0 then
  begin
    AParts[nIndex2].Len:= newLen2;
    AParts[nIndex2].Offset:= newOffset2;
  end;

  for i:= nIndex2 to High(AParts) do
  begin
    if AParts[i].Len=0 then Break;
    AddPart(AParts[i]);
  end;

  Move(ResultParts[AMainText], AParts, SizeOf(TATLineParts));
  Result:= true;
end;


procedure DoPartSetColorBG(var P: TATLineParts; AColor: TColor; AForceColor: boolean);
var
  PartPtr: PATLinePart;
  i: integer;
begin
  for i:= Low(P) to High(P) do
  begin
    PartPtr:= @P[i];
    if PartPtr^.Len=0 then Break; //comment to colorize all parts to hide possible bugs
    if AForceColor or (PartPtr^.ColorBG=clNone) then
      PartPtr^.ColorBG:= AColor;
  end;
end;

function DoPartsShow(var P: TATLineParts): string;
var
  i: integer;
begin
  Result:= '';
  for i:= Low(P) to High(P) do
  begin
    if P[i].Len=0 then break;
    Result+= Format('[%d %d]', [P[i].Offset, P[i].Len]);
  end;
end;


procedure DoPartsDim(var P: TATLineParts; ADimLevel255: integer; AColorBG: TColor);
var
  i: integer;
begin
  for i:= Low(P) to High(P) do
  begin
    if P[i].Len=0 then break;
    with P[i] do
    begin
      ColorFont:= ColorBlend(ColorBG, ColorFont, ADimLevel255);
      if ColorBG<>clNone then
        ColorBG:= ColorBlend(AColorBG, ColorBG, ADimLevel255);
      if ColorBorder<>clNone then
        ColorBorder:= ColorBlend(ColorBG, ColorBorder, ADimLevel255);
    end;
  end;
end;

procedure DoPartsCutFromOffset(var P: TATLineParts; AOffset: integer);
var
  NCount, N, i: integer;
  PartPtr: PATLinePart;
begin
  NCount:= DoPartsGetCount(P);

  //how many parts to delete from start
  N:= 0;
  repeat
    PartPtr:= @P[N];
    if PartPtr^.Len<=0 then Break;
    if PartPtr^.Offset+PartPtr^.Len>AOffset then Break;
    Inc(N);
  until false;

  //shift all parts by N items
  for i:= 0 to NCount-N-1 do
    P[i]:= P[i+N];

  //fill tail with zeros
  for i:= NCount-N to NCount-1 do
    P[i]:= Default(TATLinePart);

  for i:= 0 to NCount-1-N do
    with P[i] do
      if Len>0 then
      begin
        Dec(Offset, AOffset);
        if Offset<0 then
        begin
          Inc(Len, Offset);
          Offset:= 0;
        end;
      end;
end;


function DoPartsInsertGap(var P: TATLineParts; APos, ALen: integer;
  ADefaultFontColor, ADefaultBG: TColor): boolean;
//Makes a gap of ALen chars at position APos: parts after APos are shifted,
//a part crossing APos is split, and the gap is filled with a new part
//(copy of the left neighbour, without borders/font styles).
//If the line has no parts, APos>0 makes a base part for the chars before APos.
var
  NCount, iSplit, iGap, NEnd, k: integer;
  Gap: TATLinePart;
begin
  Result:= false;
  if ALen<=0 then exit;
  NCount:= DoPartsGetCount(P);

  if NCount=0 then
  begin
    //empty line (or APos>0 after padding with spaces, virtual caret)
    InitLinePart(Gap);
    Gap.Offset:= 0;
    Gap.ColorFont:= ADefaultFontColor;
    Gap.ColorBG:= ADefaultBG;
    if APos=0 then
    begin
      Gap.Len:= ALen;
      P[0]:= Gap;
      exit(true);
    end;
    //base part for chars before APos, the gap part is added below
    Gap.Len:= APos;
    P[0]:= Gap;
    NCount:= 1;
  end
  else
  begin
    //parts end before APos: text was padded with spaces (virtual caret), extend the last part
    NEnd:= P[NCount-1].Offset+P[NCount-1].Len;
    if NEnd<APos then
      P[NCount-1].Len:= P[NCount-1].Len+(APos-NEnd);
  end;

  //reserve: split (+1), gap (+1), parts added later by DoPartInsert
  if NCount+8>=cMaxLineParts then exit;

  //1) split the part which crosses APos
  iSplit:= -1;
  for k:= 0 to NCount-1 do
    if (P[k].Offset<APos) and (P[k].Offset+P[k].Len>APos) then
    begin
      iSplit:= k;
      Break;
    end;
  if iSplit>=0 then
  begin
    for k:= NCount-1 downto iSplit+1 do
      P[k+1]:= P[k];
    P[iSplit+1]:= P[iSplit];
    NEnd:= P[iSplit].Offset+P[iSplit].Len;
    P[iSplit].Len:= APos-P[iSplit].Offset;
    P[iSplit+1].Offset:= APos;
    P[iSplit+1].Len:= NEnd-APos;
    Inc(NCount);
  end;

  //2) shift parts which are at/after APos; iGap = index for the gap part
  iGap:= 0;
  for k:= 0 to NCount-1 do
    if P[k].Offset>=APos then
      P[k].Offset:= P[k].Offset+ALen
    else
      iGap:= k+1;

  //3) make room for the gap part and fill it
  for k:= NCount-1 downto iGap do
    P[k+1]:= P[k];
  if iGap>0 then
    Gap:= P[iGap-1]
  else
    Gap:= P[iGap+1];
  Gap.Offset:= APos;
  Gap.Len:= ALen;
  Gap.FontStyles:= 0;
  Gap.ColorBorder:= clNone;
  Gap.BorderUp:= TATLineStyle.None;
  Gap.BorderDown:= TATLineStyle.None;
  Gap.BorderLeft:= TATLineStyle.None;
  Gap.BorderRight:= TATLineStyle.None;
  P[iGap]:= Gap;
  Result:= true;
end;

procedure DoPartsMarkImeInline(var P: TATLineParts; APos, ALen: integer;
  const AAttrs: array of byte;
  AColorUnderline, AColorTargetBG, AColorTargetFont: TColor; AMainText: boolean);
//Decorates the gap made by DoPartsInsertGap: underline for all chars,
//highlight for "target" clauses. One part per run of equal attributes.
  //
  function AttrAt(n: integer): byte;
  begin
    if n<=High(AAttrs) then
      Result:= AAttrs[n]
    else
      Result:= 0;
  end;
  //
var
  i, iStart: integer;
  NAttr: byte;
  Part: TATLinePart;
begin
  i:= 0;
  while i<ALen do
  begin
    iStart:= i;
    NAttr:= AttrAt(i);
    while (i<ALen) and (AttrAt(i)=NAttr) do
      Inc(i);

    InitLinePart(Part);
    Part.Offset:= APos+iStart;
    Part.Len:= i-iStart;
    Part.ColorBorder:= AColorUnderline;
    if (NAttr=1) or (NAttr=3) then //ATTR_TARGET_CONVERTED, ATTR_TARGET_NOTCONVERTED
    begin
      Part.ColorFont:= AColorTargetFont;
      Part.ColorBG:= AColorTargetBG;
      Part.BorderDown:= TATLineStyle.Solid2px;
    end
    else
      Part.BorderDown:= TATLineStyle.Solid;

    //ColorFont=clNone: DoPartInsert takes the font color of the left part
    DoPartInsert(P, Part, false, AMainText);
  end;
end;


function ConvertFontStylesToInteger(Styles: TFontStyles): integer;
begin
  Result:= 0;
  if fsBold in Styles then
    Result:= Result or afsFontBold;
  if fsItalic in Styles then
    Result:= Result or afsFontItalic;
  if fsStrikeOut in Styles then
    Result:= Result or afsFontCrossed;
end;

function ConvertIntegerToFontStyles(Value: integer): TFontStyles;
begin
  Result:= [];
  if (Value and afsFontBold)<>0 then
    Include(Result, fsBold);
  if (Value and afsFontItalic)<>0 then
    Include(Result, fsItalic);
  if (Value and afsFontCrossed)<>0 then
    Include(Result, fsStrikeOut);
end;

{
initialization
  WriteLn('SizeOf(TATLinePart): '+IntToStr(SizeOf(TATLinePart)));
  WriteLn('SizeOf(TATLineParts): '+IntToStr(SizeOf(TATLineParts)), ', ', IntToStr(SizeOf(TATLineParts) div cMaxLineParts), ' bytes per part');
}

end.
