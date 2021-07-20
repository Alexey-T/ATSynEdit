{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_LineParts;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics;

type
  TATLineStyle = (
    cLineStyleNone,
    cLineStyleSolid,
    cLineStyleDash,
    cLineStyleSolid2px,
    cLineStyleDotted,
    cLineStyleRounded,
    cLineStyleWave
    );

const
  afsFontBold = 1;
  afsFontItalic = 2;
  afsFontCrossed = 4;

type
  TATLinePart = packed record
    Offset: SmallInt; //2 bytes
    Len: word; //2 bytes
    ColorFont, ColorBG, ColorBorder: TColor;
    FontStyles: byte;
    BorderUp, BorderDown, BorderLeft, BorderRight: TATLineStyle;
  end;
  PATLinePart = ^TATLinePart;

type
  TATLinePartClass = class
  public
    Data: TATLinePart;
    ColumnTag: Int64;
  end;

const
  cMaxLineParts = 210;
type
  TATLineParts = array[0..cMaxLineParts-1] of TATLinePart;
  PATLineParts = ^TATLineParts;

procedure DoPartFind(constref AParts: TATLineParts; APos: integer; out AIndex, AOffsetLeft: integer);
function DoPartInsert(var AParts: TATLineParts; var APart: TATLinePart; AKeepFontStyles: boolean): boolean;
procedure DoPartSetColorBG(var AParts: TATLineParts; AColor: TColor; AForceColor: boolean);

function DoPartsGetCount(constref AParts: TATLineParts): integer;
function DoPartsShow(var P: TATLineParts): string;
procedure DoPartsDim(var P: TATLineParts; ADimLevel255: integer; AColorBG: TColor);
procedure DoPartsCutFromOffset(var P: TATLineParts; AOffset: integer);

function ColorBlend(c1, c2: Longint; A: Longint): Longint;
function ColorBlendHalf(c1, c2: Longint): Longint;

implementation

function ColorBlend(c1, c2: Longint; A: Longint): Longint;
//blend level: 0..255
var
  r, g, b, v1, v2: byte;
begin
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= A * (v1 - v2) shr 8 + v2;
  Result := (b shl 16) + (g shl 8) + r;
end;

function ColorBlendHalf(c1, c2: Longint): Longint;
var
  r, g, b, v1, v2: byte;
begin
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= (v1+v2) shr 1;
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= (v1+v2) shr 1;
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= (v1+v2) shr 1;
  Result := (b shl 16) + (g shl 8) + r;
end;


procedure DoPartFind(constref AParts: TATLineParts; APos: integer; out AIndex,
  AOffsetLeft: integer);
var
  iStart, iEnd, i: integer;
begin
  AIndex:= -1;
  AOffsetLeft:= 0;

  for i:= Low(AParts) to High(AParts)-1 do
  begin
    if AParts[i].Len=0 then
    begin
      //pos after last part?
      if i>Low(AParts) then
        if APos>=AParts[i-1].Offset+AParts[i-1].Len then
          AIndex:= i;
      Break;
    end;

    iStart:= AParts[i].Offset;
    iEnd:= iStart+AParts[i].Len;

    //pos at part begin?
    if (APos=iStart) then
      begin AIndex:= i; Break end;

    //pos at part middle?
    if (APos>=iStart) and (APos<iEnd) then
      begin AIndex:= i; AOffsetLeft:= APos-iStart; Break end;
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

function DoPartsGetCount(constref AParts: TATLineParts): integer;
//func considers case when some middle part has Len=0
begin
  Result:= High(AParts)+1;
  while (Result>0) and (AParts[Result-1].Len=0) do
    Dec(Result);
end;

var
  ResultParts: TATLineParts; //size is huge, so not local var

function DoPartInsert(var AParts: TATLineParts; var APart: TATLinePart;
  AKeepFontStyles: boolean): boolean;
var
  ResultPartIndex: integer;
  //
  procedure AddPart(constref P: TATLinePart); inline;
  begin
    if P.Len>0 then
      if ResultPartIndex<High(ResultParts) then
      begin
        Move(P, ResultParts[ResultPartIndex], SizeOf(P));
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
  FillChar(PartSelBegin{%H-}, SizeOf(TATLinePart), 0);
  FillChar(PartSelEnd{%H-}, SizeOf(TATLinePart), 0);

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

  FillChar(ResultParts, SizeOf(ResultParts), 0);
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

  Move(ResultParts, AParts, SizeOf(AParts));
  Result:= true;
end;


procedure DoPartSetColorBG(var AParts: TATLineParts; AColor: TColor;
  AForceColor: boolean);
var
  PartPtr: PATLinePart;
  i: integer;
begin
  for i:= Low(AParts) to High(AParts) do
  begin
    PartPtr:= @AParts[i];
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
    FillChar(P[i], SizeOf(TATLinePart), 0);

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


end.
