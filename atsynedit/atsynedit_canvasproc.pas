{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CanvasProc;

{$mode objfpc}{$H+}
{$MinEnumSize 1}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, Graphics, Types,
  Forms,
  ATCanvasPrimitives,
  ATStringProc,
  ATStrings,
  ATSynEdit_CharSizer;

var
  OptUnprintedTabCharLength: integer = 1;
  OptUnprintedTabPointerScale: integer = 22;
  OptUnprintedEofCharLength: integer = 1;
  OptUnprintedSpaceDotScale: integer = 15;
  OptUnprintedEndDotScale: integer = 30;
  OptUnprintedEndFontScale: integer = 80;
  OptUnprintedEndFontDx: integer = 3;
  OptUnprintedEndFontDy: integer = 2;
  OptUnprintedEndArrowOrDot: boolean = true;
  OptUnprintedEndArrowLength: integer = 70;
  OptUnprintedWrapArrowLength: integer = 40;
  OptUnprintedWrapArrowWidth: integer = 80;
  OptItalicFontLongerInPercents: integer = 40;

const
  //Win: seems no slowdown from offsets
  //macOS: better use offsets, fonts have floating width value, e.g. 10.2 pixels
  //Linux gtk2: big slowdown from offsets
  CanvasTextOutMustUseOffsets =
    {$ifdef windows}
    true
    {$else}
      {$ifdef darwin}
      true
      {$else}
      false
      {$endif}
    {$endif} ;

type
  TATSynEditCallbackIsCharSelected = function(AX, AY: integer): boolean of object;

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

  TATFontNeedsOffsets = record
    ForNormal: boolean;
    ForBold: boolean;
    ForItalic: boolean;
    ForBoldItalic: boolean;
  end;

type
  TATLinePart = packed record
    Offset, Len: integer;
    ColorFont, ColorBG, ColorBorder: TColor;
    FontBold, FontItalic, FontStrikeOut: ByteBool;
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

type
  TATSynEditDrawLineEvent = procedure(Sender: TObject; C: TCanvas;
    AX, AY: integer; const AStr: atString; ACharSize: TPoint;
    const AExtent: TATIntArray) of object;

type
  TATCanvasTextOutProps = record
    SuperFast: boolean;
    NeedOffsets: TATFontNeedsOffsets;
    TabHelper: TATStringTabHelper;
    LineIndex: integer;
    CharIndexInLine: integer;
    CharSize: TPoint;
    MainTextArea: boolean;
    CharsSkipped: integer;
    DrawEvent: TATSynEditDrawLineEvent;
    ControlWidth: integer;
    TextOffsetFromLine: integer;
    ShowUnprinted: boolean;
    ShowUnprintedSpacesTrailing: boolean;
    ShowUnprintedSpacesBothEnds: boolean;
    ShowUnprintedSpacesOnlyInSelection: boolean;
    ShowFontLigatures: boolean;
    ColorUnprintedFont: TColor;
    ColorUnprintedHexFont: TColor;
    FontNormal_Name: string;
    FontNormal_Size: integer;
    FontItalic_Name: string;
    FontItalic_Size: integer;
    FontBold_Name: string;
    FontBold_Size: integer;
    FontBoldItalic_Name: string;
    FontBoldItalic_Size: integer;
    DetectIsPosSelected: TATSynEditCallbackIsCharSelected;
  end;

procedure CanvasLineEx(C: TCanvas;
  Color: TColor; Style: TATLineStyle;
  X1, Y1, X2, Y2: integer; AtDown: boolean);

procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: string); inline;

procedure CanvasTextOut(C: TCanvas;
  APosX, APosY: integer;
  AText: atString;
  AParts: PATLineParts;
  out ATextWidth: integer;
  const AProps: TATCanvasTextOutProps
  );

procedure CanvasTextOutMinimap(C: TCanvas;
  const ARect: TRect;
  APos: TPoint;
  ACharSize: TPoint;
  ATabSize: integer;
  constref AParts: TATLineParts;
  AColorBG: TColor;
  const ALine: atString;
  AUsePixels: boolean
  );

procedure DoPaintUnprintedEolText(C: TCanvas;
  const AText: string;
  APoint: TPoint;
  AColorFont, AColorBG: TColor);

procedure DoPaintUnprintedEolArrow(C: TCanvas;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont: TColor);

procedure DoPaintUnprintedWrapMark(C: TCanvas;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont: TColor);

function CanvasTextWidth(const S: atString; ALineIndex: integer;
  ATabHelper: TATStringTabHelper; ACharWidth: integer): integer; inline;

procedure DoPartFind(constref AParts: TATLineParts; APos: integer; out AIndex, AOffsetLeft: integer);
function DoPartInsert(var AParts: TATLineParts; var APart: TATLinePart; AKeepFontStyles: boolean): boolean;
procedure DoPartSetColorBG(var AParts: TATLineParts; AColor: TColor; AForceColor: boolean);
procedure DoPartsShow(var P: TATLineParts);
procedure DoPartsDim(var P: TATLineParts; ADimLevel255: integer; AColorBG: TColor);

function ColorBlend(c1, c2: Longint; A: Longint): Longint;
function ColorBlendHalf(c1, c2: Longint): Longint;

implementation

uses
  Math,
  LCLType,
  LCLIntf;

function SRemoveHexDisplayedChars(const S: UnicodeString): UnicodeString;
var
  ch: WideChar;
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
  begin
    ch:= Result[i];
    if ch=#9 then
      Result[i]:= ' '
    else
    if IsCharHexDisplayed(ch) then
      Result[i]:= '?';
  end;
end;


{$ifdef windows}
procedure _TextOutSimple_Windows(DC: HDC;
  X, Y: Integer;
  Rect: PRect;
  Str: PChar;
  StrLen: integer); inline;
begin
  Windows.ExtTextOutA(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE, Rect, Str, StrLen, nil);
end;

//to draw font ligatures
function _TextOut_Windows(DC: HDC;
  X, Y: Integer;
  Rect: PRect;
  const Str: UnicodeString;
  Dx: PInteger;
  AllowLigatures: boolean
  ): boolean;
var
  CharPlaceInfo: GCP_RESULTSW;
  Glyphs: array of WideChar;
begin
  if AllowLigatures then
  begin
    ZeroMemory(@CharPlaceInfo, SizeOf(CharPlaceInfo));
    CharPlaceInfo.lStructSize:= SizeOf(CharPlaceInfo);
    SetLength(Glyphs, Length(Str));
    CharPlaceInfo.lpGlyphs:= @Glyphs[0];
    CharPlaceInfo.nGlyphs:= Length(Glyphs);

    if GetCharacterPlacementW(DC, PWChar(Str), Length(Str), 0, @CharPlaceInfo, GCP_LIGATE)<> 0 then
      Result:= Windows.ExtTextOutW(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE or ETO_GLYPH_INDEX, Rect, Pointer(Glyphs), Length(Glyphs), Dx)
    else
      Result:= Windows.ExtTextOutW(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE, Rect, PWChar(Str), Length(Str), Dx);
  end
  else
    Result:= Windows.ExtTextOutW(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE, Rect, PWChar(Str), Length(Str), Dx);
end;

{$else}
function _TextOut_Unix(DC: HDC;
  X, Y: Integer;
  Rect: PRect;
  const Str: string;
  Dx: PInteger
  ): boolean; inline;
begin
  Result:= ExtTextOut(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE, Rect, PChar(Str), Length(Str), Dx);
end;
{$endif}

procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: string); inline;
//the same as C.TextOut but works faster on Windows
begin
  {$ifdef windows}
  _TextOutSimple_Windows(C.Handle, X, Y, nil, PChar(S), Length(S));
  {$else}
  _TextOut_Unix(C.Handle, X, Y, nil, S, nil);
  {$endif}
end;

procedure CanvasUnprintedSpace(C: TCanvas; const ARect: TRect;
  AScale: integer; AFontColor: TColor); inline;
const
  cMinDotSize = 2;
var
  R: TRect;
  NSize: integer;
begin
  NSize:= Max(cMinDotSize, ARect.Height * AScale div 100);
  R.Left:= (ARect.Left+ARect.Right) div 2 - NSize div 2;
  R.Top:= (ARect.Top+ARect.Bottom) div 2 - NSize div 2;
  R.Right:= R.Left + NSize;
  R.Bottom:= R.Top + NSize;
  C.Pen.Color:= AFontColor;
  C.Brush.Color:= AFontColor;
  C.FillRect(R);
end;

procedure DoPaintUnprintedChar(
  C: TCanvas;
  ch: WideChar;
  AIndex: integer;
  const AOffsets: TATIntArray;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont: TColor); inline; //many params, many calls, so inline
var
  R: TRect;
begin
  if IsCharSpace(ch) then
  begin
    R.Left:= APoint.X;
    R.Right:= APoint.X;
    if AIndex>1 then
      Inc(R.Left, AOffsets[AIndex-2]);
    Inc(R.Right, AOffsets[AIndex-1]);

    R.Top:= APoint.Y;
    R.Bottom:= R.Top+ACharSize.Y;

    if ch<>#9 then
      CanvasUnprintedSpace(C, R, OptUnprintedSpaceDotScale, AColorFont)
    else
      CanvasArrowHorz(C, R,
        AColorFont,
        OptUnprintedTabCharLength*ACharSize.X,
        true,
        OptUnprintedTabPointerScale);
  end;
end;


procedure CanvasLine_WithEnd(C: TCanvas; X1, Y1, X2, Y2: integer); inline;
begin
  if Y1=Y2 then
    C.Line(X1, Y1, X2+1, Y2)
  else
    C.Line(X1, Y1, X2, Y2+1);
end;

procedure CanvasLineEx(C: TCanvas; Color: TColor; Style: TATLineStyle; X1, Y1, X2, Y2: integer; AtDown: boolean);
begin
  case Style of
    cLineStyleNone:
      exit;

    cLineStyleSolid:
      begin
        C.Pen.Color:= Color;
        CanvasLine_WithEnd(C, X1, Y1, X2, Y2);
      end;

    cLineStyleSolid2px:
      begin
        C.Pen.Color:= Color;
        CanvasLine_WithEnd(C, X1, Y1, X2, Y2);
        if Y1=Y2 then
        begin
          if AtDown then
            begin Dec(Y1); Dec(Y2) end
          else
            begin Inc(Y1); Inc(Y2) end;
        end
        else
        begin
          if AtDown then
            begin Dec(X1); Dec(X2) end
          else
            begin Inc(X1); Inc(X2) end;
        end;
        CanvasLine_WithEnd(C, X1, Y1, X2, Y2);
      end;

    cLineStyleDash:
      begin
        C.Pen.Color:= Color;
        C.Pen.Style:= psDot;
        CanvasLine_WithEnd(C, X1, Y1, X2, Y2);
        C.Pen.Style:= psSolid;
      end;

    cLineStyleDotted:
      CanvasLine_Dotted(C, Color, X1, Y1, X2, Y2);

    cLineStyleRounded:
      CanvasLine_RoundedEdge(C, Color, X1, Y1, X2, Y2, AtDown);

    cLineStyleWave:
      CanvasLine_WavyHorz(C, Color, X1, Y1, X2, Y2, AtDown);
  end;
end;


procedure DoPaintHexChars(C: TCanvas;
  const AString: atString;
  ADx: PIntegerArray;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont,
  AColorBg: TColor;
  ASuperFast: boolean);
const
  HexDigits: array[0..15] of char = '0123456789ABCDEF';
  HexDummyMark = '?';
  HexPrefix = 'x';
var
  Buf2, Buf4: array[0..5] of char;
  Buf: PChar;
  Value, HexLen: integer;
  R: TRect;
  ch: WideChar;
  iChar, j: integer;
begin
  if AString='' then Exit;

  FillChar(Buf2, SizeOf(Buf2), 0);
  FillChar(Buf4, SizeOf(Buf4), 0);
  Buf2[0]:= HexPrefix;
  Buf4[0]:= HexPrefix;

  for iChar:= 1 to Length(AString) do
  begin
    ch:= AString[iChar];
    if IsCharHexDisplayed(ch) then
    begin
      R.Left:= APoint.X;
      R.Right:= APoint.X;

      C.Font.Color:= AColorFont;
      C.Brush.Color:= AColorBg;

      if ASuperFast then
      begin
        CanvasTextOutSimplest(C, R.Left, R.Top, HexDummyMark);
        Continue;
      end;

      for j:= 0 to iChar-2 do
        Inc(R.Left, ADx^[j]);
      R.Right:= R.Left+ADx^[iChar-1];

      R.Top:= APoint.Y;
      R.Bottom:= R.Top+ACharSize.Y;

      Value:= Ord(ch);
      if Value>=$100 then
      begin
        HexLen:= 4;
        Buf:= @Buf4;
      end
      else
      begin
        HexLen:= 2;
        Buf:= @Buf2;
      end;

      for j:= 1 to HexLen do
      begin
        Buf[HexLen-j+1]:= HexDigits[Value and 15];
        Value:= Value shr 4;
      end;

      {$ifdef windows}
      _TextOutSimple_Windows(C.Handle, R.Left, R.Top, @R, Buf, HexLen+1);
      {$else}
      CanvasTextOutSimplest(C, R.Left, R.Top, StrPas(Buf));
      {$endif}
    end;
  end;
end;

procedure DoPaintUnprintedEolText(C: TCanvas;
  const AText: string;
  APoint: TPoint;
  AColorFont, AColorBG: TColor);
var
  NPrevSize: integer;
begin
  if AText='' then Exit;
  NPrevSize:= C.Font.Size;
  C.Font.Size:= NPrevSize * OptUnprintedEndFontScale div 100;
  C.Font.Color:= AColorFont;
  C.Brush.Color:= AColorBG;

  CanvasTextOutSimplest(C,
    APoint.X+OptUnprintedEndFontDx,
    APoint.Y+OptUnprintedEndFontDy,
    AText);

  C.Font.Size:= NPrevSize;
end;

procedure DoPaintUnprintedEolArrow(C: TCanvas;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont: TColor);
begin
  if OptUnprintedEndArrowOrDot then
    CanvasArrowDown(C,
      Rect(APoint.X, APoint.Y, APoint.X+ACharSize.X, APoint.Y+ACharSize.Y),
      AColorFont,
      OptUnprintedEndArrowLength,
      OptUnprintedTabPointerScale
      )
  else
    CanvasUnprintedSpace(C,
      Rect(APoint.X, APoint.Y, APoint.X+ACharSize.X, APoint.Y+ACharSize.Y),
      OptUnprintedEndDotScale,
      AColorFont);
end;

procedure DoPaintUnprintedWrapMark(C: TCanvas;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont: TColor);
begin
  CanvasArrowWrapped(C,
    Rect(APoint.X, APoint.Y, APoint.X+ACharSize.X, APoint.Y+ACharSize.Y),
    AColorFont,
    OptUnprintedWrapArrowLength,
    OptUnprintedWrapArrowWidth,
    OptUnprintedTabPointerScale
    )
end;


function CanvasTextWidth(const S: atString; ALineIndex: integer;
  ATabHelper: TATStringTabHelper; ACharWidth: integer): integer;
begin
  Result:= ATabHelper.CalcCharOffsetLast(ALineIndex, S) * ACharWidth div 100;
end;


function CanvasTextOutNeedsOffsets(C: TCanvas; const AStr: atString;
  const AOffsets: TATFontNeedsOffsets): boolean;
//detect result by presence of bold/italic tokens, offsets are needed for them,
//ignore underline, strikeout
var
  St: TFontStyles;
begin
  if CanvasTextOutMustUseOffsets then exit(true);

  St:= C.Font.Style * [fsBold, fsItalic];

  if St=[] then Result:= AOffsets.ForNormal else
   if St=[fsBold] then Result:= AOffsets.ForBold else
    if St=[fsItalic] then Result:= AOffsets.ForItalic else
     if St=[fsBold, fsItalic] then Result:= AOffsets.ForBoldItalic else
      Result:= false;

  if Result then exit;
  Result:= SStringHasUnicodeChars(AStr);
end;


procedure _CalcCharSizesUtf8FromWidestring(const S: UnicodeString;
  DxIn: PInteger;
  DxInLen: integer;
  out DxOut: TATIntArray);
var
  NLen, NSize, ResLen, i: integer;
begin
  NLen:= Length(S);
  SetLength(DxOut, NLen);

  ResLen:= 0;
  i:= 0;
  repeat
    Inc(i);
    if i>NLen then Break;
    if i>DxInLen then Break;

    if (i<NLen) and
      IsCharSurrogateHigh(S[i]) and
      IsCharSurrogateLow(S[i+1]) then
    begin
      NSize:= DxIn[i-1]+DxIn[i];
      Inc(i);
    end
    else
      NSize:= DxIn[i-1];

    Inc(ResLen);
    DxOut[ResLen-1]:= NSize;
  until false;

  //realloc after the loop
  SetLength(DxOut, ResLen);
end;


procedure CanvasTextOut(C: TCanvas; APosX, APosY: integer; AText: atString;
  AParts: PATLineParts; out ATextWidth: integer;
  const AProps: TATCanvasTextOutProps);
var
  ListOffsets: TATLineOffsetsInfo;
  ListInt: TATIntArray;
  Dx: TATIntArray;
  {$ifndef windows}
  DxUTF8: TATIntArray;
  {$endif}
  NLen, NCharWidth, i, j: integer;
  PartStr: atString;
  PartOffset, PartLen,
  PixOffset1, PixOffset2: integer;
  PartPtr: ^TATLinePart;
  PartFontStyle: TFontStyles;
  PartRect: TRect;
  Buf: string;
  BufW: UnicodeString;
  DxPointer: PInteger;
  {$ifdef windows}
  bAllowLigatures: boolean;
  {$endif}
begin
  NLen:= Length(AText);
  if NLen=0 then Exit;
  NCharWidth:= AProps.CharSize.X;

  SetLength(ListInt, NLen);
  SetLength(Dx, NLen);

  if AProps.SuperFast then
  begin
    for i:= 0 to NLen-1 do
    begin
      ListInt[i]:= NCharWidth*(i+1);
      Dx[i]:= NCharWidth;
    end;
  end
  else
  begin
    AProps.TabHelper.CalcCharOffsets(AProps.LineIndex, AText, ListOffsets, AProps.CharsSkipped);

    for i:= 0 to High(ListOffsets) do
      ListInt[i]:= ListOffsets[i] * NCharWidth div 100;

    //truncate AText, to not paint over screen
    i:= AProps.ControlWidth div AProps.CharSize.X + 2;
    if Length(AText)>i then
      SetLength(AText, i);

    Dx[0]:= ListInt[0];
    for i:= 1 to High(ListInt) do
      Dx[i]:= ListInt[i]-ListInt[i-1];
  end;

  if AParts=nil then
  begin
    Buf:= UTF8Encode(SRemoveHexDisplayedChars(AText));
    if CanvasTextOutNeedsOffsets(C, AText, AProps.NeedOffsets) then
      DxPointer:= @Dx[0]
    else
      DxPointer:= nil;
    ExtTextOut(C.Handle, APosX, APosY, 0, nil, PChar(Buf), Length(Buf), DxPointer);

    DoPaintHexChars(C,
      AText,
      @Dx[0],
      Point(APosX, APosY),
      AProps.CharSize,
      AProps.ColorUnprintedHexFont,
      C.Brush.Color,
      AProps.SuperFast
      );
  end
  else
  for j:= 0 to High(TATLineParts) do
    begin
      PartPtr:= @AParts^[j];
      PartLen:= PartPtr^.Len;
      if PartLen=0 then Break;
      PartOffset:= PartPtr^.Offset;
      PartStr:= Copy(AText, PartOffset+1, PartLen);
      if PartStr='' then Break;

      PartFontStyle:= [];
      if PartPtr^.FontBold then Include(PartFontStyle, fsBold);
      if PartPtr^.FontItalic then Include(PartFontStyle, fsItalic);
      if PartPtr^.FontStrikeOut then Include(PartFontStyle, fsStrikeOut);

      if PartOffset>0 then
        PixOffset1:= ListInt[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, Length(AText));
      if i>0 then
        PixOffset2:= ListInt[i-1]
      else
        PixOffset2:= 0;

      C.Font.Color:= PartPtr^.ColorFont;
      C.Brush.Color:= PartPtr^.ColorBG;
      C.Font.Style:= PartFontStyle;

      if PartPtr^.FontItalic and not PartPtr^.FontBold then
      begin
        if AProps.FontItalic_Name<>'' then
        begin
          C.Font.Name:= AProps.FontItalic_Name;
          C.Font.Size:= AProps.FontItalic_Size;
        end;
      end
      else
      if PartPtr^.FontBold and not PartPtr^.FontItalic then
      begin
        if AProps.FontBold_Name<>'' then
        begin
          C.Font.Name:= AProps.FontBold_Name;
          C.Font.Size:= AProps.FontBold_Size;
        end;
      end
      else
      if PartPtr^.FontBold and PartPtr^.FontItalic then
      begin
        if AProps.FontBoldItalic_Name<>'' then
        begin
          C.Font.Name:= AProps.FontBoldItalic_Name;
          C.Font.Size:= AProps.FontBoldItalic_Size;
        end;
      end
      else
      begin
        C.Font.Name:= AProps.FontNormal_Name;
        C.Font.Size:= AProps.FontNormal_Size;
      end;

      PartRect:= Rect(
        APosX+PixOffset1,
        APosY,
        APosX+PixOffset2,
        APosY+AProps.CharSize.Y);

      //increase rect to avoid clipping of italic font at line end,
      //eg comment //WWW, if theme has italic comments style,
      //with font eg "Fira Code Retina"
      if PartPtr^.FontItalic then
        Inc(PartRect.Right,
          C.Font.Size * OptItalicFontLongerInPercents div 100
          );

      {$ifdef windows}
      BufW:= SRemoveHexDisplayedChars(PartStr);
      bAllowLigatures:=
        AProps.ShowFontLigatures
        and not SStringHasUnicodeChars(BufW); //disable ligatures if unicode chars

      if CanvasTextOutNeedsOffsets(C, PartStr, AProps.NeedOffsets) then
        DxPointer:= @Dx[PartOffset]
      else
        DxPointer:= nil;

      _TextOut_Windows(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.TextOffsetFromLine,
        @PartRect,
        BufW,
        DxPointer,
        bAllowLigatures
        );
      {$else}
      BufW:= PartStr;
      Buf:= UTF8Encode(SRemoveHexDisplayedChars(BufW));

      if CanvasTextOutNeedsOffsets(C, PartStr, AProps.NeedOffsets) then
      begin
        _CalcCharSizesUtf8FromWidestring(BufW, @Dx[PartOffset], Length(Dx)-PartOffset, DxUTF8);
        DxPointer:= @DxUTF8[0];
      end
      else
        DxPointer:= nil;

      _TextOut_Unix(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.TextOffsetFromLine,
        @PartRect,
        Buf,
        DxPointer
        );
      {$endif}

      DoPaintHexChars(C,
        PartStr,
        @Dx[PartOffset],
        Point(
          APosX+PixOffset1,
          APosY+AProps.TextOffsetFromLine),
        AProps.CharSize,
        AProps.ColorUnprintedHexFont,
        PartPtr^.ColorBG,
        AProps.SuperFast
        );

      //paint 4 borders of part
      if AProps.MainTextArea then
      begin
        //note: PartRect is changed here
        Dec(PartRect.Right);
        Dec(PartRect.Bottom);

        CanvasLineEx(C,
          PartPtr^.ColorBorder,
          PartPtr^.BorderDown,
          PartRect.Left, PartRect.Bottom,
          PartRect.Right, PartRect.Bottom,
          true);

        CanvasLineEx(C,
          PartPtr^.ColorBorder,
          PartPtr^.BorderUp,
          PartRect.Left, PartRect.Top,
          PartRect.Right, PartRect.Top,
          false);

        CanvasLineEx(C,
          PartPtr^.ColorBorder,
          PartPtr^.BorderLeft,
          PartRect.Left, PartRect.Top,
          PartRect.Left, PartRect.Bottom,
          false);

        CanvasLineEx(C,
          PartPtr^.ColorBorder,
          PartPtr^.BorderRight,
          PartRect.Right, PartRect.Top,
          PartRect.Right, PartRect.Bottom,
          true);
      end;
    end;

  if AProps.ShowUnprinted then
  begin
    if AProps.ShowUnprintedSpacesOnlyInSelection then
    begin
      for i:= 1 to Length(AText) do
        if AProps.DetectIsPosSelected(i-2+AProps.CharIndexInLine, AProps.LineIndex) then
          DoPaintUnprintedChar(C, AText[i], i, ListInt, Point(APosX, APosY), AProps.CharSize, AProps.ColorUnprintedFont);
    end
    else
    if AProps.ShowUnprintedSpacesBothEnds then
    begin
      //paint leading
      for i:= 1 to SGetIndentChars(AText) do
        DoPaintUnprintedChar(C, AText[i], i, ListInt, Point(APosX, APosY), AProps.CharSize, AProps.ColorUnprintedFont);
      //paint trailing
      for i:= SGetNonSpaceLength(AText)+1 to Length(AText) do
        DoPaintUnprintedChar(C, AText[i], i, ListInt, Point(APosX, APosY), AProps.CharSize, AProps.ColorUnprintedFont);
    end
    else
    if AProps.ShowUnprintedSpacesTrailing then
    begin
      //paint trailing
      for i:= SGetNonSpaceLength(AText)+1 to Length(AText) do
        DoPaintUnprintedChar(C, AText[i], i, ListInt, Point(APosX, APosY), AProps.CharSize, AProps.ColorUnprintedFont);
    end
    else
    begin
      //paint all
      for i:= 1 to Length(AText) do
        DoPaintUnprintedChar(C, AText[i], i, ListInt, Point(APosX, APosY), AProps.CharSize, AProps.ColorUnprintedFont);
    end;
  end;

  ATextWidth:= ListInt[High(ListInt)];

  if AText<>'' then
    if Assigned(AProps.DrawEvent) then
      AProps.DrawEvent(nil, C, APosX, APosY, AText, AProps.CharSize, ListInt);
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
  procedure AddPart(const P: TATLinePart); inline;
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
    if P.Offset+P.Len>NOffsetEnd then
      P.Len:= NOffsetEnd-P.Offset;
  end;
  //
var
  PartSelBegin, PartSelEnd: TATLinePart;
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

  if APart.ColorFont=clNone then
    APart.ColorFont:= AParts[nIndex1].ColorFont; //clYellow;

  //these 2 parts are for edges of selection
  FillChar(PartSelBegin{%H-}, SizeOf(TATLinePart), 0);
  FillChar(PartSelEnd{%H-}, SizeOf(TATLinePart), 0);

  PartSelBegin.ColorFont:= APart.ColorFont;
  PartSelBegin.ColorBG:= APart.ColorBG;

  PartSelBegin.Offset:= AParts[nIndex1].Offset+nOffset1;
  PartSelBegin.Len:= AParts[nIndex1].Len-nOffset1;

  PartSelBegin.FontBold:= AParts[nIndex1].FontBold;
  PartSelBegin.FontItalic:= AParts[nIndex1].FontItalic;
  PartSelBegin.FontStrikeOut:= AParts[nIndex1].FontStrikeOut;
  PartSelBegin.BorderDown:= AParts[nIndex1].BorderDown;
  PartSelBegin.BorderLeft:= AParts[nIndex1].BorderLeft;
  PartSelBegin.BorderRight:= AParts[nIndex1].BorderRight;
  PartSelBegin.BorderUp:= AParts[nIndex1].BorderUp;
  PartSelBegin.ColorBorder:= AParts[nIndex1].ColorBorder;

  PartSelEnd.ColorFont:= APart.ColorFont;
  PartSelEnd.ColorBG:= APart.ColorBG;
  PartSelEnd.Offset:= AParts[nIndex2].Offset;
  PartSelEnd.Len:= nOffset2;
  PartSelEnd.FontBold:= AParts[nIndex2].FontBold;
  PartSelEnd.FontItalic:= AParts[nIndex2].FontItalic;
  PartSelEnd.FontStrikeOut:= AParts[nIndex2].FontStrikeOut;
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
    AddPart(APart)
  else
  begin
    nOffsetLimit:= APart.Offset+APart.Len;
    FixPartLen(PartSelBegin, nOffsetLimit);
    AddPart(PartSelBegin);

    for i:= nIndex1+1 to nIndex2-1 do
    begin
      AParts[i].ColorFont:= APart.ColorFont;
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

procedure DoPartsShow(var P: TATLineParts);
var
  s: string;
  i: integer;
begin
  s:= '';
  for i:= Low(P) to High(P) do
  begin
    if P[i].Len=0 then break;
    s:= s+Format('[%d %d]', [P[i].Offset, P[i].Len]);
  end;

  Application.MainForm.Caption:= s;
end;

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


procedure CanvasTextOutMinimap(C: TCanvas; const ARect: TRect; APos: TPoint;
  ACharSize: TPoint; ATabSize: integer; constref AParts: TATLineParts;
  AColorBG: TColor; const ALine: atString; AUsePixels: boolean);
{
Line is painted with ACharSize.Y=2px height, with 1px spacing between lines
}
var
  Part: PATLinePart;
  NPartIndex, NCharIndex, NSpaces: integer;
  X1, X2, Y1, Y2: integer;
  HasBG: boolean;
  NColorBack, NColorFont, NColorFontHalf: TColor;
  ch: WideChar;
begin
  //offset<0 means some bug on making parts!
  if AParts[0].Offset<0 then exit;

  NSpaces:= 0;
  for NPartIndex:= Low(TATLineParts) to High(TATLineParts) do
  begin
    Part:= @AParts[NPartIndex];
    if Part^.Len=0 then Break; //last part
    if Part^.Offset>Length(ALine) then Break; //part out of ALine

    NColorFont:= Part^.ColorFont;
    NColorBack:= Part^.ColorBG;
    if NColorBack=clNone then
      NColorBack:= AColorBG;
    HasBG:= NColorBack<>AColorBG;

    //clNone means that it's empty/space part (adapter must set so)
    if NColorFont=clNone then
      if HasBG then
        NColorFont:= NColorBack
      else
        Continue;

    NColorFontHalf:= ColorBlendHalf(NColorBack, NColorFont);

    //iterate over all chars, to check for spaces (ignore them) and Tabs (add indent for them).
    //because need to paint multiline comments/strings nicely.
    for NCharIndex:= Part^.Offset+1 to Part^.Offset+Part^.Len do
    begin
      if NCharIndex>Length(ALine) then Break;
      ch:= ALine[NCharIndex];
      if ch=#9 then
        Inc(NSpaces, ATabSize)
      else
        Inc(NSpaces);

      X1:= APos.X + ACharSize.X*NSpaces;
      X2:= X1 + ACharSize.X;
      Y1:= APos.Y;
      Y2:= Y1 + ACharSize.Y;

      if (X1>=ARect.Left) and (X1<ARect.Right) then
      begin
        //must limit line on right edge
        //if X2>ARect.Right then
        //  X2:= ARect.Right;

        if HasBG then
        begin
          //paint BG as 2 pixel line
          if AUsePixels then
          begin
            C.Pixels[X1, Y1]:= NColorBack;
            C.Pixels[X1, Y2-1]:= NColorBack;
          end
          else
          begin
            C.Brush.Color:= NColorBack;
            C.FillRect(X1, Y1, X2, Y2);
          end;
        end;

        if not IsCharSpace(ch) then
        begin
          if AUsePixels then
          begin
            C.Pixels[X1, Y2-1]:= NColorFontHalf;
          end
          else
          begin
            C.Brush.Color:= NColorFontHalf;
            C.FillRect(X1, Y1+ACharSize.Y div 2, X2, Y2);
          end;
        end;
      end;
    end;
  end;
end;


end.

