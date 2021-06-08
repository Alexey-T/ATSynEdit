{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CanvasProc;

{$mode objfpc}{$H+}
{$MinEnumSize 1}

{$I atsynedit_defines.inc}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, Graphics, Types,
  Forms,
  BGRABitmap,
  BGRABitmapTypes,
  ATCanvasPrimitives,
  ATStringProc,
  ATStrings,
  ATSynEdit_LineParts,
  ATSynEdit_CharSizer;

type
  TATSynEditUnptintedEolSymbol = (
    aeueDot,
    aeueArrowDown,
    aeuePilcrow
    );

var
  OptUnprintedTabCharLength: integer = 1;
  OptUnprintedTabPointerScale: integer = 22;
  OptUnprintedEofCharLength: integer = 1;
  OptUnprintedSpaceDotScale: integer = 15;
  OptUnprintedEndDotScale: integer = 30;
  OptUnprintedEndFontScale: integer = 40;
  OptUnprintedEndSymbol: TATSynEditUnptintedEolSymbol = aeueArrowDown;
  OptUnprintedEndArrowLength: integer = 70;
  OptUnprintedWrapArrowLength: integer = 40;
  OptUnprintedWrapArrowWidth: integer = 80;
  OptItalicFontLongerInPercents: integer = 40;

var
  //Win: seems no slowdown from offsets
  //macOS: better use offsets, fonts have floating width value, e.g. 10.2 pixels
  //Linux Qt5: same as for macOS
  //Linux GTK2: big slowdown from offsets
  OptCanvasTextoutNeedsOffsets: boolean =
    {$if defined(windows) or defined(darwin) or defined(LCLQt5)}
    true
    {$else}
    false
    {$endif} ;

type
  TATSynEditCallbackIsCharSelected = function(AX, AY: integer): boolean of object;

  TATWiderFlags = record
    ForNormal: boolean;
    ForBold: boolean;
    ForItalic: boolean;
    ForBoldItalic: boolean;
  end;

type
  TATSynEditDrawLineEvent = procedure(Sender: TObject; C: TCanvas;
    ALineIndex: integer;
    AX, AY: integer; const AStr: atString; ACharSize: TPoint;
    constref AExtent: TATIntFixedArray) of object;

type
  TATLineEndsSymbols = (
    cEndingTextNone,
    cEndingTextLF,
    cEndingTextCRLF,
    cEndingTextCR,
    cEndingTextEOF
    );

const
  cLineEndsToSymbols: array[TATLineEnds] of TATLineEndsSymbols = (
    cEndingTextNone,
    cEndingTextCRLF,
    cEndingTextLF,
    cEndingTextCR
    );

type
  TATCanvasTextOutProps = record
    Editor: TObject;
    HasAsciiNoTabs: boolean;
    SuperFast: boolean;
    TabHelper: TATStringTabHelper;
    LineIndex: integer;
    CharIndexInLine: integer;
    CharSize: TPoint;
    CharsSkipped: integer;
    TrimmedTrailingNonSpaces: boolean;
    DrawEvent: TATSynEditDrawLineEvent;
    ControlWidth: integer;
    TextOffsetFromLine: integer;
    ShowUnprinted: boolean;
    ShowUnprintedSpacesTrailing: boolean;
    ShowUnprintedSpacesBothEnds: boolean;
    ShowUnprintedSpacesOnlyInSelection: boolean;
    ShowFontLigatures: boolean;
    ColorNormalFont: TColor;
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

procedure CanvasLineHorz(C: TCanvas; X1, Y, X2: integer; AWithEnd: boolean=false); inline;
procedure CanvasLineVert(C: TCanvas; X, Y1, Y2: integer; AWithEnd: boolean=false); inline;
procedure CanvasLineVert2(C: TCanvas; AX, AY1, AY2: integer; AWithEnd: boolean; ALineWidth: integer);

procedure CanvasLineEx(C: TCanvas;
  Color: TColor; Style: TATLineStyle;
  X1, Y1, X2, Y2: integer; AtDown: boolean);

procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: string); inline;
procedure CanvasTextOutSimplest_PChar(C: TCanvas; X, Y: integer; Buf: PChar; Len: integer); inline;

procedure CanvasTextOut(C: TCanvas;
  APosX, APosY: integer;
  const AText: atString;
  AParts: PATLineParts;
  out ATextWidth: integer;
  const AProps: TATCanvasTextOutProps
  );

procedure CanvasTextOutMinimap(
  C: TBGRABitmap;
  const ARect: TRect;
  APosX, APosY: integer;
  ACharSize: TPoint;
  ATabSize: integer;
  constref AParts: TATLineParts;
  AColorBG: TColor;
  AColorAfter: TColor;
  const ALine: atString;
  AUsePixels: boolean
  );

procedure DoPaintUnprintedSymbols(C: TCanvas;
  ASymbols: TATLineEndsSymbols;
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont, AColorBG: TColor);

procedure DoPaintUnprintedEndSymbol(C: TCanvas;
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont, AColorBg: TColor);

procedure DoPaintUnprintedWrapMark(C: TCanvas;
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont: TColor);

function CanvasTextWidth(const S: atString; ALineIndex: integer;
  ATabHelper: TATStringTabHelper; ACharWidth: integer): integer; inline;

procedure UpdateWiderFlags(C: TCanvas; out Flags: TATWiderFlags);

implementation

uses
  Math,
  LCLType,
  LCLIntf;

procedure UpdateWiderFlags(C: TCanvas; out Flags: TATWiderFlags);
const
  cTest = 'WW';
var
  N1, N2: integer;
  PrevStyle: TFontStyles;
begin
  PrevStyle:= C.Font.Style;
  try
    C.Font.Style:= [];
    N1:= C.TextWidth('n');
    N2:= C.TextWidth(cTest);

    Flags.ForNormal:= N2<>N1*Length(cTest);
    if Flags.ForNormal then
    begin
      Flags.ForBold:= true;
      Flags.ForItalic:= true;
      Flags.ForBoldItalic:= true;
      exit;
    end;

    C.Font.Style:= [fsBold];
    N2:= C.TextWidth(cTest);
    Flags.ForBold:= N2<>N1*Length(cTest);

    C.Font.Style:= [fsItalic];
    N2:= C.TextWidth(cTest);
    Flags.ForItalic:= N2<>N1*Length(cTest);

    if Flags.ForBold or Flags.ForItalic then
      Flags.ForBoldItalic:= true
    else
    begin
      C.Font.Style:= [fsBold, fsItalic];
      N2:= C.TextWidth(cTest);
      Flags.ForBoldItalic:= N2<>N1*Length(cTest);
    end;
  finally
    C.Font.Style:= PrevStyle;
  end;
  {
  application.MainForm.caption:= (format('norm %d, b %d, i %d, bi %d', [
    Ord(Flags.ForNormal),
    Ord(Flags.ForBold),
    Ord(Flags.ForItalic),
    Ord(Flags.ForBoldItalic)
    ]));
    }
end;

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
  constref Rect: PRect;
  const Str: string;
  Dx: PInteger
  ): boolean; inline;
begin
  //ETO_CLIPPED runs more code in TGtk2WidgetSet.ExtTextOut
  //ETO_OPAQUE paints rect first, but it's needed
  //  a) to fill tab-chars in selection (full width >1)
  //  b) to fill inter-line 1px area
  Result:= ExtTextOut(DC, X, Y, {ETO_CLIPPED or} ETO_OPAQUE, Rect, PChar(Str), Length(Str), Dx);
end;
{$endif}

procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: string); inline;
begin
  {$ifdef windows}
  Windows.TextOutA(C.Handle, X, Y, PChar(S), Length(S));
  {$else}
  LCLIntf.TextOut(C.Handle, X, Y, PChar(S), Length(S));
  {$endif}
end;

procedure CanvasTextOutSimplest_PChar(C: TCanvas; X, Y: integer; Buf: PChar; Len: integer); inline;
begin
  {$ifdef windows}
  Windows.TextOutA(C.Handle, X, Y, Buf, Len);
  {$else}
  LCLIntf.TextOut(C.Handle, X, Y, Buf, Len);
  {$endif}
end;

procedure CanvasUnprintedSpace(C: TCanvas; const ARect: TRect;
  AScale: integer; AFontColor: TColor);
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
  C.Brush.Color:= AFontColor;
  C.FillRect(R);
end;

procedure DoPaintUnprintedChar(
  C: TCanvas;
  ch: WideChar;
  AIndex: integer;
  var AOffsets: TATIntFixedArray;
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont: TColor);
var
  R: TRect;
begin
  R.Left:= AX;
  R.Right:= AX;
  if AIndex>1 then
    Inc(R.Left, AOffsets.Data[AIndex-2]);
  Inc(R.Right, AOffsets.Data[AIndex-1]);

  R.Top:= AY;
  R.Bottom:= AY+ACharSize.Y;

  if ch<>#9 then
    CanvasUnprintedSpace(C, R, OptUnprintedSpaceDotScale, AColorFont)
  else
    CanvasArrowHorz(C, R,
      AColorFont,
      OptUnprintedTabCharLength*ACharSize.X,
      true,
      OptUnprintedTabPointerScale);
end;


procedure CanvasLineHorz(C: TCanvas; X1, Y, X2: integer; AWithEnd: boolean);
begin
  //Assert(X2>X1, 'LineHorz x2>x1');
  if AWithEnd then Inc(X2);
  {$ifdef windows}
  Windows.MoveToEx(C.Handle, X1, Y, nil);
  Windows.LineTo(C.Handle, X2, Y);
  {$else}
  C.Line(X1, Y, X2, Y);
  {$endif}
end;

procedure CanvasLineVert(C: TCanvas; X, Y1, Y2: integer; AWithEnd: boolean);
begin
  //Assert(Y2>Y1, 'LineVert y2>y1');
  if AWithEnd then Inc(Y2);
  {$ifdef windows}
  Windows.MoveToEx(C.Handle, X, Y1, nil);
  Windows.LineTo(C.Handle, X, Y2);
  {$else}
  C.Line(X, Y1, X, Y2);
  {$endif}
end;

procedure CanvasLineVert2(C: TCanvas; AX, AY1, AY2: integer; AWithEnd: boolean; ALineWidth: integer);
var
  XFrom, XTo, X: integer;
begin
  if ALineWidth<=1 then
    CanvasLineVert(C, AX, AY1, AY2, AWithEnd)
  else
  begin
    XFrom:= AX-ALineWidth div 2;
    XTo:= XFrom+ALineWidth-1;
    for X:= XFrom to XTo do
      CanvasLineVert(C, X, AY1, AY2, AWithEnd);
  end;
end;


procedure CanvasLineEx(C: TCanvas; Color: TColor; Style: TATLineStyle; X1, Y1, X2, Y2: integer; AtDown: boolean);
  //
  procedure CanvasLine_WithEnd(X1, Y1, X2, Y2: integer); inline;
  begin
    if Y1=Y2 then
      CanvasLineHorz(C, X1, Y1, X2, true)
    else
      CanvasLineVert(C, X1, Y1, Y2, true);
  end;
  //
begin
  case Style of
    cLineStyleNone:
      exit;

    cLineStyleSolid:
      begin
        C.Pen.Color:= Color;
        CanvasLine_WithEnd(X1, Y1, X2, Y2);
      end;

    cLineStyleSolid2px:
      begin
        C.Pen.Color:= Color;
        CanvasLine_WithEnd(X1, Y1, X2, Y2);
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
        CanvasLine_WithEnd(X1, Y1, X2, Y2);
      end;

    cLineStyleDash:
      begin
        C.Pen.Color:= Color;
        C.Pen.Style:= psDot;
        CanvasLine_WithEnd(X1, Y1, X2, Y2);
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
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont,
  AColorBg: TColor;
  ASuperFast: boolean);
const
  HexDigits: array[0..15] of char = '0123456789ABCDEF';
  HexDummyMark = '?';
  Buf2: array[0..3] of char = 'x??'#0; //2 hex digits, trailing 0
  Buf4: array[0..5] of char = 'x????'#0; //4 hex digits, trailing 0
var
  Buf: PChar;
  Value, HexLen: integer;
  ch: WideChar;
  iChar, j: integer;
  bColorSet: boolean;
begin
  if AString='' then Exit;
  bColorSet:= false;

  for iChar:= 1 to Length(AString) do
  begin
    ch:= AString[iChar];
    if IsCharHexDisplayed(ch) then
    begin
      if not bColorSet then
      begin
        bColorSet:= true;
        C.Font.Color:= AColorFont;
        C.Brush.Color:= AColorBg;
      end;

      if ASuperFast then
        CanvasTextOutSimplest(C, AX, AY, HexDummyMark)
      else
      begin
        Value:= Ord(ch);
        if Value>$FF then
        begin
          HexLen:= 5;
          Buf:= @Buf4;
        end
        else
        begin
          HexLen:= 3;
          Buf:= @Buf2;
        end;

        for j:= 1 to HexLen-1 do
        begin
          Buf[HexLen-j]:= HexDigits[Value and 15];
          Value:= Value shr 4;
        end;

        CanvasTextOutSimplest_PChar(C, AX, AY, Buf, HexLen);
      end;
    end;

    Inc(AX, ADx^[iChar-1]);
  end;
end;

procedure CanvasChar(C: TCanvas; AColorFont, AColorBg: TColor; ch: char; X, Y, W, H: integer);
begin
  C.Pen.Color:= AColorFont;
  if AColorBg<>clNone then
  begin
    C.Brush.Color:= AColorBg;
    C.FillRect(X-1, Y-1, X+W+2, Y+H+2);
  end;

  case ch of
    'L':
      begin
        CanvasLineVert(C, X, Y, Y+H, true);
        CanvasLineHorz(C, X, Y+H, X+W, true);
      end;
    'F':
      begin
        CanvasLineVert(C, X, Y, Y+H, true);
        CanvasLineHorz(C, X, Y, X+W, true);
        CanvasLineHorz(C, X, Y+H div 2, X+W*3 div 4, true);
      end;
    'C':
      begin
        CanvasLineVert(C, X, Y+1, Y+H-1, true);
        CanvasLineHorz(C, X+1, Y, X+W, true);
        CanvasLineHorz(C, X+1, Y+H, X+W, true);
      end;
    'R':
      begin
        CanvasLineVert(C, X, Y, Y+H, true);
        CanvasLineVert(C, X+W, Y+1, Y+H div 2-1, true);
        CanvasLineHorz(C, X, Y, X+W-1, true);
        CanvasLineHorz(C, X, Y+H div 2, X+W-1, true);
        CanvasLine(C, Point(X+W, Y+H), Point(X+W div 2, Y+H div 2), AColorFont);
      end;
    'E':
      begin
        CanvasLineVert(C, X, Y, Y+H, true);
        CanvasLineHorz(C, X, Y, X+W, true);
        CanvasLineHorz(C, X, Y+H div 2, X+W*3 div 4, true);
        CanvasLineHorz(C, X, Y+H, X+W, true);
      end;
    'O':
      begin
        CanvasLineVert(C, X, Y+1, Y+H-1, true);
        CanvasLineVert(C, X+W, Y+1, Y+H-1, true);
        CanvasLineHorz(C, X+1, Y, X+W-1, true);
        CanvasLineHorz(C, X+1, Y+H, X+W-1, true);
      end;
  end;
end;

procedure DoPaintUnprintedSymbols(C: TCanvas;
  ASymbols: TATLineEndsSymbols;
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont, AColorBG: TColor);
const
  cText: array[TATLineEndsSymbols] of string[4] = ('', 'LF', 'CRLF', 'CR', 'EOF');
var
  SText: string[4];
  X, Y, W, H: integer;
  i: integer;
begin
  H:= ACharSize.Y * OptUnprintedEndFontScale div 100;
  W:= H div 2;

  X:= AX + 2;
  Y:= AY + ACharSize.Y div 2 - H div 2;

  SText:= cText[ASymbols];
  for i:= 1 to Length(SText) do
  begin
    CanvasChar(C, AColorFont, AColorBG, SText[i], X, Y, W, H);
    Inc(X, W+2);
  end;
end;

procedure DoPaintUnprintedEndSymbol(C: TCanvas;
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont, AColorBg: TColor);
const
  // https://www.fileformat.info/info/unicode/char/B6/index.htm
  cPilcrowString: PChar = #$C2#$B6;
begin
  case OptUnprintedEndSymbol of
    aeueDot:
      CanvasUnprintedSpace(C,
        Rect(AX, AY, AX+ACharSize.X, AY+ACharSize.Y),
        OptUnprintedEndDotScale,
        AColorFont);
    aeueArrowDown:
      CanvasArrowDown(C,
        Rect(AX, AY, AX+ACharSize.X, AY+ACharSize.Y),
        AColorFont,
        OptUnprintedEndArrowLength,
        OptUnprintedTabPointerScale
        );
    aeuePilcrow:
      begin
        C.Font.Color:= AColorFont;
        C.Brush.Style:= bsClear;
        CanvasTextOutSimplest(C, AX, AY, cPilcrowString);
        C.Brush.Style:= bsSolid;
      end;
  end;
end;

procedure DoPaintUnprintedWrapMark(C: TCanvas;
  AX, AY: integer;
  ACharSize: TPoint;
  AColorFont: TColor);
begin
  CanvasArrowWrapped(C,
    Rect(AX, AY, AX+ACharSize.X, AY+ACharSize.Y),
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


function CanvasTextOutNeedsOffsets(C: TCanvas; const AStr: UnicodeString): boolean; inline;
{
var
  Flags: TATWiderFlags;
  St: TFontStyles;
}
begin
  if OptCanvasTextoutNeedsOffsets then
    exit(true);

  {
  //disabled since CudaText 1.104
  //a) its used only on Linux/BSD yet, but is it needed there?
  //it was needed maybe for Win32 (need to check) but on Win32 const OptCanvasTextoutNeedsOffsets=true
  //b) it must be placed out of this deep func CanvasTextOut, its called too much (for each token)

  //detect result by presence of bold/italic tokens, offsets are needed for them,
  //ignore underline, strikeout

  St:= C.Font.Style * [fsBold, fsItalic];

  if St=[] then Result:= Flags.ForNormal else
   if St=[fsBold] then Result:= Flags.ForBold else
    if St=[fsItalic] then Result:= Flags.ForItalic else
     if St=[fsBold, fsItalic] then Result:= Flags.ForBoldItalic else
      Result:= false;

  if Result then exit;
  }

  //force Offsets not for all unicode.
  //only for those chars, which are full-width or "hex displayed" or unknown width.
  Result:= IsStringWithUnusualWidthChars(AStr);
end;


procedure _CalcCharSizesUtf8FromWidestring(const S: UnicodeString;
  DxIn: PInteger;
  DxInLen: integer;
  var DxOut: TATIntFixedArray);
var
  NLen, NSize, ResLen, i: integer;
begin
  NLen:= Min(Length(S), cMaxFixedArray);
  FillChar(DxOut, SizeOf(DxOut), 0);

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
    DxOut.Len:= ResLen;
    DxOut.Data[ResLen-1]:= NSize;
  until false;
end;

//global vars to avoid mem allocs, speeds up rendering by 10-30%
var
  ListOffsets,
  ListInt,
  Dx: TATIntFixedArray;
  {$ifndef windows}
  DxUTF8: TATIntFixedArray;
  {$endif}

procedure CanvasTextOut(C: TCanvas;
  APosX, APosY: integer;
  const AText: atString;
  AParts: PATLineParts;
  out ATextWidth: integer;
  const AProps: TATCanvasTextOutProps);
var
  {$ifndef windows}
  Buf: string;
  {$else}
  bAllowLigatures: boolean;
  {$endif}
  BufW: UnicodeString;
  NLen, NCharWidth, i, iPart: integer;
  NLastPart: integer;
  PartStr: atString;
  PartOffset, PartLen,
  PixOffset1, PixOffset2: integer;
  PartPtr: ^TATLinePart;
  PartFontStyle: TFontStyles;
  PartRect: TRect;
  DxPointer: PInteger;
  NStyles: integer;
  bBold, bItalic, bCrossed, bSpaceChars: boolean;
  ch: WideChar;
begin
  NLen:= Min(Length(AText), cMaxFixedArray);
  if NLen=0 then Exit;
  NCharWidth:= AProps.CharSize.X;

  FillChar(ListInt, SizeOf(ListInt), 0);
  FillChar(Dx, SizeOf(Dx), 0);
  //no need to do FillChar(DxUTF8...)

  if AProps.SuperFast or AProps.HasAsciiNoTabs then
  begin
    ListInt.Len:= NLen;
    Dx.Len:= NLen;
    for i:= 0 to NLen-1 do
    begin
      ListInt.Data[i]:= NCharWidth*(i+1);
      Dx.Data[i]:= NCharWidth;
    end;
  end
  else
  begin
    AProps.TabHelper.CalcCharOffsets(AProps.LineIndex, AText, ListOffsets, AProps.CharsSkipped);

    ListInt.Len:= ListOffsets.Len;
    Dx.Len:= ListOffsets.Len;

    for i:= 0 to ListOffsets.Len-1 do
      ListInt.Data[i]:= ListOffsets.Data[i] * NCharWidth div 100;

    Dx.Data[0]:= ListInt.Data[0];
    for i:= 1 to ListInt.Len-1 do
      Dx.Data[i]:= ListInt.Data[i]-ListInt.Data[i-1];
  end;

  if AParts=nil then
  begin
    if AProps.HasAsciiNoTabs and not OptCanvasTextoutNeedsOffsets then
    begin
      BufW:= AText;
      DxPointer:= nil;
    end
    else
    begin
      BufW:= SRemoveHexDisplayedChars(AText);
      if CanvasTextOutNeedsOffsets(C, AText) then
        DxPointer:= @Dx.Data[0]
      else
        DxPointer:= nil;
    end;

    {$ifdef windows}
    _TextOut_Windows(C.Handle, APosX, APosY, nil, BufW, DxPointer, false{no ligatures});
    {$else}
    Buf:= BufW;
    _TextOut_Unix(C.Handle, APosX, APosY, nil, Buf, DxPointer);
    {$endif}

    if not AProps.HasAsciiNoTabs then
      DoPaintHexChars(C,
        AText,
        @Dx.Data[0],
        APosX,
        APosY,
        AProps.CharSize,
        AProps.ColorUnprintedHexFont,
        C.Brush.Color,
        AProps.SuperFast
        );
  end
  else
  begin
    NLastPart:= 0;
    for iPart:= 0 to High(TATLineParts) do
    begin
      PartPtr:= @AParts^[iPart];
      PartLen:= PartPtr^.Len;
      if PartLen=0 then Break;
      PartOffset:= PartPtr^.Offset;
      PartStr:= Copy(AText, PartOffset+1, PartLen);
      if PartStr='' then Break;
      bSpaceChars:= IsStringSpaces(PartStr);
      NLastPart:= iPart;

      if PartOffset>0 then
        PixOffset1:= ListInt.Data[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, Length(AText));
      if i>0 then
        PixOffset2:= ListInt.Data[i-1]
      else
        PixOffset2:= 0;

      C.Brush.Color:= PartPtr^.ColorBG;

      if not bSpaceChars then
      begin
        NStyles:= PartPtr^.FontStyles;
        bBold:= (NStyles and afsFontBold)<>0;
        bItalic:= (NStyles and afsFontItalic)<>0;
        bCrossed:= (NStyles and afsFontCrossed)<>0;

        PartFontStyle:= [];
        if bBold then Include(PartFontStyle, fsBold);
        if bItalic then Include(PartFontStyle, fsItalic);
        if bCrossed then Include(PartFontStyle, fsStrikeOut);

        C.Font.Style:= PartFontStyle;
        C.Font.Color:= PartPtr^.ColorFont;

        if bItalic and not bBold then
        begin
          if AProps.FontItalic_Name<>'' then
          begin
            C.Font.Name:= AProps.FontItalic_Name;
            C.Font.Size:= AProps.FontItalic_Size;
          end;
        end
        else
        if bBold and not bItalic then
        begin
          if AProps.FontBold_Name<>'' then
          begin
            C.Font.Name:= AProps.FontBold_Name;
            C.Font.Size:= AProps.FontBold_Size;
          end;
        end
        else
        if bBold and bItalic then
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
      end; //if not bSpaceChars

      PartRect:= Rect(
        APosX+PixOffset1,
        APosY,
        APosX+PixOffset2,
        APosY+AProps.CharSize.Y);

      //increase rect to avoid clipping of italic font at line end,
      //eg comment //WWW, if theme has italic comments style,
      //with font eg "Fira Code Retina"
      if bItalic then
        Inc(PartRect.Right,
          C.Font.Size * OptItalicFontLongerInPercents div 100
          );

      //part with only blanks, render simpler
      //important for gtk2: tab-chars give Dx offsets, render is much slower with Dx
      if bSpaceChars then
      begin
        C.FillRect(PartRect);
        Continue;
      end;

      {$ifdef windows}
      if AProps.HasAsciiNoTabs and not OptCanvasTextoutNeedsOffsets then
      begin
        BufW:= PartStr;
        bAllowLigatures:= AProps.ShowFontLigatures;
        DxPointer:= nil;
      end
      else
      begin
        BufW:= SRemoveHexDisplayedChars(PartStr);
        bAllowLigatures:=
          AProps.ShowFontLigatures
          and not IsStringWithUnusualWidthChars(BufW); //disable ligatures if unicode chars

        if CanvasTextOutNeedsOffsets(C, PartStr) then
          DxPointer:= @Dx.Data[PartOffset]
        else
          DxPointer:= nil;
      end;

      _TextOut_Windows(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.TextOffsetFromLine,
        @PartRect,
        BufW,
        DxPointer,
        bAllowLigatures
        );
      {$else}
      if AProps.HasAsciiNoTabs and not OptCanvasTextoutNeedsOffsets then
      begin
        Buf:= PartStr;
        DxPointer:= nil;
      end
      else
      begin
        BufW:= PartStr;
        Buf:= UTF8Encode(SRemoveHexDisplayedChars(BufW));

        if CanvasTextOutNeedsOffsets(C, PartStr) then
        begin
          _CalcCharSizesUtf8FromWidestring(BufW, @Dx.Data[PartOffset], Dx.Len-PartOffset, DxUTF8);
          DxPointer:= @DxUTF8.Data[0];
        end
        else
          DxPointer:= nil;
      end;

      _TextOut_Unix(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.TextOffsetFromLine,
        @PartRect,
        Buf,
        DxPointer
        );
      {$endif}

      if not AProps.HasAsciiNoTabs then
        DoPaintHexChars(C,
          PartStr,
          @Dx.Data[PartOffset],
          APosX+PixOffset1,
          APosY+AProps.TextOffsetFromLine,
          AProps.CharSize,
          AProps.ColorUnprintedHexFont,
          PartPtr^.ColorBG,
          AProps.SuperFast
          );

      //paint 4 borders of part
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

    //paint chars after all LineParts are painted, when too many tokens in line
    if NLastPart>=High(TATLineParts)-1 then
    begin
      PartPtr:= @AParts^[NLastPart];
      PartLen:= PartPtr^.Len;
      PartOffset:= PartPtr^.Offset;
      PartStr:= Copy(AText, PartOffset+1+PartLen, MaxInt);
      PixOffset1:= ListInt.Data[PartOffset];
      C.Font.Color:= AProps.ColorNormalFont;
      C.Font.Style:= [];

      {$ifdef windows}
      _TextOut_Windows(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.TextOffsetFromLine,
        nil,
        PartStr,
        nil,
        false
        );
      {$else}
      _TextOut_Unix(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.TextOffsetFromLine,
        nil,
        PartStr,
        nil
        );
      {$endif}
    end;
  end;

  if AProps.ShowUnprinted then
  begin
    if AProps.ShowUnprintedSpacesOnlyInSelection then
    begin
      for i:= 1 to Length(AText) do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) then
          if AProps.DetectIsPosSelected(i-2+AProps.CharIndexInLine, AProps.LineIndex) then
            DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
    end
    else
    if AProps.ShowUnprintedSpacesBothEnds then
    begin
      //paint leading
      for i:= 1 to SGetIndentChars(AText) do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
      //paint trailing
      for i:= SGetNonSpaceLength(AText)+1 to Length(AText) do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
    end
    else
    if AProps.ShowUnprintedSpacesTrailing then
    begin
      //paint trailing
      if not AProps.TrimmedTrailingNonSpaces then
        for i:= SGetNonSpaceLength(AText)+1 to Length(AText) do
        begin
          ch:= AText[i];
          if IsCharUnicodeSpace(ch) then
            DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
        end;
    end
    else
    begin
      //paint all
      for i:= 1 to Length(AText) do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
    end;
  end;

  ATextWidth:= ListInt.Data[ListInt.Len-1];

  if AText<>'' then
    if Assigned(AProps.DrawEvent) then
      AProps.DrawEvent(
        AProps.Editor,
        C,
        AProps.LineIndex,
        APosX,
        APosY,
        AText,
        AProps.CharSize,
        ListInt
        );
end;


procedure CanvasTextOutMinimap(
  C: TBGRABitmap;
  const ARect: TRect; APosX, APosY: integer;
  ACharSize: TPoint; ATabSize: integer; constref AParts: TATLineParts;
  AColorBG: TColor; AColorAfter: TColor;
  const ALine: atString;
  AUsePixels: boolean
  );
{
Line is painted with ACharSize.Y=2px height, with 1px spacing between lines
}
var
  Part: PATLinePart;
  NPartIndex, NCharIndex, NSpaces, NSpaceThis: integer;
  X1, X2, Y1, Y2, Y2b: integer;
  bHasBG: boolean;
  bSpace: boolean;
  NColorBack, NColorFont, NColorFontHalf: TColor;
  ch: WideChar;
  rColorBack, rColorFont: TBGRAPixel;
begin
  //offset<0 means some bug on making parts!
  if AParts[0].Offset<0 then exit;

  X1:= 0;
  X2:= 0;
  Y1:= APosY;
  Y2:= Y1 + ACharSize.Y;
  Y2b:= Y1 + ACharSize.Y div 2;
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
    bHasBG:= NColorBack<>AColorBG;

    //clNone means that it's empty/space part (adapter must set so)
    if NColorFont=clNone then
      if bHasBG then
        NColorFont:= NColorBack
      else
        Continue;

    NColorFontHalf:= ColorBlendHalf(NColorBack, NColorFont);

    rColorBack.FromColor(NColorBack);
    rColorFont.FromColor(NColorFontHalf);

    //iterate over all chars, to check for spaces (ignore them) and Tabs (add indent for them).
    //because need to paint multiline comments/strings nicely.
    for NCharIndex:= Part^.Offset+1 to Part^.Offset+Part^.Len do
    begin
      if NCharIndex>Length(ALine) then Break;
      ch:= ALine[NCharIndex];
      if ch=#9 then
      begin
        bSpace:= true;
        NSpaceThis:= ATabSize;
      end
      else
      begin
        bSpace:= ch=' ';
        NSpaceThis:= 1;
      end;

      X1:= APosX + ACharSize.X*NSpaces;
      X2:= X1 + ACharSize.X*NSpaceThis;

      if X1>ARect.Right then Break;
      Inc(NSpaces, NSpaceThis);

      if AUsePixels and (NSpaceThis=1) then
      begin
        if bHasBG then
          C.SetPixel(X1, Y1, rColorBack);
        if not bSpace then
          C.SetPixel(X1, Y2b, rColorFont);
      end
      else
      begin
        if bHasBG then
          C.FillRect(X1, Y1, X2, Y2, rColorBack);
        if not bSpace then
          C.FillRect(X1, Y2b, X2, Y2, rColorFont);
      end;
    end;
  end;

  if AColorAfter<>clNone then
  begin
    rColorBack.FromColor(AColorAfter);
    C.FillRect(X2, Y1, ARect.Right, Y2, rColorBack);
  end;
end;

end.

