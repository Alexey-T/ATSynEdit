{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CanvasProc;

{$mode objfpc}{$H+}
{$MinEnumSize 1}
{$ScopedEnums on}

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
  ATSynEdit_Globals,
  ATSynEdit_LineParts,
  ATSynEdit_CharSizer;

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
    AX, AY: integer; const AStr: atString; const ACharSize: TATEditorCharSize;
    constref AExtent: TATIntFixedArray) of object;

type
  TATLineEndsSymbols = (
    None,
    LF,
    CRLF,
    CR,
    EOF
    );

const
  cLineEndsToSymbols: array[TATLineEnds] of TATLineEndsSymbols = (
    TATLineEndsSymbols.None,
    TATLineEndsSymbols.CRLF,
    TATLineEndsSymbols.LF,
    TATLineEndsSymbols.CR
    );

type
  TATCanvasTextOutProps = record
    Editor: TObject;
    HasAsciiNoTabs: boolean; //line has only ASCII chars w/o tab-char
    SuperFast: boolean; //line is very long so render it in simpler way (don't calculate Dx offsets array)
    TabHelper: TATStringTabHelper;
    LineIndex: integer; //index of line in Strings
    CharIndexInLine: integer; //for part of long wrapped line, this is the char index of part's begin
    CharSize: TATEditorCharSize;
    CharsSkipped: integer; //count of chars removed from the line start, before passing line to renderer
    TrimmedTrailingNonSpaces: boolean; //lines's trailing non-space chars were removed before passing line to renderer (for speedup for too long line)
    DrawEvent: TATSynEditDrawLineEvent;
    ControlWidth: integer; //width of editor control, plus small delta to render 2 edge chars
    SpacingTopEdge: integer; //Y offset, additional to Y coord of line (to emulate indent of 1st line from top edge)
    SpacingTop: integer; //Y offset, from text inside each line
    ShowUnprinted: boolean; //unprinted chars: global enabling flag
    ShowUnprintedSpacesTrailing: boolean;
    ShowUnprintedSpacesBothEnds: boolean;
    ShowUnprintedSpacesOnlyInSelection: boolean;
    ShowUnprintedSpacesAlsoInSelection: boolean;
    ShowUnprintedForceTabs: boolean;
    ShowFontLigatures: boolean;
    ColorNormalFont: TColor;
    ColorUnprintedFont: TColor;
    ColorUnprintedHexFont: TColor;
    FontProportional: boolean;
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

procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: string);
procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: UnicodeString);
procedure CanvasTextOutSimplest_PChar(C: TCanvas; X, Y: integer; Buf: PChar; Len: integer); inline;

procedure CanvasTextOut(C: TCanvas;
  APosX, APosY: integer;
  const AText: UnicodeString;
  AParts: PATLineParts;
  out ATextWidth: Int64;
  const AProps: TATCanvasTextOutProps
  );

procedure CanvasTextOutMinimap(
  C: TBGRABitmap;
  const ARect: TRect;
  APosX, APosY: integer;
  const ACharSize: TATEditorCharSize;
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
  const ACharSize: TATEditorCharSize;
  AColorFont, AColorBG: TColor);

procedure DoPaintUnprintedEndSymbol(C: TCanvas;
  AX, AY: integer;
  const ACharSize: TATEditorCharSize;
  AColorFont, AColorBg: TColor);

procedure DoPaintUnprintedWrapMark(C: TCanvas;
  AX, AY: integer;
  const ACharSize: TATEditorCharSize;
  AColorFont: TColor);

function CanvasTextWidth(const S: atString; ALineIndex: integer;
  ATabHelper: TATStringTabHelper; ACharWidth: integer; AFontProportional: boolean): Int64; inline;

procedure UpdateWiderFlags(C: TCanvas; out Flags: TATWiderFlags);

var
  FTickTextout: QWord;

implementation

uses
  Math,
  LCLType,
  LCLIntf;

const
  //TextOut with bsClear is faster on Linux/macOS
  cTextoutBrushStyle = bsClear;

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
      Result[i]:= ' '; //not '?', because it will render '?' above gray 'x00'
  end;
end;


{$IF Defined(LCLWin32)}
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

procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: string);
begin
  //don't set Brush.Style here, causes CudaText issue #3625
  {$IF Defined(LCLWin32)}
  Windows.TextOutA(C.Handle, X, Y, PChar(S), Length(S));
  {$else}
  LCLIntf.TextOut(C.Handle, X, Y, PChar(S), Length(S));
  {$endif}
end;

procedure CanvasTextOutSimplest(C: TCanvas; X, Y: integer; const S: UnicodeString);
var
  Buf: string;
begin
  //don't set Brush.Style here, causes CudaText issue #3625
  {$IF Defined(LCLWin32)}
  Windows.TextOutW(C.Handle, X, Y, PWChar(S), Length(S));
  {$else}
  Buf:= UTF8Encode(S);
  LCLIntf.TextOut(C.Handle, X, Y, PChar(Buf), Length(Buf));
  {$endif}
end;


procedure CanvasTextOutSimplest_PChar(C: TCanvas; X, Y: integer; Buf: PChar; Len: integer); inline;
begin
  {$IF Defined(LCLWin32)}
  Windows.TextOutA(C.Handle, X, Y, Buf, Len);
  {$else}
  LCLIntf.TextOut(C.Handle, X, Y, Buf, Len);
  {$endif}
end;

procedure CanvasUnprintedSpace(C: TCanvas; const ARect: TRect;
  AScale: integer; AFontColor: TColor);
const
  cMinDotSize = 1; //should be 1, VSCode paints unprinted-space as 1 pixel
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
  const ACharSize: TATEditorCharSize;
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
    CanvasUnprintedSpace(C, R, ATEditorOptions.UnprintedSpaceDotScale, AColorFont)
  else
    CanvasArrowHorz(C, R,
      AColorFont,
      ATEditorOptions.UnprintedTabCharLength*ACharSize.XScaled div ATEditorCharXScale,
      true,
      ATEditorOptions.UnprintedTabPointerScale);
end;


procedure CanvasLineHorz(C: TCanvas; X1, Y, X2: integer; AWithEnd: boolean);
begin
  //Assert(X2>X1, 'LineHorz x2>x1');
  if AWithEnd then Inc(X2);
  {$IF Defined(LCLWin32)}
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
  {$IF Defined(LCLWin32)}
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
    TATLineStyle.None:
      exit;

    TATLineStyle.Solid:
      begin
        C.Pen.Color:= Color;
        CanvasLine_WithEnd(X1, Y1, X2, Y2);
      end;

    TATLineStyle.Solid2px:
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

    TATLineStyle.Dash:
      begin
        C.Pen.Color:= Color;
        C.Pen.Style:= psDot;
        CanvasLine_WithEnd(X1, Y1, X2, Y2);
        C.Pen.Style:= psSolid;
      end;

    TATLineStyle.Dotted:
      CanvasLine_Dotted(C, Color, X1, Y1, X2, Y2);

    TATLineStyle.Rounded:
      CanvasLine_RoundedEdge(C, Color, X1, Y1, X2, Y2, AtDown);

    TATLineStyle.Wave:
      CanvasLine_WavyHorz(C, Color, X1, Y1, X2, Y2, AtDown);
  end;
end;


procedure DoPaintHexChars(C: TCanvas;
  const AString: atString;
  ADx: PIntegerArray;
  AX, AY: integer;
  const ACharSize: TATEditorCharSize;
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
        C.Brush.Style:= bsClear;
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

procedure CanvasRenderChar(C: TCanvas;
  AColorFont, AColorBg: TColor;
  AChar: AnsiChar;
  X, Y, W, H: integer);
begin
  C.Pen.Color:= AColorFont;
  if AColorBg<>clNone then
  begin
    C.Brush.Color:= AColorBg;
    C.FillRect(X-1, Y-1, X+W+2, Y+H+2);
  end;

  case AChar of
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
    else
      raise Exception.Create('Unsupported char in CanvasRenderChar');
  end;
end;

procedure DoPaintUnprintedSymbols(C: TCanvas;
  ASymbols: TATLineEndsSymbols;
  AX, AY: integer;
  const ACharSize: TATEditorCharSize;
  AColorFont, AColorBG: TColor);
const
  cText: array[TATLineEndsSymbols] of string[4] = ('', 'LF', 'CRLF', 'CR', 'EOF');
var
  SText: string[4];
  X, Y, W, H: integer;
  i: integer;
begin
  H:= ACharSize.Y * ATEditorOptions.UnprintedEndFontScale div 100;
  W:= H div 2;

  X:= AX + 2;
  Y:= AY + ACharSize.Y div 2 - H div 2;

  SText:= cText[ASymbols];
  for i:= 1 to Length(SText) do
  begin
    CanvasRenderChar(C, AColorFont, AColorBG, SText[i], X, Y, W, H);
    Inc(X, W+2);
  end;
end;

procedure DoPaintUnprintedEndSymbol(C: TCanvas;
  AX, AY: integer;
  const ACharSize: TATEditorCharSize;
  AColorFont, AColorBg: TColor);
begin
  case ATEditorOptions.UnprintedEndSymbol of
    TATEditorUnptintedEolSymbol.Dot:
      CanvasUnprintedSpace(C,
        Rect(AX, AY, AX+ACharSize.XScaled div ATEditorCharXScale, AY+ACharSize.Y),
        ATEditorOptions.UnprintedEndDotScale,
        AColorFont);
    TATEditorUnptintedEolSymbol.ArrowDown:
      CanvasArrowDown(C,
        Rect(AX, AY, AX+ACharSize.XScaled div ATEditorCharXScale, AY+ACharSize.Y),
        AColorFont,
        ATEditorOptions.UnprintedEndArrowLength,
        ATEditorOptions.UnprintedTabPointerScale
        );
    TATEditorUnptintedEolSymbol.Pilcrow:
      CanvasPilcrowChar(C,
        Rect(AX, AY, AX+ACharSize.XScaled div ATEditorCharXScale, AY+ACharSize.Y),
        AColorFont,
        ATEditorOptions.UnprintedPilcrowScale);
  end;
end;

procedure DoPaintUnprintedWrapMark(C: TCanvas;
  AX, AY: integer;
  const ACharSize: TATEditorCharSize;
  AColorFont: TColor);
begin
  CanvasArrowWrapped(C,
    Rect(AX, AY, AX+ACharSize.XScaled div ATEditorCharXScale, AY+ACharSize.Y),
    AColorFont,
    ATEditorOptions.UnprintedWrapArrowLength,
    ATEditorOptions.UnprintedWrapArrowWidth,
    ATEditorOptions.UnprintedTabPointerScale
    )
end;


function CanvasTextWidth(const S: atString; ALineIndex: integer;
  ATabHelper: TATStringTabHelper; ACharWidth: integer; AFontProportional: boolean): Int64;
begin
  Result:= ATabHelper.CalcCharOffsetLast(ALineIndex, S) * ACharWidth div 100;
end;


function CanvasTextOutNeedsOffsets(C: TCanvas; const AStr: UnicodeString): boolean; inline;
begin
  if ATEditorOptions.TextoutNeedsOffsets then
    exit(true);

  //force Offsets not for all unicode.
  //only for those chars, which are full-width or "hex displayed" or unknown width.
  Result:= IsStringWithUnusualWidthChars(AStr);
end;

function CanvasTextOutNeedsOffsets(C: TCanvas; ABuf: PWideChar; ACount: integer): boolean;
begin
  if ATEditorOptions.TextoutNeedsOffsets then
    exit(true);

  Result:= IsStringWithUnusualWidthChars(ABuf, ACount);
end;


procedure _CalcCharSizesUtf8FromWidestring(const S: UnicodeString;
  DxIn: PInteger;
  DxInLen: integer;
  out DxOut: TATInt32FixedArray);
var
  NLen, NSize, ResLen, i: integer;
begin
  NLen:= Min(Length(S), cMaxFixedArray);
  DxOut:= Default(TATInt32FixedArray);

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
  ListOffsets: TATIntFixedArray;
  ListInt: TATIntFixedArray;
  Dx: TATInt32FixedArray; //must be with 'longint' items
  {$IF not Defined(LCLWin32)}
  DxUTF8: TATInt32FixedArray; //must be with 'longint' items
  {$endif}
  bPartsSpaces: packed array[0..High(TATLineParts)] of boolean;


procedure CanvasTextOut(C: TCanvas;
  APosX, APosY: integer;
  const AText: UnicodeString;
  AParts: PATLineParts;
  out ATextWidth: Int64;
  const AProps: TATCanvasTextOutProps);
  //
  function _IsCharSelected(AColumn: integer): boolean; inline;
  begin
    Result:= AProps.DetectIsPosSelected(AColumn-2+AProps.CharIndexInLine, AProps.LineIndex);
  end;
  //
  procedure _PaintUnprintedChars;
  var
    bShowUnprintedPartially: boolean;
    NPosFirstChar, NPosLastChar: integer;
    ch: WideChar;
    i: integer;
  begin
    bShowUnprintedPartially:= true;
    if AProps.ShowUnprintedSpacesOnlyInSelection then
    begin
      for i:= 1 to Length(AText) do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) and _IsCharSelected(i) then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
    end
    else
    if AProps.ShowUnprintedSpacesBothEnds then
    begin
      NPosFirstChar:= SGetIndentChars(AText);
      NPosLastChar:= Max(NPosFirstChar, SGetNonSpaceLength(AText))+1;
      //paint leading
      for i:= 1 to NPosFirstChar do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
      //paint trailing
      for i:= NPosLastChar to Length(AText) do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
      //paint middle if "also in selection"
      if AProps.ShowUnprintedSpacesAlsoInSelection then
        for i:= NPosFirstChar+1 to NPosLastChar-1 do
        begin
          ch:= AText[i];
          if IsCharUnicodeSpace(ch) and _IsCharSelected(i) then
            DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
        end;
    end
    else
    if AProps.ShowUnprintedSpacesTrailing then
    begin
      NPosLastChar:= SGetNonSpaceLength(AText)+1;
      //paint trailing
      if not AProps.TrimmedTrailingNonSpaces then
        for i:= NPosLastChar to Length(AText) do
        begin
          ch:= AText[i];
          if IsCharUnicodeSpace(ch) then
            DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
        end;
      //paint middle if "also in selection"
      if AProps.ShowUnprintedSpacesAlsoInSelection then
        for i:= 1 to NPosLastChar-1 do
        begin
          ch:= AText[i];
          if IsCharUnicodeSpace(ch) and _IsCharSelected(i) then
            DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
        end;
    end
    else
    begin
      //paint all
      bShowUnprintedPartially:= false;
      for i:= 1 to Length(AText) do
      begin
        ch:= AText[i];
        if IsCharUnicodeSpace(ch) then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
    end;

    if AProps.ShowUnprintedForceTabs and bShowUnprintedPartially then
    begin
      //paint all tab-chars
      for i:= 1 to Length(AText) do
      begin
        ch:= AText[i];
        if ch=#9 then
          DoPaintUnprintedChar(C, ch, i, ListInt, APosX, APosY, AProps.CharSize, AProps.ColorUnprintedFont);
      end;
    end;
  end;
  //
  procedure _PaintAllBorders(APart: PATLinePart; ALeft, ATop, ARight, ABottom: integer);
  begin
    CanvasLineEx(C,
      APart^.ColorBorder,
      APart^.BorderDown,
      ALeft, ABottom,
      ARight, ABottom,
      true);

    CanvasLineEx(C,
      APart^.ColorBorder,
      APart^.BorderUp,
      ALeft, ATop,
      ARight, ATop,
      false);

    CanvasLineEx(C,
      APart^.ColorBorder,
      APart^.BorderLeft,
      ALeft, ATop,
      ALeft, ABottom,
      false);

    CanvasLineEx(C,
      APart^.ColorBorder,
      APart^.BorderRight,
      ARight, ATop,
      ARight, ABottom,
      true);
  end;
  //
  procedure _PrepareCanvasProps(APart: PATLinePart; out bItalic: boolean);
  var
    NStyles: integer;
    bBold: boolean;
  begin
    C.Brush.Color:= APart^.ColorBG;

    NStyles:= APart^.FontStyles;
    bBold:= (NStyles and afsFontBold)<>0;
    bItalic:= (NStyles and afsFontItalic)<>0;
    //bCrossed:= (NStyles and afsFontCrossed)<>0;

    C.Font.Style:= ConvertIntegerToFontStyles(NStyles);
    C.Font.Color:= APart^.ColorFont;

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
  end;
  //
  procedure _PaintWithoutParts;
  var
    Buf: string;
    BufW: UnicodeString;
    DxPointer: PInteger;
  begin
    if AProps.HasAsciiNoTabs and not ATEditorOptions.TextoutNeedsOffsets then
    begin
      BufW:= AText;
      DxPointer:= nil;
    end
    else
    begin
      BufW:= SRemoveHexDisplayedChars(AText);
      if AProps.FontProportional or CanvasTextOutNeedsOffsets(C, AText) then
        DxPointer:= @Dx.Data[0]
      else
        DxPointer:= nil;
    end;

    C.Brush.Style:= cTextoutBrushStyle;

    {$ifdef LCLWin32}
    _TextOut_Windows(C.Handle, APosX, APosY+AProps.SpacingTop, nil, BufW, DxPointer, false{no ligatures});
    {$else}
    Buf:= BufW;
    _TextOut_Unix(C.Handle, APosX, APosY+AProps.SpacingTop, nil, Buf, DxPointer);
    {$endif}
  end;
  //
var
  PartStr: UnicodeString;
  PartRect: TRect;
  PartOffset, PixOffset1, PixOffset2: integer;
  //
  {$ifdef LCLWin32}
  procedure _PaintPart_Windows;
  var
    BufW: UnicodeString = '';
    DxPointer: PInteger = nil;
    bAllowLigatures: boolean;
    tick: QWord;
  begin
    if AProps.HasAsciiNoTabs and not ATEditorOptions.TextoutNeedsOffsets then
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

      if AProps.FontProportional or CanvasTextOutNeedsOffsets(C, PartStr) then
        DxPointer:= @Dx.Data[PartOffset]
      else
        DxPointer:= nil;
    end;

    if ATEditorOptions.DebugTiming then
      tick:= GetTickCount64;
    _TextOut_Windows(C.Handle,
      APosX+PixOffset1,
      APosY+AProps.SpacingTopEdge+AProps.SpacingTop,
      @PartRect,
      BufW,
      DxPointer,
      bAllowLigatures
      );
    if ATEditorOptions.DebugTiming then
      FTickTextout+= GetTickCount64-tick;
  end;
  //
  {$else}
  procedure _PaintPart_NonWindows;
  var
    Buf: string = '';
    BufW: UnicodeString = '';
    DxPointer: PInteger = nil;
    i: integer;
    tick: QWord;
  begin
    if AProps.HasAsciiNoTabs and not ATEditorOptions.TextoutNeedsOffsets then
    begin
      SetLength(Buf, Length(PartStr));
      for i:= 1 to Length(PartStr) do
        Buf[i]:= chr(ord(PartStr[i]));
      DxPointer:= nil;
    end
    else
    begin
      BufW:= PartStr;
      Buf:= UTF8Encode(SRemoveHexDisplayedChars(BufW));

      if AProps.FontProportional or CanvasTextOutNeedsOffsets(C, PartStr) then
      begin
        _CalcCharSizesUtf8FromWidestring(BufW, @Dx.Data[PartOffset], Dx.Len-PartOffset, DxUTF8);
        DxPointer:= @DxUTF8.Data[0];
      end
      else
        DxPointer:= nil;
    end;

    if ATEditorOptions.DebugTiming then
      tick:= GetTickCount64;
    _TextOut_Unix(C.Handle,
      APosX+PixOffset1,
      APosY+AProps.SpacingTopEdge+AProps.SpacingTop,
      @PartRect,
      Buf,
      DxPointer
      );
    if ATEditorOptions.DebugTiming then
      FTickTextout+= GetTickCount64-tick;
  end;
  {$endif}
  //
var
  NLen, NCharWidthScaled, NDeltaForItalic, iPart, i: integer;
  NLastPart: integer;
  PartPtr: PATLinePart;
  PartLen: integer;
  PrevColor: TColor;
  bPrevColorInited: boolean;
  bItalic: boolean;
begin
  NLen:= Min(Length(AText), cMaxFixedArray);
  if NLen=0 then Exit;
  NCharWidthScaled:= AProps.CharSize.XScaled;
  NDeltaForItalic:= C.Font.Size * ATEditorOptions.ItalicFontLongerInPercents div 100;

  ListInt:= Default(TATIntFixedArray);
  Dx:= Default(TATInt32FixedArray);
  //no need to clear DxUTF8
  FillChar(bPartsSpaces, SizeOf(bPartsSpaces), 0);

  if AProps.SuperFast or AProps.HasAsciiNoTabs then
  begin
    ListInt.Len:= NLen;
    Dx.Len:= NLen;
    for i:= 0 to NLen-1 do
    begin
      ListInt.Data[i]:= NCharWidthScaled*(i+1) div ATEditorCharXScale;
      Dx.Data[i]:= NCharWidthScaled div ATEditorCharXScale;
    end;
  end
  else
  begin
    AProps.TabHelper.CalcCharOffsets(AProps.LineIndex, AText, ListOffsets, AProps.CharsSkipped);

    ListInt.Len:= ListOffsets.Len;
    Dx.Len:= ListOffsets.Len;

    for i:= 0 to ListOffsets.Len-1 do
      ListInt.Data[i]:= ListOffsets.Data[i] * NCharWidthScaled div 100 div ATEditorCharXScale;

    Dx.Data[0]:= ListInt.Data[0];
    for i:= 1 to ListInt.Len-1 do
      Dx.Data[i]:= ListInt.Data[i]-ListInt.Data[i-1];
  end;

  if AParts=nil then
  begin
    _PaintWithoutParts;

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
    //first, process space-only parts
    PrevColor:= clRed;
    bPrevColorInited:= false;
    for iPart:= 0 to High(TATLineParts) do
    begin
      PartPtr:= @AParts^[iPart];
      PartLen:= PartPtr^.Len;
      if PartLen=0 then Break;
      PartOffset:= PartPtr^.Offset;
      if PartOffset>ListInt.Len then Break;
      if not IsStringSpaces(AText, PartOffset+1, PartLen) then Continue;
      bPartsSpaces[iPart]:= true;

      if PartOffset>0 then
        PixOffset1:= ListInt.Data[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, ListInt.Len);
      if i>0 then
        PixOffset2:= ListInt.Data[i-1]
      else
        PixOffset2:= 0;

      PartRect:= Rect(
        APosX+PixOffset1,
        APosY,
        APosX+PixOffset2,
        APosY+AProps.CharSize.Y);

      {
      //avoid clipping previous 'italic' part
      if (iPart>0) and ((AParts^[iPart-1].FontStyles and afsFontItalic)<>0) then
        Inc(PartRect.Left, NDeltaForItalic);
      }
      if (not bPrevColorInited) or (PartPtr^.ColorBG<>PrevColor) then
      begin
        PrevColor:= PartPtr^.ColorBG;
        bPrevColorInited:= true;
        C.Brush.Color:= PrevColor;
      end;

      C.FillRect(PartRect);
    end;

    //next, process non-space parts
    NLastPart:= 0;
    for iPart:= 0 to High(TATLineParts) do
    begin
      if bPartsSpaces[iPart] then Continue;
      PartPtr:= @AParts^[iPart];
      PartLen:= PartPtr^.Len;
      if PartLen=0 then Break;
      PartOffset:= PartPtr^.Offset;
      if PartOffset>ListInt.Len then Break;
      PartStr:= Copy(AText, PartOffset+1, PartLen);
      if PartStr='' then Break;
      NLastPart:= iPart;

      if PartOffset>0 then
        PixOffset1:= ListInt.Data[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, ListInt.Len);
      if i>0 then
        PixOffset2:= ListInt.Data[i-1]
      else
        PixOffset2:= 0;

      _PrepareCanvasProps(PartPtr, bItalic);

      PartRect:= Rect(
        APosX+PixOffset1,
        APosY,
        APosX+PixOffset2,
        APosY+AProps.CharSize.Y);

      //increase rect to avoid clipping of italic font at line end,
      //eg comment //WWW, if theme has italic comments style,
      //with font eg "Fira Code Retina"
      if bItalic then
        Inc(PartRect.Right, NDeltaForItalic);

      C.Brush.Style:= cTextoutBrushStyle;

      {$ifdef LCLWin32}
      _PaintPart_Windows;
      {$else}
      _PaintPart_NonWindows;
      {$endif}

      if not AProps.HasAsciiNoTabs then
        DoPaintHexChars(C,
          PartStr,
          @Dx.Data[PartOffset],
          APosX+PixOffset1,
          APosY+AProps.SpacingTopEdge+AProps.SpacingTop,
          AProps.CharSize,
          AProps.ColorUnprintedHexFont,
          PartPtr^.ColorBG,
          AProps.SuperFast
          );

      _PaintAllBorders(
        PartPtr,
        PartRect.Left,
        PartRect.Top,
        PartRect.Right-1,
        PartRect.Bottom-1
        );
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
      C.Brush.Style:= cTextoutBrushStyle;

      {$IF Defined(LCLWin32)}
      _TextOut_Windows(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.SpacingTopEdge+AProps.SpacingTop,
        nil,
        PartStr,
        nil,
        false
        );
      {$else}
      _TextOut_Unix(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.SpacingTopEdge+AProps.SpacingTop,
        nil,
        PartStr,
        nil
        );
      {$endif}
    end;
  end;

  if AProps.ShowUnprinted then
    _PaintUnprintedChars;

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
  const ARect: TRect;
  APosX, APosY: integer;
  const ACharSize: TATEditorCharSize;
  ATabSize: integer;
  constref AParts: TATLineParts;
  AColorBG: TColor;
  AColorAfter: TColor;
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
    for NCharIndex:= Part^.Offset+1 to
                     Min(Part^.Offset+Part^.Len, Length(ALine)) do
    begin
      case ALine[NCharIndex] of
        ' ':
          begin
            bSpace:= true;
            NSpaceThis:= 1;
          end;
        #9:
          begin
            bSpace:= true;
            NSpaceThis:= ATabSize;
          end
        else
          begin
            bSpace:= false;
            NSpaceThis:= 1;
          end;
      end;

      X1:= APosX + ACharSize.XScaled*NSpaces div ATEditorCharXScale;
      X2:= X1 + ACharSize.XScaled*NSpaceThis div ATEditorCharXScale;

      if X1>ARect.Right then Break;
      Inc(NSpaces, NSpaceThis);

      if AUsePixels and (NSpaceThis=1) then
      begin
        if bHasBG then
          C.SetPixel(X1, Y1, rColorBack);
        if bSpace then
          C.SetPixel(X1, Y2b, rColorBack)
        else
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
