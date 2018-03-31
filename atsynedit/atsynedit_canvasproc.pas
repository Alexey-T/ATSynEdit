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
  ATStringProc,
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

const
  //Win: seems no slowdown from offsets
  //OSX: better use offsets, fonts have float-width, e.g. 10.2 pixels
  //Linux gtk2: big slowdown from offsets
  CanvasTextOutMustUseOffsets = {$ifdef linux} false {$else} true {$endif};
var
  CanvasTextOutHorzSpacingUsed: boolean = false;

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

type
  TATLinePartClass = class
  public
    Data: TATLinePart;
  end;

const
  cMaxLineParts = 1000; //big two monitors have total about 1000 chars (small font)
type
  TATLineParts = array[0..cMaxLineParts-1] of TATLinePart;
  PATLineParts = ^TATLineParts;

type
  TATSynEditDrawLineEvent = procedure(Sender: TObject; C: TCanvas;
    AX, AY: integer; const AStr: atString; ACharSize: TPoint;
    const AExtent: TATIntArray) of object;

type
  TATCanvasTextOutProps = record
    NeedOffsets: TATFontNeedsOffsets;
    TabSize: integer;
    CharSize: TPoint;
    MainTextArea: boolean;
    CharsSkipped: integer;
    DrawEvent: TATSynEditDrawLineEvent;
    ControlWidth: integer;
    TextOffsetFromLine: integer;
    ShowUnprinted: boolean;
    ShowUnprintedSpacesTrailing: boolean;
    ShowFontLigatures: boolean;
    ColorUnprintedFont: TColor;
    ColorUnprintedHexFont: TColor;
  end;

procedure CanvasLineEx(C: TCanvas;
  Color: TColor; Style: TATLineStyle;
  P1, P2: TPoint; AtDown: boolean);

procedure CanvasArrowHorz(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AArrowLen: integer;
  AToRight: boolean);

procedure CanvasTextOut(C: TCanvas;
  APosX, APosY: integer;
  AText: atString;
  AParts: PATLineParts;
  out ATextWidth: integer;
  const AProps: TATCanvasTextOutProps
  );

procedure CanvasTextOutMinimap(C: TCanvas;
  const AStr: atString;
  const ARect: TRect;
  APos: TPoint;
  ACharSize: TPoint;
  ATabSize: integer;
  AParts: PATLineParts;
  AColorBG: TColor
  );

procedure DoPaintUnprintedEol(C: TCanvas;
  const AStrEol: atString;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont, AColorBG: TColor;
  ADetails: boolean);

function CanvasTextWidth(const S: atString; ATabSize: integer; ACharSize: TPoint): integer;
function CanvasFontSizes(C: TCanvas): TPoint;
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
procedure CanvasDottedVertLine_Alt(C: TCanvas; Color: TColor; X1, Y1, Y2: integer);
procedure CanvasDottedHorzVertLine(C: TCanvas; Color: TColor; P1, P2: TPoint);
procedure CanvasWavyHorzLine(C: TCanvas; Color: TColor; P1, P2: TPoint; AtDown: boolean);

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
procedure CanvasPaintTriangleRight(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
procedure CanvasPaintPlusMinus(C: TCanvas; AColorBorder, AColorBG: TColor; ACenter: TPoint; ASize: integer; APlus: boolean);

procedure DoPartFind(const AParts: TATLineParts; APos: integer; out AIndex, AOffsetLeft: integer);
function DoPartInsert(var AParts: TATLineParts; var APart: TATLinePart; AKeepFontStyles: boolean): boolean;
procedure DoPartSetColorBG(var AParts: TATLineParts; AColor: TColor; AForceColor: boolean);
procedure DoPartsShow(var P: TATLineParts);
procedure DoPartsDim(var P: TATLineParts; ADimLevel255: integer);


implementation

uses
  Math,
  LCLType,
  LCLIntf;

var
  _Pen: TPen = nil;

type
  TATBorderSide = (cSideLeft, cSideRight, cSideUp, cSideDown);

function IsStringSymbolsOnly(const S: string): boolean;
var
  i, N: integer;
begin
  if S='' then exit(false);
  for i:= 1 to Length(S) do
  begin
    N:= Ord(S[i]);
    if (N<32) or (N>Ord('~')) then
      exit(false);
  end;
  Result:= true;
end;


//override LCLIntf.ExtTextOut on Win32 to draw font ligatures
function _SelfTextOut(DC: HDC; X, Y: Integer; Rect: PRect;
  const Str: String;
  Dx: PInteger;
  AllowLigatures: boolean
  ): boolean;
{$ifdef windows}
var
  CharPlaceInfo: TGCPRESULTS;
  Glyphs: array of WideChar;
{$endif}
begin
  {$ifdef windows}
  if AllowLigatures then
  begin
    ZeroMemory(@CharPlaceInfo, SizeOf(CharPlaceInfo));
    CharPlaceInfo.lStructSize:= SizeOf(CharPlaceInfo);
    SetLength(Glyphs, Length(Str));
    CharPlaceInfo.lpGlyphs:= @Glyphs[0];
    CharPlaceInfo.nGlyphs:= Length(Glyphs);

    if GetCharacterPlacement(DC, PChar(Str), Length(Str), 0, @CharPlaceInfo, GCP_LIGATE)<> 0 then
      Result:= Windows.ExtTextOut(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE or ETO_GLYPH_INDEX, Rect, Pointer(Glyphs), Length(Glyphs), Dx)
    else
      Result:= ExtTextOut(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE, Rect, PChar(Str), Length(Str), Dx);
  end
  else
  {$endif}
  Result:= ExtTextOut(DC, X, Y, ETO_CLIPPED or ETO_OPAQUE, Rect, PChar(Str), Length(Str), Dx);
end;


procedure CanvasUnprintedSpace(C: TCanvas; const ARect: TRect;
  AScale: integer; AFontColor: TColor);
const
  cMinDotSize = 2;
var
  R: TRect;
  NSize: integer;
begin
  NSize:= Max(cMinDotSize, (ARect.Bottom-ARect.Top) * AScale div 100);
  R.Left:= (ARect.Left+ARect.Right) div 2 - NSize div 2;
  R.Top:= (ARect.Top+ARect.Bottom) div 2 - NSize div 2;
  R.Right:= R.Left + NSize;
  R.Bottom:= R.Top + NSize;
  C.Pen.Color:= AFontColor;
  C.Brush.Color:= AFontColor;
  C.FillRect(R);
end;

procedure CanvasArrowHorz(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AArrowLen: integer; //OptUnprintedTabCharLength*ACharSizeX
  AToRight: boolean);
const
  cIndent = 1; //offset left/rt
var
  XLeft, XRight, X1, X2, Y, Dx: integer;
begin
  XLeft:= ARect.Left+cIndent;
  XRight:= ARect.Right-cIndent;

  if AArrowLen=0 then
  begin;
    X1:= XLeft;
    X2:= XRight;
  end
  else
  begin
    X1:= XLeft;
    X2:= Min(XRight, X1+AArrowLen);
  end;

  Y:= (ARect.Top+ARect.Bottom) div 2;
  Dx:= (ARect.Bottom-ARect.Top) * OptUnprintedTabPointerScale div 100;
  C.Pen.Color:= AColorFont;

  C.Line(X1, Y, X2, Y);
  if AToRight then
  begin
    C.MoveTo(X2, Y);
    C.LineTo(X2-Dx, Y-Dx);
    C.MoveTo(X2, Y);
    C.LineTo(X2-Dx, Y+Dx);
  end
  else
  begin
    C.MoveTo(X1, Y);
    C.LineTo(X1+Dx, Y-Dx);
    C.MoveTo(X1, Y);
    C.LineTo(X1+Dx, Y+Dx);
  end;
end;


procedure CanvasArrowDown(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor);
var
  Len, X, Y1, Y2, Dx: integer;
begin
  X:= (ARect.Left+ARect.Right) div 2;
  Len:= (ARect.Bottom-ARect.Top) * OptUnprintedEndArrowLength div 100;
  Dx:= (ARect.Bottom-ARect.Top) * OptUnprintedTabPointerScale div 100;
  C.Pen.Color:= AColorFont;

  Y1:= (ARect.Bottom+ARect.Top-Len) div 2;
  Y2:= Y1+Len;

  C.MoveTo(X, Y1);
  C.LineTo(X, Y2);
  C.MoveTo(X, Y2);
  C.LineTo(X-Dx, Y2-Dx);
  C.MoveTo(X, Y2);
  C.LineTo(X+Dx, Y2-Dx);
end;

procedure DoPaintUnprintedChars(C: TCanvas;
  const AString: atString;
  const AOffsets: TATIntArray;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont: TColor;
  ATailOnly: boolean);
var
  ch: WideChar;
  R: TRect;
  NPos, i: integer;
begin
  NPos:= 1;

  if ATailOnly then
  begin
    NPos:= Length(AString)+1;
    while (NPos>1) and IsCharSpace(AString[NPos-1]) do
      Dec(NPos);
  end;

  for i:= NPos to Length(AString) do
  begin
    ch:= AString[i];
    if IsCharSpace(ch) then
    begin
      R.Left:= APoint.X;
      R.Right:= APoint.X;
      if i>1 then
        Inc(R.Left, AOffsets[i-2]);
      Inc(R.Right, AOffsets[i-1]);

      R.Top:= APoint.Y;
      R.Bottom:= R.Top+ACharSize.Y;

      if ch<>#9 then
        CanvasUnprintedSpace(C, R, OptUnprintedSpaceDotScale, AColorFont)
      else
        CanvasArrowHorz(C, R, AColorFont, OptUnprintedTabCharLength*ACharSize.X, true);
    end;
  end;
end;

procedure CanvasSimpleLine(C: TCanvas; P1, P2: TPoint);
begin
  if P1.Y=P2.Y then
    C.Line(P1.X, P1.Y, P2.X+1, P2.Y)
  else
    C.Line(P1.X, P1.Y, P2.X, P2.Y+1);
end;

procedure CanvasRoundedLine(C: TCanvas; Color: TColor; P1, P2: TPoint; AtDown: boolean);
var
  Points: array[0..3] of TPoint;
begin
  C.Pen.Color:= Color;
  if P1.Y=P2.Y then
  begin
    //paint polyline, 4 points, horz line and 2 edges
    Points[1]:= Point(P1.X+2, P1.Y);
    Points[2]:= Point(P2.X-2, P2.Y);
    if AtDown then
    begin
      Points[0]:= Point(P1.X, P1.Y-2);
      Points[3]:= Point(P2.X+1, P2.Y-3);
    end
    else
    begin
      Points[0]:= Point(P1.X, P1.Y+2);
      Points[3]:= Point(P2.X+1, P2.Y+3);
    end;
    C.Polyline(Points);
  end
  else
  begin
    C.Line(P1.X, P1.Y+2, P2.X, P2.Y-1);
    //don't draw pixels, other lines did it
  end;
end;

procedure CanvasWavyHorzLine(C: TCanvas; Color: TColor; P1, P2: TPoint; AtDown: boolean);
const
  cWavePeriod = 2;
  cWaveInc: array[0..cWavePeriod-1] of integer = (0, 2);
var
  Points: array of TPoint;
  x, y, sign: integer;
begin
  SetLength(Points, 0);
  if AtDown then sign:= -1 else sign:= 1;
  for x:= P1.X to P2.X do
    if not Odd(x) then
    begin
      y:= P2.Y + sign * cWaveInc[(x-P1.X) div 2 mod cWavePeriod];
      SetLength(Points, Length(Points)+1);
      Points[Length(Points)-1]:= Point(x, y);
    end;

  C.Pen.Color:= Color;
  if Length(Points)>0 then
    C.Polyline(Points);
end;

procedure CanvasDottedHorzVertLine(C: TCanvas; Color: TColor; P1, P2: TPoint);
var
  i: integer;
begin
  if P1.Y=P2.Y then
  begin
    for i:= P1.X to P2.X do
      if Odd(i-P1.X+1) then
        C.Pixels[i, P2.Y]:= Color;
  end
  else
  begin
    for i:= P1.Y to P2.Y do
      if Odd(i-P1.Y+1) then
        C.Pixels[P1.X, i]:= Color;
  end;
end;

procedure CanvasLineEx(C: TCanvas; Color: TColor; Style: TATLineStyle; P1, P2: TPoint; AtDown: boolean);
begin
  case Style of
    cLineStyleSolid:
      begin
        C.Pen.Color:= Color;
        CanvasSimpleLine(C, P1, P2);
      end;

    cLineStyleSolid2px:
      begin
        C.Pen.Color:= Color;
        CanvasSimpleLine(C, P1, P2);
        if P1.Y=P2.Y then
        begin
          if AtDown then
            begin Dec(P1.Y); Dec(P2.Y) end
          else
            begin Inc(P1.Y); Inc(P2.Y) end;
        end
        else
        begin
          if AtDown then
            begin Dec(P1.X); Dec(P2.X) end
          else
            begin Inc(P1.X); Inc(P2.X) end;
        end;
        CanvasSimpleLine(C, P1, P2);
      end;

    cLineStyleDash:
      begin
        C.Pen.Color:= Color;
        C.Pen.Style:= psDot;
        CanvasSimpleLine(C, P1, P2);
        C.Pen.Style:= psSolid;
      end;

    cLineStyleDotted:
      CanvasDottedHorzVertLine(C, Color, P1, P2);

    cLineStyleRounded:
      CanvasRoundedLine(C, Color, P1, P2, AtDown);

    cLineStyleWave:
      CanvasWavyHorzLine(C, Color, P1, P2, AtDown);
  end;
end;

procedure DoPaintBorder(C: TCanvas; Color: TColor; R: TRect; Side: TATBorderSide; Style: TATLineStyle);
begin
  if Style=cLineStyleNone then Exit;
  Dec(R.Right);
  Dec(R.Bottom);

  case Side of
    cSideDown:
      CanvasLineEx(C, Color, Style,
        Point(R.Left, R.Bottom),
        Point(R.Right, R.Bottom),
        true);
    cSideLeft:
      CanvasLineEx(C, Color, Style,
        Point(R.Left, R.Top),
        Point(R.Left, R.Bottom),
        false);
    cSideRight:
      CanvasLineEx(C, Color, Style,
        Point(R.Right, R.Top),
        Point(R.Right, R.Bottom),
        true);
    cSideUp:
      CanvasLineEx(C, Color, Style,
        Point(R.Left, R.Top),
        Point(R.Right, R.Top),
        false);
  end;
end;


procedure DoPaintHexChars(C: TCanvas;
  const AString: atString;
  ADx: PIntegerArray;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont,
  AColorBg: TColor);
var
  Buf: string;
  R: TRect;
  i, j, NCode: integer;
begin
  if AString='' then Exit;

  for i:= 1 to Length(AString) do
    if IsCharHex(AString[i]) then
    begin
      R.Left:= APoint.X;
      R.Right:= APoint.X;

      for j:= 0 to i-2 do
        Inc(R.Left, ADx^[j]);
      R.Right:= R.Left+ADx^[i-1];

      R.Top:= APoint.Y;
      R.Bottom:= R.Top+ACharSize.Y;

      C.Font.Color:= AColorFont;
      C.Brush.Color:= AColorBg;

      NCode:= Ord(AString[i]);
      Buf:= 'x'+IntToHex(NCode, IfThen(NCode<$100, 2, 4));

      ExtTextOut(C.Handle,
        R.Left, R.Top,
        ETO_CLIPPED+ETO_OPAQUE,
        @R,
        PChar(Buf),
        Length(Buf),
        nil);
    end;
end;

procedure DoPaintUnprintedEol(C: TCanvas;
  const AStrEol: atString;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont, AColorBG: TColor;
  ADetails: boolean);
var
  NPrevSize: integer;
begin
  if AStrEol='' then Exit;

  if ADetails then
  begin
    NPrevSize:= C.Font.Size;
    C.Font.Size:= C.Font.Size * OptUnprintedEndFontScale div 100;
    C.Font.Color:= AColorFont;
    C.Brush.Color:= AColorBG;
    C.TextOut(
      APoint.X+OptUnprintedEndFontDx,
      APoint.Y+OptUnprintedEndFontDy,
      AStrEol);
    C.Font.Size:= NPrevSize;
  end
  else
  begin
    if OptUnprintedEndArrowOrDot then
      CanvasArrowDown(C,
        Rect(APoint.X, APoint.Y, APoint.X+ACharSize.X, APoint.Y+ACharSize.Y),
        AColorFont)
    else
      CanvasUnprintedSpace(C,
        Rect(APoint.X, APoint.Y, APoint.X+ACharSize.X, APoint.Y+ACharSize.Y),
        OptUnprintedEndDotScale,
        AColorFont);
  end;
end;


function CanvasFontSizes(C: TCanvas): TPoint;
var
  Size: TSize;
begin
  Size:= C.TextExtent('M');
  Result.X:= Size.cx;
  Result.Y:= Size.cy;
end;

function CanvasTextWidth(const S: atString; ATabSize: integer; ACharSize: TPoint): integer;
var
  Offsets: TATLineOffsetsInfo;
begin
  Result:= 0;
  if S='' then Exit;
  SCalcCharOffsets(S, Offsets, ATabSize);
  Result:= Offsets[High(Offsets)] * ACharSize.X div 100;
end;


function CanvasTextOutNeedsOffsets(C: TCanvas; const AStr: atString;
  const AOffsets: TATFontNeedsOffsets): boolean;
var
  St: TFontStyles;
begin
  if CanvasTextOutMustUseOffsets then exit(true);
  if CanvasTextOutHorzSpacingUsed then exit(true);

  //detect result but presence of bold/italic tokens, for them offsets needed
  //ignore underline, strikeout
  St:= C.Font.Style * [fsBold, fsItalic];

  if St=[] then Result:= AOffsets.ForNormal else
   if St=[fsBold] then Result:= AOffsets.ForBold else
    if St=[fsItalic] then Result:= AOffsets.ForItalic else
     if St=[fsBold, fsItalic] then Result:= AOffsets.ForBoldItalic else
      Result:= false;

  if Result then exit;
  Result:= IsStringWithUnicodeChars(AStr);
end;

procedure CanvasTextOut(C: TCanvas; APosX, APosY: integer; AText: atString;
  AParts: PATLineParts; out ATextWidth: integer;
  const AProps: TATCanvasTextOutProps);
var
  ListOffsets: TATLineOffsetsInfo;
  ListInt: TATIntArray;
  Dx: TATIntArray;
  i, j: integer;
  PartStr: atString;
  PartOffset, PartLen,
  PixOffset1, PixOffset2: integer;
  PartPtr: ^TATLinePart;
  PartFontStyle: TFontStyles;
  PartRect: TRect;
  Buf: string;
  DxPointer: PInteger;
  bAllowLigatures: boolean;
begin
  if AText='' then Exit;

  SetLength(ListInt, Length(AText));
  SetLength(Dx, Length(AText));

  SCalcCharOffsets(AText, ListOffsets, AProps.TabSize, AProps.CharsSkipped);

  for i:= 0 to High(ListOffsets) do
    ListInt[i]:= ListOffsets[i] * AProps.CharSize.X div 100;

  //truncate AText, to not paint over screen
  for i:= 1 to High(ListInt) do
    if ListInt[i]>AProps.ControlWidth then
    begin
      SetLength(AText, i);
      break;
    end;

  for i:= 0 to High(ListInt) do
    if i=0 then
      Dx[i]:= ListInt[i]
    else
      Dx[i]:= ListInt[i]-ListInt[i-1];

  if AParts=nil then
  begin
    Buf:= UTF8Encode(SRemoveHexChars(AText));
    SReplaceAllTabsToOneSpace(Buf);
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
      C.Brush.Color
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

      PartRect:= Rect(
        APosX+PixOffset1,
        APosY,
        APosX+PixOffset2,
        APosY+AProps.CharSize.Y);

      Buf:= UTF8Encode(SRemoveHexChars(PartStr));
      SReplaceAllTabsToOneSpace(Buf);
      if CanvasTextOutNeedsOffsets(C, PartStr, AProps.NeedOffsets) then
        DxPointer:= @Dx[PartOffset]
      else
        DxPointer:= nil;

      bAllowLigatures:=
        {$ifdef windows}
        AProps.ShowFontLigatures and
        IsStringSymbolsOnly(Buf) and //disable if unicode chrs
        (Pos(#9, PartStr)=0); //incorrect for str with tabs
        {$else}
        false;
        {$endif}

      _SelfTextOut(C.Handle,
        APosX+PixOffset1,
        APosY+AProps.TextOffsetFromLine,
        @PartRect,
        Buf,
        DxPointer,
        bAllowLigatures
        );

      DoPaintHexChars(C,
        PartStr,
        @Dx[PartOffset],
        Point(
          APosX+PixOffset1,
          APosY+AProps.TextOffsetFromLine),
        AProps.CharSize,
        AProps.ColorUnprintedHexFont,
        PartPtr^.ColorBG
        );

      if AProps.MainTextArea then
      begin
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideDown, PartPtr^.BorderDown);
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideUp, PartPtr^.BorderUp);
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideLeft, PartPtr^.BorderLeft);
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideRight, PartPtr^.BorderRight);
      end;
    end;

  if AProps.ShowUnprinted then
    DoPaintUnprintedChars(
      C,
      AText,
      ListInt,
      Point(APosX, APosY),
      AProps.CharSize,
      AProps.ColorUnprintedFont,
      AProps.ShowUnprintedSpacesTrailing
      );

  ATextWidth:= ListInt[High(ListInt)];

  if AText<>'' then
    if Assigned(AProps.DrawEvent) then
      AProps.DrawEvent(nil, C, APosX, APosY, AText, AProps.CharSize, ListInt);
end;


{$ifdef invert_pixels}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  i, j: integer;
begin
  for j:= R.Top to R.Bottom-1 do
    for i:= R.Left to R.Right-1 do
      C.Pixels[i, j]:= C.Pixels[i, j] xor (not AColor and $ffffff);
end;
{$else}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  X: integer;
  AM: TAntialiasingMode;
begin
  AM:= C.AntialiasingMode;
  _Pen.Assign(C.Pen);

  X:= (R.Left+R.Right) div 2;
  C.Pen.Mode:= {$ifdef darwin} pmNot {$else} pmNotXor {$endif};
  C.Pen.Style:= psSolid;
  C.Pen.Color:= AColor;
  C.AntialiasingMode:= amOff;
  C.Pen.EndCap:= pecFlat;
  C.Pen.Width:= R.Right-R.Left;

  C.MoveTo(X, R.Top);
  C.LineTo(X, R.Bottom);

  C.Pen.Assign(_Pen);
  C.AntialiasingMode:= AM;
  C.Rectangle(0, 0, 0, 0); //apply pen
end;
{$endif}

procedure CanvasDottedVertLine_Alt(C: TCanvas; Color: TColor; X1, Y1, Y2: integer);
var
  j: integer;
begin
  for j:= Y1 to Y2 do
    if Odd(j) then
      C.Pixels[X1, j]:= Color;
end;


procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y - ASize),
    Point(ACoord.X + ASize*2, ACoord.Y - ASize),
    Point(ACoord.X, ACoord.Y + ASize)
    ]);
end;

procedure CanvasPaintTriangleRight(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize, ACoord.Y - ASize*2),
    Point(ACoord.X + ASize, ACoord.Y),
    Point(ACoord.X - ASize, ACoord.Y + ASize*2)
    ]);
end;


procedure CanvasPaintPlusMinus(C: TCanvas; AColorBorder, AColorBG: TColor;
  ACenter: TPoint; ASize: integer; APlus: boolean);
begin
  C.Brush.Color:= AColorBG;
  C.Pen.Color:= AColorBorder;
  C.Rectangle(ACenter.X-ASize, ACenter.Y-ASize, ACenter.X+ASize+1, ACenter.Y+ASize+1);
  C.Line(ACenter.X-ASize+2, ACenter.Y, ACenter.X+ASize-1, ACenter.Y);
  if APlus then
    C.Line(ACenter.X, ACenter.Y-ASize+2, ACenter.X, ACenter.Y+ASize-1);
end;


procedure DoPartFind(const AParts: TATLineParts; APos: integer; out AIndex,
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


function DoPartsGetTotalLen(const AParts: TATLineParts): integer;
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

function DoPartsGetCount(const AParts: TATLineParts): integer;
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
  procedure AddPart(const P: TATLinePart);
  begin
    if P.Len>0 then
      if ResultPartIndex<High(ResultParts) then
      begin
        Move(P, ResultParts[ResultPartIndex], SizeOf(P));
        Inc(ResultPartIndex);
      end;
  end;
  //
  procedure FixPartLen(var P: TATLinePart; NOffsetEnd: integer);
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

  //if ColorBG=clNone, insert ColorBG of found part
  //tested on URLs in JS inside HTML
  if APart.ColorBG=clNone then
    APart.ColorBG:= AParts[nIndex1].ColorBG; //clYellow;

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
  i: integer;
begin
  for i:= Low(AParts) to High(AParts) do
  begin
    if AParts[i].Len=0 then Break;
    if AForceColor or (AParts[i].ColorBG=clNone) then
      AParts[i].ColorBG:= AColor;
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


procedure DoPartsDim(var P: TATLineParts; ADimLevel255: integer);
var
  i: integer;
begin
  for i:= Low(P) to High(P) do
  begin
    if P[i].Len=0 then break;
    with P[i] do
    begin
      ColorFont:= ColorBlend(ColorBG, ColorFont, ADimLevel255);
      if ColorBorder<>clNone then
        ColorBorder:= ColorBlend(ColorBG, ColorBorder, ADimLevel255);
    end;
  end;
end;


procedure CanvasTextOutMinimap(C: TCanvas; const AStr: atString;
  const ARect: TRect; APos: TPoint; ACharSize: TPoint; ATabSize: integer;
  AParts: PATLineParts;
  AColorBG: TColor);
// line is painted with 2px height,
// and 1px spacing between lines
var
  Part: ^TATLinePart;
  ch: Widechar;
  nPart, nPos, nCharSize: integer;
  X1, Y1, X2, Y2: integer;
  HasBG: boolean;
begin
  if AStr='' then exit;
  for nPart:= Low(TATLineParts) to High(TATLineParts) do
  begin
    Part:= @AParts^[nPart];
    if Part^.Len=0 then Break;

    HasBG:= Part^.ColorBG<>AColorBG;

    nPos:= Part^.Offset+1;
    if nPos>Length(AStr) then Continue;
    ch:= AStr[nPos];
    if IsStringSpaces(AStr, nPos, Part^.Len)
      and (not HasBG) then Continue;

    X1:= APos.X + ACharSize.X*Part^.Offset;
    X2:= X1 + ACharSize.X*Part^.Len;
    Y2:= APos.Y + ACharSize.Y;

    if HasBG then
      Y1:= Y2-2
    else
      Y1:= Y2-1;

    if (X1>=ARect.Left) and (X1<ARect.Right) then
    begin
      //must limit line on right edge
      if X2>ARect.Right then
        X2:= ARect.Right;

      if HasBG then
        C.Brush.Color:= Part^.ColorBG
      else
        C.Brush.Color:= ColorBlendHalf(Part^.ColorBG, Part^.ColorFont);
      C.FillRect(X1, Y1, X2, Y2);
    end;
  end;
end;


//------------------
initialization
  _Pen:= TPen.Create;

finalization
  if Assigned(_Pen) then
    FreeAndNil(_Pen);

end.

