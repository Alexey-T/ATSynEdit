unit ATSynEdit_CanvasProc;

{$mode objfpc}{$H+}

//{$define invert_pixels} //test Mac caret blinking
{$ifdef darwin}
  {$define invert_pixels}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Types,
  ATStringProc;

var
  OptUnprintedTabCharLength: integer = 1;
  OptUnprintedTabPointerScale: integer = 22;
  OptUnprintedSpaceDotScale: integer = 15;
  OptUnprintedEndDotScale: integer = 30;
  OptUnprintedEndFontScale: integer = 80;
  OptUnprintedEndFontDx: integer = 3;
  OptUnprintedEndFontDy: integer = 2;
  OptUnprintedEndArrowOrDot: boolean = true;
  OptUnprintedEndArrowLength: integer = 70;

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

type
  TATLinePart = record
    Offset, Len: integer;
    ColorFont, ColorBG, ColorBorder: TColor;
    FontBold, FontItalic, FontStrikeOut: boolean;
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

procedure CanvasLineEx(C: TCanvas; Color: TColor; Style: TATLineStyle;
  P1, P2: TPoint; AtDown: boolean);

procedure CanvasTextOut(C: TCanvas;
  PosX, PosY: integer;
  Str: atString;
  ATabSize: integer;
  ACharSize: TPoint;
  AMainText: boolean;
  AShowUnprintable: boolean;
  AColorUnprintable: TColor;
  AColorHex: TColor;
  out AStrWidth: integer;
  ACharsSkipped: integer;
  AParts: PATLineParts;
  ADrawEvent: TATSynEditDrawLineEvent;
  ATextOffsetFromLine: integer;
  AControlWidth: integer
  );

procedure CanvasTextOutMinimap(C: TCanvas;
  const AStr: atString;
  APos: TPoint;
  ACharSize: TPoint;
  ATabSize: integer;
  AParts: PATLineParts
  );

procedure DoPaintUnprintedEol(C: TCanvas;
  const AStrEol: atString;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont, AColorBG: TColor;
  ADetails: boolean);

function CanvasTextSpaces(const S: atString; ATabSize: integer): real;
function CanvasTextWidth(const S: atString; ATabSize: integer; ACharSize: TPoint): integer;

function CanvasFontSizes(C: TCanvas): TPoint;
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
procedure CanvasDottedVertLine_Alt(C: TCanvas; Color: TColor; X1, Y1, Y2: integer);
procedure CanvasDottedHorzVertLine(C: TCanvas; Color: TColor; P1, P2: TPoint);
procedure CanvasWavyHorzLine(C: TCanvas; Color: TColor; P1, P2: TPoint; AtDown: boolean);

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
procedure CanvasPaintPlusMinus(C: TCanvas; AColorBorder, AColorBG: TColor; ACenter: TPoint; ASize: integer; APlus: boolean);

procedure DoPartFind(const AParts: TATLineParts; APos: integer; out AIndex, AOffsetLeft: integer);
procedure DoPartInsert(var AParts: TATLineParts; const APart: TATLinePart; AKeepFontStyles: boolean);
procedure DoPartSetColorBG(var AParts: TATLineParts; AColor: TColor; AForceColor: boolean);


implementation

uses
  Math,
  LCLType,
  LCLIntf;

var
  _Pen: TPen = nil;

type
  TATBorderSide = (cSideLeft, cSideRight, cSideUp, cSideDown);


procedure DoPaintUnprintedSpace(C: TCanvas; const ARect: TRect; AScale: integer; AFontColor: TColor);
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

procedure DoPaintUnprintedTabulation(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  ACharSizeX: integer);
const
  cIndent = 1; //offset left/rt
var
  XLeft, XRight, X1, X2, Y, Dx: integer;
begin
  XLeft:= ARect.Left+cIndent;
  XRight:= ARect.Right-cIndent;

  if OptUnprintedTabCharLength=0 then
  begin;
    X1:= XLeft;
    X2:= XRight;
  end
  else
  begin
    X1:= XLeft;
    X2:= Min(XRight, X1+OptUnprintedTabCharLength*ACharSizeX);
  end;

  Y:= (ARect.Top+ARect.Bottom) div 2;
  Dx:= (ARect.Bottom-ARect.Top) * OptUnprintedTabPointerScale div 100;
  C.Pen.Color:= AColorFont;

  C.MoveTo(X2, Y);
  C.LineTo(X1, Y);
  C.MoveTo(X2, Y);
  C.LineTo(X2-Dx, Y-Dx);
  C.MoveTo(X2, Y);
  C.LineTo(X2-Dx, Y+Dx);
end;


procedure DoPaintUnprintedArrowDown(C: TCanvas;
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
  AColorFont: TColor);
var
  R: TRect;
  i: integer;
begin
  if AString='' then Exit;

  for i:= 1 to Length(AString) do
    if (AString[i]=' ') or (AString[i]=#9) then
    begin
      R.Left:= APoint.X;
      R.Right:= APoint.X;
      if i>1 then
        Inc(R.Left, AOffsets[i-2]);
      Inc(R.Right, AOffsets[i-1]);

      R.Top:= APoint.Y;
      R.Bottom:= R.Top+ACharSize.Y;

      if AString[i]=' ' then
        DoPaintUnprintedSpace(C, R, OptUnprintedSpaceDotScale, AColorFont)
      else
        DoPaintUnprintedTabulation(C, R, AColorFont, ACharSize.X);
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
  cPixel: TColor;
begin
  cPixel:= Color;
  C.Pen.Color:= Color;
  if P1.Y=P2.Y then
  begin
    C.Line(P1.X+2, P1.Y, P2.X-1, P2.Y);
    if AtDown then
    begin
      C.Pixels[P1.X+1, P1.Y-1]:= cPixel;
      C.Pixels[P2.X-1, P2.Y-1]:= cPixel;
    end
    else
    begin
      C.Pixels[P1.X+1, P1.Y+1]:= cPixel;
      C.Pixels[P2.X-1, P2.Y+1]:= cPixel;
    end
  end
  else
  begin
    C.Line(P1.X, P1.Y+2, P2.X, P2.Y-1);
    //don't draw pixels, other lines did it
  end;
end;

procedure CanvasWavyHorzLine(C: TCanvas; Color: TColor; P1, P2: TPoint; AtDown: boolean);
const
  cWavePeriod = 4;
  cWaveInc: array[0..cWavePeriod-1] of integer = (0, 1, 2, 1);
var
  i, y, sign: integer;
begin
  if AtDown then sign:= -1 else sign:= 1;
  for i:= P1.X to P2.X do
  begin
    y:= P2.Y + sign * cWaveInc[(i-P1.X) mod cWavePeriod];
    C.Pixels[i, y]:= Color;
  end;
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
  i, j: integer;
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

      Buf:= '<'+IntToHex(Ord(AString[i]), 4)+'>';
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
      DoPaintUnprintedArrowDown(C,
        Rect(APoint.X, APoint.Y, APoint.X+ACharSize.X, APoint.Y+ACharSize.Y),
        AColorFont)
    else
      DoPaintUnprintedSpace(C,
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

function CanvasTextSpaces(const S: atString; ATabSize: integer): real;
var
  List: TATRealArray;
begin
  Result:= 0;
  if S='' then Exit;
  SetLength(List, Length(S));
  SCalcCharOffsets(S, List, ATabSize);
  Result:= List[High(List)];
end;

function CanvasTextWidth(const S: atString; ATabSize: integer; ACharSize: TPoint): integer;
begin
  Result:= Trunc(CanvasTextSpaces(S, ATabSize)*ACharSize.X);
end;

procedure CanvasTextOut(C: TCanvas; PosX, PosY: integer; Str: atString;
  ATabSize: integer; ACharSize: TPoint; AMainText: boolean;
  AShowUnprintable: boolean; AColorUnprintable: TColor; AColorHex: TColor; out
  AStrWidth: integer; ACharsSkipped: integer; AParts: PATLineParts;
  ADrawEvent: TATSynEditDrawLineEvent; ATextOffsetFromLine: integer;
  AControlWidth: integer);
var
  ListReal: TATRealArray;
  ListInt: TATIntArray;
  Dx: TATIntArray;
  i, j: integer;
  PartStr: atString;
  PartOffset, PartLen,
  PixOffset1, PixOffset2: integer;
  PartPtr: ^TATLinePart;
  PartFontStyle: TFontStyles;
  PartRect: TRect;
  Buf: AnsiString;
begin
  if Str='' then Exit;

  SetLength(ListReal, Length(Str));
  SetLength(ListInt, Length(Str));
  SetLength(Dx, Length(Str));

  SCalcCharOffsets(Str, ListReal, ATabSize, ACharsSkipped);

  for i:= 0 to High(ListReal) do
    ListInt[i]:= Trunc(ListReal[i]*ACharSize.X);

  //truncate str, to not paint over screen
  for i:= 1 to High(ListInt) do
    if ListInt[i]>AControlWidth+ACharSize.X then
    begin
      SetLength(Str, i);
      break;
    end;

  for i:= 0 to High(ListReal) do
    if i=0 then
      Dx[i]:= ListInt[i]
    else
      Dx[i]:= ListInt[i]-ListInt[i-1];

  if AParts=nil then
  begin
    Buf:= UTF8Encode(SRemoveHexChars(Str));
    ExtTextOut(C.Handle, PosX, PosY, 0, nil, PChar(Buf), Length(Buf), @Dx[0]);

    DoPaintHexChars(C,
      Str,
      @Dx[0],
      Point(PosX, PosY),
      ACharSize,
      AColorHex,
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
      PartStr:= Copy(Str, PartOffset+1, PartLen);
      if PartStr='' then Break;

      PartFontStyle:= [];
      if PartPtr^.FontBold then Include(PartFontStyle, fsBold);
      if PartPtr^.FontItalic then Include(PartFontStyle, fsItalic);
      if PartPtr^.FontStrikeOut then Include(PartFontStyle, fsStrikeOut);

      if PartOffset>0 then
        PixOffset1:= ListInt[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, Length(Str));
      if i>0 then
        PixOffset2:= ListInt[i-1]
      else
        PixOffset2:= 0;

      C.Font.Color:= PartPtr^.ColorFont;
      C.Brush.Color:= PartPtr^.ColorBG;
      C.Font.Style:= PartFontStyle;

      PartRect:= Rect(
        PosX+PixOffset1,
        PosY,
        PosX+PixOffset2,
        PosY+ACharSize.Y);

      Buf:= UTF8Encode(SRemoveHexChars(PartStr));
      ExtTextOut(C.Handle,
        PosX+PixOffset1,
        PosY+ATextOffsetFromLine,
        ETO_CLIPPED+ETO_OPAQUE,
        @PartRect,
        PChar(Buf),
        Length(Buf),
        @Dx[PartOffset]);

      DoPaintHexChars(C,
        PartStr,
        @Dx[PartOffset],
        Point(PosX+PixOffset1, PosY),
        ACharSize,
        AColorHex,
        PartPtr^.ColorBG
        );

      if AMainText then
      begin
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideDown, PartPtr^.BorderDown);
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideUp, PartPtr^.BorderUp);
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideLeft, PartPtr^.BorderLeft);
        DoPaintBorder(C, PartPtr^.ColorBorder, PartRect, cSideRight, PartPtr^.BorderRight);
      end;
    end;

  if AShowUnprintable then
    DoPaintUnprintedChars(C, Str, ListInt, Point(PosX, PosY), ACharSize, AColorUnprintable);

  AStrWidth:= ListInt[High(ListInt)];

  if Str<>'' then
    if Assigned(ADrawEvent) then
      ADrawEvent(nil, C, PosX, PosY, Str, ACharSize, ListInt);
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
  C.Pen.Mode:= pmNotXor;
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
    Point(ACoord.X, ACoord.Y),
    Point(ACoord.X+ASize*2, ACoord.Y),
    Point(ACoord.X+ASize, ACoord.Y+ASize)
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


procedure DoPartFind(const AParts: TATLineParts; APos: integer; out
  AIndex, AOffsetLeft: integer);
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
    if APos=iStart then
      begin AIndex:= i; Break end;

    //pos at part middle?
    if (APos>=iStart) and (APos<iEnd) then
      begin AIndex:= i; AOffsetLeft:= APos-iStart; Break end;
  end;
end;


procedure DoPartInsert(var AParts: TATLineParts; const APart: TATLinePart;
  AKeepFontStyles: boolean);
var
  ResultParts: TATLineParts;
  ResultPartIndex: integer;
  //
  procedure AddPart(const P: TATLinePart);
  begin
    if P.Len>0 then
    begin
      Move(P, ResultParts[ResultPartIndex], SizeOf(P));
      Inc(ResultPartIndex);
    end;
  end;
  //
var
  PartSelBegin, PartSelEnd: TATLinePart;
  nIndex1, nIndex2,
  nOffset1, nOffset2,
  newLen1, newLen2, newOffset2: integer;
  i: integer;
begin
  DoPartFind(AParts, APart.Offset, nIndex1, nOffset1);
  DoPartFind(AParts, APart.Offset+APart.Len, nIndex2, nOffset2);
  if nIndex1<0 then Exit;
  if nIndex2<0 then Exit;

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

  PartSelEnd.ColorFont:= APart.ColorFont;
  PartSelEnd.ColorBG:= APart.ColorBG;
  PartSelEnd.Offset:= AParts[nIndex2].Offset;
  PartSelEnd.Len:= nOffset2;
  PartSelEnd.FontBold:= AParts[nIndex2].FontBold;
  PartSelEnd.FontItalic:= AParts[nIndex2].FontItalic;
  PartSelEnd.FontStrikeOut:= AParts[nIndex2].FontStrikeOut;

  with AParts[nIndex1] do
  begin
    newLen1:= nOffset1;
  end;
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
    AParts[nIndex1].Len:= newLen1;
    AddPart(AParts[nIndex1]);
  end;

  //add middle (one APart of many parts)
  if not AKeepFontStyles then
    AddPart(APart)
  else
  begin
    AddPart(PartSelBegin);

    for i:= nIndex1+1 to nIndex2-1 do
    begin
      AParts[i].ColorFont:= APart.ColorFont;
      AParts[i].ColorBG:= APart.ColorBG;
      AddPart(AParts[i]);
    end;

    if nIndex1<nIndex2 then
      AddPart(PartSelEnd);
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

  //application.mainform.caption:= format('n1 %d, n2 %d, of len %d %d',
  //  [nindex1, nindex2, aparts[nindex2].offset, aparts[nindex2].len]);

  //copy result
  Move(ResultParts, AParts, SizeOf(AParts));
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


procedure CanvasTextOutMinimap(C: TCanvas; const AStr: atString; APos: TPoint;
  ACharSize: TPoint; ATabSize: integer; AParts: PATLineParts);
const
  cLowChars = '.,:;_''-+`~=^*';
var
  Offsets: TATIntArray;
  Part: ^TATLinePart;
  ch: Widechar;
  nPos, nCharSize: integer;
  i, j: integer;
  X1, Y1, Y2: integer;
begin
  if AStr='' then exit;
  SetLength(Offsets, Length(AStr)+1);
  Offsets[0]:= 0;
  for i:= 2 to Length(AStr) do
    Offsets[i-1]:= Offsets[i-2]+IfThen(AStr[i-1]=#9, ATabSize, 1);

  for i:= Low(TATLineParts) to High(TATLineParts) do
  begin
    Part:= @AParts^[i];
    if Part^.Len=0 then Break;
    for j:= 1 to Part^.Len do
    begin
      nPos:= Part^.Offset+j;
      if nPos>Length(AStr) then Continue;
      ch:= AStr[nPos];
      if IsCharSpace(ch) then Continue;

      nCharSize:= ACharSize.Y;
      if Pos(ch, cLowChars)>0 then
        nCharSize:= nCharSize div 2;

      X1:= APos.X+ACharSize.X*Offsets[nPos-1];
      Y2:= APos.Y+ACharSize.Y;
      Y1:= Y2-nCharSize;

      C.Pen.Color:= Part^.ColorFont;
      C.Line(X1, Y1, X1, Y2);
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

