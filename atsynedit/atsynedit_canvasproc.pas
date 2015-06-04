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
  OptUnprintedTabCharLength: integer = 2;
  OptUnprintedTabPointerScale: integer = 22;
  OptUnprintedSpaceDotScale: integer = 15;
  OptUnprintedEndDotScale: integer = 30;
  OptUnprintedEndFontScale: integer = 80;
  OptUnprintedEndFontDx: integer = 3;
  OptUnprintedEndFontDy: integer = 2;


type
  TATLineBorderStyle = (cBorderNone, cBorderSingle, cBorderDot);

type
  TATLinePart = record
    Offset, Len: integer;
    ColorFont, ColorBG, ColorBorder: TColor;
    FontStyle: TFontStyles;
    BorderUp, BorderDown, BorderLeft, BorderRight: TATLineBorderStyle;
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

procedure CanvasTextOut(C: TCanvas;
  PosX, PosY: integer;
  Str: atString;
  ATabSize: integer;
  ACharSize: TPoint;
  AReplaceSpecs: boolean;
  AShowUnprintable: boolean;
  AColorUnprintable: TColor;
  AColorHex: TColor;
  out AStrWidth: integer;
  ACharsSkipped: integer;
  AParts: PATLineParts;
  AEvent: TATSynEditDrawLineEvent);

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
procedure CanvasDottedVertLine(C: TCanvas; X, Y1, Y2: integer; AColor: TColor);
procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
procedure CanvasPaintPlusMinus(C: TCanvas; AColorBorder, AColorBG: TColor; ACenter: TPoint; ASize: integer; APlus: boolean);

implementation

uses
  Math,
  LCLType,
  LCLIntf;

var
  _Pen: TPen = nil;


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

type
  TATBorderSide = (cSideLeft, cSideRight, cSideUp, cSideDown);

procedure DoPaintLine(C: TCanvas; Color: TColor; Style: TATLineBorderStyle; P1, P2: TPoint);
var
  i: integer;
begin
  case Style of
    cBorderSingle:
      begin
        C.Pen.Color:= Color;
        C.Line(P1, P2);
      end;
    cBorderDot:
      begin
        if P1.Y=P2.Y then
        begin
          for i:= P1.X to P2.X do
            if Odd(i) then
              C.Pixels[i, P2.Y]:= Color;
        end
        else
        begin
          for i:= P1.Y to P2.Y do
            if Odd(i) then
              C.Pixels[P1.X, i]:= Color;
        end;
      end;
  end;
end;

procedure DoPaintBorder(C: TCanvas; Color: TColor; const R: TRect; Side: TATBorderSide; Style: TATLineBorderStyle);
begin
  if Style=cBorderNone then Exit;
  case Side of
    cSideDown:
      DoPaintLine(C, Color, Style, Point(R.Left, R.Bottom-1), Point(R.Right-1, R.Bottom-1));
    cSideLeft:
      DoPaintLine(C, Color, Style, Point(R.Left, R.Top), Point(R.Left, R.Bottom));
    cSideRight:
      DoPaintLine(C, Color, Style, Point(R.Right-1, R.Top), Point(R.Right-1, R.Bottom));
    cSideUp:
      DoPaintLine(C, Color, Style, Point(R.Left, R.Top), Point(R.Right-1, R.Top));
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
  ATabSize: integer; ACharSize: TPoint; AReplaceSpecs: boolean;
  AShowUnprintable: boolean; AColorUnprintable: TColor; AColorHex: TColor; out
  AStrWidth: integer; ACharsSkipped: integer; AParts: PATLineParts;
  AEvent: TATSynEditDrawLineEvent);
var
  ListReal: TATRealArray;
  ListInt: TATIntArray;
  Dx: TATIntArray;
  i, j: integer;
  PartStr: atString;
  PartOffset, PartLen,
  PixOffset1, PixOffset2: integer;
  PartColorFont, PartColorBG, PartColorBorder: TColor;
  PartFontStyle: TFontStyles;
  PartBorderL, PartBorderR, PartBorderU, PartBorderD: TATLineBorderStyle;
  PartRect: TRect;
  Buf: AnsiString;
begin
  if Str='' then Exit;

  if AReplaceSpecs then
    Str:= SRemoveAsciiControlChars(Str);

  SetLength(ListReal, Length(Str));
  SetLength(ListInt, Length(Str));
  SetLength(Dx, Length(Str));

  SCalcCharOffsets(Str, ListReal, ATabSize, ACharsSkipped);

  for i:= 0 to High(ListReal) do
    ListInt[i]:= Trunc(ListReal[i]*ACharSize.X);

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
      PartLen:= AParts^[j].Len;
      if PartLen=0 then Break;
      PartOffset:= AParts^[j].Offset;
      PartStr:= Copy(Str, PartOffset+1, PartLen);
      if PartStr='' then Break;
      PartColorFont:= AParts^[j].ColorFont;
      PartColorBG:= AParts^[j].ColorBG;
      PartColorBorder:= AParts^[j].ColorBorder;
      PartFontStyle:= AParts^[j].FontStyle;
      PartBorderL:= AParts^[j].BorderLeft;
      PartBorderR:= AParts^[j].BorderRight;
      PartBorderU:= AParts^[j].BorderUp;
      PartBorderD:= AParts^[j].BorderDown;

      if PartOffset>0 then
        PixOffset1:= ListInt[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, Length(Str));
      if i>0 then
        PixOffset2:= ListInt[i-1]
      else
        PixOffset2:= 0;

      C.Font.Color:= PartColorFont;
      C.Font.Style:= PartFontStyle;
      C.Brush.Color:= PartColorBG;

      PartRect:= Rect(
        PosX+PixOffset1,
        PosY,
        PosX+PixOffset2,
        PosY+ACharSize.Y);

      Buf:= UTF8Encode(SRemoveHexChars(PartStr));
      ExtTextOut(C.Handle,
        PosX+PixOffset1,
        PosY,
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
        PartColorBG
        );

      DoPaintBorder(C, PartColorBorder, PartRect, cSideDown, PartBorderD);
      DoPaintBorder(C, PartColorBorder, PartRect, cSideUp, PartBorderU);
      DoPaintBorder(C, PartColorBorder, PartRect, cSideLeft, PartBorderL);
      DoPaintBorder(C, PartColorBorder, PartRect, cSideRight, PartBorderR);
    end;

  if AShowUnprintable then
    DoPaintUnprintedChars(C, Str, ListInt, Point(PosX, PosY), ACharSize, AColorUnprintable);

  AStrWidth:= ListInt[High(ListInt)];

  if Str<>'' then
    if Assigned(AEvent) then
      AEvent(nil, C, PosX, PosY, Str, ACharSize, ListInt);
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

procedure CanvasDottedVertLine(C: TCanvas; X, Y1, Y2: integer; AColor: TColor);
var
  j: integer;
begin
  for j:= Y1 to Y2 do
    if Odd(j) then
      C.Pixels[X, j]:= AColor;
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



initialization
  _Pen:= TPen.Create;

finalization
  if Assigned(_Pen) then
    FreeAndNil(_Pen);

end.

