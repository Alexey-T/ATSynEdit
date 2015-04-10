unit ATCanvasProc;

{$mode delphi}
//{$define win_fast} //use Windows api
//{$define invert_pixels} //slow invert-rect

{$ifdef darwin}
  {$define invert_pixels}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Types,
  ATStringProc;

type
  TATLinePart = record
    Offset, Len: integer;
    Color, ColorBG: TColor;
    Styles: TFontStyles;
  end;

type
  TATLineParts = array[0..400] of TATLinePart;
  PATLineParts = ^TATLineParts;

  TATSynEditDrawLineEvent = procedure(Sender: TObject; C: TCanvas;
    AX, AY: integer; const AStr: atString; ACharSize: TPoint;
    const AExtent: array of integer) of object;


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
  AArrowSize: integer;
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

function CanvasFontSizes(C: TCanvas): TSize;
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
procedure CanvasDottedVertLine(C: TCanvas; X, Y1, Y2: integer; AColor: TColor);


implementation

uses
  Math,
  {$ifdef win_fast}
  Windows,
  {$endif}
  LCLType,
  LCLIntf;

const
  cUnprintedDotScale = 0.15;
  cUnprintedEndScale = 0.30;
  cUnprintedFontScale = 0.80;
  cUnprintedFontOffsetX = 3;
  cUnprintedFontOffsetY = 2;

var
  _Pen: TPen = nil;


procedure DoPaintUnprintedSpace(C: TCanvas; const ARect: TRect; AScale: real; AFontColor: TColor);
const
  cMinDotSize = 2;
var
  R: TRect;
  NSize: integer;
begin
  NSize:= Max(cMinDotSize, Trunc((ARect.Bottom-ARect.Top)*AScale));
  R.Left:= (ARect.Left+ARect.Right) div 2 - NSize div 2;
  R.Top:= (ARect.Top+ARect.Bottom) div 2 - NSize div 2;
  R.Right:= R.Left + NSize;
  R.Bottom:= R.Top + NSize;
  C.Pen.Color:= AFontColor;
  C.Brush.Color:= AFontColor;
  C.FillRect(R);
end;

procedure DoPaintUnprintedTabulation(C: TCanvas; const ARect: TRect; AColorFont: TColor; ACharSizeX, AArrowSize: integer);
const
  cIndent = 1; //offset left/rt
  cScale = 4; //part 1/N of height
var
  XLeft, XRight, X1, X2, Y, Dx: integer;
begin
  XLeft:= ARect.Left+cIndent;
  XRight:= ARect.Right-cIndent;

  if AArrowSize=0 then
  begin;
    X1:= XLeft;
    X2:= XRight;
  end
  else
  begin
    X1:= XLeft;
    X2:= Min(XRight, X1+AArrowSize*ACharSizeX);
  end;

  Y:= (ARect.Top+ARect.Bottom) div 2;
  Dx:= (ARect.Bottom-ARect.Top) div cScale;
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
  const AOffsets: array of integer;
  APoint: TPoint;
  ACharSize: TPoint;
  AColorFont: TColor;
  AArrowSize: integer);
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
        DoPaintUnprintedSpace(C, R, cUnprintedDotScale, AColorFont)
      else
        DoPaintUnprintedTabulation(C, R, AColorFont, ACharSize.X, AArrowSize);
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
        Inc(R.Left, ADx[j]);
      R.Right:= R.Left+ADx[i-1];

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
    C.Font.Size:= Trunc(C.Font.Size*cUnprintedFontScale);
    C.Font.Color:= AColorFont;
    C.Brush.Color:= AColorBG;
    C.TextOut(
      APoint.X+cUnprintedFontOffsetX,
      APoint.Y+cUnprintedFontOffsetY,
      AStrEol);
    C.Font.Size:= NPrevSize;
  end
  else
  begin
    DoPaintUnprintedSpace(C,
      Rect(APoint.X, APoint.Y, APoint.X+ACharSize.X, APoint.Y+ACharSize.Y),
      cUnprintedEndScale,
      AColorFont);
  end;
end;


function CanvasFontSizes(C: TCanvas): TSize;
begin
  Result:= C.TextExtent('M');
end;

function CanvasTextSpaces(const S: atString; ATabSize: integer): real;
var
  List: array of real;
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
  AStrWidth: integer; ACharsSkipped: integer; AArrowSize: integer;
  AParts: PATLineParts; AEvent: TATSynEditDrawLineEvent);
var
  ListReal: array of real;
  ListInt: array of Longint;
  Dx: array of Longint;
  i, j: integer;
  PartStr: atString;
  PartOffset, PartLen,
  PixOffset1, PixOffset2: integer;
  PartColor, PartColorBG: TColor;
  PartStyles: TFontStyles;
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
      PartColor:= AParts^[j].Color;
      PartColorBG:= AParts^[j].ColorBG;
      PartStyles:= AParts^[j].Styles;

      if PartOffset>0 then
        PixOffset1:= ListInt[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, Length(Str));
      if i>0 then
        PixOffset2:= ListInt[i-1]
      else
        PixOffset2:= 0;

      C.Font.Color:= PartColor;
      C.Font.Style:= PartStyles;
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
    end;

  if AShowUnprintable then
    DoPaintUnprintedChars(C, Str, ListInt, Point(PosX, PosY), ACharSize, AColorUnprintable, AArrowSize);

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


initialization
  _Pen:= TPen.Create;

finalization
  if Assigned(_Pen) then
    FreeAndNil(_Pen);

end.

