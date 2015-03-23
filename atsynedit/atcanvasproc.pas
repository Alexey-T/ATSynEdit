unit ATCanvasProc;

{$mode delphi}
//{$define win_fast} //use Windows api

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

procedure CanvasTextOut(C: TCanvas;
  PosX, PosY: integer;
  Str: atString;
  ATabSize: integer;
  ACharSize: TPoint;
  AReplaceSpecs: boolean;
  AShowUnprintable: boolean;
  AColorUnprintable: TColor;
  out AStrWidth: integer;
  ACharsSkipped: integer = 0;
  AParts: PATLineParts = nil);

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

procedure DoPaintUnprintedTabulation(C: TCanvas; const ARect: TRect; AColorFont: TColor);
const
  cIndent = 1; //offset left/rt
  cScale = 4; //part 1/N of height
var
  X, X2, Y, Dx: integer;
begin
  X:= ARect.Right-cIndent;
  X2:= ARect.Left+cIndent;
  Y:= (ARect.Top+ARect.Bottom) div 2;
  Dx:= (ARect.Bottom-ARect.Top) div cScale;
  C.Pen.Color:= AColorFont;
  C.MoveTo(X, Y);
  C.LineTo(X2, Y);
  C.MoveTo(X, Y);
  C.LineTo(X-Dx, Y-Dx);
  C.MoveTo(X, Y);
  C.LineTo(X-Dx, Y+Dx);
end;

procedure DoPaintUnprintedChars(C: TCanvas;
  const AString: atString;
  const AOffsets: array of integer;
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
        DoPaintUnprintedSpace(C, R, cUnprintedDotScale, AColorFont)
      else
        DoPaintUnprintedTabulation(C, R, AColorFont);
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

procedure CanvasTextOut(C: TCanvas;
  PosX, PosY: integer;
  Str: atString;
  ATabSize: integer;
  ACharSize: TPoint;
  AReplaceSpecs: boolean;
  AShowUnprintable: boolean;
  AColorUnprintable: TColor;
  out AStrWidth: integer;
  ACharsSkipped: integer;
  AParts: PATLineParts);
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
    SReplaceSpecChars(Str);

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
    Buf:= UTF8Encode(Str);
    ExtTextOut(C.Handle, PosX, PosY, 0, nil, PChar(Buf), Length(Buf), @Dx[0]);
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

      Buf:= UTF8Encode(PartStr);
      ExtTextOut(C.Handle,
        PosX+PixOffset1,
        PosY,
        ETO_CLIPPED+ETO_OPAQUE,
        @PartRect,
        PChar(Buf),
        Length(Buf),
        @Dx[PartOffset]);
    end;

  if AShowUnprintable then
    DoPaintUnprintedChars(C, Str, ListInt, Point(PosX, PosY), ACharSize, AColorUnprintable);

  AStrWidth:= ListInt[High(ListInt)];
end;

var
  _Pen: TPen = nil;

procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  X: integer;
  AM: TAntialiasingMode;
begin
  if not Assigned(_Pen) then
    _Pen:= TPen.Create;

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



end.

