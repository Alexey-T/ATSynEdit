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

  TATLineParts = array[0..400] of TATLinePart;
  PATLineParts = ^TATLineParts;


procedure CanvasTextOut(C: TCanvas; PosX, PosY: integer; S: UnicodeString;
  ATabSize: integer;
  ACharSize: TPoint;
  AReplaceSpecs: boolean;
  ACharsSkipped: integer = 0;
  AParts: PATLineParts = nil);

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
  LCLIntf;

const
  ETO_OPAQUE = 2;
  ETO_CLIPPED = 4;


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

procedure CanvasTextOut(C: TCanvas; PosX, PosY: integer; S: UnicodeString;
  ATabSize: integer; ACharSize: TPoint; AReplaceSpecs: boolean;
  ACharsSkipped: integer; AParts: PATLineParts);
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
  if S='' then Exit;
  if AReplaceSpecs then
    SReplaceSpecChars(S);

  SetLength(ListReal, Length(S));
  SetLength(ListInt, Length(S));
  SetLength(Dx, Length(S));

  SCalcCharOffsets(S, ListReal, ATabSize, ACharsSkipped);

  for i:= 0 to High(ListReal) do
    ListInt[i]:= Trunc(ListReal[i]*ACharSize.X);

  for i:= 0 to High(ListReal) do
    if i=0 then
      Dx[i]:= ListInt[i]
    else
      Dx[i]:= ListInt[i]-ListInt[i-1];

  if AParts=nil then
  begin
    Buf:= UTF8Encode(S);
    ExtTextOut(C.Handle, PosX, PosY, 0, nil, PChar(Buf), Length(Buf), @Dx[0]);
  end
  else
  for j:= 0 to High(TATLineParts) do
    begin
      PartLen:= AParts^[j].Len;
      if PartLen=0 then Break;
      PartOffset:= AParts^[j].Offset;
      PartStr:= Copy(S, PartOffset+1, PartLen);
      if PartStr='' then Break;
      PartColor:= AParts^[j].Color;
      PartColorBG:= AParts^[j].ColorBG;
      PartStyles:= AParts^[j].Styles;

      if PartOffset>0 then
        PixOffset1:= ListInt[PartOffset-1]
      else
        PixOffset1:= 0;

      i:= Min(PartOffset+PartLen, Length(S));
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

