{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_HtmlColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATStringProc;

//convert TColor -> HTML color string #rrggbb
function SColorToHtmlColor(Color: TColor): string;

//convert string which starts with HTML color token #rgb, #rrggbb -> TColor, get len of color-string
function SHtmlColorToColor(s: PChar; out Len: integer; Default: TColor): TColor;

type
  TATHtmlColorParser = class
  public
    class function ParseFunctionRGB(const S: UnicodeString; FromPos: integer; out LenOfColor: integer): TColor;
  end;

implementation

function SColorToHtmlColor(Color: TColor): string;
var
  N: Longint;
begin
  if Color=clNone then
    exit('');
  N:= ColorToRGB(Color);
  Result:= '#'+
    IntToHex(Red(N), 2)+
    IntToHex(Green(N), 2)+
    IntToHex(Blue(N), 2);
end;

function SHtmlColorToColor(s: PChar; out Len: integer; Default: TColor): TColor;
var
  N1, N2, N3: integer;
  ch: char;
begin
  Result:= Default;
  if s=nil then Exit;

  if s^='#' then
    Inc(s);

  //must handle string longer than needed, with additional chars
  Len:= 0;
  repeat
    ch:= s[Len];
    if ch=#0 then Break;
    if not IsCharHexDigit(ch) then
      if IsCharWordA(ch) then
        Exit
      else
        Break;
    Inc(Len);
    if Len>8 then Exit;
  until false;

  //allow #rgb, #rgba, #rrggbb, #rrggbbaa (ignore alpha value)
  case Len of
    6, 8:
      begin
        N1:= HexDigitToInt(s[0])*16 + HexDigitToInt(s[1]);
        N2:= HexDigitToInt(s[2])*16 + HexDigitToInt(s[3]);
        N3:= HexDigitToInt(s[4])*16 + HexDigitToInt(s[5]);
        Result:= RGBToColor(N1, N2, N3);
      end;
    3, 4:
      begin
        N1:= HexDigitToInt(s[0])*17;
        N2:= HexDigitToInt(s[1])*17;
        N3:= HexDigitToInt(s[2])*17;
        Result:= RGBToColor(N1, N2, N3);
      end;
  end;
end;


class function TATHtmlColorParser.ParseFunctionRGB(const S: UnicodeString; FromPos: integer; out LenOfColor: integer): TColor;
var
  NLen: integer;
  //
  procedure SkipSpaces(var N: integer); inline;
  begin
    while (N<=NLen) and IsCharSpace(S[N]) do
      Inc(N);
  end;
  //
  procedure SkipComma(var N: integer); inline;
  begin
    if S[N]=',' then
      Inc(N);
  end;
  //
  function SkipInt(var N: integer): integer;
  begin
    Result:= -1;
    SkipSpaces(N);
    while (N<=NLen) and IsCharDigit(S[N]) do
    begin
      if Result=-1 then
        Result:= 0;
      Result:= Result*10+StrToIntDef(S[N], 0);
      Inc(N);
    end;
    SkipSpaces(N);
  end;
  //
  function SkipFloat(var N: integer): integer;
  begin
    Result:= -1;
    SkipSpaces(N);
    while (N<=NLen) and (IsCharDigit(S[N]) or (S[N]='.')) do
    begin
      Inc(N);
      Result:= 1; //ignore the value
    end;
    SkipSpaces(N);
  end;
var
  Val1, Val2, Val3, ValAlpha: integer;
  bAlpha: boolean;
  N: integer;
begin
  Result:= clNone;
  LenOfColor:= 0;

  NLen:= Length(S);
  N:= FromPos;
  bAlpha:= false;

  if N+9>NLen then exit;
  if S[N]<>'r' then exit;
  Inc(N);
  if S[N]<>'g' then exit;
  Inc(N);
  if S[N]<>'b' then exit;
  Inc(N);
  if S[N]='a' then
  begin
    bAlpha:= true;
    Inc(N);
  end;
  if S[N]<>'(' then exit;
  Inc(N);

  Val1:= SkipInt(N);
  if Val1<0 then exit;
  if Val1>255 then exit;
  if N>NLen then exit;
  SkipComma(N);

  Val2:= SkipInt(N);
  if Val2<0 then exit;
  if Val2>255 then exit;
  if N>NLen then exit;
  SkipComma(N);

  Val3:= SkipInt(N);
  if Val3<0 then exit;
  if Val3>255 then exit;
  if N>NLen then exit;
  if bAlpha then
  begin
    SkipComma(N);
    ValAlpha:= SkipFloat(N);
    if ValAlpha<0 then exit;
  end;
  if S[N]<>')' then exit;

  Result:= RGBToColor(byte(Val1), byte(Val2), byte(Val3));
  LenOfColor:= N-FromPos+1;
end;
  
end.

