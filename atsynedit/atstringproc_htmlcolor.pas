{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_HtmlColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { TATHtmlColorParser }

  TATHtmlColorParser = class
  private
    class procedure SkipSpaces(const S: UnicodeString; var N: integer);
    class procedure SkipComma(const S: UnicodeString; var N: integer);
    class function SkipInt(const S: UnicodeString; var N: integer): integer;
    class function SkipIntWithPercent(const S: UnicodeString; var N: integer): integer;
    class function SkipFloat(const S: UnicodeString; var N: integer): integer;
  public
    //convert TColor -> HTML color string #rrggbb
    class function ColorToHtmlString(Color: TColor): string;
    //convert string which starts with HTML color token #rgb, #rrggbb -> TColor, get len of color-string
    class function ParseTokenRGB(s: PChar; out Len: integer; Default: TColor): TColor;
    //parses 'rgb(10,20,30)' and rgba(10,20,30,0.5)
    class function ParseFunctionRGB(const S: UnicodeString; FromPos: integer; out LenOfColor: integer): TColor;
    //parses 'hsl(0,50%,100%)' and 'hsla(0,50%,100%,0.5)
    class function ParseFunctionHSL(const S: UnicodeString; FromPos: integer; out LenOfColor: integer): TColor;
  end;

implementation

uses
  GraphUtil, //for HLS color conversion
  ATStringProc;

class function TATHtmlColorParser.ColorToHtmlString(Color: TColor): string;
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

class function TATHtmlColorParser.ParseTokenRGB(s: PChar; out Len: integer; Default: TColor): TColor;
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


class procedure TATHtmlColorParser.SkipSpaces(const S: UnicodeString; var N: integer); inline;
begin
  while (N<=Length(S)) and IsCharSpace(S[N]) do
    Inc(N);
end;

class procedure TATHtmlColorParser.SkipComma(const S: UnicodeString; var N: integer); inline;
begin
  if S[N]=',' then
    Inc(N);
end;

class function TATHtmlColorParser.SkipInt(const S: UnicodeString; var N: integer): integer;
begin
  Result:= -1;
  SkipSpaces(S, N);
  while (N<=Length(S)) and IsCharDigit(S[N]) do
  begin
    if Result=-1 then
      Result:= 0;
    Result:= Result*10+StrToIntDef(S[N], 0);
    Inc(N);
  end;
  SkipSpaces(S, N);
end;

class function TATHtmlColorParser.SkipIntWithPercent(const S: UnicodeString; var N: integer): integer;
begin
  Result:= -1;
  SkipSpaces(S, N);
  while (N<=Length(S)) and IsCharDigit(S[N]) do
  begin
    if Result=-1 then
      Result:= 0;
    Result:= Result*10+StrToIntDef(S[N], 0);
    Inc(N);
  end;
  if N>Length(S) then exit(-1);
  if S[N]='%' then
    Inc(N)
  else
    exit(-1);
  SkipSpaces(S, N);
end;

class function TATHtmlColorParser.SkipFloat(const S: UnicodeString; var N: integer): integer;
begin
  Result:= -1;
  SkipSpaces(S, N);
  while (N<=Length(S)) and (IsCharDigit(S[N]) or (S[N]='.')) do
  begin
    Inc(N);
    Result:= 1; //ignore the value
  end;
  SkipSpaces(S, N);
end;


class function TATHtmlColorParser.ParseFunctionRGB(const S: UnicodeString; FromPos: integer; out LenOfColor: integer): TColor;
var
  NLen: integer;
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

  Val1:= SkipInt(S, N);
  if Val1<0 then exit;
  if Val1>255 then exit;
  if N>NLen then exit;
  SkipComma(S, N);

  Val2:= SkipInt(S, N);
  if Val2<0 then exit;
  if Val2>255 then exit;
  if N>NLen then exit;
  SkipComma(S, N);

  Val3:= SkipInt(S, N);
  if Val3<0 then exit;
  if Val3>255 then exit;
  if N>NLen then exit;
  if bAlpha then
  begin
    SkipComma(S, N);
    ValAlpha:= SkipFloat(S, N);
    if ValAlpha<0 then exit;
  end;
  if S[N]<>')' then exit;

  Result:= RGBToColor(byte(Val1), byte(Val2), byte(Val3));
  LenOfColor:= N-FromPos+1;
end;


class function TATHtmlColorParser.ParseFunctionHSL(const S: UnicodeString; FromPos: integer; out LenOfColor: integer): TColor;
var
  NLen: integer;
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
  if S[N]<>'h' then exit;
  Inc(N);
  if S[N]<>'s' then exit;
  Inc(N);
  if S[N]<>'l' then exit;
  Inc(N);
  if S[N]='a' then
  begin
    bAlpha:= true;
    Inc(N);
  end;
  if S[N]<>'(' then exit;
  Inc(N);

  Val1:= SkipInt(S, N);
  if Val1<0 then exit;
  if Val1>360 then exit;
  if N>NLen then exit;
  SkipComma(S, N);

  Val2:= SkipIntWithPercent(S, N);
  if Val2<0 then exit;
  if Val2>100 then exit;
  if N>NLen then exit;
  SkipComma(S, N);

  Val3:= SkipIntWithPercent(S, N);
  if Val3<0 then exit;
  if Val3>100 then exit;
  if N>NLen then exit;
  if bAlpha then
  begin
    SkipComma(S, N);
    ValAlpha:= SkipFloat(S, N);
    if ValAlpha<0 then exit;
  end;
  if S[N]<>')' then exit;

  Result:= HLStoColor(
    byte(Val1 * 255 div 360),
    byte(Val3 * 255 div 100),
    byte(Val2 * 255 div 100)
    );
  LenOfColor:= N-FromPos+1;
end;

end.

