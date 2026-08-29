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

  generic TATHtmlColorParser<TStr, TPChar> = class
  private
    class function IsCodeDigit(ch: word): boolean; inline;
    class function IsCodeHexDigit(ch: word): boolean;
    class function IsCodeWord(ch: word): boolean;
    class function IsCodeSpace(ch: word): boolean; inline;
    class function HexCodeToInt(ch: word): integer;
    class function ParseAngleUnit(const S: TStr; var N: SizeInt; var ValAngle: double): boolean;
    class procedure SkipSpaces(const S: TStr; var N: SizeInt); inline;
    class procedure SkipComma(const S: TStr; var N: SizeInt); inline;
    class procedure SkipCommaOrSlash(const S: TStr; var N: SizeInt); inline;
    class function SkipInt(const S: TStr; var N: SizeInt): integer;
    class function SkipIntMaybeInPercents(const S: TStr; var N: SizeInt): integer;
    class function SkipIntWithPercent(const S: TStr; var N: SizeInt): integer;
    class function SkipFloat(const S: TStr; var N: SizeInt;
      CalcValue, SkipPercent: boolean; out Ok: boolean): double;
  public
    //convert TColor -> HTML color string #rrggbb
    class function ColorToHtmlString(Color: TColor): string;
    //convert string which starts with HTML color token #rgb, #rrggbb -> TColor, get len of color-string
    class function ParseTokenRGB(S: TPChar; out LenOfColor: integer; DefaultColor: TColor): TColor;
    //parses 'rgb(10,20,30)' and 'rgba(10,20,30,0.5)'
    class function ParseFunctionRGB(const S: TStr; FromPos: SizeInt; out LenOfColor: integer): TColor;
    //parses 'hsl(0,50%,100%)' and 'hsla(0,50%,100%,0.5)
    class function ParseFunctionHSL(const S: TStr; FromPos: SizeInt; out LenOfColor: integer): TColor;
  end;

type
  TATHtmlColorParserA = specialize TATHtmlColorParser<string, PChar>;
  TATHtmlColorParserW = specialize TATHtmlColorParser<UnicodeString, PWideChar>;

implementation

uses
  GraphUtil; //for HSL color conversion

class function TATHtmlColorParser.IsCodeDigit(ch: word): boolean;
begin
  Result:= (ch>=ord('0')) and (ch<=ord('9'));
end;

class function TATHtmlColorParser.IsCodeHexDigit(ch: word): boolean;
begin
  case ch of
    ord('0')..ord('9'),
    ord('a')..ord('f'),
    ord('A')..ord('F'):
      Result:= true
    else
      Result:= false;
  end;
end;

class function TATHtmlColorParser.IsCodeWord(ch: word): boolean;
begin
  case ch of
    ord('a')..ord('z'),
    ord('A')..ord('Z'),
    ord('0')..ord('9'),
    ord('_'):
      Result:= true;
    else
      Result:= false;
  end;
end;

class function TATHtmlColorParser.IsCodeSpace(ch: word): boolean;
begin
  Result:= (ch=ord(' ')) or (ch=9);
end;

class function TATHtmlColorParser.HexCodeToInt(ch: word): integer;
begin
  case ch of
    ord('0')..ord('9'):
      Result:= Ord(ch)-Ord('0');
    ord('a')..ord('f'):
      Result:= Ord(ch)-Ord('a')+10;
    ord('A')..ord('F'):
      Result:= Ord(ch)-Ord('A')+10;
    else
      Result:= 0;
  end;
end;

class function TATHtmlColorParser.ColorToHtmlString(Color: TColor): string;
const
  SHexDigits: array[0..15] of Char = '0123456789ABCDEF';
var
  N: Longint;
begin
  if Color=clNone then Exit('');
  N:= ColorToRGB(Color);
  SetLength(Result, 7);
  Result[1]:= '#';
  Result[2]:= SHexDigits[(N shr 4)  and $F];  // Red hi
  Result[3]:= SHexDigits[ N         and $F];  // Red lo
  Result[4]:= SHexDigits[(N shr 12) and $F];  // Green hi
  Result[5]:= SHexDigits[(N shr 8)  and $F];  // Green lo
  Result[6]:= SHexDigits[(N shr 20) and $F];  // Blue hi
  Result[7]:= SHexDigits[(N shr 16) and $F];  // Blue lo
end;


class function TATHtmlColorParser.ParseTokenRGB(S: TPChar; out LenOfColor: integer;
  DefaultColor: TColor): TColor;
var
  P: TPChar;
  N1, N2, N3: integer;
  ch: word;
begin
  Result:= DefaultColor;
  LenOfColor:= 0;
  if S=nil then Exit;

  if S^='#' then
    Inc(S);

  //must handle string longer than needed, with additional chars
  P:= S;
  while True do
  begin
    ch:= Word(P^);
    if ch=0 then Break;
    if not IsCodeHexDigit(ch) then
      if IsCodeWord(ch) then
        Exit
      else
        Break;
    if LenOfColor=8 then Exit;
    Inc(P);
    Inc(LenOfColor);
  end;

  //allow #rgb, #rgba, #rrggbb, #rrggbbaa (ignore alpha value)
  case LenOfColor of
    6, 8:
      begin
        N1:= HexCodeToInt(ord(S[0]))*16 + HexCodeToInt(ord(S[1]));
        N2:= HexCodeToInt(ord(S[2]))*16 + HexCodeToInt(ord(S[3]));
        N3:= HexCodeToInt(ord(S[4]))*16 + HexCodeToInt(ord(S[5]));
        Result:= RGBToColor(N1, N2, N3);
      end;
    3, 4:
      begin
        N1:= HexCodeToInt(ord(S[0]))*17;
        N2:= HexCodeToInt(ord(S[1]))*17;
        N3:= HexCodeToInt(ord(S[2]))*17;
        Result:= RGBToColor(N1, N2, N3);
      end;
  end;

  //some chars after '#rrggbb' must break the parsing, e.g. for this case: "#add-some-value"
  ch:= ord(S[LenOfColor]);
  case ch of
    ord('-'),
    ord('+'),
    ord('$'):
      Result:= DefaultColor;
  end;
end;


class procedure TATHtmlColorParser.SkipSpaces(const S: TStr; var N: SizeInt);
begin
  while (N<=Length(S)) and IsCodeSpace(ord(S[N])) do
    Inc(N);
end;

class procedure TATHtmlColorParser.SkipComma(const S: TStr; var N: SizeInt);
begin
  if S[N]=',' then
    Inc(N);
end;

class procedure TATHtmlColorParser.SkipCommaOrSlash(const S: TStr; var N: SizeInt);
begin
  if (S[N]=',') or (S[N]='/') then
    Inc(N);
end;

class function TATHtmlColorParser.SkipInt(const S: TStr; var N: SizeInt): integer;
begin
  SkipSpaces(S, N);
  if (N>Length(S)) or not IsCodeDigit(ord(S[N])) then
    Exit(-1);
  Result:= 0;
  while (N<=Length(S)) and IsCodeDigit(ord(S[N])) do
  begin
    Result:= Result*10 + ord(S[N]) - ord('0');
    Inc(N);
  end;
  SkipSpaces(S, N);
end;

class function TATHtmlColorParser.SkipIntMaybeInPercents(const S: TStr; var N: SizeInt): integer;
begin
  Result:= SkipInt(S, N);
  if N>Length(S) then exit(-1);
  if S[N]='%' then
  begin
    Inc(N);
    Result:= Result*255 div 100;
  end;
  SkipSpaces(S, N);
end;

class function TATHtmlColorParser.SkipIntWithPercent(const S: TStr; var N: SizeInt): integer;
begin
  Result:= SkipInt(S, N);
  if N>Length(S) then exit(-1);
  if S[N]='%' then
    Inc(N)
  else
    exit(-1);
  SkipSpaces(S, N);
end;


class function TATHtmlColorParser.SkipFloat(const S: TStr; var N: SizeInt;
  CalcValue, SkipPercent: boolean; out Ok: boolean): double;
var
  NEnd: SizeInt;
  Pow: double;
  Neg, HasDigit: boolean;
begin
  Ok:= false;
  Result:= 0.0;
  SkipSpaces(S, N);
  NEnd:= N;
  Neg:= false;
  if S[NEnd]='-' then begin Neg:= true; Inc(NEnd); end;

  HasDigit:= false;
  while (NEnd<=Length(S)) and IsCodeDigit(ord(S[NEnd])) do
  begin
    if CalcValue then Result:= Result*10.0 + ord(S[NEnd]) - ord('0');
    HasDigit:= true;
    Inc(NEnd);
  end;

  if (NEnd<=Length(S)) and (S[NEnd]='.') then
  begin
    Inc(NEnd);
    Pow:= 0.1;
    while (NEnd<=Length(S)) and IsCodeDigit(ord(S[NEnd])) do
    begin
      if CalcValue then Result:= Result + (ord(S[NEnd]) - ord('0')) * Pow;
      Pow:= Pow * 0.1;
      HasDigit:= true;
      Inc(NEnd);
    end;
  end;

  if not HasDigit then Exit;

  if CalcValue and Neg then Result:= -Result;
  Ok:= true;
  N:= NEnd;
  SkipSpaces(S, N);
  if SkipPercent and (S[N]='%') then
  begin
    Inc(N);
    SkipSpaces(S, N);
  end;
end;


class function TATHtmlColorParser.ParseFunctionRGB(const S: TStr;
  FromPos: SizeInt; out LenOfColor: integer): TColor;
var
  NLen, N: SizeInt;
  Val1, Val2, Val3: integer;
  ValAlpha: double;
  bAlpha, bOk: boolean;
begin
  Result:= clNone;
  LenOfColor:= 0;

  NLen:= Length(S);
  N:= FromPos;

  if N+9>NLen then exit;
  if S[N]<>'r' then exit;
  Inc(N);
  if S[N]<>'g' then exit;
  Inc(N);
  if S[N]<>'b' then exit;
  Inc(N);
  if S[N]='a' then
    Inc(N);
  if S[N]<>'(' then exit;
  Inc(N);

  Val1:= SkipIntMaybeInPercents(S, N);
  if Val1<0 then exit;
  if Val1>255 then exit;
  if N>NLen then exit;
  SkipComma(S, N);

  Val2:= SkipIntMaybeInPercents(S, N);
  if Val2<0 then exit;
  if Val2>255 then exit;
  if N>NLen then exit;
  SkipComma(S, N);

  Val3:= SkipIntMaybeInPercents(S, N);
  if Val3<0 then exit;
  if Val3>255 then exit;
  if N>NLen then exit;
  SkipSpaces(S, N);

  //allow 'alpha' part always
  bAlpha:= (S[N]=',') or (S[N]='/');
  if bAlpha then
  begin
    SkipCommaOrSlash(S, N);
    ValAlpha:= SkipFloat(S, N, false{CalcValue}, true, bOk);
    //if ValAlpha<0 then exit; //CalcValue=False so ValAlpha is always 0.0
  end;

  if S[N]<>')' then exit;

  Result:= RGBToColor(byte(Val1), byte(Val2), byte(Val3));
  LenOfColor:= N-FromPos+1;
end;

class function TATHtmlColorParser.ParseAngleUnit(const S: TStr; var N: SizeInt;
  var ValAngle: double): boolean;
{
Supports units: 'deg', 'rad', 'grad', 'turn'.
If any of units are found in S at position N, it multiplies ValAngle (in degrees)
by corresponding factor (for 'deg' is doesn't multiply),
and increases N by the length of unit-string.
}
begin
  Result:= true;
  if (S[N]='d') and (S[N+1]='e') and (S[N+2]='g') then
  begin
    Inc(N, 3);
    if IsCodeWord(Ord(S[N])) then exit(false);
  end
  else
  if (S[N]='r') and (S[N+1]='a') and (S[N+2]='d') then
  begin
    ValAngle:= ValAngle*(360.0/2/Pi);
    Inc(N, 3);
    if IsCodeWord(Ord(S[N])) then exit(false);
  end
  else
  if (S[N]='g') and (S[N+1]='r') and (S[N+2]='a') and (S[N+3]='d') then
  begin
    ValAngle:= ValAngle*(360.0/400.0);
    Inc(N, 4);
    if IsCodeWord(Ord(S[N])) then exit(false);
  end
  else
  if (S[N]='t') and (S[N+1]='u') and (S[N+2]='r') and (S[N+3]='n') then
  begin
    ValAngle:= ValAngle*360.0;
    Inc(N, 4);
    if IsCodeWord(Ord(S[N])) then exit(false);
  end;
end;

class function TATHtmlColorParser.ParseFunctionHSL(const S: TStr;
  FromPos: SizeInt; out LenOfColor: integer): TColor;
const
  cMaxDegrees=1500.0;
var
  NLen, N: SizeInt;
  ValAngle: double;
  Val2, Val3: integer;
  ValAlpha: double;
  bAlpha, bOk: boolean;
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

  //H component
  ValAngle:= SkipFloat(S, N, true, false, bOk);
  if not bOk then exit;
  if N>NLen then exit;
  if N+4<=NLen then
    if not ParseAngleUnit(S, N, ValAngle) then exit;
  if ValAngle>cMaxDegrees then exit;
  if ValAngle<-cMaxDegrees then exit;
  while ValAngle<0.0 do
    ValAngle:= ValAngle+360.0;
  while ValAngle>360.0 do
    ValAngle:= ValAngle-360.0;
  SkipComma(S, N);

  //S component
  Val2:= SkipIntWithPercent(S, N);
  if Val2<0 then exit;
  if Val2>100 then exit;
  if N>NLen then exit;
  SkipComma(S, N);

  //L component
  Val3:= SkipIntWithPercent(S, N);
  if Val3<0 then exit;
  if Val3>100 then exit;
  if N>NLen then exit;

  //Alpha
  if bAlpha and (S[N]<>')') then
  begin
    SkipCommaOrSlash(S, N);
    ValAlpha:= SkipFloat(S, N, false{CalcValue}, true, bOk);
    //if ValAlpha<0 then exit; //CalcValue=False so ValAlpha is always 0.0
  end;
  if S[N]<>')' then exit;

  Result:= HLStoColor(
    byte(Round(ValAngle*(255.0/360.0))),
    byte(Val3 * 255 div 100),
    byte(Val2 * 255 div 100)
    );
  LenOfColor:= N-FromPos+1;
end;


end.

