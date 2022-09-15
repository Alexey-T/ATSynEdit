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
    class procedure SkipSpaces(const S: TStr; var N: integer);
    class procedure SkipComma(const S: TStr; var N: integer);
    class procedure SkipCommaOrSlash(const S: TStr; var N: integer);
    class function SkipInt(const S: TStr; var N: integer): integer;
    class function SkipIntMaybeInPercents(const S: TStr; var N: integer): integer;
    class function SkipIntWithPercent(const S: TStr; var N: integer): integer;
    class function SkipFloat(const S: TStr; var N: integer; out Ok: boolean): double;
  public
    //convert TColor -> HTML color string #rrggbb
    class function ColorToHtmlString(Color: TColor): string;
    //convert string which starts with HTML color token #rgb, #rrggbb -> TColor, get len of color-string
    class function ParseTokenRGB(S: TPChar; out Len: integer; Default: TColor): TColor;
    //parses 'rgb(10,20,30)' and rgba(10,20,30,0.5)
    class function ParseFunctionRGB(const S: TStr; FromPos: integer; out LenOfColor: integer): TColor;
    //parses 'hsl(0,50%,100%)' and 'hsla(0,50%,100%,0.5)
    class function ParseFunctionHSL(const S: TStr; FromPos: integer; out LenOfColor: integer): TColor;
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

class function TATHtmlColorParser.ParseTokenRGB(S: TPChar; out Len: integer; Default: TColor): TColor;
var
  N1, N2, N3: integer;
  ch: word;
begin
  Result:= Default;
  if S=nil then Exit;

  if S^='#' then
    Inc(S);

  //must handle string longer than needed, with additional chars
  Len:= 0;
  repeat
    ch:= ord(S[Len]);
    if ch=0 then Break;
    if not IsCodeHexDigit(ch) then
      if IsCodeWord(ch) then
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
  ch:= ord(S[Len]);
  case ch of
    ord('-'),
    ord('+'),
    ord('$'):
      Result:= Default;
  end;
end;


class procedure TATHtmlColorParser.SkipSpaces(const S: TStr; var N: integer); inline;
begin
  while (N<=Length(S)) and IsCodeSpace(ord(S[N])) do
    Inc(N);
end;

class procedure TATHtmlColorParser.SkipComma(const S: TStr; var N: integer); inline;
begin
  if S[N]=',' then
    Inc(N);
end;

class procedure TATHtmlColorParser.SkipCommaOrSlash(const S: TStr; var N: integer); inline;
begin
  if (S[N]=',') or (S[N]='/') then
    Inc(N);
end;

class function TATHtmlColorParser.SkipInt(const S: TStr; var N: integer): integer;
begin
  Result:= -1;
  SkipSpaces(S, N);
  while (N<=Length(S)) and IsCodeDigit(ord(S[N])) do
  begin
    if Result=-1 then
      Result:= 0;
    Result:= Result*10 + ord(S[N]) - ord('0');
    Inc(N);
  end;
  SkipSpaces(S, N);
end;

class function TATHtmlColorParser.SkipIntMaybeInPercents(const S: TStr; var N: integer): integer;
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

class function TATHtmlColorParser.SkipIntWithPercent(const S: TStr; var N: integer): integer;
begin
  Result:= SkipInt(S, N);
  if N>Length(S) then exit(-1);
  if S[N]='%' then
    Inc(N)
  else
    exit(-1);
  SkipSpaces(S, N);
end;


class function TATHtmlColorParser.SkipFloat(const S: TStr; var N: integer; out Ok: boolean): double;
var
  Buf: string;
  NEnd: integer;
begin
  Ok:= false;
  Result:= 0.0;
  SkipSpaces(S, N);
  NEnd:= N;

  if S[NEnd]='-' then
    Inc(NEnd);
  while (NEnd<=Length(S)) and (IsCodeDigit(ord(S[NEnd])) or (S[NEnd]='.')) do
    Inc(NEnd);
  Buf:= Copy(S, N, NEnd-N);
  if Buf='' then exit;
  if Buf[1]='.' then
    Insert('0', Buf, 1);
  Ok:= TryStrToFloat(Buf, Result);

  N:= NEnd;
  SkipSpaces(S, N);
end;


class function TATHtmlColorParser.ParseFunctionRGB(const S: TStr; FromPos: integer; out LenOfColor: integer): TColor;
var
  NLen: integer;
var
  Val1, Val2, Val3: integer;
  ValAlpha: double;
  bAlpha, bOk: boolean;
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
    ValAlpha:= SkipFloat(S, N, bOk);
    if ValAlpha<0 then exit;
  end;
  if S[N]<>')' then exit;

  Result:= RGBToColor(byte(Val1), byte(Val2), byte(Val3));
  LenOfColor:= N-FromPos+1;
end;


class function TATHtmlColorParser.ParseFunctionHSL(const S: TStr; FromPos: integer; out LenOfColor: integer): TColor;
const
  cMaxDegrees=1500.0;
var
  NLen: integer;
  ValAngle: double;
  Val2, Val3: integer;
  ValAlpha: double;
  bAlpha, bOk: boolean;
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

  //H component
  ValAngle:= SkipFloat(S, N, bOk);
  if not bOk then exit;
  if N>NLen then exit;
  if N+4<=NLen then
  begin
    if (S[N]='d') and (S[N+1]='e') and (S[N+2]='g') then
    begin
      Inc(N, 3);
      if IsCodeWord(Ord(S[N])) then exit;
    end
    else
    if (S[N]='r') and (S[N+1]='a') and (S[N+2]='d') then
    begin
      ValAngle:= ValAngle*(360.0/2/Pi);
      Inc(N, 3);
      if IsCodeWord(Ord(S[N])) then exit;
    end
    else
    if (S[N]='g') and (S[N+1]='r') and (S[N+2]='a') and (S[N+3]='d') then
    begin
      ValAngle:= ValAngle*(360.0/400.0);
      Inc(N, 4);
      if IsCodeWord(Ord(S[N])) then exit;
    end
    else
    if (S[N]='t') and (S[N+1]='u') and (S[N+2]='r') and (S[N+3]='n') then
    begin
      ValAngle:= ValAngle*360.0;
      Inc(N, 4);
      if IsCodeWord(Ord(S[N])) then exit;
    end;
  end;
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
  if bAlpha and (S[N]<>')') then
  begin
    SkipCommaOrSlash(S, N);
    ValAlpha:= SkipFloat(S, N, bOk);
    if ValAlpha<0 then exit;
    if S[N]='%' then
    begin
      //ValAlpha:= ValAlpha div 100;
      Inc(N);
    end;
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

