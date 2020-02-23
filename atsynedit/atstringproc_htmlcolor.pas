{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_HtmlColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

//convert TColor -> HTML color string #rrggbb
function SColorToHtmlColor(Color: TColor): string;

//convert string which starts with HTML color token #rgb, #rrggbb -> TColor, get len of color-string
function SHtmlColorToColor(s: PChar; out Len: integer; Default: TColor): TColor;


implementation

function IsCharHex(ch: char): boolean; inline;
begin
  case ch of
    '0'..'9',
    'a'..'f',
    'A'..'F':
      Result:= true
    else
      Result:= false;
  end;
end;

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

function HexDigitToInt(ch: char): integer;
begin
  case ch of
    '0'..'9':
      Result:= Ord(ch)-Ord('0');
    'a'..'f':
      Result:= Ord(ch)-Ord('a')+10;
    'A'..'F':
      Result:= Ord(ch)-Ord('A')+10;
    else
      Result:= 0;
  end;
end;

function SHtmlColorToColor(s: PChar; out Len: integer; Default: TColor): TColor;
var
  N1, N2, N3: integer;
  ch: char;
begin
  Result:= Default;
  if s^=#0 then Exit;

  Len:= 0;
  if s^='#' then
    Inc(s);

  repeat
    ch:= s[Len];
    if ch=#0 then Break;
    if not IsCharHex(ch) then Exit;
    Inc(Len);
    if Len>6 then Exit;
  until false;

  //allow only #rgb, #rrggbb
  if Len=6 then
  begin
    N1:= HexDigitToInt(s[0])*16 + HexDigitToInt(s[1]);
    N2:= HexDigitToInt(s[2])*16 + HexDigitToInt(s[3]);
    N3:= HexDigitToInt(s[4])*16 + HexDigitToInt(s[5]);
    Result:= RGBToColor(N1, N2, N3);
  end
  else
  if Len=3 then
  begin
    N1:= HexDigitToInt(s[0])*17;
    N2:= HexDigitToInt(s[1])*17;
    N3:= HexDigitToInt(s[2])*17;
    Result:= RGBToColor(N1, N2, N3);
  end;
end;


end.

