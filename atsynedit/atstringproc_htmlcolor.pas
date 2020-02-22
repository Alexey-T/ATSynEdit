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
function SHtmlColorToColor(s: string; out Len: integer; Default: TColor): TColor;


implementation

function IsCharWord(ch: char): boolean;
begin
  case ch of
    'a'..'z',
    'A'..'Z',
    '_',
    '0'..'9':
      Result:= true;
    else
      Result:= false
  end;
end;

function IsCharHex(ch: char): boolean;
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

function SHtmlColorToColor(s: string; out Len: integer; Default: TColor): TColor;
var
  N1, N2, N3: integer;
  i: integer;
begin
  Result:= Default;
  Len:= 0;
  if (s<>'') and (s[1]='#') then Delete(s, 1, 1);
  if (s='') then exit;

  //delete after first nonword char
  i:= 1;
  while (i<=Length(s)) and IsCharWord(s[i]) do Inc(i);
  Delete(s, i, Maxint);

  //allow only #rgb, #rrggbb
  Len:= Length(s);
  if (Len<>3) and (Len<>6) then exit;

  for i:= 1 to Len do
    if not IsCharHex(s[i]) then exit;

  if Len=6 then
  begin
    N1:= HexDigitToInt(s[1])*16 + HexDigitToInt(s[2]);
    N2:= HexDigitToInt(s[3])*16 + HexDigitToInt(s[4]);
    N3:= HexDigitToInt(s[5])*16 + HexDigitToInt(s[6]);
  end
  else
  begin
    N1:= HexDigitToInt(s[1])*17;
    N2:= HexDigitToInt(s[2])*17;
    N3:= HexDigitToInt(s[3])*17;
  end;

  Result:= RGBToColor(N1, N2, N3);
end;


end.

