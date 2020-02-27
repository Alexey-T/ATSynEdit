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


end.

