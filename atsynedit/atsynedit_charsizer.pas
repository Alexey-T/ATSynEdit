{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CharSizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  LCLType, LCLIntf;

type
  TATCharSizer = class
  private
    FontName: string;
    FontSize: integer;
    Canvas: TCanvas;
    Sizes: packed array[word] of word;
    SizeAvg: integer;
  public
    procedure Init(const AFontName: string; AFontSize: integer; ACanvas: TCanvas);
    function GetCharWidth(ch: Widechar): integer;
  end;

var
  GlobalCharSizer: TATCharSizer;

implementation

procedure TATCharSizer.Init(const AFontName: string; AFontSize: integer; ACanvas: TCanvas);
begin
  if (FontName<>AFontName) or (FontSize<>AFontSize) then
  begin
    FontName:= AFontName;
    FontSize:= AFontSize;
    FillChar(Sizes, SizeOf(Sizes), 0);
  end;
  Canvas:= ACanvas;
  Canvas.Font.Name:= AFontName;
  Canvas.Font.Size:= AFontSize;
  SizeAvg:= Canvas.TextWidth('M');
end;

function TATCharSizer.GetCharWidth(ch: Widechar): integer;
begin
  Result:= Sizes[Ord(ch)];
  if Result=0 then
  begin
    Result:= Canvas.TextWidth(UTF8Encode(UnicodeString(ch))) * 100 div SizeAvg;
    Sizes[Ord(ch)]:= Result;
  end;
end;


initialization
  GlobalCharSizer:= TATCharSizer.Create;

finalization
  FreeAndNil(GlobalCharSizer);

end.

