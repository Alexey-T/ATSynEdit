{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CanvasProc_FillRect;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Types;  
  
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);

implementation

procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);
begin
  C.Brush.Style:= bsSolid;
  C.Brush.Color:= AColor;
  C.FillRect(R);
end;

end.