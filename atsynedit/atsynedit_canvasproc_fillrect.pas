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

uses
  {$ifdef LCLGtk3}
  Gtk3Objects,
  Cairo,
  {$endif}
  LCLType;

{$ifdef LCLGtk3}
{$define HAS_F}
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  Cr: Pcairo_t;
begin
  if (C.Handle = 0) then Exit;

  cr := pcairo_t(TGtk3DeviceContext(C.Handle).pcr);
  if cr = nil then Exit;

  cairo_set_source_rgb(Cr,
    (AColor and $FF) / 255.0,
    ((AColor shr 8) and $FF) / 255.0,
    ((AColor shr 16) and $FF) / 255.0);

  cairo_rectangle(Cr, R.Left, R.Top, R.Width, R.Height);
  cairo_fill(Cr);
end;
{$endif}

{$ifndef HAS_F}
procedure CanvasFillRect(C: TCanvas; const R: TRect; AColor: TColor);
begin
  C.Brush.Style:= bsSolid;
  C.Brush.Color:= AColor;
  C.FillRect(R);
end;
{$endif}

end.
