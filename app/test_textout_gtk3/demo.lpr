program demo;

{$mode objfpc}{$H+}

{$ifndef LCLGtk3}
  {$fatal This test requires the GTK3 widgetset}
{$endif}

uses
  Interfaces, Forms, Graphics,
  ATSynEdit_CanvasProc_Text_Gtk3;

var
  Bitmap: TBitmap;
  X, Y, InkPixels: Integer;
begin
  Application.Initialize;
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(128, 64);
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);
    Bitmap.Canvas.Font.Name := 'Sans';
    Bitmap.Canvas.Font.Height := -20;
    Bitmap.Canvas.Font.Color := clBlack;

    NativeTextOut(Bitmap.Canvas, 4, 4, 'Test');

    InkPixels := 0;
    for Y := 0 to Bitmap.Height - 1 do
      for X := 0 to Bitmap.Width - 1 do
        if ColorToRGB(Bitmap.Canvas.Pixels[X, Y]) <> clWhite then
          Inc(InkPixels);
    if InkPixels = 0 then
    begin
      WriteLn(StdErr, 'NativeTextOut did not draw any text');
      ExitCode := 1;
    end
    else
      WriteLn('GTK3 native text: PASS');
  finally
    Bitmap.Free;
  end;
end.
