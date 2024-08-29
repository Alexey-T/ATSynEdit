unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

uses
  Gtk2Int, multitextout_gtk2;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  C: TCanvas;
  Delta0: array of integer;
  r0, r1: TMultiTextRec;
begin
  C:= Canvas;
  C.Brush.Color:= clMoneyGreen;
  C.Font.Color:= clRed;
  C.Font.Size:= 12;
  C.TextOut(10, 10, 'textout');

  C.Brush.Color:= clCream;
  C.Font.Color:= clGreen;

  Delta0:= [10, 10, 10, 10, 10, 10, 10];
  r0.X:= 30;
  r0.Y:= 30;
  r0.Str:= 'text_0';
  r0.Dx:= @Delta0[0];

  r1.X:= 50;
  r1.Y:= 50;
  r1.Str:= 'text_1';
  r1.Dx:= nil;

  Gtk2Widgetset.MultiTextOut(
    C.Handle,
    [r0, r1]
    );
end;

end.

