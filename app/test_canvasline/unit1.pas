unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
var
  c: tcanvas;
begin
  c:= self.canvas;
  c.brush.color:= clyellow;
  c.fillrect(0, 0, 200, 200);

  c.pen.color:= clred;
  c.line(10,10,30,10);
  c.line(30,10,50,10);
  c.line(50,10,80,10);

  c.line(10,10,10,30);
  c.line(10,30,10,50);
  c.line(10,50,10,80);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  close;
end;

end.

