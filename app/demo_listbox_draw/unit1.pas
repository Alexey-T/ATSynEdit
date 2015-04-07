unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, types;

type
  { TForm1 }

  TForm1 = class(TForm)
    L: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure LDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses lcltype;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i:= 1 to 15 do
    L.Items.Add('Item '+inttostr(i));
end;

procedure TForm1.LDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var c: tcanvas;
begin
  c:= L.canvas;

  c.brush.color:= clcream;
  c.FillRect(arect);
  if odselected in state then
    c.pen.color:= clred
  else
    c.Pen.color:= clgreen;
  InflateRect(ARect, -3, -1);
  c.Rectangle(ARect);
  c.font.color:= clgreen;
  c.TextOut(6, arect.Top+4, 'Item '+inttostr(index));
end;

end.
