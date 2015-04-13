unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ATButtons;

type
  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    b, b2, b3: TATSimpleButton;
    procedure FClick(Snd: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  b:= TATSimpleButton.create(self);
  b.parent:= self;
  b.SetBounds(50, 100, 180, 40);
  b.Caption:= 'Test caption';
  b.Font.Size:= 18;
  b.Font.Color:= clNavy;
  b.OnClick:= @FClick;

  b2:= TATSimpleButton.create(self);
  b2.parent:= self;
  b2.SetBounds(50, 150, 80, 30);
  b2.Caption:= 'Check1';
  b2.Checkable:= true;

  b3:= TATSimpleButton.create(self);
  b3.parent:= self;
  b3.SetBounds(130, 150, 80, 30);
  b3.Caption:= 'Check2';
  b3.Checkable:= true;
end;

procedure TForm1.FClick(Snd: TObject);
begin
  beep;
end;

end.

