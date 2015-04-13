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
    b: TATSimpleButton;
    procedure Cl(Snd: TObject);
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
  b.SetBounds(50, 100, 150, 30);
  b.Caption:= 'Test caption';
  b.OnClick:= @Cl;
end;

procedure TForm1.Cl(Snd: TObject);
begin
  beep;
end;

end.

