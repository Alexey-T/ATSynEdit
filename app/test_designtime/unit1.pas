unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ATSynEdit, ATSynEdit_Edits, LCLType;

type
  { TfmMain }

  TfmMain = class(TForm)
    ATComboEdit1: TATComboEdit;
    ed: TATSynEdit;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
   { private declarations }
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormShow(Sender: TObject);
begin
  ed.Strings.Clear;
  ed.Strings.LineAdd('This is demo text');
  ed.Strings.LineAdd('test');
  ed.Strings.LineAdd('test...');
  ed.Update(true);

  {
  combo2:= tatcomboedit.create(Self);
  combo2.parent:= panel1;
  combo2.left:= 350;
  combo2.width:= 300;
  combo2.top:= 20;
  }
end;

end.

