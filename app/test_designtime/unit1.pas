unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ATSynEdit, ATSynEdit_Edits;

type
  { TfmMain }

  TfmMain = class(TForm)
    combo: TATComboEdit;
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
end;

end.

