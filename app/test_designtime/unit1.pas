unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ATSynEdit, ATSynEdit_Edits;

type
  { TfmMain }

  TfmMain = class(TForm)
    ATComboEdit1: TATComboEdit;
    ATEdit1: TATEdit;
    ATSynEdit1: TATSynEdit;
    Panel1: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

end.

