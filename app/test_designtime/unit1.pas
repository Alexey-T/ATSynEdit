unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ATSynEdit,
  ATSynEdit_Edits;

type

  { TForm1 }

  TForm1 = class(TForm)
    ATComboEdit1: TATComboEdit;
    ATEdit1: TATEdit;
    ATSynEdit1: TATSynEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

