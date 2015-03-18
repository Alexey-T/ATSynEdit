unit formopt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TfmOpt }

  TfmOpt = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmOpt: TfmOpt;

implementation

{$R *.lfm}

end.

