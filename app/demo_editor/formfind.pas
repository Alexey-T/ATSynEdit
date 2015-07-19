unit formfind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls;

type

  { TfmFind }

  TfmFind = class(TForm)
    bFindFirst: TButton;
    bCancel: TButton;
    chkRegex: TCheckBox;
    chkBack: TCheckBox;
    chkCase: TCheckBox;
    chkWords: TCheckBox;
    edFind: TEdit;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmFind: TfmFind;

implementation

{$R *.lfm}

{ TfmFind }

end.

