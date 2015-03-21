unit formopt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Spin;

type
  { TfmOpt }

  TfmOpt = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkGutterBm: TCheckBox;
    chkGutterEmpty: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterStat: TCheckBox;
    chkRepSpec: TCheckBox;
    chkColorSel: TCheckBox;
    chkCopyNoSel: TCheckBox;
    chkOvrPaste: TCheckBox;
    chkAutoInd: TCheckBox;
    chkTabSp: TCheckBox;
    chkCaretVirtual: TCheckBox;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    edCaretTime: TSpinEdit;
    GroupBox1: TGroupBox;
    Label8: TLabel;
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

