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
    chkCaretMul: TCheckBox;
    chkCaretVirtual: TCheckBox;
    chkClick3: TCheckBox;
    chkClick2: TCheckBox;
    chkLastOnTop: TCheckBox;
    chkNavWrap: TCheckBox;
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
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    edCaretTime: TSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
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

