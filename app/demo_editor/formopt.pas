unit formopt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Spin, ComCtrls;

type
  { TfmOpt }

  TfmOpt = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkHomeEnd: TCheckBox;
    chkLeftRt: TCheckBox;
    chkRtMove: TCheckBox;
    chkDnD: TCheckBox;
    chkAutoInd: TCheckBox;
    chkCaretMul: TCheckBox;
    chkCaretVirtual: TCheckBox;
    chkClick2: TCheckBox;
    chkClick2W: TCheckBox;
    chkClick3: TCheckBox;
    chkColorSel: TCheckBox;
    chkCopyNoSel: TCheckBox;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    chkGutterBm: TCheckBox;
    chkGutterEmpty: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterStat: TCheckBox;
    chkLastOnTop: TCheckBox;
    chkNavWrap: TCheckBox;
    chkOvrPaste: TCheckBox;
    chkOvrSel: TCheckBox;
    chkRepSpec: TCheckBox;
    chkTabSp: TCheckBox;
    edCaretTime: TSpinEdit;
    edChars: TEdit;
    edPage: TComboBox;
    edAutoInd: TComboBox;
    edRulerFont: TSpinEdit;
    edRulerH: TSpinEdit;
    LabChars: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    PageControl1: TPageControl;
    edSizeBm: TSpinEdit;
    edSizeState: TSpinEdit;
    edSizeEmpty: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
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

