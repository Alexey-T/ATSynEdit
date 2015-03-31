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
    chkShowNumBg: TCheckBox;
    chkTabSp: TCheckBox;
    chkUndoSv: TCheckBox;
    chkUndoGr: TCheckBox;
    chkCutNoSel: TCheckBox;
    chkDotLn: TCheckBox;
    chkClickNm: TCheckBox;
    chkCrUnfocus: TCheckBox;
    chkEnd: TCheckBox;
    chkUninKeep: TCheckBox;
    chkAutoInd: TCheckBox;
    chkTabInd: TCheckBox;
    chkHome: TCheckBox;
    chkLeftRt: TCheckBox;
    chkNavWrap: TCheckBox;
    chkOvrSel: TCheckBox;
    chkRtMove: TCheckBox;
    chkDnD: TCheckBox;
    chkCrMul: TCheckBox;
    chkCrVirt: TCheckBox;
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
    chkOvrPaste: TCheckBox;
    chkRepSpec: TCheckBox;
    edAutoInd: TComboBox;
    edCrShape: TComboBox;
    edCrShape2: TComboBox;
    edCrTime: TSpinEdit;
    edChars: TEdit;
    edIndent: TSpinEdit;
    edNum: TComboBox;
    edPage: TComboBox;
    edRulerFSize: TSpinEdit;
    edRulerSize: TSpinEdit;
    LabChars: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    edSizeBm: TSpinEdit;
    edSizeState: TSpinEdit;
    edSizeEmpty: TSpinEdit;
    edUndo: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure InitShape(ed: TCombobox);
  end;

var
  fmOpt: TfmOpt;

implementation

{$R *.lfm}

{ TfmOpt }

procedure TfmOpt.FormCreate(Sender: TObject);
begin
  InitShape(edCrShape);
  InitShape(edCrShape2);
end;

procedure TfmOpt.InitShape(ed: TCombobox);
begin
  ed.Items.Clear;
  ed.Items.Add('full');
  ed.Items.Add('vert 1px');
  ed.Items.Add('vert 2px');
  ed.Items.Add('vert 3px');
  ed.Items.Add('vert 4px');
  ed.Items.Add('vert 10%');
  ed.Items.Add('vert 20%');
  ed.Items.Add('vert 30%');
  ed.Items.Add('vert 40%');
  ed.Items.Add('vert 50%');
  ed.Items.Add('horz 1px');
  ed.Items.Add('horz 2px');
  ed.Items.Add('horz 3px');
  ed.Items.Add('horz 4px');
  ed.Items.Add('horz 10%');
  ed.Items.Add('horz 20%');
  ed.Items.Add('horz 30%');
  ed.Items.Add('horz 40%');
  ed.Items.Add('horz 50%');
end;

end.

