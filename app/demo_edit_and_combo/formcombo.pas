unit formcombo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, Spin, ATSynEdit_Edits;

type
  { TfmCombo }

  TfmCombo = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkEnMouse: TCheckBox;
    chkEnabled: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    LabelMaxLen: TLabel;
    PanelCombo: TPanel;
    PanelEdit: TPanel;
    edMaxLen: TSpinEdit;
    procedure chkEnabledChange(Sender: TObject);
    procedure chkEnMouseChange(Sender: TObject);
    procedure edMaxLenChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ComboCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    { private declarations }
  public
    { public declarations }
    ed0: TATEdit;
    ed: TATComboEdit;
  end;

var
  fmCombo: TfmCombo;

implementation

uses ATSynEdit_Commands;

{$R *.lfm}

{ TfmCombo }

procedure TfmCombo.FormCreate(Sender: TObject);
begin
  ed:= TATComboEdit.Create(Self);
  ed.Parent:= PanelCombo;
  ed.Align:= alBottom;
  ed.OnCommand:= @ComboCommand;
  //ed.Text:= 'combo';
  ed.OptTextHint:= '(empty)';

  ed0:= TATEdit.Create(Self);
  ed0.Parent:= PanelEdit;
  ed0.Align:= alBottom;
  //ed0.Text:= 'edit';
  ed0.OptTextHint:= '(empty)';
end;

procedure TfmCombo.chkEnabledChange(Sender: TObject);
begin
  ed.Enabled:= chkEnabled.Checked;
  ed0.Enabled:= chkEnabled.Checked;
end;

procedure TfmCombo.chkEnMouseChange(Sender: TObject);
begin
  ed.OptMouseEnableAll:= chkEnMouse.Checked;
  ed0.OptMouseEnableAll:= chkEnMouse.Checked;
end;

procedure TfmCombo.edMaxLenChange(Sender: TObject);
begin
  ed.OptMaxLen:= edMaxLen.Value;
  ed0.OptMaxLen:= edMaxLen.Value;
end;

procedure TfmCombo.ComboCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
var
  s: string;
  n: integer;
begin
  if ACmd=cCommand_KeyEnter then
  begin
    with ed do
    begin
      s:= UTF8Encode(Trim(Text));
      ShowMessage('Enter: '+s);

      Text:= '';
      DoCaretSingle(0, 0);

      n:= Items.IndexOf(s);
      if n>=0 then Items.Delete(n);
      Items.Insert(0, s);
    end;
    AHandled:= true;
  end;
end;


end.

