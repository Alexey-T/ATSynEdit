unit formcombo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, ATSynEdit_Edits;

type
  { TfmCombo }

  TfmCombo = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkEnabled: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    procedure chkEnabledChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ComboCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    { private declarations }
  public
    { public declarations }
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
  ed.Parent:= Panel1;
  ed.Align:= alBottom;
  ed.OnCommand:= @ComboCommand;
  ed.Text:= 'Test';
  ed.OptTextHint:= '(empty)';
end;

procedure TfmCombo.chkEnabledChange(Sender: TObject);
begin
  ed.Enabled:= chkEnabled.Checked;
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

