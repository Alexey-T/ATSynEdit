unit formcombo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ATSynEdit_Edits;

type
  { TfmCombo }

  TfmCombo = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure ComboCommand(Snd: TObject; ACmd: integer; var AHandled: boolean);
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
end;

procedure TfmCombo.ComboCommand(Snd: TObject; ACmd: integer;
  var AHandled: boolean);
var
  s: string;
  n: integer;
begin
  if ACmd=cCommand_KeyEnter then
  begin
    with ed do
    begin
      s:= UTF8Encode(Text);
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

