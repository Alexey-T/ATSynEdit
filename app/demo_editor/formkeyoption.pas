unit formkeyoption;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls;

type
  { TfmKeyOpt }

  TfmKeyOpt = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkCtrl: TCheckBox;
    chkAlt: TCheckBox;
    chkShift: TCheckBox;
    chkMeta: TCheckBox;
    ed: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

function DoDialogHotkey(S: string): string;

implementation

{$R *.lfm}

function DoDialogHotkey(S: string): string;
var
  n: integer;
begin
  Result:= S;
  with TfmKeyOpt.Create(nil) do
  try
    chkCtrl.Checked:= Pos('Ctrl+', S)>0;
    chkAlt.Checked:= Pos('Alt+', S)>0;
    chkShift.Checked:= Pos('Shift+', S)>0;
    chkMeta.Checked:= Pos('Meta+', S)>0;

    repeat
      n:= Pos('+', S);
      if n=0 then Break;
      Delete(S, 1, n);
    until false;

    ed.ItemIndex:= ed.Items.IndexOf(S);
    if ed.ItemIndex<0 then
      ed.ItemIndex:= 0;

    case ShowModal of
      mrOk:
        begin
          Result:= ed.Text;
          if chkMeta.Checked then Result:= 'Meta+'+Result;
          if chkShift.Checked then Result:= 'Shift+'+Result;
          if chkAlt.Checked then Result:= 'Alt+'+Result;
          if chkCtrl.Checked then Result:= 'Ctrl+'+Result;
        end;
      mrClose:
        Result:= '';
    end;
  finally
    Free;
  end;
end;

{ TfmKeyOpt }

procedure TfmKeyOpt.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i:= Ord('A') to Ord('Z') do
    ed.Items.Add(Chr(i));
  for i:= 0 to 9 do
    ed.Items.Add(Inttostr(i));
  for i:= 1 to 12 do
    ed.Items.Add('F'+Inttostr(i));

  ed.Items.Add('Left');
  ed.Items.Add('Right');
  ed.Items.Add('Up');
  ed.Items.Add('Down');
  ed.Items.Add('Ins');
  ed.Items.Add('Del');
  ed.Items.Add('Home');
  ed.Items.Add('End');
  ed.Items.Add('PgUp');
  ed.Items.Add('PgDn');
  ed.Items.Add('Enter');
  ed.Items.Add('BkSp');
  ed.Items.Add('Tab');
  ed.Items.Add('Esc');
  ed.Items.Add('-');
  ed.Items.Add('=');
  ed.Items.Add('`');
  ed.Items.Add(',');
  ed.Items.Add('.');
  ed.Items.Add(';');
  ed.Items.Add('''');
  ed.Items.Add('\');
  ed.Items.Add('/');
  ed.Items.Add('[');
  ed.Items.Add(']');

  for i:= 0 to 9 do
    ed.Items.Add('Num'+Inttostr(i));
  ed.Items.Add('NumPlus');
  ed.Items.Add('NumMinus');
  ed.Items.Add('NumMul');
  ed.Items.Add('NumDiv');
  ed.Items.Add('NumDot');
  ed.Items.Add('NumClear');

  ed.Items.Add('NumLock');
  ed.Items.Add('ScrollLock');
  ed.Items.Add('CapsLock');
  ed.Items.Add('Break');
  ed.Items.Add('PopUp');
end;

end.

