unit formkey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, comctrls,
  ButtonPanel, ATSynedit;

type
  { TfmCmd }

  TfmCmd = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TListView;
    procedure FormShow(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    edit: TATSynEdit;
  end;

var
  fmCmd: TfmCmd;

function DoCommandDialog(AEdit: TATSynEdit): integer;

implementation

uses
  Menus, LCLProc;

{$R *.lfm}

function DoCommandDialog(AEdit: TATSynEdit): integer;
begin
  Result:= 0;
  with TfmCmd.Create(nil) do
  try
    edit:= AEdit;
    if ShowModal=mrOk then
      if List.Selected<>nil then
        Result:= StrToIntDef(List.Selected.SubItems[2], 0);
  finally
    Free
  end;
end;

{ TfmCmd }

procedure TfmCmd.FormShow(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to edit.KeyMapping.Count-1 do
    with edit.KeyMapping.Items[i] do
      with List.Items.Add do
      begin
        Caption:= Name;
        SubItems.Add(ShortCutToText(Keys1[0]));
        SubItems.Add(ShortCutToText(Keys2[0]));
        SubItems.Add(Inttostr(Cmd));
      end;

  if List.Items.Count>0 then
    List.Selected:= List.Items[0];
end;

procedure TfmCmd.ListDblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

end.

