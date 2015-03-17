unit formkey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, comctrls,
  ATSynedit;

type
  { TfmKey }

  TfmKey = class(TForm)
    List: TListView;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    edit: TATSynEdit;
  end;

var
  fmKey: TfmKey;

implementation

{$R *.lfm}

{ TfmKey }

procedure TfmKey.FormShow(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to edit.KeyMapping.Count-1 do
    with edit.KeyMapping.Items[i] do
      with List.Items.Add do
      begin
        Caption:= Name;
        SubItems.Add(Keys1[0]);
        SubItems.Add(Keys2[0]);
      end;
end;

end.

