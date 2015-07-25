unit ATSynEdit_Register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATSynEdit, ATSynEdit_Edits,
  LResources;

procedure Register;

implementation

{ Registration }
procedure Register;
begin
  RegisterComponents('Misc', [TATSynEdit, TATEdit, TATComboEdit]);
end;

initialization

//lrs file must be made by command:
// ~/lazarus/tools/lazres icons.lrs *.png
{$I res/ide/icons.lrs}

end.

