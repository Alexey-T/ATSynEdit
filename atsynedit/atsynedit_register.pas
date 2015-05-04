unit ATSynEdit_Register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATSynEdit, ATSynEdit_Edits;

procedure Register;

implementation

//{$R *.dcr}

{ Registration }
procedure Register;
begin
  RegisterComponents('Misc', [TATSynEdit, TATEdit, TATComboEdit]);
end;

end.

