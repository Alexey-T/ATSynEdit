unit atbuttons_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATButtons;

procedure Register;

implementation

{$R *.dcr}

{ Registration }
procedure Register;
begin
  RegisterComponents('Misc', [TATSimpleButton]);
end;

end.

