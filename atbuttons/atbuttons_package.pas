{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atbuttons_package;

interface

uses
  ATButtons, atbuttons_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('atbuttons_register', @atbuttons_register.Register);
end;

initialization
  RegisterPackage('atbuttons_package', @Register);
end.
