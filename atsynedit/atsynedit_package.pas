{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atsynedit_package;

interface

uses
  ATSynEdit, ATSynEdit_Edits, ATSynEdit_Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ATSynEdit_Register', @ATSynEdit_Register.Register);
end;

initialization
  RegisterPackage('atsynedit_package', @Register);
end.
