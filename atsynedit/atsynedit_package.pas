{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atsynedit_package;

interface

uses
  ATSynEdit_Register, ATStringProc, ATStringProc_TextBuffer, 
  atstringproc_utf8detect, ATStringProc_WordJump, ATStrings, ATStrings_Undo, 
  ATSynEdit, ATSynEdit_Adapters, ATSynEdit_CanvasProc, atsynedit_carets, 
  ATSynEdit_Commands, ATSynEdit_Edits, ATSynEdit_Gutter, ATSynEdit_Keymap, 
  ATSynEdit_Keymap_Init, ATSynEdit_Ranges, ATSynEdit_WrapInfo, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ATSynEdit_Register', @ATSynEdit_Register.Register);
end;

initialization
  RegisterPackage('atsynedit_package', @Register);
end.
