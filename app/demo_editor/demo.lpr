program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formmain, formkey, atcanvasproc, atkeymapping, atstringproc,
  atstringproc_wordjump, atstrings, atcarets, atsynedit, atsynedit_commands,
  atsynedit_keymapping, ATGutter, formopt, atstrings_undo;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmOpt, fmOpt);
  Application.Run;
end.

