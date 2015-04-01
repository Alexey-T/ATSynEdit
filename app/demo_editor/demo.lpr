program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  formmain, formkey, formopt,
  atcanvasproc,
  atkeymapping,
  atstringproc,
  atstringproc_wordjump,
  atstrings,
  atstrings_undo,
  atcarets,
  atgutter,
  atsynedit,
  atsynedit_commands,
  atsynedit_keymapping,
  atsynedit_wrapinfo;

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmOpt, fmOpt);
  Application.Run;
end.

