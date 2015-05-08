program demo;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  atsynedit_canvasproc,
  atsynedit_keymap,
  atstringproc,
  atstringproc_wordjump,
  atstrings,
  atstrings_undo,
  atsynedit_carets,
  atsynedit,
  atsynedit_gutter,
  atsynedit_commands,
  atsynedit_keymap_init,
  atsynedit_wrapinfo,
  atsynedit_edits,
  atsynedit_ranges,
  formmain,
  formkey,
  formopt,
  formcombo,
  proc_StreamComponent;

{$R *.res}

begin
  Application.Title:= 'Demo';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmOpt, fmOpt);
  Application.Run;
end.

