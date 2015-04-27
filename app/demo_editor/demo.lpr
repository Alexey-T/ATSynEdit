program demo;

{$mode objfpc}{$H+}

uses
  //heaptrc,
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
  atsynedit_carets,
  atsynedit,
  atsynedit_gutter,
  atsynedit_commands,
  atsynedit_keymapping,
  atsynedit_wrapinfo, atsynedit_edits, atsynedit_ranges, formcombo;

{$R *.res}

begin
  Application.Title:= 'Demo';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmOpt, fmOpt);
  Application.Run;
end.

