program demo;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  ATStringProc,
  ATStringProc_WordJump,
  ATStrings,
  ATStrings_Undo,
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_CanvasProc,
  ATSynEdit_Keymap,
  ATSynEdit_Gutter,
  ATSynEdit_Commands,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Wrapinfo,
  ATSynEdit_Edits,
  ATSynEdit_Ranges, 
  ATSynEdit_Adapters,
  ATSynEdit_Export_HTML,
  ATSynEdit_Gaps,
  formmain,
  formkey,
  formopt,
  formfind;

{$R *.res}

begin
  Application.Title:= 'Demo';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmOpt, fmOpt);
  Application.Run;
end.

