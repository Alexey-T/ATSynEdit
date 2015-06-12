program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, ATSynEdit, atsynedit_adapters, ATSynEdit_CanvasProc,
  ATStringProc_TextBuffer, ATSynEdit_Ranges, ecLists, ecStrUtils,
  eczregexpr, ecsyntanal
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

