program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, atstringproc_textbuffer
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

