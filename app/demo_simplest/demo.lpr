program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1
  { you can add units after this };

{$R *.res}

begin
  Application.Title:= 'Demo';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

