unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  atstringproc_textbuffer, Types;

type
  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure DoLog(const s: string);
    procedure DoTest(const SText: string);
    function GetTest1: string;
    function GetTestPy: string;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.GetTest1: string;
var
  s: TStringList;
  i: integer;
begin
  s:= TStringList.Create;
  try
    for i:= 0 to 20 do
      s.Add(StringOfChar('t', Random(40)));
    Result:= s.Text;
  finally
    s.Free;
  end;
end;

function TForm1.GetTestPy: string;
var
  fn: string;
begin
  Result:= '';
  fn:= ExtractFilePath(Application.ExeName)+'py_test.txt';
  if not FileExists(fn) then
    DoLog('  file not found: '+fn)
  else
    Result:= ReadFileToString(fn);
end;

procedure TForm1.DoLog(const s: string);
begin
  memo1.Lines.Add(s);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoLog('test 1: random len lines of "t" char');
  DoTest(GetTest1);
  DoLog('');
  DoLog('test 2: some long python output');
  DoTest(GetTestPy);
end;

procedure TForm1.DoTest(const SText: string);
var
  buf: TATStringBuffer;
  i: integer;
  pos: integer;
  pnt0, pnt1: tpoint;
begin
  if SText='' then exit;
  buf:= TATStringBuffer.create;
  buf.SetupSlow(SText);

  DoLog('  lines: '+IntToStr(buf.Count));

  for i:= 0 to 1000 do
  begin
    pnt0.y:= random(buf.Count);
    pnt0.x:= random(buf.LineLength(pnt0.y));
    pos:= buf.CaretToStr(pnt0);
    pnt1:= buf.StrToCaret(pos);
    if not PointsEqual(pnt0, pnt1) then
    begin
      DoLog('  failed test: line:col '+IntToStr(pnt0.y)+':'+IntToStr(pnt0.x));
      exit;
    end;
  end;
  DoLog('  ok test');
end;

end.

