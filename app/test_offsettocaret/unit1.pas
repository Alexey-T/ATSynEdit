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
    procedure DoTest(const SText: string);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function GetTest1: string;
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

function GetTestPy: string;
begin
  Result:= ReadFileToString(ExtractFileDir(Application.ExeName)+'\py_test.txt');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memo1.Lines.Add('test 1: random len lines of "t" char...');
  DoTest(GetTest1);
  memo1.Lines.Add('test 2: some long python output...');
  DoTest(GetTestPy);
end;

procedure TForm1.DoTest(const SText: string);
var
  buf: TATStringBuffer;
  i: integer;
  pos: integer;
  pnt0, pnt: tpoint;
begin
  buf:= TATStringBuffer.create;
  buf.SetupSlow(SText);

  for i:= 0 to 1000 do
  begin
    pnt0.y:= random(buf.Count);
    pnt0.x:= random(buf.LineLength(pnt0.y));
    pos:= buf.CaretToStr(pnt0);
    pnt:= buf.StrToCaret(pos);
    if not PointsEqual(pnt, pnt0) then
    begin
      memo1.Lines.Add('  failed test: line:col '+inttostr(pnt0.y)+':'+inttostr(pnt0.x));
      exit;
    end;
  end;
  memo1.lines.add('  ok test');
end;

end.

