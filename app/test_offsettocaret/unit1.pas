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
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  buf: TATStringBuffer;
  s: tstringlist;
  list: tlist;
  i: integer;
  pos: integer;
  pnt0, pnt: tpoint;
begin
  s:= tstringlist.create;
  for i:= 0 to 20 do
    s.add(stringofchar('t', random(40)));

  list:= tlist.create;
  for i:= 0 to s.count-1 do
    list.add(pointer(length(s[i])));

  buf:= TATStringBuffer.create;
  buf.Setup(s.text, list, 1);

  for i:= 0 to 1000 do
  begin
    pnt0.y:= random(s.count);
    pnt0.x:= random(length(s[pnt0.y]));
    pos:= buf.CaretToStr(pnt0);
    pnt:= buf.StrToCaret(pos);
    if not PointsEqual(pnt, pnt0) then
    begin
      memo1.lines.add('bad test: line:col '+inttostr(pnt0.y)+':'+inttostr(pnt0.x));
      exit;
    end;
  end;
  memo1.lines.add('ok test');
end;

end.

