unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  atstringproc_textbuffer;

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
  tt: TATStringBuffer;
  s: tstringlist;
  list: tlist;
  i: integer;
  x0, y0, x1, y1, pos: integer;
  str: string;
begin
  s:= tstringlist.create;
  for i:= 0 to 1000 do
    s.add(stringofchar('t', random(5)));

  list:= tlist.create;
  for i:= 0 to s.count-1 do
    list.add(pointer(length(s[i])));
  list.add(nil);

  tt:= TATStringBuffer.create;
  str:= s.text;
  tt.settext(str, list);

  for i:= 0 to 20 do
  begin
    y0:= random(s.count);
    x0:= random(length(s[y0]));
    tt.CaretPosToStrPos(y0, x0, pos);
    tt.StrPosToCaretPos(pos, y1, x1);
    if (x0=x1) and (y0=y1) then
      continue
    else
    begin
      memo1.lines.add('bad test: line:col '+inttostr(y0)+':'+inttostr(x0));
      exit;
    end;
  end;
  memo1.lines.add('ok test');
end;

end.

