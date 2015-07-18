unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  atstringproc_finder;

type

  { TForm1 }

  TForm1 = class(TForm)
    bFIndNext: TButton;
    bFInd: TButton;
    chkForw: TCheckBox;
    chkCase: TCheckBox;
    chkWords: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure bFIndNextClick(Sender: TObject);
    procedure bFIndClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DoFind(ANext: boolean);
    { private declarations }
  public
    FPos: integer;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function Iswordchar(ch: widechar): boolean;
begin
  case ch of
    '0'..'9',
    'a'..'z',
    'A'..'Z',
    '_': result:= true;
  else
    result:= false;
  end;
end;

procedure TForm1.bFIndClick(Sender: TObject);
begin
  DoFInd(false);
end;

procedure TForm1.bFIndNextClick(Sender: TObject);
begin
  DoFInd(true);
end;

procedure TForm1.DoFind(ANext: boolean);
var
  s, f: Unicodestring;
begin
  f:= trim(Memo1.Text);
  s:= trim(Memo2.Text);

  if not ANext then
    if chkForw.Checked then FPos:= 1
    else FPos:= Length(s);

  if ANext then
    if FPos=0 then Exit else
    if chkForw.Checked then Inc(FPos) else Dec(FPos);

  FPos:= SFindTextW(f, s, @Iswordchar, FPos,
    chkForw.Checked, chkWords.Checked, chkCase.Checked);
  if FPos=0 then memo3.text:= '(not found)'
  else memo3.text:= copy(s, FPos-2, length(f)+4);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPos:= 1;
end;

end.

