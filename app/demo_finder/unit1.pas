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
    chkRegex: TCheckBox;
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
    Finder: TATTextFinder;
    procedure DoFind(ANext: boolean);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bFIndClick(Sender: TObject);
begin
  DoFind(false);
end;

procedure TForm1.bFIndNextClick(Sender: TObject);
begin
  DoFind(true);
end;

procedure TForm1.DoFind(ANext: boolean);
begin
  FInder.StrFind:= trim(Memo1.Text);
  FInder.StrText:= trim(Memo2.Text);
  Finder.OptCaseSens:= chkCase.Checked;
  Finder.OptWholeWords:= chkWords.Checked;
  Finder.OptForward:= chkForw.Checked;
  FInder.OptRegex:= chkRegex.Checked;
  if not FInder.Find(ANext) then
    memo3.text:= '(not found)'
  else
  begin
    memo3.text:= Copy(Finder.StrText, Finder.MatchPos-2, FInder.MatchLen+4);
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FInder:= TATTextFinder.Create;
end;

end.

