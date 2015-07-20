unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  atsynedit_finder, regexpr;

type

  { TForm1 }

  TForm1 = class(TForm)
    bFIndNext: TButton;
    bFInd: TButton;
    chkRegex: TCheckBox;
    chkBack: TCheckBox;
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
    function IsWordChar(ch: widechar): boolean;
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

function TForm1.IsWordChar(ch: Widechar): boolean;
begin
  Result:= Pos(ch, RegExprWordChars)>0;
end;

procedure TForm1.DoFind(ANext: boolean);
var
  FromPos, SkipLen: integer;
begin
  Finder.StrFind:= trim(Memo1.Text);
  Finder.StrText:= trim(Memo2.Text);
  Finder.OptCase:= chkCase.Checked;
  Finder.OptWords:= chkWords.Checked;
  Finder.OptBack:= chkBack.Checked;
  Finder.OptRegex:= chkRegex.Checked;

  SkipLen:= Finder.MatchLen;
  if ANext then
    FromPos:= Finder.MatchPos
  else
  if FInder.OptBack then
    FromPos:= Length(Finder.StrText)
  else
    FromPos:= 1;

  if not FInder.FindMatch(ANext, SkipLen, FromPos, @IsWordChar) then
    memo3.text:= '(not found)'
  else
    memo3.text:= 'context:'#13+Copy(Finder.StrText, Finder.MatchPos-2, FInder.MatchLen+4);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FInder:= TATTextFinder.Create;
end;

end.

