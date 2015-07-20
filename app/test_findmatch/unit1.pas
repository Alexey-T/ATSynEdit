unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  atsynedit_finder, regexpr;

type

  { TfmMain }

  TfmMain = class(TForm)
    bFindNext: TButton;
    bFind: TButton;
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
    procedure bFindNextClick(Sender: TObject);
    procedure bFindClick(Sender: TObject);
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
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.bFindClick(Sender: TObject);
begin
  DoFind(false);
end;

procedure TfmMain.bFindNextClick(Sender: TObject);
begin
  DoFind(true);
end;

function TfmMain.IsWordChar(ch: Widechar): boolean;
begin
  Result:= Pos(ch, RegExprWordChars)>0;
end;

procedure TfmMain.DoFind(ANext: boolean);
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
  if Finder.OptRegex then
    FromPos:= 1
  else
  if Finder.OptBack then
    FromPos:= Length(Finder.StrText)
  else
    FromPos:= 1;

  if not FInder.FindMatch(ANext, SkipLen, FromPos, @IsWordChar) then
    memo3.text:= '(not found)'
  else
    memo3.text:= 'context:'#13+Copy(Finder.StrText, Finder.MatchPos-2, FInder.MatchLen+4);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FInder:= TATTextFinder.Create;
end;

end.

