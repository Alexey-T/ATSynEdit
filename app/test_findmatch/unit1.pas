unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ATSynEdit_Finder,
  ATSynEdit_RegExpr,
  ATSynEdit_Carets,
  ATSynEdit,
  Math;

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
    Ed: TATSynEdit;
    MemoRes: TMemo;
    MemoWhat: TEdit;
    procedure bFindNextClick(Sender: TObject);
    procedure bFindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoWhatChange(Sender: TObject);
  private
    Finder: TATEditorFinder;
    procedure DoFind(ANext: boolean);
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

procedure TfmMain.DoFind(ANext: boolean);
var
  bChanged: boolean;
begin
  Finder.OptCase:= chkCase.Checked;
  Finder.OptWords:= chkWords.Checked;
  Finder.OptRegex:= chkRegex.Checked;
  Finder.OptBack:= chkBack.Checked and not chkRegex.Checked;
  Finder.OptFromCaret:= ANext and not chkRegex.Checked;

  if Finder.DoAction_FindOrReplace(ANext, false, false, bChanged, true) then
  begin
    MemoRes.Text:= Format('(line %d)'#10, [Finder.MatchEdPos.Y+1]) +
      Ed.Strings.TextSubstring(Finder.MatchEdPos.X, Finder.MatchEdPos.Y, Finder.MatchEdEnd.X, Finder.MatchEdEnd.Y);
    Ed.DoGotoPos(
      Point(Finder.MatchEdPos.X, Finder.MatchEdPos.Y),
      Point(Finder.MatchEdEnd.X, Finder.MatchEdEnd.Y),
      5, 2,
      true,
      true);
  end
  else
    MemoRes.Text:= '(not found)';
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fn: string;
begin
  Finder:= TATEditorFinder.Create;
  Finder.Editor:= Ed;
  Finder.StrFind:= MemoWhat.Text;

  fn:= ExtractFilePath(Application.ExeName)+'unit1.pas';
  if FileExists(fn) then
    Ed.Strings.LoadFromFile(fn);
end;

procedure TfmMain.MemoWhatChange(Sender: TObject);
begin
  Finder.StrFind:= MemoWhat.Text;
end;

end.

