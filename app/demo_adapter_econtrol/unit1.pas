unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ShellCtrls, ComCtrls,
  ATSynEdit,
  ATStringProc,
  ATSynEdit_Adapter_EControl,
  ecSyntAnal,
  proc_lexer;

type
  { TfmMain }

  TfmMain = class(TForm)
    chkWrap: TCheckBox;
    edLexer: TComboBox;
    files: TShellListView;
    Panel1: TPanel;
    procedure chkWrapChange(Sender: TObject);
    procedure edLexerChange(Sender: TObject);
    procedure filesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
    filedir: string;
    procedure DoLexer(const aname: string);
    procedure DoOpen(const fn: string);
    procedure UpdateLexList;
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

var
  manager: TSyntaxManager;
  adapter: TATAdapterEControl;

{ TfmMain }

procedure TfmMain.UpdateLexList;
var
  i: integer;
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    for i:= 0 to manager.AnalyzerCount-1 do
      sl.Add(manager.Analyzers[i].LexerName);
    sl.sort;
    edLexer.Items.AddStrings(sl);
  finally
    sl.free;
  end;
end;

procedure TfmMain.DoOpen(const fn: string);
var
  an: TSyntAnalyzer;
begin
  adapter.SetLexer(nil);
  ed.LoadFromFile(fn);
  an:= DoFindLexerForFilename(manager, fn);
  adapter.SetLexer(an);

  edLexer.ItemIndex:= edLexer.Items.IndexOf(an.LexerName);
  ed.SetFocus;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fname_lxl: string;
begin
  filedir:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+'\test_syntax_files\';
  fname_lxl:= ExtractFilePath(Application.ExeName)+'lexlib.lxl';

  manager:= TSyntaxManager.Create(Self);
  manager.LoadFromFile(fname_lxl);
  UpdateLexList;

  ed:= TATSynEdit.Create(Self);
  ed.Font.Name:= 'Consolas';
  ed.Parent:= Self;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.Colors.TextBG:= $e0f0f0;

  adapter:= TATAdapterEControl.Create;
  ed.AdapterOfHilite:= adapter;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  files.Root:= filedir;
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  if chkWrap.checked then
    ed.OptWrapMode:= cWrapOn
  else
    ed.OptWrapMode:= cWrapOff;
end;

procedure TfmMain.DoLexer(const aname: string);
begin
  adapter.SetLexer(manager.FindAnalyzer(aname));
  ed.Update;
end;

procedure TfmMain.edLexerChange(Sender: TObject);
begin
  DoLexer(edLexer.Text);
end;

procedure TfmMain.filesClick(Sender: TObject);
var
  fn: string;
begin
  if files.Selected=nil then exit;
  fn:= files.GetPathFromItem(files.Selected);
  if FileExistsUTF8(fn) then
    DoOpen(fn);
end;

end.

