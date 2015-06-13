unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ShellCtrls,
  ATSynEdit,
  ATStringProc,
  ATSynEdit_Adapter_EControl,
  ecSyntAnal,
  proc_lexer;

type
  { TfmMain }

  TfmMain = class(TForm)
    bOpen: TButton;
    chkFullHilite: TCheckBox;
    chkFullSel: TCheckBox;
    chkLexer: TCheckBox;
    chkShowCur: TCheckBox;
    chkUnpri: TCheckBox;
    chkWrap: TCheckBox;
    edLexer: TComboBox;
    files: TShellListView;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure bOpenClick(Sender: TObject);
    procedure chkFullHiliteChange(Sender: TObject);
    procedure chkFullSelChange(Sender: TObject);
    procedure chkLexerChange(Sender: TObject);
    procedure chkShowCurChange(Sender: TObject);
    procedure chkUnpriChange(Sender: TObject);
    procedure chkWrapChange(Sender: TObject);
    procedure edLexerChange(Sender: TObject);
    procedure filesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
    FDir: string;
    FFilename: string;
    procedure DoLexer(const aname: string);
    procedure DoOpen(const fn: string);
    procedure EditCalcStaple(Snd: TObject; ALine, AIndent: integer; var AColor: TColor);
    procedure EditClickGutter(Sender: TObject; ABand: integer; ALine: integer);
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
  FFilename:= fn;

  adapter.Lexer:= nil;
  ed.LoadFromFile(fn);
  ed.SetFocus;

  an:= DoFindLexerForFilename(manager, fn);
  adapter.Lexer:= an;

  if Assigned(an) then
    edLexer.ItemIndex:= edLexer.Items.IndexOf(an.LexerName);
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fname_lxl: string;
begin
  FDir:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+'/test_syntax_files/';
  fname_lxl:= ExtractFilePath(Application.ExeName)+'lexlib.lxl';

  manager:= TSyntaxManager.Create(Self);
  manager.LoadFromFile(fname_lxl);
  UpdateLexList;

  ed:= TATSynEdit.Create(Self);
  ed.Font.Name:= 'Courier New';
  ed.Parent:= Self;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.Colors.TextBG:= $e0f0f0;
  ed.Colors.CurrentLineBG:= clTeal;

  ed.OnClickGutter:= @EditClickGutter;
  ed.OnCalcStaple:= @EditCalcStaple;

  adapter:= TATAdapterEControl.Create;
  ed.AdapterHilite:= adapter;

  chkWrap.Checked:= ed.OptWrapMode=cWrapOn;
  chkFullSel.Checked:= ed.OptShowFullSel;
  chkFullHilite.Checked:= ed.OptShowFullHilite;
  chkUnpri.Checked:= ed.OptUnprintedVisible;
  chkShowCur.Checked:= ed.OptShowCurLine;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  if DirectoryExists(FDir) then
    files.Root:= FDir;
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  if chkWrap.checked then
    ed.OptWrapMode:= cWrapOn
  else
    ed.OptWrapMode:= cWrapOff;
end;

procedure TfmMain.chkFullSelChange(Sender: TObject);
begin
  ed.OptShowFullSel:= chkFullSel.Checked;
  ed.Update;
end;

procedure TfmMain.chkLexerChange(Sender: TObject);
begin
  adapter.Lexer:= nil;
  ed.Fold.Clear;

  if chkLexer.Checked then
    adapter.Lexer:= DoFindLexerForFilename(manager, FFilename);
  ed.Update;
end;

procedure TfmMain.chkShowCurChange(Sender: TObject);
begin
  ed.OptShowCurLine:= chkShowCur.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnpriChange(Sender: TObject);
begin
  ed.OptUnprintedVisible:= chkUnpri.Checked;
  ed.Update;
end;

procedure TfmMain.chkFullHiliteChange(Sender: TObject);
begin
  ed.OptShowFullHilite:= chkFullHilite.Checked;
  ed.Update;
end;

procedure TfmMain.bOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    Filename:= '';
    InitialDir:= FDir;
    if not Execute then exit;
    DoOpen(Filename);
  end;
end;

procedure TfmMain.DoLexer(const aname: string);
begin
  adapter.Lexer:= manager.FindAnalyzer(aname);
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

procedure TfmMain.EditClickGutter(Sender: TObject; ABand: integer; ALine: integer);
begin
  if ABand=ed.GutterBandBm then
  begin
    if ed.Strings.LinesBm[ALine]<>0 then
      ed.Strings.LinesBm[ALine]:= 0
    else
    begin
      ed.Strings.LinesBm[ALine]:= 1;
      ed.Strings.LinesBmColor[ALine]:= clMoneyGreen;
    end;
    ed.Update;
  end;
end;

procedure TfmMain.EditCalcStaple(Snd: TObject; ALine, AIndent: integer; var AColor: TColor);
const
  nColors = 10;
  cl: array[0..nColors-1] of TColor = (
    clGray,
    clBlue,
    clRed,
    clGreen,
    clOlive,
    clMaroon,
    clLime,
    clMoneyGreen,
    clNavy,
    clTeal
    );
begin
  AColor:= cl[AIndent div 2 mod nColors];
end;

end.

