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
    chkWrap: TCheckBox;
    edLexer: TComboBox;
    files: TShellListView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure bOpenClick(Sender: TObject);
    procedure chkFullHiliteChange(Sender: TObject);
    procedure chkFullSelChange(Sender: TObject);
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
  filedir:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+'/test_syntax_files/';
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
  ed.OnClickGutter:= @EditClickGutter;
  ed.OnCalcStaple:= @EditCalcStaple;

  adapter:= TATAdapterEControl.Create;
  ed.AdapterOfHilite:= adapter;

  chkWrap.Checked:= ed.OptWrapMode=cWrapOn;
  chkFullSel.Checked:= ed.OptShowFullSel;
  chkFullHilite.Checked:= ed.OptShowFullHilite;
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

procedure TfmMain.chkFullSelChange(Sender: TObject);
begin
  ed.OptShowFullSel:= chkFullSel.Checked;
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
    InitialDir:= filedir;
    if not Execute then exit;
    DoOpen(Filename);
  end;
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

