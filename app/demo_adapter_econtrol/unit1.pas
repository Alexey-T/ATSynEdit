unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ShellCtrls, ComCtrls,
  LclIntf, LclType,
  ATSynEdit,
  ATStringProc,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Carets,
  ATSynEdit_Bookmarks,
  ATSynEdit_Export_HTML,
  ec_SyntAnal,
  ec_proc_lexer;

type
  { TfmMain }

  TfmMain = class(TForm)
    bOpen: TButton;
    bExport: TButton;
    chkDyn: TCheckBox;
    chkFullHilite: TCheckBox;
    chkFullSel: TCheckBox;
    chkLexer: TCheckBox;
    chkShowCur: TCheckBox;
    chkUnpri: TCheckBox;
    chkWrap: TCheckBox;
    edLexer: TComboBox;
    files: TShellListView;
    ImageListTree: TImageList;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PanelText: TPanel;
    Splitter1: TSplitter;
    TimerMemory: TTimer;
    Tree: TTreeView;
    procedure AdapterParseBegin(Sender: TObject);
    procedure AdapterParseDone(Sender: TObject);
    procedure bExportClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure chkDynChange(Sender: TObject);
    procedure chkFullHiliteChange(Sender: TObject);
    procedure chkFullSelChange(Sender: TObject);
    procedure chkLexerChange(Sender: TObject);
    procedure chkShowCurChange(Sender: TObject);
    procedure chkUnpriChange(Sender: TObject);
    procedure chkWrapChange(Sender: TObject);
    procedure EditorChangeCaretPos(Sender: TObject);
    procedure edLexerChange(Sender: TObject);
    procedure filesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerMemoryTimer(Sender: TObject);
    procedure TreeClick(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
    FDirApp: string;
    FFilename: string;
    procedure DoLexer(const aname: string);
    procedure DoOpenFile(const fn: string);
    procedure EditCalcStaple(Sender: TObject; ALine, AIndent: integer; var AColor: TColor);
    procedure EditClickGutter(Sender: TObject; ABand: integer; ALine: integer);
    function GetComment: string;
    procedure UpdateLexList;
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

var
  manager: TecSyntaxManager;
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

procedure TfmMain.DoOpenFile(const fn: string);
var
  an: TecSyntAnalyzer;
begin
  FFilename:= fn;

  adapter.Lexer:= nil;
  Tree.Items.Clear;

  ed.LoadFromFile(fn);
  ed.SetFocus;

  an:= DoFindLexerForFilename(manager, fn);
  adapter.Lexer:= an;

  if Assigned(an) then
  begin
    edLexer.ItemIndex:= edLexer.Items.IndexOf(an.LexerName);
    ed.DoEventChange(0);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fn_lexlib: string;
begin
  FDirApp:= ExtractFileDir(Application.ExeName);
  {$ifdef darwin}
  //out of app bundle
  FDirApp:= ExtractFileDir(ExtractFileDir(ExtractFileDir(FDirApp)));
  {$endif}

  fn_lexlib:= ExtractFileDir(FDirApp)+DirectorySeparator+'lexlib'+DirectorySeparator+'lib.lxl';

  manager:= TecSyntaxManager.Create(Self);
  manager.LoadFromFile(fn_lexlib);
  UpdateLexList;

  ed:= TATSynEdit.Create(Self);
  ed.Font.Name:= 'Courier New';
  ed.Parent:= PanelText;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.Colors.TextBG:= $e0f0f0;
  ed.Colors.CurrentLineBG:= clTeal;

  ed.Gutter[ed.GutterBandNumbers].Visible:= false;
  ed.Gutter.Update;

  ed.OnClickGutter:= @EditClickGutter;
  ed.OnCalcStaple:= @EditCalcStaple;
  ed.OnChangeCaretPos:=@EditorChangeCaretPos;

  adapter:= TATAdapterEControl.Create(Self);
  adapter.OnParseBegin:=@AdapterParseBegin;
  adapter.OnParseDone:=@AdapterParseDone;
  adapter.AddEditor(ed);

  chkWrap.Checked:= ed.OptWrapMode=cWrapOn;
  chkFullSel.Checked:= ed.OptShowFullWidthForSelection;
  chkFullHilite.Checked:= ed.OptShowFullWidthForSyntaxHilite;
  chkUnpri.Checked:= ed.OptUnprintedVisible;
  chkShowCur.Checked:= ed.OptShowCurLine;
  chkDyn.Checked:= adapter.DynamicHiliteEnabled;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  if DirectoryExists(FDirApp) then
    files.Root:= ExtractFileDir(ExtractFileDir(FDirApp))+DirectorySeparator+'test_syntax_files';
end;

procedure TfmMain.TimerMemoryTimer(Sender: TObject);
var
  MM: TMemoryManager;
begin
  GetMemoryManager(MM);
  Label1.Caption:= 'Used mem: '+IntToStr(MM.GetFPCHeapStatus().CurrHeapUsed);
end;

procedure TfmMain.TreeClick(Sender: TObject);
var
  Obj: TObject;
  Rng: TATRangeInCodeTree;
begin
  if adapter.TreeBusy then exit;
  if Tree.Selected=nil then exit;
  if Tree.Selected.Data=nil then exit;

  Obj:= TObject(Tree.Selected.Data);
  if not (Obj is TATRangeInCodeTree) then exit;
  Rng:= Obj as TATRangeInCodeTree;

  ed.DoGotoPos(
    Rng.PosBegin,
    Point(-1, -1),
    5,
    5,
    true,
    true
    );
  ed.SetFocus;
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  if chkWrap.checked then
    ed.OptWrapMode:= cWrapOn
  else
    ed.OptWrapMode:= cWrapOff;
end;

procedure TfmMain.EditorChangeCaretPos(Sender: TObject);
begin
  if ed.Carets.Count>0 then
    with ed.Carets[0] do
      CodetreeSelectItemForPosition(Tree, PosX, PosY);
end;

procedure TfmMain.chkFullSelChange(Sender: TObject);
begin
  ed.OptShowFullWidthForSelection:= chkFullSel.Checked;
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
  ed.OptShowFullWidthForSyntaxHilite:= chkFullHilite.Checked;
  ed.Update;
end;

procedure TfmMain.bOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    Filename:= '';
    InitialDir:= FDirApp;
    if not Execute then exit;
    DoOpenFile(Filename);
  end;
end;

function TfmMain.GetComment: string;
var
  an: TecSyntAnalyzer;
begin
  Result:= '';
  an:= adapter.Lexer;
  if Assigned(an) then
    Result:= an.LineComment;
end;

procedure TfmMain.bExportClick(Sender: TObject);
var
  fn: string;
begin
  fn:=     GetTempDir+DirectorySeparator+'_export.html';
  DoEditorExportToHTML(Ed, fn, 'Export test',
    'Courier New', 12, false,
    clWhite, clMedGray);
  if FileExists(fn) then
    OpenDocument(fn);
end;

procedure TfmMain.AdapterParseDone(Sender: TObject);
begin
  adapter.TreeFill(Tree);
  EditorChangeCaretPos(Self);
end;

procedure TfmMain.AdapterParseBegin(Sender: TObject);
begin
  Tree.Items.Clear;
end;

procedure TfmMain.chkDynChange(Sender: TObject);
begin
  adapter.DynamicHiliteEnabled:= chkDyn.Checked;
  Ed.Update;
end;

procedure TfmMain.DoLexer(const aname: string);
var
  an: TecSyntAnalyzer;
begin
  an:= manager.FindAnalyzer(aname);
  if Assigned(an) then
  begin
    adapter.Lexer:= an;
    ed.DoEventChange(0);
    ed.Update;
  end;
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
  //while adapter.TreeBusy do Application.ProcessMessages;

  fn:= files.GetPathFromItem(files.Selected);
  if FileExists(fn) then
    DoOpenFile(fn);
end;

procedure TfmMain.EditClickGutter(Sender: TObject; ABand: integer; ALine: integer);
var
  NIndex: integer;
  Data: TATBookmarkData;
begin
  if ABand=ed.GutterBandBookmarks then
  begin
    NIndex:= ed.Strings.Bookmarks.Find(ALine);
    if NIndex>=0 then
      ed.Strings.Bookmarks.Delete(NIndex)
    else
    begin
      FillChar(Data, SizeOf(Data), 0);
      Data.Kind:= 1;
      Data.ShowInBookmarkList:= true;
      Data.LineNum:= ALine;
      ed.Strings.Bookmarks.Add(Data);
    end;
    ed.Update;
  end;
end;

procedure TfmMain.EditCalcStaple(Sender: TObject; ALine, AIndent: integer; var AColor: TColor);
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

