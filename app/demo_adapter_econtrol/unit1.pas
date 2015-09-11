unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ShellCtrls, ComCtrls,
  ATSynEdit,
  ATStringProc,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Carets,
  ecSyntAnal,
  proc_lexer;

type
  { TfmMain }

  TfmMain = class(TForm)
    bOpen: TButton;
    bComment: TButton;
    bUncomment: TButton;
    chkDyn: TCheckBox;
    chkFullHilite: TCheckBox;
    chkFullSel: TCheckBox;
    chkLexer: TCheckBox;
    chkShowCur: TCheckBox;
    chkUnpri: TCheckBox;
    chkWrap: TCheckBox;
    edLexer: TComboBox;
    files: TShellListView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PanelText: TPanel;
    Splitter1: TSplitter;
    Tree: TTreeView;
    procedure AdapterParseBegin(Sender: TObject);
    procedure AdapterParseDone(Sender: TObject);
    procedure bCommentClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bUncommentClick(Sender: TObject);
    procedure chkDynChange(Sender: TObject);
    procedure chkFullHiliteChange(Sender: TObject);
    procedure chkFullSelChange(Sender: TObject);
    procedure chkLexerChange(Sender: TObject);
    procedure chkShowCurChange(Sender: TObject);
    procedure chkUnpriChange(Sender: TObject);
    procedure chkWrapChange(Sender: TObject);
    procedure EditoChangeCaretPos(Sender: TObject);
    procedure edLexerChange(Sender: TObject);
    procedure filesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeClick(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
    FDir: string;
    FFilename: string;
    procedure DoCommentAct(Act: TATCommentAction);
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
    edLexer.ItemIndex:= edLexer.Items.IndexOf(an.LexerName);
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fname_lxl: string;
begin
  FDir:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+'/test_syntax_files/';
  fname_lxl:= ExtractFilePath(Application.ExeName)+'lib.lxl';

  manager:= TecSyntaxManager.Create(Self);
  manager.LoadFromFile(fname_lxl);
  UpdateLexList;

  ed:= TATSynEdit.Create(Self);
  ed.Font.Name:= 'Courier New';
  ed.Parent:= PanelText;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.Colors.TextBG:= $e0f0f0;
  ed.Colors.CurrentLineBG:= clTeal;

  ed.Gutter[ed.GutterBandNum].Visible:= false;
  ed.Gutter.Update;

  ed.OnClickGutter:= @EditClickGutter;
  ed.OnCalcStaple:= @EditCalcStaple;
  ed.OnChangeCaretPos:=@EditoChangeCaretPos;

  adapter:= TATAdapterEControl.Create(Self);
  adapter.OnParseBegin:=@AdapterParseBegin;
  adapter.OnParseDone:=@AdapterParseDone;
  ed.AdapterHilite:= adapter;

  chkWrap.Checked:= ed.OptWrapMode=cWrapOn;
  chkFullSel.Checked:= ed.OptShowFullSel;
  chkFullHilite.Checked:= ed.OptShowFullHilite;
  chkUnpri.Checked:= ed.OptUnprintedVisible;
  chkShowCur.Checked:= ed.OptShowCurLine;
  chkDyn.Checked:= adapter.DynamicHiliteEnabled;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  if DirectoryExists(FDir) then
    files.Root:= FDir;
end;

procedure TfmMain.TreeClick(Sender: TObject);
var
  R: TecTextRange;
  P: TPoint;
begin
  if adapter.IsBusy then exit;
  if Tree.Selected=nil then exit;
  if Tree.Selected.Data=nil then exit;

  R:= TecTextRange(Tree.Selected.Data);
  P:= adapter.GetPositionOfRange(R);

  ed.DoGotoPosEx(P);
  ed.SetFocus;
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  if chkWrap.checked then
    ed.OptWrapMode:= cWrapOn
  else
    ed.OptWrapMode:= cWrapOff;
end;

procedure TfmMain.EditoChangeCaretPos(Sender: TObject);
begin
  adapter.TreeShowItemForCaret(Tree, Point(ed.Carets[0].PosX, ed.Carets[0].PosY));
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

procedure TfmMain.DoCommentAct(Act: TATCommentAction);
var
  Str: string;
begin
  Str:= GetComment;
  if Str='' then
    Showmessage('No line comment defined for lexer')
  else
    Ed.DoCommentSelectionLines(Act, Str);
end;

procedure TfmMain.bCommentClick(Sender: TObject);
begin
  DoCommentAct(cCommentAddIfNone);
end;

procedure TfmMain.AdapterParseDone(Sender: TObject);
begin
  adapter.TreeFill(Tree);
end;

procedure TfmMain.AdapterParseBegin(Sender: TObject);
begin
  Tree.Items.Clear;
end;

procedure TfmMain.bUncommentClick(Sender: TObject);
begin
  DoCommentAct(cCommentRemove);
end;

procedure TfmMain.chkDynChange(Sender: TObject);
begin
  adapter.DynamicHiliteEnabled:= chkDyn.Checked;
  Ed.Update;
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
  //while adapter.IsBusy do Application.ProcessMessages;

  fn:= files.GetPathFromItem(files.Selected);
  if FileExistsUTF8(fn) then
    DoOpenFile(fn);
end;

procedure TfmMain.EditClickGutter(Sender: TObject; ABand: integer; ALine: integer);
begin
  if ABand=ed.GutterBandBm then
  begin
    if ed.Strings.LinesBm[ALine]<>0 then
      ed.Strings.LinesBm[ALine]:= 0
    else
      ed.Strings.LinesBm[ALine]:= 1;
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

