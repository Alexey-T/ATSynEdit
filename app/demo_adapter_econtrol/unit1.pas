unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  ATSynEdit,
  ATStringProc,
  ATSynEdit_Adapter_EControl,
  ecSyntAnal;

type
  { TfmMain }

  TfmMain = class(TForm)
    chkWrap: TCheckBox;
    comboLexer: TComboBox;
    Panel1: TPanel;
    procedure chkWrapChange(Sender: TObject);
    procedure comboLexerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
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
  adapter: TATSynEdit_Adapter_EControl;

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
    comboLexer.Items.AddStrings(sl);
  finally
    sl.free;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
const
  lexername = 'Pascal Script';
var
  fname, fname_lxl: string;
begin
  fname:= ExtractFilePath(Application.ExeName)+'test.pas';
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

  adapter:= TATSynEdit_Adapter_EControl.Create;
  ed.AdapterOfHilite:= adapter;

  ed.LoadFromFile(fname);
  comboLexer.ItemIndex:= comboLexer.Items.IndexOf(lexername);
  comboLexerChange(nil);
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  if chkWrap.checked then
    ed.OptWrapMode:= cWrapOn
  else
    ed.OptWrapMode:= cWrapOff;
end;

procedure TfmMain.comboLexerChange(Sender: TObject);
begin
  adapter.InitLexer(manager, comboLexer.Text);
  ed.Update;
end;

end.

