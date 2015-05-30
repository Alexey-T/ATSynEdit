unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ShellCtrls, atstrings, atsynedit, atstringproc;

type
  { TfmMain }
  TfmMain = class(TForm)
    bGettext: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    List: TShellListView;
    procedure bGettextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListClick(Sender: TObject);
  private
    { private declarations }
    fDir: string;
    ed: TATSynEdit;
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ed:= TATSynEdit.Create(Self);
  ed.Parent:= Panel1;
  ed.Align:= alClient;
  ed.Font.Name:= 'Courier New';

  fDir:= ExtractFilePath(Application.Exename)+'..\..\test_files';
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  List.Root:= fDir;
end;

procedure TfmMain.ListClick(Sender: TObject);
var
  s: string;
begin
  s:= List.GetPathFromItem(List.Selected);
  if not FileExistsUTF8(s) then Exit;

  ed.BeginUpdate;
  Application.ProcessMessages;
  ed.LoadFromFile(s);
  ed.EndUpdate;

  ed.SetFocus;
  Caption:= 'App - '+ExtractFileName(s);
end;

procedure TfmMain.bGettextClick(Sender: TObject);
begin
  ShowMessage(UTF8Encode(ed.Strings.TextString));
end;


end.
