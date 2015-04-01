unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ShellCtrls, atstrings, atsynedit, atstringproc, ComCtrls;

type
  { TForm1 }
  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ed:= TATSynEdit.Create(Self);
  ed.Parent:= Panel1;
  ed.Align:= alClient;
  ed.Font.Name:= 'Courier New';

  fDir:= ExtractFilePath(Application.Exename)+'..\..\test_files';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  List.Root:= fDir;
end;

procedure TForm1.ListClick(Sender: TObject);
var
  s: string;
begin
  s:= List.GetPathFromItem(List.Selected);
  if not FileExists(s) then Exit;

  Screen.Cursor:= crHourGlass;
  ed.LoadFromFile(s);
  Screen.Cursor:= crDefault;

  ed.SetFocus;
  Caption:= 'App - '+ExtractFileName(s);
  Beep;
end;

procedure TForm1.bGettextClick(Sender: TObject);
begin
  ShowMessage(UTF8Encode(ed.Strings.TextAll));
end;


end.
