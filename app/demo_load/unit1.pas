unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, atstrings, atsynedit, atstringproc;

type
  { TForm1 }
  TForm1 = class(TForm)
    bLoad: TButton;
    bGettext: TButton;
    edFN: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure bLoadClick(Sender: TObject);
    procedure bGettextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
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
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  bLoad.Click;
end;

procedure TForm1.bLoadClick(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFilePath(Application.Exename)+'..\..\test_files\'+edFN.Text;
  ed.LoadFromFile(fn);
  ed.SetFocus;
end;

procedure TForm1.bGettextClick(Sender: TObject);
begin
  ShowMessage(UTF8Encode(ed.Strings.TextAll));
end;


end.