unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  ATSynEdit,
  ATStringProc,
  ATSynEdit_Adapter_EControl;

type
  { TfmMain }

  TfmMain = class(TForm)
    chkWrap: TCheckBox;
    Panel1: TPanel;
    procedure chkWrapChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

var
  adapter: TATSynEdit_Adapter_EControl;

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
var
  fname, fname_lxl: string;
const
  lexername = 'Pascal Script';
begin
  fname:= ExtractFilePath(Application.ExeName)+'test.pas';
  fname_lxl:= ExtractFilePath(Application.ExeName)+'lexlib.lxl';

  ed:= TATSynEdit.Create(Self);
  ed.Font.Name:= 'Consolas';
  ed.Parent:= Self;
  ed.Align:= alClient;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;
  ed.Colors.TextBG:= $e0f0f0;

  adapter:= TATSynEdit_Adapter_EControl.Create(fname_lxl);
  adapter.InitLexer(lexername);
  ed.AdapterOfHilite:= adapter;

  ed.LoadFromFile(fname);
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  if chkWrap.checked then
    ed.OptWrapMode:= cWrapOn
  else
    ed.OptWrapMode:= cWrapOff;
end;

end.

