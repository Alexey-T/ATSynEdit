unit unmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ecSyntAnal,
  formlexerlib;

type
  { TfmMain }

  TfmMain = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

var
  Manager: TecSyntaxManager;

{ TfmMain }

procedure TfmMain.Button1Click(Sender: TObject);
begin
  DoShowDialogLexerLib(Manager, 'Courier new', 10);
  if Manager.Modified then
  begin
    Manager.Modified:= false;
    Showmessage('Lib was modified');
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFileDir(Application.ExeName)+DirectorySeparator+
    'lexlib'+DirectorySeparator+'small.lxl';
  Manager:= TecSyntaxManager.Create(Self);
  Manager.LoadFromFile(fn);
  Label1.Caption:= 'library "'+Extractfilename(fn)+'" has '+Inttostr(Manager.AnalyzerCount)+ ' lexers';
end;

end.

