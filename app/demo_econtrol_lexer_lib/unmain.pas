unit unmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LclIntf, LclProc, LclType,
  ecSyntAnal,
  formlexerlib;

type
  { TfmMain }

  TfmMain = class(TForm)
    bShow: TButton;
    Label1: TLabel;
    procedure bShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdStatus;
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

procedure TfmMain.bShowClick(Sender: TObject);
var
  dirAcp: string;
begin
  dirAcp:= ExtractFileDir(Application.ExeName)+DirectorySeparator+'acp';
  CreateDir(dirAcp);

  DoShowDialogLexerLib(Manager, dirAcp, 'Courier new', 9);
  if Manager.Modified then
  begin
    UpdStatus;
    Manager.Modified:= false;
    if Application.MessageBox('Lib was modified. Save file?', 'Demo',
      MB_OKCANCEL or MB_ICONQUESTION)=id_ok then
      Manager.SaveToFile(Manager.FileName);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFileDir(ExtractFileDir(Application.ExeName))+DirectorySeparator+
    'lexlib'+DirectorySeparator+'lib.lxl';

  Manager:= TecSyntaxManager.Create(Self);
  Manager.LoadFromFile(fn);
  UpdStatus;
end;

procedure TfmMain.UpdStatus;
begin
  Label1.Caption:= Format('library "%s" has %d lexers',
    [Extractfilename(Manager.FileName), Manager.AnalyzerCount]);
end;

end.

