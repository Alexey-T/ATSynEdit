unit unmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LclIntf, LclProc, LclType,
  ec_SyntAnal,
  formlexerlib;

type
  { TfmMain }

  TfmMain = class(TForm)
    bShow: TButton;
    Label1: TLabel;
    procedure bShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDirLib: string;
    procedure UpdateStatus;
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
begin
  DoShowDialogLexerLib(Manager, FDirLib, 'Courier new', 9);
  if Manager.Modified then
  begin
    UpdateStatus;
    Manager.Modified:= false;
    if Application.MessageBox('Library was modified. Save file?',
      PChar(Caption), MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
      Manager.SaveToFile(Manager.FileName);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  fn: string;
begin
  FDirLib:= ExtractFileDir(ExtractFileDir(Application.ExeName))+DirectorySeparator+
    'lexlib';
  fn:= FDirLib+DirectorySeparator+'lib.lxl';

  Manager:= TecSyntaxManager.Create(Self);
  Manager.LoadFromFile(fn);
  UpdateStatus;
end;

procedure TfmMain.UpdateStatus;
begin
  Label1.Caption:= Format('library "%s" has %d lexers',
    [Extractfilename(Manager.FileName), Manager.AnalyzerCount]);
end;

end.

