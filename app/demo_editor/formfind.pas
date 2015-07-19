unit formfind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls;

type

  { TfmFind }

  TfmFind = class(TForm)
    bCount: TButton;
    bFind: TButton;
    bCancel: TButton;
    bRep: TButton;
    bRepAll: TButton;
    chkFromCaret: TCheckBox;
    chkRep: TCheckBox;
    chkRegex: TCheckBox;
    chkBack: TCheckBox;
    chkCase: TCheckBox;
    chkWords: TCheckBox;
    edFind: TEdit;
    edRep: TEdit;
    Label1: TLabel;
    procedure chkRegexChange(Sender: TObject);
    procedure chkRepChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Update;
    { private declarations }
  public
    { public declarations }
  end;

var
  fmFind: TfmFind;

implementation

{$R *.lfm}

{ TfmFind }

procedure TfmFind.chkRegexChange(Sender: TObject);
begin
  Update;
end;

procedure TfmFind.chkRepChange(Sender: TObject);
begin
  Update;
end;

procedure TfmFind.FormShow(Sender: TObject);
begin
  Update;
end;

procedure TfmFInd.Update;
begin
  chkWords.Enabled:= not chkRegex.Checked;
  chkBack.Enabled:= not chkRegex.Checked;
  edRep.Enabled:= chkRep.Checked;
  bFind.Visible:= not chkRep.Checked;
  bRep.Visible:= chkRep.Checked;
  bRepAll.Visible:= chkRep.Checked;

  if chkRep.Checked then Caption:= 'Replace' else Caption:= 'Find';
  if chkRep.Checked then bRep.Default:= true else bFind.Default:= true;
end;

end.

