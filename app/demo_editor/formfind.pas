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
    chkConfirm: TCheckBox;
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
var
  rep: boolean;
begin
  rep:= chkRep.Checked;

  chkWords.Enabled:= not chkRegex.Checked;
  chkBack.Enabled:= not chkRegex.Checked;
  chkConfirm.Enabled:= rep;
  edRep.Enabled:= rep;
  bFind.Visible:= not rep;
  bRep.Visible:= rep;
  bRepAll.Visible:= rep;

  if rep then Caption:= 'Replace' else Caption:= 'Find';
  if rep then bRep.Default:= true else bFind.Default:= true;
end;

end.

