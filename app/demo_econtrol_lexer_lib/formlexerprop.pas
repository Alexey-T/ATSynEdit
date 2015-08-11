unit formlexerprop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, Forms, Controls, StdCtrls,
  Dialogs, ButtonPanel, ComCtrls,
  ecSyntAnal,
  ATSynEdit,
  ATSynEdit_Adapter_EControl;

type
  { TfmLexerProp }

  TfmLexerProp = class(TForm)
    ButtonPanel1: TButtonPanel;
    edExt: TEdit;
    edLineCmt: TEdit;
    edName: TEdit;
    edSample: TATSynEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edNotes: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Adapter: TATAdapterEControl;
  end;

var
  fmLexerProp: TfmLexerProp;

function DoShowDialogLexerProp(an: TecSyntAnalyzer;
  AFontName: string; AFontSize: integer): boolean;

implementation

{$R *.lfm}

{ TfmLexerProp }

procedure TfmLexerProp.FormCreate(Sender: TObject);
begin
  Adapter:= TATAdapterEControl.Create;
  edSample.AdapterHilite:= Adapter;
end;

procedure TfmLexerProp.FormDestroy(Sender: TObject);
begin
  edSample.AdapterHilite:= nil;
  FreeAndNil(Adapter);
end;

function DoShowDialogLexerProp(an: TecSyntAnalyzer; AFontName: string;
  AFontSize: integer): boolean;
var
  F: TfmLexerProp;
begin
  Result:= false;
  if an=nil then exit;
  F:= TfmLexerProp.Create(nil);
  try
    F.edName.Text:= an.LexerName;
    F.edExt.Text:= an.Extentions;
    F.edLineCmt.Text:= an.LineComment;
    F.edNotes.Lines.AddStrings(an.Notes);

    F.edSample.Font.Name:= AFontName;
    F.edSample.Font.Size:= AFontSize;
    F.edSample.Gutter[F.edSample.GutterBandBm].Visible:= false;
    F.edSample.Gutter[F.edSample.GutterBandNum].Visible:= false;
    F.Adapter.Lexer:= an;
    if Assigned(an.SampleText) then
    begin
      F.edSample.Strings.LoadFromString(an.SampleText.Text);
      F.edSample.Update(true);
      F.edSample.DoEventChange;
    end;

    if F.ShowModal<>mrOk then exit;
    if Trim(F.edName.Text)='' then exit;
    Result:= true;

    an.LexerName:= F.edName.Text;
    an.Extentions:= F.edExt.Text;
    an.LineComment:= F.edLineCmt.Text;
    an.Notes.Clear;
    an.Notes.AddStrings(F.edNotes.Lines);
  finally
    F.Free;
  end;
end;


end.

