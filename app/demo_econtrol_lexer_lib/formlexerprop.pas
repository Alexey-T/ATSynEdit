unit formlexerprop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, Forms, Controls, StdCtrls,
  Dialogs, ButtonPanel, ComCtrls, ExtCtrls, ColorBox,
  ecSyntAnal,
  ATSynEdit,
  ATSynEdit_Adapter_EControl;

type
  { TfmLexerProp }

  TfmLexerProp = class(TForm)
    bApplyStl: TButton;
    ButtonPanel1: TButtonPanel;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkStrik: TCheckBox;
    chkUnder: TCheckBox;
    edColorFont: TColorBox;
    edColorBG: TColorBox;
    edStyleType: TComboBox;
    edExt: TEdit;
    edLineCmt: TEdit;
    edName: TEdit;
    edSample: TATSynEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edNotes: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ListStyles: TListBox;
    chkBorderT: TPageControl;
    Panel1: TPanel;
    TabSheetGen: TTabSheet;
    TabSheetNotes: TTabSheet;
    TabSheetStyles: TTabSheet;
    procedure bApplyStlClick(Sender: TObject);
    procedure edStyleTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListStylesClick(Sender: TObject);
  private
    { private declarations }
    FAn: TecSyntAnalyzer;
    procedure UpdateStl;
    procedure UpdateStlEn(fmt: TecFormatType);
    procedure UpdateStlFromList;
    procedure UpdateStlToList;
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

procedure TfmLexerProp.bApplyStlClick(Sender: TObject);
begin
  UpdateStlToList;
end;

procedure TfmLexerProp.edStyleTypeChange(Sender: TObject);
begin
  UpdateStlEn(TecFormatType(edStyleType.ItemIndex));
end;

procedure TfmLexerProp.FormDestroy(Sender: TObject);
begin
  edSample.AdapterHilite:= nil;
  FreeAndNil(Adapter);
end;

procedure TfmLexerProp.ListStylesClick(Sender: TObject);
begin
  UpdateStlFromList;
end;

procedure TfmLexerProp.UpdateStl;
var
  i: integer;
  fmt: TecSyntaxFormat;
begin
  ListStyles.Items.Clear;
  for i:= 0 to FAn.Formats.Count-1 do
  begin
    fmt:= FAn.Formats[i];
    ListStyles.Items.Add(fmt.DisplayName);
  end;

  if ListStyles.Count>0 then
    ListStyles.ItemIndex:= 0;
  UpdateStlFromList;
end;



procedure TfmLexerProp.UpdateStlFromList;
var
  n: integer;
  fmt: TecSyntaxFormat;
begin
  n:= ListStyles.ItemIndex;
  if n<0 then exit;
  fmt:= FAn.Formats[n];

  edStyleType.ItemIndex:= Ord(fmt.FormatType);
  edColorFont.Selected:= fmt.Font.Color;
  edColorBG.Selected:= fmt.BgColor;

  chkBold.Checked:= fsBold in fmt.Font.Style;
  chkItalic.Checked:= fsItalic in fmt.Font.Style;
  chkUnder.Checked:= fsUnderline in fmt.Font.Style;
  chkStrik.Checked:= fsStrikeOut in fmt.Font.Style;

  UpdateStlEn(fmt.FormatType);
end;

procedure TfmLexerProp.UpdateStlEn(fmt: TecFormatType);
begin
  edColorFont.Enabled:= fmt in [ftCustomFont, ftFontAttr, ftColor];
  edColorBG.Enabled:= true;

  chkBold.Enabled:= fmt in [ftCustomFont, ftFontAttr];
  chkItalic.Enabled:= chkBold.Enabled;
  chkUnder.Enabled:= chkBold.Enabled;
  chkStrik.Enabled:= chkBold.Enabled;
end;

procedure TfmLexerProp.UpdateStlToList;
var
  n: integer;
  fmt: TecSyntaxFormat;
  fs: TFontStyles;
begin
  n:= ListStyles.ItemIndex;
  if n<0 then exit;
  fmt:= FAn.Formats[n];

  fmt.FormatType:= TecFormatType(edStyleType.ItemIndex);
  fmt.Font.Color:= edColorFont.Selected;
  fmt.BgColor:= edColorBG.Selected;

  fs:= [];
  if chkBold.Checked then Include(fs, fsBold);
  if chkItalic.Checked then Include(fs, fsItalic);
  if chkUnder.Checked then Include(fs, fsUnderline);
  if chkStrik.Checked then Include(fs, fsStrikeOut);
  fmt.Font.Style:= fs;
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
    F.FAn:= an;
    F.edName.Text:= an.LexerName;
    F.edExt.Text:= an.Extentions;
    F.edLineCmt.Text:= an.LineComment;
    F.edNotes.Lines.AddStrings(an.Notes);
    F.UpdateStl;

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

