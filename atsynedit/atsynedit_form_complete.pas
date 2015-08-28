unit atsynedit_form_complete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LclProc, LclType,
  ATSynEdit,
  ATStringProc,
  ATListbox;

//AText is #13 separated strings, each str is '|' separated two strings:
//e.g. 'func|MyFunc1'+#13+'var|MyVar1'+#13+'var|MyVar2'
//result is item index or -1 if cancelled
function DoEditorCompletionDialog(Ed: TATSynEdit; const AText: string): integer;

type
  { TFormATSynEditComplete }

  TFormATSynEditComplete = class(TForm)
    List: TATListbox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
  private
    { private declarations }
    SList: TStringlist;
  public
    { public declarations }
  end;

var
  cCompleteColorFontPre: TColor = clPurple;
  cCompleteColorFontText: TColor = clBlack;
  cCompleteColorBg: TColor = $e0e0e0;
  cCompleteColorSelBg: TColor = clMedGray;
  cCompleteFontName: string = 'default';
  cCompleteFontSize: integer = 10;
  cCompleteBorderSize: integer = 4;
  cCompleteFormSizeX: integer = 400;
  cCompleteFormSizeY: integer = 180;
  cCompleteTextIndent1: integer = 4;
  cCompleteTextIndent2: integer = 8;
  cCompleteTextBold1: boolean = true;
  cCompleteTextBold2: boolean = false;

implementation

{$R *.lfm}

function DoEditorCompletionDialog(Ed: TATSynEdit; const AText: string): integer;
var
  P: TPoint;
begin
  Result:= -1;
  if Ed.Carets.Count=0 then exit;

  with TFormATSynEditComplete.Create(nil) do
  try
    SList.Text:= AText;
    if SList.Count=0 then exit;

    List.ItemCount:= SList.Count;
    List.ItemIndex:= 0;

    Color:= cCompleteColorBg;
    List.Color:= cCompleteColorBg;
    List.Font.Name:= cCompleteFontName;
    List.Font.Size:= cCompleteFontSize;
    List.ItemHeight:= Trunc(List.Font.Size*1.8);
    List.BorderSpacing.Around:= cCompleteBorderSize;

    P.X:= Ed.Carets[0].CoordX;
    P.Y:= Ed.Carets[0].CoordY+Ed.TextCharSize.Y;
    P:= Ed.ClientToScreen(P);
    SetBounds(P.X, P.Y, cCompleteFormSizeX, cCompleteFormSizeY);

    if ShowModal=mrOk then
      Result:= List.ItemIndex;
  finally
    Free
  end;
end;

{ TFormATSynEditComplete }

procedure TFormATSynEditComplete.FormCreate(Sender: TObject);
begin
  SList:= tstringlist.create;
end;

procedure TFormATSynEditComplete.FormDestroy(Sender: TObject);
begin
  SList.free;
end;

procedure TFormATSynEditComplete.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=vk_up) and (shift=[]) then
  begin
    if List.ItemIndex>0 then
      List.ItemIndex:= List.ItemIndex-1
    else
      List.ItemIndex:= List.ItemCount-1;
    key:= 0;
    exit
  end;

  if (key=vk_down) and (shift=[]) then
  begin
    if List.ItemIndex<List.ItemCount-1 then
      List.ItemIndex:= List.ItemIndex+1
    else
      List.ItemIndex:= 0;
    key:= 0;
    exit
  end;

  if (key=VK_ESCAPE) then
  begin
    Modalresult:= mrCancel;
    key:= 0;
    exit
  end;

  if (key=VK_RETURN) then
  begin
    Modalresult:= mrOk;
    key:= 0;
    exit
  end;
end;

procedure TFormATSynEditComplete.ListDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  S, S1, S2: string;
begin
  S:= SList[AIndex];
  S1:= SGetItem(S, '|');
  S2:= SGetItem(S, '|');

  if AIndex=List.ItemIndex then
    C.Brush.Color:= cCompleteColorSelBg
  else
    C.Brush.Color:= cCompleteColorBg;
  C.FillRect(ARect);
  C.Font.Assign(List.Font);

  if S2='' then
  begin
    if cCompleteTextBold2 then C.Font.Style:= [fsBold] else C.Font.Style:= [];
    C.Font.Color:= cCompleteColorFontText;
    C.TextOut(ARect.Left+cCompleteTextIndent1, ARect.Top, S1);
  end
  else
  begin
    if cCompleteTextBold1 then C.Font.Style:= [fsBold] else C.Font.Style:= [];
    C.Font.Color:= cCompleteColorFontPre;
    C.TextOut(ARect.Left+cCompleteTextIndent1, ARect.Top, S1);
    if cCompleteTextBold2 then C.Font.Style:= [fsBold] else C.Font.Style:= [];
    C.Font.Color:= cCompleteColorFontText;
    C.TextOut(ARect.Left+cCompleteTextIndent1+cCompleteTextIndent2+C.TextWidth(S1), ARect.Top, S2);
  end
end;

end.

