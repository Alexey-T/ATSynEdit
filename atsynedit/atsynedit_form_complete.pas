unit atsynedit_form_complete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LclProc, LclType,
  ATSynEdit,
  ATSynEdit_Carets,
  ATStringProc,
  ATListbox,
  math;

//AText is #13 separated strings, each str a) name, or b) id+'|'+name.
//e.g. 'func|MyFunc1'+#13+'var|MyVar1'+#13+'var|MyVar2'
//ACharsReplace: how many chars replace in editor before caret.
//result is name or ''.
function DoEditorCompletionDialog(Ed: TATSynEdit;
  const AText: string; ACharsIndent: integer): string;
function DoEditorCompletionDialogAndReplace(Ed: TATSynEdit;
  const AText: string; ACharsReplace: integer): boolean;

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
    procedure GetItems(Str: string; out StrPre, StrText: string);
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

function DoEditorCompletionDialogAndReplace(Ed: TATSynEdit;
  const AText: string; ACharsReplace: integer): boolean;
var
  Str: string;
  Pos, Shift, PosAfter: TPoint;
  Caret: TATCaretItem;
begin
  Result:= false;
  if Ed.ModeReadOnly then exit;
  if Ed.Carets.Count<>1 then exit;

  Str:= DoEditorCompletionDialog(Ed, AText, ACharsReplace);
  Result:= Str<>'';
  if Result then
  begin
    Caret:= Ed.Carets[0];
    Pos.X:= Caret.PosX;
    Pos.Y:= Caret.PosY;

    Ed.Strings.TextDeleteLeft(Pos.X, Pos.Y, ACharsReplace, Shift, PosAfter);
    Pos.X:= Max(0, Pos.X-ACharsReplace);
    Ed.Strings.TextInsert(Pos.X, Pos.Y, Str, false, Shift, PosAfter);

    Caret.PosX:= Pos.X+Length(Str);
    Caret.EndX:= -1;
    Caret.EndY:= -1;

    Ed.Update(true);
    Ed.DoEventChange;
  end;
end;

function DoEditorCompletionDialog(Ed: TATSynEdit; const AText: string;
  ACharsIndent: integer): string;
var
  P: TPoint;
  SPre, SText: string;
begin
  Result:= '';
  if Ed.Carets.Count<>1 then exit;

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

    P.X:= Ed.Carets[0].CoordX-Ed.TextCharSize.X*ACharsIndent;
    P.Y:= Ed.Carets[0].CoordY+Ed.TextCharSize.Y;
    P:= Ed.ClientToScreen(P);
    SetBounds(P.X, P.Y, cCompleteFormSizeX, cCompleteFormSizeY);

    if ShowModal=mrOk then
      if List.ItemIndex>=0 then
      begin
        GetItems(SList[List.ItemIndex], SPre, SText);
        Result:= SText;
      end;
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

procedure TFormATSynEditComplete.GetItems(Str: string; out StrPre, StrText: string);
begin
  StrPre:= SGetItem(Str, '|');
  StrText:= SGetItem(Str, '|');
  if StrText='' then
  begin
    StrText:= StrPre;
    StrPre:= '';
  end;
end;

procedure TFormATSynEditComplete.ListDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  SPre, SText: string;
begin
  GetItems(SList[AIndex], SPre, SText);

  if AIndex=List.ItemIndex then
    C.Brush.Color:= cCompleteColorSelBg
  else
    C.Brush.Color:= cCompleteColorBg;
  C.FillRect(ARect);
  C.Font.Assign(List.Font);

  if SPre='' then
  begin
    if cCompleteTextBold2 then C.Font.Style:= [fsBold] else C.Font.Style:= [];
    C.Font.Color:= cCompleteColorFontText;
    C.TextOut(ARect.Left+cCompleteTextIndent1, ARect.Top, SText);
  end
  else
  begin
    if cCompleteTextBold1 then C.Font.Style:= [fsBold] else C.Font.Style:= [];
    C.Font.Color:= cCompleteColorFontPre;
    C.TextOut(ARect.Left+cCompleteTextIndent1, ARect.Top, SPre);
    if cCompleteTextBold2 then C.Font.Style:= [fsBold] else C.Font.Style:= [];
    C.Font.Color:= cCompleteColorFontText;
    C.TextOut(ARect.Left+
      cCompleteTextIndent1+cCompleteTextIndent2+C.TextWidth(SPre),
      ARect.Top, SText);
  end
end;

end.

