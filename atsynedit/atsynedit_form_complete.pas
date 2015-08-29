unit atsynedit_form_complete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  LclProc, LclType,
  ATSynEdit,
  ATSynEdit_Carets,
  ATStringProc,
  ATListbox,
  Math;

//AText is #13-separated strings, each string is '|'-separated items.
//Usually item_0 is prefix to show,
//item_1 is actual text (result of function),
//item_2..etc are only to show.
//e.g. 'func|Func1|(param1, param2)'+#13+'var|Var1'+#13+'var|Var2'
//AChars: how many chars to replace before caret.

//result: text part selected.
function DoEditorCompletionDialogOnlySelect(Ed: TATSynEdit;
  const AText: string; AChars: integer): string;
//result: is item selected (text replaced).
function DoEditorCompletionDialogAndReplace(Ed: TATSynEdit;
  const AText: string; AChars: integer): boolean;

type
  { TFormATSynEditComplete }

  TFormATSynEditComplete = class(TForm)
    List: TATListbox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListClick(Sender: TObject);
    procedure ListDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
  private
    { private declarations }
    SList: TStringlist;
    function GetItemText(S: string): string;
  public
    { public declarations }
  end;

const
  cCompleteItemCount = 5;
var
  cCompleteColorFont: array[0..cCompleteItemCount-1] of TColor =
    (clPurple, clBlack, clNavy, clBlack, clBlack);
  cCompleteFontStyles: array[0..cCompleteItemCount-1] of TFontStyles =
    ([fsBold], [], [], [], []);
  cCompleteColorBg: TColor = $e0e0e0;
  cCompleteColorSelBg: TColor = clMedGray;

  cCompleteIndexOfText: integer = 1;
  cCompleteSepChar: char = '|';
  cCompleteListSort: boolean = false;
  cCompleteKeyUpDownWrap: boolean = true;
  cCompleteFontName: string = 'default';
  cCompleteFontSize: integer = 10;
  cCompleteBorderSize: integer = 4;
  cCompleteFormSizeX: integer = 450;
  cCompleteFormSizeY: integer = 200;
  cCompleteTextIndent0: integer = 4;
  cCompleteTextIndent: integer = 8;

implementation

{$R *.lfm}

function DoEditorCompletionDialogAndReplace(Ed: TATSynEdit;
  const AText: string; AChars: integer): boolean;
var
  Str: string;
  Pos, Shift, PosAfter: TPoint;
  Caret: TATCaretItem;
begin
  Result:= false;
  if Ed.ModeReadOnly then exit;
  if Ed.Carets.Count<>1 then exit;

  Str:= DoEditorCompletionDialogOnlySelect(Ed, AText, AChars);
  Result:= Str<>'';
  if Result then
  begin
    Caret:= Ed.Carets[0];
    Pos.X:= Caret.PosX;
    Pos.Y:= Caret.PosY;

    Ed.Strings.TextDeleteLeft(Pos.X, Pos.Y, AChars, Shift, PosAfter);
    Pos.X:= Max(0, Pos.X-AChars);
    Ed.Strings.TextInsert(Pos.X, Pos.Y, Str, false, Shift, PosAfter);

    Caret.PosX:= Pos.X+Length(Str);
    Caret.EndX:= -1;
    Caret.EndY:= -1;

    Ed.Update(true);
    Ed.DoEventChange;
  end;
end;

function DoEditorCompletionDialogOnlySelect(Ed: TATSynEdit; const AText: string;
  AChars: integer): string;
var
  P: TPoint;
begin
  Result:= '';
  if Ed.Carets.Count<>1 then exit;

  with TFormATSynEditComplete.Create(nil) do
  try
    SList.Text:= AText;
    if SList.Count=0 then exit;
    if cCompleteListSort then SList.Sort;

    List.ItemCount:= SList.Count;
    List.ItemIndex:= 0;

    Color:= cCompleteColorBg;
    List.Color:= cCompleteColorBg;
    List.Font.Name:= cCompleteFontName;
    List.Font.Size:= cCompleteFontSize;
    List.ItemHeight:= Trunc(List.Font.Size*1.8);
    List.BorderSpacing.Around:= cCompleteBorderSize;

    P.X:= Ed.Carets[0].CoordX-Ed.TextCharSize.X*AChars;
    P.Y:= Ed.Carets[0].CoordY+Ed.TextCharSize.Y;
    P:= Ed.ClientToScreen(P);
    SetBounds(P.X, P.Y, cCompleteFormSizeX, cCompleteFormSizeY);

    if ShowModal=mrOk then
      if List.ItemIndex>=0 then
        Result:= GetItemText(SList[List.ItemIndex]);
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
    if cCompleteKeyUpDownWrap then
      List.ItemIndex:= List.ItemCount-1;
    key:= 0;
    exit
  end;

  if (key=vk_down) and (shift=[]) then
  begin
    if List.ItemIndex<List.ItemCount-1 then
      List.ItemIndex:= List.ItemIndex+1
    else
    if cCompleteKeyUpDownWrap then
      List.ItemIndex:= 0;
    key:= 0;
    exit
  end;

  if (key=VK_PRIOR) and (shift=[]) then
  begin
    List.ItemIndex:= Max(0, List.ItemIndex-List.VisibleItems);
    key:= 0;
    exit
  end;

  if (key=VK_NEXT) and (shift=[]) then
  begin
    List.ItemIndex:= Min(List.Itemcount-1, List.ItemIndex+List.VisibleItems);
    key:= 0;
    exit
  end;

  if (key=vk_home) then
  begin
    List.ItemIndex:= 0;
    key:= 0;
    exit
  end;

  if (key=vk_end) then
  begin
    List.ItemIndex:= List.ItemCount-1;
    key:= 0;
    exit
  end;

  if (key=VK_ESCAPE) then
  begin
    Modalresult:= mrCancel;
    key:= 0;
    exit
  end;

  if (key=VK_RETURN) or (key=VK_SPACE) then
  begin
    Modalresult:= mrOk;
    key:= 0;
    exit
  end;
end;

procedure TFormATSynEditComplete.ListClick(Sender: TObject);
begin
  Modalresult:= mrOk;
end;

function TFormATSynEditComplete.GetItemText(S: string): string;
var
  i: integer;
begin
  for i:= 0 to cCompleteIndexOfText do
    Result:= SGetItem(S, cCompleteSepChar);
end;

procedure TFormATSynEditComplete.ListDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  Str, SItem: string;
  NSize, i: integer;
begin
  Str:= SList[AIndex];

  if AIndex=List.ItemIndex then
    C.Brush.Color:= cCompleteColorSelBg
  else
    C.Brush.Color:= cCompleteColorBg;
  C.FillRect(ARect);

  C.Font.Assign(List.Font);
  NSize:= cCompleteTextIndent0;

  for i:= 0 to cCompleteItemCount-1 do
  begin
    SItem:= SGetItem(Str, cCompleteSepChar);
    C.Font.Style:= cCompleteFontStyles[i];
    C.Font.Color:= cCompleteColorFont[i];
    C.TextOut(ARect.Left+NSize, ARect.Top, SItem);
    Inc(NSize, C.TextWidth(SItem)+cCompleteTextIndent);
  end;
end;

end.

