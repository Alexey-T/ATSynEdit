unit atsynedit_form_complete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  LclProc, LclType,
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_Commands,
  ATStringProc,
  ATListbox,
  Math;

type
  TATStringEvent = procedure (Sender: TObject; const Str: string) of object;
  TATGetCompletionPropEvent = procedure (Sender: TObject; out AText: string; out AChars: integer) of object;

//AText is #13-separated strings, each string is '|'-separated items.
//Usually item_0 is prefix to show,
//item_1 is actual text (result of function),
//item_2..etc are only to show.
//e.g. 'func|Func1|(param1, param2)'+#13+'var|Var1'+#13+'var|Var2'
//AChars: how many chars to replace before caret.

procedure DoEditorCompletionListbox(
  AOwner: TComponent; AEd: TATSynEdit;
  AOnGetProp: TATGetCompletionPropEvent);

type
  { TFormATSynEditComplete }

  TFormATSynEditComplete = class(TForm)
    List: TATListbox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ListClick(Sender: TObject);
    procedure ListDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
  private
    { private declarations }
    SList: TStringlist;
    FOnGetProp: TATGetCompletionPropEvent;
    FEdit: TATSynEdit;
    FChars: integer;
    procedure DoReplaceTo(const Str: string);
    procedure DoResult;
    procedure UpdateShow;
    function GetItemText(S: string): string;
    function GetResultText: string;
  public
    { public declarations }
    property Editor: TATSynEdit read FEdit write FEdit;
    property OnGetProp: TATGetCompletionPropEvent read FOnGetProp write FOnGetProp;
  end;

var
  FormComplete: TFormATSynEditComplete = nil;

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

procedure DoEditorCompletionListbox(AOwner: TComponent; AEd: TATSynEdit;
  AOnGetProp: TATGetCompletionPropEvent);
begin
  if AEd.ModeReadOnly then exit;
  if AEd.Carets.Count<>1 then exit;

  if FormComplete=nil then
    FormComplete:= TFormATSynEditComplete.Create(AOwner);

  FormComplete.Editor:= AEd;
  FormComplete.OnGetProp:= AOnGetProp;
  FormComplete.UpdateShow;
end;

procedure TFormATSynEditComplete.DoReplaceTo(const Str: string);
var
  Caret: TATCaretItem;
  Pos, Shift, PosAfter: TPoint;
begin
  if Str<>'' then
  begin
    Caret:= Editor.Carets[0];
    Pos.X:= Caret.PosX;
    Pos.Y:= Caret.PosY;

    Editor.Strings.TextDeleteLeft(Pos.X, Pos.Y, FChars, Shift, PosAfter);
    Pos.X:= Max(0, Pos.X-FChars);
    Editor.Strings.TextInsert(Pos.X, Pos.Y, Str, false, Shift, PosAfter);

    Caret.PosX:= Pos.X+Length(Str);
    Caret.EndX:= -1;
    Caret.EndY:= -1;

    Editor.Update(true);
    Editor.DoEventChange;
  end;
end;

{ TFormATSynEditComplete }

procedure TFormATSynEditComplete.FormCreate(Sender: TObject);
begin
  SList:= TStringList.Create;
end;

procedure TFormATSynEditComplete.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormATSynEditComplete.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FEdit) then
    FEdit.OptCaretStopUnfocused:= true;
  CloseAction:= caHide;
end;

procedure TFormATSynEditComplete.FormDestroy(Sender: TObject);
begin
  SList.Free;
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
    Close;
    key:= 0;
    exit
  end;

  if (key=VK_RETURN) {or (key=VK_SPACE)} then
  begin
    DoResult;
    key:= 0;
    exit
  end;
end;

procedure TFormATSynEditComplete.FormShow(Sender: TObject);
begin
  if Assigned(FEdit) then
    FEdit.OptCaretStopUnfocused:= false;
end;

procedure TFormATSynEditComplete.FormUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  Str: atString;
begin
  inherited;

  //backsp
  if (UTF8Key=#8) then
  begin
    FEdit.DoCommand(cCommand_KeyBackspace, '');
    UpdateShow;
    Utf8Key:= '';
    exit;
  end;

  //skip control Ascii chars
  if Ord(UTF8Key[1])<32 then Exit;

  Str:= Utf8Decode(Utf8Key);
  FEdit.DoCommand(cCommand_TextInsert, Str);
  UpdateShow;
  Utf8Key:= '';
end;

procedure TFormATSynEditComplete.ListClick(Sender: TObject);
begin
  DoResult;
end;

function TFormATSynEditComplete.GetItemText(S: string): string;
var
  i: integer;
begin
  for i:= 0 to cCompleteIndexOfText do
    Result:= SGetItem(S, cCompleteSepChar);
end;

function TFormATSynEditComplete.GetResultText: string;
begin
  Result:= '';
  if List.ItemIndex>=0 then
    Result:= GetItemText(SList[List.ItemIndex]);
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

procedure TFormATSynEditComplete.DoResult;
begin
  DoReplaceTo(GetResultText);
  Close;
end;

procedure TFormATSynEditComplete.UpdateShow;
var
  AText: string;
  P: TPoint;
begin
  if Assigned(FOnGetProp) then
    FOnGetProp(Editor, AText, FChars);

  if (AText='') or (FChars<=0) then
    begin Close; exit end;

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
  List.Invalidate;

  P.X:= Editor.Carets[0].CoordX-Editor.TextCharSize.X*FChars;
  P.Y:= Editor.Carets[0].CoordY+Editor.TextCharSize.Y;
  P:= Editor.ClientToScreen(P);

  SetBounds(P.X, P.Y, cCompleteFormSizeX, cCompleteFormSizeY);
  Show;
end;

end.

