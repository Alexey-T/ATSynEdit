unit atsynedit_form_complete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs,
  LclProc, LclType,
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_Commands,
  ATStringProc,
  ATListbox,
  Math;

type
  TATCompletionPropEvent = procedure (Sender: TObject;
    out AText, ASuffix: string; out ACharsLeft, ACharsRight: integer) of object;

//AText is #13-separated strings, each string is '|'-separated items.
//Usually item_0 is prefix to show,
//item_1 is actual text (result of function),
//item_2..etc are only to show.
//e.g. 'func|Func1|(param1, param2)'+#13+'var|Var1'+#13+'var|Var2'
//AChars: how many chars to replace before caret.

procedure DoEditorCompletionListbox(
  AOwner: TComponent; AEd: TATSynEdit;
  AOnGetProp: TATCompletionPropEvent);

procedure EditorGetCurrentWord(Ed: TATSynEdit; const AWordChars: atString;
  out AWord: atString; out ACharsLeft, ACharsRight: integer);

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
    FOnGetProp: TATCompletionPropEvent;
    FEdit: TATSynEdit;
    FCharsLeft,
    FCharsRight: integer;
    FSuffix: string;
    procedure DoReplaceTo(const Str: string);
    procedure DoResult;
    procedure DoUpdate;
    function GetItemText(S: string; AIndex: integer): string;
    function GetResultText: string;
  public
    { public declarations }
    property Editor: TATSynEdit read FEdit write FEdit;
    property OnGetProp: TATCompletionPropEvent read FOnGetProp write FOnGetProp;
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
  cCompleteIndexOfDesc: integer = 2;
  cCompleteSepChar: char = '|';
  cCompleteListSort: boolean = false;
  cCompleteKeyUpDownWrap: boolean = true;
  cCompleteInsertAlsoBracket: boolean = true;
  cCompleteFontName: string = 'default';
  cCompleteFontSize: integer = 10;
  cCompleteBorderSize: integer = 4;
  cCompleteFormSizeX: integer = 500;
  cCompleteFormSizeY: integer = 200;
  cCompleteTextIndent0: integer = 4;
  cCompleteTextIndent: integer = 8;

implementation

{$R *.lfm}

var
  FormComplete: TFormATSynEditComplete = nil;

procedure DoEditorCompletionListbox(AOwner: TComponent; AEd: TATSynEdit;
  AOnGetProp: TATCompletionPropEvent);
begin
  if AEd.ModeReadOnly then exit;
  if AEd.Carets.Count<>1 then exit;

  if FormComplete=nil then
    FormComplete:= TFormATSynEditComplete.Create(AOwner);

  FormComplete.Editor:= AEd;
  FormComplete.OnGetProp:= AOnGetProp;
  FormComplete.DoUpdate;
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

    FCharsLeft:= Min(Pos.X, FCharsLeft);
    Dec(Pos.X, FCharsLeft);
    Editor.Strings.TextDeleteRight(Pos.X, Pos.Y, FCharsLeft+FCharsRight, Shift, PosAfter, false);
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

  if (key=VK_RETURN) or (key=VK_TAB) then
  begin
    DoResult;
    key:= 0;
    exit
  end;

  if (key=VK_LEFT) and (shift=[]) then
  begin
    Editor.DoCommand(cCommand_KeyLeft, '');
    DoUpdate;
    key:= 0;
    exit
  end;

  if (key=VK_RIGHT) and (shift=[]) then
  begin
    Editor.DoCommand(cCommand_KeyRight, '');
    DoUpdate;
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
    DoUpdate;
    Utf8Key:= '';
    exit;
  end;

  //skip control Ascii chars
  if Ord(UTF8Key[1])<32 then Exit;

  Str:= Utf8Decode(Utf8Key);
  FEdit.DoCommand(cCommand_TextInsert, Str);
  DoUpdate;
  Utf8Key:= '';
end;

procedure TFormATSynEditComplete.ListClick(Sender: TObject);
begin
  DoResult;
end;

function TFormATSynEditComplete.GetItemText(S: string; AIndex: integer): string;
var
  i: integer;
begin
  for i:= 0 to AIndex do
    Result:= SGetItem(S, cCompleteSepChar);
end;

function TFormATSynEditComplete.GetResultText: string;
var
  SText, SDesc: string;
begin
  Result:= '';
  if List.ItemIndex>=0 then
  begin
    SText:= GetItemText(SList[List.ItemIndex], cCompleteIndexOfText);
    SDesc:= GetItemText(SList[List.ItemIndex], cCompleteIndexOfDesc);
    Result:= SText;

    if FSuffix<>'' then
      Result:= Result+FSuffix
    else
    if cCompleteInsertAlsoBracket then
      if SBegin(SDesc, '(') then
        Result:= Result+'(';
  end;
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

procedure TFormATSynEditComplete.DoUpdate;
var
  AText: string;
  P: TPoint;
begin
  if Assigned(FOnGetProp) then
    FOnGetProp(Editor, AText, FSuffix, FCharsLeft, FCharsRight);

  if (AText='') then
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

  P.X:= Editor.Carets[0].CoordX-Editor.TextCharSize.X*FCharsLeft;
  P.Y:= Editor.Carets[0].CoordY+Editor.TextCharSize.Y;
  P:= Editor.ClientToScreen(P);

  SetBounds(P.X, P.Y, cCompleteFormSizeX, cCompleteFormSizeY);
  Show;
end;


procedure EditorGetCurrentWord(Ed: TATSynEdit; const AWordChars: atString;
  out AWord: atString; out ACharsLeft, ACharsRight: integer);
var
  str: atString;
  n: integer;
begin
  AWord:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  str:= Ed.Strings.Lines[Ed.Carets[0].PosY];
  n:= Ed.Carets[0].PosX;
  if (n>Length(str)) then exit;

  while (n>0) and (IsCharWord(str[n], AWordChars)) do
  begin
    AWord:= str[n]+AWord;
    Dec(n);
    Inc(ACharsLeft);
  end;

  n:= Ed.Carets[0].PosX;
  while (n<Length(str)) and (IsCharWord(str[n+1], AWordChars)) do
  begin
    Inc(n);
    Inc(ACharsRight);
  end;
end;


end.

