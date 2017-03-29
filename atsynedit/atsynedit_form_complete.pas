{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Form_Complete;

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
  {$ifdef complete_at_scroll}
  proc_scrollbars,
  {$endif}
  Math;

type
  TATCompletionPropEvent = procedure (Sender: TObject;
    out AText: string; out ACharsLeft, ACharsRight: integer) of object;
  TATCompletionResultEvent = procedure (Sender: TObject;
    const ASnippetId: string; ASnippetIndex: integer) of object;

//AText is #13-separated strings, each string is '|'-separated items.
//Usually item_0 is prefix to show,
//        item_1 is actual text (inserted on Enter),
//        item_2..etc are only to show.
//e.g. 'func|Func1|(param1, param2)'+#13+'var|Var1'+#13+'var|Var2'
//Item for text can have suffixes after #1: text+#1+suffix_before_caret+#1+suffix_after_caret

procedure DoEditorCompletionListbox(AEd: TATSynEdit;
  AOnGetProp: TATCompletionPropEvent;
  AOnResult: TATCompletionResultEvent = nil;
  const ASnippetId: string = '';
  ASelectedIndex: integer = 0);

procedure EditorGetCurrentWord(Ed: TATSynEdit; const AWordChars: atString;
  out AWord: atString; out ACharsLeft, ACharsRight: integer);

{$ifdef complete_at_scroll}
type
  TATListbox = type TATListboxMy;
{$endif}

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
    FOnResult: TATCompletionResultEvent;
    FEdit: TATSynEdit;
    FCharsLeft,
    FCharsRight: integer;
    FHintWnd: THintWindow;
    FSnippetId: string;
    FSelectedIndex: integer;
    procedure DoHintHide;
    procedure DoHintShow(const AHint: string);
    procedure DoReplaceTo(AStr: string);
    procedure DoResult;
    procedure DoUpdate;
    function GetItemText(S: string; AIndex: integer): string;
    function GetResultText: string;
  public
    { public declarations }
    property Editor: TATSynEdit read FEdit write FEdit;
    property OnGetProp: TATCompletionPropEvent read FOnGetProp write FOnGetProp;
    property OnResult: TATCompletionResultEvent read FOnResult write FOnResult;
    property SnippetId: string read FSnippetId write FSnippetId;
    property SelectedIndex: integer read FSelectedIndex write FSelectedIndex;
  end;

const
  cCompletionColumnCount = 5;

type
  TATCompletionOptions = record
    ColorFont: array[0..cCompletionColumnCount-1] of TColor;
    ColorBg: TColor;
    ColorSelBg: TColor;
    FontStyles: array[0..cCompletionColumnCount-1] of TFontStyles;
    FontName: string;
    FontSize: integer;
    CharsCloseListbox: string;
    IndexOfText: integer;
    IndexOfDesc: integer;
    SepChar: char;
    HintChar: char;
    SuffixChar: char;
    ListSort: boolean;
    KeyUpDownWrap: boolean;
    ItemHeight: integer;
    BorderSize: integer;
    FormSizeX: integer;
    FormSizeY: integer;
    HintSizeX: integer;
    TextIndent0: integer;
    TextIndent: integer;
  end;

var
  CompletionOps: TATCompletionOptions;

implementation

{$R *.lfm}

var
  FormComplete: TFormATSynEditComplete = nil;

procedure DoEditorCompletionListbox(AEd: TATSynEdit;
  AOnGetProp: TATCompletionPropEvent;
  AOnResult: TATCompletionResultEvent = nil;
  const ASnippetId: string = '';
  ASelectedIndex: integer = 0);
begin
  if AEd.ModeReadOnly then exit;
  if AEd.Carets.Count<>1 then exit;

  if FormComplete=nil then
    FormComplete:= TFormATSynEditComplete.Create(nil);

  FormComplete.Editor:= AEd;
  FormComplete.SelectedIndex:= ASelectedIndex;
  FormComplete.SnippetId:= ASnippetId;
  FormComplete.OnGetProp:= AOnGetProp;
  FormComplete.OnResult:= AOnResult;
  FormComplete.DoUpdate;
end;

procedure TFormATSynEditComplete.DoReplaceTo(AStr: string);
var
  Caret: TATCaretItem;
  Pos, Shift, PosAfter: TPoint;
  StrText, Str1, Str2: atString;
begin
  if AStr='' then exit;
  StrText:= Utf8Decode(SGetItem(AStr, CompletionOps.SuffixChar));
  Str1:= Utf8Decode(SGetItem(AStr, CompletionOps.SuffixChar));
  Str2:= Utf8Decode(SGetItem(AStr, CompletionOps.SuffixChar));

  begin
    Caret:= Editor.Carets[0];
    Pos.X:= Caret.PosX;
    Pos.Y:= Caret.PosY;

    FCharsLeft:= Min(Pos.X, FCharsLeft);
    Dec(Pos.X, FCharsLeft);

    Editor.Strings.BeginUndoGroup;
    try
      Editor.Strings.TextDeleteRight(Pos.X, Pos.Y, FCharsLeft+FCharsRight, Shift, PosAfter, false);
      Editor.Strings.TextInsert(Pos.X, Pos.Y, StrText+Str1+Str2, false, Shift, PosAfter);
    finally
      Editor.Strings.EndUndoGroup;
    end;

    Caret.PosX:= Pos.X+Length(StrText)+Length(Str1);
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
  FHintWnd:= THintWindow.Create(Self);
end;

procedure TFormATSynEditComplete.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormATSynEditComplete.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  DoHintHide;
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
    if CompletionOps.KeyUpDownWrap then
      List.ItemIndex:= List.ItemCount-1;
    key:= 0;
    exit
  end;

  if (key=vk_down) and (shift=[]) then
  begin
    if List.ItemIndex<List.ItemCount-1 then
      List.ItemIndex:= List.ItemIndex+1
    else
    if CompletionOps.KeyUpDownWrap then
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

  if (FSelectedIndex>=0) and (FSelectedIndex<List.ItemCount) then
    List.ItemIndex:= FSelectedIndex;
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
    UTF8Key:= '';
    exit;
  end;

  //skip control Ascii chars
  if Ord(UTF8Key[1])<32 then Exit;

  Str:= UTF8Decode(UTF8Key);
  FEdit.DoCommand(cCommand_TextInsert, Str);
  DoUpdate;

  if Pos(UTF8Key, CompletionOps.CharsCloseListbox)>0 then
    Close;
  UTF8Key:= '';
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
    Result:= SGetItem(S, CompletionOps.SepChar);
end;

function TFormATSynEditComplete.GetResultText: string;
var
  SText, SDesc: string;
begin
  Result:= '';
  if List.ItemIndex>=0 then
  begin
    SText:= GetItemText(SList[List.ItemIndex], CompletionOps.IndexOfText);
    SDesc:= GetItemText(SList[List.ItemIndex], CompletionOps.IndexOfDesc);
    Result:= SText;

    if SBeginsWith(SDesc, '(') then
      Result:= Result+CompletionOps.SuffixChar+'(';
  end;
end;

procedure TFormATSynEditComplete.ListDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  Str, SItem, SHint: string;
  NSize, i: integer;
begin
  Str:= SList[AIndex];

  if AIndex=List.ItemIndex then
    C.Brush.Color:= CompletionOps.ColorSelBg
  else
    C.Brush.Color:= CompletionOps.ColorBg;
  C.FillRect(ARect);
  C.Font.Assign(List.Font);

  //alternate listbox: OnResult is set, then 3 columns, tab-separated:
  //paint column1 at left,
  //paint column2 at right
  if Assigned(FOnResult) then
  begin
    SItem:= SGetItem(Str, #9);
    SHint:= SGetItem(Str, #9);

    //prefix
    C.Font.Style:= CompletionOps.FontStyles[0];
    C.Font.Color:= CompletionOps.ColorFont[0];
    C.TextOut(ARect.Left+List.ClientWidth-List.Canvas.TextWidth(SHint)-CompletionOps.TextIndent0, ARect.Top, SHint);

    //text
    C.Font.Style:= CompletionOps.FontStyles[1];
    C.Font.Color:= CompletionOps.ColorFont[1];
    C.TextOut(ARect.Left+CompletionOps.TextIndent0, ARect.Top, SItem);

    exit;
  end;

  //usual case, n columns, tab-char separates hint (in hint window)
  SHint:= SGetItemAtEnd(Str, CompletionOps.HintChar);
  if AIndex=List.ItemIndex then
    DoHintShow(SHint);

  NSize:= CompletionOps.TextIndent0;

  for i:= 0 to cCompletionColumnCount-1 do
  begin
    SItem:= SGetItem(Str, CompletionOps.SepChar);
    if i=CompletionOps.IndexOfText then
      SItem:= SGetItem(SItem, CompletionOps.SuffixChar);

    C.Font.Style:= CompletionOps.FontStyles[i];
    C.Font.Color:= CompletionOps.ColorFont[i];
    C.TextOut(ARect.Left+NSize, ARect.Top, SItem);
    Inc(NSize, C.TextWidth(SItem)+CompletionOps.TextIndent);
  end;
end;

procedure TFormATSynEditComplete.DoResult;
begin
  if Assigned(FOnResult) then
    FOnResult(Self, FSnippetId, List.ItemIndex)
  else
    DoReplaceTo(GetResultText);

  Close;
end;

procedure TFormATSynEditComplete.DoUpdate;
var
  AText: string;
  P: TPoint;
begin
  if Assigned(FOnGetProp) then
    FOnGetProp(Editor, AText, FCharsLeft, FCharsRight);

  if (AText='') then
    begin Close; exit end;

  SList.Text:= AText;
  if SList.Count=0 then exit;
  if CompletionOps.ListSort then SList.Sort;

  List.ItemCount:= SList.Count;
  List.ItemIndex:= 0;

  Color:= CompletionOps.ColorBg;
  List.Color:= CompletionOps.ColorBg;
  List.Font.Name:= CompletionOps.FontName;
  List.Font.Size:= CompletionOps.FontSize;
  List.ItemHeight:= CompletionOps.ItemHeight;
  List.BorderSpacing.Around:= CompletionOps.BorderSize;
  List.Invalidate;

  P.X:= Editor.Carets[0].CoordX-Editor.TextCharSize.X*FCharsLeft;
  P.Y:= Editor.Carets[0].CoordY+Editor.TextCharSize.Y;
  P:= Editor.ClientToScreen(P);

  SetBounds(P.X, P.Y, CompletionOps.FormSizeX, CompletionOps.FormSizeY);
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

procedure TFormATSynEditComplete.DoHintShow(const AHint: string);
var
  P: TPoint;
  R: TRect;
begin
  R:= FHintWnd.CalcHintRect(CompletionOps.HintSizeX, AHint, nil);

  P:= ClientToScreen(Point(Width, 0));
  OffsetRect(R, P.X, P.Y);

  FHintWnd.ActivateHint(R, AHint);
  FHintWnd.Invalidate; //for Win
  Editor.Invalidate; //for Win
end;

procedure TFormATSynEditComplete.DoHintHide;
begin
  if Assigned(FHintWnd) then
    FHintWnd.Hide;
end;


initialization

  FillChar(CompletionOps, SizeOf(CompletionOps), 0);
  with CompletionOps do
  begin
    ColorFont[0] := clPurple;
    ColorFont[1] := clBlack;
    ColorFont[2] := clNavy;
    ColorFont[3] := clBlack;
    ColorFont[4] := clBlack;
    ColorBg := $e0e0e0;
    ColorSelBg := clMedGray;
    FontStyles[0] := [fsBold];
    FontName := 'default';
    FontSize := 10;
    CharsCloseListbox := ' .,;/\''"=<>()[]{}';
    IndexOfText := 1;
    IndexOfDesc := 2;
    SepChar := '|';
    HintChar := #9;
    SuffixChar := #1;
    ListSort := false;
    KeyUpDownWrap := true;
    ItemHeight := 17;
    BorderSize := 4;
    FormSizeX := 500;
    FormSizeY := 200;
    HintSizeX := 400;
    TextIndent0 := 4;
    TextIndent := 8;
  end;

finalization

  if Assigned(FormComplete) then
    FormComplete.Free;

end.

