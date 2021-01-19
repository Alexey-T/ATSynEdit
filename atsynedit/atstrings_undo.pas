{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStrings_Undo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATStringProc_Separator;

type
  TATEditAction = (
    aeaChange,
    aeaChangeEol,
    aeaInsert,
    aeaDelete,
    aeaClearModified,
    aeaCaretJump
    );

const
  StrEditActionDescriptions: array[TATEditAction] of string = (
    'change',
    'change-eol',
    'insert',
    'delete',
    'clear-mod',
    'jump'
    );

  cEditActionSetsModified: array[TATEditAction] of boolean = (
    true,
    true,
    true,
    true,
    false,
    false
    );

type
  { TATUndoItem }

  TATUndoItem = class
  private
    const PartSep = #9;
    const MarkersSep = #1;
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
  public
    ItemAction: TATEditAction;
    ItemIndex: integer;
    ItemText: atString;
    ItemEnd: TATLineEnds;
    ItemLineState: TATLineState;
    ItemCarets: TATPointArray;
    ItemMarkers: TATInt64Array;
    ItemSoftMark: boolean;
    ItemHardMark: boolean;
    procedure Assign(const D: TATUndoItem);
    property AsString: string read GetAsString write SetAsString;
    constructor Create(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
      ASoftMark, AHardMark: boolean;
      const ACarets: TATPointArray; const AMarkers: TATInt64Array); virtual;
    constructor CreateEmpty;
  end;

type
  { TATUndoList }

  TATUndoList = class
  private
    FList: TFPList;
    FMaxCount: integer;
    FLocked: boolean;
    FSoftMark: boolean;
    FHardMark: boolean;
    function GetAsString: string;
    function GetItem(N: integer): TATUndoItem;
    procedure SetAsString(const AValue: string);
  public
    constructor Create(AMaxCount: integer); virtual;
    destructor Destroy; override;
    function IsIndexValid(N: integer): boolean; inline;
    function IsItemsEqual(N1, N2: integer): boolean;
    function Count: integer; inline;
    function Last: TATUndoItem;
    property Items[N: integer]: TATUndoItem read GetItem; default;
    property MaxCount: integer read FMaxCount write FMaxCount;
    property SoftMark: boolean read FSoftMark write FSoftMark;
    property HardMark: boolean read FHardMark write FHardMark;
    property Locked: boolean read FLocked write FLocked;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteLast;
    procedure DeleteUnmodifiedMarks;
    procedure Add(AAction: TATEditAction; AIndex: integer; const AText: atString;
      AEnd: TATLineEnds; ALineState: TATLineState;
      const ACarets: TATPointArray; const AMarkers: TATInt64Array);
    procedure AddUnmodifiedMark;
    function DebugText: string;
    function IsEmpty: boolean;
    property AsString: string read GetAsString write SetAsString;
  end;


implementation

uses
  Math, Dialogs;

function PointsArrayToString(const A: TATPointArray): string;
var
  j: integer;
  Pnt: TPoint;
begin
  Result:= '';
  for j:= 0 to Length(A)-1 do
  begin
    Pnt:= A[j];
    Result+= IntToStr(Pnt.X)+','+IntToStr(Pnt.Y)+';';
  end;
end;

function Int64ArrayToString(const A: TATInt64Array): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to High(A) do
    Result+= IntToStr(A[i])+',';
end;

procedure StringToPointsArray(var A: TATPointArray; const AStr: string);
var
  Sep: TATStringSeparator;
  SItem, S1, S2: string;
  i, NLen: integer;
begin
  NLen:= SFindCharCount(AStr, ';');
  SetLength(A, NLen);
  Sep.Init(AStr, ';');
  for i:= 0 to NLen-1 do
  begin
    Sep.GetItemStr(SItem);
    SSplitByChar(SItem, ',', S1, S2);
    A[i].X:= StrToIntDef(S1, 0);
    A[i].Y:= StrToIntDef(S2, 0);
  end;
end;

procedure StringToInt64Array(var A: TATInt64Array; const AStr: string);
var
  Sep: TATStringSeparator;
  i, NLen: integer;
begin
  NLen:= SFindCharCount(AStr, ',');
  SetLength(A, NLen);
  Sep.Init(AStr, ',');
  for i:= 0 to NLen-1 do
  begin
    Sep.GetItemInt64(A[i], 0);
  end;
end;

{ TATUndoItem }

function TATUndoItem.GetAsString: string;
var
  SMarks: string;
begin
  if Length(ItemMarkers)>0 then
    SMarks:= MarkersSep+Int64ArrayToString(ItemMarkers)
  else
    SMarks:= '';

  Result:=
    IntToStr(Ord(ItemAction))+PartSep+
    IntToStr(ItemIndex)+PartSep+
    IntToStr(Ord(ItemEnd))+PartSep+
    IntToStr(Ord(ItemLineState))+PartSep+
    PointsArrayToString(ItemCarets)+SMarks+PartSep+
    IntToStr(Ord(ItemSoftMark))+PartSep+
    IntToStr(Ord(ItemHardMark))+PartSep+
    UTF8Encode(ItemText);
end;

procedure TATUndoItem.SetAsString(const AValue: string);
var
  Sep: TATStringSeparator;
  S, S1, S2: string;
  N: integer;
begin
  Sep.Init(AValue, PartSep);

  Sep.GetItemInt(N, 0);
  ItemAction:= TATEditAction(N);

  Sep.GetItemInt(N, 0);
  ItemIndex:= N;

  Sep.GetItemInt(N, 0);
  ItemEnd:= TATLineEnds(N);

  Sep.GetItemInt(N, 0);
  ItemLineState:= TATLineState(N);

  Sep.GetItemStr(S);
  SSplitByChar(S, MarkersSep, S1, S2);
  StringToPointsArray(ItemCarets, S1);
  if S2<>'' then
    StringToInt64Array(ItemMarkers, S2)
  else
    SetLength(ItemMarkers, 0);

  Sep.GetItemStr(S);
  ItemSoftMark:= S='1';

  Sep.GetItemStr(S);
  ItemHardMark:= S='1';

  Sep.GetItemStr(S);
  ItemText:= S;
end;

procedure TATUndoItem.Assign(const D: TATUndoItem);
begin
  ItemAction:= D.ItemAction;
  ItemIndex:= D.ItemIndex;
  ItemEnd:= D.ItemEnd;
  ItemLineState:= D.ItemLineState;
  ItemText:= D.ItemText;
  ItemCarets:= D.ItemCarets;
  ItemSoftMark:= D.ItemSoftMark;
  ItemHardMark:= D.ItemHardMark;
end;


constructor TATUndoItem.Create(AAction: TATEditAction; AIndex: integer;
  const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
  ASoftMark, AHardMark: boolean;
  const ACarets: TATPointArray; const AMarkers: TATInt64Array);
var
  i: integer;
begin
  ItemAction:= AAction;
  ItemIndex:= AIndex;
  ItemText:= AText;
  ItemEnd:= AEnd;
  ItemLineState:= ALineState;
  ItemSoftMark:= ASoftMark;
  ItemHardMark:= AHardMark;

  SetLength(ItemCarets, Length(ACarets));
  for i:= 0 to High(ACarets) do
    ItemCarets[i]:= ACarets[i];

  SetLength(ItemMarkers, Length(AMarkers));
  for i:= 0 to High(AMarkers) do
    ItemMarkers[i]:= AMarkers[i];
end;

constructor TATUndoItem.CreateEmpty;
begin
  inherited Create;
end;

{ TATUndoList }

function TATUndoList.GetItem(N: integer): TATUndoItem;
begin
  if IsIndexValid(N) then
    Result:= TATUndoItem(FList[N])
  else
    Result:= nil;
end;

constructor TATUndoList.Create(AMaxCount: integer);
begin
  FList:= TFPList.Create;
  FMaxCount:= AMaxCount;
  FSoftMark:= false;
  FHardMark:= false;
  FLocked:= false;
end;

destructor TATUndoList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATUndoList.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATUndoList.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<Count);
end;

function TATUndoList.IsItemsEqual(N1, N2: integer): boolean;
var
  i1, i2: TATUndoItem;
begin
  Result:= false;
  i1:= Items[N1];
  i2:= Items[N2];
  if i1=nil then Exit;
  if i2=nil then Exit;
  Result:=
    (i1.ItemAction=aeaChange) and
    (i1.ItemAction=i2.ItemAction) and
    (i1.ItemIndex=i2.ItemIndex) and
    (i1.ItemText=i2.ItemText);
end;

procedure TATUndoList.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

procedure TATUndoList.DeleteLast;
begin
  Delete(Count-1);
end;

procedure TATUndoList.Clear;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    Delete(i);
end;

procedure TATUndoList.Add(AAction: TATEditAction; AIndex: integer;
  const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
  const ACarets: TATPointArray; const AMarkers: TATInt64Array);
var
  Item: TATUndoItem;
begin
  if FLocked then Exit;

  //not dup change?
  if (Count>0) and (AAction in [aeaChange, aeaChangeEol]) then
  begin
    Item:= Items[Count-1];
    if (Item.ItemAction=AAction) and
      (Item.ItemIndex=AIndex) and
      (Item.ItemText=AText) then
        Exit;
  end;

  //not insert/delete same index?
  if (Count>0) and (AAction=aeaDelete) then
  begin
    Item:= Items[Count-1];
    if (Item.ItemAction=aeaInsert) and
      (Item.ItemIndex=AIndex) then
      begin
        DeleteLast;
        Exit
      end;
  end;

  Item:= TATUndoItem.Create(AAction, AIndex, AText, AEnd, ALineState, FSoftMark, FHardMark, ACarets, AMarkers);
  FList.Add(Item);
  FSoftMark:= false;

  while Count>MaxCount do
    Delete(0);
end;


procedure TATUndoList.AddUnmodifiedMark;
var
  Item: TATUndoItem;
  Carets: TATPointArray;
  Markers: TATInt64Array;
begin
  //if FLocked then exit; //on load file called with Locked=true

  //don't do two marks
  Item:= Last;
  if Assigned(Item) then
    if Item.ItemAction=aeaClearModified then exit;

  SetLength(Carets, 0);
  SetLength(Markers, 0);

  Item:= TATUndoItem.Create(
    aeaClearModified, 0, '',
    cEndNone, cLineStateNone,
    false, false, Carets, Markers);
  FList.Add(Item);
end;

procedure TATUndoList.DeleteUnmodifiedMarks;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    if Items[i].ItemAction=aeaClearModified then
      Delete(i);
end;

function TATUndoList.DebugText: string;
var
  s_action, s_text: string;
  i, n_carets: integer;
  Item: TATUndoItem;
const
  MaxItems=40;
  MaxLen=30;
begin
  Result:= '';
  for i:= 0 to Min(MaxItems, Count)-1 do
  begin
    Item:= Items[i];
    s_action:= StrEditActionDescriptions[Item.ItemAction];
    s_text:= UTF8Encode(Item.ItemText);
    if Length(s_text)>MaxLen then
      s_text:= Copy(s_text, 1, MaxLen)+'...';
    n_carets:= Length(Item.ItemCarets) div 2;
    Result:= Result+Format('actn "%s", text "%s", crts %d'#10, [s_action, s_text, n_carets]);
  end;
end;

function TATUndoList.IsEmpty: boolean;
var
  N: integer;
begin
  N:= Count-1;
  while (N>=0) and not cEditActionSetsModified[Items[N].ItemAction] do
    Dec(N);
  Result:= N<0;
end;


function TATUndoList.Last: TATUndoItem;
begin
  if Count>0 then
    Result:= Items[Count-1]
  else
    Result:= nil;
end;


function TATUndoList.GetAsString: string;
var
  L: TStringList;
  i: integer;
begin
  L:= TStringList.Create;
  try
    L.TextLineBreakStyle:= tlbsLF;
    for i:= 0 to Count-1 do
      L.Add(Items[i].AsString);
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;

procedure TATUndoList.SetAsString(const AValue: string);
var
  L: TStringList;
  Item: TATUndoItem;
  i: integer;
begin
  Clear;
  L:= TStringList.Create;
  try
    L.TextLineBreakStyle:= tlbsLF;
    L.Text:= AValue;
    for i:= 0 to L.Count-1 do
    begin
      if L[i]='' then Continue;
      Item:= TATUndoItem.CreateEmpty;
      Item.AsString:= L[i];
      FList.Add(Item);
    end;
  finally
    FreeAndNil(L);
  end;
end;


end.

