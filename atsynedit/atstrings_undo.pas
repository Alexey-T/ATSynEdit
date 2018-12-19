{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStrings_Undo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  ATStringProc;

type
  TATEditAction = (
    cEditActionChange,
    cEditActionChangeEol,
    cEditActionInsert,
    cEditActionDelete,
    cEditActionClearModified
    );

const
  StrEditActionDescriptions: array[TATEditAction] of string = (
    'change',
    'change-eol',
    'insert',
    'delete',
    'clear-mod'
    );

type
  { TATUndoItem }

  TATUndoItem = class
  private
    const PartSep = #9;
    function GetAsString: string;
    procedure SetAsString(AValue: string);
  public
    ItemAction: TATEditAction;
    ItemIndex: integer;
    ItemText: atString;
    ItemEnd: TATLineEnds;
    ItemCarets: TATPointArray;
    ItemSoftMark: boolean;
    ItemHardMark: boolean;
    procedure Assign(const D: TATUndoItem);
    property AsString: string read GetAsString write SetAsString;
    constructor Create(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds; ASoftMark, AHardMark: boolean;
      const ACarets: TATPointArray); virtual;
    constructor CreateEmpty;
  end;

type
  { TATUndoList }

  TATUndoList = class
  private
    FList: TList;
    FMaxCount: integer;
    FLocked: boolean;
    FSoftMark: boolean;
    FHardMark: boolean;
    function GetAsString: string;
    function GetItem(N: integer): TATUndoItem;
    procedure SetAsString(const AValue: string);
  public
    constructor Create; virtual;
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
      AEnd: TATLineEnds; const ACarets: TATPointArray);
    procedure AddUnmodifiedMark;
    function DebugText: string;
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

procedure StringToPointsArray(var A: TATPointArray; Str: string);
var
  SItem: string;
  i, NLen: integer;
begin
  NLen:= 0;
  for i:= 1 to Length(Str) do
    if Str[i]=';' then Inc(NLen);
  SetLength(A, NLen);
  for i:= 0 to NLen-1 do
  begin
    SItem:= SGetItem(Str, ';');
    A[i].X:= StrToIntDef(SGetItem(SItem, ','), 0);
    A[i].Y:= StrToIntDef(SGetItem(SItem, ','), 0);
  end;
end;

{ TATUndoItem }

function TATUndoItem.GetAsString: string;
begin
  Result:=
    IntToStr(Ord(ItemAction))+PartSep+
    IntToStr(ItemIndex)+PartSep+
    IntToStr(Ord(ItemEnd))+PartSep+
    PointsArrayToString(ItemCarets)+PartSep+
    IntToStr(Ord(ItemSoftMark))+PartSep+
    IntToStr(Ord(ItemHardMark))+PartSep+
    UTF8Encode(ItemText);
end;

procedure TATUndoItem.SetAsString(AValue: string);
var
  SItem: string;
begin
  SItem:= SGetItem(AValue, PartSep);
  ItemAction:= TATEditAction(StrToIntDef(SItem, 0));

  SItem:= SGetItem(AValue, PartSep);
  ItemIndex:= StrToIntDef(SItem, 0);

  SItem:= SGetItem(AValue, PartSep);
  ItemEnd:= TATLineEnds(StrToIntDef(SItem, 0));

  SItem:= SGetItem(AValue, PartSep);
  StringToPointsArray(ItemCarets, SItem);

  SItem:= SGetItem(AValue, PartSep);
  ItemSoftMark:= SItem='1';

  SItem:= SGetItem(AValue, PartSep);
  ItemHardMark:= SItem='1';

  ItemText:= UTF8Decode(AValue);
end;

procedure TATUndoItem.Assign(const D: TATUndoItem);
begin
  ItemAction:= D.ItemAction;
  ItemIndex:= D.ItemIndex;
  ItemEnd:= D.ItemEnd;
  ItemText:= D.ItemText;
  ItemCarets:= D.ItemCarets;
  ItemSoftMark:= D.ItemSoftMark;
  ItemHardMark:= D.ItemHardMark;
end;


constructor TATUndoItem.Create(AAction: TATEditAction; AIndex: integer;
  const AText: atString; AEnd: TATLineEnds; ASoftMark, AHardMark: boolean;
  const ACarets: TATPointArray);
var
  i: integer;
begin
  ItemAction:= AAction;
  ItemIndex:= AIndex;
  ItemText:= AText;
  ItemEnd:= AEnd;
  ItemSoftMark:= ASoftMark;
  ItemHardMark:= AHardMark;

  SetLength(ItemCarets, Length(ACarets));
  for i:= 0 to High(ACarets) do
  begin
    ItemCarets[i]:= ACarets[i];
  end;
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

constructor TATUndoList.Create;
begin
  FList:= TList.Create;
  FMaxCount:= 5000;
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
  if not Assigned(i1) or not Assigned(i2) then Exit;
  Result:=
    (i1.ItemAction=cEditActionChange) and
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
  const AText: atString; AEnd: TATLineEnds;
  const ACarets: TATPointArray);
var
  Item: TATUndoItem;
begin
  if FLocked then Exit;

  //not dup change?
  if (Count>0) and (AAction in [cEditActionChange, cEditActionChangeEol]) then
  begin
    Item:= Items[Count-1];
    if (Item.ItemAction=AAction) and
      (Item.ItemIndex=AIndex) and
      (Item.ItemText=AText) then
        Exit;
  end;

  //not insert/delete same index?
  if (Count>0) and (AAction=cEditActionDelete) then
  begin
    Item:= Items[Count-1];
    if (Item.ItemAction=cEditActionInsert) and
      (Item.ItemIndex=AIndex) then
      begin
        DeleteLast;
        Exit
      end;
  end;

  Item:= TATUndoItem.Create(AAction, AIndex, AText, AEnd, FSoftMark, FHardMark, ACarets);
  FList.Add(Item);
  FSoftMark:= false;

  while Count>MaxCount do
    Delete(0);
end;


procedure TATUndoList.AddUnmodifiedMark;
var
  Item: TATUndoItem;
  Carets: TATPointArray;
begin
  //if FLocked then exit; //on load file called with Locked=true

  //don't do two marks
  Item:= Last;
  if Assigned(Item) then
    if Item.ItemAction=cEditActionClearModified then exit;

  SetLength(Carets, 0);
  Item:= TATUndoItem.Create(cEditActionClearModified, 0, '', cEndNone, false, false, Carets);
  FList.Add(Item);
end;

procedure TATUndoList.DeleteUnmodifiedMarks;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    if Items[i].ItemAction=cEditActionClearModified then
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

