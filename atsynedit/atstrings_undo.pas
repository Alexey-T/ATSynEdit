{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStrings_Undo;

{$mode objfpc}{$H+}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATStringProc_Separator,
  ATStringProc_Arrays;

type
  TATEditAction = (
    Change,
    ChangeEol,
    Insert,
    Delete,
    ClearModified,
    CaretJump,
    Add
    );

  TATEditorRunningUndoOrRedo = (
    NotUndoRedo,
    Undo,
    Redo
    );

const
  cEditAction_CachedWrapinfoUpdate: array[TATEditAction] of boolean = (
    true,
    true,
    false,
    false,
    true,
    true,
    true
    );

var
  ATStrings_PauseForUndoGroup: integer = 700;
  //if pause (in msec) between 2 actions is smaller, actions will be undone as a group

type
  { TATUndoItem }

  TATUndoItem = class
  private
    const PartSep = #9; //separators for AsString property
    const MarkersSep = #1;
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
  public
    ItemTickCount: QWord; //from GetTickCount64

    ItemGlobalCounter: DWord; //several adjacent items, made by the same editor command, have the same GlobalCounter
                              //it's used for deleting old undo-items when MaxCount is reached

    ItemCommandCode: integer; //if not 0, all adjacent items with the same CommandCode will undo as a group
                              //it's used mainly for commands "move lines up/down", CudaText issue #3289

    ItemAction: TATEditAction; //action of undo-item
    ItemIndex: integer; //index of editor line
    ItemText: UnicodeString; //text of that editor line
    ItemEnd: TATLineEnds; //line-ending of that editor line
    ItemLineState: TATLineState; //line-state of that editor line
    ItemCarets: TATPointPairArray; //carets packed into array
    ItemCarets2: TATPointPairArray;
    ItemMarkers: TATMarkerMarkerArray;
    ItemMarkers2: TATMarkerMarkerArray;
    ItemAttribs: TATMarkerAttribArray;
    ItemSoftMark: boolean; //undo soft-mark. logic is described in ATSynEdit Wiki page
    ItemHardMark: boolean; //undo hard-mark

    constructor Create(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
      ASoftMark, AHardMark: boolean;
      const ACarets, ACarets2: TATPointPairArray;
      const AMarkers, AMarkers2: TATMarkerMarkerArray;
      const AAttribs: TATMarkerAttribArray;
      ACommandCode: integer;
      const ATickCount: QWord); virtual;
    constructor CreateEmpty;
    procedure Assign(const D: TATUndoItem);
    property AsString: string read GetAsString write SetAsString;
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
    FLastTick: QWord;
    FNewCommandMark: boolean;
    function GetAsString: string;
    function GetCapacity: integer;
    function GetItem(N: integer): TATUndoItem;
    procedure SetAsString(const AValue: string);
    procedure SetCapacity(AValue: integer);
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
    property Capacity: integer read GetCapacity write SetCapacity;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteLast;
    procedure DeleteUnmodifiedMarks;
    procedure DeleteTrailingCaretJumps;
    procedure Add(AAction: TATEditAction; AIndex: integer; const AText: atString;
      AEnd: TATLineEnds; ALineState: TATLineState;
      const ACarets, ACarets2: TATPointPairArray;
      const AMarkers, AMarkers2: TATMarkerMarkerArray;
      const AAttribs: TATMarkerAttribArray;
      ACommandCode: integer;
      AUndoOrRedo: TATEditorRunningUndoOrRedo);
    procedure AddUnmodifiedMark;
    function DebugText: string;
    function IsEmpty: boolean;
    property AsString: string read GetAsString write SetAsString;
    property NewCommandMark: boolean read FNewCommandMark write FNewCommandMark;
      //NewCommandMark is set from ATSynEdit.DoCommand() or CudaText API.
      //When it's set to True, Undo list increases GlobalCounter for the next added Undo item.
      //All Undo items with the same GlobalCounter are performed by a single command or single CudaText API call.
      //So they must be undone with a single undo action.
      //Also, this allows Undo list to support MaxCount _complex actions_ in the list, intead of MaxCount simple items.
  end;


implementation

uses
  Math, Dialogs;

{ TATUndoItem }

function TATUndoItem.GetAsString: string;
//if more data will be needed here, add it to 'carets' item after MarkersSep=#1 separator
begin
  Result:=
    IntToStr(Ord(ItemAction))+PartSep+
    IntToStr(ItemIndex)+PartSep+
    IntToStr(Ord(ItemEnd))+PartSep+
    IntToStr(Ord(ItemLineState))+PartSep+
    PointPairArrayToString(ItemCarets)+MarkersSep+
      MarkerArrayToString(ItemMarkers)+MarkersSep+
      IntToStr(ItemGlobalCounter)+MarkersSep+
      IntToStr(ItemTickCount)+MarkersSep+
      IntToStr(ItemCommandCode)+MarkersSep+
      PointPairArrayToString(ItemCarets2)+MarkersSep+
      MarkerArrayToString(ItemMarkers2)+PartSep+
    IntToStr(Ord(ItemSoftMark))+PartSep+
    IntToStr(Ord(ItemHardMark))+PartSep+
    UTF8Encode(ItemText);
end;

procedure TATUndoItem.SetAsString(const AValue: string);
var
  Sep, Sep2: TATStringSeparator;
  S, SubItem: string;
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

  //this item contains:
  //  carets, markers, global_cnt, tick_cnt, commandcode, carets2, markers2
  Sep.GetItemStr(S);
  Sep2.Init(S, MarkersSep);
  //a) carets
  Sep2.GetItemStr(SubItem);
  StringToPointPairArray(ItemCarets, SubItem);
  //b) markers
  Sep2.GetItemStr(SubItem);
  if SubItem<>'' then
    StringToMarkerArray(ItemMarkers, SubItem)
  else
    ItemMarkers:= nil;
  //c) global_cnt
  Sep2.GetItemDWord(ItemGlobalCounter, 0);
  //d) tick_cnt
  Sep2.GetItemStr(SubItem);
  ItemTickCount:= StrToQWordDef(SubItem, 0);
  //e) commandcode
  Sep2.GetItemInt(ItemCommandCode, 0);
  //f) carets2
  Sep2.GetItemStr(SubItem);
  StringToPointPairArray(ItemCarets2, SubItem);
  //g) markers2
  Sep2.GetItemStr(SubItem);
  if SubItem<>'' then
    StringToMarkerArray(ItemMarkers2, SubItem)
  else
    ItemMarkers2:= nil;

  Sep.GetItemStr(S);
  ItemSoftMark:= S='1';

  Sep.GetItemStr(S);
  ItemHardMark:= S='1';

  //use Sep.GetRect for last item, because line can contain tab-chars
  Sep.GetRest(S);
  ItemText:= UTF8Decode(S);
end;

procedure TATUndoItem.Assign(const D: TATUndoItem);
begin
  ItemAction:= D.ItemAction;
  ItemIndex:= D.ItemIndex;
  ItemEnd:= D.ItemEnd;
  ItemLineState:= D.ItemLineState;
  ItemText:= D.ItemText;
  ItemCarets:= D.ItemCarets;
  ItemCarets2:= D.ItemCarets2;
  ItemSoftMark:= D.ItemSoftMark;
  ItemHardMark:= D.ItemHardMark;
  ItemCommandCode:= D.ItemCommandCode;
  ItemTickCount:= D.ItemTickCount;
  ItemGlobalCounter:= D.ItemGlobalCounter;
end;


constructor TATUndoItem.Create(AAction: TATEditAction; AIndex: integer;
  const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
  ASoftMark, AHardMark: boolean;
  const ACarets, ACarets2: TATPointPairArray;
  const AMarkers, AMarkers2: TATMarkerMarkerArray;
  const AAttribs: TATMarkerAttribArray;
  ACommandCode: integer;
  const ATickCount: QWord);
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
  ItemCommandCode:= ACommandCode;
  ItemTickCount:= ATickCount;
  ItemGlobalCounter:= 0;

  SetLength(ItemCarets, Length(ACarets));
  for i:= 0 to High(ACarets) do
    ItemCarets[i]:= ACarets[i];

  SetLength(ItemCarets2, Length(ACarets2));
  for i:= 0 to High(ACarets2) do
    ItemCarets2[i]:= ACarets2[i];

  SetLength(ItemMarkers, Length(AMarkers));
  for i:= 0 to High(AMarkers) do
    ItemMarkers[i]:= AMarkers[i];

  SetLength(ItemMarkers2, Length(AMarkers2));
  for i:= 0 to High(AMarkers2) do
    ItemMarkers2[i]:= AMarkers2[i];

  SetLength(ItemAttribs, Length(AAttribs));
  for i:= 0 to High(AAttribs) do
    ItemAttribs[i]:= AAttribs[i];
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
    (i1.ItemAction=TATEditAction.Change) and
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
  const ACarets, ACarets2: TATPointPairArray;
  const AMarkers, AMarkers2: TATMarkerMarkerArray;
  const AAttribs: TATMarkerAttribArray;
  ACommandCode: integer;
  AUndoOrRedo: TATEditorRunningUndoOrRedo);
var
  Item: TATUndoItem;
  NewTick: QWord;
  NGlobalCounter: DWord;
  bNotEmpty: boolean;
begin
  if FLocked then Exit;
  if FMaxCount=0 then Exit;
  bNotEmpty:= Count>0;

  if bNotEmpty then
  begin
    NGlobalCounter:= Last.ItemGlobalCounter;
    if FNewCommandMark then
    begin
      FNewCommandMark:= false;
      Inc(NGlobalCounter);
    end;
  end
  else
    NGlobalCounter:= 0;

  //not duplicate change?
  if bNotEmpty and (AAction in [TATEditAction.Change, TATEditAction.ChangeEol]) then
  begin
    Item:= Last;
    if (Item.ItemAction=AAction) and
      (Item.ItemIndex=AIndex) and
      (Item.ItemText=AText) and
      (Item.ItemCommandCode=ACommandCode) then
        Exit;
  end;

  //don't save TickCount if we are running Undo/Redo
  if AUndoOrRedo=TATEditorRunningUndoOrRedo.NotUndoRedo then
  begin
    NewTick:= GetTickCount64;
    if (FLastTick>0) and (NewTick-FLastTick>=ATStrings_PauseForUndoGroup) then
      FSoftMark:= true;
  end
  else
  begin
    NewTick:= 0;
  end;

  FLastTick:= NewTick;

  Item:= TATUndoItem.Create(AAction, AIndex, AText, AEnd, ALineState,
                            FSoftMark, FHardMark,
                            ACarets,
                            ACarets2,
                            AMarkers,
                            AMarkers2,
                            AAttribs,
                            ACommandCode,
                            NewTick);
  Item.ItemGlobalCounter:= NGlobalCounter;

  FList.Add(Item);
  FSoftMark:= false;

  //support MaxCount _actions_ in the list, intead of MaxCount simple items
  //CudaText issue #3084
  while (NGlobalCounter-Items[0].ItemGlobalCounter)>MaxCount do
    Delete(0);
end;


procedure TATUndoList.AddUnmodifiedMark;
var
  Item: TATUndoItem;
  Carets: TATPointPairArray;
  Markers: TATMarkerMarkerArray;
  Attribs: TATMarkerAttribArray;
begin
  //if FLocked then exit; //on load file called with Locked=true

  //don't do two marks
  Item:= Last;
  if Assigned(Item) then
    if Item.ItemAction=TATEditAction.ClearModified then exit;

  Carets:= nil;
  Markers:= nil;
  Attribs:= nil;

  Item:= TATUndoItem.Create(
    TATEditAction.ClearModified,
    0,
    '',
    TATLineEnds.None,
    TATLineState.None,
    false,
    false,
    Carets,
    Carets, //2nd carets
    Markers,
    Markers, //2nd markers
    Attribs,
    0,
    0
    );
  FList.Add(Item);
end;

procedure TATUndoList.DeleteUnmodifiedMarks;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    if Items[i].ItemAction=TATEditAction.ClearModified then
      Delete(i);
end;

procedure TATUndoList.DeleteTrailingCaretJumps;
begin
  while (Count>0) and (Last.ItemAction=TATEditAction.CaretJump) do
    DeleteLast;
end;

function TATUndoList.DebugText: string;
var
  Item: TATUndoItem;
  s_action, s_text: string;
  i, n_carets: integer;
const
  MaxItems=40;
  MaxLen=30;
begin
  Result:= '';
  for i:= 0 to Min(MaxItems, Count)-1 do
  begin
    Item:= Items[i];
    System.Str(Item.ItemAction, s_action);
    s_text:= UTF8Encode(Item.ItemText);
    if Length(s_text)>MaxLen then
      s_text:= Copy(s_text, 1, MaxLen)+'...';
    n_carets:= Length(Item.ItemCarets) div 2;
    Result:= Result+Format('actn "%s", text "%s", crts %d'#10, [s_action, s_text, n_carets]);
  end;
end;

function TATUndoList.IsEmpty: boolean;
const
  cIgnoredActions = [
    TATEditAction.ClearModified,
    TATEditAction.CaretJump
    ];
var
  i: integer;
begin
  Result:= true;
  for i:= Count-1 downto 0 do
  begin
    if not (Items[i].ItemAction in cIgnoredActions) then
      exit(false);
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

function TATUndoList.GetCapacity: integer;
begin
  Result:= FList.Capacity;
end;

procedure TATUndoList.SetCapacity(AValue: integer);
begin
  FList.Capacity:= AValue;
end;


end.

