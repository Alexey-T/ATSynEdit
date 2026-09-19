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

const
  ATStrings_MinUndoRunCount = 25;
  //2026.09: performance fix. Minimum count of undo-items, which are undone in one
  //bulk operation (UndoRun* functions). Runs of smaller size are undone with
  //usual per-item code, to keep old behavior for typical small edits.

type
  { TATUndoItem }

  TATUndoItem = class
  private
    const PartSep = #9; //separators for AsString property
    const MarkersSep = #1;
    const ArraysDisabledSpec = '-';
      //2026.09 (issue #385): spec value of serialized undo-data, which marks all
      //carets/markers arrays of the item as disabled; normal serialization never
      //produces this char for those parts (arrays are digits/commas/semicolons)
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
  public
    ItemTickCount: QWord; //from GetTickCount64

    ItemGlobalCounter: DWord; //several adjacent items, made by the same editor command, have the same GlobalCounter
                              //it's used for deleting old undo-items when MaxCount is reached

    ItemCommandCode: integer; //if not 0, all adjacent items with the same CommandCode will undo as a group
                              //it's used mainly for commands "move lines up/down", CudaText issue #3289

    ItemAction: TATEditAction; //action of undo-item
    ItemIndex: SizeInt; //index of editor line
    ItemText: UnicodeString; //text of that editor line
    ItemEnd: TATLineEnds; //line-ending of that editor line
    ItemLineState: TATLineState; //line-state of that editor line
    ItemCarets: TATPointPairArray; //carets
    ItemCarets2: TATPointPairArray; //carets for 'paired' editor (one frame in CudaText has 2 editors)
    ItemMarkers: TATMarkerMarkerArray; //markers; also reused for caret pos for CaretJump action
    ItemMarkers2: TATMarkerMarkerArray; //markers for 'paired' editor
    ItemAttribs: TATMarkerAttribArray; //attributes
    ItemSoftMark: boolean; //undo soft-mark. logic is described in ATSynEdit Wiki page
    ItemHardMark: boolean; //undo hard-mark
    ItemArraysDisabled: boolean;
      //2026.09 (issue #385): True = all carets/markers/attribs arrays of this item are
      //disabled: they must be empty (nil) here, and undo/redo must not apply them,
      //i.e. must NOT touch the current carets/markers/attribs of the editor.
      //Bulk code (which saves many undo-items with identical captured arrays) stores
      //real arrays only in the 1st created undo-item of the bulk run; all other items
      //of the run get this flag. It gives big RAM saving for big runs, e.g. 10K items
      //with M live markers/attribs keep only one O(M) copy instead of one per item.
      //Serializer writes this state as ArraysDisabledSpec char, see GetAsString().

    constructor Create(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
      ASoftMark, AHardMark: boolean;
      const ACarets, ACarets2: TATPointPairArray;
      const AMarkers, AMarkers2: TATMarkerMarkerArray;
      const AAttribs: TATMarkerAttribArray;
      ACommandCode: integer;
      const ATickCount: QWord;
      AArraysDisabled: boolean = false); virtual;
      //AArraysDisabled (2026.09, issue #385): True = created item has empty (nil)
      //carets/markers/attribs arrays with ItemArraysDisabled=True, passed arrays are
      //ignored. Used by bulk paths for all items except the 1st one of a bulk run.
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
      AUndoOrRedo: TATEditorRunningUndoOrRedo;
      AArraysDisabled: boolean = false);

    //2026.09.12 (CudaText perf): bulk version of Add() for the N identical
    //placeholder undo-items of a block-insert (LineBlockInsertEnds): one
    //TATEditAction.Insert item per line, same index, empty text, same arrays.
    //Creates exactly the items N sequential Add() calls would create (same
    //fields, same order); the tick is sampled every 1024 items - sequential
    //Add() samples per item, but items of one command differ only when a
    //pause >= ATStrings_PauseForUndoGroup occurs mid-run, which the periodic
    //sampling preserves (within 1024 items)
    procedure AddInsertRun(AIndex: integer; ACount: SizeInt;
      ACommandCode: integer;
      const ACarets, ACarets2: TATPointPairArray;
      const AMarkers, AMarkers2: TATMarkerMarkerArray;
      const AAttribs: TATMarkerAttribArray);

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
var
  S: UnicodeString;
  SCarets, SCarets2, SMarkers, SMarkers2: string;
  i: SizeInt;
begin
  S:= ItemText;

  //replace CR LF chars, to not corrupt undo-data saved to a file, then loaded from a file
  if SStringHasEol(S) then
    for i:= 1 to Length(S) do
      if (S[i]=#10) or (S[i]=#13) then
        S[i]:= ' ';

  if ItemArraysDisabled then
  begin
    SCarets:= ArraysDisabledSpec;
    SCarets2:= ArraysDisabledSpec;
    SMarkers:= ArraysDisabledSpec;
    SMarkers2:= ArraysDisabledSpec;
  end
  else
  begin
    SCarets:= PointPairArrayToString(ItemCarets);
    SCarets2:= PointPairArrayToString(ItemCarets2);
    SMarkers:= MarkerArrayToString(ItemMarkers);
    SMarkers2:= MarkerArrayToString(ItemMarkers2);
  end;

  Result:=
    IntToStr(Ord(ItemAction))+PartSep+
    IntToStr(ItemIndex)+PartSep+
    IntToStr(Ord(ItemEnd))+PartSep+
    IntToStr(Ord(ItemLineState))+PartSep+
    SCarets+MarkersSep+
      SMarkers+MarkersSep+
      IntToStr(ItemGlobalCounter)+MarkersSep+
      IntToStr(ItemTickCount)+MarkersSep+
      IntToStr(ItemCommandCode)+MarkersSep+
      SCarets2+MarkersSep+
      SMarkers2+PartSep+
    IntToStr(Ord(ItemSoftMark))+PartSep+
    IntToStr(Ord(ItemHardMark))+PartSep+
    UTF8Encode(S);
end;

procedure TATUndoItem.SetAsString(const AValue: string);
var
  Sep, Sep2: TATStringSeparator;
  S, SubItem: string;
  N: integer;
begin
  Sep.Init(AValue, PartSep);

  ItemArraysDisabled:= false;

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
  if SubItem=ArraysDisabledSpec then
  begin
    ItemCarets:= nil;
    ItemArraysDisabled:= true;
  end
  else
    StringToPointPairArray(ItemCarets, SubItem);
  //b) markers
  Sep2.GetItemStr(SubItem);
  if SubItem=ArraysDisabledSpec then
  begin
    ItemMarkers:= nil;
    ItemArraysDisabled:= true;
  end
  else
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
  if SubItem=ArraysDisabledSpec then
  begin
    ItemCarets2:= nil;
    ItemArraysDisabled:= true;
  end
  else
    StringToPointPairArray(ItemCarets2, SubItem);
  //g) markers2
  Sep2.GetItemStr(SubItem);
  if SubItem=ArraysDisabledSpec then
  begin
    ItemMarkers2:= nil;
    ItemArraysDisabled:= true;
  end
  else
  if SubItem<>'' then
    StringToMarkerArray(ItemMarkers2, SubItem)
  else
    ItemMarkers2:= nil;

  Sep.GetItemStr(S);
  ItemSoftMark:= S='1';

  Sep.GetItemStr(S);
  ItemHardMark:= S='1';

  //use Sep.GetRest for last item, because line can contain tab-chars
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
  ItemArraysDisabled:= D.ItemArraysDisabled;
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
  const ATickCount: QWord;
  AArraysDisabled: boolean);
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
  ItemArraysDisabled:= AArraysDisabled;

  if AArraysDisabled then
  begin
    //2026.09 (issue #385): all arrays must be empty in a disabled item;
    //passed arrays are ignored, so callers may pass any values
    ItemCarets:= nil;
    ItemCarets2:= nil;
    ItemMarkers:= nil;
    ItemMarkers2:= nil;
    ItemAttribs:= nil;
  end
  else
  {
  if AShareArrays then
  begin
    ItemCarets:= ACarets;
    ItemCarets2:= ACarets2;
    ItemMarkers:= AMarkers;
    ItemMarkers2:= AMarkers2;
    ItemAttribs:= AAttribs;
  end
  else
  }
  begin
    ItemCarets:= Copy(ACarets);
    ItemCarets2:= Copy(ACarets2);
    ItemMarkers:= Copy(AMarkers);
    ItemMarkers2:= Copy(AMarkers2);
    ItemAttribs:= Copy(AAttribs);
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
  AUndoOrRedo: TATEditorRunningUndoOrRedo;
  AArraysDisabled: boolean);
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
                            NewTick,
                            AArraysDisabled);
  Item.ItemGlobalCounter:= NGlobalCounter;

  FList.Add(Item);
  FSoftMark:= false;

  //support MaxCount _actions_ in the list, intead of MaxCount simple items
  //CudaText issue #3084
  while (NGlobalCounter-Items[0].ItemGlobalCounter)>MaxCount do
    Delete(0);
end;


procedure TATUndoList.AddInsertRun(AIndex: integer; ACount: SizeInt;
  ACommandCode: integer;
  const ACarets, ACarets2: TATPointPairArray;
  const AMarkers, AMarkers2: TATMarkerMarkerArray;
  const AAttribs: TATMarkerAttribArray);
{
2026.09.12 (CudaText perf): see the interface comment. Mirrors the statements
of Add() for the Insert-placeholder case (empty text, NotUndoRedo):
- command-mark counter: computed once (Add() reads Last.ItemGlobalCounter
  before each item, but items of this run share the counter, so the value
  stays the same);
- FSoftMark/FHardMark: read per item like Add() does; FSoftMark turns false
  after the first item, exactly like sequential Add() calls;
- GetTickCount64: sampled per 1024 items (see interface comment);
- duplicate-change check of Add() never applies (action=Insert);
- MaxCount trimming: run once at the end (all items share the counter).
}
var
  Item: TATUndoItem;
  NGlobalCounter: DWord;
  NCounterFirst, NCounterRest: DWord;
  NewTick: QWord;
  i: SizeInt;
begin
  if FLocked then Exit;
  if FMaxCount=0 then Exit;
  if ACount<=0 then Exit;

  //command-mark counter, mirroring sequential Add() exactly:
  //- list not empty: all items get Last.Counter (+1 when the mark is set,
  //  consumed by the first Add);
  //- list empty: item 1 gets 0 and does NOT consume the mark (Add() reads
  //  the counter only when bNotEmpty), item 2 consumes it -> items 2..N
  //  get 1 (0 when no mark)
  if Count>0 then
  begin
    NGlobalCounter:= Last.ItemGlobalCounter;
    if FNewCommandMark then
    begin
      FNewCommandMark:= false;
      Inc(NGlobalCounter);
    end;
    NCounterFirst:= NGlobalCounter;
    NCounterRest:= NGlobalCounter;
  end
  else
  begin
    NCounterFirst:= 0;
    if FNewCommandMark then
    begin
      FNewCommandMark:= false;
      NCounterRest:= 1;
    end
    else
      NCounterRest:= 0;
  end;

  if Capacity < Count+ACount then
    Capacity:= Count+ACount;

  NewTick:= GetTickCount64;
  if (FLastTick>0) and (NewTick-FLastTick>=ATStrings_PauseForUndoGroup) then
    FSoftMark:= true;
  FLastTick:= NewTick;

  for i:= 1 to ACount do
  begin
    if (i>1) and ((i and 1023)=0) then
    begin
      //periodic tick sampling: a long pause inside the loop must set the
      //soft mark, like per-item Add() would do
      NewTick:= GetTickCount64;
      if NewTick-FLastTick>=ATStrings_PauseForUndoGroup then
      begin
        FSoftMark:= true;
        FLastTick:= NewTick;
      end;
    end;
    if i=1 then
      NGlobalCounter:= NCounterFirst
    else
      NGlobalCounter:= NCounterRest;
    Item:= TATUndoItem.Create(TATEditAction.Insert, AIndex, '', TATLineEnds.None,
      TATLineState.None, FSoftMark, FHardMark,
      ACarets, ACarets2, AMarkers, AMarkers2, AAttribs,
      ACommandCode, NewTick,
      (i>1){AArraysDisabled, issue #385: real arrays are stored only in the 1st item of the run});
    Item.ItemGlobalCounter:= NGlobalCounter;
    FList.Add(Item);
    FSoftMark:= false;
  end;

  //support MaxCount _actions_ in the list, intead of MaxCount simple items
  //CudaText issue #3084
  if Count>0 then
    while (NGlobalCounter-Items[0].ItemGlobalCounter)>MaxCount do
      Delete(0);
end;


procedure TATUndoList.AddUnmodifiedMark;
var
  Item: TATUndoItem;
  TempCarets: TATPointPairArray = nil;
  TempMarkers: TATMarkerMarkerArray = nil;
  TempAttribs: TATMarkerAttribArray = nil;
begin
  ////on load file called with Locked=true
  //if FLocked then exit;

  //don't make two ClearModified items
  Item:= Last;
  if Assigned(Item) then
    if Item.ItemAction=TATEditAction.ClearModified then exit;

  Item:= TATUndoItem.Create(
    TATEditAction.ClearModified,
    0,
    '',
    TATLineEnds.None,
    TATLineState.None,
    false,
    false,
    TempCarets,
    TempCarets, //2nd carets
    TempMarkers,
    TempMarkers, //2nd markers
    TempAttribs,
    0,
    0,
    true);

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

