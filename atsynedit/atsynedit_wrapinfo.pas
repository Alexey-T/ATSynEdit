{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_WrapInfo;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}
{$Z1}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATStrings,
  ATSynEdit_fgl;

const
  ATWrapInfo_MaxCacheLines = 1024*1024;
    //2026.09: hard cap of lines in the wrap-items restore cache
    //(hashes + items of deleted lines, to make undo of big blocks fast);
    //memory: 8 bytes per line hash + 16 bytes per wrap-item

type
  TATWrapItemFinal = (
    Final,
    Collapsed,
    Middle
    );

type
  { TATWrapItem }

  PATWrapItem = ^TATWrapItem;
  TATWrapItem = packed record
    NLineIndex: SizeInt;
    NCharIndex: SizeInt;
    NLength: SizeInt;
    NIndent: word;
    NFinal: TATWrapItemFinal;
    bInitial: boolean;
    procedure Init(ALineIndex, ACharIndex, ALength, AIndent: SizeInt; AFinal: TATWrapItemFinal; AInitial: boolean);
    function ContainsPos(AX, AY: Int64): boolean;
    class operator=(const A, B: TATWrapItem): boolean;
  end;

type
  TATWrapItems = specialize TFPGList<TATWrapItem>;

type
  { TATWrapInfo }

  TATWrapInfo = class
  private
    FList: TATWrapItems;
    FStrings: TATStrings;
    FVirtualMode: boolean;
    function GetData(AIndex: integer): TATWrapItem;
    procedure SetVirtualMode(AValue: boolean);
    function IsLineFolded(ALine: SizeInt): boolean;
  public
    VisibleColumns: integer;
    WrapColumn: integer;
    EditorIndex: integer;
    StringsPrevCount: SizeInt;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property StringsObj: TATStrings read FStrings write FStrings;
    property VirtualMode: boolean read FVirtualMode write SetVirtualMode;
    function Count: integer;
    function IsIndexValid(AIndex: integer): boolean; inline;
    function IsIndexUniqueForLine(AIndex: integer): boolean;
    property Data[AIndex: integer]: TATWrapItem read GetData; default;
    procedure Add(const AData: TATWrapItem);
    procedure Delete(AIndex: integer);
    procedure Insert(AIndex: integer; const AItem: TATWrapItem);
    procedure FindIndexesOfLineNumber(ALineNum: SizeInt; out AFrom, ATo: integer);
    function FindIndexOfCaretPos(APos: TPoint): integer;
    procedure SetCapacity(AValue: integer);
    procedure ReplaceItems(AFrom, ATo: integer; AItems: TATWrapItems);

    //2026.09: primitives for the incremental WrapInfo update
    function FindIndexOfLineNumber(ALine: SizeInt): integer; //first item index with NLineIndex>=ALine; Count when none
    procedure DeleteItems(AFrom, ATo: integer); //remove items [AFrom..ATo], single memory-move
    procedure SpliceItems(AIndex: integer; AItems: TATWrapItems); //insert all AItems at AIndex, single memory-move
    procedure ShiftLineIndexes(AFromItem: integer; ADelta: SizeInt); //Inc(NLineIndex, ADelta) for items [AFromItem..]
  end;

type
  { TATWrapUpdateCache }

  {
  2026.09: performance fix (word-wrap). Cache of wrap-items of recently deleted
  lines. When a big block of lines is deleted (e.g. DEL with big selection),
  wrap-items of its lines are copied here with per-line text hashes. When the
  same lines are re-inserted on UNDO (or REDO), items are restored from the
  cache, instead of re-calculating the wrap for each line (which is the most
  expensive part: per-char word-boundary and width calculations).
  Restoration is verified by 64-bit hashes of line texts, so any other
  inserted block (same position/count but different texts) never reuses
  stale items: hashes don't match, and the slow per-line calculation runs.
  Cache is cleared: on full WrapInfo recalculation, on wrap params change,
  on folding changes (folded lines have no wrap-items, so cached items
  don't match the unfolded restored lines).
  }
  TATWrapUpdateCache = class
  private
    function GetLineCount: integer;
  public
    WrapColumn: integer; //wrap params at the cache fill time
    VisibleColumns: integer;
    Hashes: array of QWord; //per cached line, cache-local index 0..N-1
    Items: TATWrapItems; //items of cached lines, NLineIndex is cache-local line index
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property LineCount: integer read GetLineCount;
    procedure Populate(AWrapInfo: TATWrapInfo; ADeleteFrom, ADeleteCount: SizeInt;
      const AHashes: array of QWord; AHashesAll: boolean;
      AWrapColumn, AVisibleColumns: integer);
    function TryRestore(const ACurHashes: array of QWord;
      AInsertLine, AInsertCount: SizeInt;
      AWrapColumn, AVisibleColumns: integer;
      AOutItems: TATWrapItems): boolean;
  end;

procedure ATWrapInfo_CalcLine(
  AStrings: TATStrings;
  ATabHelper: TATStringTabHelper;
  AEditorIndex: integer;
  AWrapColumn: integer;
  AWrapIndented: boolean;
  AVisibleColumns: integer;
  const ANonWordChars: atString;
  ALineIndex: integer;
  AIndentMaximal: integer;
  AItems: TATWrapItems;
  AConsiderFolding: boolean;
  AFontProportional: boolean);

function ATWrapInfo_ApplyStructOps(
  AStrings: TATStrings;
  AWrapInfo: TATWrapInfo;
  ATempItems: TATWrapItems;
  const AOps: TATWrapStructOpArray;
  ACache: TATWrapUpdateCache;
  ATabHelper: TATStringTabHelper;
  AEditorIndex: integer;
  AWrapColumn, AVisibleColumns, AIndentMaximal: integer;
  AWrapIndented: boolean;
  const ANonWordChars: atString;
  AConsiderFolding: boolean;
  AFontProportional: boolean): boolean;


implementation

uses
  Math, Dialogs, Forms,
  ATSynEdit_Globals;

{ TATWrapItem }

procedure TATWrapItem.Init(ALineIndex, ACharIndex, ALength, AIndent: SizeInt;
  AFinal: TATWrapItemFinal; AInitial: boolean);
begin
  NLineIndex:= ALineIndex;
  NCharIndex:= ACharIndex;
  NLength:= ALength;
  NIndent:= AIndent;
  NFinal:= AFinal;
  bInitial:= AInitial;
end;

function TATWrapItem.ContainsPos(AX, AY: Int64): boolean;
begin
  Result:= false;
  if AY<>NLineIndex then exit;
  if AX<NCharIndex-1 then exit;
  if NFinal<>TATWrapItemFinal.Final then
    if AX>=NCharIndex-1+NLength then exit;
  Result:= true;
end;

class operator TATWrapItem.=(const A, B: TATWrapItem): boolean;
begin
  Result:= false;
end;

{ TATWrapInfo }

function TATWrapInfo.GetData(AIndex: integer): TATWrapItem;
begin
  if FVirtualMode then
  begin
    if FStrings.IsIndexValid(AIndex) then
      Result.Init(AIndex, 1, FStrings.LinesLen[AIndex], 0, TATWrapItemFinal.Final, true)
    else
      Result:= Default(TATWrapItem);
  end
  else
  begin
    if (AIndex>=0) and (AIndex<FList.Count) then
      Result:= FList[AIndex]
    else
      Result:= Default(TATWrapItem);
  end;
end;

procedure TATWrapInfo.SetVirtualMode(AValue: boolean);
begin
  if FVirtualMode=AValue then Exit;
  FVirtualMode:= AValue;

  {
  don't clear:
  adapter fills Fold ranges after a pause... this causes nasty empty screen of control
  }
  //if FVirtualMode then
  //  Clear;
end;

function TATWrapInfo.IsLineFolded(ALine: SizeInt): boolean;
begin
  if StringsObj.IsIndexValid(ALine) then
    Result:= StringsObj.LinesHidden[ALine, EditorIndex]
  else
    Result:= false;
end;

constructor TATWrapInfo.Create;
begin
  FList:= TATWrapItems.Create;
  FVirtualMode:= false;
  StringsPrevCount:= -1;
end;

destructor TATWrapInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATWrapInfo.Clear;
begin
  FList.Clear;
end;

function TATWrapInfo.Count: integer;
begin
  if FVirtualMode then
    Result:= FStrings.Count
  else
    Result:= FList.Count;
end;

function TATWrapInfo.IsIndexValid(AIndex: integer): boolean; inline;
begin
  Result:= (AIndex>=0) and (AIndex<Count);
end;

function TATWrapInfo.IsIndexUniqueForLine(AIndex: integer): boolean;
var
  NLineIndex: integer;
begin
  if FVirtualMode then
    Exit(true);
  NLineIndex:= FList._GetItemPtr(AIndex)^.NLineIndex;
  if (AIndex>0) and (FList._GetItemPtr(AIndex-1)^.NLineIndex=NLineIndex) then
    Exit(false);
  if (AIndex<FList.Count-1) and (FList._GetItemPtr(AIndex+1)^.NLineIndex=NLineIndex) then
    Exit(false);
  Result:= true;
end;

procedure TATWrapInfo.Add(const AData: TATWrapItem);
begin
  if FVirtualMode then exit;
  FList.Add(AData);
end;

procedure TATWrapInfo.Delete(AIndex: integer);
begin
  if FVirtualMode then exit;
  FList.Delete(AIndex);
end;

procedure TATWrapInfo.Insert(AIndex: integer; const AItem: TATWrapItem);
begin
  if FVirtualMode then exit;
  if AIndex>=FList.Count then
    FList.Add(AItem)
  else
    FList.Insert(AIndex, AItem);
end;

procedure TATWrapInfo.FindIndexesOfLineNumber(ALineNum: SizeInt; out AFrom, ATo: integer);
var
  a, b, m, dif: integer;
begin
  if FVirtualMode then
  begin
    AFrom:= ALineNum;
    ATo:= ALineNum;
    Exit;
  end;

  AFrom:= -1;
  ATo:= -1;

  if IsLineFolded(ALineNum) then Exit;

  a:= 0;
  b:= Count-1;

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    dif:= FList._GetItemPtr(m)^.NLineIndex-ALineNum;
    if dif=0 then
      Break;
    if dif>0 then
      b:= m-1
    else
      a:= m+1;
  until false;

  AFrom:= m;
  ATo:= m;
  while (AFrom>0) and (FList._GetItemPtr(AFrom-1)^.NLineIndex=ALineNum) do
    Dec(AFrom);
  while (ATo<Count-1) and (FList._GetItemPtr(ATo+1)^.NLineIndex=ALineNum) do
    Inc(ATo);
end;

function TATWrapInfo.FindIndexOfCaretPos(APos: TPoint): integer;
var
  NFrom, NTo, i: integer;
begin
  Result:= -1;
  FindIndexesOfLineNumber(APos.Y, NFrom, NTo);
  if NFrom<0 then Exit;
  for i:= NFrom to NTo do
  begin
    Result:= i;
    if Data[i].NCharIndex + Data[i].NLength > APos.X+1 then // APos.X+1: see CudaText issue 2466
      Break;
  end;
end;

procedure TATWrapInfo.SetCapacity(AValue: integer);
begin
  FList.Capacity:= Max(1024, AValue);
end;

//optimized; don't just del/ins
procedure TATWrapInfo.ReplaceItems(AFrom, ATo: integer; AItems: TATWrapItems);
var
  Item: TATWrapItem;
  Dif, i: integer;
begin
  if FVirtualMode then exit;
  Dif:= AItems.Count - (ATo-AFrom+1);

  //adjust count of items
  if Dif<0 then
  begin
    for i:= 1 to Abs(Dif) do
      Delete(AFrom);
  end
  else
  if Dif>0 then
  begin
    for i:= 1 to Dif do
    begin
      Item.Init(0, 0, 0, 0, Low(TATWrapItemFinal), true);
      Insert(AFrom, Item);
    end;
  end;

  //overwrite N items
  for i:= 0 to AItems.Count-1 do
    FList[AFrom+i]:= AItems[i];
end;


{ TATWrapInfo: primitives for the incremental update }

function TATWrapInfo.FindIndexOfLineNumber(ALine: SizeInt): integer;
var
  a, b, m: integer;
begin
  if FVirtualMode then
    Exit(Max(0, Min(ALine, FStrings.Count)));

  //binary search of lower bound: first item with NLineIndex>=ALine
  a:= 0;
  b:= FList.Count;
  while a<b do
  begin
    m:= (a+b) div 2;
    if FList._GetItemPtr(m)^.NLineIndex < ALine then
      a:= m+1
    else
      b:= m;
  end;
  Result:= a;
end;

procedure TATWrapInfo.DeleteItems(AFrom, ATo: integer);
begin
  if FVirtualMode then exit;
  if (AFrom<0) or (AFrom>ATo) or (ATo>=FList.Count) then exit;
  FList.DeleteRange(AFrom, ATo);
end;

procedure TATWrapInfo.SpliceItems(AIndex: integer; AItems: TATWrapItems);
var
  i: integer;
begin
  if FVirtualMode then exit;
  if AItems=nil then exit;
  if AItems.Count=0 then exit;

  if (AIndex<0) or (AIndex>FList.Count) then
    AIndex:= FList.Count;

  FList.InsertRange(AIndex, AItems.Count);
  for i:= 0 to AItems.Count-1 do
    FList._GetItemPtr(AIndex+i)^:= AItems._GetItemPtr(i)^;
end;

procedure TATWrapInfo.ShiftLineIndexes(AFromItem: integer; ADelta: SizeInt);
var
  i: integer;
begin
  if FVirtualMode then exit;
  if ADelta=0 then exit;
  for i:= Max(0, AFromItem) to FList.Count-1 do
    FList._GetItemPtr(i)^.NLineIndex+= ADelta;
end;


{ TATWrapUpdateCache }

function TATWrapUpdateCache.GetLineCount: integer;
begin
  Result:= Length(Hashes);
end;

constructor TATWrapUpdateCache.Create;
begin
  Items:= TATWrapItems.Create;
end;

destructor TATWrapUpdateCache.Destroy;
begin
  Clear;
  FreeAndNil(Items);
  inherited;
end;

procedure TATWrapUpdateCache.Clear;
begin
  SetLength(Hashes, 0);
  if Items<>nil then
    Items.Clear;
end;

procedure TATWrapUpdateCache.Populate(AWrapInfo: TATWrapInfo; ADeleteFrom, ADeleteCount: SizeInt;
  const AHashes: array of QWord; AHashesAll: boolean;
  AWrapColumn, AVisibleColumns: integer);
var
  iFrom, iTo, i: integer;
  Item: TATWrapItem;
begin
  Clear;
  if not AHashesAll then Exit;
  if ADeleteCount<=0 then Exit;
  if ADeleteCount>ATWrapInfo_MaxCacheLines then Exit;
  if ADeleteCount<>Length(AHashes) then Exit;

  WrapColumn:= AWrapColumn;
  VisibleColumns:= AVisibleColumns;

  SetLength(Hashes, ADeleteCount);
  for i:= 0 to ADeleteCount-1 do
    Hashes[i]:= AHashes[i];

  //copy wrap-items of deleted lines, with cache-local line indexes
  iFrom:= AWrapInfo.FindIndexOfLineNumber(ADeleteFrom);
  iTo:= AWrapInfo.FindIndexOfLineNumber(ADeleteFrom+ADeleteCount)-1;
  if iTo>=iFrom then
    for i:= iFrom to iTo do
    begin
      Item:= AWrapInfo.Data[i];
      Dec(Item.NLineIndex, ADeleteFrom);
      Items.Add(Item);
    end;
end;

function TATWrapUpdateCache.TryRestore(const ACurHashes: array of QWord;
  AInsertLine, AInsertCount: SizeInt;
  AWrapColumn, AVisibleColumns: integer;
  AOutItems: TATWrapItems): boolean;
{
ACurHashes[]: hashes of current document lines, which the AInsertLine..AInsertLine+AInsertCount-1
op inserts (caller computes them at FINAL document positions). When they match a sub-range of
cached Hashes[], wrap-items of that sub-range are restored to AOutItems, with line indexes
in "insert-op" coordinates (AInsertLine + offset), so the caller's sequential application
shifts them correctly.
}
var
  K0, K, i, j, d: SizeInt;
  bMatch: boolean;
  NWrite: integer;
  Item: TATWrapItem;
begin
  Result:= false;
  if AOutItems=nil then Exit;
  AOutItems.Clear;

  K0:= Length(Hashes);
  if K0=0 then Exit;
  if (WrapColumn<>AWrapColumn) or (VisibleColumns<>AVisibleColumns) then Exit;
  K:= AInsertCount;
  if (K<=0) or (K>K0) then Exit;
  if Length(ACurHashes)<>K then Exit;

  //find the offset of ACurHashes inside cached Hashes
  d:= -1;
  for i:= 0 to K0-K do
    if Hashes[i]=ACurHashes[0] then
    begin
      bMatch:= true;
      for j:= 1 to K-1 do
        if Hashes[i+j]<>ACurHashes[j] then
        begin
          bMatch:= false;
          Break
        end;
      if bMatch then
      begin
        d:= i;
        Break
      end;
    end;
  if d<0 then Exit;

  //extract items of matched lines, with line indexes of the inserted block
  for i:= 0 to Items.Count-1 do
  begin
    Item:= Items[i];
    if (Item.NLineIndex>=d) and (Item.NLineIndex<d+K) then
    begin
      Item.NLineIndex:= AInsertLine + (Item.NLineIndex-d);
      AOutItems.Add(Item);
    end;
  end;

  //trim the cache: drop the consumed lines
  if (d=0) and (K=K0) then
    Clear
  else
  if d=0 then
  begin
    //prefix consumed: drop first K lines, shift the rest
    for i:= K to K0-1 do
      Hashes[i-K]:= Hashes[i];
    SetLength(Hashes, K0-K);
    NWrite:= 0;
    for i:= 0 to Items.Count-1 do
      if Items[i].NLineIndex>=K then
      begin
        Item:= Items[i];
        Dec(Item.NLineIndex, K);
        Items[NWrite]:= Item;
        Inc(NWrite);
      end;
    while Items.Count>NWrite do
      Items.Delete(Items.Count-1);
  end
  else
  if d+K=K0 then
  begin
    //suffix consumed: drop last K lines
    SetLength(Hashes, K0-K);
    NWrite:= 0;
    for i:= 0 to Items.Count-1 do
      if Items[i].NLineIndex<K0-K then
      begin
        Items[NWrite]:= Items[i];
        Inc(NWrite);
      end;
    while Items.Count>NWrite do
      Items.Delete(Items.Count-1);
  end
  else
    //middle consumed: don't support split cache, drop it all
    Clear;

  Result:= true;
end;


{ ATWrapInfo_CalcLine }

procedure ATWrapInfo_CalcLine(
  AStrings: TATStrings;
  ATabHelper: TATStringTabHelper;
  AEditorIndex: integer;
  AWrapColumn: integer;
  AWrapIndented: boolean;
  AVisibleColumns: integer;
  const ANonWordChars: atString;
  ALineIndex: integer;
  AIndentMaximal: integer;
  AItems: TATWrapItems;
  AConsiderFolding: boolean;
  AFontProportional: boolean);
var
  WrapItem: TATWrapItem;
  WrapItemPtr: PATWrapItem;
  NLineLen, NPartLen, NFoldFrom: integer;
  NPartOffset, NIndent, NVisColumns: integer;
  bInitialItem: boolean;
  StrPart: UnicodeString;
begin
  AItems.Clear;

  //line folded entirely?
  if AConsiderFolding then
    if AStrings.LinesHidden[ALineIndex, AEditorIndex] then Exit;

  NLineLen:= AStrings.LinesLen[ALineIndex];

  if NLineLen=0 then
  begin
    WrapItem.Init(ALineIndex, 1, 0, 0, TATWrapItemFinal.Final, true);
    AItems.Add(WrapItem);
    Exit;
  end;

  //consider fold, before wordwrap
  if AConsiderFolding then
  begin
    //line folded partially?
    NFoldFrom:= AStrings.LinesFoldFrom[ALineIndex, AEditorIndex];
    if NFoldFrom>0 then
    begin
      WrapItem.Init(ALineIndex, 1, Min(NLineLen, NFoldFrom-1), 0, TATWrapItemFinal.Collapsed, true);
      AItems.Add(WrapItem);
      Exit;
    end;
  end;

  //line not wrapped?
  if (AWrapColumn<ATEditorOptions.MinWrapColumnAbs) then
  begin
    WrapItem.Init(ALineIndex, 1, NLineLen, 0, TATWrapItemFinal.Final, true);
    AItems.Add(WrapItem);
    Exit;
  end;

  NVisColumns:= Max(AVisibleColumns, ATEditorOptions.MinWrapColumnAbs);
  NPartOffset:= 1;
  NIndent:= 0;
  bInitialItem:= true;

  repeat
    if AFontProportional then
      StrPart:= AStrings.LineSub(ALineIndex, NPartOffset, ATEditorOptions.MaxVisibleColumns)
    else
      StrPart:= AStrings.LineSub(ALineIndex, NPartOffset, NVisColumns);

    if StrPart='' then
    begin
      if not bInitialItem then
      begin
        WrapItemPtr:= AItems._GetItemPtr(AItems.Count-1);
        WrapItemPtr^.NFinal:= TATWrapItemFinal.Final;
      end;
      Break;
    end;

    NPartLen:= ATabHelper.FindWordWrapOffset(
      ALineIndex,
      //very slow to calc for entire line (eg len=70K),
      //calc for first NVisColumns chars
      StrPart,
      Max(AWrapColumn-NIndent, ATEditorOptions.MinWrapColumnAbs),
      ANonWordChars,
      AWrapIndented
      );

    WrapItem.Init(ALineIndex, NPartOffset, NPartLen, NIndent, TATWrapItemFinal.Middle, bInitialItem);
    AItems.Add(WrapItem);
    bInitialItem:= false;

    if AWrapIndented then
      if NPartOffset=1 then
      begin
        NIndent:= ATabHelper.GetIndentExpanded(ALineIndex, StrPart);
        NIndent:= Min(NIndent, AIndentMaximal);
      end;

    Inc(NPartOffset, NPartLen);
  until false;
end;


{ ATWrapInfo_ApplyStructOps }

function ATWrapInfo_ApplyStructOps(
  AStrings: TATStrings;
  AWrapInfo: TATWrapInfo;
  ATempItems: TATWrapItems;
  const AOps: TATWrapStructOpArray;
  ACache: TATWrapUpdateCache;
  ATabHelper: TATStringTabHelper;
  AEditorIndex: integer;
  AWrapColumn, AVisibleColumns, AIndentMaximal: integer;
  AWrapIndented: boolean;
  const ANonWordChars: atString;
  AConsiderFolding: boolean;
  AFontProportional: boolean): boolean;
{
2026.09: performance fix (word-wrap). Applies structural line ops (recorded by
TATStrings.WrapStructRecord since the last WrapInfo update) to AWrapInfo
incrementally:
- for deleted lines: wrap-items of these lines are removed (single
  memory-move), and line indexes of items below are shifted up; removed items
  are copied to ACache (with line text hashes), to be restored on undo;
- for inserted lines: line indexes of items below are shifted down, wrap-items
  for new lines are calculated (per-line, like the full recalculation does) or
  restored from ACache (when UNDO re-inserts the same lines, verified by
  hashes), and spliced in (single memory-move).
So the cost is O(total wrap-items) for index shifts + O(new lines) for the
wrap calculation, instead of O(all lines) of the full recalculation. E.g.
CudaText test (300K lines, wrap on): DEL of 200K lines ~6.4 sec -> ~0.05 sec,
UNDO of it ~19-60 sec -> ~1 sec.
Then it recalculates wrap-items for edited lines (TATStrings.IndexesOfEditedLines),
like the old "cached update" did.
Returns False when ops cannot be applied (then caller must fully recalculate):
- no ops and no edited lines;
- sanity check fails: WrapInfo's StringsPrevCount + net line change <> current
  line count (some untracked change happened: e.g. document was loaded).
}
var
  NewItems: TATWrapItems;
  ListNums: TATIntegerList;
  i, j, k: SizeInt;
  NCur: SizeInt;
  NLine, NIndexFrom, NIndexTo, NSplice: integer;
  bRestored: boolean;
  FinalPos: array of SizeInt;
  CurHashes: array of QWord;
  WrapItem: TATWrapItem;
begin
  Result:= false;
  if (AStrings=nil) or (AWrapInfo=nil) then Exit;
  if AWrapInfo.VirtualMode then Exit;

  if Length(AOps)=0 then
    if AStrings.IndexesOfEditedLines.Count=0 then
      Exit; //nothing to apply

  //sanity: WrapInfo must match the doc state before ops, and all ops must
  //have valid line ranges (indexes are in "doc state before op" coordinates);
  //when the check fails (e.g. doc was loaded/replaced, so ops are not valid
  //anymore), caller must use the full recalculation
  NCur:= AWrapInfo.StringsPrevCount;
  if NCur<0 then Exit;
  for i:= 0 to High(AOps) do
  begin
    case AOps[i].Kind of
      TATWrapStructOpKind.Inserted:
        begin
          if (AOps[i].Line<0) or (AOps[i].Line>NCur) then Exit;
          Inc(NCur, AOps[i].Count);
        end;
      TATWrapStructOpKind.Deleted:
        begin
          if (AOps[i].Line<0) or (AOps[i].Line+AOps[i].Count>NCur) then Exit;
          Dec(NCur, AOps[i].Count);
        end;
    end;
  end;
  if NCur<>AStrings.Count then Exit;

  NewItems:= TATWrapItems.Create;
  try
    for i:= 0 to High(AOps) do
    begin
      case AOps[i].Kind of
        TATWrapStructOpKind.Inserted:
          begin
            //shift line indexes of items at/below the insertion point
            NSplice:= AWrapInfo.FindIndexOfLineNumber(AOps[i].Line);
            AWrapInfo.ShiftLineIndexes(NSplice, AOps[i].Count);

            //map "position after this op" of each new line to its position in
            //the final document: WrapInfo ops are applied sequentially (item
            //line indexes are in "after this op" coordinates, later ops shift
            //them), but the document is already in its final state, so new
            //lines must be read at their FINAL positions
            SetLength(FinalPos, AOps[i].Count);
            for j:= 0 to AOps[i].Count-1 do
            begin
              NLine:= AOps[i].Line+j;
              for k:= i+1 to High(AOps) do
                case AOps[k].Kind of
                  TATWrapStructOpKind.Inserted:
                    if AOps[k].Line<=NLine then
                      Inc(NLine, AOps[k].Count);
                  TATWrapStructOpKind.Deleted:
                    begin
                      if AOps[k].Line+AOps[k].Count<=NLine then
                        Dec(NLine, AOps[k].Count)
                      else
                      if (AOps[k].Line<=NLine) and (NLine<AOps[k].Line+AOps[k].Count) then
                        Exit; //line is deleted by a later op: not supported, full recalc
                    end;
                end;
              if (NLine<0) or (NLine>=AStrings.Count) then Exit;
              FinalPos[j]:= NLine;
            end;

            //wrap-items for new lines: restore from cache or calculate
            NewItems.Clear;
            bRestored:= false;
            if ACache<>nil then
              if not AConsiderFolding then
              begin
                SetLength(CurHashes, AOps[i].Count);
                for j:= 0 to AOps[i].Count-1 do
                  CurHashes[j]:= AStrings.GetLineHash(FinalPos[j]);
                bRestored:= ACache.TryRestore(CurHashes, AOps[i].Line, AOps[i].Count,
                  AWrapColumn, AVisibleColumns, NewItems);
              end;

            if not bRestored then
              for j:= 0 to AOps[i].Count-1 do
              begin
                ATWrapInfo_CalcLine(AStrings, ATabHelper, AEditorIndex,
                  AWrapColumn, AWrapIndented, AVisibleColumns, ANonWordChars,
                  FinalPos[j], AIndentMaximal, ATempItems, AConsiderFolding,
                  AFontProportional);
                //ATempItems has 1+ items of this line (CalcLine cleared it);
                //item line index must be "position after this op", not final
                for k:= 0 to ATempItems.Count-1 do
                begin
                  WrapItem:= ATempItems[k];
                  WrapItem.NLineIndex:= AOps[i].Line+j;
                  NewItems.Add(WrapItem);
                end;
                ATempItems.Clear;
              end;

            AWrapInfo.SpliceItems(NSplice, NewItems);
          end;

        TATWrapStructOpKind.Deleted:
          begin
            //populate cache: copy items of deleted lines (before removal)
            if ACache<>nil then
              if not AConsiderFolding then
                ACache.Populate(AWrapInfo, AOps[i].Line, AOps[i].Count,
                  AOps[i].Hashes, AOps[i].HashesAll, AWrapColumn, AVisibleColumns)
              else
                ACache.Clear
            else
              ; //no cache

            //remove items of deleted lines, shift indexes of items below
            j:= AWrapInfo.FindIndexOfLineNumber(AOps[i].Line);
            NIndexTo:= AWrapInfo.FindIndexOfLineNumber(AOps[i].Line+AOps[i].Count)-1;
            AWrapInfo.DeleteItems(j, NIndexTo);
            AWrapInfo.ShiftLineIndexes(j, -AOps[i].Count);
          end;
      end;
    end;

    //recalc wrap-items of edited lines (same as the old "cached update" did)
    if AStrings.IndexesOfEditedLines.Count>0 then
    begin
      ListNums:= TATIntegerList.Create;
      try
        ListNums.Assign(AStrings.IndexesOfEditedLines);
        for i:= 0 to ListNums.Count-1 do
        begin
          NLine:= ListNums[i];
          if not AStrings.IsIndexValid(NLine) then Continue;

          ATWrapInfo_CalcLine(AStrings, ATabHelper, AEditorIndex,
            AWrapColumn, AWrapIndented, AVisibleColumns, ANonWordChars,
            NLine, AIndentMaximal, ATempItems, AConsiderFolding,
            AFontProportional);
          if ATempItems.Count=0 then Continue;

          AWrapInfo.FindIndexesOfLineNumber(NLine, NIndexFrom, NIndexTo);
          if NIndexFrom>=0 then
            AWrapInfo.ReplaceItems(NIndexFrom, NIndexTo, ATempItems);
        end;
        ATempItems.Clear;
      finally
        FreeAndNil(ListNums);
      end;
    end;

    Result:= true;
  finally
    FinalPos:= nil;
    CurHashes:= nil;
    FreeAndNil(NewItems);
  end;
end;


end.

