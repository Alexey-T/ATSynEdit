{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Ranges;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATSynEdit_FGL;

type
  TATFoldHintLong = string[95];
  TATFoldHintTrimmed = string[21];

type
  { TATFoldIndexItem }

  PATFoldIndexItem = ^TATFoldIndexItem;
  TATFoldIndexItem = packed record
    Len: SmallInt; //must be signed; if Word, then need typecast to Integer when we use 'Len-1'
    Data: array[0..Pred(19)] of Word; //about 20 levels: enough for most real cases
    function Add(AValue: Word): boolean;
    function Delete(AValue: Word): boolean;
    function Find(AValue: Word): integer;
    procedure Adjust(AValue: Word); inline;
    function MessageText: string;
  end;

type
  { TATFoldRange }

  PATFoldRange = ^TATFoldRange;
  TATFoldRange = packed record
    Tag: Int64;
    X: integer; //start column
    Y: integer; //start line
    X2: integer; //ending column
    Y2: integer; //ending line, which is fully folded (can't partially fold)
    Folded: boolean;
    Staple: boolean;
    Hint: TATFoldHintLong;
    procedure Init(AX, AY, AX2, AY2: integer; AStaple: boolean; const AHint: string; const ATag: Int64);
    function IsSimple: boolean;
    function IsLineInside(ALine: integer): boolean;
    function IsHintSame(const AOtherHint: TATFoldHintTrimmed): boolean;
    function MessageText: string;
    class operator =(const a, b: TATFoldRange): boolean;
  end;

  { TATFoldRangeList }

  TATFoldRangeList = class(specialize TFPGList<TATFoldRange>)
  public
    function ItemPtr(AIndex: integer): PATFoldRange; inline;
  end;

  { TATFoldIndexer }

  TATFoldIndexer = packed array of TATFoldIndexItem;

  { TATFoldedSaves }

  PATFoldedSave = ^TATFoldedSave;
  TATFoldedSave = record
    LineIndex: integer;
    HintTrimmed: TATFoldHintTrimmed;
    class operator=(constref a, b: TATFoldedSave): boolean;
  end;

  TATFoldedSaves = specialize TFPGList<TATFoldedSave>;

type
  { TATFoldRanges }

  TATFoldRanges = class
  private const
    MaxCount = $FFFF-4;
  private
    FList: TATFoldRangeList;
    FListPersist: TATFoldRangeList; //for BackupPersistentRanges/RestorePersistentRanges
    FListFoldedSaves: TATFoldedSaves;
    FLineIndexer: TATFoldIndexer;
    FHasTagPersist: boolean;
    FHasStaples: boolean;
    procedure AddToLineIndexer(ALine1, ALine2, AIndex: integer);
    procedure AdjustLineIndexerForInsertion(ARangeIndex: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Count: integer; inline;
    function CountOfLineIndexer: integer;
    function IsIndexValid(N: integer): boolean; inline;
    function IsRangesTouch(N1, N2: integer): boolean;
    function Add(AX, AY, AX2, AY2: integer; AWithStaple: boolean; const AHint: string;
      const ATag: Int64=0): TATFoldRange;
    function AddSorted(AX, AY, AX2, AY2: integer; AWithStaple: boolean;
      const AHint: string; const ATag: Int64;
      out AItemIndex: integer): TATFoldRange;
    function Insert(AIndex: integer; AX, AY, AX2, AY2: integer; AWithStaple: boolean;
      const AHint: string; const ATag: Int64=0): TATFoldRange;
    //procedure Merge(AX, AY, AX2, AY2: integer; const AHint: string; const ATag: Int64);
    procedure Clear;
    procedure ClearLineIndexer(ALineCount: integer; ASetLenOnly: boolean=false);
    procedure AssignList(AList: TATFoldRangeList);
    procedure Delete(AIndex: integer);
    procedure DeleteAllByTag(const ATag: Int64);
    //procedure DeleteAllExceptTag(const ATag: Int64);
    function ItemPtr(AIndex: integer): PATFoldRange;
    function IsRangeInsideOther(R1, R2: PATFoldRange): boolean;
    function IsRangesSame(R1, R2: PATFoldRange): boolean;
    function FindRanges(AOuterRange: integer; AOnlyFolded, ATopLevelOnly: boolean): TATIntArray;
    function FindRangesWithLine(ALine: integer; AOnlyFolded: boolean): TATIntArray;
    function FindRangesWithAnyOfLines(ALineFrom, ALineTo: integer): TATIntArray;
    function FindRangesWithStaples(ALineFrom, ALineTo: integer): TATIntArray;
    function FindDeepestRangeContainingLine_Old(ALine: integer; const AIndexes: TATIntArray): integer;
    function FindDeepestRangeContainingLine(ALine: integer; AWithStaple: boolean; AMinimalRangeHeight: integer): integer;
    function FindRangeWithPlusAtLine(ALine: integer; AReturnInsertPos: boolean=false): integer;
    function FindRangeWithPlusAtLine_ViaIndexer(ALine: integer): integer;
    function FindRangeLevel(AIndex: integer): integer;
    function MessageText(AMaxCount: integer): string;
    function MessageLineIndexer(AMaxCount: integer): string;
    procedure UpdateLineIndexer;
    procedure Update(AChange: TATLineChangeKind; ALineIndex, AItemCount: integer);
    property HasTagPersist: boolean read FHasTagPersist;
    property HasStaples: boolean read FHasStaples;
    procedure BackupPersistentRanges;
    procedure RestorePersistentRanges;
    procedure BackupFoldedStates;
    procedure RestoreFoldedStates;
  end;

const
  cTagPersistentFoldRange = -1;

implementation

uses
  Math,
  ATSynEdit_Carets;

//we allow one block to hangout 1 line by Y2 from outer block:
//it's needed for Pascal econtrol lexer
//(don't know why it gives such blocks)
const
  cAllowHangoutLines = 1; //0 or 1, do not bigger

{ TATFoldIndexItem }

function TATFoldIndexItem.Add(AValue: Word): boolean;
begin
  Result:= Len<Length(Data);
  if Result then
  begin
    Data[Len]:= AValue;
    Inc(Len);
  end;
end;

function TATFoldIndexItem.Delete(AValue: Word): boolean;
var
  i, j: integer;
begin
  Result:= false;
  for i:= 0 to Len-1 do
    if Data[i]=AValue then
    begin
      for j:= i to Len-2 do
        Data[j]:= Data[j+1];
      Dec(Len);
      Result:= true;
      Break;
    end;
end;

function TATFoldIndexItem.Find(AValue: Word): integer;
var
  i: integer;
begin
  for i:= 0 to Len-1 do
    if Data[i]=AValue then
      exit(i);
  Result:= -1;
end;

procedure TATFoldIndexItem.Adjust(AValue: Word);
var
  i: integer;
begin
  for i:= 0 to Len-1 do
    if Data[i]>=AValue then
      Inc(Data[i]);
end;

function TATFoldIndexItem.MessageText: string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Len-1 do
    Result+= IntToStr(Data[i])+' ';
end;


{ TATFoldRangeList }

function TATFoldRangeList.ItemPtr(AIndex: integer): PATFoldRange;
begin
  Result:= PATFoldRange(InternalGet(AIndex));
end;

{ TATFoldedSave }

class operator TATFoldedSave.=(constref a, b: TATFoldedSave): boolean;
begin
  Result:= false;
end;


{ TATFoldRange }

procedure TATFoldRange.Init(AX, AY, AX2, AY2: integer; AStaple: boolean;
  const AHint: string; const ATag: Int64);
begin
  if (AX<=0) then
    raise Exception.Create('Incorrect range with x<=0: '+MessageText);
  if (AY<0) then
    raise Exception.Create('Incorrect range with y<0: '+MessageText);
  //if (AY>AY2) then
  //  raise Exception.Create('Incorrect range with y>y2: '+MessageText);
  if (AY>AY2) then
    AY2:= AY; //hide this error, it happens in Rexx lexer

  X:= AX;
  Y:= AY;
  X2:= AX2;
  Y2:= AY2;
  Staple:= AStaple;
  Hint:= AHint;
  Folded:= false;
  Tag:= ATag;
end;

function TATFoldRange.IsSimple: boolean; inline;
//ranges of only 2 lines are needed sometimes, e.g. in FindInFiles lexer
begin
  Result:= Y=Y2;
end;

function TATFoldRange.IsLineInside(ALine: integer): boolean; inline;
begin
  Result:= (ALine>=Y) and (ALine<=Y2);
end;

function TATFoldRange.IsHintSame(const AOtherHint: TATFoldHintTrimmed): boolean;
const
  cMaxLenCompare = 20;
var
  NLenOld, NLenNew: integer;
begin
  NLenOld:= Min(cMaxLenCompare, Length(Hint));
  NLenNew:= Min(cMaxLenCompare, Length(AOtherHint));
  Result:= (NLenOld=NLenNew) and (StrLComp(@Hint[1], @AOtherHint[1], NLenOld)=0);
end;

function TATFoldRange.MessageText: string;
begin
  Result:= Format('%d..%d', [Y+1, Y2+1]);
end;

class operator TATFoldRange.=(const a, b: TATFoldRange): boolean;
begin
  Result:= false;
end;

{ TATFoldRanges }

function TATFoldRanges.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATFoldRanges.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATFoldRanges.CountOfLineIndexer: integer;
begin
  Result:= Length(FLineIndexer);
end;

procedure TATFoldRanges.ClearLineIndexer(ALineCount: integer; ASetLenOnly: boolean=false);
var
  i: integer;
begin
  if not ASetLenOnly then
    for i:= 0 to High(FLineIndexer) do
      FLineIndexer[i].Len:= 0;

  if Length(FLineIndexer)<>ALineCount then
    SetLength(FLineIndexer, ALineCount);
end;

procedure TATFoldRanges.AssignList(AList: TATFoldRangeList);
begin
  FList.Assign(AList);
end;

constructor TATFoldRanges.Create;
begin
  FList:= TATFoldRangeList.Create;
  FList.Capacity:= 2*1024;
  FListPersist:= TATFoldRangeList.Create;
  FLineIndexer:= nil;
  FHasTagPersist:= false;
  FHasStaples:= false;
end;

destructor TATFoldRanges.Destroy;
begin
  FListPersist.Clear;
  Clear;
  FLineIndexer:= nil;
  if Assigned(FListFoldedSaves) then
    FreeAndNil(FListFoldedSaves);
  FreeAndNil(FListPersist);
  FreeAndNil(FList);
  inherited;
end;

procedure TATFoldRanges.Clear;
begin
  FLineIndexer:= nil;
  FList.Clear;
  //don't clear FListFoldedSaves
  FHasTagPersist:= false;
  FHasStaples:= false;
end;

function TATFoldRanges.Add(AX, AY, AX2, AY2: integer; AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATFoldRange;
var
  NIndex: integer;
begin
  if Count>=MaxCount then exit;

  Result.Init(AX, AY, AX2, AY2, AWithStaple, AHint, ATag);
  NIndex:= FList.Add(Result);

  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
  if AWithStaple then
    FHasStaples:= true;

  AddToLineIndexer(AY, AY2, NIndex);
end;

function TATFoldRanges.AddSorted(AX, AY, AX2, AY2: integer; AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64;
  out AItemIndex: integer): TATFoldRange;
var
  Item: PATFoldRange;
  i: integer;
begin
  if Count>=MaxCount then exit;

  AItemIndex:= -1;
  Result.Init(AX, AY, AX2, AY2, AWithStaple, AHint, ATag);

  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    if Item^.Y>=AY then
    begin
      AItemIndex:= i;
      Break;
    end;
  end;

  if AItemIndex>=0 then
  begin
    FList.Insert(AItemIndex, Result);
    AdjustLineIndexerForInsertion(AItemIndex);
  end
  else
    AItemIndex:= FList.Add(Result);

  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
  if AWithStaple then
    FHasStaples:= true;

  AddToLineIndexer(AY, AY2, AItemIndex);
end;

procedure TATFoldRanges.AddToLineIndexer(ALine1, ALine2, AIndex: integer);
var
  i: integer;
begin
  if ALine1<>ALine2 then //skip one-line ranges
    if ALine2<=High(FLineIndexer) then
      for i:= ALine1 to ALine2 do
        FLineIndexer[i].Add(AIndex);
end;

procedure TATFoldRanges.AdjustLineIndexerForInsertion(ARangeIndex: integer);
var
  i: integer;
begin
  for i:= 0 to High(FLineIndexer) do
    FLineIndexer[i].Adjust(ARangeIndex);
end;

function TATFoldRanges.Insert(AIndex: integer; AX, AY, AX2, AY2: integer;
  AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATFoldRange;
begin
  if Count>=MaxCount then exit;

  Result.Init(AX, AY, AX2, AY2, AWithStaple, AHint, ATag);
  FList.Insert(AIndex, Result);

  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
  if AWithStaple then
    FHasStaples:= true;

  UpdateLineIndexer;
end;

procedure TATFoldRanges.Delete(AIndex: integer); inline;
begin
  FList.Delete(AIndex);
  UpdateLineIndexer;
end;

procedure TATFoldRanges.DeleteAllByTag(const ATag: Int64);
var
  i: integer;
  bChanged: boolean;
begin
  bChanged:= false;
  for i:= FList.Count-1 downto 0 do
    if ItemPtr(i)^.Tag=ATag then
    begin
      FList.Delete(i);
      bChanged:= true;
    end;

  if bChanged then
  begin
    if ATag=cTagPersistentFoldRange then
      FHasTagPersist:= false;

    UpdateLineIndexer;
  end;
end;

{
procedure TATFoldRanges.DeleteAllExceptTag(const ATag: Int64);
var
  TempList: TATFoldRangeList;
  i: integer;
begin
  TempList:= TATFoldRangeList.Create;
  try
    for i:= 0 to FList.Count-1 do
      if ItemPtr(i)^.Tag=ATag then
        TempList.Add(ItemPtr(i)^);
    FList.Clear;
    for i:= 0 to TempList.Count-1 do
      FList.Add(TempList.ItemPtr(i)^);
  finally
    FreeAndNil(TempList);
  end;

  if ATag<>cTagPersistentFoldRange then
    FHasTagPersist:= false;

  UpdateLineIndexer;
end;
}

procedure TATFoldRanges.BackupPersistentRanges;
var
  Item: PATFoldRange;
  i: integer;
begin
  FListPersist.Clear;

  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    if Item^.Tag=cTagPersistentFoldRange then
      FListPersist.Add(Item^);
  end;
end;

procedure TATFoldRanges.RestorePersistentRanges;
var
  ItemFrom: PATFoldRange;
  NItemIndex, i: integer;
begin
  for i:= 0 to FListPersist.Count-1 do
  begin
    ItemFrom:= FListPersist.ItemPtr(i);
    AddSorted(
      ItemFrom^.X,
      ItemFrom^.Y,
      ItemFrom^.X2,
      ItemFrom^.Y2,
      ItemFrom^.Staple,
      ItemFrom^.Hint,
      ItemFrom^.Tag,
      NItemIndex
      );
    ItemPtr(NItemIndex)^.Folded:= ItemFrom^.Folded;

    FHasTagPersist:= true;
    if ItemFrom^.Staple then
      FHasStaples:= true;
  end;

  FListPersist.Clear;
end;

function TATFoldRanges.ItemPtr(AIndex: integer): PATFoldRange;
begin
  Result:= FList.ItemPtr(AIndex);
end;

function TATFoldRanges.IsRangeInsideOther(R1, R2: PATFoldRange): boolean;
begin
  Result:=
    IsPosSorted(R2^.X, R2^.Y, R1^.X, R1^.Y, true)
    and (R1^.Y2-cAllowHangoutLines<=R2^.Y2);
end;

function TATFoldRanges.IsRangesSame(R1, R2: PATFoldRange): boolean;
begin
  if R1=R2 then
    exit(true);
  if (R1^.X=R2^.X) and (R1^.Y=R2^.Y) and (Abs(R1^.Y2-R2^.Y2)<=cAllowHangoutLines) then
    exit(true);

  Result:= false;
end;

function TATFoldRanges.FindRangeLevel(AIndex: integer): integer;
var
  NLine, iItem: integer;
begin
  Result:= 0;
  NLine:= ItemPtr(AIndex)^.Y;
  if NLine>High(FLineIndexer) then exit;

  iItem:= FLineIndexer[NLine].Find(AIndex);
  if iItem>=0 then
    Result:= iItem;

  //first in LineIndexer item? then level 0
  if Result=0 then
    exit;

  //skip previous ranges in the same LineIndexer line,
  //if they only touch our range
  while Result>0 do
  begin
    iItem:= FLineIndexer[NLine].Data[Result-1];
    if IsRangesTouch(iItem, AIndex) then
      Dec(Result);
  end;
end;

function TATFoldRanges.IsRangesTouch(N1, N2: integer): boolean;
begin
  Result:= ItemPtr(N1)^.Y2 = ItemPtr(N2)^.Y;
end;

type
  TATIntegerList = specialize TFPGList<integer>;

function TATFoldRanges.FindRanges(AOuterRange: integer; AOnlyFolded,
  ATopLevelOnly: boolean): TATIntArray;
//ATopLevel: keep from collected list only top-level ranges
//(not globally top-level, but top-level inside found list)
var
  L: TATIntegerList;
  R, RngOuter, RngLastAdded: PATFoldRange;
  nStartIndex, nEndLine, i: integer;
begin
  Result:= nil;
  RngOuter:= nil;
  RngLastAdded:= nil;
  nStartIndex:= 0;
  nEndLine:= -1;

  if AOuterRange>=0 then
  begin
    RngOuter:= FList.ItemPtr(AOuterRange);
    nStartIndex:= AOuterRange+1;
    nEndLine:= RngOuter^.Y2;
  end;

  L:= TATIntegerList.Create;
  L.Capacity:= 128;

  try
    for i:= nStartIndex to FList.Count-1 do
    begin
      R:= FList.ItemPtr(i);
      if R^.IsSimple then
        Continue;
      if AOnlyFolded and not R^.Folded then
        Continue;

      //break loop after outer-range ending line
      if nEndLine>=0 then
        if R^.Y>=nEndLine then
          Break;

      if ATopLevelOnly then
        if Assigned(RngLastAdded) then
          if IsRangeInsideOther(R, RngLastAdded) then
            Continue;

      L.Add(i);
      RngLastAdded:= R;
    end;

    SetLength(Result, L.Count);
    for i:= 0 to L.Count-1 do
      Result[i]:= L[i];
  finally
    FreeAndNil(L);
  end;
end;

function TATFoldRanges.FindRangesWithLine(ALine: integer; AOnlyFolded: boolean): TATIntArray;
var
  R: PATFoldRange;
  NLen, NRange, i: integer;
begin
  Result:= nil;
  if ALine>High(FLineIndexer) then exit;

  if not AOnlyFolded then
  begin
    SetLength(Result, FLineIndexer[ALine].Len);
    for i:= 0 to Length(Result)-1 do
      Result[i]:= FLineIndexer[ALine].Data[i];
  end
  else
  begin
    NLen:= 0;
    for i:= 0 to FLineIndexer[ALine].Len-1 do
    begin
      NRange:= FLineIndexer[ALine].Data[i];
      R:= ItemPtr(NRange);
      if R^.Folded then
      begin
        Inc(NLen);
        SetLength(Result, NLen);
        Result[NLen-1]:= NRange;
      end;
    end;
  end;
end;

function _IsArrayItemPresent(var Ar: TATIntArray; Value: integer): boolean; inline;
var
  i: integer;
begin
  for i:= 0 to High(Ar) do
    if Ar[i]=Value then
      exit(true);
  Result:= false;
end;

function TATFoldRanges.FindRangesWithAnyOfLines(ALineFrom, ALineTo: integer): TATIntArray;
var
  NMax, NRange, iLine, iItem: integer;
begin
  Result:= nil;
  NMax:= High(FLineIndexer);
  if ALineFrom>NMax then exit;
  if ALineTo>NMax then ALineTo:= NMax;

  for iLine:= ALineFrom to ALineTo do
    for iItem:= 0 to FLineIndexer[iLine].Len-1 do
    begin
      NRange:= FLineIndexer[iLine].Data[iItem];
      if not _IsArrayItemPresent(Result, NRange) then
      begin
        SetLength(Result, Length(Result)+1);
        Result[High(Result)]:= NRange;
      end;
    end;
end;

function TATFoldRanges.FindRangesWithStaples(ALineFrom, ALineTo: integer): TATIntArray;
var
  NMax, NRange, iLine, iItem: integer;
  Rng: PATFoldRange;
begin
  Result:= nil;
  NMax:= High(FLineIndexer);
  if ALineFrom>NMax then exit;
  if ALineTo>NMax then ALineTo:= NMax;

  for iLine:= ALineFrom to ALineTo do
    for iItem:= 0 to FLineIndexer[iLine].Len-1 do
    begin
      NRange:= FLineIndexer[iLine].Data[iItem];
      Rng:= ItemPtr(NRange);
      if not Rng^.Staple then Continue;
      if Rng^.Folded then Continue;
      if _IsArrayItemPresent(Result, NRange) then Continue;
      SetLength(Result, Length(Result)+1);
      Result[High(Result)]:= NRange;
    end;
end;


function TATFoldRanges.FindDeepestRangeContainingLine_Old(ALine: integer;
  const AIndexes: TATIntArray): integer;
var
  R: PATFoldRange;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to High(AIndexes) do
  begin
    R:= FList.ItemPtr(AIndexes[i]);
    if R^.IsSimple then Continue;
    if (R^.Y>ALine) then Break;
    if (R^.Y2<ALine) then Continue;
    if (Result<0) or (R^.Y>FList.ItemPtr(Result)^.Y) then
      Result:= AIndexes[i];
  end;
end;

function TATFoldRanges.FindDeepestRangeContainingLine(ALine: integer; AWithStaple: boolean; AMinimalRangeHeight: integer): integer;
var
  NItemLen, NRange, iItem: integer;
  Ptr: PATFoldRange;
begin
  Result:= -1;
  if ALine<0 then exit;
  if ALine>High(FLineIndexer) then exit;

  NItemLen:= FLineIndexer[ALine].Len;
  for iItem:= NItemLen-1 downto 0 do
  begin
    NRange:= FLineIndexer[ALine].Data[iItem];
    if not IsIndexValid(NRange) then Continue;
    Ptr:= ItemPtr(NRange);

    if Ptr^.IsSimple then
      Continue;

    if AWithStaple and not Ptr^.Staple then
      Continue;

    //skip small ranges, but don't skip _folded_ ranges; CudaText issue #4159
    if not Ptr^.Folded then
      if Ptr^.Y2-Ptr^.Y<AMinimalRangeHeight then
        Continue;

    exit(NRange);
  end;
end;


function TATFoldRanges.FindRangeWithPlusAtLine_ViaIndexer(ALine: integer): integer;
var
  NItemLen, NRange, iItem: integer;
  Ptr: PATFoldRange;
begin
  Result:= -1;
  if ALine>High(FLineIndexer) then exit;

  NItemLen:= FLineIndexer[ALine].Len;
  for iItem:= 0 to NItemLen-1 do
  begin
    NRange:= FLineIndexer[ALine].Data[iItem];
    Ptr:= ItemPtr(NRange);
    if Ptr^.Y=ALine then
      if not Ptr^.IsSimple then
        exit(NRange);
  end;
end;

function TATFoldRanges.FindRangeWithPlusAtLine(ALine: integer; AReturnInsertPos: boolean=false): integer;
// CudaText issue #2566
// because of this, we must skip all one-line ranges
var
  a, b, m, dif, NCount: integer;
  R: PATFoldRange;
begin
  Result:= -1;
  NCount:= Count;
  a:= 0;
  b:= NCount-1;

  repeat
    if a>b then
    begin
      if AReturnInsertPos and IsIndexValid(a) then
        Result:= a;
      exit;
    end;
    m:= (a+b+1) div 2;

    R:= FList.ItemPtr(m);
    dif:= R^.Y-ALine;

    if dif<0 then
      a:= m+1
    else
    if dif>0 then
      b:= m-1
    else
    begin
      //find _first_ range which begins at ALine
      while (m>0) and (FList.ItemPtr(m-1)^.Y=ALine) do
        Dec(m);
      Break;
    end;
  until false;

  //some range is found, now skip all one-line ranges
  while (m<NCount) and FList.ItemPtr(m)^.IsSimple do
    Inc(m);
  //if skipped not too far, it is the result
  if (m<NCount) and (FList.ItemPtr(m)^.Y=ALine) then
    Result:= m;
end;


function TATFoldRanges.MessageText(AMaxCount: integer): string;
var
  Ptr: PATFoldRange;
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(Count-1, AMaxCount) do
  begin
    Ptr:= ItemPtr(i);
    Result+= Ptr^.MessageText+#10;
  end;
end;

procedure TATFoldRanges.Update(AChange: TATLineChangeKind; ALineIndex, AItemCount: integer);
var
  Rng: PATFoldRange;
  i: integer;
begin
  case AChange of
    TATLineChangeKind.DeletedAll:
      Clear;

    TATLineChangeKind.Deleted:
      for i:= FList.Count-1 downto 0 do
        begin
          Rng:= FList.ItemPtr(i);
          if Rng^.Tag<>cTagPersistentFoldRange then Continue;

          if Rng^.Y>=ALineIndex+AItemCount then
          begin
            Rng^.Y-= AItemCount;
            Rng^.Y2-= AItemCount;
          end
          else
          if Rng^.Y>=ALineIndex then
          begin
            if Rng^.Y2<=ALineIndex+AItemCount then
              FList.Delete(i)
            else
            begin
              Rng^.Y:= Max(Rng^.Y-AItemCount, ALineIndex);
              Rng^.Y2-= AItemCount;
            end;
          end
          else
          if Rng^.Y2>=ALineIndex then
          begin
            Rng^.Y2:= ALineIndex;
          end;
        end;

    TATLineChangeKind.Added:
      for i:= FList.Count-1 downto 0 do
        begin
          Rng:= FList.ItemPtr(i);
          if Rng^.Tag<>cTagPersistentFoldRange then Continue;

          if Rng^.Y>=ALineIndex then
          begin
            Rng^.Y+= AItemCount;
            Rng^.Y2+= AItemCount;
          end
          else
          if Rng^.Y2>=ALineIndex then
          begin
            Rng^.Y2+= AItemCount;
          end;
        end;
  end;
end;

procedure TATFoldRanges.UpdateLineIndexer;
var
  Ptr: PATFoldRange;
  i: integer;
begin
  ClearLineIndexer(Length(FLineIndexer));
  for i:= 0 to FList.Count-1 do
  begin
    Ptr:= ItemPtr(i);
    AddToLineIndexer(Ptr^.Y, Ptr^.Y2, i);
  end;
end;

function TATFoldRanges.MessageLineIndexer(AMaxCount: integer): string;
var
  S: string;
  iLine: integer;
begin
  Result:= '';
  for iLine:= 0 to Min(High(FLineIndexer), AMaxCount) do
  begin
    S:= IntToStr(iLine)+': '+FLineIndexer[iLine].MessageText+#10;
    Result+= S;
  end;
end;

(*
procedure TATFoldRanges.Merge(AX, AY, AX2, AY2: integer; const AHint: string; const ATag: Int64);
//try to find old fold-range for the AY line;
//if found, update the range (don't change it's Folded state) without inserting new one
var
  Item: PATFoldRange;
  NIndex: integer;
begin
  if Count>=MaxCount then exit;

  NIndex:= FindRangeWithPlusAtLine(AY, true);
  if NIndex>=0 then
  begin
    Item:= ItemPtr(NIndex);
    if (Item^.Y=AY) and Item^.IsHintSame(AHint) then
    begin
      Item^.X2:= AX2;
      Item^.Y2:= AY2;
      Item^.Tag:= ATag;
    end
    else
      Insert(NIndex, AX, AY, AX2, AY2, false, AHint, ATag);
  end
  else
    Add(AX, AY, AX2, AY2, false, AHint, ATag);
end;
*)

procedure TATFoldRanges.BackupFoldedStates;
var
  Item: PATFoldRange;
  SaveState: TATFoldedSave;
  i: integer;
begin
  if FListFoldedSaves=nil then
    FListFoldedSaves:= TATFoldedSaves.Create;
  FListFoldedSaves.Clear;

  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    if Item^.Folded then
    begin
      SaveState.LineIndex:= Item^.Y;
      SaveState.HintTrimmed:= Item^.Hint;
      FListFoldedSaves.Add(SaveState);
    end;
  end;
end;

procedure TATFoldRanges.RestoreFoldedStates;
var
  SavedItem: PATFoldedSave;
  Item: PATFoldRange;
  NIndex, i: integer;
begin
  if FListFoldedSaves=nil then exit;
  for i:= 0 to FListFoldedSaves.Count-1 do
  begin
    SavedItem:= FListFoldedSaves._GetItemPtr(i);
    NIndex:= FindRangeWithPlusAtLine(SavedItem^.LineIndex, true);
    if NIndex>=0 then
    begin
      Item:= ItemPtr(NIndex);
      if Item^.IsHintSame(SavedItem^.HintTrimmed) then
        Item^.Folded:= true;
    end;
  end;
end;


end.

