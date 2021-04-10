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
  { TATSynRange }

  PATSynRange = ^TATSynRange;
  TATSynRange = packed record
    Tag: Int64;
    Hint: string[95];
    X: integer; //start column
    Y: integer; //start line
    Y2: integer; //end line which is fully folded (can't partially fold)
    Folded: boolean;
    Staple: boolean;
    procedure Init(AX, AY, AY2: integer; AStaple: boolean; const AHint: string; const ATag: Int64);
    function IsSimple: boolean;
    function IsLineInside(ALine: integer): boolean;
    function MessageText: string;
    class operator =(const a, b: TATSynRange): boolean;
  end;

  { TATSynRangeList }

  TATSynRangeList = class(specialize TFPGList<TATSynRange>)
  public
    function ItemPtr(AIndex: integer): PATSynRange; inline;
  end;

type
  { TATSynRanges }

  TATSynRanges = class
  private
    FList: TATSynRangeList;
    FLineIndexer: array of array of integer;
    FHasTagPersist: boolean;
    FHasStaples: boolean;
    procedure AddToLineIndexer(ALine1, ALine2, AIndex: integer); inline;
    function GetItems(Index: integer): TATSynRange;
    procedure SetItems(Index: integer; const AValue: TATSynRange);
    //function MessageTextForIndexList(const L: TATIntArray): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Count: integer; inline;
    function CountOfLineIndexer: integer;
    function IsIndexValid(N: integer): boolean; inline;
    function IsRangesTouch(N1, N2: integer): boolean;
    function Add(AX, AY, AY2: integer; AWithStaple: boolean; const AHint: string;
      const ATag: Int64=0): TATSynRange;
    function Insert(AIndex: integer; AX, AY, AY2: integer; AWithStaple: boolean;
      const AHint: string; const ATag: Int64=0): TATSynRange;
    procedure Clear;
    procedure ClearLineIndexer(ALineCount: integer; ASetLenOnly: boolean=false);
    procedure Delete(AIndex: integer);
    procedure DeleteAllByTag(const ATag: Int64);
    procedure DeleteAllExceptTag(const ATag: Int64);
    property Items[Index: integer]: TATSynRange read GetItems write SetItems; default;
    function ItemPtr(AIndex: integer): PATSynRange;
    function IsRangeInsideOther(R1, R2: PATSynRange): boolean;
    function IsRangesSame(R1, R2: PATSynRange): boolean;
    function FindRanges(AOuterRange: integer; AOnlyFolded, ATopLevelOnly: boolean): TATIntArray;
    function FindRangesWithLine(ALine: integer; AOnlyFolded: boolean): TATIntArray;
    function FindRangesWithAnyOfLines(ALineFrom, ALineTo: integer): TATIntArray;
    function FindRangesWithStaples(ALineFrom, ALineTo: integer): TATIntArray;
    function FindDeepestRangeContainingLine_Old(ALine: integer; const AIndexes: TATIntArray): integer;
    function FindDeepestRangeContainingLine(ALine: integer; AWithStaple: boolean): integer;
    function FindRangeWithPlusAtLine(ALine: integer): integer;
    function FindRangeWithPlusAtLine_ViaIndexer(ALine: integer): integer;
    function FindRangeLevel(AIndex: integer): integer;
    function MessageText(AMaxCount: integer): string;
    function MessageLineIndexer(AMaxCount: integer): string;
    procedure UpdateLineIndexer;
    procedure Update(AChange: TATLineChangeKind; ALineIndex, AItemCount: integer);
    property HasTagPersist: boolean read FHasTagPersist;
    property HasStaples: boolean read FHasStaples;
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

{ TATSynRangeList }

function TATSynRangeList.ItemPtr(AIndex: integer): PATSynRange;
begin
  Result:= PATSynRange(InternalGet(AIndex));
end;


{ TATSynRange }

procedure TATSynRange.Init(AX, AY, AY2: integer; AStaple: boolean;
  const AHint: string; const ATag: Int64);
begin
  if (AX<=0) then raise Exception.Create('Incorrect range with x<=0: '+MessageText);
  if (AY<0) then raise Exception.Create('Incorrect range with y<0: '+MessageText);
  //if (AY>AY2) then raise Exception.Create('Incorrect range with y>y2: '+MessageText);
  if (AY>AY2) then AY2:= AY; //hide this error, it happens in Rexx lexer

  X:= AX;
  Y:= AY;
  Y2:= AY2;
  Staple:= AStaple;
  Hint:= AHint;
  Folded:= false;
  Tag:= ATag;
end;

function TATSynRange.IsSimple: boolean; inline;
begin
  Result:= Y=Y2;
end;

function TATSynRange.IsLineInside(ALine: integer): boolean; inline;
begin
  Result:= (ALine>=Y) and (ALine<=Y2);
end;

function TATSynRange.MessageText: string;
begin
  Result:= Format('%d..%d', [Y+1, Y2+1]);
end;

class operator TATSynRange.=(const a, b: TATSynRange): boolean;
begin
  Result:= false;
end;

{ TATSynRanges }

function TATSynRanges.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATSynRanges.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATSynRanges.CountOfLineIndexer: integer;
begin
  Result:= Length(FLineIndexer);
end;

function TATSynRanges.GetItems(Index: integer): TATSynRange;
begin
  Result:= FList[Index];
end;

procedure TATSynRanges.SetItems(Index: integer; const AValue: TATSynRange);
begin
  FList[Index]:= AValue;
  if AValue.Tag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
  if AValue.Staple then
    FHasStaples:= true;
end;

procedure TATSynRanges.ClearLineIndexer(ALineCount: integer; ASetLenOnly: boolean=false);
begin
  if not ASetLenOnly then
    {
    for i:= High(FLineIndexer) downto 0 do
      SetLength(FLineIndexer[i], 0);
    }
    SetLength(FLineIndexer, 0); //it frees old memory? seems yes

  SetLength(FLineIndexer, ALineCount);
end;

constructor TATSynRanges.Create;
begin
  FList:= TATSynRangeList.Create;
  FList.Capacity:= 2*1024;
  FHasTagPersist:= false;
  FHasStaples:= false;
end;

destructor TATSynRanges.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynRanges.Clear;
begin
  ClearLineIndexer(0);
  FList.Clear;
  FHasTagPersist:= false;
  FHasStaples:= false;
end;

function TATSynRanges.Add(AX, AY, AY2: integer; AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATSynRange;
var
  NIndex: integer;
begin
  Result.Init(AX, AY, AY2, AWithStaple, AHint, ATag);
  NIndex:= FList.Add(Result);

  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
  if AWithStaple then
    FHasStaples:= true;

  AddToLineIndexer(AY, AY2, NIndex);
end;

procedure TATSynRanges.AddToLineIndexer(ALine1, ALine2, AIndex: integer);
var
  NItemLen, i: integer;
begin
  if ALine1<>ALine2 then //skip one-line ranges
    if ALine2<=High(FLineIndexer) then
      for i:= ALine1 to ALine2 do
      begin
        NItemLen:= Length(FLineIndexer[i]);
        SetLength(FLineIndexer[i], NItemLen+1);
        FLineIndexer[i][NItemLen]:= AIndex;
      end;
end;

function TATSynRanges.Insert(AIndex: integer; AX, AY, AY2: integer;
  AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATSynRange;
begin
  Result.Init(AX, AY, AY2, AWithStaple, AHint, ATag);
  FList.Insert(AIndex, Result);

  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
  if AWithStaple then
    FHasStaples:= true;

  UpdateLineIndexer;
end;

procedure TATSynRanges.Delete(AIndex: integer); inline;
begin
  FList.Delete(AIndex);
  UpdateLineIndexer;
end;

procedure TATSynRanges.DeleteAllByTag(const ATag: Int64);
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    if ItemPtr(i)^.Tag=ATag then
      FList.Delete(i);

  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= false;

  UpdateLineIndexer;
end;

procedure TATSynRanges.DeleteAllExceptTag(const ATag: Int64);
var
  TempList: TATSynRangeList;
  i: integer;
begin
  TempList:= TATSynRangeList.Create;
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

function TATSynRanges.ItemPtr(AIndex: integer): PATSynRange;
begin
  Result:= FList.ItemPtr(AIndex);
end;

function TATSynRanges.IsRangeInsideOther(R1, R2: PATSynRange): boolean;
begin
  Result:=
    IsPosSorted(R2^.X, R2^.Y, R1^.X, R1^.Y, true)
    and (R1^.Y2-cAllowHangoutLines<=R2^.Y2);
end;

function TATSynRanges.IsRangesSame(R1, R2: PATSynRange): boolean;
begin
  if R1=R2 then
    exit(true);
  if (R1^.X=R2^.X) and (R1^.Y=R2^.Y) and (Abs(R1^.Y2-R2^.Y2)<=cAllowHangoutLines) then
    exit(true);

  Result:= false;
end;

function TATSynRanges.FindRangeLevel(AIndex: integer): integer;
var
  NLine, iItem: integer;
begin
  Result:= 0;
  NLine:= ItemPtr(AIndex)^.Y;
  if NLine>High(FLineIndexer) then exit;

  for iItem:= 0 to High(FLineIndexer[NLine]) do
    if FLineIndexer[NLine][iItem] = AIndex then
    begin
      Result:= iItem;
      Break;
    end;

  //first in LineIndexer item? then level 0
  if Result=0 then
    exit;

  //skip previous ranges in the same LineIndexer line,
  //if they only touch our range
  while Result>0 do
  begin
    iItem:= FLineIndexer[NLine][Result-1];
    if IsRangesTouch(iItem, AIndex) then
      Dec(Result);
  end;
end;

function TATSynRanges.IsRangesTouch(N1, N2: integer): boolean;
begin
  Result:= ItemPtr(N1)^.Y2 = ItemPtr(N2)^.Y;
end;

type
  TATIntegerList = specialize TFPGList<integer>;

function TATSynRanges.FindRanges(AOuterRange: integer; AOnlyFolded,
  ATopLevelOnly: boolean): TATIntArray;
//ATopLevel: keep from collected list only top-level ranges
//(not globally top-level, but top-level inside found list)
var
  L: TATIntegerList;
  R, RngOuter, RngLastAdded: PATSynRange;
  nStartIndex, nEndLine, i: integer;
begin
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

  SetLength(Result{%H-}, 0);
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

function TATSynRanges.FindRangesWithLine(ALine: integer; AOnlyFolded: boolean): TATIntArray;
var
  R: PATSynRange;
  NLen, NRange, i: integer;
begin
  SetLength(Result{%H-}, 0);
  if ALine>High(FLineIndexer) then exit;

  if not AOnlyFolded then
    Result:= FLineIndexer[ALine]
  else
  begin
    NLen:= 0;
    for i:= 0 to High(FLineIndexer[ALine]) do
    begin
      NRange:= FLineIndexer[ALine][i];
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

function TATSynRanges.FindRangesWithAnyOfLines(ALineFrom, ALineTo: integer): TATIntArray;
var
  NMax, NRange, iLine, iItem: integer;
begin
  SetLength(Result{%H-}, 0);
  NMax:= High(FLineIndexer);
  if ALineFrom>NMax then exit;
  if ALineTo>NMax then ALineTo:= NMax;

  for iLine:= ALineFrom to ALineTo do
    for iItem:= 0 to High(FLineIndexer[iLine]) do
    begin
      NRange:= FLineIndexer[iLine][iItem];
      if not _IsArrayItemPresent(Result, NRange) then
      begin
        SetLength(Result, Length(Result)+1);
        Result[High(Result)]:= NRange;
      end;
    end;
end;

function TATSynRanges.FindRangesWithStaples(ALineFrom, ALineTo: integer): TATIntArray;
var
  NMax, NRange, iLine, iItem: integer;
  Rng: PATSynRange;
begin
  SetLength(Result{%H-}, 0);
  NMax:= High(FLineIndexer);
  if ALineFrom>NMax then exit;
  if ALineTo>NMax then ALineTo:= NMax;

  for iLine:= ALineFrom to ALineTo do
    for iItem:= 0 to High(FLineIndexer[iLine]) do
    begin
      NRange:= FLineIndexer[iLine][iItem];
      Rng:= ItemPtr(NRange);
      if not Rng^.Staple then Continue;
      if Rng^.Folded then Continue;
      if _IsArrayItemPresent(Result, NRange) then Continue;
      SetLength(Result, Length(Result)+1);
      Result[High(Result)]:= NRange;
    end;
end;


function TATSynRanges.FindDeepestRangeContainingLine_Old(ALine: integer;
  const AIndexes: TATIntArray): integer;
var
  R: PATSynRange;
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

function TATSynRanges.FindDeepestRangeContainingLine(ALine: integer; AWithStaple: boolean): integer;
var
  NItemLen, NRange, iItem: integer;
  Ptr: PATSynRange;
begin
  Result:= -1;
  if ALine>High(FLineIndexer) then exit;

  NItemLen:= Length(FLineIndexer[ALine]);
  for iItem:= NItemLen-1 downto 0 do
  begin
    NRange:= FLineIndexer[ALine][iItem];
    if not IsIndexValid(NRange) then Continue;
    Ptr:= ItemPtr(NRange);
    if Ptr^.IsSimple then
      Continue;
    if AWithStaple and not Ptr^.Staple then
      Continue;
    exit(NRange);
  end;
end;


function TATSynRanges.FindRangeWithPlusAtLine_ViaIndexer(ALine: integer): integer;
var
  NItemLen, NRange, iItem: integer;
  Ptr: PATSynRange;
begin
  Result:= -1;
  if ALine>High(FLineIndexer) then exit;

  NItemLen:= Length(FLineIndexer[ALine]);
  for iItem:= 0 to NItemLen-1 do
  begin
    NRange:= FLineIndexer[ALine][iItem];
    Ptr:= ItemPtr(NRange);
    if Ptr^.Y=ALine then
      if not Ptr^.IsSimple then
        exit(NRange);
  end;
end;

function TATSynRanges.FindRangeWithPlusAtLine(ALine: integer): integer;
// issue https://github.com/Alexey-T/CudaText/issues/2566
// because of this, we must skip all one-line ranges
var
  a, b, m, dif, NCount: integer;
  R: PATSynRange;
begin
  Result:= -1;
  NCount:= Count;
  a:= 0;
  b:= NCount-1;

  repeat
    if a>b then exit; //not Break
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

function TATSynRanges.MessageText(AMaxCount: integer): string;
var
  Ptr: PATSynRange;
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(Count-1, AMaxCount) do
  begin
    Ptr:= ItemPtr(i);
    Result+= Ptr^.MessageText+#10;
  end;
end;

procedure TATSynRanges.Update(AChange: TATLineChangeKind; ALineIndex, AItemCount: integer);
var
  Rng: PATSynRange;
  i: integer;
begin
  case AChange of
    cLineChangeDeletedAll:
      Clear;

    cLineChangeDeleted:
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

    cLineChangeAdded:
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

  UpdateLineIndexer;
end;

procedure TATSynRanges.UpdateLineIndexer;
var
  Ptr: PATSynRange;
  i: integer;
begin
  ClearLineIndexer(Length(FLineIndexer));
  for i:= 0 to FList.Count-1 do
  begin
    Ptr:= ItemPtr(i);
    AddToLineIndexer(Ptr^.Y, Ptr^.Y2, i);
  end;
end;

function TATSynRanges.MessageLineIndexer(AMaxCount: integer): string;
var
  S: string;
  i, iLine: integer;
begin
  Result:= '';
  for iLine:= 0 to Min(High(FLineIndexer), AMaxCount) do
  begin
    S:= IntToStr(iLine)+': ';
    for i:= 0 to High(FLineIndexer[iLine]) do
      S+= IntToStr(FLineIndexer[iLine][i])+' ';
    Result+= S+#10;
  end;
end;


end.

