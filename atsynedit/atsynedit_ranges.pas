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
  TATRangeHasLines = (
    cRngIgnore,
    cRngHasAllLines,
    cRngHasAnyOfLines,
    cRngExceptThisRange
    );

type
  { TATSynRanges }

  TATSynRanges = class
  private
    FList: TATSynRangeList;
    FLineIndexer: array of array of integer;
    FHasTagPersist: boolean;
    function GetItems(Index: integer): TATSynRange;
    procedure SetItems(Index: integer; const AValue: TATSynRange);
    //function MessageTextForIndexList(const L: TATIntArray): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    function Add(AX, AY, AY2: integer; AWithStaple: boolean; const AHint: string;
      const ATag: Int64=0): TATSynRange;
    function Insert(Index: integer; AX, AY, AY2: integer; AWithStaple: boolean;
      const AHint: string; const ATag: Int64=0): TATSynRange;
    procedure Clear;
    procedure ClearLineIndexer(ALineCount: integer);
    procedure Delete(Index: integer);
    procedure DeleteAllByTag(const ATag: Int64);
    procedure DeleteAllExceptTag(const ATag: Int64);
    property Items[Index: integer]: TATSynRange read GetItems write SetItems; default;
    function ItemPtr(AIndex: integer): PATSynRange;
    function IsRangeInsideOther(R1, R2: PATSynRange): boolean;
    function IsRangesSame(R1, R2: PATSynRange): boolean;
    function FindRangesContainingLines(ALineFrom, ALineTo: integer;
      AInRangeIndex: integer; AOnlyFolded, ATopLevelOnly: boolean;
      ALineMode: TATRangeHasLines): TATIntArray;
    function FindDeepestRangeContainingLine(ALine: integer; const AIndexes: TATIntArray): integer;
    function FindRangeWithPlusAtLine(ALine: integer): integer;
    function MessageText(Cnt: integer): string;
    procedure Update(AChange: TATLineChangeKind; ALineIndex, AItemCount: integer);
    property HasTagPersist: boolean read FHasTagPersist;
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

function TATSynRanges.GetItems(Index: integer): TATSynRange;
begin
  Result:= FList[Index];
end;

procedure TATSynRanges.SetItems(Index: integer; const AValue: TATSynRange);
begin
  FList[Index]:= AValue;
  if AValue.Tag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
end;

procedure TATSynRanges.ClearLineIndexer(ALineCount: integer);
var
  i: integer;
begin
  for i:= High(FLineIndexer) downto 0 do
    SetLength(FLineIndexer[i], 0);
  SetLength(FLineIndexer, ALineCount);
end;

constructor TATSynRanges.Create;
begin
  FList:= TATSynRangeList.Create;
  FList.Capacity:= 2*1024;
  FHasTagPersist:= false;
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
end;

function TATSynRanges.Add(AX, AY, AY2: integer; AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATSynRange;
begin
  Result.Init(AX, AY, AY2, AWithStaple, AHint, ATag);
  FList.Add(Result);
  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
end;

function TATSynRanges.Insert(Index: integer; AX, AY, AY2: integer;
  AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATSynRange;
begin
  Result.Init(AX, AY, AY2, AWithStaple, AHint, ATag);
  FList.Insert(Index, Result);
  if ATag=cTagPersistentFoldRange then
    FHasTagPersist:= true;
end;

procedure TATSynRanges.Delete(Index: integer); inline;
begin
  FList.Delete(Index);
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

type
  TATIntegerList = specialize TFPGList<integer>;

function TATSynRanges.FindRangesContainingLines(ALineFrom, ALineTo: integer;
  AInRangeIndex: integer; AOnlyFolded, ATopLevelOnly: boolean;
  ALineMode: TATRangeHasLines): TATIntArray;
var
  L: TATIntegerList;
  R, RTest: PATSynRange;
  i, j: integer;
  Ok: boolean;
begin
  SetLength(Result, 0);
  L:= TATIntegerList.Create;
  L.Capacity:= 128;
  try
    for i:= 0 to Count-1 do
    begin
      R:= FList.ItemPtr(i);
      if (not R^.IsSimple) then
        if (not AOnlyFolded or R^.Folded) then
        begin
          case ALineMode of
            cRngIgnore: Ok:= true;
            cRngHasAllLines: Ok:= (R^.Y<=ALineFrom) and (R^.Y2>=ALineTo);
            cRngHasAnyOfLines: Ok:= (R^.Y<=ALineTo) and (R^.Y2>=ALineFrom);
            cRngExceptThisRange: Ok:= i<>AInRangeIndex;
            else raise Exception.Create('unknown LineMode');
          end;
          if not Ok then Continue;

          if AInRangeIndex<0 then
            Ok:= true
          else
          begin
            RTest:= FList.ItemPtr(AInRangeIndex);
            Ok:= not IsRangesSame(RTest, R) and IsRangeInsideOther(R, RTest);
          end;

          if Ok then
            L.Add(i);
        end;
    end;

    if ATopLevelOnly then
    begin
      for i:= L.Count-1 downto 1 do
        for j:= 0 to i-1 do
          if IsRangeInsideOther(FList.ItemPtr(L[i]), FList.ItemPtr(L[j])) then
          begin
            L.Delete(i);
            Break
          end;
    end;

    SetLength(Result, L.Count);
    for i:= 0 to L.Count-1 do
      Result[i]:= L[i];
  finally
    FreeAndNil(L);
  end;
end;


function TATSynRanges.FindDeepestRangeContainingLine(ALine: integer;
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

function TATSynRanges.MessageText(Cnt: integer): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(Count-1, Cnt) do
    Result:= Result+FList.ItemPtr(i)^.MessageText+#10;
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
end;

(*
function TATSynRanges.MessageTextForIndexList(const L: TATIntArray): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Length(L)-1 do
    Result:= Result+Items[L[i]].MessageText+#10;
end;
*)

end.

