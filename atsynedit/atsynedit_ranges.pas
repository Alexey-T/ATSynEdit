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

  TATSynRangeList = specialize TFPGList<TATSynRange>;

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
    function GetItems(Index: integer): TATSynRange;
    procedure SetItems(Index: integer; const AValue: TATSynRange);
    function MessageTextForIndexList(L: TList): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    procedure Clear;
    function Add(AX, AY, AY2: integer; AWithStaple: boolean; const AHint: string;
      const ATag: Int64=0): TATSynRange;
    function Insert(Index: integer; AX, AY, AY2: integer; AWithStaple: boolean;
      const AHint: string; const ATag: Int64=0): TATSynRange;
    procedure Delete(Index: integer);
    property Items[Index: integer]: TATSynRange read GetItems write SetItems; default;
    function IsRangeInsideOther(const R1, R2: TATSynRange): boolean;
    function IsRangesSame(const R1, R2: TATSynRange): boolean;
    function FindRangesContainingLines(ALineFrom, ALineTo: integer;
      AInRangeIndex: integer; AOnlyFolded, ATopLevelOnly: boolean;
      ALineMode: TATRangeHasLines): TATIntArray;
    function FindDeepestRangeContainingLine(ALine: integer; AIndexes: TATIntArray): integer;
    function FindRangeWithPlusAtLine(ALine: integer): integer;
    function FindIndexOfRange(const R: TATSynRange): integer;
    function MessageText(Cnt: integer): string;
  end;

implementation

uses
  Math,
  ATSynEdit_Carets;

//we allow one block to hangout 1 line by Y2 from outer block:
//it's needed for Pascal econtrol lexer
//(don't know why it gives such blocks)
const
  cAllowHangoutLines = 1; //0 or 1, do not bigger


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

constructor TATSynRanges.Create;
begin
  FList:= TATSynRangeList.Create;
  FList.Capacity:= 2*1024;
end;

destructor TATSynRanges.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynRanges.Clear;
begin
  FList.Clear;
end;

function TATSynRanges.Add(AX, AY, AY2: integer; AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATSynRange;
begin
  Result.Init(AX, AY, AY2, AWithStaple, AHint, ATag);
  FList.Add(Result);
end;

function TATSynRanges.Insert(Index: integer; AX, AY, AY2: integer;
  AWithStaple: boolean;
  const AHint: string;
  const ATag: Int64=0): TATSynRange;
begin
  Result.Init(AX, AY, AY2, AWithStaple, AHint, ATag);
  FList.Insert(Index, Result);
end;

procedure TATSynRanges.Delete(Index: integer); inline;
begin
  FList.Delete(Index);
end;

function TATSynRanges.IsRangeInsideOther(const R1, R2: TATSynRange): boolean;
begin
  Result:=
    IsPosSorted(R2.X, R2.Y, R1.X, R1.Y, true)
    and (R1.Y2-cAllowHangoutLines<=R2.Y2);
end;

function TATSynRanges.IsRangesSame(const R1, R2: TATSynRange): boolean;
begin
  if R1=R2 then
    exit(true);
  if (R1.X=R2.X) and (R1.Y=R2.Y) and (Abs(R1.Y2-R2.Y2)<=cAllowHangoutLines) then
    exit(true);

  Result:= false;
end;

function TATSynRanges.FindRangesContainingLines(ALineFrom, ALineTo: integer;
  AInRangeIndex: integer; AOnlyFolded, ATopLevelOnly: boolean;
  ALineMode: TATRangeHasLines): TATIntArray;
var
  L: TList;
  R, RTest: TATSynRange;
  i, j: integer;
  Ok: boolean;
begin
  SetLength(Result, 0);
  L:= TList.Create;
  L.Capacity:= 512;
  try
    for i:= 0 to Count-1 do
    begin
      R:= Items[i];
      if (not R.IsSimple) then
        if (not AOnlyFolded or R.Folded) then
        begin
          case ALineMode of
            cRngIgnore: Ok:= true;
            cRngHasAllLines: Ok:= (R.Y<=ALineFrom) and (R.Y2>=ALineTo);
            cRngHasAnyOfLines: Ok:= (R.Y<=ALineTo) and (R.Y2>=ALineFrom);
            cRngExceptThisRange: Ok:= i<>AInRangeIndex;
            else raise Exception.Create('unknown LineMode');
          end;
          if not Ok then Continue;

          if AInRangeIndex<0 then
            Ok:= true
          else
          begin
            RTest:= Items[AInRangeIndex];
            Ok:= not IsRangesSame(RTest, R) and IsRangeInsideOther(R, RTest);
          end;

          if Ok then
            L.Add(Pointer(PtrInt(i)));
        end;
    end;

    if ATopLevelOnly then
    begin
      for i:= L.Count-1 downto 1 do
        for j:= 0 to i-1 do
          if IsRangeInsideOther(Items[PtrInt(L[i])], Items[PtrInt(L[j])]) then
          begin
            L.Delete(i);
            Break
          end;
    end;

    SetLength(Result, L.Count);
    for i:= 0 to L.Count-1 do
      Result[i]:= PtrInt(L[i]);
  finally
    FreeAndNil(L);
  end;
end;

function TATSynRanges.FindDeepestRangeContainingLine(ALine: integer; AIndexes: TATIntArray): integer;
var
  R: TATSynRange;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to High(AIndexes) do
  begin
    R:= Items[AIndexes[i]];
    if R.IsSimple then Continue;
    if (R.Y>ALine) then Break;
    if (R.Y2<ALine) then Continue;
    if (Result<0) or (R.Y>Items[Result].Y) then
      Result:= AIndexes[i];
  end;
end;


function TATSynRanges.FindRangeWithPlusAtLine(ALine: integer): integer;
var
  i: integer;
  R: TATSynRange;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
  begin
    R:= Items[i];
    if (not R.IsSimple) and (R.Y=ALine) then
      exit(i);
  end;
end;

function TATSynRanges.FindIndexOfRange(const R: TATSynRange): integer;
begin
  Result:= FList.IndexOf(R);
end;

function TATSynRanges.MessageText(Cnt: integer): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(Count-1, Cnt) do
    Result:= Result+Items[i].MessageText+#13;
end;

function TATSynRanges.MessageTextForIndexList(L: TList): string;
var
  i: integer;
begin
  Result:= '';
  if L.Count=0 then exit;
  for i:= 0 to L.Count-1 do
    Result:= Result+Items[PtrInt(L[i])].MessageText+#13;
end;

procedure TATSynRanges.SetItems(Index: integer; const AValue: TATSynRange);
begin
  FList[Index]:= AValue
end;

end.

