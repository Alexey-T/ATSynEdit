
//todo....
//optim function TATSynRanges.FindRangesContainingLine

unit ATSynEdit_Ranges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATStringProc;

type
  { TATSynRange }

  TATSynRange = class
  public
    X, Y, //start pos
    Y2: integer; //end line which is fully folded (can't partially fold)
    Folded: boolean;
    constructor Create(AX, AY, AY2: integer); virtual;
    function IsSimple: boolean;
  end;

type
  { TATSynRanges }

  TATSynRanges = class
  private
    FList: TList;
    function GetCount: integer;
    function GetItems(Index: integer): TATSynRange;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Count: integer read GetCount;
    function IsIndexValid(N: integer): boolean;
    procedure Clear;
    procedure Add(AX, AY, AY2: integer);
    procedure Insert(Index: integer; AX, AY, AY2: integer);
    procedure Delete(Index: integer);
    property Items[Index: integer]: TATSynRange read GetItems; default;
    function IsRangeInsideOther(R1, R2: TATSynRange): boolean;
    function FindRangesContainingLine(ALine: integer;
      AInRange: TATSynRange;
      AOnlyFolded: boolean = false;
      ATopLevelOnly: boolean = false): TATIntArray;
    function FindRangeWithPlusAtLine(ALine: integer): TATSynRange;
  end;

implementation

uses
  ATCarets;

{ TATSynRange }

constructor TATSynRange.Create(AX, AY, AY2: integer);
begin
  if (AX<=0) then raise Exception.Create('Incorrect range with x<=0');
  if (AY<0) then raise Exception.Create('Incorrect range with y<0');
  if (AY>AY2) then raise Exception.Create('Incorrect range with y>y2');

  X:= AX;
  Y:= AY;
  Y2:= AY2;
end;

function TATSynRange.IsSimple: boolean;
begin
  Result:= Y=Y2;
end;

{ TATSynRanges }

function TATSynRanges.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATSynRanges.GetCount: integer;
begin
  Result:= FList.Count;
end;

function TATSynRanges.GetItems(Index: integer): TATSynRange;
begin
  if IsIndexValid(Index) then
    Result:= TATSynRange(FList[Index])
  else
    Result:= nil;
end;

constructor TATSynRanges.Create;
begin
  FList:= TList.Create;
  FList.Capacity:= 2000;
end;

destructor TATSynRanges.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynRanges.Clear;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

procedure TATSynRanges.Add(AX, AY, AY2: integer);
begin
  FList.Add(TATSynRange.Create(AX, AY, AY2));
end;

procedure TATSynRanges.Insert(Index: integer; AX, AY, AY2: integer);
begin
  FList.Insert(Index, TATSynRange.Create(AX, AY, AY2));
end;

procedure TATSynRanges.Delete(Index: integer);
begin
  if IsIndexValid(Index) then
  begin
    TObject(FList[Index]).Free;
    FList.Delete(Index);
  end;
end;

function TATSynRanges.IsRangeInsideOther(R1, R2: TATSynRange): boolean;
begin
  Result:=
    IsPosSorted(R2.X, R2.Y, R1.X, R1.Y, true) and
    (R1.Y2<=R2.Y2);
end;

function TATSynRanges.FindRangesContainingLine(ALine: integer;
  AInRange: TATSynRange; AOnlyFolded: boolean; ATopLevelOnly: boolean
  ): TATIntArray;
var
  i, j: integer;
begin
  SetLength(Result, 0);

  //todo... make bin-search...
  for i:= 0 to Count-1 do
    with Items[i] do
      if (not IsSimple) and (Y<=ALine) and (Y2>=ALine) and (not AOnlyFolded or Folded) then
        if (AInRange=nil) or ((Y>=AInRange.Y) and (Y2<=AInRange.Y2)) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)]:= i;
        end;

  //todo...optim...
  if ATopLevelOnly then
    for i:= High(Result) downto 0 do
      for j:= 0 to i-1 do
      begin
        if IsRangeInsideOther(Items[i], Items[j]) then
          SetLength(Result, Length(Result)-1);
      end;
end;

function TATSynRanges.FindRangeWithPlusAtLine(ALine: integer): TATSynRange;
var
  i: integer;
  R: TATSynRange;
begin
  Result:= nil;
  for i:= 0 to Count-1 do
  begin
    R:= Items[i];
    if (not R.IsSimple) and (R.Y=ALine) then
    begin
      Result:= R;
      Break
    end;
  end;
end;

end.

