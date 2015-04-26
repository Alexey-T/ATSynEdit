unit ATSynEdit_Ranges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
  TATIntegerArray = array of integer;

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
    function FindRangesContainingLine(ALine: integer): TATIntegerArray;
  end;

implementation

uses Dialogs;

{ TATSynRange }

constructor TATSynRange.Create(AX, AY, AY2: integer);
begin
  X:= AX;
  Y:= AY;
  Y2:= AY2;
end;

function TATSynRange.IsSimple: boolean;
begin
  Result:= Y>=Y2;
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

function TATSynRanges.FindRangesContainingLine(ALine: integer): TATIntegerArray;
var
  i: integer;
begin
  //!!!todo... make bin-search...

  SetLength(Result, 0);
  for i:= 0 to Count-1 do
    with Items[i] do
      if (not IsSimple) and (Y<=ALine) and (Y2>=ALine) then
      begin
        SetLength(Result, Length(Result)+1);
        Result[High(Result)]:= i;
      end;
end;

end.

