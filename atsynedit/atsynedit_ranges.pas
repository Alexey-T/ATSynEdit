
//todo....
//optim function TATSynRanges.FindRangesContainingLine

unit ATSynEdit_Ranges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  ATStringProc;

type
  { TATSynRange }

  TATSynRange = class
  private
    FX, //start column
    FY, //start line
    FY2: integer; //end line which is fully folded (can't partially fold)
    FFolded: boolean;
    FWithStaple: boolean;
    FHintText: string;
  public
    property X: integer read FX;
    property Y: integer read FY;
    property Y2: integer read FY2;
    property Folded: boolean read FFolded write FFolded;
    property WithStaple: boolean read FWithStaple;
    property HintText: string read FHintText;
    constructor Create(AX, AY, AY2: integer; AWithStaple: boolean; const AHintText: string); virtual;
    function IsSimple: boolean;
    function IsLineInside(ALine: integer): boolean;
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
    procedure Add(AX, AY, AY2: integer; AWithStaple: boolean; const AHint: string);
    procedure Insert(Index: integer; AX, AY, AY2: integer; AWithStaple: boolean;
      const AHint: string);
    procedure Delete(Index: integer);
    property Items[Index: integer]: TATSynRange read GetItems; default;
    function IsRangeInsideOther(R1, R2: TATSynRange): boolean;
    function FindRangesContainingLines(ALineFrom, ALineTo: integer;
      AInRange: TATSynRange; AOnlyFolded, ATopLevelOnly, AAllLines: boolean): TATIntArray;
    function FindRangeWithPlusAtLine(ALine: integer): TATSynRange;
  end;

implementation

uses
  ATSynEdit_Carets;

{ TATSynRange }

constructor TATSynRange.Create(AX, AY, AY2: integer; AWithStaple: boolean;
  const AHintText: string);
begin
  if (AX<=0) then raise Exception.Create('Incorrect range with x<=0');
  if (AY<0) then raise Exception.Create('Incorrect range with y<0');
  if (AY>AY2) then raise Exception.Create('Incorrect range with y>y2');

  FX:= AX;
  FY:= AY;
  FY2:= AY2;
  FWithStaple:= AWithStaple;
  FHintText:= AHintText;
end;

function TATSynRange.IsSimple: boolean;
begin
  Result:= Y=Y2;
end;

function TATSynRange.IsLineInside(ALine: integer): boolean;
begin
  Result:= (ALine>=Y) and (ALine<=Y2);
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

procedure TATSynRanges.Add(AX, AY, AY2: integer; AWithStaple: boolean; const AHint: string);
begin
  FList.Add(TATSynRange.Create(AX, AY, AY2, AWithStaple, AHint));
end;

procedure TATSynRanges.Insert(Index: integer; AX, AY, AY2: integer; AWithStaple: boolean; const AHint: string);
begin
  FList.Insert(Index, TATSynRange.Create(AX, AY, AY2, AWithStaple, AHint));
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
  //if not Assigned(R1) or not Assigned(R2) then
  //  raise Exception.Create('nil range: IsRangeInsideOther');
  Result:=
    IsPosSorted(R2.X, R2.Y, R1.X, R1.Y, true) and
    (R1.Y2<=R2.Y2);
end;

function TATSynRanges.FindRangesContainingLines(ALineFrom, ALineTo: integer;
  AInRange: TATSynRange; AOnlyFolded, ATopLevelOnly, AAllLines: boolean): TATIntArray;
var
  L: TList;
  R: TATSynRange;
  i, j: integer;
begin
  SetLength(Result, 0);
  L:= TList.Create;
  try
    for i:= 0 to Count-1 do
    begin
      R:= Items[i];
      if (not R.IsSimple) then
        if (not AOnlyFolded or R.Folded) then
          if (AAllLines and (R.Y<=ALineFrom) and (R.Y2>=ALineTo)) or
             (not AAllLines and (R.Y<=ALineTo) and (R.Y2>=ALineFrom)) then
            if (AInRange=nil) or
              (Assigned(AInRange) and (AInRange<>R) and (R.Y>=AInRange.Y) and (R.Y2<=AInRange.Y2)) then
            begin
              L.Add(pointer(i));
            end;
    end;

    if ATopLevelOnly then
      for i:= L.Count-1 downto 1 do
        for j:= 0 to i-1 do
        begin
          if IsRangeInsideOther(Items[integer(L[i])], Items[integer(L[j])]) then
          begin
            if L.Count>0 then
              L.Delete(L.Count-1);
            Break
          end;
        end;

    SetLength(Result, L.Count);
    for i:= 0 to L.Count-1 do
      Result[i]:= integer(L[i]);
  finally
    FreeAndNil(L);
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

