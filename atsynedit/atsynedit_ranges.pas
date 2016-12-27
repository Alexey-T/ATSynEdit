{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Ranges;

{$mode objfpc}{$H+}

//{$define show_unfold_rng}

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
    FStaple: boolean;
    FHint: string;
  public
    property X: integer read FX;
    property Y: integer read FY;
    property Y2: integer read FY2;
    property Folded: boolean read FFolded write FFolded;
    property Staple: boolean read FStaple;
    property Hint: string read FHint write FHint;
    constructor Create(AX, AY, AY2: integer; AStaple: boolean; const AHint: string); virtual;
    function IsSimple: boolean;
    function IsLineInside(ALine: integer): boolean;
    function MessageText: string;
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
    FList: TList;
    function GetCount: integer;
    function GetItems(Index: integer): TATSynRange;
    function MessageTextForIndexList(L: TList): string;
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
    function IsRangesSame(R1, R2: TATSynRange): boolean;
    function FindRangesContainingLines(ALineFrom, ALineTo: integer;
      AInRange: TATSynRange; AOnlyFolded, ATopLevelOnly: boolean;
      ALineMode: TATRangeHasLines): TATIntArray;
    function FindRangeWithPlusAtLine(ALine: integer): TATSynRange;
    function FindIndexOfRange(R: TATSynRange): integer;
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

constructor TATSynRange.Create(AX, AY, AY2: integer; AStaple: boolean;
  const AHint: string);
begin
  if (AX<=0) then raise Exception.Create('Incorrect range with x<=0: '+MessageText);
  if (AY<0) then raise Exception.Create('Incorrect range with y<0: '+MessageText);
  if (AY>AY2) then raise Exception.Create('Incorrect range with y>y2: '+MessageText);

  FX:= AX;
  FY:= AY;
  FY2:= AY2;
  FStaple:= AStaple;
  FHint:= AHint;
end;

function TATSynRange.IsSimple: boolean;
begin
  Result:= Y=Y2;
end;

function TATSynRange.IsLineInside(ALine: integer): boolean;
begin
  Result:= (ALine>=Y) and (ALine<=Y2);
end;

function TATSynRange.MessageText: string;
begin
  Result:= Format('%d..%d', [Y+1, Y2+1]);
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
  Result:= TATSynRange(FList[Index]);
  {
  if IsIndexValid(Index) then
    Result:= TATSynRange(FList[Index])
  else
    Result:= nil;
    }
end;

constructor TATSynRanges.Create;
begin
  FList:= TList.Create;
  FList.Capacity:= 4000;
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
  Result:=
    IsPosSorted(R2.X, R2.Y, R1.X, R1.Y, true)
    and (R1.Y2-cAllowHangoutLines<=R2.Y2);
end;

function TATSynRanges.IsRangesSame(R1, R2: TATSynRange): boolean;
begin
  if R1=R2 then
    begin Result:= true; Exit end;
  if (R1.X=R2.X) and (R1.Y=R2.Y) and (Abs(R1.Y2-R2.Y2)<=cAllowHangoutLines) then
    begin Result:= true; Exit end;

  Result:= false;
end;

function TATSynRanges.FindRangesContainingLines(ALineFrom, ALineTo: integer;
  AInRange: TATSynRange; AOnlyFolded, ATopLevelOnly: boolean; ALineMode: TATRangeHasLines): TATIntArray;
var
  L: TList;
  R: TATSynRange;
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
            cRngExceptThisRange: Ok:= R<>AInRange;
            else raise Exception.Create('unknown LineMode');
          end;
          if not Ok then Continue;

          if AInRange=nil then
            Ok:= true
          else
            Ok:= not IsRangesSame(AInRange, R) and IsRangeInsideOther(R, AInRange);

          if Ok then
            L.Add(Pointer(PtrInt(i)));
        end;
    end;

    if ATopLevelOnly then
    begin
      {$ifdef show_unfold_rng}
      s1:= 'toplevel: ranges shortlist'#13+MessageTextForIndexList(L);
      {$endif}

      for i:= L.Count-1 downto 1 do
        for j:= 0 to i-1 do
          if IsRangeInsideOther(Items[PtrInt(L[i])], Items[PtrInt(L[j])]) then
          begin
            L.Delete(i);
            Break
          end;

      {$ifdef show_unfold_rng}
      s2:= 'toplevel: ranges done'#13+MessageTextForIndexList(L);
      if l.count>0 then
        showmessage(s1+#13+s2);
      {$endif}
    end;

    SetLength(Result, L.Count);
    for i:= 0 to L.Count-1 do
      Result[i]:= PtrInt(L[i]);
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

function TATSynRanges.FindIndexOfRange(R: TATSynRange): integer;
begin
  Result:= FList.IndexOf(Pointer(R));
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

end.

