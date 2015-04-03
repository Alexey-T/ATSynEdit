unit ATSynEdit_WrapInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TATSynWrapItem }

  TATSynWrapFinal = (cWrapItemFinal, cWrapItemCollapsed, cWrapItemMiddle);

  TATSynWrapItem = class
  public
    NLineIndex,
    NCharIndex,
    NLength,
    NIndent: integer;
    NFinal: TATSynWrapFinal;
    constructor Create(ALineIndex, ACharIndex, ALength, AIndent: integer;
      AFinal: TATSynWrapFinal); virtual;
    procedure Assign(Item: TATSynWrapItem);
  end;

type
  { TATSynWrapInfo }

  TATSynWrapInfo = class
  private
    FList: TList;
    function GetItem(N: integer): TATSynWrapItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    function IsItemInitial(N: integer): boolean;
    function IsItemAfterCollapsed(N: integer): boolean;
    property Items[N: integer]: TATSynWrapItem read GetItem; default;
    procedure Add(AItem: TATSynWrapItem);
    procedure Delete(N: integer);
    procedure Insert(N: integer; AItem: TATSynWrapItem);
    procedure FindIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
    procedure SetCapacity(N: integer);
    procedure ReplaceItems(AFrom, ATo: integer; AItems: TList);
  end;


implementation

uses
  Math;

{ TATSynWrapItem }

constructor TATSynWrapItem.Create(ALineIndex, ACharIndex, ALength,
  AIndent: integer; AFinal: TATSynWrapFinal);
begin
  NLineIndex:= ALineIndex;
  NCharIndex:= ACharIndex;
  NLength:= ALength;
  NIndent:= AIndent;
  NFinal:= AFinal;
end;

procedure TATSynWrapItem.Assign(Item: TATSynWrapItem);
begin
  NLineIndex:= Item.NLineIndex;
  NCharIndex:= Item.NCharIndex;
  NLength:= Item.NLength;
  NIndent:= Item.NIndent;
  NFinal:= Item.NFinal;
end;

{ TATSynWrapInfo }

function TATSynWrapInfo.GetItem(N: integer): TATSynWrapItem;
begin
  if IsIndexValid(N) then
    Result:= TATSynWrapItem(FList[N])
  else
    Result:= nil;
end;

constructor TATSynWrapInfo.Create;
begin
  FList:= TList.Create;
end;

destructor TATSynWrapInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynWrapInfo.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    TObject(FList[i]).Free;
    FList.Delete(i);
  end;
end;

function TATSynWrapInfo.Count: integer;
begin
  Result:= FList.Count;
end;

function TATSynWrapInfo.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATSynWrapInfo.IsItemInitial(N: integer): boolean;
begin
  if IsIndexValid(N) then
  begin
    if N=0 then
      Result:= true
    else
      Result:= Items[N].NLineIndex<>Items[N-1].NLineIndex;
  end
  else
    Result:= true;
end;

function TATSynWrapInfo.IsItemAfterCollapsed(N: integer): boolean;
begin
  if IsIndexValid(N) and (N>0) then
  begin
    Result:= Items[N].NLineIndex-Items[N-1].NLineIndex > 1;
  end
  else
    Result:= false;
end;

procedure TATSynWrapInfo.Add(AItem: TATSynWrapItem);
begin
  FList.Add(AItem);
end;

procedure TATSynWrapInfo.Delete(N: integer);
begin
  if IsIndexValid(N) then
    FList.Delete(N);
end;

procedure TATSynWrapInfo.Insert(N: integer; AItem: TATSynWrapItem);
begin
  if N>=Count then
    FList.Add(AItem)
  else
    FList.Insert(N, AItem);
end;

procedure TATSynWrapInfo.FindIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
var
  a, b, m, dif: integer;
begin
  AFrom:= -1;
  ATo:= -1;

  a:= 0;
  b:= Count-1;
  if b<0 then Exit;

  repeat
    dif:= Items[a].NLineIndex-ALineNum;
    if dif=0 then begin m:= a; Break end;

    m:= a+b;
    if Odd(m) then
      m:= m div 2 +1
    else
      m:= m div 2;

    dif:= Items[m].NLineIndex-ALineNum;
    if dif=0 then Break;

    if dif>0 then b:= m else a:= m;
    if a=b then Exit;
  until false;

  AFrom:= m;
  ATo:= m;
  while (AFrom>0) and (Items[AFrom-1].NLineIndex=ALineNum) do Dec(AFrom);
  while (ATo<Count-1) and (Items[ATo+1].NLineIndex=ALineNum) do Inc(ATo);
end;

procedure TATSynWrapInfo.SetCapacity(N: integer);
begin
  FList.Capacity:= Max(1024, N);
end;

procedure TATSynWrapInfo.ReplaceItems(AFrom, ATo: integer; AItems: TList);
var
  i: integer;
begin
  for i:= ATo downto AFrom do
    Delete(i);
  for i:= AItems.Count-1 downto 0 do
    Insert(AFrom, TATSynWrapItem(AItems[i]));
end;


end.

