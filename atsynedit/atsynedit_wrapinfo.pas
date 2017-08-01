{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_WrapInfo;

{$mode objfpc}{$H+}
{$Z1}

interface

uses
  Classes, SysUtils,
  ATStrings;

type
  TATSynWrapFinal = (
    cWrapItemFinal,
    cWrapItemCollapsed,
    cWrapItemMiddle
    );

type
  { TATSynWrapItem }

  TATSynWrapItem = packed class
  public
    NLineIndex,
    NCharIndex,
    NLength: integer;
    NIndent: word;
    NFinal: TATSynWrapFinal;
    constructor Create(ALineIndex, ACharIndex, ALength: integer;
      AIndent: word; AFinal: TATSynWrapFinal);
    procedure Assign(Item: TATSynWrapItem);
  end;

type
  TATCheckLineCollapsedEvent = function(ALineNum: integer): boolean of object;

type
  { TATSynWrapInfo }

  TATSynWrapInfo = class
  private
    FList: TList;
    FStrings: TATStrings;
    FVirtualMode: boolean;
    FVirtualItem: TATSynWrapItem;
    FOnCheckCollapsed: TATCheckLineCollapsedEvent;
    function GetItem(AIndex: integer): TATSynWrapItem;
    procedure SetVirtualMode(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property StringsObj: TATStrings read FStrings write FStrings;
    property VirtualMode: boolean read FVirtualMode write SetVirtualMode;
    function Count: integer;
    function IsIndexValid(N: integer): boolean; inline;
    function IsItemInitial(N: integer): boolean;
    property Items[N: integer]: TATSynWrapItem read GetItem; default;
    procedure Add(AItem: TATSynWrapItem);
    procedure Delete(N: integer);
    procedure Insert(N: integer; AItem: TATSynWrapItem);
    procedure FindIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
    function FindIndexOfCaretPos(APos: TPoint): integer;
    procedure SetCapacity(N: integer);
    procedure ReplaceItems(AFrom, ATo: integer; AItems: TList);
    property OnCheckLineCollapsed: TATCheckLineCollapsedEvent read FOnCheckCollapsed write FOnCheckCollapsed;
  end;


implementation

uses
  Math, Dialogs, Forms;

{ TATSynWrapItem }

constructor TATSynWrapItem.Create(ALineIndex, ACharIndex, ALength: integer;
  AIndent: word; AFinal: TATSynWrapFinal);
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

function TATSynWrapInfo.GetItem(AIndex: integer): TATSynWrapItem;
begin
  if FVirtualMode then
  begin
    FVirtualItem.NLineIndex:= AIndex;
    FVirtualItem.NCharIndex:= 1;
    FVirtualItem.NLength:= FStrings.LinesLen[AIndex];
    FVirtualItem.NIndent:= 0;
    FVirtualItem.NFinal:= cWrapItemFinal;
    Result:= FVirtualItem;
  end
  else
    Result:= TATSynWrapItem(FList[AIndex]);
end;

procedure TATSynWrapInfo.SetVirtualMode(AValue: boolean);
begin
  if FVirtualMode=AValue then Exit;
  FVirtualMode:= AValue;
  if FVirtualMode then
    FList.Clear;
end;

constructor TATSynWrapInfo.Create;
begin
  FList:= TList.Create;
  FVirtualItem:= TATSynWrapItem.Create(0, 0, 0, 0, cWrapItemFinal);
  FVirtualMode:= false;
end;

destructor TATSynWrapInfo.Destroy;
begin
  Clear;
  FreeAndNil(FVirtualItem);
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynWrapInfo.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TATSynWrapInfo.Count: integer;
begin
  if FVirtualMode then
    Result:= FStrings.Count
  else
    Result:= FList.Count;
end;

function TATSynWrapInfo.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATSynWrapInfo.IsItemInitial(N: integer): boolean;
begin
  Result:= true;
  if (N>0) and (N<FList.Count) then //cant use IsIndexValid, N>0
    Result:= Items[N].NLineIndex<>Items[N-1].NLineIndex;
end;

procedure TATSynWrapInfo.Add(AItem: TATSynWrapItem);
begin
  FList.Add(AItem);
end;

procedure TATSynWrapInfo.Delete(N: integer);
begin
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

  if Assigned(FOnCheckCollapsed) then
    if FOnCheckCollapsed(ALineNum) then Exit;

  a:= 0;
  b:= Count-1;
  if b<0 then Exit;

  repeat
    dif:= Items[a].NLineIndex-ALineNum;
    if dif=0 then begin m:= a; Break end;

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= Items[m].NLineIndex-ALineNum;
    if dif=0 then Break;

    if Abs(a-b)<=1 then Exit;
    if dif>0 then b:= m else a:= m;
  until false;

  AFrom:= m;
  ATo:= m;
  while (AFrom>0) and (Items[AFrom-1].NLineIndex=ALineNum) do Dec(AFrom);
  while (ATo<Count-1) and (Items[ATo+1].NLineIndex=ALineNum) do Inc(ATo);
end;

function TATSynWrapInfo.FindIndexOfCaretPos(APos: TPoint): integer;
var
  Item: TATSynWrapItem;
  NFrom, NTo, i: integer;
begin
  Result:= -1;
  FindIndexesOfLineNumber(APos.Y, NFrom, NTo);
  if NFrom<0 then Exit;
  for i:= NFrom to NTo do
  begin
    Result:= i;
    Item:= Items[i];
    if Item.NCharIndex+Item.NLength > APos.X then Break;
  end;
end;

procedure TATSynWrapInfo.SetCapacity(N: integer);
begin
  FList.Capacity:= Max(1024, N);
end;

//optimized; don't just del/ins
procedure TATSynWrapInfo.ReplaceItems(AFrom, ATo: integer; AItems: TList);
var
  Dif, i: integer;
begin
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
      Insert(AFrom, TATSynWrapItem.Create(0, 0, 0, 0, Low(TATSynWrapFinal)));
  end;

  //overwrite N items
  for i:= 0 to AItems.Count-1 do
    TATSynWrapItem(FList[AFrom+i]).Assign(TATSynWrapItem(AItems[i]));

  //must free list
  for i:= 0 to AItems.Count-1 do
    TObject(AItems[i]).Free;
end;


end.

