{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_WrapInfo;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$Z1}

interface

uses
  Classes, SysUtils,
  ATStrings,
  ATSynEdit_fgl;

type
  TATSynWrapFinal = (
    cWrapItemFinal,
    cWrapItemCollapsed,
    cWrapItemMiddle
    );

type
  TATSynWrapItem = packed record
    NLineIndex,
    NCharIndex,
    NLength: integer;
    NIndent: word;
    NFinal: TATSynWrapFinal;
    class operator=(const A, B: TATSynWrapItem): boolean;
  end;

type
  TATSynWrapItems = specialize TFPGList<TATSynWrapItem>;

procedure WrapItem_Init(var AItem: TATSynWrapItem;
  const ALineIndex, ACharIndex, ALength, AIndent: integer; AFinal: TATSynWrapFinal); inline;

type
  TATCheckLineCollapsedEvent = function(ALineNum: integer): boolean of object;

type
  { TATSynWrapInfo }

  TATSynWrapInfo = class
  private
    FList: TATSynWrapItems;
    FStrings: TATStrings;
    FVirtualMode: boolean;
    FOnCheckCollapsed: TATCheckLineCollapsedEvent;
    function GetData(AIndex: integer): TATSynWrapItem;
    procedure SetVirtualMode(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property StringsObj: TATStrings read FStrings write FStrings;
    property VirtualMode: boolean read FVirtualMode write SetVirtualMode;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    function IsItemInitial(N: integer): boolean;
    property Data[N: integer]: TATSynWrapItem read GetData; default;
    procedure Add(const AData: TATSynWrapItem);
    procedure Delete(N: integer);
    procedure Insert(N: integer; const AItem: TATSynWrapItem);
    procedure FindIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
    function FindIndexOfCaretPos(APos: TPoint): integer;
    procedure SetCapacity(N: integer);
    procedure ReplaceItems(AFrom, ATo: integer; AItems: TATSynWrapItems);
    property OnCheckLineCollapsed: TATCheckLineCollapsedEvent read FOnCheckCollapsed write FOnCheckCollapsed;
  end;


implementation

uses
  Math, Dialogs, Forms;

procedure WrapItem_Init(var AItem: TATSynWrapItem; const ALineIndex,
  ACharIndex, ALength, AIndent: integer; AFinal: TATSynWrapFinal); inline;
begin
  AItem.NLineIndex:= ALineIndex;
  AItem.NCharIndex:= ACharIndex;
  AItem.NLength:= ALength;
  AItem.NIndent:= AIndent;
  AItem.NFinal:= AFinal;
end;

{ TATSynWrapItem }

class operator TATSynWrapItem.=(const A, B: TATSynWrapItem): boolean;
begin
  Result:= false;
end;

{ TATSynWrapInfo }

function TATSynWrapInfo.GetData(AIndex: integer): TATSynWrapItem;
begin
  if FVirtualMode then
  begin
    Result.NLineIndex:= AIndex;
    Result.NCharIndex:= 1;
    Result.NLength:= FStrings.LinesLen[AIndex];
    Result.NIndent:= 0;
    Result.NFinal:= cWrapItemFinal;
  end
  else
  begin
    //Assert(IsIndexValid(AIndex), 'Invalid index in WrapInfo.GetData()');
    if AIndex>=0 then
      Result:= FList[AIndex]
    else
      FillChar(Result, SizeOf(Result), 0);
  end;
end;

procedure TATSynWrapInfo.SetVirtualMode(AValue: boolean);
begin
  if FVirtualMode=AValue then Exit;
  FVirtualMode:= AValue;

  {
  don't clear:
  adapter fills Fold ranges after a pause... this causes nasty empty screen of control
  }
  //if FVirtualMode then
  //  Clear;
end;

constructor TATSynWrapInfo.Create;
begin
  FList:= TATSynWrapItems.Create;
  FVirtualMode:= false;
end;

destructor TATSynWrapInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynWrapInfo.Clear; inline;
begin
  FList.Clear;
end;

function TATSynWrapInfo.Count: integer; inline;
begin
  if FVirtualMode then
    Result:= FStrings.Count
  else
    Result:= FList.Count;
end;

function TATSynWrapInfo.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<Count);
end;

function TATSynWrapInfo.IsItemInitial(N: integer): boolean;
begin
  if (N>0) and (N<Count) then //cant use IsIndexValid, N>0
    Result:= Data[N].NLineIndex<>Data[N-1].NLineIndex
  else
    Result:= true;
end;

procedure TATSynWrapInfo.Add(const AData: TATSynWrapItem); inline;
begin
  if FVirtualMode then exit;
  FList.Add(AData);
end;

procedure TATSynWrapInfo.Delete(N: integer); inline;
begin
  if FVirtualMode then exit;
  FList.Delete(N);
end;

procedure TATSynWrapInfo.Insert(N: integer; const AItem: TATSynWrapItem); inline;
begin
  if FVirtualMode then exit;
  if N>=FList.Count then
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
    dif:= Data[a].NLineIndex-ALineNum;
    if dif=0 then begin m:= a; Break end;

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= Data[m].NLineIndex-ALineNum;
    if dif=0 then Break;

    if Abs(a-b)<=1 then Exit;
    if dif>0 then b:= m else a:= m;
  until false;

  AFrom:= m;
  ATo:= m;
  while (AFrom>0) and (Data[AFrom-1].NLineIndex=ALineNum) do Dec(AFrom);
  while (ATo<Count-1) and (Data[ATo+1].NLineIndex=ALineNum) do Inc(ATo);
end;

function TATSynWrapInfo.FindIndexOfCaretPos(APos: TPoint): integer;
var
  NFrom, NTo, i: integer;
begin
  Result:= -1;
  FindIndexesOfLineNumber(APos.Y, NFrom, NTo);
  if NFrom<0 then Exit;
  for i:= NFrom to NTo do
  begin
    Result:= i;
    with Data[i] do
      if NCharIndex+NLength > APos.X then Break;
  end;
end;

procedure TATSynWrapInfo.SetCapacity(N: integer); inline;
begin
  FList.Capacity:= Max(1024, N);
end;

//optimized; don't just del/ins
procedure TATSynWrapInfo.ReplaceItems(AFrom, ATo: integer; AItems: TATSynWrapItems);
var
  Item: TATSynWrapItem;
  Dif, i: integer;
begin
  if FVirtualMode then exit;
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
    begin
      WrapItem_Init(Item, 0, 0, 0, 0, Low(TATSynWrapFinal));
      Insert(AFrom, Item);
    end;
  end;

  //overwrite N items
  for i:= 0 to AItems.Count-1 do
    FList[AFrom+i]:= AItems[i];
end;


end.

