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
  TATWrapItem = packed record
    NLineIndex,
    NCharIndex,
    NLength: integer;
    NIndent: word;
    NFinal: TATSynWrapFinal;
    procedure Init(ALineIndex, ACharIndex, ALength, AIndent: integer; AFinal: TATSynWrapFinal); inline;
    class operator=(const A, B: TATWrapItem): boolean;
  end;

type
  TATWrapItems = specialize TFPGList<TATWrapItem>;


type
  TATCheckLineCollapsedEvent = function(ALineNum: integer): boolean of object;

type
  { TATWrapInfo }

  TATWrapInfo = class
  private
    FList: TATWrapItems;
    FStrings: TATStrings;
    FVirtualMode: boolean;
    FOnCheckCollapsed: TATCheckLineCollapsedEvent;
    function GetData(AIndex: integer): TATWrapItem;
    procedure SetVirtualMode(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property StringsObj: TATStrings read FStrings write FStrings;
    property VirtualMode: boolean read FVirtualMode write SetVirtualMode;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    function IsItemInitial(N: integer): boolean;
    property Data[N: integer]: TATWrapItem read GetData; default;
    procedure Add(const AData: TATWrapItem);
    procedure Delete(N: integer);
    procedure Insert(N: integer; const AItem: TATWrapItem);
    procedure FindIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
    function FindIndexOfCaretPos(APos: TPoint): integer;
    procedure SetCapacity(N: integer);
    procedure ReplaceItems(AFrom, ATo: integer; AItems: TATWrapItems);
    property OnCheckLineCollapsed: TATCheckLineCollapsedEvent read FOnCheckCollapsed write FOnCheckCollapsed;
  end;


implementation

uses
  Math, Dialogs, Forms;

{ TATWrapItem }

procedure TATWrapItem.Init(ALineIndex, ACharIndex, ALength, AIndent: integer; AFinal: TATSynWrapFinal); inline;
begin
  NLineIndex:= ALineIndex;
  NCharIndex:= ACharIndex;
  NLength:= ALength;
  NIndent:= AIndent;
  NFinal:= AFinal;
end;

class operator TATWrapItem.=(const A, B: TATWrapItem): boolean;
begin
  Result:= false;
end;

{ TATWrapInfo }

function TATWrapInfo.GetData(AIndex: integer): TATWrapItem;
begin
  if FVirtualMode then
    Result.Init(AIndex, 1, FStrings.LinesLen[AIndex], 0, cWrapItemFinal)
  else
  begin
    if AIndex>=0 then
      Result:= FList[AIndex]
    else
      FillChar(Result, SizeOf(Result), 0);
  end;
end;

procedure TATWrapInfo.SetVirtualMode(AValue: boolean);
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

constructor TATWrapInfo.Create;
begin
  FList:= TATWrapItems.Create;
  FVirtualMode:= false;
end;

destructor TATWrapInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATWrapInfo.Clear; inline;
begin
  FList.Clear;
end;

function TATWrapInfo.Count: integer; inline;
begin
  if FVirtualMode then
    Result:= FStrings.Count
  else
    Result:= FList.Count;
end;

function TATWrapInfo.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<Count);
end;

function TATWrapInfo.IsItemInitial(N: integer): boolean;
begin
  if (N>0) and (N<Count) then //cant use IsIndexValid, N>0
    Result:= Data[N].NLineIndex<>Data[N-1].NLineIndex
  else
    Result:= true;
end;

procedure TATWrapInfo.Add(const AData: TATWrapItem); inline;
begin
  if FVirtualMode then exit;
  FList.Add(AData);
end;

procedure TATWrapInfo.Delete(N: integer); inline;
begin
  if FVirtualMode then exit;
  FList.Delete(N);
end;

procedure TATWrapInfo.Insert(N: integer; const AItem: TATWrapItem); inline;
begin
  if FVirtualMode then exit;
  if N>=FList.Count then
    FList.Add(AItem)
  else
    FList.Insert(N, AItem);
end;

procedure TATWrapInfo.FindIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
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

function TATWrapInfo.FindIndexOfCaretPos(APos: TPoint): integer;
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

procedure TATWrapInfo.SetCapacity(N: integer); inline;
begin
  FList.Capacity:= Max(1024, N);
end;

//optimized; don't just del/ins
procedure TATWrapInfo.ReplaceItems(AFrom, ATo: integer; AItems: TATWrapItems);
var
  Item: TATWrapItem;
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
      Item.Init(0, 0, 0, 0, Low(TATSynWrapFinal));
      Insert(AFrom, Item);
    end;
  end;

  //overwrite N items
  for i:= 0 to AItems.Count-1 do
    FList[AFrom+i]:= AItems[i];
end;


end.

