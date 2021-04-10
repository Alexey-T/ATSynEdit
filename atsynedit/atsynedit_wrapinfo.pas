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
  TATWrapItemFinal = (
    cWrapItemFinal,
    cWrapItemCollapsed,
    cWrapItemMiddle
    );

type
  { TATWrapItem }

  TATWrapItem = packed record
    NLineIndex: integer;
    NCharIndex: integer;
    NLength: integer;
    NIndent: word;
    NFinal: TATWrapItemFinal;
    bInitial: boolean;
    procedure Init(ALineIndex, ACharIndex, ALength, AIndent: integer; AFinal: TATWrapItemFinal; AInitial: boolean); inline;
    class operator=(const A, B: TATWrapItem): boolean;
  end;

type
  TATWrapItems = specialize TFPGList<TATWrapItem>;

type
  { TATWrapInfo }

  TATWrapInfo = class
  private
    FList: TATWrapItems;
    FStrings: TATStrings;
    FVirtualMode: boolean;
    function GetData(AIndex: integer): TATWrapItem;
    procedure SetVirtualMode(AValue: boolean);
    function IsLineFolded(ALine: integer): boolean;
  public
    VisibleColumns: integer;
    WrapColumn: integer;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property StringsObj: TATStrings read FStrings write FStrings;
    property VirtualMode: boolean read FVirtualMode write SetVirtualMode;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Data[N: integer]: TATWrapItem read GetData; default;
    procedure Add(const AData: TATWrapItem);
    procedure Delete(N: integer);
    procedure Insert(N: integer; const AItem: TATWrapItem);
    procedure FindIndexesOfLineNumber(ALineNum: integer; out AFrom, ATo: integer);
    function FindIndexOfCaretPos(APos: TPoint): integer;
    procedure SetCapacity(N: integer);
    procedure ReplaceItems(AFrom, ATo: integer; AItems: TATWrapItems);
  end;


implementation

uses
  Math, Dialogs, Forms;

{ TATWrapItem }

procedure TATWrapItem.Init(ALineIndex, ACharIndex, ALength, AIndent: integer;
  AFinal: TATWrapItemFinal; AInitial: boolean);
begin
  NLineIndex:= ALineIndex;
  NCharIndex:= ACharIndex;
  NLength:= ALength;
  NIndent:= AIndent;
  NFinal:= AFinal;
  bInitial:= AInitial;
end;

class operator TATWrapItem.=(const A, B: TATWrapItem): boolean;
begin
  Result:= false;
end;

{ TATWrapInfo }

function TATWrapInfo.GetData(AIndex: integer): TATWrapItem;
begin
  if FVirtualMode then
    Result.Init(AIndex, 1, FStrings.LinesLen[AIndex], 0, cWrapItemFinal, true)
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

function TATWrapInfo.IsLineFolded(ALine: integer): boolean;
const
  FEditorIndex = 0;
begin
  Result:= false;
  if not StringsObj.IsIndexValid(ALine) then exit;
  Result:= StringsObj.LinesHidden[ALine, FEditorIndex];
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

  if IsLineFolded(ALineNum) then Exit;

  a:= 0;
  b:= Count-1;

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    dif:= Data[m].NLineIndex-ALineNum;
    if dif=0 then
      Break;
    if dif>0 then
      b:= m-1
    else
      a:= m+1;
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
    if Data[i].NCharIndex + Data[i].NLength > APos.X+1 then // APos.X+1: see CudaText issue 2466
      Break;
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
      Item.Init(0, 0, 0, 0, Low(TATWrapItemFinal), true);
      Insert(AFrom, Item);
    end;
  end;

  //overwrite N items
  for i:= 0 to AItems.Count-1 do
    FList[AFrom+i]:= AItems[i];
end;


end.

