{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_WrapInfo;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}
{$Z1}

interface

uses
  Classes, SysUtils,
  ATStrings,
  ATSynEdit_fgl;

type
  TATWrapItemFinal = (
    Final,
    Collapsed,
    Middle
    );

type
  { TATWrapItem }

  PATWrapItem = ^TATWrapItem;
  TATWrapItem = packed record
    NLineIndex: SizeInt;
    NCharIndex: SizeInt;
    NLength: SizeInt;
    NIndent: word;
    NFinal: TATWrapItemFinal;
    bInitial: boolean;
    procedure Init(ALineIndex, ACharIndex, ALength, AIndent: SizeInt; AFinal: TATWrapItemFinal; AInitial: boolean);
    function ContainsPos(AX, AY: Int64): boolean;
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
    function IsLineFolded(ALine: SizeInt): boolean;
  public
    VisibleColumns: integer;
    WrapColumn: integer;
    EditorIndex: integer;
    StringsPrevCount: SizeInt;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property StringsObj: TATStrings read FStrings write FStrings;
    property VirtualMode: boolean read FVirtualMode write SetVirtualMode;
    function Count: integer;
    function IsIndexValid(AIndex: integer): boolean; inline;
    function IsIndexUniqueForLine(AIndex: integer): boolean;
    property Data[AIndex: integer]: TATWrapItem read GetData; default;
    procedure Add(const AData: TATWrapItem);
    procedure Delete(AIndex: integer);
    procedure Insert(AIndex: integer; const AItem: TATWrapItem);
    procedure FindIndexesOfLineNumber(ALineNum: SizeInt; out AFrom, ATo: integer);
    function FindIndexOfCaretPos(APos: TPoint): integer;
    procedure SetCapacity(AValue: integer);
    procedure ReplaceItems(AFrom, ATo: integer; AItems: TATWrapItems);
  end;


implementation

uses
  Math, Dialogs, Forms;

{ TATWrapItem }

procedure TATWrapItem.Init(ALineIndex, ACharIndex, ALength, AIndent: SizeInt;
  AFinal: TATWrapItemFinal; AInitial: boolean);
begin
  NLineIndex:= ALineIndex;
  NCharIndex:= ACharIndex;
  NLength:= ALength;
  NIndent:= AIndent;
  NFinal:= AFinal;
  bInitial:= AInitial;
end;

function TATWrapItem.ContainsPos(AX, AY: Int64): boolean;
begin
  Result:= false;
  if AY<>NLineIndex then exit;
  if AX<NCharIndex-1 then exit;
  if NFinal<>TATWrapItemFinal.Final then
    if AX>=NCharIndex-1+NLength then exit;
  Result:= true;
end;

class operator TATWrapItem.=(const A, B: TATWrapItem): boolean;
begin
  Result:= false;
end;

{ TATWrapInfo }

function TATWrapInfo.GetData(AIndex: integer): TATWrapItem;
begin
  if FVirtualMode then
  begin
    if FStrings.IsIndexValid(AIndex) then
      Result.Init(AIndex, 1, FStrings.LinesLen[AIndex], 0, TATWrapItemFinal.Final, true)
    else
      Result:= Default(TATWrapItem);
  end
  else
  begin
    if (AIndex>=0) and (AIndex<FList.Count) then
      Result:= FList[AIndex]
    else
      Result:= Default(TATWrapItem);
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

function TATWrapInfo.IsLineFolded(ALine: SizeInt): boolean;
begin
  if StringsObj.IsIndexValid(ALine) then
    Result:= StringsObj.LinesHidden[ALine, EditorIndex]
  else
    Result:= false;
end;

constructor TATWrapInfo.Create;
begin
  FList:= TATWrapItems.Create;
  FVirtualMode:= false;
  StringsPrevCount:= -1;
end;

destructor TATWrapInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATWrapInfo.Clear;
begin
  FList.Clear;
end;

function TATWrapInfo.Count: integer;
begin
  if FVirtualMode then
    Result:= FStrings.Count
  else
    Result:= FList.Count;
end;

function TATWrapInfo.IsIndexValid(AIndex: integer): boolean; inline;
begin
  Result:= (AIndex>=0) and (AIndex<Count);
end;

function TATWrapInfo.IsIndexUniqueForLine(AIndex: integer): boolean;
var
  NLineIndex: integer;
begin
  if FVirtualMode then
    Exit(true);
  NLineIndex:= FList._GetItemPtr(AIndex)^.NLineIndex;
  if (AIndex>0) and (FList._GetItemPtr(AIndex-1)^.NLineIndex=NLineIndex) then
    Exit(false);
  if (AIndex<FList.Count-1) and (FList._GetItemPtr(AIndex+1)^.NLineIndex=NLineIndex) then
    Exit(false);
  Result:= true;
end;

procedure TATWrapInfo.Add(const AData: TATWrapItem);
begin
  if FVirtualMode then exit;
  FList.Add(AData);
end;

procedure TATWrapInfo.Delete(AIndex: integer);
begin
  if FVirtualMode then exit;
  FList.Delete(AIndex);
end;

procedure TATWrapInfo.Insert(AIndex: integer; const AItem: TATWrapItem);
begin
  if FVirtualMode then exit;
  if AIndex>=FList.Count then
    FList.Add(AItem)
  else
    FList.Insert(AIndex, AItem);
end;

procedure TATWrapInfo.FindIndexesOfLineNumber(ALineNum: SizeInt; out AFrom, ATo: integer);
var
  a, b, m, dif: integer;
begin
  if FVirtualMode then
  begin
    AFrom:= ALineNum;
    ATo:= ALineNum;
    Exit;
  end;

  AFrom:= -1;
  ATo:= -1;

  if IsLineFolded(ALineNum) then Exit;

  a:= 0;
  b:= Count-1;

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    dif:= FList._GetItemPtr(m)^.NLineIndex-ALineNum;
    if dif=0 then
      Break;
    if dif>0 then
      b:= m-1
    else
      a:= m+1;
  until false;

  AFrom:= m;
  ATo:= m;
  while (AFrom>0) and (FList._GetItemPtr(AFrom-1)^.NLineIndex=ALineNum) do
    Dec(AFrom);
  while (ATo<Count-1) and (FList._GetItemPtr(ATo+1)^.NLineIndex=ALineNum) do
    Inc(ATo);
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

procedure TATWrapInfo.SetCapacity(AValue: integer);
begin
  FList.Capacity:= Max(1024, AValue);
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

