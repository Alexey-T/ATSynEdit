{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Gaps;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TATSynGapItem = class
  public
    LineIndex: integer;
    Size: integer;
  end;

type
  TATLineChangeKind = (
    cLineChangeEdited,
    cLineChangeAdded,
    cLineChangeDeleted,
    cLineChangeDeletedAll
    );

type
  { TATSynGaps }

  TATSynGaps = class
  private
    FList: TList;
    function GetItem(N: integer): TATSynGapItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATSynGapItem read GetItem; default;
    procedure Delete(N: integer);
    function DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
    function Add(ALineIndex, ASize: integer): boolean;
    function Find(ALineIndex: integer): TATSynGapItem;
    function SizeForLineRange(ALineFrom, ALineTo: integer): integer;
    procedure Update(ALine: integer; AChange: TATLineChangeKind);
  end;

implementation

const
  cMinGapSize = 10;
  cMaxGapSize = 500;

{ TATSynGaps }

constructor TATSynGaps.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATSynGaps.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynGaps.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TATSynGaps.Count: integer;
begin
  Result:= FList.Count;
end;

function TATSynGaps.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATSynGaps.GetItem(N: integer): TATSynGapItem;
begin
  Result:= TATSynGapItem(FList[N]);
end;

procedure TATSynGaps.Delete(N: integer);
begin
  TObject(FList[N]).Free;
  FList.Delete(N);
end;

function TATSynGaps.DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
var
  Item: TATSynGapItem;
  i: integer;
begin
  Result:= false;
  for i:= FList.Count-1 downto 0 do
  begin
    Item:= Items[i];
    if (Item.LineIndex>=ALineFrom) and (Item.LineIndex<=ALineTo) then
    begin
      Delete(i);
      Result:= true;
    end;
  end;
end;

function TATSynGaps.Add(ALineIndex, ASize: integer): boolean;
var
  Item: TATSynGapItem;
begin
  Result:= false;
  if (ALineIndex<0) then exit;
  if (ASize<cMinGapSize) or (ASize>cMaxGapSize) then exit;
  if Find(ALineIndex)<>nil then exit;

  Item:= TATSynGapItem.Create;
  Item.LineIndex:= ALineIndex;
  Item.Size:= ASize;
  FList.Add(Item);
  Result:= true;
end;

function TATSynGaps.Find(ALineIndex: integer): TATSynGapItem;
var
  Item: TATSynGapItem;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= Items[i];
    if Item.LineIndex=ALineIndex then exit(Item);
  end;
end;

function TATSynGaps.SizeForLineRange(ALineFrom, ALineTo: integer): integer;
var
  Item: TATSynGapItem;
  i: integer;
begin
  Result:= 0;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= Items[i];
    if (Item.LineIndex>=ALineFrom) and (Item.LineIndex<=ALineTo) then
      Inc(Result, Item.Size);
  end;
end;

procedure TATSynGaps.Update(ALine: integer;
  AChange: TATLineChangeKind);
var
  Item: TATSynGapItem;
  i: integer;
begin
  case AChange of
    cLineChangeEdited:
      begin
      end;
    cLineChangeAdded:
      begin
        for i:= 0 to Count-1 do
        begin
          Item:= Items[i];
          if Item.LineIndex>=ALine then
            Item.LineIndex:= Item.LineIndex+1;
        end;
      end;
    cLineChangeDeletedAll:
      begin
        Clear;
      end;
    cLineChangeDeleted:
      begin
        for i:= Count-1 downto 0 do
        begin
          Item:= Items[i];
          if Item.LineIndex=ALine then
            Delete(i)
          else
          if Item.LineIndex>ALine then
            Item.LineIndex:= Item.LineIndex-1;
        end;
      end;
  end;
end;


end.

