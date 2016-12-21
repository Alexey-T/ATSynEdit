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
    Height: integer;
  end;

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
    procedure Delete(N: integer);
    function Count: integer;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATSynGapItem read GetItem; default;
    procedure Add(ALineIndex, AHeight: integer);
    function Find(ALineIndex: integer): TATSynGapItem;
  end;

implementation

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

procedure TATSynGaps.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    Items[N].Free;
    FList.Delete(N);
  end;
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
  if IsIndexValid(N) then
    Result:= TATSynGapItem(FList[N])
  else
    Result:= nil;
end;

procedure TATSynGaps.Add(ALineIndex, AHeight: integer);
var
  Item: TATSynGapItem;
begin
  Item:= TATSynGapItem.Create;
  Item.LineIndex:= ALineIndex;
  Item.Height:= AHeight;
  FList.Add(Item);
end;

function TATSynGaps.Find(ALineIndex: integer): TATSynGapItem;
var
  Item: TATSynGapItem;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= TATSynGapItem(FList[i]);
    if Item.LineIndex=ALineIndex then exit(Item);
  end;
end;


end.

