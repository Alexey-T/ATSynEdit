{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Gaps;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Math;

type
  TATLineChangeKind = (
    cLineChangeEdited,
    cLineChangeAdded,
    cLineChangeDeleted,
    cLineChangeDeletedAll
    );

type
  { TATSynGapItem }

  TATSynGapItem = class
  public
    LineIndex: integer;
    Size: integer;
    Bitmap: TBitmap;
    Tag: Int64;
    Control: TControl;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type
  TATSynGapDelete = procedure(Sender: TObject; ALineIndex: integer) of object;

type
  { TATSynGaps }

  TATSynGaps = class
  private
    FList: TList;
    FOnDelete: TATSynGapDelete;
    function GetItem(N: integer): TATSynGapItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATSynGapItem read GetItem; default;
    procedure Delete(N: integer);
    function Add(ALineIndex, ASize: integer; ABitmap: TBitmap; ATag: Int64): boolean;
    function Find(ALineIndex: integer; ATag: Int64=-1): TATSynGapItem;
    function DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
    function SizeForLineRange(ALineFrom, ALineTo: integer): integer;
    procedure Update(ALine: integer; AChange: TATLineChangeKind);
    property OnDelete: TATSynGapDelete read FOnDelete write FOnDelete;
  end;

var
  cMinGapSize: integer = 8;
  cMaxGapSize: integer = 500;

function GetGapBitmapPosLeft(const ARect: TRect; ABitmap: TBitmap): integer;


implementation

function GetGapBitmapPosLeft(const ARect: TRect; ABitmap: TBitmap): integer;
begin
  Result:= Max(ARect.Left, (ARect.Left+ARect.Right-ABitmap.Width) div 2);
end;


{ TATSynGapItem }

constructor TATSynGapItem.Create;
begin
  Bitmap:= nil;
end;

destructor TATSynGapItem.Destroy;
begin
  if Bitmap<>nil then
    FreeAndNil(Bitmap);
  inherited;
end;

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
  begin
    if Assigned(FOnDelete) then
      FOnDelete(Self, Items[i].LineIndex);
    TObject(FList[i]).Free;
  end;
  FList.Clear;
end;

function TATSynGaps.Count: integer; inline;
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
  if Assigned(FOnDelete) then
    FOnDelete(Self, Items[N].LineIndex);

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

function TATSynGaps.Add(ALineIndex, ASize: integer; ABitmap: TBitmap;
  ATag: Int64): boolean;
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
  Item.Bitmap:= ABitmap;
  Item.Tag:= ATag;

  FList.Add(Item);
  Result:= true;
end;

function TATSynGaps.Find(ALineIndex: integer; ATag: Int64=-1): TATSynGapItem;
var
  Item: TATSynGapItem;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= Items[i];
    if (Item.LineIndex=ALineIndex) and ((ATag<0) or (Item.Tag=ATag)) then exit(Item);
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

