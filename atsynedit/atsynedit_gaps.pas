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
  { TATGapItem }

  TATGapItem = class
  public
    LineIndex: integer;
    Size: integer;
    Color: TColor;
    Bitmap: TBitmap;
    Tag: Int64;
    DeleteOnDelLine: boolean;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type
  TATGapDeleteEvent = procedure(Sender: TObject; ALineIndex: integer) of object;

type
  { TATGaps }

  TATGaps = class
  private
    FList: TList;
    FOnDelete: TATGapDeleteEvent;
    FSizeOfGapTop: integer;
    function GetItem(N: integer): TATGapItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATGapItem read GetItem; default;
    procedure Delete(N: integer);
    function Add(ALineIndex, ASize: integer; ABitmap: TBitmap; const ATag: Int64;
      AColor: TColor=clNone; ADeleteOnDelLine: boolean=true): boolean;
    function Find(ALineIndex: integer; ATag: Int64=-1): TATGapItem;
    function DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
    function SizeForLineRange(ALineFrom, ALineTo: integer): integer;
    function SizeForAll: integer;
    property SizeOfGapTop: integer read FSizeOfGapTop;
    procedure Update(AChange: TATLineChangeKind; ALine, AItemCount: integer);
    property OnDelete: TATGapDeleteEvent read FOnDelete write FOnDelete;
  end;

function GetGapBitmapPosLeft(const ARect: TRect; ABitmap: TBitmap): integer;


implementation

function GetGapBitmapPosLeft(const ARect: TRect; ABitmap: TBitmap): integer;
begin
  Result:= Max(ARect.Left, (ARect.Left+ARect.Right-ABitmap.Width) div 2);
end;


{ TATGapItem }

constructor TATGapItem.Create;
begin
  Bitmap:= nil;
  DeleteOnDelLine:= true;
  Color:= clNone;
end;

destructor TATGapItem.Destroy;
begin
  if Bitmap<>nil then
    FreeAndNil(Bitmap);
  inherited;
end;

{ TATGaps }

constructor TATGaps.Create;
begin
  inherited;
  FList:= TList.Create;
  FSizeOfGapTop:= 0;
end;

destructor TATGaps.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATGaps.Clear;
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
  FSizeOfGapTop:= 0;
end;

function TATGaps.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATGaps.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATGaps.GetItem(N: integer): TATGapItem;
begin
  Result:= TATGapItem(FList[N]);
end;

procedure TATGaps.Delete(N: integer);
var
  Item: TATGapItem;
begin
  Item:= Items[N];
  if Assigned(FOnDelete) then
    FOnDelete(Self, Item.LineIndex);

  if Item.LineIndex=-1 then
    FSizeOfGapTop:= 0;

  Item.Free;
  FList.Delete(N);
end;

function TATGaps.DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
var
  Item: TATGapItem;
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

function TATGaps.Add(ALineIndex, ASize: integer; ABitmap: TBitmap;
  const ATag: Int64; AColor: TColor; ADeleteOnDelLine: boolean): boolean;
var
  Item: TATGapItem;
begin
  Result:= false;
  if (ALineIndex<-1) then exit;
  if Find(ALineIndex)<>nil then exit;

  Item:= TATGapItem.Create;
  Item.LineIndex:= ALineIndex;
  Item.Size:= ASize;
  Item.Bitmap:= ABitmap;
  Item.Tag:= ATag;
  Item.Color:= AColor;
  Item.DeleteOnDelLine:= ADeleteOnDelLine;

  if ALineIndex=-1 then
    FSizeOfGapTop:= ASize;

  FList.Add(Item);
  Result:= true;
end;

function TATGaps.Find(ALineIndex: integer; ATag: Int64=-1): TATGapItem;
var
  Item: TATGapItem;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= Items[i];
    if (Item.LineIndex=ALineIndex) and ((ATag<0) or (Item.Tag=ATag)) then exit(Item);
  end;
end;

function TATGaps.SizeForLineRange(ALineFrom, ALineTo: integer): integer;
var
  Item: TATGapItem;
  i: integer;
begin
  Result:= 0;

  //support gap before 1st line
  if ALineFrom=0 then
    ALineFrom:= -1;

  for i:= 0 to FList.Count-1 do
  begin
    Item:= Items[i];
    if (Item.LineIndex>=ALineFrom) and (Item.LineIndex<=ALineTo) then
      Inc(Result, Item.Size);
  end;
end;

function TATGaps.SizeForAll: integer;
var
  Item: TATGapItem;
  i: integer;
begin
  Result:= 0;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= Items[i];
    Inc(Result, Item.Size);
  end;
end;

procedure TATGaps.Update(AChange: TATLineChangeKind; ALine, AItemCount: integer);
var
  Item: TATGapItem;
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
            Item.LineIndex:= Item.LineIndex+AItemCount;
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
          if (Item.LineIndex>=ALine) and (Item.LineIndex<ALine+AItemCount) then
          begin
            if Item.DeleteOnDelLine then
              Delete(i);
          end
          else
          if Item.LineIndex>ALine then
            Item.LineIndex:= Item.LineIndex-AItemCount;
        end;
      end;
  end;
end;


end.

