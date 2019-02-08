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
    Bitmap: TBitmap;
    Tag: Int64;
    DeleteOnDelLine: boolean;
    Control: TControl;
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
      ADeleteOnDelLine: boolean=true): boolean;
    function Find(ALineIndex: integer; ATag: Int64=-1): TATGapItem;
    function DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
    function SizeForLineRange(ALineFrom, ALineTo: integer): integer;
    procedure Update(AChange: TATLineChangeKind; ALine, AItemCount: integer);
    property OnDelete: TATGapDeleteEvent read FOnDelete write FOnDelete;
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


{ TATGapItem }

constructor TATGapItem.Create;
begin
  Bitmap:= nil;
  DeleteOnDelLine:= true;
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
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, Items[N].LineIndex);

  TObject(FList[N]).Free;
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

function TATGaps.Add(ALineIndex, ASize: integer; ABitmap: TBitmap; const ATag: Int64;
  ADeleteOnDelLine: boolean): boolean;
var
  Item: TATGapItem;
begin
  Result:= false;
  if (ALineIndex<-1) then exit;
  if (ASize<cMinGapSize) or (ASize>cMaxGapSize) then exit;
  if Find(ALineIndex)<>nil then exit;

  Item:= TATGapItem.Create;
  Item.LineIndex:= ALineIndex;
  Item.Size:= ASize;
  Item.Bitmap:= ABitmap;
  Item.Tag:= ATag;
  Item.DeleteOnDelLine:= ADeleteOnDelLine;

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

