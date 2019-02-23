{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Bookmarks;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Gaps,
  ATSynEdit_FGL;

type
  { TATBookmarkData }

  TATBookmarkData = packed record
    Tag: Int64;
    Hint: string[55];
    LineNum: integer;
    Kind: word;
    DeleteOnDelLine: boolean;
    ShowInBookmarkList: boolean;
  end;

  { TATBookmarkItem }

  TATBookmarkItem = record
    Data: TATBookmarkData;
    class operator =(const a, b: TATBookmarkItem): boolean;
    constructor Assign(const AData: TATBookmarkData);
  end;

  TATBookmarkItems = specialize TFPGList<TATBookmarkItem>;

type
  { TATBookmarks }

  TATBookmarks = class
  private
    FList: TATBookmarkItems;
    function GetItem(N: integer): TATBookmarkItem;
    procedure SetItem(N: integer; const AValue: TATBookmarkItem);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteForLine(ALine: integer);
    function DeleteByTag(const ATag: Int64): boolean;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATBookmarkItem read GetItem write SetItem; default;
    procedure Add(const AData: TATBookmarkData);
    function Find(ALineNum: integer): integer;
    procedure DeleteDups;
    procedure Update(AChange: TATLineChangeKind; ALine, AItemCount, ALineCount: integer);
  end;

implementation

{ TATBookmarkItem }

class operator TATBookmarkItem.=(const a, b: TATBookmarkItem): boolean;
begin
  Result:= false;
end;

constructor TATBookmarkItem.Assign(const AData: TATBookmarkData);
begin
  Data:= AData;
end;

{ TATBookmarks }

function TATBookmarks.GetItem(N: integer): TATBookmarkItem;
begin
  Result:= FList[N];
end;

procedure TATBookmarks.SetItem(N: integer; const AValue: TATBookmarkItem);
begin
  FList[N]:= AValue;
end;

constructor TATBookmarks.Create;
begin
  inherited;
  FList:= TATBookmarkItems.Create;
end;

destructor TATBookmarks.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATBookmarks.Clear;
begin
  FList.Clear;
end;

procedure TATBookmarks.Delete(N: integer); inline;
begin
  FList.Delete(N);
end;

procedure TATBookmarks.DeleteForLine(ALine: integer); inline;
var
  N: integer;
begin
  N:= Find(ALine);
  if N>=0 then
    Delete(N);
end;

function TATBookmarks.DeleteByTag(const ATag: Int64): boolean;
var
  i: integer;
begin
  Result:= false;
  for i:= FList.Count-1 downto 0 do
    if FList[i].Data.Tag=ATag then
    begin
      Result:= true;
      Delete(i);
    end;
end;

function TATBookmarks.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATBookmarks.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

procedure TATBookmarks.Add(const AData: TATBookmarkData);
var
  Item: TATBookmarkItem;
  nLine, i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    nLine:= Items[i].Data.LineNum;

    //bookmark already exists: overwrite
    if nLine=AData.LineNum then
    begin
      Item.Assign(AData);
      Items[i]:= Item;
      Exit
    end;

    //found bookmark for bigger line: insert before it
    if nLine>AData.LineNum then
    begin
      Item.Assign(AData);
      FList.Insert(i, Item);
      Exit;
    end;
  end;

  //not found bookmark for bigger line: append
  Item.Assign(AData);
  FList.Add(Item);
end;

procedure TATBookmarks.DeleteDups;
var
  Item1, Item2: TATBookmarkItem;
  i: integer;
begin
  for i:= Count-1 downto 1 do
  begin
    Item1:= GetItem(i);
    Item2:= GetItem(i-1);
    if Item1.Data.LineNum=Item2.Data.LineNum then
      Delete(i);
  end;
end;

function TATBookmarks.Find(ALineNum: integer): integer;
var
  a, b, m, dif: integer;
begin
  Result:= -1;
  a:= 0;
  b:= Count-1;
  if b<0 then Exit;

  repeat
    dif:= Items[a].Data.LineNum-ALineNum;
    if dif=0 then exit(a);

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= Items[m].Data.LineNum-ALineNum;
    if dif=0 then exit(m);

    if Abs(a-b)<=1 then exit;
    if dif>0 then b:= m else a:= m;
  until false;
end;


procedure TATBookmarks.Update(AChange: TATLineChangeKind;
  ALine, AItemCount, ALineCount: integer);
var
  Item: TATBookmarkItem;
  //bMovedHere: boolean;
  NIndexPlaced, i: integer;
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
          if Item.Data.LineNum>=ALine then
          begin
            Item.Data.LineNum:= Item.Data.LineNum+AItemCount;
            Items[i]:= Item;
          end;
        end;
      end;

    cLineChangeDeletedAll:
      begin
        Clear;
      end;

    cLineChangeDeleted:
      begin
        for i:= 0 to AItemCount-1 do
        begin
          NIndexPlaced:= Find(ALine+i);
          //bMovedHere:= false;

          if (NIndexPlaced>=0) and Items[NIndexPlaced].Data.DeleteOnDelLine then
          begin
            Delete(NIndexPlaced);
            NIndexPlaced:= -1;
          end;
        end;

        for i:= Count-1 downto 0 do
        begin
          Item:= Items[i];

          //spec case for bookmark on last line, keep it if deleting last line
          if (Item.Data.LineNum>ALine) or (Item.Data.LineNum=ALineCount-1) then
          begin
            Item.Data.LineNum:= Item.Data.LineNum-AItemCount;
            Items[i]:= Item;
            {
            if Item.Data.LineNum=ALine then
              bMovedHere:= true;
            }
          end;
        end;

        {
        //delete new duplicate
        if bMovedHere then
          if NIndexPlaced>=0 then
            Delete(NIndexPlaced);
        }
      end;
  end;
end;


end.
