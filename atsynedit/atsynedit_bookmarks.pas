{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Gaps;

type
  TATBookmarkData = packed record
    Tag: Int64;
    LineNum: integer;
    Hint: string;
    Kind: word;
    DeleteOnDelLine: boolean;
    ShowInBookmarkList: boolean;
  end;

  { TATBookmarkItem }

  TATBookmarkItem = class
  public
    Data: TATBookmarkData;
    constructor Create(const AData: TATBookmarkData);
  end;

type
  { TATBookmarks }

  TATBookmarks = class
  private
    FList: TList;
    function GetItem(N: integer): TATBookmarkItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteForLine(ALine: integer);
    function DeleteByTag(const ATag: Int64): boolean;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATBookmarkItem read GetItem; default;
    procedure Add(const AData: TATBookmarkData);
    function Find(ALineNum: integer): integer;
    procedure DeleteDups;
    procedure Update(AChange: TATLineChangeKind; ALine, AItemCount, ALineCount: integer);
  end;

implementation

procedure CopyBookmarkData(const Src: TATBookmarkData; var Dest: TATBookmarkData);
begin
  Dest.Tag:= Src.Tag;
  Dest.LineNum:= Src.LineNum;
  Dest.Hint:= Src.Hint;
  Dest.Kind:= Src.Kind;
  Dest.DeleteOnDelLine:= Src.DeleteOnDelLine;
  Dest.ShowInBookmarkList:= Src.ShowInBookmarkList;
end;

{ TATBookmarkItem }

constructor TATBookmarkItem.Create(const AData: TATBookmarkData);
begin
  CopyBookmarkData(AData, Data);
end;

{ TATBookmarks }

function TATBookmarks.GetItem(N: integer): TATBookmarkItem;
begin
  Result:= TATBookmarkItem(FList[N]);
end;

constructor TATBookmarks.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATBookmarks.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATBookmarks.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

procedure TATBookmarks.Delete(N: integer); inline;
begin
  TObject(FList[N]).Free;
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
    if TATBookmarkItem(FList[i]).Data.Tag=ATag then
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
    Item:= Items[i];
    nLine:= Item.Data.LineNum;

    //bookmark already exists: overwrite
    if nLine=AData.LineNum then
    begin
      CopyBookmarkData(AData, Item.Data);
      Exit
    end;

    //found bookmark for bigger line: insert before it
    if nLine>AData.LineNum then
    begin
      FList.Insert(i, TATBookmarkItem.Create(AData));
      Exit;
    end;
  end;

  //not found bookmark for bigger line: append
  FList.Add(TATBookmarkItem.Create(AData));
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
            Item.Data.LineNum:= Item.Data.LineNum+AItemCount;
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
