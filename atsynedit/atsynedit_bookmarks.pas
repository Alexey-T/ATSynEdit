{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Bookmarks;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATSynEdit_Globals,
  ATSynEdit_FGL;

type
  TATBookmarkAutoDelete = (
    DontDelete,
    Delete,
    ByOption
    );

type
  { TATBookmarkData }

  TATBookmarkData = packed record
    Tag: integer;
    LineNum: integer;
    Kind: word;
    AutoDelete: TATBookmarkAutoDelete;
    ShowInBookmarkList: boolean;
    Hint: PChar;
  end;

  { TATBookmarkItem }

  PATBookmarkItem = ^TATBookmarkItem;
  TATBookmarkItem = record
    Data: TATBookmarkData;
    class operator =(const a, b: TATBookmarkItem): boolean;
    constructor Assign(const AData: TATBookmarkData);
  end;

  { TATBookmarkItems }

  TATBookmarkItems = class(specialize TFPGList<TATBookmarkItem>)
  protected
    procedure Deref(Item: Pointer); override;
  public
    function ItemPtr(AIndex: integer): PATBookmarkItem;
  end;

type
  { TATBookmarks }

  TATBookmarks = class
  private
    FList: TATBookmarkItems;
    FModified: boolean;
    function GetItemPtr(N: integer): PATBookmarkItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteForLine(ALine: integer);
    function DeleteByTag(ATag: integer): boolean;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property ItemPtr[N: integer]: PATBookmarkItem read GetItemPtr; default;
    procedure Add(const AData: TATBookmarkData; AToggle: boolean=false);
    function Find(ALineNum: integer): integer;
    function FindNearest(ALineNum: integer; out AExactMatch: boolean): integer;
    function FindHintForLine(ALineNum: integer): string;
    procedure DeleteDups;
    procedure Update(AChange: TATLineChangeKind; ALine, AItemCount, ALineCount: integer);
    property Modified: boolean read FModified write FModified;
  end;

implementation

uses
  Math;

{ TATBookmarkItems }

procedure TATBookmarkItems.Deref(Item: Pointer);
begin
  with PATBookmarkItem(Item)^ do
    if Data.Hint<>nil then
    begin
      StrDispose(Data.Hint);
      Data.Hint:= nil;
    end;
end;

function TATBookmarkItems.ItemPtr(AIndex: integer): PATBookmarkItem;
begin
  Result:= PATBookmarkItem(InternalGet(AIndex));
end;

{ TATBookmarkItem }

class operator TATBookmarkItem.=(const a, b: TATBookmarkItem): boolean;
begin
  Result:= false;
end;

constructor TATBookmarkItem.Assign(const AData: TATBookmarkData);
begin
  Data.Tag:= AData.Tag;
  Data.LineNum:= AData.LineNum;
  Data.Kind:= AData.Kind;
  Data.AutoDelete:= AData.AutoDelete;
  Data.ShowInBookmarkList:= AData.ShowInBookmarkList;
  if Data.Hint<>nil then
    StrDispose(Data.Hint);
  Data.Hint:= StrNew(AData.Hint);
end;

{ TATBookmarks }

function TATBookmarks.GetItemPtr(N: integer): PATBookmarkItem;
begin
  Result:= FList.ItemPtr(N);
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

function TATBookmarks.DeleteByTag(ATag: integer): boolean;
var
  i, j: integer;
begin
  Result:= false;
  i:= FList.Count;
  repeat
    Dec(i);
    if i<0 then Break;
    if FList[i].Data.Tag=ATag then
    begin
      Result:= true;
      j:= i;
      while (j>0) and (FList[j-1].Data.Tag=ATag) do
        Dec(j);
      FList.DeleteRange(j, i);
      i:= j;
    end;
  until false;
end;

function TATBookmarks.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATBookmarks.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

procedure TATBookmarks.Add(const AData: TATBookmarkData; AToggle: boolean=false);
var
  Item: TATBookmarkItem;
  N: integer;
  bExact: boolean;
begin
  N:= FindNearest(AData.LineNum, bExact);
  if N<0 then
  begin
    //not found bookmark for bigger line: append
    Item:= Default(TATBookmarkItem);
    Item.Assign(AData);
    FList.Add(Item);
  end
  else
  begin
    if bExact then
    begin
      //bookmark exists
      if AToggle then
        FList.Delete(N)
      else
      begin
        Item:= Default(TATBookmarkItem);
        Item.Assign(AData);
        ItemPtr[N]^.Assign(AData);
      end;
    end
    else
    begin
      //found bookmark for bigger line: insert before it
      Item:= Default(TATBookmarkItem);
      Item.Assign(AData);
      FList.Insert(N, Item);
    end;
  end;
end;

procedure TATBookmarks.DeleteDups;
var
  Item1, Item2: PATBookmarkItem;
  i: integer;
begin
  for i:= Count-1 downto 1 do
  begin
    Item1:= GetItemPtr(i);
    Item2:= GetItemPtr(i-1);
    if Item1^.Data.LineNum=Item2^.Data.LineNum then
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

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    dif:= FList.ItemPtr(m)^.Data.LineNum-ALineNum;
    if dif=0 then
      exit(m);
    if dif>0 then
      b:= m-1
    else
      a:= m+1;
  until false;
end;

function TATBookmarks.FindNearest(ALineNum: integer; out AExactMatch: boolean): integer;
var
  a, b, m, dif: integer;
begin
  Result:= -1;
  AExactMatch:= false;

  a:= 0;
  b:= Count-1;

  repeat
    if a>b then
    begin
      while IsIndexValid(a-1) and (FList.ItemPtr(a-1)^.Data.LineNum>=ALineNum) do
        Dec(a);
      if IsIndexValid(a) then
        Result:= a;
      exit;
    end;

    m:= (a+b+1) div 2;

    dif:= FList.ItemPtr(m)^.Data.LineNum-ALineNum;
    if dif=0 then
    begin
      AExactMatch:= true;
      exit(m);
    end;
    if dif>0 then
      b:= m-1
    else
      a:= m+1;
  until false;
end;

function TATBookmarks.FindHintForLine(ALineNum: integer): string;
var
  N: integer;
begin
  Result:= '';
  N:= Find(ALineNum);
  if N>=0 then
    Result:= GetItemPtr(N)^.Data.Hint;
end;


procedure TATBookmarks.Update(AChange: TATLineChangeKind;
  ALine, AItemCount, ALineCount: integer);
var
  Item: PATBookmarkItem;
  NIndexPlaced, NewLine, i: integer;
  fAutoDel: TATBookmarkAutoDelete;
begin
  case AChange of
    TATLineChangeKind.Edited:
      begin
      end;

    TATLineChangeKind.Added:
      begin
        for i:= 0 to Count-1 do
        begin
          Item:= FList.ItemPtr(i);
          if Item^.Data.LineNum>=ALine then
          begin
            Item^.Data.LineNum+= AItemCount;
            FModified:= true;
          end;
        end;
      end;

    TATLineChangeKind.DeletedAll:
      begin
        if Count>0 then
        begin
          Clear;
          FModified:= true;
        end;
      end;

    TATLineChangeKind.Deleted:
      begin
        for i:= 0 to AItemCount-1 do
        begin
          NIndexPlaced:= Find(ALine+i);

          if (NIndexPlaced>=0) then
          begin
            fAutoDel:= FList.ItemPtr(NIndexPlaced)^.Data.AutoDelete;
            if (fAutoDel=TATBookmarkAutoDelete.Delete) or ((fAutoDel=TATBookmarkAutoDelete.ByOption) and ATEditorOptions.BookmarksAutoDelete) then
            begin
              Delete(NIndexPlaced);
              FModified:= true;
              NIndexPlaced:= -1;
            end;
          end;
        end;

        for i:= Count-1 downto 0 do
        begin
          Item:= FList.ItemPtr(i);

          //spec case for bookmark on last line, keep it if deleting last line
          if (Item^.Data.LineNum>ALine) or (Item^.Data.LineNum=ALineCount-1) then
          begin
            NewLine:= Max(Item^.Data.LineNum-AItemCount, ALine);
            Item^.Data.LineNum:= NewLine;
            FModified:= true;
          end;
        end;

        //dups may appear, if many bookmarks were in 1 block
        DeleteDups;
      end;
  end;
end;


end.
