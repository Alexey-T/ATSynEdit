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
  { TATBookmarkItem }

  TATBookmarkItem = class
  public
    LineNum: integer;
    Kind: word;
    Hint: string;
    constructor Create(ALineNum: integer; AKind: word; const AHint: string);
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
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATBookmarkItem read GetItem; default;
    procedure Add(ALineNum: integer; AKind: word; const AHint: string);
    function Find(ALineNum: integer): integer;
    procedure DeleteDups;
    procedure Update(ALine: integer; AChange: TATLineChangeKind; ALineCount: integer);
  end;

implementation

{ TATBookmarkItem }

constructor TATBookmarkItem.Create(ALineNum: integer; AKind: word;
  const AHint: string);
begin
  LineNum:= ALineNum;
  Kind:= AKind;
  Hint:= AHint;
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

procedure TATBookmarks.Delete(N: integer);
begin
  TObject(FList[N]).Free;
  FList.Delete(N);
end;

procedure TATBookmarks.DeleteForLine(ALine: integer);
var
  N: integer;
begin
  N:= Find(ALine);
  if N>=0 then
    Delete(N);
end;

function TATBookmarks.Count: integer;
begin
  Result:= FList.Count;
end;

function TATBookmarks.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

procedure TATBookmarks.Add(ALineNum: integer; AKind: word; const AHint: string);
var
  Item: TATBookmarkItem;
  nLine, i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    nLine:= Item.LineNum;

    //bookmark already exists: overwrite
    if nLine=ALineNum then
    begin
      Item.LineNum:= ALineNum;
      Item.Kind:= AKind;
      Item.Hint:= AHint;
      Exit
    end;

    //found bookmark for bigger line: insert before it
    if nLine>ALineNum then
    begin
      FList.Insert(i, TATBookmarkItem.Create(ALineNum, AKind, AHint));
      Exit;
    end;
  end;

  //not found bookmark for bigger line: append
  FList.Add(TATBookmarkItem.Create(ALineNum, AKind, AHint));
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
    if Item1.LineNum=Item2.LineNum then
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
    dif:= Items[a].LineNum-ALineNum;
    if dif=0 then exit(a);

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= Items[m].LineNum-ALineNum;
    if dif=0 then exit(m);

    if Abs(a-b)<=1 then exit;
    if dif>0 then b:= m else a:= m;
  until false;
end;


procedure TATBookmarks.Update(ALine: integer; AChange: TATLineChangeKind;
  ALineCount: integer);
var
  Item: TATBookmarkItem;
  bMovedHere: boolean;
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
          if Item.LineNum>=ALine then
            Item.LineNum:= Item.LineNum+1;
        end;
      end;

    cLineChangeDeletedAll:
      begin
        Clear;
      end;

    cLineChangeDeleted:
      begin
        NIndexPlaced:= Find(ALine);
        bMovedHere:= false;

        for i:= Count-1 downto 0 do
        begin
          Item:= Items[i];
          //spec case for bookmark on last line, keep it if deleting last line
          if (Item.LineNum>ALine) or (Item.LineNum=ALineCount-1) then
          begin
            Item.LineNum:= Item.LineNum-1;
            if Item.LineNum=ALine then
              bMovedHere:= true;
          end;
        end;

        //delete new duplicate
        if bMovedHere then
          if NIndexPlaced>=0 then
            Delete(NIndexPlaced);
      end;
  end;
end;


end.
