{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Bookmarks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATBookmarkItem read GetItem; default;
    procedure Add(ALineNum: integer; AKind: word; const AHint: string);
    function Find(ALineNum: integer): integer;
    procedure DeleteDups;
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
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
  begin
    if Items[i].LineNum=ALineNum then 
      exit(i);
  end;
end;

end.
