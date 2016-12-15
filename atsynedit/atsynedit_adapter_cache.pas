{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_Cache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit_CanvasProc;

type
  TATAdapterHiliteCacheItem = class
  public
    LineIndex, CharIndex, LineLen: integer;
    Parts: TATLineParts;
    ColorAfterEol: TColor;
  end;

type
  { TATAdapterHiliteCache }

  TATAdapterHiliteCache = class
  private
    FList: TList;
    FMaxCount: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(
      ALineIndex, ACharIndex, ALineLen: integer;
      const AParts: TATLineParts;
      AColorAfterEol: TColor);
    function Get(
      var ALineIndex, ACharIndex, ALineLen: integer;
      var AParts: TATLineParts;
      var AColorAfterEol: TColor): boolean;
  end;


implementation

{ TATAdapterHiliteCache }

constructor TATAdapterHiliteCache.Create;
begin
  FList:= TList.Create;
  FMaxCount:= 200;
end;

destructor TATAdapterHiliteCache.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATAdapterHiliteCache.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    TObject(FList[i]).Free;
    FList.Delete(i);
  end;
end;

procedure TATAdapterHiliteCache.Add(ALineIndex, ACharIndex, ALineLen: integer;
  const AParts: TATLineParts; AColorAfterEol: TColor);
var
  N: integer;
  Item: TATAdapterHiliteCacheItem;
begin
  //new item adds to begin, last item deleted
  if FList.Count>=FMaxCount then
  begin
    N:= FList.Count-1;
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;

  Item:= TATAdapterHiliteCacheItem.Create;
  Item.LineIndex:= ALineIndex;
  Item.CharIndex:= ACharIndex;
  Item.LineLen:= ALineLen;
  Item.ColorAfterEol:= AColorAfterEol;
  Move(AParts, Item.Parts, SizeOf(AParts));
  FList.Insert(0, Item);
end;

function TATAdapterHiliteCache.Get(
  var ALineIndex, ACharIndex, ALineLen: integer;
  var AParts: TATLineParts;
  var AColorAfterEol: TColor): boolean;
var
  Item: TATAdapterHiliteCacheItem;
  i: integer;
begin
  Result:= false;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= TATAdapterHiliteCacheItem(FList[i]);
    if (Item.LineIndex=ALineIndex) and
      (Item.CharIndex=ACharIndex) and
      (Item.LineLen=ALineLen) then
      begin
        Move(Item.Parts, AParts, SizeOf(AParts));
        AColorAfterEol:= Item.ColorAfterEol;
        Result:= true;
        exit
      end;
  end;
end;

end.

