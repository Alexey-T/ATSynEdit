{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_LinkCache;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  ATSynEdit_fgl;

const
  cMaxLinksPerLine = 6;

type
  TATLinkPair = record
    NFrom, NLen: integer;
  end;

  TATLinkArray = array[0..cMaxLinksPerLine-1] of TATLinkPair;

type
  { TATLinkCacheItem }

  PATLinkCacheItem = ^TATLinkCacheItem;
  TATLinkCacheItem = record
    LineIndex: integer;
    Data: TATLinkArray;
    class operator =(const a, b: TATLinkCacheItem): boolean;
  end;

  TATLinkCache = class(specialize TFPGList<TATLinkCacheItem>)
  public
    MaxCount: integer;
    constructor Create;
    function FindData(ALineIndex: integer): PATLinkCacheItem;
    procedure AddData(ALineIndex: integer; const AData: TATLinkArray);
    procedure DeleteData(ALineIndex: integer);
  end;

implementation

{ TATLinkCacheItem }

class operator TATLinkCacheItem.=(const a, b: TATLinkCacheItem): boolean;
begin
  Result:= false;
end;

{ TATLinkCache }

constructor TATLinkCache.Create;
begin
  inherited Create;
  MaxCount:= 80;
end;

function TATLinkCache.FindData(ALineIndex: integer): PATLinkCacheItem;
var
  Ptr: PATLinkCacheItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Ptr:= InternalGet(i);
    if Ptr^.LineIndex=ALineIndex then
      exit(Ptr);
  end;
  Result:= nil;
end;

procedure TATLinkCache.AddData(ALineIndex: integer; const AData: TATLinkArray);
var
  Item: TATLinkCacheItem;
begin
  while Count>MaxCount do
    Delete(Count-1);

  Item.LineIndex:= ALineIndex;
  Item.Data:= AData;
  Add(Item);
end;

procedure TATLinkCache.DeleteData(ALineIndex: integer);
var
  Ptr: PATLinkCacheItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Ptr:= InternalGet(i);
    if Ptr^.LineIndex=ALineIndex then
    begin
      Delete(i);
      exit;
    end;
  end;
end;

end.

