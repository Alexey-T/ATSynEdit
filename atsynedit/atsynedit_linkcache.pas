{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_LinkCache;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils,
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
  private
    FMaxCount: integer;
    procedure SetMaxCount(AValue: integer);
  public
    property MaxCount: integer read FMaxCount write SetMaxCount;
    constructor Create;
    function FindData(ALineIndex: integer): PATLinkCacheItem;
    procedure AddData(ALineIndex: integer; const AData: TATLinkArray);
    procedure DeleteData(ALineIndex: integer);
    function DebugText: string;
  end;

implementation

{ TATLinkCacheItem }

class operator TATLinkCacheItem.=(const a, b: TATLinkCacheItem): boolean;
begin
  Result:= false;
end;

{ TATLinkCache }

procedure TATLinkCache.SetMaxCount(AValue: integer);
begin
  if FMaxCount=AValue then Exit;
  FMaxCount:= AValue;
  if FMaxCount<2 then
    FMaxCount:= 2;

  while Count>FMaxCount do
    Delete(0);
end;

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
    Delete(0);

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

function TATLinkCache.DebugText: string;
var
  Ptr: PATLinkCacheItem;
  iCache, iPair: integer;
begin
  Result:= '';
  for iCache:= 0 to Count-1 do
  begin
    Ptr:= InternalGet(iCache);
    Result+= '['+IntToStr(Ptr^.LineIndex)+'] ';
    for iPair:= 0 to High(TATLinkArray) do
      with Ptr^.Data[iPair] do
      begin
        if NLen=0 then Break;
        Result+= IntToStr(NFrom)+','+IntToStr(NLen)+' ';
      end;
  end;
end;

end.

