{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_Cache;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit_LineParts,
  ATSynEdit_fgl;

type

  { TATAdapterCacheItem }

  PATAdapterCacheItem = ^TATAdapterCacheItem;
  TATAdapterCacheItem = packed record
    LineIndex, CharIndex: integer;
    ColorAfterEol: TColor;
    Parts: TATLineParts;
    class operator=(const A, B: TATAdapterCacheItem): boolean;
  end;

  { TATAdapterCacheItems }

  TATAdapterCacheItems = specialize TFPGList<TATAdapterCacheItem>;

type
  { TATAdapterHiliteCache }

  TATAdapterHiliteCache = class
  private
    FList: TATAdapterCacheItems;
    FEnabled: boolean;
    FTempItem: TATAdapterCacheItem;
    procedure SetEnabled(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;
    procedure Clear;
    procedure Add(
      const ALineIndex, ACharIndex: integer;
      var AParts: TATLineParts;
      const AColorAfterEol: TColor);
    function Get(
      const ALineIndex, ACharIndex: integer;
      var AParts: TATLineParts;
      var AColorAfterEol: TColor): boolean;
    procedure DeleteForLine(ALineIndex: integer);
  end;

var
  OptEditorAdapterCacheSize: integer = 100;


implementation

procedure CopyLineParts(var A, B: TATLineParts);
var
  i: integer;
begin
  //safe but slower:
  //Move(A, B, SizeOf(A));

  for i:= 0 to High(A) do
  begin
    B[i]:= A[i];
    //stop at first empty item
    if A[i].Len=0 then
      Break;
  end;
end;

{ TATAdapterCacheItem }

class operator TATAdapterCacheItem.=(const A, B: TATAdapterCacheItem): boolean;
begin
  Result:= false;
end;

{ TATAdapterHiliteCache }

procedure TATAdapterHiliteCache.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:= AValue;
  if not AValue then Clear;
end;

constructor TATAdapterHiliteCache.Create;
begin
  FList:= TATAdapterCacheItems.Create;
end;

destructor TATAdapterHiliteCache.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATAdapterHiliteCache.Clear;
begin
  FList.Clear;
end;

procedure TATAdapterHiliteCache.Add(
  const ALineIndex, ACharIndex: integer;
  var AParts: TATLineParts;
  const AColorAfterEol: TColor);
begin
  if not Enabled then exit;
  if OptEditorAdapterCacheSize<10 then exit;

  //ignore if no parts
  if (AParts[0].Len=0) then exit;

  //ignore if single part
  //(some strange bug on macOS, cache gets items with single long part)
  if (AParts[1].Len=0) then exit;

  {
  //ignore if single part, and no bold/italic/underline attr
  //e.g. lexer didnt parse end of file yet
  if (AParts[1].Len=0) and
    (AParts[0].FontBold=false) and
    (AParts[0].FontItalic=false) and
    (AParts[0].FontStrikeOut=false)
    then exit;
    }

  if FList.Count>OptEditorAdapterCacheSize then
    FList.Count:= OptEditorAdapterCacheSize;

  FTempItem.LineIndex:= ALineIndex;
  FTempItem.CharIndex:= ACharIndex;
  FTempItem.ColorAfterEol:= AColorAfterEol;
  CopyLineParts(AParts, FTempItem.Parts);
  FList.Add(FTempItem);
end;


function TATAdapterHiliteCache.Get(
  const ALineIndex, ACharIndex: integer;
  var AParts: TATLineParts;
  var AColorAfterEol: TColor): boolean;
var
  Item: PATAdapterCacheItem;
  i: integer;
begin
  Result:= false;
  if not Enabled then exit;

  for i:= 0 to FList.Count-1 do
  begin
    Item:= FList._GetItemPtr(i);
    if (Item^.LineIndex=ALineIndex) and
      (Item^.CharIndex=ACharIndex) then
      begin
        CopyLineParts(Item^.Parts, AParts);
        AColorAfterEol:= Item^.ColorAfterEol;
        exit(true);
      end;
  end;
end;

procedure TATAdapterHiliteCache.DeleteForLine(ALineIndex: integer);
var
  Item: PATAdapterCacheItem;
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    Item:= FList._GetItemPtr(i);
    if (Item^.LineIndex=ALineIndex) then
      FList.Delete(i);
  end;
end;

end.

