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
  ATSynEdit_CanvasProc,
  ATSynEdit_fgl;

type

  { TATAdapterCacheItem }

  PATAdapterCacheItem = ^TATAdapterCacheItem;
  TATAdapterCacheItem = packed record
    LineIndex, CharIndex, LineLen: integer;
    ColorAfterEol: TColor;
    Parts: TATLineParts;
    class operator =(const a, b: TATAdapterCacheItem): boolean;
  end;

  { TATAdapterCacheItems }

  TATAdapterCacheItems = class(specialize TFPGList<TATAdapterCacheItem>)
  public
    function ItemPtr(AIndex: integer): PATAdapterCacheItem; inline;
  end;

type
  { TATAdapterHiliteCache }

  TATAdapterHiliteCache = class
  private
    FList: TATAdapterCacheItems;
    FMaxCount: integer;
    FEnabled: boolean;
    procedure SetEnabled(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property MaxCount: integer read FMaxCount write FMaxCount;
    property Enabled: boolean read FEnabled write SetEnabled;
    procedure Clear;
    procedure Add(
      const ALineIndex, ACharIndex, ALineLen: integer;
      const AParts: TATLineParts;
      const AColorAfterEol: TColor);
    function Get(
      const ALineIndex, ACharIndex, ALineLen: integer;
      var AParts: TATLineParts;
      var AColorAfterEol: TColor): boolean;
    procedure Delete(N: integer);
    procedure DeleteForLine(ALineIndex: integer);
  end;


implementation

const
  //500 lines in minimap on my monitor+ 100 lines in editor
  cAdapterCacheMaxSize = 1000;

{ TATAdapterCacheItems }

function TATAdapterCacheItems.ItemPtr(AIndex: integer): PATAdapterCacheItem;
begin
  Result:= PATAdapterCacheItem(InternalGet(AIndex));
end;

{ TATAdapterCacheItem }

class operator TATAdapterCacheItem.= (const a, b: TATAdapterCacheItem): boolean;
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
  FMaxCount:= cAdapterCacheMaxSize;
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
  const ALineIndex, ACharIndex, ALineLen: integer;
  const AParts: TATLineParts;
  const AColorAfterEol: TColor);
var
  Item: TATAdapterCacheItem;
begin
  if not Enabled then exit;

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

  while FList.Count>FMaxCount do
    Delete(FList.Count-1);

  FillChar(Item, SizeOf(Item), 0);
  Item.LineIndex:= ALineIndex;
  Item.CharIndex:= ACharIndex;
  Item.LineLen:= ALineLen;
  Item.ColorAfterEol:= AColorAfterEol;
  Move(AParts, Item.Parts, SizeOf(AParts));
  FList.Insert(0, Item);
end;


function TATAdapterHiliteCache.Get(
  const ALineIndex, ACharIndex, ALineLen: integer;
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
    Item:= FList.ItemPtr(i);
    if (Item^.LineIndex=ALineIndex) and
      (Item^.CharIndex=ACharIndex) and
      (Item^.LineLen=ALineLen) then
      begin
        Move(Item^.Parts, AParts, SizeOf(AParts));
        AColorAfterEol:= Item^.ColorAfterEol;
        exit(true);
      end;
  end;
end;

procedure TATAdapterHiliteCache.Delete(N: integer);
begin
  FList.Delete(N);
end;

procedure TATAdapterHiliteCache.DeleteForLine(ALineIndex: integer);
var
  Item: PATAdapterCacheItem;
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    Item:= FList.ItemPtr(i);
    if (Item^.LineIndex=ALineIndex) then
      Delete(i);
  end;
end;

end.

