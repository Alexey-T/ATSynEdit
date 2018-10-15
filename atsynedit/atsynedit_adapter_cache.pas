{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_Cache;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$Z1}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit_CanvasProc,
  gdeque;

type
  TATAdapterCacheItem = packed record
    LineIndex, CharIndex, LineLen: integer;
    ColorAfterEol: TColor;
    Parts: TATLineParts;
    class operator=(const A, B: TATAdapterCacheItem): boolean;
  end;
  PATAdapterCacheItem = ^TATAdapterCacheItem;

type
  TATAdapterCacheItems = specialize TDeque<TATAdapterCacheItem>;

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
    function GetDump: string;
  end;


implementation

const
  //500 lines in minimap on my monitor+ 100 lines in editor
  cAdapterCacheMaxSize = 1000;

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
  while not FList.IsEmpty() do
    FList.PopBack();
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

  if FList.Size()>=FMaxCount then
    FList.PopBack;

  FillChar(Item, SizeOf(Item), 0);
  Item.LineIndex:= ALineIndex;
  Item.CharIndex:= ACharIndex;
  Item.LineLen:= ALineLen;
  Item.ColorAfterEol:= AColorAfterEol;
  Move(AParts, Item.Parts, SizeOf(AParts));
  FList.PushFront(Item);
end;

function TATAdapterHiliteCache.GetDump: string;
var
  L: TStringList;
  S: string;
  i, j: integer;
begin
  L:= TStringList.Create;
  try
    for i:= 0 to FList.Size-1 do
      with FList.Items[i] do
      begin
        S:= '';
        for j:= 0 to 10 do
        begin
          if (Parts[j].Offset=0) and
             (Parts[j].Len=0) then Break;
          S:= S+Format('[%d %d]',
            [ Parts[j].Offset, Parts[j].Len ]
            );
        end;

        S:= Format('  line %d, char %d, len %d, parts %s',
          [LineIndex, CharIndex, LineLen, S]);
        L.Add(S);
      end;
    Result:= L.Text;
  finally
    L.Free;
  end;
end;

function TATAdapterHiliteCache.Get(
  const ALineIndex, ACharIndex, ALineLen: integer;
  var AParts: TATLineParts;
  var AColorAfterEol: TColor): boolean;
var
  ItemPtr: PATAdapterCacheItem;
  i: integer;
begin
  Result:= false;
  if not Enabled then exit;

  for i:= 0 to FList.Size-1 do
  begin
    ItemPtr:= FList.Mutable[i];
    if (ItemPtr^.LineIndex=ALineIndex) and
      (ItemPtr^.CharIndex=ACharIndex) and
      (ItemPtr^.LineLen=ALineLen) then
      begin
        Move(ItemPtr^.Parts, AParts, SizeOf(AParts));
        AColorAfterEol:= ItemPtr^.ColorAfterEol;
        exit(true);
      end;
  end;
end;

end.

