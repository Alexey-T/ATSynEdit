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
    ColorAfterEol: TColor;
    Parts: TATLineParts;
  end;

type
  { TATAdapterHiliteCache }

  TATAdapterHiliteCache = class
  private
    FList: TList;
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
  cAdapterCacheMaxSize = 1000;
  //500 lines in minimap on my monitor+ 100 lines in editor

{ TATAdapterHiliteCache }

procedure TATAdapterHiliteCache.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:= AValue;
  if not AValue then Clear;
end;

constructor TATAdapterHiliteCache.Create;
begin
  FList:= TList.Create;
  FMaxCount:= cAdapterCacheMaxSize;
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
    TObject(FList[i]).Free;
  FList.Clear;
end;

procedure TATAdapterHiliteCache.Add(
  const ALineIndex, ACharIndex, ALineLen: integer;
  const AParts: TATLineParts;
  const AColorAfterEol: TColor);
var
  N: integer;
  Item: TATAdapterHiliteCacheItem;
begin
  if not Enabled then exit;

  //ignore if no parts
  if (AParts[0].Len=0) then exit;

  {
  //ignore if single part, and no bold/italic/underline attr
  //e.g. lexer didnt parse end of file yet
  if (AParts[1].Len=0) and
    (AParts[0].FontBold=false) and
    (AParts[0].FontItalic=false) and
    (AParts[0].FontStrikeOut=false)
    then exit;
    }

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

function TATAdapterHiliteCache.GetDump: string;
var
  L: TStringList;
  S: string;
  i, j: integer;
begin
  L:= TStringList.Create;
  try
    for i:= 0 to FList.Count-1 do
      with TATAdapterHiliteCacheItem(FList.Items[i]) do
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
  Item: TATAdapterHiliteCacheItem;
  i: integer;
begin
  Result:= false;
  if not Enabled then exit;

  for i:= 0 to FList.Count-1 do
  begin
    Item:= TATAdapterHiliteCacheItem(FList[i]);
    if (Item.LineIndex=ALineIndex) and
      (Item.CharIndex=ACharIndex) and
      (Item.LineLen=ALineLen) then
      begin
        Move(Item.Parts, AParts, SizeOf(AParts));
        AColorAfterEol:= Item.ColorAfterEol;
        exit(true);
      end;
  end;
end;

end.

