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
    function FindPrior(ALineIndex, ACharIndex: integer; out AExact: boolean): integer;
    function IsSorted: boolean;
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
var
  N: integer;
  bExact: boolean;
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

  N:= FindPrior(ALineIndex, ACharIndex, bExact);
  if N>=FList.Count then
    FList.Add(FTempItem)
  else
    FList.Insert(N, FTempItem);

  //for debug only
  {
  if not IsSorted then
    if N>2 then
      ;
      }
end;


function TATAdapterHiliteCache.Get(
  const ALineIndex, ACharIndex: integer;
  var AParts: TATLineParts;
  var AColorAfterEol: TColor): boolean;
var
  Item: PATAdapterCacheItem;
  N: integer;
  bExact: boolean;
begin
  Result:= false;
  if not Enabled then exit;

  N:= FindPrior(ALineIndex, ACharIndex, bExact);
  if bExact then
  begin
    Item:= FList._GetItemPtr(N);
    CopyLineParts(Item^.Parts, AParts);
    AColorAfterEol:= Item^.ColorAfterEol;
    Result:= true;
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
    if Item^.LineIndex<ALineIndex then
      Break;
    if Item^.LineIndex=ALineIndex then
      FList.Delete(i);
  end;
end;

function TATAdapterHiliteCache.FindPrior(ALineIndex, ACharIndex: integer; out AExact: boolean): integer;
//Find list index, at which data >= parameters.
//Result=Count, if all items are smaller.
  //
  function GetDif(m: integer): integer; inline;
  var
    midItem: PATAdapterCacheItem;
    midLine, midChar: integer;
  begin
    midItem:= FList._GetItemPtr(m);
    midLine:= midItem^.LineIndex;
    midChar:= midItem^.CharIndex;
    if ALineIndex>midLine then
      exit(1)
    else
    if ALineIndex<midLine then
      exit(-1)
    else
    if ACharIndex>midChar then
      exit(1)
    else
    if ACharIndex<midChar then
      exit(-1)
    else
      exit(0);
  end;
  //
var
  a, b, m: integer;
  dif: integer;
begin
  AExact:= false;
  a:= 0;
  b:= FList.Count-1;
  if b<0 then exit(0);
  m:= 0;

  while a<=b do
  begin
    m:= (a+b+1) div 2;
    dif:= GetDif(m);
    if dif>0 then
      a:= m+1
    else
    if dif<0 then
      b:= m-1
    else
    begin
      AExact:= true;
      exit(m);
    end;
  end;

  if GetDif(m)>0 then
    Inc(m);
  Result:= m;
end;

function TATAdapterHiliteCache.IsSorted: boolean;
var
  p1, p2: PATAdapterCacheItem;
  i: integer;
begin
  Result:= true;
  for i:= 0 to FList.Count-2 do
  begin
    p1:= FList._GetItemPtr(i);
    p2:= FList._GetItemPtr(i+1);
    if p1^.LineIndex>p2^.LineIndex then
      exit(false);
    if (p1^.LineIndex=p2^.LineIndex) and (p1^.CharIndex>p2^.CharIndex) then
      exit(false);
  end;
end;


end.

