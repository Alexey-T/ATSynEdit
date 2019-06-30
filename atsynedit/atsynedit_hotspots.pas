{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Hotspots;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$Z1}

interface

uses
  Classes, SysUtils,
  ATSynEdit_fgl,
  ATSynEdit_Carets;

type
  PATHotspotItem = ^TATHotspotItem;
  TATHotspotItem = record
    PosX, PosY: integer;
    EndX, EndY: integer;
    Tag: Int64;
    TagString: string;
    class operator=(const A, B: TATHotspotItem): boolean;
  end;

type

  { TATHotspotItems }

  TATHotspotItems = class(specialize TFPGList<TATHotspotItem>)
  public
    function ItemPtr(AIndex: integer): PATHotspotItem;
  end;

type
  { TATHotspots }

  TATHotspots = class
  private
    FList: TATHotspotItems;
    function GetItem(AIndex: integer): TATHotspotItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATHotspotItem read GetItem; default;
    procedure Add(const AItem: TATHotspotItem);
    procedure Insert(N: integer; const AItem: TATHotspotItem);
    procedure Delete(N: integer);
    procedure DeleteByTag(const ATag: Int64);
    function FindByPos(AX, AY: integer): integer;
    function FindByTagInt(const ATag: Int64): integer;
    function FindByTagString(const ATagString: string): integer;
  end;


implementation

{ TATHotspotItems }

function TATHotspotItems.ItemPtr(AIndex: integer): PATHotspotItem;
begin
  Result:= PATHotspotItem(InternalGet(AIndex));
end;

{ TATHotspotItem }

class operator TATHotspotItem.=(const A, B: TATHotspotItem): boolean;
begin
  Result:= false;
end;

{ TATHotspots }

constructor TATHotspots.Create;
begin
  FList:= TATHotspotItems.Create;
end;

destructor TATHotspots.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATHotspots.Clear; inline;
begin
  FList.Clear;
end;

function TATHotspots.GetItem(AIndex: integer): TATHotspotItem;
begin
  Result:= FList[AIndex];
end;

function TATHotspots.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATHotspots.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<Count);
end;

procedure TATHotspots.Add(const AItem: TATHotspotItem); inline;
begin
  FList.Add(AItem);
end;

procedure TATHotspots.Delete(N: integer); inline;
begin
  FList.Delete(N);
end;

procedure TATHotspots.DeleteByTag(const ATag: Int64);
var
  N: integer;
begin
  repeat
    N:= FindByTagInt(ATag);
    if N<0 then Break;
    Delete(N);
  until false;
end;

procedure TATHotspots.Insert(N: integer; const AItem: TATHotspotItem); inline;
begin
  if N>=FList.Count then
    FList.Add(AItem)
  else
    FList.Insert(N, AItem);
end;

function TATHotspots.FindByPos(AX, AY: integer): integer;
var
  Item: PATHotspotItem;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
  begin
    Item:= FList.ItemPtr(i);
    if IsPosInRange(AX, AY, Item^.PosX, Item^.PosY, Item^.EndX, Item^.EndY, false) = cRelateInside then
      exit(i);
  end;
end;

function TATHotspots.FindByTagInt(const ATag: Int64): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    if ATag=FList.ItemPtr(i)^.Tag then
      exit(i);
end;

function TATHotspots.FindByTagString(const ATagString: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    if ATagString=FList.ItemPtr(i)^.TagString then
      exit(i);
end;

end.

