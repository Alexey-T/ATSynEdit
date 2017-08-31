{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Markers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Carets;

type
  { TATMarkerItem }

  TATMarkerItem = class
  public
    PosX, PosY: integer;
    CoordX, CoordY: integer; //screen coords
    Tag: Int64;
      //used in CudaText: when "collect marker" runs, for all markers
      //with the same Tag>0 multi-carets placed
    LenX, LenY: integer;
      //used in CudaText: when "collect marker" runs, caret will be with selection
      //if LenY=0 - LenX is length of sel (single line)
      //if LenY>0 - LenY is Y-delta of sel-end,
      //            LenX is absolute X of sel-end
    Ptr: TObject;
      //used in Attribs object of ATSynedit
    destructor Destroy; override;
  end;

type
  { TATMarkers }

  TATMarkers = class
  private
    FList: TList;
    function GetItem(N: integer): TATMarkerItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    function Count: integer;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATMarkerItem read GetItem; default;
    function Add(APosX, APosY: integer;
      const ATag: Int64=0; ALenX: integer=0; ALenY: integer=0;
      APtr: TObject=nil;
      AInsertToBegin: boolean=false): TATMarkerItem;
    procedure DeleteInRange(AX1, AY1, AX2, AY2: integer);
    procedure DeleteWithTag(const ATag: Int64);
    function FindMarkerAtPos(AX, AY: integer): TATMarkerItem;
  end;

implementation

{ TATMarkerItem }

destructor TATMarkerItem.Destroy;
begin
  if Assigned(Ptr) then
    FreeAndNil(Ptr);
  inherited Destroy;
end;

{ TATMarkers }

constructor TATMarkers.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATMarkers.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATMarkers.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

procedure TATMarkers.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

function TATMarkers.Count: integer;
begin
  Result:= FList.Count;
end;

function TATMarkers.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATMarkers.GetItem(N: integer): TATMarkerItem;
begin
  if IsIndexValid(N) then
    Result:= TATMarkerItem(FList[N])
  else
    Result:= nil;
end;

function TATMarkers.Add(APosX, APosY: integer; const ATag: Int64;
  ALenX: integer; ALenY: integer; APtr: TObject; AInsertToBegin: boolean): TATMarkerItem;
begin
  Result:= TATMarkerItem.Create;
  Result.PosX:= APosX;
  Result.PosY:= APosY;
  Result.CoordX:= -1;
  Result.CoordY:= -1;
  Result.Tag:= ATag;
  Result.LenX:= ALenX;
  Result.LenY:= ALenY;
  Result.Ptr:= APtr;

  if AInsertToBegin then
    FList.Insert(0, Result)
  else
    FList.Add(Result);
end;

procedure TATMarkers.DeleteInRange(AX1, AY1, AX2, AY2: integer);
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    with Items[i] do
      if IsPosInRange(PosX, PosY, AX1, AY1, AX2, AY2)=cRelateInside then
        Delete(i);
end;

procedure TATMarkers.DeleteWithTag(const ATag: Int64);
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    if Items[i].Tag=ATag then
      Delete(i);
end;

function TATMarkers.FindMarkerAtPos(AX, AY: integer): TATMarkerItem;
var
  Item: TATMarkerItem;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    if (Item.PosY=AY) and (Item.PosX<=AX) and
      ( (Item.LenY>0) or (Item.PosX+Item.LenX>AX) )
      then exit(Item);
  end;
end;

end.

