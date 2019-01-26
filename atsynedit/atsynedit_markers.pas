{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Markers;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATSynEdit_fgl,
  ATSynEdit_Carets;

type
  { TATMarkerItem }

  PATMarkerItem = ^TATMarkerItem;
  TATMarkerItem = record
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
    class operator=(const A, B: TATMarkerItem): boolean;
  end;
  
type
  { TATMarkerItems }

  TATMarkerItems = specialize TFPGList<TATMarkerItem>;

type
  { TATMarkers }

  TATMarkers = class
  private
    FList: TATMarkerItems;
    function GetItem(N: integer): TATMarkerItem;
    procedure SetItem(N: integer; const AItem: TATMarkerItem);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(AIndex: integer);
    function Count: integer; inline;
    function IsIndexValid(AIndex: integer): boolean; inline;
    property Items[AIndex: integer]: TATMarkerItem read GetItem write SetItem; default;
    procedure Add(APosX, APosY: integer;
      const ATag: Int64=0;
      ALenX: integer=0;
      ALenY: integer=0;
      APtr: TObject=nil);
    procedure DeleteInRange(AX1, AY1, AX2, AY2: integer);
    procedure DeleteWithTag(const ATag: Int64);
    procedure Find(AX, AY: integer; out AIndex: integer; out AExactMatch: boolean);
    function FindAtPos(AX, AY: integer): integer;
  end;

implementation

{ TATMarkerItem }

class operator TATMarkerItem.=(const A, B: TATMarkerItem): boolean;
begin
  Result:= false;
end;

{ TATMarkers }

constructor TATMarkers.Create;
begin
  inherited;
  FList:= TATMarkerItems.Create;
end;

destructor TATMarkers.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATMarkers.Clear;
var
  Item: TATMarkerItem;
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    Item:= Items[i];
    if Assigned(Item.Ptr) then
      Item.Ptr.Free;
  end;
  FList.Clear;
end;

procedure TATMarkers.Delete(AIndex: integer);
var
  Item: TATMarkerItem;
begin
  if IsIndexValid(AIndex) then
  begin
    Item:= Items[AIndex];
    if Assigned(Item.Ptr) then
      Item.Ptr.Free;
    FList.Delete(AIndex);
  end;
end;

function TATMarkers.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATMarkers.IsIndexValid(AIndex: integer): boolean; inline;
begin
  Result:= (AIndex>=0) and (AIndex<FList.Count);
end;

function TATMarkers.GetItem(N: integer): TATMarkerItem;
begin
  Result:= FList[N];
end;

procedure TATMarkers.SetItem(N: integer; const AItem: TATMarkerItem);
begin
  FList[N]:= AItem;
end;

procedure TATMarkers.Add(APosX, APosY: integer; const ATag: Int64;
  ALenX: integer; ALenY: integer; APtr: TObject);
var
  Item: TATMarkerItem;
  NIndex: integer;
  bExact: boolean;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.PosX:= APosX;
  Item.PosY:= APosY;
  Item.CoordX:= -1;
  Item.CoordY:= -1;
  Item.Tag:= ATag;
  Item.LenX:= ALenX;
  Item.LenY:= ALenY;
  Item.Ptr:= APtr;

  //keep list sorted
  Find(APosX, APosY, NIndex, bExact);
  if bExact then
    FList.Delete(NIndex);
  FList.Insert(NIndex, Item);
end;

procedure TATMarkers.DeleteInRange(AX1, AY1, AX2, AY2: integer);
var
  Item: TATMarkerItem;
  i: integer;
begin
  for i:= Count-1 downto 0 do
  begin
    Item:= Items[i];
    if IsPosInRange(Item.PosX, Item.PosY, AX1, AY1, AX2, AY2)=cRelateInside then
      Delete(i);
  end;
end;

procedure TATMarkers.DeleteWithTag(const ATag: Int64);
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    if Items[i].Tag=ATag then
      Delete(i);
end;

function _ComparePoints(X1, Y1, X2, Y2: integer): integer; inline;
begin
  if Y2<>Y1 then
    Result:= Y2-Y1
  else
    Result:= X2-X1;
end;

procedure TATMarkers.Find(AX, AY: integer; out AIndex: integer; out AExactMatch: boolean);
//Copied from fgl unit, function TFPSMap.Find(AKey: Pointer; out Index: Integer): Boolean;
//Searches for the first item <= (AX,AY)
var
  I, L, R, Dir: Integer;
  Item: TATMarkerItem;
begin
  AExactMatch:= false;
  AIndex:= -1;
  L:= 0;
  R:= Count-1;
  while L<=R do
  begin
    I:= L + (R - L) div 2;
    Item:= Items[I];
    Dir:= _ComparePoints(AX, AY, Item.PosX, Item.PosY);
    if Dir < 0 then
      L:= I+1
    else begin
      R:= I-1;
      if Dir=0 then
      begin
        AExactMatch:= true;
        //if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  AIndex:= L;
end;

function TATMarkers.FindAtPos(AX, AY: integer): integer;
var
  Item: TATMarkerItem;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    if (Item.PosY=AY) and (Item.PosX<=AX) and
      ( (Item.LenY>0) or (Item.PosX+Item.LenX>AX) )
      then exit(i);
  end;
end;

end.

