unit ATSynEdit_Markers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Carets;

type
  TATMarkerItem = class
  public
    PosX, PosY: integer;
    CoordX, CoordY: integer; //screen coords
    Tag: integer;
      //used in CudaText: when "collect marker" runs, for all markers
      //with the same Tag>0 multi-carets placed
    LenX, LenY: integer;
      //used in CudaText: when "collect marker" runs, caret will be with selection
      //if LenY=0 - LenX is length of sel (single line)
      //if LenY>0 - LenY is Y-delta of sel-end,
      //            LenX is absolute X of sel-end
    Ptr: TObject;
      //used in Attribs object of ATSynedit
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
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATMarkerItem read GetItem; default;
    procedure Add(APosX, APosY: integer;
      ATag: integer=0; ALenX: integer=0; ALenY: integer=0;
      APtr: TObject=nil;
      AInsertToBegin: boolean=false);
    procedure DeleteInRange(AX1, AY1, AX2, AY2: integer);
    procedure DeleteWithTag(ATag: integer);
    function FindMarkerAtPos(AX, AY: integer): TATMarkerItem;
  end;

implementation

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
    Delete(i);
end;

procedure TATMarkers.Delete(N: integer);
var
  Mark: TATMarkerItem;
begin
  if IsIndexValid(N) then
  begin
    Mark:= TATMarkerItem(FList[N]);
    if Assigned(Mark.Ptr) then
      Mark.Ptr.Free;
    Mark.Free;

    FList.Delete(N);
  end;
end;

function TATMarkers.Count: integer;
begin
  Result:= FList.Count;
end;

function TATMarkers.IsIndexValid(N: integer): boolean;
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

procedure TATMarkers.Add(APosX, APosY: integer; ATag: integer; ALenX: integer;
  ALenY: integer; APtr: TObject; AInsertToBegin: boolean);
var
  Item: TATMarkerItem;
begin
  Item:= TATMarkerItem.Create;
  Item.PosX:= APosX;
  Item.PosY:= APosY;
  Item.CoordX:= -1;
  Item.CoordY:= -1;
  Item.Tag:= ATag;
  Item.LenX:= ALenX;
  Item.LenY:= ALenY;
  Item.Ptr:= APtr;

  if AInsertToBegin then
    FList.Insert(0, Item)
  else
    FList.Add(Item);
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

procedure TATMarkers.DeleteWithTag(ATag: integer);
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

