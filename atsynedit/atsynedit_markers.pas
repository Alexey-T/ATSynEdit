unit ATSynEdit_Markers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TATMarkerItem = class
  public
    PosX, PosY: integer;
    CoordX, CoordY: integer; //screen coords
    Tag: integer; //used in CudaText: when "collect marker" runs, for all markers
                  //with the same Tag>0 multi-carets placed
    SelLen: integer; //used in CudaText: when "collect marker" runs, caret will
                  //be with selection of this len
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
    procedure Add(APosX, APosY: integer; ATag: integer=0; ASelLen: integer=0);
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

procedure TATMarkers.Add(APosX, APosY: integer; ATag: integer; ASelLen: integer);
var
  Item: TATMarkerItem;
begin
  Item:= TATMarkerItem.Create;
  Item.PosX:= APosX;
  Item.PosY:= APosY;
  Item.CoordX:= -1;
  Item.CoordY:= -1;
  Item.Tag:= ATag;
  Item.SelLen:= ASelLen;

  FList.Add(Item);
end;

end.

