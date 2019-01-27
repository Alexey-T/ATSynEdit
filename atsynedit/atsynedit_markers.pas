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
    Value: Int64;
    Ptr: TObject;
      //used in Attribs object of ATSynedit
    class operator=(const A, B: TATMarkerItem): boolean;
    function Contains(AX, AY: integer): boolean;
    function PosEnd: TPoint;
  end;
  
type
  { TATMarkerItems }

  TATMarkerItems = specialize TFPGList<TATMarkerItem>;

type
  { TATMarkers }

  TATMarkers = class
  private
    FList: TATMarkerItems;
    FSorted: boolean;
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
    property Sorted: boolean read FSorted write FSorted;
    procedure Add(APosX, APosY: integer;
      const ATag: Int64=0;
      ALenX: integer=0;
      ALenY: integer=0;
      APtr: TObject=nil;
      AValue: Int64=0);
    procedure DeleteInRange(AX1, AY1, AX2, AY2: integer);
    procedure DeleteWithTag(const ATag: Int64);
    procedure Find(AX, AY: integer; out AIndex: integer; out AExactMatch, AContains: boolean);
    function FindContaining(AX, AY: integer): integer;
  end;

implementation

{ TATMarkerItem }

class operator TATMarkerItem.=(const A, B: TATMarkerItem): boolean;
begin
  Result:= false;
end;

function TATMarkerItem.Contains(AX, AY: integer): boolean;
var
  P: TPoint;
begin
  if (LenX<=0) and (LenY<=0) then
    Result:= false
  else
  begin
    P:= PosEnd;
    Result:= IsPosInRange(AX, AY, PosX, PosY, P.X, P.Y)=cRelateInside;
  end;
end;

function TATMarkerItem.PosEnd: TPoint;
begin
  if LenY<=0 then
  begin
    //LenX is selection len (selection is single line)
    Result.X:= PosX+LenX;
    Result.Y:= PosY;
  end
  else
  begin
    //LenX is selection end X-pos;
    //LenY is count of sel lines
    Result.X:= LenX;
    Result.Y:= PosY+LenY;
  end;
end;

{ TATMarkers }

constructor TATMarkers.Create;
begin
  inherited;
  FList:= TATMarkerItems.Create;
  FSorted:= false;
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

procedure TATMarkers.Add(APosX, APosY: integer; const ATag: Int64; ALenX: integer; ALenY: integer;
  APtr: TObject; AValue: Int64);
var
  Item: TATMarkerItem;
  NIndex: integer;
  bExact, bContains: boolean;
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
  Item.Value:= AValue;

  if FSorted then
  begin
    Find(APosX, APosY, NIndex, bExact, bContains);
    if bExact then
      FList.Delete(NIndex);
    FList.Insert(NIndex, Item);
  end
  else
    FList.Add(Item);
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
  if Y1<>Y2 then
    Result:= Y1-Y2
  else
    Result:= X1-X2;
end;

procedure TATMarkers.Find(AX, AY: integer; out AIndex: integer; out AExactMatch, AContains: boolean);
//gives AIndex in range [0..Count] (without -1)
var
  L, H, I, C: Integer;
  Item: TATMarkerItem;
begin
  if not FSorted then
    raise Exception.Create('Method Find can be used only with Sorted=true');

  AIndex := 0;
  AExactMatch := False;
  AContains := False;

  if Count = 0 then
    Exit;

  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Item := Items[I];
    C := _ComparePoints(Item.PosX, Item.PosY, AX, AY);
    if C < 0 then
      L := I + 1
    else
    begin
      if C = 0 then
      begin
        AIndex := I;
        AExactMatch := True;
        AContains := True;
        Exit;
      end;
      H := I - 1;
    end;
  end;
  AIndex := L;

  if IsIndexValid(AIndex) then
  begin
    Item := Items[AIndex];
    AContains := Item.Contains(AX, AY);
  end;
end;

function TATMarkers.FindContaining(AX, AY: integer): integer;
var
  Item: TATMarkerItem;
  NIndex: integer;
  bExact, bContains: boolean;
begin
  Result:= -1;
  if Count=0 then exit;

  Find(AX, AY, NIndex, bExact, bContains);

  if bContains then
    exit(NIndex);

  //because Find is limited, check also nearest 2 items
  if NIndex>=Count then
    NIndex:= Count-1;

  Item:= Items[NIndex];
  if Item.Contains(AX, AY) then
    exit(NIndex);

  if NIndex>0 then
  begin
    Dec(NIndex);
    Item:= Items[NIndex];
    if Item.Contains(AX, AY) then
      exit(NIndex);
  end;
end;


end.

