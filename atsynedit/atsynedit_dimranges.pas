{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_DimRanges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Markers;
  
type
  { TATDimRanges }

  TATDimRanges = class(TATMarkers)
  public
    constructor Create; override;
    procedure Add(ALineFrom, ALineTo: integer; ADimValue: integer);
    function GetDimValue(ALine, ADefValue: integer): integer;
  end;

implementation

constructor TATDimRanges.Create;
begin
  inherited Create;
  Sorted:= true;
end;

procedure TATDimRanges.Add(ALineFrom, ALineTo: integer; ADimValue: integer);
begin
  inherited Add(
    0,
    ALineFrom,
    0,
    0,
    ALineTo-ALineFrom+1,
    nil,
    ADimValue
    );
end;

function TATDimRanges.GetDimValue(ALine, ADefValue: integer): integer;
var
  Item: TATMarkerItem;
  NIndex: integer;
  bExact, bContains: boolean;
begin
  Result:= ADefValue;
  if Count=0 then exit;

  Find(0, ALine, NIndex, bExact, bContains);

  //because Find is limited, check also nearest 2 items
  if NIndex>=Count then
    NIndex:= Count-1;

  Item:= Items[NIndex];
  if Item.Contains(0, ALine) then
    exit(Item.Value);

  if NIndex>0 then
  begin
    Item:= Items[NIndex-1];
    if Item.Contains(0, ALine) then
      exit(Item.Value);
  end;
end;


end.

