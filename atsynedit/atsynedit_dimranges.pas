{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_DimRanges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  { TATDimRange }

  TATDimRange = class
  public
    LineFrom: integer;
    LineTo: integer;
    DimValue255: integer;
  end;

type
  { TATDimRanges }

  TATDimRanges = class
  private
    FList: TList;
    function GetItem(N: integer): TATDimRange;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    function Count: integer;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATDimRange read GetItem; default;
    procedure Add(ALineFrom, ALineTo: integer; ADimValue255: integer);
  end;

implementation

constructor TATDimRanges.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATDimRanges.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATDimRanges.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

procedure TATDimRanges.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

function TATDimRanges.Count: integer;
begin
  Result:= FList.Count;
end;

function TATDimRanges.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATDimRanges.GetItem(N: integer): TATDimRange;
begin
  if IsIndexValid(N) then
    Result:= TATDimRange(FList[N])
  else
    Result:= nil;
end;

procedure TATDimRanges.Add(ALineFrom, ALineTo: integer; ADimValue255: integer);
var
  Item: TATDimRange;
begin
  Item:= TATDimRange.Create;
  Item.LineFrom:= ALineFrom;
  Item.LineTo:= ALineTo;
  Item.DimValue255:= ADimValue255;

  FList.Add(Item);
end;


end.

