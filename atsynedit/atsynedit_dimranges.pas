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
    DimValue: integer;
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
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATDimRange read GetItem; default;
    function Add(ALineFrom, ALineTo: integer; ADimValue: integer): TATDimRange;
    function GetDimValue(ALine, ADefValue: integer): integer;
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

function TATDimRanges.Count: integer; inline;
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

function TATDimRanges.Add(ALineFrom, ALineTo: integer; ADimValue: integer): TATDimRange;
begin
  Result:= TATDimRange.Create;
  Result.LineFrom:= ALineFrom;
  Result.LineTo:= ALineTo;
  Result.DimValue:= ADimValue;

  FList.Add(Result);
end;

function TATDimRanges.GetDimValue(ALine, ADefValue: integer): integer;
var
  R: TATDimRange;
  i: integer;
begin
  Result:= ADefValue;
  for i:= 0 to Count-1 do
  begin
    R:= TATDimRange(FList[i]);
    if (ALine>=R.LineFrom) and (ALine<=R.LineTo) then
      exit(R.DimValue);
  end;
end;


end.

