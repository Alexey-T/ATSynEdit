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
  NIndex: integer;
begin
  Result:= ADefValue;
  NIndex:= FindContaining(0, ALine);
  if IsIndexValid(NIndex) then
    Result:= Items[NIndex].Value;
end;


end.

