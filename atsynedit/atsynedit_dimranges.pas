{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_DimRanges;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATSynEdit_fgl;

type
  { TATDimRange }

  PATDimRange = ^TATDimRange;
  TATDimRange = record
    LineFrom,
    LineTo: integer;
    DimValue: integer;
    class operator=(const A, B: TATDimRange): boolean;
  end;

  { TATDimRanges }

  TATDimRanges = class(specialize TFPGList<TATDimRange>)
  public
    function ItemPtr(AIndex: integer): PATDimRange;
    function IsIndexValid(AIndex: integer): boolean;
    procedure Add(ALineFrom, ALineTo: integer; ADimValue: integer);
    function GetDimValue(ALine, ADefValue: integer): integer;
  end;

implementation

{ TATDimRange }

class operator TATDimRange.=(const A, B: TATDimRange): boolean;
begin
  Result:= false;
end;

{ TATDimRanges }

function TATDimRanges.IsIndexValid(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<Count);
end;

function TATDimRanges.ItemPtr(AIndex: integer): PATDimRange;
begin
  Result:= inherited _GetItemPtr(AIndex);
end;

procedure TATDimRanges.Add(ALineFrom, ALineTo: integer; ADimValue: integer);
var
  Range: TATDimRange;
begin
  Range:= Default(TATDimRange);
  Range.LineFrom:= ALineFrom;
  Range.LineTo:= ALineTo;
  Range.DimValue:= ADimValue;
  inherited Add(Range);
end;

function TATDimRanges.GetDimValue(ALine, ADefValue: integer): integer;
var
  DimPtr: PATDimRange;
  i: integer;
begin
  //list is not sorted, cannot use binary search
  for i:= 0 to Count-1 do
  begin
    DimPtr:= ItemPtr(i);
    if (ALine>=DimPtr^.LineFrom) and (ALine<=DimPtr^.LineTo) then
      exit(DimPtr^.DimValue);
  end;
  Result:= ADefValue;
end;

end.

