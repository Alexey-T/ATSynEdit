{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Micromap;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  TATMicromapColumn = record
    NWidthPercents, NWidthPixels: integer;
    NLeft, NRight: integer;
    NTag: Int64;
  end;

type
  { TATMicromap }

  TATMicromap = class
  private
  public
    Columns: array of TATMicromapColumn;
    constructor Create;
    function ColumnFromTag(const ATag: Int64): integer;
    function ColumnAdd(const ATag: Int64; AWidthPercents: integer): boolean;
    function ColumnDelete(const ATag: Int64): boolean;
  end;

implementation


{ TATMicromap }

constructor TATMicromap.Create;
begin
  inherited Create;
  SetLength(Columns, 1);
  with Columns[0] do
  begin
    NWidthPercents:= 100;
    NTag:= 0;
  end;
end;

function TATMicromap.ColumnFromTag(const ATag: Int64): integer;
var
  i: integer;
begin
  for i:= 0 to Length(Columns)-1 do
    with Columns[i] do
      if NTag=ATag then
        exit(i);
  Result:= -1;
end;

function TATMicromap.ColumnAdd(const ATag: Int64; AWidthPercents: integer): boolean;
begin
  Result:= (ColumnFromTag(ATag)<0) and (AWidthPercents>0);
  if Result then
  begin
    SetLength(Columns, Length(Columns)+1);
    with Columns[Length(Columns)-1] do
    begin
      NTag:= ATag;
      NWidthPercents:= AWidthPercents;
    end;
  end;
end;

function TATMicromap.ColumnDelete(const ATag: Int64): boolean;
var
  NCol, NLen, i: integer;
begin
  NCol:= ColumnFromTag(ATag);
  NLen:= Length(Columns);
  Result:= (NCol>0) and (NCol<NLen); //don't allow to delete column-0
  if Result then
  begin
    for i:= NCol to NLen-2 do
      Columns[i]:= Columns[i+1];
    SetLength(Columns, NLen-1);
  end;
end;



end.
