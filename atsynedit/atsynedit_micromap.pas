{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Micromap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TATMicromapColumn = record
    NWidthUnits: integer;
    NLeft, NRight: integer;
    NTag: Int64;
    NColor: TColor;
  end;

type
  { TATMicromap }

  TATMicromap = class
  strict private
    TotalWidthPixels: integer;
  public
    Columns: array of TATMicromapColumn;
    constructor Create;
    function IsIndexValid(AIndex: integer): boolean;
    function ColumnFromTag(const ATag: Int64): integer;
    function ColumnAdd(const ATag: Int64; AWidthUnits: integer; AColor: TColor): boolean;
    function ColumnDelete(const ATag: Int64): boolean;
    procedure UpdateWidth(ATotalWidthPixels: integer);
  end;

implementation


{ TATMicromap }

constructor TATMicromap.Create;
begin
  inherited Create;
  Columns:= nil;
  ColumnAdd(0, 100, clNone);
  ColumnAdd(1, 100, clNone);
  ColumnAdd(2, 100, clNone);
end;

function TATMicromap.IsIndexValid(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<Length(Columns));
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

function TATMicromap.ColumnAdd(const ATag: Int64; AWidthUnits: integer; AColor: TColor): boolean;
begin
  Result:= (ColumnFromTag(ATag)<0) and (AWidthUnits>0);
  if Result then
  begin
    SetLength(Columns, Length(Columns)+1);
    with Columns[Length(Columns)-1] do
    begin
      NTag:= ATag;
      NWidthUnits:= AWidthUnits;
      NColor:= AColor;
    end;
  end;
end;

function TATMicromap.ColumnDelete(const ATag: Int64): boolean;
var
  NCol, NLen: integer;
begin
  NCol:= ColumnFromTag(ATag);
  NLen:= Length(Columns);
  Result:= (NCol>0) and (NCol<NLen); //don't allow to delete column-0
  if Result then
    Delete(Columns, NCol, 1); //needs FPC 3.2.0
  {
  if Result then
  begin
    for i:= NCol to NLen-2 do
      Columns[i]:= Columns[i+1];
    SetLength(Columns, NLen-1);
  end;
  }
end;

procedure TATMicromap.UpdateWidth(ATotalWidthPixels: integer);
var
  NTotalUnits, NWidthPixels: integer;
  i: integer;
begin
  TotalWidthPixels:= ATotalWidthPixels;

  NTotalUnits:= 0;
  for i:= 0 to Length(Columns)-1 do
    with Columns[i] do
    begin
      NTotalUnits+= NWidthUnits;
    end;

  for i:= 0 to Length(Columns)-1 do
    with Columns[i] do
    begin
      NWidthPixels:= TotalWidthPixels * NWidthUnits div NTotalUnits;
      if i=0 then
        NLeft:= 0
      else
        NLeft:= Columns[i-1].NRight;
      NRight:= NLeft+NWidthPixels;
    end;
end;

end.
