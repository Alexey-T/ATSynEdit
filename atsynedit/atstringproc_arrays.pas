{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_Arrays;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  ATStringProc,
  ATStringProc_Separator;

function PointsArrayToString(const A: TATPointArray): string;
function MarkerArrayToString(const Ar: TATMarkerMarkerArray): string;

procedure StringToMarkerArray(var Ar: TATMarkerMarkerArray; const AStr: string);
procedure StringToPointsArray(var A: TATPointArray; const AStr: string);

implementation

function PointsArrayToString(const A: TATPointArray): string;
var
  j: integer;
  Pnt: TPoint;
begin
  Result:= '';
  for j:= 0 to Length(A)-1 do
  begin
    Pnt:= A[j];
    Result+= IntToStr(Pnt.X)+','+IntToStr(Pnt.Y)+';';
  end;
end;


function MarkerArrayToString(const Ar: TATMarkerMarkerArray): string;
var
  L: TStringList;
  i: integer;
begin
  if Length(Ar)=0 then
    exit('');
  L:= TStringList.Create;
  try
    L.LineBreak:= ',';
    L.TrailingLineBreak:= true;
    for i:= 0 to High(Ar) do
    begin
      L.Add(IntToStr(Ar[i].PosX));
      L.Add(IntToStr(Ar[i].PosY));
      L.Add(IntToStr(Ar[i].SelX));
      L.Add(IntToStr(Ar[i].SelY));
      L.Add(IntToStr(Ar[i].Tag));
      L.Add(IntToStr(Ar[i].Value));
      L.Add(IntToStr(Ar[i].MicromapMode));
    end;
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;


procedure StringToMarkerArray(var Ar: TATMarkerMarkerArray; const AStr: string);
var
  Sep: TATStringSeparator;
  Len: integer;
  i: integer;
begin
  Ar:= nil;
  if AStr='' then
    exit;

  Len:= SFindCharCount(AStr, ',');
  if Len=0 then exit;
  SetLength(Ar, Len div 7);

  Sep.Init(AStr);
  for i:= 0 to Length(Ar)-1 do
  begin
    Sep.GetItemInt(Ar[i].PosX, 0);
    Sep.GetItemInt(Ar[i].PosY, 0);
    Sep.GetItemInt(Ar[i].SelX, 0);
    Sep.GetItemInt(Ar[i].SelY, 0);
    Sep.GetItemInt64(Ar[i].Tag, 0);
    Sep.GetItemInt64(Ar[i].Value, 0);
    Sep.GetItemInt(Ar[i].MicromapMode, 0);
  end;
end;

procedure StringToPointsArray(var A: TATPointArray; const AStr: string);
var
  Sep: TATStringSeparator;
  SItem, S1, S2: string;
  i, NLen: integer;
begin
  NLen:= SFindCharCount(AStr, ';');
  SetLength(A, NLen);
  Sep.Init(AStr, ';');
  for i:= 0 to NLen-1 do
  begin
    Sep.GetItemStr(SItem);
    SSplitByChar(SItem, ',', S1, S2);
    A[i].X:= StrToIntDef(S1, 0);
    A[i].Y:= StrToIntDef(S2, 0);
  end;
end;

procedure StringToInt64Array(var A: TATInt64Array; const AStr: string);
var
  Sep: TATStringSeparator;
  i, NLen: integer;
begin
  NLen:= SFindCharCount(AStr, ',');
  SetLength(A, NLen);
  Sep.Init(AStr, ',');
  for i:= 0 to NLen-1 do
  begin
    Sep.GetItemInt64(A[i], 0);
  end;
end;

end.

