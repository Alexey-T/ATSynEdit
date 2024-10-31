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

function MarkerArrayToString(const Ar: TATMarkerMarkerArray): string;
function PointsPairArrayToString(const A: TATPointPairArray): string;

procedure StringToMarkerArray(out Ar: TATMarkerMarkerArray; const AStr: string);
procedure StringToPointsPairArray(out A: TATPointPairArray; const AStr: string);

implementation

function PointsPairArrayToString(const A: TATPointPairArray): string;
var
  L: TStringList;
  NLen, i: integer;
begin
  NLen:= Length(A);
  if NLen=0 then
    exit('');

  if NLen=1 then
    exit(
      IntToStr(A[0].X)+','+IntToStr(A[0].Y)+';'+
      IntToStr(A[0].X2)+','+IntToStr(A[0].Y2)+';'
      );

  L:= TStringList.Create;
  try
    L.LineBreak:= ';';
    L.TrailingLineBreak:= true;
    L.Capacity:= NLen*2;
    for i:= 0 to NLen-1 do
    begin
      L.Add( IntToStr(A[i].X)+','+IntToStr(A[i].Y) );
      L.Add( IntToStr(A[i].X2)+','+IntToStr(A[i].Y2) );
    end;
    Result:= L.Text;
  finally
    FreeAndNil(L);
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
      L.Add(IntToStr(Ar[i].TagEx));
      L.Add(IntToStr(Ar[i].MicromapMode));
    end;
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;


procedure StringToMarkerArray(out Ar: TATMarkerMarkerArray; const AStr: string);
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
    Sep.GetItemInt64(Ar[i].TagEx, 0);
    Sep.GetItemInt(Ar[i].MicromapMode, 0);
  end;
end;


procedure StringToPointsPairArray(out A: TATPointPairArray; const AStr: string);
var
  Sep: TATStringSeparator;
  SItem, S1, S2: string;
  i, NLen: integer;
begin
  NLen:= SFindCharCount(AStr, ';') div 2;
  A:= nil;
  SetLength(A, NLen);
  Sep.Init(AStr, ';');
  for i:= 0 to NLen-1 do
  begin
    Sep.GetItemStr(SItem);
    SSplitByChar(SItem, ',', S1, S2);
    A[i].X:= StrToIntDef(S1, 0);
    A[i].Y:= StrToIntDef(S2, 0);

    Sep.GetItemStr(SItem);
    SSplitByChar(SItem, ',', S1, S2);
    A[i].X2:= StrToIntDef(S1, 0);
    A[i].Y2:= StrToIntDef(S2, 0);
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

