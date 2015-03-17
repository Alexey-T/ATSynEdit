procedure DoClearScrollInfo(var Info: TATSynScrollInfo);
begin
  Info.NPos:= 0;
  Info.NMin:= 0;
  Info.NMax:= 1;
  Info.NPage:= 1;
end;

function IsEqualScrollInfo(const Info1, Info2: TATSynScrollInfo): boolean;
begin
  Result:=
    (Info1.NPos=Info2.NPos) and
    (Info1.NMin=Info2.NMin) and
    (Info1.NMax=Info2.NMax) and
    (Info1.NPage=Info2.NPage);
end;

procedure _InitCarets(var C: TATSynCarets);
var
  i: integer;
begin
  for i:= 0 to C.Count-1 do
    with C[i] do
    begin
      EndY:= PosY;
      EndX:= Max(0, PosX+4);
    end;
end;

function IsCaretPosSorted(X1, Y1, X2, Y2: integer): boolean;
begin
  if Y1<>Y2 then
    Result:= Y1<Y2
  else
    Result:= X1<X2;
end;

