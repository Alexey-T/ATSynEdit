const
  ssXControl = {$ifndef darwin} ssCtrl {$else} ssMeta {$endif};

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

procedure InitClipboardFormat;
begin
  cClipFormatId:= RegisterClipboardFormat('Application/X-Laz-ATSynEdit-Block');
end;

procedure InitResources;
begin
  cBitmapNiceScroll:= TBitmap.Create;
  cBitmapNiceScroll.LoadFromResourceName(HInstance, 'AB_MOVE');
  cBitmapNiceScroll.Transparent:= true;

  Screen.Cursors[crNiceScrollNone]  := LoadCursor(HInstance, 'AB_MOVE');
  Screen.Cursors[crNiceScrollUp]    := LoadCursor(HInstance, 'AB_MOVE_U');
  Screen.Cursors[crNiceScrollDown]  := LoadCursor(HInstance, 'AB_MOVE_D');
  Screen.Cursors[crNiceScrollLeft]  := LoadCursor(HInstance, 'AB_MOVE_L');
  Screen.Cursors[crNiceScrollRight] := LoadCursor(HInstance, 'AB_MOVE_R');
end;

procedure FreeResources;
begin
  FreeAndNil(cBitmapNiceScroll);
end;


