//for browser-scroll feature
var
  cBitmapNiceScroll: TBitmap = nil;
const
  cBitmapNiceScrollRadius = 16;
  crNiceScrollNone  = TCursor(-30);
  crNiceScrollUp    = TCursor(-31);
  crNiceScrollDown  = TCursor(-32);
  crNiceScrollLeft  = TCursor(-33);
  crNiceScrollRight = TCursor(-34);

const
  //under Mac don't use Ctrl key, use Meta key as default (e.g. Meta+C, Meta+A)
  ssXControl = {$ifndef darwin} ssCtrl {$else} ssMeta {$endif};

procedure AppProcessMessages;
begin
  //why we need it?
  //1) ScrollTop:=N applies for drawed control (it needs wrapinfo),
  //and it needs paint called. paint called only passive, QT+Mac needs it.
  //so need to call processmessages to wait for paint..
  //2) for showing "wait" on loading huge file

  {$ifdef allow_proc_msg}
  Application.ProcessMessages;
  {$endif}
end;

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
  cATClipboardFormatId:= RegisterClipboardFormat('Application/X-Laz-ATSynEdit-Block');
end;

procedure InitResourcesNicescroll;
begin
  cBitmapNiceScroll:= TBitmap.Create;
  cBitmapNiceScroll.LoadFromResourceName(HInstance, 'AB_MOVE');
  cBitmapNiceScroll.Transparent:= true;

  Screen.Cursors[crNiceScrollNone]:= LoadCursor(HInstance, 'AB_MOVE');
  Screen.Cursors[crNiceScrollUp]:= LoadCursor(HInstance, 'AB_MOVE_U');
  Screen.Cursors[crNiceScrollDown]:= LoadCursor(HInstance, 'AB_MOVE_D');
  Screen.Cursors[crNiceScrollLeft]:= LoadCursor(HInstance, 'AB_MOVE_L');
  Screen.Cursors[crNiceScrollRight]:= LoadCursor(HInstance, 'AB_MOVE_R');
end;

procedure FreeResources;
begin
  FreeAndNil(cBitmapNiceScroll);
end;

