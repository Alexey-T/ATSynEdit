var
  cResourceScaleSuffix: string = '';
  cBitmapWait: TPortableNetworkGraphic = nil;
  cBitmapSaving: TPortableNetworkGraphic = nil;
  cBitmapNiceScroll: TPortableNetworkGraphic = nil;
  cBitmapFoldPlus: TPortableNetworkGraphic = nil;
  cBitmapFoldMinus: TPortableNetworkGraphic = nil;

const
  crNiceScrollNone  = TCursor(-40);
  crNiceScrollUp    = TCursor(-41);
  crNiceScrollDown  = TCursor(-42);
  crNiceScrollLeft  = TCursor(-43);
  crNiceScrollRight = TCursor(-44);

const
  //under Mac don't use Ctrl key, use Meta key as default (e.g. Meta+C, Meta+A)
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
  cATClipboardFormatId:= RegisterClipboardFormat('Application/X-Laz-ATSynEdit-Block');
end;

function _GetResourceScaleSuffix: string;
begin
  if Screen.PixelsPerInch>=96*2 then
    Result:= '_200'
  else
  if Screen.PixelsPerInch>=Round(96*1.5) then
    Result:= '_150'
  else
    Result:= '';
end;

procedure InitResources;
begin
  cResourceScaleSuffix:= _GetResourceScaleSuffix;

  Screen.Cursors[crNiceScrollNone]:= LoadCursor(HInstance, 'ATSYN_MOVE');
  Screen.Cursors[crNiceScrollUp]:= LoadCursor(HInstance, 'ATSYN_MOVE_U');
  Screen.Cursors[crNiceScrollDown]:= LoadCursor(HInstance, 'ATSYN_MOVE_D');
  Screen.Cursors[crNiceScrollLeft]:= LoadCursor(HInstance, 'ATSYN_MOVE_L');
  Screen.Cursors[crNiceScrollRight]:= LoadCursor(HInstance, 'ATSYN_MOVE_R');

  cBitmapNiceScroll:= TPortableNetworkGraphic.Create;
  cBitmapNiceScroll.LoadFromResourceName(HInstance, 'ATSYN_SCROLLMARK'+cResourceScaleSuffix);

  cBitmapWait:= TPortableNetworkGraphic.Create;
  cBitmapWait.LoadFromResourceName(HInstance, 'ATSYN_WAIT');

  cBitmapSaving:= TPortableNetworkGraphic.Create;
  cBitmapSaving.LoadFromResourceName(HInstance, 'ATSYN_SAVE');

  cBitmapFoldPlus:= TPortableNetworkGraphic.Create;
  cBitmapFoldPlus.LoadFromResourceName(HInstance, 'ATSYN_FOLD_P'+cResourceScaleSuffix);

  cBitmapFoldMinus:= TPortableNetworkGraphic.Create;
  cBitmapFoldMinus.LoadFromResourceName(HInstance, 'ATSYN_FOLD_M'+cResourceScaleSuffix);
end;

procedure FreeResources;
begin
  FreeAndNil(cBitmapWait);
  FreeAndNil(cBitmapSaving);
  FreeAndNil(cBitmapNiceScroll);
end;

function IsDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;


procedure InitMouseActions(var M: TATMouseActions);
  //
  procedure Add(AId: TATMouseActionId; AState: TShiftState);
  begin
    SetLength(M, Length(M)+1);
    M[High(M)].MouseActionId:= AId;
    M[High(M)].MouseState:= AState;
  end;
  //
begin
  SetLength(M, 0);

  Add(cMouseActionClickSimple, [ssLeft]);
  Add(cMouseActionClickRight, [ssRight]);
  Add(cMouseActionClickAndSelNormalBlock, [ssLeft, ssShift]);
  Add(cMouseActionClickAndSelVerticalBlock, [ssLeft, ssAlt, ssShift]);
  Add(cMouseActionMakeCaret, [ssLeft, ssXControl]);
  Add(cMouseActionMakeCaretsColumn, [ssLeft, ssXControl, ssShift]);
  Add(cMouseActionNiceScrolling, [ssMiddle]);

  Add(cMouseActionClickSimple, [ssLeft, ssAlt]);
  Add(cMouseActionClickSimple, [ssLeft, ssAltGr]);
  Add(cMouseActionClickSimple, [ssLeft, ssAlt, ssXControl]);
  Add(cMouseActionClickSimple, [ssLeft, ssAlt, ssShift, ssXControl]);
end;


function GetMouseActionId(const M: TATMouseActions; State: TShiftState): TATMouseActionId;
var
  i: integer;
begin
  Result:= cMouseActionNone;
  if State=[] then exit;

  for i:= Low(M) to High(M) do
    if M[i].MouseState=State then
      exit(M[i].MouseActionId);
end;

function IsPressedCtrl: boolean;
begin
  {$ifdef darwin}
  //on macOS use Command-key instead of Ctrl
  Result:=
    (GetKeyState(VK_LWIN)<0) or
    (GetKeyState(VK_RWIN)<0);
  {$else}
  Result:= GetKeyState(VK_CONTROL)<0;
  {$endif}
end;


