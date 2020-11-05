var
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

{ TATFoldedMarks }

function TATFoldedMarks.FindByCoord(ACoord: TPoint): TATFoldedMark;
var
  Mark: TATFoldedMark;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Mark:= Items[i];
    if PtInRect(Mark.Coord, ACoord) then
      exit(Mark);
  end;
  Result.InitNone;
end;

{ TATFoldedMark }

procedure TATFoldedMark.Init(const ACoord: TRect; ALineFrom, ALineTo: integer);
begin
  Coord:= ACoord;
  LineFrom:= ALineFrom;
  LineTo:= ALineTo;
end;

procedure TATFoldedMark.InitNone;
begin
  Init(Rect(0, 0, 0, 0), -1, -1);
end;

function TATFoldedMark.IsInited: boolean;
begin
  Result:= LineFrom>=0;
end;

class operator TATFoldedMark.=(const a, b: TATFoldedMark): boolean;
begin
  Result:= false;
end;

{ TATSynScrollInfo }

procedure TATSynScrollInfo.Clear;
begin
  NPos:= 0;
  NPosLast:= 1;
  NMax:= 1;
  NPage:= 1;
  NPixelOffset:= 0;
  SmoothCharSize:= 1;
  SmoothPos:= 0;
  SmoothPosLast:= 1;
  SmoothMax:= 1;
  SmoothPage:= 1;
end;

procedure TATSynScrollInfo.SetZero;
begin
  NPos:= 0;
  NPixelOffset:= 0;
  SmoothPos:= 0;
end;

procedure TATSynScrollInfo.SetLast;
begin
  NPos:= NPosLast;
  NPixelOffset:= 0;
  SmoothPos:= SmoothPosLast;
end;

function TATSynScrollInfo.TopGapVisible: boolean;
begin
  Result:= NPos=-1;
end;

function TATSynScrollInfo.TotalOffset: integer;
begin
  Result:= NPos*SmoothCharSize + NPixelOffset;
end;

class operator TATSynScrollInfo.= (const A, B: TATSynScrollInfo): boolean;
begin
  Result:=
    (A.Vertical=B.Vertical) and
    (A.NPos=B.NPos) and
    (A.NMax=B.NMax) and
    (A.NPage=B.NPage) and
    (A.NPixelOffset=B.NPixelOffset);
end;

function EditorResourceScaleSuffix: string;
var
  N: integer;
begin
  N:= Screen.PixelsPerInch;
  if N>=96*2 then
    Result:= '_200'
  else
  if N>=(96 * 3 div 2) then
    Result:= '_150'
  else
    Result:= '';
end;

procedure InitEditorResources;
var
  Suffix: string;
begin
  Suffix:= EditorResourceScaleSuffix;

  Screen.Cursors[crNiceScrollNone]:= LoadCursor(HInstance, 'ATSYN_MOVE');
  Screen.Cursors[crNiceScrollUp]:= LoadCursor(HInstance, 'ATSYN_MOVE_U');
  Screen.Cursors[crNiceScrollDown]:= LoadCursor(HInstance, 'ATSYN_MOVE_D');
  Screen.Cursors[crNiceScrollLeft]:= LoadCursor(HInstance, 'ATSYN_MOVE_L');
  Screen.Cursors[crNiceScrollRight]:= LoadCursor(HInstance, 'ATSYN_MOVE_R');

  cBitmapNiceScroll:= TPortableNetworkGraphic.Create;
  cBitmapNiceScroll.LoadFromResourceName(HInstance, 'ATSYN_SCROLLMARK'+Suffix);

  cBitmapWait:= TPortableNetworkGraphic.Create;
  cBitmapWait.LoadFromResourceName(HInstance, 'ATSYN_WAIT');

  cBitmapSaving:= TPortableNetworkGraphic.Create;
  cBitmapSaving.LoadFromResourceName(HInstance, 'ATSYN_SAVE');

  cBitmapFoldPlus:= TPortableNetworkGraphic.Create;
  cBitmapFoldPlus.LoadFromResourceName(HInstance, 'ATSYN_FOLD_P'+Suffix);

  cBitmapFoldMinus:= TPortableNetworkGraphic.Create;
  cBitmapFoldMinus.LoadFromResourceName(HInstance, 'ATSYN_FOLD_M'+Suffix);
end;

procedure FreeEditorResources;
begin
  FreeAndNil(cBitmapWait);
  FreeAndNil(cBitmapSaving);
  FreeAndNil(cBitmapNiceScroll);
  FreeAndNil(cBitmapFoldPlus);
  FreeAndNil(cBitmapFoldMinus);
end;

function EditorDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;


procedure InitEditorMouseActions(var M: TATMouseActions);
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
  Add(cMouseActionClickMiddle, [ssMiddle]);

  Add(cMouseActionClickSimple, [ssLeft, ssAlt]);
  Add(cMouseActionClickSimple, [ssLeft, ssAltGr]);
  Add(cMouseActionClickSimple, [ssLeft, ssAlt, ssXControl]);
  Add(cMouseActionClickSimple, [ssLeft, ssAlt, ssShift, ssXControl]);
end;


function EditorMouseActionId(const M: TATMouseActions; State: TShiftState): TATMouseActionId;
var
  i: integer;
begin
  Result:= cMouseActionNone;
  if State=[] then exit;

  for i:= Low(M) to High(M) do
    if M[i].MouseState=State then
      exit(M[i].MouseActionId);
end;

function EditorIsPressedCtrl: boolean;
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

function EditorLinkIsEmail(const S: string): boolean;
const
  MailPrefix = 'mailto:';
begin
  if SBeginsWith(S, MailPrefix) then
    Result:= true
  else
  if (Pos('://', S)=0) and (Pos('@', S)>0) and (Pos('.', S)>0) then
    Result:= true
  else
    Result:= false;
end;

procedure EditorOpenLink(const S: string);
const
  MailPrefix = 'mailto:';
begin
  if EditorLinkIsEmail(S) then
  begin
    if SBeginsWith(S, MailPrefix) then
      OpenURL(S)
    else
      OpenURL(MailPrefix+S);
  end
  else
  if SBeginsWith(S, 'www.') then
    OpenURL('http://'+S)
  else
  if SBeginsWith(S, 'ftp.') then
    OpenURL('ftp://'+S)
  else
    OpenURL(S);
end;

procedure SRemoveAsciiControlChars(var S: UnicodeString; AReplaceChar: Widechar);
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if IsCharAsciiControl(S[i]) then
      S[i]:= AReplaceChar;
end;

//it's like LCLProc.IsEditableTextKey() but changed to ignore Num-keys
function IsEditorHotkeyForText(Key: Word): Boolean;
begin
  case Key of
    VK_A..VK_Z,
    //VK_NUMPAD0..VK_DIVIDE,
    VK_0..VK_9,
    186..192,
    219..222:
      Result:= true;
    else
     Result:= false;
   end;
end;

function IsPointsDiffByDelta(const P1, P2: TPoint; Delta: integer): boolean; inline;
begin
  Result:=
    (Abs(P1.X-P2.X)>=Delta) or
    (Abs(P1.Y-P2.Y)>=Delta);

end;

