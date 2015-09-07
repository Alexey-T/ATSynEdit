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

//fold gutter fold
var
  cBitmapFoldPlus: TBitmap = nil;
  cBitmapFoldMinus: TBitmap = nil;

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

procedure InitResourcesFoldbar;
begin
  if cBitmapFoldPlus=nil then
  begin
    cBitmapFoldPlus:= TBitmap.Create;
    cBitmapFoldPlus.LoadFromResourceName(HInstance, 'FOLDBAR_P');
    cBitmapFoldPlus.Transparent:= true;
  end;

  if cBitmapFoldMinus=nil then
  begin
    cBitmapFoldMinus:= TBitmap.Create;
    cBitmapFoldMinus.LoadFromResourceName(HInstance, 'FOLDBAR_M');
    cBitmapFoldMinus.Transparent:= true;
  end;
end;

procedure FreeResources;
begin
  FreeAndNil(cBitmapNiceScroll);
end;

procedure InitDefaultColors(C: TATSynEditColors);
begin
  C.TextFont:= clBlack;
  C.TextBG:= clWhite;
  C.TextSelFont:= clHighlightText;
  C.TextSelBG:= clHighlight;
  C.TextDisabledFont:= clGray;
  C.TextDisabledBG:= $f0f0f0;
  C.Caret:= clBlack;
  C.GutterFont:= clGray;
  C.GutterBG:= $e0e0e0;
  C.GutterCaretBG:= $c8c8c8;
  C.GutterPlusBorder:= clGray;
  C.GutterPlusBG:= $f4f4f4;
  C.GutterFoldLine:= clGray;
  C.GutterFoldBG:= $c8c8c8;
  C.GutterSeparatorBG:= clBlack;
  C.CurrentLineBG:= $e0f0f0;
  C.BookmarkBG:= clMoneyGreen;
  C.RulerBG:= C.GutterBG;
  C.RulerFont:= clGray;
  C.CollapseLine:= $a06060;
  C.CollapseMarkFont:= $e08080;
  C.CollapseMarkBG:= clCream;
  C.MarginRight:= clLtGray;
  C.MarginCaret:= clLime;
  C.MarginUser:= clYellow;
  C.IndentVertLines:= clMedGray;
  C.UnprintedFont:= $5050f0;
  C.UnprintedBG:= $e0e0e0;
  C.UnprintedHexFont:= clMedGray;
  C.MinimapBorder:= clLtGray;
  C.MinimapSelBG:= $eeeeee;
  C.StateChanged:= $00f0f0;
  C.StateAdded:= $20c020;
  C.StateSaved:= clMedGray;
  C.TextHintFont:= clGray;
  C.BlockStaple:= clMedGray;
  C.BlockSepLine:= clMedGray;
  C.LockedBG:= $e0e0e0;
  C.ComboboxArrow:= clGray;
  C.ComboboxArrowBG:= $f0f0f0;
end;

