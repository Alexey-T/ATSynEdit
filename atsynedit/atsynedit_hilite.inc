{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCalcLineHiliteEx(ALineIndex: integer;
  var AParts: TATLineParts;
  AColorBG: TColor; out AColorAfter: TColor): boolean;
const
  cMaxCharsInLine = 50*1024;
var
  WrapItem: TATSynWrapItem;
  Str: atString;
begin
  Result:= Strings.IsIndexValid(ALineIndex);
  if not Result then exit;
  FillChar(AParts, SizeOf(AParts), 0);

  Str:= Strings.Lines[ALineIndex];
  if Str='' then exit;

  WrapItem:= TATSynWrapItem.Create(ALineIndex, 1, Length(Str), 0, cWrapItemFinal);
  try
    AColorAfter:= AColorBG;
    DoCalcLineHilite(WrapItem, AParts, 0, cMaxCharsInLine,
      AColorBG, false, AColorAfter);
  finally
    FreeAndNil(WrapItem);
  end;
end;



procedure TATSynEdit.DoCalcLineHilite(const AItem: TATSynWrapItem;
  var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor; AColorForced: boolean; var AColorAfter: TColor);
var
  nMaxOffset, nCharIndex, nLineIndex, nLineLen: integer;
  bFound: boolean;
begin
  nMaxOffset:= Min(ACharsMax, AItem.NLength-ACharsSkipped);
  nLineIndex:= AItem.NLineIndex;
  nLineLen:= AItem.NLength;
  nCharIndex:= AItem.NCharIndex+ACharsSkipped;

  FillChar(AParts, SizeOf(AParts), 0);

  //1) get hilites from adapter
  if Assigned(FAdapterHilite) then
  begin
    //DynamicHilite must disable cache
    FAdapterCache.Enabled:= not FAdapterHilite.DynamicHiliteActiveNow(Strings.Count);
    bFound:= FAdapterCache.Get(nLineIndex, nCharIndex, nLineLen, AParts, AColorAfter);
    if not bFound then
    begin
      FAdapterHilite.OnEditorCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorAfter);
      FAdapterCache.Add(nLineIndex, nCharIndex, nLineLen, AParts, AColorAfter);
    end;
  end;

  //2) get hilites from OnCalcHilite
  if Assigned(FOnCalcHilite) then
    FOnCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorAfter);

  DoPartSetColorBG(AParts, AColorBG, AColorForced);
  if AColorForced then
    AColorAfter:= AColorBG;

  //first add Attribs,
  //selection must be over attribs
  DoPartCalc_ApplyAttribsOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AColorBG);

  //Createnew makes parts for selection and fills empty AParts with these parts.
  //Applyover makes parts for selection and inserts these one-by-one over ready AParts
  //calculated before (in adapter or OnCalc event).
  //Maybe possible to always use Applyover but it's slower so i made Createnew for
  //faster render w/out adapter
  if AParts[0].Len>0 then
  begin
    DoPartCalc_ApplySelectionOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1);
  end
  else
  begin
    DoPartCalc_CreateNew(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AColorBG);
  end;
end;


procedure TATSynEdit.DoPartCalc_CreateNew(var AParts: TATLineParts;
  AOffsetMax, ALineIndex, ACharIndex: integer; AColorBG: TColor);
var
  bSel, bSelPrev, bAdd: boolean;
  nIndex, i: integer;
begin
  bSel:= false;
  bSelPrev:= false;
  nIndex:= -1;

  for i:= 0 to AOffsetMax do
  begin
    bSel:= IsPosSelected(ACharIndex+i, ALineIndex);

    if nIndex<0 then
      bAdd:= true
    else
      bAdd:= bSel<>bSelPrev;
    bSelPrev:= bSel;

    if not bAdd then
    begin
      Inc(AParts[nIndex].Len);
    end
    else
    begin
      Inc(nIndex);
      if nIndex>=High(AParts) then Break;
      with AParts[nIndex] do
      begin
        Offset:= i;
        Len:= 1;
        if bSel then
        begin
          ColorFont:= FColors.TextSelFont;//random($ffff);
          ColorBG:= FColors.TextSelBG;
        end
        else
        begin
          ColorFont:= GetColorTextFont;//random($ffff);
          ColorBG:= AColorBG;
        end;
      end;
    end;
  end;
end;

procedure TATSynEdit.DoPartCalc_ApplySelectionOver(var AParts: TATLineParts; AOffsetMax,
  ALineIndex, ACharIndex: integer);
var
  bSel, bSelPrev: boolean;
  Part: TATLinePart;
  nOffsetStart, nRangeX1, nRangeX2, i: integer;
begin
  FillChar(Part{%H-}, SizeOf(Part), 0);
  Part.ColorFont:= Colors.TextSelFont;
  Part.ColorBG:= Colors.TextSelBG;

  //optimiziation for select-all case.
  //in this case we loop over parts, and for entire-selected parts,
  //just set them back-color.
  //after that do loop (by offsets) over rest.

  nOffsetStart:= 0;
  bSel:= false;
  bSelPrev:= false;

  for i:= Low(AParts) to High(AParts) do
  begin
    if AParts[i].Len=0 then break;
    nRangeX1:= ACharIndex + AParts[i].Offset;
    nRangeX2:= nRangeX1 + AParts[i].Len;
    if IsRangeSelected(nRangeX1, ALineIndex, nRangeX2, ALineIndex) then
    begin
      AParts[i].ColorBG:= Colors.TextSelBG;
      nOffsetStart:= AParts[i].Offset+AParts[i].Len;
    end;
  end;

  for i:= nOffsetStart to AOffsetMax do
  begin
    bSel:= IsPosSelected(ACharIndex+i, ALineIndex);

    if bSel and (i=AOffsetMax) then
    begin
      DoPartInsert(AParts, Part, true);
      Break
    end;

    if bSel and bSelPrev then
      Inc(Part.Len)
    else
    if not bSelPrev and bSel then
    begin
      Part.Offset:= i;
      Part.Len:= 1;
    end
    else
    if bSelPrev and not bSel then
    begin
      DoPartInsert(AParts, Part, true);
    end;
    bSelPrev:= bSel;
  end;

  //DoPartsShow(AParts); ////debug
end;


procedure TATSynEdit.DoCalcPosColor(AX, AY: integer; var AColor: TColor);
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorCalcPosColor(Self, AX, AY, AColor);
end;

procedure TATSynEdit.DoCalcLineEntireColor(ALine: integer; ACoordTop: integer;
  ALineWithCaret: boolean; out AColor: TColor; out AColorForced: boolean);
var
  BmKind: integer;
begin
  AColor:= clNone;

  BmKind:= Strings.LinesBm[ALine];
  if BmKind<>0 then
  begin
    AColor:= Colors.BookmarkBG;
    if Assigned(FOnCalcBookmarkColor) then
      FOnCalcBookmarkColor(Self, BmKind, AColor);
  end;

  if FOptShowCurLine and (not FOptShowCurLineOnlyFocused or Self.Focused) then
  begin
    if FOptShowCurLineMinimal then
    begin
      if ALineWithCaret and IsLinePartWithCaret(ALine, ACoordTop) then
        AColor:= Colors.CurrentLineBG;
    end
    else
    begin
      if ALineWithCaret then
        AColor:= Colors.CurrentLineBG;
    end;
  end;

  if FMarkedRange.Count=2 then
    if (ALine>=FMarkedRange.Items[0].PosY) and
       (ALine<=FMarkedRange.Items[1].PosY) then
      AColor:= Colors.MarkedLinesBG;

  AColorForced:= AColor<>clNone;
  if not AColorForced then
    AColor:= GetColorTextBG;
end;


procedure TATSynEdit.DoPartCalc_ApplyAttribsOver(var AParts: TATLineParts;
  AOffsetMax, ALineIndex, ACharIndex: integer; AColorBG: TColor);
var
  i: integer;
  Attr: TATMarkerItem;
  Part: TATLinePart;
  PartObj: TATLinePartClass;
begin
  for i:= 0 to Attribs.Count-1 do
  begin
    Attr:= Attribs[i];
    PartObj:= TATLinePartClass(Attr.Ptr);
    if Assigned(PartObj) then
      if Attr.PosY=ALineIndex then
      begin
        //empty parts? init part for whole line, for DoPartInsert to work
        if AParts[0].Len=0 then
        begin
          AParts[0].Offset:= 0;
          AParts[0].Len:= AOffsetMax;
          AParts[0].ColorBG:= AColorBG;
          AParts[0].ColorFont:= GetColorTextFont;
        end;

        Part:= PartObj.Data;
        Part.Len:= Attr.LenX;
        Part.Offset:= Attr.PosX-ACharIndex;
        if Part.ColorBG=clNone then
          Part.ColorBG:= AColorBG;
        DoPartInsert(AParts, Part, false);
      end;
  end;
end;


