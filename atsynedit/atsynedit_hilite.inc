{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCalcLineHiliteEx(ALineIndex: integer;
  var AParts: TATLineParts;
  AColorBG: TColor; out AColorAfter: TColor): boolean;
const
  cMaxCharsInLine = 50*1024;
var
  Data: TATWrapItem;
begin
  FillChar(AParts, SizeOf(AParts), 0);
  Result:= Strings.IsIndexValid(ALineIndex);
  if not Result then exit;

  Data.NLineIndex:= ALineIndex;
  Data.NCharIndex:= 1;
  Data.NLength:= Strings.LinesLen[ALineIndex];
  Data.NIndent:= 0;
  Data.NFinal:= cWrapItemFinal;
  if Data.NLength=0 then exit;

  AColorAfter:= AColorBG;
  DoCalcLineHilite(Data, AParts, 0, cMaxCharsInLine,
    AColorBG, false, AColorAfter, true);
end;


procedure TATSynEdit.DoCalcLineHilite(const AData: TATWrapItem;
  var AParts: TATLineParts;
  ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor; AColorForced: boolean;
  var AColorAfter: TColor;
  AMainText: boolean);
var
  nMaxOffset, nCharIndex, nLineIndex, nLineLen: integer;
  bEnableHilite, bFound: boolean;
begin
  nMaxOffset:= Min(ACharsMax, AData.NLength-ACharsSkipped);
  nLineIndex:= AData.NLineIndex;
  nLineLen:= AData.NLength;
  nCharIndex:= AData.NCharIndex+ACharsSkipped;
  bEnableHilite:= Strings.LinesLenRaw[nLineIndex]<=FOptMaxLineLenToTokenize;

  FillChar(AParts, SizeOf(AParts), 0);

  //1) get hilites from adapter
  if bEnableHilite then
  if Assigned(FAdapterHilite) then
  begin
    {$ifdef atsynedit_cache}
    //DynamicHilite must disable cache
    FAdapterCache.Enabled:= not FAdapterHilite.DynamicHiliteActiveNow(Strings.Count);
    bFound:= FAdapterCache.Get(nLineIndex, nCharIndex, nLineLen, AParts, AColorAfter);
    if not bFound then
    begin
      FAdapterHilite.OnEditorCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorAfter);
      FAdapterCache.Add(nLineIndex, nCharIndex, nLineLen, AParts, AColorAfter);
    end;
    {$else}
    FAdapterHilite.OnEditorCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorAfter);
    {$endif}
  end;

  //2) get hilites from OnCalcHilite
  if bEnableHilite then
  if Assigned(FOnCalcHilite) then
    FOnCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorAfter);

  DoPartSetColorBG(AParts, AColorBG, AColorForced);
  if AColorForced then
    AColorAfter:= AColorBG;

  //first add Attribs,
  //selection must be over attribs
  if AMainText then
    DoPartCalc_ApplyAttribsOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AColorBG);

  //CreateNew makes parts for selection and fills empty AParts with these parts.
  //ApplyOver makes parts for selection and inserts these one-by-one over ready AParts
  //calculated before (in adapter or OnCalc event).
  //Maybe possible to always use ApplyOver but it's slower so i made CreateNew for
  //faster render w/out adapter
  if (AParts[0].Len>0) then
  begin
    DoPartCalc_ApplySelectionOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1);
  end
  else
  begin
    //this must work w/o adapter, it makes simple long part in AParts
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
          ColorFont:= FColorFont;//random($ffff);
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
  PartOther: ^TATLinePart;
  nRangeX1, nRangeX2, iPart, i: integer;
begin
  if not Carets.IsSelection then exit;

  FillChar(Part{%H-}, SizeOf(Part), 0);
  Part.ColorFont:= Colors.TextSelFont;
  Part.ColorBG:= Colors.TextSelBG;

  //optimiziation for select-all case.
  //if entire line is selected, colorize all parts.

  nRangeX1:= ACharIndex;
  for i:= Low(AParts) to High(AParts) do
    if AParts[i].Len=0 then
    begin
      if i=0 then exit;
      nRangeX2:= ACharIndex + AParts[i-1].Offset + AParts[i-1].Len;
      Break;
    end;

  case IsRangeSelected(nRangeX1, ALineIndex, nRangeX2, ALineIndex) of
    cRangeAllUnselected:
      exit;
    cRangeAllSelected:
      begin
        for i:= Low(AParts) to High(AParts) do
          if AParts[i].Len=0 then
            Break
          else
          begin
            AParts[i].ColorFont:= Part.ColorFont;
            AParts[i].ColorBG:= Part.ColorBG; //clGreen
          end;
        exit;
      end;
  end;

  //we loop over parts
  //- fully selected parts, colorize them.
  //- fully unselected parts, skip.
  //- partly selected parts, save them to FLineOtherParts,
  //  and do slow loop (by offsets) over them.

  FillChar(FLineOtherParts, SizeOf(TATLineParts), 0);
  iPart:= 0;

  for i:= Low(AParts) to High(AParts) do
  begin
    if AParts[i].Len=0 then Break;
    nRangeX1:= ACharIndex + AParts[i].Offset;
    nRangeX2:= nRangeX1 + AParts[i].Len;
    case IsRangeSelected(nRangeX1, ALineIndex, nRangeX2, ALineIndex) of
      cRangeAllSelected:
        begin
          AParts[i].ColorFont:= Part.ColorFont;
          AParts[i].ColorBG:= Part.ColorBG; //clGreen
        end;
      cRangePartlySelected:
        begin
          FLineOtherParts[iPart].Offset:= AParts[i].Offset;
          FLineOtherParts[iPart].Len:= AParts[i].Len;
          Inc(iPart);
        end;
    end;
  end;

  for iPart:= Low(AParts) to High(AParts) do
  begin
    PartOther:= @FLineOtherParts[iPart];
    if PartOther^.Len=0 then Break;

    bSel:= false;
    bSelPrev:= false;

    for i:= PartOther^.Offset to
            PartOther^.Offset + PartOther^.Len -1 do
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

    if bSelPrev then
      DoPartInsert(AParts, Part, true);
  end;

  //DoPartsShow(AParts); ////debug
end;


procedure TATSynEdit.DoCalcPosColor(AX, AY: integer; var AColor: TColor);
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorCalcPosColor(Self, AX, AY, AColor);
end;

procedure TATSynEdit.DoCalcLineEntireColor(ALine: integer;
  AUseColorOfCurrentLine: boolean;
  out AColor: TColor; out AColorForced: boolean;
  AHiliteLineWithSelection: boolean);
var
  NIndex, BmKind: integer;
begin
  AColor:= clNone;

  if AUseColorOfCurrentLine then
    AColor:= Colors.CurrentLineBG
  else
  if AHiliteLineWithSelection and Carets.IsLineWithSelection(ALine) then
    AColor:= Colors.CurrentLineBG
  else
  begin
    NIndex:= Strings.Bookmarks.Find(ALine);
    if NIndex>=0 then
    begin
      BmKind:= Strings.Bookmarks[NIndex].Data.Kind;
      AColor:= Colors.BookmarkBG;
      if Assigned(FOnCalcBookmarkColor) then
        FOnCalcBookmarkColor(Self, BmKind, AColor);
    end
    else
    begin
      NIndex:= Strings.Bookmarks2.Find(ALine);
      if NIndex>=0 then
      begin
        BmKind:= Strings.Bookmarks2[NIndex].Data.Kind;
        AColor:= Colors.BookmarkBG;
        if Assigned(FOnCalcBookmarkColor) then
          FOnCalcBookmarkColor(Self, BmKind, AColor);
      end;
    end;
  end;

  if Assigned(FMarkedRange) then
   if FMarkedRange.Count=2 then
    if (ALine>=FMarkedRange.Items[0].PosY) and
       (ALine<=FMarkedRange.Items[1].PosY) then
      AColor:= Colors.MarkedLinesBG;

  AColorForced:= AColor<>clNone;
  if not AColorForced then
    AColor:= FColorBG;
end;


procedure TATSynEdit.DoPartCalc_ApplyAttribsOver(var AParts: TATLineParts;
  AOffsetMax, ALineIndex, ACharIndex: integer; AColorBG: TColor);
var
  Attr: TATMarkerItem;
  Part: TATLinePart;
  PartObj: TATLinePartClass;
  nAttrIndex, nOffset, i: integer;
  bExact: boolean;
begin
  if FAttribs=nil then exit;
  if FAttribs.Count=0 then exit;

  FAttribs.Find(ACharIndex-1, ALineIndex, nAttrIndex, bExact);
  if nAttrIndex<0 then exit; //not IsIndexValid, because index=FAttribs.Count is ok
  if nAttrIndex>0 then //fixes disappeared attrib on horz scrolling
    Dec(nAttrIndex);

  for i:= nAttrIndex to FAttribs.Count-1 do
  begin
    Attr:= FAttribs[i];
    if Attr.MicromapOnly then Continue;
    if Attr.PosY<ALineIndex then Continue;
    if Attr.PosY>ALineIndex then Break;

    PartObj:= TATLinePartClass(Attr.Ptr);
    if Assigned(PartObj) then
      begin
        //must check offset, for part of wrapped line
        nOffset:= Attr.PosX-ACharIndex;
        if (nOffset>=AOffsetMax) then Continue;
        if (nOffset+Attr.LenX<=0) then Continue;

        //fix part visible partly, because of horz scroll
        if (nOffset<0) then
        begin
          Inc(Attr.LenX, nOffset);
          nOffset:= 0;
        end;

        //empty parts? init part for whole line, for DoPartInsert to work
        if AParts[0].Len=0 then
        begin
          AParts[0].Offset:= 0;
          AParts[0].Len:= AOffsetMax;
          AParts[0].ColorBG:= AColorBG;
          AParts[0].ColorFont:= FColorFont;
        end;

        Part:= PartObj.Data;
        Part.Len:= Attr.LenX;
        Part.Offset:= nOffset;
        //note: don't change Part.ColorBG here
        DoPartInsert(AParts, Part, false);
      end;
  end;
end;


