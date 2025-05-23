{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCalcLineHiliteEx(ALineIndex: integer;
  var AParts: TATLineParts;
  AColorBG: TColor; out AColorAfter: TColor; AWithSelection: boolean): boolean;
const
  cMaxCharsInLine = 50*1024;
var
  Data: TATWrapItem;
begin
  AParts:= Default(TATLineParts);
  Result:= Strings.IsIndexValid(ALineIndex);
  if not Result then exit;

  Data.NLineIndex:= ALineIndex;
  Data.NCharIndex:= 1;
  Data.NLength:= Strings.LinesLen[ALineIndex];
  Data.NIndent:= 0;
  Data.NFinal:= TATWrapItemFinal.Final;
  if Data.NLength=0 then exit;

  AColorAfter:= AColorBG;
  DoCalcLineHilite(
    Data,
    AParts,
    0,
    cMaxCharsInLine,
    AColorBG,
    false,
    AColorAfter,
    true,
    AWithSelection);
end;


procedure TATSynEdit.DoCalcLineHilite(const AData: TATWrapItem;
  var AParts: TATLineParts;
  ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor; AColorForced: boolean;
  var AColorAfter: TColor;
  AMainText, AWithSelection: boolean);
var
  nMaxOffset, nCharIndex, nLineIndex, nLineLen: integer;
  bEnableHilite: boolean;
begin
  nMaxOffset:= Min(ACharsMax, AData.NLength-ACharsSkipped);
  nLineIndex:= AData.NLineIndex;
  nLineLen:= AData.NLength;
  nCharIndex:= AData.NCharIndex+ACharsSkipped;

  ///the following line gives problems with huge lines (len>10K) in HTML, CudaText issue #3694
  //bEnableHilite:= St.LinesLen[nLineIndex]<=FOptMaxLineLenToTokenize;
  bEnableHilite:= true;

  AParts:= Default(TATLineParts);
  if nLineLen=0 then exit;

  (*
  if FHighlightGitConflicts then
    if (nLineLen>=7{Git marker len}) then
      if St.IsIndexValid(nLineIndex) and
        (St.LinesGitMarker[nLineIndex]<>cGitMarkNone) then
      begin
        bEnableHilite:= false; //avoid normal highlighting for this line
        AParts[0].Len:= nLineLen;
        AParts[0].Offset:= 0;
        AParts[0].ColorBG:= Colors.GitMarkerBG;
        AParts[0].ColorFont:= Colors.TextBG;
      end;
      *)

  if bEnableHilite then
  begin
    //1) get highlight from adapter
    if Assigned(FAdapterHilite) then
      FAdapterHilite.OnEditorCalcHilite(Self, AParts,
        nLineIndex,
        nCharIndex,
        nLineLen,
        AColorAfter,
        AMainText);

    //2) get highlight from OnCalcHilite
    if Assigned(FOnCalcHilite) then
      FOnCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorAfter);
  end;

  DoPartSetColorBG(AParts, AColorBG, AColorForced);
  if AColorForced then
    AColorAfter:= AColorBG;

  //first add Attribs,
  //selection must be over attribs
  if AMainText then
    DoPartCalc_ApplyAttribsOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AColorBG, AMainText);

  //_CreateNew makes parts for selection and fills empty AParts with these parts.
  //_ApplySelectionOver makes parts for selection and inserts these one-by-one over ready AParts
  //calculated before (in adapter or OnCalcHilite event).
  //Maybe it's possible to always use _ApplySelectionOver, but it's slower so I made _CreateNew
  //for faster rendering w/out adapter.
  if (AParts[0].Len>0) then
  begin
    if AWithSelection and TempSel_IsLineWithSelection(nLineIndex) then
      DoPartCalc_ApplySelectionOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AMainText);
  end
  else
  begin
    //this must work w/o adapter, it makes simple long part in AParts
    DoPartCalc_CreateNew(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AColorBG, AWithSelection);
  end;
end;

procedure TATSynEdit.DoPartCalc_CreateNew(var AParts: TATLineParts;
  AOffsetMax, ALineIndex, ACharIndex: integer; AColorBG: TColor; AWithSelection: boolean);
var
  St: TATStrings;
  bSel, bSelPrev, bAdd: boolean;
  bWord, bWordPrev: boolean;
  nPartIndex, nPartOffset, nPartLen, nCharOffset, nCharInLine: integer;
  SLine: atString;
const
  //fix sub-pixel trembling of text after selection, visible on Qt5 and Cocoa.
  //we split long line to words => only one word (touched by selection edge) is trembling.
  bFixFontTrembling = {$if defined(LCLQt) or defined(LCLQt5) or defined(LCLQt6) or defined(LCLCocoa)} true {$else} false {$endif};
begin
  St:= Strings;
  if not AWithSelection or
    (TempSel_IsRangeSelected(ACharIndex, ALineIndex, ACharIndex+AOffsetMax, ALineIndex)=TATRangeSelection.AllUnselected) then
  begin
    nPartOffset:= 0;
    nPartLen:= Min(St.LinesLen[ALineIndex], AOffsetMax);
    nPartIndex:= 0;

    //split leading tab-chars to separate part
    //this is much needed for gtk2, which paints ExtTextOut with Dx much slower
    repeat
      if nPartLen=0 then Break;
      if St.LineCharAt(ALineIndex, ACharIndex+nPartOffset+1)<>#9 then Break;
      AParts[0].Offset:= 0;
      Inc(AParts[0].Len);
      AParts[0].ColorFont:= FColorFont;
      AParts[0].ColorBG:= AColorBG;
      Inc(nPartOffset);
      Dec(nPartLen);
      nPartIndex:= 1;
    until false;

    AParts[nPartIndex].Offset:= nPartOffset;
    AParts[nPartIndex].Len:= nPartLen;
    AParts[nPartIndex].ColorFont:= FColorFont;
    AParts[nPartIndex].ColorBG:= AColorBG;
    exit;
  end;

  bSel:= false;
  bSelPrev:= false;
  bWord:= false;
  bWordPrev:= false;
  nPartIndex:= -1;

  if bFixFontTrembling then
    SLine:= St.Lines[ALineIndex]
  else
    SLine:= '';

  for nCharOffset:= 0 to AOffsetMax do
  begin
    bSel:= IsPosSelected(ACharIndex+nCharOffset, ALineIndex);

    if bFixFontTrembling then
    begin
      nCharInLine:= ACharIndex+nCharOffset+1;
      if nCharInLine<=Length(SLine) then
        bWord:= IsCharWord(SLine[nCharInLine]);
    end;

    if nPartIndex<0 then
      bAdd:= true
    else
      bAdd:= (bSel<>bSelPrev) or (bWord<>bWordPrev);
    bSelPrev:= bSel;
    bWordPrev:= bWord;

    if not bAdd then
    begin
      Inc(AParts[nPartIndex].Len);
    end
    else
    begin
      Inc(nPartIndex);
      if nPartIndex>=High(AParts) then Break;
      with AParts[nPartIndex] do
      begin
        Offset:= nCharOffset;
        Len:= 1;
        if bSel then
        begin
          if not FOptKeepSelFontColor then
            ColorFont:= Colors.TextSelFont
          else
            ColorFont:= Colors.TextFont;
          ColorBG:= Colors.TextSelBG;
        end
        else
        begin
          ColorFont:= FColorFont;
          ColorBG:= AColorBG;
          {
          //debug
          ColorFont:= Random($ffff);
          if bWord then
            ColorBG:= clYellow
          else
            ColorBg:= clGray;
          }
        end;
      end;
    end;
  end;
end;


procedure TATSynEdit.DoPartCalc_ApplySelectionOver(var AParts: TATLineParts; AOffsetMax,
  ALineIndex, ACharIndex: integer; AMainText: boolean);
var
  bSel, bSelPrev: boolean;
  Part: TATLinePart;
  PartOther: ^TATLinePart;
  nRangeX1, nRangeX2: integer;
  iPart, i: integer;
begin
  //optimiziation for select-all case.
  //if entire line is selected, colorize all parts.
  if TempSel_IsLineAllSelected(ALineIndex) then
  begin
    for i:= Low(AParts) to High(AParts) do
      if AParts[i].Len=0 then
        Break
      else
      begin
        if not FOptKeepSelFontColor then
          AParts[i].ColorFont:= Colors.TextSelFont;
        AParts[i].ColorBG:= Colors.TextSelBG;
      end;
    exit;
  end;

  Part:= Default(TATLinePart);
  Part.ColorFont:= Colors.TextSelFont;
  Part.ColorBG:= Colors.TextSelBG;

  //we loop over parts
  //- fully selected parts, colorize them.
  //- fully unselected parts, skip.
  //- partly selected parts, save them to FPartsSel,
  //  and do slow loop (by offsets) over them.

  FPartsSel[AMainText]:= Default(TATLineParts);
  iPart:= 0;

  for i:= Low(AParts) to High(AParts) do
  begin
    if AParts[i].Len=0 then Break;
    nRangeX1:= ACharIndex + AParts[i].Offset;
    nRangeX2:= nRangeX1 + AParts[i].Len;
    case IsRangeSelected(nRangeX1, ALineIndex, nRangeX2, ALineIndex) of
      TATRangeSelection.AllSelected:
        begin
          if Part.ColorFont<>clNone then
            AParts[i].ColorFont:= Part.ColorFont;
          AParts[i].ColorBG:= Part.ColorBG; //clGreen
        end;
      TATRangeSelection.PartlySelected:
        begin
          FPartsSel[AMainText][iPart].Offset:= AParts[i].Offset;
          FPartsSel[AMainText][iPart].Len:= AParts[i].Len;
          Inc(iPart);
        end;
    end;
  end;

  for iPart:= Low(AParts) to High(AParts) do
  begin
    PartOther:= @FPartsSel[AMainText][iPart];
    if PartOther^.Len=0 then Break;

    bSel:= false;
    bSelPrev:= false;

    for i:= PartOther^.Offset to
            PartOther^.Offset + PartOther^.Len -1 do
    begin
      bSel:= IsPosSelected(ACharIndex+i, ALineIndex);

      if bSel and (i=AOffsetMax) then
      begin
        DoPartInsert(AParts, Part, true, AMainText);
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
        DoPartInsert(AParts, Part, true, AMainText);
      end;
      bSelPrev:= bSel;
    end;

    if bSelPrev then
      DoPartInsert(AParts, Part, true, AMainText);
  end;

  //DoPartsShow(AParts); ////debug
end;


procedure TATSynEdit.DoCalcPosColor(AX, AY: integer; var AColor: TColor;
  AMainText: boolean);
begin
  if Assigned(FAdapterHilite) then
    FAdapterHilite.OnEditorCalcPosColor(Self, AX, AY, AColor, AMainText);
end;

procedure TATSynEdit.DoCalcLineEntireColor(ALine: integer;
  AUseColorOfCurrentLine: boolean;
  AUseColorOfCurrentLine2: boolean;
  out AColor: TColor; out AColorForced: boolean;
  out AChosenEnum: TATEditorChosenBackColor;
  AHiliteLineWithSelection: boolean);
var
  St: TATStrings;
  NIndex, BmKind: integer;
  NMarkedLine1, NMarkedLine2: integer;
begin
  AColor:= clNone;
  AChosenEnum:= TATEditorChosenBackColor.None;
  St:= Strings;

  if DoGetMarkedLines(NMarkedLine1, NMarkedLine2) then
    if (ALine>=NMarkedLine1) and (ALine<=NMarkedLine2) then
    begin
      AColor:= Colors.MarkedLinesBG;
      AChosenEnum:= TATEditorChosenBackColor.MarkedRangeBG;
    end;

  //Colors.CurrentLineBG should be assigned _after_ Colors.MarkedLinesBG
  if AUseColorOfCurrentLine then
  begin
    if AUseColorOfCurrentLine2 and (Colors.CurrentLineBG2<>clNone) then
      AColor:= Colors.CurrentLineBG2
    else
      AColor:= Colors.CurrentLineBG;
    AChosenEnum:= TATEditorChosenBackColor.CurrentLineBG;
  end
  else
  if AHiliteLineWithSelection and TempSel_IsLineWithSelection(ALine) then
  begin
    if AUseColorOfCurrentLine2 and (Colors.CurrentLineBG2<>clNone) then
      AColor:= Colors.CurrentLineBG2
    else
      AColor:= Colors.CurrentLineBG;
    AChosenEnum:= TATEditorChosenBackColor.CurrentLineBG;
  end
  else
  begin
    NIndex:= St.Bookmarks.Find(ALine);
    if NIndex>=0 then
    begin
      BmKind:= St.Bookmarks[NIndex]^.Data.Kind;
      AColor:= Colors.BookmarkBG;
      AChosenEnum:= TATEditorChosenBackColor.BookmarkBG;
      if Assigned(FOnCalcBookmarkColor) then
        FOnCalcBookmarkColor(Self, BmKind, AColor);
    end
    else
    begin
      NIndex:= St.Bookmarks2.Find(ALine);
      if NIndex>=0 then
      begin
        BmKind:= St.Bookmarks2[NIndex]^.Data.Kind;
        AColor:= Colors.BookmarkBG;
        AChosenEnum:= TATEditorChosenBackColor.BookmarkBG;
        if Assigned(FOnCalcBookmarkColor) then
          FOnCalcBookmarkColor(Self, BmKind, AColor);
      end;
    end;
  end;

  AColorForced:= AColor<>clNone;
  if not AColorForced then
  begin
    AColor:= FColorBG;
    AChosenEnum:= TATEditorChosenBackColor.DefaultBG;
  end;
end;


procedure TATSynEdit.DoPartCalc_ApplyAttribsOver(var AParts: TATLineParts;
  AOffsetMax, ALineIndex, ACharIndex: integer; AColorBG: TColor; AMainText: boolean);
  //
  function IsAttrLongEnough(constref Attr: TATMarkerItem): boolean;
  begin
    Result:=
      (Attr.PosY=ALineIndex) and
      (Attr.PosX-ACharIndex+Attr.SelX>0);
  end;
  //
var
  Attr: TATMarkerItem;
  Part: TATLinePart;
  nAttrIndex, nOffset, iAttr, j: integer;
  bExact: boolean;
begin
  if FAttribs=nil then exit;
  if FAttribs.Count=0 then exit;

  FAttribs.Find(ACharIndex-1, ALineIndex, nAttrIndex, bExact);
  if nAttrIndex<0 then exit; //not IsIndexValid, because index=FAttribs.Count is ok
  if nAttrIndex>0 then //fixes disappeared attrib on horz scrolling
    Dec(nAttrIndex);

  //for long wrapped line with long URL attrib, overlapped by short 'Highlight All' attrib(s):
  //if nAttrIndex is of 'Highlight All' attrib, we must decrement index until we find long attrib;
  //CudaText issue #5525
  if (nAttrIndex>0) and not IsAttrLongEnough(FAttribs[nAttrIndex]) then
  begin
    j:= nAttrIndex-1;
    while (j>0) and not IsAttrLongEnough(FAttribs[j]) do
      Dec(j);
    if IsAttrLongEnough(FAttribs[j]) then
      nAttrIndex:= j;
  end;

  for iAttr:= nAttrIndex to FAttribs.Count-1 do
  begin
    Attr:= FAttribs[iAttr];
    if Attr.MicromapMode=TATMarkerMicromapMode.MicromapOnly then Continue;
    if Attr.PosY<ALineIndex then Continue;
    if Attr.PosY>ALineIndex then Break;

    //must check offset, for part of wrapped line
    nOffset:= Attr.PosX-ACharIndex;
    if (nOffset>=AOffsetMax) then Continue;
    if (nOffset+Attr.SelX<=0) then Continue;

    //fix part visible partly, because of horz scroll
    if (nOffset<0) then
    begin
      Inc(Attr.SelX, nOffset);
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

    Part:= Attr.LinePart;
    Part.Len:= Attr.SelX;
    Part.Offset:= nOffset;
    //note: don't change Part.ColorBG here
    DoPartInsert(AParts, Part, false, AMainText);
  end;
end;


