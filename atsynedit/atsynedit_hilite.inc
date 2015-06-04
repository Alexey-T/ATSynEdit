{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCalcLineHilite(const AItem: TATSynWrapItem;
  var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor);
var
  nMaxOffset, nCharIndex, nLineIndex: integer;
begin
  nMaxOffset:= Min(ACharsMax, AItem.NLength-ACharsSkipped);
  nLineIndex:= AItem.NLineIndex;
  nCharIndex:= AItem.NCharIndex+ACharsSkipped;

  FillChar(AParts, SizeOf(AParts), 0);
  if Assigned(FOnCalcHilite) then
    FOnCalcHilite(Self, AParts, AItem, nCharIndex);

  if AParts[0].Len>0 then
    DoPartCalc_ApplyOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1)
  else
    DoPartCalc_CreateNew(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AColorBG);
end;

procedure DoPartFind(const AParts: TATLineParts; APos: integer; out
  AIndex, AOffsetLeft: integer);
var
  iStart, iEnd, i: integer;
begin
  AIndex:= -1;
  AOffsetLeft:= 0;

  for i:= Low(AParts) to High(AParts)-1 do
  begin
    if AParts[i].Len=0 then
    begin
      //pos after last part?
      if i>Low(AParts) then
        if APos>=AParts[i-1].Offset+AParts[i-1].Len then
          AIndex:= i;
      Break;
    end;

    iStart:= AParts[i].Offset;
    iEnd:= iStart+AParts[i].Len;

    //pos at part begin?
    if APos=iStart then
      begin AIndex:= i; Break end;

    //pos at part middle?
    if (APos>=iStart) and (APos<iEnd) then
      begin AIndex:= i; AOffsetLeft:= APos-iStart; Break end;
  end;
end;

procedure DoPartInsert(var AParts: TATLineParts; const APart: TATLinePart);
var
  ResParts: TATLineParts;
  nResPart: integer;
  //
  procedure AddPart(const P: TATLinePart);
  begin
    if P.Len=0 then Exit;
    Move(P, ResParts[nResPart], SizeOf(P));
    Inc(nResPart);
  end;
  //
var
  nIndex1, nIndex2,
  nOffset1, nOffset2,
  newLen1, newLen2, newOffset2: integer;
  i: integer;
begin
  DoPartFind(AParts, APart.Offset, nIndex1, nOffset1);
  DoPartFind(AParts, APart.Offset+APart.Len, nIndex2, nOffset2);
  if nIndex1<0 then Exit;
  if nIndex2<0 then Exit;

  with AParts[nIndex1] do
  begin
    newLen1:= nOffset1;
  end;
  with AParts[nIndex2] do
  begin
    newLen2:= Len-nOffset2;
    newOffset2:= Offset+nOffset2;
  end;

  FillChar(ResParts, SizeOf(ResParts), 0);
  nResPart:= 0;

  //add to result parts before selection
  for i:= 0 to nIndex1-1 do AddPart(AParts[i]);
  if nOffset1>0 then
  begin
    AParts[nIndex1].Len:= newLen1;
    AddPart(AParts[nIndex1]);
  end;

  //add APart
  AddPart(APart);

  //add parts after selection
  if nOffset2>0 then
  begin
    AParts[nIndex2].Len:= newLen2;
    AParts[nIndex2].Offset:= newOffset2;
  end;

  for i:= nIndex2 to High(AParts) do
  begin
    if AParts[i].Len=0 then Break;
    AddPart(AParts[i]);
  end;

  //application.mainform.caption:= format('n1 %d, n2 %d, of len %d %d',
  //  [nindex1, nindex2, aparts[nindex2].offset, aparts[nindex2].len]);

  //copy result
  Move(ResParts, AParts, SizeOf(AParts));
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
          ColorFont:= FColors.TextFont;//random($ffff);
          ColorBG:= AColorBG;
        end;
      end;
    end;
  end;
end;

procedure TATSynEdit.DoPartCalc_ApplyOver(var AParts: TATLineParts;
  AOffsetMax, ALineIndex, ACharIndex: integer);
var
  bSel, bSelPrev: boolean;
  Part: TATLinePart;
  i: integer;
begin
  FillChar(Part{%H-}, SizeOf(Part), 0);
  Part.ColorFont:= FColors.TextSelFont;
  Part.ColorBG:= Colors.TextSelBG;

  bSel:= false;
  bSelPrev:= false;

  for i:= 0 to AOffsetMax do
  begin
    bSel:= IsPosSelected(ACharIndex+i, ALineIndex);

    if bSel and (i=AOffsetMax) then
    begin
      DoPartInsert(AParts, Part);
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
      DoPartInsert(AParts, Part);
    end;
    bSelPrev:= bSel;
  end;
end;


