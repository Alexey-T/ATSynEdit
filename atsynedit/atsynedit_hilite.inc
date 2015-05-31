{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCalcLineHilite(const AItem: TATSynWrapItem;
  var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor);
var
  nIndex, nMaxOffset, i: integer;
  bSel, bSelPrev, bAdd, bApplyOver: boolean;
  Part: TATLinePart;
begin
  FillChar(AParts, SizeOf(AParts), 0);

  if Assigned(FOnCalcLineHilite) then
    FOnCalcLineHilite(Self, AItem, AParts);

  bApplyOver:= AParts[0].Len>0; //event returned some parts
  bSel:= false;
  bSelPrev:= false;
  nIndex:= -1;

  FillChar(Part{%H-}, SizeOf(Part), 0);
  Part.Color:= FColors.TextSelFont;
  Part.ColorBG:= Colors.TextSelBG;

  nMaxOffset:= Min(ACharsMax, AItem.NLength-1-ACharsSkipped +2{space eol});
  for i:= 0 to nMaxOffset-1 do
  begin
    bSel:= IsPosSelected(AItem.NCharIndex-1+i+ACharsSkipped, AItem.NLineIndex);

    if bApplyOver then
    //---------------------------------
    //event was called and made AParts,
    //now calculate selected parts and insert them over AParts
    begin
      if bSel and (i=nMaxOffset-1) then
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
    end
    else
    //---------------------------------
    //event wasnt called, make all Parts based on selection
    begin
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
            Color:= FColors.TextSelFont;//random($ffff);
            ColorBG:= FColors.TextSelBG;
          end
          else
          begin
            Color:= FColors.TextFont;//random($ffff);
            ColorBG:= AColorBG;
          end;
        end;
      end;
    end;
  end;
end;


procedure TATSynEdit.DoPartFindPos(const AParts: TATLineParts; APos: integer;
  out AIndex, AIncLeft, AIncRight: integer);
var
  iStart, iEnd, i: integer;
begin
  AIndex:= -1;
  AIncLeft:= -1;
  AIncRight:= -1;
  for i:= 0 to High(AParts) do
  begin
    if AParts[i].Len=0 then Break;
    iStart:= AParts[i].Offset;
    iEnd:= iStart+AParts[i].Len;
    if APos=iStart then
      begin AIndex:= i; Break end;
    if (APos>=iStart) and (APos<iEnd) then
      begin AIndex:= i; AIncLeft:= APos-iStart; AIncRight:= iEnd-APos; Break end;
  end;
end;

procedure TATSynEdit.DoPartInsert(var AParts: TATLineParts; const APart: TATLinePart);
var
  nStart, nEnd: integer;
  nIndex1, nIndex2, nInc1L, nInc1R, nInc2L, nInc2R: integer;
  ResParts: TATLineParts;
begin
  nStart:= APart.Offset;
  nEnd:= nStart+APart.Len;

  DoPartFindPos(AParts, nStart, nIndex1, nInc1L, nInc1R);
  DoPartFindPos(AParts, nEnd, nIndex2, nInc2L, nInc2R);
  ////////////////////todo
end;

