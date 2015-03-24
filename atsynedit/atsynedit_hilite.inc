{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCalcLineHilite(const AItem: TATSynWrapItem;
  var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor);
var
  nIndex, nMaxOffset, i: integer;
  bSel, bSelPrev, bAdd: boolean;
begin
  FillChar(AParts, SizeOf(AParts), 0);

  bSel:= false;
  bSelPrev:= false;
  nIndex:= -1;

  nMaxOffset:= Min(ACharsMax, AItem.NLength-1-ACharsSkipped +2{space eol});

  for i:= 0 to nMaxOffset-1 do
  begin
    bSel:= IsPosSelected(AItem.NCharIndex-1+i+ACharsSkipped, AItem.NLineIndex);

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
          Color:= FColors.TextSel;//random($ffff);
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

