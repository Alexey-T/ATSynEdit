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

  FillChar(Part, SizeOf(Part), 0);
  Part.Color:= FColors.TextSelFont;
  Part.ColorBG:= Colors.TextSelBG;

  nMaxOffset:= Min(ACharsMax, AItem.NLength-1-ACharsSkipped +2{space eol});
  for i:= 0 to nMaxOffset-1 do
  begin
    bSel:= IsPosSelected(AItem.NCharIndex-1+i+ACharsSkipped, AItem.NLineIndex);

    if bApplyOver then
    //event was called and did hiliting,
    //now calculate selected parts and insert them over AParts
    begin
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
        Part.Offset:= 0;
        Part.Len:= 0;
      end;
      bSelPrev:= bSel;
    end
    else
    //event OnCalcLine wasnt called,
    //make all Parts based on selection
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

procedure TATSynEdit.DoPartInsert(var AParts: TATLineParts; const APart: TATLinePart);
begin
  //AParts[0].ColorBG:= clgreen;
  //AParts[1].ColorBG:= clyellow;
end;

