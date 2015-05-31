{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCalcLineHilite(const AItem: TATSynWrapItem;
  var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor);
var
  nIndex, nMaxOffset, nPartLen, i: integer;
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
  nPartLen:= 0;

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
      Inc(nPartLen);
      AParts[nIndex].Len:= nPartLen;
    end
    else
    if not bApplyOver then
    begin
      Inc(nIndex);
      if nIndex>=High(AParts) then Break;
      with AParts[nIndex] do
      begin
        Offset:= i;
        nPartLen:= 1;
        Len:= nPartLen;
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
    end
    else
    if bSel then
    begin
      FillChar(Part, SizeOf(Part), 0);
      Part.Offset:= i;
      Part.Len:= nPartLen;
      Part.Color:= FColors.TextSelFont;
      Part.ColorBG:= Colors.TextSelBG;
      DoPartInsert(AParts, Part);
    end;
  end;
end;

procedure TATSynEdit.DoPartInsert(var AParts: TATLineParts; const APart: TATLinePart);
begin

end;

