{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  i: integer;
  Item: TATSynCaretItem;
  bEnded: boolean;
begin
  Result:= false;
  for i:= 0 to Carets.Count-1 do
  begin
    Item:= Carets[i];

    if (Item.EndY>=0) then
      bEnded:= Min(Item.PosY, Item.EndY)>AY
    else
      bEnded:= Item.PosY>AY;
    if bEnded then Exit;

    if (AY>=Item.PosY) and (AY<=Item.EndY) and
      (AX>=Item.PosX) and (AX<Item.EndX) then
      begin Result:= true; Break end;
  end;
end;


procedure TATSynEdit.DoCalcLineHilite(const AItem: TATSynWrapItem;
  var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor);
var
  nIndex, i: integer;
  bSel, bSelPrev, bAdd: boolean;
begin
  FillChar(AParts, SizeOf(AParts), 0);

  bSel:= false;
  bSelPrev:= false;
  nIndex:= -1;

  //we have string for this call not only part of Strings.Lines[i] but
  //with StringOfChar(' ', NIndent) added
  for i:= 0 to Min(ACharsMax, AItem.NLength-1+AItem.NIndent-ACharsSkipped) do
  begin
    if i<AItem.NIndent-ACharsSkipped then
      bSel:= false
    else
      bSel:= IsPosSelected(AItem.NCharIndex-1-AItem.NIndent+i+ACharsSkipped, AItem.NLineIndex);

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
          Color:= FColorTextSel;//random($ffff);
          ColorBG:= FColorTextSelBG;
        end
        else
        begin
          Color:= FColorTextFont;//random($ffff);
          ColorBG:= AColorBG;
        end;
      end;
    end;
  end;
end;

