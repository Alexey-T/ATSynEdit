{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCalcLineHilite(const AItem: TATSynWrapItem;
  var AParts: TATLineParts; ACharsSkipped, ACharsMax: integer;
  AColorBG: TColor; var AColorAfterEol: TColor);
var
  nMaxOffset, nCharIndex, nLineIndex, nLineLen: integer;
begin
  nMaxOffset:= Min(ACharsMax, AItem.NLength-ACharsSkipped);
  nLineIndex:= AItem.NLineIndex;
  nLineLen:= AItem.NLength;
  nCharIndex:= AItem.NCharIndex+ACharsSkipped;

  FillChar(AParts, SizeOf(AParts), 0);
  if Assigned(FAdapterOfHilite) then
    FAdapterOfHilite.OnEditorCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorBG, AColorAfterEol);
  if Assigned(FOnCalcHilite) then
    FOnCalcHilite(Self, AParts, nLineIndex, nCharIndex, nLineLen, AColorBG, AColorAfterEol);

  //Createnew makes parts for selection and fills empty AParts with these parts.
  //Applyover makes parts for selection and inserts these one-by-one over ready AParts
  //calculated before (in adapter or OnCalc event).
  //Maybe possible to always use Applyover but it's slower so i made Createnew for
  //faster render w/out adapter
  if AParts[0].Len>0 then
    DoPartCalc_ApplyOver(AParts, nMaxOffset, nLineIndex, nCharIndex-1)
  else
    DoPartCalc_CreateNew(AParts, nMaxOffset, nLineIndex, nCharIndex-1, AColorBG);
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

procedure TATSynEdit.DoPartCalc_ApplyOver(var AParts: TATLineParts; AOffsetMax,
  ALineIndex, ACharIndex: integer);
var
  bSel, bSelPrev: boolean;
  Part: TATLinePart;
  i: integer;
begin
  FillChar(Part{%H-}, SizeOf(Part), 0);
  Part.ColorFont:= Colors.TextSelFont;
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


procedure TATSynEdit.DoCalcLineEmptyColor(ALine: integer; var AColor: TColor);
begin
  if Assigned(FAdapterOfHilite) then
    FAdapterOfHilite.OnEditorCalcEmptyColor(Self, ALine, AColor);
end;

