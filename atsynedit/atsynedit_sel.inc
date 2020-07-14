{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  NPosLeft,
  NPosRight: integer;
begin
  if not IsSelRectEmpty then
  begin
    if not ((AY>=FSelRect.Top) and (AY<=FSelRect.Bottom)) then exit(False);

    NPosLeft:= Strings.ColumnPosToCharPos(AY, FSelRect.Left, FTabHelper);
    NPosRight:= Strings.ColumnPosToCharPos(AY, FSelRect.Right, FTabHelper);
    Result:= (AX>=NPosLeft) and (AX<NPosRight);
  end
  else
    Result:= TempSel_IsPosSelected(AX, AY);
end;


function TATSynEdit.IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
var
  NPosLeft,
  NPosRight: integer;
begin
  if not IsSelRectEmpty then
  begin
    //check for y
    if AY1<>AY2 then
      exit(cRangeAllUnselected); //dont allow multi-line range
    if (AY1<FSelRect.Top) or (AY1>FSelRect.Bottom) then
      exit(cRangeAllUnselected);

    //check for x
    NPosLeft:= Strings.ColumnPosToCharPos(AY1, FSelRect.Left, FTabHelper);
    NPosRight:= Strings.ColumnPosToCharPos(AY1, FSelRect.Right, FTabHelper);

    if (AX1>=NPosLeft) and (AX2<=NPosRight) then
      Result:= cRangeAllSelected
    else
    if (AX1>=NPosRight) or (AX2<=NPosLeft) then
      Result:= cRangeAllUnselected
    else
      Result:= cRangePartlySelected;
  end
  else
    Result:= TempSel_IsRangeSelected(AX1, AY1, AX2, AY2);
end;


function TATSynEdit.IsSelRectEmpty: boolean;
begin
  Result:= EqualRect(FSelRect, cRectEmpty);
end;

procedure TATSynEdit.DoSelect_CharGroupAtPos(P: TPoint; AddCaret, AllowOnlyWordChars: boolean);
var
  XFrom, XTo: integer;
  ch: atChar;
  Str: atString;
begin
  if not Strings.IsIndexValid(P.Y) then Exit;

  Str:= Strings.Lines[P.Y];
  if Str='' then exit;

  if P.X<0 then Exit;
  //allow clicking after line end
  if P.X>=Length(Str) then
    P.X:= Length(Str)-1;

  ch:= Str[P.X+1];

  if IsCharWord(ch) then
    SFindWordBounds(Str, P.X, XFrom, XTo, FOptNonWordChars)
  else
  begin
    if AllowOnlyWordChars then
      Exit;
    if IsCharSymbol(ch) then
      SFindSymbolsBounds(Str, P.X, XFrom, XTo)
    else
    if IsCharSpace(ch) then
      SFindSpacesBounds(Str, P.X, XFrom, XTo)
    else
      exit;
  end;

  if XFrom=XTo then exit;

  if AddCaret then
  begin
    Carets.Add(XTo, P.Y, XFrom, P.Y);
    DoCaretsSort;
  end
  else
    DoCaretSingle(XTo, P.Y, XFrom, P.Y);
end;

procedure TATSynEdit.DoSelect_CharRange(ACaretIndex: integer; Pnt: TPoint);
begin
  if not Carets.IsIndexValid(ACaretIndex) then Exit;
  Carets[ACaretIndex].SelectToPoint(Pnt.X, Pnt.Y);
end;

procedure TATSynEdit.DoSelect_WordRange(ACaretIndex: integer; P1, P2: TPoint);
begin
  if not Carets.IsIndexValid(ACaretIndex) then Exit;
  if not Strings.IsIndexValid(P1.Y) then Exit;
  if not Strings.IsIndexValid(P2.Y) then Exit;

  if not IsPosSorted(P1.X, P1.Y, P2.X, P2.Y, true) then
  begin
    SwapInt(P1.X, P2.X);
    SwapInt(P1.Y, P2.Y);
  end;

  P1.X:= SFindWordOffset(Strings.Lines[P1.Y], P1.X, cWordjumpToPrev, false, FOptNonWordChars);
  P2.X:= SFindWordOffset(Strings.Lines[P2.Y], P2.X, cWordjumpToNext, false, FOptNonWordChars);

  with Carets[ACaretIndex] do
  begin
    PosX:= P2.X;
    PosY:= P2.Y;
    EndX:= P1.X;
    EndY:= P1.Y;
  end;
end;

procedure TATSynEdit.DoSelect_Line(APos: TPoint);
var
  PLast: TPoint;
  NPosX, NPosY, NEndX, NEndY: integer;
begin
  if not Strings.IsIndexValid(APos.Y) then Exit;

  if FOptMouseClickNumberSelectsLineWithEOL then
  begin
    if APos.Y<Strings.Count-1 then
    begin
      NPosX:= 0;
      NPosY:= APos.Y+1;
    end
    else
    begin
      PLast:= GetEndOfFilePos;
      NPosX:= PLast.X;
      NPosY:= PLast.Y;
    end;
  end
  else
  begin
    NPosX:= Strings.LinesLen[APos.Y];
    NPosY:= APos.Y;
  end;

  NEndX:= 0;
  NEndY:= APos.Y;

  DoCaretSingle(NPosX, NPosY, NEndX, NEndY);
end;

procedure TATSynEdit.DoSelect_All;
var
  P: TPoint;
begin
  P:= GetEndOfFilePos;
  DoCaretSingle(P.X, P.Y, 0, 0);
end;

procedure TATSynEdit.DoSelect_Inverted;
var
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  XPrev, YPrev: integer;
  i: integer;
  Sel: boolean;
  PosLast: TPoint;
begin
  XPrev:= 0;
  YPrev:= 0;
  NewCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Carets[i].GetRange(X1, Y1, X2, Y2, Sel);
      if not Sel then Continue;

      //add range
      if (X1<>XPrev) or (Y1<>YPrev) then
        NewCarets.Add(X1, Y1, XPrev, YPrev);
      XPrev:= X2;
      YPrev:= Y2;
    end;

    //add range after last caret
    PosLast:= GetEndOfFilePos;
    if (PosLast.X<>XPrev) or (PosLast.Y<>YPrev) then
      NewCarets.Add(PosLast.X, PosLast.Y, XPrev, YPrev);

    DoCaretsAssign(NewCarets);
  finally
    FreeAndNil(NewCarets);
  end;
end;

procedure TATSynEdit.DoSelect_SplitSelectionToLines;
var
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  i, j, NLen: integer;
  Sel: boolean;
begin
  NewCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Carets[i].GetRange(X1, Y1, X2, Y2, Sel);
      if not Sel then
      begin
        NewCarets.Add(X1, Y1);
        Continue;
      end;

      if Y1=Y2 then
      begin
        NewCarets.Add(X2, Y2, X1, Y1);
        Continue;
      end;

      //add first part
      NLen:= Strings.LinesLen[Y1];
      if X1<NLen then
        NewCarets.Add(NLen, Y1, X1, Y1)
      else
        NewCarets.Add(X1, Y1);

      //add middle parts
      for j:= Y1+1 to Y2-1 do
      begin
        NLen:= Strings.LinesLen[j];
        if NLen=0 then
          NewCarets.Add(0, j)
        else
          NewCarets.Add(NLen, j, 0, j);
      end;

      //add last part
      NewCarets.Add(X2, Y2, 0, Y2);
    end;

    DoCaretsAssign(NewCarets);
  finally
    FreeAndNil(NewCarets);
  end;
end;

procedure TATSynEdit.DoSelect_ExtendSelectionByLine;
var
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  i: integer;
  Sel: boolean;
  PosLast: TPoint;
begin
  NewCarets:= TATCarets.Create;
  try
    for i:= 0 to Carets.Count-1 do
    begin
      Carets[i].GetRange(X1, Y1, X2, Y2, Sel);
      if not Sel then
        begin X2:= X1; Y2:= Y1; end;

      X1:= 0; //select entire 1st line
      if Y2<Strings.Count-1 then
      begin
        //select till start of next ln
        X2:= 0;
        Y2:= Y2+1;
      end
      else
      begin
        //select till eof
        PosLast:= GetEndOfFilePos;
        X2:= PosLast.X;
        Y2:= PosLast.Y;
      end;

      NewCarets.Add(X2, Y2, X1, Y1);
    end;

    DoCaretsAssign(NewCarets);
  finally
    FreeAndNil(NewCarets);
  end;
end;

procedure TATSynEdit.DoSelect_LineRange(ALineFrom: integer; APosTo: TPoint);
begin
  if APosTo.Y<ALineFrom then
  begin
    DoCaretSingle(
      APosTo.X,
      APosTo.Y,
      0,
      ALineFrom+1
      );
  end
  else
  if APosTo.Y>ALineFrom then
  begin
    DoCaretSingle(
      APosTo.X,
      APosTo.Y,
      0,
      ALineFrom
      );
  end
  else
  if APosTo.Y=ALineFrom then
  begin
    DoSelect_Line(APosTo);
  end;
end;

procedure TATSynEdit.DoSelect_ClearColumnBlock;
begin
  FSelRect:= cRectEmpty;
end;

procedure TATSynEdit.DoSelect_None;
var
  i: integer;
begin
  FSelRect:= cRectEmpty;

  for i:= 0 to Carets.Count-1 do
    with Carets[i] do
    begin
      EndX:= -1;
      EndY:= -1;
    end;
end;

procedure TATSynEdit.DoSelect_ColumnBlock_Primitive(P1, P2: TPoint);
var
  Coord1, Coord2: TPoint;
  LineCoord1, LineCoord2: TPoint;
  Details: TATPosDetails;
  Y, YMin, YMax: integer;
begin
  Coord1:= CaretPosToClientPos(P1);
  Coord2:= CaretPosToClientPos(P2);
  YMin:= Min(Coord1.Y, Coord2.Y);
  YMax:= Max(Coord1.Y, Coord2.Y);

  FSelRect:= cRectEmpty;
  Carets.Clear;

  Y:= YMin+1;
  repeat
    LineCoord1:= ClientPosToCaretPos(Point(Coord1.X, Y), Details);
    LineCoord2:= ClientPosToCaretPos(Point(Coord2.X, Y), Details);
    if (LineCoord1.Y>=0) and (LineCoord2.Y>=0) then
      Carets.Add(LineCoord2.X, LineCoord2.Y, LineCoord1.X, LineCoord1.Y);
    Inc(Y, FCharSize.Y);
  until Y-FCharSize.Y>YMax;

  //don't allow no carets
  if Carets.Count=0 then
    DoCaretSingle(P2.X, P2.Y);
end;

procedure TATSynEdit.DoSelect_ColumnBlock_FromPoints(P1, P2: TPoint);
var
  PTarget: TPoint;
begin
  if FOptCaretsPrimitiveColumnSelection then
  begin
    DoSelect_ColumnBlock_Primitive(P1, P2);
    exit;
  end;

  //consider tab-chars
  P1.X:= Strings.CharPosToColumnPos(P1.Y, P1.X, FTabHelper);
  P2.X:= Strings.CharPosToColumnPos(P2.Y, P2.X, FTabHelper);

  //fix coords
  PTarget:= P2;
  if P1.Y>P2.Y then
    SwapInt(P1.Y, P2.Y);

  //setting SelRect must be first, it's used later
  UpdateSelRectFromPoints(P1, P2);

  if FOptCaretsAddedToColumnSelection then
    DoCaretsAddOnColumnBlock(P1, P2, FSelRect)
  else
  begin
    //single caret at moving block edge
    PTarget.X:= Strings.ColumnPosToCharPos(PTarget.Y, PTarget.X, FTabHelper);
    DoCaretSingle(PTarget.X, PTarget.Y);
  end;
end;

procedure TATSynEdit.DoSelect_ColumnBlock_FromPointsColumns(P1, P2: TPoint);
//convert column-pos to char-pos
var
  PntBegin, PntEnd: TPoint;
begin
  if P1.Y<0 then exit;
  if P2.Y<0 then exit;

  P1.X:= Strings.ColumnPosToCharPos(P1.Y, P1.X, FTabHelper);
  P2.X:= Strings.ColumnPosToCharPos(P2.Y, P2.X, FTabHelper);

  DoSelect_ColumnBlock_FromPoints(P1, P2);
end;


procedure TATSynEdit.DoSelectionDeleteOrReset;
begin
  if FOptOverwriteSel then
    DoCommand_TextDeleteSelection
  else
    DoSelect_None;
end;

procedure TATSynEdit.DoSelect_NormalSelToColumnSel(out ABegin, AEnd: TPoint);
var
  Caret: TATCaretItem;
begin
  Caret:= Carets[0];
  if (Caret.EndY>=0) and (Caret.EndX>=0) then
  begin
    ABegin.X:= Strings.CharPosToColumnPos(Caret.EndY, Caret.EndX, FTabHelper);
    ABegin.Y:= Caret.EndY;

    AEnd.X:= Strings.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper);
    AEnd.Y:= Caret.PosY;
  end
  else
  begin
    ABegin.X:= Strings.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper);
    ABegin.Y:= Caret.PosY;
    AEnd:= ABegin;
  end;
end;

procedure TATSynEdit.DoSelectionDeleteColumnBlock;
var
  X1, X2, i: Integer;
  Str, StrNew: atString;
begin
  if IsSelRectEmpty then exit;

  Strings.BeginUndoGroup;
  try
    for i:= FSelRect.Top to FSelRect.Bottom do
    begin
      Str:= Strings.Lines[i];
      X1:= Strings.ColumnPosToCharPos(i, FSelRect.Left, FTabHelper);
      X2:= Strings.ColumnPosToCharPos(i, FSelRect.Right, FTabHelper);

      StrNew:= Str;
      Delete(StrNew, X1+1, X2-X1);
      if StrNew<>Str then
        Strings.Lines[i]:= StrNew;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  DoSelect_None;

  if Carets.Count>0 then
    Carets[0].PosX:= X1;
end;


procedure TATSynEdit.UpdateSelRectFromPoints(const P1, P2: TPoint);
begin
  FSelRect.Left:= Min(P1.X, P2.X);
  FSelRect.Top:= Min(P1.Y, P2.Y);
  FSelRect.Right:= Max(P1.X, P2.X);
  FSelRect.Bottom:= Max(P1.Y, P2.Y);
end;

function TATSynEdit.DoSelect_MultiCaretsLookLikeColumnSelection: boolean;
//carets are placed vertically,
//and all multi-selections are single line
var
  Caret0, Caret: TATCaretItem;
  Pnt: TPoint;
  bSel: boolean;
  i: integer;
begin
  Result:= false;
  if not FOptCaretsMultiToColumnSel then exit;
  if Carets.Count<2 then exit;

  Caret0:= Carets[0];
  bSel:= Caret0.EndY>=0;
  //check for single line selection
  if bSel and (Caret0.PosY<>Caret0.EndY) then exit;
  //remember column x
  Pnt.X:= Strings.CharPosToColumnPos(Caret0.PosY, Caret0.PosX, FTabHelper);
  Pnt.Y:= Caret0.PosY;

  for i:= 1{>0} to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    //check that carets placed vertically (y)
    if Caret.PosY<>Pnt.Y+i then exit;
    //check for single line selections
    if bSel and (Caret.PosY<>Caret.EndY) then exit;
    //check for vertically (x)
    if Pnt.X<>Strings.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper) then exit;
  end;

  Result:= true;
end;


procedure TATSynEdit.DebugSelRect;
begin
  Application.MainForm.Caption:= Format('selrect %d:%d-%d:%d, begin %d:%d, end %d:%d', [
    FSelRect.Left,
    FSelRect.Top,
    FSelRect.Right,
    FSelRect.Bottom,
    FSelRectBegin.X,
    FSelRectBegin.Y,
    FSelRectEnd.X,
    FSelRectEnd.Y
    ]);
end;

function TATSynEdit.TempSel_IsSelection: boolean;
begin
  Result:= Length(FSel)>0;
end;

function TATSynEdit.TempSel_IsMultiline: boolean;
var
  i: integer;
begin
  for i:= 0 to High(FSel) do
    if FSel[i].EndY<>FSel[i].PosY then
      exit(true);
  Result:= false;
end;

function TATSynEdit.TempSel_IsLineWithSelection(ALine: integer): boolean;
var
  Y1, Y2, X2: integer;
  a, b, m: integer;
begin
  Result:= false;
  a:= 0;
  b:= High(FSel);

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    Y1:= FSel[m].PosY;
    X2:= FSel[m].EndX;
    Y2:= FSel[m].EndY;

    if (X2=0) and (Y2>0) then
      Dec(Y2);

    if ALine<Y1 then
      b:= m-1
    else
    if ALine<=Y2 then
      exit(true)
    else
      a:= m+1;
  until false;
end;

function TATSynEdit.TempSel_IsLineAllSelected(ALine: integer): boolean;
var
  X1, Y1, Y2, X2: integer;
  NLen: integer;
  a, b, m: integer;
begin
  Result:= false;
  a:= 0;
  b:= High(FSel);

  NLen:= Strings.LinesLen[ALine];

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    X1:= FSel[m].PosX;
    Y1:= FSel[m].PosY;
    X2:= FSel[m].EndX;
    Y2:= FSel[m].EndY;

    if not IsPosSorted(X1, Y1, 0, ALine, true) then
      b:= m-1
    else
    if IsPosSorted(NLen, ALine, X2, Y2, true) then
      exit(true)
    else
      a:= m+1;
  until false;
end;


function TATSynEdit.TempSel_IsPosSelected(AX, AY: integer): boolean;
var
  X1, Y1, X2, Y2: integer;
  a, b, m: integer;
begin
  Result:= false;
  a:= 0;
  b:= High(FSel);

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    X1:= FSel[m].PosX;
    Y1:= FSel[m].PosY;
    X2:= FSel[m].EndX;
    Y2:= FSel[m].EndY;

    case IsPosInRange(AX, AY, X1, Y1, X2, Y2) of
      cRelateInside:
        exit(true);
      cRelateBefore:
        b:= m-1;
      cRelateAfter:
        a:= m+1;
    end;
  until false;
end;

function TATSynEdit.TempSel_IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
var
  X1, Y1, X2, Y2: integer;
  a, b, m: integer;
  bLeft, bRight: TATPosRelation;
begin
  Result:= cRangeAllUnselected;
  a:= 0;
  b:= High(FSel);

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    X1:= FSel[m].PosX;
    Y1:= FSel[m].PosY;
    X2:= FSel[m].EndX;
    Y2:= FSel[m].EndY;

    bLeft:= IsPosInRange(AX1, AY1, X1, Y1, X2, Y2);
    if (bLeft=cRelateAfter) then
    begin
      a:= m+1;
      Continue;
    end;

    bRight:= IsPosInRange(AX2, AY2, X1, Y1, X2, Y2, true);
    if (bRight=cRelateBefore) then
    begin
      b:= m-1;
      Continue;
    end;

    if (bLeft=cRelateInside) and (bRight=cRelateInside) then
      exit(cRangeAllSelected)
    else
      exit(cRangePartlySelected);
  until false;
end;

procedure TATSynEdit.TempSel_GetRangesInLineAfterPoint(AX, AY: integer; out
  ARanges: TATSimpleRangeArray);
var
  X1, Y1, X2, Y2, XFrom, XTo: integer;
  i: integer;
begin
  SetLength(ARanges, 0);
  for i:= 0 to High(FSel) do
  begin
    X1:= FSel[i].PosX;
    Y1:= FSel[i].PosY;
    X2:= FSel[i].EndX;
    Y2:= FSel[i].EndY;

    if (Y1>AY) then Break; //caret is fully after line AY: stop
    if (Y2<AY) then Continue; //caret is fully before line AY

    if (Y1<AY) then XFrom:= 0 else XFrom:= X1;
    if (Y2>AY) then XTo:= MaxInt else XTo:= X2;

    if XTo<=AX then Continue;
    if XFrom<AX then XFrom:= AX;

    SetLength(ARanges, Length(ARanges)+1);
    with ARanges[High(ARanges)] do
    begin
      NFrom:= XFrom;
      NTo:= XTo;
    end;
  end;
end;

