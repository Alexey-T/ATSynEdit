{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  NPosLeft,
  NPosRight: integer;
  SLine: atString;
begin
  if not IsSelRectEmpty then
  begin
    if not ((AY>=FSelRect.Top) and (AY<=FSelRect.Bottom)) then exit(False);

    //optimized for huge lines
    SLine:= Strings.LineSub(AY, 1, FSelRect.Right);
    NPosLeft:= SColumnPosToCharPos(SLine, FSelRect.Left, OptTabSize);
    NPosRight:= SColumnPosToCharPos(SLine, FSelRect.Right, OptTabSize);
    Result:= (AX>=NPosLeft) and (AX<NPosRight);
  end
  else
    Result:= Carets.IsPosSelected(AX, AY);
end;


function TATSynEdit.IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
var
  NPosLeft,
  NPosRight: integer;
  SLine: atString;
begin
  if not IsSelRectEmpty then
  begin
    //check for y
    if AY1<>AY2 then
      exit(cRangeAllUnselected); //dont allow multi-line range
    if (AY1<FSelRect.Top) or (AY1>FSelRect.Bottom) then
      exit(cRangeAllUnselected);

    //check for x
    SLine:= Strings.LineSub(AY1, 1, FSelRect.Right);
    NPosLeft:= SColumnPosToCharPos(SLine, FSelRect.Left, OptTabSize);
    NPosRight:= SColumnPosToCharPos(SLine, FSelRect.Right, OptTabSize);

    if (AX1>=NPosLeft) and (AX2<=NPosRight) then
      Result:= cRangeAllSelected
    else
    if (AX1>=NPosRight) or (AX2<=NPosLeft) then
      Result:= cRangeAllUnselected
    else
      Result:= cRangePartlySelected;
  end
  else
    Result:= Carets.IsRangeSelected(AX1, AY1, AX2, AY2);
end;


function TATSynEdit.IsSelRectEmpty: boolean;
begin
  Result:= EqualRect(FSelRect, cRectEmpty);
end;

procedure TATSynEdit.DoSelect_Word(P: TPoint);
var
  N1, N2: integer;
begin
  if not Strings.IsIndexValid(P.Y) then Exit;
  SFindWordBounds(Strings.Lines[P.Y], P.X, N1, N2, FOptWordChars);
  if N1<>N2 then
    DoCaretSingle(N2, P.Y, N1, P.Y);
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

  P1.X:= SFindWordOffset(Strings.Lines[P1.Y], P1.X, cWordjumpToPrev, false, FOptWordChars);
  P2.X:= SFindWordOffset(Strings.Lines[P2.Y], P2.X, cWordjumpToNext, false, FOptWordChars);

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
      NewCarets.Add(X1, Y1, XPrev, YPrev);
      XPrev:= X2;
      YPrev:= Y2;
    end;

    //add range after last caret
    PosLast:= GetEndOfFilePos;
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

procedure TATSynEdit.DoSelect_None;
var
  i: integer;
begin
  FSelRect:= cRectEmpty;
  FSelRectBegin:= Point(-1, -1);

  for i:= 0 to Carets.Count-1 do
    with Carets[i] do
    begin
      EndX:= -1;
      EndY:= -1;
    end;
end;

procedure TATSynEdit.DoSelect_ColumnBlock(P1, P2: TPoint);
var
  PTarget: TPoint;
  SLine: atString;
begin
  //consider tab-chars
  SLine:= Strings.LineSub(P1.Y, 1, P1.X);
  P1.X:= SCharPosToColumnPos(SLine, P1.X, FTabSize);

  SLine:= Strings.LineSub(P2.Y, 1, P2.X);
  P2.X:= SCharPosToColumnPos(SLine, P2.X, FTabSize);

  //fix coords
  PTarget:= P2;
  if P1.Y>P2.Y then
    SwapInt(P1.Y, P2.Y);

  //setting SelRect must be first, it's used later
  FSelRect.Left:= Min(P1.X, P2.X);
  FSelRect.Right:= Max(P1.X, P2.X);
  FSelRect.Top:= P1.Y;
  FSelRect.Bottom:= P2.Y;

  if FOptCaretsAddedToColumnSelection then
    DoCaretsAddOnColumnBlock(P1, P2, FSelRect)
  else
  begin
    //single caret at moving block edge
    SLine:= Strings.LineSub(PTarget.Y, 1, PTarget.X);
    PTarget.X:= SColumnPosToCharPos(SLine, PTarget.X, OptTabSize);
    DoCaretSingle(PTarget.X, PTarget.Y);
  end;
end;

procedure TATSynEdit.DoSelect_ColumnBlock_FromSelRect;
var
  PntBegin, PntEnd: TPoint;
  StrTemp: atString;
begin
  //convert SelRect to char-pos
  PntBegin:= FSelRectBegin;
  StrTemp:= Strings.LineSub(PntBegin.Y, 1, PntBegin.X);
  PntBegin.X:= SColumnPosToCharPos(StrTemp, PntBegin.X, OptTabSize);

  PntEnd:= FSelRectEnd;
  StrTemp:= Strings.LineSub(PntEnd.Y, 1, PntEnd.X);
  PntEnd.X:= SColumnPosToCharPos(StrTemp, PntEnd.X, OptTabSize);

  //func needs char-pos
  DoSelect_ColumnBlock(PntBegin, PntEnd);
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
  SLine: atString;
begin
  Caret:= Carets[0];
  if (Caret.EndY>=0) and (Caret.EndX>=0) then
  begin
    SLine:= Strings.LineSub(Caret.EndY, 1, Caret.EndX);
    ABegin.X:= SCharPosToColumnPos(SLine, Caret.EndX, OptTabSize);
    ABegin.Y:= Caret.EndY;

    SLine:= Strings.LineSub(Caret.PosY, 1, Caret.PosX);
    AEnd.X:= SCharPosToColumnPos(SLine, Caret.PosX, OptTabSize);
    AEnd.Y:= Caret.PosY;
  end
  else
  begin
    SLine:= Strings.LineSub(Caret.PosY, 1, Caret.PosX);
    ABegin.X:= SCharPosToColumnPos(SLine, Caret.PosX, OptTabSize);
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
      X1:= SColumnPosToCharPos(Str, FSelRect.Left, OptTabSize);
      X2:= SColumnPosToCharPos(Str, FSelRect.Right, OptTabSize);

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

