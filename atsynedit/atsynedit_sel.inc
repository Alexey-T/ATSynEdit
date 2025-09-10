{$ifdef nnn}begin end;{$endif}

function TATSynEdit.IsPosSelected(AX, AY: integer): boolean;
var
  St: TATStrings;
  NPosLeft,
  NPosRight: integer;
begin
  if not IsSelRectEmpty then
  begin
    if not ((AY>=FSelRect.Top) and (AY<=FSelRect.Bottom)) then exit(False);

    St:= Strings;
    NPosLeft:= St.ColumnPosToCharPos(AY, FSelRect.Left, FTabHelper);
    NPosRight:= St.ColumnPosToCharPos(AY, FSelRect.Right, FTabHelper);
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
      exit(TATRangeSelection.AllUnselected); //dont allow multi-line range
    if (AY1<FSelRect.Top) or (AY1>FSelRect.Bottom) then
      exit(TATRangeSelection.AllUnselected);

    //check for x
    NPosLeft:= Strings.ColumnPosToCharPos(AY1, FSelRect.Left, FTabHelper);
    NPosRight:= Strings.ColumnPosToCharPos(AY1, FSelRect.Right, FTabHelper);

    if (AX1>=NPosLeft) and (AX2<=NPosRight) then
      Result:= TATRangeSelection.AllSelected
    else
    if (AX1>=NPosRight) or (AX2<=NPosLeft) then
      Result:= TATRangeSelection.AllUnselected
    else
      Result:= TATRangeSelection.PartlySelected;
  end
  else
    Result:= TempSel_IsRangeSelected(AX1, AY1, AX2, AY2);
end;


function TATSynEdit.IsSelRectEmpty: boolean;
begin
  Result:= EqualRect(FSelRect, cRectEmpty);
end;

function TATSynEdit.IsSelColumn: boolean;
//it also gives True if 'new kind' of column block is set,
//which is activated by OptCaretsPrimitiveColumnSelection
begin
  Result:= not EqualRect(FSelRect, cRectEmpty) or
    (FSelRectBegin.Y>=0);
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
    Carets.Sort;
  end
  else
  begin
    DoCaretSingle(XTo, P.Y, XFrom, P.Y);
    with Carets[0] do
    begin
      DoubleClickRange.XFrom:= XFrom;
      DoubleClickRange.XTo:= XTo;
      DoubleClickRange.Y:= P.Y;
    end;
  end;
end;

procedure TATSynEdit.DoSelect_WordRange(ACaretIndex: integer; P1, P2: TPoint);
var
  St: TATStrings;
  bBackSelection: boolean;
begin
  St:= Strings;
  if not Carets.IsIndexValid(ACaretIndex) then Exit;
  if not St.IsIndexValid(P1.Y) then Exit;
  if not St.IsIndexValid(P2.Y) then Exit;

  bBackSelection:= IsPosSorted(P1.X, P1.Y, P2.X, P2.Y, true);
  if not bBackSelection then
  begin
    SwapInt(P1.X, P2.X);
    SwapInt(P1.Y, P2.Y);
  end;

  P1.X:= SFindWordOffset(St.Lines[P1.Y], P1.X, TATWordJump.PrevByMouse, false, FOptNonWordChars);
  P2.X:= SFindWordOffset(St.Lines[P2.Y], P2.X, TATWordJump.NextByMouse, false, FOptNonWordChars);

  if bBackSelection then
    Carets[ACaretIndex].Change(P1.X, P1.Y, P2.X, P2.Y)
  else
    Carets[ACaretIndex].Change(P2.X, P2.Y, P1.X, P1.Y);
end;

procedure TATSynEdit.DoSelect_Line(APos: TPoint);
var
  St: TATStrings;
  PLast: TPoint;
  NPosX, NPosY, NEndX, NEndY: integer;
begin
  St:= Strings;
  if not St.IsIndexValid(APos.Y) then Exit;

  if FOptMouseClickNumberSelectsLineWithEOL then
  begin
    if APos.Y<St.Count-1 then
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
    NPosX:= St.LinesLen[APos.Y];
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
  St: TATStrings;
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  i, j, NLen: integer;
  Sel: boolean;
begin
  St:= Strings;
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
      NLen:= St.LinesLen[Y1];
      if X1<NLen then
        NewCarets.Add(NLen, Y1, X1, Y1)
      else
        NewCarets.Add(X1, Y1);

      //add middle parts
      for j:= Y1+1 to Y2-1 do
      begin
        NLen:= St.LinesLen[j];
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

procedure TATSynEdit.DoSelect_ExtendSelectionByLine(AUp: boolean);
  //
  procedure GetPosDown(Y: integer; out ResX, ResY: integer);
  var
    NLastLine: integer;
    PosLast: TPoint;
  begin
    NLastLine:= Strings.Count-1;
    if Y<NLastLine then
    begin
      ResX:= 0;
      ResY:= Y+1;
    end
    else
    begin
      PosLast:= GetEndOfFilePos;
      ResX:= PosLast.X;
      ResY:= PosLast.Y;
    end;
  end;
  //
var
  St: TATStrings;
  Caret: TATCaretItem;
  NewCarets: TATCarets;
  X1, Y1, X2, Y2: integer;
  NewX, NewY, EndX, EndY: integer;
  bSel, bSelShrink, bWholeLinesSel: boolean;
  iCaret: integer;
begin
  St:= Strings;

  NewCarets:= TATCarets.Create;
  try
    for iCaret:= 0 to Carets.Count-1 do
    begin
      Caret:= Carets[iCaret];
      Caret.GetRange(X1, Y1, X2, Y2, bSel);
      if not bSel then
      begin
        X2:= X1;
        Y2:= Y1;
      end;

      bSelShrink:= bSel and (Caret.IsForwardSelection=AUp);
      bWholeLinesSel:= (X1=0) and ((X2=0) or (X2>=St.LinesLen[Y2]));

      if bSelShrink then
      begin
        //not _whole_ line(s) selected? select whole line(s), and don't shrink/extend more
        if not bWholeLinesSel then
        begin
          if not AUp then
          begin
            EndX:= 0;
            EndY:= Y1;
            GetPosDown(Y2, NewX, NewY);
          end
          else
          begin
            NewX:= 0;
            NewY:= Y1;
            if X2=0 then
            begin
              EndX:= 0;
              EndY:= Y2;
            end
            else
            begin
              GetPosDown(Y2, EndX, EndY);
            end;
          end;
        end
        else
        //shrink selection up
        if AUp then
        begin
          EndX:= X1;
          EndY:= Y1;
          NewX:= X2;
          NewY:= Max(0, Y2-1);
        end
        else
        //shrink selection down
        begin
          if Y1=Y2 then
            EndX:= 0
          else
            EndX:= X2;
          EndY:= Y2;
          GetPosDown(Y1, NewX, NewY);
        end;
      end
      else
      begin
        //no selection - select the whole line
        if not bSel then
        begin
          if not AUp then
          begin
            EndX:= 0;
            EndY:= Y1;
            GetPosDown(Y2, NewX, NewY);
          end
          else
          begin
            EndX:= 0;
            EndY:= Y2;
            NewX:= 0;
            NewY:= Max(0, Y1-1);
          end;
        end
        else
        //extend selection up
        if AUp then
        begin
          EndX:= X2;
          EndY:= Y2;
          if Y2<St.Count-1 then
            EndX:= 0;
          NewX:= 0;
          NewY:= Y1;
          if (NewY>0) and (X1=0) then
            Dec(NewY);
        end
        else
        //extend selection down
        begin
          EndX:= 0;
          EndY:= Y1;
          GetPosDown(Y2, NewX, NewY);
        end;
      end;

      if (EndX=NewX) and (EndY=NewY) then
      begin
        EndX:= -1;
        EndY:= -1;
      end;
      NewCarets.Add(NewX, NewY, EndX, EndY);
    end;

    DoCaretsAssign(NewCarets);
    Carets.Sort;
  finally
    FreeAndNil(NewCarets);
  end;
end;

procedure TATSynEdit.DoSelect_LineRange(ALineFrom: integer; APosTo: TPoint);
var
  NEndX, NEndY: integer;
begin
  if APosTo.Y<ALineFrom then
  begin
    if Strings.IsIndexValid(ALineFrom+1) then
    begin
      if FOptMouseClickNumberSelectsLineWithEOL then
      begin
        NEndX:= 0;
        NEndY:= ALineFrom+1;
      end
      else
      begin
        NEndX:= Strings.LinesLen[ALineFrom];
        NEndY:= ALineFrom;
      end;
    end
    else
    begin
      NEndX:= Strings.LinesLen[ALineFrom];
      NEndY:= ALineFrom;
    end;
    DoCaretSingle(
      APosTo.X,
      APosTo.Y,
      NEndX,
      NEndY
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
  //if APosTo.Y=ALineFrom then
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
  iCaret: integer;
begin
  FSelRect:= cRectEmpty;
  for iCaret:= 0 to Carets.Count-1 do
    Carets[iCaret].SelectNone;
end;

procedure TATSynEdit.DoSelect_ColumnBlock_Primitive(ACaretPos1, ACaretPos2: TPoint);
var
  Coord1, Coord2: TATPoint;
  LineCoord1, LineCoord2: TPoint;
  Details: TATEditorPosDetails;
  Y, YMin, YMax: Int64;
  NPosX, NPosY, NEndX, NEndY: integer;
begin
  Coord1:= CaretPosToClientPos(ACaretPos1);
  Coord2:= CaretPosToClientPos(ACaretPos2);
  YMin:= Min(Coord1.Y, Coord2.Y);
  YMax:= Max(Coord1.Y, Coord2.Y);

  FSelRect:= cRectEmpty;
  Carets.Clear;

  Y:= YMin+1;
  repeat
    LineCoord2:= ClientPosToCaretPos(ATPoint(Coord2.X, Y), Details);

    {
    ClientPosToCaretPos gives CudaText issue #5697. problem is visible like this:
    - file with very long lines + short lines
    - start column selection
    - do auto-scroll to the right, so short lines are totally scrolled out to the left
    - move mouse lower to expand block down
    - unpress the mouse
    -> those short lines have BAD selected parts
    }
    //LineCoord1:= ClientPosToCaretPos(ATPoint(Coord1.X, Y), Details);
    // workaround for the above:
    LineCoord1.X:= ACaretPos1.X;
    LineCoord1.Y:= LineCoord2.Y;

    NPosX:= LineCoord2.X;
    NPosY:= LineCoord2.Y;
    NEndX:= LineCoord1.X;
    NEndY:= LineCoord1.Y;
    //avoid empty selection
    if (NPosX=NEndX) and (NPosY=NEndY) then
    begin
      NEndX:= -1;
      NEndY:= -1;
    end;

    if (LineCoord1.Y>=0) and (LineCoord2.Y>=0) then
      Carets.Add(NPosX, NPosY, NEndX, NEndY, false{AWithEvent});
    Inc(Y, FCharSize.Y);
  until Y-FCharSize.Y>YMax;

  //don't allow no carets
  if Carets.Count=0 then
    DoCaretSingle(ACaretPos2.X, ACaretPos2.Y)
  else
    Carets.DoChanged;
end;

procedure TATSynEdit.DoSelect_ColumnBlock_FromPoints(APosChar1, APosChar2: TPoint;
  AUpdateSelRectPoints: boolean=true);
var
  St: TATStrings;
  //APosChar1/APosChar2 hold char-pos
  //Pos1Col/Pos2Col hold column-pos
  Pos1Col, Pos2Col: TPoint;
  PosTarget: TPoint;
begin
  St:= Strings;

  //exclude last empty line (CudaText issue #5794)
  if APosChar1.Y<>APosChar2.Y then
  begin
    if APosChar2.Y>=APosChar1.Y then
    begin
      if St.IsIndexValid(APosChar2.Y) and (St.LinesLen[APosChar2.Y]=0) then
        Dec(APosChar2.Y);
    end
    else
    begin
      if St.IsIndexValid(APosChar1.Y) and (St.LinesLen[APosChar1.Y]=0) then
        Dec(APosChar1.Y);
    end;
  end;

  Pos1Col:= APosChar1;
  Pos2Col:= APosChar2;
  Pos1Col.X:= St.CharPosToColumnPos(Pos1Col.Y, Pos1Col.X, FTabHelper);
  Pos2Col.X:= St.CharPosToColumnPos(Pos2Col.Y, Pos2Col.X, FTabHelper);

  if AUpdateSelRectPoints then
  begin
    FLastCommandMakesColumnSel:= true;
    FSelRectBegin:= Pos1Col;
    FSelRectEnd:= Pos2Col;
  end;

  if FOptCaretsPrimitiveColumnSelection then
  begin
    DoSelect_ColumnBlock_Primitive(APosChar1, APosChar2);
    UpdateSelRectFromPoints(Pos1Col, Pos2Col);
    //DebugSelRect; //debug
    exit;
  end;

  //setting SelRect must be first, it's used later
  UpdateSelRectFromPoints(Pos1Col, Pos2Col);

  //fix coords
  PosTarget:= Pos2Col;
  if Pos1Col.Y>Pos2Col.Y then
    SwapInt(Pos1Col.Y, Pos2Col.Y);

  if FOptCaretsAddedToColumnSelection then
    DoCaretsAddOnColumnBlock(Pos1Col, Pos2Col, FSelRect)
  else
  begin
    //single caret at moving block edge
    PosTarget.X:= St.ColumnPosToCharPos(PosTarget.Y, PosTarget.X, FTabHelper);
    DoCaretSingle(PosTarget.X, PosTarget.Y);
  end;

  //DebugSelRect; //debug
end;

procedure TATSynEdit.DoSelect_ColumnBlock_FromPointsColumns(P1, P2: TPoint);
//convert column-pos to char-pos
begin
  if P1.Y<0 then exit;
  if P2.Y<0 then exit;

  P1.X:= Strings.ColumnPosToCharPos(P1.Y, P1.X, FTabHelper);
  P2.X:= Strings.ColumnPosToCharPos(P2.Y, P2.X, FTabHelper);

  DoSelect_ColumnBlock_FromPoints(P1, P2, false{FSelRect* already set});
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
  St: TATStrings;
  Caret: TATCaretItem;
begin
  St:= Strings;
  Caret:= Carets[0];
  if St.IsIndexValid(Caret.EndY) and (Caret.EndX>=0) then
  begin
    ABegin.X:= St.CharPosToColumnPos(Caret.EndY, Caret.EndX, FTabHelper);
    ABegin.Y:= Caret.EndY;

    AEnd.X:= St.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper);
    AEnd.Y:= Caret.PosY;
  end
  else
  begin
    ABegin.X:= St.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper);
    ABegin.Y:= Caret.PosY;
    AEnd:= ABegin;
  end;
end;

procedure TATSynEdit.DoSelectionDeleteColumnBlock;
var
  St: TATStrings;
  X1, X2, i: Integer;
  Str, StrNew: atString;
begin
  if IsSelRectEmpty then exit;

  St:= Strings;
  St.BeginUndoGroup;
  try
    for i:= FSelRect.Top to FSelRect.Bottom do
    begin
      Str:= St.Lines[i];
      X1:= St.ColumnPosToCharPos(i, FSelRect.Left, FTabHelper);
      X2:= St.ColumnPosToCharPos(i, FSelRect.Right, FTabHelper);

      StrNew:= Str;
      Delete(StrNew, X1+1, X2-X1);
      if StrNew<>Str then
        St.Lines[i]:= StrNew;
    end;
  finally
    St.EndUndoGroup;
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
  St: TATStrings;
  Caret0, Caret: TATCaretItem;
  Pnt, PntEnd: TPoint;
  bSel: boolean;
  i: integer;
begin
  Result:= false;
  if not FOptCaretsMultiToColumnSel then exit;
  if Carets.Count<2 then exit;
  St:= Strings;

  Caret0:= Carets[0];
  bSel:= Caret0.EndY>=0;
  //check for single line selection
  if bSel and (Caret0.PosY<>Caret0.EndY) then exit;
  //remember column x
  Pnt.X:= St.CharPosToColumnPos(Caret0.PosY, Caret0.PosX, FTabHelper);
  Pnt.Y:= Caret0.PosY;

  Caret:= Carets[Carets.Count-1];
  PntEnd.X:= St.CharPosToColumnPos(Caret.PosY, Caret.EndX, FTabHelper);
  PntEnd.Y:= Caret.PosY;
  if PntEnd.X<0 then
    PntEnd.X:= Pnt.X;

  for i:= 1{>0} to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    //check that carets placed vertically (y)
    if Caret.PosY<>Pnt.Y+i then exit;
    //check for single line selections
    if bSel and (Caret.PosY<>Caret.EndY) then exit;
    //check for vertically (x)
    if Pnt.X<>St.CharPosToColumnPos(Caret.PosY, Caret.PosX, FTabHelper) then exit;
  end;

  //multi-carets look like column sel,
  //set the FSelRectBegin/FSelRectEnd
  FSelRectBegin.X:= Pnt.X;
  FSelRectBegin.Y:= Pnt.Y;
  FSelRectEnd.X:= PntEnd.X;
  FSelRectEnd.Y:= PntEnd.Y;

  Result:= true;
end;


procedure TATSynEdit.DoSelect_LinesByFoldedMark(ALineFrom, ALineTo: integer);
var
  St: TATStrings;
  Pnt: TPoint;
begin
  St:= Strings;
  if St.IsIndexValid(ALineTo+1) then
    Pnt:= Point(0, ALineTo+1)
  else
    Pnt:= Point(St.LinesLen[ALineTo], ALineTo);
  DoSelect_LineRange(ALineFrom, Pnt);
end;

function TATSynEdit.DoSelect_FoldingRangeStartingAtLine(
  ACaret: TATCaretItem; ALine: integer; ACaretToEndOfSel: boolean): boolean;
var
  St: TATStrings;
  RangePtr: PATFoldRange;
  NRange: integer;
  NLineBegin, NLineEnd: integer;
begin
  Result:= false;
  St:= Strings;
  NRange:= FFold.FindRangeWithPlusAtLine(ALine);
  if NRange<0 then exit;
  if not FFold.IsIndexValid(NRange) then exit;
  RangePtr:= FFold.ItemPtr(NRange);

  NLineBegin:= RangePtr^.Y;
  NLineEnd:= RangePtr^.Y2;
  if not St.IsIndexValid(NLineBegin) then exit;
  if not St.IsIndexValid(NLineEnd) then exit;

  if St.IsIndexValid(NLineEnd+1) then
    ACaret.Change(
      0,
      NLineBegin,
      0,
      NLineEnd+1
      )
  else
    ACaret.Change(
      0,
      NLineBegin,
      St.LinesLen[NLineEnd],
      NLineEnd
      );
  if ACaretToEndOfSel then
    ACaret.SwapSelection;
  Result:= true;
end;

function TATSynEdit.DoSelect_FoldingRangeAtCaret: boolean;
var
  Caret: TATCaretItem;
  NX1, NY1, NX2, NY2: integer;
  iCaret: integer;
  bSel: boolean;
begin
  Result:= false;
  for iCaret:= Carets.Count-1 downto 0 do
  begin
    Caret:= Carets[iCaret];
    Caret.GetRange(NX1, NY1, NX2, NY2, bSel);
    if bSel then Continue;
    if DoSelect_FoldingRangeStartingAtLine(
      Caret,
      NY1,
      false //false: better keep carets at original lines to avoid scrolling
      ) then
      Result:= true;
  end;
end;


procedure TATSynEdit.DoSelect_ColumnBlock_CopyToStringlist(const ASelRect: TRect; AList: TStringList);
var
  St: TATStrings;
  NBlockWidth, iLine: integer;
  S: UnicodeString;
begin
  AList.Clear;
  St:= Strings;
  NBlockWidth:= ASelRect.Width;
  for iLine:= ASelRect.Top to ASelRect.Bottom do
  begin
    if not St.IsIndexValid(iLine) then Break;
    S:= St.LineSub(iLine, ASelRect.Left+1, NBlockWidth);
    AList.Add(UTF8Encode(S));
  end;
end;

procedure TATSynEdit.DoSelect_ColumnBlock_InsertFromStringlist(ATextPos: TPoint; AList: TStringList);
var
  St: TATStrings;
  SLineOrig, SLineNew: UnicodeString;
  iLine, iBlockIndex: integer;
begin
  St:= Strings;
  iBlockIndex:= 0;
  for iLine:= ATextPos.Y to ATextPos.Y+AList.Count-1 do
  begin
    if not St.IsIndexValid(iLine) then Break;
    if iBlockIndex>=AList.Count then Break;
    SLineOrig:= St.Lines[iLine];
    SLineNew:= SLineOrig;
    Insert(UTF8Decode(AList[iBlockIndex]), SLineNew, ATextPos.X+1);
    if SLineOrig<>SLineNew then
      St.Lines[iLine]:= SLineNew;
    Inc(iBlockIndex);
  end;

end;

procedure TATSynEdit.DebugSelRect;
begin
  Application.MainForm.Caption:= Format('SelRect %d:%d-%d:%d, SelRectBegin %d:%d, SelRectEnd %d:%d', [
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
  Result:= not FSel.IsEmpty;
end;

function TATSynEdit.TempSel_IsMultiline: boolean;
begin
  Result:= FSel.IsMultiline;
end;

function TATSynEdit.TempSel_IsLineWithSelection(ALine: integer): boolean;
begin
  Result:= FSel.IsLineWithSelection(ALine);
end;

function TATSynEdit.TempSel_IsLineAllSelected(ALine: integer): boolean;
begin
  Result:= FSel.IsLineAllSelected(ALine, Strings.LinesLen[ALine]);
end;

function TATSynEdit.TempSel_IsPosSelected(AX, AY: integer): boolean;
begin
  Result:= FSel.IsPosSelected(AX, AY);
end;

function TATSynEdit.TempSel_IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
begin
  Result:= FSel.IsRangeSelected(AX1, AY1, AX2, AY2);
end;

procedure TATSynEdit.TempSel_GetRangesInLineAfterPoint(AX, AY: integer; out ARanges: TATSimpleRangeArray);
begin
  FSel.GetRangesInLineAfterPoint(AX, AY, ARanges);
end;

