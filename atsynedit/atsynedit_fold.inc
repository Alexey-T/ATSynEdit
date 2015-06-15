{$ifdef nn}begin end;{$endif}

function TATSynEdit.IsLineFolded(ALine: integer; ADetectPartialFold: boolean): boolean;
var
  Flag: integer;
begin
  Flag:= Strings.LinesHidden[ALine, FEditorIndex];
  Result:= (Flag=-1) or (ADetectPartialFold and (Flag>0));
end;

function TATSynEdit.IsLineFoldedFull(ALine: integer): boolean;
begin
  Result:= IsLineFolded(ALine, false);
end;

function TATSynEdit.GetFirstUnfoldedLineNumber: integer;
begin
  Result:= GetNextUnfoldedLineNumber(0, true);
end;

function TATSynEdit.GetLastUnfoldedLineNumber: integer;
begin
  Result:= GetNextUnfoldedLineNumber(Strings.Count-1, false);
end;

function TATSynEdit.GetNextUnfoldedLineNumber(ALine: integer; ADown: boolean): integer;
var
  N: integer;
begin
  Result:= ALine;
  N:= Result;
  while IsLineFolded(N) and Strings.IsIndexValid(N) do
    N:= N+BoolToPlusMinusOne(ADown);
  if Strings.IsIndexValid(N) then Result:= N;
end;

function TATSynEdit.IsPosFolded(AX, AY: integer): boolean;
begin
  Result:= Strings.IsPosFolded(AX, AY, FEditorIndex);
end;

(*
example of CPP file which is hard to unfold (if nested ranges folded).
 {
 d1
   {
   d2a
   }
   {
   d2b
     {
     d3a
     }
     {
     d3b
       {
       d4a
       }
       {
       d4b
       }
     }
   }
 }

what we do. for each line in range:
a) if line not in any subrange, show it
b) for all subranges at top level:
  b1) if subrange marked folded, unfold 1st line "[...]"
  b2) if subrange marked unfolded, recursion
*)
procedure TATSynEdit.DoFold_RangeUnfold(ARange: TATSynRange);
var
  List: TATIntArray;
  R: TATSynRange;
  i, j: integer;
  InSubrange: boolean;
begin
  ARange.Folded:= false;
  FWrapUpdateNeeded:= true;

  List:= FFold.FindRangesContainingLines(-1, -1, ARange,
    false{OnlyFolded}, true{TopLevel}, cRngIgnore);

  //show all lines not in subranges
  for i:= ARange.Y to ARange.Y2 do
  begin
    InSubrange:= false;
    for j:= Low(List) to High(List) do
      if FFold[List[j]].IsLineInside(i) then
      begin
        InSubrange:= true;
        Break
      end;

    if not InSubrange then
      Strings.LinesHidden[i, FEditorIndex]:= 0;
  end;

  //unfold subranges, resursion
  for i:= Low(List) to High(List) do
  begin
    R:= FFold[List[i]];
    if R.Folded then
      Strings.LinesHidden[R.Y, FEditorIndex]:= R.X
    else
      DoFold_RangeUnfold(R);
  end;
end;

procedure TATSynEdit.DoFold_RangeFold(ARange: TATSynRange);
var
  i: integer;
begin
  ARange.Folded:= true;
  FWrapUpdateNeeded:= true;

  //partially fold 1st line
  if ARange.Hint<>'' then
  begin
    Strings.LinesHidden[ARange.Y, FEditorIndex]:= ARange.X;
  end
  else
  case FFoldStyle of
    cFoldHereWithDots:
    begin
      Strings.LinesHidden[ARange.Y, FEditorIndex]:= ARange.X;
    end;
    cFoldHereWithTruncatedText:
    begin
      Strings.LinesHidden[ARange.Y, FEditorIndex]:= ARange.X;
      ARange.Hint:= Copy(Strings.Lines[ARange.Y], ARange.X+1, cFoldedLenOfEmptyHint)+'...';
    end;
    cFoldFromEndOfLine:
    begin
      Strings.LinesHidden[ARange.Y, FEditorIndex]:= Length(Strings.Lines[ARange.Y])+1;
    end;
    cFoldFromNextLine:
    begin
      //don't fold line
    end;
  end;

  //fully fold next lines
  for i:= ARange.Y+1 to ARange.Y2 do
    Strings.LinesHidden[i, FEditorIndex]:= -1;
end;


procedure TATSynEdit.DoUnfoldLine(ALine: integer);
var
  List: TATIntArray;
  i: integer;
begin
  List:= FFold.FindRangesContainingLines(ALine, ALine, nil,
    true{OnlyFolded}, false{TopLevelOnly}, cRngHasAllLines);
  for i:= Low(List) to High(List) do
    DoFold_RangeUnfold(FFold[List[i]]);
end;

procedure TATSynEdit.DoFold_ClickFoldingBar(ALine: integer);
var
  R: TATSynRange;
begin
  R:= FFold.FindRangeWithPlusAtLine(ALine);
  if Assigned(R) then
  begin
    if R.Folded then
      DoFold_RangeUnfold(R)
    else
      DoFold_RangeFold(R);
    Update;
  end;
end;


function TATSynEdit.GetFoldedMarkText(ALine: integer): string;
var
  R: TATSynRange;
begin
  Result:= '';
  R:= FFold.FindRangeWithPlusAtLine(ALine);
  if Assigned(R) then
    Result:= R.Hint;
  if Result='' then
    Result:= '...';
end;

procedure TATSynEdit.UpdateFoldedFromLinesHidden;
var
  i, j: integer;
  N: integer;
  R: TATSynRange;
begin
  for i:= 0 to Strings.Count-1 do
  begin
    N:= Strings.LinesHidden[i, FEditorIndex];
    if N<=0 then Continue;

    for j:= 0 to Fold.Count-1 do
    begin
      R:= Fold.Items[j];
      if (R.Y>i) then Break;
      if (R.Y=i) and (R.X=N) then
      begin
        R.Folded:= true;
        Break
      end;
    end;
  end;
end;

function TATSynEdit.IsFoldLineNeededBeforeWrapitem(N: integer): boolean;
var
  NLineCur, NLinePrev: integer;
begin
  if FWrapInfo.IsIndexValid(N) and (N>0) then
  begin
    NLineCur:= FWrapInfo.Items[N].NLineIndex;
    NLinePrev:= FWrapInfo.Items[N-1].NLineIndex;
    //before this line some is skipped
    Result:= NLineCur-NLinePrev > 1;
    //and prev visible line is fully visible
    if Result then
      Result:= Strings.LinesHidden[NLinePrev, FEditorIndex]=0;
  end
  else
    Result:= false;
end;


