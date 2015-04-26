{$ifdef nn}begin end;{$endif}

function TATSynEdit.IsLineFolded(ALineNum: integer; ADetectPartiallyFolded: boolean): boolean;
var
  Flag: integer;
begin
  Flag:= Strings.LinesHidden[ALineNum];
  Result:= (Flag=-1) or (ADetectPartiallyFolded and (Flag>0));
end;

function TATSynEdit.IsLineFoldedFull(ALineNum: integer): boolean;
begin
  Result:= IsLineFolded(ALineNum, false);
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
  Result:= Strings.IsPosFolded(AX, AY);
end;


procedure TATSynEdit.DoFoldUnfoldLines(ALineFrom, ALineTo, AFoldFromCharIndex: integer;
  ADoFold: boolean);
var
  List: TATIntegerArray;
  R: TATSynRange;
  i, j: integer;
begin
  if ADoFold then
  begin
    Strings.LinesHidden[ALineFrom]:= AFoldFromCharIndex;
    for i:= ALineFrom+1 to ALineTo do
      Strings.LinesHidden[i]:= -1;
  end
  else
  begin
    //unfold is hard. need to unfold only those lines
    //which aren't inside other folded ranges (nested)
    for i:= ALineFrom to ALineTo do
    begin
      //find other folded ranges
      List:= FFoldList.FindRangesContainingLine(i, ALineFrom, ALineTo, true{OnlyFolded}, true{TopLevel});

      //line not in other ranges? unfold
      if Length(List)=0 then
      begin
        Strings.LinesHidden[i]:= 0;
        Continue;
      end;

      //line in range: unfold only 1st line of range
      for j:= Low(List) to High(List) do
      begin
        R:= FFoldList[List[j]];
        Strings.LinesHidden[R.Y]:= R.X;
      end;
    end;
  end;

  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.DoUnfoldLine(ALineNum: integer);
begin
  DoFoldUnfoldLines(ALineNum, ALineNum, 0, false);
end;

procedure TATSynEdit.DoFold_ClickFoldingBar(ALineNum: integer);
var
  List: TATIntegerArray;
  Range: TATSynRange;
  i: integer;
begin
  List:= FFoldList.FindRangesContainingLine(ALineNum);
  for i:= 0 to High(List) do
  begin
    Range:= FFoldList[List[i]];
    if (not Range.IsSimple) and (Range.Y=ALineNum) then
    begin
      DoFold_ToggleFoldRange(Range);
      Break
    end;
  end;
end;

procedure TATSynEdit.DoFold_ToggleFoldRange(R: TATSynRange);
begin
  R.Folded:= not R.Folded;
  DoFoldUnfoldLines(R.Y, R.Y2, R.X, R.Folded);
  Update;
end;


