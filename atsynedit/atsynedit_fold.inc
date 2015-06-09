{$ifdef nn}begin end;{$endif}

function TATSynEdit.IsLineFolded(ALine: integer; ADetectPartialFold: boolean): boolean;
var
  Flag: integer;
begin
  Flag:= Strings.LinesHidden[ALine];
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
  Result:= Strings.IsPosFolded(AX, AY);
end;


procedure TATSynEdit.DoFold_SetRangeFolding(ARange: TATSynRange; AFolded: boolean);
var
  List: TATIntArray;
  R: TATSynRange;
  i, j: integer;
begin
  ARange.Folded:= AFolded;
  if AFolded then
  begin
    Strings.LinesHidden[ARange.Y]:= ARange.X;
    for i:= ARange.Y+1 to ARange.Y2 do
      Strings.LinesHidden[i]:= -1;
  end
  else
  begin
    //unfold is hard. need to unfold only those lines
    //which aren't inside other folded ranges
    for i:= ARange.Y to ARange.Y2 do
    begin
      //find other folded ranges
      List:= FFold.FindRangesContainingLines(i, i, ARange, true{OnlyFolded}, true{TopLevel});

      //line not in other ranges? unfold
      if Length(List)=0 then
      begin
        Strings.LinesHidden[i]:= 0;
        Continue;
      end;

      //line in range: unfold only 1st line of range
      for j:= Low(List) to High(List) do
      begin
        R:= FFold[List[j]];
        Strings.LinesHidden[R.Y]:= R.X;
      end;
    end;
  end;

  FWrapUpdateNeeded:= true;
end;

procedure TATSynEdit.DoUnfoldLine(ALine: integer);
var
  List: TATIntArray;
  i: integer;
begin
  List:= FFold.FindRangesContainingLines(ALine, ALine, nil, true, false);
  for i:= Low(List) to High(List) do
    DoFold_SetRangeFolding(FFold[List[i]], false);
end;

procedure TATSynEdit.DoFold_ClickFoldingBar(ALine: integer);
var
  R: TATSynRange;
begin
  R:= FFold.FindRangeWithPlusAtLine(ALine);
  if Assigned(R) then
  begin
    DoFold_SetRangeFolding(R, not R.Folded);
    Update;
  end;
end;

