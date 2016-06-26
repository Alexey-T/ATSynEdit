{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_ToggleOverwrite: TATCommandResults;
begin
  ModeOverwrite:= not ModeOverwrite;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleWordWrap: TATCommandResults;
begin
  if FWrapMode=cWrapOff then
    FWrapMode:= cWrapOn
  else
    FWrapMode:= cWrapOff;
  FWrapUpdateNeeded:= true;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprinted: TATCommandResults;
begin
  OptUnprintedVisible:= not OptUnprintedVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprintedSpaces: TATCommandResults;
begin
  OptUnprintedSpaces:= not OptUnprintedSpaces;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprintedEnds: TATCommandResults;
begin
  OptUnprintedEnds:= not OptUnprintedEnds;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprintedEndDetails: TATCommandResults;
begin
  OptUnprintedEndsDetails:= not OptUnprintedEndsDetails;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleLineNums: TATCommandResults;
begin
  with Gutter.Items[GutterBandNum] do
    Visible:= not Visible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleFolding: TATCommandResults;
begin
  with Gutter.Items[GutterBandFold] do
    Visible:= not Visible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleRuler: TATCommandResults;
begin
  OptRulerVisible:= not OptRulerVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleMiniMap: TATCommandResults;
begin
  OptMinimapVisible:= not OptMinimapVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleMicroMap: TATCommandResults;
begin
  OptMicromapVisible:= not OptMicromapVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleReadOnly: TATCommandResults;
begin
  ModeReadOnly:= not ModeReadOnly;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_CaretsExtend(ADown: boolean; ALines: integer): TATCommandResults;
begin
  DoCaretsExtend(ADown, ALines);
  Result:= [cResultCaretAny];
end;

function TATSynEdit.DoCommand_SizeChange(AIncrease: boolean): TATCommandResults;
begin
  DoSizeChange(AIncrease);
  Result:= [cResultState];
end;


function TATSynEdit.DoCommand_FoldUnfoldAll(ADoFold: boolean): TATCommandResults;
var
  Ar: TATIntArray;
  R: TATSynRange;
  i: integer;
begin
  if ADoFold then
  begin
    Ar:= Fold.FindRangesContainingLines(
      0, Strings.Count-1, nil,
      false{OnlyFolded}, true{TopLevelOnly}, cRngHasAnyOfLines);
    for i:= Low(Ar) to High(Ar) do
    begin
      R:= Fold.Items[Ar[i]];
      if not R.Folded then
        DoRangeFold(R);
    end;
  end
  else
  begin
    for i:= 0 to Fold.Count-1 do
    begin
      R:= Fold.Items[i];
      if R.Folded then
        DoRangeUnfold(R);
    end;
  end;

  Result:= [cResultCaretAny, cResultScroll];
end;


function TATSynEdit.DoCommand_FoldLevel(ALevel: integer): TATCommandResults;
begin
  DoFoldForLevel(ALevel);
  Result:= [cResultCaretAny, cResultScroll];
end;


