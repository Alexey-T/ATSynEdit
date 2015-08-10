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

function TATSynEdit.DoCommand_ToggleMinimap: TATCommandResults;
begin
  OptMinimapVisible:= not OptMinimapVisible;
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


