{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_ToggleOverwrite: TATCommandResults;
begin
  ModeOverwrite:= not ModeOverwrite;
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

