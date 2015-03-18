{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_ToggleOver: TATCommandResults;
begin
  ModeOver:= not ModeOver;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleReadOnly: TATCommandResults;
begin
  ModeReadOnly:= not ModeReadOnly;
  Result:= [cResultState];
end;


