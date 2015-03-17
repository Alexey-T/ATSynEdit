{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_ToggleOvr: TATCommandResults;
begin
  ModeOvr:= not ModeOvr;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleReadOnly: TATCommandResults;
begin
  ModeReadOnly:= not ModeReadOnly;
  Result:= [cResultState];
end;

