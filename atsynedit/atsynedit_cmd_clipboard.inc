function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret: boolean): TATCommandResults;
var
  Str: atString;
begin
  Str:= UTF8Decode(Clipboard.AsText);
  Result:= DoCommand_TextInsertAtCarets(Str, AKeepCaret, FOver and FUseOverOnPaste);
end;

