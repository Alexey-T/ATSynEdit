function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret: boolean): TATCommandResults;
var
  Str: atString;
begin
  Str:= UTF8Decode(Clipboard.AsText);
  Result:= DoCommand_TextInsertAtCarets(Str, AKeepCaret, FOvrMode and FOvrModeApplyForPaste);
end;

