function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean): TATCommandResults;
var
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Str:= UTF8Decode(Clipboard.AsText);
  DoCommand_TextDeleteSelections;
  Result:= DoCommand_TextInsertAtCarets(Str, AKeepCaret, FOver and FUseOverOnPaste, ASelectThen);
end;

function TATSynEdit.DoCommand_ClipboardCut: TATCommandResults;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  if not Carets.IsSelection then Exit;

  DoCommand_ClipboardCopy;
  Result:= DoCommand_TextDeleteSelections;
end;

function TATSynEdit.DoCommand_ClipboardCopy: TATCommandResults;
var
  List: TList;
  ListStr, ListItem: TStringList;
  Item: TATCaretItem;
  i, X1, Y1, X2, Y2: integer;
  bSel: boolean;
  Str: atString;
begin
  List:= TList.Create;
  ListStr:= TStringList.Create;
  ListItem:= TStringList.Create;

  try
    for i:= 0 to Carets.Count-1 do
    begin
      Item:= Carets[i];
      if List.IndexOf(pointer(Item.PosY))<0 then
        List.Add(pointer(Item.PosY));
    end;

    //no selections-- copy entire lines
    if not Carets.IsSelection then
    begin
      if FTextCopyLinesIfNoSel then
        for i:= 0 to List.Count-1 do
        begin
          Str:= Strings.Lines[integer(List[i])];
          if Str<>'' then
            ListStr.Add(UTF8Encode(Str));
        end;
    end
    else
    //selection(s)-- copy selected ranges
    begin
      for i:= 0 to Carets.Count-1 do
      begin
        Item:= Carets[i];
        Item.GetRange(X1, Y1, X2, Y2, bSel);
        if not bSel then Continue;

        Str:= Strings.TextSubstring(X1, Y1, X2, Y2);
        ListItem.Text:= UTF8Encode(Str);
        ListStr.AddStrings(ListItem);
      end;
    end;

    SListTrim(ListStr);
    Clipboard.AsText:= ListStr.Text;

  finally
    FreeAndNil(ListItem);
    FreeAndNil(ListStr);
    FreeAndNil(List);
  end;

  Result:= [];
end;

