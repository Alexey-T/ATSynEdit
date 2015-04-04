function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean): TATCommandResults;
var
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //column block
  if Clipboard.HasFormat(cClipFormatId) then
  begin
    Result:= DoCommand_ClipboardPasteColumnBlock(AKeepCaret);
    Exit
  end;

  //usual text
  Str:= UTF8Decode(Clipboard.AsText);
  DoCommand_TextDeleteSelection;
  Result:= DoCommand_TextInsertAtCarets(Str, AKeepCaret, FOver and FOptUseOverOnPaste, ASelectThen);

  //workaround: paste on last line, end
  if Carets.Count>0 then
    with Carets[0] do
      if PosY>=Strings.Count then
        Strings.LineAdd('');
end;


function TATSynEdit.DoCommand_ClipboardPasteColumnBlock(AKeepCaret: boolean): TATCommandResults;
var
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Str:= UTF8Decode(Clipboard.AsText);
  Result:= DoCommand_TextInsertColumnBlockOnce(Str, AKeepCaret);
end;


function TATSynEdit.DoCommand_ClipboardCut: TATCommandResults;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  if Carets.IsSelection then
  begin
    DoCommand_ClipboardCopy;
    Result:= DoCommand_TextDeleteSelection;
  end
  else
  begin
    if FOptCutLinesIfNoSel then
    begin
      DoCommand_ClipboardCopy;
      Result:= DoCommand_TextDeleteLines;
    end;
  end;
end;

function TATSynEdit.DoCommand_ClipboardCopy(Append: boolean): TATCommandResults;
begin
  if not EqualRect(FSelRect, cRectEmpty) then
  begin
    Clipboard.AsText:= GetTextForClipboard;
    Clipboard.AddFormat(cClipFormatId, cClipSignatureColumn, SizeOf(cClipSignatureColumn));
  end
  else
  begin
    if Append then
      Clipboard.AsText:= Clipboard.AsText+GetTextForClipboard
    else
      Clipboard.AsText:= GetTextForClipboard;
  end;

  Result:= [];
end;

function TATSynEdit.GetTextForClipboard: string;
var
  ListNum: TList;
  ListStr: TStringList;
  Item: TATCaretItem;
  i, NLen, X1, Y1, X2, Y2: integer;
  bSel: boolean;
  Str: atString;
begin
  Result:= '';

  if not EqualRect(FSelRect, cRectEmpty) then
  begin
    for i:= FSelRect.Top to FSelRect.Bottom do
    begin
      Str:= Strings.TextSubstring(FSelRect.Left, i, FSelRect.Right, i);
      NLen:= FSelRect.Right-FSelRect.Left - Length(Str);
      if NLen>0 then
        Str:= Str+StringOfChar(' ', NLen);
      Result:= Result+UTF8Encode(Str)+sLineBreak;
    end;
    Exit;
  end;

  ListNum:= TList.Create;
  ListStr:= TStringList.Create;

  try
    for i:= 0 to Carets.Count-1 do
    begin
      Item:= Carets[i];
      if ListNum.IndexOf(pointer{%H-}(Item.PosY))<0 then
        ListNum.Add(pointer{%H-}(Item.PosY));
    end;

    //no selections-- copy entire lines
    if not Carets.IsSelection then
    begin
      if FOptCopyLinesIfNoSel then
      begin
        for i:= 0 to ListNum.Count-1 do
        begin
          Str:= Strings.Lines[NativeInt{%H-}(ListNum[i])];
          if Str<>'' then
            ListStr.Add(UTF8Encode(Str));
        end;
        Result:= ListStr.Text; //always use Text, need eol
      end;
    end
    else
    //selections-- copy selected ranges
    begin
      for i:= 0 to Carets.Count-1 do
      begin
        Item:= Carets[i];
        Item.GetRange(X1, Y1, X2, Y2, bSel);
        if not bSel then Continue;

        Str:= Strings.TextSubstring(X1, Y1, X2, Y2);
        if Str<>'' then
          ListStr.Add(UTF8Encode(Str));
      end;

      if ListStr.Count=1 then
        Result:= ListStr[0] //don't use Text to skip eol
      else
        Result:= ListStr.Text;
    end;
  finally
    FreeAndNil(ListStr);
    FreeAndNil(ListNum);
  end;
end;

