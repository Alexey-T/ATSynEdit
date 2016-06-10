{$ifdef nn}begin end;{$endif}

function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean): TATCommandResults;
var
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //column block
  if Clipboard.HasFormat(cATClipboardFormatId) then
  begin
    if ModeOneLine then Exit;
    Result:= DoCommand_ClipboardPasteColumnBlock(AKeepCaret);
    Exit
  end;

  //usual text
  Str:= UTF8Decode(Clipboard.AsText);
  if ModeOneLine then
    Str:= SRemoveNewlineChars(Str);

  Strings.BeginUndoGroup;
  DoCommand_TextDeleteSelection;
  Result:= DoCommand_TextInsertAtCarets(Str,
             AKeepCaret,
             FOverwrite and FOptOverwriteAllowedOnPaste,
             ASelectThen);
  Strings.EndUndoGroup;

  //workaround: paste on last line end
  if Carets.Count>0 then
    if Carets[0].PosY>=Strings.Count then
      Strings.ActionAddFakeLineIfNeeded;
end;


function TATSynEdit.DoCommand_ClipboardPasteColumnBlock(AKeepCaret: boolean): TATCommandResults;
var
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Str:= UTF8Decode(Clipboard.AsText);
  Strings.BeginUndoGroup;
  Result:= DoCommand_TextInsertColumnBlockOnce(Str, AKeepCaret);
  Strings.EndUndoGroup;
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
  if not IsSelRectEmpty then
  begin
    Clipboard.AsText:= GetTextForClipboard;
    Clipboard.AddFormat(cATClipboardFormatId, cATClipboardSignatureColBlock, SizeOf(cATClipboardSignatureColBlock));
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
  Caret: TATCaretItem;
  i, NLen, X1, Y1, X2, Y2: integer;
  bSel: boolean;
  Str: atString;
  EolString: string;
begin
  Result:= '';
  EolString:= cLineEndStrings[Strings.Endings];

  if not IsSelRectEmpty then
  begin
    for i:= FSelRect.Top to FSelRect.Bottom do
    begin
      Str:= Strings.Lines[i];
      X1:= SColumnPosToCharPos(Str, FSelRect.Left, OptTabSize);
      X2:= SColumnPosToCharPos(Str, FSelRect.Right, OptTabSize);
      Str:= Strings.TextSubstring(X1, i, X2, i);
      NLen:= X2-X1-Length(Str);
      if NLen>0 then
        Str:= Str+StringOfChar(' ', NLen);
      Result:= Result+UTF8Encode(Str)+EolString;
    end;
    Exit;
  end;

  ListNum:= TList.Create;
  ListStr:= TStringList.Create;

  try
    ListStr.LineBreak:= EolString;

    for i:= 0 to Carets.Count-1 do
    begin
      Caret:= Carets[i];
      if ListNum.IndexOf(pointer{%H-}(Caret.PosY))<0 then
        ListNum.Add(pointer{%H-}(Caret.PosY));
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
        Caret:= Carets[i];
        Caret.GetRange(X1, Y1, X2, Y2, bSel);
        if not bSel then Continue;

        Str:= Strings.TextSubstring(X1, Y1, X2, Y2, EolString);
        if Str<>'' then
          ListStr.Add(UTF8Encode(Str));
      end;

      if ListStr.Count=1 then
        Result:= ListStr[0] //don't use Text, to skip eol
      else
        Result:= ListStr.Text;
    end;
  finally
    FreeAndNil(ListStr);
    FreeAndNil(ListNum);
  end;
end;

