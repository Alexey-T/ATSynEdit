{$ifdef nn}begin end;{$endif}

function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean;
  AClipboardObject: TClipboard): TATCommandResults;
var
  Str: atString;
  bNeedFixLastEol: boolean;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //column block
  if AClipboardObject.HasFormat(cATClipboardFormatId) then
  begin
    if ModeOneLine then Exit;
    Result:= DoCommand_ClipboardPasteColumnBlock(AKeepCaret, AClipboardObject);
    Exit
  end;

  //usual text
  Str:= UTF8Decode(AClipboardObject.AsText);
  if ModeOneLine then
    Str:= SRemoveNewlineChars(Str);

  //we delete unneeded eol after paste, if:
  //- one caret, on last line
  //- last line w/o eol
  //- pasted text w/o final eol
  if OptPasteAtEndMakesFinalEmptyLine or ASelectThen then
    bNeedFixLastEol:= false
  else
    bNeedFixLastEol:=
      (Strings.Count>0) and
      (Carets.Count=1) and
      (Carets[0].PosY=Strings.Count-1) and
      (Strings.LinesEnds[Strings.Count-1]=cEndNone) and
      not SEndsWithEol(Str);

  //paste
  Strings.BeginUndoGroup;
  DoCommand_TextDeleteSelection;
  Result:= DoCommand_TextInsertAtCarets(Str,
             AKeepCaret,
             FOverwrite and FOptOverwriteAllowedOnPaste,
             ASelectThen);
  Strings.EndUndoGroup;

  //special case: paste on text end
  if Carets.Count>0 then
    if bNeedFixLastEol then
    begin
      Strings.ActionDeleteFakeLineAndFinalEol;
      if not AKeepCaret then
        DoCaretSingle(
          Strings.LinesLen[Strings.Count-1],
          Strings.Count-1
          );
    end
    else
    begin
      if Carets[Carets.Count-1].PosY>=Strings.Count then
        Strings.ActionAddFakeLineIfNeeded;
    end;
end;


function TATSynEdit.DoCommand_ClipboardPasteColumnBlock(AKeepCaret: boolean;
  AClipboardObject: TClipboard): TATCommandResults;
var
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Str:= UTF8Decode(AClipboardObject.AsText);
  Strings.BeginUndoGroup;
  Result:= DoCommand_TextInsertColumnBlockOnce(Str, AKeepCaret);
  Strings.EndUndoGroup;
end;


function TATSynEdit.DoCommand_ClipboardCut(AClipboardObject: TClipboard): TATCommandResults;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  if Carets.IsSelection then
  begin
    DoCommand_ClipboardCopy(false, AClipboardObject);
    Result:= DoCommand_TextDeleteSelection;
  end
  else
  begin
    if FOptCutLinesIfNoSel then
    begin
      DoCommand_ClipboardCopy(false, AClipboardObject);
      Result:= DoCommand_TextDeleteLines;
    end;
  end;
end;

function TATSynEdit.DoCommand_ClipboardCopy(Append: boolean;
  AClipboardObject: TClipboard): TATCommandResults;
begin
  if not IsSelRectEmpty then
  begin
    AClipboardObject.AsText:= GetTextForClipboard;
    AClipboardObject.AddFormat(
      cATClipboardFormatId,
      cATClipboardSignatureColBlock,
      SizeOf(cATClipboardSignatureColBlock));
  end
  else
  begin
    if Append then
      AClipboardObject.AsText:= AClipboardObject.AsText+GetTextForClipboard
    else
      AClipboardObject.AsText:= GetTextForClipboard;
  end;

  {$ifndef windows}
  //for Linux gtk2, not sure about Mac
  PrimarySelection.AsText:= AClipboardObject.AsText;
  {$endif}

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

