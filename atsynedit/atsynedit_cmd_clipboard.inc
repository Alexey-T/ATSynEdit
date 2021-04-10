{$ifdef nn}begin end;{$endif}

function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean;
  AClipboardObject: TClipboard): TATCommandResults;
var
  St: TATStrings;
  Str: atString;
  bHandled, bNeedFixLastEol: boolean;
  Num64: Int64;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  St:= Strings;

  if Assigned(FOnPaste) then
  begin
    bHandled:= false;
    FOnPaste(Self, bHandled, AKeepCaret, ASelectThen);
    if bHandled then exit;
  end;

  //column block
  if AClipboardObject.HasFormat(ATClipboardColumnFormat) then
  begin
    if ModeOneLine then Exit;
    Result:= DoCommand_ClipboardPasteColumnBlock(AKeepCaret, AClipboardObject);
    Exit
  end;

  //usual text
  Str:= UTF8Decode(AClipboardObject.AsText);
  if ModeOneLine then
    Str:= SRemoveNewlineChars(Str);

  if FOptInputNumberOnly then
    if not TryStrToInt64(Str, Num64) then exit;

  //we delete unneeded eol after paste, if:
  //- one caret, on last line
  //- last line w/o eol
  //- pasted text w/o final eol
  if OptPasteAtEndMakesFinalEmptyLine or ASelectThen then
    bNeedFixLastEol:= false
  else
    bNeedFixLastEol:=
      (St.Count>0) and
      (Carets.Count=1) and
      (Carets[0].PosY=St.Count-1) and
      (St.LinesEnds[St.Count-1]=cEndNone) and
      not SEndsWithEol(Str);

  //paste
  St.BeginUndoGroup;
  DoCommand_TextDeleteSelection;
  Result:= DoCommand_TextInsertAtCarets(Str,
             AKeepCaret,
             FOverwrite and FOptOverwriteAllowedOnPaste,
             ASelectThen);
  St.EndUndoGroup;

  //solve CudaText #3270
  St.SetGroupMark;

  //test special case: paste on text end
  if Carets.Count>0 then
    if bNeedFixLastEol then
    begin
      St.ActionDeleteFakeLineAndFinalEol;
      //correct caret after line deleted
      if not AKeepCaret
        and (Carets[0].PosY>=St.Count)
        and (St.Count>0) then
        DoCaretSingle(
          St.LinesLen[St.Count-1],
          St.Count-1
          );
    end
    else
    begin
      if Carets[Carets.Count-1].PosY>=St.Count then
        St.ActionAddFakeLineIfNeeded;
    end;
end;


function TATSynEdit.DoCommand_ClipboardPasteColumnBlock(AKeepCaret: boolean;
  AClipboardObject: TClipboard): TATCommandResults;
var
  Str: atString;
  Num64: Int64;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Str:= UTF8Decode(AClipboardObject.AsText);

  if FOptInputNumberOnly then
    if not TryStrToInt64(Str, Num64) then exit;

  Strings.BeginUndoGroup;
  try
    DoCommand_TextDeleteSelection;
    Result:= DoCommand_TextInsertColumnBlockOnce(Str, AKeepCaret);
  finally
    Strings.EndUndoGroup;
  end;
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
var
  SNewText: string;
begin
  SNewText:= GetTextForClipboard;
  if not IsSelRectEmpty then
  begin
    //copy vertical block
    SClipboardCopy(SNewText, AClipboardObject);
    AClipboardObject.AddFormat(
      ATClipboardColumnFormat,
      ATClipboardColumnSignature,
      SizeOf(ATClipboardColumnSignature));
  end
  else
  begin
    //copy usual block
    if Append then
      SClipboardCopy(AClipboardObject.AsText+SNewText, AClipboardObject)
    else
      SClipboardCopy(SNewText, AClipboardObject);
  end;

  {$ifndef windows}
  //for Linux gtk2, not sure about Mac
  if AClipboardObject=Clipboard then
    SClipboardCopy(AClipboardObject.AsText, PrimarySelection);
  {$endif}

  Result:= [];
end;


function TATSynEdit.GetTextForClipboard: string;
var
  ListNum: TATIntegerList;
  ListStr: TStringList;
  Caret: TATCaretItem;
  i, NLen, X1, Y1, X2, Y2: integer;
  bSel: boolean;
  Str, STemp: atString;
  EolString: string;
begin
  Result:= '';

  //EolString:= cLineEndStrings[Strings.Endings]; //gives incorrect result with MacOS lineends on Linux
  EolString:= System.LineEnding;

  //make text from column selection
  if not IsSelRectEmpty then
  begin
    for i:= FSelRect.Top to FSelRect.Bottom do
    begin
      X1:= Strings.ColumnPosToCharPos(i, FSelRect.Left, FTabHelper);
      X2:= Strings.ColumnPosToCharPos(i, FSelRect.Right, FTabHelper);
      Str:= Strings.TextSubstring(X1, i, X2, i);
      if FOptCopyColumnBlockAlignedBySpaces then
      begin
        NLen:= X2-X1-Length(Str);
        if NLen>0 then
          Str+= StringOfCharW(' ', NLen);
      end;
      Result:= Result+UTF8Encode(Str)+EolString;
    end;
    Exit;
  end;

  //make text from normal selection
  ListNum:= TATIntegerList.Create;
  ListStr:= TStringList.Create;

  try
    ListStr.LineBreak:= EolString;

    for i:= 0 to Carets.Count-1 do
    begin
      Caret:= Carets[i];
      if not Strings.IsIndexValid(Caret.PosY) then
        Continue;
      if ListNum.IndexOf(Caret.PosY)<0 then
        ListNum.Add(Caret.PosY);
    end;

    //no selections-- copy entire lines
    if not Carets.IsSelection then
    begin
      if FOptCopyLinesIfNoSel then
      begin
        for i:= 0 to ListNum.Count-1 do
        begin
          STemp:= Strings.Lines[ListNum[i]];
          if STemp<>'' then
            ListStr.Add(UTF8Encode(STemp));
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

        Str:= Strings.TextSubstring(X1, Y1, X2, Y2, EolString{%H-});
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

