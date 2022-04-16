{$ifdef nn}begin end;{$endif}

function TATSynEdit.DoCommand_ClipboardPaste(AKeepCaret, ASelectThen: boolean;
  AClipboardHasColumnBlock: boolean; const AClipboardText: string
  ): TATCommandResults;
var
  St: TATStrings;
  Str: atString;
  bHandled, bNeedFixLastEol: boolean;
  bAtLineStart: boolean;
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
  if AClipboardHasColumnBlock then
  begin
    if ModeOneLine then Exit;
    Result:= DoCommand_ClipboardPasteColumnBlock(AKeepCaret, AClipboardText);
    Exit
  end;

  //usual text
  Str:= UTF8Decode(AClipboardText);
  bAtLineStart:= FOptPasteWithEolAtLineStart and not Carets.IsSelection and SEndsWithEol(Str);
  if ModeOneLine then
  begin
    Str:= SRemoveNewlineChars(Str);
    bAtLineStart:= false;
  end;

  if bAtLineStart then
    DoCaretsDeleteOnSameLines;

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
             ASelectThen,
             bAtLineStart
             );
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
  const AClipboardText: string): TATCommandResults;
var
  Num64: Int64;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  if FOptInputNumberOnly then
    if not TryStrToInt64(AClipboardText, Num64) then exit;

  Strings.BeginUndoGroup;
  try
    DoCommand_TextDeleteSelection;
    Result:= DoCommand_TextInsertColumnBlockOnce(AClipboardText, AKeepCaret);
  finally
    Strings.EndUndoGroup;
  end;
end;


function TATSynEdit.DoCommand_ClipboardPasteAndIndent: TATCommandResults;
//Must mimic Sublime Text's command "Paste and Indent", if I (Alexey) understood its logic
var
  ClipExData: TATEditorClipboardExData;
  Caret: TATCaretItem;
  Lines: TStringList;
  NIndent: integer;
  SNewText, SLineAll, SLineIndent, SLineText: string;
  X1, Y1, X2, Y2, XColumn, i: integer;
  bSel: boolean;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  if Carets.Count<>1 then Exit;

  ATEditorGetClipboardExData(ClipExData);

  Caret:= Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if not Strings.IsIndexValid(Y1) then Exit;

  Result:= DoCommand_TextDeleteSelection; //like Sublime

  //support indents with tab-chars
  XColumn:= TabHelper.CharPosToColumnPos(Y1, Strings.Lines[Y1], X1);

  Lines:= TStringList.Create;
  try
    Lines.TextLineBreakStyle:= tlbsLF;
    Lines.Text:= Clipboard.AsText;
    if Lines.Count=0 then Exit;

    //correct 1st line of block: delete all indent chars
    NIndent:= SGetIndentChars(Lines[0]);
    Lines[0]:= Copy(Lines[0], NIndent+1, MaxInt);

    //correct other lines of block
    for i:= 1 to Lines.Count-1 do
    begin
      SLineAll:= Lines[i];
      NIndent:= SGetIndentChars(SLineAll);
      SLineText:= Copy(SLineAll, NIndent+1, MaxInt);
      //support indent with tab-chars
      NIndent:= TabHelper.CharPosToColumnPos(Y1, SLineAll, NIndent);
      SLineIndent:= StringOfChar(' ', Max(0, XColumn+NIndent-ClipExData.FirstLineIndentColumns));
      if not FOptTabSpaces then
        SLineIndent:= StringReplace(SLineIndent, StringOfChar(' ', FTabSize), #9, [rfReplaceAll]);
      Lines[i]:= SLineIndent+SLineText;
    end;

    SNewText:= Lines.Text;
  finally
    FreeAndNil(Lines);
  end;

  Result:= DoCommand_ClipboardPaste(
    false,
    false,
    false,
    SNewText);
end;


procedure TATSynEdit.MenuitemClipboardRecentsClick(Sender: TObject);
var
  NTag: integer;
begin
  if ModeReadOnly then Exit;
  if Carets.Count=0 then Exit;

  NTag:= (Sender as TComponent).Tag;
  if (NTag>=0) and (NTag<ATEditorClipboardRecents.Count) then
  begin
    DoCommand(
      cCommand_TextInsert,
      cInvokeInternal,
      UTF8Decode(ATEditorClipboardRecents[NTag])
      );
  end;
end;


function TATSynEdit.DoCommand_ClipboardPasteFromRecents: TATCommandResults;
  //
  function PrepareStringForMenu(const S: string): string;
  var
    i: integer;
  begin
    Result:= S;
    SDeleteFrom(Result, #10);
    if Trim(Result)<>'' then exit;
    Result:= S;
    i:= 1;
    while (i<=Length(Result)) and (Result[i] in [' ', #9, #10, #13]) do
      Inc(i);
    Delete(Result, 1, i-1);
    SDeleteFrom(Result, #10);
    Result:= '[...] '+Result;
  end;
  //
var
  mi: TMenuItem;
  Pnt: TPoint;
  S: string;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  if Carets.Count=0 then Exit;

  if ATEditorClipboardRecents=nil then Exit;
  if ATEditorOptions.MaxClipboardRecents<=0 then Exit;

  if ATEditorClipboardRecentMenu=nil then
    ATEditorClipboardRecentMenu:= TPopupMenu.Create(nil);
  ATEditorClipboardRecentMenu.Items.Clear;

  for i:= 0 to ATEditorClipboardRecents.Count-1 do
  begin
    mi:= TMenuItem.Create(ATEditorClipboardRecentMenu);
    S:= ATEditorClipboardRecents[i];
    S:= PrepareStringForMenu(S);
    if Length(S)>ATEditorOptions.MaxClipboardRecentsMenuitemLen then
      S:= Copy(S, 1, ATEditorOptions.MaxClipboardRecentsMenuitemLen)+'...';
    mi.Caption:= S;
    mi.Tag:= i;
    mi.OnClick:= @MenuitemClipboardRecentsClick;
    ATEditorClipboardRecentMenu.Items.Add(mi);
  end;

  //show popup at the caret position, like Sublime
  with Carets[0] do
    Pnt:= Point(PosX, PosY);
  if not Strings.IsIndexValid(Pnt.Y) then
  begin
    Pnt.X:= 0;
    Pnt.Y:= Strings.Count-1;
  end;
  Pnt:= CaretPosToClientPos(Pnt);
  Inc(Pnt.Y, FCharSize.Y);
  //caret can be scrolled away
  Pnt.X:= Min(Width, Max(0, Pnt.X));
  Pnt.Y:= Min(Height, Max(0, Pnt.Y));
  Pnt:= ClientToScreen(Pnt);

  ATEditorClipboardRecentMenu.PopUp(Pnt.X, Pnt.Y);
end;

function TATSynEdit.DoCommand_ClipboardClearRecents: TATCommandResults;
begin
  Result:= [];
  if Assigned(ATEditorClipboardRecents) then
    ATEditorClipboardRecents.Clear;
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
  ClipExData: TATEditorClipboardExData;
  SLine: atString;
  SNewText: string;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  SNewText:= GetTextForClipboard;

  if IsSelColumn then
  begin
    //copy column block
    SClipboardCopy(SNewText, AClipboardObject);
    AClipboardObject.AddFormat(
      ATEditorOptions.ClipboardColumnFormat,
      ATEditorOptions.ClipboardColumnSignature,
      SizeOf(ATEditorOptions.ClipboardColumnSignature));
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
    SClipboardCopy(SNewText, PrimarySelection);
  {$endif}

  if Carets.Count>0 then
  begin
    Carets[0].GetRange(X1, Y1, X2, Y2, bSel);
    if Strings.IsIndexValid(Y1) then
    begin
      SLine:= Strings.Lines[Y1];
      ClipExData:= Default(TATEditorClipboardExData);
      ClipExData.CaretPos:= Rect(X1, Y1, X2, Y2);
      ClipExData.CaretCount:= Carets.Count;
      ClipExData.FirstLineIndentChars:= SGetIndentChars(SLine);
      ClipExData.FirstLineIndentColumns:= TabHelper.CharPosToColumnPos(Y1, SLine, ClipExData.FirstLineIndentChars);
      ClipExData.FileName:= Self.FileName;
      ClipExData.ModifiedVersion:= Strings.ModifiedVersion;
      ClipExData.TickOnCopy:= GetTickCount64;
      AClipboardObject.AddFormat(
        ATEditorOptions.ClipboardExFormat,
        ClipExData,
        SizeOf(ClipExData));
    end;
  end;

  UpdateClipboardRecents(SNewText);

  Result:= [];
end;


procedure TATSynEdit.UpdateClipboardRecents(const AText: string);
var
  i: integer;
begin
  if AText='' then Exit;
  if ATEditorOptions.MaxClipboardRecents<=0 then Exit;

  if ATEditorClipboardRecents=nil then
    ATEditorClipboardRecents:= TStringList.Create;

  i:= ATEditorClipboardRecents.IndexOf(AText);
  if i=0 then
  begin
  end
  else
  if i>0 then
  begin
    ATEditorClipboardRecents.Move(i, 0);
  end
  else
  begin
    ATEditorClipboardRecents.Insert(0, AText);
    while ATEditorClipboardRecents.Count>ATEditorOptions.MaxClipboardRecents do
      ATEditorClipboardRecents.Delete(ATEditorClipboardRecents.Count-1);
  end;
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

