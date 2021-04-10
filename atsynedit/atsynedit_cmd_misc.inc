{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_ToggleOverwrite: TATCommandResults;
begin
  ModeOverwrite:= not ModeOverwrite;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleWordWrap(AltOrder: boolean): TATCommandResults;
begin
  case FWrapMode of
    cWrapOff:
      OptWrapMode:= cWrapOn;
    cWrapOn:
      begin
        if AltOrder then
          OptWrapMode:= cWrapAtMargin
        else
          OptWrapMode:= cWrapOff;
      end;
    cWrapAtMargin,
    cWrapAtWindowOrMargin:
      OptWrapMode:= cWrapOff;
  end;

  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprinted: TATCommandResults;
begin
  OptUnprintedVisible:= not OptUnprintedVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprintedSpaces: TATCommandResults;
begin
  OptUnprintedSpaces:= not OptUnprintedSpaces;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprintedSpacesTrailing: TATCommandResults;
begin
  OptUnprintedSpacesTrailing:= not OptUnprintedSpacesTrailing;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprintedEnds: TATCommandResults;
begin
  OptUnprintedEnds:= not OptUnprintedEnds;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleUnprintedEndDetails: TATCommandResults;
begin
  OptUnprintedEndsDetails:= not OptUnprintedEndsDetails;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleLineNums: TATCommandResults;
begin
  with Gutter.Items[GutterBandNumbers] do
    Visible:= not Visible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleFolding: TATCommandResults;
begin
  with Gutter.Items[GutterBandFolding] do
    Visible:= not Visible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleRuler: TATCommandResults;
begin
  OptRulerVisible:= not OptRulerVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleMiniMap: TATCommandResults;
begin
  OptMinimapVisible:= not OptMinimapVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleMicroMap: TATCommandResults;
begin
  OptMicromapVisible:= not OptMicromapVisible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleReadOnly: TATCommandResults;
begin
  ModeReadOnly:= not ModeReadOnly;
  FIsReadOnlyChanged:= true;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_CaretsExtend(ADown: boolean; ALines: integer): TATCommandResults;
begin
  DoCaretsExtend(ADown, ALines);
  Result:= [cResultCaretAny];
end;

function TATSynEdit.DoCommand_ScaleDelta(AIncrease: boolean): TATCommandResults;
begin
  DoScaleFontDelta(AIncrease);
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ScaleReset: TATCommandResults;
begin
  FOptScaleFont:= 0;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_FoldAll: TATCommandResults;
var
  R: PATSynRange;
  NeedFixCaret: boolean;
  i: integer;
begin
  for i:= 0 to Fold.Count-1 do
  begin
    R:= Fold.ItemPtr(i);
    if R^.IsSimple then Continue;
    DoRangeFold(i);
  end;

  NeedFixCaret:= false;
  for i:= 0 to Carets.Count-1 do
    if IsLineFolded(Carets[i].PosY) then
    begin
      NeedFixCaret:= true;
      Break;
    end;
  if NeedFixCaret then
    DoCaretSingle(0, 0);

  Result:= [cResultCaretTop, cResultScroll, cResultFoldChange];
end;

function TATSynEdit.DoCommand_FoldUnAll: TATCommandResults;
var
  R: PATSynRange;
  Str: TATStrings;
  i: integer;
begin
  for i:= 0 to Fold.Count-1 do
  begin
    R:= Fold.ItemPtr(i);
    if R^.Folded then
    begin
      R^.Folded:= false;
      //Fold[i]:= R;
      FWrapUpdateNeeded:= true;
    end;
  end;

  Str:= Strings;
  for i:= 0 to Str.Count-1 do
  begin
    Str.LinesHidden[i, EditorIndex]:= false;
    Str.LinesFoldFrom[i, EditorIndex]:= 0;
  end;

  Result:= [cResultCaretTop, cResultScroll, cResultFoldChange];
end;

function TATSynEdit.DoCommand_FoldLevel(ALevel: integer): TATCommandResults;
begin
  DoCommand_FoldUnAll;
  DoFoldForLevelEx(ALevel, -1);
  Result:= [cResultCaretAny, cResultScroll, cResultFoldChange];
end;


function TATSynEdit.DoCommand_FoldRangeAtCurLine(ACommand: TATEditorFoldRangeCommand): TATCommandResults;
begin
  DoFoldUnfoldRangeAtCurLine(ACommand);
  Result:= [cResultCaretAny, cResultScroll, cResultFoldChange];
end;

procedure TATSynEdit.UpdateFoldLineIndexer;
//if called w/o lexer active (EControl adapter inits LineIndexer),
//init LineIndexer
var
  NCount: integer;
begin
  NCount:= Strings.Count;
  if Fold.CountOfLineIndexer<NCount then
  begin
    Fold.ClearLineIndexer(NCount, true);
    //Fold.UpdateLineIndexer; //Fold methods auto update all
  end;
end;

function TATSynEdit.DoCommand_FoldSelection: TATCommandResults;
var
  Caret: TATCaretItem;
  Y1, Y2: integer;
begin
  Result:= [];
  if Carets.Count<>1 then exit;
  Caret:= Carets[0];
  if Caret.EndY<0 then exit;
  Caret.GetSelLines(Y1, Y2);

  //better clear the selection
  Caret.SelectNone;

  UpdateFoldLineIndexer;
  if Fold.FindRangeWithPlusAtLine(Y1)>=0 then exit;

  Fold.Add(1, Y1, Y2, false, '', cTagPersistentFoldRange);
  DoRangeFold(Fold.Count-1);

  Result:= [cResultCaretAny, cResultFoldChange];
end;


procedure TATSynEdit.DoConvertTabsToSpaces;
var
  S1, S2: atString;
  i: integer;
begin
  Strings.BeginUndoGroup;
  try
    for i:= 0 to Strings.Count-1 do
    begin
      S1:= Strings.Lines[i];
      if not SStringHasTab(S1) then Continue;

      S2:= FTabHelper.TabsToSpaces(i, S1);
      if S1<>S2 then
        Strings.Lines[i]:= S2;
    end;
  finally
    Strings.EndUndoGroup;
    Update(true);
    DoEventChange;
  end;
end;

procedure TATSynEdit.DoConvertIndentation(ASpacesToTabs: boolean);
var
  S1, SBegin, SBegin2, SEnd: atString;
  N, i: integer;
begin
  Strings.BeginUndoGroup;
  try
    for i:= 0 to Strings.Count-1 do
    begin
      S1:= Strings.Lines[i];

      N:= SGetIndentChars(S1);
      if N=0 then Continue;
      SBegin:= Copy(S1, 1, N);
      SEnd:= Copy(S1, N+1, MaxInt);

      if ASpacesToTabs then
        SBegin2:= FTabHelper.SpacesToTabs(i, SBegin)
      else
        SBegin2:= FTabHelper.TabsToSpaces(i, SBegin);

      if SBegin2<>SBegin then
        Strings.Lines[i]:= SBegin2+SEnd;
    end;
  finally
    Strings.EndUndoGroup;
    Update(true);
    DoEventChange;
  end;
end;

