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
          OptWrapMode:= cWrapAtWindowOrMargin
        else
          OptWrapMode:= cWrapOff;
      end;
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
  with Gutter.Items[FGutterBandNumbers] do
    Visible:= not Visible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleFolding: TATCommandResults;
begin
  with Gutter.Items[FGutterBandFolding] do
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
  if ADown then
    Result:= [cResultCaretBottom]
  else
    Result:= [cResultCaretTop];
end;

function TATSynEdit.DoCommand_ScaleDelta(AIncrease: boolean): TATCommandResults;
begin
  DoScaleFontDelta(AIncrease, true);
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ScaleReset: TATCommandResults;
begin
  //use OptScaleFont setter, not FOptScaleFont
  OptScaleFont:= 0;
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

procedure TATSynEdit.DoMarkAllRangesUnfolded;
var
  R: PATSynRange;
  i: integer;
begin
  for i:= 0 to Fold.Count-1 do
  begin
    R:= Fold.ItemPtr(i);
    if R^.Folded then
    begin
      R^.Folded:= false;
      FWrapUpdateNeeded:= true;
    end;
  end;
end;

procedure TATSynEdit.DoMarkAllLinesVisible;
var
  St: TATStrings;
  i: integer;
begin
  St:= Strings;
  for i:= 0 to St.Count-1 do
  begin
    St.LinesHidden[i, EditorIndex]:= false;
    St.LinesFoldFrom[i, EditorIndex]:= 0;
  end;
end;

function TATSynEdit.DoCommand_FoldUnAll: TATCommandResults;
begin
  DoMarkAllRangesUnfolded;
  DoMarkAllLinesVisible;

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
  Result:= [];
  //if Carets.Count<>1 then Exit; //we don't need this cmd for multi-carets
  if DoFoldUnfoldRangeAtCurLine(ACommand) then
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
  Y1, Y2, NRangeIndex: integer;
begin
  Result:= [];
  if Carets.Count<>1 then exit;
  Caret:= Carets[0];
  if not Caret.IsSelection then exit;
  Caret.GetSelLines(Y1, Y2);

  //better clear the selection
  Caret.SelectNone;

  UpdateFoldLineIndexer;

  //don't use FindRangeWithPlusAtLine because it uses binary search, while
  //range 'for selection' is added unsorted
  if Fold.FindRangeWithPlusAtLine_ViaIndexer(Y1)>=0 then exit;

  Fold.AddSorted(1, Y1, Y2, false, '', cTagPersistentFoldRange, NRangeIndex);
  DoRangeFold(NRangeIndex);

  Result:= [cResultCaretAny, cResultFoldChange];
end;


procedure TATSynEdit.DoConvertTabsToSpaces;
var
  St: TATStrings;
  S1, S2: atString;
  i: integer;
begin
  St:= Strings;
  St.BeginUndoGroup;
  try
    for i:= 0 to St.Count-1 do
    begin
      S1:= St.Lines[i];
      if not SStringHasTab(S1) then Continue;

      S2:= FTabHelper.TabsToSpaces(i, S1);
      if S1<>S2 then
        St.Lines[i]:= S2;
    end;
  finally
    St.EndUndoGroup;
    Update(true);
    DoEventChange;
  end;
end;

procedure TATSynEdit.DoConvertIndentation(ASpacesToTabs: boolean);
var
  St: TATStrings;
  S1, SBegin, SBegin2, SEnd: atString;
  N, i: integer;
begin
  St:= Strings;
  St.BeginUndoGroup;
  try
    for i:= 0 to St.Count-1 do
    begin
      S1:= St.Lines[i];

      N:= SGetIndentChars(S1);
      if N=0 then Continue;
      SBegin:= Copy(S1, 1, N);
      SEnd:= Copy(S1, N+1, MaxInt);

      if ASpacesToTabs then
        SBegin2:= FTabHelper.SpacesToTabs(i, SBegin)
      else
        SBegin2:= FTabHelper.TabsToSpaces(i, SBegin);

      if SBegin2<>SBegin then
        St.Lines[i]:= SBegin2+SEnd;
    end;
  finally
    St.EndUndoGroup;
    Update(true);
    DoEventChange;
  end;
end;

