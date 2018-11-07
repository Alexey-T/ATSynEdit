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
    cWrapAtMargin:
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
  with Gutter.Items[GutterBandNum] do
    Visible:= not Visible;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleFolding: TATCommandResults;
begin
  with Gutter.Items[GutterBandFold] do
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
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_CaretsExtend(ADown: boolean; ALines: integer): TATCommandResults;
begin
  DoCaretsExtend(ADown, ALines);
  Result:= [cResultCaretAny];
end;

function TATSynEdit.DoCommand_SizeChange(AIncrease: boolean): TATCommandResults;
begin
  DoSizeChange(AIncrease);
  Result:= [cResultState];
end;


function TATSynEdit.DoCommand_FoldUnfoldAll(ADoFold: boolean): TATCommandResults;
var
  R: TATSynRange;
  NeedFixCaret: boolean;
  i: integer;
begin
  if ADoFold then
  begin
    for i:= 0 to Fold.Count-1 do
    begin
      R:= Fold[i];
      if R.IsSimple then Continue;
      R.Folded:= false;
      DoRangeFold(R);
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
  end
  else
  begin
    for i:= 0 to Fold.Count-1 do
    begin
      R:= Fold[i];
      if R.Folded then
        DoRangeUnfold(R);
    end;
  end;

  Result:= [cResultCaretTop, cResultScroll, cResultFoldChange];
end;

function TATSynEdit.DoCommand_FoldLevel(ALevel: integer): TATCommandResults;
begin
  DoFoldForLevel(ALevel);
  Result:= [cResultCaretAny, cResultScroll, cResultFoldChange];
end;


function TATSynEdit.DoCommand_FoldRangeAtCurLine(ACommand: TATFoldRangeCmd): TATCommandResults;
begin
  DoFoldUnfoldRangeAtCurLine(ACommand);
  Result:= [cResultCaretAny, cResultScroll, cResultFoldChange];
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

      S2:= FTabHelper.TabsToSpaces(S1);
      if S1<>S2 then
        Strings.Lines[i]:= S2;
    end;
  finally
    Strings.EndUndoGroup;
    Update(true);
    DoEventChange;
  end;
end;

procedure TATSynEdit.DoConvertSpacesToTabsLeading;
var
  S1, SBegin, SBegin2, SEnd: atString;
  N, i: integer;
begin
  Strings.BeginUndoGroup;
  try
    for i:= 0 to Strings.Count-1 do
    begin
      S1:= Strings.Lines[i];
      if Pos(' ', S1)=0 then Continue;

      N:= SGetIndentChars(S1);
      if N=0 then Continue;
      SBegin:= Copy(S1, 1, N);
      SEnd:= Copy(S1, N+1, MaxInt);

      SBegin2:= FTabHelper.SpacesToTabs(SBegin);
      if SBegin2<>SBegin then
        Strings.Lines[i]:= SBegin2+SEnd;
    end;
  finally
    Strings.EndUndoGroup;
    Update(true);
    DoEventChange;
  end;
end;

