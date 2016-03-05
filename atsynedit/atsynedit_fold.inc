{$ifdef nn}begin end;{$endif}

function TATSynEdit.IsLineFolded(ALine: integer; ADetectPartialFold: boolean): boolean;
var
  Flag: integer;
begin
  if not Strings.IsIndexValid(ALine) then
  begin
    Result:= false;
    Exit;
  end;

  Flag:= Strings.LinesHidden[ALine, FEditorIndex];
  Result:= (Flag=-1) or (ADetectPartialFold and (Flag>0));
end;

function TATSynEdit.IsLineFoldedFull(ALine: integer): boolean;
begin
  Result:= IsLineFolded(ALine, false);
end;

function TATSynEdit.GetFirstUnfoldedLineNumber: integer;
begin
  Result:= GetNextUnfoldedLineNumber(0, true);
end;

function TATSynEdit.GetLastUnfoldedLineNumber: integer;
begin
  Result:= GetNextUnfoldedLineNumber(Strings.Count-1, false);
end;

function TATSynEdit.GetNextUnfoldedLineNumber(ALine: integer; ADown: boolean): integer;
var
  N: integer;
begin
  Result:= ALine;
  N:= Result;
  while IsLineFolded(N) and Strings.IsIndexValid(N) do
    N:= N+BoolToPlusMinusOne(ADown);
  if Strings.IsIndexValid(N) then Result:= N;
end;

function TATSynEdit.IsPosFolded(AX, AY: integer): boolean;
begin
  Result:= Strings.IsPosFolded(AX, AY, FEditorIndex);
end;

(*
example of CPP file which is hard to unfold (if nested ranges folded).
 {
 d1
   {
   d2a
   }
   {
   d2b
     {
     d3a
     }
     {
     d3b
       {
       d4a
       }
       {
       d4b
       }
     }
   }
 }

what we do. for each line in range:
a) if line not in any subrange, show it
b) for all subranges at top level:
  b1) if subrange marked folded, unfold 1st line "[...]"
  b2) if subrange marked unfolded, recursion
*)
procedure TATSynEdit.DoRangeUnfold(ARange: TATSynRange);
var
  List: TATIntArray;
  R: TATSynRange;
  i, j: integer;
  InSubrange: boolean;
begin
  ARange.Folded:= false;
  FWrapUpdateNeeded:= true;

  List:= FFold.FindRangesContainingLines(-1, -1, ARange,
    false{OnlyFolded}, true{TopLevel}, cRngIgnore);

  //show all lines not in subranges
  for i:= ARange.Y to ARange.Y2 do
  begin
    InSubrange:= false;
    for j:= Low(List) to High(List) do
      if FFold[List[j]].IsLineInside(i) then
      begin
        InSubrange:= true;
        Break
      end;

    if not InSubrange then
      Strings.LinesHidden[i, FEditorIndex]:= 0;
  end;

  //unfold subranges, resursion
  for i:= Low(List) to High(List) do
  begin
    R:= FFold[List[i]];
    if R.Folded then
      Strings.LinesHidden[R.Y, FEditorIndex]:= R.X
    else
      DoRangeUnfold(R);
  end;
end;

procedure TATSynEdit.DoRangeFold(ARange: TATSynRange);
var
  i: integer;
begin
  ARange.Folded:= true;
  FWrapUpdateNeeded:= true;

  //partially fold 1st line
  if ARange.Hint<>'' then
  begin
    Strings.LinesHidden[ARange.Y, FEditorIndex]:= ARange.X;
  end
  else
  case FFoldStyle of
    cFoldHereWithDots:
    begin
      Strings.LinesHidden[ARange.Y, FEditorIndex]:= ARange.X;
    end;
    cFoldHereWithTruncatedText:
    begin
      Strings.LinesHidden[ARange.Y, FEditorIndex]:= ARange.X;
      ARange.Hint:= Copy(Strings.Lines[ARange.Y], ARange.X, cFoldedLenOfEmptyHint)+'...';
    end;
    cFoldFromEndOfLine:
    begin
      Strings.LinesHidden[ARange.Y, FEditorIndex]:= Length(Strings.Lines[ARange.Y])+1;
    end;
    cFoldFromNextLine:
    begin
      //don't fold line
    end;
  end;

  //fully fold next lines
  for i:= ARange.Y+1 to ARange.Y2 do
    Strings.LinesHidden[i, FEditorIndex]:= -1;
end;


procedure TATSynEdit.DoUnfoldLine(ALine: integer);
var
  List: TATIntArray;
  i: integer;
begin
  List:= FFold.FindRangesContainingLines(ALine, ALine, nil,
    true{OnlyFolded}, false{TopLevelOnly}, cRngHasAllLines);
  for i:= Low(List) to High(List) do
    DoRangeUnfold(FFold[List[i]]);
end;

procedure TATSynEdit.DoFoldbarClick(ALine: integer);
var
  R: TATSynRange;
begin
  R:= FFold.FindRangeWithPlusAtLine(ALine);
  if Assigned(R) then
  begin
    if R.Folded then
      DoRangeUnfold(R)
    else
      DoRangeFold(R);
    Update;
  end;
end;


function TATSynEdit.GetFoldedMarkText(ALine: integer): string;
var
  R: TATSynRange;
begin
  Result:= '';
  R:= FFold.FindRangeWithPlusAtLine(ALine);
  if Assigned(R) then
    Result:= R.Hint;
  if Result='' then
    Result:= '...';
end;

procedure TATSynEdit.UpdateFoldedFromLinesHidden;
var
  i, j: integer;
  N: integer;
  R: TATSynRange;
begin
  for i:= 0 to Strings.Count-1 do
  begin
    N:= Strings.LinesHidden[i, FEditorIndex];
    if N<=0 then Continue;

    for j:= 0 to Fold.Count-1 do
    begin
      R:= Fold.Items[j];
      if (R.Y>i) then Break;
      if (R.Y=i) and (R.X=N) then
      begin
        DoRangeFold(R); //do not just R.Folded:= true;
        Break
      end;
    end;
  end;
end;

function TATSynEdit.IsFoldLineNeededBeforeWrapitem(N: integer): boolean;
var
  NLineCur, NLinePrev: integer;
begin
  if FWrapInfo.IsIndexValid(N) and (N>0) then
  begin
    NLineCur:= FWrapInfo.Items[N].NLineIndex;
    NLinePrev:= FWrapInfo.Items[N-1].NLineIndex;
    //before this line some is skipped
    Result:= NLineCur-NLinePrev > 1;
    //and prev visible line is fully visible
    if Result then
      Result:= Strings.LinesHidden[NLinePrev, FEditorIndex]=0;
  end
  else
    Result:= false;
end;

procedure TATSynEdit.DoMenuGutterFold;
var
  Menu: TPopupMenu;
  mi, miSub: TMenuItem;
  i: integer;
begin
  InitResourcesFoldbar;
  if FMenuGutterFoldStd=nil then
    FMenuGutterFoldStd:= TPopupMenu.Create(Self);

  Menu:= FMenuGutterFoldStd;
  Menu.Images:= FFoldImageList;
  Menu.Items.Clear;

  //items "fold all", "unfold all"
  mi:= TMenuItem.Create(Self);
  mi.Caption:= cStrMenuitemFoldAll;
  mi.OnClick:= @MenuFoldFoldAllClick;
  mi.Enabled:= Fold.Count>0;
  Menu.Items.Add(mi);

  mi:= TMenuItem.Create(Self);
  mi.Caption:= cStrMenuitemUnfoldAll;
  mi.OnClick:= @MenuFoldUnfoldAllClick;
  mi.Enabled:= Fold.Count>0;
  Menu.Items.Add(mi);

  //submenu "fold level"
  miSub:= TMenuItem.Create(Self);
  miSub.Caption:= cStrMenuitemFoldLevel;
  miSub.Enabled:= Fold.Count>0;
  Menu.Items.Add(miSub);

  for i:= 2 to 9 do
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= Inttostr(i);
    mi.Tag:= i-1;
    mi.OnClick:=@MenuFoldLevelClick;
    miSub.Add(mi);
  end;

  //dynamic items [+], [-]
  DoMenuGutterFold_AddDynamicItems(Menu);

  Menu.Popup;
end;


procedure TATSynEdit.DoMenuGutterFold_AddDynamicItems(Menu: TPopupMenu);
var
  Pnt: TPoint;
  AtEnd: boolean;
  NLine: integer;
  IntList: TATIntArray;
  Rng: TATSynRange;
  mi: TMenuItem;
  i: integer;
begin
  //calc ranges for curr line
  Pnt:= ScreenToClient(Mouse.CursorPos);
  Pnt:= ClientPosToCaretPos(Pnt, AtEnd);
  NLine:= Pnt.Y;
  if NLine<0 then Exit;

  IntList:= Fold.FindRangesContainingLines(NLine, NLine, nil,
    false{OnlyFolded}, false{TopLevel}, cRngHasAllLines);
  if Length(IntList)=0 then Exit;

  //separator
  mi:= TMenuItem.Create(Self);
  mi.Caption:= '-';
  Menu.Items.Add(mi);

  //items for ranges for current line
  for i:= 0 to High(IntList) do
  begin
    Rng:= Fold[IntList[i]];
    mi:= TMenuItem.Create(Self);
    mi.Tag:= ptrint(Rng);
    mi.OnClick:= @MenuFoldPlusMinusClick;

    mi.Caption:=
      cHintScrollPrefix+' '+Inttostr(Rng.Y+1)+': '+
      UTF8Encode(Copy(Strings.Lines[Rng.Y], 1, cFoldedLenOfEmptyHint));

    if Rng.Folded then
      mi.ImageIndex:= 0
    else
      mi.ImageIndex:= 1;

    Menu.Items.Add(mi);
  end;
end;

procedure TATSynEdit.InitResourcesFoldbar;
begin
  if FFoldImageList=nil then
  begin
    FFoldImageList:= TImageList.Create(Self);
    FFoldImageList.Width:= 12;
    FFoldImageList.Height:= 12;
    FFoldImageList.AddResourceName(HInstance, 'ATSYN_FOLD_P');
    FFoldImageList.AddResourceName(HInstance, 'ATSYN_FOLD_M');
  end;
end;


procedure TATSynEdit.MenuFoldPlusMinusClick(Sender: TObject);
var
  Rng: TATSynRange;
begin
  Rng:= TATSynRange((Sender as TComponent).Tag);
  if Rng.Folded then
    DoRangeUnfold(Rng)
  else
    DoRangeFold(Rng);
  Update;
end;

procedure TATSynEdit.MenuFoldFoldAllClick(Sender: TObject);
begin
  DoCommand(cCommand_FoldAll);
end;

procedure TATSynEdit.MenuFoldLevelClick(Sender: TObject);
begin
  DoFoldForLevel((Sender as TComponent).Tag);
end;

procedure TATSynEdit.MenuFoldUnfoldAllClick(Sender: TObject);
begin
  DoCommand(cCommand_UnfoldAll);
end;


procedure TATSynEdit.DoFoldForLevelAndLines(ALineFrom, ALineTo: integer;
  ALevel: integer; AForThisRange: TATSynRange);
var
  List: TATIntArray;
  R: TATSynRange;
  i: integer;
begin
  //this func recursive. it calls itself with ALevel-1.
  //folds ranges if ALevel=0, else goes to subranges until found ALevel=0.
  if ALevel<0 then exit;

  List:= Fold.FindRangesContainingLines(ALineFrom, ALineTo, AForThisRange,
    false{OnlyFolded}, true{TopLevel}, cRngExceptThisRange);

  for i:= Low(List) to High(List) do
  begin
    R:= Fold.Items[List[i]];
    if R.IsSimple then Continue;
    if R.Folded then Continue;
    if ALevel=0 then
      DoRangeFold(R)
    else
      DoFoldForLevelAndLines(R.Y, R.Y2, ALevel-1, R);
  end;
end;

procedure TATSynEdit.DoFoldForLevel(ALevel: integer);
begin
  DoCommand(cCommand_UnfoldAll);
  DoFoldForLevelAndLines(0, Strings.Count-1, ALevel, nil);
  Update;
end;

