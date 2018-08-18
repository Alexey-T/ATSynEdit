{$ifdef nn}begin end;{$endif}

function TATSynEdit.IsLineFolded(ALine: integer; ADetectPartialFold: boolean): boolean;
var
  bHidden: boolean;
  NFoldFrom: integer;
begin
  if not Strings.IsIndexValid(ALine) then Exit(false);

  bHidden:= Strings.LinesHidden[ALine, FEditorIndex];
  if bHidden then Exit(true);

  NFoldFrom:= Strings.LinesFoldFrom[ALine, FEditorIndex];
  Result:= ADetectPartialFold and (NFoldFrom>0);
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
    begin
      Strings.LinesHidden[i, FEditorIndex]:= false;
      Strings.LinesFoldFrom[i, FEditorIndex]:= 0;
    end;
  end;

  //unfold subranges, resursion
  for i:= Low(List) to High(List) do
  begin
    R:= FFold[List[i]];
    if R.Folded then
      Strings.LinesHidden[R.Y, FEditorIndex]:= false
    else
      DoRangeUnfold(R);
  end;
end;

procedure TATSynEdit.DoRangeFold(ARange: TATSynRange);
begin
  ARange.Folded:= true;
  FWrapUpdateNeeded:= true;

  //partially hide first line
  case FFoldStyle of
    cFoldHereWithDots:
    begin
      Strings.LinesFoldFrom[ARange.Y, FEditorIndex]:= ARange.X;
    end;
    cFoldHereWithTruncatedText:
    begin
      Strings.LinesFoldFrom[ARange.Y, FEditorIndex]:= ARange.X;
      if ARange.Hint='' then
        ARange.Hint:= Strings.LineSub(ARange.Y, ARange.X, cFoldedLenOfEmptyHint)+'...';
    end;
    cFoldFromEndOfLine:
    begin
      if ARange.Hint<>'' then
        Strings.LinesFoldFrom[ARange.Y, FEditorIndex]:= ARange.X
      else
        Strings.LinesFoldFrom[ARange.Y, FEditorIndex]:= Strings.LinesLen[ARange.Y]+1;
    end;
    cFoldFromEndOfLineAlways:
    begin
      Strings.LinesFoldFrom[ARange.Y, FEditorIndex]:= Strings.LinesLen[ARange.Y]+1;
      ARange.Hint:= '...';
    end;
    cFoldFromNextLine:
    begin
      //don't fold line
    end;
  end;

  //fully hide other lines
  DoRangeHide(ARange.Y+1, ARange.Y2);

  if FOptCaretFixAfterRangeFolded then
    Carets.UpdateAfterRangeFolded(ARange.X, ARange.Y, ARange.Y2);
end;


procedure TATSynEdit.DoRangeHide(ALineFrom, ALineTo: integer);
var
  i: integer;
begin
  for i:= ALineFrom to ALineTo do
    Strings.LinesHidden[i, FEditorIndex]:= true;
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
  NFoldFrom, NPosAfterEnd: integer;
  NMaxLine, iRange: integer;
  R: TATSynRange;
  bOk: boolean;
begin
  NMaxLine:= Strings.Count-1;
  for iRange:= 0 to Fold.Count-1 do
  begin
    R:= Fold[iRange];
    if R.IsSimple then Continue;
    if R.Y>NMaxLine then Continue; //same as "if not Strings.IsIndexValid"
    if Strings.LinesHidden[R.Y, FEditorIndex] then Continue;
    NFoldFrom:= Strings.LinesFoldFrom[R.Y, FEditorIndex];

    NPosAfterEnd:= Strings.LinesLen[R.Y]+1;
    if (OptFoldStyle in [cFoldFromEndOfLine, cFoldFromEndOfLineAlways]) and
      (NFoldFrom=NPosAfterEnd) then
      bOk:= true
    else
      bOk:= R.X=NFoldFrom;

    if bOk then
      DoRangeFold(R); //do not just R.Folded:= true;
  end;
end;

function TATSynEdit.IsFoldLineNeededBeforeWrapitem(N: integer): boolean;
var
  NLineCur, NLinePrev: integer;
begin
  if FWrapInfo.IsIndexValid(N) and (N>0) then
  begin
    NLineCur:= FWrapInfo[N].NLineIndex;
    NLinePrev:= FWrapInfo[N-1].NLineIndex;
    //before this line some is skipped
    Result:= NLineCur-NLinePrev > 1;
    //and prev visible line is fully visible
    if Result then
      Result:= Strings.LinesFoldFrom[NLinePrev, FEditorIndex]=0;
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

  for i:= 1 to 9 do
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= IntToStr(i);
    mi.Tag:= i-1;
    mi.OnClick:= @MenuFoldLevelClick;
    miSub.Add(mi);
  end;

  //dynamic items [+], [-]
  DoMenuGutterFold_AddDynamicItems(Menu);

  Menu.Popup;
end;


procedure TATSynEdit.DoMenuGutterFold_AddDynamicItems(Menu: TPopupMenu);
var
  Pnt: TPoint;
  NLine: integer;
  IntList: TATIntArray;
  Details: TATPosDetails;
  Rng: TATSynRange;
  mi: TMenuItem;
  i: integer;
begin
  //calc ranges for curr line
  Pnt:= ScreenToClient(Mouse.CursorPos);
  Pnt:= ClientPosToCaretPos(Pnt, Details);
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
      UTF8Encode(Strings.LineSub(Rng.Y, 1, cFoldedLenOfEmptyHint));

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
    FFoldImageList.Width:= cBitmapFoldPlus.Width;
    FFoldImageList.Height:= cBitmapFoldPlus.Height;
    FFoldImageList.Add(cBitmapFoldPlus, nil);
    FFoldImageList.Add(cBitmapFoldMinus, nil);
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

procedure TATSynEdit.SetFoldEnabled(AValue: boolean);
begin
  if FFoldEnabled=AValue then Exit;
  FFoldEnabled:= AValue;
  if not FFoldEnabled then
  begin
    DoCommand(cCommand_UnfoldAll);
    FFold.Clear;
  end;
  Update;
end;


procedure TATSynEdit.DoFoldUnfoldRangeAtCurLine(AOp: TATFoldRangeCmd);
var
  NLine: integer;
  R: TATSynRange;
begin
  if Carets.Count<>1 then exit;
  NLine:= Carets[0].PosY;
  if not Strings.IsIndexValid(NLine) then exit;

  R:= Fold.FindRangeWithPlusAtLine(NLine);
  if R=nil then exit;

  case AOp of
    cFoldingFold:
      begin
        if not R.Folded then
        begin
          DoRangeFold(R);
          Update;
        end;
      end;
    cFoldingUnfold:
      begin
        if R.Folded then
        begin
          DoRangeUnfold(R);
          Update;
        end;
      end;
    cFoldingToggle:
      begin
        if R.Folded then
          DoRangeUnfold(R)
        else
          DoRangeFold(R);
        Update;
      end;
  end;
end;

