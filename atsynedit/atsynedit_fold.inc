{$ifdef nn}begin end;{$endif}

function TATSynEdit.IsLineFolded(ALine: integer; ADetectPartialFold: boolean): boolean;
var
  Strs: TATStrings;
begin
  Result:= false;
  Strs:= Strings;
  if not Strs.IsIndexValid(ALine) then Exit;

  if Strs.LinesHidden[ALine, FEditorIndex] then Exit(true);

  if ADetectPartialFold then
    Result:= Strs.LinesFoldFrom[ALine, FEditorIndex]>0;
end;

function TATSynEdit.Unfolded_FirstLineNumber: integer;
begin
  Result:= Unfolded_NextLineNumber(0, true);
end;

function TATSynEdit.Unfolded_LastLineNumber: integer;
begin
  Result:= Unfolded_NextLineNumber(Strings.Count-1, false);
end;

function TATSynEdit.Unfolded_NextLineNumber(ALine: integer; ADown: boolean): integer;
var
  N: integer;
begin
  Result:= ALine;
  N:= Result;
  while IsLineFolded(N) and Strings.IsIndexValid(N) do
    N:= N+BoolToPlusMinusOne[ADown];
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

what we do.
find all _folded_ sub-ranges in given range.
a) for all lines in given range: if line not in sub-range, show it
b) show first lines of found folded sub-ranges
*)
procedure TATSynEdit.DoRangeUnfold(ARangeIndex: integer);
var
  RangeIndexes: TATIntArray;
  RngOuter, Rng: PATSynRange;
  Strs: TATStrings;
  Checks: array of boolean; //take less memory
  Line1, Line2: integer;
  iLine, i, j: integer;
begin
  RngOuter:= Fold.ItemPtr(ARangeIndex);
  RngOuter^.Folded:= false;
  FWrapUpdateNeeded:= true;

  Strs:= Strings;
  RangeIndexes:= FFold.FindRanges(ARangeIndex, true{OnlyFolded}, true{TopLevel});

  //show all lines not in found _folded_ subranges
  Line1:= RngOuter^.Y;
  Line2:= RngOuter^.Y2;
  SetLength(Checks, Line2-Line1+1);

  for i:= 0 to High(Checks) do
    Checks[i]:= false;

  for i:= 0 to High(RangeIndexes) do
  begin
    Rng:= Fold.ItemPtr(RangeIndexes[i]);
    for j:= Rng^.Y to Rng^.Y2 do
      Checks[j-Line1]:= true;
  end;

  for i:= 0 to High(Checks) do
    if not Checks[i] then
    begin
      iLine:= i+Line1;
      Strs.LinesHidden[iLine, FEditorIndex]:= false;
      Strs.LinesFoldFrom[iLine, FEditorIndex]:= 0;
    end;

  //show first lines of found subranges
  for i:= 0 to High(RangeIndexes) do
  begin
    Rng:= Fold.ItemPtr(RangeIndexes[i]);
    Strs.LinesHidden[Rng^.Y, FEditorIndex]:= false;
  end;
end;

procedure TATSynEdit.DoRangeFold(ARangeIndex: integer);
var
  Range: PATSynRange;
begin
  Range:= Fold.ItemPtr(ARangeIndex);
  Range^.Folded:= true;
  FWrapUpdateNeeded:= true;

  //partially hide first line
  case FFoldStyle of
    cFoldHereWithDots:
    begin
      Strings.LinesFoldFrom[Range^.Y, FEditorIndex]:= Range^.X;
    end;
    cFoldHereWithTruncatedText:
    begin
      Strings.LinesFoldFrom[Range^.Y, FEditorIndex]:= Range^.X;
      if Range^.Hint='' then
        Range^.Hint:= Strings.LineSub(Range^.Y, Range^.X, cFoldedLenOfEmptyHint)+'...';
    end;
    cFoldFromEndOfLine:
    begin
      if Range^.Hint<>'' then
        Strings.LinesFoldFrom[Range^.Y, FEditorIndex]:= Range^.X
      else
        Strings.LinesFoldFrom[Range^.Y, FEditorIndex]:= Strings.LinesLen[Range^.Y]+1;
    end;
    cFoldFromEndOfLineAlways:
    begin
      Strings.LinesFoldFrom[Range^.Y, FEditorIndex]:= Strings.LinesLen[Range^.Y]+1;
      Range^.Hint:= '...';
    end;
    cFoldFromNextLine:
    begin
      //don't fold line
    end;
  end;

  //write to Fold list (after changed Range.Folded, Range.Hint)
  //Fold.Items[ARangeIndex]:= Range;

  //fully hide other lines of range
  DoRangeHideLines(Range^.Y+1, Range^.Y2);

  if FOptCaretFixAfterRangeFolded then
    Carets.UpdateAfterRangeFolded(Range^.X, Range^.Y, Range^.Y2);
end;


procedure TATSynEdit.DoRangeHideLines(ALineFrom, ALineTo: integer);
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
  List:= FFold.FindRangesWithLine(ALine, true{OnlyFolded});
  for i:= Low(List) to High(List) do
    DoRangeUnfold(List[i]);
end;

procedure TATSynEdit.DoFoldbarClick(ALine: integer);
var
  N: integer;
begin
  N:= FFold.FindRangeWithPlusAtLine(ALine);
  if N>=0 then
  begin
    if Fold.Items[N].Folded then
      DoRangeUnfold(N)
    else
      DoRangeFold(N);
    Update;
  end;
end;


function TATSynEdit.GetFoldedMarkText(ALine: integer): string;
const
  cDefaultHint = '...';
var
  N: integer;
begin
  Result:= '';

  if FFoldStyle=cFoldFromEndOfLineAlways then
    exit(cDefaultHint);

  N:= FFold.FindRangeWithPlusAtLine(ALine);
  if N>=0 then
    Result:= FFold.Items[N].Hint;
  if Result='' then
    Result:= cDefaultHint;
end;

procedure TATSynEdit.UpdateFoldedFromLinesHidden;
var
  NFoldFrom, NPosAfterEnd: integer;
  NMaxLine, iRange: integer;
  R: PATSynRange;
  bOk: boolean;
begin
  NMaxLine:= Strings.Count-1;
  for iRange:= 0 to Fold.Count-1 do
  begin
    R:= Fold.ItemPtr(iRange);
    if R^.IsSimple then Continue;
    if R^.Y>NMaxLine then Continue; //same as "if not Strings.IsIndexValid"
    if Strings.LinesHidden[R^.Y, FEditorIndex] then Continue;
    NFoldFrom:= Strings.LinesFoldFrom[R^.Y, FEditorIndex];

    NPosAfterEnd:= Strings.LinesLen[R^.Y]+1;
    if (OptFoldStyle in [cFoldFromEndOfLine, cFoldFromEndOfLineAlways]) and
      (NFoldFrom=NPosAfterEnd) then
      bOk:= true
    else
      bOk:= R^.X=NFoldFrom;

    if bOk then
      DoRangeFold(iRange); //do not just R.Folded:= true;
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
  InitFoldImageList;
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
  Details: TATEditorPosDetails;
  Rng: PATSynRange;
  mi: TMenuItem;
  i: integer;
begin
  //calc ranges for curr line
  Pnt:= ScreenToClient(Mouse.CursorPos);
  Pnt:= ClientPosToCaretPos(Pnt, Details);
  NLine:= Pnt.Y;
  if NLine<0 then Exit;

  IntList:= Fold.FindRangesWithLine(NLine, false{OnlyFolded});
  if Length(IntList)=0 then Exit;

  //separator
  mi:= TMenuItem.Create(Self);
  mi.Caption:= '-';
  Menu.Items.Add(mi);

  //items for ranges for current line
  for i:= 0 to High(IntList) do
  begin
    Rng:= Fold.ItemPtr(IntList[i]);
    mi:= TMenuItem.Create(Self);
    mi.Tag:= IntList[i];
    mi.OnClick:= @MenuFoldPlusMinusClick;

    mi.Caption:=
      cTextHintScrollPrefix+' '+Inttostr(Rng^.Y+1)+': '+
      UTF8Encode(Strings.LineSub(Rng^.Y, 1, cFoldedLenOfEmptyHint));

    if Rng^.Folded then
      mi.ImageIndex:= 0
    else
      mi.ImageIndex:= 1;

    Menu.Items.Add(mi);
  end;
end;

procedure TATSynEdit.InitFoldImageList;
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
  N: integer;
begin
  N:= (Sender as TComponent).Tag;
  if not Fold.IsIndexValid(N) then exit;

  if Fold.Items[N].Folded then
    DoRangeUnfold(N)
  else
    DoRangeFold(N);
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


procedure TATSynEdit.DoFoldForLevelEx(ALevel: integer; AOuterRange: integer);
//this func is recursive. it calls itself with ALevel-1.
//it folds ranges if ALevel=0, otherwise it goes to subranges until it finds ALevel=0.
var
  List: TATIntArray;
  R: PATSynRange;
  i: integer;
begin
  if ALevel<0 then exit;

  List:= Fold.FindRanges(AOuterRange, false{OnlyFolded}, true{TopLevel});

  for i:= Low(List) to High(List) do
  begin
    R:= Fold.ItemPtr(List[i]);
    if R^.IsSimple then Continue;
    if R^.Folded then Continue;
    if ALevel=0 then
      DoRangeFold(List[i])
    else
      DoFoldForLevelEx(ALevel-1, List[i]);
  end;
end;

procedure TATSynEdit.DoFoldForLevel(ALevel: integer);
begin
  DoCommand_FoldUnAll;
  DoFoldForLevelEx(ALevel, -1);
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


procedure TATSynEdit.DoFoldUnfoldRangeAtCurLine(AOp: TATEditorFoldRangeCommand);
var
  NLine, NRange: integer;
  bFolded: boolean;
begin
  if Carets.Count<>1 then exit;
  NLine:= Carets[0].PosY;
  if not Strings.IsIndexValid(NLine) then exit;

  NRange:= Fold.FindDeepestRangeContainingLine(NLine, false{AWithStaple});
  if NRange<0 then exit;
  bFolded:= Fold.Items[NRange].Folded;

  case AOp of
    cFoldingFold:
      begin
        if not bFolded then
        begin
          DoRangeFold(NRange);
          Update;
        end;
      end;
    cFoldingUnfold:
      begin
        if bFolded then
        begin
          DoRangeUnfold(NRange);
          Update;
        end;
      end;
    cFoldingToggle:
      begin
        if bFolded then
          DoRangeUnfold(NRange)
        else
          DoRangeFold(NRange);
        Update;
      end;
  end;
end;

