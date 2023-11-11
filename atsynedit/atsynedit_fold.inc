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
  St: TATStrings;
  N: integer;
begin
  Result:= ALine;
  N:= Result;
  St:= Strings;
  while IsLineFolded(N) and St.IsIndexValid(N) do
    if ADown then
      Inc(N)
    else
      Dec(N);
  if St.IsIndexValid(N) then
    Result:= N;
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
  Checks:= nil;
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
  St: TATStrings;
  Range: PATSynRange;
begin
  St:= Strings;
  Range:= Fold.ItemPtr(ARangeIndex);
  Range^.Folded:= true;
  FWrapUpdateNeeded:= true;

  //partially hide first line
  case FFoldStyle of
    TATEditorFoldStyle.HereWithDots:
      begin
        St.LinesFoldFrom[Range^.Y, FEditorIndex]:= Range^.X;
      end;
    TATEditorFoldStyle.HereWithTruncatedText:
      begin
        St.LinesFoldFrom[Range^.Y, FEditorIndex]:= Range^.X;
        if Range^.Hint='' then
          Range^.Hint:= St.LineSub(Range^.Y, Range^.X, ATEditorOptions.FoldedLenOfEmptyHint)+'...';
      end;
    TATEditorFoldStyle.FromEndOfLine:
      begin
        if Range^.Hint<>'' then
          St.LinesFoldFrom[Range^.Y, FEditorIndex]:= Range^.X
        else
          St.LinesFoldFrom[Range^.Y, FEditorIndex]:= St.LinesLen[Range^.Y]+1;
      end;
    TATEditorFoldStyle.FromEndOfLineAlways:
      begin
        St.LinesFoldFrom[Range^.Y, FEditorIndex]:= St.LinesLen[Range^.Y]+1;
        Range^.Hint:= '...';
      end;
    TATEditorFoldStyle.FromNextLine:
      begin
        //don't fold line
      end;
  end;

  //fully hide other lines of range
  DoRangeHideLines(Range^.Y+1, Range^.Y2);

  if FOptCaretFixAfterRangeFolded then
    Carets.UpdateAfterRangeFolded(Range^.X, Range^.Y, Range^.Y2);
end;


procedure TATSynEdit.DoRangeHideLines(ALineFrom, ALineTo: integer);
var
  St: TATStrings;
  i: integer;
begin
  St:= Strings;
  for i:= ALineFrom to ALineTo do
    St.LinesHidden[i, FEditorIndex]:= true;
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

procedure TATSynEdit.DoFoldbarClick_LineIndex(ALine: integer);
var
  NRange: integer;
begin
  NRange:= FFold.FindRangeWithPlusAtLine(ALine);
  if NRange>=0 then
    DoFoldbarClick_RangeIndex(NRange);
end;

procedure TATSynEdit.DoFoldbarClick_RangeIndex(ARangeIndex: integer);
var
  FoldItemPtr: PATSynRange;
  bUnfold: boolean;
  bScrolledToLastPage: boolean;
begin
  if not FFold.IsIndexValid(ARangeIndex) then exit;
  FoldItemPtr:= FFold.ItemPtr(ARangeIndex);
  bUnfold:= FoldItemPtr^.Folded;
  if bUnfold then
    DoRangeUnfold(ARangeIndex)
  else
    DoRangeFold(ARangeIndex);

  Update;

  //force repaint, if after 'fold' editor has empty space below the last line,
  //because next click on gutter-fold-mark will be ignored. CudaText issue #4850
  if not FOptLastLineOnTop and not bUnfold then
  begin
    bScrolledToLastPage:= FScrollVert.NPos>=FScrollVert.NPosLast;

    //for CudaText issue #5180
    //we folded a big range, which ends on the last page?
    if not bScrolledToLastPage then
      if (FWrapInfo.IsIndexValid(FScrollVert.NPosLast)) and
        (FoldItemPtr^.Y2>=FWrapInfo.Data[FScrollVert.NPosLast].NLineIndex) then
        bScrolledToLastPage:= true;

    if bScrolledToLastPage then
    begin
      Application.ProcessMessages;
      Update;
    end;
  end;
end;


function TATSynEdit.GetFoldedMarkText(ALine: integer): string;
const
  cDefaultHint = '...';
var
  N: integer;
begin
  Result:= '';

  if FFoldStyle=TATEditorFoldStyle.FromEndOfLineAlways then
    exit(cDefaultHint);

  N:= FFold.FindRangeWithPlusAtLine(ALine);
  if N>=0 then
    Result:= FFold.ItemPtr(N)^.Hint;
  if Result='' then
    Result:= cDefaultHint;
end;

procedure TATSynEdit.UpdateFoldedFromLinesHidden;
var
  St: TATStrings;
  NFoldFrom, NPosAfterEnd: integer;
  NMaxLine, iRange: integer;
  R: PATSynRange;
  bFold: boolean;
begin
  St:= Strings;
  NMaxLine:= St.Count-1;
  for iRange:= 0 to Fold.Count-1 do
  begin
    R:= Fold.ItemPtr(iRange);
    if R^.IsSimple then Continue;
    if R^.Y>NMaxLine then Continue; //same as "if not St.IsIndexValid"
    if St.LinesHidden[R^.Y, FEditorIndex] then Continue;

    bFold:= false;
    NFoldFrom:= St.LinesFoldFrom[R^.Y, FEditorIndex];

    case OptFoldStyle of
      TATEditorFoldStyle.FromEndOfLine:
        begin
          if R^.Hint='' then
          begin
            NPosAfterEnd:= St.LinesLen[R^.Y]+1;
            bFold:= NFoldFrom=NPosAfterEnd;
          end
          else
          begin
            bFold:= R^.X=NFoldFrom;
          end;
        end;
      TATEditorFoldStyle.FromEndOfLineAlways:
        begin
          NPosAfterEnd:= St.LinesLen[R^.Y]+1;
          bFold:= NFoldFrom=NPosAfterEnd;
        end;
      TATEditorFoldStyle.FromNextLine:
        begin
          bFold:= (R^.Y+1<=NMaxLine) and St.LinesHidden[R^.Y+1, FEditorIndex];
        end
      else
        bFold:= R^.X=NFoldFrom;
    end;

    if bFold then
      DoRangeFold(iRange); //do not just R.Folded:= true;
  end;
end;

function TATSynEdit.IsFoldingUnderlineNeededForWrapitem(AWrapIndex: integer): boolean;
var
  NLineCur, NLineNext: integer;
begin
  if FWrapInfo.IsIndexValid(AWrapIndex) then
  begin
    NLineCur:= FWrapInfo[AWrapIndex].NLineIndex;
    if FWrapInfo.IsIndexValid(AWrapIndex+1) then
      NLineNext:= FWrapInfo[AWrapIndex+1].NLineIndex
    else
      NLineNext:= Strings.Count;

    Result:=
      //after this line, some line(s) are skipped
      (NLineNext-NLineCur > 1) and
      //and line is fully visible
      (Strings.LinesFoldFrom[NLineCur, FEditorIndex]=0);
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
  mi.Caption:= ATEditorOptions.TextMenuitemFoldAll;
  mi.OnClick:= @MenuFoldFoldAllClick;
  mi.Enabled:= Fold.Count>0;
  Menu.Items.Add(mi);

  mi:= TMenuItem.Create(Self);
  mi.Caption:= ATEditorOptions.TextMenuitemUnfoldAll;
  mi.OnClick:= @MenuFoldUnfoldAllClick;
  mi.Enabled:= Fold.Count>0;
  Menu.Items.Add(mi);

  //submenu "fold level"
  miSub:= TMenuItem.Create(Self);
  miSub.Caption:= ATEditorOptions.TextMenuitemFoldLevel;
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
  PntCoord: TATPoint;
  NLine: integer;
  IntList: TATIntArray;
  Details: TATEditorPosDetails;
  Rng: PATSynRange;
  mi: TMenuItem;
  i: integer;
begin
  //calc ranges for curr line
  Pnt:= ScreenToClient(Mouse.CursorPos);
  PntCoord.X:= Pnt.X;
  PntCoord.Y:= Pnt.Y;
  Pnt:= ClientPosToCaretPos(PntCoord, Details);
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
      ATEditorOptions.TextHintScrollPrefix+' '+IntToStr(Rng^.Y+1)+': '+
      UTF8Encode(Strings.LineSub(Rng^.Y, 1, ATEditorOptions.FoldedLenOfEmptyHint));

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
    FFoldImageList.Width:= ATEditorBitmaps.BitmapFoldPlus.Width;
    FFoldImageList.Height:= ATEditorBitmaps.BitmapFoldPlus.Height;
    FFoldImageList.Add(ATEditorBitmaps.BitmapFoldPlus, nil);
    FFoldImageList.Add(ATEditorBitmaps.BitmapFoldMinus, nil);
  end;
end;


procedure TATSynEdit.MenuFoldPlusMinusClick(Sender: TObject);
var
  NRange: integer;
begin
  NRange:= (Sender as TComponent).Tag;
  if Fold.IsIndexValid(NRange) then
    DoFoldbarClick_RangeIndex(NRange);
end;

procedure TATSynEdit.MenuFoldFoldAllClick(Sender: TObject);
begin
  DoCommand(cCommand_FoldAll, TATCommandInvoke.MenuContext);
end;

procedure TATSynEdit.MenuFoldLevelClick(Sender: TObject);
begin
  DoFoldForLevel((Sender as TComponent).Tag);
end;

procedure TATSynEdit.MenuFoldUnfoldAllClick(Sender: TObject);
begin
  DoCommand(cCommand_UnfoldAll, TATCommandInvoke.MenuContext);
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
    DoCommand(cCommand_UnfoldAll, TATCommandInvoke.Internal);
    FFold.Clear;
  end;
  Update;
end;


function TATSynEdit.DoFoldUnfoldRangeAtCurLine(AOp: TATEditorFoldRangeCommand): boolean;
var
  X, Y, X2, Y2, NRange, iCaret: integer;
  bSel, bFolded: boolean;
begin
  Result:= false;
  for iCaret:= Carets.Count-1 downto 0 do
  begin
    Carets[iCaret].GetRange(X, Y, X2, Y2, bSel);
    if bSel then Continue;
    if not Strings.IsIndexValid(Y) then Continue;

    NRange:= Fold.FindDeepestRangeContainingLine(Y, false{AWithStaple}, FFoldIconForMinimalRange);
    if NRange<0 then Continue;
    bFolded:= Fold.ItemPtr(NRange)^.Folded;

    case AOp of
      TATEditorFoldRangeCommand.Fold:
        begin
          if not bFolded then
          begin
            DoRangeFold(NRange);
            Update;
            Result:= true;
          end;
        end;
      TATEditorFoldRangeCommand.Unfold:
        begin
          if bFolded then
          begin
            DoRangeUnfold(NRange);
            Update;
            Result:= true;
          end;
        end;
      TATEditorFoldRangeCommand.Toggle:
        begin
          if bFolded then
            DoRangeUnfold(NRange)
          else
            DoRangeFold(NRange);
          Update;
          Result:= true;
        end;
    end;
  end;
end;


procedure TATSynEdit.DoUnfoldLinesWithCarets;
var
  iCaret: integer;
begin
  for iCaret:= Carets.Count-1 downto 0 do
    DoUnfoldLine(Carets[iCaret].PosY);
end;


function TATSynEdit.IsPosInFoldMark(APos: TPoint): boolean;
begin
  Result:= Assigned(FFoldedMarkList) and
    FFoldedMarkList.FindByCoord(APos).IsInited;
end;

