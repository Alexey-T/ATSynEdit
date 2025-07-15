{$ifdef nnnn}begin end;{$endif}

procedure TATSynEdit.DoChangeBookmarks;
begin
  ModifiedBookmarks:= true;
  if Assigned(FOnChangeBookmarks) then
    FOnChangeBookmarks(Self);
end;

procedure TATSynEdit.BookmarkSetForLineEx(ALine, ABmKind: integer;
  const AHint: string; AAutoDelete: TATBookmarkAutoDelete; AShowInList: boolean; const ATag: Int64;
  ABookmarksObj: TATBookmarks);
var
  Data: TATBookmarkData;
begin
  if not Strings.IsIndexValid(ALine) then exit;

  Data:= Default(TATBookmarkData);
  Data.LineNum:= ALine;
  Data.Kind:= ABmKind;
  Data.Hint:= StrNew(PChar(AHint));
  Data.AutoDelete:= AAutoDelete;
  Data.ShowInBookmarkList:= AShowInList;
  Data.Tag:= ATag;
  ABookmarksObj.Add(Data);

  DoChangeBookmarks;
  Update;
end;

procedure TATSynEdit.BookmarkSetForLine(ALine, ABmKind: integer;
  const AHint: string; AAutoDelete: TATBookmarkAutoDelete;
  AShowInList: boolean; const ATag: Int64);
begin
  BookmarkSetForLineEx(
    ALine,
    ABmKind,
    AHint,
    AAutoDelete,
    AShowInList,
    ATag,
    Strings.Bookmarks
    );
end;

procedure TATSynEdit.BookmarkSetForLine_2(ALine, ABmKind: integer;
  const AHint: string; AAutoDelete: TATBookmarkAutoDelete;
  AShowInList: boolean; const ATag: Int64);
begin
  BookmarkSetForLineEx(
    ALine,
    ABmKind,
    AHint,
    AAutoDelete,
    AShowInList,
    ATag,
    Strings.Bookmarks2
    );
end;


procedure TATSynEdit.BookmarkToggleForLine(ALine, ABmKind: integer;
  const AHint: string; AAutoDelete: TATBookmarkAutoDelete;
  AShowInList: boolean; const ATag: Int64);
var
  St: TATStrings;
  NIndex: integer;
  Data: TATBookmarkData;
begin
  St:= Strings;
  if not St.IsIndexValid(ALine) then exit;

  NIndex:= St.Bookmarks.Find(ALine);
  if NIndex>=0 then
    St.Bookmarks.Delete(NIndex)
  else
  begin
    Data:= Default(TATBookmarkData);
    Data.LineNum:= ALine;
    Data.Kind:= ABmKind;
    Data.Hint:= StrNew(PChar(AHint));
    Data.AutoDelete:= AAutoDelete;
    Data.ShowInBookmarkList:= AShowInList;
    Data.Tag:= ATag;
    St.Bookmarks.Add(Data);
  end;

  DoChangeBookmarks;
  Update;
end;


procedure TATSynEdit.BookmarkToggleForCarets(ABmKind: integer; AShowInList: boolean; const ATag: Int64);
var
  Caret: TATCaretItem;
  iCaret, NLine, NLinePrev: integer;
begin
  NLinePrev:= -1;
  for iCaret:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[iCaret];
    NLine:= Caret.PosY;
    if NLine=NLinePrev then Continue;
    NLinePrev:= NLine;
    BookmarkToggleForLine(NLine, ABmKind, '', TATBookmarkAutoDelete.ByOption, AShowInList, ATag);
  end;
end;


procedure TATSynEdit.BookmarkDeleteForLineEx(ALine: integer; ABookmarksObj: TATBookmarks);
begin
  if not Strings.IsIndexValid(ALine) then exit;
  ABookmarksObj.DeleteForLine(ALine);

  DoChangeBookmarks;
  Update;
end;

procedure TATSynEdit.BookmarkDeleteForLine(ALine: integer);
begin
  BookmarkDeleteForLineEx(ALine, Strings.Bookmarks);
end;

procedure TATSynEdit.BookmarkDeleteForLine_2(ALine: integer);
begin
  BookmarkDeleteForLineEx(ALine, Strings.Bookmarks2);
end;

function TATSynEdit.BookmarkDeleteByTagEx(const ATag: Int64;
  ABookmarksObj: TATBookmarks): boolean;
begin
  Result:= ABookmarksObj.DeleteByTag(ATag);
  if Result then
  begin
    DoChangeBookmarks;
    Update;
  end;
end;

function TATSynEdit.BookmarkDeleteByTag(const ATag: Int64): boolean;
begin
  Result:= BookmarkDeleteByTagEx(ATag, Strings.Bookmarks);
end;

function TATSynEdit.BookmarkDeleteByTag_2(const ATag: Int64): boolean;
begin
  Result:= BookmarkDeleteByTagEx(ATag, Strings.Bookmarks2);
end;


function _FindInListOfNumbers(L: TFPList; AValue: PtrInt): integer;
var
  a, b, m: integer;
  dif: PtrInt;
begin
  Result:= -1;
  a:= 0;
  b:= L.Count-1;

  repeat
    if a>b then exit;

    m:= (a+b+1) div 2;
    dif:= PtrInt(L[m])-AValue;
    if dif=0 then
      exit(m)
    else
    if dif>0 then
      b:= m-1
    else
      a:= m+1;
  until false;
end;

procedure TATSynEdit.BookmarkInvertAll;
var
  St: TATStrings;
  Data: TATBookmarkData;
  L: TFPList;
  i: integer;
begin
  St:= Strings;

  Data:= Default(TATBookmarkData);
  Data.Kind:= 1;
  Data.ShowInBookmarkList:= true;
  Data.AutoDelete:= TATBookmarkAutoDelete.ByOption;

  L:= TFPList.Create;
  try
    for i:= 0 to St.Bookmarks.Count-1 do
      L.Add(pointer(PtrInt(St.Bookmarks.ItemPtr[i]^.Data.LineNum)));

    St.Bookmarks.Clear;

    for i:= 0 to St.Count-1 do
      if _FindInListOfNumbers(L, i)<0 then
      begin
        Data.LineNum:= i;
        St.Bookmarks.Add(Data);
      end
  finally
    FreeAndNil(L);
  end;

  DoChangeBookmarks;
  Update;
end;

procedure TATSynEdit.BookmarkDeleteAll(AWithEvent: boolean=true);
begin
  if Strings.Bookmarks.Count>0 then
  begin
    Strings.Bookmarks.Clear;

    if AWithEvent then
      DoChangeBookmarks;
    Update;
  end;
end;

procedure TATSynEdit.BookmarkDeleteAll_2;
begin
  if Strings.Bookmarks2.Count>0 then
  begin
    Strings.Bookmarks2.Clear;

    DoChangeBookmarks;
    Update;
  end;
end;


procedure TATSynEdit.BookmarkGotoNext(ANext: boolean; AIndentHorz,
  AIndentVert: integer; AOnlyShownInList: boolean);
var
  St: TATStrings;
  nLine, nLineOrig, nItem: integer;
begin
  St:= Strings;
  if St.Count<=1 then exit;
  if St.Bookmarks.Count=0 then exit;
  if Carets.Count=0 then exit;
  nLine:= Carets[0].PosY;
  nLineOrig:= nLine;
  repeat
    if ANext then Inc(nLine) else Dec(nLine);
    if nLine=nLineOrig then exit;

    if nLine>=St.Count then
      nLine:= 0;
    if nLine<0 then
      nLine:= St.Count-1;

    nItem:= St.Bookmarks.Find(nLine);
    if (nItem>=0) and
      (not AOnlyShownInList or St.Bookmarks[nItem]^.Data.ShowInBookmarkList) then
    begin
      DoGotoPos(
        Point(0, nLine),
        Point(-1, -1),
        AIndentHorz,
        AIndentVert,
        true,
        TATEditorActionIfFolded.Unfold
        );
      exit;
    end;
  until false;
end;


procedure TATSynEdit.BookmarkCopyMarkedLines;
var
  St: TATStrings;
  List: TStringList;
  NLine, i: integer;
begin
  St:= Strings;
  List:= TStringList.Create;
  try
    for i:= 0 to St.Bookmarks.Count-1 do
    begin
      NLine:= St.Bookmarks[i]^.Data.LineNum;
      if St.IsIndexValid(NLine) then
        List.Add(UTF8Encode(St.Lines[NLine]));
    end;
    SClipboardCopy(List.Text);
  finally
    FreeAndNil(List);
  end;
end;


procedure TATSynEdit.BookmarkDeleteMarkedLines;
var
  St: TATStrings;
  NCount, NLine, i: integer;
begin
  St:= Strings;
  NCount:= St.Bookmarks.Count;
  if NCount=0 then exit;

  for i:= NCount-1 downto 0 do
  begin
    NLine:= St.Bookmarks[i]^.Data.LineNum;
    St.LineDelete(NLine);
  end;

  DoCaretsFixIncorrectPos(false);
  Update(true);
  DoEventChange;

  if NCount<>St.Bookmarks.Count then
    DoChangeBookmarks;
end;


procedure TATSynEdit.BookmarkPlaceCaretsOnBookmarks;
var
  St: TATStrings;
  X1, Y1, X2, Y2: integer;
  NLine, i: integer;
begin
  St:= Strings;
  if Carets.Count=0 then exit;
  with Carets[0] do
  begin
    X1:= PosX;
    Y1:= PosY;
    X2:= EndX;
    Y2:= EndY;
  end;

  Carets.Clear;
  for i:= 0 to St.Bookmarks.Count-1 do
  begin
    NLine:= St.Bookmarks[i]^.Data.LineNum;
    Carets.Add(0, NLine, -1, -1, false{AWithEvent});
  end;

  if Carets.Count=0 then
    DoCaretSingle(X1, Y1, X2, Y2)
  else
    Carets.DoChanged;
end;


procedure TATSynEdit.BookmarkPlaceBookmarksOnCarets;
var
  St: TATStrings;
  Caret: TATCaretItem;
  Data: TATBookmarkData;
  i: integer;
begin
  Data:= Default(TATBookmarkData);
  Data.Kind:= 1;
  Data.ShowInBookmarkList:= true;
  Data.AutoDelete:= TATBookmarkAutoDelete.ByOption;

  St:= Strings;
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if St.IsIndexValid(Caret.PosY) then
      if St.Bookmarks.Find(Caret.PosY)=-1 then
      begin
        Data.LineNum:= Caret.PosY;
        St.Bookmarks.Add(Data);
      end;
  end;

  DoChangeBookmarks;
end;

