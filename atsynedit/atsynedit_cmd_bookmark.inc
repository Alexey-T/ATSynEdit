{$ifdef nnnn}begin end;{$endif}

procedure TATSynEdit.DoChangeBookmarks;
begin
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

  FillChar(Data, SizeOf(Data), 0);
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
  NIndex: integer;
  Data: TATBookmarkData;
begin
  if not Strings.IsIndexValid(ALine) then exit;

  NIndex:= Strings.Bookmarks.Find(ALine);
  if NIndex>=0 then
    Strings.Bookmarks.Delete(NIndex)
  else
  begin
    FillChar(Data, SizeOf(Data), 0);
    Data.LineNum:= ALine;
    Data.Kind:= ABmKind;
    Data.Hint:= StrNew(PChar(AHint));
    Data.AutoDelete:= AAutoDelete;
    Data.ShowInBookmarkList:= AShowInList;
    Data.Tag:= ATag;
    Strings.Bookmarks.Add(Data);
  end;

  DoChangeBookmarks;
  Update;
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


procedure TATSynEdit.BookmarkInvertAll;
var
  NIndex, i: integer;
  Data: TATBookmarkData;
begin
  FillChar(Data{%H-}, SizeOf(Data), 0);
  Data.Kind:= 1;
  Data.ShowInBookmarkList:= true;
  Data.AutoDelete:= bmadOption;

  for i:= 0 to Strings.Count-1 do
  begin
    NIndex:= Strings.Bookmarks.Find(i);
    if NIndex>=0 then
      Strings.Bookmarks.Delete(NIndex)
    else
    begin
      Data.LineNum:= i;
      Strings.Bookmarks.Add(Data);
    end;
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
        true
        );
      exit;
    end;
  until false;
end;


procedure TATSynEdit.BookmarkCopyMarkedLines;
var
  List: TStringList;
  NLine, i: integer;
begin
  List:= TStringList.Create;
  try
    for i:= 0 to Strings.Bookmarks.Count-1 do
    begin
      NLine:= Strings.Bookmarks[i]^.Data.LineNum;
      if Strings.IsIndexValid(NLine) then
        List.Add(UTF8Encode(Strings.Lines[NLine]));
    end;
    SClipboardCopy(List.Text);
  finally
    FreeAndNil(List);
  end;
end;


procedure TATSynEdit.BookmarkDeleteMarkedLines;
var
  NCount, NLine, i: integer;
begin
  NCount:= Strings.Bookmarks.Count;
  if NCount=0 then exit;

  for i:= NCount-1 downto 0 do
  begin
    NLine:= Strings.Bookmarks[i]^.Data.LineNum;
    Strings.LineDelete(NLine);
  end;

  DoCaretsFixIncorrectPos(false);
  Update(true);
  DoEventChange;

  if NCount<>Strings.Bookmarks.Count then
    DoChangeBookmarks;
end;


procedure TATSynEdit.BookmarkPlaceCaretsOnBookmarks;
var
  X1, Y1, X2, Y2: integer;
  NLine, i: integer;
begin
  if Carets.Count=0 then exit;
  with Carets[0] do
  begin
    X1:= PosX;
    Y1:= PosY;
    X2:= EndX;
    Y2:= EndY;
  end;

  Carets.Clear;
  for i:= 0 to Strings.Bookmarks.Count-1 do
  begin
    NLine:= Strings.Bookmarks[i]^.Data.LineNum;
    Carets.Add(0, NLine);
  end;

  if Carets.Count=0 then
    DoCaretSingle(X1, Y1, X2, Y2);
end;


procedure TATSynEdit.BookmarkPlaceBookmarksOnCarets;
var
  Caret: TATCaretItem;
  Data: TATBookmarkData;
  i: integer;
begin
  FillChar(Data{%H-}, SizeOf(Data), 0);
  Data.Kind:= 1;
  Data.ShowInBookmarkList:= true;
  Data.AutoDelete:= bmadOption;

  BookmarkDeleteAll(false);
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if Strings.IsIndexValid(Caret.PosY) then
    begin
      Data.LineNum:= Caret.PosY;
      Strings.Bookmarks.Add(Data);
    end;
  end;

  DoChangeBookmarks;
end;

