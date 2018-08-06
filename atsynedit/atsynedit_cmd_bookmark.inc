{$ifdef nnnn}begin end;{$endif}

procedure TATSynEdit.BookmarkSetForLine(ALine, ABmKind: integer;
  const AHint: string;
  ADeleteOnDelLine, AShowInList: boolean;
  const ATag: Int64);
begin
  if not Strings.IsIndexValid(ALine) then exit;
  Strings.Bookmarks.Add(
    ALine,
    ABmKind,
    AHint,
    ADeleteOnDelLine,
    AShowInList,
    ATag
    );
  Update;
end;


procedure TATSynEdit.BookmarkToggleForLine(ALine, ABmKind: integer;
  const AHint: string;
  ADeleteOnDelLine, AShowInList: boolean;
  const ATag: Int64);
var
  NIndex: integer;
begin
  if not Strings.IsIndexValid(ALine) then exit;

  NIndex:= Strings.Bookmarks.Find(ALine);
  if NIndex>=0 then
    Strings.Bookmarks.Delete(NIndex)
  else
    Strings.Bookmarks.Add(
      ALine,
      ABmKind,
      AHint,
      ADeleteOnDelLine,
      AShowInList,
      ATag
      );

  Update;
end;


procedure TATSynEdit.BookmarkDeleteForLine(ALine: integer);
begin
  if not Strings.IsIndexValid(ALine) then exit;
  Strings.Bookmarks.DeleteForLine(ALine);
  Update;
end;

function TATSynEdit.BookmarkDeleteByTag(const ATag: Int64): boolean;
begin
  Result:= Strings.Bookmarks.DeleteByTag(ATag);
  if Result then
    Update;
end;


procedure TATSynEdit.BookmarkInvertAll;
var
  NIndex, i: integer;
begin
  for i:= 0 to Strings.Count-1 do
  begin
    NIndex:= Strings.Bookmarks.Find(i);
    if NIndex>=0 then
      Strings.Bookmarks.Delete(NIndex)
    else
      Strings.Bookmarks.Add(i, 1, '', false);
  end;
  Update;
end;

procedure TATSynEdit.BookmarkDeleteAll;
begin
  Strings.Bookmarks.Clear;
  Update;
end;


procedure TATSynEdit.BookmarkGotoNext(ANext: boolean; AIndentHorz,
  AIndentVert: integer; AOnlyShownInList: boolean);
var
  nLine, nLineOrig, nItem: integer;
  bm: TATBookmarkItem;
begin
  nLine:= Carets[0].PosY;
  nLineOrig:= nLine;
  repeat
    if ANext then Inc(nLine) else Dec(nLine);
    if nLine=nLineOrig then exit;

    if nLine>=Strings.Count then
      nLine:= 0;
    if nLine<0 then
      nLine:= Strings.Count-1;

    nItem:= Strings.Bookmarks.Find(nLine);
    if (nItem>=0) and
      (not AOnlyShownInList or Strings.Bookmarks[nItem].ShowInBookmarkList) then
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
      NLine:= Strings.Bookmarks[i].LineNum;
      if Strings.IsIndexValid(NLine) then
        List.Add(Strings.LinesUTF8[NLine]);
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
    NLine:= Strings.Bookmarks[i].LineNum;
    Strings.LineDelete(NLine);
  end;

  UpdateIncorrectCaretPositions;
  Update(true);
  DoEventChange;
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
    NLine:= Strings.Bookmarks[i].LineNum;
    Carets.Add(0, NLine);
  end;

  if Carets.Count=0 then
    DoCaretSingle(X1, Y1, X2, Y2);
end;


procedure TATSynEdit.BookmarkPlaceBookmarksOnCarets;
var
  Caret: TATCaretItem;
  i: integer;
begin
  BookmarkDeleteAll;
  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    if Strings.IsIndexValid(Caret.PosY) then
      Strings.Bookmarks.Add(Caret.PosY, 1, '', false);
  end;
end;

