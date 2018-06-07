{$ifdef nnnn}begin end;{$endif}

procedure TATSynEdit.BookmarkSetForLine(ALine, ABmKind: integer;
  const AHint: string; ADeleteOnDelLine: boolean);
begin
  if not Strings.IsIndexValid(ALine) then exit;
  Strings.Bookmarks.Add(ALine, ABmKind, AHint, ADeleteOnDelLine);
  Update;
end;


procedure TATSynEdit.BookmarkToggleForLine(ALine, ABmKind: integer;
  const AHint: string; ADeleteOnDelLine: boolean);
var
  NIndex: integer;
begin
  if not Strings.IsIndexValid(ALine) then exit;

  NIndex:= Strings.Bookmarks.Find(ALine);
  if NIndex>=0 then
    Strings.Bookmarks.Delete(NIndex)
  else
    Strings.Bookmarks.Add(ALine, ABmKind, AHint, ADeleteOnDelLine);

  Update;
end;


procedure TATSynEdit.BookmarkDeleteForLine(ALine: integer);
begin
  if not Strings.IsIndexValid(ALine) then exit;
  Strings.Bookmarks.DeleteForLine(ALine);
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


procedure TATSynEdit.BookmarkGotoNext(ANext: boolean;
  AIndentHorz, AIndentVert: integer);
var
  n, nFrom: integer;
begin
  n:= Carets[0].PosY;
  nFrom:= n;
  repeat
    if ANext then Inc(n) else Dec(n);
    if n=nFrom then exit;

    if n>=Strings.Count then n:= 0;
    if n<0 then n:= Strings.Count-1;

    if Strings.Bookmarks.Find(n)>=0 then
    begin
      DoGotoPos(
        Point(0, n),
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

