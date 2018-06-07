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

