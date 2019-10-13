{$ifdef nnnn}begin end;{$endif}

procedure TATSynEdit.MarkerDrop;
var
  Caret: TATCaretItem;
begin
  if Carets.Count<>1 then exit;
  Caret:= Carets[0];
  Markers.Add(Caret.PosX, Caret.PosY);
  Update;
end;

procedure TATSynEdit.MarkerGotoLast(AndDelete: boolean;
  AIndentHorz, AIndentVert: integer);
var
  Mark: TATMarkerItem;
  X1, Y1, X2, Y2: integer;
  Pnt: TPoint;
  NTag, i: integer;
begin
  if Markers.Count=0 then exit;
  Mark:= Markers[Markers.Count-1];

  X1:= Mark.PosX;
  Y1:= Mark.PosY;

  Pnt:= Mark.PosEnd;
  X2:= Pnt.X;
  Y2:= Pnt.Y;

  DoGotoPos(
    Point(X1, Y1),
    Point(X2, Y2),
    AIndentHorz,
    AIndentVert,
    true,
    true
    );

  if AndDelete then
  begin
    NTag:= Markers[Markers.Count-1].Tag;
    Markers.Delete(Markers.Count-1);

    //Tag>0: delete also same tag marks
    //and place mul-carets
    if NTag>0 then
      for i:= Markers.Count-1 downto 0 do
      begin
        Mark:= Markers[i];
        if Mark.Tag=NTag then
        begin
          Pnt:= Mark.PosEnd;
          if (Mark.LenX<=0) and (Mark.LenY<=0) then
            Carets.Add(Mark.PosX, Mark.PosY)
          else
            Carets.Add(Pnt.X, Pnt.Y, Mark.PosX, Mark.PosY);
          Markers.Delete(i);
        end;
      end;
  end;

  Update;
end;

procedure TATSynEdit.MarkerClearAll;
begin
  Markers.Clear;
  Update;
end;

procedure TATSynEdit.MarkerSwap;
var
  Caret: TATCaretItem;
  Mark: TATMarkerItem;
  PX, PY: integer;
begin
  if Carets.Count<>1 then exit;
  if Markers.Count=0 then exit;
  Caret:= Carets[0];
  Mark:= Markers[Markers.Count-1];

  PX:= Caret.PosX;
  PY:= Caret.PosY;
  Caret.PosX:= Mark.PosX;
  Caret.PosY:= Mark.PosY;
  Mark.PosX:= PX;
  Mark.PosY:= PY;
  Markers[Markers.Count-1] := Mark;

  DoGotoCaret(cEdgeTop);
  Update;
end;

procedure TATSynEdit.MarkerSelectToCaret;
var
  Caret: TATCaretItem;
  Mark: TATMarkerItem;
begin
  if Carets.Count<>1 then exit;
  if Markers.Count=0 then exit;
  Caret:= Carets[0];
  Mark:= Markers[Markers.Count-1];

  Caret.EndX:= Mark.PosX;
  Caret.EndY:= Mark.PosY;
  Update;
end;

procedure TATSynEdit.MarkerDeleteToCaret;
var
  Caret: TATCaretItem;
  Mark: TATMarkerItem;
  X1, Y1, X2, Y2: integer;
  Shift, PosAfter: TPoint;
begin
  if Carets.Count<>1 then exit;
  if Markers.Count=0 then exit;
  Caret:= Carets[0];
  Mark:= Markers[Markers.Count-1];

  X1:= Caret.PosX;
  Y1:= Caret.PosY;
  X2:= Mark.PosX;
  Y2:= Mark.PosY;

  if not IsPosSorted(X1, Y1, X2, Y2, true) then
  begin
    SwapInt(X1, X2);
    SwapInt(Y1, Y2);
  end;

  Caret.PosX:= X1;
  Caret.PosY:= Y1;
  Markers.Delete(Markers.Count-1);

  Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
  Update;
  DoEventChange;
end;

