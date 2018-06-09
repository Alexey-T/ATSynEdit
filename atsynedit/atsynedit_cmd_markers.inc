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
  NTag, i: integer;
begin
  if Markers.Count=0 then exit;
  Mark:= Markers[Markers.Count-1];

  X1:= Mark.PosX;
  Y1:= Mark.PosY;
  X2:= -1;
  Y2:= -1;

  if Mark.LenY<=0 then
  begin
    //LenX is selection len (1-line)
    if Mark.LenX>0 then
    begin
      X2:= X1+Mark.LenX;
      Y2:= Y1;
    end;
  end
  else
  begin
    //LenX is selection end X-pos;
    //LenY is count of sel lines
    X2:= Mark.LenX;
    Y2:= Y1+Mark.LenY;
  end;

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
          if Mark.LenY>0 then
            Carets.Add(Mark.LenX, Mark.PosY+Mark.LenY, Mark.PosX, Mark.PosY)
          else
          if Mark.LenX<=0 then
            Carets.Add(Mark.PosX, Mark.PosY)
          else
            Carets.Add(Mark.PosX+Mark.LenX, Mark.PosY, Mark.PosX, Mark.PosY);
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

