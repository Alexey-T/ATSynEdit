{$ifdef nnnn}begin end;{$endif}

procedure TATSynEdit.MarkerDrop;
var
  Caret: TATCaretItem;
begin
  if Carets.Count<>1 then exit;
  Caret:= Carets[0];
  InitMarkers;
  FMarkers.Add(Caret.PosX, Caret.PosY);
  Update;
end;

procedure TATSynEdit.MarkerGotoLast(AndDelete: boolean;
  AIndentHorz, AIndentVert: integer);
  //
var
  Mark: TATMarkerItem;
  P1, P2: TPoint;
  NTag, iMarker: integer;
begin
  if FMarkers=nil then exit;
  if FMarkers.Count=0 then exit;
  Mark:= FMarkers[FMarkers.Count-1];
  NTag:= Mark.Tag;

  P1.X:= Mark.PosX;
  P1.Y:= Mark.PosY;
  P2:= Mark.SelEnd;

  if not IsPosSorted(P1.X, P1.Y, P2.X, P2.Y, false) then
    SwapPoints(P1, P2);

  DoGotoPos(
    P2,
    P1,
    AIndentHorz,
    AIndentVert,
    true,
    true
    );

  if AndDelete then
  begin
    FMarkers.Delete(FMarkers.Count-1);

    //Tag>0: delete also same tag marks
    //and place mul-carets
    if NTag>0 then
      for iMarker:= FMarkers.Count-1 downto 0 do
      begin
        Mark:= FMarkers[iMarker];
        if Mark.Tag=NTag then
        begin
          if (Mark.SelX<=0) and (Mark.SelY<=0) then
            Carets.Add(Mark.PosX, Mark.PosY)
          else
          begin
            P1.X:= Mark.PosX;
            P1.Y:= Mark.PosY;
            P2:= Mark.SelEnd;

            if not IsPosSorted(P1.X, P1.Y, P2.X, P2.Y, false) then
              SwapPoints(P1, P2);

            Carets.Add(P2.X, P2.Y, P1.X, P1.Y);
          end;
          FMarkers.Delete(iMarker);
        end;
      end;
  end;

  DoCaretsSort; //must have here
  Update;
end;

procedure TATSynEdit.MarkerClearAll;
begin
  if Assigned(FMarkers) then
  begin
    FMarkers.Clear;
    Update;
  end;
end;

procedure TATSynEdit.MarkerSwap;
var
  Caret: TATCaretItem;
  Mark: TATMarkerItem;
  PX, PY: integer;
begin
  if Carets.Count<>1 then exit;
  if FMarkers=nil then exit;
  if FMarkers.Count=0 then exit;
  Caret:= Carets[0];
  Mark:= FMarkers[FMarkers.Count-1];

  PX:= Caret.PosX;
  PY:= Caret.PosY;
  Caret.PosX:= Mark.PosX;
  Caret.PosY:= Mark.PosY;
  Mark.PosX:= PX;
  Mark.PosY:= PY;
  FMarkers[FMarkers.Count-1] := Mark;

  DoGotoCaret(cEdgeTop);
  Update;
end;

procedure TATSynEdit.MarkerSelectToCaret;
var
  Caret: TATCaretItem;
  Mark: TATMarkerItem;
begin
  if Carets.Count<>1 then exit;
  if FMarkers=nil then exit;
  if FMarkers.Count=0 then exit;
  Caret:= Carets[0];
  Mark:= FMarkers[FMarkers.Count-1];

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
  if FMarkers=nil then exit;
  if FMarkers.Count=0 then exit;
  Caret:= Carets[0];
  Mark:= FMarkers[FMarkers.Count-1];

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
  FMarkers.Delete(FMarkers.Count-1);

  Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
  Update;
  DoEventChange;
end;

