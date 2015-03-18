{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_ToggleOvr: TATCommandResults;
begin
  ModeOvr:= not ModeOvr;
  Result:= [cResultState];
end;

function TATSynEdit.DoCommand_ToggleReadOnly: TATCommandResults;
begin
  ModeReadOnly:= not ModeReadOnly;
  Result:= [cResultState];
end;

procedure TATSynEdit.DoSelect_Word(P: TPoint);
var
  N1, N2: integer;
begin
  if not Strings.IsIndexValid(P.Y) then Exit;
  SFindWordBounds(Strings.Lines[P.Y], P.X, N1, N2);
  if N1<>N2 then
  begin
    DoCaretSingle(P.X, P.Y);
    Carets[0].EndY:= P.Y;
    Carets[0].EndX:= N1;
    Carets[0].PosX:= N2;
    Update;
  end;
end;

procedure TATSynEdit.DoSelect_Line(P: TPoint);
var
  N1, N2: integer;
begin
  if not Strings.IsIndexValid(P.Y) then Exit;
  N1:= 0;
  N2:= Length(Strings.Lines[P.Y]);
  if N1<>N2 then
  begin
    DoCaretSingle(P.X, P.Y);
    Carets[0].EndY:= P.Y;
    Carets[0].EndX:= N1;
    Carets[0].PosX:= N2;
    Update;
  end;
end;

