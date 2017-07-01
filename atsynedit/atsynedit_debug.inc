{$ifdef nn}begin end;{$endif}

procedure TATSynEdit.DebugFindWrapIndex;
var
  i, j, n1, n2: integer;
begin
  for i:= 0 to Strings.Count-1 do
  begin
    FWrapInfo.FindIndexesOfLineNumber(i, n1, n2);
    if n1<0 then
    begin
      Application.MainForm.caption:= format('fail findindex: %d', [i]);
      Exit
    end;
    for j:= n1 to n2 do
      if FWrapInfo.Items[j].NLineIndex<>i then
      begin
        Application.MainForm.caption:= format('fail findindex: %d', [i]);
        Exit
      end;
  end;
  Application.MainForm.caption:= 'ok findindex';
end;

procedure TATSynEdit.DoDebugInitFoldList;
var
  i: integer;
begin
  FFold.Clear;

  //sorted by 2nd param
  FFold.Add(1, 4, 15, false, '');
  FFold.Add(1, 5, 9, false, '');
  FFold.Add(1, 7, 8, false, '');
  FFold.Add(1, 11, 14, false, '');

  for i:= 2 to (Strings.Count-1) div 10 do
    if Odd(i) then
      FFold.Add(4, i*10, i*10+9, false, '');
end;


function DoPartAsString(i: integer; const P: TATLineParts): string;
begin
  Result:= Format('[%d]: offset %d..%d', [i, P[i].Offset, P[i].Offset+P[i].Len, P[i].Len]);
end;


