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
  FFoldList.Clear;

  FFoldList.Add(4, 5, 9);
  FFoldList.Add(4, 11, 14);
  FFoldList.Add(1, 4, 15);

  for i:= 2 to (Strings.Count-1) div 10 do
    if Odd(i) then
      FFoldList.Add(4, i*10, i*10+9);
end;


