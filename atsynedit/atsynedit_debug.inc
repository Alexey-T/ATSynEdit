{$ifdef nn}begin end;{$endif}

procedure TATSynEdit.DebugTimeWrapIndex;
const
  cMax=100;
  cMul=1000;
var
  i, j, t1, t2, n1, n2: integer;
begin
  if Strings.Count<cMax then Exit;

  t1:= GetTickCount;
  for j:= 1 to cMul do
    for i:= 0 to cMax do
      FWrapInfo.FindIndexesOfLineNumber(i, n1, n2);
  t1:= GetTickCount-t1;

  t2:= GetTickCount;
  for j:= 1 to cMul do
    for i:= Strings.Count-1-cMax to Strings.Count-1 do
      FWrapInfo.FindIndexesOfLineNumber(i, n1, n2);
  t2:= GetTickCount-t2;

  Application.MainForm.caption:= Format('time findindex: %d %d', [t1, t2]);
end;

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


