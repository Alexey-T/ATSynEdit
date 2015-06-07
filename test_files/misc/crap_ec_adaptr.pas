
type
  TATSubRange = class
    P1, P2: TPoint;
    An: TSyntAnalyzer;
  end;



procedure TATAdapterEControl.DoCalcPartsForSublexers(var AParts: TATLineParts;
  ALine, AX, ALen: integer);
var
  RSub: TATSubRange;
  Part: TATLinePart;
  i: integer;
begin
  for i:= 0 to sublist.Count-1 do
  begin
    RSub:= TATSubRange(sublist[i]);
    if RSub.P1.Y>ALine then Continue;
    if RSub.P2.Y<ALine then Continue;

    Dec(RSub.P1.X, AX);
    Dec(RSub.P2.X, AX);
    if RSub.P1.X>=ALen then Continue;
    if RSub.P2.X<0 then Continue;

    FillChar(Part{%H-}, Sizeof(Part), 0);
    Part.ColorBG:= clMoneyGreen;

    if RSub.P1.Y<ALine then
      Part.Offset:= 0
    else
      Part.Offset:= RSub.P1.X;

    if RSub.P2.Y>ALine then
      Part.Len:= ALen
    else
      Part.Len:= RSub.P2.X-Part.Offset;

    DoPartInsert(AParts, Part);
  end;
end;


procedure TATAdapterEControl.__UpdateSubList;
var
  R: TSublexerRange;
  RSub: TATSubRange;
  i: integer;
begin
  sublist.Clear;
  for i:= 0 to AnClient.SubLexerRangeCount-1 do
  begin
    R:= AnClient.SubLexerRanges[i];
    RSub:= TATSubRange.Create;
    RSub.P1:= buffer.StrToCaret(R.CondStartPos);
    RSub.P2:= buffer.StrToCaret(R.CondEndPos);
    RSub.An:= R.Rule.SyntAnalyzer;
    sublist.Add(RSub);
  end;
end;


destructor TATAdapterEControl.Destroy;
var
  i: integer;
begin
  for i:= sublist.Count-1 downto 0 do
    TObject(sublist[i]).Free;
  FreeAndNil(sublist);

  FreeAndNil(buffer);
  FreeAndNil(AnClient);
  An:= nil;
  Ed:= nil;

  inherited;
end;
