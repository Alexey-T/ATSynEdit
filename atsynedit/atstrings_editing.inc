{$ifdef none}begin end;{$endif}

procedure TATStrings.TextInsert(AX, AY: integer; const AText: atString; AReplace: boolean; out AShift, APosAfter: TPoint);
var
  Str, StrLead, StrTail: atString;
  MS: TMemoryStream;
  List: TATStrings;
  i: integer;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;

  if not IsIndexValid(AY) then Exit;
  if AX<0 then Exit;
  if AText='' then Exit;

  Str:= Lines[AY];

  if AX<Length(Str) then
  begin
    StrLead:= Copy(Str, 1, AX);
    StrTail:= Copy(Str, AX+1, MaxInt);
  end
  else
  begin
    StrLead:= Str+StringOfChar(' ', AX-Length(Str));
    StrTail:= '';
  end;

  if AReplace then
    Delete(StrTail, 1, Length(AText));

  //------------------
  //Insert single line

  if (Pos(#13, AText)=0) and
    (Pos(#10, AText)=0) then
  begin
    Lines[AY]:= StrLead+AText+StrTail;
    if not AReplace then
      AShift.X:= Length(AText);
    APosAfter.X:= AX+Length(AText);
    Exit
  end;

  //----------------------
  //Insert multi-line text

  List:= TATStrings.Create;
  try
    MS:= TMemoryStream.Create;
    try
      Str:= AText+StrTail;
      MS.Write(Str[1], Length(Str)*SizeOf(atChar));
      MS.Position:= 0;
      List.Encoding:= cEncWideLE;
      List.EncodingDetect:= false;
      List.LoadFromStream(MS);
      if List.Count=0 then Exit;
    finally
      FreeAndNil(MS);
    end;

    Lines[AY]:= StrLead+List.Lines[0];
    for i:= List.Count-1 downto 1 do
      LineInsert(AY+1, List.Lines[i]);

    APosAfter.X:= Length(List.Lines[List.Count-1])-Length(StrTail);
    APosAfter.Y:= AY+List.Count-1;
    AShift.Y:= List.Count-1;
  finally
    FreeAndNil(List);
  end;
end;

procedure TATStrings.TextDeleteLeft(AX, AY: integer; ALen: integer; out AShift,
  APosAfter: TPoint);
var
  Str, StrPrev: atString;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;
  Str:= Lines[AY];

  if AX>0 then
  begin
    if AX<=Length(Str) then
    begin
      System.Delete(Str, Max(1, AX+1-ALen), ALen);
      Lines[AY]:= Str;
    end;
    AShift.X:= -Min(AX, ALen);
    APosAfter.X:= Max(0, AX-ALen);
  end
  else
  if AY>0 then
  begin
    StrPrev:= Lines[AY-1];
    Lines[AY-1]:= StrPrev+Str;
    LineDelete(AY);
    AShift.Y:= -1;
    APosAfter.X:= Length(StrPrev);
    APosAfter.Y:= AY-1;
  end;
end;

procedure TATStrings.TextDeleteRight(AX, AY: integer; ALen: integer; out AShift,
  APosAfter: TPoint);
var
  Str: atString;
  DelEol: boolean;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;
  Str:= Lines[AY];

  DelEol:= false;
  if AX<Length(Str) then
  begin
    System.Delete(Str, AX+1, ALen);
    Lines[AY]:= Str;
    AShift.X:= -ALen;
  end
  else
    DelEol:= true;

  if DelEol then
    if Str='' then //handle for simpler line-states
    begin
      AShift.Y:= -1;
      if (AY>0) and (AY=Count-1) then
      begin
        APosAfter.X:= 0;
        APosAfter.Y:= AY-1;
      end;
      LineDelete(AY);
    end
    else
    begin
      //add spaces if we are after eol
      if AX>=Length(Str) then
        Str:= Str+StringOfChar(' ', AX-Length(Str));

      if AY+1<Count then
      begin
        Lines[AY]:= Str+Lines[AY+1];
        LineDelete(AY+1);
      end
      else
        LinesEnds[AY]:= cEndNone;

      AShift.Y:= -1;
    end;
end;

procedure TATStrings.TextDeleteRange(AFromX, AFromY, AToX, AToY: integer;
  out AShift, APosAfter: TPoint);
var
  Str: atString;
  i: integer;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AFromX;
  APosAfter.Y:= AFromY;
  if not IsIndexValid(AFromY) then Exit;
  if not IsIndexValid(AToY) then Exit;
  if (AFromX=AToX) and (AFromY=AToY) then Exit;
  if (AFromY>AToY) then Exit;

  if AFromY=AToY then
  //delete range in one line
  begin
    Str:= Lines[AFromY];
    Delete(Str, AFromX+1, AToX-AFromX);
    Lines[AFromY]:= Str;

    AShift.X:= -(AToX-AFromX);
  end
  else
  //delete from 1st line, concat rest of last line, delete lines
  begin
    Str:= Lines[AFromY];
    Delete(Str, AFromX+1, MaxInt);
    Str:= Str+Copy(Lines[AToY], AToX+1, MaxInt);
    Lines[AFromY]:= Str;

    for i:= AToY downto AFromY+1 do
      LineDelete(i);

    AShift.Y:= -(AToY-AFromY);
  end;
end;

procedure TATStrings.TextInsertEol(AX, AY: integer; AKeepCaret: boolean; const AStrIndent: atString; out AShift, APosAfter: TPoint);
var
  Str, StrMove: atString;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;

  Str:= Lines[AY];
  StrMove:= '';

  if (AX<Length(Str)) then
  begin
    StrMove:= System.Copy(Str, AX+1, MaxInt);
    System.Delete(Str, AX+1, MaxInt);
    Lines[AY]:= Str;
  end;

  LineInsert(AY+1, AStrIndent+StrMove);
  LinesEnds[AY]:= Endings; //force eol
  LinesEnds[AY+1]:= Endings; //force eol
  //if AY+1=Count-1 then
  //  LineAddEx('');

  if not AKeepCaret then
  begin
    APosAfter.X:= Length(AStrIndent);
    APosAfter.Y:= AY+1;
    AShift.Y:= 1;
  end;
end;

procedure TATStrings.TextDeleteCurLine(AX, AY: integer; out AShift, APosAfter: TPoint);
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;

  AShift.Y:= -1;
  APosAfter.X:= 0;
  if AY=Count-1 then
    APosAfter.Y:= Max(0, AY-1);

  LineDelete(AY);
end;

procedure TATStrings.TextDuplicateCurLine(AX, AY: integer; out AShift, APosAfter: TPoint);
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;

  LineInsert(AY+1, Lines[AY]);

  if LinesEnds[AY]<>Endings then
    LinesEnds[AY]:= Endings;
  LinesEnds[AY+1]:= Endings;
  //if AY+1=Count-1 then
  //  LineAddEx('');

  AShift.Y:= 1;
end;


