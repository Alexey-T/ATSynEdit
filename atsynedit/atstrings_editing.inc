{$ifdef none}begin end;{$endif}

procedure TATStrings.TextInsert(AX, AY: integer; const AText: atString; AOverwrite: boolean; out AShift, APosAfter: TPoint);
var
  Str, StrLead, StrTail: atString;
  List: TATStrings;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;

  if not IsIndexValid(AY) then Exit;
  if AX<0 then Exit;
  if AText='' then Exit;

  Str:= Lines[AY];

  if AX<=Length(Str) then
  begin
    StrLead:= Copy(Str, 1, AX);
    StrTail:= Copy(Str, AX+1, MaxInt);
  end
  else
  begin
    StrLead:= Str+StringOfChar(' ', AX-Length(Str));
    StrTail:= '';
  end;

  if AOverwrite then
    Delete(StrTail, 1, Length(AText));

  //------------------
  //Insert single line

  if not SWithBreaks(AText) then
  begin
    Lines[AY]:= StrLead+AText+StrTail;
    if not AOverwrite then
      AShift.X:= Length(AText);
    APosAfter.X:= AX+Length(AText);
    Exit
  end;

  //----------------------
  //Insert multi-line text

  List:= TATStrings.Create;
  try
    List.LoadFromString(StrLead+AText);
    List.ActionDeleteFakeLine;
    if List.Count=0 then Exit;

    if StrTail<>'' then
      Lines[AY]:= StrTail
    else
    if Lines[AY]<>'' then
      LineDelete(AY);

    LineInsertStrings(AY, List);

    APosAfter.X:= 0;
    APosAfter.Y:= AY+List.Count;
    AShift.Y:= APosAfter.Y-AY;
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

  //special case: last fake line
  if (AY=Count-1) and (Str='') and (LinesEnds[AY]=cEndNone) then
    Exit;

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

      //not last: del next line
      if AY+1<Count then
      begin
        Lines[AY]:= Str+Lines[AY+1];
        LineDelete(AY+1, false{not force});
        //maybe also eol
        if AY=Count-1 then
          LinesEnds[AY]:= cEndNone;
      end
      else
        //last line: del eol
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
  begin
    //delete range in one line
    Str:= Lines[AFromY];
    Delete(Str, AFromX+1, AToX-AFromX);
    Lines[AFromY]:= Str;

    AShift.X:= -(AToX-AFromX);
  end
  else
  begin
    //correct 1st line
    Str:= Copy(Lines[AFromY], 1, AFromX)+Copy(Lines[AToY], AToX+1, MaxInt);
    Lines[AFromY]:= Str;

    //del oth lines
    for i:= AToY downto AFromY+1 do
      LineDelete(i);

    //del 1st line too?
    if Str='' then
      LineDelete(AFromY);

    AShift.Y:= -(AToY-AFromY);
  end;
end;


procedure TATStrings.TextInsertColumnBlock(AX, AY: integer; ABlock: TATStrings; AOverwrite: boolean);
var
  Shift, PosAfter: TPoint;
  i: integer;
begin
  for i:= 0 to ABlock.Count-1 do
  begin
    TextInsert(AX, AY+i, ABlock.Lines[i], AOverwrite, Shift, PosAfter);
    LinesEnds[AY+i]:= Endings; //force eol
    if not IsIndexValid(AY+i+1) then
      LineAddRaw('', cEndNone);
  end;
end;

procedure TATStrings.TextInsertEol(AX, AY: integer; AKeepCaret: boolean; const AStrIndent: atString; out AShift, APosAfter: TPoint);
var
  Str, StrMove: atString;
  NewEnd: TATLineEnds;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;

  Str:= Lines[AY];
  StrMove:= '';

  //special case AX=0: just insert empty line
  //(less changes in undo)
  if AX=0 then
  begin
    LineInsertRaw(AY, '', Endings);
  end
  else
  begin
    if (AX<Length(Str)) then
    begin
      StrMove:= Copy(Str, AX+1, MaxInt);
      Delete(Str, AX+1, MaxInt);
      Lines[AY]:= Str;
    end;

    //handle situation when we at non-eol line, this must give
    //inserted line also w/o eol
    NewEnd:= LinesEnds[AY];
    LinesEnds[AY]:= Endings; //force eol to cur line
    LineInsertRaw(AY+1, AStrIndent+StrMove, NewEnd);
  end;

  if not AKeepCaret then
  begin
    APosAfter.X:= Length(AStrIndent);
    APosAfter.Y:= AY+1;
    AShift.Y:= 1;
  end;
end;

procedure TATStrings.TextDeleteLine(AX, AY: integer; out AShift, APosAfter: TPoint);
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;

  AShift.Y:= -1;
  APosAfter.X:= 0;

  LineDelete(AY);
  if AY>=Count then
    LineAddEx('', cEndNone);
end;

procedure TATStrings.TextDuplicateLine(AX, AY: integer; out AShift, APosAfter: TPoint);
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

  AShift.Y:= 1;
end;


