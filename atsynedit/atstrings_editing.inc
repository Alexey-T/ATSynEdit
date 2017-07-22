{$ifdef none}begin end;{$endif}


procedure TATStrings.TextReplaceInOneLine(AY, AX1, AX2: integer; const AText: atString);
//simple, made to speedup mass-replace in CudaText
//(usually match replaced inside one line)
//IsIndexValid must be in outer func
var
  Str: atString;
begin
  Str:= Lines[AY];
  Str:=
    Copy(Str, 1, AX1) +
    AText +
    Copy(Str, AX2+1, MaxInt);
  Lines[AY]:= Str;
end;


procedure TATStrings.TextReplaceRange(AFromX, AFromY, AToX, AToY: integer;
  const AText: atString; out AShift, APosAfter: TPoint);
var
  bOneLine: boolean;
begin
  bOneLine:= (AFromY=AToY) and (Pos(#10, AText)=0);
  if bOneLine then
  begin
    TextReplaceInOneLine(AFromY, AFromX, AToX, AText);
    AShift.X:= 0;
    AShift.Y:= 0;
    APosAfter.X:= AFromX+Length(AText);
    APosAfter.Y:= AFromY;
  end
  else
  begin
    BeginUndoGroup;
    TextDeleteRange(AFromX, AFromY, AToX, AToY, AShift, APosAfter);
    TextInsert(AFromX, AFromY, AText, false, AShift, APosAfter);
    EndUndoGroup;
  end;
end;


function TATStrings.TextReplaceLines_UTF8(ALineFrom, ALineTo: integer;
  ANewLines: TStringList): boolean;
var
  i: integer;
begin
  Result:= false;
  if not IsIndexValid(ALineFrom) then exit;
  Result:= true;

  BeginUndoGroup;
  FChangeBlockActive:= true;
  try
    DoOnChangeBlock(
      0, ALineFrom,
      0, Min(ALineTo, Count-1),
      cBlockDeleteLines,
      nil);

    for i:= Min(ALineTo, Count-1) downto ALineFrom do
      LineDelete(i);

    DoOnChangeBlock(
      0, ALineFrom,
      0, 0,
      cBlockInsertLines,
      ANewLines);

    for i:= ANewLines.Count-1 downto 0 do
      LineInsert(ALineFrom, UTF8Decode(ANewLines[i]));
  finally
    FChangeBlockActive:= false;
    EndUndoGroup;
  end;
end;


procedure TATStrings.TextInsert(AX, AY: integer; const AText: atString; AOverwrite: boolean; out AShift, APosAfter: TPoint);
var
  Str, StrLead, StrTail: atString;
  Block: TATStrings;
  BlockUTF8: TStringList;
  bAtLineEnd, bAtFileEnd, bAtEmptyLine,
  bBlockWithEol, bUseBlockFinalEol: boolean;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;

  if not IsIndexValid(AY) then Exit;
  if AX<0 then Exit;
  if AText='' then Exit;

  Str:= Lines[AY];
  bBlockWithEol:= SEndsWithEol(AText);
  bAtLineEnd:= AX>=Length(Str);
  bAtFileEnd:= bAtLineEnd and (AY>=Count-1);
  bAtEmptyLine:= Str='';
  //note: bools used for pasting block of n lines, yes/no final eol, at end of line

  if not bAtLineEnd then
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

  Block:= TATStrings.Create;
  try
    FChangeBlockActive:= true;
    BeginUndoGroup;
    Block.LoadFromString(StrLead+AText);

    if not bBlockWithEol or bAtEmptyLine then
      Block.ActionDeleteFakeLine;
    if Block.Count=0 then Exit;
    //ShowMessage('block lines '+IntToStr(Block.Count));

    if StrTail<>'' then
      Lines[AY]:= StrTail
    else
    if Lines[AY]<>'' then
      LineDelete(AY);

    if Assigned(FOnChangeBlock) then
    begin
      BlockUTF8:= ATStrings_To_StringList(Block);
      try
        DoOnChangeBlock(
          0, AY,
          0, 0,
          cBlockInsertLines,
          BlockUTF8);
      finally
        FreeAndNil(BlockUTF8);
      end;
    end;

    if not bAtFileEnd then
      bUseBlockFinalEol:= bAtLineEnd
    else
      bUseBlockFinalEol:= bBlockWithEol;
    LineInsertStrings(AY, Block, bUseBlockFinalEol);

    APosAfter.X:= Block.LinesLen[Block.Count-1];
    APosAfter.Y:= AY+Block.Count-1;
    AShift.Y:= APosAfter.Y-AY;

  finally
    FreeAndNil(Block);
    EndUndoGroup;
    FChangeBlockActive:= false;
  end;
end;

procedure TATStrings.TextAppend(const AText: atString; out AShift, APosAfter: TPoint);
var
  NX, NY: integer;
begin
  if Count=0 then
    TextInsert(0, 0, AText, false, AShift, APosAfter)
  else
  begin
    NY:= Count-1;
    NX:= LinesLen[NY];
    TextInsert(NX, NY, AText, false, AShift, APosAfter);
  end;
end;

procedure TATStrings.TextDeleteLeft(AX, AY: integer; ALen: integer; out AShift, APosAfter: TPoint);
var
  Str, StrPrev: atString;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;
  Str:= Lines[AY];

  BeginUndoGroup;
  try

    //handle spec case: caret on last fake line, BkSp pressed:
    //delete fake line,
    //delete EOL at prev line
    if (AX=0) and (AY=Count-1) and (AY>0) and IsLastLineFake then
    begin
      //DoOnChangeBlock(
      //  0, AY,
      //  0, AY+1,
      //  cBlockDeleteLines,
      //  nil);

      LineDelete(AY, false);
      LinesEnds[AY-1]:= cEndNone;
      AShift.Y:= -1;
      APosAfter.X:= Length(Lines[AY-1]);
      APosAfter.Y:= AY-1;
      exit
    end;

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
      //DoOnChangeBlock(
      //  0, AY,
      //  0, AY+1,
      //  cBlockDeleteLines,
      //  nil);

      StrPrev:= Lines[AY-1];
      Lines[AY-1]:= StrPrev+Str;
      LineDelete(AY);
      AShift.Y:= -1;
      APosAfter.X:= Length(StrPrev);
      APosAfter.Y:= AY-1;
    end;

  finally
    EndUndoGroup;
  end;
end;

procedure TATStrings.TextDeleteRight(AX, AY: integer; ALen: integer; out AShift,
  APosAfter: TPoint; ACanDelEol: boolean = true);
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

  BeginUndoGroup;
  try

    DelEol:= false;
    if AX<Length(Str) then
    begin
      System.Delete(Str, AX+1, ALen);
      Lines[AY]:= Str;
      AShift.X:= -ALen;
    end
    else
      DelEol:= ACanDelEol;

    if DelEol then
      if Str='' then //handle for simpler line-states
      begin
        AShift.Y:= -1;
        if (AY>0) and (AY=Count-1) then
        begin
          APosAfter.X:= 0;
          APosAfter.Y:= AY-1;
        end;

        //DoOnChangeBlock(
        //  0, AY,
        //  0, AY+1,
        //  cBlockDeleteLines,
        //  nil);

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
          //DoOnChangeBlock(
          //  0, AY+1,
          //  0, AY+2,
          //  cBlockDeleteLines,
          //  nil);

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

  finally
    EndUndoGroup;
  end;
end;

function TATStrings.TextDeleteRange(AFromX, AFromY, AToX, AToY: integer;
  out AShift, APosAfter: TPoint): boolean;
var
  Str: atString;
  bDelEmpty, bDeleteEol, bDeleteEol_Alt: boolean;
  i: integer;
begin
  Result:= false;
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AFromX;
  APosAfter.Y:= AFromY;

  if Count=0 then Exit;
  if not IsIndexValid(AFromY) then Exit;

  if AToY<0 then Exit;
  if AToY>=Count then
    begin AToY:= Count-1; AToX:= LinesLen[AToY]; end;

  if (AFromX=AToX) and (AFromY=AToY) then Exit;
  if (AFromY>AToY) then Exit;
  if (AFromX<0) or (AToX<0) then Exit;

  bDeleteEol:= (AFromY=0) and (AToY>=Count-1);
  bDeleteEol_Alt:= false;

  if (AFromY=AToY) then
  begin
    //delete range in one line
    Str:= Lines[AFromY];
    Delete(Str, AFromX+1, AToX-AFromX);
    Lines[AFromY]:= Str;

    AShift.X:= -(AToX-AFromX);
  end
  else
  if (AFromX=0) and (AToX=0) then
  begin
    //selected entire lines
    FChangeBlockActive:= true;
    DoOnChangeBlock(
      0, AFromY,
      0, AToY-1,
      cBlockDeleteLines,
      nil);

    for i:= AToY-1 downto AFromY do
      LineDelete(i);
    FChangeBlockActive:= false;
  end
  else
  begin
    bDelEmpty:= false;

    //for empty last line
    if (AToY>0) and (LinesLen[AToY]=0) then
    begin
      LineDelete(AToY);
      Dec(AToY);
      AToX:= LinesLen[AToY];
    end;

    if AFromX=0 then
      bDelEmpty:= true;

    //remember no final eol
    bDeleteEol_Alt:= (AFromY=0) and (AToY=Count-1) and (LinesEnds[AToY]=cEndNone);

    //concat remaining parts of 1st+last lines
    Str:= '';
    if AFromX>0 then
      Str:= Copy(Lines[AFromY], 1, AFromX);
    Str:= Str + Copy(Lines[AToY], AToX+1, MaxInt);
    Lines[AFromY]:= Str;

    //del middle lines
    FChangeBlockActive:= true;
    DoOnChangeBlock(
      0, AFromY+1,
      0, AToY,
      cBlockDeleteLines,
      nil);

    for i:= AToY downto AFromY+1 do
      LineDelete(i);

    FChangeBlockActive:= false;

    if bDelEmpty then
      if Str='' then
        LineDelete(AFromY);

    if bDeleteEol then
    begin
      if Count>0 then
        LinesEnds[Count-1]:= cEndNone;
      ActionDeleteFakeLineAndFinalEol;
    end
    else
    if bDeleteEol_Alt then
    begin
      ActionDeleteFakeLineAndFinalEol;
    end;

    AShift.Y:= -(AToY-AFromY);
  end;

  //count=0 may be after deling block, if last line has no eol
  if Count=0 then
    LineAddEx('', cEndNone);

  Result:= true;
end;


procedure TATStrings.TextInsertColumnBlock(AX, AY: integer; ABlock: TATStrings; AOverwrite: boolean);
var
  Shift, PosAfter: TPoint;
  BlockUTF8: TStringList;
  i: integer;
begin
  FChangeBlockActive:= true;

  if Assigned(FOnChangeBlock) then
  begin
    BlockUTF8:= ATStrings_To_StringList(ABlock);
    try
      if AOverwrite then
        DoOnChangeBlock(
          AX, AY,
          AX + Length(ABlock.Lines[0]), AY + ABlock.Count,
          cBlockDeleteColumn,
          nil);
      DoOnChangeBlock(
        AX, AY,
        0, 0,
        cBlockInsertColumn,
        BlockUTF8);
    finally
      FreeAndNil(BlockUTF8);
    end;
  end;

  for i:= 0 to ABlock.Count-1 do
  begin
    TextInsert(AX, AY+i, ABlock.Lines[i], AOverwrite, Shift, PosAfter);
    LinesEnds[AY+i]:= Endings; //force eol
    if not IsIndexValid(AY+i+1) then
      LineAddRaw('', cEndNone);
  end;

  FChangeBlockActive:= false;
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
    BeginUndoGroup;

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

    EndUndoGroup;
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

  LineDelete(AY);
  if AY>=Count then
    LineAddEx('', cEndNone);
end;


