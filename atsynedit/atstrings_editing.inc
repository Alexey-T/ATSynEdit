{$ifdef none}begin end;{$endif}


procedure TATStrings.TextReplace_OneLine(AY, AX1, AX2: SizeInt; const AText: atString);
//simple, made to speedup mass-replace in CudaText
//(usually match replaced inside one line)
//IsIndexValid must be in outer func
var
  Str: atString;
begin
  Str:= Lines[AY];
  SDeleteAndInsert(Str, AX1+1, AX2-AX1, AText);
  Lines[AY]:= Str;
end;

procedure TATStrings.TextReplace_OneLine_ReplaceOneEol(AY, AX1, AX2: SizeInt; const ATextPart1,
  ATextPart2: atString);
{
removes substr (AX1, AX2) from line AY, and splits the line to two parts.
ATextPart1 is appeneded to 1st part (before AX1),
ATextPart2 is prepended to 2nd part (after AX2).
}
var
  Str: atString;
  NEnd: TATLineEnds;
begin
  Str:= Lines[AY];
  if AY=Count-1 then
    NEnd:= LinesEnds[AY]
  else
    NEnd:= FEndings;

  Lines[AY]:= Copy(Str, 1, AX1) + ATextPart1;

  //line AY becomes not last, so set ending for it
  if LinesEnds[AY]=TATLineEnds.None then
    LinesEnds[AY]:= FEndings;
  //line AY+1 can be last, so keep old eol
  LineInsertEx(AY+1, ATextPart2 + Copy(Str, AX2+1, MaxInt), NEnd);
end;


procedure TATStrings.TextReplaceRange(AFromX, AFromY, AToX, AToY: SizeInt;
  const AText: atString; out AShift, APosAfter: TPoint; AWithUndoGroup: boolean);
var
  bOneLine, bOneEol: boolean;
  NCountEol, N: SizeInt;
  SPart1, SPart2: atString;
begin
  NCountEol:= SFindCharCount(AText, #10);
  bOneLine:= (AFromY=AToY) and (NCountEol=0);
  bOneEol:= (AFromY=AToY) and (NCountEol=1);

  if bOneLine then
  begin
    TextReplace_OneLine(AFromY, AFromX, AToX, AText);
    AShift.X:= Length(AText)-(AToX-AFromX);
    AShift.Y:= 0;
    APosAfter.X:= AFromX+Length(AText);
    APosAfter.Y:= AFromY;
  end
  else
  if bOneEol then
  begin
    N:= Pos(#10, AText);
    SPart1:= Copy(AText, 1, N-1);
    SPart2:= Copy(AText, N+1, MaxInt);
    TextReplace_OneLine_ReplaceOneEol(AFromY, AFromX, AToX, SPart1, SPart2);
    AShift.X:= 0;
    AShift.Y:= 1;
    APosAfter.X:= Length(SPart2);
    APosAfter.Y:= AFromY+1;
  end
  else
  begin
    if AWithUndoGroup then
      BeginUndoGroup;
    TextDeleteRange(AFromX, AFromY, AToX, AToY, AShift, APosAfter);
    TextInsert(AFromX, AFromY, AText, false, AShift, APosAfter);
    if AWithUndoGroup then
      EndUndoGroup;
  end;
end;


function TATStrings.TextReplaceLines_UTF8(ALineFrom, ALineTo: SizeInt;
  ANewLines: TStringList): boolean;
var
  bHadFakeLine: boolean;
begin
  Result:= false;
  if not IsIndexValid(ALineFrom) then exit;
  Result:= true;

  BeginUndoGroup;
  try
    bHadFakeLine:= IsLastLineFake;
    LineBlockDelete(ALineFrom, Min(ALineTo, Count-1)); //too big ALineTo allowed
    LineBlockInsert(ALineFrom, ANewLines);
    //fake line maybe appeared if we insert block until file end. delete fake line.
    if not bHadFakeLine and (Count>1) and IsLastLineFake then
      ActionDeleteFakeLineAndFinalEol;
  finally
    EndUndoGroup;
  end;
end;


procedure TATStrings.TextInsert(AX, AY: SizeInt; const AText: atString; AOverwrite: boolean; out AShift, APosAfter: TPoint);
var
  Str, StrLead, StrTail: atString;
  LastItem: PATStringItem;
  Block: TATStrings;
  BlockUTF8: TStringList;
  bTrimRedundantEol, bInsertSingleLine,
  bAtLineEnd, bAtFileEnd, bAtEmptyLine, bBlockWithEol: boolean;
  NBlockOriginalCount, NBlockLastCount,
  NDeleteLen, NLineOfBlockInsert, iLine: SizeInt;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;

  if not IsIndexValid(AY) then Exit;
  if AX<0 then Exit;
  if AText='' then Exit;

  Str:= Lines[AY];
  StrLead:= '';
  StrTail:= '';

  bBlockWithEol:= SEndsWithEol(AText);
  bAtLineEnd:= AX>=Length(Str);
  bAtFileEnd:= bAtLineEnd and (AY>=Count-1);
  bAtEmptyLine:= Str='';
  bInsertSingleLine:= not SStringHasEol(AText);
  bTrimRedundantEol:= false;

  if bAtFileEnd then
    bTrimRedundantEol:= not bBlockWithEol
  else
  if not bBlockWithEol then
  begin
    if (Count=0) or ((Count=1) and (LinesLen[0]=0)) then
      bTrimRedundantEol:= not bBlockWithEol
    else
    begin
      LastItem:= FList.GetItem(Count-1);
      bTrimRedundantEol:= (LastItem^.CharLen>0) and (LastItem^.LineEnds=TATLineEnds.None);
    end
  end;

  if not bAtLineEnd then
  begin
    if bInsertSingleLine then
    begin
      //simplest case: typing a char in a middle of line
      //optimize it specially via SDeleteAndInsert
      if not AOverwrite then
        NDeleteLen:= 0
      else
      begin
        NDeleteLen:= Length(AText);
        //if we type over emoji - delete 2 surrogate chars
        if IsCharSurrogateHigh(Str[AX+1]) then
          Inc(NDeleteLen);
      end;
      SDeleteAndInsert(Str, AX+1, NDeleteLen, AText);
      Lines[AY]:= Str;
      if not AOverwrite then
        AShift.X:= Length(AText);
      APosAfter.X:= AX+Length(AText);
      Exit
    end
    else
    begin
      StrLead:= Copy(Str, 1, AX);
      StrTail:= Copy(Str, AX+1, MaxInt);
    end;
  end
  else
  begin
    StrLead:= Str+StringOfCharW(' ', AX-Length(Str));
    StrTail:= '';
  end;

  if AOverwrite and (StrTail<>'') then
  begin
    NDeleteLen:= Length(AText);
    //if we type over emoji - delete 2 surrogate chars
    if IsCharSurrogateHigh(StrTail[1]) then
      Inc(NDeleteLen);
    Delete(StrTail, 1, NDeleteLen);
  end;

  //------------------
  //Insert single line

  if bInsertSingleLine then
  begin
    Lines[AY]:= StrLead+AText+StrTail;
    if not AOverwrite then
      AShift.X:= Length(AText);
    APosAfter.X:= AX+Length(AText);
    Exit
  end;

  //----------------------
  //Insert multi-line text

  Block:= TATStrings.Create(0); //set UndoLimit=0 to avoid undo in Block
  try
    FChangeBlockActive:= true;
    BeginUndoGroup;

    Block.LoadFromString(UTF8Encode(StrLead+AText));
    NBlockOriginalCount:= Block.Count;
    if Block.Count=0 then Exit;
    NLineOfBlockInsert:= AY;

    if StrTail<>'' then
    begin
      if not bBlockWithEol then
      begin
        Lines[AY]:= Block.Lines[Block.Count-1]+StrTail;
        Block.LineDelete(Block.Count-1);
      end
      else
        Lines[AY]:= StrTail;
    end
    else
    if Lines[AY]<>'' then
    begin
      Lines[AY]:= Block.Lines[0];
      Block.LineDelete(0, false, false, false);
      if (Block.Count>0) and (LinesEnds[AY]=TATLineEnds.None) then
        LinesEnds[AY]:= Endings;
      Inc(NLineOfBlockInsert);
    end;

    if Assigned(FOnChangeBlock) then
    begin
      BlockUTF8:= ATStrings_To_StringList(Block);
      try
        DoOnChangeBlock(
          0, NLineOfBlockInsert,
          0, 0,
          TATBlockChangeKind.InsertLines,
          BlockUTF8);
      finally
        FreeAndNil(BlockUTF8);
      end;
    end;

    {
    must test like this:
    1) copy to clipboard 3 lines without trailing EOL
      test pasting at:
       a) empty line
       b) middle of line
       c) end of non-empty line
    2) copy to clipboard 3 lines with trailing EOL
       again: a) b) c)
    }

    Block.ActionDeleteFakeLine;
    NBlockLastCount:= Block.Count;
    if NBlockLastCount=0 then
      ActionAddFakeLineIfNeeded
    else
    for iLine:= 0 to NBlockLastCount-1 do
    begin
      if IsIndexValid(NLineOfBlockInsert+iLine) then
        LineInsert(NLineOfBlockInsert+iLine, Block.Lines[iLine], false)
      else
        LineAdd(Block.Lines[iLine]);
    end;

    //fix CudaText issue #3408
    if bTrimRedundantEol then
      ActionDeleteFakeLineAndFinalEol;

    if bBlockWithEol then
    begin
      APosAfter.X:= 0;
      APosAfter.Y:= AY+NBlockOriginalCount-1;
    end
    else
    begin
      APosAfter.X:= Block.LinesLen[Block.Count-1];
      APosAfter.Y:= AY+NBlockOriginalCount-1;
    end;

    //make sure caret won't go after text end, due to unfixed bugs (currently cannot reproduce)
    if not IsIndexValid(APosAfter.Y) then
    begin
      APosAfter.Y:= Count-1;
      APosAfter.X:= LinesLen[APosAfter.Y];
    end;

    AShift.Y:= Max(1, APosAfter.Y-AY - IfThen(bBlockWithEol and bAtLineEnd, 1));
    //to check that AShift.Y is ok:
    //paste eg 3-lines block, when eg 4 carets exist below (on different lines),
    //and check how 4 carets were shifted down.

    DoEventLog(AY);
    DoEventChange(TATLineChangeKind.Added, AY, AShift.Y);
    //to check DoEventChange params are ok:
    //paste eg 3-lines block, when a bookmark exists below,
    //and check how bookmark was shifted down.

  finally
    FreeAndNil(Block);
    EndUndoGroup;
    FChangeBlockActive:= false;
  end;
end;

procedure TATStrings.TextAppend(const AText: atString; out AShift, APosAfter: TPoint);
var
  NX, NY: SizeInt;
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

procedure TATStrings.TextDeleteLeft(AX, AY: SizeInt; ALen: SizeInt;
  out AShift, APosAfter: TPoint;
  AllowGoToPrevLine: boolean;
  out ATextChanged: boolean);
var
  Str, StrPrev: atString;
  bNeedGroup: boolean;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  ATextChanged:= false;
  if not IsIndexValid(AY) then Exit;

  ATextChanged:= true;
  Str:= Lines[AY];

  bNeedGroup:= true; //ALen>1; //must not create undo-group for usual BkSpace pressing (ALen=1)
  if bNeedGroup then
    BeginUndoGroup;

  try

    //handle spec case: caret on last fake line, BkSp pressed:
    //delete fake line,
    //delete EOL at prev line
    if (AX=0) and (AY=Count-1) and (AY>0) and IsLastLineFake and AllowGoToPrevLine then
    begin
      //DoOnChangeBlock(
      //  0, AY,
      //  0, AY+1,
      //  cBlockDeleteLines,
      //  nil);

      LineDelete(AY, false);
      LinesEnds[AY-1]:= TATLineEnds.None;
      AShift.Y:= -1;
      APosAfter.X:= Length(Lines[AY-1]);
      APosAfter.Y:= AY-1;
      exit
    end;

    if AX>0 then
    begin
      if AX-ALen<Length(Str) then
      begin
        System.Delete(Str, Max(1, AX+1-ALen), ALen);
        Lines[AY]:= Str;
      end
      else
        ATextChanged:= false;
      AShift.X:= -Min(AX, ALen);
      APosAfter.X:= Max(0, AX-ALen);
    end
    else
    if (AY>0) and AllowGoToPrevLine then
    begin
      //DoOnChangeBlock(
      //  0, AY,
      //  0, AY+1,
      //  cBlockDeleteLines,
      //  nil);

      StrPrev:= Lines[AY-1];
      if StrPrev<>'' then
        //better do changing of line AY (and delete AY-1), this will unfold block on this line
        Lines[AY]:= StrPrev+Str;
      LineDelete(AY-1);

      AShift.Y:= -1;
      APosAfter.X:= Length(StrPrev);
      APosAfter.Y:= AY-1;
    end
    else
      ATextChanged:= false;

  finally
    if bNeedGroup then
      EndUndoGroup;
  end;
end;

procedure TATStrings.TextDeleteRight(AX, AY: SizeInt; ALen: SizeInt; out AShift,
  APosAfter: TPoint; ACanDelEol: boolean = true);
var
  Str: atString;
  bDelEol: boolean;
  bNeedGroup: boolean;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;
  Str:= Lines[AY];

  //special case: last fake line
  if (AY=Count-1) and (Str='') and (LinesEnds[AY]=TATLineEnds.None) then
    Exit;

  bNeedGroup:= true; //ALen>1; //must not create undo-group for usual Delete pressing
  if bNeedGroup then
    BeginUndoGroup;

  try
    bDelEol:= false;
    if AX<Length(Str) then
    begin
      System.Delete(Str, AX+1, ALen);
      Lines[AY]:= Str;
      AShift.X:= -ALen;
    end
    else
      bDelEol:= ACanDelEol;

    if bDelEol then
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
          Str+= StringOfCharW(' ', AX-Length(Str));

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
            LinesEnds[AY]:= TATLineEnds.None;
        end
        else
          //last line: del eol
          LinesEnds[AY]:= TATLineEnds.None;

        AShift.Y:= -1;
      end;

  finally
    if bNeedGroup then
      EndUndoGroup;
  end;
end;


procedure TATStrings.LineBlockDelete(ALine1, ALine2: SizeInt; AForceLast: boolean = true);
var
  NCount, i: SizeInt;
  //CurUndoList: TATUndoList;
  //NNeedUndoCapacity: integer;
begin
  if ALine1>ALine2 then Exit;
  NCount:= Count;
  if ALine1>=NCount then ALine1:= NCount-1;
  if ALine2>=NCount then ALine2:= NCount-1;

  {
  //reduce reallocs when adding lot of undo-items.
  //but, seems it don't give any speedup.
  //on deleting 700K lines on CPU Intel Core i3 3GHz - time is ~1sec.
  case FRunningUndoOrRedo of
    TATEditorRunningUndoOrRedo.Undo:
      CurUndoList:= FRedoList;
    TATEditorRunningUndoOrRedo.Redo:
      CurUndoList:= FUndoList;
    else
      CurUndoList:= FUndoList;
  end;

  NNeedUndoCapacity:= CurUndoList.Count+(ALine2-ALine1+2);
  if CurUndoList.Capacity<NNeedUndoCapacity then
    CurUndoList.Capacity:= NNeedUndoCapacity;
  }

  FChangeBlockActive:= true;
  DoOnChangeBlock(
    0, ALine1,
    0, ALine2,
    TATBlockChangeKind.DeleteLines,
    nil);

  //delete slowly with undo; yes, we do it for any count of deleted lines
  for i:= ALine2 downto ALine1 do
    LineDelete(i,
      false{AForceLast},
      false{WithEvent},
      true{AWithUndo}
      );

  (*
  if (ALine2-ALine1)<FUndoList.MaxCount then
  begin
    //delete slowly with undo
    //AWithEvent=false to fix freeze by NN sec, by Ctrl+A,Del during lexer parsing
    for i:= ALine2 downto ALine1 do
      LineDelete(i, false{AForceLast}, false{WithEvent});
  end
  else
  begin
    //delete fast
    FList.DeleteRange(ALine1, ALine2);
    ClearUndo;
  end;
  *)

  if AForceLast then
    ActionAddFakeLineIfNeeded;

  DoEventChange(TATLineChangeKind.Deleted, ALine1, ALine2-ALine1+1);
  DoEventLog(ALine1);
  Modified:= true;

  FChangeBlockActive:= false;
end;

procedure TATStrings.LineBlockInsert(ALineFrom: SizeInt; ANewLines: TStringList);
var
  i: SizeInt;
begin
  FChangeBlockActive:= true;
  DoOnChangeBlock(
    0, ALineFrom,
    0, 0,
    TATBlockChangeKind.InsertLines,
    ANewLines);

  for i:= ANewLines.Count-1 downto 0 do
    LineInsert(ALineFrom, UTF8Decode(ANewLines[i]), false{AWithEvent});
  FChangeBlockActive:= false;

  DoEventLog(ALineFrom);
  DoEventChange(TATLineChangeKind.Added, ALineFrom, ANewLines.Count);
end;


function TATStrings.TextDeleteRange(AFromX, AFromY, AToX, AToY: SizeInt;
  out AShift, APosAfter: TPoint): boolean;
var
  Str: atString;
  bDeleteEol, bDeleteEol_Alt: boolean;
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
  begin
    AToY:= Count-1;
    AToX:= LinesLen[AToY];
  end;

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
    LineBlockDelete(AFromY, AToY-1, false{AForceLast});

    AShift.Y:= -(AToY-AFromY);
  end
  else
  if (AFromX=0) and (AToY=Count-1) and (AToX=LinesLen[AToY]) then
  begin
    //selected entire lines, to the end of last line
    LineBlockDelete(AFromY, AToY);

    AShift.Y:= -(AToY-AFromY);
  end
  else
  begin
    //for empty last line
    if (AToY>0) and (LinesLen[AToY]=0) then
    begin
      if AToY=Count-1 then //AFromY<AToY here
        bDeleteEol:= true;
      LineDelete(AToY, false{AForceLast});
      Dec(AToY);
      AToX:= LinesLen[AToY];
    end;

    //remember no final eol
    bDeleteEol_Alt:= (AFromY=0) and (AToY=Count-1) and (LinesEnds[AToY]=TATLineEnds.None);

    //concat remaining parts of 1st+last lines
    Str:= '';
    if AFromX>0 then
      Str:= LineSub(AFromY, 1, AFromX);
    Str:= Str + LineSub(AToY, AToX+1, MaxInt);
    Lines[AFromY]:= Str;
    LinesEnds[AFromY]:= LinesEnds[AToY];
    
    //del middle lines
    LineBlockDelete(AFromY+1, AToY, false{AForceLast});

    if bDeleteEol then
    begin
      if Count>0 then
        LinesEnds[Count-1]:= TATLineEnds.None;
      ActionDeleteFakeLineAndFinalEol;
    end
    else
    if bDeleteEol_Alt then
    begin
      ActionDeleteFakeLineAndFinalEol;
    end;

    AShift.X:= AFromX-AToX;
    AShift.Y:= -(AToY-AFromY);
  end;

  //count=0 may be after deleting block, if last line has no eol
  if Count=0 then
    ActionAddFakeLineIfNeeded;

  //when deleting multiline block, _cached_ WrapInfo update works incorrect
  if AToY>AFromY then
    EnableCachedWrapinfoUpdate:= false;

  Result:= true;
end;


procedure TATStrings.TextInsertColumnBlock(AX, AY: SizeInt; ABlock: TATStrings; AOverwrite: boolean);
var
  Shift, PosAfter: TPoint;
  BlockUTF8: TStringList;
  i: SizeInt;
begin
  if not IsIndexValid(AY) then exit;
  FChangeBlockActive:= true;

  if Assigned(FOnChangeBlock) then
  begin
    BlockUTF8:= ATStrings_To_StringList(ABlock);
    try
      if AOverwrite then
        DoOnChangeBlock(
          AX, AY,
          AX + Length(ABlock.Lines[0]), AY + ABlock.Count,
          TATBlockChangeKind.DeleteColumn,
          nil);
      DoOnChangeBlock(
        AX, AY,
        0, 0,
        TATBlockChangeKind.InsertColumn,
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
      LineAddRaw('', TATLineEnds.None);
  end;

  FChangeBlockActive:= false;
end;

procedure TATStrings.TextInsertEol(AX, AY: SizeInt; AKeepCaret: boolean; const AStrIndent: atString; out AShift, APosAfter: TPoint);
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

    if (LinesFoldFrom[AY, 0]>0) or
      (LinesFoldFrom[AY, 1]>0) then
      if Assigned(FOnUnfoldLine) then
        FOnUnfoldLine(Self, AY);

    if AX<Length(Str) then
    begin
      StrMove:= Copy(Str, AX+1, MaxInt);
      Delete(Str, AX+1, MaxInt);
      Lines[AY]:= Str;
    end;

    //handle situation when we at non-eol line, this must give
    //inserted line also w/o eol
    NewEnd:= LinesEnds[AY];
    if NewEnd=TATLineEnds.None then
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

procedure TATStrings.TextDeleteLine(AX, AY: SizeInt; out AShift, APosAfter: TPoint);
var
  bOnLastLine: boolean;
begin
  AShift.X:= 0;
  AShift.Y:= 0;
  APosAfter.X:= AX;
  APosAfter.Y:= AY;
  if not IsIndexValid(AY) then Exit;

  AShift.Y:= -1;
  bOnLastLine:= AY=Count-1;

  LineDelete(AY, false{AForceLast});

  if Count=0 then
    LineAddEx('', TATLineEnds.None)
  else
  if bOnLastLine then
  begin
    Dec(AY);
    if AY>=0 then
      LinesEnds[AY]:= TATLineEnds.None;
  end;
end;


