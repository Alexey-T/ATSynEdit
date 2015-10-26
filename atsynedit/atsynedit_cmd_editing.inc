{$ifdef nnn}begin end;{$endif}

procedure TATSynEdit.DoCaretsShift(APosX, APosY: integer; AShiftX,
  AShiftY: integer; AShiftBelowX: integer);
var
  iStart, i: integer;
  Caret: TATCaretItem;
begin
  if APosX<0 then Exit;
  if APosY<0 then Exit;

  iStart:= FCarets.IndexOfPosYAvg(APosY);
  if iStart<0 then Exit;

  for i:= iStart to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];

    //carets below src, apply ShiftY/ShiftBelowX
    if Caret.PosY>APosY then
    begin
      if AShiftY=0 then Exit;

      if Caret.PosY=APosY+1 then
        Inc(Caret.PosX, AShiftBelowX);

      Inc(Caret.PosY, AShiftY);
    end
    else
    //carets on same line as src, apply ShiftX
    begin
      if Caret.PosX>APosX then
        Inc(Caret.PosX, AShiftX);
    end;

    //same, but for EndX/EndY
    if Caret.EndY>APosY then
    begin
      if Caret.EndY=APosY+1 then
        Inc(Caret.EndX, AShiftBelowX);

      Inc(Caret.EndY, AShiftY);
    end
    else
    begin
      if Caret.EndX>APosX then
        Inc(Caret.EndX, AShiftX);
    end;

    if Caret.PosX<0 then Caret.PosX:= 0;
    if Caret.PosY<0 then Caret.PosY:= 0;
  end;
end;

function TATSynEdit.DoCommand_TextTabulation: TATCommandResults;
var
  Str: atString;
  N1, N2: integer;
begin
  //multiline selection?
  //instead of tabulation, do indent
  if FOptKeyTabIndents then
    if Carets.Count=1 then
    begin
      Carets[0].GetSelLines(N1, N2);
      if (N1>=0) and (N2>N1) then
      begin
        Result:= DoCommand_TextIndentUnindent(true);
        Exit
      end;
    end;

  if FOptTabSpaces then
    Str:= StringOfChar(' ', FTabSize)
  else
    Str:= #9;
  Result:= DoCommand_TextInsertAtCarets(Str, false, FOverwrite, false);
end;


function TATSynEdit.DoCommand_TextInsertAtCarets(const AText: atString;
  AKeepCaret, AOvrMode, ASelectThen: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  List: TStringList;
  i: integer;
  //
  function TextItem(i: integer): atString;
  begin
    if Assigned(List) and (Carets.Count=List.Count) and (i>=0) and (i<List.Count) then
      Result:= UTF8Decode(List[i])
    else
      Result:= AText;
  end;
  //
begin
  Result:= [];
  if ModeReadOnly then Exit;

  try
    Strings.BeginUndoGroup;
    DoSelectionDeleteOrReset;

    //list allows to insert each clip-line into one caret
    List:= nil;
    if (AText<>sLineBreak) and (Length(AText)>1) and (Carets.Count>1) then
    begin
      List:= TStringList.Create;
      List.Text:= UTF8Encode(AText);
    end;

    try
      for i:= FCarets.Count-1 downto 0 do
      begin
        Caret:= FCarets[i];
        Strings.TextInsert(Caret.PosX, Caret.PosY, TextItem(i), AOvrMode, Shift, PosAfter);
        DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y);

        if not AKeepCaret then
        begin
          if ASelectThen then
          begin
            Caret.EndX:= Caret.PosX;
            Caret.EndY:= Caret.PosY;
          end
          else
          begin
            Caret.EndX:= -1;
            Caret.EndY:= -1;
          end;
          Caret.PosX:= PosAfter.X;
          Caret.PosY:= PosAfter.Y;
        end;
      end;
    finally
      if Assigned(List) then
        FreeAndNil(List);
    end;
  finally
    Strings.EndUndoGroup;
  end;

  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_TextDeleteLeft(ALen: integer; AAllowUnindent: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  Str: atString;
  NIndent, NDeleteLen: integer;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //first, see if selection exists.
  //yes: clear selection, exit. no: delete text.
  if Carets.IsSelection then
  begin
    Result:= DoCommand_TextDeleteSelection;
    Exit
  end;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];
    NDeleteLen:= ALen;

    if AAllowUnindent then
    //do only if caret on 1st nonspace char (Delphi)
    begin
      Str:= Strings.Lines[Caret.PosY];
      NIndent:= SGetIndentChars(Str);
      if (Caret.PosX=NIndent) and (NIndent>0) then
      begin
        //on 1st line: delete to 0
        if Caret.PosY=0 then
          NDeleteLen:= NIndent
        else
        begin
          Str:= Strings.Lines[Caret.PosY-1];
          NIndent:= SGetIndentChars(Str);
          //from indent of prev line: delete to 0
          if Caret.PosX=NIndent then
            NDeleteLen:= NIndent
          else
          //from bigger indent: delete to prev indent
          if Caret.PosX>NIndent then
            NDeleteLen:= (Caret.PosX-NIndent);
        end;
      end;
    end;

    Strings.TextDeleteLeft(Caret.PosX, Caret.PosY, NDeleteLen, Shift, PosAfter);
    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y);
    Caret.PosX:= PosAfter.X;
    Caret.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDelete: TATCommandResults;
begin
  if not IsSelRectEmpty or Carets.IsSelection then
    Result:= DoCommand_TextDeleteSelection
  else
    Result:= DoCommand_TextDeleteRight(1);
end;

function TATSynEdit.DoCommand_TextDeleteSelection: TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  i: integer;
  AX1, AY1, AX2, AY2: integer;
  bSel: boolean;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  if not IsSelRectEmpty and not OptCaretManyAllowed then
  begin
    DoSelectionDeleteColumnBlock;
    Result:= [cResultText, cResultCaretTop];
    Exit
  end;

  if not Carets.IsSelection then Exit;

  Strings.BeginUndoGroup;
  try
    for i:= FCarets.Count-1 downto 0 do
    begin
      Caret:= Carets[i];
      Caret.GetRange(AX1, AY1, AX2, AY2, bSel);
      if not bSel then Continue;

      Strings.TextDeleteRange(AX1, AY1, AX2, AY2, Shift, PosAfter);
      DoCaretsShift(AX1, AY1, Shift.X, Shift.Y);
      Caret.PosX:= PosAfter.X;
      Caret.PosY:= PosAfter.Y;
      Caret.EndX:= -1;
      Caret.EndY:= -1;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  Result:= [cResultText, cResultCaretTop];
end;


function TATSynEdit.DoCommand_TextDeleteRight(ALen: integer): TATCommandResults;
var
  Caret: TATCaretItem;
  i, Len, ShiftBelowX: integer;
  Shift, PosAfter: TPoint;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  DoSelectionDeleteOrReset;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];

    //offsetX for carets in line[PosY+1]
    ShiftBelowX:= 0;
    Len:= Length(Strings.Lines[Caret.PosY]);
    if Caret.PosX=Len then
      ShiftBelowX:= Len;

    Strings.TextDeleteRight(Caret.PosX, Caret.PosY, ALen, Shift, PosAfter);
    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y, ShiftBelowX);
    Caret.PosX:= PosAfter.X;
    Caret.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextInsertEol(AKeepCaret: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  i: integer;
  Str: atString;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  if ModeOneLine then Exit;

  DoSelectionDeleteOrReset;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Caret:= FCarets[i];
    Str:= GetAutoIndentString(Caret.PosX, Caret.PosY);
    Strings.TextInsertEol(Caret.PosX, Caret.PosY, AKeepCaret, Str, Shift, PosAfter);
    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y);
    Caret.PosX:= PosAfter.X;
    Caret.PosY:= PosAfter.Y;
  end;

  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_TextDeleteLines: TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Strings.BeginUndoGroup;
  try
    DoCaretsDeleteOnSameLines;

    for i:= FCarets.Count-1 downto 0 do
    begin
      Caret:= FCarets[i];
      Strings.TextDeleteLine(Caret.PosX, Caret.PosY, Shift, PosAfter);
      DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y);
      Caret.PosX:= PosAfter.X;
      Caret.PosY:= PosAfter.Y;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_TextDuplicateLine: TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Strings.BeginUndoGroup;
  try
    DoCaretsDeleteOnSameLines;

    for i:= FCarets.Count-1 downto 0 do
    begin
      Caret:= FCarets[i];
      Strings.TextDuplicateLine(Caret.PosX, Caret.PosY, Shift, PosAfter);
      DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y);
      Caret.PosX:= PosAfter.X;
      Caret.PosY:= PosAfter.Y;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDeleteToLineBegin: TATCommandResults;
var
  Caret: TATCaretItem;
  Str: atString;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Caret:= FCarets[i];
    Str:= Strings.Lines[Caret.PosY];
    Delete(Str, 1, Caret.PosX);
    Strings.Lines[Caret.PosY]:= Str;
    Caret.PosX:= 0;
  end;

  Result:= [cResultText, cResultCaretLeft];
end;

function TATSynEdit.DoCommand_TextDeleteToLineEnd: TATCommandResults;
var
  Caret: TATCaretItem;
  Str: atString;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  DoCaretsDeleteOnSameLines;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Caret:= FCarets[i];
    Str:= Strings.Lines[Caret.PosY];
    if Caret.PosX<Length(Str) then
    begin
      Delete(Str, Caret.PosX+1, MaxInt);
      Strings.Lines[Caret.PosY]:= Str;
    end;
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextDeleteWord(ANext: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Str: atString;
  Shift, PosAfter: TPoint;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Caret:= FCarets[i];
    if not Strings.IsIndexValid(Caret.PosY) then Continue;
    Str:= Strings.Lines[Caret.PosY];
    Shift.X:= 0;
    Shift.Y:= 0;
    PosAfter.X:= Caret.PosX;
    PosAfter.Y:= Caret.PosY;

    //delete to prev line?
    if (Caret.PosX=0) and (not ANext) then
    begin
      Strings.TextDeleteLeft(Caret.PosX, Caret.PosY, 1, Shift, PosAfter);
    end
    else
    //delete to next line?
    if (Caret.PosX>=Length(Str)) and ANext then
    begin
      Strings.TextDeleteRight(Caret.PosX, Caret.PosY, 1, Shift, PosAfter);
    end
    else
    //jump from beyond eol to eol?
    if (Caret.PosX>Length(Str)) and (not ANext) then
    begin
      Caret.PosX:= Length(Str);
    end
    else
    //delete inside line?
    if (Caret.PosX<=Length(Str)) then
    begin
      PosAfter.X:= SFindWordOffset(Str, Caret.PosX, ANext, false, FOptWordChars);
      if PosAfter.X<>Caret.PosX then
      begin
        System.Delete(Str, Min(Caret.PosX, PosAfter.X)+1, Abs(Caret.PosX-PosAfter.X));
        Strings.Lines[Caret.PosY]:= Str;
        Shift.X:= -Abs(Caret.PosX-PosAfter.X);
        PosAfter.X:= Min(Caret.PosX, PosAfter.X);
      end;
    end;

    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y);

    if ((Caret.PosX<>PosAfter.X) or (Caret.PosY<>PosAfter.Y)) and
      (FCarets.IndexOfPosXY(PosAfter.X, PosAfter.Y)>=0) then
    begin
      if FCarets.Count>1 then
        FCarets.Delete(i);
    end
    else
    begin
      Caret.PosX:= PosAfter.X;
      Caret.PosY:= PosAfter.Y;
    end;
  end;

  if ANext then
    Result:= [cResultText, cResultCaretBottom]
  else
    Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_TextIndentUnindent(ARight: boolean): TATCommandResults;
var
  Y1, Y2: integer;
  NDecSpaces, NMinSpaces, i: integer;
  Str: atString;
  Caret: TATCaretItem;
begin
  Result:= [];
  DoCaretSingleAsIs;

  Caret:= Carets[0];
  Caret.GetSelLines(Y1, Y2);
  if Y1<0 then Exit;

  if FOptIndentSize>=0 then
    NDecSpaces:= FOptIndentSize
  else
    NDecSpaces:= Abs(FOptIndentSize)*FTabSize;

  //calc minimal indent of all
  NMinSpaces:= MaxInt;
  for i:= Y1 to Y2 do
  begin
    Str:= Strings.Lines[i];
    if Trim(Str)='' then Continue;
    NMinSpaces:= Min(NMinSpaces, SGetIndentExpanded(Str, FTabSize));
  end;
  if NMinSpaces=MaxInt then Exit;

  //consider "Unindent keeps align"
  if FOptIndentKeepsAlign then
    if (not ARight) and (NMinSpaces<NDecSpaces) then Exit;

  //do indent
  Strings.BeginUndoGroup;
  try
    for i:= Y1 to Y2 do
    begin
      Str:= Strings.Lines[i];
      if Trim(Str)='' then Continue;
      Str:= SIndentUnindent(Str, ARight, FOptIndentSize, FTabSize);
      if Strings.Lines[i]<>Str then
        Strings.Lines[i]:= Str;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  //correct selection: select entire lines y1..y2
  Caret.EndX:= 0;
  Caret.EndY:= Y1;
  Caret.PosX:= Length(Strings.Lines[Y2]);
  Caret.PosY:= Y2;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_Undo: TATCommandResults;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Strings.Undo(FOptUndoGrouped);
  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_Redo: TATCommandResults;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Strings.SetGroupMark;
  Strings.Redo(FOptUndoGrouped);
  Result:= [cResultText, cResultCaretBottom];
end;


function TATSynEdit.DoCommand_TextInsertColumnBlockOnce(const AText: atString;
  AKeepCaret: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Block: TATStrings;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //cannot handle carets/selections for colblock
  DoCaretSingleAsIs;
  DoSelect_None;
  Caret:= FCarets[0];

  Block:= TATStrings.Create;
  try
    Block.LoadFromString(AText);
    Block.ActionDeleteFakeLine;
    if Block.Count=0 then Exit;

    Strings.TextInsertColumnBlock(Caret.PosX, Caret.PosY, Block, FOverwrite);
    if not AKeepCaret then
      Inc(Caret.PosY, Block.Count-1);
  finally
    FreeAndNil(Block);
  end;

  Result:= [cResultText, cResultCaretBottom];
end;


function TATSynEdit.DoCommand_TextDeleteToFileEnd: TATCommandResults;
var
  Caret: TATCaretItem;
  Str: atString;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  DoCaretSingleAsIs;

  Caret:= FCarets[0];
  Str:= Strings.Lines[Caret.PosY];
  if Caret.PosX<Length(Str) then
  begin
    Delete(Str, Caret.PosX+1, MaxInt);
    Strings.Lines[Caret.PosY]:= Str;
  end;

  for i:= Strings.Count-1 downto Caret.PosY+1 do
    Strings.LineDelete(i);

  if Caret.PosY>=Strings.Count-1 then
    Strings.LinesEnds[Caret.PosY]:= cEndNone;

  Result:= [cResultText, cResultCaretBottom];
end;


function TATSynEdit.DoCommand_TextInsertEmptyAboveBelow(ADown: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  DoCaretsDeleteOnSameLines;

  for i:= Carets.Count-1 downto 0 do
  begin
    Caret:= Carets[i];
    Strings.LineInsert(Caret.PosY + IfThen(ADown, 1), '');
    DoCaretsShift(0, Caret.PosY, 0, 1);

    Caret.PosX:= 0;
    if ADown then
      Inc(Caret.PosY);
  end;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextChangeCase(AMode: TATCaseConvert): TATCommandResults;
var
  Caret: TATCaretItem;
  Str1, Str2: atString;
  X1, Y1, X2, Y2: integer;
  Shift, PosAfter: TPoint;
  bSel: boolean;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  for i:= Carets.Count-1 downto 0 do
  begin
    Caret:= Carets[i];
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    if not bSel then
    begin
      SFindWordBounds(Strings.Lines[Caret.PosY], Caret.PosX, X1, X2, FOptWordChars);
      if X1<0 then Continue;
      Y1:= Caret.PosY;
      Y2:= Caret.PosY;
    end;

    Str1:= Strings.TextSubstring(X1, Y1, X2, Y2);
    case AMode of
      cCaseLower: Str2:= UnicodeLowerCase(Str1);
      cCaseUpper: Str2:= UnicodeUpperCase(Str1);
      cCaseTitle: Str2:= SCaseTitle(Str1, FOptWordChars);
      cCaseInvert: Str2:= SCaseInvert(Str1);
      cCaseSentence: Str2:= SCaseSentence(Str1, FOptWordChars);
    end;
    if Str1=Str2 then Continue;

    Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
    Strings.TextInsert(X1, Y1, Str2, false, Shift, PosAfter);
  end;

  Result:= [cResultText, cResultCaretAny];
end;


procedure TATSynEdit.DoCommentSelectionLines(Act: TATCommentAction; const AComment: atString);
var
  Caret: TATCaretItem;
  L: TStringList;
  NFrom, NTo, i: integer;
  Shift, PosAfter: TPoint;
  bChange: boolean;
begin
  if Carets.Count=0 then exit;
  Caret:= Carets[0];
  Caret.GetSelLines(NFrom, NTo, true);
  if NFrom<0 then exit;
  if NTo<0 then exit;

  L:= TStringList.Create;
  try
    for i:= NFrom to NTo do
      L.Add(Utf8Encode(Strings.Lines[i]));

    bChange:= SCommentLineAction(L, AComment, Act);
    if not bChange then exit;
    Assert(L.Count=(NTo-NFrom+1), 'DoCommentSel changed line count');

    for i:= NFrom to NTo do
      Strings.Lines[i]:= Utf8Decode(L[i-NFrom]);
  finally
    L.Free;
  end;

  DoEventChange;
  Update(true);
end;

function TATSynEdit.DoCommand_TextTrimSpaces(AMode: TATTrimSpaces
  ): TATCommandResults;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  if Strings.ActionTrimSpaces(AMode) then
    Result:= [cResultCaretAny, cResultText];
end;

