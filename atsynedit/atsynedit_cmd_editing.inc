{$ifdef nnn}begin end;{$endif}

function TATSynEdit.DoCommand_TextTabulation: TATCommandResults;
var
  N1, N2: integer;
  bBlockSel: boolean;
begin
  //multiline selection?
  //instead of tabulation, do indent
  if FOptKeyTabIndents then
  begin
    if not IsSelRectEmpty then
      bBlockSel:= true
    else
    if Carets.Count<>1 then
      bBlockSel:= false
    else
    begin
      Carets[0].GetSelLines(N1, N2);
      bBlockSel:= (N1>=0) and (N2>N1);
    end;

    if bBlockSel then
      Exit(DoCommand_TextIndentUnindent(true));
  end;

  if FOptTabSpaces then
    Result:= DoCommand_TextInsertTabSpacesAtCarets(FOverwrite)
  else
    Result:= DoCommand_TextInsertAtCarets(#9, false, FOverwrite, false);
end;


function TATSynEdit.DoCommand_TextInsertAtCarets(const AText: atString;
  AKeepCaret, AOvrMode, ASelectThen: boolean): TATCommandResults;
var
  List: TStringList;
  //
  function TextItem(i: integer): atString;
  begin
    if Assigned(List) and (Carets.Count=List.Count) and (i>=0) and (i<List.Count) then
      Result:= UTF8Decode(List[i])
    else
      Result:= AText;
  end;
  //
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  bNeedGroup: boolean;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  List:= nil;

  bNeedGroup:= (Carets.Count>1) or (Carets.IsSelection);
  if bNeedGroup then Strings.BeginUndoGroup;

  DoSelectionDeleteOrReset;

  //list allows to insert each clip-line into one caret
  if (AText<>sLineBreak) and (Length(AText)>1) and (Carets.Count>1) then
  begin
    List:= TStringList.Create;
    List.Text:= UTF8Encode(AText);
  end;

  try
    for i:= Carets.Count-1 downto 0 do
    begin
      Caret:= Carets[i];
      Strings.TextInsert(Caret.PosX, Caret.PosY, TextItem(i), AOvrMode, Shift, PosAfter);
      DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y, PosAfter);

      if not AKeepCaret then
      begin
        Caret.EndX:= IfThen(ASelectThen, Caret.PosX, -1);
        Caret.EndY:= IfThen(ASelectThen, Caret.PosY, -1);
        Caret.PosX:= PosAfter.X;
        Caret.PosY:= PosAfter.Y;
      end;
    end;
  finally
    if Assigned(List) then
      FreeAndNil(List);
  end;

  if bNeedGroup then Strings.EndUndoGroup;

  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_TextInsertTabSpacesAtCarets(AOvrMode: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  StrSpaces: atString;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Strings.BeginUndoGroup;
  try
    DoSelectionDeleteOrReset;

    for i:= Carets.Count-1 downto 0 do
    begin
      Caret:= Carets[i];
      StrSpaces:= StringOfChar(' ', FTabSize - Caret.PosX mod FTabSize);
      Strings.TextInsert(Caret.PosX, Caret.PosY, StrSpaces, AOvrMode, Shift, PosAfter);
      DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y, PosAfter);

      Caret.PosX:= PosAfter.X;
      Caret.PosY:= PosAfter.Y;
      Caret.EndX:= -1;
      Caret.EndY:= -1;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCalcIndentCharsFromPrevLines(AX, AY: integer): integer;
var
  Str: atString;
  NIndent, i: integer;
begin
  Result:= -1;
  if not Strings.IsIndexValid(AY) then Exit;

  //allow smart unindent only if caret on 1st nonspace char
  //(else Bksp must delete 1 char)
  Str:= Strings.Lines[AY];
  NIndent:= SGetIndentChars(Str);
  if not ((AX=NIndent) and (NIndent>0)) then Exit;

  //calc indent of N prev lines.
  //if indent<AX then ok
  for i:= 1 to FOptMaxLinesToCountUnindent do
  begin
    Dec(AY);
    if not Strings.IsIndexValid(AY) then Exit;

    Str:= Strings.Lines[AY];
    NIndent:= SGetIndentChars(Str);
    if NIndent<AX then
      Exit(NIndent);
  end;
end;

function TATSynEdit.DoCommand_TextDeleteLeft(ALen: integer; AAllowUnindent: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  NIndent, NDeleteLen: integer;
  bNeedGroup: boolean;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //selection? delete it, exit.
  if Carets.IsSelection then
  begin
    Result:= DoCommand_TextDeleteSelection;
    Exit
  end;

  bNeedGroup:= Carets.Count>1;
  if bNeedGroup then Strings.BeginUndoGroup;

  for i:= 0 to Carets.Count-1 do
  begin
    Caret:= Carets[i];
    NDeleteLen:= ALen;

    if AAllowUnindent then
    begin
      NIndent:= DoCalcIndentCharsFromPrevLines(Caret.PosX, Caret.PosY);
      if NIndent>=0 then
        if Caret.PosX>NIndent then
          NDeleteLen:= Caret.PosX-NIndent
        else
          NDeleteLen:= Caret.PosX;
    end;

    Strings.TextDeleteLeft(Caret.PosX, Caret.PosY, NDeleteLen, Shift, PosAfter);
    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y, PosAfter);
    Caret.PosX:= PosAfter.X;
    Caret.PosY:= PosAfter.Y;
    Caret.EndX:= -1;
    Caret.EndY:= -1;
  end;

  if bNeedGroup then Strings.EndUndoGroup;

  Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_TextDelete: TATCommandResults;
var
  bColBlock: boolean;
begin
  bColBlock:= not IsSelRectEmpty;
  if bColBlock then
    if FSelRect.Left=FSelRect.Right then
    begin
      DoSelect_None;
      bColBlock:= false;
    end;

  if bColBlock or Carets.IsSelection then
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

      Markers.DeleteInRange(AX1, AY1, AX2, AY2);
      Attribs.DeleteInRange(AX1, AY1, AX2, AY2);

      Strings.TextDeleteRange(AX1, AY1, AX2, AY2, Shift, PosAfter);
      DoCaretsShift(AX1, AY1, Shift.X, Shift.Y, PosAfter);
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
  bNeedGroup: boolean;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //selection? delete it, exit.
  if Carets.IsSelection then
  begin
    Result:= DoCommand_TextDeleteSelection;
    exit
  end;

  bNeedGroup:= Carets.Count>1;
  if bNeedGroup then Strings.BeginUndoGroup;

  for i:= 0 to FCarets.Count-1 do
  begin
    Caret:= FCarets[i];

    //offsetX for carets in line[PosY+1]
    ShiftBelowX:= 0;
    Len:= Strings.LinesLen[Caret.PosY];
    if Caret.PosX=Len then
      ShiftBelowX:= Len;

    Strings.TextDeleteRight(Caret.PosX, Caret.PosY, ALen, Shift, PosAfter);
    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y, PosAfter, ShiftBelowX);
    Caret.PosX:= PosAfter.X;
    Caret.PosY:= PosAfter.Y;
  end;

  if bNeedGroup then Strings.EndUndoGroup;

  Result:= [cResultText, cResultCaretAny];
end;

function TATSynEdit.DoCommand_TextInsertEol(AKeepCaret: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  Str: atString;
  bNeedGroup: boolean;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;
  if ModeOneLine then Exit;

  bNeedGroup:= Carets.Count>1;
  if bNeedGroup then Strings.BeginUndoGroup;

  DoSelectionDeleteOrReset;

  for i:= FCarets.Count-1 downto 0 do
  begin
    Caret:= FCarets[i];
    Str:= GetAutoIndentString(Caret.PosX, Caret.PosY);
    Strings.TextInsertEol(Caret.PosX, Caret.PosY, AKeepCaret, Str, Shift, PosAfter);
    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y, PosAfter);
    Caret.PosX:= PosAfter.X;
    Caret.PosY:= PosAfter.Y;
    //on Enter reset prev column for up/down
    Caret.SavedX:= 0;
  end;

  if bNeedGroup then Strings.EndUndoGroup;

  Result:= [cResultText, cResultCaretBottom];
end;

function TATSynEdit.DoCommand_TextDeleteLines: TATCommandResults;
var
  Caret: TATCaretItem;
  Shift, PosAfter: TPoint;
  YFrom, YTo, i, j: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  Strings.BeginUndoGroup;
  try
    DoCaretsDeleteOnSameLines;

    for i:= FCarets.Count-1 downto 0 do
    begin
      Caret:= FCarets[i];
      Caret.GetSelLines(YFrom, YTo, true);

      //Caret.PosX:= 0;
      Caret.PosY:= YFrom;
      Caret.EndX:= -1;
      Caret.EndY:= -1;

      for j:= YTo downto YFrom do
      begin
        Strings.TextDeleteLine(0, j, Shift, PosAfter);
        DoCaretsShift(0, j, Shift.X, Shift.Y, PosAfter);
      end;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_TextDuplicateLine: TATCommandResults;
var
  Caret: TATCaretItem;
  Y1, Y2, i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  //cmd handles only 1 caret, to handle seltext
  Caret:= Carets[0];
  if not Strings.IsIndexValid(Caret.PosY) then Exit;

  Strings.BeginUndoGroup;
  try
    //universal code, for selection or not
    Caret.GetSelLines(Y1, Y2, true);
    Strings.LinesEnds[Y2]:= Strings.Endings;
    for i:= Y2 downto Y1 do
    begin
      Strings.LineInsert(Y2+1, Strings.Lines[i]);
      Strings.LinesEnds[Y2+1]:= Strings.Endings;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  DoCaretSingleAsIs;
  Result:= [cResultText, cResultCaretTop];
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
const
  cWordJumps: array[boolean] of TATWordJump =
    (cWordjumpToPrev, cWordjumpToNext);
var
  Caret: TATCaretItem;
  Str: atString;
  Shift, PosAfter: TPoint;
  bNeedGroup: boolean;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  bNeedGroup:= Carets.Count>1;
  if bNeedGroup then Strings.BeginUndoGroup;

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
      PosAfter.X:= SFindWordOffset(Str, Caret.PosX, cWordJumps[ANext], false, FOptWordChars);
      if PosAfter.X<>Caret.PosX then
      begin
        System.Delete(Str, Min(Caret.PosX, PosAfter.X)+1, Abs(Caret.PosX-PosAfter.X));
        Strings.Lines[Caret.PosY]:= Str;
        Shift.X:= -Abs(Caret.PosX-PosAfter.X);
        PosAfter.X:= Min(Caret.PosX, PosAfter.X);
      end;
    end;

    DoCaretsShift(Caret.PosX, Caret.PosY, Shift.X, Shift.Y, PosAfter);

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

  if bNeedGroup then Strings.EndUndoGroup;

  if ANext then
    Result:= [cResultText, cResultCaretBottom]
  else
    Result:= [cResultText, cResultCaretTop];
end;

function TATSynEdit.DoCommand_TextIndentUnindent(ARight: boolean): TATCommandResults;
begin
  Result:= [];
  if Strings.Count=0 then exit;
  if Carets.Count=0 then exit;

  if IsSelRectEmpty then
    Result:= DoCommand_TextIndentUnindent_StreamBlock(ARight)
  else
    Result:= DoCommand_TextIndentUnindent_ColumnBlock(ARight);
end;

function TATSynEdit.DoCommand_TextIndentUnindent_StreamBlock(ARight: boolean): TATCommandResults;
var
  NDecSpaces, NMinSpaces, i: integer;
  Str: atString;
  Caret: TATCaretItem;
  NShiftInit, NShift1, NShift2, NShiftChars, NIndent1, NIndent2: integer;
  NBlockBeginX, NBlockBeginY, NBlockEndX, NBlockEndY: integer;
begin
  Caret:= Carets[0];
  Caret.GetSelLines(NBlockBeginY, NBlockEndY, true{Allow no sel});
  if NBlockBeginY<0 then exit;
  if Caret.EndY<0 then
  begin
    Caret.EndX:= Caret.PosX;
    Caret.EndY:= Caret.PosY;
  end;
  NBlockBeginX:= Caret.PosX;
  NBlockEndX:= Caret.EndX;

  if FOptIndentSize=0 then
  begin
    NDecSpaces:= FTabSize;
    NShiftChars:= 1;
  end
  else
  if FOptIndentSize>0 then
  begin
    NDecSpaces:= FOptIndentSize;
    NShiftChars:= FOptIndentSize;
  end
  else
  begin
    NDecSpaces:= Abs(FOptIndentSize)*FTabSize;
    NShiftChars:= Abs(FOptIndentSize);
  end;

  //calc minimal indent of all
  NMinSpaces:= MaxInt;
  for i:= NBlockBeginY to NBlockEndY do
  begin
    Str:= Strings.Lines[i];
    if Trim(Str)='' then Continue;
    NMinSpaces:= Min(NMinSpaces, SGetIndentExpanded(Str, FTabSize));
  end;
  if NMinSpaces=MaxInt then Exit;

  //consider "Unindent keeps align"
  if FOptIndentKeepsAlign then
    if (not ARight) and (NMinSpaces<NDecSpaces) then Exit;

  //calc shifts (emulate Lazarus IDE indent)
  NIndent1:= SGetIndentChars(Strings.Lines[NBlockBeginY]);
  NIndent2:= SGetIndentChars(Strings.Lines[NBlockEndY]);
  NShiftInit:= NShiftChars * IfThen(ARight, 1, -1);
  NShift1:= IfThen((NBlockBeginX>=NIndent1) and (NBlockBeginX>0), NShiftInit, 0);
  NShift2:= IfThen((NBlockEndX>=NIndent2), NShiftInit, 0);

  //do indent
  Strings.BeginUndoGroup;
  try
    for i:= NBlockBeginY to NBlockEndY do
    begin
      Str:= Strings.Lines[i];
      if Trim(Str)='' then Continue;
      Str:= SIndentUnindent(Str, ARight, FOptIndentSize, FTabSize, FOptTabSpaces);
      if Strings.Lines[i]<>Str then
        Strings.Lines[i]:= Str;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  //correct selection
  if FOptIndentMakesWholeLinesSelection then
  begin
    Caret.EndX:= 0;
    Caret.EndY:= NBlockBeginY;
    if Strings.IsIndexValid(NBlockEndY+1) then
    begin
      Caret.PosX:= 0;
      Caret.PosY:= NBlockEndY+1;
    end
    else
    begin
      Caret.PosY:= NBlockEndY;
      Caret.PosX:= Strings.LinesLen[NBlockEndY];
    end;
  end
  else
  begin
    if NBlockBeginX>0 then
      Caret.PosX:= Max(0, NBlockBeginX+NShift1);
    if NBlockEndX>0 then
      Caret.EndX:= Max(0, NBlockEndX+NShift2);
  end;

  Result:= [cResultText, cResultCaretAny];
end;


function TATSynEdit.DoCommand_TextIndentUnindent_ColumnBlock(ARight: boolean): TATCommandResults;
var
  NBlockBeginY, NBlockEndY, NIndentAtCol: integer;
  Str: atString;
  i: integer;
begin
  NBlockBeginY:= Min(SelRect.Top, Strings.Count-1);
  NBlockEndY:= Min(SelRect.Bottom, Strings.Count-1);
  NIndentAtCol:= SelRect.Left;

  //do indent
  Strings.BeginUndoGroup;
  try
    for i:= NBlockBeginY to NBlockEndY do
    begin
      Str:= Strings.Lines[i];
      if Trim(Str)='' then Continue;

      Str:= Copy(Str, 1, NIndentAtCol) + SIndentUnindent(
              Copy(Str, NIndentAtCol+1, MaxInt), ARight, FOptIndentSize,
              FTabSize,
              FOptTabSpaces);

      if Strings.Lines[i]<>Str then
        Strings.Lines[i]:= Str;
    end;
  finally
    Strings.EndUndoGroup;
  end;

  Result:= [cResultText, cResultCaretAny, cResultKeepColumnSel];
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
      case FOptCaretPosAfterPasteColumn of
        cPasteCaretNoChange:
          begin
          end;
        cPasteCaretLeftBottom:
          begin
            Inc(Caret.PosY, Block.Count-1);
          end;
        cPasteCaretRightBottom:
          begin
            Inc(Caret.PosX, Block.LinesLen[0]);
            Inc(Caret.PosY, Block.Count-1);
          end;
        cPasteCaretRightTop:
          begin
            Inc(Caret.PosX, Block.LinesLen[0]);
          end;
        cPasteCaretColumnLeft:
          begin
            DoCaretsColumnToPoint(Caret.PosX, Caret.PosY + Block.Count-1);
          end;
        cPasteCaretColumnRight:
          begin
            Inc(Caret.PosX, Block.LinesLen[0]);
            DoCaretsColumnToPoint(Caret.PosX, Caret.PosY + Block.Count-1);
          end;
      end;
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

  Strings.ChangeBlockActive:= true;
  Strings.DoOnChangeBlock(
    Caret.PosX, Caret.PosY,
    0, Strings.Count,
    cBlockDeleteLines,
    nil
    );

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

  Strings.ChangeBlockActive:= false;

  Result:= [cResultText, cResultCaretBottom];
end;


function TATSynEdit.DoCommand_TextInsertEmptyAboveBelow(ADown: boolean): TATCommandResults;
var
  Caret: TATCaretItem;
  PosAfter: TPoint;
  i: integer;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  DoCaretsDeleteOnSameLines;

  for i:= Carets.Count-1 downto 0 do
  begin
    Caret:= Carets[i];
    Strings.LineInsert(Caret.PosY + IfThen(ADown, 1), '');

    PosAfter.X:= 0;
    PosAfter.Y:= Caret.PosY+IfThen(ADown, 1);

    DoCaretsShift(0, Caret.PosY, 0, 1, PosAfter);

    Caret.PosX:= PosAfter.X;
    Caret.PosY:= PosAfter.Y;
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

  Strings.BeginUndoGroup;
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
  Strings.EndUndoGroup;

  Result:= [cResultText, cResultCaretAny];
end;


function TATSynEdit.DoCommand_TextTrimSpaces(AMode: TATTrimSpaces): TATCommandResults;
begin
  Result:= [];
  if ModeReadOnly then Exit;

  if Strings.ActionTrimSpaces(AMode) then
    Result:= [cResultCaretAny, cResultText];
end;

function TATSynEdit.DoCommand_ForceFinalEndOfLine: TATCommandResults;
begin
  if Strings.ActionEnsureFinalEol then
  begin
    Strings.ActionAddFakeLineIfNeeded;
    Result:= [cResultText];
  end
  else
    Result:= [];
end;

