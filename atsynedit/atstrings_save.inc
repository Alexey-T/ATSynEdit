{$ifdef nn}begin end;{$endif}

procedure TATStrings.SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
var
  Item: PATStringItem;
  ItemEnd: TATLineEnds;
  SAnsi, Sign: string;
  SWide: UnicodeString;
  SQuad: UCS4String;
  NLast, i: integer;
begin
  EncConvErrorMode:= eemException;

  if AWithSignature then
  begin
    case FEncoding of
      cEncUTF8:
        Sign:= cSignUTF8;
      cEncWideLE:
        Sign:= cSignWideLE;
      cEncWideBE:
        Sign:= cSignWideBE;
      cEnc32LE:
        Sign:= cSign32LE;
      cEnc32BE:
        Sign:= cSign32BE;
      else
        Sign:= '';
    end;
    if Sign<>'' then
      Stream.WriteBuffer(Sign[1], Length(Sign));
  end;

  NLast:= Count-1;
  for i:= 0 to NLast do
  begin
    Item:= FList.GetItem(i);

    ItemEnd:= Item^.LineEnds;
    //don't break file if ending of some non-last line is unset
    if i<NLast then
      if ItemEnd=cEndNone then
        ItemEnd:= FEndings;

    SWide:= Item^.Line + cLineEndStrings[ItemEnd];

    if SWide<>'' then
    case AEncoding of
      cEncAnsi:
        begin
          SAnsi:= UTF8Encode(SWide);
          SAnsi:= EncConvertFromUTF8(SAnsi, FEncodingCodepage);
          Stream.WriteBuffer(SAnsi[1], Length(SAnsi));
        end;

      cEncUTF8:
        begin
          SAnsi:= UTF8Encode(SWide);
          Stream.WriteBuffer(SAnsi[1], Length(SAnsi));
        end;

      cEncWideLE,
      cEncWideBE:
        begin
          if AEncoding=cEncWideBE then
            SSwapEndianWide(SWide);
          Stream.WriteBuffer(SWide[1], Length(SWide)*2);
        end;

      cEnc32LE,
      cEnc32BE:
        begin
          SQuad:= UnicodeStringToUCS4String(SWide);
          if AEncoding=cEnc32BE then
            SSwapEndianUCS4(SQuad);
          Stream.WriteBuffer(SQuad[0], (Length(SQuad)-1)*4);
        end;

      else
        DoEncError;
    end;

    if (i mod cStringsProgressSaveLines) = (cStringsProgressSaveLines div 2) then
    begin
      FProgressKind:= cStringsProgressSaving;
      FProgressValue:= Int64(i)*100 div Count;
      if Assigned(FOnProgress) then
        FOnProgress(Self);
    end;
  end;

  FProgressValue:= 0;
  FProgressKind:= cStringsProgressNone;
end;

procedure TATStrings.SaveToFile(const AFilename: string);
var
  fs: TFileStream;
  WithSign: boolean;
begin
  WithSign:=
    ((FEncoding in [cEncUTF8]) and FSaveSignUtf8) or
    ((FEncoding in [cEncWideLE, cEncWideBE, cEnc32LE, cEnc32BE]) and FSaveSignWide);

  fs:= TFileStream.Create(AFilename, fmCreate or fmOpenWrite or fmShareDenyWrite);
  try
    SaveToStream(fs, FEncoding, WithSign);
  finally
    FreeAndNil(fs);
  end;

  DoFinalizeSaving;
end;

procedure TATStrings.DoFinalizeSaving;
begin
  ClearLineStates(true);

  if not FUndoAfterSave then
    ClearUndo
  else
  begin
    FUndoList.DeleteUnmodifiedMarks;
    FRedoList.DeleteUnmodifiedMarks;
  end;

  Modified:= false;
end;

