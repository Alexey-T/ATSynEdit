{$ifdef nn}begin end;{$endif}

procedure TATStrings.SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
var
  Item: TATStringItem;
  SA, Sign: string;
  SWide: UnicodeString;
  bEncoded: boolean;
  i: integer;
begin
  if AWithSignature then
  begin
    Sign:= '';
    case FEncoding of
      cEncUTF8: Sign:= cSignUTF8;
      cEncWideLE: Sign:= cSignWideLE;
      cEncWideBE: Sign:= cSignWideBE;
    end;
    if Sign<>'' then
      Stream.WriteBuffer(Sign[1], Length(Sign));
  end;

  for i:= 0 to Count-1 do
  begin
    Item:= FList[i];
    SA:= Item.ItemString + cLineEndStrings[Item.ItemEnd];
    if SA<>'' then
    case AEncoding of
      cEncAnsi:
        begin
          if FEncodingCodepage='' then
            SA:= SConvertEnc_Utf8ToAnsi(SA)
          else
            SA:= ConvertEncodingFromUTF8(SA, FEncodingCodepage, bEncoded);

          Stream.WriteBuffer(SA[1], Length(SA));
        end;

      cEncUTF8:
        begin
          Stream.WriteBuffer(SA[1], Length(SA));
        end;

      cEncWideLE,
      cEncWideBE:
        begin
          SWide:= UTF8Decode(SA);
          if AEncoding=cEncWideBE then
            SWide:= SSwapEndian(SWide);
          Stream.WriteBuffer(SWide[1], Length(SWide)*2);
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
  fs: TFileStreamUtf8;
  WithSign: boolean;
begin
  WithSign:=
    ((FEncoding in [cEncUTF8]) and FSaveSignUtf8) or
    ((FEncoding in [cEncWideLE, cEncWideBE]) and FSaveSignWide);

  fs:= TFileStreamUtf8.Create(AFilename, fmCreate or fmOpenWrite or fmShareDenyWrite);
  try
    SaveToStream(fs, FEncoding, WithSign);
  finally
    FreeAndNil(fs);
  end;

  DoFinalizeSaving;
end;

procedure TATStrings.DoFinalizeSaving;
begin
  DoClearLineStates(true);

  if not FUndoAfterSave then
    DoClearUndo
  else
  begin
    FUndoList.DeleteUnmodifiedMarks;
    FRedoList.DeleteUnmodifiedMarks;
  end;

  Modified:= false;
end;

