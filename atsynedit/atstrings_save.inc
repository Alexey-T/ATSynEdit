{$ifdef nn}begin end;{$endif}

procedure TATStrings.SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
var
  i: integer;
  Item: TATStringItem;
  SA: AnsiString;
  SWide: UnicodeString;
  SUtf8: UTF8String;
  Sign: AnsiString;
  bEncoded: boolean;
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
    Item:= TATStringItem(FList[i]);
    SUtf8:= Item.ItemString + cLineEndStrings_UTF8[Item.ItemEnd];
    if SUtf8<>'' then
    case AEncoding of
      cEncAnsi:
        begin
          if FEncodingCodepage='' then
            SA:= SConvertUtf8ToAnsi(SUtf8)
          else
            SA:= ConvertEncodingFromUTF8(SUtf8, FEncodingCodepage, bEncoded);

          Stream.WriteBuffer(SA[1], Length(SA));
        end;

      cEncUTF8:
        begin
          SA:= SUtf8;
          Stream.WriteBuffer(SA[1], Length(SA));
        end;

      cEncWideLE,
      cEncWideBE:
        begin
          SWide:= UTF8Decode(SUtf8);
          if AEncoding=cEncWideBE then
            SWide:= SSwapEndian(SWide);
          Stream.WriteBuffer(SWide[1], Length(SWide)*2);
        end;

      else
        DoEncError;
    end;
  end;
end;

procedure TATStrings.SaveToFile(const AFilename: string);
var
  fs: TFileStreamUtf8;
  WithSign: boolean;
begin
  WithSign:=
    ((FEncoding in [cEncUTF8]) and FSaveSignUtf8) or
    ((FEncoding in [cEncWideLE, cEncWideBE]) and FSaveSignWide);

  fs:= TFileStreamUtf8.Create(AFilename, fmCreate or fmOpenWrite);
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

