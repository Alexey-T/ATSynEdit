{$ifdef nn}begin end;{$endif}

procedure TATStrings.SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
var
  i: integer;
  Item: TATStringItem;
  SA: AnsiString;
  SW: UnicodeString;
  Sign: AnsiString;
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
    SW:= Item.ItemString + cLineEndStrings[Item.ItemEnd];
    if SW<>'' then
    case AEncoding of
      cEncAnsi:
        begin
          SA:= SW;
          Stream.WriteBuffer(SA[1], Length(SA));
        end;
      cEncUTF8:
        begin
          SA:= UTF8Encode(SW);
          Stream.WriteBuffer(SA[1], Length(SA));
        end;
      cEncWideLE,
      cEncWideBE:
        begin
          if AEncoding=cEncWideBE then
            SW:= SSwapEndian(SW);
          Stream.WriteBuffer(SW[1], Length(SW)*2);
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
    DoClearUndo;
  FModified:= false;
end;

