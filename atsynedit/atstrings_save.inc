{$ifdef nn}begin end;{$endif}

const
  cLineEndMatrix: array[TATLineEnds, TATFileEncoding] of string = (
    ('', '', '', '', '', ''),
    (#13#10, #13#10, #13#0#10#0, #0#13#0#10, #13#0#0#0#10#0#0#0, #0#0#0#13#0#0#0#10),
    (#10, #10, #10#0, #0#10, #10#0#0#0, #0#0#0#10),
    (#13, #13, #13#0, #0#13, #13#0#0#0, #0#0#0#13)
  );

procedure TATStrings.SaveToStream(AStream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
var
  Item: PATStringItem;
  ItemEnd: TATLineEnds;
  SAnsi, Sign: string;
  SWide: UnicodeString;
  SQuad: UCS4String;
  NLast, i: integer;
  bCancel: boolean;
begin
  //AStream.Size:= 0;
  //AStream.Seek(0, soFromBeginning);

  EncConvErrorMode:= eemException;

  if AWithSignature then
  begin
    case FEncoding of
      TATFileEncoding.UTF8:
        Sign:= cSignUTF8;
      TATFileEncoding.UTF16LE:
        Sign:= cSignWideLE;
      TATFileEncoding.UTF16BE:
        Sign:= cSignWideBE;
      TATFileEncoding.UTF32LE:
        Sign:= cSign32LE;
      TATFileEncoding.UTF32BE:
        Sign:= cSign32BE;
      else
        Sign:= '';
    end;
    if Sign<>'' then
      AStream.WriteBuffer(Sign[1], Length(Sign));
  end;

  NLast:= Count-1;
  for i:= 0 to NLast do
  begin
    Item:= FList.GetItem(i);

    //write line without line-ending
    if Item^.CharLen>0 then
    case AEncoding of
      TATFileEncoding.ANSI:
        begin
          if Item^.Ex.Wide then
          begin
            SWide:= Item^.Line;
            SAnsi:= UTF8Encode(SWide);
            SAnsi:= EncConvertFromUTF8(SAnsi, FEncodingCodepage);
            AStream.WriteBuffer(SAnsi[1], Length(SAnsi));
          end
          else
          begin
            AStream.WriteBuffer(Item^.Buf[1], Length(Item^.Buf));
          end;
        end;

      TATFileEncoding.UTF8:
        begin
          if Item^.Ex.Wide then
          begin
            SWide:= Item^.Line;
            SAnsi:= UTF8Encode(SWide);
            AStream.WriteBuffer(SAnsi[1], Length(SAnsi));
          end
          else
          begin
            AStream.WriteBuffer(Item^.Buf[1], Length(Item^.Buf));
          end;
        end;

      TATFileEncoding.UTF16LE,
      TATFileEncoding.UTF16BE:
        begin
          SWide:= Item^.Line;
          if AEncoding=TATFileEncoding.UTF16BE then
            SSwapEndianWide(SWide);
          AStream.WriteBuffer(SWide[1], Length(SWide)*2);
        end;

      TATFileEncoding.UTF32LE,
      TATFileEncoding.UTF32BE:
        begin
          SWide:= Item^.Line;
          SQuad:= UnicodeStringToUCS4String(SWide);
          if AEncoding=TATFileEncoding.UTF32BE then
            SSwapEndianUCS4(SQuad);
          AStream.WriteBuffer(SQuad[0], (Length(SQuad)-1)*4);
        end;

      else
        DoEncError;
    end;

    //write line-ending as additional string
    ItemEnd:= Item^.LineEnds;
    //don't break the file if some middle ending is unset
    if (ItemEnd=TATLineEnds.None) and (i<NLast) then
      ItemEnd:= FEndings;

    if ItemEnd<>TATLineEnds.None then
    begin
      SAnsi:= cLineEndMatrix[ItemEnd, AEncoding];
      AStream.WriteBuffer(SAnsi[1], Length(SAnsi));
    end;

    if (i mod TATEditorOptions.ProgressSaveLines) = TATEditorOptions.ProgressSaveLines div 2 then
    begin
      FProgressKind:= TATStringsProgressKind.Saving;
      FProgressValue:= Int64(i)*100 div NLast;
      bCancel:= false;
      if Assigned(FOnProgress) then
        FOnProgress(Self, bCancel);
    end;
  end;

  FProgressValue:= 0;
  FProgressKind:= TATStringsProgressKind.None;
end;

function TATStrings.IsSavingWithSignature: boolean;
begin
  case FEncoding of
    TATFileEncoding.ANSI:
      Result:= false;
    TATFileEncoding.UTF8:
      Result:= FSaveSignUtf8;
    TATFileEncoding.UTF16LE,
    TATFileEncoding.UTF16BE,
    TATFileEncoding.UTF32LE,
    TATFileEncoding.UTF32BE:
      Result:= FSaveSignWide;
    else
      raise Exception.Create('Unknown encoding on saving');
  end;
end;

procedure TATStrings.SaveToFile(const AFilename: string);
var
  fs: TBufferedFileStream; //improves saving speed a lot
  NMode: word;
begin
  NMode:= fmOpenReadWrite or fmShareDenyWrite;
  //don't set fmCreate for existing file, to keep NTFS file streams
  if not FileExists(AFilename) then
    NMode:= NMode or fmCreate;

  fs:= TBufferedFileStream.Create(AFilename, NMode);
  try
    SaveToStream(fs, FEncoding, IsSavingWithSignature);
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

