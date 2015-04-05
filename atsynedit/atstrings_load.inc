{$ifdef nn}begin end;{$endif}

procedure TATStrings.LoadFromString(const AText: atString);
var
  MS: TMemoryStream;
begin
  Clear;
  if AText='' then Exit;
  MS:= TMemoryStream.Create;
  try
    MS.Write(AText[1], Length(AText)*SizeOf(atChar));
    MS.Position:= 0;

    Encoding:= cEncWideLE;
    EncodingDetect:= false;
    LoadFromStream(MS);
  finally
    FreeAndNil(MS);
  end;
end;

procedure TATStrings.DoLoadFromStream(Stream: TStream);
var
  Buf: PAnsiChar;
  BufSize: int64;
  CharSize: integer;

  function _BufferCharCode(NPos: integer): Word;
  begin
    case FEncoding of
      cEncAnsi,
      cEncUTF8:
        Result:= PByte(Buf)[NPos];
      cEncWideLE:
        Result:= PByte(Buf)[NPos] + $100 * PByte(Buf)[NPos+1];
      cEncWideBE:
        Result:= PByte(Buf)[NPos+1] + $100 * PByte(Buf)[NPos];
      else
        DoEncError;
    end;
  end;

  function _FindNextEol(NPos: integer): integer;
  begin
    Result:= NPos;
    while (Result<BufSize) and not IsCodeEol(_BufferCharCode(Result)) do
      Inc(Result, CharSize);
  end;

var
  NStart, NEnd, Len: integer;
  SA: AnsiString;
  SW: UnicodeString;
  LineEnd: TATLineEnds;
begin
  Clear;

  Len:= 0;
  if FEncodingDetect then
    DoDetectStreamEncoding(Stream, FEncoding, Len);
  CharSize:= cEncodingSize[FEncoding];

  BufSize:= Stream.Size-Len;
  if BufSize<=0 then Exit;

  GetMem(Buf, BufSize);
  try
    Stream.Position:= Len;
    Stream.ReadBuffer(Buf^, BufSize);

    NStart:= 0;
    repeat
      NEnd:= _FindNextEol(NStart);
      Len:= NEnd-NStart;

      //detect+skip Eol
      LineEnd:= cEndNone;
      if (Int64(NEnd)+CharSize<BufSize) and (_BufferCharCode(NEnd)=13) and (_BufferCharCode(NEnd+CharSize)=10) then
      begin
        LineEnd:= cEndWin;
        Inc(NEnd, CharSize*2);
      end
      else
      if (NEnd<BufSize) and (_BufferCharCode(NEnd)=10) then
      begin
        LineEnd:= cEndUnix;
        Inc(NEnd, CharSize);
      end
      else
      if (NEnd<BufSize) and (_BufferCharCode(NEnd)=13) then
      begin
        LineEnd:= cEndMac;
        Inc(NEnd, CharSize);
      end
      else
        Inc(NEnd, CharSize);

      if Len=0 then
        LineAddRaw('', LineEnd)
      else
      begin
        case FEncoding of
          cEncAnsi:
            begin
              SA:= '';
              SetLength(SA, Len);
              Move(Buf[NStart], SA[1], Len);
              LineAddRaw(SA, LineEnd);
            end;

          cEncUTF8:
            begin
              SA:= '';
              SetLength(SA, Len);
              Move(Buf[NStart], SA[1], Len);
              SW:= UTF8Decode(SA);
              LineAddRaw(SW, LineEnd);
            end;

          cEncWideLE,
          cEncWideBE:
            begin
              SW:= '';
              SetLength(SW, Len div 2);
              Move(Buf[NStart], SW[1], Len);
              if FEncoding=cEncWideBE then
                SW:= SSwapEndian(SW);
              LineAddRaw(SW, LineEnd);
            end;

          else
            DoEncError;
        end;
      end;

      NStart:= NEnd;
      if (NStart>=BufSize) then Break;
    until false;

  finally
    FreeMem(Buf);
  end;
end;

procedure TATStrings.LoadFromStream(Stream: TStream);
begin
  DoClearUndo(true);
  DoLoadFromStream(Stream);
  DoFinalizeLoading;
end;


procedure TATStrings.LoadFromFile(const Filename: string);
var
  fs: TFileStreamUtf8;
begin
  fs:= TFileStreamUtf8.Create(Filename, fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TATStrings.DoFinalizeLoading;
begin
  DoDetectEndings;
  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);
  DoClearUndo;
end;


