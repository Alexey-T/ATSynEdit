{$ifdef nn}begin end;{$endif}

function SConvertAnsiToUtf8(const SA: string): UTF8String;
begin
  {$ifdef windows}
  case Windows.GetACP of
    1250: Result:= CP1250ToUTF8(SA);
    1251: Result:= CP1251ToUTF8(SA);
    1252: Result:= CP1252ToUTF8(SA);
    1253: Result:= CP1253ToUTF8(SA);
    1254: Result:= CP1254ToUTF8(SA);
    1255: Result:= CP1255ToUTF8(SA);
    1256: Result:= CP1256ToUTF8(SA);
    1257: Result:= CP1257ToUTF8(SA);
    1258: Result:= CP1258ToUTF8(SA);
    437: Result:= CP437ToUTF8(SA);
    else Result:= CP1250ToUTF8(SA);
  end;
  {$else}
  Result:= CP1250ToUTF8(SA);
  {$endif}
end;

function SConvertUtf8ToAnsi(const S: UTF8String): string;
begin
  {$ifdef windows}
  case Windows.GetACP of
    1250: Result:= UTF8ToCP1250(S);
    1251: Result:= UTF8ToCP1251(S);
    1252: Result:= UTF8ToCP1252(S);
    1253: Result:= UTF8ToCP1253(S);
    1254: Result:= UTF8ToCP1254(S);
    1255: Result:= UTF8ToCP1255(S);
    1256: Result:= UTF8ToCP1256(S);
    1257: Result:= UTF8ToCP1257(S);
    1258: Result:= UTF8ToCP1258(S);
    437: Result:= UTF8ToCP437(S);
    else Result:= UTF8ToCP1250(S);
  end;
  {$else}
  Result:= UTF8ToCP1250(S);
  {$endif}
end;



function IsStreamWithSignature(Stream: TStream; const Sign: AnsiString): boolean;
var
  Buf: AnsiString;
begin
  Result:= false;
  if Stream.Size<Length(Sign) then Exit;
  SetLength(Buf, Length(Sign));
  Stream.Position:= 0;
  Stream.ReadBuffer(Buf[1], Length(Sign));
  Stream.Position:= 0;
  Result:= Buf=Sign;
end;

function IsStreamWithUt8NoBom(Stream: TStream; BufSizeKb: word): boolean;
const
  cMinLen = 15;
var
  Buf: PChar;
  Size: integer;
begin
  Result:= false;
  if Stream.Size<cMinLen then Exit;

  if BufSizeKb=0 then BufSizeKb:= 1;
  Size:= BufSizeKb*1024;
  GetMem(Buf, Size);
  try
    FillChar(Buf^, Size, 0);
    Stream.Position:= 0;
    Stream.Read(Buf^, Size-1{trail zero});
    Stream.Position:= 0;
    Result:= IsBufferUtf8(Buf, true);
  finally
    FreeMem(Buf);
  end;
end;

procedure DoDetectStreamEncoding(Stream: TStream;
  out Enc: TATFileEncoding;
  out SignLen: integer;
  out EncWithBom: boolean;
  BufSizeKb: integer);
begin
  Enc:= cEncUTF8;
  EncWithBom:= false;
  SignLen:= 0;

  if IsStreamWithSignature(Stream, cSignUTF8) then
  begin
    Enc:= cEncUTF8;
    EncWithBom:= true;
    SignLen:= Length(cSignUTF8);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSignWideLE) then
  begin
    Enc:= cEncWideLE;
    EncWithBom:= true;
    SignLen:= Length(cSignWideLE);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSignWideBE) then
  begin
    Enc:= cEncWideBE;
    EncWithBom:= true;
    SignLen:= Length(cSignWideBE);
    Exit
  end;

  if IsStreamWithUt8NoBom(Stream, BufSizeKb) then
  begin
    Enc:= cEncUTF8;
    EncWithBom:= false;
    Exit
  end;
end;


procedure TATStrings.LoadFromString(const AText: atString);
var
  MS: TMemoryStream;
  i: integer;
begin
  if ReadOnly then exit;
  Clear;
  if AText='' then
  begin
    ActionAddFakeLineIfNeeded;
    Exit;
  end;

  MS:= TMemoryStream.Create;
  try
    MS.Write(AText[1], Length(AText)*SizeOf(atChar));
    MS.Position:= 0;

    Encoding:= cEncWideLE;
    EncodingDetect:= false;
    LoadFromStream(MS);
    ActionAddFakeLineIfNeeded;
  finally
    FreeAndNil(MS);
  end;

  Modified:= true;
  for i:= 0 to Count-1 do
    SetLineState(i, cLineStateAdded);
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
    while (Result<BufSize) and not IsCharEol(Widechar(_BufferCharCode(Result))) do
      Inc(Result, CharSize);
  end;

var
  NStart, NEnd, Len: integer;
  SA: AnsiString;
  SUtf8: UTF8String;
  SWide: UnicodeString;
  LineEnd: TATLineEnds;
  bWithBom, bEncoded: boolean;
  NPercents: integer;
begin
  Clear;

  Len:= 0;
  if FEncodingDetect then
  begin
    DoDetectStreamEncoding(Stream, FEncoding, Len, bWithBom, FEncodingDetectBufSizeKb);
    case FEncoding of
      cEncUTF8: SaveSignUtf8:= bWithBom;
      cEncWideBE,
      cEncWideLE: SaveSignWide:= bWithBom;
    end;
  end;
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

      if Stream.Size>=cMinSizeForProgress then
      begin
        NPercents:= Int64(NEnd)*100 div Stream.Size;
        if Abs(NPercents-FProgress)>=cMinIncForProgress then
        begin
          FProgress:= NPercents;
          if Assigned(FOnProgress) then
            FOnProgress(Self);
        end;
      end;

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
        LineAddRaw_UTF8_NoUndo('', LineEnd)
      else
      begin
        case FEncoding of
          cEncAnsi:
            begin
              SA:= '';
              SetLength(SA, Len);
              Move(Buf[NStart], SA[1], Len);

              if FEncodingCodepage='' then
                SUtf8:= SConvertAnsiToUTF8(SA)
              else
                SUtf8:= ConvertEncodingToUTF8(SA, FEncodingCodepage, bEncoded);

              LineAddRaw_UTF8_NoUndo(SUtf8, LineEnd);
            end;

          cEncUTF8:
            begin
              SA:= '';
              SetLength(SA, Len);
              Move(Buf[NStart], SA[1], Len);
              LineAddRaw_UTF8_NoUndo(SA, LineEnd);
            end;

          cEncWideLE,
          cEncWideBE:
            begin
              SWide:= '';
              SetLength(SWide, Len div 2);
              Move(Buf[NStart], SWide[1], Len);
              if FEncoding=cEncWideBE then
                SWide:= SSwapEndian(SWide);
              LineAddRaw_UTF8_NoUndo(UTF8Encode(SWide), LineEnd);
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
  Modified:= false;
  FProgress:= 0;
end;


