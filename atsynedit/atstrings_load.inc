{$ifdef nn}begin end;{$endif}

function SConvertEnc_AnsiToUtf8(const SA: string): string;
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

function SConvertEnc_Utf8ToAnsi(const S: string): string;
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

function IsStreamWithSignature(Stream: TStream; const Sign: string): boolean;
var
  Buf: string;
begin
  Result:= false;
  if Stream.Size<Length(Sign) then Exit;
  SetLength(Buf, Length(Sign));
  Stream.Position:= 0;
  Stream.ReadBuffer(Buf[1], Length(Sign));
  Stream.Position:= 0;
  Result:= Buf=Sign;
end;


function IsByteAscii(value: byte): boolean; inline;
begin
  case value of
    9, 10, 13,
    32..126:
      Result:= true;
    else
      Result:= false;
  end;

end;


function IsStreamWithUtf16NoBom(Stream: TStream; BufSizeWords: integer; out IsLE: boolean): boolean;
var
  Buf: array[0..100] of word;
  NSize, i: integer;
  NWord: Word;
  FlagBE_0, FlagBE,
  FlagLE_0, FlagLE: boolean;
begin
  Result:= false;
  NSize:= BufSizeWords*2;
  if Stream.Size<NSize then Exit;

  if BufSizeWords<2 then BufSizeWords:= 2;
  if BufSizeWords>High(Buf) then BufSizeWords:= High(Buf);

  FillChar(Buf, SizeOf(Buf), 0);
  Stream.Position:= 0;
  Stream.Read(Buf, NSize);
  Stream.Position:= 0;

  NWord:= Buf[0];
  FlagLE_0:= (Hi(NWord)=0) and IsByteAscii(Lo(NWord));
  FlagBE_0:= (Lo(NWord)=0) and IsByteAscii(Hi(NWord));
  if not FlagLE_0 and not FlagBE_0 then exit;

  for i:= 1 to BufSizeWords-1 do
  begin
    NWord:= Buf[i];
    FlagLE:= (Hi(NWord)=0) and IsByteAscii(Lo(NWord));
    FlagBE:= (Lo(NWord)=0) and IsByteAscii(Hi(NWord));
    if not FlagLE and not FlagBE then exit;
    if FlagLE<>FlagLE_0 then exit;
    if FlagBE<>FlagBE_0 then exit;
  end;

  Result:= true;
  IsLE:= FlagLE_0;
end;


function IsStreamWithUt8NoBom(Stream: TStream; BufSizeKb: word): boolean;
const
  cMinLen = 10;
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
  BufSizeKb: integer;
  BufSizeWords: integer;
  DefaultIsUtf8: boolean);
var
  IsLE: boolean;
begin
  if DefaultIsUtf8 then
  begin
    Enc:= cEncUTF8;
    EncWithBom:= false;
  end
  else
  begin
    Enc:= cEncAnsi;
    EncWithBom:= false;
  end;

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

  if IsStreamWithUtf16NoBom(Stream, BufSizeWords, IsLE) then
  begin
    if IsLE then
      Enc:= cEncWideLE
    else
      Enc:= cEncWideBE;
    Exit
  end;
end;


procedure TATStrings.LoadFromString(const AText: atString);
var
  MS: TMemoryStream;
  Str: atString;
  i: integer;
begin
  if ReadOnly then exit;
  Clear;
  if AText='' then
  begin
    ActionAddFakeLineIfNeeded;
    Exit;
  end;

  if OneLine then
  begin
    Str:= AText;
    SDeleteFromEol(Str);
    LineAddEx(Str, cEndNone);
  end
  else
  begin
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
  end;

  Modified:= true;
  for i:= 0 to Count-1 do
    SetLineState(i, cLineStateAdded);
end;

type
  TATCharEnding = (
    cCharNotEnding,
    cCharEnding10,
    cCharEnding13
    );

procedure TATStrings.DoLoadFromStream(Stream: TStream);
var
  Buf: PAnsiChar;
  BufSize: int64;
  CharSize: integer;
  //------
  function _GetEnding(APos: integer): TATCharEnding;
  var
    code: word;
    p: PByte absolute Buf;
  begin
    case FEncoding of
      cEncAnsi,
      cEncUTF8:
        code:= p[APos];
      cEncWideLE:
        code:= p[APos] + p[APos+1] shl 8;
      cEncWideBE:
        code:= p[APos+1] + p[APos] shl 8;
      else
        code:= 0;
    end;

    case code of
      10: Result:= cCharEnding10;
      13: Result:= cCharEnding13;
      else Result:= cCharNotEnding;
    end;
  end;
  //------
  procedure _FindNextEol(AFromPos: integer;
    out AFoundPos: integer;
    out AEnding: TATLineEnds);
  begin
    AFoundPos:= AFromPos;
    AEnding:= cEndNone;
    repeat
      if AFoundPos>=BufSize then exit;

      case _GetEnding(AFoundPos) of
        cCharNotEnding:
          begin
            Inc(AFoundPos, CharSize);
          end;
        cCharEnding10:
          begin
            AEnding:= cEndUnix;
            exit
          end;
        cCharEnding13:
          begin
            if AFoundPos+CharSize<BufSize then
              if _GetEnding(AFoundPos+CharSize)=cCharEnding10 then
                AEnding:= cEndWin
              else
                AEnding:= cEndMac;
            exit
          end;
      end;
    until false;
  end;
  //------
var
  NStart, NEnd, NCounted, Len: integer;
  SA, SUtf8: string;
  SWide: UnicodeString;
  LineEnd: TATLineEnds;
  bWithBom, bEncoded: boolean;
begin
  Clear;

  Len:= 0;
  if FEncodingDetect then
  begin
    DoDetectStreamEncoding(Stream, FEncoding, Len, bWithBom,
      FEncodingDetectBufSizeKb,
      FEncodingDetectBufSizeWords,
      FEncodingDetectDefaultUtf8);
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
    NCounted:= 0;
    repeat
      _FindNextEol(NStart, NEnd, LineEnd);
      Len:= NEnd-NStart;

      //call OnProgress each 1M chars
      //it's smooth enough progress, on 280Mb log, 10 seconds
      if NStart-NCounted>cStringsProgressLoadChars then
      begin
        NCounted:= NStart;
        FProgressKind:= cStringsProgressLoading;
        FProgressValue:= Int64(NStart)*100 div Stream.Size;
        if Assigned(FOnProgress) then
          FOnProgress(Self);
      end;

      if LineEnd=cEndWin then
        Inc(NEnd, CharSize*2)
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
                SUtf8:= SConvertEnc_AnsiToUTF8(SA)
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
    FProgressValue:= 0;
    FProgressKind:= cStringsProgressNone;
  end;
end;

procedure TATStrings.LoadFromStream(Stream: TStream);
begin
  DoClearUndo(true);
  DoLoadFromStream(Stream);
  DoFinalizeLoading;
end;


procedure TATStrings.LoadFromFile(const AFilename: string);
var
  fs: TFileStreamUtf8;
  //NSize: Int64;
begin
  //NSize:= FileUtil.FileSize(AFilename);

  fs:= TFileStreamUtf8.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    //if fs.Size<NSize then
    //  raise Exception.Create('Cannot open file. Opened content size reported less than disk file size. File is write-only?');
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
  FProgressValue:= 0;
end;


