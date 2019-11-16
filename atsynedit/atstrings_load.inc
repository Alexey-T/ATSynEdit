{$ifdef nn}begin end;{$endif}

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
  Buf: array[0..80] of word;
  BufItem: word;
  NSize, i: integer;
  CountLE, CountBE: integer;
  DetectLE, DetectBE: boolean;
begin
  Result:= false;
  if Stream.Size<6 then exit;

  if BufSizeWords<3 then BufSizeWords:= 3;
  if BufSizeWords>High(Buf) then BufSizeWords:= High(Buf);
  i:= Stream.Size div 2;
  if BufSizeWords>i then BufSizeWords:= i;

  NSize:= BufSizeWords*2;

  FillChar(Buf, SizeOf(Buf), 0);
  Stream.Position:= 0;
  Stream.Read(Buf, NSize);
  Stream.Position:= 0;

  CountLE:= 0;
  CountBE:= 0;

  for i:= 0 to BufSizeWords-1 do
  begin
    BufItem:= Buf[i];
    if (Hi(BufItem)=0) and IsByteAscii(Lo(BufItem)) then
      Inc(CountLE)
    else
    if (Lo(BufItem)=0) and IsByteAscii(Hi(BufItem)) then
      Inc(CountBE);
  end;

  DetectLE:= (CountBE=0) and (CountLE>=BufSizeWords div 3 * 2);
  DetectBE:= (CountLE=0) and (CountBE>=BufSizeWords div 3 * 2);

  Result:= DetectLE or DetectBE;
  IsLE:= DetectLE;
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


procedure TATStrings.LoadFromStrings(AList: TStrings; AEnds: TATLineEnds);
var
  i: integer;
begin
  if ReadOnly then exit;
  Clear;
  if AList.Count=0 then
  begin
    ActionAddFakeLineIfNeeded;
    Exit;
  end;

  for i:= 0 to AList.Count-1 do
    LineAddRaw_UTF8_NoUndo(AList[i], AEnds);

  ActionAddFakeLineIfNeeded;
  Modified:= true;
  for i:= 0 to Count-1 do
    SetLineState(i, cLineStateAdded);
end;

procedure TATStrings.LoadFromString(const AText: atString);
var
  MS: TMemoryStream;
  Str: atString;
  EncOld: TATFileEncoding;
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
    EncOld:= Encoding;
    try
      MS.Write(AText[1], Length(AText)*SizeOf(atChar));
      MS.Position:= 0;
      try
        Encoding:= cEncWideLE;
        EncodingDetect:= false;
        LoadFromStream(MS);
      finally
        Encoding:= EncOld;
        ActionAddFakeLineIfNeeded;
      end;
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
  NStart, NEnd, NCounted, Len, NSignatureLen: integer;
  SA, SUtf8: string;
  SWide: UnicodeString;
  LineEnd: TATLineEnds;
  bWithBom: boolean;
begin
  Clear;

  NSignatureLen:= 0;
  if FEncodingDetect then
  begin
    DoDetectStreamEncoding(Stream, FEncoding, NSignatureLen, bWithBom,
      GlobalDetectUtf8BufferKb,
      GlobalDetectUf16BufferWords,
      FEncodingDetectDefaultUtf8);
    case FEncoding of
      cEncUTF8: SaveSignUtf8:= bWithBom;
      cEncWideBE,
      cEncWideLE: SaveSignWide:= bWithBom;
    end;
  end;
  CharSize:= cEncodingSize[FEncoding];

  BufSize:= Stream.Size-NSignatureLen;
  if BufSize<=0 then Exit;

  GetMem(Buf, BufSize);
  try
    Stream.Position:= NSignatureLen;
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

              SUtf8:= EncConvertToUTF8(SA, FEncodingCodepage);

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
  ms: TMemoryStream;
begin
  fs:= TFileStreamUtf8.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    if fs.Size>=0 then
      LoadFromStream(fs)
    else
    begin
      ms:= TMemoryStream.Create;
      try
        _ReadFileToStream(ms, AFilename);
        LoadFromStream(ms);
      finally
        FreeAndNil(ms);
      end;
    end;
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


