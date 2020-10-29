{$ifdef nn}begin end;{$endif}

function IsStreamWithSignature(Stream: TStream; const Sign: string): boolean;
const
  cMinimalCountOfChars = 1; //even empty text with only BOM must detect this BOM
var
  Buf: string;
begin
  Result:= false;
  if Stream.Size<Length(Sign)*cMinimalCountOfChars then Exit;
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


function DetectStreamUtf16NoBom(Stream: TStream; BufSizeWords: integer; out IsLE: boolean): boolean;
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


function DetectStreamUtf8NoBom(Stream: TStream; BufSizeKb: word): TBufferUTF8State;
const
  cMinLen = 10;
var
  Buf: PChar;
  Size: integer;
begin
  Result:= u8sUnknown;
  if Stream.Size<cMinLen then Exit;

  if BufSizeKb=0 then BufSizeKb:= 1;
  Size:= BufSizeKb*1024;
  GetMem(Buf, Size);
  try
    FillChar(Buf^, Size, 0);
    Stream.Position:= 0;
    Stream.Read(Buf^, Size-1{trail zero});
    Stream.Position:= 0;
    Result:= IsBufferUtf8(Buf, Size-1, true);
  finally
    FreeMem(Buf);
  end;
end;

function DetectStreamEncodingByPythonId(Stream: TStream; var AEnc: TATFileEncoding; var AEncCodepage: TEncConvId): boolean;
// https://docs.python.org/3/reference/lexical_analysis.html#encoding-declarations
const
  BufSize = 100;
  cRegex = '^.+coding[=:]\s*([-\w.]+)';
  cRegexGroup = 1;
var
  Buf, SLine, SEncId: string;
  Sep: TATStringSeparator;
  i: integer;
begin
  Result:= false;
  Buf:= StringOfChar(' ', BufSize);
  Stream.Position:= 0;
  Stream.Read(Buf[1], Length(Buf));
  Sep.Init(Buf, #10); //don't support CR separator yet

  //detect by first 2 lines, 2nd used if 1st is comment
  for i:= 1 to 2 do
    if Sep.GetItemStr(SLine) then
    begin
      if SLine='' then exit;

      //support several line comments, not all
      if
        not SBeginsWith(SLine, '#') and
        not SBeginsWith(SLine, ';') and
        not SBeginsWith(SLine, '//') and
        not SBeginsWith(SLine, '--') then exit;

      SEncId:= SFindRegexMatch(SLine, cRegex, cRegexGroup, false, false, false);
      if SEncId<>'' then
      begin
        AEncCodepage:= EncConvFindEncoding(SEncId);
        case AEncCodepage of
          eidUTF8,
          eidUTF8BOM,
          eidUCS2LE,
          eidUCS2BE:
            begin
              AEnc:= cEncUTF8;
              AEncCodepage:= eidUTF8;
            end
          else
            AEnc:= cEncAnsi;
        end;
        Result:= true;
        exit;
      end;
    end;
end;

procedure DetectStreamEncoding(Stream: TStream;
  var Enc: TATFileEncoding;
  var EncCodepage: TEncConvId;
  out SignLen: integer;
  out EncWithBom: boolean;
  BufSizeUTF8: integer;
  BufSizeUTF16: integer);
var
  IsLE: boolean;
begin
  SignLen:= 0;
  EncWithBom:= false;

  if IsStreamWithSignature(Stream, cSignUTF8) then
  begin
    Enc:= cEncUTF8;
    EncWithBom:= true;
    SignLen:= Length(cSignUTF8);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSign32LE) then
  begin
    Enc:= cEnc32LE;
    EncWithBom:= true;
    SignLen:= Length(cSign32LE);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSign32BE) then
  begin
    Enc:= cEnc32BE;
    EncWithBom:= true;
    SignLen:= Length(cSign32BE);
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

  if BufSizeUTF8>0 then
    case DetectStreamUtf8NoBom(Stream, BufSizeUTF8) of
      u8sYes:
        begin
          Enc:= cEncUTF8;
          EncWithBom:= false;
          Exit
        end;
      u8sNo:
        begin
          // https://github.com/Alexey-T/CudaText/issues/2928
          Enc:= cEncAnsi;
        end;
    end;

  if DetectStreamEncodingByPythonId(Stream, Enc, EncCodepage) then
    Exit;

  if BufSizeUTF16>0 then
    if DetectStreamUtf16NoBom(Stream, BufSizeUTF16, IsLE) then
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
    LineAddRaw_NoUndo(AList[i], AEnds);

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

function _GetBufferEnding(ABuffer: pointer; APos: integer; AEnc: TATFileEncoding): TATCharEnding; inline;
var
  p: PByte absolute ABuffer;
  Check: byte;
begin
  Result:= cCharNotEnding;
  case AEnc of
    cEncAnsi,
    cEncUTF8:
      begin
        Check:= p[APos];
      end;
    cEncWideLE:
      begin
        if p[APos+1]<>0 then exit;
        Check:= p[APos];
      end;
    cEncWideBE:
      begin
        if p[APos]<>0 then exit;
        Check:= p[APos+1];
      end;
    cEnc32LE:
      begin
        if p[APos+3]<>0 then exit;
        if p[APos+2]<>0 then exit;
        if p[APos+1]<>0 then exit;
        Check:= p[APos];
      end;
    cEnc32BE:
      begin
        if p[APos]<>0 then exit;
        if p[APos+1]<>0 then exit;
        if p[APos+2]<>0 then exit;
        Check:= p[APos+3];
      end;
  end;

  case Check of
    10: Result:= cCharEnding10;
    13: Result:= cCharEnding13;
  end;
end;

procedure TATStrings.DoLoadFromStream(Stream: TStream; out AForcedToANSI: boolean);
var
  Buf: PAnsiChar;
  BufSize: int64;
  CharSize: integer;
  NSignatureLen: integer;
  NStreamSize: Int64;
  //------
  procedure _FindNextEol(AFromPos: integer;
    out AFoundPos: integer;
    out AEnding: TATLineEnds);
  begin
    AFoundPos:= AFromPos;
    AEnding:= cEndNone;
    repeat
      if AFoundPos>=BufSize then exit;

      case _GetBufferEnding(Buf, AFoundPos, FEncoding) of
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
              if _GetBufferEnding(Buf, AFoundPos+CharSize, FEncoding)=cCharEnding10 then
                AEnding:= cEndWin
              else
                AEnding:= cEndMac;
            exit
          end;
      end;
    until false;
  end;
  //------
  procedure _LoadItems;
  var
    Len, NStart, NEnd, NCounted: integer;
    LineEnd: TATLineEnds;
    SA, SUtf8: string;
    SWide: UnicodeString;
    SQuad: UCS4String;
  begin
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
        FProgressValue:= Int64(NStart)*100 div NStreamSize;
        if Assigned(FOnProgress) then
          FOnProgress(Self);
      end;

      if LineEnd=cEndWin then
        Inc(NEnd, CharSize*2)
      else
        Inc(NEnd, CharSize);

      if Len=0 then
        LineAddRaw_NoUndo('', LineEnd)
      else
      begin
        case FEncoding of
          cEncAnsi:
            begin
              SetString(SA, @Buf[NStart], Len);
              SUtf8:= EncConvertToUTF8(SA, FEncodingCodepage);
              LineAddRaw_NoUndo(SUtf8, LineEnd);
            end;

          cEncUTF8:
            begin
              //this now raises exception if UTF8 content is bad, it's needed
              SetString(SA, @Buf[NStart], Len);
              LineAddRaw_NoUndo(SA, LineEnd);
            end;

          cEncWideLE,
          cEncWideBE:
            begin
              SWide:= '';
              SetLength(SWide, Len div 2);
              Move(Buf[NStart], SWide[1], Len);
              if FEncoding=cEncWideBE then
                SSwapEndianWide(SWide);
              LineAddRaw_NoUndo(SWide, LineEnd);
            end;

          cEnc32LE,
          cEnc32BE:
            begin
              SetLength(SQuad, Len div 4+1);
              Move(Buf[NStart], SQuad[0], Len);
              if FEncoding=cEnc32BE then
                SSwapEndianUCS4(SQuad);
              SWide:= UCS4StringToUnicodeString(SQuad);
              LineAddRaw_NoUndo(SWide, LineEnd);
            end;

          else
            DoEncError;
        end;
      end;

      NStart:= NEnd;
      if (NStart>=BufSize) then Break;
    until false;

  end;
  //------
var
  bWithBom: boolean;
begin
  AForcedToANSI:= false;
  Clear;

  bWithBom:= false;
  NStreamSize:= Stream.Size;
  NSignatureLen:= 0;

  if NStreamSize>(cMaxFileSizeMbToDetectEncoding*1024*1024) then
  begin
    //it's safe to set here UTF8 even for binary files, later contents will be checked
    FEncoding:= cEncUTF8;
  end
  else
  if FEncodingDetect then
  begin
    //detect encoding by signature and by buffer content
    if FEncodingDetectDefaultUtf8 then
      FEncoding:= cEncUTF8
    else
      FEncoding:= cEncAnsi;
    DetectStreamEncoding(Stream, FEncoding, FEncodingCodepage, NSignatureLen, bWithBom,
      GlobalDetectUtf8BufferKb,
      GlobalDetectUf16BufferWords);
  end
  else
  begin
    //detect encoding only by signature
    DetectStreamEncoding(Stream, FEncoding, FEncodingCodepage, NSignatureLen, bWithBom,
      0, //buffer sizes=0
      0);
  end;

  case FEncoding of
    cEncUTF8: SaveSignUtf8:= bWithBom;
    cEncWideBE,
    cEncWideLE: SaveSignWide:= bWithBom;
  end;

  CharSize:= cEncodingSize[FEncoding];

  BufSize:= NStreamSize-NSignatureLen;
  if BufSize<=0 then Exit;

  GetMem(Buf, BufSize);
  try
    Stream.Position:= NSignatureLen;
    Stream.ReadBuffer(Buf^, BufSize);

    (*
    //check that we won't break file contents, it may be cp1252 file, so UTF8 loading will break chars to '?'
    //this is now not needed, utf8 load error raises exception in ATStringItem.SetLine, ie in _LoadItems
    if FEncoding=cEncUTF8 then
      if IsBufferUtf8(Buf, BufSize, false)=u8sNo then
      begin
        AForcedToANSI:= true;
        FEncoding:= cEncAnsi;
        CharSize:= 1;
      end;
      *)

    try
      _LoadItems;
    except
      //failed to load text as UTF8, so load again as ANSI
      Clear; //clear loaded items
      AForcedToANSI:= true;
      FEncoding:= cEncAnsi;
      CharSize:= 1;
      _LoadItems;
    end;
  finally
    FreeMem(Buf);
    FProgressValue:= 0;
    FProgressKind:= cStringsProgressNone;
  end;
end;

procedure TATStrings.LoadFromStream(Stream: TStream);
begin
  DoClearUndo(true);
  DoLoadFromStream(Stream, FLoadingForcedANSI);
  DoFinalizeLoading;
end;


procedure TATStrings.LoadFromFile(const AFilename: string);
var
  fs: TFileStream;
  ms: TMemoryStream;
begin
  fs:= TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
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


