{$GOTO ON} //2026.09: enable goto/label for the tight scan loops below (FPC needs -Sg or this directive; Lazarus lpk passes no -Sg)
{$ifdef nn}begin end;{$endif}

function IsStreamWithSignature(Stream: TStream; const Sign: string): boolean;
const
  cMinimalCountOfChars = 1; //even empty text with only BOM must detect this BOM
var
  Buf: string;
begin
  Result:= false;
  if Stream.Size<Length(Sign)*cMinimalCountOfChars then Exit;
  Buf:= '';
  SetLength(Buf, Length(Sign));
  Stream.Position:= 0;
  Stream.ReadBuffer(Buf[1], Length(Sign));
  Stream.Position:= 0;
  Result:= Buf=Sign;
end;


function IsByteAscii(value: byte): boolean; inline;
begin
  Result:= value in [9, 10, 13, 32..126];
end;


function IsWordWithEolChar(const Q: QWord): boolean; inline;
{
2026.09.11 (CudaText perf): SWAR test - does the 8-byte word contain char
#10 or #13? Classic "zero byte" bit-trick (see haszero in Bit Twiddling
Hacks; borrow-safe for all inputs) applied to the word XOR-ed with a
broadcasted char. Used by the tight EOL scan in ParseBuffer/_FindNextEol
for ANSI/UTF8 buffers: the previous per-byte 'if (p^=#10) or (p^=#13)'
loop was the dominating cost of parsing big files (500MB file: ~1.4s of
~3.9s total, Very Sleepy callgrind).
}
const
  cOnes: QWord = $0101010101010101;
  cHigh: QWord = $8080808080808080;
  cCharLF: QWord = $0A0A0A0A0A0A0A0A;
  cCharCR: QWord = $0D0D0D0D0D0D0D0D;
var
  T1, T2: QWord;
begin
  T1:= Q xor cCharLF;
  T2:= Q xor cCharCR;
  Result:= (((T1-cOnes) and (not T1) and cHigh) or
            ((T2-cOnes) and (not T2) and cHigh))<>0;
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

  FillChar(Buf{%H-}, SizeOf(Buf), 0);
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


function DetectStreamUtf8NoBom(Stream: TStream; BufSizeKb: word): TATBufferUTF8State;
const
  cMinLen = 10;
var
  Buf: PChar;
  Size: integer;
begin
  Result:= TATBufferUTF8State.Unknown;
  if Stream.Size<cMinLen then Exit;

  if BufSizeKb=0 then
    BufSizeKb:= 1;
  Size:= Min(BufSizeKb*1024, Stream.Size);

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

function FixEncodingNameByIANAStandart(const S: string): string;
//correct some names according to this standard:
// IANA Registered Character Sets
// https://www.iana.org/assignments/character-sets/character-sets.xhtml
begin
  Result:= LowerCase(S);
  Result:= StringReplace(Result, 'windows-', 'cp', []);
  Result:= StringReplace(Result, 'ibm', 'cp', []);
  Result:= StringReplace(Result, 'iso_', 'iso-', []);
  Result:= StringReplace(Result, 'utf-8', 'utf8', []);
  if (Result='us') or (Result='us-ascii') then //this maps to cp367 but Lazarus don't have it
    Result:= 'cp437';
end;

function DetectStreamEncodingByXmlId(Stream: TStream; var AEnc: TATFileEncoding; var AEncCodepage: TEncConvId): boolean;
//detect in XML files by signature <?xml version="1.0" encoding="ISO-8859-9"?>
const
  cTagXML = '<?xml ';
  cTagEnc = ' encoding="';
var
  Buf: array[0..70] of char;
  TempEnc: TEncConvId;
  S: string;
  N: integer;
begin
  Result:= false;
  FillChar(Buf{%H-}, SizeOf(Buf), 0);
  Stream.Position:= 0;
  N:= Stream.Read(Buf, SizeOf(Buf)-1);
  Stream.Position:= 0;
  if N<=20 then exit;

  SetString(S, Buf, N);
  SDeleteFrom(S, #10);
  SDeleteFrom(S, #13);

  N:= Pos(cTagXML, S);
  if N=0 then exit;
  N:= Pos(cTagEnc, S);
  if N=0 then exit;
  Delete(S, 1, N-1+Length(cTagEnc));
  SDeleteFrom(S, '"');

  S:= FixEncodingNameByIANAStandart(S);

  TempEnc:= EncConvFindEncoding(S);
  case TempEnc of
    eidUTF8,
    eidUTF8BOM:
      begin
        AEnc:= TATFileEncoding.UTF8;
        Result:= true;
      end;
    eidUCS2LE,
    eidUCS2BE:
      exit;
    else
      begin
        AEnc:= TATFileEncoding.ANSI;
        AEncCodepage:= TempEnc;
        Result:= true;
      end;
  end;
end;

function DetectStreamEncodingByPythonId(Stream: TStream; var AEnc: TATFileEncoding; var AEncCodepage: TEncConvId): boolean;
{
https://docs.python.org/3/reference/lexical_analysis.html#encoding-declarations
The recommended forms of an encoding expression are

# -*- coding: <encoding-name> -*-

which is recognized also by GNU Emacs, and

# vim:fileencoding=<encoding-name>

which is recognized by VIM.
}
  //
  function IsLineComment(const S: string): boolean;
  begin
    Result:=
      SBeginsWith(S, '#') or
      SBeginsWith(S, ';') or
      SBeginsWith(S, '//') or
      SBeginsWith(S, '--');
  end;
  //
  function GetEnc(const S: string): string;
  var
    N, N0, Len: integer;
  begin
    Result:= '';
    N:= Pos('coding', S);
    if N=0 then exit;
    Len:= Length(S);
    if N+8>Len then exit;
    Inc(N, 6);
    if not (S[N] in ['=', ':']) then exit;
    Inc(N);
    while (N<=Len) and (S[N]=' ') do Inc(N);
    N0:= N;
    while (N<=Len) and (S[N] in ['a'..'z', 'A'..'Z', '0'..'9', '-']) do Inc(N);
    Result:= Copy(S, N0, N-N0);

    Result:= FixEncodingNameByIANAStandart(Result);
  end;
  //
const
  cBufSize = 100;
  cLineCount = 2; //detect by first 2 lines, 2nd used if 1st is comment
var
  Buf, SLine, SEncId: string;
  Sep: TATStringSeparator;
  i: integer;
begin
  Result:= false;
  Buf:= StringOfChar(' ', cBufSize);
  Stream.Position:= 0;
  i:= Stream.Read(Buf[1], Length(Buf));
  SetLength(Buf, i);
  Stream.Position:= 0;

  Buf:= StringReplace(Buf, #13#10, #10, [rfReplaceAll]);
  Buf:= StringReplace(Buf, #13, #10, [rfReplaceAll]);
  Sep.Init(Buf, #10);

  for i:= 0 to cLineCount-1 do
  begin
    Sep.GetItemStr(SLine);
    if SLine='' then exit;
    if not IsLineComment(SLine) then exit;
    SEncId:= LowerCase(GetEnc(SLine));
    if SEncId<>'' then
    begin
      AEncCodepage:= EncConvFindEncoding(SEncId);
      case AEncCodepage of
        eidUTF8,
        eidUTF8BOM,
        eidUCS2LE,
        eidUCS2BE:
          begin
            AEnc:= TATFileEncoding.UTF8;
            AEncCodepage:= eidUTF8;
          end
        else
          AEnc:= TATFileEncoding.ANSI;
      end;
      Result:= true;
      exit;
    end;
  end;
end;

procedure DetectStreamEncoding(Stream: TStream;
  var Enc: TATFileEncoding;
  var EncCodepage: TEncConvId;
  out SignLen: SizeInt;
  out EncWithBom: boolean;
  BufSizeUTF8: integer;
  BufSizeUTF16: integer;
  DetectByPythonSignature: boolean;
  DetectByXmlSignature: boolean);
var
  IsLE: boolean;
begin
  SignLen:= 0;
  EncWithBom:= false;

  if IsStreamWithSignature(Stream, cSignUTF8) then
  begin
    Enc:= TATFileEncoding.UTF8;
    EncWithBom:= true;
    SignLen:= Length(cSignUTF8);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSign32LE) then
  begin
    Enc:= TATFileEncoding.UTF32LE;
    EncWithBom:= true;
    SignLen:= Length(cSign32LE);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSign32BE) then
  begin
    Enc:= TATFileEncoding.UTF32BE;
    EncWithBom:= true;
    SignLen:= Length(cSign32BE);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSignWideLE) then
  begin
    Enc:= TATFileEncoding.UTF16LE;
    EncWithBom:= true;
    SignLen:= Length(cSignWideLE);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSignWideBE) then
  begin
    Enc:= TATFileEncoding.UTF16BE;
    EncWithBom:= true;
    SignLen:= Length(cSignWideBE);
    Exit
  end;

  if BufSizeUTF8>0 then
    case DetectStreamUtf8NoBom(Stream, BufSizeUTF8) of
      TATBufferUTF8State.Yes:
        begin
          Enc:= TATFileEncoding.UTF8;
          EncWithBom:= false;
          Exit
        end;
      TATBufferUTF8State.No:
        begin
          // https://github.com/Alexey-T/CudaText/issues/2928
          Enc:= TATFileEncoding.ANSI;
        end;
    end;

  if DetectByPythonSignature then
    if DetectStreamEncodingByPythonId(Stream, Enc, EncCodepage) then
      Exit;

  if DetectByXmlSignature then
    if DetectStreamEncodingByXmlId(Stream, Enc, EncCodepage) then
      Exit;

  if BufSizeUTF16>0 then
    if DetectStreamUtf16NoBom(Stream, BufSizeUTF16, IsLE) then
    begin
      if IsLE then
        Enc:= TATFileEncoding.UTF16LE
      else
        Enc:= TATFileEncoding.UTF16BE;
      Exit
    end;
end;


procedure TATStrings.LoadFromStrings(AList: TStrings; AEnds: TATLineEnds; AllowBadCharsOfLen1: boolean);
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
    LineAddRaw_NoUndo(AList[i], AEnds, AllowBadCharsOfLen1);

  ActionAddFakeLineIfNeeded;
  Modified:= true;
  for i:= 0 to Count-1 do
    SetLineState(i, TATLineState.Added);
end;

procedure TATStrings.LoadFromString(const AText: string);
var
  EncOld: TATFileEncoding;
  Str: UnicodeString;
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
    Str:= UTF8Decode(AText);
    SDeleteFromEol(Str);
    LineAddEx(Str, TATLineEnds.None);
  end
  else
  begin
    {
    2026.09.11 (CudaText perf): parse the string data in-place, without
copying the whole text to TMemoryStream + GetMem buffer (that was 2x memcpy
of the full text, ~1.2s for the 500MB CudaText test, and 1GB of extra peak
memory). Events/progress/fallbacks run like in LoadFromStream(): the same
ParseBuffer() core is called, wrapped in ClearUndo/FLoadingFromStream/
DoFinalizeLoading exactly like LoadFromStream() does.
    }
    EncOld:= Encoding;
    ClearUndo(true);
    FLoadingFromStream:= true;
    try
      try
        Encoding:= TATFileEncoding.UTF8;
        EncodingDetect:= false;
        ParseBuffer(@AText[1], Length(AText), Length(AText), 1,
          [TATLoadStreamOption.FromUTF8],
          true,
          FLoadingForcedANSI);
      finally
        //same position as LoadFromStream()'s finally: before the
        //CudaText #4172 fix below, so its undo items (if UndoLimit>0)
        //survive, like they did before
        DoFinalizeLoading;
        FLoadingFromStream:= false;
      end;

      //fixing CudaText issue #4172
      if not SEndsWithEol(AText) then
        ActionDeleteFakeLineAndFinalEol;
    finally
      Encoding:= EncOld;
      ActionAddFakeLineIfNeeded;
    end;
  end;

  Modified:= true;
  for i:= 0 to Count-1 do
    SetLineState(i, TATLineState.Added);
end;

{
type
  TATCharEnding = (
    NotEnding,
    Ending10,
    Ending13
    );

2026.09: _GetBufferEnding() and TATCharEnding are removed: _FindNextEol()
now uses per-encoding tight scanning loops, which don't need per-char
EOL classification (see the comment in _FindNextEol).
}

procedure TATStrings.ParseBuffer(ABuf: PAnsiChar; ABufSize, AStreamSize: Int64;
  ACharSize: SizeInt; AOptions: TATLoadStreamOptions; AFirePreProgress: boolean;
  out AForcedToANSI: boolean);
{
2026.09.11 (CudaText perf): the parsing core, split from DoLoadFromStream()
so that LoadFromString() can parse the string data directly (no 2x memcpy
of the whole text via TMemoryStream + GetMem buffer, ~1.2s for the 500MB
CudaText test + 1GB of extra peak memory). Behavior is identical to the
previous inline code: same scans, same events/progress, same UTF8-to-ANSI
fallback.
}
var
  Buf: PAnsiChar;
  BufSize: Int64;
  NStreamSize: Int64;
  CharSize: SizeInt;
  //------
  procedure _FindNextEol(AFromPos: SizeInt;
    out AFoundPos: SizeInt;
    out AEnding: TATLineEnds;
    out AWithHighBit: boolean);
  {
  2026.09: optimized scanning (CudaText perf: parsing of big files was
  dominated by per-char _GetBufferEnding() case-dispatch + 'mod' check,
  e.g. 300MB text ~11s). Now per-encoding tight pointer loops, which give
  identical EOL detection. OnProgress event now fires when the scan crosses
  a multiple of ProgressLoadChars (detected per cScanChunk block, so loading
  of files with huge single lines still shows progress and allows cancel).
  }
  const
    cScanChunk = 4096;
    cHighBits: QWord = $8080808080808080;
  label
    FoundEol;
  var
    bCancel: boolean;
    p, pLimit, pEnd, pWordEnd: PAnsiChar;
    NNextProgress: Int64;
    Q: QWord;
  begin
    AFoundPos:= AFromPos;
    AEnding:= TATLineEnds.None;
    AWithHighBit:= false;
    if AFromPos>=BufSize then exit;

    NNextProgress:=
      ((Int64(AFromPos) div TATEditorOptions.ProgressLoadChars)+1)*TATEditorOptions.ProgressLoadChars;

    p:= Buf+AFromPos;
    pEnd:= Buf+BufSize;

    case FEncoding of
      TATFileEncoding.ANSI,
      TATFileEncoding.UTF8:
        {
        2026.09.11: 8 bytes per iteration (SWAR, see IsWordWithEolChar):
        ~4x faster than the per-byte loop, same detected position. The scan
        also detects bytes >=128 (AWithHighBit) for the caller: replaces the
        IsStringWithUnicode() re-scan of each added line. PQWord reads are
        done only for 8-aligned pointers (leading bytes per-byte), like in
        IsStringWithUnicode().
        }
        repeat
          pLimit:= p+cScanChunk;
          if pLimit>pEnd then pLimit:= pEnd;

          //leading bytes until 8-aligned: per-byte
          while (p<pLimit) and ((PtrUInt(p) and 7)<>0) do
          begin
            if (p^=#10) or (p^=#13) then goto FoundEol;
            if Byte(p^)>=128 then AWithHighBit:= true;
            Inc(p);
          end;

          //main part: 8 bytes per iteration
          while p+8<=pLimit do
          begin
            Q:= PQWord(p)^;
            if IsWordWithEolChar(Q) then
            begin
              //locate the exact EOL byte (word is fully inside the buffer)
              pWordEnd:= p+8;
              while p<pWordEnd do
              begin
                if (p^=#10) or (p^=#13) then goto FoundEol;
                if Byte(p^)>=128 then AWithHighBit:= true;
                Inc(p);
              end;
              //EOL is guaranteed to be found above; if not, continue safely
              Continue;
            end;
            if (Q and cHighBits)<>0 then AWithHighBit:= true;
            Inc(p, 8);
          end;

          //tail: per-byte
          while p<pLimit do
          begin
            if (p^=#10) or (p^=#13) then goto FoundEol;
            if Byte(p^)>=128 then AWithHighBit:= true;
            Inc(p);
          end;

          if p>=pEnd then Break;
          if (p^=#10) or (p^=#13) then goto FoundEol;

          AFoundPos:= p-Buf;
          if AFoundPos>=NNextProgress then
          begin
            FProgressValue:= Int64(AFoundPos)*100 div NStreamSize;
            if Assigned(FOnProgress) then
            begin
              bCancel:= false;
              FOnProgress(Self, bCancel);
              if bCancel then
                raise EEditorUserStopped.Create('User stopped');
            end;
            NNextProgress:=
              ((Int64(AFoundPos) div TATEditorOptions.ProgressLoadChars)+1)*TATEditorOptions.ProgressLoadChars;
          end;
        until false;

      TATFileEncoding.UTF16LE:
        //EOL is encoded word $000A or $000D (bytes: 0A 00 / 0D 00)
        repeat
          pLimit:= p+cScanChunk;
          if pLimit>pEnd then pLimit:= pEnd;
          while p+2<=pLimit do
          begin
            if ((p^=#10) or (p^=#13)) and ((p+1)^=#0) then goto FoundEol;
            Inc(p, 2);
          end;
          if p+2>pEnd then Break;

          AFoundPos:= p-Buf;
          if AFoundPos>=NNextProgress then
          begin
            FProgressValue:= Int64(AFoundPos)*100 div NStreamSize;
            if Assigned(FOnProgress) then
            begin
              bCancel:= false;
              FOnProgress(Self, bCancel);
              if bCancel then
                raise EEditorUserStopped.Create('User stopped');
            end;
            NNextProgress:=
              ((Int64(AFoundPos) div TATEditorOptions.ProgressLoadChars)+1)*TATEditorOptions.ProgressLoadChars;
          end;
        until false;

      TATFileEncoding.UTF16BE:
        //EOL is encoded word $0A00 or $0D00 (bytes: 00 0A / 00 0D)
        repeat
          pLimit:= p+cScanChunk;
          if pLimit>pEnd then pLimit:= pEnd;
          while p+2<=pLimit do
          begin
            if (p^=#0) and (((p+1)^=#10) or ((p+1)^=#13)) then goto FoundEol;
            Inc(p, 2);
          end;
          if p+2>pEnd then Break;

          AFoundPos:= p-Buf;
          if AFoundPos>=NNextProgress then
          begin
            FProgressValue:= Int64(AFoundPos)*100 div NStreamSize;
            if Assigned(FOnProgress) then
            begin
              bCancel:= false;
              FOnProgress(Self, bCancel);
              if bCancel then
                raise EEditorUserStopped.Create('User stopped');
            end;
            NNextProgress:=
              ((Int64(AFoundPos) div TATEditorOptions.ProgressLoadChars)+1)*TATEditorOptions.ProgressLoadChars;
          end;
        until false;

      TATFileEncoding.UTF32LE:
        //EOL is encoded dword $0000000A/$0000000D (bytes: 0A 00 00 00)
        repeat
          pLimit:= p+cScanChunk;
          if pLimit>pEnd then pLimit:= pEnd;
          while p+4<=pLimit do
          begin
            if ((p^=#10) or (p^=#13)) and
              ((p+1)^=#0) and ((p+2)^=#0) and ((p+3)^=#0) then goto FoundEol;
            Inc(p, 4);
          end;
          if p+4>pEnd then Break;

          AFoundPos:= p-Buf;
          if AFoundPos>=NNextProgress then
          begin
            FProgressValue:= Int64(AFoundPos)*100 div NStreamSize;
            if Assigned(FOnProgress) then
            begin
              bCancel:= false;
              FOnProgress(Self, bCancel);
              if bCancel then
                raise EEditorUserStopped.Create('User stopped');
            end;
            NNextProgress:=
              ((Int64(AFoundPos) div TATEditorOptions.ProgressLoadChars)+1)*TATEditorOptions.ProgressLoadChars;
          end;
        until false;

      TATFileEncoding.UTF32BE:
        //EOL is encoded dword $0A000000/$0D000000 (bytes: 00 00 00 0A)
        repeat
          pLimit:= p+cScanChunk;
          if pLimit>pEnd then pLimit:= pEnd;
          while p+4<=pLimit do
          begin
            if (p^=#0) and ((p+1)^=#0) and ((p+2)^=#0) and
              (((p+3)^=#10) or ((p+3)^=#13)) then goto FoundEol;
            Inc(p, 4);
          end;
          if p+4>pEnd then Break;

          AFoundPos:= p-Buf;
          if AFoundPos>=NNextProgress then
          begin
            FProgressValue:= Int64(AFoundPos)*100 div NStreamSize;
            if Assigned(FOnProgress) then
            begin
              bCancel:= false;
              FOnProgress(Self, bCancel);
              if bCancel then
                raise EEditorUserStopped.Create('User stopped');
            end;
            NNextProgress:=
              ((Int64(AFoundPos) div TATEditorOptions.ProgressLoadChars)+1)*TATEditorOptions.ProgressLoadChars;
          end;
        until false;
    end;

    //no EOL till the buffer end
    AFoundPos:= p-Buf;
    exit;

  FoundEol:
    AFoundPos:= p-Buf;

    //detect EOL kind: LF or CR (byte position depends on encoding)
    case FEncoding of
      TATFileEncoding.UTF16BE:
        if (p+1)^=#10 then
        begin
          AEnding:= TATLineEnds.Unix;
          exit;
        end;
      TATFileEncoding.UTF32BE:
        if (p+3)^=#10 then
        begin
          AEnding:= TATLineEnds.Unix;
          exit;
        end;
      else
        if p^=#10 then
        begin
          AEnding:= TATLineEnds.Unix;
          exit;
        end;
    end;

    //CR: check if it's CRLF pair
    //if CR is the last char of buffer, it cannot be part of CRLF pair,
    //so it's a Mac-style line ending (fixes loss of trailing CR on load/save)
    if AFoundPos+CharSize>=BufSize then
    begin
      AEnding:= TATLineEnds.Mac;
      exit;
    end;
    if AFoundPos+Int64(CharSize)*2>BufSize then
    begin
      //next full char doesn't fit into buffer
      AEnding:= TATLineEnds.Mac;
      exit;
    end;

    case FEncoding of
      TATFileEncoding.ANSI,
      TATFileEncoding.UTF8:
        if Buf[AFoundPos+1]=#10 then
          AEnding:= TATLineEnds.Windows
        else
          AEnding:= TATLineEnds.Mac;
      TATFileEncoding.UTF16LE:
        if (Buf[AFoundPos+2]=#10) and (Buf[AFoundPos+3]=#0) then
          AEnding:= TATLineEnds.Windows
        else
          AEnding:= TATLineEnds.Mac;
      TATFileEncoding.UTF16BE:
        if (Buf[AFoundPos+2]=#0) and (Buf[AFoundPos+3]=#10) then
          AEnding:= TATLineEnds.Windows
        else
          AEnding:= TATLineEnds.Mac;
      TATFileEncoding.UTF32LE:
        if (Buf[AFoundPos+4]=#10) and (Buf[AFoundPos+5]=#0) and
          (Buf[AFoundPos+6]=#0) and (Buf[AFoundPos+7]=#0) then
          AEnding:= TATLineEnds.Windows
        else
          AEnding:= TATLineEnds.Mac;
      TATFileEncoding.UTF32BE:
        if (Buf[AFoundPos+4]=#0) and (Buf[AFoundPos+5]=#0) and
          (Buf[AFoundPos+6]=#0) and (Buf[AFoundPos+7]=#10) then
          AEnding:= TATLineEnds.Windows
        else
          AEnding:= TATLineEnds.Mac;
    end;
  end;
  //------
  procedure _LoadItems;
  var
    Len, NStart, NEnd: SizeInt;
    LineEndDeltas: array[boolean] of SizeInt;
    LineEnd: TATLineEnds;
    bWithHigh: boolean;
    SA, SUtf8: string;
    SWide: UnicodeString;
    SQuad: UCS4String;
  begin
    NStart:= 0;

    FProgressKind:= TATStringsProgressKind.Loading;
    FProgressValue:= 0;

    LineEndDeltas[false]:= CharSize;
    LineEndDeltas[true]:= CharSize*2;

    repeat
      _FindNextEol(NStart, NEnd, LineEnd, bWithHigh);
      Len:= NEnd-NStart;
      if Len>=MaxInt-12 then
        raise EEditorTooLongLine.Create('Too big line length on reading: 0x'+IntToHex(Len, 8));

      Inc(NEnd, LineEndDeltas[LineEnd=TATLineEnds.Windows]);

      if Application.Terminated then
      begin
        Clear(false);
        Break;
      end;

      if Len=0 then
        LineAddRaw_NoUndo('', LineEnd, false)
      else
      begin
        case FEncoding of
          TATFileEncoding.ANSI:
            begin
              SetString(SA, @Buf[NStart], Len);
              SUtf8:= EncConvertToUTF8(SA, FEncodingCodepage);
              LineAddRaw_NoUndo(SUtf8, LineEnd, TATLoadStreamOption.AllowBadCharsOfLen1 in AOptions);
            end;

          TATFileEncoding.UTF8:
            begin
              //this now raises exception if UTF8 content is bad, it's needed
              SetString(SA, @Buf[NStart], Len);
              //2026.09.11: the EOL scan already knows if the line has bytes
              //>=128; for pure-ASCII lines (majority of big files) skip the
              //IsStringWithUnicode() re-scan in the item Init
              if bWithHigh then
                LineAddRaw_NoUndo(SA, LineEnd, TATLoadStreamOption.AllowBadCharsOfLen1 in AOptions)
              else
                LineAddRaw_NoUndoAscii(SA, LineEnd);
            end;

          TATFileEncoding.UTF16LE,
          TATFileEncoding.UTF16BE:
            begin
              SWide:= '';
              SetLength(SWide, Len div 2);
              Move(Buf[NStart], SWide[1], Len);
              if FEncoding=TATFileEncoding.UTF16BE then
                SSwapEndianWide(SWide);
              LineAddRaw_NoUndo(SWide, LineEnd);
            end;

          TATFileEncoding.UTF32LE,
          TATFileEncoding.UTF32BE:
            begin
              SetLength(SQuad{%H-}, Len div 4+1);
              Move(Buf[NStart], SQuad[0], Len);
              if FEncoding=TATFileEncoding.UTF32BE then
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
  bCancel: boolean;
begin
  AForcedToANSI:= false;

  Buf:= ABuf;
  BufSize:= ABufSize;
  NStreamSize:= AStreamSize;
  CharSize:= ACharSize;

  //LoadFromFile/LoadFromString already call Clear
  if Count>0 then
    Clear(false{AWithEvent});

  //show progress before the parse of a huge buffer;
  //(for stream loading this event is fired by DoLoadFromStream, before the
  //slow GetMem call; for string loading there is no GetMem)
  if AFirePreProgress and (BufSize>50*1024*1024) then
  begin
    FProgressKind:= TATStringsProgressKind.None;
    FProgressValue:= 0;
    if Assigned(FOnProgress) then
    begin
      bCancel:= false;
      FOnProgress(Self, bCancel);
    end;
  end;

  try
    try
      _LoadItems;
    except
      on E: Exception do
      begin
        Clear;
        if E is EEditorBadUTF8 then
        begin
          //failed to load text as UTF8, so load again as ANSI
          AForcedToANSI:= true;
          FEncoding:= TATFileEncoding.ANSI;
          FEncodingCodepage:= ATEditorOptions.FallbackEncoding;
          CharSize:= 1;
          _LoadItems;
        end
        else
          raise;
      end;
    end;
  finally
    FProgressValue:= 0;
    FProgressKind:= TATStringsProgressKind.None;
  end;
end;

procedure TATStrings.DoLoadFromStream(Stream: TStream;
  AOptions: TATLoadStreamOptions; out AForcedToANSI: boolean);
var
  Buf: PAnsiChar;
  BufSize: Int64;
  NStreamSize: Int64;
  CharSize: SizeInt;
  NSignatureLen: SizeInt;
  bWithBom, bCancel: boolean;
begin
  AForcedToANSI:= false;

  //LoadFromFile already calls Clear
  if Count>0 then
    Clear(false{AWithEvent});

  bWithBom:= false;
  NStreamSize:= Stream.Size;
  NSignatureLen:= 0;

  if TATLoadStreamOption.FromUTF8 in AOptions then
  begin
    //func param forces UTF8
    FEncoding:= TATFileEncoding.UTF8;
  end
  else
  if NStreamSize>ATEditorOptions.MaxFileSizeMbToDetectEncoding*1024*1024 then
  begin
    //detect encoding by signature, even for huge files
    DetectStreamEncoding(Stream, FEncoding, FEncodingCodepage, NSignatureLen, bWithBom,
      0, //buffer size=0
      0, //buffer size=0
      ATEditorOptions.DetectEncodingByPythonSignature,
      ATEditorOptions.DetectEncodingByXmlSignature
      );
    //no signature?
    //it's safe to set here UTF8 even for binary files, later contents will be checked
    if not bWithBom then
      FEncoding:= TATFileEncoding.UTF8;
  end
  else
  if FEncodingDetect then
  begin
    //detect encoding by signature and by buffer content
    if FEncodingDetectDefaultUtf8 then
      FEncoding:= TATFileEncoding.UTF8
    else
      FEncoding:= TATFileEncoding.ANSI;
    DetectStreamEncoding(Stream, FEncoding, FEncodingCodepage, NSignatureLen, bWithBom,
      ATEditorOptions.DetectUTF8BufferKb,
      ATEditorOptions.DetectUTF16BufferWords,
      ATEditorOptions.DetectEncodingByPythonSignature,
      ATEditorOptions.DetectEncodingByXmlSignature
      );
  end
  else
  begin
    //detect encoding only by signature
    DetectStreamEncoding(Stream, FEncoding, FEncodingCodepage, NSignatureLen, bWithBom,
      0, //buffer sizes=0
      0,
      false,
      false
      );
  end;

  case FEncoding of
    TATFileEncoding.UTF8:
      SaveSignUtf8:= bWithBom;
    TATFileEncoding.UTF16BE,
    TATFileEncoding.UTF16LE:
      SaveSignWide:= bWithBom;
  end;

  CharSize:= cEncodingSize[FEncoding];

  BufSize:= NStreamSize-NSignatureLen;
  if BufSize<=0 then Exit;

  //show progress before calling GetMem which may be very slow for huge file
  if BufSize>50*1024*1024 then
  begin
    FProgressKind:= TATStringsProgressKind.None;
    FProgressValue:= 0;
    if Assigned(FOnProgress) then
    begin
      bCancel:= false;
      FOnProgress(Self, bCancel);
    end;
  end;

  GetMem(Buf, BufSize);

  try
    Stream.Position:= NSignatureLen;
    Stream.ReadBuffer(Buf^, BufSize);

    //2026.09.11: parsing core (was inline here) moved to ParseBuffer(),
    //shared with LoadFromString()
    ParseBuffer(Buf, BufSize, NStreamSize, CharSize, AOptions, false, AForcedToANSI);
  finally
    FreeMem(Buf);
  end;
end;

procedure TATStrings.LoadFromStream(AStream: TStream; AOptions: TATLoadStreamOptions);
begin
  ClearUndo(true);
  FLoadingFromStream:= true;
  try
    DoLoadFromStream(AStream, AOptions, FLoadingForcedANSI);
  finally
    DoFinalizeLoading;
    FLoadingFromStream:= false;
  end;
end;


procedure TATStrings.LoadFromFile(const AFilename: string; AOptions: TATLoadStreamOptions);
var
  fs: TFileStream;
  ms: TMemoryStream;
begin
  fs:= TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    if fs.Size>=0 then
      LoadFromStream(fs, AOptions)
    else
    begin
      //code for stream which Size cannot be detected, ie special Linux files in /proc
      ms:= TMemoryStream.Create;
      try
        _ReadFileToStream(ms, AFilename);
        LoadFromStream(ms, AOptions);
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
  ClearLineStates(false);
  ClearUndo;
  Modified:= false;
  FProgressValue:= 0;
end;


