unit ATStringProc;

{$mode objfpc}{$H+}
//{$define test_wide_char}

interface

uses
  Classes, SysUtils, StrUtils;

type
  atString = UnicodeString;
  atChar = WideChar;
  PatChar = PWideChar;

type
  TATIntArray = array of Longint;
  TATRealArray = array of real;
  TATPointArray = array of TPoint;

function SCharUpper(ch: atChar): atChar;
function SCharLower(ch: atChar): atChar;
function SCaseTitle(const S, SWordChars: atString): atString;
function SCaseInvert(const S: atString): atString;
function SCaseSentence(const S, SWordChars: atString): atString;

{$Z1}
type
  TATLineEnds = (cEndNone, cEndWin, cEndUnix, cEndMac);
const
  cLineEndStrings: array[TATLineEnds] of atString = ('', #13#10, #10, #13);
  cLineEndStrings_UTF8: array[TATLineEnds] of UTF8String = ('', #13#10, #10, #13);
  cLineEndNiceNames: array[TATLineEnds] of string = ('', 'win', 'un', 'mac');

const
  cMaxTabPositionToExpand = 500; //no sense to expand too far tabs
  cCharScaleFullwidth_Default = 1.7; //width of CJK chars
  cCharScaleHex = 6.0; //width of hex show: "<NNNN>"
  cMinWordWrapOffset = 3;

var
  OptHexCharsDefault: UnicodeString = ''; //show these chars as "<NNNN>"
  OptHexCharsUser: UnicodeString = ''; //these too
  OptCommaCharsWrapWithWords: UnicodeString = '.,;:''"`~?!&%$';
  cCharScaleFullwidth: Single = cCharScaleFullwidth_Default;


function IsCharEol(ch: atChar): boolean;
function IsCharWord(ch: atChar; const AWordChars: atString): boolean;
function IsCharSpace(ch: atChar): boolean;
function IsCharAsciiControl(ch: atChar): boolean;
function IsCharAccent(ch: atChar): boolean;
function IsCharHex(ch: atChar): boolean;
function IsStringWithUnicodeChars(const S: atString): boolean;

function SBeginsWith(const S, SubStr: atString): boolean;
function SBeginsWith(const S, SubStr: string): boolean;
function SEndsWith(const S, SubStr: atString): boolean;
function SEndsWith(const S, SubStr: string): boolean;

function STrimRight(const S: atString): atString;
function SGetIndentChars(const S: atString): integer;
function SGetIndentExpanded(const S: atString; ATabSize: integer): integer;
function SGetNonSpaceLength(const S: atString): integer;

function STabsToSpaces(const S: atString; ATabSize: integer): atString;
function SSpacesToTabs(const S: atString; ATabSize: integer): atString;
function SCharPosToColumnPos(const S: atString; APos, ATabSize: integer): integer;
function SColumnPosToCharPos(const S: atString; AColumn, ATabSize: integer): integer;

type
  TATCommentAction = (
    cCommentAdd_AtNonspace,
    cCommentAdd_AtNonespace_IfNone,
    cCommentAdd_AtStart,
    cCommentAdd_AtStart_IfNone,
    cCommentRemove,
    cCommentToggle_AtNonspace,
    cCommentToggle_AtStart
    );

function SCommentLineAction(L: TStringList; const AComment: atString; Act: TATCommentAction): boolean;

function SRemoveNewlineChars(const S: atString): atString;
function SRemoveHexChars(const S: atString): atString;
function SRemoveAsciiControlChars(const S: atString): atString;

procedure SCalcCharOffsets(const S: atString; var AList: TATRealArray;
  ATabSize: integer; ACharsSkipped: integer = 0);
function SFindWordWrapOffset(const S: atString; AColumns, ATabSize: integer;
  const AWordChars: atString; AWrapIndented: boolean): integer;
function SFindClickedPosition(const Str: atString;
  APixelsFromLeft, ACharSize, ATabSize: integer;
  AAllowVirtualPos: boolean;
  out AEndOfLinePos: boolean): integer;
procedure SFindOutputSkipOffset(const S: atString; ATabSize, AScrollPos: integer;
  out ACharsSkipped: integer; out ASpacesSkipped: real);

function SIndentUnindent(const Str: atString; ARight: boolean;
  AIndentSize, ATabSize: integer): atString;
function SGetItem(var S: string; const sep: Char = ','): string;
function SGetItemAtEnd(var S: string; const sep: Char = ','): string;
function SSwapEndian(const S: UnicodeString): UnicodeString;
function SWithBreaks(const S: atString): boolean;
procedure SAddStringToHistory(const S: string; List: TStrings; MaxItems: integer);

function BoolToPlusMinusOne(b: boolean): integer;
procedure TrimStringList(L: TStringList);

type
  TATDecodeRec = record SFrom, STo: UnicodeString; end;
function SDecodeRecords(const S: UnicodeString; const Decode: array of TATDecodeRec): UnicodeString;

procedure SReplaceAll(var s: string; const SFrom, STo: string);
procedure SReplaceAllPercentChars(var S: string);
procedure SDeleteFrom(var s: string; const SFrom: string);

var
  OptUnprintedReplaceSpec: boolean = false;


implementation

uses
  Dialogs, Math;

function IsCharEol(ch: atChar): boolean;
begin
  Result:= (ch=#10) or (ch=#13);
end;

function IsCharWord(ch: atChar; const AWordChars: atString): boolean;
begin
  Result:= false;

  case Ord(ch) of
    //Eng
    Ord('0')..Ord('9'),
    Ord('a')..Ord('z'),
    Ord('A')..Ord('Z'),
    Ord('_'),
    //German
    $E4, $C4, $E9, $F6, $D6, $FC, $DC, $DF,
    //Rus
    $0430..$044F, //a..z
    $0410..$042F, //A..Z
    $0451, $0401, //yo, Yo
    //Greek
    $0391..$03A9,
    $03B1..$03C9:
      begin Result:= true; Exit end;
  end;

  if AWordChars<>'' then
    if Pos(ch, AWordChars)>0 then
      Result:= true;
end;

function IsCharSpace(ch: atChar): boolean;
begin
  Result:= (ch=' ') or (ch=#9);
end;

function IsCharAsciiControl(ch: atChar): boolean;
begin
  Result:= (ch<>#9) and (Ord(ch)<$20);
end;

function IsCharHex(ch: atChar): boolean;
begin
  Result:= Pos(ch, OptHexCharsDefault+OptHexCharsUser)>0;
end;

function IsStringWithUnicodeChars(const S: atString): boolean;
var
  i, N: integer;
begin
  Result:= false;
  for i:= 1 to Length(S) do
  begin
    N:= Ord(S[i]);
    if (N<32) or (N>126) then exit(true);
  end;
end;


procedure DoDebugOffsets(const List: TATRealArray);
var
  i: integer;
  s: string;
begin
  s:= '';
  for i:= Low(List) to High(List) do
    s:= s+FloatToStr(List[i])+' ';
  showmessage('Offsets'#13+s);
end;

function SFindWordWrapOffset(const S: atString; AColumns, ATabSize: integer;
  const AWordChars: atString; AWrapIndented: boolean): integer;
  //
  //override IsCharWord to check also commas,dots,quotes
  //to wrap them with wordchars
  function _IsWord(ch: atChar): boolean;
  begin
    Result:= IsCharWord(ch, AWordChars+OptCommaCharsWrapWithWords);
  end;
  //
var
  N, NMin, NAvg: integer;
  List: TATRealArray;
begin
  if S='' then
    begin Result:= 0; Exit end;
  if AColumns<cMinWordWrapOffset then
    begin Result:= AColumns; Exit end;

  SetLength(List, Length(S));
  SCalcCharOffsets(S, List, ATabSize);

  if List[High(List)]<=AColumns then
  begin
    Result:= Length(S);
    Exit
  end;

  //NAvg is average wrap offset, we use it if no correct offset found
  N:= Length(S)-1;
  while (N>0) and (List[N]>AColumns+1) do Dec(N);
  NAvg:= N;
  if NAvg<cMinWordWrapOffset then
    begin Result:= cMinWordWrapOffset; Exit end;

  //find correct offset: not allowed at edge
  //a) 2 wordchars,
  //b) space as 2nd char (not nice look for Python src)
  NMin:= SGetIndentChars(S)+1;
  while (N>NMin) and
    ((_IsWord(S[N]) and _IsWord(S[N+1])) or
     (AWrapIndented and IsCharSpace(S[N+1])))
    do Dec(N);

  //use correct of avg offset
  if N>NMin then
    Result:= N
  else
    Result:= NAvg;
end;

function SGetIndentChars(const S: atString): integer;
begin
  Result:= 0;
  while (Result<Length(S)) and IsCharSpace(S[Result+1]) do
    Inc(Result);
end;

function SGetNonSpaceLength(const S: atString): integer;
begin
  Result:= Length(S);
  while (Result>0) and IsCharSpace(S[Result]) do Dec(Result);
  if Result=0 then
    Result:= Length(S);
end;

function SGetIndentExpanded(const S: atString; ATabSize: integer): integer;
var
  SIndent: atString;
begin
  SIndent:= Copy(S, 1, SGetIndentChars(S));
  SIndent:= STabsToSpaces(SIndent, ATabSize);
  Result:= Length(SIndent);
end;

function SSwapEndian(const S: UnicodeString): UnicodeString;
var
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    Result[i]:= WideChar(SwapEndian(Ord(Result[i])));
end;

function SCalcTabulationSize(const ATabSize, APos: integer): integer;
begin
  Result:= 1;
  if APos>cMaxTabPositionToExpand then Exit;
  while (APos+Result-1) mod ATabSize <> 0 do
    Inc(Result);
end;

function STabsToSpaces(const S: atString; ATabSize: integer): atString;
var
  N, NSize: integer;
begin
  Result:= S;
  repeat
    N:= Pos(#9, Result);
    if N=0 then Break;
    NSize:= SCalcTabulationSize(ATabSize, N);
    if NSize<=1 then
      Result[N]:= ' '
    else
    begin
      Delete(Result, N, 1);
      Insert(StringOfChar(' ', NSize), Result, N);
    end;
  until false;
end;

{
http://en.wikipedia.org/wiki/Combining_character
Combining Diacritical Marks (0300–036F), since version 1.0, with modifications in subsequent versions down to 4.1
Combining Diacritical Marks Extended (1AB0–1AFF), version 7.0
Combining Diacritical Marks Supplement (1DC0–1DFF), versions 4.1 to 5.2
Combining Diacritical Marks for Symbols (20D0–20FF), since version 1.0, with modifications in subsequent versions down to 5.1
Combining Half Marks (FE20–FE2F), versions 1.0, updates in 5.2
}
{
http://www.unicode.org/charts/PDF/U0E80.pdf
cannot render them ok anyway as accents:
0EB1, 0EB4..0EBC, 0EC8..0ECD
}
function IsCharAccent(ch: atChar): boolean;
begin
  case Ord(ch) of
    $0300..$036F,
    $1AB0..$1AFF,
    $1DC0..$1DFF,
    $20D0..$20FF,
    {$ifdef unix}
    $0EB1, $0EB4..$0EBC, $0EC8..$0ECD, //Lao accent chars
    {$endif}
    $FE20..$FE2F:
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsCharFullWidth(ch: atChar): boolean;
begin
  case Ord(ch) of
    $1100..$115F,
    $2329..$232A,
    $2E80..$303E,
    $3041..$33FF,
    $3400..$4DB5,
    $4E00..$9FC3,
    $A000..$A4C6,
    $AC00..$D7A3,
    $F900..$FAD9,
    $FE10..$FE19,
    $FE30..$FE6B,
    $FF01..$FF60,
    $FFE0..$FFE6:
      Result:= true;
    else
      Result:= false;
  end;
end;

{$ifdef test_wide_char}
const
  cScaleTest = 1.9; //debug, for test code, commented
{$endif}

procedure SCalcCharOffsets(const S: atString; var AList: TATRealArray;
  ATabSize: integer; ACharsSkipped: integer);
var
  NSize, NTabSize, NCharsSkipped: integer;
  Scale: real;
  i: integer;
begin
  if S='' then Exit;
  if Length(AList)<>Length(S) then
    raise Exception.Create('Bad list len: CalcCharOffsets');

  NCharsSkipped:= ACharsSkipped;

  for i:= 1 to Length(S) do
  begin
    Inc(NCharsSkipped);

    Scale:= 1.0;
    if OptUnprintedReplaceSpec and IsCharAsciiControl(S[i]) then
      Scale:= 1.0
    else
    if IsCharHex(S[i]) then
      Scale:= cCharScaleHex
    else
    if IsCharFullWidth(S[i]) then
      Scale:= cCharScaleFullwidth;

    {$ifdef test_wide_char}
    if IsSpaceChar(S[i]) then
      Scale:= 1
    else
      Scale:= cScaleTest;
    {$endif}

    if S[i]<>#9 then
      NSize:= 1
    else
    begin
      NTabSize:= SCalcTabulationSize(ATabSize, NCharsSkipped);
      NSize:= NTabSize;
      Inc(NCharsSkipped, NTabSize-1);
    end;

    if (i<Length(S)) and IsCharAccent(S[i+1]) then
      NSize:= 0;

    if i=1 then
      AList[i-1]:= NSize*Scale
    else
      AList[i-1]:= AList[i-2]+NSize*Scale;
  end;
end;

function SFindClickedPosition(const Str: atString;
  APixelsFromLeft, ACharSize, ATabSize: integer;
  AAllowVirtualPos: boolean;
  out AEndOfLinePos: boolean): integer;
var
  ListReal: TATRealArray;
  ListEnds, ListMid: TATIntArray;
  i: integer;
begin
  AEndOfLinePos:= false;
  if Str='' then
  begin
    if AAllowVirtualPos then
      Result:= 1+APixelsFromLeft div ACharSize
    else
      Result:= 1;
    Exit;
  end;

  SetLength(ListReal, Length(Str));
  SetLength(ListEnds, Length(Str));
  SetLength(ListMid, Length(Str));
  SCalcCharOffsets(Str, ListReal, ATabSize);

  //positions of each char end
  for i:= 0 to High(ListEnds) do
    ListEnds[i]:= Trunc(ListReal[i]*ACharSize);

  //positions of each char middle
  for i:= 0 to High(ListEnds) do
    if i=0 then
      ListMid[i]:= ListEnds[i] div 2
    else
      ListMid[i]:= (ListEnds[i-1]+ListEnds[i]) div 2;

  for i:= 0 to High(ListEnds) do
    if APixelsFromLeft<ListMid[i] then
    begin
      Result:= i+1;
      Exit
    end;

  AEndOfLinePos:= true;
  if AAllowVirtualPos then
    Result:= Length(Str)+1 + (APixelsFromLeft - ListEnds[High(ListEnds)]) div ACharSize
  else
    Result:= Length(Str)+1;
end;

procedure SFindOutputSkipOffset(const S: atString; ATabSize, AScrollPos: integer;
  out ACharsSkipped: integer; out ASpacesSkipped: real);
var
  List: TATRealArray;
begin
  ACharsSkipped:= 0;
  ASpacesSkipped:= 0;
  if (S='') or (AScrollPos=0) then Exit;

  SetLength(List, Length(S));
  SCalcCharOffsets(S, List, ATabSize);

  while (ACharsSkipped<Length(S)) and (List[ACharsSkipped]<AScrollPos) do
    Inc(ACharsSkipped);

  if (ACharsSkipped>0) then
    ASpacesSkipped:= List[ACharsSkipped-1];
end;


function BoolToPlusMinusOne(b: boolean): integer;
begin
  if b then Result:= 1 else Result:= -1;
end;

function SGetItem(var S: string; const sep: Char = ','): string;
var
  i: integer;
begin
  i:= Pos(sep, s);
  if i=0 then i:= MaxInt;
  Result:= Copy(s, 1, i-1);
  Delete(s, 1, i);
end;

function SGetItemAtEnd(var S: string; const sep: Char = ','): string;
var
  i: integer;
begin
  Result:= '';
  i:= Pos(sep, S);
  if i>0 then
  begin
    Result:= Copy(S, i+1, MaxInt);
    Delete(S, i, MaxInt);
  end;
end;

procedure TrimStringList(L: TStringList);
begin
  //dont do "while", we need correct last empty lines
  if (L.Count>0) and (L[L.Count-1]='') then
    L.Delete(L.Count-1);
end;

function SWithBreaks(const S: atString): boolean;
begin
  Result:=
    (Pos(#13, S)>0) or
    (Pos(#10, S)>0);
end;

function SSpacesToTabs(const S: atString; ATabSize: integer): atString;
begin
  Result:= StringReplace(S, StringOfChar(' ', ATabSize), #9, [rfReplaceAll]);
end;

function SCharPosToColumnPos(const S: atString; APos, ATabSize: integer): integer;
begin
  Result:= Length(STabsToSpaces(Copy(S, 1, APos), ATabSize));
  if APos>Length(S) then
    Inc(Result, APos-Length(S));
end;

function SColumnPosToCharPos(const S: atString; AColumn, ATabSize: integer): integer;
var
  size, i: integer;
begin
  if AColumn=0 then exit(AColumn);
  if Pos(#9, S)=0 then exit(AColumn);

  size:= 0;
  for i:= 1 to Length(S) do
  begin
    if S[i]<>#9 then
      Inc(size)
    else
      Inc(size, SCalcTabulationSize(ATabSize, size+1));
    if size>=AColumn then
      exit(i);
  end;

  Result:= AColumn-Length(STabsToSpaces(S, ATabSize))+Length(S);
end;

function SIndentUnindent(const Str: atString; ARight: boolean;
  AIndentSize, ATabSize: integer): atString;
var
  StrIndent, StrText: atString;
  DecSpaces, N: integer;
  DoTabs: boolean;
begin
  Result:= Str;

  //indent<0 - use tabs
  if AIndentSize>=0 then
  begin
    StrIndent:= StringOfChar(' ', AIndentSize);
    DecSpaces:= AIndentSize;
  end
  else
  begin
    StrIndent:= StringOfChar(#9, Abs(AIndentSize));
    DecSpaces:= Abs(AIndentSize)*ATabSize;
  end;

  if ARight then
    Result:= StrIndent+Str
  else
  begin
    N:= SGetIndentChars(Str);
    StrIndent:= Copy(Str, 1, N);
    StrText:= Copy(Str, N+1, MaxInt);
    DoTabs:= Pos(#9, StrIndent)>0;

    StrIndent:= STabsToSpaces(StrIndent, ATabSize);
    if DecSpaces>Length(StrIndent) then
      DecSpaces:= Length(StrIndent);
    Delete(StrIndent, 1, DecSpaces);

    if DoTabs then
      StrIndent:= SSpacesToTabs(StrIndent, ATabSize);
    Result:= StrIndent+StrText;
  end;
end;

function SRemoveAsciiControlChars(const S: atString): atString;
var
  i: integer;
begin
  Result:= S;
  if OptUnprintedReplaceSpec then
    for i:= 1 to Length(Result) do
      if IsCharAsciiControl(Result[i]) then
        Result[i]:= '.';
end;

function SRemoveHexChars(const S: atString): atString;
var
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    if IsCharHex(Result[i]) then
      Result[i]:= '?';
end;

function SRemoveNewlineChars(const S: atString): atString;
var
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    if IsCharEol(Result[i]) then
      Result[i]:= ' ';
end;


{
http://unicode.org/reports/tr9/#Directional_Formatting_Characters
Implicit Directional Formatting Characters 	LRM, RLM, ALM
Explicit Directional Embedding and Override Formatting Characters 	LRE, RLE, LRO, RLO, PDF
Explicit Directional Isolate Formatting Characters 	LRI, RLI, FSI, PDI
}
const
  cDirCodes: UnicodeString =
    #$202A {LRE} + #$202B {RLE} + #$202D {LRO} + #$202E {RLO} + #$202C {PDF} +
    #$2066 {LRI} + #$2067 {RLI} + #$2068 {FSI} + #$2069 {PDI} +
    #$200E {LRM} + #$200F {RLM} + #$061C {ALM};

procedure _InitCharsHex;
var
  i: integer;
begin
  OptHexCharsDefault:= '';

  for i:= 0 to 31 do
    if (i<>13) and (i<>10) and (i<>9) then
      OptHexCharsDefault:= OptHexCharsDefault+Chr(i);

  OptHexCharsDefault:= OptHexCharsDefault + cDirCodes;
end;


function STrimRight(const S: atString): atString;
var
  N: integer;
begin
  N:= Length(S);
  while (N>0) and (S[N]=' ') do Dec(N);
  Result:= Copy(S, 1, N);
end;

function SBeginsWith(const S, SubStr: atString): boolean;
begin
  Result:= (SubStr<>'') and (Copy(S, 1, Length(SubStr))=SubStr);
end;

function SBeginsWith(const S, SubStr: string): boolean;
begin
  Result:= (SubStr<>'') and (Copy(S, 1, Length(SubStr))=SubStr);
end;

function SEndsWith(const S, SubStr: atString): boolean;
begin
  Result:= (SubStr<>'') and (Length(SubStr)<=Length(S)) and
    (Copy(S, Length(S)-Length(SubStr)+1, MaxInt)=SubStr);
end;

function SEndsWith(const S, SubStr: string): boolean;
begin
  Result:= (SubStr<>'') and (Length(SubStr)<=Length(S)) and
    (Copy(S, Length(S)-Length(SubStr)+1, MaxInt)=SubStr);
end;


function SCharUpper(ch: atChar): atChar;
begin
  Result:= UnicodeUpperCase(ch)[1];
end;

function SCharLower(ch: atChar): atChar;
begin
  Result:= UnicodeLowerCase(ch)[1];
end;


function SCaseTitle(const S, SWordChars: atString): atString;
var
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    if (i=1) or not IsCharWord(S[i-1], SWordChars) then
      Result[i]:= SCharUpper(Result[i])
    else
      Result[i]:= SCharLower(Result[i]);
end;

function SCaseInvert(const S: atString): atString;
var
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    if S[i]<>SCharUpper(S[i]) then
      Result[i]:= SCharUpper(Result[i])
    else
      Result[i]:= SCharLower(Result[i]);
end;

function SCaseSentence(const S, SWordChars: atString): atString;
var
  dot: boolean;
  i: Integer;
begin
  Result:= S;
  dot:= True;
  for i:= 1 to Length(Result) do
  begin
    if IsCharWord(Result[i], SWordChars) then
    begin
      if dot then
        Result[i]:= SCharUpper(Result[i])
      else
        Result[i]:= SCharLower(Result[i]);
      dot:= False;
    end
    else
      if (Result[i] = '.') or (Result[i] = '!') or (Result[i] = '?') then
        dot:= True;
  end;
end;


function SDecodeRecords(const S: UnicodeString; const Decode: array of TATDecodeRec): UnicodeString;
var
  i, j: Integer;
  DoDecode: Boolean;
begin
  Result := '';
  i := 1;
  repeat
    if i > Length(S) then Break;
    DoDecode := False;
    for j := Low(Decode) to High(Decode) do
      with Decode[j] do
        if SFrom = Copy(S, i, Length(SFrom)) then
        begin
          DoDecode := True;
          Result := Result + STo;
          Inc(i, Length(SFrom));
          Break
        end;
    if DoDecode then Continue;
    Result := Result + S[i];
    Inc(i);
  until False;
end;


function SCommentLineAction(L: TStringList;
  const AComment: atString; Act: TATCommentAction): boolean;
var
  Str, Str0: atString;
  IndentThis, IndentAll, i: integer;
  IsCmtThis, IsCmtAll: boolean;
begin
  Result:= false;
  if L.Count=0 then exit;

  IndentAll:= MaxInt;
  for i:= 0 to L.Count-1 do
    IndentAll:= Min(IndentAll, SGetIndentChars(L[i])+1);
    //no need Utf8decode

  for i:= 0 to L.Count-1 do
  begin
    Str:= Utf8Decode(L[i]);
    Str0:= Str;

    //IndentThis, IsCmtThis: regarding indent if this line
    //IndentAll, IsCmtAll: regarding minimal indent of block
    IndentThis:= SGetIndentChars(Str)+1;
    IsCmtThis:= Copy(Str, IndentThis, Length(AComment))=AComment;
    IsCmtAll:= Copy(Str, IndentAll, Length(AComment))=AComment;

    case Act of
      cCommentAdd_AtNonspace:
        begin
          Insert(AComment, Str, IndentAll);
        end;
      cCommentAdd_AtNonespace_IfNone:
        begin
          if not IsCmtAll then
            Insert(AComment, Str, IndentAll);
        end;
      cCommentAdd_AtStart:
        begin
          Insert(AComment, Str, 1);
        end;
      cCommentAdd_AtStart_IfNone:
        begin
          if not IsCmtAll then
            Insert(AComment, Str, 1);
        end;
      cCommentRemove:
        begin
          if IsCmtAll then
            Delete(Str, IndentAll, Length(AComment))
          else
          if IsCmtThis then
            Delete(Str, IndentThis, Length(AComment))
        end;
      cCommentToggle_AtNonspace:
        begin
          if IsCmtAll then
            Delete(Str, IndentAll, Length(AComment))
          else
            Insert(AComment, Str, IndentAll);
        end;
      cCommentToggle_AtStart:
        begin
          if IsCmtAll then
            Delete(Str, IndentAll, Length(AComment))
          else
            Insert(AComment, Str, 1);
        end;
    end;

    if Str<>Str0 then
    begin
      Result:= true; //modified
      L[i]:= Utf8Encode(Str);
    end;
  end;
end;


procedure SReplaceAll(var s: string; const SFrom, STo: string);
begin
  S:= StringReplace(S, SFrom, STo, [rfReplaceAll]);
end;

procedure SReplaceAllPercentChars(var S: string);
var
  i: Integer;
begin
  for i:= $20 to $2F do
    SReplaceAll(S, '%'+IntToHex(i, 2), Chr(i));

  i:= $7C;
  SReplaceAll(S, '%'+IntToHex(i, 2), Chr(i));
end;

procedure SDeleteFrom(var s: string; const SFrom: string);
var
  n: integer;
begin
  n:= Pos(SFrom, S);
  if n>0 then
    Delete(S, n, MaxInt);
end;

procedure SAddStringToHistory(const S: string; List: TStrings; MaxItems: integer);
var
  n: integer;
begin
  if s<>'' then
  begin
    n:= List.IndexOf(s);
    if n>=0 then
      List.Delete(n);
    List.Insert(0, s);
  end;

  while List.Count>MaxItems do
    List.Delete(List.Count-1);
end;


initialization
  _InitCharsHex;

end.

