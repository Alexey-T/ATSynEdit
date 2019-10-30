{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc;

{$mode objfpc}{$H+}
{$codepage utf8}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, StrUtils,
  LCLType, LCLIntf, Clipbrd,
  UnicodeData,
  ATSynEdit_RegExpr,
  ATSynEdit_CharSizer;

type
  atString = UnicodeString;
  atChar = WideChar;
  PatChar = PWideChar;

type
  TATLineChangeKind = (
    cLineChangeEdited,
    cLineChangeAdded,
    cLineChangeDeleted,
    cLineChangeDeletedAll
    );

type
  TATIntArray = array of integer;
  TATPointArray = array of TPoint;
  TATLineOffsetsInfo = array of integer; //word is too small
  TATSimpleRange = record NFrom, NTo: integer; end;
  TATSimpleRangeArray = array of TATSimpleRange;

function SCharUpper(ch: widechar): widechar; inline;
function SCharLower(ch: widechar): widechar; inline;
function SCaseTitle(const S, SNonWordChars: atString): atString;
function SCaseInvert(const S: atString): atString;
function SCaseSentence(const S, SNonWordChars: atString): atString;

type
  { TATStringSeparator }

  TATStringSeparator = record
  private
    FSep: char;
    FStr: string;
    FPos: integer;
  public
    procedure Init(const AStr: string; ASep: char=',');
    function GetItemStr(out AValue: string): boolean;
    function GetItemStr(out AValue: UnicodeString): boolean;
    function GetItemInt(out AValue: integer; const ADefault: integer): boolean;
  end;

{$Z1}
type
  TATLineEnds = (cEndNone, cEndWin, cEndUnix, cEndMac);
const
  cLineEndStrings: array[TATLineEnds] of string = ('', #13#10, #10, #13);
  cLineEndNiceNames: array[TATLineEnds] of string = ('', 'CRLF', 'LF', 'CR');
  cLineEndLength: array[TATLineEnds] of integer = (0, 2, 1, 1);

var
  EditorScalePercents: integer = 100;
  EditorScaleFontPercents: integer = 0; //if 0, it follows previous variable

function EditorScale(AValue: integer): integer; inline;
function EditorScaleFont(AValue: integer): integer;

var
  OptEmojiWidthPercents: integer = 210;
  OptMaxTabPositionToExpand: integer = 500; //no sense to expand too far tabs
  OptMinWordWrapOffset: integer = 3;
  OptCommaCharsWrapWithWords: UnicodeString = '.,;:''"`~?!&%$';

type
  TATStringTabCalcEvent = function(Sender: TObject; ALineIndex, ACharIndex: integer): integer of object;

type

  { TATStringTabHelper }

  TATStringTabHelper = class
  private
  public
    TabSpaces: boolean;
    TabSize: integer;
    IndentSize: integer;
    SenderObj: TObject;
    OnCalcTabSize: TATStringTabCalcEvent;
    function CalcTabulationSize(ALineIndex, APos: integer): integer;
    function TabsToSpaces(ALineIndex: integer; const S: atString): atString;
    function TabsToSpaces_Length(ALineIndex: integer; const S: atString; AMaxLen: integer): integer;
    function SpacesToTabs(ALineIndex: integer; const S: atString): atString;
    function GetIndentExpanded(ALineIndex: integer; const S: atString): integer;
    function CharPosToColumnPos(ALineIndex: integer; const S: atString; APos: integer): integer;
    function ColumnPosToCharPos(ALineIndex: integer; const S: atString; AColumn: integer): integer;
    function IndentUnindent(ALineIndex: integer; const Str: atString; ARight: boolean): atString;
    procedure CalcCharOffsets(ALineIndex: integer; const S: atString; var AInfo: TATLineOffsetsInfo; ACharsSkipped: integer = 0);
    function FindWordWrapOffset(ALineIndex: integer; const S: atString; AColumns: integer;
      const ANonWordChars: atString; AWrapIndented: boolean): integer;
    function FindClickedPosition(ALineIndex: integer; const Str: atString;
      APixelsFromLeft, ACharSize: integer;
      AAllowVirtualPos: boolean;
      out AEndOfLinePos: boolean): integer;
    procedure FindOutputSkipOffset(ALineIndex: integer; const S: atString; AScrollPos: integer;
      out ACharsSkipped: integer; out ASpacesSkipped: integer);
  end;

function IsCharEol(ch: widechar): boolean; inline;
function IsCharWord(ch: widechar; const ANonWordChars: atString): boolean;
function IsCharWordInIdentifier(ch: widechar): boolean;
function IsCharDigit(ch: widechar): boolean; inline;
function IsCharSpace(ch: widechar): boolean; inline;
function IsCharSymbol(ch: widechar): boolean;

function IsCharSurrogateAny(ch: widechar): boolean; inline;
function IsCharSurrogateHigh(ch: widechar): boolean; inline;
function IsCharSurrogateLow(ch: widechar): boolean; inline;

function IsStringWithUnicodeChars(const S: atString): boolean;
function IsStringSpaces(const S: atString): boolean; inline;
function IsStringSpaces(const S: atString; AFrom, ALen: integer): boolean;

function SBeginsWith(const S, SubStr: atString): boolean; inline;
function SBeginsWith(const S, SubStr: string): boolean; inline;
function SEndsWith(const S, SubStr: atString): boolean; inline;
function SEndsWith(const S, SubStr: string): boolean; inline;
function SEndsWithEol(const S: string): boolean; inline;
function SEndsWithEol(const S: atString): boolean; inline;

function STrimRight(const S: atString): atString;
function SGetIndentChars(const S: atString): integer;
function SGetIndentCharsToOpeningBracket(const S: atString): integer;
function SGetTrailingSpaceChars(const S: atString): integer;
function SGetNonSpaceLength(const S: atString): integer;

function SStringHasTab(const S: atString): boolean; inline;
function SStringHasTab(const S: string): boolean; inline;
function SStringHasAsciiAndNoTabs(const S: atString): boolean;
function SStringHasAsciiAndNoTabs(const S: string): boolean;

function SRemoveNewlineChars(const S: atString): atString;
function SRemoveHexChars(const S: atString): atString;
function SRemoveAsciiControlChars(const S: atString; AReplaceChar: Widechar): atString;

function SGetItem(var S: string; const ch: Char = ','): string;
function SSwapEndian(const S: UnicodeString): UnicodeString;
function SWithBreaks(const S: atString): boolean; inline;
procedure SAddStringToHistory(const S: string; List: TStrings; MaxItems: integer);

function BoolToPlusMinusOne(b: boolean): integer; inline;
procedure TrimStringList(L: TStringList); inline;

const
  cDefaultNonWordChars: UnicodeString = '-+*=/\()[]{}<>"''.,:;~?!@#$%^&|`…';

type
  TATDecodeRec = record SFrom, STo: UnicodeString; end;
function SDecodeRecords(const S: UnicodeString; const Decode: array of TATDecodeRec): UnicodeString;
function SConvertUtf8ToWideForAscii(const S: string): UnicodeString;

procedure SReplaceAll(var S: string; const SFrom, STo: string); inline;
procedure SReplaceAllPercentChars(var S: string);
procedure SReplaceAllTabsToOneSpace(var S: string); inline;
procedure SReplaceAllTabsToOneSpace(var S: UnicodeString); inline;
procedure SDeleteFrom(var s: string; const SFrom: string); inline;
procedure SDeleteFrom(var s: atString; const SFrom: atString); inline;
procedure SDeleteFromEol(var S: string); inline;
procedure SDeleteFromEol(var S: atString); inline;

procedure SClipboardCopy(AText: string; AClipboardObj: TClipboard=nil);
function SFindCharCount(const S: UnicodeString; ch: WideChar): integer;
function SFindRegexMatch(const Subject, Regex: UnicodeString; out MatchPos, MatchLen: integer): boolean;
function SCountTextOccurrences(const SubStr, Str: UnicodeString): integer;
function SCountTextLines(const Str, StrBreak: UnicodeString): integer;
procedure SSplitByChar(const S: string; Sep: char; out S1, S2: string);


implementation

uses
  Dialogs, Math;

function IsCharEol(ch: widechar): boolean; inline;
begin
  Result:= (ch=#10) or (ch=#13);
end;

function IsCharWord(ch: widechar; const ANonWordChars: atString): boolean;
var
  NType: byte;
begin
  case ch of
    '0'..'9',
    'a'..'z',
    'A'..'Z',
    '_':
      exit(true);
    #0..' ':
      exit(false);
  end;

  if Pos(ch, ANonWordChars)>0 then
    exit(false);

  if Ord(ch)<128 then
    Result:= true
  else
  if Ord(ch)>=LOW_SURROGATE_BEGIN then
    exit(false)
  else
  begin
    NType:= GetProps(Ord(ch))^.Category;
    Result:= (NType<=UGC_OtherNumber);
  end;
end;

function IsCharWordInIdentifier(ch: widechar): boolean;
begin
  case ch of
    '0'..'9',
    'a'..'z',
    'A'..'Z',
    '_':
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsCharDigit(ch: widechar): boolean; inline;
begin
  Result:= (ch>='0') and (ch<='9');
end;

function IsCharSpace(ch: widechar): boolean; inline;
begin
  case ch of
    #9, //tab
    ' ', //space
    #$A0, //no-break space, NBSP, often used on macOS
    #$1680, //white space
    #$2007, //figure space
    #$200B, //zero width space https://en.wikipedia.org/wiki/Zero-width_space
    #$202F, //narrow no-break space
    #$205F, //white space
    #$2060, //white space
    #$3000: //CJK white space
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsCharSymbol(ch: widechar): boolean;
begin
  Result:= Pos(ch, '.,;:''"/\-+*=()[]{}<>?!@#$%^&|~`')>0;
end;

function IsCharSurrogateAny(ch: widechar): boolean; inline;
begin
  Result:= (ch>=#$D800) and (ch<=#$DFFF);
end;

function IsCharSurrogateHigh(ch: widechar): boolean; inline;
begin
  Result:= (ch>=#$D800) and (ch<=#$DBFF);
end;

function IsCharSurrogateLow(ch: widechar): boolean; inline;
begin
  Result:= (ch>=#$DC00) and (ch<=#$DFFF);
end;


function IsStringSpaces(const S: atString): boolean; inline;
begin
  Result:= IsStringSpaces(S, 1, Length(S));
end;

function IsStringSpaces(const S: atString; AFrom, ALen: integer): boolean;
var
  NLen, i: integer;
begin
  Result:= true;
  NLen:= Length(S);
  for i:= AFrom to AFrom+ALen-1 do
  begin
    if i>NLen then exit;
    if not IsCharSpace(S[i]) then exit(false);
  end;
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


procedure DoDebugOffsets(const AList: TATLineOffsetsInfo);
var
  i: integer;
  s: string;
begin
  s:= '';
  for i:= Low(AList) to High(AList) do
    s:= s+IntToStr(AList[i])+'% ';
  ShowMessage('Offsets'#10+s);
end;

function TATStringTabHelper.FindWordWrapOffset(ALineIndex: integer; const S: atString; AColumns: integer;
  const ANonWordChars: atString; AWrapIndented: boolean): integer;
  //
  //override IsCharWord to check also commas,dots,quotes
  //to wrap them with wordchars
  function _IsWord(ch: widechar): boolean; inline;
  begin
    if Pos(ch, OptCommaCharsWrapWithWords)>0 then
      Result:= true
    else
      Result:= IsCharWord(ch, ANonWordChars);
  end;
  //
var
  N, NMin, NAvg: integer;
  Offsets: TATLineOffsetsInfo;
begin
  if S='' then
    Exit(0);
  if AColumns<OptMinWordWrapOffset then
    Exit(AColumns);

  CalcCharOffsets(ALineIndex, S, Offsets);

  if Offsets[High(Offsets)]<=AColumns*100 then
    Exit(Length(S));

  //NAvg is average wrap offset, we use it if no correct offset found
  N:= Length(S)-1;
  while (N>0) and (Offsets[N]>(AColumns+1)*100) do Dec(N);
  NAvg:= N;
  if NAvg<OptMinWordWrapOffset then
    Exit(OptMinWordWrapOffset);

  //find correct offset: not allowed at edge
  //a) 2 wordchars,
  //b) space as 2nd char (not nice look for Python src)
  NMin:= SGetIndentChars(S)+1;
  while (N>NMin) and
    (IsCharSurrogateLow(S[N+1]) or
     (_IsWord(S[N]) and _IsWord(S[N+1])) or
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

function SGetTrailingSpaceChars(const S: atString): integer;
var
  N: integer;
begin
  Result:= 0;
  N:= Length(S);
  while (N>0) and IsCharSpace(S[N]) do
  begin
    Inc(Result);
    Dec(N);
  end;
end;


function SGetIndentCharsToOpeningBracket(const S: atString): integer;
var
  n: integer;
begin
  Result:= 0;
  n:= Length(S);
  //note RPos() don't work with UnicodeString
  while (n>0) and (S[n]<>'(') do Dec(n);
  if n>0 then
    //test that found bracket is not closed
    if PosEx(')', S, n)=0 then
      Result:= n;
end;

function SGetNonSpaceLength(const S: atString): integer;
begin
  Result:= Length(S);
  while (Result>0) and IsCharSpace(S[Result]) do Dec(Result);
  if Result=0 then
    Result:= Length(S);
end;


function SSwapEndian(const S: UnicodeString): UnicodeString;
var
  i: integer;
  p: PWord;
begin
  Result:= S;
  if S='' then exit;
  UniqueString(Result);
  P:= PWord(@Result[1]);
  for i:= 1 to Length(Result) do
  begin
    P^:= SwapEndian(P^);
    Inc(P);
  end;
end;

function TATStringTabHelper.CalcTabulationSize(ALineIndex, APos: integer): integer;
begin
  if Assigned(OnCalcTabSize) then
    Result:= OnCalcTabSize(SenderObj, ALineIndex, APos)
  else
  if APos<=OptMaxTabPositionToExpand then
    Result:= TabSize - (APos-1) mod TabSize
  else
    Result:= 1;
end;


function TATStringTabHelper.GetIndentExpanded(ALineIndex: integer; const S: atString): integer;
var
  ch: widechar;
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
  begin
    ch:= S[i];
    if not IsCharSpace(ch) then exit;
    if ch<>#9 then
      Inc(Result)
    else
      Inc(Result, CalcTabulationSize(ALineIndex, Result+1));
  end;
end;


function TATStringTabHelper.TabsToSpaces(ALineIndex: integer; const S: atString): atString;
var
  N, NSize: integer;
begin
  Result:= S;
  N:= 0;
  repeat
    N:= PosEx(#9, Result, N+1);
    if N=0 then Break;
    NSize:= CalcTabulationSize(ALineIndex, N);
    if NSize<2 then
      Result[N]:= ' '
    else
    begin
      Result[N]:= ' ';
      Insert(StringOfChar(' ', NSize-1), Result, N);
    end;
  until false;
end;

function TATStringTabHelper.TabsToSpaces_Length(ALineIndex: integer; const S: atString;
  AMaxLen: integer): integer;
var
  i: integer;
begin
  Result:= 0;
  if AMaxLen<0 then
    AMaxLen:= Length(S);
  for i:= 1 to AMaxLen do
    if S[i]<>#9 then
      Inc(Result)
    else
      Inc(Result, CalcTabulationSize(ALineIndex, Result+1));
end;


procedure TATStringTabHelper.CalcCharOffsets(ALineIndex: integer; const S: atString;
  var AInfo: TATLineOffsetsInfo; ACharsSkipped: integer);
var
  NSize, NTabSize, NCharsSkipped: integer;
  NScalePercents: integer;
  //NPairSize: integer;
  //StrPair: WideString;
  ch: widechar;
  i: integer;
begin
  SetLength(AInfo, Length(S));
  if S='' then Exit;

  NCharsSkipped:= ACharsSkipped;
  //NPairSize:= 0;
  //StrPair:= 'ab';

  for i:= 1 to Length(S) do
  begin
    ch:= S[i];
    Inc(NCharsSkipped);

    {
    ////if used GetStrWidth, then strange bug on Win32, Emoji wrap pos is not ok
    if (NPairSize>0) and IsCharSurrogateLow(ch) then
    begin
      NScalePercents:= NPairSize div 2;
      NPairSize:= 0;
    end
    else
    if IsCharSurrogateHigh(ch) and (i<Length(S)) then
    begin
      StrPair[1]:= ch;
      StrPair[2]:= S[i+1];
      NPairSize:= GlobalCharSizer.GetStrWidth(StrPair);
      NScalePercents:= NPairSize - NPairSize div 2;
    end
    }
    if IsCharSurrogateAny(ch) then
    begin
      NScalePercents:= OptEmojiWidthPercents div 2;
    end
    else
    begin
      NScalePercents:= GlobalCharSizer.GetCharWidth(ch);
      //NPairSize:= 0;
    end;

    if ch<>#9 then
      NSize:= 1
    else
    begin
      NTabSize:= CalcTabulationSize(ALineIndex, NCharsSkipped);
      NSize:= NTabSize;
      Inc(NCharsSkipped, NTabSize-1);
    end;

    if (i<Length(S)) and IsCharAccent(S[i+1]) then
      NSize:= 0;

    if i=1 then
      AInfo[i-1]:= NSize*NScalePercents
    else
      AInfo[i-1]:= AInfo[i-2]+NSize*NScalePercents;
  end;
end;

function TATStringTabHelper.FindClickedPosition(ALineIndex: integer; const Str: atString; APixelsFromLeft,
  ACharSize: integer; AAllowVirtualPos: boolean; out AEndOfLinePos: boolean): integer;
var
  ListOffsets: TATLineOffsetsInfo;
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

  SetLength(ListEnds, Length(Str));
  SetLength(ListMid, Length(Str));
  CalcCharOffsets(ALineIndex, Str, ListOffsets);

  //positions of each char end
  for i:= 0 to High(ListEnds) do
    ListEnds[i]:= ListOffsets[i]*ACharSize div 100;

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

      //don't get position inside utf16 surrogate pair
      if (Result<=Length(Str)) and IsCharSurrogateLow(Str[Result]) then
        Inc(Result);

      Exit
    end;

  AEndOfLinePos:= true;
  if AAllowVirtualPos then
    Result:= Length(Str)+1 + (APixelsFromLeft - ListEnds[High(ListEnds)]) div ACharSize
  else
    Result:= Length(Str)+1;
end;

procedure TATStringTabHelper.FindOutputSkipOffset(ALineIndex: integer; const S: atString;
  AScrollPos: integer; out ACharsSkipped: integer; out ASpacesSkipped: integer);
var
  Offsets: TATLineOffsetsInfo;
begin
  ACharsSkipped:= 0;
  ASpacesSkipped:= 0;
  if (S='') or (AScrollPos=0) then Exit;

  CalcCharOffsets(ALineIndex, S, Offsets);

  while (ACharsSkipped<Length(S)) and
    (Offsets[ACharsSkipped] < AScrollPos*100) do
    Inc(ACharsSkipped);

  if (ACharsSkipped>0) then
    ASpacesSkipped:= Offsets[ACharsSkipped-1] div 100;
end;


function BoolToPlusMinusOne(b: boolean): integer; inline;
begin
  if b then Result:= 1 else Result:= -1;
end;

function SGetItem(var S: string; const ch: Char = ','): string;
var
  i: integer;
begin
  i:= Pos(ch, S);
  if i=0 then
  begin
    Result:= S;
    S:= '';
  end
  else
  begin
    Result:= Copy(S, 1, i-1);
    Delete(S, 1, i);
  end;
end;


procedure TrimStringList(L: TStringList); inline;
begin
  //dont do "while", we need correct last empty lines
  if (L.Count>0) and (L[L.Count-1]='') then
    L.Delete(L.Count-1);
end;

function SWithBreaks(const S: atString): boolean; inline;
begin
  Result:=
    (Pos(#13, S)>0) or
    (Pos(#10, S)>0);
end;

function TATStringTabHelper.SpacesToTabs(ALineIndex: integer; const S: atString): atString;
begin
  Result:= StringReplace(S, StringOfChar(' ', TabSize), #9, [rfReplaceAll]);
end;

function TATStringTabHelper.CharPosToColumnPos(ALineIndex: integer; const S: atString;
  APos: integer): integer;
begin
  if APos>Length(S) then
    Result:= TabsToSpaces_Length(ALineIndex, S, -1) + APos-Length(S)
  else
    Result:= TabsToSpaces_Length(ALineIndex, S, APos);
end;

function TATStringTabHelper.ColumnPosToCharPos(ALineIndex: integer; const S: atString;
  AColumn: integer): integer;
var
  size, i: integer;
begin
  if AColumn=0 then exit(AColumn);
  if not SStringHasTab(S) then exit(AColumn);

  size:= 0;
  for i:= 1 to Length(S) do
  begin
    if S[i]<>#9 then
      Inc(size)
    else
      Inc(size, CalcTabulationSize(ALineIndex, size+1));
    if size>=AColumn then
      exit(i);
  end;

  //column is too big, after line end
  Result:= AColumn - size + Length(S);
end;

function SStringHasTab(const S: atString): boolean; inline;
begin
  Result:= Pos(#9, S)>0;
end;

function SStringHasTab(const S: string): boolean; inline;
begin
  Result:= Pos(#9, S)>0;
end;


function SStringHasAsciiAndNoTabs(const S: atString): boolean;
var
  code, i: integer;
begin
  Result:= true;
  for i:= 1 to Length(S) do
  begin
    code:= Ord(S[i]);
    if (code<32) or (code>=127) then
      exit(false);
  end;
end;

function SStringHasAsciiAndNoTabs(const S: string): boolean;
var
  code, i: integer;
begin
  Result:= true;
  for i:= 1 to Length(S) do
  begin
    code:= Ord(S[i]);
    if (code<32) or (code>=127) then
      exit(false);
  end;
end;


function TATStringTabHelper.IndentUnindent(ALineIndex: integer; const Str: atString;
  ARight: boolean): atString;
var
  StrIndent, StrText: atString;
  DecSpaces, N: integer;
  DoTabs: boolean;
begin
  Result:= Str;

  if IndentSize=0 then
  begin
    if TabSpaces then
      StrIndent:= StringOfChar(' ', TabSize)
    else
      StrIndent:= #9;
    DecSpaces:= TabSize;
  end
  else
  if IndentSize>0 then
  begin
    //use spaces
    StrIndent:= StringOfChar(' ', IndentSize);
    DecSpaces:= IndentSize;
  end
  else
  begin
    //indent<0 - use tabs
    StrIndent:= StringOfChar(#9, Abs(IndentSize));
    DecSpaces:= Abs(IndentSize)*TabSize;
  end;

  if ARight then
    Result:= StrIndent+Str
  else
  begin
    N:= SGetIndentChars(Str);
    StrIndent:= Copy(Str, 1, N);
    StrText:= Copy(Str, N+1, MaxInt);
    DoTabs:= SStringHasTab(StrIndent);

    StrIndent:= TabsToSpaces(ALineIndex, StrIndent);
    if DecSpaces>Length(StrIndent) then
      DecSpaces:= Length(StrIndent);
    Delete(StrIndent, 1, DecSpaces);

    if DoTabs then
      StrIndent:= SpacesToTabs(ALineIndex, StrIndent);
    Result:= StrIndent+StrText;
  end;
end;

function SRemoveAsciiControlChars(const S: atString; AReplaceChar: Widechar
  ): atString;
var
  i: integer;
begin
  Result:= S;
  if OptUnprintedReplaceSpec then
    for i:= 1 to Length(Result) do
      if IsCharAsciiControl(Result[i]) then
        Result[i]:= AReplaceChar;
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


function STrimRight(const S: atString): atString;
var
  N: integer;
begin
  N:= Length(S);
  while (N>0) and (S[N]=' ') do Dec(N);
  Result:= Copy(S, 1, N);
end;

function SBeginsWith(const S, SubStr: atString): boolean; inline;
begin
  Result:= (SubStr<>'') and (Copy(S, 1, Length(SubStr))=SubStr);
end;

function SBeginsWith(const S, SubStr: string): boolean; inline;
begin
  Result:= (SubStr<>'') and (Copy(S, 1, Length(SubStr))=SubStr);
end;

function SEndsWith(const S, SubStr: atString): boolean; inline;
begin
  Result:= (SubStr<>'') and (Length(SubStr)<=Length(S)) and
    (Copy(S, Length(S)-Length(SubStr)+1, MaxInt)=SubStr);
end;

function SEndsWith(const S, SubStr: string): boolean; inline;
begin
  Result:= (SubStr<>'') and (Length(SubStr)<=Length(S)) and
    (Copy(S, Length(S)-Length(SubStr)+1, MaxInt)=SubStr);
end;

function SEndsWithEol(const S: string): boolean; inline;
begin
  Result:= (S<>'') and IsCharEol(S[Length(S)]);
end;

function SEndsWithEol(const S: atString): boolean; inline;
begin
  Result:= (S<>'') and IsCharEol(S[Length(S)]);
end;


function SCharUpper(ch: widechar): widechar; inline;
begin
  Result:= UnicodeUpperCase(ch)[1];
end;

function SCharLower(ch: widechar): widechar; inline;
begin
  Result:= UnicodeLowerCase(ch)[1];
end;


function SCaseTitle(const S, SNonWordChars: atString): atString;
var
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    if (i=1) or not IsCharWord(S[i-1], SNonWordChars) then
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

function SCaseSentence(const S, SNonWordChars: atString): atString;
var
  dot: boolean;
  i: Integer;
begin
  Result:= S;
  dot:= True;
  for i:= 1 to Length(Result) do
  begin
    if IsCharWord(Result[i], SNonWordChars) then
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


procedure SReplaceAll(var S: string; const SFrom, STo: string); inline;
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

procedure SReplaceAllTabsToOneSpace(var S: string); inline;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if S[i]=#9 then
      S[i]:= ' ';
end;

procedure SReplaceAllTabsToOneSpace(var S: UnicodeString); inline;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if S[i]=#9 then
      S[i]:= ' ';
end;

procedure SDeleteFrom(var s: string; const SFrom: string); inline;
var
  n: integer;
begin
  n:= Pos(SFrom, S);
  if n>0 then
    SetLength(S, n-1);
end;

procedure SDeleteFrom(var s: atString; const SFrom: atString); inline;
var
  n: integer;
begin
  n:= Pos(SFrom, S);
  if n>0 then
    SetLength(S, n-1);
end;

procedure SDeleteFromEol(var s: string); inline;
begin
  SDeleteFrom(s, #10);
  SDeleteFrom(s, #13);
end;

procedure SDeleteFromEol(var s: atString); inline;
begin
  SDeleteFrom(s, #10);
  SDeleteFrom(s, #13);
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

procedure SClipboardCopy(AText: string; AClipboardObj: TClipboard=nil);
begin
  if AText='' then exit;
  if AClipboardObj=nil then
    AClipboardObj:= Clipboard;

  {$IFDEF LCLGTK2}
  //Workaround for Lazarus bug #0021453. LCL adds trailing zero to clipboard in Clipboard.AsText.
  AClipboardObj.Clear;
  AClipboardObj.AddFormat(PredefinedClipboardFormat(pcfText), AText[1], Length(AText));
  {$ELSE}
  AClipboardObj.AsText:= AText;
  {$ENDIF}
end;


//function posted by user "mse" at Laz forum
function SConvertUtf8ToWideForAscii(const S: string): UnicodeString;
var
  PStart, PEnd: PByte;
  PDest: PWord;
  NLen: integer;
begin
  NLen:= Length(S);
  SetLength(Result, NLen);
  PStart:= Pointer(S);
  PEnd:= PStart+NLen;
  PDest:= Pointer(Result);
  while PStart<PEnd do
  begin
    PDest^:= PStart^;
    Inc(PStart);
    Inc(PDest);
  end;
end;

function SFindCharCount(const S: UnicodeString; ch: WideChar): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    if S[i]=ch then
      Inc(Result);
end;


function EditorScale(AValue: integer): integer; inline;
begin
  Result:= AValue * EditorScalePercents div 100;
end;

function EditorScaleFont(AValue: integer): integer;
begin
  if EditorScaleFontPercents=0 then
    Result:= EditorScale(AValue)
  else
    Result:= AValue * EditorScaleFontPercents div 100;
end;


function SFindRegexMatch(const Subject, Regex: UnicodeString; out MatchPos, MatchLen: integer): boolean;
var
  Obj: TRegExpr;
begin
  Result:= false;
  MatchPos:= 0;
  MatchLen:= 0;

  Obj:= TRegExpr.Create;
  try
    Obj.ModifierS:= false;
    Obj.ModifierM:= true;
    Obj.ModifierI:= false;
    Obj.Expression:= Regex;

    if Obj.Exec(Subject) then
    begin
      Result:= true;
      MatchPos:= Obj.MatchPos[0];
      MatchLen:= Obj.MatchLen[0];
    end;
  finally
    FreeAndNil(Obj);
  end;
end;


function SCountTextOccurrences(const SubStr, Str: UnicodeString): integer;
var
  Offset: integer;
begin
  Result:= 0;
  if (Str='') or (SubStr='') then exit;
  Offset:= PosEx(SubStr, Str, 1);
  while Offset<>0 do
  begin
    Inc(Result);
    Offset:= PosEx(SubStr, Str, Offset + Length(SubStr));
  end;
end;

function SCountTextLines(const Str, StrBreak: UnicodeString): integer;
begin
  Result:= SCountTextOccurrences(StrBreak, Str)+1;
  // ignore trailing EOL
  if Length(Str)>=Length(StrBreak) then
    if Copy(Str, Length(Str)-Length(StrBreak)+1, Length(StrBreak))=StrBreak then
      Dec(Result);
end;

procedure SSplitByChar(const S: string; Sep: char; out S1, S2: string);
var
  N: integer;
begin
  N:= Pos(Sep, S);
  if N=0 then
  begin
    S1:= S;
    S2:= '';
  end
  else
  begin
    S1:= Copy(S, 1, N-1);
    S2:= Copy(S, N+1, Length(S));
  end;
end;

{ TATStringSeparator }

procedure TATStringSeparator.Init(const AStr: string; ASep: char);
begin
  FStr:= AStr;
  FSep:= ASep;
  FPos:= 1;
end;

function TATStringSeparator.GetItemStr(out AValue: string): boolean;
var
  N: integer;
begin
  if FPos>Length(FStr) then
  begin
    AValue:= '';
    exit(false);
  end;
  N:= PosEx(FSep, FStr, FPos);
  if N=0 then
    N:= Length(FStr)+1;
  AValue:= Copy(FStr, FPos, N-FPos);
  FPos:= N+1;
  Result:= true;
end;

function TATStringSeparator.GetItemStr(out AValue: UnicodeString): boolean;
var
  SVal: string;
begin
  Result:= GetItemStr(SVal);
  AValue:= SVal;
end;

function TATStringSeparator.GetItemInt(out AValue: integer; const ADefault: integer): boolean;
var
  SVal: string;
begin
  Result:= GetItemStr(SVal);
  if Result then
    AValue:= StrToIntDef(SVal, ADefault)
  else
    AValue:= ADefault;
end;

procedure TestStringSeparator;
var
  sep: TATStringSeparator;
  res, item: string;
begin
  sep.init(',,1,,,4', ',');
  res:= '';
  while sep.GetItemStr(item) do
    res+= item+','#10;
  ShowMessage(res);
end;


end.

