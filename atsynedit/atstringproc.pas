{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, StrUtils,
  LCLType, LCLIntf, Clipbrd,
  ATSynEdit_UnicodeData,
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
  TATInt64Array = array of Int64;

const
  //must be >= OptMaxLineLenForAccurateCharWidths
  cMaxFixedArray = 1024;

type
  TATIntFixedArray = record
    Data: packed array[0..cMaxFixedArray-1] of integer; //'word' is too small for CalcCharOffsets
    Len: integer;
  end;

type
  TATSimpleRange = record NFrom, NTo: integer; end;
  TATSimpleRangeArray = array of TATSimpleRange;

function IsStringWithUnicode(const S: string): boolean; inline;
function IsStringWithUnicode(const S: UnicodeString): boolean; inline;

function SCharUpper(ch: WideChar): WideChar; inline;
function SCharLower(ch: WideChar): WideChar; inline;

function SCaseTitle(const S, SNonWordChars: atString): atString;
function SCaseInvert(const S: atString): atString;
function SCaseSentence(const S, SNonWordChars: atString): atString;

function StringOfCharW(ch: WideChar; Len: integer): UnicodeString;

{$Z1}
type
  TATLineEnds = (cEndNone, cEndWin, cEndUnix, cEndMac);

  TATLineState = (
    cLineStateNone,
    cLineStateChanged,
    cLineStateAdded,
    cLineStateSaved
    );

const
  cLineEndStrings: array[TATLineEnds] of UnicodeString = ('', #13#10, #10, #13);
  cLineEndNiceNames: array[TATLineEnds] of string = ('', 'CRLF', 'LF', 'CR');
  cLineEndLength: array[TATLineEnds] of integer = (0, 2, 1, 1);

const
  BoolToPlusMinusOne: array[boolean] of integer = (-1, 1);

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
  OptMaxLineLenForAccurateCharWidths: integer = 500;

type
  TATStringTabCalcEvent = function(Sender: TObject; ALineIndex, ACharIndex: integer): integer of object;
  TATStringGetLenEvent = function(ALineIndex: integer): integer of object;

type

  { TATStringTabHelper }

  TATStringTabHelper = class
  private
    //these arrays are local vars, placed here to alloc 2*4Kb not in stack
    ListEnds: TATIntFixedArray;
    ListMid: TATIntFixedArray;
  public
    TabSpaces: boolean;
    TabSize: integer;
    IndentSize: integer;
    SenderObj: TObject;
    OnCalcTabSize: TATStringTabCalcEvent;
    OnCalcLineLen: TATStringGetLenEvent;
    function CalcTabulationSize(ALineIndex, APos: integer): integer;
    function TabsToSpaces(ALineIndex: integer; const S: atString): atString;
    function TabsToSpaces_Length(ALineIndex: integer; const S: atString; AMaxLen: integer): integer;
    function SpacesToTabs(ALineIndex: integer; const S: atString): atString;
    function GetIndentExpanded(ALineIndex: integer; const S: atString): integer;
    function CharPosToColumnPos(ALineIndex: integer; const S: atString; APos: integer): integer;
    function ColumnPosToCharPos(ALineIndex: integer; const S: atString; AColumn: integer): integer;
    function IndentUnindent(ALineIndex: integer; const Str: atString; ARight: boolean): atString;
    procedure CalcCharOffsets(ALineIndex: integer; const S: atString; var AInfo: TATIntFixedArray; ACharsSkipped: integer = 0);
    function CalcCharOffsetLast(ALineIndex: integer; const S: atString; ACharsSkipped: integer = 0): integer;
    function FindWordWrapOffset(ALineIndex: integer; const S: atString; AColumns: integer;
      const ANonWordChars: atString; AWrapIndented: boolean): integer;
    function FindClickedPosition(ALineIndex: integer; const Str: atString;
      constref ListOffsets: TATIntFixedArray;
      APixelsFromLeft, ACharSize: integer;
      AAllowVirtualPos: boolean;
      out AEndOfLinePos: boolean): integer;
    procedure FindOutputSkipOffset(ALineIndex: integer; const S: atString; AScrollPos: integer;
      out ACharsSkipped: integer; out ACellPercentsSkipped: integer);
  end;

function IsCharEol(ch: widechar): boolean; inline;
function IsCharEol(ch: char): boolean; inline;
function IsCharWord(ch: widechar; const ANonWordChars: UnicodeString): boolean;
function IsCharWordA(ch: char): boolean; inline;
function IsCharWordInIdentifier(ch: widechar): boolean;
function IsCharDigit(ch: widechar): boolean; inline;
function IsCharDigit(ch: char): boolean; inline;
function IsCharSpace(ch: widechar): boolean; inline;
function IsCharSpace(ch: char): boolean; inline;
function IsCharSymbol(ch: widechar): boolean;
function IsCharHexDigit(ch: widechar): boolean; inline;
function IsCharHexDigit(ch: char): boolean; inline;
function HexDigitToInt(ch: char): integer;

function IsCharSurrogateAny(ch: widechar): boolean; inline;
function IsCharSurrogateHigh(ch: widechar): boolean; inline;
function IsCharSurrogateLow(ch: widechar): boolean; inline;

function IsStringSpaces(const S: atString): boolean; inline;
function IsStringSpaces(const S: atString; AFrom, ALen: integer): boolean;

function SBeginsWith(const S, SubStr: UnicodeString): boolean;
function SBeginsWith(const S, SubStr: string): boolean;
function SBeginsWith(const S: UnicodeString; ch: WideChar): boolean; inline;
function SBeginsWith(const S: string; ch: char): boolean; inline;
function SEndsWith(const S, SubStr: UnicodeString): boolean;
function SEndsWith(const S, SubStr: string): boolean;
function SEndsWith(const S: UnicodeString; ch: WideChar): boolean; inline;
function SEndsWith(const S: string; ch: char): boolean; inline;
function SEndsWithEol(const S: string): boolean; inline;
function SEndsWithEol(const S: atString): boolean; inline;

function STrimRight(const S: atString): atString;
function SGetIndentChars(const S: atString): integer;
function SGetIndentCharsToOpeningBracket(const S: atString): integer;
function SGetTrailingSpaceChars(const S: atString): integer;
function SGetNonSpaceLength(const S: atString): integer;

function SStringHasEol(const S: atString): boolean; inline;
function SStringHasEol(const S: string): boolean; inline;
function SStringHasTab(const S: atString): boolean; inline;
function SStringHasTab(const S: string): boolean; inline;
//function SStringHasAsciiAndNoTabs(const S: atString): boolean;
//function SStringHasAsciiAndNoTabs(const S: string): boolean;

function SRemoveNewlineChars(const S: atString): atString;

function SGetItem(var S: string; const ch: Char = ','): string;
procedure SSwapEndianWide(var S: UnicodeString);
procedure SSwapEndianUCS4(var S: UCS4String); inline;
procedure SAddStringToHistory(const S: string; List: TStrings; MaxItems: integer);

procedure TrimStringList(L: TStringList); inline;

const
  cDefaultNonWordChars: UnicodeString = '-+*=/\()[]{}<>"''.,:;~?!@#$%^&|`…';

type
  TATDecodeRec = record SFrom, STo: UnicodeString; end;
function SDecodeRecords(const S: UnicodeString; const Decode: array of TATDecodeRec): UnicodeString;

procedure SReplaceAll(var S: string; const SFrom, STo: string); inline;
procedure SReplaceAllPercentChars(var S: string);
procedure SDeleteFrom(var s: string; const SFrom: string); inline;
procedure SDeleteFrom(var s: UnicodeString; const SFrom: UnicodeString); inline;
procedure SDeleteFromEol(var S: string);
procedure SDeleteFromEol(var S: UnicodeString);

procedure SClipboardCopy(AText: string; AClipboardObj: TClipboard=nil);
function SFindCharCount(const S: string; ch: char): integer;
function SFindCharCount(const S: UnicodeString; ch: WideChar): integer;
function SFindRegexMatch(const Subject, Regex: UnicodeString; out MatchPos, MatchLen: integer): boolean;
function SFindRegexMatch(const Subject, Regex: UnicodeString; GroupIndex: integer; ModS, ModI, ModM: boolean): UnicodeString;
function SCountTextOccurrences(const SubStr, Str: UnicodeString): integer;
function SCountTextLines(const Str, StrBreak: UnicodeString): integer;
procedure SSplitByChar(const S: string; Sep: char; out S1, S2: string);


implementation

uses
  Dialogs, Math;

function IsCharEol(ch: widechar): boolean;
begin
  Result:= (ch=#10) or (ch=#13);
end;

function IsCharEol(ch: char): boolean;
begin
  Result:= (ch=#10) or (ch=#13);
end;

function IsCharWord(ch: widechar; const ANonWordChars: UnicodeString): boolean;
begin
  //to make '_' non-word char, specify it as _first_ in ANonWordChars
  if ch='_' then
    exit( (ANonWordChars='') or (ANonWordChars[1]<>'_') );

  //if it's unicode letter, return true, ignore option string
  //if it's not letter, check for option (maybe char '$' is present there for PHP lexer)
  // bit 7 in value: is word char
  Result := CharCategoryArray[Ord(ch)] and 128 <> 0;
  if not Result then
  begin
    if IsCharUnicodeSpace(ch) then
      exit(false);

    if Pos(ch, ANonWordChars)=0 then
      exit(true);
  end;
end;

function IsCharWordA(ch: char): boolean;
begin
  // bit 7 in value: is word char
  Result := CharCategoryArray[Ord(ch)] and 128 <> 0;
end;


function IsCharWordInIdentifier(ch: widechar): boolean;
begin
  if Ord(ch)>Ord('z') then
    exit(false);
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

function IsCharDigit(ch: widechar): boolean;
begin
  Result:= (ch>='0') and (ch<='9');
end;

function IsCharDigit(ch: char): boolean;
begin
  Result:= (ch>='0') and (ch<='9');
end;

function IsCharHexDigit(ch: widechar): boolean;
begin
  case ch of
    '0'..'9',
    'a'..'f',
    'A'..'F':
      Result:= true
    else
      Result:= false;
  end;
end;

function IsCharHexDigit(ch: char): boolean;
begin
  case ch of
    '0'..'9',
    'a'..'f',
    'A'..'F':
      Result:= true
    else
      Result:= false;
  end;
end;

function IsCharSpace(ch: widechar): boolean;
begin
  Result:= IsCharUnicodeSpace(ch);
end;

function IsCharSpace(ch: char): boolean;
begin
  case ch of
    #9, //tab
    ' ', //space
    #$A0: //no-break space, NBSP, often used on macOS
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsCharSymbol(ch: widechar): boolean;
begin
  Result:= Pos(ch, '.,;:''"/\-+*=()[]{}<>?!@#$%^&|~`')>0;
end;

function IsCharSurrogateAny(ch: widechar): boolean;
begin
  Result:= (ch>=#$D800) and (ch<=#$DFFF);
end;

function IsCharSurrogateHigh(ch: widechar): boolean;
begin
  Result:= (ch>=#$D800) and (ch<=#$DBFF);
end;

function IsCharSurrogateLow(ch: widechar): boolean;
begin
  Result:= (ch>=#$DC00) and (ch<=#$DFFF);
end;


function IsStringSpaces(const S: atString): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if not IsCharSpace(S[i]) then
      exit(false);
  Result:= true;
end;

function IsStringSpaces(const S: atString; AFrom, ALen: integer): boolean;
var
  i: integer;
begin
  for i:= AFrom to Min(AFrom+ALen-1, Length(S)) do
    if not IsCharSpace(S[i]) then
      exit(false);
  Result:= true;
end;

{
function SStringHasUnicodeChars(const S: atString): boolean;
var
  i, N: integer;
begin
  for i:= 1 to Length(S) do
  begin
    N:= Ord(S[i]);
    if (N<32) or (N>126) then exit(true);
  end;
  Result:= false;
end;
}

procedure DoDebugOffsets(const Info: TATIntFixedArray);
var
  i: integer;
  s: string;
begin
  s:= '';
  for i:= 0 to Info.Len-1 do
    s:= s+IntToStr(Info.Data[i])+'% ';
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
  Offsets: TATIntFixedArray;
begin
  if S='' then
    Exit(0);
  if AColumns<OptMinWordWrapOffset then
    Exit(AColumns);

  CalcCharOffsets(ALineIndex, S, Offsets);

  if Offsets.Data[Offsets.Len-1]<=AColumns*100 then
    Exit(Length(S));

  //NAvg is average wrap offset, we use it if no correct offset found
  N:= Min(Length(S), cMaxFixedArray)-1;
  while (N>0) and (Offsets.Data[N]>(AColumns+1)*100) do Dec(N);
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

procedure SSwapEndianWide(var S: UnicodeString);
var
  i: integer;
  P: PWord;
begin
  if S='' then exit;
  UniqueString(S);
  for i:= 1 to Length(S) do
  begin
    P:= @S[i];
    P^:= SwapEndian(P^);
  end;
end;

procedure SSwapEndianUCS4(var S: UCS4String);
var
  i: integer;
begin
  for i:= 0 to Length(S)-1 do
    S[i]:= SwapEndian(S[i]);
end;

function TATStringTabHelper.CalcTabulationSize(ALineIndex, APos: integer): integer;
begin
  if Assigned(OnCalcTabSize) then
    Result:= OnCalcTabSize(SenderObj, ALineIndex, APos)
  else
  if Assigned(OnCalcLineLen) and (OnCalcLineLen(ALineIndex)>OptMaxLineLenForAccurateCharWidths) then
    Result:= 1
  else
  if APos>OptMaxTabPositionToExpand then
    Result:= 1
  else
    Result:= TabSize - (APos-1) mod TabSize;
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
      Insert(StringOfCharW(' ', NSize-1), Result, N);
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
  var AInfo: TATIntFixedArray; ACharsSkipped: integer);
var
  NLen, NSize, NTabSize, NCharsSkipped: integer;
  NScalePercents: integer;
  //NPairSize: integer;
  //StrPair: WideString;
  ch: widechar;
  i: integer;
begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  NLen:= Min(Length(S), cMaxFixedArray);
  AInfo.Len:= NLen;
  if NLen=0 then Exit;

  NCharsSkipped:= ACharsSkipped;
  //NPairSize:= 0;
  //StrPair:= 'ab';

  if NLen>OptMaxLineLenForAccurateCharWidths then
  begin
    for i:= 0 to NLen-1 do
      AInfo.Data[i]:= 100*(i+1);
    exit;
  end;

  for i:= 1 to NLen do
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

    if i=1 then
      AInfo.Data[i-1]:= NSize*NScalePercents
    else
      AInfo.Data[i-1]:= AInfo.Data[i-2]+NSize*NScalePercents;
  end;
end;

function TATStringTabHelper.CalcCharOffsetLast(ALineIndex: integer; const S: atString;
  ACharsSkipped: integer): integer;
var
  NLen, NSize, NTabSize, NCharsSkipped: integer;
  NScalePercents: integer;
  ch: WideChar;
  i: integer;
begin
  Result:= 0;
  NLen:= Length(S);
  if NLen=0 then Exit;

  if NLen>OptMaxLineLenForAccurateCharWidths then
    exit(NLen*100);

  NCharsSkipped:= ACharsSkipped;

  for i:= 1 to NLen do
  begin
    ch:= S[i];
    Inc(NCharsSkipped);

    if IsCharSurrogateAny(ch) then
    begin
      NScalePercents:= OptEmojiWidthPercents div 2;
    end
    else
    begin
      NScalePercents:= GlobalCharSizer.GetCharWidth(ch);
    end;

    if ch<>#9 then
      NSize:= 1
    else
    begin
      NTabSize:= CalcTabulationSize(ALineIndex, NCharsSkipped);
      NSize:= NTabSize;
      Inc(NCharsSkipped, NTabSize-1);
    end;

    Inc(Result, NSize*NScalePercents);
  end;
end;


function TATStringTabHelper.FindClickedPosition(ALineIndex: integer; const Str: atString;
  constref ListOffsets: TATIntFixedArray;
  APixelsFromLeft, ACharSize: integer; AAllowVirtualPos: boolean; out AEndOfLinePos: boolean): integer;
var
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

  ListEnds.Len:= ListOffsets.Len;
  ListMid.Len:= ListOffsets.Len;

  //positions of each char end
  for i:= 0 to ListOffsets.Len-1 do
    ListEnds.Data[i]:= ListOffsets.Data[i]*ACharSize div 100;

  //positions of each char middle
  for i:= 0 to ListOffsets.Len-1 do
    if i=0 then
      ListMid.Data[i]:= ListEnds.Data[i] div 2
    else
      ListMid.Data[i]:= (ListEnds.Data[i-1]+ListEnds.Data[i]) div 2;

  for i:= 0 to ListOffsets.Len-1 do
    if APixelsFromLeft<ListMid.Data[i] then
    begin
      Result:= i+1;

      //don't get position inside utf16 surrogate pair
      if (Result<=Length(Str)) and IsCharSurrogateLow(Str[Result]) then
        Inc(Result);

      Exit
    end;

  AEndOfLinePos:= true;
  if AAllowVirtualPos then
    Result:= Length(Str)+1 + (APixelsFromLeft - ListEnds.Data[ListEnds.Len-1]) div ACharSize
  else
    Result:= Length(Str)+1;
end;

procedure TATStringTabHelper.FindOutputSkipOffset(ALineIndex: integer; const S: atString;
  AScrollPos: integer; out ACharsSkipped: integer; out ACellPercentsSkipped: integer);
var
  Offsets: TATIntFixedArray;
begin
  ACharsSkipped:= 0;
  ACellPercentsSkipped:= 0;
  if (S='') or (AScrollPos=0) then Exit;

  CalcCharOffsets(ALineIndex, S, Offsets);

  while (ACharsSkipped<Offsets.Len) and
    (Offsets.Data[ACharsSkipped] < AScrollPos*100) do
    Inc(ACharsSkipped);

  if (ACharsSkipped>0) then
    ACellPercentsSkipped:= Offsets.Data[ACharsSkipped-1];
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


procedure TrimStringList(L: TStringList);
begin
  //dont do "while", we need correct last empty lines
  if (L.Count>0) and (L[L.Count-1]='') then
    L.Delete(L.Count-1);
end;

function SStringHasEol(const S: atString): boolean;
begin
  Result:=
    (Pos(#10, S)>0) or
    (Pos(#13, S)>0);
end;

function SStringHasEol(const S: string): boolean;
begin
  Result:=
    (Pos(#10, S)>0) or
    (Pos(#13, S)>0);
end;

function TATStringTabHelper.SpacesToTabs(ALineIndex: integer; const S: atString): atString;
begin
  Result:= StringReplace(S, StringOfCharW(' ', TabSize), WideChar(9), [rfReplaceAll]);
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

function SStringHasTab(const S: atString): boolean;
begin
  Result:= Pos(#9, S)>0;
end;

function SStringHasTab(const S: string): boolean;
begin
  Result:= Pos(#9, S)>0;
end;


(*
function SStringHasAsciiAndNoTabs(const S: atString): boolean;
var
  code, i: integer;
begin
  for i:= 1 to Length(S) do
  begin
    code:= Ord(S[i]);
    if (code<32) or (code>=127) then
      exit(false);
  end;
  Result:= true;
end;

function SStringHasAsciiAndNoTabs(const S: string): boolean;
var
  code, i: integer;
begin
  for i:= 1 to Length(S) do
  begin
    code:= Ord(S[i]);
    if (code<32) or (code>=127) then
      exit(false);
  end;
  Result:= true;
end;
*)

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
      StrIndent:= StringOfCharW(' ', TabSize)
    else
      StrIndent:= #9;
    DecSpaces:= TabSize;
  end
  else
  if IndentSize>0 then
  begin
    //use spaces
    StrIndent:= StringOfCharW(' ', IndentSize);
    DecSpaces:= IndentSize;
  end
  else
  begin
    //indent<0 - use tabs
    StrIndent:= StringOfCharW(#9, Abs(IndentSize));
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

function SBeginsWith(const S, SubStr: UnicodeString): boolean;
var
  i: integer;
begin
  Result:= false;
  if S='' then exit;
  if SubStr='' then exit;
  if Length(SubStr)>Length(S) then exit;
  for i:= 1 to Length(SubStr) do
    if S[i]<>SubStr[i] then exit;
  Result:= true;
end;

function SBeginsWith(const S, SubStr: string): boolean;
var
  i: integer;
begin
  Result:= false;
  if S='' then exit;
  if SubStr='' then exit;
  if Length(SubStr)>Length(S) then exit;
  for i:= 1 to Length(SubStr) do
    if S[i]<>SubStr[i] then exit;
  Result:= true;
end;

function SEndsWith(const S, SubStr: UnicodeString): boolean;
var
  i, Offset: integer;
begin
  Result:= false;
  if S='' then exit;
  if SubStr='' then exit;
  Offset:= Length(S)-Length(SubStr);
  if Offset<0 then exit;
  for i:= 1 to Length(SubStr) do
    if S[i+Offset]<>SubStr[i] then exit;
  Result:= true;
end;

function SEndsWith(const S, SubStr: string): boolean;
var
  i, Offset: integer;
begin
  Result:= false;
  if S='' then exit;
  if SubStr='' then exit;
  Offset:= Length(S)-Length(SubStr);
  if Offset<0 then exit;
  for i:= 1 to Length(SubStr) do
    if S[i+Offset]<>SubStr[i] then exit;
  Result:= true;
end;

function SBeginsWith(const S: UnicodeString; ch: WideChar): boolean;
begin
  Result:= (S<>'') and (S[1]=ch);
end;

function SBeginsWith(const S: string; ch: char): boolean;
begin
  Result:= (S<>'') and (S[1]=ch);
end;

function SEndsWith(const S: UnicodeString; ch: WideChar): boolean;
begin
  Result:= (S<>'') and (S[Length(S)]=ch);
end;

function SEndsWith(const S: string; ch: char): boolean;
begin
  Result:= (S<>'') and (S[Length(S)]=ch);
end;

function SEndsWithEol(const S: string): boolean;
begin
  Result:= (S<>'') and IsCharEol(S[Length(S)]);
end;

function SEndsWithEol(const S: atString): boolean;
begin
  Result:= (S<>'') and IsCharEol(S[Length(S)]);
end;

function SCharUpper(ch: WideChar): WideChar;
begin
  Result := CharUpperArray[Ord(Ch)];
end;

function SCharLower(ch: WideChar): WideChar;
begin
  Result := CharLowerArray[Ord(Ch)];
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
  ch, ch_up: WideChar;
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
  begin
    ch:= Result[i];
    ch_up:= SCharUpper(ch);
    if ch<>ch_up then
      Result[i]:= ch_up
    else
      Result[i]:= SCharLower(ch);
  end;
end;

function SCaseSentence(const S, SNonWordChars: atString): atString;
var
  dot: boolean;
  ch: WideChar;
  i: Integer;
begin
  Result:= S;
  dot:= True;
  for i:= 1 to Length(Result) do
  begin
    ch:= Result[i];
    if IsCharWord(ch, SNonWordChars) then
    begin
      if dot then
        Result[i]:= SCharUpper(ch)
      else
        Result[i]:= SCharLower(ch);
      dot:= False;
    end
    else
      if (ch = '.') or (ch = '!') or (ch = '?') then
        dot:= True;
  end;
end;

function StringOfCharW(ch: WideChar; Len: integer): UnicodeString;
var
  i: integer;
begin
  SetLength(Result, Len);
  for i:= 1 to Len do
    Result[i]:= ch;
end;


function SDecodeRecords(const S: UnicodeString; const Decode: array of TATDecodeRec): UnicodeString;
var
  DoDecode: Boolean;
  i, iPart: integer;
begin
  Result := '';
  i := 1;
  repeat
    if i > Length(S) then Break;
    DoDecode := False;
    for iPart := Low(Decode) to High(Decode) do
      with Decode[iPart] do
        if strlcomp(PChar(SFrom), @S[i], Length(SFrom)) = 0 then
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


procedure SReplaceAll(var S: string; const SFrom, STo: string);
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
    SetLength(S, n-1);
end;

procedure SDeleteFrom(var s: UnicodeString; const SFrom: UnicodeString);
var
  n: integer;
begin
  n:= Pos(SFrom, S);
  if n>0 then
    SetLength(S, n-1);
end;

procedure SDeleteFromEol(var S: string);
var
  i: integer;
  ch: char;
begin
  for i:= 1 to Length(S) do
  begin
    ch:= S[i];
    if (ch=#10) or (ch=#13) then
    begin
      SetLength(S, i-1);
      Exit;
    end;
  end;
end;

procedure SDeleteFromEol(var S: UnicodeString);
var
  i: integer;
  ch: WideChar;
begin
  for i:= 1 to Length(S) do
  begin
    ch:= S[i];
    if (ch=#10) or (ch=#13) then
    begin
      SetLength(S, i-1);
      Exit;
    end;
  end;
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


function SFindCharCount(const S: string; ch: char): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    if S[i]=ch then
      Inc(Result);
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


function EditorScale(AValue: integer): integer;
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

function SFindRegexMatch(const Subject, Regex: UnicodeString; GroupIndex: integer; ModS, ModI, ModM: boolean): UnicodeString;
var
  Obj: TRegExpr;
begin
  Result:= '';
  Obj:= TRegExpr.Create;
  try
    Obj.ModifierS:= ModS;
    Obj.ModifierM:= ModM;
    Obj.ModifierI:= ModI;
    Obj.Expression:= Regex;
    if Obj.Exec(Subject) then
      Result:= Obj.Match[GroupIndex];
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

function IsStringWithUnicode(const S: string): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if Ord(S[i])>=128 then
      exit(true);
  Result:= false;
end;

function IsStringWithUnicode(const S: UnicodeString): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if Ord(S[i])>=128 then
      exit(true);
  Result:= false;
end;

function HexDigitToInt(ch: char): integer;
begin
  case ch of
    '0'..'9':
      Result:= Ord(ch)-Ord('0');
    'a'..'f':
      Result:= Ord(ch)-Ord('a')+10;
    'A'..'F':
      Result:= Ord(ch)-Ord('A')+10;
    else
      Result:= 0;
  end;
end;

end.

