{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc;

{$mode objfpc}{$H+}
{$codepage utf8}
{$ScopedEnums on}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, StrUtils,
  LCLType, LCLIntf, Clipbrd,
  ATSynEdit_Globals,
  ATSynEdit_UnicodeData,
  ATSynEdit_RegExpr,
  ATSynEdit_CharSizer;

type
  atString = UnicodeString;
  atChar = WideChar;
  PatChar = PWideChar;

type

  { TATPoint }

  TATPoint = record
    X, Y: Int64;
    class operator =(const A, B: TATPoint): boolean;
  end;

type
  TATEditorCharSize = record
    //XScaled is the average-char-width (usually of 'N'),
    //multiplied by ATEditorCharXScale and truncated.
    //on Win32/Gtk2, XScaled is multiple of ATEditorCharXScale; but not on Qt5/macOS.
    //macOS has actually floating-number font width, e.g. 7.801 pixels of a single char in monospaced fonts
    //(all ASCII chars have the same width, it was tested on macOS).
    XScaled: Int64;
    //XSpacePercents is the width of space-char in percents (of average-char-width).
    //if FontProportional=False, it is 100; otherwise it's different.
    XSpacePercents: Int64;
    //line height.
    //macOS height is still an integer number.
    Y: Int64;
  end;

type
  TATLineChangeKind = (
    Edited,
    Added,
    Deleted,
    DeletedAll
    );

type
  TATIntArray = array of integer;
  TATInt64Array = array of Int64;
  TATPointPair = packed record X, Y, X2, Y2: integer; end;
  TATPointPairArray = array of TATPointPair;

type
  TATMarkerMarkerRecord = record
    Tag: integer;
    TagEx: integer;
    PosX: integer;
    PosY: integer;
    SelX: integer;
    SelY: integer;
    MicromapMode: integer;
  end;
  TATMarkerMarkerArray = array of TATMarkerMarkerRecord;

type
  TATMarkerAttribRecord = record
    Tag: integer;
    TagEx: integer;
    PosX: integer;
    PosY: integer;
    SelX: integer;
    ColorFont: integer;
    ColorBG: integer;
    ColorBorder: integer;
    FontStyles: integer;
    BorderLeft: integer;
    BorderRight: integer;
    BorderUp: integer;
    BorderDown: integer;
    MicromapMode: integer;
  end;
  TATMarkerAttribArray = array of TATMarkerAttribRecord;

const
  //TATEditorCharSize uses this constant.
  //must be some big value from e.g. 100 to e.g. 10K.
  ATEditorCharXScale = 1024;

  //if line is longer - all line chars will be rendered in normal-width (100%) cells
  //so all CJK chars (full width 200%) will overlap by design
  ATEditorMaxFixedArray = 4*1024;

type
  //must be with Int64 items, 32-bit is not enough for single line with len>40M
  TATIntFixedArray = record
    Data: packed array[0..ATEditorMaxFixedArray-1] of Int64;
    Len: SizeInt;
  end;

  //must be with Longint items, it's for Dx offsets for rendering
  TATInt32FixedArray = record
    Data: packed array[0..ATEditorMaxFixedArray-1] of Longint;
    Len: SizeInt;
  end;

type
  TATSimpleRange = record NFrom, NTo: integer; end;
  TATSimpleRangeArray = array of TATSimpleRange;

function ATPoint(const X, Y: Int64): TATPoint;
function ATPointInRect(const Rect: TRect; const P: TATPoint): boolean;

function IsStringWithUnicode(const S: string): boolean; inline;
function IsStringWithUnicode(const S: UnicodeString): boolean; inline;

function SCharUpper(ch: WideChar): WideChar; inline;
function SCharLower(ch: WideChar): WideChar; inline;

function SConvertCaseToTitle(const S, SNonWordChars: UnicodeString): UnicodeString;
function SConvertCaseToInverted(const S: UnicodeString): UnicodeString;
function SConvertCaseToSentence(const S, SNonWordChars: UnicodeString): UnicodeString;
function SConvertCaseToUpper(const S: UnicodeString): UnicodeString;
function SConvertCaseToLower(const S: UnicodeString): UnicodeString;

function StringOfCharW(ch: WideChar; Len: SizeInt): UnicodeString;

{$Z1}
type
  TATLineEnds = (None, Windows, Unix, Mac);

  TATLineState = (None, Changed, Added, Saved);

const
  cLineEndOsDefault = {$ifdef windows} TATLineEnds.Windows {$else} TATLineEnds.Unix {$endif};
  cLineEndLength: array[TATLineEnds] of integer = (0, 2, 1, 1);

const
  BoolToPlusMinusOne: array[boolean] of integer = (-1, 1);

type
  TATStringTabCalcEvent = function(Sender: TObject; ALineIndex, ACharIndex: SizeInt): SizeInt of object;
  TATStringGetLenEvent = function(ALineIndex: SizeInt): SizeInt of object;

type

  { TATStringTabHelper }

  TATStringTabHelper = class
  private
    //these arrays are local vars, placed here to alloc 2*4Kb not in stack
    ListEnds: TATIntFixedArray;
    ListMid: TATIntFixedArray;
  public
    CharSizer: TATCharSizer;
    TabSpaces: boolean;
    TabSize: integer;
    IndentSize: integer;
    CharSize: TATEditorCharSize;
    FontProportional: boolean;
    SenderObj: TObject;
    OnCalcTabSize: TATStringTabCalcEvent;
    OnCalcLineLen: TATStringGetLenEvent;
    constructor Create(ACharSizer: TATCharSizer);
    function CalcTabulationSize(ALineIndex, APos: SizeInt): integer;
    function TabsToSpaces(ALineIndex: integer; const S: atString): atString;
    function TabsToSpaces_Length(ALineIndex: integer; const S: atString; AMaxLen: SizeInt): SizeInt;
    function SpacesToTabs(ALineIndex: integer; const S: atString): atString;
    function GetIndentExpanded(ALineIndex: integer; const S: atString): integer;
    //2026.09 (CudaText perf): PWideChar-based variants for the word-wrap calc:
    //they work directly on a raw char buffer (stack buffer of one line part),
    //so no UnicodeString is allocated/copied per wrapped line part;
    //behavior is identical to the atString-based versions
    function GetIndentExpandedBuf(ALineIndex: integer; P: PatChar; ALen: SizeInt): integer;
    function CharPosToColumnPos(ALineIndex: integer; const S: atString; APos: SizeInt): integer;
    function ColumnPosToCharPos(ALineIndex: integer; const S: atString; AColumn: SizeInt): integer;
    function IndentUnindent(ALineIndex: integer; const Str: atString; ARight: boolean): atString;
    procedure CalcCharOffsets(ALineIndex: integer; const S: atString;
      out AInfo: TATIntFixedArray; ACharsSkipped: SizeInt=0);
    procedure CalcCharOffsetsBuf(ALineIndex: integer; P: PatChar; ALen: SizeInt;
      out AInfo: TATIntFixedArray; ACharsSkipped: SizeInt=0);
    function CalcCharOffsetLast(ALineIndex: integer; const S: atString; ACharsSkipped: SizeInt=0): Int64;
    function FindWordWrapOffset(ALineIndex: integer; const S: atString; AColumns: Int64;
      const ANonWordChars: atString; AWrapIndented: boolean): integer;
    function FindWordWrapOffsetBuf(ALineIndex: integer; P: PatChar; ALen: SizeInt; AColumns: Int64;
      const ANonWordChars: atString; AWrapIndented: boolean): integer;
    //2026.09.12 (CudaText perf): raw-ASCII fast path of FindWordWrapOffsetBuf:
    //P points to the raw ANSI bytes of an ASCII-stored line part (TATStrings
    //items with Ex.Wide=false never contain bytes >=128), no WideChar
    //conversion buffer is needed. Returns the same value as
    //FindWordWrapOffsetBuf would return for the zero-extended buffer, or -1
    //when the part is not pure printable ASCII (tab/control/8-bit byte):
    //caller must then use the PWideChar version
    function FindWordWrapOffsetBytes(P: PByte; ALen: SizeInt; AColumns: Int64;
      const ANonWordChars: atString; AWrapIndented: boolean): integer;
    function FindClickedPosition(ALineIndex: integer;
      const Str: atString;
      const StrLen: SizeInt;
      constref ListOffsets: TATIntFixedArray;
      APixelsFromLeft: Int64;
      AAllowVirtualPos: boolean;
      out AEndOfLinePos: boolean): Int64;
    procedure FindOutputSkipOffset(ALineIndex: integer;
      const S: atString;
      const AScrollPosSmooth: Int64;
      out ACharsSkipped: Int64;
      out ACellPercentsSkipped: Int64);
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
function IsCharSurrogateAny(ch: widechar): boolean; inline;
function IsCharSurrogateHigh(ch: widechar): boolean; inline;
function IsCharSurrogateLow(ch: widechar): boolean; inline;
function IsCharCJKText(ch: widechar): boolean; inline;
function IsCharCJKPunctuation(ch: widechar): boolean; inline;

function IsStringSpaces(const S: atString): boolean; inline;
function IsStringSpaces(const S: atString; AFrom, ALen: SizeInt): boolean;

function HexDigitToInt(ch: char): integer;
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

function STrimAll(const S: UnicodeString): UnicodeString;
function STrimLeft(const S: UnicodeString): UnicodeString;
function STrimRight(const S: UnicodeString): UnicodeString;

function SGetIndentChars(const S: string): SizeInt;
function SGetIndentChars(const S: atString): SizeInt;
function SGetIndentCharsToOpeningBracket(const S: atString): SizeInt;
function SGetTrailingSpaceChars(const S: atString): SizeInt;
function SGetNonSpaceLength(const S: atString): SizeInt;

function SStringHasEol(const S: atString): boolean; inline;
function SStringHasEol(const S: string): boolean; inline;
function SStringHasTab(const S: atString): boolean; inline;
function SStringHasTab(const S: string): boolean; inline;
//function SStringHasAsciiAndNoTabs(const S: atString): boolean;
//function SStringHasAsciiAndNoTabs(const S: string): boolean;
function SStringHasCharsBadForLigatures(const S: UnicodeString): boolean;

function STrimNewlineChars(const S: UnicodeString): UnicodeString;
function SRemoveNewlineChars(const S: UnicodeString): UnicodeString;

function SGetItem(var S: string; const ch: Char = ','): string;
procedure SSwapEndianWide(var S: UnicodeString);
procedure SSwapEndianUCS4(var S: UCS4String); inline;
procedure SAddStringToHistory(const S: string; List: TStrings; MaxItems: integer);

procedure TrimStringList(L: TStringList); inline;

procedure SReplaceAll(var S: string; const SFrom, STo: string); inline;
procedure SDeleteFrom(var s: string; const SFrom: string); inline;
procedure SDeleteFrom(var s: UnicodeString; const SFrom: UnicodeString); inline;
procedure SDeleteFromEol(var S: string);
procedure SDeleteFromEol(var S: UnicodeString);

procedure SClipboardCopy(AText: string; AClipboardObj: TClipboard=nil);
function SFindCharCount(const S: string; ch: char): SizeInt;
function SFindCharCount(const S: UnicodeString; ch: WideChar): SizeInt;
function SFindRegexMatch(const Subject, Regex: UnicodeString; out MatchPos, MatchLen: SizeInt): boolean;
function SFindRegexMatch(const Subject, Regex: UnicodeString; GroupIndex: integer; ModS, ModI, ModM: boolean): UnicodeString;
function SCountTextOccurrences(const SubStr, Str: UnicodeString): SizeInt;
function SCountTextLines(const Str, StrBreak: UnicodeString): SizeInt;
procedure SSplitByChar(const S: string; Sep: char; out S1, S2: string);
procedure SDeleteAndInsert(var AStr: UnicodeString; AFromPos, ACount: SizeInt; const AReplace: UnicodeString);
procedure SDeleteHtmlTags(var S: string);
procedure SFixGreekTextAfterCaseConversion(var S: UnicodeString; ALastCharIsWordEdge: boolean);

function SCalcHashQword(const S: string): QWord;


implementation

uses
  Dialogs, Math,
  ATSynEdit_CharSizeArray;

function ATPoint(const X, Y: Int64): TATPoint;
begin
  Result.X:= X;
  Result.Y:= Y;
end;

function ATPointInRect(const Rect: TRect; const P: TATPoint): boolean;
begin
  Result:= (p.y>=Rect.Top) and
           (p.y<Rect.Bottom) and
           (p.x>=Rect.Left) and
           (p.x<Rect.Right);
end;

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
    if Ord(ch)<=32 then
      exit(false);

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

function IsCharSpace(ch: widechar): boolean; inline;
begin
  //2026.09.11 (CudaText perf): inlined body of IsCharUnicodeSpace() (same
  //FixedSizes lookup, no cross-unit call): this function is called per char
  //in the word-wrap scan of SGetIndentCharsBuf()/FindWordWrapOffsetBuf()
  Result:= FixedSizes[Ord(ch)]=uw_space;
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

function IsCharSurrogateAny(ch: widechar): boolean; inline;
begin
  Result:= (ch>=#$D800) and (ch<=#$DFFF);
end;

function IsCharSurrogateHigh(ch: widechar): boolean;
begin
  Result:= (ch>=#$D800) and (ch<=#$DBFF);
end;

function IsCharSurrogateLow(ch: widechar): boolean; inline;
begin
  Result:= (ch>=#$DC00) and (ch<=#$DFFF);
end;


function IsStringSpaces(const S: atString): boolean;
var
  i: SizeInt;
begin
  for i:= 1 to Length(S) do
    if not IsCharSpace(S[i]) then
      exit(false);
  Result:= true;
end;

function IsStringSpaces(const S: atString; AFrom, ALen: SizeInt): boolean;
var
  i: SizeInt;
begin
  for i:= AFrom to Min(AFrom+ALen-1, Length(S)) do
    if not IsCharSpace(S[i]) then
      exit(false);
  Result:= true;
end;

{
function SStringHasUnicodeChars(const S: atString): boolean;
var
  i, N: SizeInt;
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

{
    Blocks Containing Han Ideographs
    Han ideographic characters are found in five main blocks of the Unicode Standard, as shown in Table 12-2
Table 12-2. Blocks Containing Han Ideographs
Block                                   Range       Comment
CJK Unified Ideographs                  4E00-9FFF   Common
CJK Unified Ideographs Extension A      3400-4DBF   Rare
CJK Unified Ideographs Extension B      20000-2A6DF Rare, historic
CJK Unified Ideographs Extension C      2A700–2B73F Rare, historic
CJK Unified Ideographs Extension D      2B740–2B81F Uncommon, some in current use
CJK Unified Ideographs Extension E      2B820–2CEAF Rare, historic
CJK Compatibility Ideographs            F900-FAFF   Duplicates, unifiable variants, corporate characters
CJK Compatibility Ideographs Supplement 2F800-2FA1F Unifiable variants
}

function IsCharCJKText(ch: widechar): boolean; inline;
begin
  case Ord(ch) of
    $4E00..$9FFF,
    $3400..$4DBF,
    $F900..$FAFF:
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsCharCJKPunctuation(ch: widechar): boolean; inline;
begin
  case Ord(ch) of
    $3002,
    $ff01,
    $ff0c,
    $ff1a,
    $ff1b,
    $ff1f:
      Result:= true;
    else
      Result:= false;
  end;
end;

{ TATPoint }

class operator TATPoint.=(const A, B: TATPoint): boolean;
begin
  Result:= (A.X=B.X) and (A.Y=B.Y);
end;

var
  //2026.09: cached word-char classification for FindWordWrapOffset()
  //_(replaces per-char _IsWord: IsCharCJKText + Pos() + IsCharWord calls,
  //which dominated the word-wrap calc time of big documents, CudaText perf).
  //Table is rebuilt when the key (NonWordChars, PunctuationToWrapWithWords)
  //changes - these are editor options, stable during typical wrap calc.
  //Main-thread only (like all editor painting code).
  WrapWordTable: array[0..$FFFF] of boolean;
  WrapWordKeyNonChars: UnicodeString = '';
  WrapWordKeyPunct: UnicodeString = '';
  WrapWordTableValid: boolean = false;

procedure WrapWordTableBuild(const ANonWordChars: UnicodeString);
var
  i: integer;
  SPunct: UnicodeString;
begin
  SPunct:= ATEditorOptions.PunctuationToWrapWithWords;
  //2026.09.12 (CudaText perf): pointer equality of the same string instance
  //(editors pass their constant FOptNonWordChars; the stored keys hold
  //references, so a stored key's address cannot be reused by another string)
  //skips the full content comparison, which ran for every wrapped line part
  if WrapWordTableValid and
    (Pointer(WrapWordKeyNonChars)=Pointer(ANonWordChars)) and
    (Pointer(WrapWordKeyPunct)=Pointer(SPunct)) then
    Exit;
  if WrapWordTableValid and
    (WrapWordKeyNonChars=ANonWordChars) and
    (WrapWordKeyPunct=SPunct) then
    Exit;
  WrapWordKeyNonChars:= ANonWordChars;
  WrapWordKeyPunct:= SPunct;
  for i:= 0 to $FFFF do
    if IsCharCJKText(widechar(i)) then
      WrapWordTable[i]:= false
    else
    if Pos(widechar(i), SPunct)>0 then
      WrapWordTable[i]:= true
    else
      WrapWordTable[i]:= IsCharWord(widechar(i), ANonWordChars);
  WrapWordTableValid:= true;
end;

function WrapWordChar(ch: widechar): boolean; inline;
begin
  Result:= WrapWordTable[Ord(ch)];
end;

function SGetIndentCharsBuf(P: PatChar; ALen: SizeInt): SizeInt; inline;
begin
  //2026.09 (CudaText perf): pointer-based version of SGetIndentChars(atString)
  Result:= 0;
  if P=nil then Exit;
  while (Result<ALen) and IsCharSpace(P[Result]) do
    Inc(Result);
end;

function TATStringTabHelper.FindWordWrapOffset(ALineIndex: integer; const S: atString; AColumns: Int64;
  const ANonWordChars: atString; AWrapIndented: boolean): integer;
begin
  //2026.09 (CudaText perf): thin wrapper, all logic is in the PWideChar-based
  //version (used by the word-wrap calc without per-part string allocations)
  Result:= FindWordWrapOffsetBuf(ALineIndex, Pointer(S), Length(S), AColumns, ANonWordChars, AWrapIndented);
end;

function TATStringTabHelper.FindWordWrapOffsetBuf(ALineIndex: integer; P: PatChar; ALen: SizeInt; AColumns: Int64;
  const ANonWordChars: atString; AWrapIndented: boolean): integer;
//
//P points to the first char of the scanned line part, ALen is its length
//(P[k] corresponds to S[k+1] of the old atString-based version)
//
//override IsCharWord to check also commas,dots,quotes
//to wrap them with wordchars
//(old nested _IsWord() is replaced by the cached WrapWordTable lookup,
//which gives identical results for the same options)
//
var
  N, NMin, NAvg: SizeInt;
  Offsets: TATIntFixedArray;
  ch: widechar;
  bWordCh, bWordNext: boolean;
  i: SizeInt;
  NClass: byte;
  bCheckWidth, bAllWidth, bAllWordChars: boolean;
begin
  if (P=nil) or (ALen=0) then
    Exit(0);
  if AColumns<ATEditorOptions.MinWordWrapOffset then
    Exit(AColumns);

  //2026.09.11 (CudaText perf): forward scan of the line part detects the
  //frequent case when ALL chars have the fixed width of 1 column (100
  //percents), i.e. CalcCharOffsetsBuf() would fill Offsets.Data[k]=(k+1)*100
  //for them (exactly what it fills for parts longer than ATEditorMaxFixedArray).
  //It happens for monospaced fonts (not FontProportional) when all chars are
  //of FixedSizes classes uw_normal/uw_space, without tab-chars (checked by
  //the scan below); for parts longer than ATEditorMaxFixedArray it's always so
  //(CalcCharOffsetsBuf fills uniform values for them regardless of the font).
  //Then the wrap candidates are calculated arithmetically, without building
  //the 32Kb Offsets array per line part, which dominated the wrap calc time
  //of big documents (CudaText test: 1M lines x 500 random hex chars).
  //Mixed parts (with tabs/CJK/fullwidth chars) and proportional fonts fall
  //back to the original code, results are identical in all cases.
  //
  //2026.09.12 fix (CudaText perf): the scan MUST set bAllWidth:=true on
  //success. It was initialized to (ALen>ATEditorMaxFixedArray), which is
  //false for all parts <=4096 chars, and the scan below only assigned
  //false - so the arithmetic path was DEAD CODE for every part of the wrap
  //calc (parts are capped by NPartCap<=2048 chars), and CalcCharOffsetsBuf
  //still built the 32Kb Offsets array for every line part. Profile of
  //CudaText set_text_all (1M lines x 500 hex chars, wrap on): UPDATEWRAPINFO
  //18.0s, of which CALCCHAROFFSETSBUF 10.1s - all of it avoidable by the
  //already-verified arithmetic path.
  //The scan also detects parts consisting entirely of word-chars (no
  //spaces/punctuation-mix/CJK): for such parts the backward word-boundary
  //scan below provably ends at N<=NMin and returns NAvg, so it is skipped
  //(typical for hex/base64/URL-like corpora without any spaces).
  bCheckWidth:= (not FontProportional) and (ALen<=ATEditorMaxFixedArray);
  bAllWidth:= (ALen>ATEditorMaxFixedArray);
  bAllWordChars:= false;

  WrapWordTableBuild(ANonWordChars);

  if bCheckWidth then
  begin
    bAllWidth:= true;
    bAllWordChars:= true;
    i:= 0;
    while i<ALen do
    begin
      ch:= P[i];
      //2026.09.12 (CudaText perf): printable ASCII (space..tilde) is always
      //of fixed 1-column width (uw_normal, and space is non-tab uw_space),
      //so the FixedSizes lookup is only needed for other chars; this halved
      //the scan time of the word-wrap calc for hex/base64-like corpora
      if (ch<#32) or (ch>#126) then
      begin
        NClass:= FixedSizes[Ord(ch)];
        if (NClass<>uw_normal) and
          ((NClass<>uw_space) or (ch=#9)) then
        begin
          bAllWidth:= false;
          Break;
        end;
      end;
      if not WrapWordChar(ch) then
        bAllWordChars:= false;
      Inc(i);
    end;
    if not bAllWidth then
      bAllWordChars:= false; //scan stopped early: word-char info is partial
  end;

  if bAllWidth then
  begin
    //offsets of the whole part are (k+1)*100
    if Min(ALen, ATEditorMaxFixedArray)<=AColumns then
      Exit(ALen);
    //NAvg is average wrap offset, we use it if no correct offset found
    N:= Min(ALen, ATEditorMaxFixedArray)-1;
    if N>AColumns then
      N:= SizeInt(AColumns); //AColumns<N<=ATEditorMaxFixedArray here, fits SizeInt
  end
  else
  begin
    CalcCharOffsetsBuf(ALineIndex, P, ALen, Offsets);

    if Offsets.Data[Offsets.Len-1]<=AColumns*100 then
      Exit(ALen);

    //NAvg is average wrap offset, we use it if no correct offset found
    N:= Min(ALen, ATEditorMaxFixedArray)-1;
    while (N>0) and (Offsets.Data[N]>(AColumns+1)*100) do Dec(N);
  end;

  NAvg:= N;
  if NAvg<ATEditorOptions.MinWordWrapOffset then
    Exit(ATEditorOptions.MinWordWrapOffset);

  //2026.09.12 (CudaText perf): if all chars of the part are word-chars,
  //then every position of the backward scan below satisfies its first
  //continue-condition (bWordCh and bWordNext), so the scan always runs
  //down to N<=NMin and the result is NAvg - skip it entirely
  if bAllWordChars then
    Exit(NAvg);

  NMin:= SGetIndentCharsBuf(P, ALen)+1;

  //2026.09: optimized scan (CudaText perf): PWideChar access instead of
  //indexed S[N] (each indexed read of UnicodeString is a runtime helper call
  //with range check); classification of a char is done once and reused on
  //the next loop iteration (as classification of the previous char)
  //2026.09.11: test of 2 word-chars is placed first (2 cached booleans,
  //cheaper than IsCharSurrogateLow/IsCharCJKText calls), operands are pure
  //so the order gives identical results
  //0-based pointer access: S[i] = P[i-1]; initial N is in 1..Length(S)-1,
  //loop keeps N>=1 (Break when N<=NMin, NMin>=1)
  if N>=1 then
    bWordCh:= WrapWordChar(P[N-1]) //class of S[N]
  else
    bWordCh:= false;
  bWordNext:= WrapWordChar(P[N]); //class of S[N+1]
  repeat
    if (N>NMin) and
     ((bWordCh and bWordNext) or //don't wrap between 2 word-chars
      IsCharSurrogateLow(P[N]) or //don't wrap inside surrogate pair: S[N+1]
      (IsCharCJKText(P[N-1]) and IsCharCJKPunctuation(P[N])) or //don't wrap between CJK char and CJK punctuation
      (AWrapIndented and IsCharSpace(P[N])) //space as 2nd char looks bad with Python sources
     )
    then
    begin
      Dec(N);
      bWordNext:= bWordCh;
      bWordCh:= WrapWordChar(P[N-1]); //class of new S[N]; N>=1 here
    end
    else
      Break;
  until false;

  if N>NMin then
    Result:= N
  else
    Result:= NAvg;
end;

function TATStringTabHelper.FindWordWrapOffsetBytes(P: PByte; ALen: SizeInt; AColumns: Int64;
  const ANonWordChars: atString; AWrapIndented: boolean): integer;
{
2026.09.12 (CudaText perf): raw-ASCII variant of FindWordWrapOffsetBuf for
the word-wrap calculation (ATWrapInfo_CalcLine). It works on the raw ANSI
bytes of ASCII-stored TATStrings items (Ex.Wide=false: all bytes <128), so
the per-part byte-to-WideChar conversion (TATStringItem.LineSubBuf, ~0.2s
per 1M lines x 500 chars on the reference machine) is skipped entirely, and
the scan reads 1 byte per char instead of 2.

Behavior: returns the same offset as FindWordWrapOffsetBuf would give for
the zero-extended WideChar buffer, or -1 if the part contains a byte outside
#printable-ASCII #32..#126 (tab, control char, or -defensively- a byte >=128,
which cannot occur in ASCII-stored items): caller falls back to the PWideChar
version then. For bytes in #32..#126:
- all are of fixed 1-column width (uw_normal, and #32 is non-tab uw_space),
  exactly what the width scan of FindWordWrapOffsetBuf verifies;
- surrogate/CJK conditions of the backward scan are never true, so only the
  word-glue test and the wrap-indented space test remain;
- IsCharSpace() is true only for #32 (FixedSizes: #9/#32/#A0.. are uw_space,
  of which only #32 is in the accepted byte range), so the indent scan counts
  leading #32 bytes, like SGetIndentCharsBuf does for the wide buffer.
}
var
  N, NMin, NAvg: SizeInt;
  b: byte;
  bWordCh, bWordNext, bAllWordChars: boolean;
  i: SizeInt;
begin
  Result:= -1;
  if (P=nil) or (ALen=0) then
    Exit(0);
  if AColumns<ATEditorOptions.MinWordWrapOffset then
    Exit(AColumns);

  WrapWordTableBuild(ANonWordChars);

  //one scan: verify all bytes are printable ASCII (else -1 = fallback;
  //such bytes are all of fixed 1-column width), and detect the all-word-chars
  //case (to skip the backward scan)
  bAllWordChars:= true;
  i:= 0;
  while i<ALen do
  begin
    b:= P[i];
    if (b<32) or (b>126) then
      Exit(-1);
    if not WrapWordTable[b] then
      bAllWordChars:= false;
    Inc(i);
  end;

  //offsets of the whole part are (k+1)*1
  if Min(ALen, ATEditorMaxFixedArray)<=AColumns then
    Exit(ALen);

  N:= Min(ALen, ATEditorMaxFixedArray)-1;
  if N>AColumns then
    N:= SizeInt(AColumns);

  NAvg:= N;
  if NAvg<ATEditorOptions.MinWordWrapOffset then
    Exit(ATEditorOptions.MinWordWrapOffset);

  if bAllWordChars then
    Exit(NAvg);

  //indent chars of the part: leading #32 bytes (see comment above)
  NMin:= 0;
  while (NMin<ALen) and (P[NMin]=32) do
    Inc(NMin);
  Inc(NMin);

  //backward word-boundary scan, byte version: only the word-glue test and
  //the wrap-indented space test apply (surrogate/CJK are impossible here);
  //0-based access: P[i] = wide-buffer P[i]
  if N>=1 then
    bWordCh:= WrapWordTable[P[N-1]]
  else
    bWordCh:= false;
  bWordNext:= WrapWordTable[P[N]];
  repeat
    if (N>NMin) and
     ((bWordCh and bWordNext) or //don't wrap between 2 word-chars
      (AWrapIndented and (P[N]=32)) //space as 2nd char looks bad with Python sources
     )
    then
    begin
      Dec(N);
      bWordNext:= bWordCh;
      bWordCh:= WrapWordTable[P[N-1]]; //N>=1 here
    end
    else
      Break;
  until false;

  if N>NMin then
    Result:= N
  else
    Result:= NAvg;
end;

function SGetIndentChars(const S: string): SizeInt;
var
  P: PChar;
begin
  //2026.09: optimized (CudaText perf): pointer access instead of indexed S[i]
  Result:= 0;
  P:= Pointer(S);
  while (P<>nil) and (P^<>#0) and IsCharSpace(P^) do
  begin
    Inc(Result);
    Inc(P);
  end;
end;

function SGetIndentChars(const S: atString): SizeInt;
begin
  //2026.09 (CudaText perf): reuses the pointer-based scan
  Result:= SGetIndentCharsBuf(Pointer(S), Length(S));
end;

function SGetTrailingSpaceChars(const S: atString): SizeInt;
var
  N: SizeInt;
begin
  Result:= 0;
  N:= Length(S);
  while (N>0) and IsCharSpace(S[N]) do
  begin
    Inc(Result);
    Dec(N);
  end;
end;


function SGetIndentCharsToOpeningBracket(const S: atString): SizeInt;
var
  n: SizeInt;
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

function SGetNonSpaceLength(const S: atString): SizeInt;
begin
  Result:= Length(S);
  while (Result>0) and IsCharSpace(S[Result]) do Dec(Result);
end;

procedure SSwapEndianWide(var S: UnicodeString);
var
  i: SizeInt;
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
  i: SizeInt;
begin
  for i:= 0 to Length(S)-1 do
    S[i]:= SwapEndian(S[i]);
end;

constructor TATStringTabHelper.Create(ACharSizer: TATCharSizer);
begin
  inherited Create;
  CharSizer:= ACharSizer;
end;

function TATStringTabHelper.CalcTabulationSize(ALineIndex, APos: SizeInt): integer;
begin
  if Assigned(OnCalcTabSize) then
    Result:= OnCalcTabSize(SenderObj, ALineIndex, APos)
  else
  if Assigned(OnCalcLineLen) and (ALineIndex>=0) and (OnCalcLineLen(ALineIndex)>ATEditorMaxFixedArray) then
    Result:= 1
  else
  if APos>ATEditorOptions.MaxTabPositionToExpand then
    Result:= 1
  else
    Result:= TabSize - (APos-1) mod TabSize;
end;


function TATStringTabHelper.GetIndentExpanded(ALineIndex: integer; const S: atString): integer;
begin
  //2026.09 (CudaText perf): thin wrapper, logic is in the PWideChar-based version
  Result:= GetIndentExpandedBuf(ALineIndex, Pointer(S), Length(S));
end;

function TATStringTabHelper.GetIndentExpandedBuf(ALineIndex: integer; P: PatChar; ALen: SizeInt): integer;
var
  ch: widechar;
  i: SizeInt;
begin
  //2026.09 (CudaText perf): pointer-based version, same behavior:
  //P[k] corresponds to S[k+1] of the atString-based version
  Result:= 0;
  if P=nil then Exit;
  for i:= 0 to ALen-1 do
  begin
    ch:= P[i];
    if not IsCharSpace(ch) then exit;
    if ch<>#9 then
      Inc(Result)
    else
      Inc(Result, CalcTabulationSize(ALineIndex, Result+1));
  end;
end;


function TATStringTabHelper.TabsToSpaces(ALineIndex: integer; const S: atString): atString;
var
  N, NSize: SizeInt;
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
  AMaxLen: SizeInt): SizeInt;
var
  i: SizeInt;
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
  out AInfo: TATIntFixedArray; ACharsSkipped: SizeInt=0);
begin
  //2026.09 (CudaText perf): thin wrapper, logic is in the PWideChar-based version
  CalcCharOffsetsBuf(ALineIndex, Pointer(S), Length(S), AInfo, ACharsSkipped);
end;

procedure TATStringTabHelper.CalcCharOffsetsBuf(ALineIndex: integer; P: PatChar; ALen: SizeInt;
  out AInfo: TATIntFixedArray; ACharsSkipped: SizeInt=0);
var
  NLen, NSize, NTabSize, NCharsSkipped: SizeInt;
  NScalePercents: SizeInt;
  ch: widechar;
  i: SizeInt;
  NSum: Int64;
begin
  //2026.09: don't clear the whole 32Kb record here (was: AInfo:= Default()):
  //CalcCharOffsets is called ~1.5M times for a wrapped 300K-lines document,
  //so full-record clearing was several seconds of pure memset; all callers
  //read only Data[0..Len-1], so clearing Len is enough
  AInfo.Len:= 0;
  if (P=nil) or (ALen=0) then Exit;
  NLen:= Min(ALen, ATEditorMaxFixedArray);
  AInfo.Len:= NLen;

  if ALen>ATEditorMaxFixedArray then
  begin
    for i:= 0 to NLen-1 do
      AInfo.Data[i]:= (Int64(i)+1)*100;
    exit;
  end;

  NCharsSkipped:= ACharsSkipped;

  //2026.09: optimized loop (CudaText perf): PWideChar access instead of
  //indexed S[i] (each indexed read of UnicodeString is a runtime helper call
  //with range check); running sum in local var instead of reading
  //AInfo.Data[i-2] per char;
  //for monospaced fonts, width of 'normal' chars is 100% without calling
  //CharSizer.GetCharWidth (which was a per-char method call dominating the
  //wrap calc of big all-ASCII documents) - exactly what GetCharWidth()
  //returns for FixedSizes[ch]=uw_normal in non-proportional mode
  NSum:= 0;
  for i:= 1 to NLen do
  begin
    ch:= P^;
    Inc(P);
    Inc(NCharsSkipped);

    if IsCharSurrogateAny(ch) then
      NScalePercents:= ATEditorOptions.EmojiWidthPercents div 2
    else
    if (not FontProportional) and (FixedSizes[Ord(ch)]=uw_normal) then
      NScalePercents:= 100
    else
      NScalePercents:= CharSizer.GetCharWidth(ch);

    if ch<>#9 then
      NSize:= 1
    else
    if FontProportional then
      NSize:= 1
    else
    begin
      NTabSize:= CalcTabulationSize(ALineIndex, NCharsSkipped);
      NSize:= NTabSize;
      Inc(NCharsSkipped, NTabSize-1);
    end;

    NSum:= NSum + Int64(NSize)*NScalePercents;
    AInfo.Data[i-1]:= NSum;
  end;
end;

function TATStringTabHelper.CalcCharOffsetLast(ALineIndex: integer; const S: atString;
  ACharsSkipped: SizeInt=0): Int64;
var
  NLen, NSize, NTabSize, NCharsSkipped: SizeInt;
  NScalePercents: SizeInt;
  ch: WideChar;
  i: SizeInt;
  P: PWideChar;
begin
  Result:= 0;
  NLen:= Length(S);
  if NLen=0 then Exit;

  if NLen>ATEditorMaxFixedArray then
    exit(NLen*100);

  NCharsSkipped:= ACharsSkipped;

  //2026.09: optimized loop (CudaText perf): PWideChar access instead of
  //indexed S[i] (each indexed read of UnicodeString is a runtime helper call
  //with range check); for monospaced fonts, width of 'normal' chars is 100%
  //without the per-char CharSizer.GetCharWidth method call (same trick as
  //in CalcCharOffsetsBuf)
  P:= Pointer(S);
  for i:= 1 to NLen do
  begin
    ch:= P^;
    Inc(P);
    Inc(NCharsSkipped);

    if IsCharSurrogateAny(ch) then
    begin
      NScalePercents:= ATEditorOptions.EmojiWidthPercents div 2;
    end
    else
    if (not FontProportional) and (FixedSizes[Ord(ch)]=uw_normal) then
    begin
      NScalePercents:= 100;
    end
    else
    begin
      NScalePercents:= CharSizer.GetCharWidth(ch);
    end;

    if ch<>#9 then
      NSize:= 1
    else
    if FontProportional then
      NSize:= 1
    else
    begin
      NTabSize:= CalcTabulationSize(ALineIndex, NCharsSkipped);
      NSize:= NTabSize;
      Inc(NCharsSkipped, NTabSize-1);
    end;

    Inc(Result, Int64(NSize)*NScalePercents);
  end;
end;


function TATStringTabHelper.FindClickedPosition(ALineIndex: integer;
  const Str: atString;
  const StrLen: SizeInt;
  constref ListOffsets: TATIntFixedArray;
  APixelsFromLeft: Int64;
  AAllowVirtualPos: boolean;
  out AEndOfLinePos: boolean): Int64;
var
  i: SizeInt;
begin
  AEndOfLinePos:= false;

  if Str='' then
  begin
    Result:= 1;
    if AAllowVirtualPos then
      Inc(Result, Round(APixelsFromLeft * ATEditorCharXScale / CharSize.XScaled));
      //use Round() to fix CudaText issue #4240
    Exit;
  end;

  ListEnds.Len:= ListOffsets.Len;
  ListMid.Len:= ListOffsets.Len;

  //positions of each char end
  for i:= 0 to ListOffsets.Len-1 do
    ListEnds.Data[i]:= ListOffsets.Data[i]*CharSize.XScaled div ATEditorCharXScale div 100;

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

  Result:= ListEnds.Len + Round((APixelsFromLeft - ListEnds.Data[ListEnds.Len-1]) * ATEditorCharXScale / CharSize.XScaled) + 1;
  //use Round() to fix CudaText issue #4240

  ////this works
  ////a) better if clicked after line end, far
  ////b) bad if clicked exactly on line end (shifted to right by 1)
  //Result:= ListEnds.Len + (APixelsFromLeft - ListEnds.Data[ListEnds.Len-1] - ACharSize div 2) div ACharSize + 2;

  if not AAllowVirtualPos then
    Result:= Min(Result, StrLen+1);
end;

procedure TATStringTabHelper.FindOutputSkipOffset(ALineIndex: integer;
  const S: atString;
  const AScrollPosSmooth: Int64;
  out ACharsSkipped: Int64;
  out ACellPercentsSkipped: Int64);
var
  Offsets: TATIntFixedArray;
  NCheckedOffset: Int64;
begin
  ACharsSkipped:= 0;
  ACellPercentsSkipped:= 0;
  if (S='') or (AScrollPosSmooth=0) then Exit;

  CalcCharOffsets(ALineIndex, S, Offsets);

  NCheckedOffset:= AScrollPosSmooth * 100 * ATEditorCharXScale div CharSize.XScaled;

  while (ACharsSkipped<Offsets.Len) and
    (Offsets.Data[ACharsSkipped] < NCheckedOffset) do
    Inc(ACharsSkipped);

  if (ACharsSkipped>0) then
    ACellPercentsSkipped:= Offsets.Data[ACharsSkipped-1];
end;

function SGetItem(var S: string; const ch: Char = ','): string;
var
  i: SizeInt;
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
{
2026.09.12 (CudaText perf): word-at-a-time scan - 8 bytes per iteration instead
of two Pos() calls (two full passes over the line). Same result: any byte
#10 or #13. It's called per line by TATStrings.TextReplaceLines_UTF8 (the
parse loop of CudaText's ed.replace_lines: 1M lines x 500 chars gave
~0.4s in the user's callgrind for the two Pos passes).
}
var
  P: PByte;
  NLen: SizeInt;
  Q, X: QWord;
begin
  NLen:= Length(S);
  if NLen=0 then exit(false);
  P:= Pointer(S);
  //leading unaligned bytes: per-byte
  while (NLen>0) and ((PtrUInt(P) and 7)<>0) do
  begin
    if (P^=10) or (P^=13) then exit(true);
    Inc(P);
    Dec(NLen);
  end;
  //main part: 8 bytes per iteration, SWAR zero-byte detection for both #10 and #13
  while NLen>=8 do
  begin
    Q:= PQWord(P)^;
    X:= Q xor QWord($0A0A0A0A0A0A0A0A);
    if (((X-QWord($0101010101010101)) and (not X) and QWord($8080808080808080))<>0) then exit(true);
    X:= Q xor QWord($0D0D0D0D0D0D0D0D);
    if (((X-QWord($0101010101010101)) and (not X) and QWord($8080808080808080))<>0) then exit(true);
    Inc(P, 8);
    Dec(NLen, 8);
  end;
  //tail: per-byte
  while NLen>0 do
  begin
    if (P^=10) or (P^=13) then exit(true);
    Inc(P);
    Dec(NLen);
  end;
  Result:= false;
end;

function TATStringTabHelper.SpacesToTabs(ALineIndex: integer; const S: atString): atString;
begin
  Result:= StringReplace(S, StringOfCharW(' ', TabSize), WideChar(9), [rfReplaceAll]);
end;

function TATStringTabHelper.CharPosToColumnPos(ALineIndex: integer; const S: atString;
  APos: SizeInt): integer;
begin
  if APos>Length(S) then
    Result:= TabsToSpaces_Length(ALineIndex, S, -1) + APos-Length(S)
  else
    Result:= TabsToSpaces_Length(ALineIndex, S, APos);
end;

function TATStringTabHelper.ColumnPosToCharPos(ALineIndex: integer; const S: atString;
  AColumn: SizeInt): integer;
var
  size, i: SizeInt;
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
  code, i: SizeInt;
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
  code, i: SizeInt;
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
  DecSpaces, N: SizeInt;
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


function STrimNewlineChars(const S: UnicodeString): UnicodeString;
var
  Ofs, Len: sizeint;
begin
  Len := Length(S);
  while (Len>0) and IsCharEol(S[Len]) do
    Dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and IsCharEol(S[Ofs]) do
    Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end;

function SRemoveNewlineChars(const S: UnicodeString): UnicodeString;
var
  i: SizeInt;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    if IsCharEol(Result[i]) then
      Result[i]:= ' ';
end;

function STrimAll(const S: UnicodeString): UnicodeString;
var
  Ofs, Len: sizeint;
begin
  len := Length(S);
  while (Len>0) and (IsCharSpace(S[Len])) do
   dec(Len);
  Ofs := 1;
  while (Ofs<=Len) and (IsCharSpace(S[Ofs])) do
    Inc(Ofs);
  result := Copy(S, Ofs, 1 + Len - Ofs);
end;

function STrimLeft(const S: UnicodeString): UnicodeString;
var
  i,l:sizeint;
begin
  l := length(s);
  i := 1;
  while (i<=l) and (IsCharSpace(s[i])) do
    inc(i);
  Result := copy(s, i, l);
end;

function STrimRight(const S: UnicodeString): UnicodeString;
var
  l:sizeint;
begin
  l := length(s);
  while (l>0) and (IsCharSpace(s[l])) do
    dec(l);
  result := copy(s,1,l);
end;

function SBeginsWith(const S, SubStr: UnicodeString): boolean;
var
  i: SizeInt;
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
  i: SizeInt;
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
  i, Offset: SizeInt;
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
  i, Offset: SizeInt;
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


function SConvertCaseToTitle(const S, SNonWordChars: UnicodeString): UnicodeString;
var
  i: SizeInt;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    if (i=1) or (S[i-1]='_') or not IsCharWord(S[i-1], SNonWordChars) then
      Result[i]:= SCharUpper(Result[i])
    else
      Result[i]:= SCharLower(Result[i]);
end;

function SConvertCaseToInverted(const S: UnicodeString): UnicodeString;
var
  ch, ch_up: WideChar;
  i: SizeInt;
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

function SConvertCaseToUpper(const S: UnicodeString): UnicodeString;
var
  i: SizeInt;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    Result[i]:= SCharUpper(Result[i]);
end;

function SConvertCaseToLower(const S: UnicodeString): UnicodeString;
var
  i: SizeInt;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    Result[i]:= SCharLower(Result[i]);
end;

function SConvertCaseToSentence(const S, SNonWordChars: UnicodeString): UnicodeString;
var
  dot: boolean;
  ch: WideChar;
  i: SizeInt;
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

function StringOfCharW(ch: WideChar; Len: SizeInt): UnicodeString;
var
  i: SizeInt;
begin
  SetLength(Result{%H-}, Len);
  for i:= 1 to Len do
    Result[i]:= ch;
end;


procedure SReplaceAll(var S: string; const SFrom, STo: string);
begin
  S:= StringReplace(S, SFrom, STo, [rfReplaceAll]);
end;

procedure SDeleteFrom(var s: string; const SFrom: string);
var
  n: SizeInt;
begin
  n:= Pos(SFrom, S);
  if n>0 then
    SetLength(S, n-1);
end;

procedure SDeleteFrom(var s: UnicodeString; const SFrom: UnicodeString);
var
  n: SizeInt;
begin
  n:= Pos(SFrom, S);
  if n>0 then
    SetLength(S, n-1);
end;

procedure SDeleteFromEol(var S: string);
var
  i: SizeInt;
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
  i: SizeInt;
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

procedure SClipboardCopy({const? not for gtk2} AText: string; AClipboardObj: TClipboard=nil);
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


function SFindCharCount(const S: string; ch: char): SizeInt;
var
  i: SizeInt;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    if S[i]=ch then
      Inc(Result);
end;

function SFindCharCount(const S: UnicodeString; ch: WideChar): SizeInt;
var
  i: SizeInt;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    if S[i]=ch then
      Inc(Result);
end;


function SFindRegexMatch(const Subject, Regex: UnicodeString; out MatchPos, MatchLen: SizeInt): boolean;
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


function SCountTextOccurrences(const SubStr, Str: UnicodeString): SizeInt;
var
  Offset: SizeInt;
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

function SCountTextLines(const Str, StrBreak: UnicodeString): SizeInt;
begin
  Result:= SCountTextOccurrences(StrBreak, Str)+1;
  // ignore trailing EOL
  if Length(Str)>=Length(StrBreak) then
    if Copy(Str, Length(Str)-Length(StrBreak)+1, Length(StrBreak))=StrBreak then
      Dec(Result);
end;

procedure SSplitByChar(const S: string; Sep: char; out S1, S2: string);
var
  N: SizeInt;
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
{
2026.09 (CudaText perf): word-at-a-time scan - 8 bytes per iteration instead of
per-byte loop (it's called per line on the API paths: replace_lines, file load,
SetLineW/SetLineA classification of big blocks). Gives the same result: any byte
with high bit set = multi-byte UTF8 sequence. Bytes are read via PQWord only
after the pointer is 8-aligned (leading bytes handled per-byte), so it's safe
on all CPUs/OSes.
}
var
  P: PByte;
  NLen: SizeInt;
  Q: QWord;
begin
  NLen:= Length(S);
  if NLen=0 then exit(false);
  P:= Pointer(S);
  //leading unaligned bytes: per-byte
  while (NLen>0) and ((PtrUInt(P) and 7)<>0) do
  begin
    if P^>=128 then exit(true);
    Inc(P);
    Dec(NLen);
  end;
  //main part: 8 bytes per iteration
  while NLen>=8 do
  begin
    Q:= PQWord(P)^;
    if (Q and QWord($8080808080808080))<>0 then exit(true);
    Inc(P, 8);
    Dec(NLen, 8);
  end;
  //tail: per-byte
  while NLen>0 do
  begin
    if P^>=128 then exit(true);
    Inc(P);
    Dec(NLen);
  end;
  Result:= false;
end;

function IsStringWithUnicode(const S: UnicodeString): boolean;
{
2026.09 (CudaText perf): word-at-a-time scan - 4 WideChars per iteration
instead of per-char loop (used by TATStringItem.SetLineW). Gives the same
result: any WideChar >=128. Alignment-safe like the 'string' overload.
}
var
  P: PWord;
  NLen: SizeInt;
  Q: QWord;
begin
  NLen:= Length(S);
  if NLen=0 then exit(false);
  P:= Pointer(S);
  //leading unaligned words: per-char
  while (NLen>0) and ((PtrUInt(P) and 7)<>0) do
  begin
    if P^>=128 then exit(true);
    Inc(P);
    Dec(NLen);
  end;
  //main part: 4 WideChars (8 bytes) per iteration
  //mask $FF80 per 16-bit lane: char>=128 <=> (lane and $FF80)<>0
  //(low byte's bit 7, or any bit of the high byte)
  while NLen>=4 do
  begin
    Q:= PQWord(P)^;
    if (Q and QWord($FF80FF80FF80FF80))<>0 then exit(true);
    Inc(P, 4);
    Dec(NLen, 4);
  end;
  //tail: per-char
  while NLen>0 do
  begin
    if P^>=128 then exit(true);
    Inc(P);
    Dec(NLen);
  end;
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


//from https://forum.lazarus.freepascal.org/index.php?topic=65813.0
procedure SDeleteAndInsert(var AStr: UnicodeString; AFromPos, ACount: SizeInt;
  const AReplace: UnicodeString);
var
  LenStr: SizeInt;
  LenReplace: SizeInt;
  GrowCount: SizeInt;
  MoveSize: SizeInt;
  AfterPos: SizeInt;
begin
  LenStr := Length(AStr);
  if LenStr = 0 then
  begin
    AStr := AReplace;
    UniqueString(AStr);
    Exit;
  end;
  LenReplace := Length(AReplace);
  if LenReplace = 0 then
  begin
    Delete(AStr, AFromPos, ACount);
    Exit;
  end;
  AFromPos := Min(AFromPos, LenStr + 1);
  ACount := Min(ACount, LenStr + 1 - AFromPos);
  AfterPos := AFromPos + ACount;
  // Was: AStr[1]..AStr[AFromPos]..AStr[AfterPos]..AStr[LenStr]
  // New: AStr[1]..AReplace..AStr[LenStr]
  MoveSize := (LenStr + 1 - AfterPos) * SizeOf(UnicodeChar);
  GrowCount := LenReplace - ACount;
  if GrowCount > 0 then  // Need grow
    SetLength(AStr, LenStr + GrowCount);
  if MoveSize > 0 then
    Move(AStr[AfterPos], AStr[AFromPos + LenReplace], MoveSize);
  Move(AReplace[1], AStr[AFromPos], LenReplace * SizeOf(UnicodeChar));
  if GrowCount < 0 then  // Need shrink
    SetLength(AStr, LenStr + GrowCount);
end;


procedure SDeleteHtmlTags(var S: string);
var
  n1, n2: SizeInt;
begin
  repeat
    n1:= Pos('<', S);
    if n1=0 then Break;
    n2:= Pos('>', S, n1+1);
    if n2=0 then Break;
    Delete(S, n1, n2-n1+1);
  until false;
end;


procedure SFixGreekTextAfterCaseConversion(var S: UnicodeString;
  ALastCharIsWordEdge: boolean);
var
  SNonWord: UnicodeString;
  NLen, i: SizeInt;
begin
  UniqueString(S);
  NLen:= Length(S);
  SNonWord:= ATEditorOptions.DefaultNonWordChars;

  //replace sigma 'σ' at word ends to final-sigma 'ς'
  i:= 0;
  repeat
    i:= Pos('σ', S, i+1);
    if i=0 then
      Break;
    if (i=NLen) and (not ALastCharIsWordEdge) then
      Break;
    if ((i=1) or IsCharWord(S[i-1], SNonWord)) and
       ((i=NLen) or not IsCharWord(S[i+1], SNonWord)) then
      S[i]:= 'ς';
  until false;

  //uppercase letters don't need diacritics (this is what Remove_Greek_Accents CudaText plugin does)
  S:= UnicodeStringReplace(S, 'Ά' , 'Α', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, 'Έ' , 'Ε', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, 'Ή' , 'Η', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, 'Ί' , 'Ι', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, 'Ϊ́' , 'Ι', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, 'Ό' , 'Ο', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, 'Ύ' , 'Υ', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, 'Ώ' , 'Ω', [rfReplaceAll]);
end;


function SStringHasCharsBadForLigatures(const S: UnicodeString): boolean;
var
  i: SizeInt;
  N: word;
begin
  for i:= 1 to Length(S) do
  begin
    N:= Ord(S[i]);
    //Georgian chars are badly rendered (disappear) with ligatures=on on Win32
    if (N>=$10A0) and (N<=$10FF) then
      exit(true);
  end;
  Result:= false;
end;

{$push}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
function SCalcHashQword(const S: string): QWord;
{
FNV-1a hash of the line's raw buffer bytes, used only to compare identity of
lines ("is this line the same text as that deleted line?"), so the encoding
of the buffer doesn't matter, hashing must be just stable and fast.
2026.09 (CudaText perf): 8 bytes per iteration, two independent FNV chains
(one per 4-byte half of each QWord, combined at the end) - about 2x faster
than the old 1-byte-per-iteration loop on big buffers (callgrind: it was
~6s of the wrap recalc of a 1M-lines replace_lines). Hash values differ from
the old version, but the hash is not persisted anywhere - it's only compared
at runtime, as a part of wrap-cache keys. Alignment-safe: leading bytes
before an 8-aligned pointer are hashed one-by-one.
}
var
  P: PByte;
  NLen, i: SizeInt;
  H1, H2: QWord;
  Q: QWord;
begin
  Result:= 14695981039346656037;
  NLen:= Length(S);
  if NLen=0 then Exit;
  P:= Pointer(S);
  H1:= Result;
  H2:= Result;
  //leading unaligned bytes: per-byte chain H1
  while (NLen>0) and ((PtrUInt(P) and 7)<>0) do
  begin
    H1:= (H1 xor P^) * 1099511628211;
    Inc(P);
    Dec(NLen);
  end;
  //main part: 8 bytes per iteration, two 4-byte chains
  i:= NLen;
  while i>=8 do
  begin
    Q:= PQWord(P)^;
    H1:= (H1 xor (Q and QWord($FFFFFFFF))) * 1099511628211;
    H2:= (H2 xor (Q shr 32)) * 1099511628211;
    Inc(P, 8);
    Dec(i, 8);
  end;
  //tail: per-byte chain H1
  while i>0 do
  begin
    H1:= (H1 xor P^) * 1099511628211;
    Inc(P);
    Dec(i);
  end;
  Result:= (H1 xor H2) * 1099511628211;
end;
{$pop}

end.
