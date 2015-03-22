unit ATStringProc;

{$mode delphi}
//{$define test_wide_char}

interface

uses
  Classes, SysUtils;

type
  atString = UnicodeString;
  atChar = WideChar;

const
  cMaxTabPositionToExpand = 500; //no sense to expand too far tabs
  cCharScaleFullwidth = 1.7; //width of CJK chars

function IsWordChar(ch: atChar; const AWordChars: atString): boolean;
function IsSpecialCodeChar(ch: atChar): boolean;
function IsEolCode(N: Word): boolean;
function IsAccentChar(ch: WideChar): boolean;

function SGetItem(var S: string; const sep: Char = ','): string;
function BoolToPlusMinusOne(b: boolean): integer;
function SSwapEndian(const S: UnicodeString): UnicodeString;
function SGetIndentChars(const S: atString): integer;
function SGetNonSpaceLength(const S: atString): integer;
function SGetIndentExpanded(const S: atString; ATabSize: integer): integer;
procedure SReplaceSpecChars(var S: atString);
procedure TrimStringList(L: TStringList);
function SWithBreaks(const S: atString): boolean;

procedure SCalcCharOffsets(const AStr: atString; var AList: array of real;
  ATabSize: integer; ACharsSkipped: integer = 0);
function SExpandTabulations(const S: atString; ATabSize: integer): atString;
function SFindWordWrapPosition(const S: atString; AColumns, ATabSize: integer;
  const AWordChars: atString): integer;
function SFindClickedPosition(const Str: atString;
  APixelsFromLeft, ACharSize, ATabSize: integer;
  AAllowVirtualPos: boolean): integer;
procedure SFindOutputSkipPosition(const S: atString; ATabSize, AScrollPos: integer;
  out ACharsSkipped: integer; out ASpacesSkipped: real);

implementation

uses
  Dialogs, Math;

function IsEolCode(N: Word): boolean;
begin
  Result:= (N=10) or (N=13);
end;

function IsWordChar(ch: atChar; const AWordChars: atString): boolean;
begin
  Result:=
    ((ch>='0') and (ch<='9')) or
    ((ch>='a') and (ch<='z')) or
    ((ch>='A') and (ch<='Z')) or
    (ch='_');

  if not Result then
    if AWordChars<>'' then
      if Pos(ch, AWordChars)>0 then
        Result:= true;
end;

function IsSpaceChar(ch: atChar): boolean;
begin
  Result:= (ch=' ') or (ch=#9);
end;

function IsSpecialCodeChar(ch: atChar): boolean;
begin
  Result:= (ch<>#9) and (AnsiChar(ch)<' ');
end;

procedure DoDebugOffsets(const List: array of real);
var
  i: integer;
  s: string;
begin
  s:= '';
  for i:= Low(List) to High(List) do
    s:= s+FloatToStr(List[i])+' ';
  showmessage('Offsets'#13+s);
end;

function SFindWordWrapPosition(const S: atString; AColumns, ATabSize: integer;
  const AWordChars: atString): integer;
var
  N, NMin, NAvg: integer;
  List: array of real;
begin
  if S='' then
    begin Result:= 0; Exit end;

  SetLength(List, Length(S));
  SCalcCharOffsets(S, List, ATabSize);

  if List[High(List)]<=AColumns then
  begin
    Result:= Length(S);
    Exit
  end;

  NMin:= SGetIndentChars(S)+1;
  N:= Length(S)-1;
  while (N>NMin) and (List[N]>AColumns+1) do Dec(N);
  NAvg:= N;
  while (N>NMin) and IsWordChar(S[N], AWordChars) and IsWordChar(S[N+1], AWordChars) do Dec(N);

  if N>NMin then
    Result:= N
  else
  if NAvg>NMin then
    Result:= NAvg
  else
    Result:= Length(S);
end;

function SGetIndentChars(const S: atString): integer;
begin
  Result:= 0;
  while (Result<Length(S)) and IsSpaceChar(S[Result+1]) do
    Inc(Result);
end;

function SGetNonSpaceLength(const S: atString): integer;
begin
  Result:= Length(S);
  while (Result>0) and IsSpaceChar(S[Result]) do Dec(Result);
  if Result=0 then
    Result:= Length(S);
end;

function SGetIndentExpanded(const S: atString; ATabSize: integer): integer;
var
  SIndent: atString;
begin
  SIndent:= Copy(S, 1, SGetIndentChars(S));
  SIndent:= SExpandTabulations(SIndent, ATabSize);
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

function SExpandTabulations(const S: atString; ATabSize: integer): atString;
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
function IsAccentChar(ch: WideChar): boolean;
begin
  case Ord(ch) of
    $0300..$036F,
    $1AB0..$1AFF,
    $1DC0..$1DFF,
    $20D0..$20FF,
    $FE20..$FE2F:
      Result:= true;
    else
      Result:= false;
  end;
end;

{
Ranges that are FullWidth char
 1100  e1 84 80  ..  115F  e1 85 9f
 2329  e2 8c a9  ..  232A  e2 8c aa
 2E80  e2 ba 80  ..  303E  e3 80 be
 3041  e3 81 81  ..  33FF  e3 8f bf
 3400  e3 90 80  ..  4DB5  e4 b6 b5
 4E00  e4 b8 80  ..  9FC3  e9 bf 83
 A000  ea 80 80  ..  A4C6  ea 93 86
 AC00  ea b0 80  ..  D7A3  ed 9e a3
 F900  ef a4 80  ..  FAD9  ef ab 99
 FE10  ef b8 90  ..  FE19  ef b8 99
 FE30  ef b8 b0  ..  FE6B  ef b9 ab
 FF01  ef bc 81  ..  FF60  ef bd a0
 FFE0  ef bf a0  ..  FFE6  ef bf a6
20000  f0 a0 80 80  .. 2FFFD f0 af bf bd
30000  f0 b0 80 80  .. 3FFFD f0 bf bf bd
}
function IsCharFullWidth(ch: WideChar): boolean;
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

procedure SCalcCharOffsets(const AStr: atString; var AList: array of real;
  ATabSize: integer; ACharsSkipped: integer);
var
  S: atString;
  NOffset, NTabSize, NListIndex, i: integer;
  Scale: real;
begin
  if AStr='' then Exit;
  if Length(AList)<>Length(AStr) then
    raise Exception.Create('Bad list len in CalcCharOffsets');

  S:= AStr;
  i:= 0;
  NListIndex:= 0;

  repeat
    Inc(i);
    if i>Length(S) then Break;

    if IsCharFullWidth(S[i]) then
      Scale:= cCharScaleFullwidth
    else
      Scale:= 1.0;

    {$ifdef test_wide_char}
    if IsSpaceChar(S[i]) then
      Scale:= 1
    else
      Scale:= cScaleTest;
    {$endif}

    if S[i]<>#9 then
      NOffset:= 1
    else
    begin
      NTabSize:= SCalcTabulationSize(ATabSize, i+ACharsSkipped);
      NOffset:= NTabSize;
      S[i]:= ' ';
      if NTabSize>1 then
        Insert(StringOfChar(' ', NTabSize-1), S, i);
      Inc(i, NTabSize-1);
    end;

    if (i<Length(S)) and IsAccentChar(S[i+1]) then
    begin
      NOffset:= 0;
    end;

    if NListIndex=0 then
      AList[NListIndex]:= NOffset*Scale
    else
      AList[NListIndex]:= AList[NListIndex-1]+NOffset*Scale;

    Inc(NListIndex);
  until false;
end;


function SFindClickedPosition(const Str: atString;
  APixelsFromLeft, ACharSize, ATabSize: integer;
  AAllowVirtualPos: boolean): integer;
var
  ListReal: array of real;
  ListEnds, ListMid: array of integer;
  i: integer;
begin
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

  if AAllowVirtualPos then
    Result:= Length(Str)+1 + (APixelsFromLeft - ListEnds[High(ListEnds)]) div ACharSize
  else
    Result:= Length(Str)+1;
end;

procedure SFindOutputSkipPosition(const S: atString; ATabSize, AScrollPos: integer;
  out ACharsSkipped: integer; out ASpacesSkipped: real);
var
  List: array of real;
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

procedure SReplaceSpecChars(var S: atString);
const
  cSpecChar = '.';
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if IsSpecialCodeChar(S[i]) then
      S[i]:= cSpecChar;
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


procedure TrimStringList(L: TStringList);
begin
  while (L.Count>0) and (L[L.Count-1]='') do
    L.Delete(L.Count-1);
end;

function SWithBreaks(const S: atString): boolean;
begin
  Result:=
    (Pos(#13, S)>0) or
    (Pos(#10, S)>0);
end;

end.

