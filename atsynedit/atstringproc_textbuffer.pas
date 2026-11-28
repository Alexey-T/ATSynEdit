{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_TextBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATSynEdit_FGL;

type
  TATStringBufferChange = procedure(Sender: TObject; Pos, Count, LineChange: integer) of object;

type
  TATGenericIntList = specialize TFPGList<integer>;

type
  { TATStringBuffer }

  TATStringBuffer = class
  strict private
    //buffer must always use LF line endings, FText must have only such line endings
    const LenEOL=1;
  strict private
    FList: array of integer;
    FOnChange: TATStringBufferChange;
    function GetCount: integer; inline;
    procedure SetCount(AValue: integer);
    procedure SetupFromGenericList(L: TATGenericIntList);
  public
    Valid: boolean;
    Version: Int64;
    FText: UnicodeString;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Setup(const AText: UnicodeString; const ALineLens: array of integer);
    procedure SetupSlow(const AText: UnicodeString);
    //one line's text changed, no line was added or removed: the text is
    //patched in place (a single move of the tail) and the offsets of the
    //following lines shift by the length delta. O(line + tail move +
    //line count), no full-text rebuild (Setup of a huge text is slow)
    procedure ChangeLine(ALine: integer; const SNewLine: UnicodeString);
    procedure Assign(Other: TATStringBuffer);
    function CaretToStr(const APnt: TPoint): integer;
    function StrToCaret(APos: integer): TPoint;
    function SubString(APos, ALen: integer): UnicodeString; inline;
    function TextLength: integer;
    function OffsetOfLineIndex(N: integer): integer;
    function LineLength(N: integer): integer;
    function OffsetToOffsetOfLineStart(APos: integer): integer;
    function OffsetToOffsetOfLineEnd(APos: integer): integer;
    property Count: integer read GetCount;
    property OnChange: TATStringBufferChange read FOnChange write FOnChange;
  end;

var
  //this var is set by EControl parser thread, when thread gets an exception
  EditorParserExceptionMessage: string;

implementation

{ TATStringBuffer }

procedure TATStringBuffer.SetCount(AValue: integer);
begin
  if AValue<0 then
    raise Exception.Create('StringBuffer Count<0');

  if Length(FList)<>AValue then
    SetLength(FList, AValue);
end;

function TATStringBuffer.GetCount: integer; inline;
begin
  Result:= Length(FList);
end;

constructor TATStringBuffer.Create;
begin
  FText:= '';
  SetCount(0);
  Version:= 0;
  Valid:= false;
end;

destructor TATStringBuffer.Destroy;
begin
  SetCount(0);
  inherited;
end;


procedure TATStringBuffer.Setup(const AText: UnicodeString;
  const ALineLens: array of integer);
var
  Pos, NLen, i: integer;
begin
  FText:= AText;
  Valid:= false;

  SetCount(Length(ALineLens)+1);
  Pos:= 0;
  FList[0]:= 0;
  for i:= 0 to Length(ALineLens)-1 do
  begin
    NLen:= ALineLens[i];
    Inc(Pos, NLen);
    Inc(Pos, LenEOL);
    FList[i+1]:= Pos;
  end;
end;

procedure TATStringBuffer.SetupFromGenericList(L: TATGenericIntList);
var
  Pos, NLen, i: integer;
begin
  SetCount(L.Count+1);
  Pos:= 0;
  FList[0]:= 0;
  for i:= 0 to L.Count-1 do
  begin
    NLen:= L[i];
    Inc(Pos, NLen);
    Inc(Pos, LenEOL);
    FList[i+1]:= Pos;
  end;
end;

procedure TATStringBuffer.ChangeLine(ALine: integer; const SNewLine: UnicodeString);
var
  NPos, NLenOld, NLenNew, NDlt, NLenTail, i: integer;
begin
  //line indexes are 0..Count-2 (Count is the count of offset entries)
  if (ALine<0) or (ALine+1>=Length(FList)) then Exit;

  NPos:= FList[ALine];
  NLenOld:= FList[ALine+1]-FList[ALine]-LenEOL;
  NLenNew:= Length(SNewLine);
  NDlt:= NLenNew-NLenOld;

  //all edits below mutate FText in place: it must not be shared (COW)
  //UniqueString(FText);

  if NDlt<>0 then
  begin
    NLenTail:= Length(FText)-NPos-NLenOld;
    if NDlt>0 then
    begin
      SetLength(FText, Length(FText)+NDlt);
      if NLenTail>0 then
        Move(FText[NPos+NLenOld+1], FText[NPos+NLenOld+1+NDlt], NLenTail*SizeOf(WideChar));
    end
    else
    begin
      if NLenTail>0 then
        Move(FText[NPos+NLenOld+1], FText[NPos+NLenNew+1], NLenTail*SizeOf(WideChar));
      SetLength(FText, Length(FText)+NDlt);
    end;
    for i:= ALine+1 to High(FList) do
      Inc(FList[i], NDlt);
  end;

  if NLenNew>0 then
    Move(PWideChar(Pointer(SNewLine))^, FText[NPos+1], NLenNew*SizeOf(WideChar));
  Inc(Version);
end;


procedure TATStringBuffer.SetupSlow(const AText: UnicodeString);
var
  LenList: TATGenericIntList;
  NLen, i: integer;
begin
  FText:= AText;
  if FText='' then
  begin
    SetCount(0);
    Exit
  end;

  //replace CR LF to LF
  FText:= UnicodeStringReplace(FText, #13#10, #10, [rfReplaceAll]);

  Valid:= false;
  NLen:= 0;

  LenList:= TATGenericIntList.Create;
  try
    for i:= 1 to Length(FText) do
    begin
      //replace CR to LF
      if FText[i]=#13 then
        FText[i]:= #10;

      if FText[i]=#10 then
      begin
        LenList.Add(NLen);
        NLen:= 0;
      end
      else
        Inc(NLen);
    end;

    if NLen>0 then
      LenList.Add(NLen);

    SetupFromGenericList(LenList);
  finally
    FreeAndNil(LenList);
  end;
end;

procedure TATStringBuffer.Assign(Other: TATStringBuffer);
begin
  FText:= Other.FText;
  UniqueString(FText);
  FList:= Other.FList;
  Version:= Other.Version;
end;


procedure TATStringBuffer.Clear;
begin
  FText:= '';
  SetCount(0);
  Valid:= false;
end;

function TATStringBuffer.CaretToStr(const APnt: TPoint): integer;
var
  Len, x: integer;
begin
  Result:= -1;
  x:= APnt.X;
  if (APnt.Y<0) then Exit;
  if (APnt.X<0) then Exit;
  if (APnt.Y>=Count) then Exit;

  //handle caret pos after eol
  if x>0 then
  begin
    Len:= LineLength(APnt.Y);
    if x>Len then
      x:= Len;
  end;
  Result:= FList[APnt.Y]+x;
end;

function TATStringBuffer.StrToCaret(APos: integer): TPoint;
var
  a, b, m, dif: integer;
begin
  Result.Y:= -1;
  Result.X:= 0;
  if APos<=0 then
  begin
    Result.Y:= 0;
    Exit
  end;

  a:= 0;
  b:= Count-1;
  if b<0 then Exit;

  repeat
    m:= (a+b+1) div 2;

    dif:= FList[m]-APos;
    if dif=0 then Break;

    if Abs(a-b)<=1 then
    begin
      m:= a;
      Break
    end;

    if dif>0 then
      b:= m
    else
      a:= m;
  until false;

  Result.Y:= m;
  Result.X:= APos-FList[m];
end;

function TATStringBuffer.SubString(APos, ALen: integer): UnicodeString; inline;
begin
  Result:= Copy(FText, APos, ALen);
end;

function TATStringBuffer.TextLength: integer;
var
  NCnt: integer;
begin
  NCnt:= Count;
  if NCnt>0 then
    Result:= FList[NCnt-1]-LenEOL
  else
    Result:= 0;
end;

function TATStringBuffer.OffsetOfLineIndex(N: integer): integer;
var
  NCnt: integer;
begin
  NCnt:= Count;
  if (N<0) or (NCnt=0) then
    Result:= 0
  else
  if N>=NCnt then
    Result:= FList[NCnt-1]
  else
    Result:= FList[N];
end;

function TATStringBuffer.LineLength(N: integer): integer;
begin
  if (N>=0) and (N+1<Count) then
    Result:= FList[N+1]-FList[N]-LenEOL
  else
    Result:= 0;
end;

function TATStringBuffer.OffsetToOffsetOfLineStart(APos: integer): integer;
begin
  Result:= APos-StrToCaret(APos).X;
end;

function TATStringBuffer.OffsetToOffsetOfLineEnd(APos: integer): integer;
var
  NLine: integer;
begin
  NLine:= StrToCaret(APos).Y;
  Result:= OffsetOfLineIndex(NLine+1)-LenEOL;
end;

end.

