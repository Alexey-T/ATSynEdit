{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_TextBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Dialogs,
  LazUTF8,
  ATStringProc;

type
  TTextChangedEvent = procedure(Sender: TObject; Pos, Count, LineChange: integer) of object;

type
  { TATStringBuffer }

  TATStringBuffer = class
  private
    FList: array of integer;
    FCount: integer;
    FLenEol: integer;
    FOnChange: TTextChangedEvent;
    procedure SetCount(AValue: integer);
  public
    FText: UnicodeString;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Setup(const AText: UnicodeString; const ALineLens: array of integer);
    procedure SetupSlow(const AText: UnicodeString);
    function CaretToStr(APnt: TPoint): integer;
    function StrToCaret(APos: integer): TPoint;
    function SubString(APos, ALen: integer): UnicodeString; inline;
    function TextLength: integer; inline;
    function LineIndex(N: integer): integer;
    function LineLength(N: integer): integer;
    function LineSpace(N: integer): integer; inline;
    function OffsetToDistanceFromLineStart(APos: integer): integer;
    function OffsetToDistanceFromLineEnd(APos: integer): integer;
    function OffsetToOffsetOfLineStart(APos: integer): integer; inline;
    function OffsetToOffsetOfLineEnd(APos: integer): integer; inline;
    property Count: integer read FCount;
    property OnChange: TTextChangedEvent read FOnChange write FOnChange;
  end;

implementation

{ TATStringBuffer }

procedure TATStringBuffer.SetCount(AValue: integer);
begin
  if AValue<0 then
    raise Exception.Create('StringBuffer Count<0');

  FCount:= AValue;
  if Length(FList)<>FCount then
    SetLength(FList, FCount);
end;

constructor TATStringBuffer.Create;
begin
  FText:= '';
  FLenEol:= 1; //no apps should use other
  FCount:= 0;
  SetCount(0);
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
  //FLenEol:= ALenEol;

  SetCount(Length(ALineLens)+1);
  Pos:= 0;
  FList[0]:= 0;
  for i:= 0 to Length(ALineLens)-1 do
  begin
    NLen:= ALineLens[i];
    Inc(Pos, NLen+FLenEol);
    FList[i+1]:= Pos;
  end;
end;

procedure TATStringBuffer.SetupSlow(const AText: UnicodeString);
var
  STextFinal: UnicodeString;
  L: TStringList;
  Lens: array of integer;
  i: integer;
begin
  if AText='' then
  begin
    FText:= '';
    SetCount(0);
    Exit
  end;

  L:= TStringList.Create;
  try
    L.TextLineBreakStyle:= tlbsLF;
    L.Text:= UTF8Encode(AText);
    STextFinal:= UTF8Decode(L.Text); //this converts all ends to LF

    SetLength(Lens, L.Count);
    for i:= 0 to L.Count-1 do
      Lens[i]:= UTF8Length(L[i]);

    Setup(STextFinal, Lens);
  finally
    FreeAndNil(L);
  end;
end;

procedure TATStringBuffer.Clear;
begin
  FText:= '';
  SetCount(0);
end;

function TATStringBuffer.CaretToStr(APnt: TPoint): integer;
var
  Len: integer;
begin
  Result:= -1;
  if (APnt.Y<0) then Exit;
  if (APnt.X<0) then Exit;
  if (APnt.Y>=FCount) then Exit;

  //handle caret pos after eol
  if APnt.X>0 then
  begin
    Len:= LineLength(APnt.Y);
    if APnt.X>Len then
      APnt.X:= Len;
  end;

  Result:= FList[APnt.Y]+APnt.X;
end;

function TATStringBuffer.StrToCaret(APos: integer): TPoint;
var
  a, b, m, dif: integer;
begin
  Result.Y:= -1;
  Result.X:= 0;
  if APos<=0 then
    begin Result.Y:= 0; Exit end;

  a:= 0;
  b:= FCount-1;
  if b<0 then Exit;

  repeat
    dif:= FList[a]-APos;
    if dif=0 then begin m:= a; Break end;

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= FList[m]-APos;
    if dif=0 then Break;

    if Abs(a-b)<=1 then begin m:= a; Break end;
    if dif>0 then b:= m else a:= m;
  until false;

  Result.Y:= m;
  Result.X:= APos-FList[Result.Y];
end;

function TATStringBuffer.SubString(APos, ALen: integer): UnicodeString; inline;
begin
  Result:= Copy(FText, APos, ALen);
end;

function TATStringBuffer.TextLength: integer; inline;
begin
  Result:= Length(FText);
end;

function TATStringBuffer.LineIndex(N: integer): integer;
begin
  if N<0 then Result:= 0
  else
  if N>=FCount then Result:= TextLength-1
  else
    Result:= FList[N];
end;

function TATStringBuffer.LineLength(N: integer): integer;
begin
  if N<0 then Result:= 0
  else
  if N>=FCount-1 then Result:= 0
  else
    Result:= FList[N+1]-FList[N]-FLenEol;
end;

function TATStringBuffer.LineSpace(N: integer): integer; inline;
begin
  Result:= LineLength(N)+FLenEol;
end;

(*
//old code, seems it's slower so del'ed
function TATStringBuffer.OffsetToOffsetOfLineStart(APos: integer): integer;
var
  N: integer;
begin
  N:= StrToCaret(APos).Y;
  Result:= LineIndex(N);
end;

function TATStringBuffer.OffsetToOffsetOfLineEnd(APos: integer): integer;
var
  N: integer;
begin
  N:= StrToCaret(APos).Y;
  Result:= LineIndex(N)+LineLength(N);
end;
*)

function TATStringBuffer.OffsetToOffsetOfLineStart(APos: integer): integer; inline;
begin
  Result:= APos-OffsetToDistanceFromLineStart(APos);
end;

function TATStringBuffer.OffsetToOffsetOfLineEnd(APos: integer): integer; inline;
begin
  Result:= APos+OffsetToDistanceFromLineEnd(APos);
end;

function TATStringBuffer.OffsetToDistanceFromLineStart(APos: integer): integer;
const
  CharEol = #10;
var
  NPos, NLen: integer;
begin
  Result:= 0;
  NPos:= APos+1;
  NLen:= TextLength;
  while (NPos>1) and (NPos-1<=NLen) and (FText[NPos-1]<>CharEol) do
  begin
    Inc(Result);
    Dec(NPos);
  end;
end;

function TATStringBuffer.OffsetToDistanceFromLineEnd(APos: integer): integer;
const
  CharEol = #10;
var
  NLen, NPos: integer;
begin
  Result:= 0;
  NPos:= APos+1;
  NLen:= TextLength;
  while (NPos<NLen) and (FText[NPos+1]<>CharEol) do
  begin
    Inc(Result);
    Inc(NPos);
  end;
end;


end.

