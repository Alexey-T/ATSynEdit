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
  ATStringProc,
  ATSynEdit_FGL;

type
  TTextChangedEvent = procedure(Sender: TObject; Pos, Count, LineChange: integer) of object;

type
  TATGenericIntList = specialize TFPGList<integer>;

type
  { TATStringBuffer }

  TATStringBuffer = class
  strict private
    FList: array of integer;
    FCount: integer;
    FLenEol: integer;
    FLocked: boolean;
    FVersion: integer;
    FOnChange: TTextChangedEvent;
    procedure SetCount(AValue: integer);
    procedure SetupFromGenericList(L: TATGenericIntList);
  public
    FText: UnicodeString;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Setup(const AText: UnicodeString; const ALineLens: array of integer);
    procedure SetupSlow(const AText: UnicodeString);
    procedure Lock;
    procedure Unlock;
    procedure Assign(Other: TATStringBuffer);
    function CaretToStr(constref APnt: TPoint): integer;
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
    property Version: integer read FVersion;
    property IsLocked: boolean read FLocked;
  end;

implementation

{ TATStringBuffer }

procedure TATStringBuffer.SetCount(AValue: integer);
begin
  Assert(not FLocked, 'SetCount called for locked StringBuffer');
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
  FVersion:= 0;
  FLocked:= false;
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
  Assert(not FLocked, 'Attempt to change locked StringBuffer');
  Inc(FVersion);
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

procedure TATStringBuffer.SetupFromGenericList(L: TATGenericIntList);
var
  Pos, NLen, i: integer;
begin
  Assert(not FLocked, 'Attempt to change locked StringBuffer');
  Inc(FVersion);
  SetCount(L.Count+1);
  Pos:= 0;
  FList[0]:= 0;
  for i:= 0 to L.Count-1 do
  begin
    NLen:= L[i];
    Inc(Pos, NLen+FLenEol);
    FList[i+1]:= Pos;
  end;
end;


procedure TATStringBuffer.SetupSlow(const AText: UnicodeString);
var
  Lens: TATGenericIntList;
  i, N: integer;
begin
  Assert(not FLocked, 'Attempt to change locked StringBuffer');
  Inc(FVersion);

  FText:= AText;
  if FText='' then
  begin
    SetCount(0);
    Exit
  end;

  N:= 0;
  i:= 1;

  Lens:= TATGenericIntList.Create;
  try
    while i<=Length(FText) do
    begin
      //Replace CR LF and CR to LF
      if FText[i]=#13 then
      begin
        if (i<Length(FText)) and (FText[i+1]=#10) then
        begin
          Delete(FText, i, 1);
          Continue;
        end
        else
          FText[i]:= #10;
      end;

      if FText[i]=#10 then
      begin
        Lens.Add(N);
        N:= 0;
      end
      else
        Inc(N);
      Inc(i);
    end;

    if N>0 then
      Lens.Add(N);

    SetupFromGenericList(Lens);
  finally
    FreeAndNil(Lens);
  end;
end;

procedure TATStringBuffer.Lock;
begin
  FLocked:= true;
end;

procedure TATStringBuffer.Unlock;
begin
  FLocked:= false;
end;

procedure TATStringBuffer.Assign(Other: TATStringBuffer);
begin
  FText:= Other.FText;
  UniqueString(FText);
  FList:= Other.FList;
  FCount:= Other.FCount;
  FLenEol:= Other.FLenEol;
  FVersion:= Other.FVersion;
end;


procedure TATStringBuffer.Clear;
begin
  Assert(not FLocked, 'Attempt to clear locked StringBuffer');
  FText:= '';
  SetCount(0);
end;

function TATStringBuffer.CaretToStr(constref APnt: TPoint): integer;
var
  Len,x: integer;
begin
  Result:= -1;
  x:=APnt.X;
  if (APnt.Y<0) then Exit;
  if (APnt.X<0) then Exit;
  if (APnt.Y>=FCount) then Exit;

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
  b:= FCount-1;
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
  while (NPos<NLen) and (FText[NPos+1]<>CharEol) do  begin
    Inc(Result);
    Inc(NPos);
  end;
end;


end.

