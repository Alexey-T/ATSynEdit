unit ATStringProc_TextBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Dialogs,
  ATStringProc;

type
  TTextChangedEvent = procedure(Sender: TObject; Pos, Count, LineChange: integer) of object;

type
  { TATStringBuffer }

  TATStringBuffer = class
  private
    FStarts: TList; //contains offsets of lines
    FLenEol: integer;
    FOnChange: TTextChangedEvent;
  public
    FText: atString;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Setup(const AText: atString; ALineLens: TList; ALenEol: integer);
    procedure SetupSlow(const AText: atString);
    procedure Clear;
    function CaretToStr(APnt: TPoint): integer;
    function StrToCaret(APos: integer): TPoint;
    function SubString(AFrom, ALen: integer): atString;
    function TextLength: integer;
    function LineIndex(N: integer): integer;
    function LineLength(N: integer): integer;
    function LineSpace(N: integer): integer;
    function OffsetToDistanceFromLineStart(APos: integer): integer;
    function OffsetToDistanceFromLineEnd(APos: integer): integer;
    function OffsetToOffsetOfLineStart(APos: integer): integer;
    function OffsetToOffsetOfLineEnd(APos: integer): integer;
    function Count: integer;
    property OnChange: TTextChangedEvent read FOnChange write FOnChange;
  end;

implementation

const
  cInitListCapacity = 10000;

{ TATStringBuffer }

constructor TATStringBuffer.Create;
begin
  FText:= '';
  FStarts:= TList.Create;
  FStarts.Capacity:= cInitListCapacity;
  FLenEol:= 1;
end;

destructor TATStringBuffer.Destroy;
begin
  FStarts.Clear;
  FreeAndNil(FStarts);
  inherited;
end;

procedure TATStringBuffer.Setup(const AText: atString; ALineLens: TList;
  ALenEol: integer);
var
  Pos, NLen, i: integer;
begin
  FText:= AText;
  FLenEol:= ALenEol;

  FStarts.Clear;
  Pos:= 0;
  FStarts.Add(nil);
  for i:= 0 to ALineLens.Count-1 do
  begin
    NLen:= PtrUInt(ALineLens[i]);
    Inc(Pos, NLen+FLenEol);
    FStarts.Add(Pointer(PtrUInt(Pos)));
  end;
end;

procedure TATStringBuffer.SetupSlow(const AText: atString);
var
  STextFinal: atString;
  L: TStringList;
  Lens: TList;
  i: integer;
begin
  if Trim(AText)='' then
  begin
    FText:= '';
    FStarts.Clear;
    Exit
  end;

  L:= TStringList.Create;
  Lens:= TList.Create;
  try
    L.TextLineBreakStyle:= tlbsLF;
    L.Text:= UTF8Encode(AText);
    STextFinal:= UTF8Decode(L.Text); //this converts eol to LF
    for i:= 0 to L.Count-1 do
      Lens.Add(pointer(Length(UTF8Decode(L[i]))));
    Setup(STextFinal, Lens, 1);
  finally
    FreeAndNil(Lens);
    FreeAndNil(L);
  end;
end;

procedure TATStringBuffer.Clear;
begin
  FText:= '';
  FStarts.Clear;
end;

function TATStringBuffer.CaretToStr(APnt: TPoint): integer;
var
  Len: integer;
begin
  Result:= -1;
  if (APnt.Y<0) then Exit;
  if (APnt.X<0) then Exit;
  if (APnt.Y>=FStarts.Count) then Exit;

  //handle caret pos after eol
  Len:= LineLength(APnt.Y);
  if APnt.X>Len then
    APnt.X:= Len;

  Result:= PtrUInt(FStarts[APnt.Y])+APnt.X;
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
  b:= FStarts.Count-1;
  if b<0 then Exit;

  repeat
    dif:= PtrUInt(FStarts[a])-PtrUInt(APos);
    if dif=0 then begin m:= a; Break end;

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= PtrUInt(FStarts[m])-PtrUInt(APos);
    if dif=0 then Break;

    if Abs(a-b)<=1 then begin m:= a; Break end;
    if dif>0 then b:= m else a:= m;
  until false;

  Result.Y:= m;
  Result.X:= PtrUInt(APos)-PtrUInt(FStarts[Result.Y]);
end;

function TATStringBuffer.SubString(AFrom, ALen: integer): atString;
begin
  Result:= Copy(FText, AFrom, ALen);
end;

function TATStringBuffer.TextLength: integer;
begin
  Result:= Length(FText);
end;

function TATStringBuffer.LineIndex(N: integer): integer;
begin
  if N<0 then Result:= 0
  else
  if N>=FStarts.Count then Result:= TextLength-1
  else
    Result:= PtrUInt(FStarts[N]);
end;

function TATStringBuffer.LineLength(N: integer): integer;
begin
  if N<0 then Result:= 0
  else
  if N>=FStarts.Count-1 then Result:= 0
  else
    Result:= PtrUInt(FStarts[N+1])-PtrUInt(FStarts[N])-FLenEol;
end;

function TATStringBuffer.LineSpace(N: integer): integer;
begin
  Result:= LineLength(N)+FLenEol;
end;

function TATStringBuffer.Count: integer;
begin
  Result:= FStarts.Count;
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

function TATStringBuffer.OffsetToOffsetOfLineStart(APos: integer): integer;
begin
  Result:= APos-OffsetToDistanceFromLineStart(APos);
end;

function TATStringBuffer.OffsetToOffsetOfLineEnd(APos: integer): integer;
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

