unit ATStringProc_TextBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATStringProc;

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
    procedure Clear;
    function CaretToStr(APnt: TPoint): integer;
    function StrToCaret(APos: integer): TPoint;
    function SubString(AFrom, ALen: integer): atString;
    function TextLength: integer;
    function LineIndex(N: integer): integer;
    function LineLength(N: integer): integer;
    function LineSpace(N: integer): integer;
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
  Pos, i: integer;
begin
  FText:= AText;
  FLenEol:= ALenEol;

  FStarts.Clear;
  Pos:= 0;
  FStarts.Add(nil);
  for i:= 0 to ALineLens.Count-1 do
  begin
    Inc(Pos, integer(ALineLens[i])+FLenEol);
    FStarts.Add(pointer(Pos));
  end;
end;

procedure TATStringBuffer.Clear;
begin
  FText:= '';
  FStarts.Clear;
end;

function TATStringBuffer.CaretToStr(APnt: TPoint): integer;
begin
  Result:= -1;
  if (APnt.Y<0) or (APnt.X<0) then Exit;
  if (APnt.Y>=FStarts.Count) then Exit;
  Result:= integer(FStarts[APnt.Y])+APnt.X;
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
    dif:= integer(FStarts[a])-APos;
    if dif=0 then begin m:= a; Break end;

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= integer(FStarts[m])-APos;
    if dif=0 then Break;

    if Abs(a-b)<=1 then begin m:= a; Break end;
    if dif>0 then b:= m else a:= m;
  until false;

  Result.Y:= m;
  Result.X:= APos-integer(FStarts[Result.Y]);
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
    Result:= integer(FStarts[N]);
end;

function TATStringBuffer.LineLength(N: integer): integer;
begin
  if N<0 then Result:= 0
  else
  if N>=FStarts.Count-1 then Result:= 0
  else
    Result:= integer(FStarts[N+1])-integer(FStarts[N])-FLenEol;
end;

function TATStringBuffer.LineSpace(N: integer): integer;
begin
  Result:= LineLength(N)+FLenEol;
end;

function TATStringBuffer.Count: integer;
begin
  Result:= FStarts.Count;
end;

end.

