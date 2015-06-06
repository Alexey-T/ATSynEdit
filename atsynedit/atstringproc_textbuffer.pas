unit ATStringProc_TextBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATStringProc;

type
  { TATStringBufferHelper }

  TATStringBufferHelper = class
  private
    FStarts: TList; //contains offsets of lines
    FLenEol: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Setup(ALineLens: TList; ALenEol: integer);
    function CaretToOffset(APnt: TPoint): integer;
    function OffsetToCaret(APos: integer): TPoint;
  end;

implementation

const
  cInitListCapacity = 10000;

{ TATStringBufferHelper }

constructor TATStringBufferHelper.Create;
begin
  FStarts:= TList.Create;
  FStarts.Capacity:= cInitListCapacity;
  FLenEol:= 1;
end;

destructor TATStringBufferHelper.Destroy;
begin
  FStarts.Clear;
  FreeAndNil(FStarts);
  inherited;
end;

procedure TATStringBufferHelper.Setup(ALineLens: TList; ALenEol: integer);
var
  Pos, i: integer;
begin
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

function TATStringBufferHelper.CaretToOffset(APnt: TPoint): integer;
begin
  Result:= -1;
  if (APnt.Y<0) or (APnt.X<0) then Exit;
  if (APnt.Y>=FStarts.Count) then Exit;
  Result:= integer(FStarts[APnt.Y])+APnt.X;
end;

function TATStringBufferHelper.OffsetToCaret(APos: integer): TPoint;
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

end.

