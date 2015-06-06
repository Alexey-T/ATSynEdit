unit ATStringProc_TextBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATStringProc;

type
  { TATStringBuffer }

  TATStringBuffer = class
  private
    FStarts: TList; //contains offsets of lines from FText
    FText: atString;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetText(const AText: atString; ALineLens: TList);
    function CaretToOffset(APnt: TPoint): integer;
    function OffsetToCaret(APos: integer): TPoint;
  end;

implementation

const
  cInitListCapacity = 5000;
  cLenEol = 1;

{ TATStringBuffer }

procedure TATStringBuffer.SetText(const AText: atString; ALineLens: TList);
var
  i, pos: integer;
begin
  FText:= AText;
  FStarts.Clear;

  pos:= 0;
  for i:= 0 to ALineLens.Count-1 do
  begin
    FStarts.Add(pointer(pos));
    Inc(pos, integer(ALineLens[i])+cLenEol);
  end;
end;

function TATStringBuffer.CaretToOffset(APnt: TPoint): integer;
begin
  Result:= -1;
  if (APnt.Y<0) or (APnt.X<0) then Exit;
  if (APnt.Y>=FStarts.Count) then Exit;
  Result:= integer(FStarts[APnt.Y])+APnt.X;
end;

function TATStringBuffer.OffsetToCaret(APos: integer): TPoint;
var
  i: integer;
begin
  Result.Y:= -1;
  Result.X:= 0;
  if APos<=0 then
    begin Result.Y:= 0; Exit end;

  for i:= 1{!} to FStarts.Count-1 do
    if integer(FStarts[i])>APos then
    begin
      Result.Y:= i-1;
      Result.X:= APos-integer(FStarts[Result.Y]);
      Exit
    end;
end;

constructor TATStringBuffer.Create;
begin
  FStarts:= TList.Create;
  FStarts.Capacity:= cInitListCapacity;
  FText:= '';
end;

destructor TATStringBuffer.Destroy;
begin
  FStarts.Clear;
  FreeAndNil(FStarts);

  inherited;
end;

end.

