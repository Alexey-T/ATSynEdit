unit atstringproc_textbuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, atStringProc;

type
  { TATStringBuffer }

  TATStringBuffer = class
  private
    FStarts: TList;
    FText: atString;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetText(const AValue: atString; AListLen: TList);
    procedure CaretPosToStrPos(ALine, ACol: integer; out APos: integer);
    procedure StrPosToCaretPos(APos: integer; out ALine, ACol: integer);
  end;

implementation

const
  cInitListCapacity = 10000;
  cCharEol = #10;
  cLenEol = 1;

{ TATStringBuffer }

procedure TATStringBuffer.SetText(const AValue: atString; AListLen: TList);
var
  i, pos: integer;
begin
  FText:= AValue;
  FStarts.Clear;

  pos:= 0;
  for i:= 0 to AListLen.Count-1 do
  begin
    FStarts.Add(pointer(pos));
    Inc(pos, integer(AListLen[i])+cLenEol);
  end;
end;

procedure TATStringBuffer.CaretPosToStrPos(ALine, ACol: integer; out APos: integer);
var
  i: integer;
begin
  APos:= -1;
  if (ALine<0) or (ACol<0) then Exit;
  if (ALine>=FStarts.Count) then Exit;
  APos:= integer(FStarts[ALine])+ACol;
end;

procedure TATStringBuffer.StrPosToCaretPos(APos: integer; out ALine, ACol: integer);
var
  i: integer;
begin
  ALine:= -1;
  ACol:= 0;
  if APos<0 then Exit;
  if APos=0 then
    begin ALine:= 0; ACol:= 0; Exit end;

  for i:= 1 to FStarts.Count-1 do
    if integer(FStarts[i])>APos then
    begin
      ALine:= i-1;
      ACol:= APos-integer(FStarts[ALine]);
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

