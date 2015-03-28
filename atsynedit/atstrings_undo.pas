unit ATStrings_Undo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATStringProc;

type
  TATUndoAction = (
    cUndoActionDelete,
    cUndoActionChange,
    cUndoActionInsert
    );

type
  { TATUndoItem }

  TATUndoItem = class
    ItemAction: TATUndoAction;
    ItemIndex: integer;
    ItemWithEnd: boolean;
    ItemText: atString;
    constructor Create(AAction: TATUndoAction; AIndex: integer;
      const AText: atString; AWithEnd: boolean); virtual;
  end;

type
  { TATUndoList }

  TATUndoList = class
  private
    FList: TList;
    FMaxCount: integer;
    FLocked: boolean;
    function GetItem(N: integer): TATUndoItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsIndexValid(N: integer): boolean;
    function Count: integer;
    function Last: TATUndoItem;
    property Items[N: integer]: TATUndoItem read GetItem; default;
    property MaxCount: integer read FMaxCount write FMaxCount;
    property Locked: boolean read FLocked write FLocked;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteLast;
    procedure Add(AAction: TATUndoAction; AIndex: integer; const AText: atString;
      AWithEnd: boolean);
  end;


implementation

{ TATUndoItem }

constructor TATUndoItem.Create(AAction: TATUndoAction; AIndex: integer;
  const AText: atString; AWithEnd: boolean);
begin
  ItemAction:= AAction;
  ItemIndex:= AIndex;
  ItemText:= AText;
  ItemWithEnd:= AWithEnd;
end;

{ TATUndoList }

function TATUndoList.GetItem(N: integer): TATUndoItem;
begin
  if IsIndexValid(N) then
    Result:= TATUndoItem(FList[N])
  else
    Result:= nil;
end;

constructor TATUndoList.Create;
begin
  FList:= TList.Create;
  FMaxCount:= 5000;
  FLocked:= false;
end;

destructor TATUndoList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATUndoList.Count: integer;
begin
  Result:= FList.Count;
end;

function TATUndoList.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<Count);
end;

procedure TATUndoList.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

procedure TATUndoList.DeleteLast;
begin
  Delete(Count-1);
end;

procedure TATUndoList.Clear;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    Delete(i);
end;

procedure TATUndoList.Add(AAction: TATUndoAction; AIndex: integer; const AText: atString; AWithEnd: boolean);
var
  Item: TATUndoItem;
begin
  if FLocked then Exit;

  Item:= TATUndoItem.Create(AAction, AIndex, AText, AWithEnd);
  FList.Add(Item);

  while Count>MaxCount do
    Delete(0);
end;

function TATUndoList.Last: TATUndoItem;
begin
  Result:= Items[Count-1];
end;

end.

