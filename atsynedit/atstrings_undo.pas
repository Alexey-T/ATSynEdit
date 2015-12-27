unit ATStrings_Undo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  ATStringProc;

type
  TATEditAction = (
    cEditActionChange,
    cEditActionChangeEol,
    cEditActionInsert,
    cEditActionDelete,
    cEditActionClearModified
    );

const
  StrEditActionDescriptions: array[TATEditAction] of string = (
    'change',
    'change-eol',
    'insert',
    'delete',
    'clear-mod'
    );

type
  { TATUndoItem }

  TATUndoItem = class
    ItemAction: TATEditAction;
    ItemIndex: integer;
    ItemText: atString;
    ItemEnd: TATLineEnds;
    ItemCarets: TATPointArray;
    ItemSoftMark: boolean;
    ItemHardMark: boolean;
    constructor Create(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds; ASoftMark, AHardMark: boolean;
      const ACarets: TATPointArray); virtual;
  end;

type
  { TATUndoList }

  TATUndoList = class
  private
    FList: TList;
    FMaxCount: integer;
    FLocked: boolean;
    FSoftMark: boolean;
    FHardMark: boolean;
    function GetItem(N: integer): TATUndoItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsIndexValid(N: integer): boolean;
    function IsItemsEqual(N1, N2: integer): boolean;
    function Count: integer;
    function Last: TATUndoItem;
    property Items[N: integer]: TATUndoItem read GetItem; default;
    property MaxCount: integer read FMaxCount write FMaxCount;
    property SoftMark: boolean read FSoftMark write FSoftMark;
    property HardMark: boolean read FHardMark write FHardMark;
    property Locked: boolean read FLocked write FLocked;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteLast;
    procedure Add(AAction: TATEditAction; AIndex: integer; const AText: atString;
      AEnd: TATLineEnds; const ACarets: TATPointArray);
    procedure AddUnmodifiedMark;
    procedure DebugShow;
  end;


implementation

uses
  Math, Dialogs;

{ TATUndoItem }

constructor TATUndoItem.Create(AAction: TATEditAction; AIndex: integer;
  const AText: atString; AEnd: TATLineEnds; ASoftMark, AHardMark: boolean;
  const ACarets: TATPointArray);
var
  i: integer;
begin
  ItemAction:= AAction;
  ItemIndex:= AIndex;
  ItemText:= AText;
  ItemEnd:= AEnd;
  ItemSoftMark:= ASoftMark;
  ItemHardMark:= AHardMark;

  SetLength(ItemCarets, Length(ACarets));
  for i:= 0 to High(ACarets) do
  begin
    ItemCarets[i]:= ACarets[i];
  end;
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
  FSoftMark:= false;
  FHardMark:= false;
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

function TATUndoList.IsItemsEqual(N1, N2: integer): boolean;
var
  i1, i2: TATUndoItem;
begin
  Result:= false;
  i1:= Items[N1];
  i2:= Items[N2];
  if not Assigned(i1) or not Assigned(i2) then Exit;
  Result:=
    (i1.ItemAction=cEditActionChange) and
    (i1.ItemAction=i2.ItemAction) and
    (i1.ItemIndex=i2.ItemIndex) and
    (i1.ItemText=i2.ItemText);
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

procedure TATUndoList.Add(AAction: TATEditAction; AIndex: integer;
  const AText: atString; AEnd: TATLineEnds;
  const ACarets: TATPointArray);
var
  Item: TATUndoItem;
begin
  if FLocked then Exit;

  //not dup change?
  if (Count>0) and (AAction in [cEditActionChange, cEditActionChangeEol]) then
  begin
    Item:= Items[Count-1];
    if (Item.ItemAction=AAction) and
      (Item.ItemIndex=AIndex) and
      (Item.ItemText=AText) then
        Exit;
  end;

  //not insert/delete same index?
  if (Count>0) and (AAction=cEditActionDelete) then
  begin
    Item:= Items[Count-1];
    if (Item.ItemAction=cEditActionInsert) and
      (Item.ItemIndex=AIndex) then
      begin
        DeleteLast;
        Exit
      end;
  end;

  Item:= TATUndoItem.Create(AAction, AIndex, AText, AEnd, FSoftMark, FHardMark, ACarets);
  FList.Add(Item);
  FSoftMark:= false;

  while Count>MaxCount do
    Delete(0);
end;


procedure TATUndoList.AddUnmodifiedMark;
var
  Item: TATUndoItem;
  Carets: TATPointArray;
begin
  if FLocked then exit;

  //don't do two marks
  Item:= Last;
  if Assigned(Item) then
    if Item.ItemAction=cEditActionClearModified then exit;

  SetLength(Carets, 0);
  Item:= TATUndoItem.Create(cEditActionClearModified, 0, '', cEndNone, false, false, Carets);
  FList.Add(Item);
end;


procedure TATUndoList.DebugShow;
var
  i: integer;
  s: string;
  Item: TATUndoItem;
begin
  s:= '';
  for i:= 0 to Min(40, Count)-1 do
  begin
    Item:= Items[i];
    s:= s+Format('%s, text "%s", %s, index %d', [
      StrEditActionDescriptions[Item.ItemAction],
      UTF8Encode(Item.ItemText),
      IfThen(Item.ItemEnd=cEndNone, 'no-eol', ''),
      Item.ItemIndex
      ])+#13;
  end;
  ShowMessage('Undo list:'#13+s);
end;

function TATUndoList.Last: TATUndoItem;
begin
  if Count>0 then
    Result:= Items[Count-1]
  else
    Result:= nil;
end;

end.

