unit ATSynCarets;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TATSynCaretItem = class
    PosX, PosY,
    CoordX, CoordY,
    EndX, EndY: integer;
  end;

type
  TATSynCaretEdge = (
    cEdgeTop,
    cEdgeBottom,
    cEdgeLeft,
    cEdgeRight
    );

type

  { TATSynCarets }

  TATSynCarets = class
  private
    FList: TList;
    function GetItem(N: integer): TATSynCaretItem;
    procedure DeleteDups;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATSynCaretItem read GetItem; default;
    procedure Add(APosX, APosY: integer);
    procedure Sort;
    procedure Assign(Obj: TATSynCarets);
    function IndexOfPosXY(APosX, APosY: integer): integer;
    function IndexOfPosYAvg(APosY: integer): integer;
    function IndexOfLeftRight(ALeft: boolean): integer;
    function IsLineListed(APosY: integer): boolean;
    function CaretAtEdge(AEdge: TATSynCaretEdge): TPoint;
  end;


implementation

{ TATSynCarets }

function TATSynCarets.GetItem(N: integer): TATSynCaretItem;
begin
  if IsIndexValid(N) then
    Result:= TATSynCaretItem(FList[N])
  else
    Result:= nil;
end;

constructor TATSynCarets.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATSynCarets.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATSynCarets.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    Delete(i);
end;

procedure TATSynCarets.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

function TATSynCarets.Count: integer;
begin
  Result:= FList.Count;
end;

function TATSynCarets.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

procedure TATSynCarets.Add(APosX, APosY: integer);
var
  Item: TATSynCaretItem;
begin
  Item:= TATSynCaretItem.Create;
  Item.PosX:= APosX;
  Item.PosY:= APosY;
  Item.CoordX:= -1;
  Item.CoordY:= -1;
  FList.Add(Item);
end;

function _ListCaretsCompare(Item1, Item2: Pointer): Integer;
var
  Obj1, Obj2: TATSynCaretItem;
begin
  Obj1:= TATSynCaretItem(Item1);
  Obj2:= TATSynCaretItem(Item2);
  Result:= Obj1.PosY-Obj2.PosY;
  if Result=0 then
    Result:= Obj1.PosX-Obj2.PosX;
end;

procedure TATSynCarets.Sort;
begin
  FList.Sort(_ListCaretsCompare);
  DeleteDups;
end;

procedure TATSynCarets.DeleteDups;
var
  i: integer;
  Item1, Item2: TATSynCaretItem;
begin
  for i:= Count-1 downto 1 do
  begin
    Item1:= GetItem(i);
    Item2:= GetItem(i-1);
    if (Item1.PosY=Item2.PosY) and (Item1.PosX=Item2.PosX) then
      Delete(i);
  end;
end;


procedure TATSynCarets.Assign(Obj: TATSynCarets);
var
  i: integer;
begin
  Clear;
  for i:= 0 to Obj.Count-1 do
    Add(Obj[i].PosX, Obj[i].PosY);
end;

function TATSynCarets.IndexOfPosXY(APosX, APosY: integer): integer;
var
  iStart, i: integer;
  Item: TATSynCaretItem;
begin
  Result:= -1;
  iStart:= IndexOfPosYAvg(APosY);
  if iStart<0 then Exit;
  for i:= iStart to FList.Count-1 do
  begin
    Item:= TATSynCaretItem(FList[i]);
    if (Item.PosY>APosY) then Break;
    if (Item.PosX=APosX) and (Item.PosY=APosY) then
      begin Result:= i; Break end;
  end;
end;

//todo-- binary search
function TATSynCarets.IndexOfPosYAvg(APosY: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to FList.Count-1 do
    if TATSynCaretItem(FList[i]).PosY>=APosY then
      begin Result:= i; Exit end;
end;

function TATSynCarets.IndexOfLeftRight(ALeft: boolean): integer;
var
  Item: TATSynCaretItem;
  i, NPos: integer;
  Upd: boolean;
begin
  Result:= -1;
  if Count>0 then
    NPos:= Items[0].PosX;
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    if ALeft then
      Upd:= Item.PosX<=NPos
    else
      Upd:= Item.PosX>=NPos;
    if Upd then
    begin
      Result:= i;
      NPos:= Item.PosX;
    end;
  end;
end;

function TATSynCarets.IsLineListed(APosY: integer): boolean;
var
  i: integer;
  Item: TATSynCaretItem;
begin
  Result:= false;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= TATSynCaretItem(FList[i]);
    if Item.PosY=APosY then
    begin
      Result:= true;
      Exit
    end;
  end;
end;


function TATSynCarets.CaretAtEdge(AEdge: TATSynCaretEdge): TPoint;
var
  N: integer;
begin
  Result:= Point(0, 0);
  case AEdge of
    cEdgeTop: N:= 0;
    cEdgeBottom: N:= Count-1;
    cEdgeLeft: N:= IndexOfLeftRight(true);
    cEdgeRight: N:= IndexOfLeftRight(false);
  end;
  if IsIndexValid(N) then
    with Items[N] do
      Result:= Point(PosX, PosY);
end;


end.

