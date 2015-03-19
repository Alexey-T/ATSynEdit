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
    function IsJoinNeeded(N1, N2: integer;
      out OutPosX, OutPosY, OutEndX, OutEndY: integer): boolean;
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
    function IndexOfPosXY(APosX, APosY: integer; AUseEndXY: boolean= false): integer;
    function IndexOfPosYAvg(APosY: integer): integer;
    function IndexOfLeftRight(ALeft: boolean): integer;
    function IsLineListed(APosY: integer): boolean;
    function CaretAtEdge(AEdge: TATSynCaretEdge): TPoint;
    procedure ExtendSelectionToPoint(NIndex: integer; AX, AY: integer);
  end;

function IsPosSorted(X1, Y1, X2, Y2: integer; AllowEq: boolean): boolean;
procedure SwapInt(var n1, n2: integer);


implementation

uses
  Math;

procedure SwapInt(var n1, n2: integer);
var
  n: integer;
begin
  n:= n1;
  n1:= n2;
  n2:= n;
end;

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
  Item.EndX:= -1;
  Item.EndY:= -1;
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
  OutPosX, OutPosY, OutEndX, OutEndY: integer;
begin
  for i:= Count-1 downto 1 do
  begin
    Item1:= GetItem(i);
    Item2:= GetItem(i-1);

    if (Item1.PosY=Item2.PosY) and (Item1.PosX=Item2.PosX) then
      Delete(i);

    if IsJoinNeeded(i, i-1, OutPosX, OutPosY, OutEndX, OutEndY) then
    begin
      Delete(i);
      Item2.PosX:= OutPosX;
      Item2.PosY:= OutPosY;
      Item2.EndX:= OutEndX;
      Item2.EndY:= OutEndY;
    end;
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

function TATSynCarets.IndexOfPosXY(APosX, APosY: integer; AUseEndXY: boolean = false): integer;
var
  iStart, i: integer;
  Item: TATSynCaretItem;
  XUse, YUse: integer;
begin
  Result:= -1;

  iStart:= 0;
  {//don't work
  iStart:= IndexOfPosYAvg(APosY);
  if iStart<0 then Exit;
  }

  for i:= iStart to FList.Count-1 do
  begin
    Item:= TATSynCaretItem(FList[i]);

    if Item.EndY<0 then
      AUseEndXY:= false;
    if AUseEndXY then
      begin XUse:= Item.EndX; YUse:= Item.EndY; end
    else
      begin XUse:= Item.PosX; YUse:= Item.PosY; end;

    if (YUse>APosY) then Break;
    if (XUse=APosX) and (YUse=APosY) then
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

procedure TATSynCarets.ExtendSelectionToPoint(NIndex: integer; AX, AY: integer);
var
  Item: TATSynCaretItem;
begin
  if not IsIndexValid(NIndex) then Exit;
  Item:= Items[NIndex];

  if Item.EndX<0 then Item.EndX:= Item.PosX;
  if Item.EndY<0 then Item.EndY:= Item.PosY;
  Item.PosX:= AX;
  Item.PosY:= AY;
end;

function IsPosSorted(X1, Y1, X2, Y2: integer; AllowEq: boolean): boolean;
begin
  if Y1<>Y2 then
    Result:= Y1<Y2
  else
    Result:= (X1<X2) or (AllowEq and (X1=X2));
end;

function TATSynCarets.IsJoinNeeded(N1, N2: integer;
  out OutPosX, OutPosY, OutEndX, OutEndY: integer): boolean;
var
  Item1, Item2: TATSynCaretItem;
  XMin1, XMin2, YMin1, YMin2, XMax1, XMax2, YMax1, YMax2: integer;
  bSorted: boolean;
begin
  Result:= false;
  if not IsIndexValid(N1) then Exit;
  if not IsIndexValid(N2) then Exit;
  Item1:= Items[N1];
  Item2:= Items[N2];

  if Item1.EndY<0 then Exit;
  if Item2.EndY<0 then Exit;

  bSorted:= IsPosSorted(Item1.PosX, Item1.PosY, Item1.EndX, Item1.EndY, true);
  if bSorted then
  begin
    XMin1:= Item1.PosX;
    YMin1:= Item1.PosY;
    XMax1:= Item1.EndX;
    YMax1:= Item1.EndY;
  end
  else
  begin
    XMin1:= Item1.EndX;
    YMin1:= Item1.EndY;
    XMax1:= Item1.PosX;
    YMax1:= Item1.PosY;
  end;

  bSorted:= IsPosSorted(Item2.PosX, Item2.PosY, Item2.EndX, Item2.EndY, true);
  if bSorted then
  begin
    XMin2:= Item2.PosX;
    YMin2:= Item2.PosY;
    XMax2:= Item2.EndX;
    YMax2:= Item2.EndY;
  end
  else
  begin
    XMin2:= Item2.EndX;
    YMin2:= Item2.EndY;
    XMax2:= Item2.PosX;
    YMax2:= Item2.PosY;
  end;

  OutPosX:= IfThen(XMin1<XMin2, XMin1, XMin2);
  OutPosY:= IfThen(XMin1<XMin2, YMin1, YMin2);
  OutEndX:= IfThen(XMin1<XMin2, XMax2, XMax1);
  OutEndY:= IfThen(XMin1<XMin2, YMax2, YMax1);

  if not bSorted then
  begin
    SwapInt(OutPosX, OutEndX);
    SwapInt(OutPosY, OutEndY);
  end;

  if IsPosSorted(XMax1, YMax1, XMin2, YMin2, false) then Exit; //ranges not overlap [x1, y1]...[x2, y2]
  if IsPosSorted(XMax2, YMax2, XMin1, YMin1, false) then Exit; //ranges not overlap [x2, y2]...[x1, y1]

  Result:= true;
end;


end.

