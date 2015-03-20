unit ATSynCarets;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  { TATCaretItem }

  TATCaretItem = class
  private
  public
    PosX, PosY,
    EndX, EndY: integer;
    CoordX, CoordY: integer;
    procedure SelectToPoint(AX, AY: integer);
    procedure GetRange(out AX1, AY1, AX2, AY2: integer; out ASel: boolean);
  end;

type
  TATCaretEdge = (
    cEdgeTop,
    cEdgeBottom,
    cEdgeLeft,
    cEdgeRight
    );

type
  { TATCarets }

  TATCarets = class
  private
    FList: TList;
    function GetItem(N: integer): TATCaretItem;
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
    property Items[N: integer]: TATCaretItem read GetItem; default;
    procedure Add(APosX, APosY: integer);
    procedure Sort;
    procedure Assign(Obj: TATCarets);
    function IndexOfPosXY(APosX, APosY: integer; AUseEndXY: boolean= false): integer;
    function IndexOfPosYAvg(APosY: integer): integer;
    function IndexOfLeftRight(ALeft: boolean): integer;
    function IsLineListed(APosY: integer): boolean;
    function IsSelection: boolean;
    function CaretAtEdge(AEdge: TATCaretEdge): TPoint;
    function DebugText: string;
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

{ TATCaretItem }

procedure TATCaretItem.GetRange(out AX1, AY1, AX2, AY2: integer; out ASel: boolean);
begin
  AX1:= PosX;
  AY1:= PosY;
  AX2:= EndX;
  AY2:= EndY;
  ASel:= false;

  if (AX2<0) or (AY2<0) then Exit;
  if (AX1=AX2) and (AY1=AY2) then Exit;

  ASel:= true;
  if IsPosSorted(AX2, AY2, AX1, AY1, false) then
  begin
    SwapInt(AX1, AX2);
    SwapInt(AY1, AY2);
  end;
end;

procedure TATCaretItem.SelectToPoint(AX, AY: integer);
begin
  if EndX<0 then EndX:= PosX;
  if EndY<0 then EndY:= PosY;
  PosX:= AX;
  PosY:= AY;
end;

{ TATCarets }

function TATCarets.GetItem(N: integer): TATCaretItem;
begin
  if IsIndexValid(N) then
    Result:= TATCaretItem(FList[N])
  else
    Result:= nil;
end;

constructor TATCarets.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATCarets.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATCarets.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    Delete(i);
end;

procedure TATCarets.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

function TATCarets.Count: integer;
begin
  Result:= FList.Count;
end;

function TATCarets.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

procedure TATCarets.Add(APosX, APosY: integer);
var
  Item: TATCaretItem;
begin
  Item:= TATCaretItem.Create;
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
  Obj1, Obj2: TATCaretItem;
begin
  Obj1:= TATCaretItem(Item1);
  Obj2:= TATCaretItem(Item2);
  Result:= Obj1.PosY-Obj2.PosY;
  if Result=0 then
    Result:= Obj1.PosX-Obj2.PosX;
end;

procedure TATCarets.Sort;
begin
  FList.Sort(_ListCaretsCompare);
  DeleteDups;
end;

procedure TATCarets.DeleteDups;
var
  i: integer;
  Item1, Item2: TATCaretItem;
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


procedure TATCarets.Assign(Obj: TATCarets);
var
  i: integer;
begin
  Clear;
  for i:= 0 to Obj.Count-1 do
    Add(Obj[i].PosX, Obj[i].PosY);
end;

function TATCarets.IndexOfPosXY(APosX, APosY: integer; AUseEndXY: boolean = false): integer;
var
  iStart, i: integer;
  Item: TATCaretItem;
  XUse, YUse: integer;
begin
  Result:= -1;

  iStart:= 0;
  //todo--fix for case called from TimerScrollTick, dont work for cScrollUp
  //iStart:= IndexOfPosYAvg(APosY);
  //if iStart<0 then Exit;

  for i:= iStart to Count-1 do
  begin
    Item:= Items[i];

    if AUseEndXY and (Item.EndY>=0) then
      begin XUse:= Item.EndX; YUse:= Item.EndY; end
    else
      begin XUse:= Item.PosX; YUse:= Item.PosY; end;

    if (YUse>APosY) then Break;
    if (XUse=APosX) and (YUse=APosY) then
      begin Result:= i; Break end;
  end;
end;

//todo-- binary search
function TATCarets.IndexOfPosYAvg(APosY: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to FList.Count-1 do
    if TATCaretItem(FList[i]).PosY>=APosY then
      begin Result:= i; Exit end;
end;

function TATCarets.IndexOfLeftRight(ALeft: boolean): integer;
var
  Item: TATCaretItem;
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

function TATCarets.IsLineListed(APosY: integer): boolean;
var
  i: integer;
  Item: TATCaretItem;
begin
  Result:= false;
  for i:= 0 to FList.Count-1 do
  begin
    Item:= TATCaretItem(FList[i]);
    if Item.PosY=APosY then
    begin
      Result:= true;
      Exit
    end;
  end;
end;

function TATCarets.IsSelection: boolean;
var
  Item: TATCaretItem;
  i: integer;
begin
  Result:= false;
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    if (Item.EndX<0) or (Item.EndY<0) then Continue;
    if (Item.PosX<>Item.EndX) or (Item.PosY<>Item.EndY) then
      begin Result:= true; Exit end;
  end;
end;


function TATCarets.CaretAtEdge(AEdge: TATCaretEdge): TPoint;
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

function IsPosSorted(X1, Y1, X2, Y2: integer; AllowEq: boolean): boolean;
begin
  if Y1<>Y2 then
    Result:= Y1<Y2
  else
    Result:= (X1<X2) or (AllowEq and (X1=X2));
end;

function TATCarets.IsJoinNeeded(N1, N2: integer;
  out OutPosX, OutPosY, OutEndX, OutEndY: integer): boolean;
var
  Item1, Item2: TATCaretItem;
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

function TATCarets.DebugText: string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Count-1 do
    with Items[i] do
      Result:= Result+Format('caret[%d] pos %d:%d end %d:%d', [
        i, posy, posx, endy, endx
        ])+sLineBreak;
end;

end.

