{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Carets;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  LCLIntf,
  ATStringProc,
  ATStringProc_Separator;

type
  TATPosRelation = (
    cRelateBefore,
    cRelateInside,
    cRelateAfter
    );

  TATRangeSelection = (
    cRangeAllSelected,
    cRangeAllUnselected,
    cRangePartlySelected
    );

  TATCaretMemoryAction = (
    cCaretMem_PrepareX,
    cCaretMem_SaveX,
    cCaretMem_ClearX
    );

procedure SwapInt(var n1, n2: integer); inline;
function IsPosSorted(X1, Y1, X2, Y2: integer; AllowEq: boolean): boolean; inline;
function IsPosInRange(X, Y, X1, Y1, X2, Y2: integer; AllowOnRightEdge: boolean=false): TATPosRelation;


type
  { TATCaretItem }

  TATCaretItem = class
  public
    PosX, PosY, //caret text position
    EndX, EndY: integer; //end of selection, or (-1,-1) if no selection
    CoordX, CoordY: integer; //screen coords
    OldRect: TRect; //screen rect, but before running the last command
    SavedX, SavedX_Pre: integer; //memory of last column, to use with arrows Up/Down
    procedure SelectNone;
    procedure SelectToPoint(AX, AY: integer);
    procedure GetRange(out AX1, AY1, AX2, AY2: integer; out ASel: boolean);
    procedure GetSelLines(out AFrom, ATo: integer; AllowNoSel: boolean=false);
    function Change(APosX, APosY, AEndX, AEndY: integer): boolean;
    procedure SwapSelection;
    function IsForwardSelection: boolean;
    function IsMultilineSelection: boolean;
    function FirstTouchedLine: integer;
    procedure UpdateMemory(AMode: TATCaretMemoryAction; AArrowUpDown: boolean);
  end;

type
  TATCaretEdge = (
    cEdgeTop,
    cEdgeBottom,
    cEdgeLeft,
    cEdgeRight
    );

  TATCaretScreenSide = (
    cScreenSideTop,
    cScreenSideMiddle,
    cScreenSideBottom
    );

type
  { TATCaretSelections }
  {
  why new record here? we could make methods in TATCarets, but during loops,
  we must always a) skip carets w/o selection,
  b) call CaretItem.GetRange to get _sorted_ range.
  code for binary search (Is***Selected) would be ugly.
  }

  TATCaretSelections = record
  private
    type TATCaretSelection = record
      PosX, PosY, EndX, EndY: integer;
    end;
  public
    Data: array of TATCaretSelection;
    procedure Clear;
    function IsEmpty: boolean;
    function IsMultiline: boolean;
    function IsLineWithSelection(ALine: integer): boolean;
    function IsLineAllSelected(ALine, ALineLen: integer): boolean;
    function IsPosSelected(AX, AY: integer): boolean;
    function IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
    procedure GetRangesInLineAfterPoint(AX, AY: integer; out ARanges: TATSimpleRangeArray);
  end;

type
  { TATCarets }

  TATCarets = class
  private const
    AllowSelectionsTouch = true;
  private
    FList: TFPList;
    FManyAllowed: boolean;
    FOneLine: boolean;
    FOnCaretChanged: TNotifyEvent;
    function GetItem(N: integer): TATCaretItem;
    procedure DeleteDups(AJoinAdjacentCarets: boolean);
    function IsJoinNeeded(AIndex1, AIndex2: integer;
      out OutPosX, OutPosY, OutEndX, OutEndY: integer): boolean;
    function GetAsArray: TATPointArray;
    procedure SetAsArray(const Res: TATPointArray);
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    function IsPosSelected(AX, AY: integer; AllowAtEdge: boolean=false): boolean;
    property Items[N: integer]: TATCaretItem read GetItem; default;
    procedure Add(APosX, APosY: integer; AEndX: integer=-1; AEndY: integer=-1);
    procedure Sort(AJoinAdjacentCarets: boolean=true);
    procedure Assign(Obj: TATCarets);
    function FindCaretBeforePos(APosX, APosY: integer; ARequireSel: boolean): TATCaretItem;
    function IndexOfPosXY(APosX, APosY: integer; AUseEndXY: boolean= false): integer;
    function IndexOfLeftRight(ALeft: boolean): integer;
    function IsLineWithCaret(APosY: integer): boolean;
    function IsLineWithSelection(APosY: integer): boolean;
    function IsSelection: boolean;
    function IsAnyCaretInVisibleRect(const R: TRect): boolean;
    procedure GetSelections(var D: TATCaretSelections);
    function CaretAtEdge(AEdge: TATCaretEdge): TPoint;
    function DebugText: string;
    property ManyAllowed: boolean read FManyAllowed write FManyAllowed;
    property OneLine: boolean read FOneLine write FOneLine;
    property AsArray: TATPointArray read GetAsArray write SetAsArray;
    property AsString: string read GetAsString write SetAsString;
    property OnCaretChanged: TNotifyEvent read FOnCaretChanged write FOnCaretChanged;
    procedure UpdateMemory(AMode: TATCaretMemoryAction; AArrowUpDown: boolean);
    procedure UpdateAfterRangeFolded(ARangeX, ARangeY, ARangeY2: integer);
    procedure DoChanged;
  end;


implementation

uses
  Math{%H-};

function IsPosSorted(X1, Y1, X2, Y2: integer; AllowEq: boolean): boolean; inline;
begin
  if Y1<>Y2 then
    Result:= Y1<Y2
  else
    Result:= (X1<X2) or (AllowEq and (X1=X2));
end;

procedure GetPositionMinOrMax(X1, Y1, X2, Y2: integer; AMaximal: boolean; out OutX, OutY: integer);
begin
  if IsPosSorted(X1, Y1, X2, Y2, true) xor AMaximal then
  begin
    OutX:= X1;
    OutY:= Y1;
  end
  else
  begin
    OutX:= X2;
    OutY:= Y2;
  end;
end;


function IsPosInRange(X, Y, X1, Y1, X2, Y2: integer;
    AllowOnRightEdge: boolean=false): TATPosRelation;
begin
  if IsPosSorted(X, Y, X1, Y1, false) then
    Result:= cRelateBefore
  else
  if IsPosSorted(X, Y, X2, Y2, AllowOnRightEdge) then
    Result:= cRelateInside
  else
    Result:= cRelateAfter;
end;

procedure SwapInt(var n1, n2: integer); inline;
var
  n: integer;
begin
  n:= n1;
  n1:= n2;
  n2:= n;
end;

{ TATCaretSelections }

procedure TATCaretSelections.Clear;
begin
  SetLength(Data, 0);
end;

function TATCaretSelections.IsEmpty: boolean;
begin
  Result:= Length(Data)=0;
end;

function TATCaretSelections.IsMultiline: boolean;
var
  i: integer;
begin
  for i:= 0 to High(Data) do
    if Data[i].EndY<>Data[i].PosY then
      exit(true);
  Result:= false;
end;

function TATCaretSelections.IsLineWithSelection(ALine: integer): boolean;
var
  Y1, Y2, X2: integer;
  a, b, m: integer;
begin
  Result:= false;
  a:= 0;
  b:= High(Data);

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    Y1:= Data[m].PosY;
    X2:= Data[m].EndX;
    Y2:= Data[m].EndY;

    if (X2=0) and (Y2>0) then
      Dec(Y2);

    if ALine<Y1 then
      b:= m-1
    else
    if ALine<=Y2 then
      exit(true)
    else
      a:= m+1;
  until false;
end;

function TATCaretSelections.IsLineAllSelected(ALine, ALineLen: integer): boolean;
var
  X1, Y1, Y2, X2: integer;
  a, b, m: integer;
begin
  Result:= false;
  a:= 0;
  b:= High(Data);

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    X1:= Data[m].PosX;
    Y1:= Data[m].PosY;
    X2:= Data[m].EndX;
    Y2:= Data[m].EndY;

    if not IsPosSorted(X1, Y1, 0, ALine, true) then
      b:= m-1
    else
    if IsPosSorted(ALineLen, ALine, X2, Y2, true) then
      exit(true)
    else
      a:= m+1;
  until false;
end;


function TATCaretSelections.IsPosSelected(AX, AY: integer): boolean;
var
  X1, Y1, X2, Y2: integer;
  a, b, m: integer;
begin
  Result:= false;
  a:= 0;
  b:= High(Data);

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    X1:= Data[m].PosX;
    Y1:= Data[m].PosY;
    X2:= Data[m].EndX;
    Y2:= Data[m].EndY;

    case IsPosInRange(AX, AY, X1, Y1, X2, Y2) of
      cRelateInside:
        exit(true);
      cRelateBefore:
        b:= m-1;
      cRelateAfter:
        a:= m+1;
    end;
  until false;
end;

function TATCaretSelections.IsRangeSelected(AX1, AY1, AX2, AY2: integer): TATRangeSelection;
var
  X1, Y1, X2, Y2: integer;
  a, b, m: integer;
  bLeft, bRight: TATPosRelation;
begin
  Result:= cRangeAllUnselected;
  a:= 0;
  b:= High(Data);

  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;

    X1:= Data[m].PosX;
    Y1:= Data[m].PosY;
    X2:= Data[m].EndX;
    Y2:= Data[m].EndY;

    bLeft:= IsPosInRange(AX1, AY1, X1, Y1, X2, Y2);
    if (bLeft=cRelateAfter) then
    begin
      a:= m+1;
      Continue;
    end;

    bRight:= IsPosInRange(AX2, AY2, X1, Y1, X2, Y2, true);
    if (bRight=cRelateBefore) then
    begin
      b:= m-1;
      Continue;
    end;

    if (bLeft=cRelateInside) and (bRight=cRelateInside) then
      exit(cRangeAllSelected)
    else
      exit(cRangePartlySelected);
  until false;
end;

procedure TATCaretSelections.GetRangesInLineAfterPoint(AX, AY: integer; out
  ARanges: TATSimpleRangeArray);
var
  X1, Y1, X2, Y2, XFrom, XTo: integer;
  i: integer;
begin
  SetLength(ARanges, 0);
  for i:= 0 to High(Data) do
  begin
    X1:= Data[i].PosX;
    Y1:= Data[i].PosY;
    X2:= Data[i].EndX;
    Y2:= Data[i].EndY;

    if (Y1>AY) then Break; //caret is fully after line AY: stop
    if (Y2<AY) then Continue; //caret is fully before line AY

    if (Y1<AY) then XFrom:= 0 else XFrom:= X1;
    if (Y2>AY) then XTo:= MaxInt else XTo:= X2;

    if XTo<=AX then Continue;
    if XFrom<AX then XFrom:= AX;

    SetLength(ARanges, Length(ARanges)+1);
    with ARanges[High(ARanges)] do
    begin
      NFrom:= XFrom;
      NTo:= XTo;
    end;
  end;
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

procedure TATCaretItem.GetSelLines(out AFrom, ATo: integer;
  AllowNoSel: boolean = false);
var
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  AFrom:= -1;
  ATo:= -1;

  GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then
  begin
    if AllowNoSel then
      begin AFrom:= PosY; ATo:= PosY; end;
    Exit
  end;

  AFrom:= Y1;
  ATo:= Y2;
  //sel ended at line-start?
  if (X2=0) and (Y2>0) then Dec(ATo);
end;

function TATCaretItem.Change(APosX, APosY, AEndX, AEndY: integer): boolean;
begin
  Result:=
    (PosX<>APosX) or
    (PosY<>APosY) or
    (EndX<>AEndX) or
    (EndY<>AEndY);
  if Result then
  begin
    PosX:= APosX;
    PosY:= APosY;
    EndX:= AEndX;
    EndY:= AEndY;
  end;
end;

procedure TATCaretItem.SwapSelection;
begin
  if EndY>=0 then
  begin
    SwapInt(PosX, EndX);
    SwapInt(PosY, EndY);
  end;
end;

function TATCaretItem.IsForwardSelection: boolean;
begin
  Result:= (PosY>=0) and IsPosSorted(EndX, EndY, PosX, PosY, false);
end;

function TATCaretItem.IsMultilineSelection: boolean;
begin
  Result:= (EndY>=0) and (EndY<>PosY);
end;

function TATCaretItem.FirstTouchedLine: integer;
begin
  if (EndY>=0) and (EndY<PosY) then
    Result:= EndY
  else
    Result:= PosY;
end;

procedure TATCaretItem.UpdateMemory(AMode: TATCaretMemoryAction; AArrowUpDown: boolean);
begin
  case AMode of
    cCaretMem_PrepareX:
      begin
        SavedX_Pre:= CoordX;
      end;
    cCaretMem_SaveX:
      begin
        if (not AArrowUpDown) or (SavedX<SavedX_Pre) then
          SavedX:= SavedX_Pre;
      end;
    cCaretMem_ClearX:
      begin
        SavedX:= 0;
      end;
  end;
end;

procedure TATCaretItem.SelectNone;
begin
  EndX:= -1;
  EndY:= -1;
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
  FList:= TFPList.Create;
  FManyAllowed:= true;
  FOneLine:= false;
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
    TObject(FList[i]).Free;
  FList.Clear;
end;

procedure TATCarets.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
    DoChanged;
  end;
end;

function TATCarets.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATCarets.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

procedure TATCarets.Add(APosX, APosY: integer; AEndX: integer=-1; AEndY: integer=-1);
var
  Item: TATCaretItem;
begin
  if (not ManyAllowed) and (Count>=1) then Exit;
  if OneLine then
  begin
    APosY:= 0;
    if AEndY>0 then AEndY:= 0;
  end;

  Item:= TATCaretItem.Create;
  Item.PosX:= APosX;
  Item.PosY:= APosY;
  Item.EndX:= AEndX;
  Item.EndY:= AEndY;

  FList.Add(Item);
  DoChanged;
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

procedure TATCarets.Sort(AJoinAdjacentCarets: boolean=true);
begin
  FList.Sort(@_ListCaretsCompare);
  DeleteDups(AJoinAdjacentCarets);
end;

procedure TATCarets.DeleteDups(AJoinAdjacentCarets: boolean);
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

    if AJoinAdjacentCarets and
      IsJoinNeeded(i, i-1, OutPosX, OutPosY, OutEndX, OutEndY) then
    begin
      Delete(i);
      Item2.PosX:= OutPosX;
      Item2.PosY:= OutPosY;
      Item2.EndX:= OutEndX;
      Item2.EndY:= OutEndY;
    end;
  end;

  DoChanged;
end;


procedure TATCarets.Assign(Obj: TATCarets);
var
  Caret: TATCaretItem;
  i: integer;
begin
  Clear;
  for i:= 0 to Obj.Count-1 do
  begin
    Caret:= Obj[i];
    Add(Caret.PosX, Caret.PosY, Caret.EndX, Caret.EndY);
  end;
  DoChanged;
end;

function TATCarets.FindCaretBeforePos(APosX, APosY: integer;
  ARequireSel: boolean): TATCaretItem;
var
  Item: TATCaretItem;
  bSel: boolean;
  X1, Y1, X2, Y2: integer;
  i: integer;
begin
  Result:= nil;
  for i:= Count-1 downto 0 do
  begin
    Item:= Items[i];
    Item.GetRange(X1, Y1, X2, Y2, bSel);
    if ARequireSel and not bSel then
      Continue;
    if (Y1<APosY) or ((Y1=APosY) and (X1<APosX)) then
      exit(Item);
  end;
end;

function TATCarets.IndexOfPosXY(APosX, APosY: integer; AUseEndXY: boolean = false): integer;
var
  Item: TATCaretItem;
  useX, useY: integer;
  a, b, m, difX, difY: integer;
begin
  Result:= -1;
  a:= 0;
  b:= Count-1;
  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;
    Item:= Items[m];
    if AUseEndXY and (Item.EndY>=0) then
    begin
      useX:= Item.EndX;
      useY:= Item.EndY;
    end
    else
    begin
      useX:= Item.PosX;
      useY:= Item.PosY;
    end;
    difX:= useX-APosX;
    difY:= useY-APosY;
    if (difX=0) and (difY=0) then
      exit(m);
    if (difY>0) or ((difY=0) and (difX>0)) then
      b:= m-1
    else
      a:= m+1;
  until false;
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

function TATCarets.IsLineWithCaret(APosY: integer): boolean;
var
  a, b, m, dif: integer;
begin
  Result:= false;
  a:= 0;
  b:= Count-1;
  repeat
    if a>b then exit;
    m:= (a+b+1) div 2;
    dif:= Items[m].PosY-APosY;
    if dif=0 then
      exit(true);
    if dif>0 then
      b:= m-1
    else
      a:= m+1;
  until false;
end;

function TATCarets.IsLineWithSelection(APosY: integer): boolean;
var
  Item: TATCaretItem;
  Y1, Y2, X1, X2: integer;
  i: integer;
begin
  Result:= false;
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    if Item.EndY>=0 then
    begin
      X1:= Item.PosX;
      Y1:= Item.PosY;
      X2:= Item.EndX;
      Y2:= Item.EndY;
      if (Y1>Y2) or ((Y1=Y2) and (X1>X2)) then
      begin
        SwapInt(Y1, Y2);
        SwapInt(X1, X2);
      end;
      if (X2=0) and (Y2>0) then
        Dec(Y2);
      if (Y1<=APosY) and (APosY<=Y2) then
        exit(true);
    end;
  end;
end;


function TATCarets.IsSelection: boolean;
var
  i: integer;
begin
  for i:= 0 to Count-1 do
    if Items[i].EndY>=0 then
      exit(true);
  Result:= false;
end;

function TATCarets.IsAnyCaretInVisibleRect(const R: TRect): boolean;
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  i: integer;
begin
  Result:= false;
  for i:= 0 to Count-1 do
  begin
    Caret:= Items[i];
    Pnt.X:= Caret.CoordX;
    Pnt.Y:= Caret.CoordY;
    if PtInRect(R, Pnt) then
      exit(true);
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

function TATCarets.IsJoinNeeded(AIndex1, AIndex2: integer;
  out OutPosX, OutPosY, OutEndX, OutEndY: integer): boolean;
var
  Item1, Item2: TATCaretItem;
  XMin1, XMin2, YMin1, YMin2, XMax1, XMax2, YMax1, YMax2: integer;
  Sel1, Sel2: boolean;
begin
  Result:= false;
  if not IsIndexValid(AIndex1) then Exit;
  if not IsIndexValid(AIndex2) then Exit;

  Item1:= Items[AIndex1];
  Item2:= Items[AIndex2];
  Item1.GetRange(XMin1, YMin1, XMax1, YMax1, Sel1);
  Item2.GetRange(XMin2, YMin2, XMax2, YMax2, Sel2);

  //caret1 w/out selection inside caret2 selection?
  if not Sel1 and not Sel2 then Exit;
  if not Sel1 then
  begin
    Result:= IsPosInRange(Item1.PosX, Item1.PosY, XMin2, YMin2, XMax2, YMax2)=cRelateInside;
    if Result then
      begin OutPosX:= Item2.PosX; OutPosY:= Item2.PosY; OutEndX:= Item2.EndX; OutEndY:= Item2.EndY; end;
    Exit
  end;
  if not Sel2 then
  begin
    Result:= IsPosInRange(Item2.PosX, Item2.PosY, XMin1, YMin1, XMax1, YMax1)=cRelateInside;
    if Result then
      begin OutPosX:= Item1.PosX; OutPosY:= Item1.PosY; OutEndX:= Item1.EndX; OutEndY:= Item1.EndY; end;
    Exit
  end;

  //calc join-result, needed only for Result=true
  //minimal point
  GetPositionMinOrMax(XMin1, YMin1, XMin2, YMin2, false, OutPosX, OutPosY);
  //maximal point
  GetPositionMinOrMax(XMax1, YMax1, XMax2, YMax2, true, OutEndX, OutEndY);

  //swap points?
  if Item1.IsForwardSelection then
  begin
    SwapInt(OutPosX, OutEndX);
    SwapInt(OutPosY, OutEndY);
  end;

  if IsPosSorted(XMax1, YMax1, XMin2, YMin2, AllowSelectionsTouch) then
    Exit; //ranges not overlap [x1, y1]...[x2, y2]

  if IsPosSorted(XMax2, YMax2, XMin1, YMin1, AllowSelectionsTouch) then
    Exit; //ranges not overlap [x2, y2]...[x1, y1]

  Result:= true; //ranges overlap
end;

procedure TATCarets.DoChanged;
begin
  if Assigned(FOnCaretChanged) then
    FOnCaretChanged(Self);
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

function TATCarets.GetAsArray: TATPointArray;
var
  Item: TATCaretItem;
  i: integer;
begin
  SetLength(Result{%H-}, Count*2);
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    Result[i*2].X:= Item.PosX;
    Result[i*2].Y:= Item.PosY;
    Result[i*2+1].X:= Item.EndX;
    Result[i*2+1].Y:= Item.EndY;
  end;
end;

procedure TATCarets.SetAsArray(const Res: TATPointArray);
var
  i: integer;
begin
  Clear;
  for i:= 0 to Length(Res) div 2 - 1 do
    Add(
      Res[i*2].X,
      Res[i*2].Y,
      Res[i*2+1].X,
      Res[i*2+1].Y
      );
  DoChanged;
end;

function TATCarets.GetAsString: string;
var
  Item: TATCaretItem;
  S: string;
  NLast, i: integer;
begin
  Result:= '';
  NLast:= Count-1;
  for i:= 0 to NLast do
  begin
    Item:= Items[i];
    if Item.EndY<0 then
      S:= Format('%d,%d', [Item.PosX, Item.PosY])
    else
      S:= Format('%d,%d,%d,%d', [Item.PosX, Item.PosY, Item.EndX, Item.EndY]);
    if i<NLast then
      S+= ';';
    Result+= S;
  end;
end;

procedure TATCarets.SetAsString(const AValue: string);
var
  Sep1, Sep2: TATStringSeparator;
  OneItem: string;
  X1, Y1, X2, Y2: integer;
begin
  Clear;
  Sep1.Init(AValue, ';');

  while Sep1.GetItemStr(OneItem) do
  begin
    Sep2.Init(OneItem, ',');
    Sep2.GetItemInt(X1, -1, 0, MaxInt);
    if X1<0 then Continue;
    Sep2.GetItemInt(Y1, -1, 0, MaxInt);
    if Y1<0 then Continue;
    Sep2.GetItemInt(X2, -1, -1{no sel}, MaxInt);
    Sep2.GetItemInt(Y2, -1, -1{no sel}, MaxInt);
    Add(X1, Y1, X2, Y2);
  end;

  Sort;
end;

procedure TATCarets.UpdateMemory(AMode: TATCaretMemoryAction; AArrowUpDown: boolean);
var
  i: integer;
begin
  for i:= 0 to Count-1 do
    Items[i].UpdateMemory(AMode, AArrowUpDown);
end;

procedure TATCarets.UpdateAfterRangeFolded(ARangeX, ARangeY, ARangeY2: integer);
var
  Caret: TATCaretItem;
  bChange: boolean;
  i: integer;
begin
  bChange:= false;
  for i:= 0 to Count-1 do
  begin
    Caret:= GetItem(i);
    if (Caret.PosY>=ARangeY) and (Caret.PosY<=ARangeY2) and
      ((Caret.PosY>ARangeY) or (Caret.PosX>ARangeX)) then
    begin
      bChange:= true;
      Caret.PosX:= Max(0, ARangeX-1);
      Caret.PosY:= ARangeY;
    end;
  end;
  if bChange then
    Sort;
end;

procedure TATCarets.GetSelections(var D: TATCaretSelections);
var
  Item: TATCaretItem;
  NLen, i: integer;
  X1, Y1, X2, Y2: integer;
begin
  SetLength(D.Data, Count);
  NLen:= 0;

  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    X1:= Item.PosX;
    Y1:= Item.PosY;
    X2:= Item.EndX;
    Y2:= Item.EndY;
    if Y2>=0 then
    begin
      if (Y1>Y2) or ((Y1=Y2) and (X1>X2)) then
      begin
        SwapInt(Y1, Y2);
        SwapInt(X1, X2);
      end;
      D.Data[NLen].PosX:= X1;
      D.Data[NLen].PosY:= Y1;
      D.Data[NLen].EndX:= X2;
      D.Data[NLen].EndY:= Y2;
      Inc(NLen);
    end;
  end;

  //don't realloc in a loop
  SetLength(D.Data, NLen);
end;


function TATCarets.IsPosSelected(AX, AY: integer; AllowAtEdge: boolean): boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
  i: integer;
begin
  Result:= false;
  for i:= 0 to Count-1 do
  begin
    Caret:= Items[i];
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    if bSel then
      if IsPosInRange(AX, AY, X1, Y1, X2, Y2, AllowAtEdge)=cRelateInside then
        exit(true);
  end;
end;

end.

