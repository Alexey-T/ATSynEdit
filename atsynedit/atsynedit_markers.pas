{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Markers;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATStringProc_Arrays,
  ATSynEdit_fgl,
  ATSynEdit_Carets,
  ATSynEdit_LineParts;

type
  TATMarkerMicromapMode = (
    mmmShowInTextOnly,
    mmmShowInMicromapOnly,
    mmmShowInTextAndMicromap
    );

type

  { TATMarkerTags }

  TATMarkerTags = record
    Tag, ColumnTag, Value: Int64;
    constructor Init(const ATag, AColumnTag, AValue: Int64);
  end;

type
  { TATMarkerItem }

  PATMarkerItem = ^TATMarkerItem;
  TATMarkerItem = record
    //text position of marker
    PosX, PosY: integer;

    //render underline near the marker, when LineLen<>0
    LineLen: integer;

    //screen coords
    CoordX, CoordY: integer;
    //screen coords of line end, when LineLen<>0
    CoordX2, CoordY2: integer;

    //used in CudaText: when "Collect marker" runs, for all markers
    //with the same Tag>0 multi-carets are placed
    Tag: Int64;

    //used in CudaText: when "Collect marker" gets this marker, caret will be with selection
    //if SelY=0 - LenX is length of sel (single line)
    //if SelY>0 - LenY is Y-delta of sel-end,
    //            LenX is absolute X of sel-end
    SelX, SelY: integer;

    //used in DimRanges list, holds dim value
    Value: Int64;

    //used to place marker on micromap column, with given Tag
    ColumnTag: Int64;

    //used in Attribs object
    LinePart: TATLinePart;

    //enables to show marker on micromap
    MicromapMode: TATMarkerMicromapMode;

    class operator=(const A, B: TATMarkerItem): boolean;
    function SelContains(AX, AY: integer): boolean;
    function SelEnd: TPoint;
    procedure UpdateOnEditing(APosX, APosY, AShiftX, AShiftY,
      AShiftBelowX: integer; APosAfter: TPoint);
  end;

function IsMarkerPositionsEqual(A, B: PATMarkerItem): boolean;

type
  { TATMarkerItems }

  TATMarkerItems = specialize TFPGList<TATMarkerItem>;

type
  { TATMarkers }

  TATMarkers = class
  private
    FList: TATMarkerItems;
    FSorted: boolean;
    FDuplicates: boolean;
    function GetAsMarkerArray: TATMarkerMarkerArray;
    function GetAsAttribArray: TATMarkerAttribArray;
    function GetAsMarkerString: string;
    function GetItem(N: integer): TATMarkerItem;
    procedure SetAsMarkerArray(const AValue: TATMarkerMarkerArray);
    procedure SetAsAttribArray(const AValue: TATMarkerAttribArray);
    procedure SetAsMarkerString(const AValue: string);
    procedure SetItem(N: integer; const AItem: TATMarkerItem);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(AIndex: integer);
    function Count: integer; inline;
    function IsIndexValid(AIndex: integer): boolean; inline;
    function ItemPtr(AIndex: integer): PATMarkerItem;
    property Items[AIndex: integer]: TATMarkerItem read GetItem write SetItem; default;
    property Sorted: boolean read FSorted write FSorted;
    property Duplicates: boolean read FDuplicates write FDuplicates;
    procedure Add(
      APosX, APosY: integer;
      const ATags: TATMarkerTags;
      ASelX: integer=0;
      ASelY: integer=0;
      ALinePart: PATLinePart=nil;
      AMicromapMode: TATMarkerMicromapMode=mmmShowInTextOnly;
      ALineLen: integer=0);
    function DeleteInRange(AX1, AY1, AX2, AY2: integer): boolean;
    function DeleteWithTag(const ATag: Int64): boolean;
    function DeleteByPos(AX, AY: integer): boolean;
    procedure Find(AX, AY: integer; out AIndex: integer; out AExactMatch: boolean);
    function FindContaining(AX, AY: integer): integer;
    procedure UpdateOnEditing(APosX, APosY, AShiftX, AShiftY, AShiftBelowX: integer; APosAfter: TPoint);
    property AsMarkerArray: TATMarkerMarkerArray read GetAsMarkerArray write SetAsMarkerArray;
    property AsAttribArray: TATMarkerAttribArray read GetAsAttribArray write SetAsAttribArray;
    property AsMarkerString: string read GetAsMarkerString write SetAsMarkerString;
  end;

implementation

function IsMarkerPositionsEqual(A, B: PATMarkerItem): boolean;
begin
  Result:= (A^.PosX=B^.PosX) and (A^.PosY=B^.PosY);
end;

{ TATMarkerTags }

constructor TATMarkerTags.Init(const ATag, AColumnTag, AValue: Int64);
begin
  Tag:= ATag;
  ColumnTag:= AColumnTag;
  Value:= AValue;
end;

{ TATMarkerItem }

class operator TATMarkerItem.=(const A, B: TATMarkerItem): boolean;
begin
  Result:= (A.PosX=B.PosX) and (A.PosY=B.PosY);
end;

function TATMarkerItem.SelContains(AX, AY: integer): boolean;
var
  P: TPoint;
begin
  if (SelX<=0) and (SelY<=0) then
    Result:= false
  else
  begin
    P:= SelEnd;
    Result:= IsPosInRange(AX, AY, PosX, PosY, P.X, P.Y)=cRelateInside;
  end;
end;

function TATMarkerItem.SelEnd: TPoint;
begin
  if SelY<=0 then
  begin
    //SelX is selection len (selection is single line)
    Result.X:= PosX+SelX;
    Result.Y:= PosY;
  end
  else
  begin
    //SelX is selection end X-pos;
    //SelY is count of sel lines
    Result.X:= SelX;
    Result.Y:= PosY+SelY;
  end;
end;

procedure TATMarkerItem.UpdateOnEditing(
  APosX, APosY, AShiftX, AShiftY, AShiftBelowX: integer;
  APosAfter: TPoint);
begin
  //marker below src, apply ShiftY/ShiftBelowX
  if PosY>APosY then
  begin
    if AShiftY=0 then exit;

    if PosY=APosY+1 then
      Inc(PosX, AShiftBelowX);

    Inc(PosY, AShiftY);
  end
  else
  //marker on same line as src
  if PosY=APosY then
  begin
    if PosX=APosX then
    begin
      PosX:= APosAfter.X;
      PosY:= APosAfter.Y;
    end
    else
    if PosX>=APosX then
      if AShiftY=0 then
        Inc(PosX, AShiftX)
      else
      begin
        Inc(PosX, -APosX+APosAfter.X);
        Inc(PosY, AShiftY);
      end;
  end;

  if PosX<0 then PosX:= 0;
  if PosY<0 then PosY:= 0;
end;

{ TATMarkers }

constructor TATMarkers.Create;
begin
  inherited;
  FList:= TATMarkerItems.Create;
  FSorted:= false;
  FDuplicates:= false;
end;

destructor TATMarkers.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATMarkers.Clear;
begin
  FList.Clear;
end;

procedure TATMarkers.Delete(AIndex: integer);
begin
  if IsIndexValid(AIndex) then
    FList.Delete(AIndex);
end;

function TATMarkers.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATMarkers.IsIndexValid(AIndex: integer): boolean; inline;
begin
  Result:= (AIndex>=0) and (AIndex<FList.Count);
end;

function TATMarkers.GetItem(N: integer): TATMarkerItem;
begin
  Result:= FList[N];
end;

function TATMarkers.ItemPtr(AIndex: integer): PATMarkerItem;
begin
  Result:= FList._GetItemPtr(AIndex);
end;

function TATMarkers.GetAsMarkerArray: TATMarkerMarkerArray;
var
  Item: PATMarkerItem;
  i: integer;
begin
  SetLength(Result{%H-}, Count);
  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    Result[i].PosX:= Item^.PosX;
    Result[i].PosY:= Item^.PosY;
    Result[i].SelX:= Item^.SelX;
    Result[i].SelY:= Item^.SelY;
    Result[i].Tag:= Item^.Tag;
    Result[i].Value:= Item^.Value;
    Result[i].MicromapMode:= Ord(Item^.MicromapMode);
  end;
end;

function TATMarkers.GetAsAttribArray: TATMarkerAttribArray;
var
  Item: PATMarkerItem;
  i: integer;
begin
  SetLength(Result{%H-}, Count);
  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    Result[i].Tag:= Item^.Tag;
    Result[i].PosX:= Item^.PosX;
    Result[i].PosY:= Item^.PosY;
    Result[i].SelX:= Item^.SelX;

    Result[i].ColorFont:= Item^.LinePart.ColorFont;
    Result[i].ColorBG:= Item^.LinePart.ColorBG;
    Result[i].ColorBorder:= Item^.LinePart.ColorBorder;
    Result[i].FontStyles:= Item^.LinePart.FontStyles;
    Result[i].BorderLeft:= Ord(Item^.LinePart.BorderLeft);
    Result[i].BorderRight:= Ord(Item^.LinePart.BorderRight);
    Result[i].BorderDown:= Ord(Item^.LinePart.BorderDown);
    Result[i].BorderUp:= Ord(Item^.LinePart.BorderUp);
    Result[i].ColumnTag:= Item^.ColumnTag;
    Result[i].MicromapMode:= Ord(Item^.MicromapMode);
  end;
end;

procedure TATMarkers.SetAsMarkerArray(const AValue: TATMarkerMarkerArray);
var
  i: integer;
begin
  Clear;
  for i:= 0 to Length(AValue)-1 do
  begin
    Add(
      AValue[i].PosX,
      AValue[i].PosY,
      TATMarkerTags.Init(AValue[i].Tag, 0, AValue[i].Value),
      AValue[i].SelX,
      AValue[i].SelY,
      nil,
      TATMarkerMicromapMode(AValue[i].MicromapMode)
      );
  end;
end;

procedure TATMarkers.SetAsAttribArray(const AValue: TATMarkerAttribArray);
var
  LinePart: TATLinePart;
  i: integer;
begin
  Clear;
  InitLinePart(LinePart);
  for i:= 0 to Length(AValue)-1 do
  begin
    LinePart.ColorFont:= AValue[i].ColorFont;
    LinePart.ColorBG:= AValue[i].ColorBG;
    LinePart.ColorBorder:= AValue[i].ColorBorder;
    LinePart.FontStyles:= AValue[i].FontStyles;
    LinePart.BorderLeft:= TATLineStyle(AValue[i].BorderLeft);
    LinePart.BorderRight:= TATLineStyle(AValue[i].BorderRight);
    LinePart.BorderDown:= TATLineStyle(AValue[i].BorderDown);
    LinePart.BorderUp:= TATLineStyle(AValue[i].BorderUp);

    Add(
      AValue[i].PosX,
      AValue[i].PosY,
      TATMarkerTags.Init(AValue[i].Tag, AValue[i].ColumnTag, 0),
      AValue[i].SelX,
      0,
      @LinePart,
      TATMarkerMicromapMode(AValue[i].MicromapMode)
      );
  end;
end;

function TATMarkers.GetAsMarkerString: string;
begin
  Result:= MarkerArrayToString(AsMarkerArray);
end;

procedure TATMarkers.SetAsMarkerString(const AValue: string);
var
  Ar: TATMarkerMarkerArray;
begin
  StringToMarkerArray(Ar, AValue);
  AsMarkerArray:= Ar;
end;

procedure TATMarkers.SetItem(N: integer; const AItem: TATMarkerItem);
begin
  FList[N]:= AItem;
end;

procedure TATMarkers.Add(APosX, APosY: integer; const ATags: TATMarkerTags;
  ASelX: integer; ASelY: integer; ALinePart: PATLinePart;
  AMicromapMode: TATMarkerMicromapMode; ALineLen: integer);
var
  Item: TATMarkerItem;
  NIndex, NIndexFrom, NIndexTo: integer;
  bExact: boolean;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.PosX:= APosX;
  Item.PosY:= APosY;
  Item.CoordX:= -1;
  Item.CoordY:= -1;
  Item.CoordX2:= -1;
  Item.CoordY2:= -1;
  Item.SelX:= ASelX;
  Item.SelY:= ASelY;
  Item.LineLen:= ALineLen;
  Item.MicromapMode:= AMicromapMode;

  Item.Tag:= ATags.Tag;
  Item.ColumnTag:= ATags.ColumnTag;
  Item.Value:= ATags.Value;

  if Assigned(ALinePart) then
    Item.LinePart:= ALinePart^
  else
    InitLinePart(Item.LinePart);

  if FSorted then
  begin
    Find(APosX, APosY, NIndex, bExact);
    if bExact then
    begin
      if not FDuplicates then
      begin
        NIndexFrom:= NIndex;
        NIndexTo:= NIndex;
        while IsIndexValid(NIndexTo+1) and IsMarkerPositionsEqual(ItemPtr(NIndexTo+1), @Item) do
          Inc(NIndexTo);
        while IsIndexValid(NIndexFrom-1) and IsMarkerPositionsEqual(ItemPtr(NIndexFrom-1), @Item) do
          Dec(NIndexFrom);
        //save 2 reallocs (delete, insert)
        //FList.Deref(FList._GetItemPtr(NIndexFrom));
        FList.Items[NIndexFrom]:= Item;
        //delete other dups
        if NIndexFrom+1<=NIndexTo then
          FList.DeleteRange(NIndexFrom+1, NIndexTo);
      end
      else
      begin
        repeat
          Inc(NIndex)
        until not IsIndexValid(NIndex) or not IsMarkerPositionsEqual(ItemPtr(NIndex), @Item);
        FList.Insert(NIndex, Item);
      end;
    end
    else
      FList.Insert(NIndex, Item);
  end
  else
    FList.Add(Item);
end;

function TATMarkers.DeleteInRange(AX1, AY1, AX2, AY2: integer): boolean;
  //
  function IsMarkerOk(Item: PATMarkerItem): boolean; inline;
  begin
    Result:= IsPosInRange(Item^.PosX, Item^.PosY, AX1, AY1, AX2, AY2)=cRelateInside;
  end;
  //
var
  i, j: integer;
begin
  Result:= false;
  i:= Count;
  repeat
    Dec(i);
    if i<0 then Break;
    if IsMarkerOk(ItemPtr(i)) then
    begin
      Result:= true;
      j:= i;
      while (j>0) and IsMarkerOk(ItemPtr(j-1)) do
        Dec(j);
      FList.DeleteRange(j, i);
      i:= j;
    end;
  until false;
end;

function TATMarkers.DeleteWithTag(const ATag: Int64): boolean;
var
  i, j: integer;
begin
  Result:= false;
  i:= Count;
  repeat
    Dec(i);
    if i<0 then Break;
    if ItemPtr(i)^.Tag=ATag then
    begin
      Result:= true;
      j:= i;
      while (j>0) and (ItemPtr(j-1)^.Tag=ATag) do
        Dec(j);
      FList.DeleteRange(j, i);
      i:= j;
    end;
  until false;
end;

function TATMarkers.DeleteByPos(AX, AY: integer): boolean;
// if AX=-1, delete all items for line AY
  //
  function IsMarkerOk(AIndex: integer): boolean; inline;
  var
    P: PATMarkerItem;
  begin
    P:= ItemPtr(AIndex);
    Result:= (P^.PosY=AY) and ((AX<0) or (P^.PosX=AX));
  end;
  //
var
  N, NFrom, NTo: integer;
  bExact: boolean;
begin
  Result:= false;

  if AX<0 then
    Find(0, AY, N, bExact)
  else
    Find(AX, AY, N, bExact);

  if N>=0 then
  begin
    NFrom:= N;
    NTo:= N-1;
    while IsIndexValid(NTo+1) and IsMarkerOk(NTo+1) do
      Inc(NTo);
    while IsIndexValid(NFrom-1) and IsMarkerOk(NFrom-1) do
      Dec(NFrom);
    if NTo>=NFrom then
    begin
      FList.DeleteRange(NFrom, NTo);
      Result:= true;
    end;
  end;
end;

function _ComparePoints(X1, Y1, X2, Y2: integer): integer; inline;
begin
  if Y1<>Y2 then
    Result:= Y1-Y2
  else
    Result:= X1-X2;
end;

procedure TATMarkers.Find(AX, AY: integer; out AIndex: integer; out AExactMatch: boolean);
//gives AIndex in range [0..Count] (without -1)
var
  L, H, I, C: Integer;
  Item: PATMarkerItem;
begin
  if not FSorted then
    raise Exception.Create('Method Find can be used only with Sorted=true');

  AIndex := 0;
  AExactMatch := False;

  if Count = 0 then
    Exit;

  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Item := ItemPtr(I);
    C := _ComparePoints(Item^.PosX, Item^.PosY, AX, AY);
    if C < 0 then
      L := I + 1
    else
    begin
      if C = 0 then
      begin
        AIndex := I;
        if FDuplicates then
          while (AIndex>0) and (Item^=ItemPtr(AIndex-1)^) do
            Dec(AIndex);
        AExactMatch := True;
        Exit;
      end;
      H := I - 1;
    end;
  end;
  AIndex := L;
end;

function TATMarkers.FindContaining(AX, AY: integer): integer;
var
  Item: PATMarkerItem;
  NIndex: integer;
  bExact: boolean;
begin
  Result:= -1;
  if Count=0 then exit;

  Find(AX, AY, NIndex, bExact);

  //because Find is limited, check also nearest 2 items
  if NIndex>=Count then
    NIndex:= Count-1;

  Item:= ItemPtr(NIndex);
  if Item^.SelContains(AX, AY) then
    exit(NIndex);

  if NIndex>0 then
  begin
    Dec(NIndex);
    Item:= ItemPtr(NIndex);
    if Item^.SelContains(AX, AY) then
      exit(NIndex);
  end;
end;


procedure TATMarkers.UpdateOnEditing(APosX, APosY, AShiftX, AShiftY, AShiftBelowX: integer;
  APosAfter: TPoint);
var
  Item: PATMarkerItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    Item^.UpdateOnEditing(APosX, APosY, AShiftX, AShiftY, AShiftBelowX, APosAfter);
  end;
end;

end.

