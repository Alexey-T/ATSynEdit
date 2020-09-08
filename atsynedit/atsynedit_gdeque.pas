{
   This file is part of the Free Pascal FCL library.
   BSD parts (c) 2011 Vlado Boza

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{$mode objfpc}

unit atsynedit_gdeque;

interface

type

  { TDeque }

  generic TDeque<T>=class
  private
  type 
    PT=^T;
    TArr=array of T;
  var 
    FData:TArr;
    FDataSize:SizeUInt;
    FCapacity:SizeUInt;
    FStart:SizeUInt;
    function GetMutable(position:SizeUInt):PT;inline;
    procedure IncreaseCapacity();inline;
  public
    function Size():SizeUInt;inline;
    constructor Create();
    Procedure Clear;
    procedure PushBack(constref value:T);inline;
    procedure PushFront(constref value:T);inline;
    procedure PopBack();inline;
    procedure PopFront();inline;
    function Front():T;inline;
    function Back():T;inline;
    function IsEmpty():boolean;inline;
    procedure Reserve(cap:SizeUInt);inline;
    procedure Resize(cap:SizeUInt);inline;
    procedure Insert(Position:SizeUInt; constref Value:T);inline;
    procedure Erase(Position:SIzeUInt);inline;
    property Mutable[i : SizeUInt]:PT read GetMutable;
end;

implementation

constructor TDeque.Create();
begin
  FDataSize:=0;
  FCapacity:=0;
  FStart:=0;
end;

procedure TDeque.Clear;
begin
 FDataSize:=0;
 FStart:=0;
end;

function TDeque.Size():SizeUInt;inline;
begin
  Size:=FDataSize;
end;

function TDeque.IsEmpty():boolean;inline;
begin
  IsEmpty:=Size()=0;
end;

procedure TDeque.PushBack(constref value: T);
begin
  if(FDataSize=FCapacity) then 
    IncreaseCapacity;
  FData[(FStart+FDataSize)mod FCapacity]:=value;
  inc(FDataSize);
end;

procedure TDeque.PopFront();inline;
begin
  if(FDataSize>0) then 
  begin
    inc(FStart);
    dec(FDataSize);
    if(FStart=FCapacity) then 
      FStart:=0;
  end;
end;

procedure TDeque.PopBack();inline;
begin
  if(FDataSize>0) then
    dec(FDataSize);
end;

procedure TDeque.PushFront(constref value: T);
begin
  if(FDataSize=FCapacity) then
    IncreaseCapacity;
  if(FStart=0) then
    FStart:=FCapacity-1
  else
    dec(FStart);
  FData[FStart]:=value;
  inc(FDataSize);
end;

function TDeque.Front():T;inline;
begin
  Assert(size > 0, 'Accessing empty deque');
  Front:=FData[FStart];
end;

function TDeque.Back():T;inline;
begin
  Assert(size > 0, 'Accessing empty deque');
  Back:=FData[(FStart+FDataSize-1)mod FCapacity];
end;

function TDeque.GetMutable(position:SizeUInt):PT;inline;
begin
  Assert(position < size, 'Deque access out of range');
  GetMutable:=@FData[(FStart+position) mod FCapacity];
end;

procedure TDeque.IncreaseCapacity();
const
  // if size is small, multiply by 2;
  // if size bigger but <256M, inc by 1/8*size;
  // otherwise inc by 1/16*size
  cSizeSmall = 1*1024*1024;
  cSizeBig = 256*1024*1024;
var
  i,OldEnd,
  DataSize:SizeUInt;
begin
  OldEnd:=FCapacity;
  DataSize:=FCapacity*SizeOf(T);
  if FCapacity=0 then
    FCapacity:=4
  else
  if DataSize<cSizeSmall then
    FCapacity:=FCapacity*2
  else
  if DataSize<cSizeBig then
    FCapacity:=FCapacity+FCapacity div 8
  else
    FCapacity:=FCapacity+FCapacity div 16;

  SetLength(FData, FCapacity);
  if (FStart>0) then 
    for i:=0 to FStart-1 do
      FData[OldEnd+i]:=FData[i];
end;

procedure TDeque.Reserve(cap:SizeUInt);inline;
var i,OldEnd:SizeUInt;
begin
  if(cap<FCapacity) then 
    exit
  else if(cap<=2*FCapacity) then 
    IncreaseCapacity
  else 
  begin
    OldEnd:=FCapacity;
    FCapacity:=cap;
    SetLength(FData, FCapacity);
    if FStart > 0 then
      for i:=0 to FStart-1 do
        FData[OldEnd+i]:=FData[i];
  end;
end;

procedure TDeque.Resize(cap:SizeUInt);inline;
begin
  Reserve(cap);
  FDataSize:=cap;
end;

procedure TDeque.Insert(Position: SizeUInt; constref Value: T);
var i:SizeUInt;
begin
  pushBack(Value);
  for i:=Size-1 downto Position+1 do 
  begin
    Move(Mutable[i-1]^, Mutable[i]^, SizeOf(T));
  end;
  Move(Value, Mutable[Position]^, SizeOf(T));
end;

procedure TDeque.Erase(Position: SIzeUInt);
var i:SizeUInt;
begin
  if Position <= Size then 
  begin
    for i:=Position to Size-2 do
    begin
      Move(Mutable[i+1]^, Mutable[i]^, SizeOf(T));
    end;
    popBack();
  end;
end;


end.
