{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Gutter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TATGutterItem = class
    Visible: boolean;
    Size: integer;
    Left, Right: integer;
  end;

type

  { TATGutter }

  TATGutter = class
  private
    FList: TList;
    function GetItem(N: Integer): TATGutterItem;
  public
    GutterLeft: integer;
    constructor Create; virtual;
    destructor Destroy; override;
    function IsIndexValid(N: integer): boolean;
    procedure Add(ASize: integer);
    procedure Delete(N: integer);
    procedure Clear;
    function Count: integer;
    property Items[N: integer]: TATGutterItem read GetItem; default;
    function Width: integer;
    procedure Update;
    function IndexAt(AX: integer): integer;
  end;


implementation

{ TATGutter }

function TATGutter.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATGutter.GetItem(N: Integer): TATGutterItem;
begin
  if IsIndexValid(N) then
    Result:= TATGutterItem(FList[N])
  else
    Result:= nil;
end;

constructor TATGutter.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATGutter.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATGutter.Add(ASize: integer);
var
  Item: TATGutterItem;
begin
  Item:= TATGutterItem.Create;
  Item.Size:= ASize;
  Item.Visible:= true;
  FList.Add(Item);
  Update;
end;

procedure TATGutter.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
  Update;
end;

procedure TATGutter.Clear;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    Delete(i);
end;

function TATGutter.Count: integer;
begin
  Result:= FList.Count;
end;

function TATGutter.Width: integer;
begin
  if Count>0 then
    Result:= Items[Count-1].Right - GutterLeft
  else
    Result:= 0;
end;

procedure TATGutter.Update;
var
  i: integer;
begin
  for i:= 0 to Count-1 do
    with Items[i] do
    begin
      if i>0 then
        Left:= Items[i-1].Right
      else
        Left:= GutterLeft;
      Right:= Left;
      if Visible then
        Inc(Right, Size);
    end;
end;

function TATGutter.IndexAt(AX: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    with Items[i] do
      if (AX>=Left) and (AX<Right) then
      begin
        Result:= i;
        Exit
      end;
end;

end.

