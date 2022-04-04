{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Gutter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Globals;

type
  TATGutterItem = class
  public
    Tag: Int64;
    Visible: boolean;
    Size: integer; //column width in pixels
    Scaled: boolean; //adjust column width using ATEditorScale()
    Left, Right: integer; //these are calculated by TATGutter.Update()
  end;

type

  { TATGutter }

  TATGutter = class
  private
    FList: TFPList;
    function GetItem(N: integer): TATGutterItem;
  public
    GutterCoordLeft: integer;
    constructor Create; virtual;
    destructor Destroy; override;
    function IsIndexValid(N: integer): boolean; inline;
    procedure Add(AIndex: integer; ASize: integer; ATag: Int64; AScaled, AVisible: boolean);
    procedure Delete(N: integer);
    procedure Move(AIndexCur, AIndexNew: integer);
    procedure Clear;
    function Count: integer; inline;
    property Items[N: integer]: TATGutterItem read GetItem; default;
    function Width: integer;
    procedure Update;
    function FindIndexAtCoordX(AX: integer): integer;
    function FindIndexByTag(ATag: Int64): integer;
  end;


implementation

{ TATGutter }

function TATGutter.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATGutter.GetItem(N: integer): TATGutterItem;
begin
  if IsIndexValid(N) then
    Result:= TATGutterItem(FList[N])
  else
    Result:= nil;
end;

constructor TATGutter.Create;
begin
  inherited;
  FList:= TFPList.Create;
end;

destructor TATGutter.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATGutter.Add(AIndex: integer; ASize: integer; ATag: Int64; AScaled,
  AVisible: boolean);
var
  Item: TATGutterItem;
begin
  Item:= TATGutterItem.Create;
  Item.Size:= ASize;
  Item.Tag:= ATag;
  Item.Scaled:= AScaled;
  Item.Visible:= AVisible;
  if AIndex<0 then
    FList.Add(Item)
  else
    FList.Insert(AIndex, Item);
end;

procedure TATGutter.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

procedure TATGutter.Move(AIndexCur, AIndexNew: integer);
begin
  FList.Move(AIndexCur, AIndexNew);
end;

procedure TATGutter.Clear;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    Delete(i);
end;

function TATGutter.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATGutter.Width: integer;
begin
  if Count>0 then
    Result:= Items[Count-1].Right - GutterCoordLeft
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
        Left:= GutterCoordLeft;
      Right:= Left;
      if Visible then
      begin
        if Scaled then
          Inc(Right, ATEditorScale(Size))
        else
          Inc(Right, Size);
      end;
    end;
end;

function TATGutter.FindIndexAtCoordX(AX: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    with Items[i] do
      if (AX>=Left) and (AX<Right) then
        Exit(i);
end;

function TATGutter.FindIndexByTag(ATag: Int64): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    if Items[i].Tag=ATag then
      Exit(i);
end;

end.
