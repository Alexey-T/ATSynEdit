{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Hotspots;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$Z1}

interface

uses
  Classes, SysUtils,
  ATSynEdit_fgl;

type
  TATHotspotItem = record
    PosX, PosY: integer;
    EndX, EndY: integer;
    class operator=(const A, B: TATHotspotItem): boolean;
  end;

type
  TATHotspotItems = specialize TFPGList<TATHotspotItem>;

type
  { TATHotspots }

  TATHotspots = class
  private
    FList: TATHotspotItems;
    function GetItem(AIndex: integer): TATHotspotItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATHotspotItem read GetItem; default;
    procedure Add(const AItem: TATHotspotItem);
    procedure Delete(N: integer);
    procedure Insert(N: integer; const AItem: TATHotspotItem);
  end;


implementation

{ TATHotspotItem }

class operator TATHotspotItem.=(const A, B: TATHotspotItem): boolean;
begin
  Result:= false;
end;

{ TATHotspots }

function TATHotspots.GetItem(AIndex: integer): TATHotspotItem;
begin
  Result:= FList[AIndex];
end;

constructor TATHotspots.Create;
begin
  FList:= TATHotspotItems.Create;
end;

destructor TATHotspots.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATHotspots.Clear;
begin
  FList.Clear;
end;

function TATHotspots.Count: integer;
begin
  Result:= FList.Count;
end;

function TATHotspots.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<Count);
end;

procedure TATHotspots.Add(const AItem: TATHotspotItem);
begin
  FList.Add(AItem);
end;

procedure TATHotspots.Delete(N: integer);
begin
  FList.Delete(N);
end;

procedure TATHotspots.Insert(N: integer; const AItem: TATHotspotItem);
begin
  if N>=FList.Count then
    FList.Add(AItem)
  else
    FList.Insert(N, AItem);
end;

end.

