unit ATKeyMapping;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TATKeyMappingItem = class
    Cmd: integer;
    Name: string;
    Keys1, Keys2: array[0..0] of string;
  end;

type
  { TATKeyMapping }

  TATKeyMapping = class
  private
    FList: TList;
    function GetItem(N: integer): TATKeyMappingItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATKeyMappingItem read GetItem; default;
    procedure Add(ACmd: integer; const AName: string; const AKeys1,
      AKeys2: array of string);
  end;

implementation

{ TATKeyMapping }

constructor TATKeyMapping.Create;
begin
  FList:= TList.Create;
end;

destructor TATKeyMapping.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATKeyMapping.GetItem(N: integer): TATKeyMappingItem;
begin
  if IsIndexValid(N) then
    Result:= TATKeyMappingItem(FList[N])
  else
    Result:= nil;
end;

procedure TATKeyMapping.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    Delete(i);
end;

procedure TATKeyMapping.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

function TATKeyMapping.Count: integer;
begin
  Result:= FList.Count;
end;

function TATKeyMapping.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

procedure TATKeyMapping.Add(ACmd: integer; const AName: string;
  const AKeys1, AKeys2: array of string);
var
  Item: TATKeyMappingItem;
  i: integer;
begin
  Item:= TATKeyMappingItem.Create;
  Item.Cmd:= ACmd;
  Item.Name:= AName;
  for i:= 0 to High(Item.Keys1) do Item.Keys1[i]:= '';
  for i:= 0 to High(Item.Keys2) do Item.Keys2[i]:= '';
  for i:= 0 to High(AKeys1) do Item.Keys1[i]:= AKeys1[i];
  for i:= 0 to High(AKeys2) do Item.Keys2[i]:= AKeys2[i];
  FList.Add(Item);
end;


end.

