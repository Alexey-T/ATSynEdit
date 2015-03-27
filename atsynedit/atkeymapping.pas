unit ATKeyMapping;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TATKeyMappingItem = class
    Cmd: integer;
    Name: string;
    Keys1, Keys2: array[0..0] of TShortcut;
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
    function GetShortcutFromCommand(Cmd: integer): TShortcut;
  end;

implementation

uses
  LCLProc,
  Dialogs;

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

procedure TATKeyMapping.Add(ACmd: integer; const AName: string; const AKeys1,
  AKeys2: array of string);
var
  Item: TATKeyMappingItem;
  Sh: TShortcut;
  i: integer;
begin
  Item:= TATKeyMappingItem.Create;
  Item.Cmd:= ACmd;
  Item.Name:= AName;

  for i:= 0 to High(Item.Keys1) do Item.Keys1[i]:= 0;
  for i:= 0 to High(Item.Keys2) do Item.Keys2[i]:= 0;

  for i:= 0 to High(AKeys1) do
  begin
    Sh:= TextToShortCut(AKeys1[i]);
    if Sh=0 then Showmessage('Incorrect key: '+AKeys1[i]);
    Item.Keys1[i]:= Sh;
  end;

  for i:= 0 to High(AKeys2) do
  begin
    Sh:= TextToShortCut(AKeys2[i]);
    if Sh=0 then Showmessage('Incorrect key: '+AKeys2[i]);
    Item.Keys2[i]:= Sh;
  end;
  FList.Add(Item);
end;

function TATKeyMapping.GetShortcutFromCommand(Cmd: integer): TShortcut;
var
  i: integer;
begin
  Result:= scNone;
  for i:= 0 to Count-1 do
    if Items[i].Cmd=Cmd then
    begin
      Result:= Items[i].Keys1[0];
      Exit
    end;
end;


end.

