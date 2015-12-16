unit ATSynEdit_Keymap;

{$mode objfpc}{$H+}

//{$define test_correct_keynames}

interface

uses
  Classes, SysUtils, Forms;

const
  cMaxKeyCombo = 3; //3 must be enougth for everybody..

type
  TATKeyArray = array[0..Pred(cMaxKeyCombo)] of TShortcut;

function KeyArrayToString(const K: TATKeyArray): string;
function KeyArraysEqualNotEmpty(const a1, a2: TATKeyArray): boolean;

type
  { TATKeymapItem }

  TATKeymapItem = class
  public
    Command: integer;
    Name: string;
    Keys1,
    Keys2: TATKeyArray;
  end;

type
  { TATKeymap }

  TATKeymap = class
  private
    FList: TList;
    FHistory: TATKeyArray;
    function GetItem(N: integer): TATKeymapItem;
    procedure ClearHistory;
    procedure AddToHistory(sh: TShortcut);
    function IsMatchedKeys(const AKeys: TATKeyArray; AKey: TShortcut;
      AAllowOneKey: boolean): boolean;
 public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATKeymapItem read GetItem; default;
    procedure Add(ACmd: integer; const AName: string; const AKeys1, AKeys2: array of string);
    procedure Delete(N: integer);
    function IndexOf(ACmd: integer): integer;
    function GetShortcutFromCommand(ACode: integer): TShortcut;
    function GetCommandFromShortcut(AKey: TShortcut): integer;
  end;

implementation

uses
  Math,
  LCLProc,
  Dialogs;

function GetKeysLen(const AKeys: TATKeyArray): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= Low(AKeys) to High(AKeys) do
    if AKeys[i]<>0 then
      Inc(Result);
end;

{ TATKeymap }

constructor TATKeymap.Create;
begin
  FList:= TList.Create;
  ClearHistory;
end;

destructor TATKeymap.Destroy;
begin
  ClearHistory;
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATKeymap.GetItem(N: integer): TATKeymapItem;
begin
  if IsIndexValid(N) then
    Result:= TATKeymapItem(FList[N])
  else
    Result:= nil;
end;

procedure TATKeymap.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TATKeymap.Count: integer;
begin
  Result:= FList.Count;
end;

function TATKeymap.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function _TextToShortcut(const S: string): TShortcut;
begin
  Result:= TextToShortCut(S);
  {$ifdef test_correct_keynames}
  if Result=0 then
    Showmessage('Incorrect key in keymap: "'+S+'"');
  {$endif}
end;

procedure TATKeymap.Add(ACmd: integer; const AName: string; const AKeys1,
  AKeys2: array of string);
var
  Item: TATKeymapItem;
  i: integer;
begin
  Item:= TATKeymapItem.Create;
  Item.Command:= ACmd;
  Item.Name:= AName;

  FillChar(Item.Keys1, Sizeof(Item.Keys1), 0);
  FillChar(Item.Keys2, Sizeof(Item.Keys2), 0);

  for i:= 0 to Min(High(AKeys1), High(Item.Keys1)) do Item.Keys1[i]:= _TextToShortcut(AKeys1[i]);
  for i:= 0 to Min(High(AKeys2), High(Item.Keys2)) do Item.Keys2[i]:= _TextToShortcut(AKeys2[i]);

  FList.Add(Item);
end;

procedure TATKeymap.Delete(N: integer);
begin
  if IsIndexValid(N) then
    FList.Delete(N);
end;

function TATKeymap.IndexOf(ACmd: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACmd then
      begin Result:= i; Exit end;
end;

function TATKeymap.GetShortcutFromCommand(ACode: integer): TShortcut;
var
  i: integer;
begin
  Result:= scNone;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACode then
    begin
      Result:= Items[i].Keys1[0];
      Exit
    end;
end;

function TATKeymap.GetCommandFromShortcut(AKey: TShortcut): integer;
var
  bCheckSingle: boolean;
  i: integer;
begin
  Result:= 0;

  //first check combos, then check single-keys
  for bCheckSingle:= false to true do
    for i:= 0 to Count-1 do
      if IsMatchedKeys(Items[i].Keys1, AKey, bCheckSingle) or
         IsMatchedKeys(Items[i].Keys2, AKey, bCheckSingle) then
      begin
        Result:= Items[i].Command;
        ClearHistory;
        Exit
      end;

  if AKey>0 then
    AddToHistory(AKey);
end;

function TATKeymap.IsMatchedKeys(const AKeys: TATKeyArray; AKey: TShortcut;
  AAllowOneKey: boolean): boolean;
//function called first for all items with Allow=false (for combos)
//if not found, called for all items with Allow=true (for single keys)
var
  LenThis, LenStack, IndexStack, i: integer;
begin
  Result:= false;

  LenThis:= GetKeysLen(AKeys);
  if LenThis=0 then Exit;

  if LenThis=1 then
  begin
    Result:= AAllowOneKey and (AKeys[0]=AKey);
    Exit
  end;

  //AKey is last in combo AKeys?
  if AKeys[LenThis-1]<>AKey then Exit;

  //stack filled?
  LenStack:= GetKeysLen(FHistory);
  if LenStack<LenThis-1 then
  begin
    //showmessage('no match: if lenstack');
    Exit;
  end;

  //first keys (except last) of combo lie in stack?
  for i:= LenThis-2 downto 0 do
  begin
    IndexStack:= LenStack-1-(LenThis-2-i);
    if (IndexStack>=Low(FHistory)) and (IndexStack<=High(FHistory)) then
      if AKeys[i]<>FHistory[IndexStack] then
      begin
        //showmessage('no match: check items');
        Exit;
      end;
  end;

  Result:= true;
end;

procedure TATKeymap.ClearHistory;
begin
  FillChar(FHistory, Sizeof(FHistory), 0);
end;

procedure TATKeymap.AddToHistory(sh: TShortcut);
var
  len: integer;
begin
  len:= GetKeysLen(FHistory);
  if len>=Length(FHistory) then
  begin
    ClearHistory;
    len:= GetKeysLen(FHistory);
  end;
  FHistory[len]:= sh;
end;


function KeyArrayToString(const K: TATKeyArray): string;
var
  i: integer;
begin
  result:= '';
  for i:= Low(K) to High(K) do
    if K[i]<>0 then
    begin
      if result<>'' then
        result:= result+' * ';
      result:= result+ShortcutToText(K[i]);
    end;
end;

function KeyArraysEqualNotEmpty(const a1, a2: TATKeyArray): boolean;
var
  i: integer;
begin
  Result:= true;

  if a1[0]=0 then Exit(false);
  if a2[0]=0 then Exit(false);

  for i:= Low(a1) to High(a1) do
    if a1[i]<>a2[i] then Exit(false);
end;


end.

