{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Keymap;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

//{$define test_correct_keynames}

interface

uses
  Classes, SysUtils, Forms, ATStringProc;

type
  { TATKeyArray }

  TATKeyArray = record
  private
    const ComboSeparator = '*';
  public
    Data: array[0..2] of TShortcut; //hotkey combo max len, 3 must be enough for everybody
    procedure Clear;
    function Length: integer;
    function ToString: string;
    procedure SetFromString(const AHotkey: string; AComboSepar: char= ComboSeparator);
    class operator =(const a1, a2: TATKeyArray): boolean;
  end;

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
    FList: TFPList;
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
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATKeymapItem read GetItem; default;
    procedure Add(ACmd: integer; const AName: string; const AKeys1, AKeys2: array of string);
    procedure Delete(N: integer);
    function IndexOf(ACmd: integer): integer;
    function GetShortcutFromCommand(ACode: integer): TShortcut;
    function GetCommandFromShortcut(AKey: TShortcut): integer;
    function GetCommandFromHotkeyString(AHotkey: string; AComboSepar: char): integer;
  end;

implementation

uses
  Math,
  LCLProc,
  Dialogs;

{ TATKeyArray }

function TATKeyArray.Length: integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= Low(Data) to High(Data) do
    if Data[i]<>0 then
      Inc(Result);
end;

procedure TATKeyArray.Clear;
begin
  FillChar(Data, SizeOf(Data), 0);
end;

{ TATKeymap }

constructor TATKeymap.Create;
begin
  FList:= TFPList.Create;
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

function TATKeymap.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATKeymap.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function _TextToShortcut(const S: string): TShortcut; inline;
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

  Item.Keys1.Clear;
  Item.Keys2.Clear;

  for i:= 0 to Min(High(AKeys1), High(Item.Keys1.Data)) do Item.Keys1.Data[i]:= _TextToShortcut(AKeys1[i]);
  for i:= 0 to Min(High(AKeys2), High(Item.Keys2.Data)) do Item.Keys2.Data[i]:= _TextToShortcut(AKeys2[i]);

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
  if ACmd<=0 then exit;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACmd then exit(i);
end;

function TATKeymap.GetShortcutFromCommand(ACode: integer): TShortcut;
var
  i: integer;
begin
  Result:= 0; //scNone
  if ACode<=0 then Exit;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACode then
    begin
      //don't get result if combo is set
      if Items[i].Keys1.Data[1]=0 then
        Result:= Items[i].Keys1.Data[0];
      Exit;
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

function TATKeymap.GetCommandFromHotkeyString(AHotkey: string; AComboSepar: char): integer;
var
  Ar: TATKeyArray;
  Item: TATKeymapItem;
  i: integer;
begin
  Result:= -1;
  if AHotkey='' then exit;
  Ar.SetFromString(AHotkey, AComboSepar);
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    if (Item.Keys1=Ar) or
       (Item.Keys2=Ar) then
        exit(i);
  end;
end;

function TATKeymap.IsMatchedKeys(const AKeys: TATKeyArray; AKey: TShortcut;
  AAllowOneKey: boolean): boolean;
//function called first for all items with Allow=false (for combos)
//if not found, called for all items with Allow=true (for single keys)
var
  LenThis, LenStack, IndexStack, i: integer;
begin
  Result:= false;

  LenThis:= AKeys.Length;
  if LenThis=0 then Exit;

  if LenThis=1 then
  begin
    Result:= AAllowOneKey and (AKeys.Data[0]=AKey);
    Exit
  end;

  //AKey is last in combo AKeys?
  if AKeys.Data[LenThis-1]<>AKey then Exit;

  //stack filled?
  LenStack:= FHistory.Length;
  if LenStack<LenThis-1 then
  begin
    //showmessage('no match: if lenstack');
    Exit;
  end;

  //first keys (except last) of combo lie in stack?
  for i:= LenThis-2 downto 0 do
  begin
    IndexStack:= LenStack-1-(LenThis-2-i);
    if (IndexStack>=Low(FHistory.Data)) and (IndexStack<=High(FHistory.Data)) then
      if AKeys.Data[i]<>FHistory.Data[IndexStack] then
      begin
        //showmessage('no match: check items');
        Exit;
      end;
  end;

  Result:= true;
end;

procedure TATKeymap.ClearHistory;
begin
  FHistory.Clear;
end;

procedure TATKeymap.AddToHistory(sh: TShortcut);
var
  len: integer;
begin
  len:= FHistory.Length;
  if len>=Length(FHistory.Data) then
  begin
    ClearHistory;
    len:= 0;
  end;
  FHistory.Data[len]:= sh;
end;

{ TATKeyArray }

function TATKeyArray.ToString: string;
const
  cNiceSeparator = ' '+ComboSeparator+' ';
var
  i: integer;
begin
  result:= '';
  for i:= Low(Data) to High(Data) do
    if Data[i]<>0 then
    begin
      if Result<>'' then
        Result+= cNiceSeparator;
      Result+= ShortcutToText(Data[i]);
    end;
end;

procedure TATKeyArray.SetFromString(const AHotkey: string; AComboSepar: char);
var
  Sep: TATStringSeparator;
  S: string;
  i: integer;
begin
  Clear;
  Sep.Init(AHotkey, AComboSepar);
  for i:= Low(Data) to High(Data) do
  begin
    if not Sep.GetItemStr(S) then Break;
    S:= Trim(S); //Trim to allow near spaces
    if S='' then Break;
    Data[i]:= TextToShortCut(S);
  end;
end;

class operator TATKeyArray.=(const a1, a2: TATKeyArray): boolean;
var
  i: integer;
begin
  Result:= true;

  if a1.Data[0]=0 then Exit(false);
  if a2.Data[0]=0 then Exit(false);

  for i:= Low(a1.Data) to High(a1.Data) do
    if a1.Data[i]<>a2.Data[i] then Exit(false);
end;


end.

