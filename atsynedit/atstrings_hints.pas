{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStrings_Hints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  cSyneditMaxHints = 250;

type
  { TATHintList }

  TATHintList = class
  private
    FList: TStringList;
    function GetItem(Index: integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: integer]: string read GetItem; default;
    procedure Clear;
    function Add(const S: string): integer;
  end;

implementation

{ TATHintList }

procedure TATHintList.Clear;
begin
  FList.Clear;
  FList.Add('?'); //index 0 busy
end;

function TATHintList.GetItem(Index: integer): string;
begin
  if (Index>0) {0 is busy} and (Index<FList.Count) then
    Result:= FList[Index]
  else
    Result:= '';
end;

constructor TATHintList.Create;
begin
  FList:= TStringList.Create;
  Clear;
end;

destructor TATHintList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TATHintList.Add(const S: string): integer;
begin
  if FList.Count>=cSyneditMaxHints then
    Result:= 0
  else
  begin
    FList.Add(S);
    Result:= FList.Count-1;
  end;
end;

end.

