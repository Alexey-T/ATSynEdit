{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_WordJump;

{$mode objfpc}{$H+}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils,
  ATStringProc;

type
  TATWordJump = (
    Next,
    EndOrNext,
    Prev,
    NextByMouse,
    PrevByMouse
    );

function SFindWordOffset(const S: atString; AOffset: integer; AJump: TATWordJump; ABigJump: boolean;
  const ANonWordChars: atString; AJumpSimple: boolean=false): integer;

procedure SFindWordBounds(const S: atString; AOffset: integer; out AOffset1, AOffset2: integer;
  const ANonWordChars: atString);

procedure SFindSymbolsBounds(const S: atString; AOffset: integer; out AOffset1,
  AOffset2: integer);

procedure SFindSpacesBounds(const S: atString; AOffset: integer; out AOffset1,
  AOffset2: integer);


implementation

(*
const
  //no chars '@' (email) and '$' (used in php)
  cCharsSymbols: atString = '!"#%&''()[]{}<>*+-/=,.:;?\^`|~‚„…‹›‘’“”–—¦«»­±';
*)

type
  TCharGroup = (
    Spaces,
    Symbols,
    Word
    );

type
  TCharGroupFunction = function(ch: atChar; const ANonWordChars: atString): TCharGroup;

function GroupOfChar_Usual(ch: atChar; const ANonWordChars: atString): TCharGroup;
begin
  if (ch=#9) or IsCharSpace(ch) then
    Result:= TCharGroup.Spaces
  else
  if IsCharWord(ch, ANonWordChars) then
    Result:= TCharGroup.Word
  else
    Result:= TCharGroup.Symbols;
end;

function GroupOfChar_Simple(ch: atChar; const ANonWordChars: atString): TCharGroup;
begin
  if (ch=#9) or IsCharSpace(ch) then
    Result:= TCharGroup.Spaces
  else
    Result:= TCharGroup.Word;
end;


function SFindWordOffset(const S: atString; AOffset: integer;
  AJump: TATWordJump; ABigJump: boolean;
  const ANonWordChars: atString;
  AJumpSimple: boolean): integer;
var
  GroupOfChar: TCharGroupFunction;
  //------------
  procedure Next(var n: integer);
  var gr: TCharGroup;
  begin
    if not ((n>=0) and (n<Length(s))) then Exit;
    gr:= GroupOfChar(s[n+1], ANonWordChars);
    repeat Inc(n)
    until
      (n>=Length(s)) or (GroupOfChar(s[n+1], ANonWordChars)<>gr);
  end;
  //------------
  procedure Home(var n: integer);
  var gr: TCharGroup;
  begin
    if not ((n>0) and (n<Length(s))) then Exit;
    gr:= GroupOfChar(s[n+1], ANonWordChars);
    while (n>0) and (GroupOfChar(s[n], ANonWordChars)=gr) do
      Dec(n);
  end;
  //------------
  procedure JumpToNext(var n: integer);
  begin
    Next(n);
    if ABigJump then
      if (n<Length(s)) and (GroupOfChar(s[n+1], ANonWordChars)=TCharGroup.Spaces) then
        Next(n);
  end;
  //------------
  procedure JumpToEnd(var n: integer);
  begin
    while (n<Length(S)) and (GroupOfChar(S[n+1], ANonWordChars)=TCharGroup.Word) do
      Inc(n);
  end;
  //------------
var
  n, NLen: integer;
begin
  n:= AOffset;
  NLen:= Length(S);
  if NLen=0 then exit;

  if AJumpSimple then
    GroupOfChar:= @GroupOfChar_Simple
  else
    GroupOfChar:= @GroupOfChar_Usual;

  case AJump of
    TATWordJump.NextByMouse:
      begin
        while (n<NLen) and
          ((n=0) or IsCharWord(S[n], ANonWordChars)) and
          IsCharWord(S[n+1], ANonWordChars) do
          Inc(n);
      end;

    TATWordJump.PrevByMouse:
      begin
        while (n>0) and (n<=NLen) and
          IsCharWord(S[n], ANonWordChars) and
          ((n=NLen) or IsCharWord(S[n+1], ANonWordChars)) do
          Dec(n);
      end;

    TATWordJump.Next:
      JumpToNext(n);

    TATWordJump.Prev:
      begin
        //if we at word middle, jump to word start
        if (n>0) and (n<Length(s)) and (GroupOfChar(s[n], ANonWordChars)=GroupOfChar(s[n+1], ANonWordChars)) then
          Home(n)
        else
        begin
          //jump lefter, then jump to prev word start
          if (n>0) then
            begin Dec(n); Home(n); end;
          if ABigJump then
            if (n>0) and (GroupOfChar(s[n+1], ANonWordChars)<>TCharGroup.Word) then
              begin Dec(n); Home(n); end;
        end
      end;

    TATWordJump.EndOrNext:
      begin
        JumpToEnd(n);
        //not moved? jump again
        if n=AOffset then
        begin
          JumpToNext(n);
          JumpToEnd(n);
        end;
      end;
  end;

  Result:= n;
end;


procedure SFindWordBounds(const S: atString; AOffset: integer; out AOffset1,
  AOffset2: integer; const ANonWordChars: atString);
begin
  AOffset1:= AOffset;
  AOffset2:= AOffset;
  if S='' then exit;

  //pos at end
  if (AOffset=Length(S)) then Dec(AOffset);

  if (AOffset>=0) and (AOffset<Length(S)) then
  begin
    //not on word-char?
    //move left
    if (AOffset>0) and not IsCharWord(S[AOffset+1], ANonWordChars) then
      Dec(AOffset);

    //not on word-char? exit
    if not IsCharWord(S[AOffset+1], ANonWordChars) then exit;

    //for caret initially after length=1 word
    AOffset1:= AOffset;

    //jump left only if at middle of word
    if (AOffset>0) and IsCharWord(S[AOffset], ANonWordChars) then
      AOffset1:= SFindWordOffset(S, AOffset, TATWordJump.Prev, false, ANonWordChars);

    //jump right always
    AOffset2:= SFindWordOffset(S, AOffset, TATWordJump.Next, false, ANonWordChars);
  end;
end;


procedure SFindSymbolsBounds(const S: atString; AOffset: integer; out AOffset1, AOffset2: integer);
begin
  AOffset1:= AOffset;
  AOffset2:= AOffset;
  if S='' then exit;

  //pos at end
  if (AOffset=Length(S)) then Dec(AOffset);

  if (AOffset>=0) and (AOffset<Length(S)) then
  begin
    while (AOffset1>0) and IsCharSymbol(S[AOffset1]) do
      Dec(AOffset1);

    while (AOffset2<Length(S)) and IsCharSymbol(S[AOffset2+1]) do
      Inc(AOffset2);
  end;
end;

procedure SFindSpacesBounds(const S: atString; AOffset: integer; out AOffset1, AOffset2: integer);
begin
  AOffset1:= AOffset;
  AOffset2:= AOffset;
  if S='' then exit;

  //pos at end
  if (AOffset=Length(S)) then Dec(AOffset);

  if (AOffset>=0) and (AOffset<Length(S)) then
  begin
    while (AOffset1>0) and IsCharSpace(S[AOffset1]) do
      Dec(AOffset1);

    while (AOffset2<Length(S)) and IsCharSpace(S[AOffset2+1]) do
      Inc(AOffset2);
  end;
end;


end.

