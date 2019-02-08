{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStringProc_WordJump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATStringProc;

type
  TATWordJump = (
    cWordjumpToNext,
    cWordjumpToEndOrNext,
    cWordjumpToPrev
    );

function SFindWordOffset(const S: atString; AOffset: integer; AJump: TATWordJump; ABigJump: boolean;
  const AWordChars: atString; AJumpSimple: boolean=false): integer;

procedure SFindWordBounds(const S: atString; AOffset: integer; out AOffset1, AOffset2: integer;
  const AWordChars: atString);

implementation

const
  //no chars '@' (email) and '$' (used in php)
  cCharsSymbols: atString = '!"#%&''()[]{}<>*+-/=,.:;?\^`|~‚„…‹›‘’“”–—¦«»­±';


type
  TCharGroup = (cgSpaces, cgSymbols, cgWord);

type
  TCharGroupFunction = function(ch: atChar; const AWordChars: atString): TCharGroup;

function GroupOfChar_Usual(ch: atChar; const AWordChars: atString): TCharGroup;
begin
  if (AWordChars<>'') and (Pos(ch, AWordChars)>0) then Result:= cgWord else
   if (ch=#9) or IsCharSpace(ch) then Result:= cgSpaces else
    if Pos(ch, cCharsSymbols)>0 then Result:= cgSymbols else
     Result:= cgWord;
end;

function GroupOfChar_Simple(ch: atChar; const AWordChars: atString): TCharGroup;
begin
  if (ch=#9) or IsCharSpace(ch) then
    Result:= cgSpaces
  else
    Result:= cgWord;
end;


function SFindWordOffset(const S: atString; AOffset: integer;
  AJump: TATWordJump; ABigJump: boolean;
  const AWordChars: atString;
  AJumpSimple: boolean): integer;
var
  GroupOfChar: TCharGroupFunction;
  n: integer;
  //------------
  procedure Next;
  var gr: TCharGroup;
  begin
    if not ((n>=0) and (n<Length(s))) then Exit;
    gr:= GroupOfChar(s[n+1], AWordChars);
    repeat Inc(n)
    until
      (n>=Length(s)) or (GroupOfChar(s[n+1], AWordChars)<>gr);
  end;
  //------------
  procedure Home;
  var gr: TCharGroup;
  begin
    if not ((n>0) and (n<Length(s))) then Exit;
    gr:= GroupOfChar(s[n+1], AWordChars);
    while (n>0) and (GroupOfChar(s[n], AWordChars)=gr) do
      Dec(n);
  end;
  //------------
  procedure JumpToNext;
  begin
    Next;
    if ABigJump then
      if (n<Length(s)) and (GroupOfChar(s[n+1], AWordChars)=cgSpaces) then
        Next;
  end;
  //------------
  procedure JumpToEnd;
  begin
    while (n<Length(S)) and (GroupOfChar(S[n+1], AWordChars)=cgWord) do
      Inc(n);
  end;
  //------------
begin
  n:= AOffset;

  if AJumpSimple then
    GroupOfChar:= @GroupOfChar_Simple
  else
    GroupOfChar:= @GroupOfChar_Usual;

  case AJump of
    cWordjumpToNext:
      JumpToNext;

    cWordjumpToPrev:
      begin
        //if we at word middle, jump to word start
        if (n>0) and (n<Length(s)) and (GroupOfChar(s[n], AWordChars)=GroupOfChar(s[n+1], AWordChars)) then
          Home
        else
        begin
          //jump lefter, then jump to prev word start
          if (n>0) then
            begin Dec(n); Home end;
          if ABigJump then
            if (n>0) and (GroupOfChar(s[n+1], AWordChars)= cgSpaces) then
              begin Dec(n); Home end;
        end
      end;

    cWordjumpToEndOrNext:
      begin
        JumpToEnd;
        //not moved? jump again
        if n=AOffset then
        begin
          JumpToNext;
          JumpToEnd;
        end;
      end;
  end;

  Result:= n;
end;


procedure SFindWordBounds(const S: atString; AOffset: integer; out AOffset1,
  AOffset2: integer; const AWordChars: atString);
begin
  AOffset1:= AOffset;
  AOffset2:= AOffset;
  if S='' then exit;

  //pos at end
  if (AOffset=Length(S)) then Dec(AOffset);

  if (AOffset>=0) and (AOffset<Length(S)) then
  begin
    //not on wrdchar?
    //move left
    if (AOffset>0) and not IsCharWord(S[AOffset+1], AWordChars) then
      Dec(AOffset);

    //not on wrdchar? exit
    if not IsCharWord(S[AOffset+1], AWordChars) then exit;

    //jump left only if at middle of word
    if (AOffset>0) and IsCharWord(S[AOffset], AWordChars) then
      AOffset1:= SFindWordOffset(S, AOffset, cWordjumpToPrev, false, AWordChars);
    //jump right always
    AOffset2:= SFindWordOffset(S, AOffset, cWordjumpToNext, false, AWordChars);
  end;
end;

end.

