unit ATStringProc_WordJump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATStringProc;

function SFindWordOffset(const S: atString; AOffset: integer; ANext, ABigJump: boolean;
  const AWordChars: atString): integer;
procedure SFindWordBounds(const S: atString; AOffset: integer; out AOffset1, AOffset2: integer;
  const AWordChars: atString);

implementation

const
  //no EOL here, we jump only inside line
  cCharsSp: atString = ' '#9;
  //no chars '@' and '$', word-chars for email-text, php
  cCharsSymb: atString = '!"#%&''()[]{}<>*+-/=,.:;?\^`|~‚„…‹›‘’“”–—¦«»­±';


type
  TCharGr = (cgSp, cgSymb, cgWord);

function SCharGr(ch: atChar; const AWordChars: atString): TCharGr;
begin
  if (AWordChars<>'') and (Pos(ch, AWordChars)>0) then Result:= cgWord else
   if Pos(ch, cCharsSp)>0 then Result:= cgSp else
    if Pos(ch, cCharsSymb)>0 then Result:= cgSymb else
     Result:= cgWord;
end;

function SFindWordOffset(const S: atString; AOffset: integer; ANext,
  ABigJump: boolean; const AWordChars: atString): integer;
var
  n: integer;
  //------------
  procedure Next;
  var gr: TCharGr;
  begin
    if not ((n>=0) and (n<Length(s))) then Exit;
    gr:= SCharGr(s[n+1], AWordChars);
    repeat Inc(n)
    until
      (n>=Length(s)) or (SCharGr(s[n+1], AWordChars)<>gr);
  end;
  //------------
  procedure Home;
  var gr: TCharGr;
  begin
    if not ((n>0) and (n<Length(s))) then Exit;
    gr:= SCharGr(s[n+1], AWordChars);
    while (n>0) and (SCharGr(s[n], AWordChars)=gr) do
      Dec(n);
  end;
  //------------
begin
  n:= AOffset;
  if ANext then
  begin
    Next;
    if ABigJump then
      if (n<Length(s)) and (SCharGr(s[n+1], AWordChars)= cgSp) then
        Next;
  end
  else
  begin
    //if we at word middle, jump to word start
    if (n>0) and (n<Length(s)) and (SCharGr(s[n], AWordChars)=SCharGr(s[n+1], AWordChars)) then
      Home
    else
    begin
      //jump lefter, then jump to prev word start
      if (n>0) then
        begin Dec(n); Home end;
      if ABigJump then
        if (n>0) and (SCharGr(s[n+1], AWordChars)= cgSp) then
          begin Dec(n); Home end;
    end
  end;
  Result:= n;
end;

procedure SFindWordBounds(const S: atString; AOffset: integer; out AOffset1,
  AOffset2: integer; const AWordChars: atString);
begin
  AOffset1:= AOffset;
  AOffset2:= AOffset;

  if (AOffset>=0) and (AOffset<Length(S)) and
    IsWordChar(S[AOffset+1], AWordChars) then
  begin
    //jump left only if at middle of word
    if (AOffset>0) and IsWordChar(S[AOffset], AWordChars) then
      AOffset1:= SFindWordOffset(S, AOffset, false, false, AWordChars);
    //jump right always
    AOffset2:= SFindWordOffset(S, AOffset, true, false, AWordChars);
  end;
end;

end.

