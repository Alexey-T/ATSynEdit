{
Based on code by Christian Ghisler (ghisler.com)
Christian gave code to open-source at Total Commander public forum
Changed for ATSynEdit by Alexey Torgashin (UVviewsoft.com)
}
unit ATStringProc_UTF8Detect;

{$mode objfpc}{$H+}

interface

type
  TBufferUTF8State = (u8sUnknown, u8sYes, u8sNo);

//PartialAllowed must be set to true if the buffer is smaller than the file.
function IsBufferUtf8(buf:PAnsiChar; bufSize:SizeInt; PartialAllowed:boolean): TBufferUTF8State;

implementation

const bytesFromUTF8:array[AnsiChar] of byte = (
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 32
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 64
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  // 96
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  //128
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  //160
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  //192
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  //224
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5); //256

{
function GetUtf8CharWidth(firstchar:AnsiChar):integer;
begin
  result:=bytesFromUTF8[firstchar]+1;
end;
}

function IsFirstUTF8Char(thechar:AnsiChar):boolean; inline;
{The remaining bytes in a multi-byte sequence have 10 as their two most significant bits.}
begin
  result:=(byte(thechar) and (128+64))<>128;
end;

function IsSecondaryUTF8Char(thechar:AnsiChar):boolean; inline;
{The remaining bytes in a multi-byte sequence have 10 as their two most significant bits.}
begin
  result:=(byte(thechar) and (128+64))=128;
end;

function IsBufferUtf8(buf:PAnsiChar; bufSize:SizeInt; PartialAllowed:boolean):TBufferUTF8State;
{Buffer contains only valid UTF-8 characters, no secondary alone,
no primary without the correct nr of secondary}
var
  p:PAnsiChar;
  i:SizeInt;
  utf8bytes:integer;
  hadutf8bytes:boolean;
begin
  p:=buf;
  hadutf8bytes:=false;
  result:=u8sUnknown;
  utf8bytes:=0;
  for i:= 1 to bufSize do
  begin
    if utf8bytes>0 then
    begin  {Expecting secondary AnsiChar}
      hadutf8bytes:=true;
      if not IsSecondaryUTF8Char(p^) then exit(u8sNo);  {Fail!}
      dec(utf8bytes);
    end
    else
    if IsFirstUTF8Char(p^) then
      utf8bytes:=bytesFromUTF8[p^]
    else
    if IsSecondaryUTF8Char(p^) then
      exit(u8sNo);  {Fail!}
    inc(p);
  end;
  if hadutf8bytes and (PartialAllowed or (utf8bytes=0)) then
    result:=u8sYes;
end;

end.
