
//Code by Christian Ghisler (ghisler.com)
//Christian gave code to open-source at TCmd forum

unit atstringproc_utf8detect;

interface

//PartialAllowed must be set to true if the buffer is smaller than the file.
function IsBufferUtf8(buf:PAnsiChar;PartialAllowed:boolean):boolean;

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

function GetUtf8CharWidth(firstchar:AnsiChar):integer;
begin
  result:=bytesFromUTF8[firstchar]+1;
end;

function IsFirstUTF8Char(thechar:AnsiChar):boolean;
{The remaining bytes in a multi-byte sequence have 10 as their two most significant bits.}
begin
  result:=(byte(thechar) and (128+64))<>128;
end;

function IsSecondaryUTF8Char(thechar:AnsiChar):boolean;
{The remaining bytes in a multi-byte sequence have 10 as their two most significant bits.}
begin
  result:=(byte(thechar) and (128+64))=128;
end;

function IsBufferUtf8(buf:PAnsiChar;PartialAllowed:boolean):boolean;
{Buffer contains only valid UTF-8 characters, no secondary alone,
no primary without the correct nr of secondary}
var p:PAnsiChar;
    utf8bytes:integer;
    hadutf8bytes:boolean;
begin
  p:=buf;
  hadutf8bytes:=false;
  result:=false;
  utf8bytes:=0;
  while p[0]<>#0 do begin
    if utf8bytes>0 then begin  {Expecting secondary AnsiChar}
      hadutf8bytes:=true;
      if not IsSecondaryUTF8Char(p[0]) then exit;  {Fail!}
      dec(utf8bytes);
    end else if IsFirstUTF8Char(p[0]) then
      utf8bytes:=GetUtf8CharWidth(p[0])-1
    else if IsSecondaryUTF8Char(p[0]) then
      exit;  {Fail!}
    inc(p);
  end;
    result:=hadutf8bytes and (PartialAllowed or (utf8bytes=0));
end;

end.
