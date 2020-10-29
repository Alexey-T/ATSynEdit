{
Source code is taken from FPC 3.2fixes, and changed by Alexey Torgashin.
License: same as for FPC.
}
unit ATStringProc_UTF8Decode;

interface

procedure CustomUTF8Decode(const s: RawByteString; out Res: UnicodeString; out Error: boolean);
function CustomUTF8ToUnicode(Dest: PUnicodeChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;
procedure RaiseUTF8TextError;

implementation

uses SysUtils;

type
  EBadUTF8Text = class(Exception);

procedure RaiseUTF8TextError;
begin
  raise EBadUTF8Text.Create('UTF-8 decode error');
end;

function CustomUTF8ToUnicode(Dest: PUnicodeChar; MaxDestChars: SizeUInt; Source: PChar; SourceBytes: SizeUInt): SizeUInt;
var
  InputUTF8: SizeUInt;
  IBYTE: BYTE;
  OutputUnicode: SizeUInt;
  TempBYTE: BYTE;
  CharLen: SizeUint;
  LookAhead: SizeUInt;
  UC: SizeUInt;
begin
  result:=SizeUInt(-1);
  InputUTF8:=0;
  OutputUnicode:=0;
  if Dest=nil Then
    raise Exception.Create('UTF8 dest is nil');
    
  while (OutputUnicode<MaxDestChars) and (InputUTF8<SourceBytes) do
    begin
      IBYTE:=byte(Source[InputUTF8]);
      if (IBYTE and $80) = 0 then
        begin
          Dest[OutputUnicode]:=WideChar(IBYTE);
          inc(OutputUnicode);
          inc(InputUTF8);
        end
      else
        begin
          TempByte:=IBYTE;
          CharLen:=0;
          while (TempBYTE and $80)<>0 do
            begin
              TempBYTE:=(TempBYTE shl 1) and $FE;
              inc(CharLen);
            end;
          //Test for the "CharLen" conforms UTF-8 string
          //This means the 10xxxxxx pattern.
          if SizeUInt(InputUTF8+CharLen-1)>SourceBytes then
            begin
              //Insuficient chars in string to decode
              //UTF-8 array. Fallback to single char.
              CharLen:= 1;
            end;
          for LookAhead := 1 to CharLen-1 do
            begin
              if ((byte(Source[InputUTF8+LookAhead]) and $80)<>$80) or
                 ((byte(Source[InputUTF8+LookAhead]) and $40)<>$00) then
                begin
                  //Invalid UTF-8 sequence, fallback.
                  CharLen:= LookAhead;
                  break;
                end;
            end;
          UC:=$FFFF;
          case CharLen of
            1:  begin
                  //Not valid UTF-8 sequence
                  RaiseUTF8TextError;
                end;
            2:  begin
                  //Two bytes UTF, convert it
                  UC:=(byte(Source[InputUTF8]) and $1F) shl 6;
                  UC:=UC or (byte(Source[InputUTF8+1]) and $3F);
                  if UC <= $7F then
                    begin
                      //Invalid UTF sequence.
                      RaiseUTF8TextError;
                    end;
                end;
            3:  begin
                  //Three bytes, convert it to unicode
                  UC:= (byte(Source[InputUTF8]) and $0F) shl 12;
                  UC:= UC or ((byte(Source[InputUTF8+1]) and $3F) shl 6);
                  UC:= UC or ((byte(Source[InputUTF8+2]) and $3F));
                  if (UC <= $7FF) or (UC >= $FFFE) or ((UC >= $D800) and (UC <= $DFFF)) then
                    begin
                      //Invalid UTF-8 sequence
                      RaiseUTF8TextError;
                    End;
                end;
            4:  begin
                  //Four bytes, convert it to two unicode characters
                  UC:= (byte(Source[InputUTF8]) and $07) shl 18;
                  UC:= UC or ((byte(Source[InputUTF8+1]) and $3F) shl 12);
                  UC:= UC or ((byte(Source[InputUTF8+2]) and $3F) shl 6);
                  UC:= UC or ((byte(Source[InputUTF8+3]) and $3F));
                  if (UC < $10000) or (UC > $10FFFF) then
                    begin
                      RaiseUTF8TextError;
                    end
                  else
                    begin
                      { only store pair if room }
                      dec(UC,$10000);
                      if (OutputUnicode<MaxDestChars-1) then
                        begin
                          Dest[OutputUnicode]:=WideChar(UC shr 10 + $D800);
                          inc(OutputUnicode);
                          UC:=(UC and $3ff) + $DC00;
                        end
                      else
                        begin
                          InputUTF8:= InputUTF8 + CharLen;
                          { don't store anything }
                          CharLen:=0;
                        end;
                    end;
                end;
            5,6,7:  begin
                      //Invalid UTF8 to unicode conversion,
                      //mask it as invalid UNICODE too.
                      RaiseUTF8TextError;
                    end;
          end;
          if CharLen > 0 then
            begin
              Dest[OutputUnicode]:=WideChar(UC);
              inc(OutputUnicode);
            end;
          InputUTF8:= InputUTF8 + CharLen;
        end;
    end;
  Result:=OutputUnicode+1;
end;

           
procedure CustomUTF8Decode(const s: RawByteString; out Res: UnicodeString; out Error: boolean);
var
  i : SizeInt;
  hs : UnicodeString;
begin
  Res:='';
  Error:=false;
  if s='' then
    exit;
  SetLength(hs,length(s));
  try
    i:=CustomUtf8ToUnicode(PUnicodeChar(hs),length(hs)+1,pchar(s),length(s));
    if i>0 then
      begin
        SetLength(hs,i-1);
        Res:=hs;
      end;
  except
    Error:=true;
  end;
end;

end.
