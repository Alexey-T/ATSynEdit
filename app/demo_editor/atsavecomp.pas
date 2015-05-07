unit atsavecomp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SaveComponentToFile(Component: TComponent; const FileName: string);
procedure LoadComponentFromFile(Component: TComponent; const FileName: string; OnError: TReaderError);


implementation


procedure SaveComponentToStream(Component: TComponent; Stream: TStream);
var
  MemSt: TStream;
  Writer: TWriter;
begin
  MemSt := TMemoryStream.Create;
  try
    if Component.Owner = nil then
      MemSt.WriteComponent(Component)
    else
      begin
        Writer := TWriter.Create(MemSt, 4096);
        try
          Writer.Root := Component.Owner;
          //Delphi needs Writer.WriteSignature
          Writer.WriteComponent(Component);
        finally
          FreeAndNil(Writer);
        end;
      end;
    MemSt.Position := 0;
    ObjectBinaryToText(MemSt, Stream);
  finally
    FreeAndNil(MemSt);
  end
end;

procedure SaveComponentToFile(Component: TComponent; const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveComponentToStream(Component, Stream);
  finally
    FreeAndNil(Stream);
  end
end;

procedure LoadComponentFromStream(Component: TComponent; Stream: TStream; OnError: TReaderError = nil);
var
  MemSt: TStream;
  Reader: TReader;
  Sign: array[0..3] of char = '    ';
begin
   MemSt := TMemoryStream.Create;
   try
     ObjectTextToBinary(Stream, MemSt);
     MemSt.Position := 0;
     Reader := TReader.Create(MemSt, 4096);
     Reader.OnError := OnError;
    try
      if Component.Owner = nil then
        Reader.ReadRootComponent(Component)
      else
        begin
            Reader.Root := Component.Owner;

            //Reader.ReadSignature; //AT
            Reader.Read(Sign, SizeOf(Sign));

            Reader.BeginReferences;
            try
              Reader.ReadComponent(Component);
              Reader.FixupReferences;
            finally
              Reader.EndReferences;
            end;
        end;
    finally
      FreeAndNil(Reader);
    end;
  finally
    FreeAndNil(MemSt);
  end;
end;

procedure LoadComponentFromFile(Component: TComponent; const FileName: string; OnError: TReaderError);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadComponentFromStream(Component, Stream, OnError);
  finally
    FreeAndNil(Stream);
  end;
end;

end.

