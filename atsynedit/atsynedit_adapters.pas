{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit_CanvasProc;

type
  { TATAdapterHilite }

  TATAdapterHilite = class(TComponent)
  private
    FDynamicHiliteEnabled: boolean;
    FDynamicHiliteMaxLines: integer;
    FDynamicHiliteSupportedInCurLexer: boolean;
    FEditorLineCount: integer;
  public
    constructor Create(AOwner: TComponent); override;
    //
    procedure OnEditorChange(Sender: TObject); virtual;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); virtual;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); virtual;
    procedure OnEditorCaretMove(Sender: TObject); virtual;
    procedure OnEditorScroll(Sender: TObject); virtual;
    procedure OnEditorBeforeCalcHilite(Sender: TObject); virtual;
    //
    property DynamicHiliteEnabled: boolean read FDynamicHiliteEnabled write FDynamicHiliteEnabled;
    property DynamicHiliteMaxLines: integer read FDynamicHiliteMaxLines write FDynamicHiliteMaxLines;
    property DynamicHiliteSupportedInCurLexer: boolean read FDynamicHiliteSupportedInCurLexer write FDynamicHiliteSupportedInCurLexer;
    function DynamicHiliteActiveNow: boolean;
    property EditorLineCount: integer write FEditorLineCount;
  end;

implementation

{ TATAdapterHilite }

constructor TATAdapterHilite.Create(AOwner: TComponent);
begin
  inherited;
  FDynamicHiliteEnabled:= true;
  FDynamicHiliteSupportedInCurLexer:= true;
  FDynamicHiliteMaxLines:= 1000;
  FEditorLineCount:= 0;
end;

procedure TATAdapterHilite.OnEditorChange(Sender: TObject);
begin
  //
end;

procedure TATAdapterHilite.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor);
begin
  //
end;

procedure TATAdapterHilite.OnEditorCalcPosColor(Sender: TObject; AX,
  AY: integer; var AColor: TColor);
begin
  //
end;

procedure TATAdapterHilite.OnEditorCaretMove(Sender: TObject);
begin
  //
end;

procedure TATAdapterHilite.OnEditorScroll(Sender: TObject);
begin
  //
end;

procedure TATAdapterHilite.OnEditorBeforeCalcHilite(Sender: TObject);
begin
  //
end;

function TATAdapterHilite.DynamicHiliteActiveNow: boolean;
begin
  Result:=
    DynamicHiliteEnabled and
    DynamicHiliteSupportedInCurLexer and
    (FEditorLineCount<=DynamicHiliteMaxLines);
end;


end.

