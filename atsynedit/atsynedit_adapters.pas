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
    FDynamicHiliteSupportedInCurrentSyntax: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddEditor(AEditor: TComponent); virtual;
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
    property DynamicHiliteEnabled: boolean
      read FDynamicHiliteEnabled
      write FDynamicHiliteEnabled;
    //dyn-hiliting global enabled flag.
    //app must set it.
    //dyn-hiliting is on, if some chars colors depend on caret position,
    //e.g. in EControl HTML lexer: hilites of < > change, if caret is near < >
    property DynamicHiliteMaxLines: integer
      read FDynamicHiliteMaxLines
      write FDynamicHiliteMaxLines;
    //app must set it, e.g. 5000 is ok
    property DynamicHiliteSupportedInCurrentSyntax: boolean
      read FDynamicHiliteSupportedInCurrentSyntax
      write FDynamicHiliteSupportedInCurrentSyntax;
    //real adapter (subclass of this class) must set it.
    //EControl adapter calculates it from lexer-file.
    function DynamicHiliteActiveNow(ALinesCount: integer): boolean;
    //resulting bool, calculated from above props, and current count of lines.
    //ATSynEdit reads it.
  end;

implementation

{ TATAdapterHilite }

constructor TATAdapterHilite.Create(AOwner: TComponent);
begin
  inherited;
  FDynamicHiliteEnabled:= true;
  FDynamicHiliteSupportedInCurrentSyntax:= true;
  FDynamicHiliteMaxLines:= 1000;
end;

procedure TATAdapterHilite.AddEditor(AEditor: TComponent);
begin
  // not nil: adapter adds this editor object to his editors list,
  //   and should setup editor's OnLog
  // nil: adapter forgets about all editors
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

function TATAdapterHilite.DynamicHiliteActiveNow(ALinesCount: integer): boolean;
begin
  Result:=
    DynamicHiliteEnabled and
    DynamicHiliteSupportedInCurrentSyntax and
    (ALinesCount<=DynamicHiliteMaxLines);
end;


end.

