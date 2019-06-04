{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Messages,
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
    //called when editor's text changes.

    procedure OnEditorIdle(Sender: TObject); virtual;
    //called after text is changed, and pause passed (OptIdleInterval)
    //fast changes (faster than OptIdleInterval): called only after last change

    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); virtual;
    //called to calculate hilite of entire line.
    //ACharIndex is starting offset in this line, >0 if editor scrolled horizontally.
    //ALineLen is len of line part, starting from ACharIndex.

    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); virtual;
    //called to calculate BG/background color at position (usually pos after line end).

    procedure OnEditorCaretMove(Sender: TObject); virtual;
    //called after caret is moved.

    procedure OnEditorScroll(Sender: TObject); virtual;
    //called after editor is scrolled horz/vert.

    procedure OnEditorBeforeCalcHilite(Sender: TObject); virtual;
    //called before calculation of hilites for n lines, before 1st of these lines.
    //adapter should prepare buffers here for next lines.

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
    //max count of lines, to use dyn-hiliting (for EControl its slow for 10K lines)
    //app must set it, e.g. 5K is ok

    property DynamicHiliteSupportedInCurrentSyntax: boolean
      read FDynamicHiliteSupportedInCurrentSyntax
      write FDynamicHiliteSupportedInCurrentSyntax;
    //real adapter (subclass of this class) must set it.
    //EControl adapter calculates it from lexer-file.

    function DynamicHiliteActiveNow(ALinesCount: integer): boolean;
    //resulting bool, calculated from above props, and current count of lines.
    //ATSynEdit reads it.
  end;

type
  { TATAdapterIME }

  TATAdapterIME = class
  public
    procedure Stop(Sender: TObject; Success: boolean); virtual;
    procedure Request(Sender: TObject; var Msg: TMessage); virtual;
    procedure Notify(Sender: TObject; var Msg: TMessage); virtual;
    procedure StartComposition(Sender: TObject; var Msg: TMessage); virtual;
    procedure Composition(Sender: TObject; var Msg: TMessage); virtual;
    procedure EndComposition(Sender: TObject; var Msg: TMessage); virtual;
  end;


implementation

{ TATAdapterIME }

procedure TATAdapterIME.Stop(Sender: TObject; Success: boolean);
begin
  //
end;

procedure TATAdapterIME.Request(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.Notify(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.StartComposition(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.Composition(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.EndComposition(Sender: TObject; var Msg: TMessage);
begin
  //
end;

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

procedure TATAdapterHilite.OnEditorIdle(Sender: TObject);
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

