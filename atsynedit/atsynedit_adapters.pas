{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Messages,
  ATStringProc,
  ATSynEdit_LineParts;

type
  { TATAdapterHilite }

  TATAdapterHilite = class(TComponent)
  private
    FDynamicHiliteEnabled: boolean;
    FDynamicHiliteMaxLines: integer;
    FDynamicHiliteSupportedInCurrentSyntax: boolean;
    FImplementsDataReady: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    //
    procedure OnEditorChange(Sender: TObject); virtual;
    //called when editor's text changes.

    procedure OnEditorChangeEx(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: integer); virtual;
    //detailed version of OnEditorChange

    procedure OnEditorIdle(Sender: TObject); virtual;
    //called after text is changed, and pause passed (OptIdleInterval)
    //fast changes (faster than OptIdleInterval): called only after last change

    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor;
      AMainText: boolean); virtual;
    //called to calculate hilite of entire line.
    //ACharIndex is starting offset in this line, >0 if editor scrolled horizontally.
    //ALineLen is len of line part, starting from ACharIndex.

    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); virtual;
    //called to calculate BG/background color at position (usually pos after line end).

    procedure OnEditorCalcPosForeground(Sender: TObject;
      AX, AY: integer; var AColor: TColor; var AFontStyles: TFontStyles); virtual;
    //called to calculate foreground/font color at position

    procedure OnEditorCaretMove(Sender: TObject); virtual;
    //called after caret is moved.

    procedure OnEditorScroll(Sender: TObject); virtual;
    //called after editor is scrolled horz/vert.

    procedure OnEditorBeforeCalcHilite(Sender: TObject); virtual;
    //called before calculation of hilites for n lines, before 1st of these lines.
    //adapter should prepare buffers here for next lines.

    function IsParsedAtLeastPartially: boolean; virtual;
    //returns False to supress unneeded painting, when parsing is not done

    function GetLexerName: string; virtual;
    //return lexer name

    property ImplementsDataReady: boolean read FImplementsDataReady write FImplementsDataReady;
    function IsDataReady: boolean; virtual;
    function IsDataReadyPartially: boolean; virtual;
    //return False to prevent Minimap repainting (avoid Minimap flicker during typing)

    //
    //dynamic-highlighting = some elements colors depend on caret position.
    //e.g. in EControl HTML lexer: highlight of < > is changed, when caret is near < >.
    //
    property DynamicHiliteEnabled: boolean
      read FDynamicHiliteEnabled
      write FDynamicHiliteEnabled;
    //dynamic-highlighting global enabled flag.
    //app must set it.

    property DynamicHiliteMaxLines: integer
      read FDynamicHiliteMaxLines
      write FDynamicHiliteMaxLines;
    //max count of lines, to use dynamic-highlighting (for EControl its slow for 10K lines)
    //app must set it, e.g. 5K is ok

    property DynamicHiliteSupportedInCurrentSyntax: boolean
      read FDynamicHiliteSupportedInCurrentSyntax
      write FDynamicHiliteSupportedInCurrentSyntax;
    //adapter must set it.
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
    procedure ImeRequest(Sender: TObject; var Msg: TMessage); virtual;
    procedure ImeNotify(Sender: TObject; var Msg: TMessage); virtual;
    procedure ImeStartComposition(Sender: TObject; var Msg: TMessage); virtual;
    procedure ImeComposition(Sender: TObject; var Msg: TMessage); virtual;
    procedure ImeEndComposition(Sender: TObject; var Msg: TMessage); virtual;
  end;


implementation

{ TATAdapterIME }

procedure TATAdapterIME.Stop(Sender: TObject; Success: boolean);
begin
  //
end;

procedure TATAdapterIME.ImeRequest(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.ImeNotify(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.ImeStartComposition(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.ImeComposition(Sender: TObject; var Msg: TMessage);
begin
  //
end;

procedure TATAdapterIME.ImeEndComposition(Sender: TObject; var Msg: TMessage);
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
  FImplementsDataReady:= false;
end;

procedure TATAdapterHilite.OnEditorChange(Sender: TObject);
begin
  //
end;

procedure TATAdapterHilite.OnEditorChangeEx(Sender: TObject; AChange: TATLineChangeKind; ALine,
  AItemCount: integer);
begin
  //
end;

procedure TATAdapterHilite.OnEditorIdle(Sender: TObject);
begin
  //
end;

procedure TATAdapterHilite.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor;
  AMainText: boolean);
begin
  //
end;

procedure TATAdapterHilite.OnEditorCalcPosColor(Sender: TObject;
  AX, AY: integer; var AColor: TColor);
begin
  //
end;

procedure TATAdapterHilite.OnEditorCalcPosForeground(Sender: TObject;
  AX, AY: integer; var AColor: TColor; var AFontStyles: TFontStyles);
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

function TATAdapterHilite.IsParsedAtLeastPartially: boolean;
begin
  Result:= true;
end;

function TATAdapterHilite.GetLexerName: string;
begin
  Result:= '-';
end;

function TATAdapterHilite.IsDataReady: boolean;
begin
  Result:= true;
end;

function TATAdapterHilite.IsDataReadyPartially: boolean;
begin
  Result:= true;
end;

function TATAdapterHilite.DynamicHiliteActiveNow(ALinesCount: integer): boolean;
begin
  Result:=
    DynamicHiliteEnabled and
    DynamicHiliteSupportedInCurrentSyntax and
    (ALinesCount<=DynamicHiliteMaxLines);
end;


end.

