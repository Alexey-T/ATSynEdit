unit ATSynEdit_Adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit_CanvasProc;

type
  { TATAdapterHilite }

  TATAdapterHilite = class(TComponent)
  public
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
  end;

implementation

{ TATAdapterHilite }

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

end.

