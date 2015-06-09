unit ATSynEdit_Adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit_CanvasProc;

type
  TATSynEdit_AdapterOfHilite = class(TObject)
  public
    procedure OnEditorChange(Sender: TObject); virtual; abstract;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); virtual; abstract;
  end;

implementation

end.

