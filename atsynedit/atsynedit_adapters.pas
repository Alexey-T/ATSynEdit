unit ATSynEdit_Adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit_CanvasProc;

type
  TATAdapterHilite = class(TObject)
  public
    procedure OnEditorChange(Sender: TObject); virtual; abstract;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); virtual; abstract;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); virtual; abstract;
    //procedure OnEditorScroll(Sender: TObject); virtual; abstract;
  end;

implementation

end.

