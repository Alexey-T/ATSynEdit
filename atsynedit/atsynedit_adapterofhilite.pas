unit ATSynEdit_AdapterOfHilite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_CanvasProc,
  ATSynEdit_WrapInfo;

type
  TATSynEdit_AdapterOfHilite = class(TComponent)
  public
    procedure OnEditorChange(Sender: TObject); virtual; abstract;
    procedure OnEditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
      const AWrapItem: TATSynWrapItem; ACharIndexFrom: integer); virtual; abstract;
  end;

implementation

end.

