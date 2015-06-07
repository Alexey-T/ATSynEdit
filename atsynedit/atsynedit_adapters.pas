unit ATSynEdit_Adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_CanvasProc,
  ATSynEdit_WrapInfo;

type
  TATSynEdit_AdapterOfHilite = class(TObject)
  public
    procedure OnEditorChange(Sender: TObject); virtual; abstract;
    procedure OnEditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
      const AWrapItem: TATSynWrapItem; ACharIndexFrom: integer); virtual; abstract;
  end;

implementation

end.

