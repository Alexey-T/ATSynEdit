unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private

  public
    mx,my: integer;
  end;

var
  Form1: TForm1;

implementation

uses
  LCLIntf, LCLProc, LCLType,
  BGRABitmap, BGRABitmapTypes, BGRAText, BGRAPath;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mx := x;
  my := y;
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
const
  SampleText = 'TextOut LTR TextOut LTR TextOut LTR TextOut LTR TextOut LTR';
  zoom = 1;
  cnt = 2000;
var
  tick_bgra, tick_cnv: QWord;
  image: TBGRABitmap;
  i: integer;
begin
  image := TBGRABitmap.Create(ClientWidth div zoom, ClientHeight div zoom, BGRAWhite);
  image.FontName := Font.Name;
  image.FontHeight := round(Font.Height*96/PixelsPerInch);
  image.FontQuality := fqSystemClearType;

  tick_bgra:= GetTickCount64;
  for i:= 0 to cnt do
    image.TextOut(240, 190, SampleText, BGRABlack);
  tick_bgra:= GetTickCount64-tick_bgra;

  image.Draw(Canvas, 0,0, true);
  image.Free;

  //-----------------

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Height := round(Font.Height*96/PixelsPerInch);
  Canvas.Font.Color := clBlack;

  tick_cnv:= GetTickCount64;
  for i:= 0 to cnt do
    ExtTextOut(Canvas.Handle, 250, 140, 0, nil, PChar(SampleText), Length(SampleText), nil);
  tick_cnv:= GetTickCount64-tick_cnv;

  //-----------------

  Canvas.Font.Color:= clBlue;
  Canvas.TextOut(10, 10, 'time of TextOut: bgrabitmap/tcanvas = '+FloatToStr(tick_bgra/tick_cnv));
end;

end.

