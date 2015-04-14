unit ATEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ATSynEdit, Menus;

type
  { TATEdit }

  TATEdit = class(TATSynEdit)
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  { TATComboEdit }

  TATComboEdit = class(TATEdit)
  private
    FItems: TStringList;
    FMenu: TPopupMenu;
    procedure MicromapClick(Sender: TObject; AX, AY: integer);
    procedure MicromapDraw(Sender: TObject; C: TCanvas; const ARect: TRect);
    procedure DoMenu;
    procedure MenuItemClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStringList read FItems;
  end;



implementation

uses
  ATSynEdit_Commands;

{ TATEdit }

constructor TATEdit.Create(AOwner: TComponent);
begin
  inherited;

  ModeOneLine:= true;
  BorderStyle:= bsSingle;

  OptOffsetTop:= 2;
  Height:= 26;
end;

{ TATComboEdit }

procedure TATComboEdit.MicromapClick(Sender: TObject; AX, AY: integer);
begin
  DoMenu;
end;

procedure TATComboEdit.MicromapDraw(Sender: TObject; C: TCanvas;
  const ARect: TRect);
const
  dx = 4;
var
  cl: TColor;
  size, dy: integer;
begin
  cl:= Colors.GutterFont;
  size:= OptMicromapWidth - 2*dx;
  dy:= (ClientHeight-size div 2) div 2;

  C.Brush.Color:= Colors.TextBG;
  C.FillRect(ARect);

  C.Brush.Color:= cl;
  C.Pen.Color:= cl;
  C.Polygon([
    Point(ARect.Left+dx, ARect.Top+dy),
    Point(ARect.Left+dx+size, ARect.Top+dy),
    Point(ARect.Left+dx+size div 2, ARect.Top+dy+size div 2)
    ]);
end;

procedure TATComboEdit.DoMenu;
var
  p: TPoint;
  i: integer;
  mi: TMenuItem;
begin
  p:= ClientToScreen(Point(Width-OptMicromapWidth, Height));
  with FMenu.Items do
  begin
    Clear;
    for i:= 0 to FItems.Count-1 do
    begin
      mi:= TMenuItem.Create(Self);
      mi.Caption:= FItems[i];
      mi.Tag:= i;
      mi.OnClick:= @MenuItemClick;
      Add(mi);
    end;
  end;
  FMenu.PopUp(p.x, p.y);
end;

procedure TATComboEdit.MenuItemClick(Sender: TObject);
var
  n: integer;
begin
  n:= (Sender as TMenuItem).Tag;
  if n>=0 then
  begin
    Text:= UTF8Decode(FItems[n]);
    DoCommandExec(cCommand_SelectNone);
    DoCaretSingle(Length(Text), 0);
    DoCommandExec(cCommand_ScrollToCaretRight);
  end;
end;

constructor TATComboEdit.Create(AOwner: TComponent);
begin
  inherited;

  FItems:= TStringList.Create;
  FMenu:= TPopupMenu.Create(Self);

  OptMicromapVisible:= true;
  OptMicromapWidth:= 16;
  OnClickMicromap:= @MicromapClick;
  OnDrawMicromap:= @MicromapDraw;
end;

destructor TATComboEdit.Destroy;
begin
  FreeAndNil(FMenu);
  FreeAndNil(FItems);
  inherited;
end;


end.

