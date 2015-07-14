unit ATSynEdit_Edits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Menus,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATStringProc;

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
    procedure DoCommand(ACmd: integer; const AText: atString = ''); override;
  end;


implementation

uses
  Types,
  ATSynEdit_Commands,
  ATSynEdit_Keymap_Init;

const
  cComboArrowSpace = 24;
  cComboArrowSize = 4;

{ TATEdit }

constructor TATEdit.Create(AOwner: TComponent);
begin
  inherited;

  WantTabs:= false;
  ModeOneLine:= true;
  BorderStyle:= bsSingle;

  Keymap:= KeymapCombo;

  OptTextOffsetTop:= 2;
  Height:= 26;
end;

{ TATComboEdit }

procedure TATComboEdit.MicromapClick(Sender: TObject; AX, AY: integer);
begin
  DoMenu;
end;

procedure TATComboEdit.MicromapDraw(Sender: TObject; C: TCanvas;
  const ARect: TRect);
begin
  C.Brush.Color:= Colors.ComboboxArrowBG;
  C.FillRect(ARect);

  CanvasPaintTriangleDown(C, Colors.ComboboxArrow,
    Point(
      (ARect.Left+ARect.Right) div 2 - cComboArrowSize,
      (ARect.Top+ARect.Bottom) div 2 - cComboArrowSize div 2),
    cComboArrowSize);
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
    DoCommand(cCommand_SelectNone);
    DoCaretSingle(Length(Text), 0);
    DoCommand(cCommand_ScrollToCaretRight);
  end;
end;

procedure TATComboEdit.DoCommand(ACmd: integer; const AText: atString);
begin
  inherited;
  if ACmd=cCommand_RecentsPopup then
  begin
    DoMenu;
  end;
end;

constructor TATComboEdit.Create(AOwner: TComponent);
begin
  inherited;

  FItems:= TStringList.Create;
  FMenu:= TPopupMenu.Create(Self);

  OptMicromapVisible:= true;
  OptMicromapWidth:= cComboArrowSpace;
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

