unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ComCtrls, Menus,
  ATStrings,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  formkey,
  formopt,
  formcombo, 
  proc_streamcomponent;

type
  { TfmMain }
  TfmMain = class(TForm)
    bFont: TButton;
    bOpt: TButton;
    chkGutter: TCheckBox;
    chkMicromap: TCheckBox;
    chkMinimap: TCheckBox;
    chkRuler: TCheckBox;
    chkUnprintEnd: TCheckBox;
    chkUnprintEndDet: TCheckBox;
    chkUnprintSp: TCheckBox;
    chkUnprintVis: TCheckBox;
    chkWrapIndent: TCheckBox;
    chkWrapMargin: TRadioButton;
    chkWrapOff: TRadioButton;
    chkWrapOn: TRadioButton;
    edFontsize: TSpinEdit;
    edMarRt: TSpinEdit;
    edSpaceX: TSpinEdit;
    edSpaceY: TSpinEdit;
    edTabsize: TSpinEdit;
    FontDialog1: TFontDialog;
    gUnpri: TGroupBox;
    gWrap: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuSave: TMenuItem;
    mnuLoad: TMenuItem;
    MenuItem2: TMenuItem;
    btnHlp: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuBms: TMenuItem;
    mnuOneLine: TMenuItem;
    mnuPane: TMenuItem;
    mnuHilit: TMenuItem;
    mnuTCaret1: TMenuItem;
    mnuOpt: TMenuItem;
    mnuTBms: TMenuItem;
    mnuTMargin: TMenuItem;
    mnuFile: TMenuItem;
    mnuHlp: TMenuItem;
    MenuItem5: TMenuItem;
    mnuTst: TMenuItem;
    mnuTCaretK: TMenuItem;
    mnuEndW: TMenuItem;
    mnuEndUn: TMenuItem;
    mnuEndMc: TMenuItem;
    mnuKey: TMenuItem;
    mnuOpn: TMenuItem;
    mnuSav: TMenuItem;
    mnuGoto: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelMain: TPanel;
    PanelRt: TPanel;
    PopupBookmk: TPopupMenu;
    PopupFold: TPopupMenu;
    PopupMinimap: TPopupMenu;
    PopupMicromap: TPopupMenu;
    PopupRuler: TPopupMenu;
    PopupNums: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Status: TStatusBar;
    procedure bGotoClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bFontClick(Sender: TObject);
    procedure bAddCrtClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bKeymapClick(Sender: TObject);
    procedure bOptClick(Sender: TObject);
    procedure btnHlpClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkGutterChange(Sender: TObject);
    procedure chkMicromapChange(Sender: TObject);
    procedure chkMinimapChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuBmsClick(Sender: TObject);
    procedure mnuOneLineClick(Sender: TObject);
    procedure mnuPaneClick(Sender: TObject);
    procedure chkRulerChange(Sender: TObject);
    procedure chkUnprintVisChange(Sender: TObject);
    procedure chkUnprintEndChange(Sender: TObject);
    procedure chkUnprintEndDetChange(Sender: TObject);
    procedure chkUnprintSpChange(Sender: TObject);
    procedure chkWrapMarginChange(Sender: TObject);
    procedure chkWrapOffChange(Sender: TObject);
    procedure chkWrapOnChange(Sender: TObject);
    procedure chkWrapIndentChange(Sender: TObject);
    procedure edFontsizeChange(Sender: TObject);
    procedure edMarRtChange(Sender: TObject);
    procedure edSpaceXChange(Sender: TObject);
    procedure edSpaceYChange(Sender: TObject);
    procedure edTabsizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuEndMcClick(Sender: TObject);
    procedure mnuEndUnClick(Sender: TObject);
    procedure mnuEndWClick(Sender: TObject);
    procedure mnuHilitClick(Sender: TObject);
    procedure mnuLockClick(Sender: TObject);
    procedure mnuTBmsClick(Sender: TObject);
    procedure mnuTCaret1Click(Sender: TObject);
    procedure mnuTMarginClick(Sender: TObject);
    procedure mnuUnlockClick(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
    wait: boolean;
    FDir: string;
    FIniName: string;
    procedure DoOpen(const fn: string);
    procedure EditChanged(Sender: TObject);
    procedure EditCaretMoved(Sender: TObject);
    procedure EditDrawLine(Sender: TObject; C: TCanvas; AX, AY: integer;
      const AStr: atString; ACharSize: TPoint; const AExtent: TATIntArray);
    procedure EditScroll(Sender: TObject);
    procedure EditCommand(Snd: TObject; ACmd{%H-}: integer; var AHandled: boolean);
    procedure EditClickGutter(Snd: TObject; ABand, ALine: integer);
    procedure EditClickMicromap(Snd: TObject; AX, AY: integer);
    procedure EditDrawBm(Snd: TObject; C: TCanvas; ALineNum{%H-}: integer; const ARect: TRect);
    procedure EditDrawMicromap(Snd: TObject; C: TCanvas; const ARect: TRect);
    procedure EditDrawTest(Snd: TObject; C: TCanvas; const ARect: TRect);
    procedure UpdateStatus;
    procedure UpdateChecks;
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  Math,
  Types,
  atsynedit_commands{%H-};

{$R *.lfm}

const
  cColorBmLine = clMoneyGreen;
  cColorBmIco = clMedGray;

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FDir:= ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName)))+'test_files';
  wait:= true;

  ed:= TATSynEdit.Create(Self);
  ed.Parent:= PanelMain;
  ed.Align:= alClient;
  {$ifdef windows}
  ed.Font.Name:= 'Consolas';
  {$else}
  ed.Font.Name:= 'Courier New';
  {$endif}

  ed.PopupGutterBm:= PopupBookmk;
  ed.PopupGutterNum:= PopupNums;
  ed.PopupGutterFold:= PopupFold;
  ed.PopupMinimap:= PopupMinimap;
  ed.PopupMicromap:= PopupMicromap;
  ed.PopupRuler:= PopupRuler;

  ed.OnChanged:= @EditChanged;
  ed.OnCaretMoved:= @EditCaretMoved;
  ed.OnScrolled:= @EditCaretMoved;
  ed.OnStateChanged:= @EditCaretMoved;
  ed.OnCommand:= @EditCommand;
  ed.OnClickGutter:= @EditClickGutter;
  ed.OnClickMicromap:= @EditClickMicromap;
  ed.OnDrawBookmarkIcon:= @EditDrawBm;
  ed.OnDrawLine:= @EditDrawLine;
  ed.OnDrawMicromap:= @EditDrawMicromap;
  //ed.OnDrawRuler:= EditDrawTest;//test

  ed.SetFocus;
end;

procedure TfmMain.FormShow(Sender: TObject);
var
  fn: string;
begin
  FIniName:= ExtractFilePath(Application.ExeName)+'saved.ini';

  if wait then UpdateChecks;
  wait:= false;
  ActiveControl:= ed;

  fn:= FDir+'\fn.txt';
  if FileExists(fn) then
    DoOpen(fn);
end;

procedure TfmMain.mnuEndMcClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndMac;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndUnClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndUnix;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndWClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndWin;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuHilitClick(Sender: TObject);
begin
  with mnuHilit do Checked:= not Checked;
  ed.Update;
end;

procedure TfmMain.mnuLockClick(Sender: TObject);
begin
  ed.BeginUpdate;
end;

procedure TfmMain.mnuTBmsClick(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to ed.Strings.Count-1 do
  begin
    if ed.Strings.LinesBm[i]=0 then
    begin
      ed.Strings.LinesBm[i]:= 1;
      ed.Strings.LinesBmColor[i]:= cColorBmLine;
    end
    else
      ed.Strings.LinesBm[i]:= 0;
  end;
  ed.Update;
end;

procedure TfmMain.mnuTCaret1Click(Sender: TObject);
var
  i: integer;
begin
  for i:= 1 to 100 do
    ed.Carets.Add(0, i);
  ed.Carets.Sort;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuTMarginClick(Sender: TObject);
var
  S: string;
begin
  S:= ed.OptMarginString;
  if InputQuery('Margins', 'space separated ints', S) then
  begin
    ed.OptMarginString:= S;
    ed.Update;
  end;
end;

procedure TfmMain.mnuUnlockClick(Sender: TObject);
begin
  ed.EndUpdate;
end;

procedure TfmMain.EditCaretMoved(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfmMain.UpdateStatus;
const
  cEnd: array[TATLineEnds] of string = ('?', 'Win', 'Unix', 'Mac');
  cOvr: array[boolean] of string = ('-', 'Ovr');
  cRo: array[boolean] of string = ('-', 'RO');
  cMod: array[boolean] of string = ('-', 'Mod');
  cSel: array[boolean] of string = ('-', 'Column');
var
  sPos: string;
  i: integer;
begin
  sPos:= '';
  for i:= 0 to Min(4, ed.Carets.Count-1) do
    with ed.Carets[i] do
      sPos:= sPos+Format(' %d:%d', [PosY+1, PosX+1]);

  Status.SimpleText:= Format('Line:Col%s | Carets: %d | Top: %d | %s | %s %s %s %s | Undo: %d, Redo: %d', [
    sPos,
    ed.Carets.Count,
    ed.ScrollTop+1,
    cEnd[ed.Strings.Endings],
    cOvr[ed.ModeOverwrite],
    cRo[ed.ModeReadOnly],
    cSel[not ed.IsSelRectEmpty],
    cMod[ed.Modified],
    ed.UndoCount,
    ed.RedoCount
    ]);
end;

procedure TfmMain.UpdateChecks;
begin
  chkGutter.Checked:= ed.OptGutterVisible;
  chkRuler.Checked:= ed.OptRulerVisible;
  chkMinimap.Checked:= ed.OptMinimapVisible;
  chkMicromap.Checked:= ed.OptMicromapVisible;
  edFontsize.Value:= ed.Font.Size;
  edTabsize.Value:= ed.OptTabSize;
  edSpaceX.Value:= ed.OptCharSpacingX;
  edSpaceY.Value:= ed.OptCharSpacingY;
  edMarRt.Value:= ed.OptMarginRight;
  case ed.OptWrapMode of
    cWrapOff: chkWrapOff.Checked:= true;
    cWrapOn: chkWrapOn.Checked:= true;
    cWrapAtMargin: chkWrapMargin.Checked:= true;
  end;
  chkWrapIndent.Checked:= ed.OptWrapIndented;
  chkUnprintVis.Checked:= ed.OptUnprintedVisible;
  chkUnprintSp.Checked:= ed.OptUnprintedSpaces;
  chkUnprintEnd.Checked:= ed.OptUnprintedEnds;
  chkUnprintEndDet.Checked:= ed.OptUnprintedEndsDetails;
end;

procedure TfmMain.EditScroll(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfmMain.EditCommand(Snd: TObject; ACmd: integer; var AHandled: boolean);
begin
  AHandled:= false;
  {
  if ACmd=cCommand_KeyTab then
  begin
    AHandled:= true;
    Beep;
  end;
  }
end;

procedure TfmMain.EditClickGutter(Snd: TObject; ABand, ALine: integer);
begin
  if ABand=ed.GutterBandBm then
  begin
    if ed.Strings.LinesBm[ALine]<>0 then
      ed.Strings.LinesBm[ALine]:= 0
    else
    begin
      ed.Strings.LinesBm[ALine]:= 1;
      ed.Strings.LinesBmColor[ALine]:= cColorBmLine;
    end;
    ed.Update;
  end;

  {//in control
  if ABand=ed.GutterBandNum then
  begin
    ed.DoSelect_Line(Point(0, ALine), true);
  end;
  }
end;

procedure TfmMain.EditClickMicromap(Snd: TObject; AX, AY: integer);
begin
  Showmessage(Format('Micromap click: %d:%d', [AX, AY]));
end;

procedure TfmMain.EditDrawBm(Snd: TObject; C: TCanvas; ALineNum: integer;
  const ARect: TRect);
var
  r: trect;
  dx: integer;
begin
  r:= arect;
  if IsRectEmpty(r) then exit;
  c.brush.color:= cColorBmIco;
  c.pen.color:= cColorBmIco;
  inc(r.top, 1);
  inc(r.left, 4);
  dx:= (r.bottom-r.top) div 2-1;
  c.Polygon([Point(r.left, r.top), Point(r.left+dx, r.top+dx), Point(r.left, r.top+2*dx)]);
end;

procedure TfmMain.EditDrawMicromap(Snd: TObject; C: TCanvas; const ARect: TRect);
begin
  C.Pen.Color:= $c0c0c0;
  C.Brush.Color:= $eeeeee;
  C.Rectangle(ARect);
  C.TextOut(ARect.Left+2, ARect.Top+2, 'tst');
end;

procedure TfmMain.EditDrawTest(Snd: TObject; C: TCanvas; const ARect: TRect);
begin
  //Exit;
  C.Pen.Color:= clred;
  C.Brush.Style:= bsClear;
  C.Rectangle(ARect);
end;

procedure TfmMain.bOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    InitialDir:= FDir;
    if not Execute then Exit;
    DoOpen(FileName);
  end;
end;

procedure TfmMain.DoOpen(const fn: string);
begin
  ed.BeginUpdate;
  Application.ProcessMessages;
  ed.LoadFromFile(fn);
  ed.EndUpdate;

  Caption:= 'Demo - '+ExtractFileName(fn);
end;

procedure TfmMain.EditChanged(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfmMain.bGotoClick(Sender: TObject);
var
  s: string;
  n: integer;
begin
  s:= Inttostr(ed.ScrollTop+1);
  if not InputQuery('Go to', 'Line:', s) then Exit;
  if s='' then Exit;
  n:= StrToIntDef(s, 0)-1;
  if (n<0) or (n>=ed.Strings.Count) then
  begin
    Showmessage('Incorr index: '+s);
    Exit
  end;
  ed.DoGotoPosEx(Point(0, n));
end;

procedure TfmMain.bFontClick(Sender: TObject);
begin
  with FontDialog1 do
  begin
    Font:= ed.Font;
    if Execute then
    begin
      ed.Font.Assign(Font);
      ed.Update;
    end;
  end;
  ed.SetFocus;
end;

procedure TfmMain.bAddCrtClick(Sender: TObject);
var
  i, j: integer;
begin
  for j:= 1 to 100 do
    for i:= 1 to 20 do
      ed.Carets.Add(i*2, j);
  ed.Carets.Sort;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.bSaveClick(Sender: TObject);
begin
  with SaveDialog1 do
  begin
    InitialDir:= FDir;
    FileName:= '';
    if Execute then
    begin
      ed.SaveToFile(FileName);
      ed.Update;
    end;
  end;
end;

procedure TfmMain.bKeymapClick(Sender: TObject);
var
  Cmd: integer;
begin
  Cmd:= DoCommandDialog(ed);
  if Cmd>0 then
  begin
    ed.DoCommandExec(Cmd);
    ed.Update;
  end;
end;

procedure TfmMain.bOptClick(Sender: TObject);
begin
  DoConfigEditor(ed);

  wait:= true;
  chkUnprintVis.Checked:= ed.OptUnprintedVisible;
  chkUnprintSp.Checked:= ed.OptUnprintedSpaces;
  chkUnprintEnd.Checked:= ed.OptUnprintedEnds;
  chkUnprintEndDet.Checked:= ed.OptUnprintedEndsDetails;
  wait:= false;

  ed.SetFocus;
end;

procedure TfmMain.btnHlpClick(Sender: TObject);
const
  txt =
    'Ctrl+click - add/del caret'#13+
    'Ctrl+drag - add caret with select'#13+
    'Ctrl+Shift+click - add caret column'#13+
    #13+
    'Alt+drag - column-select (looks weird with wrap, ignores tab-width)'#13+
    'drag on line numbers - line-select'#13;
begin
  Showmessage(txt);
end;

procedure TfmMain.btnLoadClick(Sender: TObject);
begin
  wait:= true;

  LoadComponentFromFile(ed, FIniName, nil);
  ed.Update(true);

  UpdateChecks;
  wait:= false;
end;


procedure TfmMain.btnSaveClick(Sender: TObject);
begin
  SaveComponentToFile(ed, FIniName);
end;

procedure TfmMain.chkGutterChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptGutterVisible:= chkGutter.Checked;
  ed.Update;
end;

procedure TfmMain.chkMicromapChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptMicromapVisible:= chkMicromap.Checked;
  ed.Update;
end;

procedure TfmMain.chkMinimapChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptMinimapVisible:= chkMinimap.Checked;
  ed.Update;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  //
end;

procedure TfmMain.mnuBmsClick(Sender: TObject);
begin
  mnuTBmsClick(Self);
end;

procedure TfmMain.mnuOneLineClick(Sender: TObject);
begin
  with TfmCombo.Create(Self) do
  try
    ShowModal
  finally
    Free
  end;
end;

procedure TfmMain.mnuPaneClick(Sender: TObject);
begin
  with mnuPane do
  begin
    Checked:= not Checked;
    PanelRt.Visible:= Checked;
  end;
end;

procedure TfmMain.chkRulerChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptRulerVisible:= chkRuler.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintVisChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedVisible:= chkUnprintVis.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintEndChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedEnds:= chkUnprintEnd.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintEndDetChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedEndsDetails:= chkUnprintEndDet.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintSpChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedSpaces:= chkUnprintSp.Checked;
  ed.Update;
end;

procedure TfmMain.chkWrapMarginChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapMode:= cWrapAtMargin;
end;

procedure TfmMain.chkWrapOffChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapMode:= cWrapOff;
end;

procedure TfmMain.chkWrapOnChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapMode:= cWrapOn;
end;

procedure TfmMain.chkWrapIndentChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapIndented:= chkWrapIndent.Checked;
  ed.Update;
end;

procedure TfmMain.edFontsizeChange(Sender: TObject);
begin
  if wait then Exit;
  ed.Font.Size:= edFontsize.Value;
  ed.Update(true);
end;

procedure TfmMain.edMarRtChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptMarginRight:= edMarRt.Value;
  ed.Update;
end;

procedure TfmMain.edSpaceXChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptCharSpacingX:= edSpaceX.Value;
  ed.Update;
end;

procedure TfmMain.edSpaceYChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptCharSpacingY:= edSpaceY.Value;
  ed.Update;
end;

procedure TfmMain.edTabsizeChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptTabSize:= edTabsize.Value;
  ed.Update;
end;

procedure TfmMain.EditDrawLine(Sender: TObject; C: TCanvas;
  AX, AY: integer; const AStr: atString; ACharSize: TPoint; const AExtent: TATIntArray);
var
  X1, X2, Y, i: integer;
begin
  if AStr='' then Exit;
  if not mnuHilit.Checked then Exit;

  C.Pen.Color:= clBlue;
  C.Pen.Width:= 2;
  C.Pen.EndCap:= pecSquare;

  for i:= 1 to Length(AStr) do
    if AStr[i]='w' then
    begin
      X1:= AX;
      if i>1 then
        Inc(X1, AExtent[i-2]);
      X2:= AX+AExtent[i-1];
      Y:= AY+ACharSize.Y-1;

      C.Line(X1, Y, X2, Y);
    end;

  C.Pen.Width:= 1;
end;

end.
