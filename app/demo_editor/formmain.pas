unit formmain;

{$mode objfpc}{$H+}

//{$define test_text}

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
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    mnuSyntax: TMenuItem;
    mnuEnc: TMenuItem;
    mnuOptSave: TMenuItem;
    mnuOptLoad: TMenuItem;
    MenuItem2: TMenuItem;
    mnuHelpMous: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    mnuOpts: TMenuItem;
    mnuBms: TMenuItem;
    mnuOneLine: TMenuItem;
    mnuPane: TMenuItem;
    mnuUnderline: TMenuItem;
    mnuTCaret1: TMenuItem;
    mnuOptDlg: TMenuItem;
    mnuTBms: TMenuItem;
    mnuTMargin: TMenuItem;
    mnuFile: TMenuItem;
    mnuHlp: TMenuItem;
    mnuFileEnd: TMenuItem;
    mnuTst: TMenuItem;
    mnuTCaretK: TMenuItem;
    mnuEndWin: TMenuItem;
    mnuEndUnix: TMenuItem;
    mnuEndMac: TMenuItem;
    mnuHelpKey: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSav: TMenuItem;
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
    procedure mnuFileOpenClick(Sender: TObject);
    procedure bFontClick(Sender: TObject);
    procedure bAddCrtClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure bKeymapClick(Sender: TObject);
    procedure bOptClick(Sender: TObject);
    procedure mnuSyntaxClick(Sender: TObject);
    procedure UpdateEnc;
    procedure mnuHelpMousClick(Sender: TObject);
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
    procedure mnuEndMacClick(Sender: TObject);
    procedure mnuEndUnixClick(Sender: TObject);
    procedure mnuEndWinClick(Sender: TObject);
    procedure mnuUnderlineClick(Sender: TObject);
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
    FFileName: string;
    procedure DoAddEnc(Sub, SName: string);
    procedure DoOpen(const fn: string; ADetectEnc: boolean);
    procedure DoSetEnc(const Str: string);
    procedure EditChanged(Sender: TObject);
    procedure EditCaretMoved(Sender: TObject);
    procedure EditDrawLine(Sender: TObject; C: TCanvas; AX, AY: integer;
      const AStr: atString; ACharSize: TPoint; const AExtent: TATIntArray);
    procedure EditCalcLine(Sender: TObject; var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
    procedure EditScroll(Sender: TObject);
    procedure EditCommand(Snd: TObject; ACmd{%H-}: integer; var AHandled: boolean);
    procedure EditClickGutter(Snd: TObject; ABand, ALine: integer);
    procedure EditClickMicromap(Snd: TObject; AX, AY: integer);
    procedure EditDrawBm(Snd: TObject; C: TCanvas; ALineNum{%H-}: integer; const ARect: TRect);
    procedure EditDrawMicromap(Snd: TObject; C: TCanvas; const ARect: TRect);
    procedure EditDrawTest(Snd: TObject; C: TCanvas; const ARect: TRect);
    procedure MenuEncClick(Sender: TObject);
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
  LCLType,
  LCLProc,
  atsynedit_commands{%H-};

{$R *.lfm}

const
  sEncAnsi = 'ANSI';
  sEncUtf8 = 'UTF-8';
  sEncUtf8NoBom = 'UTF-8 no bom';
  sEncUtf16LE = 'UTF-16 LE';
  sEncUtf16BE = 'UTF-16 BE';

const
  cColorBmLine = clMoneyGreen;
  cColorBmIco = clMedGray;

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  UpdateEnc;

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

  ed.OnChange:= @EditChanged;
  ed.OnChangeCaretPos:= @EditCaretMoved;
  ed.OnChangeState:= @EditCaretMoved;
  ed.OnScroll:= @EditCaretMoved;
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
    DoOpen(fn, true);
end;

procedure TfmMain.mnuEndMacClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndMac;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndUnixClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndUnix;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndWinClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndWin;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuUnderlineClick(Sender: TObject);
begin
  with mnuUnderline do Checked:= not Checked;
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
    ed.LineTop+1,
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

procedure TfmMain.mnuFileOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    InitialDir:= FDir;
    if not Execute then Exit;
    DoOpen(FileName, true);
  end;
end;

procedure TfmMain.DoOpen(const fn: string; ADetectEnc: boolean);
begin
  Application.ProcessMessages;
  FFileName:= fn;

  ed.BeginUpdate;
  try
    if ADetectEnc then
      ed.Strings.EncodingCodepage:= '';
    ed.Strings.EncodingDetect:= ADetectEnc;
    ed.LoadFromFile(fn);
    ed.Strings.EncodingDetect:= true;
  finally
    ed.EndUpdate;
  end;

  Caption:= 'Demo - '+ExtractFileName(fn);
end;

procedure TfmMain.EditChanged(Sender: TObject);
begin
  UpdateStatus;

  {$ifdef test_text}
  with Memo1 do
  begin
    Lines.Clear;
    Lines.Text:= utf8encode(ed.Strings.TextString);
  end;
  {$else}
  Memo1.Hide;
  {$endif}
end;

procedure TfmMain.bGotoClick(Sender: TObject);
var
  s: string;
  n: integer;
begin
  s:= Inttostr(ed.LineTop+1);
  if not InputQuery('Go to', 'Line:', s) then Exit;
  if s='' then Exit;
  n:= StrToIntDef(s, 0)-1;
  if (n>=0) and (n<ed.Strings.Count) then
    ed.DoGotoPosEx(Point(0, n))
  else
    Showmessage('Incorrect index: '+s);
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

procedure TfmMain.mnuFileSaveClick(Sender: TObject);
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

procedure TfmMain.mnuSyntaxClick(Sender: TObject);
begin
  mnuSyntax.Checked:= not mnuSyntax.Checked;
  if mnuSyntax.Checked then
    ed.OnCalcHilite:= @EditCalcLine
  else
    ed.OnCalcHilite:= nil;
  ed.Update;
end;

procedure TfmMain.DoSetEnc(const Str: string);
begin
  if Str=sEncAnsi then
  begin
    Ed.Strings.Encoding:= cEncAnsi;
    Ed.Strings.EncodingCodepage:= '';
  end
  else
  if Str=sEncUtf8 then
  begin
    Ed.Strings.Encoding:= cEncUTF8;
    Ed.Strings.SaveSignUtf8:= true;
    Ed.Strings.EncodingCodepage:= '';
  end
  else
  if Str=sEncUtf8NoBom then
  begin
    Ed.Strings.Encoding:= cEncUTF8;
    Ed.Strings.SaveSignUtf8:= false;
    Ed.Strings.EncodingCodepage:= '';
  end
  else
  if Str=sEncUtf16LE then
  begin
    Ed.Strings.Encoding:= cEncWideLE;
    Ed.Strings.EncodingCodepage:= '';
  end
  else
  if Str=sEncUtf16BE then
  begin
    Ed.Strings.Encoding:= cEncWideBE;
    Ed.Strings.EncodingCodepage:= '';
  end
  else
  begin
    Ed.Strings.Encoding:= cEncAnsi;
    Ed.Strings.EncodingCodepage:= Str;
  end;

  if FFileName<>'' then
    if Application.Messagebox('Encoding changed in mem. Also reload file in this encoding?',
      'Editor', MB_OKCANCEL or MB_ICONQUESTION) = id_ok then
      DoOpen(FFileName, false);
end;

procedure TfmMain.DoAddEnc(Sub, SName: string);
var
  mi, miSub: TMenuItem;
  n: integer;
  subEnc: TMenuItem;
begin
  subEnc:= mnuEnc;
  miSub:= nil;
  if Sub<>'' then
  begin
    n:= subEnc.IndexOfCaption(Sub);
    if n<0 then
    begin
      mi:= TMenuItem.Create(Self);
      mi.Caption:= Sub;
      subEnc.Add(mi);
      n:= subEnc.IndexOfCaption(Sub);
    end;
    miSub:= subEnc.Items[n]
  end;
  if miSub=nil then miSub:= subEnc;
  mi:= TMenuItem.Create(Self);
  mi.Caption:= SName;
  mi.OnClick:= @MenuEncClick;
  miSub.Add(mi);
end;

procedure TfmMain.MenuEncClick(Sender: TObject);
begin
  DoSetEnc((Sender as TMenuItem).Caption);
end;


procedure TfmMain.UpdateEnc;
begin
  mnuEnc.Clear;

  DoAddEnc('', sEncAnsi);
  DoAddEnc('', sEncUtf8);
  DoAddEnc('', sEncUtf8NoBom);
  DoAddEnc('', sEncUtf16LE);
  DoAddEnc('', sEncUtf16BE);
  DoAddEnc('', '-');

  DoAddEnc('Europe', 'CP1250');
  DoAddEnc('Europe', 'CP1251');
  DoAddEnc('Europe', 'CP1252');
  DoAddEnc('Europe', 'CP1253');
  DoAddEnc('Europe', 'CP1257');
  DoAddEnc('Europe', '-');
  DoAddEnc('Europe', 'CP437');
  DoAddEnc('Europe', 'CP850');
  DoAddEnc('Europe', 'CP852');
  DoAddEnc('Europe', 'CP866');
  DoAddEnc('Europe', '-');
  DoAddEnc('Europe', 'ISO-8859-1');
  DoAddEnc('Europe', 'ISO-8859-2');
  DoAddEnc('Europe', 'Macintosh');

  DoAddEnc('Other', 'CP1254');
  DoAddEnc('Other', 'CP1255');
  DoAddEnc('Other', 'CP1256');

  DoAddEnc('Asian', 'CP874');
  DoAddEnc('Asian', 'CP932');
  DoAddEnc('Asian', 'CP936');
  DoAddEnc('Asian', 'CP949');
  DoAddEnc('Asian', 'CP950');
  DoAddEnc('Asian', 'CP1258');
end;

procedure TfmMain.mnuHelpMousClick(Sender: TObject);
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
  if not mnuUnderline.Checked then Exit;

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

procedure TfmMain.EditCalcLine(Sender: TObject; var AParts: TATLineParts;
  ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
var
  nlen, npart, noffset: integer;
  kind, kindnew: integer;
  //
  procedure Add;
  begin
    if npart>High(AParts) then exit;
    with AParts[npart] do
    begin
      ColorBG:= clNone;
      case kind of
        1: begin
             ColorFont:= clblue;
           end;
        2: begin
             ColorFont:= clgreen;
             ColorBorder:= clgreen;
             BorderLeft:= cBorderDotted;
             BorderDown:= BorderLeft;
             BorderRight:= BorderLeft;
             BorderUp:= BorderLeft;
             FontItalic:= true;
             FontBold:= true;
           end;
        3: begin
             ColorFont:= clred;
             Colorbg:= clyellow;
             ColorBorder:= clred;
             BorderDown:= cBorderWave;
             //BorderLeft:= cBorderDotted;
             //BorderRight:= cBorderRounded;
             //BorderUp:= cBorderRounded;
          end;
        else
          begin
            ColorFont:= clgray;
          end;
      end;
      Offset:= noffset;
      Len:= nlen;
    end;
    inc(npart);
  end;
  //
var
  Str: atString;
  i: integer;
begin
  Str:= Copy(ed.Strings.Lines[ALineIndex], ACharIndex, ALineLen);

  npart:= 0;
  noffset:= 0;
  nlen:= 1;
  kind:= -1;

  for i:= 1 to Length(Str) do
  begin
    case Str[i] of
      'w': kindnew:= 1;
      'e': kindnew:= 2;
      '0'..'9': kindnew:= 3;
      else kindnew:= 0;
    end;

    if kindnew=kind then
    begin
      inc(nlen);
      Continue;
    end;
    if kind>=0 then Add;
    kind:= kindnew;
    nlen:= 1;
    noffset:= i-1;
  end;

  Add;

  //test
  //AParts[0].Colorbg:= clgreen;
  //AParts[1].Colorbg:= clyellow;
end;

end.
