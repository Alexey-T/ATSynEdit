unit formopt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Spin, ComCtrls, ATSynedit;

type
  { TfmOpt }

  TfmOpt = class(TForm)
    bColUp: TButton;
    bColDown: TButton;
    ButtonPanel1: TButtonPanel;
    chkPopupDown: TCheckBox;
    chkShowFoldLines: TCheckBox;
    chkShowFoldAlways: TCheckBox;
    chkCrPreferLeft: TCheckBox;
    chkGutterFold: TCheckBox;
    chkKeepCol: TCheckBox;
    chkCurLineMin: TCheckBox;
    chkHint: TCheckBox;
    chkPageKeepRel: TCheckBox;
    chkNavWrap2: TCheckBox;
    chkGutterBm: TCheckBox;
    chkGutterEmpty: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterStat: TCheckBox;
    chkNice: TCheckBox;
    chkRepSpec: TCheckBox;
    chkSaveEol: TCheckBox;
    chkSaveTrim: TCheckBox;
    chkShowNum1st: TCheckBox;
    chkShowNumCr: TCheckBox;
    chkMapBord: TCheckBox;
    chkMapAlw: TCheckBox;
    chkShowNumBg: TCheckBox;
    chkTabSp: TCheckBox;
    chkUndoSv: TCheckBox;
    chkUndoGr: TCheckBox;
    chkCutNoSel: TCheckBox;
    chkDotLn: TCheckBox;
    chkClickNm: TCheckBox;
    chkCrUnfocus: TCheckBox;
    chkEnd: TCheckBox;
    chkUninKeep: TCheckBox;
    chkAutoInd: TCheckBox;
    chkTabInd: TCheckBox;
    chkHome: TCheckBox;
    chkLeftRt: TCheckBox;
    chkNavWrap: TCheckBox;
    chkOvrSel: TCheckBox;
    chkRtMove: TCheckBox;
    chkDnD: TCheckBox;
    chkCrMul: TCheckBox;
    chkCrVirt: TCheckBox;
    chkClick2: TCheckBox;
    chkClick2W: TCheckBox;
    chkClick3: TCheckBox;
    chkColorSel: TCheckBox;
    chkCopyNoSel: TCheckBox;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    chkLastOnTop: TCheckBox;
    chkOvrPaste: TCheckBox;
    edAutoInd: TComboBox;
    edCrShape: TComboBox;
    edCrShape2: TComboBox;
    edCrTime: TSpinEdit;
    edChars: TEdit;
    edIndent: TSpinEdit;
    edPlusSize: TSpinEdit;
    edNumSkip: TEdit;
    edMapFont: TSpinEdit;
    edNum: TComboBox;
    edPage: TComboBox;
    edRulerFSize: TSpinEdit;
    edRulerSize: TSpinEdit;
    edSizeBm: TSpinEdit;
    edSizeEmpty: TSpinEdit;
    edSizeState: TSpinEdit;
    edSizeFold: TSpinEdit;
    edTabArrow: TSpinEdit;
    edTabArrowPnt: TSpinEdit;
    GroupBox2: TGroupBox;
    LabChars: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelArr: TLabel;
    LabelArr1: TLabel;
    ListCol: TListBox;
    PageControl1: TPageControl;
    edUndo: TSpinEdit;
    edNumSize: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    procedure bColDownClick(Sender: TObject);
    procedure bColUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure InitShape(ed: TCombobox);
  end;

var
  fmOpt: TfmOpt;

procedure DoConfigEditor(ed: TATSynEdit);


implementation

{$R *.lfm}

const
  nameBm = 'bookmk';
  nameNums = 'nums';
  nameState = 'states';
  nameFold = 'fold';

procedure DoConfigEditor(ed: TATSynEdit);
var
  i: integer;
begin
  with fmOpt do
  begin
    with ListCol do
    begin
      Items.Clear;
      for i:= 0 to 3 do
      begin
        if i=ed.GutterBandBm then Items.Add(nameBm);
        if i=ed.GutterBandNum then Items.Add(nameNums);
        if i=ed.GutterBandState then Items.Add(nameState);
        if i=ed.GutterBandFold then Items.Add(nameFold);
      end;
      ItemIndex:= 0;
    end;

    //general
    chkCurLine.Checked:= ed.OptShowCurLine;
    chkCurLineMin.Checked:= ed.OptShowCurLineMinimal;
    chkCurCol.Checked:= ed.OptShowCurColumn;
    chkLastOnTop.Checked:= ed.OptLastLineOnTop;
    chkColorSel.Checked:= ed.OptHiliteSelFull;
    chkPopupDown.Checked:= ed.OptPopupOnMouseDown;
    chkCopyNoSel.Checked:= ed.OptCopyLinesIfNoSel;
    chkCutNoSel.Checked:= ed.OptCutLinesIfNoSel;
    chkOvrPaste.Checked:= ed.OptUseOverOnPaste;
    chkRepSpec.Checked:= ed.OptUnprintedReplaceSpec;
    chkDotLn.Checked:= ed.OptShowIndentLines;
    edChars.Text:= ed.OptWordChars;
    chkSaveEol.Checked:= ed.OptSavingForceFinalEol;
    chkSaveTrim.Checked:= ed.OptSavingTrimSpaces;
    edTabArrow.Value:= ed.OptUnprintedArrowSize;
    edTabArrowPnt.Value:= ed.OptUnprintedArrowPointer;
    chkHint.Checked:= ed.OptShowScrollHint;

    //caret
    chkCrVirt.Checked:= ed.OptCaretVirtual;
    chkCrMul.Checked:= ed.OptCaretManyAllowed;
    chkCrUnfocus.Checked:= ed.OptCaretStopUnfocused;
    chkCrPreferLeft.Checked:= ed.OptCaretPreferLeftSide;
    edCrTime.Value:= ed.OptCaretTime;
    edCrShape.ItemIndex:= Ord(ed.OptCaretShape);
    edCrShape2.ItemIndex:= Ord(ed.OptCaretShapeOvr);

    //gutter
    edNum.ItemIndex:= Ord(ed.OptNumbersStyle);
    edNumSize.Value:= ed.OptNumbersFontSize;
    edNumSkip.Text:= ed.OptNumbersSkippedChar;
    edPlusSize.Value:= ed.OptGutterPlusSize;
    chkShowNum1st.Checked:= ed.OptNumbersShowFirst;
    chkShowNumCr.Checked:= ed.OptNumbersShowCarets;
    chkShowNumBg.Checked:= ed.OptShowGutterCaretBG;
    chkShowFoldAlways.Checked:= ed.OptGutterShowFoldAlways;
    chkShowFoldLines.Checked:= ed.OptGutterShowFoldLines;
    edRulerSize.Value:= ed.OptRulerSize;
    edRulerFSize.Value:= ed.OptRulerFontSize;

    chkGutterBm.Checked:= ed.Gutter[ed.GutterBandBm].Visible;
    chkGutterNum.Checked:= ed.Gutter[ed.GutterBandNum].Visible;
    chkGutterFold.Checked:= ed.Gutter[ed.GutterBandFold].Visible;
    chkGutterStat.Checked:= ed.Gutter[ed.GutterBandState].Visible;
    chkGutterEmpty.Checked:= ed.Gutter[ed.GutterBandEmpty].Visible;
    edSizeBm.Value:= ed.Gutter[ed.GutterBandBm].Size;
    edSizeFold.Value:= ed.Gutter[ed.GutterBandFold].Size;
    edSizeState.Value:= ed.Gutter[ed.GutterBandState].Size;
    edSizeEmpty.Value:= ed.Gutter[ed.GutterBandEmpty].Size;

    //minimap
    edMapFont.Value:= ed.OptMinimapFontSize;
    chkMapBord.Checked:= ed.OptMinimapShowSelBorder;
    chkMapAlw.Checked:= ed.OptMinimapShowSelAlways;

    //key
    chkTabSp.Checked:= ed.OptTabSpaces;
    chkOvrSel.Checked:= ed.OptOverwriteSel;
    chkNavWrap.Checked:= ed.OptKeyUpDownNavigateWrapped;
    chkNavWrap2.Checked:= ed.OptKeyHomeEndNavigateWrapped;
    chkKeepCol.Checked:= ed.OptKeyUpDownKeepColumn;
    chkLeftRt.Checked:= ed.OptKeyLeftRightSwapSel;
    chkHome.Checked:= ed.OptKeyHomeToNonSpace;
    chkEnd.Checked:= ed.OptKeyEndToNonSpace;
    chkTabInd.Checked:= ed.OptKeyTabIndents;
    chkAutoInd.Checked:= ed.OptAutoIndent;
    edAutoInd.ItemIndex:= Ord(ed.OptAutoIndentKind);
    edIndent.Value:= ed.OptIndentSize;
    chkUninKeep.Checked:= ed.OptIndentKeepsAlign;
    edPage.ItemIndex:= Ord(ed.OptKeyPageUpDownSize);
    chkPageKeepRel.Checked:= ed.OptKeyPageKeepsRelativePos;

    //mouse
    chkClick2.Checked:= ed.OptMouse2ClickSelectsLine;
    chkClick3.Checked:= ed.OptMouse3ClickSelectsLine;
    chkClick2W.Checked:= ed.OptMouse2ClickDragSelectsWords;
    chkClickNm.Checked:= ed.OptMouseGutterClickSelectsLine;
    chkDnD.Checked:= ed.OptMouseDragDrop;
    chkRtMove.Checked:= ed.OptMouseRightClickMovesCaret;
    chkNice.Checked:= ed.OptMouseNiceScroll;

    //undo
    edUndo.Value:= ed.OptUndoLimit;
    chkUndoGr.Checked:= ed.OptUndoGrouped;
    chkUndoSv.Checked:= ed.OptUndoAfterSave;

    if ShowModal=mrOk then
    begin
      ed.GutterBandBm:= ListCol.Items.IndexOf(nameBm);
      ed.GutterBandNum:= ListCol.Items.IndexOf(nameNums);
      ed.GutterBandState:= ListCol.Items.IndexOf(nameState);
      ed.GutterBandFold:= ListCol.Items.IndexOf(nameFold);

      //general
      ed.OptShowCurLine:= chkCurLine.Checked;
      ed.OptShowCurLineMinimal:= chkCurLineMin.Checked;
      ed.OptShowCurColumn:= chkCurCol.Checked;
      ed.OptWordChars:= edChars.Text;
      ed.OptUseOverOnPaste:= chkOvrPaste.Checked;
      ed.OptCopyLinesIfNoSel:= chkCopyNoSel.Checked;
      ed.OptCutLinesIfNoSel:= chkCutNoSel.Checked;
      ed.OptHiliteSelFull:= chkColorSel.Checked;
      ed.OptPopupOnMouseDown := chkPopupDown.Checked;
      ed.OptUnprintedReplaceSpec:= chkRepSpec.Checked;
      ed.OptLastLineOnTop:= chkLastOnTop.Checked;
      ed.OptShowIndentLines:= chkDotLn.Checked;
      ed.OptSavingForceFinalEol:= chkSaveEol.Checked;
      ed.OptSavingTrimSpaces:= chkSaveTrim.Checked;
      ed.OptUnprintedArrowSize:= edTabArrow.Value;
      ed.OptUnprintedArrowPointer := edTabArrowPnt.Value;
      ed.OptShowScrollHint:= chkHint.Checked;

      //caret
      ed.OptCaretVirtual:= chkCrVirt.Checked;
      ed.OptCaretTime:= edCrTime.Value;
      ed.OptCaretShape:= TATSynCaretShape(edCrShape.ItemIndex);
      ed.OptCaretShapeOvr:= TATSynCaretShape(edCrShape2.ItemIndex);
      ed.OptCaretManyAllowed:= chkCrMul.Checked;
      ed.OptCaretStopUnfocused:= chkCrUnfocus.Checked;
      ed.OptCaretPreferLeftSide:= chkCrPreferLeft.Checked;

      //gutter
      ed.OptNumbersFontSize:= edNumSize.Value;
      ed.OptNumbersStyle:= TATSynNumbersStyle(edNum.ItemIndex);
      ed.OptNumbersShowFirst:= chkShowNum1st.Checked;
      ed.OptNumbersShowCarets:= chkShowNumCr.Checked;
      ed.OptNumbersSkippedChar:= edNumSkip.Text;
      ed.OptGutterShowFoldAlways := chkShowFoldAlways.Checked;
      ed.OptGutterShowFoldLines := chkShowFoldLines.Checked;
      ed.OptGutterPlusSize:= edPlusSize.Value;
      ed.OptShowGutterCaretBG:= chkShowNumBg.Checked;
      ed.OptRulerSize:= edRulerSize.Value;
      ed.OptRulerFontSize:= edRulerFSize.Value;

      ed.Gutter[ed.GutterBandBm].Visible:= chkGutterBm.Checked;
      ed.Gutter[ed.GutterBandNum].Visible:= chkGutterNum.Checked;
      ed.Gutter[ed.GutterBandFold].Visible:= chkGutterFold.Checked;
      ed.Gutter[ed.GutterBandState].Visible:= chkGutterStat.Checked;
      ed.Gutter[ed.GutterBandEmpty].Visible:= chkGutterEmpty.Checked;
      ed.Gutter[ed.GutterBandBm].Size:= edSizeBm.Value;
      ed.Gutter[ed.GutterBandFold].Size:= edSizeFold.Value;
      ed.Gutter[ed.GutterBandState].Size:= edSizeState.Value;
      ed.Gutter[ed.GutterBandEmpty].Size:= edSizeEmpty.Value;

      //minimap
      ed.OptMinimapFontSize:= edMapFont.Value;
      ed.OptMinimapShowSelBorder:= chkMapBord.Checked;
      ed.OptMinimapShowSelAlways:= chkMapAlw.Checked;

      //key
      ed.OptTabSpaces:= chkTabSp.Checked;
      ed.OptOverwriteSel:= chkOvrSel.Checked;
      ed.OptKeyUpDownKeepColumn:= chkKeepCol.Checked;
      ed.OptKeyUpDownNavigateWrapped:= chkNavWrap.Checked;
      ed.OptKeyHomeEndNavigateWrapped:= chkNavWrap2.Checked;
      ed.OptKeyPageUpDownSize:= TATPageUpDownSize(edPage.ItemIndex);
      ed.OptKeyLeftRightSwapSel:= chkLeftRt.Checked;
      ed.OptKeyHomeToNonSpace:= chkHome.Checked;
      ed.OptKeyEndToNonSpace:= chkEnd.Checked;
      ed.OptKeyPageKeepsRelativePos:= chkPageKeepRel.Checked;
      ed.OptKeyTabIndents:= chkTabInd.Checked;
      ed.OptAutoIndent:= chkAutoInd.Checked;
      ed.OptAutoIndentKind:= TATAutoIndentKind(edAutoInd.ItemIndex);
      ed.OptIndentSize:= edIndent.Value;
      ed.OptIndentKeepsAlign:= chkUninKeep.Checked;

      //mouse
      ed.OptMouse2ClickSelectsLine:= chkClick2.Checked;
      ed.OptMouse3ClickSelectsLine:= chkClick3.Checked;
      ed.OptMouse2ClickDragSelectsWords:= chkClick2W.Checked;
      ed.OptMouseGutterClickSelectsLine:= chkClickNm.Checked;
      ed.OptMouseDragDrop:= chkDnD.Checked;
      ed.OptMouseRightClickMovesCaret:= chkRtMove.Checked;
      ed.OptMouseNiceScroll:= chkNice.Checked;

      //undo
      ed.OptUndoLimit:= edUndo.Value;
      ed.OptUndoGrouped:= chkUndoGr.Checked;
      ed.OptUndoAfterSave:= chkUndoSv.Checked;

      //apply
      ed.Gutter.Update;
      ed.Update;
    end;
  end;
end;


{ TfmOpt }

procedure TfmOpt.FormCreate(Sender: TObject);
begin
  InitShape(edCrShape);
  InitShape(edCrShape2);
end;

procedure SwapItems(L: TListbox; n1, n2: integer);
var
  s: string;
begin
  s:= L.Items[n1];
  L.Items[n1]:= L.Items[n2];
  L.Items[n2]:= s;
  L.ItemIndex:= n2;
end;

procedure TfmOpt.bColUpClick(Sender: TObject);
begin
  with ListCol do
    if ItemIndex>0 then
      SwapItems(ListCol, ItemIndex, ItemIndex-1);
end;

procedure TfmOpt.bColDownClick(Sender: TObject);
begin
  with ListCol do
    if ItemIndex<Count-1 then
      SwapItems(ListCol, ItemIndex, ItemIndex+1);
end;

procedure TfmOpt.InitShape(ed: TCombobox);
begin
  ed.Items.Clear;
  ed.Items.Add('full');
  ed.Items.Add('vert 1px');
  ed.Items.Add('vert 2px');
  ed.Items.Add('vert 3px');
  ed.Items.Add('vert 4px');
  ed.Items.Add('vert 10%');
  ed.Items.Add('vert 20%');
  ed.Items.Add('vert 30%');
  ed.Items.Add('vert 40%');
  ed.Items.Add('vert 50%');
  ed.Items.Add('horz 1px');
  ed.Items.Add('horz 2px');
  ed.Items.Add('horz 3px');
  ed.Items.Add('horz 4px');
  ed.Items.Add('horz 10%');
  ed.Items.Add('horz 20%');
  ed.Items.Add('horz 30%');
  ed.Items.Add('horz 40%');
  ed.Items.Add('horz 50%');
end;

end.

