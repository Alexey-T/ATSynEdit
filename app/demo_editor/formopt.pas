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
    chkBackspUnindent: TCheckBox;
    chkEnterIndent: TCheckBox;
    chkPopupDown: TCheckBox;
    chkTabIndent: TCheckBox;
    chkUnindentKeepAlign: TCheckBox;
    chkUnprintAsciiRep: TCheckBox;
    chkShowFoldLines: TCheckBox;
    chkShowFoldAlways: TCheckBox;
    chkCrPreferLeft: TCheckBox;
    chkGutterFold: TCheckBox;
    chkKeepCol: TCheckBox;
    chkCurLineMin: TCheckBox;
    chkHint: TCheckBox;
    chkPageKeepRel: TCheckBox;
    chkNavHomeEnd: TCheckBox;
    chkGutterBm: TCheckBox;
    chkGutterEmpty: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterStat: TCheckBox;
    chkNice: TCheckBox;
    chkSaveEol: TCheckBox;
    chkSaveTrim: TCheckBox;
    chkShowNum1st: TCheckBox;
    chkShowNumCr: TCheckBox;
    chkMapBord: TCheckBox;
    chkMapAlw: TCheckBox;
    chkShowNumBg: TCheckBox;
    chkTabSpaces: TCheckBox;
    chkUndoSv: TCheckBox;
    chkUndoGr: TCheckBox;
    chkCutNoSel: TCheckBox;
    chkDotLn: TCheckBox;
    chkClickNm: TCheckBox;
    chkCrUnfocus: TCheckBox;
    chkEndNonspace: TCheckBox;
    chkHomeNonspace: TCheckBox;
    chkLeftRtSwap: TCheckBox;
    chkNavUpDown: TCheckBox;
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
    chkUnprintEnd: TCheckBox;
    chkUnprintEndDet: TCheckBox;
    chkUnprintSpace: TCheckBox;
    chkUnprintEn: TCheckBox;
    edIndentKind: TComboBox;
    edCrShape: TComboBox;
    edCrShape2: TComboBox;
    edCrTime: TSpinEdit;
    edChars: TEdit;
    edIndentSize: TSpinEdit;
    edPlusSize: TSpinEdit;
    edNumSkip: TEdit;
    edMapFont: TSpinEdit;
    edNum: TComboBox;
    edPageSize: TComboBox;
    edRulerFSize: TSpinEdit;
    edRulerSize: TSpinEdit;
    edSizeBm: TSpinEdit;
    edSizeEmpty: TSpinEdit;
    edSizeState: TSpinEdit;
    edSizeFold: TSpinEdit;
    edSizeNum1: TSpinEdit;
    edSizeNum2: TSpinEdit;
    edTabArrowSize: TSpinEdit;
    edTabArrowPnt: TSpinEdit;
    groupIndent: TGroupBox;
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
    ListShapes: TListBox;
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
    TabSheet8: TTabSheet;
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
    chkDotLn.Checked:= ed.OptShowIndentLines;
    edChars.Text:= ed.OptWordChars;
    chkSaveEol.Checked:= ed.OptSavingForceFinalEol;
    chkSaveTrim.Checked:= ed.OptSavingTrimSpaces;
    chkHint.Checked:= ed.OptShowScrollHint;

    //unprint
    chkUnprintEn.Checked:= ed.OptUnprintedVisible;
    chkUnprintSpace.Checked:= ed.OptUnprintedSpaces;
    chkUnprintEnd.Checked:= ed.OptUnprintedEnds;
    chkUnprintEndDet.Checked:= ed.OptUnprintedEndsDetails;
    chkUnprintAsciiRep.Checked:= ed.OptUnprintedReplaceSpec;
    edTabArrowSize.Value:= ed.OptUnprintedArrowSize;
    edTabArrowPnt.Value:= ed.OptUnprintedArrowPointer;

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
    edSizeNum1.Value:= ed.OptNumbersIndentLeft;
    edSizeNum2.Value:= ed.OptNumbersIndentRight;

    //minimap
    edMapFont.Value:= ed.OptMinimapFontSize;
    chkMapBord.Checked:= ed.OptMinimapShowSelBorder;
    chkMapAlw.Checked:= ed.OptMinimapShowSelAlways;

    //key
    chkTabSpaces.Checked:= ed.OptTabSpaces;
    chkOvrSel.Checked:= ed.OptOverwriteSel;
    chkNavUpDown.Checked:= ed.OptKeyUpDownNavigateWrapped;
    chkNavHomeEnd.Checked:= ed.OptKeyHomeEndNavigateWrapped;
    chkKeepCol.Checked:= ed.OptKeyUpDownKeepColumn;
    chkLeftRtSwap.Checked:= ed.OptKeyLeftRightSwapSel;
    chkHomeNonspace.Checked:= ed.OptKeyHomeToNonSpace;
    chkEndNonspace.Checked:= ed.OptKeyEndToNonSpace;
    chkTabIndent.Checked:= ed.OptKeyTabIndents;
    chkEnterIndent.Checked:= ed.OptAutoIndent;
    chkBackspUnindent.Checked:= ed.OptKeyBackspaceUnindent;
    edIndentKind.ItemIndex:= Ord(ed.OptAutoIndentKind);
    edIndentSize.Value:= ed.OptIndentSize;
    chkUnindentKeepAlign.Checked:= ed.OptIndentKeepsAlign;
    edPageSize.ItemIndex:= Ord(ed.OptKeyPageUpDownSize);
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
      ed.OptLastLineOnTop:= chkLastOnTop.Checked;
      ed.OptShowIndentLines:= chkDotLn.Checked;
      ed.OptSavingForceFinalEol:= chkSaveEol.Checked;
      ed.OptSavingTrimSpaces:= chkSaveTrim.Checked;
      ed.OptShowScrollHint:= chkHint.Checked;

      //unprint
      ed.OptUnprintedVisible     := chkUnprintEn.Checked;
      ed.OptUnprintedSpaces      := chkUnprintSpace.Checked;
      ed.OptUnprintedEnds        := chkUnprintEnd.Checked;
      ed.OptUnprintedEndsDetails := chkUnprintEndDet.Checked;
      ed.OptUnprintedReplaceSpec:= chkUnprintAsciiRep.Checked;
      ed.OptUnprintedArrowSize:= edTabArrowSize.Value;
      ed.OptUnprintedArrowPointer := edTabArrowPnt.Value;

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
      ed.OptNumbersIndentLeft:= edSizeNum1.Value;
      ed.OptNumbersIndentRight:= edSizeNum2.Value;

      //minimap
      ed.OptMinimapFontSize:= edMapFont.Value;
      ed.OptMinimapShowSelBorder:= chkMapBord.Checked;
      ed.OptMinimapShowSelAlways:= chkMapAlw.Checked;

      //key
      ed.OptTabSpaces:= chkTabSpaces.Checked;
      ed.OptOverwriteSel:= chkOvrSel.Checked;
      ed.OptKeyUpDownKeepColumn:= chkKeepCol.Checked;
      ed.OptKeyUpDownNavigateWrapped:= chkNavUpDown.Checked;
      ed.OptKeyHomeEndNavigateWrapped:= chkNavHomeEnd.Checked;
      ed.OptKeyPageUpDownSize:= TATPageUpDownSize(edPageSize.ItemIndex);
      ed.OptKeyLeftRightSwapSel:= chkLeftRtSwap.Checked;
      ed.OptKeyHomeToNonSpace:= chkHomeNonspace.Checked;
      ed.OptKeyEndToNonSpace:= chkEndNonspace.Checked;
      ed.OptKeyPageKeepsRelativePos:= chkPageKeepRel.Checked;
      ed.OptKeyTabIndents:= chkTabIndent.Checked;
      ed.OptAutoIndent:= chkEnterIndent.Checked;
      ed.OptKeyBackspaceUnindent := chkBackspUnindent.Checked;
      ed.OptAutoIndentKind:= TATAutoIndentKind(edIndentKind.ItemIndex);
      ed.OptIndentSize:= edIndentSize.Value;
      ed.OptIndentKeepsAlign:= chkUnindentKeepAlign.Checked;

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
  ed.Items.AddStrings(ListShapes.Items);
end;

end.

