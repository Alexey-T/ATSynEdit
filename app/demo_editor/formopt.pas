unit formopt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  Forms, Controls, Graphics, StdCtrls, Dialogs,
  ButtonPanel, Spin, ComCtrls, ExtCtrls,
  FileUtil,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_CharSizer;

type
  { TfmOpt }

  TfmOpt = class(TForm)
    bColDown: TButton;
    bColUp: TButton;
    ButtonPanel1: TButtonPanel;
    chkUndoForCrt: TCheckBox;
    chkUndoGrp: TCheckBox;
    chkUndoAfterSave: TCheckBox;
    chkUpDownToEdge: TCheckBox;
    chkCrPrimitiveCol: TCheckBox;
    chkClickLink: TCheckBox;
    chkCopyNoSel: TCheckBox;
    chkCutNoSel: TCheckBox;
    chkDotLn: TCheckBox;
    chkPasteOvr: TCheckBox;
    chkPasteSpread: TCheckBox;
    chkSaveEol: TCheckBox;
    chkSaveTrim: TCheckBox;
    chkSaveTrimEmptyLines: TCheckBox;
    chkBkspGoPrev: TCheckBox;
    chkUnprintOnlyBothEnds: TCheckBox;
    chkUnprintOnlyEnd: TCheckBox;
    chkCrEmptyNormal: TCheckBox;
    chkCrBlinkEn: TCheckBox;
    chkMsNormalSel: TCheckBox;
    chkMsColumnSel: TCheckBox;
    chkShowFullHilite: TCheckBox;
    chkMsHideCursor: TCheckBox;
    chkLeftRtSwapAndSel: TCheckBox;
    chkGutterSep: TCheckBox;
    chkGutterNumAuto: TCheckBox;
    chkGutterBm: TCheckBox;
    chkGutterEmpty: TCheckBox;
    chkGutterFold: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterStat: TCheckBox;
    chkShowFoldLinesAll: TCheckBox;
    chkBackspUnindent: TCheckBox;
    chkEnterIndent: TCheckBox;
    chkTabIndent: TCheckBox;
    chkUnindentKeepAlign: TCheckBox;
    chkUnprintAsciiRep: TCheckBox;
    chkShowFoldLines: TCheckBox;
    chkShowFoldAlways: TCheckBox;
    chkCrPreferLeft: TCheckBox;
    chkKeepCol: TCheckBox;
    chkCurLineMin: TCheckBox;
    chkScrollHint: TCheckBox;
    chkPageKeepRel: TCheckBox;
    chkNavHomeEnd: TCheckBox;
    chkShowNum1st: TCheckBox;
    chkShowNumCr: TCheckBox;
    chkMapSelBorder: TCheckBox;
    chkMapSelAlways: TCheckBox;
    chkShowNumBg: TCheckBox;
    chkTabSpaces: TCheckBox;
    chkMsClickNumSel: TCheckBox;
    chkCrStopUnfocus: TCheckBox;
    chkEndNonspace: TCheckBox;
    chkHomeNonspace: TCheckBox;
    chkLeftRtSwap: TCheckBox;
    chkNavUpDown: TCheckBox;
    chkOvrSel: TCheckBox;
    chkMsRtClickMove: TCheckBox;
    chkMsDragDrop: TCheckBox;
    chkCrMul: TCheckBox;
    chkCrVirt: TCheckBox;
    chkMsClick2Drag: TCheckBox;
    chkMsClick3: TCheckBox;
    chkShowFullSel: TCheckBox;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    chkLastOnTop: TCheckBox;
    chkUnprintEnd: TCheckBox;
    chkUnprintEndDet: TCheckBox;
    chkUnprintSpace: TCheckBox;
    chkUnprintEn: TCheckBox;
    chkZebraActive: TCheckBox;
    edUnpriEol: TComboBox;
    comboMsMidClick: TComboBox;
    ComboMsClick2: TComboBox;
    comboRulerStyle: TComboBox;
    edCrHeightNormal: TSpinEdit;
    edRulerFSize: TSpinEdit;
    edRulerIndent: TSpinEdit;
    edRulerSize: TSpinEdit;
    edMapCharWidth: TSpinEdit;
    edNumAlign: TComboBox;
    edIndentKind: TComboBox;
    edCrTime: TSpinEdit;
    edSizeSep: TSpinEdit;
    edNonWordChars: TEdit;
    edIndentSize: TSpinEdit;
    edPlusSize: TSpinEdit;
    edNumChar: TEdit;
    edNumStyle: TComboBox;
    edPageSize: TComboBox;
    edSizeBm: TSpinEdit;
    edSizeEmpty: TSpinEdit;
    edSizeFold: TSpinEdit;
    edSizeNumIndent: TSpinEdit;
    edSizeNum: TSpinEdit;
    edSizeState: TSpinEdit;
    edTabArrowSize: TSpinEdit;
    edTabArrowPnt: TSpinEdit;
    edTextHint: TEdit;
    edUndoLimit: TSpinEdit;
    edUndoPause: TSpinEdit;
    edZebraAlpha: TSpinEdit;
    edZebraStep: TSpinEdit;
    grpOrderCols: TGroupBox;
    grpSizeCols: TGroupBox;
    grpUndo: TGroupBox;
    grpZebra: TGroupBox;
    groupIndent: TGroupBox;
    LabChars: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label6: TLabel;
    LabelZebraAlpha: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelArr: TLabel;
    LabelArr1: TLabel;
    LabelHint: TLabel;
    LabelZebraStep: TLabel;
    ListCol: TListBox;
    PageControl1: TPageControl;
    edCrWidthNormal: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    procedure bColDownClick(Sender: TObject);
    procedure bColUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
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
  nameSep = 'separator';

procedure DoConfigEditor(ed: TATSynEdit);
var
  i: integer;
begin
  with fmOpt do
  begin
    with ListCol do
    begin
      Items.Clear;
      for i:= 0 to 4 do
      begin
        if i=ed.GutterBandBookmarks then Items.Add(nameBm);
        if i=ed.GutterBandNumbers then Items.Add(nameNums);
        if i=ed.GutterBandStates then Items.Add(nameState);
        if i=ed.GutterBandFolding then Items.Add(nameFold);
        if i=ed.GutterBandSeparator then Items.Add(nameSep);
      end;
      ItemIndex:= 0;
    end;

    //general
    chkCurLine.Checked:= ed.OptShowCurLine;
    chkCurLineMin.Checked:= ed.OptShowCurLineMinimal;
    chkCurCol.Checked:= ed.OptShowCurColumn;
    chkLastOnTop.Checked:= ed.OptLastLineOnTop;
    chkShowFullSel.Checked:= ed.OptShowFullWidthForSelection;
    chkShowFullHilite.Checked:= ed.OptShowFullWidthForSyntaxHilite;
    chkCopyNoSel.Checked:= ed.OptCopyLinesIfNoSel;
    chkCutNoSel.Checked:= ed.OptCutLinesIfNoSel;
    chkPasteOvr.Checked:= ed.OptOverwriteAllowedOnPaste;
    chkDotLn.Checked:= ed.OptShowIndentLines;
    edTextHint.Text:= ed.OptTextHint;
    edNonWordChars.Text:= UTF8Encode(ed.OptNonWordChars);
    chkSaveEol.Checked:= ed.OptSavingForceFinalEol;
    chkSaveTrim.Checked:= ed.OptSavingTrimSpaces;
    chkSaveTrimEmptyLines.Checked:= ed.OptSavingTrimFinalEmptyLines;
    chkScrollHint.Checked:= ed.OptShowScrollHint;
    chkClickLink.Checked:= ed.OptMouseClickOpensURL;
    chkPasteSpread.Checked:= ed.OptPasteMultilineTextSpreadsToCarets;

    //unprint
    chkUnprintEn.Checked:= ed.OptUnprintedVisible;
    chkUnprintSpace.Checked:= ed.OptUnprintedSpaces;
    chkUnprintEnd.Checked:= ed.OptUnprintedEnds;
    chkUnprintEndDet.Checked:= ed.OptUnprintedEndsDetails;
    chkUnprintOnlyBothEnds.Checked:= ed.OptUnprintedSpacesBothEnds;
    chkUnprintOnlyEnd.Checked:= ed.OptUnprintedSpacesTrailing;
    edUnpriEol.ItemIndex:= Ord(OptUnprintedEndSymbol);
    chkUnprintAsciiRep.Checked:= OptUnprintedReplaceSpec;
    edTabArrowSize.Value:= OptUnprintedTabCharLength;
    edTabArrowPnt.Value:= OptUnprintedTabPointerScale;

    //caret
    chkCrBlinkEn.Checked:= ed.OptCaretBlinkEnabled;
    edCrTime.Value:= ed.OptCaretBlinkTime;
    chkCrVirt.Checked:= ed.OptCaretVirtual;
    chkCrMul.Checked:= ed.OptCaretManyAllowed;
    chkCrStopUnfocus.Checked:= ed.OptCaretStopUnfocused;
    chkCrPreferLeft.Checked:= ed.OptCaretPreferLeftSide;
    chkCrPrimitiveCol.Checked:= ed.OptCaretsPrimitiveColumnSelection;

    edCrWidthNormal.Value:= ed.CaretShapeNormal.Width;
    edCrHeightNormal.Value:= ed.CaretShapeNormal.Height;
    chkCrEmptyNormal.Checked:= ed.CaretShapeNormal.EmptyInside;

    //gutter
    edNumStyle.ItemIndex:= Ord(ed.OptNumbersStyle);
    edNumAlign.ItemIndex:= Ord(ed.OptNumbersAlignment);
    //edNumSize.Value:= ed.OptNumbersFontSizePercents;
    edPlusSize.Value:= ed.OptGutterPlusSize;
    chkShowNum1st.Checked:= ed.OptNumbersShowFirst;
    chkShowNumCr.Checked:= ed.OptNumbersShowCarets;
    chkShowNumBg.Checked:= ed.OptShowGutterCaretBG;
    chkShowFoldAlways.Checked:= ed.OptGutterShowFoldAlways;
    chkShowFoldLines.Checked:= ed.OptGutterShowFoldLines;
    chkShowFoldLinesAll.Checked:= ed.OptGutterShowFoldLinesAll;
    edRulerSize.Value:= ed.OptRulerHeightPercents;
    edRulerFSize.Value:= ed.OptRulerFontSizePercents;
    edRulerIndent.Value:= ed.OptRulerTopIndentPercents;
    comboRulerStyle.ItemIndex:= Ord(ed.OptRulerNumeration);

    chkGutterBm.Checked:= ed.Gutter[ed.GutterBandBookmarks].Visible;
    chkGutterNum.Checked:= ed.Gutter[ed.GutterBandNumbers].Visible;
    chkGutterFold.Checked:= ed.Gutter[ed.GutterBandFolding].Visible;
    chkGutterStat.Checked:= ed.Gutter[ed.GutterBandStates].Visible;
    chkGutterSep.Checked:= ed.Gutter[ed.GutterBandSeparator].Visible;
    chkGutterEmpty.Checked:= ed.Gutter[ed.GutterBandEmpty].Visible;
    edSizeBm.Value:= ed.Gutter[ed.GutterBandBookmarks].Size;
    edSizeFold.Value:= ed.Gutter[ed.GutterBandFolding].Size;
    edSizeState.Value:= ed.Gutter[ed.GutterBandStates].Size;
    edSizeSep.Value:= ed.Gutter[ed.GutterBandSeparator].Size;
    edSizeEmpty.Value:= ed.Gutter[ed.GutterBandEmpty].Size;
    edSizeNum.Value:= ed.Gutter[ed.GutterBandNumbers].Size;
    edSizeNumIndent.Value:= ed.OptNumbersIndentPercents;
    chkGutterNumAuto.Checked:= ed.OptNumbersAutosize;

    //minimap
    edMapCharWidth.Value:= ed.OptMinimapCharWidth;
    chkMapSelBorder.Checked:= ed.OptMinimapShowSelBorder;
    chkMapSelAlways.Checked:= ed.OptMinimapShowSelAlways;

    //key
    chkTabSpaces.Checked:= ed.OptTabSpaces;
    chkOvrSel.Checked:= ed.OptOverwriteSel;
    chkNavUpDown.Checked:= ed.OptKeyUpDownNavigateWrapped;
    chkUpDownToEdge.Checked:= ed.OptKeyUpDownAllowToEdge;
    chkNavHomeEnd.Checked:= ed.OptKeyHomeEndNavigateWrapped;
    chkKeepCol.Checked:= ed.OptKeyUpDownKeepColumn;
    chkLeftRtSwap.Checked:= ed.OptKeyLeftRightSwapSel;
    chkLeftRtSwapAndSel.Checked:= ed.OptKeyLeftRightSwapSelAndSelect;
    chkHomeNonspace.Checked:= ed.OptKeyHomeToNonSpace;
    chkEndNonspace.Checked:= ed.OptKeyEndToNonSpace;
    chkTabIndent.Checked:= ed.OptKeyTabIndents;
    chkEnterIndent.Checked:= ed.OptAutoIndent;
    chkBackspUnindent.Checked:= ed.OptKeyBackspaceUnindent;
    chkBkspGoPrev.Checked:= ed.OptKeyBackspaceGoesToPrevLine;
    edIndentKind.ItemIndex:= Ord(ed.OptAutoIndentKind);
    edIndentSize.Value:= ed.OptIndentSize;
    chkUnindentKeepAlign.Checked:= ed.OptIndentKeepsAlign;
    edPageSize.ItemIndex:= Ord(ed.OptKeyPageUpDownSize);
    chkPageKeepRel.Checked:= ed.OptKeyPageKeepsRelativePos;

    //mouse
    chkMsNormalSel.Checked:= ed.OptMouseEnableNormalSelection;
    chkMsColumnSel.Checked:= ed.OptMouseEnableColumnSelection;
    ComboMsClick2.ItemIndex:= Ord(ed.OptMouse2ClickAction);
    chkMsClick3.Checked:= ed.OptMouse3ClickSelectsLine;
    chkMsClick2Drag.Checked:= ed.OptMouse2ClickDragSelectsWords;
    chkMsClickNumSel.Checked:= ed.OptMouseClickNumberSelectsLine;
    chkMsDragDrop.Checked:= ed.OptMouseDragDrop;
    chkMsRtClickMove.Checked:= ed.OptMouseRightClickMovesCaret;
    comboMsMidClick.ItemIndex:= Ord(ed.OptMouseMiddleClickAction);
    chkMsHideCursor.Checked:= ed.OptMouseHideCursorOnType;

    //undo
    edUndoLimit.Value:= ed.OptUndoLimit;
    edUndoPause.Value:= ed.OptUndoPause;
    chkUndoGrp.Checked:= ed.OptUndoGrouped;
    chkUndoAfterSave.Checked:= ed.OptUndoAfterSave;
    chkUndoForCrt.Checked:= ed.OptUndoForCaretJump;

    //zebra
    chkZebraActive.Checked:= ed.OptZebraActive;
    edZebraAlpha.Value:= ed.OptZebraAlphaBlend;
    edZebraStep.Value:= ed.OptZebraStep;

    if ShowModal=mrOk then
    begin
      ed.GutterBandBookmarks:= ListCol.Items.IndexOf(nameBm);
      ed.GutterBandNumbers:= ListCol.Items.IndexOf(nameNums);
      ed.GutterBandStates:= ListCol.Items.IndexOf(nameState);
      ed.GutterBandFolding:= ListCol.Items.IndexOf(nameFold);
      ed.GutterBandSeparator:= ListCol.Items.IndexOf(nameSep);

      //general
      ed.OptShowCurLine:= chkCurLine.Checked;
      ed.OptShowCurLineMinimal:= chkCurLineMin.Checked;
      ed.OptShowCurColumn:= chkCurCol.Checked;
      ed.OptTextHint:= edTextHint.Text;
      ed.OptNonWordChars:= UTF8Decode(edNonWordChars.Text);
      ed.OptOverwriteAllowedOnPaste:= chkPasteOvr.Checked;
      ed.OptCopyLinesIfNoSel:= chkCopyNoSel.Checked;
      ed.OptCutLinesIfNoSel:= chkCutNoSel.Checked;
      ed.OptShowFullWidthForSelection:= chkShowFullSel.Checked;
      ed.OptShowFullWidthForSyntaxHilite:= chkShowFullHilite.Checked;
      ed.OptLastLineOnTop:= chkLastOnTop.Checked;
      ed.OptShowIndentLines:= chkDotLn.Checked;
      ed.OptSavingForceFinalEol:= chkSaveEol.Checked;
      ed.OptSavingTrimSpaces:= chkSaveTrim.Checked;
      ed.OptSavingTrimFinalEmptyLines:= chkSaveTrimEmptyLines.Checked;
      ed.OptShowScrollHint:= chkScrollHint.Checked;
      ed.OptMouseClickOpensURL:= chkClickLink.Checked;
      ed.OptPasteMultilineTextSpreadsToCarets:= chkPasteSpread.Checked;

      //unprint
      ed.OptUnprintedVisible:= chkUnprintEn.Checked;
      ed.OptUnprintedSpaces:= chkUnprintSpace.Checked;
      ed.OptUnprintedEnds:= chkUnprintEnd.Checked;
      ed.OptUnprintedEndsDetails:= chkUnprintEndDet.Checked;
      ed.OptUnprintedSpacesBothEnds:= chkUnprintOnlyBothEnds.Checked;
      ed.OptUnprintedSpacesTrailing:= chkUnprintOnlyEnd.Checked;
      OptUnprintedReplaceSpec:= chkUnprintAsciiRep.Checked;
      OptUnprintedTabCharLength:= edTabArrowSize.Value;
      OptUnprintedTabPointerScale:= edTabArrowPnt.Value;
      OptUnprintedEndSymbol:= TATSynEditUnptintedEolSymbol(edUnpriEol.ItemIndex);

      //caret
      ed.OptCaretBlinkEnabled:= chkCrBlinkEn.Checked;
      ed.OptCaretBlinkTime:= edCrTime.Value;
      ed.OptCaretVirtual:= chkCrVirt.Checked;
      ed.OptCaretManyAllowed:= chkCrMul.Checked;
      ed.OptCaretStopUnfocused:= chkCrStopUnfocus.Checked;
      ed.OptCaretPreferLeftSide:= chkCrPreferLeft.Checked;
      ed.OptCaretsPrimitiveColumnSelection:= chkCrPrimitiveCol.Checked;

      ed.CaretShapeNormal.Width:= edCrWidthNormal.Value;
      ed.CaretShapeNormal.Height:= edCrHeightNormal.Value;
      ed.CaretShapeNormal.EmptyInside:= chkCrEmptyNormal.Checked;

      //gutter
      //ed.OptNumbersFontSizePercents:= edNumSize.Value;
      ed.OptNumbersStyle:= TATEditorNumbersStyle(edNumStyle.ItemIndex);
      ed.OptNumbersAlignment:= TAlignment(edNumAlign.ItemIndex);
      ed.OptNumbersShowFirst:= chkShowNum1st.Checked;
      ed.OptNumbersShowCarets:= chkShowNumCr.Checked;
      ed.OptGutterShowFoldAlways:= chkShowFoldAlways.Checked;
      ed.OptGutterShowFoldLines:= chkShowFoldLines.Checked;
      ed.OptGutterShowFoldLinesAll:= chkShowFoldLinesAll.Checked;
      ed.OptGutterPlusSize:= edPlusSize.Value;
      ed.OptShowGutterCaretBG:= chkShowNumBg.Checked;
      ed.OptRulerHeightPercents:= edRulerSize.Value;
      ed.OptRulerFontSizePercents:= edRulerFSize.Value;
      ed.OptRulerTopIndentPercents:= edRulerIndent.Value;
      ed.OptRulerNumeration:= TATEditorRulerNumeration(comboRulerStyle.ItemIndex);

      ed.Gutter[ed.GutterBandBookmarks].Visible:= chkGutterBm.Checked;
      ed.Gutter[ed.GutterBandNumbers].Visible:= chkGutterNum.Checked;
      ed.Gutter[ed.GutterBandFolding].Visible:= chkGutterFold.Checked;
      ed.Gutter[ed.GutterBandStates].Visible:= chkGutterStat.Checked;
      ed.Gutter[ed.GutterBandSeparator].Visible:= chkGutterSep.Checked;
      ed.Gutter[ed.GutterBandEmpty].Visible:= chkGutterEmpty.Checked;
      ed.Gutter[ed.GutterBandBookmarks].Size:= edSizeBm.Value;
      ed.Gutter[ed.GutterBandNumbers].Size:= edSizeNum.Value;
      ed.Gutter[ed.GutterBandFolding].Size:= edSizeFold.Value;
      ed.Gutter[ed.GutterBandStates].Size:= edSizeState.Value;
      ed.Gutter[ed.GutterBandSeparator].Size:= edSizeSep.Value;
      ed.Gutter[ed.GutterBandEmpty].Size:= edSizeEmpty.Value;
      ed.OptNumbersAutosize:= chkGutterNumAuto.Checked;
      ed.OptNumbersIndentPercents:= edSizeNumIndent.Value;

      //minimap
      ed.OptMinimapCharWidth:= edMapCharWidth.Value;
      ed.OptMinimapShowSelBorder:= chkMapSelBorder.Checked;
      ed.OptMinimapShowSelAlways:= chkMapSelAlways.Checked;

      //key
      ed.OptTabSpaces:= chkTabSpaces.Checked;
      ed.OptOverwriteSel:= chkOvrSel.Checked;
      ed.OptKeyUpDownKeepColumn:= chkKeepCol.Checked;
      ed.OptKeyUpDownAllowToEdge:= chkUpDownToEdge.Checked;
      ed.OptKeyUpDownNavigateWrapped:= chkNavUpDown.Checked;
      ed.OptKeyHomeEndNavigateWrapped:= chkNavHomeEnd.Checked;
      ed.OptKeyPageUpDownSize:= TATEditorPageDownSize(edPageSize.ItemIndex);
      ed.OptKeyLeftRightSwapSel:= chkLeftRtSwap.Checked;
      ed.OptKeyLeftRightSwapSelAndSelect:= chkLeftRtSwapAndSel.Checked;
      ed.OptKeyHomeToNonSpace:= chkHomeNonspace.Checked;
      ed.OptKeyEndToNonSpace:= chkEndNonspace.Checked;
      ed.OptKeyPageKeepsRelativePos:= chkPageKeepRel.Checked;
      ed.OptKeyTabIndents:= chkTabIndent.Checked;
      ed.OptAutoIndent:= chkEnterIndent.Checked;
      ed.OptKeyBackspaceUnindent := chkBackspUnindent.Checked;
      ed.OptKeyBackspaceGoesToPrevLine:= chkBkspGoPrev.Checked;
      ed.OptAutoIndentKind:= TATEditorAutoIndentKind(edIndentKind.ItemIndex);
      ed.OptIndentSize:= edIndentSize.Value;
      ed.OptIndentKeepsAlign:= chkUnindentKeepAlign.Checked;

      //mouse
      ed.OptMouseEnableNormalSelection:= chkMsNormalSel.Checked;
      ed.OptMouseEnableColumnSelection:= chkMsColumnSel.Checked;
      ed.OptMouse2ClickAction:= TATEditorDoubleClickAction(ComboMsClick2.ItemIndex);
      ed.OptMouse3ClickSelectsLine:= chkMsClick3.Checked;
      ed.OptMouse2ClickDragSelectsWords:= chkMsClick2Drag.Checked;
      ed.OptMouseClickNumberSelectsLine:= chkMsClickNumSel.Checked;
      ed.OptMouseDragDrop:= chkMsDragDrop.Checked;
      ed.OptMouseRightClickMovesCaret:= chkMsRtClickMove.Checked;
      ed.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(comboMsMidClick.ItemIndex);
      ed.OptMouseHideCursorOnType:= chkMsHideCursor.Checked;

      //undo
      ed.OptUndoLimit:= edUndoLimit.Value;
      ed.OptUndoPause:= edUndoPause.Value;
      ed.OptUndoGrouped:= chkUndoGrp.Checked;
      ed.OptUndoAfterSave:= chkUndoAfterSave.Checked;
      ed.OptUndoForCaretJump:= chkUndoForCrt.Checked;

      //zebra
      ed.OptZebraActive     := chkZebraActive.Checked;
      ed.OptZebraAlphaBlend := edZebraAlpha.Value;
      ed.OptZebraStep       := edZebraStep.Value;

      //apply
      ed.Gutter.Update;
      ed.Update;
    end;
  end;
end;


{ TfmOpt }

procedure TfmOpt.FormCreate(Sender: TObject);
begin
end;

procedure TfmOpt.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex:= 0;
end;

procedure TfmOpt.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

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


end.

