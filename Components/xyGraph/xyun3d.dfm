�
 TFORM1 0�'  TPF0TForm1Form1Left� Topk
AutoScrollCaption2XYTEST3D: sample/test program for XYGRAPH3D/4D 2.4ClientHeight�ClientWidth@Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderScaled
OnActivateFormActivateOnCreate
FormCreatePixelsPerInch`
TextHeight 
TStatusBar
StatusBar1Left TopxWidth@HeightPanels SimplePanel	  TPageControlPageControl1LeftTop Width9Heightu
ActivePage	TabSheet1AnchorsakLeftakTopakRightakBottom Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 	MultiLine	
ParentFontTabOrderTabPositiontpLeftOnChangePageControl1Change 	TTabSheet	TabSheet1CaptionCartesian Coord 	TGroupBox	GroupBox2LeftTop� Width� Height� TabOrder TLabelLabel5Left� Top8WidthHeightCaptionRev  TLabelLabel6Left� TopXWidthHeightCaptionGrid  TLabelLabel8Left� Top� WidthHeightCaptionStyle  TLabelLabel4Left� TopxWidthHeightCaptionFast  	TPaintBox	PaintBox2LeftTopWidth� Height� HintY|demonstration of contour plots; click for 3D ruler, zoom (Ctrl) or cross-section (Shift)OnMouseDownPaintBox2MouseDownOnMouseMovePaintBox1MouseMove	OnMouseUpPaintBox2MouseUpOnPaintPaintBox2Paint  	TCheckBox	CheckBox2Left� TopdWidthHeightHint|set grid on/offTabOrder OnClickCheckBox2Click  	TCheckBox	CheckBox3Left� TopDWidthHeightHint)|reverse colour scheme (only for style 2)TabOrderOnClickCheckBox2Click  TButtonButton3Left� TopWidthHeight)Hint.|toggle colour scheme (only for style 2 and 3)CaptionCTabOrderOnClickButton3Click  	TSpinEdit	SpinEdit8Left� Top� WidthHeightHint2|select the style of the presentation, range 0 - 5MaxValueMinValue TabOrderValue OnChangeSpinEdit8Change  	TCheckBox	CheckBox7Left� Top� WidthHeightHint|set fast mode on/offCaption	CheckBox7TabOrderOnClickCheckBox2Click   	TGroupBox	GroupBox1LeftTop Width� Height� TabOrder  TImageImage1LeftTopWidth� Height� Hint\|demonstration of surface plots with animation, or cross-section; click to restart animationOnClickImage1Click  TLabelLabel7Left� Top� WidthHeightCaptionStyle  
TScrollBar
ScrollBar1Left� TopWidthHeightEHint)|set the tilt of the surface presentationKind
sbVerticalMaxMin�PageSize TabOrder   	TCheckBox	CheckBox1Left� Top|WidthHeightHint|switch frame on/offCaptionFTabOrder  	TSpinEdit	SpinEdit7Left� Top� WidthHeightHint2|select the style of the presentation, range 0 - 6MaxValueMinValue TabOrderValue OnChangeSpinEdit7Change  	TCheckBox	CheckBox5Left� ToplWidthHeightHint|show as solid modelCaptionSTabOrderOnClickCheckBox5Click  	TCheckBox	CheckBox6Left� TopXWidthHeightHint|show as thick modelCaptionTTabOrderOnClickCheckBox6Click   TRadioGroupRadioGroup1Left� Top WidthVHeight.Hint2|select item for surface and contour presentationsCaptionTestsurfaceItems.Stringsexponantialpyramid TabOrderOnClickRadioGroup1Click  	TGroupBox	GroupBox3Left/Top WidthFHeight.Caption	TrianglesFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style 
ParentFontTabOrder 	TSpinEdit	SpinEdit1LeftTopWidth%HeightHint@|select the way rectangles are split into triangles, range 0 - 7Font.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style MaxValueMinValue 
ParentFontTabOrder ValueOnChangeSpinEdit1Change   	TGroupBox	GroupBox4LeftdTop WidthmHeight.CaptionLight sourceTabOrder TUpDownUpDown2LeftTopWidthHeightHint#|select orientation of light sourceMin�Max�OrientationudHorizontalPosition TabOrder WrapOnClickUpDown2Click  TUpDownUpDown3LeftTTopWidthHeightHint|select height of light sourceMin�MaxPosition TabOrderWrapOnClickUpDown3Click  TEditEdit1Left TopWidth5HeightHint|shows position of light sourceTabOrderTextEdit1   	TGroupBox	GroupBox5Left� Top.Width>Height;AnchorsakLeftakTopakRightakBottom TabOrder 	TPaintBox	PaintBox1LeftTopWidth.HeightHint^|demonstration of normal 3D-plotting; click for ruler function in dead-point side and top viewAnchorsakLeftakTopakRightakBottom OnMouseDownPaintBox1MouseDownOnMouseMovePaintBox1MouseMove	OnMouseUpPaintBox1MouseUpOnPaintPaintBox1Paint  TLabelLabel1LeftTop#Width
HeightAnchorsakLeftakBottom CaptionFr  TLabelLabel2Left5Top#WidthHeightAnchorsakLeftakBottom CaptionOr  TLabelLabel3Left� Top#WidthHeightAnchorsakLeftakBottom CaptionZm  TLabelLabel9Left� Top#WidthHeightAnchorsakLeftakBottom CaptionSy  	TSpinEdit	SpinEdit2LeftTop WidthHeightHint |select frame style, range 0 - 4AnchorsakLeftakBottom MaxValueMinValue TabOrder ValueOnChangeSpinEdit2Change  	TSpinEdit	SpinEdit3LeftBTop Width(HeightHint:|select view orientation (range is limited for simplicity)AnchorsakLeftakBottom 	IncrementMaxValueZMinValue TabOrderValue#OnChangeSpinEdit2Change  	TSpinEdit	SpinEdit4LefteTop Width(HeightHint3|select view tilt (range is limited for simplicity)AnchorsakLeftakBottom 	IncrementMaxValueZMinValue TabOrderValueOnChangeSpinEdit2Change  	TSpinEdit	SpinEdit5Left� Top Width,HeightHint|set zoom factor in %AnchorsakLeftakBottom 	Increment
MaxValue� MinValue2TabOrderValuedOnChangeSpinEdit2Change  	TCheckBox	CheckBox4LeftTop"WidthHeightHint|set perspective on/offAnchorsakLeftakBottom CaptionPTabOrderOnClickCheckBox4Click  	TSpinEdit	SpinEdit6Left� Top WidthHeightHint |select symbol type, range 1 - 4AnchorsakLeftakBottom MaxValueMinValueTabOrderValueOnChangeSpinEdit2Change   TButtonButton1Left�Top Width9HeightHint3|start print and copy  (click animation to restart)CaptionCOPYFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFontTabOrderOnClickButton1Click  	TCheckBox	CheckBox9Left�TopWidth9HeightHint|toggle red/green stereoCaptionStereoTabOrderOnClickCheckBox9Click   	TTabSheet	TabSheet2CaptionCyindrical & Spherical Coord
ImageIndex 	TPaintBox	PaintBox3LeftTop WidthHeightMHintN|demonstration of normal 3D-plotting in cylindrical and spherical co-ordinatesAnchorsakLeftakTopakRightakBottom OnPaintPaintBox3Paint  TLabelLabel10LeftTopWWidthHeightAnchorsakLeftakBottom CaptionFrame  TLabelLabel11LeftMTopWWidth4HeightAnchorsakLeftakBottom CaptionOrientation  TLabelLabel12Left� TopWWidth0HeightAnchorsakLeftakBottom CaptionZoom (%)  	TSpinEdit	SpinEdit9Left+TopTWidthHeightHint |select frame style, range 0 - 4AnchorsakLeftakBottom MaxValueMinValue TabOrder ValueOnChangeSpinEdit9Change  	TSpinEdit
SpinEdit10Left� TopTWidth(HeightHint|select view orientation)AnchorsakLeftakBottom 	IncrementMaxValuehMinValue�TabOrderValue#OnChangeSpinEdit10Change  	TSpinEdit
SpinEdit11Left� TopTWidth(HeightHint|select view tilt AnchorsakLeftakBottom 	IncrementMaxValueZMinValue�TabOrderValueOnChangeSpinEdit9Change  	TSpinEdit
SpinEdit12LeftTopTWidth,HeightHint|set zoom factorAnchorsakLeftakBottom 	Increment
MaxValue� MinValue2TabOrderValuexOnChangeSpinEdit9Change  TButtonButton2Left�TopSWidthAHeightHint|start print and copy AnchorsakLeftakBottom CaptionPrint / CopyTabOrderOnClickButton1Click  	TCheckBox	CheckBox8Left>TopVWidthOHeightHint|set perspective on/offAnchorsakLeftakBottom CaptionPerspectiveChecked	State	cbCheckedTabOrderOnClickCheckBox8Click  	TCheckBox
CheckBox10Left�TopWWidth9HeightHint| toggle red/green stereoAnchorsakLeftakBottom CaptionStereoTabOrderOnClickCheckBox10Click   	TTabSheet	TabSheet3Caption4D Plot
ImageIndex 	TPaintBox	PaintBox4LeftTopWidthHeight/HintL|Demonstration of 4D plot showing two views of the same volume (4D function)AnchorsakLeftakTopakRightakBottom OnPaintPaintBox4Paint  TLabelLabel14Left	TopSWidth4HeightAnchorsakLeftakBottom CaptionOrientation  TLabelLabel16Left� TopSWidthHeightAnchorsakLeftakBottom CaptionStyle  	TSpinEdit
SpinEdit15Left@TopPWidth*HeightHint|select view orientation)AnchorsakLeftakBottom 	IncrementMaxValuehMinValue�TabOrder Value#OnChangeSpinEdit15Change  	TSpinEdit
SpinEdit16LefteTopPWidth(HeightHint|select view tilt AnchorsakLeftakBottom 	IncrementMaxValueZMinValue�TabOrderValueOnChangeSpinEdit16Change  	TSpinEdit
SpinEdit18Left� TopPWidth"HeightHint"|select display style, range 0 - 5AnchorsakLeftakBottom MaxValueMinValue TabOrderValueOnChangeSpinEdit16Change  TRadioGroupElementsLeftTopGWidth� Height!Hint|Select resolution of volumeAnchorsakLeftakBottom CaptionNumber of ElementsColumns	ItemIndex Items.Strings10x10x1020x20x20 TabOrderOnClickSpinEdit16Change  TButtonButton4Left�TopOWidthAHeightHint|start print and copy AnchorsakLeftakBottom CaptionPrint / CopyTabOrderOnClickButton1Click  TRadioGroupRadioGroup2LeftTop�WidthHeight%ColumnsFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold Items.StringsXY4DShowVolumeXY4DSectVolumeXY4DIsoVolume 
ParentFontTabOrderOnClickRadioGroup2Click    TTimerTimer1Interval� OnTimerTimer1TimerLeftPTop@   