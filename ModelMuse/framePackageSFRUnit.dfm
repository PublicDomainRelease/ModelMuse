inherited framePackageSFR: TframePackageSFR
  Width = 614
  Height = 445
  ExplicitWidth = 614
  ExplicitHeight = 445
  DesignSize = (
    614
    445)
  object lblPrintStreams: TLabel [2]
    Left = 176
    Top = 144
    Width = 109
    Height = 13
    Caption = 'Print streams (ISTCB2)'
  end
  object lblStreamTolerance: TLabel [3]
    AlignWithMargins = True
    Left = 122
    Top = 246
    Width = 129
    Height = 13
    Caption = 'Tolerance (L^3/T) (DLEAK)'
  end
  object lblSfrTrailingWaveIncrements: TLabel [4]
    AlignWithMargins = True
    Left = 122
    Top = 270
    Width = 222
    Height = 13
    Caption = 'Number of trailing wave increments (NSTRAIL)'
  end
  object lblSfrMaxTrailingWaves: TLabel [5]
    AlignWithMargins = True
    Left = 122
    Top = 293
    Width = 226
    Height = 13
    Caption = 'Maximum number of trailing waves (NSFRSETS)'
  end
  object lblSfrMaxUnsatCells: TLabel [6]
    AlignWithMargins = True
    Left = 122
    Top = 316
    Width = 297
    Height = 13
    Caption = 'Maximum number of cells to define unsaturated zone (ISUZN) '
  end
  object lblNUMTIM: TLabel [7]
    AlignWithMargins = True
    Left = 122
    Top = 354
    Width = 311
    Height = 13
    Caption = 'Number of divisions per time step for kinematic waves (NUMTIM) '
  end
  object lblWeight: TLabel [8]
    AlignWithMargins = True
    Left = 122
    Top = 376
    Width = 309
    Height = 13
    Caption = 'Time weighting factor for the kinematic wave solution (WEIGHT) '
  end
  object lblFLWTOL: TLabel [9]
    AlignWithMargins = True
    Left = 122
    Top = 398
    Width = 284
    Height = 13
    Caption = 'Closure criterion for the kinematic wave solution (FLWTOL) '
  end
  inherited memoComments: TMemo
    Width = 583
    Height = 51
    ExplicitWidth = 583
    ExplicitHeight = 51
  end
  object cbSfrUnsatflow: TCheckBox95 [11]
    Left = 16
    Top = 117
    Width = 241
    Height = 23
    Hint = 
      'Model flow from the stream through the unsaturated zone to the w' +
      'ater table.'
    HelpContext = 621
    Alignment = taLeftJustify
    Caption = 'Unsaturated Flow (ISFROPT)'
    Enabled = False
    TabOrder = 1
    WordWrap = False
    OnClick = cbSfrUnsatflowClick
    AlignmentBtn = taLeftJustify
    LikePushButton = False
    VerticalAlignment = vaTop
  end
  object cbSfrLpfHydraulicCond: TCheckBox95 [12]
    Left = 263
    Top = 117
    Width = 330
    Height = 23
    Hint = 
      'Hydraulic conductivity in unsaturated zone comes from LPF packag' +
      'e'
    HelpContext = 622
    Alignment = taLeftJustify
    Caption = 'LPF hydraulic conductivites used (ISFROPT) '
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 2
    WordWrap = False
    AlignmentBtn = taLeftJustify
    LikePushButton = False
    VerticalAlignment = vaTop
  end
  object rgSfr2ISFROPT: TRadioGroup [13]
    Left = 16
    Top = 170
    Width = 583
    Height = 70
    Caption = 'Streambed properties (ISFROPT)'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Specify some streambed properties using segment endpoints'
      
        'Specify some streambed properties by reach (can'#39't inactivate str' +
        'eams)')
    TabOrder = 3
  end
  object comboPrintStreams: TComboBox [14]
    Left = 16
    Top = 143
    Width = 145
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Don'#39't print flows'
      'Print flows in listing file'
      'Print flows in .sfr_out file')
  end
  object cbGage8: TCheckBox [15]
    Left = 16
    Top = 420
    Width = 415
    Height = 17
    Caption = 'Gage overall stream budget (OUTTYPE = 8)'
    Enabled = False
    TabOrder = 5
    OnClick = cbIRTFLGClick
  end
  object rdeDLEAK: TRbwDataEntry [16]
    Left = 16
    Top = 246
    Width = 100
    Height = 18
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 6
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeNstrail: TRbwDataEntry [17]
    Left = 16
    Top = 270
    Width = 100
    Height = 17
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 7
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeNsfrsets: TRbwDataEntry [18]
    Left = 16
    Top = 293
    Width = 100
    Height = 17
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 8
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeIsuzn: TRbwDataEntry [19]
    Left = 16
    Top = 316
    Width = 100
    Height = 17
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 9
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbIRTFLG: TCheckBox [20]
    Left = 16
    Top = 331
    Width = 425
    Height = 17
    Caption = 
      'Use transient streamflow routing with kinematic-wave equation (I' +
      'RTFLG)'
    Enabled = False
    TabOrder = 10
    OnClick = cbIRTFLGClick
  end
  object rdeNUMTIM: TRbwDataEntry [21]
    Left = 16
    Top = 354
    Width = 100
    Height = 16
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 11
    Text = '1'
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeWeight: TRbwDataEntry [22]
    Left = 16
    Top = 376
    Width = 100
    Height = 16
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 12
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    Min = 0.500000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeFLWTOL: TRbwDataEntry [23]
    Left = 16
    Top = 398
    Width = 100
    Height = 16
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 13
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = rgSfr2ISFROPT
      end
      item
        Control = cbSfrUnsatflow
      end
      item
        Control = comboPrintStreams
      end
      item
        Control = rgSfr2ISFROPT
      end
      item
        Control = cbIRTFLG
      end
      item
        Control = cbGage8
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
