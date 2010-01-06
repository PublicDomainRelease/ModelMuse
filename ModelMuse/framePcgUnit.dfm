inherited framePCG: TframePCG
  Width = 451
  Height = 437
  Align = alClient
  Anchors = [akLeft, akTop, akBottom]
  ExplicitWidth = 451
  ExplicitHeight = 304
  DesignSize = (
    451
    437)
  inherited lblComments: TLabel
    Top = 40
    ExplicitTop = 40
  end
  inherited memoComments: TMemo
    Left = 13
    Top = 59
    Width = 386
    Height = 81
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitLeft = 13
    ExplicitTop = 59
    ExplicitWidth = 386
    ExplicitHeight = 81
  end
  object gpPCG: TGridPanel [3]
    Left = 13
    Top = 150
    Width = 435
    Height = 246
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColumnCollection = <
      item
        Value = 54.545454545454540000
      end
      item
        Value = 45.454545454545450000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lblPCGMaxOuter
        Row = 0
      end
      item
        Column = 1
        Control = rdePCGMaxOuter
        Row = 0
      end
      item
        Column = 0
        Control = lblPCGMaxInner
        Row = 1
      end
      item
        Column = 1
        Control = rdePCGMaxInner
        Row = 1
      end
      item
        Column = 0
        Control = lblPCGMethod
        Row = 2
      end
      item
        Column = 1
        Control = comboPCGPrecondMeth
        Row = 2
      end
      item
        Column = 0
        Control = lblPCGMaxChangeHead
        Row = 3
      end
      item
        Column = 1
        Control = rdePCGMaxHeadChange
        Row = 3
      end
      item
        Column = 0
        Control = lblPCGMaxResidual
        Row = 4
      end
      item
        Column = 1
        Control = rdePCGMaxResChange
        Row = 4
      end
      item
        Column = 0
        Control = lblPCGRelaxation
        Row = 5
      end
      item
        Column = 1
        Control = rdePCGRelax
        Row = 5
      end
      item
        Column = 0
        Control = lblPCGMaxEigen
        Row = 6
      end
      item
        Column = 1
        Control = comboPCGEigenValue
        Row = 6
      end
      item
        Column = 0
        Control = lblPCGPrintInterval
        Row = 7
      end
      item
        Column = 1
        Control = rdePCGPrintInt
        Row = 7
      end
      item
        Column = 0
        Control = lblPCGPrintControl
        Row = 8
      end
      item
        Column = 1
        Control = comboPCGPrint
        Row = 8
      end
      item
        Column = 0
        Control = lblPCGDampingFactor
        Row = 9
      end
      item
        Column = 1
        Control = rdePCGDamp
        Row = 9
      end
      item
        Column = 0
        Control = lblPCGDampPcgT
        Row = 10
      end
      item
        Column = 1
        Control = rdePCGDampPcgT
        Row = 10
      end>
    RowCollection = <
      item
        Value = 9.090771673705404000
      end
      item
        Value = 9.090802969843631000
      end
      item
        Value = 9.090837395595685000
      end
      item
        Value = 9.090875263922941000
      end
      item
        Value = 9.090926009541635000
      end
      item
        Value = 9.090975011878168000
      end
      item
        Value = 9.091028914448348000
      end
      item
        Value = 9.091091843449947000
      end
      item
        Value = 9.090885235707040000
      end
      item
        Value = 9.090896590998126000
      end
      item
        Value = 9.090909090909096000
      end>
    TabOrder = 1
    object lblPCGMaxOuter: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 7
      Width = 230
      Height = 13
      Margins.Top = 6
      Align = alClient
      Caption = 'Max. number of outer iterations (MXITER):'
      ExplicitWidth = 205
    end
    object rdePCGMaxOuter: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 4
      Width = 191
      Height = 16
      Hint = 
        'For linear problems, MXITER, should be 1 unless more than 50 inn' +
        'er iterations are required.  In that case MXITER could be as lar' +
        'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
        'but should usually be less than 100.'
      HelpContext = 910
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      Text = '20'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitHeight = 13
    end
    object lblPCGMaxInner: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 29
      Width = 230
      Height = 13
      Margins.Top = 6
      Align = alClient
      Caption = 'Max. number of inner iterations (ITER1):'
      ExplicitTop = 26
      ExplicitWidth = 195
    end
    object rdePCGMaxInner: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 26
      Width = 191
      Height = 16
      Hint = 
        'Usually <=30 for linear problems; usually 3-10 for nonlinear pro' +
        'blems.'
      HelpContext = 920
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
      Text = '30'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 23
      ExplicitHeight = 13
    end
    object lblPCGMethod: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 52
      Width = 230
      Height = 12
      Margins.Top = 7
      Align = alClient
      Caption = 'Matrix preconditioning method (NPCOND):'
      ExplicitTop = 46
      ExplicitWidth = 202
      ExplicitHeight = 13
    end
    object comboPCGPrecondMeth: TJvImageComboBox
      AlignWithMargins = True
      Left = 240
      Top = 48
      Width = 191
      Height = 21
      Hint = 
        'NPCOND = 2 is rarely used because it is generally slower than NP' +
        'COND = 1.'
      HelpContext = 930
      Style = csDropDownList
      Align = alClient
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 265
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 13
      ItemIndex = -1
      TabOrder = 2
      OnChange = comboPCGPrecondMethChange
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Modified incomplete Cholesky (1)'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Polynomial (2)'
        end>
      ExplicitTop = 42
    end
    object lblPCGMaxChangeHead: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 73
      Width = 230
      Height = 13
      Margins.Top = 6
      Align = alClient
      Caption = 'Max. abs. change in head (HCLOSE):'
      ExplicitTop = 64
      ExplicitWidth = 178
    end
    object rdePCGMaxHeadChange: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 70
      Width = 191
      Height = 16
      Hint = 
        'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
        'imum change in head between two iterations is less than HCLOSE, ' +
        'the program will check the other criterion (RCLOSE).  If both cr' +
        'iteria are met, the program will go on to the next outer iterati' +
        'on.'
      HelpContext = 940
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
      Text = '0.001'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 61
      ExplicitHeight = 13
    end
    object lblPCGMaxResidual: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 95
      Width = 230
      Height = 13
      Margins.Top = 6
      Align = alClient
      Caption = 'Max. abs. residual (RCLOSE):'
      ExplicitTop = 83
      ExplicitWidth = 142
    end
    object rdePCGMaxResChange: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 92
      Width = 191
      Height = 16
      Hint = 
        'RCLOSE is one of two convergence criteria in PCG2.  When the max' +
        'imum absolute flow residual is less than RCLOSE, the program wil' +
        'l check HCLOSE{linkID=940}.  If both criteria are met, the progr' +
        'am will go on to the next outer iteration.'
      HelpContext = 950
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 4
      Text = '1000'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 80
      ExplicitHeight = 13
    end
    object lblPCGRelaxation: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 117
      Width = 230
      Height = 13
      Margins.Top = 6
      Align = alClient
      Caption = 'Relaxation parameter (RELAX):'
      ExplicitTop = 102
      ExplicitWidth = 150
    end
    object rdePCGRelax: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 114
      Width = 191
      Height = 16
      Hint = 
        'The relaxation input value is usually set to 1.  However, if you' +
        ' are using the rewetting capability in the Block-Centered Flow a' +
        'nd Layer Property Flow Packages, you may wish to set it to 0.97 ' +
        'to 0.99 because this may prevent zero divide and non-diagonally ' +
        'dominant matrix errors.'
      HelpContext = 960
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 5
      Text = '1'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 99
      ExplicitHeight = 13
    end
    object lblPCGMaxEigen: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 140
      Width = 230
      Height = 12
      Margins.Top = 7
      Align = alClient
      Caption = 'Upper bound of the max. eigenvalue (NBPOL):'
      ExplicitTop = 122
      ExplicitWidth = 223
      ExplicitHeight = 13
    end
    object comboPCGEigenValue: TJvImageComboBox
      AlignWithMargins = True
      Left = 240
      Top = 136
      Width = 191
      Height = 21
      Hint = 
        'In many cases you can speed up execution time slightly by settin' +
        'g NBPOL=2.  The estimated value is usually close to 2 and the nu' +
        'mber of iterations required is relatively insensitive to the exa' +
        'ct value of the estimate.'
      HelpContext = 970
      Style = csDropDownList
      Align = alClient
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 265
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 13
      ItemIndex = -1
      TabOrder = 6
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Calculated (1)'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Two (2)'
        end>
      ExplicitTop = 118
    end
    object lblPCGPrintInterval: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 161
      Width = 230
      Height = 13
      Margins.Top = 6
      Align = alClient
      Caption = 'Printout interval (IPRPCG):'
      ExplicitTop = 140
      ExplicitWidth = 129
    end
    object rdePCGPrintInt: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 158
      Width = 191
      Height = 16
      Hint = 
        'Information is printed for each iteration of a time step wheneve' +
        'r the time step is an even multiple of IPRPCG.  The printout als' +
        'o is generated at the end of each stress period.'
      HelpContext = 980
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 7
      Text = '1'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 137
      ExplicitHeight = 13
    end
    object lblPCGPrintControl: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 184
      Width = 230
      Height = 12
      Margins.Top = 7
      Align = alClient
      Caption = 'Printing control (MUTPCG):'
      ExplicitTop = 160
      ExplicitWidth = 128
      ExplicitHeight = 13
    end
    object comboPCGPrint: TJvImageComboBox
      AlignWithMargins = True
      Left = 240
      Top = 180
      Width = 191
      Height = 21
      Hint = 'MUTPCG controls the information that is to be printed.'
      HelpContext = 990
      Style = csDropDownList
      Align = alClient
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 265
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 13
      ItemIndex = -1
      TabOrder = 8
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Solver information (0)'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Iteration only (1)'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Suppress printing (2)'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Print only if convergence fails (3)'
        end>
      ExplicitTop = 156
    end
    object lblPCGDampingFactor: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 205
      Width = 230
      Height = 13
      Margins.Top = 6
      Align = alClient
      Caption = 'Damping factor (DAMPPCG):'
      ExplicitTop = 178
      ExplicitWidth = 136
    end
    object rdePCGDamp: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 202
      Width = 191
      Height = 16
      Hint = 'Damping factor for reducing oscillation.'
      HelpContext = 1000
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 9
      Text = '1'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMax = True
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 175
      ExplicitHeight = 13
    end
    object lblPCGDampPcgT: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 227
      Width = 230
      Height = 15
      Margins.Top = 6
      Align = alClient
      Caption = 'Transient damping factor (DAMPPCGT):'
      ExplicitTop = 197
      ExplicitWidth = 189
      ExplicitHeight = 13
    end
    object rdePCGDampPcgT: TRbwDataEntry
      AlignWithMargins = True
      Left = 240
      Top = 224
      Width = 191
      Height = 18
      Hint = 'Damping factor for reducing oscillation.'
      HelpContext = 1000
      Align = alClient
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 10
      Text = '1'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMax = True
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 194
      ExplicitHeight = 20
    end
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
        Control = rdePCGMaxOuter
      end
      item
        Control = rdePCGMaxInner
      end
      item
        Control = comboPCGPrecondMeth
      end
      item
        Control = rdePCGMaxHeadChange
      end
      item
        Control = rdePCGMaxResChange
      end
      item
        Control = rdePCGPrintInt
      end
      item
        Control = comboPCGPrint
      end
      item
        Control = rdePCGDamp
      end
      item
        Control = rdePCGDampPcgT
      end>
  end
end
