inherited frmModflowPackages: TfrmModflowPackages
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Packages_Dialog_Box'
  Caption = 'MODFLOW Packages and Programs'
  ClientHeight = 544
  ClientWidth = 792
  OnDestroy = FormDestroy
  OnResize = FormResize
  ExplicitTop = -8
  ExplicitWidth = 800
  ExplicitHeight = 578
  PixelsPerInch = 96
  TextHeight = 18
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 177
    Top = 0
    Height = 503
    Align = alLeft
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 121
    ExplicitTop = -14
    ExplicitHeight = 252
  end
  object tvPackages: TTreeView
    Left = 0
    Top = 0
    Width = 177
    Height = 503
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    StateImages = ilCheckImages
    TabOrder = 0
    OnChange = tvPackagesChange
    OnMouseUp = tvPackagesMouseUp
  end
  object jvplPackages: TJvPageList
    Left = 187
    Top = 0
    Width = 605
    Height = 503
    ActivePage = jvspUZF
    PropagateEnable = False
    Align = alClient
    OnChange = jvplPackagesChange
    object jvspLPF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'LPF_Layer_Property_Flow_Package'
      Caption = 'LPF (Layer Property Flow)'
      object JvNetscapeSplitter2: TJvNetscapeSplitter
        Left = 121
        Top = 291
        Height = 212
        Align = alLeft
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 256
        ExplicitTop = 96
        ExplicitHeight = 100
      end
      object JvNetscapeSplitter3: TJvNetscapeSplitter
        Left = 0
        Top = 281
        Width = 605
        Height = 10
        Cursor = crVSplit
        Align = alTop
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 131
        ExplicitWidth = 222
      end
      inline frameLpfParameterDefinition: TframeArrayParameterDefinition
        Left = 131
        Top = 291
        Width = 474
        Height = 212
        Align = alClient
        Enabled = False
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 131
        ExplicitTop = 291
        ExplicitWidth = 474
        ExplicitHeight = 212
        inherited pnlParameterCount: TPanel
          Top = 164
          Width = 474
          ExplicitTop = 164
          ExplicitWidth = 474
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 380
            Top = 6
            Enabled = True
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 380
            ExplicitTop = 6
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            Enabled = True
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 468
          Height = 101
          Enabled = True
          Columns = <
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 10
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Boolean
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Boolean
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = True
            end>
          ExplicitLeft = 3
          ExplicitTop = 60
          ExplicitWidth = 468
          ExplicitHeight = 101
        end
        inherited pnlTop: TPanel
          Width = 474
          ExplicitWidth = 474
          inherited lblParamValue: TLabel
            Width = 102
            Height = 18
            ExplicitWidth = 102
            ExplicitHeight = 18
          end
          inherited rdeParamValue: TRbwDataEntry
            ItemHeight = 18
          end
        end
      end
      object tvLpfParameterTypes: TTreeView
        Left = 0
        Top = 291
        Width = 121
        Height = 212
        Align = alLeft
        Enabled = False
        HideSelection = False
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        OnChange = tvLpfParameterTypesChange
      end
      inline framePkgLPF: TframePackageLpf
        Left = 0
        Top = 0
        Width = 605
        Height = 281
        Align = alTop
        TabOrder = 2
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 281
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rdgOptions: TRbwDataGrid4
          Width = 577
          ExplicitWidth = 577
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgLPF.lblComments
            end
            item
              Control = framePkgLPF.memoComments
            end
            item
              Control = framePkgLPF.rdgOptions
            end
            item
              Control = frameLpfParameterDefinition
            end
            item
              Control = tvLpfParameterTypes
            end>
        end
      end
    end
    object jvspHUF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'HUF2_Hydrogeologic_Unit_Flow'
      Caption = 'jvspHUF'
      object JvNetscapeSplitter4: TJvNetscapeSplitter
        Left = 0
        Top = 275
        Width = 605
        Height = 10
        Cursor = crVSplit
        Align = alTop
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitTop = 204
      end
      object JvNetscapeSplitter5: TJvNetscapeSplitter
        Left = 121
        Top = 285
        Height = 218
        Align = alLeft
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 256
        ExplicitTop = 96
        ExplicitHeight = 100
      end
      inline framePkgHuf: TframePackageHuf
        Left = 0
        Top = 0
        Width = 605
        Height = 275
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        DesignSize = (
          605
          275)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited cbSaveHeads: TCheckBox
          Width = 409
          ExplicitWidth = 409
        end
        inherited cbSaveFlows: TCheckBox
          Width = 385
          ExplicitWidth = 385
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHuf.lblComments
            end
            item
              Control = framePkgHuf.memoComments
            end
            item
              Control = framePkgHuf.cbSaveHeads
            end
            item
              Control = framePkgHuf.cbSaveFlows
            end
            item
              Control = frameHufParameterDefinition
            end
            item
              Control = tvHufParameterTypes
            end>
          OnEnabledChange = framePkgHufrcSelectionControllerEnabledChange
        end
      end
      object tvHufParameterTypes: TTreeView
        Left = 0
        Top = 285
        Width = 121
        Height = 218
        Align = alLeft
        Enabled = False
        HideSelection = False
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        OnChange = tvHufParameterTypesChange
      end
      inline frameHufParameterDefinition: TframeListParameterDefinition
        Left = 131
        Top = 285
        Width = 474
        Height = 218
        Align = alClient
        Enabled = False
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 131
        ExplicitTop = 285
        ExplicitWidth = 474
        ExplicitHeight = 218
        inherited pnlParameterCount: TPanel
          Top = 170
          Width = 474
          ExplicitTop = 170
          ExplicitWidth = 474
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 386
            Enabled = True
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 386
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 468
          Height = 164
          Enabled = True
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 468
          ExplicitHeight = 164
        end
      end
    end
    object jvspCHD: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'CHD_Time_Variant_Specified_Head'
      Caption = 'CHD (Time-Variant Specified-Head Package)'
      inline framePkgCHD: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgCHD.lblComments
            end
            item
              Control = framePkgCHD.memoComments
            end
            item
              Control = frameChdParameterDefinition
            end>
        end
      end
      inline frameChdParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 605
        Height = 342
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 605
        ExplicitHeight = 342
        inherited pnlParameterCount: TPanel
          Top = 294
          Width = 605
          ExplicitTop = 294
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 288
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 288
        end
      end
    end
    object jvspGHB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'GHB_General_Head_Boundary_Package'
      Caption = 'GHB (General Head Boundary)'
      inline framePkgGHB: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgGHB.lblComments
            end
            item
              Control = framePkgGHB.memoComments
            end
            item
              Control = frameGhbParameterDefinition
            end>
        end
      end
      inline frameGhbParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 605
        Height = 342
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 605
        ExplicitHeight = 342
        inherited pnlParameterCount: TPanel
          Top = 294
          Width = 605
          ExplicitTop = 294
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 288
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 288
        end
      end
    end
    object jvspPCG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'PCG_Preconditioned_Conjugate_Gradiant'
      Caption = 'PCG (Preconditioned Conjugate-Gradient)'
      inline framePCG: TframePCG
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        DesignSize = (
          605
          503)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 132
          ExplicitWidth = 584
          ExplicitHeight = 132
        end
        inherited gpPCG: TGridPanel
          Top = 202
          Width = 589
          Height = 301
          Anchors = [akLeft, akRight, akBottom]
          BevelOuter = bvNone
          ControlCollection = <
            item
              Column = 0
              Control = framePCG.lblPCGMaxOuter
              Row = 0
            end
            item
              Column = 1
              Control = framePCG.rdePCGMaxOuter
              Row = 0
            end
            item
              Column = 0
              Control = framePCG.lblPCGMaxInner
              Row = 1
            end
            item
              Column = 1
              Control = framePCG.rdePCGMaxInner
              Row = 1
            end
            item
              Column = 0
              Control = framePCG.lblPCGMethod
              Row = 2
            end
            item
              Column = 1
              Control = framePCG.comboPCGPrecondMeth
              Row = 2
            end
            item
              Column = 0
              Control = framePCG.lblPCGMaxChangeHead
              Row = 3
            end
            item
              Column = 1
              Control = framePCG.rdePCGMaxHeadChange
              Row = 3
            end
            item
              Column = 0
              Control = framePCG.lblPCGMaxResidual
              Row = 4
            end
            item
              Column = 1
              Control = framePCG.rdePCGMaxResChange
              Row = 4
            end
            item
              Column = 0
              Control = framePCG.lblPCGRelaxation
              Row = 5
            end
            item
              Column = 1
              Control = framePCG.rdePCGRelax
              Row = 5
            end
            item
              Column = 0
              Control = framePCG.lblPCGMaxEigen
              Row = 6
            end
            item
              Column = 1
              Control = framePCG.comboPCGEigenValue
              Row = 6
            end
            item
              Column = 0
              Control = framePCG.lblPCGPrintInterval
              Row = 7
            end
            item
              Column = 1
              Control = framePCG.rdePCGPrintInt
              Row = 7
            end
            item
              Column = 0
              Control = framePCG.lblPCGPrintControl
              Row = 8
            end
            item
              Column = 1
              Control = framePCG.comboPCGPrint
              Row = 8
            end
            item
              Column = 0
              Control = framePCG.lblPCGDampingFactor
              Row = 9
            end
            item
              Column = 1
              Control = framePCG.rdePCGDamp
              Row = 9
            end
            item
              Column = 0
              Control = framePCG.lblPCGDampPcgT
              Row = 10
            end
            item
              Column = 1
              Control = framePCG.rdePCGDampPcgT
              Row = 10
            end>
          RowCollection = <
            item
              Value = 9.090596132233804000
            end
            item
              Value = 9.090596132233804000
            end
            item
              Value = 9.090596132233804000
            end
            item
              Value = 9.090505226272480000
            end
            item
              Value = 9.090573405743472000
            end
            item
              Value = 9.090573405743474000
            end
            item
              Value = 9.090537043449851000
            end
            item
              Value = 9.093295381593441000
            end
            item
              Value = 9.090909107438018000
            end
            item
              Value = 9.090909016528926000
            end
            item
              Value = 9.090909016528926000
            end>
          ExplicitTop = 202
          ExplicitWidth = 589
          ExplicitHeight = 301
          inherited lblPCGMaxOuter: TLabel
            Left = 3
            Top = 6
            Width = 315
            Height = 21
            ExplicitLeft = 3
            ExplicitTop = 6
            ExplicitWidth = 289
            ExplicitHeight = 18
          end
          inherited rdePCGMaxOuter: TRbwDataEntry
            Left = 324
            Top = 3
            Width = 262
            Height = 21
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 3
            ExplicitWidth = 262
            ExplicitHeight = 21
          end
          inherited lblPCGMaxInner: TLabel
            Left = 3
            Top = 33
            Width = 315
            Height = 21
            ExplicitLeft = 3
            ExplicitTop = 33
            ExplicitWidth = 273
            ExplicitHeight = 18
          end
          inherited rdePCGMaxInner: TRbwDataEntry
            Left = 324
            Top = 30
            Width = 262
            Height = 21
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 30
            ExplicitWidth = 262
            ExplicitHeight = 21
          end
          inherited lblPCGMethod: TLabel
            Left = 3
            Top = 61
            Width = 315
            Height = 20
            ExplicitLeft = 3
            ExplicitTop = 61
            ExplicitWidth = 296
            ExplicitHeight = 18
          end
          inherited comboPCGPrecondMeth: TJvImageComboBox
            Left = 324
            Top = 57
            Width = 262
            Height = 26
            DroppedWidth = 266
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 57
            ExplicitWidth = 262
            ExplicitHeight = 26
          end
          inherited lblPCGMaxChangeHead: TLabel
            Left = 3
            Top = 87
            Width = 315
            Height = 21
            ExplicitLeft = 3
            ExplicitTop = 87
            ExplicitWidth = 261
            ExplicitHeight = 18
          end
          inherited rdePCGMaxHeadChange: TRbwDataEntry
            Left = 324
            Top = 84
            Width = 262
            Height = 21
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 84
            ExplicitWidth = 262
            ExplicitHeight = 21
          end
          inherited lblPCGMaxResidual: TLabel
            Left = 3
            Top = 114
            Width = 315
            Height = 21
            ExplicitLeft = 3
            ExplicitTop = 114
            ExplicitWidth = 210
            ExplicitHeight = 18
          end
          inherited rdePCGMaxResChange: TRbwDataEntry
            Left = 324
            Top = 111
            Width = 262
            Height = 21
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 111
            ExplicitWidth = 262
            ExplicitHeight = 21
          end
          inherited lblPCGRelaxation: TLabel
            Left = 3
            Top = 141
            Width = 315
            Height = 21
            ExplicitLeft = 3
            ExplicitTop = 141
            ExplicitWidth = 220
            ExplicitHeight = 18
          end
          inherited rdePCGRelax: TRbwDataEntry
            Left = 324
            Top = 138
            Width = 262
            Height = 21
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 138
            ExplicitWidth = 262
            ExplicitHeight = 21
          end
          inherited lblPCGMaxEigen: TLabel
            Left = 3
            Top = 169
            Width = 315
            Height = 20
            ExplicitLeft = 3
            ExplicitTop = 169
            ExplicitWidth = 320
            ExplicitHeight = 18
          end
          inherited comboPCGEigenValue: TJvImageComboBox
            Left = 324
            Top = 165
            Width = 262
            Height = 26
            DroppedWidth = 266
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 165
            ExplicitWidth = 262
            ExplicitHeight = 26
          end
          inherited lblPCGPrintInterval: TLabel
            Left = 3
            Top = 195
            Width = 315
            Height = 21
            ExplicitLeft = 3
            ExplicitTop = 195
            ExplicitWidth = 184
            ExplicitHeight = 18
          end
          inherited rdePCGPrintInt: TRbwDataEntry
            Left = 324
            Top = 192
            Width = 262
            Height = 21
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 192
            ExplicitWidth = 262
            ExplicitHeight = 21
          end
          inherited lblPCGPrintControl: TLabel
            Left = 3
            Top = 223
            Width = 315
            Height = 20
            ExplicitLeft = 3
            ExplicitTop = 223
            ExplicitWidth = 189
            ExplicitHeight = 18
          end
          inherited comboPCGPrint: TJvImageComboBox
            Left = 324
            Top = 219
            Width = 262
            Height = 26
            DroppedWidth = 266
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 219
            ExplicitWidth = 262
            ExplicitHeight = 26
          end
          inherited lblPCGDampingFactor: TLabel
            Left = 3
            Top = 249
            Width = 315
            Height = 21
            Caption = 'Steady-state damping factor (DAMPPCG):'
            ExplicitLeft = 3
            ExplicitTop = 249
            ExplicitWidth = 296
            ExplicitHeight = 18
          end
          inherited rdePCGDamp: TRbwDataEntry
            Left = 324
            Top = 246
            Width = 262
            Height = 21
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 246
            ExplicitWidth = 262
            ExplicitHeight = 21
          end
          inherited lblPCGDampPcgT: TLabel
            Left = 3
            Top = 276
            Width = 315
            Height = 25
            ExplicitLeft = 3
            ExplicitTop = 276
            ExplicitWidth = 281
            ExplicitHeight = 18
          end
          inherited rdePCGDampPcgT: TRbwDataEntry
            Left = 324
            Top = 273
            Width = 262
            Height = 25
            ItemHeight = 18
            ExplicitLeft = 324
            ExplicitTop = 273
            ExplicitWidth = 262
            ExplicitHeight = 25
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePCG.lblComments
            end
            item
              Control = framePCG.memoComments
            end
            item
              Control = framePCG.comboPCGPrecondMeth
            end
            item
              Control = framePCG.comboPCGPrint
            end
            item
              Control = framePCG.rdePCGDamp
            end
            item
              Control = framePCG.rdePCGMaxHeadChange
            end
            item
              Control = framePCG.rdePCGMaxInner
            end
            item
              Control = framePCG.rdePCGMaxOuter
            end
            item
              Control = framePCG.rdePCGMaxResChange
            end
            item
              Control = framePCG.rdePCGPrintInt
            end
            item
              Control = framePCG.rdePCGDampPcgT
            end>
        end
      end
    end
    object jvspWEL: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'WEL_Well_Package_Pane'
      Caption = 'jvspWEL'
      inline framePkgWEL: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgWEL.lblComments
            end
            item
              Control = framePkgWEL.memoComments
            end
            item
              Control = frameWelParameterDefinition
            end>
        end
      end
      inline frameWelParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 605
        Height = 342
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 605
        ExplicitHeight = 342
        inherited pnlParameterCount: TPanel
          Top = 294
          Width = 605
          ExplicitTop = 294
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 288
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 288
        end
      end
    end
    object jvspRIV: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'RIV_River_Package'
      Caption = 'jvspRIV'
      inline framePkgRIV: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRIV.lblComments
            end
            item
              Control = framePkgRIV.memoComments
            end
            item
              Control = frameRivParameterDefinition
            end>
        end
      end
      inline frameRivParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 605
        Height = 342
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 605
        ExplicitHeight = 342
        inherited pnlParameterCount: TPanel
          Top = 294
          Width = 605
          ExplicitTop = 294
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 288
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 288
        end
      end
    end
    object jvspDRN: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'DRN_Drain_Package_Pane'
      Caption = 'jvspDRN'
      inline framePkgDRN: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDRN.lblComments
            end
            item
              Control = framePkgDRN.memoComments
            end
            item
              Control = frameDrnParameterDefinition
            end>
        end
      end
      inline frameDrnParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 605
        Height = 342
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 605
        ExplicitHeight = 342
        inherited pnlParameterCount: TPanel
          Top = 294
          Width = 605
          ExplicitTop = 294
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 288
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 288
        end
      end
    end
    object jvspDRT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'DRT_Drain_Return_Package_Pane'
      Caption = 'jvspDRT'
      inline framePkgDRT: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDRT.lblComments
            end
            item
              Control = framePkgDRT.memoComments
            end
            item
              Control = frameDrtParameterDefinition
            end>
        end
      end
      inline frameDrtParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 605
        Height = 342
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 605
        ExplicitHeight = 342
        inherited pnlParameterCount: TPanel
          Top = 294
          Width = 605
          ExplicitTop = 294
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 288
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 288
        end
      end
    end
    object jvspRCH: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'RCH_Recharge_Package_Pane'
      Caption = 'jvspRCH'
      inline framePkgRCH: TframePackageTransientLayerChoice
        Left = 0
        Top = 0
        Width = 605
        Height = 201
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          ExplicitWidth = 584
        end
        inherited pnLayerOption: TPanel
          Width = 605
          ExplicitWidth = 605
          inherited lblLayerOption: TLabel
            Width = 173
            Height = 18
            Caption = 'Recharge location option'
            ExplicitWidth = 173
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ItemHeight = 18
            ExplicitHeight = 26
          end
          inherited cbTimeVaryingLayers: TCheckBox
            Caption = 'Time varying recharge layers'
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRCH.lblComments
            end
            item
              Control = framePkgRCH.memoComments
            end
            item
              Control = framePkgRCH.cbTimeVaryingLayers
            end
            item
              Control = framePkgRCH.comboLayerOption
            end
            item
              Control = framePkgRCH.lblLayerOption
            end
            item
              Control = frameRchParameterDefinition
            end>
          OnEnabledChange = framePkgRCHrcSelectionControllerEnabledChange
        end
      end
      inline frameRchParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 201
        Width = 605
        Height = 302
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 201
        ExplicitWidth = 605
        ExplicitHeight = 302
        inherited pnlParameterCount: TPanel
          Top = 254
          Width = 605
          ExplicitTop = 254
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 248
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 248
        end
      end
    end
    object jvspEVT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'EVT_Evapotranspiration_Package'
      Caption = 'jvspEVT'
      inline framePkgEVT: TframePackageTransientLayerChoice
        Left = 0
        Top = 0
        Width = 605
        Height = 201
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          ExplicitWidth = 584
        end
        inherited pnLayerOption: TPanel
          Width = 605
          ExplicitWidth = 605
          inherited lblLayerOption: TLabel
            Width = 136
            Height = 18
            Caption = 'EVT location option'
            ExplicitWidth = 136
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ItemHeight = 18
            ExplicitHeight = 26
          end
          inherited cbTimeVaryingLayers: TCheckBox
            Caption = 'Time varying EVT layers'
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgEVT.lblComments
            end
            item
              Control = framePkgEVT.memoComments
            end
            item
              Control = framePkgEVT.cbTimeVaryingLayers
            end
            item
              Control = framePkgEVT.comboLayerOption
            end
            item
              Control = framePkgEVT.lblLayerOption
            end
            item
              Control = frameEvtParameterDefinition
            end>
          OnEnabledChange = framePkgEVTrcSelectionControllerEnabledChange
        end
      end
      inline frameEvtParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 201
        Width = 605
        Height = 302
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 201
        ExplicitWidth = 605
        ExplicitHeight = 302
        inherited pnlParameterCount: TPanel
          Top = 254
          Width = 605
          ExplicitTop = 254
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 248
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 248
        end
      end
    end
    object jvspETS: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'ETS_Evapotranspiration_Segments_Package'
      Caption = 'jvspETS'
      inline framePkgETS: TframeEtsPackage
        Left = 0
        Top = 0
        Width = 605
        Height = 226
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          ExplicitWidth = 584
        end
        inherited pnLayerOption: TPanel
          Width = 605
          ExplicitWidth = 605
          inherited lblLayerOption: TLabel
            Width = 136
            Height = 18
            Caption = 'ETS location option'
            ExplicitWidth = 136
            ExplicitHeight = 18
          end
          inherited lblSegments: TLabel
            Width = 144
            Height = 18
            ExplicitWidth = 144
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ItemHeight = 18
            ExplicitHeight = 26
          end
          inherited cbTimeVaryingLayers: TCheckBox
            Caption = 'Time varying ETS layers'
          end
          inherited seSegments: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgETS.lblComments
            end
            item
              Control = framePkgETS.memoComments
            end
            item
              Control = framePkgETS.cbTimeVaryingLayers
            end
            item
              Control = framePkgETS.comboLayerOption
            end
            item
              Control = framePkgETS.lblLayerOption
            end
            item
              Control = framePkgETS.seSegments
            end
            item
              Control = framePkgETS.lblSegments
            end
            item
              Control = frameEtsParameterDefinition
            end>
        end
      end
      inline frameEtsParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 226
        Width = 605
        Height = 277
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 226
        ExplicitWidth = 605
        ExplicitHeight = 277
        inherited pnlParameterCount: TPanel
          Top = 229
          Width = 605
          ExplicitTop = 229
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 223
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 223
        end
      end
    end
    object jvspRES: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'RES_Reservoir_Package_Pane'
      Caption = 'jvspRES'
      inline framePkgRES: TframePackageRes
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 356
          ExplicitWidth = 584
          ExplicitHeight = 356
        end
        inherited pnLayerOption: TPanel
          Top = 427
          Width = 605
          ExplicitTop = 427
          ExplicitWidth = 605
          inherited lblLayerOption: TLabel
            Top = 5
            Width = 172
            Height = 18
            ExplicitTop = 5
            ExplicitWidth = 172
            ExplicitHeight = 18
          end
          inherited lblTableSize: TLabel
            Width = 469
            Height = 18
            ExplicitWidth = 469
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ItemHeight = 18
            ExplicitHeight = 26
          end
          inherited cbPrintStage: TCheckBox
            Width = 561
            ExplicitWidth = 561
          end
          inherited seTableSize: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRES.lblComments
            end
            item
              Control = framePkgRES.memoComments
            end
            item
              Control = framePkgRES.cbTimeVaryingLayers
            end
            item
              Control = framePkgRES.comboLayerOption
            end
            item
              Control = framePkgRES.lblLayerOption
            end
            item
              Control = framePkgRES.cbPrintStage
            end
            item
              Control = framePkgRES.seTableSize
            end
            item
              Control = framePkgRES.lblTableSize
            end>
        end
      end
    end
    object jvspLAK: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'LAK_Lake_Package_Pane'
      Caption = 'jvspLAK'
      inline framePkgLAK: TframePackageLAK
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblTheta: TLabel
          Left = 168
          Top = 373
          Width = 39
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 373
          ExplicitWidth = 39
          ExplicitHeight = 18
        end
        inherited lblIterations: TLabel
          Left = 168
          Top = 401
          Width = 286
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 401
          ExplicitWidth = 286
          ExplicitHeight = 18
        end
        inherited lblConvergenceCriterion: TLabel
          Left = 168
          Top = 429
          Width = 235
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 429
          ExplicitWidth = 235
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblSurfDepth: TLabel
          Left = 168
          Top = 453
          Width = 340
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 453
          ExplicitWidth = 340
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 302
          ExplicitWidth = 584
          ExplicitHeight = 302
        end
        inherited rdeTheta: TRbwDataEntry
          Top = 370
          Width = 146
          ItemHeight = 18
          ExplicitTop = 370
          ExplicitWidth = 146
        end
        inherited rdeIterations: TRbwDataEntry
          Top = 398
          Width = 146
          ItemHeight = 18
          ExplicitTop = 398
          ExplicitWidth = 146
        end
        inherited rdeConvergenceCriterion: TRbwDataEntry
          Top = 426
          Width = 146
          ItemHeight = 18
          ExplicitTop = 426
          ExplicitWidth = 146
        end
        inherited cbPrintLake: TCheckBox
          Top = 478
          ExplicitTop = 478
        end
        inherited rdeSurfDepth: TRbwDataEntry
          Top = 450
          Width = 146
          ItemHeight = 18
          ExplicitTop = 450
          ExplicitWidth = 146
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgLAK.lblComments
            end
            item
              Control = framePkgLAK.memoComments
            end
            item
              Control = framePkgLAK.cbPrintLake
            end
            item
              Control = framePkgLAK.rdeIterations
            end
            item
              Control = framePkgLAK.rdeConvergenceCriterion
            end
            item
              Control = framePkgLAK.rdeTheta
            end
            item
              Control = framePkgLAK.lblConvergenceCriterion
            end
            item
              Control = framePkgLAK.lblIterations
            end
            item
              Control = framePkgLAK.lblTheta
            end
            item
              Control = framePkgLAK.rdeSurfDepth
            end
            item
              Control = framePkgLAK.lblSurfDepth
            end>
        end
      end
    end
    object jvspSFR: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'SFR_Stream_Flow_Routing_Package'
      Caption = 'jvspSFR'
      object pcSFR: TJvPageControl
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        ActivePage = tabSfrGeneral
        Align = alClient
        TabOrder = 0
        ClientBorderWidth = 0
        object tabSfrGeneral: TTabSheet
          Caption = 'General'
          inline framePkgSFR: TframePackageSFR
            Left = 0
            Top = 0
            Width = 605
            Height = 478
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 605
            ExplicitHeight = 478
            inherited lblComments: TLabel
              Width = 76
              Height = 18
              ExplicitWidth = 76
              ExplicitHeight = 18
            end
            inherited lblPackage: TLabel
              Width = 78
              Height = 18
              ExplicitWidth = 78
              ExplicitHeight = 18
            end
            inherited lblPrintStreams: TLabel
              Left = 231
              Top = 173
              Width = 161
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 231
              ExplicitTop = 173
              ExplicitWidth = 161
              ExplicitHeight = 18
            end
            inherited lblStreamTolerance: TLabel
              Left = 112
              Top = 274
              Width = 189
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 274
              ExplicitWidth = 189
              ExplicitHeight = 18
            end
            inherited lblSfrTrailingWaveIncrements: TLabel
              Left = 112
              Top = 298
              Width = 321
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 298
              ExplicitWidth = 321
              ExplicitHeight = 18
            end
            inherited lblSfrMaxTrailingWaves: TLabel
              Left = 112
              Top = 321
              Width = 337
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 321
              ExplicitWidth = 337
              ExplicitHeight = 18
            end
            inherited lblSfrMaxUnsatCells: TLabel
              Left = 112
              Top = 344
              Width = 424
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 344
              ExplicitWidth = 424
              ExplicitHeight = 18
            end
            inherited lblNUMTIM: TLabel
              Left = 112
              Top = 390
              Width = 451
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 390
              ExplicitWidth = 451
              ExplicitHeight = 18
            end
            inherited lblWeight: TLabel
              Left = 112
              Top = 412
              Width = 442
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 412
              ExplicitWidth = 442
              ExplicitHeight = 18
            end
            inherited lblFLWTOL: TLabel
              Left = 112
              Top = 434
              Width = 411
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 434
              ExplicitWidth = 411
              ExplicitHeight = 18
            end
            inherited memoComments: TMemo
              Left = 20
              Width = 592
              Height = 81
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitLeft = 20
              ExplicitWidth = 592
              ExplicitHeight = 81
            end
            inherited cbSfrUnsatflow: TCheckBox95
              Left = 6
              Top = 149
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRcbSfrUnsatflowClick
              ExplicitLeft = 6
              ExplicitTop = 149
            end
            inherited cbSfrLpfHydraulicCond: TCheckBox95
              Left = 253
              Top = 147
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRcbSfrLpfHydraulicCondClick
              ExplicitLeft = 253
              ExplicitTop = 147
            end
            inherited rgSfr2ISFROPT: TRadioGroup
              Left = 6
              Top = 202
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRrgSfr2ISFROPTClick
              ExplicitLeft = 6
              ExplicitTop = 202
            end
            inherited comboPrintStreams: TComboBox
              Left = 6
              Top = 170
              Width = 219
              Height = 26
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ItemIndex = 1
              Text = 'Print flows in listing file'
              ExplicitLeft = 6
              ExplicitTop = 170
              ExplicitWidth = 219
              ExplicitHeight = 26
            end
            inherited cbGage8: TCheckBox
              Left = 6
              Top = 455
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 455
            end
            inherited rdeDLEAK: TRbwDataEntry
              Left = 6
              Top = 271
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ExplicitLeft = 6
              ExplicitTop = 271
            end
            inherited rdeNstrail: TRbwDataEntry
              Left = 6
              Top = 295
              Height = 18
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ExplicitLeft = 6
              ExplicitTop = 295
              ExplicitHeight = 18
            end
            inherited rdeNsfrsets: TRbwDataEntry
              Left = 6
              Top = 318
              Height = 18
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ExplicitLeft = 6
              ExplicitTop = 318
              ExplicitHeight = 18
            end
            inherited rdeIsuzn: TRbwDataEntry
              Left = 6
              Top = 341
              Height = 18
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ExplicitLeft = 6
              ExplicitTop = 341
              ExplicitHeight = 18
            end
            inherited cbIRTFLG: TCheckBox
              Left = 6
              Top = 364
              Width = 555
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 364
              ExplicitWidth = 555
            end
            inherited rdeNUMTIM: TRbwDataEntry
              Left = 6
              Top = 387
              Height = 18
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ExplicitLeft = 6
              ExplicitTop = 387
              ExplicitHeight = 18
            end
            inherited rdeWeight: TRbwDataEntry
              Left = 6
              Top = 409
              Height = 18
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ExplicitLeft = 6
              ExplicitTop = 409
              ExplicitHeight = 18
            end
            inherited rdeFLWTOL: TRbwDataEntry
              Left = 6
              Top = 431
              Height = 18
              Anchors = [akLeft, akBottom]
              ItemHeight = 18
              ExplicitLeft = 6
              ExplicitTop = 431
              ExplicitHeight = 18
            end
            inherited rcSelectionController: TRbwController
              ControlList = <
                item
                  Control = framePkgSFR.lblComments
                end
                item
                  Control = framePkgSFR.memoComments
                end
                item
                  Control = framePkgSFR.rdeDLEAK
                end
                item
                  Control = framePkgSFR.cbSfrUnsatflow
                end
                item
                  Control = framePkgSFR.comboPrintStreams
                end
                item
                  Control = framePkgSFR.rgSfr2ISFROPT
                end
                item
                  Control = framePkgSFR.cbIRTFLG
                end
                item
                  Control = framePkgSFR.cbGage8
                end>
              OnEnabledChange = framePkgSFRrcSelectionControllerEnabledChange
            end
          end
        end
        object tabSfrParameters: TTabSheet
          Caption = 'Parameters'
          ImageIndex = 1
          object splitSFR: TSplitter
            Left = 0
            Top = 257
            Width = 605
            Height = 5
            Cursor = crVSplit
            Align = alTop
            ExplicitWidth = 603
          end
          inline frameSFRParameterDefinition: TframeListParameterDefinition
            Left = 0
            Top = 0
            Width = 605
            Height = 257
            Align = alTop
            Enabled = False
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 605
            ExplicitHeight = 257
            inherited pnlParameterCount: TPanel
              Top = 209
              Width = 605
              ExplicitTop = 209
              ExplicitWidth = 605
              inherited lblNumParameters: TLabel
                Width = 192
                Height = 18
                Caption = 'Number of SFR parameters'
                ExplicitWidth = 192
                ExplicitHeight = 18
              end
              inherited btnDelete: TBitBtn
                Left = 517
                OnClick = frameSFRParameterDefinitionbtnDeleteClick
                ExplicitLeft = 517
              end
              inherited seNumberOfParameters: TJvSpinEdit
                Left = 6
                Height = 26
                OnChange = frameSFRParameterDefinitionseNumberOfParametersChange
                ExplicitLeft = 6
                ExplicitHeight = 26
              end
            end
            inherited dgParameters: TRbwDataGrid4
              Width = 599
              Height = 203
              OnSelectCell = frameSFRParameterDefinitiondgParametersSelectCell
              OnSetEditText = frameSFRParameterDefinitiondgParametersSetEditText
              ExplicitLeft = 3
              ExplicitTop = 3
              ExplicitWidth = 599
              ExplicitHeight = 203
              ColWidths = (
                64
                64)
            end
          end
          object jplSfrParameters: TJvPageList
            Left = 0
            Top = 262
            Width = 605
            Height = 216
            PropagateEnable = False
            Align = alClient
          end
        end
      end
    end
    object jvspUZF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'UZF_Unsaturated_Zone_Flow_Package'
      Caption = 'jvspUZF'
      inline framePkgUZF: TframePackageUZF
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Top = 39
          Width = 76
          Height = 18
          ExplicitTop = 39
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 136
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 584
          ExplicitHeight = 136
        end
        inherited pnLayerOption: TPanel
          Top = 160
          Width = 605
          Height = 343
          ExplicitTop = 160
          ExplicitWidth = 605
          ExplicitHeight = 343
          inherited lblLayerOption: TLabel
            Top = 6
            Width = 357
            Height = 18
            Caption = 'Recharge and discharge location option (NUZTOP) '
            ExplicitTop = 6
            ExplicitWidth = 357
            ExplicitHeight = 18
          end
          inherited lblVerticalKSource: TLabel
            Top = 57
            Width = 336
            Height = 18
            Caption = 'Vertical hydraulic conductivity source (IUZFOPT) '
            ExplicitTop = 57
            ExplicitWidth = 336
            ExplicitHeight = 18
          end
          inherited lblNumberOfTrailingWaves: TLabel
            Top = 112
            Width = 251
            Height = 18
            Caption = 'Number of trailing waves (NTRAIL2) '
            ExplicitTop = 112
            ExplicitWidth = 251
            ExplicitHeight = 18
          end
          inherited lblNumberOfWaveSets: TLabel
            Top = 161
            Width = 225
            Height = 18
            Caption = 'Number of wave sets (NSETS2) '
            ExplicitTop = 161
            ExplicitWidth = 225
            ExplicitHeight = 18
          end
          inherited lblSURFDEP: TLabel
            Top = 282
            Width = 506
            Height = 18
            ExplicitTop = 282
            ExplicitWidth = 506
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Top = 27
            Height = 26
            ItemHeight = 18
            ExplicitTop = 27
            ExplicitHeight = 26
          end
          inherited comboVerticalKSource: TComboBox
            Top = 80
            Width = 449
            Height = 26
            ItemHeight = 18
            ExplicitTop = 80
            ExplicitWidth = 449
            ExplicitHeight = 26
          end
          inherited cbRouteDischargeToStreamsAndLakes: TCheckBox
            Top = 210
            Width = 449
            Caption = 'Route discharge to streams and lakes (IRUNFLG)'
            ExplicitTop = 210
            ExplicitWidth = 449
          end
          inherited cbSimulateEvapotranspiration: TCheckBox
            Top = 233
            Width = 335
            ExplicitTop = 233
            ExplicitWidth = 335
          end
          inherited rdeNumberOfTrailingWaves: TRbwDataEntry
            Top = 133
            ItemHeight = 18
            ExplicitTop = 133
          end
          inherited rdeNumberOfWaveSets: TRbwDataEntry
            Top = 182
            ItemHeight = 18
            ExplicitTop = 182
          end
          inherited cbPrintSummary: TCheckBox
            Top = 256
            Width = 585
            Caption = 'Print summary of UZF budget terms (IFTUNIT)'
            ExplicitTop = 256
            ExplicitWidth = 585
          end
          inherited rdeSURFDEP: TRbwDataEntry
            Top = 303
            ItemHeight = 18
            ExplicitTop = 303
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgUZF.lblComments
            end
            item
              Control = framePkgUZF.memoComments
            end
            item
              Control = framePkgUZF.comboLayerOption
            end
            item
              Control = framePkgUZF.lblLayerOption
            end
            item
              Control = framePkgUZF.cbRouteDischargeToStreamsAndLakes
            end
            item
              Control = framePkgUZF.cbSimulateEvapotranspiration
            end
            item
            end
            item
              Control = framePkgUZF.rdeNumberOfTrailingWaves
            end
            item
              Control = framePkgUZF.lblNumberOfTrailingWaves
            end
            item
              Control = framePkgUZF.rdeNumberOfWaveSets
            end
            item
            end
            item
              Control = framePkgUZF.lblNumberOfWaveSets
            end
            item
              Control = framePkgUZF.cbPrintSummary
            end
            item
              Control = framePkgUZF.rdeSURFDEP
            end
            item
              Control = framePkgUZF.lblSURFDEP
            end>
          OnEnabledChange = framePkgUZFrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspGMG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'GMG_Geometric_Multigrid_Package'
      Caption = 'jvspGMG'
      inline framePkgGMG: TframeGMG
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        DesignSize = (
          605
          503)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 169
          ExplicitWidth = 584
          ExplicitHeight = 169
        end
        inherited pcGMG: TJvPageControl
          Top = 240
          Width = 605
          Height = 263
          ExplicitTop = 240
          ExplicitWidth = 605
          ExplicitHeight = 263
          inherited tabControlAndPrint: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 597
            ExplicitHeight = 230
            DesignSize = (
              597
              230)
            inherited lblGmgRclose: TLabel
              Top = 12
              Width = 294
              Height = 18
              ExplicitTop = 12
              ExplicitWidth = 294
              ExplicitHeight = 18
            end
            inherited lblGmgIiter: TLabel
              Top = 44
              Width = 297
              Height = 18
              ExplicitTop = 44
              ExplicitWidth = 297
              ExplicitHeight = 18
            end
            inherited lblGmgHclose: TLabel
              Top = 76
              Width = 326
              Height = 18
              ExplicitTop = 76
              ExplicitWidth = 326
              ExplicitHeight = 18
            end
            inherited lblGmgMxiter: TLabel
              Top = 108
              Width = 319
              Height = 18
              ExplicitTop = 108
              ExplicitWidth = 319
              ExplicitHeight = 18
            end
            inherited lblGmgIoutgmg: TLabel
              Top = 171
              Width = 172
              Height = 18
              ExplicitTop = 171
              ExplicitWidth = 172
              ExplicitHeight = 18
            end
            inherited lblGmgIsm: TLabel
              Top = 142
              Width = 273
              Height = 18
              ExplicitTop = 142
              ExplicitWidth = 273
              ExplicitHeight = 18
            end
            inherited rdeGmgRclose: TRbwDataEntry
              Top = 7
              ItemHeight = 18
              ExplicitTop = 7
            end
            inherited rdeGmgIiter: TRbwDataEntry
              Top = 39
              ItemHeight = 18
              ExplicitTop = 39
            end
            inherited rdeGmgHclose: TRbwDataEntry
              Top = 71
              ItemHeight = 18
              ExplicitTop = 71
            end
            inherited rdeGmgMxiter: TRbwDataEntry
              Top = 103
              ItemHeight = 18
              ExplicitTop = 103
            end
            inherited comboGmgIoutgmg: TJvImageComboBox
              Left = 304
              Top = 168
              Width = 269
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 168
              ExplicitWidth = 269
              ExplicitHeight = 28
            end
            inherited cbGmbIunitmhc: TCheckBox
              Top = 205
              ExplicitTop = 205
            end
            inherited comboGmgIsm: TJvImageComboBox
              Left = 304
              Top = 139
              Width = 269
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 139
              ExplicitWidth = 269
              ExplicitHeight = 28
            end
          end
          inherited tabDampRelax: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 617
            ExplicitHeight = 225
            inherited lblGmgDup: TLabel
              Top = 76
              Width = 220
              Height = 18
              ExplicitTop = 76
              ExplicitWidth = 220
              ExplicitHeight = 18
            end
            inherited lblGmgDlow: TLabel
              Top = 108
              Width = 230
              Height = 18
              ExplicitTop = 108
              ExplicitWidth = 230
              ExplicitHeight = 18
            end
            inherited lblGmgChglimit: TLabel
              Top = 139
              Width = 304
              Height = 18
              ExplicitTop = 139
              ExplicitWidth = 304
              ExplicitHeight = 18
            end
            inherited lblGmgRelax: TLabel
              Top = 203
              Width = 216
              Height = 18
              ExplicitTop = 203
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblGmgIadamp: TLabel
              Top = 44
              Width = 258
              Height = 18
              ExplicitTop = 44
              ExplicitWidth = 258
              ExplicitHeight = 18
            end
            inherited lblGmgIsc: TLabel
              Top = 171
              Width = 204
              Height = 18
              ExplicitTop = 171
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited lblGmgDamp: TLabel
              Top = 12
              Width = 201
              Height = 18
              ExplicitTop = 12
              ExplicitWidth = 201
              ExplicitHeight = 18
            end
            inherited rdeGmgDup: TRbwDataEntry
              Top = 73
              ItemHeight = 18
              ExplicitTop = 73
            end
            inherited rdeGmgRelax: TRbwDataEntry
              Top = 200
              ItemHeight = 18
              ExplicitTop = 200
            end
            inherited rdeGmgChglimit: TRbwDataEntry
              Top = 136
              ItemHeight = 18
              ExplicitTop = 136
            end
            inherited rdeGmgDlow: TRbwDataEntry
              Top = 105
              ItemHeight = 18
              ExplicitTop = 105
            end
            inherited comboGmgIadamp: TJvImageComboBox
              Left = 304
              Top = 41
              Width = 269
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 41
              ExplicitWidth = 269
              ExplicitHeight = 28
            end
            inherited comboGmgIsc: TJvImageComboBox
              Left = 304
              Width = 269
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitWidth = 269
              ExplicitHeight = 28
            end
            inherited rdeGmgDamp: TRbwDataEntry
              Top = 9
              ItemHeight = 18
              ExplicitTop = 9
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgGMG.lblComments
            end
            item
              Control = framePkgGMG.memoComments
            end
            item
              Control = framePkgGMG.rdeGmgDamp
            end
            item
              Control = framePkgGMG.rdeGmgMxiter
            end
            item
              Control = framePkgGMG.rdeGmgHclose
            end
            item
              Control = framePkgGMG.rdeGmgIiter
            end
            item
              Control = framePkgGMG.rdeGmgRclose
            end
            item
              Control = framePkgGMG.comboGmgIsc
            end
            item
              Control = framePkgGMG.comboGmgIsm
            end
            item
              Control = framePkgGMG.cbGmbIunitmhc
            end
            item
              Control = framePkgGMG.comboGmgIoutgmg
            end
            item
              Control = framePkgGMG.comboGmgIadamp
            end>
        end
      end
    end
    object jvspSIP: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'SIP_Strongly_Implicit_Procedure_Package'
      Caption = 'jvspSIP'
      inline framePkgSIP: TframeSIP
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        DesignSize = (
          605
          503)
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblSipMxiter: TLabel
          Top = 170
          Width = 246
          Height = 18
          ExplicitTop = 170
          ExplicitWidth = 246
          ExplicitHeight = 18
        end
        inherited lblSipNparm: TLabel
          Top = 202
          Width = 270
          Height = 18
          ExplicitTop = 202
          ExplicitWidth = 270
          ExplicitHeight = 18
        end
        inherited lblSipAccl: TLabel
          Top = 234
          Width = 204
          Height = 18
          ExplicitTop = 234
          ExplicitWidth = 204
          ExplicitHeight = 18
        end
        inherited lblSipHclose: TLabel
          Top = 266
          Width = 233
          Height = 18
          ExplicitTop = 266
          ExplicitWidth = 233
          ExplicitHeight = 18
        end
        inherited lblSipIpcalc: TLabel
          Top = 298
          Width = 161
          Height = 18
          ExplicitTop = 298
          ExplicitWidth = 161
          ExplicitHeight = 18
        end
        inherited lblSipWseed: TLabel
          Top = 330
          Width = 399
          Height = 18
          ExplicitTop = 330
          ExplicitWidth = 399
          ExplicitHeight = 18
        end
        inherited lblSipIprsip: TLabel
          Top = 362
          Width = 170
          Height = 18
          ExplicitTop = 362
          ExplicitWidth = 170
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 99
          ExplicitWidth = 584
          ExplicitHeight = 99
        end
        inherited rdeSipMxiter: TRbwDataEntry
          Top = 167
          ItemHeight = 18
          ExplicitTop = 167
        end
        inherited rdeSipNparm: TRbwDataEntry
          Top = 195
          ItemHeight = 18
          ExplicitTop = 195
        end
        inherited rdeSipAccl: TRbwDataEntry
          Top = 231
          ItemHeight = 18
          ExplicitTop = 231
        end
        inherited rdeSipHclose: TRbwDataEntry
          Top = 259
          ItemHeight = 18
          ExplicitTop = 259
        end
        inherited comboSipIpcalc: TJvImageComboBox
          Top = 287
          Height = 28
          ItemHeight = 22
          ExplicitTop = 287
          ExplicitHeight = 28
        end
        inherited rdeSipWseed: TRbwDataEntry
          Top = 323
          ItemHeight = 18
          ExplicitTop = 323
        end
        inherited rdeSipIprsip: TRbwDataEntry
          Top = 355
          ItemHeight = 18
          ExplicitTop = 355
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSIP.lblComments
            end
            item
              Control = framePkgSIP.memoComments
            end
            item
              Control = framePkgSIP.rdeSipMxiter
            end
            item
              Control = framePkgSIP.rdeSipNparm
            end
            item
              Control = framePkgSIP.rdeSipAccl
            end
            item
              Control = framePkgSIP.rdeSipHclose
            end
            item
              Control = framePkgSIP.comboSipIpcalc
            end
            item
              Control = framePkgSIP.rdeSipIprsip
            end>
        end
      end
    end
    object jvspDE4: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'DE4_Direct_Solver_Package_Pane'
      Caption = 'jvspDE4'
      inline framePkgDE4: TframeDE4
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblDe4Itmx: TLabel
          Top = 178
          Width = 258
          Height = 18
          ExplicitTop = 178
          ExplicitWidth = 258
          ExplicitHeight = 18
        end
        inherited lblDe4Mxup: TLabel
          Top = 210
          Width = 363
          Height = 18
          ExplicitTop = 210
          ExplicitWidth = 363
          ExplicitHeight = 18
        end
        inherited lblDe4Mxlow: TLabel
          Top = 238
          Width = 374
          Height = 18
          ExplicitTop = 238
          ExplicitWidth = 374
          ExplicitHeight = 18
        end
        inherited lblDe4Mxbw: TLabel
          Top = 270
          Width = 210
          Height = 18
          ExplicitTop = 270
          ExplicitWidth = 210
          ExplicitHeight = 18
        end
        inherited lblDe4Ifreq: TLabel
          Top = 298
          Width = 332
          Height = 18
          ExplicitTop = 298
          ExplicitWidth = 332
          ExplicitHeight = 18
        end
        inherited lblDe4Mutd4: TLabel
          Top = 330
          Width = 150
          Height = 18
          ExplicitTop = 330
          ExplicitWidth = 150
          ExplicitHeight = 18
        end
        inherited lblDe4Accl: TLabel
          Top = 358
          Width = 221
          Height = 18
          ExplicitTop = 358
          ExplicitWidth = 221
          ExplicitHeight = 18
        end
        inherited lblDe4Hclose: TLabel
          Top = 390
          Width = 287
          Height = 18
          ExplicitTop = 390
          ExplicitWidth = 287
          ExplicitHeight = 18
        end
        inherited lblRdeIprd4: TLabel
          Top = 418
          Width = 166
          Height = 18
          ExplicitTop = 418
          ExplicitWidth = 166
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 99
          ExplicitWidth = 584
          ExplicitHeight = 99
        end
        inherited rdeDe4Itmx: TRbwDataEntry
          Left = 454
          Top = 175
          ItemHeight = 18
          ExplicitLeft = 454
          ExplicitTop = 175
        end
        inherited rdeDe4Mxup: TRbwDataEntry
          Left = 454
          Top = 207
          ItemHeight = 18
          ExplicitLeft = 454
          ExplicitTop = 207
        end
        inherited rdeDe4Mxlow: TRbwDataEntry
          Left = 454
          Top = 235
          ItemHeight = 18
          ExplicitLeft = 454
          ExplicitTop = 235
        end
        inherited rdeDe4Mxbw: TRbwDataEntry
          Left = 454
          Top = 267
          ItemHeight = 18
          ExplicitLeft = 454
          ExplicitTop = 267
        end
        inherited comboDe4Ifreq: TJvImageComboBox
          Left = 360
          Top = 294
          Width = 239
          Height = 28
          DroppedWidth = 263
          ItemHeight = 22
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coefficients constant (1)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coefficients vary (2)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Nonlinear flow equations (3)'
            end>
          ExplicitLeft = 360
          ExplicitTop = 294
          ExplicitWidth = 239
          ExplicitHeight = 28
        end
        inherited comboDe4Mutd4: TJvImageComboBox
          Left = 360
          Top = 327
          Width = 241
          Height = 28
          DroppedWidth = 272
          ItemHeight = 22
          ExplicitLeft = 360
          ExplicitTop = 327
          ExplicitWidth = 241
          ExplicitHeight = 28
        end
        inherited rdeDe4Accl: TRbwDataEntry
          Left = 454
          Top = 355
          ItemHeight = 18
          ExplicitLeft = 454
          ExplicitTop = 355
        end
        inherited rdeDe4Hclose: TRbwDataEntry
          Left = 454
          Top = 387
          ItemHeight = 18
          ExplicitLeft = 454
          ExplicitTop = 387
        end
        inherited rdeRdeIprd4: TRbwDataEntry
          Left = 454
          Top = 415
          ItemHeight = 18
          ExplicitLeft = 454
          ExplicitTop = 415
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDE4.lblComments
            end
            item
              Control = framePkgDE4.memoComments
            end
            item
              Control = framePkgDE4.rdeDe4Itmx
            end
            item
              Control = framePkgDE4.rdeDe4Mxup
            end
            item
              Control = framePkgDE4.rdeDe4Mxlow
            end
            item
              Control = framePkgDE4.rdeDe4Mxbw
            end
            item
              Control = framePkgDE4.comboDe4Ifreq
            end
            item
              Control = framePkgDE4.comboDe4Mutd4
            end
            item
              Control = framePkgDE4.rdeDe4Accl
            end
            item
              Control = framePkgDE4.rdeDe4Hclose
            end
            item
              Control = framePkgDE4.rdeRdeIprd4
            end>
        end
      end
    end
    object jvspHOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'HOB_Head_Observation_Package'
      Caption = 'jvspHOB'
      inline framePkgHOB: TframePackageHob
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblDryHead: TLabel
          Top = 453
          Width = 172
          Height = 18
          ExplicitTop = 453
          ExplicitWidth = 172
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 584
          Height = 378
          ExplicitWidth = 584
          ExplicitHeight = 378
        end
        inherited rdeDryHead: TRbwDataEntry
          Top = 472
          ItemHeight = 18
          ExplicitTop = 472
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHOB.lblComments
            end
            item
              Control = framePkgHOB.memoComments
            end
            item
              Control = framePkgHOB.rdeDryHead
            end>
        end
      end
    end
    object jvspHFB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'HFB_Horizontal_Flow_Barrier_Package'
      Caption = 'jvspHFB'
      inline framePkgHFB: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHFB.lblComments
            end
            item
              Control = framePkgHFB.memoComments
            end
            item
              Control = frameHfbParameterDefinition
            end>
        end
      end
      inline frameHfbParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 605
        Height = 342
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 605
        ExplicitHeight = 342
        inherited pnlParameterCount: TPanel
          Top = 294
          Width = 605
          ExplicitTop = 294
          ExplicitWidth = 605
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 517
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 517
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            Enabled = True
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 599
          Height = 288
          ExplicitLeft = 3
          ExplicitTop = 3
          ExplicitWidth = 599
          ExplicitHeight = 288
        end
      end
    end
    object jvspModpath: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'MODPATH'
      Caption = 'jvspModpath'
      inline frameModpath: TframeModpathSelection
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 586
          ExplicitWidth = 586
        end
        inherited pcModpath: TPageControl
          inherited tabInput: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 331
            inherited lblEndingTime: TLabel
              Width = 241
              Height = 18
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
            inherited lblBeginningTime: TLabel
              Width = 288
              Height = 18
              ExplicitWidth = 288
              ExplicitHeight = 18
            end
            inherited lblRchSource: TLabel
              Width = 224
              Height = 18
              ExplicitWidth = 224
              ExplicitHeight = 18
            end
            inherited lblEvtSink: TLabel
              Width = 288
              Height = 18
              ExplicitWidth = 288
              ExplicitHeight = 18
            end
            inherited lblMaxSize: TLabel
              Width = 245
              Height = 72
              ExplicitWidth = 245
              ExplicitHeight = 72
            end
            inherited rdeMaxSize: TRbwDataEntry
              ItemHeight = 18
            end
            inherited comboEvtSink: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboRchSource: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited rdeBeginningTime: TRbwDataEntry
              ItemHeight = 18
            end
            inherited rdeEndingTime: TRbwDataEntry
              ItemHeight = 18
            end
          end
          inherited tabResponse: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 331
            inherited lblMaxTime: TLabel
              Width = 160
              Height = 18
              ExplicitWidth = 160
              ExplicitHeight = 18
            end
            inherited lblTrackingDirection: TLabel
              Width = 124
              Height = 18
              ExplicitWidth = 124
              ExplicitHeight = 18
            end
            inherited lblWeakSinkTreatment: TLabel
              Width = 168
              Height = 18
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited lblWeakSinkThreshold: TLabel
              Width = 207
              Height = 18
              ExplicitWidth = 207
              ExplicitHeight = 18
            end
            inherited lblStopZone: TLabel
              Width = 208
              Height = 18
              ExplicitWidth = 208
              ExplicitHeight = 18
            end
            inherited lblWhichEndpoints: TLabel
              Width = 262
              Height = 18
              ExplicitWidth = 262
              ExplicitHeight = 18
            end
            inherited lblErrorTolerance: TLabel
              Width = 103
              Height = 18
              ExplicitWidth = 103
              ExplicitHeight = 18
            end
            inherited lblReferenceTime: TLabel
              Width = 202
              Height = 18
              ExplicitWidth = 202
              ExplicitHeight = 18
            end
            inherited lblReleaseTime: TLabel
              Width = 241
              Height = 18
              Margins.Bottom = 0
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
            inherited rdeMaxTime: TRbwDataEntry
              ItemHeight = 18
            end
            inherited comboTrackingDirection: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboWeakSinkTreatment: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited rdeWeakSinkThreshold: TRbwDataEntry
              ItemHeight = 18
            end
            inherited rdeStopZone: TRbwDataEntry
              ItemHeight = 18
            end
            inherited comboWhichEndpoints: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited rdeErrorTolerance: TRbwDataEntry
              ItemHeight = 18
            end
            inherited rdeReferenceTime: TRbwDataEntry
              ItemHeight = 18
            end
            inherited rdeReleaseTime: TRbwDataEntry
              ItemHeight = 18
            end
          end
          inherited tabOutputTimes: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 331
            inherited lblTimeMethod: TLabel
              Width = 185
              Height = 18
              ExplicitWidth = 185
              ExplicitHeight = 18
            end
            inherited lblParticleInterval: TLabel
              Width = 156
              Height = 18
              ExplicitWidth = 156
              ExplicitHeight = 18
            end
            inherited lblMaxTimes: TLabel
              Width = 239
              Height = 18
              ExplicitWidth = 239
              ExplicitHeight = 18
            end
            inherited gbTime: TJvGroupBox
              inherited lblTimeCount: TLabel
                Width = 55
                Height = 36
                ExplicitWidth = 55
                ExplicitHeight = 36
              end
              inherited rdgTimes: TRbwDataGrid4
                FixedCols = 0
              end
              inherited seTimeCount: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited comboTimeMethod: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited rdeParticleInterval: TRbwDataEntry
              ItemHeight = 18
            end
            inherited rdeMaxTimes: TRbwDataEntry
              ItemHeight = 18
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameModpath.lblComments
            end
            item
              Control = frameModpath.memoComments
            end
            item
              Control = frameModpath.cbCompact
            end
            item
              Control = frameModpath.cbBinary
            end
            item
              Control = frameModpath.rdeMaxSize
            end
            item
              Control = frameModpath.rgOutputMode
            end
            item
              Control = frameModpath.comboTrackingDirection
            end
            item
              Control = frameModpath.comboWeakSinkTreatment
            end
            item
              Control = frameModpath.cbStopInZone
            end
            item
              Control = frameModpath.cbComputeBudget
            end
            item
              Control = frameModpath.cbSummarize
            end
            item
              Control = frameModpath.cbBigBudget
            end
            item
              Control = frameModpath.cbStopAfterMaxTime
            end>
          OnEnabledChange = frameModpathrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspCHOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'CHOB_Specified_Head_Flow_Observations'
      Caption = 'jvspCHOB'
      inline framePkgCHOB: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 435
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 574
          ExplicitHeight = 435
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgCHOB.lblComments
            end
            item
              Control = framePkgCHOB.memoComments
            end>
        end
      end
    end
    object jvspDROB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'DROB_Drain_Observation_Package'
      Caption = 'jvspDROB'
      inline framePkgDROB: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 574
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgDROB.lblComments
            end
            item
              Control = framePkgDROB.memoComments
            end>
        end
      end
    end
    object jvspGBOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'GBOB_General_Head_Boundary_Observations'
      Caption = 'jvspGBOB'
      inline framePkgGBOB: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 574
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgGBOB.lblComments
            end
            item
              Control = framePkgGBOB.memoComments
            end>
        end
      end
    end
    object jvspRVOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'RVOB_River_Observation_Package'
      Caption = 'jvspRVOB'
      inline framePkgRVOB: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 448
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 574
          ExplicitHeight = 448
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgRVOB.lblComments
            end
            item
              Control = framePkgRVOB.memoComments
            end>
        end
      end
    end
    object jvspMNW2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'MNW2_Multi_Node_Well_Package'
      Caption = 'jvspMNW2'
      inline framePkgMnw2: TframePackageMnw2
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblPrintOption: TLabel
          Top = 345
          Width = 174
          Height = 18
          ExplicitTop = 345
          ExplicitWidth = 174
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 277
          ExplicitWidth = 574
          ExplicitHeight = 277
        end
        inherited comboPrintOption: TJvImageComboBox
          Top = 368
          Height = 28
          ItemHeight = 22
          ExplicitTop = 368
          ExplicitHeight = 28
        end
        inherited gbMnwiOptions: TGroupBox
          Top = 402
          Width = 574
          ExplicitTop = 402
          ExplicitWidth = 574
          inherited cbWellOutput: TCheckBox
            Width = 558
            ExplicitWidth = 558
          end
          inherited cbSummarizeByWell: TCheckBox
            Width = 558
            ExplicitWidth = 558
          end
          inherited cbSummarizeByNode: TCheckBox
            Width = 550
            ExplicitWidth = 550
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMnw2.lblComments
            end
            item
              Control = framePkgMnw2.memoComments
            end
            item
              Control = framePkgMnw2.lblPrintOption
            end
            item
              Control = framePkgMnw2.comboPrintOption
            end>
        end
      end
    end
    object jvspBCF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'BCF_Block-Centered_Flow_Package'
      Caption = 'jvspBCF'
      inline framePkgBCF: TframePackage
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Margins.Bottom = 0
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 435
          ExplicitWidth = 574
          ExplicitHeight = 435
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgBCF.lblComments
            end
            item
              Control = framePkgBCF.memoComments
            end>
          OnEnabledChange = framePkgBCFrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspSUB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 605
      Height = 503
      HelpType = htKeyword
      HelpKeyword = 'SUB_Subsidence_and_Aquifer_Sys'
      Caption = 'jvspSUB'
      inline framePkgSUB: TframePackageSub
        Left = 0
        Top = 0
        Width = 605
        Height = 503
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 605
        ExplicitHeight = 503
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited pcSub: TPageControl
          Width = 605
          Height = 384
          ExplicitWidth = 605
          ExplicitHeight = 384
          inherited tabControls: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 436
            ExplicitHeight = 250
            inherited lblNumberOfNodes: TLabel
              Width = 287
              Height = 18
              Margins.Bottom = 0
              ExplicitWidth = 287
              ExplicitHeight = 18
            end
            inherited lblAccel1: TLabel
              Width = 238
              Height = 18
              Margins.Bottom = 0
              ExplicitWidth = 238
              ExplicitHeight = 18
            end
            inherited lblAccel2: TLabel
              Width = 265
              Height = 18
              Margins.Bottom = 0
              ExplicitWidth = 265
              ExplicitHeight = 18
            end
            inherited lblMinIterations: TLabel
              Width = 353
              Height = 18
              Margins.Bottom = 0
              ExplicitWidth = 353
              ExplicitHeight = 18
            end
            inherited lbReadRestart: TLabel
              Width = 380
              Height = 18
              Margins.Bottom = 0
              ExplicitWidth = 380
              ExplicitHeight = 18
            end
            inherited lblOutputChoice: TLabel
              Width = 140
              Height = 18
              Margins.Bottom = 0
              ExplicitWidth = 140
              ExplicitHeight = 18
            end
            inherited seNumberOfNodes: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited rdeAccel1: TRbwDataEntry
              ItemHeight = 18
            end
            inherited rdeAccel2: TRbwDataEntry
              ItemHeight = 18
            end
            inherited rdeMinIterations: TRbwDataEntry
              ItemHeight = 18
            end
            inherited feReadRestart: TJvFilenameEdit
              Left = 14
              Top = 163
              Width = 573
              Height = 26
              ExplicitLeft = 14
              ExplicitTop = 163
              ExplicitWidth = 573
              ExplicitHeight = 26
            end
            inherited comboOutputChoice: TJvImageComboBox
              Top = 213
              Height = 28
              ItemHeight = 22
              ExplicitTop = 213
              ExplicitHeight = 28
            end
          end
          inherited tabFormat: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 436
            ExplicitHeight = 250
            inherited rdgFormats: TRbwDataGrid4
              Left = 13
              Top = 40
              Width = 734
              Height = 392
              ExplicitLeft = 13
              ExplicitTop = 40
              ExplicitWidth = 734
              ExplicitHeight = 392
              ColWidths = (
                83
                64
                64
                64
                64
                64)
            end
            inherited comboMultiFomat: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
          end
          inherited tabPrintSave: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 597
            ExplicitHeight = 351
            inherited lblNumExportPeriods: TLabel
              Top = 427
              Width = 176
              Height = 18
              Margins.Bottom = 0
              ExplicitTop = 326
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited sbAdd: TSpeedButton
              Left = 667
              Top = 428
              ExplicitLeft = 506
              ExplicitTop = 327
            end
            inherited sbInsert: TSpeedButton
              Left = 696
              Top = 428
              ExplicitLeft = 535
              ExplicitTop = 327
            end
            inherited sbDelete: TSpeedButton
              Left = 725
              Top = 428
              ExplicitLeft = 564
              ExplicitTop = 327
            end
            inherited cbMultiPrintSave: TCheckBox
              Width = 209
              ExplicitWidth = 209
            end
            inherited rdgOutput: TRbwDataGrid4
              Left = 13
              Top = 40
              Width = 573
              Height = 297
              ExplicitLeft = 13
              ExplicitTop = 40
              ExplicitWidth = 573
              ExplicitHeight = 297
            end
            inherited seNumExportPeriods: TJvSpinEdit
              Top = 424
              Height = 26
              ExplicitTop = 424
              ExplicitHeight = 26
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSUB.lblComments
            end
            item
              Control = framePkgSUB.memoComments
            end
            item
              Control = framePkgSUB.seNumberOfNodes
            end
            item
              Control = framePkgSUB.rdeAccel1
            end
            item
              Control = framePkgSUB.rdeAccel2
            end
            item
              Control = framePkgSUB.rdeMinIterations
            end
            item
              Control = framePkgSUB.cbSaveRestart
            end
            item
              Control = framePkgSUB.feReadRestart
            end
            item
              Control = framePkgSUB.rdgFormats
            end
            item
              Control = framePkgSUB.rdgOutput
            end
            item
              Control = framePkgSUB.seNumExportPeriods
            end
            item
              Control = framePkgSUB.sbAdd
            end
            item
              Control = framePkgSUB.sbInsert
            end>
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 503
    Width = 792
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      792
      41)
    object btnHelp: TBitBtn
      Left = 451
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 565
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 679
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
  end
  object rbwLpfParamCountController: TRbwController
    ControlList = <
      item
        Control = frameLpfParameterDefinition.btnDelete
      end
      item
        Control = frameLpfParameterDefinition.dgParameters
      end
      item
        Control = frameLpfParameterDefinition.lblNumParameters
      end
      item
        Control = frameLpfParameterDefinition.seNumberOfParameters
      end>
    Left = 192
    Top = 216
  end
  object ilCheckImages: TImageList
    Left = 136
    Top = 96
    Bitmap = {
      494C010108000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      00000000000000000000000000000000000000000000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C00000000000000000000000000000000000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C00000000000000000000000000000000000C0C0C000C0C0
      C0000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C0000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C00000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C0000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000008080
      80000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000808080008080
      80000000000000000000000000000000000000000000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000000000000000
      00000000000000000000000000000000000000000000C0C0C000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0000000000D8E9EC0099A8AC00D8E9EC0000000000D8E9EC0000000000D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0099A8AC0099A8AC0099A8AC00D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC00D8E9EC0000000000D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      000099A8AC0099A8AC00D8E9EC0099A8AC0099A8AC0099A8AC00D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0099A8AC00D8E9EC0000000000D8E9EC0099A8AC0099A8AC0099A8AC00D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0099A8AC0099A8AC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC0099A8AC00D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFC3FFC3FBFFFFC3FE3CFE3CF800FE00FE7EFE7EF800FE00F
      CFF7CE77800FC007CFF7CC37800FC007CFF7CC37800FC007CFF7CE77800FC007
      E7EFE7EF800FE00FE3CFE3CF800FE00FF00FF00F800FF00FFC3FFC3F800FFC3F
      FFFFFFFF8007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF80078007800780079FF79FF79FF795579FF79FF79DF788A7
      9FF79FF798F790579FF79FF7907780279FF79FF7923790179FF79FF797178207
      9FF79FF79F9795179FF79FF79FD78A879FF79FF79FF795578007800780078007
      8007800780078007FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object rbwHufParamCountController: TRbwController
    ControlList = <
      item
        Control = frameHufParameterDefinition.btnDelete
      end
      item
        Control = frameHufParameterDefinition.dgParameters
      end
      item
        Control = frameHfbParameterDefinition.lblNumParameters
      end
      item
        Control = frameHfbParameterDefinition.seNumberOfParameters
      end>
    Left = 192
    Top = 256
  end
end
