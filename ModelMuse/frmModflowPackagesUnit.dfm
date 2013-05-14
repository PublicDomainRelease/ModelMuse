inherited frmModflowPackages: TfrmModflowPackages
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Packages_Dialog_Box'
  Caption = 'MODFLOW Packages and Programs'
  ClientHeight = 555
  ClientWidth = 782
  OnResize = FormResize
  ExplicitWidth = 800
  ExplicitHeight = 600
  PixelsPerInch = 120
  TextHeight = 18
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 177
    Top = 0
    Height = 514
    Align = alLeft
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 121
    ExplicitTop = -14
    ExplicitHeight = 252
  end
  object jvplPackages: TJvPageList
    Left = 187
    Top = 0
    Width = 595
    Height = 514
    ActivePage = jvspSWI
    PropagateEnable = False
    Align = alClient
    OnChange = jvplPackagesChange
    object jvspLPF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'LPF_Layer_Property_Flow_Package'
      Caption = 'LPF (Layer Property Flow)'
      object splitLprParameter: TJvNetscapeSplitter
        Left = 121
        Top = 291
        Height = 223
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
        Width = 595
        Height = 10
        Cursor = crVSplit
        Align = alTop
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = -4
        ExplicitTop = 283
        ExplicitWidth = 605
      end
      inline frameLpfParameterDefinition: TframeArrayParameterDefinition
        Left = 131
        Top = 291
        Width = 464
        Height = 223
        Align = alClient
        Enabled = False
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 131
        ExplicitTop = 291
        ExplicitWidth = 464
        ExplicitHeight = 223
        inherited pnlParameterCount: TPanel
          Top = 175
          Width = 464
          ExplicitTop = 175
          ExplicitWidth = 464
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 351
            Top = 9
            Enabled = True
            TabOrder = 1
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 351
            ExplicitTop = 9
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            Enabled = True
            TabOrder = 0
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 458
          Height = 112
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 458
          ExplicitHeight = 112
        end
        inherited pnlTop: TPanel
          Width = 464
          ExplicitWidth = 464
          inherited lblParamValue: TLabel
            Width = 101
            Height = 18
            ExplicitWidth = 101
            ExplicitHeight = 18
          end
        end
      end
      object tvLpfParameterTypes: TTreeView
        Left = 0
        Top = 291
        Width = 121
        Height = 223
        Align = alLeft
        Enabled = False
        HideSelection = False
        Indent = 20
        ReadOnly = True
        TabOrder = 1
        OnChange = tvLpfParameterTypesChange
      end
      inline framePkgLPF: TframePackageLpf
        Left = 0
        Top = 0
        Width = 595
        Height = 281
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited rdgOptions: TRbwDataGrid4
          Width = 567
          ExplicitWidth = 567
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
            end>
          OnEnabledChange = framePkgLPFrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspHUF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'HUF2_Hydrogeologic_Unit_Flow'
      Caption = 'jvspHUF'
      object JvNetscapeSplitter4: TJvNetscapeSplitter
        Left = 0
        Top = 275
        Width = 595
        Height = 10
        Cursor = crVSplit
        Align = alTop
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitTop = 204
        ExplicitWidth = 605
      end
      object JvNetscapeSplitter5: TJvNetscapeSplitter
        Left = 121
        Top = 285
        Height = 229
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
        Width = 595
        Height = 275
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        DesignSize = (
          595
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited cbSaveHeads: TCheckBox
          Width = 441
          ExplicitWidth = 441
        end
        inherited cbSaveFlows: TCheckBox
          Width = 417
          ExplicitWidth = 417
        end
        inherited rgElevationSurfaceChoice: TRadioGroup
          Width = 313
          ExplicitWidth = 313
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
        Height = 229
        Align = alLeft
        Enabled = False
        HideSelection = False
        Indent = 20
        ReadOnly = True
        TabOrder = 1
        OnChange = tvHufParameterTypesChange
      end
      inline frameHufParameterDefinition: TframeListParameterDefinition
        Left = 131
        Top = 285
        Width = 464
        Height = 229
        Align = alClient
        Enabled = False
        TabOrder = 2
        TabStop = True
        ExplicitLeft = 131
        ExplicitTop = 285
        ExplicitWidth = 464
        ExplicitHeight = 229
        inherited pnlParameterCount: TPanel
          Top = 181
          Width = 464
          ExplicitTop = 181
          ExplicitWidth = 464
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 351
            Top = 9
            Enabled = True
            TabOrder = 1
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 351
            ExplicitTop = 9
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            TabOrder = 0
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 458
          Height = 175
          Enabled = True
          ExplicitWidth = 458
          ExplicitHeight = 175
        end
      end
    end
    object jvspCHD: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'CHD_Time_Variant_Specified_Head'
      Caption = 'CHD (Time-Variant Specified-Head Package)'
      inline framePkgCHD: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
          Width = 564
          ExplicitWidth = 564
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
        Width = 595
        Height = 353
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 353
        inherited pnlParameterCount: TPanel
          Top = 305
          Width = 595
          ExplicitTop = 305
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            Top = 9
            TabOrder = 1
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
            ExplicitTop = 9
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            TabOrder = 0
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 299
          ExplicitWidth = 589
          ExplicitHeight = 299
        end
      end
    end
    object jvspGHB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'GHB_General_Head_Boundary_Package'
      Caption = 'GHB (General Head Boundary)'
      inline framePkgGHB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
          Width = 564
          ExplicitWidth = 564
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
        Width = 595
        Height = 353
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 353
        inherited pnlParameterCount: TPanel
          Top = 305
          Width = 595
          ExplicitTop = 305
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 299
          ExplicitWidth = 589
          ExplicitHeight = 299
        end
      end
    end
    object jvspPCG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'PCG_Preconditioned_Conjugate_Gradiant'
      Caption = 'PCG (Preconditioned Conjugate-Gradient)'
      inline framePCG: TframePCG
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        DesignSize = (
          595
          514)
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
        inherited lblPCGMaxOuter: TLabel
          Top = 127
          Width = 289
          Height = 18
          ExplicitTop = 127
          ExplicitWidth = 289
          ExplicitHeight = 18
        end
        inherited lblPCGMaxInner: TLabel
          Top = 155
          Width = 273
          Height = 18
          ExplicitTop = 155
          ExplicitWidth = 273
          ExplicitHeight = 18
        end
        inherited lblPCGMethod: TLabel
          Top = 183
          Width = 296
          Height = 18
          ExplicitTop = 183
          ExplicitWidth = 296
          ExplicitHeight = 18
        end
        inherited lblPCGMaxChangeHead: TLabel
          Top = 295
          Width = 261
          Height = 18
          ExplicitTop = 306
          ExplicitWidth = 261
          ExplicitHeight = 18
        end
        inherited lblPCGMaxResidual: TLabel
          Top = 323
          Width = 210
          Height = 18
          ExplicitTop = 334
          ExplicitWidth = 210
          ExplicitHeight = 18
        end
        inherited lblPCGRelaxation: TLabel
          Top = 351
          Width = 220
          Height = 18
          ExplicitTop = 362
          ExplicitWidth = 220
          ExplicitHeight = 18
        end
        inherited lblPCGMaxEigen: TLabel
          Top = 379
          Width = 320
          Height = 18
          ExplicitTop = 390
          ExplicitWidth = 320
          ExplicitHeight = 18
        end
        inherited lblPCGPrintInterval: TLabel
          Top = 406
          Width = 184
          Height = 18
          ExplicitTop = 417
          ExplicitWidth = 184
          ExplicitHeight = 18
        end
        inherited lblPCGPrintControl: TLabel
          Top = 434
          Width = 189
          Height = 18
          ExplicitTop = 445
          ExplicitWidth = 189
          ExplicitHeight = 18
        end
        inherited lblPCGDampingFactor: TLabel
          Top = 461
          Width = 207
          Height = 18
          ExplicitTop = 472
          ExplicitWidth = 207
          ExplicitHeight = 18
        end
        inherited lblPCGDampPcgT: TLabel
          Top = 489
          Width = 280
          Height = 18
          ExplicitTop = 489
          ExplicitWidth = 280
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          ExplicitWidth = 574
        end
        inherited rdePCGMaxOuter: TRbwDataEntry
          Top = 124
          Width = 242
          ExplicitTop = 124
          ExplicitWidth = 242
        end
        inherited rdePCGMaxInner: TRbwDataEntry
          Top = 152
          Width = 242
          ExplicitTop = 152
          ExplicitWidth = 242
        end
        inherited comboPCGPrecondMeth: TJvImageComboBox
          Top = 180
          Width = 242
          Height = 26
          ItemHeight = 18
          ExplicitTop = 180
          ExplicitWidth = 242
          ExplicitHeight = 26
        end
        inherited rdePCGMaxHeadChange: TRbwDataEntry
          Top = 292
          Width = 242
          ExplicitTop = 292
          ExplicitWidth = 242
        end
        inherited rdePCGMaxResChange: TRbwDataEntry
          Top = 320
          Width = 242
          ExplicitTop = 320
          ExplicitWidth = 242
        end
        inherited rdePCGRelax: TRbwDataEntry
          Top = 348
          Width = 242
          ExplicitTop = 348
          ExplicitWidth = 242
        end
        inherited comboPCGEigenValue: TJvImageComboBox
          Top = 376
          Width = 242
          Height = 26
          ItemHeight = 18
          ExplicitTop = 376
          ExplicitWidth = 242
          ExplicitHeight = 26
        end
        inherited rdePCGPrintInt: TRbwDataEntry
          Top = 403
          Width = 242
          ExplicitTop = 403
          ExplicitWidth = 242
        end
        inherited comboPCGPrint: TJvImageComboBox
          Top = 431
          Width = 242
          Height = 26
          ItemHeight = 18
          ExplicitTop = 431
          ExplicitWidth = 242
          ExplicitHeight = 26
        end
        inherited rdePCGDamp: TRbwDataEntry
          Top = 458
          Width = 242
          ExplicitTop = 458
          ExplicitWidth = 242
        end
        inherited rdePCGDampPcgT: TRbwDataEntry
          Top = 486
          Width = 242
          ExplicitTop = 486
          ExplicitWidth = 242
        end
        inherited gbIHCOFADD: TGroupBox
          Top = 202
          Anchors = [akLeft, akBottom]
          ExplicitTop = 202
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
            end
            item
              Control = framePCG.rbIHCOFADD_0
            end
            item
              Control = framePCG.rbIHCOFADD_1
            end>
        end
      end
    end
    object jvspWEL: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'WEL_Well_Package_Pane'
      Caption = 'jvspWEL'
      inline framePkgWEL: TframePackageWell
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
        inherited lblPhiRamp: TLabel
          Width = 382
          Height = 18
          ExplicitWidth = 382
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
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
              Control = framePkgWEL.rdePhiRamp
            end
            item
              Control = frameWelParameterDefinition
            end>
        end
      end
      inline frameWelParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 161
        Width = 595
        Height = 353
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 353
        inherited pnlParameterCount: TPanel
          Top = 305
          Width = 595
          ExplicitTop = 305
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 299
          ExplicitWidth = 589
          ExplicitHeight = 299
        end
      end
    end
    object jvspRIV: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'RIV_River_Package'
      Caption = 'jvspRIV'
      inline framePkgRIV: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
          Width = 564
          ExplicitWidth = 564
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
        Width = 595
        Height = 353
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 353
        inherited pnlParameterCount: TPanel
          Top = 305
          Width = 595
          ExplicitTop = 305
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 299
          ExplicitWidth = 589
          ExplicitHeight = 299
        end
      end
    end
    object jvspDRN: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'DRN_Drain_Package_Pane'
      Caption = 'jvspDRN'
      inline framePkgDRN: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
          Width = 564
          ExplicitWidth = 564
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
        Width = 595
        Height = 353
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 353
        inherited pnlParameterCount: TPanel
          Top = 305
          Width = 595
          ExplicitTop = 305
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 299
          ExplicitWidth = 589
          ExplicitHeight = 299
        end
      end
    end
    object jvspDRT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'DRT_Drain_Return_Package_Pane'
      Caption = 'jvspDRT'
      inline framePkgDRT: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
          Width = 564
          ExplicitWidth = 564
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
        Width = 595
        Height = 353
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 353
        inherited pnlParameterCount: TPanel
          Top = 305
          Width = 595
          ExplicitTop = 305
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 299
          ExplicitWidth = 589
          ExplicitHeight = 299
        end
      end
    end
    object jvspRCH: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'RCH_Recharge_Package_Pane'
      Caption = 'jvspRCH'
      inline framePkgRCH: TframePackageRCH
        Left = 0
        Top = 0
        Width = 595
        Height = 249
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 249
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
          Height = 81
          ExplicitWidth = 574
          ExplicitHeight = 81
        end
        inherited pnLayerOption: TPanel
          Top = 149
          Width = 595
          ExplicitTop = 149
          ExplicitWidth = 595
          inherited lblLayerOption: TLabel
            Width = 173
            Height = 18
            Caption = 'Recharge location option'
            ExplicitWidth = 173
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited cbTimeVaryingLayers: TCheckBox
            Caption = 'Time varying recharge layers'
          end
          inherited rgAssignmentMethod: TRadioGroup
            Width = 574
            ExplicitWidth = 574
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
              Control = framePkgRCH.rgAssignmentMethod
            end
            item
              Control = frameRchParameterDefinition
            end>
          OnEnabledChange = framePkgRCHrcSelectionControllerEnabledChange
        end
      end
      inline frameRchParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 249
        Width = 595
        Height = 265
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 249
        ExplicitWidth = 595
        ExplicitHeight = 265
        inherited pnlParameterCount: TPanel
          Top = 217
          Width = 595
          ExplicitTop = 217
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 211
          ExplicitWidth = 589
          ExplicitHeight = 211
        end
      end
    end
    object jvspEVT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'EVT_Evapotranspiration_Package'
      Caption = 'jvspEVT'
      inline framePkgEVT: TframePackageTransientLayerChoice
        Left = 0
        Top = 0
        Width = 595
        Height = 201
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
        inherited pnLayerOption: TPanel
          Width = 595
          ExplicitWidth = 595
          inherited lblLayerOption: TLabel
            Width = 136
            Height = 18
            Caption = 'EVT location option'
            ExplicitWidth = 136
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
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
        Width = 595
        Height = 313
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 201
        ExplicitWidth = 595
        ExplicitHeight = 313
        inherited pnlParameterCount: TPanel
          Top = 265
          Width = 595
          ExplicitTop = 265
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 259
          ExplicitWidth = 589
          ExplicitHeight = 259
        end
      end
    end
    object jvspETS: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'ETS_Evapotranspiration_Segments_Package'
      Caption = 'jvspETS'
      inline framePkgETS: TframeEtsPackage
        Left = 0
        Top = 0
        Width = 595
        Height = 226
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
        inherited pnLayerOption: TPanel
          Width = 595
          ExplicitWidth = 595
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
        Width = 595
        Height = 288
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 226
        ExplicitWidth = 595
        ExplicitHeight = 288
        inherited pnlParameterCount: TPanel
          Top = 240
          Width = 595
          ExplicitTop = 240
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 234
          ExplicitWidth = 589
          ExplicitHeight = 234
        end
      end
    end
    object jvspRES: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'RES_Reservoir_Package_Pane'
      Caption = 'jvspRES'
      inline framePkgRES: TframePackageRes
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Height = 367
          ExplicitWidth = 574
          ExplicitHeight = 367
        end
        inherited pnLayerOption: TPanel
          Top = 438
          Width = 595
          ExplicitTop = 438
          ExplicitWidth = 595
          inherited lblLayerOption: TLabel
            Top = 5
            Width = 172
            Height = 18
            ExplicitTop = 5
            ExplicitWidth = 172
            ExplicitHeight = 18
          end
          inherited lblTableSize: TLabel
            Top = 54
            Width = 469
            Height = 18
            ExplicitTop = 54
            ExplicitWidth = 469
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited cbPrintStage: TCheckBox
            Width = 561
            Height = 18
            ExplicitWidth = 561
            ExplicitHeight = 18
          end
          inherited seTableSize: TJvSpinEdit
            Top = 51
            Height = 26
            ExplicitTop = 51
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'LAK_Lake_Package_Pane'
      Caption = 'jvspLAK'
      inline framePkgLAK: TframePackageLAK
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblTheta: TLabel
          Left = 168
          Top = 295
          Width = 39
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 306
          ExplicitWidth = 39
          ExplicitHeight = 18
        end
        inherited lblIterations: TLabel
          Left = 168
          Top = 323
          Width = 286
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 334
          ExplicitWidth = 286
          ExplicitHeight = 18
        end
        inherited lblConvergenceCriterion: TLabel
          Left = 168
          Top = 351
          Width = 235
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 362
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
          Top = 375
          Width = 340
          Height = 18
          ExplicitLeft = 168
          ExplicitTop = 386
          ExplicitWidth = 340
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 224
          ExplicitWidth = 574
          ExplicitHeight = 224
        end
        inherited rdeTheta: TRbwDataEntry
          Top = 292
          Width = 146
          ExplicitTop = 292
          ExplicitWidth = 146
        end
        inherited rdeIterations: TRbwDataEntry
          Top = 320
          Width = 146
          ExplicitTop = 320
          ExplicitWidth = 146
        end
        inherited rdeConvergenceCriterion: TRbwDataEntry
          Top = 348
          Width = 146
          ExplicitTop = 348
          ExplicitWidth = 146
        end
        inherited cbPrintLake: TCheckBox
          Top = 400
          ExplicitTop = 400
        end
        inherited rdeSurfDepth: TRbwDataEntry
          Top = 372
          Width = 146
          ExplicitTop = 372
          ExplicitWidth = 146
        end
        inherited rgBathymetry: TRadioGroup
          Top = 423
          Width = 345
          Height = 75
          ExplicitTop = 423
          ExplicitWidth = 345
          ExplicitHeight = 75
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
            end
            item
              Control = framePkgLAK.rgBathymetry
            end>
        end
      end
    end
    object jvspSFR: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SFR_Stream_Flow_Routing_Package'
      Caption = 'jvspSFR'
      object pcSFR: TJvPageControl
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        ActivePage = tabSfrGeneral
        Align = alClient
        TabOrder = 0
        ClientBorderWidth = 0
        object tabSfrGeneral: TTabSheet
          Caption = 'General'
          inline framePkgSFR: TframePackageSFR
            Left = 0
            Top = 0
            Width = 595
            Height = 489
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 595
            ExplicitHeight = 489
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
              Top = 168
              Width = 161
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 231
              ExplicitTop = 179
              ExplicitWidth = 161
              ExplicitHeight = 18
            end
            inherited lblStreamTolerance: TLabel
              Left = 112
              Top = 269
              Width = 187
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 269
              ExplicitWidth = 187
              ExplicitHeight = 18
            end
            inherited lblSfrTrailingWaveIncrements: TLabel
              Left = 112
              Top = 293
              Width = 321
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 304
              ExplicitWidth = 321
              ExplicitHeight = 18
            end
            inherited lblSfrMaxTrailingWaves: TLabel
              Left = 112
              Top = 316
              Width = 337
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 327
              ExplicitWidth = 337
              ExplicitHeight = 18
            end
            inherited lblSfrMaxUnsatCells: TLabel
              Left = 112
              Top = 339
              Width = 424
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 350
              ExplicitWidth = 424
              ExplicitHeight = 18
            end
            inherited lblNUMTIM: TLabel
              Left = 112
              Top = 385
              Width = 451
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 396
              ExplicitWidth = 451
              ExplicitHeight = 18
            end
            inherited lblWeight: TLabel
              Left = 112
              Top = 407
              Width = 441
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 407
              ExplicitWidth = 441
              ExplicitHeight = 18
            end
            inherited lblFLWTOL: TLabel
              Left = 112
              Top = 429
              Width = 410
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 112
              ExplicitTop = 429
              ExplicitWidth = 410
              ExplicitHeight = 18
            end
            inherited memoComments: TMemo
              Left = 20
              Width = 582
              Height = 48
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitLeft = 20
              ExplicitWidth = 582
              ExplicitHeight = 48
            end
            inherited cbSfrUnsatflow: TCheckBox95
              Left = 6
              Top = 116
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRcbSfrUnsatflowClick
              ExplicitLeft = 6
              ExplicitTop = 116
            end
            inherited cbSfrLpfHydraulicCond: TCheckBox95
              Left = 6
              Top = 136
              Width = 356
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRcbSfrLpfHydraulicCondClick
              ExplicitLeft = 6
              ExplicitTop = 136
              ExplicitWidth = 356
            end
            inherited rgSfr2ISFROPT: TRadioGroup
              Left = 6
              Top = 197
              Anchors = [akLeft, akBottom]
              OnClick = framePkgSFRrgSfr2ISFROPTClick
              ExplicitLeft = 6
              ExplicitTop = 197
            end
            inherited comboPrintStreams: TComboBox
              Left = 6
              Top = 165
              Width = 219
              Height = 26
              Anchors = [akLeft, akBottom]
              ItemIndex = 1
              Text = 'Print flows in listing file'
              ExplicitLeft = 6
              ExplicitTop = 165
              ExplicitWidth = 219
              ExplicitHeight = 26
            end
            inherited cbGage8: TCheckBox
              Left = 6
              Top = 450
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 450
            end
            inherited rdeDLEAK: TRbwDataEntry
              Left = 6
              Top = 269
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 269
            end
            inherited rdeNstrail: TRbwDataEntry
              Left = 6
              Top = 293
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 293
              ExplicitHeight = 18
            end
            inherited rdeNsfrsets: TRbwDataEntry
              Left = 6
              Top = 316
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 316
              ExplicitHeight = 18
            end
            inherited rdeIsuzn: TRbwDataEntry
              Left = 6
              Top = 339
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 339
              ExplicitHeight = 18
            end
            inherited cbIRTFLG: TCheckBox
              Left = 6
              Top = 359
              Width = 555
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 359
              ExplicitWidth = 555
            end
            inherited rdeNUMTIM: TRbwDataEntry
              Left = 6
              Top = 385
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 385
              ExplicitHeight = 18
            end
            inherited rdeWeight: TRbwDataEntry
              Left = 6
              Top = 407
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 407
              ExplicitHeight = 18
            end
            inherited rdeFLWTOL: TRbwDataEntry
              Left = 6
              Top = 429
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 6
              ExplicitTop = 429
              ExplicitHeight = 18
            end
            inherited cbUseGsflowFormat: TCheckBox
              Left = 408
              Top = 169
              ExplicitLeft = 408
              ExplicitTop = 169
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
                end
                item
                  Control = framePkgSFR.cbUseGsflowFormat
                end>
              OnEnabledChange = framePkgSFRrcSelectionControllerEnabledChange
            end
          end
        end
        object tabSfrParameters: TTabSheet
          Caption = 'Parameters'
          ImageIndex = 1
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object splitSFR: TSplitter
            Left = 0
            Top = 257
            Width = 595
            Height = 5
            Cursor = crVSplit
            Align = alTop
            ExplicitWidth = 603
          end
          inline frameSFRParameterDefinition: TframeListParameterDefinition
            Left = 0
            Top = 0
            Width = 595
            Height = 257
            Align = alTop
            Enabled = False
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 595
            ExplicitHeight = 257
            inherited pnlParameterCount: TPanel
              Top = 209
              Width = 595
              ExplicitTop = 209
              ExplicitWidth = 595
              inherited lblNumParameters: TLabel
                Width = 192
                Height = 18
                Caption = 'Number of SFR parameters'
                ExplicitWidth = 192
                ExplicitHeight = 18
              end
              inherited btnDelete: TBitBtn
                Left = 481
                OnClick = frameSFRParameterDefinitionbtnDeleteClick
                ExplicitLeft = 481
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
              Width = 589
              Height = 203
              OnSelectCell = frameSFRParameterDefinitiondgParametersSelectCell
              OnSetEditText = frameSFRParameterDefinitiondgParametersSetEditText
              ExplicitWidth = 589
              ExplicitHeight = 203
              ColWidths = (
                64
                64)
            end
          end
          object jplSfrParameters: TJvPageList
            Left = 0
            Top = 262
            Width = 595
            Height = 227
            PropagateEnable = False
            Align = alClient
          end
        end
      end
    end
    object jvspUZF: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'UZF_Unsaturated_Zone_Flow_Package'
      Caption = 'jvspUZF'
      inline framePkgUZF: TframePackageUZF
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        DesignSize = (
          595
          514)
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
          Width = 574
          Height = 69
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 574
          ExplicitHeight = 69
        end
        inherited pnLayerOption: TPanel
          Top = 137
          Width = 595
          Height = 377
          ExplicitTop = 137
          ExplicitWidth = 595
          ExplicitHeight = 377
          inherited lblLayerOption: TLabel
            Width = 357
            Height = 18
            Caption = 'Recharge and discharge location option (NUZTOP) '
            ExplicitWidth = 357
            ExplicitHeight = 18
          end
          inherited lblVerticalKSource: TLabel
            Top = 56
            Width = 335
            Height = 18
            Caption = 'Vertical hydraulic conductivity source (IUZFOPT) '
            ExplicitTop = 56
            ExplicitWidth = 335
            ExplicitHeight = 18
          end
          inherited lblNumberOfTrailingWaves: TLabel
            Top = 111
            Width = 251
            Height = 18
            Caption = 'Number of trailing waves (NTRAIL2) '
            ExplicitTop = 111
            ExplicitWidth = 251
            ExplicitHeight = 18
          end
          inherited lblNumberOfWaveSets: TLabel
            Top = 160
            Width = 225
            Height = 18
            Caption = 'Number of wave sets (NSETS2) '
            ExplicitTop = 160
            ExplicitWidth = 225
            ExplicitHeight = 18
          end
          inherited lblSURFDEP: TLabel
            Top = 322
            Width = 506
            Height = 18
            ExplicitTop = 322
            ExplicitWidth = 506
            ExplicitHeight = 18
          end
          inherited comboLayerOption: TComboBox
            Top = 26
            Height = 26
            ExplicitTop = 26
            ExplicitHeight = 26
          end
          inherited comboVerticalKSource: TComboBox
            Top = 79
            Width = 449
            Height = 26
            ExplicitTop = 79
            ExplicitWidth = 449
            ExplicitHeight = 26
          end
          inherited rdeNumberOfTrailingWaves: TRbwDataEntry
            Top = 132
            ExplicitTop = 132
          end
          inherited rdeNumberOfWaveSets: TRbwDataEntry
            Top = 181
            ExplicitTop = 181
          end
          inherited rdeSURFDEP: TRbwDataEntry
            Top = 343
            ExplicitTop = 343
          end
          inherited chklstOptions: TCheckListBox
            Top = 209
            Width = 584
            Height = 107
            ItemHeight = 18
            ExplicitTop = 209
            ExplicitWidth = 584
            ExplicitHeight = 107
          end
          inherited rgAssignmentMethod: TRadioGroup
            Left = 288
            Top = 111
            Width = 305
            Height = 92
            ExplicitLeft = 288
            ExplicitTop = 111
            ExplicitWidth = 305
            ExplicitHeight = 92
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
              Control = framePkgUZF.chklstOptions
            end
            item
              Control = framePkgUZF.comboLayerOption
            end
            item
              Control = framePkgUZF.lblLayerOption
            end
            item
              Control = framePkgUZF.rdeSURFDEP
            end
            item
              Control = framePkgUZF.lblSURFDEP
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
              Control = framePkgUZF.lblNumberOfWaveSets
            end
            item
              Control = framePkgUZF.comboVerticalKSource
            end
            item
              Control = framePkgUZF.lblVerticalKSource
            end
            item
              Control = framePkgUZF.rgAssignmentMethod
            end>
          OnEnabledChange = framePkgUZFrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspGMG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'GMG_Geometric_Multigrid_Package'
      Caption = 'jvspGMG'
      inline framePkgGMG: TframeGMG
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        DesignSize = (
          595
          514)
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
          Height = 158
          ExplicitWidth = 574
          ExplicitHeight = 158
        end
        inherited pcGMG: TJvPageControl
          Top = 214
          Width = 595
          Height = 300
          ActivePage = framePkgGMG.tabDampRelax
          ExplicitTop = 214
          ExplicitWidth = 595
          ExplicitHeight = 300
          inherited tabControlAndPrint: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 617
            ExplicitHeight = 225
            DesignSize = (
              617
              225)
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
              Left = 366
              Top = 7
              ExplicitLeft = 366
              ExplicitTop = 7
            end
            inherited rdeGmgIiter: TRbwDataEntry
              Left = 366
              Top = 39
              ExplicitLeft = 366
              ExplicitTop = 39
            end
            inherited rdeGmgHclose: TRbwDataEntry
              Left = 366
              Top = 71
              ExplicitLeft = 366
              ExplicitTop = 71
            end
            inherited rdeGmgMxiter: TRbwDataEntry
              Left = 366
              Top = 103
              ExplicitLeft = 366
              ExplicitTop = 103
            end
            inherited comboGmgIoutgmg: TJvImageComboBox
              Left = 304
              Top = 168
              Width = 239
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 168
              ExplicitWidth = 239
              ExplicitHeight = 28
            end
            inherited cbGmbIunitmhc: TCheckBox
              Top = 205
              ExplicitTop = 205
            end
            inherited comboGmgIsm: TJvImageComboBox
              Left = 304
              Top = 139
              Width = 239
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 139
              ExplicitWidth = 239
              ExplicitHeight = 28
            end
          end
          inherited tabDampRelax: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 267
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
              ExplicitTop = 73
            end
            inherited rdeGmgRelax: TRbwDataEntry
              Top = 200
              ExplicitTop = 200
            end
            inherited rdeGmgChglimit: TRbwDataEntry
              Top = 136
              ExplicitTop = 136
            end
            inherited rdeGmgDlow: TRbwDataEntry
              Top = 105
              ExplicitTop = 105
            end
            inherited comboGmgIadamp: TJvImageComboBox
              Left = 304
              Top = 41
              Width = 209
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitTop = 41
              ExplicitWidth = 209
              ExplicitHeight = 28
            end
            inherited comboGmgIsc: TJvImageComboBox
              Left = 304
              Width = 209
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 304
              ExplicitWidth = 209
              ExplicitHeight = 28
            end
            inherited rdeGmgDamp: TRbwDataEntry
              Top = 9
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SIP_Strongly_Implicit_Procedure_Package'
      Caption = 'jvspSIP'
      inline framePkgSIP: TframeSIP
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        DesignSize = (
          595
          514)
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
          Top = 181
          Width = 246
          Height = 18
          ExplicitTop = 170
          ExplicitWidth = 246
          ExplicitHeight = 18
        end
        inherited lblSipNparm: TLabel
          Top = 213
          Width = 269
          Height = 18
          ExplicitTop = 213
          ExplicitWidth = 269
          ExplicitHeight = 18
        end
        inherited lblSipAccl: TLabel
          Top = 245
          Width = 204
          Height = 18
          ExplicitTop = 234
          ExplicitWidth = 204
          ExplicitHeight = 18
        end
        inherited lblSipHclose: TLabel
          Top = 277
          Width = 233
          Height = 18
          ExplicitTop = 266
          ExplicitWidth = 233
          ExplicitHeight = 18
        end
        inherited lblSipIpcalc: TLabel
          Top = 309
          Width = 161
          Height = 18
          ExplicitTop = 298
          ExplicitWidth = 161
          ExplicitHeight = 18
        end
        inherited lblSipWseed: TLabel
          Top = 341
          Width = 329
          Height = 36
          WordWrap = True
          ExplicitTop = 352
          ExplicitWidth = 329
          ExplicitHeight = 36
        end
        inherited lblSipIprsip: TLabel
          Top = 389
          Width = 170
          Height = 18
          ExplicitTop = 400
          ExplicitWidth = 170
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 110
          ExplicitWidth = 574
          ExplicitHeight = 110
        end
        inherited rdeSipMxiter: TRbwDataEntry
          Left = 417
          Top = 178
          ExplicitLeft = 417
          ExplicitTop = 178
        end
        inherited rdeSipNparm: TRbwDataEntry
          Left = 417
          Top = 206
          ExplicitLeft = 417
          ExplicitTop = 206
        end
        inherited rdeSipAccl: TRbwDataEntry
          Left = 417
          Top = 242
          ExplicitLeft = 417
          ExplicitTop = 242
        end
        inherited rdeSipHclose: TRbwDataEntry
          Left = 417
          Top = 270
          ExplicitLeft = 417
          ExplicitTop = 270
        end
        inherited comboSipIpcalc: TJvImageComboBox
          Left = 326
          Top = 298
          Height = 28
          ItemHeight = 22
          ExplicitLeft = 326
          ExplicitTop = 298
          ExplicitHeight = 28
        end
        inherited rdeSipWseed: TRbwDataEntry
          Left = 417
          Top = 334
          ExplicitLeft = 417
          ExplicitTop = 334
        end
        inherited rdeSipIprsip: TRbwDataEntry
          Left = 417
          Top = 382
          ExplicitLeft = 417
          ExplicitTop = 382
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'DE4_Direct_Solver_Package_Pane'
      Caption = 'jvspDE4'
      inline framePkgDE4: TframeDE4
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Top = 189
          Width = 258
          Height = 18
          ExplicitTop = 178
          ExplicitWidth = 258
          ExplicitHeight = 18
        end
        inherited lblDe4Mxup: TLabel
          Top = 221
          Width = 363
          Height = 18
          ExplicitTop = 210
          ExplicitWidth = 363
          ExplicitHeight = 18
        end
        inherited lblDe4Mxlow: TLabel
          Top = 249
          Width = 374
          Height = 18
          ExplicitTop = 238
          ExplicitWidth = 374
          ExplicitHeight = 18
        end
        inherited lblDe4Mxbw: TLabel
          Top = 281
          Width = 210
          Height = 18
          ExplicitTop = 270
          ExplicitWidth = 210
          ExplicitHeight = 18
        end
        inherited lblDe4Ifreq: TLabel
          Top = 309
          Width = 275
          Height = 36
          WordWrap = True
          ExplicitTop = 320
          ExplicitWidth = 275
          ExplicitHeight = 36
        end
        inherited lblDe4Mutd4: TLabel
          Top = 357
          Width = 150
          Height = 18
          ExplicitTop = 368
          ExplicitWidth = 150
          ExplicitHeight = 18
        end
        inherited lblDe4Accl: TLabel
          Top = 385
          Width = 221
          Height = 18
          ExplicitTop = 396
          ExplicitWidth = 221
          ExplicitHeight = 18
        end
        inherited lblDe4Hclose: TLabel
          Top = 417
          Width = 287
          Height = 18
          ExplicitTop = 428
          ExplicitWidth = 287
          ExplicitHeight = 18
        end
        inherited lblRdeIprd4: TLabel
          Top = 445
          Width = 166
          Height = 18
          ExplicitTop = 456
          ExplicitWidth = 166
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 110
          ExplicitWidth = 574
          ExplicitHeight = 110
        end
        inherited rdeDe4Itmx: TRbwDataEntry
          Left = 439
          Top = 186
          ExplicitLeft = 439
          ExplicitTop = 186
        end
        inherited rdeDe4Mxup: TRbwDataEntry
          Left = 439
          Top = 218
          ExplicitLeft = 439
          ExplicitTop = 218
        end
        inherited rdeDe4Mxlow: TRbwDataEntry
          Left = 439
          Top = 246
          ExplicitLeft = 439
          ExplicitTop = 246
        end
        inherited rdeDe4Mxbw: TRbwDataEntry
          Left = 439
          Top = 278
          ExplicitLeft = 439
          ExplicitTop = 278
        end
        inherited comboDe4Ifreq: TJvImageComboBox
          Left = 344
          Top = 305
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
          ExplicitLeft = 344
          ExplicitTop = 305
          ExplicitWidth = 239
          ExplicitHeight = 28
        end
        inherited comboDe4Mutd4: TJvImageComboBox
          Left = 344
          Top = 354
          Width = 241
          Height = 28
          DroppedWidth = 272
          ItemHeight = 22
          ExplicitLeft = 344
          ExplicitTop = 354
          ExplicitWidth = 241
          ExplicitHeight = 28
        end
        inherited rdeDe4Accl: TRbwDataEntry
          Left = 439
          Top = 382
          ExplicitLeft = 439
          ExplicitTop = 382
        end
        inherited rdeDe4Hclose: TRbwDataEntry
          Left = 439
          Top = 414
          ExplicitLeft = 439
          ExplicitTop = 414
        end
        inherited rdeRdeIprd4: TRbwDataEntry
          Left = 439
          Top = 442
          ExplicitLeft = 439
          ExplicitTop = 442
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'HOB_Head_Observation_Package'
      Caption = 'jvspHOB'
      inline framePkgHOB: TframePackageHob
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Top = 464
          Width = 252
          Height = 18
          ExplicitTop = 453
          ExplicitWidth = 252
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 574
          Height = 389
          ExplicitWidth = 574
          ExplicitHeight = 389
        end
        inherited rdeDryHead: TRbwDataEntry
          Top = 483
          ExplicitTop = 483
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'HFB_Horizontal_Flow_Barrier_Package'
      Caption = 'jvspHFB'
      inline framePkgHFB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 161
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
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
          Width = 564
          ExplicitWidth = 564
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
        Width = 595
        Height = 353
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 161
        ExplicitWidth = 595
        ExplicitHeight = 353
        inherited pnlParameterCount: TPanel
          Top = 305
          Width = 595
          ExplicitTop = 305
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 481
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 481
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            Enabled = True
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 299
          ExplicitWidth = 589
          ExplicitHeight = 299
        end
      end
    end
    object jvspModpath: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'MODPATH'
      Caption = 'jvspModpath'
      inline frameModpath: TframeModpathSelection
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 576
          ExplicitWidth = 576
        end
        inherited pcModpath: TPageControl
          Width = 590
          Height = 375
          ExplicitWidth = 590
          ExplicitHeight = 375
          inherited tabResponse: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 582
            ExplicitHeight = 342
            inherited lblTrackingDirection: TLabel
              Left = 359
              Top = 257
              Width = 132
              Height = 36
              WordWrap = True
              ExplicitLeft = 359
              ExplicitTop = 257
              ExplicitWidth = 132
              ExplicitHeight = 36
            end
            inherited lblWeakSinkTreatment: TLabel
              Width = 299
              Height = 18
              ExplicitWidth = 299
              ExplicitHeight = 18
            end
            inherited lblWeakSinkThreshold: TLabel
              Top = 309
              Width = 207
              Height = 18
              ExplicitTop = 309
              ExplicitWidth = 207
              ExplicitHeight = 18
            end
            inherited lblStopZone: TLabel
              Top = 225
              Width = 290
              Height = 18
              ExplicitTop = 225
              ExplicitWidth = 290
              ExplicitHeight = 18
            end
            inherited lblWhichEndpoints: TLabel
              Top = 166
              Width = 262
              Height = 18
              ExplicitTop = 166
              ExplicitWidth = 262
              ExplicitHeight = 18
            end
            inherited lblReferenceTime: TLabel
              Left = 335
              Top = 118
              Width = 206
              Height = 36
              ExplicitLeft = 335
              ExplicitTop = 118
              ExplicitWidth = 206
              ExplicitHeight = 36
            end
            inherited lblEvtSink: TLabel
              Width = 287
              Height = 18
              ExplicitWidth = 287
              ExplicitHeight = 18
            end
            inherited lblRchSource: TLabel
              Width = 223
              Height = 18
              ExplicitWidth = 223
              ExplicitHeight = 18
            end
            inherited comboTrackingDirection: TJvImageComboBox
              Left = 359
              Top = 296
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 359
              ExplicitTop = 296
              ExplicitHeight = 28
            end
            inherited comboWeakSinkTreatment: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited cbStopInZone: TCheckBox
              Width = 288
              ExplicitWidth = 288
            end
            inherited rdeStopZone: TRbwDataEntry
              Top = 224
              TabOrder = 7
              ExplicitTop = 224
            end
            inherited comboWhichEndpoints: TJvImageComboBox
              Top = 190
              Height = 28
              ItemHeight = 22
              ExplicitTop = 190
              ExplicitHeight = 28
            end
            inherited rdeReferenceTime: TRbwDataEntry
              Left = 335
              Top = 162
              TabOrder = 5
              ExplicitLeft = 335
              ExplicitTop = 162
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
          end
          inherited tabVersion5Options: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 331
            inherited lblBeginningTime: TLabel
              Width = 288
              Height = 18
              ExplicitWidth = 288
              ExplicitHeight = 18
            end
            inherited lblEndingTime: TLabel
              Width = 241
              Height = 18
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
            inherited lblMaxSize: TLabel
              Width = 386
              Height = 36
              ExplicitWidth = 386
              ExplicitHeight = 36
            end
            inherited lblErrorTolerance: TLabel
              Width = 103
              Height = 18
              ExplicitWidth = 103
              ExplicitHeight = 18
            end
            inherited lblMaxTime: TLabel
              Width = 160
              Height = 18
              ExplicitWidth = 160
              ExplicitHeight = 18
            end
            inherited lblReleaseTime: TLabel
              Width = 241
              Height = 18
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
          end
          inherited tsVersion6Options: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 331
            inherited lblWeakSource: TLabel
              Width = 337
              Height = 18
              ExplicitWidth = 337
              ExplicitHeight = 18
            end
            inherited lblStopOption: TLabel
              Width = 252
              Height = 18
              Caption = 'End of particle tracking (StopOption)'
              ExplicitWidth = 252
              ExplicitHeight = 18
            end
            inherited lblStopTime: TLabel
              Top = 123
              Width = 241
              Height = 18
              ExplicitTop = 123
              ExplicitWidth = 241
              ExplicitHeight = 18
            end
            inherited lblBudget: TLabel
              Width = 271
              Height = 18
              ExplicitWidth = 271
              ExplicitHeight = 18
            end
            inherited lblTraceID: TLabel
              Width = 176
              Height = 18
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited lblAdvObs: TLabel
              Left = 3
              Width = 216
              Height = 36
              ExplicitLeft = 3
              ExplicitWidth = 216
              ExplicitHeight = 36
            end
            inherited lblUzfIface: TLabel
              Width = 159
              Height = 18
              ExplicitWidth = 159
              ExplicitHeight = 18
            end
            inherited lblMnw2Iface: TLabel
              Width = 177
              Height = 18
              ExplicitWidth = 177
              ExplicitHeight = 18
            end
            inherited lblResIface: TLabel
              Width = 196
              Height = 18
              ExplicitWidth = 196
              ExplicitHeight = 18
            end
            inherited lblSfrIface: TLabel
              Width = 161
              Height = 18
              ExplicitWidth = 161
              ExplicitHeight = 18
            end
            inherited lblEtsIface: TLabel
              Width = 160
              Height = 18
              ExplicitWidth = 160
              ExplicitHeight = 18
            end
            inherited lblLakIface: TLabel
              Width = 164
              Height = 18
              ExplicitWidth = 164
              ExplicitHeight = 18
            end
            inherited comboWeakSource: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboStopOption: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboBudget: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited chkRetardation: TCheckBox
              Width = 302
              ExplicitWidth = 302
            end
            inherited comboAdvObs: TJvImageComboBox
              Left = 3
              Top = 292
              Height = 28
              ItemHeight = 22
              TabOrder = 11
              ExplicitLeft = 3
              ExplicitTop = 292
              ExplicitHeight = 28
            end
            inherited comboUzfIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboMnw2Iface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboResIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboSfrIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboEtsIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboLakIface: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              TabOrder = 12
              ExplicitHeight = 28
            end
          end
          inherited tabOutputTimes: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 331
            inherited lblTimeMethod: TLabel
              Width = 315
              Height = 18
              ExplicitWidth = 315
              ExplicitHeight = 18
            end
            inherited lblParticleInterval: TLabel
              Width = 328
              Height = 18
              ExplicitWidth = 328
              ExplicitHeight = 18
            end
            inherited lblMaxTimes: TLabel
              Width = 243
              Height = 36
              ExplicitWidth = 243
              ExplicitHeight = 36
            end
            inherited gbTime: TJvGroupBox
              inherited lblTimeCount: TLabel
                Width = 121
                Height = 36
                ExplicitWidth = 121
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
              Top = 94
              ExplicitTop = 94
            end
            inherited rdeMaxTimes: TRbwDataEntry
              Top = 167
              ExplicitTop = 167
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
              Control = frameModpath.cbBinary
            end
            item
              Control = frameModpath.cbCompact
            end
            item
              Control = frameModpath.rdeBeginningTime
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
              Control = frameModpath.cbStopAfterMaxTime
            end
            item
              Control = frameModpath.cbBigBudget
            end
            item
              Control = frameModpath.cbSummarize
            end
            item
              Control = frameModpath.cbComputeBudget
            end
            item
              Control = frameModpath.rgModpathVersion
            end
            item
              Control = frameModpath.comboWeakSource
            end
            item
              Control = frameModpath.comboStopOption
            end
            item
              Control = frameModpath.comboBudget
            end
            item
              Control = frameModpath.chkRetardation
            end
            item
              Control = frameModpath.comboAdvObs
            end
            item
              Control = frameModpath.comboEtsIface
            end
            item
              Control = frameModpath.comboUzfIface
            end
            item
              Control = frameModpath.comboMnw2Iface
            end
            item
              Control = frameModpath.comboResIface
            end
            item
              Control = frameModpath.comboSfrIface
            end
            item
              Control = frameModpath.comboLakIface
            end>
          OnEnabledChange = frameModpathrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspCHOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'CHOB_Specified_Head_Flow_Observations'
      Caption = 'jvspCHOB'
      inline framePkgCHOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 446
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 446
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'DROB_Drain_Observation_Package'
      Caption = 'jvspDROB'
      inline framePkgDROB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 446
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 446
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'GBOB_General_Head_Boundary_Observations'
      Caption = 'jvspGBOB'
      inline framePkgGBOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 446
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 446
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'RVOB_River_Observation_Package'
      Caption = 'jvspRVOB'
      inline framePkgRVOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 446
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExplicitWidth = 564
          ExplicitHeight = 446
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'MNW2_Multi_Node_Well_Package'
      Caption = 'jvspMNW2'
      inline framePkgMnw2: TframePackageMnw2
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Top = 356
          Width = 174
          Height = 18
          ExplicitTop = 345
          ExplicitWidth = 174
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 288
          ExplicitWidth = 564
          ExplicitHeight = 288
        end
        inherited comboPrintOption: TJvImageComboBox
          Top = 379
          Height = 28
          ItemHeight = 22
          ExplicitTop = 379
          ExplicitHeight = 28
        end
        inherited gbMnwiOptions: TGroupBox
          Top = 413
          Width = 564
          ExplicitTop = 413
          ExplicitWidth = 564
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'BCF_Block-Centered_Flow_Package'
      Caption = 'jvspBCF'
      inline framePkgBCF: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 435
          ExplicitWidth = 564
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
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SUB_Subsidence_and_Aquifer_Sys'
      Caption = 'jvspSUB'
      inline framePkgSUB: TframePackageSub
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcSub: TPageControl
          Width = 595
          Height = 395
          ActivePage = framePkgSUB.tabControls
          ExplicitWidth = 595
          ExplicitHeight = 395
          inherited tabControls: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 362
            inherited lblNumberOfNodes: TLabel
              Width = 287
              Height = 18
              ExplicitWidth = 287
              ExplicitHeight = 18
            end
            inherited lblAccel1: TLabel
              Width = 238
              Height = 18
              ExplicitWidth = 238
              ExplicitHeight = 18
            end
            inherited lblAccel2: TLabel
              Width = 265
              Height = 18
              ExplicitWidth = 265
              ExplicitHeight = 18
            end
            inherited lblMinIterations: TLabel
              Width = 353
              Height = 18
              ExplicitWidth = 353
              ExplicitHeight = 18
            end
            inherited lbReadRestart: TLabel
              Width = 380
              Height = 18
              ExplicitWidth = 380
              ExplicitHeight = 18
            end
            inherited lblOutputChoice: TLabel
              Width = 140
              Height = 18
              ExplicitWidth = 140
              ExplicitHeight = 18
            end
            inherited seNumberOfNodes: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited feReadRestart: TJvFilenameEdit
              Left = 14
              Top = 163
              Width = 569
              Height = 26
              ExplicitLeft = 14
              ExplicitTop = 163
              ExplicitWidth = 569
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
          inherited tabPrintSave: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 436
            ExplicitHeight = 250
            inherited lblNumExportPeriods: TLabel
              Left = 67
              Top = 348
              Width = 176
              Height = 18
              ExplicitLeft = 67
              ExplicitTop = 326
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited sbAdd: TSpeedButton
              Left = 512
              Top = 344
              ExplicitLeft = 512
              ExplicitTop = 322
            end
            inherited sbInsert: TSpeedButton
              Left = 541
              Top = 344
              ExplicitLeft = 541
              ExplicitTop = 322
            end
            inherited sbDelete: TSpeedButton
              Left = 570
              Top = 344
              ExplicitLeft = 570
              ExplicitTop = 322
            end
            inherited cbMultiPrintSave: TCheckBox
              Width = 209
              ExplicitWidth = 209
            end
            inherited rdgOutput: TRbwDataGrid4
              Top = 40
              Width = 581
              Height = 298
              ExplicitTop = 40
              ExplicitWidth = 581
              ExplicitHeight = 298
            end
            inherited seNumExportPeriods: TJvSpinEdit
              Left = 4
              Top = 344
              Height = 26
              ExplicitLeft = 4
              ExplicitTop = 344
              ExplicitHeight = 26
            end
            inherited comboMultiFomat: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
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
            end
            item
              Control = framePkgSUB.comboOutputChoice
            end
            item
              Control = framePkgSUB.lblOutputChoice
            end>
        end
      end
    end
    object jvspZoneBudget: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'ZONEBUDGET'
      Caption = 'jvspZoneBudget'
      inline frameZoneBudget: TframeZoneBudget
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        HelpType = htKeyword
        HelpKeyword = 'ZONEBUDGET'
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        inherited lblComments: TLabel
          Width = 205
          Height = 18
          ExplicitWidth = 205
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblCompositeZones: TLabel
          Width = 122
          Height = 18
          ExplicitWidth = 122
          ExplicitHeight = 18
        end
        inherited lblNumberOfZones: TLabel
          Top = 479
          Width = 194
          Height = 18
          Caption = 'Number of composite zones'
          ExplicitTop = 468
          ExplicitWidth = 194
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rdgCompositeZones: TRbwDataGrid4
          Width = 564
          Height = 262
          ExplicitWidth = 564
          ExplicitHeight = 262
        end
        inherited seNumberOfZones: TJvSpinEdit
          Top = 476
          Height = 26
          ExplicitTop = 476
          ExplicitHeight = 26
        end
        inherited btnInsertZone: TButton
          Left = 423
          Top = 477
          ExplicitLeft = 423
          ExplicitTop = 477
        end
        inherited btnDeleteZone: TButton
          Left = 504
          Top = 477
          ExplicitLeft = 504
          ExplicitTop = 477
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameZoneBudget.lblComments
            end
            item
              Control = frameZoneBudget.memoComments
            end
            item
              Control = frameZoneBudget.btnInsertZone
            end
            item
              Control = frameZoneBudget.btnInsertZone
            end
            item
              Control = frameZoneBudget.cbExportCsv
            end
            item
              Control = frameZoneBudget.cbExportCsv2
            end
            item
              Control = frameZoneBudget.cbExportZblst
            end
            item
              Control = frameZoneBudget.seNumberOfZones
            end>
        end
      end
    end
    object jvspSWT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWT_Package'
      Caption = 'jvspSWT'
      inline framePkgSwt: TframePackageSwt
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        inherited lblComments: TLabel
          Top = 55
          Width = 76
          Height = 18
          ExplicitTop = 55
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          WordWrap = True
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Top = 78
          Width = 564
          Height = 52
          ExplicitTop = 78
          ExplicitWidth = 564
          ExplicitHeight = 52
        end
        inherited pcSWT: TPageControl
          Top = 136
          Width = 595
          Height = 378
          ActivePage = framePkgSwt.tabControls
          ExplicitTop = 136
          ExplicitWidth = 595
          ExplicitHeight = 378
          inherited tabControls: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 345
            inherited lblIvoid: TLabel
              Top = 92
              Width = 209
              Height = 18
              ExplicitTop = 92
              ExplicitWidth = 209
              ExplicitHeight = 18
            end
            inherited lblIstpcs: TLabel
              Top = 151
              Width = 379
              Height = 18
              ExplicitTop = 151
              ExplicitWidth = 379
              ExplicitHeight = 18
            end
            inherited lblIcrcc: TLabel
              Top = 210
              Width = 493
              Height = 18
              ExplicitTop = 210
              ExplicitWidth = 493
              ExplicitHeight = 18
            end
            inherited lblOutputChoice: TLabel
              Top = 273
              Width = 140
              Height = 18
              ExplicitTop = 273
              ExplicitWidth = 140
              ExplicitHeight = 18
            end
            inherited gbIthk: TGroupBox
              Top = 0
              Width = 571
              Height = 86
              ExplicitTop = 0
              ExplicitWidth = 571
              ExplicitHeight = 86
              inherited rgIthkConstant: TRadioButton
                Width = 565
                ExplicitWidth = 565
              end
              inherited rbIthkVariable: TRadioButton
                Top = 39
                Width = 556
                ExplicitTop = 39
                ExplicitWidth = 556
              end
            end
            inherited comboOutputChoice: TJvImageComboBox
              Top = 297
              Height = 28
              ItemHeight = 22
              ExplicitTop = 297
              ExplicitHeight = 28
            end
            inherited comboIvoid: TJvImageComboBox
              Top = 114
              Height = 28
              ItemHeight = 22
              ExplicitTop = 114
              ExplicitHeight = 28
            end
            inherited comboIstpcs: TJvImageComboBox
              Top = 173
              Height = 28
              ItemHeight = 22
              ExplicitTop = 173
              ExplicitHeight = 28
            end
            inherited comboIcrcc: TJvImageComboBox
              Top = 236
              Width = 557
              Height = 28
              DroppedWidth = 557
              ItemHeight = 22
              ExplicitTop = 236
              ExplicitWidth = 557
              ExplicitHeight = 28
            end
          end
          inherited tabPrintSave: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 345
            inherited sbAdd: TSpeedButton
              Left = 505
              Top = 357
              ExplicitLeft = 505
              ExplicitTop = 335
            end
            inherited sbInsert: TSpeedButton
              Left = 534
              Top = 357
              ExplicitLeft = 534
              ExplicitTop = 335
            end
            inherited sbDelete: TSpeedButton
              Left = 563
              Top = 357
              ExplicitLeft = 563
              ExplicitTop = 335
            end
            inherited lblNumExportPeriods: TLabel
              Top = 357
              Width = 176
              Height = 18
              ExplicitTop = 335
              ExplicitWidth = 176
              ExplicitHeight = 18
            end
            inherited rdgInitialPrintChoices: TRbwDataGrid4
              Width = 574
              FixedCols = 0
              ExplicitWidth = 574
              ColWidths = (
                64
                64
                64)
            end
            inherited rdgOutput: TRbwDataGrid4
              Top = 204
              Width = 574
              Height = 139
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
                  ComboUsed = True
                  Format = rcf4String
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  ComboUsed = True
                  Format = rcf4String
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  PickList.Strings = (
                    '11G10.3'
                    '9G13.6'
                    '15F7.1'
                    '15F7.2'
                    '15F7.3'
                    '15F7.4'
                    '20F5.0'
                    '20F5.1'
                    '20F5.2'
                    '20F5.3'
                    '20F5.4'
                    '10G11.4')
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end>
              ExplicitTop = 204
              ExplicitWidth = 574
              ExplicitHeight = 139
            end
            inherited seNumExportPeriods: TJvSpinEdit
              Top = 353
              Height = 26
              ExplicitTop = 353
              ExplicitHeight = 26
            end
            inherited comboMultiFomat: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited cbMultiPrintSave: TCheckBox
              Width = 232
              ExplicitWidth = 232
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSwt.lblComments
            end
            item
              Control = framePkgSwt.memoComments
            end
            item
              Control = framePkgSwt.rgIthkConstant
            end
            item
              Control = framePkgSwt.rbIthkVariable
            end
            item
              Control = framePkgSwt.comboIvoid
            end
            item
              Control = framePkgSwt.comboIstpcs
            end
            item
              Control = framePkgSwt.comboIcrcc
            end
            item
              Control = framePkgSwt.comboOutputChoice
            end
            item
              Control = framePkgSwt.rdgInitialPrintChoices
            end
            item
              Control = framePkgSwt.rdgOutput
            end
            item
              Control = framePkgSwt.seNumExportPeriods
            end
            item
              Control = framePkgSwt.sbAdd
            end
            item
              Control = framePkgSwt.sbInsert
            end>
        end
      end
    end
    object jvspHydmod: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'HYD_HYDMOD_Package'
      Caption = 'jvspHydmod'
      inline framePkgHydmod: TframePkgHydmod
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
        inherited lblHYDNOH: TLabel
          Top = 462
          Width = 396
          Height = 18
          ExplicitTop = 462
          ExplicitWidth = 396
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          Height = 394
          ExplicitWidth = 564
          ExplicitHeight = 394
        end
        inherited rdeHYDNOH: TRbwDataEntry
          Top = 481
          ExplicitTop = 481
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgHydmod.lblComments
            end
            item
              Control = framePkgHydmod.memoComments
            end
            item
              Control = framePkgHydmod.lblHYDNOH
            end
            item
              Control = framePkgHydmod.rdeHYDNOH
            end>
        end
      end
    end
    object jvspUPW: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'UPW_Upstream_Weighting_Package_Pane'
      Caption = 'jvspUPW'
      object JvNetscapeSplitter6: TJvNetscapeSplitter
        Left = 0
        Top = 233
        Width = 595
        Height = 10
        Cursor = crVSplit
        Align = alTop
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitTop = 192
        ExplicitWidth = 311
      end
      inline framePkgUPW: TframePackageUpw
        Left = 0
        Top = 0
        Width = 595
        Height = 233
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 233
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited cbPrintHDRY: TCheckBox
          Top = 157
          Width = 449
          ExplicitTop = 157
          ExplicitWidth = 449
        end
        inherited cbNoParCheck: TCheckBox
          Width = 145
          Height = 40
          ExplicitWidth = 145
          ExplicitHeight = 40
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgUPW.lblComments
            end
            item
              Control = framePkgUPW.memoComments
            end
            item
              Control = framePkgUPW.cbPrintHDRY
            end
            item
              Control = framePkgUPW.cbNoParCheck
            end>
          OnEnabledChange = framePkgUPWrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspNWT: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'NWT_Newton_Solver_Package_Pane'
      Caption = 'jvspNWT'
      inline framePkgNwt: TframePackageNwt
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcNWT: TPageControl
          Width = 595
          Height = 357
          ActivePage = framePkgNwt.tabAdditional
          OnChange = framePkgNwtpcNWTChange
          ExplicitWidth = 595
          ExplicitHeight = 357
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 324
            inherited lblSolverMethod: TLabel
              Left = 126
              Top = 134
              Width = 168
              Height = 18
              ExplicitLeft = 126
              ExplicitTop = 134
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited lblThicknessFactor: TLabel
              Left = 126
              Width = 381
              Height = 36
              WordWrap = True
              ExplicitLeft = 126
              ExplicitWidth = 381
              ExplicitHeight = 36
            end
            inherited lblMaxOuterIt: TLabel
              Left = 126
              Width = 362
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 362
              ExplicitHeight = 18
            end
            inherited lblFluxTolerance: TLabel
              Left = 126
              Width = 233
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 233
              ExplicitHeight = 18
            end
            inherited lblHeadTolerance: TLabel
              Left = 126
              Width = 218
              Height = 18
              ExplicitLeft = 126
              ExplicitWidth = 218
              ExplicitHeight = 18
            end
            inherited lblOptions: TLabel
              Left = 126
              Top = 227
              Width = 203
              Height = 18
              ExplicitLeft = 126
              ExplicitTop = 227
              ExplicitWidth = 203
              ExplicitHeight = 18
            end
            inherited rdeHeadTolerance: TRbwDataEntry
              Width = 117
              ExplicitWidth = 117
            end
            inherited rdeFluxTolerance: TRbwDataEntry
              Width = 117
              ExplicitWidth = 117
            end
            inherited spinMaxOuterIt: TJvSpinEdit
              Width = 117
              Height = 26
              ExplicitWidth = 117
              ExplicitHeight = 26
            end
            inherited rdeThicknessFactor: TRbwDataEntry
              Width = 117
              ExplicitWidth = 117
            end
            inherited comboSolverMethod: TJvImageComboBox
              Left = 2
              Top = 130
              Width = 118
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 2
              ExplicitTop = 130
              ExplicitWidth = 118
              ExplicitHeight = 28
            end
            inherited cbPrintFlag: TCheckBox
              Left = 2
              Top = 160
              Width = 390
              ExplicitLeft = 2
              ExplicitTop = 160
              ExplicitWidth = 390
            end
            inherited cbCorrectForCellBottom: TCheckBox
              Left = 2
              Top = 183
              Width = 566
              ExplicitLeft = 2
              ExplicitTop = 183
              ExplicitWidth = 566
            end
            inherited comboOptions: TJvImageComboBox
              Left = 1
              Top = 223
              Width = 119
              Height = 28
              ItemHeight = 22
              ExplicitLeft = 1
              ExplicitTop = 223
              ExplicitWidth = 119
              ExplicitHeight = 28
            end
            inherited cbContinue: TCheckBox
              Left = 2
              Top = 253
              Width = 582
              ExplicitLeft = 2
              ExplicitTop = 253
              ExplicitWidth = 582
            end
          end
          inherited tabAdditional: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 324
            inherited lblDbdTheta: TLabel
              Left = 86
              Width = 452
              Height = 36
              ExplicitLeft = 86
              ExplicitWidth = 452
              ExplicitHeight = 36
            end
            inherited lblDbdKappa: TLabel
              Left = 86
              Top = 38
              Width = 464
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 38
              ExplicitWidth = 464
              ExplicitHeight = 36
            end
            inherited lblDbdGamma: TLabel
              Left = 86
              Top = 88
              Width = 430
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 88
              ExplicitWidth = 430
              ExplicitHeight = 36
            end
            inherited lblMomentumCoefficient: TLabel
              Left = 86
              Top = 127
              Width = 245
              Height = 18
              Caption = 'Momentum coefficient (MOMFACT)'
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 127
              ExplicitWidth = 245
              ExplicitHeight = 18
            end
            inherited Label4: TLabel
              Left = 86
              Top = 175
              Width = 445
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 175
              ExplicitWidth = 445
              ExplicitHeight = 36
            end
            inherited lblBackTol: TLabel
              Left = 85
              Top = 216
              Width = 452
              Height = 54
              ExplicitLeft = 85
              ExplicitTop = 216
              ExplicitWidth = 452
              ExplicitHeight = 54
            end
            inherited lblReductionFactor: TLabel
              Left = 86
              Top = 276
              Width = 494
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 276
              ExplicitWidth = 494
              ExplicitHeight = 36
            end
            inherited rdeDbdTheta: TRbwDataEntry
              Left = 2
              ExplicitLeft = 2
            end
            inherited rdeDbdKappa: TRbwDataEntry
              Left = 2
              Top = 42
              ExplicitLeft = 2
              ExplicitTop = 42
            end
            inherited rdeDbdGamma: TRbwDataEntry
              Left = 2
              Top = 87
              ExplicitLeft = 2
              ExplicitTop = 87
            end
            inherited rdeMomentumCoefficient: TRbwDataEntry
              Left = 2
              Top = 124
              ExplicitLeft = 2
              ExplicitTop = 124
            end
            inherited cbUseResidualControl: TCheckBox
              Left = 2
              Top = 152
              Width = 423
              ExplicitLeft = 2
              ExplicitTop = 152
              ExplicitWidth = 423
            end
            inherited seMaxReductions: TJvSpinEdit
              Left = 1
              Top = 175
              Width = 79
              Height = 26
              ExplicitLeft = 1
              ExplicitTop = 175
              ExplicitWidth = 79
              ExplicitHeight = 26
            end
            inherited rdeBackTol: TRbwDataEntry
              Left = 1
              Top = 224
              ExplicitLeft = 1
              ExplicitTop = 224
            end
            inherited rdeReductionFactor: TRbwDataEntry
              Left = 2
              Top = 281
              ExplicitLeft = 2
              ExplicitTop = 281
            end
          end
          inherited tabGmresVariables: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 449
            ExplicitHeight = 263
            inherited lblMaxIterationsGmres: TLabel
              Width = 462
              Height = 18
              ExplicitWidth = 462
              ExplicitHeight = 18
            end
            inherited lblIluMethod: TLabel
              Left = 2
              Top = 35
              Width = 347
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 35
              ExplicitWidth = 347
              ExplicitHeight = 18
            end
            inherited lblFillLimit1: TLabel
              Left = 89
              Top = 100
              Width = 271
              Height = 18
              ExplicitLeft = 89
              ExplicitTop = 100
              ExplicitWidth = 271
              ExplicitHeight = 18
            end
            inherited lblFillLimit2: TLabel
              Left = 89
              Top = 132
              Width = 291
              Height = 18
              ExplicitLeft = 89
              ExplicitTop = 132
              ExplicitWidth = 291
              ExplicitHeight = 18
            end
            inherited lblTolerance: TLabel
              Left = 89
              Top = 164
              Width = 398
              Height = 18
              WordWrap = False
              ExplicitLeft = 89
              ExplicitTop = 164
              ExplicitWidth = 398
              ExplicitHeight = 18
            end
            inherited lblRestarts: TLabel
              Left = 89
              Top = 192
              Width = 469
              Height = 18
              ExplicitLeft = 89
              ExplicitTop = 192
              ExplicitWidth = 469
              ExplicitHeight = 18
            end
            inherited seMaxIterationsGmres: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboIluMethod: TJvImageComboBox
              Left = 2
              Top = 63
              Width = 358
              Height = 28
              DroppedWidth = 358
              ItemHeight = 22
              ExplicitLeft = 2
              ExplicitTop = 63
              ExplicitWidth = 358
              ExplicitHeight = 28
            end
            inherited seFillLimit1: TJvSpinEdit
              Left = 2
              Top = 97
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 97
              ExplicitHeight = 26
            end
            inherited seFillLimit2: TJvSpinEdit
              Left = 2
              Top = 129
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 129
              ExplicitHeight = 26
            end
            inherited rdeTolerance: TRbwDataEntry
              Left = 2
              Top = 161
              ExplicitLeft = 2
              ExplicitTop = 161
            end
            inherited seRestarts: TJvSpinEdit
              Left = 2
              Top = 189
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 189
              ExplicitHeight = 26
            end
          end
          inherited TabChi_MD_Variables: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 449
            ExplicitHeight = 263
            inherited lblAccelMethod: TLabel
              Left = 239
              Width = 192
              Height = 18
              ExplicitLeft = 239
              ExplicitWidth = 192
              ExplicitHeight = 18
            end
            inherited lblOrderingScheme: TLabel
              Left = 239
              Width = 316
              Height = 18
              ExplicitLeft = 239
              ExplicitWidth = 316
              ExplicitHeight = 18
            end
            inherited lblFillLevel: TLabel
              Left = 86
              Top = 66
              Width = 351
              Height = 18
              ExplicitLeft = 86
              ExplicitTop = 66
              ExplicitWidth = 351
              ExplicitHeight = 18
            end
            inherited lblNumOrtho: TLabel
              Left = 86
              Top = 87
              Width = 479
              Height = 36
              ExplicitLeft = 86
              ExplicitTop = 87
              ExplicitWidth = 479
              ExplicitHeight = 36
            end
            inherited lblResRedCrit: TLabel
              Left = 86
              Top = 157
              Width = 372
              Height = 18
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 157
              ExplicitWidth = 372
              ExplicitHeight = 18
            end
            inherited lblDropTolerance: TLabel
              Left = 86
              Top = 209
              Width = 305
              Height = 18
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 209
              ExplicitWidth = 305
              ExplicitHeight = 18
            end
            inherited lblHeadClosure: TLabel
              Left = 86
              Top = 237
              Width = 440
              Height = 18
              WordWrap = False
              ExplicitLeft = 86
              ExplicitTop = 237
              ExplicitWidth = 440
              ExplicitHeight = 18
            end
            inherited lblMaxIterChimd: TLabel
              Left = 86
              Top = 265
              Width = 462
              Height = 18
              ExplicitLeft = 86
              ExplicitTop = 265
              ExplicitWidth = 462
              ExplicitHeight = 18
            end
            inherited comboAccelMethod: TJvImageComboBox
              Width = 230
              Height = 28
              ItemHeight = 22
              ExplicitWidth = 230
              ExplicitHeight = 28
            end
            inherited comboOrderingScheme: TJvImageComboBox
              Width = 230
              Height = 28
              DroppedWidth = 230
              ItemHeight = 22
              ExplicitWidth = 230
              ExplicitHeight = 28
            end
            inherited seFillLevel: TJvSpinEdit
              Left = 2
              Height = 26
              ExplicitLeft = 2
              ExplicitHeight = 26
            end
            inherited seNumOrtho: TJvSpinEdit
              Left = 2
              Top = 93
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 93
              ExplicitHeight = 26
            end
            inherited cbApplyReducedPreconditioning: TCheckBox
              Left = 2
              Top = 131
              ExplicitLeft = 2
              ExplicitTop = 131
            end
            inherited rdeResRedCrit: TRbwDataEntry
              Left = 2
              Top = 154
              ExplicitLeft = 2
              ExplicitTop = 154
            end
            inherited cbUseDropTolerance: TCheckBox
              Left = 2
              Top = 183
              Width = 439
              ExplicitLeft = 2
              ExplicitTop = 183
              ExplicitWidth = 439
            end
            inherited rdeDropTolerance: TRbwDataEntry
              Left = 2
              Top = 206
              ExplicitLeft = 2
              ExplicitTop = 206
            end
            inherited rdeHeadClosure: TRbwDataEntry
              Left = 2
              Top = 234
              ExplicitLeft = 2
              ExplicitTop = 234
            end
            inherited seMaxIterChimd: TJvSpinEdit
              Left = 2
              Top = 262
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 262
              ExplicitHeight = 26
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgNwt.lblComments
            end
            item
              Control = framePkgNwt.memoComments
            end
            item
              Control = framePkgNwt.rdeHeadTolerance
            end
            item
              Control = framePkgNwt.rdeFluxTolerance
            end
            item
              Control = framePkgNwt.spinMaxOuterIt
            end
            item
              Control = framePkgNwt.rdeThicknessFactor
            end
            item
              Control = framePkgNwt.comboSolverMethod
            end
            item
              Control = framePkgNwt.cbPrintFlag
            end
            item
              Control = framePkgNwt.cbCorrectForCellBottom
            end
            item
              Control = framePkgNwt.comboOptions
            end
            item
              Control = framePkgNwt.rdeDbdTheta
            end
            item
              Control = framePkgNwt.rdeDbdKappa
            end
            item
              Control = framePkgNwt.rdeDbdGamma
            end
            item
              Control = framePkgNwt.rdeMomentumCoefficient
            end
            item
              Control = framePkgNwt.cbUseResidualControl
            end
            item
              Control = framePkgNwt.seMaxReductions
            end
            item
              Control = framePkgNwt.rdeBackTol
            end
            item
              Control = framePkgNwt.rdeReductionFactor
            end
            item
              Control = framePkgNwt.seMaxIterationsGmres
            end
            item
              Control = framePkgNwt.comboIluMethod
            end
            item
              Control = framePkgNwt.rdeTolerance
            end
            item
              Control = framePkgNwt.seRestarts
            end
            item
              Control = framePkgNwt.comboAccelMethod
            end
            item
              Control = framePkgNwt.comboOrderingScheme
            end
            item
              Control = framePkgNwt.seFillLevel
            end
            item
              Control = framePkgNwt.seNumOrtho
            end
            item
              Control = framePkgNwt.cbApplyReducedPreconditioning
            end
            item
              Control = framePkgNwt.rdeResRedCrit
            end
            item
              Control = framePkgNwt.cbUseDropTolerance
            end
            item
              Control = framePkgNwt.rdeDropTolerance
            end
            item
              Control = framePkgNwt.rdeHeadClosure
            end
            item
              Control = framePkgNwt.seMaxIterChimd
            end>
          OnEnabledChange = framePkgNwtrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspMt3dmsBasic: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'BTN_Basic_Transport_Package'
      Caption = 'jvspMt3dmsBasic'
      inline framePkgMt3dBasic: TframeMt3dBasicPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        inherited lblComments: TLabel
          Width = 294
          Height = 18
          Caption = 'Comments (first two lines are the Heading)'
          ExplicitWidth = 294
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblInactiveConcentration: TLabel
          Top = 150
          Width = 278
          Height = 18
          ExplicitTop = 150
          ExplicitWidth = 278
          ExplicitHeight = 18
        end
        inherited lblMinimumSaturatedFraction: TLabel
          Width = 259
          Height = 18
          ExplicitWidth = 259
          ExplicitHeight = 18
        end
        inherited edMassUnit: TLabeledEdit
          Width = 65
          Height = 26
          EditLabel.Width = 127
          EditLabel.Height = 18
          EditLabel.ExplicitLeft = 84
          EditLabel.ExplicitTop = 123
          EditLabel.ExplicitWidth = 127
          EditLabel.ExplicitHeight = 18
          ExplicitWidth = 65
          ExplicitHeight = 26
        end
        inherited grpInitialConcentrationTimes: TGroupBox
          Width = 576
          ExplicitWidth = 576
          inherited lblStressPeriod: TLabel
            Left = 8
            Width = 94
            Height = 18
            ExplicitLeft = 8
            ExplicitWidth = 94
            ExplicitHeight = 18
          end
          inherited lblTimeStep: TLabel
            Left = 216
            Width = 68
            Height = 18
            ExplicitLeft = 216
            ExplicitWidth = 68
            ExplicitHeight = 18
          end
          inherited lblTransportStep: TLabel
            Left = 392
            Width = 99
            Height = 18
            ExplicitLeft = 392
            ExplicitWidth = 99
            ExplicitHeight = 18
          end
          inherited seStressPeriod: TJvSpinEdit
            Left = 108
            Top = 21
            Height = 26
            TabOrder = 0
            ExplicitLeft = 108
            ExplicitTop = 21
            ExplicitHeight = 26
          end
          inherited seTimeStep: TJvSpinEdit
            Left = 290
            Height = 26
            TabOrder = 1
            ExplicitLeft = 290
            ExplicitHeight = 26
          end
          inherited seTransportStep: TJvSpinEdit
            Left = 497
            Height = 26
            TabOrder = 2
            ExplicitLeft = 497
            ExplicitHeight = 26
          end
        end
        inherited pnlSpecies: TPanel
          Top = 265
          Width = 595
          Height = 249
          ExplicitTop = 265
          ExplicitWidth = 595
          ExplicitHeight = 249
          inherited Splitter1: TSplitter
            Left = 300
            Height = 247
            ExplicitLeft = 300
            ExplicitHeight = 321
          end
          inherited frameGridImmobile: TframeGrid
            Left = 305
            Width = 289
            Height = 247
            ExplicitLeft = 305
            ExplicitWidth = 289
            ExplicitHeight = 247
            inherited Panel: TPanel
              Top = 206
              Width = 289
              ExplicitTop = 206
              ExplicitWidth = 289
              inherited lbNumber: TLabel
                Width = 55
                Height = 18
                ExplicitWidth = 55
                ExplicitHeight = 18
              end
              inherited sbAdd: TSpeedButton
                Left = 147
                ExplicitLeft = 147
              end
              inherited sbInsert: TSpeedButton
                Left = 175
                ExplicitLeft = 175
              end
              inherited sbDelete: TSpeedButton
                Left = 203
                ExplicitLeft = 203
              end
              inherited seNumber: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 289
              Height = 206
              Columns = <
                item
                  AutoAdjustRowHeights = True
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
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
                  ButtonCaption = '...'
                  ButtonFont.Charset = DEFAULT_CHARSET
                  ButtonFont.Color = clWindowText
                  ButtonFont.Height = -13
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = False
                  ButtonCaption = 'Select...'
                  ButtonFont.Charset = DEFAULT_CHARSET
                  ButtonFont.Color = clWindowText
                  ButtonFont.Height = -13
                  ButtonFont.Name = 'Tahoma'
                  ButtonFont.Style = []
                  ButtonUsed = True
                  ButtonWidth = 80
                  CheckMax = False
                  CheckMin = False
                  ComboUsed = False
                  Format = rcf4String
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = False
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end>
              ExplicitWidth = 289
              ExplicitHeight = 206
            end
          end
          inherited frameGridMobile: TframeGrid
            Width = 299
            Height = 247
            ExplicitWidth = 299
            ExplicitHeight = 247
            inherited Panel: TPanel
              Top = 206
              Width = 299
              ExplicitTop = 206
              ExplicitWidth = 299
              inherited lbNumber: TLabel
                Width = 55
                Height = 18
                ExplicitWidth = 55
                ExplicitHeight = 18
              end
              inherited seNumber: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 299
              Height = 206
              Columns = <
                item
                  AutoAdjustRowHeights = True
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
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = True
                  ButtonCaption = '...'
                  ButtonFont.Charset = DEFAULT_CHARSET
                  ButtonFont.Color = clWindowText
                  ButtonFont.Height = -13
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
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = False
                  ButtonCaption = 'Select...'
                  ButtonFont.Charset = DEFAULT_CHARSET
                  ButtonFont.Color = clWindowText
                  ButtonFont.Height = -13
                  ButtonFont.Name = 'Tahoma'
                  ButtonFont.Style = []
                  ButtonUsed = True
                  ButtonWidth = 80
                  CheckMax = False
                  CheckMin = False
                  ComboUsed = False
                  Format = rcf4String
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = False
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end>
              ExplicitWidth = 299
              ExplicitHeight = 206
            end
          end
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMt3dBasic.lblComments
            end
            item
              Control = framePkgMt3dBasic.memoComments
            end
            item
              Control = framePkgMt3dBasic.edMassUnit
            end
            item
              Control = framePkgMt3dBasic.rdeInactiveConcentration
            end
            item
              Control = framePkgMt3dBasic.rdeMinimumSaturatedFraction
            end
            item
              Control = framePkgMt3dBasic.frameGridMobile
            end
            item
              Control = framePkgMt3dBasic.frameGridImmobile
            end>
        end
      end
    end
    object jvspMt3dmsGCG: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'GCG_Generalized_Conjugate_Grad'
      Caption = 'jvspMt3dmsGCG'
      inline frameMt3dmsGcgPackage: TframeMt3dmsGcgPackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
        inherited lblMaxOuter: TLabel
          Width = 309
          Height = 18
          ExplicitWidth = 309
          ExplicitHeight = 18
        end
        inherited lblMaxInner: TLabel
          Width = 293
          Height = 18
          ExplicitWidth = 293
          ExplicitHeight = 18
        end
        inherited lblPreconditioner: TLabel
          Width = 172
          Height = 18
          ExplicitWidth = 172
          ExplicitHeight = 18
        end
        inherited lblDispersion: TLabel
          Width = 251
          Height = 18
          ExplicitWidth = 251
          ExplicitHeight = 18
        end
        inherited lblRelaxationFactor: TLabel
          Width = 174
          Height = 18
          ExplicitWidth = 174
          ExplicitHeight = 18
        end
        inherited lblConvergence: TLabel
          Width = 234
          Height = 18
          ExplicitWidth = 234
          ExplicitHeight = 18
        end
        inherited lblPrintoutInterval: TLabel
          Width = 181
          Height = 18
          ExplicitWidth = 181
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited spinMaxOuter: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited spinMaxInner: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited comboPreconditioner: TComboBox
          Width = 259
          Height = 26
          ExplicitWidth = 259
          ExplicitHeight = 26
        end
        inherited comboDispersion: TComboBox
          Width = 425
          Height = 26
          ExplicitWidth = 425
          ExplicitHeight = 26
        end
        inherited spinPrintoutInterval: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dmsGcgPackage.lblComments
            end
            item
              Control = frameMt3dmsGcgPackage.memoComments
            end
            item
              Control = frameMt3dmsGcgPackage.spinMaxOuter
            end
            item
              Control = frameMt3dmsGcgPackage.spinMaxInner
            end
            item
              Control = frameMt3dmsGcgPackage.comboPreconditioner
            end
            item
              Control = frameMt3dmsGcgPackage.comboDispersion
            end
            item
              Control = frameMt3dmsGcgPackage.rdeConvergence
            end
            item
              Control = frameMt3dmsGcgPackage.spinPrintoutInterval
            end>
        end
      end
    end
    object jvspMt3dmsAdv: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'ADV_Advection_Package_Pane'
      Caption = 'jvspMt3dmsAdv'
      inline frameMt3dmsAdvPkg: TframeMt3dmsAdvPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        DesignSize = (
          595
          514)
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcAdvection: TPageControl
          Top = 146
          Width = 595
          Height = 368
          ExplicitTop = 146
          ExplicitWidth = 595
          ExplicitHeight = 368
          inherited tabAdvection1: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 335
            inherited lbllMethod: TLabel
              Top = 6
              Width = 189
              Height = 36
              WordWrap = True
              ExplicitTop = 6
              ExplicitWidth = 189
              ExplicitHeight = 36
            end
            inherited lblParticleTracking: TLabel
              Top = 51
              Width = 251
              Height = 18
              ExplicitTop = 51
              ExplicitWidth = 251
              ExplicitHeight = 18
            end
            inherited lbNumCellsParticle: TLabel
              Left = 2
              Top = 115
              Width = 434
              Height = 36
              WordWrap = True
              ExplicitLeft = 2
              ExplicitTop = 115
              ExplicitWidth = 434
              ExplicitHeight = 36
            end
            inherited lblMaxParticlesCount: TLabel
              Left = 2
              Top = 157
              Width = 369
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 157
              ExplicitWidth = 369
              ExplicitHeight = 18
            end
            inherited lblConcWeight: TLabel
              Left = 2
              Top = 189
              Width = 321
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 189
              ExplicitWidth = 321
              ExplicitHeight = 18
            end
            inherited lblNegConcGrad: TLabel
              Left = 2
              Top = 217
              Width = 348
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 217
              ExplicitWidth = 348
              ExplicitHeight = 18
            end
            inherited lblInitParticlesSmall: TLabel
              Left = 2
              Top = 245
              Width = 373
              Height = 36
              WordWrap = True
              ExplicitLeft = 2
              ExplicitTop = 245
              ExplicitWidth = 373
              ExplicitHeight = 36
            end
            inherited lblInitParticlesLarge: TLabel
              Left = 2
              Top = 289
              Width = 373
              Height = 36
              WordWrap = True
              ExplicitLeft = 2
              ExplicitTop = 289
              ExplicitWidth = 373
              ExplicitHeight = 36
            end
            inherited Label12: TLabel
              Top = 83
              Width = 210
              Height = 18
              ExplicitTop = 83
              ExplicitWidth = 210
              ExplicitHeight = 18
            end
            inherited comboAdvSolScheme: TComboBox
              Left = 224
              Width = 350
              Height = 26
              ItemIndex = 3
              Text = 'Modified method of characterisitics MMOC (2)'
              ExplicitLeft = 224
              ExplicitWidth = 350
              ExplicitHeight = 26
            end
            inherited comboParticleTrackingAlg: TComboBox
              Left = 269
              Top = 48
              Height = 26
              ExplicitLeft = 269
              ExplicitTop = 48
              ExplicitHeight = 26
            end
            inherited adeMaxParticleMovement: TRbwDataEntry
              Left = 485
              Top = 112
              ExplicitLeft = 485
              ExplicitTop = 112
            end
            inherited adeConcWeight: TRbwDataEntry
              Left = 485
              Top = 186
              ExplicitLeft = 485
              ExplicitTop = 186
            end
            inherited adeNeglSize: TRbwDataEntry
              Left = 485
              Top = 214
              ExplicitLeft = 485
              ExplicitTop = 214
            end
            inherited comboAdvWeightingScheme: TComboBox
              Left = 344
              Top = 80
              Width = 230
              Height = 26
              ExplicitLeft = 344
              ExplicitTop = 80
              ExplicitWidth = 230
              ExplicitHeight = 26
            end
            inherited spinMaxParticlesCount: TJvSpinEdit
              Left = 485
              Top = 154
              Height = 26
              ExplicitLeft = 485
              ExplicitTop = 154
              ExplicitHeight = 26
            end
            inherited spinInitParticlesSmall: TJvSpinEdit
              Left = 485
              Top = 242
              Height = 26
              ExplicitLeft = 485
              ExplicitTop = 242
              ExplicitHeight = 26
            end
            inherited spinInitParticlesLarge: TJvSpinEdit
              Left = 485
              Top = 286
              Height = 26
              ExplicitLeft = 485
              ExplicitTop = 286
              ExplicitHeight = 26
            end
          end
          inherited tabAdvection2: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 593
            ExplicitHeight = 303
            inherited lblInitParticlePlacement: TLabel
              Width = 323
              Height = 18
              ExplicitWidth = 323
              ExplicitHeight = 18
            end
            inherited lblInitParticlePlanes: TLabel
              Top = 35
              Width = 361
              Height = 18
              ExplicitTop = 35
              ExplicitWidth = 361
              ExplicitHeight = 18
            end
            inherited lblMinParticles: TLabel
              Left = 2
              Top = 67
              Width = 400
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 67
              ExplicitWidth = 400
              ExplicitHeight = 18
            end
            inherited lblMaxParticles: TLabel
              Left = 2
              Top = 99
              Width = 309
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 99
              ExplicitWidth = 309
              ExplicitHeight = 18
            end
            inherited lblSinkParticlePlacement: TLabel
              Left = 2
              Top = 131
              Width = 317
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 131
              ExplicitWidth = 317
              ExplicitHeight = 18
            end
            inherited lblSinkParticlePlanes: TLabel
              Left = 2
              Top = 163
              Width = 385
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 163
              ExplicitWidth = 385
              ExplicitHeight = 18
            end
            inherited lblSinkParticleN: TLabel
              Left = 2
              Top = 195
              Width = 404
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 195
              ExplicitWidth = 404
              ExplicitHeight = 18
            end
            inherited lblCritConcGrad: TLabel
              Left = 2
              Top = 227
              Width = 347
              Height = 18
              ExplicitLeft = 2
              ExplicitTop = 227
              ExplicitWidth = 347
              ExplicitHeight = 18
            end
            inherited comboInitPartPlace: TComboBox
              Left = 437
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited comboInitPartSinkChoice: TComboBox
              Left = 437
              Top = 128
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 128
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited adeCritRelConcGrad: TRbwDataEntry
              Left = 437
              Top = 224
              Width = 108
              ExplicitLeft = 437
              ExplicitTop = 224
              ExplicitWidth = 108
            end
            inherited spinInitParticlePlanes: TJvSpinEdit
              Left = 437
              Top = 32
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 32
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinMinParticles: TJvSpinEdit
              Left = 437
              Top = 64
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 64
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinMaxParticles: TJvSpinEdit
              Left = 437
              Top = 96
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 96
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinSinkParticlePlanes: TJvSpinEdit
              Left = 437
              Top = 160
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 160
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
            inherited spinSinkParticleN: TJvSpinEdit
              Left = 437
              Top = 192
              Width = 108
              Height = 26
              ExplicitLeft = 437
              ExplicitTop = 192
              ExplicitWidth = 108
              ExplicitHeight = 26
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dmsAdvPkg.lblComments
            end
            item
              Control = frameMt3dmsAdvPkg.memoComments
            end
            item
              Control = frameMt3dmsAdvPkg.comboAdvSolScheme
            end
            item
              Control = frameMt3dmsAdvPkg.comboAdvWeightingScheme
            end
            item
              Control = frameMt3dmsAdvPkg.adeMaxParticleMovement
            end
            item
              Control = frameMt3dmsAdvPkg.spinMaxParticlesCount
            end>
        end
      end
    end
    object jvspMt3dmsDsp: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'DSP_Dispersion_Package_Pane'
      Caption = 'jvspMt3dmsDsp'
      inline frameMt3dmsDispersionPkg: TframeMt3dmsDispersionPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited cbMultiDiffusion: TCheckBox
          Width = 545
          Height = 44
          ExplicitWidth = 545
          ExplicitHeight = 44
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = frameMt3dmsDispersionPkg.lblComments
            end
            item
              Control = frameMt3dmsDispersionPkg.memoComments
            end
            item
              Control = frameMt3dmsDispersionPkg.cbMultiDiffusion
            end>
        end
      end
    end
    object jvspMt3dmsSsm: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SSM_Sink__Source_Mixing_Packag'
      Caption = 'jvspMt3dmsSsm'
      inline framePkgSSM: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 463
          ExplicitWidth = 564
          ExplicitHeight = 463
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSSM.lblComments
            end
            item
              Control = framePkgSSM.memoComments
            end>
        end
      end
    end
    object jvspMt3dmsRct: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'RCT_Chemical_Reactions_Package'
      Caption = 'jvspMt3dmsRctPkg'
      inline framePkgMt3dmsRct: TframeMt3dmsChemReactionPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        DesignSize = (
          595
          514)
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
        inherited lblSorptionChoice: TLabel
          Width = 182
          Height = 18
          ExplicitWidth = 182
          ExplicitHeight = 18
        end
        inherited lblKineticChoice: TLabel
          Width = 169
          Height = 18
          ExplicitWidth = 169
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Width = 564
          ExplicitWidth = 564
        end
        inherited comboSorptionChoice: TJvImageComboBox
          Height = 26
          ItemHeight = 18
          ExplicitHeight = 26
        end
        inherited comboKineticChoice: TJvImageComboBox
          Height = 26
          ItemHeight = 18
          ExplicitHeight = 26
        end
        inherited cbInitialConcChoice: TCheckBox
          Width = 561
          ExplicitWidth = 561
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMt3dmsRct.lblComments
            end
            item
              Control = framePkgMt3dmsRct.memoComments
            end
            item
              Control = framePkgMt3dmsRct.comboSorptionChoice
            end
            item
              Control = framePkgMt3dmsRct.comboKineticChoice
            end
            item
              Control = framePkgMt3dmsRct.cbInitialConcChoice
            end>
        end
      end
    end
    object jvspMt3dmsTOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'TOB_Transport_Observation_Pack'
      Caption = 'jvspMt3dmsTOB'
      inline framePkgMt3dmsTob: TframeMt3dmsTransObsPkg
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        DesignSize = (
          595
          514)
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited grpbxConcentrationObservations: TGroupBox
          inherited lblConcScaleFactor: TLabel
            Width = 247
            Height = 18
            ExplicitWidth = 247
            ExplicitHeight = 18
          end
          inherited lblSaveType: TLabel
            Top = 70
            Width = 373
            Height = 18
            ExplicitTop = 70
            ExplicitWidth = 373
            ExplicitHeight = 18
          end
          inherited rdeConcScaleFactor: TRbwDataEntry
            Top = 42
            ExplicitTop = 42
          end
          inherited comboSaveConcType: TJvImageComboBox
            Height = 28
            ItemHeight = 22
            ExplicitHeight = 28
          end
          inherited cbLogTransform: TCheckBox
            Top = 125
            ExplicitTop = 125
          end
          inherited cbInterpolate: TCheckBox
            Top = 149
            ExplicitTop = 149
          end
        end
        inherited grpbxMassFluxObservations: TGroupBox
          Left = 17
          ExplicitLeft = 17
          inherited lblMassFluxScaleFactor: TLabel
            Width = 212
            Height = 18
            ExplicitWidth = 212
            ExplicitHeight = 18
          end
          inherited lblSaveMassFluxType: TLabel
            Width = 330
            Height = 18
            ExplicitWidth = 330
            ExplicitHeight = 18
          end
          inherited comboSaveMassFluxType: TJvImageComboBox
            Height = 28
            ItemHeight = 22
            ExplicitHeight = 28
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgMt3dmsTob.lblComments
            end
            item
              Control = framePkgMt3dmsTob.memoComments
            end
            item
              Control = framePkgMt3dmsTob.cbSaveBinary
            end
            item
              Control = framePkgMt3dmsTob.rdeConcScaleFactor
            end
            item
              Control = framePkgMt3dmsTob.comboSaveConcType
            end
            item
              Control = framePkgMt3dmsTob.cbLogTransform
            end
            item
              Control = framePkgMt3dmsTob.cbInterpolate
            end
            item
              Control = framePkgMt3dmsTob.rdeMassFluxScaleFactor
            end
            item
              Control = framePkgMt3dmsTob.comboSaveMassFluxType
            end>
        end
      end
    end
    object jvspPCGN: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'PCGN_Preconditioned_Conjugate_'
      Caption = 'jvspPCGN'
      inline framePackagePcgn: TframePackagePcgn
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
        inherited lblComments: TLabel
          Top = 56
          Width = 76
          Height = 18
          ExplicitTop = 56
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          WordWrap = True
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited memoComments: TMemo
          Top = 80
          Width = 564
          Height = 71
          ExplicitTop = 80
          ExplicitWidth = 564
          ExplicitHeight = 71
        end
        inherited pcControls: TPageControl
          Width = 595
          Height = 357
          ExplicitWidth = 595
          ExplicitHeight = 357
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 592
            ExplicitHeight = 360
            inherited lblIter_mo: TLabel
              Width = 389
              Height = 18
              ExplicitWidth = 389
              ExplicitHeight = 18
            end
            inherited lblIter_mi: TLabel
              Width = 368
              Height = 18
              ExplicitWidth = 368
              ExplicitHeight = 18
            end
            inherited lblCLOSE_R: TLabel
              Width = 429
              Height = 18
              ExplicitWidth = 429
              ExplicitHeight = 18
            end
            inherited lblClose_H: TLabel
              Width = 409
              Height = 18
              ExplicitWidth = 409
              ExplicitHeight = 18
            end
            inherited lblRelax: TLabel
              Width = 216
              Height = 18
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblIfill: TLabel
              Width = 281
              Height = 18
              ExplicitWidth = 281
              ExplicitHeight = 18
            end
            inherited seIter_mo: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seIter_mi: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seIfill: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited cbUnit_pc: TCheckBox
              Width = 470
              ExplicitWidth = 470
            end
          end
          inherited tabNonLinear: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 324
            inherited lblDampingMode: TLabel
              Width = 180
              Height = 18
              ExplicitWidth = 180
              ExplicitHeight = 18
            end
            inherited lblDamp: TLabel
              Width = 251
              Height = 18
              ExplicitWidth = 251
              ExplicitHeight = 18
            end
            inherited lblDamp_Lb: TLabel
              Width = 238
              Height = 18
              ExplicitWidth = 238
              ExplicitHeight = 18
            end
            inherited lblDamp_D: TLabel
              Width = 324
              Height = 18
              ExplicitWidth = 324
              ExplicitHeight = 18
            end
            inherited lblChglimit: TLabel
              Width = 247
              Height = 18
              ExplicitWidth = 247
              ExplicitHeight = 18
            end
            inherited lblAcnvg: TLabel
              Width = 208
              Height = 18
              ExplicitWidth = 208
              ExplicitHeight = 18
            end
            inherited lblChvg_Lb: TLabel
              Width = 361
              Height = 18
              ExplicitWidth = 361
              ExplicitHeight = 18
            end
            inherited lblMcnvg: TLabel
              Width = 374
              Height = 36
              WordWrap = True
              ExplicitWidth = 374
              ExplicitHeight = 36
            end
            inherited lblRate_C: TLabel
              Top = 255
              Width = 316
              Height = 18
              ExplicitTop = 255
              ExplicitWidth = 316
              ExplicitHeight = 18
            end
            inherited lblIpunit: TLabel
              Top = 280
              Width = 349
              Height = 18
              ExplicitTop = 280
              ExplicitWidth = 349
              ExplicitHeight = 18
            end
            inherited comboDampingMode: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboAcnvg: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited seMcnvg: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited rdeRate_C: TRbwDataEntry
              Top = 252
              ExplicitTop = 252
            end
            inherited comboIpunit: TJvImageComboBox
              Top = 280
              Height = 28
              ItemHeight = 22
              ExplicitTop = 280
              ExplicitHeight = 28
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackagePcgn.lblComments
            end
            item
              Control = framePackagePcgn.memoComments
            end
            item
              Control = framePackagePcgn.seIter_mo
            end
            item
              Control = framePackagePcgn.seIter_mi
            end
            item
              Control = framePackagePcgn.rdeCLOSE_R
            end
            item
              Control = framePackagePcgn.rdeClose_H
            end
            item
              Control = framePackagePcgn.rdeRelax
            end
            item
              Control = framePackagePcgn.seIfill
            end
            item
              Control = framePackagePcgn.cbUnit_pc
            end
            item
              Control = framePackagePcgn.cbUnit_ts
            end
            item
              Control = framePackagePcgn.comboDampingMode
            end
            item
              Control = framePackagePcgn.rdeDamp
            end
            item
              Control = framePackagePcgn.rdeDamp_Lb
            end
            item
              Control = framePackagePcgn.rdeRate_D
            end
            item
              Control = framePackagePcgn.rdeChglimit
            end
            item
              Control = framePackagePcgn.comboAcnvg
            end
            item
              Control = framePackagePcgn.comboIpunit
            end>
        end
      end
    end
    object jvspSTR: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'STR_Stream_package'
      Caption = 'jvspSTR'
      inline framePkgStr: TframePackageStr
        Left = 0
        Top = 0
        Width = 595
        Height = 201
        Align = alTop
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 201
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited cbCalculateStage: TCheckBox
          Height = 20
          ExplicitHeight = 20
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgStr.lblComments
            end
            item
              Control = framePkgStr.memoComments
            end
            item
              Control = framePkgStr.cbCalculateStage
            end
            item
              Control = frameStrParameterDefinition
            end>
        end
      end
      inline frameStrParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 201
        Width = 595
        Height = 313
        Align = alClient
        Enabled = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 201
        ExplicitWidth = 595
        ExplicitHeight = 313
        inherited pnlParameterCount: TPanel
          Top = 265
          Width = 595
          ExplicitTop = 265
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 491
            OnClick = frameParameterDefinition_btnDeleteClick
            ExplicitLeft = 491
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 259
          ExplicitWidth = 589
          ExplicitHeight = 259
        end
      end
    end
    object jvspSTOB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'STOB_Stream_Observation_Packag'
      Caption = 'jvspSTOB'
      inline framePkgSTOB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 446
          ExplicitWidth = 564
          ExplicitHeight = 446
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgSTOB.lblComments
            end
            item
              Control = framePkgSTOB.memoComments
            end>
        end
      end
    end
    object jvspFHB: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'FHB_Flow_and_Head_Boundary_Pac'
      Caption = 'jvspFHB'
      inline framePkgFHB: TframePackage
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          Height = 446
          ExplicitWidth = 564
          ExplicitHeight = 446
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgFHB.lblComments
            end
            item
              Control = framePkgFHB.memoComments
            end>
        end
      end
    end
    object jvspFMP: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'FMP_Farm_Process'
      Caption = 'jvspFMP'
      inline frameFmpParameterDefinition: TframeListParameterDefinition
        Left = 0
        Top = 360
        Width = 595
        Height = 154
        Align = alBottom
        Enabled = False
        TabOrder = 0
        TabStop = True
        ExplicitTop = 360
        ExplicitWidth = 595
        ExplicitHeight = 154
        inherited pnlParameterCount: TPanel
          Top = 106
          Width = 595
          ExplicitTop = 106
          ExplicitWidth = 595
          inherited lblNumParameters: TLabel
            Width = 156
            Height = 18
            ExplicitWidth = 156
            ExplicitHeight = 18
          end
          inherited btnDelete: TBitBtn
            Left = 491
            ExplicitLeft = 491
          end
          inherited seNumberOfParameters: TJvSpinEdit
            Height = 26
            OnChange = frameParameterDefinition_seNumberOfParametersChange
            ExplicitHeight = 26
          end
        end
        inherited dgParameters: TRbwDataGrid4
          Width = 589
          Height = 100
          ExplicitWidth = 589
          ExplicitHeight = 100
        end
      end
      inline framePkgFrm: TframePkgFarm
        Left = 0
        Top = 0
        Width = 595
        Height = 360
        Align = alClient
        TabOrder = 1
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 360
        inherited lblPackage: TLabel
          Width = 78
          Height = 18
          ExplicitWidth = 78
          ExplicitHeight = 18
        end
        inherited lblComments: TLabel
          Width = 76
          Height = 18
          ExplicitWidth = 76
          ExplicitHeight = 18
        end
        inherited splttrFarm: TJvNetscapeSplitter
          Left = 128
          Height = 360
          ExplicitLeft = 128
          ExplicitHeight = 350
        end
        inherited memoComments: TMemo
          Width = 163
          Height = 47
          ExplicitWidth = 163
          ExplicitHeight = 47
        end
        inherited jvplFarm: TJvPageList
          Left = 138
          Width = 457
          Height = 360
          ActivePage = framePkgFrm.jvspCropConsumptiveUse
          OnChange = framePkgFrmjvplFarmChange
          ExplicitLeft = 138
          ExplicitWidth = 457
          ExplicitHeight = 360
          inherited jvspOptions: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'Farm_Process_Options'
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblCropIrrigationRequirement: TLabel
              Width = 322
              Height = 18
              ExplicitWidth = 322
              ExplicitHeight = 18
            end
            inherited lblRecomputeFlows: TLabel
              Width = 419
              Height = 18
              ExplicitWidth = 419
              ExplicitHeight = 18
            end
            inherited rgAssignmentMethod: TRadioGroup
              Width = 345
              ExplicitWidth = 345
            end
            inherited comboCropIrrigationRequirement: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboRecomputeFlows: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited jvspParameters: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'FarmParameters'
            ExplicitWidth = 457
            ExplicitHeight = 360
          end
          inherited jvspWhenToRead: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'When_to_Read_Flags'
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblRootingDepth: TLabel
              Width = 153
              Height = 18
              ExplicitWidth = 153
              ExplicitHeight = 18
            end
            inherited lblConsumptiveUse: TLabel
              Width = 179
              Height = 18
              ExplicitWidth = 179
              ExplicitHeight = 18
            end
            inherited lblPrecipitation: TLabel
              Width = 135
              Height = 18
              ExplicitWidth = 135
              ExplicitHeight = 18
            end
            inherited lblInefficiencyLosses: TLabel
              Width = 280
              Height = 18
              ExplicitWidth = 280
              ExplicitHeight = 18
            end
            inherited comboRootingDepth: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboConsumptiveUse: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboInefficiencyLosses: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPrecipitation: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited jvspWaterPolicy: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'Water_Policy_Flags'
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblDeficiency: TLabel
              Width = 186
              Height = 18
              ExplicitWidth = 186
              ExplicitHeight = 18
            end
            inherited comboDeficiency: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited frameEfficiencyBehavior: TframeRadioGrid
              Width = 457
              ExplicitWidth = 457
              inherited grpDescription: TGroupBox
                Width = 454
                ExplicitWidth = 454
                inherited lblTop: TLabel
                  Left = 168
                  Width = 214
                  Height = 18
                  ExplicitLeft = 168
                  ExplicitWidth = 214
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 18
                  Height = 106
                  ExplicitWidth = 18
                  ExplicitHeight = 106
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 417
                  Height = 233
                  Margins.Top = 20
                  ExplicitTop = 40
                  ExplicitWidth = 417
                  ExplicitHeight = 233
                end
              end
            end
          end
          inherited jvspCropConsumptiveUse: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'Crop_Consumptive_Use_Flags'
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited frameCropConsumptiveUse: TframeRadioGrid
              Width = 457
              Height = 360
              ExplicitWidth = 457
              ExplicitHeight = 360
              inherited grpDescription: TGroupBox
                Width = 454
                Height = 357
                ExplicitWidth = 454
                ExplicitHeight = 357
                inherited lblTop: TLabel
                  Width = 185
                  Height = 18
                  ExplicitWidth = 185
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 18
                  Height = 188
                  ExplicitWidth = 18
                  ExplicitHeight = 188
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 417
                  Height = 312
                  FixedCols = 0
                  OnSelectCell = frameCropConsumptiveUserdgGridSelectCell
                  ExplicitTop = 40
                  ExplicitWidth = 417
                  ExplicitHeight = 312
                end
              end
            end
          end
          inherited jvspSurfaceWater: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'Surface_Water_Flags'
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblRoutedDelivery: TLabel
              Width = 264
              Height = 18
              ExplicitWidth = 264
              ExplicitHeight = 18
            end
            inherited lblRoutedReturnFlow: TLabel
              Width = 318
              Height = 18
              ExplicitWidth = 318
              ExplicitHeight = 18
            end
            inherited lblAllotment: TLabel
              Width = 229
              Height = 18
              ExplicitWidth = 229
              ExplicitHeight = 18
            end
            inherited lblDiversionCriterion: TLabel
              Width = 260
              Height = 18
              ExplicitWidth = 260
              ExplicitHeight = 18
            end
            inherited comboRoutedDelivery: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboRoutedReturnFlow: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboAllotment: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited jvspMandatoryPrintFlags: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'Mandatory_Print_Flags'
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblSaveWellFlowRates: TLabel
              Width = 245
              Height = 18
              ExplicitWidth = 245
              ExplicitHeight = 18
            end
            inherited lblSaveRecharge: TLabel
              Top = 72
              Width = 199
              Height = 18
              ExplicitTop = 72
              ExplicitWidth = 199
              ExplicitHeight = 18
            end
            inherited lblSupplyAndDemand: TLabel
              Top = 128
              Width = 208
              Height = 18
              ExplicitTop = 128
              ExplicitWidth = 208
              ExplicitHeight = 18
            end
            inherited frameFarmBudgetPrintFlag: TframeRadioGrid
              Top = 110
              Width = 457
              Height = 250
              TabOrder = 2
              ExplicitTop = 110
              ExplicitWidth = 457
              ExplicitHeight = 250
              inherited grpDescription: TGroupBox
                Width = 454
                Height = 247
                ExplicitWidth = 454
                ExplicitHeight = 247
                inherited lblTop: TLabel
                  Width = 65
                  Height = 18
                  ExplicitWidth = 65
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 18
                  Height = 146
                  ExplicitWidth = 18
                  ExplicitHeight = 146
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 401
                  Height = 202
                  FixedCols = 0
                  ExplicitTop = 40
                  ExplicitWidth = 401
                  ExplicitHeight = 202
                end
              end
            end
            inherited comboSaveWellFlowRates: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboSaveRecharge: TComboBox
              Top = 94
              Height = 26
              ExplicitTop = 94
              ExplicitHeight = 26
            end
            inherited comboSupplyAndDemand: TComboBox
              Top = 150
              Height = 26
              TabOrder = 3
              ExplicitTop = 150
              ExplicitHeight = 26
            end
          end
          inherited jvspOptionalPrintFlags: TJvStandardPage
            Width = 457
            Height = 360
            HelpType = htKeyword
            HelpKeyword = 'Optional_Print_Flags'
            ExplicitWidth = 457
            ExplicitHeight = 360
            inherited lblDiversionBudgetLocation: TLabel
              Width = 244
              Height = 18
              ExplicitWidth = 244
              ExplicitHeight = 18
            end
            inherited frameAcreageOptimizationPrintSettings: TframeRadioGrid
              Top = 60
              Width = 457
              Height = 300
              TabOrder = 1
              ExplicitTop = 60
              ExplicitWidth = 457
              ExplicitHeight = 300
              inherited grpDescription: TGroupBox
                Width = 454
                Height = 297
                ExplicitWidth = 454
                ExplicitHeight = 297
                inherited lblTop: TLabel
                  Left = 256
                  Width = 90
                  Height = 18
                  ExplicitLeft = 256
                  ExplicitWidth = 90
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 18
                  Height = 86
                  ExplicitWidth = 18
                  ExplicitHeight = 86
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 401
                  Height = 252
                  FixedCols = 0
                  ExplicitTop = 40
                  ExplicitWidth = 401
                  ExplicitHeight = 252
                end
              end
            end
            inherited comboDiversionBudgetLocation: TComboBox
              Height = 26
              TabOrder = 2
              ExplicitHeight = 26
            end
            inherited frameRoutingInformationPrintFlag: TframeRadioGrid
              Width = 457
              ExplicitWidth = 457
              inherited grpDescription: TGroupBox
                Width = 454
                ExplicitWidth = 454
                inherited lblTop: TLabel
                  Width = 60
                  Height = 18
                  ExplicitWidth = 60
                  ExplicitHeight = 18
                end
                inherited lblLeft: TMMJLabel
                  Width = 77
                  Height = 18
                  ExplicitWidth = 77
                  ExplicitHeight = 18
                end
                inherited rdgGrid: TRbwDataGrid4
                  Top = 40
                  Width = 401
                  Height = 97
                  FixedCols = 0
                  ExplicitTop = 40
                  ExplicitWidth = 401
                  ExplicitHeight = 97
                end
              end
            end
          end
        end
        inherited tvpglstFarm: TJvPageListTreeView
          Width = 128
          Height = 360
          Items.Links = {00000000}
          ExplicitWidth = 128
          ExplicitHeight = 360
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgFrm.lblComments
            end
            item
              Control = framePkgFrm.memoComments
            end
            item
              Control = framePkgFrm.comboRecomputeFlows
            end
            item
              Control = framePkgFrm.rgAssignmentMethod
            end
            item
              Control = framePkgFrm.comboCropIrrigationRequirement
            end
            item
              Control = framePkgFrm.frameCropConsumptiveUse
            end
            item
              Control = framePkgFrm.comboSaveWellFlowRates
            end
            item
              Control = framePkgFrm.comboSaveRecharge
            end
            item
              Control = framePkgFrm.comboSupplyAndDemand
            end
            item
              Control = framePkgFrm.frameFarmBudgetPrintFlag
            end
            item
              Control = framePkgFrm.rgAssignmentMethod
            end
            item
              Control = framePkgFrm.comboRoutedDelivery
            end
            item
              Control = framePkgFrm.comboRoutedReturnFlow
            end
            item
              Control = framePkgFrm.comboAllotment
            end
            item
              Control = framePkgFrm.frameEfficiencyBehavior
            end
            item
              Control = framePkgFrm.comboDeficiency
            end
            item
              Control = framePkgFrm.comboRootingDepth
            end
            item
              Control = framePkgFrm.comboConsumptiveUse
            end
            item
              Control = framePkgFrm.comboPrecipitation
            end
            item
              Control = framePkgFrm.comboInefficiencyLosses
            end
            item
              Control = frameFmpParameterDefinition
            end>
          OnEnabledChange = framePkgFrmrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspCFP: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'CFP_Conduit_Flow_Process'
      Caption = 'jvspCFP'
      inline framePkgCFP: TframePackageCFP
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited pgcConduits: TPageControl
          Top = 135
          Width = 595
          ExplicitTop = 135
          ExplicitWidth = 595
          inherited tabCFP: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 346
            inherited lblLayerTemperature: TLabel
              Width = 339
              Height = 18
              ExplicitWidth = 339
              ExplicitHeight = 18
            end
            inherited lblRelaxationParameter: TLabel
              Width = 216
              Height = 18
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblMaxIterations: TLabel
              Width = 267
              Height = 18
              ExplicitWidth = 267
              ExplicitHeight = 18
            end
            inherited lblEpsilon: TLabel
              Width = 268
              Height = 18
              ExplicitWidth = 268
              ExplicitHeight = 18
            end
            inherited lblPipeExchange: TLabel
              Width = 401
              Height = 18
              ExplicitWidth = 401
              ExplicitHeight = 18
            end
            inherited lblPipeElevationOffset: TLabel
              Width = 252
              Height = 18
              ExplicitWidth = 252
              ExplicitHeight = 18
            end
            inherited lblElevationChoice: TLabel
              Width = 367
              Height = 18
              ExplicitWidth = 367
              ExplicitHeight = 18
            end
            inherited lblConduitTemperature: TLabel
              Width = 403
              Height = 18
              ExplicitWidth = 403
              ExplicitHeight = 18
            end
            inherited cbPipes: TCheckBox
              OnClick = framePkgCFPcbPipesClick
            end
            inherited seMaxIterations: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPipeExchange: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
            inherited comboElevationChoice: TJvImageComboBox
              Height = 28
              ItemHeight = 22
              ExplicitHeight = 28
            end
          end
          inherited tabCRCH_COC: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 522
            ExplicitHeight = 346
            inherited lblOutputInterval: TLabel
              Width = 381
              Height = 18
              ExplicitWidth = 381
              ExplicitHeight = 18
            end
            inherited cbConduitRecharge: TCheckBox
              Width = 493
              ExplicitWidth = 493
            end
            inherited seOutputInterval: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePkgCFP.lblComments
            end
            item
              Control = framePkgCFP.memoComments
            end
            item
              Control = framePkgCFP.cbPipes
            end
            item
              Control = framePkgCFP.cbLayers
            end>
          OnEnabledChange = framePkgCFPrcSelectionControllerEnabledChange
        end
      end
    end
    object jvspSWI: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWI2_Seawater_Intrusion_Packag'
      Caption = 'jvspSWI'
      inline framePackageSWI: TframePackageSWI
        Left = 0
        Top = 0
        Width = 595
        Height = 514
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 595
        ExplicitHeight = 514
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
          Width = 564
          ExplicitWidth = 564
        end
        inherited pcSWI: TPageControl
          Width = 595
          Height = 411
          ExplicitWidth = 595
          ExplicitHeight = 411
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitHeight = 343
            inherited lblNumberOfSurfaces: TLabel
              Width = 192
              Height = 36
              ExplicitWidth = 192
              ExplicitHeight = 36
            end
            inherited lblDensityChoice: TLabel
              Left = 364
              Width = 188
              Height = 18
              ExplicitLeft = 364
              ExplicitWidth = 188
              ExplicitHeight = 18
            end
            inherited lblObservations: TLabel
              Left = 256
              Width = 172
              Height = 18
              ExplicitLeft = 256
              ExplicitWidth = 172
              ExplicitHeight = 18
            end
            inherited lblToeslope: TLabel
              Width = 287
              Height = 18
              ExplicitWidth = 287
              ExplicitHeight = 18
            end
            inherited lblTipSlope: TLabel
              Width = 273
              Height = 18
              ExplicitWidth = 273
              ExplicitHeight = 18
            end
            inherited lblAlpha: TLabel
              Top = 182
              Width = 407
              Height = 18
              ExplicitTop = 182
              ExplicitWidth = 407
              ExplicitHeight = 18
            end
            inherited lblBeta: TLabel
              Width = 344
              Height = 18
              ExplicitWidth = 344
              ExplicitHeight = 18
            end
            inherited lblMaxAdaptiveSteps: TLabel
              Top = 252
              Width = 410
              Height = 36
              ExplicitTop = 252
              ExplicitWidth = 410
              ExplicitHeight = 36
            end
            inherited lblMinAdaptiveSteps: TLabel
              Top = 290
              Width = 406
              Height = 36
              ExplicitTop = 290
              ExplicitWidth = 406
              ExplicitHeight = 36
            end
            inherited lblAdaptiveFactor: TLabel
              Left = 93
              Top = 325
              Width = 458
              Height = 36
              ExplicitLeft = 93
              ExplicitTop = 325
              ExplicitWidth = 458
              ExplicitHeight = 36
            end
            inherited comboObservations: TJvImageComboBox
              Width = 238
              Height = 28
              DroppedWidth = 238
              ItemHeight = 22
              ExplicitWidth = 238
              ExplicitHeight = 28
            end
            inherited seNumberOfSurfaces: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited cbSaveZeta: TCheckBox
              Width = 301
              ExplicitWidth = 301
            end
            inherited comboDensityChoice: TJvImageComboBox
              Width = 346
              Height = 28
              DroppedWidth = 346
              ItemHeight = 22
              ExplicitWidth = 346
              ExplicitHeight = 28
            end
            inherited cbAdaptive: TCheckBox
              Width = 429
              ExplicitWidth = 429
            end
            inherited seMaxAdaptiveSteps: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seMinAdaptiveSteps: TJvSpinEdit
              Top = 293
              Height = 26
              ExplicitTop = 293
              ExplicitHeight = 26
            end
            inherited rdeAdaptiveFactor: TRbwDataEntry
              Left = 13
              Top = 330
              ExplicitLeft = 13
              ExplicitTop = 330
            end
          end
          inherited tabSolver: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 587
            ExplicitHeight = 378
            inherited lblSolver: TLabel
              Left = 139
              Width = 183
              Height = 18
              ExplicitLeft = 139
              ExplicitWidth = 183
              ExplicitHeight = 18
            end
            inherited lblPrintoutInterval: TLabel
              Top = 35
              Width = 177
              Height = 18
              ExplicitTop = 35
              ExplicitWidth = 177
              ExplicitHeight = 18
            end
            inherited lblPCGPrintControl: TLabel
              Top = 68
              Width = 182
              Height = 18
              Caption = 'Printing control (MUTSOL)'
              ExplicitTop = 68
              ExplicitWidth = 182
              ExplicitHeight = 18
            end
            inherited lblMaxIterOuter: TLabel
              Width = 319
              Height = 18
              ExplicitWidth = 319
              ExplicitHeight = 18
            end
            inherited lblMaxIterInner: TLabel
              Width = 303
              Height = 18
              ExplicitWidth = 303
              ExplicitHeight = 18
            end
            inherited lblPCGMethod: TLabel
              Left = 281
              Top = 151
              Width = 292
              Height = 18
              ExplicitLeft = 281
              ExplicitTop = 151
              ExplicitWidth = 292
              ExplicitHeight = 18
            end
            inherited lblMaxZetaChange: TLabel
              Width = 249
              Height = 18
              ExplicitWidth = 249
              ExplicitHeight = 18
            end
            inherited lblMaxRes: TLabel
              Width = 206
              Height = 18
              ExplicitWidth = 206
              ExplicitHeight = 18
            end
            inherited lblRelax: TLabel
              Width = 216
              Height = 18
              ExplicitWidth = 216
              ExplicitHeight = 18
            end
            inherited lblEigenValue: TLabel
              Left = 187
              Width = 316
              Height = 18
              ExplicitLeft = 187
              ExplicitWidth = 316
              ExplicitHeight = 18
            end
            inherited lblDamp: TLabel
              Width = 168
              Height = 18
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited lblDampT: TLabel
              Width = 276
              Height = 18
              ExplicitWidth = 276
              ExplicitHeight = 18
            end
            inherited comboSolver: TJvImageComboBox
              Width = 130
              Height = 28
              ItemHeight = 22
              ExplicitWidth = 130
              ExplicitHeight = 28
            end
            inherited sePrintoutInterval: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPCGPrint: TJvImageComboBox
              Height = 26
              ItemHeight = 18
              ExplicitHeight = 26
            end
            inherited seMaxIterOuter: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seMaxIterInner: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboPCGPrecondMeth: TJvImageComboBox
              Top = 148
              Width = 273
              Height = 26
              DroppedWidth = 273
              ItemHeight = 18
              ExplicitTop = 148
              ExplicitWidth = 273
              ExplicitHeight = 26
            end
            inherited comboEigenValue: TJvImageComboBox
              Width = 178
              Height = 26
              ItemHeight = 18
              ExplicitWidth = 178
              ExplicitHeight = 26
            end
          end
          inherited tabDensity: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 343
            inherited rdgDensity: TRbwDataGrid4
              Width = 587
              Height = 378
              FixedCols = 0
              ExplicitWidth = 587
              ExplicitHeight = 343
            end
          end
        end
        inherited rcSelectionController: TRbwController
          ControlList = <
            item
              Control = framePackageSWI.lblComments
            end
            item
              Control = framePackageSWI.memoComments
            end
            item
              Control = framePackageSWI.seNumberOfSurfaces
            end
            item
              Control = framePackageSWI.comboDensityChoice
            end
            item
              Control = framePackageSWI.cbSaveZeta
            end
            item
              Control = framePackageSWI.comboObservations
            end
            item
              Control = framePackageSWI.rdeToeslope
            end
            item
              Control = framePackageSWI.rdeTipSlope
            end
            item
              Control = framePackageSWI.rdeAlpha
            end
            item
              Control = framePackageSWI.rdeBeta
            end
            item
              Control = framePackageSWI.cbAdaptive
            end
            item
              Control = framePackageSWI.comboSolver
            end
            item
              Control = framePackageSWI.sePrintoutInterval
            end
            item
              Control = framePackageSWI.comboPCGPrint
            end
            item
              Control = framePackageSWI.rdgDensity
            end>
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 514
    Width = 782
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      782
      41)
    object btnHelp: TBitBtn
      Left = 441
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 555
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 669
      Top = 6
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 514
    Align = alLeft
    TabOrder = 0
    object pnlModel: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 64
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblModel: TLabel
        Left = 8
        Top = 11
        Width = 43
        Height = 18
        Caption = 'Model'
      end
      object comboModel: TComboBox
        Left = 8
        Top = 32
        Width = 162
        Height = 26
        Style = csDropDownList
        TabOrder = 0
        OnChange = comboModelChange
      end
    end
    object tvPackages: TTreeView
      Left = 1
      Top = 65
      Width = 175
      Height = 448
      Align = alClient
      HideSelection = False
      Indent = 20
      ReadOnly = True
      StateImages = ilCheckImages
      TabOrder = 1
      OnChange = tvPackagesChange
      OnCustomDrawItem = tvPackagesCustomDrawItem
      OnMouseUp = tvPackagesMouseUp
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
    Left = 72
    Top = 216
  end
  object ilCheckImages: TImageList
    Left = 136
    Top = 96
    Bitmap = {
      494C0101080009002C0110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
    Left = 48
    Top = 256
  end
end
