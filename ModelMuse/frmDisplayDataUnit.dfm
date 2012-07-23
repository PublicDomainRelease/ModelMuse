inherited frmDisplayData: TfrmDisplayData
  HelpType = htKeyword
  HelpKeyword = 'Data_Visualization_Dialog_Box'
  Caption = 'Data Visualization'
  ClientHeight = 508
  ClientWidth = 779
  ExplicitWidth = 797
  ExplicitHeight = 553
  PixelsPerInch = 120
  TextHeight = 18
  object splSplit: TSplitter
    Left = 201
    Top = 0
    Width = 5
    Height = 467
    ExplicitLeft = 178
    ExplicitHeight = 420
  end
  object pglstMain: TJvPageList
    Left = 206
    Top = 0
    Width = 573
    Height = 467
    ActivePage = jvspContourData
    PropagateEnable = False
    Align = alClient
    OnChange = pglstMainChange
    object jvspModpathPathline: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 467
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Display_Dialog_Box'
      Caption = 'jvspModpathPathline'
      inline frameModpathDisplay: TframeModpathDisplay
        Left = 0
        Top = 0
        Width = 573
        Height = 467
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 467
        inherited pcMain: TPageControl
          Width = 573
          Height = 467
          ExplicitWidth = 573
          ExplicitHeight = 467
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 434
            inherited lblModpathFile: TLabel
              Width = 159
              Height = 18
              ExplicitWidth = 159
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Width = 47
              Height = 18
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited lblMaxTime: TLabel
              Width = 78
              Height = 18
              ExplicitWidth = 78
              ExplicitHeight = 18
            end
            inherited lblModelSelection: TLabel
              Width = 109
              Height = 18
              ExplicitWidth = 109
              ExplicitHeight = 18
            end
            inherited fedModpathFile: TJvFilenameEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Height = 23
              ExplicitHeight = 23
            end
            inherited seCycles: TJvSpinEdit
              Height = 23
              ExplicitHeight = 23
            end
            inherited comboModelSelection: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 457
            ExplicitHeight = 317
          end
        end
      end
    end
    object jvspStreamLinks: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 467
      HelpType = htKeyword
      HelpKeyword = 'Stream_Links_Pane'
      Caption = 'jvspStreamLinks'
      inline frameStreamLink: TframeStreamLink
        Left = 0
        Top = 0
        Width = 573
        Height = 467
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 467
        inherited lblTimeToPlot: TLabel
          Width = 84
          Height = 18
          ExplicitWidth = 84
          ExplicitHeight = 18
        end
        inherited comboTimeToPlot: TJvComboBox
          Height = 26
          ItemHeight = 18
          ExplicitHeight = 26
        end
      end
    end
    object jvspHeadObsResults: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 467
      HelpType = htKeyword
      HelpKeyword = 'Head_Observation_Results'
      Caption = 'jvspHeadObsResults'
      inline frameHeadObservationResults: TframeHeadObservationResults
        Left = 0
        Top = 0
        Width = 573
        Height = 467
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 467
        inherited pgcHeadObs: TPageControl
          Width = 573
          Height = 426
          ExplicitWidth = 573
          ExplicitHeight = 426
          inherited tabControls: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 393
            inherited lblNegativeColor: TLabel
              Width = 190
              Height = 18
              ExplicitWidth = 190
              ExplicitHeight = 18
            end
            inherited lblColorPositive: TLabel
              Width = 185
              Height = 18
              ExplicitWidth = 185
              ExplicitHeight = 18
            end
            inherited lblMaxSymbolSize: TLabel
              Width = 206
              Height = 18
              ExplicitWidth = 206
              ExplicitHeight = 18
            end
            inherited lblHeadObsResults: TLabel
              Width = 69
              Height = 18
              ExplicitWidth = 69
              ExplicitHeight = 18
            end
            inherited flnmedHeadObsResults: TJvFilenameEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited grpbxFilter: TGroupBox
              inherited lblMaximumTime: TLabel
                Width = 101
                Height = 18
                ExplicitWidth = 101
                ExplicitHeight = 18
              end
              inherited lblMaxResidual: TLabel
                Width = 126
                Height = 18
                ExplicitWidth = 126
                ExplicitHeight = 18
              end
              inherited lblMinimumTime: TLabel
                Width = 97
                Height = 18
                ExplicitWidth = 97
                ExplicitHeight = 18
              end
              inherited lblMinResidual: TLabel
                Width = 122
                Height = 18
                ExplicitWidth = 122
                ExplicitHeight = 18
              end
              inherited framelmtMaximumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinimumTime: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMinResidual: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
            end
            inherited spinSymbolSize: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited tabValues: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 520
            ExplicitHeight = 309
            inherited rdgHeadObs: TRbwDataGrid4
              Height = 309
              ExplicitHeight = 309
            end
          end
        end
        inherited pnlBottom: TPanel
          Top = 426
          Width = 573
          ExplicitTop = 426
          ExplicitWidth = 573
          inherited comboModels: TComboBox
            Left = 259
            Top = 5
            Height = 26
            TabOrder = 0
            ExplicitLeft = 259
            ExplicitTop = 5
            ExplicitHeight = 26
          end
          inherited btnHightlightObjects: TButton
            Width = 246
            TabOrder = 1
            ExplicitWidth = 246
          end
        end
      end
    end
    object jvspModpathTimeSeries: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 467
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Time_Series_Display'
      Caption = 'jvspModpathTimeSeries'
      inline frameModpathTimeSeriesDisplay: TframeModpathTimeSeriesDisplay
        Left = 0
        Top = 0
        Width = 573
        Height = 467
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 467
        inherited pcMain: TPageControl
          Width = 573
          Height = 467
          ActivePage = frameModpathTimeSeriesDisplay.tabOptions
          ExplicitWidth = 573
          ExplicitHeight = 467
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 457
            ExplicitHeight = 365
            inherited lblModpathFile: TLabel
              Width = 182
              Height = 18
              ExplicitWidth = 182
              ExplicitHeight = 18
            end
            inherited lblTimeToPlot: TLabel
              Width = 80
              Height = 18
              ExplicitWidth = 80
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Width = 556
              ExplicitWidth = 505
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 459
              Width = 47
              Height = 18
              ExplicitLeft = 459
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited lblModelSelection: TLabel
              Width = 109
              Height = 18
              ExplicitWidth = 109
              ExplicitHeight = 18
            end
            inherited fedModpathFile: TJvFilenameEdit
              Width = 556
              Height = 26
              ExplicitWidth = 556
              ExplicitHeight = 26
            end
            inherited comboTimeToPlot: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Width = 556
              Height = 26
              ExplicitWidth = 556
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Height = 23
              ExplicitHeight = 23
            end
            inherited seCycles: TJvSpinEdit
              Left = 458
              Height = 23
              ExplicitLeft = 458
              ExplicitHeight = 23
            end
            inherited comboModelSelection: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 434
            inherited rgShow2D: TRadioGroup
              Width = 551
              ExplicitWidth = 551
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 398
              Height = 320
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 398
              ExplicitHeight = 320
            end
          end
        end
      end
    end
    object jvspModpathEndpoints: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 467
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Endpoint_Display'
      Caption = 'jvspModpathEndpoints'
      inline frameModpathEndpointDisplay1: TframeModpathEndpointDisplay
        Left = 0
        Top = 0
        Width = 573
        Height = 467
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 467
        inherited pcMain: TPageControl
          Width = 573
          Height = 467
          ActivePage = frameModpathEndpointDisplay1.tabOptions
          ExplicitWidth = 573
          ExplicitHeight = 467
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 468
            ExplicitHeight = 326
            inherited lblModpathFile: TLabel
              Width = 165
              Height = 18
              ExplicitWidth = 165
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Width = 554
              ExplicitWidth = 503
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 461
              Width = 47
              Height = 18
              ExplicitLeft = 461
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited lblModelSelection: TLabel
              Width = 109
              Height = 18
              ExplicitWidth = 109
              ExplicitHeight = 18
            end
            inherited fedModpathFile: TJvFilenameEdit
              Width = 554
              Height = 26
              ExplicitWidth = 554
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Width = 554
              Height = 26
              ExplicitWidth = 554
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Height = 23
              ExplicitHeight = 23
            end
            inherited seCycles: TJvSpinEdit
              Left = 462
              Height = 23
              ExplicitLeft = 462
              ExplicitHeight = 23
            end
            inherited comboModelSelection: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 434
            inherited rdgLimits: TRbwDataGrid4
              Width = 377
              Height = 354
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 377
              ExplicitHeight = 354
            end
          end
        end
      end
    end
    object jvspColorGrid: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 467
      HelpType = htKeyword
      HelpKeyword = 'Color_Grid_Dialog_Box'
      Caption = 'jvspColorGrid'
      inline frameColorGrid: TframeColorGrid
        Left = 0
        Top = 0
        Width = 573
        Height = 467
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 467
        inherited pcChoices: TPageControl
          Width = 573
          Height = 467
          ExplicitWidth = 573
          ExplicitHeight = 467
          inherited tabSelection: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 434
            inherited lblDataSet: TLabel
              Width = 212
              Height = 18
              ExplicitWidth = 212
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Top = 271
              Width = 97
              Height = 18
              ExplicitTop = 271
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 424
              Top = 269
              Width = 47
              Height = 18
              ExplicitLeft = 424
              ExplicitTop = 269
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Left = 8
              Top = 322
              Width = 365
              ExplicitLeft = 8
              ExplicitTop = 322
              ExplicitWidth = 365
            end
            inherited lblColorAdjustment: TLabel
              Top = 368
              Width = 117
              Height = 18
              ExplicitTop = 368
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblComment: TLabel
              Width = 204
              Height = 18
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited lblTime: TLabel
              Left = 424
              Width = 34
              Height = 18
              ExplicitLeft = 424
              ExplicitWidth = 34
              ExplicitHeight = 18
            end
            inherited comboColorScheme: TComboBox
              Top = 290
              Width = 410
              Height = 26
              ExplicitTop = 290
              ExplicitWidth = 410
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 424
              Top = 290
              Height = 26
              ExplicitLeft = 424
              ExplicitTop = 290
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Left = 8
              Top = 389
            end
            inherited seColorExponent: TJvSpinEdit
              Left = 164
              Top = 396
              Height = 26
              ExplicitLeft = 164
              ExplicitTop = 396
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Top = 401
              ExplicitTop = 401
            end
            inherited udDataSets: TJvUpDown
              Left = 401
              Height = 26
              ExplicitLeft = 401
              ExplicitHeight = 26
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 191
              ExplicitTop = 191
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Width = 393
              Height = 26
              Anchors = [akLeft, akTop, akRight]
              ExplicitWidth = 409
              ExplicitHeight = 26
            end
            inherited reComment: TRichEdit
              Width = 517
              Height = 104
              ParentFont = True
              ExplicitWidth = 517
              ExplicitHeight = 104
            end
            inherited udTime: TJvUpDown
              Left = 509
              Height = 26
              ExplicitLeft = 509
              ExplicitHeight = 26
            end
            inherited comboTime3D: TJvComboBox
              Left = 424
              Height = 26
              ItemHeight = 18
              ExplicitLeft = 424
              ExplicitHeight = 26
            end
          end
          inherited tabFilters: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitHeight = 395
            inherited lblLowerLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblUpperLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblValuesToIgnore: TLabel
              Width = 112
              Height = 18
              ExplicitWidth = 112
              ExplicitHeight = 18
            end
            inherited lblNumberOfValuesToIgnore: TLabel
              Left = 135
              Top = 402
              Width = 185
              Height = 18
              ExplicitLeft = 135
              ExplicitTop = 402
              ExplicitWidth = 185
              ExplicitHeight = 18
            end
            inherited lblEpsilon: TLabel
              Width = 168
              Height = 18
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited frameCheck3DMax: TframeDisplayLimit
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited frameCheck3DMin: TframeDisplayLimit
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited cbActiveOnly: TCheckBox
              Width = 145
              ExplicitWidth = 145
            end
            inherited rdgValuesToIgnore: TRbwDataGrid4
              Height = 281
              ExplicitHeight = 281
            end
            inherited seNumberOfValuesToIgnore: TJvSpinEdit
              Top = 399
              Height = 26
              ExplicitTop = 399
              ExplicitHeight = 26
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitHeight = 395
            inherited imLegend: TImage
              Height = 395
              ExplicitWidth = 347
              ExplicitHeight = 495
            end
            inherited pnlLegend: TPanel
              Height = 395
              ExplicitHeight = 395
              inherited lblMethod: TLabel
                Width = 52
                Height = 18
                ExplicitWidth = 52
                ExplicitHeight = 18
              end
              inherited lblColorLegendRows: TLabel
                Top = 317
                Width = 109
                Height = 18
                ExplicitTop = 317
                ExplicitWidth = 109
                ExplicitHeight = 18
              end
              inherited comboMethod: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
              inherited seLegendRows: TJvSpinEdit
                Top = 338
                Height = 26
                ExplicitTop = 338
                ExplicitHeight = 26
              end
              inherited rdgLegend: TRbwDataGrid4
                Height = 252
                ExplicitHeight = 252
              end
            end
          end
        end
      end
    end
    object jvspContourData: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 467
      HelpType = htKeyword
      HelpKeyword = 'Contour_Data_Dialog_Box'
      Caption = 'jvspContourData'
      inline frameContourData: TframeContourData
        Left = 0
        Top = 0
        Width = 573
        Height = 467
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 467
        inherited pcChoices: TPageControl
          Width = 573
          Height = 467
          ExplicitWidth = 573
          ExplicitHeight = 467
          inherited tabSelection: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 434
            inherited lblDataSet: TLabel
              Width = 212
              Height = 18
              ExplicitWidth = 212
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitTop = 247
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 459
              Top = 252
              Width = 47
              Height = 18
              ExplicitLeft = 459
              ExplicitTop = 252
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Top = 317
              Width = 400
              ExplicitTop = 309
              ExplicitWidth = 400
            end
            inherited lblColorAdjustment: TLabel
              Top = 359
              Width = 117
              Height = 18
              ExplicitTop = 324
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblComment: TLabel
              Width = 204
              Height = 18
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited comboColorScheme: TComboBox
              Top = 274
              Width = 445
              Height = 26
              TabOrder = 9
              ExplicitTop = 274
              ExplicitWidth = 445
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 459
              Top = 273
              Height = 26
              Anchors = [akRight, akBottom]
              TabOrder = 8
              ExplicitLeft = 459
              ExplicitTop = 273
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Left = 8
              Top = 383
            end
            inherited seColorExponent: TJvSpinEdit
              Left = 159
              Top = 389
              Height = 26
              ExplicitLeft = 159
              ExplicitTop = 389
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Left = 238
              Top = 389
              Height = 26
              ExplicitLeft = 238
              ExplicitTop = 389
              ExplicitHeight = 26
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 173
              ExplicitTop = 173
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Height = 26
              Anchors = [akLeft, akTop, akRight]
              OnChange = frameContourDatavirttreecomboDataSetsChange
              ExplicitHeight = 26
            end
            inherited reComment: TRichEdit
              Width = 552
              Height = 72
              ParentFont = True
              ExplicitWidth = 552
              ExplicitHeight = 72
            end
            inherited btnEditContours: TButton
              Left = 446
              ExplicitLeft = 446
            end
            inherited cbSpecifyContours: TJvCheckBox
              Left = 345
              ExplicitLeft = 345
            end
            inherited cbLabelContours: TCheckBox
              Top = 180
              Anchors = [akRight, akBottom]
              ExplicitTop = 180
            end
            inherited btnContourFont: TButton
              Top = 221
              Anchors = [akRight, akBottom]
              ExplicitTop = 221
            end
          end
          inherited tabFilters: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 562
            ExplicitHeight = 366
            inherited lblLowerLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblUpperLimit: TLabel
              Width = 75
              Height = 18
              ExplicitWidth = 75
              ExplicitHeight = 18
            end
            inherited lblValuesToIgnore: TLabel
              Width = 112
              Height = 18
              ExplicitWidth = 112
              ExplicitHeight = 18
            end
            inherited lblNumberOfValuesToIgnore: TLabel
              Top = 373
              Width = 185
              Height = 18
              ExplicitTop = 370
              ExplicitWidth = 185
              ExplicitHeight = 18
            end
            inherited lblEpsilon: TLabel
              Width = 168
              Height = 18
              ExplicitWidth = 168
              ExplicitHeight = 18
            end
            inherited frameCheck3DMax: TframeDisplayLimit
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited frameCheck3DMin: TframeDisplayLimit
              inherited comboBoolLimit: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited cbActiveOnly: TCheckBox
              Width = 177
              ExplicitWidth = 177
            end
            inherited rdgValuesToIgnore: TRbwDataGrid4
              Height = 252
              ExplicitHeight = 252
            end
            inherited seNumberOfValuesToIgnore: TJvSpinEdit
              Top = 370
              Height = 26
              ExplicitTop = 370
              ExplicitHeight = 26
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 562
            ExplicitHeight = 366
            inherited imLegend: TImage
              Height = 366
              ExplicitWidth = 347
              ExplicitHeight = 495
            end
            inherited pnlLegend: TPanel
              Height = 366
              ExplicitHeight = 366
              inherited lblMethod: TLabel
                Width = 52
                Height = 18
                ExplicitWidth = 52
                ExplicitHeight = 18
              end
              inherited lblColorLegendRows: TLabel
                Width = 109
                Height = 18
                ExplicitWidth = 109
                ExplicitHeight = 18
              end
              inherited comboMethod: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
              inherited seLegendRows: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
          end
        end
      end
    end
  end
  object tvpglstMain: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 201
    Height = 467
    PageDefault = 0
    PageList = pglstMain
    Align = alLeft
    HideSelection = False
    Indent = 20
    TabOrder = 0
    OnChanging = tvpglstMainChanging
    OnCustomDrawItem = tvpglstMainCustomDrawItem
    Items.NodeData = {
      0307000000320000000000000000000000FFFFFFFFFFFFFFFF00000000050000
      0000000000010A43006F006C006F007200200047007200690064003600000000
      00000000000000FFFFFFFFFFFFFFFF000000000600000000000000010C43006F
      006E0074006F0075007200200044006100740061004000000000000000000000
      00FFFFFFFFFFFFFFFF00000000000000000000000001114D004F004400500041
      0054004800200050006100740068006C0069006E006500730042000000000000
      0000000000FFFFFFFFFFFFFFFF00000000040000000000000001124D004F0044
      005000410054004800200045006E006400200050006F0069006E007400730044
      0000000000000000000000FFFFFFFFFFFFFFFF00000000030000000000000001
      134D004F00440050004100540048002000540069006D00650020005300650072
      006900650073004E0000000000000000000000FFFFFFFFFFFFFFFF0000000002
      000000000000000118480065006100640020004F006200730065007200760061
      00740069006F006E00200052006500730075006C007400730036000000000000
      0000000000FFFFFFFFFFFFFFFF000000000100000000000000010C5300740072
      00650061006D0020004C0069006E006B007300}
    Items.Links = {
      0700000005000000060000000000000004000000030000000200000001000000}
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 467
    Width = 779
    Height = 41
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      Left = 448
      Top = 6
      Width = 101
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnApply: TBitBtn
      Left = 555
      Top = 6
      Width = 101
      Height = 33
      Caption = 'Apply'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnApplyClick
    end
    object btnClose: TBitBtn
      Left = 662
      Top = 6
      Width = 101
      Height = 33
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 2
    end
  end
end
