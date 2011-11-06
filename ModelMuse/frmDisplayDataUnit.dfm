inherited frmDisplayData: TfrmDisplayData
  HelpType = htKeyword
  HelpKeyword = 'Data_Visualization_Dialog_Box'
  Caption = 'Data Visualization'
  ClientHeight = 472
  ClientWidth = 779
  ExplicitWidth = 787
  ExplicitHeight = 506
  PixelsPerInch = 96
  TextHeight = 18
  object splSplit: TSplitter
    Left = 201
    Top = 0
    Width = 5
    Height = 431
    ExplicitLeft = 178
    ExplicitHeight = 420
  end
  object pglstMain: TJvPageList
    Left = 206
    Top = 0
    Width = 573
    Height = 431
    ActivePage = jvspHeadObsResults
    PropagateEnable = False
    Align = alClient
    OnChange = pglstMainChange
    object jvspModpathPathline: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 431
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Display_Dialog_Box'
      Caption = 'jvspModpathPathline'
      inline frameModpathDisplay: TframeModpathDisplay
        Left = 0
        Top = 0
        Width = 573
        Height = 431
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 431
        inherited pcMain: TPageControl
          Width = 573
          Height = 431
          ActivePage = frameModpathDisplay.tabOptions
          ExplicitWidth = 573
          ExplicitHeight = 431
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 457
            ExplicitHeight = 279
            inherited lblModpathFile: TLabel
              Width = 161
              Height = 18
              ExplicitWidth = 161
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Width = 97
              Height = 18
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Width = 546
              ExplicitWidth = 495
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 453
              Width = 47
              Height = 18
              ExplicitLeft = 345
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited lblMaxTime: TLabel
              Width = 79
              Height = 18
              ExplicitWidth = 79
              ExplicitHeight = 18
            end
            inherited fedModpathFile: TJvFilenameEdit
              Width = 546
              Height = 26
              ExplicitWidth = 546
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Width = 546
              Height = 26
              ExplicitWidth = 546
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 453
              ExplicitLeft = 453
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 398
            inherited rgShow2D: TRadioGroup
              Width = 550
              Anchors = [akLeft, akTop, akRight]
              ExplicitWidth = 550
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 407
              Height = 266
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 407
              ExplicitHeight = 266
            end
          end
        end
      end
    end
    object jvspStreamLinks: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 431
      HelpType = htKeyword
      HelpKeyword = 'Stream_Links_Pane'
      Caption = 'jvspStreamLinks'
      inline frameStreamLink: TframeStreamLink
        Left = 0
        Top = 0
        Width = 573
        Height = 431
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 431
        inherited shpStreamColor: TShape
          Left = 472
          ExplicitLeft = 472
        end
        inherited shpDiversionColor: TShape
          Left = 472
          ExplicitLeft = 472
        end
        inherited lblTimeToPlot: TLabel
          Top = 216
          Width = 85
          Height = 18
          ExplicitTop = 216
          ExplicitWidth = 85
          ExplicitHeight = 18
        end
        inherited shpUnconnectedColor: TShape
          Left = 472
          ExplicitLeft = 472
        end
        inherited btnStreamColor: TButton
          Left = 248
          Width = 218
          ExplicitLeft = 248
          ExplicitWidth = 218
        end
        inherited btnDiversionColor: TButton
          Left = 248
          Width = 218
          ExplicitLeft = 248
          ExplicitWidth = 218
        end
        inherited rgItemsToPlot: TRadioGroup
          Height = 110
          ExplicitHeight = 110
        end
        inherited cbStreams: TCheckBox
          Top = 6
          Width = 185
          ExplicitTop = 6
          ExplicitWidth = 185
        end
        inherited cbPlotDiversions: TCheckBox
          Top = 37
          Width = 199
          ExplicitTop = 37
          ExplicitWidth = 199
        end
        inherited comboTimeToPlot: TJvComboBox
          Top = 213
          Height = 26
          ExplicitTop = 213
          ExplicitHeight = 26
        end
        inherited cbPlotUnconnected: TCheckBox
          Top = 68
          Width = 239
          ExplicitTop = 68
          ExplicitWidth = 239
        end
        inherited btnUnconnectedColor: TButton
          Left = 248
          Width = 218
          ExplicitLeft = 248
          ExplicitWidth = 218
        end
        inherited dlgLinkColor: TColorDialog
          Left = 232
          Top = 120
        end
      end
    end
    object jvspHeadObsResults: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 431
      HelpType = htKeyword
      HelpKeyword = 'Head_Observation_Results'
      Caption = 'jvspHeadObsResults'
      inline frameHeadObservationResults: TframeHeadObservationResults
        Left = 0
        Top = 0
        Width = 573
        Height = 431
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 431
        inherited pgcHeadObs: TPageControl
          Width = 573
          Height = 390
          ExplicitWidth = 573
          ExplicitHeight = 390
          inherited tabControls: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 357
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
            ExplicitTop = 29
            ExplicitHeight = 309
            inherited rdgHeadObs: TRbwDataGrid4
              Height = 309
              ExplicitHeight = 309
            end
          end
        end
        inherited pnlBottom: TPanel
          Top = 390
          Width = 573
          ExplicitTop = 390
          ExplicitWidth = 573
          inherited comboModels: TComboBox
            Left = 259
            Top = 5
            Height = 26
            ExplicitLeft = 259
            ExplicitTop = 5
            ExplicitHeight = 26
          end
          inherited btnHightlightObjects: TButton
            Width = 246
            ExplicitWidth = 246
          end
        end
      end
    end
    object jvspModpathTimeSeries: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 431
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Time_Series_Display'
      Caption = 'jvspModpathTimeSeries'
      inline frameModpathTimeSeriesDisplay: TframeModpathTimeSeriesDisplay
        Left = 0
        Top = 0
        Width = 573
        Height = 431
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 431
        inherited pcMain: TPageControl
          Width = 573
          Height = 431
          ActivePage = frameModpathTimeSeriesDisplay.tabOptions
          ExplicitWidth = 573
          ExplicitHeight = 431
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 457
            ExplicitHeight = 365
            inherited lblModpathFile: TLabel
              Width = 184
              Height = 18
              ExplicitWidth = 184
              ExplicitHeight = 18
            end
            inherited lblTimeToPlot: TLabel
              Width = 81
              Height = 18
              ExplicitWidth = 81
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
              ExplicitLeft = 351
              ExplicitWidth = 47
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
            inherited seCycles: TJvSpinEdit
              Left = 458
              ExplicitLeft = 458
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 398
            inherited rgShow2D: TRadioGroup
              Width = 551
              ExplicitWidth = 551
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 398
              Height = 284
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 398
              ExplicitHeight = 284
            end
          end
        end
      end
    end
    object jvspModpathEndpoints: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 431
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Endpoint_Display'
      Caption = 'jvspModpathEndpoints'
      inline frameModpathEndpointDisplay1: TframeModpathEndpointDisplay
        Left = 0
        Top = 0
        Width = 573
        Height = 431
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 431
        inherited pcMain: TPageControl
          Width = 573
          Height = 431
          ActivePage = frameModpathEndpointDisplay1.tabOptions
          ExplicitWidth = 573
          ExplicitHeight = 431
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 468
            ExplicitHeight = 326
            inherited lblModpathFile: TLabel
              Width = 167
              Height = 18
              ExplicitWidth = 167
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
              ExplicitLeft = 364
              ExplicitWidth = 47
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
            inherited seCycles: TJvSpinEdit
              Left = 462
              ExplicitLeft = 462
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 398
            inherited rdgLimits: TRbwDataGrid4
              Width = 377
              Height = 318
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 377
              ExplicitHeight = 318
            end
          end
        end
      end
    end
    object jvspColorGrid: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 431
      HelpType = htKeyword
      HelpKeyword = 'Color_Grid_Dialog_Box'
      Caption = 'jvspColorGrid'
      inline frameColorGrid: TframeColorGrid
        Left = 0
        Top = 0
        Width = 573
        Height = 431
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 431
        inherited pcChoices: TPageControl
          Width = 573
          Height = 431
          ExplicitWidth = 573
          ExplicitHeight = 431
          inherited tabSelection: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 398
            inherited lblDataSet: TLabel
              Width = 212
              Height = 18
              ExplicitWidth = 212
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Top = 239
              Width = 97
              Height = 18
              ExplicitTop = 236
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 459
              Top = 266
              Width = 47
              Height = 18
              ExplicitLeft = 494
              ExplicitTop = 263
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Top = 303
              Width = 400
              ExplicitTop = 270
              ExplicitWidth = 438
            end
            inherited lblColorAdjustment: TLabel
              Top = 342
              Width = 117
              Height = 18
              ExplicitTop = 339
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
              Left = 459
              Width = 35
              Height = 18
              ExplicitLeft = 497
              ExplicitWidth = 35
              ExplicitHeight = 18
            end
            inherited reComment: TJvRichEdit
              Width = 552
              Height = 71
              ParentFont = True
              TabOrder = 3
              ExplicitWidth = 552
              ExplicitHeight = 71
            end
            inherited comboColorScheme: TComboBox
              Top = 258
              Width = 445
              Height = 26
              TabOrder = 5
              ExplicitTop = 258
              ExplicitWidth = 445
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 459
              Top = 303
              Height = 26
              TabOrder = 7
              ExplicitLeft = 459
              ExplicitTop = 303
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Top = 358
              TabOrder = 6
            end
            inherited seColorExponent: TJvSpinEdit
              Top = 369
              Height = 26
              TabOrder = 10
              ExplicitTop = 369
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Top = 373
              ExplicitTop = 373
            end
            inherited udDataSets: TJvUpDown
              Left = 428
              Height = 26
              ExplicitLeft = 428
              ExplicitHeight = 26
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 159
              TabOrder = 4
              ExplicitTop = 159
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Width = 420
              Height = 26
              ExplicitWidth = 436
              ExplicitHeight = 26
            end
            inherited udTime: TJvUpDown
              Left = 544
              Height = 26
              TabOrder = 2
              ExplicitLeft = 544
              ExplicitHeight = 26
            end
            inherited comboTime3D: TJvComboBox
              Left = 459
              Height = 26
              TabOrder = 1
              ExplicitLeft = 459
              ExplicitHeight = 26
            end
          end
          inherited tabFilters: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 600
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
              Width = 113
              Height = 18
              ExplicitWidth = 113
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
              inherited cbCheck: TCheckBox
                TabOrder = 2
              end
              inherited rdeLimit: TRbwDataEntry
                TabOrder = 0
              end
              inherited comboBoolLimit: TComboBox
                Height = 26
                TabOrder = 1
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
            ExplicitWidth = 600
            ExplicitHeight = 395
            inherited imLegend: TImage
              Height = 395
              ExplicitHeight = 395
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
                Width = 109
                Height = 18
                ExplicitTop = 339
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
    object jvspContourData: TJvStandardPage
      Left = 0
      Top = 0
      Width = 573
      Height = 431
      HelpType = htKeyword
      HelpKeyword = 'Contour_Data_Dialog_Box'
      Caption = 'jvspContourData'
      inline frameContourData: TframeContourData
        Left = 0
        Top = 0
        Width = 573
        Height = 431
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 573
        ExplicitHeight = 431
        inherited pcChoices: TPageControl
          Width = 573
          Height = 431
          ExplicitWidth = 573
          ExplicitHeight = 431
          inherited tabSelection: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 565
            ExplicitHeight = 398
            inherited lblDataSet: TLabel
              Width = 212
              Height = 18
              ExplicitWidth = 212
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Top = 239
              Width = 97
              Height = 18
              ExplicitTop = 236
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 459
              Top = 266
              Width = 47
              Height = 18
              ExplicitLeft = 456
              ExplicitTop = 263
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Top = 303
              Width = 400
              ExplicitTop = 303
              ExplicitWidth = 400
            end
            inherited lblColorAdjustment: TLabel
              Top = 342
              Width = 117
              Height = 18
              ExplicitTop = 339
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblComment: TLabel
              Width = 204
              Height = 18
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited reComment: TJvRichEdit
              Width = 552
              Height = 71
              ParentFont = True
              TabOrder = 4
              ExplicitWidth = 552
              ExplicitHeight = 71
            end
            inherited comboColorScheme: TComboBox
              Top = 258
              Width = 445
              Height = 26
              TabOrder = 8
              ExplicitTop = 258
              ExplicitWidth = 445
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 459
              Top = 303
              Height = 26
              TabOrder = 9
              ExplicitLeft = 459
              ExplicitTop = 303
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Top = 358
              TabOrder = 10
            end
            inherited seColorExponent: TJvSpinEdit
              Top = 369
              Height = 26
              TabOrder = 11
              ExplicitTop = 369
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Top = 373
              TabOrder = 12
              ExplicitTop = 373
            end
            inherited udDataSets: TJvUpDown
              TabOrder = 2
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 159
              TabOrder = 5
              ExplicitTop = 159
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Height = 26
              TabOrder = 1
              ExplicitHeight = 26
            end
            inherited btnEditContours: TButton
              Left = 446
              TabOrder = 3
              ExplicitLeft = 446
            end
            inherited cbSpecifyContours: TJvCheckBox
              Left = 345
              TabOrder = 0
              ExplicitLeft = 345
            end
            inherited cbLabelContours: TCheckBox
              TabOrder = 6
            end
            inherited btnContourFont: TButton
              TabOrder = 7
            end
          end
          inherited tabFilters: TTabSheet
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
              Width = 113
              Height = 18
              ExplicitWidth = 113
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
              inherited cbCheck: TCheckBox
                TabOrder = 2
              end
              inherited rdeLimit: TRbwDataEntry
                TabOrder = 0
              end
              inherited comboBoolLimit: TComboBox
                Height = 26
                TabOrder = 1
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
            ExplicitTop = 29
            ExplicitHeight = 395
            inherited imLegend: TImage
              Height = 395
              ExplicitHeight = 395
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
                Width = 109
                Height = 18
                ExplicitTop = 339
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
    Height = 431
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
    Top = 431
    Width = 779
    Height = 41
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      Left = 448
      Top = 6
      Width = 101
      Height = 33
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
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
      DoubleBuffered = True
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
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnApplyClick
    end
    object btnClose: TBitBtn
      Left = 662
      Top = 6
      Width = 101
      Height = 33
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
    end
  end
end
