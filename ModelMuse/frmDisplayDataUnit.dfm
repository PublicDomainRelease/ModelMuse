inherited frmDisplayData: TfrmDisplayData
  HelpType = htKeyword
  HelpKeyword = 'Data_Visualization_Dialog_Box'
  Caption = 'Data Visualization'
  ClientHeight = 542
  ClientWidth = 784
  ExplicitWidth = 800
  ExplicitHeight = 580
  PixelsPerInch = 96
  TextHeight = 18
  object splSplit: TSplitter
    Left = 201
    Top = 0
    Width = 5
    Height = 501
    ExplicitLeft = 178
    ExplicitHeight = 420
  end
  object pglstMain: TJvPageList
    Left = 206
    Top = 0
    Width = 578
    Height = 501
    ActivePage = jvspContourData
    PropagateEnable = False
    Align = alClient
    OnChange = pglstMainChange
    object jvspModpathPathline: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Display_Dialog_Box'
      Caption = 'jvspModpathPathline'
      inline frameModpathDisplay: TframeModpathDisplay
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited pcMain: TPageControl
          Width = 578
          Height = 501
          ExplicitWidth = 578
          ExplicitHeight = 501
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 570
            ExplicitHeight = 468
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
            inherited pbColorScheme: TPaintBox
              Width = 551
              ExplicitWidth = 548
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 458
              Width = 47
              Height = 18
              ExplicitLeft = 455
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
              Width = 551
              Height = 26
              ExplicitWidth = 551
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Width = 551
              Height = 26
              ExplicitWidth = 551
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 458
              Height = 26
              ExplicitLeft = 458
              ExplicitHeight = 26
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
            inherited rgShow2D: TRadioGroup
              Width = 550
              Items.Strings = (
                'Show all'
                'Specify columns, rows, layers, times and/or groups to show'
                
                  'Specify starting columns, rows, layers, times and/or groups to s' +
                  'how'
                
                  'Specify ending columns, rows, layers, times and/or groups to sho' +
                  'w')
              ExplicitWidth = 550
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 407
              Height = 212
              ExplicitWidth = 407
              ExplicitHeight = 212
            end
          end
        end
      end
    end
    object jvspSfrStreamLinks: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'Stream_Links_Pane'
      Caption = 'jvspSfrStreamLinks'
      inline frameSfrStreamLink: TframeStreamLink
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited shpStreamColor: TShape
          Left = 448
          Top = 4
          ExplicitLeft = 448
          ExplicitTop = 4
        end
        inherited shpDiversionColor: TShape
          Left = 448
          Top = 35
          ExplicitLeft = 448
          ExplicitTop = 35
        end
        inherited lblTimeToPlot: TLabel
          Top = 218
          Width = 84
          Height = 18
          ExplicitTop = 218
          ExplicitWidth = 84
          ExplicitHeight = 18
        end
        inherited shpUnconnectedColor: TShape
          Left = 448
          Top = 66
          ExplicitLeft = 448
          ExplicitTop = 66
        end
        inherited btnStreamColor: TButton
          Left = 242
          Top = 4
          ExplicitLeft = 242
          ExplicitTop = 4
        end
        inherited btnDiversionColor: TButton
          Left = 242
          Top = 35
          ExplicitLeft = 242
          ExplicitTop = 35
        end
        inherited rgItemsToPlot: TRadioGroup
          Height = 112
          ExplicitHeight = 112
        end
        inherited cbStreams: TCheckBox
          Width = 150
          ExplicitWidth = 150
        end
        inherited cbPlotDiversions: TCheckBox
          Width = 166
          ExplicitWidth = 166
        end
        inherited comboTimeToPlot: TJvComboBox
          Top = 215
          Height = 26
          ItemHeight = 18
          ExplicitTop = 215
          ExplicitHeight = 26
        end
        inherited cbPlotUnconnected: TCheckBox
          Width = 222
          ExplicitWidth = 222
        end
        inherited btnUnconnectedColor: TButton
          Left = 242
          Top = 66
          ExplicitLeft = 242
          ExplicitTop = 66
        end
      end
    end
    object jvspHeadObsResults: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'Head_Observation_Results'
      Caption = 'jvspHeadObsResults'
      inline frameHeadObservationResults: TframeHeadObservationResults
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited pgcHeadObs: TPageControl
          Width = 578
          Height = 465
          ExplicitWidth = 578
          ExplicitHeight = 465
          inherited tabControls: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 520
            ExplicitHeight = 399
            inherited lblNegativeColor: TLabel
              Width = 190
              Height = 36
              ExplicitWidth = 190
              ExplicitHeight = 36
            end
            inherited lblColorPositive: TLabel
              Width = 185
              Height = 36
              ExplicitWidth = 185
              ExplicitHeight = 36
            end
            inherited lblMaxSymbolSize: TLabel
              Top = 351
              Width = 206
              Height = 18
              ExplicitTop = 351
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
              inherited lblMinLayer: TLabel
                Width = 100
                Height = 18
                ExplicitWidth = 100
                ExplicitHeight = 18
              end
              inherited lblMaxLayer: TLabel
                Width = 104
                Height = 18
                ExplicitWidth = 104
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
              inherited framelmtMinLayer: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
              inherited framelmtMaxLayer: TframeDisplayLimit
                inherited comboBoolLimit: TComboBox
                  Height = 26
                  ExplicitHeight = 26
                end
              end
            end
            inherited clrbtnNegative: TJvColorButton
              Top = 321
              ExplicitTop = 321
            end
            inherited clrbtnPositive: TJvColorButton
              Top = 321
              ExplicitTop = 321
            end
            inherited spinSymbolSize: TJvSpinEdit
              Top = 348
              Height = 26
              ExplicitTop = 348
              ExplicitHeight = 26
            end
          end
          inherited tabValues: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 520
            ExplicitHeight = 399
            inherited rdgHeadObs: TRbwDataGrid4
              Height = 334
              ExplicitHeight = 334
            end
            inherited pnlValueControls: TPanel
              Top = 334
              ExplicitTop = 334
              inherited btnCopy: TButton
                Left = 151
                Width = 146
                ExplicitLeft = 151
                ExplicitWidth = 146
              end
              inherited btnHightlightObjects: TButton
                Top = 2
                Width = 141
                Height = 55
                ExplicitTop = 2
                ExplicitWidth = 141
                ExplicitHeight = 55
              end
              inherited btnRestore: TButton
                Left = 151
                Top = 2
                Width = 146
                ExplicitLeft = 151
                ExplicitTop = 2
                ExplicitWidth = 146
              end
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 520
            ExplicitHeight = 399
            inherited lblMax: TLabel
              Width = 44
              Height = 18
              ExplicitWidth = 44
              ExplicitHeight = 18
            end
            inherited lblHalfMax: TLabel
              Width = 71
              Height = 18
              ExplicitWidth = 71
              ExplicitHeight = 18
            end
          end
          inherited tabGraph: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 570
            ExplicitHeight = 432
            inherited pbHeadObs: TPaintBox
              Width = 570
              Height = 351
              ExplicitWidth = 570
              ExplicitHeight = 403
            end
            inherited pnlGraphControls: TPanel
              Top = 351
              Width = 570
              ExplicitTop = 351
              ExplicitWidth = 570
              inherited lblGraphInstructions: TLabel
                Width = 201
                Height = 18
                Anchors = [akLeft, akTop, akRight]
                ExplicitWidth = 201
                ExplicitHeight = 18
              end
            end
          end
        end
        inherited pnlBottom: TPanel
          Top = 465
          Width = 578
          ExplicitTop = 465
          ExplicitWidth = 578
          inherited lblRMS: TLabel
            Width = 222
            Height = 18
            ExplicitWidth = 222
            ExplicitHeight = 18
          end
          inherited comboModels: TComboBox
            Top = 4
            Height = 26
            ExplicitTop = 4
            ExplicitHeight = 26
          end
        end
      end
    end
    object jvspModpathTimeSeries: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Time_Series_Display'
      Caption = 'jvspModpathTimeSeries'
      inline frameModpathTimeSeriesDisplay: TframeModpathTimeSeriesDisplay
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited pcMain: TPageControl
          Width = 578
          Height = 501
          ExplicitWidth = 578
          ExplicitHeight = 501
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 570
            ExplicitHeight = 468
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
              Width = 548
              ExplicitWidth = 545
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 450
              Width = 47
              Height = 18
              ExplicitLeft = 473
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
              Top = 32
              Width = 548
              Height = 26
              ExplicitTop = 32
              ExplicitWidth = 548
              ExplicitHeight = 26
            end
            inherited comboTimeToPlot: TComboBox
              Height = 26
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Left = 2
              Top = 190
              Width = 548
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 190
              ExplicitWidth = 548
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Height = 26
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 450
              Height = 26
              ExplicitLeft = 450
              ExplicitHeight = 26
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
            ExplicitHeight = 365
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
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'MODPATH_Endpoint_Display'
      Caption = 'jvspModpathEndpoints'
      inline frameModpathEndpointDisplay1: TframeModpathEndpointDisplay
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited pcMain: TPageControl
          Width = 578
          Height = 501
          ActivePage = frameModpathEndpointDisplay1.tabBasic
          ExplicitWidth = 578
          ExplicitHeight = 501
          inherited tabBasic: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 570
            ExplicitHeight = 468
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
              Width = 548
              ExplicitWidth = 545
            end
            inherited lblColorAdjustment: TLabel
              Width = 117
              Height = 18
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 574
              Width = 47
              Height = 18
              ExplicitLeft = 472
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
              Top = 30
              Width = 548
              Height = 26
              ExplicitTop = 30
              ExplicitWidth = 548
              ExplicitHeight = 26
            end
            inherited comboColorScheme: TComboBox
              Width = 548
              Height = 26
              ExplicitWidth = 548
              ExplicitHeight = 26
            end
            inherited seColorExponent: TJvSpinEdit
              Top = 235
              Height = 26
              ExplicitTop = 235
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 574
              Top = 235
              Width = 84
              Height = 26
              ExplicitLeft = 574
              ExplicitTop = 235
              ExplicitWidth = 84
              ExplicitHeight = 26
            end
            inherited comboModelSelection: TComboBox
              Left = 2
              Top = 297
              Height = 26
              ExplicitLeft = 2
              ExplicitTop = 297
              ExplicitHeight = 26
            end
          end
          inherited tabOptions: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitWidth = 468
            ExplicitHeight = 326
            inherited rgShow2D: TRadioGroup
              Width = 316
              ExplicitWidth = 316
            end
            inherited rgWhereToPlot: TRadioGroup
              Left = 331
              ExplicitLeft = 331
            end
            inherited rgColorBy: TRadioGroup
              Height = 270
              ExplicitHeight = 270
            end
            inherited rdgLimits: TRbwDataGrid4
              Width = 379
              Height = 354
              Anchors = [akLeft, akTop, akRight, akBottom]
              ExplicitWidth = 379
              ExplicitHeight = 354
            end
          end
        end
      end
    end
    object jvspColorGrid: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'Color_Grid_Dialog_Box'
      Caption = 'jvspColorGrid'
      inline frameColorGrid: TframeColorGrid
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited pcChoices: TPageControl
          Width = 578
          Height = 501
          ExplicitWidth = 578
          ExplicitHeight = 501
          inherited tabSelection: TTabSheet
            ExplicitLeft = 4
            ExplicitTop = 29
            ExplicitHeight = 395
            inherited lblDataSet: TLabel
              Width = 212
              Height = 18
              ExplicitWidth = 212
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Top = 305
              Width = 97
              Height = 18
              ExplicitTop = 271
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 429
              Top = 303
              Width = 47
              Height = 18
              ExplicitLeft = 424
              ExplicitTop = 269
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Left = 8
              Top = 356
              Width = 370
              ExplicitLeft = 8
              ExplicitTop = 322
              ExplicitWidth = 365
            end
            inherited lblColorAdjustment: TLabel
              Top = 402
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
              Left = 429
              Width = 34
              Height = 18
              ExplicitLeft = 424
              ExplicitWidth = 34
              ExplicitHeight = 18
            end
            inherited comboColorScheme: TComboBox
              Top = 324
              Width = 415
              Height = 26
              ExplicitTop = 324
              ExplicitWidth = 415
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 429
              Top = 324
              Height = 26
              ExplicitLeft = 429
              ExplicitTop = 324
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Left = 8
              Top = 423
            end
            inherited seColorExponent: TJvSpinEdit
              Left = 164
              Top = 430
              Height = 26
              ExplicitLeft = 164
              ExplicitTop = 430
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Top = 435
              ExplicitTop = 435
            end
            inherited udDataSets: TJvUpDown
              Left = 406
              Height = 26
              ExplicitLeft = 406
              ExplicitHeight = 26
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 225
              Width = 308
              ExplicitTop = 225
              ExplicitWidth = 308
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Width = 398
              Height = 26
              Anchors = [akLeft, akTop, akRight]
              ExplicitWidth = 418
              ExplicitHeight = 26
            end
            inherited reComment: TRichEdit
              Width = 522
              Height = 138
              ParentFont = True
              ExplicitWidth = 522
              ExplicitHeight = 138
            end
            inherited btnColorSchemes: TButton
              Left = 348
              Top = 257
              Width = 120
              ExplicitLeft = 348
              ExplicitTop = 257
              ExplicitWidth = 120
            end
            inherited udTime: TJvUpDown
              Left = 514
              Height = 26
              ExplicitLeft = 514
              ExplicitHeight = 26
            end
            inherited comboTime3D: TJvComboBox
              Left = 429
              Height = 26
              ItemHeight = 18
              ExplicitLeft = 429
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
            ExplicitWidth = 570
            ExplicitHeight = 468
            inherited imLegend: TImage
              Width = 352
              Height = 468
              ExplicitWidth = 382
              ExplicitHeight = 468
            end
            inherited pnlLegend: TPanel
              Height = 468
              ExplicitHeight = 468
              inherited lblMethod: TLabel
                Width = 52
                Height = 18
                ExplicitWidth = 52
                ExplicitHeight = 18
              end
              inherited lblColorLegendRows: TLabel
                Top = 390
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
                Top = 411
                Height = 26
                ExplicitTop = 338
                ExplicitHeight = 26
              end
              inherited rdgLegend: TRbwDataGrid4
                Height = 325
                ExplicitHeight = 325
              end
              inherited btnFont: TButton
                Top = 441
                Anchors = [akLeft, akBottom]
                ExplicitTop = 441
              end
            end
          end
        end
        inherited dlgFontLegend: TFontDialog
          Top = 456
        end
      end
    end
    object jvspContourData: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'Contour_Data_Dialog_Box'
      Caption = 'jvspContourData'
      inline frameContourData: TframeContourData
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited pcChoices: TPageControl
          Width = 578
          Height = 501
          ActivePage = frameContourData.tabLegend
          ExplicitWidth = 578
          ExplicitHeight = 501
          inherited tabSelection: TTabSheet
            ExplicitTop = 29
            ExplicitHeight = 366
            inherited lblDataSet: TLabel
              Width = 59
              Height = 18
              Caption = 'Data set'
              ExplicitWidth = 59
              ExplicitHeight = 18
            end
            inherited lblColorScheme: TLabel
              Top = 314
              Width = 97
              Height = 18
              ExplicitTop = 314
              ExplicitWidth = 97
              ExplicitHeight = 18
            end
            inherited lblCycles: TLabel
              Left = 469
              Top = 313
              Width = 47
              Height = 18
              ExplicitLeft = 466
              ExplicitTop = 309
              ExplicitWidth = 47
              ExplicitHeight = 18
            end
            inherited pbColorScheme: TPaintBox
              Top = 367
              Width = 410
              ExplicitTop = 363
              ExplicitWidth = 407
            end
            inherited lblColorAdjustment: TLabel
              Top = 404
              Width = 117
              Height = 18
              ExplicitTop = 404
              ExplicitWidth = 117
              ExplicitHeight = 18
            end
            inherited lblComment: TLabel
              Width = 204
              Height = 18
              ExplicitWidth = 204
              ExplicitHeight = 18
            end
            inherited lblAlgorithm: TLabel
              Left = 8
              Top = 179
              Width = 52
              Height = 18
              Anchors = [akLeft, akBottom]
              ExplicitLeft = 8
              ExplicitTop = 175
              ExplicitWidth = 52
              ExplicitHeight = 18
            end
            inherited lblContourInterval: TLabel
              Left = 440
              Top = 61
              Width = 107
              Height = 18
              Anchors = [akTop, akRight]
              ExplicitLeft = 463
              ExplicitTop = 61
              ExplicitWidth = 107
              ExplicitHeight = 18
            end
            inherited lblSpacing: TLabel
              Left = 337
              Top = 231
              Width = 152
              Height = 18
              Anchors = [akRight, akBottom]
              ExplicitLeft = 337
              ExplicitTop = 231
              ExplicitWidth = 152
              ExplicitHeight = 18
            end
            inherited comboColorScheme: TComboBox
              Top = 335
              Width = 455
              Height = 26
              TabOrder = 13
              ExplicitTop = 335
              ExplicitWidth = 455
              ExplicitHeight = 26
            end
            inherited seCycles: TJvSpinEdit
              Left = 469
              Top = 334
              Height = 26
              Anchors = [akRight, akBottom]
              ExplicitLeft = 469
              ExplicitTop = 334
              ExplicitHeight = 26
            end
            inherited jsColorExponent: TJvxSlider
              Left = 8
              Top = 427
              TabOrder = 14
            end
            inherited seColorExponent: TJvSpinEdit
              Left = 159
              Top = 430
              Height = 26
              TabOrder = 16
              ExplicitLeft = 159
              ExplicitTop = 430
              ExplicitHeight = 26
            end
            inherited cbLogTransform: TCheckBox
              Left = 238
              Top = 427
              Height = 26
              TabOrder = 15
              ExplicitLeft = 238
              ExplicitTop = 427
              ExplicitHeight = 26
            end
            inherited udDataSets: TJvUpDown
              Left = 296
              Top = 28
              TabOrder = 3
              ExplicitLeft = 296
              ExplicitTop = 28
            end
            inherited rgUpdateLimitChoice: TRadioGroup
              Top = 235
              Width = 315
              TabOrder = 9
              ExplicitTop = 235
              ExplicitWidth = 315
            end
            inherited virttreecomboDataSets: TRbwStringTreeCombo
              Top = 25
              Width = 315
              Height = 26
              Anchors = [akLeft, akTop, akRight]
              OnChange = frameContourDatavirttreecomboDataSetsChange
              ExplicitTop = 25
              ExplicitWidth = 315
              ExplicitHeight = 26
            end
            inherited reComment: TRichEdit
              Top = 85
              Width = 562
              Height = 88
              ParentFont = True
              ExplicitTop = 85
              ExplicitWidth = 562
              ExplicitHeight = 88
            end
            inherited btnColorSchemes: TButton
              Left = 337
              Top = 284
              Width = 121
              Height = 40
              TabOrder = 11
              ExplicitLeft = 337
              ExplicitTop = 284
              ExplicitWidth = 121
              ExplicitHeight = 40
            end
            inherited btnEditContours: TButton
              Left = 440
              Top = 26
              TabOrder = 2
              ExplicitLeft = 440
              ExplicitTop = 26
            end
            inherited cbSpecifyContours: TJvCheckBox
              Left = 346
              Top = 18
              HotTrackFont.Charset = ANSI_CHARSET
              HotTrackFont.Height = -16
              HotTrackFont.Name = 'Arial'
              HotTrackFont.Pitch = fpVariable
              ExplicitLeft = 346
              ExplicitTop = 18
            end
            inherited cbLabelContours: TCheckBox
              Left = 337
              Top = 179
              Height = 16
              Anchors = [akRight, akBottom]
              TabOrder = 6
              ExplicitLeft = 337
              ExplicitTop = 179
              ExplicitHeight = 16
            end
            inherited btnContourFont: TButton
              Left = 337
              Top = 201
              Height = 24
              Anchors = [akRight, akBottom]
              TabOrder = 7
              ExplicitLeft = 337
              ExplicitTop = 201
              ExplicitHeight = 24
            end
            inherited comboAlgorithm: TComboBox
              Left = 8
              Top = 203
              Height = 26
              Anchors = [akLeft, akBottom]
              TabOrder = 8
              ExplicitLeft = 8
              ExplicitTop = 203
              ExplicitHeight = 26
            end
            inherited rdeContourInterval: TRbwDataEntry
              Left = 346
              Top = 58
              Width = 79
              Anchors = [akTop, akRight]
              ExplicitLeft = 346
              ExplicitTop = 58
              ExplicitWidth = 79
            end
            inherited seLabelSpacing: TJvSpinEdit
              Left = 337
              Top = 252
              Height = 26
              Anchors = [akRight, akBottom]
              TabOrder = 10
              ExplicitLeft = 337
              ExplicitTop = 252
              ExplicitHeight = 26
            end
          end
          inherited tabFilters: TTabSheet
            ExplicitTop = 29
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
              Top = 475
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
              Height = 354
              ExplicitHeight = 354
            end
            inherited seNumberOfValuesToIgnore: TJvSpinEdit
              Top = 472
              Height = 26
              ExplicitTop = 472
              ExplicitHeight = 26
            end
          end
          inherited tabLegend: TTabSheet
            ExplicitTop = 29
            ExplicitWidth = 570
            ExplicitHeight = 468
            inherited imLegend: TImage
              Width = 352
              Height = 468
              ExplicitWidth = 344
              ExplicitHeight = 468
            end
            inherited pnlLegend: TPanel
              Height = 468
              ExplicitHeight = 468
              inherited lblMethod: TLabel
                Width = 52
                Height = 18
                ExplicitWidth = 52
                ExplicitHeight = 18
              end
              inherited lblColorLegendRows: TLabel
                Top = 390
                Width = 109
                Height = 18
                ExplicitTop = 390
                ExplicitWidth = 109
                ExplicitHeight = 18
              end
              inherited comboMethod: TComboBox
                Height = 26
                ExplicitHeight = 26
              end
              inherited seLegendRows: TJvSpinEdit
                Top = 411
                Height = 26
                ExplicitTop = 411
                ExplicitHeight = 26
              end
              inherited rdgLegend: TRbwDataGrid4
                Height = 325
                ExplicitHeight = 325
              end
              inherited btnFont: TButton
                Top = 440
                Anchors = [akLeft, akBottom]
                ExplicitTop = 440
              end
            end
          end
        end
      end
    end
    object jvspVectors: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'Vectors_Pane'
      Caption = 'jvspVectors'
      inline frameVectors: TframeVectors
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        HelpType = htKeyword
        HelpKeyword = 'Vectors_Pane'
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited lblScale: TLabel
          Width = 130
          Height = 18
          ExplicitWidth = 130
          ExplicitHeight = 18
        end
        inherited lblMaxColor: TLabel
          Width = 105
          Height = 18
          ExplicitWidth = 105
          ExplicitHeight = 18
        end
        inherited lblMidColor: TLabel
          Width = 85
          Height = 18
          ExplicitWidth = 85
          ExplicitHeight = 18
        end
        inherited lblMinColor: TLabel
          Width = 101
          Height = 18
          ExplicitWidth = 101
          ExplicitHeight = 18
        end
        inherited lblVelocityColor: TLabel
          Width = 92
          Height = 18
          ExplicitWidth = 92
          ExplicitHeight = 18
        end
        inherited lblVectorSource: TLabel
          Width = 96
          Height = 18
          ExplicitWidth = 96
          ExplicitHeight = 18
        end
        inherited lblScale3D: TLabel
          Width = 130
          Height = 18
          ExplicitWidth = 130
          ExplicitHeight = 18
        end
        inherited lblMinSpacing2D: TLabel
          Width = 247
          Height = 18
          ExplicitWidth = 247
          ExplicitHeight = 18
        end
        inherited lblMinHorizontalSpacing3D: TLabel
          Width = 217
          Height = 18
          ExplicitWidth = 217
          ExplicitHeight = 18
        end
        inherited lblMinVerticalSpacing3D: TLabel
          Width = 200
          Height = 18
          ExplicitWidth = 200
          ExplicitHeight = 18
        end
        inherited comboVectorSource: TComboBox
          Width = 543
          Height = 26
          Anchors = [akLeft, akTop, akRight]
          ExplicitWidth = 543
          ExplicitHeight = 26
        end
        inherited udVectors: TJvUpDown
          Left = 546
          Height = 26
          ExplicitLeft = 546
          ExplicitHeight = 26
        end
        inherited seMinSpacing2D: TJvSpinEdit
          Height = 26
          ExplicitHeight = 26
        end
      end
    end
    object jvspStrStreamLinks: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'Stream_Links_Pane'
      Caption = 'jvspStrStreamLinks'
      inline frameStrStreamLink: TframeStreamLink
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited shpStreamColor: TShape
          Left = 448
          Top = 4
          ExplicitLeft = 448
          ExplicitTop = 4
        end
        inherited shpDiversionColor: TShape
          Left = 448
          Top = 35
          ExplicitLeft = 448
          ExplicitTop = 35
        end
        inherited lblTimeToPlot: TLabel
          Top = 218
          Width = 84
          Height = 18
          ExplicitTop = 218
          ExplicitWidth = 84
          ExplicitHeight = 18
        end
        inherited shpUnconnectedColor: TShape
          Left = 448
          Top = 66
          ExplicitLeft = 448
          ExplicitTop = 66
        end
        inherited btnStreamColor: TButton
          Left = 242
          Top = 4
          ExplicitLeft = 242
          ExplicitTop = 4
        end
        inherited btnDiversionColor: TButton
          Left = 242
          Top = 35
          ExplicitLeft = 242
          ExplicitTop = 35
        end
        inherited rgItemsToPlot: TRadioGroup
          Height = 112
          ExplicitHeight = 112
        end
        inherited cbStreams: TCheckBox
          Width = 150
          ExplicitWidth = 150
        end
        inherited cbPlotDiversions: TCheckBox
          Width = 166
          ExplicitWidth = 166
        end
        inherited comboTimeToPlot: TJvComboBox
          Top = 215
          Height = 26
          ItemHeight = 18
          ExplicitTop = 215
          ExplicitHeight = 26
        end
        inherited cbPlotUnconnected: TCheckBox
          Width = 222
          ExplicitWidth = 222
        end
        inherited btnUnconnectedColor: TButton
          Left = 242
          Top = 66
          ExplicitLeft = 242
          ExplicitTop = 66
        end
      end
    end
    object jvspCrossSection: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'Cross_Sections_Pane'
      Caption = 'jvspCrossSection'
      inline frameDrawCrossSection: TframeDrawCrossSection
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited btnAddDataSet: TSpeedButton
          Left = 273
          ExplicitLeft = 273
        end
        inherited btnRemoveDataSet: TSpeedButton
          Left = 273
          ExplicitLeft = 273
        end
        inherited vstAvailableDataSets: TVirtualStringTree
          Width = 264
          Height = 495
          ExplicitWidth = 264
          ExplicitHeight = 495
        end
        inherited pnlUsed: TPanel
          Left = 371
          Height = 501
          ExplicitLeft = 371
          ExplicitHeight = 501
          inherited spl1: TSplitter
            Top = 367
            ExplicitTop = 318
          end
          inherited pnlTop: TPanel
            Height = 367
            ExplicitHeight = 367
            inherited lblDataSets: TLabel
              Width = 113
              Height = 18
              ExplicitWidth = 113
              ExplicitHeight = 18
            end
            inherited lstSelectedDataSets: TListBox
              Height = 334
              ExplicitHeight = 334
            end
          end
          inherited pnlBottom: TPanel
            Top = 372
            ExplicitTop = 372
            inherited lblLayers: TLabel
              Width = 93
              Height = 18
              ExplicitWidth = 93
              ExplicitHeight = 18
            end
            inherited clbLayers: TJvCheckListBox
              ItemHeight = 18
            end
          end
        end
      end
    end
    object jvspSwrReachConnections: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'SWR_Reach_Connections'
      Caption = 'jvspSwrReachConnections'
      inline frameSwrReachConnections: TframeSwrReachConnections
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited shpReachColor: TShape
          Left = 453
          Top = 4
          ExplicitLeft = 453
          ExplicitTop = 4
        end
        inherited shpUnconnectedColor: TShape
          Left = 453
          Top = 34
          ExplicitLeft = 453
          ExplicitTop = 34
        end
        inherited shpStructureColor: TShape
          Left = 453
          Top = 64
          ExplicitLeft = 453
          ExplicitTop = 64
        end
        inherited cbReaches: TCheckBox
          Width = 150
          OnClick = nil
          ExplicitWidth = 150
        end
        inherited btnReachColor: TButton
          Left = 247
          Top = 4
          OnClick = frameSwrReachConnectionsbtnReachColorClick
          ExplicitLeft = 247
          ExplicitTop = 4
        end
        inherited cbPlotUnconnected: TCheckBox
          Width = 206
          OnClick = nil
          ExplicitWidth = 206
        end
        inherited btnUnconnectedColor: TButton
          Left = 247
          Top = 34
          OnClick = frameSwrReachConnectionsbtnUnconnectedColorClick
          ExplicitLeft = 247
          ExplicitTop = 34
        end
        inherited rgItemsToPlot: TRadioGroup
          Height = 112
          ExplicitHeight = 112
        end
        inherited cbPlotStructures: TCheckBox
          Top = 67
          ExplicitTop = 67
        end
        inherited btnStructureColor: TButton
          Left = 247
          Top = 64
          ExplicitLeft = 247
          ExplicitTop = 64
        end
      end
    end
    object jvspSwrObsDisplay: TJvStandardPage
      Left = 0
      Top = 0
      Width = 578
      Height = 501
      HelpType = htKeyword
      HelpKeyword = 'SWR_Observations'
      Caption = 'jvspSwrObsDisplay'
      inline frameSwrObsDisplay: TframeSwrObsDisplay
        Left = 0
        Top = 0
        Width = 578
        Height = 501
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 578
        ExplicitHeight = 501
        inherited pbPlot: TPaintBox
          Width = 452
          Height = 372
          ExplicitWidth = 452
          ExplicitHeight = 436
        end
        inherited spl1: TSplitter
          Height = 372
          ExplicitHeight = 436
        end
        inherited pnlTop: TPanel
          Width = 578
          ExplicitWidth = 578
          inherited lblAnimationInterval: TLabel
            Width = 162
            Height = 18
            ExplicitWidth = 162
            ExplicitHeight = 18
          end
          inherited lblObservationFile: TLabel
            Width = 146
            Height = 18
            ExplicitWidth = 146
            ExplicitHeight = 18
          end
          inherited fedObservationFile: TJvFilenameEdit
            Height = 26
            ExplicitHeight = 26
          end
          inherited comboObservationType: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
          inherited seIncrement: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited rdgTimes: TRbwDataGrid4
          Height = 372
          ExplicitTop = 129
          ExplicitHeight = 372
        end
      end
    end
  end
  object tvpglstMain: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 201
    Height = 501
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
    Top = 501
    Width = 784
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
