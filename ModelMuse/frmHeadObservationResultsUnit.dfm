inherited frmHeadObservationResults: TfrmHeadObservationResults
  HelpType = htKeyword
  HelpKeyword = 'Head_Observation_Results'
  Caption = 'Head Observation Results'
  ClientHeight = 397
  ClientWidth = 521
  ExplicitWidth = 529
  ExplicitHeight = 431
  PixelsPerInch = 96
  TextHeight = 18
  object pnl1: TPanel
    Left = 0
    Top = 348
    Width = 521
    Height = 49
    Align = alBottom
    TabOrder = 0
    object btnClose: TBitBtn
      Left = 412
      Top = 6
      Width = 101
      Height = 33
      Caption = '&Close'
      DoubleBuffered = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00388888888877
        F7F787F8888888888333333F00004444400888FFF444448888888888F333FF8F
        000033334D5007FFF4333388888888883338888F0000333345D50FFFF4333333
        338F888F3338F33F000033334D5D0FFFF43333333388788F3338F33F00003333
        45D50FEFE4333333338F878F3338F33F000033334D5D0FFFF43333333388788F
        3338F33F0000333345D50FEFE4333333338F878F3338F33F000033334D5D0FFF
        F43333333388788F3338F33F0000333345D50FEFE4333333338F878F3338F33F
        000033334D5D0EFEF43333333388788F3338F33F0000333345D50FEFE4333333
        338F878F3338F33F000033334D5D0EFEF43333333388788F3338F33F00003333
        4444444444333333338F8F8FFFF8F33F00003333333333333333333333888888
        8888333F00003333330000003333333333333FFFFFF3333F00003333330AAAA0
        333333333333888888F3333F00003333330000003333333333338FFFF8F3333F
        0000}
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnCloseClick
    end
    object btnHelp: TBitBtn
      Left = 198
      Top = 6
      Width = 101
      Height = 33
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object btnApply: TBitBtn
      Left = 305
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
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object comboModels: TComboBox
      Left = 7
      Top = 6
      Width = 180
      Height = 26
      Style = csDropDownList
      TabOrder = 3
      OnChange = comboModelsChange
    end
  end
  object pcHeadObs: TPageControl
    Left = 0
    Top = 0
    Width = 521
    Height = 348
    ActivePage = tabControls
    Align = alClient
    TabOrder = 1
    object tabControls: TTabSheet
      Caption = 'Controls'
      object lblNegativeColor: TLabel
        Left = 3
        Top = 234
        Width = 190
        Height = 18
        Caption = 'Color for negative residuals'
      end
      object lblColorPositive: TLabel
        Left = 255
        Top = 234
        Width = 185
        Height = 18
        Caption = 'Color for positive residuals'
      end
      object lblMaxSymbolSize: TLabel
        Left = 130
        Top = 283
        Width = 206
        Height = 18
        Caption = 'Maximum symbol size (pixels)'
      end
      object lblHeadObsResults: TLabel
        Left = 3
        Top = 3
        Width = 69
        Height = 18
        Caption = 'File name'
      end
      object flnmedHeadObsResults: TJvFilenameEdit
        Left = 3
        Top = 27
        Width = 505
        Height = 26
        Filter = 'Head Observation Output files (*.hob_out)|*.hob_out'
        TabOrder = 0
        OnChange = flnmedHeadObsResultsChange
      end
      object grpbxFilter: TGroupBox
        Left = 3
        Top = 82
        Width = 505
        Height = 146
        Caption = 'Filters'
        TabOrder = 1
        object lblMaximumTime: TLabel
          Left = 252
          Top = 86
          Width = 101
          Height = 18
          Caption = 'Maximum time'
        end
        object lblMaxResidual: TLabel
          Left = 252
          Top = 24
          Width = 126
          Height = 18
          Caption = 'Maximum residual'
        end
        object lblMinimumTime: TLabel
          Left = 3
          Top = 86
          Width = 97
          Height = 18
          Caption = 'Minimum time'
        end
        object lblMinResidual: TLabel
          Left = 3
          Top = 24
          Width = 122
          Height = 18
          Caption = 'Minimum residual'
        end
        inline framelmtMaximumTime: TframeDisplayLimit
          Left = 3
          Top = 107
          Width = 243
          Height = 35
          HorzScrollBar.Range = 188
          VertScrollBar.Range = 30
          TabOrder = 2
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 107
          inherited cbCheck: TCheckBox
            Height = 23
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
        end
        inline framelmtMaxResidual: TframeDisplayLimit
          Left = 252
          Top = 48
          Width = 243
          Height = 35
          HorzScrollBar.Range = 188
          VertScrollBar.Range = 30
          TabOrder = 1
          TabStop = True
          ExplicitLeft = 252
          ExplicitTop = 48
          inherited cbCheck: TCheckBox
            Height = 23
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
        end
        inline framelmtMinimumTime: TframeDisplayLimit
          Left = 252
          Top = 107
          Width = 243
          Height = 35
          HorzScrollBar.Range = 188
          VertScrollBar.Range = 30
          TabOrder = 3
          TabStop = True
          ExplicitLeft = 252
          ExplicitTop = 107
          inherited cbCheck: TCheckBox
            Height = 23
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
        end
        inline framelmtMinResidual: TframeDisplayLimit
          Left = 3
          Top = 48
          Width = 243
          Height = 35
          HorzScrollBar.Range = 188
          VertScrollBar.Range = 30
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 48
          inherited cbCheck: TCheckBox
            Height = 23
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            Height = 26
            ExplicitHeight = 26
          end
        end
      end
      object clrbtnNegative: TJvColorButton
        Left = 3
        Top = 255
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 2
        TabStop = False
      end
      object clrbtnPositive: TJvColorButton
        Left = 255
        Top = 255
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 3
        TabStop = False
      end
      object spinSymbolSize: TJvSpinEdit
        Left = 3
        Top = 280
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 4
      end
      object cbShow: TCheckBox
        Left = 6
        Top = 59
        Width = 158
        Height = 17
        Caption = 'Show residuals'
        TabOrder = 5
      end
    end
    object tabValues: TTabSheet
      Caption = 'Values (read only)'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 305
      object rdgHeadObs: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 513
        Height = 315
        Align = alClient
        ColCount = 8
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = True
        ColorSelectedRow = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end>
        ExplicitTop = -2
      end
    end
  end
end
