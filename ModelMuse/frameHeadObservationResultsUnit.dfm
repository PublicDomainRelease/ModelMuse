object frameHeadObservationResults: TframeHeadObservationResults
  Left = 0
  Top = 0
  Width = 528
  Height = 383
  TabOrder = 0
  object pgcHeadObs: TPageControl
    Left = 0
    Top = 0
    Width = 528
    Height = 342
    ActivePage = tabControls
    Align = alClient
    TabOrder = 0
    object tabControls: TTabSheet
      Caption = 'Controls'
      object lblNegativeColor: TLabel
        Left = 3
        Top = 234
        Width = 132
        Height = 13
        Caption = 'Color for negative residuals'
      end
      object lblColorPositive: TLabel
        Left = 255
        Top = 234
        Width = 127
        Height = 13
        Caption = 'Color for positive residuals'
      end
      object lblMaxSymbolSize: TLabel
        Left = 130
        Top = 283
        Width = 139
        Height = 13
        Caption = 'Maximum symbol size (pixels)'
      end
      object lblHeadObsResults: TLabel
        Left = 3
        Top = 3
        Width = 45
        Height = 13
        Caption = 'File name'
      end
      object flnmedHeadObsResults: TJvFilenameEdit
        Left = 3
        Top = 27
        Width = 505
        Height = 21
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
        TabOrder = 2
        object lblMaximumTime: TLabel
          Left = 252
          Top = 86
          Width = 67
          Height = 13
          Caption = 'Maximum time'
        end
        object lblMaxResidual: TLabel
          Left = 252
          Top = 24
          Width = 84
          Height = 13
          Caption = 'Maximum residual'
        end
        object lblMinimumTime: TLabel
          Left = 3
          Top = 86
          Width = 63
          Height = 13
          Caption = 'Minimum time'
        end
        object lblMinResidual: TLabel
          Left = 3
          Top = 24
          Width = 80
          Height = 13
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
        end
      end
      object clrbtnNegative: TJvColorButton
        Left = 3
        Top = 255
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 3
        TabStop = False
      end
      object clrbtnPositive: TJvColorButton
        Left = 255
        Top = 255
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 4
        TabStop = False
      end
      object spinSymbolSize: TJvSpinEdit
        Left = 3
        Top = 280
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 5
      end
      object cbShow: TCheckBox
        Left = 6
        Top = 59
        Width = 158
        Height = 17
        Caption = 'Show residuals'
        TabOrder = 1
      end
    end
    object tabValues: TTabSheet
      Caption = 'Values (read only)'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rdgHeadObs: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 520
        Height = 314
        Align = alClient
        ColCount = 8
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnMouseUp = rdgHeadObsMouseUp
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = True
        ColorSelectedRow = True
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
            CaseSensitivePicklist = False
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
            CaseSensitivePicklist = False
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
            CaseSensitivePicklist = False
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
            CaseSensitivePicklist = False
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
            CaseSensitivePicklist = False
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
            CaseSensitivePicklist = False
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
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end>
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 342
    Width = 528
    Height = 41
    Align = alBottom
    TabOrder = 1
    object comboModels: TComboBox
      Left = 231
      Top = 6
      Width = 180
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = comboModelsChange
    end
    object btnHightlightObjects: TButton
      Left = 4
      Top = 6
      Width = 221
      Height = 25
      Caption = 'Highlight selected objects'
      TabOrder = 0
      OnClick = btnHightlightObjectsClick
    end
  end
end
