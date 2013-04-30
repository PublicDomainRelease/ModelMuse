object frameHeadObservationResults: TframeHeadObservationResults
  Left = 0
  Top = 0
  Width = 528
  Height = 441
  TabOrder = 0
  object pgcHeadObs: TPageControl
    Left = 0
    Top = 0
    Width = 528
    Height = 376
    ActivePage = tabControls
    Align = alClient
    TabOrder = 0
    object tabControls: TTabSheet
      Caption = 'Controls'
      object lblNegativeColor: TLabel
        Left = 3
        Top = 274
        Width = 157
        Height = 16
        Caption = 'Color for negative residuals'
      end
      object lblColorPositive: TLabel
        Left = 255
        Top = 274
        Width = 152
        Height = 16
        Caption = 'Color for positive residuals'
      end
      object lblMaxSymbolSize: TLabel
        Left = 130
        Top = 323
        Width = 171
        Height = 16
        Caption = 'Maximum symbol size (pixels)'
      end
      object lblHeadObsResults: TLabel
        Left = 3
        Top = 3
        Width = 56
        Height = 16
        Caption = 'File name'
      end
      object flnmedHeadObsResults: TJvFilenameEdit
        Left = 3
        Top = 27
        Width = 505
        Height = 24
        Filter = 'Head Observation Output files (*.hob_out)|*.hob_out'
        TabOrder = 0
        OnChange = flnmedHeadObsResultsChange
      end
      object grpbxFilter: TGroupBox
        Left = 3
        Top = 82
        Width = 505
        Height = 191
        Caption = 'Filters'
        TabOrder = 2
        object lblMaximumTime: TLabel
          Left = 252
          Top = 86
          Width = 84
          Height = 16
          Caption = 'Maximum time'
        end
        object lblMaxResidual: TLabel
          Left = 252
          Top = 24
          Width = 104
          Height = 16
          Caption = 'Maximum residual'
        end
        object lblMinimumTime: TLabel
          Left = 3
          Top = 86
          Width = 81
          Height = 16
          Caption = 'Minimum time'
        end
        object lblMinResidual: TLabel
          Left = 3
          Top = 24
          Width = 101
          Height = 16
          Caption = 'Minimum residual'
        end
        object lblMinLayer: TLabel
          Left = 3
          Top = 138
          Width = 84
          Height = 16
          Caption = 'Minimum layer'
        end
        object lblMaxLayer: TLabel
          Left = 252
          Top = 138
          Width = 87
          Height = 16
          Caption = 'Maximum layer'
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
            ExplicitHeight = 24
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
            ExplicitHeight = 24
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
            ExplicitHeight = 24
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
            ExplicitHeight = 24
          end
        end
        inline framelmtMinLayer: TframeDisplayLimit
          Left = 3
          Top = 160
          Width = 243
          Height = 35
          HorzScrollBar.Range = 188
          VertScrollBar.Range = 30
          TabOrder = 4
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 160
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 24
          end
        end
        inline framelmtMaxLayer: TframeDisplayLimit
          Left = 252
          Top = 160
          Width = 243
          Height = 35
          HorzScrollBar.Range = 188
          VertScrollBar.Range = 30
          TabOrder = 5
          TabStop = True
          ExplicitLeft = 252
          ExplicitTop = 160
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 24
          end
        end
      end
      object clrbtnNegative: TJvColorButton
        Left = 3
        Top = 295
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 3
        TabStop = False
      end
      object clrbtnPositive: TJvColorButton
        Left = 255
        Top = 295
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 4
        TabStop = False
      end
      object spinSymbolSize: TJvSpinEdit
        Left = 3
        Top = 320
        Width = 121
        Height = 24
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 5
        OnChange = spinSymbolSizeChange
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
      object rdgHeadObs: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 520
        Height = 345
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
          end>
        WordWrapRowCaptions = False
      end
    end
    object tabLegend: TTabSheet
      Caption = 'Legend'
      ImageIndex = 2
      object shpMax: TShape
        Left = 3
        Top = 3
        Width = 65
        Height = 22
        Shape = stCircle
      end
      object shpHalfMax: TShape
        Left = 3
        Top = 31
        Width = 65
        Height = 26
        Shape = stCircle
      end
      object lblMax: TLabel
        Left = 56
        Top = 9
        Width = 36
        Height = 16
        Caption = 'lblMax'
      end
      object lblHalfMax: TLabel
        Left = 56
        Top = 41
        Width = 58
        Height = 16
        Caption = 'lblHalfMax'
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 376
    Width = 528
    Height = 65
    Align = alBottom
    TabOrder = 1
    object comboModels: TComboBox
      Left = 231
      Top = 6
      Width = 180
      Height = 24
      Style = csDropDownList
      TabOrder = 2
      OnChange = comboModelsChange
    end
    object btnHightlightObjects: TButton
      Left = 4
      Top = 6
      Width = 101
      Height = 51
      Caption = 'Highlight selected objects'
      TabOrder = 0
      WordWrap = True
      OnClick = btnHightlightObjectsClick
    end
    object btnRestore: TButton
      Left = 111
      Top = 6
      Width = 107
      Height = 25
      Caption = 'Original order'
      TabOrder = 1
      OnClick = btnRestoreClick
    end
    object btnCopy: TButton
      Left = 111
      Top = 32
      Width = 107
      Height = 25
      Caption = 'Copy to clipboard'
      TabOrder = 3
      OnClick = btnCopyClick
    end
  end
end
