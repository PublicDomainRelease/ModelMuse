object frameHeadObservations: TframeHeadObservations
  Left = 0
  Top = 0
  Width = 568
  Height = 350
  TabOrder = 0
  TabStop = True
  OnResize = FrameResize
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 25
    Align = alTop
    Caption = 'pnlCaption'
    TabOrder = 0
  end
  object pnlName: TPanel
    Left = 0
    Top = 25
    Width = 568
    Height = 114
    Align = alTop
    TabOrder = 1
    object lblTreatment: TLabel
      Left = 200
      Top = 6
      Width = 50
      Height = 13
      Caption = 'Treatment'
    end
    object edObsName: TLabeledEdit
      Left = 8
      Top = 22
      Width = 121
      Height = 21
      EditLabel.Width = 88
      EditLabel.Height = 13
      EditLabel.Caption = 'Observation name'
      MaxLength = 12
      OEMConvert = True
      TabOrder = 0
      OnChange = edObsNameChange
      OnExit = edObsNameExit
    end
    object rgMultiObsMethod: TRadioGroup
      Left = 9
      Top = 49
      Width = 368
      Height = 56
      Caption = 'How will observations be analyzed? (ITT)'
      Enabled = False
      ItemIndex = 1
      Items.Strings = (
        'All heads (1)'
        'Calculate drawdown relative to first head (2)')
      TabOrder = 1
      OnClick = rgMultiObsMethodClick
    end
    object comboTreatment: TComboBox
      Left = 200
      Top = 22
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'Observation'
      OnChange = comboTreatmentChange
      Items.Strings = (
        'Observation'
        'Prediction'
        'Inactive')
    end
  end
  object pcData: TJvPageControl
    Left = 0
    Top = 139
    Width = 568
    Height = 211
    ActivePage = tabTimes
    Align = alClient
    TabOrder = 2
    object tabTimes: TTabSheet
      Caption = 'Observation times'
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 560
        Height = 35
        Align = alTop
        TabOrder = 0
        object rdeMultiValueEdit: TRbwDataEntry
          Left = 8
          Top = 5
          Width = 61
          Height = 22
          ItemHeight = 13
          TabOrder = 0
          Text = '0'
          OnChange = rdeMultiValueEditChange
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object comboMultiStatFlag: TJvImageComboBox
          Left = 99
          Top = 5
          Width = 89
          Height = 23
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          DroppedWidth = 145
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 17
          ItemIndex = -1
          TabOrder = 1
          OnChange = comboMultiStatFlagChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Variance'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Standard dev.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coef. of var.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Weight'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Sq. rt. of weight'
            end>
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 107
        Width = 560
        Height = 76
        Align = alBottom
        TabOrder = 1
        object lblNumberOfTimes: TLabel
          Left = 63
          Top = 9
          Width = 78
          Height = 13
          Caption = 'Number of times'
        end
        object seTimes: TJvSpinEdit
          Left = 8
          Top = 6
          Width = 49
          Height = 21
          CheckMinValue = True
          ButtonKind = bkClassic
          TabOrder = 0
          OnChange = seTimesChange
        end
        object btnDeleteValue: TButton
          Left = 8
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Delete'
          TabOrder = 1
          OnClick = btnDeleteValueClick
        end
        object btnInsertValue: TButton
          Left = 71
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Insert'
          TabOrder = 2
          OnClick = btnInsertValueClick
        end
      end
      object rdgHeads: TRbwDataGrid4
        Left = 0
        Top = 35
        Width = 560
        Height = 72
        Align = alClient
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
        TabOrder = 2
        OnExit = rdgHeadsExit
        OnMouseDown = rdgHeadsMouseDown
        OnSelectCell = rdgHeadsSelectCell
        OnSetEditText = rdgHeadsSetEditText
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = rdgHeadsColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgHeadsHorizontalScroll
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
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
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
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
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Variance (0)'
              'Standard dev. (1)'
              'Coef. of var. (2)'
              'Weight (3)'
              'Sq. rt. of weight (4)')
            WordWrapCaptions = False
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
            WordWrapCaptions = False
            WordWrapCells = True
            AutoAdjustColWidths = False
          end>
        OnEndUpdate = rdgHeadsEndUpdate
        ColWidths = (
          64
          64
          64
          127
          213)
      end
    end
    object tabLayers: TTabSheet
      Caption = 'Layers'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 107
        Width = 560
        Height = 76
        Align = alBottom
        TabOrder = 0
        object lblNumberOfLayers: TLabel
          Left = 64
          Top = 9
          Width = 82
          Height = 13
          Caption = 'Number of layers'
        end
        object seLayers: TJvSpinEdit
          Left = 8
          Top = 6
          Width = 49
          Height = 21
          CheckMinValue = True
          ButtonKind = bkClassic
          TabOrder = 0
          OnChange = seLayersChange
        end
        object btnDeleteLayer: TButton
          Left = 8
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Delete'
          TabOrder = 1
          OnClick = btnDeleteLayerClick
        end
        object btnInsertLayer: TButton
          Left = 71
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Insert'
          TabOrder = 2
          OnClick = btnInsertLayerClick
        end
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 560
        Height = 35
        Align = alTop
        TabOrder = 1
        object rdeMultiLayerEdit: TRbwDataEntry
          Left = 3
          Top = 7
          Width = 61
          Height = 22
          ItemHeight = 13
          TabOrder = 0
          Text = '0'
          OnChange = rdeMultiLayerEditChange
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
      object rdgLayers: TRbwDataGrid4
        Left = 0
        Top = 35
        Width = 560
        Height = 72
        Align = alClient
        ColCount = 2
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
        TabOrder = 2
        OnExit = rdgLayersExit
        OnMouseDown = rdgLayersMouseDown
        OnSelectCell = rdgLayersSelectCell
        OnSetEditText = rdgLayersSetEditText
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
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
          end>
      end
    end
  end
end
