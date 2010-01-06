inherited frmModflowTime: TfrmModflowTime
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Time_Dialog_Box'
  Caption = 'MODFLOW Time'
  ClientHeight = 327
  ClientWidth = 754
  OnDestroy = FormDestroy
  OnResize = FormResize
  ExplicitWidth = 762
  ExplicitHeight = 361
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 254
    Width = 754
    Height = 73
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      754
      73)
    object lblNumPeriods: TLabel
      Left = 67
      Top = 10
      Width = 175
      Height = 18
      Caption = 'Number of stress periods'
    end
    object lblTimeUnit: TLabel
      Left = 155
      Top = 43
      Width = 127
      Height = 18
      Caption = 'Time unit (ITMUNI)'
    end
    object btnCancel: TBitBtn
      Left = 662
      Top = 39
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 574
      Top = 39
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object seNumPeriods: TJvSpinEdit
      Left = 4
      Top = 6
      Width = 57
      Height = 26
      CheckMaxValue = False
      ButtonKind = bkClassic
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      TabOrder = 2
      OnChange = seNumPeriodsChange
    end
    object btnDelete: TButton
      Left = 574
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      TabOrder = 3
      OnClick = btnDeleteClick
    end
    object btnInsert: TButton
      Left = 662
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Caption = 'Insert'
      TabOrder = 4
      OnClick = btnInsertClick
    end
    object comboTimeUnit: TJvComboBox
      Left = 4
      Top = 40
      Width = 145
      Height = 26
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 5
      Text = 'seconds (1)'
      Items.Strings = (
        'undefined (0)'
        'seconds (1)'
        'minutes (2)'
        'hours (3)'
        'days (4)'
        'years (5)')
      ItemIndex = 1
    end
    object btnHelp: TBitBtn
      Left = 486
      Top = 39
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 6
      OnClick = btnHelpClick
      Kind = bkHelp
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 754
    Height = 254
    ActivePage = tabEdit
    Align = alClient
    TabOrder = 1
    object tabEdit: TTabSheet
      Caption = 'Edit'
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 746
        Height = 81
        Align = alTop
        TabOrder = 0
        object lblPeriodLength: TLabel
          Left = 183
          Top = 8
          Width = 35
          Height = 18
          Alignment = taCenter
          Caption = 'lblPe'
        end
        object lblMaxFirstTimeStepLength: TLabel
          Left = 262
          Top = 8
          Width = 47
          Height = 18
          Alignment = taCenter
          Caption = 'lblNum'
          WordWrap = True
        end
        object lblMultiplier: TLabel
          Left = 336
          Top = 8
          Width = 77
          Height = 18
          Alignment = taCenter
          Caption = 'lblMultiplier'
        end
        object lblSteadyTransient: TLabel
          Left = 412
          Top = 8
          Width = 128
          Height = 18
          Alignment = taCenter
          Caption = 'lblSteadyTransient'
          WordWrap = True
        end
        object rdePeriodLength: TRbwDataEntry
          Left = 191
          Top = 44
          Width = 65
          Height = 27
          Color = clBtnFace
          Enabled = False
          ItemHeight = 0
          TabOrder = 0
          Text = '0'
          OnChange = rdePeriodLengthChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeMaxFirstStepLength: TRbwDataEntry
          Left = 262
          Top = 44
          Width = 65
          Height = 27
          Color = clBtnFace
          Enabled = False
          ItemHeight = 0
          TabOrder = 1
          Text = '0'
          OnChange = rdeMaxFirstStepLengthChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeMultiplier: TRbwDataEntry
          Left = 341
          Top = 44
          Width = 65
          Height = 27
          Color = clBtnFace
          Enabled = False
          ItemHeight = 0
          TabOrder = 2
          Text = '0'
          OnChange = rdeMultiplierChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object comboSteadyTransient: TJvImageComboBox
          Left = 421
          Top = 44
          Width = 91
          Height = 28
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          Color = clBtnFace
          DroppedWidth = 91
          Enabled = False
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 22
          ItemIndex = -1
          TabOrder = 3
          OnChange = comboSteadyTransientChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Steady state'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Transient'
            end>
        end
      end
      object dgTime: TRbwDataGrid4
        Left = 0
        Top = 81
        Width = 746
        Height = 163
        Align = alClient
        ColCount = 9
        FixedCols = 1
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 1
        OnMouseDown = dgTimeMouseDown
        OnMouseUp = dgTimeMouseUp
        OnSelectCell = dgTimeSelectCell
        OnSetEditText = dgTimeSetEditText
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = dgTimeBeforeDrawCell
        OnButtonClick = dgTimeButtonClick
        OnColSize = dgTimeColSize
        ColorRangeSelection = False
        OnHorizontalScroll = dgTimeHorizontalScroll
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
            ButtonFont.Pitch = fpVariable
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
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
            CheckMax = False
            CheckMin = True
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
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
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
            ButtonFont.Pitch = fpVariable
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
              'Steady state'
              'Transient')
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
            ButtonFont.Pitch = fpVariable
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
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
        ColWidths = (
          64
          64
          64
          64
          74
          64
          64
          64
          64)
      end
    end
  end
end
