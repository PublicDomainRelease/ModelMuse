inherited frmTimeControl: TfrmTimeControl
  Left = 728
  Top = 245
  Width = 413
  Height = 292
  HelpType = htKeyword
  HelpKeyword = 'Time_Control_Dialog_Box'
  VertScrollBar.Range = 81
  Caption = 'PHAST Time Control'
  KeyPreview = True
  OnKeyDown = FormKeyDown
  ExplicitWidth = 413
  ExplicitHeight = 292
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 152
    Width = 405
    Height = 106
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      405
      106)
    object lblNumberOfPeriods: TLabel
      Left = 95
      Top = 9
      Width = 129
      Height = 18
      Caption = 'Number of periods'
    end
    object lblStartTime: TLabel
      Left = 95
      Top = 40
      Width = 67
      Height = 18
      Caption = 'Start time'
    end
    object seTimeCount: TJvSpinEdit
      Left = 5
      Top = 6
      Width = 84
      Height = 26
      ButtonKind = bkClassic
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      TabOrder = 0
      OnChange = seTimeCountChange
    end
    object btnOK: TBitBtn
      Left = 216
      Top = 64
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 312
      Top = 64
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 3
      Kind = bkCancel
    end
    object btnHelp: TBitBtn
      Left = 120
      Top = 64
      Width = 89
      Height = 33
      HelpType = htKeyword
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object rdeStartTime: TRbwDataEntry
      Left = 5
      Top = 37
      Width = 84
      Height = 22
      ItemHeight = 18
      TabOrder = 4
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
  end
  object dgTime: TRbwDataGrid4
    Left = 0
    Top = 33
    Width = 405
    Height = 119
    Align = alClient
    ColCount = 3
    DefaultColWidth = 180
    FixedCols = 1
    RowCount = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 17
    Font.Name = 'Microsoft Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs]
    ParentFont = False
    TabOrder = 0
    OnMouseUp = dgTimeMouseUp
    OnSetEditText = dgTimeSetEditText
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
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
        ButtonFont.Name = 'MS Sans Serif'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 25
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'MS Sans Serif'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 25
        CheckMax = False
        CheckMin = True
        ComboUsed = False
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'MS Sans Serif'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 25
        CheckMax = False
        CheckMin = True
        ComboUsed = False
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
      end>
    RowHeights = (
      24
      24)
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 405
    Height = 33
    Align = alTop
    ParentColor = True
    TabOrder = 2
    object rdeLength: TRbwDataEntry
      Left = 136
      Top = 6
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      ItemHeight = 18
      TabOrder = 0
      Text = '0'
      OnChange = rdeLengthChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
end
