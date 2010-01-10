inherited frmFormulaErrors: TfrmFormulaErrors
  Left = 496
  Top = 282
  Width = 556
  HelpType = htKeyword
  HelpKeyword = 'Formula_Errors_Dialog_Box'
  VertScrollBar.Range = 110
  ActiveControl = btnClose
  Caption = 'Formula Errors'
  Font.Height = 19
  FormStyle = fsStayOnTop
  OnResize = FormResize
  ExplicitWidth = 556
  ExplicitHeight = 274
  PixelsPerInch = 96
  TextHeight = 19
  object pnlBottom: TPanel
    Left = 0
    Top = 195
    Width = 548
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      548
      45)
    object btnClose: TBitBtn
      Left = 457
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkClose
    end
    object btnCopy: TButton
      Left = 8
      Top = 6
      Width = 81
      Height = 33
      Hint = 'Copy table to clipboard'
      Caption = 'Copy'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnCopyClick
    end
    object btnHelp: TBitBtn
      Left = 368
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnClear: TButton
      Left = 95
      Top = 6
      Width = 81
      Height = 33
      Caption = 'Clear'
      TabOrder = 3
      OnClick = btnClearClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 548
    Height = 65
    Align = alTop
    ParentColor = True
    TabOrder = 1
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 546
      Height = 63
      Align = alClient
      Caption = 'The following formulas were invalid. They may have been reset.'
      WordWrap = True
      ExplicitWidth = 459
      ExplicitHeight = 19
    end
  end
  object sgErrors: TRbwDataGrid4
    Left = 0
    Top = 65
    Width = 548
    Height = 130
    Align = alClient
    ColCount = 4
    DefaultColWidth = 100
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 2
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
    ColorSelectedRow = True
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
        AutoAdjustColWidths = True
      end>
    RowHeights = (
      24
      25)
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 144
    Top = 112
  end
end