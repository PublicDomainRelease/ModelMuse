inherited frmSpecifyContours: TfrmSpecifyContours
  HelpType = htKeyword
  HelpKeyword = 'Specify_Contours_Dialog_Box'
  Caption = 'Specify Contours'
  ClientHeight = 301
  ClientWidth = 316
  ExplicitWidth = 324
  ExplicitHeight = 335
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 177
    Width = 316
    Height = 124
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    object lblRowCount: TLabel
      Left = 95
      Top = 9
      Width = 55
      Height = 36
      Caption = 'Number of rows'
      WordWrap = True
    end
    object btnCancel: TBitBtn
      Left = 215
      Top = 76
      Width = 91
      Height = 33
      TabOrder = 1
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 109
      Top = 76
      Width = 91
      Height = 33
      TabOrder = 2
      Kind = bkOK
    end
    object btnHelp: TBitBtn
      Left = 12
      Top = 76
      Width = 91
      Height = 33
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object GridPanel1: TGridPanel
      Left = 171
      Top = 10
      Width = 135
      Height = 32
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 33.333333333333330000
        end
        item
          Value = 33.333333333333330000
        end
        item
          Value = 33.333333333333330000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = sbAddRow
          Row = 0
        end
        item
          Column = 1
          Control = sbInsertRow
          Row = 0
        end
        item
          Column = 2
          Control = sbDeleteRow
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 3
      DesignSize = (
        135
        32)
      object sbAddRow: TSpeedButton
        Left = 10
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Add row|Add a row below the bottom row.'
        Anchors = []
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
          CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = sbAddRowClick
        ExplicitLeft = 11
        ExplicitTop = 6
      end
      object sbInsertRow: TSpeedButton
        Left = 54
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Insert row|Insert a row above the selected row.'
        Anchors = []
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
          CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = sbInsertRowClick
        ExplicitLeft = 56
        ExplicitTop = 6
      end
      object sbDeleteRow: TSpeedButton
        Left = 100
        Top = 5
        Width = 23
        Height = 22
        Hint = 'Delete row|Delete the selected row.'
        Anchors = []
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        ParentShowHint = False
        ShowHint = True
        OnClick = sbDeleteRowClick
        ExplicitLeft = 107
        ExplicitTop = 6
      end
    end
    object seRowCount: TJvSpinEdit
      Left = 12
      Top = 14
      Width = 77
      Height = 26
      ButtonKind = bkClassic
      TabOrder = 4
      OnChange = seRowCountChange
    end
    object cbAutomaticColors: TJvCheckBox
      Left = 12
      Top = 49
      Width = 137
      Height = 18
      Caption = 'Automatic colors'
      TabOrder = 5
      OnClick = cbAutomaticColorsClick
      LinkedControls = <>
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = 17
      HotTrackFont.Name = 'Microsoft Sans Serif'
      HotTrackFont.Pitch = fpVariable
      HotTrackFont.Style = []
    end
  end
  object rdgContourData: TRbwDataGrid4
    Left = 0
    Top = 33
    Width = 316
    Height = 144
    Align = alClient
    ColCount = 4
    DefaultColWidth = 20
    FixedCols = 1
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    OnMouseUp = rdgContourDataMouseUp
    OnSelectCell = rdgContourDataSelectCell
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnBeforeDrawCell = rdgContourDataBeforeDrawCell
    OnButtonClick = rdgContourDataButtonClick
    OnColSize = rdgContourDataColSize
    ColorRangeSelection = False
    OnHorizontalScroll = rdgContourDataHorizontalScroll
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
        Format = rcf4Real
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
        AutoAdjustRowHeights = True
        ButtonCaption = 'Edit'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = True
        ButtonWidth = 40
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
    OnEndUpdate = rdgContourDataEndUpdate
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 316
    Height = 33
    Align = alTop
    TabOrder = 2
    object rdeLineThickness: TRbwDataEntry
      Left = 60
      Top = 5
      Width = 79
      Height = 22
      Color = clBtnFace
      Enabled = False
      ItemHeight = 18
      TabOrder = 0
      Text = '2'
      OnChange = rdeLineThicknessChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
  object clrDlg: TJvColorDialog
    Left = 232
    Top = 88
  end
end
