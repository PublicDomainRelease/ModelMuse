inherited frmMeshGenerationControlVariables: TfrmMeshGenerationControlVariables
  Caption = 'Mesh Generation Control Variables'
  ClientHeight = 317
  ClientWidth = 415
  ExplicitWidth = 431
  ExplicitHeight = 355
  PixelsPerInch = 96
  TextHeight = 18
  object rdgControlVariables: TRbwDataGrid4
    Left = 0
    Top = 57
    Width = 415
    Height = 184
    Align = alClient
    ColCount = 3
    FixedCols = 1
    RowCount = 7
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
    ColorSelectedRow = False
    Columns = <
      item
        AutoAdjustRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -13
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
        CaseSensitivePicklist = False
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -13
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -13
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        AutoAdjustColWidths = True
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 241
    Width = 415
    Height = 76
    Align = alBottom
    TabOrder = 2
    object lblElementGrowthRate: TLabel
      Left = 103
      Top = 9
      Width = 138
      Height = 18
      Caption = 'Element growth rate'
    end
    object btnHelp: TBitBtn
      Left = 127
      Top = 38
      Width = 89
      Height = 33
      HelpType = htKeyword
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 222
      Top = 38
      Width = 89
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 318
      Top = 38
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
    end
    object btnResetDefaults: TButton
      Left = 6
      Top = 38
      Width = 115
      Height = 33
      Caption = 'Reset Defaults'
      TabOrder = 1
      OnClick = btnResetDefaultsClick
    end
    object rdeGrowthRate: TRbwDataEntry
      Left = 16
      Top = 6
      Width = 81
      Height = 22
      TabOrder = 0
      Text = '1'
      DataType = dtReal
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
  object rgMethod: TRadioGroup
    Left = 0
    Top = 0
    Width = 415
    Height = 57
    Align = alTop
    Caption = 'Mesh generation method'
    Columns = 2
    Items.Strings = (
      'Fishnet'
      'Irregular')
    TabOrder = 0
    OnClick = rgMethodClick
  end
end
