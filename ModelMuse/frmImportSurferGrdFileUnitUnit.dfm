inherited frmImportSurferGrdFile: TfrmImportSurferGrdFile
  Width = 578
  Height = 406
  HelpKeyword = 'Import_Surfer_Grid_File'
  Caption = 'Import Surfer Grid File'
  ExplicitWidth = 578
  ExplicitHeight = 406
  PixelsPerInch = 120
  TextHeight = 18
  inherited lblDataSet: TLabel
    Top = 136
    ExplicitTop = 136
  end
  inherited lblInterpolator: TLabel
    Top = 192
    ExplicitTop = 192
  end
  object Label1: TLabel [2]
    Left = 8
    Top = 8
    Width = 99
    Height = 18
    Caption = 'Grid file extent'
  end
  inherited comboDataSets: TComboBox
    Top = 160
    Width = 547
    TabOrder = 1
    ExplicitTop = 160
    ExplicitWidth = 547
  end
  inherited comboInterpolators: TComboBox
    Top = 213
    TabOrder = 2
    ExplicitTop = 213
  end
  inherited cbEnclosedCells: TCheckBox
    Left = 496
    Top = 277
    Width = 41
    TabOrder = 5
    Visible = False
    ExplicitLeft = 496
    ExplicitTop = 277
    ExplicitWidth = 41
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 245
    ExplicitTop = 245
  end
  inherited cbInterpolation: TCheckBox
    Left = 8
    Top = 276
    ExplicitLeft = 8
    ExplicitTop = 276
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 313
    TabOrder = 6
    ExplicitTop = 313
  end
  inherited btnOK: TBitBtn
    Top = 324
    TabOrder = 8
    OnClick = btnOKClick
    ExplicitTop = 324
  end
  inherited btnCancel: TBitBtn
    Top = 324
    TabOrder = 9
    ExplicitTop = 324
  end
  inherited btnHelp: TBitBtn
    Top = 324
    TabOrder = 7
    ExplicitTop = 324
  end
  object rdgLimits: TRbwDataGrid4 [12]
    Left = 8
    Top = 29
    Width = 545
    Height = 89
    ColCount = 4
    FixedCols = 1
    RowCount = 3
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
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
        CaseSensitivePicklist = False
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
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        AutoAdjustColWidths = True
      end>
  end
  inherited OpenDialogFile: TOpenDialog
    Filter = 'Surfer grid file (*.grd, *.dat)|*.grd;*.dat|All files (*.*)|*.*'
    Title = 'Open a Surfer grid file'
    Top = 184
  end
end
