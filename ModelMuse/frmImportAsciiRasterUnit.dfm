inherited frmImportAsciiRaster: TfrmImportAsciiRaster
  Width = 579
  Height = 314
  HelpKeyword = 'Import_ASCII_Raster_File'
  Caption = 'Import ASCII Raster File'
  ExplicitWidth = 579
  ExplicitHeight = 314
  PixelsPerInch = 96
  TextHeight = 18
  inherited lblDataSet: TLabel
    Top = -21
    Anchors = [akLeft, akBottom]
    ExplicitTop = 13
  end
  inherited lblInterpolator: TLabel
    Top = 31
    Anchors = [akLeft, akBottom]
    ExplicitTop = 65
  end
  inherited comboDataSets: TComboBox
    Top = 3
    Width = 519
    Anchors = [akLeft, akBottom]
    ExplicitTop = 3
    ExplicitWidth = 519
  end
  inherited comboInterpolators: TComboBox
    Top = 52
    Anchors = [akLeft, akBottom]
    ExplicitTop = 52
  end
  inherited cbEnclosedCells: TCheckBox
    Left = 132
    Top = -26
    Anchors = [akLeft, akBottom]
    Visible = False
    ExplicitLeft = 132
    ExplicitTop = -26
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 84
    Anchors = [akLeft, akBottom]
    ExplicitTop = 84
  end
  inherited cbInterpolation: TCheckBox
    Left = 8
    Top = 116
    Anchors = [akLeft, akBottom]
    ExplicitLeft = 8
    ExplicitTop = 116
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 181
    Anchors = [akLeft, akBottom]
    ExplicitTop = 181
  end
  inherited btnOK: TBitBtn
    Top = 193
    Anchors = [akLeft, akBottom]
    OnClick = btnOKClick
    ExplicitTop = 193
  end
  inherited btnCancel: TBitBtn
    Top = 193
    Anchors = [akLeft, akBottom]
    ExplicitTop = 193
  end
  inherited btnHelp: TBitBtn
    Top = 193
    Anchors = [akLeft, akBottom]
    ExplicitTop = 193
  end
  object rgFilterMethod: TRadioGroup [11]
    Left = 306
    Top = 39
    Width = 213
    Height = 108
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Filter method'
    ItemIndex = 2
    Items.Strings = (
      'Lowest point in cell'
      'Highest point in cell'
      'Average of points in cell'
      'Point closest to cell center'
      'None')
    TabOrder = 9
  end
  object rdgFilesAndDataSets: TRbwDataGrid4 [12]
    Left = 440
    Top = 0
    Width = 131
    Height = 150
    ColCount = 3
    DefaultColWidth = 20
    FixedCols = 1
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 10
    Visible = False
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = rdgFilesAndDataSetsButtonClick
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
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = ANSI_CHARSET
        ButtonFont.Color = clBlack
        ButtonFont.Height = -16
        ButtonFont.Name = 'Arial'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = True
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
        ButtonFont.Charset = ANSI_CHARSET
        ButtonFont.Color = clBlack
        ButtonFont.Height = -16
        ButtonFont.Name = 'Arial'
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        AutoAdjustColWidths = True
      end>
  end
  object comboModel: TComboBox [13]
    Left = 8
    Top = 153
    Width = 281
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 11
  end
  inherited OpenDialogFile: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    FilterIndex = 1
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Open a ASCII raster file(s)'
    Top = 120
  end
end
