inherited frmImportAsciiRaster: TfrmImportAsciiRaster
  Width = 582
  Height = 304
  HelpKeyword = 'Import_ASCII_Raster_File'
  Caption = 'Import ASCII Raster File'
  ExplicitWidth = 582
  ExplicitHeight = 304
  PixelsPerInch = 120
  TextHeight = 18
  inherited lblDataSet: TLabel
    Top = 7
    Anchors = [akLeft, akBottom]
    ExplicitTop = 305
  end
  inherited lblInterpolator: TLabel
    Top = 59
    Anchors = [akLeft, akBottom]
    ExplicitTop = 357
  end
  inherited comboDataSets: TComboBox
    Top = 31
    Width = 519
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    ExplicitTop = 329
    ExplicitWidth = 519
  end
  inherited comboInterpolators: TComboBox
    Top = 80
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    ExplicitTop = 378
  end
  inherited cbEnclosedCells: TCheckBox
    Left = 132
    Top = 2
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    Visible = False
    ExplicitLeft = 132
    ExplicitTop = 300
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 112
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    ExplicitTop = 410
  end
  inherited cbInterpolation: TCheckBox
    Left = 8
    Top = 144
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    ExplicitLeft = 8
    ExplicitTop = 442
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 209
    Anchors = [akLeft, akBottom]
    TabOrder = 8
    ExplicitTop = 507
  end
  inherited btnOK: TBitBtn
    Top = 221
    Anchors = [akLeft, akBottom]
    TabOrder = 10
    OnClick = btnOKClick
    ExplicitTop = 519
  end
  inherited btnCancel: TBitBtn
    Top = 221
    Anchors = [akLeft, akBottom]
    TabOrder = 11
    ExplicitTop = 519
  end
  inherited btnHelp: TBitBtn
    Top = 221
    Anchors = [akLeft, akBottom]
    TabOrder = 9
    ExplicitTop = 519
  end
  object rgFilterMethod: TRadioGroup [11]
    Left = 306
    Top = 67
    Width = 250
    Height = 140
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Filter method'
    ItemIndex = 2
    Items.Strings = (
      'Lowest point in cell'
      'Highest point in cell'
      'Average of points in cell'
      'Point closest to cell center'
      'None')
    TabOrder = 3
  end
  object rdgFilesAndDataSets: TRbwDataGrid4 [12]
    Left = 528
    Top = 8
    Width = 49
    Height = 65
    ColCount = 3
    DefaultColWidth = 20
    FixedCols = 1
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 1
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
    Top = 181
    Width = 281
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 7
    ExplicitTop = 479
  end
  inherited OpenDialogFile: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    FilterIndex = 1
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Open a ASCII raster file(s)'
    Left = 536
    Top = 80
  end
end
