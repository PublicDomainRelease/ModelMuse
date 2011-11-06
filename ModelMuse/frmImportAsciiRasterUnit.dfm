inherited frmImportAsciiRaster: TfrmImportAsciiRaster
  Width = 579
  Height = 286
  HelpKeyword = 'Import_ASCII_Raster_File'
  Caption = 'Import ASCII Raster File'
  ExplicitWidth = 579
  ExplicitHeight = 286
  PixelsPerInch = 96
  TextHeight = 18
  inherited lblInterpolator: TLabel
    Top = 69
    Anchors = [akLeft, akBottom]
    ExplicitTop = 101
  end
  inherited comboDataSets: TComboBox
    Width = 535
    Anchors = [akLeft, akTop, akRight]
    ExplicitWidth = 535
  end
  inherited comboInterpolators: TComboBox
    Top = 90
    Anchors = [akLeft, akBottom]
    ExplicitTop = 122
  end
  inherited cbEnclosedCells: TCheckBox
    Left = 136
    Top = -5
    Visible = False
    ExplicitLeft = 136
    ExplicitTop = -5
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 122
    Anchors = [akLeft, akBottom]
    ExplicitTop = 154
  end
  inherited cbInterpolation: TCheckBox
    Left = 8
    Top = 154
    Anchors = [akLeft, akBottom]
    ExplicitLeft = 8
    ExplicitTop = 186
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 191
    Anchors = [akLeft, akBottom]
    ExplicitTop = 223
  end
  inherited btnOK: TBitBtn
    Top = 203
    Anchors = [akLeft, akBottom]
    OnClick = btnOKClick
    ExplicitTop = 235
  end
  inherited btnCancel: TBitBtn
    Top = 203
    Anchors = [akLeft, akBottom]
    ExplicitTop = 235
  end
  inherited btnHelp: TBitBtn
    Top = 203
    Anchors = [akLeft, akBottom]
    ExplicitTop = 235
  end
  object rgFilterMethod: TRadioGroup [11]
    Left = 306
    Top = 77
    Width = 237
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
    ExplicitTop = 109
  end
  object rdgFilesAndDataSets: TRbwDataGrid4 [12]
    Left = 0
    Top = 0
    Width = 571
    Height = 63
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
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
        AutoAdjustColWidths = True
      end>
  end
  inherited OpenDialogFile: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    FilterIndex = 1
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Open a ASCII raster file(s)'
    Top = 120
  end
end
