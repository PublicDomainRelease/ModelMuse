inherited frmPointValues: TfrmPointValues
  HelpType = htKeyword
  HelpKeyword = 'Vertex_Values_Dialog_Box'
  Caption = 'Vertex Values'
  ClientWidth = 367
  ExplicitWidth = 375
  ExplicitHeight = 268
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 192
    Width = 367
    Height = 42
    Align = alBottom
    TabOrder = 1
    ExplicitWidth = 468
    object btnHelp: TBitBtn
      Left = 97
      Top = 6
      Width = 83
      Height = 33
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 186
      Top = 6
      Width = 83
      Height = 33
      TabOrder = 2
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 275
      Top = 6
      Width = 83
      Height = 33
      TabOrder = 3
      Kind = bkCancel
    end
    object btnAdd: TButton
      Left = 8
      Top = 6
      Width = 83
      Height = 33
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddClick
    end
  end
  object rdgValues: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 367
    Height = 192
    Align = alClient
    ColCount = 2
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 0
    OnKeyPress = rdgValuesKeyPress
    AutoMultiEdit = True
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
      end>
    ExplicitWidth = 468
  end
end
