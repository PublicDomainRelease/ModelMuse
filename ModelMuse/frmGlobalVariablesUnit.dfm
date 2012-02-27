inherited frmGlobalVariables: TfrmGlobalVariables
  HelpType = htKeyword
  HelpKeyword = 'Global_Variables_Dialog_Box'
  Caption = 'Global Variables'
  ClientHeight = 284
  ClientWidth = 494
  ExplicitWidth = 502
  ExplicitHeight = 318
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 178
    Width = 494
    Height = 106
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 120
    object Label1: TLabel
      Left = 71
      Top = 11
      Width = 185
      Height = 18
      Caption = 'Number of global variables'
    end
    object btnHelp: TBitBtn
      Left = 224
      Top = 72
      Width = 82
      Height = 27
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 312
      Top = 72
      Width = 82
      Height = 27
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 5
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 400
      Top = 72
      Width = 83
      Height = 27
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 6
    end
    object seGlobalVariableCount: TJvSpinEdit
      Left = 8
      Top = 6
      Width = 57
      Height = 26
      ButtonKind = bkClassic
      TabOrder = 0
      OnChange = seGlobalVariableCountChange
    end
    object btnDelete: TButton
      Left = 400
      Top = 39
      Width = 83
      Height = 27
      Caption = 'Delete'
      Enabled = False
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnImportGlobalVariables: TButton
      Left = 8
      Top = 39
      Width = 185
      Height = 27
      Caption = 'Import global variables'
      TabOrder = 1
      OnClick = btnImportGlobalVariablesClick
    end
    object btnSaveGlobalVariables: TButton
      Left = 8
      Top = 72
      Width = 185
      Height = 27
      Caption = 'Save global variables'
      TabOrder = 3
      OnClick = btnSaveGlobalVariablesClick
    end
  end
  object rdgGlobalVariables: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 494
    Height = 178
    Align = alClient
    ColCount = 4
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 0
    OnSelectCell = rdgGlobalVariablesSelectCell
    OnSetEditText = rdgGlobalVariablesSetEditText
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnBeforeDrawCell = rdgGlobalVariablesBeforeDrawCell
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Real'
          'Integer'
          'Boolean'
          'Text')
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
        CaseSensitivePicklist = False
        AutoAdjustColWidths = True
      end>
    OnEndUpdate = rdgGlobalVariablesEndUpdate
    ExplicitHeight = 156
    ColWidths = (
      64
      80
      64
      187)
  end
  object dlgOpenGlobVar: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All Files|*.*'
    Left = 192
    Top = 112
  end
  object dlgSaveGlobalVariables: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All Files|*.*'
    Left = 280
    Top = 128
  end
end
