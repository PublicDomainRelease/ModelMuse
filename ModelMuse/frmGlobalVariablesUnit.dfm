inherited frmGlobalVariables: TfrmGlobalVariables
  HelpType = htKeyword
  HelpKeyword = 'Global_Variables_Dialog_Box'
  Caption = 'Global Variables'
  ClientWidth = 494
  OnDestroy = FormDestroy
  ExplicitWidth = 502
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 164
    Width = 494
    Height = 70
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    object Label1: TLabel
      Left = 71
      Top = 11
      Width = 185
      Height = 18
      Caption = 'Number of global variables'
    end
    object btnHelp: TBitBtn
      Left = 224
      Top = 40
      Width = 82
      Height = 27
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 312
      Top = 40
      Width = 82
      Height = 27
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 400
      Top = 40
      Width = 83
      Height = 27
      TabOrder = 2
      Kind = bkCancel
    end
    object seGlobalVariableCount: TJvSpinEdit
      Left = 8
      Top = 6
      Width = 57
      Height = 26
      ButtonKind = bkClassic
      TabOrder = 3
      OnChange = seGlobalVariableCountChange
    end
    object btnDelete: TButton
      Left = 400
      Top = 7
      Width = 83
      Height = 27
      Caption = 'Delete'
      Enabled = False
      TabOrder = 4
      OnClick = btnDeleteClick
    end
  end
  object rdgGlobalVariables: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 494
    Height = 164
    Align = alClient
    ColCount = 4
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 1
    OnMouseDown = rdgGlobalVariablesMouseDown
    OnSelectCell = rdgGlobalVariablesSelectCell
    OnSetEditText = rdgGlobalVariablesSetEditText
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
    OnEndUpdate = rdgGlobalVariablesEndUpdate
    ColWidths = (
      64
      80
      64
      187)
  end
end
