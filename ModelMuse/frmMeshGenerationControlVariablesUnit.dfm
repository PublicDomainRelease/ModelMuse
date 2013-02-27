inherited frmMeshGenerationControlVariables: TfrmMeshGenerationControlVariables
  HelpType = htKeyword
  HelpKeyword = 'Mesh_Generation_Control_Variab'
  Caption = 'Mesh Generation Control Variables'
  ClientHeight = 384
  ClientWidth = 415
  ExplicitWidth = 431
  ExplicitHeight = 422
  PixelsPerInch = 96
  TextHeight = 18
  object rdgControlVariables: TRbwDataGrid4
    Left = 0
    Top = 121
    Width = 415
    Height = 215
    Align = alClient
    ColCount = 2
    FixedCols = 1
    RowCount = 8
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
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
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 336
    Width = 415
    Height = 48
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      Left = 127
      Top = 6
      Width = 89
      Height = 33
      HelpType = htKeyword
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 222
      Top = 6
      Width = 89
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 318
      Top = 6
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
    end
    object btnResetDefaults: TButton
      Left = 6
      Top = 6
      Width = 115
      Height = 33
      Caption = 'Reset Defaults'
      TabOrder = 0
      OnClick = btnResetDefaultsClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 415
    Height = 121
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
    object rgRenumberingMethod: TRadioGroup
      Left = 0
      Top = 57
      Width = 415
      Height = 64
      Align = alClient
      Caption = 'Renumbering method'
      ItemIndex = 0
      Items.Strings = (
        'Cuthill and McKee (1969)'
        'Sloan and Randolph (1983)')
      TabOrder = 1
    end
  end
end
