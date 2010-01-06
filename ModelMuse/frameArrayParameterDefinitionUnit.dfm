inherited frameArrayParameterDefinition: TframeArrayParameterDefinition
  inherited dgParameters: TRbwDataGrid4
    Top = 57
    Height = 135
    ColCount = 4
    OnMouseDown = dgParametersMouseDown
    OnMouseUp = dgParametersMouseUp
    OnColSize = dgParametersColSize
    OnHorizontalScroll = dgParametersHorizontalScroll
    Columns = <
      item
        AutoAdjustRowHeights = False
        ButtonCaption = 'F()'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 35
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4String
        LimitToList = False
        MaxLength = 10
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = 'F()'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 35
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
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = 'F()'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 35
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4Boolean
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        ButtonCaption = 'F()'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 35
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4Boolean
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
      end>
    ExplicitTop = 57
    ExplicitHeight = 135
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lblParamValue: TLabel
      Left = 24
      Top = 8
      Width = 66
      Height = 13
      Caption = 'lblParamValue'
      Enabled = False
    end
    object rdeParamValue: TRbwDataEntry
      Left = 25
      Top = 31
      Width = 33
      Height = 22
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      Text = '0'
      OnChange = rdeParamValueChange
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object cbUseZone: TCheckBox
      Left = 64
      Top = 35
      Width = 97
      Height = 17
      Caption = 'cbUseZone'
      Enabled = False
      TabOrder = 1
      OnClick = cbUseZoneClick
    end
    object cbUseMultiplier: TCheckBox
      Left = 167
      Top = 35
      Width = 97
      Height = 17
      Caption = 'cbUseMultiplier'
      Enabled = False
      TabOrder = 2
      OnClick = cbUseMultiplierClick
    end
  end
end
