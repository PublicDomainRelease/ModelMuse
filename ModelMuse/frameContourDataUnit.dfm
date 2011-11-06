inherited frameContourData: TframeContourData
  inherited pcChoices: TPageControl
    inherited tabSelection: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 562
      ExplicitHeight = 400
      inherited cbLogTransform: TCheckBox
        OnClick = cbLogTransformClick
      end
      inherited udDataSets: TJvUpDown
        Left = 319
        Top = 23
        ExplicitLeft = 319
        ExplicitTop = 23
      end
      inherited virttreecomboDataSets: TRbwStringTreeCombo
        Top = 23
        Width = 305
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        ExplicitTop = 23
        ExplicitWidth = 305
      end
      object btnEditContours: TButton
        Left = 443
        Top = 23
        Width = 119
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Edit contours...'
        Enabled = False
        TabOrder = 9
        OnClick = btnEditContoursClick
      end
      object cbSpecifyContours: TJvCheckBox
        Left = 342
        Top = 13
        Width = 96
        Height = 41
        Anchors = [akTop, akRight]
        Caption = 'Specify contours'
        TabOrder = 10
        WordWrap = True
        OnClick = cbSpecifyContoursClick
        LinkedControls = <>
        AutoSize = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = 17
        HotTrackFont.Name = 'Microsoft Sans Serif'
        HotTrackFont.Pitch = fpVariable
        HotTrackFont.Style = []
      end
      object cbLabelContours: TCheckBox
        Left = 328
        Top = 160
        Width = 177
        Height = 17
        Caption = 'Label contours'
        TabOrder = 11
        OnClick = cbLabelContoursClick
      end
      object btnContourFont: TButton
        Left = 328
        Top = 204
        Width = 145
        Height = 25
        Caption = 'Contour label font'
        Enabled = False
        TabOrder = 12
        OnClick = btnContourFontClick
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 562
      ExplicitHeight = 400
    end
    inherited tabLegend: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 562
      ExplicitHeight = 400
    end
  end
  object fdContourFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 480
    Top = 232
  end
end
