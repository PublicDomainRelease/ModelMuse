inherited frameColorGrid: TframeColorGrid
  Width = 608
  ExplicitWidth = 608
  inherited pcChoices: TPageControl
    Width = 608
    ExplicitWidth = 621
    inherited tabSelection: TTabSheet
      ExplicitWidth = 613
      DesignSize = (
        600
        400)
      inherited lblCycles: TLabel
        Left = 494
        ExplicitLeft = 507
      end
      inherited pbColorScheme: TPaintBox
        Width = 435
        ExplicitWidth = 448
      end
      object lblTime: TLabel [6]
        Left = 494
        Top = 4
        Width = 22
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Time'
      end
      inherited reComment: TJvRichEdit
        Width = 587
        ExplicitWidth = 587
      end
      inherited comboColorScheme: TComboBox
        Width = 480
        ExplicitWidth = 493
      end
      inherited seCycles: TJvSpinEdit
        Left = 494
        ExplicitLeft = 507
      end
      inherited cbLogTransform: TCheckBox
        TabOrder = 9
      end
      inherited udDataSets: TJvUpDown
        Left = 473
        Top = 25
        Width = 16
        Height = 21
        Associate = virttreecomboDataSets
        ExplicitLeft = 473
        ExplicitTop = 25
        ExplicitWidth = 16
        ExplicitHeight = 21
      end
      inherited virttreecomboDataSets: TRbwStringTreeCombo
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        Text = '0'
        ExplicitWidth = 481
      end
      object udTime: TJvUpDown
        Left = 579
        Top = 25
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = comboTime3D
        Max = 0
        TabOrder = 10
        OnChangingEx = udTimeChangingEx
        ExplicitLeft = 592
      end
      object comboTime3D: TJvComboBox
        Left = 494
        Top = 25
        Width = 85
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 8
        Text = '0'
        OnChange = comboTime3DChange
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitWidth = 613
      DesignSize = (
        600
        400)
    end
    inherited tabLegend: TTabSheet
      ExplicitWidth = 613
      inherited imLegend: TImage
        Width = 382
        ExplicitWidth = 395
      end
    end
  end
end
