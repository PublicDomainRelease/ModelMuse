inherited frameColorGrid: TframeColorGrid
  Width = 608
  ExplicitWidth = 608
  inherited pcChoices: TPageControl
    Width = 608
    ActivePage = tabSelection
    ExplicitWidth = 608
    inherited tabSelection: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 600
      ExplicitHeight = 400
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
        TabOrder = 4
        ExplicitWidth = 587
      end
      inherited comboColorScheme: TComboBox
        Width = 480
        TabOrder = 6
        ExplicitWidth = 480
      end
      inherited seCycles: TJvSpinEdit
        Left = 494
        TabOrder = 7
        ExplicitLeft = 494
      end
      inherited jsColorExponent: TJvxSlider
        Top = 357
        TabOrder = 8
      end
      inherited seColorExponent: TJvSpinEdit
        TabOrder = 9
      end
      inherited cbLogTransform: TCheckBox
        TabOrder = 10
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
      inherited rgUpdateLimitChoice: TRadioGroup
        TabOrder = 5
      end
      inherited virttreecomboDataSets: TRbwStringTreeCombo
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        Text = '0'
        ExplicitWidth = 481
      end
      object udTime: TJvUpDown
        Left = 579
        Top = 25
        Width = 17
        Height = 21
        Anchors = [akTop, akRight]
        Max = 0
        TabOrder = 3
        OnChangingEx = udTimeChangingEx
      end
      object comboTime3D: TJvComboBox
        Left = 494
        Top = 25
        Width = 85
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 2
        Text = '0'
        OnChange = comboTime3DChange
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 600
      ExplicitHeight = 400
      DesignSize = (
        600
        400)
    end
    inherited tabLegend: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 600
      ExplicitHeight = 400
      inherited imLegend: TImage
        Width = 382
        ExplicitWidth = 395
      end
    end
  end
end
