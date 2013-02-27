inherited frameColorGrid: TframeColorGrid
  Width = 608
  ExplicitWidth = 608
  inherited pcChoices: TPageControl
    Width = 608
    ExplicitWidth = 608
    inherited tabSelection: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 27
      ExplicitWidth = 600
      ExplicitHeight = 397
      DesignSize = (
        600
        397)
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
        Width = 29
        Height = 16
        Anchors = [akTop, akRight]
        Caption = 'Time'
      end
      inherited comboColorScheme: TComboBox
        Width = 480
        TabOrder = 7
        ExplicitWidth = 480
      end
      inherited seCycles: TJvSpinEdit
        Left = 494
        TabOrder = 8
        ExplicitLeft = 494
      end
      inherited jsColorExponent: TJvxSlider
        Top = 357
        TabOrder = 9
      end
      inherited seColorExponent: TJvSpinEdit
        TabOrder = 10
      end
      inherited cbLogTransform: TCheckBox
        TabOrder = 11
      end
      inherited udDataSets: TJvUpDown
        Left = 473
        Top = 25
        Width = 20
        Height = 24
        Associate = virttreecomboDataSets
        ExplicitLeft = 473
        ExplicitTop = 25
        ExplicitWidth = 20
        ExplicitHeight = 24
      end
      inherited rgUpdateLimitChoice: TRadioGroup
        TabOrder = 5
      end
      inherited virttreecomboDataSets: TRbwStringTreeCombo
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        Text = '0'
        ExplicitWidth = 485
      end
      inherited reComment: TRichEdit
        Width = 587
        TabOrder = 4
        ExplicitWidth = 587
      end
      inherited btnColorSchemes: TButton
        TabOrder = 6
      end
      object udTime: TJvUpDown
        Left = 579
        Top = 25
        Width = 21
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
        Height = 24
        Anchors = [akTop, akRight]
        TabOrder = 2
        Text = '0'
        OnChange = comboTime3DChange
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 27
      ExplicitWidth = 600
      ExplicitHeight = 397
      DesignSize = (
        600
        397)
    end
    inherited tabLegend: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 27
      ExplicitWidth = 600
      ExplicitHeight = 397
      inherited imLegend: TImage
        Width = 382
        ExplicitWidth = 395
      end
    end
  end
end
