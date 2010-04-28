inherited frmGridColor: TfrmGridColor
  Left = 429
  Top = 292
  HelpType = htKeyword
  HelpKeyword = 'Color_Grid_Dialog_Box'
  HorzScrollBar.Range = 557
  VertScrollBar.Range = 225
  Caption = 'Color Grid'
  DesignSize = (
    606
    417)
  PixelsPerInch = 96
  TextHeight = 18
  object lblTime: TLabel [0]
    Left = 487
    Top = 4
    Width = 35
    Height = 18
    Anchors = [akTop, akRight]
    Caption = 'Time'
    ExplicitLeft = 492
  end
  object comboTime3D: TJvComboBox [1]
    Left = 492
    Top = 24
    Width = 85
    Height = 26
    Anchors = [akTop, akRight]
    ItemHeight = 18
    TabOrder = 0
    Text = '0'
    OnChange = comboTime3DChange
  end
  object udTime: TJvUpDown [2]
    Left = 576
    Top = 24
    Width = 17
    Height = 21
    Anchors = [akTop, akRight]
    Associate = btnCancel
    Max = 0
    TabOrder = 3
    OnChangingEx = udTimeChangingEx
  end
  inherited pcChoices: TPageControl
    TabOrder = 2
    inherited tabSelection: TTabSheet
      inherited virttreecomboDataSets: TTntExDropDownVirtualStringTree
        Anchors = [akLeft, akTop, akRight]
        Tree.OnGetNodeDataSize = virttreecomboDataSetsDropDownTreeGetNodeDataSize
      end
      inherited udDataSets: TJvUpDown
        Anchors = [akTop, akRight]
      end
    end
    inherited tabFilters: TTabSheet
      inherited lblLowerLimit: TLabel
        Anchors = [akLeft, akTop, akBottom]
      end
    end
  end
  inherited Panel1: TPanel
    inherited btnOK: TBitBtn
      OnClick = btnOKClick
    end
  end
end
