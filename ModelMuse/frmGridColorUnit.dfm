inherited frmGridColor: TfrmGridColor
  Left = 429
  Top = 292
  HelpType = htKeyword
  HelpKeyword = 'Color_Grid_Dialog_Box'
  HorzScrollBar.Range = 557
  VertScrollBar.Range = 225
  Caption = 'Color Grid'
  ExplicitTop = 8
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
    Max = 0
    TabOrder = 3
    OnChangingEx = udTimeChangingEx
  end
  inherited pcChoices: TPageControl
    TabOrder = 2
    inherited tabSelection: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 29
      ExplicitWidth = 598
      ExplicitHeight = 335
      inherited virttreecomboDataSets: TTntExDropDownVirtualStringTree
        Tree.OnGetNodeDataSize = virttreecomboDataSetsDropDownTreeGetNodeDataSize
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 29
      ExplicitWidth = 598
      ExplicitHeight = 335
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
