inherited frmGridColor: TfrmGridColor
  Left = 429
  Top = 292
  HelpType = htKeyword
  HelpKeyword = 'Color_Grid_Dialog_Box'
  HorzScrollBar.Range = 557
  VertScrollBar.Range = 225
  Caption = 'Color Grid'
  ExplicitWidth = 320
  ExplicitHeight = 240
  DesignSize = (
    606
    489)
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
  object udTime: TJvUpDown [1]
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
    end
    inherited tabFilters: TTabSheet
      inherited lblLowerLimit: TLabel
        Anchors = [akLeft, akTop, akBottom]
      end
      inherited seNumberOfValuesToIgnore: TJvSpinEdit
        ExplicitTop = 307
      end
    end
    inherited tabLegend: TTabSheet
      inherited imLegend: TImage
        AlignWithMargins = True
        Left = 221
        Top = 3
        Width = 374
        Height = 401
        ExplicitLeft = 221
        ExplicitTop = 3
        ExplicitWidth = 374
        ExplicitHeight = 329
      end
      inherited Panel2: TPanel
        inherited seLegendRows: TJvSpinEdit
          ExplicitTop = 300
        end
      end
    end
  end
  inherited Panel1: TPanel
    inherited btnOK: TBitBtn
      OnClick = btnOKClick
    end
  end
  object comboTime3D: TJvComboBox [4]
    Left = 492
    Top = 24
    Width = 85
    Height = 26
    Anchors = [akTop, akRight]
    ItemHeight = 0
    TabOrder = 0
    Text = '0'
    OnChange = comboTime3DChange
  end
end
