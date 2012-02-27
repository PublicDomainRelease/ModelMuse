inherited frameMt3dBasicPkg: TframeMt3dBasicPkg
  Width = 528
  Height = 347
  ExplicitWidth = 528
  ExplicitHeight = 347
  DesignSize = (
    528
    347)
  object lblInactiveConcentration: TLabel [2]
    Left = 87
    Top = 149
    Width = 193
    Height = 13
    Caption = 'Concentration at inactive cells (CINACT)'
  end
  object lblMinimumSaturatedFraction: TLabel [3]
    Left = 87
    Top = 177
    Width = 179
    Height = 13
    Caption = 'Minimum saturated fraction (THKMIN)'
  end
  object edMassUnit: TLabeledEdit [4]
    Left = 16
    Top = 119
    Width = 50
    Height = 21
    EditLabel.Width = 88
    EditLabel.Height = 13
    EditLabel.Caption = 'Mass unit (MUNIT)'
    Enabled = False
    LabelPosition = lpRight
    MaxLength = 4
    TabOrder = 1
  end
  object pnlSpecies: TPanel [5]
    Left = 0
    Top = 224
    Width = 528
    Height = 123
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    object Splitter1: TSplitter
      Left = 249
      Top = 1
      Width = 5
      Height = 121
      ExplicitLeft = 145
      ExplicitHeight = 115
    end
    inline frameGridImmobile: TframeGrid
      Left = 254
      Top = 1
      Width = 273
      Height = 121
      Align = alClient
      Enabled = False
      TabOrder = 0
      ExplicitLeft = 254
      ExplicitTop = 1
      ExplicitWidth = 273
      ExplicitHeight = 121
      inherited Panel: TPanel
        Top = 80
        Width = 273
        ExplicitTop = 80
        ExplicitWidth = 273
      end
      inherited Grid: TRbwDataGrid4
        Width = 273
        Height = 80
        ExplicitWidth = 273
        ExplicitHeight = 80
      end
    end
    inline frameGridMobile: TframeGrid
      Left = 1
      Top = 1
      Width = 248
      Height = 121
      Align = alLeft
      Enabled = False
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 248
      ExplicitHeight = 121
      inherited Panel: TPanel
        Top = 80
        Width = 248
        ExplicitTop = 80
        ExplicitWidth = 248
      end
      inherited Grid: TRbwDataGrid4
        Width = 248
        Height = 80
        ExplicitWidth = 248
        ExplicitHeight = 80
      end
    end
  end
  object rdeMinimumSaturatedFraction: TRbwDataEntry [6]
    Left = 16
    Top = 174
    Width = 65
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeInactiveConcentration: TRbwDataEntry [7]
    Left = 16
    Top = 146
    Width = 65
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  inherited memoComments: TMemo
    Width = 497
    Height = 51
    ExplicitWidth = 497
    ExplicitHeight = 51
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = edMassUnit
      end
      item
        Control = rdeInactiveConcentration
      end
      item
        Control = rdeMinimumSaturatedFraction
      end
      item
        Control = frameGridImmobile
      end
      item
        Control = frameGridMobile
      end>
  end
end
