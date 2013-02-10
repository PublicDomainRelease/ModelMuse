inherited framePackageWell: TframePackageWell
  inherited lblComments: TLabel
    Top = 55
    ExplicitTop = 55
  end
  object lblPhiRamp: TLabel [2]
    Left = 104
    Top = 34
    Width = 259
    Height = 13
    Caption = 'Cell adjustment fraction (PHIRAMP - MODFLOW-NWT)'
  end
  inherited memoComments: TMemo
    Top = 80
    Height = 71
    TabOrder = 1
    ExplicitTop = 80
    ExplicitHeight = 71
  end
  object rdePhiRamp: TRbwDataEntry [4]
    Left = 16
    Top = 31
    Width = 73
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 0
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
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
        Control = rdePhiRamp
      end>
  end
end
