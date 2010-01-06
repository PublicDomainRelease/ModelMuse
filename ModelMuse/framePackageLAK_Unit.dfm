inherited framePackageLAK: TframePackageLAK
  Height = 301
  ExplicitHeight = 301
  DesignSize = (
    304
    301)
  object lblTheta: TLabel [1]
    Left = 120
    Top = 170
    Width = 28
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Theta'
    Enabled = False
  end
  object lblIterations: TLabel [2]
    Left = 120
    Top = 198
    Width = 197
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Maximumn number of iterations (NSSITR)'
    Enabled = False
  end
  object lblConvergenceCriterion: TLabel [3]
    Left = 120
    Top = 226
    Width = 157
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Convergence criterion (SSCNCR)'
    Enabled = False
  end
  object lblSurfDepth: TLabel [5]
    Left = 120
    Top = 251
    Width = 230
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Height of lake bottom undulations (SURFDEPTH)'
    Enabled = False
  end
  inherited memoComments: TMemo
    Height = 99
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitHeight = 99
  end
  object rdeTheta: TRbwDataEntry [7]
    Left = 16
    Top = 167
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 1
    Text = '0.5'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeIterations: TRbwDataEntry [8]
    Left = 16
    Top = 195
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 2
    Text = '1'
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeConvergenceCriterion: TRbwDataEntry [9]
    Left = 16
    Top = 223
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbPrintLake: TCheckBox [10]
    Left = 16
    Top = 276
    Width = 217
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Print lake output (LWRT)'
    Enabled = False
    TabOrder = 4
  end
  object rdeSurfDepth: TRbwDataEntry [11]
    Left = 16
    Top = 248
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    TabOrder = 5
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
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
        Control = cbPrintLake
      end
      item
        Control = rdeIterations
      end
      item
        Control = rdeConvergenceCriterion
      end
      item
        Control = rdeTheta
      end
      item
        Control = lblConvergenceCriterion
      end
      item
        Control = lblIterations
      end
      item
        Control = lblTheta
      end
      item
        Control = rdeSurfDepth
      end
      item
        Control = lblSurfDepth
      end>
  end
end
