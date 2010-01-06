inherited frmTimeStepLengthCalculator: TfrmTimeStepLengthCalculator
  HelpType = htKeyword
  HelpKeyword = 'Time_Step_Length_Calculator'
  Caption = 'Time Step Length Calculator'
  ClientHeight = 163
  ClientWidth = 471
  ExplicitWidth = 479
  ExplicitHeight = 197
  PixelsPerInch = 96
  TextHeight = 18
  object lblNumSteps: TLabel
    Left = 8
    Top = 8
    Width = 148
    Height = 18
    Caption = 'Number of time steps'
  end
  object lblMultiplier: TLabel
    Left = 8
    Top = 107
    Width = 62
    Height = 18
    Caption = 'Multiplier'
  end
  object lblPeriodLength: TLabel
    Left = 8
    Top = 59
    Width = 92
    Height = 18
    Caption = 'Period length'
  end
  object pbArrow: TPaintBox
    Left = 168
    Top = 79
    Width = 98
    Height = 22
    OnPaint = pbArrowPaint
  end
  object lblMaxLengthFirstTimeStep: TLabel
    Left = 272
    Top = 36
    Width = 133
    Height = 36
    Caption = 'Maximum length of the first time step'
    WordWrap = True
  end
  object seNumSteps: TJvSpinEdit
    Left = 8
    Top = 28
    Width = 145
    Height = 26
    ButtonKind = bkClassic
    MaxValue = 10000000.000000000000000000
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    TabOrder = 0
    OnChange = seNumStepsChange
  end
  object rdeMultiplier: TRbwDataEntry
    Left = 8
    Top = 127
    Width = 145
    Height = 22
    ItemHeight = 18
    TabOrder = 1
    Text = '1'
    OnChange = seNumStepsChange
    DataType = dtReal
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdePeriodLength: TRbwDataEntry
    Left = 8
    Top = 79
    Width = 145
    Height = 22
    ItemHeight = 18
    TabOrder = 2
    Text = '0'
    OnChange = seNumStepsChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeMaxLengthFirstTimeStep: TRbwDataEntry
    Left = 271
    Top = 79
    Width = 194
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 18
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnCancel: TBitBtn
    Left = 386
    Top = 121
    Width = 79
    Height = 34
    Anchors = [akTop, akRight]
    TabOrder = 4
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 297
    Top = 121
    Width = 83
    Height = 34
    Anchors = [akTop, akRight]
    TabOrder = 5
    Kind = bkOK
  end
  object btnHelp: TBitBtn
    Left = 207
    Top = 121
    Width = 84
    Height = 34
    Anchors = [akTop, akRight]
    TabOrder = 6
    OnClick = btnHelpClick
    Kind = bkHelp
  end
end
