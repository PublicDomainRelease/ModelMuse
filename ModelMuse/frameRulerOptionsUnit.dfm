object frameRulerOptions: TframeRulerOptions
  Left = 0
  Top = 0
  Width = 356
  Height = 131
  TabOrder = 0
  TabStop = True
  object lblPreview: TLabel
    Left = 128
    Top = 94
    Width = 6
    Height = 13
    Caption = '0'
  end
  object lblPreviewLabel: TLabel
    Left = 3
    Top = 94
    Width = 42
    Height = 13
    Caption = 'Preview:'
  end
  object lblSampleNumber: TLabel
    Left = 3
    Top = 65
    Width = 73
    Height = 13
    Caption = 'Sample number'
  end
  object lblPrecision: TLabel
    Left = 3
    Top = 10
    Width = 42
    Height = 13
    Caption = 'Precision'
  end
  object Label1: TLabel
    Left = 3
    Top = 37
    Width = 41
    Height = 13
    Caption = 'Decimals'
  end
  object rdePreviewNumber: TRbwDataEntry
    Left = 128
    Top = 58
    Width = 101
    Height = 21
    Cursor = crIBeam
    ItemHeight = 13
    TabOrder = 0
    Text = '123456789'
    OnChange = rdePreviewNumberChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object sePrecision: TJvSpinEdit
    Left = 128
    Top = 4
    Width = 101
    Height = 21
    ButtonKind = bkClassic
    MaxValue = 15.000000000000000000
    MinValue = 1.000000000000000000
    Value = 5.000000000000000000
    TabOrder = 1
    OnChange = sePrecisionChange
  end
  object seDigits: TJvSpinEdit
    Left = 128
    Top = 31
    Width = 101
    Height = 21
    ButtonKind = bkClassic
    MaxValue = 4.000000000000000000
    TabOrder = 2
    OnChange = sePrecisionChange
  end
end
