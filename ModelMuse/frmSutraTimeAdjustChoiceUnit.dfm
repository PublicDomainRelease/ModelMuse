inherited frmSutraTimeAdjustChoice: TfrmSutraTimeAdjustChoice
  Caption = 'Adjust Times'
  ClientHeight = 189
  ExplicitWidth = 442
  ExplicitHeight = 234
  PixelsPerInch = 96
  TextHeight = 18
  object lblMessage: TLabel
    Left = 8
    Top = 8
    Width = 407
    Height = 54
    Caption = 
      'The times for the schedule do not match the times for this obser' +
      'vation when it was originally created. How should this be treate' +
      'd?'
    WordWrap = True
  end
  object rgTimeTreatment: TRadioGroup
    Left = 8
    Top = 76
    Width = 408
    Height = 77
    Caption = 'Treatment'
    ItemIndex = 1
    Items.Strings = (
      'Use the times from the time schedule'
      'Convert to a custom time schedule')
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 341
    Top = 159
    Width = 75
    Height = 25
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
  end
end
