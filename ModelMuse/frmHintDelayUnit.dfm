inherited frmHintDelay: TfrmHintDelay
  Left = 554
  Top = 516
  Width = 327
  Height = 123
  HelpType = htKeyword
  HelpKeyword = 'Hint_Display_Time_Dialog_Box'
  HorzScrollBar.Range = 313
  VertScrollBar.Range = 81
  ActiveControl = rdeHintDelay
  Caption = 'Hint Display Time'
  ExplicitWidth = 327
  ExplicitHeight = 123
  PixelsPerInch = 96
  TextHeight = 17
  object lblHintDisplayTime: TLabel
    Left = 8
    Top = 10
    Width = 124
    Height = 17
    Caption = 'Hint display time (s)'
  end
  object rdeHintDelay: TRbwDataEntry
    Left = 212
    Top = 8
    Width = 101
    Height = 28
    Cursor = crIBeam
    ItemHeight = 17
    TabOrder = 1
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnCancel: TBitBtn
    Left = 224
    Top = 48
    Width = 89
    Height = 33
    TabOrder = 0
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 128
    Top = 48
    Width = 89
    Height = 33
    TabOrder = 3
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnHelp: TBitBtn
    Left = 32
    Top = 48
    Width = 89
    Height = 33
    TabOrder = 2
    OnClick = btnHelpClick
    Kind = bkHelp
  end
end
