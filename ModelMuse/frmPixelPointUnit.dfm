inherited frmPixelPoint: TfrmPixelPoint
  Left = 554
  Top = 516
  Width = 305
  Height = 160
  HelpType = htKeyword
  HelpKeyword = 'Import_Shapefile_Dialog_Box'
  HorzScrollBar.Range = 290
  VertScrollBar.Range = 117
  ActiveControl = rdeX
  Caption = 'Coordinates'
  ExplicitWidth = 305
  ExplicitHeight = 160
  PixelsPerInch = 96
  TextHeight = 17
  object lblX: TLabel
    Left = 8
    Top = 8
    Width = 9
    Height = 17
    Caption = 'X'
  end
  object lblY: TLabel
    Left = 153
    Top = 8
    Width = 9
    Height = 17
    Caption = 'Y'
  end
  object rdeX: TRbwDataEntry
    Left = 8
    Top = 40
    Width = 137
    Height = 28
    Cursor = crIBeam
    ItemHeight = 17
    TabOrder = 0
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeY: TRbwDataEntry
    Left = 153
    Top = 40
    Width = 137
    Height = 28
    Cursor = crIBeam
    ItemHeight = 17
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnOK: TBitBtn
    Left = 104
    Top = 84
    Width = 91
    Height = 33
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      04000000000068010000120B0000120B00001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnCancel: TBitBtn
    Left = 199
    Top = 84
    Width = 91
    Height = 33
    TabOrder = 2
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 8
    Top = 84
    Width = 91
    Height = 33
    TabOrder = 4
    OnClick = btnHelpClick
    Kind = bkHelp
  end
end
