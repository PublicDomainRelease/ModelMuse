inherited frmSelectImage: TfrmSelectImage
  Left = 558
  Top = 536
  Width = 428
  Height = 164
  HelpType = htKeyword
  HelpKeyword = 'ImportEdit_Bitmap_Dialog_Box'
  HorzScrollBar.Range = 412
  VertScrollBar.Range = 121
  ActiveControl = comboBitmaps
  Caption = 'Select image'
  OldCreateOrder = True
  ExplicitWidth = 428
  ExplicitHeight = 164
  PixelsPerInch = 96
  TextHeight = 17
  object lblSelect: TLabel
    Left = 16
    Top = 8
    Width = 124
    Height = 17
    Caption = 'Select image to edit'
  end
  object comboBitmaps: TComboBox
    Left = 16
    Top = 40
    Width = 393
    Height = 25
    Style = csDropDownList
    ItemHeight = 17
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 225
    Top = 88
    Width = 91
    Height = 33
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
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
    Left = 321
    Top = 88
    Width = 91
    Height = 33
    TabOrder = 2
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 129
    Top = 88
    Width = 91
    Height = 33
    TabOrder = 1
    OnClick = btnHelpClick
    Kind = bkHelp
  end
end
