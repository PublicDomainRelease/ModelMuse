inherited frmDeleteImage: TfrmDeleteImage
  HelpType = htKeyword
  HelpKeyword = 'Delete_Image_Dialog_Box'
  Caption = 'Delete Image'
  PixelsPerInch = 96
  TextHeight = 18
  object clbBitmaps: TCheckListBox
    Left = 0
    Top = 0
    Width = 434
    Height = 195
    Align = alClient
    ItemHeight = 18
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 195
    Width = 434
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitLeft = -13
    ExplicitWidth = 447
    DesignSize = (
      434
      45)
    object btnOK: TBitBtn
      Left = 254
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnOKClick
      Kind = bkOK
    end
    object BitBtn1: TBitBtn
      Left = 343
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkCancel
    end
    object btnHelp: TBitBtn
      Left = 165
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnClick = btnHelpClick
      Kind = bkHelp
    end
  end
end
