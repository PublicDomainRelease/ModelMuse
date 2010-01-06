inherited frmShowHideBitmaps: TfrmShowHideBitmaps
  Left = 546
  Top = 310
  Width = 455
  HelpType = htKeyword
  HelpKeyword = 'Show_or_Hide_Bitmaps_Dialog_Box'
  VertScrollBar.Range = 45
  ActiveControl = clbBitmaps
  Caption = 'Show or Hide Images'
  ExplicitWidth = 455
  ExplicitHeight = 274
  PixelsPerInch = 96
  TextHeight = 17
  object clbBitmaps: TCheckListBox
    Left = 0
    Top = 0
    Width = 447
    Height = 195
    OnClickCheck = clbBitmapsClickCheck
    Align = alClient
    ItemHeight = 17
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 195
    Width = 447
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      447
      45)
    object btnClose: TBitBtn
      Left = 363
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkClose
    end
    object btnHelp: TBitBtn
      Left = 274
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnShowAll: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 33
      Caption = 'Show all'
      TabOrder = 2
      OnClick = btnShowClick
    end
    object btnShowNone: TButton
      Left = 88
      Top = 6
      Width = 97
      Height = 33
      Caption = 'Show none'
      TabOrder = 3
      OnClick = btnShowClick
    end
    object btnToggle: TButton
      Left = 192
      Top = 6
      Width = 75
      Height = 33
      Caption = 'Toggle'
      TabOrder = 4
      OnClick = btnToggleClick
    end
  end
end
