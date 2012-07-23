inherited frmShowHideBitmaps: TfrmShowHideBitmaps
  Left = 546
  Top = 310
  Width = 482
  HelpType = htKeyword
  HelpKeyword = 'Show_or_Hide_Bitmaps_Dialog_Box'
  VertScrollBar.Range = 45
  ActiveControl = clbBitmaps
  Caption = 'Show or Hide Images'
  ExplicitWidth = 482
  ExplicitHeight = 271
  PixelsPerInch = 120
  TextHeight = 18
  object clbBitmaps: TCheckListBox
    Left = 0
    Top = 0
    Width = 464
    Height = 181
    OnClickCheck = clbBitmapsClickCheck
    Align = alClient
    ItemHeight = 18
    TabOrder = 0
    ExplicitWidth = 437
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 181
    Width = 464
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitWidth = 437
    DesignSize = (
      464
      45)
    object btnClose: TBitBtn
      Left = 366
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 4
    end
    object btnHelp: TBitBtn
      Left = 277
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object btnShowAll: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 33
      Caption = 'Show all'
      TabOrder = 0
      OnClick = btnShowClick
    end
    object btnShowNone: TButton
      Left = 88
      Top = 6
      Width = 97
      Height = 33
      Caption = 'Show none'
      TabOrder = 1
      OnClick = btnShowClick
    end
    object btnToggle: TButton
      Left = 192
      Top = 6
      Width = 75
      Height = 33
      Caption = 'Toggle'
      TabOrder = 2
      OnClick = btnToggleClick
    end
  end
end
