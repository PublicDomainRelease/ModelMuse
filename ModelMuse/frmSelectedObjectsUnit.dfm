inherited frmSelectedObjects: TfrmSelectedObjects
  Left = 554
  Top = 516
  Width = 220
  HelpType = htKeyword
  HelpKeyword = 'Selected_Objects_Dialog_Box'
  VertScrollBar.Range = 41
  ActiveControl = lbSelected
  Caption = 'Selected Objects'
  Position = poDesigned
  ExplicitWidth = 220
  ExplicitHeight = 274
  PixelsPerInch = 96
  TextHeight = 18
  object lbSelected: TListBox
    Left = 0
    Top = 0
    Width = 212
    Height = 188
    Align = alClient
    ItemHeight = 17
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 188
    Width = 212
    Height = 46
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 194
    DesignSize = (
      212
      46)
    object btnClose: TBitBtn
      Left = 115
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object btnHelp: TBitBtn
      Left = 20
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
end
