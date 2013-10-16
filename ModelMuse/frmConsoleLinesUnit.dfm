inherited frmConsoleLines: TfrmConsoleLines
  Caption = 'Console Lines'
  ClientHeight = 363
  ClientWidth = 588
  ExplicitWidth = 606
  ExplicitHeight = 408
  PixelsPerInch = 120
  TextHeight = 18
  object memoConsoleLines: TMemo
    Left = 0
    Top = 57
    Width = 588
    Height = 265
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitLeft = 104
    ExplicitTop = 48
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 322
    Width = 588
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitLeft = 240
    ExplicitTop = 168
    ExplicitWidth = 185
    object btnClose: TBitBtn
      Left = 509
      Top = 6
      Width = 75
      Height = 25
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 57
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 424
    object lblMessage: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 80
      Height = 18
      Align = alClient
      Caption = 'lblMessage'
      WordWrap = True
    end
  end
end
