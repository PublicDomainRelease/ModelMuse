inherited frmFootprintProperties: TfrmFootprintProperties
  HelpType = htKeyword
  HelpKeyword = 'Footprint_Properties_Dialog_Bo'
  Caption = 'Footprint Properties'
  ClientHeight = 301
  ClientWidth = 517
  ExplicitWidth = 535
  ExplicitHeight = 346
  PixelsPerInch = 120
  TextHeight = 18
  object pnl1: TPanel
    Left = 0
    Top = 260
    Width = 517
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 216
    DesignSize = (
      517
      41)
    object btnHelp: TBitBtn
      Left = 176
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 290
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 404
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 517
    Height = 260
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 216
    object lblClosureCriterion: TLabel
      Left = 16
      Top = 115
      Width = 114
      Height = 18
      Caption = 'Closure criterion'
    end
    object lblMaxIteration: TLabel
      Left = 16
      Top = 143
      Width = 212
      Height = 18
      Caption = 'Maximum number of  iterations'
    end
    object lblRedistribution: TLabel
      Left = 16
      Top = 219
      Width = 156
      Height = 18
      Caption = 'Redistribution criterion'
    end
    object rdeClosureCriterion: TRbwDataEntry
      Left = 266
      Top = 112
      Width = 145
      Height = 22
      TabOrder = 2
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMax = True
      CheckMin = True
      ChangeDisabledColor = True
    end
    object seMaxIteration: TJvSpinEdit
      Left = 266
      Top = 140
      Width = 145
      Height = 26
      Increment = 1000.000000000000000000
      MaxValue = 2147483647.000000000000000000
      Value = 10000.000000000000000000
      TabOrder = 3
    end
    object cbIntitialDistribution: TCheckBox
      Left = 16
      Top = 183
      Width = 481
      Height = 17
      Caption = 'Initially distribute withdrawals halfway to nearest neighbor'
      TabOrder = 4
    end
    object seRedistribution: TJvSpinEdit
      Left = 266
      Top = 216
      Width = 145
      Height = 26
      MaxValue = 2147483647.000000000000000000
      Value = 10000.000000000000000000
      TabOrder = 5
    end
    object cbSaveBinary: TCheckBox
      Left = 16
      Top = 17
      Width = 473
      Height = 17
      Caption = 'Save results to binary file (Binary_Results_File)'
      TabOrder = 0
    end
    object cbSaveText: TCheckBox
      Left = 16
      Top = 49
      Width = 457
      Height = 17
      Caption = 'Save results to text file (Text_Results_File)'
      TabOrder = 1
    end
    object cbOpenListFile: TCheckBox
      Left = 16
      Top = 80
      Width = 395
      Height = 17
      Caption = 'Open listing file in text editor'
      TabOrder = 6
    end
  end
end
