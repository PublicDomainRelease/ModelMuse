inherited frmGenerateGrid: TfrmGenerateGrid
  Left = 551
  Top = 437
  Width = 344
  Height = 280
  HelpType = htKeyword
  HelpKeyword = 'Generate_Grid_Dialog_Box'
  HorzScrollBar.Range = 329
  VertScrollBar.Range = 241
  ActiveControl = cbSpecifyGridAngle
  Caption = 'Generate Grid'
  ExplicitWidth = 344
  ExplicitHeight = 280
  PixelsPerInch = 96
  TextHeight = 18
  object lblGridAngle: TLabel
    Left = 128
    Top = 44
    Width = 144
    Height = 18
    Caption = 'Grid angle (degrees)'
  end
  object cbSpecifyGridAngle: TCheckBox
    Left = 16
    Top = 8
    Width = 305
    Height = 31
    Caption = 'Calculate grid angle automatically'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = cbSpecifyGridAngleClick
  end
  object rdeGridAngle: TRbwDataEntry
    Left = 16
    Top = 40
    Width = 101
    Height = 28
    Cursor = crIBeam
    Color = clBtnFace
    Enabled = False
    TabOrder = 1
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnCancel: TBitBtn
    Left = 238
    Top = 208
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object btnOK: TBitBtn
    Left = 142
    Top = 208
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnHelp: TBitBtn
    Left = 46
    Top = 208
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object gbGridSmoothing: TGroupBox
    Left = 8
    Top = 72
    Width = 321
    Height = 129
    Caption = 'Grid Smoothing'
    TabOrder = 5
    object lblCriterion: TLabel
      Left = 8
      Top = 92
      Width = 166
      Height = 18
      Caption = 'Grid smoothing criterion'
      Enabled = False
    end
    object cbColumns: TCheckBox
      Left = 8
      Top = 56
      Width = 100
      Height = 30
      Caption = 'Columns'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 0
    end
    object cbSmoothGrid: TCheckBox
      Left = 8
      Top = 19
      Width = 153
      Height = 31
      Caption = 'Smooth grid '
      TabOrder = 1
      OnClick = cbSmoothGridClick
    end
    object cbRows: TCheckBox
      Left = 120
      Top = 56
      Width = 81
      Height = 30
      Caption = 'Rows'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 2
    end
    object cbLayers: TCheckBox
      Left = 208
      Top = 56
      Width = 100
      Height = 30
      Caption = 'Layers'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 3
    end
    object rdeCriterion: TRbwDataEntry
      Left = 208
      Top = 88
      Width = 100
      Height = 28
      Cursor = crIBeam
      Color = clBtnFace
      Enabled = False
      TabOrder = 4
      Text = '1.2'
      DataType = dtReal
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
end
