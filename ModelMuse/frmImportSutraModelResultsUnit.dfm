inherited frmImportSutraModelResults: TfrmImportSutraModelResults
  Caption = 'Import SUTRA Model Results'
  ClientHeight = 265
  ExplicitWidth = 442
  ExplicitHeight = 310
  PixelsPerInch = 120
  TextHeight = 18
  object lblTimeStepsToImport: TLabel
    Left = 255
    Top = 16
    Width = 141
    Height = 18
    Caption = 'Time steps to import'
  end
  object lblDataToImport: TLabel
    Left = 16
    Top = 16
    Width = 99
    Height = 18
    Caption = 'Data to import'
  end
  object chklstTimeStepsToImport: TCheckListBox
    Left = 255
    Top = 40
    Width = 161
    Height = 176
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 18
    TabOrder = 1
  end
  object chklstDataToImport: TCheckListBox
    Left = 16
    Top = 40
    Width = 225
    Height = 137
    ItemHeight = 18
    Items.Strings = (
      'Pressure'
      'Concentration or temperature'
      'Saturation'
      'X velocity'
      'Y velocity'
      'Z velocity')
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 224
    Width = 424
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      424
      41)
    object btnHelp: TBitBtn
      Left = 160
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 248
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 336
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object dlgOpenSutraFile: TOpenDialog
    DefaultExt = '.nod'
    Filter = 'SUTRA output files (*.nod; *.ele)|*.nod;*.ele'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 96
    Top = 192
  end
end
