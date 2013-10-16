inherited frmImportMultipleGriddedDataFiles: TfrmImportMultipleGriddedDataFiles
  HelpType = htKeyword
  HelpKeyword = 'Import_Gridded_Data_Files_Dial'
  Caption = 'Import Gridded Data Files'
  ExplicitWidth = 440
  ExplicitHeight = 264
  PixelsPerInch = 120
  TextHeight = 18
  inline frameGridFiles: TframeGrid
    Left = 0
    Top = 0
    Width = 424
    Height = 184
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 424
    ExplicitHeight = 184
    inherited Panel: TPanel
      Top = 143
      Width = 424
      ExplicitTop = 143
      ExplicitWidth = 424
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 219
        ExplicitLeft = 219
      end
      inherited sbInsert: TSpeedButton
        Left = 259
        ExplicitLeft = 259
      end
      inherited sbDelete: TSpeedButton
        Left = 300
        ExplicitLeft = 300
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 424
      Height = 143
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goAlwaysShowEditor]
      OnButtonClick = frameGridFilesGridButtonClick
      Columns = <
        item
          AutoAdjustRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = True
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitWidth = 424
      ExplicitHeight = 143
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 184
    Width = 424
    Height = 42
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      424
      42)
    object btnHelp: TBitBtn
      Left = 153
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 242
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 331
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 3
    end
    object btnOpenFiles: TButton
      Left = 8
      Top = 6
      Width = 97
      Height = 33
      Caption = 'Open files'
      TabOrder = 0
      OnClick = btnOpenFilesClick
    end
  end
  object dlgOpenFiles: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 168
    Top = 16
  end
end
