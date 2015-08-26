inherited frmFilesToArchive: TfrmFilesToArchive
  HelpType = htKeyword
  HelpKeyword = 'Files_to_Archive_Dialog_Box'
  Caption = ' Files To Archive'
  ClientWidth = 512
  ExplicitWidth = 530
  ExplicitHeight = 271
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 153
    Width = 512
    Height = 73
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      512
      73)
    object JvLinkLabel1: TJvLinkLabel
      Left = 8
      Top = 6
      Width = 297
      Height = 18
      Caption = '<link>USGS model archiving policy<\link>'
      Text.Strings = (
        '<link>USGS model archiving policy<\link>')
      OnLinkClick = JvLinkLabel1LinkClick
    end
    object btnCancel: TBitBtn
      Left = 414
      Top = 29
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 4
    end
    object btnOK: TBitBtn
      Left = 325
      Top = 29
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 238
      Top = 29
      Width = 81
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object btnArchive: TButton
      Left = 8
      Top = 29
      Width = 121
      Height = 33
      Caption = 'Create archive'
      TabOrder = 0
      OnClick = btnArchiveClick
    end
    object btnAddFiles: TButton
      Left = 135
      Top = 29
      Width = 75
      Height = 33
      Caption = 'Add files'
      TabOrder = 1
      OnClick = btnAddFilesClick
    end
  end
  object reFilesToSave: TJvRichEdit
    Left = 0
    Top = 0
    Width = 512
    Height = 153
    Align = alClient
    TabOrder = 0
    WordWrap = False
    OnChange = reFilesToSaveChange
  end
  object sdArchive: TSaveDialog
    DefaultExt = '.zip'
    Filter = 'Archives (*.zip)|*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 24
    Top = 16
  end
  object odAddFiles: TOpenDialog
    Filter = 'Any File (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 88
    Top = 72
  end
end
