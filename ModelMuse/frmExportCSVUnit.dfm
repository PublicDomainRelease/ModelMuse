inherited frmExportCSV: TfrmExportCSV
  HelpType = htKeyword
  HelpKeyword = 'Export_Data_as_CSV_Dialog_Box'
  Caption = 'Export Data as CSV'
  ClientHeight = 344
  ExplicitWidth = 440
  ExplicitHeight = 378
  PixelsPerInch = 96
  TextHeight = 18
  object vstDataSets: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 416
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    OnChecked = vstDataSetsChecked
    OnGetText = vstDataSetsGetText
    OnGetNodeDataSize = vstDataSetsGetNodeDataSize
    Columns = <>
  end
  object rgOrientation: TRadioGroup
    Left = 8
    Top = 271
    Width = 113
    Height = 65
    Anchors = [akLeft, akBottom]
    Caption = 'Orientation'
    ItemIndex = 1
    Items.Strings = (
      '2D Top'
      '3D')
    TabOrder = 1
    OnClick = rgOrientationClick
  end
  object rgEvaluatedAt: TRadioGroup
    Left = 127
    Top = 271
    Width = 122
    Height = 65
    Anchors = [akLeft, akBottom]
    Caption = 'Evaluated at'
    ItemIndex = 0
    Items.Strings = (
      'Elements'
      'Nodes')
    TabOrder = 2
    OnClick = rgEvaluatedAtClick
  end
  object btnSave: TBitBtn
    Left = 320
    Top = 309
    Width = 104
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = 'Save Data'
    TabOrder = 3
    OnClick = btnSaveClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
      7700333333337777777733333333008088003333333377F73377333333330088
      88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
      000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
      FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
      99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
      99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
      99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
      93337FFFF7737777733300000033333333337777773333333333}
    NumGlyphs = 2
  end
  object btnHelp: TBitBtn
    Left = 320
    Top = 271
    Width = 104
    Height = 27
    Anchors = [akRight, akBottom]
    TabOrder = 4
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object sdSaveCSV: TSaveDialog
    DefaultExt = 'csv'
    Filter = 'Comma Separated Value files (*.csv)|*.csv|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    OnTypeChange = sdSaveCSVTypeChange
    Left = 264
    Top = 288
  end
end
