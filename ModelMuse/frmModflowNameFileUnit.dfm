inherited frmModflowNameFile: TfrmModflowNameFile
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Name_File_Dialog_Box'
  Caption = 'MODFLOW Name File'
  ClientHeight = 342
  ClientWidth = 483
  ExplicitWidth = 491
  ExplicitHeight = 376
  PixelsPerInch = 96
  TextHeight = 18
  object lblLines: TLabel
    Left = 8
    Top = 8
    Width = 466
    Height = 54
    Caption = 
      'Additional lines to add to the MODFLOW name file.'#13#10'(Use this spa' +
      'ce to add input files generated outside of ModelMuse.)'#13#10'Unit num' +
      'bers 70-95 are available for the user.'
  end
  object memoLines: TMemo
    Left = 8
    Top = 72
    Width = 467
    Height = 175
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object btnCancel: TBitBtn
    Left = 393
    Top = 297
    Width = 82
    Height = 34
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 305
    Top = 297
    Width = 82
    Height = 34
    Anchors = [akRight, akBottom]
    TabOrder = 2
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnHelp: TBitBtn
    Left = 217
    Top = 297
    Width = 82
    Height = 34
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object cbFlowPackage: TCheckBox
    Left = 8
    Top = 253
    Width = 433
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Flow package other than BCF6, LPF, or HUF2 included'
    TabOrder = 4
  end
  object cbSolvers: TCheckBox
    Left = 8
    Top = 276
    Width = 409
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Solver other than PCG, GMG, SIP or DE4 included'
    TabOrder = 5
  end
end
