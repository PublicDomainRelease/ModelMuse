inherited frmModflowNameFile: TfrmModflowNameFile
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Name_File_Dialog_Box'
  Caption = 'MODFLOW Name File'
  ClientHeight = 346
  ClientWidth = 483
  ExplicitWidth = 491
  ExplicitHeight = 380
  PixelsPerInch = 96
  TextHeight = 18
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 483
    Height = 305
    Align = alClient
    TabOrder = 0
    DesignSize = (
      483
      305)
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
      Height = 138
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object cbFlowPackage: TCheckBox
      Left = 8
      Top = 216
      Width = 433
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Flow package other than BCF6, LPF, or HUF2 included'
      TabOrder = 1
    end
    object cbSolvers: TCheckBox
      Left = 8
      Top = 239
      Width = 409
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Solver other than PCG, GMG, SIP or DE4 included'
      TabOrder = 2
    end
    object btnHelp: TBitBtn
      Left = 217
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      TabOrder = 3
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 305
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      TabOrder = 4
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 393
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      TabOrder = 5
      Kind = bkCancel
    end
  end
  object pnlModel: TPanel
    Left = 0
    Top = 0
    Width = 483
    Height = 41
    Align = alTop
    TabOrder = 1
    DesignSize = (
      483
      41)
    object lblModel: TLabel
      Left = 432
      Top = 11
      Width = 43
      Height = 18
      Anchors = [akTop, akRight]
      Caption = 'Model'
    end
    object comboModel: TComboBox
      Left = 8
      Top = 8
      Width = 418
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 18
      TabOrder = 0
      OnChange = comboModelChange
    end
  end
end
