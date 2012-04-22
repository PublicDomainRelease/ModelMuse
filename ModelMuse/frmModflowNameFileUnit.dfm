inherited frmModflowNameFile: TfrmModflowNameFile
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Name_File_Dialog_Box'
  Caption = 'MODFLOW Name File'
  ClientHeight = 346
  ClientWidth = 510
  ExplicitWidth = 518
  ExplicitHeight = 380
  PixelsPerInch = 96
  TextHeight = 18
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 510
    Height = 305
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 483
    DesignSize = (
      510
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
      Width = 494
      Height = 138
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitWidth = 467
    end
    object cbFlowPackage: TCheckBox
      Left = 8
      Top = 216
      Width = 449
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Flow package other than BCF6, LPF, HUF2, or UPW included'
      TabOrder = 1
    end
    object cbSolvers: TCheckBox
      Left = 8
      Top = 239
      Width = 481
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Solver other than PCG, PCGN, GMG, SIP, DE4, or NWT included'
      TabOrder = 2
    end
    object btnHelp: TBitBtn
      Left = 244
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = btnHelpClick
      ExplicitLeft = 217
    end
    object btnOK: TBitBtn
      Left = 332
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = btnOKClick
      ExplicitLeft = 305
    end
    object btnCancel: TBitBtn
      Left = 420
      Top = 260
      Width = 82
      Height = 34
      Anchors = [akRight, akBottom]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 5
      ExplicitLeft = 393
    end
  end
  object pnlModel: TPanel
    Left = 0
    Top = 0
    Width = 510
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 483
    DesignSize = (
      510
      41)
    object lblModel: TLabel
      Left = 459
      Top = 11
      Width = 43
      Height = 18
      Anchors = [akTop, akRight]
      Caption = 'Model'
      ExplicitLeft = 432
    end
    object comboModel: TComboBox
      Left = 8
      Top = 8
      Width = 445
      Height = 26
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = comboModelChange
      ExplicitWidth = 418
    end
  end
end
