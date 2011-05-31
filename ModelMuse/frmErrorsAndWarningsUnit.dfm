inherited frmErrorsAndWarnings: TfrmErrorsAndWarnings
  HelpType = htKeyword
  HelpKeyword = 'Errors_and_Warnings_Dialog_Box'
  Caption = 'Errors and Warnings'
  ClientWidth = 472
  OnResize = FormResize
  ExplicitWidth = 480
  ExplicitHeight = 268
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 193
    Width = 472
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      472
      41)
    object btnClose: TBitBtn
      Left = 374
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkClose
    end
    object btnHelp: TBitBtn
      Left = 279
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnSave: TButton
      Left = 88
      Top = 4
      Width = 75
      Height = 33
      Caption = 'Save'
      TabOrder = 2
      OnClick = btnSaveClick
    end
    object btnClear: TButton
      Left = 169
      Top = 4
      Width = 75
      Height = 33
      Caption = 'Clear'
      TabOrder = 3
      OnClick = btnClearClick
    end
    object btnCopy: TButton
      Left = 7
      Top = 4
      Width = 75
      Height = 33
      Caption = 'Copy'
      TabOrder = 4
      OnClick = btnCopyClick
    end
  end
  object vstWarningsAndErrors: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 472
    Height = 193
    Align = alClient
    CheckImageKind = ckLightTick
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag]
    TabOrder = 1
    OnGetText = vstWarningsAndErrorsGetText
    OnInitNode = vstWarningsAndErrorsInitNode
    OnMeasureItem = vstWarningsAndErrorsMeasureItem
    Columns = <
      item
        Position = 0
        Width = 200
      end>
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 136
    Top = 72
  end
  object sdSaveFileDlg: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 176
    Top = 88
  end
end
