inherited frmErrorsAndWarnings: TfrmErrorsAndWarnings
  HelpType = htKeyword
  HelpKeyword = 'Errors_and_Warnings_Dialog_Box'
  Caption = 'Errors and Warnings'
  OnResize = FormResize
  ExplicitWidth = 442
  ExplicitHeight = 274
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 193
    Width = 432
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      432
      41)
    object btnClose: TBitBtn
      Left = 334
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkClose
    end
    object btnHelp: TBitBtn
      Left = 239
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnSave: TButton
      Left = 8
      Top = 4
      Width = 75
      Height = 33
      Caption = 'Save'
      TabOrder = 2
      OnClick = btnSaveClick
    end
    object btnClear: TButton
      Left = 89
      Top = 6
      Width = 75
      Height = 31
      Caption = 'Clear'
      TabOrder = 3
      OnClick = btnClearClick
    end
  end
  object vstWarningsAndErrors: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 432
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
    ExplicitWidth = 434
    ExplicitHeight = 199
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
