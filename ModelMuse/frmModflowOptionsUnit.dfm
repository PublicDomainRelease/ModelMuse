inherited frmModflowOptions: TfrmModflowOptions
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Options_Dialog_Box'
  Caption = 'MODFLOW Options'
  ClientHeight = 314
  ClientWidth = 543
  ExplicitWidth = 551
  ExplicitHeight = 348
  PixelsPerInch = 96
  TextHeight = 18
  object pcOptions: TPageControl
    Left = 0
    Top = 0
    Width = 543
    Height = 273
    ActivePage = tabWetting
    Align = alClient
    TabOrder = 0
    OnChange = pcOptionsChange
    object TabSheet1: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Description_Tab'
      Caption = 'Description'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        535
        240)
      object Label3: TLabel
        Left = 8
        Top = 1
        Width = 92
        Height = 18
        Caption = 'Project name'
      end
      object Label4: TLabel
        Left = 8
        Top = 55
        Width = 34
        Height = 18
        Caption = 'Date'
      end
      object Label2: TLabel
        Left = 135
        Top = 55
        Width = 57
        Height = 18
        Caption = 'Modeler'
      end
      object Label1: TLabel
        Left = 8
        Top = 109
        Width = 152
        Height = 18
        Caption = 'Description of project.'
      end
      object edProjectName: TEdit
        Left = 8
        Top = 24
        Width = 520
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = edProjectNameExit
      end
      object edDate: TEdit
        Left = 8
        Top = 78
        Width = 121
        Height = 26
        TabOrder = 1
        OnExit = edDateExit
      end
      object edModeler: TEdit
        Left = 135
        Top = 78
        Width = 393
        Height = 26
        TabOrder = 2
        OnExit = edModelerExit
      end
      object memoComments: TMemo
        Left = 8
        Top = 132
        Width = 520
        Height = 101
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 3
        WordWrap = False
        OnExit = memoCommentsExit
      end
    end
    object TabSheet2: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Options_Tab'
      Caption = 'Options'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 3
        Top = 103
        Width = 271
        Height = 18
        Caption = 'Head value for inactive cells (HNOFLO)'
      end
      object Label6: TLabel
        Left = 342
        Top = 154
        Width = 127
        Height = 18
        Caption = 'Time unit (ITMUNI)'
      end
      object Label7: TLabel
        Left = 342
        Top = 103
        Width = 145
        Height = 18
        Caption = 'Length unit (LENUNI)'
      end
      object Label8: TLabel
        Left = 3
        Top = 154
        Width = 307
        Height = 18
        Caption = 'Head value for cells that become dry (HDRY)'
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 3
        Width = 529
        Height = 78
        Caption = 'Basic package options'
        TabOrder = 0
        object cbPRINTTIME: TJvCheckBox
          Left = 8
          Top = 43
          Width = 437
          Height = 18
          Caption = 'Print the start time, end time, and elapsed time (PRINTTIME)'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbPRINTTIMEClick
          LinkedControls = <>
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = 17
          HotTrackFont.Name = 'Microsoft Sans Serif'
          HotTrackFont.Pitch = fpVariable
          HotTrackFont.Style = []
        end
        object cbCHTOCH: TJvCheckBox
          Left = 8
          Top = 20
          Width = 461
          Height = 18
          Caption = 'Calculate flow between adjacent constant-head cells (CHTOCH)'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbCHTOCHClick
          LinkedControls = <>
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = 17
          HotTrackFont.Name = 'Microsoft Sans Serif'
          HotTrackFont.Pitch = fpVariable
          HotTrackFont.Style = []
        end
      end
      object rdeHNOFLO: TRbwDataEntry
        Left = 3
        Top = 126
        Width = 145
        Height = 22
        ItemHeight = 0
        TabOrder = 1
        Text = '-1e20'
        OnExit = rdeHNOFLOExit
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object comboTimeUnit: TJvComboBox
        Left = 342
        Top = 177
        Width = 145
        Height = 26
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 2
        Text = 'seconds (1)'
        OnChange = comboTimeUnitChange
        Items.Strings = (
          'undefined (0)'
          'seconds (1)'
          'minutes (2)'
          'hours (3)'
          'days (4)'
          'years (5)')
        ItemIndex = 1
      end
      object comboLengthUnit: TJvComboBox
        Left = 342
        Top = 126
        Width = 145
        Height = 26
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 3
        Text = 'meters (2)'
        OnChange = comboLengthUnitChange
        Items.Strings = (
          'undefined (0)'
          'feet (1)'
          'meters (2)'
          'centimeters (3)')
        ItemIndex = 2
      end
      object rdeHDRY: TRbwDataEntry
        Left = 3
        Top = 177
        Width = 145
        Height = 22
        ItemHeight = 0
        TabOrder = 4
        Text = '-2e20'
        OnExit = rdeHDRYExit
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object cbOpenInTextEditor: TJvCheckBox
        Left = 3
        Top = 208
        Width = 361
        Height = 18
        Caption = 'Open listing file in text editor when model is done.'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = cbOpenInTextEditorClick
        LinkedControls = <>
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = 17
        HotTrackFont.Name = 'Microsoft Sans Serif'
        HotTrackFont.Pitch = fpVariable
        HotTrackFont.Style = []
      end
    end
    object tabWetting: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Wetting_Tab'
      Caption = 'Wetting'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblWetFact: TLabel
        Left = 8
        Top = 35
        Width = 213
        Height = 36
        Caption = 'Wetting Factor (WETFCT) (usually between zero and one)'
        Enabled = False
        WordWrap = True
      end
      object lblCheckDry: TLabel
        Left = 8
        Top = 107
        Width = 303
        Height = 18
        Caption = 'Iterations to  check for wetting cells (IWETIT)'
        Enabled = False
      end
      object lblWettingEquation: TLabel
        Left = 8
        Top = 163
        Width = 271
        Height = 18
        Caption = 'Equation for Rewetting Cells (IHDWET)'
        Enabled = False
      end
      object lblWettingDataSets: TLabel
        Left = 245
        Top = 3
        Width = 243
        Height = 90
        Caption = 
          'You will need to have at least one unconfined or convertible lay' +
          'er and assign non-zero values to the Wet_Dry_Threshold and Wet_D' +
          'ry_Flag data sets'
        Visible = False
        WordWrap = True
      end
      object rdeWettingFact: TRbwDataEntry
        Left = 8
        Top = 75
        Width = 92
        Height = 22
        Hint = 'This affects model stability'
        HelpContext = 290
        Color = clBtnFace
        Enabled = False
        ItemHeight = 0
        TabOrder = 0
        Text = '0.5'
        OnChange = rdeWettingFactChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seCheckDry: TJvSpinEdit
        Left = 8
        Top = 130
        Width = 92
        Height = 26
        CheckMaxValue = False
        ButtonKind = bkClassic
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 1
        OnChange = seCheckDryChange
      end
      object comboWettingEquation: TJvImageComboBox
        Left = 8
        Top = 186
        Width = 305
        Height = 28
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 305
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 22
        ItemIndex = 0
        TabOrder = 2
        OnChange = comboWettingEquationChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'h = BOT + WETFCT (hh-BOT) (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'h = BOT + WETFCT (WETDRY) (1)'
          end>
      end
      object cbWetting: TCheckBox
        Left = 8
        Top = 12
        Width = 153
        Height = 17
        Caption = 'Wetting active'
        TabOrder = 3
        OnClick = cbWettingClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 273
    Width = 543
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      543
      41)
    object btnHelp: TBitBtn
      Left = 249
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 346
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOKClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object btnCancel: TBitBtn
      Left = 441
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
  end
  object rconWet: TRbwController
    ControlList = <
      item
        Control = comboWettingEquation
      end
      item
        Control = seCheckDry
      end
      item
        Control = rdeWettingFact
      end
      item
        Control = lblWetFact
      end
      item
        Control = lblWettingEquation
      end
      item
        Control = lblCheckDry
      end>
    Enabled = False
    Left = 232
    Top = 16
  end
end
