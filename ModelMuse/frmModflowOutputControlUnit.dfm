inherited frmModflowOutputControl: TfrmModflowOutputControl
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Output_Control_Dialog_Box'
  Caption = 'MODFLOW Output Control'
  ClientHeight = 373
  ClientWidth = 549
  ExplicitWidth = 557
  ExplicitHeight = 407
  PixelsPerInch = 96
  TextHeight = 18
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 121
    Top = 0
    Height = 332
    Align = alLeft
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 0
    ExplicitTop = 160
    ExplicitHeight = 100
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 332
    Width = 549
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      549
      41)
    object btnHelp: TBitBtn
      Left = 285
      Top = 7
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 373
      Top = 7
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 461
      Top = 7
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
  end
  object pltrPageNavigator: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 332
    AutoExpand = False
    ShowLines = True
    PageDefault = 0
    PageList = jvPages
    Align = alLeft
    HideSelection = False
    Indent = 19
    RowSelect = True
    TabOrder = 1
    Items.NodeData = {
      0104000000270000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0007470065006E006500720061006C00210000000000000000000000FFFFFFFF
      FFFFFFFF01000000000000000448006500610064002900000000000000000000
      00FFFFFFFFFFFFFFFF020000000000000008440072006100770064006F007700
      6E00250000000000000000000000FFFFFFFFFFFFFFFF00000000000000000642
      0075006400670065007400}
    Items.Links = {0400000000000000000000000000000000000000}
  end
  object jvPages: TJvPageList
    Left = 131
    Top = 0
    Width = 418
    Height = 332
    ActivePage = jvspDrawdown
    PropagateEnable = False
    Align = alClient
    OnChange = jvPagesChange
    object jvspGeneral: TJvStandardPage
      Left = 0
      Top = 0
      Width = 418
      Height = 332
      HelpType = htKeyword
      HelpKeyword = 'General_Pane'
      Caption = 'jvspGeneral'
      DesignSize = (
        418
        332)
      object Comments: TLabel
        Left = 6
        Top = 49
        Width = 76
        Height = 18
        Caption = 'Comments'
      end
      object cbPrintInputArrays: TJvCheckBox
        Left = 6
        Top = 3
        Width = 137
        Height = 18
        Caption = 'Print input arrays'
        Checked = True
        State = cbChecked
        TabOrder = 0
        LinkedControls = <>
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = 17
        HotTrackFont.Name = 'Microsoft Sans Serif'
        HotTrackFont.Pitch = fpVariable
        HotTrackFont.Style = []
      end
      object cbPrintInputCellLists: TJvCheckBox
        Left = 6
        Top = 26
        Width = 148
        Height = 18
        Caption = 'Print input cell lists'
        Checked = True
        State = cbChecked
        TabOrder = 1
        LinkedControls = <>
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = 17
        HotTrackFont.Name = 'Microsoft Sans Serif'
        HotTrackFont.Pitch = fpVariable
        HotTrackFont.Style = []
      end
      object memoComments: TMemo
        Left = 6
        Top = 72
        Width = 409
        Height = 254
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 2
      end
    end
    object jvspHeads: TJvStandardPage
      Left = 0
      Top = 0
      Width = 418
      Height = 332
      HelpType = htKeyword
      HelpKeyword = 'Head_and_Drawdown_Panes'
      Caption = 'jvspHead'
      inline frameHead: TframeOutputControl
        Left = 0
        Top = 0
        Width = 418
        Height = 332
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 418
        ExplicitHeight = 332
        inherited lblOutputType: TLabel
          Width = 38
          Caption = 'Head'
          ExplicitWidth = 38
        end
        inherited lblExternalFormat: TLabel
          Width = 211
          Caption = 'External file format (CHEDFM)'
          ExplicitWidth = 211
        end
        inherited lblDot: TLabel
          Font.Pitch = fpVariable
        end
        inherited lblListinglFormat: TLabel
          Width = 192
          Caption = 'Listing file format (IHEDFM)'
          ExplicitWidth = 192
        end
        inherited rcExternalFormat: TRbwController
          ControlList = <
            item
              Control = frameHead.adeD
            end
            item
              Control = frameHead.adeW
            end
            item
              Control = frameHead.comboP
            end
            item
              Control = frameHead.comboREdit
            end
            item
              Control = frameHead.lblResult
            end
            item
              Control = frameHead.lblDot
            end>
        end
        inherited rcListingFormat: TRbwController
          ControlList = <
            item
              Control = frameHead.comboPrintFormat
            end
            item
              Control = frameHead.comboPrintStyle
            end>
        end
      end
    end
    object jvspDrawdown: TJvStandardPage
      Left = 0
      Top = 0
      Width = 418
      Height = 332
      HelpType = htKeyword
      HelpKeyword = 'Head_and_Drawdown_Panes'
      Caption = 'jvspDrawdown'
      inline frameDrawdown: TframeOutputControl
        Left = 0
        Top = 0
        Width = 418
        Height = 332
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 418
        ExplicitHeight = 332
        inherited lblOutputType: TLabel
          Width = 74
          Caption = 'Drawdown'
          ExplicitWidth = 74
        end
        inherited lblExternalFormat: TLabel
          Width = 212
          Caption = 'External file format (CDDNFM)'
          ExplicitWidth = 212
        end
        inherited lblDot: TLabel
          Font.Pitch = fpVariable
        end
        inherited lblListinglFormat: TLabel
          Width = 193
          Caption = 'Listing file format (IDDNFM)'
          ExplicitWidth = 193
        end
        inherited rcExternalFormat: TRbwController
          ControlList = <
            item
              Control = frameDrawdown.adeD
            end
            item
              Control = frameDrawdown.adeW
            end
            item
              Control = frameDrawdown.comboP
            end
            item
              Control = frameDrawdown.comboREdit
            end
            item
              Control = frameDrawdown.lblResult
            end
            item
              Control = frameDrawdown.lblDot
            end>
        end
        inherited rcListingFormat: TRbwController
          ControlList = <
            item
              Control = frameDrawdown.comboPrintFormat
            end
            item
              Control = frameDrawdown.comboPrintStyle
            end>
        end
      end
    end
    object jvspBudget: TJvStandardPage
      Left = 0
      Top = 0
      Width = 418
      Height = 332
      HelpType = htKeyword
      HelpKeyword = 'Budget_Pane'
      Caption = 'jvspBudget'
      object lblN: TLabel
        Left = 16
        Top = 113
        Width = 24
        Height = 18
        Caption = 'N ='
      end
      object lblFrequency: TLabel
        Left = 16
        Top = 54
        Width = 73
        Height = 18
        Caption = 'Frequency'
      end
      object lblBudget: TLabel
        Left = 16
        Top = 8
        Width = 50
        Height = 18
        Caption = 'Budget'
      end
      object cbCompact: TJvCheckBox
        Left = 16
        Top = 31
        Width = 137
        Height = 18
        Caption = 'Compact budget'
        Checked = True
        State = cbChecked
        TabOrder = 0
        LinkedControls = <>
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = 17
        HotTrackFont.Name = 'Microsoft Sans Serif'
        HotTrackFont.Pitch = fpVariable
        HotTrackFont.Style = []
      end
      object rgSaveCellFlows: TJvRadioGroup
        Left = 16
        Top = 145
        Width = 385
        Height = 48
        Caption = 'Save cell flows'
        Columns = 3
        Items.Strings = (
          'None'
          'Binary'
          'Listing')
        TabOrder = 1
        CaptionVisible = True
      end
      object comboFrequency: TJvImageComboBox
        Left = 16
        Top = 77
        Width = 393
        Height = 28
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 409
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 22
        ItemIndex = 0
        TabOrder = 2
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'First N times steps and each N'#39'th time step thereafter'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Last time step of each N'#39'th stress period'
          end>
      end
      object spN: TJvSpinEdit
        Left = 44
        Top = 110
        Width = 65
        Height = 26
        CheckMaxValue = False
        ButtonKind = bkClassic
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 3
      end
    end
  end
end
