inherited frmContourData: TfrmContourData
  HelpType = htKeyword
  HelpKeyword = 'Contour_Data_Dialog_Box'
  Caption = 'Contour Data'
  DesignSize = (
    606
    489)
  PixelsPerInch = 96
  TextHeight = 18
  object cbSpecifyContours: TJvCheckBox [0]
    Left = 367
    Top = 15
    Width = 96
    Height = 41
    Anchors = [akTop, akRight]
    Caption = 'Specify contours'
    TabOrder = 0
    WordWrap = True
    OnClick = cbSpecifyContoursClick
    LinkedControls = <>
    AutoSize = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = 17
    HotTrackFont.Name = 'Microsoft Sans Serif'
    HotTrackFont.Pitch = fpVariable
    HotTrackFont.Style = []
  end
  object btnEditContours: TButton [1]
    Left = 469
    Top = 23
    Width = 119
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Edit contours...'
    Enabled = False
    TabOrder = 3
    OnClick = btnEditContoursClick
  end
  inherited pcChoices: TPageControl
    TabOrder = 2
    inherited tabSelection: TTabSheet
      inherited virttreecomboDataSets: TTntExDropDownVirtualStringTree
        Top = 29
        Width = 337
        Anchors = [akLeft, akTop, akRight]
        Tree.OnGetNodeDataSize = virttreecomboDataSetsDropDownTreeGetNodeDataSize
        PanelWidth = 337
        ExplicitTop = 29
        ExplicitWidth = 337
      end
      inherited cbLogTransform: TCheckBox
        OnClick = cbLogTransformClick
      end
      inherited udDataSets: TJvUpDown
        Left = 351
        Top = 27
        Anchors = [akTop, akRight]
        ExplicitLeft = 351
        ExplicitTop = 27
      end
    end
    inherited tabFilters: TTabSheet
      inherited lblLowerLimit: TLabel
        Anchors = [akLeft, akTop, akBottom]
      end
      inherited lblValuesToIgnore: TLabel
        Top = 56
        ExplicitTop = 56
      end
      inherited cbActiveOnly: TCheckBox
        Left = 498
        Top = 1
        Visible = False
        ExplicitLeft = 498
        ExplicitTop = 1
      end
      inherited rdgValuesToIgnore: TRbwDataGrid4
        Top = 77
        Height = 296
        ExplicitTop = 77
        ExplicitHeight = 296
      end
    end
  end
  inherited Panel1: TPanel
    inherited btnOK: TBitBtn
      OnClick = btnOKClick
    end
  end
end
