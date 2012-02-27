inherited frmCustomSelectObjects: TfrmCustomSelectObjects
  Caption = 'frmCustomSelectObjects'
  ClientHeight = 414
  ClientWidth = 422
  ExplicitWidth = 430
  ExplicitHeight = 448
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 373
    Width = 422
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      422
      41)
    object btnClose: TBitBtn
      Left = 330
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object btnHelp: TBitBtn
      Left = 237
      Top = 4
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
    end
  end
  object vstObjects: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 422
    Height = 373
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
    OnChecking = vstObjectsChecking
    OnFreeNode = vstObjectsFreeNode
    OnGetText = vstObjectsGetText
    OnInitNode = vstObjectsInitNode
    OnStateChange = vstObjectsStateChange
    Columns = <>
  end
end
