inherited frmExportShapefileObjects: TfrmExportShapefileObjects
  HelpType = htKeyword
  HelpKeyword = 'Export_Objects_to_Shapefile'
  Caption = 'Export Objects as Shapefile'
  ClientWidth = 617
  OnResize = FormResize
  ExplicitWidth = 625
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 18
  object Splitter1: TSplitter [0]
    Left = 300
    Top = 41
    Height = 287
    OnMoved = Splitter1Moved
    ExplicitLeft = 304
    ExplicitTop = 128
    ExplicitHeight = 100
  end
  inherited pnlBottom: TPanel
    Top = 328
    Width = 617
    Height = 86
    ExplicitTop = 328
    ExplicitWidth = 617
    ExplicitHeight = 86
    object lblMissingData: TLabel [0]
      Left = 437
      Top = 16
      Width = 129
      Height = 18
      Caption = 'Missing data value'
    end
    inherited btnClose: TBitBtn
      Left = 421
      Top = 46
      Caption = '&OK'
      OnClick = btnCloseClick
      Kind = bkOK
      ExplicitLeft = 421
      ExplicitTop = 46
    end
    inherited btnHelp: TBitBtn
      Left = 326
      Top = 46
      OnClick = btnHelpClick
      ExplicitLeft = 326
      ExplicitTop = 46
    end
    object BitBtn1: TBitBtn
      Left = 516
      Top = 46
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
    object rdeMissingData: TRbwDataEntry
      Left = 326
      Top = 13
      Width = 105
      Height = 27
      ItemHeight = 0
      TabOrder = 3
      Text = '-99900000'
      DataType = dtInteger
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object gbExportAs: TGroupBox
      Left = 8
      Top = 8
      Width = 249
      Height = 73
      Caption = 'Export as'
      TabOrder = 4
      object rbPoints: TRadioButton
        Left = 8
        Top = 24
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Points'
        TabOrder = 0
      end
      object rbMultipoint: TRadioButton
        Left = 8
        Top = 47
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Multipoints'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
      object rbPolyline: TRadioButton
        Left = 132
        Top = 24
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Polylines'
        TabOrder = 2
      end
      object rbPolygons: TRadioButton
        Left = 132
        Top = 47
        Width = 113
        Height = 17
        Margins.Left = 8
        Caption = 'Polygons'
        TabOrder = 3
      end
    end
  end
  inherited vstObjects: TVirtualStringTree
    Left = 303
    Top = 41
    Width = 314
    Height = 287
    OnChecked = vstObjectsChecked
    ExplicitLeft = 303
    ExplicitTop = 41
    ExplicitWidth = 314
    ExplicitHeight = 287
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 41
    Align = alTop
    TabOrder = 2
    object lblObjects: TLabel
      Left = 404
      Top = 12
      Width = 117
      Height = 18
      Caption = 'Objects to export'
    end
    object lblDataArrays: TLabel
      Left = 70
      Top = 12
      Width = 131
      Height = 18
      Caption = 'Data sets to export'
    end
  end
  object vstDataSets: TVirtualStringTree
    Left = 0
    Top = 41
    Width = 300
    Height = 287
    Align = alLeft
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    TabOrder = 3
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
    OnChecked = vstDataSetsChecked
    OnChecking = vstDataSetsChecking
    OnGetText = vstDataSetsGetText
    Columns = <>
  end
  object sdShapefile: TSaveDialog
    DefaultExt = '.shp'
    Filter = 'Shapefiles (*.shp)|*.shp'
    Left = 192
    Top = 96
  end
  object XBaseShapeFile: TXBase
    Active = False
    AutoUpDate = True
    DebugErr = False
    Deleted = False
    Left = 232
    Top = 96
  end
end
