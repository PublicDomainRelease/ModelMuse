object frameView: TframeView
  Left = 0
  Top = 0
  Width = 516
  Height = 262
  HorzScrollBar.Range = 49
  VertScrollBar.Range = 54
  TabOrder = 0
  TabStop = True
  OnMouseMove = rulerMouseMove
  object rulVertical: TRbwRuler
    Left = 0
    Top = 54
    Width = 49
    Height = 208
    Hint = 'Double-click to edit|Double-click to edit the ruler format'
    Align = alLeft
    ParentShowHint = False
    ShowHint = True
    OnDblClick = rulerDblClick
    OnMouseMove = rulerMouseMove
    RulerDesiredSpacing = 100
    RulerDigits = 0
    RulerEnds.Lower = 10
    RulerEnds.Upper = 230
    RulerLinePosition = 30
    RulerMajorTickLength = 10
    RulerMinorTickLength = 5
    RulerPosition = rpLeft
    RulerPrecision = 5
    RulerStart = sBottomRight
    RulerTextOffset = 5
    RulerTextPosition = tpOutside
    RulerValues.Upper = 100.000000000000000000
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 516
    Height = 54
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object ModelCube: TRbwModelCube
      Left = 0
      Top = 0
      Width = 49
      Height = 54
      Hint = 
        'Click to change layer|Click to change the selected layer up or d' +
        'own.'
      Align = alLeft
      ParentShowHint = False
      ShowHint = True
      OnPaint = ModelCubePaint
      CanClickFace = False
      CubeFraction = 0.150000000000000000
      CubeHeight = 25
      CubeLeftX = 5
      CubeTopY = 20
      CubeVanishingPointX = 118
      CubeVanishingPointY = -40
      CubeWidth = 25
      Opaque = False
      SelectedFace = faTop
      Selection2 = 0.250000000000000000
      ShowSelection = True
      XOrigin = xoWest
      YOrigin = yoSouth
      ZOrigin = zoBottom
      ExplicitLeft = -6
      ExplicitTop = -6
    end
    object rulHorizontal: TRbwRuler
      Left = 49
      Top = 0
      Width = 467
      Height = 54
      Hint = 'Double-click to edit|Double-click to edit the ruler format'
      Align = alClient
      ParentShowHint = False
      ShowHint = True
      OnDblClick = rulerDblClick
      OnMouseMove = rulerMouseMove
      RulerDesiredSpacing = 100
      RulerDigits = 0
      RulerEnds.Lower = 60
      RulerEnds.Upper = 480
      RulerLinePosition = 30
      RulerMajorTickLength = 10
      RulerMinorTickLength = 5
      RulerPosition = rpTop
      RulerPrecision = 5
      RulerStart = sTopLeft
      RulerTextOffset = 5
      RulerTextPosition = tpOutside
      RulerValues.Upper = 100.000000000000000000
    end
  end
  object ZoomBox: TQRbwZoomBox2
    Left = 49
    Top = 54
    Width = 467
    Height = 208
    Hint = 'Choose a tool and click to edit model'
    Align = alClient
    ParentColor = True
    ParentShowHint = False
    PopupMenu = OrderMenu
    ShowHint = True
    TabOrder = 1
    OnExit = ZoomBoxExit
    OnResize = ZoomBoxResize
    Exaggeration = 1.000000000000000000
    HorizontalDirection = hdRight
    Image32.Left = 0
    Image32.Top = 167
    Image32.Width = 467
    Image32.Height = 208
    Image32.Anchors = [akLeft, akBottom]
    Image32.Bitmap.ResamplerClassName = 'TNearestResampler'
    Image32.BitmapAlign = baTopLeft
    Image32.Color = clWhite
    Image32.ParentColor = False
    Image32.Scale = 1.000000000000000000
    Image32.ScaleMode = smNormal
    Image32.TabOrder = 0
    Image32.OnDblClick = ZoomBoxImage32DblClick
    Image32.OnMouseDown = ZoomBoxImage32MouseDown
    Image32.OnMouseMove = ZoomBoxImage32MouseMove
    Image32.OnMouseUp = ZoomBoxImage32MouseUp
    Image32.ExplicitTop = 0
    ImmediateResize = True
    Magnification = 1.000000000000000000
    VerticalDirection = vdUp
    OnPan = ZoomBoxPan
    DesignSize = (
      467
      208)
  end
  object OrderMenu: TPopupMenu
    OnPopup = OrderMenuPopup
    Left = 56
    Top = 64
    object ToFront: TMenuItem
      Caption = 'To Front'
      OnClick = ToFrontClick
    end
    object ToBack: TMenuItem
      Caption = 'To Back'
      OnClick = ToBackClick
    end
    object ForwardOne: TMenuItem
      Caption = 'Forward One'
      OnClick = ForwardOneClick
    end
    object BackOne: TMenuItem
      Caption = 'Back One'
      OnClick = BackOneClick
    end
    object Hide: TMenuItem
      Caption = 'Hide Selected Objects'
      Hint = 'Hide selected objects'
      OnClick = HideClick
    end
    object HideAllOthers: TMenuItem
      Caption = 'Hide Unselected Objects'
      Hint = 'Hide all objects that are not selected'
      OnClick = HideAllOthersClick
    end
    object ShowAll1: TMenuItem
      Caption = 'Show All'
      OnClick = ShowAll1Click
    end
    object miSelectAll: TMenuItem
      Caption = 'Select All'
      OnClick = miSelectAllClick
    end
    object miMergeObjects: TMenuItem
      Caption = '&Merge Objects'
      OnClick = miMergeObjectsClick
    end
    object miEditSelectedObjects: TMenuItem
      Caption = 'Edit Selected Objects'
    end
  end
end