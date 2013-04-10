inherited frmSutraOptions: TfrmSutraOptions
  Caption = 'SUTRA Options'
  ClientHeight = 443
  ClientWidth = 750
  ExplicitWidth = 768
  ExplicitHeight = 488
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 393
    Width = 750
    Height = 50
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      750
      50)
    object btnCancel: TBitBtn
      Left = 640
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 543
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 446
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
  end
  object jplMain: TJvPageList
    Left = 169
    Top = 0
    Width = 581
    Height = 393
    ActivePage = jvspFluidProperties
    PropagateEnable = False
    Align = alClient
    object jvspConfiguration: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspConfiguration'
      DesignSize = (
        581
        393)
      object lblGravX: TLabel
        Left = 106
        Top = 90
        Width = 362
        Height = 18
        Caption = 'Component of gravity vector in +x direction (GRAVX)'
      end
      object lblGravY: TLabel
        Left = 106
        Top = 118
        Width = 362
        Height = 18
        Caption = 'Component of gravity vector in +Y direction (GRAVY)'
      end
      object lblGravZ: TLabel
        Left = 106
        Top = 145
        Width = 362
        Height = 18
        Caption = 'Component of gravity vector in +Z direction (GRAVZ)'
      end
      object rgMeshType: TRadioGroup
        Left = 6
        Top = 8
        Width = 251
        Height = 73
        Caption = 'Mesh type (MSHSTR)'
        ItemIndex = 1
        Items.Strings = (
          '2D'
          '3D')
        TabOrder = 0
        OnClick = rgMeshTypeClick
      end
      object rgSaturation: TRadioGroup
        Left = 263
        Top = 171
        Width = 305
        Height = 96
        Caption = 'Flow conditions (CUNSAT)'
        ItemIndex = 0
        Items.Strings = (
          'Saturated'
          'Unsaturated')
        TabOrder = 5
      end
      object rgTransport: TRadioGroup
        Left = 6
        Top = 170
        Width = 251
        Height = 97
        Caption = 'Transport (SIMULA)'
        ItemIndex = 0
        Items.Strings = (
          'Solute using pressure'
          'Solute using Head'
          'Energy')
        TabOrder = 4
        OnClick = rgTransportClick
      end
      object rgSimulationType: TRadioGroup
        Left = 6
        Top = 273
        Width = 564
        Height = 105
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Simulation type (CSSFLO, CSSTRA)'
        ItemIndex = 0
        Items.Strings = (
          'Steady-state flow, steady-state transport'
          'Steady-state flow, transient transport'
          'Transient flow, transient transport')
        TabOrder = 6
      end
      object rdeGravX: TRbwDataEntry
        Left = 6
        Top = 87
        Width = 94
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        ChangeDisabledColor = True
      end
      object rdeGravY: TRbwDataEntry
        Left = 6
        Top = 114
        Width = 94
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        ChangeDisabledColor = True
      end
      object rdeGravZ: TRbwDataEntry
        Left = 6
        Top = 142
        Width = 94
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        ChangeDisabledColor = True
      end
    end
    object jvspTitle: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspTitle'
      object jvedTitle: TJvEditor
        Left = 0
        Top = 49
        Width = 581
        Height = 344
        Cursor = crIBeam
        Completion.ItemHeight = 13
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        KeepTrailingBlanks = True
        CursorBeyondEOL = False
        BracketHighlighting.StringEscape = #39#39
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Courier New'
        Font.Style = []
      end
      object pnlTitleCaption: TPanel
        Left = 0
        Top = 0
        Width = 581
        Height = 49
        Align = alTop
        Alignment = taLeftJustify
        TabOrder = 0
        object lblTitle: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 573
          Height = 44
          Align = alClient
          Caption = 
            'The first 80 characters of the first two lines are TITLE1 and TI' +
            'TLE2.'#13#10'The remaining lines will be treated as comments.'
          ExplicitWidth = 458
          ExplicitHeight = 36
        end
      end
    end
    object jvspInitialCondition: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspInitialCondition'
      DesignSize = (
        581
        393)
      object lblRestartFile: TLabel
        Left = 6
        Top = 87
        Width = 74
        Height = 18
        Caption = 'Restart file'
      end
      object lblRestartFrequency: TLabel
        Left = 6
        Top = 143
        Width = 287
        Height = 18
        Caption = 'Frequency for saving restart file (ISTORE)'
      end
      object rgStartType: TRadioGroup
        Left = 6
        Top = 3
        Width = 564
        Height = 78
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Starting type (CREAD)'
        ItemIndex = 0
        Items.Strings = (
          'Cold start (start from first time step)'
          'Warm start (start from restart file)')
        TabOrder = 0
        OnClick = rgStartTypeClick
      end
      object fedRestartFile: TJvFilenameEdit
        Left = 6
        Top = 111
        Width = 564
        Height = 26
        Enabled = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object seRestartFrequency: TJvSpinEdit
        Left = 6
        Top = 167
        Width = 91
        Height = 26
        MaxValue = 2147483647.000000000000000000
        Value = 10000.000000000000000000
        TabOrder = 2
      end
    end
    object jvspNumericalControls: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspNumericalControls'
      object lblFractionalUpstreamWeight: TLabel
        Left = 79
        Top = 16
        Width = 223
        Height = 18
        Caption = 'Fractional upstream weight (UP)'
      end
      object lblPressureFactor: TLabel
        Left = 79
        Top = 44
        Width = 300
        Height = 18
        Caption = 'Pressure boundary condition factor (GNUP)'
      end
      object lblUFactor: TLabel
        Left = 79
        Top = 72
        Width = 422
        Height = 18
        Caption = 'Concentration/temperature boundary condition factor (GNUU)'
      end
      object lblMaxIterations: TLabel
        Left = 127
        Top = 96
        Width = 381
        Height = 36
        Caption = 
          'Maximum number of iterations allowed per time step to resolve no' +
          'nlinearities (ITRMAX)'
        WordWrap = True
      end
      object lblNonLinPressureCriterion: TLabel
        Left = 79
        Top = 135
        Width = 422
        Height = 36
        Caption = 
          'Absolute iteration convergence criterion for pressure solution (' +
          'RPMAX) '
        WordWrap = True
      end
      object lblUCriterion: TLabel
        Left = 79
        Top = 177
        Width = 422
        Height = 36
        Caption = 
          'Absolute iteration convergence criterion for transport solution ' +
          '(RUMAX) '
        WordWrap = True
      end
      object rdeFractionalUpstreamWeight: TRbwDataEntry
        Left = 16
        Top = 13
        Width = 57
        Height = 22
        TabOrder = 0
        Text = '0'
        OnChange = rdeFractionalUpstreamWeightChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdePressureFactor: TRbwDataEntry
        Left = 16
        Top = 41
        Width = 57
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeUFactor: TRbwDataEntry
        Left = 16
        Top = 69
        Width = 57
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seMaxIterations: TJvSpinEdit
        Left = 16
        Top = 103
        Width = 105
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 3
        OnChange = seMaxIterationsChange
      end
      object rdeNonLinPressureCriterion: TRbwDataEntry
        Left = 16
        Top = 132
        Width = 57
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeUCriterion: TRbwDataEntry
        Left = 16
        Top = 184
        Width = 57
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspSolverControls: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspSolverControls'
      DesignSize = (
        581
        393)
      object lblMaxPressureIterations: TLabel
        Left = 111
        Top = 114
        Width = 426
        Height = 36
        Caption = 
          'Maximum number of solver iterations during pressure solution (IT' +
          'RMXP)'
        WordWrap = True
      end
      object lblPressureCriterion: TLabel
        Left = 111
        Top = 156
        Width = 465
        Height = 36
        Caption = 
          'Convergence tolerance for solver iterations during pressure solu' +
          'tion (TOLP)'
        WordWrap = True
      end
      object lblMaxTransportIterations: TLabel
        Left = 111
        Top = 303
        Width = 426
        Height = 36
        Caption = 
          'Maximum number of solver iterations during transport solution (I' +
          'TRMXU)'
        WordWrap = True
      end
      object lblTransportCriterion: TLabel
        Left = 111
        Top = 345
        Width = 465
        Height = 36
        Caption = 
          'Convergence tolerance for solver iterations during transport sol' +
          'ution (TOLU)'
        WordWrap = True
      end
      object rgPressureSolution: TRadioGroup
        Left = 6
        Top = 3
        Width = 572
        Height = 105
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Pressure solution solver (CSOLVP)'
        Items.Strings = (
          'Banded Gaussian elimination (DIRECT)'
          'IC-preconditioned conjugate gradient (CG)'
          'ILU-preconditioned generalized minimum residual (GMRES)'
          'ILU-preconditioned orthomin (ORTHOMIN)')
        TabOrder = 0
        OnClick = rgPressureSolutionClick
      end
      object seMaxPressureIterations: TJvSpinEdit
        Left = 6
        Top = 119
        Width = 99
        Height = 26
        TabOrder = 1
      end
      object rdePressureCriterion: TRbwDataEntry
        Left = 6
        Top = 163
        Width = 99
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgUSolutionMethod: TRadioGroup
        Left = 6
        Top = 207
        Width = 572
        Height = 90
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Transport solution solver (CSOLVU)'
        Items.Strings = (
          'Banded Gaussian elimination (DIRECT)'
          'ILU-preconditioned generalized minimum residual (GMRES)'
          'ILU-preconditioned orthomin (ORTHOMIN)')
        TabOrder = 3
        OnClick = rgUSolutionMethodClick
      end
      object seMaxTransportIterations: TJvSpinEdit
        Left = 6
        Top = 313
        Width = 99
        Height = 26
        TabOrder = 4
      end
      object rdeTransportCriterion: TRbwDataEntry
        Left = 6
        Top = 345
        Width = 99
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspFluidProperties: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspFluidProperties'
      object lblFluidCompressibility: TLabel
        Left = 106
        Top = 14
        Width = 223
        Height = 18
        Caption = 'Fluid compressibility (COMPFL)'
      end
      object lblFluidSpecificHeat: TLabel
        Left = 106
        Top = 42
        Width = 167
        Height = 18
        Caption = 'Fluid specific heat (CW)'
      end
      object lblFluidDiffusivity: TLabel
        Left = 106
        Top = 70
        Width = 179
        Height = 18
        Caption = 'Fluid diffusivity (SIGMAW)'
      end
      object lblBaseFluidDensity: TLabel
        Left = 106
        Top = 126
        Width = 435
        Height = 18
        Caption = 'Density of fluid at base concentration or temperature (RHOW'#216')'
      end
      object lblBaseU: TLabel
        Left = 106
        Top = 154
        Width = 322
        Height = 18
        Caption = 'Base value of solute concentration (URHOW'#216')'
      end
      object lblFluidDensityCoefficientConcentration: TLabel
        Left = 106
        Top = 205
        Width = 429
        Height = 36
        Caption = 
          'Coefficient of fluid density change with concentration (fraction' +
          ') (DRWDU) '
        WordWrap = True
      end
      object lblViscosityScaleFactor: TLabel
        Left = 106
        Top = 271
        Width = 160
        Height = 18
        Caption = 'Fluid viscosity (VISC'#216')'
      end
      object lblScaleFactor: TLabel
        Left = 106
        Top = 299
        Width = 146
        Height = 18
        Caption = 'Scale factor (VISC'#216')'
      end
      object lblFluidThermalConductivity: TLabel
        Left = 106
        Top = 98
        Width = 251
        Height = 18
        Caption = 'Fluid thermal conductivity (SIGMAW)'
      end
      object lblFluidDensityCoefficientTemperature: TLabel
        Left = 106
        Top = 243
        Width = 448
        Height = 18
        Caption = 'Coefficient of fluid density change with or temperature (DRWDU) '
      end
      object lblBaseTemperature: TLabel
        Left = 106
        Top = 181
        Width = 268
        Height = 18
        Caption = 'Base value of temperature (URHOW'#216')'
      end
      object rdeFluidCompressibility: TRbwDataEntry
        Left = 6
        Top = 11
        Width = 94
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidSpecificHeat: TRbwDataEntry
        Left = 6
        Top = 39
        Width = 94
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidDiffusivity: TRbwDataEntry
        Left = 6
        Top = 67
        Width = 94
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeBaseFluidDensity: TRbwDataEntry
        Left = 6
        Top = 123
        Width = 94
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeBaseConcentration: TRbwDataEntry
        Left = 6
        Top = 151
        Width = 94
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidDensityCoefficientConcentration: TRbwDataEntry
        Left = 6
        Top = 212
        Width = 94
        Height = 22
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeViscosity: TRbwDataEntry
        Left = 6
        Top = 268
        Width = 94
        Height = 22
        TabOrder = 9
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeScaleFactor: TRbwDataEntry
        Left = 6
        Top = 296
        Width = 94
        Height = 22
        TabOrder = 10
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidThermalConductivity: TRbwDataEntry
        Left = 6
        Top = 95
        Width = 94
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidDensityCoefficientTemperature: TRbwDataEntry
        Left = 6
        Top = 240
        Width = 94
        Height = 22
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeBaseTemperature: TRbwDataEntry
        Left = 6
        Top = 178
        Width = 94
        Height = 22
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspSolidAdsorption: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspSolidAdsorption'
      DesignSize = (
        581
        393)
      object lblMatrixCompressibility: TLabel
        Left = 106
        Top = 14
        Width = 276
        Height = 18
        Caption = 'Solid matrix compressibility (COMPMA)'
      end
      object lblSolidGrainSpecificHeat: TLabel
        Left = 106
        Top = 42
        Width = 204
        Height = 18
        Caption = 'Solid grain specific heat (CS)'
      end
      object lblSolidGrainDiffusivity: TLabel
        Left = 106
        Top = 70
        Width = 217
        Height = 18
        Caption = 'Solid grain diffusivity (SIGMAS)'
      end
      object lblSolidGrainDensity: TLabel
        Left = 106
        Top = 98
        Width = 217
        Height = 18
        Caption = 'Density of a solid grain (RHOS)'
      end
      object lblFirstDistributionCoefficient: TLabel
        Left = 106
        Top = 237
        Width = 235
        Height = 18
        Caption = 'First distribution coefficient (CHI1)'
      end
      object lblSecondDistributionCoefficient: TLabel
        Left = 106
        Top = 265
        Width = 258
        Height = 18
        Caption = 'Second distribution coefficient (CHI2)'
      end
      object rdeMatrixCompressibility: TRbwDataEntry
        Left = 6
        Top = 11
        Width = 94
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSolidGrainSpecificHeat: TRbwDataEntry
        Left = 6
        Top = 39
        Width = 94
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSolidGrainDiffusivity: TRbwDataEntry
        Left = 6
        Top = 67
        Width = 94
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSolidGrainDensity: TRbwDataEntry
        Left = 6
        Top = 95
        Width = 94
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgSorptionModel: TRadioGroup
        Left = 6
        Top = 123
        Width = 564
        Height = 105
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Sorption model (ADSMOD)'
        Items.Strings = (
          'None'
          'Linear'
          'Freundlich'
          'Langmuir')
        TabOrder = 4
        OnClick = rgSorptionModelClick
      end
      object rdeFirstDistributionCoefficient: TRbwDataEntry
        Left = 6
        Top = 234
        Width = 94
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSecondDistributionCoefficient: TRbwDataEntry
        Left = 6
        Top = 262
        Width = 94
        Height = 22
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspProdGrav: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 393
      Caption = 'jvspProdGrav'
      object lblZeroFluidProd: TLabel
        Left = 106
        Top = 14
        Width = 354
        Height = 18
        Caption = 'Zero-order rate of production in the fluid (PRODF'#216')'
      end
      object lblZeroImmobProd: TLabel
        Left = 106
        Top = 42
        Width = 425
        Height = 18
        Caption = 'Zero-order rate of production in the imobile phase (PRODS'#216')'
      end
      object lblFirstFluidProd: TLabel
        Left = 106
        Top = 69
        Width = 437
        Height = 18
        Caption = 'First-order rate of solute mass production in the fluid (PRODF1)'
      end
      object lblFirstImmobProd: TLabel
        Left = 106
        Top = 94
        Width = 428
        Height = 36
        Caption = 
          'First-order rate of adsorbate mass production in the immobile ph' +
          'ase (PRODS1)'
        WordWrap = True
      end
      object rdeZeroFluidProd: TRbwDataEntry
        Left = 6
        Top = 11
        Width = 94
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeZeroImmobProd: TRbwDataEntry
        Left = 6
        Top = 38
        Width = 94
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFirstFluidProd: TRbwDataEntry
        Left = 6
        Top = 66
        Width = 94
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFirstImmobProd: TRbwDataEntry
        Left = 6
        Top = 101
        Width = 94
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
  end
  object jvpltvNavigation: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 169
    Height = 393
    PageDefault = 0
    PageList = jplMain
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    Items.NodeData = {
      0303000000380000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010D43006F006E00660069006700750072006100740069006F006E
      00280000000000000000000000FFFFFFFFFFFFFFFF0000000001000000000000
      0001055400690074006C006500400000000000000000000000FFFFFFFFFFFFFF
      FF000000000200000000000000011149006E0069007400690061006C0043006F
      006E0064006900740069006F006E007300}
    Items.Links = {03000000000000000100000002000000}
  end
end
