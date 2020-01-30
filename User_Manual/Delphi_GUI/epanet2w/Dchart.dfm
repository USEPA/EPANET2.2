object ChartOptionsDlg: TChartOptionsDlg
  Left = 410
  Top = 182
  BorderStyle = bsDialog
  BorderWidth = 1
  Caption = 'Graph Options'
  ClientHeight = 407
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object DefaultBox: TCheckBox
    Left = 20
    Top = 331
    Width = 213
    Height = 21
    Caption = 'Make these the default options '
    TabOrder = 1
  end
  object OkBtn: TButton
    Left = 54
    Top = 367
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 147
    Top = 367
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 239
    Top = 367
    Width = 74
    Height = 25
    Caption = 'Help'
    TabOrder = 4
    OnClick = HelpBtnClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 4
    Width = 351
    Height = 312
    ActivePage = GeneralPage
    TabOrder = 0
    OnChange = PageControl1Change
    object GeneralPage: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 61
        Height = 15
        Caption = 'Panel Color'
      end
      object Label2: TLabel
        Left = 16
        Top = 75
        Width = 96
        Height = 15
        Caption = 'Background Color'
      end
      object Label4: TLabel
        Left = 16
        Top = 160
        Width = 90
        Height = 15
        Caption = '3D Effect Percent'
      end
      object Label5: TLabel
        Left = 16
        Top = 203
        Width = 53
        Height = 15
        Caption = 'Main Title'
      end
      object PanelColorBox: TColorBox
        Left = 163
        Top = 21
        Width = 147
        Height = 26
        DefaultColorColor = clBtnFace
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
        Ctl3D = True
        ItemHeight = 20
        ParentCtl3D = False
        TabOrder = 0
      end
      object BackColorBox1: TColorBox
        Left = 163
        Top = 72
        Width = 147
        Height = 26
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
        ItemHeight = 20
        TabOrder = 1
      end
      object View3DBox: TCheckBox
        Left = 16
        Top = 117
        Width = 161
        Height = 21
        Alignment = taLeftJustify
        Caption = 'View in 3-D'
        TabOrder = 2
      end
      object GraphTitleBox: TEdit
        Left = 16
        Top = 223
        Width = 313
        Height = 23
        TabOrder = 5
      end
      object Pct3DUpDown: TUpDown
        Left = 212
        Top = 157
        Width = 16
        Height = 23
        Associate = Pct3DEdit
        Min = 1
        Increment = 5
        Position = 15
        TabOrder = 4
        TabStop = True
      end
      object Pct3DEdit: TEdit
        Left = 163
        Top = 157
        Width = 49
        Height = 23
        NumbersOnly = True
        TabOrder = 3
        Text = '15'
      end
      object GraphTitleFontLabel: TLinkLabel
        Left = 248
        Top = 203
        Width = 81
        Height = 19
        Caption = '<a>Change Font...</a>'
        TabOrder = 6
        TabStop = True
        OnLinkClick = GraphTitleFontLabelLinkClick
      end
    end
    object XaxisPage: TTabSheet
      Caption = 'Horizontal Axis'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object XminLabel: TLabel
        Left = 16
        Top = 28
        Width = 53
        Height = 15
        Caption = 'Minimum'
      end
      object XmaxLabel: TLabel
        Left = 16
        Top = 68
        Width = 54
        Height = 15
        Caption = 'Maximum'
      end
      object XIncrementLabel: TLabel
        Left = 16
        Top = 108
        Width = 54
        Height = 15
        Caption = 'Increment'
      end
      object Label11: TLabel
        Left = 16
        Top = 186
        Width = 47
        Height = 15
        Caption = 'Axis Title'
      end
      object XDataMinLabel: TLabel
        Left = 236
        Top = 29
        Width = 80
        Height = 15
        Caption = 'XDataMinLabel'
      end
      object XDataMaxLabel: TLabel
        Left = 236
        Top = 68
        Width = 81
        Height = 15
        Caption = 'XDataMaxLabel'
      end
      object Xmin: TEdit
        Left = 126
        Top = 25
        Width = 86
        Height = 23
        TabOrder = 0
      end
      object Xmax: TEdit
        Left = 126
        Top = 65
        Width = 86
        Height = 23
        TabOrder = 1
      end
      object Xinc: TEdit
        Left = 126
        Top = 105
        Width = 86
        Height = 23
        TabOrder = 2
      end
      object Xtitle: TEdit
        Left = 16
        Top = 205
        Width = 313
        Height = 23
        TabOrder = 4
      end
      object Xgrid: TCheckBox
        Left = 126
        Top = 146
        Width = 87
        Height = 21
        Caption = 'Grid Lines'
        TabOrder = 3
      end
      object XaxisFontLabel: TLinkLabel
        Left = 236
        Top = 186
        Width = 78
        Height = 17
        Caption = '<a>Change Font...</a>'
        TabOrder = 5
        TabStop = True
        OnLinkClick = XaxisFontLabelLinkClick
      end
      object XautoScale: TLinkLabel
        Left = 236
        Top = 109
        Width = 56
        Height = 17
        Caption = '<a>Auto-Scale</a>'
        TabOrder = 6
        OnLinkClick = XautoScaleLinkClick
      end
    end
    object YaxisPage: TTabSheet
      Caption = 'Vertical Axis'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label6: TLabel
        Left = 16
        Top = 28
        Width = 53
        Height = 15
        Caption = 'Minimum'
      end
      object Label7: TLabel
        Left = 16
        Top = 68
        Width = 54
        Height = 15
        Caption = 'Maximum'
      end
      object Label9: TLabel
        Left = 16
        Top = 108
        Width = 54
        Height = 15
        Caption = 'Increment'
      end
      object YDataMaxLabel: TLabel
        Left = 236
        Top = 68
        Width = 81
        Height = 15
        Caption = 'YDataMaxLabel'
      end
      object YDataMinLabel: TLabel
        Left = 236
        Top = 29
        Width = 80
        Height = 15
        Caption = 'YDataMinLabel'
      end
      object Label10: TLabel
        Left = 16
        Top = 186
        Width = 47
        Height = 15
        Caption = 'Axis Title'
      end
      object Ymin: TEdit
        Left = 126
        Top = 25
        Width = 86
        Height = 23
        TabOrder = 0
      end
      object Ymax: TEdit
        Left = 126
        Top = 65
        Width = 86
        Height = 23
        TabOrder = 1
      end
      object Yinc: TEdit
        Left = 126
        Top = 105
        Width = 86
        Height = 23
        TabOrder = 2
      end
      object Ygrid: TCheckBox
        Left = 126
        Top = 146
        Width = 87
        Height = 21
        Caption = 'Grid Lines'
        TabOrder = 3
      end
      object YaxisFontLabel: TLinkLabel
        Left = 236
        Top = 186
        Width = 78
        Height = 17
        Caption = '<a>Change Font...</a>'
        TabOrder = 5
        TabStop = True
        OnLinkClick = YaxisFontLabelLinkClick
      end
      object Ytitle: TEdit
        Left = 16
        Top = 205
        Width = 313
        Height = 23
        TabOrder = 4
      end
      object YautoScale: TLinkLabel
        Left = 236
        Top = 109
        Width = 56
        Height = 17
        Caption = '<a>Auto-Scale</a>'
        TabOrder = 6
        OnLinkClick = YautoScaleLinkClick
      end
    end
    object LegendPage: TTabSheet
      Caption = 'Legend'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label18: TLabel
        Left = 16
        Top = 28
        Width = 43
        Height = 15
        Caption = 'Position'
      end
      object Label19: TLabel
        Left = 16
        Top = 68
        Width = 29
        Height = 15
        Caption = 'Color'
      end
      object Label3: TLabel
        Left = 16
        Top = 115
        Width = 176
        Height = 15
        Caption = 'Symbol Width (% of Label Width)'
      end
      object LegendFrameBox: TCheckBox
        Left = 16
        Top = 152
        Width = 109
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Framed'
        TabOrder = 4
      end
      object LegendVisibleBox: TCheckBox
        Left = 16
        Top = 187
        Width = 109
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Visible'
        TabOrder = 6
      end
      object LegendPosBox: TComboBox
        Left = 124
        Top = 25
        Width = 146
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 0
      end
      object LegendColorBox: TColorBox
        Left = 124
        Top = 65
        Width = 146
        Height = 22
        DefaultColorColor = clBtnFace
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbPrettyNames]
        ItemHeight = 20
        TabOrder = 1
      end
      object LegendCheckBox: TCheckBox
        Left = 16
        Top = 225
        Width = 109
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Check Boxes'
        TabOrder = 8
      end
      object LegendShadowBox: TCheckBox
        Left = 149
        Top = 152
        Width = 121
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Shadowed'
        TabOrder = 5
      end
      object LegendTransparentBox: TCheckBox
        Left = 149
        Top = 187
        Width = 121
        Height = 21
        Alignment = taLeftJustify
        Caption = 'Transparent'
        TabOrder = 7
      end
      object LegendWidthUpDown: TUpDown
        Left = 257
        Top = 112
        Width = 16
        Height = 23
        Associate = LegendWidthEdit
        Min = 5
        Increment = 5
        Position = 5
        TabOrder = 3
      end
      object LegendWidthEdit: TEdit
        Left = 211
        Top = 112
        Width = 46
        Height = 23
        Ctl3D = True
        NumbersOnly = True
        ParentCtl3D = False
        TabOrder = 2
        Text = '5'
      end
    end
    object StylesPage: TTabSheet
      Caption = 'Series'
      ImageIndex = 4
      OnExit = StylesPageExit
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label21: TLabel
        Left = 8
        Top = 19
        Width = 30
        Height = 15
        Caption = 'Series'
      end
      object Label22: TLabel
        Left = 8
        Top = 62
        Width = 65
        Height = 15
        Caption = 'Legend Title'
      end
      object SeriesComboBox: TComboBox
        Left = 96
        Top = 16
        Width = 91
        Height = 22
        Style = csOwnerDrawFixed
        TabOrder = 0
        OnClick = SeriesComboBoxClick
      end
      object SeriesTitle: TEdit
        Left = 96
        Top = 56
        Width = 227
        Height = 23
        TabOrder = 1
      end
      object Panel6: TPanel
        Left = 8
        Top = 96
        Width = 319
        Height = 178
        BevelOuter = bvNone
        Caption = 'Panel1'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 3
        object PageControl2: TPageControl
          Left = 0
          Top = 0
          Width = 319
          Height = 178
          ActivePage = LineOptionsSheet
          Align = alClient
          TabOrder = 0
          object LineOptionsSheet: TTabSheet
            Caption = 'Lines'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label23: TLabel
              Left = 55
              Top = 14
              Width = 25
              Height = 15
              Caption = 'Style'
            end
            object Label24: TLabel
              Left = 55
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object Label25: TLabel
              Left = 55
              Top = 94
              Width = 20
              Height = 15
              Caption = 'Size'
            end
            object LineStyleBox: TComboBox
              Left = 111
              Top = 11
              Width = 146
              Height = 22
              Style = csOwnerDrawFixed
              TabOrder = 0
            end
            object LineColorBox: TColorBox
              Left = 111
              Top = 48
              Width = 146
              Height = 22
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object LineVisibleBox: TCheckBox
              Left = 55
              Top = 125
              Width = 69
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Visible'
              TabOrder = 4
            end
            object LineSizeEdit: TEdit
              Left = 111
              Top = 91
              Width = 32
              Height = 23
              Ctl3D = True
              NumbersOnly = True
              ParentCtl3D = False
              TabOrder = 2
              Text = '1'
            end
            object LineSizeUpDown: TUpDown
              Left = 143
              Top = 91
              Width = 16
              Height = 21
              Associate = LineSizeEdit
              Min = 1
              Max = 10
              Position = 1
              TabOrder = 3
            end
          end
          object MarkOptionsSheet: TTabSheet
            Caption = 'Markers'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label26: TLabel
              Left = 55
              Top = 14
              Width = 25
              Height = 15
              Caption = 'Style'
            end
            object Label27: TLabel
              Left = 55
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object Label28: TLabel
              Left = 55
              Top = 94
              Width = 20
              Height = 15
              Caption = 'Size'
            end
            object MarkVisibleBox: TCheckBox
              Left = 55
              Top = 125
              Width = 69
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Visible'
              TabOrder = 4
            end
            object MarkStyleBox: TComboBox
              Left = 111
              Top = 11
              Width = 146
              Height = 22
              Style = csOwnerDrawFixed
              TabOrder = 0
            end
            object MarkColorBox: TColorBox
              Left = 111
              Top = 48
              Width = 146
              Height = 22
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object MarkSizeEdit: TEdit
              Left = 111
              Top = 91
              Width = 32
              Height = 23
              Ctl3D = True
              NumbersOnly = True
              ParentCtl3D = False
              TabOrder = 2
              Text = '1'
            end
            object MarkSizeUpDown: TUpDown
              Left = 143
              Top = 91
              Width = 16
              Height = 21
              Associate = MarkSizeEdit
              Min = 1
              Max = 10
              Position = 1
              TabOrder = 3
            end
          end
          object AreaOptionsSheet: TTabSheet
            Caption = 'Patterns'
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label29: TLabel
              Left = 47
              Top = 14
              Width = 45
              Height = 15
              AutoSize = False
              Caption = 'Style'
            end
            object Label30: TLabel
              Left = 47
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object Label31: TLabel
              Left = 47
              Top = 90
              Width = 57
              Height = 15
              AutoSize = False
              Caption = 'Stacking'
            end
            object AreaFillStyleBox: TComboBox
              Left = 118
              Top = 11
              Width = 147
              Height = 22
              Style = csOwnerDrawFixed
              TabOrder = 0
            end
            object AreaColorBox: TColorBox
              Left = 118
              Top = 48
              Width = 147
              Height = 22
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object StackStyleBox: TComboBox
              Left = 118
              Top = 87
              Width = 147
              Height = 22
              Style = csOwnerDrawFixed
              TabOrder = 2
            end
          end
          object PieOptionsSheet: TTabSheet
            Caption = 'Pie Options'
            ImageIndex = 3
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label32: TLabel
              Left = 80
              Top = 96
              Width = 79
              Height = 15
              Caption = 'Rotation Angle'
            end
            object PieCircledBox: TCheckBox
              Left = 80
              Top = 24
              Width = 114
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Circular'
              TabOrder = 0
            end
            object PiePatternBox: TCheckBox
              Left = 80
              Top = 56
              Width = 114
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Use Patterns'
              TabOrder = 1
            end
            object PieRotateEdit: TEdit
              Left = 185
              Top = 91
              Width = 32
              Height = 23
              Ctl3D = True
              NumbersOnly = True
              ParentCtl3D = False
              TabOrder = 2
              Text = '0'
            end
            object PieRotateUpDown: TUpDown
              Left = 217
              Top = 91
              Width = 16
              Height = 21
              Associate = PieRotateEdit
              Max = 360
              Increment = 10
              TabOrder = 3
            end
          end
          object LabelsOptionsSheet: TTabSheet
            Caption = 'Labels'
            ImageIndex = 4
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label33: TLabel
              Left = 43
              Top = 14
              Width = 25
              Height = 15
              Caption = 'Style'
            end
            object Label34: TLabel
              Left = 43
              Top = 51
              Width = 29
              Height = 15
              Caption = 'Color'
            end
            object LabelsStyleBox: TComboBox
              Left = 122
              Top = 11
              Width = 147
              Height = 22
              Style = csOwnerDrawFixed
              TabOrder = 0
            end
            object LabelsBackColorBox: TColorBox
              Left = 122
              Top = 48
              Width = 147
              Height = 22
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
              ItemHeight = 20
              TabOrder = 1
            end
            object LabelsTransparentBox: TCheckBox
              Left = 43
              Top = 79
              Width = 96
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Transparent'
              TabOrder = 2
            end
            object LabelsArrowsBox: TCheckBox
              Left = 43
              Top = 104
              Width = 96
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Show Arrows'
              TabOrder = 3
            end
            object LabelsVisibleBox: TCheckBox
              Left = 43
              Top = 128
              Width = 96
              Height = 21
              Alignment = taLeftJustify
              Caption = 'Visible'
              TabOrder = 4
            end
          end
        end
      end
      object LegendFontLabel: TLinkLabel
        Left = 242
        Top = 37
        Width = 78
        Height = 17
        Caption = '<a>Change Font...</a>'
        TabOrder = 2
        TabStop = True
        OnLinkClick = LegendFontLabelLinkClick
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 440
    Top = 432
  end
end
