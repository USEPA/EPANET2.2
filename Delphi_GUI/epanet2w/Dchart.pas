unit Dchart;
{
   Unit:    Dchart.pas
   Project: EPANET4W
   Author:  L. Rossman
   Version: 2.2
   Date:    6/24/19

   This is dialog form used to set display options for a
   TChart component.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, Spin, ExtCtrls, ComCtrls, Math, StrUtils,
  VCLTee.Chart, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeCanvas,
  Vcl.Grids, System.UITypes;

const
  LineStyleText: array[0..4] of PChar =
    ('Solid','Dash','Dot','DashDot','DashDotDot');
  LegendPosText: array[0..3] of PChar =
    ('Left','Right','Top','Bottom');
  MarkStyleText: array[0..7] of PChar =
    ('Rectangle','Circle','Up Triangle','Down Triangle','Cross',
     'Diagonal Cross','Star','Diamond');
  FillStyleText: array[0..7] of PChar =
    ('Solid','Clear','Horizontal','Vertical','Foward Diagonal','Back Diagonal',
     'Cross','Diagonal Cross');
  StackStyleText: array[0..3] of PChar =
    ('None','Side','Stacked','Stacked 100%');
  LabelStyleText: array[0..8] of PChar =
    ('Value','Percent','Label','Label & %','Label & Value','Legend','% Total',
     'Label & % Total','X Value');

type
//Graph series types
  TSeriesType = (stLine, stFastLine, stPoint, stBar, stHorizBar, stArea, stPie);

//Axis types
  TAxisType = (atX, atY);

//Axis information
  TAxisInfo = record
    DataMin: String;
    DataMax: String;
    AxisMin: String;
    AxisMax: String;
    AxisInc: String;
  end;

//Graph series options
  TSeriesOptions = class(TObject)
    Constructor Create;
    public
      SeriesType      : TSeriesType;
      LineVisible     : Boolean;
      LineStyle       : Integer;
      LineColor       : TColor;
      LineWidth       : Integer;
      PointVisible    : Boolean;
      PointStyle      : Integer;
      PointColor      : TColor;
      PointSize       : Integer;
      AreaFillStyle   : Integer;
      AreaFillColor   : TColor;
      AreaStacking    : Integer;
      PieCircled      : Boolean;
      PieUsePatterns  : Boolean;
      PieRotation     : Integer;
      LabelsVisible   : Boolean;
      LabelsTransparent: Boolean;
      LabelsArrows    : Boolean;
      LabelsBackColor : TColor;
      LabelsStyle     : Integer;
    end;

  TChartOptionsDlg = class(TForm)
    DefaultBox: TCheckBox;
    FontDialog1: TFontDialog;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    PageControl1: TPageControl;
    GeneralPage: TTabSheet;
    XaxisPage: TTabSheet;
    LegendPage: TTabSheet;
    StylesPage: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PanelColorBox: TColorBox;
    BackColorBox1: TColorBox;
    View3DBox: TCheckBox;
    GraphTitleBox: TEdit;
    Pct3DUpDown: TUpDown;
    Pct3DEdit: TEdit;
    XminLabel: TLabel;
    XmaxLabel: TLabel;
    XIncrementLabel: TLabel;
    Label11: TLabel;
    XDataMinLabel: TLabel;
    XDataMaxLabel: TLabel;
    Xmin: TEdit;
    Xmax: TEdit;
    Xinc: TEdit;
    Xtitle: TEdit;
    Xgrid: TCheckBox;
    Label18: TLabel;
    Label19: TLabel;
    LegendFrameBox: TCheckBox;
    LegendVisibleBox: TCheckBox;
    LegendPosBox: TComboBox;
    LegendColorBox: TColorBox;
    LegendCheckBox: TCheckBox;
    LegendShadowBox: TCheckBox;
    Label21: TLabel;
    Label22: TLabel;
    SeriesComboBox: TComboBox;
    SeriesTitle: TEdit;
    Panel6: TPanel;
    PageControl2: TPageControl;
    LineOptionsSheet: TTabSheet;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    LineStyleBox: TComboBox;
    LineColorBox: TColorBox;
    LineVisibleBox: TCheckBox;
    LineSizeEdit: TEdit;
    LineSizeUpDown: TUpDown;
    MarkOptionsSheet: TTabSheet;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    MarkVisibleBox: TCheckBox;
    MarkStyleBox: TComboBox;
    MarkColorBox: TColorBox;
    MarkSizeEdit: TEdit;
    MarkSizeUpDown: TUpDown;
    AreaOptionsSheet: TTabSheet;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    AreaFillStyleBox: TComboBox;
    AreaColorBox: TColorBox;
    StackStyleBox: TComboBox;
    PieOptionsSheet: TTabSheet;
    Label32: TLabel;
    PieCircledBox: TCheckBox;
    PiePatternBox: TCheckBox;
    PieRotateEdit: TEdit;
    PieRotateUpDown: TUpDown;
    LabelsOptionsSheet: TTabSheet;
    Label33: TLabel;
    Label34: TLabel;
    LabelsStyleBox: TComboBox;
    LabelsBackColorBox: TColorBox;
    LabelsTransparentBox: TCheckBox;
    LabelsArrowsBox: TCheckBox;
    LabelsVisibleBox: TCheckBox;
    LegendTransparentBox: TCheckBox;
    Label3: TLabel;
    GraphTitleFontLabel: TLinkLabel;
    XaxisFontLabel: TLinkLabel;
    LegendFontLabel: TLinkLabel;
    LegendWidthUpDown: TUpDown;
    LegendWidthEdit: TEdit;
    YaxisPage: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Ymin: TEdit;
    Ymax: TEdit;
    Yinc: TEdit;
    YDataMaxLabel: TLabel;
    YDataMinLabel: TLabel;
    Ygrid: TCheckBox;
    Label10: TLabel;
    YaxisFontLabel: TLinkLabel;
    Ytitle: TEdit;
    XautoScale: TLinkLabel;
    YautoScale: TLinkLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SeriesComboBoxClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure StylesPageExit(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure GraphTitleFontLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure XaxisFontLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure LegendFontLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure YaxisFontLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure XautoScaleLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure YautoScaleLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }
    SeriesIndex: Integer;
    theSeries: TStringlist;
    IsPieChart: Boolean;
    IsDateTime: Boolean;
    procedure SaveSeriesOptions(const Index: Integer);
    procedure SetSeriesOptions(const Index: Integer);
    procedure SetAxisScaling(theAxis: TChartAxis; const Smin, Smax, Sinc: String);
    function  GetAxisValue(S: string): Double;
  public
    { Public declarations }
    UseDefaultPanelColor: Boolean;
    procedure LoadOptions(theChart: TChart);
    procedure UnloadOptions(theChart: TChart);
  end;

procedure Execute(theForm: TForm;
                  theChart: TChart;
                  var startPage: Integer;
                  var default: Boolean);

implementation

{$R *.dfm}

uses Uglobals, Uutils;

{------------------------------------------------------------------------------
  Procedure called externally to launch the dialog.
  theForm: the form with the chart on it
  theChart: the chart whose options are being set
  startPage: on input the dialog page to display first,
             on output the page currently displayed
  default: on input determines if DefaultBox checkbox is visible,
           on output contains state of DefaultBox checkbox.
------------------------------------------------------------------------------}
procedure Execute(theForm: TForm;
                  theChart: TChart;
                  var startPage: Integer;
                  var default: Boolean);
var
  ChartOptionsDlg: TChartOptionsDlg;
begin
  ChartOptionsDlg := TChartOptionsDlg.Create(theForm);
  with ChartOptionsDlg do
  try
    PageControl1.ActivePageIndex := startPage;
    DefaultBox.Visible := default;
    LoadOptions(theChart);
    if ShowModal = mrOK then
    begin
      startPage := PageControl1.ActivePageIndex;
      UnloadOptions(theChart);
      default := DefaultBox.Checked;
    end;
  finally
    ChartOptionsDlg.Free;
  end;

end;

{Constructor for TSeriesOptions}
Constructor TSeriesOptions.Create;
begin
  Inherited Create;
end;

{Dialog's Constructor}
procedure TChartOptionsDlg.FormCreate(Sender: TObject);
var
  i: Integer;
begin
//  Uglobals.SetFont(self);
{ Load option choices into comboboxes }
  for i := 0 to High(LineStyleText) do
    LineStyleBox.Items.Add(LineStyleText[i]);
  for i := 0 to High(LegendPosText) do
    LegendPosBox.Items.Add(LegendPosText[i]);
  for i := 0 to High(FillStyleText) do
    AreaFillStyleBox.Items.Add(FillStyleText[i]);
  for i := 0 to High(MarkStyleText) do
    MarkStyleBox.Items.Add(MarkStyleText[i]);
  for i := 0 to High(StackStyleText) do
    StackStyleBox.Items.Add(StackStyleText[i]);
  for i := 0 to High(LabelStyleText) do
    LabelsStyleBox.Items.Add(LabelStyleText[i]);
  UseDefaultPanelColor := False;
{
  PanelColorBox.DefaultColorColor :=
    Integer(StyleServices.GetStyleColor(scPanel));
  LegendColorBox.DefaultColorColor := PanelColorBox.DefaultColorColor;
}
{ Create a stringlist to hold data series options }
  theSeries := TStringlist.Create;
  PageControl1.ActivePage := GeneralPage;

end;

{Dialog's OnDestroy handler.}
procedure TChartOptionsDlg.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  with theSeries do
  begin
    for i := 0 to Count - 1 do
      // Free the TSeriesOptions objects created in LoadOptions()
      Objects[i].Free;
    Free;
  end;
end;


{Change the font used for the chart's title.}
procedure TChartOptionsDlg.GraphTitleFontLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  with FontDialog1 do
  begin
    Font.Assign(GraphTitleBox.Font);
    if Execute then GraphTitleBox.Font.Assign(Font);
  end;
end;

procedure TChartOptionsDlg.XautoScaleLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
var
  XminValue : Double;
  XmaxValue : Double;
  XincValue : Double;
begin
  XminValue := GetAxisValue(XdataMinLabel.Caption);
  XmaxValue := GetAxisValue(XdataMaxLabel.Caption);
  Uutils.AutoScale(XminValue, XmaxValue, XincValue);
  Xmin.Text := Format('%f', [XminValue]);
  Xmax.Text := Format('%f', [XmaxValue]);
  Xinc.Text := Format('%f', [XincValue]);
end;

procedure TChartOptionsDlg.YautoScaleLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
var
  YminValue : Double;
  YmaxValue : Double;
  YincValue : Double;
begin
  YminValue := GetAxisValue(YdataMinLabel.Caption);
  YmaxValue := getAxisValue(YdataMaxLabel.Caption);
  Uutils.AutoScale(YminValue, YmaxValue, YincValue);
  Ymin.Text := Format('%f', [YminValue]);
  Ymax.Text := Format('%f', [YmaxValue]);
  Yinc.Text := Format('%f', [YincValue]);
end;

function  TChartOptionsDlg.GetAxisValue(S: string): Double;
begin
  S := StrUtils.MidStr(S, 2, S.Length-2);
  Result := StrToFloat(S);
end;

{Change the font used for a chart's axis labels.}
procedure TChartOptionsDlg.XaxisFontLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  with FontDialog1 do
  begin
    Font.Assign(Xtitle.Font);
    if Execute then
    begin
      Xtitle.Font.Assign(Font);
      Ytitle.Font.Assign(Font);
    end;
  end;
end;

procedure TChartOptionsDlg.YaxisFontLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  with FontDialog1 do
  begin
    Font.Assign(Ytitle.Font);
    if Execute then
    begin
      Xtitle.Font.Assign(Font);
      Ytitle.Font.Assign(Font);
    end;
  end;
end;


{Change the font used for the chart's legend.}
procedure TChartOptionsDlg.LegendFontLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  with FontDialog1 do
  begin
    Font.Assign(SeriesTitle.Font);
    if Execute then SeriesTitle.Font.Assign(Font);
  end;
end;

{OnClick handler for user's choice of a chart series to edit.}
procedure TChartOptionsDlg.SeriesComboBoxClick(Sender: TObject);
begin
  if (Sender is TComboBox) then
    with Sender as TComboBox do
    begin
      SaveSeriesOptions(SeriesIndex); {Store options for current series}
      SeriesIndex := ItemIndex;       {Update value of current series}
      SetSeriesOptions(SeriesIndex);  {Load series options into form}
    end;
end;

{OnExit handler for the dialog's Styles page.}
procedure TChartOptionsDlg.StylesPageExit(Sender: TObject);
begin
  SaveSeriesOptions(SeriesIndex);
end;


{Transfer options from the chart to the dialog.}
procedure TChartOptionsDlg.LoadOptions(theChart: TChart);

var
  i: Integer;
  s: String;
  SeriesOptions: TSeriesOptions;
begin
  IsPieChart := False;
  with theChart do
  begin

  { General Page }
    View3DBox.Checked := View3D;
    Pct3DUpDown.Position := Chart3DPercent;
    with PanelColorBox do
    begin
      if theChart.Color = DefaultColorColor then
        ItemIndex := Items.IndexOf('Default')
      else Selected := theChart.Color;
    end;
    BackColorBox1.Selected := BackColor; //BackWall.Gradient.StartColor;
    GraphTitleBox.Font.Assign(Title.Font);
    if (Title.Text.Count > 0) then
      GraphTitleBox.Text := Title.Text[0];

  { Series Page - do before Axis pages to get value for IsPieChart }
  { Save current line series options }
    IsDateTime := False;
    SeriesTitle.Font.Assign(Legend.Font);
    for i := 0 to SeriesCount-1 do
    begin
      if Series[i].Active then
      begin
        SeriesOptions := TSeriesOptions.Create;
        s := 'Series' + IntToStr(i+1);
        SeriesComboBox.Items.Add(s);
        if Series[i].XValues.DateTime then IsDateTime := True;

        with Series[i], SeriesOptions do
        begin
          LabelsVisible := Marks.Visible;
          LabelsArrows := Marks.Arrow.Visible;
          LabelsTransparent := Marks.Transparent;
          LabelsBackColor := Marks.BackColor;
          LabelsStyle := Ord(Marks.Style);
        end;
        if Series[i] is TLineSeries then
          with Series[i] as TLineSeries, SeriesOptions do
          begin
            SeriesType := stLine;
            LineVisible := LinePen.Visible;
            LineStyle := Ord(LinePen.Style);
            LineColor := SeriesColor;
            LineWidth := LinePen.Width;
            AreaFillStyle := Ord(LineBrush);
            PointVisible := Pointer.Visible;
            PointStyle := Ord(Pointer.Style);
            PointColor := ValueColor[0];
            PointSize := Pointer.VertSize;
          end
        else if Series[i] is TFastLineSeries then
          with Series[i] as TFastLineSeries, SeriesOptions do
          begin
            SeriesType := stFastLine;
            LineVisible := LinePen.Visible;
            LineStyle := Ord(LinePen.Style);
            LineColor := SeriesColor;
            LineWidth := LinePen.Width;
          end
        else if Series[i] is TPointSeries then
          with Series[i] as TPointSeries, SeriesOptions do
          begin
            SeriesType := stPoint;
            PointVisible := Pointer.Visible;
            PointStyle := Ord(Pointer.Style);
            PointColor := SeriesColor;
            PointSize := Pointer.HorizSize;
          end
        else if Series[i] is TBarSeries then
          with Series[i] as TBarSeries, SeriesOptions do
          begin
            SeriesType := stBar;
            AreaFillStyle := Ord(BarBrush.Style);
            if BarBrush.Style = bsSolid then
            begin
              AreaFillColor := SeriesColor;
              LineColor := BarBrush.Color;
            end
            else
            begin
              LineColor := SeriesColor;
              AreaFillColor := BarBrush.Color;
            end;
            AreaStacking := Ord(MultiBar);
          end
        else if Series[i] is THorizBarSeries then
          with Series[i] as THorizBarSeries, SeriesOptions do
          begin
            SeriesType := stHorizBar;
            AreaFillStyle := Ord(BarBrush.Style);
            if BarBrush.Style = bsSolid then
            begin
              AreaFillColor := SeriesColor;
              LineColor := BarBrush.Color;
            end
            else
            begin
              LineColor := SeriesColor;
              AreaFillColor := BarBrush.Color;
            end;
            AreaStacking := Ord(MultiBar);
          end
        else if Series[i] is TAreaSeries then
          with Series[i] as TAreaSeries, SeriesOptions do
          begin
            SeriesType := stArea;
            LineVisible := AreaLinesPen.Visible;
            LineStyle := Ord(AreaLinesPen.Style);
            LineColor := AreaLinesPen.Color;
            LineWidth := AreaLinesPen.Width;
            AreaFillColor := SeriesColor;
            AreaFillStyle := Ord(AreaBrush);
          end
        else if Series[i] is TPieSeries then
          with Series[i] as TPieSeries, SeriesOptions do
          begin
            SeriesType := stPie;
            IsPieChart := True;
            LineVisible := PiePen.Visible;
            LineStyle := Ord(PiePen.Style);
            LineColor := PiePen.Color;  //SeriesColor;
            LineWidth := PiePen.Width;
            PieCircled := Circled;
            PieUsePatterns := UsePatterns;
            PieRotation := RotationAngle;
          end;
        if Length(Series[i].Title) > 0 then s := Series[i].Title;
        theSeries.AddObject(s,SeriesOptions);
      end;
    end;

  { X Axis }
    if IsPieChart then XaxisPage.TabVisible := False
    else
    begin
      XdataMinLabel.Caption := Format('(%f)',[MinXValue(BottomAxis)]);
      XdataMaxLabel.Caption := Format('(%f)',[MaxXValue(BottomAxis)]);
      with BottomAxis do
      begin
///////////////        Xauto.Checked := Automatic;
//////////////        if not Automatic then
/////////////        begin
          Xmin.Text := Format('%f',[Minimum]);
          Xmax.Text := Format('%f',[Maximum]);
          Xinc.Text := Format('%f',[Increment]);
/////////////        end;
        Xgrid.Checked := Grid.Visible;
        Xtitle.Font.Assign(Title.Font);
        Xtitle.Text := Title.Caption;
      end;
    end;

  { Y Axis }
    if IsPieChart then YaxisPage.TabVisible := False
    else
    begin
      YdataMinLabel.Caption := Format('(%f)',[MinYValue(LeftAxis)]);
      YdataMaxLabel.Caption := Format('(%f)',[MaxYValue(LeftAxis)]);
      with LeftAxis do
      begin
        Ymin.Text := Format('%f',[Minimum]);
        Ymax.Text := Format('%f',[Maximum]);
        Yinc.Text := Format('%f',[Increment]);
        Ygrid.Checked := Grid.Visible;
        Ytitle.Font.Assign(Title.Font);
        Ytitle.Text := Title.Caption;
      end;
    end;

  { Legend Page }
    LegendPosBox.ItemIndex := Ord(Legend.Alignment);
    LegendColorBox.Selected := Legend.Color;
    LegendCheckBox.Checked := Legend.CheckBoxes;
    LegendShadowBox.Checked := Legend.Shadow.Visible;
    LegendFrameBox.Checked := Legend.Frame.Visible;
    LegendTransparentBox.Checked := Legend.Transparent;
    LegendVisibleBox.Checked := Legend.Visible;
    LegendWidthUpDown.Position := Legend.ColorWidth;
  end;

//Set current series to first series & update dialog entries
  if theChart.SeriesCount > 0 then
  begin
    SeriesIndex := 0;
    SeriesComboBox.ItemIndex := 0;
    SetSeriesOptions(0);
  end
  else StylesPage.TabVisible := False;
end;

{OnChange handler for the dialog's page control.}
procedure TChartOptionsDlg.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = StylesPage
    then SetSeriesOptions(SeriesIndex);
end;

{Transfer otions from the dialog back to the chart.}
procedure TChartOptionsDlg.UnloadOptions(theChart: TChart);
var
  i,j: Integer;
  s  : String;
  SeriesOptions: TSeriesOptions;
begin
  with theChart do
  begin

  { General Page }
    AutoRepaint := False;
    View3D := View3DBox.Checked;
    Chart3DPercent := Pct3DUpDown.Position;
    Backwall.Color := BackColorBox1.Selected;
    with PanelColorBox do
    begin
      if Items[ItemIndex] = 'Default' then
      begin
        theChart.Color := DefaultColorColor;
        UseDefaultPanelColor := True;
      end
      else theChart.Color := Selected;
    end;
    s := GraphTitleBox.Text;
    Title.Text.Clear;
    if (Length(s) > 0) then Title.Text.Add(s);
    Title.Font.Assign(GraphTitleBox.Font);

  { X Axis Page }
    if not IsPieChart then with BottomAxis do
    begin
      SetAxisScaling(BottomAxis,Xmin.Text,Xmax.Text,Xinc.Text);
      Grid.Visible := Xgrid.Checked;
      Title.Caption := Xtitle.Text;
      Title.Font.Assign(Xtitle.Font);
      LabelsFont.Assign(Xtitle.Font);
    end;

  { Y Axis Page }
    if not IsPieChart then with LeftAxis do
    begin
      SetAxisScaling(LeftAxis,Ymin.Text,Ymax.Text,Yinc.Text);
      Grid.Visible := Ygrid.Checked;
      Title.Caption := Ytitle.Text;
      Title.Font.Assign(Ytitle.Font);
      LabelsFont.Assign(Ytitle.Font);
    end;

  { Legend Page }
    Legend.Alignment := TLegendAlignment(LegendPosBox.ItemIndex);
    with LegendColorBox do
    begin
      if Items[ItemIndex] = 'Default' then Legend.Color := DefaultColorColor
      else Legend.Color := LegendColorBox.Selected;
    end;
    Legend.CheckBoxes := LegendCheckBox.Checked;
    Legend.Shadow.Visible := LegendShadowBox.Checked;
    Legend.Frame.Visible := LegendFrameBox.Checked;
    Legend.Transparent := LegendTransparentBox.Checked;
    Legend.Visible := LegendVisibleBox.Checked;
    Legend.ColorWidth := LegendWidthUpDown.Position;
    Legend.Font.Assign(SeriesTitle.Font);

  { Series Page }
    if SeriesCount > 0 then
    begin
      SaveSeriesOptions(SeriesIndex);
      j := 0;
      for i := 0 to SeriesCount-1 do
      begin
        if Series[i].Active then
        begin
          SeriesOptions := TSeriesOptions(theSeries.Objects[j]);
          Series[i].Title := theSeries.Strings[j];

          with Series[i], SeriesOptions do
          begin
            Marks.Visible := LabelsVisible;
            Marks.Arrow.Visible := LabelsArrows;
            Marks.Transparent := LabelsTransparent;
            Marks.BackColor := LabelsBackColor;
            Marks.Style := TSeriesMarksStyle(LabelsStyle);
          end;

          if Series[i] is TLineSeries then
          with Series[i] as TLineSeries, SeriesOptions do
          begin
            LinePen.Visible := LineVisible;
            LinePen.Style := TPenStyle(LineStyle);
            SeriesColor := LineColor;
            LinePen.Width := LineWidth;
            Pointer.Visible := PointVisible;
            Pointer.Style := TSeriesPointerStyle(PointStyle);
            Pointer.Color := PointColor;
            Pointer.Size := PointSize;
            LineBrush := TBrushStyle(AreaFillStyle);
            if (not Pointer.Visible) and (not LinePen.Visible) then
              ShowinLegend := False
            else
              ShowinLegend := True;
          end;

          if Series[i] is TFastLineSeries then
          with Series[i] as TFastLineSeries, SeriesOptions do
          begin
            LinePen.Visible := LineVisible;
            LinePen.Style := TPenStyle(LineStyle);
            SeriesColor := LineColor;
            LinePen.Width := LineWidth;
            ShowinLegend := LinePen.Visible;
          end;

          if Series[i] is TPointSeries then
          with Series[i] as TPointSeries, SeriesOptions do
          begin
            Pointer.Visible := PointVisible;
            Pointer.Style := TSeriesPointerStyle(PointStyle);
            SeriesColor := PointColor;
            Pointer.Size := PointSize;
          end

          else if Series[i] is TBarSeries then
          with Series[i] as TBarSeries, SeriesOptions do
          begin
            BarBrush.Style := TBrushStyle(AreaFillStyle);
            if BarBrush.Style = bsSolid then
            begin
              SeriesColor := AreaFillColor;
              BarBrush.Color := AreaFillColor
            end
            else
            begin
              SeriesColor := LineColor;
              BarBrush.Color := AreaFillColor;
            end;
            MultiBar := TMultiBar(AreaStacking);
          end

          else if Series[i] is THorizBarSeries then
          with Series[i] as THorizBarSeries, SeriesOptions do
          begin
            BarBrush.Style := TBrushStyle(AreaFillStyle);
            if BarBrush.Style = bsSolid then
            begin
              SeriesColor := AreaFillColor;
              BarBrush.Color := AreaFillColor
            end
            else
            begin
              SeriesColor := LineColor;
              BarBrush.Color := AreaFillColor;
            end;
            MultiBar := TMultiBar(AreaStacking);
          end

          else if Series[i] is TAreaSeries then
          with Series[i] as TAreaSeries, SeriesOptions do
          begin
            AreaBrush := TBrushStyle(AreaFillStyle);
            SeriesColor := AreaFillColor;
            if AreaBrush = bsSolid then
              AreaColor := AreaFillColor
            else
              AreaColor := LineColor;
            AreaLinesPen.Visible := LineVisible;
            AreaLinesPen.Style := TPenStyle(LineStyle);
            AreaLinesPen.Color := LineColor;
            AreaLinesPen.Width := LineWidth;
            LinePen.Color := LineColor;
            LinePen.Width := LineWidth;
            Pointer.Visible := False;
          end

          else if Series[i] is TPieSeries then
          with Series[i] as TPieSeries, SeriesOptions do
          begin
            PiePen.Visible := LineVisible;
            PiePen.Style := TPenStyle(LineStyle);
            PiePen.Color := LineColor;
            PiePen.Width := LineWidth;
            Circled := PieCircled;
            UsePatterns := PieUsePatterns;
            RotationAngle := PieRotation;
          end;

          Inc(j);
        end;
      end;
    end;
    AutoRepaint := True;
    Refresh;
  end;
end;


procedure TChartOptionsDlg.SetAxisScaling(theAxis: TChartAxis;
            const Smin,Smax,Sinc: String);
{-------------------------------------------------
   Retrieves axis scaling options from form.
--------------------------------------------------}
{var
  code: Integer;
  v   : Double;}
var
  zMin, zMax, zInc: Double;
begin
  zMin := StrToFloatDef(Smin, 0);
  zMax := StrToFloatDef(Smax, 0);
  zInc := StrToFloatDef(Sinc, 0);
  if (zMin >= zMax) or (zInc < 0) or (zInc > zMax - zMin) then exit;
  with theAxis do
  begin
    Automatic := False;
    AutomaticMinimum := False;
    AutomaticMaximum := False;
    Minimum := zMin;
    Maximum := zMax;
    Increment := zInc;

{
    AutomaticMinimum := False;
    Val(Smin,v,code);
    if (code = 0) then
      Minimum := v
    else
      AutomaticMinimum := True;
    AutomaticMaximum := False;
    Val(Smax,v,code);
    if (code = 0) then
      Maximum := v
    else
      AutomaticMaximum := True;
    Val(Sinc,v,code);
    if (code = 0) then
      Increment := v
    else
      Increment := 0;
}
  end;
end;


{Transfer options for a data series to the dialog.}
procedure TChartOptionsDlg.SetSeriesOptions(const Index: Integer);
var
  SeriesOptions: TSeriesOptions;
begin
  SeriesTitle.Text := theSeries.Strings[Index];
  SeriesOptions := TSeriesOptions(theSeries.Objects[Index]);
  with SeriesOptions do
  begin
    LineStyleBox.ItemIndex := LineStyle;
    LineColorBox.Selected := LineColor;
    LineSizeUpDown.Position := LineWidth;
    LineVisibleBox.Checked := LineVisible;
    MarkStyleBox.ItemIndex := PointStyle;
    MarkColorBox.Selected := PointColor;
    MarkSizeUpDown.Position := PointSize;
    MarkVisibleBox.Checked := PointVisible;
    AreaFillStyleBox.ItemIndex := AreaFillStyle;
    AreaColorBox.Selected := AreaFillColor;
    StackStyleBox.ItemIndex := AreaStacking;
    PieCircledBox.Checked := PieCircled;
    PiePatternBox.Checked := PieUsePatterns;
    PieRotateUpDown.Position := PieRotation;
    LabelsVisibleBox.Checked := LabelsVisible;
    LabelsTransparentBox.Checked := LabelsTransparent;
    LabelsBackColorBox.Selected := LabelsBackColor;
    LabelsArrowsBox.Checked := LabelsArrows;
    LabelsStyleBox.ItemIndex := LabelsStyle;
  end;
  PieOptionsSheet.TabVisible := False;
  case SeriesOptions.SeriesType of
  stLine:
  begin
    LineOptionsSheet.TabVisible := True;
    LineVisibleBox.Visible := False;
    MarkOptionsSheet.TabVisible := True;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := LineOptionsSheet;
  end;
  stFastLine:
  begin
    LineOptionsSheet.TabVisible := True;
    LineVisibleBox.Visible := False;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := False;
    PageControl2.ActivePage := LineOptionsSheet;
  end;
  stPoint:
  begin
    LineOptionsSheet.TabVisible := False;
    MarkOptionsSheet.TabVisible := True;
    AreaOptionsSheet.TabVisible := False;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := MarkOptionsSheet;
  end;
  stBar, stHorizBar:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := AreaOptionsSheet;
  end;
  stArea:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := AreaOptionsSheet;
  end;
  stPie:
  begin
    LineOptionsSheet.TabVisible := True;
    MarkOptionsSheet.TabVisible := False;
    AreaOptionsSheet.TabVisible := False;
    PieOptionsSheet.TabVisible := True;
    LabelsOptionsSheet.TabVisible := True;
    PageControl2.ActivePage := PieOptionsSheet;
  end;
  end;
end;

{Transfer options from the dialog to a data series.}
procedure TChartOptionsDlg.SaveSeriesOptions(const Index: Integer);
var
  SeriesOptions: TSeriesOptions;
begin
  theSeries.Strings[Index] := SeriesTitle.Text;
  SeriesOptions := TSeriesOptions(theSeries.Objects[Index]);
  with SeriesOptions do
  begin
    if LineOptionsSheet.TabVisible then
    begin
      LineStyle := LineStyleBox.ItemIndex;
      LineColor := LineColorBox.Selected;
      LineWidth := LineSizeUpDown.Position;
      LineVisible := LineVisibleBox.Checked;
    end;
    if MarkOptionsSheet.TabVisible then
    begin
      PointStyle := MarkStyleBox.ItemIndex;
      PointColor := MarkColorBox.Selected;
      PointSize := MarkSizeUpDown.Position;
      PointVisible := MarkVisibleBox.Checked;
    end;
    if AreaOptionsSheet.TabVisible then
    begin
      AreaFillStyle := AreaFillStyleBox.ItemIndex;
      AreaFillColor := AreaColorBox.Selected;
      AreaStacking := StackStyleBox.ItemIndex;
    end;
    if PieOptionsSheet.TabVisible then
    begin
      PieCircled := PieCircledBox.Checked;
      PieUsePatterns := PiePatternBox.Checked;
      PieRotation := PieRotateUpDown.Position;
    end;
    if LabelsOptionsSheet.TabVisible then
    begin
      LabelsVisible := LabelsVisibleBox.Checked;
      LabelsArrows := LabelsArrowsBox.Checked;
      LabelsTransparent := LabelsTransparentBox.Checked;
      LabelsBackColor := LabelsBackColorBox.Selected;
      LabelsStyle := LabelsStyleBox.ItemIndex;
    end;
  end;
end;

procedure TChartOptionsDlg.HelpBtnClick(Sender: TObject);
var
  HC: Integer;
begin
  case PageControl1.ActivePageIndex of
    0:   HC := 250;
    1:   HC := 251;
    2:   HC := 251;
    3:   HC := 252;
    4:   HC := 253;
    else HC := 0;
  end;
  if HC > 0
  then HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, HC);
end;

end.
