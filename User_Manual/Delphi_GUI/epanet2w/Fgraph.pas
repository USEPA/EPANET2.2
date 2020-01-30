unit Fgraph;

{-------------------------------------------------------------------}
{                    Unit:    Fgraph.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that displays network data and analysis          }
{   results in the form of a graph. The graph is created by         }
{   calling CreateGraph. It gets refreshed (after a new network     }
{   analysis is made) by calling RefreshGraph. SetGraphOptions      }
{   is called to change display options.                            }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Math, Clipbrd, System.UITypes,
  VclTee.Series, VclTee.Chart, VclTee.TeEngine, VclTee.TeeProcs,
  VclTee.TeCanvas, VclTee.TeeGDIPlus,
  XPrinter, Uglobals, Uutils;

const
  PRODUCED = 0;
  CONSUMED = 1;
  TXT_NODE = 'Node';
  TXT_LINK = 'Link';
  TXT_SERIES = 'Series';
  TXT_COMPUTED = 'Computed';
  TXT_OBSERVED = 'Observed';
  TXT_TIME_HRS = 'Time (hours)';
  TXT_FLOW = 'Flow';
  TXT_HEAD = 'Head';
  TXT_HEADLOSS = 'Headloss';
  TXT_DISTANCE = 'Distance';
  TXT_FEET = ' (feet)';
  TXT_METERS = ' (meters)';
  TXT_NO_REACTION = 'No reactions occurred';
  TXT_AVG_RATES = 'Average Reaction Rates (kg/day)';
  TXT_BULK = 'Bulk';
  TXT_WALL = 'Wall';
  TXT_TANKS = 'Tanks';
  TXT_PRODUCED = 'Produced';
  TXT_CONSUMED = 'Consumed';
  TXT_TIME_SERIES_PLOT = ' Time Series Plot - ';
  TXT_FREQUENCY_PLOT = ' Frequency Plot - ';
  TXT_PROFILE_PLOT = ' Profile Plot - ';
  TXT_REACT_REPORT = ' Reaction Report';
  TXT_SYSTEM_FLOW = ' System Flow Balance';
  TXT_DOES_NOT_EXIST = ' does not exist.';
  TXT_DISTRIBUTION = 'Distribution of ';
  TXT_PERCENT_LESS = 'Percent Less Than';
  TXT_FREQUENCY = 'Frequency';
  TXT_PROFILE = 'Profile of ';
  TXT_FOR_LINK = 'for Link ';
  TXT_FOR_SELECTED_LINKS = 'for Selected Links';
  TXT_FOR_NODE = 'for Node ';
  TXT_FOR_SELECTED_NODES = 'for Selected Nodes';
  TXT_AT = ' at ';
  FMT_INFLOW = 'Inflow Rate = %.0f';

type
  TGraphForm = class(TForm)
    Chart1: TChart;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    Graph: TGraphSelection;
    SeriesList: TStringlist;    //List of data series created
    ChartDlgPage: Integer;      //Starting page of chart options dialog
    procedure AddObsTimeData(aSeries: TPointSeries; const aTime: String;
      const Ystr: String);
    procedure CalibDataPlot(const Fname, ID: String);
    procedure CopyToBitmap(const Fname: String);
    procedure CopyToMetafile(const Fname: String);
    procedure CopyToString(const Fname: String);
    function  CreateSysFlowPlot: Boolean;
    function  CreateAreaSeries(const Stitle: String): Boolean;
    function  CreateFrequencyPlot: Boolean;
    function  CreateLineSeries(const I: Integer; const Stitle: String): Boolean;
    function  CreatePieSeries(const Stitle: String): Boolean;
    function  CreatePointSeries(const Stitle: String): Boolean;
    function  CreateProfilePlot: Boolean;
    function  CreateReactRatePlot: Boolean;
    function  CreateTimeSeriesPlot: Boolean;
    function  GetLinkData(const ID: String; const Vtype: Integer;
      const N1, N2: Integer; var Y: PSingleArray): Boolean;
    procedure GetMarkText(Sender: TChartSeries; ValueIndex: LongInt;
      var MarkText: String);
    function  GetNodeData(const ID: String; const Vtype: Integer;
      const N1, N2: Integer; var Y: PSingleArray): Boolean;
    procedure GetSysFlow(const Period: Integer; var SysFlow: array of Double);
    procedure RefreshSysFlowPlot;
    procedure RefreshFrequencyPlot;
    procedure RefreshProfilePlot;
    procedure RefreshReactRatePlot;
    procedure RefreshTimeSeriesPlot;
    procedure SaveDefaultOptions;
    procedure SetAxisScale(theAxis: TChartAxis);  {--- Added 5/11/18 ---}
    procedure SetAxisOptions(Axis: TChartAxis);
  public
    { Public declarations }
    procedure CopyTo;
    function  CreateGraph(GraphSelection: TGraphSelection): Boolean;
    procedure Print(Destination: TDestination);
    procedure RefreshGraph;
    procedure SetGraphOptions;
  end;

//var
//  GraphForm: TGraphForm;

implementation

{$R *.DFM}

uses
  Dcopy, Fbrowser, Fmain, Uinput, Uoutput, Dchart;


//===================================================================
//                        Event handlers
//===================================================================

procedure TGraphForm.FormCreate(Sender: TObject);
//-----------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------
begin
// Create stringlists to store items being graphed
// and names of data series being graphed
  Uglobals.SetFont(self);
  Graph.Items := TStringlist.Create;
  SeriesList := TStringlist.Create;
  ChartDlgPage := 0;

  // Create a TeeChart GDIplus canvas for antialiasing
  Chart1.Canvas := TGDIPlusCanvas.Create;

  // Make max. axis label round to highest integer value
  with Chart1 do
  begin
    LeftAxis.MaximumRound := True;
    BottomAxis.MaximumRound := True;
    LeftAxis.MinimumRound := True;
    BottomAxis.MinimumRound := True;
  end;

  // Make the left mouse zoom when shift key is pressed
  Chart1.Zoom.KeyShift :=[ssShift];
  Chart1.ScrollMouseButton := mbLeft;

// Initialize chart properties
  with Chart1 do
  begin
    View3D := GraphOptions.View3D;
    Chart3DPercent := GraphOptions.Percent3D;
    Color := GraphOptions.PanelColor;
    with BackWall do
    begin
      Color := GraphOptions.BackColor;
      Gradient.Visible := False;
      Transparent := False;
      Visible := True;
    end;
    Legend.ColorWidth := GraphOptions.LegendWidth;
    Legend.LegendStyle :=lsSeries;
    Legend.Alignment := TLegendAlignment(GraphOptions.LegendPosition);
    Legend.Visible := GraphOptions.LegendVisible;
    Title.Text.Clear;
    Title.Font.Name := GraphOptions.TitleFontName;
    Title.Font.Size := GraphOptions.TitleFontSize;
    if GraphOptions.TitleFontBold then
      Title.Font.Style := Title.Font.Style + [fsBold]
    else
      Title.Font.Style := [];
    SetAxisOptions(BottomAxis);
    SetAxisOptions(LeftAxis);
    Legend.Font.Assign(BottomAxis.Title.Font);
    BottomAxis.Title.Caption := TXT_TIME_HRS;
  end;
end;

procedure TGraphForm.FormActivate(Sender: TObject);
//---------------------------------------------------
// OnActivate handler for form.
// Enables Options speedbutton on MainForm.
//---------------------------------------------------
begin
  MainForm.TBOptions.Enabled := True;
end;

procedure TGraphForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------
// OnClose handler for form. Frees all memory.
//-----------------------------------------------
begin
  Action := caFree;
end;

procedure TGraphForm.FormDestroy(Sender: TObject);
//------------------------------------------------
// OnDestroy handler for form. Frees chart series
// and stringlists.
//------------------------------------------------
begin
  with Chart1 do
    while SeriesCount > 0 do Series[0].Free;
  SeriesList.Free;
  if Assigned(Graph.Items) then Graph.Items.Free;
end;

procedure TGraphForm.Chart1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//---------------------------------------------------------------
// OnMouseDown handler for Chart1. Launches graph options dialog
// when user right-clicks over the graph.
//---------------------------------------------------------------
begin
  if (Button = mbRight) and not (ssCtrl in Shift)
    then SetGraphOptions;
end;

function  TGraphForm.CreateGraph(GraphSelection: TGraphSelection): Boolean;
//-------------------------------------------------------
// Creates a graph based on selections in GraphSelection.
//-------------------------------------------------------
begin
// Copy graph selection choices
  with GraphSelection do
  begin
    Graph.GraphType := GraphType;
    Graph.VarType := VarType;
    Graph.ObjectType := ObjectType;
    Graph.Period := Period;
    if Assigned(Items) then Uutils.CopyStringList(Items, Graph.Items)
    else Graph.Items := nil;
  end;

// Create the called for graph
  case Graph.GraphType of
    TIMESERIESPLOT: Result := CreateTimeSeriesPlot;
    PROFILEPLOT:    Result := CreateProfilePlot;
    FREQUENCYPLOT:  Result := CreateFrequencyPlot;
    SYSFLOWPLOT:    Result := CreateSysFlowPlot;
    REACTRATEPLOT:  Result := CreateReactRatePlot;
    else            Result := False;
  end;
end;

procedure TGraphForm.RefreshGraph;
//--------------------------------------------
// Refreshes graph display after data changes.
//--------------------------------------------
begin
  case Graph.GraphType of
    TIMESERIESPLOT: RefreshTimeSeriesPlot;
    PROFILEPLOT:    RefreshProfilePlot;
    FREQUENCYPLOT:  RefreshFrequencyPlot;
    REACTRATEPLOT:  RefreshReactRatePlot;
    SYSFLOWPLOT:    RefreshSysFlowPlot;
  end;
end;


//===================================================================
//                 Time Series Ploting Functions
//===================================================================

function TGraphForm.CreateTimeSeriesPlot: Boolean;
//------------------------------
// Creates a time series graph.
//------------------------------
var
  k          : Integer;
  prefix     : String;
  ytitle     : String;
  vtitle     : String;
  ltitle     : String;
begin
  Result := True;
  with Graph do
  begin

  // Assign titles
    if ObjectType = NODESERIES then
    begin
      vtitle := BrowserForm.NodeViewBox.Items[VarType];
      ytitle := vtitle;
      prefix := TXT_NODE;
      if (Length(NodeUnits[VarType].Units) > 0) then
      ytitle := ytitle + ' (' + NodeUnits[VarType].Units + ')';
      if Items.Count > 1 then
        ltitle := TXT_FOR_SELECTED_NODES
      else
        ltitle := TXT_FOR_NODE + Items[0];
    end;
    if ObjectType = LINKSERIES then
    begin
      vtitle := BrowserForm.LinkViewBox.Items[VarType];
      ytitle := vtitle;
      prefix := TXT_LINK;
      if (Length(LinkUnits[VarType].Units) > 0) then
        ytitle := ytitle + ' (' + LinkUnits[VarType].Units + ')';
      if Items.Count > 1 then
        ltitle := TXT_FOR_SELECTED_LINKS
      else
        ltitle := TXT_FOR_LINK + Items[0];
    end;
    Caption := TXT_TIME_SERIES_PLOT + vtitle + ' ' + ltitle;
    Chart1.LeftAxis.Title.Caption := ytitle;
    Chart1.Title.Text.Add(vtitle + ' ' + ltitle);
    if Items.Count > 1 then
      Chart1.Legend.Visible := True;

  // Create line series for each item to be plotted,
  // create a point series for calibration data & plot data
    ltitle := TXT_COMPUTED;
    for k := 0 to Items.Count - 1 do
    begin
      if Items.Count > 1 then ltitle := prefix + ' ' + Items[k];
      CreateLineSeries(k,ltitle);
    end;
    CreatePointSeries(TXT_OBSERVED);
    RefreshTimeSeriesPlot;
  end;
end;

procedure TGraphForm.RefreshTimeSeriesPlot;
//-----------------------------------------
// Refreshes drawing of a time series plot.
//-----------------------------------------
var
  j, k       : Integer;
  n, n1, n2  : Integer;
  flag       : Boolean;
  ID         : String;
  dt         : Single;
  x          : Single;
  y          : PSingleArray;
begin
//Determine number of points to plot
  n1 := 0;
  n2 := Nperiods - 1;
  n  := n2 - n1 + 1;
  dt := Rstep/3600.;

//Allocate memory for y-data
  GetMem(y, n*SizeOf(Single));
  with Graph do
  try

  // Get data for each time series
    if Items.Count = 1 then Chart1.Legend.Visible := False;
    for k := 0 to Items.Count - 1 do
    begin

    //Get time series data
      ID := Items[k];
      if ObjectType = NODESERIES then
        flag := GetNodeData(ID, VarType, n1, n2, y)
      else
        flag := GetLinkData(ID, VarType, n1, n2, y);

    //Transfer data to chart series
      Chart1.Series[k].Clear;
      Chart1.Series[k].Active := False;
      if flag then
      try
        with Chart1.Series[k] do
        begin
          x := (Rstart + n1*Rstep)/3600;
          for j := 0 to n-1 do
          begin
            AddXY(x, y^[j], '', clTeeColor);
            x := x + dt;
          end;
          Active := True;
        end;
      finally
      end;

    // Proceed to next series
    end;
  finally
    FreeMem(y, n*SizeOf(Single));
  end;

//Load calibration data into chart series
  with Graph do
  begin
    if  (Items.Count = 1) then
    begin
      if (ObjectType = NODESERIES)
      and (VarType in [DEMAND..NODEQUAL])
      and (NodeCalibData[VarType].Locations.IndexOfName(ID) >= 0)
      then CalibDataPlot(NodeCalibData[VarType].FileName, ID);
      if (ObjectType = LINKSERIES)
      and (VarType in [FLOW..HEADLOSS])
      and (LinkCalibData[VarType].Locations.IndexOfName(ID) >= 0)
      then CalibDataPlot(LinkCalibData[VarType].FileName, ID);
    end;
  end;

  // Scale the axes
  SetAxisScale(Chart1.BottomAxis);
  SetAxisScale(Chart1.LeftAxis);
end;

function TGraphForm.GetNodeData(const ID: String; const Vtype: Integer;
  const N1, N2: Integer; var Y: PSingleArray): Boolean;
//-------------------------------------------------------------------
// Retrieves time series data (Y) for variable Vtype for node ID
// for time periods N1 to N2.
//-------------------------------------------------------------------
var
  atype : Integer;
  index : Integer;
  i, j  : Integer;
  z     : Single;
begin
// Check that node is in the database
  if (Uinput.FindNode(ID,atype,index) = False) then
    Result := False

// If variable is an input parameter then all series entries have same value
  else if NodeVariable[Vtype].Source = vsInput then
  begin
    z := Uoutput.GetNodeValue(Vtype,0,atype,index);
    for i := N1 to N2 do Y^[i] := z;
    Result := True;
  end

// If variable is a computed output, then make sure results
// for node exist and retrieve them from output file
  else
  begin
    i := Node(atype,index).Zindex;
    if (i < 0) then
      Result := False
    else
    begin
      j := NodeVariable[Vtype].SourceIndex[JUNCS];
      Uoutput.GetNodeSeries(i, j, N1, N2, Y);
      Result := True;
    end;
  end;
end;

function TGraphForm.GetLinkData(const ID: String; const Vtype: Integer;
  const N1, N2: Integer; var Y: PSingleArray): Boolean;
//-------------------------------------------------------------------
// Retrieves time series data (Y) for variable Vtype for lin ID
// for time periods N1 to N2.
//-------------------------------------------------------------------
var
  atype : Integer;
  index : Integer;
  i, j  : Integer;
  z     : Single;
begin
// Check that link is in the database
  if (Uinput.FindLink(ID,atype,index) = False) then
    Result := False

// If variable is an input parameter then all series entries have same value
  else if LinkVariable[Vtype].Source = vsInput then
  begin
    z := Uoutput.GetLinkValue(Vtype,0,atype,index);
    for i := N1 to N2 do Y^[i] := z;
    Result := True;
  end

// If variable is a computed output, then make sure results
// for link exist and retrieve them from output file
  else
  begin
    i := Link(atype,index).Zindex;
    if (i < 0) then
      Result := False
    else
    begin
      j := LinkVariable[Vtype].SourceIndex[PIPES];
      Uoutput.GetLinkSeries(i, j, n1, n2, y);
      Result := True;
    end;
  end;
end;

procedure TGraphForm.CalibDataPlot(const Fname, ID: String);
//-----------------------------------------------------------------
// Adds calibration data (as a point series) to a time series plot.
//-----------------------------------------------------------------
var
  nseries  : Integer;
  ntoks, p : Integer;
  toklist  : TStringList;
  usedata  : Boolean;
  F        : TextFile;
  Line     : String;
  S        : String;
begin
  nseries := Chart1.SeriesCount-1;
  Chart1.Series[nseries].Active := False;
  if FileExists(Fname) then
  begin
    AssignFile(F,Fname);
    {$I-}
    Reset(F);
    {$I+}
    if IOResult = 0 then
    begin
      toklist := TStringList.Create;
      try
        usedata := False;
        while not EOF(F) do
        begin
          Readln(F,Line);
          S := Line;
          p := Pos(';',S);
          if (p > 0) then Delete(S,p,256);
          Uutils.Tokenize(S,toklist,ntoks);
          if (ntoks = 3) then
          begin
            if (toklist[0] = ID) then
              usedata := True
            else
              usedata := False;
          end;
          if (ntoks >= 2) and (ntoks <= 3) and (usedata) then
            AddObsTimeData(TPointSeries(Chart1.Series[nseries]),
              toklist[ntoks-2],toklist[ntoks-1]);
        end;
        with Chart1.Series[nseries] do
        begin
          if (Count > 0) then
          begin
            ShowInLegend := True;
            Chart1.Legend.Visible := True;
          end;
        end;
        Chart1.Series[nseries].Active := True;
      finally
        toklist.Free;
      end;
    end;
    CloseFile(F);
  end;
end;

procedure TGraphForm.AddObsTimeData(aSeries: TPointSeries; const aTime: String;
  const Ystr: String);
//--------------------------------------------------------------------
// Adds observed data value Ystr at time aTime to plotted point
// series aSeries.
//---------------------------------------------------------------------
var
  x,y: Single;
begin
  x := Uutils.StrHoursToFloat(aTime);
  if (x >= 0) then
  begin
    if Uutils.GetSingle(Ystr,y) then aSeries.AddXY(x,y,'',clTeeColor);
  end;
end;


//===============================================================
//                  Frequency Plot Functions
//===============================================================

function TGraphForm.CreateFrequencyPlot: Boolean;
//------------------------------------------------------
// Creates a frequency plot of ViewVar at time period T.
//------------------------------------------------------
var
  xTitle : String;
  vTitle : String;
  tTitle : String;
  sTitle : String;

begin
  with Graph do
  begin
    tTitle := '';
    if ObjectType in [JUNCS..TANKS] then
    begin
      vTitle := BrowserForm.NodeViewBox.Items[VarType];
      xTitle := vTitle;
      if (Length(NodeUnits[VarType].Units) > 0) then
        xTitle := xTitle + ' (' + NodeUnits[VarType].Units + ')';
      with NodeVariable[VarType] do
      begin
        if (Source = vsOutput) and (Nperiods > 1) then
          tTitle := BrowserForm.TimeListBox.Items[Period];
      end;
    end
    else
    begin
      vTitle := BrowserForm.LinkViewBox.Items[VarType];
      xTitle := vTitle;
      if (Length(LinkUnits[VarType].Units) > 0) then
        xTitle := xTitle + ' (' + LinkUnits[VarType].Units + ')';
      tTitle := '';
      with LinkVariable[VarType] do
      begin
        if (Source = vsOutput) and (Nperiods > 1) then
        tTitle := BrowserForm.TimeListBox.Items[Period];
      end;
    end;
  end;
  sTitle := vTitle;
  if Length(tTitle) > 0 then
    sTitle := vTitle + TXT_AT + tTitle;
  Caption := TXT_FREQUENCY_PLOT + sTitle;
  with Chart1 do
  begin
    Title.Text.Add(TXT_DISTRIBUTION + sTitle);
    BottomAxis.Title.Caption := xTitle;
    if Graph.ObjectType in [JUNCS..TANKS]
    then sTitle := 'Percent of Nodes Less Than'
    else sTitle := 'Percent of Links Less Than';
    LeftAxis.Title.Caption := sTitle;
    Legend.Visible := False;
  end;
  CreateAreaSeries(TXT_FREQUENCY);
  RefreshFrequencyPlot;
  Result := True;
end;

procedure TGraphForm.RefreshFrequencyPlot;
//----------------------------------------
// Refreshes drawing of a frequency plot.
//----------------------------------------
var
  i          : Integer;
  ny         : Integer;
  ycount     : Integer;
  en         : Single;
  y          : PSingleArray;
  aList      : TList;
  Compare    : function(a, b: Pointer): Integer;

begin
// Check if TimePeriod still valid
  if Chart1.SeriesCount > 0 then with Chart1.Series[0] do Clear;
  if Graph.Period >= Nperiods then Exit;

// Assign Compare function for sorting
  Compare := Uutils.CompareSingles;

// Allocate memory for plot's data
  if (Graph.ObjectType in [JUNCS..TANKS]) then
    ny := Network.Lists[JUNCS].Count
  else
    ny := Network.Lists[PIPES].Count;
  GetMem(y, ny*SizeOf(Single));
  aList := TList.Create;

  // Get plot data
  with Graph do
  try
    if ObjectType in [JUNCS..TANKS] then
      Uoutput.GetJuncValues(VarType,Period,y,ycount)
    else if ObjectType in [PIPES..VALVES] then
      Uoutput.GetPipeValues(VarType,Period,y,ycount)
    else ycount := 0;

  // Use a TList to sort the data
    for i := 0 to ycount-1 do
      aList.Add(Addr(y^[i]));
    aList.Sort(Compare);

  // Load data into chart series
    if Chart1.SeriesCount > 0 then with Chart1.Series[0] do
    begin
      Active := False;
      en := ycount;
      for i := 0 to ycount-1 do
        AddXY(Single(aList.Items[i]^), 100*i/en, '', clTeeColor);
      Active := True;
    end;
    SetAxisScale(Chart1.LeftAxis);

  finally
    aList.Free;
    FreeMem(y, ny*SizeOf(Single));
  end;
end;


//===================================================================
//                     Profile Plot Functions
//===================================================================

function TGraphForm.CreateProfilePlot: Boolean;
//---------------------------------------------
// Creates a profile plot
//---------------------------------------------
var
  vTitle: String;
  tTitle: String;
  yTitle: String;
  sTitle: String;

begin

// Set form caption
  with Graph do
  begin
    vTitle := BrowserForm.NodeViewBox.Items[VarType];
    yTitle := vTitle;
    if (Length(NodeUnits[VarType].Units) > 0) then
      yTitle := yTitle + ' (' + NodeUnits[VarType].Units + ')';
    tTitle := '';
    if (NodeVariable[VarType].Source = vsOutput) and (Nperiods > 1) then
      tTitle := BrowserForm.TimeListBox.Items[Period];
    if Length(tTitle) = 0 then
      sTitle := vTitle
    else
      sTitle := vTitle + TXT_AT + tTitle;
  end;
  Caption := TXT_PROFILE_PLOT + sTitle;

// Set chart titles
  with Chart1 do
  begin
    Title.Text.Clear;
    Title.Text.Add(TXT_PROFILE + sTitle);
    sTitle := TXT_DISTANCE;
    if MapDimensions.Units = muFeet then sTitle := sTitle + TXT_FEET;
    if MapDimensions.Units = muMeters then sTitle := sTitle + TXT_METERS;
    BottomAxis.Title.Caption := sTitle;
    LeftAxis.Title.Caption := yTitle;
  end;

// Create an AreaSeries for chart and draw it
  CreateAreaSeries('Series1');
  if Chart1.SeriesCount > 0 then
    with Chart1.Series[0] as TAreaSeries do
  begin
    Marks.Visible := True;
    Marks.Transparent := GraphOptions.LabelsTransparent;
    Marks.BackColor := GraphOptions.LabelsBackColor;
    Marks.Clip := False;
    SeriesColor := clMoneyGreen;;
    AreaChartBrush.Color := clMoneyGreen;
  end;
  RefreshProfilePlot;
  Result := True;
end;

procedure TGraphForm.RefreshProfilePlot;
//-----------------------------------------
// Refreshes the drawing of a profile plot.
//-----------------------------------------
var
  i      : Integer;
  objtype  : Integer;
  objindex : Integer;
  dist     : Single;
  y        : Single;
  x0, y0   : Single;
  x1, y1   : Single;
  ID       : String;
begin
  if Chart1.SeriesCount > 0 then with Graph, Chart1.Series[0] do
  begin

  // Initialize the data series
    Clear;
    Active := False;
    Title := NodeVariable[VarType].Name;
    SeriesList.Clear;
    x0 := MISSING;
    y0 := MISSING;
    dist := 0;

  // For each node in the list of nodes
    if Period < Nperiods then for i := 0 to Items.Count-1 do
    begin

    // Check that item is in the database
      ID := Items[i];
      if (Uinput.FindNode(ID,objtype,objindex)) then
      begin

      // Find distance from last node
        x1 := Node(objtype,objindex).X;
        y1 := Node(objtype,objindex).Y;
        if (x1 <> MISSING) and (y1 <> MISSING) then
        begin
          if (x0 = MISSING) and (y0 = MISSING) then
          begin
            x0 := x1;
            y0 := y1;
          end;
          dist := dist + sqrt(sqr(x1-x0) + sqr(y1-y0));
          x0 := x1;
          y0 := y1;

      // Get value for this node & add to chart
          y := Uoutput.GetNodeValue(VarType,Period,objtype,objindex);
          if (y <> MISSING) then
          begin
            Chart1.Series[0].AddXY(dist,y,'',clTeeColor);
            SeriesList.Add(Items[i]);
          end;
        end;
      end;
    end;
    Active := True;

{---  Added 5/11/18  ---}
    // Scale the axes
    SetAxisScale(Chart1.BottomAxis);
    SetAxisScale(Chart1.LeftAxis);

  end;
end;


//===================================================================
//                      Reaction Rate Plot
//===================================================================

function TGraphForm.CreateReactRatePlot: Boolean;
//----------------------------------------
// Creates a reaction rate plot.
//----------------------------------------
begin
  Caption := TXT_REACT_REPORT;
  with Chart1 do
  begin
    Title.Text.Clear;
    Foot.Text.Clear;
    Foot.Font.Assign(BottomAxis.Title.Font);
    Foot.Font.Size := 10;
    Legend.LegendStyle := lsValues;
    Legend.ColorWidth := 24;
  end;
  CreatePieSeries('Series1');
  RefreshReactRatePlot;
  Result := True;
end;

procedure TGraphForm.RefreshReactRatePlot;
//-----------------------------------------------
// Refreshes the drawing of a reaction rate plot.
//-----------------------------------------------
var
  i: Integer;
  rate : array[0..3] of Double;
  r: array[0..3] of Single;
  maxrate: Double;
  ucf: Single;
begin
// Find conversion factor to kilograms/day
  ucf := 1.0e6/24;
  if Pos('ug', NodeUnits[NODEQUAL].Units) > 0 then ucf := 1.0e9/24;

  if Chart1.SeriesCount > 0 then with Chart1.Series[0] do
  begin

  // Initialize the chart
    Clear;
    Active := False;
    Chart1.Title.Text.Clear;
    Chart1.Foot.Text.Clear;

  // Get average reaction rates from output file
    Uoutput.GetReactRates(r);
    for i := 0 to 3 do rate[i] := r[i] / ucf;

  // Check max. rate to see if any reactions occurred
    maxrate := MaxValue(Slice(rate,3));
    if maxrate = 0 then
    begin
      Chart1.Foot.Text.Add(TXT_NO_REACTION);
    end

  // Add each rate category to chart
    else
    begin
      Chart1.Title.Text.Add(TXT_AVG_RATES);
      Add(rate[0],TXT_BULK,clBlue);
      Add(rate[1],TXT_WALL,clRed);
      Add(rate[2],TXT_TANKS,clGreen);
      Active := True;
      Chart1.Foot.Text.Add(Format(FMT_INFLOW,[rate[3]]));
    end;
  end;

// Set chart display options
  with Chart1 do
  begin
    AxisVisible := False;
    Frame.Visible := True;
    BackColor := clWhite;
    Legend.Visible := True;
    Refresh;
  end;
end;


//===================================================================
//                      System Flow Plot
//===================================================================

function TGraphForm.CreateSysFlowPlot: Boolean;
//--------------------------------------
// Creates a system flow plot.
//--------------------------------------
begin
  Result := True;
  Caption := TXT_SYSTEM_FLOW;
  Chart1.Title.Text.Add(TXT_SYSTEM_FLOW);
  Chart1.LeftAxis.Title.Caption := TXT_FLOW +
    ' (' + NodeUnits[DEMAND].Units + ')';
  Chart1.Legend.Visible := True;
  CreateLineSeries(0,TXT_PRODUCED);
  SeriesList.Add(TXT_PRODUCED);
  CreateLineSeries(1,TXT_CONSUMED);
  SeriesList.Add(TXT_CONSUMED);
  RefreshSysFlowPlot;
end;

procedure TGraphForm.RefreshSysFlowPlot;
//---------------------------------------------
// Refreshes the display of a system flow plot.
//---------------------------------------------
var
  j, k       : Integer;
  n, n1, n2  : Integer;
  dt         : Single;
  x          : Single;
  sysflow    : Array[PRODUCED..CONSUMED] of Double;
begin
//Determine number of points to plot
  n1 := 0;
  n2 := Nperiods - 1;
  n  := n2 - n1 + 1;
  dt := Rstep/3600.;

//Fill in system flow time series
  for k := PRODUCED to CONSUMED do
  begin
    Chart1.Series[k].Clear;
    Chart1.Series[k].Active := False;
  end;
  x := (Rstart + n1*Rstep)/3600;
  for j := 0 to n-1 do
  begin
    GetSysFlow(j,sysflow);
    for k := PRODUCED to CONSUMED do
      Chart1.Series[k].AddXY(x, sysflow[k], '', clTeeColor);
    x := x + dt;
  end;
  for k := PRODUCED to CONSUMED do Chart1.Series[k].Active := True;

{---  Added 5/11/18  ---}
  // Scale the axes
  SetAxisScale(Chart1.BottomAxis);
  SetAxisScale(Chart1.LeftAxis);
end;

procedure TGraphForm.GetSysFlow(const Period: Integer;
  var SysFlow: array of Double);
//--------------------------------------------------------------
// Sums up flow produced and consumed in network in time Period.
//--------------------------------------------------------------
var
  i,j,k: Integer;
  aNode: TNode;
  Z  : PSingleArray;
begin
// Allocate scratch array to hold nodal demands
  for i := PRODUCED to CONSUMED do SysFlow[i] := 0;
  GetMem(Z, Nnodes*SizeOf(Single));
  try

  // Retrieve nodal demands from output file for time Period
    k := NodeVariable[DEMAND].SourceIndex[JUNCS];
    GetNodeValues(k,Period,Z);

  // Update total consumption/production depending on sign of demand
    for i := JUNCS to RESERVS do
    begin
      for j := 0 to Network.Lists[i].Count-1 do
      begin
        aNode := Node(i,j);
        k := aNode.Zindex;
        if k >= 0 then
        begin
          if Z[k] > 0 then SysFlow[CONSUMED] := SysFlow[CONSUMED] + Z[k]
          else SysFlow[PRODUCED] := SysFlow[PRODUCED] - Z[k];
        end;
      end;
    end;
  finally
    FreeMem(Z, Nnodes*SizeOf(Single));
  end;
end;


//===================================================================
//                 Data Series Creation Functions
//===================================================================

function TGraphForm.CreateLineSeries(const I: Integer;
  const Stitle: String): Boolean;
//----------------------------------------------------
// Creates a Line series for use in Time Series and
// frequency plots.
//----------------------------------------------------
var
  aSeries: TLineSeries;
begin
  aSeries := TLineSeries.Create(self);
  with aSeries do
  try
    ParentChart := Chart1;
    Active := False;
    Title := Stitle;
    ShowInLegend := True;
    SeriesColor := GraphOptions.LineColor[i];
    LinePen.Visible := GraphOptions.LineVisible[i];
    LinePen.Style := TPenStyle(GraphOptions.LineStyle[i]);
    LinePen.Width := GraphOptions.LineWidth[i];
    Pointer.Visible := GraphOptions.PointVisible[i];
    Pointer.Style := TSeriesPointerStyle(GraphOptions.PointStyle[i]);
    Pointer.Brush.Color := GraphOptions.PointColor[i];
    Pointer.HorizSize := GraphOptions.PointSize[i];
    Pointer.VertSize := GraphOptions.PointSize[i];
  finally
    Result := Assigned(aSeries);
  end;
end;

function TGraphForm.CreatePointSeries(const Stitle: String): Boolean;
//-------------------------------------------------------------------
// Creates a Point series for plotting calibration data.
//-------------------------------------------------------------------
var
  aSeries : TPointSeries;
begin
  aSeries := TPointSeries.Create(self);
  with aSeries do
  try
    ParentChart := Chart1;
    Active := False;
    Title := Stitle;
    ShowInLegend := False;
    Pointer.Visible := GraphOptions.PointVisible[MAXSERIES];
    Pointer.Style := TSeriesPointerStyle(GraphOptions.PointStyle[MAXSERIES]);
    SeriesColor := GraphOptions.PointColor[MAXSERIES];
    Pointer.HorizSize := GraphOptions.PointSize[MAXSERIES];
    Pointer.VertSize := GraphOptions.PointSize[MAXSERIES];
  finally
    Result := Assigned(aSeries);
  end;
end;

function TGraphForm.CreateAreaSeries(const Stitle: String): Boolean;
//-------------------------------------------------------------------
// Creates an Area series for Profile plots.
//-------------------------------------------------------------------
var
  aSeries: TAreaSeries;
begin
  aSeries := TAreaSeries.Create(self);
   with aSeries do
  try
    ParentChart := Chart1;
    Active := False;
    Title := Stitle;
    ShowInLegend := False;

    SeriesColor := clSkyBlue;;
    AreaLinesPen.Visible := False;
    AreaChartBrush.Color := clSkyBlue;
    AreaChartBrush.Gradient.Visible := False;  //True;
    Transparency := 25;
    Marks.Visible := False;
    Marks.Transparent := GraphOptions.LabelsTransparent;
    Marks.BackColor := GraphOptions.LabelsBackColor;
    Marks.Clip := False;
    OnGetMarkText := GetMarkText;

{  DEPRECATED
    SeriesColor := GraphOptions.AreaFillColor;
    AreaBrush := GraphOptions.AreaFillStyle;
    Marks.Visible := GraphOptions.LabelsVisible;
    Marks.Transparent := GraphOptions.LabelsTransparent;
    Marks.BackColor := GraphOptions.LabelsBackColor;
    Marks.Clip := False;
    OnGetMarkText := GetMarkText;
}
  finally
    Result := Assigned(aSeries);
  end;
end;

procedure TGraphForm.GetMarkText(Sender: TChartSeries; ValueIndex: LongInt;
  var MarkText: String);
//------------------------------------------------------------------
// OnGetMarktext event handler used by Area series in profile plots.
//------------------------------------------------------------------
begin
  if SeriesList.Count > ValueIndex then
    MarkText := SeriesList[ValueIndex];
end;

function TGraphForm.CreatePieSeries(const Stitle: String): Boolean;
//-------------------------------------------------------------------
// Creates a Pie series for Reaction Rate plots.
//-------------------------------------------------------------------
var
  aSeries: TPieSeries;
begin
  aSeries := TPieSeries.Create(self);
  with aSeries do
  try
    ParentChart := Chart1;
    Active := False;
    Circled := not GraphOptions.View3D;
    Title := Stitle;
    ValueFormat := '0.0';
    //ShowInLegend := True;
    UsePatterns := False;
    Marks.Visible := GraphOptions.LabelsVisible;
    Marks.Arrow.Visible := GraphOptions.LabelsArrows;
    Marks.Transparent := GraphOptions.LabelsTransparent;
    Marks.BackColor := GraphOptions.LabelsBackColor;
    Marks.Clip := False;
    Marks.Style := smsLabelPercent;
    Marks.Arrow.Color := clBlack;
  finally
    Result := Assigned(aSeries);
  end;
end;


//===================================================================
//         Functions for Setting & Saving Graph Options
//===================================================================

{---  Added 5/11/18  ---}
procedure TGraphForm.SetAxisScale(theAxis: TChartAxis);
//-----------------------------------------------------------------------------
//  Automatically scales the graph's axes
//-----------------------------------------------------------------------------
var
  zMin, zMax, zInc: Double;
begin
  if theAxis.IsDateTime then Exit;
  with theAxis do
  begin
    Automatic := False;
    AutomaticMinimum := False;
    AutomaticMaximum := False;
    if theAxis = Chart1.BottomAxis then
      begin
        zMin := Chart1.MinXValue(theAxis);
        zMax := Chart1.MaxXValue(theAxis);
      end
      else
      begin
        zMin := Chart1.MinYValue(theAxis);
        zMax := Chart1.MaxYValue(theAxis);
      end;
      Uutils.AutoScale(zMin, zMax, zInc);
      SetMinMax(zMin, zMax);
      Increment := zInc;
  end;
end;

procedure TGraphForm.SetAxisOptions(Axis: TChartAxis);
//----------------------------------------------------
// Initializes axis properties for TeeChart component
//----------------------------------------------------
var
  i: Integer;
begin
  with Axis do
  begin
    if Horizontal then i := 0
    else i := 1;
    Grid.Visible := GraphOptions.AxisGridStyle[i] > 0;
    Title.Font.Name := GraphOptions.AxisFontName;
    Title.Font.Size := GraphOptions.AxisFontSize;
    if GraphOptions.AxisFontBold then
      Title.Font.Style := Title.Font.Style + [fsBold]
    else
      Title.Font.Style := [];
    LabelsFont.Assign(Title.Font);
  end;
end;

procedure TGraphForm.SetGraphOptions;
var
   default: Boolean;
begin
  default := True;
  Dchart.Execute(self, Chart1, ChartDlgPage, default);
  if default then SaveDefaultOptions;
end;

procedure TGraphForm.SaveDefaultOptions;
var
  i: Integer;
begin
  with GraphOptions do
  begin
    View3D := Chart1.View3D;
    Percent3D := Chart1.Chart3DPercent;
    PanelColor := Chart1.Color;
    BackColor := Chart1.BackColor;
    LegendPosition := Ord(Chart1.Legend.Alignment);
    LegendVisible := Chart1.Legend.Visible;
    LegendColor := Chart1.Legend.Color;
    LegendWidth := Chart1.Legend.ColorWidth;
    LegendFramed := Chart1.Legend.Frame.Visible;
    with Chart1.Title.Font do
    begin
      TitleFontName := Name;
      TitleFontColor := Color;
      TitleFontSize := Size;
      TitleFontBold := (fsBold in Style);
    end;
    with Chart1.BottomAxis do
    begin
      AxisGridStyle[0] := Integer(Grid.Visible);
      with Title.Font do
      begin
        AxisFontName := Name;
        AxisFontSize := Size;
        AxisFontBold := (fsBold in Style);
      end;
    end;
    with Chart1.LeftAxis do
    begin
      AxisGridStyle[1] := Integer(Grid.Visible);
    end;
    for i := 0 to Chart1.SeriesCount-1 do
    begin
      if Chart1.Series[i].Active then
      begin
        if (Graph.GraphType = PROFILEPLOT)
        or (Graph.GraphType = REACTRATEPLOT) then
        begin
          LabelsVisible := Chart1.Series[i].Marks.Visible;
          LabelsArrows := Chart1.Series[i].Marks.Arrow.Visible;
          LabelsTransparent := Chart1.Series[i].Marks.Transparent;
          LabelsBackColor := Chart1.Series[i].Marks.BackColor;
        end;
        if Chart1.Series[i] is TAreaSeries then
          with Chart1.Series[i] as TAreaSeries do
        begin
          AreaFillColor := SeriesColor;
          AreaFillStyle := AreaBrush;
        end;
        if Chart1.Series[i] is TLineSeries then
          with Chart1.Series[i] as TLineSeries do
        begin
          LineColor[i] := SeriesColor;
          LineStyle[i] := Ord(LinePen.Style);
          LineWidth[i] := LinePen.Width;
          PointVisible[i] := Pointer.Visible;
          PointStyle[i] := Ord(Pointer.Style);
          PointColor[i] := Pointer.Brush.Color;
          PointSize[i] := Pointer.HorizSize;
        end;
        if Chart1.Series[i] is TPointSeries then
          with Chart1.Series[i] as TPointSeries do
        begin
          PointStyle[MAXSERIES] := Ord(Pointer.Style);
          PointColor[MAXSERIES] := SeriesColor;
          PointSize[MAXSERIES] := Pointer.HorizSize;
        end;
      end;
    end;
  end;
end;


//===================================================================
//                Copying and Printing Functions
//===================================================================

procedure TGraphForm.CopyTo;
//---------------------------------------------------
// Copies the graph to a file or to the Clipboard.
//---------------------------------------------------
begin
// Create the CopyTo dialog form
  with TCopyToForm.Create(self) do
  try

  // Show the dialog and retrieve name of file (DestFileName)
  // (If name is empty then graph is copied to the Clipboard)
    if ShowModal = mrOK then
    begin

    // Copy graph using selected format
      case FormatGroup.ItemIndex of
      0: CopyToBitmap(DestFileName);
      1: CopyToMetafile(DestFileName);
      2: CopyToString(DestFileName);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TGraphForm.CopyToBitmap(const Fname: String);
//-----------------------------------------------------
// Copies graph to file Fname (or to Clipboard)
// in bitmap format.
//-----------------------------------------------------
begin
  if Length(Fname) > 0 then
    Chart1.SaveToBitmapFile(Fname)
  else
    Chart1.CopyToClipboardBitmap;
end;

procedure TGraphForm.CopyToMetafile(const Fname: String);
//-----------------------------------------------------
// Copies graph to file Fname (or to Clipboard)
// in enhanced metafile format.
//-----------------------------------------------------
begin
  if Length(Fname) > 0 then
    Chart1.SaveToMetafileEnh(Fname)
  else
    Chart1.CopyToClipBoardMetaFile(True);
end;

procedure TGraphForm.CopyToString(const Fname: String);
//-------------------------------------------------------
// Copies data behind graph to file Fname or to Clipboard.
//-------------------------------------------------------
var
  Slist: TStringList;
  I,J  : Integer;
  S,S1 : String;

begin
// Create a stringlist to hold data points
  Slist := TStringList.Create;
  try

  // Add title to stringlist
    Slist.Add(Network.Options.Title);
    Slist.Add(Caption);

  // Add column headings to stringlist
    if Graph.GraphType <> REACTRATEPLOT then
    begin
      S := Format('%-16s'+#9+'%-16s'+#9+'%-16s', [TXT_SERIES,
        Chart1.BottomAxis.Title.Caption,
          Chart1.LeftAxis.Title.Caption]);
      Slist.Add(S);
    end;

  // Iterate over each series
    with Chart1 do
    begin
      for I := 0 to SeriesCount - 1 do
      begin
        if Series[I].Active then with Series[I] do
        begin
          S1 := Title;
          for J := 0 to Count-1 do
          begin
          // Get node labels for profile plot
            if Graph.GraphType = PROFILEPLOT then
              GetMarkText(Series[I],J,S1);

          // Build up tab-delimited string of X, Y values
            S := Format('%-16s'+#9+'%-16.4f'+#9+'%-16.4f',
              [S1,XValues.Value[J],YValues.Value[J]]);

          // Add tab-delimited string to list
            Slist.Add(S);
          end;
        end;
      end;
    end;

  // Save stringlist to file if file name supplied
  // Otherwise place text of stringlist onto Clipboard
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)
    else Clipboard.SetTextBuf(PChar(Slist.Text));

// Free the stringlist.
  finally
    Slist.Free;
  end;
end;

procedure TGraphForm.Print(Destination: TDestination);
//-----------------------------------------------------------
// Prints the graph to Destination (printer or preview form).
//-----------------------------------------------------------
var
  w,h         : Single;
  Left,Top    : Single;
  Width,Height: Single;
  aPicture    : TPicture;
begin
  with PageLayout, MainForm.thePrinter do
  begin
    Left := LMargin;
    Width := GetPageWidth - LMargin - RMargin;
    Height := GetPageHeight - TMargin - BMargin;
  end;
  aPicture := TPicture.Create;
  with MainForm.thePrinter do
  try
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);
    Uutils.ChartToPicture(Chart1, aPicture);
    Uutils.FitChartToPage(Chart1, Width, Height, w, h);
    Top := GetYPos;
    Left := Left + (Width - w)/2;
    StretchGraphic(Left,Top,Left+w,Top+h,aPicture);
    EndJob;
  finally
    aPicture.Free;
  end;
end;

end.
