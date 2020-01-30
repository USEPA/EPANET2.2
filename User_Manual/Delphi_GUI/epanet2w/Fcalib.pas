unit Fcalib;

{-------------------------------------------------------------------}
{                    Unit:    Fcalib.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that displays a Calibration Report comparing     }
{   simulated results against observed data.                        }
{-------------------------------------------------------------------}

(*******************************************************************
    The form contains a PageControl with 3 Tabsheets:
      Tabsheet1 - displays calibration statistics
      Tabsheet2 - scatter plot of observed v. simulated values.
      Tabsheet3 - bar chart of mean simulated and observed values

    The report is created by calling CreateCalibReport() and is
    refreshed by calling RefreshCalibReport().

    The form's procedures are divided into the following sections:
      Control Procedures
      Event Handlers
      Statistics Procedures
      Data Retrieval Procedures
      Display Procedures
      Calibration Error Procedures
      Copying & Printing

    The calling sequence used to create and refresh a report is:
      CreateCalibReport
      |
      |-- RefreshCalibReport
          |
          |-- DisplayTitle
          |-- GetCalibErrors
          |   |
          |   |-- GetTokens
          |   |-- GetData
          |   |-- GetTimeSeries
          |   |-- UpdateErrorStats
          |   |-- DisplayLocationStats
          |       |
          |       |-- SumsToStats
          |       |-- DisplayStats
          |
          |-- DisplayNetworkStats
          |-- DisplayPlots
********************************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Checklst, Clipbrd, System.UITypes,
  VCLTee.Chart, VCLTee.Series, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.TeCanvas,
  Xprinter, Uglobals, Uutils, VclTee.TeeGDIPlus;

const
  NSUMS = 8;
  TXT_REPORT = 'Calibration Report - ';
  TXT_NETWORK = 'Network';
  TXT_NO_DATA = ' *** No observed data during simulation period. ***';
  TXT_CORRELATION = '  Correlation Between Means: ';
  TXT_TITLE = ' Calibration Statistics for ';
  TXT_HEADING1 =
  '                Num    Observed    Computed    Mean     RMS';
  TXT_HEADING2 =
  '  Location      Obs        Mean        Mean   Error   Error';
  TXT_HEADING3 =
  '  ---------------------------------------------------------';

  MarkerColors : array[0..14] of TColor =
    (clBlack, clRed, clPurple, clLime, clBlue, clFuchsia, clAqua,
     clGray, clGreen, clMaroon, clYellow, clTeal, clSilver, clNavy,
     clOlive);

type
  TCalibReportForm = class(TForm)
    PageControl1: TPageControl;
      TabSheet1: TTabSheet;
        Memo1: TMemo;
      TabSheet2: TTabSheet;
        Chart1: TChart;
      TabSheet3: TTabSheet;
        Chart2: TChart;
        Series3: TBarSeries;
        Series4: TBarSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    RptType : Integer;
    RptVar  : Integer;
    procedure CopyToBitmap(const Fname: String);
    procedure CopyToMetafile(const Fname: String);
    procedure CopyToString(const Fname: String);
    procedure DisplayLocationStats(const ID: String;
      const Sums: array of Single; var NetSums: array of Single;
      var MeanSums: array of Single);
    procedure DisplayNetworkStats(var NetSums: array of Single;
      var MeanSums: array of Single);
    procedure DisplayPlots(const Vname: String);
    procedure DisplayStats(const s: String; const stats: array of Single);
    procedure DisplayTitle(const Vname: String);
    procedure FreeChart1Series;
    procedure GetCalibErrors(CalibData: TCalibData;
      var Netsums: array of Single; var Meansums: array of Single);
    function  GetTimeSeries(const ID: String): Boolean;
    procedure UpdateErrorStats(const aTime: String; const aValue: String;
      var Sums: array of Single);
  public
    { Public declarations }
    procedure CopyTo;
    procedure CreateCalibReport(const Rtype: Integer; const Rvar: Integer);
    procedure Print(Destination: TDestination);
    procedure RefreshCalibReport;
    procedure SetCalibOptions;
  end;

//var
//  CalibReportForm: TCalibReportForm;

implementation

{$R *.DFM}

uses
  Dcalib2, Dcopy, FBrowser, Fmain, Uinput, Uoutput;

var
  ColorIndex : Integer;
  N1, N2     : Integer;        // 1st & last simulation time period
  Nsim       : Integer;        // number of time periods
  Tsim       : PLongintArray;  // time at start of each period (sec)
  Vsim       : PSingleArray;   // simulated value at each period
  Sigma      : Single;         // std. dev. of measurement error


//===============================================================
//                       Control Procedures
//===============================================================

procedure TCalibReportForm.CreateCalibReport(const Rtype: Integer;
  const Rvar: Integer);
//------------------------------------------------------
// Produces a Calibration Error report for variable Rvar
// of type Rtype (NETNODES or NETLINKS).
//------------------------------------------------------
begin
  RptType := Rtype;
  RptVar := Rvar;
  RefreshCalibReport;
end;


procedure TCalibReportForm.RefreshCalibReport;
//--------------------------------------------
// Refreshes contents of Calibration Report.
//--------------------------------------------
var
  vname        : String;
  j            : Integer;
  meansums     : array [0 .. NSUMS] of Single;
  netsums      : array [0 .. NSUMS] of Single;
  CalibData    : TCalibData;
begin
//Clear report components
  Memo1.Clear;
  FreeChart1Series;
  Chart2.Series[0].Clear;
  Chart2.Series[1].Clear;

//Save information about variable being calibrated
  if RptType = NETNODES then
  begin
    CalibData := NodeCalibData[RptVar];
    if RptVar = NODEQUAL then
      vname := BrowserForm.NodeViewBox.Items[NODEQUAL]
    else
      vname := NodeVariable[RptVar].Name;
  end
  else
  begin
    CalibData := LinkCalibData[RptVar];
    vname := LinkVariable[RptVar].Name;
  end;

//Display form's title caption & exit if no
//simulated results exist
  Sigma := (CalibData.MeasError)/100;
  DisplayTitle(vname);
  if not RunFlag or not FileExists(CalibData.FileName) then Exit;

//Initialize arrays for simulation results
  N1 := 0;
  N2 := Nperiods - 1;
  Nsim  := N2 - N1 + 1;
  GetMem(Tsim, Nsim*SizeOf(Longint));
  GetMem(Vsim, Nsim*SizeOf(Single));
  for j := 0 to Nsim-1 do
    Tsim^[j] := Rstart + j*Rstep;

//Initialize network statistics
  for j := 1 to NSUMS do
  begin
    netsums[j] := 0.0;
    meansums[j] := 0.0;
  end;

//Compute errors between simulated & observed values
  GetCalibErrors(CalibData, netsums, meansums);

//Free allocated memory
  FreeMem(Tsim,Nsim*SizeOf(Longint));
  FreeMem(Vsim,Nsim*SizeOf(Single));

//Display network statistics calibration plots
  DisplayNetworkStats(netsums, meansums);
  DisplayPlots(vname);
end;


procedure TCalibReportForm.SetCalibOptions;
//-------------------------------------------
// Launches dialog that allows user to change
// Calibration Report options.
//-------------------------------------------
var
  TmpRptType: Integer;
  TmpRptVar:  Integer;
begin
  TmpRptType := NONE;
  with TCalibOptionsForm.Create(self) do
  try
    if ShowModal = mrOK then GetOptions(TmpRptType, TmpRptVar);
  finally
    Free;
  end;
  if (TmpRptType <> NONE) then
    CreateCalibReport(TmpRptType, TmpRptVar);
end;


//===============================================================
//                        Event Handlers
//===============================================================

procedure TCalibReportForm.FormCreate(Sender: TObject);
//---------------------------------------------------
// OnCreate handler for form.
//---------------------------------------------------
begin
  Uglobals.SetFont(self);
  Memo1.Font.Style := Font.Style;
  PageControl1.ActivePage := TabSheet1;
end;


procedure TCalibReportForm.FormActivate(Sender: TObject);
//-------------------------------------------------------
// OnActivate handler for form.
// Enables Options speedbutton on MainForm.
//-------------------------------------------------------
begin
  MainForm.TBOptions.Enabled := True;
end;


procedure TCalibReportForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
//---------------------------------------------------
// OnClose handler for form.
// Frees the form and its allocated memory.
//---------------------------------------------------
begin
  Action := caFree;
end;


procedure TCalibReportForm.FormDestroy(Sender: TObject);
//------------------------------------------------------
// OnDestroy handler for form.
// Frees data series allocated to Chart1.
//------------------------------------------------------
begin
  FreeChart1Series;
end;


procedure TCalibReportForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  HC: Integer;
begin
  if Key = vk_F1 then
  begin
    case PageControl1.ActivePageIndex of
      0: HC := 270;
      1: HC := 271;
      2: HC := 272;
      else HC := 0;
    end;
    HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, HC);
  end;
end;

procedure TCalibReportForm.FreeChart1Series;
begin
  with Chart1 do
    while SeriesCount > 0 do Series[0].Free;
end;


//===============================================================
//                     Statistics Procedures
//===============================================================

procedure SumsToStats(const s: array of Single; var stats: array of Single);
//------------------------------------------------
// Computes error statistics from cumulative sums.
//------------------------------------------------
begin
  stats[1] := s[1];                  {# observations }
  if (s[1] > 0) then
  begin
    stats[2] := s[2]/s[1];           {observed mean }
    stats[3] := s[3]/s[1];           {simulated mean }
    stats[4] := s[4]/s[1];           {mean error }
    stats[5] := Sqrt(s[5]/s[1]);     {RMS error}
  end;
end;


procedure UpdateMeanSums(const stats: array of Single;
  var meansums: array of Single);
//---------------------------------------------------
// Udates cumulative sums used for correlation coeff.
//---------------------------------------------------
begin
  meansums[1] := meansums[1] + 1;
  meansums[2] := meansums[2] + stats[2];
  meansums[3] := meansums[3] + stats[3];
  meansums[4] := meansums[4] + stats[2]*stats[2];
  meansums[5] := meansums[5] + stats[3]*stats[3];
  meansums[6] := meansums[6] + stats[2]*stats[3];
end;


function Rcoeff(const s: array of Single): Single;
//-----------------------------------------------------
//  Finds correlation coefficient between X & Y where:
//    s[1] = # values
//    s[2] = Sum of X
//    s[3] = Sum of Y
//    s[4] = Sum of X*X
//    s[5] = Sum of Y*Y
//    s[6] = Sum of X*Y
//-----------------------------------------------------
var
  t1, t2, t3, t4 : Single;
begin
  t1 := s[1]*s[4] - s[2]*s[2];
  t2 := s[1]*s[5] - s[3]*s[3];
  t3 := s[1]*s[6] - s[2]*s[3];
  t4 := t1*t2;
  if t4 <= 0 then
    Rcoeff := 0.0
  else
    Rcoeff := t3/Sqrt(t4);
end;


//===============================================================
//                          Data Retrieval
//===============================================================

procedure GetTokens(var F: TextFile; var Ntoks: Integer;
  TokList: TStringList);
//------------------------------------------------------
// Tokenizes a line read from the calibration data file.
//------------------------------------------------------
var
  Line: String;
  S   : String;
  p   : Integer;
begin
//Read line & strip off any comment
  Readln(F,Line);
  S := Line;
  p := Pos(';',S);
  if (p > 0) then Delete(S,p,256);

//Tokenize line
  Uutils.Tokenize(S,Toklist,Ntoks);
end;


function GetData(const Ntoks: Integer; Toklist: TStringList;
  const OldID: String; var NewID: String; var aTime: String;
  var aValue: String): Boolean;
//-------------------------------------------------------------------
// Parses string tokens for Location ID, Measurement Time, and Value.
//-------------------------------------------------------------------
begin
  if (Ntoks >= 2) and (Ntoks <= 3) then
  begin

  //If only 2 tokens then use previous location ID
    if (Ntoks = 2) then
    begin
      NewID := OldID;
      aTime := Toklist[0];
      aValue := Toklist[1];
    end
    else

  //Otherwise retrieve new location ID
    begin
      NewID := Toklist[0];
      aTime := Toklist[1];
      aValue := Toklist[2];
    end;
    Result := True;
  end
  else Result := False;
end;


function TCalibReportForm.GetTimeSeries(const ID: String): Boolean;
//-------------------------------------------------
// Retrieves simulated time series for location ID.
//-------------------------------------------------
var
  i,j,k: Integer;
begin
  Result := False;

// Report is for a node variable
  if (RptType = NETNODES) then
  begin

  // Check that node exists
    if not Uinput.FindNode(ID,i,j) then Exit;

  // Get index (k) of node in output data listing
    k := Node(i,j).Zindex;
    if (k < 0) then Exit;

  // Retrieve output data for calib. variable with index j
    j := NodeVariable[RptVar].SourceIndex[JUNCS];
    Uoutput.GetNodeSeries(k, j, N1, N2, Vsim);
  end

// Report is for a link variable
  else
  begin

  // Check that link exists
    if not Uinput.FindLink(ID,i,j) then Exit;

  // Get index (k) of link in output data listing
    k := Link(i,j).Zindex;
    if (k < 0) then Exit;

  // Retrieve output data for calib. variable with index j
    j := LinkVariable[RptVar].SourceIndex[PIPES];
    Uoutput.GetLinkSeries(k, j, N1, N2, Vsim);

  // If flow is calib. variable, use absolute values
    if (RptVar = FLOW) then for j := N1 to N2 - 1 do
      Vsim^[j] := Abs(Vsim^[j]);
  end;
  Result := True;
end;


//===============================================================
//                       Display Procedures
//===============================================================

procedure TCalibReportForm.DisplayTitle(const Vname: String);
begin
  Caption := TXT_REPORT + Vname;
  with Memo1.Lines do
  begin
    Add(TXT_TITLE + Vname);
    Add('');
    Add(TXT_HEADING1);
    Add(TXT_HEADING2);
    Add(TXT_HEADING3);
  end;
end;


procedure TCalibReportForm.DisplayLocationStats(const ID: String;
  const Sums: array of Single; var NetSums: array of Single;
  var MeanSums: array of Single);
//----------------------------------------------------
// Displays error statistics for a specified location.
//----------------------------------------------------
var
  stats: array [0 .. NSUMS] of Single;
  j    : Integer;
begin
//If there are any data, then
  if Sums[1] > 0 then
  begin

  //Update network summary statistics
    for j := 1 to NSUMS do NetSums[j] := NetSums[j] + Sums[j];

  //Convert sums to statistics
    SumsToStats(Sums, stats);
    UpdateMeanSums(stats, MeanSums);

  //Display stats for this location
    DisplayStats(ID, stats);

  //Add obs. & simul. means to Mean Comparison chart
    Chart2.Series[0].Add(stats[3],ID,clTeeColor);
    Chart2.Series[1].Add(stats[2],ID,clTeeColor);
  end;
end;


procedure TCalibReportForm.DisplayNetworkStats(var NetSums: array of Single;
  var MeanSums: array of Single);
//------------------------------------------------------------------
// Displays overall calibration error statistics for entire network.
//------------------------------------------------------------------
var
  stats: array[0..NSUMS] of Single;
  r    : Single;
begin
  with Memo1.Lines do
  begin

  //Check if any calibration data exists.
    if NetSums[1] = 0 then
    begin
      Add('');
      Add(TXT_NO_DATA);
    end
    else
    begin

    //Data exists. Convert sums to statistics and display.
      SumsToStats(NetSums, stats);
      Add(TXT_HEADING3);
      DisplayStats(TXT_NETWORK,stats);
      Add('');

    //Compute correlation coeff. between obs. and simul.
    //mean values at each location.
      r := Rcoeff(MeanSums);
      Add(TXT_CORRELATION + Format('%.3f',[r]));
    end;
  end;
end;


procedure TCalibReportForm.DisplayStats(const s: String;
  const stats: array of Single);
//------------------------------------------------
// Displays error statistics for a given location.
//------------------------------------------------
begin
  with Memo1.Lines do

{*** Updated 6/24/02 ***}
    Add(Format('  %-14s%3.0f%12.2f%12.2f%8.3f%8.3f',

      [s,stats[1],stats[2],stats[3],stats[4],stats[5]]));
end;


procedure TCalibReportForm.DisplayPlots(const Vname: String);
//-----------------------------------------------------
// Displays correlation plot and mean comparison chart.
//-----------------------------------------------------
var
  x1,x2: Double;
  y1,y2: Double;
  z1,z2: Double;
  aSeries: TFastLineSeries;
begin
  with Chart1 do
  begin

  //Find coordinates of perfect correlation line
    Title.Text[0] := 'Correlation Plot for ' + Vname;
    x1 := MinXValue(BottomAxis);
    x2 := MaxXValue(BottomAxis);
    y1 := MinYValue(LeftAxis);
    y2 := MaxYValue(LeftAxis);
    if (x1 < y1) then y1 := x1
    else x1 := y1;
    if (x2 > y2) then y2 := x2
    else x2 := y2;
    z1 := 0.1*(x2-x1);
    z2 := 0.1*(y2-y1);

  //Create FastLineSeries for perfect correlation line
    aSeries := TFastLineSeries.Create(self);
    with aSeries do
    try
      ParentChart := Chart1;
      Title := '';
      ShowInLegend := False;
      SeriesColor := clBlack;
      AddXY((x1-z1),(y1-z2),'',clTeeColor);
      AddXY((x2+z1),(y2+z2),'',clTeeColor);
    finally
    end;
    Visible := True;
  end;
  with Chart2 do
  begin
    Title.Text[0] := 'Comparison of Mean Values for ' + Vname;
    Visible := True;
  end;
end;


//===============================================================
//                      Calibration Errors
//===============================================================

procedure TCalibReportForm.GetCalibErrors(CalibData: TCalibData;
  var Netsums: array of Single; var Meansums: array of Single);
//-----------------------------------------------------------
// Computes differences between simulated and observed values
// over entire simulation period.
//-----------------------------------------------------------
var
  IDflag       : Boolean;
  j            : Integer;
  ntoks        : Integer;
  F            : TextFile;
  sums         : array [0 .. NSUMS] of Single;
  oldID        : String;
  newID        : String;
  atime        : String;
  avalue       : String;
  aSeries      : TPointSeries;
  toklist      : TStringList;

begin
//Initialize values
  ColorIndex := Low(MarkerColors) - 1;
  IDflag := False;
  oldID := '';
  sums[1] := 0;

//Open calibration data file
  AssignFile(F,CalibData.FileName);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult = 0 then
  begin

  //Create a list of string tokens
    toklist := TStringList.Create;
    try

    //Read data from each line of the file (Location ID, Time, Value)
      while not EOF(F) do
      begin
        GetTokens(F,ntoks,toklist);
        if (GetData(ntoks,toklist,oldID,newID,atime,avalue)) then
        begin

        //Line begins data for a new location
          if (newID <> oldID) then
          begin

          //Report statistics for previous location
            if (sums[1] > 0) then
              DisplayLocationStats(oldID, sums, Netsums, Meansums);
            oldID := newID;

          //Initialize statistics for new location
            for j := 1 to NSUMS do sums[j] := 0.0;

          //Check if new location belongs in the report
            if (CalibData.Locations.Values[TokList[0]] <> '1') then
              IDflag := False
            else
            begin
            //Update ColorIndex of markers for this location
            //used in the correlation plot
              Inc(ColorIndex);
              if (ColorIndex > High(MarkerColors)) then
                ColorIndex := Low(MarkerColors);

            //Create new PointSeries for correlation plot
              aSeries := TPointSeries.Create(self);
              with aSeries do
              try
                ParentChart := Chart1;
                Title := newID;
                ShowInLegend := True;
                Pointer.Visible := True;
                Pointer.Style := psCross;
                Pointer.Pen.Width := 2;
                Pointer.Pen.Color := MarkerColors[ColorIndex];
                SeriesColor := MarkerColors[ColorIndex];
              finally
              end;

            //Get simulation results for new location
              IDflag := GetTimeSeries(newID);
            end;
          end;

        //Compute error & update statistics for observed datum
          if (IDflag) then UpdateErrorStats(atime,avalue,sums);
        end;

      end; //Next line of file

    //Report statistics for last location
      if (sums[1] > 0) then
        DisplayLocationStats(oldID, sums, Netsums, Meansums);

    finally
      toklist.Free;
    end;  { Try-Finally block }

  end;    { If IOResult block }
  CloseFile(F);
end;


procedure TCalibReportForm.UpdateErrorStats(const aTime: String;
  const aValue: String; var Sums: array of Single);
//-------------------------------------------------
// Updates error statistics at a specific location.
//-------------------------------------------------
var
  code  : Integer;
  j1, j2: Longint;  //Time periods
  tsec  : Longint;  //Time (in seconds)
  t     : Single;   //Time (in hours)
  v, v1 : Single;   //Obs. & simul. values
  e     : Single;   //Error
begin
//Convert observed data to numbers
  t := Uutils.StrHoursToFloat(aTime);
  if (t < 0) then Exit;
  val(aValue,v,code);
  if (code <> 0) then Exit;
  tsec := Round(3600.0*t);

//Get simulated value at time tsec
//(Interpolate if tsec falls between time periods)
  v1 := -1.;
  if Nsim = 1 then
    v1 := Vsim^[0]
  else if (tsec >= Rstart) and (tsec <= Dur) then
  begin
    j1 := (tsec - Rstart) div Rstep;
    j2 := j1 + 1;
    if (j1 >= 0) and (j2 < Nsim) then
      v1 := Vsim^[j1] + (tsec - Tsim^[j1])/(Tsim^[j2] - Tsim^[j1])*
            (Vsim^[j2] - Vsim^[j1]);
  end;

//Add obs. & simul. values to correlation plot
//and update error statistics
  if v1 >= 0.0 then
  begin
    with Chart1 do
      Series[SeriesCount-1].AddXY(v, v1, '', clTeeColor);
    e := v1 - v;
    Sums[1] := Sums[1] + 1;
    Sums[2] := Sums[2] + v;
    Sums[3] := Sums[3] + v1;
    Sums[4] := Sums[4] + Abs(e);
    Sums[5] := Sums[5] + e*e;
    if (Sigma > 0) and (v <> 0.0) then
    begin
      Sums[7] := Sums[7] + 1;
      e := e/v;
      Sums[8] := Sums[8] + e*e;
    end;
  end;
end;


//===============================================================
//                      Copying & Printing 
//===============================================================

procedure TCalibReportForm.CopyTo;
//----------------------------------------------
// Launches CopyTo dialog to copy contents of
// current report page to clipboard or to file.
//----------------------------------------------
begin
  with TCopyToForm.Create(self) do
  try

  // If on statistics page, then format to
  // be copied must be text.
    if PageControl1.ActivePage = TabSheet1 then
    begin
      FormatGroup.ItemIndex := 2;
      FormatGroup.Enabled := False;
    end;

  // Copy contents of page in selected format
    if ShowModal = mrOK then
    begin
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


procedure TCalibReportForm.CopyToBitmap(const Fname: String);
//-----------------------------------------------------------
// Copies current chart to a bitmap.
//-----------------------------------------------------------
var
  theChart: TChart;
begin
// Determine which chart to copy
  if PageControl1.ActivePage = Tabsheet2 then
    theChart := Chart1
  else
    theChart := Chart2;

// Copy to file if file name supplied, otherwise copy to clipboard
  if Length(Fname) > 0 then
    theChart.SaveToBitmapFile(Fname)
  else
    theChart.CopyToClipboardBitmap;
end;


procedure TCalibReportForm.CopyToMetafile(const Fname: String);
//-----------------------------------------------------------
// Copies current chart to a metafile.
//-----------------------------------------------------------
var
  theChart: TChart;
begin
// Determine which chart to copy
  if PageControl1.ActivePage = Tabsheet2 then
    theChart := Chart1
  else
    theChart := Chart2;

// Copy to file if file name supplied, otherwise copy to clipboard
  if Length(Fname) > 0 then
    theChart.SaveToMetafileEnh(Fname)
  else
    theChart.CopyToClipBoardMetaFile(True);
end;


procedure TCalibReportForm.CopyToString(const Fname: String);
//-----------------------------------------------------------
// Copies data behind active page of the Calibration Report
// to file or to the clipboard.
//-----------------------------------------------------------
var
  Slist: TStringlist;
  I,J: Integer;
begin
// Create a stringlist to hold the data
  Slist := TStringList.Create;
  try

  // Add project's title to stringlist
    Slist.Add(Network.Options.Title);

  // Add contents of calibration statistics table to list
    if PageControl1.ActivePage = TabSheet1 then
      Slist.AddStrings(Memo1.Lines)

  // Add data from Chart1 (correlation plot) to list
    else if PageControl1.ActivePage = TabSheet2 then with Chart1 do
    begin
      Slist.Add(Title.Text[0]);
      Slist.Add('Location' + #9 + BottomAxis.Title.Caption +
        #9 + LeftAxis.Title.Caption);
      for I := 0 to SeriesCount - 2 do
      begin
        if Series[I].Active then with Series[I] do
        begin
          for J := 0 to Count-1 do
            Slist.Add(Title + #9 + FloatToStr(XValues.Value[J]) + #9 +
               FloatToStr(YValues.Value[J]));
        end;
      end;
    end

  // Add data from Chart2 (mean comparisons) to list
    else if PageControl1.ActivePage = TabSheet3 then with Chart2 do
    begin
      Slist.Add(Title.Text[0]);
      Slist.Add('Location' + #9 + 'Computed' + #9 + 'Observed');
      for J := 0 to Series[0].Count-1 do
        Slist.Add(Series[0].Xlabel[J] + #9 +
                  FloatToStr(Series[0].YValues.Value[J]) + #9 +
                  FloatToStr(Series[1].YValues.Value[J]));
    end;

  // Save list to file if file name supplied, otherwise save to clipboard
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)
    else Clipboard.SetTextBuf(PChar(Slist.Text));

// Free the stringlist.
  finally
    Slist.Free;
  end;
end;


procedure TCalibReportForm.Print(Destination: TDestination);
//------------------------------------------------------------------
// Prints current page of Calibration Report using thePrinter object.
//------------------------------------------------------------------
var
  i           : Integer;
  w,h         : Single;
  Left,Top    : Single;
  Width,Height: Single;
  aPicture    : TPicture;
begin
  aPicture := TPicture.Create;
  with MainForm.thePrinter, PageLayout do
  try

  // Get width & height of printing area
    Width := GetPageWidth - LMargin - RMargin;
    Height := GetPageHeight - TMargin - BMargin;

  // Begin job on thePrinter's destination (printer or print preview)
    BeginJob;
    SetDestination(Destination);

  // Print calibration statistics table
    if PageControl1.ActivePage = TabSheet1 then
    begin
      with Memo1.Font do
        SetFontInformation(Name, Size, Style);
      with Memo1 do
        for i := 0 to Lines.Count - 1 do
          PrintLine(Lines[i]);
    end

  // Print Chart1 centered on page
    else if PageControl1.ActivePage = TabSheet2 then
    begin
      Uutils.ChartToPicture(Chart1, aPicture);
      Uutils.FitChartToPage(Chart1, Width, Height, w, h);
      Top := GetYPos;
      Left := LMargin + (Width - w)/2;
      StretchGraphic(Left, Top, Left+w, Top+h, aPicture);
    end

// Print Chart2 centered on page
    else if PageControl1.ActivePage = TabSheet3 then
    begin
      Uutils.ChartToPicture(Chart2, aPicture);
      Uutils.FitChartToPage(Chart2, Width, Height, w, h);
      Top := GetYPos;
      Left := LMargin + (Width - w)/2;
      StretchGraphic(Left,Top,Left+w,Top+h,aPicture);
    end;
    EndJob;
  finally
    aPicture.Free;
  end;
end;

end.
