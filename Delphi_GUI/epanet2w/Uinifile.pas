unit Uinifile;

{-------------------------------------------------------------------}
{                    Unit:    Uinifile.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that reads and writes data to the EPANET2W   }
{   INI file (epanet2.ini).                                         }
{-------------------------------------------------------------------}

interface

uses Dialogs, Classes, SysUtils, Forms, Controls, Windows, IniFiles,
     Graphics, FileCtrl, Uglobals, Uutils;

procedure ReadIniFile;
procedure SaveIniFile;
procedure ReadDefaults;
procedure SaveDefaults;
procedure ReadMainFormSize;
procedure SaveMainFormSize;
procedure ReadStyleName;

implementation

uses Fmain, Fproped;


procedure InitMapOptions;
//---------------------------------------------------
// Asigns factory defaults to map options
//---------------------------------------------------
var
  i: Integer;
begin
// Assign factory defaults to map options
  MapOptions := DefMapOptions;

// Assign factory defaults to legend colors
  for i := 0 to MAXINTERVALS do
  begin
    MapNodeColor[i] := DefLegendColor[i];
    MapLinkColor[i] := DefLegendColor[i];
  end;
end;


procedure InitLegends;
//---------------------------------------------------
// Asigns factory defaults to map legend intervals
//---------------------------------------------------
var
  i,j: Integer;
begin
// Assign factory defaults to legend intervals for node variables
  for i := ELEVATION to NODEVIEWS do
  begin
    with NodeLegend[i] do
    begin
      Nintervals := MAXINTERVALS;
      ViewVar := i;
      LType := NETNODES;
      for j := 1 to Nintervals do
        Intervals[j] := NodeVariable[i].DefIntervals[j];
    end;
  end;

// Assign factory defaults to legend intervals for link variables
  for i := LINKLENGTH to LINKVIEWS do
  begin
    with LinkLegend[i] do
    begin
      Nintervals := MAXINTERVALS;
      ViewVar := i;
      LType := NETLINKS;
      for j := 1 to Nintervals do
        Intervals[j] := LinkVariable[i].DefIntervals[j];
    end;
  end;
end;


procedure ReadIniFile;
//--------------------------------------------------------------
// Reads map settings, program preferences, current directories,
// and most-recently-used file list from the .INI file.
//--------------------------------------------------------------
var
  i       : Integer;
  s       : String;
  fname   : String;
{ dname   : String;  DEPRECATED  }
begin
// Initialize graph options with factory settings
  GraphOptions := DefGraphOptions;

// Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try

  // Retrieve Graph Options
    with GraphOptions do
    begin
      View3D := ReadBool('Graph','View3D',View3D);
      Percent3D := ReadInteger('Graph','Percent3D',Percent3D);
      PanelColor := ReadInteger('Graph','PanelColor',PanelColor);
      BackColor := ReadInteger('Graph','BackColor',BackColor);
      LegendPosition := ReadInteger('Graph','LegendPosition',LegendPosition);
      LegendColor := ReadInteger('Graph','LegendColor',LegendColor);
      LegendWidth := ReadInteger('Graph','LegendWidth',LegendWidth);
      LegendFramed := ReadBool('Graph','LegendFramed',LegendFramed);
      LegendVisible := ReadBool('Graph','LegendVisible',LegendVisible);
      AxisGridStyle[0] := ReadInteger('Graph','X-AxisGrid',AxisGridStyle[0]);
      AxisGridStyle[1] := ReadInteger('Graph','Y-AxisGrid',AxisGridStyle[1]);
      for i := 0 to MAXSERIES do
      begin
        s := IntToStr(i);
        LineStyle[i] := ReadInteger('Graph','LineStyle'+s,LineStyle[i]);
        LineColor[i] := ReadInteger('Graph','LineColor'+s,LineColor[i]);
        LineWidth[i] := ReadInteger('Graph','LineWidth'+s,LineWidth[i]);
        PointVisible[i] := ReadBool('Graph','PointVisible'+s,PointVisible[i]);
        PointStyle[i] := ReadInteger('Graph','PointStyle'+s,PointStyle[i]);
        PointColor[i] := ReadInteger('Graph','PointColor'+s,PointColor[i]);
        PointSize[i] := ReadInteger('Graph','PointSize'+s,PointSize[i]);
      end;
      TitleFontColor := ReadInteger('Graph','TitleFontColor',TitleFontColor);
      TitleFontName := ReadString('Graph','TitleFontName',TitleFontName);
      TitleFontSize := ReadInteger('Graph','TitleFontSize',TitleFontSize);
      TitleFontBold := ReadBool('Graph','TitleFontBold',TitleFontBold);
      AxisFontName := ReadString('Graph','AxisFontName',AxisFontName);
      AxisFontSize := ReadInteger('Graph','AxisFontSize',AxisFontSize);
      AxisFontBold := ReadBool('Graph','AxisFontBold',AxisFontBold);
      AreaFillColor := ReadInteger('Graph','AreaFillColor',AreaFillColor);
      AreaFillStyle := TBrushStyle(ReadInteger('Graph',
                          'AreaFillStyle',Ord(AreaFillStyle)));
      LabelsVisible := ReadBool('Graph','LabelsVisible',LabelsVisible);
      LabelsTransparent := ReadBool('Graph','LabelsTransparent',LabelsTransparent);
      LabelsArrows := ReadBool('Graph','LabelsArrows',LabelsArrows);
      LabelsBackColor := ReadInteger('Graph','LabelsBackColor',LabelsBackColor);
    end;

  // Retrieve directory names
    fname := ReadString('Directories','DataDir',EpanetDir);
    if (SysUtils.DirectoryExists(fname)) then SetCurrentDir(fname);

  // Retrieve general preferences
    FontName := ReadString('Preferences','FontName','Segoe UI'); //'Tahoma'); //'MS Sans Serif');
    BoldFonts := ReadBool('Preferences','BoldFonts',False);
    Blinking := ReadBool('Preferences','Blinking',True);
    FlyOvers := ReadBool('Preferences','FlyOvers',True);
    AutoBackup := ReadBool('Preferences','AutoBackup',False);
    ConfirmDelete := ReadBool('Preferences','ConfirmDelete',True);

  // Retrieve MRU file names
    for i := 0 to 3 do
      MainForm.MRUList.Add(ReadString('MRU',IntToStr(i),''));

  // Retrieve Property Editor parameters
    with PropEditForm do
    begin
      Width := ReadInteger('Property Editor','Width',Width);
      Height := ReadInteger('Property Editor','Height',Height);
      Editor.HeaderSplit := ReadInteger('Property Editor','HeaderSplit',
         Editor.HeaderSplit);
    end;

// Free the .INI file object
  finally
    Free;
  end;
end;


procedure SaveIniFile;
//--------------------------------------------------------------
// Saves map settings, program preferences, current directories,
// and most-recently-used file list to the .INI file.
//--------------------------------------------------------------
var
  i       : Integer;
  s       : String;
begin
// Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try

  // Save Graph options
    with GraphOptions do
    begin
      WriteBool('Graph','View3D',View3D);
      WriteInteger('Graph','Percent3D',Percent3D);
      WriteInteger('Graph','PanelColor',PanelColor);
      WriteInteger('Graph','BackColor',BackColor);
      WriteInteger('Graph','LegendPosition',LegendPosition);
      WriteInteger('Graph','LegendColor',LegendColor);
      WriteInteger('Graph','LegendWidth',LegendWidth);
      WriteBool('Graph','LegendFramed',LegendFramed);
      WriteBool('Graph','LegendVisible',LegendVisible);
      WriteInteger('Graph','X-AxisGrid',AxisGridStyle[0]);
      WriteInteger('Graph','Y-AxisGrid',AxisGridStyle[1]);
      for i := 0 to MAXSERIES do
      begin
        s := IntToStr(i);
        WriteInteger('Graph','LineStyle'+s,LineStyle[i]);
        WriteInteger('Graph','LineColor'+s,LineColor[i]);
        WriteInteger('Graph','LineWidth'+s,LineWidth[i]);
        WriteBool('Graph','PointVisible'+s,PointVisible[i]);
        WriteInteger('Graph','PointStyle'+s,PointStyle[i]);
        WriteInteger('Graph','PointColor'+s,PointColor[i]);
        WriteInteger('Graph','PointSize'+s,PointSize[i]);
      end;
      WriteInteger('Graph','TitleFontColor',TitleFontColor);
      WriteString('Graph','TitleFontName',TitleFontName);
      WriteInteger('Graph','TitleFontSize',TitleFontSize);
      WriteBool('Graph','TitleFontBold',TitleFontBold);
      WriteString('Graph','AxisFontName',AxisFontName);
      WriteInteger('Graph','AxisFontSize',AxisFontSize);
      WriteBool('Graph','AxisFontBold',AxisFontBold);
      WriteInteger('Graph','AreaFillColor',AreaFillColor);
      WriteInteger('Graph','AreaFillStyle',Ord(AreaFillStyle));
      WriteBool('Graph','LabelsVisible',LabelsVisible);
      WriteBool('Graph','LabelsTransparent',LabelsTransparent);
      WriteBool('Graph','LabelsArrows',LabelsArrows);
      WriteInteger('Graph','LabelsBackColor',LabelsBackColor);
    end;

  // Save directory names
    GetDir(0,s);
    WriteString('Directories','DataDir',s);

  // Save general program preferences
    WriteBool('Preferences','BoldFonts',BoldFonts);
    WriteBool('Preferences','Blinking',Blinking);
    WriteBool('Preferences','FlyOvers',FlyOvers);
    WriteBool('Preferences','AutoBackup',AutoBackup);
    WriteBool('Preferences','ConfirmDelete',ConfirmDelete);
    WriteString('Preferences', 'StyleName', StyleName);

  // Save MRU file names
    for i := 0 to 3 do WriteString('MRU',IntToStr(i),MainForm.MRUList[i]);

  // Save Property Editor parameters
    with PropEditForm do
    begin
      WriteInteger('Property Editor','Left',Left);
      WriteInteger('Property Editor','Top',Top);
      WriteInteger('Property Editor','Width',Width);
      WriteInteger('Property Editor','Height',Height);
      WriteInteger('Property Editor','HeaderSplit',Editor.HeaderSplit);
    end;

// Free the .INI file object
  finally
    Free;
  end;
end;


procedure ReadDefaults;
//----------------------------------------------------
// Initializes default object properties and reads in
// previously saved Project defaults from the INI file.
//----------------------------------------------------
var
  i: Integer;
begin
// Use factory settings for map options & legend intervals
  InitMapOptions;
  InitLegends;

// Copy factory defaults (e.g., DefJunc)
// to current defaults (e.g., DefProp[JUNCS].Data)
  Uutils.CopyStringArray(DefJunc,   DefProp[JUNCS].Data);
  Uutils.CopyStringArray(DefReserv, DefProp[RESERVS].Data);
  Uutils.CopyStringArray(DefTank,   DefProp[TANKS].Data);
  Uutils.CopyStringArray(DefPipe,   DefProp[PIPES].Data);
  Uutils.CopyStringArray(DefPump,   DefProp[PUMPS].Data);
  Uutils.CopyStringArray(DefValve,  DefProp[VALVES].Data);
  Uutils.CopyStringArray(DefOptions,Network.Options.Data);

// Assign factory defaults to ID label prefixes & increment
  IDIncrement := 1;
  for i := JUNCS to CNTRLS do IDPrefix[i] := '';

// Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try

  // Retrieve default ID labeling prefixes & increment
    IDIncrement := ReadInteger('Labels','Increment',1);
    for i := JUNCS to CNTRLS do
      IDPrefix[i] := ReadString('Labels',ObjectLabel[i],'');

  // Retrieve default hydraulic analysis options
    for i := FLOW_UNITS_INDEX to STATUS_RPT_INDEX do
      Network.Options.Data[i] := ReadString('Hydraulics',
        HydraulicProps[i].Name,Network.Options.Data[i]);

  // Retrieve specific default node properties
    DefProp[JUNCS].Data[JUNC_ELEV_INDEX] :=
      ReadString('Properties','Elevation',DefJunc[JUNC_ELEV_INDEX]);
    DefProp[RESERVS].Data[RES_HEAD_INDEX] :=
      DefProp[JUNCS].Data[JUNC_ELEV_INDEX];
    DefProp[TANKS].Data[TANK_ELEV_INDEX] :=
      DefProp[JUNCS].Data[JUNC_ELEV_INDEX];
    DefProp[TANKS].Data[TANK_DIAM_INDEX] :=
      ReadString('Properties','TankDiam',DefTank[TANK_DIAM_INDEX]);
    DefProp[TANKS].Data[TANK_MAXLVL_INDEX] :=
      ReadString('Properties','TankHeight',DefTank[TANK_MAXLVL_INDEX]);

  // Retrieve specific default link properties
    DefProp[PIPES].Data[PIPE_LEN_INDEX] :=
      ReadString('Properties','Length',DefPipe[PIPE_LEN_INDEX]);
    DefProp[PIPES].Data[PIPE_DIAM_INDEX] :=
      ReadString('Properties','PipeDiam',DefPipe[PIPE_DIAM_INDEX]);
    DefProp[PIPES].Data[PIPE_ROUGH_INDEX] :=
      ReadString('Properties','Roughness',DefPipe[PIPE_ROUGH_INDEX]);
    DefProp[VALVES].Data[VALVE_DIAM_INDEX] :=
      DefProp[PIPES].Data[PIPE_DIAM_INDEX];
    AutoLength := ReadBool('Properties','AutoLength',False);

// Free the .INI file object
  finally
    Free;
  end;
end;


procedure SaveDefaults;
//----------------------------------------
// Saves Project Defaults to the INI file.
//----------------------------------------
var
  i: Integer;
begin
// Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try

  // Save default ID labeling prefixes & increment
    WriteInteger('Labels','Increment',IDIncrement);
    for i := JUNCS to CNTRLS do
      WriteString('Labels',ObjectLabel[i],IDPrefix[i]);

  // Save default hydraulic analysis options
    for i := FLOW_UNITS_INDEX to STATUS_RPT_INDEX do
       WriteString('Hydraulics',HydraulicProps[i].Name,
         Network.Options.Data[i]);

  // Save specific default node properties
    WriteString('Properties','Elevation',DefProp[JUNCS].Data[JUNC_ELEV_INDEX]);
    WriteString('Properties','TankDiam',DefProp[TANKS].Data[TANK_DIAM_INDEX]);
    WriteString('Properties','TankHeight',
      DefProp[TANKS].Data[TANK_MAXLVL_INDEX]);

  // Save specific default link properties
    WriteString('Properties','Length',DefProp[PIPES].Data[PIPE_LEN_INDEX]);
    WriteString('Properties','PipeDiam',DefProp[PIPES].Data[PIPE_DIAM_INDEX]);
    WriteString('Properties','Roughness',DefProp[PIPES].Data[PIPE_ROUGH_INDEX]);
    WriteBool('Properties','AutoLength',AutoLength);

// Free the .INI file object
  finally
    Free;
  end;
end;


procedure ReadMainFormSize;
//------------------------------------
// Reads main form's position and size
//------------------------------------
var
  L,T,W,H: Integer;
  S: TWindowState;
begin
  with TIniFile.Create(IniFileDir + INIFILE) do
  try
    with MainForm do
    begin
      S := TWindowState(ReadInteger('MainForm','State',Ord(wsMaximized)));
      T := ReadInteger('MainForm','Top',Top);
      L := ReadInteger('MainForm','Left',Left);
      W := ReadInteger('MainForm','Width',Width);
      H := ReadInteger('MainForm','Height',Height);
      SetBounds(L,T,W,H);
      if S = wsMinimized then S := wsNormal;
      WindowState := S;
    end;
  finally
    Free;
  end;
end;


procedure SaveMainFormSize;
//------------------------------------
// Saves main form's position and size
//------------------------------------
var
  P: TWindowPlacement;
  R: TRect;
  S: Integer;
begin
  with TIniFile.Create(IniFileDir + INIFILE) do
  try
    with MainForm do
    begin
      P.length := SizeOf(TWindowPlacement);
      GetWindowPlacement(Handle,@P);
      R := P.rcNormalPosition;
      if IsIconic(Application.Handle) then S := Ord(wsNormal)
      else S := Ord(WindowState);
      WriteInteger('MainForm','State',S);
      WriteInteger('MainForm','Left',R.Left);
      WriteInteger('MainForm','Top',R.Top);
      WriteInteger('MainForm','Width',R.Right-R.Left);
      WriteInteger('MainForm','Height',R.Bottom-R.Top);
    end;
  finally
    Free;
  end;
end;


procedure ReadStyleName;
//-------------------------------------
// Reads the name of a UI style to use
//-------------------------------------
begin
  // Initialize UI style
  Uglobals.StyleName := 'Windows';

  // Create the .INI file object
  with TIniFile.Create(IniFileDir + INIFILE) do
  try
    Uglobals.StyleName := ReadString('Preferences', 'StyleName', 'Windows');

  // Free the .INI file object
  finally
    Free;
  end;
end;

end.
