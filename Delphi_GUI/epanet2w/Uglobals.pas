unit Uglobals;

{-------------------------------------------------------------------}
{                    Unit:    Uglobals.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that defines all global data types and       }
{   constants used by EPANET2W.                                     }
{-------------------------------------------------------------------}

interface

uses SysUtils, WinTypes, WinProcs, Forms, Messages, Classes, Graphics,
     Controls, Dialogs, Propedit, Uutils;

type

  TSinglePoint = record
    X : Single;
    Y : Single;
  end;

  TExtendedPoint = record
    X : Extended;
    Y : Extended;
  end;

  PVertex = ^TVertex;

  TVertex = record
    X    : Extended;
    Y    : Extended;
    Next : PVertex;
  end;

  TUnitSystem     = (usUS,usSI);
  TVariableSource = (vsInput, vsOutput);
  TRunStatus      = (rsSuccess, rsWarning, rsError, rsWrongVersion,
                     rsFailed, rsShutdown, rsCancelled, rsNone);
  TContourStyle   = (csFilled, csLines);
  TArrowStyle     = (asNone, asOpen, asFilled, asFancy);
  TWaterQuality   = (wqNone, wqChem, wqTrace, wqAge);
  TMapUnits       = (muFeet, muMeters, muDegrees, muNone);
  TInputFileType  = (iftNone, iftNET, iftINP);

const

//------------------
// Version ID
//------------------
  VERSIONID1 = 20005;
  VERSIONID2 = 20201;

//------------------
// Maximum limits
//------------------
  MAXID        = 31; //Max. chars. in ID label
  MAXINTERVALS = 4;  //Max. color scale interval index
  MAXNODEPROPS = 26; //Max. index for node property array
  MAXLINKPROPS = 25; //Max. index for link property array
  MAXOPTIONS   = 44; //Max. index for network options array
  MAXSERIES    = 5;  //Max. time series plots per graph
  MAXCOLS      = 15; //Max. columns in a table
  MAXFILTERS   = 10; //Max. filter conditions for table
  MAXQUALPARAMS = 3; //Max. types of WQ analyses
  MAXDEGDIGITS  = 4; //Max. decimal digits for lat-long degrees

//---------------
// Custom cursors
//---------------
  crXHAIR    = 1;
  crZOOMIN   = 2;
  crZOOMOUT  = 3;
  crFIST     = 4;
  crMOVE     = 5;
  crPENCIL   = 6;
  crARROWTIP = 7;

//------------------
// Object categories
//------------------
  JUNCS    = 0;
  RESERVS  = 1;
  TANKS    = 2;
  PIPES    = 3;
  PUMPS    = 4;
  VALVES   = 5;
  LABELS   = 6;
  PATTERNS = 7;
  CURVES   = 8;
  CNTRLS   = 9;
  OPTS     = 10;
  VERTICES = 11;

//-----------------
// Node variables
//-----------------
  ELEVATION   = 1;
  BASEDEMAND  = 2;
  INITQUAL    = 3;
  DEMAND      = 4;
  HEAD        = 5;
  PRESSURE    = 6;
  NODEQUAL    = 7;
  NODEVIEWS   = 7;   //Max. display variable index

//--------------------
// Link view variables
//--------------------
  LINKLENGTH  = 1;
  DIAMETER    = 2;
  ROUGHNESS   = 3;
  BULKCOEFF   = 4;
  WALLCOEFF   = 5;
  FLOW        = 6;
  VELOCITY    = 7;
  HEADLOSS    = 8;
  FRICTION    = 9;
  REACTRATE   = 10;
  LINKQUAL    = 11;
  LINKSTAT    = 12;
  LINKSET     = 13;
  LINKVIEWS   = 11;   //Max. display variable index

//-------------------------
// Generic property indexes
//-------------------------
  COMMENT_INDEX       = 0;  //Comment index
  TAG_INDEX           = 1;  //Tag index
  X_INDEX             = 1;  //Node's X-coord.
  Y_INDEX             = 2;  //Node's Y-coord.
  UP_INDEX            = 1;  //Link's upstream node
  DN_INDEX            = 2;  //Link's downstream node
  PROP_INDEX_OFFSET   = 3;  //Offset in Property Editor

//--------------------------
// Junction property indexes
//--------------------------
  JUNC_ELEV_INDEX     = 2;  //Elevation
  JUNC_DEMAND_INDEX   = 3;  //Demand
  JUNC_PATTERN_INDEX  = 4;  //Demand pattern
  JUNC_DMNDCAT_INDEX  = 5;  //Demand categories
  JUNC_EMITTER_INDEX  = 6;  //Emitter coeff.
  JUNC_INITQUAL_INDEX = 7;  //Init. quality
  JUNC_SRCQUAL_INDEX  = 8;  //Source quality
  JUNC_SRCPAT_INDEX   = 9;  //Source pattern
  JUNC_SRCTYPE_INDEX  = 10; //Source type

//---------------------------
// Reservoir property indexes
//---------------------------
  RES_HEAD_INDEX      = 2;  //Head
  RES_PATTERN_INDEX   = 3;  //Head pattern
  RES_INITQUAL_INDEX  = 4;  //Init. quality
  RES_SRCQUAL_INDEX   = 5;  //Source quality
  RES_SRCPAT_INDEX    = 6;  //Source pattern
  RES_SRCTYPE_INDEX   = 7;  //Source type

//----------------------
// Tank property indexes
//----------------------
  TANK_ELEV_INDEX     = 2;  //Elevation
  TANK_INITLVL_INDEX  = 3;  //Init. level
  TANK_MINLVL_INDEX   = 4;  //Min. level
  TANK_MAXLVL_INDEX   = 5;  //Max. level
  TANK_DIAM_INDEX     = 6;  //Diameter
  TANK_MINVOL_INDEX   = 7;  //Min. volume
  TANK_VCURVE_INDEX   = 8;  //Volume curve
  TANK_MIXMODEL_INDEX = 9;  //Mixing model
  TANK_MIXFRAC_INDEX  = 10; //Mixing fraction
  TANK_KBULK_INDEX    = 11; //Bulk coeff.
  TANK_INITQUAL_INDEX = 12; //Init. quality
  TANK_SRCQUAL_INDEX  = 13; //Source quality
  TANK_SRCPAT_INDEX   = 14; //Source pattern
  TANK_SRCTYPE_INDEX  = 15; //Source type
  TANK_OVERFLOW_INDEX = 16; //Overflow indicator

//----------------------
// Pipe property indexes
//----------------------
  PIPE_LEN_INDEX      = 2;  //Length
  PIPE_DIAM_INDEX     = 3;  //Diameter
  PIPE_ROUGH_INDEX    = 4;  //Roughness coeff.
  PIPE_MLOSS_INDEX    = 5;  //Minor loss coeff.
  PIPE_STATUS_INDEX   = 6;  //Status
  PIPE_KBULK_INDEX    = 7;  //Bulk coeff.
  PIPE_KWALL_INDEX    = 8;  //Wall coeff.

//----------------------
// Pump property indexes
//----------------------
  PUMP_HCURVE_INDEX   = 2;  //Head curve
  PUMP_HP_INDEX       = 3;  //Horsepower
  PUMP_SPEED_INDEX    = 4;  //Speed
  PUMP_PATTERN_INDEX  = 5;  //Speed pattern
  PUMP_STATUS_INDEX   = 6;  //Status
  PUMP_ECURVE_INDEX   = 7;  //Efficiency curve
  PUMP_EPRICE_INDEX   = 8;  //Energy price
  PUMP_PRICEPAT_INDEX = 9;  //Price pattern

//-----------------------
// Valve property indexes
//-----------------------
  VALVE_DIAM_INDEX    = 2;  //Diameter
  VALVE_TYPE_INDEX    = 3;  //Type
  VALVE_SETTING_INDEX = 4;  //Setting
  VALVE_MLOSS_INDEX   = 5;  //Minor loss coeff.
  VALVE_STATUS_INDEX  = 6;  //Status

//---------------------------
// Map Label property indexes
//---------------------------
  LABEL_TEXT_INDEX    = 0;
  ANCHOR_NODE_INDEX   = 3;
  METER_TYPE_INDEX    = 4;
  METER_ID_INDEX      = 5;

//------------------------
// Analysis option indexes
//------------------------
  FLOW_UNITS_INDEX    = 0;  //Flow units
  HLOSS_FORM_INDEX    = 1;  //Headloss formula
  SPEC_GRAV_INDEX     = 2;  //Specific gravity
  VISCOS_INDEX        = 3;  //Relative viscosity
  TRIALS_INDEX        = 4;  //Max. trials
  ACCURACY_INDEX      = 5;  //Hydraul. accuracy
  UNBALANCED_INDEX    = 6;  //If unbalanced option
  GLOBAL_PAT_INDEX    = 7;  //Default demand pattern
  DEMAND_MULT_INDEX   = 8;  //Demand multiplier
  EMITTER_EXP_INDEX   = 9;  //Emitter exponent
  STATUS_RPT_INDEX    = 10; //Status report option

  QUAL_PARAM_INDEX    = 11; //Quality parameter
  QUAL_UNITS_INDEX    = 12; //Concen. units
  DIFFUS_INDEX        = 13; //Diffusivity
  TRACE_NODE_INDEX    = 14; //Trace node index
  QUAL_TOL_INDEX      = 15; //Quality tolerance
  MAX_SEGS_INDEX      = 16; //Max. pipe segments

  BULK_ORDER_INDEX    = 17; //Bulk reaction order
  WALL_ORDER_INDEX    = 18; //Wall reaction order
  GLOBAL_KBULK_INDEX  = 19; //Default bulk react. coeff.
  GLOBAL_KWALL_INDEX  = 20; //Default wall react. coeff.
  LIMIT_QUAL_INDEX    = 21; //Limiting potential concen.
  ROUGH_CORREL_INDEX  = 22; //Relation between Kwall & roughness

  DURATION_INDEX      = 23; //Simulation duration
  HYD_TSTEP_INDEX     = 24; //Hydraulic time step
  QUAL_TSTEP_INDEX    = 25; //Quality time step
  PAT_TSTEP_INDEX     = 26; //Pattern time step
  PAT_START_INDEX     = 27; //Pattern start time
  RPT_TSTEP_INDEX     = 28; //Reporting time step
  RPT_START_INDEX     = 29; //Report start time
  START_TIME_INDEX    = 30; //Starting time of day
  TIME_STAT_INDEX     = 31; //Time statistic option
  EFFIC_INDEX         = 32; //Default pump effic.
  EPRICE_INDEX        = 33; //Default energy price
  PRICE_PAT_INDEX     = 34; //Default price pattern
  DMND_CHARGE_INDEX   = 35; //Energy demand charge

  CHECK_FREQ_INDEX    = 36; //Frequency of status checks
  MAX_CHECK_INDEX     = 37; //Trials when status checks end
  DAMP_LIMIT_INDEX    = 38; //Accuracy limit to begin damping

  HEAD_ERROR_INDEX    = 39; //Head error tolerance
  FLOW_CHANGE_INDEX   = 40; //Flow change tolerance
  DEMAND_MODEL_INDEX  = 41; //Demand model type
  MIN_PRESSURE_INDEX  = 42; //Minimum service pressure
  REQ_PRESSURE_INDEX  = 43; //Required service pressure
  PRESSURE_EXP_INDEX  = 44; //Exponent in demand v. pressure function

//-----------------
// Graph-type codes
//-----------------
  TIMESERIESPLOT = 0;
  PROFILEPLOT    = 1;
  CONTOURPLOT    = 2;
  FREQUENCYPLOT  = 3;
  SYSFLOWPLOT    = 4;
  REACTRATEPLOT  = 5;

//------------------
// Report-type codes
//------------------
  NETNODES    = 1;
  NETLINKS    = 2;
  NODESERIES  = 3;
  LINKSERIES  = 4;

//----------------------
// Network map constants
//----------------------
  MINMAPSIZE = 100;
  SYMBOLSIZE = 4;
  PIXTOL = 5;

//------------------------------
// Map and default legend colors
//------------------------------
  MapBackColor: array[1..5] of TColor =
    (clWhite, clInfoBk, clBtnFace, clBlack, clWhite);
  MapForeColor: array[1..5] of TColor =
    (clBlack, clBlack, clBlack, clWhite, clSilver);
  MapGrayColor: array[1..5] of TColor =
    (clBlack, clBlack, clBlack, clGray, clSilver);
  DefLegendColor: array[0..MAXINTERVALS] of TColor =
    ($FF0000, $FFFF00, $FF00, $FFFF, $FF);

//-------------------------
// Map viewing action codes
//-------------------------
  SELECT       = 11;
  GROUPSELECT  = 12;
  PAN          = 13;
  ZOOMIN       = 14;
  ZOOMOUT      = 15;
  FULLEXTENT   = 16;
  VERTEXSELECT = 17;

//------------------------
// Miscellaneous constants
//------------------------
  FLOWTOL         = 0.005;     //Zero flow tolerance
  MISSING         = -1.0e10;   //Missing value
  NOXY            = -9999999;  //Missing map coordinate
  NOPOINT: TPoint = (X: -9999999; Y: -9999999);
  NORECT: TRect   = (Left: -9999999; Top: -9999999;
                     Right: -9999999; Bottom: -9999999);
  NA              = '#N/A';
  NONE            = 0;
  NOVIEW          = 0;
  PLUS            = 1;
  MINUS           = 2;
  DefMeasError    = 5;
  DefIDIncrement  = 1;
  METERSperFOOT   = 0.3048;
  FEETperMETER    = 3.281;

type

//-------------------------
// Default property options
//-------------------------
  TDefProp = record
    Data: array[0..MAXOPTIONS] of String;
  end;

//--------------------------
// View variable information
//--------------------------
  TNodeVariable = record
    Name        : String;
    Source      : TVariableSource;
    SourceIndex : array[JUNCS..TANKS] of Integer;
    DefIntervals: array[1..MAXINTERVALS] of Single;
  end;
  TLinkVariable = record
    Name        : String;
    Source      : TVariableSource;
    SourceIndex : array[PIPES..VALVES] of Integer;
    DefIntervals: array[1..MAXINTERVALS] of Single;
  end;
  TVariableUnits = record
    Units       : String;
    Digits      : Integer;
  end;

//-------------------------
// Map physical dimensions
//-------------------------
  TMapDimensions = record
    LowerLeft    : TExtendedPoint; //Lower left corner of map
    UpperRight   : TExtendedPoint; //Upper right corner of map
    XperDeg      : Extended;       //Meters per degree longitude
    YperDeg      : Extended;       //Meters per degree latitude
    LengthUCF    : Extended;       //Length units conversion factor
    Units        : TMapUnits;      //Length units
    Digits       : Integer;        //Decimal digits in XY-coords.
  end;

//----------------------------
// Map display window scaling
//----------------------------
  TMapWindow = record
    PPW        : Extended;         //Pixels per world coords.
    Woffset    : TExtendedPoint;   //World coord. offset
    Poffset    : TPoint;           //Pixel coord. offset
    Pwidth     : Integer;          //Window width
    Pheight    : Integer;          //Window height
    MapRect    : TRect;            //Window rect
    ZoomFactor : array [0..10] of Extended;
    ZoomIndex  : Integer;
  end;

//--------------------
// Map backdrop image
//--------------------
  TMapBackDrop = record
    Filename   : String;           //File containing backdrop image
    Offset     : TExtendedPoint;   //Image offset from top-left of map
    Visible    : Boolean;          //True if backdrop is visible
  end;

//--------------------
// Map display options
//--------------------
  TMapOptions   = record
    DispNodeIDs      : Boolean;
    DispNodeValues   : Boolean;
    DispNodesBySize  : Boolean;
    DispNodeBorder   : Boolean;
    NodeSize         : Integer;
    DispLinkIDs      : Boolean;
    DispLinkValues   : Boolean;
    DispLinksBySize  : Boolean;
    LinkSize         : Integer;
    DispJuncs        : Boolean;
    DispTanks        : Boolean;
    DispPumps        : Boolean;
    DispValves       : Boolean;
    DispEmitters     : Boolean;
    DispSources      : Boolean;
    DispLabels       : Boolean;
    LabelsTranspar   : Boolean;
    NotationTranspar : Boolean;
    ArrowStyle       : TArrowStyle;
    ArrowSize        : Integer;
    ColorIndex       : Integer;
    NotationZoom     : Integer;
    LabelZoom        : Integer;
    SymbolZoom       : Integer;
    ArrowZoom        : Integer;
    NotationSize     : Integer;
    DispLinkBorder   : Boolean;
  end;

//-----------
// Map legend
//-----------
  TMapLegend = record
    Intervals  : array[0..MAXINTERVALS] of Single;
    Nintervals : Integer; //# intervals used
    Ltype      : Integer; //Legend type (NODE or LINK)
    ViewVar    : Integer; //View variable index (pressure, flow, etc.)
  end;

//-----------------
// Map legend frame
//-----------------
  TLegendFrame = record
    X,Y      : Single;
    Framed   : Boolean;
  end;

//-----------------
// Calibration data
//-----------------
  TCalibData = record
    FileName : String;      //File where data resides
    Locations: TStringList; //Locations where mesurements made
    MeasError: Integer;     //Precision error (%) of measurment
  end;

//----------------------
// Graph display options
//----------------------
  TGraphOptions = record
    View3D          : Boolean;
    Percent3D       : Integer;
    PanelColor      : TColor;
    BackColor       : TColor;
    LegendPosition  : Integer;
    LegendColor     : TColor;
    LegendWidth     : Integer;
    LegendFramed    : Boolean;
    LegendVisible   : Boolean;
    AxisGridStyle   : array[0..1] of Integer;
    LineVisible     : array [0..MAXSERIES] of Boolean;
    LineStyle       : array [0..MAXSERIES] of Integer;
    LineColor       : array [0..MAXSERIES] of TColor;
    LineWidth       : array [0..MAXSERIES] of Integer;
    PointVisible    : array [0..MAXSERIES] of Boolean;
    PointStyle      : array [0..MAXSERIES] of Integer;
    PointColor      : array [0..MAXSERIES] of TColor;
    PointSize       : array [0..MAXSERIES] of Integer;
    TitleFontColor  : TColor;
    TitleFontName   : String;
    AxisFontName    : String;
    TitleFontSize   : Integer;
    AxisFontSize    : Integer;
    TitleFontBold   : Boolean;
    AxisFontBold    : Boolean;
    AreaFillColor   : TColor;
    AreaFillStyle   : TBrushStyle;
    LabelsVisible   : Boolean;
    LabelsTransparent: Boolean;
    LabelsArrows    : Boolean;
    LabelsBackColor : TColor;
  end;

//--------------------
// Contour Map Options
//--------------------
  TContourOptions = record
    Style       : TContourStyle;
    ForeColor   : TColor;
    BackColor   : TColor;
    LinkSize    : Integer;
    LineSize    : Integer;
    NumLines    : Integer;
  end;

//-----------------------
// Table Selection Filter
// ("Pressure below 30")
//-----------------------
  TFilter = record
    Variable: Integer;        //Index of variable
    Relation: TRelationType;  //Relational operator
    StrValue: String;         //Value as a string
  end;

//--------------
// Table Options
//--------------
  TTableOptions = record
    TableType    : Integer;
    TimePeriod   : Integer;
    ObjectID     : String;
    SortField    : Integer;
    NodeFields   : array[ELEVATION..NODEQUAL] of Boolean;
    LinkFields   : array[LINKLENGTH..LINKSTAT] of Boolean;
    Filters      : array[1..MAXFILTERS] of TFilter;
    FilterString : String;
  end;

//------------------------
// Graph Selection Options
//------------------------
  TGraphSelection = record
    GraphType    : Integer;
    ObjectType   : Integer;
    VarType      : Integer;
    Period       : Integer;
    Items        : TStrings;
  end;

//--------------------
// Printed Page Layout
//--------------------
  TPageLayout = record
    PaperSize    : TSinglePoint;
    LMargin      : Single;
    RMargin      : Single;
    TMargin      : Single;
    BMargin      : Single;
  end;

//------------
// Node object
//------------
  TNode = class(TObject)
    ID: PChar;                  // Pointer to ID label

{*** Updated 11/19/01 ***}
    X, Y : Extended;            // X,Y map coordinates

    Zindex: Integer;            // Index in array of computed results
    ColorIndex: Integer;        // Index in array of map display colors
    Data : array [0..MAXNODEPROPS] of String; // Node-specific data
  end;

//-------------------
// Junction object
// (subclass of Node)
//-------------------
  TJunc = class(TNode)
    Demands    : TStringList;
    constructor Create;
    destructor  Destroy; override;
  end;

//------------
// Link object
//------------
  TLink = class(TObject)
    Node1,Node2: TNode;         // Start & end nodes
    Vlist: PVertex;             // List of vertex points
    Zindex: Integer;            // Index in array of computed results
    ColorIndex: Integer;        // Index in array of map display colors
    Data : array [0..MAXLINKPROPS] of String; // Link-specific data
    constructor Create;
    destructor  Destroy; override;
    function    GetVertexCount: Integer;
    procedure   ReverseNodes;
    procedure   ReverseVlist;
  end;

//--------------------
// Time pattern object
//--------------------
  TPattern = class(TObject)
    Comment     : String;
    Multipliers : TStringList;
    constructor Create;
    destructor  Destroy; override;
  end;

//-------------
// Curve object
//-------------
  TCurve = class(TObject)
    Comment : String;
    Ctype   : String;
    Xdata   : TStringList;
    Ydata   : TStringList;
    constructor Create;
    destructor  Destroy; override;
  end;

//-----------------
// Map label object
//-----------------
  TMapLabel = class(Tobject)
    X, Y      : Extended;    // X,Y coordinates
    Anchor    : TNode;       // Anchor node
    MeterType : Integer;     // Meter type
    MeterId   : String;      // ID of object being metered
    MeterText : String;      // Meter text
    FontName  : String;      // Font properties
    FontSize  : Integer;
    FontBold  : Boolean;
    FontItalic: Boolean;
    constructor Create;
  end;

//-----------------------
// Project Options object
//-----------------------
  TOptions = class(TObject)
    Title : String;          // Project title
    Notes : TStringList;     // Project notes
    Data  : array[0..MAXOPTIONS] of String;
    constructor Create;
    destructor  Destroy; override;
  end;

//-------------------------
// Network object clipboard
//-------------------------
  TNetClipboard = class
    ObjType    : Integer;
    Data       : TStringlist;
    Demands    : TStringlist;
    Font       : TFont;
    constructor  Create;
    destructor   Destroy; override;
  end;

//---------------
// Network object
//---------------
  TNetwork = class(TObject)
    Lists              : array [JUNCS..OPTS] of TStringList;
    PropList           : TStringList;   //Property list (for Property Editor)
    SimpleControls     : TStringList;   //Simple controls
    RuleBasedControls  : TStringList;   //Rule-based controls
    Options            : TOptions;      //Analysis options
    NetClipboard       : TNetClipboard; //Network clipboard
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
  end;

const

  DefMapDimensions: TMapDimensions =
    (LowerLeft : (X:0.00; Y:0.00);
     UpperRight: (X:10000.00; Y:10000.00);
     XperDeg   : 111195; //Meters per degree of longitude on spherical earth
     YperDeg   : 111195;
     LengthUCF : 1.0;
     Units     : muNone;
     Digits    : 2);

  DefMapBackdrop: TMapBackdrop =
    (Filename: '';
     Offset  : (X:0.00; Y:0.00);
     Visible : False);

  DefMapOptions: TMapOptions =
    (DispNodeIDs     : False;
     DispNodeValues  : False;
     DispNodesBySize : False;
     DispNodeBorder  : True;
     NodeSize        : 3;
     DispLinkIDs     : False;
     DispLinkValues  : False;
     DispLinksBySize : False;
     LinkSize        : 1;
     DispJuncs       : True;
     DispTanks       : True;
     DispPumps       : True;
     DispValves      : True;
     DispEmitters    : True;
     DispSources     : True;
     DispLabels      : True;
     LabelsTranspar  : True;
     NotationTranspar: False;
     ArrowStyle      : asNone;
     ArrowSize       : 2;
     ColorIndex      : 1;
     NotationZoom    : 100;
     LabelZoom       : 100;
     SymbolZoom      : 100;
     ArrowZoom       : 100;
     NotationSize    : 7;
     DispLinkBorder  : False);

  DefGraphOptions: TGraphOptions =
    (View3D          : False;
     Percent3D       : 25;
     PanelColor      : clBtnFace;
     BackColor       : clWhite;
     LegendPosition  : 2;
     LegendColor     : clWhite;
     LegendWidth     : 60;
     LegendFramed    : True;
     LegendVisible   : False;
     AxisGridStyle   : (2, 2);
     LineVisible     : (True, True, True, True, True, False);
     LineStyle       : (0, 0, 0, 0, 0, 0);
     LineColor       : (clRed, clGreen, clFuchsia, clBlue, clGray, clGreen);
     LineWidth       : (2, 2, 2, 2, 2, 2);
     PointVisible    : (False, False, False, False, False, True);
     PointStyle      : (0, 1, 2, 3, 7, 0);
     PointColor      : (clRed, clGreen, clFuchsia, clBlue, clGray, clGreen);
     PointSize       : (3, 4, 4, 4, 4, 4);
     TitleFontColor  : clBlue;
     TitleFontName   : 'Arial';
     AxisFontName    : 'Arial';
     TitleFontSize   : 12;
     AxisFontSize    : 8;
     TitleFontBold   : True;
     AxisFontBold    : False;
     AreaFillColor   : clRed;
     AreaFillStyle   : bsSolid;
     LabelsVisible   : True;
     LabelsTransparent: False;
     LabelsArrows    : False;
     LabelsBackColor : clYellow);

  DefContourOptions: TContourOptions =
    (Style       : csFilled;
     ForeColor   : clBlack;
     BackColor   : clWhite;
     LinkSize    : 2;
     LineSize    : 1;
     NumLines    : 1);

  DefTableOptions: TTableOptions =
    (TableType   : NONE;
     TimePeriod  : 0;
     ObjectID    : '';
     SortField   : 0;
     NodeFields  : (TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE);
     LinkFields  : (TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                    TRUE, TRUE, TRUE)
    );


{$I consts.txt}  //File containing various constants (strings, defaults, etc.)

var

//--------------------
// Program preferences
//--------------------
  StyleName     : String;
  FontName      : String;
  BoldFonts     : Boolean;              //Dialogs use bold fonts
  Blinking      : Boolean;              //Map hilighter blinks
  FlyOvers      : Boolean;              //Flyover map labels
  AutoBackup    : Boolean;              //Auto project file backup
  ConfirmDelete : Boolean;              //Confirm object deletions
  AutoLength    : Boolean;              //Automatically computes pipe length
  HasChanged    : Boolean;              //Records if changes made to data
  UpdateFlag    : Boolean;              //Records if data changed after a run
  TitleAsHeader : Boolean;              //Use project title as page header
  PageLayout    : TPageLayout;          //Printed page size & margins
  Orientation   : Integer;              //Page orientation

//-----------------
// Network database
//-----------------
  Network       : TNetwork;             //Instance of Network object
  CurrentList   : Integer;              //Current network object type
  CurrentItem   : array [JUNCS..OPTS] of Integer;  //Current item index
  DefProp       : array [JUNCS..OPTS] of TDefProp; //Default properties
  NextID        : array [JUNCS..CNTRLS] of Integer; //Default ID labels
  IDPrefix      : array [JUNCS..CNTRLS] of String;  //ID label prefix
  IDIncrement   : Integer;              //Increment added to ID prefix
  EditorObject  : Integer;              //Type of object being edited
  EditorIndex   : Integer;              //Index of object being edited
  CurrentNodeVar: Integer;              //Node variable being viewed
  CurrentLinkVar: Integer;              //Link variable being viewed
  CurrentPeriod : Integer;              //Time period being viewed
  UnitSystem    : TUnitSystem;          //System of units in use
  FlowUnits     : String;               //Flow units
  QualUnits     : String;               //WQ concen. units
  QualParam     : TWaterQuality;        //Type of WQ parameter
  NodeUnits     : array [0..NODEVIEWS] of TVariableUnits;
  LinkUnits     : array [0..LINKVIEWS] of TVariableUnits;
  GraphOptions  : TGraphOptions;        //Actual graph options

//---------------
// File variables
//---------------
  EpanetDir     : String;               //Program start-up directory
  WindowsDir    : String;               //Windows directory
  IniFileDir    : String;               //Program settings directory
  TempDir       : String;               //Temporary file directory
  InputFileName : String;               //Name of input data file
  InputFileType : TInputFileType;       //Type of input file
  TempInputFile : String;               //Name of temporary input file
  TempReportFile: String;               //Name of status report file
  TempOutputFile: String;               //Name of temporary output file
  ReadOnlyFlag  : Boolean;              //Input file read-only flag

//-------------------
// Map view variables
//-------------------
  CurrentX      : Extended;             //Current map X-coordinate
  CurrentY      : Extended;             //Current map Y-coordinate
  MapBackdrop   : TMapBackdrop;         //Backdrop image
  MapDimensions : TMapDimensions;       //Actual map dimensions
  MapOptions    : TMapOptions;          //Actual map options
  MapZoomRatio  : LongInt;              //Map zoom ratio
  MapNodeColor  : array [0..MAXINTERVALS] of TColor; //Node colors used
  MapLinkColor  : array [0..MAXINTERVALS] of TColor; //Link colors used
  NodeLegend    : array [ELEVATION..NODEVIEWS] of TMapLegend;
  LinkLegend    : array [LINKLENGTH..LINKVIEWS] of TMapLegend;
  NodeLegendFrame : TLegendFrame;
  LinkLegendFrame : TLegendFrame;
  TimeLegendFrame : TLegendFrame;

//-----------------
// Analysis results
//-----------------
  Nnodes        : Integer;              //Number of network nodes
  Ntanks        : Integer;              //Number of tanks
  Nreservs      : Integer;              //Number of reservoirs
  Njuncs        : Integer;              //Number of junctions
  Nlinks        : Integer;              //Number of network links
  Npumps        : Integer;              //Number of pumps
  Nvalves       : Integer;              //Number of control valves
  Npipes        : Integer;              //Number of pipes
  QualFlag      : Integer;              //Type of WQ analysis
  FlowFlag      : Integer;              //Type of flow units used
  TimeStatFlag  : Integer;              //Time-statistic flag
  Rstart        : Longint;              //Start of reporting period (sec)
  Rstep         : Longint;              //Interval between reporting (sec)
  Dur           : Longint;              //Duration of simulation (sec)
  StartTime     : String;               //Starting time of day
  NodeZ         : PSingleArray;         //Values of node view variable
  LinkZ         : PSingleArray;         //Values of link view variable
  FlowDir       : PByteArray;           //Flow direction (+-) of each link
  Title         : array[1..3] of String[80];
  RunFlag       : Boolean;              //Analysis ran OK (True/False)
  RunStatus     : TRunStatus;           //Current run status flag
  Nperiods      : Integer;              //Total number of time periods

//--------------------
// Map query variables
//--------------------
  QueryFlag     : Boolean;              //Query in progress flag
  QueryRelation : TRelationType;        //Query relation type
  QueryValue    : Single;               //Query value
  QueryColor    : TColor;               //Query color

//-----------------
// Calibration data
//-----------------
  NodeCalibData : array [DEMAND..NODEQUAL] of TCalibData;
  LinkCalibData : array [FLOW..HEADLOSS] of TCalibData;

//-----------------------
// Procedure Declarations
//-----------------------
  procedure DeleteNetworkObject(const ObjType: Integer; const Index: Integer);
  function  Link(const Ltype: Integer; const Index: Integer): TLink;
  function  MapLabel(const Index: Integer): TMapLabel;
  function  Node(const Ntype: Integer; const Index: Integer):TNode;
  function  GetID(const ObjType: Integer; const Index: Integer): String;
  procedure SetFont(aForm: TForm);

implementation

//-----------------------
// Network object methods
//-----------------------

constructor TNetwork.Create;
var
  I: Integer;
begin
  inherited Create;
  for I := JUNCS to OPTS do
    Lists[I] := TStringList.Create;
  Lists[CNTRLS].SetText(ControlCategories);
  Lists[OPTS].SetText(OptionCategories);
  PropList := TStringList.Create;
  SimpleControls := TStringList.Create;
  RuleBasedControls := TStringList.Create;
  Options := TOptions.Create;
  NetClipboard := TNetClipboard.Create;
end;

destructor TNetwork.Destroy;
var
  I : Integer;
begin
  for I := JUNCS to OPTS do
  begin
    Lists[I].Free;
    CurrentItem[I] := -1;
  end;
  PropList.Free;
  SimpleControls.Free;
  RuleBasedControls.Free;
  Options.Free;
  NetClipboard.Free;
  inherited Destroy;
end;

procedure TNetwork.Clear;
var
  i,j: Integer;
begin
  for i := JUNCS to CURVES do
  begin
    for j := 0 to Lists[i].Count - 1 do Lists[i].Objects[j].Free;
    Lists[i].Clear;
  end;
  SimpleControls.Clear;
  RuleBasedControls.Clear;
  Options.Title := '';
  Options.Notes.Clear;
end;

//------------------------
// Junction object methods
//------------------------

constructor TJunc.Create;
begin
  inherited Create;
  Demands := TStringList.Create;
end;

destructor TJunc.Destroy;
begin
  Demands.Free;
  inherited Destroy;
end;

//------------------------
// Link object methods
//------------------------

constructor TLink.Create;
begin
  inherited Create;
  Vlist := nil;
end;

destructor TLink.Destroy;
var
  V: PVertex;
begin
  while Vlist <> nil do
  begin
    V := Vlist;
    Vlist := V.Next;
    Dispose(V);
  end;
  inherited Destroy;
end;

function TLink.GetVertexCount: Integer;
var
  aVertex: PVertex;
begin
  Result := 0;
  aVertex := Vlist;
  while aVertex <> nil do
  begin
    Inc(Result);
    aVertex := aVertex^.Next;
  end;
end;

procedure TLink.ReverseNodes;
var
  aNode: TNode;
begin
  aNode := Node2;
  Node2 := Node1;
  Node1 := aNode;
end;

procedure TLink.ReverseVlist;
var
  V, Vprior, Vnext: PVertex;
begin
  Vprior := nil;
  V := Vlist;
  while V <> nil do
  begin
    Vlist := V;
    Vnext := V^.Next;
    V^.Next := Vprior;
    Vprior := V;
    V := Vnext;
  end;
end;

//-------------------------
// Map Label object methods
//-------------------------

constructor TMapLabel.Create;
begin
  inherited Create;
  X := MISSING;
  Y := MISSING;
  Anchor := nil;
  MeterType := 0;
  MeterID := '';
  MeterText := '';
  FontName := 'Arial';
  FontSize := 10;
  FontBold := False;
  FontItalic := False;
end;

//-----------------------
// Pattern object methods
//-----------------------

constructor TPattern.Create;
begin
  inherited Create;
  Multipliers := TStringList.Create;
end;

destructor TPattern.Destroy;
begin
  Multipliers.Free;
  inherited Destroy;
end;

//---------------------
// Curve object methods
//---------------------

constructor TCurve.Create;
begin
  inherited Create;
  Xdata := TStringList.Create;
  Ydata := TStringList.Create;
  Ctype := CurveLabel[1];  //Pump curve is default curve type
end;

destructor TCurve.Destroy;
begin
  Xdata.Free;
  Ydata.Free;
  inherited Destroy;
end;

//-----------------------
// Options object methods
//-----------------------

constructor TOptions.Create;
begin
  inherited Create;
  Notes := TStringList.Create;
end;

destructor TOptions.Destroy;
begin
  Notes.Free;
  inherited Destroy;
end;

//---------------------------------
// Network Clipboard object methods
//---------------------------------

constructor TNetClipboard.Create;
begin
  inherited Create;
  Data := TStringlist.Create;
  Demands := TStringlist.Create;
  Font := TFont.Create;
  ObjType := -1;
end;

destructor TNetClipboard.Destroy;
begin
  Data.Free;
  Demands.Free;
  Font.Free;
  inherited Destroy;
end;

//-------------------------
// Miscellaneous Procedures
//-------------------------

procedure DeleteNetworkObject(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------
// Deletes object of type ObjType and position Index from database.
//-----------------------------------------------------------------
begin
  with Network.Lists[ObjType] do
  begin

  //Free the object & delete it from the object list
    Network.Lists[ObjType].Objects[Index].Free;
    Network.Lists[ObjType].Delete(Index);

  //Update the CurrentItem pointer for the object class
    if Index >= Count then
      CurrentItem[ObjType] := Index - 1
    else
      CurrentItem[ObjType] := Index;
  end;
end;

function Node(const Ntype: Integer; const Index: Integer): TNode;
//----------------------------------------------------------------
// Returns pointer to node object of type Ntype and position Index
//-----------------------------------------------------------------
begin
  Result := TNode(Network.Lists[Ntype].Objects[Index]);
end;

function Link(const Ltype: Integer; const Index: Integer): TLink;
//----------------------------------------------------------------
// Returns pointer to link object of type Ltype and position Index
//-----------------------------------------------------------------
begin
  Result := TLink(Network.Lists[Ltype].Objects[Index]);
end;

function MapLabel(const Index: Integer): TMapLabel;
//------------------------------------------------------
// Returns pointer to map label object at position Index
//------------------------------------------------------
begin
  Result := TMapLabel(Network.Lists[LABELS].Objects[Index]);
end;

function GetID(const ObjType: Integer; const Index: Integer): String;
//---------------------------------------------------------------------
// Returns ID label of network object of type ObjType and index Index.
//---------------------------------------------------------------------
begin
  Result := Network.Lists[ObjType].Strings[Index];
end;

procedure SetFont(aForm: TForm);
//---------------------------------------
// Sets the font to be used on form aForm
//---------------------------------------
begin
  with aForm.Font do
  begin
    Name := FontName;  //'Arial'; //'MS Sans Serif';
    Size := 9;  //8;
    if BoldFonts then Style := [fsBold]
    else Style := [];
  end;
end;

end.
