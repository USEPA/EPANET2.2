unit Uoutput;

{-------------------------------------------------------------------}
{                    Unit:    Uoutput.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that provides interface and retrieval        }
{   routines to EPANET2W's input and computed output.               }
{-------------------------------------------------------------------}

(********************************************************************
    Routine           Purpose
    --------------    -----------------------------------------------
    CheckRunStatus    Checks if a successful simulation was made
    ClearOutput       Clears all output results
    FindLinkColor     Finds color to display a link value with
    FindNodeColor     Finds color to display a node value with
    GetBasicOutput    Stores basic information from output results file
    GetFlowDir        Gets flow direction of each link
    GetJuncValues     Gets all node values for a specific time period
    GetLinkInputStr   Gets string value of a link's input property
    GetLinkMinMax     Gets min and max value of a link variable
    GetLinkQuantiles  Gets selected quantile values of a link variable
    GetLinkSeries     Gets time series of a link output variable
    GetLinkValStr     Gets string value of a link variable
    GetLinkValue      Gets value of a link variable for a specific link
    GetLinkValues     Gets values at all links for a specific time period
    GetMeterLabel     Gets string value to display in a meter label
    GetNodeMinMax     Gets min and max value of a node variable
    GetNodeQuantiles  Gets selected quantile values of a node variable
    GetNodeSeries     Gets time series of a node output variable
    GetNodeValStr     Gets string value of a node variable
    GetNodeValue      Gets value of a node variable for a specific node
    GetNodeValues     Gets values at all nodes for a specific time period
    GetPipeValues     Gets all pipe values for a specific time period
    GetPumpEnergy     Gets energy usage statistics for a specific pump
    GetReactRates     Gets overall average reaction rates
    GetString         Reads a fixed-length string from a binary file
    GetWallCoeff      Gets global or roughness-based wall coefficient
    SetLinkColor      Sets color of a link's input value
    SetLinkColors     Sets colors for all links
    SetNodeColor      Sets color of a node's input value
    SetNodeColors     Sets colors for all nodes
    SetQueryColor     Sets node/link color based on query condition
********************************************************************)
interface

uses SysUtils, Dialogs, Classes, Consts, Graphics, WinTypes, WinProcs,
     Uglobals, Uutils;

function  CheckRunStatus(const Fname: String): TRunStatus;
procedure ClearOutput;
function  FindLinkColor(const Z: Single): Integer;
function  FindNodeColor(const Z: Single): Integer;
procedure GetBasicOutput;
procedure GetFlowDir(const TimePeriod: Integer);
procedure GetJuncValues(const NodeVar: Integer; const Period: Integer;
            var Value: PSingleArray; var Count: Integer);
function  GetLinkInputStr(aLink: TLink; const Index: Integer): String;
procedure GetLinkMinMax(const LinkVar: Integer; const Period: Integer;
            var Xmin: Single; var Xmax: Single);
procedure GetLinkQuantiles(const LinkVar: Integer; const Period: Integer;
            var X: array of Single);
procedure GetLinkSeries(const Link: Integer; const LinkVar: Integer;
            const N1, N2: Integer; var Value: PSingleArray);
function  GetLinkValStr(const V,T,I,J: Integer): String;
function  GetLinkValue(const V,T,I,J: Integer): Single;
procedure GetLinkValues(const LinkVar: Integer;
            const TimePeriod: Integer; var Value: PSingleArray);
procedure GetMeterLabel(const ObjType, ObjIndex: Integer;
            var IDStr, ValStr: String);
procedure GetNodeMinMax(const NodeVar: Integer; const Period: Integer;
            var Xmin: Single; var Xmax: Single);
procedure GetNodeQuantiles(const NodeVar: Integer; const Period: Integer;
            var X: array of Single);
procedure GetNodeSeries(const Node: Integer; const NodeVar: Integer;
            const N1, N2: Integer; var Value: PSingleArray);
function  GetNodeValStr(const V,T,I,J: Integer): String;
function  GetNodeValue(const V,T,I,J: Integer): Single;
procedure GetNodeValues(const NodeVar: Integer;
            const TimePeriod: Integer; var Value: PSingleArray);
procedure GetPipeValues(const LinkVar: Integer; const Period: Integer;
            var Value: PSingleArray; var Count: Integer);
procedure GetPumpEnergy(const P: Integer; var Value: array of Single;
            var Dcharge: Single);
procedure GetReactRates(var R: array of Single);
procedure GetString(var F: File; var S: ShortString);
function  GetWallCoeff(aLink: TLink): String;
procedure SetLinkColor(aLink: TLink; const K: Integer);
procedure SetLinkColors;
procedure SetNodeColor(aNode: TNode; const K: Integer);
procedure SetNodeColors;
function  SetQueryColor(const Z: Single): Integer;

procedure GetLinkLengths(var Value: PSingleArray);


implementation

uses Fmain, Fbrowser;

const

//****************** EXTREMELY IMPORTANT CONSTANTS ******************
//
// These constants allow one to correctly read the results of a
// simulation saved to a binary output file by the network solver
// EPANET2.DLL.
  MagicNumber = 516114521; //File signature

  Version     = 20012;     //Solver version number
  RECORDSIZE  = 4;         //Byte size of each record
  IDSIZE      = 32;        //Size of ID strings

  NODEVARS    = 4;         //Num. of node variables reported on
  LINKVARS    = 8;         //Num. of link variables reported on
//*******************************************************************

var
  Fout       : file;
  Offset1    : Longint;
  Offset2    : Longint;

procedure SetLinkInColors(const LinkVar: Integer); forward;
procedure SetLinkOutColors(const LinkVar: Integer); forward;
procedure SetNodeInColors(const NodeVar: Integer); forward;
procedure SetNodeOutColors(const NodeVar: Integer); forward;

function CheckRunStatus(const Fname: String): TRunStatus;
//-------------------------------------------------------
// Checks if a successful simulation was made
//-------------------------------------------------------
var
  mfirst : Longint;
  mlast  : Longint;
  np     : Longint;
  v      : Longint;
  w      : Longint;

begin
// Open binary output file & check for minimum size
  AssignFile(Fout, Fname);
  {$I-}
  Reset(Fout, 1);
  {$I+}
  if (IOResult <> 0) or (FileSize(Fout)/RecordSize < 21) then
  begin
    Result := rsError;
    CloseFile(Fout);
    exit;
  end;

// Read # time periods, warning flag & magic number at end of file
  Seek(Fout, FileSize(Fout)-3*RecordSize);
  BlockRead(Fout, np, SizeOf(np));
  Nperiods := np;
  BlockRead(Fout, w, SizeOf(w));
  BlockRead(Fout, mlast, SizeOf(mlast));

// Read magic number & version number from start of file
  Seek(Fout, 0);
  BlockRead(Fout, mfirst, SizeOf(mfirst));
  BlockRead(Fout, v, SizeOf(v));

// Check if EPANET run was completed
  if mlast <> MagicNumber then Result := rsError

// Ckeck if results saved for 1 or more time periods
  else if np <= 0 then Result := rsError

// Check if correct version of EPANET was used
  else if (mfirst <> MagicNumber) or (v <> Version)
    then Result := rsWrongVersion

// Check if warning messages were generated
  else if w <> 0 then Result := rsWarning
  else Result := rsSuccess;

// Close file if run was unsuccessful
  if Result in [rsFailed, rsWrongVersion, rsError]
    then CloseFile(Fout);
end;


procedure ClearOutput;
//----------------------------------------------------
// Closes binary output results file and frees all
//  memory allocated to hold simulation output values.
//----------------------------------------------------
var
  i,j: Integer;
begin
  if RunFlag then
  begin

  // Close binary output file
    CloseFile(Fout);

  // Free memory used for network data
    FreeMem(NodeZ, Nnodes*SizeOf(Single));
    FreeMem(LinkZ, Nlinks*SizeOf(Single));
    FreeMem(FlowDir, Nlinks*SizeOf(Byte));

  // Re-set Zindex values
    for i := JUNCS to TANKS do
    begin
      for j := 0 to Network.Lists[i].Count - 1 do
        Node(i,j).Zindex := -1;
    end;
    for i := PIPES to VALVES do
    begin
      for j := 0 to Network.Lists[i].Count - 1 do
        Link(i,j).Zindex := -1;
    end;
  end;

// Re-set status flags
  RunStatus := rsNone;
  RunFlag := False;
  UpdateFlag := False;
end;


function FindLinkColor(const Z: Single): Integer;
//-----------------------------------------------
// Finds color index of value Z in Link legend.
//-----------------------------------------------
var
  i: Integer;
begin
  if (QueryFlag) then Result := SetQueryColor(Z)
  else
  begin
    with LinkLegend[CurrentLinkVar] do
      for i := Nintervals downto 1 do
        if Z >= Intervals[i] then
        begin
          Result := i;
          Exit;
        end;
    Result := 0;
  end;
end;


function FindNodeColor(const Z: Single): Integer;
//-----------------------------------------------
// Finds color index of value Z in Node legend.
//-----------------------------------------------
var
  i: Integer;
begin
  if (QueryFlag) then Result := SetQueryColor(Z)
  else
  begin
    with NodeLegend[CurrentNodeVar] do
      for i := Nintervals downto 1 do
        if Z >= Intervals[i] then
        begin
          Result := i;
          Exit;
        end;
    Result := 0;
  end;
end;


procedure GetBasicOutput;
//------------------------------------------------
// Gets basic information from binary output file.
//------------------------------------------------
var
  i,j,k         : Integer;
  TraceNode     : Longint;
  dummy         : Longint;

begin
// Read number of network components
  BlockRead(Fout, Nnodes, SizeOf(Nnodes));
  BlockRead(Fout, Ntanks, SizeOf(Ntanks));
  Njuncs := Nnodes - Ntanks;
  Nreservs := Network.Lists[RESERVS].Count;
  BlockRead(Fout, Nlinks, SizeOf(Nlinks));
  BlockRead(Fout, Npumps, SizeOf(Npumps));
  BlockRead(Fout, Nvalves, SizeOf(Nvalves));
  Npipes := Nlinks - Npumps - Nvalves;

// Read other network data
  BlockRead(Fout, QualFlag, SizeOf(QualFlag));
  BlockRead(Fout, TraceNode, SizeOf(TraceNode));
  BlockRead(Fout, FlowFlag, SizeOf(FlowFlag));
  BlockRead(Fout, dummy, SizeOf(dummy));
  BlockRead(Fout, TimeStatFlag,  SizeOf(TimeStatFlag));
  BlockRead(Fout, Rstart, SizeOf(Rstart));
  BlockRead(Fout, Rstep, SizeOf(Rstep));
  BlockRead(Fout, Dur, SizeOf(Dur));

// Compute file offset to where dynamic results begin
  Offset1 := 15*RECORDSIZE           //Integer parameters
             + 3*80                  //Title lines
             + 2*260                 //File names
             + 2*IDSIZE              //WQ parameter & units
             + Nnodes*IDSIZE         //Node ID labels
             + Nlinks*IDSIZE         //Link ID labels
             + 3*Nlinks*RECORDSIZE   //Link end nodes & types
             + 2*Ntanks*RECORDSIZE   //Tank node indexes & x-areas
             + Nnodes*RECORDSIZE     //Node elevations
             + 2*Nlinks*RECORDSIZE;  //Link lengths & diameters
  Offset2 := Offset1
             + (7*Npumps+1)*RECORDSIZE; //Pump energy usage

// Allocate memory for output variable arrays
  GetMem(NodeZ, Nnodes*SizeOf(Single));
  GetMem(LinkZ, Nlinks*SizeOf(Single));
  GetMem(FlowDir, Nlinks*SizeOf(Byte));

// For each network component, assign an index into the output arrays
  k := 0;
  for i := JUNCS to TANKS do
  begin
    for j := 0 to Network.Lists[i].Count - 1 do
    begin
      Node(i,j).Zindex := k;
      Inc(k);
    end;
  end;
  k := 0;
  for i := PIPES to VALVES do
  begin
    for j := 0 to Network.Lists[i].Count - 1 do
    begin
      Link(i,j).Zindex := k;
      Inc(k);
    end;
  end;
end;


procedure GetFlowDir(const TimePeriod: Integer);
//----------------------------------------------------
// Determines flow direction in each link of network.
//----------------------------------------------------
var
  i     : Integer;
  d     : Byte;          // Flow direction
  f     : Single;        // Flow value

begin

// Retrieve link flows from binary output file.
  i := LinkVariable[FLOW].SourceIndex[PIPES];
  GetLinkValues(i,TimePeriod,LinkZ);

// Establish flow direction of each link.
  for i := 0 to Nlinks-1 do
  begin
    f := LinkZ^[i];
    d := NONE;
    if f < -FLOWTOL then d := MINUS
    else if f > FLOWTOL then d := PLUS;
    FlowDir^[i] := d;
  end;
end;


function GetLinkInputStr(aLink: TLink; const Index: Integer): String;
//------------------------------------------------------------------
// Gets string value of link's input property
//------------------------------------------------------------------
var
  n: Integer;
begin
// Retrieve database entry for link aLink at position Index
  Result := aLink.Data[Index];
  n := Length(Trim(Result));

// If Index is bulk reaction coeff. then check if global value applies
  if (Index = PIPE_KBULK_INDEX) and (n = 0) then
    Result := Network.Options.Data[GLOBAL_KBULK_INDEX];

// If Index is wall reaction coeff. then check if global value
// or roughness correlation applies (using GetWallCoeff())
  if (Index = PIPE_KWALL_INDEX) and (n = 0) then
    Result := GetWallCoeff(aLink);
end;


procedure GetLinkLengths(var Value: PSingleArray);
begin
  Seek(Fout, Offset1 - 2*Nlinks*RECORDSIZE);
  BlockRead(Fout, Value^, Nlinks*SizeOf(Single));
end;


procedure GetLinkMinMax(const LinkVar: Integer; const Period: Integer;
            var Xmin: Single; var Xmax: Single);
//-------------------------------------------------
// Gets min & max values of pipe variable
//-------------------------------------------------
var
  j,k,m,n : Integer;
  x       : Single;
  y       : PSingleArray;
begin
  k := LinkVariable[LinkVar].SourceIndex[PIPES];
  n := Network.Lists[PIPES].Count - 1;
  Xmin := -MISSING;
  Xmax := MISSING;
  if (LinkVariable[LinkVar].Source = vsInput) then
  begin
    for j := 0 to n do
    begin
      if not Uutils.GetSingle(Link(PIPES,j).Data[k],x) then continue;
      x := Abs(x);
      if (x < Xmin) then Xmin := x;
      if (x > Xmax) then Xmax := x;
    end
  end
  else
  begin
    if (Period = CurrentPeriod) then y := LinkZ
    else GetMem(y, Nlinks*SizeOf(Single));
    try
      if (Period <> CurrentPeriod) then GetLinkValues(k,Period,y);
      for j := 0 to n do
      begin
        m := Link(PIPES,j).Zindex;
        if (m < 0) then continue;
        x := Abs(y^[m]);
        if (x < Xmin) then Xmin := x;
        if (x > Xmax) then Xmax := x;
      end;
    finally
      if (Period <> CurrentPeriod) then FreeMem(y,Nlinks*SizeOf(Single));
    end;
  end;
end;


procedure GetLinkQuantiles(const LinkVar: Integer; const Period: Integer;
  var X: array of Single);
//--------------------------------------------------------------
// Returns in X the sorted values of link variable LinkVar
// at time period Period.
//--------------------------------------------------------------
var
  i,m,my,ny : Integer;
  y         : PSingleArray;
  aList     : TList;
  Compare   : function(a, b: Pointer): Integer;

begin
// Assign Compare function for sorting
  Compare := Uutils.CompareSingles;
  for i := 0 to High(X) do X[i] := 0;

// Allocate memory for temporary array
  ny := Network.Lists[PIPES].Count;
  if (ny < High(X)+1) then Exit;
  GetMem(y, ny*SizeOf(Single));

// Load link variable values into y[]
// (my = number of values loaded)
  try
    GetPipeValues(LinkVar,Period,y,my);
    if (my > 0) then

  // Place pointers to y[] in list & sort it
    begin
      aList := TList.Create;
      try
        for i := 0 to my-1 do aList.Add(Addr(y^[i]));
        aList.Sort(Compare);

      // Extract quantile values from pointer list
        m := my div (High(X)+2);
        if ((High(X)+2)*m) < my then
          for i := 0 to High(X) do
            X[i] := Single(aList.Items[(i+1)*m]^);

    // Free list
      finally
      aList.Free;
      end;
    end;

  finally
    FreeMem(y, ny*SizeOf(Single));
  end;
end;


procedure GetLinkSeries(const Link: Integer; const LinkVar: Integer;
            const N1, N2: Integer; var Value: PSingleArray);
//---------------------------------------------------------
// Gets time series data for a link from output file where:
//   Link    = link index
//   LinkVar = link variable index
//   N1, N2  = start & end time period indexes
//   Value   = array that stores retrieved data
//---------------------------------------------------------
var
  p1, p2, p3 : Longint;
  i, k       : Integer;
begin
  p1 := RecordSize*(Nnodes*NODEVARS + Nlinks*LINKVARS);
  p2 := Offset2 + RecordSize*(Nnodes*NODEVARS
        + Nlinks*(LinkVar-1) + Link);
  i := 0;
  for k := N1 to N2 do
  begin
    p3 := p2 + k*p1;
    Seek(Fout, p3);
    BlockRead(Fout, Value^[i], SizeOf(Single));
    i := i + 1;
  end;
end;


function  GetLinkValStr(const V,T,I,J: Integer): String;
//------------------------------------------------------
// Gets string value of variable V at time period T for
// J-th link of type I.
//------------------------------------------------------
var
  k: Integer;
  s: Integer;
  z: Single;
begin

// Default result is N/A
  Result := NA;
  if (V = NOVIEW) then Exit;

// V is an input variable
  if (LinkVariable[V].Source = vsInput) then
  begin

  // Find index of variable V in property list
    k := LinkVariable[V].SourceIndex[I];
    if (k >= 0) then Result := GetLinkInputStr(Link(I,J),k);
  end

// V is an output variable
  else
  begin

  // Make sure output results exist
    if (RunFlag) and (T < Nperiods) then
    begin

    // Get numerical value & convert to string
      z := GetLinkValue(V,T,I,J);
      if (z <> MISSING) then
      begin

      // If V is link status then convert value to text
        if (V = LINKSTAT) then
        begin
          s := Round(z);
          if (s >= Low(LinkStatus))
          and (s <= High(LinkStatus))
          then Result := LinkStatus[s]
          else Result := NA;
        end

      // Otherwise format value using #digits assigned to V
        else Result := FloatToStrF(z, ffFixed, 7, LinkUnits[V].Digits);
      end;
    end;
  end;
end;


function GetLinkValue(const V,T,I,J: Integer): Single;
//-----------------------------------------------------
// Gets value for variable V at time period T for the
// J-th link of type I.
//-----------------------------------------------------
var
  k : Integer;
  n : Integer;
  p : Longint;
  x : Single;
begin
// Default result is missing value
  Result := MISSING;
  if (V = NOVIEW) then exit;

// V is an input variable
  if (LinkVariable[V].Source = vsInput) then
  begin

  // Find index of variable V in property list
    k := LinkVariable[V].SourceIndex[i];
    if (k >= 0) then
      if Uutils.GetSingle(Link(i,j).Data[k],x) then Result := x;
  end

// V is an output variable
  else
  begin

  // Make sure output results exist
    if (RunFlag) and (T < Nperiods) then
    begin

    // Find index of link & variable in output listing
      n := Link(i,j).Zindex;
      k := LinkVariable[V].SourceIndex[i];
      if (n >= 0) then
      begin

      // Determine position of output value in Output file
        p := Offset2 + T*RecordSize*(Nnodes*NODEVARS + Nlinks*LINKVARS)
               + RecordSize*(Nnodes*NODEVARS + n);
        p := p + (k-1)*Nlinks*RecordSize;
        Seek(Fout,p);
        BlockRead(Fout,Result,SizeOf(Single));
      end;
    end;
  end
end;

procedure GetLinkValues(const LinkVar: Integer;
            const TimePeriod: Integer; var Value: PSingleArray);
//------------------------------------------------------
// Gets data for all links from output file where:
//   LinkVar    = index of link output variable
//   TimePeriod = index of time period
//   Value      = array that stores retrieved values
//------------------------------------------------------
var
  p1,p2,p3: Longint;
begin
  if (LinkVar <> NONE) and (TimePeriod < Nperiods) then
  begin
    p1 := RecordSize*(Nnodes*NODEVARS + Nlinks*LINKVARS);
    p2 := RecordSize*(Nnodes*NODEVARS + Nlinks*(LinkVar-1));
    p3 := Offset2 + (TimePeriod*p1) + p2;
    Seek(Fout, p3);
    BlockRead(Fout, Value^, Nlinks*SizeOf(Single));
  end;
end;

procedure GetMeterLabel(const ObjType, ObjIndex: Integer;
  var IDStr, ValStr: String);
//-------------------------------------------------------------------
// Retrieves object's ID label and current value of its view variable
//-------------------------------------------------------------------
begin
  IDStr := ' ' + ObjectLabel[ObjType] + ' ' + GetID(ObjType,ObjIndex) + ' ';
  if (ObjType in [JUNCS..TANKS]) and (CurrentNodeVar <> NOVIEW) then
    ValStr := GetNodeValStr(CurrentNodeVar, CurrentPeriod, ObjType, ObjIndex)
              + ' ' + NodeUnits[CurrentNodeVar].Units
  else if (ObjType in [PIPES..VALVES]) and (CurrentLinkVar <> NOVIEW) then
  begin
    ValStr := GetLinkValStr(CurrentLinkVar, CurrentPeriod, ObjType, ObjIndex);
    if (ObjType in [PUMPS..VALVES]) and (CurrentLinkVar = HEADLOSS) then
      ValStr := ValStr + ' ' + LinkUnits[LINKLENGTH].Units
    else
      ValStr := ValStr + ' ' + LinkUnits[CurrentLinkVar].Units;
  end
  else ValStr := '';
end;

procedure GetNodeMinMax(const NodeVar: Integer; const Period: Integer;
            var Xmin: Single; var Xmax: Single);
//-------------------------------------------------
// Gets min & max values of junction variable
//-------------------------------------------------
var
  j,k,m,n : Integer;
  x       : Single;
  y       : PSingleArray;
begin
  n := Network.Lists[JUNCS].Count - 1;
  k := NodeVariable[NodeVar].SourceIndex[JUNCS];
  Xmin := -MISSING;
  Xmax := MISSING;
  if (NodeVariable[NodeVar].Source = vsInput) then
  begin
    for j := 0 to n do
    begin
      if not Uutils.GetSingle(Node(JUNCS,j).Data[k],x) then continue;
      x := Abs(x);
      if (x < Xmin) then Xmin := x;
      if (x > Xmax) then Xmax := x;
    end
  end
  else
  begin
    if (Period = CurrentPeriod) then y := NodeZ
    else GetMem(y, Nnodes*SizeOf(Single));
    try
      if (Period <> CurrentPeriod) then GetNodeValues(k,Period,y);
      for j := 0 to n do
      begin
        m := Node(JUNCS,j).Zindex;
        if (m < 0) then continue;
        x := Abs(y^[m]);
        if (x < Xmin) then Xmin := x;
        if (x > Xmax) then Xmax := x;
      end;
    finally
      if (Period <> CurrentPeriod) then FreeMem(y,Nnodes*SizeOf(Single));
    end;
  end;
end;


procedure GetNodeQuantiles(const NodeVar: Integer; const Period: Integer;
  var X: array of Single);
//--------------------------------------------------------
// Returns in X the sorted values of node variable NodeVar
// at time period Period.
//--------------------------------------------------------
var
  i,m,my,ny    : Integer;
  y            : PSingleArray;
  aList        : TList;
  Compare      : function(a, b: Pointer): Integer;

begin
// Assign Compare function for sorting
  Compare := Uutils.CompareSingles;
  for i := 0 to High(X) do X[i] := 0;

// Allocate memory for temporary array
  ny := Network.Lists[JUNCS].Count;
  if (ny < 2*(High(X) + 1)) then Exit;
  GetMem(y, ny*SizeOf(Single));

// Load variable values into y[]
// (my = number of values loaded)
  try
    GetJuncValues(NodeVar,Period,y,my);
    if (my > 0) then
    begin

    // Place pointers to y[] in list & sort it
      aList := TList.Create;
      try
        for i := 0 to my-1 do aList.Add(Addr(y^[i]));
        aList.Sort(Compare);

      // Extract quantile values from pointer list
        m := my div (High(X)+2);
        if ((High(X)+1)*m) < my then
          for i := 0 to High(X) do
            X[i] := Single(aList.Items[(i+1)*m]^);

    // Free list memory
      finally
        aList.Free;
      end;
    end;

  finally
    FreeMem(y, ny*SizeOf(Single));
  end;
end;


procedure GetNodeSeries(const Node: Integer; const NodeVar: Integer;
             const N1, N2: Integer; var Value: PSingleArray);
//---------------------------------------------------------
// Gets time series data for a node from output file where:
//   Node    = node index
//   NodeVar = node variable index
//   N1, N2  = start & end time period indexes
//   Value   = array that stores retrieved data
//---------------------------------------------------------
var
  p1, p2, p3 : Longint;
  i, k       : Integer;
begin
  p1 := RecordSize*(Nnodes*NODEVARS + Nlinks*LINKVARS);
  p2 := Offset2 + RecordSize*(Nnodes*(NodeVar-1) + Node);
  i := 0;
  for k := N1 to N2 do
  begin
    p3 := p2 + k*p1;
    Seek(Fout, p3);
    BlockRead(Fout, Value^[i], SizeOf(Single));
    i := i + 1;
  end;
end;


function  GetNodeValStr(const V,T,I,J: Integer): String;
//------------------------------------------------------
// Gets string value of variable V at time period T for
// J-th node of type I.
//------------------------------------------------------
var
  k: Integer;
  z: Single;
begin
// Default result is N/A
  Result := NA;
  if (V = NOVIEW) then Exit;

// V is an input variable
  if (NodeVariable[V].Source = vsInput) then
  begin

  // Find index of variable V in property list
    k := NodeVariable[V].SourceIndex[I];
    if (k >= 0) then Result := Node(I,J).Data[k];
    if (V = INITQUAL) and (Length(Result) = 0) then Result := '0';
  end

// V is an output variable
  else
  begin

  // Make sure output results exist
    if (RunFlag) and (T < Nperiods) then
    begin

    // Get numerical value & convert to string
      z := GetNodeValue(V,T,I,J);
      if (z <> MISSING) then
        Result := FloatToStrF(z, ffFixed, 7, NodeUnits[V].Digits);
    end;
  end;
end;


function GetNodeValue(const V,T,I,J: Integer): Single;
//-----------------------------------------------------
// Gets value for Variable V at time period T for the
// J-th node of type I.
//-----------------------------------------------------
var
  k : Integer;
  n : Integer;
  p : Longint;
  x : Single;
begin
// Default result is missing value (or 0 for InitQual)
  Result := MISSING;
  if (V = INITQUAL) then Result := 0;
  if (V = NOVIEW) then exit;

// V is an input variable
  if (NodeVariable[V].Source = vsInput) then
  begin

  // Find index of variable V in property list
    k := NodeVariable[V].SourceIndex[i];
    if (k >= 0) then
      if Uutils.GetSingle(Node(i,j).Data[k],x) then Result := x;
  end

// V is an output variable
  else
  begin

  // Make sure output results exist
    if (RunFlag) and (T < Nperiods) then
    begin

    // Find index of node & variable in output listing
      n := Node(i,j).Zindex;
      k := NodeVariable[V].SourceIndex[i];
      if (n >= 0) then
      begin

      // Determine position of output value in Output file
        p := Offset2 + T*RecordSize*(Nnodes*NODEVARS + Nlinks*LINKVARS)
               + n*RecordSize;
        p := p + (k-1)*Nnodes*RecordSize;
        Seek(Fout,p);
        BlockRead(Fout,Result,SizeOf(Single));
      end;
    end;
  end
end;

procedure GetNodeValues(const NodeVar: Integer;
            const TimePeriod: Integer; var Value: PSingleArray);
//------------------------------------------------------
// Gets data for all nodes from output file where:
//   NodeVar    = index of node output variable
//   TimePeriod = index of time period
//   Value      = array that stores retrieved values
//------------------------------------------------------
var
  p1,p2,p3: Longint;
begin
  if (NodeVar <> NONE) and (TimePeriod < Nperiods) then
  begin
    p1 := RecordSize*(Nnodes*NODEVARS + Nlinks*LINKVARS);
    p2 := RecordSize*Nnodes*(NodeVar-1);
    p3 := Offset2 + (TimePeriod*p1) + p2;
    Seek(Fout, p3);
    BlockRead(Fout, Value^, Nnodes*SizeOf(Single));
  end;
end;


procedure GetJuncValues(const NodeVar: Integer; const Period: Integer;
  var Value: PSingleArray; var Count: Integer);
//--------------------------------------------------------------------
// Stores junction values for node variable NodeVar at time period
// Period in array Value. Count is the number of values found.
//--------------------------------------------------------------------
var
  j,k,m,n: Integer;
  v      : Single;
  z      : PSingleArray;
begin
  n := Network.Lists[JUNCS].Count;
  Count := 0;
  k := NodeVariable[NodeVar].SourceIndex[JUNCS];
  if (NodeVariable[NodeVar].Source = vsInput) then
  begin
    for j := 0 to n-1 do
    begin
      if not Uutils.GetSingle(Node(JUNCS,j).Data[k],v) then continue;
      Value^[Count] := Abs(v);
      Inc(Count);
    end;
  end
  else if RunFlag and (Period < Nperiods) then
  begin
    GetMem(z, Nnodes*SizeOf(Single));
    try
      GetNodeValues(k,Period,z);
      for j := 0 to n-1 do
      begin
        m := Node(JUNCS,j).Zindex;
        if (m < 0) then continue;
        Value^[Count] := Abs(z^[m]);
        Inc(Count);
      end;
    finally
      FreeMem(z, Nnodes*SizeOf(Single));
    end;
  end;
end;


procedure GetPipeValues(const LinkVar: Integer; const Period: Integer;
  var Value: PSingleArray; var Count: Integer);
//--------------------------------------------------------------------
// Stores pipe values for link variable LinkVar at time period Period
// in array Value. Count is the number of values found.
//--------------------------------------------------------------------
var
  j,k,m,n: Integer;
  v      : Single;
  z      : PSingleArray;
begin
  n := Network.Lists[PIPES].Count;
  Count := 0;
  k := LinkVariable[LinkVar].SourceIndex[PIPES];
  if (LinkVariable[LinkVar].Source = vsInput) then
  begin
    for j := 0 to n-1 do
    begin
      if not Uutils.GetSingle(Link(PIPES,j).Data[k],v) then continue;
      Value^[Count] := Abs(v);
      Inc(Count);
    end;
  end
  else if RunFlag and (Period < Nperiods) then
  begin
    GetMem(z, Nlinks*SizeOf(Single));
    try
      GetLinkValues(k,Period,z);
      for j := 0 to n-1 do
      begin
        m := Link(PIPES,j).Zindex;
        if (m < 0) then continue;
        Value^[Count] := Abs(z^[m]);
        Inc(Count);
      end;
    finally
      FreeMem(z, Nlinks*SizeOf(Single));
    end;
  end;
end;


procedure GetPumpEnergy(const P: Integer; var Value: array of Single;
  var Dcharge: Single);
//-------------------------------------------------------
// Retrieves energy usage statistics for a specific pump
//-------------------------------------------------------
begin
  Seek(Fout, Offset1 + (7*(P-Npipes)+1)*RECORDSIZE);
  BlockRead(Fout, Value, 6*Sizeof(Single));
  BlockRead(Fout, Dcharge, Sizeof(Single));
end;


procedure GetReactRates(var R: array of Single);
//-----------------------------------------------
// Retrieves overall average reaction rates
// NOTE: The 3 avg. reaction rates + avg. source
//       input rate are stored at end of the
//       binary output file just before the last
//       3 records.
//-----------------------------------------------
begin
  Seek(Fout, FileSize(Fout)-7*RECORDSIZE);
  BlockRead(Fout, R, 4*Sizeof(Single));
end;


procedure GetString(var F: File; var S: ShortString);
//--------------------------------------------------------
// Reads fixed-size string from current position of file F.
// (F must be declared as 'var' because BlockRead is used.)
//---------------------------------------------------------
var
  buf: Pchar;
  size: Word;
begin
  size := SizeOf(S);
  buf := StrAlloc(size);
  BlockRead(F, buf^, size-1);
  S := StrPas(buf);
  StrDispose(buf);
end;


function GetWallCoeff(aLink: TLink): String;
//---------------------------------------------------------
// Returns string value of pipe's wall reaction coefficient
// as determined by global network option settings.
//---------------------------------------------------------
var
  c,r,kw: Single;
  hlf   : String;
begin
// Default is global wall coefficient
  Result := Network.Options.Data[GLOBAL_KWALL_INDEX];

// Check for use of a roughness correlation
// Convert roughness correlation (c) & pipe roughness (r) to floats
  Uutils.GetSingle(Network.Options.Data[ROUGH_CORREL_INDEX],c);
  Uutils.GetSingle(aLink.Data[PIPE_ROUGH_INDEX],r);

// Check for legal values
  if (r > 0.0) and (c <> 0.0) then
  begin

  // Compute wall coeff. based on head loss formula (hlf)
    hlf := UpperCase(Network.Options.Data[HLOSS_FORM_INDEX]);
    if      hlf = 'H-W' then kw := c/r
    else if hlf = 'D-W' then kw := c/Abs(Ln(r))
    else if hlf = 'C-M' then kw := c*r
    else Exit;
    Result := Format('%.3f',[kw]);
  end;
end;

procedure SetLinkColor(aLink: TLink; const K: Integer);
//-------------------------------------------------------
// Sets map color index for link's K-th input data item.
//-------------------------------------------------------
var
  code : Boolean;
  x    : Single;
begin
  if K < 0 then
    aLink.ColorIndex := -1
  else
  begin
    if K in [PIPE_KBULK_INDEX, PIPE_KWALL_INDEX] then
      code := Uutils.GetSingle(GetLinkInputStr(aLink,K), x)
    else code := Uutils.GetSingle(aLink.Data[K], x);
    if code then
      aLink.ColorIndex := FindLinkColor(Abs(x))
    else
      aLink.ColorIndex := -1;
  end;
end;


procedure SetLinkColors;
//------------------------------------------------
// Sets map link colors for current link variable
//------------------------------------------------
var
  clv  : Integer;
begin
  clv := CurrentLinkVar;
  if (LinkVariable[clv].Source = vsOutput) and (RunFlag = False) then
    clv := NOVIEW;
  if (LinkVariable[clv].Source = vsInput) then
    SetLinkInColors(clv)
  else
    SetLinkOutColors(clv);
end;

procedure SetLinkInColors(const LinkVar: Integer);
//------------------------------------------------
// Sets map link colors for input variable LinkVar
//------------------------------------------------
var
  i,j,k : Integer;
begin
  for i := PIPES to VALVES do
  begin
    if LinkVar = NOVIEW then k := -1
    else k := LinkVariable[LinkVar].SourceIndex[i];
    for j := 0 to Network.Lists[i].Count-1 do
      SetLinkColor(Link(i,j),k);
  end;
end;

procedure SetLinkOutColors(const LinkVar: Integer);
//-------------------------------------------------
// Sets map link colors for output variable LinkVar
//-------------------------------------------------
var
  i,j,k : Integer;
  aLink : TLink;
begin
    k := LinkVariable[LinkVar].SourceIndex[PIPES];
    GetLinkValues(k,CurrentPeriod,LinkZ);
    for i := PIPES to VALVES do
    begin
      for j := 0 to Network.Lists[i].Count-1 do
      begin
        aLink := Link(i,j);
        k := aLink.Zindex;
        if k >= 0 then
          aLink.ColorIndex := FindLinkColor(abs(LinkZ^[k]))
        else
          aLink.ColorIndex := -1;
      end;
    end;
end;


procedure SetNodeColor(aNode: TNode; const K: Integer);
//-------------------------------------------------------
// Sets map color index for node's K-th input data item.
//-------------------------------------------------------
var
  x    : Single;
begin
  if K < 0 then
    aNode.ColorIndex := -1
  else
  begin
    Uutils.GetSingle(aNode.Data[K],x);
    aNode.ColorIndex := FindNodeColor(x);
  end;
end;


procedure SetNodeColors;
//------------------------------------------------
// Sets map node colors for current node variable
//------------------------------------------------
var
  cnv: Integer;
begin
  cnv := CurrentNodeVar;
  if (NodeVariable[cnv].Source = vsOutput) and
     (RunFlag = False) then cnv := NOVIEW;
  if (NodeVariable[cnv].Source = vsInput) then
    SetNodeInColors(cnv)
  else
    SetNodeOutColors(cnv);
end;


procedure SetNodeInColors(const NodeVar: Integer);
//------------------------------------------------
// Sets map node colors for input variable NodeVar
//------------------------------------------------
var
  i,j,k : Integer;
begin
  for i := JUNCS to TANKS do
  begin
    if NodeVar = NOVIEW then k := -1
    else k := NodeVariable[NodeVar].SourceIndex[i];
    for j := 0 to Network.Lists[i].Count-1 do
      SetNodeColor(Node(i,j),k);
  end;
end;

procedure SetNodeOutColors(const NodeVar: Integer);
//-------------------------------------------------
// Sets map node colors for output variable NodeVar
//-------------------------------------------------
var
  i,j,k : Integer;
  aNode : TNode;
begin
    k := NodeVariable[NodeVar].SourceIndex[JUNCS];
    GetNodeValues(k,CurrentPeriod,NodeZ);
    for i := JUNCS to TANKS do
    begin
      for j := 0 to Network.Lists[i].Count-1 do
      begin
        aNode := Node(i,j);
        k := aNode.Zindex;
        if k >= 0 then
          aNode.ColorIndex := FindNodeColor(NodeZ^[k])
        else
          aNode.ColorIndex := -1;
      end;
    end;
end;

function SetQueryColor(const Z: Single): Integer;
//--------------------------------------------------
// Sets color of network item based on query result.
//--------------------------------------------------
begin
  Result := -1;
  case QueryRelation of
  rtBelow: if Z < QueryValue then Result := 1;
  rtEquals: if Z = QueryValue then Result := 1;
  rtAbove: if Z > QueryValue then Result := 1;
  end;
end;

end.
