unit Uimport;

{-------------------------------------------------------------------}
{                    Unit:    Uimport.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that imports network database files, map     }
{   files and scenario files in .INP text format.                   }
{-------------------------------------------------------------------}

interface

uses
 Classes, Forms, Controls, Dialogs, SysUtils, System.UITypes, Math,
 Uutils, Uglobals;

const
  MSG_INVALID_FILE = ' is not a valid EPANET data file.';
  MSG_IMPORTING_DATA = 'Importing network data...';
  MSG_NO_READ_MAP_FILE = 'Could not read map file ';
  MSG_MAP_FILE = 'Map file ';
  MSG_NOT_EXIST = ' does not exist.';
  MSG_READING_MAP_FILE = 'Reading map file...';
  TXT_ERROR = 'Error ';
  TXT_AT_LINE = ' at line ';
  TXT_MORE_ERRORS = ' more errors found in file.';
  TXT_ERROR_REPORT = 'Error Report for File ';

  SectionWords : array[0..29] of PChar =
    ('[TITLE',      {0: Title}
     '[JUNCTIONS',  {1: Junctions}
     '[RESERVOIRS', {2: Reservoirs}
     '[TANKS',      {3: Tanks}
     '[PIPES',      {4: Pipes}
     '[PUMPS',      {5: Pumps}
     '[VALVES',     {6: Valves}
     '[CONTROLS',   {7: Controls}
     '[RULES',      {8: Rules}
     '[DEMANDS',    {9: Demands}
     '[ROUGHNESS',  {10: Roughness}
     '[ENERGY',     {11: Energy}
     '[EMITTERS',   {12: Emitters}
     '[SOURCES',    {13: Sources}
     '[PATTERNS',   {14: Patterns}
     '[CURVES',     {15: Curves}
     '[QUALITY',    {16: Quality}
     '[STATUS',     {17: Status}
     '[REACTIONS',  {18: Reactions}
     '[MIXING',     {19: Mixing}
     '[REPORT',     {20: Report}
     '[TIMES',      {21: Times}
     '[OPTIONS',    {22: Options}
     '[END',        {23: End}
     '[COORDINATES',{24: Coordinates}
     '[VERTICES',   {25: Vertices}
     '[LABELS',     {26: Labels}
     '[BACKDROP',   {27: Backdrop}
     '[DIAMETERS',  {28: Diameters}
     '[TAGS');      {29: Tags}

  OptionWords : array[0..23] of PChar =
    ('UNIT',      {0: Flow Units}
     'HEAD',      {1: Headloss Formula or Head Error}
     'SPEC',      {2: Specific Gravity}
     'VISC',      {3: Viscosity}
     'TRIA',      {4: Trials}
     'ACCU',      {5: Accuracy}
     'UNBA',      {6: Unbalanced}
     'PATT',      {7: Pattern}
     'DEMA',      {8: Demand Multiplier or Demand Model}
     'EMIT',      {9: Emitter Exponent}
     'STAT',      {10: Status Report}
     'QUAL',      {11: Quality}
     'PRES',      {12: Pressure Units or Pressure Exponent}
     'DIFF',      {13: Diffusivity}
     '',          {14: blank}
     'TOLE',      {15: Tolerance}
     'SEGM',      {16: Segments}
     'MAP',       {17: Map File}
     'FLOW',      {18: Flow Change}
     'MINI',      {19: Minimum Pressure}
     'REQU',      {20: Required Pressure}
     'CHEC',      {21: CHECKFREQ}
     'MAXC',      {22: MAXCHECK}
     'DAMP');     {23: DAMPLIMIT}

  EnergyWords : array[0..5] of PChar =
    ('GLOBAL',
     'PUMP',
     'EFFIC',
     'PRICE',
     'PATTERN',
     'DEMAND');

  TimeWords : array[0..7] of PChar =
    ('DURA',      {0: Duration}
     'HYDR',      {1: Hydraulic Timestep}
     'QUAL',      {2: Quality Timestep}
     'PATT',      {3: Pattern Timestep or Start}
     'REPO',      {4: Report Timestep or Start}
     'STAR',      {5: Start Clocktime}
     'RULE',      {6: Rule Timestep}
     'MINI');     {7: Minimum Traveltime}

  TimeStatWords : array[0..4] of PChar =
    ('NONE', 'AVERAGE', 'MINIMUM', 'MAXIMUM', 'RANGE');

  TimeUnitWords : array[0..4] of PChar =
    ('DAY', 'HR', 'HOUR', 'MIN', 'SEC');

  StatusWords: array[0..2] of PChar =
    ('OPEN','CLOSED','CV');

  ReactWords: array[0..6] of PChar =
    ('ORDER','GLOBAL','LIMIT','ROUGH','BULK','WALL','TANK');

  OrderWords: array[0..1] of PChar =
    ('Zero','First');

  SourceWords: array[0..3] of PChar =
    ('CONCEN','MASS','SETPOINT','FLOWPACED');

  CurveWords: array[0..3] of PChar =
    ('POWER','PATTERN','HEAD','SPEED');

  MixWords: array[0..3] of PChar =
    ('MIXED','2COMP','FIFO','LIFO');

  BackdropWords: array[0..3] of PChar =
    ('DIMENSIONS','UNITS','FILE','OFFSET');

  IntegrateWords: array[0..2] of PChar =
    ('SE','ME','RK');

  MULTIPLY = 'MULTIPLY';

type
  TFileType = (ftInput, ftImport);

// These routines can be called from other units
function  ImportMapFile(const Fname: String): Boolean;
function  ReadInpFile(const Fname: String):Boolean;
procedure ReadScnFile(const Fname: String);
procedure SetMapDimensions(var NewDimensions: TMapDimensions);


implementation

uses Fmain, Dinperr, Uinput;

var
  FileType     : TFileType;
  MapFile      : String;
  ErrList      : TStringlist;
  NodeList     : TStringlist;
  LinkList     : TStringlist;
  TokList      : TStringlist;
  Ntoks        : Integer;
  Section      : Integer;
  Comment      : String;
  PrevComment  : String;
  PrevID       : String;
  PrevIndex    : Integer;
  DimensionsSet: Boolean;

function ReadTitleData(Line: String): Integer;
//-------------------------------------------
// Adds title and notes to project
//-------------------------------------------
begin
  if Network.Options.Title = '' then
    Network.Options.Title := Line
  else
    Network.Options.Notes.Add(Line);
  Result := 0;
end;

function ReadJunctionData: Integer;
//-------------------------------------
// Parses data junction with format:
//   JuncID  Elev.  (Demand   Pattern)
//--------------------------------------
var
  J    : Integer;
  ID   : String;
  aJunc: TJunc;
begin
  if Ntoks < 1 then
    Result := 201
  else
  begin
    ID := TokList[0];
    aJunc := TJunc.Create;
    aJunc.Zindex := JUNCS;
    aJunc.X := MISSING;
    aJunc.Y := MISSING;
    Uutils.CopyStringArray(DefProp[JUNCS].Data, aJunc.Data);
    aJunc.Data[0] := Comment;
    for J := 1 to Ntoks-1 do
      aJunc.Data[J+1] := TokList[J];
    Network.Lists[JUNCS].AddObject(ID, aJunc);
    NodeList.AddObject(ID, aJunc);
    Result := 0;
  end;
end;

function ReadTankData: Integer;
//----------------------------------------------------------------------
// Parses tank/reservoir data with formats:
//  ReservID  Elev.  (Pattern)
//  TankID    Elev.  (InitLvl  MinLvl  MaxLvl  Diam.  MinVol.  VolCurve  Overflow)
//----------------------------------------------------------------------
var
  J    : Integer;
  ID   : String;
  aNode: TNode;
begin
  if Ntoks < 1 then
    Result := 201
  else
  begin
    ID := TokList[0];
    aNode := TNode.Create;
    aNode.X := MISSING;
    aNode.Y := MISSING;

    // Check if processing a Reservoir or Tank
    if Ntoks <= 3 then
    begin
      aNode.Zindex := RESERVS;
      Uutils.CopyStringArray(DefProp[RESERVS].Data,aNode.Data);
    end
    else
    begin
      aNode.Zindex := TANKS;
      Uutils.CopyStringArray(DefProp[TANKS].Data,aNode.Data);
    end;

    // Assign tokens (indexes 1 - 7) to data items (indexes 2 - 8)
    aNode.Data[0] := Comment;
     for J := 1 to Math.min(Ntoks-1, 7) do
      aNode.Data[J+1] := TokList[J];

    // Check if tank curve name is placeholder value '*'
    if SameText(aNode.Data[TANK_VCURVE_INDEX], '*')
    then aNode.Data[TANK_VCURVE_INDEX] := '';

    // Parse overflow indicator if present
    if (Ntoks > 8) then
    begin
      if SameText(TokList[8], 'Yes')
      then aNode.Data[TANK_OVERFLOW_INDEX] := 'Yes'
      else aNode.Data[TANK_OVERFLOW_INDEX] := 'No';
    end;

    // Add node to network
    if Ntoks <= 3 then Network.Lists[RESERVS].AddObject(ID, aNode)
    else               Network.Lists[TANKS].AddObject(ID, aNode);
    NodeList.AddObject(ID, aNode);
    Result := 0;
  end;
end;

function ReadPipeData: Integer;
//--------------------------------------------------------------
// Parses pipe data with formats:
//  PipeID  Node1  Node2  (Length  Diam  Rough.  Mloss  Status)
//  PipeID  Node1  Node2  (Length  Diam  Rough.  Status)
//--------------------------------------------------------------
var
  J    : Integer;
  n1,n2: Integer;
  ID   : String;
  aLink: TLink;
begin
  if Ntoks < 3 then Result := 201
  else
  begin
  // Check that end nodes exist
    ID := TokList[0];
    if not NodeList.Find(TokList[1],n1)
    or not NodeList.Find(TokList[2],n2) then Result := 203
    else
    begin
      aLink := TLink.Create;
      aLink.Node1 := TNode(NodeList.Objects[n1]);
      aLink.Node2 := TNode(NodeList.Objects[n2]);
      aLink.Zindex := PIPES;
      Uutils.CopyStringArray(DefProp[PIPES].Data, aLink.Data);
      aLink.Data[COMMENT_INDEX] := Comment;
      if Ntoks > 3 then
      begin
        n2 := 5;
        if n2 >= Ntoks then n2 := Ntoks - 1;
        for J := 3 to n2 do aLink.Data[J-1] := TokList[J];
        if (Ntoks = 7) then
        begin
          if (Uutils.FindKeyWord(TokList[6],StatusWords,4) >= 0) then
            aLink.Data[PIPE_STATUS_INDEX] := TokList[6]
          else
            aLink.Data[PIPE_MLOSS_INDEX] := TokList[6];
        end;
        if (Ntoks = 8) then
        begin
          aLink.Data[PIPE_MLOSS_INDEX] := TokList[6];
          aLink.Data[PIPE_STATUS_INDEX] := TokList[7];
        end;
      end;
      Network.Lists[PIPES].AddObject(ID,aLink);
      LinkList.AddObject(ID,aLink);
      Result := 0;
    end;
  end;
end;

procedure ReadOldPumpData(aLink: TLink);
//-----------------------------------------
// Reads pump data with formats:
//   PumpID Node1 Node2 Power
//   PumpID Node1 Node2 H1 Q1
//   PumpID Node1 Node2 H0 H1 Q1 H2 Q2
//-----------------------------------------
var
  aCurve : TCurve;
  id     : String;
begin
{ Check if only pump power supplied }
  if (Ntoks = 4) then
  begin
    aLink.Data[PUMP_HP_INDEX] := TokList[3];
  end
{ Check if pump curve supplied }
  else if (Ntoks >= 5) then
  begin
  { Create a new Curve object }
    aCurve := TCurve.Create;
    id := Uinput.GetNextID(CURVES);
    aCurve.Comment := 'Pump curve for Pump ' + TokList[0];
    aCurve.Ctype := 'PUMP';
  { Add generic pump data to curve }
    if (Ntoks = 5) then
    begin
      aCurve.Ydata.Add(TokList[3]);
      aCurve.Xdata.Add(TokList[4]);
    end
  { Add 3-pt. pump data to curve }
    else if (Ntoks >= 8) then
    begin
      aCurve.Ydata.Add(TokList[3]);
      aCurve.Xdata.Add('0');
      aCurve.Ydata.Add(TokList[4]);
      aCurve.Xdata.Add(TokList[5]);
      aCurve.Ydata.Add(TokList[6]);
      aCurve.Xdata.Add(TokList[7]);
      if (Ntoks > 8) and (TokList[7] <> TokList[8]) then
      begin
        aCurve.Xdata.Add(TokList[8]);
        aCurve.Ydata.Add('0');
      end;
    end;
    Network.Lists[CURVES].AddObject(id,aCurve);
    aLink.Data[PUMP_HCURVE_INDEX] := id;
  end;
end;

procedure ReadNewPumpData(aLink: TLink);
//---------------------------------------------------------------------
// Reads pump data with format:
//   PumpID  Node1  Node2  (POWER  value  PATTERN  patID  HEAD  curveID
//                          SPEED  value)
// where keyword-value pairs can be in any order
//---------------------------------------------------------------------
var
  i,j: Integer;
begin
  i := 4;
  while (i < Ntoks) do
  begin
    j := Uutils.FindKeyWord(TokList[i-1],CurveWords,4);
    case j of
    0:  aLink.Data[PUMP_HP_INDEX]      := TokList[i];   {Power value}
    1:  aLink.Data[PUMP_PATTERN_INDEX] := TokList[i];   {Speed pattern}
    2:  aLink.Data[PUMP_HCURVE_INDEX]  := TokList[i];   {Pump curve id}
    3:  aLink.Data[PUMP_SPEED_INDEX]   := TokList[i];   {Pump speed}
    end;
    i := i + 2;
  end;
end;

function ReadPumpData: Integer;
//---------------------------------------------
// Reads pump data in either old or new format
//---------------------------------------------
var
  ID   : String;
  aLink: TLink;
  n1,n2: Integer;
  v    : Single;
begin
  if Ntoks < 3 then Result := 201
  else
  begin
    ID := TokList[0];
    if not NodeList.Find(TokList[1],n1)
    or not NodeList.Find(TokList[2],n2) then Result := 203
    else
    begin
      aLink := TLink.Create;
      aLink.Node1 := TNode(NodeList.Objects[n1]);
      aLink.Node2 := TNode(NodeList.Objects[n2]);
      aLink.Zindex := PUMPS;
      Uutils.CopyStringArray(DefProp[PUMPS].Data, aLink.Data);
      aLink.Data[COMMENT_INDEX] := Comment;
      if Ntoks > 3 then
      begin
        if Uutils.GetSingle(TokList[3],v) then
          ReadOldPumpData(aLink)
        else
          ReadNewPumpData(aLink);
      end;
      Network.Lists[PUMPS].AddObject(ID,aLink);
      LinkList.AddObject(ID,aLink);
      Result := 0;
    end;
  end;
end;

function ReadValveData: Integer;
//-----------------------------------------------------
// Parses valve data with format:
//   ValveID  Node1  Node2  Diam  Type  Setting  Mloss
//-----------------------------------------------------
var
  n1,n2: Integer;
  ID   : String;
  aLink: TLink;
begin
  if Ntoks < 3 then Result := 201
  else
  begin
    ID := TokList[0];
    if not NodeList.Find(TokList[1],n1)
    or not NodeList.Find(TokList[2],n2) then Result := 203
    else
    begin
      aLink := TLink.Create;
      aLink.Node1 := TNode(NodeList.Objects[n1]);
      aLink.Node2 := TNode(NodeList.Objects[n2]);
      aLink.Zindex := VALVES;
      Uutils.CopyStringArray(DefProp[VALVES].Data,aLink.Data);
      aLink.Data[COMMENT_INDEX] := Comment;
      if Ntoks > 3 then aLink.Data[VALVE_DIAM_INDEX] := TokList[3];
      if Ntoks > 4 then aLink.Data[VALVE_TYPE_INDEX] := TokList[4];
      if Ntoks > 5 then aLink.Data[VALVE_SETTING_INDEX] := TokList[5];
      if Ntoks > 6 then aLink.Data[VALVE_MLOSS_INDEX] := TokList[6];
      Network.Lists[VALVES].AddObject(ID,aLink);
      LinkList.AddObject(ID,aLink);
      Result := 0;
    end;
  end;
end;

function ReadControlData(Line: String): Integer;
//---------------------------------------------
// Adds input line to list of simple controls
//---------------------------------------------
begin
  Network.SimpleControls.Add(Line);
  Result := 0;
end;

function ReadRuleData(Line: String): Integer;
//-----------------------------------------------
// Adds input line to list of rule-based controls
//-----------------------------------------------
begin
  Network.RuleBasedControls.Add(Line);
  Result := 0;
end;

procedure AddDemand(aJunc: TJunc);
//--------------------------------------------------
// Adds new demand record to junction's Demands list
//--------------------------------------------------
var
  s: String;
begin
// Concatenate tokens into a demand record
  s := TokList[1] + ' ' + #13;
  if (Ntoks > 2) then s := s + TokList[2] + ' ' + #13
  else s := s + ' ' + #13;
  s := s + Comment + ' ';

// Add demand record to list
  aJunc.Demands.Add(s);

// If first record, then update junction's property slots
  if (aJunc.Demands.Count = 1) then
  begin
    aJunc.Data[JUNC_DEMAND_INDEX] := TokList[1];
    if (Ntoks > 2) then aJunc.Data[JUNC_PATTERN_INDEX] := TokList[2];
  end;
end;

function ReadDemandData: Integer;
//----------------------------------------------
// Parses junction demand statement with format:
//  JuncID  demand  (pattern)
//  MULTIPLY factor
//----------------------------------------------
var
  index: Integer;
  aNode: TNode;
  x    : Single;
begin
  Result := 0;
  if Ntoks < 2 then Result := 201
  else if CompareText(MULTIPLY,TokList[0]) = 0 then
  begin
    if not Uutils.GetSingle(TokList[1],x) then Result := 201
    else Network.Options.Data[DEMAND_MULT_INDEX] := TokList[1];
  end
  else
  begin
    if not NodeList.Find(TokList[0],index) then Result := 203
    else
    begin
      aNode := TNode(NodeList.Objects[index]);
      if (aNode.Zindex = JUNCS) then
        AddDemand(TJunc(NodeList.Objects[index]));
    end;
  end;
end;

function ReadRoughnessData: Integer;
//-----------------------------------------
// Parses pipe roughness line with format:
//   PipeID  value
//   PipeID1 PipeID2 value
//-----------------------------------------
var
  I, I1, I2: Integer;
  Index    : Integer;
  aLink    : TLink;
  Found    : Boolean;
begin
  if Ntoks < 2 then
    Result := 201
  else
  begin
    I1 := StrToIntDef(TokList[0],0);
    I2 := I1;
    if Ntoks = 3 then I2 := StrToIntDef(TokList[1],0);
    for I := I1 to I2 do
    begin
      if I1 > 0 then Found := LinkList.Find(IntToStr(I),Index)
      else           Found := LinkList.Find(TokList[0],Index);
      if Found then
      begin
        aLink := TLink(LinkList.Objects[Index]);
        if (aLink.Zindex = PIPES) then
          aLink.Data[PIPE_ROUGH_INDEX] := TokList[Ntoks-1];
      end;
    end;
    Result := 0;
  end;
end;

function ReadEnergyData: Integer;
//--------------------------------------------
// Parses line of ENERGY section with formats:
//   GLOBAL EFFIC/PRICE/PATTERN  value
//   PUMP   pumpID  EFFIC/PRICE/PATTERN  value
//   DEMAND CHARGE  value
//--------------------------------------------
var
  index: Integer;
  j, k : Integer;
  code : Integer;
  aLink: TLink;
begin
  code := 0;
  if Ntoks < 3 then code := 201
  else
  begin
    j := Uutils.FindKeyWord(TokList[0],EnergyWords,4);
    case j of
    0: begin  {GLOBAL}
         k := Uutils.FindKeyWord(TokList[1],EnergyWords,4);
         if k = 2 then Network.Options.Data[EFFIC_INDEX] := TokList[2]
         else if k = 3 then Network.Options.Data[EPRICE_INDEX] := TokList[2]
         else if k = 4 then Network.Options.Data[PRICE_PAT_INDEX] := TokList[2]
         else code := 201;
       end;
    1: begin  {PUMP}
         if Ntoks < 4 then code := 201
         else
         begin
           if not LinkList.Find(TokList[1],index) then code := 204
           else
           begin
             aLink := TLink(LinkList.Objects[index]);
             if (aLink.Zindex = PUMPS) then
             begin
               k := Uutils.FindKeyWord(TokList[2],EnergyWords,4);
               if k = 2 then aLink.Data[PUMP_ECURVE_INDEX] := TokList[3]
               else if k = 3 then aLink.Data[PUMP_EPRICE_INDEX] := TokList[3]
               else if k = 4 then aLink.Data[PUMP_PRICEPAT_INDEX] := TokList[3]
               else code := 201;
             end
             else code := 201;
           end;
         end;
       end;
    5: Network.Options.Data[DMND_CHARGE_INDEX] := TokList[2];
    end;
  end;
  Result := code;
end;

function ReadEmitterData: Integer;
//------------------------------------------
// Parses junction emitter data with format:
//   JuncID  value
//------------------------------------------
var
  aNode: TNode;
  index: Integer;
begin
  if Ntoks < 2 then
    Result := 201
  else
  begin
    if not NodeList.Find(TokList[0],index) then Result := 203
    else
    begin
      aNode := TNode(NodeList.Objects[Index]);
      if aNode.Zindex = JUNCS then
        aNode.Data[JUNC_EMITTER_INDEX] := TokList[1];
      Result := 0;
    end;
  end;
end;

function ReadSourceData: Integer;
//-----------------------------------------------------------
// Parses WQ source data with formats:
//   nodeID  CONCEN/MASS/SETPOINT/FLOWPACED  value  (pattern)
//   nodeID  value  (pattern)
//-----------------------------------------------------------
var
  aNode  : TNode;
  index  : Integer;
  i,j,k  : Integer;
  x      : Single;
  stype  : String;
begin
  if Ntoks < 2 then
    Result := 201
  else
  begin
    if not NodeList.Find(TokList[0],index) then Result := 203
    else
    begin
      aNode := TNode(NodeList.Objects[Index]);
      case aNode.Zindex of
      RESERVS: i := RES_SRCQUAL_INDEX;
      TANKS:   i := TANK_SRCQUAL_INDEX;
      else     i := JUNC_SRCQUAL_INDEX;
      end;
      k := 2;
      stype := SourceType[0];
      j := Uutils.FindKeyWord(TokList[1], SourceWords,4);
      if j >= 0 then stype := SourceType[j]
      else k := 1;
      if not Uutils.GetSingle(TokList[k],x) then Result := 201
      else
      begin
        aNode.Data[i] := TokList[k];
        if (Ntoks > k+1) then aNode.Data[i+1] := TokList[k+1];
        aNode.Data[i+2] := stype;
        Result := 0;
      end;
    end;
  end;
end;

function ReadPatternData: Integer;
//--------------------------------------
// Parses time pattern data with format:
//   Pattern ID   Mutipliers...
//--------------------------------------
var
  J, Index: Integer;
  ID      : String;
  aPattern: TPattern;
begin
// Check if ID is same as one for previous input line
  ID := TokList[0];
  if (ID = PrevID) then Index := PrevIndex
  else Index := Network.Lists[PATTERNS].IndexOf(ID);

// If starting a new pattern then create it
  if Index < 0 then
  begin
    aPattern := TPattern.Create;
    aPattern.Comment := PrevComment;
    Network.Lists[PATTERNS].AddObject(ID, aPattern);
    Index := Network.Lists[PATTERNS].Count - 1;
    PrevID := ID;
    PrevIndex := Index;
  end;

// Add multipliers to the list for the pattern
  aPattern := TPattern(Network.Lists[PATTERNS].Objects[Index]);
  for J := 1 to Ntoks-1 do
    aPattern.Multipliers.Add(TokList[J]);
  Result := 0;
end;

function ReadCurveData: Integer;
//-------------------------------
// Parses Curve data with format:
//   Curve ID  X-value  Y-value
//-------------------------------
var
  Index   : Integer;
  I, J    : Integer;
  ID      : String;
  aCurve  : TCurve;
begin
// Check for too few tokens
  Result := 0;
  if Ntoks < 3 then
    Result := 201
  else
  begin

  // Check if curve ID is same as for previous line
    ID := TokList[0];
    if (ID = PrevID) then Index := PrevIndex
    else Index := Network.Lists[CURVES].IndexOf(ID);

  // If starting input for a new curve then create it
    if Index < 0 then
    begin
      aCurve := TCurve.Create;
      aCurve.Comment := PrevComment;
      aCurve.Ctype := 'PUMP';

    // See if the comment on the previous line contains a curve type
    // (e.g., ";PUMP: rest of comment")
      J := Pos(':',PrevComment);
      if J > 0 then
      begin
        I := Uutils.FindKeyword(Trim(Copy(PrevComment,2,J-1)),CurveLabel,10);
        if I >= 0 then
        begin
          aCurve.Ctype := CurveLabel[I];
          aCurve.Comment := Copy(PrevComment,J+1,255);
        end;
      end;
      Network.Lists[CURVES].AddObject(ID,aCurve);
      Index := Network.Lists[CURVES].Count - 1;
      PrevID := ID;
      PrevIndex := Index;
    end;

  // Add x,y values to the list maintained by the curve
    aCurve := TCurve(Network.Lists[CURVES].Objects[Index]);
    aCurve.Xdata.Add(TokList[1]);
    aCurve.Ydata.Add(TokList[2]);
  end;
end;

function ReadQualityData: Integer;
//----------------------------------
// Parses quality data with formats:
//   NodeID  value
//   NodeID1 NodeID2 value
//----------------------------------
var
  I, I1, I2: Integer;
  J, Index : Integer;
  aNode    : TNode;
  Found    : Boolean;
begin
  if Ntoks < 2 then
    Result := 201
  else
  begin
    I1 := StrToIntDef(TokList[0],0);
    I2 := I1;
    if Ntoks = 3 then I2 := StrToIntDef(TokList[1],0);
    for I := I1 to I2 do
    begin
      if I1 > 0 then Found := NodeList.Find(IntToStr(I),Index)
      else           Found := NodeList.Find(TokList[0],Index);
      if Found then
      begin
        aNode := TNode(NodeList.Objects[Index]);
        case aNode.Zindex of
          RESERVS: J := RES_INITQUAL_INDEX;
          TANKS:   J := TANK_INITQUAL_INDEX;
          else     J := JUNC_INITQUAL_INDEX;
        end;
        aNode.Data[J] := TokList[Ntoks-1];
      end;
    end;
    Result := 0;
  end;
end;

function ReadStatusData: Integer;
//-------------------------------------
// Parses link status data with format:
//   LinkID  status
//-------------------------------------
var
  I, I1, I2: Integer;
  Index    : Integer;
  aLink    : TLink;
  Found    : Boolean;
  status   : Integer;    {Open = 0, Closed = 1}
  numflag  : Boolean;    {Numerical setting supplied}
  y        : Single;     {Numerical setting}
begin
// Check for sufficient number of items
  if Ntoks < 2 then
    Result := 201

// Check if line contains a single link ID or a range of numerical IDs
  else
  begin
    I1 := StrToIntDef(TokList[0],0);
    I2 := I1;
    if Ntoks = 3 then I2 := StrToIntDef(TokList[1],0);

  // Examine each link in the range
    for I := I1 to I2 do
    begin

    // Check that link exists
      if I1 > 0 then Found := LinkList.Find(IntToStr(I),Index)
      else           Found := LinkList.Find(TokList[0],Index);
      if Found then
      begin

      // Parse last token which contains status value
        aLink := TLink(LinkList.Objects[Index]);
        status := 0;
        numflag := Uutils.GetSingle(TokList[Ntoks-1],y);

      // Token is a word. Check that its a valid status word.
        if not numflag then
          status := Uutils.FindKeyWord(TokList[Ntoks-1],StatusWords,4);
        if (status < 0) then
        begin
          Result := 201;
          Exit;
        end;

      // Update link's status
        case aLink.Zindex of
        PIPES:   if status = 1 then aLink.Data[PIPE_STATUS_INDEX] := 'Closed'
                 else aLink.Data[PIPE_STATUS_INDEX] := 'Open';
        PUMPS:   // If status is a number then update pump speed setting
                 if numflag then
                   aLink.Data[PUMP_SPEED_INDEX] := TokList[Ntoks-1]
                 else if status = 1 then
                   aLink.Data[PUMP_STATUS_INDEX] := 'Closed'
                 else aLink.Data[PUMP_STATUS_INDEX] := 'Open';
        VALVES:  // If status is a number then update valve setting
                 if numflag then
                   aLink.Data[VALVE_SETTING_INDEX] := TokList[Ntoks-1]
                 else if status = 1 then
                   aLink.Data[VALVE_STATUS_INDEX] := 'Closed'
                 else
                   aLink.Data[VALVE_STATUS_INDEX] := 'Open';
        end;
      end;
    end;
    Result := 0;
  end;
end;

procedure GetPipeReactData(const DataIndex: Integer);
//-------------------------------------------------------
// Parses pipe reaction data with format:
//    BULK/WALL  Link1  {Link2}  value
// where DataIndex reflects whether BULK or WALL applies.
//-------------------------------------------------------
var
  I, I1, I2: Integer;
  Index    : Integer;
  aLink    : TLink;
  Found    : Boolean;
begin
  I1 := StrToIntDef(TokList[1],0);
  I2 := I1;
  if Ntoks = 4 then I2 := StrToIntDef(TokList[2],0);
  for I := I1 to I2 do
  begin
    if I1 > 0 then Found := LinkList.Find(IntToStr(I),Index)
    else           Found := LinkList.Find(TokList[1],Index);
    if Found then
    begin
      aLink := TLink(LinkList.Objects[Index]);
      if (aLink.Zindex = PIPES) then
        aLink.Data[DataIndex] := TokList[Ntoks-1];
    end;
  end;
end;

procedure GetTankReactData;
//---------------------------------------
// Parses pipe reaction data with format:
//    TANK  Node1  {Node2}  value
//---------------------------------------
var
  I, I1, I2: Integer;
  Index    : Integer;
  aNode    : TNode;
  Found    : Boolean;
begin
  I1 := StrToIntDef(TokList[1],0);
  I2 := I1;
  if Ntoks = 4 then I2 := StrToIntDef(TokList[2],0);
  for I := I1 to I2 do
  begin
    if I1 > 0 then Found := NodeList.Find(IntToStr(I),Index)
    else           Found := NodeList.Find(TokList[1],Index);
    if Found then
    begin
      aNode := TNode(NodeList.Objects[Index]);
      if (aNode.Zindex = TANKS) then
        aNode.Data[TANK_KBULK_INDEX] := TokList[Ntoks-1];
    end;
  end;
end;

function ReadReactionData: Integer;
//------------------------------------------------
// Parses pipe or tank reaction data with formats:
//   ORDER   BULK/WALL   value
//   GLOBAL  BULK/WALL   value
//   BULK/WALL/TANK  id1 {id2}  value
//   LIMITING POTENTIAL  value
//   ROUGHNESS CORREL    value
//------------------------------------------------
var
  i    : Integer;
  v    : Single;
  s    : String;
begin
// Check for sufficient number of tokens
  Result := 0;
  if Ntoks < 3 then Result := 201
  else
  begin

  // Find Reaction keyword
    i := Uutils.FindKeyWord(TokList[0],ReactWords,4);
    if i = -1 then Result := 201

  // Check for numerical option
    else
    begin
      s := TokList[Ntoks-1];
      if not Uutils.GetSingle(s,v) then Result := 201
      else case i of
        0:  begin  {Reaction ORDER }

          // Check if for BULK or WALL
            if (UpperCase(TokList[1]) = ReactWords[4]) then
              Network.Options.Data[BULK_ORDER_INDEX] := s
            else if (UpperCase(TokList[1]) = ReactWords[5]) then
            begin

            // If WALL ORDER check for a 0 or 1
              if Uutils.GetSingle(s,v) then
              begin
                if v = 0 then
                Network.Options.Data[WALL_ORDER_INDEX] := OrderWords[0]
                else if v = 1 then
                  Network.Options.Data[WALL_ORDER_INDEX] := OrderWords[1]
                else Result := 201;
              end;
            end

          // If TANK ORDER then skip it
            else if (UpperCase(TokList[1]) <> ReactWords[6]) then Result := 201;
            end;

        1:  begin  {GLOBAL Reaction coeff. - check for BULK or WALL}
            if (UpperCase(TokList[1]) = ReactWords[4]) then
              Network.Options.Data[GLOBAL_KBULK_INDEX] := s
            else if (UpperCase(TokList[1]) = ReactWords[5]) then
              Network.Options.Data[GLOBAL_KWALL_INDEX] := s
            else Result := 201;
            end;

        2:  begin  {LIMITING POTENTIAL}
            Network.Options.Data[LIMIT_QUAL_INDEX] := s;
            end;

        3:  begin  {ROUGHNESS CORRELATION}
            Network.Options.Data[ROUGH_CORREL_INDEX] := s;
            end;

        4:  begin  {BULK coeff. for specific pipe(s)}
            GetPipeReactData(PIPE_KBULK_INDEX);
            end;

        5:  begin  {WALL coeff. for specific pipe}
            GetPipeReactData(PIPE_KWALL_INDEX);
            end;

        6:  begin  {TANK bulk coeff.}
            GetTankReactData;
            end;
        else Result := 201;
      end;
    end;
  end;
end;

function ReadMixingData: Integer;
//-------------------------------------------
// Parses tank mixing model data with format:
//  TankID  model  (fraction)
//-------------------------------------------
var
  Index : Integer;
  Mtype : Integer;
  Numflag  : Boolean;
  y     : Single;
  aNode : TNode;
begin
  Result := 0;
  if Ntoks < 2 then Result := 201
  else
  begin
    if not NodeList.Find(TokList[0],Index) then Result := 203
    else
    begin
      aNode := TNode(NodeList.Objects[Index]);
      if (aNode.Zindex = TANKS) then
      begin
        Mtype := Uutils.FindKeyWord(TokList[1],MixWords,4);
        if (Mtype < 0) then Result := 201
        else
        begin
          aNode.Data[TANK_MIXMODEL_INDEX] := TokList[1];
          if (Ntoks >= 3) then
          begin
            Numflag := Uutils.GetSingle(TokList[2],y);
            if (Numflag) then
              aNode.Data[TANK_MIXFRAC_INDEX] := TokList[2];
          end;
        end;
      end;
    end;
  end;
end;

function ReadReportData: Integer;
//------------------------------
// Reads [REPORT] Status choice
//------------------------------
begin
  if (UpperCase(TokList[0]) = 'STATUS') then
  begin
    if (UpperCase(TokList[1]) = 'YES') then
      Network.Options.Data[STATUS_RPT_INDEX] := 'Yes'
    else if (UpperCase(TokList[1]) = 'FULL') then
      Network.Options.Data[STATUS_RPT_INDEX] := 'Full';
  end;
  Result := 0;
end;

function ReadTimeData: Integer;
//-----------------------------------------
// Parses line of data from [TIMES] section
//-----------------------------------------
var
  Index   : Integer;
  y       : Single;
  W       : array[0..255] of Char;
  S       : String;
begin
  Result := 0;
  if Ntoks = 1 then Exit;

// Check for 'STATISTIC' keyword
  if (UpperCase(TokList[0]) = 'STATISTIC') then
  begin
    Index := Uutils.FindKeyWord(TokList[1],TimeStatWords,4);
    if Index >= 0 then
       Network.Options.Data[TIME_STAT_INDEX] := TimeStatWords[Index];
    Exit;
  end;

// Check for 'Start Clocktime' keyword
  if Uutils.FindKeyWord(TokList[0],[TimeWords[5]],4) = 0 then
  begin
    if Ntoks < 3 then Exit;
    Network.Options.Data[START_TIME_INDEX] := TokList[2];
    if Ntoks >= 4 then
      Network.Options.Data[START_TIME_INDEX] :=
        Network.Options.Data[START_TIME_INDEX] + ' ' + TokList[3];
    Exit;
  end;

// For all other options, if last token not a number,
// then retrieve time value and units
  y := Uutils.StrHoursToFloat(TokList[Ntoks-1]);
  if y < 0 then
  begin
    y := Uutils.StrHoursToFloat(TokList[Ntoks-2]);
    Index := Uutils.FindKeyWord(TokList[Ntoks-1],TimeUnitWords,4);
    if (Index < 0) or (y < 0) then
    begin
      Result := 201;
      Exit;
    end;
    if Pos(':',TokList[Ntoks-2]) = 0 then  {Time given as a decimal}
    begin
      if Index = 0 then y := 24*y;       {Convert days to hours}
      if Index = 3 then y := y/60.0;     {Convert minutes to hours}
      if Index = 4 then y := y/3600.0;   {Convert seconds to hours}
    end;
  end;                                 {Now y = time in hours}
  S := Uutils.GetTimeString(Round(y*3600));

// Find which Times option applies
  Index := Uutils.FindKeyWord(TokList[0],TimeWords,4);
  case Index of
  0:      {Duration in hours}
    Network.Options.Data[DURATION_INDEX] := S;

  1:      {Hydraulic Timestep in hours}
    Network.Options.Data[HYD_TSTEP_INDEX] := S;

  2:      {Quality Timestep in minutes}
    Network.Options.Data[DURATION_INDEX+Index] := S;

  3:      {Pattern Timestep or Start in hours}
    if Ntoks > 2 then
    begin
      StrPcopy(W,UpperCase(TokList[1]));
      if StrPos(W,'START') <> nil then
        Network.Options.Data[PAT_START_INDEX] := S
      else
        Network.Options.Data[PAT_TSTEP_INDEX] := S;
    end;

  4:      {Report Timestep or Start in hours}
    if Ntoks > 2 then
    begin
      StrPcopy(W,UpperCase(TokList[1]));
      if StrPos(W,'START') <> nil then
        Network.Options.Data[RPT_START_INDEX] := S
      else
        Network.Options.Data[RPT_TSTEP_INDEX] := S;
    end;

  5,6,7: begin end;  {Skip Start Clocktime, Rule Timestep & Minimum Traveltime}
  else
    Result := 201;
  end;
end;

function ReadOptionData: Integer;
//--------------------------------------------
// Parses line of input from [OPTIONS] section
//
// Modified for release 2.00.012.
//--------------------------------------------
var
  Index   : Integer;
  Keyword : String;
begin

// Check which keyword applies
  Result := 0;
  Keyword := TokList[0];
  Index := Uutils.FindKeyWord(Keyword,OptionWords,4);
  if Ntoks >= 2 then case Index of

  // Flow units option
    0: if UpperCase(TokList[1]) = 'SI' then
         Network.Options.Data[FLOW_UNITS_INDEX] := 'LPS'
       else
           Network.Options.Data[FLOW_UNITS_INDEX] := UpperCase(TokList[1]);

  // Headloss formula or Head Error option
    1: if CompareText(Keyword, 'HEADERROR') = 0
       then Network.Options.Data[HEAD_ERROR_INDEX] := TokList[1]
       else Network.Options.Data[HLOSS_FORM_INDEX] := UpperCase(TokList[1]);

  // Specific Gravity option
    2: if Ntoks >= 3 then
         Network.Options.Data[SPEC_GRAV_INDEX] := TokList[2];

  // Demand Multiplier or Demand Model option
    8: if Ntoks >= 3 then
       begin
         if CompareText(TokList[1], 'MODEL') = 0
         then Network.Options.Data[DEMAND_MODEL_INDEX] := TokList[2]
         else Network.Options.Data[DEMAND_MULT_INDEX] := TokList[2];
       end;

  // Emitter Exponent option
    9: if Ntoks >= 3 then
         Network.Options.Data[EMITTER_EXP_INDEX] := TokList[2];

    3,  //Rel. Viscosity
    4,  //Max Trials
    5,  //Unbalanced
    6,  //Accuracy
    7,  //Demand pattern
    13, //Rel. Diffusivity
    15, //Qual. Tolerance
    16: //Max Segments
       Network.Options.Data[Index] := TokList[1];

  // Quality option
    11: begin
          Network.Options.Data[QUAL_PARAM_INDEX] := TokList[1];
          if Ntoks >= 3 then
          begin
            if CompareText(TokList[1],'TRACE') = 0
            then Network.Options.Data[TRACE_NODE_INDEX] := TokList[2]
            else Network.Options.Data[QUAL_UNITS_INDEX] := TokList[2];
          end;
        end;

  // Pressure Units (not used) or Pressure Exponent option
   12: if (Ntoks >= 3) and (CompareText(TokList[1], 'EXPONENT') = 0)
       then Network.Options.Data[PRESSURE_EXP_INDEX] := TokList[2];

  // Map file option
   17: MapFile := TokList[1];

  // Flow Change option
   18: Network.Options.Data[FLOW_CHANGE_INDEX] := TokList[1];

  // Minimum Pressure & Required Pressure options
   19: if Ntoks >= 3 then Network.Options.Data[MIN_PRESSURE_INDEX] := TokList[2];
   20: if Ntoks >= 3 then Network.Options.Data[REQ_PRESSURE_INDEX] := TokList[2];

  // CHECKFREQ, MAXCHECK & DAMPLIMIT options
   21: Network.Options.Data[CHECK_FREQ_INDEX] := TokList[1];
   22: Network.Options.Data[MAX_CHECK_INDEX] := TokList[1];
   23: Network.Options.Data[DAMP_LIMIT_INDEX] := TokList[1];

  else
    Result := 201;
  end;
end;

function ReadCoordData: Integer;
//-----------------------------------------
// Parses node coordinate data with format:
//   NodeID  X-coord.  Y-coord.
//-----------------------------------------
var
  index: Integer;
  ntype: Integer;
  x,y  : Extended;
  aNode: TNode;
begin
  Result := 0;
  if (Ntoks < 3) then Result := 201
  else
  begin

  // Locate the node ID in the database
  // Search the NodeList if reading a new input file
    aNode := nil;
    if NodeList.Find(TokList[0],index) then
      aNode := TNode(NodeList.Objects[index])
    else if (FileType = ftImport)
    and (Uinput.FindNode(TokList[0],ntype,index)) then
      aNode := Node(ntype,index);

  // If node exists then assign it X & Y coordinates
    if (aNode <> nil) then
    begin
      if not Uutils.GetExtended(TokList[1],x) then Result := 201
      else
      begin
        if not Uutils.GetExtended(TokList[2],y) then Result := 201
        else
        begin
          aNode.X := x;
          aNode.Y := y;
        end;
      end;
    end;
  end;
end;

function ReadVertexData: Integer;
//-----------------------------------------
// Parses link vertex data with format:
//   LinkID  X-coord.  Y-coord.
//-----------------------------------------
var
  Index: Integer;
  Ltype: Integer;
  x,y  : Extended;
  aLink: TLink;
  aVertex: PVertex;
begin
  Result := 0;
  if (Ntoks < 3) then Result := 201
  else
  begin

  // Locate the link ID in the database
  // Search the LinkList if reading a new input file
    aLink := nil;
    if (FileType = ftInput) then
    begin
      if LinkList.Find(TokList[0],index) then
        aLink := TLink(LinkList.Objects[index]);
    end

  // Search existing database if only importing a new map file
    else if (Uinput.FindLink(TokList[0],Ltype,Index)) then
      aLink := Link(Ltype,Index);

  // If link exists then assign it X & Y coordinates
    if (aLink <> nil) then
    begin
      if not Uutils.GetExtended(TokList[1],x) then Result := 201
      else
      begin
        if not Uutils.GetExtended(TokList[2],y) then Result := 201
        else
        begin
          new(aVertex);
          aVertex^.X := x;
          aVertex^.Y := y;
          aVertex^.Next := aLink.Vlist;
          aLink.Vlist := aVertex;
        end;
      end;
    end;
  end;
end;

function ReadLabelData: Integer;
//---------------------------------------
// Parses map label data with format:
//   X-coord.  Y-coord.  Label  (Anchor)
//---------------------------------------
var
  ntype    : Integer;
  index    : Integer;
  x, y     : Extended;
  s        : String;
  aMapLabel: TMapLabel;
begin
  if (Ntoks < 3) then Result := 201
  else
  begin
    if not Uutils.GetExtended(TokList[0],x) then Result := 201
    else
    begin
      if not Uutils.GetExtended(TokList[1],y) then Result := 201
      else
      begin
        s := TokList[2];
        aMapLabel := TMapLabel.Create;
        aMapLabel.X := x;
        aMapLabel.Y := y;
        Network.Lists[LABELS].AddObject(s,aMapLabel);
        if Ntoks = 4 then
        begin
          if Uinput.FindNode(TokList[3],ntype,index) then
            aMapLabel.Anchor := Node(ntype,index);
        end;
        Result := 0;
      end;
    end;
  end;
end;

function ReadBackdropData: Integer;
var
  Index   : Integer;
  Keyword : String;
  i: Integer;
  x: array[1..4] of Extended;
begin
// Check which keyword applies
  Result := 201;
  Keyword := TokList[0];
  Index := Uutils.FindKeyWord(Keyword,BackdropWords,4);
  case Index of
    0:  // Map dimensions
    begin
      if Ntoks < 5 then Exit;
      for i := 1 to 4 do if not Uutils.GetExtended(TokList[i],x[i]) then Exit;
      with MapDimensions do
      begin
        LowerLeft.X := x[1];
        LowerLeft.Y := x[2];
        UpperRight.X := x[3];
        UpperRight.Y := x[4];
      end;
      DimensionsSet := True;
    end;

    1:  //Map units
    if Ntoks > 1 then
    begin
      i := Uutils.FindKeyWord(Copy(TokList[1],1,1),MapUnits,1);
      if i < 0 then Exit;
      MapDimensions.Units := TMapUnits(i);
    end;

    2:  //Backdrop file
    if Ntoks > 1 then MapBackdrop.Filename := TokList[1];

    3:  //Backdrop offset
    begin
      if Ntoks < 3 then Exit;
      for i := 1 to 2 do if not Uutils.GetExtended(TokList[i],x[i]) then Exit;
      MapBackdrop.Offset.X := x[1];
      MapBackdrop.Offset.Y := x[2];
    end;
    else Exit;
  end;
  Result := 0;
end;

function ReadTagData: Integer;
//------------------------------
// Parses Tag data with format:
//   NODE/LINK  ID  TAG
//------------------------------
var
  Index: Integer;
begin
  Result := 0;
  if (Ntoks < 3) then Result := 201
  else if CompareText(TokList[0],'NODE') = 0 then
  begin
    with NodeList do
    begin
      if not Find(TokList[1],Index) then Result := 203
      else
      TNode(Objects[Index]).Data[TAG_INDEX] := TokList[2];
    end;
  end
  else if CompareText(TokList[0],'LINK') = 0 then
  begin
    with LinkList do
    begin
      if not Find(TokList[1],Index) then Result := 204
      else
      TLink(Objects[Index]).Data[TAG_INDEX] := TokList[2];
    end;
  end
  else Result := 201;
end;

procedure AdjustMapDimensions(var zmin, zmax: Extended);
//-------------------------------------------------------------
// Adjusts min & max map extent in case they are the same value
//-------------------------------------------------------------
var
  dz: Extended;
begin
  if zmin = 0 then
  begin
    zmin := -5;
    zmax := 5;
  end
  else
  begin
    dz := 0.05*Abs(zmax);
    zmin := zmin - dz;
    zmax := zmax + dz;
  end;
end;

procedure SetMapDimensions(var NewDimensions: TMapDimensions);
//--------------------------------------------------------------
// Determines map dimensions based on range of nodal coordinates
//--------------------------------------------------------------
var
  n, ntype: Integer;
  index   : Integer;
  xLL, xUR: Extended;
  yLL, yUR: Extended;
  z       : Extended;
  aNode   : TNode;
begin
  xLL := -MISSING;
  xUR := MISSING;
  yLL := -MISSING;
  yUR := MISSING;
  for ntype := JUNCS to TANKS do
  begin
    n := Network.Lists[ntype].Count - 1;
    for index := 0 to n do
    begin
      aNode := Node(ntype,index);
      z := aNode.X;
      if (z <> MISSING) then
      begin
        if (z < xLL) then xLL := z;
        if (z > xUR) then xUR := z;
      end;
      z := aNode.Y;
      if (z <> MISSING) then
      begin
        if (z < yLL) then yLL := z;
        if (z > yUR) then yUR := z;
      end;
    end;
  end;
  if (xLL <> -MISSING) and (xUR <> MISSING) then with NewDimensions do
  begin
    if xLL = xUR then AdjustMapDimensions(xLL,xUR);
    LowerLeft.X := xLL - 0.05*(xUR - xLL);
    UpperRight.X := xUR + 0.05*(xUR - xLL);
  end;
  if (yLL <> -MISSING) and (yUR <> MISSING) then with NewDimensions do
  begin
    if yLL = yUR then AdjustMapDimensions(yLL,yUR);
    LowerLeft.Y := yLL - 0.05*(yUR - yLL);
    UpperRight.Y := yUR + 0.05*(yUR - yLL);
  end;
end;

function ParseInpLine(Line: String): Integer;
//----------------------------------------------------
// Parses current input line depending on type of data
//----------------------------------------------------
begin
  case Section of
    1:    Result := ReadJunctionData;
    2..3: Result := ReadTankData;
    4:    Result := ReadPipeData;
    5:    Result := ReadPumpData;
    6:    Result := ReadValveData;
    9:    Result := ReadDemandData;
    10:   Result := ReadRoughnessData;
    11:   Result := ReadEnergyData;
    12:   Result := ReadEmitterData;
    13:   Result := ReadSourceData;
    14:   Result := ReadPatternData;
    15:   Result := ReadCurveData;
    16:   Result := ReadQualityData;
    17:   Result := ReadStatusData;
    18:   Result := ReadReactionData;
    19:   Result := ReadMixingData;
    20:   Result := ReadReportData;
    21:   Result := ReadTimeData;
    22:   Result := ReadOptionData;
   {23: = [END] keyword (no data to read)}
    24:   Result := ReadCoordData;
    25:   Result := ReadVertexdata;
    26:   Result := ReadLabelData;
    27:   Result := ReadBackdropData;
    29:   Result := ReadTagData;
    else  Result := 0;
  end;
end;

function ImportDemandData: Integer;
//-------------------------------
// Processes imported demand data
//-------------------------------
var
  Index: Integer;
  aJunc: TJunc;
begin
  if (Ntoks < 2) then Result := 201
  else with NodeList do
  begin
    if not Find(TokList[0],Index) then Result := 203
    else
    begin
      aJunc := TJunc(Objects[Index]);
      if StrToInt(aJunc.Data[JUNC_DMNDCAT_INDEX]) > 0 then
      begin
        aJunc.Demands.Clear;
        aJunc.Data[JUNC_DMNDCAT_INDEX] := '0';
      end;
      AddDemand(aJunc);
      Result := 0;
    end;
  end;
end;

function ImportDiameterData: Integer;
//---------------------------------------
// Processes imported pipe diameter value
//---------------------------------------
var
  Index: Integer;
begin
  if (Ntoks < 2) then Result := 201
  else with LinkList do
  begin
    if not Find(TokList[0],Index) then Result := 204
    else
    begin
      TLink(Objects[Index]).Data[PIPE_DIAM_INDEX] := TokList[1];
      Result := 0;
    end;
  end;
end;

function ImportRoughnessData: Integer;
//----------------------------------------
// Processes imported pipe roughness value
//----------------------------------------
var
  Index: Integer;
begin
  if (Ntoks < 2) then Result := 201
  else with LinkList do
  begin
    if (not Find(TokList[0],Index)) then Result := 204
    else
    begin
      TLink(Objects[Index]).Data[PIPE_ROUGH_INDEX] := TokList[1];
      Result := 0;
    end;
  end;
end;

function ImportQualityData: Integer;
//------------------------------------
// Processes imported initial WQ value
//------------------------------------
var
  Index: Integer;
begin
// Too few tokens
  Result := 203;
  if (Ntoks < 2) then Result := 201

// Search sorted list of junctions
  else if NodeList.Find(TokList[0],Index) then
  begin
    TJunc(NodeList.Objects[Index]).Data[JUNC_INITQUAL_INDEX] := TokList[1];
    Result := 0;
  end

// Search original lists of reservoirs & tanks
  else
  begin
    Index := Network.Lists[RESERVS].IndexOf(TokList[0]);
    if Index >= 0 then
    begin
      Node(RESERVS,Index).Data[RES_INITQUAL_INDEX] := TokList[1];
      Result := 0;
    end
    else
    begin
      Index := Network.Lists[TANKS].IndexOf(TokList[0]);
      if Index >= 0 then
      begin
        Node(TANKS,Index).Data[TANK_INITQUAL_INDEX] := TokList[1];
        Result := 0;
      end;
    end;
  end;
end;

function ImportReactionData: Integer;
//----------------------------------------------
// Processes imported pipe reaction coeff. value
//----------------------------------------------
var
  j, k, Index: Integer;
begin
  if (Ntoks < 3) then Result := 201
  else
  begin
    k := Uutils.FindKeyWord(TokList[0],ReactWords,4);
    case k of
    4,5:  begin  //Pipe bulk or wall reaction coefficient
            if (not LinkList.Find(TokList[1],Index)) then
              Result := 204
            else
            begin
              if k = 4 then j := PIPE_KBULK_INDEX
              else          j := PIPE_KWALL_INDEX;
              TLink(LinkList.Objects[Index]).Data[j] := TokList[2];
              Result := 0;
            end;
          end;
    6:    begin  //Tank bulk reaction coefficient
            Index := Network.Lists[TANKS].IndexOf(TokList[0]);
            if Index < 0 then Result := 203
            else
            begin
              Node(TANKS,Index).Data[TANK_KBULK_INDEX] := TokList[2];
              Result := 0;
            end;
          end;
    else  Result := 201;
    end;
  end;
end;

function ParseImportLine(Line: String): Integer;
//-----------------------------------------------------------
// Processes line of input from imported scenario or map file
//-----------------------------------------------------------
begin
  case Section of
    9:    Result := ImportDemandData;
    10:   Result := ImportRoughnessData;
    16:   Result := ImportQualityData;
    18:   Result := ImportReactionData;
    24:   Result := ReadCoordData;
    25:   Result := ReadVertexdata;
    26:   Result := ReadLabelData;
    27:   Result := ReadBackdropData;
    28:   Result := ImportDiameterData;
    else  Result := 0;
  end;
end;

procedure StripComment(const Line: String; var S: String; var Comment: String);
//-----------------------------------------------------------
// Strips comment (text following a ';') from a line of input
//-----------------------------------------------------------
var
  P: Integer;
begin
  Comment := '';
  S := Line;
  P := Pos(';',S);
  if P > 0 then
  begin
    Comment := Copy(S,P+1,256);
    Delete(S,P,256);
  end;
end;


procedure SetDemandCounts;
//---------------------------------------------------------
// Determines number of demand categories for each junction
//---------------------------------------------------------
var
  i: Integer;
begin
// Examine each junction
  for i := 0 to Network.Lists[JUNCS].Count - 1 do
  with TJunc(Network.Lists[JUNCS].Objects[i]) do
  begin

  // If no demand categories specified then create one
    if Demands.Count = 0 then
      Demands.Add(Data[JUNC_DEMAND_INDEX] + #13
        + Data[JUNC_PATTERN_INDEX] + ' ' + #13 + ' ');

  // Update demand count for this junction
    Data[JUNC_DMNDCAT_INDEX] := IntToStr(Demands.Count);
  end;
end;


function StartNewSection(var DemandFlag: Boolean): Integer;
//---------------------------------------------------------
// Begins reading a new section of the input/import file.
//---------------------------------------------------------
var
  k : Integer;
begin
// Determine which section to begin
  k := Uutils.FindKeyWord(TokList[0],SectionWords,5);
  if (k >= 0) then
  begin

  //Update section code
    Section := k;
    PrevID := '';

  // If importing new controls then clear current ones
    if (Section = 7) and (FileType = ftImport) then
      Network.SimpleControls.Clear;
    if (Section = 8) and (FileType = ftImport) then
      Network.RuleBasedControls.Clear;

  // If importing demands or initial quality then
  // place junction data in a sorted stringlist
    if  (Section in [9,16])
    and (FileType = ftImport)
    and (NodeList.Count = 0) then
    begin
      NodeList.AddStrings(Network.Lists[JUNCS]);
      NodeList.Sort;
    end;
    if Section = 9 then DemandFlag := True;

  // If importing diameters, roughness or reactions then place pipe
  // data in a sorted stringlist
    if  (Section in [10, 18, 28])
    and (FileType = ftImport)
    and (LinkList.Count = 0) then
    begin
      LinkList.AddStrings(Network.Lists[PIPES]);
      LinkList.Sort;
    end;
    Result := 0;
  end
  else
    Result := 201;
end;


function ReadFile(var F: Textfile; const Fsize: LongInt):Boolean;
//------------------------------------------------
// Reads each line of .inp input file
//------------------------------------------------
var
  DemandFlag: Boolean;
  Err      : Integer;
  ErrCount : Integer;
  Lcount   : Integer;
  Bcount   : Integer;
  StepSize : Integer;
  Line     : String;
  S        : String;
begin
  DemandFlag := False;
  ErrCount := 0;
  Result := True;
  Lcount := 0;

// Initialize progress meter settings
  StepSize := Fsize div (MainForm.ProgressBar.Max
                div MainForm.ProgressBar.Step);
  Bcount := 0;

// Read each line of input file
  Reset(F);
  while not Eof(F) do
  begin
    Err := 0;
    Readln(F,Line);
    Inc(Lcount);
    Inc(Bcount,Length(Line));
    MainForm.UpdateProgressBar(Bcount,StepSize);

  // Parse line into tokens
    PrevComment := Comment;
    StripComment(Line,S,Comment);
    Uutils.Tokenize(S,TokList,Ntoks);

  // Check if line begins a new input section
    if (Ntoks > 0) and (Pos('[',TokList[0]) = 1) then
      Err := StartNewSection(DemandFlag)

  // Check if line contains title, control or rule
    else if (Section = 0) and (Ntoks > 0) then Err := ReadTitleData(Line)
    else if (Section = 7) then Err := ReadControlData(Line)
    else if (Section = 8) then Err := ReadRuleData(Line)

  // Check if in current section then process the input line
    else if (Ntoks > 0) and (Section >= 0) then
    begin
    // Parse an input file differently from an import file
      if FileType = ftInput then Err := ParseInpLine(Line)
      else Err := ParseImportLine(Line);
    end

  // No current section -- file was probably not an EPANET file
    else if (Ntoks > 0) then
    begin
      Result := False;
      Exit;
    end;

  // Report any error message
    if Err > 0 then
    begin
      if ErrCount <= 10 then
      begin
        ErrList.Add(TXT_ERROR + IntToStr(Err) + TXT_AT_LINE +
          IntToStr(Lcount) + ':');
        if Section >= 0 then ErrList.Add(SectionWords[Section] + ']');
        ErrList.Add(Line);
        ErrList.Add('');
      end;
      Inc(ErrCount);
    end;
  end;  //End of file.

// Determine demand count property for each junction.
  if (DemandFlag) or (FileType = ftInput) then SetDemandCounts;
  if ErrCount > 10 then ErrList.Add(
    IntToStr(ErrCount-10) + TXT_MORE_ERRORS);
end;


function ReadMapFile(const Fpath: String): Boolean;
//-------------------------------------------------
// Reads in a map file that was referenced in
// a network .INP input file.
//-------------------------------------------------
var
  F      : Textfile;
  Fsize  : LongInt;
begin
  if (ExtractFilePath(MapFile) = '') then
    MapFile := Fpath + MapFile;
  if FileExists(MapFile) then
  begin
    Fsize := Uutils.GetFileSize(MapFile);
    AssignFile(F,MapFile);
    {$I-}
    Reset(F);
    {$I+}
    if (IOResult = 0) then
    try
      Section := 24;  //[COORDINATES]
      Result := ReadFile(F,Fsize);
    finally
    end
    else
    begin
      Screen.Cursor := crDefault;
      Uutils.MsgDlg(MSG_NO_READ_MAP_FILE + MapFile, mtWarning, [mbOK]);
      Result := False
    end;
    CloseFile(F);
  end
  else
  begin
    Screen.Cursor := crDefault;
    Uutils.MsgDlg(MSG_MAP_FILE + MapFile + MSG_NOT_EXIST, mtWarning, [mbOK]);
    Result := False;
  end;
end;


procedure DisplayInpErrForm(const Fname: String);
//-----------------------------------------
// Displays InpErrForm form that lists any
// error messages.
//-----------------------------------------

begin
  with TInpErrForm.Create(Application) do
  try
    Memo1.Lines.Add(TXT_ERROR_REPORT + Fname);
    Memo1.Lines.Add('');
    Memo1.Lines.AddStrings(TStrings(ErrList));
    ShowModal;
  finally
    Free;
  end;
end;

procedure ReverseVertexLists;
//--------------------------------------------------------
// Reverses list of vertex points for each link in network
//--------------------------------------------------------
var
  i,j: Integer;
begin
  for i := PIPES to VALVES do
    for j := 0 to Network.Lists[i].Count-1 do
      if Link(i,j).Vlist <> nil then Link(i,j).ReverseVlist;
end;

function ReadInpFile(const Fname: String):Boolean;
//------------------------------------------------
// Reads network input data from a .INP text file
//------------------------------------------------
var
  F : Textfile;
begin
// Try to open the file
  MapFile := '';
  Result := False;
  AssignFile(F,Fname);
  {$I-}
  Reset(F);
  {$I+}
  if (IOResult = 0) then
  begin

  // Create stringlists
    Screen.Cursor := crHourGlass;
    MapBackdrop := DefMapBackdrop;
    MapDimensions := DefMapDimensions;
    DimensionsSet := False;
    ErrList := TStringList.Create;
    TokList := TStringList.Create;
    NodeList := TStringList.Create;
    LinkList := TStringList.Create;
    FileType := ftInput;
    try

    // Read the INP file
      NodeList.Sorted := True;
      LinkList.Sorted := True;
      Section := -1;
      Result := ReadFile(F,Uutils.GetFileSize(Fname));

    // If a map file was specified, then read it
      if (Result = True) and (MapFile <> '') then
        ReadMapFile(ExtractFilePath(Fname));

    finally
    // Free the stringlists
      NodeList.Free;
      LinkList.Free;
      TokList.Free;
      MainForm.PageSetupDialog.Header.Text := Network.Options.Title;
      MainForm.HideProgressBar;
      Screen.Cursor := crDefault;

    // Display errors if found & set map dimensions
      if Result = True then
      begin
        if ErrList.Count > 0 then DisplayInpErrForm(Fname);
        ReverseVertexLists;
        if not DimensionsSet then SetMapDimensions(MapDimensions);
        Uinput.UpdateQualParam;
      end;
      ErrList.Free;
    end;
  end;

// Close the input file
  CloseFile(F);
end;

procedure ReadScnFile(const Fname: String);
//-------------------------------------------
// Imports scenario data from a scenario file
//-------------------------------------------
var
  R : Boolean;
  F : Textfile;
begin
  R := False;
  AssignFile(F,Fname);
  {$I-}
  Reset(F);
  {$I+}
  if (IOResult = 0) then
  begin
    Screen.Cursor := crHourGlass;
    ErrList := TStringList.Create;
    FileType := ftImport;
    TokList := TStringList.Create;
    NodeList := TStringList.Create;
    LinkList := TStringList.Create;
    try
      MainForm.ShowProgressBar(MSG_IMPORTING_DATA);
      Section := -1;  //[END]
      R := ReadFile(F,Uutils.GetFileSize(Fname));
    finally
      MainForm.HideProgressBar;
      Screen.Cursor := crDefault;
      NodeList.Free;
      LinkList.Free;
      if (R) and (ErrList.Count > 0) then DisplayInpErrForm(Fname);
      ErrList.Free;
      TokList.Free;
    end;
  end;
  CloseFile(F);
  Uinput.UpdateEditor(EditorObject,EditorIndex);
  MainForm.RefreshMapForm;
  if not R then Uutils.MsgDlg(
   Fname + MSG_INVALID_FILE, mtError, [mbOK])
  else HasChanged := True;
end;

function ImportMapFile(const Fname: String): Boolean;
//-------------------------------------------------
// Imports map coordinate data from a map text file
//-------------------------------------------------
var
  F : Textfile;
begin
  Result := False;
  AssignFile(F,Fname);
  {$I-}
  Reset(F);
  {$I+}
  if (IOResult = 0) then
  begin
    Screen.Cursor := crHourGlass;
    MainForm.ShowProgressBar(MSG_READING_MAP_FILE);
    DimensionsSet := False;
    ErrList := TStringList.Create;
    FileType := ftImport;
    TokList := TStringList.Create;
    NodeList := TStringList.Create;
    try
      NodeList.AddStrings(Network.Lists[JUNCS]);
      NodeList.Sort;
      Section := -1;  //[COORDINATES]
      Result := ReadFile(F,Uutils.GetFileSize(Fname));
    finally
      NodeList.Free;
      MainForm.HideProgressBar;
      Screen.Cursor := crDefault;
      if (Result) then
      begin
        if ErrList.Count > 0 then DisplayInpErrForm(Fname);
        ReverseVertexLists;
        if not DimensionsSet then SetMapDimensions(MapDimensions);
        HasChanged := True;
      end;
      ErrList.Free;
      TokList.Free;
    end;
  end;
  CloseFile(F);
end;

end.
