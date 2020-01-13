unit Uinput;

{-------------------------------------------------------------------}
{                    Unit:    Uinput.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that provides interface routines to          }
{   EPANET2W's internal database.                                   }
{-------------------------------------------------------------------}

(********************************************************************
    Routine        Purpose
    --------       ----------------------------------------
    AddCurve       Adds new X-Y curve to database
    AddDemand      Adds demand record to junction's data
    AddLabel       Adds new label to map
    AddLink        Adds new link (pipe, pump or valve)
    AddNode        Adds new node (junction, reservoir,
                   or tank) to network
    AddPattern     Adds new time pattern to database

    CopyLabel      Copies map label to clipboard
    CopyLink       Copies link's data to clipboard
    CopyNode       Copies node's data to clipboard
    DeleteLabelAnchor  Deletes references to a specific node
                       for all map label anchor nodes
    DeleteLabelMeter   Deletes references to a specific node/link
                       as a meter in all map labels

    DupID          Checks for duplicate ID for curves & patterns
    DupLinkID      Checks for duplicate link ID
    DupNodeID      Checks for duplicate node ID

    EditCurve      Edits a curve
    EditDemands    Edits a node's demands
    EditLabel      Edits a map label
    EditLabelFont  Edits a map label's font
    EditLink       Edits a link
    EditNode       Edits a node
    EditOptions    Edits analysis options
    EditPattern    Edits a time pattern
    Editcontrols   Edits control rules
    EditSource     Edits a node's WQ source data

    FindLink       Finds link by ID in database
    FindNode       Finds node by ID in database

    GetLabelFont   Retrieves a map label's font info
    GetNextID      Gets next unused ID for an object
    GetPipeLength  Gets length of pipe as drawn on map
    GetQualParam   Gets WQ analysis option
    GroupEdit      Modifies property of a group of
                   nodes or links selected on map
    GroupDelete    Deletes objects in selected region of map
    PasteLabel     Pastes copied data into a map label
    PasteLink      Pastes copied data into a link
    PasteNode      Pastes copied data into a node

    UpdateAllUnits     Updates units when flow units changed
    UpdateEditor       Updates contents of the Property Editor
    UpdateID           Updates an object's ID label
    UpdateLabelAnchor  Updates a map label's anchor node
    UpdateLinkNode     Updates a link's end node
    UpdateLinkColor    Updates a link's color
    UpdateMapUnits     Updates map length units conversion factors
    UpdateNodeColor    Updates a node's color
    UpdatePipeLengths  Updates lengths of pipes connected to a node
    UpdateQualLegend   Updates WQ parameter legend
    UpdateQualParam    Updates WQ parameter
    UpdateQualUnits    Updates WQ units
    UpdateRoughnessUnits Updates pipe roughness units
    UpdateXY           Updates a node's position on map

    ValidateInput      Validates input to Property Editor
    ValidJunc          Validates junction input
    ValidLabel         Validates map label input
    ValidOption        Validates analysis option input
    ValidPipe          Validates pipe input
    ValidPump          Validates pump input
    ValidReserv        Validates reservoir input
    ValidTank          Validates tank input
    ValidValve         Validates valve input
********************************************************************)

interface

uses Controls, Classes, SysUtils, Dialogs, Windows, Math,
     Forms, Graphics, System.UITypes,
     Uglobals, Uutils, Fproped;

const
  FMT_NODE_EXISTS = 'Node %s already exists.';
  FMT_LINK_EXISTS = 'Link %s already exists.';
  MSG_ALREADY_HAVE = 'You already have a ';
  MSG_CONFIRM_DELETE =
  'Are you sure that you want to delete all objects in the selected region?';
  TXT_TIME = 'Hrs:Min';
  TXT_VALUE = 'Value';
  TXT_LABEL_EDITOR = 'Label Editor';
  TXT_DEMAND_EDITOR = 'Demands for Junction ';
  TXT_SOURCE_EDITOR = 'Source Editor for Node ';
  TXT_OPTIONS_EDITOR = ' Options';
  TXT_CONTROLS_EDITOR = 'Simple Controls Editor';
  TXT_RULES_EDITOR = 'Rule-Based Controls Editor';
  TXT_WERE_UPDATED = 's were updated.';
  TXT_NO_NODE = 'There is no node named ';
  TXT_NO_LINK = 'There is no link named ';
  TXT_NAMED = ' named ';
  TXT_NOT_A_NUMBER = ' is not a valid number.';
  TXT_NO_NODE_NAMED = 'There is no node named ';
  TXT_BAD_CONNECTION = 'Node cannot be connected to itself.';

  procedure AddCurve;
  procedure AddDemand(aJunc: TJunc);
  procedure AddLabel(const X: Extended; const Y: Extended; const S: String);
  procedure AddLink(const Ltype: Integer; Node1, Node2: TNode;
    PointList: array of TPoint; N: Integer);
  procedure AddNode(const Ntype: Integer; const X: Extended; const Y: Extended);
  procedure AddPattern;

  procedure CopyNode(const Ntype: Integer; const Index: Integer);
  procedure CopyLink(const Ltype: Integer; const Index: Integer);
  procedure CopyLabel(const Index: Integer);
  procedure DeleteLabelAnchor(aNode: TNode);
  procedure DeleteLabelMeter(Mtype: Integer; ID: String);

  function  DupID(const S: String): Boolean;
  function  DupNodeID(const S: String): Boolean;
  function  DupLinkID(const S: String): Boolean;

  procedure EditCurve(const Index: Integer);
  procedure EditDemands(const Index: Integer);
  procedure EditLabel(const Index: Integer);
  procedure EditLabelFont(const Index: Integer);
  procedure EditLink(const Ltype: Integer; const Index: Integer);
  procedure EditNode(const Ntype: Integer; const Index: Integer);
  procedure EditOptions(const Index: Integer);
  procedure EditPattern(const Index: Integer);
  procedure EditControls(const Index: Integer);
  procedure EditSource(const Ntype: Integer; const Index: Integer);

  function  FindLink(const S: String; var LinkType: Integer;
    var LinkIndex: Integer): Boolean;
  function  FindNode(const S: String; var NodeType: Integer;
    var NodeIndex: Integer): Boolean;

  procedure GetLabelFont(const Index: Integer; aFont: TFont);
  function  GetNextID(const ObjType : Integer): String;
  function  GetPipeLength(const Index: Integer): String;
  function  GetQualParam: TWaterQuality;
  procedure GroupEdit(const ObjType: Integer; const PropIndex: Integer;
    const ActionType: Integer; const S: String; const Filter: TFilter);
  procedure GroupDelete;

  procedure PasteNode(const Ntype: Integer; const Index: Integer);
  procedure PasteLink(const Ltype: Integer; const Index: Integer);
  procedure PasteLabel(const Index: Integer);

  procedure UpdateAllUnits;

{*** Updated 12/29/00 ***}
  procedure UpdateDemandList(const Index: Integer);

  procedure UpdateEditor(const ObjType: Integer; const Index: Integer);
  procedure UpdateID(const S: String; const ObjType: Integer;
    const Index: Integer);
  function  UpdateLabelAnchor(aMapLabel: TMapLabel; const S: String): Boolean;
  function  UpdateLabelMeter(aMapLabel: TMapLabel; const S: String): Boolean;
  procedure UpdateLinkColor(const Ltype: Integer; const Lindex: Integer;
    const Kindex: Integer);
  function  UpdateLinkNode(const Ltype: Integer; const Index: Integer;
    const I: LongInt; const S: String): Boolean;
  procedure UpdateMapUnits;
  procedure UpdateNodeColor(const Ntype: Integer; const Nindex: Integer;
    const Kindex: Integer);
  procedure UpdatePipeLengths(const ObjType: Integer; const Index: Integer);
  procedure UpdateQualLegend(var aLegend: TMapLegend);
  procedure UpdateQualParam;
  procedure UpdateQualUnits;
  procedure UpdateRoughnessUnits;
  function  UpdateXY(const I: LongInt; const S: String): Boolean;

  function  ValidateInput(I: Integer; var S: String; var E: String): Boolean;
  function  ValidJunc(I: Integer; var S: String): Boolean;
  function  ValidLabel(I: Integer; var S: String): Boolean;
  function  ValidOption(I: Integer; var S: String): Boolean;
  function  ValidPipe(I: Integer; var S: String): Boolean;
  function  ValidPump(I: Integer; var S: String): Boolean;
  function  ValidReserv(I: Integer; var S: String): Boolean;
  function  ValidTank(I: Integer; var S: String): Boolean;
  function  ValidValve(I: Integer; var S: String): Boolean;


implementation

uses
 Dcontrol, Dcurve, Ddemand, Dpattern, Dsource, Fmain, Fmap,
 Fbrowser, Umap, Uoutput;

var
  ErrMsg: String;

//================================================================
//           Procedures that add objects to the database
//================================================================

procedure AddCurve;
//-------------------------------------
// Adds a new curve object to database.
//-------------------------------------
var
  aCurve : TCurve;
begin
  aCurve := TCurve.Create;
  Network.Lists[CURVES].AddObject(GetNextID(CURVES), aCurve);
  BrowserForm.AddObject(CURVES,Network.Lists[CURVES].Count-1);
end;


procedure AddLabel(const X: Extended; const Y: Extended; const S: String);
//--------------------------------------
// Adds a new map label to the database.
//--------------------------------------
var
  n         : Integer;
  aMapLabel : TMapLabel;
begin
  aMapLabel := TMapLabel.Create;
  aMapLabel.X := X;
  aMapLabel.Y := Y;
  Network.Lists[LABELS].AddObject(S,aMapLabel);
  n := Network.Lists[LABELS].Count - 1;
  MapForm.DrawObject(LABELS,n);
  BrowserForm.AddObject(LABELS,n);
end;


procedure AddLink(const Ltype: Integer; Node1, Node2: TNode;
  PointList: array of TPoint; N: Integer);
//---------------------------------
// Adds a new link to the database.
//---------------------------------
var
  aLink : TLink;
  aVertex: PVertex;
  i     : Integer;
  id    : String;
begin
// Create the link object and assign to it default properties
  aLink := TLink.Create;
  case Ltype of
    PIPES:  Uutils.CopyStringArray(DefProp[PIPES].Data, aLink.Data);
    PUMPS:  Uutils.CopyStringArray(DefProp[PUMPS].Data, aLink.Data);
    VALVES: Uutils.CopyStringArray(DefProp[VALVES].Data,aLink.Data);
  end;

// Assign end nodes to the link
  if (Node1 <> nil) and (Node2 <> nil) then
  begin
    aLink.Node1 := Node1;
    aLink.Node2 := Node2;
  end;

// Create linked list of interior vertices
  for i := N downto 1 do
  begin
    New(aVertex);
    aVertex^.X := MapForm.Map.GetX(PointList[i].X);
    aVertex^.Y := MapForm.Map.GetY(PointList[i].Y);
    aVertex^.Next := aLink.Vlist;
    aLink.Vlist := aVertex;
  end;

// Initialize output result index and ID label
  aLink.Zindex := -1;
  id := GetNextID(Ltype);

// Add link to list of network objects
  Network.Lists[Ltype].AddObject(id,aLink);
  i := Network.Lists[Ltype].Count - 1;

// Compute length if AutoLength on
  if AutoLength and (Ltype = PIPES) then
    aLink.Data[PIPE_LEN_INDEX] := GetPipeLength(i);

// Assign the link a map color
  if (CurrentLinkVar = NOVIEW)
  or (LinkVariable[CurrentLinkVar].Source = vsOutput)
  then aLink.ColorIndex := -1
  else Uoutput.SetLinkColor(aLink,
    LinkVariable[CurrentLinkVar].SourceIndex[Ltype]);

// Draw the link on the map and update the Data Browser
// (must update map before Browser)
  MapForm.DrawObject(Ltype,i);
  BrowserForm.AddObject(Ltype,i);
end;

procedure AddDemand(aJunc: TJunc);
//-------------------------------------------------
// Adds new record to a junction's list of demands.
//-------------------------------------------------
var
  s: String;
begin
  s := aJunc.Data[JUNC_DEMAND_INDEX] + #13 + ' ' + #13 + ' ';
  aJunc.Demands.Add(s);
end;

procedure AddNode(const Ntype: Integer; const X: Extended; const Y: Extended);
//-------------------------------
// Adds new node to the database.
//-------------------------------
var
  i     : Integer;
  aJunc : TJunc;
  aNode : TNode;
  id    : String;
begin
// Get a default ID label for the node
  id := GetNextID(Ntype);

// If node is a junction then create it with default properties
  if Ntype = JUNCS then
  begin
    aJunc := TJunc.Create;
    aJunc.X := X;
    aJunc.Y := Y;
    aJunc.Zindex := -1;
    Uutils.CopyStringArray(DefProp[JUNCS].Data, aJunc.Data);
    Network.Lists[JUNCS].AddObject(id, aJunc);
  end

// Otherwise create a reservoir/tank with default properties
  else
  begin
    aNode := TNode.Create;
    aNode.X := X;
    aNode.Y := Y;
    aNode.Zindex := -1;
    if Ntype = RESERVS then
      Uutils.CopyStringArray(DefProp[RESERVS].Data,aNode.Data);
    if Ntype = TANKS then
      Uutils.CopyStringArray(DefProp[TANKS].Data,aNode.Data);
    Network.Lists[Ntype].AddObject(id, aNode);
  end;

// Add the node to the list of network objects
  i := Network.Lists[Ntype].Count - 1;
  Node(Ntype,i).ID := PChar(Network.Lists[Ntype].Strings[i]);

// Assign the node a map color
  aNode := Node(Ntype,i);
  if (CurrentNodeVar = NOVIEW)
  or (NodeVariable[CurrentNodeVar].Source = vsOutput)
  then aNode.ColorIndex := -1
  else Uoutput.SetNodeColor(aNode,
    NodeVariable[CurrentNodeVar].SourceIndex[Ntype]);

// Draw the node on map and update the Data Browser
// (must update map before Browser)
  MapForm.DrawObject(Ntype,i);
  BrowserForm.AddObject(Ntype,i);
end;


procedure AddPattern;
//-----------------------------------------
// Adds a new time pattern to the database.
//-----------------------------------------
var
  aPattern : TPattern;
begin
  aPattern := TPattern.Create;
  Network.Lists[PATTERNS].AddObject(GetNextID(PATTERNS), aPattern);
  BrowserForm.AddObject(PATTERNS,Network.Lists[PATTERNS].Count-1);
end;

//=================================================================
//           Procedures that edit objects in the database
//=================================================================

procedure UpdateEditor(const ObjType: Integer; const Index: Integer);
//-------------------------------------------------------
//Updates contents of Property Editor for specific object
//-------------------------------------------------------
begin
  if PropEditForm.Visible then
  begin
    PropEditForm.Editor.ColHeading2 := TXT_VALUE;
    EditorObject := ObjType;
    EditorIndex := Index;
    case ObjType of
      JUNCS..TANKS:  EditNode(ObjType,Index);
      PIPES..VALVES: EditLink(ObjType,Index);
      LABELS:        EditLabel(Index);
      OPTS:          EditOptions(Index);
    end;
  end;
end;


procedure EditLabel(const Index: Integer);
//-------------------------------------------------------
// Edits map label with position Index in list of labels.
//-------------------------------------------------------
var
  aMapLabel : TMapLabel;
begin
//Load current properties into PropList
  with Network do
  begin
    aMapLabel := MapLabel(Index);
    PropList.Clear;
    PropList.Add(GetID(LABELS,Index));
    PropList.Add(Format('%f',[aMapLabel.X]));
    PropList.Add(Format('%f',[aMapLabel.Y]));
    if aMapLabel.Anchor = nil then PropList.Add('')
    else PropList.Add(aMapLabel.Anchor.ID);
    PropList.Add(MeterTypes[aMapLabel.MeterType]);
    PropList.Add(aMapLabel.MeterID);
    PropList.Add(aMapLabel.FontName);
  end;

// Update the Property Editor
  PropEditForm.Caption := TXT_LABEL_EDITOR;
  PropEditForm.Editor.SetProps(LabelProps,Network.PropList);
end;


procedure GetLabelFont(const Index: Integer; aFont: TFont);
//---------------------------------------------------------
// Assigns font properties of a map Label to a font object.
//---------------------------------------------------------
begin
  with MapLabel(Index) do
  begin
    aFont.Name := FontName;
    aFont.Size := FontSize;
    aFont.Style := [];
    if FontBold then aFont.Style := aFont.Style + [fsBold];
    if FontItalic then aFont.Style := aFont.Style + [fsItalic];
  end;
end;


procedure EditLabelFont(const Index: Integer);
//--------------------------------------------------
// Edits font for map label Index in list of labels.
//--------------------------------------------------
begin
  with MainForm.FontDialog do
  begin
    GetLabelFont(Index,Font);
    if Execute then
    begin
      MapForm.EraseLabel(Index);
      with MapLabel(Index) do
      begin
        FontName := Font.Name;
        FontSize := Font.Size;
        if (fsBold in Font.Style) then FontBold := True
        else FontBold := False;
        if (fsItalic in Font.Style) then FontItalic := True
        else FontItalic := False;
      end;
      MainForm.SetChangeFlags;
      MapForm.DrawObject(LABELS,Index);
      UpdateEditor(LABELS,CurrentItem[LABELS]);
    end;
  end;
end;


procedure EditNode(const Ntype: Integer; const Index: Integer);
//------------------------------------------------------------------
// Edits node of type Ntype at position Index in list of nodes.
//------------------------------------------------------------------
var
  i    : Integer;
  v    : Integer;
  aNode: TNode;
begin
//Set caption for Property Editor window
  PropEditForm.Caption := ObjectLabel[Ntype] + ' ' + GetID(Ntype,Index);

//Get pointer to node's data
  aNode := Node(Ntype,Index);

//Place current input data values in PropList
  with Network do
  begin
    PropList.Clear;
    PropList.Add(aNode.ID);
    if aNode.X = MISSING then PropList.Add('')
    else PropList.Add(Format('%f',[aNode.X]));
    if aNode.Y = MISSING then PropList.Add('')
    else PropList.Add(Format('%f',[aNode.Y]));
    case Ntype of
      JUNCS:
        for i := 0 to JUNC_SRCQUAL_INDEX do PropList.Add(aNode.Data[i]);
      RESERVS:
        for i := 0 to RES_SRCQUAL_INDEX do PropList.Add(aNode.Data[i]);
      TANKS:
        begin
          for i := 0 to TANK_VCURVE_INDEX do PropList.Add(aNode.Data[i]);
          PropList.Add(aNode.Data[TANK_OVERFLOW_INDEX]);
          for i := TANK_MIXMODEL_INDEX to TANK_SRCQUAL_INDEX do
            PropList.Add(aNode.Data[i]);
        end;
    end;

  // Add output values to PropList
    for v := DEMAND to NODEQUAL do
      PropList.Add(GetNodeValStr(v,CurrentPeriod,Ntype,Index));
  end;

// Update Property Editor
  case Ntype of
    JUNCS:   PropEditForm.Editor.SetProps(JuncProps, Network.PropList);
    RESERVS: PropEditForm.Editor.SetProps(ReservProps, Network.PropList);
    TANKS:   PropEditForm.Editor.SetProps(TankProps, Network.PropList);
  end;
end;


procedure EditDemands(const Index: Integer);
//-------------------------------------------------------
// Invokes Demand Editor form to edit junction's demands
//-------------------------------------------------------
begin
  with TDemandsForm.Create(PropEditForm) do
  try
    Caption := TXT_DEMAND_EDITOR + GetID(JUNCS,Index);
    LoadDemands;
    if (ShowModal = mrOK) and (Modified = True) then
    begin
      UnloadDemands;
      UpdateEditor(JUNCS,CurrentItem[JUNCS]);
      MainForm.SetChangeFlags;
    end;
  finally
    Free;
  end;
end;


procedure EditSource(const Ntype: Integer; const Index: Integer);
//----------------------------------------------------------
// Invokes Source Editor form to edit node's source quality
//----------------------------------------------------------
begin
  with TSourceForm.Create(Application) do
  try
    Caption := TXT_SOURCE_EDITOR + GetID(Ntype,Index);
    if (ShowModal = mrOK) and (Modified = True) then
    begin
      UpdateEditor(Ntype,Index);
      MapForm.DrawObject(Ntype,Index);
      MainForm.SetChangeFlags;
    end;
  finally
    Free;
  end;
end;


procedure EditLink(const Ltype: Integer; const Index: Integer);
//------------------------------------------------------------------
// Edits link of type Ltype at position Index in list of links.
//------------------------------------------------------------------
var
  i,v  : Integer;
  last : Integer;
  s    : String;
  aLink: TLink;
begin

// Set Property Editor's caption
  s := GetID(Ltype,Index);
  PropEditForm.Caption := ObjectLabel[Ltype] + ' ' + s;

// Get pointer to link's data
  aLink := Link(Ltype,Index);

// Place current into data into the PropList
  with Network do
  begin
    PropList.Clear;
    PropList.Add(s);
    if aLink.Node1 = nil then PropList.Add('')
    else PropList.Add(aLink.Node1.ID);
    if aLink.Node2 = nil then PropList.Add('')
    else PropList.Add(aLink.Node2.ID);
    case Ltype of
      PIPES:  last := High(DefPipe);
      PUMPS:  last := High(DefPump);
      VALVES: last := High(DefValve);
      else    last := -1;
    end;
    for i := 0 to last do PropList.Add(aLink.Data[i]);

  // Add output values to PropList
    for v := FLOW to LINKSTAT do
    begin
      if (v = VELOCITY) and (Ltype = PUMPS) then continue;
      if (v in [FRICTION,REACTRATE]) and (Ltype in [PUMPS,VALVES]) then continue;
      PropList.Add(GetLinkValStr(v,CurrentPeriod,Ltype,Index));
    end;
  end;

// Update the Property Editor
  case Ltype of
    PIPES:   PropEditForm.Editor.SetProps(PipeProps, Network.PropList);
    PUMPS:   PropEditForm.Editor.SetProps(PumpProps, Network.PropList);
    VALVES:  PropEditForm.Editor.SetProps(ValveProps, Network.PropList);
  end;
end;


procedure EditOptions(const Index: Integer);
//------------------------------------------------
// Edits Analysis Options category given by Index
//------------------------------------------------
var
  k: Integer;
begin
  with Network do
  begin

  // Set Property Editor caption
    PropEditForm.Caption := GetID(OPTS,Index) + TXT_OPTIONS_EDITOR;

  // Place current options into PropList
    PropList.Clear;
    with Options do
    begin
      case Index of
      0:  begin
            for k := FLOW_UNITS_INDEX to STATUS_RPT_INDEX do //Hydraulics
              PropList.Add(Data[k]);
            for k := HEAD_ERROR_INDEX to PRESSURE_EXP_INDEX do
              PropList.Add(Data[k]);
            for k := CHECK_FREQ_INDEX to DAMP_LIMIT_INDEX do
              PropList.Add(Data[k]);
          end;

      1:  for k := QUAL_PARAM_INDEX to QUAL_TOL_INDEX do     //Quality
            PropList.Add(Data[k]);
      2:  for k := BULK_ORDER_INDEX to ROUGH_CORREL_INDEX do //Reactions
            PropList.Add(Data[k]);
      3:  begin
             PropEditForm.Editor.ColHeading2 := TXT_TIME;
             for k := DURATION_INDEX to TIME_STAT_INDEX do   //Times
              PropList.Add(Data[k]);
          end;
      4:  for k := EFFIC_INDEX to DMND_CHARGE_INDEX do       //Energy
            PropList.Add(Data[k]);
      end;

    // Update the Property Editor
      with PropEditForm.Editor do
      begin
        case Index of
        0: SetProps(HydraulicProps, PropList);
        1: SetProps(QualityProps, PropList);
        2: SetProps(ReactionProps, PropList);
        3: SetProps(TimeProps, PropList);
        4: SetProps(EnergyProps, PropList);
        end;
      end;
    end;
  end;
end;


procedure EditPattern(const Index: Integer);
//--------------------------------------------------
// Invokes Pattern Editor form to edit time pattern.
//--------------------------------------------------
begin
  with TPatternForm.Create(MainForm) do
  try
    LoadData(Index);
    if ShowModal = mrOK then
    begin
      UnloadData(Index);
      MainForm.SetChangeFlags;
    end;
  finally
    Free;
  end;
end;


procedure EditCurve(const Index: Integer);
//----------------------------------------------
// Invokes Curve Editor form to edit X-Y curve.
//----------------------------------------------
begin
  with TCurveForm.Create(MainForm) do
  try
    LoadData(Index);
    if (ShowModal = mrOK) then
    begin
      UnloadData(Index);
      MainForm.SetChangeFlags;
    end;
  finally
    Free;
  end;
end;


procedure EditControls(const Index: Integer);
//------------------------------------------------
// Invokes Controls Editor form to edit controls.
// Index = 0 for simple, 1 for rule-based controls.
//------------------------------------------------
var
  aList : TStringlist;
begin
  with TControlsForm.Create(MainForm) do
  try
    if Index = 0 then
    begin
      aList := Network.SimpleControls;
      Caption := TXT_CONTROLS_EDITOR;
      HelpContext := 164;
    end
    else
    begin
      aList := Network.RuleBasedControls;
      Caption := TXT_RULES_EDITOR;
      HelpContext := 165;
    end;
    Memo1.Lines.BeginUpdate;
    Memo1.Lines.Assign(aList);
    Memo1.Lines.EndUpdate;
    ActiveControl := Memo1;
    Memo1.SelStart := 0;
    if (ShowModal = mrOK) and Memo1.Modified then
    begin
      aList.Assign(Memo1.Lines);
      MainForm.SetChangeFlags;
    end;
  finally
    Free;
  end;
end;

//=============================================================================
//         Procedures that copy/paste objects to/from the clipboard
//=============================================================================

function GetDataIndexes(const ObjType: Integer; var First, Last: Integer):Boolean;
//-----------------------------------------------------
// Gets first and last indexes of data items that are
// copied/pasted from EPANET's internal clipboard.
//-----------------------------------------------------
begin
  Result := False;
  case ObjType of
    JUNCS:
    begin
      first := JUNC_ELEV_INDEX;
      last := JUNC_INITQUAL_INDEX;
    end;
    RESERVS:
    begin
      first := RES_HEAD_INDEX;
      last := RES_INITQUAL_INDEX;
    end;
    TANKS:
    begin
      first := TANK_ELEV_INDEX;
      last := TANK_INITQUAL_INDEX;
    end;
    PIPES:
    begin
      first := PIPE_DIAM_INDEX;
      last := PIPE_KWALL_INDEX;
    end;
    PUMPS:
    begin
      first := PUMP_HCURVE_INDEX;
      last := PUMP_PRICEPAT_INDEX;
    end;
    VALVES:
    begin
      first := VALVE_DIAM_INDEX;
      last := VALVE_STATUS_INDEX;
    end;
    else Exit;
  end;
  Result := True;
end;

procedure CopyNode(const Ntype: Integer; const Index: Integer);
//-----------------------------------------------
// Copies a node's data to the Network clipboard
//-----------------------------------------------
var
  i      : Integer;
  first  : Integer;
  last   : Integer;
  aNode  : TNode;
  Dlist  : TStringlist;
begin
  with Network.NetClipboard do
  begin
    Data.Clear;
    Demands.Clear;
    ObjType := -1;
    if not GetDataIndexes(Ntype,first,last) then Exit;
    aNode := Node(Ntype,Index);
    for i := first to last do Data.Add(aNode.Data[i]);
    if Ntype = JUNCS then
    begin
      Dlist := TJunc(Network.Lists[JUNCS].Objects[Index]).Demands;
      for i := 0 to Dlist.Count-1 do Demands.Add(Dlist[i]);
    end;
    ObjType := Ntype;
  end;
end;

procedure CopyLink(const Ltype: Integer; const Index: Integer);
//-----------------------------------------------
// Copies a link's data to the Network clipboard
//-----------------------------------------------
var
  i:      Integer;
  first:  Integer;
  last:   Integer;
  aLink:  TLink;
begin
  with Network.NetClipboard do
  begin
    Data.Clear;
    Demands.Clear;
    ObjType := -1;
    if not GetDataIndexes(Ltype,first,last) then Exit;
    aLink := Link(Ltype,Index);
    for i := first to last do Data.Add(aLink.Data[i]);
    ObjType := Ltype;
  end;
end;

procedure CopyLabel(const Index: Integer);
//---------------------------------------------------
// Copies a map label's data to the Network clipboard
//---------------------------------------------------
var
  aLabel: TMapLabel;
begin
  with Network.NetClipboard do
  begin
    Data.Clear;
    Demands.Clear;
    ObjType := LABELS;
    aLabel := MapLabel(Index);
    with Font do
    begin
      Name := aLabel.FontName;
      Size := aLabel.FontSize;
      Style := [];
      if aLabel.FontBold then Style := Style + [fsBold];
      if aLabel.FontItalic then Style := Style + [fsItalic];
    end;
  end;
end;

procedure PasteNode(const Ntype: Integer; const Index: Integer);
//--------------------------------------------------
// Pastes data from the Network clipboard to a node
//--------------------------------------------------
var
  i:     Integer;
  first: Integer;
  last:  Integer;
  aNode: TNode;
  Dlist: TStringlist;
begin
  if not GetDataIndexes(Ntype,first,last) then Exit;
  aNode := Node(Ntype,Index);
  with Network.NetClipboard do
  begin
    for i := first to last do aNode.Data[i] := Data[i-first];
    if Ntype = JUNCS then
    begin
      Dlist := TJunc(Network.Lists[JUNCS].Objects[Index]).Demands;
      Dlist.Clear;
      for i := 0 to Demands.Count-1 do Dlist.Add(Demands[i]);
    end;
  end;
  UpdateEditor(Ntype,Index);
  Uoutput.SetNodeColor(aNode,NodeVariable[CurrentNodeVar].SourceIndex[Ntype]);
  MapForm.DrawObject(Ntype,Index);
  MainForm.SetChangeFlags;
end;

procedure PasteLink(const Ltype: Integer; const Index: Integer);
//--------------------------------------------------
// Pastes data from the Network clipboard to a link
//--------------------------------------------------
var
  i:     Integer;
  first: Integer;
  last:  Integer;
  aLink: TLink;
begin
  if not GetDataIndexes(Ltype,first,last) then Exit;
  aLink := Link(Ltype,Index);
  with Network.NetClipboard do
  begin
    for i := first to last do aLink.Data[i] := Data[i-first];
  end;
  UpdateEditor(Ltype,Index);
  Uoutput.SetLinkColor(aLink,LinkVariable[CurrentLinkVar].SourceIndex[Ltype]);
  MapForm.DrawObject(Ltype,Index);
  MainForm.SetChangeFlags;
end;

procedure PasteLabel(const Index: Integer);
//------------------------------------------------------
// Pastes data from the Network clipboard to a map label
//------------------------------------------------------
var
  aLabel: TMapLabel;
begin
  with Network.NetClipboard do
  begin
    aLabel := MapLabel(Index);
    with Font do
    begin
      aLabel.FontName := Name;
      aLabel.FontSize := Size;
      if (fsBold in Style) then aLabel.FontBold := True
      else aLabel.FontBold := False;
      if (fsItalic in Style) then aLabel.FontItalic := True
      else aLabel.FontItalic := False;
    end;
  end;
  MapForm.EraseLabel(Index);
  MapForm.DrawObject(LABELS,Index);
  UpdateEditor(LABELS,Index);
  HasChanged := True;
end;

//===========================================================================
//          Procedures that edit a property for a group of objects
//===========================================================================

function Filtered(aObject: TObject; aFilter: TFilter): Boolean;
//-----------------------------------------------------------------
// Passes the object (node or link) aObject through filter aFilter.
//-----------------------------------------------------------------
var
  S: String;
begin
// Return true if no filter variable specified
   Result := True;
   if aFilter.Variable < 0 then Exit
   else
   begin

   // Retrieve string value of object's variable
     if aObject is TNode then with aObject as TNode do
       S := Trim(Data[aFilter.Variable])
     else if aObject is TLink then with aObject as TLink do
       S := Trim(Data[aFilter.Variable])
     else S := '';

   // Compare S to filter value
     Result := Uutils.CompareStrVals(S, aFilter.StrValue, aFilter.Relation);
   end;
end;


procedure UpdateJuncProp(aNode: TNode; const PropIndex: Integer;
  const ActionType: Integer; const S: String);
//-------------------------------------------------------------
// Updates property PropIndex of node object aNode to value S.
// ActionType determines type of updating
// (0 = replace, 1 = multiply by, 2 = add to).
//-------------------------------------------------------------
var
  v,v1,v2: Single;
begin
  case ActionType of
  0: aNode.Data[PropIndex] := S;
  1: if (Uutils.GetSingle(aNode.Data[PropIndex],v1))
     and (Uutils.GetSingle(S,v2)) then
     begin
       v := v1*v2;
       aNode.Data[PropIndex] := Format('%f',[v]);
     end;
  2: if (Uutils.GetSingle(aNode.Data[PropIndex],v1))
     and (Uutils.GetSingle(S,v2)) then
     begin
       v := v1+v2;
       aNode.Data[PropIndex] := Format('%f',[v]);
     end;
  end;
end;


procedure UpdatePipeProp(aLink: TLink; const PropIndex: Integer;
            const ActionType: Integer; const S: String);
//-------------------------------------------------------------
// Updates property PropIndex of link object aLink to value S.
// ActionType determines type of updating
// (0 = replace, 1 = multiply by, 2 = add to)
//-------------------------------------------------------------
var
  v,v1,v2: Single;
begin
  case ActionType of
  0: aLink.Data[PropIndex] := S;
  1: if (Uutils.GetSingle(aLink.Data[PropIndex],v1))
     and (Uutils.GetSingle(S,v2)) then
     begin
       v := v1*v2;
       aLink.Data[PropIndex] := Format('%f',[v]);
     end;
  2: if (Uutils.GetSingle(aLink.Data[PropIndex],v1))
     and (Uutils.GetSingle(S,v2)) then
     begin
       v := v1+v2;
       aLink.Data[PropIndex] := Format('%f',[v]);
     end;
  end;
end;


procedure GroupEdit(const ObjType: Integer; const PropIndex: Integer;
    const ActionType: Integer; const S: String; const Filter: TFilter);
//-------------------------------------------------------------
// Replaces data of junctions or pipes within selection region.
//-------------------------------------------------------------
var
  aRegion : HRgn;
  aNode   : TNode;
  aLink   : TLink;
  i       : Integer;
  n       : Integer;
  count   : Integer;
  p1, p2  : TPoint;
begin
//Create a GDI region from user's fenceline region
  count := 0;
  n := MapForm.NumFencePts - 1;
  aRegion := CreatePolygonRgn(MapForm.Fenceline,n,WINDING);
  try

  //Identify junctions in selected region
    if (ObjType = JUNCS) then
    begin
      n := Network.Lists[JUNCS].Count - 1;
      for i := 0 to n do
      begin
        p1 := MapForm.Map.GetNodePoint(JUNCS,i);

      //Update property and count
        if (PtInRegion(aRegion,p1.X,p1.Y)) then
        begin
          aNode := Node(JUNCS,i);
          if not Filtered(aNode,Filter) then continue;
          UpdateJuncProp(aNode,PropIndex,ActionType,S);
          Inc(count);
        end;
      end;

    //Update map color coding
      with NodeVariable[CurrentNodeVar] do
      begin
        if (Source = vsInput) and (SourceIndex[JUNCS] = PropIndex) then
        begin
          Uoutput.SetNodeColors;
          MapForm.RedrawMap;
        end;
      end;
    end;

  // Identify pipes in the selected region
    if (ObjType = PIPES) then
    begin
      n := Network.Lists[PIPES].Count - 1;
      for i := 0 to n do
      begin
        aLink := Link(PIPES,i);
        MapForm.Map.GetNodePixPos(aLink.Node1,p1);
        MapForm.Map.GetNodePixPos(aLink.Node2,p2);

      //Update property and count
        if (PtInRegion(aRegion,p1.X,p1.Y)) and
           (PtInRegion(aRegion,p2.X,p2.Y)) then
        begin
          if not Filtered(aLink,Filter) then continue;
          UpdatePipeProp(aLink,PropIndex,ActionType,S);
          Inc(Count);
        end;
      end;

    //Update map color coding
      with LinkVariable[CurrentLinkVar] do
      begin
        if (Source = vsInput) and (SourceIndex[PIPES] = PropIndex) then
        begin
          Uoutput.SetLinkColors;
          MapForm.RedrawMap;
        end;
      end;
    end;
  finally
    DeleteObject(aRegion);
  end;
  UpdateEditor(EditorObject, EditorIndex);
  if Count > 0 then MainForm.SetChangeFlags;
  Uutils.MsgDlg(IntToStr(count) + ' ' + ObjectLabel[ObjType] +
    TXT_WERE_UPDATED,mtInformation,[mbOK]);
end;


procedure GroupDelete;
//-------------------------------------------------------------
// Deletes all nodes and links within selection region of map.
//-------------------------------------------------------------
var
  aRegion : HRgn;
  aNode   : TNode;
  i,j,k,
  m,n     : Integer;
  p1      : TPoint;
begin
// Confirm deletion
 if Uutils.MsgDlg(MSG_CONFIRM_DELETE, mtConfirmation,[mbYes,mbNo]) = mrNo
   then Exit;

//Create a GDI region from user's fenceline region
  n := MapForm.NumFencePts - 1;
  aRegion := CreatePolygonRgn(MapForm.Fenceline,n,WINDING);
  try

  //Delete all nodes in selected region
    n := 0;
    for i := JUNCS to TANKS do
    begin
      for j := Network.Lists[i].Count-1 downto 0 do
      begin
        p1 := MapForm.Map.GetNodePoint(i,j);
        if (PtInRegion(aRegion,p1.X,p1.Y)) then
        begin
          Inc(n);
          aNode := Node(i,j);
          for k := PIPES to VALVES do
          begin
            for m := Network.Lists[k].Count-1 downto 0 do
              if (Link(k,m).Node1 = aNode)
              or (Link(k,m).Node2 = aNode) then
              begin
                DeleteLabelMeter(NETLINKS,GetID(i,j));
                DeleteNetworkObject(k,m);
              end;
          end;
          DeleteLabelAnchor(aNode);
          DeleteNetworkObject(i,j);
        end;
      end;
    end;
    if n > 0 then
    begin
      MainForm.SetChangeFlags;
      with BrowserForm do
      begin
        ItemListBox.Count := Network.Lists[CurrentList].Count;
        ItemListBox.Refresh;
        //ItemListBox.SetFocus;       {*** Updated 12/29/00 ***}
        UpdateBrowser(CurrentList, CurrentItem[CurrentList]);
      end;
      MapForm.NumFencePts := 0;       {*** Updated 12/29/00 ***}
      MapForm.RedrawMap;
    end;
  finally
    DeleteObject(aRegion);
  end;
end;


//================================================================
//                   Data Validation Functions
// These functions validate user input in the Property Editor
// and save valid input to the database for the item being edited.
//================================================================

function  ValidateInput(I: Integer; var S: String; var E: String): Boolean;
//----------------------------------------------------------------------
// This is the OnValidate event handler assigned to the Property Editor.
//----------------------------------------------------------------------
begin
  E := '';
  case EditorObject of
    JUNCS:   Result := ValidJunc(I, S);
    RESERVS: Result := ValidReserv(I, S);
    TANKS:   Result := ValidTank(I, S);
    PIPES:   Result := ValidPipe(I, S);
    PUMPS:   Result := ValidPump(I, S);
    VALVES:  Result := ValidValve(I, S);
    LABELS:  Result := ValidLabel(I, S);
    OPTS:    Result := ValidOption(I, S);
    else Result := False;
  end;
  if Result = False then E := ErrMsg;
end;


function ValidJunc(I: Integer; var S: String): Boolean;
//-------------------------------------------------
// Validation function for junction data.
//-------------------------------------------------
var
  k: Integer;
  v: Single;
begin
  Result := True;
  case I of
  0: if DupNodeID(S) then Result := False;  {Duplicate ID}
  1,
  2: Result := UpdateXY(I,S);               {X,Y coords.}
  5,                                        {Elev.}
  6: Result := Uutils.GetSingle(S,v);       {Demand}
  10: if (Length(Trim(S)) > 0) then         {InitQual}
       Result := Uutils.GetSingle(S,v);
  end;
  if Result then
  begin
    MainForm.SetChangeFlags;
    k := I - PROP_INDEX_OFFSET;
    if k >= 0 then
    begin
      Node(JUNCS,EditorIndex).Data[k] := S;
      UpdateNodeColor(JUNCS,EditorIndex,k);
    end;
    if (k in [JUNC_DEMAND_INDEX, JUNC_PATTERN_INDEX]) then
      UpdateDemandList(EditorIndex);
    if (k = JUNC_EMITTER_INDEX) then MapForm.DrawObject(JUNCS,EditorIndex); 
  end;
end;


function ValidReserv(I: Integer; var S: String): Boolean;
//-------------------------------------------------
// Validation function for reservoir data.
//-------------------------------------------------
var
  k: Integer;
  v: Single;
begin
  Result := True;
  case I of
  0: if DupNodeID(S) then Result := False; {Duplicate ID}
  1,
  2: Result := UpdateXY(I,S);              {X,Y coords.}
  5: Result := Uutils.GetSingle(S,v);      {Head}
  7: if (Length(Trim(S)) > 0) then         {InitQual}
       Result := Uutils.GetSingle(S,v);
  end;
  if Result then
  begin
    MainForm.SetChangeFlags;
    k := I - PROP_INDEX_OFFSET;
    if k >= 0 then
    begin
      Node(RESERVS,EditorIndex).Data[k] := S;
      UpdateNodeColor(RESERVS,EditorIndex,k);
    end;
  end;
end;


function ValidTank(I: Integer; var S: String): Boolean;
//-------------------------------------------------
// Validation function for tank data.
//-------------------------------------------------
var
  k: Integer;
  v: Single;
begin
  Result := True;
  case I of
  0: if DupNodeID(S) then Result := False; {Duplicate ID}
  1,
  2: Result := UpdateXY(I,S);              {X,Y coords.}
  5,                                       {Elev.}
  6,                                       {InitLevel}
  7,                                       {MinLevel}
  8,                                       {MaxLevel}
  9:                                       {Diam}
     Result := Uutils.GetSingle(S,v);
  10,                                      {MinVol}
  14,                                      {MixFrac}
  15,                                      {Kbulk}
  16:                                      {InitQual}
     if (Length(Trim(S)) > 0) then
       Result := Uutils.GetSingle(S,v);
  end;
  if Result then
  begin
    MainForm.SetChangeFlags;
    k := -1;                               //ID, X, Y
    if (I >= 3) and (I <= 11)
      then k := I - 3                      //Elev - VolCurve
    else if I = 12 then k := 16            //Overflow
    else if I >= 13 then k := I - 4;       //MixModel - SrcQual
    if k >= 0 then
    begin
      Node(TANKS,EditorIndex).Data[k] := S;
      UpdateNodeColor(TANKS,EditorIndex,k);
    end;
  end;
end;


function ValidPipe(I: Integer; var S: String): Boolean;
//-------------------------------------------------
// Validation function for pipe data.
//-------------------------------------------------
var
  k: Integer;
  v: Single;
begin
  Result := True;
  case I of
  0: if DupLinkID(S) then Result := False; {Duplicate ID}
  1,
  2: Result := UpdateLinkNode(PIPES,EditorIndex,I,S); {From/To nodes}
  5,                                       {Length}
  6,                                       {Diam}
  7,                                       {Roughness}
  8: Result := Uutils.GetSingle(S,v);      {MinLoss}
  10,                                      {Kbulk}
  11: if (Length(Trim(S)) > 0) then        {Kwall}
        Result := Uutils.GetSingle(S,v);
  end;
  if Result then
  begin
    MainForm.SetChangeFlags;
    k := I - PROP_INDEX_OFFSET;            {I = position in Property Editor}
    if k >= 0 then                         {k = position in Data array}
    begin
      Link(PIPES,EditorIndex).Data[k] := S;      {Update database}
      if k = PIPE_STATUS_INDEX then              {Redraw pipe in case check}
        MapForm.DrawObject(PIPES,EditorIndex)    {valve status changes.}
      else UpdateLinkColor(PIPES,EditorIndex,k); {Update pipe color on map}
    end;
  end;
end;


function ValidPump(I: Integer; var S: String): Boolean;
//-------------------------------------------------
// Validation function for pump data.
//-------------------------------------------------
var
  k: Integer;
  v: Single;
begin
  Result := True;
  case I of
  0: if DupLinkID(S) then Result := False; {Duplicate ID}
  1,
  2: Result := UpdateLinkNode(PUMPS,EditorIndex,I,S);
  6,                                       {Power}
  7,                                       {Speed}
  11: if (Length(Trim(S)) > 0) then        {Eprice}
        Result := Uutils.GetSingle(S,v);
  end;
  if Result then
  begin
    MainForm.SetChangeFlags;
    k := I - PROP_INDEX_OFFSET;
    if k >= 0 then
    begin
      Link(PUMPS,EditorIndex).Data[k] := S;
      UpdateLinkColor(PUMPS,EditorIndex,k);
    end;
  end;
end;


function ValidValve(I: Integer; var S: String): Boolean;
//-------------------------------------------------
// Validation function for valve data.
//-------------------------------------------------
var
  k: Integer;
  v: Single;
begin
  Result := True;
  case I of
  0: if DupLinkID(S) then Result := False; {Duplicate ID}
  1,
  2: Result := UpdateLinkNode(VALVES,EditorIndex,I,S);
  5,                                       {Diam}
  8: Result := Uutils.GetSingle(S,v);      {MinLoss}
  end;
  if Result then
  begin
    MainForm.SetChangeFlags;
    k := I - PROP_INDEX_OFFSET;
    if k >= 0 then
    begin
      Link(VALVES,EditorIndex).Data[k] := S;
      UpdateLinkColor(VALVES,EditorIndex,k);
    end;
  end;
end;


function ValidLabel(I: Integer; var S: String): Boolean;
//--------------------------------------------------
// Validation function for label data
//--------------------------------------------------
var
  v: Single;
  aMapLabel: TMapLabel;
  OldType: Integer;
begin
  Result := True;
  aMapLabel := MapLabel(EditorIndex);

{Replace old text with new}
  if I = LABEL_TEXT_INDEX then
  begin
    MapForm.ReplaceLabel(EditorIndex,S);
    UpdateID(S,EditorObject,EditorIndex);
  end

{Move label to new position}
  else if I in [X_INDEX..Y_INDEX] then
  begin
    if Uutils.GetSingle(S,v) then
    begin
      MapForm.HiliteOff;
      MapForm.EraseLabel(EditorIndex);
      case I of
        X_INDEX: aMapLabel.X := v;
        Y_INDEX: aMapLabel.Y := v;
      end;
      MapForm.DrawObject(LABELS,EditorIndex);
      MapForm.HiliteOn;
    end;
  end

{Update connection to an anchor node}
  else if I = ANCHOR_NODE_INDEX then Result := UpdateLabelAnchor(aMapLabel,S)

{Update label's meter type}
  else if I = METER_TYPE_INDEX then
  begin
    OldType := aMapLabel.MeterType;
    if CompareText(S,MeterTypes[NETNODES]) = 0 then
      aMapLabel.MeterType := NETNODES
    else if CompareText(S,MeterTypes[NETLINKS]) = 0 then
      aMapLabel.MeterType := NETLINKS
    else aMapLabel.MeterType := 0;
    if OldType <> aMapLabel.MeterType then
      MapForm.ReplaceLabel(EditorIndex,GetID(LABELS,EditorIndex));
  end

{Update node/link being metered}
  else if I = METER_ID_INDEX then
  begin
    Result := UpdateLabelMeter(aMapLabel,S);
    if Result = True then
      MapForm.ReplaceLabel(EditorIndex,GetID(LABELS,EditorIndex));
  end;
  if Result = True then HasChanged := True;
end;


function  ValidOption(I: Integer; var S: String): Boolean;
//-----------------------------------------------
// Input validation function for analysis options
//-----------------------------------------------
var
  k: Integer;
begin
  Result := True;
  k := I;
  // Convert from editor index I to property index k
  case EditorIndex of
    0: if I < 11 then k := I       //HYDRAULICS
       else if I < 17 then k := HEAD_ERROR_INDEX + (I - 11)
       else k := CHECK_FREQ_INDEX + (I - 17);

    1: k := QUAL_PARAM_INDEX  + I;  //QUALITY
    2: k := BULK_ORDER_INDEX + I;   //REACTIONS
    3: k := DURATION_INDEX + I;     //TIMES
    4: k := EFFIC_INDEX + I;        //ENERGY
  end;

// Revert to default option if user entry is blank
  if Length(S) = 0 then S := DefOptions[k];

// Check for valid time value
  if (k >= DURATION_INDEX) and (k <= RPT_START_INDEX) then
    if Uutils.StrHoursToFloat(S) < 0 then Result := False;

// Update option value & adjust associated input data
  if Result then
  begin
    Network.Options.Data[k] := S;
    MainForm.SetChangeFlags;
    case k of
      FLOW_UNITS_INDEX:   UpdateAllUnits;
      HLOSS_FORM_INDEX:   UpdateRoughnessUnits;
      QUAL_UNITS_INDEX:   UpdateQualUnits;
      QUAL_PARAM_INDEX:   UpdateQualParam;
      ROUGH_CORREL_INDEX: if CurrentLinkVar = WALLCOEFF then
                          begin
                            Uoutput.SetLinkColors;
                            MapForm.RedrawMap;
                          end;
    end;
  end;

end;

//==============================================================
//                 Database Updating Functions
//==============================================================

procedure UpdateID(const S: String; const ObjType: Integer;
  const Index: Integer);
//--------------------------------------------------
// Updates current object's ID to S in the database.
//--------------------------------------------------
begin
  with Network.Lists[ObjType] do
  begin

// Replace ID in database & in listbox display }
    Strings[Index] := S;
    BrowserForm.ItemListBox.Refresh;
    if ObjType in [JUNCS..TANKS] then
      Node(ObjType,Index).ID := PChar(Strings[Index]);
    if (ObjType = EditorObject) and (ObjType <> LABELS) then
      PropEditForm.Caption := ObjectLabel[EditorObject] + ' ' + S;
  end;

// Redraw ID on map if displayed
  if (ObjType in [JUNCS..TANKS])
  and (MapForm.Map.Options.DispNodeIDs)
  then MapForm.DrawObject(ObjType,Index);
  if (ObjType in [PIPES..VALVES])
  and (MapForm.Map.Options.DispLinkIDs)
  then MapForm.DrawObject(ObjType,Index);
end;


function UpdateXY(const I: LongInt; const S: String): Boolean;
//---------------------------------
// Updates node's position on map
//---------------------------------
var
  x,y,v : Extended;
  aNode : TNode;
begin
// Convert coordinate string to value
  v := MISSING;
  if (Length(S) > 0) and Uutils.GetExtended(S,v) then
  begin

  // Move node to new coordinate position
    aNode := Node(EditorObject,EditorIndex);
    x := aNode.X;
    y := aNode.Y;
    if I = X_INDEX then x := v else y := v;
    MapForm.HiliteOff;
    MapForm.MoveNode(EditorObject,EditorIndex,x,y);
    MapForm.HiliteOn;
    if AutoLength then UpdatePipeLengths(EditorObject,EditorIndex);
    Result := True;
  end
  else
  begin
    Errmsg := '''' + S + '''' + TXT_NOT_A_NUMBER;
    Result := False;
  end;
end;


function UpdateLinkNode(const Ltype: Integer; const Index: Integer;
  const I: LongInt; const S: String): Boolean;
//--------------------------------------------------------------
// Updates a link's node I (UP or DOWN) to node S & redraws link
//--------------------------------------------------------------
var
  ntype  : Integer;
  index2 : Integer;
  aLink  : TLink;
  aNode  : TNode;
  aRect  : TRect;

begin
// New node must be in database
  if not FindNode(S,ntype,index2) then
  begin
    Errmsg := TXT_NO_NODE_NAMED + S;
    Result := False;
    Exit;
  end;

// Both end nodes cannot be the same
  aLink := Link(Ltype, Index);
  aNode := Node(ntype,index2);
  if ((I = UP_INDEX) and (aNode = aLink.Node2))
  or  (aNode = aLink.Node1) then
  begin
    ErrMsg := TXT_BAD_CONNECTION;
    Result := False;
    Exit;
  end;

// Replace old end node with new one.
// Get bounding rectangle of current link connection.
  MapForm.HiliteOff;
  aRect := MapForm.Map.GetAdjacencyRect(Ltype,Index,False);
  if I = UP_INDEX then aLink.Node1 := aNode
  else                 aLink.Node2 := aNode;

// Union new bounding rectangle with old one & redraw map.
  if UnionRect(aRect, aRect,
       MapForm.Map.GetAdjacencyRect(Ltype,Index,False)) then
         MapForm.InvalidateMap(aRect);
  MapForm.HiliteOn;
  if AutoLength and (Ltype = PIPES) then
  begin
    aLink.Data[PIPE_LEN_INDEX] := GetPipeLength(Index);
    UpdateEditor(Ltype,Index);
  end;
  Result := True;
end;


function  UpdateLabelAnchor(aMapLabel: TMapLabel; const S: String): Boolean;
//--------------------------------------------------------------
// Makes node with ID S the anchor node for map label aMapLabel.
//--------------------------------------------------------------
var
  ntype, index: Integer;
begin
// Erase current label from map
  MapForm.HiliteOff;
  MapForm.EraseLabel(EditorIndex);

// No anchor node if S is blank
  Result := True;
  if Length(Trim(S)) = 0 then aMapLabel.Anchor := nil

// Otherwise make sure S is in database
  else
  begin
    if not FindNode(S, ntype, index) then
    begin
      ErrMsg := TXT_NO_NODE + S;
      Result := False;
    end
    else aMapLabel.Anchor := Node(ntype, index);
  end;

// Redraw label on map
  MapForm.DrawObject(LABELS,EditorIndex);
  MapForm.HiliteOn;
end;


function  UpdateLabelMeter(aMapLabel: TMapLabel; const S: String): Boolean;
//-------------------------------------------------------------------------
// Assigns S to a map label's meter ID if the ID is valid.
//-------------------------------------------------------------------------
var
  atype,index: Integer;
begin
  Result := True;
  if Length(Trim(S)) = 0 then aMapLabel.MeterID := ''
  else
  begin
    case aMapLabel.MeterType of
    NETNODES: if not FindNode(S, atype, index) then
              begin
                ErrMsg := TXT_NO_NODE + S;
                Result := False;
              end;
    NETLINKS: if not FindLink(S, atype, index) then
              begin
                ErrMsg := TXT_NO_LINK + S;
                Result := False;
              end;
    end;
    if Result = True then aMapLabel.MeterID := S;
  end;
end;


procedure DeleteLabelAnchor(aNode: TNode);
//-------------------------------------------------
// Deletes use of aNode as a map label anchor node.
//-------------------------------------------------
var
  i: Integer;
begin
  for i := 0 to Network.Lists[LABELS].Count - 1 do
  begin
    with MapLabel(i) do
      if Anchor = aNode then Anchor := nil;
  end;
end;

procedure DeleteLabelMeter(Mtype: Integer; ID: String);
//------------------------------------------------------
// Deletes use of Node/Link ID as a Label Meter
//------------------------------------------------------
var
  i: Integer;
begin
  for i := 0 to Network.Lists[LABELS].Count - 1 do
  begin
    with MapLabel(i) do
    begin
      if (MeterType = Mtype) and (CompareText(ID,MeterID) = 0) then
      begin
        MeterType := NONE;
        MeterID := '';
        MapForm.ReplaceLabel(i, GetID(LABELS,i));
      end;
    end;
  end;
end;


function FindNode(const S: String; var NodeType: Integer;
                  var NodeIndex: Integer): Boolean;
//-------------------------------------------------------
// Locates node whose ID is S in network database
//-------------------------------------------------------
var
  i: Integer;
begin
  Result := True;
  for i := JUNCS to TANKS do
  begin
    NodeType := i;
    NodeIndex := Network.Lists[NodeType].IndexOf(S);
    if NodeIndex >= 0 then Exit;
  end;
  Result := False;
end;


function FindLink(const S: String; var LinkType: Integer;
                  var LinkIndex: Integer): Boolean;
//-------------------------------------------------------
// Locates link whose ID is S in network database
//-------------------------------------------------------
var
  i: Integer;
begin
  Result := True;
  for i := PIPES to VALVES do
  begin
    LinkType := i;
    LinkIndex := Network.Lists[LinkType].IndexOf(S);
    if LinkIndex >= 0 then Exit;
  end;
  Result := False;
end;


procedure UpdateAllUnits;
//-----------------------------------------------------
// Updates units of expression when flow units changes.
//-----------------------------------------------------
var
  i: Integer;
begin
// Determine which system of units being used
  FlowUnits := Network.Options.Data[FLOW_UNITS_INDEX];
  i := Uutils.FindKeyWord(FlowUnits,USFlowUnits,4);
  if i >= 0 then UnitSystem := usUS
  else UnitSystem := usSI;

// Assign units to node & link input variables
  for i := 0 to NODEVIEWS do
    if NodeVariable[i].Source = vsInput then
      NodeUnits[i].Units := BaseNodeUnits[i,UnitSystem];
  for i := 0 to LINKVIEWS do
    if LinkVariable[i].Source = vsInput then
      LinkUnits[i].Units := BaseLinkUnits[i,UnitSystem];

// Use new flow units for flow-type variables
  NodeUnits[BASEDEMAND].Units := FlowUnits;

// Update roughness units, water quality units
// and map units conversion factors
  UpdateRoughnessUnits;
  UpdateQualUnits;
  UpdateMapUnits;
  MainForm.StatusBarPanel1.Caption := FlowUnits;
end;


procedure UpdateRoughnessUnits;
//------------------------------------
// Updates units for roughness coeffs.
//------------------------------------
begin
// Only roughness for D-W headloss equation has units
  if (CompareText(Network.Options.Data[HLOSS_FORM_INDEX], 'D-W') = 0) then
    LinkUnits[ROUGHNESS].Units := BaseLinkUnits[ROUGHNESS,UnitSystem]
  else
    LinkUnits[ROUGHNESS].Units := '';
end;


procedure UpdateQualUnits;
//------------------------------------------------------------
// Updates WQ units when a new WQ parameter is chosen
//------------------------------------------------------------
begin
  if QualParam = wqChem then
    QualUnits := Network.Options.Data[QUAL_UNITS_INDEX]
  else
    QualUnits := BaseQualUnits[Ord(QualParam)];
  NodeUnits[INITQUAL].Units := QualUnits;
  if not Runflag then
  begin
    NodeUnits[NODEQUAL].Units := QualUnits;
    LinkUnits[LINKQUAL].Units := QualUnits;
    if CurrentNodeVar = NODEQUAL then MapForm.DrawNodeLegend;
    if CurrentLinkVar = LINKQUAL then MapForm.DrawLinkLegend;
  end;
  if (CurrentNodeVar = INITQUAL) then MapForm.DrawNodeLegend;
end;


procedure UpdateMapUnits;
//------------------------------------------------------------
// Updates length units conversion factor
//------------------------------------------------------------
begin
  with MapDimensions do
  begin
    LengthUCF := 1.0;
    if (UnitSystem = usUS) then
    begin
      if (Units = muMeters) or (Units = muDegrees)
      then LengthUCF := FEETperMETER;
    end;
    if (UnitSystem = usSI) then
    begin
      if (Units = muFeet)
      then LengthUCF := METERSperFOOT;
    end;
    Digits := 2;
    if Units = muDegrees then
    begin
      Digits := MAXDEGDIGITS;
      YperDeg := XperDeg*Cos(DegToRad((LowerLeft.Y + UpperRight.Y)/2));
    end;
  end;
end;


{*** Updated 12/29/00 ***}
procedure UpdateDemandList(const Index: Integer);
//------------------------------------------------------------
// Updates primary demand for junction with multiple demands
//------------------------------------------------------------
var
  j      : Integer;
  s, c   : String;
begin
  with TJunc(Network.Lists[JUNCS].Objects[Index]) do
  begin

  // Check if multiple demands exist
    if Demands.Count > 1 then
    begin

    // Strip off category from primary demand record
    // (demand record = demand + #13 + pattern + #13 + category)
      c := ' ';
      s := Demands[0];
      j := Pos(#13,s);
      if j > 0 then
      begin
        s[j] := ' ';
        j := Pos(#13,s);
        if j > 0 then c := Copy(s,j+1,Length(s)-j)
      end;

    // Replace primary demand in demand list with junction's property data
      s := Data[JUNC_DEMAND_INDEX] + #13 + Data[JUNC_PATTERN_INDEX]
           + #13 + c;
      Demands[0] := s;
    end;
  end;
end;


function GetQualParam: TWaterQuality;
//-----------------------------------------------------------
// Returns type of WQ parameter being analyzed for.
// (The enumerated type TWaterQuality is defined in Globals.pas)
//-----------------------------------------------------------
var
  i: Integer;
  s: String;
begin
// Make default quality param = Chemical in case
// user supplied a specific chemical name.
  Result := wqChem;
  s := Network.Options.Data[QUAL_PARAM_INDEX];
  for i := 0 to High(QualParams) do
  begin
    if CompareText(QualParams[i],s) = 0 then
    begin
      Result := TWaterQuality(i);
      Exit;
    end;
  end;
end;


procedure UpdateQualParam;
//---------------------------------------------------
// Updates choice of WQ parameter being analyzed for.
//---------------------------------------------------
var
  oldqualparam: TWaterQuality;
begin
// Check that a new WQ parameter was selected
  oldqualparam := QualParam;
  QualParam := GetQualParam;
  if oldqualparam <> QualParam then
  begin

  // Update WQ units
    UpdateQualUnits;

  // Update WQ name
    BrowserForm.UpdateQualName;

  // Update map display
    UpdateQualLegend(NodeLegend[INITQUAL]);
    if not Runflag then
    begin
      UpdateQualLegend(NodeLegend[NODEQUAL]);
      UpdateQualLegend(LinkLegend[LINKQUAL]);
      if CurrentNodeVar = NODEQUAL then MapForm.DrawNodeLegend;
      if CurrentLinkVar = LINKQUAL then MapForm.DrawLinkLegend;
    end;
    if CurrentNodeVar = INITQUAL then with MapForm do
    begin
      Uoutput.SetNodeColors;
      RedrawMap;
      DrawNodeLegend;
    end;
  end;
end;


procedure UpdateQualLegend(var aLegend: TMapLegend);
//--------------------------------------------------------------
// Updates color intervals for Legend aLegend to
// the default intervals of the current choice of WQ parameter.
//--------------------------------------------------------------
var
  i,j: Integer;
begin
  i := Ord(QualParam);
  for j := 1 to MAXINTERVALS do
  begin
    aLegend.Intervals[j] := QualVariable[i].DefIntervals[j];
  end;
end;


procedure UpdateNodeColor(const Ntype: Integer; const Nindex: Integer;
  const Kindex: Integer);
//-------------------------------------------------------------
// Updates color of node of type Ntype and index Nindex if
// current view variable has index Kindex in node's data list.
//-------------------------------------------------------------
begin
  with NodeVariable[CurrentNodeVar] do
  begin
    if (Source = vsInput) and (SourceIndex[Ntype] = Kindex) then
    begin
      Uoutput.SetNodeColor(Node(Ntype,Nindex),Kindex);
      MapForm.DrawObject(Ntype,Nindex);
    end;
  end;
end;


procedure UpdateLinkColor(const Ltype: Integer; const Lindex: Integer;
  const Kindex: Integer);
//-------------------------------------------------------------
// Updates color of link of type Ltype and index Lindex if
// current view variable has index Kindex in node's data list.
//-------------------------------------------------------------
begin
  with LinkVariable[CurrentLinkVar] do
  begin
    if (Source = vsInput) and (SourceIndex[Ltype] = Kindex) then
    begin
      Uoutput.SetLinkColor(Link(Ltype,Lindex),Kindex);
      MapForm.DrawObject(Ltype,Lindex);
    end;
  end;
end;


procedure UpdatePipeLengths(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
// Updates lengths of pipes connected to node of type ObjType with index Index.
//-----------------------------------------------------------------------------
var
  J    : Integer;
  aNode: TNode;
  aLink: TLink;
begin
  if not ObjType in [JUNCS..TANKS] then Exit;
  aNode := Node(ObjType,Index);
  for J := Network.Lists[PIPES].Count-1 downto 0 do
  begin
    aLink := Link(PIPES,J);
    if (aLink.Node1 = aNode) or (aLink.Node2 = aNode) then
      aLink.Data[PIPE_LEN_INDEX] := GetPipeLength(J);
  end;
end;


function GetPipeLength(const Index: Integer): String;
//-----------------------------------------------------
// Returns string with length of pipe with given index.
//-----------------------------------------------------

  procedure ConvertMapUnits(var Dx, Dy: Extended);
  begin
    with MapDimensions do
      if Units = muDegrees then
      begin
        Dx := Dx*XperDeg;
        Dy := Dy*YperDeg;
      end;
  end;

var
  aLink  : TLink;
  aVertex: PVertex;
  Length : Extended;
  X1, X2 : Extended;
  Y1, Y2 : Extended;
  Dx, Dy : Extended;
begin
  aLink := Link(PIPES,Index);
  Length := 0;
  X1 := aLink.Node1.X;
  Y1 := aLink.Node1.Y;
  aVertex := aLink.Vlist;
  while aVertex <> nil do
  begin
    X2 := aVertex^.X;
    Y2 := aVertex^.Y;
    Dx := X2 - X1;
    Dy := Y2 - Y1;
    ConvertMapUnits(Dx,Dy);
    Length := Length + Sqrt(Sqr(Dx) + Sqr(Dy));
    X1 := X2;
    Y1 := Y2;
    aVertex := aVertex^.Next;
  end;
  X2 := aLink.Node2.X;
  Y2 := aLink.Node2.Y;
  Dx := X2 - X1;
  Dy := Y2 - Y1;
  ConvertMapUnits(Dx,Dy);
  Length := Length + Sqrt(Sqr(Dx) + Sqr(Dy));
  Length := MapDimensions.LengthUCF*Length;
  Result := Format('%.2f',[Length]);
end;


//=========================================================
//               Object ID management functions
//=========================================================

function IDExists(const ObjType: Integer; const ID: string): Boolean;
//------------------------------------------------------
// Determines if network object already has ID
//------------------------------------------------------
var
  i,j: Integer;
begin
  Result := False;
  case ObjType of
    JUNCS..TANKS:  if FindNode(ID,i,j) then Result := True;
    PIPES..VALVES: if FindLink(ID,i,j) then Result := True;
    else           if Network.Lists[ObjType].IndexOf(ID) >= 0 then
                     Result := True;
  end;
end;


function GetNextID(const ObjType : Integer): String;
//----------------------------------------------------------
// Returns next available default ID tag for network object.
//----------------------------------------------------------
var
  n : LongInt;
  s : String;
  v : LongInt;
  code: Integer;
begin
// Get ID prefix and next ID number.
  s := IDPrefix[ObjType];
  Val(s,v,code);
  n := NextID[ObjType];

// Keep incrementing ID number until a unique ID is created.
// If prefix is a number, then ID = Prefix + Next ID Number.
  if (code = 0) then
  begin
    while IDExists(ObjType, IntToStr(v+n)) do Inc(n, IDIncrement);
    Result := IntToStr(v+n);
  end
// Otherwise ID = concatonation of Prefix & ID number.
  else
  begin
    while IDExists(ObjType, s+IntToStr(n)) do Inc(n, IDIncrement);
    Result := s + IntToStr(n);
  end;

// Save last ID number used.
  NextID[ObjType] := n;
end;


function DupNodeID(const S: String): Boolean;
//--------------------------------------------
// Checks for duplicate node ID label.
//--------------------------------------------
var
  ntype : Integer;
  index : Integer;
  s1    : String;
begin
// Temporarily blank out ID of current item.
  Result := False;
  s1 := GetID(EditorObject,EditorIndex);
  Network.Lists[EditorObject].Strings[EditorIndex] := '';

// If there is another node with same ID then
// restore ID of current node and return True.
  if FindNode(S,ntype,index) then
  begin
    Errmsg := Format(FMT_NODE_EXISTS, [S]);
    Network.Lists[EditorObject].Strings[EditorIndex] := s1;
    Node(EditorObject,EditorIndex).ID :=
      PChar(Network.Lists[EditorObject].Strings[EditorIndex]);
    Result := True;
  end

// Otherwise replace the ID of the current node with S.
  else
  begin
  // Check if Trace Node ID needs updating
    if (Network.Options.Data[TRACE_NODE_INDEX] = s1)
    then Network.Options.Data[TRACE_NODE_INDEX] := S;
    UpdateID(S,EditorObject,EditorIndex);
  end;
end;


function DupLinkID(const S: String): Boolean;
//--------------------------------------------
// Checks for duplicate link ID label.
//--------------------------------------------
var
  ltype: Integer;
  index: Integer;
  s1   : String;
begin
// Temporarily blank out ID of current item.
  Result := False;
  s1 := GetID(EditorObject,EditorIndex);
  Network.Lists[EditorObject].Strings[EditorIndex] := '';

// If there is another link with same ID then
// restore ID of current link and return True.
  if FindLink(S,ltype,index) then
  begin
    Errmsg := Format(FMT_LINK_EXISTS, [S]);
    Network.Lists[EditorObject].Strings[EditorIndex] := s1;
    Result := True;
  end

// Otherwise replace the ID of the current node with S.
  else UpdateID(S,EditorObject,EditorIndex);
end;


function DupID(const S: String): Boolean;
//-------------------------------------------------
// Checks for duplicate ID's for patterns & curves
//-------------------------------------------------
var
  i    : Integer;
  s1   : String;
begin
// Temporarily blank out current ID
  Result := False;
  i := CurrentItem[CurrentList];
  s1 := GetID(CurrentList,i);
  Network.Lists[CurrentList].Strings[i] := '';

// Search for ID in object list
// If ID exists then restore current ID & exit.
  if Network.Lists[CurrentList].IndexOf(S) >= 0 then
  begin
    Uutils.MsgDlg(MSG_ALREADY_HAVE + ObjectLabel[CurrentList] +
      TXT_NAMED + S, mtError, [mbOK]);
    Network.Lists[CurrentList].Strings[i] := s1;
    Result := True;
    Exit;
  end;

// Otherwise replace current ID with S.
  UpdateID(S,CurrentList,i);
end;

end.
