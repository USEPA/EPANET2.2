unit Dgraph;

{-------------------------------------------------------------------}
{                    Unit:    Dgraph.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box that selects a type of graph to     }
{   plot. When user clicks OK button, contents of dialog are        }
{   placed into a TGraphSelection record (see Uglobals.pas          }
{   for description) and the MainForm's CreateGraph procedure       }
{   is called to create the graph.                                  }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, System.UITypes, Uglobals, Uutils;

const
  TXT_NO_VARIABLE = 'No parameter was selected';
  TXT_NO_ITEMS = 'No items to graph';
  TXT_TOO_MANY_ITEMS = 'Too many items to graph';
  TXT_NO_RESULTS = 'No results available to graph';
  TXT_NO_NODE_SELECTED = 'Must select a node object from Browser';
  TXT_NO_LINK_SELECTED = 'Must select a link object from Browser';
  TXT_OPEN_PROFILE_TITLE = 'Open Profile';
  TXT_SAVE_PROFILE_TITLE = 'Save Profile As';
  TXT_PROFILE_FILTER = 'Profile files (*.PRO)|*.PRO|All files|*.*';
  TXT_PROFILE_FOR = 'Profile for ';
  TXT_SAVE_PROFILE = 'Save Profile';
  TXT_PROFILE_ID = 'Profile identifier:';
  TXT_NODES_TO_GRAPH = 'Nodes to Graph';
  TXT_LINKS_TO_GRAPH = 'Links to Graph';

type
  TGraphSelectForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    BtnAdd: TButton;
    BtnDelete: TButton;
    BtnMoveUp: TButton;
    BtnMoveDown: TButton;
    BtnLoad: TButton;
    BtnSave: TButton;
    RGGraphType: TRadioGroup;
    RGObjectType: TRadioGroup;
    GBVariable: TGroupBox;
    CBVariable: TComboBox;
    GBTimePeriod: TGroupBox;
    CBTimePeriod: TComboBox;
    GBItems: TGroupBox;
    LBItems: TListBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnMoveUpClick(Sender: TObject);
    procedure BtnMoveDownClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure RGGraphTypeClick(Sender: TObject);
    procedure RGObjectTypeClick(Sender: TObject);
    procedure LBItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
    procedure AddToItemList;
    procedure CreateGraph;
    procedure EnableItemsList(const State: Boolean);
    procedure EnableTimePeriod(const State: Boolean);
  public
    { Public declarations }
  end;

//var
//  GraphSelectForm: TGraphSelectForm;

implementation

uses Fbrowser, Fcontour, Fgraph, Fmain, Fmap, Uinput;

{$R *.DFM}

procedure TGraphSelectForm.FormCreate(Sender: TObject);
//----------------------------------------------------
// Form's OnCreate event handler.
//----------------------------------------------------
begin
// Use global font currently in effect
  Uglobals.SetFont(self);

// Default graph type is Time Series
  RGGraphType.ItemIndex := TIMESERIESPLOT;

// Default object type is Nodes unless
// a Link object is selected in Browser
  RGObjectType.ItemIndex := 0;
  if CurrentList in [PIPES..VALVES] then
    RGObjectType.ItemIndex := 1;
  RGObjectTypeClick(Sender);

// If no simulation results available,
// then default graph is contour plot
// and time period control is disabled
  if not RunFlag then
  begin
    RGGraphType.ItemIndex := CONTOURPLOT;
    CBTimePeriod.Visible := False;
    GBTimePeriod.Visible := False;
  end
  else
  begin
    CBTimePeriod.Visible := True;
    GBTimePeriod.Visible := True;
  end;

// Assign time periods to time period control
  if CBTimePeriod.Visible then
  begin
    CBTimePeriod.Items.Assign(BrowserForm.TimeListBox.Items);
    CBTimePeriod.ItemIndex := BrowserForm.TimeListBox.ItemIndex;
  end;

// Enable/disable controls based on graph type
  RGGraphTypeClick(Sender);
end;

procedure TGraphSelectForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
//-------------------------------------------------
// Form's OnClose event handler.
//-------------------------------------------------
begin
  Action := caFree;
end;

procedure TGraphSelectForm.RGGraphTypeClick(Sender: TObject);
//--------------------------------------------------------
// OnClick event handler for Graph Type Radio Group
//--------------------------------------------------------
var
  Flag: Boolean;
begin
  case RGGraphType.ItemIndex of
    TIMESERIESPLOT:
    begin                           //Time Series Plot
      EnableTimePeriod(False);      //Disable time control
      RGObjectType.Enabled := True; //Enable object type &
      EnableItemsList(True);        //item list controls
    end;
    PROFILEPLOT:
    begin                           //Profile Plot
      EnableTimePeriod(True);       //Enable time control
      EnableItemsList(True);        //Enable item list control
      if RGObjectType.ItemIndex <> 0 then
      begin                         //Object type must be Nodes
        RGObjectType.ItemIndex := 0;
        RGObjectTypeClick(Sender);
      end;
      RGObjectType.Enabled := False;
    end;
    CONTOURPLOT:
    begin                           //Contour Plot
      EnableTimePeriod(True);       //Enable time control
      LBItems.Clear;                //Disable item list control
      EnableItemsList(False);
      if RGObjectType.ItemIndex <> 0 then
      begin                         //Object type must be Nodes
        RGObjectType.ItemIndex := 0;
        RGObjectTypeClick(Sender);
      end;
      RGObjectType.Enabled := False;
    end;
    FREQUENCYPLOT:
    begin                           //Frequency Plot
      EnableTimePeriod(True);       //Enable time control
      RGObjectType.Enabled := True; //Enable object control
      LBItems.Clear;                //Disable item list control
      EnableItemsList(False);
    end;
    SYSFLOWPLOT:
    begin                           //System Flow Plot
      EnableTimePeriod(False);      //Disable time control
      LBItems.Clear;                //Disable item list control
      EnableItemsList(False);
      if RGObjectType.ItemIndex <> 0 then
      begin                         //Object type must be Nodes
        RGObjectType.ItemIndex := 0;
        RGObjectTypeClick(Sender);
      end;
      RGObjectType.Enabled := False;
    end;
  end;
  Flag := not (RGGraphType.ItemIndex = SYSFLOWPLOT);
  GBVariable.Enabled := Flag;
  CBVariable.Visible := Flag;

// Load and Save buttons enabled only for Profile Plot
  Flag := (RGGraphType.ItemIndex = PROFILEPLOT);
  BtnLoad.Visible := Flag;
  BtnSave.Visible := Flag;
end;

procedure TGraphSelectForm.RGObjectTypeClick(Sender: TObject);
//---------------------------------------------------------
// OnClick event handler for Object Type radio group.
// Changes entries in Variable Combobox depending on
// whether Nodes or Links was selected.
//---------------------------------------------------------
var
  First,       // Index of first variable in combobox items
  Last,        // Index of last variable in combobox items
  i: Integer;

begin

// Check which type of object was selected
  Case RGObjectType.ItemIndex of

  // Nodes were selected
    0: begin

    // Load node variables into the Variable combobox
       CBVariable.Items.Clear;
       First := ELEVATION;
       Last := INITQUAL;
       if RunFlag then Last := NODEQUAL;
       for i := First to Last do
         CBVariable.Items.Add(BrowserForm.NodeViewBox.Items[i]);
       CBVariable.ItemIndex := CurrentNodeVar - 1;

    // Place current node selected in Browser into the ItemsList listbox
       GBItems.Caption := TXT_NODES_TO_GRAPH;
       LBItems.Clear;
       if LBItems.Enabled then
         if CurrentList in [JUNCS..TANKS] then AddToItemList;
     end;

  // Links were selected
    1: begin

    // Load link variables into the Variable combobox
       GBItems.Caption := TXT_LINKS_TO_GRAPH;
       CBVariable.Items.Clear;
       First := LINKLENGTH;
       Last := WALLCOEFF;
       if RunFlag then Last := LINKQUAL;
       for i := First to Last do
         CBVariable.Items.Add(BrowserForm.LinkViewBox.Items[i]);
       CBVariable.ItemIndex := CurrentLinkVar - 1;

    // Place current link selected in Browser into the ItemsList listbox
       LBItems.Clear;
       if LBItems.Enabled then
         if CurrentList in [PIPES..VALVES] then AddToItemList;
     end;
  end;
end;

procedure TGraphSelectForm.LBItemsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
//-----------------------------------------------------------
// OnDragOver event handler for the Items listbox. Allows
// listbox to accept items dragged onto it from the Browser.
//-----------------------------------------------------------
begin
  Accept := False;
  if (Source = BrowserForm.ItemListBox) and
     (LBItems.Enabled = True) then
    case RGObjectType.ItemIndex of
    0: if (CurrentList in [JUNCS..TANKS]) then Accept := True;
    1: if (CurrentList in [PIPES..VALVES]) then Accept := True;
    end;
end;

procedure TGraphSelectForm.LBItemsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
//--------------------------------------------------------
// OnDragDrop event handler for the Items listbox.
// Adds item dragged from Browser to Items list.
//--------------------------------------------------------
begin
  if Source = BrowserForm.ItemListBox then AddToItemList;
end;

procedure TGraphSelectForm.BtnOKClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for OK button.
//------------------------------------------------------
begin
//Check that a variable was chosen
  if (RGGraphType.ItemIndex <> SYSFLOWPLOT)
  and (CBVariable.ItemIndex < 0) then
  begin
    Uutils.MsgDlg(TXT_NO_VARIABLE, mtError, [mbOK]);
    Exit;
  end;

//Check that there are items to plot and then create the graph
  case RGGraphType.ItemIndex of
  TIMESERIESPLOT:
    if LBItems.Items.Count <= 0 then
    begin
      Uutils.MsgDlg(TXT_NO_ITEMS, mtError, [mbOK]);
      Exit;
    end
    else if LBItems.Items.Count > MAXSERIES then
    begin
      Uutils.MsgDlg(TXT_TOO_MANY_ITEMS, mtError, [mbOK]);
      Exit;
    end;
  PROFILEPLOT:
    if LBItems.Items.Count <= 0 then
    begin
      Uutils.MsgDlg(TXT_NO_ITEMS, mtError, [mbOK]);
      Exit;
    end;
  SYSFLOWPLOT:
    if not RunFlag then
    begin
      Uutils.MsgDlg(TXT_NO_RESULTS, mtError, [mbOK]);
      Exit;
    end;
  end;
  CreateGraph;
  Close;
end;

procedure TGraphSelectForm.BtnCancelClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for Cancel button.
//------------------------------------------------------
begin
  Close;
end;

procedure TGraphSelectForm.BtnAddClick(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for the Add button.
// Adds currently selected node or link in the Browser
// to the items to be plotted.
//-------------------------------------------------------
begin
// Check that proper type of object selected in Browser
  case RGObjectType.ItemIndex of
  0: if CurrentList in [JUNCS..TANKS] = False then
     begin
       Uutils.MsgDlg(TXT_NO_NODE_SELECTED, mtError, [mbOK]);
       Exit;
     end;
  1: if CurrentList in [PIPES..VALVES] = False then
     begin
       Uutils.MsgDlg(TXT_NO_LINK_SELECTED, mtError, [mbOK]);
       Exit;
     end;
  end;

// Add the object to the list of items to be plotted
  AddToItemList;
end;

procedure TGraphSelectForm.BtnDeleteClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for Delete button.
// Deletes selected item from Items listbox.
//-------------------------------------------------------
var
  i: Integer;
begin
  with LBItems do
  begin
    i := ItemIndex;
    if ItemIndex >= 0 then
      Items.Delete(ItemIndex);
    if Items.Count > 0 then
    begin
      if i < Items.Count then
        ItemIndex := i
      else
        ItemIndex := Items.Count-1;
    end;
  end;
end;

procedure TGraphSelectForm.BtnMoveUpClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for Move Up button.
// Moves selected item in Items listbox up one level.
//-------------------------------------------------------
begin
  with LBItems do
  begin
    if ItemIndex > 0 then Items.Exchange(ItemIndex,ItemIndex-1);
  end;
end;

procedure TGraphSelectForm.BtnMoveDownClick(Sender: TObject);
//---------------------------------------------------------
// OnClick handler for Move Down button.
// Moves selected item in Items listbox down one level.
//---------------------------------------------------------
begin
  with LBItems do
  begin
    if ItemIndex < Items.Count-1 then Items.Exchange(ItemIndex,ItemIndex+1);
  end;
end;

procedure TGraphSelectForm.BtnLoadClick(Sender: TObject);
//------------------------------------------------------
// OnClick procedure for Load button.
// Loads list of nodes from file into Items listbox.
//------------------------------------------------------
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := TXT_OPEN_PROFILE_TITLE;
    Filter := TXT_PROFILE_FILTER;
    Filename := '*.pro';
    Options := Options + [ofHideReadOnly];
    if Execute then with LBItems do
    begin
      Clear;
      Items.BeginUpdate;
      Items.LoadFromFile(Filename);
      if Items.Count >= 2 then
      begin
        Items.Delete(0); // Delete the ID label from the Save (see below)
        ItemIndex := 0;
      end;
      Items.EndUpdate;
    end;
  end;
end;

procedure TGraphSelectForm.BtnSaveClick(Sender: TObject);
//------------------------------------------------------
// OnClick procedure for Save button.
// Saves contents of Items listbox to file.
//------------------------------------------------------
var
  ProfileID: String;

begin
// Use an InputQuery form to get an ID label for the items in the listbox
  ProfileID := TXT_PROFILE_FOR +
    ChangeFileExt(ExtractFileName(InputFileName),'');
  if InputQuery(TXT_SAVE_PROFILE,TXT_PROFILE_ID,ProfileID) then

// Use the MainForm's SaveDialog form to get a name for the file
  with MainForm.SaveDialog do
  begin
    Title := TXT_SAVE_PROFILE_TITLE;
    Filter := TXT_PROFILE_FILTER;
    Filename := '*.pro';
    if Execute then with LBItems.Items do
    begin

    // Prepend the ID label to the items in the listbox,
    // save them to the file, & then delete the ID label.
      BeginUpdate;
      Insert(0,ProfileID);
      SaveToFile(Filename);
      Delete(0);
      EndUpdate;
    end;
  end;
end;

procedure TGraphSelectForm.AddToItemList;
//--------------------------------------------------------
// Adds a new item (node or link ID) to the Items listbox.
//--------------------------------------------------------
begin
  if CurrentItem[CurrentList] >= 0 then with LBItems do
  begin
    Items.Add(GetID(CurrentList,CurrentItem[CurrentList]));
    ItemIndex := Items.Count-1;
  end;
end;

procedure TGraphSelectForm.EnableTimePeriod(const State: Boolean);
//----------------------------------------------------------------
// Enables or disables the TimePeriod Combobox.
//----------------------------------------------------------------
begin
  if RunFlag then
  begin
    GBTimePeriod.Enabled := State;
    CBTimePeriod.Visible := State;
  end;
end;

procedure TGraphSelectForm.EnableItemsList(const State: Boolean);
//--------------------------------------------------------------------
// Enables/disables the Items listbox and its associated buttons.
//--------------------------------------------------------------------
begin
  LBItems.Visible := State;
  GBItems.Enabled := State;
  BtnAdd.Enabled := State;
  BtnDelete.Enabled := State;
  BtnMoveUp.Enabled := State;
  BtnMoveDown.Enabled := State;
  BtnLoad.Enabled := State;
  BtnSave.Enabled := State;
end;

procedure TGraphSelectForm.CreateGraph;
//-------------------------------------------
// Creates a graph based on selected choices.
//-------------------------------------------
var
  GraphSelection: TGraphSelection;
begin
  with GraphSelection do
  begin
    Items := LBItems.Items;
    case RGGraphType.ItemIndex of
      TIMESERIESPLOT:
      begin
        GraphType := TIMESERIESPLOT;
        if RGObjectType.ItemIndex = 0 then
          ObjectType := NODESERIES
        else
          ObjectType := LINKSERIES;
        VarType := CBVariable.ItemIndex + 1;
      end;
      PROFILEPLOT:
      begin
        GraphType := PROFILEPLOT;
        VarType := CBVariable.ItemIndex + 1;
        ObjectType := NETNODES;
        Period := CBTimePeriod.ItemIndex;
      end;
      CONTOURPLOT:
      begin
        GraphType := CONTOURPLOT;
        VarType := CBVariable.ItemIndex + 1;
        ObjectType := NETNODES;
        Period := CBTimePeriod.ItemIndex;
      end;
      FREQUENCYPLOT:
      begin
        GraphType := FREQUENCYPLOT;
        VarType := CBVariable.ItemIndex + 1;
        if RGObjectType.ItemIndex = 0 then
          ObjectType := JUNCS
        else
          ObjectType := PIPES;
        Period := CBTimePeriod.ItemIndex;
      end;
      SYSFLOWPLOT:
      begin
        GraphType := SYSFLOWPLOT;
        VarType := 0;
        ObjectType := 0;
        Period := 0;
      end;
    end;
  end;
  MainForm.CreateGraph(GraphSelection);
end;

procedure TGraphSelectForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 287);
end;

end.
