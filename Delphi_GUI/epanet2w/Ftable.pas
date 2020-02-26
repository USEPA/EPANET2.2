unit Ftable;

{-------------------------------------------------------------------}
{                    Unit:    Ftable.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that displays a tabular listing of selected      }
{   network design and analysis variables in a DrawGrid control.    }
{-------------------------------------------------------------------}

(*******************************************************************
    Four types of tables are possible:
      NETNODES - results for nodes at current time
      NETLINKS - results for links at current time
      NODESERIES - time series for current node
      LINKSERIES - time series for current link.
    The table is created with the CreateTable procedure, which is
    passed a TTableOptions record that determines what kind of
    table to create (see Uglobals.pas for a description). The
    table is updated with the RefreshTable procedure whenever new
    results are available. Entries in the table are drawn on an
    as-needed basis using the DrawGrid's OnDrawCell method.
********************************************************************)

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, ClipBrd, ExtCtrls, StdCtrls, System.UITypes,
  System.Types, Printers, XPrinter, Uglobals, Uutils;

const
  TXT_NETWORK_NODES = 'Network Table - Nodes';
  TXT_NETWORK_LINKS = 'Network Table - Links';
  TXT_NODE_SERIES = 'Time Series Table - Node ';
  TXT_LINK_SERIES = 'Time Series Table - Link ';
  TXT_NODE_ID = #13' Node ID';
  TXT_LINK_ID = #13' Link ID';
  TXT_TIME = ' Time'#13' Hours';
  TXT_AT = ' at ';
  TXT_ITEMS_WITH = ' items with';
  NodeTag: array[JUNCS..TANKS]  of PChar = ('Junc ','Resvr ','Tank ');
  LinkTag: array[PIPES..VALVES] of PChar = ('Pipe ','Pump ','Valve ');

type
  TTableForm = class(TForm)
    ListBox1: TListBox;
    Splitter1: TSplitter;
    Grid1: TDrawGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure Grid1DrawCell(Sender: TObject; vCol, vRow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Grid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Grid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBox1Exit(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    NumCols      : Integer;       //Number of columns in table
    ColSort      : Integer;       //Index of column to sort on
    ColIndex     : array[0..MAXCOLS] of Integer;
                                  //Index of variable in each column
    ColLabel     : array[0..MAXCOLS] of String;
                                  //Label for each column
    TableOptions : TTableOptions; //Table options
    IndexList    : TList;         //Row indexes (after sorting)
    ObjectType   : Integer;       //Type of object for time series table
    ObjectIndex  : Integer;       //Index of object for time series table
    Count1       : Integer;       //# of JUNCS or PIPES
    Count2       : Integer;       //Count1 + # RESERVS or PUMPS
    Count3       : Integer;       //Count2 + # TANKS or VALVES
    procedure CopyToString(const Fname: String);
    procedure CreatePrintTable(const FirstCol: Integer; var Ncol: Integer);
    function  Filtered(const T,I,J: Integer): Boolean;
    function  GetColZeroEntry(const R: LongInt): String;
    procedure GetLinkObject(const R: LongInt; var I,J: Integer);
    procedure GetNodeObject(const R: LongInt; var I,J: Integer);
    function  GetRowColEntry(const R: LongInt; const C: LongInt): String;
    function  GetRowCount: Integer;
    procedure PrintRow(const R, FirstCol, Ncol: Integer);
    procedure SelectBrowserObject(const R: LongInt);
    procedure SetColumnLabels;
    procedure SetColumnWidths;
    procedure SetFont;
    procedure SortTable;
  public
    { Public declarations }
    procedure CopyTo;
    procedure CreateTable(T: TTableOptions);
    procedure Print(Destination: TDestination);
    procedure RefreshTable;
    procedure SetTableOptions;
  end;

//var
//  TableForm: TTableForm;

implementation

{$R *.DFM}

uses Dcopy, Fbrowser, Fmain, Fmap, Dtable, Uinput, Uoutput;

var
  SortArray: PSingleArray;        //Temporary array used for sorting
  OldWidth : Integer;             //Old width of column in grid


function CompareByIndex(a,b: Pointer): Integer;
//------------------------------------------------------
// Comparison function for sorting contents of SortArray
//------------------------------------------------------
begin
  if      SortArray^[Integer(a)] < SortArray^[Integer(b)] then Result := -1
  else if SortArray^[Integer(a)] > SortArray^[Integer(b)] then Result := 1
  else Result := 0;
end;

procedure TTableForm.FormCreate(Sender: TObject);
//----------------------------------------------
// OnCreate handler for TableForm.
//----------------------------------------------
begin
// Set form's font size
  SetFont;

// Create list to hold indexes of row items (for sorting purposes)
  IndexList := TList.Create;
  TableOptions.TableType := NONE;
end;


procedure TTableForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1)
  then HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 263);
end;


procedure TTableForm.FormClose(Sender: TObject; var Action: TCloseAction);
//----------------------------------------------------------------------
// OnClose handler for TableForm.
//----------------------------------------------------------------------
begin
  IndexList.Free;
  Action := caFree;
end;


procedure TTableForm.Grid1DrawCell(Sender: TObject; vCol, vRow: Longint;
  Rect: TRect; State: TGridDrawState);
//----------------------------------------------------------------------
// OnDrawCell handler for grid cell in table.
//----------------------------------------------------------------------
var
  x : Integer;
  s : String;
  Buff: array[0..255] of Char;
begin
  with Sender as TDrawGrid do
  begin

  // Draw column headings in row 0
    if (vRow = 0) then
    begin

    // Use Win API DrawText function to enable word-wraping
      StrPCopy(Buff, ColLabel[vCol]);
      if vCol = 0 then
        DrawText(Canvas.Handle, Buff, StrLen(Buff), Rect,
               DT_LEFT OR DT_VCENTER OR DT_WORDBREAK)
      else
        DrawText(Canvas.Handle, Buff, StrLen(Buff), Rect,
               DT_CENTER OR DT_VCENTER OR DT_WORDBREAK);
    end

  // Draw cell value for body of table
    else
    begin
      if vCol = 0 then
      begin
        SetTextAlign(Canvas.Handle, TA_LEFT);
        x := (Rect.Left + 2);
        s := GetColZeroEntry(vRow);
      end
      else
      begin
        SetTextAlign(Canvas.Handle, TA_RIGHT);
        x := Rect.Right - 2;
        s := GetRowColEntry(vRow,vCol);
      end;

      if (gdSelected in State) then
      begin
        Canvas.Brush.Color := clHighlight;  //clNavy;
        Canvas.Font.Color := clHighlightText;  //clWhite;
      end
      else
      begin
        if vCol = 0 then
          Canvas.Brush.Color := Color
        else
          Canvas.Brush.Color := clInfoBk;   //$0080FFFF;
      end;
      Canvas.FillRect(Rect);
      SetBkMode(Canvas.Handle,TRANSPARENT);
      Canvas.TextOut(x,Rect.Top+2,s);
    end;
  end;
end;


function TTableForm.GetColZeroEntry(const R: LongInt): String;
//-----------------------------------------------------------
// Constructs string value for row R heading of table.
//-----------------------------------------------------------
var
  i: Integer;
  hours, minutes, seconds: Longint;
begin
// Get index of item appearing in row R
  i := Integer(IndexList.Items[R-1]);

// For network table, get ID of object with index i,
  case TableOptions.TableType of
    NETNODES:
    begin
      if i < Count1 then
        Result := NodeTag[JUNCS] + GetID(JUNCS,i)
      else if i < Count2 then
          Result := NodeTag[RESERVS] + GetID(RESERVS, i - Count1)
      else if i < Count3 then
          Result := NodeTag[TANKS] + GetID(TANKS, i - Count2)
      else Result := '';
    end;
    NETLINKS:
    begin
      if i < Count1 then
        Result := LinkTag[PIPES] + GetID(PIPES,i)
      else if i < Count2 then
          Result := LinkTag[PUMPS] + GetID(PUMPS, i - Count1)
      else if i < Count3 then
          Result := LinkTag[VALVES] + GetID(VALVES, i - Count2)
      else Result := '';
    end;

// For timeseries table, get string value of timeperiod i.
    NODESERIES..LINKSERIES:
    begin
      seconds := Rstart+i*Rstep;
      hours := seconds div 3600;
      minutes := (seconds - (3600*hours)) div 60;
      Result := Format('%d:%.2d ',[hours,minutes]);
    end;
  end;
end;


function TTableForm.GetRowColEntry(const R: LongInt;
                                   const C: LongInt): String;
//-------------------------------------------------------------
// Constructs string value of table item in row R and column C.
//-------------------------------------------------------------
var
  i,j: Integer;
begin
  with TableOptions do
  begin
    case TableType of

    // For network nodes at time TimePeriod, GetNodeObject() returns
    // the node object type in i and item index in j.
      NETNODES:
        begin
          GetNodeObject(R,i,j);
          if (j >= 0) then
            Result := Uoutput.GetNodeValStr(ColIndex[C],TimePeriod,i,j)
          else
            Result := NA;
        end;

    // For network links at time TimePeriod, GetLinkObject() returns
    // the link object type in i and item index in j.
      NETLINKS:
        begin
          GetLinkObject(R,i,j);
          if (j >= 0) then
            Result := Uoutput.GetLinkValStr(ColIndex[C],TimePeriod,i,j)
          else
            Result := NA;
        end;

    // For time series, use object type & item index from TableOptions
      NODESERIES:
        if ObjectIndex < 0 then
          Result := NA
        else
        begin
          j := Integer(IndexList.Items[R-1]);
          Result :=
            Uoutput.GetNodeValStr(ColIndex[C],j,ObjectType,ObjectIndex);
        end;
      LINKSERIES:
        if ObjectIndex < 0 then
          Result := NA
        else
        begin
          j := Integer(IndexList.Items[R-1]);
          Result :=
            Uoutput.GetLinkValStr(ColIndex[C],j,ObjectType,ObjectIndex);
        end;
    end;
  end;
end;


procedure TTableForm.GetNodeObject(const R: LongInt; var I,J: Integer);
//---------------------------------------------------------------------
// Finds object type (I) and item index (J) which corresponds to
// node appearing in row R of the table.
//---------------------------------------------------------------------
var
  k: Integer;
begin
// Convert pointer holding sorted index from IndexList
  k := Integer(IndexList.Items[R-1]);

// If k < # Junctions, then object is a Junction
  if k < Count1 then
  begin
    i := JUNCS;
    j := k;
  end

// If k < # Junctions + # Reservoirs then object is a Reservoir
  else if k < Count2 then
  begin
    i := RESERVS;
    j := k - Count1;
  end

// If k < # Junctions + # Reservoirs + # Tanks then object is a Tank
  else if k < Count3 then
  begin
    i := TANKS;
    j := k - Count2;
  end
  else
  begin
    i := -1;
    j := -1;
  end;
end;


procedure TTableForm.GetLinkObject(const R: LongInt; var I,J: Integer);
//---------------------------------------------------------------------
// Finds object type (I) and item index (J) which corresponds to
// link appearing in row R of the table.
//---------------------------------------------------------------------
var
  k: Integer;
begin
// Convert pointer holding sorted index from IndexList
  k := Integer(IndexList.Items[R-1]);

// If k < # Pipes, then object is a Pipe
  if k < Count1 then
  begin
    i := PIPES;
    j := k;
  end

// If k < # Pipes + # Pumps, then object is a Pump
  else if k < Count2 then
  begin
    i := PUMPS;
    j := k - Count1;
  end

// If k < # Pipes + #Pumps + # Valves, then object is a Valve
  else if k < Count3 then
  begin
    i := VALVES;
    j := k - Count2;
  end
  else
  begin
    i := -1;
    j := -1;
  end;
end;


procedure TTableForm.CreateTable(T: TTableOptions);
//-------------------------------------------------------
// Public procedure called to create a table after user
// has entered choices on the Table Options dialog form.
//-------------------------------------------------------
var
  i: Integer;
begin
// Save copy of current set of table options
  TableOptions := T;

// Build up form's caption depending on table type
  with TableOptions do
  begin
    case TableType of
      NETNODES:
      begin
        Caption := TXT_NETWORK_NODES;
        if (RunFlag) and (Nperiods > 1) then Caption := Caption + TXT_AT +
                  BrowserForm.TimeListBox.Items[TimePeriod];
      end;
      NETLINKS:
      begin
        Caption := TXT_NETWORK_LINKS;
        if (RunFlag) and (Nperiods > 1) then Caption := Caption + TXT_AT +
                  BrowserForm.TimeListBox.Items[TimePeriod];
      end;
      NODESERIES: Caption := TXT_NODE_SERIES + ObjectID;
      LINKSERIES: Caption := TXT_LINK_SERIES + ObjectID;
    end;

// Determine which variables will be displayed in table's columns
// and which column will be sorted on.
    NumCols := 1;
    ColSort := 0;
    if (TableType = NETNODES) or (TableType = NODESERIES) then
    begin
      for i := ELEVATION to NODEQUAL do
      begin
        if (NodeFields[i] = TRUE) then
        begin
          ColIndex[NumCols] := i;
          if (SortField = i) then ColSort := NumCols;
          Inc(NumCols);
        end;
      end;
      if (TableType = NODESERIES) then ColSort := 0;
    end
    else
    begin
      for i := LINKLENGTH to LINKSTAT do
      begin
        if (LinkFields[i] = TRUE) then
        begin
          ColIndex[NumCols] := i;
          if (SortField = i) then ColSort := NumCols;
          Inc(NumCols);
        end;
      end;
      if (TableType = LINKSERIES) then ColSort := 0;
    end;
  end;

// Reconstruct the entire table.
  RefreshTable;
end;


procedure TTableForm.RefreshTable;
//-------------------------------------------------------
// Reconstructs the table after a new set of options has
// been selected or after the table receives focus again
// (since user may have modified network data).
//-------------------------------------------------------
var
  showfilters: Boolean;
begin
// Reset column labels & widths
  Screen.Cursor := crHourGlass;
  SetColumnLabels;
  SetColumnWidths;

// Sort table if called for
  Grid1.RowCount := GetRowCount;
  if (ColSort > 0) and (Grid1.RowCount > 2) then SortTable;

// Display filter conditions in listbox below grid
  if (TableOptions.Filters[1].Variable < 0) then showfilters := False
  else showfilters := True;
  Splitter1.Visible := showfilters;
  ListBox1.Visible := showfilters;
  if showfilters then
    ListBox1.Items.Text := IntToStr(Grid1.RowCount - 1) + TXT_ITEMS_WITH + #13
      + TableOptions.FilterString;

// Refresh table display in Grid control
  SetFont;
  Grid1.Refresh;
  Grid1.SetFocus;
  Screen.Cursor := crDefault;
end;

procedure TTableForm.SetFont;
begin
  Uglobals.SetFont(self);
  Grid1.DefaultRowheight := 24;
  Grid1.RowHeights[0] := Grid1.DefaultRowHeight + (-Font.Height);

end;

procedure TTableForm.SetColumnLabels;
//--------------------------------------
// Constructs labels for column headings
//--------------------------------------
var
  i : Integer;
  s : String;
begin
  case TableOptions.TableType of

    NETNODES:
    begin
    // Determine range of row indexes containing Junctions, Reservoirs & Tanks
      Count1 := Network.Lists[JUNCS].Count;
      Count2 := Count1 + Network.Lists[RESERVS].Count;
      Count3 := Count2 + Network.Lists[TANKS].Count;

    // Index of node variable appearing in column I is given by ColIndex[I]
      with Grid1 do
      begin
        ColCount := NumCols;
        ColLabel[0] := TXT_NODE_ID;
        for i := 1 to ColCount-1 do
        begin
          if ColIndex[i] = NODEQUAL then
            s := BrowserForm.NodeViewBox.Items[ColIndex[i]]
          else
            s := NodeVariable[ColIndex[i]].Name;
          ColLabel[i] := s + #13 + NodeUnits[ColIndex[i]].Units;
        end;
      end;
    end;

    NETLINKS:
    begin
    // Determine range of row indexes containing Pipes, Pumps & Valves
      Count1 := Network.Lists[PIPES].Count;
      Count2 := Count1 + Network.Lists[PUMPS].Count;
      Count3 := Count2 + Network.Lists[VALVES].Count;

    // Index of link variable appearing in column I is given by ColIndex[I]
      with Grid1 do
      begin
        ColCount := NumCols;
        ColLabel[0] := TXT_LINK_ID;
        for i := 1 to ColCount - 1 do
        begin
          if ColIndex[i] = LINKQUAL then
            s := BrowserForm.LinkViewBox.Items[ColIndex[i]]
          else
            s := LinkVariable[ColIndex[i]].Name;
          ColLabel[i] := s + #13 + LinkUnits[ColIndex[i]].Units;
        end;
      end;
    end;

    NODESERIES:
    begin
    // Find object type & item index of node
      if (not Uinput.FindNode(TableOptions.ObjectID,
        ObjectType,ObjectIndex)) then ObjectIndex := -1;

    // Index of node variable appearing in column I is given by ColIndex[I]
      with Grid1 do
      begin
        ColCount := NumCols;
        ColLabel[0] := TXT_TIME;
        for i := 1 to ColCount-1 do
        begin
          if ColIndex[i] = NODEQUAL then
            s := BrowserForm.NodeViewBox.Items[ColIndex[i]]
          else
            s := NodeVariable[ColIndex[i]].Name;
          ColLabel[i] := s + #13 + NodeUnits[ColIndex[i]].Units;
        end;
      end;
    end;

    LINKSERIES:
    begin
    // Find object type & item index of link
      if (not Uinput.FindLink(TableOptions.ObjectID,
        ObjectType,ObjectIndex)) then ObjectIndex := -1;

    // Index of link variable appearing in column I is given by ColIndex[I]
      with Grid1 do
      begin
        ColCount := NumCols;
        ColLabel[0] := TXT_TIME;
        for i := 1 to ColCount-1 do
        begin
          if ColIndex[i] = LINKQUAL then
            s := BrowserForm.LinkViewBox.Items[ColIndex[i]]
          else
            s := LinkVariable[ColIndex[i]].Name;
          ColLabel[i] := s + #13 + LinkUnits[ColIndex[i]].Units;
        end;
      end;
    end;
  end;
end;


procedure TTableForm.SetColumnWidths;
//---------------------------------------------------
// Determines width of each column based on its label
//---------------------------------------------------
var
  i : Integer;
  TM: TTextMetric;
begin
  with Grid1 do
  begin
    GetTextMetrics(Canvas.Handle,TM);
    if TableOptions.TableType in [NODESERIES, LINKSERIES] then
      ColWidths[0] := 80
    else ColWidths[0] := (MAXID+6)*TM.tmAveCharWidth;
    for i := 1 to ColCount - 1 do
    begin
      ColWidths[i] := 80;
    end;
  end;
end;


function TTableForm.GetRowCount: Integer;
//----------------------------------------------------------
// Fills IndexList with pointers to integers representing
// the items that should appear in the table. Returns number
// of items added to IndexList + 1 (for the Row 0 header).
//----------------------------------------------------------
var
  i, i1, i2, j, rcount: Integer;
begin
// Initialize row count and IndexList
  rcount := 0;
  IndexList.Clear;
  case TableOptions.TableType of

// For table of network nodes or links at a given TimePeriod...
    NETNODES, NETLINKS:
    begin

    // Determine range of object codes
      if (TableOptions.TableType = NETNODES) then
      begin
        i1 := JUNCS;
        i2 := TANKS;
      end
      else
      begin
        i1 := PIPES;
        i2 := VALVES;
      end;

    // For each object type and item of that type...
      for i := i1 to i2 do
      begin
        for j := 0 to Network.Lists[i].Count-1 do
        begin

        // If item meets filter criteria then add to row count
          if Filtered(TableOptions.TimePeriod,i,j) then
            IndexList.Add(Pointer(rcount));
          Inc(rcount);
        end;
      end;
    end;

    // For table of specific node or link at all time periods..
    NODESERIES, LINKSERIES:
    begin

    // For each time period...
      for i := 0 to Nperiods-1 do
      begin

        // If item meets filter criteria then add to row count
        if Filtered(i,ObjectType,ObjectIndex) then
          IndexList.Add(Pointer(rcount));
        Inc(rcount);
      end;
    end;
  end;
  Result := IndexList.Count + 1;
end;


function TTableForm.Filtered(const T,I,J: Integer): Boolean;
//----------------------------------------------------------------------
// Determines if Item J of Object type I meets filter criteria at time T
//----------------------------------------------------------------------
var
  n,v: Integer;
  s1,s2: String;
  r: TRelationType;
  r1: Boolean;
begin
  Result := False;
  n := 1;
  with TableOptions do
  begin

  // Evaluate each filter condition
    while (n <= MAXFILTERS) and (Filters[n].Variable >= 0) do
    begin

    // Get filter condition variable (v), relation (r) & value (s2)
    // (e.g. Pressure > 20)
      v := Filters[n].Variable;
      r := Filters[n].Relation;
      s2 := Filters[n].StrValue;

    // Get value of current network item (s1)
      if (TableType = NETNODES) or (TableType = NODESERIES) then
        s1 := GetNodeValStr(v,T,I,J)
      else
        s1 := GetLinkValStr(v,T,I,J);

    // If condition not satisfied then stop, otherwise check next condition
      r1 := Uutils.CompareStrVals(s1,s2,r);
      if not r1 then exit;
      Inc(n);
    end;
  end;

// Return True if all conditions satisfied
  Result := True;
end;


procedure TTableForm.SortTable;
//------------------------------------------------------------
// Sorts row indexes contained in IndexList on column ColSort.
// Only used for network nodes or link tables, not for time
// series tables.
//------------------------------------------------------------
var
  i,j,r,n,t,v: Integer;
  x: Single;
  Compare : function(a,b: Pointer): Integer;
begin
// Initialize parameters
  v := ColIndex[ColSort];       // Index of variable to sort on
  t := TableOptions.TimePeriod; // Time period of table
  Compare := CompareByIndex;    // Pointer to comparison function
  n := Count3;                  // Total number of items

// If sort variable is an output variable then
// make sure that output values exist
  if (TableOptions.TableType = NETNODES) then
  begin
    if (NodeVariable[v].Source = vsOutput) then
      if (not RunFlag) or (t < 0) then Exit;
  end;
  if (TableOptions.TableType = NETLINKS) then
  begin
    if (LinkVariable[v].Source = vsOutput) then
      if (not RunFlag) or (t < 0) then Exit;
  end;

// Create temporary SortArray to hold values to be sorted
  GetMem(SortArray, n*SizeOf(Single));
  try

    r := 0;

  // For network node table...
    if TableOptions.TableType = NETNODES then
    begin

    // Load node values into SortArray
      for i := JUNCS to TANKS do
      begin
        for j := 0 to Network.Lists[i].Count-1 do
        begin
          x := Uoutput.GetNodeValue(v,t,i,j);
          if (x = MISSING) then x := 1e10;
          SortArray^[r] := x;
          Inc(r);
        end;
      end;
    end

  // For network node table...
    else if TableOptions.TableType = NETLINKS then
    begin

    // Load link values into SortArray
      for i := PIPES to VALVES do
      begin
        for j := 0 to Network.Lists[i].Count-1 do
        begin
          x := Uoutput.GetLinkValue(v,t,i,j);
          if (x = MISSING) then x := 1e10;
          SortArray^[r] := x;
          Inc(r);
        end;
      end;
    end;

  // Execute an index sort on the values in SortArray
    if (r > 0) then IndexList.Sort(Compare);

// Free memory used for SortArray
  finally
    FreeMem(SortArray, n*SizeOf(Single));
  end;
end;


procedure TTableForm.Grid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-------------------------------------------
// OnMouseDown handler for DrawGrid control.
//-------------------------------------------
var
  aCol, aRow: LongInt;

begin
// For left button press save width of current column
  if (Button = mbLeft) then with Sender as TDrawGrid do
  begin
    MouseToCell(X,Y,aCol,aRow);
    OldWidth := ColWidths[aCol];
  end;

// For right button press invoke Table Options dialog form
  if (Button = mbRight) then SetTableOptions;
end;


procedure TTableForm.Grid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//------------------------------------------------------------------
// OnMouseUp handler for DrawGrid control.
// Used to select an entire column of cells or the entire grid.
//------------------------------------------------------------------
var
  aCol, aRow: LongInt;
  gRect : TGridRect;
begin
  if (Button = mbLeft) then with Sender as TDrawGrid do
  begin

  // If user just resized a column then exit
    MouseToCell(X,Y,aCol,aRow);
    if (OldWidth <> ColWidths[aCol]) then Exit;

// Mouse pressed in a column of row 0
    if (aRow = 0) then
    begin

  // Column is > 0, select all rows in the column
      if (aCol > 0) then
      begin
        gRect.Left := aCol;
        gRect.Right := aCol;
      end

  // Column is 0, select all columns & rows
      else
      begin
        gRect.Left := FixedCols;
        gRect.Right := ColCount - FixedCols;
      end;
      gRect.Top := FixedRows;
      gRect.Bottom := RowCount - FixedRows;
      Selection := gRect;
    end

  // Mouse pressed in a row of Col 0.
  // Select entire row & select object in Browser
    else if (aCol = 0) then
    begin
      gRect.Left := FixedCols;
      gRect.Top := aRow;
      gRect.Right := ColCount - FixedCols;
      gRect.Bottom := aRow;
      Selection := gRect;
      SelectBrowserObject(aRow);
    end;
  end;
end;


procedure TTableForm.SetTableOptions;
//--------------------------------------
// Invokes Table Options dialog form to
// obtain new set of display options.
//--------------------------------------
var
  ReviseTable: Boolean;
begin
  ReviseTable := False;
  with TTableOptionsForm.Create(self) do
  try
    LoadOptions(TableOptions);
    if ShowModal = mrOK then
    begin
      UnloadOptions(TableOptions);
      ReviseTable := True;
    end;
  finally
    Free;
  end;
  if (ReviseTable) then CreateTable(TableOptions);
end;


procedure TTableForm.SelectBrowserObject(const R: LongInt);
//----------------------------------------------------------------
// Updates Browser form when a row is selected in a network table.
//----------------------------------------------------------------
var
  i, j  : Integer;
  s     : String;
  Found : Boolean;

begin
// Get the ID label in column 0
// (Strip off type-of-object prefix)
  if (R <= 0) then exit;
  s := GetColZeroEntry(R);
  s := Copy(s, Pos(' ',s)+1, Length(s));

// Search for ID in the database
  if (TableOptions.TableType = NETNODES) then
    Found := Uinput.FindNode(s,i,j)
  else if (TableOptions.TableType = NETLINKS) then
    Found := Uinput.FindLink(s,i,j)
  else Exit;

// Update Browser if item found
  if Found then
    BrowserForm.UpdateBrowser(i,j);
end;


procedure TTableForm.FormActivate(Sender: TObject);
//------------------------------------------
// OnActivate handler for Table form.
// Refreshes contents of table to reflect
// any changes that might have occurred to
// the database.
//------------------------------------------
begin
  MainForm.TBOptions.Enabled := True;
  if TableOptions.TableType <> NONE then
  begin
    RefreshTable;
  end;
end;


procedure TTableForm.ListBox1Exit(Sender: TObject);
//------------------------------------------------------
// Un-highlights listbox selection when it looses focus.
//------------------------------------------------------
begin
  ListBox1.ItemIndex := -1;
end;


procedure GetHeading(const S: String; var H1, H2: String);
//----------------------------------------------------------
// Converts a column heading into two separate lines of text
//----------------------------------------------------------
var
  p: Integer;
begin
  H1 := '';
  H2 := '';
  p := Pos(#13, S);
  if (p = 0) then
  begin
    H1 := S;
  end
  else
  begin
    H1 := Copy(S,1,p-1);
    if p < Length(S) then
      H2 := Copy(S,p+1,Length(S)-p);
  end;
end;


procedure TTableForm.CopyTo;
//----------------------------------------------
// Copies table to file or to Windows Clipboard.
//----------------------------------------------
begin
// Launch CopyTo dialog to get file name to copy to
// (Blank name means copy to Clipboard).
  with TCopyToForm.Create(self) do
  try
    FormatGroup.ItemIndex := 2;
    FormatGroup.Enabled := False;
    if ShowModal = mrOK then
      CopyToString(DestFileName);
  finally
    Free;
  end;
end;


procedure TTableForm.CopyToString(const Fname: String);
//-------------------------------------------------------
// Places current selection from table into a stringlist
// which is then saved to file or to the Clipboard.
//-------------------------------------------------------
var
  Slist: TStringList;
  C1,C2,C,R  : LongInt;
  h1,h2: String;
  S1,S2: String;
begin
// Create a stringlist to hold each row of selected cells
  Slist := TStringList.Create;
  try
    with Grid1.Selection do
    begin

    // Add title to stringlist
      Slist.Add(Network.Options.Title);
      Slist.Add(Caption);

    // Add column headings to stringlist
      GetHeading(ColLabel[0], h1, h2);
      S1 := Format('%-24s',[h1]);
      S2 := Format('%-24s',[h2]);;
      C1 := Left;
      if C1 = 0 then C1 := 1;
      C2 := Right;
      if C2 >= Grid1.ColCount then C2 := Grid1.ColCount-1;
      for C := C1 to C2 do
      begin
        GetHeading(ColLabel[C], h1, h2);
        S1 := S1 + #9 + Format('%-16s',[h1]);
        S2 := S2 + #9 + Format('%-16s',[h2]);
      end;
      Slist.Add(S1);
      Slist.Add(S2);

    // Iterate through each row of selected block of cells
      for R := Top to Bottom do
      begin

      // Build up tab-delimited string of entries in selected columns
        S1 := Format('%-24s',[GetColZeroEntry(R)]);
        for C := C1 to C2 do
          S1 := S1 + #9 + Format('%-16s',[GetRowColEntry(R,C)]);

      // Add tab-delimited string to list
        Slist.Add(S1);
      end;
    end;

  // Save stringlist to file if file name supplied
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)

  // Otherwise place text of stringlist onto clipboard
    else Clipboard.SetTextBuf(PChar(Slist.Text));

// Free the stringlist.
  finally
    Slist.Free;
  end;
end;


procedure TTableForm.CreatePrintTable(const FirstCol: Integer; var Ncol: Integer);
//-------------------------------------------------------------------------
// Creates a printout table with Ncol columns starting from column FirstCol.
//-------------------------------------------------------------------------
const
  ColWidth = 1.0;
  ColWidth1 = 1.75;
var
  i, j      : Integer;
  LastCol   : Integer;
  W, L,
  Left,
  Width,
  Width1    : Single;
  h1,h2     : String;
begin
// Assign width to column 1
  if TableOptions.TableType in [NETNODES, NETLINKS] then
    Width1 := ColWidth1  // Large width for ID label
  else
    Width1 := ColWidth;  // Normal width for time period

// Determine how many remaining columns fit on a page
  with MainForm.ThePrinter do
  begin

  // Get left margin & printable page width
      with PageLayout do
      begin
        Left := LMargin;
        Width := GetPageWidth - LMargin - RMargin;
      end;

  // Start with one column (for ID or time period)
      Ncol := 1;
      W := Width1;
      LastCol := FirstCol;

  // Add columns until we run out of space or columns
      while LastCol < NumCols do
      begin
        if W + ColWidth > Width then break;
        W := W + ColWidth;
        Inc(Ncol);
        Inc(LastCol);
      end;

   // Create a table on thePrinter object and
   // define its column properties
      CreateTable(Ncol);
      L := Left + (Width - W)/2;
      GetHeading(ColLabel[0], h1, h2);
      SetColumnHeaderText(1,1,h1);
      SetColumnHeaderText(1,2,h2);
      SetColumnHeaderAlignment(1,jLeft);
      SetColumnDimensions(1,L,Width1);
      L := L + Width1;
      for i := 2 to Ncol do
      begin
        j := FirstCol + i - 2;
        GetHeading(ColLabel[j], h1, h2);
        SetColumnHeaderText(i,1,h1);
        SetColumnHeaderText(i,2,h2);
        SetColumnHeaderAlignment(i,jCenter);
        SetColumnDimensions(i,L,ColWidth);
        L := L + ColWidth;
      end;
      SetTableStyle([sBorder, sVerticalGrid, sHorizontalGrid]);
  end;
end;


procedure TTableForm.PrintRow(const R: LongInt; const FirstCol, Ncol: Integer);
//--------------------------------------------------------
// Prints entries in first column (ID or time period) and
// Ncol columns starting with FirstCol for row R to page.
//--------------------------------------------------------
var
  i,j: LongInt;
begin
  with MainForm.thePrinter do
  begin
    PrintColumnLeft(1,GetColZeroEntry(r));
    for i := 2 to Ncol do
    begin
      j := FirstCol + i - 2;
      PrintColumnRight(i,GetRowColEntry(r,j));
    end;
  end;
end;


procedure TTableForm.Print(Destination: TDestination);
//-------------------------------------------------------
// Prints table to Destination (printer or preview form).
//-------------------------------------------------------
var
  FirstCol,
  Ncol      : Integer;
  r         : LongInt;
begin
  with MainForm.thePrinter do
  begin

  // Initialize Printer
    Screen.Cursor := crHourglass;
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);
    PrintCenter(Caption);
    NewLines(2);

  // Continue until all columns are printed
    FirstCol := 1;
    while FirstCol < NumCols do
    begin

    // Create a table with enough columns that fit across page
      CreatePrintTable(FirstCol,Ncol);
      BeginTable;

    // Transfer contents of form's Grid table to printed table
      with Grid1 do
      begin
        for r := 1 to RowCount - 1 do
        begin
          PrintRow(r,FirstCol,Ncol);
          NextTableRow( (r >= RowCount-1) );
        end;
      end;
      EndTable;

    // Continue creating a new table for remaining columns
      FirstCol := FirstCol + Ncol - 1;
      if (FirstCol < NumCols) then
      begin
        NewPage;
        PrintCenter(Caption);
        NewLines(2);
      end;
    end;
    Screen.Cursor := crDefault;
    EndJob;
  end;
end;

end.
