unit Dtable;

{-------------------------------------------------------------------}
{                    Unit:    Dtable.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box used to select options for a Table. }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, checklst, ComCtrls, ExtCtrls, System.UITypes,
  Ftable, Uutils, Uglobals;

const
  MSG_NO_NODE = 'There is no Node ';
  MSG_NO_LINK = 'There is no Link ';
  MSG_FILTER_LIMIT = 'Maximum number of filters has been reached.';
  TXT_CAPTION1 = 'Table Selection';
  TXT_CAPTION2 = 'Table Options';
  TXT_NODES = 'Network Nodes';
  TXT_LINKS = 'Network Links';
  TXT_SORTED_BY = 'Sorted by ';
  TXT_AT = ' at';

type
  TTableOptionsForm = class(TForm)
    PageControl1: TPageControl;
    TableTypePage: TTabSheet;
    ColumnsPage: TTabSheet;
    FiltersPage: TTabSheet;
    NetNodesBtn: TRadioButton;
    NetLinksBtn: TRadioButton;
    NodeSeriesBtn: TRadioButton;
    LinkSeriesBtn: TRadioButton;
    TimePeriodBox: TComboBox;
    ObjectIDBox: TEdit;
    Label2: TLabel;
    ColumnsListBox: TCheckListBox;
    SortedCheckBox: TCheckBox;
    FilterVariableBox: TComboBox;
    FilterRelationBox: TComboBox;
    FilterValueBox: TEdit;
    FiltersListBox: TListBox;
    FilterAddBtn: TButton;
    FilterDelBtn: TButton;
    CancelBtn: TButton;
    OKBtn: TButton;
    HelpBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NetNodesBtnClick(Sender: TObject);
    procedure ColumnsListBoxClick(Sender: TObject);
    procedure ColumnsListBoxClickCheck(Sender: TObject);
    procedure SortedCheckBoxClick(Sender: TObject);
    procedure FilterAddBtnClick(Sender: TObject);
    procedure FilterDelBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    TempOptions: TTableOptions;
    TempFilters: array[1..MAXFILTERS] of TFilter;
    NewTable: Boolean;
    TimeFlag: Boolean;
    NextFilter: Integer;
    procedure InitExistingTable;
    procedure InitNewTable;
  public
    { Public declarations }
    procedure LoadOptions(T: TTableOptions);
    procedure UnloadOptions(var T: TTableOptions);
  end;

//var
//  TableOptionsForm: TTableOptionsForm;

implementation

{$R *.DFM}

uses Fbrowser, Fmap, Uinput;

procedure TTableOptionsForm.FormCreate(Sender: TObject);
//---------------------------------------------
// OnCreate handler for form.
//---------------------------------------------
var
  i: Integer;
  S: String;
begin

// Set form's font
  Uglobals.SetFont(self);

// Initialize radio buttons depending on whether
// output of an analysis is available or not
  if RunFlag and (Nperiods > 1) then TimeFlag := True
  else TimeFlag := False;
  S := TXT_NODES;
  if TimeFlag then S := S + TXT_AT;;
  NetNodesBtn.Caption := S;
  S := TXT_LINKS;
  if TimeFlag then S := S + TXT_AT;
  NetLinksBtn.Caption := S;
  TimePeriodBox.Visible := TimeFlag;

// Initialize filter relations
  with FilterRelationBox do
  begin
    for i := Low(FilterRelation) to High(FilterRelation) do
      Items.Add(FilterRelation[i]);
  end;
  FilterRelationBox.ItemIndex := 0;
end;


procedure TTableOptionsForm.LoadOptions(T: TTableOptions);
//----------------------------------------------------
// Loads table options T into the form. This procedure
// is invoked by the unit which creates this form.
//----------------------------------------------------
var
  i: Integer;

begin
// Set temporary options equal to T
  TempOptions := T;

// Determine if working with a new or an existing table
  if (T.TableType = NONE) then
    NewTable := True
  else
    NewTable := False;

// Initialize the form's appearance for a new table
  if (NewTable) then
  begin
    Caption := TXT_CAPTION1;
    T.Filters[1].Variable := -1;
    NextFilter := 1;
    InitNewTable;
  end

// Initialize form's appearance for existing table
  else
  begin
    Caption := TXT_CAPTION2;
    InitExistingTable;
  end;

// Settings for the "Filter" page of the dialog
  FilterVariableBox.Items.Assign(ColumnsListBox.Items);
  FilterVariableBox.ItemIndex := 0;
  if not NewTable then with TempOptions do
  begin
    for i := 1 to MAXFILTERS do
    begin
      NextFilter := i;
      if (Filters[i].Variable < 0) then break;
      TempFilters[i] := Filters[i];
    end;
  end;
end;


procedure TTableOptionsForm.InitNewTable;
//-------------------------------------------------------
// Sets up dialog form when called before a table exists.
//-------------------------------------------------------
begin

// Use BrowserForm's TimeListBox to assign time periods to TimePeriodBox
  if TimeFlag then
  begin
    TimePeriodBox.Items.Assign(BrowserForm.TimeListBox.Items);
    TimePeriodBox.ItemIndex := BrowserForm.TimeListBox.ItemIndex;
  end;

// Activate time series table RadioButtons only if
// output results for an analysis are available
  if TimeFlag then
  begin
    NodeSeriesBtn.Enabled := True;
    LinkSeriesBtn.Enabled := True;

  // Set time series node/link ID to currently selected object
    if (CurrentList in [JUNCS..VALVES]) then
      ObjectIDBox.Text := GetID(CurrentList,CurrentItem[CurrentList])
  end
  else
  begin
    NodeSeriesBtn.Enabled := False;
    LinkSeriesBtn.Enabled := False;
    ObjectIDBox.Visible := False;
  end;

// Default TableType is NETNODES (network nodes at current time period)
  NetNodesBtn.Checked := True;
  PageControl1.ActivePage := TableTypePage;
end;


procedure TTableOptionsForm.InitExistingTable;
//-----------------------------------------------
// Initializes the dialog form when it is called
// for an existing table.
//-----------------------------------------------
var
  i: Integer;
  ischecked: Boolean;

begin
  with TempOptions do
  begin

  //Load items into the ListBox used to select columns in the table
    with ColumnsListBox do
    begin
      Clear;

    //List column variables used for a table displaying nodal values
      if (TableType = NETNODES) or (TableType = NODESERIES) then
      begin
        for i := Low(NodeFields) to High(NodeFields) do
        begin
          Items.Add(NodeVariable[i].Name);
          Checked[i - Low(NodeFields)] := NodeFields[i];
        end;

      //Extract name of WQ variable from Browser form
        i := Items.IndexOf(NodeVariable[NODEQUAL].Name);
        if (i >= 0) then
        begin
          ischecked := Checked[i];
          Items[i] := BrowserForm.NodeViewBox.Items[NODEQUAL];
          Checked[i] := ischecked;
        end;

      //Make selected item the one being sorted
        if (SortField > 0) then
          ItemIndex := SortField - Low(NodeFields)
        else
          ItemIndex := 0;
      end

    //List variables used for a table displaying link values
      else
      begin
        for i := Low(LinkFields) to High(LinkFields) do
        begin
          Items.Add(LinkVariable[i].Name);
          Checked[i - Low(LinkFields)] := LinkFields[i];
        end;

      //Extract name of WQ variable from Browser form
        i := Items.IndexOf(LinkVariable[LINKQUAL].Name);
        if (i >= 0) then
        begin
          ischecked := Checked[i];
          Items[i] := BrowserForm.LinkViewBox.Items[LINKQUAL];
          Checked[i] := ischecked;
        end;

      //Make selected item the one being sorted
        if (SortField > 0) then
          ItemIndex := SortField - Low(LinkFields)
        else
          ItemIndex := 0;
      end;

    //Set status of "Sorted By" check box.
    //(Make box invisible if table displays a time series)
      if (TableType = NETNODES) or (TableType = NETLINKS) then
      begin
        SortedCheckBox.Caption := TXT_SORTED_BY + Items[ItemIndex];
        if (SortField > 0) then SortedCheckBox.Checked := True;
      end
      else SortedCheckBox.Visible := False;
    end;

  //Display existing filters on "Filters" page
    FiltersListBox.Items.SetText(PChar(FilterString));

  //Since this is an existing table, make the "Table Type" page invisible
  //and start the dialog with the "Columns" page.
    TableTypePage.TabVisible := False;
    PageControl1.ActivePage := ColumnsPage;
  end;
end;


procedure TTableOptionsForm.UnloadOptions(var T: TTableOptions);
//-------------------------------------------------------------
// Unloads selections made on form to table options variable T.
//-------------------------------------------------------------
var
  i: Integer;
begin
  with T do
  begin

  //If processing a new table then save selections from TableType page
    if (NewTable = TRUE) then
    begin
      if NetNodesBtn.Checked then TableType := NETNODES;
      if NetLinksBtn.Checked then TableType := NETLINKS;
      if NodeSeriesBtn.Checked then TableType := NODESERIES;
      if LinkSeriesBtn.Checked then TableType := LINKSERIES;
      ObjectID := ObjectIDBox.Text;
      TimePeriod := 0;
      if (TimePeriodBox.Visible) then TimePeriod := TimePeriodBox.ItemIndex;
    end;

  //Save selections from Columns page
    with ColumnsListBox do
    begin
      for i := 0 to Items.Count - 1 do
      begin
        if (TableType = NETNODES) or (TableType = NODESERIES) then
          NodeFields[Low(NodeFields) + i] := Checked[i]
        else
          LinkFields[Low(LinkFields) + i] := Checked[i];
      end;
      if (SortedCheckBox.Visible) and (SortedCheckBox.Checked) then
      begin
        if (TableType = NETNODES) or (TableType = NODESERIES) then
          SortField := Low(NodeFields) + ItemIndex
        else
          SortField := Low(LinkFields) + ItemIndex;
      end
      else SortField := 0;
    end;

  // Save selections from "Filters" page
    for i := 1 to NextFilter - 1 do
      Filters[i] := TempFilters[i];
    Filters[NextFilter].Variable := -1;
    FilterString := FiltersListBox.Items.Text;
  end;
end;


procedure TTableOptionsForm.NetNodesBtnClick(Sender: TObject);
//------------------------------------------------------------
// OnClick handler for the RadioButtons that select TableType.
//------------------------------------------------------------
var
  OldType, i: Integer;
  IsChecked:  Boolean;
begin
  with TempOptions do
  begin

  //Determine which TableType has been selected
    OldType := TableType;
    if      NetNodesBtn.Checked then TableType := NETNODES
    else if NetLinksBtn.Checked then TableType := NETLINKS
    else if NodeSeriesBtn.Checked then TableType := NODESERIES
    else if LinkSeriesBtn.Checked then TableType := LINKSERIES;

  //If new type of table selected, then re-set the
  //column selection options on the Columns page
    if OldType <> TableType then
    begin
      with ColumnsListBox do
      begin
        Clear;

      //Add node variables to list box for a node-type table
        if (TableType = NETNODES) or (TableType = NODESERIES) then
        begin
          for i := Low(NodeFields) to High(NodeFields) do
          begin
            Items.Add(NodeVariable[i].Name);

          //Check off output variables if analysis results exist
            if RunFlag then
              IsChecked := (NodeVariable[i].Source = vsOutput)
          //Otherwise check off input variables
            else
              IsChecked := (NodeVariable[i].Source = vsInput);
            Checked[Items.Count-1] := IsChecked;
          end;

        //Get name of WQ variable from the Browser form
          i := Items.IndexOf(NodeVariable[NODEQUAL].Name);
          if (i >= 0) then
          begin
            Items[i] := BrowserForm.NodeViewBox.Items[NODEQUAL];
            Checked[i] := RunFlag;
          end;
        end

      //Otherwise add link variables to the list box for a link-type table
        else
        begin
          for i := Low(LinkFields) to High(LinkFields) do
          begin
            Items.Add(LinkVariable[i].Name);

          //Check off output variables if analysis results exist
            if RunFlag then
              IsChecked := (LinkVariable[i].Source = vsOutput)
          //Otherwise check off input variables
            else
              IsChecked := (LinkVariable[i].Source = vsInput);
            Checked[Items.Count-1] := IsChecked;
          end;

        //Get name of WQ variable from the Browser form
          i := Items.IndexOf(LinkVariable[LINKQUAL].Name);
          if (i >= 0) then
          begin
            Items[i] := BrowserForm.LinkViewBox.Items[LINKQUAL];
            Checked[i] := RunFlag;
          end;
        end;
        ItemIndex := 0;
        if (TableType = NETNODES) or (TableType = NETLINKS) then
        begin
          SortedCheckBox.Caption := TXT_SORTED_BY + Items[ItemIndex];
          SortedCheckBox.Enabled := True;
        end
        else
        begin
          SortedCheckBox.Caption := TXT_SORTED_BY;
          SortedCheckBox.Enabled := False;
        end;
      end;

    //Clear all controls on "Filters" page
      FilterVariableBox.Clear;
      FilterVariableBox.Items.Assign(ColumnsListBox.Items);
      FilterVariableBox.ItemIndex := 0;
      FilterRelationBox.ItemIndex := 0;
      FilterValueBox.Clear;
      FiltersListBox.Clear;
    end;
  end;
end;


procedure TTableOptionsForm.ColumnsListBoxClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for CheckListBox that selects columns
// to be included in the table.
//------------------------------------------------------
begin
//Update status of the SortedCheckBox
  if SortedCheckBox.Enabled then with ColumnsListBox do
  begin
    SortedCheckBox.Caption := TXT_SORTED_BY + Items[ItemIndex];
    if not Checked[ItemIndex] then SortedCheckBox.Checked := False;
  end;
end;


procedure TTableOptionsForm.ColumnsListBoxClickCheck(Sender: TObject);
//---------------------------------------------------------------
// OnClick handler for check box next to item in the CheckListBox
// that selects columns to be included in the table.
//---------------------------------------------------------------
begin
//Uncheck SortedCheckBox if current item is unchecked
  if SortedCheckBox.Enabled then with ColumnsListBox do
    if not Checked[ItemIndex] then SortedCheckBox.Checked := False;
end;


procedure TTableOptionsForm.SortedCheckBoxClick(Sender: TObject);
//-----------------------------------------------------------
// OnClick handler for the SortedCheckBox that determines
// if the table gets sorted according to a particular column.
//-----------------------------------------------------------
begin
  with ColumnsListBox do
    if SortedCheckBox.Checked then Checked[ItemIndex] := True;
end;


procedure TTableOptionsForm.FilterAddBtnClick(Sender: TObject);
//----------------------------------------------------------------
// OnClick handler for button that adds a new filter to the table
//----------------------------------------------------------------
var
  s: String;
begin
  if (NextFilter > MAXFILTERS) then
    Uutils.MsgDlg(MSG_FILTER_LIMIT,mtWARNING,[mbOK])
  else
  begin
    s := FilterValueBox.Text;
    if (Length(Trim(s)) > 0) then
    begin
      TempFilters[NextFilter].Variable := FilterVariableBox.ItemIndex + 1;
      TempFilters[NextFilter].Relation :=
        TRelationType(FilterRelationBox.ItemIndex);
      TempFilters[NextFilter].StrValue := s;
      Inc(NextFilter);
      s := FilterVariableBox.Items[FilterVariableBox.ItemIndex];
      s := s + ' ' + FilterRelationBox.Items[FilterRelationBox.ItemIndex];
      s := s + ' ' + FilterValueBox.Text;
      FiltersListBox.Items.Add(s);
    end;
  end;
end;


procedure TTableOptionsForm.FilterDelBtnClick(Sender: TObject);
//----------------------------------------------------------------
// OnClick handler for button that deletes a filter from the table
//----------------------------------------------------------------
var
  i, first, last: Integer;
begin
  with FiltersListBox do
  begin
    if ItemIndex >= 0 then
    begin
      first := ItemIndex + 1;
      last  := NextFilter - 2;
      for i := first to last do TempFilters[i] := TempFilters[i+1];
      Dec(NextFilter);
      Items.Delete(ItemIndex);
    end;
  end;
end;


procedure TTableOptionsForm.OKBtnClick(Sender: TObject);
//----------------------------------------------
// OnClick handler for the OK button.
//----------------------------------------------
var
  i,j: Integer;
  S: String;
begin
//Check that a valid Object ID was supplied for a new NODESERIES
//or LINKSERIES table.
  if (NewTable = TRUE) then
  begin
    S := ObjectIDBox.Text;
    if (NodeSeriesBtn.Checked) and (not FindNode(S,i,j))
    then Uutils.MsgDlg(MSG_NO_NODE + S, mtError, [mbOK])  // NODESERIES
    else if (LinkSeriesBtn.Checked) and (not FindLink(S,i,j))
    then Uutils.MsgDlg(MSG_NO_LINK + S, mtError, [mbOK]) // LINKSERIES
    else ModalResult := mrOK;
  end
  else ModalResult := mrOK;
end;


procedure TTableOptionsForm.HelpBtnClick(Sender: TObject);
//----------------------------------------------
// OnClick handler for the Help button.
//----------------------------------------------
var
  HC: Integer;
begin
  with PageControl1 do
    if ActivePage = TableTypePage then HC := 264
    else if ActivePage = ColumnsPage then HC := 265
    else HC := 266;
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, HC);
end;

end.
