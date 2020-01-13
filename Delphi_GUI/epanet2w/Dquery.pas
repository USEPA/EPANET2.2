unit Dquery;

{-------------------------------------------------------------------}
{                    Unit:    Dquery.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{  Form unit with a dialog box for performing a Map Query           }
{  (e.g., locate all nodes with Pressure below 30).                 }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Uglobals, Uutils, ExtCtrls;

const
  TXT_NODES_WITH = 'Find Nodes with';
  TXT_LINKS_WITH = 'Find Links with';
  TXT_ITEMS_FOUND = ' items found';

type
  TQueryForm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Button1: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    OldLinkVar: Integer;
    OldNodeVar: Integer;
    OldDispJuncs: Boolean;
  public
    { Public declarations }
    procedure Clear;
    procedure UpdateQueryCaption;
  end;

var
  QueryForm: TQueryForm;

implementation

{$R *.DFM}

uses Fbrowser, Fmain, Fmap, Uoutput;

procedure TQueryForm.FormCreate(Sender: TObject);
//-----------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------
var
  I: Integer;
  P: TPoint;
begin
// Load items into Node/Link selection combo box
  ComboBox1.Items.Add(TXT_NODES_WITH);
  ComboBox1.Items.Add(TXT_LINKS_WITH);
  ComboBox1.ItemIndex := 0;

// Call OnChange handler to load appropriate variables
// into combo box that selects a variable to query
  ComboBox1Change(self);

// Load the choices of relations to query on (below, equals, above)
  for I := Low(FilterRelation) to High(FilterRelation) do
    ComboBox3.Items.Add(FilterRelation[I]);
  ComboBox3.ItemIndex := 0;
  OldNodeVar := -1;
  OldLinkVar := -1;

// Position form at top left of Main form
  with MainForm do
  begin
    P.x := Left + (Width - ClientWidth) - 2;
    P.Y := Top + (Height-ClientHeight) - 2;
  end;
  Top := P.Y;
  Left := P.X;
end;

procedure TQueryForm.FormShow(Sender: TObject);
//---------------------------------------
// OnShow handler for the form.
//---------------------------------------
begin
// Set form's font
  Uglobals.SetFont(self);
  Panel1.Font.Color := clRed;

// Make sure map will display junction symbols
  with MapForm.Map.Options do
  begin
    OldDispJuncs := DispJuncs;
    DispJuncs := True;
  end;
end;

procedure TQueryForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-------------------------------------------------------------------------
// OnClose handler for form.
// Restores the map display to the state it had before last query was made.
//-------------------------------------------------------------------------
begin
  if (QueryFlag) then
  begin
    QueryFlag := False;
    CurrentNodeVar := OldNodeVar;
    CurrentLinkVar := OldLinkVar;
    OldNodeVar := -1;
    OldLinkVar := -1;
    Uoutput.SetNodeColors;
    Uoutput.SetLinkColors;
    MapForm.Map.Options.DispJuncs := OldDispJuncs;
    MapForm.RedrawMap;
    MapForm.DrawNodeLegend;
    MapForm.DrawLinkLegend;
  end;
  BrowserForm.NodeViewBox.Enabled := True;
  BrowserForm.LinkViewBox.Enabled := True;
  Action := caHide;
end;

procedure TQueryForm.Clear;
//-----------------------------------------------
// Clears Query Value and Results fields of form.
//-----------------------------------------------
begin
  Edit1.Text := '';
  Panel1.Caption := '';
end;

procedure TQueryForm.ComboBox1Change(Sender: TObject);
//----------------------------------------------------
// OnChange handler for the Node/Link combo box.
// Changes list of map display variables to choose
// from when user switches between Node & Link query.
//----------------------------------------------------
var
  i: Integer;
begin
  ComboBox2.Clear;
  if ComboBox1.ItemIndex = 0 then
    for i := ELEVATION to NODEQUAL do
      ComboBox2.Items.Add(NodeVariable[i].Name)
  else
    for i := LINKLENGTH to LINKQUAL do
      ComboBox2.Items.Add(LinkVariable[i].Name);
  ComboBox2.ItemIndex := 0;
end;

procedure TQueryForm.Button1Click(Sender: TObject);
//----------------------------------------------------------------
// OnClick handler for the command button that executes the query.
//----------------------------------------------------------------
begin
// Check for valid numerical entry for query comparison value
  if Uutils.IsValidNumber(Edit1.Text,QueryValue) then
  begin

  // Set a flag & save query relation type
    QueryFlag := True;
    QueryRelation := TRelationType(ComboBox3.ItemIndex);

  // Disable selection of map view variable from Browser form
    BrowserForm.NodeViewBox.Enabled := False;
    BrowserForm.LinkViewBox.Enabled := False;

  // If this is first query since form was shown again then save
  // current status of map view
    if (OldNodeVar < 0) then OldNodeVar := CurrentNodeVar;
    if (OldLinkVar < 0) then OldLinkVar := CurrentLinkVar;

  // Select the query variable for viewing on the map
    CurrentLinkVar := NOVIEW;
    CurrentNodeVar := NOVIEW;
    if (ComboBox1.ItemIndex = 0) then
    begin
      CurrentNodeVar := ComboBox2.ItemIndex+1;
      Uoutput.SetNodeColors;
    end
    else
    begin
      CurrentLinkVar := ComboBox2.ItemIndex+1;
      Uoutput.SetLinkColors;
    end;

  // Display number of items matching the query
    UpdateQueryCaption;

  // Redraw the map
    MapForm.RedrawMap;
    MapForm.DrawNodeLegend;
    MapForm.DrawLinkLegend;
  end
  else Edit1.SetFocus;
end;

procedure TQueryForm.Edit1KeyPress(Sender: TObject; var Key: Char);
//-------------------------------------------------------------
// OnKeyPress handler for the Edit box where user enters a
// value to compare against. Causes the query to be submitted
// if the user hits the Enter key.
//-------------------------------------------------------------
begin
  if Key = #13 then
  begin
    Button1Click(Sender);
    Key := #0;
  end;
end;

procedure TQueryForm.FormKeyPress(Sender: TObject; var Key: Char);
//----------------------------------
// OnKeyPress handler for the form.
// Closes form when user hits Esc.
//----------------------------------
begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;
end;

procedure TQueryForm.UpdateQueryCaption;
//----------------------------------------------------------------
// Updates the display of number of items matching the query.
//----------------------------------------------------------------
var
  I,J,N: Integer;
begin
  Panel1.Caption := '';
  N := 0;
  if CurrentNodeVar <> NOVIEW then
  begin
      for I := JUNCS to TANKS do
        for J := 0 to Network.Lists[I].Count-1 do
          if Node(I,J).ColorIndex > 0 then Inc(N);
  end
  else if CurrentLinkVar <> NOVIEW then
  begin
      for I := PIPES to VALVES do
        for J := 0 to Network.Lists[I].Count-1 do
          if Link(I,J).ColorIndex > 0 then Inc(N);
  end;
  Panel1.Caption := IntToStr(N) + TXT_ITEMS_FOUND;
end;

end.
