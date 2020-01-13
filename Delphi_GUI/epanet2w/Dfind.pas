unit Dfind;

{-------------------------------------------------------------------}
{                    Unit:    Dfind.pas                             }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box used to find a specific node or     }
{   link or WQ source nodes on the network map.                     }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, System.UITypes, Uglobals, Uutils;

const
  TXT_NO_SUCH_OBJECT = 'There is no such object on the map';
  TXT_NO_SOURCE_NODES = 'There are no WQ source nodes.';
  TXT_LINK_NOT_ON_MAP = 'Link exists but is not on the map.';
  TXT_NODE_NOT_ON_MAP = 'Node exists but is not on the map.';
  GBCaption: array[0..2] of PChar =
    ('Adjacent Links', 'Adjacent Nodes', 'Source Nodes');

type
  TFindForm = class(TForm)
    RadioGroup1: TRadioGroup;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Button1: TButton;
    GroupBox2: TGroupBox;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Edit1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    procedure GetAdjacentObjects;
    procedure GetWQSources;
    procedure UpdateMapDisplay;
  public
    { Public declarations }
    FoundObject: Integer;
    FoundIndex:  Integer;
    procedure Clear;
  end;

var
  FindForm: TFindForm;

implementation

{$R *.DFM}

uses Fmain, Fbrowser, Fmap, Fovmap, Uinput, Umap;

procedure TFindForm.FormCreate(Sender: TObject);
//----------------------------------------------
// Form's OnCreate event handler.
// Position form at top left of Main form.
//----------------------------------------------
var
  P: TPoint;
begin
  Uglobals.SetFont(self);
  with MainForm do
  begin
    P.x := Left + (Width - ClientWidth) - 2;
    P.Y := Top + (Height-ClientHeight) - 2;
  end;
  Top := P.Y;
  Left := P.X;
end;

procedure TFindForm.FormShow(Sender: TObject);
//--------------------------------------------
// Form's OnShow event handler.
// Updates font to current global font.
//--------------------------------------------
begin
  Uglobals.SetFont(self);
  RadioGroup1Click(Sender);
end;

procedure TFindForm.FormClose(Sender: TObject; var Action: TCloseAction);
//----------------------------------------------------------------------
// Form's OnClose event handler (when user clicks
// the Close icon in the system menu in the form's border.
//-----------------------------------------------------------------------
begin
  Action := caHide;
end;

procedure TFindForm.Clear;
//-------------------------------------------------
// Clears ID field and Adjacent Nodes/Links fields
//-------------------------------------------------
begin
  Edit1.Text := '';
  ListBox1.Clear;
end;

procedure TFindForm.Button1Click(Sender: TObject);
//------------------------------------------------
// OnClick event handler for the Find button.
//------------------------------------------------
var
  s     : String;
  Found : Boolean;

begin
{*** Updated 12/29/00 ***}
// Place map in Selection mode
    MainForm.SelectorButtonClick;

// Search database for specified node/link ID
// and save object type and index if found.
  s := Edit1.Text;
  ListBox1.Clear;
  if (RadioGroup1.ItemIndex = 2) then
  begin
    GetWQSources;
    Exit;
  end;
  if (RadioGroup1.ItemIndex = 0) then
    Found := Uinput.FindNode(s,FoundObject,FoundIndex)
  else
    Found := Uinput.FindLink(s,FoundObject,FoundIndex);

// If ID found then highlight object on the map and
// make it the current item shown in the Browser.
  if Found then
  begin
    GetAdjacentObjects;
    UpdateMapDisplay;
    BrowserForm.UpdateBrowser(FoundObject,FoundIndex)
  end

// If not found then issue a message.
  else
    Uutils.MsgDlg(TXT_NO_SUCH_OBJECT,mtInformation,[mbOK]);
  Edit1.SetFocus;
  Edit1.SelectAll;
end;

procedure TFindForm.GetAdjacentObjects;
//----------------------------------------------
// Lists links connected to a found node or
// nodes connected to a found link.
//----------------------------------------------
var
  aLink: TLink;
  aNode: TNode;
  k,m  : Integer;
begin
  if FoundObject in [PIPES, PUMPS, VALVES] then
  begin
    aLink := Link(FoundObject,FoundIndex);
    ListBox1.Items.Add(aLink.Node1.ID);
    ListBox1.Items.Add(aLink.Node2.ID);
  end
  else
  begin
    aNode := Node(FoundObject,FoundIndex);
    for k := PIPES to VALVES do
    begin
      for m := Network.Lists[k].Count-1 downto 0 do
      begin
        aLink := Link(k,m);
        if (aLink.Node1 = aNode) or (aLink.Node2 = aNode)
          then ListBox1.Items.Add(GetID(k,m));
      end;
    end;
  end;
end;

procedure TFindForm.GetWQSources;
//----------------------------------------------
// Lists WQ source nodes in network
//----------------------------------------------
var
  i,j,k: Integer;
  x    : Single;
begin
  for i := JUNCS to TANKS do
  begin
    case i of
      JUNCS:   k := JUNC_SRCQUAL_INDEX;
      RESERVS: k := RES_SRCQUAL_INDEX;
      TANKS:   k := TANK_SRCQUAL_INDEX;
      else     k := 0;
    end;
    for j := 0 to Network.Lists[i].Count-1 do
      if Uutils.GetSingle(Node(i,j).Data[k],x) and (x > 0) then
        ListBox1.Items.Add(GetID(i,j));
  end;
  if ListBox1.Items.Count = 0 then
    Uutils.MsgDlg(TXT_NO_SOURCE_NODES,mtInformation,[mbOK]);
end;

procedure TFindForm.UpdateMapDisplay;
//----------------------------------------------
// Highlights found object on the network map,
// panning the map into position if necessary.
//----------------------------------------------
var
  P1, P2, P: TPoint;
  Xf, Yf   : Single;
  aNode1   : TNode;
  aNode2   : TNode;

begin
  with MapForm do
  begin

  // If found object is a link then get coords. of midpoint
    if FoundObject in [PIPES..VALVES] then
    begin
      aNode1 := Link(FoundObject,FoundIndex).Node1;
      aNode2 := Link(FoundObject,FoundIndex).Node2;
      if not (Map.GetNodePixPos(aNode1,P1))
      or not (Map.GetNodePixPos(aNode2,P2)) then
      begin
        Uutils.MsgDlg(TXT_LINK_NOT_ON_MAP, mtInformation, [mbOK]);
        Exit;
      end;
      P.X := (P1.X + P2.X) div 2;
      P.Y := (P1.Y + P2.Y) div 2;
      Xf := (aNode1.X + aNode2.X) / 2;
      Yf := (aNode1.Y + aNode2.Y) / 2;
    end

  // Otherwise get found node's coords.
    else
    begin
      aNode1 := Node(FoundObject,FoundIndex);
      if not Map.GetNodePixPos(aNode1,P) then
      begin
        Uutils.MsgDlg(TXT_NODE_NOT_ON_MAP, mtInformation, [mbOK]);
        Exit;
      end;
      Xf := aNode1.X;
      Yf := aNode1.Y;
    end;

  // If object within current map view window then exit
    if PtInRect(Map.Window.MapRect,P) then Exit;

  // Adjust map offset to position object in center of map
    with Map.Window do
    begin
      Woffset.X := Xf - Pwidth/PPW/2;
      Woffset.Y := Yf - Pheight/PPW/2;
    end;

  // Redraw the map
{*** Updated 11/19/01 ***}
    Map.RedrawBackdrop;
    RedrawMap;
    OVMapForm.ShowMapExtent;
  end;
end;

procedure TFindForm.RadioGroup1Click(Sender: TObject);
//-----------------------------------------------------------
// OnClick handler for RadioGroup that selects Node, Link,
// or Sources. Changes caption on list of adjacent objects.
//-----------------------------------------------------------
begin
  GroupBox2.Caption := GBCaption[RadioGroup1.ItemIndex];
  ListBox1.Clear;
  Edit1.Visible := not (RadioGroup1.ItemIndex = 2);
end;

procedure TFindForm.FormKeyPress(Sender: TObject; var Key: Char);
//---------------------------------------------------------------
// OnKeyPress handler for form (including the ID Edit box).
//---------------------------------------------------------------
begin
// Submit search if Enter key was pressed.
  if Key = #13 then
  begin
    Key := #0;
    Button1Click(Sender);
  end;

// Hide form if Escape key was pressed.
  if Key = #27 then
  begin
    Key := #0;
    Hide;
  end;
end;

procedure TFindForm.Edit1Change(Sender: TObject);
//----------------------------------------------
// OnChange handler for ID Edit box.
//----------------------------------------------
begin
  ListBox1.Clear;
end;

procedure TFindForm.ListBox1Click(Sender: TObject);
//---------------------------------------------------------------
// OnClick handler for listbox that displays adjacent links/nodes
// to the found node/link. Highlights the selected object both on
// the map and in the Data Browser.
//---------------------------------------------------------------
var
  s: String;
  Found: Boolean;
begin
// Get ID of adjacent object selected
  with ListBox1 do
    s := Items[ItemIndex];

// Search for object in Node or Link database
  if (RadioGroup1.ItemIndex in [1,2]) then
    Found := Uinput.FindNode(s,FoundObject,FoundIndex)
  else
    Found := Uinput.FindLink(s,FoundObject,FoundIndex);

// If object found then highlight it on the map and
// make it the current item shown in the Browser.
  if Found then
  begin
    UpdateMapDisplay;
    BrowserForm.UpdateBrowser(FoundObject,FoundIndex)
  end;
end;

end.
