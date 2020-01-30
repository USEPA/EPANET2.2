unit Dmap;

{-------------------------------------------------------------------}
{                    Unit:    Dmap.pas                              }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box for changing Network Map display    }
{   options.                                                        }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, Math, Uglobals, Grids;

type
  TMapOptionsForm = class(TForm)

    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Panel1: TPanel;
    Notebook1: TNotebook;
    NodesBySize: TCheckBox;
    JuncSymbols: TCheckBox;
    NodeBorder: TCheckBox;
    GroupBox2: TGroupBox;
    NodeShape: TShape;
    NodeSpin: TSpinEdit;
    LinksBySize: TCheckBox;
    GroupBox3: TGroupBox;
    LinkShape: TShape;
    LinkSpin: TSpinEdit;
    Label4: TLabel;
    MapLabels: TCheckBox;
    SpinEdit1: TSpinEdit;
    Label7: TLabel;
    NodeIDs: TCheckBox;
    NodeValues: TCheckBox;
    LinkIDs: TCheckBox;
    LinkValues: TCheckBox;
    SpinEdit4: TSpinEdit;
    NotationTransparent: TCheckBox;
    Label3: TLabel;
    TankSymbols: TCheckBox;
    PumpSymbols: TCheckBox;
    ValveSymbols: TCheckBox;
    EmitterSymbols: TCheckBox;
    SourceSymbols: TCheckBox;
    SpinEdit2: TSpinEdit;
    Label6: TLabel;
    Label8: TLabel;
    SpinEdit3: TSpinEdit;
    LinkArrows: TRadioGroup;
    ArrowSpin: TSpinEdit;
    Label5: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    ListBox1: TListBox;
    LabelsTransparent: TCheckBox;

{*** Updated 3/1/01 ***}    
    NotationFontSize: TSpinEdit;
    Label1: TLabel;
    LinkBorder: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpinEdit1Change(Sender: TObject);
    procedure NodeSpinChange(Sender: TObject);
    procedure LinkSpinChange(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnHelpClick(Sender: TObject);
    procedure NodeBorderClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
    RadioButtons: array[1..4] of TRadioButton;
    ZoomLevel: array[1..4] of Integer;
    procedure ResizeNodeShape;
    procedure ResizeLinkShape;
  public
    { Public declarations }
    TmpOptions: TMapOptions;
    procedure GetActivePage(var aPage: Integer);
    procedure LoadOptions;
    procedure SetActivePage(aPage: Integer);
    procedure UnloadOptions;
  end;

var
  MapOptionsForm: TMapOptionsForm;

implementation

{$R *.DFM}

uses Umap;

procedure TMapOptionsForm.FormCreate(Sender: TObject);
//-------------------------------------------------
// OnCreate handler for form.
//-------------------------------------------------
var
  i: Integer;
begin
  Uglobals.SetFont(self);
  for i := 1 to 4 do
    RadioButtons[i] := FindComponent('RadioButton' + IntToStr(i))
      as TRadioButton;
  with ListBox1 do
  begin
    ItemHeight := (ClientHeight) div Items.Count;
    ItemIndex := 0;
  end;
  ListBox1Click(Sender);
end;

procedure TMapOptionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//------------------------------------------------------------------
// OnKeyDown handler for form (KeyPreview was set to True).
// Calls OnClick handler for Help button when F1 is pressed.
//------------------------------------------------------------------
begin
  if Key = VK_F1 then BtnHelpClick(Sender);
end;

procedure TMapOptionsForm.LoadOptions;
//-------------------------------------------------
// Loads current map display options into form.
//-------------------------------------------------

var
  i: Integer;
begin
  with TmpOptions do
  begin
    NodeSpin.Value := NodeSize;
    NodesBySize.Checked := DispNodesBySize;

    LinkSpin.Value := LinkSize;
    LinksBySize.Checked := DispLinksBySize;
    LinkBorder.Checked := DispLinkBorder;

    NodeBorder.Checked := DispNodeBorder;
    JuncSymbols.Checked := DispJuncs;
    TankSymbols.Checked := DispTanks;
    PumpSymbols.Checked := DispPumps;
    ValveSymbols.Checked := DispValves;
    EmitterSymbols.Checked := DispEmitters;
    SourceSymbols.Checked := DispSources;

    LinkArrows.ItemIndex := Ord(ArrowStyle);
    ArrowSpin.Value := ArrowSize;

    MapLabels.Checked := DispLabels;
    LabelsTransparent.Checked := LabelsTranspar;

    NodeIDs.Checked := DispNodeIDs;
    NodeValues.Checked := DispNodeValues;
    LinkIDs.Checked := DispLinkIDs;
    LinkValues.Checked := DispLinkValues;
    NotationTransparent.Checked := NotationTranspar;
    NotationFontSize.Value := NotationSize;      {*** Updated 3/1/01 ***}

    ZoomLevel[1] := LabelZoom;
    ZoomLevel[2] := SymbolZoom;
    ZoomLevel[3] := ArrowZoom;
    ZoomLevel[4] := NotationZoom;

    for i := 1 to 4 do
      with FindComponent('SpinEdit' + IntToStr(i))
        as TSpinEdit do
          Value := ZoomLevel[i];

    for i := 1 to 4 do
      RadioButtons[i].Checked := False;
    RadioButtons[ColorIndex].Checked := True;
  end;
  ResizeNodeShape;
  ResizeLinkShape;
  NodeBorderClick(self);
  JuncSymbols.Enabled := not QueryFlag;
end;

procedure TMapOptionsForm.UnloadOptions;
//---------------------------------------------------
// Unloads contents of form into map display options.
//---------------------------------------------------
var
  i: Integer;
begin
  with TmpOptions do
  begin
    NodeSize := NodeSpin.Value;
    DispNodesBySize := NodesBySize.Checked;

    LinkSize := LinkSpin.Value;
    DispLinksBySize := LinksBySize.Checked;
    DispLinkBorder := LinkBorder.Checked;

    DispNodeBorder := NodeBorder.Checked;
    DispJuncs := JuncSymbols.Checked;
    DispTanks := TankSymbols.Checked;
    DispPumps := PumpSymbols.Checked;
    DispValves := ValveSymbols.Checked;
    DispEmitters := EmitterSymbols.Checked;
    DispSources := SourceSymbols.Checked;

    ArrowStyle := TArrowStyle(LinkArrows.ItemIndex);
    ArrowSize := ArrowSpin.Value;

    DispLabels := MapLabels.Checked;
    LabelsTranspar := LabelsTransparent.Checked;

    DispNodeIDs := NodeIDs.Checked;
    DispNodeValues := NodeValues.Checked;
    DispLinkIDs := LinkIDs.Checked;
    DispLinkValues := LinkValues.Checked;
    NotationTranspar := NotationTransparent.Checked;
    NotationSize := NotationFontSize.Value;      {*** Updated 3/1/01 ***}

    LabelZoom := ZoomLevel[1];
    SymbolZoom := ZoomLevel[2];
    ArrowZoom := ZoomLevel[3];
    NotationZoom := ZoomLevel[4];

    for i := 1 to 4 do
      if RadioButtons[i].Checked then ColorIndex := i;
  end;
end;

procedure TMapOptionsForm.GetActivePage(var aPage: Integer);
//---------------------------------------------------------
// Retrieves index of page of options currently displayed.
//---------------------------------------------------------
begin
  aPage := ListBox1.ItemIndex;
end;

procedure TMapOptionsForm.SetActivePage(aPage: Integer);
//------------------------------------------------------
// Displays specific page of map options.
//------------------------------------------------------
begin
  with ListBox1 do
  begin
    if aPage < Items.Count then
    begin
      ItemIndex := aPage;
      ListBox1Click(Self);
    end;
  end;
end;


procedure TMapOptionsForm.ListBox1Click(Sender: TObject);
//------------------------------------------------------
// OnClick handler for the option category list box.
//------------------------------------------------------
begin
  NoteBook1.PageIndex := ListBox1.ItemIndex;
end;

procedure TMapOptionsForm.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
//------------------------------------------------------------
// OnDrawItem handler for the item category listbox.
// Centers the text within the item's drawing rectangle.
//------------------------------------------------------------
var
  ht: Integer;
  dy: Integer;
  s:  String;
begin
  with Control as TListBox do
  begin
    s := Items[Index];
    ht := Canvas.TextHeight(s);
    Canvas.FillRect(Rect);
    dy := (Rect.Bottom - Rect.Top - ht) div 2;
    Canvas.TextOut(0,Rect.Top+dy,s);
  end;
end;

procedure TMapOptionsForm.SpinEdit1Change(Sender: TObject);
//--------------------------------------------------------
// OnChange handler for all of the SpinEdit controls
// associated with Zoom Level options. The Tag property
// assigned to these controls determine which Zoom Level
// property is being changed (1 = Labels, 2 = Symbols,
// 3 = Arrows, 4 = Notation).
//--------------------------------------------------------
begin
  with Sender as TSpinEdit do
    ZoomLevel[Tag] := Value;
end;

procedure TMapOptionsForm.NodeSpinChange(Sender: TObject);
//-------------------------------------------------------
// OnChange handler for Node Size SpinEdit control.
//-------------------------------------------------------
begin
  ResizeNodeShape;
end;

procedure TMapOptionsForm.ResizeNodeShape;
//----------------------------------------
// Resizes the NodeShape control
//----------------------------------------
var
  newsize : Integer;
  aRect   : TRect;
begin
  newsize := 3*NodeSpin.Value+2;
  aRect := NodeShape.BoundsRect;
  aRect.Top := NodeSpin.Top + (NodeSpin.Height div 2)
               - (newsize div 2);
  aRect.Bottom := aRect.Top + newsize;// + 1;
  aRect.Right := aRect.Left + newsize;// + 1;
  NodeShape.BoundsRect := aRect;
end;

procedure TMapOptionsForm.LinkSpinChange(Sender: TObject);
//-------------------------------------------------------
// OnChange handler for Link Size SpinEdit control.
//-------------------------------------------------------
begin
  ResizeLinkShape;
end;

procedure TMapOptionsForm.ResizeLinkShape;
//----------------------------------------
// Resizes the LinkShape control
//----------------------------------------
begin
  LinkShape.Top := LinkSpin.Top + (LinkSpin.Height - LinkSpin.Value) div 2;
  LinkShape.Pen.Width := LinkSpin.Value;
  LinkShape.Height := LinkSpin.Value;
end;

procedure TMapOptionsForm.Shape1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//--------------------------------------------------------
// OnMouseDown handler for Shape controls used to display
// background color choices. Causes the corresponding
// RadioButton control to be selected.
//--------------------------------------------------------
var
  i: Integer;
begin
  with Sender as TShape do
    i := Tag;
  with FindComponent('RadioButton' + IntToStr(i))
    as TRadioButton do
      Checked := True;
end;

procedure TMapOptionsForm.NodeBorderClick(Sender: TObject);
//--------------------------------------------------------
// OnClick handler for NodeBorder checkbox. Causes the
// node shape symbol to be drawn with/without a border.
//--------------------------------------------------------
begin
  if NodeBorder.Checked then
    NodeShape.Pen.Color := clBlack
  else
    NodeShape.Pen.Color := clRed;
end;

procedure TMapOptionsForm.BtnHelpClick(Sender: TObject);
var
  HC: Integer;
begin
  case NoteBook1.PageIndex of
    0: HC := 193;
    1: HC := 194;
    2: HC := 195;
    3: HC := 198;
    4: HC := 196;
    5: HC := 197;
    6: HC := 199;
    else HC := 0;
  end;
  if HC > 0
  then HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, HC);
end;

end.
