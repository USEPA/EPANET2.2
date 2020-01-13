unit Fmap;

{-------------------------------------------------------------------}
{                    Unit:    Fmap.pas                              }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{    MDI child form that displays a map of the pipe network         }
{    being analyzed. The map is an object of class TMap             }
{    whose display is handled by methods in the Umap.pas unit.      }
{    Methods contained in this form handle user interaction         }
{    with the map, such as zooming, panning, and adding             }
{    or moving map objects. The form is created on startup          }
{    and remains active throughout the session.                     }
{-------------------------------------------------------------------}

(*******************************************************************
    The main components on this form include:
      PopupMenu1 - popup menu for editing nodes & links
      PopupMenu2 - popup menu for toggling display of
                   legends, backdrop and map options dialog
      PopupMenu3 - popup menu for editing link vertices
      NodeLegendPanel - panel containing Paintbox that
                        displays legend for nodes
      LinkLegendPanel - panel containing Pantbox that
                        displays legend for links
      TimePanel - panel displaying simulation time of day
      HintPanel - panel displaying flyover information
                  on node or link under the mouse pointer
      Timer1    - timer used to make highlighted object blink
*********************************************************************)

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Spin, StdCtrls, ExtCtrls, Tabs, Buttons, Menus,
  Clipbrd, Math, Printers, System.Types, System.UITypes,
  Xprinter, Uglobals, Uutils, Umap;

const
  MAXZOOMRATIO = 1000000;    //Max. zoom-in ratio
  MAXVERTICES  = 100;        //Max. number of vertices fenceline
  TICKDELAY    = 100;        //Delay before object can be moved
  MSG_NO_FIND_BACKDROP = 'Could not find backdrop file ';
  MSG_NO_READ_BACKDROP = 'Could not read backdrop file ';
  TXT_RESTORE_JUNCS = 'Restoring display of junctions on the map.';
  TXT_RESTORE_LABELS = 'Restoring display of labels on the map.';
  TXT_NODE = 'Node ';

type

  TLinkInfo = record        //Data structure for drawing a link
    Node1, Node2  : TNode;  // End nodes of link
    Point1, Point2: TPoint; // Pixel location of link's end points
  end;

  TMapForm = class(TForm)

    PopupMenu1: TPopupMenu;
      PopupCopy: TMenuItem;
      PopupPaste: TMenuItem;
      PopupDelete: TMenuItem;
      PopupReverse: TMenuItem;
      N4: TMenuItem;
      PopupVertices: TMenuItem;
      PopupProperties: TMenuItem;

    PopupMenu2: TPopupMenu;
      PopupNodeLegend: TMenuItem;
      PopupLinkLegend: TMenuItem;
      PopupTimeLegend: TMenuItem;
      PopupBackdrop: TMenuItem;
      N1: TMenuItem;
      PopupOptions: TMenuItem;

    PopupMenu3: TPopupMenu;
      PopupAddVertex: TMenuItem;
      PopupDeleteVertex: TMenuItem;
      N2: TMenuItem;
      PopupQuitEditing: TMenuItem;

    NodeLegendPanel: TPanel;
    NodeLegendBox: TPaintBox;

    LinkLegendPanel: TPanel;
    LinkLegendBox: TPaintBox;

    TimeLegendPanel: TPanel;

    HintPanel: TPanel;
    HintLabel: TLabel;
    Timer1: TTimer;
    Timer2: TTimer;

////    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDblClick(Sender: TObject);

    procedure NodeLegendBoxPaint(Sender: TObject);
    procedure NodeLegendBoxDblClick(Sender: TObject);
    procedure LinkLegendBoxPaint(Sender: TObject);
    procedure LinkLegendBoxDblClick(Sender: TObject);
    procedure TimeLegendPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimeLegendPanelDblClick(Sender: TObject);
    procedure DragLegend(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure Timer1Timer(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure PopupMenu3Popup(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure PopupPasteClick(Sender: TObject);
    procedure PopupDeleteClick(Sender: TObject);
    procedure PopupReverseClick(Sender: TObject);
    procedure PopupPropertiesClick(Sender: TObject);
    procedure PopupOptionsClick(Sender: TObject);
    procedure PopupNodeLegendClick(Sender: TObject);
    procedure PopupLinkLegendClick(Sender: TObject);
    procedure PopupTimeLegendClick(Sender: TObject);
    procedure PopupBackdropClick(Sender: TObject);
    procedure PopupAddVertexClick(Sender: TObject);
    procedure PopupDeleteVertexClick(Sender: TObject);
    procedure PopupVerticesClick(Sender: TObject);
    procedure PopupQuitEditingClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  private
    { Private declarations }
    FlyOverObject: Integer;     //Type of object under the mouse pointer
    FlyOverIndex: Integer;      //Item index of object under mouse pointer
    MapOptionsPage: Integer;    //Current page of map options dialog form
    NodeHiliteSize: Integer;    //Size of highlighting rectangle for nodes
    LinkHiliteSize: Integer;    //Size of highlighting rectangle for links
    HiliteObject: Integer;      //Type of object highlighted on map
    HiliteIndex: Integer;       //Index of item highlighted on map
    SelectedObject: Integer;    //Type of object selected on map
    SelectedIndex : Integer;    //Index of item selected on map
    SelectedLink: TLink;        //Pointer to selected link
    SelectedVertex: PVertex;    //Pointer to selected link vertex
    LinkInfo : TLinkInfo;       //Data structure used for drawing links
    Moving   : Boolean;         //True if user is moving an object
    Aligning : Boolean;         //True if user is aligning the backdrop
    Fencing  : Boolean;         //True if user is drawing a fenceline
    Panning  : Boolean;         //True if user is panning the map
    Vertexing: Boolean;         //True if user is editing a link's vertices
    Zooming  : Boolean;         //True if user if zooming the map
    ZoomRect : TRect;           //Rectangle that defines zoom-in extent
    OldTickCount: DWORD;        //Used to measure a small time delay
    procedure AddVertex;
    procedure BeginFencing(X: Integer; Y: Integer);
    function  BeginLinking(const X: Integer; const Y: Integer): Boolean;
    procedure BeginPanning(X: Integer; Y: Integer);
    procedure BeginSelecting(X: Integer; Y: Integer);
    procedure BeginVertexing;
    procedure BeginZooming(X: Integer; Y: Integer);
    procedure CopyToBitmap(const Fname: String);
    procedure CopyToString(const Fname: String);
    procedure DeleteVertex;
    procedure DisplayFlyOver(const X,Y: Integer);
    procedure DrawDotLink(const P1: TPoint; const P2: TPoint);
    procedure DrawDotRect(aRect: TRect);
    procedure DrawFenceline;
    procedure DrawToMetafile(aMetafile: TMetafile);
    function  DrawVertices(DrawFlag: Boolean): TRect;
    procedure DrawZoomRect;
    procedure EndFencing;
    procedure EndLinking(const X: Integer; const Y: Integer);
    procedure EndPanning;
    procedure EndSelecting;
    procedure EndVertexing;
    procedure EraseFenceline;
    function  FindLabel(const X: Integer; const Y: Integer): Boolean;
    function  FindLink(const X: Integer; const Y: Integer): Boolean;
    function  FindNode(const X: Integer; const Y: Integer): Boolean;
    function  FindObject(const X: Integer; const Y: Integer): Boolean;
    function  FindVertex(P: TPoint): Boolean;
    function  GetVertexRect(aVertex: PVertex): TRect;
    procedure GoFlyOver(const X,Y: Integer);
    procedure GoLabeling(X: Integer; Y: Integer);
    procedure GoLinking(X: Integer; Y: Integer);
    procedure GoPanning(X: Integer; Y: Integer);
    procedure GoVertexing(X: Integer; Y: Integer);
    procedure GoZoomIn(X: Integer; Y: Integer);
    procedure GoZoomOut(X: Integer; Y: Integer);
    procedure MoveLabel(const Index: Integer);
    procedure MoveLabelRect(const X,Y: Integer);
    procedure MoveVertex(const X,Y: Extended);
    procedure PlacePanel(aPanel: TPanel; X, Y, W, H: Integer);
    procedure UpdateZoomFactor;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;

  public
    { Public declarations }
    Map         : TMap;                   //Network map object
    CurrentTool : Integer;                //Current map drawing tool
    Hilited     : Boolean;                //True if an object is highlighted
    HiliteRect  : TRect;                  //Highlighted rectangle
    HiliteBitmap: TBitmap;                //Bitmap storing highlighted rectangle
    Fenceline   : array[1..MAXVERTICES] of TPoint; //Fenceline points
    Linking     : Boolean;                //True if user is drawing a link
    NumFencePts : Integer;                //Number of fenceline points
    RedrawOnResize: Boolean;              //True if map redrawn when form resized
    procedure BeginAligning(Sender: TObject);
    procedure ChangeHiliteObject(const ObjType, Index: Integer);
    procedure ClearMap;
    procedure CopyTo;
    procedure CopyToMetafile(const Fname: String);
    procedure DrawLinkLegend;
    procedure DrawNodeLegend;
    procedure DrawObject(const ObjType: Integer; const Index: Integer);
    procedure EraseLabel(const Index: Integer);
    procedure EraseObject(const ObjType: Integer; const Index: Integer);
    procedure HiliteOff;
    procedure HiliteOn;
    procedure InvalidateMap(var aRect: TRect);
    procedure ModifyMapDimensions;
    procedure ModifyLinkLegend;
    procedure ModifyNodeLegend;
    procedure MoveNode(const ObjType, ObjIndex: Integer; const X, Y: Extended);
    procedure OpenMapBackdrop;
    procedure Print(Destination: TDestination);
    procedure RedrawMap;
    procedure ReplaceLabel(const Index: Integer; const S: String);
    procedure SelectAll;
    procedure SetPanelPos;
    procedure SetMapOptions;
    procedure ToggleLinkLegend;
    procedure ToggleNodeLegend;
    procedure ToggleTimeLegend;
    procedure ToolButtonClick(ButtonTag: Integer);
  end;

var
  MapForm: TMapForm;

implementation

{$R *.DFM}

uses Dcopy, Dlabel, Dmap, Dmapdim, Fmain, Fbrowser, Fovmap, Fproped,
     Uinput, Uoutput;

var
  FlyOverX  : Integer;
  FlyOverY  : Integer;
  AnchorX   : Integer;
  AnchorY   : Integer;
  DeltaX    : Extended;
  DeltaY    : Extended;
  ScrollRect: TRect;


//================================================================
//             Form Creation, Resizing and Destruction
//================================================================

procedure TMapForm.FormCreate(Sender: TObject);
//-------------------------------------------------
// Form's OnCreate event handler.
//-------------------------------------------------
var
  y: Integer;
begin
    Left := 0;
    Top := 0;
    Width := MainForm.ClientWidth - BrowserForm.Width - 4;
    Height := MainForm.ClientHeight - MainForm.ControlBar1.Height -
              MainForm.StatusPanel.Height - 4;


//Create a map object
  Map := TMap.Create;
  Map.Dimensions := MapDimensions;
  RedrawOnResize := False;

//Set background and foreground colors
  Color := MapBackColor[Map.Options.ColorIndex];
  Canvas.Pen.Color := MapForeColor[Map.Options.ColorIndex];
  MapOptionsPage := 0;

//Initialize object selector & highlighting variables
  HiliteObject := -1;
  Hilited := False;
  HiliteRect := Rect(0,0,0,0);
  NodeHiliteSize := MaxIntValue([PIXTOL,Map.Options.NodeSize+1]);
  LinkHiliteSize := MaxIntValue([PIXTOL,Map.Options.LinkSize+1]);
  HiliteBitmap := TBitmap.Create;
  Timer1.Enabled := False;
  FlyOverObject := -1;
  FlyOverIndex  := -1;

//Size the node & link legends
  with NodeLegendBox.Canvas do
  begin
    y := TextHeight('[');
    y := (MAXINTERVALS + 2) * ((3*y) div 2) + y;
  end;
  NodeLegendPanel.Height := y;
  LinkLegendPanel.Height := y;

//Initialize legend frames
  with NodeLegendFrame do
  begin
    X := 0;
    Y := 0;
    Framed := True;
  end;
  with LinkLegendFrame do
  begin
    X := 0;
    Y := MISSING;
    Framed := True;
  end;
  with TimeLegendFrame do
  begin
    X := 1;
    Y := 0;
  end;

//Make legends visible, hide hint panel
  NodeLegendPanel.Visible := True;
  LinkLegendPanel.Visible := True;
  TimeLegendPanel.Visible := True;
  HintPanel.Visible := False;

//Disable replacement of '&' with '_' in flyover labels
  HintLabel.ShowAccelChar := False;  

//Set checked status of legend menu items
  PopupNodeLegend.Checked := NodeLegendPanel.Visible;
  PopupLinkLegend.Checked := LinkLegendPanel.Visible;
  PopupTimeLegend.Checked := TimeLegendPanel.Visible;
  MainForm.MnuNodeLegend.Checked := PopupNodeLegend.Checked;
  MainForm.MnuLinkLegend.Checked := PopupLinkLegend.Checked;
  MainForm.MnuTimeLegend.Checked := PopupTimeLegend.Checked;
end;


procedure TMapForm.FormClose(Sender: TObject; var Action: TCloseAction);
//-----------------------------------------------------
// OnClose handler for MapForm -
// minimizes or restores window when close icon clicked.
//------------------------------------------------------
begin
  if (WindowState = wsMinimized) then
  begin
    Action := caNone;
  end
  else Action := caMinimize;
end;


procedure TMapForm.FormDestroy(Sender: TObject);
//----------------------------------------------
// Frees Map object when form is destroyed.
//----------------------------------------------
begin
  Map.Free;
  HiliteBitmap.Free;
end;


procedure TMapForm.FormActivate(Sender: TObject);
//-----------------------------------------------
// OnActivate event handler.
// Enables Options toolbar button on MainForm.
//-----------------------------------------------
begin
  MainForm.TBOptions.Enabled := True;
end;


procedure TMapForm.FormResize(Sender: TObject);
//--------------------------------------------
// OnResize event handler.
//--------------------------------------------
begin
  if Assigned(Map) then
  try
    Map.Resize(Rect(0,0,ClientWidth,ClientHeight));
    if RedrawOnResize then
    begin
      Map.Rescale(MapDimensions);
      if Visible then SetPanelPos;
      Map.RedrawBackdrop;
      RedrawMap;
      OVMapForm.Rescale;
    end;
  finally
  end;
end;


procedure TMapForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
//----------------------------------------------------------
// Message handler for WM_GETMINMAXINFO that restricts
// smallest size that user can re-size form to.
//----------------------------------------------------------
var
  MinMax: PMinMaxInfo;
begin
  inherited;
  MinMax := Msg.MinMaxInfo;
  MinMax^.ptMinTrackSize.X := MINMAPSIZE;
  MinMax^.ptMinTrackSize.Y := MINMAPSIZE;
end;


procedure TMapForm.SetPanelPos;
// ---------------------------------------------------------------------
// Re-locates legends and Time Panel on MapForm after its been re-sized.
//----------------------------------------------------------------------
begin
  if LinkLegendFrame.Y = MISSING then
    LinkLegendFrame.Y := (NodeLegendPanel.Height+1)/MapForm.ClientHeight;
  with NodeLegendPanel do
    PlacePanel(NodeLegendPanel, Round(NodeLegendFrame.X*MapForm.ClientWidth),
               Round(NodeLegendFrame.Y*MapForm.ClientHeight), Width, Height);
  with LinkLegendPanel do
    PlacePanel(LinkLegendPanel, Round(LinkLegendFrame.X*MapForm.ClientWidth),
               Round(LinkLegendFrame.Y*MapForm.ClientHeight), Width, Height);
  with TimeLegendPanel do
    PlacePanel(TimeLegendPanel, Round(TimeLegendFrame.X*MapForm.ClientWidth-Width),
               Round(TimeLegendFrame.Y*MapForm.ClientHeight), Width, Height);
end;


procedure TMapForm.PlacePanel(aPanel: TPanel; X, Y, W, H: Integer);
//-------------------------------------------------------------------
// Positions aPanel so that it falls within bounds of MapForm window.
//-------------------------------------------------------------------
begin
  if X < 0 then X := 0;
  if X + W > ClientWidth then X := ClientWidth - W;
  if Y < 0 then Y := 0;
  if Y + H > ClientHeight then Y := ClientHeight - H;
  aPanel.SetBounds(X,Y,W,H);
end;


//================================================================
//                     Map Drawing Procedures
//================================================================

procedure TMapForm.FormPaint(Sender: TObject);
//----------------------------------------------------
// OnPaint event handler. Copies bitmap image of Map
// object to form's canvas when form needs repainting.
//----------------------------------------------------
begin
  Canvas.Draw(0,0,Map.Bitmap);
  DrawFenceline;
  HiliteOn;
end;


procedure TMapForm.InvalidateMap(var aRect: TRect);
//-----------------------------------------------------
// Redraws portion of map contained in rectangle aRect.
//-----------------------------------------------------
var
  FullRect: TRect;
begin
  FullRect := Map.Window.MapRect;  //Save map window's rectangle
  OffsetRect(aRect, -10, -10);
  InflateRect(aRect,20,20);        //Inflate aRect a little
  Map.Window.MapRect := aRect;     //Replace map window with aRect
  Map.DrawMap;                     //Draw the map into aRect & copy
  Canvas.CopyRect(aRect,Map.Bitmap.Canvas,aRect);  //to form's canvas
  Map.Window.MapRect := FullRect;  //Restore map's original rectangle
  OVMapForm.NeedsUpdating := True; //Mark Overview Map for updating
end;


procedure TMapForm.RedrawMap;
//-------------------------------
// Redraws map at current extent.
//-------------------------------
begin
  if  (Assigned(BrowserForm))            //Display hourglass if
  and (not BrowserForm.VCRTimer.Enabled) //  not animating map
  then Screen.Cursor := crHourglass;
  UpdateZoomFactor;
  HiliteOff;
  Map.DrawMap;
  Canvas.Draw(0,0,Map.Bitmap);
  DrawFenceline;
  HiliteOn;
  Screen.Cursor := crDefault;
end;


procedure TMapForm.ClearMap;
//-------------------------
// Clears contents of map.
//-------------------------
begin
  Fencing := False;
  Vertexing := False;
  Linking := False;
  HiliteOff;
  HiliteObject := -1;
  SelectedLink := nil;
  SelectedVertex := nil;
  EraseFenceline;
  MapBackdrop := DefMapBackdrop;
  MapDimensions := DefMapDimensions;
  Map.Resize(ClientRect);
  Map.Rescale(MapDimensions);
  PopupBackdrop.Checked := False;
  PopupBackdrop.Enabled := False;
  RedrawMap;
  DrawNodeLegend;
  DrawLinkLegend;
  OVMapForm.Rescale;
  OVMapForm.Hide;
end;


procedure TMapForm.SelectAll;
//-----------------------------------
// Draws fenceline around entire map.
//-----------------------------------
begin
  DrawFenceline;
  NumFencePts := 5;
  Fenceline[1] := Point(1,1);
  Fenceline[2] := Point(1,ClientHeight-1);
  Fenceline[3] := Point(ClientWidth-1,ClientHeight-1);
  Fenceline[4] := Point(ClientWidth-1,0);
  Fenceline[5] := Point(1,1);
  DrawFenceline;
  MainForm.SelectorButtonClick;
end;


procedure TMapForm.DrawObject(const ObjType: Integer; const Index: Integer);
//---------------------------------------------------------------
// Draws an object on the map by determining the area it occupies
// and invalidating this area.
//---------------------------------------------------------------
var
  aRect: TRect;
begin
  HiliteOff;
  aRect := Map.GetRect(ObjType,Index);
  InvalidateMap(aRect);
  HiliteOn;
end;


procedure TMapForm.DrawDotLink(const P1: TPoint; const P2: TPoint);
//--------------------------------------------
// Draws dotted line between points P1 and P2.
//--------------------------------------------
begin
  with Canvas do
  begin
    Pen.Mode := pmXor;   {pmNotXor;}
    Pen.Style := psDot;
    MoveTo(P1.X,P1.Y);
    LineTo(P2.X,P2.Y);
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
  end;
end;


procedure TMapForm.DrawDotRect(aRect: TRect);
//--------------------------------------------
// Draws dotted outline of rectangle aRect
//--------------------------------------------
begin
  with Canvas do
  begin
    Pen.Mode := pmXor;   {pmNotXor;}
    Pen.Style := psDot;
    with aRect do
    begin
      PolyLine([Point(Left,Top), Point(Right,Top), Point(Right,Bottom),
        Point(Left,Bottom), Point(Left,Top)]);
    end;
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
  end;
end;


procedure TMapForm.DrawFenceline;
//---------------------------------------------------
// Draws a fence line around selected portion of map.
//---------------------------------------------------
var
  i: Integer;
begin
  if NumFencePts >= 2 then
    for i := 1 to NumFencePts-1 do
      DrawDotLink(Fenceline[i], Fenceline[i+1]);
end;


procedure TMapForm.EraseFenceline;
//---------------------------------
// Erases a fenceline.
//----------------------------------
begin
  DrawFenceline;
  NumFencePts := 0;
  Fencing := False;
end;


procedure TMapForm.ModifyMapDimensions;
//--------------------------------------------------------
// Invokes Map Dimensions dialog to modify map dimensions.
//--------------------------------------------------------
begin
//Create map dimensions dialog form
  MainForm.SelectorButtonClick;
  with TMapDimensionsForm.Create(self) do
  try

  //Load current dimensions into form
    LoadDimensions;

  //Retrieve new extents from form
    if ShowModal = mrOK then
    begin
      UnloadDimensions;
      HasChanged := True;

    //Re-scale & redraw map
      Map.Rescale(MapDimensions);   //Re-dimension
      Map.RedrawBackdrop;           //Redraw backdrop
      RedrawMap;                    //Redraw map
      OVMapForm.Rescale;            //Update overview map
    end;
  finally
    Free;
  end;
end;


//==================================================================
//                    Backdrop Image Procedures
//==================================================================

procedure TMapForm.OpenMapBackdrop;
//---------------------------------
// Opens a map backdrop image file.
//---------------------------------
var
  Opened: Boolean;
begin
  if Length(MapBackdrop.Filename) = 0 then Exit;
  Opened := False;
  if not FileExists(MapBackdrop.Filename) then Uutils.MsgDlg(
    MSG_NO_FIND_BACKDROP + MapBackdrop.Filename, mtError, [mbOK], MainForm)
  else if not Map.RedrawBackdrop then Uutils.MsgDlg(
    MSG_NO_READ_BACKDROP + MapBackdrop.Filename, mtError, [mbOK], MainForm)
  else Opened := True;
  if not Opened then
  begin
    MapBackdrop := DefMapBackdrop;
    HasChanged := True;
  end
  else MapBackdrop.Visible := True;
end;


//==================================================================
//                  User Interaction Procedures
//==================================================================

procedure TMapForm.ToolButtonClick(ButtonTag: Integer);
//----------------------------------------------------
// Processes OnClick events for toolbuttons on Main
// Form's Map Toolbar panel. The ButtonTag parameter
// determines which toolbutton was selected. See notes
// in Fmain unit's ToolButton1Click procedure for
// listing of which Tags go with which buttons.
//----------------------------------------------------
begin
//Determine which button was pressed
  CurrentTool := ButtonTag;

//End vertex editing if Vertex Selection tool not selected
  if (CurrentTool <> VERTEXSELECT) and Vertexing then
  begin
    EndVertexing;
    SelectedLink := nil;
  end;

//Turn off Hiliting if a map object tool was selected
  if CurrentTool in [JUNCS..VALVES, LABELS, GROUPSELECT] then HiliteOff;

//Turn off all action flags
  if Fencing or Linking then EraseFenceline;
  Linking := False;
  Moving := False;
  Aligning := False;
  Panning := False;
  Fencing := False;
  Zooming := False;

//Change cursor shape depending on tool button pressed.
//and update current selected object in database, if applicable.
  case CurrentTool of

  JUNCS..TANKS:
    // Restore junction display if it was turned off
    begin
      if (CurrentTool = JUNCS) and not (Map.Options.DispJuncs) then
      begin
        Uutils.MsgDlg(TXT_RESTORE_JUNCS, mtInformation, [mbOK], MainForm);
        Map.Options.DispJuncs := True;
        RedrawMap;
      end;
      Cursor := crXHAIR;
    end;

  PIPES..VALVES:
    Cursor := -3;  {crCROSS}

  LABELS:
    // Restore label display if it was turned off
    begin
      if not Map.Options.DispLabels
        or (MapZoomRatio < Map.Options.LabelZoom) then
      begin
        Uutils.MsgDlg(TXT_RESTORE_LABELS, mtInformation, [mbOK], MainForm);
        Map.Options.DispLabels := True;
        Map.Options.LabelZoom := MapZoomRatio;
        RedrawMap;
      end;
      Cursor := -4;  {crlBEAM}
    end;

  SELECT:
    Cursor := -2;  {crARROW}

  GROUPSELECT:
    begin
      EraseFenceline;
      Cursor := -3;
    end;

  PAN:
    Cursor := crMOVE;

  ZOOMIN:
    Cursor := crZOOMIN;

  ZOOMOUT:
    Cursor := crZOOMOUT;

  VERTEXSELECT:
    begin
      BeginVertexing;
      Cursor := crARROWTIP;
    end;
  end;

// Make Hint Panel invisible if Selection Tool not selected
  if CurrentTool <> SELECT then HintPanel.Visible := False;
end;


procedure TMapForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------
// OnMouseDown event handler.
//-----------------------------------------------------
var
  aPoint: TPoint;

begin
// Left button is used for object selection, panning & zooming
  if Button = mbLeft then
  begin

  // Erase fenceline if not Fencing
    if not Fencing and not Linking then EraseFenceline;

  // Begin executing the called-for action
    case CurrentTool of
      SELECT:        BeginSelecting(X,Y);
      GROUPSELECT:   BeginFencing(X,Y);
      VERTEXSELECT:  GoVertexing(X,Y);
      PAN:           BeginPanning(X,Y);
      ZOOMIN:        BeginZooming(X,Y);
    end;
  end

// Right button either cancels a linking operation,
// completes a fencelining operation,
// or activates one of the popup menus
  else if Button = mbRight then
  begin
    aPoint := ClientToScreen(Point(X,Y));

  // If linking, then cancel
    if Linking then EndLinking(-1,-1)

  // If drawing a fenceline, then complete the operation
    else if Fencing then EndFencing

  // If vertex editing, then display vertex popup menu
    else if Vertexing then PopupMenu3.Popup(aPoint.X,aPoint.Y)

  // If object was selected, then display object's popup menu
    else if (CurrentTool = SELECT) and FindObject(X,Y) then
    begin
      BrowserForm.UpdateBrowser(SelectedObject,SelectedIndex);
      PopupMenu1.Popup(aPoint.X,aPoint.Y);
    end

  // Otherwise invoke the map's popup menu.
    else
    begin
      PopupMenu2.Popup(aPoint.X,aPoint.Y);
    end;
  end;
end;


procedure TMapForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//-------------------------------------------------------
// OnMouseMove event handler.
//-------------------------------------------------------
begin
// Display real world map coordinates in Mainform's status panel
  if not (PtInRect(Map.Window.MapRect,Point(X,Y))) then Exit;
  FlyOverX := X;
  FlyOverY := Y;
  CurrentX := Map.GetX(X);
  CurrentY := Map.GetY(Y);
  MainForm.StatusBarPanel4.Caption :=
    '  X,Y: ' + FloatToStrF(CurrentX,ffFixed,18,MapDimensions.Digits) +
    ', ' + FloatToStrF(CurrentY,ffFixed,18,MapDimensions.Digits);

// Draw dotted line if fencelining or linking is in progress
  if Fencing or Linking then
  begin
    DrawDotLink(Fenceline[NumFencePts-1],Fenceline[NumFencePts]);
    Fenceline[NumFencePts] := Point(X,Y);
    DrawDotLink(Fenceline[NumFencePts-1],Fenceline[NumFencePts]);
  end

// Scroll map if panning in progress
  else if Panning = True then GoPanning(X,Y)

// Draw dotted rectangle if zooming is in progress
  else if Zooming = True then
  begin
    DrawZoomRect;
    ZoomRect.BottomRight := Point(X,Y);
    DrawZoomRect;
  end

// Move selected object if a move is in progress
// (Add slight delay between time object first selected
// and when it can be moved)
  else if (Moving = True) and (GetTickCount - OldTickCount > TICKDELAY)
  and (ssLeft in Shift) then
  begin
    OldTickCount := 0;
    HiliteOff;
    if Vertexing then MoveVertex(CurrentX,CurrentY)
    else if HiliteObject = LABELS then MoveLabelRect(X,Y)
    else if HiliteObject in [JUNCS..TANKS] then
      MoveNode(HiliteObject,HiliteIndex,CurrentX,CurrentY);
  end;

end;


procedure TMapForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-------------------------------------------------------
// OnMouseUp event handler.
// Completes the operation begun by the MouseDown action.
//-------------------------------------------------------
begin
  if Button = mbLeft then
  begin
    case CurrentTool of
    JUNCS..TANKS:  Uinput.AddNode(CurrentTool,CurrentX,CurrentY);
    LABELS:        GoLabeling(X,Y);
    PIPES..VALVES: GoLinking(X,Y);
    SELECT,
    VERTEXSELECT:  EndSelecting;
    PAN:           EndPanning;
    ZOOMIN:        GoZoomIn(X,Y);
    ZOOMOUT:       GoZoomOut(X,Y);
    end;
  end;
end;


procedure TMapForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
//-----------------------------------------------------
// OnMouseWheel event handler.
// Implements Zoom In/Out on the map.
//-----------------------------------------------------
var
  Pos: TPoint;
begin
  Pos := ScreenToClient(MousePos);
  if WheelDelta > 0 then
  begin
    BeginZooming(Pos.X, Pos.Y);
    GoZoomIn(Pos.X, Pos.Y);
  end
  else GoZoomOut(Pos.X, Pos.Y);
  Handled := true;
end;


procedure TMapForm.FormDblClick(Sender: TObject);
//-----------------------------------------------------
// OnDoubleClick event handler.
// Invokes property editor if a map object is selected.
//-----------------------------------------------------
begin
  if (CurrentTool = SELECT)
  and (HiliteObject in [JUNCS..VALVES,LABELS])
  then BrowserForm.BtnEditClick(Sender);
end;


procedure TMapForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//-----------------------------------------------------
// OnKeyDown event handler.
// Used to complete or cancel a fencelining operation,
// to delete a selected object, or pass keystroke to
// Property Editor.
//-----------------------------------------------------
begin
  if CurrentTool = VERTEXSELECT then
  begin
    if (Key = VK_DELETE) then DeleteVertex;
    if (Key = VK_INSERT) then AddVertex;
  end

  else if (Key = VK_RETURN) and Fencing then EndFencing

  else if (Key = VK_ESCAPE) then
  begin
    if Fencing then EraseFenceline;
    if Linking then EndLinking(-1,-1);
  end

  else if (Key = VK_DELETE) then
  begin
    if ((HiliteObject in [JUNCS..VALVES,LABELS])
    or (NumFencePts > 0))
    then BrowserForm.BtnDeleteClick(Sender)
  end

  else if (Key = VK_F1) then
  begin
    HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 108);
  end

  else with PropEditForm do
    if Visible then
    begin
      SendMessage(Editor.Handle,WM_KEYDOWN,Key,0);
      Editor.Edit;
    end;
end;


procedure TMapForm.Timer2Timer(Sender: TObject);
//----------------------------------------------
// OnTimer handler for Timer2.
// Displays flyover hint feature.
//----------------------------------------------
begin
  if FlyOvers and (CurrentTool = SELECT) then GoFlyOver(FlyOverX,FlyOverY);
end;


procedure TMapForm.GoFlyover(const X,Y: Integer);
//----------------------------------------------------
// Implements the flyover hint feature. This
// displays an object's ID and view variable value
// when the mouse rests over the object on the map.
//----------------------------------------------------
begin
// Feature only applies when user is selecting an object
  if (Moving) then Exit;
  if (CurrentTool = SELECT) then
  begin

  // Check if mouse X,Y is over a network object
    if (FindObject(X,Y) and (SelectedObject in [JUNCS..VALVES])) then
    begin

    // Check if a new object selected
      if (SelectedObject <> FlyOverObject) or
         (SelectedIndex <> FlyOverIndex) then
      begin

      // Save new flyover object and index
        FlyOverObject := SelectedObject;
        FlyOverIndex := SelectedIndex;

      // Display flyover hint
        DisplayFlyOver(X,Y);
      end;
    end

  // Otherwise hide the flyover Hint Panel
    else
    begin
      HintPanel.Visible := False;
      FlyOverObject := -1;
      FlyOverIndex := -1;
    end;
  end;
end;


procedure TMapForm.DisplayFlyOver(const X,Y: Integer);
//---------------------------------------------------
// Displays flyover hint label on map.
//---------------------------------------------------
var
  s1,s2  : String;
  L,T: Integer;
  w,h: Integer;

begin
// Use the GetMeterLabel procedure to obtain text of object's ID & value.
  Uoutput.GetMeterLabel(FlyOverObject,FlyOverIndex,s1,s2);

// Build the hint panel's caption and determine its width & height
  Hint := s1;
  w := Canvas.TextWidth(Hint);
  h := Canvas.TextHeight(Hint);
  if (Length(s2) > 0) then
  begin
    s2 := ' ' + s2 + ' ';
    w := MaxIntValue([w,Canvas.TextWidth(s2)]);
    h := h + Canvas.TextHeight(s2);
    Hint := Hint + #10 + s2;
  end;

// Position hint panel so it does not fall outside of map window & display it.
  HintLabel.Caption := Hint;
  Inc(w,2);
  Inc(h,2);
  L := X;
  T := Y + 20;
  PlacePanel(HintPanel,L,T,w,h);
  HintPanel.Visible := True;
end;


procedure TMapForm.GoLabeling(X: Integer; Y: Integer);
//--------------------------------------------------------------
// Displays Edit box on map to obtain user entry of a map label.
//--------------------------------------------------------------
var
  s: String;
  p: TPoint;

begin
// Create a borderless LabelForm that contains an Edit control
  s := '';
  p := Point(X,Y);
  with TLabelForm.Create(self) do
  try

  // Position the Edit control at the current mouse location
    Top := MapForm.ClientToScreen(p).Y;
    Left := MapForm.ClientToScreen(p).X;

  // Retrieve the text from the Edit control
    if ShowModal = mrOK then s := Edit1.Text;
  finally
    Free;
  end;

// Add a new label to the map
  if Length(s) > 0 then
  begin
    Repaint;
    Uinput.AddLabel(CurrentX, CurrentY, s);
  end;
end;


procedure TMapForm.GoLinking(X: Integer; Y: Integer);
//--------------------------------------------------
// Begins/ends linking of one node to another when a
// new link (pipe, pump, or valve) is being created.
//
// Uses API call SetCapture to have Map Window
// capture all mouse messages while linking.
//--------------------------------------------------
begin
// If linking already begun, then end it
  if Linking = True then
  begin
    EndLinking(X,Y);
  end

// Otherwise begin linking and capture mouse
  else
  begin
    Linking := BeginLinking(X,Y);
  end;
end;


function TMapForm.BeginLinking(const X: Integer; const Y: Integer): Boolean;
//------------------------------------------------------
// Processes the mouse click that begins drawing a link.
//------------------------------------------------------
begin
// Locate the node clicked on
  Result := False;
  if not FindNode(X,Y) then Exit;

// Highlight & save info about this node
  ChangeHiliteObject(SelectedObject, SelectedIndex);
  LinkInfo.Node1 := Node(SelectedObject,SelectedIndex);
  LinkInfo.Point1 := Map.GetNodePoint(SelectedObject,SelectedIndex);
  LinkInfo.Point2 := LinkInfo.Point1;

// Create the fenceline used to draw the link
  Fenceline[1] := LinkInfo.Point1;
  NumFencePts := 1;
  Inc(NumFencePts);
  Fenceline[NumFencePts] := Point(X,Y);

// Switch to Pencil cursor
  Cursor := crPENCIL;
  Result := True;
end;


procedure TMapForm.EndLinking(const X: Integer; const Y: Integer);
//------------------------------------------------
// Processes mouse click that ends drawing a link.
//------------------------------------------------
var
  N: Integer;
begin
// Cancel linking if called for
  if (X < 0) and (Y < 0) then Linking := False

// Else check if a node was clicked on
  else
  begin

  // Node not clicked on, so add to fenceline
    if not FindNode(X,Y) then
    begin
      if NumFencePts < MAXVERTICES then
      begin
        Inc(NumFencePts);
        Fenceline[NumFencePts] := Point(X,Y);
      end;
      Exit;
    end

  // Node clicked on, so save it to LinkInfo
    else LinkInfo.Node2 := Node(SelectedObject,SelectedIndex);
  end;

// Erase fenceline & highlighting and release mouse capture
  N := NumFencePts;
  EraseFenceLine;
  HiliteOff;
  Cursor := -3;

// Add new link to database and turn off linking
  if Linking and (LinkInfo.Node1 <> LinkInfo.Node2)
    then Uinput.AddLink(CurrentTool,LinkInfo.Node1,LinkInfo.Node2,
      FenceLine, N-2);
  Linking := False;
end;


procedure TMapForm.BeginFencing(X: Integer; Y: Integer);
//----------------------------------------------------
// Processes the mouse click that begins or continues
// drawing a fence line on the map.
//-----------------------------------------------------
begin
// If this is first fence point then begin a new fence line
  if not Fencing then
  begin
    Fenceline[1] := Point(X,Y);
    NumFencePts := 1;
    Fencing := True;
  end;

// Add new vertex point to the fence line
  if NumFencePts < MAXVERTICES - 1 then
  begin
    Inc(NumFencePts);
    Fenceline[NumFencePts] := Point(X,Y);
  end;
end;


procedure TMapForm.EndFencing;
//-------------------------------------------------
// Completes the drawing of an enclosed fence line.
//--------------------------------------------------
begin
// Turn off fencelining
  Fencing := False;

// Connect last fence line vertex to first one
  DrawDotLink(Fenceline[NumFencePts],Fenceline[1]);
  Inc(NumFencePts);
  Fenceline[NumFencePts] := Fenceline[1];

// Activate Selection tool on Mainform Toolbar
  MainForm.SelectorButtonClick;
end;


procedure TMapForm.BeginZooming(X: Integer; Y: Integer);
//-----------------------------------------------------
// Processes the MouseDown action that begins a Zoom In.
//-----------------------------------------------------
begin
// Initialize the zoom rectangle that will be drawn by user
  ZoomRect.TopLeft := Point(X,Y);
  ZoomRect.BottomRight := Point(X,Y);

// Prepare form's canvas for drawing the zoom rectangle
  Canvas.Pen.Mode := pmXor;
  Canvas.Pen.Style := psDot;

// Turn Zooming on
  Zooming := True;
end;


procedure TMapForm.DrawZoomRect;
//--------------------------------------
// Draws rubberbanding rectangle around
// the current selected Zoom In area.
//--------------------------------------
begin
  with Canvas, ZoomRect do
  begin
    PolyLine([Point(Left,Top),Point(Right,Top),Point(Right,Bottom)]);
    PolyLine([Point(Left,Top),Point(Left,Bottom),Point(Right,Bottom)]);
  end;
end;


procedure TMapForm.GoZoomIn(X: Integer; Y: Integer);
//--------------------------------------
// Implements the Zoom In on the Zoom
// rectangle which ends at location X,Y.
//--------------------------------------
var
  xOff, yOff: Extended;
  xSF, ySF  : Extended;
  temp      : Integer;

begin
// Erase zoom rectangle from map
  Zooming := False;
  DrawZoomRect;
  Canvas.Pen.Mode := pmCopy;
  Canvas.Pen.Style := psSolid;

  with Map.Window do
  begin
  // Exit if can't zoom in anymore
    if ZoomIndex = High(ZoomFactor) then Exit;

  {*** Updated 12/29/00 ***}
  // Make sure zoom rectangle is oriented correctly
    with ZoomRect do
    begin
      if Left > Right then
      begin
        temp := Left;
        Left := Right;
        Right := temp;
      end;
      if Top > Bottom then
      begin
        temp := Bottom;
        Bottom := Top;
        Top := temp;
      end;
    end;

  // If user simply clicked mouse without drawing a
  // a zoom rectangle, then create a zoom rectangle
  // which equals 1/2 of view window centered at X,Y.
    if IsRectEmpty(ZoomRect) = True then
      ZoomRect := Bounds(X-(Pwidth div 4), Y-(Pheight div 4),
        (Pwidth div 2), (Pheight div 2));

  // Get world coords. of lower left of zoom rectangle
    xOff := (ZoomRect.Left - Poffset.X)/PPW + Woffset.X;
    yOff := (Poffset.Y - ZoomRect.Bottom)/PPW + Woffset.Y;

  // Get minimum scale factor for zoom rectangle
    xSF := Pwidth/(ZoomRect.Right - ZoomRect.Left);
    ySF := Pheight/(ZoomRect.Bottom - ZoomRect.Top);
    if ySF < xSF then PPW := ySF*PPW
    else PPW := xSF*PPW;

  // Save scale factor and adjust map window offsets
    Inc(ZoomIndex);
    ZoomFactor[ZoomIndex] := PPW;
    Woffset.X := xOff;
    Woffset.Y := yOff;

  // Redraw map & display map extent on overview map
    Map.RedrawBackdrop;
    RedrawMap;
    OVMapForm.ShowMapExtent;
  end;
end;


procedure TMapForm.GoZoomOut(X: Integer; Y: Integer);
//----------------------------------
// Implements a Zoom Out on the map.
//----------------------------------
begin
  with Map.Window do
  begin
  // Exit if can't zoom out any more
    if ZoomIndex = 0 then exit;

  // Restore previous zoom factor
    Dec(ZoomIndex);
    PPW := ZoomFactor[ZoomIndex];

  // Adjust window offsets
    Woffset.X := CurrentX - (Pwidth div 2)/PPW;
    Woffset.Y := CurrentY - (Pheight div 2)/PPW;

  // Redraw map & update overview map
    Map.RedrawBackdrop;
    RedrawMap;
    OVMapForm.ShowMapExtent;
  end;
end;


procedure TMapForm.UpdateZoomFactor;
//-------------------------------------------------
// Updates global MapZoomRatio variable and checks
// if further zooming should be allowed.
//-------------------------------------------------
var
  zoominflag: Boolean;
  zoomoutflag: Boolean;

begin
  with Map.Window do
  begin

  // Determine if further zooming allowed
     zoominflag  := not (ZoomIndex = High(ZoomFactor));
     zoomoutflag := not (ZoomIndex = Low(ZoomFactor));

  // Determine current zoom ratio
    if 100*ZoomFactor[ZoomIndex] > MAXZOOMRATIO*ZoomFactor[0] then
    begin
       ZoomFactor[ZoomIndex] := MAXZOOMRATIO*ZoomFactor[0];
       zoominflag := False;
    end;
    MapZoomRatio := Round(100*ZoomFactor[ZoomIndex]/ZoomFactor[0]);
  end;

  with MainForm do
  begin

  // Adjust status of menus & toolbar buttons on Main form
    MnuZoomIn.Enabled := zoominflag;
    MnuZoomOut.Enabled := zoomoutflag;
    ToolButton4.Enabled := zoominflag;
    ToolButton5.Enabled := zoomoutflag;

  // Switch to Selection tool if cannot zoom anymore
    if ((CurrentTool = ZOOMIN) and (zoominflag = FALSE))
    or ((CurrentTool = ZOOMOUT) and (zoomoutflag = FALSE))
    then SelectorButtonClick;

  // Update display of zoom ratio on status panel
    StatusBarPanel2.Caption := IntToStr(MapZoomRatio) + '%';
  end;
end;


procedure TMapForm.BeginPanning(X: Integer; Y: Integer);
//-----------------------------------------------------------
// Processes the MouseDown action that begins panning of map.
//-----------------------------------------------------------
begin
// Change cursor shape (by forcing ButtonUp message)
  SendMessage(Handle,WM_LBUTTONUP,0,0);
  Cursor := crFIST;
  HiliteOff;

// Set panning flag & save anchor coordinates & map window rectangle
  Panning := True;
  AnchorX := X;
  AnchorY := Y;
  ScrollRect := Map.Window.MapRect;
end;


procedure TMapForm.GoPanning(X: Integer; Y: Integer);
//----------------------------------------------
// Processes MouseMove action that pans the map.
//----------------------------------------------
var
  xScroll, yScroll: Integer;

begin
  if Panning = True then
  begin

  // Determine scrolling distance
    xScroll := X - AnchorX;
    yScroll := Y - AnchorY;

  // Save current position
    AnchorX := X;
    AnchorY := Y;

  // Update world coordinate offsets of map
    with Map.Window do
    begin
      Woffset.X := Woffset.X - xScroll/PPW;
      Woffset.Y := Woffset.Y + yScroll/PPW;
    end;

  // Display map's outline on screen
    with Map do
    begin
      ClearMap;
      if MapBackdrop.Visible then
      begin
        if not Aligning then OffsetRect(ScrollRect, xScroll, yScroll);
        Bitmap.Canvas.CopyRect(ScrollRect,BackBM.Canvas,Window.MapRect);
      end;
      DrawOutline(Options.LinkSize,MapGrayColor[Map.Options.ColorIndex]);
    end;
    Canvas.Draw(0,0,Map.Bitmap);
  end;
end;


procedure TMapForm.EndPanning;
//--------------------------------------------------------
// Processes MouseUp action that completes panning of map.
//--------------------------------------------------------
begin
  if Panning = True then
  begin

  // If aligning map's backdrop image, then adjust backdrop offset.
  // New offset is old offset + change in the network offset.
  // (use - change for Y because Y-offset is measured
  //  relative to upper right corner of map.)
    if Aligning then with MapBackdrop, Map.Window do
    begin
      Offset.X := Offset.X + (Woffset.X - DeltaX);
      Offset.Y := Offset.Y - (Woffset.Y - DeltaY);
      HasChanged := True;
    end;

  // Redraw map
    Map.RedrawBackdrop;
    RedrawMap;

  // Redraw Overview Map if aligning backdrop,
  // otherwise just update its Map Extent rectangle.
    if Aligning then OVMapForm.Redraw else OVMapForm.ShowMapExtent;

  // If aligning backdrop, then switch to Selection mode (which
  // will turn off Aligning), otherwise restore Panning mouse cursor.
    Panning := False;
    if Aligning then MainForm.SelectorButtonClick else Cursor := crMOVE;
  end;
end;


procedure TMapForm.BeginAligning(Sender: TObject);
//-------------------------------------------------------
// Begins process of aligning backdrop with network map
//-------------------------------------------------------
begin
// Save current offset of Map viewport
  with Map.Window do
  begin
    DeltaX := Woffset.X;
    DeltaY := Woffset.Y;
  end;

// Display backdrop if not visible
  if not MapBackdrop.Visible then PopupBackdropClick(Sender);

// Put map into Panning & Aligning modes
  MainForm.PanButtonClick;
  Aligning := True;
end;


procedure TMapForm.BeginSelecting(X: Integer; Y: Integer);
//-------------------------------------------------------
// Processes MouseDown action when Select tool is active.
//-------------------------------------------------------
begin
// Check if mouse is over a currently selected object
  if (Hilited)
  and InRect(X,Y,HiliteRect)
  and (SelectedObject in [JUNCS..TANKS,LABELS]) then
  begin

  // Hide hint panel and allow object to be moved
    HintPanel.Visible := False;
    Moving := True;
    OldTickCount := GetTickCount;

  // For labels, copy HiliteRect to a moveable ScrollRect
    if SelectedObject = LABELS then
    begin
      ScrollRect := HiliteRect;
      AnchorX := X;
      AnchorY := Y;
    end;
    Exit;
  end;

// See if mouse is over a new object
// (Hiliting will be turned on in UpdateBrowser() procedure)
  if FindObject(X,Y) then
    BrowserForm.UpdateBrowser(SelectedObject,SelectedIndex)

// Otherwise no object selected so cancel any highlighting
  else ChangeHiliteObject(-1,-1);
end;


procedure TMapForm.EndSelecting;
//-----------------------------------------------------
// Processes MouseUp action when Select tool is active.
//-----------------------------------------------------
begin
// Finish up moving a node or label
  if Moving and (not Vertexing) then
  begin
    if HiliteObject = LABELS then MoveLabel(HiliteIndex)
    else
    begin
      if AutoLength then Uinput.UpdatePipeLengths(HiliteObject, HiliteIndex);
      OVMapForm.NeedsUpdating := True;
    end;
    BrowserForm.UpdateBrowser(HiliteObject, HiliteIndex);
  end;
  Moving := False;

// If a pipe's vertices were moved then update its length
  if AutoLength and Vertexing and (HiliteObject = PIPES) then
  begin
    Link(PIPES, HiliteIndex).Data[PIPE_LEN_INDEX] :=
      Uinput.GetPipeLength(HiliteIndex);
    Uinput.UpdateEditor(HiliteObject, HiliteIndex);
  end;
end;


//==================================================================
//                Link Vertex Editing Routines
//==================================================================

procedure TMapForm.BeginVertexing;
//----------------------------------------------
// Places the map into Link Vertex Editing mode
//----------------------------------------------
begin
  if not HiliteObject in [PIPES..VALVES] then exit;
  Vertexing := True;
  SelectedLink := Link(HiliteObject,HiliteIndex);
  SelectedVertex := SelectedLink.Vlist;
  HiliteOff;
  HiliteOn;
end;


procedure TMapForm.GoVertexing(X: Integer; Y: Integer);
//-----------------------------------------------------
// Processes OnMouseDown action for Link vertex editing
//-----------------------------------------------------
begin
// If mouse over a selected vertex, then allow vertex to be moved
  if not Vertexing then Exit;
  if (Hilited) and InRect(X,Y,HiliteRect) then
  begin
    Moving := True;
    OldTickCount := GetTickCount;
  end

// Else if mouse over a new vertex, then select it
  else
  if FindVertex(Point(X,Y)) then Exit

// Else if mouse over another link, then begin editing its vertexes
  else if FindLink(X,Y) then
  begin
    EndVertexing;
    BrowserForm.UpdateBrowser(SelectedObject,SelectedIndex);
    BeginVertexing;
  end;
end;


procedure TMapForm.EndVertexing;
//-----------------------------------------------
// Places the map out of Link Vertex Editing mode
//-----------------------------------------------
begin
  HiliteOff;
  if Vertexing then DrawVertices(False);
  SelectedVertex := nil;
  Vertexing := False;
  HiliteOn;
end;


procedure TMapForm.AddVertex;
//--------------------------------------------
// Adds a new vertex to the link being editied
//--------------------------------------------
var
  aVertex: PVertex;
  Vx, Vy : Extended;
begin
  if SelectedVertex = nil then
  begin
    Vx := (SelectedLink.Node1.X + SelectedLink.Node2.X)/2;
    Vy := (SelectedLink.Node1.Y + SelectedLink.Node2.Y)/2;
  end
  else if SelectedVertex^.Next = nil then
  begin
    Vx := (SelectedVertex^.X + SelectedLink.Node2.X)/2;
    Vy := (SelectedVertex^.Y + SelectedLink.Node2.Y)/2;
  end
  else
  begin
    Vx := (SelectedVertex^.X + (SelectedVertex^.Next)^.X)/2;
    Vy := (SelectedVertex^.Y + (SelectedVertex^.Next)^.Y)/2;
  end;
  New(aVertex);
  aVertex^.X := Vx;
  aVertex^.Y := Vy;
  if SelectedVertex = nil then
  begin
    aVertex^.Next := nil;
    SelectedLink.Vlist := aVertex;
  end
  else
  begin
    aVertex^.Next := SelectedVertex^.Next;
    SelectedVertex^.Next := aVertex;
  end;
  SelectedVertex := aVertex;
  HiliteOff;
  HiliteOn;
  HasChanged := True;
end;


procedure TMapForm.DeleteVertex;
//----------------------------------------------------------
// Deletes currently selected vertex from link being edited
//----------------------------------------------------------
var
  aVertex: PVertex;
  aRect  : TRect;
begin
  if SelectedVertex <> nil then
  begin
    aRect := Map.GetRect(HiliteObject, HiliteIndex);
    aVertex := SelectedLink.Vlist;
    if aVertex = SelectedVertex then aVertex := nil
    else while (aVertex <> nil) do
    begin
      if aVertex^.Next = SelectedVertex then break
      else aVertex := aVertex^.Next;
    end;
    if aVertex = nil then
    begin
      aVertex := SelectedVertex^.Next;
      SelectedLink.Vlist := aVertex;
    end
    else aVertex^.Next := SelectedVertex^.Next;
    Dispose(SelectedVertex);
    SelectedVertex := aVertex;
    HiliteOff;
    InvalidateMap(aRect);
    HiliteOn;
    HasChanged := True;
  end;
end;


function TMapForm.FindVertex(P: TPoint): Boolean;
//------------------------------------------------------
// Finds which vertex of the link being edited contains
// the point P. If a vertex is found, then it becomes
// the currently highlighted vertex.
//------------------------------------------------------
var
  aVertex: PVertex;
  aRect  : TRect;
begin
  aVertex := SelectedLink.Vlist;
  while aVertex <> nil do
  begin
    aRect := GetVertexRect(aVertex);
    if PtInRect(aRect,P) then
    begin
      HiliteOff;
      SelectedVertex := aVertex;
      HiliteOn;
      Result := True;
      Exit;
    end;
    aVertex := aVertex^.Next;
  end;
  Result := False;
end;


function TMapForm.GetVertexRect(aVertex: PVertex): TRect;
//-----------------------------------------------------------
// Determines a bounding rectangle for a specific link vertex
//-----------------------------------------------------------
var
  x, y, size: Integer;
begin
  if aVertex = nil then Result := NORECT
  else
  begin
    size := LinkHiliteSize - 1;
    x := Map.GetXPix(aVertex^.X);
    y := Map.GetYpix(aVertex^.Y);
    Result := Rect(x-size, y-size, x+size, y+size);
  end;
end;


function TMapForm.DrawVertices(DrawFlag: Boolean): TRect;
//---------------------------------------------------------
// Draws or erases the vertexes of the link being edited,
// depending on whether DrawFlag is True or not. Returns
// the bounding rectangle of the currently selected vertex.
//---------------------------------------------------------
var
  R: TRect;
  aVertex: PVertex;
  BrushColor: TColor;
begin
  Result := NORECT;
  if not DrawFlag then
  begin
    R := Map.GetRect(HiliteObject,HiliteIndex);
    InvalidateMap(R);
  end

  else with Canvas do
  begin
    BrushColor := Brush.Color;
    Brush.Color := MapForeColor[Map.Options.ColorIndex];
    aVertex := SelectedLink.Vlist;
    while aVertex <> nil do
    begin
      R := GetVertexRect(aVertex);
      if aVertex = SelectedVertex then Result := R;
      FrameRect(R);
      aVertex := aVertex^.Next;
    end;
    Brush.Color := BrushColor;
  end;
end;


procedure TMapForm.MoveVertex(const X,Y: Extended);
//----------------------------------------------
// Moves the currently selected vertex of a link
// to new world coordinate position X,Y.
//----------------------------------------------
var
  aRect: TRect;
begin
  if SelectedVertex <> nil then
  begin
    HiliteOff;
    aRect := Map.GetRect(HiliteObject,HiliteIndex);
    SelectedVertex^.X := X;
    SelectedVertex^.Y := Y;
    if UnionRect(aRect,aRect,Map.GetRect(HiliteObject,HiliteIndex)) then
      InvalidateMap(aRect);
    HasChanged := True;
    HiliteOn;
  end;
end;


//===================================================================
//                     Map Selection Methods
// These functions determine what map object the mouse pointer is
// currently on by finding values for the following variables:
//    SelectedObject   (e.g., junction, tank, pipe, etc.)
//    SelectedIndex    (index of selected object)
//===================================================================

function TMapForm.FindObject(const X: Integer; const Y: Integer): Boolean;
//---------------------------------------------------------
// Determines which object on map is located at pixels X,Y.
//---------------------------------------------------------
begin
  SelectedObject := -1;
  Result := False;
  if FindNode(X,Y) or FindLabel(X,Y) or FindLink(X,Y)
  then Result := True;
end;


function TMapForm.FindNode(const X: Integer; const Y: Integer): Boolean;
//-------------------------------------------------------
// Determines which node on map is located at pixels X,Y.
//-------------------------------------------------------
var
  i, j, B: Integer;
  P      : TPoint;
  R      : TRect;
begin
  for i := TANKS downto JUNCS do
  begin
    for j := 0 to Network.Lists[i].Count - 1 do
    begin
      B := Map.Options.NodeSize;
      P := Map.GetNodePoint(i,j);
      R := Rect(P.X-B,P.Y-B,P.X+B+1,P.Y+B+1);
      if PtInRect(R,Point(X,Y)) then
      begin
        SelectedObject := i;
        SelectedIndex := j;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;


function TMapForm.FindLabel(const X: Integer; const Y: Integer): Boolean;
//---------------------------------------------------------
// Determines which label on map is located at pixels X,Y.
//---------------------------------------------------------
var
  i: Integer;
  R: TRect;
begin
  for i := 0 to Network.Lists[LABELS].Count - 1 do
  begin
    R := Map.GetRect(LABELS,i);
    if PtInRect(R,Point(X,Y)) then
    begin
      SelectedObject := LABELS;
      SelectedIndex := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;


function TMapForm.FindLink(const X: Integer; const Y: Integer): Boolean;
//-------------------------------------------------------
// Determines which link on map is located at pixels X,Y.
//-------------------------------------------------------
var
  i, j    : Integer;
  p1, p2  : TPoint;
  p3, p   : TPoint;
  aLink   : TLink;
  aVertex : PVertex;
  Hit     : Boolean;
begin
// Check each type of link
  for i := PIPES to VALVES do
  begin
    Hit := False;
    p := Point(X,Y);

  // Check each link of this type
    for j := 0 to Network.Lists[i].Count - 1 do
    begin

    // Get pixel coords. of link's end points (p1 & p2)
      aLink := Link(i,j);
      if  (Map.GetNodePixPos(aLink.Node1,p1))
      and (Map.GetNodePixPos(aLink.Node2,p2)) then
      begin

      // If X,Y close to line between p1 & p2 then select link
        aVertex := aLink.Vlist;
        while aVertex <> nil do
        begin
          p3 := Point(Map.GetXPix(aVertex^.X),Map.GetYpix(aVertex^.Y));
          if Uutils.PtOnLine(p1,p3,p,PIXTOL) then
          begin
            Hit := True;
            break;
          end;
          p1 := p3;
          aVertex := aVertex^.Next;
        end;
        if not Hit then Hit := Uutils.PtOnLine(p1,p2,p,PIXTOL);
        if Hit then
        begin
          SelectedObject := i;
          SelectedIndex := j;
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  Result := False;
end;


//===================================================================
//        Methods That Erase, Move & Highlight Objects
//===================================================================

procedure TMapForm.EraseObject(const ObjType: Integer; const Index: Integer);
//---------------------------------------------------
// Erases object from map and from database.
//---------------------------------------------------
var
  aRect : TRect;
begin
// Object is a node
  if ObjType in [JUNCS..TANKS] then
  begin

  // Delete any reference to node as a label's anchor point or meter node
    Uinput.DeleteLabelAnchor(Node(ObjType,Index));
    Uinput.DeleteLabelMeter(NETNODES,GetID(ObjType,Index));

  // Get bounding map area of all links adjacent to this node & delete them
    aRect := Map.GetAdjacencyRect(ObjType,Index,True);
  end

// Object is a link or a label
  else
  begin

  // If a link, then delete any reference to it as a label's meter link
    if ObjType in [PIPES..VALVES] then
      Uinput.DeleteLabelMeter(NETLINKS,GetID(ObjType,Index));

  // Get the object's bounding rectangle
    aRect := Map.GetRect(ObjType,Index);
  end;

// Delete the object and redraw the area it occupied on the map
  HiliteOff;
  DeleteNetworkObject(ObjType,Index);
  InvalidateMap(aRect);
end;


procedure TMapForm.MoveNode(const ObjType, ObjIndex: Integer;
  const X,Y: Extended);
//------------------------------------------------------------------
// Moves node of type ObjType with index ObjIndex to position X,Y.
//------------------------------------------------------------------
var
  aNode : TNode;
  aRect : TRect;

begin
// Get bounding area of all links adjacent to node without deleting them
  aRect := Map.GetAdjacencyRect(ObjType,ObjIndex,False);

// Update node's coordinates
  aNode := Node(ObjType,ObjIndex);
  aNode.X := X;
  aNode.Y := Y;

// Union bounding area for new position with that of
// old position and redraw that portion of the map
  if UnionRect(aRect,aRect,Map.GetRect(ObjType,ObjIndex))
  then InvalidateMap(aRect);
  HasChanged := True;
end;


procedure TMapForm.MoveLabelRect(const X,Y: Integer);
//----------------------------------------------------
// Moves a label's outline rectangle to offset of X,Y
// from its current position.
//----------------------------------------------------
begin
  DrawDotRect(ScrollRect);
  OffsetRect(ScrollRect,X-AnchorX,Y-AnchorY);
  DrawDotRect(ScrollRect);
  AnchorX := X;
  AnchorY := Y;
end;


procedure TMapForm.MoveLabel(const Index: Integer);
//--------------------------------------------------
// Moves label with index Index to position to which
// its outline rectangle was moved.
//--------------------------------------------------
var
  aLabel : TMapLabel;
  Xp, Yp : Integer;
begin
  DrawDotRect(ScrollRect);
  EraseLabel(Index);
  Xp := ScrollRect.Left;
  Yp := ScrollRect.Top;
  aLabel := MapLabel(Index);
  with aLabel do
  begin
    if (Anchor <> nil) and (Map.Window.ZoomIndex > 0) then
    begin
      X := Anchor.X + (Xp - Map.GetXpix(Anchor.X))/Map.Window.ZoomFactor[0];
      Y := Anchor.Y - (Yp - Map.GetYpix(Anchor.Y))/Map.Window.ZoomFactor[0];
    end
    else
    begin
      X := Map.GetX(Xp);
      Y := Map.GetY(Yp);
    end;
  end;
  DrawObject(LABELS,Index);
  HasChanged := True;
end;


procedure TMapForm.EraseLabel(const Index: Integer);
//----------------------------------------
// Erases a map label without deleting it.
//----------------------------------------
var
  aRect: TRect;
  aLabel: TMapLabel;
  x: Single;
begin
  HiliteOff;
  aLabel := MapLabel(Index);
  aRect := Map.GetRect(LABELS,Index);
  x := aLabel.X;
  aLabel.X := MISSING;
  InvalidateMap(aRect);
  aLabel.X := x;
end;


procedure TMapForm.ReplaceLabel(const Index: Integer; const S: String);
//----------------------------------------
// Replaces the text of a map label.
//----------------------------------------
var
  aRect: TRect;
begin
  HiliteOff;
  aRect := Map.GetRect(LABELS,Index);
  Network.Lists[LABELS].Strings[Index] := S;
  if UnionRect(aRect, aRect, Map.GetRect(LABELS,Index)) then
    InvalidateMap(aRect);
  HiliteOn;
end;


procedure TMapForm.ChangeHiliteObject(const ObjType, Index: Integer);
//-------------------------------------------------------------------
// Changes the currently highlighted object on the map to object
// of type ObjType whose index is Index.
//-------------------------------------------------------------------
begin
  if Vertexing then EndVertexing;
  HiliteObject := ObjType;
  HiliteIndex := Index;
  if  (HiliteObject in [PIPES..VALVES])
  and (HiliteIndex >= 0)
  then SelectedLink := Link(HiliteObject,HiliteIndex)
  else SelectedLink := nil;
  with MainForm do
  begin
    ToolButton14.Enabled := SelectedLink <> nil;
    MnuSelectVertex.Enabled := ToolButton14.Enabled;
  end;
  HiliteOff;
  HiliteOn;
end;


procedure TMapForm.HiliteOff;
//---------------------------------------------------
// Turns off hilighting of currently selected object.
//---------------------------------------------------
begin
// Disable the highlighting timer.
  if not Hilited then Exit;
  Timer1.Enabled := False;

// Copy over highlighted rectangle with
// corresponding area of Map's bitmap.
  Canvas.CopyRect(HiliteRect,Map.Bitmap.Canvas,HiliteRect);

// Zero the highlight rectangle
  HiliteRect := Rect(0,0,0,0);
  Hilited := False;
end;


procedure TMapForm.HiliteOn;
//--------------------------------------------------
// Turns on hilighting of currently selected object.
//--------------------------------------------------
var
  p1    : TPoint;
  hRect : TRect;  // Highlighted rectangle

begin
// Check that there is something to highlight
  if (HiliteObject < 0) then Exit;
  if (HiliteIndex < 0) then Exit;
  hRect := NORECT;

// Highlighted object is a node:
  if HiliteObject in [JUNCS..TANKS] then
  begin
    p1 := Map.GetNodePoint(HiliteObject, HiliteIndex);
    hRect := Rect(p1.X-NodeHiliteSize,p1.Y-NodeHiliteSize,
               p1.X+NodeHiliteSize+1,p1.Y+NodeHiliteSize+1);
  end

// Highlighted object is a label:
  else if HiliteObject = LABELS then
    hRect := Map.GetRect(HiliteObject,HiliteIndex)

// Highlighted object is a link:
  else if HiliteObject in [PIPES..VALVES] then
  begin
    if Vertexing and (SelectedVertex <> nil) then
    begin
      hRect := DrawVertices(True);
    end
    else if Map.GetLinkMidPoint(Link(HiliteObject,HiliteIndex),p1) then
      hRect := Rect(p1.X-LinkHiliteSize,p1.Y-LinkHiliteSize,
                    p1.X+LinkHiliteSize+1,p1.Y+LinkHiliteSize+1);
  end;

// Highlight the selection rectangle and enable
// the highlighting timer if Blinking option is on.
  if not IsRectEmpty(hRect) then
  begin
    Hilited := True;
    HiliteRect := hRect;
    with HiliteBitmap do
    begin
      Width  := HiliteRect.Right - HiliteRect.Left;
      Height := HiliteRect.Bottom - HiliteRect.Top;
      Canvas.CopyRect(Rect(0,0,Width,Height),Map.Canvas,hRect);
    end;
    Timer1Timer(self);
    Timer1.Enabled := (not Vertexing) and Blinking;
  end;
end;


procedure TMapForm.Timer1Timer(Sender: TObject);
//-------------------------------------------------
// Cause highlighted object to flash by inverting
// its highlighting rectangle.
//-------------------------------------------------
begin
  with HiliteBitmap do
  begin
    InvertRect(Canvas.Handle, Rect(0,0,Width,Height));
    self.Canvas.CopyRect(HiliteRect,Canvas,Rect(0,0,Width,Height));
  end;
end;


//===================================================================
//                   Event handlers for PopupMenu1
//                 (Options for editng a map object)
//===================================================================

procedure TMapForm.PopupMenu1Popup(Sender: TObject);
//------------------------------------------------------------
// OnPopup event handler for PopupMenu1.
// Determines which items appear in the menu.
//------------------------------------------------------------
begin
// Reverse & Vertices options appears only if current object is a link
  PopupReverse.Visible := CurrentList in [PIPES..VALVES];
  PopupVertices.Visible := PopupReverse.Visible;

// Paste option appears only if Network clipboard contains same object
  PopupPaste.Enabled := (CurrentList = Network.NetClipboard.ObjType);
  HiliteOff;
end;


procedure TMapForm.PopupCopyClick(Sender: TObject);
//-----------------------------------------------------------
// OnClick handler for PopUpCopy menu option.
// Copies selected object's properties to internal clipboard.
//-----------------------------------------------------------
begin
  case CurrentList of
  JUNCS..TANKS:  Uinput.CopyNode(CurrentList,CurrentItem[CurrentList]);
  PIPES..VALVES: Uinput.CopyLink(CurrentList, CurrentItem[CurrentList]);
  LABELS:        Uinput.CopyLabel(CurrentItem[LABELS]);
  end;
  HiliteOn;
end;


procedure TMapForm.PopupPasteClick(Sender: TObject);
//-----------------------------------------------------------
// OnClick handler for PopUpPaste menu option.
// Pastes copied properties into selected object.
//-----------------------------------------------------------
begin
  case CurrentList of
  JUNCS..TANKS:  Uinput.PasteNode(CurrentList,CurrentItem[CurrentList]);
  PIPES..VALVES: Uinput.PasteLink(CurrentList, CurrentItem[CurrentList]);
  LABELS:        Uinput.PasteLabel(CurrentItem[LABELS]);
  end;
  //Hilite turned back on in Paste... procedures
end;


procedure TMapForm.PopupDeleteClick(Sender: TObject);
//---------------------------------------------------
// OnClick handler for PopupDelete menu option.
// Deletes currently selected map item.
//---------------------------------------------------
begin
  BrowserForm.BtnDeleteClick(Sender);
end;


procedure TMapForm.PopupReverseClick(Sender: TObject);
//----------------------------------------------
// OnClick handler for PopupReverse menu option.
// Reverses the direction of a link.
//----------------------------------------------
begin
  if SelectedLink <> nil then
  begin
    SelectedLink.ReverseNodes;
    SelectedLink.ReverseVlist;
    MainForm.SetChangeFlags;
    UpdateEditor(HiliteObject, HiliteIndex);
    DrawObject(HiliteObject, HiliteIndex);
    HiliteOn;
  end;
end;


procedure TMapForm.PopupVerticesClick(Sender: TObject);
//-----------------------------------------------------
// OnClick handler for PopupVertices menu option.
// Puts map into Link Vertex Editing mode.
//-----------------------------------------------------
begin
  MainForm.ToolButton1Click(MainForm.ToolButton14);
end;


procedure TMapForm.PopupPropertiesClick(Sender: TObject);
//----------------------------------------------------------
// OnClick handler for PopupProperties menu option.
// Displays property editor for currently selected map item.
//-----------------------------------------------------------
begin
  BrowserForm.BtnEditClick(Sender);
end;


//===================================================================
//                 Event Handlers for PopUpMenu2.
//(Show/hide the legends & backdrop and invoke the Map Options dialog.)
//===================================================================

procedure TMapForm.PopupMenu2Popup(Sender: TObject);
begin
  PopupNodeLegend.Enabled := (not QueryFlag) and (CurrentNodeVar <> NOVIEW);
  PopupLinkLegend.Enabled := (not QueryFlag) and (CurrentLinkVar <> NOVIEW);
  PopupTimeLegend.Enabled := RunFlag;
  PopupBackdrop.Checked := MapBackdrop.Visible;
  PopupBackdrop.Enabled := (Length(MapBackdrop.Filename) > 0);
end;


procedure TMapForm.PopupOptionsClick(Sender: TObject);
begin
  SetMapOptions;
  HiliteOn;
end;


procedure TMapForm.PopupNodeLegendClick(Sender: TObject);
begin
  ToggleNodeLegend;
  HiliteOn;
end;


procedure TMapForm.PopupLinkLegendClick(Sender: TObject);
begin
  ToggleLinkLegend;
  HiliteOn;
end;


procedure TMapForm.PopupTimeLegendClick(Sender: TObject);
begin
  ToggleTimeLegend;
  HiliteOn;
end;


procedure TMapForm.PopupBackdropClick(Sender: TObject);
begin
  PopupBackdrop.Checked := not PopupBackdrop.Checked;
  MapBackdrop.Visible := PopupBackdrop.Checked;
  RedrawMap;
  OVmapForm.Redraw;
end;


procedure TMapForm.SetMapOptions;
//----------------------------------------------
// Invokes dialog to change map display options.
//----------------------------------------------
var
  OldColor: TColor;
begin
  HiliteOff;
  with TMapOptionsForm.Create(self) do
  try
    OldColor := MapForm.Color;
    TmpOptions := Map.Options;
    LoadOptions;
    SetActivePage(MapOptionsPage);
    if ShowModal = mrOK then
    begin
      UnloadOptions;
      Map.Options := TmpOptions;
      MapOptions := Map.Options;
      MapForm.Color := MapBackColor[Map.Options.ColorIndex];
      MapForm.Canvas.Pen.Color := MapForeColor[Map.Options.ColorIndex];
      NodeHiliteSize := MaxIntValue([PIXTOL,Map.Options.NodeSize+1]);
      LinkHiliteSize := MaxIntValue([PIXTOL,Map.Options.LinkSize+1]);
      if OldColor <> MapForm.Color then Map.RedrawBackdrop;
      RedrawMap;
      DrawNodeLegend;
      DrawLinkLegend;
    end;
    GetActivePage(MapOptionsPage);
  finally
    Free;
  end;
end;


//===================================================================
//                 Event Handlers for PopUpMenu3.
//                 (Link vertex editing options)
//===================================================================

procedure TMapForm.PopupMenu3Popup(Sender: TObject);
begin
  PopupDeleteVertex.Enabled := (SelectedVertex <> nil);
end;


procedure TMapForm.PopupAddVertexClick(Sender: TObject);
begin
  AddVertex;
end;


procedure TMapForm.PopupDeleteVertexClick(Sender: TObject);
begin
  DeleteVertex;
end;


procedure TMapForm.PopupQuitEditingClick(Sender: TObject);
begin
  MainForm.SelectorButtonClick;
end;


//===================================================================
//                       Map Legend Procedures
//===================================================================

procedure TMapForm.LinkLegendBoxDblClick(Sender: TObject);
//--------------------------------------------------------
// OnDoubleClick event handler for LinkLegendBox control.
// Makes legend invisible through call to ToggleLinkLegend.
//--------------------------------------------------------
begin
  ToggleLinkLegend;
end;


procedure TMapForm.NodeLegendBoxDblClick(Sender: TObject);
//--------------------------------------------------------
// OnDoubleClick event handler for NodeLegendBox control.
// Makes legend invisible through call to ToggleNodeLegend.
//--------------------------------------------------------
begin
  ToggleNodeLegend;
end;


procedure TMapForm.ToggleLinkLegend;
//---------------------------------------
// Toggles display of link legend on/off.
//---------------------------------------
begin
  with MainForm.MnuLinkLegend do
  begin
    Checked := not Checked;
    LinkLegendPanel.Visible := Checked;
    PopupLinkLegend.Checked := Checked;
  end;
end;


procedure TMapForm.ToggleNodeLegend;
//---------------------------------------
// Toggles display of node legend on/off.
//---------------------------------------
begin
  with MainForm.MnuNodeLegend do
  begin
    Checked := not Checked;
    NodeLegendPanel.Visible := Checked;
    PopupNodeLegend.Checked := Checked;
  end;
end;


procedure TMapForm.ModifyNodeLegend;
//--------------------------------------------------------
// Invokes dialog that modifies properties of node legend.
//--------------------------------------------------------
begin
// Invoke dialog through general purpose procedure in Umap unit
  if Umap.EditLegend(NodeLegend[CurrentNodeVar],CurrentPeriod,MapNodeColor,
    NodeLegendFrame.Framed) then
  begin

  // Redraw map with new set of node colors
    Uoutput.SetNodeColors;
    RedrawMap;
    DrawNodeLegend;
    HasChanged := True;
  end;
end;


procedure TMapForm.ModifyLinkLegend;
//--------------------------------------------------------
// Invokes dialog that modifies properties of link legend.
//--------------------------------------------------------
begin
// Invoke dialog through general purpose procedure in Umap unit
  if Umap.EditLegend(LinkLegend[CurrentLinkVar],CurrentPeriod,MapLinkColor,
    LinkLegendFrame.Framed) then
  begin

  // Redraw map with new set of link colors
    Uoutput.SetLinkColors;
    RedrawMap;
    DrawLinkLegend;
    HasChanged := True;
  end;
end;


procedure TMapForm.DragLegend(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-------------------------------------------------------
// OnMouseDown event handler for both the NodeLegendBox
// and the LinkLegendBox. Left button drags legend to
// new position, right button invokes Legend Editor form.
//-------------------------------------------------------
const
  SC_DragMove = $F012;  // Magic number for the drag operation.

begin
// Pressing left button begins a drag event.
  if Button = mbLeft then
  begin
    ReleaseCapture;
    if Sender = NodeLegendBox then
    begin
      NodeLegendPanel.Perform(WM_SysCommand, SC_DragMove, 0);
      NodeLegendFrame.X := NodeLegendPanel.Left / MapForm.ClientWidth;
      NodeLegendFrame.Y := NodeLegendPanel.Top / MapForm.ClientHeight;
    end
    else if Sender = LinkLegendBox then
    begin
      LinkLegendPanel.Perform(WM_SysCommand, SC_DragMove, 0);
      LinkLegendFrame.X := LinkLegendPanel.Left / MapForm.ClientWidth;
      LinkLegendFrame.Y := LinkLegendPanel.Top / MapForm.ClientHeight;
    end;
  end

// Pressing right button modifies the map legend.
  else
  begin
    if Sender = NodeLegendBox then
      ModifyNodeLegend
    else
      ModifyLinkLegend;
  end;
end;


procedure TMapForm.NodeLegendBoxPaint(Sender: TObject);
//-----------------------------------
// OnPaint handler for NodeLegendBox.
// Redraws the Node Legend.
//-----------------------------------
begin
  DrawNodeLegend;
end;


procedure TMapForm.LinkLegendBoxPaint(Sender: TObject);
//-----------------------------------
// OnPaint handler for LinkLegendBox.
// Redraws the Link Legend.
//-----------------------------------
begin
  DrawLinkLegend;
end;


procedure TMapForm.DrawNodeLegend;
//------------------------------------------
// Oversees the drawing of the Node Legend
// (actual drawing done by Umap.DrawLegend).
//------------------------------------------
begin
  if (CurrentNodeVar = NOVIEW) or (QueryFlag) then
    NodeLegendPanel.Visible := False
  else if PopupNodeLegend.Checked then
  begin
    NodeLegendPanel.Color := MapBackColor[Map.Options.ColorIndex];
    Umap.DrawLegend(NodeLegendBox.BoundsRect,
      NodeLegendBox.Canvas,
      NodeLegendPanel.Color,
      NodeLegendFrame.Framed,
      BrowserForm.NodeViewBox.Items[CurrentNodeVar],
      NodeLegend[CurrentNodeVar],
      MapNodeColor);
    NodeLegendPanel.Visible := True;
  end;
end;


procedure TMapForm.DrawLinkLegend;
//------------------------------------------
// Oversees the drawing of the Link Legend
// (actual drawing done by Umap.DrawLegend).
//------------------------------------------
begin
  if (CurrentLinkVar = NOVIEW) or (QueryFlag) then
    LinkLegendPanel.Visible := False
  else if PopupLinkLegend.Checked then
  begin
    LinkLegendPanel.Color := MapBackColor[Map.Options.ColorIndex];
    Umap.DrawLegend(LinkLegendBox.BoundsRect,
      LinkLegendBox.Canvas,
      LinkLegendPanel.Color,
      LinkLegendFrame.Framed,
      BrowserForm.LinkViewBox.Items[CurrentLinkVar],
      LinkLegend[CurrentLinkVar],
      MapLinkColor);
    LinkLegendPanel.Visible := True;
  end;
end;


//===================================================================
//                       Time Panel Procedures
//===================================================================

procedure TMapForm.TimeLegendPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//---------------------------------------------------------
// OnMouseDown event handler.
//---------------------------------------------------------
const
  SC_DragMove = $F012;  // Magic number for the drag operation.

begin
// Pressing left button begins a drag event.
  if Button = mbLeft then
  begin
    ReleaseCapture;
    if Sender = TimeLegendPanel then with Sender as TPanel do
    begin
      Perform(WM_SysCommand, SC_DragMove, 0);
      TimeLegendFrame.X := (Left + Width) / MapForm.ClientWidth;
      TimeLegendFrame.Y := Top / MapForm.ClientHeight;
    end;
  end
end;


procedure TMapForm.TimeLegendPanelDblClick(Sender: TObject);
//---------------------------------------------------
// OnDoubleClick event handler.
//---------------------------------------------------
begin
  ToggleTimeLegend;
end;


procedure TMapForm.ToggleTimeLegend;
//---------------------------------------
// Toggles display of time legend on/off.
//---------------------------------------
begin
  with MainForm.MnuTimeLegend do
  begin
    Checked := not Checked;
    TimeLegendPanel.Visible := Checked;
    PopupTimeLegend.Checked := Checked;
  end;
end;


//===================================================================
//                   Map Copying & Printing Procedures
//===================================================================

procedure TMapForm.CopyTo;
//------------------------------------------------------------------
// Copies map to file or to Windows Clipboard in variety of formats.
//------------------------------------------------------------------
begin
// Create CopyTo dialog box
  with TCopyToForm.Create(self) do
  try

  // Get name of destination file
  // (Empty name means we copy to Clipboard)
    if ShowModal = mrOK then
    begin

    // Copy map in the selected format
      case FormatGroup.ItemIndex of
        0: CopyToBitmap(DestFileName);
        1: CopyToMetafile(DestFileName);
        2: CopyToString(DestFileName);
      end;
    end;
  finally
    Free;
  end;
end;


procedure TMapForm.CopyToBitmap(const Fname: String);
//-----------------------------------------------------------------
// Copies network map in bitmap format to either file or clipboard.
//-----------------------------------------------------------------
var
  aBitmap: TBitmap;
begin
  aBitmap := GetFormImage;
  try
    if Length(Fname) > 0 then aBitmap.SaveToFile(Fname)
    else Clipboard.Assign(aBitmap);
  finally
    aBitmap.Free;
  end;
end;


procedure TMapForm.CopyToMetafile(const Fname: String);
//-------------------------------------------------------
// Copies network map in Windows enhanced metafile format
// to either file or clipboard.
//-------------------------------------------------------
var
  MyMetafile: TMetafile;
begin
  MyMetafile := TMetaFile.Create;   // Create the metafile
  try
    DrawToMetafile(MyMetafile);     // Draw map on metafile
    if Length(Fname) > 0 then
      MyMetafile.SaveToFile(Fname)  // Save metafile to file
    else
      ClipBoard.Assign(MyMetafile); // Copy metafile to clipboard
  finally
    MyMetafile.Free;                // Free the metafile
  end;
end;


procedure TMapForm.DrawToMetafile(aMetafile: TMetafile);
//------------------------------------------------------
// Draws network map on aMetafile.
//------------------------------------------------------
var
  aMetafileCanvas: TMetafileCanvas;
  WidthInMM, HeightInMM: Integer;
  WidthInPixels, HeightInPixels: Integer;
  MMPerPixelHorz, MMPerPixelVert: Integer;
  ClippingRgn: HRGN;
  DC: HDC;
  SFx, SFy: Single;
  R: TRect;

begin
//Set dimensions of metafile
  DC := Canvas.Handle;
  WidthInMM := GetDeviceCaps(DC, HORZSIZE);
  HeightInMM := GetDeviceCaps(DC, VERTSIZE);
  WidthInPixels := GetDeviceCaps(DC, HORZRES);
  HeightInPixels := GetDeviceCaps(DC, VERTRES);
  MMPerPixelHorz := (WidthInMM*100) div WidthInPixels;
  MMPerPixelVert := (HeightInMM*100) div HeightInPixels;
  aMetafile.MMWidth := Map.Window.Pwidth*MMPerPixelHorz;
  aMetafile.MMHeight := Map.Window.Pheight*MMPerPixelVert;

//Create a canvas for the metafile & a clipping region for the canvas
  aMetafileCanvas := TMetafileCanvas.Create(aMetafile,0);
  ClippingRgn := CreateRectRgn(0,0,Map.Window.Pwidth,Map.Window.Pheight);

//Make the metafile canvas the map's canvas & redraw map on it
  try
    with Map do
    begin
      Canvas := TCanvas(aMetafileCanvas);
      SelectClipRgn(Canvas.Handle, ClippingRgn);
      if MapBackdrop.Visible then DrawBackdrop(Canvas);
      DrawNetwork;
      SFx := Map.Window.Pwidth/ClientWidth - 1.0;
      SFy := Map.Window.Pheight/ClientHeight - 1.0;
      if NodeLegendPanel.Visible then
      begin
        R := NodeLegendPanel.BoundsRect;
        if OffsetRect(R, Round(R.Left*SFx), Round(R.Top*SFy)) then
        DrawLegend(R, Canvas,
          MapBackColor[Options.ColorIndex],
          NodeLegendFrame.Framed,
          BrowserForm.NodeViewBox.Items[CurrentNodeVar],
          NodeLegend[CurrentNodeVar],
          MapNodeColor);
      end;
      if LinkLegendPanel.Visible then
      begin
        R := LinkLegendPanel.BoundsRect;
        if OffsetRect(R, Round(R.Left*SFx), Round(R.Top*SFy)) then
        DrawLegend(R, Canvas,
          MapBackColor[Options.ColorIndex],
          LinkLegendFrame.Framed,
          BrowserForm.LinkViewBox.Items[CurrentLinkVar],
          LinkLegend[CurrentLinkVar],
          MapLinkColor);
      end;
      if TimeLegendPanel.Visible then
      begin
        R := TimeLegendPanel.BoundsRect;
        if OffsetRect(R, Round(R.Left*SFx), Round(R.Top*SFy)) then
        DrawTimeLegend(R, Canvas,
          MapBackColor[Options.ColorIndex], TimeLegendPanel.Caption);
      end;
    end;

//Restore map's canvas & free the metafile canvas & clipping region
  finally
    Map.Canvas := Map.Bitmap.Canvas;
    DeleteObject(ClippingRgn);
    aMetafileCanvas.Free;
  end;
end;


procedure TMapForm.CopyToString(const Fname: String);
//-----------------------------------------------------
// Copies data for current network node view variable
// in string (text) format to either file or Clipboard.
//-----------------------------------------------------
var
  Slist: TStringList;
  aNode: TNode;
  I,J  : Integer;
  S    : String;

begin
// Create a stringlist to hold value for each node
  Slist := TStringList.Create;
  try

  // Add heading for each item
    with BrowserForm.NodeViewBox do S := Items[ItemIndex];
    Slist.Add(TXT_NODE + #9 + 'X ' + #9 + 'Y ' + #9 + S);

  // Iterate over each node
    for I := JUNCS to TANKS do
    begin
      for J := 0 to Network.Lists[I].Count-1 do
      begin

      // Build up tab-delimited string of ID, X, Y, & value
        aNode := Node(I,J);
        S := GetID(I,J) + '' + #9;
        S := S + FloatToStr(aNode.X) + #9;
        S := S + FloatToStr(aNode.Y) + #9;
        S := S +
          Uoutput.GetNodeValStr(CurrentNodeVar,CurrentPeriod,I,J);

      // Add tab-delimited string to list
        Slist.Add(S);
      end;
    end;

  // Save stringlist to file if file name supplied
  // Otherwise place text of stringlist onto clipboard
    if Length(Fname) > 0 then Slist.SaveToFile(Fname)
    else Clipboard.SetTextBuf(PChar(Slist.Text));

// Free the stringlist.
  finally
    Slist.Free;
  end;
end;


procedure TMapForm.Print(Destination: TDestination);
//----------------------------------------------------
// Prints map to Destination (printer or preview form)
//----------------------------------------------------
var
  aPicture      : TPicture;
  oldColorIndex : Integer;
  MapWidth,
  MapHeight,
  FrameLeft,
  FrameTop,
  FrameWidth,
  Frameheight   : Single;
  SF            : Single;
  MyMetafile    : TMetafile;
  oldRect       : TRect;
  oldPPW        : Single;
  newWidth      : Integer;
  newHeight     : Integer;

begin
// Begin job on thePrinter's destination (printer or print preview)
  Screen.Cursor := crHourglass;
  with MainForm.thePrinter do
  begin
    BeginJob;
    SetDestination(Destination);
    SetFontInformation('Times New Roman',11,[]);

  // Find bounding box of display area on printed page
    FrameTop := GetYPos;
    with PageLayout do
    begin
      FrameLeft := LMargin;
      FrameWidth := GetPageWidth - LMargin - RMargin;
      FrameHeight := GetPageHeight - FrameTop - BMargin - 0.25;
    end;
    MapWidth := FrameWidth;
    MapHeight := FrameHeight;

  // Adjust bounding box to preserve aspect ratio & center it
    SF := ClientHeight/ClientWidth;
    if MapHeight/SF > MapWidth
    then MapHeight := MapWidth*SF
    else MapWidth  := MapHeight/SF;
    FrameLeft := FrameLeft + (FrameWidth - MapWidth)/2;

  // If map has a backdrop then print headers/footers
  // (otherwise, for some unknown reason headers/footers don't
  // get printed as they normally would after EndJob is called)
    if MapBackdrop.Visible then FinishPage;

  // Save current map pixel window and scale factor
     oldRect := Map.Window.MapRect;
     oldPPW := Map.Window.PPW;

  // Rescale map window to pixel extent of printed area
     SF := MapWidth/ClientWidth*Screen.PixelsPerInch;
     newWidth := Round(SF*ClientWidth);
     if newWidth > Printer.PageWidth then newWidth := Printer.PageWidth;
     newHeight := Round(SF*ClientHeight);
     if newHeight > Printer.PageHeight then newHeight := Printer.PageHeight;
     Map.ResizeWindow(Rect(0,0,newWidth,newHeight));
     Map.Window.PPW := oldPPW*SF;

  // Draw map with a white background to a metafile
    oldColorIndex := Map.Options.ColorIndex;
    Map.Options.ColorIndex := 1;
    MyMetafile := TMetaFile.Create;
    try

      DrawToMetafile(MyMetafile);

  // Create a Picture object to hold the metafile
  // and draw it on the printed page
      aPicture := TPicture.Create;
      try
        aPicture.Assign(MyMetafile);
        StretchGraphic(FrameLeft, FrameTop, FrameLeft+MapWidth,
          FrameTop+MapHeight, aPicture);
      finally
        aPicture.Free;
      end;

    finally
      MyMetafile.Free;
    end;

  // Restore map properties
    Map.Options.ColorIndex := oldColorIndex;
    Map.ResizeWindow(oldRect);
    Map.Window.PPW := oldPPW;
    Screen.Cursor := crDefault;
    EndJob;
  end;
end;

end.
