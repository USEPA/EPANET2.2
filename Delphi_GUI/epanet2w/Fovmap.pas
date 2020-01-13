unit Fovmap;

{-------------------------------------------------------------------}
{                    Unit:    Fovmap.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit that contains a full-scale outline of the network     }
{   map with a focus rectangle drawn around the current view        }
{   area. This form is created at start up and remains active       }
{   throughout the session.                                         }
{                                                                   }
{   Draging the rectangle causes the view area on the main map      }
{   (drawn on the MapForm form) to shift accordingly.               }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, System.Types,
  Uglobals, Umap, Uutils;

type
  TOVMapForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    OVmap    : Tmap;
    FocusRect: TRect;
    DragRect : TRect;
    Draging  : Boolean;
    DragX    : Integer;
    DragY    : Integer;
    procedure PaintDragRect;
    procedure PaintFocusRect;
    procedure SizeFocusRect;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
  public
    { Public declarations }
    NeedsUpdating: Boolean;
    procedure ShowMapExtent;
    procedure Redraw;
    procedure Rescale;
  end;

var
  OVMapForm: TOVMapForm;

implementation

{$R *.DFM}

uses Fmain, Fmap;

procedure TOVMapForm.FormCreate(Sender: TObject);
//-------------------------------------------------------
// OnCreate event handler for the overview map form.
// Creates an OVmap object of class TMap (see Umap unit).
//-------------------------------------------------------
var
  P: TPoint;
begin
  OVmap := TMap.Create;
  with OVmap do
  begin
    Options.DispLabels := False;
    Options.ColorIndex := 5;
    Options.NodeSize := 0;
  end;
  FocusRect := Rect(-1,-1,-1,-1);
  Draging := False;
  NeedsUpdating := False;

// Position form at bottom right of Main form
  with MainForm do
  begin
    P.x := Left + Width - 2;
    P.Y := Top + Height - 2;
  end;
  Top := P.Y - Height;
  Left := P.X - Width;
end;

procedure TOVMapForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
//-------------------------------------
// OnClose event handler -- hides form.
//-------------------------------------
begin
  MainForm.MnuOVMap.Checked := False;
  Action := caHide;
end;

procedure TOVMapForm.FormDestroy(Sender: TObject);
//--------------------------------------------------
// OnDestroy handler for the form.
// Frees the OVmap object.
//--------------------------------------------------
begin
  OVmap.Free;
end;

procedure TOVMapForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1
  then HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 170);
end;

procedure TOVMapForm.FormResize(Sender: TObject);
//-----------------------------------------------
// OnResize handler for the form.
// Re-scales the map to fit the window.
//-----------------------------------------------
begin
  Rescale;
end;

procedure TOVMapForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
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

procedure TOVMapForm.FormPaint(Sender: TObject);
//--------------------------------------------------
// OnPaint handler for form.
// Copies network map outline from internal bitmap
// to form's canvas and redraws the focus rectangle.
//--------------------------------------------------
begin
  Canvas.Draw(0,0,OVmap.Bitmap);
  PaintFocusRect;
end;

procedure TOVMapForm.FormActivate(Sender: TObject);
//-------------------------------------------------
// OnActivate handler for the form.
//-------------------------------------------------
begin
  if NeedsUpdating then Redraw;
end;

procedure TOVMapForm.Redraw;
//-------------------------------------------------
// Redraws the network map outine and the current
// map extent on the form.
//-------------------------------------------------
var
  Color: TColor;
begin
  with OVmap do
  begin
    Color := MapGrayColor[Options.ColorIndex];
    if MapBackdrop.Visible then Color := clBlack;
    ClearMap;
    RedrawBackdrop;
    if MapBackdrop.Visible then
      Canvas.CopyRect(Window.MapRect,BackBM.Canvas,Window.MapRect);
    DrawOutline(1,Color);
  end;
  ShowMapExtent;
  NeedsUpdating := False;
end;

procedure TOVMapForm.Rescale;
//----------------------------------------------------
// Updates the scale and position of the overview map.
//----------------------------------------------------
begin
  OVmap.Resize(ClientRect);
  OVmap.Rescale(MapDimensions);
  Redraw;
end;

procedure TOVMapForm.ShowMapExtent;
//-------------------------------------------------
// Displays focus rectangle. Called from Fmap unit.
//-------------------------------------------------
begin
  SizeFocusRect;
  Refresh;
end;

procedure TOVMapForm.SizeFocusRect;
//--------------------------------------------------------
// Finds bounding rectangle for main map's current extent.
//--------------------------------------------------------
var
  X1,X2,Y1,Y2: Single;
  L,T,R,B: Integer;
begin
//If no zoom-in, then don't display focus rectangle.
  if MapForm.Map.Window.ZoomIndex = 0 then
    FocusRect := Rect(-1,-1,-1,-1)

  else
  begin
  //Determine world coordinates of zoomed-in area.
    with MapForm.Map.Window do
    begin
      X1 := Woffset.X;
      Y1 := Woffset.Y;
      X2 := X1 + Pwidth/PPW;
      Y2 := Y1 + Pheight/PPW;
    end;
  //Translate these coordinates to overview map scaling.
    with OVmap.Window do
    begin
      L := Round((X1-Woffset.X)*PPW + Poffset.X);
      T := Round(Poffset.Y - (Y2-Woffset.Y)*PPW);
      R := Round((X2-Woffset.X)*PPW + Poffset.X);
      B := Round(Poffset.Y - (Y1-Woffset.Y)*PPW);
    end;
    FocusRect := Rect(L,T,R,B);
  end;
end;

procedure TOVMapForm.PaintFocusRect;
//----------------------------------------
// Draws focus rectangle on form's canvas.
//----------------------------------------
begin
  with Canvas do
  begin
    Pen.Width := 3;
    Pen.Color := clRed;
    Brush.Style := bsClear;
    with FocusRect do
      Rectangle(Left,Top,Right,Bottom);
  end;
end;

procedure TOVMapForm.PaintDragRect;
//------------------------------------------------
// Draws outline of focus rectangle when dragging.
//------------------------------------------------
begin
  with Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psDot;
    Pen.Color := clBlack;
    Brush.Style := bsSolid;
    DrawFocusRect(DragRect);
    Pen.Style := psSolid;
  end;
end;

procedure TOVMapForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//---------------------------------------
// Initiates dragging of focus rectangle.
//---------------------------------------
begin
  if (Button = mbLeft) and
  not IsRectEmpty(FocusRect) and
  PtInRect(FocusRect, Point(X,Y)) then
  begin
    Draging := True;
    DragX := X;
    DragY := Y;
    DragRect := FocusRect;
    PaintDragRect;
  end;
end;

procedure TOVMapForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------
// Relocates focus rectangle after dragging ends.
//-----------------------------------------------
var
  X1,Y1: Single;
  aRect: TRect;
begin
  if Draging then
  begin
  //Stop dragging
    PaintDragRect;
    Draging := False;

  //Exit if focus rectangle dragged off of form
    if not IntersectRect(aRect,DragRect,ClientRect)
      then Exit;

  //Relocate focus rectangle
    FocusRect := DragRect;
    Refresh;

  //Pan system map to new position
    with OVMap.Window do
    begin
      X1 := Woffset.X + (FocusRect.Left-Poffset.X)/PPW;
      Y1 := Woffset.Y - (FocusRect.Bottom-Poffset.Y)/PPW;
    end;
    with MapForm do
    begin
      Map.Window.Woffset.X := X1;
      Map.Window.Woffset.Y := Y1;
      Map.RedrawBackdrop;
      RedrawMap;
    end;
  end;
end;

procedure TOVMapForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//------------------------------------------------------------
// Drags outline of focus rectangle to current mouse position.
//------------------------------------------------------------
begin
  if Draging then
  begin
    PaintDragRect;
    OffsetRect(DragRect, X-DragX, Y-DragY);
    PaintDragRect;
    DragX := X;
    DragY := Y;
  end;
end;

end.
