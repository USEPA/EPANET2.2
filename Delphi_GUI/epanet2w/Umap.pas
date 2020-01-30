unit Umap;

{-------------------------------------------------------------------}
{                    Unit:    Umap.pas                              }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that defines the TMap object. This object    }
{   contains drawing methods for rendering the Network Map on a     }
{   memory bitmap. It also draws the map legends, identifies the    }
{   bounding rectangle for an object, and handles map re-scaling.   }
{-------------------------------------------------------------------}

interface

uses Windows, Graphics, SysUtils, Dialogs, Forms, Classes,
     Controls, Math, System.Types, System.UITypes,
     Uglobals, Uutils, Uinput, Uoutput, Dlegend;

const
  MAX_INT_COORD = 32767;

type

  TLinkSymbol = (lsArrow, lsPump, lsValve, lsCheckValve);

  TMap = class(TObject)          // Map object
    Canvas    : TCanvas;         // Display canvas
    Bitmap    : TBitmap;         // Bitmap containing the drawn map
    BackBM    : TBitmap;         // Bitmap containing backdrop image
    Window    : TMapWindow;      // Display window sizing info
    Dimensions: TMapDimensions;  // Physical map dimensions
    Options   : TMapOptions;     // Display options
    constructor Create;
    destructor  Destroy; override;

    procedure ClearMap;
    procedure DrawArrow(aLink: TLink; P1: TPoint; P2: TPoint);
    procedure DrawArrowHead(const X: Integer; const Y: Integer;
      const Style: TArrowStyle; const Size: Integer;
      const Aratio: Single; const Asin: Extended;
      const Acos: Extended);
    function  DrawBackdrop(BDCanvas: TCanvas): Boolean;
    procedure DrawEmitter(const X: Integer; const Y: Integer;
      const Size: Integer);
    procedure DrawJunc(const X: Integer; const Y: Integer;
      const Size: Integer);
    procedure DrawLabels;
    procedure DrawLink(const P1: TPoint; const P2: TPoint; aLink: TLink);
    procedure DrawLinkIDLabel(const ObjType: Integer; const Index: Integer;
      const P1: TPoint; const P2: TPoint; const Size: Integer);
    procedure DrawLinks;
    procedure DrawLinkSymbol(const P1: TPoint; const P2: TPoint;
      const Symbol: TLinkSymbol);
    procedure DrawLinkValueLabel(const ObjType: Integer; const Index: Integer;
      const P1: TPoint; const P2: TPoint; const Size: Integer);
    procedure DrawMap;
    procedure DrawMeterLabel(const Index: Integer);
    procedure DrawNetwork;
    procedure DrawNodeIDLabel(const ObjType: Integer; const Index: Integer;
      const P: TPoint; const Size: Integer);
    procedure DrawNodes;
    procedure DrawNodeValueLabel(const ObjType: Integer; const Index: Integer;
      const P: TPoint; const Size: Integer);
    procedure DrawObject(const ObjType: Integer; const Index: Integer);
    procedure DrawOutline(const LineWidth: Integer; const LineColor: TColor);
    procedure DrawPumpSymbol(const X: Integer; const Y: Integer;
      const Size: Integer; const Direction: Integer;
      const Asin: Extended; const Acos: Extended);
    procedure DrawReserv(const X: Integer; const Y: Integer;
      const Size: Integer);
    procedure DrawSource(const ObjType, Index, X, Y: Integer);
    procedure DrawTank(const X: Integer; const Y: Integer; const Size: Integer);

    function  GetAdjacencyRect(const ObjType: Integer; const Index: Integer;
      const DeleteLinks: Boolean): TRect;
    procedure GetAnchoredLabelsRect(aNode: TNode; var aRect: TRect);
    function  GetBackdrop(Picture:TPicture):Boolean;
    function  GetBackdropBounds(const AspectRatio: Extended): TRect;
    function  GetLabelRect(const Index: Integer): TRect;
    function  GetLinkMidpoint(aLink: TLink; var P: TPoint): Boolean;
    procedure GetLinkMidsegment(aLink: TLink; var Pa, Pb: TPoint);
    function  GetLinkRect(const ObjType, Index: Integer): TRect;
    procedure GetNodeIDRect(const NodeType: Integer; const Index: Integer;
      var aRect: TRect);
    function  GetNodePixPos(aNode: TNode; var P: TPoint): Boolean;
    function  GetNodePoint(const NodeType: Integer;
      const Index: Integer): TPoint;
    function  GetNodeRect(const ObjType, Index: Integer): TRect;
    function  GetRect(const ObjType: Integer; const Index: Integer): TRect;
    function  GetX(const X: Integer): Extended;
    function  GetY(const Y: Integer): Extended;
    function  GetXpix(const X: Extended): Integer;
    function  GetYpix(const Y: Extended): Integer;

    function  IsBounded(const A, B, C: Extended): Boolean;
    function  RedrawBackdrop: Boolean;
    procedure Rescale(const NewDimensions: TMapDimensions);
    procedure Resize(const Rect: TRect);
    procedure ResizeWindow(const Rect: TRect);
    procedure ResizeBitmap(var aBitmap: TBitmap; const W,H: Integer);

    procedure SetLinkColor(const ObjType: Integer; const Index: Integer);
    function  SetLinkSize: Integer;
    procedure SetNodeColor(const ObjType: Integer; const Index: Integer);
    function  SetNodeSize: Integer;
  end;

  procedure DrawLegend(const R: TRect;  const C: TCanvas;
    const BackColor: TColor; const Framed: Boolean; const T: String;
    const Legend: TMapLegend; const MapColor: array of TColor);
  procedure DrawTimeLegend(R: TRect; C: TCanvas;
    const BackColor: TColor; const S: String);
  function EditLegend(var Legend: TMapLegend; const TimePeriod: Integer;
    var Colors: array of TColor; var Framed: Boolean): Boolean;

implementation

var
  P1, P2: TPoint;
  LastColorIndex: Integer;
  CharHeight: Integer;
  BackColor: TColor;
  ForeColor: TColor;
  GrayColor: TColor;

//==================================================================
//                       TMap Creator & Destructor
//==================================================================

constructor TMap.Create;
begin
  inherited Create;
  Bitmap := TBitmap.Create;
  if Bitmap <> nil then Canvas := Bitmap.Canvas;
  BackBM := TBitmap.Create;
  Options := MapOptions;
  Dimensions := MapDimensions;
end;

destructor TMap.Destroy;
begin
  Bitmap.Free;
  BackBM.Free;
  inherited Destroy;
end;


//===================================================================
//                         Map Drawing Methods
//===================================================================

procedure TMap.ClearMap;
//--------------------------------
// Clears map to background color
//--------------------------------
begin
  Uutils.Cls(Canvas,Window.MapRect,MapBackColor[Options.ColorIndex]);
end;


function TMap.RedrawBackdrop: Boolean;
//-----------------------------------------------------------
// Redraws backdrop image on the Map object's backdrop bitmap
//-----------------------------------------------------------
begin
  Result := DrawBackdrop(BackBM.Canvas);
end;


function TMap.GetBackdrop(Picture:TPicture):Boolean;
//--------------------------------------------------
// Retrives backdrop image from file into Picture.
//--------------------------------------------------
var
  FullName    : string;
  ValidPicture: Boolean;

  function ValidFile(const FileName: string): Boolean;
  begin
    Result := GetFileAttributes(PChar(FileName)) <> $FFFFFFFF;
  end;

begin
    Screen.Cursor := crHourglass;
    FullName := MapBackdrop.Filename;
    ValidPicture := FileExists(FullName) and ValidFile(FullName);
    if ValidPicture then
    try
      Picture.LoadFromFile(FullName);
    except
      ValidPicture := False;
    end;
    if not ValidPicture then
    begin
     Picture.Assign(nil);
    end;
    Screen.Cursor := crDefault;
    Result := ValidPicture;
end;


function TMap.GetBackdropBounds(const AspectRatio: Extended): TRect;
//------------------------------------------------------------------
// Finds bounding rectangle of backdrop that fills map extent
// while preserving its aspect ratio.
//------------------------------------------------------------------
var
  w, h        : Extended;
  x0, y0      : Extended;
begin
  with Dimensions do
  begin

  // Find origin of picture within map extent
    x0 := LowerLeft.X + MapBackdrop.Offset.X;
    y0 := UpperRight.Y - MapBackdrop.Offset.Y;

  // Re-scale the width & height of the picture so that it
  // fills the map's extent, while preserving its aspect ratio
    w := UpperRight.X - LowerLeft.X;
    h := UpperRight.Y - LowerLeft.Y;
    if AspectRatio > 1 then h := w/AspectRatio
    else w := h*AspectRatio;
  end;

// Determine bounding rectangle in pixel units
  Result := Rect(GetXpix(x0), GetYpix(y0), GetXpix(x0+w), GetYpix(y0-h));
end;


function TMap.DrawBackdrop(BDCanvas: TCanvas): Boolean;
//---------------------------------------------------
// Draws map's backdrop image on specified Canvas
//---------------------------------------------------
var
  AspectRatio : Extended;
  Picture     : TPicture;
  r           : TRect;

begin
// Create a TPicture to hold the backdrop image
  Result := False;
  Picture := TPicture.Create;
  try

  // Retrieve the backdrop picture & find its bounding rectangle
    if GetBackdrop(Picture) then
    begin
      AspectRatio := Picture.Width / Picture.Height;
      r := GetBackdropBounds(AspectRatio);

    // Draw the picture in the canvas
      Uutils.Cls(BDCanvas, Window.MapRect, MapBackColor[Options.ColorIndex]);
      BDCanvas.StretchDraw(r, Picture.Graphic);
      Result := True;
    end;

// Free the Picture
  finally
    Picture.Free;
  end;
end;


procedure TMap.DrawMap;
//--------------------------------------
// Draws system map on current canvas.
//--------------------------------------
begin
  ClearMap;
  if MapBackdrop.Visible then
    Canvas.CopyRect(Window.MapRect,BackBM.Canvas,Window.MapRect);
  DrawNetwork;
end;


procedure TMap.DrawNetwork;
//--------------------------------------
// Draws pipe network  on current canvas.
//--------------------------------------
begin
// Assign values to global colors
  ForeColor := MapForeColor[Options.ColorIndex];
  BackColor := MapBackColor[Options.ColorIndex];
  GrayColor := MapGrayColor[Options.ColorIndex];

// Setup pen & brush to draw on map's Canvas
  Canvas.Pen.Color := ForeColor;
  Canvas.Brush.Color := ForeColor;
  Canvas.Brush.Style := bsSolid;

// Set Canvas's font for ID/Value labeling
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Options.NotationSize;
  Canvas.Font.Style := [];
  CharHeight := Canvas.TextHeight('[');

// Draw links, nodes, & labels
  DrawLinks;
  DrawNodes;
  DrawLabels;
  LastColorIndex := -999;
end;


procedure TMap.DrawLinks;
//------------------------------
// Draws pipes, pumps, & valves.
//------------------------------
var
  I,J,N: Integer;
begin
  if Options.NotationTranspar then
    SetBkMode(Canvas.Handle,TRANSPARENT);
  LastColorIndex := -999;
  Canvas.Pen.Width := Options.LinkSize;
  for I := PIPES to VALVES do
  begin
    N := Network.Lists[I].Count - 1;
    for J := 0 to N do DrawObject(I,J);
  end;
  Canvas.Pen.Width := 1;
  SetBkMode(Canvas.Handle,OPAQUE);
end;


procedure TMap.DrawNodes;
//---------------------------------------
// Draws junctions, reservoirs and tanks.
//---------------------------------------
var
  I,I1,J,N: Integer;
begin
  if Options.NotationTranspar then
    SetBkMode(Canvas.Handle,TRANSPARENT);
  LastColorIndex := -999;
  I1 := JUNCS;
  if (not Options.DispJuncs) then I1 := RESERVS;
  for I := I1 to TANKS do
  begin
    N := Network.Lists[I].Count - 1;
    for J := 0 to N do DrawObject(I,J);
  end;
  SetBkMode(Canvas.Handle,OPAQUE);
end;


procedure TMap.DrawLabels;
//------------------------
// Draws map labels.
//------------------------
var
  J: Integer;
  ZoomFlag: Boolean;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := BackColor;
  Canvas.Pen.Color := ForeColor;
  Canvas.Font.Color := ForeColor;
  if (Options.DispLabels) then
  begin
    ZoomFlag := (MapZoomRatio >= Options.LabelZoom);
    if Options.LabelsTranspar then SetBkMode(Canvas.Handle,TRANSPARENT);
    for J := 0 to Network.Lists[LABELS].Count - 1 do
      if ZoomFlag or (MapLabel(J).MeterType > 0) then
      begin
        if MapLabel(J).X <> MISSING then DrawObject(LABELS,J);
      end;
    SetBkMode(Canvas.Handle,OPAQUE);
  end;
end;


procedure TMap.DrawOutline(const LineWidth: Integer; const LineColor: TColor);
//------------------------------------------
// Draws network outline.
// Used for panning and displaying map
// on contour plot and on Overview map.
//------------------------------------------
var
  I, J    : Integer;
  aLink   : TLink;
begin
  LastColorIndex := -999;
  Canvas.Pen.Color := LineColor;
  Canvas.Pen.Width := LineWidth;
  for I := PIPES to VALVES do
  begin
    for J := 0 to Network.Lists[I].Count - 1 do
    begin
      aLink := Link(I,J);
      if not GetNodePixPos(aLink.Node1,P1) then Continue;
      if not GetNodePixPos(aLink.Node2,P2) then Continue;
      if not PtInRect(Window.MapRect,P1) and
         not PtInRect(Window.MapRect,P2) then Continue;
      DrawLink(P1,P2,aLink);
    end;
  end;
  Canvas.Pen.Width := 1;
end;


procedure TMap.DrawObject(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------
// Draws object identified by ObjType and Index on map.
//-----------------------------------------------------
var
  aRect : TRect;
  aLink : TLink;
  size  : Integer;
  offset: Integer;
  e     : Single;
  color : TColor;   {*** Added 5/11/18 ***}

begin
// Check if object falls within current display window
// (GetRect also gets global variables P1 & P2 that
// define the object's bounding rectangle).
  aRect := GetRect(ObjType,Index);
  if not IntersectRect(aRect,aRect,Window.MapRect) then Exit;

// Object is a Node
  if (ObjType in [JUNCS..TANKS]) then
  begin

  // Determine color & size
    SetNodeColor(ObjType,Index);
    size := SetNodeSize;

  // Draw the node
    case ObjType of
      JUNCS:
        if (not QueryFlag) or (LastColorIndex >= 0) then
        begin
          if  (Options.DispEmitters)
          and (MapZoomRatio >= Options.SymbolZoom)
          and (Uutils.GetSingle(Node(ObjType,Index).Data[JUNC_EMITTER_INDEX],e))
          and (e > 0)
          then DrawEmitter(P1.X,P1.Y,size)
          else DrawJunc(P1.X,P1.Y,size);
        end;
      RESERVS: DrawReserv(P1.X,P1.Y,size);
      TANKS:   DrawTank(P1.X,P1.Y,size);
    end;
    if (Options.DispSources) then DrawSource(ObjType,Index,P1.X,P1.Y);

  // Add notation if called for
    if (MapZoomRatio >= Options.NotationZoom) then
    begin
      if (Options.DispNodeIDs) then DrawNodeIDLabel(ObjType,Index,P1,size);
      if (Options.DispNodeValues) and (CurrentNodeVar > NOVIEW) then
        DrawNodeValueLabel(ObjType,Index,P1,size);
    end;
  end

// Object is a Link
  else if (ObjType in [PIPES..VALVES]) then
  begin

  // Determine color & size and draw the link
    aLink := Link(ObjType,Index);
    SetLinkColor(ObjType,Index);
    size := SetLinkSize;

    if Options.DispLinkBorder then
    begin
      color := Canvas.Pen.Color;
      if color <> ForeColor then
      begin
        Canvas.Pen.Color := ForeColor;
        if size = 4 then size := 3
        else if size = 5 then size := 6;
        Canvas.Pen.Width := size + 2;
        DrawLink(P1, P2, aLink);
        Canvas.Pen.Color := color;
      end;
    end;

    offset := size;
    Canvas.Pen.Width := size;
    DrawLink(P1,P2,aLink);
    GetLinkMidSegment(aLink, P1, P2);

  // Draw object's symbol & arrow if called for
    case ObjType of
    PIPES:
      if (Options.DispValves) and
      (UpperCase(aLink.Data[PIPE_STATUS_INDEX]) = 'CV') and
      (MapZoomRatio >= Options.SymbolZoom) then
      begin
        DrawLinkSymbol(P1,P2,lsCheckValve);
        offset := offset + SYMBOLSIZE;
      end
      else if (Options.ArrowStyle <> asNone) and RunFlag then
      begin
        DrawArrow(aLink,P1,P2);
        offset := offset + Options.ArrowSize div 2;
      end;
    PUMPS:
      if (Options.DispPumps) and (MapZoomRatio >= Options.SymbolZoom) then
      begin
        DrawLinkSymbol(P1,P2,lsPump);
        offset := offset + SYMBOLSIZE;
      end
      else if (Options.ArrowStyle <> asNone) and RunFlag then
      begin
        DrawArrow(aLink,P1,P2);
        offset := offset + Options.ArrowSize div 2;
      end;
    VALVES:
      if (Options.DispValves) and (MapZoomRatio >= Options.SymbolZoom) then
      begin
        DrawLinkSymbol(P1,P2,lsValve);
        offset := offset + SYMBOLSIZE;
      end
      else if (Options.ArrowStyle <> asNone) and RunFlag then
      begin
        DrawArrow(aLink,P1,P2);
        offset := offset + Options.ArrowSize div 2;
      end;
    end;

  // Add link notation if called for
    if (MapZoomRatio >= Options.NotationZoom) then
    begin
      if (Options.DispLinkIDs) then DrawLinkIDLabel(ObjType,Index,P1,P2,offset);
      if (Options.DispLinkValues) and (CurrentLinkVar > NOVIEW) then
        DrawLinkValueLabel(ObjType,Index,P1,P2,offset);
    end;
  end

  // Object is a Label
  else if (ObjType = LABELS) then
  begin
    with MapLabel(Index), Canvas do
    begin
      Font.Name := FontName;
      Font.Size := FontSize;
      Font.Style := [];
      if FontBold then Font.Style := Font.Style + [fsBold];
      if FontItalic then Font.Style := Font.Style + [fsItalic];
      if MeterType > 0 then DrawMeterLabel(Index)
      else Canvas.TextOut(P1.X,P1.Y,GetID(LABELS,Index));
    end;
  end;
end;


procedure TMap.DrawJunc(const X: Integer; const Y: Integer;
  const Size: Integer);
//------------------------------------------------------
// Draws a junction node at location X,Y with size Size
//------------------------------------------------------
begin
  Canvas.Ellipse(X-Size, Y-Size, X+Size+1, Y+Size+1);
end;


procedure TMap.DrawEmitter(const X: Integer; const Y: Integer;
  const Size: Integer);
//------------------------------------------------------
// Draws an emitter node at location X,Y with size Size
//------------------------------------------------------
var
  Poly: array[0..3] of TPoint;
  w: Integer;
begin
  w := Size + 1;
  Poly[0] := Point(X,Y-w);
  Poly[1] := Point(X-w,Y);
  Poly[2] := Point(X,Y+w);
  Poly[3] := Point(X+w,Y);
  Canvas.Polygon(Poly);
end;


procedure TMap.DrawReserv(const X: Integer; const Y: Integer;
  const Size: Integer);
//------------------------------------------------------
// Draws a reservoir node at location X,Y with size Size
//------------------------------------------------------
var
  Poly: array[0..3] of TPoint;
  w: Integer;
begin
  if (Options.DispTanks) and (MapZoomRatio >= Options.SymbolZoom) then
  begin
    //Canvas.Pen.Width := 2;
    w := 2*Size;
    Poly[0] := Point(X-w,Y-w);
    Poly[1] := Point(X-w,Y+Size+1);
    Poly[2] := Point(X+w+1,Y+Size+1);
    Poly[3] := Point(X+w+1,Y-w-1);
    Canvas.PolyLine(Poly);
    Canvas.Rectangle(X-w, Y-Size, X+w+2, Y+Size+2);
  end
  else
    Canvas.Rectangle(X-Size, Y-Size, X+Size+1, Y+Size+1);
end;


procedure TMap.DrawTank(const X: Integer; const Y: Integer;
  const Size: Integer);
//------------------------------------------------------
// Draws a tank node at location X,Y with size Size
//------------------------------------------------------
var
  w: Integer;
begin
  if (Options.DispTanks) and (MapZoomRatio >= Options.SymbolZoom) then
  begin
    w := 2*Size + 1;
    Canvas.Rectangle(X-Size, Y, X+Size+1, Y+w+1);
    Canvas.RoundRect(X-w, Y-w, X+w+1, Y+1, 3, 3);
  end
  else
    Canvas.Rectangle(X-Size, Y-Size, X+Size+1, Y+Size+1);
end;


procedure TMap.DrawSource(const ObjType, Index, X, Y: Integer);
//------------------------------------------------------
// Draws a WQ source symbol for object at position X,Y
//------------------------------------------------------
var
  k,w :         Integer;
  z   :         Single;
  oldcolor:     TColor;
begin
  if ObjType = RESERVS then k := RES_SRCQUAL_INDEX
  else if ObjType = TANKS then k := TANK_SRCQUAL_INDEX
  else k := JUNC_SRCQUAL_INDEX;
  try
    Uutils.GetSingle(Node(ObjType,Index).Data[k],z);
    if z > 0 then with Canvas do
    begin
      w := 4;
      oldcolor := Pen.Color;
      Pen.Color := ForeColor;
      Pen.Width := 3;
      MoveTo(X, Y-w);
      LineTo(X, Y+w);
      MoveTo(X-w, Y);
      LineTo(X+w, Y);
      Pen.Width := 1;
      Pen.Color := oldColor;
    end;
  except
  end;
end;


procedure TMap.DrawLink(const P1: TPoint; const P2: TPoint; aLink: TLink);
//----------------------------------------
// Draws a link between points P1 and P2.
//----------------------------------------
var
  aVertex: PVertex;
begin
  Canvas.MoveTo(P1.X,P1.Y);
  aVertex := aLink.Vlist;
  while aVertex <> nil do
  begin
    Canvas.LineTo(GetXPix(aVertex^.X),GetYpix(aVertex^.Y));
    aVertex := aVertex^.Next;
  end;
  Canvas.LineTo(P2.X,P2.Y);
end;


procedure TMap.DrawArrow(aLink: TLink; P1: TPoint; P2: TPoint);
//-------------------------------------------------------------
// Draws flow direction arrow on link between points P1 and P2.
//-------------------------------------------------------------
var
  k: Integer;
  Ptmp: TPoint;
begin
  if (MapZoomRatio < Options.ArrowZoom) then exit;
  k := aLink.Zindex;
  if (k >= 0) then
  case FlowDir^[k] of
  NONE: Exit;
  MINUS: begin
           Ptmp := P1;
           P1 := P2;
           P2 := Ptmp;
         end;
  end;
  DrawLinkSymbol(P1,P2,lsArrow);
end;


procedure TMap.DrawLinkSymbol(const P1: TPoint; const P2: TPoint;
  const Symbol: TLinkSymbol);
//-----------------------------------------------------
// Draws a symbol on the link between points P1 and P2.
//-----------------------------------------------------
var
  aSin,
  aCos   : Extended;
  dx, dy : Extended;
  x, y   : Integer;
  size   : Integer;
  width  : Integer;
begin
// Determine angle of inclination of symbol
  dy := P2.Y - P1.Y;
  dx := P2.X - P1.X;
  SinCos(arctan2(dy,dx), Asin, Acos);

// Determine location & size of symbol
  x := (P1.x + P2.X) div 2;
  y := (P1.Y + P2.Y) div 2;
  width := Canvas.Pen.Width;
  size := SYMBOLSIZE + width;

// Call symbol-specific drawing procedure
  case Symbol of

    lsArrow:
    begin
      with Options do
        DrawArrowHead(x,y,ArrowStyle,ArrowSize,0.5,Asin,Acos);
    end;

    lsValve:
    begin
    // Draw first half of valve
      DrawArrowHead(x,y,asFilled,size,1.0,Asin,Acos);
    // Draw second half of valve
      Asin := -Asin;
      Acos := -Acos;
      DrawArrowHead(x,y,asFilled,size,1.0,Asin,Acos);
    end;

    lsCheckValve:
    begin
    // Draw arrowhead symbol
      DrawArrowHead(x,y,asFilled,size,1.0,Asin,Acos);
    // Draw perpendicular line
      with Canvas do
      begin
        Pen.Width := MaxIntValue([width,2]);
        MoveTo(x + Round(Asin*size), y - Round(Acos*size));
        LineTo(x - Round(Asin*size), y + Round(Acos*size));
      end;
    end;

    lsPump:
    begin
      if (dx >= 0) then
        DrawPumpSymbol(x,y,size,1,Asin,Acos)
      else
        DrawPumpSymbol(x,y,size,-1,Asin,Acos);
    end;
  end;
  Canvas.Pen.Width := width;
end;


procedure TMap.DrawArrowHead(const X: Integer; const Y: Integer;
  const Style: TArrowStyle; const Size: Integer;
  const Aratio: Single; const Asin: Extended; const Acos: Extended);
//------------------------------------------------------------------
// Draws arrowhead symbol starting from position X,Y.
// Style = arrowhead style,
// Size  = arrowhead style,
// Aratio = ratio of arrowhead width to length
// Asin, Acos = sine & cosine of angle of inclination of arrowhead.
//------------------------------------------------------------------
var
  x1, x2: Integer;
  y1, y2: Integer;
  poly  : array[0..3] of TPoint;
begin
  x1 := X + Round((-Acos+Aratio*Asin)*Size);
  y1 := Y - Round((Asin+Aratio*Acos)*Size);
  x2 := X + Round((-Acos-Aratio*Asin)*Size);
  y2 := Y - Round((Asin-Aratio*Acos)*Size);
  case Style of
    asOpen:
    begin
      poly[0] := Point(x1,y1);
      poly[1] := Point(X,Y);
      poly[2] := Point(x2,y2);
      Canvas.PolyLine(Slice(poly,3));
    end;
    asFilled:
    begin
      poly[0] := Point(X,Y);
      poly[1] := Point(x1,y1);
      poly[2] := Point(x2,y2);
      Canvas.Pen.Width := 1;
      Canvas.Polygon(Slice(poly,3));
    end;
    asFancy:
    begin
      poly[0] := Point(X,Y);
      poly[1] := Point(x1,y1);
      x1 := X + Round((0.67*Acos)*Size);
      y1 := Y - Round((-0.67*Asin)*Size);
      poly[2] := Point(x1,y1);
      poly[3] := Point(x2,y2);
      Canvas.Pen.Width := 1;
      Canvas.Polygon(Slice(poly,4));
    end;
  end;
end;


procedure TMap.DrawPumpSymbol(const X: Integer; const Y: Integer;
  const Size: Integer; const Direction: Integer;
  const Asin: Extended; const Acos: Extended);
//------------------------------------------------------------------
// Draws pump symbol centered at position X,Y.
// Size  = arrowhead style,
// Direction = orientation of pump
// Asin, Acos = sine & cosine of angle of inclination of pump.
//------------------------------------------------------------------
var
  xi, yi, r : Integer;
  aColor    : TColor;
  poly      : array[0..3] of TPoint;
begin
  aColor := Canvas.Pen.Color;
  if Options.DispNodeBorder then
    Canvas.Pen.Color := MapForeColor[Options.ColorIndex];
  r := 2*Size;
  poly[0] := Point(X,Y);
  xi := X + Round(Acos*r);
  yi := Y + Round(Asin*r);
  poly[1] := Point(xi,yi);
  xi := X + Round((Acos + 0.5*Asin*Direction)*r);
  yi := Y + Round((Asin - 0.5*Acos*Direction)*r);
  poly[2] := Point(xi,yi);
  xi := X + Round(+0.5*Asin*r*Direction);
  yi := Y + Round(-0.5*Acos*r*Direction);
  poly[3] := Point(xi,yi);
  Canvas.Polygon(Slice(poly,4));
  Canvas.Ellipse(X-Size,Y-Size,X+Size,Y+Size);
  Canvas.Pen.Color := aColor;
end;


procedure TMap.DrawNodeIDLabel(const ObjType: Integer; const Index: Integer;
  const P: TPoint; const Size: Integer);
//------------------------------------------
// Draws text of node ID label next to node.
//------------------------------------------
var
  color : TColor;
  offset: Integer;
begin
// Increase offset distance from node for tanks & reservoirs
  offset := Size + CharHeight + 1;
  if (ObjType <> JUNCS) and (Options.DispTanks)
  and (MapZoomRatio >= Options.SymbolZoom) then
    offset := offset + Size + 1;
  with Canvas do
  begin
    color := Brush.Color;
    Brush.Style := bsSolid;
    Brush.Color := BackColor;
    Font.Color := GrayColor;
    if Options.NotationTranspar then SetBkMode(Handle,TRANSPARENT);
    TextOut(P.X,P.Y-offset,GetID(ObjType,Index));
    Brush.Color := color;
  end;
end;


procedure TMap.DrawNodeValueLabel(const ObjType: Integer; const Index: Integer;
  const P: TPoint; const Size: Integer);
//----------------------------------------------------------------
// Draws text of value of current node view variable next to node.
//----------------------------------------------------------------
var
  color : TColor;
  offset: Integer;
  s: String;
begin
  s := Uoutput.GetNodeValStr(CurrentNodeVar,CurrentPeriod,ObjType,Index);
  if (Length(s) = 0) then Exit;
  offset := Size + 2;
  if (ObjType = TANKS) and (Options.DispTanks)
  and (MapZoomRatio >= Options.SymbolZoom) then
    offset := offset + Size;
  with Canvas do
  begin
    color := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := color;               {*** Updated 3/1/01 ***}
    if Options.NotationTranspar then SetBkMode(Handle,TRANSPARENT);
    TextOut(P.X,P.Y+offset,s);
    Brush.Color := color;
  end;
end;


procedure TMap.DrawLinkIDLabel(const ObjType: Integer; const Index: Integer;
  const P1: TPoint; const P2: TPoint; const Size: Integer);
//-------------------------------------------------
// Draws text of link ID label at midpoint of link.
//-------------------------------------------------
var
  color : TColor;
  s     : String;
  x,y   : Integer;
begin
  s := GetID(ObjType,Index);
  x := (P1.X + P2.X) div 2;
  y := (P1.Y + P2.Y) div 2;
  if (Abs(P2.Y - P1.Y) < Size) then
  begin
    x := x - (Canvas.TextWidth(s) div 2);
    y := y - Size - CharHeight;
  end
  else
  begin
    x := x - Canvas.TextWidth(s) - Size;
    if ((P1.Y < P2.Y) and (P1.X < P2.X)) then
      y := y + Size
    else
      y := y - Size - CharHeight;
  end;
  with Canvas do
  begin
    color := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;
    if Options.NotationTranspar then SetBkMode(Handle,TRANSPARENT);
    TextOut(x,y,s);
    Brush.Color := color;
  end;
end;


procedure TMap.DrawLinkValueLabel(const ObjType: Integer; const Index: Integer;
  const P1: TPoint; const P2: TPoint; const Size: Integer);
//--------------------------------------------------------------------
// Draws text of value of current link view variable at link midpoint.
//--------------------------------------------------------------------
var
  color : TColor;
  s     : String;
  x,y   : Integer;
begin
  s := Uoutput.GetLinkValStr(CurrentLinkVar,CurrentPeriod,ObjType,Index);
  if (Length(s) = 0) then Exit;
  x := (P1.X + P2.X) div 2;
  y := (P1.Y + P2.Y) div 2;
  if (Abs(P2.Y - P1.Y) < Size) then
  begin
    x := x - (Canvas.TextWidth(s) div 2);
    y := y + Size;
  end
  else
  begin
    x := x + Size;
    if ((P1.Y < P2.Y) and (P1.X < P2.X)) then
      y := y - Size - CharHeight
    else
      y := y + Size;
  end;
  with Canvas do
  begin
    color := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := Pen.Color;
    if Options.NotationTranspar then SetBkMode(Handle,TRANSPARENT);
    TextOut(x,y,s);
    Brush.Color := color;
  end;
end;


procedure TMap.DrawMeterLabel(const Index: Integer);
//---------------------------------------------------
// Draws a Map Label which serves as a meter
//---------------------------------------------------
var
  S1, S2: String;
  aLabel: TMapLabel;
  aRect : TRect;
  Found : Boolean;
  ObjType: Integer;
  ObjIndex: Integer;
begin
// If object being metered cannot be found then just display map label.
  aLabel := MapLabel(Index);
  if aLabel.MeterType = NETNODES then
    Found := Uinput.FindNode(aLabel.MeterID, ObjType, ObjIndex)
  else if aLabel.MeterType = NETLINKS then
    Found := Uinput.FindLink(aLabel.MeterID, ObjType, ObjIndex)
  else Found := False;
  if not Found then
  begin
    aLabel.MeterText := '';
    Canvas.TextOut(P1.X,P1.Y,GetID(LABELS,Index));
    Exit;
  end;

// Get the text strings to display in the meter
  Uoutput.GetMeterLabel(ObjType, ObjIndex, S1, S2);
  S1 := GetID(LABELS,Index);
  aLabel.MeterText := S2;

// Get the meter's bounding rectangle
  aRect := GetRect(LABELS,Index);
  with Canvas do
  begin

  // Change canvas properties to display the meter
    Brush.Color := clInfoBk;
    Pen.Color := clBlack;
    Font.Color := clBlack;

  // Draw bounding rectangle with meter text inside it
    RoundRect(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom,15,15);
    with aRect do
    begin
      TextOut(Left + (Right-Left-TextWidth(S1)) div 2, Top+2, S1);
      if Length(S2) > 0 then
        TextOut(Left + (Right-Left-TextWidth(S2)) div 2, Top+TextHeight(S1)+2, S2);
    end;

  // Restore global canvas properties
    Brush.Color := BackColor;
    Pen.Color := ForeColor;
    Font.Color := ForeColor;
  end;
end;


procedure TMap.SetLinkColor(const ObjType: Integer; const Index: Integer);
//----------------------------------------------
// Sets the color to be used for drawing a link.
//----------------------------------------------
var
  K      : Integer;
  aColor : TColor;
begin
// Retrieve the color stored with the link's object in the network database
  K := -1;
  if (CurrentLinkVar = NOVIEW) then
    aColor := MapGrayColor[Options.ColorIndex]
  else
  begin
    K := Link(ObjType,Index).ColorIndex;
    if K < 0 then aColor := MapGrayColor[Options.ColorIndex]
    else if QueryFlag then aColor := QueryColor
    else aColor := MapLinkColor[K];
  end;

// If this color is different than the last one used to draw with
// then update the color used to draw with.
  if K <> LastColorIndex then
  begin
    Canvas.Pen.Color := aColor;
    Canvas.Brush.Color := aColor;
    LastColorIndex := K;
  end;
end;


procedure TMap.SetNodeColor(const ObjType: Integer; const Index: Integer);
//----------------------------------------------
// Sets the color to be used for drawing a node.
//----------------------------------------------
var
  K      : Integer;
  aColor : TColor;
begin
// Retrieve the color stored with the node's object in the network database
  K := -1;
  if (CurrentNodeVar = NOVIEW) then
    aColor := MapGrayColor[Options.ColorIndex]
  else
  begin
    K := Node(ObjType,Index).ColorIndex;
    if K < 0 then aColor := MapGrayColor[Options.ColorIndex]
    else if QueryFlag then aColor := QueryColor
    else aColor := MapNodeColor[K];
  end;

// If this color is different than the last one used to draw with
// then update the color used to draw with.
  if K <> LastColorIndex then
  begin
    if Options.DispNodeBorder then
      Canvas.Pen.Color := MapForeColor[Options.ColorIndex]
    else Canvas.Pen.Color := aColor;
    Canvas.Brush.Color := aColor;
    LastColorIndex := K;
  end;
end;


function TMap.SetLinkSize: Integer;
//----------------------------------------
// Sets the size used to draw a link with.
//----------------------------------------
begin
  with Options do
    if DispLinksBySize then
      Result := LinkSize + LastColorIndex
    else Result := LinkSize;
end;


function TMap.SetNodeSize: Integer;
//----------------------------------------
// Sets the size used to draw a node with.
//----------------------------------------
begin
  with Options do
    if DispNodesBySize then
      Result := NodeSize + LastColorIndex
    else Result := NodeSize;
end;


//===================================================================
//                    Object Location Methods
//===================================================================

procedure UpdateBounds(var R: TRect; const P: TPoint);
//----------------------------------------------------
// Updates the size of rectangle R to include point P.
//----------------------------------------------------
begin
  if P.X < R.Left   then R.Left := P.X;
  if P.X > R.Right  then R.Right := P.X;
  if P.Y < R.Top    then R.Top := P.Y;
  if P.Y > R.Bottom then R.Bottom := P.Y;
end;


function TMap.GetRect(const ObjType: Integer; const Index: Integer): TRect;
//-----------------------------------------------------------
// Returns bounding rectangle for item Index of type ObjType.
// Saves pixel locations of top-left & bottom-right points in
// global variables P1 and P2.
//-----------------------------------------------------------
begin
  case ObjType of
    JUNCS..TANKS:  Result := GetNodeRect(ObjType,Index);
    PIPES..VALVES: Result := GetLinkRect(ObjType,Index);
    LABELS:        Result := GetLabelRect(Index);
  end;
end;


function TMap.GetNodeRect(const ObjType, Index: Integer): TRect;
//--------------------------------------------------------------
// Gets bounding rectangle for a node. Rectangle is centered
// at node's pixel position and extends a distance on all sides
// equal to the node size option plus the height of notation
// labels (CharHeight).
//--------------------------------------------------------------
var
  Pbuf : Integer;
begin
   Pbuf := Options.NodeSize + CharHeight;
   P1 := GetNodePoint(ObjType,Index);
   Result := Rect(P1.X-Pbuf,P1.Y-Pbuf,P1.X+Pbuf+1,P1.Y+Pbuf+1);
end;


function TMap.GetLinkRect(const ObjType, Index: Integer): TRect;
//--------------------------------------------------------------
// Gets bounding rectangle for a link. Rectangle equals pixel
// extent of all nodes & vertices that comprise the link and is
// then enlarged by the size of link symbols (or flow arrows)
// plus the link's width.
//--------------------------------------------------------------
var
  Pbuf          : Integer;
  P1a           : TPoint;
  R             : TRect;
  aLink         : TLink;
  aVertex       : PVertex;
begin
  Pbuf := MaxIntValue([SYMBOLSIZE,Options.ArrowSize]) + Options.LinkSize;
  Result := NORECT;
  aLink := Link(ObjType,Index);
  if not GetNodePixPos(aLink.Node1,P1) then Exit;
  if not GetNodePixPos(aLink.Node2,P2) then Exit;
  R := Rect(P1.x, P1.y, P1.x+1, P1.y+1);
  aVertex := aLink.Vlist;
  while aVertex <> nil do
  begin
    P1a := Point(GetXPix(aVertex^.X),GetYpix(aVertex^.Y));
    UpdateBounds(R,P1a);
    aVertex := aVertex^.Next;
  end;
  UpdateBounds(R,P2);
  OffsetRect(R, -Pbuf, -Pbuf);
  InflateRect(R,2*Pbuf,2*Pbuf);
  Result := R;
end;


function TMap.GetLabelRect(const Index: Integer): TRect;
//------------------------------------------------------
// Gets bounding rectangle for a label.
//------------------------------------------------------
var
  H, W          : Integer;
  Xa, Ya        : Extended;
  aFont         : TFont;
  aLabel        : TMapLabel;
  Lsize         : TSize;
begin
    aLabel := MapLabel(Index);
    with aLabel do
    begin

  // Find world coordinates of label's anchor node
  // (if no anchor node then these are same coordinates as label)
      if Anchor = nil then
      begin
        Xa := X;
        Ya := Y;
      end
      else
      begin
        Xa := Anchor.X;
        Ya := Anchor.Y;
      end;

  // Find pixel coordinates of upper left of label's bounding rectangle
      with Window do
      begin
        if ZoomIndex = 0 then
        begin
          P1.X := GetXpix(X);
          P1.Y := GetYpix(Y);
        end
        else
        begin
          P1.X := GetXpix(Xa) + Round((X-Xa)*ZoomFactor[0]);
          P1.Y := GetYpix(Ya) + Round((Ya-Y)*ZoomFactor[0]);
        end;
      end;
    end;

// Create a Font object to determine label's width & height
    aFont := TFont.Create;
    try
      aFont.Assign(Canvas.Font);
      Uinput.GetLabelFont(Index,Canvas.Font);
      Lsize := Canvas.TextExtent(Network.Lists[LABELS].Strings[Index]);
      W := Lsize.cx;
      H := Lsize.cy;
      if aLabel.MeterType <> 0 then
      begin
        Lsize := Canvas.TextExtent(aLabel.MeterText);
        W := MaxIntValue([W,Lsize.cx]) + 7;
        H := H + Lsize.cy + 4;
      end;
      Canvas.Font.Assign(aFont);
    finally
      aFont.Free;
    end;

// Construct the label's bounding rectangle
    Result := Rect(P1.X,P1.Y,P1.X+W,P1.Y+H);
end;


function TMap.GetAdjacencyRect(const ObjType: Integer; const Index: Integer;
  const DeleteLinks: Boolean): TRect;
//----------------------------------------------------------------
// Finds rectangle encompassing all nodes adjacent to given node.
// If DeleteLinks = True, then connecting links are deleted (as
// when the node is being deleted from the network database).
//----------------------------------------------------------------
var
  I, J   : Integer;
  aRect  : TRect;
  Links  : TStringList;
  aNode  : TNode;
begin
  Result := GetRect(ObjType,Index);
  aNode := Node(ObjType,Index);
  if (Options.DispNodeIDs) then GetNodeIDRect(ObjType,Index,Result);
  if (Options.DispLabels) and (Window.ZoomIndex > 0) then
    GetAnchoredLabelsRect(aNode,Result);
  for I := PIPES to VALVES do
  begin
    Links := Network.Lists[I];
    for J := Links.Count-1 downto 0 do
    begin
      if (Link(I,J).Node1 = aNode) or
         (Link(I,J).Node2 = aNode) then
      begin
        aRect := GetRect(I,J);
        if IntersectRect(aRect,aRect,Window.MapRect) then
          UnionRect(Result,Result,aRect);
        if DeleteLinks then
        begin
          Uinput.DeleteLabelMeter(NETLINKS,GetID(I,J));
          DeleteNetworkObject(I,J);
        end;
      end;
    end;
  end;
end;


procedure TMap.GetAnchoredLabelsRect(aNode: TNode; var aRect: TRect);
//-------------------------------------------------------------------
// Expands the bounding rectangle aRect of node aNode to include
// any labels anchored to aNode.
//-------------------------------------------------------------------
var
  i: Integer;
begin
  for i := 0 to Network.Lists[LABELS].Count - 1 do
  begin
    if MapLabel(i).Anchor = aNode then UnionRect(aRect,aRect,GetLabelRect(i));
  end;
end;


procedure TMap.GetLinkMidSegment(aLink: TLink; var Pa, Pb: TPoint);
//-----------------------------------------------------------------
// Gets pixel coordinates of link vertices that define the midpoint
// segment of a link (aLink).
//-----------------------------------------------------------------
var
  m, n   : Integer;
  Va, Vb : PVertex;
begin
  n := aLink.GetVertexCount;
  if n > 0 then
  begin
    n := (n div 2) + 1;
    Va := nil;
    Vb := aLink.Vlist;
    for m := 2 to n do
    begin
      Va := Vb;
      Vb := Vb^.Next;
    end;
    if Va <> nil then
      Pa := Point(GetXPix(Va^.X), GetYpix(Va^.Y));
    if Vb <> nil then
      Pb := Point(GetXPix(Vb^.X), GetYpix(Vb^.Y));
  end;
end;


function TMap.GetLinkMidpoint(aLink: TLink; var P: TPoint): Boolean;
//-------------------------------------------------------------------
// Finds the pixel coordinates (P) of the midpoint of a link (aLink).
//-------------------------------------------------------------------
var
  Pa, Pb: TPoint;
begin
  Result := False;
  if not GetNodePixPos(aLink.Node1, Pa)
  or not GetNodePixPos(aLink.Node2, Pb) then Exit;
  GetLinkMidSegment(aLink,Pa,Pb);
  P.X := (Pa.X + Pb.X) div 2;
  P.Y := (Pa.Y + Pb.Y) div 2;
  Result := True;
end;


procedure TMap.GetNodeIDRect(const NodeType: Integer;
  const Index: Integer; var aRect: TRect);
//------------------------------------------------------
// Modifies a node's bounding rectangle (aRect) to
// include the area needed to display its ID label.
//-----------------------------------------------------
var
  s: String;
begin
  s := GetID(NodeType,Index);
  aRect.Top := aRect.Top - CharHeight - Canvas.TextHeight(s);
  aRect.Right := aRect.Right + Canvas.TextWidth(s);
end;


function TMap.GetNodePoint(const NodeType: Integer;
  const Index: Integer): TPoint;
//------------------------------------------------------
// Returns X,Y pixel location of node object.
//------------------------------------------------------
var
  X,Y : Extended;
begin
  X := Node(NodeType,Index).X;
  Y := Node(NodeType,Index).Y;
  if (X = MISSING) or (Y = MISSING) then
    Result := NOPOINT
  else
    Result := Point(GetXpix(X),GetYpix(Y));
end;


function TMap.GetNodePixPos(aNode: TNode; var P: TPoint): Boolean;
//------------------------------------------------------------
// Finds pixel coordinates (P) of a node (aNode).
// Returns False if node's coordinates are missing.
//------------------------------------------------------------
begin
  P := NOPOINT;
  Result := False;
  if aNode = nil then Exit;
  if (aNode.X = MISSING) or (aNode.Y = MISSING) then Exit;
  P := Point(GetXpix(aNode.X),GetYpix(aNode.Y));
  Result := True;
end;


function TMap.GetXpix(const X: Extended):Integer;
//------------------------------------------------
// Converts world coordinate X to pixel value.
//------------------------------------------------
begin
  with Window do
  try
    Result := Round( (X-Woffset.X)*PPW + Poffset.X);
  except
    On EInvalidOp do Result := MAX_INT_COORD;
  end;
end;


function TMap.GetYpix(const Y: Extended):Integer;
//------------------------------------------------
// Converts world coordinate Y to pixel value.
//------------------------------------------------
begin
  with Window do
  try
    Result := Round( Poffset.Y - (Y-Woffset.Y)*PPW);
  except
    On EInvalidOp do Result := MAX_INT_COORD;
  end;
end;


function  TMap.GetX(const X: Integer): Extended;
begin
  with Window do
    Result := (X - Poffset.X) / PPW + Woffset.X;
end;


function  TMap.GetY(const Y: Integer): Extended;
begin
  with Window do
    Result := (Poffset.Y - Y) / PPW + Woffset.Y;
end;


function TMap.IsBounded(const A, B, C: Extended): Boolean;
//------------------------------------------------
// Determines if C is between A and B
//------------------------------------------------
begin
  if (C < A) or (C > B) then Result := False
  else Result := True;
end;


//===================================================================
//                Map Resizing & Re-scaling Methods
//===================================================================

procedure TMap.Resize(const Rect: TRect);
//----------------------------------------
// Resizes map's display window & bitmaps.
//----------------------------------------
begin
  ResizeWindow(Rect);
  ResizeBitmap(Bitmap,Window.Pwidth,Window.Pheight);
  ResizeBitmap(BackBM,Window.Pwidth,Window.Pheight);
end;


procedure TMap.ResizeWindow(const Rect: TRect);
begin
  with Window do
  begin
    MapRect := Rect;
    Pwidth := MapRect.Right - MapRect.Left;
    Pheight := MapRect.Bottom - MapRect.Top;
    Poffset.X := MapRect.Left;
    Poffset.Y := Pheight + MapRect.Top;
  end;
end;


procedure TMap.ResizeBitmap(var aBitmap: TBitmap; const W,H: Integer);
begin
  if aBitmap <> nil then
  begin
    aBitmap.Width := W;
    aBitmap.Height := H;
  end;
end;


procedure TMap.Rescale(const NewDimensions: TMapDimensions);
//-------------------------------------------------
// Resets map's dimensions and scale factors.
//-------------------------------------------------
var
  SFx, SFy : Extended;
  Dx, Dy   : Extended;
begin
  Dimensions.LowerLeft := NewDimensions.LowerLeft;
  Dimensions.UpperRight := NewDimensions.UpperRight;
  with Dimensions, Window do
  begin
    Dx := UpperRight.X - LowerLeft.X;
    Dy := UpperRight.Y - LowerLeft.Y;
    SFx := Pwidth/Dx;
    SFy := Pheight/Dy;
    if SFy < SFx then PPW := SFy
    else PPW := SFx;
    ZoomIndex := 0;
    ZoomFactor[ZoomIndex] := PPW;
    Woffset.X := LowerLeft.X - (Pwidth/PPW - Dx)/2;
    Woffset.Y := LowerLeft.Y - (Pheight/PPW - Dy)/2;
  end;
end;


//===================================================================
//                          Legend Handlers
//===================================================================

procedure DrawLegend(const R: TRect;  const C: TCanvas;
  const BackColor: TColor; const Framed: Boolean; const T: String;
  const Legend: TMapLegend; const MapColor: array of TColor);
//------------------------------------------------------------
// Draws map legend Legend with colors MapColor in rectangle
// R on canvas C with background color BackColor and title T.
//------------------------------------------------------------
var
  d, x, y  : Integer;
  digits   : Integer;
  i, dy    : Integer;
  w,n      : Integer;
  units    : String;
  bordercolor: TColor;

begin
  with C, Legend do
  begin

  // Determine units & decimal digits
    if Ltype = NETNODES then
    begin
      units := NodeUnits[ViewVar].Units;
      digits := NodeUnits[ViewVar].Digits;
    end
    else
    begin
      units := LinkUnits[ViewVar].Units;
      digits := LinkUnits[ViewVar].Digits;
    end;

  // Determine width
    Font.Name := 'Arial';
    Font.Size := 8;
    if BoldFonts then Font.Style := [fsBold];
    dy := TextHeight('[');
    d := dy div 2;
    w := TextWidth(T);
    for i := 1 to Nintervals do
    begin
      n := TextWidth(FloatToStrF(Intervals[i], ffFixed, 7, digits));
      if n > w then w := n;
    end;

  // Clear canvas background.
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    bordercolor := clBlack;
    if BackColor = clBlack then bordercolor := clWhite;
    Pen.Color := bordercolor;
    Font.Color := bordercolor;
    if Framed then Rectangle(R.Left,R.Top,R.Right,R.Bottom)
    else FillRect(R);

  // Draw title
    SetBkMode(Handle,TRANSPARENT);
    x := R.Left + 3*d;
    y := R.Top + d;
    TextOut(x, y, T);
    y := y + d;

  // Draw color bars
    Pen.Color := clBlack;
    for i := 0 to Nintervals do
    begin
      x := R.Left + d;
      Brush.Color := MapColor[i];
      Rectangle(x,y,x+d,y+d+dy);
      y := y + d + dy - 1;
    end;

  // Draw scale labels
    Brush.Style := bsClear;
    x := R.Left + d + 2*d;
    y := R.Top + 2*dy;
    for i := 1 to Nintervals do
    begin
      TextOut(x, y, FloatToStrF(Intervals[i], ffFixed, 7, digits));
      y := y + dy + d - 1;
    end;

  // Draw units label
    TextOut(x, y, units);
  end;
end;


function EditLegend(var Legend: TMapLegend; const TimePeriod: Integer;
  var Colors: array of TColor; var Framed: Boolean): Boolean;
//----------------------------------------------------------------
// Launches Legend dialog box to modify map colors and intervals.
//----------------------------------------------------------------
var
  Sname, Sunits: String;
begin
// Determine name & units of legend
  Result := True;
  with Legend do
  begin
    if (Ltype = NETNODES) then
    begin
      Sname := NodeVariable[ViewVar].Name;
      Sunits := NodeUnits[ViewVar].Units;
    end
    else
    begin
      Sname := LinkVariable[ViewVar].Name;
      Sunits := LinkUnits[ViewVar].Units;
    end;
  end;

// Create Legend dialog form at top left of screen
  with TLegendForm.Create(Application) do
  try
    LoadData(Sname,Sunits,TimePeriod,Legend,Colors,Framed);

  // Show form
    if ShowModal = mrOK then UnloadData(Legend,Colors,Framed)
    else Result := False;
  finally
    Free;
  end;
end;


procedure DrawTimeLegend(R: TRect; C: TCanvas;
  const BackColor: TColor; const S: String);
//------------------------------------------
// Draws Time Legend text S on map canvas C.
//------------------------------------------
var
  Buff: array[0..255] of Char;
  BorderColor: TColor;
begin
  with C do
  begin

  // Set brush & pen properties
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    BorderColor := clBlack;
    if BackColor = clBlack then BorderColor := clWhite;
    Pen.Color := BorderColor;

  // Draw time legend text
    SetBkMode(Handle,TRANSPARENT);
    Font.Name := 'Arial';
    Font.Style := [fsBold];
    Font.Size := 8;
    Font.Color := Pen.Color;
    StrPCopy(Buff, S);
    DrawText(Handle, PChar(S), -1, R, DT_CENTER or DT_VCENTER);
  end;
end;

end.
