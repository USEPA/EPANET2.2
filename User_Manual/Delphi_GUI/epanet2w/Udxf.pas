unit Udxf;

{-------------------------------------------------------------------}
{                    Unit:    Udxf.pas                              }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that exports the Network Map to a file       }
{   in DXF format by calling the DXFexport procedure.               }
{-------------------------------------------------------------------}

interface

uses SysUtils, Windows, Graphics, Math, Uglobals;

procedure DXFexport(const fname: String; const Jstyle: Integer);

implementation

uses Fmap,Umap;

const
  DXFcolors  : array[1..7] of TColor =
               (clRed, clYellow, clLime, clAqua,
                clBlue, clFuchsia, clBlack);
  StartTable : array[1..31] of String =
               ('0','SECTION','2','TABLES','0','TABLE',
                '2','LTYPE','70','1','0','LTYPE','2',
                'CONTINUOUS','70','64','3','SOLID LINE',
                '72','65','73','0','40','0.0','0','ENDTAB',
                '0','TABLE','2','LAYER','70');
  EndTable   : array[1..16] of String =
               ('0','ENDTAB','0','TABLE','2','VIEW','70','0',
                '0','ENDTAB','0','ENDSEC','0','SECTION','2','ENTITIES');

var
  DXFfile   : TextFile;
  JuncStyle : Integer;
  LinkSize  : Single;
  NodeSize  : Single;
  LabelSize : Single;
  ArrowSize : Single;
  Map       : TMap;
  MapOptions: TMapOptions;
  PixPerMapExt: Single;
  DXFLinkColor: array[0..MAXINTERVALS] of Integer;
  DXFNodeColor: array[0..MAXINTERVALS] of Integer;


function GetDXFColor(aColor : TColor): Integer;
var
  diff    : LongInt;
  maxdiff : LongInt;
  i, imax : Integer;
begin
  imax := 1;
  maxdiff := High(LongInt);
  for i := Low(DXFColors) to High(DXFColors) do
  begin
    diff := Abs(aColor - DXFColors[i]);
    if diff < maxdiff then
    begin
      maxdiff := diff;
      imax := i;
    end
  end;
  Result := imax;
end;


procedure AddHeader(const xmin,ymin,xmax,ymax: Single);
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'SECTION');
  Writeln(DXFfile,'2');
  Writeln(DXFfile,'HEADER');
  Writeln(DXFfile,'9');
  Writeln(DXFfile,'$EXTMIN');
  Writeln(DXFfile,'10');
  Writeln(DXFfile,xmin);
  Writeln(DXFfile,'20');
  Writeln(DXFfile,ymin);
  Writeln(DXFfile,'9');
  Writeln(DXFfile,'$EXTMAX');
  Writeln(DXFfile,'10');
  Writeln(DXFfile,xmax);
  Writeln(DXFfile,'20');
  Writeln(DXFfile,ymax);
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'ENDSEC');
end;


procedure StartTables(Nlayers: Integer);
var
  i : Integer;
begin
  for i := 1 to High(StartTable) do
    Writeln(DXFfile,StartTable[i]);
  Writeln(DXFfile,Nlayers);
end;


procedure AddLayer(layer: String; color: Integer);
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'LAYER');
  Writeln(DXFfile,'2');
  Writeln(DXFfile,layer);
  Writeln(DXFfile,'70');
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'62');
  Writeln(DXFfile,color);
  Writeln(DXFfile,'6');
  Writeln(DXFfile,'CONTINUOUS');
end;


procedure EndTables;
var
  i : Integer;
begin
  for i := 1 to High(EndTable) do
    Writeln(DXFfile,EndTable[i]);
end;


procedure EndDXF;
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'ENDSEC');
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'EOF');
end;

procedure AddCircle(const x,y   : Single;
                    const radius: Single;
                    const color : Integer;
                    const layer : String);
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'CIRCLE');
  Writeln(DXFfile,'8');
  Writeln(DXFfile,layer);
  Writeln(DXFfile,'10');
  Writeln(DXFfile,x);
  Writeln(DXFfile,'20');
  Writeln(DXFfile,y);
  Writeln(DXFfile,'40');
  Writeln(DXFfile,radius);
  if color > 0 then
  begin
    Writeln(DXFfile,'62');
    Writeln(DXFfile,color);
  end;
end;

procedure AddVertex(const x, y, bulge: Single; const layer: String);
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'VERTEX');
  Writeln(DXFfile,'8');
  Writeln(DXFfile,layer);
  Writeln(DXFfile,'10');
  Writeln(DXFfile,x);
  Writeln(DXFfile,'20');
  Writeln(DXFfile,y);
  Writeln(DXFfile,'30');
  Writeln(DXFfile,'0.0');
  Writeln(DXFfile,'42');
  Writeln(DXFfile,bulge);
end;

procedure AddLine(const x1,y1,x2,y2: Single;
                  const thickness,
                        bulge      : Single;
                  const color      : Integer;
                  const layer      : String);
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'POLYLINE');   //LWPOLYLINE??
  Writeln(DXFfile,'8');
  Writeln(DXFfile,layer);
  Writeln(DXFfile,'66');
  Writeln(DXFfile,'1');

  if bulge > 0 then
  begin
    Writeln(DXFfile,'70');
    Writeln(DXFfile,'1');
  end;

  Writeln(DXFfile,'40');
  Writeln(DXFfile,thickness);
  Writeln(DXFfile,'41');
  Writeln(DXFfile,thickness);

  if color > 0 then
  begin
    Writeln(DXFfile,'62');
    Writeln(DXFfile,color);
  end;

  AddVertex(x1, y1, bulge, layer);
  AddVertex(x2, y2, bulge, layer);
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'SEQEND');
end;


procedure AddPolyLine(aLink: TLink;
                      const  thickness,
                             bulge     : Single;
                      const  color     : Integer;
                      const  layer     : String);
var
  aVertex: PVertex;
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'POLYLINE');   //LWPOLYLINE??
  Writeln(DXFfile,'8');
  Writeln(DXFfile,layer);
  Writeln(DXFfile,'66');
  Writeln(DXFfile,'1');

  if bulge > 0 then
  begin
    Writeln(DXFfile,'70');
    Writeln(DXFfile,'1');
  end;

  Writeln(DXFfile,'40');
  Writeln(DXFfile,thickness);
  Writeln(DXFfile,'41');
  Writeln(DXFfile,thickness);

  if color > 0 then
  begin
    Writeln(DXFfile,'62');
    Writeln(DXFfile,color);
  end;

  AddVertex(aLink.Node1.X, aLink.Node1.Y, bulge, layer);
  aVertex := aLink.Vlist;
  while aVertex <> nil do
  begin
    AddVertex(aVertex^.X, aVertex^.Y, bulge, layer);
    aVertex := aVertex^.Next;
  end;
  AddVertex(aLink.Node2.X, aLink.Node2.Y, bulge, layer);
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'SEQEND');
end;


procedure AddSolid(const x: array of Single;
                   const y: array of Single;
                   const color: Integer;
                   const layer: String);
var
  i : Integer;
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'SOLID');
  Writeln(DXFfile,'8');
  Writeln(DXFfile,layer);
  for i := 0 to 3 do
  begin
    Writeln(DXFfile,IntToStr(10+i));
    Writeln(DXFfile,x[i]);
    Writeln(DXFfile,IntToStr(20+i));
    Writeln(DXFfile,y[i]);
    Writeln(DXFfile,IntToStr(30+i));
    Writeln(DXFfile,'0.0');
  end;
  if color > 0 then
  begin
    Writeln(DXFfile,'62');
    Writeln(DXFfile,color);
  end;
end;


procedure AddText(const x,y,ht,rot : Single;
                  const txt,layer  : String);
begin
  Writeln(DXFfile,'0');
  Writeln(DXFfile,'TEXT');
  Writeln(DXFfile,'8');
  Writeln(DXFfile,layer);
  Writeln(DXFfile,'10');
  Writeln(DXFfile,x);
  Writeln(DXFfile,'20');
  Writeln(DXFfile,y);
  Writeln(DXFfile,'30');
  Writeln(DXFfile,'0.0');
  Writeln(DXFfile,'40');
  Writeln(DXFfile,ht);
  Writeln(DXFfile,'1');
  Writeln(DXFfile,txt);
  Writeln(DXFfile,'50');
  Writeln(DXFfile,rot);
end;


procedure AddArrow(const x1,y1,size: Single;
                   const asin,acos: Extended;
                   const color: Integer;
                   const layer: String);
var
  asize  : Single;
  x      : array[0..2] of Single;
  y      : array[0..2] of Single;
begin
  asize := size*PixPerMapExt*ArrowSize/2.0;
  x[0] := x1;
  y[0] := y1;
  x[1] := x1 + (-acos - 0.5*asin)*asize;
  y[1] := y1 + (-asin + 0.5*acos)*asize;
  x[2] := x1 + (-acos + 0.5*asin)*asize;
  y[2] := y1 + (-asin - 0.5*acos)*asize;
  AddLine(x[0],y[0],x[1],y[1],size,0.0,color,layer);
  AddLine(x[0],y[0],x[2],y[2],size,0.0,color,layer);
end;


procedure AddLink(const I,J: Integer);
var
  color, k  : Integer;
  symbolflag: Boolean;
  x1, y1    : Single;
  x2, y2    : Single;
  asin,acos : Extended;
  size      : Single;
  P1, P2    : TPoint;
  aLink     : TLink;
begin
  aLink := Link(I,J);
  with aLink do
  begin
    x1 := Node1.X;
    y1 := Node1.Y;
    if (x1 = MISSING) or (y1 = MISSING) then exit;
    x2 := Node2.X;
    y2 := Node2.Y;
    if (x2 = MISSING) or (y2 = MISSING) then exit;
  end;
  if CurrentLinkVar = NOVIEW then k := -1
  else k := Link(I,J).ColorIndex;
  if k < 0 then color := 7
  else color := DXFLinkColor[k];
  size := LinkSize;
  if (MapOptions.DispLinksBySize) then
  begin
    if (k >= 0) then size := size + k/PixPerMapExt;
  end;
  AddPolyLine(aLink,size,0.0,color,'Links');
  symbolflag := False;
  if (RunFlag)
  and (MapOptions.ArrowStyle <> asNone)
  and (MapOptions.ArrowZoom = 100)
  and (not symbolflag) then
  begin
    if aLink.GetVertexCount > 0 then with MapForm.Map do
    begin
      GetLinkMidsegment(aLink, P1, P2);
      x1 := GetX(P1.X);
      y1 := GetY(P1.Y);
      x2 := GetX(P2.X);
      y2 := GetY(P2.Y);
    end;
    SinCos(arctan2(y2 - y1,x2 - x1), asin, acos);
    k := aLink.Zindex;
    if (k >= 0) then
    case FlowDir^[k] of
      PLUS:  AddArrow((x1+x2)/2.0,(y1+y2)/2.0,size,asin,acos,color,'Arrows');
      MINUS: AddArrow((x1+x2)/2.0,(y1+y2)/2.0,size,-asin,-acos,color,'Arrows');
    end;
  end;
end;


procedure AddJunc(I: Integer);
var
  color, k : Integer;
  x1, y1,
  radius   : Single;
  x, y     : array[0..3] of Single;
begin
  x1 := Node(JUNCS,I).X;
  y1 := Node(JUNCS,I).Y;
  if (x1 = MISSING) or (y1 = MISSING) then Exit;

  if CurrentNodeVar = NOVIEW then k := -1
  else k := Node(JUNCS,I).ColorIndex;
  if k < 0 then color := 7
  else color := DXFLinkColor[k];

  radius := NodeSize;
  if (MapOptions.DispNodesBySize) then
  begin
    if (k >= 0) then radius := radius +  k/PixPerMapExt;
  end;
  case JuncStyle of
    0: AddCircle(x1,y1,radius,color,'Junctions');
    1: AddLine(x1-radius,y1,x1+radius,y1,2*radius,1,color,'Junctions');
    2: begin
         x[0] := x1 - radius;
         y[0] := y1 - radius;
         x[1] := x1 + radius;
         y[1] := y[0];
         x[2] := x[0];
         y[2] := y1 + radius;
         x[3] := x[1];
         y[3] := y[2];
         AddSolid(x,y,color,'Junctions');
       end;
  end;
end;


procedure AddTank(const I,J: Integer);
var
  x0, y0       : Single;
  r            : Single;
  x, y         : array[0..3] of Single;
  color, k     : Integer;
begin
  x0 := Node(I,J).X;
  y0 := Node(I,J).Y;
  if (x0 = MISSING) or (y0 = MISSING) then Exit;

  if CurrentNodeVar = NOVIEW then k := -1
  else k := Node(I,J).ColorIndex;
  if k < 0 then color := 7
  else color := DXFLinkColor[k];

  r := NodeSize;
  if (MapOptions.DispNodesBySize) then
  begin
    if (k >= 0) then r := r +  k/PixPerMapExt;
  end;

  if MapOptions.DispTanks then
  begin
    if I = TANKS then
    begin
      x[0] := x0 - 2*r;
      y[0] := y0;
      x[1] := x0 + 2*r;
      y[1] := y0;
      x[2] := x[0];
      y[2] := y0 + 2*r;
      x[3] := x[1];
      y[3] := y[2];
      AddSolid(x,y,color,'Tanks');
      x[0] := x0 - r;
      y[0] := y0;
      x[1] := x0 + r;
      y[1] := y0;
      x[2] := x[0];
      y[2] := y0 - 2*r;
      x[3] := x[1];
      y[3] := y[2];
      AddSolid(x,y,color,'Tanks');
    end
    else
    begin
      x[0] := x0 - 2*r;
      y[0] := y0 - r;
      x[1] := x0 + 2*r;
      y[1] := y[0];
      x[2] := x[0];
      y[2] := y0 + r;
      x[3] := x[1];
      y[3] := y[2];
      AddSolid(x,y,color,'Tanks');
      AddLine(x[0],y0+2*r,x[0],y0-r,(1.0/PixPerMapExt),0.0,color,'Tanks');
      AddLine(x[1],y0+2*r,x[1],y0-r,(1.0/PixPerMapExt),0.0,color,'Tanks');
    end;
  end
  else
  begin
    x[0] := x0 - r;
    y[0] := y0 - r;
    x[1] := x0 + r;
    y[1] := y[0];
    x[2] := x[0];
    y[2] := y0 + r;
    x[3] := x[1];
    y[3] := y[2];
    AddSolid(x,y,color,'Tanks');
  end;
end;


procedure AddLabels;
var
  I   : Integer;
  X,Y : Single;
  aLabel: TMapLabel;
begin
  with Network.Lists[LABELS] do
    for I := 0 to Count-1 do
    begin
      X := MapLabel(I).X;
      Y := MapLabel(I).Y;
      if (X <> MISSING) and (Y <> MISSING) then
      begin
        with Map.Bitmap.Canvas do
        begin
          aLabel := MapLabel(I);
          Font.Name := aLabel.FontName;
          Font.Size := aLabel.FontSize;
          LabelSize := 0.75*TextHeight(Strings[I])/PixPerMapExt;
          Y := Y - LabelSize;
          AddText(X,Y,0.75*LabelSize,0.0,Strings[I],'Labels');
          if Length(aLabel.MeterText) > 0 then
            AddText(X,Y-LabelSize,0.75*LabelSize,0.0,aLabel.MeterText,'Labels');
        end;
      end;
    end;
end;


procedure DXFexport(const Fname: String; const Jstyle: Integer);
var
  i,j,n : Integer;
begin
  AssignFile(DXFfile,Fname);
  {$I-}
  Rewrite(DXFfile);
  {$I+}
  if (IOResult = 0) then
  begin

    Map := MapForm.Map;
    MapOptions := Map.Options;
    PixPerMapExt := MapForm.Map.Window.PPW;
    JuncStyle := Jstyle;
    LinkSize := MapOptions.LinkSize/PixPerMapExt;
    NodeSize := MapOptions.NodeSize/PixPerMapExt;
    LabelSize := 8.0/PixPerMapExt;
    ArrowSize := 2*MapOptions.ArrowSize/PixPerMapExt;
    for i := 0 to MAXINTERVALS do
    begin
      DXFLinkColor[i] := GetDXFColor(MapLinkColor[i]);
      DXFNodeColor[i] := GetDXFColor(MapNodeColor[i]);
    end;

    try
      with MapForm.Map.Dimensions do
        AddHeader(LowerLeft.X, LowerLeft.Y, UpperRight.X, UpperRight.Y);
      StartTables(5);             // #Layers = 5
      AddLayer('Links',7);
      AddLayer('Junctions',7);
      AddLayer('Tanks',7);
      AddLayer('Arrows',7);
      AddLayer('Labels',7);
      EndTables;

      for i := PIPES to VALVES do
      begin
        n := Network.Lists[i].Count - 1;
        for j := 0 to n do AddLink(i,j);
      end;

      if (MapOptions.DispJuncs) then
      begin
        n := Network.Lists[JUNCS].Count - 1;
        for j := 0 to n do AddJunc(j);
      end;

      for i := RESERVS to TANKS do
      begin
        n := Network.Lists[i].Count - 1;
        for j := 0 to n do AddTank(i,j);
      end;

      if MapOptions.DispLabels then AddLabels;
      EndDXF;

    finally
    end;
  end;
  CloseFile(DXFfile);
end;

end.
