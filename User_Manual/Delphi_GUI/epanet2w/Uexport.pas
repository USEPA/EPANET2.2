unit Uexport;

{-------------------------------------------------------------------}
{                    Unit:    Uexport.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that exports network data to INP file.       }
{-------------------------------------------------------------------}

interface

uses Classes, Forms, Controls, Dialogs, SysUtils, Uutils, Uglobals;

procedure ExportAllDemands(var F: TextFile);
procedure ExportControls(var F: TextFile);
procedure ExportDataBase(const Fname: String; const Withmap: Boolean);
procedure ExportDiameters(var F: TextFile);
procedure ExportMap(var F: TextFile);
procedure ExportQuality(var F: TextFile);
procedure ExportReactions(var F: TextFile; const S: String);
procedure ExportRoughness(var F: TextFile);

implementation

uses Fmain, Uimport, Uinput;

{*******************************************************************}
{                Procedures for Exporting Scenarios                 }
{*******************************************************************}

procedure WriteDemandList(var F: TextFile; const ID: String;
  aList: TStringList);
//----------------------------------------------------------
// Writes demands for junction ID to file F.
// Demands are stored in Stringlist aList.
// Each demand record is stored in list with #13 separating
// fields as follows:
// Base Demand + #13 + Demand Pattern + #13 + Category
//----------------------------------------------------------
var
  i,j,j1,k: Integer;
  s: String;                  // Copy of a demand record
  t: array[1..3] of String;   // Fields within demand record
begin
// Examine each demand record in list after primary demand
  for i := 0 to aList.Count - 1 do
  begin

  // Parse fields between #13 characters
    s := aList[i];
    j1 := 1;
    for k := 1 to 2 do
    begin
      j := Pos(#13,s);
      if j > 0 then
      begin
        s[j] := ' ';
        t[k] := Copy(s,j1,j-j1);
        j1 := j+1;
      end;
    end;

  // Retrieve last field
    if j1 > 1 then t[3] := Copy(s,j1,Length(s)-j1+1)
    else t[3] := '';

  // Write record to file
    if Length(t[3]) > 0 then
      Writeln(F,Format(' %-16s'#9'%-12s'#9'%-16s'#9';%s',[ID,t[1],t[2],t[3]]))
    else
      Writeln(F,Format(' %-16s'#9'%-12s'#9'%-16s',[ID,t[1],t[2]]))
  end;
end;

procedure ExportDemands(var F: TextFile);
//------------------------------------------------------------------
// Exports junction demands to [DEMANDS] section of file F
// if junction has at least 2 demand categories.
//------------------------------------------------------------------
var
  i: Integer;
  slist: TStringlist;
begin
  Writeln(F,'[DEMANDS]');
  Writeln(F,';Junction        '#9'Demand      '#9'Pattern         '#9'Category');
  slist := Network.Lists[JUNCS];
  for i := 0 to slist.Count-1 do
    with TJunc(slist.Objects[i]) do
      if (Demands.Count >= 2) then WriteDemandList(F, slist[i], Demands);
  Writeln(F);
end;

procedure ExportAllDemands(var F: TextFile);
//------------------------------------------------------------------
// Exports all junction demands to [DEMANDS] section of file F
//------------------------------------------------------------------
var
  i: Integer;
  slist: TStringlist;
begin
  Writeln(F,'[DEMANDS]');
  Writeln(F,';Junction        '#9'Demand      '#9'Pattern         '#9'Category');
  slist := Network.Lists[JUNCS];
  for i := 0 to slist.Count-1 do
  begin
    with TJunc(slist.Objects[i]) do
      if (Demands.Count >= 2) then WriteDemandList(F, slist[i], Demands)
      else Writeln(F,Format(' %-16s'#9'%-12s'#9'%-16s',
        [ID,Data[JUNC_DEMAND_INDEX],Data[JUNC_PATTERN_INDEX]]));
  end;
  Writeln(F);
end;

procedure ExportDiameters(var F: TextFile);
var
  i: Integer;
begin
  Writeln(F, '[DIAMETERS]');
  Writeln(F, ';Pipe            '#9'Diameter');
  with Network.Lists[PIPES] do
  begin
    for i := 0 to Count - 1 do
      Writeln(F,Format(' %-16s'#9'%s',
        [Strings[i],TLink(Objects[i]).Data[PIPE_DIAM_INDEX]]));
  end;
  Writeln(F);
end;

procedure ExportControls(var F: TextFile);
var
  i,n: Integer;
  slist: TStringList;
begin
  Writeln(F,'[CONTROLS]');
  slist := Network.SimpleControls;
  n := slist.Count;
  if n > 0 then for i := 0 to n-1 do
    Writeln(F,slist[i]);
  Writeln(F);
  Writeln(F,'[RULES]');
  slist := Network.RuleBasedControls;
  n := slist.Count;
  if n > 0 then for i := 0 to n-1 do
    Writeln(F,slist[i]);
  Writeln(F);
end;

procedure ExportMap(var F: TextFile);
var
  i, j, n: Integer;
  s      : String;
  slist  : TStringlist;
  aNode  : TNode;
  aVertex: PVertex;
  aMapLabel: TMapLabel;
begin
  Writeln(F, '[COORDINATES]');
  Writeln(F,';Node            '#9'X-Coord         '#9'Y-Coord');
  for i := JUNCS to TANKS do
  begin
    slist := Network.Lists[i];
    n := slist.Count - 1;
    for j := 0 to n do
    begin
      aNode := Node(i,j);
      with aNode do
        if (X <> MISSING) and (Y <> MISSING) then
          Writeln(F, Format(' %-16s'#9'%-16f'#9'%-16f',[slist[j],X,Y]));
    end;
  end;
  Writeln(F);

  Writeln(F, '[VERTICES]');
  Writeln(F,';Link            '#9'X-Coord         '#9'Y-Coord');
  for i := PIPES to VALVES do
  begin
    slist := Network.Lists[i];
    for j := 0 to slist.Count - 1 do
    begin
      aVertex := Link(i,j).Vlist;
      while aVertex <> nil do
      begin
        Writeln(F, Format(' %-16s'#9'%-16f'#9'%-16f',
          [slist[j],aVertex^.X,aVertex^.Y]));
        aVertex := aVertex^.Next;
      end;
    end;
  end;
  Writeln(F);

  Writeln(F, '[LABELS]');
  Writeln(F, ';X-Coord           Y-Coord          Label & Anchor Node');

  slist := Network.Lists[LABELS];
  n := slist.Count - 1;
  for j := 0 to n do
  begin
    aMapLabel := MapLabel(j);
    with aMapLabel do
    begin
      s := '';
      if Anchor <> nil then s := Anchor.ID;
    end;
    Writeln(F, Format(' %-16f %-16f "%s" %-16s',
      [aMapLabel.X,aMapLabel.Y,slist[j],s]));
  end;
  Writeln(F);

  Writeln(F, '[BACKDROP]');
  with MapDimensions do
  Writeln(F, Format(' DIMENSIONS     '#9'%-16f'#9'%-16f'#9'%-16f'#9'%-16f',
    [LowerLeft.X, LowerLeft.Y, UpperRight.X, UpperRight.Y]));
  Writeln(F, Format(' UNITS          '#9'%s',[MapUnits[Ord(MapDimensions.Units)]]));
  Writeln(F, Format(' FILE           '#9'%s',[MapBackdrop.Filename]));
  Writeln(F, Format(' OFFSET         '#9'%-16f'#9'%-16f',
    [MapBackdrop.Offset.X, MapBackdrop.Offset.Y]));
  Writeln(F);
end;

procedure ExportQuality(var F: TextFile);
var
  i,j,k,n  : Integer;
  slist    : TStringList;
begin
  Writeln(F,'[QUALITY]');
  Writeln(F,';Node            '#9'InitQual');
  for j := JUNCS to TANKS do
  begin
    if (j = RESERVS) then    k := RES_INITQUAL_INDEX
    else if (j = TANKS) then k := TANK_INITQUAL_INDEX
    else                     k := JUNC_INITQUAL_INDEX;
    slist := Network.Lists[j];
    n := slist.Count;
    if n > 0 then for i := 0 to n-1 do
    begin
      with TNode(slist.Objects[i]) do
        if Length(Trim(Data[k])) > 0 then
          Writeln(F,Format(' %-16s'#9'%s',[slist[i],Data[k]]));
    end;
  end;
  Writeln(F);
end;

procedure ExportReactions(var F: TextFile; const S: String);
var
  i,n: Integer;
  slist: TStringList;
begin
  if (Length(S) > 0) then WriteLn(F,S);
  slist := Network.Lists[PIPES];
  n := slist.Count;
  if n > 0 then for i := 0 to n-1 do
  begin
    with TLink(slist.Objects[i]) do
    begin
      if Length(Trim(Data[PIPE_KBULK_INDEX])) > 0 Then
        Writeln(F,Format(' Bulk     '#9'%-16s'#9'%s',
          [slist[i],Data[PIPE_KBULK_INDEX]]));
      if Length(Trim(Data[PIPE_KWALL_INDEX])) > 0 Then
        Writeln(F,Format(' Wall     '#9'%-16s'#9'%s',
          [slist[i],Data[PIPE_KWALL_INDEX]]));
    end;
  end;
  slist := Network.Lists[TANKS];
  n := slist.Count;
  if n > 0 then for i := 0 to n-1 do
  begin
    with TNode(slist.Objects[i]) do
    begin
      if Length(Trim(Data[TANK_KBULK_INDEX])) > 0 then
        Writeln(F,Format(' Tank     '#9'%-16s'#9'%s',
          [slist[i],Data[TANK_KBULK_INDEX]]));
    end;
  end;
  Writeln(F);
end;

procedure ExportRoughness(var F: TextFile);
var
  i,n: Integer;
  slist: TStringList;
begin
  WriteLn(F,'[ROUGHNESS]');
  Writeln(F,';Pipe            '#9'Roughness');
  slist := Network.Lists[PIPES];
  n := slist.Count;
  if n > 0 then for i := 0 to n-1 do
  begin
    with TLink(slist.Objects[i]) do
      Writeln(F,Format(' %-16s'#9'%s',
        [slist[i],Data[PIPE_ROUGH_INDEX]]));
  end;
  Writeln(F);
end;


{********************************************************************}
{                 Procedures for Exporting a Project                 }
{********************************************************************}

procedure ExportDataBase(const Fname: String; const Withmap: Boolean);
{---------------------------------------------------------}
{ Saves data base to file Fname in standard EPANET format.}
{ Argument Withmap = TRUE if map coordinates included.    }
{---------------------------------------------------------}
var
  F     : TextFile;
  i,j   : Integer;
  m,n   : Integer;
  k     : Integer;
  v     : Single;
  id    : String;
  s     : String;
  slist : TStringList;
  xlist : TStringList;
  ylist : TStringList;
begin
  AssignFile(F,Fname);
  {$I-}
  Rewrite(F);
  {$I+}
  if (IOResult = 0) then
  try

    Writeln(F,'[TITLE]');
    Writeln(F,Network.Options.Title);
    slist := Network.Options.Notes;
    n := slist.Count;
    if n > 0 then for i := 0 to n-1 do
      Writeln(F,slist[i]);
    Writeln(F);

    Writeln(F,'[JUNCTIONS]');
    Writeln(F,
';ID              '#9'Elev        '#9'Demand      '#9'Pattern         ');
    slist := Network.Lists[JUNCS];
    n := slist.Count;
    if n > 0 then
    begin
      for i := 0 to n-1 do
      begin
        with TJunc(slist.Objects[i]) do
          Writeln(F,Format(' %-16s'#9'%-12s'#9'%-12s'#9'%-16s'#9';%s',
             [slist[i],Data[JUNC_ELEV_INDEX],Data[JUNC_DEMAND_INDEX],
               Data[JUNC_PATTERN_INDEX],Data[COMMENT_INDEX]]));
      end;
    end;
    Writeln(F);

    Writeln(F,'[RESERVOIRS]');
    Writeln(F,';ID              '#9'Head        '#9'Pattern         ');
    slist := Network.Lists[RESERVS];
    n := slist.Count;
    if n > 0 then
    begin
      for i := 0 to n-1 do
      begin
        with TNode(slist.Objects[i]) do
          Writeln(F,Format(' %-16s'#9'%-12s'#9'%-16s'#9';%s',[slist[i],
            Data[RES_HEAD_INDEX],Data[RES_PATTERN_INDEX],Data[COMMENT_INDEX]]));
      end;
    end;
    Writeln(F);

    Writeln(F,'[TANKS]');
    Writeln(F,';ID              '#9'Elevation   '#9'InitLevel   '#9+
      'MinLevel    '#9'MaxLevel    '#9'Diameter    '#9'MinVol      '#9+
        'VolCurve        '#9'Overflow');
    slist := Network.Lists[TANKS];
    n := slist.Count;
    if n > 0 then
    begin
      for i := 0 to n-1 do
      begin
        with TNode(slist.Objects[i]) do
        begin
          s := Format(' %-16s'#9'%-12s'#9'%-12s'#9'%-12s'#9'%-12s'#9+'%-12s'#9,
               [slist[i], Data[TANK_ELEV_INDEX], Data[TANK_INITLVL_INDEX],
               Data[TANK_MINLVL_INDEX], Data[TANK_MAXLVL_INDEX],
               Data[TANK_DIAM_INDEX]]);
          if Length(Trim(Data[TANK_MINVOL_INDEX])) = 0
          then s := s + '0           '#9
          else s := s + Format('%-12s'#9, [Data[TANK_MINVOL_INDEX]]);
          if SameText(Data[TANK_OVERFLOW_INDEX], 'Yes') then
          begin
            if Length(Trim(Data[TANK_VCURVE_INDEX])) = 0
            then s := s + '*               '#9
            else s := s + Format('-16s'#9, [Data[TANK_VCURVE_INDEX]]);
            s := s + 'Yes'#9';' + Data[COMMENT_INDEX];
          end
          else s := s + Format('%-16s'#9';%s', [Data[TANK_VCURVE_INDEX],
                        Data[COMMENT_INDEX]]);
          Writeln(F, s);
        end;
      end;
    end;
    Writeln(F);

    Writeln(F,'[PIPES]');
    Writeln(F,';ID              '#9'Node1           '#9'Node2           '#9+
      'Length      '#9'Diameter    '#9'Roughness   '#9'MinorLoss   '#9+
        'Status');
    slist := Network.Lists[PIPES];
    n := slist.Count;
    if n > 0 then
    begin
      for i := 0 to n-1 do
      begin
        with TLink(slist.Objects[i]) do
          if Assigned(Node1) and Assigned(Node2) then
            Writeln(F,Format(' %-16s'#9'%-16s'#9'%-16s'#9'%-12s'#9'%-12s'+
              #9'%-12s'#9'%-12s'#9'%-6s'#9';%s',
                [slist[i],Node1.ID,Node2.ID,Data[PIPE_LEN_INDEX],
                  Data[PIPE_DIAM_INDEX],Data[PIPE_ROUGH_INDEX],
                    Data[PIPE_MLOSS_INDEX],Data[PIPE_STATUS_INDEX],
                      Data[COMMENT_INDEX]]));
      end;
    end;
    Writeln(F);

    Writeln(F,'[PUMPS]');
    Writeln(F,';ID              '#9'Node1           '#9'Node2           '#9+
      'Parameters');
    slist := Network.Lists[PUMPS];
    n := slist.Count;
    if n > 0 then
    begin
      for i := 0 to n-1 do
      begin
        with TLink(slist.Objects[i]) do
        begin
          if Assigned(Node1) and Assigned(Node2) then
          begin
            s := Format(' %-16s'#9'%-16s'#9'%-16s',[slist[i],Node1.ID,Node2.ID]);
            if Length(Trim(Data[PUMP_HCURVE_INDEX])) > 0 then
              s := s + #9 + 'HEAD ' + Data[PUMP_HCURVE_INDEX]
            else
              s := s + #9 + 'POWER ' + Data[PUMP_HP_INDEX];
            if Length(Trim(Data[PUMP_SPEED_INDEX])) > 0 then
              s := s + #9 + 'SPEED ' + Data[PUMP_SPEED_INDEX];
            if Length(Trim(Data[PUMP_PATTERN_INDEX])) > 0 then
              s := s + #9 + 'PATTERN ' + Data[PUMP_PATTERN_INDEX];
            s := s + #9 + ';' + Data[COMMENT_INDEX];
            Writeln(F,s);
          end;
        end;
      end;
    end;
    Writeln(F);

    Writeln(F,'[VALVES]');
    Writeln(F,';ID              '#9'Node1           '#9'Node2           '#9+
      'Diameter    '#9'Type'#9'Setting     '#9'MinorLoss   ');
    slist := Network.Lists[VALVES];
    n := slist.Count;
    if n > 0 then
    begin
      for i := 0 to n-1 do
      begin
        with TLink(slist.Objects[i]) do
          if Assigned(Node1) and Assigned(Node2) then
            Writeln(F,Format(' %-16s'#9'%-16s'#9'%-16s'#9'%-12s'#9'%-4s'#9+
              '%-12s'#9'%-12s'#9';%s',
                [slist[i],Node1.ID,Node2.ID,Data[VALVE_DIAM_INDEX],
                  Data[VALVE_TYPE_INDEX],Data[VALVE_SETTING_INDEX],
                    Data[VALVE_MLOSS_INDEX],Data[COMMENT_INDEX]]));
      end;
    end;
    Writeln(F);

    Writeln(F,'[TAGS]');
    for i := JUNCS to TANKS do
    begin
      slist := Network.Lists[i];
      for j := 0 to slist.Count-1 do
      begin
        s := TNode(slist.Objects[j]).Data[TAG_INDEX];
        if Length(Trim(s)) > 0 then
          Writeln(F,Format(' NODE '#9'%-16s'#9'%s',[slist[j],s]));
      end;
    end;
    for i := PIPES to VALVES do
    begin
      slist := Network.Lists[i];
      for j := 0 to slist.Count-1 do
      begin
        s := TLink(slist.Objects[j]).Data[TAG_INDEX];
        if Length(Trim(s)) > 0 then
          Writeln(F,Format(' LINK '#9'%-16s'#9'%s',[slist[j],s]));
      end;
    end;
    Writeln(F);

    ExportDemands(F);

    Writeln(F,'[STATUS]');
    Writeln(F,';ID              '#9'Status/Setting');
    slist := Network.Lists[PUMPS];
    n := slist.Count;
    if n > 0 then for i := 0 to n-1 do
    begin
      with TLink(slist.Objects[i]) do
      begin
        if Uutils.GetSingle(Data[PUMP_SPEED_INDEX],v) and (v <> 1.0) then
          Writeln(F,Format(' %-16s'#9'%s',[slist[i],Data[PUMP_SPEED_INDEX]]));
        if Data[PUMP_STATUS_INDEX] = 'Closed' then
          Writeln(F,Format(' %-16s'#9'%s',[slist[i],Data[PUMP_STATUS_INDEX]]));
      end;
    end;
    slist := Network.Lists[VALVES];
    n := slist.Count;
    if n > 0 then for i := 0 to n-1 do
    begin
      with TLink(slist.Objects[i]) do
        if Data[VALVE_STATUS_INDEX] <> 'None' then
          Writeln(F,Format(' %-16s'#9'%s',[slist[i],Data[VALVE_STATUS_INDEX]]));
    end;
    Writeln(F);

    Writeln(F,'[PATTERNS]');
    Writeln(F,';ID              '#9'Multipliers');
    slist := Network.Lists[PATTERNS];
    n := slist.Count;
    if n > 0 then for i := 0 to n-1 do
    begin
      id := slist[i];
      Writeln(F,Format(';%s',[TPattern(slist.Objects[i]).Comment]));
      xlist := TPattern(slist.Objects[i]).Multipliers;
      k := 0;
      m := xlist.Count;
      if m > 0 then for j := 0 to m-1 do
      begin
        if k = 6 then
        begin
          Writeln(F);
          k := 0;
        end;
        if k = 0 then Write(F, Format(' %-16s',[id]));
        Write(F,Format(#9'%-12s',[xlist[j]]));
        Inc(k);
      end;
      Writeln(F);
    end;
    Writeln(F);

    Writeln(F,'[CURVES]');
    Writeln(F,';ID              '#9'X-Value     '#9'Y-Value');
    slist := Network.Lists[CURVES];
    n := slist.Count;
    if n > 0 then for i := 0 to n-1 do
    begin
      with TCurve(slist.Objects[i]) do
      begin
         Writeln(F,Format(';%s: %s',[Ctype,Comment]));
        xlist := Xdata;
        ylist := Ydata;
        m := xlist.Count;
        if m > 0 then for j := 0 to m-1 do
          Writeln(F,Format(' %-16s'#9'%-12s'#9'%-12s',
            [slist[i],xlist[j],ylist[j]]));
      end;
    end;
    Writeln(F);

    ExportControls(F);

    Writeln(F,'[ENERGY]');
    with Network.Options do
    begin
      if Length(Trim(Data[EFFIC_INDEX])) > 0 then
        Writeln(F,' Global Efficiency  '#9,Data[EFFIC_INDEX]);
      if Length(Trim(Data[EPRICE_INDEX])) > 0 then
        Writeln(F,' Global Price       '#9,Data[EPRICE_INDEX]);
      if Length(Trim(Data[PRICE_PAT_INDEX])) > 0 then
        Writeln(F,' Global Pattern     '#9,Data[PRICE_PAT_INDEX]);
      if Length(Trim(Data[DMND_CHARGE_INDEX])) > 0 then
        Writeln(F,' Demand Charge      '#9,Data[DMND_CHARGE_INDEX]);
    end;
    slist := Network.Lists[PUMPS];
    for i := 0 to slist.Count-1 do
    begin
      with TLink(slist.Objects[i]) do
      begin
        if (Length(Trim(Data[PUMP_ECURVE_INDEX])) > 0) then
          Writeln(F, Format(' Pump '#9'%-16s'#9'Efficiency'#9'%s',[slist[i],
            Data[PUMP_ECURVE_INDEX]]));
        if (Length(Trim(Data[PUMP_EPRICE_INDEX])) > 0) then
          Writeln(F, Format(' Pump '#9'%-16s'#9'Price     '#9'%s',[slist[i],
            Data[PUMP_EPRICE_INDEX]]));
        if (Length(Trim(Data[PUMP_PRICEPAT_INDEX])) > 0) then
          Writeln(F, Format(' Pump '#9'%-16s'#9'Pattern   '#9'%s',[slist[i],
            Data[PUMP_PRICEPAT_INDEX]]));
      end;
    end;
    Writeln(F);

    Writeln(F,'[EMITTERS]');
    Writeln(F,';Junction        '#9'Coefficient');
    slist := Network.Lists[JUNCS];
    n := slist.Count;
    for i := 0 to n-1 do
    begin
      with TNode(slist.Objects[i]) do
        if Length(Data[JUNC_EMITTER_INDEX]) > 0 then
          Writeln(F,Format(' %-16s'#9'%s',[slist[i],Data[JUNC_EMITTER_INDEX]]));
    end;
    Writeln(F);

    ExportQuality(F);

    Writeln(F,'[SOURCES]');
    Writeln(F,';Node            '#9'Type        '#9'Quality     '#9'Pattern');
    for j := JUNCS to TANKS do
    begin
       slist := Network.Lists[j];
       n := slist.Count;
       if n > 0 then for i := 0 to n-1 do
       begin
          with TNode(slist.Objects[i]) do
          begin
            if (j = RESERVS)    then k := RES_SRCQUAL_INDEX
            else if (j = TANKS) then k := TANK_SRCQUAL_INDEX
            else                     k := JUNC_SRCQUAL_INDEX;
            if Uutils.GetSingle(Data[k],v) and (v > 0.0) then
              Writeln(F,Format(' %-16s'#9'%-12s'#9'%-12s'#9'%s',
                [slist[i],Data[k+2],Data[k],Data[k+1]]));
          end;
       end;
    end;
    Writeln(F);

    Writeln(F,'[REACTIONS]');
    Writeln(F,';Type     '#9'Pipe/Tank       '#9'Coefficient');
    ExportReactions(F, '');
    Writeln(F);

    Writeln(F,'[REACTIONS]');
    with Network.Options do
    begin
      if Length(Trim(Data[BULK_ORDER_INDEX])) > 0 then
      begin
        Writeln(F,' Order Bulk            '#9,Data[BULK_ORDER_INDEX]);
        Writeln(F,' Order Tank            '#9,Data[BULK_ORDER_INDEX]);
      end;

      if UpperCase(Trim(Data[WALL_ORDER_INDEX])) = 'ZERO' then
        Writeln(F,' Order Wall            '#9'0')
      else
        Writeln(F,' Order Wall            '#9'1');
      if Length(Trim(Data[GLOBAL_KBULK_INDEX])) > 0 then
        Writeln(F,' Global Bulk           '#9,Data[GLOBAL_KBULK_INDEX]);
      if Length(Trim(Data[GLOBAL_KWALL_INDEX])) > 0 then
        Writeln(F,' Global Wall           '#9,Data[GLOBAL_KWALL_INDEX]);
      if Length(Trim(Data[LIMIT_QUAL_INDEX])) > 0 then
        Writeln(F,' Limiting Potential    '#9,Data[LIMIT_QUAL_INDEX]);
      if Length(Trim(Data[ROUGH_CORREL_INDEX])) > 0 then
        Writeln(F,' Roughness Correlation '#9,Data[ROUGH_CORREL_INDEX]);
    end;
    Writeln(F);

    Writeln(F,'[MIXING]');
    Writeln(F,';Tank            '#9'Model');
    slist := Network.Lists[TANKS];
    n := slist.Count;
    if n > 0 then for i := 0 to n-1 do
    begin
      with Node(TANKS,i) do
      begin
        if UpperCase(Trim(Data[TANK_MIXMODEL_INDEX])) <> 'MIXED' then
            Writeln(F,Format(' %-16s'#9'%-12s'#9'%s',
                [slist[i],Data[TANK_MIXMODEL_INDEX],Data[TANK_MIXFRAC_INDEX]]));
      end;
    end;
    Writeln(F);

    Writeln(F,'[TIMES]');
    with Network.Options do
    begin
      Writeln(F,' Duration           '#9,Data[DURATION_INDEX]);
      Writeln(F,' Hydraulic Timestep '#9,Data[HYD_TSTEP_INDEX]);
      Writeln(F,' Quality Timestep   '#9,Data[QUAL_TSTEP_INDEX]);
      Writeln(F,' Pattern Timestep   '#9,Data[PAT_TSTEP_INDEX]);
      Writeln(F,' Pattern Start      '#9,Data[PAT_START_INDEX]);
      Writeln(F,' Report Timestep    '#9,Data[RPT_TSTEP_INDEX]);
      Writeln(F,' Report Start       '#9,Data[RPT_START_INDEX]);
      Writeln(F,' Start ClockTime    '#9,Data[START_TIME_INDEX]);
      Writeln(F,' Statistic          '#9,Data[TIME_STAT_INDEX]);
      Writeln(F);

      Writeln(F,'[REPORT]');
      Writeln(F,' Status             '#9,Data[STATUS_RPT_INDEX]);
      Writeln(F,' Summary            '#9'No');
      Writeln(F,' Page               '#9'0');
      Writeln(F);

      Writeln(F,'[OPTIONS]');
      Writeln(F,' Units              '#9,Data[FLOW_UNITS_INDEX]);
      Writeln(F,' Headloss           '#9,Data[HLOSS_FORM_INDEX]);
      Writeln(F,' Specific Gravity   '#9,Data[SPEC_GRAV_INDEX]);
      Writeln(F,' Viscosity          '#9,Data[VISCOS_INDEX]);
      Writeln(F,' Trials             '#9,Data[TRIALS_INDEX]);
      Writeln(F,' Accuracy           '#9,Data[ACCURACY_INDEX]);

      if Uutils.GetSingle(Data[HEAD_ERROR_INDEX], v) and (v > 0)
      then Writeln(F,' HEADERROR          '#9,Data[HEAD_ERROR_INDEX]);
      if Uutils.GetSingle(Data[FLOW_CHANGE_INDEX], v) and (v > 0)
      then Writeln(F,' FLOWCHANGE         '#9,Data[FLOW_CHANGE_INDEX]);

      Writeln(F,' CHECKFREQ          '#9,Data[CHECK_FREQ_INDEX]);
      Writeln(F,' MAXCHECK           '#9,Data[MAX_CHECK_INDEX]);
      Writeln(F,' DAMPLIMIT          '#9,Data[DAMP_LIMIT_INDEX]);

      if CompareText(Data[UNBALANCED_INDEX],'Stop') = 0 then
      Writeln(F,' Unbalanced         '#9,Data[UNBALANCED_INDEX])
      else
      Writeln(F,' Unbalanced         '#9'Continue 10');
      if Length(Trim(Data[GLOBAL_PAT_INDEX])) > 0 then
      Writeln(F,' Pattern            '#9,Data[GLOBAL_PAT_INDEX]);
      Writeln(F,' Demand Multiplier  '#9,Data[DEMAND_MULT_INDEX]);

      if CompareText(Data[DEMAND_MODEL_INDEX], 'PDA') = 0 then
      begin
        Writeln(F,' Demand Model       '#9,Data[DEMAND_MODEL_INDEX]);
        Writeln(F,' Minimum Pressure   '#9,Data[MIN_PRESSURE_INDEX]);
        Writeln(F,' Required Pressure  '#9,Data[REQ_PRESSURE_INDEX]);
        Writeln(F,' Pressure Exponent  '#9,Data[PRESSURE_EXP_INDEX]);
      end;

      Writeln(F,' Emitter Exponent   '#9,Data[EMITTER_EXP_INDEX]);
      if UpperCase(Trim(Data[QUAL_PARAM_INDEX])) = 'TRACE' then
      Writeln(F,' Quality            '#9'Trace ',Data[TRACE_NODE_INDEX])
      else
      Writeln(F,' Quality            '#9,Data[QUAL_PARAM_INDEX],' ',
          Data[QUAL_UNITS_INDEX]);
      Writeln(F,' Diffusivity        '#9,Data[DIFFUS_INDEX]);
      Writeln(F,' Tolerance          '#9,Data[QUAL_TOL_INDEX]);
      Writeln(F);
    end;

    if (Withmap) then ExportMap(F);
    Writeln(F,'[END]');

  finally
  end;
  CloseFile(F);
end;

end.
