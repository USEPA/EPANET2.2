unit Ufileio;

{-------------------------------------------------------------------}
{                    Unit:    Ufileio.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that reads network database from .NET file   }
{   and saves network database to .NET file                         }
{-------------------------------------------------------------------}

interface

uses Dialogs, Classes, SysUtils, Forms, Controls, Windows, Math,
     FileCtrl, System.UITypes, PgSetup, Uglobals, Uutils;

const
  MSG_READING_INPUT = 'Reading input file...';
  MSG_READ_ERR = 'Invalid file format.';
  FILE_ERR1a = 'Calibration file ';
  FILE_ERR1b = ' does not exist.';
  FILE_ERR2a = 'Could not access calibration file ';
  FILE_ERR2b = 'Check that file is not in use by another program.';
  FILE_ERR3a = 'Could not open file ';
  FILE_ERR4a = 'Could not read file ';
  FILE_ERR4b = 'File is probably not a valid EPANET project or input file.';
  FILE_ERR5  = 'Data could not be saved to file.';
  FILE_OK = 0;
  FILE_NO_NET = 1;
  FILE_NO_OPEN = 2;

type
  EInvalidFileType = class(Exception);
  function  OpenProject(var Fname: String): TInputFileType;
  procedure RegisterCalibData;
  procedure SaveProject(const Fname: String);

implementation

uses Fmain, Uimport, Uinput;

var
  Version: Integer;


{*******************************************************************}
{       Procedures for Reading & Writing Lists and Arrays           }
{*******************************************************************}

procedure ReadArray(Reader: TReader; var A: array of String);
var
  i,n: Integer;
begin
  n := Reader.ReadInteger - 1;
  n := MinIntValue([n, High(A)]);
  for i := 0 to n do A[i] := Reader.ReadString;
end;

procedure WriteArray(Writer: TWriter; A: array of String);
var
  i,n: Integer;
begin
  n := High(A) + 1;
  Writer.WriteInteger(n);
  for i := 0 to n-1 do Writer.WriteString(A[i]);
end;

procedure ReadList(Reader: TReader; SList: TStringList);
begin
  SList.Clear;
  with Reader do
  try
    ReadListBegin;
    while not EndOfList do SList.Add(ReadString);
    ReadListEnd;
  except
    raise;
  end;
end;

procedure WriteList(Writer: TWriter; SList: TStringList);
var
  i : Integer;
begin
  with Writer do
  try
    WriteListBegin;
    for i := 0 to SList.Count-1 do WriteString(SList[i]);
    WriteListEnd;
  except
    raise;
  end;
end;

procedure WriteVertices(Writer: TWriter; aLink: TLink);
var
  n: Integer;
  aVertex: PVertex;
begin
  n := aLink.GetVertexCount;
  Writer.WriteInteger(n);
  aVertex := aLink.Vlist;
  while aVertex <> nil do
  begin
    Writer.WriteFloat(aVertex^.X);
    Writer.WriteFloat(aVertex^.Y);
    aVertex := aVertex^.Next;
  end;
end;

procedure ReadVertices(Reader: TReader; aLink: TLink);
var
  i, n: Integer;
  aVertex: PVertex;
begin
  n := Reader.ReadInteger;
  for i := 1 to n do
  begin
    New(aVertex);
    aVertex^.X := Reader.ReadFloat;
    aVertex^.Y := Reader.ReadFloat;
    aVertex^.Next := aLink.Vlist;
    aLink.Vlist := aVertex;
  end;
  if n > 1 then aLink.ReverseVlist;
end;


{*******************************************************************}
{              Procedure for Saving a Project to File               }
{*******************************************************************}

procedure SaveProject(const Fname: String);
//------------------------------------------
// Saves current project to file Fname.
//------------------------------------------
var
  FileStream : TFileStream;
  Writer     : TWriter;
  i,j,n      : Integer;
  slist      : TStringList;
  aNode1     : TNode;
  aNode2     : TNode;
  aLink      : TLink;
  aLabel     : TMapLabel;
begin
// Create a file stream object
  try
    FileStream := TFileStream.Create(Fname,
      fmCreate or fmOpenWrite or fmShareDenyWrite);
  except
    Uutils.MsgDlg(FILE_ERR3a + Fname + '.' + #10 + #10 + FILE_ERR2b,
      mtError, [mbOK]);
    Exit;
  end;

// Create a writer object
  try
    Writer := TWriter.Create(FileStream, $ff);

    with Writer do
    try

    //Write file header.
      WriteString('<EPANET2>');
      WriteInteger(VERSIONID2);

    //Write numbers of network components
      for i := JUNCS to CNTRLS do
        WriteInteger(Network.Lists[i].Count);

    //Write network options to file
      WriteString(Network.Options.Title);
      WriteList(Writer,Network.Options.Notes);
      WriteArray(Writer,Network.Options.Data);
      WriteBoolean(AutoLength);

    //Write patterns to file
      slist := Network.Lists[PATTERNS];
      n := slist.Count;
      WriteInteger(n);
      if n > 0 then for i := 0 to n-1 do
      begin
        WriteString(slist.Strings[i]);
        WriteString(TPattern(slist.Objects[i]).Comment);
        WriteList(Writer,TPattern(slist.Objects[i]).Multipliers);
      end;

    //Write curves to file
      slist := Network.Lists[CURVES];
      n := slist.Count;
      WriteInteger(n);
      if n > 0 then for i := 0 to n-1 do
      begin
        WriteString(slist.Strings[i]);
        WriteString(TCurve(slist.Objects[i]).Comment);
        WriteString(TCurve(slist.Objects[i]).Ctype);
        WriteList(Writer,TCurve(slist.Objects[i]).Xdata);
        WriteList(Writer,TCurve(slist.Objects[i]).Ydata);
      end;

    //Write junctions to file
      slist := Network.Lists[JUNCS];
      n := slist.Count;
      WriteInteger(n);
      if n > 0 then for i := 0 to n-1 do
      begin
        WriteString(slist[i]);
        WriteFloat(TJunc(slist.Objects[i]).X);
        WriteFloat(TJunc(slist.Objects[i]).Y);
        WriteArray(Writer,TJunc(slist.Objects[i]).Data);
        WriteList(Writer,TJunc(slist.Objects[i]).Demands);
      end;

    //Write reservoirs & tanks to file
      for j := RESERVS to TANKS do
      begin
        slist := Network.Lists[j];
        n := slist.Count;
        WriteInteger(n);
        if n > 0 then for i := 0 to n-1 do
        begin
          WriteString(slist[i]);
          WriteFloat(TNode(slist.Objects[i]).X);
          WriteFloat(TNode(slist.Objects[i]).Y);
          WriteArray(Writer,TNode(slist.Objects[i]).Data);
        end;
      end;

    //Write links to file
      for j := PIPES to VALVES do
      begin
        slist := Network.Lists[j];
        n := slist.Count;
        WriteInteger(n);
        if n > 0 then for i := 0 to n-1 do
        begin
          aLink := Link(j,i);
          aNode1 := aLink.Node1;
          aNode2 := aLink.Node2;
          WriteString(slist[i]);
          if Assigned(aNode1) then WriteString(aNode1.ID)
          else WriteString('');
          if Assigned(aNode2) then WriteString(aNode2.ID)
          else WriteString('');
          WriteVertices(Writer,aLink);
          WriteArray(Writer,aLink.Data);
        end;
      end;

    //Write control rules to file
      WriteList(Writer,Network.SimpleControls);
      WriteList(Writer,Network.RuleBasedControls);

    //Write map labels to file
      slist := Network.Lists[LABELS];
      n := slist.Count;
      WriteInteger(n);
      if n > 0 then for i := 0 to n-1 do
      begin
        aLabel := MapLabel(i);
        WriteString(slist.Strings[i]);
        WriteFloat(aLabel.X);
        WriteFloat(aLabel.Y);
        if Assigned(aLabel.Anchor) then
          WriteString(aLabel.Anchor.ID)
        else
          WriteString('');
        WriteString(aLabel.FontName);
        WriteInteger(aLabel.FontSize);
        WriteBoolean(aLabel.FontBold);
        WriteBoolean(aLabel.FontItalic);
        WriteInteger(aLabel.MeterType);
        WriteString(aLabel.MeterId);
      end;

    //Write map dimensions to file
      with MapDimensions do
      begin
        WriteFloat(LowerLeft.X);
        WriteFloat(LowerLeft.Y);
        WriteFloat(UpperRight.X);
        WriteFloat(UpperRight.Y);
        WriteInteger(Ord(Units));
      end;

    //Write map backdrop information to file
      with MapBackdrop do
      begin
        WriteString(Filename);
        WriteFloat(Offset.X);
        WriteFloat(Offset.Y);
      end;

    //Write map display options to file
      with MapOptions do
      begin
        WriteBoolean(DispNodeIDs);
        WriteBoolean(DispNodeValues);
        WriteBoolean(DispNodesBySize);
        WriteBoolean(DispNodeBorder);
        WriteBoolean(DispLinkIDs);
        WriteBoolean(DispLinkValues);
        WriteBoolean(DispLinksBySize);
        WriteBoolean(DispJuncs);
        WriteBoolean(DispTanks);
        WriteBoolean(DispPumps);
        WriteBoolean(DispValves);
        WriteBoolean(DispEmitters);
        WriteBoolean(DispSources);
        WriteBoolean(DispLabels);
        WriteBoolean(LabelsTranspar);
        WriteBoolean(NotationTranspar);
        WriteInteger(NodeSize);
        WriteInteger(LinkSize);
        WriteInteger(Ord(ArrowStyle));
        WriteInteger(ArrowSize);
        WriteInteger(ColorIndex);
        WriteInteger(NotationZoom);
        WriteInteger(LabelZoom);
        WriteInteger(SymbolZoom);
        WriteInteger(ArrowZoom);
      end;
      WriteInteger(MAXINTERVALS + 1);
      for i := 0 to MAXINTERVALS do WriteInteger(MapNodeColor[i]);
      WriteInteger(MAXINTERVALS + 1);
      for i := 0 to MAXINTERVALS do WriteInteger(MapLinkColor[i]);

    //Write calibration files to file
      for i := Low(NodeCalibData) to High(NodeCalibData) do
        WriteString(NodeCalibData[i].FileName);
      for i := Low(LinkCalibData) to High(LinkCalibData) do
        WriteString(LinkCalibData[i].FileName);

    // Write default properties to file
      WriteInteger(IDIncrement);
      WriteArray(Writer,IDPrefix);
      for i := JUNCS to VALVES do
        WriteArray(Writer,DefProp[i].Data);

    // Write legend intervals to file
      for i := ELEVATION to NODEVIEWS do
        for j := 1 to MAXINTERVALS do WriteFloat(NodeLegend[i].Intervals[j]);
      for i := DIAMETER to LINKVIEWS do
        for j := 1 to MAXINTERVALS do WriteFloat(LinkLegend[i].Intervals[j]);

    // Write printed Page Layout info to file
      with PageLayout do
      begin
        WriteFloat(LMargin);
        WriteFloat(TMargin);
        WriteFloat(RMargin);
        WriteFloat(BMargin);
      end;
      with MainForm.PageSetupDialog do
      begin
        WriteString(Header.Text);
        WriteInteger(Ord(Header.Alignment));
        WriteBoolean(Header.Enabled);
        WriteString(Footer.Text);
        WriteInteger(Ord(Footer.Alignment));
        WriteBoolean(Footer.Enabled);
        WriteInteger(Ord(PageNumbers));
      end;
      WriteBoolean(TitleAsHeader);
      WriteInteger(Orientation);

    // Write end-of-data marker
      WriteInteger(-1);

    finally
      Writer.Free;
    end;

  finally
    FileStream.Free;
  end;
end;


{*******************************************************************}
{             Procedures for Opening a Project                      }
{*******************************************************************}

function ReadNetFile(const Fname: String): Integer;
{-----------------------------------------------}
{ Reads in network data from .NET file          }
{-----------------------------------------------}
var
  i,j,n,p   : Integer;
  id,s      : String;
  junc      : TJunc;
  node      : TNode;
  link      : TLink;
  pat       : TPattern;
  curve     : TCurve;
  mlabel    : TMapLabel;
  Ncomp     : Integer;
  StepSize  : Integer;
  FileStream: TFileStream;
  Reader    : TReader;
  nList     : TStringList;
  sList     : TStringList;
  Qstep     : Single;

begin
// Create a FileStream object
  try
    //Result := 0;
    FileStream := TFileStream.Create(Fname, fmOpenRead);
  except
    Result := FILE_NO_OPEN;
    Exit;
  end;

// Create sorted node list & general purpose string list
  nList  := TStringList.Create;
  sList  := TStringList.Create;
  try
    nList.Sorted := True;

  // Create a Reader object
    Reader := TReader.Create(FileStream, $ff);
    try
      with Reader do
      try

      //Check for <EPANET2> marker at start of input file
        s := ReadString;
        if Trim(s) <> '<EPANET2>' then
        begin
          raise EReadError.Create(MSG_READ_ERR);
        end;

      //Read version ID
        Version := ReadInteger;
        if (Version < VERSIONID1) or (Version > VERSIONID2) then
        begin
          raise EReadError.Create(MSG_READ_ERR);
        end;

      //Total up number of network components
        Ncomp := 0;
        for i := JUNCS to CNTRLS do Inc(Ncomp,ReadInteger);

      //Compute Main form's progress bar increment
        StepSize := Ncomp div (MainForm.ProgressBar.Max div
                    MainForm.ProgressBar.Step);
        Ncomp := 0;

      //Read in network options.
        Network.Options.Title := ReadString;
        ReadList(Reader,sList);
        Network.Options.Notes.Assign(sList);
        ReadArray(Reader,Network.Options.Data);
        QualParam := Uinput.GetQualParam;
        AutoLength := ReadBoolean;

      //Use default values for new options not included in earlier versions
        if (Version < VersionID2) then
        begin
          for i := HEAD_ERROR_INDEX to PRESSURE_EXP_INDEX do
            Network.Options.Data[i] := DefOptions[i];
        end;

      //Check if Quality Time Step is in hours instead of minutes
        with Network.Options do
        begin
          Qstep := Uutils.StrHoursToFloat(Data[QUAL_TSTEP_INDEX]);
          if Qstep > 1 then
            Data[QUAL_TSTEP_INDEX] := Uutils.GetTimeString(Round(Qstep*60));
        end;

      //Read in time patterns.
        n := ReadInteger;
        if n > 0 then for i := 0 to n-1 do
        begin
          MainForm.UpdateProgressBar(Ncomp,StepSize);
          pat := TPattern.Create;
          id := ReadString;
          pat.Comment := ReadString;
          ReadList(Reader,pat.Multipliers);
          Network.Lists[PATTERNS].AddObject(id,pat);
        end;

      //Read in curves.
        n := ReadInteger;
        if n > 0 then for i := 0 to n-1 do
        begin
          MainForm.UpdateProgressBar(Ncomp,StepSize);
          curve := TCurve.Create;
          id := ReadString;
          curve.Comment := ReadString;
          curve.Ctype := ReadString;
          ReadList(Reader,curve.Xdata);
          ReadList(Reader,curve.Ydata);
          Network.Lists[CURVES].AddObject(id,curve);
        end;

      //Read in junctions.
        n := ReadInteger;
        if n > 0 then for i := 0 to n-1 do
        begin
          MainForm.UpdateProgressBar(Ncomp,StepSize);
          junc := TJunc.Create;
          id := ReadString;
          junc.X := ReadFloat;
          junc.Y := ReadFloat;
          ReadArray(Reader,junc.Data);
          ReadList(Reader,junc.Demands);
          Network.Lists[JUNCS].AddObject(id,junc);
          nList.AddObject(id,junc);
        end;

      //Read in reservoirs
        n := ReadInteger;
        if n > 0 then for i := 0 to n-1 do
        begin
          MainForm.UpdateProgressBar(Ncomp,StepSize);
          node := TNode.Create;
          id := ReadString;
          node.X := ReadFloat;
          node.Y := ReadFloat;
          ReadArray(Reader,node.Data);
          Network.Lists[RESERVS].AddObject(id,node);
          nList.AddObject(id,node);
        end;

      //Read in tanks
        n := ReadInteger;
        if n > 0 then for i := 0 to n-1 do
        begin
          MainForm.UpdateProgressBar(Ncomp,StepSize);
          node := TNode.Create;
          id := ReadString;
          node.X := ReadFloat;
          node.Y := ReadFloat;
          ReadArray(Reader,node.Data);
          //Use default value for new option not included in earlier versions
          if (Version < VersionID2)
          then node.Data[TANK_OVERFLOW_INDEX] := 'No';
          Network.Lists[TANKS].AddObject(id,node);
          nList.AddObject(id,node);
        end;

      //Read in pipes, pumps, & valves
        for j := PIPES to VALVES do
        begin
          n := ReadInteger;
          if n > 0 then for i := 0 to n-1 do
          begin
            MainForm.UpdateProgressBar(Ncomp,StepSize);
            link := TLink.Create;
            id := ReadString;
            s := ReadString;
            p := nList.IndexOf(s);
            if p >= 0 then link.Node1 := TNode(nList.Objects[p]);
            s := ReadString;
            p := nList.IndexOf(s);
            if p >= 0 then link.Node2 := TNode(nList.Objects[p]);
            ReadVertices(Reader,link);
            ReadArray(Reader,link.Data);
            Network.Lists[j].AddObject(id,link);
          end;
        end;

      //Read in control rules.
        ReadList(Reader,Network.SimpleControls);
        ReadList(Reader,Network.RuleBasedControls);

      //Read in map labels.
        n := ReadInteger;
        if n > 0 then for i := 0 to n-1 do
        begin
          mlabel := TMapLabel.Create;
          id := ReadString;
          mlabel.X := ReadFloat;
          mlabel.Y := ReadFloat;
          s := ReadString;
          p := nList.IndexOf(s);
          if p >= 0 then mlabel.Anchor := TNode(nList.Objects[p]);
          mlabel.FontName := ReadString;
          mlabel.FontSize := ReadInteger;
          mlabel.FontBold := ReadBoolean;
          mlabel.FontItalic := ReadBoolean;
          mlabel.MeterType := ReadInteger;
          mlabel.MeterId := ReadString;
          Network.Lists[LABELS].AddObject(id,mlabel);
        end;

      //Read in map dimensions
        with MapDimensions do
        begin
          LowerLeft.X := ReadFloat;
          LowerLeft.Y := ReadFloat;
          UpperRight.X := ReadFloat;
          UpperRight.Y := ReadFloat;
          n := ReadInteger;
          Units := TMapUnits(n);
        end;

      //Read in map backdrop information
        with MapBackdrop do
        begin
          Filename := ReadString;
          Offset.X := ReadFloat;
          Offset.Y := ReadFloat;
        end;

      //Read in map display options
        with MapOptions do
        begin
          DispNodeIDs      := ReadBoolean;
          DispNodeValues   := ReadBoolean;
          DispNodesBySize  := ReadBoolean;
          DispNodeBorder   := ReadBoolean;
          DispLinkIDs      := ReadBoolean;
          DispLinkValues   := ReadBoolean;
          DispLinksBySize  := ReadBoolean;
          DispJuncs        := ReadBoolean;
          DispTanks        := ReadBoolean;
          DispPumps        := ReadBoolean;
          DispValves       := ReadBoolean;
          DispEmitters     := ReadBoolean;
          DispSources      := ReadBoolean;
          DispLabels       := ReadBoolean;
          LabelsTranspar   := ReadBoolean;
          NotationTranspar := ReadBoolean;
          NodeSize         := ReadInteger;
          LinkSize         := ReadInteger;
          ArrowStyle       := TArrowStyle(ReadInteger);
          ArrowSize        := ReadInteger;
          ColorIndex       := ReadInteger;
          NotationZoom     := ReadInteger;
          LabelZoom        := ReadInteger;
          SymbolZoom       := ReadInteger;
          ArrowZoom        := ReadInteger;
        end;
        n := ReadInteger;
        for i := 0 to n-1 do MapNodeColor[i] := ReadInteger;
        n := ReadInteger;
        for i := 0 to n-1 do MapLinkColor[i] := ReadInteger;

      //Read in calibration file names.
        for i := Low(NodeCalibData) to High(NodeCalibData) do
          NodeCalibData[i].FileName := ReadString;
        for i := Low(LinkCalibData) to High(LinkCalibData) do
          LinkCalibData[i].FileName := ReadString;
        RegisterCalibData;

      //Read in default properties
        IDIncrement := ReadInteger;
        ReadArray(Reader,IDPrefix);
        for i := JUNCS to VALVES do
          ReadArray(Reader,DefProp[i].Data);

      // Read legend intervals
        for i := ELEVATION to NODEVIEWS do
          for j := 1 to MAXINTERVALS do
            NodeLegend[i].Intervals[j] := ReadFloat;
        for i := DIAMETER to LINKVIEWS do
          for j := 1 to MAXINTERVALS do
            LinkLegend[i].Intervals[j] := ReadFloat;
        Result := FILE_OK;

      // Read printed Page Layout info
        with PageLayout do
        begin
          LMargin := ReadFloat;
          TMargin := ReadFloat;
          RMargin := ReadFloat;
          BMargin := ReadFloat;
        end;
        with MainForm.PageSetupDialog do
        begin
          Header.Text := ReadString;
          Header.Alignment := TAlignment(ReadInteger);
          Header.Enabled := ReadBoolean;
          Footer.Text := ReadString;
          Footer.Alignment := TAlignment(ReadInteger);
          Footer.Enabled := ReadBoolean;
          PageNumbers := TPageNumbers(ReadInteger);
        end;
        TitleAsHeader := ReadBoolean;
        Orientation := ReadInteger;

      except
        Result := FILE_NO_NET;
      end;

    finally
      Reader.Free;
    end;

  finally
    nList.Free;
    sList.Free;
    FileStream.Free;
  end;
end;

procedure SetNodeIDPtrs;
//-----------------------------------------------
// Saves pointer to ID label for each node object
//-----------------------------------------------
var
  i,j: Integer;
begin
  for i := JUNCS to TANKS do
  begin
    with Network.Lists[i] do
      for j := 0 to Count-1 do
        TNode(Objects[j]).ID := PChar(Strings[j]);
  end;
end;

function OpenProject(var Fname: String): TInputFileType;
{------------------------------------------}
{ Reads in a network data base from file.  }
{------------------------------------------}
var
  R: Integer;
begin
// Show progress meter
  Screen.Cursor := crHourGlass;
  MainForm.ShowProgressBar(MSG_READING_INPUT);

// Try to read file as a .NET binary input file
  Result := iftNone;
  R := ReadNetFile(Fname);

// File can't be opened
  if R = FILE_NO_OPEN then
  begin
    Uutils.MsgDlg(FILE_ERR3a + Fname + '.' + #10 + #10 + FILE_ERR2b,
      mtError, [mbOK]);
  end

// File not a .NET file;
// try reading it as a .INP text file.
  else if R = FILE_NO_NET then
  begin
    if Uimport.ReadInpFile(Fname) then Result := iftINP
    else Uutils.MsgDlg(FILE_ERR4a + Fname + '.' + #10 + #10 + FILE_ERR4b,
      mtError, [mbOK]);
  end

// File is a .NET file;
// create a backup if called for.
  else
  begin
    Result := iftNET;
    if AutoBackup then
      CopyFile(PChar(Fname),PChar(ChangeFileExt(Fname,'.bak')), FALSE);
  end;

// If file read successfully, save pointers to node ID strings.
  if Result <> iftNone then SetNodeIDPtrs;

// Hide progress meter.
  MainForm.HideProgressBar;
  Screen.Cursor := crDefault;
end;


{********************************************************************}
{        Procedures for Registering Calibration Data Files           }
{********************************************************************}

procedure RegisterLocations(const Fname: String; Locations: TStringList);
//-----------------------------------------------------------------
// Saves ID of locations that appear in the calibration file Fname.
// Locations are saved to a StringList in Name=Value format, where
// Value = 1 if location is active and 0 if not.
//-----------------------------------------------------------------
var
  ntoks, p : Integer;
  toklist  : TStringList;
  F        : TextFile;
  Line     : String;
  S        : String;
begin
//Clear Locations list & check if file can be accessed
  Locations.Clear;
  if (Length(Fname) = 0) then exit;
  if FileExists(Fname) then
  begin
    AssignFile(F,Fname);
    {$I-}
    Reset(F);
    {$I+}
    if (IOResult = 0) then
    begin

    //Create token list and read each line of file
      toklist := TStringList.Create;
      try
         while not EOF(F) do
         begin
           Readln(F,Line);
           S := Line;

         //Tokenize the line after stripping
         //off any comment which starts with ';'
           p := Pos(';',S);
           if (p > 0) then Delete(S,p,256);
           Uutils.Tokenize(S,toklist,ntoks);

         //If line has 3 tokens, then 1st is node/link ID.
         //Save in Locations list as ID=1
           if (ntoks >= 3) then with Locations do
             if (IndexOfName(toklist[0]) < 0) then
               Add(toklist[0]+'=1');
         end;
      finally
        toklist.Free;
      end;

    end

  // File access error
    else Uutils.MsgDlg(FILE_ERR2a + Fname + '.' + #13 + #13 + FILE_ERR2b,
           mtError, [mbOK]);
    CloseFile(F);
  end

// File does not exist error
  else Uutils.MsgDlg(FILE_ERR1a + Fname + FILE_ERR1b, mtError, [mbOK]);
end;

procedure RegisterCalibData;
//----------------------------
// Registers calibration data
//----------------------------
var
  i: Integer;
begin
  for i := Low(NodeCalibData) to High(NodeCalibData) do
    with NodeCalibData[i] do
      RegisterLocations(FileName,Locations);
  for i := Low(LinkCalibData) to High(LinkCalibData) do
    with LinkCalibData[i] do
      RegisterLocations(FileName,Locations);
end;

end.
