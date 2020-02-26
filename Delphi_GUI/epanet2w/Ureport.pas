unit Ureport;

{-------------------------------------------------------------------}
{                    Unit:    Ureport.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that writes full report to text file.        }
{-------------------------------------------------------------------}

interface

uses
  Forms, Controls, Messages, SysUtils, Dialogs, System.UITypes,
  Uutils, Uglobals;

procedure CreateFullReport;

implementation

uses FBrowser, Fmain, Uoutput;

const
  LogoTxt: array[0..5] of PChar =
(  '**********************************************************************',
   '*                             E P A N E T                            *',
   '*                     Hydraulic and Water Quality                    *',
   '*                     Analysis for Pipe Networks                     *',
   '*                           Version 2.2                              *',
   '**********************************************************************');
  FORMFEED = #12;
  PAGESIZE = 55;
  ULINE =
   '----------------------------------------------------------------------';
  FMT18  = '  Page 1                                          %22s';
  FMT82  = '  Page %-4d %60s';
  FMT71  = 'Energy Usage:';
  FMT72  = '                  Usage   Avg.     Kw-hr      Avg.      Peak      Cost';
  FMT73  = 'Pump             Factor Effic.     %s        Kw        Kw      /day';
  FMT74  = '%45s Demand Charge: %9.2f';
  FMT75  = '%45s Total Cost:    %9.2f';
  TXT_perM3 = '/m3';
  TXT_perMGAL = '/Mgal';
  TXT_CONTINUED =  ' (continued)';
  TXT_INPUT_FILE = 'Input File: ';
  TXT_LINK_INFO = 'Link - Node Table:';
  TXT_NODE_RESULTS = 'Node Results';
  TXT_LINK_RESULTS = 'Link Results';
  TXT_AT = ' at ';
  TXT_ID = 'ID';
  TXT_NODE = 'Node';
  TXT_LINK = 'Link';
  TXT_START = 'Start';
  TXT_END = 'End';
  MSG_REPORT_SIZE1 = 'This full report will use over ';
  MSG_REPORT_SIZE2 = ' Mbytes of disk space. Do you wish to proceed?';
  MSG_WRITING_REPORT = 'Writing full report...';
  MSG_NO_WRITE = 'Could not write full report to file ';
  TXT_REPORT_FILTER = 'Report files (*.RPT)|*.RPT|All files|*.*';

var
  F:         TextFile;
  RptTitle:  String;
  PageNum:   Integer;
  LineNum:   Integer;
  Progstep:  Integer;
  Nprogress: Integer;

procedure WriteLine(S: String);
begin
  try
    if (LineNum = PAGESIZE) then
    begin
      Inc(PageNum);
      Writeln(F);
      Write(F, FORMFEED);
      Writeln(F);
      Writeln(F, Format(FMT82, [PageNum, RptTitle]));
      LineNum := 3;
    end;
    Writeln(F,'  ', S);
    Inc(LineNum);
  except
    raise;
  end;
end;

procedure WriteLogo;
var
  I: Integer;
  S: String;
begin
  try
    PageNum := 1;
    LineNum := 2;
    S := Format(FMT18, [DateTimeToStr(Now)]);
    Writeln(F, S);
    for I := 0 to High(LogoTxt) do
      Writeline(LogoTxt[I]);
    Writeline('');
    Writeline(TXT_INPUT_FILE + ExtractFileName(InputFileName));
    Writeline('');
    with Network.Options do
    begin
      RptTitle := Title;
      Writeline(RptTitle);
      for I := 0 to Notes.Count-1 do
        Writeline(Notes[I]);
    end;
    Writeline('');
  except
    raise;
  end;
end;

procedure WriteEnergyHeader(ContinueFlag: Boolean);
var
  S:  String;
  S2: String;
begin
  try
    if LineNum + 11 > PAGESIZE then LineNum := PAGESIZE;
    if (FlowFlag > High(USFlowUnits)) then
      S2 := TXT_perM3
    else
      S2 := TXT_perMGAL;
    S := FMT71;
    if ContinueFlag then S := S + TXT_CONTINUED;
    Writeline(S);
    Writeline(ULINE);
    S := FMT72;
    Writeline(S);
    S := Format(FMT73,[S2]);
    Writeline(S);
    Writeline(ULINE);
  except
    raise;
  end;
end;

procedure WriteEnergy;
var
  j,k: Integer;
  x: array[0..5] of Single;
  Dcharge: Single;
  Csum: Single;
  S: String;
begin
  try
    WriteEnergyHeader(False);
    Csum := 0.0;
    for j := 0 to Network.Lists[PUMPS].Count-1 do
    begin
      k := Link(PUMPS,j).Zindex;
      if k < 0 then continue;
      Uoutput.GetPumpEnergy(k,x,Dcharge);
      Csum := Csum + x[5];
      if (LineNum = PageSize) then WriteEnergyHeader(True);
      S := Format('%-15s  %6.2f %6.2f %9.2f %9.2f %9.2f %9.2f',
          [GetID(PUMPS,j),x[0],x[1],x[2],x[3],x[4],x[5]]);
      Writeline(S);
    end;
    Writeline(ULINE);
    S := Format(FMT74,['', Dcharge]);
    Writeline(S);
    S := Format(FMT75, ['',Csum+Dcharge]);
    Writeline(S);
    Writeline('');
  except
    raise
  end;
end;

procedure WriteNodeHeader(const T: Integer; ContinueFlag: Boolean);
var
  S: String;
begin
  try
    if LineNum + 11 > PAGESIZE then LineNum := PAGESIZE;
    S := TXT_NODE_RESULTS;
    if Nperiods > 1 then S := S + TXT_AT + BrowserForm.TimeListBox.Items[T];
    S := S + ':';
    if ContinueFlag then S := S + TXT_CONTINUED;
    Writeline(S);
    Writeline(ULINE);
    S := Format('%-15s %10s%10s%10s%10s',[TXT_NODE, NodeVariable[DEMAND].Name,
      NodeVariable[HEAD].Name, NodeVariable[PRESSURE].Name,
        NodeVariable[NODEQUAL].Name]);
    Writeline(S);
    S := Format('%-15s %10s%10s%10s%10s',[TXT_ID, NodeUnits[DEMAND].Units,
      NodeUnits[HEAD].Units, NodeUnits[PRESSURE].Units,
        NodeUnits[NODEQUAL].Units]);
    Writeline(S);
    Writeline(ULINE);
  except
    raise;
  end;
end;

procedure WriteLinkHeader(const T: Integer; ContinueFlag: Boolean);
var
  S: String;
begin
  try
    if LineNum + 11 > PAGESIZE then LineNum := PAGESIZE;
    S := TXT_LINK_RESULTS;
    if Nperiods > 1 then S := S + TXT_AT + BrowserForm.TimeListBox.Items[T];
    S := S + ':';
    if ContinueFlag then S := S + TXT_CONTINUED;
    Writeline(S);
    Writeline(ULINE);
    S := Format('%-15s %10s%10s%10s%10s',[TXT_LINK, LinkVariable[FLOW].Name,
      LinkVariable[VELOCITY].Name, LinkVariable[HEADLOSS].Name,
        LinkVariable[LINKSTAT].Name]);
    Writeline(S);
    S := Format('%-15s %10s%10s%10s',[TXT_ID, LinkUnits[FLOW].Units,
      LinkUnits[VELOCITY].Units, LinkUnits[HEADLOSS].Units]);
    Writeline(S);
    Writeline(ULINE);
  except
    raise;
  end;
end;

procedure WriteLinkInfoHeader(ContinueFlag: Boolean);
var
  S: String;
begin
  try
    S := TXT_LINK_INFO;
    if ContinueFlag then S := S + TXT_CONTINUED;
    Writeline(S);
    Writeline(ULINE);
    S := Format('%-15s%-15s%-15s%10s%10s',[TXT_LINK, TXT_START, TXT_END,
      LinkVariable[LINKLENGTH].Name, LinkVariable[DIAMETER].Name]);
    Writeline(S);
    S := Format('%-15s%-15s%-15s%10s%10s',[TXT_ID, TXT_NODE, TXT_NODE,
      LinkUnits[LINKLENGTH].Units, LinkUnits[DIAMETER].Units]);
    Writeline(S);
    Writeline(ULINE);
  except
    raise;
  end;
end;

procedure WriteLinkInfo;
var
  I,J: Integer;
  ID,N1,N2,L,D,S: String;
  aLink: TLink;
begin
  try
    WriteLinkInfoHeader(False);
    for I := PIPES to VALVES do
    begin
      for J := 0 to Network.Lists[I].Count-1 do
      begin
        MainForm.UpdateProgressBar(Nprogress, ProgStep);
        if Link(I,J).Zindex < 0 then continue;
        aLink := Link(I,J);
        ID := GetID(I,J);
        N1 := aLink.Node1.ID;
        N2 := aLink.Node2.ID;
        L := Uoutput.GetLinkValStr(LINKLENGTH,0,I,J);
        D := Uoutput.GetLinkValStr(DIAMETER,0,I,J);
        if (LineNum = PageSize) then WriteLinkInfoHeader(True);
        S := Format('%-15s%-15s%-15s%10s%10s',[ID,N1,N2,L,D]);
        if I > PIPES then S := S + ' ' + ObjectLabel[I];
        Writeline(S);
      end;
    end;
    Writeline('');
  except
    raise;
  end;
end;

procedure WriteNodeTable(const T: Integer);
var
  I,J: Integer;
  ID,D,H,P,C,S: String;
begin
  try
    WriteNodeHeader(T,False);
    for I := JUNCS to TANKS do
    begin
      for J := 0 to Network.Lists[I].Count-1 do
      begin
        MainForm.UpdateProgressBar(Nprogress, ProgStep);
        if Node(I,J).Zindex < 0 then continue;
        ID := GetID(I,J);
        D := Uoutput.GetNodeValStr(DEMAND,T,I,J);
        H := Uoutput.GetNodeValStr(HEAD,T,I,J);
        P := Uoutput.GetNodeValStr(PRESSURE,T,I,J);
        C := Uoutput.GetNodeValStr(NODEQUAL,T,I,J);
        if (LineNum = PageSize) then WriteNodeHeader(T,True);
        S := Format('%-15s %10s%10s%10s%10s',[ID,D,H,P,C]);
        if I > JUNCS then S := S + ' ' + ObjectLabel[I];
        Writeline(S);
      end;
    end;
    Writeline('');
  except
    raise;
  end;
end;

procedure WriteLinkTable(const T: Integer);
var
  I,J: Integer;
  ID,Q,V,H,S,R: String;
begin
  try
    WriteLinkHeader(T,False);
    for I := PIPES to VALVES do
    begin
      for J := 0 to Network.Lists[I].Count-1 do
      begin
        MainForm.UpdateProgressBar(Nprogress, ProgStep);
        if Link(I,J).Zindex < 0 then continue;
        ID := GetID(I,J);
        Q := Uoutput.GetLinkValStr(FLOW,T,I,J);
        V := Uoutput.GetLinkValStr(VELOCITY,T,I,J);
        H := Uoutput.GetLinkValStr(HEADLOSS,T,I,J);
        S := Uoutput.GetLinkValStr(LINKSTAT,T,I,J);
        if (LineNum = PageSize) then WriteLinkHeader(T,True);
        R := Format('%-15s %10s%10s%10s%10s',[ID,Q,V,H,S]);
        if I > PIPES then R := R + ' ' + ObjectLabel[I];
        Writeline(R);
      end;
    end;
    Writeline('');
  except
    raise;
  end;
end;

procedure WriteResults;
var
  N: Integer;
begin
  try
    for N := 0 to Nperiods-1 do
    begin
      Application.ProcessMessages;
      WriteNodeTable(N);
      Application.ProcessMessages;
      WriteLinkTable(N);
    end;
  except
    raise;
  end;
end;


function WriteReport(Fname: String): Boolean;
var
  Total: Single;
  N: Integer;
begin
  Result := False;
  MainForm.ShowProgressBar(MSG_WRITING_REPORT);
  AssignFile(F,Fname);
  {$I-}
  Rewrite(F);
  {$I+}
  if (IOResult = 0) then
  try
    Total := Nnodes + Nlinks;
    Total := Total * Nperiods + Nlinks;
    with MainForm.ProgressBar do
      N := Max div Step;
    ProgStep := Round(Total/N);
    Nprogress := 0;
    WriteLogo;
    Application.ProcessMessages;
    WriteLinkInfo;
    if Npumps > 0 then WriteEnergy;
    WriteResults;
    Result := True;
  finally
  end;
  CloseFile(F);
  MainForm.HideProgressBar;
end;

procedure CreateFullReport;
var
  R: Boolean;
  Size: Single;
  Fname: String;
begin
// Check for huge file size
  Size := (Nlinks + (Nnodes + Nlinks)*Nperiods)*60*1e-6;
  if Size > 10 then
    if Uutils.MsgDlg(MSG_REPORT_SIZE1 + IntToStr(Trunc(Size)) +
      MSG_REPORT_SIZE2, mtConfirmation, [mbYes,mbNo], MainForm) = mrNo
        then Exit;

// Get a report file name
  Fname := '';
  with MainForm.SaveDialog do
  begin

  // Set options for Save File dialog
    Filter := TXT_REPORT_FILTER;
    if Length(InputFileName) > 0 then Filename :=
      ChangeFileExt(ExtractFileName(InputFileName),'.rpt')
    else Filename := '*.rpt';

  // Execute Save File dialog & write report to file
    if Execute then Fname := Filename;
    if Length(Fname) > 0 then
    begin
      Screen.Cursor := crHourGlass;
      R := WriteReport(Filename);
      Screen.Cursor := crDefault;
      if not R then
        Uutils.MsgDlg(MSG_NO_WRITE + ExtractFileName(Filename),
          mtError, [mbOK], MainForm);
    end;
  end;
end;

end.
