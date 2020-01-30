unit Fsimul;

{-------------------------------------------------------------------}
{                    Unit:    Fsimul.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit used to execute the network hydraulic and water       }
{   quality solver (contained in  EPANET2.DLL) and display its      }
{   progress.                                                       }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Consts, System.UITypes,
  Uglobals, Uutils, Epanet2;

const
  TXT_STATUS_RUNING = 'Runing EPANET simulator...';
  TXT_STATUS_NONE = 'Unable to run simulator.';
  TXT_STATUS_WRONGVERSION = 'Run was unsuccessful. Wrong version of simulator.';
  TXT_STATUS_FAILED = 'Run was unsuccessful due to system error.';
  TXT_STATUS_ERROR = 'Run was unsuccessful. See Status Report for reasons.';
  TXT_STATUS_WARNING =
    'Warning messages were generated. See Status Report for details.';
  TXT_STATUS_SUCCESS = 'Run was successful.';
  TXT_STATUS_SHUTDOWN =
   'Simulator performed an illegal operation and was shut down.';
  TXT_STATUS_CANCELLED = 'Run cancelled by user.';
  TXT_COMPILING = 'Compiling network data...';
  TXT_CHECKING = 'Checking network data...';
  TXT_REORDERING = 'Re-ordering network nodes...';
  TXT_SOLVING_HYD = 'Solving hydraulics at hour';
  TXT_SAVING_HYD  = 'Saving hydraulics at hour';
  TXT_SOLVING_WQ  = 'Solving quality at hour';

type
  TSimulationForm = class(TForm)
    StatusLabel: TLabel;
    OKbtn: TButton;
    CancelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OKbtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure DisplayRunStatus;
    function  RunHydraulics: Integer;
    procedure RunQuality;
  public
    { Public declarations }
    procedure Execute;
  end;

var
  SimulationForm: TSimulationForm;   // Don't comment this out!

implementation

{$R *.DFM}

uses
  Fmain, Uexport, Uoutput;

procedure TSimulationForm.FormCreate(Sender: TObject);
//---------------------------------------------------
// OnCreate handler for form
//---------------------------------------------------
begin
// Set font size and style
  Uglobals.SetFont(self);

// Hide OK button
  OKBtn.Visible := False;
  CancelBtn.Top := OKBtn.Top;
  CancelBtn.Left := OKBtn.Left;

// Assign form variable for use in viewstatus() procedure
  SimulationForm := self;
end;


procedure TSimulationForm.FormActivate(Sender: TObject);
//------------------------------------------------------
// OnActivate handler for form
//------------------------------------------------------
var
  OldDir: String;
begin
// Change to temporary directory
  GetDir(0,OldDir);
  ChDir(TempDir);

// Update the form's display
  Update;

// Execute the simulation
  Execute;

// Restore original directory
  ChDir(OldDir);

// Hide Cancel button & enable OK button
  CancelBtn.Visible := False;
  OKBtn.Visible := True;
  OKbtn.SetFocus;

end;


procedure TSimulationForm.OKbtnClick(Sender: TObject);
//---------------------------------------------------
// OnClick procedure for OK button
//---------------------------------------------------
begin
  Hide;
end;


procedure TSimulationForm.CancelBtnClick(Sender: TObject);
//---------------------------------------------------
// OnClick procedure for Cancel button
//---------------------------------------------------
begin
  RunStatus := rsCancelled;
end;


procedure TSimulationForm.Execute;
//--------------------------------------
// Implements the simulation
//--------------------------------------
var
  err: Integer;
  InpFile, RptFile, OutFile: AnsiString;  // Ansi string versions of file names

begin
// Save current input data to temporary file
  StatusLabel.Caption := TXT_COMPILING;
  StatusLabel.Refresh;
  Uexport.ExportDataBase(TempInputFile,False);

// Open solver and read in network data
  try
    StatusLabel.Caption := TXT_CHECKING;
    StatusLabel.Refresh;
    InpFile := AnsiString(TempInputFile);
    RptFile := AnsiString(TempReportFile);
    OutFile := AnsiString(TempOutputFile);
    err := ENopen(PAnsiChar(InpFile), PAnsiChar(RptFile), PAnsiChar(OutFile));

// Solve for hydraulics & water quality, then close solver
    if (err = 0) and (RunStatus <> rsCancelled) then  err := RunHydraulics;
    if (err = 0) and (RunStatus <> rsCancelled) then  RunQuality;
    ENclose;

// Close solver if an exception occurs
  except
    on E: Exception do
    begin
      Uutils.MsgDlg(E.Message, mtError, [mbOK]);
      ENclose;
      Runstatus := rsShutdown;
    end;
  end;

// Display run status
  DisplayRunStatus;
end;


function TSimulationForm.RunHydraulics: Integer;
//----------------------------------------------
// Runs hydraulic simulation
//----------------------------------------------
var
  err: Integer;
  t, tstep: Longint;
  h: Single;
  slabel: String;

begin
// Open hydraulics solver
  err := 0;
  StatusLabel.Caption := TXT_REORDERING;
  StatusLabel.Refresh;
  try
    if ENopenH() = 0 then
    begin

    // Initialize hydraulics solver
      ENinitH(1);
      h := 0;
      slabel := TXT_SOLVING_HYD;

    // Solve hydraulics in each period
      repeat
        StatusLabel.Caption := Format('%s %.2f',[slabel,h]);
        Application.ProcessMessages;
        err := ENrunH(t);
        tstep := 0;
        if err <= 100 then err := ENnextH(tstep);
        h := h + tstep/3600;
      until (tstep = 0) or (err > 100) or (RunStatus = rsCancelled);
    end;

  // Close hydraulics solver & ignore warning conditions
    ENcloseH();
    if err <= 100 then err := 0;
    Result := err;

// Exception handler
  except
    ENcloseH();
    raise;
  end;
end;


procedure TSimulationForm.RunQuality;
//----------------------------------------------
// Runs water quality simulation
//----------------------------------------------
var
  err: Integer;
  t, tstep: Longint;
  h: Single;
  slabel: String;

begin
// Open WQ solver
  h := 0;
  if UpperCase(Trim(Network.Options.Data[QUAL_PARAM_INDEX])) = 'NONE'
  then slabel := TXT_SAVING_HYD
  else slabel := TXT_SOLVING_WQ;
  try
    if ENopenQ() = 0 then
    begin

  // Initialize WQ solver & solve WQ in each period
      ENinitQ(1);
      repeat
        StatusLabel.Caption := Format('%s %.2f',[slabel,h]);
        err := ENrunQ(t);
        tstep := 0;
        if err <= 100 then err := ENnextQ(tstep);
        h := h + tstep/3600;
        Application.ProcessMessages;
      until (tstep = 0) or (err > 100) or (RunStatus = rsCancelled);
    end;

  // Close WQ solver & ignore warning conditions
    ENcloseQ();

  except
    ENcloseQ();
    raise;
  end;
end;


procedure TSimulationForm.DisplayRunStatus;
//----------------------------------------------
// Displays final status of simulation run
//----------------------------------------------
begin
// Retrieve final run status
  if not (RunStatus in [rsCancelled, rsShutdown]) then
  begin
    if GetFileSize(TempReportFile) <= 0 then RunStatus := rsFailed
    else RunStatus := Uoutput.CheckRunStatus(TempOutputFile);
  end;

// Display run status message
  case RunStatus of
    rsShutdown:     StatusLabel.Caption := TXT_STATUS_SHUTDOWN;
    rsNone:         StatusLabel.Caption := TXT_STATUS_NONE;
    rsWrongVersion: StatusLabel.Caption := TXT_STATUS_WRONGVERSION;
    rsFailed:       StatusLabel.Caption := TXT_STATUS_FAILED;
    rsError:        StatusLabel.Caption := TXT_STATUS_ERROR;
    rsWarning:      StatusLabel.Caption := TXT_STATUS_WARNING;
    rsSuccess:      StatusLabel.Caption := TXT_STATUS_SUCCESS;
    rsCancelled:    StatusLabel.Caption := TXT_STATUS_CANCELLED;
  end;
end;

end.
