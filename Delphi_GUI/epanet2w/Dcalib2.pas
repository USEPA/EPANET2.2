unit Dcalib2;

{-------------------------------------------------------------------}
{                    Unit:    Dcalib2.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box that retrieves options for          }
{   creating or updating a Calibration Report.                      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, checklst, Spin, ExtCtrls, Uglobals;

const
  TXT_AT_NODES = 'Measured at Nodes:';
  TXT_IN_LINKS = 'Measured in Links:';

type
  TCalibOptionsForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    GBVariable: TGroupBox;
    VariablesList: TComboBox;
    GBLocations: TGroupBox;
    LocationsList: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure VariablesListChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
    CalibType: Integer;
    CalibVar:  Integer;
    IndexList: TList;
  public
    { Public declarations }
    procedure GetOptions(var Ctype: Integer; var Cvar: Integer);
  end;

//var
//  CalibOptionsDlg: TCalibOptionsDlg;

implementation

{$R *.DFM}

var
  Nlinkvars: Integer;
  Nnodevars: Integer;

procedure TCalibOptionsForm.FormCreate(Sender: TObject);
//----------------------------------------------------
// OnCreate handler for form.
//----------------------------------------------------
var
  i, j: Integer;
begin

// Set font size & style
  Uglobals.SetFont(self);

// Create a TList to store indexes of variables with calib data
  IndexList := TList.Create;
  Nnodevars := High(NodeCalibData) - Low(NodeCalibData) + 1;
  Nlinkvars := High(LinkCalibData) - Low(LinkCalibData) + 1;
  try

  // Add node variable to VariablesList if calib data file exists
    for i := 0 to Nnodevars-1 do
    begin
      j := Low(NodeCalibData) + i;
      if FileExists(NodeCalibData[j].FileName) then
      begin
        VariablesList.Items.Add(NodeVariable[j].Name);
        IndexList.Add(Pointer(i));
      end;
    end;

  // Add link variable to VariablesList if calib data file exists
    for i := 0 to Nlinkvars-1 do
    begin
      j := Low(LinkCalibData) + i;
      if FileExists(LinkCalibData[j].FileName) then
      begin
        VariablesList.Items.Add(LinkVariable[j].Name);
        IndexList.Add(Pointer(Nnodevars+i));
      end;
    end;
  finally
  end;

// Initialize display of calib. variable & meas. locations
  if VariablesList.Items.Count > 0 then
  begin
    VariablesList.ItemIndex := 0;
    VariablesListChange(self);
  end;
end;

procedure TCalibOptionsForm.FormDestroy(Sender: TObject);
//------------------------------------------------------
// OnDestroy handler for form. Frees the IndexList.
//------------------------------------------------------
begin
  IndexList.Free;
end;

procedure TCalibOptionsForm.GetOptions(var Ctype: Integer; var Cvar: Integer);
//-------------------------------------------------------------
// Retrieves option selections from form. Called by procedure
// that creates the form.
// Ctype = NETNODES or NETLINKS (depending on Cvar)
// Cvar = choice of variable to include in Calibration Report
//-------------------------------------------------------------
type
  PCalibData = ^TCalibData;   // Pointer to TCalibData structure
var
  i: Integer;
  CalibData: PCalibData;
begin

// Save current choice of calibration variable
  Ctype := CalibType;
  Cvar := CalibVar;

// Obtain pointer to structure which holds calibration info for
// the chosen calibration variable
  if (Ctype = NETNODES) then
    CalibData := @NodeCalibData[Cvar]
  else
    CalibData := @LinkCalibData[Cvar];

// Update the list of locations to be included in the report.
  with LocationsList, CalibData^ do
  begin
    for i := 0 to Items.Count-1 do
    begin
      if (Checked[i]) then
        Locations.Values[Items[i]] := '1'
      else
        Locations.Values[Items[i]] := '0';
    end;
  end;
end;

procedure TCalibOptionsForm.VariablesListChange(Sender: TObject);
//-------------------------------------------------------------
// OnChange handler for VariablesList.
// Updates choice of calibration variable.
// Displays measurement locations for calib. data registered
// to the selected variable.
//
// As a reminder:
//   Each calibration variable has a TCalibData data structure
//   associated with it.
//   The FileName field of this structure holds the name of
//   the calibration file (which contains the measurement
//   locations, times, and measurements for the variable).
//   The Locations field holds a StringList with the locations
//   (node/link IDs) where measurements were taken.
//   The items in this list are stored in Name = Value format,
//   where Name = node/link ID and Value = '1' if the location
//   is included in the Calibration report and '0' if not.
//-------------------------------------------------------------
var
  i : Integer;
  CalibData: TCalibData;
begin

// Find calibration data corresonding to selected variable
  i := Integer(IndexList.Items[VariablesList.ItemIndex]);
  if i < Nnodevars then
  begin
    CalibType := NETNODES;
    CalibVar :=  Low(NodeCalibData) + i;
    CalibData := NodeCalibData[CalibVar];
    GBLocations.Caption := TXT_AT_NODES;
  end
  else
  begin
    CalibType := NETLINKS;
    CalibVar := Low(LinkCalibData) + i - Nnodevars;
    CalibData := LinkCalibData[CalibVar];
    GBLocations.Caption := TXT_IN_LINKS;
  end;

// Load measurement locations into the form's LocationsList
  with LocationsList, CalibData do
  begin
    Clear;
    Enabled := False;
    if FileExists(FileName) then
    begin
      for i := 0 to Locations.Count - 1 do
      begin
        Items.Add(Locations.Names[i]);
        Checked[i] := (Locations.Values[Items[i]] = '1');
      end;
      Enabled := True;
    end;
  end;
end;

procedure TCalibOptionsForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 306);
end;

end.
