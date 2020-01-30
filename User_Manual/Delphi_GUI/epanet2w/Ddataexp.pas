unit Ddataexp;

{-------------------------------------------------------------------}
{                    Unit:    Ddataexp.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box that selects data scenarios to      }
{   export to a file.                                               }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Uglobals;

const
  TXT_SAVE_SCENARIO_TITLE = 'Save Scenario As';
  TXT_SAVE_SCENARIO_FILTER = 'Scenario files (*.SCN)|*.SCN|All files|*.*';

type
  TDataExportForm = class(TForm)
    GroupBox1: TGroupBox;
    DemandsCheckBox: TCheckBox;
    RoughnessCheckBox: TCheckBox;
    QualityCheckBox: TCheckBox;
    ReactionsCheckBox: TCheckBox;
    ControlsCheckBox: TCheckBox;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    DiametersCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure ExportSelections(const Fname: String);
  public
    { Public declarations }
  end;

//var
//  DataExportForm: TDataExportForm;

implementation

{$R *.DFM}

uses Uexport;

procedure TDataExportForm.FormCreate(Sender: TObject);
//-----------------------------------------------
// Form's OnCreate event handler.
//-----------------------------------------------
begin
  Uglobals.SetFont(self);
  DemandsCheckBox.Checked := False;
  QualityCheckBox.Checked := False;
  DiametersCheckBox.Checked := False;
  RoughnessCheckBox.Checked := False;
  ReactionsCheckBox.Checked := False;
  ControlsCheckBox.Checked := False;
end;

procedure TDataExportForm.Button1Click(Sender: TObject);
//-------------------------------------------------
// OnClick event handler for the OK button.
// Invokes Save Dialog box to get name of
// file to export data to.
//-------------------------------------------------
begin
  with SaveDialog1 do
  begin
    Title := TXT_SAVE_SCENARIO_TITLE;
    Filter := TXT_SAVE_SCENARIO_FILTER;
    Filename := '*.scn';
    if Execute then ExportSelections(Filename);
  end;
end;

procedure TDataExportForm.ExportSelections(const Fname: String);
//---------------------------------------------------------
// Exports the selected data categories to file Fname.
//---------------------------------------------------------
var
  F     : TextFile;
  i     : Integer;

begin
// Make sure Fname can be written to
  AssignFile(F,Fname);
  {$I-}
  Rewrite(F);
  {$I+}
  if (IOResult = 0) then
  try

  // Add the comment lines to the file first
    with Memo1 do
      for i := 0 to Lines.Count-1 do
        Writeln(F,';' + Lines[i]);

  // Use procedures in Ufileio unit to write selected data to the file
    if DemandsCheckBox.Checked then Uexport.ExportAllDemands(F);
    if DiametersCheckBox.Checked then Uexport.ExportDiameters(F);
    if RoughnessCheckBox.Checked then Uexport.ExportRoughness(F);
    if QualityCheckBox.Checked then Uexport.ExportQuality(F);
    if ReactionsCheckBox.Checked then Uexport.ExportReactions(F,'[REACTIONS]');
    if ControlsCheckBox.Checked then Uexport.ExportControls(F);
  finally
  end;
  CloseFile(F);
end;

procedure TDataExportForm.Button3Click(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 279);
end;

end.
