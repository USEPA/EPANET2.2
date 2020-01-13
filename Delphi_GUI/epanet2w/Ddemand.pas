unit Ddemand;

{-------------------------------------------------------------------}
{                    Unit:    Ddemand.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{  Form unit with a dialog box for editing multiple demand          }
{  categories for a network junction object.                        }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, Uglobals, ExtCtrls;

const
  INITROWS = 10;  // Initial number of rows displayed in the grid
  ColLabels: PChar = ' '#13'Base Demand'#13'Time Pattern'#13'Category';

type
  TDemandsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
  private
    { Private declarations }
    theJunc: TJunc;
  public
    { Public declarations }
    Modified: Boolean;
    procedure LoadDemands;
    procedure UnLoadDemands;
  end;

//var
//  DemandsForm: TDemandsForm;

implementation

{$R *.DFM}

procedure TDemandsForm.FormCreate(Sender: TObject);
//--------------------------------------------------
// Form's OnCreate event handler.
//--------------------------------------------------
var
  cxvscroll: Integer;
  i, w: Integer;

begin
// Assign current global font to form
  Uglobals.SetFont(self);

// Determine width of vertical scrollbar
  cxvscroll := GetSystemMetrics(SM_CXVSCROLL);

// Size the grid's columns and set text in its header row
  with StringGrid1 do
  begin
    w := ((Width - DefaultColWidth - cxvscroll) div
         (ColCount-1)) - 2;
    for i := 1 to ColCount - 1 do ColWidths[i] := w;
    RowCount := INITROWS + 1;
    Rows[0].SetText(ColLabels);
  end;

// Get pointer to the current junction being edited
  theJunc := TJunc(Network.Lists[JUNCS].Objects[CurrentItem[JUNCS]]);
end;

procedure TDemandsForm.FormShow(Sender: TObject);
//------------------------------------------------
// Form's OnShow event handler.
//------------------------------------------------
begin
  StringGrid1.SetFocus;
end;

procedure TDemandsForm.Button1Click(Sender: TObject);
//----------------------------------------------------
// OnClick event handler for OK button.
//----------------------------------------------------
begin
  UnloadDemands;
  ModalResult := mrOK;
end;

procedure TDemandsForm.Button2Click(Sender: TObject);
//----------------------------------------------------
// OnClick event handler for the Cancel button.
//----------------------------------------------------
begin
  ModalResult := mrCancel;
end;

procedure TDemandsForm.LoadDemands;
//------------------------------------------------------
// Loads current set of demand categories into the grid.
//------------------------------------------------------
var
  i, n : Integer;
  s    : String;

begin
  with StringGrid1 do
  begin

  // Get number of current demand categories for junction
    n := theJunc.Demands.Count;
    if n >= RowCount then RowCount := n + 1;

  // Place index numbers in Column 0 of grid
    for i := 1 to RowCount - 1 do Cells[0,i] := IntToStr(i);

  // Place current set of categories in the grid
    if n > 0 then for i := 0 to n-1 do
    begin
      s := IntToStr(i+1) + #13 + theJunc.Demands[i];
      Rows[i+1].SetText(PChar(s));
    end;

  // Update entries for the primary demand category since
  // they can be edited separately in the Property Editor
    Cells[1,1] := theJunc.Data[JUNC_DEMAND_INDEX];
    Cells[2,1] := theJunc.Data[JUNC_PATTERN_INDEX];
  end;
  Modified := False;
end;

procedure TDemandsForm.UnLoadDemands;
//------------------------------------------------------
// Unloads updated set of demand category data from the
// grid into the junction's Demands list data structure.
//------------------------------------------------------
var
  FirstFlag: Boolean;
  i : Integer;
  d, p, c: String;

begin
// Clear the Demands list
  FirstFlag := False;
  theJunc.Demands.Clear;

  with StringGrid1 do
  begin
  // Extract demands from each row of the grid
    for i := 1 to RowCount - 1 do
    begin

    // Transfer contents of grid cells to scratch strings
    // adding trailing spaces so blanks get included
      d := Trim(Cells[1,i]);
      p := Trim(Cells[2,i]) + ' ';
      c := Trim(Cells[3,i]) + ' ';

    // Skip rows with no demand entered
      if Length(d) > 0 then
      begin

      // Update junction's primary demand properties if
      // this is first demand found
        if not FirstFlag then
        begin
          FirstFlag := True;
          theJunc.Data[JUNC_DEMAND_INDEX] := d;
          theJunc.Data[JUNC_PATTERN_INDEX] := p;
        end;

      // Add new demand record to list
        theJunc.Demands.Add(d + #13 + p + #13 + c);
      end;
    end;
  end;

// If list empty, add a zero demand record to the list
  if theJunc.Demands.Count = 0 then
  begin
    theJunc.Demands.Add('0' + #13 + ' ' + #13 + ' ');
    theJunc.Data[JUNC_DEMAND_INDEX] := '0';
    theJunc.Data[JUNC_PATTERN_INDEX] := ' ';
  end;

// Update number of demand categories in the Property Editor
  theJunc.Data[JUNC_DMNDCAT_INDEX] := IntToStr(theJunc.Demands.Count);
end;

procedure TDemandsForm.StringGrid1KeyPress(Sender: TObject;
  var Key: Char);
//----------------------------------------------------------
// OnKeyPress event handler for StringGrid1.
// Adds a new row to the grid when user is in the current
// last row and presses the Enter key.
//-----------------------------------------------------------
begin
  if (Key = #13) then with Sender as TStringGrid do
  begin
    if (Row = RowCount-1) then
    begin
      RowCount := RowCount + 1;
      Cells[0,RowCount-1] := IntToStr(RowCount-1);
      Refresh;
      Row := Row + 1;
      Col := 1;
    end;
  end;
end;

procedure TDemandsForm.StringGrid1SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
//-------------------------------------------------------------------  
// OnSetEditText event handler for StringGrid1.
// Updates the Modified flag when text in the grid is edited.
//--------------------------------------------------------------------
begin
  Modified := True;
end;

procedure TDemandsForm.Button3Click(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 216);
end;

end.
