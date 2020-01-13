unit Dlegend;

{-------------------------------------------------------------------}
{                    Unit:    Dlegend.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box for editing a map legend.           }
{                                                                   }
{   NOTE: built to accommodate only 5 colors.                       }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ColorGrd, ExtCtrls, Grids, StdCtrls, Buttons,
  System.UITypes, Uglobals, Uutils;

const
  MSG_RANGE_TOO_SMALL = 'Range too small.';
  MSG_NO_INTERVALS = 'No intervals specified.';

type
  TLegendForm = class(TForm)
    Box0: TShape;
    Box1: TShape;
    Box2: TShape;
    Box3: TShape;
    Box4: TShape;

    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    BtnIntervals: TButton;
    BtnColorRamp: TButton;
    BtnReverse: TButton;
    Panel1: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    ColorDialog1: TColorDialog;
    Hiliter: TShape;
    NameLabel: TLabel;
    UnitsLabel: TLabel;
    BtnQuantiles: TButton;
    Bevel1: TBevel;
    BtnHelp: TButton;
    CheckFramed: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure BtnIntervalsClick(Sender: TObject);
    procedure BtnColorRampClick(Sender: TObject);
    procedure BtnReverseClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnQuantilesClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure CheckFramedClick(Sender: TObject);
  private
    { Private declarations }
    LegendType  : Integer;
    LegendVar   : Integer;
    LegendPeriod: Integer;
    function  ValidateInput: Boolean;
  public
    { Public declarations }
    Modified: Boolean;
    procedure LoadData(const Sname: String;
      const Sunits: String; const TimePeriod: Integer;
      const Legend: TMapLegend; const Colors: array of TColor;
      const Framed: Boolean);
    procedure UnloadData(var Legend: TMapLegend;
      var Colors: array of TColor; var Framed: Boolean);
  end;

implementation

{$R *.DFM}

uses Fmain, Dcolramp, Uoutput;


procedure TLegendForm.FormCreate(Sender: TObject);
//------------------------------------------------
//  OnCreate handler for form.
//------------------------------------------------
var
  P: TPoint;
begin
// Adjust font to global setting
  Uglobals.SetFont(self);

// Adjust size of the highlighting box to
// fit around the color boxes
  Hiliter.Height := Box0.Height + 4;
  Hiliter.Width := Box0.Width + 4;

// Position form at top left of Main form
  with MainForm do
  begin
    P.x := Left + (Width - ClientWidth) - 2;
    P.Y := Top + (Height-ClientHeight) - 2;
  end;
  Top := P.Y;
  Left := P.X;
  Modified := False;
end;


function TLegendForm.ValidateInput: Boolean;
//-----------------------------------------------------
// Checks that form's text boxes contain valid numbers.
//-----------------------------------------------------
var
  i       : Integer;
  count   : Integer;  //Count of non-blank edit boxes
  number  : Single;   //Numeric value in an edit box
begin
  Result := False;
  count := 0;

// Check each edit component
  for i := 1 to MAXINTERVALS do
    with FindComponent('Edit' + IntToStr(i)) as TEdit do
    begin

    // If not blank and not a valid number then exit
      if Text <> '' then
      begin
        if not Uutils.IsValidNumber(Text,number) then
        begin
          SetFocus;
          Exit;
        end
        else Inc(count);
      end;
      if Modified then self.Modified := True;
    end;

// Issue warning message if no edit boxes filled in
  if count = 0 then
  begin
    Uutils.MsgDlg(MSG_NO_INTERVALS,mtWarning,[mbOK]);
    Edit1.SetFocus;
  end
  else
    Result := True;
end;


procedure TLegendForm.LoadData(const Sname: String;
  const Sunits: String; const TimePeriod: Integer;
  const Legend: TMapLegend; const Colors: array of TColor;
  const Framed: Boolean);
//-------------------------------------------------
//  Loads legend information into form.
//    Sname = map view variable's name
//    Sunits = map view variable's measurement units
//    TimePeriod = time period legend applies to
//    Legend = legend record
//    Colors = legend's colors
//    Framed = true if legend framed
//--------------------------------------------------
var
  i: Integer;
  s: String;
  d: Integer;
begin
// Save legend information
  NameLabel.Caption := Sname;
  UnitsLabel.Caption := Sunits;
  LegendPeriod := TimePeriod;
  LegendType := Legend.Ltype;
  LegendVar := Legend.ViewVar;
  CheckFramed.Checked := Framed;

// Determine how many decimal places to display
  if LegendType = NETNODES then
    d := NodeUnits[LegendVar].Digits
  else
    d := LinkUnits[LegendVar].Digits;

// Assign legend colors to shape controls
  for i := 0 to MAXINTERVALS do
    with FindComponent('Box' + IntToStr(i)) as TShape do
      Brush.Color := Colors[i];

// Assign legend interval values to edit controls
  for i := 1 to MAXINTERVALS do
  begin
    if Legend.Intervals[i] = MISSING then
      s := ''
    else
      s := FloatToStrF(Legend.Intervals[i], ffFixed, 7, d);
    with FindComponent('Edit' + IntToStr(i)) as TEdit do
      Text := s;
  end;

// Inactivate Equal Intervals & Equal Quantiles
// buttons if no computed results exist.
  if (not Runflag) then
  begin
    if ((LegendType = NETNODES) and
        (NodeVariable[LegendVar].Source = vsOutput))
    or ((LegendType = NETLINKS) and
        (LinkVariable[LegendVar].Source = vsOutput))
    then
    begin
      BtnIntervals.Enabled := False;
      BtnQuantiles.Enabled := False;
    end;
  end;
end;


procedure TLegendForm.UnLoadData(var Legend: TMapLegend;
  var Colors: array of TColor; var Framed: Boolean);
//------------------------------------------------------
// Unloads contents of form into Legend structure,
// consolidating any gaps left by blank entries.
//------------------------------------------------------
var
  i,n   : Integer;
  lasti : Integer;
  code  : Integer;
  number: Single;
  s     : String;
begin
// Initialize count of non-blank intervals
  n := 0;
  lasti := 1;

// Examine each interval's edit control
  for i := 1 to MAXINTERVALS do
  begin

  // Initialize legend interval to MISSING (equivalent to blank)
    Legend.Intervals[i] := MISSING;

  // Retrieve text from edit control
    with FindComponent('Edit' + IntToStr(i)) as TEdit do
      s := Text;

  // If text not blank, then update Legend structure
    if s <> '' then
    begin

    // Save index of this edit control
      lasti := i;

    // Store color of shape control appearing above edit control
      with FindComponent('Box' + IntToStr(i-1)) as TShape do
        Colors[n] := Brush.Color;

    // Increment non-blank interval count & store new interval value
      Inc(n);
      val(s,number,code);
      Legend.Intervals[n] := number;
    end;
  end;

// Store color of shape control appearing below last non-blank interval
  with FindComponent('Box' + IntToStr(lasti)) as TShape do
    Colors[n] := Brush.Color;

// Store number of non-blank intervals
  Legend.Nintervals := n;
  Framed := CheckFramed.Checked;
end;


procedure TLegendForm.BtnIntervalsClick(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for Equal Intervals button.
// Divides range of Legend variable into equal intervals.
//-------------------------------------------------------
var
  d, i, n     : Integer;
  dx, x       : Single;
  xmin, xmax  : Single;
begin
  xmin := 1e20;
  xmax := 0;
  if (LegendType = NETNODES) then
  begin
    d := NodeUnits[LegendVar].Digits;
    Uoutput.GetNodeMinMax(LegendVar,LegendPeriod,xmin,xmax);
  end
  else
  begin
    d := LinkUnits[LegendVar].Digits;
    Uoutput.GetLinkMinMax(LegendVar,LegendPeriod,xmin,xmax);
  end;
  if (xmax = MISSING) then
    Uutils.MsgDlg(MSG_RANGE_TOO_SMALL,mtWarning,[mbOK])
  else
  begin
    try
      n := Trunc(xmin/10);
      xmin := 10*n;
      if (xmax < 10) then xmax := Trunc(xmax) + 1
      else
      begin
        n := Trunc(xmax/10) + 1;
        xmax := 10*n;
      end;
      dx := (xmax - xmin)/(MAXINTERVALS+1);
    except
      Uutils.MsgDlg(MSG_RANGE_TOO_SMALL,mtWarning,[mbOK]);
      Exit;
    end;
    for i := 1 to MAXINTERVALS do
    begin
      x := xmin + i*dx;
      with FindComponent('Edit' + IntToStr(i)) as TEdit do
        Text := FloatToStrF(x, ffFixed, 7, d);
    end;
    Modified := True;
  end;
end;


procedure TLegendForm.BtnQuantilesClick(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for Equal Quantiles button.
// Divides range of Legend variable into intervals
// with equal number of items.
//-------------------------------------------------------
var
  d,i: Integer;
  X: array [1..MAXINTERVALS] of Single;
begin
  if (LegendType = NETNODES) then
  begin
    d := NodeUnits[LegendVar].Digits;
    Uoutput.GetNodeQuantiles(LegendVar,LegendPeriod,X);
  end
  else
  begin
    d := LinkUnits[LegendVar].Digits;
    Uoutput.GetLinkQuantiles(LegendVar,LegendPeriod,X);
  end;
  if (X[1] = X[MAXINTERVALS]) then
    Uutils.MsgDlg(MSG_RANGE_TOO_SMALL,mtWarning,[mbOK])
  else
  begin
    for i := 1 to MAXINTERVALS do
      with FindComponent('Edit' + IntToStr(i)) as TEdit do
        Text := FloatToStrF(X[i], ffFixed, 7, d);
    Modified := True;
  end;
end;


procedure TLegendForm.BtnColorRampClick(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for Color Ramp button.
// Selects colors from a set of pre-defined color schemes.
//-------------------------------------------------------
var
  I : Integer;
  N : Integer;
  ColorRampDlg : TColorRampForm;
begin

// Create the color ramp dialog form and display modally
  ColorRampDlg := TColorRampForm.Create(self);
  try
    if ColorRampDlg.ShowModal = mrOK then
    begin

    // Transfer colors from dialog's color boxes to Legend Editor's
      Modified := True;
      N := High(ColorRampDlg.Colors);
      if N > MAXINTERVALS then N := MAXINTERVALS;
      for I := 0 to N do
      try
        with self.FindComponent('Box' + IntToStr(I)) as TShape do
          Brush.Color := ColorRampDlg.Colors[I];
      finally
      end;
    end;

// Free the dialog form
  finally
    ColorRampDlg.Free;
  end;
end;


procedure TLegendForm.BtnReverseClick(Sender: TObject);
//----------------------------------------------------
// OnClick handler for Reverse Colors button.
// Reverses color ordering of legend's color boxes.
//----------------------------------------------------
var
  TmpColor: array [0..MAXINTERVALS] of TColor;
  I : Integer;
begin
  for I := 0 to MAXINTERVALS do
    with FindComponent('Box' + IntToStr(I)) as TShape do
      TmpColor[I] := Brush.Color;
  for I := 0 to MAXINTERVALS do
    with FindComponent('Box' + IntToStr(I)) as TShape do
      Brush.Color := TmpColor[MAXINTERVALS-I];
  Modified := True;
end;


procedure TLegendForm.BtnOKClick(Sender: TObject);
//------------------------------------------------
// OnClick handler for OK button.
// Validates numeric entries in text boxes.
//------------------------------------------------
begin
  if not ValidateInput then ModalResult := mrNone
  else if not Modified then ModalResult := mrCancel
  else ModalResult := mrOK;
end;


procedure TLegendForm.BtnCancelClick(Sender: TObject);
//---------------------------------------------------
// OnClick handler for Cancel button.
//---------------------------------------------------
begin
  ModalResult := mrCancel;
end;


procedure TLegendForm.CheckFramedClick(Sender: TObject);
//------------------------------------------------------
// OnClick handler for Framed checkbox.
//------------------------------------------------------
begin
  Modified := True;
end;


procedure TLegendForm.BoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//-----------------------------------------------------------------
// OnMouseDown handler for all of the color boxes (Shape controls).
// Highlights the box and opens up a color selection dialog form.
//-----------------------------------------------------------------
var
  aBox : TShape;
begin

// Highlight box clicked on
  aBox := TShape(Sender);
  Hiliter.Left := aBox.Left - 2;
  Hiliter.Top := aBox.Top - 2;
  Hiliter.Visible := True;

// Execute standard color dialog to get new color for box
  ColorDialog1.Color := TShape(Sender).Brush.Color;
  try
    if ColorDialog1.Execute then
    begin
      TShape(Sender).Brush.Color := ColorDialog1.Color;
      Modified := True;
    end;
  finally
    Hiliter.Visible := False;
  end;
end;


procedure TLegendForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 217);
end;

end.
