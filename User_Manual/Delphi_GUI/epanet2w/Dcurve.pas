unit Dcurve;

{-------------------------------------------------------------------}
{                    Unit:    Dcurve.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box for editing an X-Y curve.           }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart,
  VCLTee.Series, VclTee.TeeGDIPlus, System.UITypes, Grids, Math,
  Uglobals, Uutils;

const
  VOLCURVE   = 0;
  HEADCURVE  = 1;
  EFFCURVE   = 2;
  HLOSSCURVE = 3;
  MAXPOINTS = 50;
  TINY = 0.000001;  //1.e-6;
  Xlabel: array[0..3] of PChar =
    (' Height', ' Flow', ' Flow', ' Flow');
  Ylabel: array[0..3] of PChar =
    (' Volume', ' Head', ' Efficiency', ' Headloss');
  MSG_OUT_OF_ORDER = ' values are not in ascending order.';
  MSG_BAD_CURVE = 'Illegal pump curve. Continue editing?';
  FMT_EQN = ' Head = %f%-.4g(Flow)^%f';
  TXT_PERCENT = ' (%)';
  TXT_CUBIC = ' (cubic ';
  TXT_PUMP = 'PUMP';
  TXT_BAD_CURVE = ' Illegal pump curve.';
  TXT_OPEN_CURVE_TITLE = 'Open a Curve';
  TXT_SAVE_CURVE_TITLE = 'Save Curve As';
  TXT_CURVE_FILTER = 'Curve files (*.CRV)|*.CRV|All files|*.*';
  TXT_CURVE_HEADER = 'EPANET Curve Data';

var
  X : array[1..MAXPOINTS] of Double;
  Y : array[1..MAXPOINTS] of Double;
  Xunits: array[0..3] of String;
  Yunits: array[0..3] of String;

type
  TCurveForm = class(TForm)
    CurveID: TEdit;
    Comment: TEdit;
    CurveGrid: TStringGrid;
    CurveType: TComboBox;
    CurveEqn: TPanel;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EqnLabel: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TPointSeries;
    BtnLoad: TButton;
    BtnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CurveTypeClick(Sender: TObject);
    procedure CurveIDChange(Sender: TObject);
    procedure CurveGridClick(Sender: TObject);
    procedure CurveGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure CurveGridKeyPress(Sender: TObject; var Key: Char);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure CurveIDKeyPress(Sender: TObject; var Key: Char);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private
    { Private declarations }
    //BadPumpCurve: Boolean;                                                    //(2.00.12 - LR)
    Modified: Boolean;
    function  CanClose: Boolean;
    function  GetData: Integer;
    function  FitPumpCurve(var N: Integer): Boolean;
    procedure PlotData;
  public
    { Public declarations }
    procedure LoadData(const Index: Integer);
    procedure UnloadData(const Index: Integer);
  end;

//var
//  CurveForm: TCurveForm;

implementation

{$R *.DFM}

uses Fmain, Uinput;

procedure TCurveForm.FormCreate(Sender: TObject);
//-----------------------------------------------
// Form's OnCreate handler.
//-----------------------------------------------
var
  N : Integer;
  LengthUnits: String;
begin
// Set font size and style
  Uglobals.SetFont(self);

// Set CurveGrid column widths to fill clientwidth.
  with CurveGrid do
  begin
    DefaultColWidth := (ClientWidth-1) div 2;
  end;

// Add curve types to CurveType combobox
  for N := 0 to High(CurveLabel) do
    CurveType.Items.Add(CurveLabel[N]);
  LengthUnits := BaseNodeUnits[ELEVATION,UnitSystem];
  Xunits[VOLCURVE]   := ' (' + LengthUnits + ')';
  Xunits[HEADCURVE]  := ' (' + FlowUnits + ')';
  Xunits[EFFCURVE]   := Xunits[HEADCURVE];
  Xunits[HLOSSCURVE] := Xunits[HEADCURVE];
  Yunits[VOLCURVE]   := TXT_CUBIC + LengthUnits + ')';
  Yunits[HEADCURVE]  := ' (' + LengthUnits + ')';
  Yunits[EFFCURVE]   := TXT_PERCENT;
  Yunits[HLOSSCURVE] := ' (' + LengthUnits + ')';
  CurveGrid.RowCount := MAXPOINTS + 1;
  CurveID.MaxLength := MAXID;  //Max. chars. in a ID
  ActiveControl := CurveID;
end;

function TCurveForm.CanClose: Boolean;
//------------------------------------
// Form's OnClose handler.
// Validates data before closing form.
//------------------------------------
var
  I, N : Integer;
  Xprev: Double;
begin
// Check for duplicate curve ID
  Result := False;
  if Uinput.DupID(CurveID.Text) = True then
  begin
    ActiveControl := CurveID;
    Exit;
  end;

// Check if X-values are in ascending order
  N := GetData;
  if N > 1 then
  begin
    Xprev := X[1];
    for I := 2 to N do
    begin
      if X[I] <= Xprev then
      begin
        Uutils.MsgDlg(CurveGrid.Cells[0,0] + MSG_OUT_OF_ORDER, mtError, [mbOK]);
        exit;
      end;
      Xprev := X[I];
    end;
  end;

// Check for legal pump curve
  if CompareText(CurveType.Items[CurveType.ItemIndex],TXT_PUMP) = 0 then
  begin
    if (N = 1) or (N = 3) then
    begin
      if FitPumpCurve(N) = False then
      begin
        if (Uutils.MsgDlg(MSG_BAD_CURVE, mtWarning, [mbYes,mbNo])
          = mrYes) then Exit;
      end;
    end;
  end;
  Result := True;
end;

procedure TCurveForm.CurveTypeClick(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for CurveType combobox.
// Changes column headings on CurveGrid & re-plots curve.
//-------------------------------------------------------
begin
  CurveGrid.Cells[0,0] := Xlabel[CurveType.ItemIndex];
  CurveGrid.Cells[1,0] := Ylabel[CurveType.ItemIndex];
  PlotData;
end;

procedure TCurveForm.LoadData(const Index: Integer);
//--------------------------------------------------
// Loads curve data from database into form.
//--------------------------------------------------
var
  I: Integer;
  curve: TCurve;
begin
  with Network.Lists[CURVES] do
  begin
    CurveID.Text := Strings[Index];
    curve := TCurve(Objects[Index]);
  end;
  Comment.Text := curve.Comment;
  with CurveType do
  begin
    I := Items.IndexOf(curve.Ctype);
    if I >= 0 then ItemIndex := I
    else ItemIndex := 0;
    CurveGrid.Cells[0,0] := Xlabel[ItemIndex];
    CurveGrid.Cells[1,0] := Ylabel[ItemIndex];
  end;
  with curve.Xdata do
  begin
    for I := 0 to Count-1 do
      CurveGrid.Cells[0,I+1] := Strings[I];
  end;
  with curve.Ydata do
  begin
    for I := 0 to Count-1 do
      CurveGrid.Cells[1,I+1] := Strings[I];
  end;
  PlotData;
  Modified := False;
end;

procedure TCurveForm.UnloadData(const Index: Integer);
//----------------------------------------------------
// Unloads data from form to database.
//----------------------------------------------------
var
  i: Integer;
  sx,sy: String;
  curve: TCurve;
begin
  if not Modified then Exit;
  with Network.Lists[CURVES] do
  begin
    Strings[Index] := CurveID.Text;
    curve := TCurve(Objects[Index]);
  end;
  curve.Comment := Comment.Text;
  with CurveType do
    curve.Ctype := Items.Strings[ItemIndex];
  curve.Xdata.Clear;
  curve.Ydata.Clear;
  with CurveGrid do
  begin
    for i := 1 to RowCount-1 do
    begin
      sx := Trim(Cells[0,i]);
      sy := Trim(Cells[1,i]);
      if (Length(sx) > 0) and (Length(sy) > 0) then
      begin
        curve.Xdata.Add(sx);
        curve.Ydata.Add(sy);
      end;
    end;
  end;
end;

function TCurveForm.GetData: Integer;
//--------------------------------------------------------
// Retrieves X-Y string values from CurveGrid for plotting
//--------------------------------------------------------
var
  I, N  : Integer;
  Vx, Vy: Single;
begin
  N := 0;
  with CurveGrid do for I := 1 to RowCount-1 do
  begin
    if Uutils.GetSingle(Cells[0,I],Vx) then
    begin
      if Uutils.GetSingle(Cells[1,I],Vy) then
      begin
        Inc(N);
        X[N] := Vx;
        Y[N] := Vy;
      end;
    end;
  end;
  Result := N;
end;

function TCurveForm.FitPumpCurve(var N: Integer): Boolean;
//------------------------------------------------------
// Fits 1- or 3-point head curve data to power function.
//------------------------------------------------------
var
  h0, h1, h2 : Double;
  h4, h5     : Double;
  q0, q1, q2 : Double;
  a, a1, b, c: Double;
  I, Iter    : Integer;
begin
  if N = 1 then
  begin
    q0 := 0.0;
    q1 := X[1];
    h1 := Y[1];
    h0 := 1.33334*h1;
    q2 := 2.0*q1;
    h2 := 0.0;
  end
  else
  begin
    q0 := X[1];
    h0 := Y[1];
    q1 := X[2];
    h1 := Y[2];
    q2 := X[3];
    h2 := Y[3];
  end;
  a := h0;
  b := 0.0;
  c := 1.0;
  if (h0 < TINY)
  or (h0 - h1 < TINY)
  or (h1 - h2 < TINY)
  or (q1 - q0 < TINY)
  or (q2 - q1 < TINY)
  then Result := False
  else
  begin
    a := h0;
    Result := False;
    for Iter := 1 to 5 do
    begin
      h4 := a - h1;
      h5 := a - h2;
      c := ln(h5/h4)/ln(q2/q1);
{************************************************
 NOTE: If c < 1.0 then pump curve is convex which
       might cause convergence problems. This was
       permitted in Version 1.x so it is kept the
       same here. We might want to enforce c >= 1
       in the future.
*************************************************}
      if (c <= 0.0) or (c > 20.0) then break;
      b := -h4/Power(q1,c);
      if b > 0.0 then break;
      a1 := h0 - b*Power(q0,c);
      if abs(a1 - a) < 0.01 then
      begin
        Result := True;
        break;
      end;
      a := a1;
    end;
  end;
  if Result = True then
  begin
    N := 25;
    with CurveGrid do
      if N > RowCount then N := RowCount;
    h4 := -a/b;
    h5 := 1.0/c;
    q1 := Power(h4,h5);
    q1 := q1/N;
    X[1] := 0.0;
    Y[1] := a;
    for I := 2 to N do
    begin
      X[I] := (I-1)*q1;
      Y[I] := a + b*Power(X[I],c);
    end;
    CurveEqn.Caption := Format(FMT_EQN,[a,b,c]);
  end
  else CurveEqn.Caption := TXT_BAD_CURVE;
  EqnLabel.Enabled := True;
end;

////  This procedure was completely re-written. ////                            //(2.00.12 - LR)
procedure TCurveForm.PlotData;
//----------------------------
// Plots curve data on Chart1
//----------------------------
var
  I, N: Integer;
  CurveDrawn: Boolean;
begin
// Clear the chart's line and point series
  CurveEqn.Caption := '';
  EqnLabel.Enabled := False;
  Series1.Clear;
  Series2.Clear;
  CurveDrawn := False;

// Label chart's axes
  I := CurveType.ItemIndex;
  Chart1.BottomAxis.Title.Caption := Xlabel[I] + Xunits[I];
  Chart1.LeftAxis.Title.Caption := Ylabel[I] + Yunits[I];

// Retrieve numerical data from CurveGrid
  N := GetData;
  if N < 1 then exit;

// Display pump curve if applicable
  if CompareText(CurveType.Items[CurveType.ItemIndex],TXT_PUMP) = 0 then
  begin
  // Fit power function to 1- or 3-point pump head curves
    if (N = 1) or (N = 3) then
    begin

    // Valid pump curve - draw function in Series1
      if FitPumpCurve(N) then
      begin
        with Series1 do
        begin
          Pointer.Visible := False;
          Active := False;
          for I := 1 to N do AddXY(X[I], Y[I], '', clTeeColor);
          Active := True;
        end;

      // Draw curve points in Series2
        N := GetData;
        if N > 1 then with Series2 do
        begin
          Active := False;
          for I := 1 to N do AddXY(X[I], Y[I], '', clTeeColor);
          Active := True;
        end;
        CurveDrawn := True;
      end

    // Invalid pump curve - retrieve curve points again
      else N := GetData;
    end;
  end;

// Draw curve data points in Series1
  if not CurveDrawn and (N > 1) then with Series1 do
  begin
    Active := False;
    Pointer.Visible := True;
    for I := 1 to N do AddXY(X[I], Y[I], '', clTeeColor);
    Active := True;
  end;
end;

procedure TCurveForm.CurveGridClick(Sender: TObject);
//---------------------------------------------------
// OnClick handler for CurveGrid control.
// Re-plots the curve & puts grid in editing mode.
//---------------------------------------------------
begin
  PlotData;
  PostMessage(CurveGrid.Handle, WM_KeyDown, VK_F2, 0);
end;

procedure TCurveForm.CurveGridKeyPress(Sender: TObject; var Key: Char);
//-------------------------------------------
// OnKeyPress handler for CurveGrid control.
// Re-plots the curve if Enter key pressed.
//-------------------------------------------
begin
  if Key = #13 then PlotData;
end;

procedure TCurveForm.CurveGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
//---------------------------------------------
// OnSetEditText handler for CurveGrid control.
// Sets Modified flag to True.
//---------------------------------------------
begin
  Modified := True;
end;

procedure TCurveForm.CurveIDChange(Sender: TObject);
//-------------------------------------------------
// OnChange handler for CurveID edit control.
// Sets Modified flag to True.
//-------------------------------------------------
begin
  Modified := True;
end;

procedure TCurveForm.CurveIDKeyPress(Sender: TObject; var Key: Char);
//-----------------------------------------------
// OnKeyPress handler for PatternID text box.
// Prevents user from entering a space character.
//------------------------------------------------
begin
  if (Key = ' ') then Key := #0;
end;

procedure TCurveForm.BtnOKClick(Sender: TObject);
//---------------------------------
// OnClick handler for OK button.
// Validates and accepts the form.
//---------------------------------
begin
  if CanClose then
  begin
    Hide;
    ModalResult := mrOK;
  end;
end;

procedure TCurveForm.BtnLoadClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for Load button.
// Loads in curve data from a text file.
//-------------------------------------------------
var
  F : Textfile;
  S : String;
  I : Integer;
  P : Integer;
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := TXT_OPEN_CURVE_TITLE;
    Filter := TXT_CURVE_FILTER;
    Filename := '*.crv';
    Options := Options + [ofHideReadOnly];
    if Execute then
    begin
      AssignFile(F,Filename);
      {$I-}
      Reset(F);
      {$I+}
      if (IOResult = 0) then
      try
        Readln(F, S);
        Readln(F, S);
        with CurveType do
        begin
          for I := 0 to Items.Count-1 do
          begin
            if CompareText(S, Items[I]) = 0 then
            begin
              ItemIndex := I;
              break;
            end;
          end;
        end;
        Readln(F, S);
        Comment.Text := S;
        with CurveGrid do
        begin
          for I := 1 to RowCount-1 do
          begin
            Cells[0,I] := '';
            Cells[1,I] := '';
          end;
          I := 1;
          while not Eof(F) and (I < RowCount) do
          begin
            Readln(F, S);
            P := Pos(' ', Trim(S));
            if P > 0 then
            begin
              Cells[0,I] := Copy(S,1,P-1);
              Cells[1,I] := Trim(Copy(S,P+1,Length(S)));
              Inc(I);
            end;
          end;
        end;
      finally
      end;
      CloseFile(F);
      Modified := True;
      PlotData;
      Comment.SetFocus;
    end;
  end;
end;

procedure TCurveForm.BtnSaveClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for Save button.
// Saves curve data to a text file.
//-------------------------------------------------
var
  F : Textfile;
  Sx: String;
  Sy: String;
  I : Integer;
begin
  with MainForm.SaveDialog do
  begin
    Title := TXT_SAVE_CURVE_TITLE;
    Filter := TXT_CURVE_FILTER;
    Filename := '*.crv';
    if Execute then
    begin
      AssignFile(F,Filename);
      {$I-}
      Rewrite(F);
      {$I+}
      if (IOResult = 0) then
      try
        Writeln(F, TXT_CURVE_HEADER);
        with CurveType do Writeln(F, Items[ItemIndex]);
        Writeln(F, Comment.Text);
        with CurveGrid do for I := 1 to RowCount-1 do
        begin
          Sx := Trim(Cells[0,I]);
          Sy := Trim(Cells[1,I]);
          if (Length(Sx) > 0) and (Length(Sy) > 0) then
            Writeln(F, Sx, '  ', Sy);
        end;
      finally
      end;
      CloseFile(F);
    end;
  end;
end;

procedure TCurveForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 233);
end;

end.
