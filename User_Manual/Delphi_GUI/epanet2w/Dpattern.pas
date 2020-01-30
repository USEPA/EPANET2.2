unit Dpattern;

{-------------------------------------------------------------------}
{                    Unit:    Dpattern.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit containing a Time Pattern Editor dialog box.          }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Grids, ExtCtrls, System.UITypes,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
  VclTee.TeeGDIPlus, Uglobals, Uutils;

const
  MAXPERIODS = 24;
  FMT_LEFT_AXIS = 'Avg. = %.2f';
  TXT_REPLACE_ALL = 'Replace all references to Pattern ';
  TXT_REPLACE_WITH = ' with ';
  TXT_TIME_PERIOD = 'Time Period';
  TXT_MULTIPLIER = 'Multiplier';
  TXT_AXIS_TITLE =  'Time (Time Period = ';
  TXT_HRS = ' hrs)';
  TXT_OPEN_PATTERN_TITLE = 'Open a Pattern';
  TXT_SAVE_PATTERN_TITLE = 'Save Pattern As';
  TXT_PATTERN_FILTER = 'Pattern files (*.PAT)|*.PAT|All files|*.*';
  TXT_PATTERN_HEADER = 'EPANET Pattern Data';


type
  TPatternForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    PatternID: TEdit;
    Comment: TEdit;
    Grid1: TStringGrid;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Chart1: TChart;
    Series2: TFastLineSeries;
    Series1: TAreaSeries;
    BtnLoad: TButton;
    BtnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure Grid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure Grid1KeyPress(Sender: TObject; var Key: Char);
    procedure PatternIDChange(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure PatternIDKeyPress(Sender: TObject; var Key: Char);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private
    { Private declarations }
    Modified: Boolean;
    OldID:    String;
    procedure PlotData;
    procedure UpdateIDs(const newID: String);
  public
    { Public declarations }
    procedure LoadData(const Index: Integer);
    procedure UnloadData(const Index: Integer);
  end;

implementation

{$R *.DFM}

uses Fmain, Uinput;

procedure TPatternForm.FormCreate(Sender: TObject);
//-------------------------------------------------
// OnCreate handler for form.
//-------------------------------------------------
begin
// Set font size and style
  Uglobals.SetFont(self);

// Set properties of multiplier grid
  with Grid1 do
  begin
    ColCount := MAXPERIODS + 1;
    RowCount := 2;
    ColWidths[0] := 2*DefaultColWidth;
    Cells[0,0] := TXT_TIME_PERIOD;
    Cells[0,1] := TXT_MULTIPLIER;
  end;

// Make PatternID edit box the active control
  PatternID.MaxLength := MAXID;  //Max. chars. in a ID
  ActiveControl := PatternID;
end;

procedure TPatternForm.LoadData(const Index: Integer);
//-----------------------------------------------
// Loads data for pattern being edited into form.
//-----------------------------------------------
var
  i: Integer;
  imax: Integer;
  pattern: TPattern;
begin
  with Network.Lists[PATTERNS] do
  begin
    PatternID.Text := Strings[Index];
    OldID := PatternID.Text;
    pattern := TPattern(Objects[Index]);
  end;
  Comment.Text := pattern.Comment;
  with pattern.Multipliers, Grid1 do
  begin
    imax := Count;
    if imax > MAXPERIODS then ColCount := imax + 1;
    for i := 1 to ColCount-1 do Cells[i,0] := IntToStr(i);
    for i := 1 to imax do Cells[i,1] := Strings[i-1];
  end;
  PlotData;
  Modified := False;
end;

procedure TPatternForm.UnloadData(const Index: Integer);
//------------------------------------------------------
// Unloads new data from form into pattern being edited.
//------------------------------------------------------
var
  i: Integer;
  s: String;
  pattern: TPattern;
begin
  if not Modified then Exit;
  with Network.Lists[PATTERNS] do
  begin
    Strings[Index] := PatternID.Text;
    pattern := TPattern(Objects[Index]);
  end;
  pattern.Comment := Comment.Text;
  pattern.Multipliers.Clear;
  with Grid1 do
  begin
    for i := 1 to ColCount-1 do
    begin
      s := Trim(Cells[i,1]);
      if Length(s) > 0 then pattern.Multipliers.Add(s);
    end;
  end;
end;

procedure TPatternForm.PlotData;
//--------------------------------------
// Plots multiplier value v. time period
//--------------------------------------
var
  I     : Integer;
  Vy    : Single;
  Avg   : Single;
  T, DT : Single;
  PatTstep: String;
begin
// Include length of time period in X-axis title
  PatTstep := Network.Options.Data[PAT_TSTEP_INDEX];
  Chart1.BottomAxis.Title.Caption := TXT_AXIS_TITLE + PatTstep + TXT_HRS;
  Chart1.LeftAxis.Title.Caption := TXT_MULTIPLIER;
  DT := Uutils.StrHoursToFloat(PatTstep);
  if DT < 0 then DT := 1;

// Add multiplier data to Chart1's Series1
  Series1.Clear;
  Series2.Clear;
  with Series1 do
  begin

  // Initialize average & time values
    Active := False;
    Avg := 0;
    T := 0;

  // For each non-empty cell in Grid1...
    with Grid1 do for I := 1 to ColCount-1 do
    begin

    // Add multiplier & update time & average value
      if Uutils.GetSingle(Cells[I,1],Vy) then
      begin
        AddXY(T, Vy, '', clTeeColor);
        T := T + DT;
        Avg := Avg + Vy;
      end;
    end;

  // Repeat final point & adjust scale of left axis
    if Count > 0 then
    begin
      AddXY(T, Vy, '', clTeeColor);
      with Chart1.LeftAxis do
      begin
        if MinYvalue > 0 then Minimum := 0;
        AutomaticMinimum := False;
        if (MaxYvalue > 10) or (MinYvalue < 0) then
          AutomaticMinimum := True;
      end;
      Active := True;
    end;
  end;

// Display line for average multiplier value
  if Series1.Count >= 2 then with Series2 do
  begin
    Avg := Avg/(Series1.Count-1);
    AddXY(0, Avg, '', clTeeColor);
    AddXY(T, Avg, '', clTeeColor);
    Chart1.LeftAxis.Title.Caption := Format(FMT_LEFT_AXIS,[Avg]);
  end;

end;

procedure TPatternForm.Grid1Click(Sender: TObject);
//------------------------------------------------
// OnClick handler for multiplier grid
//------------------------------------------------
begin
// Re-plot the data
  PlotData;

// Put grid in editing mode
  PostMessage(Grid1.Handle, WM_KeyDown, VK_F2, 0);
end;

procedure TPatternForm.Grid1KeyPress(Sender: TObject; var Key: Char);
//----------------------------------------
// OnKeyPress handler for multiplier grid
//----------------------------------------
begin
//  If Enter key pressed then ...
  if (Key = #13) then with Sender as TStringGrid do
  begin

  // Add new column if currently in last column
    if (Col = ColCount-1) then
    begin
      ColCount := ColCount + 1;
      Cells[ColCount-1,0] := IntToStr(ColCount-1);
      Refresh;
      Col := Col + 1;
    end;

  // Re-plot the data
    PlotData;
  end;
end;

procedure TPatternForm.Grid1SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
//----------------------------------------------------
// OnSetEditText handler for pattern multipliers grid.
// Sets Modified flag to true.
//----------------------------------------------------
begin
  Modified := True;
end;

procedure TPatternForm.PatternIDChange(Sender: TObject);
begin
  Modified := True;
end;

procedure TPatternForm.BtnOKClick(Sender: TObject);
//------------------------------------------------
// OnClick handler for OK button.
// Checks that pattern ID is unique before
// closing the form.
//------------------------------------------------
var
  msg: String;
begin
  if Uinput.DupID(PatternID.Text) = True then
  begin
    PatternID.Text := OldID;
    ActiveControl := PatternID;
    ModalResult := mrNone;
  end
  else
  begin
    if PatternID.Text <> OldID then
    begin
      msg := TXT_REPLACE_ALL + OldID +
             TXT_REPLACE_WITH + PatternID.Text + '?';
      if Uutils.MsgDlg(msg, mtConfirmation, [mbYes, mbNo]) = mrYes then
        UpdateIDs(PatternID.Text);
    end;
    ModalResult := mrOK;
  end;
end;


procedure TPatternForm.UpdateIDs(const NewID: String);
//-------------------------------------------------------------
// Changes all references to Pattern OldID in database to NewID
//-------------------------------------------------------------
var
  i, j    : Integer;
  ntoks   : Integer;
  s       : String;
  toklist : TStringlist;

begin
// Examine junction demand categories
  Screen.Cursor := crHourGlass;
  toklist := TStringlist.Create;
  try
    for i := 0 to Network.Lists[JUNCS].Count - 1 do
    with TJunc(Network.Lists[JUNCS].Objects[i]) do
    begin
      for j := 0 to Demands.Count - 1 do
      begin
        Uutils.Tokenize(Demands[j],toklist,ntoks);
        if ntoks >= 2 then
        begin
          if toklist[1] = OldID then
          begin
            s := toklist[0] + #13 + NewID;
            if ntoks > 2 then s := s + #13 + toklist[2];
            Demands[j] := s;
          end;
        end;
      end;
    end;
  finally
    toklist.Free;
  end;

// Examine junction demand patterns & source patterns
  for i := 0 to Network.Lists[JUNCS].Count - 1 do
  with Node(JUNCS,i) do
  begin
    if Data[JUNC_PATTERN_INDEX] = OldID then Data[JUNC_PATTERN_INDEX] := NewID;
    if Data[JUNC_SRCPAT_INDEX] = OldID then  Data[JUNC_SRCPAT_INDEX] := NewID;
  end;

// Examine reservoir head patterns & source patterns
  for i := 0 to Network.Lists[RESERVS].Count - 1 do
  with Node(RESERVS,i) do
  begin
    if Data[RES_PATTERN_INDEX] = OldID then Data[RES_PATTERN_INDEX] := NewID;
    if Data[RES_SRCPAT_INDEX] = OldID then  Data[RES_SRCPAT_INDEX] := NewID;
  end;

// Examine tank source patterns
  for i := 0 to Network.Lists[TANKS].Count - 1 do
  with Node(TANKS,i) do
  begin
    if Data[TANK_SRCPAT_INDEX] = OldID then Data[TANK_SRCPAT_INDEX] := NewID;
  end;

// Examine pump speed patterns & energy price patterns
  for i := 0 to Network.Lists[PUMPS].Count - 1 do
  with Link(PUMPS,i) do
  begin
    if Data[PUMP_PATTERN_INDEX] = OldID then Data[PUMP_PATTERN_INDEX] := NewID;
    if Data[PUMP_PRICEPAT_INDEX] = OldID then Data[PUMP_PRICEPAT_INDEX] := NewID;
  end;

// Examine global demand pattern & energy price pattern
  with Network.Options do
  begin
    if Data[GLOBAL_PAT_INDEX] = OldID then Data[GLOBAL_PAT_INDEX] := NewID;
    if Data[PRICE_PAT_INDEX] = OldID then Data[PRICE_PAT_INDEX] := NewID;
  end;
  Screen.Cursor := crDefault;
end;


procedure TPatternForm.PatternIDKeyPress(Sender: TObject; var Key: Char);
//------------------------------------------------
// OnKeyPress handler for PatternID text box.
// Prevents user from entering a space character
//------------------------------------------------
begin
  if (Key = ' ') then Key := #0;
end;


procedure TPatternForm.BtnLoadClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for Load button.
// Loads in pattern data from a text file.
//-------------------------------------------------
var
  F : Textfile;
  S : String;
  I : Integer;
begin
  with MainForm.OpenTextFileDialog do
  begin
    Title := TXT_OPEN_PATTERN_TITLE;
    Filter := TXT_PATTERN_FILTER;
    Filename := '*.pat';
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
        Comment.Text := S;
        with Grid1 do
        begin
          for I := 1 to ColCount-1 do
          begin
            Cells[I,1] := '';
          end;
          I := 1;
          while not Eof(F) do
          begin
            Readln(F, S);
            if I = ColCount then
            begin
              ColCount := ColCount + 1;
              Cells[I,0] := IntToStr(I);
            end;
            Cells[I,1] := Trim(S);
            Inc(I);
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

procedure TPatternForm.BtnSaveClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for Save button.
// Saves pattern data to a text file.
//-------------------------------------------------
var
  F : Textfile;
  S : String;
  I : Integer;
begin
  with MainForm.SaveDialog do
  begin
    Title := TXT_SAVE_PATTERN_TITLE;
    Filter := TXT_PATTERN_FILTER;
    Filename := '*.pat';
    if Execute then
    begin
      AssignFile(F,Filename);
      {$I-}
      Rewrite(F);
      {$I+}
      if (IOResult = 0) then
      try
        Writeln(F, TXT_PATTERN_HEADER);
        Writeln(F, Comment.Text);
        with Grid1 do for I := 1 to ColCount-1 do
        begin
          S := Trim(Cells[I,1]);
          if (Length(S) > 0) then Writeln(F, S);
        end;
      finally
      end;
      CloseFile(F);
    end;
  end;
end;

procedure TPatternForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 234);
end;

end.
