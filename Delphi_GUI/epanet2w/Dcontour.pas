unit Dcontour;

{-------------------------------------------------------------------}
{                    Unit:    Dcontour.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box that retrieves options for          }
{   displaying a Contour Plot.                                      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, Uglobals, Fcontour, ExtCtrls;

type
  TContourOptionsForm = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    LegendGroup: TGroupBox;
    LgndDisplay: TCheckBox;
    StyleGroup: TGroupBox;
    StyleFilled: TRadioButton;
    StyleLines: TRadioButton;
    NetworkGroup: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LinkSize: TSpinEdit;
    ContourGroup: TGroupBox;
    NumLines: TSpinEdit;
    LineSize: TSpinEdit;
    DefaultBox: TCheckBox;
    LgndModify: TButton;
    ForeColorBox: TColorBox;
    BackColorBox: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure LgndModifyClick(Sender: TObject);
    procedure StyleFilledClick(Sender: TObject);
    procedure StyleLinesClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
    Cform: TContourForm;
  public
    { Public declarations }
    procedure LoadOptions(Sender: TObject);
    procedure UnloadOptions;
  end;

//var
//  ContourOptionsForm: TContourOptionsForm;

implementation

{$R *.DFM}

uses Umap;

procedure TContourOptionsForm.FormCreate(Sender: TObject);
//-------------------------------------------------------
// OnCreate handler for form.
//-------------------------------------------------------

begin

// Set font size and style
  Uglobals.SetFont(self);
end;

procedure TContourOptionsForm.LoadOptions(Sender: TObject);
//--------------------------------------------------------
// Loads existing options of the Sender TContourForm
// which invoked the dialog box. This form is saved
// in variable Cform so it can be used in the UnloadOptions
// procedure to assign updated options back to itself.
//--------------------------------------------------------
begin
  if (Sender is TContourForm) then with Sender as TContourForm do
  begin
    Cform := TContourForm(Sender);
    LgndDisplay.Checked := Cform.LegendPanel.Visible;
    if (Cform.Options.Style = csFilled) then StyleFilled.Checked := True;
    if (Cform.Options.Style = csLines) then StyleLines.Checked := True;
    //BackColorBox.ItemIndex := GetColorIndex(Cform.Options.BackColor);
    //ForeColorBox.ItemIndex := GetColorIndex(Cform.Options.ForeColor);
    BackColorBox.Selected := Cform.Options.BackColor;
    ForeColorBox.Selected := Cform.Options.ForeColor;
    LinkSize.Value := Cform.Options.LinkSize;
    LineSize.Value := Cform.Options.LineSize;
    NumLines.Value := Cform.Options.NumLines;
  end;
end;

procedure TContourOptionsForm.UnLoadOptions;
//-----------------------------------------------------
// Unloads options selected in the dialog back into the
// TContourForm (Cform) which invoked the dialog.
//-----------------------------------------------------
begin
  with Cform do
  begin
    LegendPanel.Visible := LgndDisplay.Checked;
    if (StyleFilled.Checked) then Options.Style := csFilled;
    if (StyleLines.Checked) then Options.Style := csLines;
    Options.BackColor := BackColorBox.Selected;
    Options.ForeColor := ForeColorBox.Selected;
    Options.LinkSize := LinkSize.Value;
    if (StyleLines.Checked) then
    begin
      Options.LineSize := LineSize.Value;
      Options.NumLines := NumLines.Value;
    end;
  end;
  if (DefaultBox.Checked) then
  begin
    if (StyleFilled.Checked) then DefContourOptions.Style := csFilled;
    if (StyleLines.Checked) then DefContourOptions.Style := csLines;
    DefContourOptions.BackColor := BackColorBox.Selected;
    DefContourOptions.ForeColor := ForeColorBox.Selected;
    DefContourOptions.LinkSize := LinkSize.Value;
    if (StyleLines.Checked) then
    begin
      DefContourOptions.LineSize := LineSize.Value;
      DefContourOptions.NumLines := NumLines.Value;
    end;
  end;
end;

procedure TContourOptionsForm.LgndModifyClick(Sender: TObject);
//------------------------------------------------------------
// OnClick handler for the "Modify Legend" button.
// Launches a dialog box, from the Umap unit, that modifies
// the Contour Plot's legend.
//------------------------------------------------------------
begin
  Umap.EditLegend(Cform.Legend,Cform.TimePeriod,Cform.MapColor,
    Cform.LegendFrame.Framed);
end;

procedure TContourOptionsForm.StyleFilledClick(Sender: TObject);
//----------------------------------------------------------------------
// OnClick handler for StyleFilled checkbox.
// Disables the Contour Lines options when filled contours are selected.
//----------------------------------------------------------------------
begin
  ContourGroup.Enabled := False;
end;

procedure TContourOptionsForm.StyleLinesClick(Sender: TObject);
//-------------------------------------------------------------------
// OnClick handler for StyleLines checkbox.
// Enables the Contour Lines options when line contours are selected.
//-------------------------------------------------------------------
begin
  ContourGroup.Enabled := True;
end;

procedure TContourOptionsForm.BtnOKClick(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for "OK" button.
//-------------------------------------------------------
begin
  Hide;
  ModalResult := mrOK;
end;

procedure TContourOptionsForm.BtnCancelClick(Sender: TObject);
//-----------------------------------------------------------
// OnClick handler for "Cancel" button.
//-----------------------------------------------------------
begin
  Hide;
  ModalResult := mrCancel;
end;

procedure TContourOptionsForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 231);
end;

end.
