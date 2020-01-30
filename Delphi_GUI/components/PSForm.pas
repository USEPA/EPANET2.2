{
  Unit:    PSForm.pas
  Project: TPageSetupDialog Component
  Author:  L. Rossman
  Version: 1.0
  Date:    1/30/00 

  This is the form unit used for the TPageSetupDialog 
  dialog box component (PgSetup.pas).
  
}

unit PSForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Printers, StdCtrls, ExtCtrls, ComCtrls, Math, System.UITypes, PgSetup;

const
  MM = 'mm';
  INCHES = '"';
  MMperIN = 25.4;
  MSG_BADMARGINS = 'Page margins are too large for paper size.';

type
  TPageSetupForm = class(TForm)
    PrinterSetupDialog1: TPrinterSetupDialog;
    BtnOK: TButton;
    BtnCancel: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    SizeBox: TGroupBox;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    OrientBox: TRadioGroup;
    MarginsBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditLeft: TEdit;
    EditTop: TEdit;
    EditRight: TEdit;
    EditBot: TEdit;
    PageShape: TShape;
    PrintAreaShape: TShape;
    PageShadowShape: TShape;
    PrinterSetup: TButton;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    EditHeader: TEdit;
    GroupBox2: TGroupBox;
    EditFooter: TEdit;
    Label7: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RBHeaderLeft: TRadioButton;
    RBHeaderCenter: TRadioButton;
    RBHeaderRight: TRadioButton;
    CheckHeaderEnabled: TCheckBox;
    RBFooterLeft: TRadioButton;
    RBFooterCenter: TRadioButton;
    RBFooterRight: TRadioButton;
    CheckFooterEnabled: TCheckBox;
    CBPageNumbers: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure PrinterSetupClick(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditChange(Sender: TObject);
    procedure EditBotExit(Sender: TObject);
    procedure EditLeftExit(Sender: TObject);
    procedure EditRightExit(Sender: TObject);
    procedure EditTopExit(Sender: TObject);
    procedure OrientBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    { Private declarations }
    PageWidth,
    PageHeight: Single;
    ScaleX,
    ScaleY : Single;
    MinMarginLeft,
    MinMarginRight,
    MinMarginTop,
    MinMarginBot: Single;
    PageCenterX,
    PageCenterY,
    PageSizeMax: Integer;
    Units : String;
    UCF   : Single;
    DecimalChar: Char;
    procedure CheckMargins;
    procedure DisplayMargins;
    procedure GetPrinterCaps;
    procedure ResizeMargins;
    procedure ResizePage;
  public
    { Public declarations }
    Measurements: TMeasurements;
    MarginLeft,
    MarginRight,
    MarginTop,
    MarginBot: Single;
  end;

var
  PageSetupForm: TPageSetupForm;

implementation

{$R *.DFM}

procedure TPageSetupForm.FormCreate(Sender: TObject);
begin
//Set font size
//  Font.Size := 8;
// Get decimal character from Delphi's DecimalSeparator global variable
  DecimalChar := FormatSettings.DecimalSeparator;
//Save location of page shape
  PageCenterX := MarginsBox.Left + MarginsBox.Width div 2;
  PageCenterY := PageShape.Top + PageShape.Height div 2;
  PageSizeMax := PageShape.Height;
//Initialize page margins
  MarginLeft := -1;
  MarginRight := -1;
  MarginTop := -1;
  MarginBot := -1;
end;

procedure TPageSetupForm.FormShow(Sender: TObject);
begin
  if Measurements = pmMillimeters then
  begin
    MarginsBox.Caption := 'Margins (mm)';
    Units := MM;
    UCF := MMperIN;
  end
  else
  begin
    MarginsBox.Caption := 'Margins (inches)';
    Units := INCHES;
    UCF := 1;
  end;
  GetPrinterCaps;
  EditLeft.SetFocus;
end;

procedure TPageSetupForm.GetPrinterCaps;
var
  physWidth,
  physHeight : Integer;
  physOffsetX,
  physOffsetY: Integer;
  xPixPerInch,
  yPixPerInch: Integer;
  PrintWidth,
  PrintHeight: Integer;
begin
  OrientBox.ItemIndex := Ord(Printer.Orientation);
  xPixPerInch := GetDeviceCaps(Printer.Handle,LOGPIXELSX);
  yPixPerInch := GetDeviceCaps(Printer.Handle,LOGPIXELSY);
  physWidth := GetDeviceCaps(Printer.Handle,PHYSICALWIDTH);
  physHeight := GetDeviceCaps(Printer.Handle,PHYSICALHEIGHT);
  physOffsetX := GetDeviceCaps(Printer.Handle,PHYSICALOFFSETX);
  physOffsetY := GetDeviceCaps(Printer.Handle,PHYSICALOFFSETY);
  PrintWidth := GetDeviceCaps(Printer.Handle,HORZRES);
  PrintHeight := GetDeviceCaps(Printer.Handle,VERTRES);
  PageWidth := physWidth / xPixPerInch * UCF;
  PageHeight := physHeight / yPixPerInch * UCF;
  WidthLabel.Caption := Format('Width: %.1f ' + Units,[PageWidth]);
  HeightLabel.Caption := Format('Height: %.1f ' + Units,[PageHeight]);
  MinMarginLeft := (physOffsetX / xPixPerInch) * UCF;
  MinMarginTop := (physOffsetY / yPixPerInch) * UCF;
  MinMarginRight := PageWidth - (PrintWidth / xPixPerInch)*UCF - MinMarginLeft;
  MinMarginBot := PageHeight - (PrintHeight / yPixPerInch)*UCF - MinMarginTop;
  CheckMargins;
  DisplayMargins;
  ResizePage;
  ResizeMargins;
end;

procedure TPageSetupForm.CheckMargins;
begin
  if MarginLeft < MinMarginLeft then
  begin
    MarginLeft := MinMarginLeft;
    EditLeft.Text := Format('%.2f',[MarginLeft]);
  end;
  if MarginRight < MinMarginRight then
  begin
    MarginRight := MinMarginRight;
    EditRight.Text := Format('%.2f',[MarginRight]);
  end;
  if MarginTop < MinMarginTop then
  begin
    MarginTop := MinMarginTop;
    EditTop.Text := Format('%.2f',[MarginTop]);
  end;
  if MarginBot < MinMarginBot then
  begin
    MarginBot := MinMarginBot;
    EditBot.Text := Format('%.2f',[MarginBot]);
  end;
end;

procedure TPageSetupForm.DisplayMargins;
begin
  EditLeft.Text := Format('%.2f',[MarginLeft]);
  EditRight.Text := Format('%.2f',[MarginRight]);
  EditTop.Text := Format('%.2f',[MarginTop]);
  EditBot.Text := Format('%.2f',[MarginBot]);
end;

procedure TPageSetupForm.ResizePage;
var
  shapewidth,
  shapeheight : Integer;
  aLeft,
  aTop        : Integer;
begin
// Determine pixel width & height of page shape
  if PageWidth > PageHeight then
  begin
    shapewidth := PageSizeMax;
    shapeheight := Round(PageHeight * PageSizeMax / PageWidth);
  end
  else
  begin
    shapeheight := PageSizeMax;
    shapewidth := Round(PageWidth * PageSizeMax / PageHeight);
  end;
// Compute scaling factors
  ScaleX := shapewidth / PageWidth;
  ScaleY := shapeheight / PageHeight;
// Set bounds of page shape and shadow
  aLeft := PageCenterX - shapewidth div 2;
  aTop := PageCenterY - shapeheight div 2;
  PageShadowShape.SetBounds(aLeft+4,aTop+4,shapewidth,shapeheight);
  PageShape.SetBounds(aLeft,aTop,shapewidth,shapeheight);
end;

procedure TPageSetupForm.ResizeMargins;
var
  aLeft,
  aRight,
  aTop,
  aBot : Integer;
begin
  aLeft := PageShape.Left + Round(MarginLeft * ScaleX);
  aTop  := PageShape.Top + Round(MarginTop * ScaleY);
  aRight := PageShape.Left + PageShape.Width - Round(MarginRight * ScaleX);
  aBot := PageShape.Top + PageShape.Height - Round(MarginBot*ScaleY);
  if (aLeft < PageShape.Left)
  or (aRight > PageShape.Left + PageShape.Width)
  or (aLeft >= aRight)
  or (aTop < PageShape.Top)
  or (aBot > PageShape.Top + PageShape.Height)
  or (aTop >= aBot)
  then PrintAreaShape.Visible := False
  else
  begin
    PrintAreaShape.SetBounds(aLeft,aTop,(aRight-aLeft),(aBot-aTop));
    PrintAreaShape.Visible := True;
  end;
end;

procedure TPageSetupForm.EditKeyPress(Sender: TObject; var Key: Char);
begin
  with Sender as TEdit do
  begin
    if not CharInSet(Key, ['0'..'9',DecimalChar,#8]) then Key := #0;
    if (Key = DecimalChar) and (Pos(Key,Text) > 0) then Key := #0;
  end;
end;

procedure TPageSetupForm.EditChange(Sender: TObject);
var
  x: Single;
begin
  with Sender as TEdit do
  try
    x := StrToFloat(Text);
  except
    x := 0;
  end;
  if Sender = EditLeft then
    MarginLeft := MaxValue([x,MinMarginLeft])
  else if Sender = EditRight then
    MarginRight := MaxValue([x,MinMarginRight])
  else if Sender = EditTop then
    MarginTop := MaxValue([x,MinMarginTop])
  else if Sender = EditBot then
    MarginBot := MaxValue([x,MinMarginBot]);
  ResizeMargins;
end;

procedure TPageSetupForm.EditBotExit(Sender: TObject);
begin
  if MarginBot <= MinMarginBot then
    EditBot.Text := Format('%.2f',[MarginBot]);
end;

procedure TPageSetupForm.EditLeftExit(Sender: TObject);
begin
  if MarginLeft <= MinMarginLeft then
    EditLeft.Text := Format('%.2f',[MarginLeft]);
end;

procedure TPageSetupForm.EditRightExit(Sender: TObject);
begin
  if MarginRight <= MinMarginRight then
    EditRight.Text := Format('%.2f',[MarginRight]);
end;

procedure TPageSetupForm.EditTopExit(Sender: TObject);
begin
  if MarginTop <= MinMarginTop then
    EditTop.Text := Format('%.2f',[MarginTop]);
end;

procedure TPageSetupForm.OrientBoxClick(Sender: TObject);
var
  OldOrient: TPrinterOrientation;
begin
  OldOrient := Printer.Orientation;
  if OrientBox.ItemIndex = 0 then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandscape;
  if OldOrient <> Printer.Orientation then
  begin
    GetPrinterCaps;
  end;
end;

procedure TPageSetupForm.PrinterSetupClick(Sender: TObject);
begin
   if PrinterSetupDialog1.Execute then GetPrinterCaps;
end;

procedure TPageSetupForm.BtnOKClick(Sender: TObject);
begin
  if PrintAreaShape.Visible = False then
    MessageDlg(MSG_BADMARGINS, mtError, [mbOK], 0)
  else
    ModalResult := mrOK;
end;

end.
