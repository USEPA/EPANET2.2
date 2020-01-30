{ Component Name Changed to Avoid Conflict with VCL's TPageSetupDialog }
{
   TPageSetupDialogEx Component
   **************************

   A Delphi component that implements a Page Setup Dialog box.
   It can be used to:
   - select a printer
   - set the page orientation
   - set page margins
   - assign header/footer text and alignment
   - select page number position

  The component contains the following published properties:
    BoldFont: Boolean            (True if dialog uses bold font)
    Footer,Header                (Page footer/header structure)
       Alignment: TAlignment     (Footer/header alignment)
       Enabled: Boolean          (True if footer/header is visible)
       Text: String              (Footer/header text)
    HelpContext: THelpContext    (Help context ID number)
    PageMargins: TPageMargins    (Page margins)
       Left, Right,
       Top, Bottom: Single
    Measurements: TMeasurements  (Measurement units for margins)
    PageNumbers: TPageNumbers    (Page number position)
    ShowHFPage: Boolean          (True if Header/Footer tabsheet shown)
    TrueTypeFont: Boolean        (True if Arial font used)
     
   This component also uses the PSForm.pas file which contains the Page
   Setup form.

   Author:  L. Rossman
   Version: 1.0
   Date:    1/30/00
}

unit PgSetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Printers, Math, System.UITypes;

const
  PRINTINGMSG = 'Printer is currently printing.';
  NOPRINTERMSG = 'There are no installed printers.';

type
  TMeasurements = (pmDefault, pmMillimeters, pmInches);
  TPageNumbers  = (pnNone, pnUpperLeft, pnUpperCenter, pnUpperRight,
                   pnLowerLeft, pnLowerCenter, pnLowerRight);

  TPageMargins = class(TPersistent)
  private
    FLeft   : Single;
    FRight  : Single;
    FTop    : Single;
    FBottom : Single;
  protected
  published
    property Left:    Single read FLeft   write FLeft;
    property Right:   Single read FRight  write FRight;
    property Top:     Single read FTop    write FTop;
    property Bottom:  Single read FBottom write FBottom;
  end;

  TPageCaption = class(TPersistent)
  private
    FText     : String;
    FEnabled  : Boolean;
    FAlignment: TAlignment;
  protected
  published
    property Text:      String  read FText    write FText;
    property Enabled:   Boolean read FEnabled write FEnabled default True;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
  end;

  TPageSetupDialogEx = class(TComponent)
  private
    FBoldFont:     Boolean;
    FFooter:       TPageCaption;
    FHeader:       TPageCaption;
    FHelpContext:  THelpContext;
    FPageMargins:  TPageMargins;
    FMeasurements: TMeasurements;
    FPageNumbers:  TPageNumbers;
    FShowHFPage:   Boolean;
    FTrueTypeFont: Boolean;
    procedure      SetPageMargins(Value: TPageMargins);
    function       GetDefMeasurements: TMeasurements;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property BoldFont: Boolean read FBoldFont write FBoldFont;
    property Footer: TPageCaption read FFooter write FFooter;
    property Header: TPageCaption read FHeader write FHeader;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property PageMargins: TPageMargins read FPageMargins write SetPageMargins;
    property Measurements: TMeasurements read FMeasurements write FMeasurements
                           default pmDefault;
    property PageNumbers: TPageNumbers read FPageNumbers write FPageNumbers
                          default pnNone;
    property ShowHFPage: Boolean read FShowHFPage write FShowHFPage
                           default True;
    property TrueTypeFont: Boolean read FTrueTypeFont write FTrueTypeFont
                           default False;
  end;

  procedure Register;

implementation

uses PSForm;

{$R 'PgSetup.dcr'}

procedure Register;
begin
  RegisterComponents('EPA',[TPageSetupDialogEx]);
end;

constructor TPageSetupDialogEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageMargins := TPageMargins.Create;
  FHeader  := TPageCaption.Create;
  FHeader.Text := '';
  FHeader.Enabled := True;
  FHeader.Alignment := taLeftJustify;
  FFooter  := TPageCaption.Create;
  FFooter.Text := '';
  FFooter.Enabled := True;
  FFooter.Alignment := taLeftJustify;
  FPageNumbers := pnNone;
  FShowHFPage := True;
  FTrueTypeFont := False;
  FBoldFont := False;
  FMeasurements := pmDefault;
end;

destructor TPageSetupDialogEx.Destroy;
begin
  FPageMargins.Free;
  FHeader.Free;
  FFooter.Free;
  inherited Destroy;
end;

procedure TPageSetupDialogEx.SetPageMargins(Value: TPageMargins);
begin
  FPageMargins.Assign(Value);
end;

function TPageSetupDialogEx.GetDefMeasurements: TMeasurements;
//Get units of measurement for user's system
var
  Buffer: PChar;
begin
  Buffer := StrAlloc(2);
  GetLocaleInfo(GetUserDefaultLCID, LOCALE_IMEASURE, Buffer, 2);
  if Buffer[0] = '0' then
    Result := pmMillimeters
  else
    Result := pmInches;
  StrDispose(Buffer);
end;

function TPageSetupDialogEx.Execute: Boolean;
var
  ucf: Single;
begin
  Result := False;
  if Printer.Printing then
  begin
    MessageDlg(PRINTINGMSG,mtError,[mbOK],0);
    Exit;
  end;
  if Printer.Printers.Count <= 0 then
  begin
    MessageDlg(NOPRINTERMSG,mtError,[mbOK],0);
    Exit;
  end;
  PageSetupForm := TPageSetupForm.Create(Application);
  try
    if BoldFont then PageSetupForm.Font.Style := [fsBold];
    PageSetupForm.HelpContext := HelpContext;
    if TrueTypeFont then PageSetupForm.Font.Name := 'Arial';
    if Measurements <> pmDefault then
      PageSetupForm.Measurements := Measurements
    else
      PageSetupForm.Measurements := GetDefMeasurements;
    if PageSetupForm.Measurements = pmMillimeters then
      ucf := 25.4
    else
      ucf := 1;
    with PageSetupForm do
    begin
      MarginLeft := PageMargins.Left*ucf;
      MarginRight := PageMargins.Right*ucf;
      MarginTop := PageMargins.Top*ucf;
      MarginBot := PageMargins.Bottom*ucf;
      if ShowHFPage then begin
        TabSheet2.TabVisible := True;
        CheckHeaderEnabled.Checked := Header.Enabled;
        CheckFooterEnabled.Checked := Footer.Enabled;
        CBPageNumbers.ItemIndex := Ord(PageNumbers);
        EditHeader.Text := Header.Text;
        EditFooter.Text := Footer.Text;
        with Header do
        begin
          if Alignment = taLeftJustify  then RBHeaderLeft.Checked := True;
          if Alignment = taCenter       then RBHeaderCenter.Checked := True;
          if Alignment = taRightJustify then RBHeaderRight.Checked := True;
        end;
        with Footer do
        begin
          if Alignment = taLeftJustify  then RBFooterLeft.Checked := True;
          if Alignment = taCenter       then RBFooterCenter.Checked := True;
          if Alignment = taRightJustify then RBFooterRight.Checked := True;
        end;
      end
      else TabSheet2.TabVisible := False;
      PageControl1.ActivePage := TabSheet1;
      if ShowModal = mrOK then
      begin
        if ShowHFPage then
        begin
          Header.Text := EditHeader.Text;
          Header.Enabled := CheckHeaderEnabled.Checked;
          Footer.Text := EditFooter.Text;
          Footer.Enabled := CheckFooterEnabled.Checked;
          with Header do
          begin
            if RBHeaderLeft.Checked   then Alignment := taLeftJustify;
            if RBHeaderCenter.Checked then Alignment := taCenter;
            if RBHeaderRight.Checked  then Alignment := taRightJustify;
          end;
          with Footer do
          begin
            if RBFooterLeft.Checked   then Alignment := taLeftJustify;
            if RBFooterCenter.Checked then Alignment := taCenter;
           if RBFooterRight.Checked  then Alignment := taRightJustify;
          end;
          PageNumbers := TPageNumbers(CBPageNumbers.ItemIndex);
        end;
        if ucf = 1 then
        begin
          PageMargins.Left := MarginLeft;
          PageMargins.Right := MarginRight;
          PageMargins.Top := MarginTop;
          PageMargins.Bottom := MarginBot;
        end
        else
        begin
          PageMargins.Left := floor(MarginLeft/ucf);
          PageMargins.Right := floor(MarginRight/ucf);
          PageMargins.Top := floor(MarginTop/ucf);
          PageMargins.Bottom := floor(MarginBot/ucf);
        end;
        Result := True;
      end;
    end;
  finally
    PageSetupForm.Free;
  end;
end;

end.
