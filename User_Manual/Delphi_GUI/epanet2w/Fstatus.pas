unit Fstatus;

{-------------------------------------------------------------------}
{                    Unit:    Fstatus.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   MDI child form that lists error/warning messages and the        }
{   times at which status changes occur after a network             }
{   simulation has been run.                                        }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, Clipbrd, System.UITypes,
  Xprinter, Uglobals, Uutils;

const
  MSG_1 = 'Unable to load more than ';
  MSG_2 = ' lines into Status Report.';

type
  TStatusForm = class(TForm)
    FileViewer: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure SetFont;
  public
    { Public declarations }
    procedure CopyTo;
    procedure Print(Destination: TDestination);
    procedure RefreshStatusReport;
    procedure SelectText(const S: String);
  end;

//var
//  StatusForm: TStatusForm;

implementation

{$R *.DFM}

uses Dcopy, Fmain;

procedure TStatusForm.FormClose(Sender: TObject; var Action: TCloseAction);
//--------------------------------------
// OnClose handler for form.
//--------------------------------------
begin
  Action := caFree;
end;


procedure TStatusForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1)
  then HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 177);
end;

procedure TStatusForm.FormActivate(Sender: TObject);
//---------------------------------------------------
// OnActivate handler for the form.
// Disables Options speedbutton on MainForm.
//---------------------------------------------------
begin
  MainForm.TBOptions.Enabled := False;
end;


procedure TStatusForm.SetFont;
//----------------------------------------------------
// Sets font style of the FileViewer control.
//----------------------------------------------------
begin
  if BoldFonts then FileViewer.Font.Style := [fsBold]
  else FileViewer.Font.Style := [];
end;


procedure TStatusForm.RefreshStatusReport;
//----------------------------------------------------
// Reloads the FileViewer control with the contents
// of the Status Report File generated from a
// network analysis.
//----------------------------------------------------
begin
// Set the FileViewer's font & clear its contents
  FileViewer.Clear;
  SetFont;

// Make sure that the report file exists
  if FileExists(TempReportFile) then with FileViewer do
  try

  // Load the contents of the file into the FileViewer
    Items.LoadFromFile(TempReportFile);
  except
    On E: Exception do
      Uutils.MsgDlg(MSG_1 + IntToStr(Items.Count) + MSG_2, mtWarning, [mbOK],
      MainForm);
  end;
  if FileViewer.Items.Count >= 1 then FileViewer.ItemIndex := 0;
end;


procedure TStatusForm.Print(Destination: TDestination);
//---------------------------------------------------------------
// Prints Status Report to Destination (printer or preview form).
//---------------------------------------------------------------
var
  i: Integer;
begin
  with MainForm.thePrinter do
  begin
    BeginJob;
    SetDestination(Destination);
    with FileViewer.Font do
      SetFontInformation(Name, Size, Style);
    with FileViewer do
      for i := 0 to Items.Count - 1 do PrintLine(Items[i]);
    EndJob;
  end;
end;


procedure TStatusForm.SelectText(const S: String);
//--------------------------------------------------
// Locates text string S in the FileViewer.
// (Called from MainForm to locate a Warning message
// after an analysis ends with warning messages).
//--------------------------------------------------
var
  Buf: array[0..255] of Char;
begin
  StrPCopy(Buf, S);
  with FileViewer do
  begin
    ItemIndex := Perform(LB_SELECTSTRING,0,LongInt(@Buf));
  end;
end;


procedure TStatusForm.CopyTo;
//----------------------------------------------------
// Copies contents of the FileViewer to either a file
// or to the Clipboard.
//----------------------------------------------------
begin
// Create the CopyTo dialog form
  with TCopyToForm.Create(self) do
  try

  // Disable format selection (since it has to be Text)
    FormatGroup.ItemIndex := 2;
    FormatGroup.Enabled := False;

  // Show the form modally
    if ShowModal = mrOK then with FileViewer do
    begin

    // If user supplies a file name then copy contents of FileViewer to it
      if Length(DestFileName) > 0 then
        CopyFile(PChar(TempReportFile), PChar(DestFileName), FALSE)

    // Otherwise copy the contents into the Clipboard
      else Clipboard.SetTextBuf(PChar(Items.Text));
    end;

// Free the CopyTo dialog form
  finally
    Free;
  end;
end;

end.
