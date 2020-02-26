unit Dinperr;

{-------------------------------------------------------------------}
{                    Unit:    Dinperr.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit that lists any error messages generated when a        }
{   file in .INP format is imported.                                }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, System.UITypes, Xprinter, Uglobals;

type
  TInpErrForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InpErrForm: TInpErrForm;

implementation

{$R *.DFM}

uses Fmain;

procedure TInpErrForm.FormCreate(Sender: TObject);
//------------------------------------------------
// Form's OnCreate message handler.
//------------------------------------------------
begin
  Uglobals.SetFont(self);
  Memo1.Font.Style := Font.Style;
  Button2.Enabled := MainForm.MnuPageSetup.Enabled;
end;

procedure TInpErrForm.Button2Click(Sender: TObject);
//--------------------------------------------------
// OnClick handler for Print button.
//--------------------------------------------------
var
  i: Integer;
begin
  with MainForm.thePrinter do
  begin
    BeginJob;
    SetDestination(dPrinter);
    with Memo1.Font do
      SetFontInformation(Name, Size, Style);
    with Memo1 do
      for i := 0 to Lines.Count - 1 do PrintLine(Lines[i]);
    EndJob;
  end;
end;

procedure TInpErrForm.Button3Click(Sender: TObject);
//--------------------------------------------------
// OnClick handler for Error Codes button.
//--------------------------------------------------
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 298);
end;

end.
