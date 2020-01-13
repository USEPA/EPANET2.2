unit Dcopy;

{-------------------------------------------------------------------}
{                    Unit:    Dcopy.pas                             }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog that gets choice of format and          }
{   destination that a view should be copied to.                    }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Uglobals;

const
  TXT_COPY = 'Copy ';
  TXT_SAVE_AS = 'Save As';
  FilterTxt: array[0..2] of PChar =
   ('Bitmap files (*.BMP)|*.BMP|All files|*.*',
    'EMF files (*.EMF)|*.EMF|All files|*.*',
    'Text files (*.TXT)|*.TXT|All files|*.*');
  ExtensionTxt: array[0..2] of PChar =
    ('.bmp','.emf','.txt');

type
  TCopyToForm = class(TForm)
    DestGroup: TRadioGroup;
    FormatGroup: TRadioGroup;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DestFileName: String;
  end;

//var
//  CopyToForm: TCopyToForm;

implementation

{$R *.DFM}

uses FMain;

procedure TCopyToForm.FormCreate(Sender: TObject);
//---------------------------------------------
// OnCreate handler for form.
//---------------------------------------------
var
  s: String;
  n: Integer;
begin
  Uglobals.SetFont(self);
  s := '';
  with MainForm do
  begin
    s := ActiveMDIChild.Caption;
    n := Pos(' -',s);
    if n > 0 then s := Copy(s, 1, n-1);
  end;
  Caption := TXT_COPY + s;
end;


procedure TCopyToForm.BtnOKClick(Sender: TObject);
//--------------------------------------------
// OnClick handler for OK button.
// Retrieves name of file to copy to.
//--------------------------------------------
var
  Ftype: Integer;
begin
// Use the MainForm's SaveDialog control to obtain the file name
  DestFileName := '';
  if DestGroup.ItemIndex = 1 then
  with MainForm.SaveDialog do
  begin
    Title := TXT_SAVE_AS;
    Ftype := FormatGroup.ItemIndex;
    Filter := FilterTxt[Ftype];
    DefaultExt := Copy(ExtensionTxt[Ftype],2,3);
    Filename := '*' + ExtensionTxt[Ftype];
    if Execute then
    begin
      DestFileName := Filename;
      ModalResult := mrOK;
    end
    else ModalResult := mrCancel;
    DefaultExt := '';
  end
  else ModalResult := mrOK;
  Hide;
end;


procedure TCopyToForm.BtnCancelClick(Sender: TObject);
//-------------------------------------------------
// OnClick handler for the Cancel button.
//-------------------------------------------------
begin
  ModalResult := mrCancel;
  Hide;
end;


procedure TCopyToForm.BtnHelpClick(Sender: TObject);
//-----------------------------------------------
// OnClick handler for the Help button.
//-----------------------------------------------
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 301);
end;

end.
