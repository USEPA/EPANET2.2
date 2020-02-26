unit Dmapdim;

{-------------------------------------------------------------------}
{                    Unit:    Dmapdim.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box sets dimensions of the Network Map. }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, System.UITypes,
  Uglobals, Uutils, NumEdit;

const
  MSG_ILLEGAL_MAP_LIMITS = 'Illegal map limits.';

type
  TMapDimensionsForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    MapUnits: TRadioGroup;
    BtnAuto: TButton;
    URYEdit: TNumEdit;
    URXEdit: TNumEdit;
    LLYEdit: TNumEdit;
    LLXEdit: TNumEdit;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnAutoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadDimensions;
    procedure UnloadDimensions;
  end;

//var
//  MapDimensionsForm: TMapDimensionsForm;

implementation

{$R *.DFM}

uses Uimport, Uinput;

procedure TMapDimensionsForm.FormCreate(Sender: TObject);
//-----------------------------------------------
// OnCreate handler for form.
//-----------------------------------------------
begin
  Uglobals.SetFont(self);
end;

procedure TMapDimensionsForm.LoadDimensions;
//----------------------------------------
// Loads current map dimensions into form.
//----------------------------------------
begin
  with MapDimensions do
  begin
    LLXEdit.Text := FloatToStrF(LowerLeft.X,ffFixed,18,Digits);
    LLYEdit.Text := FloatToStrF(LowerLeft.Y,ffFixed,18,Digits);
    URXEdit.Text := FloatToStrF(UpperRight.X,ffFixed,18,Digits);
    URYEdit.Text := FloatToStrF(UpperRight.Y,ffFixed,18,Digits);
    MapUnits.ItemIndex := Ord(Units);
  end;
end;

procedure TMapDimensionsForm.UnloadDimensions;
//-----------------------------------------
// Unloads contents of form into map scale.
//-----------------------------------------
begin
  Uutils.GetExtended(LLXEdit.Text,MapDimensions.LowerLeft.X);
  Uutils.GetExtended(LLYEdit.Text,MapDimensions.LowerLeft.Y);
  Uutils.GetExtended(URXEdit.Text,MapDimensions.UpperRight.X);
  Uutils.GetExtended(URYEdit.Text,MapDimensions.UpperRight.Y);
  MapDimensions.Units := TMapUnits(MapUnits.ItemIndex);
  Uinput.UpdateMapUnits;
end;

procedure TMapDimensionsForm.BtnAutoClick(Sender: TObject);
var
  Dimensions: TMapDimensions;
begin
  Dimensions := DefMapDimensions;
  Dimensions.Units := TMapUnits(MapUnits.ItemIndex);
  if Dimensions.Units = muDegrees then Dimensions.Digits := MAXDEGDIGITS
  else Dimensions.Digits := 2;
  Uimport.SetMapDimensions(Dimensions);
  with Dimensions do
  begin
    LLXEdit.Text := FloatToStrF(LowerLeft.X,ffFixed,18,Digits);
    LLYEdit.Text := FloatToStrF(LowerLeft.Y,ffFixed,18,Digits);
    URXEdit.Text := FloatToStrF(UpperRight.X,ffFixed,18,Digits);
    URYEdit.Text := FloatToStrF(UpperRight.Y,ffFixed,18,Digits);
  end;
end;

procedure TMapDimensionsForm.BtnOKClick(Sender: TObject);
//-------------------------------------------------------
// OnClick handler for OK button.
// Checks for valid map dimensions.
//-------------------------------------------------------
var
  x1,x2,y1,y2: Single;
begin
  Uutils.GetSingle(LLXEdit.Text,x1);
  Uutils.GetSingle(LLYEdit.Text,y1);
  Uutils.GetSingle(URXEdit.Text,x2);
  Uutils.GetSingle(URYEdit.Text,y2);
  if (x1 = x2) or (y1 = y2) then
  begin
    Uutils.MsgDlg(MSG_ILLEGAL_MAP_LIMITS,mtError,[mbOK]);
    LLXEdit.SetFocus;
  end
  else ModalResult := mrOK;
end;

procedure TMapDimensionsForm.FormKeyPress(Sender: TObject; var Key: Char);
//------------------------------------------------
// OnKeyPress handler for form.
// Moves focus to next dialog control on the form.
//------------------------------------------------
begin
  if Key = #13 then
  begin
    Key := #0;
    Perform(WM_NEXTDLGCTL,0,0);
  end;
end;

procedure TMapDimensionsForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 288);
end;

end.
