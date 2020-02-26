unit Dsource;

{-------------------------------------------------------------------}
{                    Unit:    Dsource.pas                           }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box that edits water quality source     }
{   options for a node.                                             }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Uglobals, NumEdit;

type
  TSourceForm = class(TForm)
    NumEdit1: TNumEdit;
    Edit2: TEdit;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
    theNode: TNode;
    theIndex: Integer;
    theItemIndex: Integer;
  public
    { Public declarations }
    Modified: Boolean;
  end;

//var
//  SourceForm: TSourceForm;

implementation

{$R *.DFM}

procedure TSourceForm.FormCreate(Sender: TObject);
//--------------------------------------------------
// OnCreate handler for form.
//--------------------------------------------------
var
  i: Integer;
begin
// Set form's font
  Uglobals.SetFont(self);

// Get pointer to node being edited
  theNode := Node(CurrentList, CurrentItem[CurrentList]);

// Get index of Source Quality property for the node
  case CurrentList of
  JUNCS:   theIndex := JUNC_SRCQUAL_INDEX;
  RESERVS: theIndex := RES_SRCQUAL_INDEX;
  TANKS:   theIndex := TANK_SRCQUAL_INDEX;
  else     theIndex := -1;
  end;

// Load current source quality properties into the form
  if theIndex > 0 then
  begin
    NumEdit1.Text := theNode.Data[theIndex];
    Edit2.Text := theNode.Data[theIndex+1];
    RadioGroup1.ItemIndex := 0;
    for i := Low(SourceType) to High(SourceType) do
      if CompareText(theNode.Data[theIndex+2],SourceType[i]) = 0 then
      begin
        RadioGroup1.ItemIndex := i;
        break;
      end;
  end;
  theItemIndex := RadioGroup1.ItemIndex;
  Modified := False;
end;

procedure TSourceForm.BtnOKClick(Sender: TObject);
//----------------------------------------------------
// OnClick handler for OK button.
// Transfers data from form to node being edited.
//----------------------------------------------------
begin
  if theIndex > 0 then
  begin
    theNode.Data[theIndex] := NumEdit1.Text;
    theNode.Data[theIndex+1] := Edit2.Text;
    theNode.Data[theIndex+2] := SourceType[RadioGroup1.ItemIndex];
  end;
  if (NumEdit1.Modified)
  or (Edit2.Modified)
  or (RadioGroup1.ItemIndex <> theItemIndex)
  then Modified := True;
  ModalResult := mrOK;
end;

procedure TSourceForm.BtnCancelClick(Sender: TObject);
//----------------------------------------------------
// OnClick handler for Cancel button.
//----------------------------------------------------
begin
  ModalResult := mrCancel;
end;

procedure TSourceForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 245);
end;

end.
