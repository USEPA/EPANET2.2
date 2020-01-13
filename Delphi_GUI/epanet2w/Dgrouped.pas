unit Dgrouped;

{-------------------------------------------------------------------}
{                    Unit:    Dgrouped.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit with a dialog box for specifying a group editing      }
{   operation.                                                      }
{-------------------------------------------------------------------}

(*
    Control           Type        Purpose
    ----------------- ---------   ---------------------------
    EditOjbects       TComboBox   Selects Nodes or Links
    FilterProperty    TComboBox   Selects property to filter
    FilterRelation    TComboBox   Selects filter relation
    FilterValueTxt    TNumEdit    Sets filter property value
    EditAction        TComboBox   Selects editing action
    EditProperty      TComboBox   Selects property to edit
    EditValueTxt      TNumEdit    Sets edit value

    Results from this dialog might look like:
    "For Links with Diameter = 12, Multiply Roughness by 1.2"

    Results are returned to the calling procedure through the
    GetGroupEditParams procedure.
*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Uutils, Uglobals, ExtCtrls, NumEdit;

const
  TXT_JUNCTIONS = 'Junctions';
  TXT_PIPES     = 'Pipes';

// Junction properties that can be group edited
  JuncPropIndex: array[0..5] of Integer =
    (TAG_INDEX, JUNC_ELEV_INDEX, JUNC_DEMAND_INDEX, JUNC_PATTERN_INDEX,
     JUNC_EMITTER_INDEX, JUNC_INITQUAL_INDEX);

// Edit mask styles used in the NumEdit control for each junction property
  JuncEditStyle: array[0..5] of TEditStyle =
    (esNone, esNumber, esNumber, esNoSpace, esPosNumber, esPosNumber);

// Pipe properties that can be group edited
  PipePropIndex: array[0..5] of Integer =
    (TAG_INDEX, PIPE_DIAM_INDEX, PIPE_ROUGH_INDEX, PIPE_MLOSS_INDEX,
     PIPE_KBULK_INDEX, PIPE_KWALL_INDEX);

// Edit mask styles used in the NumEdit control for each pipe property
  PipeEditStyle: array[0..5] of TEditStyle =
    (esNone, esPosNumber, esPosNumber, esPosNumber, esNumber, esNumber);

// Types of editing action that can be taken
  ActionItems = 'Replace'#13'Multiply'#13'Add';
  ActionAdverb: array[0..2] of PChar = ('with','by','to');

type
  TGroupEditForm = class(TForm)
    EditObjects: TComboBox;
    FilterCheck: TCheckBox;
    FilterProperty: TComboBox;
    FilterRelationList: TComboBox;
    FilterValueTxt: TNumEdit;
    EditAction: TComboBox;
    EditProperty: TComboBox;
    EditLabel: TLabel;
    EditValueTxt: TNumEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure EditObjectsClick(Sender: TObject);
    procedure EditActionChange(Sender: TObject);
    procedure FilterCheckClick(Sender: TObject);
    procedure EditPropertyChange(Sender: TObject);
    procedure FilterPropertyChange(Sender: TObject);
    procedure FilterValueTxtEnter(Sender: TObject);
    procedure EditValueTxtEnter(Sender: TObject);
  private
    { Private declarations }
    procedure UpdatePropertyCombo(aCombo: TComboBox);
  public
    { Public declarations }
    procedure GetGroupEditParams(var ObjType: Integer; var PropIndex: Integer;
                var ActionType: Integer; var Value: String; var Filter: TFilter);
  end;

//var
//  GroupEditForm: TGroupEditForm;

implementation

{$R *.DFM}

procedure TGroupEditForm.FormCreate(Sender: TObject);
//--------------------------------------------------
// OnCreate handler for the form.
//--------------------------------------------------
var
  I: Integer;

begin
// Use current global font style
  Uglobals.SetFont(self);

// Load items into EditObjects combo box
  EditObjects.Items.Add(TXT_JUNCTIONS);
  EditObjects.Items.Add(TXT_PIPES);
  EditObjects.ItemIndex := 0;

// Update items in FilterProperty & EditProperty combo boxes
  UpdatePropertyCombo(FilterProperty);
  UpdatePropertyCombo(EditProperty);
  for I := Low(FilterRelation) to High(FilterRelation) do
    FilterRelationList.Items.Add(FilterRelation[I]);
  EditAction.Items.SetText(ActionItems);

// Initialize selected item in each combo boxes
  FilterRelationList.ItemIndex := 1;
  FilterProperty.ItemIndex := 0;
  EditProperty.ItemIndex := 0;
  EditAction.ItemIndex := 0;
  EditPropertyChange(Sender);
end;

procedure TGroupEditForm.FormShow(Sender: TObject);
//-------------------------------------------------
// OnShow event handler.
//-------------------------------------------------
begin
  EditObjects.SetFocus;
end;

procedure TGroupEditForm.BtnOKClick(Sender: TObject);
//----------------------------------------------------
// OnClick handler for OK button.
//----------------------------------------------------
var
  v: Single;
begin
  if EditValueTxt.Style in [esNumber, esPosNumber] then
  begin
    if Uutils.IsValidNumber(EditValueTxt.Text,v) then
      ModalResult := mrOK;
  end
  else ModalResult := mrOK;
end;

procedure TGroupEditForm.GetGroupEditParams(var ObjType: Integer;
  var PropIndex: Integer; var ActionType: Integer; var Value: String;
  var Filter: TFilter);
//------------------------------------------------------
// Returns group editing choices to calling procedure.
//------------------------------------------------------
begin
  Filter.Variable := -1;
  if EditObjects.ItemIndex = 0 then
  begin
    ObjType := JUNCS;
    PropIndex := JuncPropIndex[EditProperty.ItemIndex];
    if FilterCheck.Checked then
      Filter.Variable := JuncPropIndex[FilterProperty.ItemIndex];
  end
  else
  begin
    ObjType := PIPES;
    PropIndex := PipePropIndex[EditProperty.ItemIndex];
    if FilterCheck.Checked then
      Filter.Variable := PipePropIndex[FilterProperty.ItemIndex];
  end;
  ActionType := EditAction.ItemIndex;
  Value := EditValueTxt.Text;
  Filter.Relation := TRelationType(FilterRelationList.ItemIndex);
  Filter.StrValue := FilterValueTxt.Text;
end;

procedure TGroupEditForm.EditObjectsClick(Sender: TObject);
//---------------------------------------------------------
// OnClick handler for EditObjects combo box.
// Updates list of properties that can be selected
// for group editing.
//---------------------------------------------------------
begin
  UpdatePropertyCombo(EditProperty);
  UpdatePropertyCombo(FilterProperty);
end;

procedure TGroupEditForm.UpdatePropertyCombo(aCombo: TComboBox);
//-------------------------------------------------------------
// Changes entries in combobox aCombo depending on whether
// Junctions or Pipes is selected in the EditObjects combobox.
//-------------------------------------------------------------
var
  oldindex: Integer;
  I       : Integer;
begin
// Clear existing entries
  oldindex := aCombo.ItemIndex;
  aCombo.Clear;

// Junctions selected from EditObjects combobox
  if EditObjects.ItemIndex = 0 then
  begin
    for I := 0 to High(JuncPropIndex) do
      aCombo.Items.Add(JuncProps[JuncPropIndex[I]+PROP_INDEX_OFFSET].Name);
  end

// Pipes selected from EditObjects combobox
  else
  begin
    for I := 0 to High(PipePropIndex) do
      aCombo.Items.Add(PipeProps[PipePropIndex[I]+PROP_INDEX_OFFSET].Name);
  end;

// Restore old ItemIndex if possible
  if oldindex >= aCombo.Items.Count then
    aCombo.ItemIndex := 0
  else aCombo.ItemIndex := oldindex;
end;

procedure TGroupEditForm.EditActionChange(Sender: TObject);
//--------------------------------------------------------
// OnChange handler for EditAction combobox.
//--------------------------------------------------------
begin
  EditLabel.Caption := ActionAdverb[EditAction.ItemIndex];
end;

procedure TGroupEditForm.FilterCheckClick(Sender: TObject);
//--------------------------------------------------------
// OnClick handler for FilterCheck checkbox.
//--------------------------------------------------------
begin
   with FilterCheck do
   begin
     FilterProperty.Enabled := Checked;
     FilterRelationList.Enabled := Checked;
     FilterValueTxt.Enabled := Checked;
   end;
end;

procedure TGroupEditForm.EditPropertyChange(Sender: TObject);
//--------------------------------------------------------
// OnChange handler for EditProperty combobox.
// Clears text of property value when new property selected.
//--------------------------------------------------------
begin
  EditValueTxt.Text := '';
end;

procedure TGroupEditForm.FilterPropertyChange(Sender: TObject);
//--------------------------------------------------------
// OnChange handler for FilterProperty combobox.
// Clears text of property value when new property selected.
//--------------------------------------------------------
begin
  FilterValueTxt.Text := '';
end;

procedure TGroupEditForm.FilterValueTxtEnter(Sender: TObject);
//-----------------------------------------------------------
// OnEnter handler for FilterValueTxt NumEdit box.
// Changes editing style for the control depending on the
// type of filter property selected.
//-----------------------------------------------------------
var
  i: Integer;
begin
  i := FilterProperty.ItemIndex;
  if EditObjects.ItemIndex = 0 then
    FilterValueTxt.Style := JuncEditStyle[i]
  else
    FilterValueTxt.Style := PipeEditStyle[i];
end;

procedure TGroupEditForm.EditValueTxtEnter(Sender: TObject);
//-----------------------------------------------------------
// OnEnter handler for EditValueTxt NumEdit box.
// Changes editing style for the control depending on the
// type of group editing property selected.
//-----------------------------------------------------------
var
  i: Integer;
begin
  i := EditProperty.ItemIndex;
  if EditObjects.ItemIndex = 0 then
    EditValueTxt.Style := JuncEditStyle[i]
  else
    EditValueTxt.Style := PipeEditStyle[i];
end;

procedure TGroupEditForm.BtnHelpClick(Sender: TObject);
begin
  HtmlHelp(GetDesktopWindow, Application.HelpFile, HH_HELP_CONTEXT, 282);
end;

end.
