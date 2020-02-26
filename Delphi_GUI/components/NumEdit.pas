{
   TNUMEDIT Component
   ******************

   This is a Delphi component derived from the edit box (TEdit) component
   that restricts text entry to either a number or a string with no spaces.
   It adds only one property, named Style, to those of TEdit. The Style
   property can have one of the following values:
      esNone      -- accepts any characetr (same as a TEdit control)
      esNumber    -- restricts text entry to a numerical value
      esPosNumber -- restricts text entry to a number >= 0
      esNoSpace   -- accepts any character except a space, double quote or
                     semicolon.

   Version: 2.00
   Author:  L. Rossman
   Date:    9/2/05
}

unit NumEdit;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TEditStyle = (esNone, esNumber, esPosNumber, esNoSpace);
  TNumEdit = class(TEdit)
  private
    { Private declarations }
    FStyle      : TEditStyle;
  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Style: TEditStyle read FStyle write Fstyle;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property HideSelection;
    property MaxLength;
    property Modified;
    property SelLength;
    property SelStart;
    property SelText;
    property Ctl3D;
    property Color;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    property Left;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation
{$R 'Numedit.dcr'}

procedure Register;
begin
  RegisterComponents('EPA', [TNumEdit]);
end;

constructor TNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := esNone;
end;

procedure TNumEdit.KeyPress(var Key: Char);
var
  S: String;
  X: Extended;
begin
  inherited KeyPress(Key);

  // Allow backspace key press
  if Key = #8 then Exit;

  // For numeric entries
  if (FStyle in [esNumber, esPosNumber]) then
  begin

    // Insert the key character into the current text
    S := Text;
    Delete(S, SelStart+1, SelLength);
    Insert(Key, S, SelStart+1);

    // Add a 1 to complete a partial numeric entry
    S := S + '1';

    // Check that this creates a valid number
    try
      X := StrToFloat(S);

      // Check if a positive number is required
      if (FStyle = esPosNumber) and (X < 0.0) then Key := #0;

    // Ignore the key if we don't have a valid number
    except
      on EConvertError do Key := #0;
    end;
  end

  // For the NoSpace style, ignore spaces, double quotes & semicolons
  else if (FStyle = esNoSpace) then
  begin
    if CharInSet(Key,[' ', '"', ';']) then Key := #0;
  end;
end;

end.
