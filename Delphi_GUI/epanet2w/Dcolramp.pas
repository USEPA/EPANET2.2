unit Dcolramp;

{-------------------------------------------------------------------}
{                    Unit:    Dcolramp.pas                          }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit that contains a color ramp selection dialog box.      }
{-------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Uglobals;

const

  MAXSCHEME = 8;  // Highest index of available color schemes
  MAXCOLOR = 4;   // Highest index of colors in a color scheme

  Schemes: array[0..MAXSCHEME] of String =
  ('Rainbow',
   'Red',
   'Orange',
   'Yellow',
   'Green',
   'Blue',
   'Purple',
   'Magenta',
   'Gray');

   SchemeColors: array[0..MAXSCHEME, 0..MAXCOLOR] of TColor =
   (($FF0000, $FFFF00, $FF00,   $FFFF,   $FF),
    ($EAEAEA, $8080FF, $FF,     $99,     $66),
    ($F0FBFF, $99CCFF, $3399FF, $66CC,   $3399),
    ($F0FBFF, $99FFFF, $33CCFF, $99CC,   $8080),
    ($F1F1F1, $99FF99, $FF00,   $9900,   $6600),
    ($EAEAEA, $FF9999, $FF3333, $CC0000, $800000),
    ($FFCCFF, $FF99FF, $FF00CC, $CC0099, $660066),
    ($F1F1F1, $CC99FF, $CC33FF, $9900CC, $660099),
    ($F8F8F8, $C0C0C0, $808080, $424242, $0));


type
  TColorRampForm = class(TForm)
    Shape0: TShape;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeColorScheme(const I: Integer);
  public
    { Public declarations }
    Colors: array [0..MAXCOLOR] of TColor;
  end;

//var
//  ColorRampForm: TColorRampForm;

implementation

{$R *.DFM}

procedure TColorRampForm.FormCreate(Sender: TObject);
//--------------------------------------------------
// OnCreate event handler.
// Loads color scheme labels into ComboBox1.
//--------------------------------------------------
var
  I: Integer;
begin
  Uglobals.SetFont(self);
  with ComboBox1 do
  begin
    for I := 0 to MAXSCHEME do
      Items.Add(Schemes[I]);
    ItemIndex := 0;
  end;
  ChangeColorScheme(0);
end;

procedure TColorRampForm.ComboBox1Change(Sender: TObject);
//-------------------------------------------------------
// OnChange event handler for ComboBox1.
//-------------------------------------------------------
begin
  ChangeColorScheme(ComboBox1.ItemIndex);
end;

procedure TColorRampForm.ChangeColorScheme(const I: Integer);
//----------------------------------------------------------
// Updates display of currently selected color scheme.
//----------------------------------------------------------
var
  J: Integer;
begin
  for J := 0 to MAXCOLOR do
    with FindComponent('Shape' + IntToStr(J)) as TShape do
    begin
      Brush.Color := SchemeColors[I,J];
      Colors[J] := SchemeColors[I,J];
    end;
end;

end.
