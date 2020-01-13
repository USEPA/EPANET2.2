unit Dlabel;

{-------------------------------------------------------------------}
{                    Unit:    Dlabel.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.0                                   }
{                    Date:    5/29/00                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit which consists of a frameless modal dialog used to    }
{   enter a Text Label on the network map.                          }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons;

type
  TLabelForm = class(TForm)
    Edit1: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TLabelForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLabelForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ModalResult := mrOK;
  end;
  if Key = #27 then
  begin
    Key := #0;
    ModalResult := mrCancel;
  end;
end;

procedure TLabelForm.FormCreate(Sender: TObject);
begin
  Edit1.Left := 0;
  Edit1.Top := 0;
  ClientWidth := Edit1.Width;
  ClientHeight := Edit1.Height;
end;

end.
