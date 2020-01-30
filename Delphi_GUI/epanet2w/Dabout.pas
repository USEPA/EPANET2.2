unit Dabout;

{-------------------------------------------------------------------}
{                    Unit:    Dabout.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Form unit containing the "About" dialog box for EPANET2W.       }
{-------------------------------------------------------------------}

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBoxForm = class(TForm)
    Panel1: TPanel;
    ProductName: TLabel;
    Version: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Build: TLabel;
    Panel2: TPanel;
    ProgramIcon: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  AboutBoxForm: TAboutBoxForm;

implementation

{$R *.DFM}

procedure TAboutBoxForm.FormCreate(Sender: TObject);
begin
   //Build.Caption := 'Build 2.2.01';
end;

end.

