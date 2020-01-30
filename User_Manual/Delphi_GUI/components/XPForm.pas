unit XPForm;
(***********************************************************************

  This unit contains a Printing Progress dialog form that accompanies
  the TPrinterControl component contained in the XPrinter unit.

***********************************************************************)
  
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TProgressForm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  ProgressForm: TProgressForm;

implementation

{$R *.DFM}

end.
