{
   TOpenTxtFileDialog Component
   *****************************

   A Delphi component that implements an Open File common dialog
   control with a preview window that displays the first few lines
   of the file selected to be opened. It is derived from the
   TOpenDialog control using the Delphi TOpenPictureDialog component
   as a model.

   The WordWrap property determines if text in the preview window is
   word wrapped or not. An OnPreview event is available that allows
   one to customize the text displayed in the preview window.

   Version: 1.0
   Author:  L. Rossman
   Date:    3/30/00
            9/5/05
            4/18/14
}

unit OpenDlg;

{$R-}

interface

uses Messages, Windows, SysUtils, Classes, Controls, StdCtrls, Graphics,
  ExtCtrls, Buttons, Dialogs, ExtDlgs;

const
  MAXLINES = 50;

type

  TPreviewEvent = procedure(Sender: TObject; Fname: String; var S: String;
    var WW: Boolean) of Object;

{ TOpenTxtFileDialog }

  TOpenTxtFileDialog = class(TOpenDialog)
  private
    FPreviewPanel: TPanel;
    FShowPreview: Boolean;
    FTextFileLabel: TLabel;
    FTextPanel: TPanel;
    FTextLabel: TLabel;
    FWordWrap:  Boolean;
    FOnPreview: TPreviewEvent;
  protected
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
    procedure SetShowPreview(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
  published
    property OnPreview: TPreviewEvent read FOnPreview write FOnPreview;
    property ShowPreview: Boolean read FShowPreview write SetShowPreview;
    property WordWrap:  Boolean read FWordWrap write SetWordWrap;
  end;

  procedure Register;

implementation

uses Consts, CommDlg, Forms, Dlgs;

{ TOpenTxtFileDialog }

//{$R EXTDLGS.RES}
{$R 'OpenDlg.dcr'}

constructor TOpenTxtFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Filter := GraphicFilter(TGraphic);
  //Options := Options - [ofEnableSizing];                                     //(4/18/14)
  FShowPreview := True;
  FWordWrap := False;
  FOnPreview := nil;
  FPreviewPanel := TPanel.Create(Self);
  with FPreviewPanel do
  begin
    Name := 'PreviewPanel';
    Caption := '';
    //SetBounds(204, 5, 169, 200);                                             //(4/18/14)
    Align := alRight;                                                          //(4/18/14)
    BevelOuter := bvNone;
    BorderWidth := 6;
    TabOrder := 1;
    FTextFileLabel := TLabel.Create(Self);
    with FTextFileLabel do
    begin
      Name := 'TextFileLabel';
      Caption := 'Preview';                                                    //(4/18/14)
      SetBounds(6, 6, 157, 23);
      Align := alTop;
      AutoSize := False;
      Parent := FPreviewPanel;
    end;
    FTextPanel := TPanel.Create(Self);
    with FTextPanel do
    begin
      Name := 'TextPanel';
      Caption := '';
      SetBounds(6, 29, 157, 145);
      Align := alClient;
      BevelInner := bvRaised;
      BevelOuter := bvLowered;
      Color := clWindow;
      TabOrder := 0;
      Parent := FPreviewPanel;
      ParentBackground := False;
      FTextLabel := TLabel.Create(Self);
      with FTextLabel do
      begin
        Name := 'TextLabel';
        Caption := '';
        //Color := clWindow;
        Align := alClient;
        AutoSize := False;
        Font.Name := 'Courier New';                                            //(4/18/14)
        Font.Size := 8;                                                        //(4/18/14)
        WordWrap := FWordWrap;
        Parent := FTextPanel;
        ParentColor := True;
      end;
    end;
  end;
end;

destructor TOpenTxtFileDialog.Destroy;
begin
  FTextLabel.Free;
  FTextPanel.Free;
  FTextFileLabel.Free;
  FPreviewPanel.Free;
  inherited Destroy;
end;

procedure TOpenTxtFileDialog.SetShowPreview(Value: Boolean);
begin
  if FShowPreview <> Value then
  begin
    FShowPreview := Value;
  end;
end;

procedure TOpenTxtFileDialog.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    FTextLabel.WordWrap := Value;
  end;
end;

procedure TOpenTxtFileDialog.DoSelectionChange;
var
  F: TextFile;
  S: String;
  Line: String;
  LineCount: Integer;
  FullName: String;
  ValidTextFile: Boolean;
  WW: Boolean;

  function ValidFile(const FileName: string): Boolean;
  begin
    Result := GetFileAttributes(PChar(FileName)) <> $FFFFFFFF;
  end;

begin
  S := '';
  WW := FWordWrap;
  FullName := FileName;
  ValidTextFile := FileExists(FullName) and ValidFile(FullName);
  if ValidTextFile and FShowPreview then
  begin
    if Assigned(FOnPreview) then FOnPreview(self, FullName, S, WW);
    if S = '' then
    try
      WW := FWordWrap;
      AssignFile(F,Fullname);
      {$I-}
      Reset(F);
      {$I+}
      if (IOResult = 0) then
      try
        LineCount := 0;
        while (LineCount < MAXLINES) and (not EOF(F)) do
        begin
          ReadLn(F,Line);
          S := S + Line + #13;
          Inc(LineCount);
        end;
      except
        ValidTextFile := False;
      end;
    finally
      CloseFile(F);
    end;
  end;
  FTextLabel.WordWrap := WW;
  if not ValidTextFile then
    FTextLabel.Caption := ''
  else
    FTextLabel.Caption := S;
  inherited DoSelectionChange;
end;

procedure TOpenTxtFileDialog.DoClose;
begin
  inherited DoClose;
  FTextLabel.Caption := '';
  { Hide any hint windows left behind }
//  Application.HideHint;
end;

procedure TOpenTxtFileDialog.DoShow;
var
  PreviewRect: TRect;
begin
  if FShowPreview then
  begin

  { Set preview area to entire dialog }
  GetClientRect(Handle, PreviewRect);
  { Move preview area to right of static area }
  PreviewRect.Left := GetStaticRect.Right;
  Inc(PreviewRect.Top, 4);
  FPreviewPanel.BoundsRect := PreviewRect;
  FPreviewPanel.ParentWindow := Handle;

  end
  else
  begin
    FPreviewPanel.Visible := False;
  end;

  inherited DoShow;
end;

function TOpenTxtFileDialog.Execute : Boolean;
begin
  if FShowPreview and NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE' else
    Template := nil;
  Result := inherited Execute;
end;

procedure Register;
begin
  RegisterComponents('EPA', [TOpenTxtFileDialog]);
end;

end.
