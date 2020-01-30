unit Xprinter;
{
   TPrintControl Component
   ***********************

   This is a non-visual Delphi component that contains various functions
   for printing text, tables, and graphics. It includes a print preview
   feature and the ability to add headers, footers, and page numbering.

   Printing is accomplished via a two-stage process:
   1. Following a call to the BeginJob procedure, printing commands
      are rendered on a series of metafiles, one for each printed page.
   2. Following a call of the EndJob procedure the metafiles are either
      played back through Delphi's TPrinter object or are displayed one
      page at a time on a Preview form.

   This component also uses the XPForm.pas file which contains the Print
   Preview form.

   Author:  L. Rossman
   Version: 1.0
   Date:    1/30/00

   All references to TChart component removed - 4/29/2018 (LR)
}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, Printers, ExtCtrls, //VCLTee.Chart,
  Buttons, Dialogs, System.UITypes, XPForm;


const
  MaxHeaderLines = 5;                     { Number of allowable header lines }
  MaxFooterLines = 5;                     { Number of allowable footer lines }
  MaxColumns = 20;                        { Number of allowable columns }
  DefaultDPI = 300;                       { Default dots per inch }

type

  TDestination = (dPrinter, dPreview);
  TJustify = (jLeft, jRight, jCenter);
  TTableStyle = set of (sBorder, sVerticalGrid, sHorizontalGrid);
  TUnits = (uInches, uMM);

  THeaderRecord = Record
     Text: String;                        { Header text }
     YPosition: Single;                   { Inches from the top }
     Alignment: TJustify;                 { jLeft jCenter jRight }
     FontName: String;                    { Font name }
     FontSize: Integer;                   { Font size }
     FontStyle: TFontStyles;              { Font style }
     End;

  TFooterRecord = Record
     Text: String;                        { Footer text }
     YPosition: Single;                   { Inches from the top }
     Alignment: TJustify;                 { jLeft jCenter jRight }
     FontName: String;                    { Font name }
     FontSize: Integer;                   { Font size }
     FontStyle: TFontStyles;              { Font style }
     End;

  THeaderCoordinates = Record
     XTop: Single;
     YTop: Single;
     XBottom: Single;
     YBottom: Single;
     Boxed: Boolean;
     Shading: Word;
     LineWidth: Word;
     End;

  TFooterCoordinates = Record
     XTop: Single;
     YTop: Single;
     XBottom: Single;
     YBottom: Single;
     Boxed: Boolean;
     Shading: Word;
     LineWidth: Word;
     End;

  TPageNumberRecord = Record
     YPosition: Single;
     Text: String;
     Alignment: TJustify;
     FontName: String;
     FontSize: Word;
     FontStyle: TFontStyles;
     End;

  TColumnInformationRecord = Record
     XPosition: Single;
     Length: Single;
     Text: array [1..MaxHeaderLines] of String;
     Alignment: TJustify;
     End;

  TTableHeaderRecord = Record
    FontName:  String;
    FontSize:  Word;
    FontStyle: TFontStyles;
    Boxed:     Boolean;
    Shading:   Word;
    End;

  TPrintPage = TMetafile;            { Each page stored in a metafile }
  TPrintCanvas = TMetafileCanvas;    { Metafile canvas }
  TPageList = class(TList)           { List of printed pages }
  public
    destructor Destroy; override;
    procedure ClearPageList;
    function GetPage(const Index: Integer): TPrintPage;
  end;

  TPrintControl = class;

  TPreviewForm = class(TForm)
    Panel1: TPanel;
    PrintBtn: TButton;
    CloseBtn: TButton;
    Panel2: TPanel;
    ScrollBox: TScrollBox;
    PaintBox1: TPaintBox;
    ShwGrdBtn: TSpeedButton;
    PageWidthBtn: TSpeedButton;
    FullPageBtn: TSpeedButton;
    PrevPageBtn: TButton;
    NextPageBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PrintBtnClick(Sender: TObject);
    procedure NextPageBtnClick(Sender: TObject);
    procedure PrevPageBtnClick(Sender: TObject);
    procedure PageWidthBtnClick(Sender: TObject);
    procedure FullPageBtnClick(Sender: TObject);
    procedure ShwGrdBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    PageDisplaying : Integer;
  public
    { Public declarations }
    PrintControl : TPrintControl;
  end;

  TPrintControl = class(TComponent)

  private
    { Private declarations }
    fCurrentPage   : Integer;       {Current page}
    fCanvas        : TPrintCanvas;  {Canvas of current page's metafile}
    fPage          : TPrintPage;    {Current page's metafile}
    fPages         : TPageList;     {List of page metafiles}
    fPrinter       : TPrinter;      {Pointer to Printer object}
    fPreviewForm   : TPreviewForm;  {Pointer to preview form}
    fProgressForm  : TProgressForm; {Pointer to progress form}
    fDoneDrawing   : Boolean;       {Signals when all canvases drawn on}
    fCancelPrinting: Boolean;       {Signal to cancel printing}

    fAutoPaging    : Boolean;       {Are new pages automatically generated? }
    fDestination   : TDestination;  {Destination for print-out}
    fFont          : TFont;         {Current font}
    fPen           : TPen;          {Current pen}
    fOrientation   : TPrinterOrientation; {Page orientation}
    fTableStyle    : TTableStyle;   { Table grid style }
    fTitle         : String;        { Job title for Print manager }
    fUnits         : TUnits;        { Measurement units }
    fHeaders       : Boolean;       { Print headers }
    fFooters       : Boolean;       { Print footers }
    fPageNumbers   : Boolean;       { Print page numbers }
    fShowProgress  : Boolean;       { Display progress form }

    PixelsPerInchVertical  : Integer; { # of pixels per inch along Y axis }
    PixelsPerInchHorizontal: Integer; { # of pixels per inch along X axis }
    UnitLineWidth          : Integer; { # pixels per line width = 1 }
    TotalPageHeightInches  : Single;  { Height of page in inches }
    TotalPageWidthInches   : Single;  { Width of page in inches }
    GutterLeft             : Integer; { Unprintable area on left }
    GutterRight            : Integer; { Unprintable area on right }
    GutterTop              : Integer; { Unprintable area on top }
    GutterBottom           : Integer; { Unprintable area on bottom }
    TopMargin              : Integer; { Top margin in pixels }
    BottomMargin           : Integer; { Bottom margin in pixels }
    LeftMargin             : Integer; { Left margin in pixels }
    RightMargin            : Integer; { Right margin in pixels }
    DetailTop              : Single;  { Inches from the top where the }
                                      { detail section starts }
    DetailBottom           : Single;  { Inches from the top where the
                                      { detail section ends }
    CurrentX               : Single;  { Current X position (inches) }
    CurrentY               : Single;  { Current Y position (inches) }
    CurrentTab             : Single;  { The value of the current tab }
    CurrentFontName        : String;  { Information about current font }
    CurrentFontSize        : Integer;
    CurrentFontStyle       : TFontStyles;
    TextMetrics            : TTextMetric;
    Header                 : Array[1..MaxHeaderLines] of THeaderRecord;
    Footer                 : Array[1..MaxFooterLines] of TFooterRecord;
    PageNumber             : TPageNumberRecord;
    HeaderCoordinates      : THeaderCoordinates;
    FooterCoordinates      : TFooterCoordinates;
    ColumnInformation      : Array[1..MaxColumns] of TColumnInformationRecord;
    TableColumns           : Integer;  { Number of columns in table }
    TableHeader            : TTableHeaderRecord; { Table header style }
    TableHorizOffset       : Single;   { Horizontal offset within column }
    TableVertOffset        : Single;   { Vertical offset within column }
    TableBodyVertOffset    : Single;   { Vertical offset within table body }
    TableStartY            : Single;   { Y position of top of table }
    TableStartBodyY        : Single;   { Y position of top of table body }

    procedure DoneDrawing;
    procedure PrintAll;
    procedure Preview;
    procedure OnCancelPrinting(Sender: TObject);
    function  CalculateLineHeight: Integer;
    function  InchesToPixelsHorizontal( Inches: Single ): Integer;
    function  InchesToPixelsVertical( Inches: Single ): Integer;
    function  PixelsToInchesHorizontal( Pixels: Integer ): Single;
    function  PixelsToInchesVertical( Pixels: Integer ): Single;
    function  GetLineHeightPixels: Word;
    function  GetLineHeightInches: Single;
    procedure CalculateMeasurements;

    procedure _WriteLine( X:Single; Y:Single; Text:String );
    procedure _DrawBox( XTop:Word; YTop:Word; XBottom:Word; YBottom:Word;
                        LineWidth:Word; Shading:Word );
    procedure _WriteTableHeaders;
    procedure _WriteHeaders;
    procedure _WriteFooters;
    procedure _WritePageNumber;

{ These functions not currently used }
(*
    procedure Abort;
    function  GetPrinterHandle: HDC;
    function  GetTextWidth( Text:String ): Integer;
    function  LinesToPixels( Line:Integer ): Integer;
    procedure GetPixelsPerInch( var X:Word; var Y:Word );
    procedure GetPixelsPerPage( var X:Word; var Y:Word );
    procedure GetGutter( var Top:Word; var Bottom:Word; var Left:Word;
                         var Right:Word );
    procedure SetLineWidth( Width:Word );
    function  GetLineWidth: Word;
    function  GetColumnsPerLine: Integer;
    function  GetFontName: String;
    function  GetFontSize: Word;
*)
  protected
    { Protected declarations }
    TotalPageWidthPixels: Integer;    { Full width of page in pixels - }
                                      { includes gutters }
    TotalPageHeightPixels: Integer;   { Full height of page in pixels - }
                                      { includes gutters }
    PageWidthPixels: Integer;         { Width of printable area in pixels }
    PageHeightPixels: Integer;        { Height of printable area in pixels }
    function  GetPageCount : Integer;
    procedure SetCurrentPage(Index : Integer);
    function  GetFont : TFont;
    procedure SetFont(Value : TFont);
    function  GetPen : TPen;
    procedure SetPen(Value : TPen);
    procedure DisplayPage(Page : Integer); {Called by preview form}

  public
    { Public declarations }
    constructor Create(Aowner: TComponent); override;
    destructor  Destroy; override;

    { Job Control Methods }
    procedure BeginJob;
    procedure EndJob;
    procedure FinishPage;
    function  NewPage : Integer;

    { Appearance Methods }
    procedure SetMargins( Top:Single; Bottom:Single; Left:Single;
                          Right:Single );
    procedure SetFontInformation( Name:String; Size:Word; Style: TFontStyles );
    procedure SaveCurrentFont;
    procedure RestoreCurrentFont;

    { Positioning Methods }
    procedure SetTab( Inches:Single );
    function  GetLinesPerPage: Integer;
    function  GetPageHeight: Single;
    function  GetPageWidth: Single;
    function  GetXPos: Single;
    procedure GoToXPos( XPosition: Single);
    function  GetYPos: Single;
    procedure GoToYPos( YPosition:Single );
    procedure GoToXY( XPosition: Single; YPosition: Single);
    procedure GoToPageTop;
    function  GetLinesLeft: Word;
    function  GetLinesInDetailArea: Word;
    procedure NextLine;
    procedure NewLines( Number:Word );
    function  IsPageEnd: Boolean;

    { Text Printing Methods }
    procedure Print(Text: String);
    procedure PrintLine(Text: String);
    procedure PrintLeft(Text: String);
    procedure PrintCenter(Text: String);
    procedure PrintRight(Text: String);

    { Table Printing Methods }
    procedure CreateTable( Columns:Word );
    procedure SetTableStyle(Value: TTableStyle);
    procedure SetTableHeaderStyle(FontName:String; FontSize:Word;
                FontStyle:TFontStyles; Boxed: Boolean; Shading: Word);
    procedure SetColumnDimensions(ColumnNumber:Word; XPosition:Single;
                Length:Single);
    procedure SetColumnHeaderText(ColumnNumber:Word; Line:Integer;
                Text:String);
    procedure SetColumnHeaderAlignment(ColumnNumber:Word; Alignment:TJustify);
    procedure BeginTable;
    procedure PrintColumnLeft( ColumnNumber:Word; Text:String );
    procedure PrintColumnRight( ColumnNumber:Word; Text:String );
    procedure PrintColumnCenter( ColumnNumber:Word; Text:String );
    procedure DrawColumnGridLine( ColumnNumber:Word; Style:TTableStyle; LineWidth:Word );
    procedure NextTableRow(IsLastRow: Boolean);
    procedure EndTable;

    { Line Printing Methods }
    procedure DrawLine( TopX:Single; TopY:Single; BottomX:Single;
                        BottomY:Single; LineWidth:Word );
    procedure DrawBox( XTop:Single; YTop:Single; XBottom:Single;
                       YBottom:Single; LineWidth:Word );
    procedure DrawBoxShaded( XTop:Single; YTop:Single; XBottom:Single;
                             YBottom:Single; LineWidth:Word; Shading:Word );

    { Picture Printing Methods }
    procedure DrawGraphic(aPicture: TPicture);
    procedure PositionGraphic(Alignment: TJustify; aPicture: TPicture);
    procedure StretchGraphic(Xtop,Ytop,Xbottom,Ybottom: Single;
                             aPicture: TPicture);
////    procedure StretchChart(Xtop, Ytop, Xbottom, Ybottom: Single;
////                           aChart: TChart);

    { Header/Footer Methods }
    procedure SetHeaderInformation( Line:Integer; YPosition: Single;
                                    Text:String; Alignment:TJustify;
                                    FontName:String; FontSize: Word;
                                    FontStyle: TFontStyles );
    procedure SetFooterInformation( Line:Integer; YPosition: Single;
                                    Text:String; Alignment:TJustify;
                                    FontName:String; FontSize: Word;
                                    FontStyle: TFontStyles );
    procedure SetHeaderDimensions( XTop:Single; YTop:Single; XBottom:Single;
                                   YBottom:Single; Boxed: Boolean;
                                   LineWidth:Word; Shading:Word );
    procedure SetFooterDimensions( XTop:Single; YTop:Single; XBottom:Single;
                                   YBottom:Single; Boxed: Boolean;
                                   LineWidth:Word; Shading:Word );
    procedure SetPageNumberInformation( YPosition:Single; Text:String;
                                        Alignment:TJustify; FontName:String;
                                        FontSize:Word; FontStyle:TFontStyles );

    { Printer Emulation Methods }
    procedure PrinterBeginDoc;
    procedure PrinterEndDoc;
    procedure PrinterPrintPage;
    procedure PrinterNewPage;

    { Internal Properties }
    property PageCount : Integer  read GetPageCount;
    property CurrentPage : Integer read FCurrentPage write SetCurrentPage;
    property Pen : TPen read GetPen write SetPen;
    property Font : TFont read GetFont write SetFont;

    procedure SetAutoPaging(Value: Boolean);
    procedure SetDestination(Value: TDestination);
    procedure SetOrientation(Value : TPrinterOrientation);
    procedure SetTitle(Value: String);
    procedure SetHeaders(Value: Boolean);
    procedure SetFooters(Value: Boolean);
    procedure SetPageNumbers(Value: Boolean);
    procedure SetShowProgress(Value: Boolean);

  end;

  procedure Register;


implementation

{$R *.DFM}
{$R 'Xprinter.dcr'}

uses
  System.Types;

procedure Register;
begin
  RegisterComponents('EPA', [TPrintControl]);
end;


{=============================================================}
{                    Methods for TPageList                    }
{=============================================================}

destructor TPageList.Destroy;
  Begin

  ClearPageList;
  inherited Destroy;

  End;


procedure TPageList.ClearPageList;

    { Free memory used for all pages }

  Var
  I: Integer;
  Page: TPrintPage;

  Begin
  For I := 1 to Count do
    Begin
    Page := GetPage(I);
    if Page <> nil then Page.Free;
    End;
  Clear;
  End;


function TPageList.GetPage(const Index: Integer): TPrintPage;

    { Retrieve pointer to page Index }

  Begin
  Result := TPrintPage(Items[Index-1]);
  End;


{=============================================================}
{                    Constructor & Destructor                 }
{=============================================================}

constructor TPrintControl.Create(AOwner: TComponent);
   Var
      I: Integer;

   Begin

   inherited Create(AOwner);
   fPrinter := Printers.Printer;
   fPage  := nil;
   fPages := TPageList.Create;
   fFont := TFont.Create;
   With fFont Do
     Begin
     Name := 'Courier New';
     Size := 10;
     Style := [];
     End;
   //SetFont(fFont);
   fPen := TPen.Create;

   fAutoPaging := True;
   fCancelPrinting := False;
   fCurrentPage := 0;
   fDestination := dPreview;
   fDoneDrawing := True;
   fOrientation := poPortrait;
   fTableStyle := [];
   fTitle := 'Untitled';
   fUnits := uInches;
   fHeaders := False;
   fFooters := False;
   fPageNumbers := False;
   fShowProgress := True;
   TableColumns := 0;

   For I := 1 To MaxHeaderLines Do Header[I].Text := '';
   HeaderCoordinates.Boxed := False;
   HeaderCoordinates.Shading := 0;

   For I := 1 To MaxFooterLines Do Footer[I].Text := '';
   FooterCoordinates.Boxed := False;
   FooterCoordinates.Shading := 0;

   SetPageNumberInformation(0.5,'',jRight,'Arial',10,[]);

   CalculateMeasurements;
   SetMargins(PixelsToInchesVertical( GutterTop ),
              PixelsToInchesVertical( GutterBottom ),
              PixelsToInchesHorizontal( GutterLeft ),
              PixelsToInchesHorizontal( GutterRight ));

   End;


destructor TPrintControl.Destroy;

   Begin
   If not fDoneDrawing Then DoneDrawing;
   fPages.Free;
   fFont.Free;
   fPen.Free;
   inherited Destroy;
   End;


{=============================================================}
{                       Property servers                      }
{=============================================================}

function TPrintControl.GetPageCount : Integer;

   { Get total number of pages rendered. }

   Begin
   Result := fPages.Count;
   End;


procedure TPrintControl.SetCurrentPage(Index : Integer);

   { Change the current page. }

   Begin
   If (Index <= PageCount) And (Index > 0) Then fCurrentPage := Index;
   End;


function TPrintControl.GetFont : TFont;

   { Retrieve the current font. }

   Begin
   Result := fCanvas.Font;
   End;


procedure TPrintControl.SetFont(Value : TFont);

   { Change the current font. }

   Begin
   With fCanvas Do
      Begin
      Font.Assign(Value);
      GetTextMetrics(Handle,TextMetrics );
      TableHorizOffset :=
         PixelsToInchesHorizontal(TextMetrics.tmAveCharWidth) / 2;
      TableVertOffset := GetLineHeightInches / 3;
      End;
   End;


function TPrintControl.GetPen : TPen;

    { Retrieve the current pen. }

    Begin
    Result := fCanvas.Pen;
    End;


procedure TPrintControl.SetPen(Value : TPen);

    { Change the current pen. }

   Begin
   fCanvas.Pen := Value;
   End;


procedure TPrintControl.SetFontInformation( Name:String; Size:Word;
                                           Style: TFontStyles );

   { Change the current font information }

   Begin

   If Not fDoneDrawing Then
     Begin
     fFont.Name := Name;
     fFont.Size := Size;
     fFont.Style := Style;
     SetFont(fFont);
     End;

   End;

(*
function TPrintControl.GetFontName: String;

   { Return the current font name }

   Begin
   Result := fFont.Name;
   End;


function TPrintControl.GetFontSize: Word;

   { Return the current font size }

   Begin
   Result := fFont.Size;
   End;
*)

procedure TPrintControl.SaveCurrentFont;

   { Save the current font information. }

   Begin
   CurrentFontName :=  fFont.Name;
   CurrentFontSize :=  fFont.Size;
   CurrentFontStyle := fFont.Style;
   End;


procedure TPrintControl.RestoreCurrentFont;

   { Restore previously saved font information. }

   Begin
   SetFontInformation( CurrentFontName,CurrentFontSize,CurrentFontStyle );
   End;


procedure TPrintControl.SetMargins( Top:Single; Bottom:Single; Left:Single;
                               Right:Single );

   { Set the top, bottom, left and right margins in inches }

   Var
     GLI, GRI, GTI, GBI: Single;

   Begin
   { Compute gutter widths in inches }
   GTI := PixelsToInchesVertical( GutterTop );
   GBI := PixelsToInchesVertical( GutterBottom );
   GLI := PixelsToInchesHorizontal( GutterLeft );
   GRI := PixelsToInchesHorizontal( GutterRight );

   { If the sum of the left and right margins exceeds the width of the page,
     set the left margin to the value of 'GutterLeft' and set the right
     margin to the value of 'GutterRight' }
   If ( Left + Right >= TotalPageWidthInches ) Then
      Begin
      Left := GLI;
      Right := GRI;
      End;
   If ( Left <= 0 ) Then   Left := GLI;
   If ( Right <= 0 ) Then  Right := GRI;

   { If the sum of the top and bottom margins exceeds the height of the
     page, set the top margin to the value of 'GutterTop' and set the
     bottom margin to the value of 'GutterBottom' }
   If ( Top + Bottom >= TotalPageHeightInches ) Then
      Begin
      Top := GTI;
      Bottom := GBI;
      End;
   If ( Top <= 0 ) Then    Top := GTI;
   If ( Bottom <= 0 ) Then Bottom := GBI;

   { Convert everything to pixels }
   TopMargin := InchesToPixelsVertical( Top );
   BottomMargin := InchesToPixelsVertical( Bottom );
   LeftMargin := InchesToPixelsHorizontal( Left );
   RightMargin := InchesToPixelsHorizontal( Right );

   { Make sure margins fall within printable area. }
   If ( TopMargin < GutterTop ) Then TopMargin := GutterTop;
   If ( BottomMargin < GutterBottom ) Then BottomMargin := GutterBottom;
   If ( LeftMargin < GutterLeft ) Then LeftMargin := GutterLeft;
   If ( RightMargin < GutterRight ) Then RightMargin := GutterRight;

   { Re-set the detail area }
   DetailTop := Top;
   DetailBottom := TotalPageHeightInches - Bottom;
   CurrentY := Top;
   End;


procedure TPrintControl.SetAutoPaging( Value: Boolean );

   { Change the autopaging setting. }

   Begin
   If fAutoPaging <> Value Then fAutoPaging := Value;
   End;


procedure TPrintControl.SetDestination( Value: TDestination);

   { Change the destination for the print-out. }

   Begin
   If fDestination <> Value Then fDestination := Value;
   End;


procedure TPrintControl.SetOrientation( Value: TPrinterOrientation );

   { Change the page orientation. }

   Begin
   If fOrientation <> Value Then
      begin
      fOrientation := Value;
      CalculateMeasurements;
      end;
   End;


procedure TPrintControl.SetTableStyle( Value: TTableStyle);

   { Change the style used for tabular output. }

   Begin
   If fTableStyle <> Value Then fTableStyle := Value;
   End;


procedure TPrintControl.SetTitle( Value: String );

   { Change the title assigned to the print job. }

   Begin
   If fTitle <> Value Then fTitle := Value;
   End;


procedure TPrintControl.SetHeaderInformation( Line:Integer;
                                         YPosition:Single;
                                         Text:String;
                                         Alignment: TJustify;
                                         FontName:String;
                                         FontSize: Word;
                                         FontStyle: TFontStyles );

   { Change the current header information.}

   Begin
   If ( Line < 1 ) Or ( Line > MaxHeaderLines ) Then Exit;
   Header[Line].Text := Text;
   Header[Line].YPosition := YPosition;
   Header[Line].Alignment := Alignment;
   Header[Line].FontName := FontName;
   Header[Line].FontSize := FontSize;
   Header[Line].FontStyle := FontStyle;
   End;


procedure TPrintControl.SetHeaderDimensions( XTop:Single; YTop:Single;
                                        XBottom:Single; YBottom:Single;
                                        Boxed: Boolean; LineWidth:Word;
                                        Shading:Word );

   { Change the current dimensions of the header box. }

   Begin
   HeaderCoordinates.XTop := XTop;
   HeaderCoordinates.XBottom := XBottom;
   HeaderCoordinates.YTop := YTop;
   HeaderCoordinates.YBottom := YBottom;
   HeaderCoordinates.Boxed := Boxed;
   HeaderCoordinates.LineWidth := LineWidth;
   HeaderCoordinates.Shading := Shading;
   End;

procedure TPrintControl.SetFooterInformation( Line:Integer;
                                         YPosition:Single;
                                         Text:String;
                                         Alignment:TJustify;
                                         FontName:String;
                                         FontSize:Word;
                                         FontStyle: TFontStyles );

   { Change the current footer information.}

   Begin
   If ( Line < 1 ) Or ( Line > MaxFooterLines ) Then Exit;
   Footer[Line].Text := Text;
   Footer[Line].YPosition := YPosition;
   Footer[Line].Alignment := Alignment;
   Footer[Line].FontName := FontName;
   Footer[Line].FontSize := FontSize;
   Footer[Line].FontStyle := FontStyle;
   End;

procedure TPrintControl.SetFooterDimensions( XTop:Single; YTop:Single;
                                        XBottom:Single; YBottom:Single;
                                        Boxed: Boolean; LineWidth:Word;
                                        Shading:Word );

   { Change the current dimensions of the footer box. }

   Begin
   FooterCoordinates.XTop := XTop;
   FooterCoordinates.XBottom := XBottom;
   FooterCoordinates.YTop := YTop;
   FooterCoordinates.YBottom := YBottom;
   FooterCoordinates.Boxed := Boxed;
   FooterCoordinates.LineWidth := LineWidth;
   FooterCoordinates.Shading := Shading;
   End;


procedure TPrintControl.SetPageNumberInformation( YPosition:Single;
                                             Text:String;
                                             Alignment:TJustify;
                                             FontName:String;
                                             FontSize:Word;
                                             FontStyle:TFontStyles );

   { Change the current page numbering settings. }

   Begin
   PageNumber.Text := Text;
   PageNumber.YPosition := YPosition;
   PageNumber.Alignment := Alignment;
   PageNumber.FontName := FontName;
   PageNumber.FontSize := FontSize;
   PageNumber.FontStyle := FontStyle;
   End;


procedure TPrintControl.SetTableHeaderStyle(FontName:String;
                                            FontSize:Word;
                                            FontStyle:TFontStyles;
                                            Boxed: Boolean;
                                            Shading: Word);

   { Change the current style settings for table column headings. }

   Begin
   TableHeader.Fontname := FontName;
   TableHeader.FontSize := FontSize;
   TableHeader.FontStyle := FontStyle;
   TableHeader.Boxed := Boxed;
   TableHeader.Shading := Shading;
   End;


procedure TPrintControl.SetColumnDimensions( ColumnNumber: Word;
                                             Xposition: Single;
                                             Length: Single);

   { Change the dimensions of table columns. }

   Begin
   If not (ColumnNumber in [1..MaxColumns]) Then Exit;
   ColumnInformation[ColumnNumber].Xposition := Xposition;
   ColumnInformation[ColumnNumber].Length := Length;
   End;


procedure TPrintControl.SetColumnHeaderAlignment( ColumnNumber: Word;
                                                  Alignment: TJustify );

   { Change the current choice of text alignment for table columns. }

   Begin
   If not (ColumnNumber in [1..MaxColumns]) Then Exit;
   ColumnInformation[ColumnNumber].Alignment := Alignment;
   End;


procedure TPrintControl.SetColumnHeaderText( ColumnNumber: Word;
                                        Line: Integer; Text: String);

   { Set the text for a table column heading. }

   Begin
   If not (ColumnNumber in [1..MaxColumns]) Then Exit;
   If (Line < 1) Or (Line > MaxHeaderLines) Then Exit;
   ColumnInformation[ColumnNumber].Text[Line] := Text;
   End;


procedure TPrintControl.SetHeaders(Value: Boolean);

   { Change the option of displaying page headers. }

   Begin
   if fHeaders <> Value then fHeaders := Value;
   End;


procedure TPrintControl.SetFooters(Value: Boolean);

   { Change the option of displaying page footers. }

   Begin
   if fFooters <> Value then fFooters := Value;
   End;


procedure TPrintControl.SetPageNumbers(Value: Boolean);

   { Change the option of displaying page numbers. }

   Begin
   if fPageNumbers <> Value then fPageNumbers := Value;
   End;


procedure TPrintControl.SetShowProgress(Value: Boolean);

   { Change the option of displaying a print progress message. }

   Begin
   if fShowProgress <> Value then fShowProgress := Value;
   End;


procedure TPrintControl.SetTab( Inches:Single );

   { Change the value of the current tab width in effect. }

   Begin
   CurrentTab := Inches;
   End;

(*
function TPrintControl.GetPrinterHandle: HDC;

    { Return the handle of the current printer. }

     Begin
     try
        Result := fPrinter.Handle;
     except
        on EPrinter do Result := 0;
     end;
     End;
*)

{=============================================================}
{                       System Functions                      }
{=============================================================}

procedure TPrintControl.BeginJob;

  { Begins processing a new print job. }

  Begin

  If not fDoneDrawing Then DoneDrawing;
  fPages.ClearPageList;
  fPage := nil;
  fCurrentPage := 0;
  fDoneDrawing := False;
  CalculateMeasurements;
  SetMargins(PixelsToInchesVertical(TopMargin),
             PixelsToInchesVertical(BottomMargin),
             PixelsToInchesHorizontal(LeftMargin),
             PixelsToInchesHorizontal(RightMargin));
  CurrentTab := 0.0;
  CurrentX := 0.0;
  CurrentY := 0.0;
  NewPage;

  End;


procedure TPrintControl.EndJob;

   { Completes processing the current print job. }

   Begin
   If not fDoneDrawing Then DoneDrawing;
   If fDestination = dPreview Then Preview;
   If fDestination = dPrinter Then PrintAll;
   fPages.ClearPageList;
   fPage := nil;
   End;


function TPrintControl.NewPage : Integer;

  { Creates a new page for the print-out. }

  Begin
  if not fDoneDrawing then
    begin
    FinishPage;
    fCanvas.Free;
    fPage := TPrintPage.Create;
    fPage.Width := PageWidthPixels;
    fPage.Height := PageHeightPixels;
    fCanvas := TPrintCanvas.Create(fPage, 0);
//    fCanvas := TPrintCanvas.Create(fPage, GetPrinterHandle);

// NOTE: Using GetPrinterHandle instead of 0 in the Create above
// caused text not to show up when metafiles pictures were drawn
// using the xxxGraphic() procedures.

    fCanvas.Brush.Style := bsClear;
    fCanvas.Font.PixelsPerInch := PixelsPerInchVertical;
    SetFont(fFont);
    fPages.Add(fPage);
    CurrentY := DetailTop;
    end;
  Result := fPages.Count;
  fCurrentPage := Result;
  End;


procedure TPrintControl.FinishPage;

  { Completes the rendering of a page. }

  Begin
  if (not fDoneDrawing) and (fCanvas <> nil) then
    Begin
      if fHeaders then _WriteHeaders;
      if fFooters then _WriteFooters;
      if fPageNumbers then _WritePageNumber;
    End;
  End;


procedure TPrintControl.DoneDrawing;

  { Finishes up the rendering of the print-out. }

  Begin
  if not fDoneDrawing then
     begin
     FinishPage;
     fCanvas.Free;
     fCanvas := nil;
     fDoneDrawing := True;
     end;
  End;


procedure TPrintControl.PrintAll;

   { Sends all rendered pages to the printer. }

   Var
   i : Integer;

   Begin

   if not fDoneDrawing then DoneDrawing;
   if (PageCount > 0) and (Printer.Printers.Count > 0) then

      try

      { Initialize printer. }
      Printer.Title := fTitle;
      Printer.BeginDoc;
      fCancelPrinting := False;

      { Create printing progress dialog form. }
      if fShowProgress and not Assigned(fProgressForm) then
         fProgressForm := TProgressForm.Create(Application);

         try
            { Show Progress form if called for. }
            if fShowProgress and Assigned(fProgressForm) then
               begin
               fProgressForm.Button1.OnClick := OnCancelPrinting;
               fProgressForm.Show;
               end;

            { Process each rendered page. }
            i := 1;
            while not fCancelPrinting and (i <= PageCount) do

               begin

               { Display current page number in progress dialog form. }
               if fShowProgress and Assigned(fProgressForm) then with fProgressForm do
                  begin
                  Application.ProcessMessages;
                  Label1.Caption := Format('Printing page %d of %d',[i, PageCount]);
                  Update;
                  end;

               { Draw the page on the printer's canvas. }
               if i > 1 then Printer.NewPage;
               Printer.Canvas.Draw(0, 0, fPages.GetPage(i));
               Inc(i);

               end;

         finally

            { Free the Progress form. }
            if Assigned(fProgressForm) then
            begin
              fProgressForm.Release;
              fProgressForm := nil;
            end;

         end;


      finally
         { End the print job. }
         if not Printer.Aborted then Printer.EndDoc;
         if fCancelPrinting then
            begin
            MessageDlg('Printing aborted', mtInformation, [mbOK], 0);
            fCancelPrinting := False;
            end;

      end;
   End;


procedure TPrintControl.Preview;

   { Activates the Preview form to display individual pages. }

   Begin
   if not fDoneDrawing then DoneDrawing;
   fPreviewForm := TPreviewForm.Create(Application);

   try
      fPreviewForm.PrintControl := Self;
      fPreviewForm.FullPageBtnClick(Self);
      fPreviewForm.ShowModal;
   finally
      fPreviewForm.Free;
   end;

   End;

(*
procedure TPrintControl.Abort;

   { Aborts the current print job. }

   Begin
   try
     Printer.Abort;
   except
     On EPrinter do
   end;
   End;
*)

procedure TPrintControl.OnCancelPrinting(Sender: TObject);

   Begin
   fProgressForm.Hide;
   fCancelPrinting := True;
   End;


procedure TPrintControl.DisplayPage(Page : Integer);

   { Displays a page on the Preview form. }

   Var
   Scx, Scy: Single;
   r : TRect;
   i : Integer;

   Begin
   if (Page > 0) AND (Page <= PageCount) then

      begin
      { Set size of page display on preview form }
      Scx := fPreviewForm.PaintBox1.Width / TotalPageWidthPixels;
      Scy := fPreviewForm.PaintBox1.Height / TotalPageHeightPixels;
      fPreviewForm.PaintBox1.Canvas.Rectangle(
         0, 0, fPreviewForm.PaintBox1.Width,fPreviewForm.PaintBox1.Height);
      fPreviewForm.PaintBox1.Canvas.FillRect(
         Rect(1, 1, fPreviewForm.PaintBox1.Width - 2,
              fPreviewForm.PaintBox1.Height - 2));

      { Find size of drawing area }
      r.Left := Trunc(GutterLeft * Scx);
      r.Top := Trunc(GutterTop * Scy);
      r.Right := r.Left + Trunc(PageWidthPixels * Scx);
      r.Bottom := r.Top + Trunc(PageHeightPixels * Scy);

      { Draw grid if called for }
      if fPreviewForm.ShwGrdBtn.Down then
         begin
         with fPreviewForm.PaintBox1.Canvas do
            begin
            Pen.Style := psDot;
            Pen.Color := clSilver;
            Rectangle(r.Left, r.Top, r.Right, r.Bottom);
            for i := 1 to TotalPageWidthPixels div PixelsPerInchHorizontal do
               begin
               MoveTo(Trunc(i * PixelsPerInchHorizontal * Scx), 0);
               LineTo(Trunc(i * PixelsPerInchHorizontal * Scx),
                      Trunc(TotalPageHeightPixels * Scy));
               end;
            for i := 1 to TotalPageHeightPixels div PixelsPerInchVertical do
               begin
               MoveTo(0, Trunc(i * PixelsPerInchVertical * Scy));
               LineTo(Trunc(TotalPageWidthPixels * Scx),
                      Trunc(i * PixelsPerInchVertical * Scy));
               end;
            Pen.Style := psSolid;
            Pen.Color := clBlack;
            end;

         end;

        { Draw page on preview form }
        fPreviewForm.PaintBox1.Canvas.StretchDraw(r, fPages.GetPage(Page));
        fPreviewForm.Panel2.Caption := Format('Page %d of %d',
                                              [Page, PageCount]);

        fPreviewForm.PageDisplaying := Page;
        if Page = 1 then
           fPreviewForm.PrevPageBtn.Enabled := False
        else
           fPreviewForm.PrevPageBtn.Enabled := True;

        if PageCount > Page then
           fPreviewForm.NextPageBtn.Enabled := True
        else
           fPreviewForm.NextPageBtn.Enabled := False;
      end;
   End;

{=============================================================}
{                      Emulation Methods                      }
{ Use as follows with Destination set to dPrinter:            }
{  PrinterBeginDoc;                                           }
{  (Use regular TPrintControl methods to write to page)       }
{  PrinterPrintPage;                                          }
{  PrinterNewPage;                                            }
{  (Print to another page, etc.)                              }
{  PrinterEndDoc;                                             }
{=============================================================}

procedure TPrintControl.PrinterBeginDoc;
begin
  try
  fAutoPaging := False;
  Printer.Title := ftitle;
  Printer.BeginDoc;
  BeginJob;
  finally
  end;
end;

procedure TPrintControl.PrinterEndDoc;
begin
  DoneDrawing;
  try
  if not Printer.Aborted then Printer.EndDoc;
  finally
  end;
  fPages.ClearPageList;
end;

procedure TPrintControl.PrinterPrintPage;
begin
  DoneDrawing;
  try
    Printer.Canvas.Draw(0,0,fPages.GetPage(1));
  finally
  end;
  BeginJob;
end;

procedure TPrintControl.PrinterNewPage;
begin
  try
  Printer.NewPage;
  finally
  end;
end;


{=============================================================}
{                      Measurement Routines                   }
{=============================================================}

procedure TPrintControl.CalculateMeasurements;

   { Calculate some necessary measurements. }

   Begin

   try
   { Set Printer properties }
   Printer.Orientation := fOrientation;
   Printer.Title := fTitle;

   { Calculate the number of pixels per inch vertical and horizontal.
     'GetDeviceCaps' is a Windows API call. }
   PixelsPerInchVertical := GetDeviceCaps( Printer.Handle,LOGPIXELSY );
   PixelsPerInchHorizontal := GetDeviceCaps( Printer.Handle,LOGPIXELSX );

   { Get the physical size of the page. }
   TotalPageWidthPixels := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
   TotalPageHeightPixels := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);

   { Get the printable size of the page. }
   PageWidthPixels := GetDeviceCaps(Printer.Handle, HORZRES);
   PageHeightPixels := GetDeviceCaps(Printer.Handle, VERTRES);

   { Get the gutters on the left, top, right, and bottom. }
   GutterLeft := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
   GutterTop  := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);

   except

   { Use default measurements if there's an exception thrown. }
     on EPrinter do
       begin
       fOrientation := poPortrait;
       PixelsPerInchVertical := DefaultDPI;
       PixelsPerInchHorizontal := DefaultDPI;
       TotalPageWidthPixels := Trunc(8.5*PixelsPerInchHorizontal);
       TotalPageHeightPixels := Trunc(11*PixelsPerInchVertical);
       PageWidthPixels := TotalPageWidthPixels;
       PageHeightPixels := TotalPageHeightPixels;
       GutterLeft := 0;
       GutterTop := 0;
       end;

   end;

   { Complete calculation of measurements }
   GutterRight := TotalPageWidthPixels - PageWidthPixels - GutterLeft;
   GutterBottom := TotalPageHeightPixels - PageHeightPixels - GutterTop;
   TotalPageWidthInches := TotalPageWidthPixels / PixelsPerInchHorizontal;
   TotalPageHeightInches := TotalPageHeightPixels / PixelsPerInchVertical;
   UnitLineWidth := PixelsPerInchVertical div DefaultDPI;
   if UnitLineWidth = 0 then UnitLineWidth := 1;
   
   { Adjust margins to fit within printable area. }
   If ( TopMargin < GutterTop )       Then TopMargin := GutterTop;
   If ( BottomMargin < GutterBottom ) Then BottomMargin := GutterBottom;
   If ( LeftMargin < GutterLeft )     Then LeftMargin := GutterLeft;
   If ( RightMargin < GutterRight )   Then RightMargin := GutterRight;
   End;


function TPrintControl.CalculateLineHeight: Integer;

   { Calculate the height of a line plus the normal amount of space between
     each line }

   Begin
   Result := TextMetrics.tmHeight + TextMetrics.tmExternalLeading;
   End;

(*
function TPrintControl.GetTextWidth( Text:String ): Integer;

   { Return the width of the text contained in 'Text' in pixels }

   Begin
   Result := fCanvas.TextWidth( Text );
   End;
*)

function TPrintControl.GetLineHeightPixels: Word;

   { Return the height of the current line in pixels. }

   Begin
   Result := CalculateLineHeight;
   End;


function TPrintControl.GetLineHeightInches: Single;

   { Return the height of the current line in inches. }

   Begin
   Result := PixelsToInchesVertical( GetLineHeightPixels );
   End;


function TPrintControl.GetLinesPerPage: Integer;

   { Return the number of lines on the entire page }

   Begin
   Result := (TotalPageHeightPixels - GutterTop - GutterBottom) Div
             CalculateLineHeight;
   End;


function TPrintControl.GetLinesInDetailArea: Word;

   { Return the number of lines in the detail area }

   Begin
   Result := InchesToPixelsVertical( DetailBottom - DetailTop ) Div
             CalculateLineHeight;
   End;

(*
procedure TPrintControl.GetPixelsPerInch( var X:Word; var Y:Word );

   { Get the number of pixels per inch in the horizontal and vertical. }

   Begin
   X := PixelsPerInchHorizontal;
   Y := PixelsPerInchVertical;
   End;


procedure TPrintControl.GetPixelsPerPage( var X:Word; var Y:Word );

   { Get the number of pixels in the horizontal and vertical
     directions within the printable area of the page. }

   Begin
   X := TotalPageWidthPixels - GutterLeft - GutterRight;
   Y := TotalPageHeightPixels - GutterTop - GutterBottom;
   End;


procedure TPrintControl.GetGutter( var Top:Word; var Bottom:Word;
                                  var Left:Word; var Right:Word );

   { Get the gutters (printing offsets) currently in effect. }

   Begin
   Top := GutterTop;
   Bottom := GutterBottom;
   Left := GutterLeft;
   Right := GutterRight;
   End;


function TPrintControl.GetColumnsPerLine: Integer;

   { How many columns are there in a Line? }

   Var
   Pixels: Integer;

   Begin
   Pixels := TotalPageWidthPixels - GutterLeft - GutterRight;
   Result := Pixels Div FCanvas.TextWidth( 'M' );
   End;


procedure TPrintControl.SetLineWidth( Width:Word );

   { Changes the current line width. }

   Begin
   fCanvas.Pen.Width := Width * UnitLineWidth;
   End;


function TPrintControl.GetLineWidth: Word;

   { Returns the current line width. }

   Begin
   Result := fCanvas.Pen.Width div UnitLineWidth;
   if Result = 0 then Result := 1;
   End;
*)

function TPrintControl.GetPageHeight: Single;

   { Returns the current paper height in inches. }

   Begin
   Result := TotalPageHeightInches;
   End;


function TPrintControl.GetPageWidth: Single;

   { Returns the current paper width in inches. }

   Begin
   Result := TotalPageWidthInches;
   End;


{=============================================================}
{                     Positioning Routines                    }
{=============================================================}

procedure TPrintControl.GoToXPos( XPosition:Single );

   { Moves to horizontal position XPosition in inches on the page. }

   Begin
   CurrentX := XPosition;
   End;


function TPrintControl.GetXPos: Single;

   { Returns the current horizontal position on the page in inches. }

   Begin
   Result := CurrentX;
   End;


procedure TPrintControl.GoToYPos( YPosition:Single );

   { Moves to vertical position YPosition in inches on the page. }

   Begin
   CurrentY := YPosition;
   End;


function TPrintControl.GetYPos: Single;

   { Returns the current vertical position on the page in inches. }

   Begin
   Result := CurrentY;
   End;


procedure TPrintControl.GoToXY(XPosition: Single; YPosition: Single);

   { Moves to horizontal location Xposition and vertical location
     YPosition (in inches) on the page.}

   Begin
   CurrentX := XPosition;
   CurrentY := YPosition;
   End;


procedure TPrintControl.NextLine;

   { Moves to the next line on the page. }

   Begin
   CurrentY := CurrentY + GetLineHeightInches;
   If fAutoPaging And IsPageEnd then NewPage;
   End;


function TPrintControl.GetLinesLeft: Word;

   { Returns the number of lines left in the detail area }

   Begin
   Result := Trunc((DetailBottom - CurrentY) / GetLineHeightInches);
   End;


procedure TPrintControl.GoToPageTop;

   { Moves to the top of the detail area on the page.}

   Begin
   CurrentY := DetailTop;
   End;


procedure TPrintControl.NewLines( Number:Word );

   { Generates the number of line feeds represented in 'Number' }

   Var
   I: Word;

   Begin
   For I := 1 To Number Do
     NextLine;
   End;


function TPrintControl.IsPageEnd: Boolean;

   { Determines if at last line of current page. }

   Begin
   If CurrentY + GetLineHeightInches - 0.05 > DetailBottom Then
      Result := True
   Else
      Result := False;
   End;


{=============================================================}
{                      Conversion Routines                    }
{=============================================================}

function TPrintControl.InchesToPixelsHorizontal( Inches: Single ): Integer;

   { Convert the horizontal inches represented in 'Inches' to pixels }

   Begin
   Result := Trunc(Inches * PixelsPerInchHorizontal);
   End;


function TPrintControl.InchesToPixelsVertical( Inches: Single ): Integer;

   { Convert the vertical inches represented in 'Inches' to pixels }

   Begin
   Result := Trunc(Inches * PixelsPerInchVertical);
   End;


function TPrintControl.PixelsToInchesHorizontal( Pixels: Integer ): Single;

   { Convert the horizontal pixels represented in 'Pixels' to inches }

   Begin
   Result := Pixels / PixelsPerInchHorizontal;
   End;


function TPrintControl.PixelsToInchesVertical( Pixels: Integer ): Single;

   { Convert the vertical pixels represented in 'Pixels' to inches }

   Begin
   Result := Pixels / PixelsPerInchVertical;
   End;

(*
function TPrintControl.LinesToPixels( Line:Integer ): Integer;

   { Calculate the number of vertical pixels in 'Line' }

   Begin
   If ( Line <= 0 ) Then Line := 1;
   Result := (Line-1) * CalculateLineHeight;
   End;
*)

{=============================================================}
{                    Canvas Drawing Routines                  }
{=============================================================}

procedure TPrintControl.Print(Text: String);

   { Prints a text string starting at current X,Y position. }

   Begin
   _WriteLine(CurrentX, CurrentY, Text);
   End;


procedure TPrintControl.PrintLine(Text: String);

   { Prints a text string starting at left margin and current
     Y position and moves Y position down one line.}

   Begin
   CurrentX := PixelsToInchesHorizontal(LeftMargin);
   Print(Text);
   NextLine;
   End;


procedure TPrintControl.PrintLeft(Text: String);

   { Prints a text string starting at left margin }

   Begin
   _WriteLine(PixelsToInchesHorizontal(LeftMargin), CurrentY, Text);
   End;


procedure TPrintControl.PrintRight(Text: String);

   { Prints a text string ending at right margin }

   Var
   PixelLength: Word;
   StartPixel: Word;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;

   { How many pixels does the text in 'Text' require? }
   PixelLength := fCanvas.TextWidth( Text );

   { Calculate where printing should start }
   StartPixel := (TotalPageWidthPixels-RightMargin) - PixelLength;

   { Temporarily disable tabs before printing }
   SetTab( 0.0 );
   _WriteLine( PixelsToInchesHorizontal(StartPixel),CurrentY,Text );
   SetTab( CurrentTab );
   End;


procedure TPrintControl.PrintCenter( Text:String );

   { Prints a text string centered between margins}

   Var
   PixelLength: Integer;
   StartPixel: Integer;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;

   { How many pixels does the text in 'Text' require? }
   PixelLength := fCanvas.TextWidth( Text );

   { Calculate where printing should start }
   StartPixel := ((LeftMargin+(TotalPageWidthPixels-RightMargin)) Div 2) -
                 (PixelLength Div 2);

   { Temporarily disable tabs before printing }
   SetTab( 0.0 );
   _WriteLine( PixelsToInchesHorizontal(StartPixel),CurrentY,Text );
   SetTab( CurrentTab );
   End;


procedure TPrintControl._WriteLine( X:Single; Y:Single; Text:String );

   { Low-level routine to write some text at location X inches from
     the left and Y inches from the top of the page. }

   Var
   XPixels: Integer;
   YPixels: Integer;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;

   { How many pixels are there in the inches represented by 'X'? }
   XPixels := InchesToPixelsHorizontal( X );
   If ( XPixels < GutterLeft ) Then XPixels := GutterLeft;

   { If there is a tab set, increase 'XPixels' by the amount of the tab }
   If ( CurrentTab > 0.0 ) Then
      Inc( XPixels,InchesToPixelsHorizontal(CurrentTab) );

   { How many pixels are there in the inches represented by 'Y'? }
   YPixels := InchesToPixelsVertical( Y );
   If ( YPixels < GutterTop ) Then YPixels := GutterTop;

   With fCanvas Do
      Begin
      SetBkMode(Handle, TRANSPARENT);
      TextOut( XPixels-GutterLeft,YPixels-GutterTop,Text );
      End;
   End;


procedure TPrintControl.DrawLine( TopX:Single; TopY:Single; BottomX:Single;
                                 BottomY:Single; LineWidth:Word );

   { Draw a line beginning at a particular X,Y coordinate and ending at a
     particular X,Y coordinate (in inches). }

   var
      TopXPixels, BottomXPixels, TopYPixels, BottomYPixels: Integer;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;

   TopXPixels := InchesToPixelsHorizontal( TopX );
   BottomXPixels := InchesToPixelsHorizontal( BottomX );
   TopYPixels := InchesToPixelsVertical( TopY );
   BottomYPixels := InchesToPixelsVertical( BottomY );

   Dec( TopXPixels,GutterLeft );
   Dec( BottomXPixels,GutterLeft );
   Dec( TopYPixels,GutterTop );
   Dec( BottomYPixels,GutterTop );

   with fCanvas do
      begin
      Pen.Width := LineWidth*UnitLineWidth;
      MoveTo( TopXPixels,TopYPixels );
      LineTo( BottomXPixels,BottomYPixels );
      end;
   End;


procedure TPrintControl._DrawBox( XTop:Word; YTop:Word; XBottom:Word;
                                 YBottom:Word; LineWidth:Word; Shading:Word );

   { The low level routine which actually draws the box and shades it as
     desired. The parameters are in pixels and not inches. }

   Begin
   if (fCurrentPage > 0) and (not fDoneDrawing) then

      with fCanvas do
         begin
         Pen.Width := LineWidth*UnitLineWidth;
         if Shading <> 255 then
           begin
           Brush.Color := RGB( Shading,Shading,Shading );
           Rectangle( XTop,YTop,XBottom,YBottom );
           end
         else PolyLine([Point(XTop,YTop), Point(XTop,YBottom),
                        Point(XBottom,YBottom), Point(XBottom,YTop),
                        Point(XTop,YTop)]);
         end;
   End;


procedure TPrintControl.DrawBox( XTop:Single; YTop:Single; XBottom:Single;
                                YBottom:Single; LineWidth:Word );

   { Draw a box at the X,Y coordinates (in inches) passed in the parameters }

   Var
   BLinePixels,BColPixels,ELinePixels,EColPixels: Integer;

   Begin
   BLinePixels := InchesToPixelsVertical( YTop ) - GutterTop;
   ELinePixels := InchesToPixelsVertical( YBottom ) - GutterTop;


   BColPixels := InchesToPixelsHorizontal( XTop ) - GutterLeft;
   EColPixels := InchesToPixelsHorizontal( XBottom ) - GutterLeft;

   _DrawBox( BColPixels,BLinePixels,EColPixels,ELinePixels,LineWidth,255 );
   End;


procedure TPrintControl.DrawBoxShaded( XTop:Single; YTop:Single;
                                      XBottom:Single; YBottom:Single;
                                      LineWidth:Word; Shading:Word );

   { Draw a shaded box at the X,Y coordinates (in inches) passed in the
     parameters }

   Var
   BLinePixels,BColPixels,ELinePixels,EColPixels: Integer;

   Begin
   BLinePixels := InchesToPixelsVertical( YTop ) - GutterTop;
   ELinePixels := InchesToPixelsVertical( YBottom ) - GutterTop;

   BColPixels := InchesToPixelsHorizontal( XTop ) - GutterLeft;
   EColPixels := InchesToPixelsHorizontal( XBottom ) - GutterLeft;

   _DrawBox(BColPixels,BLinePixels,EColPixels,ELinePixels,LineWidth,Shading);
   End;


procedure TPrintControl.DrawGraphic(aPicture: TPicture);

   { Draw a picture with upper left corner at the current X,Y location. }

   Var
   Xpixels, Ypixels: Integer;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;
   Xpixels := InchesToPixelsHorizontal(CurrentX) - GutterLeft;
   Ypixels := InchesToPixelsVertical(CurrentY) - GutterTop;
   fCanvas.Draw(Xpixels,Ypixels,aPicture.Graphic);
   End;


procedure TPrintControl.PositionGraphic(Alignment: TJustify; aPicture: TPicture);

   { Draws a picture horizontally aligned to the left/right margins. }

   Var
   Xpixels, Ypixels, PixelLength: Integer;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;
   PixelLength := aPicture.Width;
   Xpixels := LeftMargin;
   If Alignment = jCenter Then
     Xpixels := ((LeftMargin+(TotalPageWidthPixels-RightMargin)) Div 2) -
                 (PixelLength Div 2);
   If Alignment = jRight Then
     XPixels := (TotalPageWidthPixels-RightMargin) - PixelLength;
   Xpixels := Xpixels - GutterLeft;
   Ypixels := InchesToPixelsVertical(CurrentY) - GutterTop;
   fCanvas.Draw(Xpixels,Ypixels,aPicture.Graphic);
   End;


procedure TPrintControl.StretchGraphic(Xtop,Ytop,Xbottom,Ybottom: Single;
                                  aPicture: TPicture);

   { Draws a picture stretched to fit within coordinates passed in
     as parameters (in absolute inches). }

   Var
   x1,y1,x2,y2: Integer;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;
   x1 := InchesToPixelsHorizontal(Xtop) - GutterLeft;
   y1 := InchesToPixelsVertical(Ytop) - GutterTop;
   x2 := InchesToPixelsHorizontal(Xbottom) - GutterLeft;
   y2 := InchesToPixelsVertical(Ybottom) - GutterTop;
   fCanvas.StretchDraw(Rect(x1,y1,x2,y2),aPicture.Graphic);
   End;

{
procedure TPrintControl.StretchChart(Xtop, Ytop, Xbottom, Ybottom: Single;
             aChart: TChart);
}
   { Draws a TeeChart chart stretched to fit within the coordinates passed
     in as parameters (in absolute inches). }
{
   Var
   L,R,T,B: Integer;

   Begin
   L := InchesToPixelsHorizontal(Xtop) - GutterLeft;
   R := InchesToPixelsHorizontal(Xbottom) - GutterLeft;
   T := InchesToPixelsVertical(Ytop) - GutterTop;
   B := InchesToPixelsVertical(Ybottom) - GutterTop;
   aChart.PrintPartialCanvas(fCanvas,Rect(L,T,R,B));
   End;
}

procedure TPrintControl._WriteHeaders;

   { If any headers are defined, write them }

   var
      I: Integer;

   Begin
      SaveCurrentFont;

      { Does the user desire a box around the header? }
      If ( HeaderCoordinates.Boxed = True ) Then
         begin
         If ( HeaderCoordinates.Shading > 0 ) Then
            DrawBoxShaded( HeaderCoordinates.XTop,HeaderCoordinates.YTop,
                           HeaderCoordinates.XBottom,HeaderCoordinates.YBottom,
                           HeaderCoordinates.LineWidth,
                           HeaderCoordinates.Shading )
         Else
            DrawBox( HeaderCoordinates.XTop,HeaderCoordinates.YTop,
                     HeaderCoordinates.XBottom,HeaderCoordinates.YBottom,
                     HeaderCoordinates.LineWidth );
         end;

      For I := 1 To MaxHeaderLines Do
         begin
         If ( Length(Header[I].Text) > 0 ) Then

            begin
            With Header[I] Do
               begin
               GoToYPos(YPosition);
               SetFontInformation( FontName,FontSize,FontStyle );
               If ( Alignment = jLeft ) Then
                  PrintLeft( Text );
               If ( Alignment = jCenter ) Then
                  PrintCenter( Text );
               If ( Alignment = jRight ) Then
                  PrintRight( Text );
               end;
            end;

      end;
      RestoreCurrentFont;
   End;


procedure TPrintControl._WriteFooters;

   { If any footers are defined, write them }

   Var
   I: Integer;
   Temp: Boolean;

   Begin
      SaveCurrentFont;

      { Set 'AutoPaging' off.
       Otherwise the footer will not get written correctly. }
      Temp := fAutoPaging;
      fAutoPaging := False;

      { Does the user desire a box around the footer? }
      If ( FooterCoordinates.Boxed = True ) Then
         begin
         If ( FooterCoordinates.Shading > 0 ) Then
            DrawBoxShaded( FooterCoordinates.XTop,FooterCoordinates.YTop,
                           FooterCoordinates.XBottom,FooterCoordinates.YBottom,
                           FooterCoordinates.LineWidth,
                           FooterCoordinates.Shading )
         Else
            DrawBox( FooterCoordinates.XTop,FooterCoordinates.YTop,
                     FooterCoordinates.XBottom, FooterCoordinates.YBottom,
                     FooterCoordinates.LineWidth );
         end;

      For I := 1 To MaxFooterLines Do

         begin
         If ( Length(Footer[I].Text) > 0 ) Then
            begin
            With Footer[I] Do

               begin
               GoToYPos(YPosition);
               SetFontInformation( FontName,FontSize,FontStyle );
               If ( Alignment = jLeft ) Then
                  PrintLeft( Text );
               If ( Alignment = jCenter ) Then
                  PrintCenter( Text );
               If ( Alignment = jRight ) Then
                  PrintRight( Text );
               end;

            end;

         end;
      RestoreCurrentFont;

      fAutoPaging := Temp;
   End;


procedure TPrintControl._WritePageNumber;

   { Write page numbers. }

   Var
   Temp: Boolean;
   Buffer: String;

   Begin
      Buffer := PageNumber.Text + IntToStr(fCurrentPage);
      SaveCurrentFont;
      SetFontInformation(PageNumber.FontName,PageNumber.FontSize,
                         PageNumber.FontStyle );

      Temp := fAutoPaging;
      fAutoPaging := False;

      GoToYPos(PageNumber.YPosition);
      If ( PageNumber.Alignment = jLeft ) Then
         PrintLeft( Buffer );
      If ( PageNumber.Alignment = jCenter ) Then
         PrintCenter( Buffer );
      If ( PageNumber.Alignment = jRight ) Then
         PrintRight( Buffer );

      fAutoPaging := Temp;
      RestoreCurrentFont;
   End;


{============================================================}
{                    Table Drawing Routines                  }
{============================================================}

procedure TPrintControl.CreateTable(Columns:Word);

   { Creates a new table }

   Var
   I,J: Integer;
   ColumnWidth: Single;

   Begin
   If Columns in [0..MaxColumns] Then
      Begin
      TableColumns := Columns;
      ColumnWidth := PixelsToInchesHorizontal(TotalPageWidthPixels - LeftMargin
                       - RightMargin)/Columns;
      For I := 1 To TableColumns do
         With ColumnInformation[I] do
            Begin
            Xposition := PixelsToInchesHorizontal(LeftMargin) +
                         (I-1)*ColumnWidth;
            Length := ColumnWidth;
            For J := 1 To MaxHeaderLines do
               Begin
               Text[J] := '';
               End;
            Alignment := jCenter;
            End;
      End;
      fTableStyle := [];
      with TableHeader do
         Begin
         FontName := fFont.Name;
         FontSize := fFont.Size;
         FontStyle := fFont.Style;
         Boxed := False;
         Shading := 255;
         End;
      TableStartY := 0.0;
      TableStartBodyY := 0.0;
      TableBodyVertOffset := TableVertOffset;

   End;


procedure TPrintControl.BeginTable;

   { Begins drawing a table  on the current page }

   Begin

   TableStartY := CurrentY;
   _WriteTableHeaders;
   TableStartBodyY := CurrentY;
   TableBodyVertOffset := TableVertOffset;
   End;


procedure TPrintControl.NextTableRow(IsLastRow: Boolean);

   { Moves to next row of current table being drawn }

   Var
   RowHeight: Single;

   Begin

   { Move current Y position to bottom of current row }
   RowHeight := GetLineHeightInches + 2*TableBodyVertOffset;
   CurrentY := CurrentY + RowHeight;

   { Check if at end of page }
   If (CurrentY + RowHeight > DetailBottom) And Not IsLastRow Then
      Begin
      EndTable;
      NewPage;
      BeginTable;
      End;

   End;


procedure TPrintControl.DrawColumnGridLine( ColumnNumber:Word;
             Style:TTableStyle; LineWidth:Word );

   { Draws grid line at left or bottom of column }

   Var
   Left,Right: Single;
   Top,Bottom: Single;

   Begin
   Left := ColumnInformation[ColumnNumber].Xposition;
   Right := Left + ColumnInformation[ColumnNumber].Length;
   Top := CurrentY - TableBodyVertOffset;
   Bottom := CurrentY + GetLineHeightInches + 2*TableBodyVertOffset;
   If (sHorizontalGrid in Style) Then
      DrawLine( Left, Bottom, Right, Bottom, LineWidth );
   If (sVerticalGrid in Style) Then
      DrawLine( Right, Top, Right, Bottom, LineWidth );
   if (sBorder in Style) Then
      DrawBox( Left, Top, Right, Bottom, LineWidth );
   End;
      

procedure TPrintControl.EndTable;

   { Ends drawing of table on current page }

   Var
   Left,Right: Single;
   X,Y: Single;
   I: Integer;

   Begin

   If (fTableStyle <> []) And (TableColumns > 0) Then

      Begin
      Left := ColumnInformation[1].Xposition;
      Right := ColumnInformation[TableColumns].Xposition +
               ColumnInformation[TableColumns].Length;

      if (sHorizontalGrid in fTableStyle) Then
         Begin
         Y := TableStartBodyY;
         While Y < CurrentY Do
            Begin
            DrawLine(Left,Y,Right,Y,1);
            Y := Y + GetLineHeightInches + 2*TableBodyVertOffset;
            End;
         End;

      if (sVerticalGrid in fTableStyle) And (TableColumns > 1) Then
         Begin
         For I := 2 To TableColumns Do
            Begin
            X := ColumnInformation[I].Xposition;
            DrawLine(X,TableStartY,X,CurrentY, 1);
            End;
         End;

      if (sBorder in fTableStyle) Then
         DrawBox( Left, TableStartY, Right, CurrentY, 2 );
      End;

   End;


procedure TPrintControl._WriteTableHeaders;

   { Write headers at top of table }

   Var
   I,J,Nlines: Integer;
   RowHeight: Single;
   Voffset: Single;
   Ytop, Ybot: Single;
   HeaderColumn: array[1..MAXCOLUMNS] of Boolean;
   S: String;

   Begin

   { Determine number of lines in header }
   If TableColumns = 0 Then Exit;
   Nlines := 0;
   For I := 1 To TableColumns Do
      Begin
      HeaderColumn[I] := False;
      For J := MaxHeaderLines Downto 1 Do
         Begin
         If Length(ColumnInformation[I].Text[J]) > 0 Then
            Begin
            HeaderColumn[I] := True;
            If Nlines < J Then Nlines := J;
            Break;
            End;
         End;
      End;
   If Nlines = 0 Then Exit;  { No header text to print }

   { Set header font }
   SaveCurrentFont;
   with TableHeader do
     SetFontInformation( FontName, FontSize, FontStyle );

   { Save current vertical offset & set it to 0 for subsequent header lines }
   Voffset := TableVertOffset;
   RowHeight := GetLineHeightInches;
   TableVertOffset := 0;

   { Compute top & bottom of table heading }
   Ytop := CurrentY;
   Ybot := CurrentY + Nlines*RowHeight + Voffset;

   { Draw box around each header if called for }
   if TableHeader.Boxed then
      For I := 1 to TableColumns Do
         If HeaderColumn[I] then with ColumnInformation[I] do
            DrawBoxShaded(Xposition, Ytop, Xposition+Length, Ybot, 2,
                          TableHeader.Shading);

   { Draw each line of header text }
   For J := 1 To Nlines Do
      Begin
      For I := 1 To TableColumns Do
         Begin
         S := ColumnInformation[I].Text[J];
         If Length(S) > 0 Then
            Case ColumnInformation[I].Alignment Of
            jLeft:   PrintColumnLeft( I, S );
            jCenter: PrintColumnCenter( I, S );
            jRight:  PrintColumnRight( I, S );
            End;
         End;

      { Move to next row of header }
      CurrentY := CurrentY + RowHeight;
      End;

   { Shift current Y down by vertical offset }
   CurrentY := CurrentY + Voffset;

   { Draw horizontal line underneath header }
   For I := 1 to TableColumns Do
      If HeaderColumn[I]
      Or (sHorizontalGrid in fTableStyle)
      Then with ColumnInformation[I] do
         DrawLine(Xposition, CurrentY, Xposition+Length, CurrentY, 2);

   { Restore original font & vertical offset }
   RestoreCurrentFont;
   TableVertOffset := Voffset;
   End;


procedure TPrintControl.PrintColumnLeft( ColumnNumber:Word; Text:String );

   { Write text, left aligned against the column represented by
     'ColumnInformation[ColumnNumber]' }

   Var
   X,Y: Single;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;
   X := ColumnInformation[ColumnNumber].XPosition + TableHorizOffset;
   Y := CurrentY + TableBodyVertOffset;
   _WriteLine( X, Y, Text );
   End;


procedure TPrintControl.PrintColumnRight( ColumnNumber:Word; Text:String );

   { Write text, right aligned against the column represented by
     'ColumnInformation[ColumnNumber]' }

   Var
   PixelLength: Word;
   StartPixel: Word;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;

   { How many pixels does the text in 'Text' require? }
   PixelLength := fCanvas.TextWidth( Text );

   { Calculate where printing should start }
   StartPixel :=
     InchesToPixelsHorizontal(ColumnInformation[ColumnNumber].XPosition +
     ColumnInformation[ColumnNumber].Length - TableHorizOffset) - PixelLength;

   SetTab( 0.0 );
   _WriteLine( PixelsToInchesHorizontal(StartPixel),
               CurrentY + TableBodyVertOffset, Text );
   SetTab( CurrentTab );

   End;


procedure TPrintControl.PrintColumnCenter( ColumnNumber:Word; Text:String );

   { Print a line of text centered within the column number represented by
     'ColumnNumber', at current Y position }

   Var
   PixelLength: Integer;
   StartPixel: Integer;

   Begin
   if (fCurrentPage <= 0) or (fDoneDrawing) then Exit;

   { How many pixels does the text in 'Text' require? }
   PixelLength := fCanvas.TextWidth( Text );

   { Calculate where printing should start }
   StartPixel :=
     (InchesToPixelsHorizontal( ColumnInformation[ColumnNumber].Length ) Div 2)
      + InchesToPixelsHorizontal(ColumnInformation[ColumnNumber].XPosition)
      - (PixelLength Div 2);

   SetTab( 0.0 );
   _WriteLine( PixelsToInchesHorizontal(StartPixel),
               CurrentY + TableBodyVertOffset, Text );
   SetTab( CurrentTab );

   End;


{***************** These are for the Preview form ****************}

procedure TPreviewForm.NextPageBtnClick(Sender: TObject);
begin
PrintControl.DisplayPage(PageDisplaying + 1);
end;

procedure TPreviewForm.PrevPageBtnClick(Sender: TObject);
begin
PrintControl.DisplayPage(PageDisplaying - 1);
end;

procedure TPreviewForm.PrintBtnClick(Sender: TObject);
begin
PrintControl.PrintAll;
end;

procedure TPreviewForm.FormCreate(Sender: TObject);
begin
KeyPreview := True;
//WindowState := wsMaximized;
Width := Screen.Width;
Height := Screen.Height;
PageDisplaying := 1;
end;

procedure TPreviewForm.PaintBox1Paint(Sender: TObject);
begin
PrintControl.DisplayPage(PageDisplaying);
end;

procedure TPreviewForm.PageWidthBtnClick(Sender: TObject);
begin
PageWidthBtn.Down := True;
PaintBox1.Visible := False;
PaintBox1.Top := Panel1.Height + 15;
PaintBox1.Left := 15;
PaintBox1.Width := ClientWidth - 45;
PaintBox1.Height :=
   (Longint(PaintBox1.Width) * Longint(PrintControl.TotalPageHeightPixels))
   div Longint(PrintControl.TotalPageWidthPixels);
PaintBox1.Visible := True;
end;

procedure TPreviewForm.ShwGrdBtnClick(Sender: TObject);
begin
PaintBox1.Refresh;
end;

procedure TPreviewForm.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 i : Integer;
begin
case Button of
  mbLeft : begin
    PaintBox1.Visible := False;
    PaintBox1.Top := (ClientHeight div 2) - Y;
    PaintBox1.Left := (ClientWidth div 2) - X;
    i := Trunc(PaintBox1.Width * 0.35) div 2;
    PaintBox1.Width := PaintBox1.Width + i;
    PaintBox1.Left := PaintBox1.Left - i;
    i := Trunc(PaintBox1.Height * 0.35) div 2;
    PaintBox1.Height := PaintBox1.Height + i;
    PaintBox1.Top := PaintBox1.Top - i;
    PaintBox1.Visible := True;
    end;
  mbRight : begin
    PaintBox1.Visible := False;
    i := Trunc(PaintBox1.Width * 0.35) div 2;
    PaintBox1.Width := PaintBox1.Width - i;
    PaintBox1.Left := PaintBox1.Left + i;
    i := Trunc(PaintBox1.Height * 0.35) div 2;
    PaintBox1.Height := PaintBox1.Height - i;
    PaintBox1.Top := PaintBox1.Top + i;
    PaintBox1.Top := (ClientHeight div 2) - Y;
    PaintBox1.Left := (ClientWidth div 2) - X;
    PaintBox1.Visible := True;
    end;
  end;
end;

procedure TPreviewForm.FullPageBtnClick(Sender: TObject);
begin
FullPageBtn.Down := True;
PaintBox1.Visible := False;
PaintBox1.Top := 15;
PaintBox1.Height := ScrollBox.height - 30;
PaintBox1.Width :=
   (Longint(PaintBox1.Height) * Longint(PrintControl.TotalPageWidthPixels))
   div Longint(PrintControl.TotalPageHeightPixels);
PaintBox1.Left := (Width div 2) - (PaintBox1.Width div 2);
PaintBox1.Visible := True;
end;

procedure TPreviewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_NEXT) and (NextPageBtn.Enabled) then NextPageBtnClick(Sender);
  if (Key = VK_PRIOR) and (PrevPageBtn.Enabled) then PrevPageBtnClick(Sender);
end;

end.
