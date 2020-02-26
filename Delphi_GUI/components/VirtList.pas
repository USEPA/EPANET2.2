{
   TVirtualListBox Component
   *************************

   A Delphi component that implements a Virtual Listbox control
   for displaying a virtually unlimited number of items.
   It is derived from the TCustomGrid control, consisting of a
   grid with just a single column.
  
   Key Properties:
      Count       = number of items in the list
      HorizScroll = true if horizontal scrollbar used
      ItemIndex   = zero-based index of the currently selected item
                    (-1 indicates no selection)
      ItemHeight  = pixel height of rows used to display items
      (Note: whenever the Font property is changed at design time,
       Itemheight is automatically changed to accomodate the new font size.)

   Key Methods:
      ItemAtPos(Pos: TPoint; Exisitng: Boolean): Integer;
      (Returns the index of the item appearing at X,Y pixel
       position Pos. If no item appears at Pos, returns -1 if
       Exisiting is true, or Count-1 otherwise.)

   Key Events:
      OnGetItem: procedure(Sender: TObject; Index: LongInt;
                 var Value: string; var aColor: TColor);
      (User-supplied event handler that determines the string
       Value and its color to display for list item at position
       Index. Use clVLB for aColor to maintain current font color.)

   Version: 1.0
   Author:  L. Rossman
   Date:    4/7/01
}

unit VirtList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls;

const
  clVLB = -1;

type
  TGetItemEvent = procedure(Sender: TObject; Index: LongInt;
    var Value: string; var aColor: TColor) of Object;

  TVirtualListBox = class(TCustomGrid)
  private
    { Private declarations }
    FCount: LongInt;
    FHorizScroll: Boolean;
    FItemHeight: Integer;
    FItemIndex: LongInt;
    FOnGetItem: TGetItemEvent;
  protected
    { Protected declarations }
    procedure SetCount(Value: LongInt);
    procedure SetHorizScroll(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemIndex(Value: LongInt);
    function  GetItemIndex: LongInt;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure Click; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property Canvas;
    property Col;
  published
    { Published declarations }
    property Count:     LongInt read FCount write SetCount;
    property HorizScroll: Boolean read FHorizScroll write SetHorizScroll;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property ItemIndex: LongInt read GetItemIndex write SetItemIndex;
    property OnGetItem: TGetItemEvent read FOnGetItem write FOnGetItem;
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
    property Align;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
{$R 'VirtList.dcr'}

procedure Register;
begin
  RegisterComponents('EPA', [TVirtualListBox]);
end;

constructor TVirtualListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ColCount := 1;
  Col := 0;
  DefaultDrawing := False;
  FixedCols := 0;
  FixedRows := 0;
  Options := Options - [goVertLine, goHorzLine, goRangeSelect];
  Options := Options + [goRowSelect, goThumbTracking];
  RowCount := 1;
  ScrollBars := ssVertical;
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FCount := 0;
  FItemHeight := 16;
  DefaultRowHeight := FItemHeight;
  DefaultColWidth := Screen.Width;
  FItemIndex := -1;
end;

procedure TVirtualListBox.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) then
    SetItemHeight(Abs(Font.Height)+5);
  inherited;
end;

procedure TVirtualListBox.DrawCell(aCol, aRow: LongInt;
      aRect: TRect; aState: TGridDrawState);
var
  S: String;
  C: TColor;
begin
  S := '';
  C := clVLB;
  if Assigned(FOnGetItem) and (FCount > 0) then
  begin
    OnGetItem(self,aRow,S,C);
  end;
  Canvas.Font.Assign(Font);
    if (gdSelected in aState) and (FCount > 0) then
    begin
      Canvas.Font.Color := clHighlightText;
      Canvas.Brush.Color := clHighlight;
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Font.Color := C;
    end;
    Canvas.FillRect(aRect);
    Canvas.TextOut(aRect.Left+2, aRect.Top, S);
    if (gdSelected in aState) and (gdFocused in aState) then
      Canvas.DrawFocusRect(aRect);
end;

procedure TVirtualListBox.SetCount(Value: LongInt);
begin
  if Value < 0 then FCount := 0
  else FCount := Value;
  if FCount = 0 then
  begin
    RowCount := 1;
    FItemIndex := -1;
  end
  else
  begin
    RowCount := FCount;
    if FItemIndex >= FCount then FItemIndex := FCount-1;
  end;
  if FItemIndex < 0 then Row := 0
  else Row := FItemIndex;
  Repaint;
end;

procedure TVirtualListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then
  begin
    FItemHeight := Value;
    DefaultRowHeight := FItemHeight;
  end;
end;

procedure TVirtualListBox.SetHorizScroll(Value: Boolean);
begin
  if (FHorizScroll <> Value) then
  begin
    FHorizScroll := Value;
    if FHorizScroll = True then ScrollBars := ssBoth
    else ScrollBars := ssVertical;
  end;
end;

procedure TVirtualListBox.SetItemIndex(Value: LongInt);
begin
  if Value < 0 then
  begin
    FItemIndex := -1;
    Row := 0;
  end
  else if Value >= FCount then
  begin
    FItemIndex := FCount - 1;
    if FCount = 0 then Row := 0
    else Row := FItemIndex;
  end
  else
  begin
    FItemIndex := Value;
    Row := FItemIndex;
  end;
end;

function TVirtualListBox.GetItemIndex: LongInt;
begin
  if FCount = 0 then Result := -1
  else
  begin
    FItemIndex := Row;
    Result := FItemIndex;
  end;
end;

function TVirtualListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
var
  Coord: TGridCoord;
  aRow: LongInt;
begin
  if FCount = 0 then Result := -1
  else
  begin
    Coord := MouseCoord(Pos.X, Pos.Y);
    aRow := Coord.Y;
    if (aRow >= 0) and (aRow < FCount) then Result := aRow
    else
    begin
      if Existing then Result := -1
      else Result := FCount;
    end;
  end;
end;

procedure TVirtualListBox.Click;
begin
  inherited Click;
  FItemIndex := Row;
end;

end.
