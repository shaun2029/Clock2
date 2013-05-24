unit TouchList;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, types, LCLProc;

type
  TTouchItemSelected = procedure(Sender: TObject; Index: integer) of object;

  { TTouchList }

  TTouchList = class(TCustomControl)
  private
    FPositionIndex: Integer;
    FItemIndex: Integer;
    FWindowColour: TColor;
    FSelectedColour: tColor;
    FScrollbarColour: TColor;
    FScrollbarPositionColour: TColor;
    FPageButtonColour: TColor;
    FTrimItems: boolean;

    function GetTextHeight: integer;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure SetItemIndex(AValue: integer);
    procedure SetPageButtonColour(AValue: TColor);
    procedure SetScrollbarColour(AValue: TColor);
    procedure SetSelectedColour(AValue: TColor);
    procedure SetTrimItems(AValue: boolean);
    procedure SetWindowColour(AValue: TColor);

    procedure Paint; override;
    procedure OnItemsChange(Sender: TObject);
  public
    Items: TStringList;
    OnItemSelected: TTouchItemSelected;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property WindowColour: TColor read FWindowColour write SetWindowColour;
    property SelectedColour: TColor read FSelectedColour write SetSelectedColour;
    property ScrollbarColour: TColor read FScrollbarColour write SetScrollbarColour;
    property PageButtonColour: TColor read FPageButtonColour write SetPageButtonColour;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property TrimItems: boolean read FTrimItems write SetTrimItems;
  end;

const
  LEFT_MARGIN = 8;
  SCROLLBAR_WIDTH = 32;
  PAGE_BUTTON_HEIGHT = 32;

implementation

{ TTouchList }

function TTouchList.GetTextHeight: integer;
begin
  Result := Canvas.TextHeight('Jj');
  Result := Result * 2;
end;

procedure TTouchList.Paint;
var
  TextHeight: Integer;
  i, x, y: Integer;
  DisplayedLines: Integer;
  Pages: Integer;
  PosHeight: integer;
  BarLength: Integer;
  PosPage: Integer;
  Position: Int64;
  ItemStr: string;
begin
  Canvas.Brush.Color := FWindowColour;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(0, 0, Width-1, Height-1);

  TextHeight := GetTextHeight;

  x := 10;
  y := 0;
  i := FPositionIndex;

  // Write items
  while (i >= 0) and (i < Items.Count) do
  begin
    if i <> FItemIndex then
      Canvas.Brush.Color := FWindowColour
    else
      Canvas.Brush.Color := FSelectedColour;

    ItemStr := Items.Strings[i];
    ItemStr := ExcludeTrailingPathDelimiter(ItemStr);

    if FTrimItems then
    begin
      // Trim from the front of the string until it fits
      while Canvas.TextExtent(ItemStr).cx > (Width - SCROLLBAR_WIDTH - LEFT_MARGIN * 2) do
      begin
        ItemStr := UTF8Copy(ItemStr, 2, UTF8Length(ItemStr));
      end;
    end;

    Canvas.TextOut(x, y + TextHeight div 4, ItemStr);

    Inc(y, TextHeight);
    Inc(i);

    if Y + TextHeight > Height then Break;
  end;

  // Draw scrollbar
  Canvas.Brush.Color := FScrollbarColour;
  Canvas.Rectangle(Width - SCROLLBAR_WIDTH, 0, Width-1, Height-1);

  //Draw position
  BarLength := Height - (PAGE_BUTTON_HEIGHT * 2);
  DisplayedLines := Height div TextHeight;
  Pages := (Items.Count div DisplayedLines);
  if Items.Count mod DisplayedLines > 0 then Inc(Pages);

  if (Pages > 1) and (FPositionIndex >= 0) then
  begin
    PosHeight := BarLength div Pages;
    PosPage := FPositionIndex div DisplayedLines;
    if FPositionIndex mod DisplayedLines > 0 then Inc(PosPage);

    Position := Trunc((BarLength * PosPage) / Pages) + PAGE_BUTTON_HEIGHT;

    if PosPage >= Pages - 1 then
    begin
      PosHeight := Height - Position;
    end
    else if PosPage = 0 then
    begin
      PosHeight := PosHeight + Position;
      Position := 0;
    end;

    // Draw bottom page button
    Canvas.Brush.Color := FScrollbarPositionColour;
    Canvas.Rectangle(Width - SCROLLBAR_WIDTH, Position, Width-1, Position+PosHeight-1);
  end;

  // Draw top page button
  Canvas.Brush.Color := FPageButtonColour;
  Canvas.Rectangle(Width - SCROLLBAR_WIDTH, 0, Width-1, PAGE_BUTTON_HEIGHT);

  // Draw bottom page button
  Canvas.Rectangle(Width - SCROLLBAR_WIDTH, Height - PAGE_BUTTON_HEIGHT, Width-1, Height-1);

  inherited Paint;
end;

procedure TTouchList.OnItemsChange(Sender: TObject);
begin
  FItemIndex := -1;
  FPositionIndex := 0;
  Invalidate;
end;

procedure TTouchList.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  TextHeight, BarPosition, BarLength, Pages, DisplayedLines: integer;
begin
  TextHeight := GetTextHeight;
  DisplayedLines := Height div TextHeight;

  Pages := (Items.Count div DisplayedLines);
  if Items.Count mod DisplayedLines > 0 then Inc(Pages);

  if Pages > 0 then
  begin
    if Button = mbLeft then
    begin
      if X < Width - SCROLLBAR_WIDTH then
      begin
        // Text clicked
        FItemIndex := FPositionIndex + (Y div TextHeight);
        Invalidate;

        if Assigned(OnItemSelected) then OnItemSelected(Self, FItemIndex);
      end
      else
      begin
        // Scrollbar clicked
        if Y < PAGE_BUTTON_HEIGHT then
        begin
          // Page up clicked
          FPositionIndex := FPositionIndex - DisplayedLines;
          if FPositionIndex < 0 then FPositionIndex := 0;
          Invalidate;
        end
        else if Y > Height - PAGE_BUTTON_HEIGHT then
        begin
          // Page down clicked
          if FPositionIndex + DisplayedLines < Items.Count then
            FPositionIndex := FPositionIndex + DisplayedLines;

          if FPositionIndex > Items.Count - 1 then FPositionIndex := Items.Count - 1;
          Invalidate;
        end
        else
        begin
          // Scrollbar clicked
          BarLength := Height - (PAGE_BUTTON_HEIGHT * 2);
          BarPosition := Y - PAGE_BUTTON_HEIGHT;

          FPositionIndex := ((BarPosition * Pages) div BarLength) * DisplayedLines;

          if FPositionIndex < 0 then FPositionIndex := 0
          else if FPositionIndex > Items.Count - 1 then FPositionIndex := Items.Count - 1;

          Invalidate;
        end
      end;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TTouchList.SetItemIndex(AValue: integer);
begin
  if FItemIndex = AValue then Exit;
  if AValue < Items.Count -1 then
    FItemIndex := AValue
  else
    FItemIndex := -1;
end;

procedure TTouchList.SetPageButtonColour(AValue: TColor);
begin
  if FPageButtonColour=AValue then Exit;
  FPageButtonColour:=AValue;
  Invalidate;
end;

procedure TTouchList.SetScrollbarColour(AValue: TColor);
begin
  if FScrollbarColour=AValue then Exit;
  FScrollbarColour:=AValue;
  Invalidate;
end;

procedure TTouchList.SetSelectedColour(AValue: TColor);
begin
  if FSelectedColour=AValue then Exit;
  FSelectedColour:=AValue;
  Invalidate;
end;

procedure TTouchList.SetTrimItems(AValue: boolean);
begin
  if FTrimItems=AValue then Exit;
  FTrimItems:=AValue;
  Invalidate;
end;

procedure TTouchList.SetWindowColour(AValue: TColor);
begin
  if FWindowColour=AValue then Exit;
  FWindowColour:=AValue;
  Invalidate;
end;


constructor TTouchList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnItemSelected := nil;

  Align := alClient;
  Items := tStringList.Create;
  Items.OnChange := OnItemsChange;
  FPositionIndex := 0;
  FItemIndex := -1;

  FWindowColour := clWhite;
  FSelectedColour := $F0F0F0;
  FScrollbarColour := $D0D0D0;
  FScrollbarPositionColour := $A0A0A0;
  FPageButtonColour := $C0C0C0;

  FTrimItems := False;
end;

destructor TTouchList.Destroy;
begin
  Items.Free;

  inherited Destroy;
end;

end.
