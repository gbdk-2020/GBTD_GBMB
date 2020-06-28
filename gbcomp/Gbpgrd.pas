unit gbpgrd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  extCtrls, gbconst, ColorController;


type
  TGBPixelGrid = class(TCustomControl)
  private
    { Private declarations }
    FTileSize      : TGBTileSize;
    FXLen          : integer;
    FYLen          : integer;
    FPixelSize     : integer;
    FGridLines     : boolean;
    FNibbleMarkers : boolean;
    FFreezed       : boolean;
    FTileData      : PTileType;
    FOnPixelClick  : TPixelClickEvent;
    FAfterPaint    : TAfterPaintEvent;
    Image          : TBitmap;
    FXPos          : integer;
    FYPos          : integer;

    procedure SetTileSize(i : TGBTileSize);
    procedure SetPixelSize(i : integer);
    procedure SetGridLines(i : boolean);
    procedure SetNibbleMarkers(i : boolean);

    procedure CalcPixel(x, y : integer; var col, row : integer);
    procedure SetControlSize;
    procedure PixelClick(Button: TMouseButton; x,y : integer);
    procedure DrawPixel(x,y : integer);

    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;

  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  public
    { Public declarations }
    procedure Paint; override;
    property TileData : PTileType read FTileData write FTileData;

  published
    { Published declarations }
    property TileSize : TGBTileSize read FTileSize write SetTileSize;
    property PixelSize : integer read FPixelSize write SetPixelSize;
    property GridLines: boolean read FGridLines write SetGridLines;
    property NibbleMarkers: boolean read FNibbleMarkers write SetNibbleMarkers;
    property Freezed: boolean read FFreezed write FFreezed;
    property AfterPaint: TAfterPaintEvent read FAfterPaint write FAfterPaint;
    property OnPixelClick: TPixelClickEvent read FOnPixelClick write FOnPixelClick;
    property OnMouseMove;
  end;


procedure Register;



implementation


procedure Register;
begin
  RegisterComponents('Gameboy', [TGBPixelGrid]);
end;


constructor TGBPixelGrid.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);

  Image := TBitmap.Create;

  FXPos := -1;
  FYPos := -1;
end;


destructor TGBPixelGrid.Destroy;
begin
  Image.free;

  Inherited Destroy;
end;


procedure TGBPixelGrid.SetTileSize(i : TGBTileSize);
begin
  DetermineTileSize(i, FXLen, FYLen);
  FTileSize := i;
  SetControlSize;
end;


procedure TGBPixelGrid.SetPixelSize(i : integer);
begin
  if (i < 2) then i := 2;
  FPixelSize := i;
  SetControlSize;
end;


procedure TGBPixelGrid.SetGridLines(i : boolean);
begin
  FGridLines := i;
  Invalidate;
end;


procedure TGBPixelGrid.SetNibbleMarkers(i : boolean);
begin
  FNibbleMarkers := i;
  Invalidate;
end;


procedure TGBPixelGrid.SetControlSize;
begin
  (* change width *)
  SetBounds(Left,Top, FXLen*FPixelSize+1, FYLen*FPixelSize+1);
  Image.Width := FXLen*FPixelSize+1;
  Image.Height := FYLen*FPixelSize+1;
end;


procedure TGBPixelGrid.CalcPixel(x, y : integer; var col, row : integer);
begin
  col := (x div FPixelSize);
  row := (y div FPixelSize);
end;


procedure TGBPixelGrid.PixelClick(Button: TMouseButton; x,y : integer);
var Upd : TGBUpdateMode;
begin
  if Assigned(FOnPixelClick) then
    FOnPixelClick(x,y, Button, Upd);

  if (Upd = gbumPixel) then
    DrawPixel(x,y)
  else
    Invalidate;
end;


procedure TGBPixelGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i,j : integer;
begin

  if (ssLeft in Shift) then
  begin
    CalcPixel(x, y, i,j);
    PixelClick(Button, i,j);
  end;

  if (ssRight in Shift) then
  begin
    CalcPixel(x, y, i,j);
    PixelClick(Button, i,j);
  end;

end;


procedure TGBPixelGrid.WMMouseMove(var Message: TWMMouseMove);
var
  i,j    : integer;
  Button : TMouseButton;
  upd    : TGBUpdateMode;
begin

  if ((Message.Keys and MK_LBUTTON)<>0) or ((Message.Keys and MK_RBUTTON)<>0) then
  begin
    CalcPixel(Message.xPos,Message.yPos,i,j);

    if (i >= 0) and (i < FXLen) and (j >= 0) and (j < FYLen) then
    begin
      if ((FXPos <> i) or (FYPos <> j)) then
      begin
        FXPos := i;
        FYPos := j;

        if ((Message.Keys and MK_LBUTTON)<>0) then
          Button := mbLeft
        else Button := mbRight;

        PixelClick(Button,i,j);
      end;
    end;
  end;

end;


procedure TGBPixelGrid.Paint;
(***************)
(* Draws image *)
(***************)
var
  i,j      : integer;
  c        : TColor;
  iCol     : PGBColorSets;
  ClrIndex : integer;

begin

  if (not FFreezed) and Showing and Assigned(FTileData) then
  begin
    iCol     := @GBColorController.CurColorSets[0];
    ClrIndex := GBColorController.ColorSetIndex;

    (* Reset Brush *)
    Image.Canvas.Brush.Style := bsSolid;

    (* draw pixels *)
    for i := 0 to FYLen-1 do
      for j := 0 to FXLen-1 do
      begin
        c := iCol[FTileData.ColorSet[ClrIndex],FTileData.data[i,j]];
        with Image.Canvas do
        begin
          Pen.Color := c;
          Brush.Color := c;
          Rectangle(1 + (j*FPixelSize), 1 + (i*FPixelSize), 1+((j+1)*FPixelSize), 1+((i+1)*FPixelSize));
        end;
      end;

    with Image.Canvas do
    begin
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(0,0,Width,height);
    end;

    (* Draw gridlines *)
    if FGridLines then
    begin
      (* draw horizontal lines *)
      for i := 1 to FYLen-1 do
        with Image.Canvas do
        begin
          MoveTo(i*FPixelSize,1);
          LineTo(i*FPixelSize, height-1);
        end;

      (* draw vertical lines *)
      for i := 1 to FYLen-1 do
        with Image.Canvas do
        begin
          MoveTo(1,i*FPixelSize);
          LineTo(Width-1, i*FPixelSize);
        end;
    end;

    (* Draw nibble markers *)
    if FNibbleMarkers then
    with Image.Canvas do
    begin
      Pen.Color := clblue;
      for i := 0 to (FXLen div 4) do
        for j := 0 to (FYLen div 4) do
        begin
          MoveTo((i*4*FPixelSize)-1, (j*4*FPixelSize));
          LineTo((i*4*FPixelSize)+2, (j*4*FPixelSize));

          MoveTo((i*4*FPixelSize), (j*4*FPixelSize)-1);
          LineTo((i*4*FPixelSize), (j*4*FPixelSize)+2);
        end;
    end;

    (* Blitter *)
    Self.Canvas.Draw(0,0,Image);

    (* Handle event *)
    if Assigned(FAfterPaint) then FAfterPaint(0,0,gbumTotal);
  end;

end;


procedure TGBPixelGrid.DrawPixel(x,y : integer);
var
  c      : TColor;
  xb, yb : integer;
begin

  if Assigned(FTileData) then
  begin
    if GridLines or (x >= FXLen-1) then
      xb := 0
    else
      xb := 1;

    if GridLines or (y >= FYLen-1) then
      yb := 0
    else
      yb := 1;

    with GBColorController do
      c := CurColorSets[0,FTileData.ColorSet[ColorSetIndex],FTileData.data[y,x]];

    with Canvas do
    begin
      Pen.Color := c;
      Brush.Color := c;
      Rectangle(1 + (x*FPixelSize), 1 + (y*FPixelSize), xb+((x+1)*FPixelSize), yb+((y+1)*FPixelSize));
    end;

    (* Draw gridlines *)
    if FNibbleMarkers then
      with Canvas do
      begin
        Pen.Color := clblue;

        xb := ((x+1) div 4) * (4*FPixelSize);
        yb := ((y+1) div 4) * (4*FPixelSize);

        MoveTo(xb-1, yb);
        LineTo(xb+2, yb);

        MoveTo(xb, yb-1);
        LineTo(xb, yb+2);
      end;

    (* Handle event *)
    if Assigned(FAfterPaint) then FAfterPaint(x,y,gbumPixel);
  end;

end;

end.
