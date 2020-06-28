unit gbtlst;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  gbconst, StdCtrls, ShadowCanvas, ColorController;

type TOnBeforeSelectEvent = procedure (Sender: TObject; var Proceed : boolean) of object;

type
  TGBTileList = class(TCustomControl)
  private
    { Private declarations }
    FTileSize  : TGBTileSize;
    FFreezed   : boolean;
    FTileCount : Integer;

    Image      : TBitmap;
    PartImage  : TBitmap;
    FShadowCanvas : TShadowCanvas;

    FXLen      : integer;
    FYLen      : integer;

    ScrollBar  : TScrollBar;

    FBookmarks  : array[0..2] of integer;
    FBookCanvas : TShadowCanvas;

    TopTile    : integer;
    FSelTile   : integer;

    FTileData  : TList;

    FOnBeforeSelect : TOnBeforeSelectEvent;
    FOnAfterSelect  : TNotifyEvent;

    procedure SetTileSize(i : TGBTileSize);
    procedure SetControlSize;
    procedure SetTileCount(i : integer);
    procedure ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure DrawBookmark( Tile : integer; y : integer);

    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;

  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure AfterSelect;
    function  BeforeSelect: boolean;
    procedure SetSelTile( i : integer);

    procedure SetBookmark(index : integer; Tile : integer);
    function  GetBookmark(index : integer): integer;
    procedure SetTileData( td : TList );

  public
    { Public declarations }

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property TileData : TList read FTileData write SetTileData;

    procedure paint; override;
    procedure PaintTile( TileNr : integer );

    procedure RefreshList;

    procedure DrawPixel(x,y : integer);

    procedure ScrollUp;
    procedure ScrollDown;

  public
    property Bookmarks[index : integer] : integer read GetBookmark write SetBookmark;
  
  published
    { Published declarations }

    property TileSize : TGBTileSize read FTileSize write SetTileSize;
    property Freezed: boolean read FFreezed write FFreezed;
    property TileCount : integer read FTileCount write SetTileCount;
    property SelTile : Integer read FSelTile write SetSelTile;
    property OnAfterSelect  : TNotifyEvent read FOnAfterSelect write FOnAfterSelect;
    property OnBeforeSelect : TOnBeforeSelectEvent read FOnBeforeSelect write FOnBeforeSelect;

    property OnKeyDown;

    property TabStop;
    property TabOrder;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gameboy', [TGBTileList]);
end;


procedure TGBTileList.ScrollUp;
begin
  with ScrollBar do Position := Position + (FTileCount div 3);
  Paint;
end;


procedure TGBTileList.ScrollDown;
begin
  with ScrollBar do Position := Position - (FTileCount div 3);
  Paint;
end;


procedure TGBTileList.SetTileData( td : TList );
var i : integer;
begin
  FTileData := td;
  if Assigned(td) then
  begin
    for i := 0 to 2 do
      if FBookMarks[i] >= td.Count then
        FBookMarks[i] := -1;

    Paint;
  end;
end;


procedure TGBTileList.SetSelTile( i : integer);
begin
  if BeforeSelect then
  begin
    if (i < 0) or not assigned(FTileData) then
      FSelTile := 0
    else
      if (i > FTileData.Count-1) then
        FSelTile := FTileData.Count-1
    else
        FSelTile := i;

    ScrollBar.Position := FSelTile;
    Paint;
    AfterSelect;
  end;
end;


procedure TGBTileList.SetBookmark(index : integer; Tile : integer);
begin
  if (index >= 0) and (index <= 2) then
  begin
    if (Tile >= 0) and assigned(FTileData) and (Tile < FTileData.Count) then
      FBookmarks[Index] := Tile
    else
      FBookmarks[Index] := -1;
    Paint;
  end;
end;


function  TGBTileList.GetBookmark(index : integer): integer;
begin
  Result := FBookmarks[index];
end;



procedure TGBTileList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SelTile := TopTile + (y div (FYLen*2+1));
end;

procedure TGBTileList.AfterSelect;
begin
  if Assigned(FOnAfterSelect) then FOnAfterSelect(Self);
end;

function TGBTileList.BeforeSelect: boolean;
begin
  Result := True;
  if Assigned(FOnBeforeSelect) then FOnBeforeSelect(Self, Result);
end;

constructor TGBTileList.Create(AOwner : TComponent);
var i : integer;
begin
  Inherited Create(AOwner);

  FSelTile := 0;
  TopTile := 0;

  Image := TBitmap.Create;
  with Image.Canvas.Font do
  begin
    Name := 'Arial';
    Pitch := fpFixed;
    Size := 8;
  end;
  Image.Width := 23;

  PartImage := TBitmap.Create;
  FShadowCanvas := TShadowCanvas.Create(1,1);
  FBookCanvas := TShadowCanvas.Create(5,7);

  ScrollBar := TScrollBar.Create(Self);
  ScrollBar.Parent := Self;
  ScrollBar.Kind := sbVertical;
  ScrollBar.SmallChange := 1;
  ScrollBar.SetParams(SelTile, 0,0);
  ScrollBar.OnScroll := ScrollEvent;

  ScrollBar.ControlStyle := ScrollBar.ControlStyle - [csNoStdEvents];

  for i := 0 to 2 do FBookmarks[i] := -1;
end;



destructor TGBTileList.Destroy;
begin

  ScrollBar.Free;
  Image.free;
  PartImage.free;
  FShadowCanvas.Free;
  FBookCanvas.Free;

  Inherited destroy;
end;



procedure TGBTileList.SetControlSize;
begin
  (* change width *)
  Image.Height := (FYLen* 2 * FTileCount)+FTileCount+1;

  PartImage.Width := FXLen*2;
  PartImage.Height := FYLen*2;

  FShadowCanvas.Free;
  FShadowCanvas := TShadowCanvas.Create(FXLen*2, (FYLen* 2 * FTileCount)+FTileCount+1 );


  SetBounds(Left, Top, Image.Width + FShadowCanvas.Width + ScrollBar.Width+2, Image.Height);

  if Assigned(FTileData) and (FTileData.Count > 0) then
    ScrollBar.SetParams(SelTile, 0, FTileData.Count-1)
  else
    ScrollBar.SetParams(SelTile, 0, 0);

  ScrollBar.Top := 1;
  ScrollBar.Left := Image.Width + FShadowCanvas.Width + 1;
  ScrollBar.Height := Height-2;
  ScrollBar.LargeChange := FTileCount+1;
  ScrollBar.TabStop := False;

end;

procedure TGBTileList.ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin

  (* handle arrow-buttons *)
  case ScrollCode of
    scLineUp   : ScrollPos := TopTile-1;
    scLineDown : ScrollPos := TopTile+TileCount;
  end;

  if (ScrollPos < 0) then ScrollPos := 0;
  if (ScrollPos > ScrollBar.Max) then ScrollPos := ScrollBar.Max;

  paint;
end;


procedure TGBTileList.SetTileCount(i : integer);
begin
  if (i < 1) then FTileCount := 1
  else            FTileCount := i;
  SetControlSize;
end;


procedure TGBTileList.SetTileSize(i : TGBTileSize);
begin
  DetermineTileSize(i, FXLen, FYLen);
  FTileSize := i;
  SetControlSize;
end;


procedure TGBTileList.DrawBookmark( Tile : integer; y : integer);
var i : integer;
var iHighLightText : integer;
begin
  i := 0;
  if (FBookmarks[0] = Tile) then i := 1
  else if (FBookmarks[1] = Tile) then i := 2
  else if (FBookmarks[2] = Tile) then i := 3;

  if (i <> 0) then
    with FBookCanvas do
    begin
      iHighLightText := GetShadowColor(clHighLightText);

      RectangleSized(0,0,5,7, GetShadowColor(clHighLight));
      case i of
        1  :
        begin
          SetPixel(1,2, iHighLightText);
          VerLine(2,1, 5, iHighLightText);
          SetPixel(1, 5, iHighLightText);
          SetPixel(3, 5, iHighLightText);
        end;
        2  :
        begin
          HorLine (1, 1, 2, iHighLightText);
          SetPixel(3, 2, iHighLightText);
          SetPixel(2, 3, iHighLightText);
          SetPixel(1, 4, iHighLightText);
          HorLine (1, 5, 3, iHighLightText);
        end;
        3  :
        begin
          HorLine (1, 1, 2, iHighLightText);
          SetPixel(3, 2, iHighLightText);
          SetPixel(2, 3, iHighLightText);
          SetPixel(3, 4, iHighLightText);
          HorLine (1, 5, 2, iHighLightText);
        end;
      end;


      Draw(Image.Canvas.Handle,2, y+2);
    end;


end;


procedure TGBTileList.Paint;
var i,j,k: integer;
var p : PTileType;
var base,x,y, tx : integer;
var s : string;
var c : TColor;

var iBtnFace, i3DDkShadow, iHighLight : integer;
var Col : PGBColorType;
var iCol, iSelCol : PGBColorSets;
var ClrIndex : integer;
begin

  if (not Freezed) and Assigned(GBColorController) then
  begin

    (* Get colors *)
    iBtnFace    := GetShadowColor(clBtnFace);
    i3DDkShadow := GetShadowColor(cl3DDkShadow);
    iHighLight  := GetShadowColor(clHighLight);

    iCol    := @GBColorController.CurShadowColorSets[0];
    iSelCol := @GBColorController.CurShadowSelColorSets[0];
    ClrIndex := GBColorController.ColorSetIndex;



    if (ScrollBar.Position < TopTile) then TopTile := ScrollBar.Position;
    if (ScrollBar.Position >= (TopTile+FTileCount)) then TopTile := (ScrollBar.Position-FTileCount)+1;

    x := FXLen*2;
    y := FYLen*2;

    with Image.Canvas do
    begin

      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;

      base := 0;
      for i := 0 to FTileCount-1 do
      begin
        Pen.Color := clBtnShadow;
        Rectangle(0, base, 22, base + y + 1 );

        base := base + y + 1 ;
      end;

      base := 1;
      for i := 0 to FTileCount-1 do
      begin
        Pen.Color := clBtnHighlight;
        MoveTo( 21, base );
        LineTo( 1, base );
        LineTo( 1, base + y -1 );

        Pen.Color := cl3DDkShadow;
        MoveTo( 22, base-1 );
        LineTo( 22, base + y  );
        LineTo( 0, base + y  );

        base := base + y + 1;
      end;

      (* clean up *)
      Pen.Color := clBtnHighlight;
      MoveTo(1, Image.Height-1 );
      LineTo(23, Image.Height-1 );
      Pixels[0, Image.Height-1] := clBtnShadow;
      Pixels[Image.Width-1,0] := clBtnShadow;

      if Assigned(FTileData) then
      begin

        Pen.Color := clBtnText;
        base := ((y - Image.Canvas.Font.Size) div 2) - 2;
        for i:= 0 to FTileCount-1 do
        begin
          if ((TopTile+i) < FTileData.Count) then
          begin
            s := IntToStr(TopTile+i);
            tx := 2 + ((21 - (Length(s)*7)) div 2);
            TextOut(tx, (i * y) + i + base , s);

            DrawBookmark( TopTile+i, (i*y) +i);
          end;
        end;


        base := 1;
        for k :=0 to FTileCount-1 do
        begin
          FShadowCanvas.HorLine( 0, base-1, x, i3DDkShadow);

          if ((TopTile+k) < FTileData.Count) then
          begin
            p := FTileData.Items[TopTile+k];

            (* determine colorset *)
            if ((TopTile + k) = FSelTile) then
              Col := @iSelCol[p.ColorSet[ClrIndex]]
            else
              Col := @iCol[p.ColorSet[ClrIndex]];

            (* draw pixels *)
            for i := 0 to FYLen-1 do
              for j := 0 to FXLen-1 do
              begin
                FShadowCanvas.RectangleSized( (j*2), base + i*2, 2, 2, Col[p.data[i,j]]);
              end;

          end
          else
          begin
            with FShadowCanvas do
              RectangleSized( 0, base, x, y, iBtnFace );
          end;

          base := base + y + 1;
        end;


        (* clean up *)
        with FShadowCanvas do
        begin
          HorLine( 0, 0, x, GetShadowColor(clBtnShadow));
          HorLine( 0, Height-1, x, GetShadowColor(clBtnHighlight));
        end;



      end;
    end;
    self.canvas.draw(0,0,Image);
    FShadowCanvas.Draw(Self.Canvas.Handle, 23, 0 );
  end;

end;


procedure TGBTileList.PaintTile( TileNr : integer );
var i, j : integer;
    p    : PTileType;
    Col  : PGBColorType;

var ClrIndex : integer;

begin

  if (not Freezed) and Assigned(GBColorController) and
     (TileNr >= TopTile) and (TileNr < (TopTile+FTileCount)) then
  begin

    p := FTileData.Items[TileNr];
    ClrIndex := GBColorController.ColorSetIndex;

    (* determine colorset *)
    if (TileNr = FSelTile) then
      Col := @GBColorController.CurShadowSelColorSets[0, p.ColorSet[ClrIndex]]
    else
      Col := @GBColorController.CurShadowColorSets[0, p.ColorSet[ClrIndex]];



    (* draw pixels *)
    for i := 0 to FYLen-1 do
      for j := 0 to FXLen-1 do
      begin
        FShadowCanvas.RectangleSized( (j*2), i*2, 2, 2, Col[p.data[i,j]]);
      end;


    FShadowCanvas.DrawPart( Self.Canvas.Handle, 23,
                            ((TileNr - TopTile) * FYLen * 2) + (TileNr - TopTile)+1,
                            FXLen*2, FYLen*2);
  end;

end;



procedure TGBTileList.DrawPixel(x,y: integer);
var p : PTileType;
var i,j : integer;
var col : TColor;
begin

  if (FSelTile >= TopTile) and (FSelTile <= (TopTile + FTileCount)) and
     (Not Freezed) and Assigned(FTileData) then
  begin
    (* get color *)
    p := FTileData.Items[FSelTile];
    j := p.data[y,x];

    (* draw pixel *)
    i := ((FSelTile-TopTile) * (FYLen*2)) + (FSelTile-TopTile) + 1;
    with Canvas do
    begin
      Pen.Color := GBColorController.CurSelColorSets[0,p.ColorSet[GBColorController.ColorSetIndex],j];
      Rectangle( 23+(x*2), i + y*2, 23 + (x+1)*2, i + (y+1)*2 );
    end;
  end;
end;



procedure TGBTileList.RefreshList;
begin
  SetControlSize;
  if Assigned(FTileData) and ((TopTile+TileCount) >= FTileData.Count-1) then TopTile := 0;
//[har]
  SelTile := SelTile;
end;


procedure TGBTileList.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;


end.
