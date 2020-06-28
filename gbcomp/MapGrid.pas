unit MapGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls, GBConst, ShadowCanvas, extctrls, ColorController ;

const
  MAXZOOMFACTOR = 8;
  MAXFIXEDSIZE  = 256 * 32;




type
  TSelectTileEvent = procedure (Sender: TObject; x,y : integer;Button: TMouseButton; var FullRefresh, Continue : boolean) of object;

  TMouseMode = (mmNone, mmSelecting, mmMoving);


  TMapGrid = class(TCustomControl)
  private
    { Private declarations }

    FZmWidth       : integer;
    FZmHeight      : integer;


    FMouseMode : TMouseMode;

    FHorScroll : TScrollBar;
    FVerScroll : TScrollBar;
    FScrollsShowing : boolean;
    FShadowCanvas : TShadowCanvas;
    FFixedRow     : TBitmap;
    FFixedCol     : TBitmap;

    FTileData   : TList;
    FMapData    : TGBMapType;
    FFreezed    : boolean;
    FZoomFactor : integer;
    FTileWidth  : integer;
    FTileHeight : integer;
    FGrid       : boolean;
    FDoubleMarkers : boolean;
    FPropColors : boolean;

    FBeforeSelect : TSelectTileEvent;
    FAfterSelect  : TNotifyEvent;

    FSelCacheX  : integer;
    FSelCacheY  : integer;

    FSelStartX    : integer;
    FSelStartY    : integer;

    FSelZoneLeft   : integer;
    FSelZoneRight  : integer;
    FSelZoneTop    : integer;
    FSelZoneBottom : integer;

    FRowCached     : boolean;
    FColCached     : boolean;

    procedure DrawDoubleMarkers;
    procedure DrawTiles;

//    procedure BuildFixed;
    procedure BuildFixedRow;
    procedure BuildFixedCol;
    procedure DrawFixed;
    procedure DrawSelTile(b : boolean);

    procedure SetTileData( lst : TList );
    procedure SetMapData( m : TGBMapType );
    procedure SetZoomFactor( i : integer );
    procedure SetTileWidth( i : integer );
    procedure SetTileHeight( i : integer );

    function GetRowCount : integer;
    function GetColCount : integer;
    procedure OnScrollChange(Sender: TObject);

    procedure SetSelZone(x,y : integer);
    procedure AddSelZone(x,y : integer);

    procedure FillZmDim;


    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;

  protected
    { Protected declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure BeforeSelect(x,y : integer;Button: TMouseButton; var FullRefresh, Continue : boolean);
    procedure AfterSelect;

    procedure CalcScreen( w, h : integer);

  public
    { Public declarations }
    property TileData : TList read FTileData write SetTileData;
    property MapData : TGBMapType read FMapData write SetMapData;
    property Hor     : TScrollBar read FHorScroll;
    property Ver     : TScrollBar read FVerScroll;
    property RowCount : integer read GetRowCount;
    property ColCount : integer read GetColCount;


    procedure Paint; override;
    procedure PaintSelected;

    procedure ScrollUp;
    procedure ScrollDown;

    procedure SetTileBounds(TileWidth, TileHeight : integer);
    procedure SetSelectedTile( x,y : integer;Button: TMouseButton);
    procedure VerifySelBoundaries;

  published
    { Published declarations }

    property ZoomFactor : integer read FZoomFactor write SetZoomFactor;
    property TileWidth : integer read FTileWidth write SetTileWidth;
    property TileHeight : integer read FTileHeight write SetTileHeight;
    property Freezed: boolean read FFreezed write FFreezed;

    property Grid: boolean read FGrid write FGrid;
    property DoubleMarkers: boolean read FDoubleMarkers write FDoubleMarkers;
    property PropColors: boolean read FPropColors write FPropColors;

    property SelZoneLeft   : integer read FSelZoneLeft;
    property SelZoneRight  : integer read FSelZoneRight;
    property SelZoneTop    : integer read FSelZoneTop;
    property SelZoneBottom : integer read FSelZoneBottom;

    property OnBeforeSelect : TSelectTileEvent read FBeforeSelect write FBeforeSelect;
    property OnAfterSelect : TNotifyEvent read FAfterSelect write FAfterSelect;

  end;

procedure Register;

implementation


const FIXEDWIDTH = 27;
      FIXEDHEIGHT = 17;


procedure Register;
begin
  RegisterComponents('Gameboy', [TMapGrid]);
end;


procedure TMapGrid.ScrollUp;
begin
  with FVerScroll do
    if Visible then Position := Position + 2;
end;

procedure TMapGrid.ScrollDown;
begin
  with FVerScroll do
    if Visible then Position := Position - 2;
end;

procedure TMapGrid.FillZmDim;
begin
  if (FZoomFactor > 0) then
  begin
    FZmWidth  := FTileWidth * FZoomFactor;
    FZmHeight := FTileHeight * FZoomFactor;
  end
  else
  begin
    FZmWidth  := FTileWidth div 2;
    FZmHeight := FTileHeight div 2;
  end;
end;


function TMapGrid.GetRowCount : integer;
begin
  Result := FShadowCanvas.Height div FZmHeight;
end;

function TMapGrid.GetColCount : integer;
begin
  Result := FShadowCanvas.Width div FZmWidth;
end;

constructor TMapGrid.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FHorScroll := TScrollBar.Create(Self);
  FHorScroll.Parent := Self;
  FHorScroll.Kind := sbHorizontal;
  FHorScroll.OnChange := OnScrollChange;
  FHorScroll.TabStop := False;


  FVerScroll := TScrollBar.Create(Self);
  FVerScroll.Parent := Self;
  FVerScroll.Kind := sbVertical;
  FVerScroll.OnChange := OnScrollChange;
  FVerScroll.TabStop := False;

  FShadowCanvas := TShadowCanvas.Create(500,400);
  FFixedRow     := TBitmap.Create;
  FFixedRow.Height := FIXEDHEIGHT-1;
  FFixedCol     := TBitmap.Create;
  FFixedCol.Width := FIXEDWIDTH-1;
end;



destructor TMapGrid.Destroy;
begin
  FFixedCol.Free;
  FFixedRow.Free;
  FShadowCanvas.Free;

  FVerScroll.Free;
  FHorScroll.Free;

  inherited Destroy;
end;



procedure TMapGrid.SetTileData( lst : TList);
begin
  FTileData := lst;
end;


procedure TMapGrid.SetSelectedTile( x,y : integer;Button: TMouseButton);
var FF,cont : boolean;
begin

  if not Assigned(FMapData) then
  begin
    x := 0;
    y := 0;
  end
  else
  begin
    if (x < 0) then x := 0;
    if (x >= FMapData.Width) then x := FMapData.Width-1;
    if (y < 0) then y := 0;
    if (y >= FMapData.Height) then y := FMapData.Height-1;
  end;

  FF := False;
  Cont := True;
  BeforeSelect(x, y, Button,FF, Cont);

  if Cont then
  begin
    (* determine what to draw *)
    if (FF or (FSelZoneLeft <> FSelZoneRight) or (FSelZoneTop <> FSelZoneBottom)) then
    begin

      FSelZoneLeft := x;
      FSelZoneTop := y;
      FSelZoneRight := x;
      FSelZoneBottom := y;

      if Assigned (FMapData) then DrawTiles;
    end
    else
      begin
        if ((FSelZoneLeft <> x) or (FSelZoneTop <> y)) then DrawSelTile(False);

        FSelZoneLeft := x;
        FSelZoneTop := y;
        FSelZoneRight := x;
        FSelZoneBottom := y;

        DrawSelTile(True);
      end;

    AfterSelect;
  end;
end;


procedure TMapGrid.SetMapData( m : TGBMapType );
begin
  FMapData := m;

  if Assigned(FMapData) then
  begin
    FVerScroll.Max := m.Height-1;
    FHorScroll.Max := m.Width-1;

    CalcScreen( Width, Height );
  end;

end;

procedure TMapGrid.SetZoomFactor( i : integer );
begin
  if (i < 0) then i := 0;
  if (i > MAXZOOMFACTOR) then i:= MAXZOOMFACTOR;
  FZoomFactor := i;
  FillZmDim;

  CalcScreen( Width, Height );
  Paint;
end;

procedure TMapGrid.SetTileWidth( i : integer );
begin
  if (i < 8) then i := 8;
  if (i > 32) then i := 32;
  FTileWidth := i;
  FillZmDim;
end;

procedure TMapGrid.SetTileHeight( i : integer );
begin
  if (i < 8) then i := 8;
  if (i > 32) then i := 32;
  FTileHeight := i;
  FillZmDim;
end;

procedure TMapGrid.SetTileBounds(TileWidth, TileHeight : integer);
var PreFreeze : boolean;
begin
  PreFreeze := Freezed;
  Freezed := True;
  try
    Self.TileWidth := TileWidth;
    Self.TileHeight := TileHeight;
  finally
    Freezed := PreFreeze;
  end;
end;

procedure TMapGrid.BeforeSelect(x,y : integer;Button: TMouseButton; var FullRefresh, Continue: boolean);
begin
  if Assigned(FBeforeSelect) then FBeforeSelect(Self, x, y, Button, FullRefresh, Continue);
end;

procedure TMapGrid.AfterSelect;
begin
  if Assigned(FAfterSelect) then FAfterSelect(Self);
end;

procedure TMapGrid.Loaded;
begin
  with Canvas.Font do
  begin
    Height := -11;
    Pitch := fpFixed;
    Name := 'MS Sans Serif';
  end;
end;



procedure TMapGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var wdth, hght : integer;
begin

  (* fill opt *)
  wdth := FZmWidth;
  hght := FZmHeight;

  case Button of

    mbLeft:
    begin
      (* left button handling *)
      FMouseMode := mmSelecting;
      SetCapture( Self.Handle);
      SetSelZone( ((x-(FIXEDWIDTH+1)) div (wdth)) + FHorScroll.Position,
                  ((y-(FIXEDHEIGHT+1)) div (hght)) + FVerScroll.Position );
    end;

    else
    begin
      (* Right button handling *)
      FMouseMode := mmNone;
      SetSelectedTile(((x-(FIXEDWIDTH+1)) div (wdth)) + FHorScroll.Position,
                     ((y-(FIXEDHEIGHT+1)) div (hght)) + FVerScroll.Position,
                     Button );
    end;
  end;

end;


procedure TMapGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    FMouseMode := mmNone;
    ReleaseCapture;
  end;
end;

procedure TMapGrid.WMMouseMove(var Message: TWMMouseMove);
var Button : TMouseButton;
var i,j : integer;
    wdth, hght : integer;
begin

  (* fill opt *)
  wdth := FZmWidth;
  hght := FZmHeight;

  if ((Message.Keys and MK_LBUTTON)<>0) then
  begin

    (* left button handling *)
    if (FMouseMode = mmSelecting) then
    begin

      (* handle 'outside-bounds' moving *)
      if (Message.xPos < (FIXEDWIDTH+1)) then
      begin
        FHorScroll.Position := FHorScroll.Position-1;
        Message.xPos := (FIXEDWIDTH+1);
      end
      else
        if (Message.xPos > (Width-1)) then
        begin
          FHorScroll.Position := FHorScroll.Position+1;
          Message.xPos := (Width-1);
        end;

      if (Message.yPos < (FIXEDHEIGHT+1)) then
      begin
        FVerScroll.Position := FVerScroll.Position-1;
        Message.yPos := (FIXEDHEIGHT+1);
      end
      else
        if (Message.yPos > (Height-1)) then
        begin
          FVerScroll.Position := FVerScroll.Position+1;
          Message.yPos := (Height-1);
        end;

      (* is change required ? *)
      i := ((Message.xPos-(FIXEDWIDTH+1)) div (wdth)) + FHorScroll.Position;
      j := ((Message.yPos-(FIXEDHEIGHT+1)) div (hght)) + FVerScroll.Position;
      if ((i <> FSelCacheX) or (j <> FSelCacheY)) then
      begin
        FSelCacheX := i;
        FSelCacheY := j;

        if (FMouseMode = mmSelecting) then
          AddSelZone( i, j);
      end;
    end;

  end
  else
  if ((Message.Keys and MK_RBUTTON)<>0) then
    begin

      (* Right button handling *)

      i := ((Message.xPos-(FIXEDWIDTH+1)) div (wdth)) + FHorScroll.Position;
      j := ((Message.yPos-(FIXEDHEIGHT+1)) div (hght)) + FVerScroll.Position;
      if ((i <> FSelZoneLeft) or (j <> FSelZoneTop)) then
      begin
        Button := mbRight;
        MouseDown(Button, [], Message.xPos, Message.yPos);
      end;
    end;
end;



procedure TMapGrid.OnScrollChange(Sender: TObject);
begin
  Paint;
end;

procedure TMapGrid.PaintSelected;
begin
  DrawTiles;
end;


procedure TMapGrid.DrawTiles;
const s1 : string = 'No tileset selected.';
const s2 : string = 'Goto File, Map properties to select a tileset.';

var i,j,k,l,m : integer;
    PColors   : PGBColorType;
    PTile     : PTileType;
    x,y       : integer;
    Size      : TSize;
    r         : TRect;
    iCol, iSelCol : PGBPropColorSets;
    ClrIndex : integer;

    MapWidth        : integer;
    HorFlip,VerFlip : integer;
    HorBase,VerBase : integer;
    CGB             : boolean;
    TileCnt         : integer;
    pal             : integer;
begin

  (* draw tiles *)
  if assigned(FTileData) and (FTileData.Count > 0) and assigned(GBColorController) then
  begin

    iCol     := GBColorController.CurShadowColorSets;
    iSelCol  := GBColorController.CurShadowSelColorSets;
    ClrIndex := GBColorController.ColorSetIndex;
    CGB      := GBColorController.GBCSelected;
    MapWidth := FMapData.Width;
    TileCnt  := FTileData.Count;

    if (FZoomFactor > 0) then
    begin
      for i := 0 to FShadowCanvas.Height div (FZoomFactor*FTileHeight) do
        for j :=0  to FShadowCanvas.Width div (FZoomFactor*FTileWidth) do
        begin
          y := FVerScroll.Position+i;
          x := FHorScroll.Position+j;

          if (y >= FMapData.Height) or (x >= FMapData.Width) then
            FShadowCanvas.RectangleSized( (j * FTileWidth) * FZoomFactor,
                                          (i * FTileHeight) * FZoomFactor,
                                          FTileWidth * FZoomFactor,
                                          FTileHeight * FZoomFactor,
                                          GetSysColor(COLOR_3DFACE))
          else
          begin

            with FMapData.Data[(y*MapWidth) + x] do
            begin
              if (Tile < TileCnt) then
              begin
                PTile := FTileData.Items[Tile];

                (* determine required palette *)
                if (CGB = True) then
                  pal := (Palette and $0F)
                else
                  pal := (Palette and $0700) shr 8;

                if (pal = 0) then
                  pal := PTile.ColorSet[ClrIndex]
                else
                  Dec(pal);


                if ((x >= FSelZoneLeft) and (x <= FSelZoneRight)) and
                   ((y >= FSelZoneTop) and (y <= FSelZoneBottom)) then
                begin
                  if (FPropColors) then
                    PColors := @iSelCol[PropCol, pal]//PTile.ColorSet[ClrIndex]]
                  else
                    PColors := @iSelCol[0, pal];//PTile.ColorSet[ClrIndex]];
                end
                else
                  if (FPropColors) then
                    PColors := @iCol[PropCol, pal]//PTile.ColorSet[ClrIndex]]
                  else
                    PColors := @iCol[0, pal];//PTile.ColorSet[ClrIndex]];

                if (CGB=True) and ((Flags and $10)<>0) then
                begin
                  VerBase := ((j+1) * FTileWidth)-1;
                  VerFlip := -1;
                end
                else
                begin
                  VerBase := (j * FTileWidth);
                  VerFlip := 1;
                end;

                if (CGB=True) and ((Flags and $20)<>0) then
                begin
                  HorBase := ((i+1) * FTileHeight)-1;
                  HorFlip := -1;
                end
                else
                begin
                  HorBase := (i * FTileHeight);
                  HorFlip := 1;
                end;


                for k := 0 to FTileHeight-1 do
                  for l := 0 to FTileWidth-1 do
                    FShadowCanvas.RectangleSized( (VerBase + (l * VerFlip)) * FZoomFactor,
                                                  (HorBase + (k * HorFlip)) * FZoomFactor,
                                                  FZoomFactor, FZoomFactor,
                                                  PColors[PTile.data[k,l]] );
              end;
            end;
          end;
        end;
    end
    else
    begin
      (* 25 % *)
      for i := 0 to (FShadowCanvas.Height div (FTileHeight div 2)) do
        for j :=0  to (FShadowCanvas.Width div (FTileWidth div 2)) do
        begin
          y := FVerScroll.Position+i;
          x := FHorScroll.Position+j;

          if (y >= FMapData.Height) or (x >= FMapData.Width) then
            FShadowCanvas.RectangleSized( (j * (FTileWidth div 2)),
                                          (i * (FTileHeight div 2)),
                                          FTileWidth,
                                          FTileHeight,
                                          GetSysColor(COLOR_3DFACE))
          else
          begin

            with FMapData.Data[(y*MapWidth) + x] do
            begin
              PTile := FTileData.Items[Tile];
              if ((x >= FSelZoneLeft) and (x <= FSelZoneRight)) and
                 ((y >= FSelZoneTop) and (y <= FSelZoneBottom)) then
              begin
                if (FPropColors) then
                  PColors := @iSelCol[PropCol,PTile.ColorSet[ClrIndex]]
                else
                  PColors := @iSelCol[0,PTile.ColorSet[ClrIndex]];
              end
              else
                if (FPropColors) then
                  PColors := @iCol[PropCol,PTile.ColorSet[ClrIndex]]
                else
                  PColors := @iCol[0,PTile.ColorSet[ClrIndex]];


              if (CGB=True) and ((Flags and $20)<>0) then
              begin
                VerBase := ((j+1) * (FTileWidth div 2))-1;
                VerFlip := -1;
              end
              else
              begin
                VerBase := (j * (FTileWidth div 2));
                VerFlip := 1;
              end;

              if (CGB=True) and ((Flags and $10)<>0) then
              begin
                HorBase := ((i+1) * (FTileHeight div 2))-1;
                HorFlip := -1;
              end
              else
              begin
                HorBase := (i * (FTileHeight div 2));
                HorFlip := 1;
              end;
            end;

            for k := 0 to (FTileHeight div 2)-1 do
              for l := 0 to (FTileWidth div 2)-1 do
                FShadowCanvas.SetPixel( (VerBase + (l * VerFlip)),
                                              (HorBase + (k * HorFlip)),
                                              PColors[PTile.data[k*2,l*2]] );
          end;
        end;

    end;




    if FGrid then
    begin
      k := FZmWidth;
      l := FZmHeight;
      y := (FShadowCanvas.Height div l);
      x := (FShadowCanvas.Width div k);

      if ((FVerScroll.Position + y) >= FMapData.Height) then
        m := (FMapData.Height - FVerScroll.Position) * l
      else
        m := FSHadowCanvas.Height;

      if ((FHorScroll.Position + x) >= FMapData.Width) then
        j := (FMapData.Width - FHorScroll.Position) * k
      else
        j := FSHadowCanvas.Width;

      for i := 1 to (j div k) do
        FShadowCanvas.VerLine( (i * k)-1, 0, m, clBlack);

      for i := 1 to (m div l) do
        FShadowCanvas.HorLine( 0, (i * l)-1, j, clBlack);
    end;



    DrawDoubleMarkers;



    FShadowCanvas.Draw(Canvas.Handle, FIXEDWIDTH+1, FIXEDHEIGHT+1 );
  end
  else
  begin

    (* tell user to select a tileset *)
    with Canvas do
    begin
      Pen.Color := clBtnFace;
      Brush.Color := clBtnFace;

      Rectangle( FIXEDWIDTH+1, FIXEDHEIGHT+1,
                 FShadowCanvas.Width + FIXEDWIDTH + 1, FShadowCanvas.Height + FIXEDHEIGHT + 1);

      Pen.Color := clBtnText;

      Size := TextExtent( s1 );
      i := FIXEDHEIGHT+20;
      r := Rect(FIXEDWIDTH+1, i, FShadowCanvas.Width+FIXEDWIDTH+1, i + Size.cy);
      TextRect(r,FIXEDWIDTH +((FShadowCanvas.Width-Size.cx) div 2),i, s1 );

      Size := TextExtent( s2 );
      i := i + Size.cy + 4;
      r := Rect(FIXEDWIDTH+1, i, FShadowCanvas.Width+FIXEDWIDTH+1, i + Size.cy);
      TextRect(r,FIXEDWIDTH +((FShadowCanvas.Width-Size.cx) div 2),i, s2 );
    end;


  end;
end;


procedure TMapGrid.DrawSelTile( b : boolean );
var k,l : integer;
    PColors   : PGBColorType;
    PTile      : PTileType;
    wdth, hght : integer;
    MapWidth : integer;

    HorFlip,VerFlip : integer;
    HorBase,VerBase : integer;
    x,y : integer;

begin

  (* fill opt *)
  wdth := FZmWidth;
  hght := FZmHeight;

  if Assigned(FMapData) and
     Assigned(FTileData) and (FTileData.Count > 0) and
     (FSelZoneLeft >= FHorScroll.Position) and
     (FSelZoneLeft <= FHorScroll.Position + (FShadowCanvas.Width div (wdth))) and
     (FSelZoneTop >= FVerScroll.Position) and
     (FSelZoneTop <= FVerScroll.Position + (FShadowCanvas.Height div (hght))) then
  begin

    MapWidth := FMapData.Width;
    PTile := FTileData.Items[FMapData.Data[(FSelZoneTop*MapWidth) + FSelZoneLeft].Tile];

    if b then
    begin
      if (FPropColors) then
        PColors := @GBColorController.CurShadowSelColorSets[FMapData.Data[(FSelZoneTop*MapWidth) + FSelZoneLeft].PropCol, PTile.ColorSet[GBColorController.ColorSetIndex]]
      else
        PColors := @GBColorController.CurShadowSelColorSets[0, PTile.ColorSet[GBColorController.ColorSetIndex]];
    end
    else
      if (FPropColors) then
        PColors := @GBColorController.CurShadowColorSets[FMapData.Data[(FSelZoneTop*MapWidth) + FSelZoneLeft].PropCol, PTile.ColorSet[GBColorController.ColorSetIndex]]
      else
        PColors := @GBColorController.CurShadowColorSets[0, PTile.ColorSet[GBColorController.ColorSetIndex]];

    if (FZoomFactor > 0) then
    begin
      for k := 0 to FTileHeight-1 do
        for l := 0 to FTileWidth-1 do
          FShadowCanvas.RectangleSized( (l * FZoomFactor), (k * FZoomFactor), FZoomFactor, FZoomFactor, PColors[PTile.data[k,l]] );
    end
    else
    begin
      for k := 0 to (FTileHeight div 2)-1 do
        for l := 0 to (FTileWidth div 2)-1 do
          FShadowCanvas.SetPixel( l, k, PColors[PTile.data[(k*2),(l*2)]] );
    end;

    if FGrid then
    begin
      FShadowCanvas.VerLine(wdth-1,0, hght, clblack );
      FShadowCanvas.HorLine(0,hght-1, wdth, clblack );
    end;

    DrawDoubleMarkers;

    x := ((FSelZoneLeft-FHorScroll.Position) * wdth);
    y := ((FSelZoneTop-FVerScroll.Position) * hght);
    if ((FShadowCanvas.Width-x) < wdth)  then wdth := FShadowCanvas.Width-x;
    if ((FShadowCanvas.Height-y) < hght) then hght := FShadowCanvas.Height-y;

    FShadowCanvas.DrawPart( Canvas.Handle, (FIXEDWIDTH+1) + x,
                            (FIXEDHEIGHT+1) + y, wdth, hght );
  end;
end;



procedure TMapGrid.Paint;
begin

  if (not FFreezed) and Assigned(FTileData) and Assigned(FMapData) then
  begin
    with Canvas do
    begin

      (* inner lining *)
      Pen.Color := cl3DDkShadow;
      MoveTo(FIXEDWIDTH,FShadowCanvas.Height+FIXEDHEIGHT);
      LineTo(FIXEDWIDTH, FIXEDHEIGHT);
      LineTo(FShadowCanvas.Width + FIXEDWIDTH,FIXEDHEIGHT);

      (* Outside frame *)
      Pen.Color := clBtnShadow;
      MoveTo(0,Height-1);
      LineTo(0,0);
      LineTo(Width-1,0);

      Pen.Color := clBtnHighLight;
      LineTo(Width-1, Height-1);
      LineTo(0,Height-1);

      (* Left-Top box *)
      Pen.Color := clBtnShadow;
      MoveTo(1,FIXEDHEIGHT-1);
      LineTo(FIXEDWIDTH-1,FIXEDHEIGHT-1);
      LineTo(FIXEDWIDTH-1,1);

      Pen.Color := clBtnHighLight;
      LineTo(1, 1);
      LineTo(1,FIXEDHEIGHT-1);

      Brush.Color := clBtnFace;
      Pen.Color := clBtnFace;
      Rectangle(2, 2, FIXEDWIDTH-1, FIXEDHEIGHT-1);

      (* Right-Bottom box *)
      if FScrollsShowing then
        Rectangle(FHorScroll.Width+2, FVerScroll.Height+2, Width-1, Height-1);

    end;

    DrawTiles;
    DrawFixed;

  end;

end;




procedure TMapGrid.CalcScreen( w, h  : integer);
var Wdt, Hgt : integer;
    ShowVer, ShowHor : boolean;
    i  : integer;
    wdth, hght : integer;

begin

  if Assigned(FMapData) then
  begin
    Wdt := w-1;
    Hgt := h-1;

    (* fill opt *)
    wdth := FZmWidth;
    hght := FZmHeight;


    (* determine ScrollBars *)
    ShowVer := False;
    ShowHor := False;
    i := (FMapData.Width * wdth)+FIXEDWIDTH+2;
    if i > Wdt then
    begin
      Hgt := Hgt - FHorScroll.Height - 1;
      ShowHor := True;
    end;
    if (FMapData.Height * hght)+FIXEDHEIGHT+2 > Hgt then
    begin
      Wdt :=  Wdt - FVerScroll.Width - 1;
      ShowVer := True;
      if (not ShowHor) and (i > Wdt) then
      begin
        Hgt := Hgt - FHorScroll.Height - 1;
        ShowHor := True;
      end;
    end;

    if (ShowVer) then FVerScroll.SetBounds(Wdt+1, 1, FVerScroll.Width, Hgt-1)
    else FVerScroll.Position := 0;
    if (ShowHor) then FHorScroll.SetBounds(1, Hgt+1, Wdt-1, FHorScroll.Height)
    else FHorScroll.Position := 0;

    FVerScroll.Visible := ShowVer;
    FHorScroll.Visible := ShowHor;
    FScrollsShowing := ShowHor and ShowVer;


    (* setup shadowcanvas *)
    FShadowCanvas.Free;
    FShadowCanvas := TShadowCanvas.Create(wdt-(FIXEDWIDTH+1), hgt-(FIXEDHEIGHT+1) );


    (* Init scrollbars *)
    FVerScroll.LargeChange := (hgt-(FIXEDHEIGHT+1)) div (hght);
    FHorScroll.LargeChange := (wdt-(FIXEDWIDTH+1)) div (wdth);


    (* setup fixed cells *)
    BuildFixedRow;
    BuildFixedCol;
  end;
end;


procedure TMapGrid.BuildFixedRow;
var i,j,k,l : integer;
    s : string;
    r : TRect;
    wdth, hght : integer;
    TileCnt, TileStart : integer;

  procedure DrawHorLines;
  begin
    with FFixedRow.canvas do
    begin
      Pen.Color := cl3DDkShadow;
      MoveTo(j,0);
      LineTo(j,FIXEDHEIGHT);

      Pen.Color := clBtnShadow;
      MoveTo(j-1,1);
      LineTo(j-1,FIXEDHEIGHT-1);

      Pen.Color := clBtnHighlight;
      MoveTo(j+1,1);
      LineTo(j+1,FIXEDHEIGHT-2);
    end;
  end;



begin

  if Assigned(FMapData) then
  begin


    (* fill opt *)
    wdth := FZmWidth;
    hght := FZmHeight;

     (* Fixed row *)
    FRowCached := (((FMapData.Width-1) * wdth) < MAXFIXEDSIZE);
    if FRowCached then
    begin
      TileCnt := FMapData.Width-1;
      TileStart := 0;
    end
    else
    begin
      TileStart := FHorScroll.Position;
      TileCnt := (1 + FShadowCanvas.Width div (FZoomFactor*FTileWidth));
      if ((TileStart + TileCnt) > FMapData.Width) then TileCnt := (FMapData.Width - TileStart)-1;
    end;
    FFixedRow.Width := (TileCnt * wdth) + FShadowCanvas.Width;

    with FFixedRow.canvas do
    begin

      (* draw block-independent *)
      Brush.Color := clBtnFace;
      FillRect(Rect(((TileCnt) * wdth)+1,1, FFixedRow.Width-1, FIXEDHEIGHT-2));

      Pen.Color := clBtnHighLight;
      MoveTo(0, 0);
      LineTo(FFixedRow.Width-1,0);

      Pen.Color := clBtnShadow;
      MoveTo(0, FIXEDHEIGHT-2);
      LineTo(FFixedRow.Width-1,FIXEDHEIGHT-2);

      (* draw block-dependent *)
      l := (wdth)-5;      // available room for text
      j := 0;
      for i := TileStart to (TileStart + TileCnt) do
      begin

        DrawHorLines;


        (* Draw text *)
        s := IntToStr(i);
        k := TextWidth(s);

        if (k >= l) then k := j+2 else k := j + 2 + ((1 + l - k) div 2);
        r := Rect(j+2, 1, j+(wdth), FIXEDHEIGHT-2);
        if (FZoomfactor > 0) then
          TextRect(r, k, 1, s)
        else
        begin
          Rectangle(r.Left-1, r.Top-1, r.Right+1, r.Bottom+1);
          Pen.Color := clBtnShadow;
          MoveTo(r.Left-1, r.Bottom);
          LineTo(r.Right, r.Bottom);
        end;


        j := j + (wdth);
      end;
      DrawHorLines;
    end;

  end;
end;



procedure TMapGrid.BuildFixedCol;
var i,j,k,l : integer;
    s : string;
    r : TRect;
    wdth, hght : integer;
    TileCnt, TileStart : integer;

  procedure DrawVerLines;
  begin
    with FFixedCol.canvas do
    begin
      Pen.Color := cl3DDkShadow;
      MoveTo(0,j);
      LineTo(FIXEDWIDTH,j);

      Pen.Color := clBtnShadow;
      MoveTo(0,j-1);
      LineTo(FIXEDWIDTH-1,j-1);

      Pen.Color := clBtnHighlight;
      MoveTo(1,j+1);
      LineTo(FIXEDWIDTH-1,j+1);
    end;
  end;


begin

  if Assigned(FMapData) then
  begin


    (* fill opt *)
    wdth := FZmWidth;
    hght := FZmHeight;



    (* Fixed col *)
    FColCached := (((FMapData.Height-1) * hght) < MAXFIXEDSIZE);

    if FColCached then
    begin
      TileCnt := FMapData.Height-1;
      TileStart := 0;
    end
    else
    begin
      TileStart := FVerScroll.Position;
      TileCnt := (1 + FShadowCanvas.Height div (FZoomFactor*FTileHeight));
      if ((TileStart + TileCnt) > FMapData.Height) then TileCnt := (FMapData.Height - TileStart)-1;
    end;
    FFixedCol.Height := (TileCnt * hght) + FShadowCanvas.Height;

    with FFixedCol.canvas do
    begin

      (* draw block-independent *)
      Brush.Color := clBtnFace;
      FillRect(Rect(1, ((TileCnt) * hght)+1, FIXEDWIDTH-2, FFixedCol.Height-1));

      Pen.Color := clBtnHighLight;
      MoveTo(0, 0);
      LineTo(0, FFixedCol.Height);

      Pen.Color := clBtnShadow;
      MoveTo(FIXEDWIDTH-2, 0);
      LineTo(FIXEDWidth-2, FFixedCol.Height-1);


      (* draw block-dependent *)
      l := FIXEDWIDTH-4;      // available room for text
      j := 0;
      for i := TileStart to (TileStart+TileCnt) do
      begin
        DrawVerLines;

        (* Draw text *)
        s := IntToStr(i);
        k := TextWidth(s);
        if (k >= l) then k := 2 else k := 2 + ((l - k) div 2);
        r := Rect(1, j+2, FIXEDWIDTH-2, j+(hght));

        if (FZoomfactor > 0) then
          TextRect(r, k, j + ((hght) div 2) - 6,  s)
        else
        begin
          Rectangle(r.Left-1, r.Top-1, r.Right+1, r.Bottom+1);
          Pen.Color := clBtnShadow;
          MoveTo(r.Right, r.Top);
          LineTo(r.Right, r.Bottom);
        end;

        j := j + (hght);
      end;
      DrawVerLines;

    end;

  end;
end;







procedure TMapGrid.DrawFixed;
var r,s : TRect;
    wdth, hght : integer;
begin

  GDIFlush;

    (* fill opt *)
    wdth := FZmWidth;
    hght := FZmHeight;

  r := Rect(FIXEDWIDTH, 1, FShadowCanvas.Width+FIXEDWIDTH+1, FIXEDHEIGHT);

  with s do
  begin
    if FRowCached then
      Left   := FHorScroll.Position * (wdth)
    else
    begin
      BuildFixedRow;
      Left   := 0;
    end;
    Top    := 0;
    Right  := Left + r.Right - FIXEDWIDTH;
    Bottom := FIXEDHEIGHT-1;
  end;
  Canvas.CopyRect(r, FFixedRow.Canvas, s );


  r := Rect(1, FIXEDHEIGHT, FIXEDWIDTH, FShadowCanvas.Height + FIXEDHEIGHT+1);
  with s do
  begin
    if FColCached then
      Top := FVerScroll.Position * (hght)
    else
    begin
      BuildFixedCol;
      Top   := 0;
    end;

    Left := 0;
    Right := FIXEDWIDTH-1;
    Bottom := Top + r.Bottom - FIXEDHEIGHT;
  end;
  Canvas.CopyRect(r, FFixedCol.Canvas, s);
end;


procedure TMapGrid.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TMapGrid.WMSize(var Message: TWMSize);
begin
  CalcScreen( Message.Width, Message.Height );
end;


procedure TMapGrid.DrawDoubleMarkers;
var i,j,k,l,m,n : integer;
    size : integer;
begin
  if FDoubleMarkers then
  begin

    if (FZoomFactor > 0) then size  := 8 * FZoomFactor else size  := 8 div 2;

    m := ((FMapData.Width - FHorScroll.Position) * FTileWidth - 8) div 16;
    n := ((FMapData.Height - FVerScroll.Position) * FTileHeight - 8) div 16;

    k := -1;
    if (TileHeight = 8) then
    begin
      k := k - ((FVerScroll.Position mod 2) * 8 * FZoomFactor);
    end;

    l := -1;
    if (TileWidth = 8) then l := l - ((FHorScroll.Position mod 2) * size);

    size := size * 2;
    for i := 1 to n do
      for j := 1 to m do
      begin
        FShadowCanvas.HorLine( (j * size) + l - 1, (i * size) + k, 3, clBlue);
        FShadowCanvas.VerLine( (j * size) + l, (i * size) + k - 1, 3, clBlue);
      end;
  end;
end;



procedure TMapGrid.SetSelZone(x,y : integer);
var FF,cont : boolean;
begin

  FF := False;                    (*ignored *)
  Cont := True;
  BeforeSelect(x, y, mbLeft,FF, Cont);

  if Cont then
  begin
    FSelZoneLeft   := x;
    FSelZoneRight  := x;
    FSelZoneTop    := y;
    FSelZoneBottom := y;

    FSelStartX := x;
    FSelStartY := y;

    VerifySelBoundaries;
    Paint;
    AfterSelect;
  end;
end;

procedure TMapGrid.AddSelZone(x,y : integer);
var FF,cont : boolean;
begin

  FF := False;                    (*ignored *)
  Cont := True;
  BeforeSelect(x, y, mbLeft,FF, Cont);

  if Cont then
  begin

    if ( x < FSelStartX) then
    begin
      FSelZoneLeft := x;
      FSelZoneRight := FSelStartX;
    end
    else
    begin
      FSelZoneRight := x;
      FSelZoneLeft := FSelStartX;
    end;

    if ( y < FSelStartY) then
    begin
      FSelZoneTop := y;
      FSelZoneBottom := FSelStartY;
    end
    else
    begin
      FSelZoneBottom := y;
      FSelZoneTop := FSelStartY;
    end;

    VerifySelBoundaries;
    Paint;
    AfterSelect;
  end;
end;

procedure TMapGrid.VerifySelBoundaries;
begin
  if (FSelZoneLeft >= FMapData.Width) then FSelZoneLeft := FMapData.Width-1;
  if (FSelZoneRight >= FMapData.Width) then FSelZoneRight := FMapData.Width-1;

  if (FSelZoneTop >= FMapData.Height) then FSelZoneTop := FMapData.Height-1;
  if (FSelZoneBottom >= FMapData.Height) then FSelZoneBottom := FMapData.Height-1;
end;

end.
