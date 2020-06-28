unit GBMBMain;

interface

uses Classes, StdCtrls, SysUtils, gbofile, Forms, gbconst,
     Controls, ColorController, graphics, Clipbrd, ExtCtrls,
     mainlib, GBColorCombo;






procedure BuildInfoPanel;
procedure RefreshMap;
procedure FullRefresh;
procedure RefreshInfoPanel;
procedure RefreshList;
procedure SetTileFile( f : string );
procedure FreeTileFileObserver;

procedure SetExchangerActive( b : boolean);

procedure SetTitle(const s : string);
procedure SetTileSize(x,y : integer);
procedure SetInfoPanel( v : boolean);
procedure SetDrawStyle( s : TGBPenStyle);
procedure SetColorSet(i : integer);

procedure SetTileOfLocation(x,y,i: integer);
procedure PokeTileOfLocation(x,y,i: integer);
function GetTileOfLocation(x,y : integer): integer;
procedure SetPropOfLocation(x,y,p,i: integer);
function GetPropOfLocation(x,y,p: integer): integer;
procedure SetFlipOfLocation(x,y : integer; Hor,Value : boolean);
function GetFlipOfLocation(x,y : integer; Hor : boolean): boolean;

procedure CopyTile( Srcx, Srcy, Destx, Desty : integer);

procedure FreezeAll;
procedure UnFreezeAll;

procedure CalcAllPropColors;

procedure CopyAsBitmap;

function GetRelativePath(const p : string ): string;
procedure ResetBaseDir;

procedure SetPalOfLocation (x,y,clr : integer; GBC : boolean);
function GetPalOfLocation(x,y : integer; GBC : boolean): integer;
function GetPalIndexOfLocation(x,y : integer; GBC : boolean): integer;

implementation

Uses Map, TileFileObserver, files, MapUndo, Repository;

{--$I ..\hlp\gbmb.inc}


const LEFTMARGIN = 5;
      TOPMARGIN  = 23;
      ROWHEIGHT = 20;//22;
      LBLWIDTH  = 100;
      EDWIDTH  = 38;
      SPACING  = 15;


      //EmptyPropRec : RMapRec = (Tile:0;PropCol:0;Prop:(0,0,0,0,0,0,0));

var FTileFileObserver : TTileFileObserver;

procedure SetExchangerActive( b : boolean);
begin
  TileExchanger.Active := b;
  FrmMap.BtnAutoUpdate.Down := b;
//  FrmMap.BtnAutoUpdate.KludgeKeepColor;    (* !! fooling delphi !! *)
  Modified := True;
end;

procedure ResetBaseDir;
begin
{$I-}
//  if (BaseDir <> '') then ChDir(BaseDir);
end;

function GetPropOfLocation(x,y,p: integer): integer;
begin
  with TileMap do result := PropData[p][(y*Width) + x];
end;

procedure SetPropOfLocation(x,y,p,i: integer);
begin
  with TileMap do PropData[p][(y * Width) + x] := i;
end;




procedure DeterminePropColor( x,y : integer );
var p,i : integer;
begin
  p := 0;
  for i := 0 to 1 do
    with PropColors.Colors[i] do
      if (Prop >= 0) and (TileMap.PropCount > Prop) then
        case Operator of
          0  : if (TileMap.PropData[Prop][(y*TileMap.Width) + x] = Value) then p := i+1;
          1  : if (TileMap.PropData[Prop][(y*TileMap.Width) + x] <> Value) then p := i+1;
          2  : if (TileMap.PropData[Prop][(y*TileMap.Width) + x] < Value) then p := i+1;
          3  : if (TileMap.PropData[Prop][(y*TileMap.Width) + x] > Value) then p := i+1;
          4  : if (TileMap.PropData[Prop][(y*TileMap.Width) + x] <= Value) then p := i+1;
          5  : if (TileMap.PropData[Prop][(y*TileMap.Width) + x] >= Value) then p := i+1;
        end;
  TileMap.Data[(y*TileMap.Width) + x].PropCol := p;
end;


procedure CalcAllPropColors;
var i,j : integer;
begin
  for i:= 0 to TileMap.Height-1 do
    for j :=0 to TileMap.Width-1 do
      DeterminePropColor(j,i);
end;


procedure CopyTile( Srcx, Srcy, Destx, Desty : integer);
var i : integer;
begin
  with Tilemap do
  begin
    (* set Tile *)
    Data[(Desty * Width) + Destx] := Data[(Srcy * Width) + Srcx];

    (* set properties *)
    for i := 0 to PropCount-1 do
      SetPropOfLocation( Destx, Desty, i, GetPropOfLocation(Srcx, Srcy, i) );
  end;
end;

procedure SetTileOfLocation(x,y,i: integer);
var j : integer;
begin
  MapUndoCtrl.AddUndo(x,y, TileMap.Data[(y*TileMap.Width) + x]);

  (* set Tile *)
  TileMap.Data[(y*TileMap.Width) + x].Tile := i;

  (* set properties *)
  for j := 0 to TileMap.PropCount-1 do
    SetPropOfLocation(x,y, j, TileMap.PropInitVal[j][i] );

  (* set flips *)
  SetFlipOfLocation(x,y, False, False);
  SetFlipOfLocation(x,y, True, False);

  (* reset palettes *)
  TileMap.Data[(y*TileMap.Width) + x].Palette := 0;

  DeterminePropColor(x,y);

  Modified := True;
end;

procedure PokeTileOfLocation(x,y,i: integer);
var j : integer;
begin
  (* set Tile *)
  TileMap.Data[(y*TileMap.Width) + x].Tile := i;

  (* set properties *)
  for j := 0 to TileMap.PropCount-1 do
    SetPropOfLocation(x,y, j, TileMap.PropInitVal[j][i] );

  (* set flips *)
  SetFlipOfLocation(x,y, False, False);
  SetFlipOfLocation(x,y, True, False);

  (* reset palettes *)
  TileMap.Data[(y*TileMap.Width) + x].Palette := 0;
end;


procedure SetFlipOfLocation(x,y : integer; Hor,Value : boolean);
var b,m: byte;
begin
  with TileMap.Data[(y*TileMap.Width) + x] do
  begin
    b := Flags;
    if Hor then m := $10 else m := $20;
    if Value then
      b := b or m
    else
    begin
      m := m xor $FF;
      b := b and m;
    end;

    Flags := b;
  end;
end;

function GetFlipOfLocation(x,y : integer; Hor : boolean): boolean;
var b,m: byte;
begin
  b := TileMap.Data[(y*TileMap.Width) + x].Flags;
  if Hor then m := $10 else m := $20;
  result := ((b and m) <> 0);
end;

function GetPalIndexOfLocation(x,y : integer; GBC : boolean): integer;
var i,j : integer;
begin
  if (not GBC) then
  begin
    Result := (TileMap.Data[(y*TileMap.Width) + x].Palette and $0700) SHR 8;
    j := 4;
    i := 1;
  end
  else
  begin
    Result := (TileMap.Data[(y*TileMap.Width) + x].Palette and $000F);
    j := 8;
    i := 0;
  end;

  (* modify for default *)
  if (Result = 0) then
    result := PTileType(TileList.Items[TileMap.Data[(y*TileMap.Width) + x].Tile]).ColorSet[i]
  else
    Dec(Result);
end;


function GetPalOfLocation(x,y : integer; GBC : boolean): integer;
var j : integer;
begin
  if (not GBC) then
  begin
    Result := (TileMap.Data[(y*TileMap.Width) + x].Palette and $0700) SHR 8;
    j := 4;
  end
  else
  begin
    Result := (TileMap.Data[(y*TileMap.Width) + x].Palette and $000F);
    j := 8;
  end;

  (* modify for default *)
  if (Result = 0) then
    Result := j
  else
    Dec(Result);
end;

procedure SetPalOfLocation (x,y,clr : integer; GBC : boolean);
var pal : word;
begin
  pal := TileMap.Data[(y*TileMap.Width) + x].Palette;
  if (not GBC) then
  begin
    if (clr = 4) then clr := 0 else Inc(clr);
    pal := (pal and $F8FF);
    pal := pal + (clr SHL 8);
  end
  else
  begin
    if (clr = 8) then clr := 0 else Inc(clr);
    pal := (pal and $FFF0);
    pal := pal + clr;
  end;
  TileMap.Data[(y*TileMap.Width) + x].Palette := pal;
end;





function GetTileOfLocation(x,y : integer): integer;
begin
  Result := TileMap.Data[(y*TileMap.Width) + x].Tile;
end;

procedure SetDrawStyle( s : TGBPenStyle);
begin
  with FrmMap do
  begin
    case s of
      gbdsPen  :
      begin
        BtnPenCursor.Down := True;
        Pen1.Checked := True;
        GrdMap.Cursor := crDefault;
      end;
      gbdsFloodFill :
      begin
        BtnFloodFillCursor.Down := True;
        FloodFill1.Checked := True;
        GrdMap.Cursor := crDrag;
      end;
      gbdsDropper :
      begin
        BtnDropperCursor.Down := True;
        Dropper1.Checked := True;
        GrdMap.Cursor := crHandPoint;//crDrag;
      end;
    end;
  end;
//  MapUndoCtrl.Clear;
end;

procedure SetColorSet(i : integer);
var j : integer;
begin
  with FrmMap do
  begin
    if (MnuColorSet.Count <= i) then i := 0;
    for j := 0 to MnuColorSet.Count-1 do
      if mnuColorSet.Items[j].Tag = i then
        MnuColorSet.Items[j].Checked := True;

    with GBColorController do
    begin
      ColorMode := TGBColorMode(i);
      if (i = 3) then ColorSetIndex := 1 else ColorSetIndex := 0;
    end;

    LstTiles.Paint;
    GrdMap.Paint;
    BuildInfoPanel;
  end;

  Modified := True;
end;


procedure SetTileSize(x,y : integer);
begin
  with FrmMap do
  begin
    LstTiles.TileSize := GetTileSize(x,y);
    TileExchanger.TileSize := LstTiles.TileSize;
    GrdMap.TileWidth := x;
    GrdMap.TileHeight := y;
    Resize;
  end;
end;


procedure SetInfoPanel( v : boolean);
begin
  with FrmMap do
  begin
    PnlInfo.Visible := v;
    InfoPanel1.Checked := v;
    Resize;
  end;
  Modified := True;
end;


procedure RefreshMap;
begin
  with FrmMap.GrdMap do
  begin
    MapData := TileMap;
    Refresh;
    VerifySelBoundaries;
  end;
  RefreshInfoPanel;
end;

procedure RefreshList;
begin
  with FrmMap.LstTiles do
  begin
    TileData := TileList;
    Refresh;
  end;
end;


procedure FullRefresh;
begin
  RefreshMap;
  RefreshList;
  BuildInfoPanel;
  RefreshInfoPanel;
end;

procedure FreeTileFileObserver;
begin
  if Assigned(FTileFileObserver) then
  begin
    FTileFileObserver.Terminate;
    FTileFileObserver := nil;
  end;
end;

procedure SetTileFile( f : string );
begin
  TileFileName := f;
  FreeTileFileObserver;

  if (f <> '') then
  begin
    f := ExpandRelativePath(f, BaseDir);
    LoadTileSet(f);
    TileMap.SetTileCount(TileList.Count);
    RefreshList;

    TileExchanger.FileName := f;

    (* setup change observer *)
    FTileFileObserver := TTileFileObserver.Create(False, f, FrmMap);
  end;
end;




procedure RefreshInfoPanel;
var i : integer;
    b : boolean;
begin

  with FrmMap do
  begin
    if (GrdMap.SelZoneLeft <> GrdMap.SelZoneRight) or
       (GrdMap.SelZoneTop <> GrdMap.SelZoneBottom) then
    begin
      (* multiple tiles selected *)
      LblLocation.Caption := Format( '[%d,%d] - [%d,%d]',
                                     [GrdMap.SelZoneLeft, GrdMap.SelZoneTop,
                                      GrdMap.SelZoneRight, GrdMap.SelZoneBottom] );
      for i := 0 to PROPCNT-1 do
        if Assigned(PropEd[i].Ctrl) then
          TEdit(PropEd[i].Ctrl).Text := '';

      if Assigned(ChkVer) then
      begin
        ChkVer.Tag := 1;
        ChkVer.Checked := False;
        ChkHor.Checked := False;
        ChkVer.Tag := 0;
      end;

      if Assigned(ColorCombo) then
        ColorCombo.ItemIndex := ColorCombo.Items.Count-1;
    end
    else
    begin
      (* single tile selected *)
      LblLocation.Caption := Format('[%d,%d] : %d', [GrdMap.SelZoneLeft, GrdMap.SelZoneTop, GetTileOfLocation(GrdMap.SelZoneLeft, GrdMap.SelZoneTop)] );
      for i := 0 to TileMap.PropCount-1 do
        TEdit(PropEd[i].Ctrl).Text := IntToStr(GetPropOfLocation(GrdMap.SelZoneLeft, GrdMap.SelZoneTop, i));

      if Assigned(ChkVer) then
      begin
        ChkVer.Tag := 1;
        ChkVer.Checked := GetFlipOfLocation(GrdMap.SelZoneLeft, GrdMap.SelZoneTop, False );
        ChkHor.Checked := GetFlipOfLocation(GrdMap.SelZoneLeft, GrdMap.SelZoneTop, True );
        ChkVer.Tag := 0;
      end;


      if Assigned(ColorCombo) and assigned(TileList) and (TileList.count >0) then
      begin
        ColorCombo.ItemIndex := GetPalOfLocation(GrdMap.SelZoneLeft, GrdMap.SelZoneTop, (GBColorController.ColorMode <> gbcmSGB));
        ColorCombo.Refresh;
      end;



    end;
  end;
end;




procedure BuildInfoPanel;
var i,j,k : integer;
    Lbl   : TLabel;
    Ed    : TEdit;
    Cnv   : TImage;
    Size, FullSize  : integer;
    x,y   : integer;
    RightDelta : integer;

begin

  RightDelta := 0;

  with FrmMap do
  begin

    PnlInfo.Visible := False;
    try
      (* destroy current controls *)
      for i := 0 to PnlInfo.ControlCount-1 do
        PnlInfo.Controls[0].Free;
      ChkVer     := nil;
      ChkHor     := nil;
      ColorCombo := nil;

      for i := 0 to PROPCNT-1 do PropEd[i].Ctrl := nil;

      (* resize panel *)
      PnlInfo.Width := PnlMain.Width - (33+5);

      (* build standard controls *)
      Lbl := TLabel.Create( FrmMap );
      with Lbl do
      begin
        Parent := FrmMap.PnlInfo;
        Left := 6;
        Top := 3;
        Caption := 'Location';
      end;

      Lbl := TLabel.Create( FrmMap );
      with Lbl do
      begin
        Parent := PnlInfo;
        LblLocation := Lbl;
        Left := 56;
        Top := 3;
      end;

      if (GBColorController.ColorMode in [gbcmGBC, gbcmGBCFiltered]) then
      begin
        ChkHor := TCheckBox.Create( FrmMap );
        with ChkHor do
        begin
          Parent := PnlInfo;
          Caption := '&Horizontal';
          RightDelta := RightDelta + 80;
          SetBounds( PnlInfo.Width - RightDelta, 2, 73, 17);
          OnClick := InfoPanelFlipClick;
          TabOrder := 0;
//          HelpContext := Info_panel;
        end;

        ChkVer := TCheckBox.Create( FrmMap );
        with ChkVer do
        begin
          Parent := PnlInfo;
          Caption := '&Vertical';
          RightDelta := RightDelta + 70;
          SetBounds( PnlInfo.Width - RightDelta, 2, 70, 17);
          OnClick := InfoPanelFlipClick;
          TabOrder := 0;
//          HelpContext := Info_panel;
        end;
      end;

      if (GBColorController.ColorMode in [gbcmGBC, gbcmSGB, gbcmGBCFiltered]) then
      begin
        ColorCombo := TGBColorCombo.Create( FrmMap );
        with ColorCombo do
        begin
          Parent := PnlInfo;
          Width := 83;
          ItemHeight := 13;
          RightDelta := RightDelta + 96;
          Left := PnlInfo.Width - RightDelta;
          Top := 1;
          SelColor := -1;
          TabStop := True;
          TabOrder := 0;
//          HelpContext := Info_panel;
          Refresh;
          AddDefault := True;
          OnChange := FrmMap.ColorComboChange;
          FillDropDown;
        end;

        with TLabel.Create( FrmMap ) do
        begin
          Parent := PnlInfo;
          RightDelta := RightDelta + 22;
          Left := PnlInfo.Width - RightDelta;
          Top := 3;
          Caption := '&Pal';
          FocusControl := ColorCombo;
        end;

      end;

      if ( TileMap.PropCount < 1) then
      begin
        PnlInfo.Height := TOPMARGIN-2;
        RefreshInfoPanel;
      end
      else
      begin

        (* Determine size *)
        Size := 0;
        for i := 0 to TileMap.PropCount-1 do
        begin
          j := Canvas.TextWidth( Tilemap.PropDef[i].Name);
          if (j > Size) then Size := j;
        end;

        FullSize := Size + EDWIDTH + SPACING;
        x := (PnlInfo.Width - LEFTMARGIN - 5) div FullSize;
        y := (TileMap.PropCount) div x;
        if ( ((TileMap.PropCount) mod x) <> 0) then inc(y);
        PnlInfo.Height := TOPMARGIN + (y * ROWHEIGHT) + 3;

        (* setup canvas *)
        Cnv := TImage.Create(FrmMap.PnlInfo);
        with Cnv do
        begin
          Parent := PnlInfo;
          SendToBack;
          SetBounds(2,2, PnlInfo.Width-4, PnlInfo.Height-4);
          Canvas.Brush.Color := clBtnFace;
          Canvas.Pen.Color := clBtnFace;
          Canvas.Rectangle(0,0, Width, Height);
          Canvas.Pen.Color := clBtnShadow;
        end;

        (* Build prop controls *)
        k := 0;
        for i:= 0 to y-1 do
        begin
          for j := 0 to x-1 do
          begin

            if (k < TileMap.PropCount) then
            begin

              (* create a new edit box *)
              Ed := TEdit.Create( FrmMap );
              with Ed do
              begin
                Parent := PnlInfo;
                AutoSize := False;
                SetBounds( LEFTMARGIN + (j * FullSize) + Size + SPACING,
                           TOPMARGIN + (ROWHEIGHT * i), EDWIDTH, 19 );
                MaxLength := 5;
                OnExit := InfoPanelEdExit;
                OnChange := GlobalNumericEdChange;
                OnKeyDown := InfoPanelKeyDown;
//                HelpContext := Info_panel;
              end;
              PropEd[k].Ctrl := Ed;
              PropEd[k].Size := TileMap.PropDef[k].Size;
              PropEd[k].Name := TileMap.PropDef[k].Name;

              Lbl := TLabel.Create( FrmMap );
              with Lbl do
              begin
                Parent := PnlInfo;
                AutoSize := False;
                SetBounds( LEFTMARGIN + 5 + (j * FullSize),
                           TOPMARGIN + (ROWHEIGHT * i) + 2,
                           Size, 13 );

                Caption := TileMap.PropDef[k].Name;
                FocusControl := Ed;
              end;

              inc(k);
            end;
          end;

          if (k < TileMap.PropCount) then
            with Cnv.Canvas do
            begin
              MoveTo(0, TOPMARGIN + (ROWHEIGHT * i) + 19);
              LineTo(Cnv.Width-1, TOPMARGIN + (ROWHEIGHT * i) + 19);
            end;
        end;

        (* draw frame *)
        with Cnv.Canvas do
        begin
          Pen.Color := clBtnShadow;
          MoveTo(0,18);
          Lineto(Cnv.Width, 18);

          Pen.Color := cl3DDkShadow;
          MoveTo(1,19);
          Lineto(Cnv.Width-1, 19);

          Pen.Color := clBtnHighlight;
          Lineto(Cnv.Width-1, Cnv.Height-1 );
          Lineto(0, Cnv.Height-1 );

          Pen.Color := clBtnShadow;
          Lineto(0, 18 );
        end;

        MapMinWidth := RightDelta + 220;
        
        (* fill them up *)
        RefreshInfoPanel;
      end;

    finally
      PnlInfo.Visible := True;
    end;
  end;
end;


function GetRelativePath(const p : string ): string;
begin
  if (StrComp(PChar(UpperCase(ExtractFilePath(p))), PChar(UpperCase(BaseDir))) <> 0) then
    result := p
  else
    result := ExtractFileName(p);
end;


procedure SetTitle(const s : string);
begin
  Application.Title := SHORTTITLE + ' - ' + ExtractFileName(s);
  FrmMap.Caption    := LONGTITLE + ' - ' + ExtractFileName(s);
  BaseDir           := ExtractFilePath(s);
end;


procedure FreezeAll;
begin
  with FrmMap do
  begin
    GrdMap.Freezed := True;
    LstTiles.Freezed := True;
  end;
end;

procedure UnFreezeAll;
begin
  with FrmMap do
  begin
    GrdMap.Freezed := False;
    LstTiles.Freezed := False;
  end;
end;



procedure CopyAsBitmap;
var Pic : TBitmap;
    x,y, sx,sy : integer;
    i,j,k,l : integer;
    p : PTileType;
    VerBase, HorBase, VerFlip, HorFlip : integer;
begin

  Pic := TBitmap.Create;
  try

    (* get size *)
    with FrmMap.GrdMap do
      if (SelZoneLeft <> SelZoneRight) or (SelZoneTop <> SelZoneBottom) then
      begin
        x := SelZoneLeft;
        y := SelZoneTop;
        sx := SelZoneRight+1;
        sy := SelZoneBottom+1;
      end
      else
      begin
        x := 0;
        y := 0;
        sx := TileMap.Width;
        sy := TileMap.Height;
      end;


    with FrmMap,Pic do
    begin
      Pic.Width := (sx-x) * GrdMap.TileWidth;
      Pic.Height := (sy-y) * GrdMap.TileHeight;

      for j := y to sy-1 do
        for i := x to sx-1 do
        begin
          p := PTileType(TileList.Items[TileMap.Data[(j * TileMap.Width) + i].Tile]);

          if GBColorController.GBCSelected and GetFlipOfLocation(i,j,false) then
          begin
            VerBase := GrdMap.TileWidth-1;
            VerFlip := -1;
          end
          else
          begin
            VerBase := 0;
            VerFlip := 1;
          end;

          if GBColorController.GBCSelected and GetFlipOfLocation(i,j, True) then
          begin
            HorBase := GrdMap.TileHeight-1;
            HorFlip := -1;
          end
          else
          begin
            HorBase := 0;
            HorFlip := 1;
          end;

          for k := 0 to GrdMap.TileWidth-1 do
            for l := 0 to GrdMap.TileHeight-1 do
              Canvas.pixels[((i-x)*GrdMap.TileWidth)+ k,((j-y)*GrdMap.TileHeight)+l] :=
                GBColorController.CurColorSets[0,
                  GetPalIndexOfLocation(i,j,(GBColorController.ColorMode <> gbcmSGB)),
                  p.data[VerBase+(l*VerFlip),HorBase+(k*HorFlip)]];
        end;

      Clipboard.Assign(Pic);
    end;

  finally
    Pic.Free;
  end;
end;




end.
