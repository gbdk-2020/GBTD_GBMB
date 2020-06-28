unit files;

interface

uses gbofile, gbconst, repository, GBMBMain, SysUtils, Forms,ColorController,
     Windows, ProGboFile, MainLib;

procedure LoadTileSet( name : string );
procedure SaveMap( name : string );
procedure LoadMap( name : string );

implementation

uses map;



procedure LoadMap( name : string );
var
  f       : TProGBOFile;
  w       : word;
  i,j,k   : integer;
  Size    : integer;
  pal     : word;

  tmpPropCnt   : integer;
  tmpTileCnt   : integer;

  TileRec              : ProGBOMapTileDataRec;
  ProGBOMap            : TProGBOMap;
  ProGBOMapTileData    : PProGBOMapTileData;
  ProGBOMapProp        : PProGBOMapProp;
  MapPropDef           : RMapPropDef;
  ProGBOMapPropData    : PProGBOMapPropData;
  ProGBOMapPropDefault : PProGBOMapPropDefault;

  WinPlace : TWINDOWPLACEMENT;
  WinP     : PWINDOWPLACEMENT;

begin

  tmpTileCnt := 0;
  tmpPropCnt := 0;

  f := TProGBOFile.Create( name, False );
  FreezeAll;
  try
//    ChDir(ExtractFilePath(name));

    (* init *)
    for i := 0 to TileMap.PropCount-1 do TileMap.DeletePropDef(0);
    TileMap.SetTileCount(0);


    if f.LoadFrom(ProGBOMapID, 1, 0, SizeOf(TProGBOMap), @ProGBOMap ) then
    begin
      (* Load map *)
      with ProGBOMap do
      begin
        TileMap.SetBounds( Width, Height, False);
        TileFileName   := TileFile;
        tmpPropCnt     := PropCount;
        tmpTileCnt     := TileCount;
      end;

      (* Load map data *)
      Size := (SizeOf(ProGBOMapTileDataRec) * TileMap.Width * TileMap.Height);
      GetMem(ProGBOMapTileData, Size);
      try
        if f.LoadFrom(ProGBOMapTileDataID, 2, 1, Size, ProGBOMapTileData ) then
          for i := 0 to TileMap.Height-1 do
            for j := 0 to TileMap.Width-1 do
            begin
              TileRec := ProGBOMapTileData.Data[(i*TileMap.Width)+j];
              k := TileRec.Tile[2] + (TileRec.Tile[1] SHL 8) + (TileRec.Tile[0] SHL 16);
              TileMap.Data[(i*TileMap.Width)+j].Tile  := (k and $03FF);
              TileMap.Data[(i*TileMap.Width)+j].Flags := (k SHR (22-4));

              pal := ((k shr 10) and $0F) + (((k shr 16) and $07) SHL 8);
              TileMap.Data[(i*TileMap.Width)+j].Palette := pal;
            end;
      finally
        FreeMem(ProGBOMapTileData, Size);
      end;


      (* Load map property definitions *)
      Size := (SizeOf(ProGBOMapPropRec) * tmpPropCnt);
      GetMem(ProGBOMapProp, Size);
      try
        if f.LoadFrom( ProGBOMapPropID, 3, 1, Size, ProGBOMapProp ) then
          for i := 0 to tmpPropCnt-1 do
            with ProGBOMapProp.Data[i] do
            begin
              MapPropDef.Loc   := i;
              MapPropDef.pType := pType;
              MapPropDef.Name  := ProGBOMapProp.Data[i].Name;
              MapPropDef.Size  := ProGBOMapProp.Data[i].Size;
              TileMap.InsertPropDef(i, MapPropDef);
            end;
      finally
        FreeMem(ProGBOMapProp, Size);
      end;


      (* Load map property data *)
      Size := (Sizeof(Word) * TileMap.Width * TileMap.Height * TileMap.PropCount);
      GetMem(ProGBOMapPropData, Size);
      try
        (* get size *)
        if f.LoadFrom(ProGBOMapPropDataID, 4, 1, Size, ProGBOMapPropData ) then
          with TileMap do
            for i := 0 to PropCount-1 do
             for j := 0 to Height-1 do
               for k := 0 to Width-1 do
                 SetPropOfLocation(k,j,i, ProGBOMapPropData.Data[(i * Height * Width)+(j*Width)+k]);

      finally
        FreeMem(ProGBOMapPropData, Size);
      end;

    end;


        (* Load property colors *)
    f.LoadFrom(ProGBOPropColorsID, 7, 1, SizeOf(TProGBOPropColors), @PropColors);
    CalcAllPropColors;

    (* Load export settings *)
    ExportSettings.Load(f, 8, 9, 1 );

    (* Load settings *)
    MapSettings.FormMaximized := False;
    if f.LoadFrom(ProGBOMapSettingsID,  6, 1, SizeOf(TProGBOMapSettings), @MapSettings) then
      with MapSettings do
      begin

        FrmMap.ChangeZoom(Zoom);
        SetInfoPanel(InfoPanel);
        SetColorSet(ColorSet);
        FrmMap.GrdMap.Grid := Grid;
        FrmMap.GrdMap.DoubleMarkers := DoubleMarkers;
        FrmMap.GrdMap.PropColors := PropColors;

        for i := 0 to 2 do FrmMap.LstTiles.Bookmarks[i] := Bookmarks[i];

        SetTitle(name);

        with WinPlace do
        begin
          length := SizeOf(TWindowPlacement);
          Flags := 0;
          if FormMaximized then showCmd := SW_SHOWMAXIMIZED else showCmd := SW_SHOWNORMAL;
          ptMinPosition.x := 0;
          ptMinPosition.y := 0;
          ptMaxPosition.x := 0;
          ptMaxPosition.y := 0;
          rcNormalPosition.Left := FrmMap.Left;
          rcNormalPosition.Top := FrmMap.Top;
          rcNormalPosition.Right := FrmMap.Left + FormWidth;
          rcNormalPosition.Bottom := FrmMap.Top + FormHeight;
          SetWindowPlacement(FrmMap.Handle, @WinPlace );
        end;

      end;





    (* load tiles *)
    SetTileFile(TileFilename);
//    TileExchanger.Active := MapSettings.AutoUpdate;
    SetExchangerActive( MapSettings.AutoUpdate );


    (* Load tile default props *)
    if (tmpTileCnt > TileList.Count) then k := TileList.Count else k := tmpTileCnt;
    Size := (SizeOf(Word) * tmpTileCnt * TileMap.PropCount);
    GetMem(ProGBOMapPropDefault, Size);
    try
      if f.LoadFrom(ProGBOMapPropDefaultID, 5, 1, Size, ProGBOMapPropDefault) then
        for j := 0 to k-1 do
         for i :=0 to TileMap.PropCount-1 do
           TileMap.PropInitVal[i][j] := ProGBOMapPropDefault.Data[(i * tmpTileCnt) + j];
    finally
      FreeMem(ProGBOMapPropDefault, Size);
    end;



  finally
    UnFreezeAll;
    f.Free;
    MapUndoCtrl.Clear;
    FullRefresh;
  end;

end;









procedure SaveMap( name : string );
var
  f                    : TProGBOFile;
  i,j,k                : integer;
  Size                 : integer;
  w                    : word;
  TileRec              : ProGBOMapTileDataRec;

  WinPlace             : TWINDOWPLACEMENT;

  ProGBOProducer       : TProGBOProducer;
  ProGBOMap            : TProGBOMap;
  ProGBOMapTileData    : PProGBOMapTileData;
  ProGBOMapProp        : PProGBOMapProp;
  ProGBOMapPropData    : PProGBOMapPropData;
  ProGBOMapPropDefault : PProGBOMapPropDefault;

begin

  f := TProGBOFile.Create( name, not FileExists(name) );
  try
//    ChDir(ExtractFilePath(name));

    (* Save Producer *)
    with ProGBOProducer do
    begin
      Name := LONGTITLE;
      Version := GBMBVERSION;
      Info := 'Home: http://www.casema.net/~hpmulder';
    end;
    f.SaveTo(ProGBOProducerID, 0, 0, SizeOf(TProGBOProducer), addr(ProGBOProducer) );


    (* Save Map *)
    with ProGBOMap do
    begin
      Name       := '';
      Width      := TileMap.Width;
      Height     := TileMap.Height;
      PropCount  := TileMap.PropCount;
      CopyBuf( PChar(TileFileName), addr(TileFile), Length(TileFileName)+1 );
      TileCount  := TileList.Count;
      PropColorCount := 2;
    end;
    f.SaveTo(ProGBOMapID, 1, 0, SizeOf(TProGBOMap), addr(ProGBOMap) );


    (* Save map Tile data *)
    Size := (SizeOf(ProGBOMapTileDataRec) * TileMap.Width * TileMap.Height);
    GetMem( ProGBOMapTileData, Size );
    try
      for i := 0 to TileMap.Height-1 do
        for j := 0 to TileMap.Width-1 do
        begin
          k := (TileMap.Data[(i*TileMap.Width)+j].Tile and $03FF);
          k := k + (TileMap.Data[(i*TileMap.Width)+j].Flags shl (22-4));

          k := k + ((TileMap.Data[(i*TileMap.Width)+j].Palette and $0F) shl 10);
          k := k + ((TileMap.Data[(i*TileMap.Width)+j].Palette and $0700) shl (16-8));

//                    pal := ((k shr 9) and $0F) + ((k shr 16) and $07);
//              TileMap.Data[(i*TileMap.Width)+j].Palette := pal;


          TileRec.Tile[0] := (k SHR 16);
          TileRec.Tile[1] := (k SHR 8);
          TileRec.Tile[2] := (k);
          ProGBOMapTileData.Data[(i*TileMap.Width)+j] := TileRec;
        end;

      f.SaveTo(ProGBOMapTileDataID, 2, 1, Size, ProGBOMapTileData);
    finally
      FreeMem(ProGBOMapTileData, Size);
    end;


    (* Save map prop defs *)
    Size := (SizeOf(ProGBOMapPropRec) * TileMap.PropCount);
    GetMem( ProGBOMapProp, Size );
    try
      for i :=0 to TileMap.PropCount-1 do
        with TileMap.PropDef[i] do
        begin
          ProGBOMapProp.Data[i].pType  := pType;
          StrPCopy(ProGBOMapProp.Data[i].Name, Name);
          ProGBOMapProp.Data[i].Size   := TileMap.PropDef[i].Size;
        end;

      f.SaveTo(ProGBOMapPropID, 3, 1, Size, ProGBOMapProp);
    finally
      FreeMem(ProGBOMapProp, Size);
    end;


    (* Save map Prop data *)
    with TileMap do
    begin
      Size := (SizeOf(Word) * PropCount * Width * Height);
      GetMem( ProGBOMapPropData, Size );
      try
        for k := 0 to PropCount-1 do
          for i := 0 to Height-1 do
            for j := 0 to Width-1 do
              ProGBOMapPropData.Data[(k * Height * Width)+(i*Width)+j] := GetPropOfLocation(j,i,k);

        f.SaveTo(ProGBOMapPropDataID, 4, 1, Size, ProGBOMapPropData);
      finally
        FreeMem(ProGBOMapPropData, Size);
      end;
    end;


    (* Save tile default props *)
    Size := (SizeOf(Word) * TileList.Count * TileMap.PropCount);
    GetMem( ProGBOMapPropDefault, Size );
    try
      for j := 0 to TileList.Count-1 do
        for i :=0 to TileMap.PropCount-1 do
          ProGBOMapPropDefault.Data[(i * TileList.Count) + j] := TileMap.PropInitVal[i][j];

      f.SaveTo(ProGBOMapPropDefaultID, 5, 1, Size, ProGBOMapPropDefault);
    finally
      FreeMem(ProGBOMapPropDefault, Size);
    end;


    (* Save settings *)
    WinPlace.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(FrmMap.Handle, @WinPlace );
    with MapSettings do
    begin
      FormWidth      := WinPlace.rcNormalPosition.Right - WinPlace.rcNormalPosition.Left;
      FormHeight     := WinPlace.rcNormalPosition.Bottom - WinPlace.rcNormalPosition.Top;
      FormMaximized  := (FrmMap.WindowState = wsMaximized);

      InfoPanel     := FrmMap.InfoPanel1.Checked;
      Grid          := FrmMap.GrdMap.Grid;
      DoubleMarkers := FrmMap.GrdMap.DoubleMarkers;
      PropColors    := FrmMap.GrdMap.PropColors;

      Zoom     := FrmMap.CmbZoom.ItemIndex;
      ColorSet := byte(GBColorController.ColorMode);
      for i := 0 to 2 do Bookmarks[i] := FrmMap.LstTiles.Bookmarks[i];

      AutoUpdate    := TileExchanger.Active;
    end;
    f.SaveTo(ProGBOMapSettingsID, 6, 1, SizeOf(TProGBOMapSettings), @MapSettings);

    (* Save property colors *)
    f.SaveTo(ProGBOPropColorsID, 7, 1, SizeOf(TProGBOPropColors), @PropColors);

    (* Save export settings *)
    ExportSettings.Save(f, 8, 9, 1 );

    SetTitle(name);


  finally
    f.Free;
    MapUndoCtrl.Clear;
  end;
end;










procedure LoadTileSet( name : string );
var f : TGBOFile;
    PData : Pointer;
    i,j, Size,Cnt : integer;
    h,w,x,y : integer;
    p : PTileType;
    q : pointer;

    ClrSets : TGBColorSets;
begin

//  (* deactivate exchanger *)
//  TileExchanger.FileName := '';

  f := TGBOFile.Create( name, False );
  try
    if f.Locate( GBOTileDataID, 1 ) then
    begin
      Size := f.CurSize;

      GetMem(PData, Size);
      try
        f.LoadFrom( GBOTileDataID, 1, Size, PData );

        h := TGBOTileData(PData^).Height;
        w := TGBOTileData(PData^).Width;

        with TGBOTileData(PData^) do
        begin
          Cnt := Count-1;

          (* empty current set *)
          for i := 0 to TileList.Count-1 do
          begin
            dispose( PTileType(TileList.Items[0]));
            TileList.Delete(0);
          end;

          j := 0;
          for i := 0 to Cnt do
          begin
            GetMem( p, SizeOf(TileType) );
            TileList.Add(p);
            for y := 0 to h-1 do
              for x := 0 to w-1 do
              begin
                p.data[y,x] := byte(Data[j]);
                Inc(j);
              end;
          end;
          GBColorController.Palette := TGBPaletteType(Colorset);

        end;

        SetTileSize(w,h);
      finally
        FreeMem(PData, Size);
      end;

      Size := SizeOf(TGBOPalettes);
      GetMem(PData, Size);
      ClearBuf(PData, Size);
      try
        if f.LoadFrom(GBOPalettesID, 5, SizeOf(TGBOPalettes), PData) then
          with TGBOPalettes(PData^) do
          begin
            if Count > 8 then i := 7 else i := Count-1;
            for j := 0 to i do ClrSets[j] := Colors[j];
            GBColorController.CGBSets := ClrSets;

            if SGBCount > 4 then i := 3 else i := SGBCount-1;
            for j := 0 to i do ClrSets[j] := SGBColors[j];
            GBColorController.SGBSets := ClrSets;
          end;
      finally
        FreeMem(PData, Size);
      end;

      Size := SizeOf(TGBOTilePal) + (TileList.Count * SizeOf(integer) * 2);
      GetMem(PData, Size);
      ClearBuf(PData, Size);
      try
        if f.LoadFrom(GBOTilePalID, 6, Size, PData) then
        with TGBOTilePal(PData^) do
        begin
          if (Count > TileList.Count) then j := TileList.Count-1 else j := Count-1;
          for i :=0 to j do PTileType(TileList.Items[i]).ColorSet[0] := ColorSet[i];

          q := @ColorSet[Count];             // first free space
          j := word(q^);
          if (j > TileList.Count) then j := TileList.Count-1;
          inc(integer(q), SizeOF(word));
          if (j > 0) then
            for i :=0 to j-1 do
            begin
              PTileType(TileList.Items[i]).ColorSet[1] := integer(q^);
              inc(integer(q), SizeOf(integer));
            end;
        end;

      finally
        FreeMem(PData, Size);
      end;

    end;
  finally
    f.Free;
  end;

//  (* activate exchanger *)
//  TileExchanger.FileName := Name;
end;

end.
