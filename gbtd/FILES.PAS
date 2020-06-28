unit Files;

interface

procedure LoadFromGBR(const f : string);
function SaveToGBR(AutoSave : boolean): boolean;
function HandleModified : boolean;

implementation

uses SysUtils,Forms, dialogs, controls,gbofile, gbconst, IniFiles, TileEdit,
     ShlObj, GBTDMain, ColorController, GBTileExchanger;


function HandleModified: boolean;
var i : word;
begin
  if Modified then
  begin
    i := messagedlg('The current tiles have been changed. Save changes first ?', mtConfirmation, mbYesNoCancel,0);
    case i of
      mrYes    : Result := SaveToGBR(True);
      mrNo     : Result := True;
      mrCancel : Result := False;
    end;
  end
  else
    Result := True;
end;




function SaveToGBR(AutoSave : boolean): boolean;
var GBOFile : TGBOFile;
    Prod : TGBOProducer;
    TileData : Pointer;
    x,y, Cnt : integer;
    i,j, Size : integer;
    p : PTileType;
    s : string;
    Ok : boolean;
    Data      : Pointer;
    q  : Pointer;

    Pal : TGBPaletteType;

    AUpdate : boolean;
 
begin

  s := IniFile.ReadString('General', 'GBRPath', ExtractFilePath(Application.ExeName)) + Filename;
  Ok := True;
  if (not AutoSave) or (FileName = '') or (not FileExists(s)) then
  begin
     if (Filename = '') then s := s + '*.gbr';

     FrmTile.DlgSave.FileName := s;
     if FrmTile.DlgSave.Execute then
     begin
       s := FrmTile.DlgSave.FileName;
       Filename := ExtractFileName(s);
       IniFile.WriteString('General', 'GBRPath', ExtractFilePath(s));
     end
     else
     begin
       Ok := False;
       Result := False;
     end;
  end;

  if Ok then
  begin

    (* activate exchanger *)
    AUpdate := TileExchanger.Active;
    TileExchanger.FileName := '';

    GBOFile := TGBOFile.Create(s, not FileExists(s));
    try

      (* fill Producer-object *)
      Prod.Name := 'Gameboy Tile Designer';
      StrPCopy( Prod.Version, GBTDVersion );
      Prod.Info := 'Home: www.casema.net/~hpmulder';

      GBOFile.SaveTo(GBOProducerID, 0, Sizeof(TGBOProducer), addr(Prod) );


      (* Fill data object *)
      x := CurTileWidth;
      y := CurTileHeight;
      Cnt := TileList.Count;

      Size := SizeOf(TGBOTileData) + (x * y * Cnt) - 1;
      GetMem(TileData, Size);
      try
        with TGBOTileData(TileData^) do
        begin
          Name := '';
          Width := x;
          Height := y;
          Count := cnt;

          Pal := GBColorController.Palette;
          for i := 0 to 3 do ColorSet[i] := Char(Pal[i]);

          (* get data *)
          j := 0;
          for i :=0 to Cnt-1 do
          begin
            p := TileList.Items[i];
            for y := 0 to Height-1 do
              for x := 0 to Width-1 do
              begin
                Data[j] := Char(p.data[y,x]);
                Inc(j);
              end;
          end;
        end;
        GBOFile.SaveTo(GBOTileDataID, 1, Size, TileData );


        (* fill TileSettings *)
        with TileSettings do
        begin
          TileID := 1;
          Simple := FrmTile.Simple1.Checked;

          Flags := 0;
          if FrmTile.Grid1.Checked then Flags := (Flags or GBOTSGrid);
          if FrmTile.NibbleMarkers1.Checked then Flags := (Flags or GBOTSNibble);

          LeftColor  := FrmTile.clrl;
          RightColor := FrmTile.clrr;
          ColorSet := Byte(GBColorController.ColorMode);
          for i :=0 to 2 do
            BookMarks[i] := FrmTile.LstTiles.Bookmarks[i];

          AutoUpdate := AUpdate;
        end;
        GBOFile.SaveTo(GBOTileSettingsID, 2, SizeOf(TGBOTileSettings), addr(TileSettings) );

        (* Im/Export settings *)
        GBOFile.SaveTo(GBOTileExportID, 3, Sizeof(TGBOTileExport), Addr(TileExport));
        GBOFile.SaveTo(GBOTileImportID, 4, Sizeof(TGBOTileImport), Addr(TileImport));

        (* fill palettes *)
        Size := SizeOf(TGBOPalettes);
        GetMem(Data, Size);
        try
          with TGBOPalettes(Data^) do
          begin
            ID := 1;
            Count := 8;
            Colors := GBColorController.CGBSets;

            SGBCount := 4;
            SGBColors := GBColorController.SGBSets;
          end;
          GBOFile.SaveTo(GBOPalettesID, 5, SizeOf(TGBOPalettes), Data );
        finally
          FreeMem(Data, Size);
        end;


        (* store tile-palettes *)
        Size := SizeOf(TGBOTilePal) + (TileList.Count * (SizeOf(integer)*2));
        GetMem(Data, Size);
        try
          with TGBOTilePal(Data^) do
          begin
            TileID := 1;
            Count := TileList.Count;
            for i :=0 to Count-1 do ColorSet[i] := PTileType(TileList.Items[i]).ColorSet[0];

            q := @ColorSet[TileList.Count];      // first free space
            word(q^) := TileList.Count;
            inc(integer(q),SizeOf(word));
            for i :=0 to Count-1 do
            begin
              integer(q^) := PTileType(TileList.Items[i]).ColorSet[1];
              inc(integer(q), SizeOf(integer));
            end;
          end;

          GBOFile.SaveTo(GBOTilePalID, 6, Size, Data );
        finally
          FreeMem(Data, Size);
        end;

        (* Update 'recent docs' *)
        SHAddToRecentDocs(SHARD_PATH, PChar(s));
        FrmTile.MRUList.InsertItem(0,LowerCase(s));
        SetTitle(s);

        Result := True;
        Modified := False;
      finally
        FreeMem(TileData, Size);
      end;

    finally
      GBOFile.free;
    end;

    (* activate exchanger *)
    TileExchanger.FileName := s;
    TileExchanger.Active   := AUpdate;
    SetExchangerActive( AUpdate );
    Modified := False;
  end;
end;


procedure LoadFromGBR(const f : string);
var GBOFile : TGBOFile;
    s : string;
    TileData : Pointer;
    i,j, Size : integer;
    h,w : integer;
    x,y, Cnt : integer;
    p : PTileType;
    Ok : boolean;
    Data : Pointer;
    q  : pointer;

    ClrSets : TGBColorSets;
    Pal : TGBPaletteType;

    AUpdate : boolean;
begin

  if HandleModified then
  begin

    if (f = '') then
    begin
      s := IniFile.ReadString('General', 'GBRPath', ExtractFilePath(Application.ExeName)) + '*.gbr';
      FrmTile.DlgOpen.FileName := s;
      if FrmTile.DlgOpen.Execute then
      begin
        s := FrmTile.DlgOpen.FileName;
        Ok := True;
      end
      else
        Ok := False;
    end
    else
    begin
      s := f;
      Ok := True;
    end;

    if Ok then
    begin

      (* deactivate exchanger *)
      TileExchanger.FileName := '';

      Filename := ExtractFileName(s);
      IniFile.WriteString('General', 'GBRPath', ExtractFilePath(s));

      GBOFile := TGBOFile.Create(s, False);
      try

        if GBOFile.Locate( GBOTileDataID, 1 ) then
        begin
          Size := GBOFile.CurSize;

          GetMem(TileData, Size);
          try
            FrmTile.GrdPixel.Freezed := True;
            FrmTile.ShwOne.Freezed := True;
            FrmTile.ShwMult.Freezed := True;
            FrmTile.LstTiles.Freezed := True;
            try

              GBOFile.LoadFrom( GBOTileDataID, 1, Size, TileData );

              h := TGBOTileData(TileData^).Height;
              w := TGBOTileData(TileData^).Width;

              with TGBOTileData(TileData^) do
              begin
                CurTileSize := GetTileSize(w, h);
                CurTileWidth := w;
                CurTileHeight := h;

                for i := 0 to 3 do Pal[i] := byte(ColorSet[i]);
                GBColorController.Palette := Pal;

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
                  GetMem( p, Sizeof(TileType) );
                  TileList.Add(p);
                  p.ColorSet[0] := 0;
                  p.ColorSet[1] := 0;

                  for y := 0 to h-1 do
                    for x := 0 to w-1 do
                    begin
                      p.data[y,x] := byte(Data[j]);
                      Inc(j);
                    end;
                end;
              end;

              Size := SizeOf(TGBOPalettes);
              GetMem(Data, Size);
              try
                for i := 0 to Size-1 do byte(pointer(integer(Data) + i)^) := 0;

                if GBOFile.LoadFrom(GBOPalettesID, 5, SizeOf(TGBOPalettes), Data) then
                  with TGBOPalettes(Data^) do
                  begin
                    if Count > 8 then i := 7 else i := Count-1;
                    for j := 0 to i do ClrSets[j] := Colors[j];
                    GBColorController.CGBSets := ClrSets;

                    if SGBCount > 4 then i := 3 else i := SGBCount-1;
                    for j := 0 to i do ClrSets[j] := SGBColors[j];
                    GBColorController.SGBSets := ClrSets;

                 end;

              finally
                FreeMem(Data, Size);
              end;

              Size := SizeOf(TGBOTilePal) + (TileList.Count * (SizeOf(integer)*2));
              GetMem(Data, Size);
              try
                for i := 0 to Size-1 do byte(Data^) := 0;
                if GBOFile.LoadFrom(GBOTilePalID, 6, Size, Data) then
                with TGBOTilePal(Data^) do
                begin
                  if (Count > TileList.Count) then j := TileList.Count-1 else j := Count-1;
                  for i :=0 to j do PTileType(TileList.Items[i]).ColorSet[0] := ColorSet[i];

                  q := @ColorSet[Count];             // first free space
                  j := word(q^);
                  inc(integer(q), SizeOF(word));
                  if (j > 0) then
                    for i :=0 to j-1 do
                    begin
                      PTileType(TileList.Items[i]).ColorSet[1] := integer(q^);
                      inc(integer(q), SizeOf(integer));
                    end;
                end;

              finally
                FreeMem(Data, Size);
              end;


              (* set to default -1 *)
              for i :=0 to 2 do TileSettings.Bookmarks[i] := word(-1);
              if GBOFile.LoadFrom(GBOTileSettingsID, 2, SizeOf(TGBOTileSettings), addr(TileSettings)) then
              with TileSettings do
              begin
                FrmTile.Simple1.Checked := Simple;

                FrmTile.Grid1.Checked := boolean(Flags and 1);
                FrmTile.GrdPixel.GridLines := FrmTile.Grid1.Checked;
                FrmTile.NibbleMarkers1.Checked := boolean((Flags shr 1) and 1);
                FrmTile.GrdPixel.NibbleMarkers := FrmTile.NibbleMarkers1.Checked;
                FrmTile.clrl := LeftColor;
                FrmTile.clrr := RightColor;
                SetColorset(ColorSet);
                for i := 0 to 2 do FrmTile.LstTiles.BookMarks[i] := Bookmarks[i];

                AUpdate := AutoUpdate;
              end;

              GBOFile.LoadFrom(GBOTileExportID, 3, Sizeof(TGBOTileExport), Addr(TileExport));
              GBOFile.LoadFrom(GBOTileImportID, 4, Sizeof(TGBOTileImport), Addr(TileImport));

            finally
              with FrmTile do
              begin
                GrdPixel.Freezed := False;
                ShwOne.Freezed := False;
                ShwMult.Freezed := False;
                LstTiles.Freezed := False;
              end;
            end;
            FrmTile.SetTileSize(CurTileSize);
            Modified := False;

          finally
            FreeMem(TileData, Size);
          end;
        end;

        SHAddToRecentDocs(SHARD_PATH, PChar(s));
        FrmTile.MRUList.InsertItem(0,LowerCase(s));
        SetTitle(s);

        (* activate exchanger *)
        TileExchanger.FileName := s;
        SetExchangerActive( AUpdate );

      finally
        GBOFile.Free;
        Modified := False;
      end;

      UndoCtrl.Clear;
    end;
  end;
end;


end.
