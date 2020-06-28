unit GBMBClipboard;

interface

uses Classes, Clipbrd;

procedure CutToClipBoard;
procedure CopyToClipBoard;
procedure PasteFromClipBoard;

implementation

uses Repository, MainLib, SySutils, Map, gbConst, GBMBMain;

procedure CutToClipBoard;
var l,r,t,b : integer;
    i,j : integer;
begin
  CopyToClipBoard;

  MapUndoCtrl.Mode := gbumMap;
  MapUndoCtrl.AddUndo(0,0,TileMap.data[0]);  (* params unused *)
  Modified := True;

  (* determine size *)
  if (FrmMap.GrdMap.SelZoneLeft <> FrmMap.GrdMap.SelZoneRight) or
     (FrmMap.GrdMap.SelZoneTop <> FrmMap.GrdMap.SelZoneBottom) then
  begin
    l := FrmMap.GrdMap.SelZoneLeft;
    r := FrmMap.GrdMap.SelZoneRight;
    t := FrmMap.GrdMap.SelZoneTop;
    b := FrmMap.GrdMap.SelZoneBottom;
  end
  else
  begin
    l := 0;
    r := TileMap.Width-1;
    t := 0;
    b := TileMap.Height-1;
  end;


  (* setup map data *)
  for j := t to b do
    for i := l to r do
    begin
      PokeTileOfLocation(i,j, 0);
    end;

  CalcAllPropColors;
  FrmMap.GrdMap.Paint;
  RefreshInfoPanel;
end;

procedure PasteFromClipBoard;
var strm  : TStream;
    Lst   : TStringList;
    s     : string;
    x,y,i,j : integer;
    p       : PChar;
    len     : integer;
begin
  MapUndoCtrl.Clear;
  MapUndoCtrl.Mode := gbumMap;
  MapUndoCtrl.AddUndo(0,0,TileMap.data[0]);  (* params unused *)
  Modified := True;

  strm := TMemoryStream.Create;
  Lst  := TStringList.Create;
  try

    TextClipboardToStream(strm);

{    (* convert from clipboard to stream *)
    GetMem(p, 128 * 1024);
    try
      len := Clipboard.GetTextBuf(p, 128*1024);
      strm.seek(soFromBeginning,0);
      strm.Write(p^, len);
    finally
      FreeMem(p);
    end;
}
    strm.seek(soFromBeginning,0);

    (* starting position *)
    x := FrmMap.GrdMap.SelZoneLeft;
    y := FrmMap.GrdMap.SelZoneTop;

    (* load map data *)
    s := LineFromStream(strm);
    while (s <> '') and (strm.Position < strm.Size ) do
    begin
      Lst.Clear;
      SeparateStrData(s,Lst);

      for i := 0 to Lst.Count-1 do
        if (y < TileMap.Height) and ((x+i) < TileMap.Width) then
          PokeTileOfLocation( (x+i), y, SaveStrToInt(Lst.Strings[i]) );

      Inc(y);
      s := LineFromStream(strm);
    end;


    (* load V-Flip *)
    y := FrmMap.GrdMap.SelZoneTop;
    s := LineFromStream(strm);
    while (s <> '') and (strm.Position < strm.Size ) do
    begin
      Lst.Clear;
      SeparateStrData(s,Lst);

      for i := 0 to Lst.Count-1 do
        if (y < TileMap.Height) and ((x+i) < TileMap.Width) then
          if (SaveStrToInt(Lst.Strings[i]) = 0) then
            SetFlipOfLocation( (x+i), y, False, False )
          else
            SetFlipOfLocation( (x+i), y, False, True );

      Inc(y);
      s := LineFromStream(strm);
    end;


    (* load H-Flip *)
    y := FrmMap.GrdMap.SelZoneTop;
    s := LineFromStream(strm);
    while (s <> '') and (strm.Position < strm.Size ) do
    begin
      Lst.Clear;
      SeparateStrData(s,Lst);

      for i := 0 to Lst.Count-1 do
        if (y < TileMap.Height) and ((x+i) < TileMap.Width) then
          if (SaveStrToInt(Lst.Strings[i]) = 0) then
            SetFlipOfLocation( (x+i), y, True, False )
          else
            SetFlipOfLocation( (x+i), y, True, True );

      Inc(y);
      s := LineFromStream(strm);
    end;

    (* load GBC palettes *)
    y := FrmMap.GrdMap.SelZoneTop;
    s := LineFromStream(strm);
    while (s <> '') and (strm.Position < strm.Size ) do
    begin
      Lst.Clear;
      SeparateStrData(s,Lst);

      for i := 0 to Lst.Count-1 do
        if (y < TileMap.Height) and ((x+i) < TileMap.Width) then
          SetPalOfLocation( (x+i), y, SaveStrToInt(Lst.Strings[i]), True );

      Inc(y);
      s := LineFromStream(strm);
    end;

    
    (* load SGB palettes *)
    y := FrmMap.GrdMap.SelZoneTop;
    s := LineFromStream(strm);
    while (s <> '') and (strm.Position < strm.Size ) do
    begin
      Lst.Clear;
      SeparateStrData(s,Lst);

      for i := 0 to Lst.Count-1 do
        if (y < TileMap.Height) and ((x+i) < TileMap.Width) then
          SetPalOfLocation( (x+i), y, SaveStrToInt(Lst.Strings[i]), False );

      Inc(y);
      s := LineFromStream(strm);
    end;


    (* load properties *)
    for j := 0 to TileMap.PropCount-1 do
    begin
      y := FrmMap.GrdMap.SelZoneTop;

      (* load map data *)
      s := LineFromStream(strm);
      while (s <> '') and (strm.Position < strm.Size ) do
      begin
        Lst.Clear;
        SeparateStrData(s,Lst);

        for i := 0 to Lst.Count-1 do
          if (y < TileMap.Height) and ((x+i) < TileMap.Width) then
            SetPropOfLocation( (x+i), y, j, SaveStrToInt(Lst.Strings[i]) );

        Inc(y);
        s := LineFromStream(strm);
      end;
    end;

    CalcAllPropColors;


  finally
    Lst.Free;
    strm.Free;

    FrmMap.GrdMap.Paint;
//    RefreshMap;
    RefreshInfoPanel;
  end;
end;



procedure CopyToClipBoard;

const SEP    : PChar = chr(9);
      EOL    : PChar = chr(13) + chr(10);
      FTRUE  : PChar = '1';
      FFALSE : PChar = '0';

var strm  : TStream;
    l,r,t,b : integer;
    i,j,k : integer;
    s     : string;

begin
  strm := TMemoryStream.Create;
  try

    (* determine size *)
    if (FrmMap.GrdMap.SelZoneLeft <> FrmMap.GrdMap.SelZoneRight) or
       (FrmMap.GrdMap.SelZoneTop <> FrmMap.GrdMap.SelZoneBottom) then
    begin
      l := FrmMap.GrdMap.SelZoneLeft;
      r := FrmMap.GrdMap.SelZoneRight;
      t := FrmMap.GrdMap.SelZoneTop;
      b := FrmMap.GrdMap.SelZoneBottom;
    end
    else
    begin
      l := 0;
      r := TileMap.Width-1;
      t := 0;
      b := TileMap.Height-1;
    end;


    (* setup map data *)
    for j := t to b do
    begin
      for i := l to r do
      begin
        s := IntToStr(GetTileOfLocation(i,j) );
        strm.Write(PChar(s)^, Length(s) );
        if (i < r) then strm.Write(SEP^,1);
      end;

      strm.Write(EOL^,2);
    end;
    strm.Write(EOL^,2);                    (* extra separator *)


    (* setup V-Flip *)
    for j := t to b do
    begin
      for i := l to r do
      begin
        if GetFlipOfLocation(i,j, False) then strm.Write(FTRUE^, 1 ) else strm.Write(FFALSE^, 1 );
        if (i < r) then strm.Write(SEP^,1);
      end;
      strm.Write(EOL^,2);
    end;
    strm.Write(EOL^,2);                    (* extra separator *)


    (* setup H-Flip *)
    for j := t to b do
    begin
      for i := l to r do
      begin
        if GetFlipOfLocation(i,j, True) then strm.Write(FTRUE^, 1 ) else strm.Write(FFALSE^, 1 );
        if (i < r) then strm.Write(SEP^,1);
      end;
      strm.Write(EOL^,2);
    end;
    strm.Write(EOL^,2);                    (* extra separator *)


    (* setup GBC palettes *)
    for j := t to b do
    begin
      for i := l to r do
      begin
        s := IntToStr(GetPalOfLocation(i,j,True) );
        strm.Write(PChar(s)^, Length(s) );
        if (i < r) then strm.Write(SEP^,1);
      end;
      strm.Write(EOL^,2);
    end;
    strm.Write(EOL^,2);                    (* extra separator *)

    (* setup SGB palettes *)
    for j := t to b do
    begin
      for i := l to r do
      begin
        s := IntToStr(GetPalOfLocation(i,j,False) );
        strm.Write(PChar(s)^, Length(s) );
        if (i < r) then strm.Write(SEP^,1);
      end;
      strm.Write(EOL^,2);
    end;
    strm.Write(EOL^,2);                    (* extra separator *)


    (* setup properties *)
    for k := 0 to TileMap.PropCount-1 do
    begin
      for j := t to b do
      begin
        for i := l to r do
        begin
          s := IntToStr(GetPropOfLocation(i,j,k) );
          strm.Write(PChar(s)^, Length(s) );
          if (i < r) then strm.Write(SEP^,1);
        end;

        strm.Write(EOL^,2);
      end;
      strm.Write(EOL^,2);                    (* extra separator *)
    end;


    (* move data to the clipboard *)
    StreamToClipBoard(strm);

  finally
    strm.Free;
  end;

end;


end.
