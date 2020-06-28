unit GBTDMain;

interface

uses classes,SysUtils,IniFiles, gbConst, gboFile, Undo, GBTileExchanger;

  procedure SetTitle(s : string);
  procedure SetColorSet(i : integer);
  procedure SetExchangerActive( b : boolean);

var
  CurTileSize   : TGBTileSize;
  CurTileWidth  : integer;
  CurTileHeight : integer;
  UndoCtrl      : TGBTDUndo;
  TileList      : TList;
  IniFile       : TIniFile;
  FileName      : TFileName;
  TileExport    : TGBOTileExport;
  TileImport    : TGBOTileImport;
  TileSettings  : TGBOTileSettings;
  Modified      : boolean;

  TileExchanger : TGBTileExchanger;


implementation

uses Forms, TileEdit, ColorController;


procedure SetExchangerActive( b : boolean);
begin
  TileExchanger.Active := b;
  FrmTile.BtnAutoUpdate.Down := b;
  Modified := True;
  UndoCtrl.PixelEnd;
end;



procedure SetColorSet(i : integer);
var j : integer;
begin
  with FrmTile.MnuColorSet do
  begin
    if (Count <= i) then i := 0;
    for j := 0 to Count-1 do
      if Items[j].tag = i then
        Items[j].Checked := True;
  end;

  with GBColorController do
  begin
    ColorMode := TGBColorMode(i);
    if (i = 3) then ColorSetIndex := 1 else ColorSetIndex := 0;
  end;

  FrmTile.RefreshSelector;
  FrmTile.GrdPixel.Paint;
  FrmTile.LstTiles.Paint;
  Modified := True;
end;


procedure SetTitle(s : string);
var t : string;
    rec : TSearchRec;
begin
  if FindFirst(s, faAnyFile, rec) <> 0 then
    t := trim(ExtractFileName(s))
  else
    t := rec.Name;

  if (t <> '') then t := ' - ' + t;

  if (t <> '') then
    Application.Title := 'GBTD' + t
  else
    Application.Title := GBTDTitle;

  FrmTile.Caption := GBTDTitle + t;
end;


end.
