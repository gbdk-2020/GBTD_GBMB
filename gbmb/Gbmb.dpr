program gbmb;

uses
  Forms,
  map in 'map.pas' {FrmMap},
  files in 'files.pas',
  TileProps in 'TileProps.pas' {FrmTileProps},
  Repository in 'Repository.pas',
  GBMBMain in 'GBMBMain.pas',
  MapProps in 'MapProps.pas' {FrmMapProps},
  TileFileObserver in 'TileFileObserver.pas',
  Export in 'Export.pas' {FrmExport},
  ExportClass in 'ExportClass.pas',
  DefTileProps in 'DefTileProps.pas' {FrmDefTileProps},
  about in 'about.pas' {FrmAbout},
  fldfill in 'fldfill.pas',
  BlockFill in 'BlockFill.pas' {FrmBlockFill},
  MapUndo in 'MapUndo.pas',
  GBMBClipboard in 'GBMBClipboard.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Gameboy Map Builder';
  Application.CreateForm(TFrmMap, FrmMap);
  Application.Run;
end.
