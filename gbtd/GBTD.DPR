program gbtd;

uses
  Forms,
  SysUtils,
  tileedit in 'tileedit.pas' {FrmTile},
  export in 'export.pas' {FrmExport},
  about in 'about.pas' {FrmAbout},
  expbase in 'expbase.pas',
  exptypes in 'exptypes.pas',
  fldfill in 'fldfill.pas',
  undo in 'undo.pas',
  tilecnt in 'tilecnt.pas' {FrmTileCnt},
  SplitClip in 'SplitClip.pas' {FrmSplitClip},
  split in 'split.pas',
  import in 'Import.pas' {FrmImport},
  Palette in 'Palette.pas' {frmPalette},
  Files in 'Files.pas',
  GBTDMain in 'GBTDMain.pas',
  AppMem;

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Gameboy Tile Designer';
  Application.CreateForm(TFrmTile, FrmTile);
  Application.CreateForm(TFrmImport, FrmImport);
  Application.Run;
end.
