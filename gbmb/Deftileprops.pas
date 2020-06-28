unit DefTileProps;

interface

uses
  Windows, Messages, Classes, Forms, SysUtils, gbtlst, MainLib, Grids,
  ExtCtrls, Controls, Repository, gbconst, StdCtrls;

type
  TFrmDefTileProps = class(TForm)
    Pnl: TPanel;
    BtnHelp: TButton;
    BtnOK: TButton;
    LstTiles: TGBTileList;
    GrpProps: TGroupBox;
    grdProps: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LstTilesAfterSelect(Sender: TObject);
    procedure LstTilesBeforeSelect(Sender: TObject; var Proceed: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnHelpClick(Sender: TObject);
    procedure grdPropsKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }

    function SaveProps : boolean;
    procedure FillValues( TileNr : integer );

  public
    { Public declarations }
  end;

var
  FrmDefTileProps: TFrmDefTileProps;

implementation

uses Map;

{$R *.DFM}
{--$I ..\hlp\gbmb.inc}


procedure TFrmDefTileProps.FillValues( TileNr : integer );
var i : integer;
begin
  with grdProps do
    for i := 0 to RowCount-2 do
      Cells[1,i+1] := IntToStr(TileMap.PropInitVal[i][TileNr])
end;


procedure TFrmDefTileProps.FormCreate(Sender: TObject);
var i : integer;
begin

  (* init grid *)
  with grdProps do
  begin
    RowCount := TileMap.PropCount+1;
    Cells[0,0] := 'Property';
    Cells[1,0] := 'Value';

    for i:=0 to TileMap.PropCount-1 do
      Cells[0,i+1] := TileMap.PropDef[i].Name;

    FillValues(FrmMap.LstTiles.SelTile);
  end;

  (* init tilelist *)
  with LstTiles do
  begin
    TileData  := TileList;
    TileSize  := FrmMap.LstTiles.TileSize;

    case TileSize of
      gbts8x8    : TileCount := 11;
      gbts8x16,
      gbts16x16  : TileCount := 6;
      gbts32x32  : TileCount := 3;
    end;

    SelTile := FrmMap.LstTiles.SelTile;
  end;

  (* resize form *)
  GrpProps.Left := LstTiles.Left + LstTiles.Width + 8;
  Pnl.Left := GrpProps.Left;
  TFrmDefTileProps(Sender).ClientWidth := GrpProps.Left + GrpProps.Width + 7;

  GrpProps.Height := LstTiles.Height + 5;
  GrdProps.Height := GrpProps.Height - 24;
  Pnl.Top := GrpProps.Top + GrpProps.Height;
  TFrmDefTileProps(Sender).ClientHeight := Pnl.Top + Pnl.Height;

  (* size grid *)
  with GrdProps do
    if (RowCount > VisibleRowCount+1) then ColWidths[1] := 45;

//  HelpContext := Default_location_properties;
end;


procedure TFrmDefTileProps.FormDestroy(Sender: TObject);
begin
  FrmDefTileProps := nil;
end;


procedure TFrmDefTileProps.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  Modified := True;
end;


procedure TFrmDefTileProps.LstTilesAfterSelect(Sender: TObject);
begin
  GrpProps.Caption := 'Tile ' + IntToStr(TGBTileList(Sender).SelTile);
  FillValues(TGBTileList(Sender).SelTile);
end;


procedure TFrmDefTileProps.LstTilesBeforeSelect(Sender: TObject; var Proceed: Boolean);
begin
  Proceed := SaveProps;
end;


function TFrmDefTileProps.SaveProps : boolean;
var i : integer;

  procedure Err( s : string);
  begin
    Msg( s, MsgError, MB_OK );
    GrdProps.Row := i+1;
    GrdProps.SetFocus;
    Result := False;
  end;

begin
  Result := True;
  i := 0;
  with GrdProps do
    while (Result) and (i < TileMap.PropCount) do
    begin
      if (Cells[1,i+1] = '') then
        Err(TileMap.PropDef[i].Name + ' should be at least 0.' )
      else
      if ( StrToInt(Cells[1,i+1]) > TileMap.PropDef[i].Size ) then
        Err(TileMap.PropDef[i].Name + ' should be lower or equal to ' + IntToStr( TileMap.PropDef[i].Size) + '.' )
      else
        TileMap.PropInitVal[i][LstTiles.SelTile] := StrToInt(Cells[1,i+1]);

      Inc(i);
    end;
end;


procedure TFrmDefTileProps.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SaveProps;
end;


procedure TFrmDefTileProps.BtnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;


procedure TFrmDefTileProps.grdPropsKeyPress(Sender: TObject; var Key: Char);
begin
  (* Force Numbers & length *)
  with GrdProps do
    if (Key <> char(8)) then
      if (not (Key in ['0'..'9'])) or (Length(Cells[1,Row]) >= 5) then
        Key := char(0);
end;

end.
