unit BlockFill;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  gbtlst, StdCtrls, MainLib, ExtCtrls, gbConst;

type
  TFrmBlockFill = class(TForm)
    LstTiles: TGBTileList;
    GrpProps: TGroupBox;
    EdHeight: TEdit;
    Label5: TLabel;
    Label4: TLabel;
    EdWidth: TEdit;
    EdTop: TEdit;
    Label3: TLabel;
    Label2: TLabel;
    EdLeft: TEdit;
    cmbPattern: TComboBox;
    Label1: TLabel;
    Pnl: TPanel;
    BtnHelp: TButton;
    BtnCancel: TButton;
    BtnOK: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NumEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    { Private declarations }
    function ValidSettings: boolean;

  public
    { Public declarations }
  end;

var
  FrmBlockFill: TFrmBlockFill;

implementation

uses map, Repository, gbmbmain;

{$R *.DFM}
{--$I ..\hlp\gbmb.inc}


procedure TFrmBlockFill.FormDestroy(Sender: TObject);
begin
  frmBlockFill := nil;
end;


procedure TFrmBlockFill.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TFrmBlockFill.FormCreate(Sender: TObject);
begin
//  HelpContext := Block_fill;

  with LstTiles do
  begin
    TileData  := FrmMap.LstTiles.TileData;
    TileSize  := FrmMap.LstTiles.TileSize;

    case TileSize of
      gbts8x8    : TileCount := 8;
      gbts8x16,
      gbts16x16  : TileCount := 4;
      gbts32x32  : TileCount := 2;
    end;

    SelTile   := FrmMap.LstTiles.SelTile;
  end;

  (* load values *)
  with FrmMap.GrdMap do
  begin
    EdLeft.Text := IntToStr(SelZoneLeft);
    EdTop.Text := IntToStr(FrmMap.GrdMap.SelZoneTop);

    if (SelZoneLeft <> SelZoneRight) or (SelZoneTop <> SelZoneBottom) then
    begin
      EdWidth.Text := IntToStr(1 + SelZoneRight-SelZoneLeft);
      EdHeight.Text := IntToStr(1 + SelZoneBottom - SelZoneTop);
    end
    else
    begin
      EdWidth.Text := IntToStr(MapSettings.BlockFillWidth);
      EdHeight.Text := IntToStr(MapSettings.BlockFillHeight);
    end;
  end;

  if (MapSettings.BlockFillPattern > cmbPattern.Items.Count-1 ) then
    cmbPattern.ItemIndex := 0
  else
    cmbPattern.ItemIndex := MapSettings.BlockFillPattern;

  (* setup screen *)
  GrpProps.Left := LstTiles.Left + LstTiles.Width + 8;
  Pnl.Left := GrpProps.Left;
  TFrmBlockFill(Sender).Width := GrpProps.Left + GrpProps.Width + 12;

  GrpProps.Height := LstTiles.Height + 5;
  Pnl.Top := GrpProps.Top + GrpProps.Height;
  TFrmBlockFill(Sender).Height := Pnl.Top + Pnl.Height + GetSystemMetrics( SM_CYCAPTION ) ;
end;


procedure TFrmBlockFill.BtnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;


procedure TFrmBlockFill.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;


procedure TFrmBlockFill.NumEditChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;


function TFrmBlockFill.ValidSettings: boolean;
var s : string;

  function Valid( cont : boolean; msg : string; Ctrl : TWinControl; var res : string ): boolean;
  begin
    Result := Cont;
    if not Result then
    begin
      res := msg;
      Ctrl.SetFocus;
    end;
  end;

begin
  s := '';
  if Valid( Trim(EdLeft.Text) <> '', 'Left is mandatory.', EdLeft, s) then
  if Valid( StrToInt(EdLeft.Text) < TileMap.Width , Format('Left should be lower or equal to %d.', [TileMap.Width-1]), EdLeft, s) then
  if Valid( Trim(EdTop.Text) <> '', 'Top is mandatory.', EdTop, s) then
  if Valid( StrToInt(EdTop.Text) < TileMap.Height , Format('Top should be lower or equal to %d.', [TileMap.Height-1]), EdTop, s) then
  if Valid( Trim(EdWidth.Text) <> '', 'Width is mandatory.', EdWidth, s) then
  if Valid( StrToInt(EdWidth.Text) > 0, 'Width should be higher than 0.', EdWidth, s) then
  if Valid( Trim(EdHeight.Text) <> '', 'Height is mandatory.', EdHeight, s) then
     Valid( StrToInt(EdHeight.Text) > 0, 'Height should be higher than 0.', EdHeight, s);

  Result := (s = '');
  if not Result then Msg( s, MsgError, MB_OK );
end;


procedure TFrmBlockFill.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i,j,l,t          : integer;
  CurTile,BaseTile : integer;

  procedure UpdateTileAndMove;
  begin
    if (i < TileMap.Height) and (j < TileMap.Width) then
      SetTileOfLocation(j,i,CurTile);

    Inc(CurTile);
    if (CurTile >= LstTiles.TileData.Count) then CurTile := 0;
  end;

begin

  if (ModalResult = mrOK) then
  begin
    (* check settings *)
    CanClose := ValidSettings;
    if CanClose then
    begin

      (* save settings *)
      with MapSettings do
      begin
        BlockFillPattern := cmbPattern.ItemIndex;
        BlockFillWidth   := StrToInt(EdWidth.Text);
        BlockFillHeight  := StrToInt(EdHeight.Text);
      end;

      t := StrToInt(EdTop.Text);
      l := StrToInt(EdLeft.Text);
      CurTile  := LstTiles.SelTile;
      BaseTile := CurTile;

      MapUndoCtrl.Mode := gbumTotal;
      (* write patterns *)
      case cmbPattern.ItemIndex of
        0 :
        (* selected tile *)
        with MapSettings do
        begin
          for i := t to t + BlockFillHeight-1 do
            for j := l to l + BlockFillWidth-1 do
              if (i < TileMap.Height) and (j < TileMap.Width) then
                SetTileOfLocation(j,i,CurTile);
        end;

        1 :
        (* Left to right *)
        with MapSettings do
          for i := t to t + BlockFillHeight-1 do
          begin
            CurTile := BaseTile;
            for j := l to l + BlockFillWidth-1 do
              UpdateTileAndMove;
          end;

        2 :
        (* Left to right, top to bottom *)
        with MapSettings do
          for i := t to t + BlockFillHeight-1 do
            for j := l to l + BlockFillWidth-1 do
              UpdateTileAndMove;

        3 :
        (* Top to bottom *)
        with MapSettings do
          for j := l to l + BlockFillWidth-1 do
          begin
            CurTile := BaseTile;
            for i := t to t + BlockFillHeight-1 do
              UpdateTileAndMove;
          end;

        4 :
        (* Top to bottom, Left to right *)
        with MapSettings do
          for j := l to l + BlockFillWidth-1 do
            for i := t to t + BlockFillHeight-1 do
              UpdateTileAndMove;

        5 :
        (* Right to left *)
        with MapSettings do
          for i := t + BlockFillHeight-1 downto t do
          begin
            CurTile := BaseTile;
            for j := l + BlockFillWidth-1 downto l do
              UpdateTileAndMove;
          end;

        6 :
        (* Right to Left, top to bottom *)
        with MapSettings do
          for i := t to t + BlockFillHeight-1 do
            for j := l + BlockFillWidth-1 downto l do
              UpdateTileAndMove;

        7 :
        (* Bottom to top *)
        with MapSettings do
          for j := l + BlockFillWidth-1 downto l do
          begin
            CurTile := BaseTile;
            for i := t + BlockFillHeight-1 downto t do
              UpdateTileAndMove;
          end;

        else
        (* Bottom to top , Right to Left *)
        with MapSettings do
          for j := l + BlockFillWidth-1 downto l do
            for i := t + BlockFillHeight-1 downto t do
              UpdateTileAndMove;
      end;

      FrmMap.GrdMap.Invalidate;
    end;
  end;

end;

end.
