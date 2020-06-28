unit tilecnt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TileEdit, gbConst, mainlib, GBTDMain;

type
  TFrmTileCnt = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    GroupBox1: TGroupBox;
    LblTileCnt: TLabel;
    EdTileCnt: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure EdChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FTileSize : integer;
    function GetMax: integer;
  public
    { Public declarations }
    property TileSize : integer read FTileSize write FTileSize;
  end;

var
  FrmTileCnt: TFrmTileCnt;

implementation

{$R *.DFM}

{--$I ..\hlp\gbtd.inc}


function TFrmTileCnt.GetMax: integer;
begin
  result := MAXTILECNT div ((CurTileWidth * CurTileHeight) div (8*8));
end;

procedure TFrmTileCnt.FormDestroy(Sender: TObject);
begin
  FrmTileCnt := nil;
end;

procedure TFrmTileCnt.EdChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;

procedure TFrmTileCnt.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var s : string;
var cnt : integer;
var x,y,i : integer;
begin

  if (ModalResult = mrOk) then
  begin
    if (Trim(EdTileCnt.Text) = '') then CanClose := False
    else
    begin
      cnt := StrToInt(EdTileCnt.Text);

      if ( cnt > GetMax) then s := Format( 'Tile count can not exceed %d.', [GetMax] );
      if ( cnt < 1) then s := 'Tile count should be at least 1.';

      if (s <> '') then
      begin
        MessageDlg( s, mtError, [mbOk], 0 );
        EdTileCnt.SetFocus;
        CanClose := False;
      end
      else
      begin
        (* determine new size *)
        i := 16 div ((CurTileWidth*CurTileHeight) div (8*8));
        FTileSize := ((cnt + i - 1) div i) * i;
      end;
    end;
  end;
end;

procedure TFrmTileCnt.FormShow(Sender: TObject);
begin
  LblTileCnt.Caption := Format( '&Tile count (%d maximum):', [GetMax] );
  EdTileCnt.Text := IntToStr( TileList.Count );

//  HelpContext := Tile_count;
end;

procedure TFrmTileCnt.BtnHelpClick(Sender: TObject);
begin
//  Application.HelpContext(Tile_Count);
end;

procedure TFrmTileCnt.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

end.
