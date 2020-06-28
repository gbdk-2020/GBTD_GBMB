unit tileedit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, gbpgrd, Buttons, ExtCtrls, gbconst, gbshow, ComCtrls,
  Menus,  gbcbtn, gbtlst, gboFile, Clipbrd, ExpBtn, IniFiles,
  fldfill, undo, MRUList, mainlib, Split, GBColorSetSelector, ColorController,
  Spin, GBColorCombo, GBTileExchanger;

type
  TFrmTile = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    PnlPixel: TPanel;
    ShpOne: TShape;
    ShwOne: TGBShowCase;
    GrdPixel: TGBPixelGrid;
    View1: TMenuItem;
    Grid1: TMenuItem;
    N8x81: TMenuItem;
    N8x161: TMenuItem;
    TileSize1: TMenuItem;
    N16x161: TMenuItem;
    N32x321: TMenuItem;
    ShwMult: TGBShowCase;
    ShpMult: TShape;
    Simple1: TMenuItem;
    N1: TMenuItem;
    LstTiles: TGBTileList;
    Edit1: TMenuItem;
    CutTile1: TMenuItem;
    N2: TMenuItem;
    FlipHorizontal1: TMenuItem;
    FlipVertical1: TMenuItem;
    PnlBtn: TPanel;
    LneBottom: TShape;
    LneTop: TShape;
    PnlEdit: TPanel;
    Panel1: TPanel;
    BtnLeftClick: TGBColorButton;
    Label1: TLabel;
    Panel2: TPanel;
    Label2: TLabel;
    BtnRightClick: TGBColorButton;
    DlgSave: TSaveDialog;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    DlgOpen: TOpenDialog;
    N3: TMenuItem;
    Exit1: TMenuItem;
    NReopen: TMenuItem;
    Copytile1: TMenuItem;
    Pastetile1: TMenuItem;
    N5: TMenuItem;
    Scrolltileleft1: TMenuItem;
    Scrolltileright1: TMenuItem;
    Scrolltileup1: TMenuItem;
    Scrolltiledown1: TMenuItem;
    LneBtmShdw: TShape;
    LneBtmHgh: TShape;
    About1: TMenuItem;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Exportto1: TMenuItem;
    Export1: TMenuItem;
    N6: TMenuItem;
    Cleartiles1: TMenuItem;
    Nibblemarkers1: TMenuItem;
    Help1: TMenuItem;
    Helptopics1: TMenuItem;
    N7: TMenuItem;
    Helpindex1: TMenuItem;
    PnlTools: TPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Undo1: TMenuItem;
    N8: TMenuItem;
    Design1: TMenuItem;
    Pen1: TMenuItem;
    Floodfill1: TMenuItem;
    Flipcolors1: TMenuItem;
    Importfrom1: TMenuItem;
    Rotateclockwise1: TMenuItem;
    Tilecount1: TMenuItem;
    N9: TMenuItem;
    SplitCopy1: TMenuItem;
    SplitPaste1: TMenuItem;
    SplitOptions: TMenuItem;
    N10: TMenuItem;
    mnuColorSetGameboy: TMenuItem;
    mnuColorSetStandard: TMenuItem;
    mnuColorset: TMenuItem;
    Setbookmark1: TMenuItem;
    mnuSet1: TMenuItem;
    mnuSet2: TMenuItem;
    mnuSet3: TMenuItem;
    N4: TMenuItem;
    Gotobookmark1: TMenuItem;
    mnuGoto1: TMenuItem;
    mnuGoto2: TMenuItem;
    mnuGoto3: TMenuItem;
    Palettes1: TMenuItem;
    mnuColorsetColor: TMenuItem;
    mnuColorsetSuper: TMenuItem;
    SbLColor: TSpinButton;
    GBColorCombo: TGBColorCombo;
    N11: TMenuItem;
    Autoupdate1: TMenuItem;
    MRUList: TMRUFileList;
    BtnOpen: TSpeedButton;
    BtnSave: TSpeedButton;
    BtnExport: TSpeedButton;
    BtnCut: TSpeedButton;
    BtnCopy: TSpeedButton;
    BtnPaste: TSpeedButton;
    BtnHelp: TSpeedButton;
    BtnAutoUpdate: TSpeedButton;
    BtnVertical: TSpeedButton;
    BtnHorizontal: TSpeedButton;
    BtnRotate: TSpeedButton;
    BtnScrollUp: TSpeedButton;
    BtnScrollLeft: TSpeedButton;
    BtnScrollRight: TSpeedButton;
    BtnScrollDown: TSpeedButton;
    BtnPenCursor: TSpeedButton;
    BtnFloodFillCursor: TSpeedButton;
    GameboyColorFiltered1: TMenuItem;
    procedure BtnScrollLeftClick(Sender: TObject);
    procedure BtnScrollRightClick(Sender: TObject);
    procedure BtnScrollUpClick(Sender: TObject);
    procedure BtnScrollDownClick(Sender: TObject);
    procedure SbLColorDownClick(Sender: TObject);
    procedure SbLColorUpClick(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure N8x81Click(Sender: TObject);
    procedure N16x161Click(Sender: TObject);
    procedure N8x161Click(Sender: TObject);
    procedure N32x321Click(Sender: TObject);
    procedure Simple1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GrdPixelPixelClick(x, y: Integer; Button: TMouseButton;
      var Update: TGBUpdateMode);
    procedure GrdPixelAfterPaint(x, y: Integer; Update: TGBUpdateMode);
    procedure CutTile1Click(Sender: TObject);
    procedure BtnVerticalClick(Sender: TObject);
    procedure PnlBtnResize(Sender: TObject);
    procedure BtnHorizontalClick(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exportto1Click(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure Cleartiles1Click(Sender: TObject);
    procedure Nibblemarkers1Click(Sender: TObject);
    procedure Helptopics1Click(Sender: TObject);
    procedure Helpindex1Click(Sender: TObject);
    procedure BtnPenCursorClick(Sender: TObject);
    procedure BtnFloodFillCursorClick(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Pen1Click(Sender: TObject);
    procedure Floodfill1Click(Sender: TObject);
    procedure MRUListCreate(Sender: TObject);
    procedure MRUListMRUItemClick(Sender: TObject; AFilename: TFileName);
    procedure Flipcolors1Click(Sender: TObject);
    procedure Importfrom1Click(Sender: TObject);
    procedure BtnRotateClick(Sender: TObject);
    procedure Tilecount1Click(Sender: TObject);
    procedure SplitOptionsClick(Sender: TObject);
    procedure SplitPaste1Click(Sender: TObject);
    procedure SplitCopy1Click(Sender: TObject);
    procedure ColorSetClick(Sender: TObject);
    procedure SetBookMark(Sender: TObject);
    procedure GotoBookMark(Sender: TObject);
    procedure Gotobookmark1Click(Sender: TObject);
    procedure FlipHorizontal1Click(Sender: TObject);
    procedure LstTilesAfterSelect(Sender: TObject);
    procedure Palettes1Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure GBColorComboColorSelect(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; clr: Integer);
    procedure GBColorComboChange(Sender: TObject);
    procedure Autoupdate1Click(Sender: TObject);

  private
    { Private declarations }

   CBC        : HWND;
   SelectorColors  : TGBColorType;

   procedure ScrollLeft;
   procedure ScrollRight;
   procedure ScrollUp;
   procedure ScrollDown;

   procedure Clear(i : integer);

   procedure FlipHorizontal;
   procedure FlipVertical;
   procedure Rotate;

   procedure ChangeColor(i : integer; Delta : integer);
   procedure ChangeColorSet( ClrIndex, Boundary, Delta : integer);

   procedure RebuildScreen;
   procedure SyncTiles;
   procedure RefreshClickBtns;
   procedure SetPalHint;
   procedure ChangeTileCount(cnt : integer);

   procedure CopyTileToClipboard;
   procedure PasteTileFromClipboard;

   Procedure HandleMessages ( Var Msg : tMsg; Var Handled : Boolean );
   function AppOnHelp(Command: Word; Data: Longint;   var CallHelp: Boolean): Boolean;
   procedure SetHelpContext;

   procedure WMChangeCBChain(var Message: TWMChangeCBChain); message WM_CHANGECBCHAIN;
   procedure WMDrawClipBoard(var Message: TWMNoParams ); message WM_DRAWCLIPBOARD;
   procedure SetPasteState;

   procedure TileExchangerChange( Chg : TGBTileExchangerChange; TileNr : integer);


  public
    { Public declarations }
    clrl, clrr : integer;

    procedure UpdateSelector;
    procedure RefreshSelector;
    procedure SetTileSize( t : TGBTileSize);
  end;

var
  FrmTile    : TFrmTile;

implementation

{$R *.DFM}

uses Export, about, import, TileCnt, SplitClip, Palette, Files, GBTDMain;

const FImportPal : TGBColorType = (clWhite, clLtGray, clGray, clBlack);

{--$I ..\hlp\gbtd.inc}

(**********************************************)
(*                                            *)
(*          Constructors & Destructors        *)
(*                                            *)
(**********************************************)



procedure TFrmTile.FormClose(Sender: TObject; var Action: TCloseAction);
var i : integer;
    p : PTileType;
begin

  if not HandleModified then
    Action := caNone
  else
  begin

    for i := 0 to (TileList.Count-1) do
    begin
      p := TileList.items[0];
      dispose(p);
      TileList.Delete(0);
    end;

    UndoCtrl.Free;
    IniFile.Free;
    TileExchanger.Free;

    ChangeClipboardChain(Self.Handle, CBC );

    GBColorController.Free;
  end;
end;



(**********************************************)
(*                                            *)
(*             Property handlers              *)
(*                                            *)
(**********************************************)



procedure TFrmTile.BtnScrollLeftClick(Sender: TObject);
begin
  ScrollLeft;
end;

procedure TFrmTile.BtnScrollRightClick(Sender: TObject);
begin
  ScrollRight;
end;

procedure TFrmTile.BtnScrollUpClick(Sender: TObject);
begin
  ScrollUp;
end;

procedure TFrmTile.BtnScrollDownClick(Sender: TObject);
begin
  ScrollDown;
end;


procedure TFrmTile.N8x81Click(Sender: TObject);
begin
  SetTileSize(gbts8x8);
end;

procedure TFrmTile.N16x161Click(Sender: TObject);
begin
  SetTileSize(gbts16x16);
end;

procedure TFrmTile.N8x161Click(Sender: TObject);
begin
  SetTileSize(gbts8x16);
end;

procedure TFrmTile.N32x321Click(Sender: TObject);
begin
  SetTileSize(gbts32x32);
end;


procedure TFrmTile.Grid1Click(Sender: TObject);
begin
  GrdPixel.GridLines := not Grid1.Checked;
  Grid1.Checked := not Grid1.Checked;
  Modified := True;
  UndoCtrl.PixelEnd;
end;


procedure TFrmTile.Simple1Click(Sender: TObject);
begin
  Simple1.Checked := not Simple1.Checked;
  Modified := True;
  RebuildScreen;
  UndoCtrl.PixelEnd;
end;

procedure TFrmTile.CutTile1Click(Sender: TObject);
begin
  CopyTileToClipboard;
  Clear(LstTiles.SelTile);
  Modified := True;
  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
end;


procedure TFrmTile.BtnVerticalClick(Sender: TObject);
begin
  FlipVertical;
end;


procedure TFrmTile.SaveAs1Click(Sender: TObject);
begin
  SaveToGBR(False);
end;


procedure TFrmTile.Save1Click(Sender: TObject);
begin
  SaveToGBR(True);
end;


procedure TFrmTile.Open1Click(Sender: TObject);
begin
  LoadFromGBR('');
end;


(**********************************************)
(*                                            *)
(*         Extended Property handlers         *)
(*                                            *)
(**********************************************)



procedure TFrmTile.SbLColorDownClick(Sender: TObject);
begin
  case GBColorController.ColorMode of
    gbcmPocket,
    gbcmGameboy  : ChangeColor(GBColorCombo.SelColor, 1);
    gbcmSGB      : ChangeColorSet( 1, 3, 1 );
    gbcmGBC,
    gbcmGBCFiltered : ChangeColorSet( 0, 7, 1 );
  end;
end;

procedure TFrmTile.SbLColorUpClick(Sender: TObject);
begin
  case GBColorController.ColorMode of
    gbcmPocket,
    gbcmGameboy  : ChangeColor(GBColorCombo.SelColor, -1);
    gbcmSGB      : ChangeColorSet( 1, 3, -1 );
    gbcmGBC,
    gbcmGBCFiltered : ChangeColorSet( 0, 7, -1 );
  end;
end;



procedure TFrmTile.ChangeColorSet( ClrIndex, Boundary, Delta : integer);
var i : integer;
begin
  i := PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[ClrIndex];

  if (i+Delta < 0) then i := Boundary
  else
    if (i+Delta > Boundary) then i := 0
    else
      i := i + Delta;

  PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[ClrIndex] := i;


  Modified := True;
  RefreshSelector;
  GrdPixel.Paint;
  UndoCtrl.PixelEnd;
end;


procedure TFrmTile.ChangeColor(i : integer; Delta : integer);
var Pal : TGBPaletteType;
begin

  Pal := GBColorController.Palette;

  if (Pal[i]+Delta < 0) then pal[i] := 3
  else
    if (pal[i]+Delta > 3) then pal[i] := 0
    else
      pal[i] := pal[i] + Delta;

  GBColorController.Palette := Pal;

  Modified := True;
  RefreshSelector;
  GrdPixel.Paint;
  LstTiles.Paint;
  UndoCtrl.PixelEnd;
  TileExchanger.PalToExchange;
end;





procedure TFrmTile.GrdPixelPixelClick(x, y: Integer; Button: TMouseButton;
  var Update: TGBUpdateMode);
var p   : PTileType;
var w,h : integer;
begin
  p := TileList.Items[lstTiles.SelTile];

  if BtnPenCursor.Down then
  begin
    UndoCtrl.LogPixel(x,y, p.data[y,x] );

    if Button = mbLeft then
      p.data[y,x] := clrl
    else p.data[y,x] := clrr;

    Update := gbumPixel;
    Modified := True;
    TileExchanger.TileToExchange( LstTiles.SelTile);
  end
  else
  begin
    UndoCtrl.LogTotal(p);
//    DetermineTileSize(TileSize, w, h);

    if Button = mbLeft then
      FloodFill(x,y, clrl, p, CurTileWidth, CurTileHeight )
    else
      FloodFill(x,y, clrr, p, CurTileWidth, CurTileHeight );

    UpDate := gbumTotal;
    TileExchanger.TileToExchange( LstTiles.SelTile);
  end;
end;


procedure TFrmTile.GrdPixelAfterPaint(x, y: Integer; Update: TGBUpdateMode);
begin
  if (Update = gbumTotal) then
  begin
    ShwOne.Paint;
    ShwMult.Paint;
    LstTiles.PaintTile(LstTiles.SelTile);
//    LstTiles.Paint;
  end
  else
  begin
    ShwOne.DrawPixel(x,y);
    ShwMult.DrawPixel(x,y);
    LstTiles.DrawPixel(x,y);
  end;
end;



(**********************************************)
(*                                            *)
(*             Clipboard Support              *)
(*                                            *)
(**********************************************)


procedure TFrmTile.WMChangeCBChain(var Message: TWMChangeCBChain);
begin
  with Message do
  begin
    if (Remove = CBC) then CBC := Next;
    if (CBC <> NULL) then SendMessage(CBC, WM_CHANGECBCHAIN, Remove, Next );
  end;
end;


procedure TFrmTile.WMDrawClipBoard(var Message: TWMNoParams );
begin
  SetPasteState;

  if (CBC <> NULL) then SendMessage(CBC, WM_DRAWCLIPBOARD, 0, 0 );
end;


procedure TFrmTile.SetPasteState;
var b : boolean;
begin
  b := Clipboard.HasFormat(CF_BITMAP);;
  BtnPaste.Enabled := b;
  PasteTile1.Enabled := b;
  SplitPaste1.Enabled := b;
end;



(**********************************************)
(*                                            *)
(*             Support functions              *)
(*                                            *)
(**********************************************)



procedure TFrmTile.SyncTiles;
begin
  UpdateSelector;

  ShwOne.TileData := TileList.Items[LstTiles.SelTile];
  ShwMult.TileData := TileList.Items[LstTiles.SelTile];
  GrdPixel.TileData := TileList.Items[LstTiles.SelTile];

  UndoCtrl.ChangeTile(LstTiles.SelTile);
end;

procedure TFrmTile.UpdateSelector;
begin
  with GBColorController do
  begin
    SelectorColors := CurColorSets[0,PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[ColorSetIndex]];
    GBColorCombo.ItemIndex := PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[ColorSetIndex];
  end;

  GBColorCombo.Refresh;
  SetPalHint;
  RefreshClickBtns;
end;

procedure TFrmTile.RefreshSelector;
begin
  UpdateSelector;

  TileExchanger.TileToExchange(LstTiles.SelTile);
end;


procedure TFrmTile.RefreshClickBtns;
begin
  with BtnLeftClick do
  begin
    Color := SelectorColors[clrl];
    Caption := IntToStr(clrl);
    Paint;
  end;

  with BtnRightClick do
  begin
    Color := SelectorColors[clrr];
    Caption := IntToStr(clrr);
    Paint;
  end;

  UndoCtrl.PixelEnd;
end;




procedure TFrmTile.RebuildScreen;
var MenuSize : integer;
begin
  GrdPixel.Freezed := True;
  ShwOne.Freezed := True;
  ShwMult.Freezed := True;
  LstTiles.Freezed := True;

  case CurTileSize of
    gbts8x8 :
      begin
        GrdPixel.PixelSize := 24;
        LstTiles.TileCount := 13;

        if Simple1.Checked then
        begin
          ShwOne.Top := 88;
          ShwOne.Left := 236;
        end
        else
        begin
          ShwOne.Top := 40;
          ShwOne.Left := 272;

          ShwMult.Top := 100;
          ShwMult.Left := 236;
          ShwMult.TileCount := 4;
        end;

      end;

    gbts8x16 :
      begin
        GrdPixel.PixelSize := 24;
        LstTiles.TileCount := 12;

        if Simple1.Checked then
        begin
          ShwOne.Top := 172;
          ShwOne.Left := 256;
        end
        else
        begin
          ShwOne.Top := 77;
          ShwOne.Left := 292;

          ShwMult.Top := 196;
          ShwMult.Left := 256;
          ShwMult.TileCount := 4;
        end;
      end;

    gbts16x16 :
      begin
        GrdPixel.PixelSize := 14;
        LstTiles.TileCount := 7;

        if Simple1.Checked then
        begin
          ShwOne.Top := 84;
          ShwOne.Left := 282;
        end
        else
        begin
          ShwOne.Top := 17;
          ShwOne.Left := 330;

          ShwMult.Top := 84;
          ShwMult.Left := 282;
          ShwMult.TileCount := 3;
        end;
      end;


    gbts32x32 :
      begin
        GrdPixel.PixelSize := 8;
        LstTiles.TileCount := 4;

        if Simple1.Checked then
        begin
          ShwOne.Top := 86;
          ShwOne.Left := 298;
        end
        else
        begin
          ShwOne.Top := 5;
          ShwOne.Left := 346;

          ShwMult.Top := 105;
          ShwMult.Left := 298;
          ShwMult.TileCount := 2;
        end;
      end;
  end;

  GrdPixel.TileSize := CurTileSize;

  ShwOne.TileSize := CurTileSize;
  ShpOne.SetBounds(ShwOne.Left-1, ShwOne.Top-1, ShwOne.Width+2, ShwOne.Height+2);

  PnlEdit.SetBounds( GrdPixel.Left + ((GrdPixel.Width-PnlEdit.Width) div 2),
                     GrdPixel.Top + GrdPixel.Height + 3,
                     PnlEdit.Width, PnlEdit.Height);

  if Simple1.Checked then
  begin
    ShwMult.Visible := False;
    PnlPixel.Setbounds(0,PnlBtn.Top+PnlBtn.Height+2, ShwOne.Left + ShwOne.Width + 5, PnlEdit.Top + PnlEdit.Height + 4);
  end
  else
  begin
    ShwMult.Visible := True;
    ShwMult.TileSize := CurTileSize;
    ShpMult.SetBounds(ShwMult.Left-1, ShwMult.Top-1, ShwMult.Width+2, ShwMult.Height+2);
    if (CurTileSize = gbts32x32)  then
      PnlPixel.Setbounds(0, PnlBtn.Top+PnlBtn.Height+2, ShpMult.Left + ShpMult.Width + 5, ShpMult.Top + ShpMult.Height + 4)
    else
      PnlPixel.Setbounds(0, PnlBtn.Top+PnlBtn.Height+2, ShpMult.Left + ShpMult.Width + 5, PnlEdit.Top + PnlEdit.Height + 4);
  end;
  ShpMult.Visible := ShwMult.Visible;
  LstTiles.TileSize := CurTileSize;

  LstTiles.SetBounds( PnlPixel.Left + PnlPixel.Width + 3, PnlPixel.Top + ((PnlPixel.Height-LstTiles.Height) div 2), LstTiles.Width, LstTiles.Height);


  MenuSize := GetSystemMetrics( SM_CYCAPTION ) + GetSystemMetrics( SM_CYMENU ) + 6;
  Self.SetBounds( Self.Left, Self.Top, LstTiles.Left + LstTiles.Width + 5, PnlPixel.Top + PnlPixel.Height + MenuSize);

  GrdPixel.Freezed := False;
  ShwOne.Freezed := False;
  ShwMult.Freezed := False;
  LstTiles.Freezed := False;
  GrdPixel.Paint;
  LstTiles.Paint;
end;


procedure TFrmTile.SetTileSize( t : TGBTileSize );
begin
  ConvertTileSize(CurTileSize, t, TileList);
  LstTiles.Freezed := True;
  LstTiles.SelTile := 0;
  LstTiles.Freezed := False;

  SyncTiles;
  CurTileSize := t;
  DetermineTileSize(t, CurTileWidth, CurTileHeight);

  N8x81.Checked := (t = gbts8x8);
  N8x161.Checked := (t = gbts8x16);
  N16x161.Checked := (t = gbts16x16);
  N32x321.Checked := (t = gbts32x32);

  (* rotation not available in 8x16 *)
  BtnRotate.Enabled := (t <> gbts8x16);
  RotateClockwise1.Enabled := (t <> gbts8x16);

  RebuildScreen;
  Modified := True;
  TileExchanger.TileSize := t;
  TileExchanger.DimToExchange;
end;


procedure TFrmTile.Cleartiles1Click(Sender: TObject);
var i : integer;
begin
  if HandleModified then
  begin
    (* clear tiles *)
    for i :=0 to TileList.Count-1 do Clear(i);

    (* refresh *)
    GrdPixel.Paint;
    LstTiles.Paint;
    TileExchanger.TotalToExchange;
    Modified := True;
    UndoCtrl.Clear;
  end;
end;


(**********************************************)
(*                                            *)
(*                Main functions              *)
(*                                            *)
(**********************************************)


procedure TFrmTile.ScrollLeft;
var b        : array[0..31] of byte;
    x,y, i,j : integer;
    p        : PTileType;
begin
  x := CurTileWidth-1;
  y := CurTileHeight-1;
  p := TileList.Items[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  (* Store first line *)
  for i:=0 to y do b[i] := p.data[i,0];

  for i:=0 to x-1 do
    for j:=0 to y do
      p.data[j,i] := p.data[j,i+1];

  for i:=0 to y do p.data[i,x] := b[i];

  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
  Modified := True;
end;

procedure TFrmTile.ScrollRight;
var b        : array[0..31] of byte;
    x,y, i,j : integer;
    p        : PTileType;
begin
  x := CurTileWidth-1;
  y := CurTileHeight-1;
  p := TileList.Items[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  (* Store first line *)
  for i:=0 to y do b[i] := p.data[i,x];

  for i:=x-1 downto 0 do
    for j:=0 to y do
      p.data[j, i+1] := p.data[j, i];

  for i:=0 to y do p.data[i,0] := b[i];

  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
  Modified := True;
end;


procedure TFrmTile.ScrollUp;
var b        : array[0..31] of byte;
    x,y, i,j : integer;
    p        : PTileType;
begin
  x := CurTileWidth-1;
  y := CurTileHeight-1;
  p := TileList.Items[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  (* Store first line *)
  for i:=0 to x do b[i] := p.data[0,i];

  for i:=1 to y do
    for j:=0 to x do
      p.data[i-1, j] := p.data[i, j];

  for i:=0 to x do p.data[y,i] := b[i];

  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
  Modified := True;
end;


procedure TFrmTile.ScrollDown;
var b        : array[0..31] of byte;
    x,y, i,j : integer;
    p        : PTileType;
begin
  x := CurTileWidth-1;
  y := CurTileHeight-1;
  p := TileList.Items[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  (* Store first line *)
  for i:=0 to x do b[i] := p.data[y,i];

  for i:=y downto 1 do
    for j:=0 to x do
      p.data[i, j] := p.data[i-1, j];

  for i:=0 to x do p.data[0,i] := b[i];

  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
  Modified := True;
end;


procedure TFrmTile.Clear(i : integer);
var j : integer;
    p : PTileType;
begin
  p := TileList.Items[i];
  UndoCtrl.LogTotal(p);

  ClearBuf( @p.data, 32*32 );
end;

procedure TFrmTile.FlipHorizontal;
var b        : TileType;
    x,y, i,j : integer;
    p        : PTileType;
begin
  x := CurTileWidth-1;
  y := CurTileHeight-1;
  p := TileList.Items[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  (* Make scratch *)
  for i:=0 to y do
    for j:=0 to x do
      b.data[i,j] := p.data[i,j];

  (* Flip *)
  for i :=0 to y do
    for j :=0 to x do
      p.data[y-i,j] := b.data[i,j];

  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
  Modified := True;
end;


procedure TFrmTile.Rotate;
var b        : TileType;
    x,y, i,j : integer;
    p        : PTileType;
begin
  x := CurTileWidth-1;
  y := CurTileHeight-1;
  p := TileList.Items[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  (* Make scratch *)
  for i:=0 to y do
    for j:=0 to x do
      b.data[j,i] := p.data[j,i];

  (* Rotate *)
  for i :=0 to y do
    for j :=0 to x do
      p.data[j,i] := b.data[y-i,j];

  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
  Modified := True;
end;
                      

procedure TFrmTile.FlipVertical;
var b        : TileType;
    x,y, i,j : integer;
    p        : PTileType;
begin
  x := CurTileWidth-1;
  y := CurTileHeight-1;
  p := TileList.Items[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  (* Make scratch *)
  for i:=0 to y do                                                        
    for j:=0 to x do
      b.data[i,j] := p.data[i,j];

  (* Flip *)
  for j :=0 to x do
    for i :=0 to y do
      p.data[i,x-j] := b.data[i,j];

  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
  Modified := True;
end;

procedure TFrmTile.PnlBtnResize(Sender: TObject);
begin
  LneTop.Width := PnlBtn.Width+2;
  LneBottom.Width := PnlBtn.Width+2;
  LneBtmShdw.Width := PnlBtn.Width+2;
  LneBtmHgh.Width := PnlBtn.Width+2;
end;

procedure TFrmTile.BtnHorizontalClick(Sender: TObject);
begin
  FlipHorizontal;
end;



procedure TFrmTile.Exit1Click(Sender: TObject);
begin
  Close;
end;



procedure TFrmTile.FormCreate(Sender: TObject);
var i,j : integer;
    p : PTileType;
var s : string;
    b : boolean;
begin
  GBColorController := TGBColorController.Create(self);
  TileExchanger := TGBTileExchanger.Create(Self, False);
  TileExchanger.OnChange := TileExchangerChange;

  Application.OnMessage := HandleMessages;
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'gbtd.hlp';
  Application.OnHelp := AppOnHelp;
  SetTitle('');

  (* set shortcuts *)
  SplitCopy1.ShortCut := ShortCut(Word('C'), [ssShift,ssCtrl]);
  SplitPaste1.ShortCut := ShortCut(Word('V'), [ssShift,ssCtrl]);

  mnuSet1.ShortCut := ShortCut( Word('1'), [ssAlt] );
  mnuSet2.ShortCut := ShortCut( Word('2'), [ssAlt] );
  mnuSet3.ShortCut := ShortCut( Word('3'), [ssAlt] );

  mnuGoto1.ShortCut := ShortCut( Word('1'), [ssCtrl] );
  mnuGoto2.ShortCut := ShortCut( Word('2'), [ssCtrl] );
  mnuGoto3.ShortCut := ShortCut( Word('3'), [ssCtrl] );


  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'gbtd.ini');
  MRUList.AutoSaveName := IniFile.FileName;
  FileName := '';

  (* setup clipboard responses *)
  CBC := SetClipboardViewer(Self.Handle);


  with TileExport do
  begin
    TileID := 1;
    FileName[0] := char(0);
    FileType := 0;
    SectionName[0] := char(0);
    LabelName[0] := char(0);
    Bank := 0;
    TileArray := True;
    Counter := 0;
    From := 0;
    Upto := 0;
    IncludeColors := False;
    SGBPalettes   := 0;
    CGBPalettes   := 0;
  end;

  with TileImport do
  begin
    TileID := 1;
    FileName[0] := char(0);
    FileType := 0;
    FromTile := 0;
    ToTile := 0;
    TileCount := 127;
    ColorConversion := 0;
  end;


  with TileSettings do
  begin
    SplitWidth := 1;
    SplitHeight := 1;
    SplitOrder := soLeftRightTopBottom;
  end;

  TileList := TList.Create;
  for i := 0 to 127 do
  begin
    New(p);
    p.ColorSet[0] := 0;
    p.ColorSet[1] := 0;
    ClearBuf(p, 32*32);
    TileList.Add(p);
  end;

  UndoCtrl := TGBTDUndo.Create( TileList, 0, Undo1 );

  with LstTiles do
  begin
    TileData := TileList;
    SelTile := 0;
  end;

  ShwOne.TileData := TileList.Items[0];
  ShwMult.TileData := TileList.Items[0];
  GrdPixel.TileData := TileList.Items[0];
  GBColorCombo.ColorSet := @SelectorColors;

  TileExchanger.TileData := TileList;


  (* load defaults from ini-file *)
  Simple1.Checked := IniFile.ReadBool('General', 'ViewSimple', False );

  Grid1.Checked := IniFile.ReadBool('General', 'ViewGrid', True );
  GrdPixel.GridLines := Grid1.Checked;

  NibbleMarkers1.Checked := IniFile.ReadBool('General', 'ViewNibblemarkers', False );
  GrdPixel.NibbleMarkers := NibbleMarkers1.Checked;

  SetColorSet( IniFile.ReadInteger('General', 'ViewColorSet', 0 ) );

  SetTileSize(gbts8x8);

  GrdPixel.Paint;
  Modified := False;

  (* Check for switches *)
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    if (s[1] = '-') then
    begin
      MessageDlg('This switch is not supported by this version.', mtInformation, [mbOk], 0);
      Halt;              (* ! dirty ! *)
    end
  end;


  if (ParamCount > 0) then
    LoadFromGBR(ParamStr(1));

  UndoCtrl.ChangeTile(LstTiles.SelTile);

  SetHelpContext;
end;


procedure TFrmTile.PasteTileFromClipboard;
var Bmp : TBitmap;
var x,y, sx,sy : integer;
var p : PTileType;
begin

  if Clipboard.HasFormat(CF_BITMAP) then
  begin

    Bmp := TBitmap.Create;
    try

      sx := CurTileWidth;
      sy := CurTileHeight;

      Bmp.Assign(Clipboard);

      with Bmp do
      begin
        Width := sx;
        Height := sy;
        p := TileList.Items[LstTiles.SelTile];
        UndoCtrl.LogTotal(p);

        for y := 0 to sy-1 do
          for x := 0 to sx-1 do
            p.data[y,x] := ColorToInt(NormalizeClipColor(Canvas.pixels[x,y]), GBColorController.Palette, FImportPal);
      end;

    finally
      Bmp.Free;
    end;

    Modified := True;
    GrdPixel.Paint;
    TileExchanger.TileToExchange(LstTiles.SelTile);
  end;

end;


procedure TFrmTile.CopyTileToClipboard;
var Pic : TBitmap;
var x,y, sx,sy : integer;
var p : PTileType;
begin

  Pic := TBitmap.Create;
  try

    sx := CurTileWidth;
    sy := CurTileHeight;

    with Pic do
    begin
      Width := sx;
      Height := sy;
      p := TileList.Items[LstTiles.SelTile];
      UndoCtrl.LogTotal(p);

      for y := 0 to sy-1 do
        for x := 0 to sx-1 do
          Canvas.pixels[x,y] := IntToColor(p.data[y,x], GBColorController.Palette, @FImportPal);

      Clipboard.Assign(Pic);
    end;

  finally
    Pic.Free;
  end;
end;

procedure TFrmTile.BtnCopyClick(Sender: TObject);
begin
  CopyTileToClipboard;
end;

procedure TFrmTile.BtnPasteClick(Sender: TObject);
begin
  PasteTileFromClipboard;
end;

procedure TFrmTile.About1Click(Sender: TObject);
begin
  if not Assigned(FrmAbout) then FrmAbout := TFrmAbout.Create(Application);
  FrmAbout.ShowModal;
  UndoCtrl.PixelEnd;
end;


procedure TFrmTile.HandleMessages ( Var Msg : tMsg; Var Handled : Boolean );
begin
  if ( Msg.Message = WM_KeyDown ) And
     ( Msg.wParam In [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END] ) and
     ( Screen.ActiveForm = FrmTile) Then
  begin
    case Msg.wParam Of
      VK_UP    : LstTiles.SelTile := LstTiles.SelTile - 1;
      VK_DOWN  : LstTiles.SelTile := LstTiles.SelTile + 1;
      VK_PRIOR : LstTiles.SelTile := LstTiles.SelTile - LstTiles.TileCount;
      VK_NEXT  : LstTiles.SelTile := LstTiles.SelTile + LstTiles.TileCount;
      VK_HOME  : LstTiles.SelTile := 0;
      VK_END   : LstTiles.SelTile := LstTiles.TileData.Count-1;
    end;
    Handled := True;
  end;
end;



procedure TFrmTile.Exportto1Click(Sender: TObject);
begin
  if not Assigned(FrmExport) then FrmExport := TFrmExport.Create(Application);
  FrmExport.ShowModal;
  UndoCtrl.PixelEnd;
  SyncTiles;
end;


procedure TFrmTile.Export1Click(Sender: TObject);
begin
  if not Assigned(FrmExport) then FrmExport := TFrmExport.Create(Application);
  FrmExport.StartExport;
  UndoCtrl.PixelEnd;
  SyncTiles;
end;


procedure TFrmTile.Nibblemarkers1Click(Sender: TObject);
begin
  GrdPixel.NibbleMarkers := not NibbleMarkers1.Checked;
  NibbleMarkers1.Checked := not NibbleMarkers1.Checked;
  Modified := True;
  UndoCtrl.PixelEnd;
end;

procedure TFrmTile.Helptopics1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
  UndoCtrl.PixelEnd;
end;

procedure TFrmTile.Helpindex1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_PARTIALKEY, LongInt(PChar('')) );
  UndoCtrl.PixelEnd;
end;

procedure TFrmTile.BtnPenCursorClick(Sender: TObject);
begin
  GrdPixel.Cursor := crDefault;
  Pen1.Checked := True;
  UndoCtrl.PixelEnd;
end;

procedure TFrmTile.BtnFloodFillCursorClick(Sender: TObject);
begin
  GrdPixel.Cursor := crDrag;
  FloodFill1.Checked := True;
  UndoCtrl.PixelEnd;
end;


procedure TFrmTile.Undo1Click(Sender: TObject);
begin
  UndoCtrl.Undo;
  GrdPixel.Paint;
  TileExchanger.TileToExchange(LstTiles.SelTile);
end;


procedure TFrmTile.Pen1Click(Sender: TObject);
begin
  BtnPenCursor.Click;
  BtnPenCursor.Down := True;
end;


procedure TFrmTile.Floodfill1Click(Sender: TObject);
begin
  BtnFloodFillCursor.Click;
  BtnFloodFillCursor.Down := True;
end;

procedure TFrmTile.View1Click(Sender: TObject);
begin
  Palettes1.Enabled := (GBColorController.ColorMode in [gbcmSGB,gbcmGBC,gbcmGBCFiltered]);
  Autoupdate1.Checked := TileExchanger.Active;
end;

procedure TFrmTile.MRUListCreate(Sender: TObject);
begin
  MRUList.AutoSaveName := ExtractFilePath(Application.ExeName) + 'gbtd.ini';
end;


procedure TFrmTile.MRUListMRUItemClick(Sender: TObject; AFilename: TFileName);
begin
  LoadFromGBR(AFilename);
end;


procedure TFrmTile.Flipcolors1Click(Sender: TObject);
var p : PTileType;
var i,j : integer;
begin
  p := LstTiles.TileData[LstTiles.SelTile];
  UndoCtrl.LogTotal(p);

  for i := 0 to 31 do
    for j := 0 to 31 do
    begin
      if (p.data[i,j] = clrl) then p.data[i,j] := clrr else
      if (p.data[i,j] = clrr) then p.data[i,j] := clrl
    end;

  GrdPixel.Paint;
  TileExchanger.TileToExchange( LstTiles.SelTile );
  Modified := True;
end;


procedure TFrmTile.SetHelpContext;
begin

//  Export1.HelpContext := _Export__vs__Export_to_;
//  ExportTo1.HelpContext := _Export__vs__Export_to_;
//  ImportFrom1.HelpContext := Import_options;

//  CutTile1.HelpContext := Cutting_and_Pasting;
//  CopyTile1.HelpContext := Cutting_and_Pasting;
//  PasteTile1.HelpContext := Cutting_and_Pasting;
//  SplitCopy1.HelpContext := Split_Copy_and_Paste;
//  SplitPaste1.HelpContext := Split_Copy_and_Paste;
//  SplitOptions.HelpContext := Split_options;

//  Pen1.HelpContext := Draw_types;
//  FloodFill1.HelpContext := Draw_types;
//  FlipVertical1.HelpContext := Flipping_tiles;
//  FlipHorizontal1.HelpContext := Flipping_tiles;
//  Rotateclockwise1.HelpContext := Rotating_tiles;

//  ScrollTileLeft1.HelpContext := Scrolling_tiles;
//  ScrollTileRight1.HelpContext := Scrolling_tiles;
//  ScrollTileUp1.HelpContext := Scrolling_tiles;
//  ScrollTileDown1.HelpContext := Scrolling_tiles;
//  ClearTiles1.HelpContext := Clear_tiles;
//  FlipColors1.HelpContext := Flip_colors;

//  TileSize1.HelpContext := Tile_size;
//  TileCount1.HelpContext := Tile_count;
//  Simple1.HelpContext := Simple;
//  Grid1.HelpContext := Grid;
//  NibbleMarkers1.HelpContext := Nibble_markers;
//  AutoUpdate1.HelpContext := Auto_update;
//  mnuColorSet.HelpContext := Color_set;
//  Palettes1.HelpContext := Palettes;
//  SetBookmark1.HelpContext := Bookmarks;
//  GotoBookmark1.HelpContext := Bookmarks;

//  Help1.HelpContext := Contacting_me_and_others;
//  HelpTopics1.HelpContext := Contacting_me_and_others;
//  HelpIndex1.HelpContext := Contacting_me_and_others;
//  About1.HelpContext := Contacting_me_and_others;

//  HelpContext := Using_the_mouse;
end;


function TFrmTile.AppOnHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
begin
  CallHelp := (Data <> 0) or (Command <> HELP_CONTEXT);
end;


procedure TFrmTile.Importfrom1Click(Sender: TObject);
begin
  if not Assigned(FrmImport) then FrmImport := TFrmImport.Create(Application);
  FrmImport.ShowModal;

  SyncTiles;
  GrdPixel.Paint;
  UndoCtrl.Clear;
end;



procedure TFrmTile.BtnRotateClick(Sender: TObject);
begin
  Rotate;
end;


procedure TFrmTile.Tilecount1Click(Sender: TObject);
begin
  if not Assigned(FrmTileCnt) then FrmTileCnt := TFrmTileCnt.Create(Self);

  FrmTileCnt.ShowModal;
  if (FrmTileCnt.ModalResult = mrOk) then
  begin
    try
      ChangeTileCount( FrmTileCnt.TileSize );
      FrmTileCnt.Release;
    finally
      UndoCtrl.Clear;
      Modified := True;
    end;
  end;
end;


procedure TFrmTile.ChangeTileCount(cnt : integer);
var cur, i,j : integer;
    p : PTileType;
begin
  cur := TileList.Count;
  if (cnt < cur) then
  begin
    if ( MessageDlg( Format( 'The requested tile count (%d) is lower than the current (%d). The remaining tiles will be lost. Do you want to continue ?',
                     [cnt, cur] ), mtConfirmation, [mbYes, mbNo],0) = mrYes) then
    begin
      (* delete tiles *)
      for i := cur-1 downto cnt do
      begin
        dispose( PTileType(TileList.Items[i]));
        TileList.Delete(i);
      end;

      LstTiles.RefreshList;
    end;
  end
  else
    if (cnt > cur) then
    begin
      (* add tiles *)
      for i := cur to cnt-1 do
      begin
        New(p);
        for j:= 0 to (32*32)-1 do p.data[j mod 32, j div 32] := 0;
        p.ColorSet[0] := 0;
        p.ColorSet[1] := 0;

        TileList.Add(p);
      end;

      LstTiles.RefreshList;
    end;

  TileExchanger.ListToExchange;
end;


procedure TFrmTile.SplitOptionsClick(Sender: TObject);
begin
  if not Assigned(FrmSplitClip) then FrmSplitClip := TFrmSplitClip.Create(Application);
  with FrmSplitClip do
  begin

    (* load current settings *)
    with TileSettings do
    begin
      TileWidth  := SplitWidth;
      TileHeight := SplitHeight;
      TileOrder  := SplitOrder;

      if (ShowModal = mrOK) then
      begin
        (* save current settings *)
        SplitWidth := TileWidth;
        SplitHeight := TileHeight;
        SplitOrder := TileOrder;


        case SplitFunction of
        sfPaste :  SplitPaste( TileList, LstTiles.SelTile, SplitWidth, SplitHeight,
                               CurTileWidth, CurTileHeight, SplitOrder,
                               GBColorController.Palette, FImportPal );
        else       SplitCopy(  TileList, LstTiles.SelTile, SplitWidth, SplitHeight,
                               CurTileWidth, CurTileHeight, SplitOrder,
                               GBColorController.Palette, FImportPal );
        end;

       (* refresh *)
       GrdPixel.Paint;
       LstTiles.Paint;
       Modified := True;
       UndoCtrl.Clear;
      end;
    end;

    Release;
  end;
end;


procedure TFrmTile.SplitPaste1Click(Sender: TObject);
var x,y,w,h : integer;
begin
  x := CurTileWidth;
  y := CurTileHeight;

  (* load current settings *)
  with TileSettings do
  begin
    if GetClipboardBitmapSize(w, h) then
    begin
      SplitWidth := (w+x-1) div x;
      SplitHeight := (h+y-1) div y;
    end;

    SplitPaste( TileList, LstTiles.SelTile, SplitWidth, SplitHeight, x, y,
                SplitOrder, GBColorController.Palette, FImportPal );
  end;

  (* refresh *)
  GrdPixel.Paint;
  LstTiles.Paint;
  Modified := True;
  UndoCtrl.Clear;
end;


procedure TFrmTile.SplitCopy1Click(Sender: TObject);
begin
  with TileSettings do
    SplitCopy( TileList, LstTiles.SelTile, SplitWidth, SplitHeight, CurTileWidth,
               CurTileHeight, SplitOrder, GBColorController.Palette, FImportPal );

  (* refresh *)
  GrdPixel.Paint;
  Modified := True;
  UndoCtrl.Clear;
end;


procedure TFrmTile.ColorSetClick(Sender: TObject);
begin
  SetColorset(TComponent(Sender).Tag);
end;

procedure TFrmTile.SetBookMark(Sender: TObject);
begin
  LstTiles.Bookmarks[TComponent(Sender).Tag] := LstTiles.SelTile;
  Modified := True;
end;

procedure TFrmTile.GotoBookMark(Sender: TObject);
begin
  LstTiles.SelTile := LstTiles.Bookmarks[TComponent(Sender).Tag];
end;

procedure TFrmTile.Gotobookmark1Click(Sender: TObject);
begin
  mnuGoto1.Enabled := (LstTiles.Bookmarks[0] <> -1);
  mnuGoto2.Enabled := (LstTiles.Bookmarks[1] <> -1);
  mnuGoto3.Enabled := (LstTiles.Bookmarks[2] <> -1);
end;

procedure TFrmTile.FlipHorizontal1Click(Sender: TObject);
begin
  FlipHorizontal;
end;

procedure TFrmTile.LstTilesAfterSelect(Sender: TObject);
begin
  SyncTiles;
  GrdPixel.Paint;
end;

procedure TFrmTile.Palettes1Click(Sender: TObject);
begin
  if not Assigned(FrmPalette) then FrmPalette := TFrmPalette.Create(Application);
  FrmPalette.ShowModal;
  UndoCtrl.PixelEnd;
  RefreshSelector;
  LstTiles.Paint
end;

procedure TFrmTile.SetPalHint;
begin
  case GBColorController.ColorMode of
    gbcmPocket,
    gbcmGameboy : GBColorCombo.Hint := '';
    gbcmSGB     : GBColorCombo.Hint := 'SGB Palette ' + IntToStr(PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[1]);
    gbcmGBC,
    gbcmGBCFiltered     : GBColorCombo.Hint := 'GBC Palette ' + IntToStr(PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[0]);
  end;
end;









procedure TFrmTile.GBColorComboColorSelect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; clr: Integer);
begin
  if (Button = mbLeft) then
  begin
    Clrl := clr;
    GBColorCombo.SelColor := clr;
  end
  else
    Clrr := clr;

  RefreshClickBtns;
  Modified := True;
end;

procedure TFrmTile.GBColorComboChange(Sender: TObject);
begin
  case GBColorController.ColorMode of
    gbcmSGB      : PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[1] := GBColorCombo.ItemIndex;
    gbcmGBC,
    gbcmGBCFiltered : PTileType(TileList.Items[LstTiles.SelTile]).ColorSet[0] := GBColorCombo.ItemIndex;
  end;

  Modified := True;
  RefreshSelector;
  GrdPixel.Paint;
  UndoCtrl.PixelEnd;
end;

procedure TFrmTile.Autoupdate1Click(Sender: TObject);
begin
  SetExchangerActive( not TileExchanger.Active );
end;




procedure TFrmTile.TileExchangerChange( Chg : TGBTileExchangerChange; TileNr : integer);
begin
  case Chg of
    GBTETile  :
    begin
      if (TileNr = LstTiles.SelTile) then
      begin
        GrdPixel.paint;
        UpdateSelector;
      end
      else
        LstTiles.PaintTile(TileNr);

    end;
    GBTEList  : LstTiles.RefreshList;
    GBTEDim,
    GBTETotal :
      begin
      CurTileSize := TileExchanger.TileSize;
      RebuildScreen;
      end;
    else
    begin
      UpdateSelector;
      LstTiles.Invalidate;
      GrdPixel.Paint;
    end;
  end;
end;

end.
