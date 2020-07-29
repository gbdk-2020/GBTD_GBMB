unit map;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Menus, MapGrid,
  gbtlst, gbconst, StdCtrls, Files, ExpBtn, MainLib, IniFiles, MRUList,
  ShlObj, gbofile, TileFileObserver, clipbrd, ColorController, Dialogs,
  Buttons, ExtCtrls, GBTileExchanger, GBColorCombo;

type
  TFrmMap = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    PnlToolbar: TPanel;
    LstTiles: TGBTileList;
    Tileproperties1: TMenuItem;
    LneBtmShdw: TShape;
    LneBtmHgh: TShape;
    LneTop: TShape;
    LneTopShdw: TShape;
    PnlMain: TPanel;
    GrdMap: TMapGrid;
    PnlBtn: TPanel;
    PnlInfo: TPanel;
    Loadmap1: TMenuItem;
    Mapproperties1: TMenuItem;
    BtnExport: TSpeedButton;
    PnlZoom: TPanel;
    CmbZoom: TComboBox;
    BtnPenCursor: TSpeedButton;
    BtnFloodFillCursor: TSpeedButton;
    Bevel1: TBevel;
    BtnInsRow: TSpeedButton;
    BtnInsertCol: TSpeedButton;
    BtnDelCol: TSpeedButton;
    BtnDelRow: TSpeedButton;
    Design1: TMenuItem;
    Setbookmark1: TMenuItem;
    mnuGoto1: TMenuItem;
    mnuGoto2: TMenuItem;
    mnuGoto3: TMenuItem;
    mnuSet1: TMenuItem;
    mnuSet2: TMenuItem;
    mnuSet3: TMenuItem;
    DlgSave: TSaveDialog;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    NReopen: TMenuItem;
    DlgOpen: TOpenDialog;
    MRUList: TMRUFileList;
    N1: TMenuItem;
    Exportto1: TMenuItem;
    Defaulttileproperties1: TMenuItem;
    BtnOpen: TSpeedButton;
    BtnSave: TSpeedButton;
    Shape1: TShape;
    Shape2: TShape;
    View1: TMenuItem;
    Zoom1: TMenuItem;
    N2: TMenuItem;
    N501: TMenuItem;
    N1001: TMenuItem;
    N1501: TMenuItem;
    N2001: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    Helptopics1: TMenuItem;
    Helpindex1: TMenuItem;
    N4: TMenuItem;
    About1: TMenuItem;
    Insertrow1: TMenuItem;
    Insertcolumn1: TMenuItem;
    Deleterow1: TMenuItem;
    Deletecolumn1: TMenuItem;
    N5: TMenuItem;
    Infopanel1: TMenuItem;
    Pen1: TMenuItem;
    Floodfill1: TMenuItem;
    N6: TMenuItem;
    Shape3: TShape;
    Shape4: TShape;
    BtnHelp: TSpeedButton;
    Gotobookmark1: TMenuItem;
    Export1: TMenuItem;
    N7: TMenuItem;
    Clearmap1: TMenuItem;
    N8: TMenuItem;
    MnuColorset: TMenuItem;
    MnuColorsetStandard: TMenuItem;
    MnuColorsetGameboy: TMenuItem;
    Blockfill1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Grid1: TMenuItem;
    Doublemarkers1: TMenuItem;
    N9: TMenuItem;
    Copy1: TMenuItem;
    BtnCut: TSpeedButton;
    BtnCopy: TSpeedButton;
    BtnPaste: TSpeedButton;
    Shape5: TShape;
    Shape6: TShape;
    Paste1: TMenuItem;
    Cut1: TMenuItem;
    MnuColorsetColor: TMenuItem;
    mnuColorSuper: TMenuItem;
    BtnDropperCursor: TSpeedButton;
    Dropper1: TMenuItem;
    N251: TMenuItem;
    Propertycolors1: TMenuItem;
    N10: TMenuItem;
    Copyasbitmap1: TMenuItem;
    N11: TMenuItem;
    Autoupdate1: TMenuItem;
    BtnAutoUpdate: TSpeedButton;
    GameboyColorFiltered1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Tileproperties1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Savefile1Click(Sender: TObject);
    procedure Loadmap1Click(Sender: TObject);
    procedure Mapproperties1Click(Sender: TObject);
    procedure CmbZoomChange(Sender: TObject);
    procedure PnlZoomClick(Sender: TObject);
    procedure GrdMapAfterSelect(Sender: TObject);
    procedure SetBookmark(Sender: TObject);
    procedure GotoBookmark(Sender: TObject);
    procedure Gotobookmark1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure MRUListCreate(Sender: TObject);
    procedure MRUListMRUItemClick(Sender: TObject; AFilename: TFileName);
    procedure Exportto1Click(Sender: TObject);
    procedure Defaulttileproperties1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure SetmnuZoom(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Insertrow1Click(Sender: TObject);
    procedure Deleterow1Click(Sender: TObject);
    procedure Insertcolumn1Click(Sender: TObject);
    procedure Deletecolumn1Click(Sender: TObject);
    procedure Infopanel1Click(Sender: TObject);
    procedure Pen1Click(Sender: TObject);
    procedure Floodfill1Click(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure Helptopics1Click(Sender: TObject);
    procedure Helpindex1Click(Sender: TObject);
    procedure GrdMapBeforeSelect(Sender: TObject; x, y: Integer;
      Button: TMouseButton; var FullRefresh, Continue: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Clearmap1Click(Sender: TObject);
    procedure MnuColorsetClick(Sender: TObject);
    procedure Blockfill1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure LstTilesAfterSelect(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure Doublemarkers1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Dropper1Click(Sender: TObject);
    procedure Propertycolors1Click(Sender: TObject);
    procedure Copyasbitmap1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Autoupdate1Click(Sender: TObject);
  private
    { Private declarations }

    IPLeave : boolean;
    PropChanged : boolean;
    CBC        : HWND;

    procedure SetPasteState;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    function AppOnHelp(Command: Word; Data: Longint;   var CallHelp: Boolean): Boolean;
    procedure SetContextIDs;

    procedure ExceptionHandler(Sender: TObject; E: Exception);


    procedure TileExchangerChange( Chg : TGBTileExchangerChange; TileNr : integer);

  protected
    procedure CMFileChangeNotification(var Message: TMessage); message CM_FILECHANGENOTIFICATION;
   procedure WMChangeCBChain(var Message: TWMChangeCBChain); message WM_CHANGECBCHAIN;
   procedure WMDrawClipBoard(var Message: TWMNoParams ); message WM_DRAWCLIPBOARD;

  public
    { Public declarations }

    LblLocation : TLabel;
    ChkVer      : TCheckBox;
    ChkHor      : TCheckBox;
    ColorCombo  : TGBColorCombo;

    procedure InfoPanelFlipClick(Sender: TObject);
    procedure InfoPanelEdExit(Sender: TObject);
    procedure GlobalNumericEdChange(Sender: TObject);

    procedure InfoPanelKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure Resize; override;
    procedure ChangeZoom( i : integer);

    function SaveToGBM(AutoSave : boolean ): boolean;
    procedure LoadFromGBM(const f : string);

    function HandleModified: boolean;

    procedure ColorComboChange(Sender: TObject);

  end;

var
  FrmMap: TFrmMap;



implementation

uses TileProps, Repository, GBMBMain, MapProps,  Export,
     DefTileProps, about, fldfill, BlockFill, MapUndo, GBMBClipboard;

{$R *.DFM}
{--$I ..\hlp\gbmb.inc}


procedure TFrmMap.TileExchangerChange( Chg : TGBTileExchangerChange; TileNr : integer);
begin
  case Chg of
    GBTETile  : LstTiles.PaintTile(TileNr);
    GBTEList  : LstTiles.RefreshList;
    GBTEDim,
    GBTETotal :
      begin
        SetTileSize( TileExchanger.Width, TileExchanger.Height );
        LstTiles.Invalidate;
      end;
    else        LstTiles.Invalidate;
  end;
  GrdMap.Invalidate;
end;


procedure TFrmMap.WMMouseWheel(var Msg: TMessage);
var
  ScrollMsg: TMessage;
  p : TPoint;
begin
  if ComponentState = [csDesigning] then Exit;

  (* get mouse pos *)
  p.x := msg.LParamLo;
  p.y := msg.LParamHi;
  p := ScreenToClient(p);

  (* determine action *)
  if p.x < LstTiles.Left then
  begin
    if ShortInt(Msg.WParamHi) = -120 then GrdMap.ScrollUp;
    if ShortInt(Msg.WParamHi) = 120  then GrdMap.ScrollDown;
  end
  else
  begin
    if ShortInt(Msg.WParamHi) = -120 then LstTiles.ScrollUp;
    if ShortInt(Msg.WParamHi) = 120  then LstTiles.ScrollDown;
  end;
end;



function TFrmMap.AppOnHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
begin
  CallHelp := (Data <> 0) or (Command <> HELP_CONTEXT);
  Result := CallHelp;
end;


procedure TFrmMap.CMFileChangeNotification(var Message: TMessage);
var f : string;
begin

  GrdMap.Freezed   := True;
  LstTiles.Freezed := True;
  try
    f := ExpandRelativePath(TileFileName, BaseDir);
    LoadTileSet(f);
  finally
    GrdMap.Freezed   := False;
    LstTiles.Freezed := False;
  end;
  RefreshList;
  RefreshMap;
end;


procedure TFrmMap.FormCreate(Sender: TObject);
var i : integer;
    s :string;
begin
  MapMinWidth := 390;

  GBColorController := TGBColorController.Create(Self);
  TileList := TList.Create;
  TileMap := TGBMapType.Create;
  ExportSettings := TExportSettings.Create;

  Application.OnException := ExceptionHandler;

  InitRepository;
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'gbmb.hlp';
  Application.OnHelp := AppOnHelp;
  SetContextIDs;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'gbmb.ini');

  (* setup clipboard responses *)
  CBC := SetClipboardViewer(Self.Handle);

  with MapSettings do
  begin
    BlockFillPattern := 0;
    BlockFillWidth := 1;
    BlockFillHeight := 1;
  end;

  SetColorSet( IniFile.ReadInteger('General', 'ViewColorSet', 0 ));

  CalcAllPropColors;

  LstTiles.TileData := TileList;

  GrdMap.TileData := TileList;
  GrdMap.MapData := TileMap;
  GrdMap.SetSelectedTile(0,0, mbLeft);

  TileExchanger := TGBTileExchanger.Create(Self, True);
  TileExchanger.TileData := TileList;
  TileExchanger.OnChange := TileExchangerChange;


  i := IniFile.ReadInteger('General', 'ViewZoom', 2 );
  if (i > CmbZoom.Items.Count-1) then i := CmbZoom.Items.Count-1;
  CmbZoom.ItemIndex := i;
  Zoom1.Items[i].Checked := True;
  GrdMap.ZoomFactor := i;

  BuildInfoPanel;
  SetInfoPanel( IniFile.ReadBool('General', 'ViewInfoPanel', True ) );

  with GrdMap do
  begin
    Grid := IniFile.ReadBool('General', 'ViewGrid', True );
    DoubleMarkers := IniFile.ReadBool('General', 'ViewDoubleMarkers', False );
    PropColors := IniFile.ReadBool('General', 'ViewPropertyColors', False );
  end;

  (* set shortcuts *)
  mnuSet1.ShortCut := ShortCut( Word('1'), [ssAlt] );
  mnuSet2.ShortCut := ShortCut( Word('2'), [ssAlt] );
  mnuSet3.ShortCut := ShortCut( Word('3'), [ssAlt] );

  mnuGoto1.ShortCut := ShortCut( Word('1'), [ssCtrl] );
  mnuGoto2.ShortCut := ShortCut( Word('2'), [ssCtrl] );
  mnuGoto3.ShortCut := ShortCut( Word('3'), [ssCtrl] );

  Modified := False;
  PropChanged := False;

  (* Check for switches *)
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    if (s[1] = '-') then
    begin
      Msg('This switch is not supported by this version.', MsgError, MB_OK );
      Halt;              (* ! dirty ! *)
    end
  end;

  if (ParamCount > 0) then LoadFromGBM(ParamStr(1));

end;


procedure TFrmMap.Tileproperties1Click(Sender: TObject);
begin
  if not Assigned(FrmTileProps) then FrmTileProps := TFrmTileProps.Create(Application);
  if (FrmTileProps.ShowModal = mrOK) then
  begin
    Resize;
    CalcAllPropColors;
    if GrdMap.PropColors then GrdMap.Paint;
  end;
  MapUndoCtrl.Clear;
end;

procedure TFrmMap.Resize;
var i,x,y : integer;
begin
  DetermineTileSize(LstTiles.TileSize, x, y);
  LstTiles.Left := Width - LstTiles.Width - GetSystemMetrics( SM_CXVSCROLL );
  PnlMain.Setbounds( 0, 34, LstTiles.Left-3,
                     Height - (GetSystemMetrics( SM_CYCAPTION ) + GetSystemMetrics( SM_CYMENU ) + 48) );
  LstTiles.TileCount := (PnlMain.Height div ((y+1) * 2));

  if (PnlInfo.Visible) then
  begin
    BuildInfoPanel;
    i := PnlInfo.Height;
    PnlInfo.Top := PnlMain.Height-PnlInfo.Height-4;
  end
  else
    i := -4;

  GrdMap.SetBounds( GrdMap.Left, GrdMap.Top, PnlMain.Width - (33+4), PnlMain.Height - i - 12 );
end;


procedure TFrmMap.FormResize(Sender: TObject);
begin
  Resize;
end;



procedure TFrmMap.Mapproperties1Click(Sender: TObject);
begin
  if not Assigned(FrmMapProps) then FrmMapProps := TFrmMapProps.Create(Application);
  FrmMapProps.ShowModal;
  MapUndoCtrl.Clear;
end;

procedure TFrmMap.CmbZoomChange(Sender: TObject);
begin
  ChangeZoom( CmbZoom.ItemIndex );
end;

procedure TFrmMap.ChangeZoom( i : integer);
begin
  CmbZoom.ItemIndex := i;
  GrdMap.ZoomFactor := i;
  Zoom1.Items[CmbZoom.ItemIndex].Checked := True;
  Modified := True;
end;

procedure TFrmMap.PnlZoomClick(Sender: TObject);
begin
  CmbZoom.SetFocus;
end;

procedure TFrmMap.InfoPanelFlipClick(Sender: TObject);
var i,j : integer;
var b,c   : boolean;
begin
  if (ChkVer.tag = 0) then
  begin
    b := (Sender = ChkHor);
    c := TCheckBox(Sender).Checked;
    for i := GrdMap.SelZoneTop to GrdMap.SelZoneBottom do
      for j := GrdMap.SelZoneLeft to GrdMap.SelZoneRight do
        SetFlipOfLocation(j,i, b, c);

    GrdMap.Paint;
    Modified := True;
  end;
end;

procedure TFrmMap.InfoPanelEdExit(Sender: TObject);
var i,j, k,l : integer;
begin
  if (PropChanged) then
  begin
    i := 0;
    while (i < PROPCNT) and (PropEd[i].Ctrl <> Sender) do Inc(i);

    if (Sender is TEdit) and (TEdit(Sender).Text <> '') then
    begin
      IPLeave := False;
      j := StrToInt(TEdit(Sender).Text);
      if (j > PropEd[i].Size) then
        raise EConvertError.Create( PropEd[i].Name + ' should be lower or equal to ' + IntToStr(PropEd[i].Size));

      for k := GrdMap.SelZoneTop to GrdMap.SelZoneBottom do
        for l := GrdMap.SelZoneLeft to GrdMap.SelZoneRight do
          SetPropOfLocation(l,k,i,j);

      CalcAllPropColors;

      IPLeave := True;
      Modified := True;
      PropChanged := False;
    end;
  end;
end;

procedure TFrmMap.GlobalNumericEdChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;

procedure TFrmMap.InfoPanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  PropChanged := True;
  if (Key = VK_RETURN) then InfoPanelEdExit(Sender);
end;

procedure TFrmMap.GrdMapAfterSelect(Sender: TObject);
begin
  RefreshInfoPanel;
end;

procedure TFrmMap.SetBookmark(Sender: TObject);
begin
  LstTiles.Bookmarks[TComponent(Sender).Tag] := LstTiles.SelTile;
  Modified := True;
end;

procedure TFrmMap.GotoBookmark(Sender: TObject);
var i : integer;
begin
  i := LstTiles.BookMarks[TComponent(Sender).Tag];
  if (i >= 0) and (i < TileList.Count) then
    LstTiles.SelTile := i;
end;

procedure TFrmMap.Gotobookmark1Click(Sender: TObject);
begin
  mnuGoto1.Enabled := (LstTiles.Bookmarks[0] <> -1);
  mnuGoto2.Enabled := (LstTiles.Bookmarks[1] <> -1);
  mnuGoto3.Enabled := (LstTiles.Bookmarks[2] <> -1);
end;





(*****************************************************)
(*                                                   *)
(*                Load and Save functions            *)
(*                                                   *)
(*****************************************************)


procedure TFrmMap.MRUListCreate(Sender: TObject);
begin
  MRUList.AutoSaveName := ExtractFilePath(Application.ExeName) + 'gbmb.ini';
end;

procedure TFrmMap.MRUListMRUItemClick(Sender: TObject; AFilename: TFileName);
begin
  LoadFromGBM(AFilename);
end;

procedure TFrmMap.Loadmap1Click(Sender: TObject);
begin
  LoadFromGBM('');
end;

procedure TFrmMap.Savefile1Click(Sender: TObject);
begin
  SaveToGBM(True);
end;

procedure TFrmMap.SaveAs1Click(Sender: TObject);
begin
  SaveToGBM(False);
end;


function TFrmMap.HandleModified: boolean;
var i : word;
begin
  if Modified then
  begin
    i := msg('The current tiles have been changed. Save changes first ?', MsgConfirmation, MB_YESNOCANCEL);
    case i of
      IDYES    : Result := SaveToGBM(True);
      IDNO     : Result := True;
      else       Result := False;
    end;
  end
  else
    Result := True;
end;


function TFrmMap.SaveToGBM(AutoSave : boolean): boolean;
var s : string;
begin
  s := Filename;
  Result := True;
  if (not AutoSave) or (s = '') or (not FileExists(s)) then
  begin
    if (s = '') then s := IniFile.ReadString('General', 'GBMPath', ExtractFilePath(Application.ExeName))+ '*.gbm';

    DlgSave.FileName := s;
    if DlgSave.Execute then
    begin
      FileName := DlgSave.FileName;
      IniFile.WriteString('General', 'GBMPath', ExtractFilePath(FileName));
    end
    else
      Result := False;
      
    ResetBaseDir;
  end;

  if Result then
  begin
    SaveMap(Filename);

    SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
    MRUList.InsertItem(0,LowerCase(FileName));
    Modified := False;
  end;
  MapUndoCtrl.Clear;
end;

procedure TFrmMap.LoadFromGBM(const f : string);
var Ok : boolean;
begin

  if HandleModified then
  begin
    Ok := True;

    if (f = '') then
    begin
      DlgOpen.FileName := IniFile.ReadString('General', 'GBMPath', ExtractFilePath(Application.ExeName)) + '*.gbm';
      if DlgOpen.Execute then
        FileName := DlgOpen.FileName
      else
        Ok := False;
        
      ResetBaseDir;
    end
    else
      FileName := f;

    if Ok then
    begin
      IniFile.WriteString('General', 'GBMPath', ExtractFilePath(FileName));
      LoadMap(FileName);
      SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
      MRUList.InsertItem(0,LowerCase(FileName));
      Modified := False;
      PropChanged := False;
    end;

    MapUndoCtrl.Clear;
  end;
end;

procedure TFrmMap.Exportto1Click(Sender: TObject);
begin
  if not Assigned(FrmExport) then FrmExport := TFrmExport.Create(Application);
  FrmExport.ShowModal;
end;

procedure TFrmMap.Defaulttileproperties1Click(Sender: TObject);
begin
  if not Assigned(FrmDefTileProps) then FrmDefTileProps := TFrmDefTileProps.Create(Application);
  FrmDefTileProps.ShowModal;
  MapUndoCtrl.Clear;
end;

procedure TFrmMap.File1Click(Sender: TObject);
var i : integer;
begin

  (* enable/disable default tile props *)
  DefaultTileProperties1.Enabled := False;
  for i := 0 to PROPCNT-1 do
    if Assigned(PropEd[i].Ctrl) then
      DefaultTileProperties1.Enabled := True;
end;


procedure TFrmMap.SetmnuZoom(Sender: TObject);
begin
  ChangeZoom(TComponent(Sender).Tag);
end;

procedure TFrmMap.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFrmMap.About1Click(Sender: TObject);
begin
  if not Assigned(FrmAbout) then FrmAbout := TFrmAbout.Create(Self);
  FrmAbout.ShowModal;
end;



procedure TFrmMap.Insertrow1Click(Sender: TObject);
var i,j : integer;
begin
  with TileMap do
    if (Height < MAXMAPHEIGHT) then
    begin
      MapUndoCtrl.Mode := gbumMap;
      MapUndoCtrl.AddUndo(0,0,Data[0]);  (* params unused *)
      Height := Height + 1;

      (* copy rows *)
      for j := Height-1 downto GrdMap.SelZoneTop+1 do
        for i := 0 to Width-1 do
          CopyTile( i, (j-1), i, j  );

      (* setup new row *)
      for i := 0 to Width-1 do
        SetTileOfLocation(i, GrdMap.SelZoneTop, LstTiles.SelTile);

      (* refresh various *)
      CalcAllPropColors;
      RefreshMap;
      RefreshInfoPanel;
    end;
end;

procedure TFrmMap.Deleterow1Click(Sender: TObject);
var i,j : integer;
begin
  with TileMap do
    if (Height > 1) then
    begin
      MapUndoCtrl.Mode := gbumMap;
      MapUndoCtrl.AddUndo(0,0,Data[0]);  (* params unused *)

      (* copy rows *)
      for j := GrdMap.SelZoneTop to Height-2 do
        for i := 0 to Width-1 do
          CopyTile(i, j+1, i, j);

      Height := Height - 1;

      (* refresh various *)
      GrdMap.VerifySelBoundaries;
      RefreshMap;
      RefreshInfoPanel;
      Modified := True;
    end;
end;

procedure TFrmMap.Insertcolumn1Click(Sender: TObject);
var i,j : integer;
begin
  with TileMap do
    if (Width < MAXMAPWIDTH) then
    begin
      MapUndoCtrl.Mode := gbumMap;
      MapUndoCtrl.AddUndo(0,0,Data[0]);  (* params unused *)

      Width := Width + 1;

      (* copy cols *)
      for j := Width-1 downto GrdMap.SelZoneLeft+1 do
        for i := 0 to Height-1 do
          CopyTile( j-1, i, j, i );

      (* setup new col *)
      for i := 0 to Height-1 do
        SetTileOfLocation(GrdMap.SelZoneLeft, i, LstTiles.SelTile);

      (* refresh various *)
      CalcAllPropColors;
      RefreshMap;
      RefreshInfoPanel;
    end;
end;

procedure TFrmMap.Deletecolumn1Click(Sender: TObject);
var i,j : integer;
begin
  with TileMap do
    if (Width > 1) then
    begin
      MapUndoCtrl.Mode := gbumMap;
      MapUndoCtrl.AddUndo(0,0, Data[0]);  (* params unused *)

      (* copy rows *)
      for j := GrdMap.SelZoneLeft to Width-2 do
        for i := 0 to Height-1 do
          CopyTile( j+1, i, j, i);

      Width := Width - 1;

      (* refresh various *)
      GrdMap.VerifySelBoundaries;
      RefreshMap;
      RefreshInfoPanel;
      Modified := True;
    end;
end;

procedure TFrmMap.Infopanel1Click(Sender: TObject);
begin
  SetInfoPanel(not InfoPanel1.Checked);
end;

procedure TFrmMap.Pen1Click(Sender: TObject);
begin
  SetDrawStyle(gbdsPen);
end;

procedure TFrmMap.Floodfill1Click(Sender: TObject);
begin
  SetDrawStyle(gbdsFloodFill);
end;

procedure TFrmMap.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var i : integer;
begin
  with Message do
  begin
    MinMaxInfo.ptMinTrackSize.x := MapMinWidth;

    i := 250 + GetSystemMetrics( SM_CYCAPTION ) + GetSystemMetrics( SM_CYMENU );
    if (Assigned(PnlInfo) and PnlInfo.Visible) then i := i + PnlInfo.Height;
    MinMaxInfo.ptMinTrackSize.y := i;

    Result := 0;
  end;
end;




procedure TFrmMap.Export1Click(Sender: TObject);
begin
  if not Assigned(FrmExport) then FrmExport := TFrmExport.Create(Application);
  FrmExport.DirectExport;
  FrmExport.Release;
end;

procedure TFrmMap.Helptopics1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TFrmMap.Helpindex1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_PARTIALKEY, LongInt(PChar('')) );
end;

procedure TFrmMap.SetContextIDs;
begin
{
  MapProperties1.HelpContext := Map_properties;
  TileProperties1.HelpContext := Location_properties;
  DefaultTileProperties1.HelpContext := Default_Location_Properties;
  Export1.HelpContext := _Export__vs__Export_to_;
  ExportTo1.HelpContext := Export_options;

  Cut1.HelpContext := Cutting_and_Pasting;
  Copy1.HelpContext := Cutting_and_Pasting;
  Paste1.HelpContext := Cutting_and_Pasting;
  CopyAsBitmap1.HelpContext := Copy_as_bitmap;


  Pen1.HelpContext := Draw_Types;
  FloodFill1.HelpContext := Draw_Types;
  Dropper1.HelpContext := Draw_Types;

  InsertRow1.HelpContext := InsDelRowCol;
  InsertColumn1.HelpContext := InsDelRowCol;
  DeleteRow1.HelpContext := InsDelRowCol;
  DeleteColumn1.HelpContext := InsDelRowCol;

  ClearMap1.HelpContext := Clear_map;
  BlockFill1.HelpContext := block_fill;

  Zoom1.HelpContext := Zoom;
  InfoPanel1.HelpContext := Info_panel;
  Grid1.HelpContext := Grid;
  DoubleMarkers1.HelpContext := Double_markers;
  PropertyColors1.HelpContext := Property_colors;
  Autoupdate1.HelpContext := Auto_update;
  mnuColorSet.HelpContext := Color_set;
  SetBookmark1.HelpContext := bookmarks;
  GotoBookmark1.HelpContext := Bookmarks;
}
end;

procedure TFrmMap.GrdMapBeforeSelect(Sender: TObject; x, y: Integer;
  Button: TMouseButton; var FullRefresh, Continue: Boolean);
begin
  if (Button = mbRight) then
    if BtnPenCursor.Down then
    begin
      MapUndoCtrl.Mode := gbumPixel;
      SetTileOfLocation(x,y, LstTiles.SelTile)
    end
    else if BtnFloodFillCursor.Down then
    begin
      MapFloodFill(x,y, LstTiles.SelTile, TileMap.Width, TileMap.Height );
      FullRefresh := True;
    end
    else
    begin
      MapUndoCtrl.Mode := gbumPixel;
      LstTiles.SelTile := GetTileOfLocation(x,y);
      SetDrawStyle(gbdsPen);
    end
  else
  begin
    IPLeave := True;
    InfoPanelEdExit(ActiveControl);
    Continue := IPLeave;
  end;
end;

procedure TFrmMap.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i,j : integer;
begin
  if (ssCtrl in Shift) then
  with GrdMap do
  begin
    case Key of
      VK_PRIOR :
      begin
        Freezed := True;
        Ver.Position := 0;
        Hor.Position := TileMap.Width - ColCount;
        Freezed := False;
        Paint;
        Key := 0;
      end;
      VK_END :
      begin
        Freezed := True;
        Ver.Position := TileMap.Height - RowCount;
        Hor.Position := 0;
        Freezed := False;
        Paint;
        Key := 0;
      end;
      VK_NEXT :
      begin
        Freezed := True;
        Ver.Position := TileMap.Height - RowCount;
        Hor.Position := TileMap.Width - ColCount;
        Freezed := False;
        Paint;
        Key := 0;
      end;
      VK_HOME :
      begin
        Freezed := True;
        Ver.Position := 0;
        Hor.Position := 0;
        Freezed := False;
        Paint;
        Key := 0;
      end;
      VK_DOWN :
      begin
        if Ver.Visible then Ver.Position := Ver.Position + 1;
        Key := 0;
      end;
      VK_UP :
      begin
        if Ver.Visible then Ver.Position := Ver.Position - 1;
        Key := 0;
      end;
      VK_RIGHT :
      begin
        if Hor.Visible then Hor.Position := Hor.Position + 1;
        Key := 0;
      end;
      VK_LEFT :
      begin
        if Hor.Visible then Hor.Position := Hor.Position - 1;
        Key := 0;
      end;
    end;
  end;

  (* handle space bar *)
  if (key = ord(' ')) then
  begin
    (* fill whole block with current tile *)
    MapUndoCtrl.Mode := gbumTotal;

    for i := GrdMap.SelZoneTop to GrdMap.SelZoneBottom do
      for j := GrdMap.SelZoneLeft to GrdMap.SelZoneRight do
        SetTileOfLocation(j,i, LstTiles.SelTile);

    Modified := True;
    RefreshInfoPanel;
    GrdMap.Paint;
    Key := 0;
  end;
end;

procedure TFrmMap.Clearmap1Click(Sender: TObject);
var i,j : integer;
begin
  if HandleModified then
  begin
    for i := 0 to TileMap.Width-1 do
      for j := 0 to TileMap.Height-1 do
        SetTileOfLocation(i,j,0);

    RefreshMap;
    RefreshInfoPanel;
    MapUndoCtrl.Clear;
  end;
end;


procedure TFrmMap.MnuColorsetClick(Sender: TObject);
begin
  SetColorSet(TComponent(Sender).Tag);
end;

procedure TFrmMap.Blockfill1Click(Sender: TObject);
begin
  if not Assigned(frmBlockFill) then frmBlockFill := TfrmBlockFill.Create(Application);
  frmBlockFill.ShowModal;
end;

procedure TFrmMap.Edit1Click(Sender: TObject);
begin
  Undo1.Enabled := MapUndoCtrl.UnDoAvailable;
end;

procedure TFrmMap.Undo1Click(Sender: TObject);
begin
  MapUndoCtrl.Undo;
  GrdMap.Paint;
  RefreshInfoPanel;
end;

procedure TFrmMap.LstTilesAfterSelect(Sender: TObject);
begin
  MapUndoCtrl.Clear;
end;

procedure TFrmMap.View1Click(Sender: TObject);
begin
  Grid1.Checked := GrdMap.Grid;
  DoubleMarkers1.Checked := GrdMap.DoubleMarkers;
  Propertycolors1.Checked := GrdMap.PropColors;
  Autoupdate1.Checked := TileExchanger.Active;
end;

procedure TFrmMap.Grid1Click(Sender: TObject);
begin
  GrdMap.Grid := (not Grid1.Checked);
  Modified := True;

  GrdMap.Paint;
end;

procedure TFrmMap.Doublemarkers1Click(Sender: TObject);
begin
  GrdMap.DoubleMarkers := not DoubleMarkers1.Checked;
  Modified := True;

  GrdMap.Paint;
end;

procedure TFrmMap.Copy1Click(Sender: TObject);
begin
  CopyToClipBoard;
end;

procedure TFrmMap.Paste1Click(Sender: TObject);
begin
  PasteFromClipBoard;
end;

procedure TFrmMap.Cut1Click(Sender: TObject);
begin
  CutToClipBoard;
end;

procedure TFrmMap.WMChangeCBChain(var Message: TWMChangeCBChain);
begin
  with Message do
  begin
    if (Remove = CBC) then CBC := Next;
    if (CBC <> NULL) then SendMessage(CBC, WM_CHANGECBCHAIN, Remove, Next );
  end;
end;

procedure TFrmMap.WMDrawClipBoard(var Message: TWMNoParams );
begin
  SetPasteState;

  if (CBC <> NULL) then SendMessage(CBC, WM_DRAWCLIPBOARD, 0, 0 );
end;

procedure TFrmMap.SetPasteState;
var b : boolean;
begin
  b := Clipboard.HasFormat(CF_TEXT);
  BtnPaste.Enabled := b;
  Paste1.Enabled := b;
end;

procedure TFrmMap.Dropper1Click(Sender: TObject);
begin
  SetDrawStyle(gbdsDropper);
end;

procedure TFrmMap.ExceptionHandler(Sender: TObject; E: Exception);
begin
  Msg(E.Message + '.', MsgError, MB_OK);
end;

procedure TFrmMap.Propertycolors1Click(Sender: TObject);
begin
  GrdMap.PropColors := (not Propertycolors1.Checked);
  Modified := True;

  GrdMap.Paint;
end;

procedure TFrmMap.Copyasbitmap1Click(Sender: TObject);
begin
  CopyAsBitmap;
end;


procedure TFrmMap.FormDestroy(Sender: TObject);
begin
  FreeTileFileObserver;
  MapUndoCtrl.Free;
  ChangeClipboardChain(Self.Handle, CBC );

  TileExchanger.Free;

  ExportSettings.Free;
  GBColorController.Free;
  IniFile.Free;
end;

procedure TFrmMap.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := HandleModified;
end;

procedure TFrmMap.Autoupdate1Click(Sender: TObject);
begin
  SetExchangerActive( not TileExchanger.Active );
end;

procedure TFrmMap.ColorComboChange(Sender: TObject);
var i,j : integer;
begin
  for i := GrdMap.SelZoneTop to GrdMap.SelZoneBottom do
    for j := GrdMap.SelZoneLeft to GrdMap.SelZoneRight do
      SetPalOfLocation( j, i, ColorCombo.ItemIndex, (GBColorController.ColorMode <> gbcmSGB) );

  GrdMap.Paint;
  Modified := True;
end;

end.
