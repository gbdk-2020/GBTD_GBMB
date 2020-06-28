unit import;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, gbconst, gbofile, MainLib, ColorController, GBTDMain;

type
  TFrmImport = class(TForm)
    GroupBox1: TGroupBox;
    LblFilename: TLabel;
    EdFilename: TEdit;
    CmbType: TComboBox;
    Label2: TLabel;
    BtnBrowse: TButton;
    BtnOK: TButton;
    BtnCancel: TButton;
    GroupBox2: TGroupBox;
    LblFromTile: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    DlgOpen: TOpenDialog;
    BtnHelp: TButton;
    EdFromTile: TEdit;
    EdToTile: TEdit;
    EdTileCount: TEdit;
    LblFirstByte: TLabel;
    EdFirstByte: TEdit;
    LblBinType: TLabel;
    CmbBinType: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EdChange(Sender: TObject);
    procedure CmbTypeChange(Sender: TObject);
  private
    { Private declarations }
    procedure TypeChanged;
    procedure LoadFromGBE(TileImport : TGBOTileImport; TileSize : TGBTileSize ; Lst : TList );
    procedure LoadFromBin(TileImport : TGBOTileImport; TileSize : TGBTileSize ; Lst : TList );

  public
    { Public declarations }
  end;

var
  FrmImport: TFrmImport;

implementation

uses TileEdit;

{$R *.DFM}

{--$I ..\hlp\gbtd.inc}

procedure TFrmImport.FormShow(Sender: TObject);
var i : integer;
begin
  (* Init fields *)
  with TileImport do
  begin

    EdFileName.Text := FileName;
    CmbType.ItemIndex := FileType;
    if (FromTile > MAXTILECNT-1) then
      EdFromTile.Text := '0'
    else
      EdFromTile.Text := IntToStr(FromTile);

    i := FrmTile.LstTiles.TileData.Count-1;
    if (ToTile > i) then
      EdFromTile.Text := IntToStr(i)
    else
      EdToTile.Text := IntToStr(ToTile);

    if ((ToTile + TileCount) > i) then
      EdTileCount.Text := IntToStr( i - ToTile)
    else
      EdTileCount.Text := IntToStr(TileCount);

    EdFirstByte.Text := IntToStr(FirstByte);

    CmbBinType.ItemIndex := 0;
    if ( BinType < CmbBinType.Items.Count) then
      CmbBinType.ItemIndex := BinType;
    TypeChanged;
  end;

end;

procedure TFrmImport.BtnBrowseClick(Sender: TObject);
begin
  with DlgOpen do
  begin
    if (Trim(EdFilename.Text) <> '') then
      FileName := EdFilename.Text
    else
      Filename := '*.*';

    if Execute then
      EdFileName.Text := FileName;
  end;
end;





procedure TFrmImport.BtnOKClick(Sender: TObject);
var s : string;
var t,c,m : integer;
    Ctrl : TWinControl;

procedure ReportError( s : string; Ctrl : TWinControl );
begin
  Messagedlg(s, mtError, [mbOk],0 );
  Ctrl.SetFocus;
end;

begin

  (* validate fields *)
  s := '';
  if      (trim(EdFileName.Text) = '')  then begin s := 'Filename'; Ctrl := EdFileName; end
  else if (trim(EdToTile.Text) = '')    then begin s := 'First tile in GBTD'; Ctrl := EdToTile; end
  else if (CmbType.ItemIndex=0) and (trim(EdFromTile.Text) = '') then begin s := 'First tile from import file'; Ctrl := EdFromTile; end
  else if (CmbType.ItemIndex=1) and (trim(EdFirstByte.Text) = '') then begin s := 'First byte to use'; Ctrl := EdFirstByte; end;


  if (s <> '') then
  begin
    s := '''' + s + ''' is mandatory for this import type.';
    ReportError(s,Ctrl);
  end
  else
  begin

    t := StrToInt(Trim(EdToTile.Text));
    c := StrToInt(Trim(EdTileCOunt.Text));
    m := FrmTile.LstTiles.TileData.Count-1;

    if (c < 1) then ReportError('''Tile count'' should be higher or equal to 1.', EdTileCount)
    else if ( (t+c) > m+1 ) then ReportError('''First tile in GBTD'' combined with ''Tile count'' can not exceed ' + IntToStr(m+1) + '.',  EdToTile )
    else if (not FileExists(EdFileName.Text)) then ReportError('''' + Trim(EdFileName.Text) + ''' does not exist.', EdFilename )
    else
    begin

      (* Save settings *)
      with TileImport do
      begin
        StrPCopy(FileName, EdFileName.Text);
        FileType  := CmbType.ItemIndex;
        ToTile    := StrToInt(Trim(EdToTile.Text));
        TileCount := StrToInt(Trim(EdTileCount.Text));
        try FromTile  := StrToInt(Trim(EdFromTile.Text)); except FromTile :=0; end;
        try FirstByte := StrToInt(Trim(EdFirstByte.Text)); except FirstByte :=0; end;
        BinType   := CmbBinType.ItemIndex;

        Modified := True;


        case FileType of

          0 :  (* import GBE *)
               LoadFromGBE(TileImport, CurTileSize, TileList);

          else (* import binary 8x8 *)
               LoadFromBin(TileImport, CurTileSize, TileList);
        end;

      end;
      Close;

    end;
  end;
end;






procedure TFrmImport.LoadFromBin(TileImport : TGBOTileImport; TileSize : TGBTileSize ; Lst : TList );
var FStrm : TFileStream;
    p     : PTileType;
    i,j,k : integer;
    sx,sy : integer;

  procedure LoadByte( const FStrm : TFileStream; var buf : TileType );
  var x,y  : integer;
      b    : byte;
  begin
    for y := 0 to 7 do
      for x := 0 to 7 do
      begin
        if (FStrm.Position < FStrm.Size) then FStrm.Read( b, 1 );
        buf.data[y,x] := (b mod 4);
      end;
  end;

  procedure Load2bit( const FStrm : TFileStream; var buf : TileType );
  var x,y,i : integer;
      b,c,s : byte;
  begin
    for y := 0 to 7 do
    begin
      for x := 0 to 1 do
      begin
        if (FStrm.Position < FStrm.Size) then FStrm.Read( b, 1 );
        c := $C0;
        s := 6;
        for i := 0 to 3 do
        begin
          buf.data[y,(x*4)+i] := (b and c) shr s;
          c := (c shr 2);
          dec(s,2);
        end;
      end;
    end;
  end;

  procedure LoadVRAM( const FStrm : TFileStream; var buf : TileType );
  var x,y,i : integer;
      pnl   : array[0..1] of byte;
  begin
    for y := 0 to 7 do
    begin
      if ((FStrm.Position+1) < FStrm.Size) then FStrm.Read( pnl, 2 );
      for i := 0 to 7 do
      begin
        buf.data[y,i] := ((pnl[0] and $80) shr 7) + ((pnl[1] and $80) shr 6);
        pnl[0] := pnl[0] shl 1;
        pnl[1] := pnl[1] shl 1;
      end;
    end;
  end;

begin

  DetermineTileSize(TileSize, sx, sy);

  FStrm := TFileStream.Create(StrPas(TileImport.Filename), fmOpenRead);
  try

    with TileImport do
    begin

      (* Convert to 8x8 *)
      ConvertTileSize(TileSize, gbts8x8, Lst);
      try

        (* move to first byte *)
        FStrm.Seek(FirstByte, soFromBeginning);

        (* loading *)
        i := 0;
        j := TileCount * ((sx div 8) * (sy div 8));
        k := ToTile * ((sx div 8) * (sy div 8));
        while (i < j) and (FStrm.Position < FStrm.Size) do
        begin

          case BinType of
            0 : LoadByte(FStrm, TileType(Lst.items[i+k]^) );
            1 : Load2Bit(FStrm, TileType(Lst.items[i+k]^) );
            2 : LoadVRAM(FStrm, TileType(Lst.items[i+k]^) );
          end;

          inc(i);
        end;

      finally
        (* Convert back to current sizes *)
        ConvertTileSize(gbts8x8, TileSize, Lst);
      end;
    end;
  finally
    FStrm.Free;
  end;

end;



procedure TFrmImport.LoadFromGBE(TileImport : TGBOTileImport; TileSize : TGBTileSize ; Lst : TList );
var FStrm : TFileStream;
var i,j : integer;
var p : PTileType;
var buf : array[0..7,0..7] of byte;
var x,y, sx,sy : integer;
var palbuf : array[0..11] of byte;
    Pal : TGBPaletteType;
begin

  DetermineTileSize(TileSize, sx, sy);

  FStrm := TFileStream.Create(StrPas(TileImport.Filename), fmOpenRead);
  try

    with TileImport do
    begin

      (* Convert to 8x8 *)
      ConvertTileSize(TileSize, gbts8x8, Lst);
      try

        (* Colors *)
        FStrm.Read(palbuf, 3*4);
        for i:=0 to 3 do
        begin
          case palbuf[i*3] of
            $2F  : Pal[i] := 0;
            $1F  : Pal[i] := 1;
            $10  : Pal[i] := 2;
            $00  : Pal[i] := 3;
          end;
        end;
        GBColorController.Palette := Pal;


        (* Data *)
        FStrm.Seek(768 + (FromTile*(sx*sy)), soFromBeginning);


        i := TileCount * ((sx div 8) * (sy div 8));
        j := ToTile * ((sx div 8) * (sy div 8));
        while (FStrm.Size > FStrm.Position) and (i > 0) do
        begin

          FStrm.Read(buf, 64);
          p := Lst.Items[j];
          for y := 0 to 7 do
            for x := 0 to 7 do
              p.data[y,x] := buf[y,x];

          Dec(i);
          Inc(j);
        end;

      finally
        (* Convert back to current sizes *)
        ConvertTileSize(gbts8x8, TileSize, Lst);
      end;
    end;
  finally
    FStrm.Free;
  end;


end;

procedure TFrmImport.BtnHelpClick(Sender: TObject);
begin
//  Application.HelpContext(Import_options);
end;

procedure TFrmImport.FormCreate(Sender: TObject);
begin
//  HelpContext := Import_options;
//  CmbType.HelpContext := Import_file_types;
end;

procedure TFrmImport.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

procedure TFrmImport.EdChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;



procedure TFrmImport.CmbTypeChange(Sender: TObject);
begin
  TypeChanged;
end;

procedure TFrmImport.TypeChanged;
begin
  If (CmbType.ItemIndex = 0) then
  begin
    EdFromTile.Enabled   := True;
    LblFromTile.Enabled  := True;

    EdFirstByte.Enabled  := False;
    LblFirstByte.Enabled := False;
    CmbBinType.Enabled   := False;
    LblBinType.Enabled   := False;
  end
  else
  begin
    EdFromTile.Enabled   := False;
    LblFromTile.Enabled  := False;

    EdFirstByte.Enabled  := True;
    LblFirstByte.Enabled := True;
    CmbBinType.Enabled   := True;
    LblBinType.Enabled   := True;
  end;
end;





end.
