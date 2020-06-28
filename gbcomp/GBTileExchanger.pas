unit GBTileExchanger;

interface

uses Classes, SysUtils, Controls, Messages, Dialogs, MainLib, gbConst,
     AppMessage, AppMem, ColorController, forms;

type

  TGBTileExchangerChange = ( GBTETile, GBTETotal, GBTEList, GBTEDim,
                             GBTEPal, GBTEColSets);

  TGBTileExchangerChangeEvent = procedure ( Chg : TGBTileExchangerChange; TileNr : integer) of object;


  TGBTileExchanger = class
    private
    { Private declarations }

      FAppMessage  : TAppMessage;
      FAppMem      : TAppMem;
      FMainControl : TControl;
      FReadOnly    : boolean;

      FWidth       : integer;
      FHeight      : integer;
      FTileSize    : TGBTileSize;


      FTileData : TList;
      FFileName : TFileName;
      FActive   : boolean;
      FOnChange : TGBTileExchangerChangeEvent;

      procedure UploadList;
      procedure TileToEx( inb, outb : PByteMem );
      procedure ExToTile( inb, outb : PByteMem );

    protected
    { Protected declarations }

      procedure SetFileName( f : TFileName );
      procedure SetActive( b : boolean );
      procedure MessageHandler(var Message: TMessage);
      procedure SetTileSize( Size : TGBTileSize );




    public
    { Public declarations }

      constructor Create( MainControl : TControl; ReadOnly : boolean );
      destructor Destroy; override;

      property TileData  : TList read FTileData write FTileData;
      property FileName  : TFileName read FFileName write SetFileName;
      property TileSize  : TGBTileSize read FTileSize write SetTileSize;
      property Width     : integer read FWidth;
      property Height    : integer read FHeight;

      property Active    : boolean read FActive write SetActive;
      property OnChange  : TGBTileExchangerChangeEvent read FOnChange write FOnChange;


      procedure TileToExchange( TileNr : integer);
      procedure ExchangeToTile( TileNr : integer);

      procedure ListToExchange;
      procedure PalToExchange;
      procedure ColorSetsToExchange;
      procedure TotalToExchange;
      procedure DimToExchange;

      procedure ExchangeToList;
      procedure ExchangeToColorSets;
      procedure ExchangeToPal;
      procedure ExchangeToDim;
      procedure ExchangeToTotal;
  end;


implementation

const
  TILEMSGTILE    = $0000;
  TILEMSGSPECIAL = $8000;
    TILEMSGTOTAL   = $8000;
    TILEMSGLIST    = $8001;
    TILEMSGDIM     = $8002;
    TILEMSGPAL     = $8003;
    TILEMSGCOLSETS = $8004;



type
  TGBTileExchangerData = packed record
    ID         : array[0..3] of char;
    Tiles      : array[0..((8*8)*MAXTILECNT)-1 ] of byte;
    PalMaps    : array[0..767] of ColSetArray;
    TileCount  : integer;
    TileWidth  : integer;
    TileHeight : integer;
    Palettes   : TGBPaletteType;
    GBCColSet  : TGBColorSets;
    SGBColSet  : TGBColorSets;
  end;

  PGBTileExchangerData = ^TGBTileExchangerData;


procedure TGBTileExchanger.SetTileSize( Size : TGBTileSize );
begin
  DetermineTileSize(Size, FWidth, FHeight);
  FTileSize := Size;
end;


constructor TGBTileExchanger.Create( MainControl : TControl; ReadOnly : boolean );
begin
  inherited Create;

  FMainControl := MainControl;
  FReadOnly := ReadOnly;
end;


destructor TGBTileExchanger.Destroy;
begin
  Active := False;

  inherited Destroy;
end;





procedure TGBTileExchanger.MessageHandler(var Message: TMessage);
begin
  if (Message.LParam <> integer(Application.handle)) and Assigned(FTileData) then
  begin
    if ((Message.WParam and TILEMSGSPECIAL) = 0) then
    begin
      ExchangeToTile( Message.WParam );

      if Assigned(FOnChange) then FOnChange( GBTETile, Message.WParam );
    end
    else
      case Message.WParam of
        TILEMSGPAL     : ExchangeToPal;
        TILEMSGCOLSETS : ExchangeToColorSets;
        TILEMSGTOTAL   : ExchangeToTotal;
        TILEMSGLIST    :
          begin
            ExchangeToList;
            if Assigned(FOnChange) then FOnChange( GBTEList, 0);
          end;
        TILEMSGDIM     :
          begin
            ExchangeToDim;
            ExchangeToList;
            if Assigned(FOnChange) then FOnChange( GBTEDim, 0);
          end;
      end;
  end;
end;

procedure TGBTileExchanger.ExchangeToTotal;
begin
  ExchangeToDim;
  ExchangeToList;
  ExchangeToPal;
  ExchangeToColorSets;

  if Assigned(FOnChange) then FOnChange( GBTETotal, 0);
end;

procedure TGBTileExchanger.ExchangeToTile( TileNr : integer);
var i : integer;
    b : PByteMem;
    p : PByteMem;
begin
  p := FTileData.Items[TileNr];
  b := @PGBTileExchangerData(FAppMem.MemBlock).Tiles[FWidth*FHeight*TileNr];

  ExToTile(b,p);
{
  for i:=0 to FHeight-1 do
  begin
    CopyBuf(b, p, FWidth);
    b := pointer(integer(b) + FWidth);
    p := pointer(integer(p) + 32);
  end;
}
  PTileType(FTileData.Items[TileNr]).ColorSet := PGBTileExchangerData(FAppMem.MemBlock).PalMaps[TileNr];
end;

procedure TGBTileExchanger.TileToExchange( TileNr : integer);
var i : integer;
    p : PByteMem;
    b : PByteMem;

begin
  if Active then
  begin

    b := @PGBTileExchangerData(FAppMem.MemBlock).Tiles[FWidth*FHeight*TileNr];
    p := FTileData.Items[TileNr];

    TileToEx(p,b);

{    for i:=0 to FHeight-1 do
    begin
      CopyBuf(p, b, FWidth);
      b := pointer(integer(b) + FWidth);
      p := pointer(integer(p) + 32);
    end;
}
    PGBTileExchangerData(FAppMem.MemBlock).PalMaps[TileNr] := PTileType(FTileData.Items[TileNr]).ColorSet;


    FAppMessage.SendAppMessage(TILEMSGTILE + TileNr,integer(Application.handle));
  end;
end;


procedure TGBTileExchanger.PalToExchange;
begin
  if Active then
  begin
    PGBTileExchangerData(FAppMem.MemBlock).Palettes := GBColorController.Palette;

    FAppMessage.SendAppMessage(TILEMSGPAL,integer(Application.handle));
  end;
end;

procedure TGBTileExchanger.ExchangeToPal;
begin
  GBColorController.Palette := PGBTileExchangerData(FAppMem.MemBlock).Palettes;
  if Assigned(FOnChange) then FOnChange( GBTEPal, 0);
end;


procedure TGBTileExchanger.DimToExchange;
begin
  if Active then
  begin
    with PGBTileExchangerData(FAppMem.MemBlock)^ do
    begin
      TileWidth := FWidth;
      TileHeight := FHeight;

      UploadList;
    end;

    FAppMessage.SendAppMessage(TILEMSGDIM,integer(Application.handle));
  end;
end;

procedure TGBTileExchanger.ExchangeToDim;
begin
  with PGBTileExchangerData(FAppMem.MemBlock)^ do
  begin
    FWidth  := TileWidth;
    FHeight := TileHeight;
    FTileSize := GetTileSize(FWidth, FHeight);
  end;
end;



procedure TGBTileExchanger.ExchangeToColorSets;
begin
  GBColorController.CGBSets := PGBTileExchangerData(FAppMem.MemBlock).GBCColSet;
  GBColorController.SGBSets := PGBTileExchangerData(FAppMem.MemBlock).SGBColSet;
  if Assigned(FOnChange) then FOnChange( GBTEColSets, 0);
end;


procedure TGBTileExchanger.ColorSetsToExchange;
begin
  if Active then
  begin
    PGBTileExchangerData(FAppMem.MemBlock).GBCColSet := GBColorController.CGBSets;
    PGBTileExchangerData(FAppMem.MemBlock).SGBColSet := GBColorController.SGBSets;

    FAppMessage.SendAppMessage(TILEMSGCOLSETS,integer(Application.handle));
  end;
end;


procedure TGBTileExchanger.ListToExchange;
begin
  if Active then
  begin
    UploadList;

    FAppMessage.SendAppMessage(TILEMSGLIST, integer(Application.handle));
  end;
end;

procedure TGBTileExchanger.UploadList;
var i,j : integer;
    p : PByteMem;
    b : PByteMem;
begin
  b := @PGBTileExchangerData(FAppMem.MemBlock).Tiles;
  PGBTileExchangerData(FAppMem.MemBlock).TileCount := FTileData.Count;

  for j := 0 to FTileData.Count-1 do
  begin
    p := FTileData.Items[j];
    PGBTileExchangerData(FAppMem.MemBlock).PalMaps[j] := PTileType(p).ColorSet;

    TileToEx(p,b);
    b := pointer(integer(b) + (FWidth * FHeight));
{

    for i:=0 to FHeight-1 do
    begin
      CopyBuf(p, b, FWidth);
      b := pointer(integer(b) + FWidth);
      p := pointer(integer(p) + 32);
    end;}
  end;
end;

procedure TGBTileExchanger.ExchangeToList;
var i,j : integer;
    p : PByteMem;
    b : PByteMem;
    cnt : integer;
begin
  cnt := PGBTileExchangerData(FAppMem.MemBlock).TileCount;
  b := @PGBTileExchangerData(FAppMem.MemBlock).Tiles;
  for j := 0 to Cnt-1 do
  begin
    if (j >= FTileData.Count) then
    begin
      GetMem(p, SizeOf(TileType));
      FTileData.Add(p);
    end
    else
      p := FTileData.Items[j];
    PTileType(p).ColorSet := PGBTileExchangerData(FAppMem.MemBlock).PalMaps[j];

    ExToTile(b,p);
    b := pointer(integer(b) + (FWidth * FHeight));


{    for i:=0 to FHeight-1 do
    begin
      CopyBuf(b, p, FWidth);
      b := pointer(integer(b) + FWidth);
      p := pointer(integer(p) + 32);
    end;}
  end;

  for i:= cnt to FTileData.Count-1 do
  begin
    dispose( PTileType(FTileData.Items[cnt]));
    FTileData.Delete(cnt);
  end;

end;




procedure TGBTileExchanger.TotalToExchange;
begin
  if Active then
    with PGBTileExchangerData(FAppMem.MemBlock)^ do
    begin
      ID         := 'TU01';
      TileCount  := FTileData.Count-1;
      TileWidth  := FWidth;
      TileHeight := FHeight;

      UploadList;

      Palettes := GBColorController.Palette;
      GBCColSet := GBColorController.CGBSets;
      SGBColSet := GBColorController.SGBSets;

      FAppMessage.SendAppMessage(TILEMSGTOTAL,integer(Application.handle));
    end;
end;


procedure TGBTileExchanger.SetFileName( f : TFileName );
var b : boolean;
begin
  if ( f <> FFileName ) then
  begin
    b := Active;
    Active := False;

    FFileName := f;
    if b then Active := True;
  end;
end;


procedure TGBTileExchanger.SetActive( b : boolean );
var Existed : boolean;
begin
  if (b <> Active) then
  begin
    (* close off current resources *)
    if Assigned(FAppMessage) then FAppMessage.Free;
    FAppMessage := nil;

    if Assigned(FAppMem) then FAppMem.Free;
    FAppMem := nil;

    FActive := False;


    if b and (FFileName <> '') then
    begin
      (* setup new resources *)
      FAppMessage := TAppMessage.Create( FMainControl, 'GBHMTILE' + FFileName );
      FAppMessage.OnMessage := MessageHandler;

      FAppMem := TAppMem.Create( FFileName, SizeOf(TGBTileExchangerData), FReadOnly, Existed );


      FAppMessage.Active := True;
      FActive := True;

      if (not FReadOnly) then TotalToExchange
      else if Existed then ExchangeToTotal;
    end;
  end;
end;




procedure TGBTileExchanger.TileToEx( inb, outb : PByteMem );
type PInt = ^integer;
var i,b : integer;
begin
  case FTileSize of
    gbts8x8:
      begin
        for i := 0 to 7 do
        begin
          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          integer(inb) := integer(inb) + (32-4);
          PInt(outb)^ := b;
          inc(PInt(outb));
        end;
      end;

    gbts8x16:
      begin
        for i := 0 to 15 do
        begin
          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          integer(inb) := integer(inb) + (32-4);
          PInt(outb)^ := b;
          inc(PInt(outb));
        end;
      end;

    gbts16x16:
      begin
        for i := 0 to 15 do
        begin
          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          integer(inb) := integer(inb) + (32-12);
          PInt(outb)^ := b;
          inc(PInt(outb));
        end;
      end;

    gbts32x32: CopyBuf(inb, outb, 32*32);

  end;
end;


procedure TGBTileExchanger.ExToTile( inb, outb : PByteMem );
type PInt = ^integer;
var i,b : integer;
begin
  case FTileSize of
    gbts8x8:
      begin
        for i := 0 to 7 do
        begin
          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          integer(outb) := integer(outb) + (32-4);
        end;
      end;

    gbts8x16:
      begin
        for i := 0 to 15 do
        begin
          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          integer(outb) := integer(outb) + (32-4);
        end;
      end;

    gbts16x16:
      begin
        for i := 0 to 15 do
        begin
          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          inc(PInt(outb));

          b := PInt(inb)^;
          inc(PInt(inb));
          PInt(outb)^ := b;
          integer(outb) := integer(outb) + (32-12);
        end;
      end;

    gbts32x32: CopyBuf(inb, outb, 32*32);

  end;
end;


end.
