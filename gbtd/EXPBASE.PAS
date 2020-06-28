unit expbase;
interface

uses Windows, Messages, SysUtils, classes, gbconst, gbofile, GBCompress, MainLib,
     Graphics;

type
  TByteArray = array[0..0] of byte;
  TPByteArray = ^TByteArray;

  TGBCounterType = (gbctByte, gbctWord, gbctEQU);

type
  TGBTDExport = class
  private
    { Private declarations }
    function GetData( const Info : TGBOTileExport; const Tile : TileType; const buf : TPByteArray ): integer;
    procedure PutCounter( const Info : TGBOTileExport; x,y : integer; size : integer; cnt : integer; lbl : string; Cnter : byte );
    function ProcessData( const Info : TGBOTileExport; const buf : TPByteArray; size : integer ): integer;
    procedure ExportPaletteIndexes(Info : TGBOTileExport; Lst : TList; GBType: integer );
    function ConvertToMetaTiles( Info : TGBOTileExport; TileSize : TGBTileSize; Buf : TList;TileCnt : integer): TList;

  protected
    { protected declarations }
    procedure WriteHeader( const Info : TGBOTileExport; x,y : integer ); virtual;
    procedure WriteInit( const Info : TGBOTileExport ); virtual;
    procedure WriteLabel( lbl : string; comment : string; MainLabel : boolean ); virtual;
    procedure WriteCounter( cnt : integer; CType : TGBCounterType; lbl : string ); virtual;
    procedure WriteData( buf : TPByteArray; size : integer); virtual;
    procedure WriteEnd( const Info : TGBOTileExport ); virtual;
    procedure WriteConstant( lbl : string; Value : string; comment : string ); virtual;

  public
    { Public declarations }
    constructor Create( fn : string ); virtual; abstract;
    function GBTDExport( Info : TGBOTileExport; TileSize : TGBTileSize; Lst : TList; const SGB, CGB  : TGBColorSets ): boolean;
  end;

  function TileArrayDesc( Ta : boolean ): string;
  function FormatDesc( Format : integer ): string;
  function CounterDesc( Counter : integer ): string;
  function CounterType( Counter : integer): TGBCounterType;
  function CompressionDesc( Compression : integer ): string;
  function PalColDesc( IncludeColors : boolean): string;
  function PalFormatDesc( Format : integer ): string;
  function MakeMetaTileDesc( MakeMetaTile : boolean): string;
  function MetaFormatDesc( Format : integer ): string;
  function MetaLayoutDesc( Layout : integer ): string;

implementation

uses tileEdit;


function CounterType( Counter : integer): TGBCounterType;
begin
  case ((Counter-1) mod 3) of
    0  : Result := gbctByte;
    1  : Result := gbctWord;
    2  : Result := gbctEQU;
  end;
end;


function TileArrayDesc( Ta : boolean ): string;
begin
  if Ta then result := 'All tiles as one unit.'
        else result := 'Each tile separate.';
end;

function CompressionDesc( Compression : integer ): string;
begin
  if (Compression = 0) then Result := 'None.' else Result := 'GB-Compress.';
end;

function PalColDesc( IncludeColors : boolean): string;
begin
  if (IncludeColors) then Result := 'Included.' else Result := 'None.';
end;

function MakeMetaTileDesc( MakeMetaTile : boolean): string;
begin
  if (MakeMetaTile) then Result := 'Yes.' else Result := 'No.';
end;

function FormatDesc( Format : integer ): string;
begin
  case Format of
    0   : Result := 'Gameboy 4 color.';
    1   : Result := 'Gameboy 2 color.';
    2   : Result := 'Byte per color.';
    3   : Result := 'Consecutive 4 color.';
  end;
end;

function PalFormatDesc( Format : integer ): string;
begin
  case Format of
    0   : Result := 'None.';
    1   : Result := 'Constant per entry.';
    2   : Result := '2 Bits per entry.';
    3   : Result := '4 Bits per entry.';
    4   : Result := '1 Byte per entry.';
  end;
end;

function MetaFormatDesc( Format : integer ): string;
begin
  case Format of
    0   : Result := 'None.';
    1   : Result := 'Constant per entry.';
    2   : Result := '2 Bits per entry.';
    3   : Result := '4 Bits per entry.';
    4   : Result := '1 Byte per entry.';
  end;
end;

function MetaLayoutDesc( Layout : integer ): string;
begin
  case Layout of
    0   : Result := 'Standard.';
    1   : Result := 'Left to right, top to bottom.';
  end;
end;

function CounterDesc( Counter : integer ): string;
begin
  case Counter of
    0   : Result := 'None.';
    1   : Result := 'Byte-count as Byte.';
    2   : Result := 'Byte-count as Word.';
    3   : Result := 'Byte-count as Constant.';
    4   : Result := 'Tile-count as Byte.';
    5   : Result := 'Tile-count as Word.';
    6   : Result := 'Tile-count as Constant.';
    7   : Result := '8x8-count as Byte.';
    8   : Result := '8x8-count as Word.';
    9   : Result := '8x8-count as Constant.';
  end;
end;

function TColorToPal( c : TColor) : integer;
begin
  Result := (GetRValue(ColorToRGB(c)) shr 3) +
            ((GetGValue(ColorToRGB(c)) shr 3) shl 5) +
            ((GetBValue(ColorToRGB(c)) shr 3) shl 10);
end;


procedure TGBTDExport.ExportPaletteIndexes(Info : TGBOTileExport; Lst : TList; GBType: integer );
var sType : string;
    PalIndex : integer;
    ExType : integer;
    i,j,k,l  : integer;
    buf : TPByteArray;
begin

  if (GBType = 0) then
  begin
    sType := 'CGB';
    PalIndex := 0;
    ExType := Info.CGBPalettes;
  end
  else
  begin
    sType := 'SGB';
    PalIndex := 1;
    ExType := Info.SGBPalettes;
  end;

  if (ExType <> 0) then
  begin
    (* get mem *)
    GetMem(buf, (Info.Upto-Info.From)+1 );
    try
      with Info do
      begin
        case ExType of
        1 :
          begin
            (* Constants *)
            WriteConstant( LabelName + sType + IntToStr(Info.From),
                           IntToStr( TileType(Lst.Items[Info.From]^).ColorSet[PalIndex]),
                           sType + ' palette entries.');

            for i:= Info.From+1 to Info.Upto do
              WriteConstant( LabelName + sType + IntToStr(i),
                             IntToStr( TileType(Lst.Items[i]^).ColorSet[PalIndex]), '');
          end;

        2 :
          begin
            (* 2 bits *)
            WriteLabel( LabelName + sType, sType + 'palette entries.', False);

            j := 0;
            l := 0;
            k := 0;
            for i:= Info.From to Info.Upto do
            begin
              k := (k shl 2);
              k := k + (TileType(Lst.Items[i]^).ColorSet[PalIndex] and $03);
              Inc(l);

              if (l >= 4) then
              begin
                buf[j] := k;
                Inc(j);
                l := 0;
                k := 0;
              end;
            end;

            if (l > 0) then
            begin
              buf[j] := (k shl ((4-l)*2));
              Inc(j);
            end;

            WriteData(buf,j);

          end;

       3 :
          begin
            (* 4 bits *)
            WriteLabel( LabelName + sType, sType + 'palette entries.', False);

            j := 0;
            l := 0;
            k := 0;
            for i:= Info.From to Info.Upto do
            begin
              k := (k shl 4);
              k := k + (TileType(Lst.Items[i]^).ColorSet[PalIndex] and $0F);
              Inc(l);

              if (l >= 2) then
              begin
                buf[j] := k;
                Inc(j);
                l := 0;
                k := 0;
              end;
            end;

            if (l > 0) then
            begin
              buf[j] := (k shl ((2-l)*4));
              Inc(j);
            end;

            WriteData(buf,j);

          end;

        4 :
          begin
            (* byte *)
            WriteLabel( LabelName + sType, sType + 'palette entries.', False);

            j := 0;
            for i:= Info.From to Info.Upto do
            begin
              buf[j] := TileType(Lst.Items[i]^).ColorSet[PalIndex];
              Inc(j);
            end;
            WriteData(buf,j);

          end;

        end;
      end;

    finally
      FreeMem(buf);
    end;

  end;
end;




function TGBTDExport.ConvertToMetaTiles( Info : TGBOTileExport; TileSize : TGBTileSize; Buf : TList; TileCnt : integer): TList;
type TLookupIndex = array[0..7] of word;
     TLookupArr   = array[0..MAXTILECNT-1] of TLookupIndex;

var Lst    : TList;
    i,j    : integer;
    pl     : TLookupIndex;
    Loc    : integer;
    Lookup : TLookupArr;
    Meta   : array[0..MAXTILECNT-1] of byte;
    Order  : array[0..MAXTILECNT-1] of word;
    Index  : array[0..MAXTILECNT-1] of byte;
    Cnt    : integer;
    w,h    : integer;

  procedure BuildLookup(var pl : TLookupIndex; const Tile : TileType);
  var x,y : integer;
  begin
    (* convert to 2 bit *)
    for y := 0 to 7 do
      for x := 0 to 7 do
      begin
        pl[y] := pl[y] shl 2;
        pl[y] := pl[y] + (Tile.data[y,x] and $03);
      end;
  end;

  function FindInLookup(const pl : TLookupIndex ): integer;
  var i : integer;
  begin
    Result := -1;
    for i := 0 to Cnt-1 do
      if MemComp(PByteMem(@Lookup[i]),PByteMem(@pl), SizeOf(TLookupIndex)) then
      begin
        Result := i;
        exit;
      end;
  end;


begin

  Lst := TList.Create;

  (* Analyse data *)
  j := 0;
  Cnt := 0;
  for i := (Info.From * TileCnt) to ((1+Info.UpTo) * TileCnt)-1 do
  begin

    (* determine meta-loc *)
    BuildLookup(pl, TileType(Buf.Items[i]^));

    Loc := FindInLookup(pl);
    if (Loc = -1) then
    begin
      (* add new one *)
      Lookup[Cnt] := pl;
      Order[Cnt] := i;
      Loc := Cnt;
      Inc(Cnt);
    end;

    (* save index *)
    Meta[j] := Loc + Info.MetaOffset;
    Inc(j);
  end;

  (* build new tile-list *)
  for i := 0 to Cnt-1 do
    Lst.Add( Buf.Items[Order[i]] );


  (* write index *)
  DetermineTileSize(TileSize, w, h);
  with Info do
  begin
    Writelabel( LabelName + 'Index', 'Metatile index', False );
    PutCounter(Info, w, h, j, 1 + (Upto-From), LabelName + 'Index', MetaCounter );
    writedata(@Meta, j );
  end;

  (* return list *)
  Result := Lst;
end;






function TGBTDExport.GBTDExport( Info : TGBOTileExport; TileSize : TGBTileSize; Lst : TList; const SGB, CGB : TGBColorSets ): boolean;
var i,j,size : integer;
    x,y : integer;
    buf : TPByteArray;
    TileCnt : integer;
    LstInt : TList;
    iFrom,iUpTo, iActualUpto : integer;


  procedure BuildTileData(const Lbl : string );
  var i,j : integer;
  begin
    with Info do
      if (TileArray) then
      begin
        (* Everything in one big bunch *)

        (* Main label *)
        WriteLabel(Lbl, 'Start of tile array.', False);

        GetMem(buf, x * y * (1 +(iUpTo-iFrom)) );
        try
          (* Process each tile *)
          size := 0;
          for i := (iFrom * TileCnt) to ((1+iUpTo) * TileCnt)-1 do
            size := size + GetData(Info, TileType(LstInt.Items[i]^), Addr(buf[size]) );

          (* write counter *)
          PutCounter(Info, x, y, size, 1 + (iUpto-iFrom), Lbl, Counter );

          Size := ProcessData( Info, buf, Size );

          (* write data *)
          WriteData(buf, size);
        finally
          FreeMem(buf, x * y * (1 +(iUpTo-iFrom)) );
        end;
      end
      else
      begin
        (* process per tile *)

        (* Main label *)
        WriteLabel(Lbl, 'Start of tile array.', True);

        GetMem(buf, x * y );
        try
          (* Process each tile *)
          for i := iFrom to iUpTo do
          begin
            Size := 0;
            for j :=0 to TileCnt-1 do
              size := size + GetData(Info, TileType(LstInt.Items[(i*TileCnt)+j]^), Addr(buf[Size]) );

            (* write tile *)
            Writelabel( Lbl + 'TLE' + IntToStr(i), '', False );
            PutCounter(Info, x, y, size, 1, Lbl + 'TLE' + IntToStr(i),Counter );

            Size := ProcessData( Info, buf, Size );
            WriteData(buf, size);
          end;

        finally
          FreeMem(buf, x * y );
        end;

      end;
  end;


begin
  DetermineTileSize(TileSize, x, y);
  TileCnt := (x div 8) * (y div 8);
  ConvertTileSize(TileSize, gbts8x8, Lst);
  LstInt := Lst;
  try

    WriteHeader(Info, x, y);
    WriteInit(Info);

    with Info do
    begin

      (* Bank constant *)
      WriteConstant( String(LabelName) + 'Bank', IntToStr(Bank), 'Bank of tiles.');


      if Info.IncludeColors then
      begin

        (* SGB Color palettes *)
        for i := 0 to 3 do
        begin
          WriteConstant( LabelName + SysUtils.Format('SGBPal%dc%d', [i,0]),
                         IntToStr(TColorToPal(SGB[i,0])), 'Super Gameboy palette ' + IntToStr(i));
          for j := 1 to 3 do
            WriteConstant( LabelName + SysUtils.Format('SGBPal%dc%d', [i,j]),
                           IntToStr(TColorToPal(SGB[i,j])), '')
        end;

        (* CGB Color palettes *)
        if FrmTile.mnuColorsetColor.Visible then
        begin
          for i := 0 to 7 do
          begin
            WriteConstant( LabelName + SysUtils.Format('CGBPal%dc%d', [i,0]),
                           IntToStr(TColorToPal(CGB[i,0])), 'Gameboy Color palette ' + IntToStr(i));
            for j := 1 to 3 do
              WriteConstant( LabelName + SysUtils.Format('CGBPal%dc%d', [i,j]),
                             IntToStr(TColorToPal(CGB[i,j])), '')
          end;
        end;
      end;


      (* SGB palettes *)
      ExportPaletteIndexes( Info, Lst,  1);

      (* CGB palettes *)
      ExportPaletteIndexes( Info, Lst,  0);

      if MakeMetaTiles then
      begin
        LstInt := ConvertToMetaTiles( Info, TileSize, Lst, TileCnt );
        iFrom := 0;
        iUpTo := LstInt.Count-1;
        TileCnt := 1;
      end
      else
      begin
        iFrom := Info.From;
        iUpTo := Info.Upto;
      end;


      if ( Split ) then
      begin
        iActualUpto := iUpto;
        WriteLabel(String(LabelName), 'Main block label.', True );
        for i:= 0 to ((iUpto-iFrom) div BlockSize)-1 do
        begin
          iUpto := (iFrom + BlockSize)-1;
          BuildTileData(String(LabelName) + 'BLK' + IntToStr(i));
          iFrom := iFrom + BlockSize;
        end;
        iUpto := iActualUpto;
        BuildTileData(String(LabelName) + 'BLK' + IntToStr(((iUpto-iFrom) div BlockSize)+1) );
      end
      else
        BuildTileData(String(LabelName));



    end;
    WriteEnd(Info);

  finally
    ConvertTileSize(gbts8x8, TileSize, Lst);
    if (LstInt <> Lst) then LstInt.Free;
  end;

end;



procedure TGBTDExport.PutCounter(const Info : TGBOTileExport; x,y : integer; size : integer; cnt : integer; lbl : string; Cnter : byte );
var i : integer;
begin
  with Info do
    if (Cnter > 0) then
    begin
      (* determine counter *)
      case ((Cnter-1) div 3) of
        0  : i := size;
        1  : i := cnt;
        2  : i := cnt * ((x div 8) * (y div 8));
      end;

      (* write counter *)
      WriteCounter(i, CounterType(Cnter), lbl);
    end;
end;



function TGBTDExport.GetData( const Info : TGBOTileExport; const Tile : TileType; const buf : TPByteArray ): integer;
var i,j     : integer;
    c, size : integer;
    plane   : array [0..1] of byte;

begin
  size := 0;

  case Info.Format of
    0  :
      (* Gameboy 4 color *)
      begin
        for j := 0 to (8-1) do
        begin
          plane[0] := 0;
          plane[1] := 0;

          for i := 0 to 7 do
          begin
            plane[0] := (plane[0] shl 1);
            plane[1] := (plane[1] shl 1);

            c := Tile.data[j,i];
            plane[0] := plane[0] + (c and $01);
            plane[1] := plane[1] + ((c and $02) shr 1);
          end;

          buf[size] := plane[0];
          Inc(Size);
          buf[size] := plane[1];
          Inc(Size);
        end;
      end;


    1  :
      (* Gameboy 2 color *)
      begin
        for j := 0 to (8-1) do
        begin
          plane[0] := 0;

          for i := 0 to 7 do
          begin
            plane[0] := (plane[0] shl 1);

            c := Tile.data[j, i];
            if (c = 3) then plane[0] := plane[0] + 1;
          end;

          buf[size] := plane[0];
          Inc(Size);
        end;
      end;


    2  :
      (* byte per color *)
      begin
        for j := 0 to (8-1) do
          for i := 0 to (8-1) do
          begin
            buf[size] := Tile.data[j, i];
            Inc(Size);
          end;
      end;

   3  :
      (* consecutive 4 color *)
      begin
        for j := 0 to (8-1) do
        begin
          plane[0] := 0;
          plane[1] := 0;

          for i := 0 to 3 do
          begin
            plane[0] := (plane[0] shl 2);
            c := Tile.data[j, i];
            plane[0] := plane[0] + (c and $03);
          end;

          for i := 4 to 7 do
          begin
            plane[1] := (plane[1] shl 2);
            c := Tile.data[j, i];
            plane[1] := plane[1] + (c and $03);
          end;

          buf[size] := plane[0];
          Inc(Size);
          buf[size] := plane[1];
          Inc(Size);
        end;
      end;

  end;

  Result := size;
end;


function TGBTDExport.ProcessData( const Info : TGBOTileExport; const buf : TPByteArray; Size : integer ): integer;
var tmp : TPByteArray;
    i   : integer;
begin
  Result := Size;
  if (Info.Compression <> 0) then
  begin
    GetMem(tmp, Size);
    try
      for i := 0 to Size-1 do tmp[i] := buf[i];
      Result := GBCompressBuf( PByteMem(tmp), Size, PByteMem(buf), Size );
    finally
      FreeMem(tmp);
    end;
  end;
end;



procedure TGBTDExport.WriteHeader( const Info : TGBOTileExport; x,y : integer );
begin
  (* use this to write the info.record to the source *)
end;

procedure TGBTDExport.WriteInit( const Info : TGBOTileExport );
begin
  (* use this to start the source (setting the section, opening brackets, etc) *)
end;

procedure TGBTDExport.WriteLabel( lbl : string; comment : string; MainLabel : boolean );
begin
  (* this writes a label *)
end;

procedure TGBTDExport.WriteConstant( lbl : string; Value : string; comment : string );
begin
  (* this writes a constant *)
end;

procedure TGBTDExport.WriteCounter( cnt : integer; CType : TGBCounterType; lbl : string );
begin
  (* this writes a counter byte/word of size cnt *)
end;

procedure TGBTDExport.WriteData( buf : TPByteArray; size : integer);
begin
  (* this writes size bytes from buf to the output *)
end;

procedure TGBTDExport.WriteEnd( const Info : TGBOTileExport );
begin
  (* use this as opposite of WriteInit *)
end;





end.
