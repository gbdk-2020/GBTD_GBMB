unit gbofile;

interface

  uses
  {Classes, SysUtils, windows;}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids,  gbpgrd, Buttons, ExtCtrls, gbconst, gbshow, ComCtrls,
  Menus,  gbcbtn, Spin, gbtlst, MainLib;

  const
    OLDPROPCNT = 7;


  const GBOProducerID = $01;
  type TGBOProducer = record
    Name    : array[0..29] of char;
    Version : array[0..9] of char;
    Info    : array[0..79] of char;
  end;


  const GBOTileDataID = $02;
  type TGBOTileData = record
    Name     : array[0..29] of char;
    Width    : Word;
    Height   : Word;
    Count    : Word;
    ColorSet : array[0..3] of char;
    Data     : array[0..0] of char;  (* Width * Height * Count *)
  end;


  const GBOTileSettingsID = $03;
  type TGBOTileSettings = record
    TileID      : Word;
    Simple      : boolean;
    Flags       : byte;
    LeftColor   : byte;
    RightColor  : byte;

    (* GBTD 0.9 *)
    SplitWidth  : Word;
    SplitHeight : Word;
    SplitOrder  : TSplitOrder;

    (* GBTD 1.0 *)
    ColorSet    : byte;

    (* GBTD 1.1 *)
    Bookmarks   : array[0..2] of word;

    (* GBTD 2.0 *)
    AutoUpdate  : boolean;
  end;


  const
    GBOTSGrid   = $01;
    GBOTSNibble = $02;


  const GBOTileExportID = $04;
  type TGBOTileExport = record
    TileID      : Word;
    FileName    : array[0..127] of char;
    FileType    : Byte;
    SectionName : array[0..19] of char;
    LabelName   : array[0..19] of char;
    Bank        : Byte;
    TileArray   : Boolean;
    Format      : byte;
    Counter     : byte;
    From        : Word;
    Upto        : Word;
    Compression : byte;

    (* GBTD 1.3 *)
    IncludeColors : Boolean;

    (* GBTD 1.4 *)
    SGBPalettes   : byte;
    CGBPalettes   : byte;

    (* GBTD 1.5 *)
    MakeMetaTiles   : boolean;
    MetaOffset      : integer;
    MetaCounter     : byte;

    (* GBTD 1.8 *)
    Split           : boolean;
    BlockSize       : integer;
    SelTab          : byte;
  end;


  const GBOTileImportID = $05;
  type TGBOTileImport = record
    TileID          : Word;
    FileName        : array[0..127] of char;
    FileType        : Byte;
    FromTile        : Word;
    ToTile          : Word;
    TileCount       : Word;
    ColorConversion : Byte;

    (* GBTD 1.5 *)

    FirstByte       : integer;
    BinType         : Byte;
  end;


  const GBOMovieSettingsID = $06;
  type TGBOMovieSettings = record
    MovieID    : Word;
    Simple     : boolean;
    Flags      : byte;
    LeftColor  : byte;
    RightColor : byte;
  end;

  const
    GBOMSGrid       = $01;
    GBOMSTileMarker = $02;

  const GBOMovieDataID = $07;
  type TGBOMovieData = record
    Name     : array[0..29] of char;
    FrmWidth    : Word;
    FrmHeight   : Word;
    FrmCount    : Word;
    ColorSet : array[0..3] of char;
    Data     : array[0..0] of char;  (* Width * Height * Count / 4 *)
  end;


  const GBOMapDataID = $08;
    type
      RGBOMapRec = record
      Tile   : Word;
      Prop   : array [0..6] of Word;
    end;
  type TGBOMapData = record
    Name     : array[0..29] of char;
    Width    : Word;
    Height   : Word;
    TileFile : string[127];
    Data     : array[0..0] of RGBOMapRec;  (* Width * Height *)
  end;


  const GBOMapPropID = $09;
    type
      RGBOMapPropRec = packed record
        pType  : byte;             // Type of property
        Name   : String[22];
        Size   : integer;
    end;
  type TGBOMapProp = record
    MapID    : Word;
    Count    : Word;
    Data     : array[0..0] of RGBOMapPropRec;  (* Count *)
  end;


  const GBOMapExportID = $0A;
    type
      RGBOMapExportPropDefRec = record
        Nr     : byte;
        Size   : integer;
    end;
  type TGBOMapExport = record
    MapID       : Word;
    FileName    : array[0..127] of char;
    FileType    : Byte;
    SectionName : array[0..19] of char;
    LabelName   : array[0..19] of char;
    Bank        : Byte;
    PlaneCnt    : Word;
    PlaneOrder  : Word;
    Reserved    : array[0..9] of char;
    PropCnt     : Word;
    Props       : array[0..7] of RGBOMapExportPropDefRec;  (* PropCnt *)

    (* GBMB 0.6 *)
    MapLayout   : Word;
  end;


  const GBOMapDefaultTilePropID = $0B;
  type RDefaultTileProp = array [0..OLDPROPCNT-1] of word;
  type TGBOMapDefaultTileProp = record
    MapID       : Word;
    Count       : integer;
    Props       : array [0..383] of RDefaultTileProp;
  end;

  const GBOMapSettingsID = $0C;
  type TGBOMapSettings = record
    MapID       : Word;
    Width       : integer;
    Height      : integer;
    Zoom        : word;
    InfoPanel   : boolean;

    (* GBMB 0.2 *)
    ColorSet    : byte;

    (* GBMB 0.4 *)
    Grid             : Boolean;
    DoubleMarkers    : Boolean;
    Bookmarks        : array[0..2] of word;
    BlockFillPattern : integer;
    BlockFillWidth   : integer;
    BlockFillHeight  : integer;

    (* GBMB 0.7 *)
    Maximized        : boolean;

    (* GBMB 0.8 *)
    PropColors       : boolean;    
  end;

  const GBOPalettesID = $0D;
  type TGBOPalettes = record
    ID          : Word;
    Count       : Word;
    Colors      : TGBColorSets;
    SGBCount    : Word;             (* variable starting point *)
    SGBColors   : TGBColorSets;
  end;

  const GBOTilePalID = $0E;
  type TGBOTilePal = record
    TileID   : Word;
    Count    : Word;
    ColorSet : array[0..0] of integer;          (* Count *)
    SGBCount : Word;                (* variable starting point *)
    SGBColorSet : array[0..0] of integer;          (* Count *)
  end;

  (* GBMB 0.8 *)
  const GBOPropColorsID = $0F;
    type TPropColorsRec = record
      Prop        : integer;
      Operator    : integer;
      Value       : integer;
    end;
  type TGBOPropColors = record
    ID          : Word;
    Count       : Word;
    Colors      : array[0..1] of TPropColorsRec;
  end;

  (* GBMB Pro 1.0 *)
  const GBOMapPropInitValID = $10;
  type TGBOMapPropInitVal = packed record
    MapID       : Word;
    TileCount   : Word;
    PropCount   : Integer;
    Props       : array [0..0] of Word;    (* Prop#, Tile# *)
  end;

  const GBOMapPropDataID = $11;
  type TGBOMapPropData = packed record
    MapID       : Word;
    PropCount   : Word;
    Width       : Integer;
    Height      : Integer;
    Data        : array [0..0] of Word;    (* Prop * Width * Height *)
  end;
  PGBOMapPropData = ^TGBOMapPropData;


  const GBOExtMapDataID = $12;
  type TGBOExtMapData = packed record
    Name        : array[0..29] of char;
    TileFile    : string[255];
    Width       : Integer;
    Height      : Integer;
    Data        : array [0..0] of Word;    (* Width * Height *)
  end;
  PGBOExtMapData = ^TGBOExtMapData;

  (* Data:                         *)
  (*     15    Vert Flip           *)
  (*     14    Hor Flip            *)
  (*  13-10    Palette             *)
  (*    9-0    TileNo              *)



  const GBODeleted = $FF;

  type TGBOInternalInfo = record
    ID       : Word;
    ObjectID : Word;
    Size     : LongInt;
  end;



  type TGBOFile = class
    private
      FStrm    : TFileStream;
      FCurInfo : TGBOInternalInfo;
      FPos     : Integer;

      procedure ProcessHeader;

      function GetCurID: Word;
      function GetCurSize: LongInt;
      function GetCurObjectID: Word;
      function FileIsEmpty : boolean;

    protected





    public
      property CurID : word read GetCurID;
      property CurSize : LongInt read GetCurSize;
      property CurObjectID : word read GetCurObjectID;
      property Empty : boolean read FileIsEmpty;

      procedure CreateNewObject(const ID : Word; const ObjectID : Word; const Count : LongInt);
      procedure DeleteObject;

      procedure SaveTo(tID : word; ObjectID : word; Count : longint; buf : pointer );
      function LoadFrom(tID : word; ObjectID : word; Count : longint; buf : Pointer ): boolean;
      function Locate(tID : word; ObjectID : word): boolean;

      function EOF : boolean;
      procedure LoadData(  buf : Pointer; Count : longint );
      procedure SaveData(  buf : Pointer; Count : longint);
      procedure AssureSpace ( Count : longint );

      procedure First;
      procedure Next;

      constructor Create( const f : string; NewFile : Boolean);
      destructor Destroy; override;

  end;

type
  EGBOError = class(Exception);

implementation


type
  TArray = array[0..1] of char;
  TPArray = ^TArray;


procedure TGBOFile.ProcessHeader;
begin
  with Fstrm do
  begin
    Read(FCurInfo, SizeOf(TGBOInternalInfo));
    FPos := Position;
  end;
end;


function TGBOFile.FileIsEmpty : boolean;
begin
  Result := (FStrm.Size = 4);
end;


procedure TGBOFile.LoadData( buf : Pointer; Count : longint);
var i : LongInt;
begin
  if Empty then raise EGBOError.Create('End of file reached');

  FStrm.Seek(FPos, soFromBeginning);

  i := Count;
  if (i > FCurInfo.Size) then i := FCurInfo.Size;
  Fstrm.Read(buf^, i);
end;


procedure TGBOFile.First;
begin
  with Fstrm do
  begin
    seek(4, soFromBeginning);
    ProcessHeader;
  end;
end;


procedure TGBOFile.Next;
begin
  with Fstrm do
  begin
    Seek(FCurInfo.Size + FPos, soFromBeginning);
    if not Eof then
      ProcessHeader;
  end;
end;


procedure TGBOFile.CreateNewObject(const ID : Word; const ObjectID : Word; const Count : LongInt);
var i : LongInt;
var p : TPArray;
begin
  FStrm.Seek(0, soFromEnd);
  FCurInfo.ID := ID;
  FCurInfo.ObjectID := ObjectID;
  FCurInfo.Size := Count;

  FStrm.Write(FCurInfo, SizeOf(TGBOInternalInfo));
  FPos := Fstrm.Position;

  GetMem(p, Count);
  try
    for i := 0 to count-1 do
      p[i] := char(0);
    FStrm.Write(p, Count);
  finally
    FreeMem(p, Count);
  end;
end;


procedure TGBOFile.DeleteObject;
var w : word;
begin
  if Empty then raise EGBOError.Create('End of file reached');

  FStrm.Seek(FPos - SizeOf(TGBOInternalInfo), soFromBeginning);
  w := GBODeleted;
  FStrm.Write(w, SizeOf(Word));
end;


procedure TGBOFile.SaveData(  buf : pointer; Count : longint);
var i : LongInt;
begin
  FStrm.Seek(FPos, soFromBeginning);

  i := Count;
  if (i > FCurInfo.Size) then i := FCurInfo.Size;
  Fstrm.Write(buf^, i);
end;


procedure TGBOFile.AssureSpace ( Count : longint );
var p    : TPArray;
    Info : TGBOInternalInfo;
begin

  if Empty then raise EGBOError.Create('End of file reached');

  if ( Count > FCurInfo.Size ) then
  begin

    (* save data *)
    Info := FCurInfo;
    GetMem(p, Info.Size);
    try
      LoadData(p, Info.Size);

      (* Delete current record *)
      DeleteObject;

      (* Build up new one *)
      with Info do CreateNewObject( ID, ObjectID, Count );
      SaveData(p, Count);

    finally
      FreeMem(p, Info.Size );
    end;
  end;

end;

procedure TGBOFile.SaveTo( tID: word; ObjectID : word; Count : longint; buf : pointer );
var AddNew : boolean;
begin

  AddNew := False;

  (* First, Check current block *)
  if not((FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID)) or Empty then
  begin
    AddNew := True;

    (* walk through all *)
    First;
    if (FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID) and (not Eof) then
      AddNew := False
    else
      while (not EOF) and (AddNew) do
      begin
        Next;
        if (FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID) then
          AddNew := False
      end;
  end;

  if AddNew then
    CreateNewObject(tID, ObjectID, Count)
  else
    AssureSpace(Count);

  SaveData( buf, Count );
end;




function TGBOFile.GetCurID : Word;
begin
  if Empty then raise EGBOError.Create('End of file reached');
  result := FCurInfo.ID;
end;




function TGBOFile.GetCurSize : LongInt;
begin
  if Empty then raise EGBOError.Create('End of file reached');
  result := FCurInfo.Size;
end;



function TGBOFile.GetCurObjectID : Word;
begin
  if Empty then raise EGBOError.Create('End of file reached');
  result := FCurInfo.ObjectID;
end;




function TGBOFile.EOF : boolean;
begin
  result := Empty or ((FPos + FCurInfo.Size) >= FStrm.Size);
end;



constructor TGBOFile.Create( const f : string; NewFile : boolean);
var b : array [0..4] of char;
    i : integer;
begin

  if NewFile then
  begin
    FStrm := TFileStream.Create( f, fmCreate);

    (* Create File type ID *)
    FStrm.Write( 'GBO0' , 4);
  end
  else
  begin

    i := 100;
    repeat
      try
        FStrm := TFileStream.Create( f, fmOpenReadWrite);
        i := 0;
      except
        on EFOpenError do
          begin
            dec(i);
            if (i = 0) then raise;
            Application.ProcessMessages;
          end;
      end;
    until (i = 0);


    with FStrm do
    begin
      (* Check on ID *)
      Read( b, 3);
      b[3] := chr(0);
      if ( StrComp(b, 'GBO') <> 0) then
        raise EGBOError.Create('This is not a GBO-file');

      (* Check version *)
      Read(b, 1);
      if (b[0] <> '0') then
        raise EGBOError.Create('This GBO-file version is not supported');

      if (Position < Size) then
        ProcessHeader
    end;
  end;

  FPos := FStrm.Position;

end;



destructor TGBOFile.Destroy;
begin
  FStrm.Free;
end;


function TGBOFile.Locate(tID : word; ObjectID : word): boolean;
var Found : boolean;
begin
  Found := True;

  (* First, Check current block *)
  if not((FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID)) or Empty then
  begin
    Found := False;

    (* walk through all *)
    First;
    if (FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID) and (not Eof) then
      Found := True
    else
      while (not EOF) and (not Found) do
      begin
        Next;
        if (FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID) then
          Found := True;
      end;
  end;

  Result := Found;
end;



function TGBOFile.LoadFrom(tID : word; ObjectID : word; Count : longint; buf : Pointer ): boolean;
var p : Pointer;
begin
  (* set to default *)
  ClearBuf( buf, Count);

  (* load possible object *)
  Result := Locate(tID, ObjectID);
  if Result then LoadData( buf, Count );            
end;


end.
