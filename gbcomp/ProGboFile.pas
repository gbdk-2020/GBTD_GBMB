unit ProGboFile;

interface

  uses
  {Classes, SysUtils, windows;}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids,  gbpgrd, Buttons, ExtCtrls, gbconst, gbshow, ComCtrls,
  Menus,  gbcbtn, Spin, gbtlst, MainLib;



  (*-------------  GLOBAL  ---------------*)

  const ProGBOProducerID = $01;
  type TProGBOProducer = record
    Name    : array [0..127] of char;
    Version : array [0..9] of char;
    Info    : array [0..127] of char;
  end;


  (*-------------  GBMB  ---------------*)

  const ProGBOMapID = $02;
  type TProGBOMap = packed record
    Name        : array [0..127] of char;
    Width       : Integer;
    Height      : Integer;
    PropCount   : Integer;
    TileFile    : array [0..255] of char;
    TileCount   : Integer;                   (* for default storage *)
    PropColorCount : Integer;
  end;
  PProGBOMap = ^TProGBOMap;


    const ProGBOMapTileDataID = $03;
      type ProGBOMapTileDataRec = packed record
        Tile  :  array[0..2] of byte;
        (*    Bit    Description           *)
        (*    0-8    Tile-number (0..511)  *)
        (*   9-21    Reserved, set to 0    *)
        (*     22    flipped horizontally  *)
        (*     23    flipped vertically    *)
      end;
    type TProGBOMapTileData = packed record
      Data        : array [0..0] of ProGBOMapTileDataRec;    (* Width * Height *)


    end;
    PProGBOMapTileData = ^TProGBOMapTileData;


    const ProGBOMapPropID = $04;
      type
        ProGBOMapPropRec = packed record
          pType  : integer;             // Type of property
          Size   : integer;
          Name   : array[0..31] of char;
      end;
    type TProGBOMapProp = record
      Data     : array[0..0] of ProGBOMapPropRec;  (* Count *)
    end;
    PProGBOMapProp = ^TProGBOMapProp;

    const ProGBOMapPropDataID = $05;
    type TProGBOMapPropData = packed record
      Data        : array[0..0] of Word;    (* PropCount * Width * Height *)
    end;
    PProGBOMapPropData = ^TProGBOMapPropData;


    const ProGBOMapPropDefaultID = $06;
    type TProGBOMapPropDefault = packed record
      Data       : array[0..0] of Word;    (* PropCount * TileCount *)
    end;
    PProGBOMapPropDefault = ^TProGBOMapPropDefault;


    const ProGBOMapSettingsID = $07;
    type TProGBOMapSettings = packed record
      FormWidth        : integer;
      FormHeight       : integer;
      FormMaximized    : boolean;

      InfoPanel        : boolean;
      Grid             : Boolean;
      DoubleMarkers    : Boolean;
      PropColors       : boolean;

      Zoom             : word;
      ColorSet         : Word;
      Bookmarks        : array[0..2] of word;

      BlockFillPattern : integer;
      BlockFillWidth   : integer;
      BlockFillHeight  : integer;

      (* GBMB 1.3 *)
      AutoUpdate       : boolean;
    end;
    PProGBOMapSettings = ^TProGBOMapSettings;


    const ProGBOPropColorsID = $08;
      type TPropColorsRec = record
        Prop        : integer;
        Operator    : integer;
        Value       : integer;
      end;
    type TProGBOPropColors = record
      Colors      : array[0..1] of TPropColorsRec;
    end;

    const ProGBOMapExportID = $09;
    type TProGBOMapExport = packed record
      FileName    : array[0..254] of char;
      FileType    : Byte;
      SectionName : array[0..39] of char;
      LabelName   : array[0..39] of char;
      Bank        : Byte;

      PlaneCnt    : Word;
      PlaneOrder  : Word;
      MapLayout   : Word;

      Split       : boolean;
      SplitSize   : integer;
      SplitBank   : boolean;


      SelTab      : Byte;

      PropCnt     : Word;

      (* GBMB 1.2 *)
      TileOffset  : Word;
    end;
    PProGBOMapExport = ^TProGBOMapExport;


    const ProGBOMapExportPropID = $0A;
      type
        RProGBOMapExportPropDefRec = record
          Nr     : byte;
          Size   : integer;
      end;
    type TProGBOMapExportProp = packed record
      Props       : array[0..0] of RProGBOMapExportPropDefRec;  (* PropCnt *)
    end;
    PProGBOMapExportProp = ^TProGBOMapExportProp;








  const GBODeleted = $FFFF;


  type TMagicMarker = array[0..5] of char;

    TProGBOInternalInfo = packed record
      Marker         : TMagicMarker;
      ID             : Word;
      ObjectID       : Word;
      MasterID       : Word;
//      RecordLength   : LongInt;
      CRC            : Integer;
      Size           : LongInt;
    end;



  type TProGBOFile = class
    private
      FStrm    : TFileStream;
      FCurInfo : TProGBOInternalInfo;
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

//      procedure CreateNewObject(const ID : Word; const ObjectID : Word; const MasterID : Word; const Count : LongInt);
        procedure CreateNewObject( const ID, ObjectID, MasterID : Word;
                                   const ObjectLength : LongInt);

      procedure DeleteObject;

//      procedure SaveTo( tID: word; ObjectID, MasterID : word; Count : longint; buf : pointer );
        procedure SaveTo( const tID, ObjectID, MasterID : word;
                          const ObjectLength : longint; buf : pointer );
//      function LoadFrom(tID : word; ObjectID : word; Count : longint; buf : Pointer ): boolean;
      function LoadFrom(tID, ObjectID, MasterID : word; Count : longint; buf : Pointer ): boolean;

//      function Locate(tID : word; ObjectID : word): boolean;
      function Locate(tID : word; ObjectID, MasterID : word): boolean;

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


procedure TProGBOFile.ProcessHeader;
begin
  with Fstrm do
  begin
    Read(FCurInfo, SizeOf(TProGBOInternalInfo));
    FPos := Position;
  end;
end;


function TProGBOFile.FileIsEmpty : boolean;
begin
  Result := (FStrm.Size = 4);
end;


procedure TProGBOFile.LoadData( buf : Pointer; Count : longint);
var i : LongInt;
begin
  if Empty then raise EGBOError.Create('End of file reached');

  FStrm.Seek(FPos, soFromBeginning);

  i := Count;
  if (i > FCurInfo.Size) then i := FCurInfo.Size;
  Fstrm.Read(buf^, i);
end;


procedure TProGBOFile.First;
begin
  with Fstrm do
  begin
    seek(4, soFromBeginning);
    ProcessHeader;
  end;
end;


procedure TProGBOFile.Next;
begin
  with Fstrm do
  begin
    Seek(FCurInfo.Size + FPos, soFromBeginning);
    if not Eof then
      ProcessHeader;
  end;
end;


procedure TProGBOFile.CreateNewObject(const ID, ObjectID, MasterID : Word;
                                       const ObjectLength : LongInt);

const MagicMarker : TMagicMarker = ('H','P','J','M','T','L');

var i : LongInt;
var p : TPArray;
begin
  FStrm.Seek(0, soFromEnd);

  FCurInfo.Marker := MagicMarker;
  FCurInfo.ID := ID;
  FCurInfo.ObjectID := ObjectID;
  FCurInfo.Size := ObjectLength;
  FCurInfo.MasterID := MasterID;
  FCurInfo.CRC := 0;             (* not implemented *)

  FStrm.Write(FCurInfo, SizeOf(TProGBOInternalInfo));
  FPos := Fstrm.Position;

  GetMem(p, ObjectLength);
  try
    ClearBuf(p, ObjectLength);
    FStrm.Write(p, ObjectLength);
  finally
    FreeMem(p, ObjectLength);
  end;
end;


{    TProGBOInternalInfo = packed record
      Marker   : TMagicMarker;
      ID       : Word;
      ObjectID : Word;
      MasterID : Word;
      CRC      : Integer;
      Size     : LongInt;
    end;
}

procedure TProGBOFile.DeleteObject;
var DelRec : TProGBOInternalInfo;
begin
  if Empty then raise EGBOError.Create('End of file reached');

  FStrm.Seek(FPos - SizeOf(TProGBOInternalInfo), soFromBeginning);
  FStrm.Read(DelRec, SizeOf(TProGBOInternalInfo));
  DelRec.ID := GBODeleted;
  FStrm.Seek(FPos - SizeOf(TProGBOInternalInfo), soFromBeginning);
  FStrm.Write(DelRec, SizeOf(TProGBOInternalInfo));
end;


procedure TProGBOFile.SaveData(  buf : pointer; Count : longint);
var i : LongInt;
begin
  FStrm.Seek(FPos, soFromBeginning);

  i := Count;
  if (i > FCurInfo.Size) then i := FCurInfo.Size;
  Fstrm.Write(buf^, i);
end;


procedure TProGBOFile.AssureSpace ( Count : longint );
var p    : TPArray;
    Info : TProGBOInternalInfo;
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
      with Info do CreateNewObject( ID, ObjectID, MasterID, Count );
      SaveData(p, Count);

    finally
      FreeMem(p, Info.Size );
    end;
  end;

end;

procedure TProGBOFile.SaveTo( const tID, ObjectID, MasterID : word;
                              const ObjectLength : longint;
                              buf : pointer );
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
    CreateNewObject(tID, ObjectID, MasterID, ObjectLength)
  else
    AssureSpace(ObjectLength);

  SaveData( buf, ObjectLength );
end;




function TProGBOFile.GetCurID : Word;
begin
  if Empty then raise EGBOError.Create('End of file reached');
  result := FCurInfo.ID;
end;




function TProGBOFile.GetCurSize : LongInt;
begin
  if Empty then raise EGBOError.Create('End of file reached');
  result := FCurInfo.Size;
end;



function TProGBOFile.GetCurObjectID : Word;
begin
  if Empty then raise EGBOError.Create('End of file reached');
  result := FCurInfo.ObjectID;
end;




function TProGBOFile.EOF : boolean;
begin
  result := Empty or ((FPos + FCurInfo.Size) >= FStrm.Size);
end;



constructor TProGBOFile.Create( const f : string; NewFile : boolean);
var b : array [0..4] of char;
begin

  if NewFile then
  begin
    FStrm := TFileStream.Create( f, fmCreate);

    (* Create File type ID *)
    FStrm.Write( 'GBO1' , 4);
  end
  else
  begin
    FStrm := TFileStream.Create( f, fmOpenReadWrite);

    with FStrm do
    begin
      (* Check on ID *)
      Read( b, 3);
      b[3] := chr(0);
      if ( StrComp(b, 'GBO') <> 0) then
        raise EGBOError.Create('This is not a GBO-file');

      (* Check version *)
      Read(b, 1);
      if (b[0] <> '1') then
        raise EGBOError.Create('This GBO-file version is not supported');

      if (Position < Size) then
        ProcessHeader
    end;
  end;

  FPos := FStrm.Position;

end;



destructor TProGBOFile.Destroy;
begin
  FStrm.Free;
end;


function TProGBOFile.Locate(tID : word; ObjectID, MasterID : word): boolean;
var Found : boolean;
begin
  Found := True;

  (* First, Check current block *)
  if not((FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID) and (FCurInfo.MasterID = MasterID)) or Empty then
  begin
    Found := False;

    (* walk through all *)
    First;
    if (FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID) and (FCurInfo.MasterID = MasterID) and (not Eof) then
      Found := True
    else
      while (not EOF) and (not Found) do
      begin
        Next;
        if (FCurInfo.ID = tID) and (FCurInfo.ObjectID = ObjectID) and (FCurInfo.MasterID = MasterID) then
          Found := True;
      end;
  end;

  Result := Found;
end;



function TProGBOFile.LoadFrom(tID, ObjectID, MasterID : word; Count : longint; buf : Pointer ): boolean;
var p : Pointer;
begin
  (* set to default *)
  ClearBuf( buf, Count);

  (* load possible object *)
  Result := Locate(tID, ObjectID, MasterID);
  if Result then LoadData( buf, Count );
end;                                              


end.
