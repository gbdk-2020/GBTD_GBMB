unit ExportClass;

interface

uses SysUtils,Classes, gbConst, MainLib, Repository, GBMBMain;



type
  TMapExport = class
  private
    { Private declarations }
    FOptions : TExportSettings;
    FTileMap : TGBMapType;
    FFileName : string;

    FBitCnt : integer;
    FPlaneCnt : integer;
    FPlanes  : array[0..3] of PByteMem;

    procedure BuildTile(x,y, FrameLoc : integer);
    function WriteData( Size, Offset, Block : integer; ExportData : boolean ): integer;

  protected
    { Protected declarations }
    procedure WriteHeader; virtual; abstract;
    procedure WriteSection( Offset : integer ); virtual; abstract;
    procedure WritePlaneLabel( i, Block : integer); virtual; abstract;
    procedure WriteLine( cnt : integer; EndOfData : boolean); virtual; abstract;
    procedure WriteEnd; virtual; abstract;
    procedure WriteBlockEnd; virtual; abstract;
    procedure WriteEQUS; virtual; abstract;

    property Options : TExportSettings read FOptions;
    property FileName : string read FFileName;
    property TileMap : TGBMapType read FTileMap;

  public
    { Public declarations }
    constructor Create(fn : string);

    procedure MapExport( const Options : TExportSettings; const TileMap : TGBMapType ; ExportData : boolean);
  end;

type
  TRGBDSMapExport = class(TMapExport)
  private
    { Private declarations }
    F : TextFile;
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteLine( cnt : integer ; EndOfData : boolean); override;
    procedure WriteBlockEnd; override;
    procedure WriteEnd; override;
    procedure WriteEQUS; override;

  public
    { Public declarations }
    constructor Create( fn : string );
    destructor Destroy; override;
  end;

  TRGBDSObjMapExport = class(TMapExport)
  private
    { Private declarations }
    FStrm : TFileStream;
    Lst : TList;
    DataBuf : PByteMem;
    Pos : integer;
    Section : string;
    Bank : integer;
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteLine( cnt : integer; EndOfData : boolean); override;
    procedure WriteEnd; override;
    procedure WriteBlockEnd; override;
    procedure WriteEQUS; override;

  public
    { Public declarations }
    constructor Create( fn : string );
    destructor Destroy; override;
  end;


  TRGBDSIncMapExport = class(TRGBDSMapExport)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteBlockEnd; override;

  public
    { Public declarations }
  end;


  TASMMapExport = class(TMapExport)
  private
    { Private declarations }
    F : TextFile;
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteLine( cnt : integer; EndOfData : boolean); override;
    procedure WriteBlockEnd; override;
    procedure WriteEnd; override;
    procedure WriteEQUS; override;

  public
    { Public declarations }
    constructor Create( fn : string );
    destructor Destroy; override;
  end;

  TGBDKMapExport = class(TMapExport)
  private
    { Private declarations }
    F : TextFile;
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteLine( cnt : integer; EndOfData : boolean); override;
    procedure WriteBlockEnd; override;
    procedure WriteEnd; override;
    procedure WriteEQUS; override;

  public
    { Public declarations }
    constructor Create( fn : string );
    destructor Destroy; override;
  end;

  TGBDKIncMapExport = class(TMapExport)
  private
    { Private declarations }
    F : TextFile;
    IncludeName : string;
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteLine( cnt : integer; EndOfData : boolean); override;
    procedure WriteBlockEnd; override;
    procedure WriteEnd; override;
    procedure WriteEQUS; override;

  public
    { Public declarations }
    constructor Create( fn : string );
    destructor Destroy; override;
  end;

  TBinMapExport = class(TMapExport)
  private
    { Private declarations }
    FStrm : TFileStream;
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteLine( cnt : integer; EndOfData : boolean); override;
    procedure WriteBlockEnd; override;
    procedure WriteEnd; override;
    procedure WriteEQUS; override;

  public
    { Public declarations }
    constructor Create( fn : string );
    destructor Destroy; override;
  end;


  TISASMapExport = class(TMapExport)
  private
    { Private declarations }
    F : TextFile;
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteLine( cnt : integer ; EndOfData : boolean); override;
    procedure WriteBlockEnd; override;
    procedure WriteEnd; override;
    procedure WriteEQUS; override;

  public
    { Public declarations }
    constructor Create( fn : string );
    destructor Destroy; override;
  end;


  TISASIncMapExport = class(TISASMapExport)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure WriteHeader; override;
    procedure WriteSection( Offset : integer ); override;
    procedure WritePlaneLabel( i, Block : integer); override;
    procedure WriteBlockEnd; override;
  public
    { Public declarations }
  end;




implementation

var DataStore : array[0..9] of byte;

procedure TMapExport.MapExport( const Options : TExportSettings; const TileMap : TGBMapType; ExportData : boolean);
var i,j,k,l,m : integer;
begin
  FOptions := Options;
  FTileMap := TileMap;

  (* Create planes *)
  if (FOptions.PlaneCnt = 0) then
  begin
    FPlaneCnt := 1;
    FBitCnt := 4;
  end
  else
  begin
    FPlaneCnt := FOptions.PlaneCnt;
    FBitCnt := 8;
  end;

  for i := 0 to FPlaneCnt-1 do GetMem(FPlanes[i], FTileMap.Width * FTileMap.Height);
  try


    (* build data *)
    If ExportData then
    begin
      k := 0;

      if (FOptions.MapLayout = 0) then
      begin
        (* Rows *)
        for j := 0 to FTileMap.Height-1 do
          for i := 0 to FTileMap.Width-1 do
          begin
            BuildTile(i,j,k);
            Inc(k);
          end;
      end
      else
      begin
        (* Cols *)
        for j := 0 to FTileMap.Width-1 do
          for i := 0 to FTileMap.Height-1 do
          begin
            BuildTile(j,i,k);
            Inc(k);
          end;
      end

    end;

    (* write file *)
    WriteHeader;
    WriteEQUS;
    WriteSection(0);
    (* write data *)
    if (not FOptions.Split) then
    begin
      WriteData( (FTileMap.Width * FTileMap.Height), 0, 0, ExportData);
      WriteBlockEnd;
    end
    else
    begin
      l := ((FTileMap.Width * FTileMap.Height) div FOptions.SplitSize);
      m := ((FTileMap.Width * FTileMap.Height) mod FOptions.SplitSize);
      for i := 0 to l-1 do
      begin
        if (FOptions.SplitBank) and (i > 0) then WriteSection(i);
        WriteData( FOptions.SplitSize, (FOptions.SplitSize*i), i, ExportData );
        WriteBlockEnd;
      end;

      if (m > 0) then
      begin
        if (FOptions.SplitBank) then WriteSection(l);
        WriteData( m, (FOptions.SplitSize*l), l, ExportData );
        WriteBlockEnd;
      end;
    end;

    WriteEnd;

  finally
    for i := 0 to FPlaneCnt-1 do FreeMem(FPlanes[i]);
  end;

end;




function TMapExport.WriteData( Size, Offset, Block : integer; ExportData : boolean ): integer;
var i : integer;
    k,l,m : integer;
begin
  m := 0;


  (* write data *)
  if Options.PlaneCnt = 0 then
  begin

    (* 0.5 planes *)
    WritePlaneLabel(0, Block);
    If ExportData then
    begin
      l := 0;
      m := Offset;
      for i := 0 to (Size div 2)+1 do
      begin
        if ((l and $01) = 0) then
        begin
          DataStore[l shr 1] := FPlanes[0, m] and $0F shl 4;
        end
        else
        begin
          DataStore[l shr 1] := DataStore[l shr 1] + (FPlanes[0, m] and $0F);
        end;

        inc(m);

        (* should a line be written ? *)
        Inc(l);
        if (l >= 18) then
        begin
          WriteLine(l shr 1, (m >= (Size + Offset-1)) );
          l := 0;
        end;
      end;

      (* write last bytes *)
      WriteLine((l+1) shr 1, True);
    end;

  end
  else if (Options.PlaneOrder = 0) then
  begin

    (* tiles are continues *)
    WritePlaneLabel(0, Block);
    If ExportData then
    begin
      l := 0;
      m := Offset;
      for i := 0 to Size-1 do
      begin
        for k := 0 to FPlaneCnt-1 do
        begin
          DataStore[l] := FPlanes[k, m];

          (* should a line be written ? *)
          Inc(l);
          if (l > 9) then
          begin
            WriteLine(l, (m >= (Size + Offset-1)) );
            l := 0;
          end;

        end;
        Inc(m);
      end;

      (* write last bytes *)
      WriteLine(l, True);
    end;

  end
  else
  begin

    (* Planes are continues *)
    l := 0;
    for k := 0 to FPlaneCnt-1 do
    begin
      WritePlaneLabel(k, Block);

      If ExportData then
      begin
        m := Offset;

        for i := 0 to Size-1 do
        begin
          DataStore[l] := FPlanes[k, m];

          Inc(m);

          (* should a line be written ? *)
          Inc(l);
          if (l > 9) then
          begin
            WriteLine(l, (m >= (Size + Offset)));
            l := 0;
          end;
        end;

        (* write last bytes *)
        WriteLine(l,True);
        l := 0;
      end;
    end;
  end;

  Result := m;
end;








procedure TMapExport.BuildTile(x, y, FrameLoc : integer);
var PropVal : integer;
    PropSize : integer;
    i, b  : integer;
    data : integer;

  function GetMask(cnt : integer): integer;
  var i : integer;
  begin
    Result := 0;
    for i := 0 to cnt-1 do
    begin
      Result := Result shl 1;
      Inc(Result);
    end;
  end;

  function GetTileWithOffset : integer;
  begin
    Result := FTileMap.Data[(y * FTileMap.Width) + x].Tile + FOptions.TileOffset;
  end;
  //FTileMap.Data[(y * FTileMap.Width) + x].Tile
begin

  data := 0;
  b := 0;

  for i := 0 to FOptions.PropCount-1 do
  begin

    (* get data *)
    case FOptions.Props[i].Nr of
      SPNONE      : PropVal := 0;
      SPTILENR    : PropVal := GetTileWithOffset;
      SPTILENR8   : PropVal := (GetTileWithOffset and $FF);
      SPTILENR9   : PropVal := (GetTileWithOffset shr 8);
      SPVERTFLIP  : PropVal := ((FTileMap.Data[(y * FTileMap.Width) + x].Flags shr 5) and $01);
      SPHORFLIP   : PropVal := ((FTileMap.Data[(y * FTileMap.Width) + x].Flags shr 4) and $01);
      SPGBCPAL    : PropVal := GetPalIndexOfLocation(x,y, True);
      SPSGBPAL    : PropVal := GetPalIndexOfLocation(x,y, False);
      SPBGATTR    : PropVal := ((FTileMap.Data[(y * FTileMap.Width) + x].Flags shl 1) and $60) +
                               ((GetTileWithOffset shr 5) and $08) +
                               GetPalIndexOfLocation(x,y, True);

      SP0FILL     : PropVal := 0;
      SP1FILL     : PropVal := -1;
    else            PropVal := FTileMap.PropData[FOptions.Props[i].Nr-SYSPROPCNT][(y * FTileMap.Width) + x];
    end;
    PropSize := FOptions.Props[i].Size;

    (* insert data into storage *)
    PropVal := PropVal and GetMask(PropSize);
    PropVal := PropVal shl b;
    data := data + PropVal;
    b := b + PropSize;
  end;

  (* insert data into planes *)
  for i := 0 to FPlaneCnt-1 do
    FPlanes[i,FrameLoc] := (data shr (i*8)) and $FF;

end;


constructor TMapExport.Create(fn : string);
begin
  inherited Create;

  FFileName := fn;
end;



function DescMapSourceFile: string;
begin
  result := ' Map Source File.';
end;

function DescMapIncludeFile: string;
begin
  result := ' Map Include File.';
end;

function DescGBMB: string;
begin
  result := ' This file was generated by ' + SHORTTITLE + ' v' + GBMBVERSION;
end;

function DescSection(const s : string): string;
begin
  result := '   Section       : ' + Trim(s);
end;

function DescBank( i : integer): string;
begin
  result := '   Bank          : ' + IntToStr(i);
end;

function DescTileSet( const s : string): string;
begin
  result := '   Tile set      : ' + Trim(s);
end;

function DescPlaneCnt( i : integer): string;
const Pln : array[0..4] of string =
  ( '0.5 plane (4 bits)', '1 plane (8 bits)', '2 planes (16 bits)',
    '3 planes (24 bits)', '4 planes (32 bits)' );
begin
  result := '   Plane count   : ' + Pln[i];
end;


function DescPlaneOrder( i : integer): string;
const Pln : array[0..1] of string =
  ( 'Tiles are continues', 'Planes are continues');
begin
  result := '   Plane order   : ' + Pln[i];
end;

function DescMapSize( const TileMap : TGBMapType): string;
begin
  result := '   Map size      : ' + IntToStr(TileMap.Width) + ' x ' + IntToStr(TileMap.Height);
end;

function DescTileOffset( const i : integer): string;
begin
  result := '   Tile offset   : ' + IntToStr(i);
end;

function DescSplit( b : boolean): string;
var s : string;
begin
  if b then s := 'Yes' else s := 'No';
  result := '   Split data    : ' + s;
end;

function DescSplitSize( i : integer): string;
begin
  result := '   Block size    : ' + IntToStr(i);
end;

function DescSplitBank( b : boolean): string;
var s : string;
begin
  if b then s := 'Yes' else s := 'No';
  result := '   Inc bank      : ' + s;
end;



function DescEnd( s : string): string;
begin
 result := ' End of ' + UpperCase(ExtractFileName(s)) ;
end;




procedure ASMWriteHeader(const F : TextFile; const Filename : string ; const Options : TExportSettings; incfile : boolean );
begin
  writeln(F, '; ' + UpperCase(ExtractFileName(FileName)) );
  writeln(F, ';' );

  if not incFile then
    writeln(F, ';' + DescMapSourceFile )
  else
    writeln(F, ';' + DescMapIncludeFile );

  writeln(F, ';' );
  writeln(F, '; Info:' );
  writeln(F, ';' + DescSection(Options.SectionName) );
  writeln(F, ';' + DescBank(Options.Bank) );
  writeln(F, ';' + DescMapSize(TileMap) );
  writeln(F, ';' + DescTileSet(TileFileName) );
  writeln(F, ';' + DescPlaneCnt(Options.PlaneCnt) );
  writeln(F, ';' + DescPlaneOrder(Options.PlaneOrder) );
  writeln(F, ';' + DescTileOffset(Options.TileOffset) );
  writeln(F, ';' + DescSplit(Options.Split) );
  if (Options.Split) then
  begin
    writeln(F, ';' + DescSplitSize(Options.SplitSize) );
    writeln(F, ';' + DescSplitBank(Options.SplitBank) );
  end;
  writeln(F, ';' );
  writeln(F, ';' + DescGBMB );

  writeln(F);
end;


procedure CWriteHeader(const F : TextFile; const Filename : string ; const Options : TExportSettings; incfile : boolean );
begin
  writeln(F, '/*');
  writeln(F);
  writeln(F, ' ' + UpperCase(ExtractFileName(FileName)) );
  writeln(F);

  if not incFile then
    writeln(F, DescMapSourceFile )
  else
    writeln(F, DescMapIncludeFile );

  writeln(F);
  writeln(F, ' Info:' );
  writeln(F, DescSection(Options.SectionName) );
  writeln(F, DescBank(Options.Bank) );
  writeln(F, DescMapSize(TileMap) );
  writeln(F, DescTileSet(TileFileName) );
  writeln(F, DescPlaneCnt(Options.PlaneCnt) );
  writeln(F, DescPlaneOrder(Options.PlaneOrder) );
  writeln(F, DescTileOffset(Options.TileOffset) );
  writeln(F, DescSplit(Options.Split) );
  if (Options.Split) then
    writeln(F, DescSplitSize(Options.SplitSize) );
  writeln(F);
  writeln(F, DescGBMB );
  writeln(F);
  writeln(F,'*/');
end;


(*************************************************)
(*                                               *)
(*              RGBDS ASM output                 *)
(*                                               *)
(*************************************************)


constructor TRGBDSMapExport.Create( fn : string );
begin
  inherited Create(fn);

  AssignFile(F, fn);
  Rewrite(F);
end;


destructor TRGBDSMapExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TRGBDSMapExport.WriteHeader;
begin
  ASMWriteHeader(F, Filename, Options, False );
end;


procedure TRGBDSMapExport.WriteSection( Offset : integer );
var s : string;
begin
  writeln(F);
  if (Options.Split) and (Options.SplitBank) then
    s := Trim(Options.SectionName) + 'BLK' + IntToStr(Offset)
  else
    s := Trim(Options.SectionName);

  if (Options.Bank+Offset = 0) then
    writeln(F, ' SECTION "' + s + '", ROM0' )
  else
    writeln(F, ' SECTION "' + s + '", ROMX, BANK[' + IntToStr(Options.Bank+Offset) + ']' );
end;


procedure TRGBDSMapExport.WritePlaneLabel( i, Block : integer);
var s : string;
begin
  writeln(F);

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  if (i = 0) then
  begin
    writeln(F, s + '::');
    if (Options.PlaneOrder <> 0) then
      writeln(F, s + 'PLN0::');
  end
  else
    writeln(F, s + 'PLN' + IntToStr(i) + '::');
end;


procedure TRGBDSMapExport.WriteLine( cnt : integer; EndOfData : boolean);
var i : integer;
begin
  if ( cnt > 0 ) then
  begin
    write(F, 'DB ' );
    for i := 0 to cnt-1 do
    begin
      write(F, '$'+ IntToHex(DataStore[i],2) );
      if (i < cnt-1) then write(F, ',' );
    end;
    writeln(F);
  end;
end;

procedure TRGBDSMapExport.WriteBlockEnd;
begin
end;

procedure TRGBDSMapExport.WriteEnd;
begin
  writeln(F);
  writeln(F, ';' + DescEnd(FileName) );
end;


procedure TRGBDSMapExport.WriteEQUS;
begin
  writeln(F, Trim(Options.LabelName) + 'Width  EQU ' + IntToStr(TileMap.Width) );
  writeln(F, Trim(Options.LabelName) + 'Height EQU ' + IntToStr(TileMap.Height) );
  writeln(F, Trim(Options.LabelName) + 'Bank   EQU ' + IntToStr(Options.Bank) );
end;


(*************************************************)
(*                                               *)
(*          RGBDS ASM Include output             *)
(*                                               *)
(*************************************************)


procedure TRGBDSIncMapExport.WriteHeader;
begin
  ASMWriteHeader(F, Filename, Options, True );
end;


procedure TRGBDSIncMapExport.WriteSection( Offset : integer );
begin

end;


procedure TRGBDSIncMapExport.WritePlaneLabel( i, Block : integer);
var s : string;
begin

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  if (i = 0) then
  begin
    writeln(F);
    writeln(F, ' GLOBAL ' + s);
    if (Options.PlaneOrder <> 0) then
      writeln(F, ' GLOBAL ' + s + 'PLN0');
  end
  else
    writeln(F, ' GLOBAL ' + s + 'PLN' + IntToStr(i));
end;

procedure TRGBDSIncMapExport.WriteBlockEnd;
begin
end;



(*************************************************)
(*                                               *)
(*              RGBDS Obj output                 *)
(*                                               *)
(*************************************************)

type TGBTDRGBDSLabelType = record
  Name : ShortString;
  Location : integer;
end;

constructor TRGBDSObjMapExport.Create( fn : string );
begin
  inherited Create(fn);

  FStrm := TFileStream.Create(fn, fmCreate);
  Lst := TList.Create;
  Pos := 0;
end;


destructor TRGBDSObjMapExport.Destroy;
var p : ^TGBTDRGBDSLabelType;
    i : integer;
begin
  FreeMem( DataBuf );

  (* remove labels *)
  for i :=0 to (Lst.Count-1) do
  begin
    p := Lst.Items[0];
    Lst.Delete(0);
    FreeMem(p, SizeOf( TGBTDRGBDSLabelType));
  end;
  Lst.Free;

  FStrm.Free;

  inherited;
end;


procedure TRGBDSObjMapExport.WriteHeader;
begin
end;


procedure TRGBDSObjMapExport.WriteSection( Offset : integer );
begin
end;


procedure TRGBDSObjMapExport.WritePlaneLabel( i, Block : integer);
var p : ^TGBTDRGBDSLabelType;
var s : string;
begin

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  (* create new label *)
  GetMem(p, SizeOf( TGBTDRGBDSLabelType));
  Lst.Add(p);

  (* fill it *)
  if (i = 0) then
  begin
    p^.Name := s;
    p^.Location := pos;
    if (Options.PlaneOrder <> 0) then
    begin
      GetMem(p, SizeOf( TGBTDRGBDSLabelType));
      Lst.Add(p);
      p^.Name := s + 'PLN0';
      p^.Location := pos;
    end
  end
  else
  begin
    p^.Name := s + 'PLN' + IntToStr(i);
    p^.Location := pos;
  end;
end;


procedure TRGBDSObjMapExport.WriteLine( cnt : integer; EndOfData : boolean);
var i : integer;
begin
  if not Assigned(Databuf) then GetMem( DataBuf, (4 * FTileMap.Width * FTileMap.Height) );

  (* pump over to internal buffer *)
  for i := 0 to (cnt-1) do
    DataBuf[Pos + i] := DataStore[i];

  (* save for later *)
  Pos := Pos + cnt;
end;

procedure TRGBDSObjMapExport.WriteBlockEnd;
begin
end;

procedure TRGBDSObjMapExport.WriteEnd;
var i : integer;
var p : ^TGBTDRGBDSLabelType;
var s : array[0..20] of char;
var l : LongInt;
begin

  (* write header *)
  FStrm.Write('RGB1', 4);

  FStrm.Write(Lst.Count, 4);                  (* Label-count *)
  l := 1;
  FStrm.Write(l, 4);                          (* Section-count *)

  (* labels *)
  for i := 0 to (Lst.Count-1) do
  begin
    p := Lst.Items[i];
    StrPcopy(s, p^.Name);
    FStrm.Write(s, StrLen(s)+1);              (* labelname *)
    l := 2;
    FStrm.Write(l, 1);                        (* EXPORT *)
    l := 0;
    FStrm.Write(l, 4);                        (* Section *)
    FStrm.Write(p^.Location, 4);              (* Location *)
  end;


  (* Section *)
  FStrm.Write(Pos, 4);                        (* Section size *)
  if (Options.Bank = 0) then
    l := 3
  else
    l := 2;
  FStrm.Write(l, 1);                          (* HOME/CODE *)
  l := -1;
  FStrm.Write(l, 4);                          (* Org *)
  if (Options.Bank = 0) then
    i := -1
  else
    i := Options.Bank;
  FStrm.Write(i,4);                           (* Bank *)


  (* Export data *)
  FStrm.Write(DataBuf^, Pos);

  (* EOF ? *)
  l := 0;
  FStrm.Write(l, 4);
end;


procedure TRGBDSObjMapExport.WriteEQUS;
begin
end;



(*************************************************)
(*                                               *)
(*               TASM ASM output                 *)
(*                                               *)
(*************************************************)


constructor TASMMapExport.Create( fn : string );
begin
  inherited Create(fn);

  AssignFile(F, fn);
  Rewrite(F);
end;


destructor TASMMapExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TASMMapExport.WriteHeader;
begin
  ASMWriteHeader(F, Filename, Options, False );
end;


procedure TASMMapExport.WriteSection( Offset : integer );
begin
end;


procedure TASMMapExport.WritePlaneLabel( i, Block : integer);
var s : string;
begin

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  writeln(F);
  if (i = 0) then
  begin
    writeln(F, s + ':');
    if (Options.PlaneOrder <> 0) then
      writeln(F, s + 'PLN0:');
  end
  else
    writeln(F, s + 'PLN' + IntToStr(i) + ':');
end;


procedure TASMMapExport.WriteLine( cnt : integer; EndOfData : boolean);
var i : integer;
begin
  if ( cnt > 0 ) then
  begin
    write(F, '.byte ' );
    for i := 0 to cnt-1 do
    begin
      write(F, '$'+ IntToHex(DataStore[i],2) );
      if (i < cnt-1) then write(F, ',' );
    end;
    writeln(F);
  end;
end;

procedure TASMMapExport.WriteBlockEnd;
begin
end;

procedure TASMMapExport.WriteEnd;
begin
  writeln(F);
  writeln(F, '.end');
  writeln(F);
  writeln(F, ';' + DescEnd(FileName) );
end;


procedure TASMMapExport.WriteEQUS;
begin
  writeln(F, Trim(Options.LabelName) + 'Width  .equ ' + IntToStr(TileMap.Width) );
  writeln(F, Trim(Options.LabelName) + 'Height .equ ' + IntToStr(TileMap.Height) );
  writeln(F, Trim(Options.LabelName) + 'Bank   .equ ' + IntToStr(Options.Bank) );
end;


(*************************************************)
(*                                               *)
(*                 GBDK C output                 *)
(*                                               *)
(*************************************************)


constructor TGBDKMapExport.Create( fn : string );
begin
  inherited Create(fn);

  AssignFile(F, fn);
  Rewrite(F);
end;


destructor TGBDKMapExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TGBDKMapExport.WriteHeader;
begin
  if (Options.Bank <> 0) then begin
    writeln(F, '#pragma bank ' + IntToStr(Options.Bank) );
    writeln(F);
  end;
  CWriteHeader(F, Filename, Options, False );
end;


procedure TGBDKMapExport.WriteSection( Offset : integer );
begin
end;


procedure TGBDKMapExport.WritePlaneLabel( i, Block : integer);
var s : string;
begin

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  if (i = 0) then
  begin
    writeln(F);
    if (Options.PlaneOrder = 0) then
    begin
      writeln(F, 'const unsigned char ' + s + '[] =');
    end
    else
    begin
      writeln(F, '#define ' + s + ' ' + s + 'PLN0' );
      writeln(F, 'const unsigned char ' + s + 'PLN0[] =');
    end
  end
  else
  begin
    WriteBlockEnd;
    writeln(F, 'const unsigned char ' + s + 'PLN' + IntToStr(i) + '[] =');
  end;
  writeln(F, '{');
end;


procedure TGBDKMapExport.WriteLine( cnt : integer; EndOfData : boolean);
var i : integer;
begin
  if ( cnt > 0 ) then
  begin
    write(F, '  ' );
    for i := 0 to cnt-1 do
    begin
      write(F, '0x'+ IntToHex(DataStore[i],2) );
      if (i < cnt-1) then write(F, ',' );
    end;
    if EndOfData then
      writeln(F)
    else
      writeln(F, ',');
  end;
end;

procedure TGBDKMapExport.WriteBlockEnd;
begin
  writeln(F, '};');
  writeln(F);
end;

procedure TGBDKMapExport.WriteEnd;
begin
  writeln(F, '/*' + DescEnd(FileName) + ' */' );
end;


procedure TGBDKMapExport.WriteEQUS;
begin
  writeln(F);
  writeln(F, '#define ' + Trim(Options.LabelName) + 'Width ' + IntToStr(TileMap.Width) );
  writeln(F, '#define ' + Trim(Options.LabelName) + 'Height ' + IntToStr(TileMap.Height) );
  writeln(F, '#define ' + Trim(Options.LabelName) + 'Bank ' + IntToStr(Options.Bank) );
end;

(*************************************************)
(*                                               *)
(*               GBDK header output              *)
(*                                               *)
(*************************************************)


constructor TGBDKIncMapExport.Create( fn : string );
begin
  inherited Create(fn);

  AssignFile(F, fn);
  Rewrite(F);
end;


destructor TGBDKIncMapExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TGBDKIncMapExport.WriteHeader;
  function replacechr(const src: string): string;
  var i : integer;
  begin
    result:= src;
    for i:= 1 to length(result) do
      if not (result[i] in ['0'..'9', 'A'..'Z', 'a'..'z']) then result[i]:= '_';
  end;
begin
  CWriteHeader(F, Filename, Options, True );

  IncludeName:= format('__%s_INCLUDE', [replacechr(ExtractFileName(Filename))]);
  writeln(F, '#ifndef ' + IncludeName);
  writeln(F, '#define ' + IncludeName);
end;


procedure TGBDKIncMapExport.WriteSection( Offset : integer );
begin
end;


procedure TGBDKIncMapExport.WritePlaneLabel( i, Block : integer);
var s : string;
begin

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  if (i = 0) then
  begin
    if (Options.PlaneOrder = 0) then
    begin
      writeln(F, 'extern const unsigned char ' + s + '[];');
    end
    else
    begin
      writeln(F, '#define ' + s + ' ' + s + 'PLN0' );
      writeln(F, 'extern const unsigned char ' + s + 'PLN0[];');
    end
  end
  else
  begin
    writeln(F, 'extern const unsigned char ' + s + 'PLN' + IntToStr(i) + '[];');
  end;
end;


procedure TGBDKIncMapExport.WriteLine( cnt : integer; EndOfData : boolean);
begin
end;

procedure TGBDKIncMapExport.WriteBlockEnd;
begin
end;

procedure TGBDKIncMapExport.WriteEnd;
begin
  writeln(F);
  writeln(F, '#endif');
  writeln(F);
  writeln(F, '/*' + DescEnd(FileName) + ' */' );
end;


procedure TGBDKIncMapExport.WriteEQUS;
begin
  writeln(F);
  writeln(F, '#define ' + Trim(Options.LabelName) + 'Width ' + IntToStr(TileMap.Width) );
  writeln(F, '#define ' + Trim(Options.LabelName) + 'Height ' + IntToStr(TileMap.Height) );
  writeln(F, '#define ' + Trim(Options.LabelName) + 'Bank ' + IntToStr(Options.Bank) );
  writeln(F);
end;



(*************************************************)
(*                                               *)
(*                Binary output                  *)
(*                                               *)
(*************************************************)

constructor TBinMapExport.Create( fn : string );
begin
  inherited Create(fn);

  FStrm := TFileStream.Create(fn, fmCreate);
end;


destructor TBinMapExport.Destroy;
begin
  FStrm.Free;

  inherited;
end;


procedure TBinMapExport.WriteHeader;
begin
end;


procedure TBinMapExport.WriteSection( Offset : integer );
begin
end;


procedure TBinMapExport.WritePlaneLabel( i, Block : integer);
begin
end;


procedure TBinMapExport.WriteLine( cnt : integer; EndOfData : boolean);
begin
  FStrm.Write(DataStore, cnt);
end;

procedure TBinMapExport.WriteBlockEnd;
begin
end;

procedure TBinMapExport.WriteEnd;
begin
end;


procedure TBinMapExport.WriteEQUS;
begin
end;



(*************************************************)
(*                                               *)
(*               ISAS ASM output                 *)
(*                                               *)
(*************************************************)


constructor TISASMapExport.Create( fn : string );
begin
  inherited Create(fn);

  AssignFile(F, fn);
  Rewrite(F);
end;


destructor TISASMapExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TISASMapExport.WriteHeader;
begin
  ASMWriteHeader(F, Filename, Options, False );
end;


procedure TISASMapExport.WriteSection( Offset : integer );
begin
  writeln(F);
  if (Options.Split) and (Options.SplitBank) then
    writeln(F, Trim(Options.SectionName) + 'BLK ' + IntToStr(Offset) + ' GROUP ' + IntToStr(Options.Bank+Offset) )
  else
    writeln(F, Trim(Options.SectionName) + ' GROUP ' + IntToStr(Options.Bank+Offset) );
end;


procedure TISASMapExport.WritePlaneLabel( i, Block : integer);
var s : string;

  procedure SendToFile( const s :string);
  begin
    writeln(F);
    writeln(F, ' PUBLIC ' + s );
    writeln(F, s );
  end;

begin
  writeln(F);

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  if (i = 0) then
  begin
    SendToFile(s);
    if (Options.PlaneOrder <> 0) then
      SendToFile(s + 'PLN0');
  end
  else
    SendToFile(s + 'PLN' + IntToStr(i));
end;


procedure TISASMapExport.WriteLine( cnt : integer; EndOfData : boolean);
var i : integer;
begin
  if ( cnt > 0 ) then
  begin
    write(F, ' DB ' );
    for i := 0 to cnt-1 do
    begin
      write(F, '0'+ IntToHex(DataStore[i],2) + 'h' );
      if (i < cnt-1) then write(F, ',' );
    end;
    writeln(F);
  end;
end;

procedure TISASMapExport.WriteBlockEnd;
begin
end;

procedure TISASMapExport.WriteEnd;
begin
  writeln(F);
  writeln(F, ';' + DescEnd(FileName) );
end;


procedure TISASMapExport.WriteEQUS;
begin
  writeln(F, Trim(Options.LabelName) + 'Width  EQU ' + IntToStr(TileMap.Width) );
  writeln(F, Trim(Options.LabelName) + 'Height EQU ' + IntToStr(TileMap.Height) );
  writeln(F, Trim(Options.LabelName) + 'Bank   EQU ' + IntToStr(Options.Bank) );
end;




(*************************************************)
(*                                               *)
(*           ISAS ASM Include output             *)
(*                                               *)
(*************************************************)


procedure TISASIncMapExport.WriteHeader;
begin
  ASMWriteHeader(F, Filename, Options, True );
end;


procedure TISASIncMapExport.WriteSection( Offset : integer );
begin
end;


procedure TISASIncMapExport.WriteBlockEnd;
begin
end;


procedure TISASIncMapExport.WritePlaneLabel( i, Block : integer);
var s : string;
begin

  if (not Options.Split) then
    s := Trim(Options.Labelname)
  else
    s := Trim(Options.Labelname) + 'BLK' + IntToStr(Block);

  if (i = 0) then
  begin
    writeln(F);
    writeln(F, ' EXTERN ' + s);
    if (Options.PlaneOrder <> 0) then
      writeln(F, ' EXTERN ' + s + 'PLN0');
  end
  else
    writeln(F, ' EXTERN ' + s + 'PLN' + IntToStr(i));
end;

end.

