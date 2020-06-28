unit Repository;

interface

uses Classes, Controls, GBConst, StdCtrls, SysUtils, IniFiles, gbofile,
     MapUndo, GBMBMain, ProGboFile, GBTileExchanger;

const MAXMAPWIDTH = 1023;
      MAXMAPHEIGHT = 1023;
      EXPORTPROPMAX = 31;
      LONGTITLE = 'Gameboy Map Builder';
      SHORTTITLE = 'GBMB';
      GBMBVERSION = '1.8';

      (* System properties *)
      SYSPROPCNT  = 11;
      SPNONE      = 0;
      SPTILENR    = 1;
      SPTILENR8   = 2;
      SPTILENR9   = 3;
      SPVERTFLIP  = 4;
      SPHORFLIP   = 5;
      SPGBCPAL    = 6;
      SPSGBPAL    = 7;
      SPBGATTR    = 8;
      SP0FILL     = 9;
      SP1FILL     = 10;



type
  TExportSettings = class
  private
    FGBOMapExport : TProGBOMapExport;
    FProps        : TList;

    function GetFileName: string;
    procedure SetFileName( s : string );
    function GetSectionName: string;
    procedure SetSectionName( s : string );
    function GetLabelName: string;
    procedure SetLabelName( s : string );
    function GetProps(index : integer): RProGBOMapExportPropDefRec;
    function GetPropCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearProps;
    procedure NewProp( Nr : byte; Size   : integer);

    procedure Load(const F : TProGBOFile; const ID, PropID, MasterID : word );
    procedure Save(const F : TProGBOFile; const ID, PropID, MasterID : word );

    Property FileName    : string read GetFileName write SetFileName;
    Property SectionName : string read GetSectionName write SetSectionName;
    Property LabelName   : string read GetLabelName write SetLabelName;
    Property FileType    : byte read FGBOMapExport.FileType write FGBOMapExport.FileType;
    Property Bank        : Byte read FGBOMapExport.Bank write FGBOMapExport.Bank;
    Property PlaneCnt    : Word read FGBOMapExport.PlaneCnt write FGBOMapExport.PlaneCnt;
    Property PlaneOrder  : Word read FGBOMapExport.PlaneOrder write FGBOMapExport.PlaneOrder;
    Property MapLayout   : Word read FGBOMapExport.MapLayout write FGBOMapExport.MapLayout;
    Property Split       : boolean read FGBOMapExport.Split write FGBOMapExport.Split;
    Property SplitSize   : integer read FGBOMapExport.SplitSize write FGBOMapExport.SplitSize;
    Property SplitBank   : boolean read FGBOMapExport.SplitBank write FGBOMapExport.SplitBank;
    Property SelTab      : Byte read FGBOMapExport.SelTab write FGBOMapExport.SelTab;
    Property PropCount   : integer read GetPropCount;
    Property TileOffset  : word read FGBOMapExport.TileOffset write FGBOMapExport.TileOffset;
    Property Props[Index : integer] : RProGBOMapExportPropDefRec read GetProps;
end;





type
  RTilePropRec = record
    pType  : byte;             // Type of property
    Name   : String[20];
    Size   : integer;
  end;

  RTileProp = record
    Cnt      : integer;        // Total amount of properties (used/unused)
    Prop     : Array [0..PROPCNT-1] of RTilePropRec;
  end;

  RDefaultTileProp = array [0..PROPCNT-1] of word;

type RInfoPanelRec = record
  Ctrl : TWinControl;
  Name : string[20];
  Size : integer;
end;


type RBlockFillRec = record
  Pattern : integer;
  Width : integer;
  Height : integer;
end;


procedure InitRepository;


var
  TileProp     : RTileProp;
  TileList     : TList;
  TileMap      : TGBMapType;
  PropEd       : array[0..PROPCNT-1] of RInfoPanelRec;
  FileName     : TFileName;
  TileFileName : TFilename;
  BaseDir      : TFilename; //array[0..255] of char;
  IniFile      : TIniFile;
  Modified     : boolean;
  MapUndoCtrl  : TMapUndoCtrl;
  MapSettings  : TProGBOMapSettings;
  PropColors   : TProGBOPropColors;
  ExportSettings : TExportSettings;

  TileExchanger : TGBTileExchanger;

  MapMinWidth   : integer;

implementation




procedure InitTileProp;
var i : integer;
begin

  with TileProp do
  begin
    Cnt := PROPCNT;
    for i := 0 to PROPCNT-1 do
    begin
      Prop[i].pType := 0;
      Prop[i].Name  := '';
      Prop[i].Size  := 0;
    end;
  end;

end;


procedure InitRepository;
begin
  MapUndoCtrl := TMapUndoCtrl.Create;
  InitTileProp;
end;



constructor TExportSettings.Create;
begin
  inherited Create;

  FProps := TList.Create;
end;

destructor TExportSettings.Destroy;
begin
  ClearProps;
  FProps.Free;

  inherited Destroy;
end;

procedure TExportSettings.ClearProps;
var i : integer;
begin
  for i:= 0 to FProps.Count-1 do
  begin
    FreeMem(FProps.items[0]);
    FProps.Delete(0);
  end;
end;

procedure TExportSettings.NewProp( Nr : byte; Size : integer);
var r : ^RProGBOMapExportPropDefRec;
begin
  GetMem(r, SizeOf(RProGBOMapExportPropDefRec));
  r.Nr := Nr;
  r.Size := Size;
  FProps.Add(r);
end;

procedure TExportSettings.Load(const F : TProGBOFile; const ID, PropID, MasterID : word );
type
  PropRecArr = array[0..0] of RProGBOMapExportPropDefRec;
  PPropRecArr = ^PropRecArr;

var
  p : PPropRecArr;
  i,Size : integer;
  r : ^RProGBOMapExportPropDefRec;

begin
  ClearProps;

  (* load main info *)
  if f.LoadFrom( ProGBOMapExportID, ID, MasterID, SizeOf(TProGBOMapExport), @FGBOMapExport) then
  begin
    (* load sub objects *)
    if (FGBOMapExport.PropCnt > EXPORTPROPMAX+1) then FGBOMapExport.PropCnt := EXPORTPROPMAX+1;
    Size := SizeOf(RProGBOMapExportPropDefRec) * FGBOMapExport.PropCnt;
    GetMem(p, Size);
    try
      if f.LoadFrom( ProGBOMapExportPropID, PropID, ID, Size, p) then
        for i := 0 to FGBOMapExport.PropCnt-1 do
        begin
          GetMem(r, SizeOf(RProGBOMapExportPropDefRec));
          r^ := p[i];
          FProps.Add(r);
        end;
    finally
      FreeMem(p);
    end;
  end;
end;


procedure TExportSettings.Save(const F : TProGBOFile; const ID, PropID, MasterID : word );
type
  PropRecArr = array[0..0] of RProGBOMapExportPropDefRec;
  PPropRecArr = ^PropRecArr;

var
  p : PPropRecArr;
  i,Size : integer;
begin
  FGBOMapExport.PropCnt := FProps.Count;

  (* save main info *)
  f.SaveTo( ProGBOMapExportID, ID, MasterID, SizeOf(TProGBOMapExport), @FGBOMapExport);

  (* Save sub objects *)
  Size := SizeOf(RProGBOMapExportPropDefRec) * FGBOMapExport.PropCnt;
  GetMem(p, Size);
  try
    for i := 0 to FGBOMapExport.PropCnt-1 do
      p[i] := Props[i];

    f.SaveTo( ProGBOMapExportPropID, PropID, ID, Size, p);
  finally
    FreeMem(p);
  end;
end;

function TExportSettings.GetFileName: string;
begin
  Result := FGBOMapExport.FileName;
end;

procedure TExportSettings.SetFileName( s : string );
begin
  StrLCopy(FGBOMapExport.FileName, PChar(s), 254);
end;

function TExportSettings.GetSectionName: string;
begin
  Result := FGBOMapExport.SectionName;
end;

procedure TExportSettings.SetSectionName( s : string );
begin
  StrLCopy(FGBOMapExport.SectionName, PChar(s), 39);
end;

function TExportSettings.GetLabelName: string;
begin
  Result := FGBOMapExport.LabelName;
end;

procedure TExportSettings.SetLabelName( s : string );
begin
  StrLCopy(FGBOMapExport.LabelName, PChar(s), 39);
end;

function TExportSettings.GetProps(index : integer): RProGBOMapExportPropDefRec;
begin
  result := RProGBOMapExportPropDefRec(FProps.Items[index]^);
end;

function TExportSettings.GetPropCount: integer;
begin
  result := FProps.Count;
end;

end.
