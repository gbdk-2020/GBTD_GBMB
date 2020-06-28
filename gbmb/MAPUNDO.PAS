unit MapUndo;

interface

uses Classes, gbConst, MainLib ;

type
  GBUndoMapType = record
    Width  : integer;
    Height  : integer;
//    Data   : array[0..0] of RMapRec;
    Data   : array[0..0] of byte;
  end;
  PGBUndoMapType = ^GBUndoMapType;

type
  TMapUndoCtrl = class(TObject)
  private
    { Private declarations }
    FLst  : TList;
    FMode : TGBUpdateMode;
//    FLocked : boolean;
    FBufP  : integer;
    FMapBufP : PGBUndoMapType;
    FMaxBuf : integer;

    FRecSize : integer;


    Function GetUndoAvailable: boolean;
    procedure AddPages(min : integer);
    procedure KillPages;
    procedure SetMode(mode : TGBUpdateMode);
    procedure ShiftMainPage;
  protected
    { Protected declarations }



  public
    { Public declarations }

    constructor Create;
    destructor Destroy; override;

    property UnDoAvailable : boolean read GetUndoAvailable;
    property Mode : TGBUpdateMode read FMode write SetMode;

    procedure Undo;
    procedure AddUndo(ix,iy : integer; const Tile : RMapRec);
    procedure Clear;

  end;

implementation

uses Repository, gboFile, gbmbMain;

const SLOTCNT = 32;

type
  RUndoRec = packed record
    x,y    : byte;
    Data   : RMapRec;
    Props  : array[0..0] of word;
  end;
  PUndoRec = ^RUndoRec;




  RUndoPage = record
//    Slot : array[0..SLOTCNT-1] of RUndoRec;
     Slot : array[0..0] of byte;
  end;
  PUndoPage = ^RUndoPage;


constructor TMapUndoCtrl.Create;
begin
  inherited Create;

  FLst := TList.Create;
  FBufP := 0;
  FMode := gbumPixel;
  FMaxBuf := 0;

end;


destructor TMapUndoCtrl.Destroy;
begin
  Clear;
  FLst.Free;

  inherited Destroy;
end;


Function TMapUndoCtrl.GetUndoAvailable: boolean;
begin
  Result := (FBufp > 0) or Assigned(FMapBufP);
end;


procedure TMapUndoCtrl.AddUndo(ix,iy : integer; const Tile : RMapRec);
var i,j,k : integer;
var p : PUndoPage;
    r : PUndoRec;
    w : word;
begin
  if (Mode = gbumMap) and (not Assigned(FMapBufP)) then
  begin
    GetMem(FMapBufP, SizeOf(GBUndoMapType) + (TileMap.Width * TileMap.Height * FRecSize) );
    with GBUndoMapType(FMapBufP^) do
    begin
      Width := TileMap.Width;
      Height := TileMap.Height;

      for i := 0 to Height-1 do
        for j := 0 to Width-1 do
          with PUndoRec(@Data[((i * Width) + j) * FRecSize])^ do
          begin
            Data := TileMap.Data[(i * TileMap.Width) + j];
            for k := 0 to TileMap.PropCount-1 do
              Props[k] := TileMap.PropData[k][(i * TileMap.Width) + j];
          end;

    end
  end
  else
  begin
    if (FMode = gbumPixel) and (FBufP >= SLOTCNT) then
      ShiftMainPage
    else
      if (FBufP >= FMaxBuf) then AddPages(FBufP);

    p := FLst.Items[(FbufP div SLOTCNT)];
    r := PUndoRec(@p.Slot[(FBufP mod SLOTCNT) * FRecSize]);

    with r^ do
    begin
      x := ix;
      y := iy;
      Data := Tile;
      for j := 0 to TileMap.PropCount-1 do
        Props[j] := TileMap.PropData[j][(iy * TileMap.Width) + ix];
    end;

    Inc(FBufP);
  end;
end;


procedure TMapUndoCtrl.AddPages(min : integer);
var i,j : integer;
var p : PUndoPage;
begin
  j := (min div SLOTCNT) - FLst.Count;

  for i := 0 to j do
  begin
    GetMem(p, FRecSize * SLOTCNT);
    FLst.Add(p);
  end;
  FMaxBuf := FLst.Count * SLOTCNT;
end;

procedure TMapUndoCtrl.KillPages;
var i : integer;
var p : PUndoPage;
begin
  for i := 0 to FLst.Count-1 do
  begin
    p := FLst.Items[0];
    FreeMem(p, FRecSize * SLOTCNT);
    Flst.Delete(0);
  end;

  FMaxBuf := 0;
end;


procedure TMapUndoCtrl.SetMode(mode : TGBUpdateMode);
begin
  if (Mode = gbumTotal) or (Mode <> FMode) then
  begin
    Fmode := Mode;
    Clear;
  end;
end;


procedure TMapUndoCtrl.Undo;
var i,j,k : integer;
    r : PUndoRec;
    p : PUndoPage;
begin
  if (Fmode = gbumMap) then
  begin
    TileMap.Width := FMapBufP.Width;
    TileMap.Height := FMapBufP.Height;

    for i := 0 to FMapBufP.Height-1 do
      for j := 0 to FMapBufP.Width-1 do
        with PUndoRec(@FMapBufP.Data[((i * TileMap.Width) + j) * FRecSize])^ do
        begin
          TileMap.Data[(i * TileMap.Width) + j] := Data;
          for k := 0 to TileMap.PropCount-1 do
            TileMap.PropData[k][(i * TileMap.Width) + j] := Props[k];
        end;
    RefreshMap;
  end
  else
    for i := FBufP-1 downto 0 do
    begin
      p := FLst.Items[(i div SLOTCNT)];
      r := PUndoRec(@p.Slot[(i mod SLOTCNT) * FRecSize]);
      with r^ do
      begin
        TileMap.Data[(y * TileMap.Width) + x] := Data;
        for j := 0 to TileMap.PropCount-1 do
          TileMap.PropData[j][(y * TileMap.Width) + x] := Props[j];
      end;
    end;

  Clear;
end;

procedure TMapUndoCtrl.Clear;
begin
  FreeMem(FMapBufP);
  FMapBufP := nil;
  KillPages;
  FBufP := 0;

  FRecSize := SizeOf(RUndoRec) + (TileMap.PropCount * SizeOf(word));
end;

procedure TMapUndoCtrl.ShiftMainPage;
var i,j : integer;
    p : PUndoPage;
begin
  p := FLst.Items[0];
  for i := 0 to SLOTCNT-2 do
    CopyBuf(@p.Slot[(i+1) * FRecSize], @p.Slot[i * FRecSize], FRecSize);

  FMaxBuf := SLOTCNT-1;
  FBufP := SLOTCNT-1;
end;




end.
