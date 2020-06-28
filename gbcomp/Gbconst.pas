unit gbconst;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, MainLib;

const
  PROPCNT = 31;
//  MAXTILECNT = 512;
  MAXTILECNT = 768;

type
  TGBCompColor       = Word;
  PGBCompColor       = ^TGBCompColor;
  TGBCompColorArray  = packed array [0..0] of TGBCompColor;
  PGBCompColorArray  = ^TGBCompColorArray;

  TGBPaletteType = array [0..3] of Byte;
  PGBPaletteType = ^TGBPaletteType;

  TGBColorType    = array [0..3] of TColor;
  PGBColorType    = ^TGBColorType;

  TGBColorSets    = array[0..7] of TGBColorType;
  PGBColorSets    = ^TGBColorSets;

  TGBPropColorSets = array[0..2] of TGBColorSets;
  PGBPropColorSets = ^TGBPropColorSets;

  ColSetArray = array [0..1] of byte;

  TileType = packed record
    data     : array[0..31,0..31] of byte;
    ColorSet : ColSetArray;   //integer;
  end;
  PTileType = ^TileType;


  RMapRec = packed record
    Tile     : word;
    PropCol  : Byte;
    Flags    : Byte;
    Palette  : word;
  end;

  (* Flags:                                 *)
  (*    6-7   SGB Palette  (not used)  FREE *)
  (*      5   Vert Flip                     *)
  (*      4   Hor Flip                      *)
  (*    3-0   GBC Palette  (not used)  FREE *)

  (* Palette:                               *)
  (*    8-10  SGB palette  (0=def,1-4=#)    *)
  (*    0-4   GBC palette  (0=def,1-9=#)    *)


  GBMapData = packed array[0..0] of RMapRec;
  PGBMapData = ^GBMapData;

  GBMapPropData = packed array[0..0] of word;
  PGBMapPropData = ^GBMapPropData;


  RMapPropDef = record
    Loc    : integer;
    pType  : byte;             // Type of property
    Name   : String[20];
    Size   : integer;
  end;
  PMapPropDef = ^RMapPropDef;


  GBMapPropDef = array[0..PROPCNT-1] of RMapPropDef;

  GBMapPropInitVal = packed array[0..0] of word;
  PGBMapPropInitVal = ^GBMapPropInitVal;
  AGBMapPropInitVal = array[0..PROPCNT-1] of PGBMapPropInitVal;

 TGBMapType = class
    private
      FWidth   : integer;
      FHeight  : integer;
      FData    : PGBMapData;
      FTileCount : integer;


      FPropCount : integer;
      FPropData  : array[0..PROPCNT-1] of PGBMapPropData;
      FPropDef   : GBMapPropDef;
      FPropInitVal : AGBMapPropInitVal;

      procedure SetWidth( i : integer);
      procedure SetHeight( i : integer);

      function GetPropData(Indx : integer): PGBMapPropData;
      function GetPropDef(Indx : integer): RMapPropDef;
      function GetPropInitVal(Indx : integer): PGBMapPropInitVal;
      procedure ResizeInitVal( Indx : integer; cnt : integer; Preserve : boolean);

    public
      property Width  : integer read FWidth write SetWidth;
      property Height : integer read FHeight write SetHeight;
      property Data   : PGBMapData read FData;

      property PropCount : integer read FPropCount;
      property PropData[Indx : integer] : PGBMapPropData read GetPropData;
      property PropDef[Indx : integer] : RMapPropDef read GetPropDef;
      property PropInitVal[Indx : integer] : PGBMapPropInitVal read GetPropInitVal;

      procedure InsertPropDef( indx : integer; const def : RMapPropDef );
      procedure DeletePropDef( indx : integer);


      procedure SetPropDefLoc(Loc : integer; const Def : RMapPropDef);

      procedure SetBounds( w, h : integer; PreserveData : boolean);
      procedure SetTileCount( cnt : integer);

      destructor destroy; override;
      constructor Create;
    end;



{  GBMapType = record
    Width  : integer;
    Height  : integer;
    Data   : array[0..255,0..255] of RMapRec;
  end;
  PGBMapType = ^GBMapType;
}
type
  TGBTileSize  = (gbts8x8, gbts8x16, gbts16x16, gbts32x32);
  TSplitOrder  = (soLeftRightTopBottom, soTopBottomLeftRight);
  TGBPenStyle  = (gbdsPen, gbdsFloodFill,gbdsDropper);
  TGBColorMode = (gbcmPocket, gbcmGameBoy, gbcmGBC, gbcmSGB, gbcmGBCFiltered );

type
  TGBUpdateMode = (gbumTotal, gbumPixel, gbumMap);

type
{  TDrawPixelEvent = procedure(x,y : integer; var p : integer) of object;  }
  TPixelClickEvent = procedure(x,y :integer; Button : TMouseButton; var Update : TGBUpdateMode) of object;
  TAfterPaintEvent = procedure(x,y :integer; Update : TGBUpdateMode) of object;


function IntToColor(i : integer; p : TGBPaletteType;  c : PGBColorType): TColor;
function ColorToInt(i : TColor; p : TGBPaletteType; c : TGBColorType): Integer;
procedure DetermineTileSize(i : TGBTileSize; var x,y : integer);
function GetTileSize( x,y : integer): TGBTileSize;
procedure ConvertTileSize(InS, OutS : TGBTileSize; Lst : TList);
function NormalizeClipColor(cIn : TColor): TColor;
{procedure GetSelColorSet( const Base : TGBColorType; var Sel : TGBColorType );
function GetSelColor( c : TColor ): TColor;}
function FlipColor( i : TColor ): TColor;

const GBTDVersion : string = '2.2';
const GBTDTitle : string = 'Gameboy Tile Designer';

implementation
(*
function GetSelColor( c : TColor ): TColor;
var b : byte;
begin
  b := (c and $FF0000) shr 16;
  if (b < ($FF - $20)) then b := b + $20 else b := $FF;
  Result := (b shl 16);

  b := (c and $FF00) shr 8;
  if (b > $20) then b := 0 else b := (b and $20);
  Result := Result + (b shl 8);

  b := (c and $FF);
  if (b > $20) then b := 0 else b := (b and $20);
  Result := Result + b;

{
  if ((c and $FF) < ($FF-$7F)) then
    Result := c + $7F
  else
    if ((c and $FFFF00) > ($808000)) then
      Result := c - ($808000)
    else
      Result := (c and $FF);
}
end;
*)
function FlipColor( i : TColor ): TColor;
begin
  Result := ((i and $0000FF) shl 16) + (i and $00FF00) + ((i and $FF0000) shr 16);
end;
{
procedure GetSelColorSet( const Base : TGBColorType; var Sel : TGBColorType );
var i : integer;
begin
  for i := 0 to 3 do
    Sel[i] := GetSelColor(Base[i]);
end;
}

function NormalizeClipColor(cIn : TColor): TColor;
var i : integer;
begin
  i := ((integer(cIn) and $F0) SHR 4);
  case i of
    $0 : Result := clBlack;
    $8 : Result := clDkGray;
    $C : Result := clLtGray;
    else Result := clWhite;
  end;
end;


procedure ConvertTileSize(InS, OutS : TGBTileSize; Lst : TList);

var Buf   : TList;
var p,q   : PTileType;
var i,j,k,l : integer;
var xd, yd : integer;
var col : array[0..MAXTILECNT-1] of ColSetArray;
begin

  Buf := TList.Create;
  for i:= 0 to MAXTILECNT-1 do
  begin
    col[i,0] := 0;
    col[i,1] := 0;
  end;

  (* Stap #1: van InS naar gbts8x8 *)

  case InS of
    gbts8x8:
      for k := 0 to Lst.Count-1 do
      begin
        p := Lst.Items[0];
        col[k] := p.Colorset;
        Buf.Add(p);
        Lst.Delete(0);
      end;

    gbts8x16:
      for k := 0 to Lst.Count-1 do
      begin
        (* block #1 *)
        p := Lst.Items[0];
        col[k] := p.Colorset;
        Buf.Add(p);

        (* block #2 *)
        New(q);
        for i := 0 to 7 do
          for j :=0 to 7 do
            q.data[i,j] := p.data[i+8,j];
        Buf.Add(q);
        Lst.Delete(0);
      end;

    gbts16x16:
      for k := 0 to Lst.Count-1 do
      begin
        (* block #1 *)
        p := Lst.Items[0];
        col[k] := p.Colorset;
        Buf.Add(p);

        (* block #2 *)
        New(q);
        for i := 0 to 7 do
          for j :=0 to 7 do
            q.data[i,j] := p.data[i+8,j];
        Buf.Add(q);

        (* block #3 *)
        New(q);
        for i := 0 to 7 do
          for j :=0 to 7 do
            q.data[i,j] := p.data[i,j+8];
        Buf.Add(q);

        (* block #4 *)
        New(q);
        for i := 0 to 7 do
          for j :=0 to 7 do
            q.data[i,j] := p.data[i+8,j+8];
        Buf.Add(q);

        Lst.Delete(0);
      end;



    gbts32x32:
      for k := 0 to Lst.Count-1 do
      begin
        p := Lst.Items[0];
        col[k] := p.Colorset;

        for l := 0 to 3 do
        begin

          if (l > 1) then xd := 16 else xd := 0;
          if ((l mod 2) = 0) then yd := 0 else yd := 16;


          (* block #1 *)
          New(q);
          for i := 0 to 7 do
            for j := 0 to 7 do
              q.data[i ,j] := p.data[i + yd, j + xd];
          Buf.Add(q);

          (* block #2 *)
          New(q);
          for i := 0 to 7 do
            for j := 0 to 7 do
              q.data[i, j] := p.data[i + 8 + yd, j + xd];
          Buf.Add(q);

          (* block #3 *)
          New(q);
          for i := 0 to 7 do
            for j := 0 to 7 do
              q.data[i, j] := p.data[i + yd, j + 8 + xd];
          Buf.Add(q);

          (* block #4 *)
          New(q);
          for i := 0 to 7 do
            for j := 0 to 7 do
              q.data[i, j] := p.data[i + 8 + yd, j + 8 + xd];
          Buf.Add(q);

        end;
        Lst.Delete(0);
        Dispose(p);
      end;
  end;


  (* Stap #2: van gbts8x8 naar OutS *)

  case OutS of
    gbts8x8:
      for k := 0 to Buf.Count-1 do
      begin
        p := Buf.Items[0];
        p.ColorSet := col[k];
        Lst.Add(p);
        Buf.Delete(0);
      end;

    gbts8x16:
      for k := 0 to (Buf.Count div 2)-1 do
      begin
        p := Buf.Items[0];
        p.ColorSet := col[k];
        q := Buf.Items[1];

        for i := 0 to 7 do
          for j :=0 to 7 do
            p.data[i+8,j] := q.data[i,j];
        Lst.Add(p);
        Buf.Delete(0);
        Buf.Delete(0);
      end;

    gbts16x16:
      for k := 0 to (Buf.Count div 4)-1 do
      begin
        p := Buf.Items[0];
        p.ColorSet := col[k];

        q := Buf.Items[1];
        for i := 0 to 7 do
          for j :=0 to 7 do
            p.data[i+8,j] := q.data[i,j];

        q := Buf.Items[2];
        for i := 0 to 7 do
          for j :=0 to 7 do
            p.data[i,j+8] := q.data[i,j];

        q := Buf.Items[3];
        for i := 0 to 7 do
          for j :=0 to 7 do
            p.data[i+8,j+8] := q.data[i,j];

        Lst.Add(p);

        for i := 0 to 3 do
          Buf.Delete(0);
      end;

    gbts32x32:
      for k := 0 to (Buf.Count div 16)-1 do
      begin

        p := Buf.Items[0];
        p.ColorSet := col[k];

        for l := 0 to 3 do
        begin

          if (l > 1) then xd := 16 else xd := 0;
          if ((l mod 2) = 0) then yd := 0 else yd := 16;

          q := Buf.Items[(4*l) + 0];
          for i := 0 to 7 do
            for j :=0 to 7 do
              p.data[i + yd, j + xd] := q.data[i, j];

          q := Buf.Items[(4*l) + 1];
          for i := 0 to 7 do
            for j :=0 to 7 do
              p.data[i + 8 + yd, j + xd] := q.data[i, j];

          q := Buf.Items[(4*l) + 2];
          for i := 0 to 7 do
            for j :=0 to 7 do
              p.data[i + yd, j + 8 + xd] := q.data[i, j];

          q := Buf.Items[(4*l) + 3];
          for i := 0 to 7 do
            for j :=0 to 7 do
              p.data[i + 8 + yd, j + 8 + xd] := q.data[i, j];
        end;

        Lst.Add(p);

        for i := 0 to 15 do
          Buf.Delete(0);

      end;

  end;

  Buf.Free;

end;




function IntToColor(i : integer; p : TGBPaletteType; c : PGBColorType): TColor;
begin
      case p[i] of
        0  : Result := c[0];
        1  : Result := c[1];
        2  : Result := c[2];
        else Result := c[3];
      end;
end;

function ColorToInt(i : TColor; p : TGBPaletteType; c : TGBColorType): Integer;
var j : integer;
begin
  if (i = c[0]) then j := 0 else
  if (i = c[1]) then j := 1 else
  if (i = c[2]) then j := 2 else
                     j := 3;

  if (j = p[0]) then Result := 0 else
  if (j = p[1]) then Result := 1 else
  if (j = p[2]) then Result := 2 else
                     Result := 3;

end;


function GetTileSize( x,y : integer): TGBTileSize;
begin
  if (x = 8)  and (y = 8)  then Result := gbts8x8   else
  if (x = 8)  and (y = 16) then Result := gbts8x16  else
  if (x = 16) and (y = 16) then Result := gbts16x16 else
                                Result := gbts32x32;
end;


procedure DetermineTileSize(i : TGBTileSize; var x,y : integer);
begin

  (* determine pixel-count *)
  case i of
    gbts8x8   : begin
                  x := 8;
                  y := 8;
                end;
    gbts8x16  : begin
                  x := 8;
                  y := 16;
                end;
    gbts16x16 : begin
                  x := 16;
                  y := 16;
                end;
    else        begin
                  x := 32;
                  y := 32;
                end;
    end;
end;


procedure TGBMapType.SetBounds( w, h : integer; PreserveData : boolean);
const
  EmptyRec : RMapRec = (Tile:0;PropCol:0);
var
  buf  : PGBMapData;
  prpbuf : PGBMapPropData;
  x, y,i : integer;
begin

  (* get new data-space *)
  GetMem(buf, (w * h * SizeOf(RMapRec)) );

  (* copy data *)
  if Assigned(FData) then
  begin
    if PreserveData then
      for x:= 0 to w-1 do
        for y := 0 to h-1 do
          if (FWidth > x) and (FHeight > y) then
            buf[(y*w) + x] := FData[(y*FWidth) + x]
          else
            buf[(y*w) + x] := EmptyRec;

    FreeMem(FData);
  end
  else
  begin
    if PreserveData then
      for y:= 0 to h-1 do
        for x := 0 to w-1 do
          buf[(y*w) + x] := EmptyRec;
  end;

  (* link in new data *)
  FData := buf;



  (* properties *)
  for i:= 0 to FPropCount-1 do
  begin
    (* get new data-space *)
    GetMem(prpbuf, (w * h * SizeOf(RMapPropDef)) );

    (* copy data *)
    if Assigned(FPropData[i]) then
    begin
      if PreserveData then
        for x:= 0 to w-1 do
          for y := 0 to h-1 do
            if (FWidth > x) and (FHeight > y) then
              prpbuf[(y*w) + x] := FPropData[i][(y*FWidth) + x]
            else
              prpbuf[(y*w) + x] := 0;

      FreeMem(FPropData[i]);
    end
    else
    begin
      if PreserveData then
        for x:= 0 to w-1 do
          for y := 0 to h-1 do
            prpbuf[(y*w) + x] := 0;
    end;

    FPropData[i] := prpbuf;
  end;

  (* setup Property Colors *)

  FWidth := w;
  FHeight := h;
end;

procedure TGBMapType.SetWidth( i : integer);
begin
  SetBounds( i, FHeight, True );
end;

procedure TGBMapType.SetHeight( i : integer);
begin
  SetBounds( FWidth, i, True);
end;

destructor TGBMapType.destroy;
var i : integer;
begin
  if Assigned(FData) then FreeMem(FData);

  for i := 0 to PROPCNT-1 do
    if Assigned(FPropData[i]) then FreeMem(FPropData[i]);

  inherited Destroy;
end;

constructor TGBMapType.Create;
begin
  inherited Create;

  SetBounds( 10, 10, True);
end;


function TGBMapType.GetPropData(Indx : integer): PGBMapPropData;
begin
  result := FPropData[Indx];
end;

function TGBMapType.GetPropInitVal(Indx : integer): PGBMapPropInitVal;
begin
  result := FPropInitVal[Indx];
end;

function TGBMapType.GetPropDef(Indx : integer): RMapPropDef;
begin
  Result := FPropDef[Indx];
end;

procedure TGBMapType.SetPropDefLoc(Loc : integer; const Def : RMapPropDef);
var i : integer;
begin
  if (Def.Loc = -1) then
  begin
    (* delete *)
    DeletePropDef(Loc);
//    for i := FPropCount-1 downto 0 do
//      if FPropDef[i].Loc = Loc then DeletePropDef(i);
  end
  else
  begin
    if (Def.Loc < FPropCount) then
    begin
      FPropDef[Def.Loc].pType := Def.pType;
      FPropDef[Def.Loc].Name  := Def.Name;
      FPropDef[Def.Loc].Size  := Def.Size;
    end
    else
      InsertPropDef(FPropCount, Def);
{
    for i := FPropCount-1 downto 0 do
      if FPropDef[i].Loc = Loc then
      begin
        FPropDef[i].pType := Def.pType;
        FPropDef[i].Name  := Def.Name;
        FPropDef[i].Size  := Def.Size;
        Exit;
      end;

    InsertPropDef(FPropCount, Def);
}  end;
end;



procedure TGBMapType.InsertPropDef( indx : integer; const def : RMapPropDef );
var i : integer;
begin

  if (FPropCount < PROPCNT-1) and (indx < PROPCNT-1) then
  begin
    (* move indexes *)
    for i := PROPCNT-2 downto indx do FPropData[i+1] := FPropData[i];

    (* make new set *)
    GetMem(FPropData[indx], FWidth * FHeight * SizeOf(Word));
    ClearBuf( FPropData[indx], FWidth * FHeight * SizeOf(Word));

    (* move defs *)
    for i := PROPCNT-2 downto indx do FPropDef[i+1] := FPropDef[i];
    FPropDef[indx] := def;

    (* move initial values *)
    for i := PROPCNT-2 downto indx do FPropInitVal[i+1] := FPropInitVal[i];
    ResizeInitVal(Indx, FTileCount, False);

    inc(FPropCount);
  end;

end;


procedure TGBMapType.DeletePropDef( indx : integer);
var b : PGBMapPropData;
    c : PGBMapPropInitVal;
    i : integer;
begin
  if (indx < FPropCount) then
  begin
    (* move indexes *)
    b := FPropData[indx];
    for i := indx to PROPCNT-2 do FPropData[i] := FPropData[i+1];
    FPropData[PROPCNT-1] := nil;
    FreeMem(b);

    (* move definitions *)
    for i := indx to PROPCNT-2 do FPropDef[i] := FPropDef[i+1];

    (* move initial values *)
    c := FPropInitVal[i];
    for i := indx to PROPCNT-2 do FPropInitVal[i] := FPropInitVal[i+1];
    FPropInitVal[PROPCNT-1] := nil;
    FreeMem(c);

    dec(FPropCount);
  end;
end;


procedure TGBMapType.SetTileCount( cnt : integer);
var i : integer;
begin
  for i := 0 to FPropCount-1 do ResizeInitVal( i, cnt, True);
  FTileCount := cnt;
end;



procedure TGBMapType.ResizeInitVal( Indx : integer; cnt : integer; Preserve : boolean);
var p,q : PGBMapPropInitVal;
    i,j : integer;
begin
  q := FPropInitVal[Indx];

  GetMem(FPropInitVal[Indx], cnt * SizeOf(Word));
  ClearBuf( FPropInitVal[Indx], cnt * SizeOf(Word));

  if (Preserve and assigned(q)) then
  begin
    j := cnt;
    if (j > FTileCount) then j := FTileCount;
    for i := 0 to j-1 do
      FPropInitVal[Indx][i] := q[i];
  end;

  FreeMem(q);                   
end;




end.

