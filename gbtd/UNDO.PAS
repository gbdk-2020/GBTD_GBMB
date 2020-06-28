unit undo;

interface

uses Classes, Menus, gbconst;

const
  TGBTDUndoMaxPixel = 30;

type

  TGBTDUndoState = (gbunNone, gbunPixel, gbunTotal, gbunPixelEnd);

  TGBTDUndoPixelType = record
    x   : integer;
    y   : integer;
    Clr : byte;
  end;

  TGBTDUndo = class
  private

    FCurState : TGBTDUndoState;
    FTileList : TList;
    FCurTile : integer;

    FTotalStore : TileType;
    FPixelStore : array [0..TGBTDUndoMaxPixel] of TGBTDUndoPixelType;
    FPixelCnt   : integer;

    FMenuItem   : TMenuItem;

  public

    constructor Create( TileList : TList; Tile : integer; UndoMenu : TMenuItem );

    procedure Clear;
    procedure ChangeTile( i : integer );
    procedure PixelEnd;

    procedure LogPixel(x,y : integer; Clr : byte);
    procedure LogTotal(p : PTileType);

    procedure Undo;

  end;

implementation

constructor TGBTDUndo.Create( TileList : TList; Tile : integer; UndoMenu : TMenuItem );
begin
  FTileList := TileList;
  FMenuItem := UndoMenu;

  ChangeTile(Tile);
end;




procedure TGBTDUndo.Clear;
begin
  FCurState := gbunNone;
  FPixelCnt := 0;

  if Assigned(FMenuItem) then FMenuItem.Enabled := False;
end;



procedure TGBTDUndo.ChangeTile( i : integer );
begin
  FCurTile := i;
  Clear;
end;


procedure TGBTDUndo.PixelEnd;
begin
  if (FCurState = gbunPixel) then FCurState := gbunPixelEnd;
end;


procedure TGBTDUndo.LogTotal(p : PTileType);
var i, j : integer;
begin
  if (FCurState <> gbunTotal) then Clear;

  for i := 0 to 31 do
    for j := 0 to 31 do
      FTotalStore.data[i,j] := p^.data[i,j];

  FCurState := gbunTotal;

  if Assigned(FMenuItem) then FMenuItem.Enabled := True;
end;



procedure TGBTDUndo.LogPixel(x,y : integer; Clr : byte);
begin
  if (FCurState <> gbunPixel) then
  begin
    Clear;
    FCurState := gbunPixel;
    if Assigned(FMenuItem) then FMenuItem.Enabled := True;
  end;

  if (FPixelCnt >= TGBTDUndoMaxPixel-1) then FPixelCnt := 0;

  FPixelStore[FPixelCnt].x := x;
  FPixelStore[FPixelCnt].y := y;
  FPixelStore[FPixelCnt].Clr := Clr;

  Inc(FPixelCnt);
end;




procedure TGBTDUndo.Undo;
var i,j : integer;
    p : PTileType;
begin
  case FCurState of
    gbunNone  : ;

    gbunTotal : begin
                  p := FTileList.Items[FCurTile];
                  for i := 0 to 31 do
                    for j := 0 to 31 do
                      p^.data[i,j] := FTotalStore.data[i,j];
                  Clear;
                end;

    else        begin
                  p := FTileList.Items[FCurTile];
                  for i := FPixelCnt-1 downto 0 do
                    p^.data[ FPixelStore[i].y, FPixelStore[i].x ] := FPixelStore[i].Clr;
                  Clear;
                end;
  end;
end;

end.
