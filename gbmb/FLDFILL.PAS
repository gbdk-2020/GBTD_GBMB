unit fldfill;

interface

uses gbconst, GBMBMain, MapUndo, Repository ;

  procedure MapFloodFill(x,y : integer; TileNr : integer; Width, Height : integer);


implementation

(* Globals (to reduce stack-space) *)
var FldTile, FilTile : integer;
var w,h            : integer;


procedure RecuringFill(x,y : word );
begin
  SetTileOfLocation(x,y, FilTile);

  if ( y < h ) and ( GetTileOfLocation(x, y+1) = FldTile ) then RecuringFill(x, y+1);
  if ( y > 0 ) and ( GetTileOfLocation(x, y-1) = FldTile ) then RecuringFill(x, y-1);
  if ( x < w ) and ( GetTileOfLocation(x+1, y) = FldTile ) then RecuringFill(x+1, y);
  if ( x > 0 ) and ( GetTileOfLocation(x-1, y) = FldTile ) then RecuringFill(x-1, y);
end;



procedure MapFloodFill(x,y : integer; TileNr : integer; Width, Height : integer);
begin
  FilTile := TileNr;
  FldTile := GetTileOfLocation(x,y);
  w := Width-1;
  h := Height-1;
  MapUndoCtrl.Mode := gbumTotal;

  if (FilTile <> FldTile) then
    RecuringFill(x,y);
end;




end.
