unit fldfill;

interface

uses gbconst;

  procedure FloodFill(x,y : integer; Clr : integer; p : PTileType; Width, Height : integer);

implementation

(* Globals (to reduce stack-space) *)
var FldClr, FilClr : integer;
var PTile          : PTileType;
var w,h            : integer;


procedure RecuringFill(x,y : byte );
begin
  (* change actual color *)
  PTile.data[y,x] := FilClr;

  (* check and process neigbours *)
  if ( y < h ) and ( PTile.data[y+1, x] = FldClr ) then RecuringFill(x, y+1);
  if ( y > 0 ) and ( PTile.data[y-1, x] = FldClr ) then RecuringFill(x, y-1);
  if ( x < w ) and ( PTile.data[y, x+1] = FldClr ) then RecuringFill(x+1, y);
  if ( x > 0 ) and ( PTile.data[y, x-1] = FldClr ) then RecuringFill(x-1, y);
end;


procedure FloodFill(x,y : integer; Clr : integer; p : PTileType; Width, Height : integer);
(*******************************************************************)
(* FloodFills a tile (p) with color Clr, starting at location x,y. *)
(* Width and Height should contain the dimensions of the tile.     *)
(*******************************************************************)
begin
  (* init *)
  FilClr := Clr;
  PTile := p;
  FldClr := p.data[y,x];
  w := Width-1;
  h := Height-1;

  (* start recursive process *)
  if (FilClr <> FldClr) then
    RecuringFill(x,y);
end;

end.
