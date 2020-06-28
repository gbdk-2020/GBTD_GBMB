unit gbshow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  gbconst, ShadowCanvas, ColorController;

type
  TGBShowCase = class(TCustomControl)
  private
    { Private declarations }
    FTileSize  : TGBTileSize;
    FTileCount : Integer;
    FFreezed   : boolean;
    FTileData  : PTileType;

    FXLen      : integer;
    FYLen      : integer;
    Image      : TShadowCanvas;

    procedure SetTileSize(i : TGBTileSize);
    procedure SetControlSize;
    procedure SetTileCount(i : integer);

  protected
    { Protected declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  public
    { Public declarations }
    procedure Paint; override;
    property TileData : PTileType read FTileData write FTileData;
    procedure DrawPixel(x,y : integer);

  published
    { Published declarations }
    property TileSize  : TGBTileSize read FTileSize write SetTileSize;
    property TileCount : Integer read FTileCount write SetTileCount;
    property Freezed   : Boolean read FFreezed write FFreezed;


  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Extensions', [TGBShowCase]);
end;



(**********************************************)
(*                                            *)
(*          Constructors & Destructors        *)
(*                                            *)
(**********************************************)


constructor TGBShowCase.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  Image := TShadowCanvas.Create(1,1);
end;


destructor TGBShowCase.Destroy;
begin
  Image.free;
  Inherited destroy;
end;



(**********************************************)
(*                                            *)
(*             Property handlers              *)
(*                                            *)
(**********************************************)


procedure TGBShowCase.SetTileCount(i : integer);
begin
  if (i < 1) then FTileCount := 1
  else            FTileCount := i;
  SetControlSize;
end;


procedure TGBShowCase.SetTileSize(i : TGBTileSize);
begin
  DetermineTileSize(i, FXLen, FYLen);
  FTileSize := i;
  SetControlSize;
end;


procedure TGBShowCase.SetControlSize;
begin
  (* change width *)
  Image.Free;
  Image := TShadowCanvas.Create(FXLen*3, FYLen*3);
  SetBounds(Left, Top, (FXLen*3)*TileCount, (FYLen*3)*TileCount);
end;




(**********************************************)
(*                                            *)
(*                   Main                     *)
(*                                            *)
(**********************************************)



procedure TGBShowCase.Paint;
var i,j : integer;
var c : integer;
    iCol : PGBColorSets;
    ClrIndex : integer;
begin
  if (not FFreezed) and Showing and assigned(FTileData) then
  begin
    iCol     := @GBColorController.CurShadowColorSets[0];
    ClrIndex := GBColorController.ColorSetIndex;

    (* draw pixels *)
    for i := 0 to FYLen-1 do
      for j := 0 to FXLen-1 do
      begin
        c := iCol[FTileData.ColorSet[ClrIndex],FTileData.data[i,j]];
        Image.RectangleSized( (j*3), (i*3), 3, 3, c);
      end;

    (* draw main showcase *)
    Image.Draw(Canvas.handle,0,0);

    if (FTileCount > 1) then
    begin
      (* fill up hor line *)
      for i := 1 to FTileCount-1 do
        Image.Draw(self.canvas.handle,(i*FXLen*3),0);

      (* fill up the rest *)
      for j := 1 to FTileCount-1 do
        for i := 0 to FTileCount-1 do
          Image.Draw(self.canvas.handle,(i*FXLen*3), (j*FYLen*3));
    end;
  end;
end;



procedure TGBShowCase.DrawPixel(x,y : integer);
var i,j : integer;
var c : TColor;
var rSrc, rDest : tRect;
begin

  if (not FFreezed) and Showing and assigned(FTileData) then
  begin

    with GBColorController do
      c := CurColorSets[0,FTileData.ColorSet[ColorSetIndex],FTileData.data[y,x]];

    with Canvas do
    begin
      Pen.Color := c;
      Brush.Color := c;
      Brush.Style := bsSolid;
      Rectangle(x*3, y*3, (x*3)+3, (y*3)+3);
    end;


    if (FTileCount > 1) then
    begin
      (* init *)
      x := FXLen*3;
      y := FYLen*3;

      with rSrc do
      begin
        Left := 0;
        Top := 0;
        Right := x;
        Bottom := y;
      end;

      (* huidige kollom aanvullen *)
      with rDest do
      begin
        Top := 0;
        Bottom := y;
        for i := 1 to FTileCount-1 do
        begin
          Left := i * x;
          Right := (i+1) * x;
          Canvas.CopyRect(rDest, Canvas, rSrc);
        end;

        (* Overige kollomen *)
        for j := 1 to FTileCount -1 do
        begin
          Top := j * y;
          Bottom := (j+1) * y;

          for i := 0 to FTileCount-1 do
          begin
            Left := i * x;
            Right := (i+1) * x;
            Canvas.CopyRect(rDest, Canvas, rSrc);
          end;
        end;
      end;
    end;

  end;

end;



end.
