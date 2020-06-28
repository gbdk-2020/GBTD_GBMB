unit GBCCBitmap;

interface

uses ShadowCanvas, Graphics, mxarrays, gbConst;



  type
    TGBCCBitmap = class
    private
    { Private declarations }

      FWidth         : integer;
      FHeight        : integer;

      FXOffset       : integer;
      FYOffset       : integer;

      FResultWidth   : integer;
      FResultHeight  : integer;

      FImage         : PGBCompColorArray;


      function ColorToComp( c : TColor ): TGBCompColor;
      function CompToColor( c : TGBCompColor ): TColor;


    protected
    { protected declarations }

      procedure SetWidth (i : integer);
      procedure SetHeight (i : integer);

      procedure SetCompColors( x,y : integer; c : TGBCompColor);
      function GetCompColors( x,y : integer): TGBCompColor;

      procedure SetColors( x,y : integer; c : TColor);
      function GetColors( x,y : integer): TColor;


    public
    { Public declarations }

      property Width : integer read FWidth write SetWidth;
      property Height : integer read FHeight write SetHeight;

      property CompColors[x,y : integer] : TGBCompColor read GetCompColors write SetCompColors;
      property Colors[x,y : integer] : TColor read GetColors write SetColors;


      procedure SetBounds( Width, Height : integer; Conserve : boolean);

      procedure LoadBitmap( Bitmap : TBitmap);



      constructor Create;
      destructor Destroy; override;

  end;


implementation


constructor TGBCCBitmap.Create;
begin
  inherited Create;

end;


destructor TGBCCBitmap.Destroy;
begin
  FreeMem(FImage);

  inherited Destroy;
end;

procedure TGBCCBitmap.SetWidth (i : integer);
begin
  SetBounds(i, FHeight, True);
end;

procedure TGBCCBitmap.SetHeight (i : integer);
begin
  SetBounds(FWidth, i, True);
end;


procedure TGBCCBitmap.SetBounds( Width, Height : integer; Conserve : boolean);
var
  p : PGBCompColorArray;
  x,y,i,j : integer;

begin
  GetMem(p, Width * Height * SizeOf(TGBCompColor) );

  if Assigned(FImage) then
  begin
    if Conserve then
    begin
      x := FWidth;
      if (x > Width) then x := Width;

      y := FHeight;
      if (y > Height) then y := Height;

      for j := 0 to y-1 do
        for i := 0 to x-1 do
          p[(j * Width) + i] := FImage[(j * FWidth) + i];
    end;

    FreeMem(FImage);
  end;

  FImage := p;
  FWidth := Width;
  FHeight := Height;
end;


procedure TGBCCBitmap.SetCompColors( x,y : integer; c : TGBCompColor);
begin
  if (x >= FWidth) or (y >= FHeight) then raise EArrayError.Create('Array out of bounds');
  FImage[(y * FWidth) + x] := c;
end;


function TGBCCBitmap.GetCompColors( x,y : integer): TGBCompColor;
begin
  if (x >= FWidth) or (y >= FHeight) then raise EArrayError.Create('Array out of bounds');
  Result := FImage[(y * FWidth) + x];
end;


procedure TGBCCBitmap.SetColors( x,y : integer; c : TColor);
begin
  if (x >= FWidth) or (y >= FHeight) then raise EArrayError.Create('Array out of bounds');
  FImage[(y * FWidth) + x] := ColorToComp(c);
end;


function TGBCCBitmap.GetColors( x,y : integer): TColor;
begin
  if (x >= FWidth) or (y >= FHeight) then raise EArrayError.Create('Array out of bounds');
  Result := CompToColor(FImage[(y * FWidth) + x]);
end;


function TGBCCBitmap.ColorToComp( c : TColor ): TGBCompColor;
begin
  Result := ((c and $000000F8) shr 3) +
            ((c and $0000F800) shr (8+3-5)) +
            ((c and $00F80000) shr (16+3-11));
end;

function TGBCCBitmap.CompToColor( c : TGBCompColor ): TColor;
begin
  Result := ((c shl (3))       and $0000F8) +
            ((c shl (8+3-5))   and $00F800) +
            ((c shl (16+3-11)) and $F80000);
end;


procedure TGBCCBitmap.LoadBitmap( Bitmap : TBitmap);
var i,j : integer;
begin
  SetBounds(Bitmap.Width, Bitmap.Height, False);

  for j := 0 to Height-1 do
    for i := 0 to Width-1 do
      FImage[(j * FWidth) + i] := ColorToComp( Bitmap.canvas.Pixels[i,j] );
end;



end.
