unit ShadowCanvas;

interface

uses SysUtils, Windows, Graphics;


  type EShadowCanvasError = class(Exception);

  type TShadowCanvas = class
  private
    { Private declarations }
    FBitmapInfoHeader : TBitmapInfoHeader;
    FBitMapInfo       : TBitmapInfo;
    DIB               : HBitMap;
    FPlane            : Pointer;
    FDeviceContext    : HDC; {A device context for the DIB}

    function GetWidth : integer;
    function GetHeight : integer;
  public
    { Public declarations }
    constructor Create(x,y : integer);
    destructor Destroy; override;
    procedure Draw(DeviceContext : HDC; x, y : integer);
    procedure DrawPart(DeviceContext : HDC; x, y, w, h : integer);

    procedure RectangleSized(X, Y, Width, Height: Integer; c : TColor);
    procedure HorLine(X, Y, Width: Integer; c : TColor);
    procedure VerLine(X, Y, Height: Integer; c : TColor);

    procedure SetPixel(X, Y: Integer; c : TColor);
    function GetPixel(X,Y : integer): TColor;

    property Width : integer read GetWidth;
    property Height : integer read GetHeight;
    property Plane : Pointer read FPlane;

  end;

  function GetShadowColor( c : TColor): integer;

implementation

function GetShadowColor( c : TColor): integer;
var i : integer;
begin
  i := ColorToRGB(c);
  Result := ((i and $0000FF) shl 16) + (i and $00FF00) + ((i and $FF0000) shr 16);
end;


constructor TShadowCanvas.Create(x,y : integer);
begin
  inherited Create;

  with FBitmapInfoHeader do
  begin
    biSize   := SizeOf(TBitMapInfoHeader);
    biWidth  := x;
    biHeight := y;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := x* y * 4;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;//$00FFFFFF;
    biClrImportant := 0;
  end;

  with FBitMapInfo do
  begin
    bmiHeader := FBitmapInfoHeader;
    bmiColors[0].rgbRed := 255;
    bmiColors[0].rgbBlue := 255;
    bmiColors[0].rgbGreen := 255;
    bmiColors[0].rgbReserved := 255;
  end;

  FDeviceContext := CreateCompatibleDC(0);
  DIB := CreateDIBSection(FDeviceContext, FBitmapInfo, DIB_RGB_COLORS, FPlane, 0,0);
  if (DIB = 0) or (GDIFlush = False) then
  begin
    DeleteDC(FDeviceContext);
    raise EShadowCanvasError.Create('Unable to create a DIB');
  end;
end;


destructor TShadowCanvas.Destroy;
begin
  {Delete the DIB}
  DeleteObject(DIB);
  {Delete the Device Context}
  DeleteDC(FDeviceContext);

  inherited destroy;
end;


function TShadowCanvas.GetWidth: integer;
begin
  Result := FBitmapInfoHeader.biWidth;
end;

function TShadowCanvas.GetHeight : integer;
begin
  Result := FBitmapInfoHeader.biHeight;
end;




procedure TShadowCanvas.Draw(DeviceContext : HDC; x, y : integer);
begin
  with FBitmapInfoHeader do
    StretchDIBits( DeviceContext, x, y, biWidth, biHeight, 0, 0, biWidth, biHeight,
                   FPlane, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure TShadowCanvas.DrawPart(DeviceContext : HDC; x, y, w, h : integer);
begin
    StretchDIBits( DeviceContext, x, y, w, h, 0, FBitmapInfoHeader.biHeight-h, w, h,
                   FPlane, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;


procedure TShadowCanvas.RectangleSized(X, Y, Width, Height: Integer; c : TColor);
var i,j : integer;
    BasePointer : ^TColor;
begin

  with FBitmapInfoHeader do
    if (X < biWidth) and (Y < biHeight) then
    begin
      if ((X+Width) >= biWidth) then Width := biWidth-X;
      if ((Y+Height) >= biHeight) then Height := biHeight-Y;

      Integer(BasePointer) := Integer(FPlane) + (((biHeight-(Y+1)) * biWidth) + X) * 4;

      for i := 0 to Height-1 do
      begin
        for j := 0 to Width-1 do
        begin
          BasePointer^ := c;
          Inc(BasePointer);
        end;
        Dec(BasePointer, biWidth + Width  );
      end;
    end;

end;

procedure TShadowCanvas.HorLine(X, Y, Width: Integer; c : TColor);
var i : integer;
    BasePointer : ^TColor;
begin

  with FBitmapInfoHeader do
    if (X < biWidth) and (Y < biHeight) then
    begin
      if ((X+Width) >= biWidth) then Width := biWidth-X;

      Integer(BasePointer) := Integer(FPlane) + (((biHeight-(Y+1)) * biWidth) + X) * 4;

      for i := 0 to Width-1 do
      begin
        BasePointer^ := c;
        Inc(BasePointer);
      end;
    end;

end;


procedure TShadowCanvas.VerLine(X, Y, Height: Integer; c : TColor);
var i : integer;
    BasePointer : ^TColor;
begin

  with FBitmapInfoHeader do
    if (X < biWidth) and (Y < biHeight) then
    begin
      if ((Y+Height) >= biHeight) then Height := biHeight-Y;

      Integer(BasePointer) := Integer(FPlane) + (((biHeight-(Y+1)) * biWidth) + X) * 4;

      for i := 0 to Height-1 do
      begin
        BasePointer^ := c;
        Dec(BasePointer, biWidth );
      end;
    end;

end;





procedure TShadowCanvas.SetPixel(X, Y: Integer; c : TColor);
var BasePointer : ^TColor;
begin

  with FBitmapInfoHeader do
    if (X < biWidth) and (Y < biHeight) then
    begin
      Integer(BasePointer) := Integer(FPlane) + (((biHeight-(Y+1)) * biWidth) + X) * 4;
      BasePointer^ := c;
    end;

end;


function TShadowCanvas.GetPixel(X,Y : integer): TColor;
var BasePointer : ^TColor;
begin
  with FBitmapInfoHeader do
    if (X < biWidth) and (Y < biHeight) then
    begin
      Integer(BasePointer) := Integer(FPlane) + (((biHeight-(Y+1)) * biWidth) + X) * 4;
      Result := BasePointer^;
    end
    else
      Result := 0;
end;


end.
