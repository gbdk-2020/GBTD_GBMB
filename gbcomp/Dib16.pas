unit DIB16;

interface

uses
  Windows, Messages, WinProcs, WinTypes, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, stdctrls, ExtCtrls;

type

TDIB16bit = class(Tobject)
  private
    {Privates}
    FDibHandle : HBitmap;  {Bitmap Handle to the DIB}
    FBheader : TBitMapInfo; {Holds info about the DIB}
    FPointerToBitmap : Pointer; {The pointer to the DIB's memory}
    FScanWidth : Integer; {The width in bytes of a scanline}
    FDeviceContext : HDC; {A device context for the DIB}
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure FlipBackPage(DeviceContext : HDC);
    procedure SetPixel(X, Y : Integer; Color : Word);
    procedure ClearBackPage(Color : Word);
    procedure DrawHorizontalLine (Y, X1, X2 : Integer; Color : Word) ;
    procedure DrawVerticalLine (X, Y1, Y2 : Integer; Color : Word) ;
    function GetHandle : HBitmap; {Returns the Bitmap handle of the DIB}
    constructor Create(Height, Width : Integer);
    destructor Destroy; override;
  end;

function CalculateRGBWord(Color : TColor) : Word;

implementation

function CalculateRGBWord(Color : TColor) : Word;
var
  Calc : Single;
  R,G,B : Integer;
begin
  {GetRValue, GetGValue, GetBValue return a value which is based on the color scale of  0 to 255}

  {This gets the red value and rescales it from a 1/256 number to a 0/31 number}
  Calc := GetRValue(Color) / 2.56;
  Calc := Calc * 0.31;
  R := Round(Calc);

  {This gets the green value and rescales it from a 1/256 number to a 0/31 number}
  Calc := GetGValue(Color) / 2.56;
  Calc := Calc * 0.31;
  G := Round(Calc);

  {This gets the red value and rescales it from a 1/256 number to a 0/31 number}
  Calc := GetBValue(Color) / 2.56;
  Calc := Calc * 0.31;
  B := Round(Calc);

  {The B Value is the last 5 bits so it can be left alone}
  {R must be shifted left by 10 bits so that it sits at the position 15-11}
  R := R shl 10;
  {G must be shifted to the left 5 bits so that it sits at the position 10..6}
  G := G shl 5;
  {Then just add them together}
  Result := R + G + B;
end;




procedure TDIB16bit.FlipBackPage(DeviceContext : HDC);
begin
{This copies the DIB to the device context (usually a handle to a canvas) specified in the
parameter DeviceContext : HDC}

  StretchDIBits(DeviceContext, 0, 0, FBHeader.bmiHeader.biWidth, FBHeader.bmiHeader.biHeight,
                0, 0, FBHeader.bmiHeader.biWidth, FBHeader.bmiHeader.biHeight, FPointerToBitmap, FBheader, DIB_RGB_COLORS, SRCCOPY);

end;

procedure TDIB16bit.SetPixel(X, Y : Integer; Color : Word);
var
  BasePointer : ^Word;
begin
{Assigns BasePointer the memory address of the pixel in the DIB by adding to the
beginning of the address (FPointerBitmap) the Y value multiplied by the ScanWidth.  Then the X is added
to the result.  X is multiplied by two because a pixel in 16bit color takes up 2 bytes
of memory}
  Integer(BasePointer) := Integer(FPointerToBitmap) + (Y * FScanWidth) + (X * 2);
  BasePointer^ := Color;
end;

procedure TDIB16bit.ClearBackPage(Color : Word);
var
  X : Integer;
  BasePointer : ^Word;
begin
{Loops through from the beginning of the bitmap to the end setting every pixel
to the 'Color' parameter}
  BasePointer := FPointerToBitmap;
  for X := 0 to ((FScanWidth div 2) * (FBHeader.bmiHeader.biHeight)) - 1 do
    begin
      BasePointer^ := Color;
      {inc increments a pointer by the size of the type that the pointer points to.
      In other words 2 bytes}
      inc(BasePointer);
    end;
end;

procedure TDIB16bit.DrawHorizontalLine (Y, X1, X2 : Integer; Color : Word) ;
var
  X : Integer;
  BasePointer : ^Word;
begin
{Assigns BasePointer the memory address of the pixel in the DIB by adding to the
beginning of the address (FPointerBitmap) the Y value multiplied by the ScanWidth.  Then the X1 is added
to the result.  X1 is multiplied by two becasue a pixel in 16bit color takes up 2 bytes
of memory.  Then the code just loops through the number of pixels in the line}
  Integer(BasePointer) := Integer(FPointerToBitmap) + (Y * FScanWidth) + (X1 * 2);
  for X := 0 to (X2 - X1) do
    begin
      BasePointer^ := Color;
      {inc moves the pointer along 1 pixel in memory (2 bytes)}
      inc(BasePointer);
    end;
end;


procedure TDIB16bit.DrawVerticalLine (X, Y1, Y2 : Integer; Color : Word) ;
var
  Y : Integer;
  BasePointer : ^Word;
begin
{Assigns BasePointer the memory address of the pixel in the DIB by adding to the
beginning of the address (FPointerBitmap) the Y value multiplied by the ScanWidth.  Then the X1 is added
to the result.  X1 is multiplied by two becasue a pixel in 16bit color takes up 2 bytes
of memory.  Then the code just loops through the number of pixels in the line}
  Integer(BasePointer) := Integer(FPointerToBitmap) + (X*2) + (Y1 * (FScanWidth div 2));
  for Y := 0 to (Y2 - Y1) do
    begin
      BasePointer^ := Color;
      {inc moves the pointer along 1 pixel in memory (2 bytes)}
      Inc(BasePointer , (FScanWidth div 2));
    end;
end;


function TDIB16bit.GetHandle : HBitmap;
begin
  result := FDibHandle;
end;

constructor TDIB16Bit.Create(Height, Width : Integer);
begin
  Inherited Create;
  {Set up the FBHeader with the values it needs to know what type of DIB is required}
  {First set up the Height and Width in pixels}
  FBHeader.bmiHeader.biWidth := Width;
  FBHeader.bmiHeader.biHeight := Height;
  {FBHeader.bmiHeader.biPlanes is always 1}
  FBHeader.bmiHeader.biPlanes := 1;
  {Set up what color depth is required - 16bit}
  FBHeader.bmiHeader.biBitCount := 16;
  {No compression}
  FBHeader.bmiHeader.biCompression := BI_RGB;
  {Work out the size of memory needed for the DIB}
  FBHeader.bmiHeader.biSizeImage := ((((16*FBHeader.bmiHeader.biWidth) + 31) div 32) * 4) * Height;
  {The width in bytes of every scanline}
  FScanWidth := (((FBHeader.bmiHeader.biWidth * 16) + 31) div 32) * 4;

  {These can be ignored and set to 0 for 16bit color}
  FBHeader.bmiHeader.biXPelsPerMeter := 0;
  FBHeader.bmiHeader.biYPelsPerMeter := 0;
  FBHeader.bmiHeader.biclrUsed := 0;
  FBHeader.bmiHeader.biclrImportant := 0;
  {The size of the bmiHeader structure}
  FBHeader.bmiHeader.biSize := 40;
  {Not used in 16bit color}
  FBHeader.bmiColors[0].rgbRed := 255;
  FBHeader.bmiColors[0].rgbBlue := 255;
  FBHeader.bmiColors[0].rgbGreen := 255;
  FBHeader.bmiColors[0].rgbReserved := 255;
  {Create a Device context that is compatable with the current screen}
  FDeviceContext := CreateCompatibleDC(0);
  {Now the DIB is allocated and it returns a HBitmap handle to itself}
  FDibHandle := CreateDibSection(FDeviceContext, FBHeader, DIB_RGB_COLORS, FPointerToBitmap, 0,0);
end;

destructor TDIB16bit.Destroy;
begin
  {Delete the DIB}
  DeleteObject(FDibHandle);
  {Delete the Device Context}
  DeleteDC(FDeviceContext);
  Inherited;
end;

end.
