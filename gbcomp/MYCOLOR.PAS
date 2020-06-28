{*****************************************************************************}
{                                                                             }
{  file:      MYCOLOR.PAS                                                     }
{  title:     DIB handling unit                                               }
{  version:   1.0.32                                                          }
{  date:      2/10/98                                                         }
{  author:    Andreas Heckel                                                  }
{             Mail: andreas.heckel@wirtschaft.tu-ilmenau.de                   }
{  copyright: DELPHI STORAGE                                                  }
{             Web: http://www.wirtschaft.tu-ilmenau.de/~aeg/                  }
{                                                                             }
{  compiler:  Borland DELPHI 2.0                                              }
{  descript.: required DIB handling unit for TPicBtn and TFormShaper          }
{                                                                             }
{  revision history:                                                          }
{             2/10/98    first release                                        }
{                                                                             }
{  Category: FREEWARE                                                         }
{  Can you please put a link to the site in your list and not just the zip    }
{*****************************************************************************}

unit mycolor;

interface
uses  Windows, Messages, SysUtils, Classes,Graphics;
const NumColors=256;

  FUNCTION  PaletteInstalled:Bool;                                        { Test if 16 or 256 Colors }
  FUNCTION  ColorDepth:BYTE;
  PROCEDURE InitBitmapInfoHeader(HBM: HBitmap;var BI: TBitmapInfoHeader); { prepares Bitmap Header   }
  PROCEDURE GetSizes(HBM: HBITMAP; var BIPSize:Integer; var BufSize:DWord;var BitCount:Word;var ColorSize : Longint);
  FUNCTION  MyGetLogPalette(Var aPalette:HPalette;Var aLogPalette:PLogPalette;ReWrite:Boolean):Boolean;
  FUNCTION  MyGetDIBits(HBM: HBITMAP; var BIP:PBitmapInfo; var Buf:Pointer; var aLogPalette:PLogPalette): Boolean;
  FUNCTION  MyInitDIBits(HBM: HBITMAP; var BIP:PBitmapInfo; var Buf:Pointer; var aLogPalette:PLogPalette): Boolean;
  FUNCTION  QuadToColor(aQuad:TRGBQuad):TColor;
  FUNCTION  TripleToColor(aTriple:TRGBTriple):TColor;
  FUNCTION  ColorToTriple(aColor:DWORD):TRGBTriple;
  FUNCTION  SetPixelTriple(aBitmap:TBitmap;aLogPalette: PLogPalette; x,y:Integer;aTriple:TRGBTriple):Boolean;
  FUNCTION  SetDIPixelTriple(var BIP:PBitmapInfo; var Buf:Pointer;aPalette:HPalette; x,y:Integer;aTriple:TRGBTriple):Boolean;
  FUNCTION SetDIPixelColor(var BIP:PBitmapInfo; var Buf:Pointer;aPalette:HPalette; x,y:Integer;aColor:TColor):Boolean;
  FUNCTION GetPixelTriple(aBitmap:TBitmap;aLogPalette: PLogPalette; x,y:Integer;var aTriple:TRGBTriple):Boolean;
  FUNCTION GetDIPixelTriple(BIP:PBitmapInfo;Buf:Pointer;aPalette:HPalette; x,y:Integer;var aTriple:TRGBTriple):Boolean;

implementation

FUNCTION QuadToColor(aQuad:TRGBQuad):TColor;
begin
  with aQuad do Result:=(rgbBlue shl 16)+(rgbGreen shl 8)+rgbRed;
end;

FUNCTION TripleToColor(aTriple:TRGBTriple):TColor;
begin
  with aTriple do Result:=(rgbtBlue shl 16)+(rgbtGreen shl 8)+rgbtRed;
end;

FUNCTION  ColorToTriple(aColor:DWORD):TRGBTriple;
begin
  with Result do
  begin
    rgbtBlue :=pbyteArray(@aColor)^[2] ;
    rgbtGreen:=pbyteArray(@aColor)^[1] ;
    rgbtRed  :=pbyteArray(@aColor)^[0];
  end;
end;

FUNCTION PaletteInstalled:Bool;
VAR DC      :Word;
    NumCols :Integer;
BEGIN
  DC:=GetDC(0);
  NumCols:=GetDeviceCaps(DC,SizePalette)-GetDeviceCaps(DC,NumReserved);
  Result:=(GetDeviceCaps(DC,RasterCaps)AND RC_PALETTE)>0;
  IF NumCols<21 THEN Result:=False;
  ReleaseDC(0,DC);
END;

FUNCTION ColorDepth:Byte;
VAR DC :Word;
BEGIN
  DC:=GetDC(0);
  Result:=GetDeviceCaps(DC,BITSPIXEL);
  ReleaseDC(0,DC);
END;



function bmAlignDouble(Size: Longint): Longint;
begin
  bmAlignDouble := (Size + 31) div 32 * 4;
end;

PROCEDURE InitBitmapInfoHeader(HBM: HBitmap;var BI: TBitmapInfoHeader);
  var
    BM:   Windows.TBitmap;	{ Bitmap information }
    DataSize: DWORD;            { Size needed to store Color/Data }
    ColorSize:DWORD;
    BitCount: Word;		{ Number of bits per pixel }
begin
   { Get the information about the Bitmap }
   if GetObject(HBM, SizeOf(Windows.TBitmap), @BM) = 0 then Exit;

   BitCount := bm.bmPlanes * bm.bmBitsPixel;
   if (BitCount = 16)or(BitCount = 32) then
   begin
     getDibSizes(hbm,ColorSize,DataSize);
     ColorSize := 0;
   end
   else
   begin
     ColorSize := SizeOf(TRGBQuad) * (1 shl BitCount);
     DataSize := bmAlignDouble(bm.bmWidth * BitCount) * bm.bmHeight;
   end;
   if (BitCount = 32) then BitCount := 24;
   with BI do
   begin
     biSize := SizeOf(TBitmapInfoHeader);
     biWidth := bm.bmWidth;
     biHeight := bm.bmHeight;
     biPlanes := 1;
     biBitCount := BitCount;
     biCompression := 0;
     biSizeImage := DataSize;
     biXPelsPerMeter := 0;
     biYPelsPerMeter := 0;
     biClrUsed := 0;
     biClrImportant := 0;
   end;
end;

FUNCTION SetPixelTriple(aBitmap:TBitmap;aLogPalette: PLogPalette; x,y:Integer;aTriple:TRGBTriple):Boolean;
var BIP:PBitmapInfo;
    hmem         :THandle;
    buf          :Pointer;
    BIPSize      :Integer;
    ImageSize    :DWORD;
    ColorSize    :LongInt;
    BitCount     : Word;		{ Number of bits per pixel }
    Palette      : HPalette;
begin
 Result:=False;
 Palette:=0;
 GetSizes(aBitmap.handle,BIPSize,ImageSize,BitCount,ColorSize);
 GetMem(BIP, BIPSize);
 HMem := GlobalAlloc(gptr, ImageSize);
 try
   if HMem <> 0 then
   begin
     Buf := GlobalLock(HMem);
     try
       MyGetLogPalette(Palette,aLogPalette,Boolean(aLogPalette=nil));
       if MyGetDIBits(aBitmap.Handle,BIP,Buf,aLogPalette)then
       begin
         if SetDIPixelTriple(BIP,Buf,Palette,x,y,aTriple) then
         begin
           with BIP^.bmiHeader do
                SetDIBitsToDevice(aBitmap.Canvas.Handle,
                                 0, 0,
                                 biWidth, biHeight,
                                 0, 0,
                                 0,
                                 biHeight,
                                 buf,
                                 BIP^,
                                 dib_rgb_colors);
           Result:=True;
         end;
       end;
     finally
       if Palette<>0 then DeleteObject(Palette);
       GlobalUnlock(HMem);
       GlobalFree(HMem);
     end;
 end;
 finally
   FreeMem(BIP, BIPSize);
 end;
end;

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

FUNCTION SetDIPixelTriple(var BIP:PBitmapInfo; var Buf:Pointer;aPalette:HPalette; x,y:Integer;aTriple:TRGBTriple):Boolean;
Var P            : LongInt;
    pi           : Integer;
    C:Word;
begin
   Result:=False;
   if (BIP=nil)or(Buf=nil) then exit;
   with BIP^.bmiHeader do
   if (x >= 0)and(x<=biWidth-1)and
      (y >= 0)and(y<=biHeight-1)
   then
   with aTriple,BIP^.bmiHeader do
   begin
     case biBitCount of
       24:  begin
              P:=BytesPerScanline(biWidth,biBitCount,32)*(biHeight-1-y)+(3*x);
              pbyteArray(buf)^[p]:=rgbtblue;
              pbyteArray(buf)^[p+1]:=rgbtgreen;
              pbyteArray(buf)^[p+2]:=rgbtred;
             end;
       16:  begin
              c:=((rgbtRed div 8) shl 10)+((rgbtGreen div 8) shl 5)+(rgbtBlue div 8);
              p:= bmAlignDouble(biWidth*16)*(biHeight-1-y)+(2*x);
              pbyteArray(buf)^[p]:=LoByte(c);
              pbyteArray(buf)^[p+1]:=HiByte(c);
            end;
       8 :  begin
              pi:= GetNearestPaletteIndex(aPalette,TripleToColor(aTriple));
              p:= (biSizeImage div biHeight)*(biHeight-1-y)+x;
              pbyteArray(buf)^[p]:=pi;
            end;
     end;
     Result:=True;
   end;
end;

FUNCTION SetDIPixelColor(var BIP:PBitmapInfo; var Buf:Pointer;aPalette:HPalette; x,y:Integer;aColor:TColor):Boolean;
Var P            : LongInt;
    pi           : Integer;
    C:Word;
begin
   Result:=False;
   if (BIP=nil)or(Buf=nil) then exit;
   with BIP^.bmiHeader do
   if (x >= 0)and(x<=biWidth-1)and
      (y >= 0)and(y<=biHeight-1)
   then
   with BIP^.bmiHeader do
   begin
     case biBitCount of
       24:  begin
              P:=BytesPerScanline(biWidth,biBitCount,32)*(biHeight-1-y)+(3*x);
              pbyteArray(buf)^[p]:=byte((aColor and $00FF0000)shr 16);
              pbyteArray(buf)^[p+1]:=byte((aColor and $0000FF00)shr 8);
              pbyteArray(buf)^[p+2]:=byte(aColor and $000000FF);
             end;
       16:  begin
              c:=(((byte(aColor and $000000FF)) div 8) shl 10)+(((byte((aColor and $0000FF00)shr 8)) div 8) shl 5)+((byte((aColor and $00FF0000)shr 16)) div 8);
              p:= bmAlignDouble(biWidth*16)*(biHeight-1-y)+(2*x);
              pbyteArray(buf)^[p]:=LoByte(c);
              pbyteArray(buf)^[p+1]:=HiByte(c);
            end;
       8 :  begin
              pi:= GetNearestPaletteIndex(aPalette,aColor);
              p:= (biSizeImage div biHeight)*(biHeight-1-y)+x;
              pbyteArray(buf)^[p]:=pi;
            end;
     end;
     Result:=True;
   end;
end;

FUNCTION GetPixelTriple(aBitmap:TBitmap;aLogPalette: PLogPalette; x,y:Integer;var aTriple:TRGBTriple):Boolean;
var BIP          :PBitmapInfo;
    hmem         :THandle;
    buf          :Pointer;
    BIPSize      :Integer;
    ImageSize    :DWORD;
    ColorSize    :LongInt;
    BitCount     : Word;		{ Number of bits per pixel }
    Palette      : HPalette;
begin
 Result:=False;
 Palette:=0;
 GetSizes(aBitmap.handle,BIPSize,ImageSize,BitCount,ColorSize);
 GetMem(BIP, BIPSize);
 HMem := GlobalAlloc(gptr, ImageSize);
 try
   if HMem <> 0 then
   begin
     Buf := GlobalLock(HMem);
     try
       MyGetLogPalette(Palette,aLogPalette,Boolean(aLogPalette=nil));
       if (MyGetDIBits(aBitmap.Handle,BIP,Buf,aLogPalette) and
           GetDIPixelTriple(BIP,Buf,Palette,x,y,aTriple)) then Result:=True;
     finally
       if Palette<>0 then DeleteObject(Palette);
       GlobalUnlock(HMem);
       GlobalFree(HMem);
     end;
 end;
 finally
   FreeMem(BIP, BIPSize);
 end;
end;

FUNCTION GetDIPixelTriple(BIP:PBitmapInfo;Buf:Pointer;aPalette:HPalette; x,y:Integer;var aTriple:TRGBTriple):Boolean;
Var P            : LongInt;
    pi           : Integer;
    Log          : TLogPalette;
begin
   Result:=False;
   if (BIP=nil)or(Buf=nil) then exit;
   with BIP^.bmiHeader do
   if (x >= 0)and(x<=biWidth-1)and
      (y >= 0)and(y<=biHeight-1)
   then
   with aTriple,BIP^.bmiHeader do
   begin
     case biBitCount of
       24:  begin
              p:= (biSizeImage div biHeight)*(biHeight-1-y)+(3*x);
              rgbtblue:=pbyteArray(buf)^[p];
              rgbtgreen:=pbyteArray(buf)^[p+1];
              rgbtred:=pbyteArray(buf)^[p+2];
             end;
       16:  begin
              p:= bmAlignDouble(biWidth*16)*(biHeight-1-y)+(2*x);
              rgbtRed:=((pbyteArray(buf)^[p+1] and $7C)shr 2)shl 3;
              rgbtBlue:=(pbyteArray(buf)^[p] AND $1F)shl 3;
              rgbtGreen:=(((pbyteArray(buf)^[p] and $E0)shr 5) +(pbyteArray(buf)^[p+1] and $03)shl 3) shl 3;
            end;
       8 :  begin
              p:= (biSizeImage div biHeight)*(biHeight-1-y)+x;
              pi:=pbyteArray(buf)^[p];
              GetPaletteEntries(aPalette,pi,1,Log.palpalEntry);
              with log.palpalentry[0],aTriple do
              begin          
                rgbtred   :=pered;
                rgbtGreen :=peGreen;
                rgbtBlue  :=peBlue;
              end;
            end;
     end;
     Result:=True;
   end;
end;


PROCEDURE GetSizes(HBM: HBITMAP; var BIPSize:Integer; var BufSize:DWord;var BitCount:Word;var ColorSize : Longint);
var BM        : Windows.TBitmap;
begin
   if GetObject(HBM, SizeOf(Windows.TBitmap), @BM) = 0 then Exit;
   getDibSizes(hbm,DWORD(BIPSize),BufSize);
   BitCount := bm.bmPlanes * bm.bmBitsPixel;
   if (BitCount = 16)or(BitCount = 32) then ColorSize := 0 else
      ColorSize := SizeOf(TRGBQuad) * (1 shl BitCount);
   BIPSize:= SizeOf(WINDOWS.TBitmapInfoHeader) + ColorSize;
end;

FUNCTION MyGetLogPalette(Var aPalette:HPalette;Var aLogPalette:PLogPalette;ReWrite:Boolean):Boolean;
var ScreenDC:HDC;
    Focus:HWND;
begin
  Result:=False;
  If (Not Rewrite)and (aLogPalette=nil) then exit;
  if (aLogPalette<>nil)and ReWrite then
  begin
    freemem(aLogPalette,(sizeof(TLogPalette)+sizeof(TPaletteEntry)*NumColors));
    aLogPalette:=nil;
  end;
  if aLogPalette=nil then
  Getmem(aLogPalette,(sizeof(TLogPalette)+sizeof(TPaletteEntry)*NumColors));
  try
    aLogPalette^.palVersion:=$300;
    aLogPalette^.palNumEntries:=NumColors;
    focus:=GetFocus;
    ScreenDC:=GetDC(Focus);
    if ReWrite then
    if aPalette=0 then
      GetSystemPaletteEntries(ScreenDC,0,256,aLogPalette^.palpalEntry)
    else GetPaletteEntries(aPalette,0,256,aLogPalette^.palpalEntry);
    releasedc(Focus,ScreenDC);
    if aPalette=0 then aPalette:=CreatePalette(aLogPalette^);
    Result:=True;
  except
    if aPalette<>0 then DeleteObject(aPalette);
    aPalette:=0;
    if (aLogPalette<>nil) then
    freemem(aLogPalette,(sizeof(TLogPalette)+sizeof(TPaletteEntry)*NumColors));
    aLogPalette:=nil;
  end;
end;


FUNCTION MyGetDIBits(HBM: HBITMAP; var BIP:PBitmapInfo; var Buf:Pointer; var aLogPalette:PLogPalette): Boolean;
  var
    DC:   HDC;			{ Drawing context }
    Focus:HWND;
    I:Integer;
 begin
   Result:=false;
   if BIP <> nil then InitBitmapInfoHeader(HBM,BIP^.bmiHeader);
   Focus:=GetFocus;
   DC := GetDC(Focus);
   if GetDIBits(DC, HBM, 0, BIP^.bmiHeader.biSizeImage, Buf, BIP^, dib_RGB_Colors) <> 0 then
   begin
     Result:=true;
     if (BIP^.bmiHeader.biBitCount=8)and PaletteInstalled then
     with BIP^ , aLogPalette^ do
     for I:=0 to 255 do
     begin
       bmiColors[I].rgbReserved:=pc_Reserved;
       bmiColors[I].rgbRed  :=palpalEntry[i].peRed;
       bmiColors[I].rgbGreen:=palpalEntry[i].peGreen;
       bmiColors[I].rgbBlue :=palpalEntry[i].peBlue;
     end;
   end;
   ReleaseDC(Focus, DC);
end;

FUNCTION MyInitDIBits(HBM: HBITMAP; var BIP:PBitmapInfo; var Buf:Pointer; var aLogPalette:PLogPalette): Boolean;
  var
    DC:   HDC;			{ Drawing context }
    Focus:HWND;
    I:Integer;
 begin
   Result:=False;
   try
     if BIP <> nil then InitBitmapInfoHeader(HBM,BIP^.bmiHeader);
     Focus:=GetFocus;
     DC := GetDC(Focus);
     if (BIP^.bmiHeader.biBitCount=8)and PaletteInstalled then
     with BIP^ , aLogPalette^ do
     for I:=0 to 255 do
     begin
       bmiColors[I].rgbReserved:=pc_Reserved;
       bmiColors[I].rgbRed  :=palpalEntry[i].peRed;
       bmiColors[I].rgbGreen:=palpalEntry[i].peGreen;
       bmiColors[I].rgbBlue :=palpalEntry[i].peBlue;
     end;
     ReleaseDC(Focus, DC);
     Result:=true;
   except end;
end;


end.
