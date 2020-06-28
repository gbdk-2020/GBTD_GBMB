unit GIFImage;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	GIF Graphics Object                                           //
// Module:	gifimage                                                      //
// Description:	TGraphic implementation of the GIF89a graphics format         //
// Version:	2.1                                                           //
// Release:	4                                                             //
// Date:	15-OCT-1998                                                   //
// Target:	Win32, Delphi 2, 3 & 4, C++ Builder 3	                      //
// Author(s):	anme: Anders Melander, anders@melander.dk                     //
//		fila: Filip Larsen                                            //
//		rps: Reinier Sterkenburg                                      //
// Copyright	(c) 1997,98 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// This software is copyrighted as noted above.  It may be freely copied,     //
// modified, and redistributed, provided that the copyright notice(s) is      //
// preserved on all copies.                                                   //
//                                                                            //
// TGIFImage is freeware and I would like it to remain so. This means that it //
// may not be bundled with commercial libraries or sold as shareware. You are //
// welcome to use it in commercial and shareware applications providing you   //
// do not charge for the functionality provided by TGIFImage.                 //
// If you are in doubt, please contact me and I will explain this.            //
//                                                                            //
// There is no warranty or other guarantee of fitness for this software, it   //
// is provided solely "as is".  Bug reports or fixes may be sent to the       //
// author, who may or may not act on them as he desires.                      //
//                                                                            //
// If you redistribute this code in binary form (i.e. as a library or linked  //
// into an application), the accompanying documentation should state that     //
// "this software is based, in part, on the work of Anders Melander" or words //
// to that effect.                                                            //
//                                                                            //
// If you modify this software, you should include a notice in the revision   //
// history in the history.txt file giving the date and the name of the person //
// performing the modification and a brief description of the modification.   //
//                                                                            //
// Please see the file copyright.txt for additional copyrights.               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
// Known problems:
//
// * The combination of buffered, tiled and transparent draw will display the
//   background incorrectly (scaled).
//
// * The combination of non-buffered and stretched draw is some times distorted
//   with a pattern effect.
//
// * Buffered display flickers when TGIFImage is used by a transparent TImage
//   component.
//   This is a problem with TImage caused by the fact that TImage was designed
//   with static images in mind. Not much I can do about it.
//
////////////////////////////////////////////////////////////////////////////////
// To do (in rough order of priority):
//
// * TImage hook for destroy notification.
//
// * TBitmap pool to limit resource consumption on Win95/98.
//
// * TImage.AssignTransparent method.
//
// * Modify GetDitherBitmap to use generic dithering engine.
//
// * Make BitsPerPixel property writable.
//
// * Optimized single global palette from multiple *external* palettes.
//
// * Import of bitmap(s) with color mapping to *external* palette.
//
// * Visual GIF component.
//
// * Easier method to determine DrawPainter status.
//
// * Import to 256+ color GIF.
//
// * Make some of TGIFImage's properties persistent (DrawOptions etc).
//   Add TGIFImage.Persistent property. Should save published properties
//   in application extension when this options is set. 
//
// * Solution for background buffering in scollbox.
//
// * Implement TGIFPainter support for:
//   Morphing
//   goValidateCanvas option
//   Source/Target canvas palette normalization
//
// * Optimize TGIFSubImage.Decompress
//
// * Implement TGIFPainter support for:
//   Interlaced display
//   Progressive display (piped load/decompress/display)
//
// * Implement TGIFPainter support for:
//   UserInput
//   Text extension
//
////////////////////////////////////////////////////////////////////////////////


interface
////////////////////////////////////////////////////////////////////////////////
//
//		Conditional Compiler Symbols
//
////////////////////////////////////////////////////////////////////////////////
(*
  DEBUG				Must be defined if any of the DEBUG_xxx
  				symbols are defined.
                                If the symbol is defined the source will not be
                                optimized and overflow- and range checks will be
                                enabled.

  DEBUG_HASHPERFORMANCE		Calculates hash table performance data.
  DEBUG_HASHFILLFACTOR		Calculates fill factor of hash table -
  				Interferes with DEBUG_HASHPERFORMANCE.
  DEBUG_COMPRESSPERFORMANCE	Calculates LZW compressor performance data.
  DEBUG_DECOMPRESSPERFORMANCE	Calculates LZW decompressor performance data.
  DEBUG_DITHERPERFORMANCE	Calculates color reduction performance data.
  DEBUG_DRAWPERFORMANCE		Calculates low level drawing performance data.
  				The performance data for DEBUG_DRAWPERFORMANCE
                                will be displayed when you press the Ctrl key.

  GIF_NOSAFETY			Define this symbol to disable overflow- and
				range checks.
                                Ignore if the DEBUG symbol is defined.

  STRICT_MOZILLA		Define to mimic Mozilla as closely as possible.
  				If not defined, a slightly more "optimal"
                                implementation is used (IMHO).

  FAST_AS_HELL			Define this symbol to use strictly GIF compliant
  				(but too fast) animation timing.
                                Since our paint routines are much faster and
                                more precise timed than Mozilla's, the standard
                                GIF and Mozilla values causes animations to loop
                                faster than they would in Mozilla.
                                If the symbol is _not_ defined, an alternative
                                set of tweaked timing values will be used.
                                The tweaked values are not optimal but are based
                                on tests performed on my reference system:
                                - Windows 95
                                - 133 MHz Pentium
                                - 64Mb RAM
                                - Diamond Stealth64/V3000
                                - 1600*1200 in 256 colors
                                The alternate values can be modified if you are
                                not satisfied with my defaults (they can be
                                found a few pages down).

  REGISTER_TGIFIMAGE            Define this symbol to register TGIFImage with
  				the TPicture class and integrate with TImage.
                                This is required to be able to display GIFs in
                                the TImage component.
                                The symbol is defined by default.
                                Undefine if you use another GIF library to
                                provide GIF support for TImage.

  PIXELFORMAT_TOO_SLOW		When this symbol is defined, the internal
  				PixelFormat routines are used in some places
                                instead of TBitmap.PixelFormat
                                The current implementation (Delphi4, Builder 3)
                                of TBitmap.PixelFormat can in some situation
                                degrade performance.
                                The symbol is defined by default.

*)

{$DEFINE REGISTER_TGIFIMAGE}
{$DEFINE PIXELFORMAT_TOO_SLOW}

////////////////////////////////////////////////////////////////////////////////
//
//		Determine Delphi and C++ Builder version
//
////////////////////////////////////////////////////////////////////////////////

// Delphi 1.x
{$IFDEF VER80}
  'Error: TGIFImage does not support Delphi 1.x'
{$ENDIF}

// Delphi 2.x
{$IFDEF VER90}
  {$DEFINE VER9x}
{$ENDIF}

// C++ Builder 1.x
{$IFDEF VER93}
  // Good luck...
  {$DEFINE VER9x}
{$ENDIF}

// Delphi 3.x
{$IFDEF VER100}
  {$DEFINE VER10x}
{$ENDIF}

// C++ Builder 3.x
{$IFDEF VER110}
  {$DEFINE VER10x}
  {$DEFINE VER11_PLUS}
  {$DEFINE D4_BCB3}
{$ENDIF}

// Delphi 4.x
{$IFDEF VER120}
  {$DEFINE VER10x}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER12_PLUS}
  {$DEFINE D4_BCB3}
{$ENDIF}

// Unknown compiler version - assume D3 compatible
{$IFNDEF VER9x}
  {$IFNDEF VER10x}
    {$DEFINE VER10x}
  {$ENDIF}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//		Compiler Options required to compile this library
//
////////////////////////////////////////////////////////////////////////////////
{$A+,B-,H+,J+,K-,M-,T-,X+}

// Debug control - You can safely change these settings
{$IFDEF DEBUG}
  {$C+}	// ASSERTIONS
  {$O-}	// OPTIMIZATION
  {$Q+}	// OVERFLOWCHECKS
  {$R+}	// RANGECHECKS
{$ELSE}
  {$C-}	// ASSERTIONS
  {$IFDEF GIF_NOSAFETY}
    {$Q-}// OVERFLOWCHECKS
    {$R-}// RANGECHECKS
  {$ENDIF}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			External dependecies
//
////////////////////////////////////////////////////////////////////////////////
uses
  sysutils,
  Windows,
  Graphics,
  Classes;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImage library version
//
////////////////////////////////////////////////////////////////////////////////
const
  GIFVersion		= $0201;
  GIFVersionMajor	= 2;
  GIFVersionMinor	= 1;
  GIFVersionRelease	= 4;

////////////////////////////////////////////////////////////////////////////////
//
//			Misc constants and support types
//
////////////////////////////////////////////////////////////////////////////////
const
  GIFMaxColors	= 256;			// Max number of colors supported by GIF
  					// Don't bother changing this value!

  BitmapAllocationThreshold = 500000;	// Bitmap pixel count limit at which
  					// a newly allocated bitmap will be
                                        // converted to 1 bit format before
                                        // being resized and converted to 8 bit.

var
{$IFDEF FAST_AS_HELL}
  GIFDelayExp: integer = 10;		// Delay multiplier in mS.
{$ELSE}
  GIFDelayExp: integer = 12;		// Delay multiplier in mS. Tweaked.
{$ENDIF}
					// * GIFDelayExp:
  					// The following delay values should all
                                        // be multiplied by this value to
                                        // calculate the effective time (in mS).
                                        // According to the GIF specs, this
                                        // value should be 10.
                                        // Since our paint routines are much
                                        // faster than Mozilla's, you might need
                                        // to increase this value if your
                                        // animations loops too fast. The
                                        // optimal value is impossible to
                                        // determine since it depends on the
                                        // speed of the CPU, the viceo card,
                                        // memory and many other factors.

  GIFDefaultDelay: integer = 10;	// * GIFDefaultDelay:
  					// Default animation delay.
  					// This value is used if no GCE is
                                        // defined.
                                        // (10 = 100 mS)

{$IFDEF FAST_AS_HELL}
  GIFMinimumDelay: integer = 1;		// Minimum delay (from Mozilla source).
  					// (1 = 10 mS)
{$ELSE}
  GIFMinimumDelay: integer = 3;		// Minimum delay - Tweaked.
{$ENDIF}
					// * GIFMinimumDelay:
					// The minumum delay used in the Mozilla
                                        // source is 10mS. This corresponds to a
                                        // value of 1. However, since our paint
                                        // routines are much faster than
                                        // Mozilla's, a value of 3 or 4 gives
                                        // better results.

  GIFMaximumDelay: integer = 1000;	// * GIFMaximumDelay:
  					// Maximum delay when painter is running
  					// in main thread (goAsync is not set).
                                        // This value guarantees that a very
                                        // long and slow GIF does not hang the
                                        // system.
                                        // (1000 = 10000 mS = 10 Seconds)

  MaxRenderThreads: integer = 4;	// Max # of concurrent rendering worker
  					// threads.
					// This number should probably be
                                        // dependant on the CPU speed...
                                        // If Win95/98 is detected, the value
                                        // is decreased in the initialization
                                        // section.

//  MaxRenderBatch: integer = 1;	// Max number of rendering jobs to
  					// submit at once for each GIF.
                                        // Not implemented!

//  MinRenderThreshold: integer = 2;	// Lower threshold at which to submit a
  					// new batch of rendering jobs.
                                        // Not implemented!

  MaxRenderTime: integer = 5000;	// Max # of mS to wait for completion of
  					// rendering.

  MaxRenderIdleTime: integer = 60000;	// Max # of idle mS to wait for
  					// rendering job before worker thread
                                        // is destroyed.

type
  TGIFVersion = (gvUnknown, gv87a, gv89a);
  TGIFVersionRec = array[0..2] of char;

const
  GIFVersions : array[gv87a..gv89a] of TGIFVersionRec = ('87a', '89a');

type
  // TGIFImage mostly throws exceptions of type GIFException
  GIFException = class(EInvalidGraphic);

  // Severity level as indicated in the Warning methods and the OnWarning event
  TGIFSeverity = (gsInfo, gsWarning, gsError);

////////////////////////////////////////////////////////////////////////////////
//
//			Delphi 2.x support
//
////////////////////////////////////////////////////////////////////////////////
{$IFDEF VER9x}
// Delphi 2 doesn't support TBitmap.PixelFormat
{$DEFINE PIXELFORMAT_TOO_SLOW}
type
  // TThreadList from Delphi 3 classes.pas
  TThreadList = class
  private
    FList: TList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
  end;

  // From Delphi 3 sysutils.pas
  EOutOfMemory = class(Exception);

  // From Delphi 3 classes.pas
  EOutOfResources = class(EOutOfMemory);

  // From Delphi 3 windows.pas
  PMaxLogPalette = ^TMaxLogPalette;
  TMaxLogPalette = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array [Byte] of TPaletteEntry;
  end; { TMaxLogPalette }

  // From Delphi 3 graphics.pas. Used by the D3 TGraphic class.
  TProgressStage = (psStarting, psRunning, psEnding);
  TProgressEvent = procedure (Sender: TObject; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string) of object;

  // From Delphi 3 windows.pas
  PRGBTriple = ^TRGBTriple;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Forward declarations
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFImage = class;
  TGIFSubImage = class;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFItem
//
////////////////////////////////////////////////////////////////////////////////
  TGIFItem = class(TPersistent)
  private
    FGIFImage: TGIFImage;
  protected
    function GetVersion: TGIFVersion; virtual;
    procedure Warning(Severity: TGIFSeverity; Message: string); virtual;
  public
    constructor Create(GIFImage: TGIFImage); virtual;

    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    property Version: TGIFVersion read GetVersion;
    property Image: TGIFImage read FGIFImage;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFList
//
////////////////////////////////////////////////////////////////////////////////
  TGIFList = class(TPersistent)
  private
    FItems: TList;
    FImage: TGIFImage;
  protected
    function GetItem(Index: Integer): TGIFItem;
    procedure SetItem(Index: Integer; Item: TGIFItem);
    function GetCount: Integer;
    procedure Warning(Severity: TGIFSeverity; Message: string); virtual;
  public
    constructor Create(Image: TGIFImage);
    destructor Destroy; override;

    function Add(Item: TGIFItem): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: TGIFItem;
    function IndexOf(Item: TGIFItem): Integer;
    procedure Insert(Index: Integer; Item: TGIFItem);
    function Last: TGIFItem;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TGIFItem): Integer;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream; Parent: TObject); virtual; abstract;

    property Items[Index: Integer]: TGIFItem read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property List: TList read FItems;
    property Image: TGIFImage read FImage;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFColorMap
//
////////////////////////////////////////////////////////////////////////////////
  // One way to do it:
  //  TBaseColor = (bcRed, bcGreen, bcBlue);
  //  TGIFColor = array[bcRed..bcBlue] of BYTE;
  // Another way:
  TGIFColor = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
  end;

  TColorMap = packed array[0..GIFMaxColors-1] of TGIFColor;
  PColorMap = ^TColorMap;

  TGIFColorMap = class(TPersistent)
  private
    FColorMap		: PColorMap;
    FCount		: integer;
    FCapacity		: integer;
    FOptimized		: boolean;
  protected
    function GetColor(Index: integer): TColor;
    procedure SetColor(Index: integer; Value: TColor);
    function GetBitsPerPixel: integer;
    function DoOptimize(Image: TGIFSubImage; CleanUp: boolean): boolean;
    procedure SetCapacity(Size: integer);
    procedure Warning(Severity: TGIFSeverity; Message: string); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    class function Color2RGB(Color: TColor): TGIFColor;
    class function RGB2Color(Color: TGIFColor): TColor;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream; Count: integer);
    procedure Assign(Source: TPersistent); override;
    function IndexOf(Color: TColor): integer;
    function Add(Color: TColor): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    function Optimize: boolean; virtual; abstract;
    procedure Changed; virtual; abstract;
    procedure ImportPalette(Palette: HPalette);
    procedure ImportColorTable(Pal: pointer; Count: integer);
    procedure ImportDIBColors(Handle: HDC);
    procedure ImportColorMap(Map: TColorMap; Count: integer);
    function ExportPalette: HPalette;
    property Colors[Index: integer]: TColor read GetColor write SetColor; default;
    property Data: PColorMap read FColorMap;
    property Count: integer read FCount;
    property Optimized: boolean read FOptimized write FOptimized;
    property BitsPerPixel: integer read GetBitsPerPixel;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFHeader
//
////////////////////////////////////////////////////////////////////////////////
  TLogicalScreenDescriptor = packed record
    ScreenWidth: word;              { logical screen width }
    ScreenHeight: word;             { logical screen height }
    PackedFields: byte;             { packed fields }
    BackgroundColorIndex: byte;     { index to global color table }
    AspectRatio: byte;              { actual ratio = (AspectRatio + 15) / 64 }
  end;

  TGIFHeader = class(TGIFItem)
  private
    FLogicalScreenDescriptor: TLogicalScreenDescriptor;
    FColorMap		: TGIFColorMap;
    procedure Prepare;
  protected
    function GetVersion: TGIFVersion; override;
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(Color: TColor);
    procedure SetBackgroundColorIndex(Index: BYTE);
    function GetBitsPerPixel: integer;
    function GetColorResolution: integer;
  public
    constructor Create(GIFImage: TGIFImage); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Version: TGIFVersion read GetVersion;
    property Width: WORD read FLogicalScreenDescriptor.ScreenWidth
                         write FLogicalScreenDescriptor.ScreenWidth;
    property Height: WORD read FLogicalScreenDescriptor.ScreenHeight
                          write FLogicalScreenDescriptor.Screenheight;
    property BackgroundColorIndex: BYTE read FLogicalScreenDescriptor.BackgroundColorIndex
                                        write SetBackgroundColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor
                                     write SetBackgroundColor;
    property AspectRatio: BYTE read FLogicalScreenDescriptor.AspectRatio
                               write FLogicalScreenDescriptor.AspectRatio;
    property ColorMap: TGIFColorMap read FColorMap;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property ColorResolution: integer read GetColorResolution;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFExtensionType = BYTE;
  TGIFExtension = class;
  TGIFExtensionClass = class of TGIFExtension;

  TGIFGraphicControlExtension = class;

  TGIFExtension = class(TGIFItem)
  private
    FSubImage: TGIFSubImage;
  protected
    function GetExtensionType: TGIFExtensionType; virtual; abstract;
    function GetVersion: TGIFVersion; override;
    function DoReadFromStream(Stream: TStream): TGIFExtensionType;
    class procedure RegisterExtension(elabel: BYTE; eClass: TGIFExtensionClass);
    class function FindExtension(Stream: TStream): TGIFExtensionClass;
    class function FindSubExtension(Stream: TStream): TGIFExtensionClass; virtual;
  public
     // Ignore D2 warning about hiding base class constructor
    constructor Create(ASubImage: TGIFSubImage); {$IFDEF VER12_PLUS} reintroduce; {$ENDIF} virtual; 
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property ExtensionType: TGIFExtensionType read GetExtensionType;
    property SubImage: TGIFSubImage read FSubImage;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFSubImage
//
////////////////////////////////////////////////////////////////////////////////
  TGIFExtensionList = class(TGIFList)
  protected
    function GetExtension(Index: Integer): TGIFExtension;
    procedure SetExtension(Index: Integer; Extension: TGIFExtension);
  public
    procedure LoadFromStream(Stream: TStream; Parent: TObject); override;
    property Extensions[Index: Integer]: TGIFExtension read GetExtension write SetExtension; default;
  end;

  TImageDescriptor = packed record
    Separator: byte;	{ fixed value of ImageSeparator }
    Left: word;		{ Column in pixels in respect to left edge of logical screen }
    Top: word;		{ row in pixels in respect to top of logical screen }
    Width: word;	{ width of image in pixels }
    Height: word;	{ height of image in pixels }
    PackedFields: byte;	{ Bit fields }
  end;

  TGIFSubImage = class(TGIFItem)
  private
    FBitmap		: TBitmap;
    FMask		: HBitmap;
    FNeedMask		: boolean;
    FLocalPalette	: HPalette;
    FData		: PChar;
    FDataSize		: integer;
    FColorMap		: TGIFColorMap;
    FImageDescriptor	: TImageDescriptor;
    FExtensions		: TGIFExtensionList;
    FTransparent	: boolean;
    FGCE		: TGIFGraphicControlExtension;
    procedure Prepare;
    procedure Compress(Stream: TStream);
    procedure Decompress(Stream: TStream);
  protected
    function GetVersion: TGIFVersion; override;
    function GetInterlaced: boolean;
    procedure SetInterlaced(Value: boolean);
    function GetColorResolution: integer;
    function GetBitsPerPixel: integer;
    procedure AssignTo(Dest: TPersistent); override;
    function DoGetBitmap: TBitmap;
    function DoGetDitherBitmap: TBitmap;
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure FreeBitmap;
    procedure FreeMask;
    function GetEmpty: Boolean;
    function GetPalette: HPALETTE;
    procedure SetPalette(Value: HPalette);
    function GetActiveColorMap: TGIFColorMap;
    function GetBoundsRect: TRect;
    function GetClientRect: TRect;
    function GetPixel(x, y: integer): BYTE;
    procedure NewBitmap;
    procedure NewImage;
    function ScaleRect(DestRect: TRect): TRect;
    function HasMask: boolean;
    function GetBounds(Index: integer): WORD;
    procedure SetBounds(Index: integer; Value: WORD);
  public
    constructor Create(GIFImage: TGIFImage); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromFile(const Filename: string); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect;
      DoTransparent, DoTile: boolean);
    procedure StretchDraw(ACanvas: TCanvas; const Rect: TRect;
      DoTransparent, DoTile: boolean);
    function HasBitmap: boolean;
    property Left: WORD index 1 read GetBounds write SetBounds;
    property Top: WORD index 2 read GetBounds write SetBounds;
    property Width: WORD index 3 read GetBounds write SetBounds;
    property Height: WORD index 4 read GetBounds write SetBounds;
    property BoundsRect: TRect read GetBoundsRect;
    property ClientRect: TRect read GetClientRect;
    property Interlaced: boolean read GetInterlaced write SetInterlaced;
    property ColorMap: TGIFColorMap read FColorMap;
    property ActiveColorMap: TGIFColorMap read GetActiveColorMap;
    property Data: PChar read FData;
    property DataSize: integer read FDataSize;
    property Extensions: TGIFExtensionList read FExtensions;
    property Version: TGIFVersion read GetVersion;
    property ColorResolution: integer read GetColorResolution;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Mask: HBitmap read FMask;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property Empty: boolean read GetEmpty;
    property Transparent: boolean read FTransparent;
    property GraphicControlExtension: TGIFGraphicControlExtension read FGCE;
    property Pixels[x, y: integer]: BYTE read GetPixel;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFTrailer
//
////////////////////////////////////////////////////////////////////////////////
  TGIFTrailer = class(TGIFItem)
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFGraphicControlExtension
//
////////////////////////////////////////////////////////////////////////////////
  // Graphic Control Extension block a.k.a GCE
  TGIFGCERec = packed record
    BlockSize: byte;         { should be 4 }
    PackedFields: Byte;
    DelayTime: Word;         { in centiseconds }
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

  TDisposalMethod = (dmNone, dmNoDisposal, dmBackground, dmPrevious);

  TGIFGraphicControlExtension = class(TGIFExtension)
  private
    FGCExtension: TGIFGCERec;
  protected
    function GetExtensionType: TGIFExtensionType; override;
    function GetTransparent: boolean;
    procedure SetTransparent(Value: boolean);
    function GetTransparentColor: TColor;
    procedure SetTransparentColor(Color: TColor);
    function GetTransparentColorIndex: BYTE;
    procedure SetTransparentColorIndex(Value: BYTE);
    function GetDelay: WORD;
    procedure SetDelay(Value: WORD);
    function GetUserInput: boolean;
    procedure SetUserInput(Value: boolean);
    function GetDisposal: TDisposalMethod;
    procedure SetDisposal(Value: TDisposalMethod);

  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Delay: WORD read GetDelay write SetDelay;
    property Transparent: boolean read GetTransparent write SetTransparent;
    property TransparentColorIndex: BYTE read GetTransparentColorIndex
                                            write SetTransparentColorIndex;
    property TransparentColor: TColor read GetTransparentColor write SetTransparentColor;
    property UserInput: boolean read GetUserInput write SetUserInput;
    property Disposal: TDisposalMethod read GetDisposal write SetDisposal;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFTextExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFPlainTextExtensionRec = packed record
    BlockSize: byte;         { should be 12 }
    Left, Top, Width, Height: Word;
    CellWidth, CellHeight: Byte;
    TextFGColorIndex,
    TextBGColorIndex: Byte;
  end;

  TGIFTextExtension = class(TGIFExtension)
  private
    FText		: TStrings;
    FPlainTextExtension	: TGIFPlainTextExtensionRec;
  protected
    function GetExtensionType: TGIFExtensionType; override;
    function GetForegroundColor: TColor;
    procedure SetForegroundColor(Color: TColor);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(Color: TColor);
    function GetBounds(Index: integer): WORD;
    procedure SetBounds(Index: integer; Value: WORD);
    function GetCharWidthHeight(Index: integer): BYTE;
    procedure SetCharWidthHeight(Index: integer; Value: BYTE);
    function GetColorIndex(Index: integer): BYTE;
    procedure SetColorIndex(Index: integer; Value: BYTE);
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Left: WORD index 1 read GetBounds write SetBounds;
    property Top: WORD index 2 read GetBounds write SetBounds;
    property GridWidth: WORD index 3 read GetBounds write SetBounds;
    property GridHeight: WORD index 4 read GetBounds write SetBounds;
    property CharWidth: BYTE index 1 read GetCharWidthHeight write SetCharWidthHeight;
    property CharHeight: BYTE index 2 read GetCharWidthHeight write SetCharWidthHeight;
    property ForegroundColorIndex: BYTE index 1 read GetColorIndex write SetColorIndex;
    property ForegroundColor: TColor read GetForegroundColor;
    property BackgroundColorIndex: BYTE  index 2 read GetColorIndex write SetColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor;
    property Text: TStrings read FText write FText;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFCommentExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFCommentExtension = class(TGIFExtension)
  private
    FText		: TStrings;
  protected
    function GetExtensionType: TGIFExtensionType; override;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Text: TStrings read FText;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFApplicationExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFIdentifierCode = array[0..7] of char;
  TGIFAuthenticationCode = array[0..2] of char;
  TGIFApplicationRec = packed record
    Identifier: TGIFIdentifierCode;
    Authentication: TGIFAuthenticationCode;
  end;

  TGIFApplicationExtension = class;
  TGIFAppExtensionClass = class of TGIFApplicationExtension;

  TGIFApplicationExtension = class(TGIFExtension)
  private
    FIdent		: TGIFApplicationRec;
  protected
    function GetExtensionType: TGIFExtensionType; override;
    procedure SaveData(Stream: TStream); virtual; abstract;
    procedure LoadData(Stream: TStream); virtual; abstract;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    class procedure RegisterExtension(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
    class function FindSubExtension(Stream: TStream): TGIFExtensionClass; override;
    property Identifier: TGIFIdentifierCode read FIdent.Identifier
      write FIdent.Identifier;
    property Authentication: TGIFAuthenticationCode read FIdent.Authentication
      write FIdent.Authentication;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFUnknownAppExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFBlock = class(TObject)
  private
    FSize		: BYTE;
    FData		: pointer;
  public
    constructor Create(ASize: integer);
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Size: BYTE read FSize;
    property Data: pointer read FData;
  end;

  TGIFUnknownAppExtension = class(TGIFApplicationExtension)
  private
    FBlocks		: TList;
  protected
    procedure SaveData(Stream: TStream); override;
    procedure LoadData(Stream: TStream); override;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    property Blocks: TList read FBlocks;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFAppExtNSLoop
//
////////////////////////////////////////////////////////////////////////////////
  TGIFAppExtNSLoop = class(TGIFApplicationExtension)
  private
    FLoops		: WORD;
  protected
    procedure SaveData(Stream: TStream); override;
    procedure LoadData(Stream: TStream); override;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    property Loops: WORD read FLoops write FLoops;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFImage
//
////////////////////////////////////////////////////////////////////////////////
  TGIFImageList = class(TGIFList)
  protected
    function GetImage(Index: Integer): TGIFSubImage;
    procedure SetImage(Index: Integer; SubImage: TGIFSubImage);
  public
    procedure LoadFromStream(Stream: TStream; Parent: TObject); override;
    procedure SaveToStream(Stream: TStream); override;
    property SubImages[Index: Integer]: TGIFSubImage read GetImage write SetImage; default;
  end;

  TGIFCompression =
    (gcLZW,			// Normal LZW compression
     gcRLE			// GIF compatible RLE compression
    );

  // Color reduction methods
  TColorReduction =
    (rmNone,			// Do not perform color reduction
     rmWindows20,		// Reduce to the Windows 20 color system palette
     rmWindows256,		// Reduce to the Windows 256 color halftone palette (Only works in 256 color display mode)
     rmWindowsGray,		// Reduce to the Windows 4 grayscale colors
     rmMonochrome,		// Reduce to a black/white monochrome palette
     rmGrayScale,		// Reduce to a uniform 256 shade grayscale palette
     rmNetscape,		// Reduce to the Netscape 216 color palette
     rmQuantize,		// Reduce to optimal 2^n color palette
     rmQuantizeWindows		// Reduce to optimal 256 color windows palette
    );
  TDitherMode =
    (dmNearest,			// Nearest color matching w/o error correction
     dmFloydSteinberg		// Floyd Steinberg Error Diffusion dithering
     // dmOrdered,		// Ordered dither
     // dmCustom		// Custom palette
    );

  TGIFDrawOption =
    (goAsync,			// Asyncronous draws (paint in thread)
     goTransparent,		// Transparent draws
     goAnimate,			// Animate draws
     goLoop,			// Loop animations
     goLoopContinously,		// Ignore loop count and loop forever
     goValidateCanvas,		// Validate canvas in threaded paint ***NOT IMPLEMENTED***
     goDirectDraw,		// Draw() directly on canvas
     goClearOnLoop,		// Clear animation on loop
     goTile,			// Tiled display
     goDither,			// Dither to Netscape palette
     goAutoDither		// Only dither on 256 color systems
    );
  TGIFDrawOptions = set of TGIFDrawOption;
  // Note: if goAsync is not set then goDirectDraw should be set. Otherwise
  // the image will not be displayed.

  PGIFPainter = ^TGIFPainter;

  TGIFPainter = class(TThread)
  private
    FImage		: TGIFImage;	// The TGIFImage that owns this painter
    FCanvas		: TCanvas;	// Destination canvas
    FRect		: TRect;	// Destination rect
    FDrawOptions	: TGIFDrawOptions;// Paint options
    FAnimationSpeed	: integer;	// Animation speed %
    FActiveImage	: integer;	// Current frame
    Disposal		,		// Used by synchronized paint
    OldDisposal		: TDisposalMethod;// Used by synchronized paint
    BackupBuffer	: TBitmap;	// Used by synchronized paint
    FrameBuffer		: TBitmap;	// Used by synchronized paint
    Background		: TBitmap;	// Used by synchronized paint
    ValidateDC		: HDC;
    DoRestart		: boolean;	// Flag used to restart animation
    FStarted		: boolean;	// Flag used to signal start of paint
    PainterRef		: PGIFPainter;	// Pointer to var referencing painter
    FEventHandle	: THandle;	// Animation delay event
    ExceptObject	: Exception;	// Eaten exception
    ExceptAddress	: pointer;	// Eaten exceptions address
    FEvent		: TNotifyEvent;	// Used by synchronized events
    FOnStartPaint	: TNotifyEvent;
    FOnPaint		: TNotifyEvent;
    FOnLoop		: TNotifyEvent;
    FOnEndPaint		: TNotifyEvent;
    procedure DoOnTerminate(Sender: TObject);// Sync. shutdown procedure
    procedure DoSynchronize(Method: TThreadMethod);// Conditional sync stub
    procedure PrefetchBitmap;		// Sync. bitmap prefetch
    procedure DoPaintFrame;		// Sync. buffered paint procedure
    procedure DoPaint;			// Sync. paint procedure
    procedure DoEvent;
    procedure SetActiveImage(const Value: integer);			// Sync. event procedure
  protected
    procedure Execute; override;
    procedure SetAnimationSpeed(Value: integer);
  public
    constructor Create(AImage: TGIFImage; ACanvas: TCanvas; ARect: TRect;
      Options: TGIFDrawOptions);
    constructor CreateRef(Painter: PGIFPainter; AImage: TGIFImage; ACanvas: TCanvas; ARect: TRect;
      Options: TGIFDrawOptions);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Restart;
    property Image: TGIFImage read FImage;
    property Canvas: TCanvas read FCanvas;
    property Rect: TRect read FRect write FRect;
    property DrawOptions: TGIFDrawOptions read FDrawOptions write FDrawOptions;
    property AnimationSpeed: integer read FAnimationSpeed write SetAnimationSpeed;
    property Started: boolean read FStarted;
    property ActiveImage: integer read FActiveImage write SetActiveImage;
    property OnStartPaint: TNotifyEvent read FOnStartPaint write FOnStartPaint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnEndPaint	: TNotifyEvent read FOnEndPaint	 write FOnEndPaint	;
    property EventHandle: THandle read FEventHandle;
  end;

  TGIFWarning = procedure(Sender: TObject; Severity: TGIFSeverity; Message: string) of object;

  TGIFImage = class(TGraphic)
  private
    IsDrawing		: Boolean;
    IsInsideGetPalette	: boolean;
    FImages		: TGIFImageList;
    FHeader		: TGIFHeader;
    FGlobalPalette	: HPalette;
    FPainters		: TThreadList;
    FDrawOptions	: TGIFDrawOptions;
    FColorReduction	: TColorReduction;
    FReductionBits	: integer;
    FDitherMode		: TDitherMode;
    FCompression	: TGIFCompression;
    FOnWarning		: TGIFWarning;
    FBitmap		: TBitmap;
    FDrawPainter	: TGIFPainter;
    FThreadPriority	: TThreadPriority;
    FAnimationSpeed	: integer;
    FDrawBackgroundColor: TColor;
    FOnStartPaint	: TNotifyEvent;
    FOnPaint		: TNotifyEvent;
    FOnLoop		: TNotifyEvent;
    FOnEndPaint		: TNotifyEvent;
{$IFDEF VER9x}
    FPaletteModified	: Boolean;
    FOnProgress		: TProgressEvent;
{$ENDIF}
  protected
    // procedure Changed(Sender: TObject); {$IFDEF VER9x} virtual; {$ELSE} override; {$ENDIF}
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
    function InternalPaint(Painter: PGIFPainter; ACanvas: TCanvas; const Rect: TRect; Options: TGIFDrawOptions): TGIFPainter;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetPalette: HPALETTE; {$IFDEF VER9x} virtual; {$ELSE} override; {$ENDIF}
    procedure SetPalette(Value: HPalette); {$IFDEF VER9x} virtual; {$ELSE} override; {$ENDIF}
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetVersion: TGIFVersion;
    function GetColorResolution: integer;
    function GetBitsPerPixel: integer;
    function GetBackgroundColorIndex: BYTE;
    function GetBackgroundColor: TColor;
    procedure SetDrawOptions(Value: TGIFDrawOptions);
    procedure SetAnimationSpeed(Value: integer);
    procedure SetReductionBits(Value: integer);
    procedure NewImage;
    function GetBitmap: TBitmap;
    function NewBitmap: TBitmap;
    procedure FreeBitmap;
    function GetColorMap: TGIFColorMap;
    function GetDoDither: boolean;
    property DrawPainter: TGIFPainter read FDrawPainter; // Extremely volatile
    property DoDither: boolean read GetDoDither;
{$IFDEF VER9x}
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    function Add(Source: TPersistent): integer;
    procedure Pack;
    procedure OptimizeColorMap;
    procedure Clear;
    procedure StopDraw;
    function Paint(ACanvas: TCanvas; const Rect: TRect; Options: TGIFDrawOptions): TGIFPainter;
    procedure PaintStart;
    procedure PaintPause;
    procedure PaintStop;
    procedure PaintResume;
    procedure PaintRestart;
    procedure Warning(Sender: TObject; Severity: TGIFSeverity; Message: string); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    property GlobalColorMap: TGIFColorMap read GetColorMap;
    property Version: TGIFVersion read GetVersion;
    property Images: TGIFImageList read FImages;
    property ColorResolution: integer read GetColorResolution;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property BackgroundColorIndex: BYTE read GetBackgroundColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor;
    property Header: TGIFHeader read FHeader;
    property DrawOptions: TGIFDrawOptions read FDrawOptions write SetDrawOptions;
    property DrawBackgroundColor: TColor read FDrawBackgroundColor write FDrawBackgroundColor;
    property ColorReduction: TColorReduction read FColorReduction write FColorReduction;
    property ReductionBits: integer read FReductionBits write SetReductionBits;
    property DitherMode: TDitherMode read FDitherMode write FDitherMode;
    property Compression: TGIFCompression read FCompression write FCompression;
    property AnimationSpeed: integer read FAnimationSpeed write SetAnimationSpeed;
    property Painters: TThreadList read FPainters;
    property ThreadPriority: TThreadPriority read FThreadPriority write FThreadPriority;
    property Bitmap: TBitmap read GetBitmap; // Volatile - beware!
    property OnWarning: TGIFWarning read FOnWarning write FOnWarning;
    property OnStartPaint: TNotifyEvent read FOnStartPaint write FOnStartPaint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnEndPaint	: TNotifyEvent read FOnEndPaint	 write FOnEndPaint	;
{$IFDEF VER9x}
    property Palette: HPALETTE read GetPalette write SetPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
{$ENDIF}
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      Utility routines
//
////////////////////////////////////////////////////////////////////////////////
  // WebPalette creates a 216 color uniform palette a.k.a. the Netscape Palette
  function WebPalette: HPalette;

  // ReduceColors
  // Map colors in a bitmap to their nearest representation in a palette using
  // the methods specified by the ColorReduction and DitherMode parameters.
  // The ReductionBits parameter specifies the desired number of colors (bits
  // per pixel) when the reduction method is rmQuantize.
  function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
    DitherMode: TDitherMode; ReductionBits: integer): TBitmap;

////////////////////////////////////////////////////////////////////////////////
//
//                      Global variables
//
////////////////////////////////////////////////////////////////////////////////
// GIF Clipboard format identifier for use by LoadFromClipboardFormat and
// SaveToClipboardFormat.
// Set in Initialization section.
var
  CF_GIF: WORD;

////////////////////////////////////////////////////////////////////////////////
//
//                      Library defaults
//
////////////////////////////////////////////////////////////////////////////////
var
  // Default options for TGIFImage.DrawOptions.
  GIFImageDefaultDrawOptions : TGIFDrawOptions =
    [goAsync, goLoop, goTransparent, goAnimate, goDither, goAutoDither
{$IFDEF STRICT_MOZILLA}
     ,goClearOnLoop
{$ENDIF}
    ];

  // WARNING! Do not use goAsync and goDirectDraw unless you have absolute
  // control of the destination canvas.
  // TGIFPainter will continue to write on the canvas even after the canvas has
  // been deleted, unless *you* prevent it.
  // The goValidateCanvas option will fix this problem if it is ever implemented.

  // Default color reduction methods for bitmap import.
  // These are the fastest settings, but also the ones that gives the
  // worst result (in most cases).
  GIFImageDefaultColorReduction: TColorReduction = rmNetscape;
  GIFImageDefaultColorReductionBits: integer = 8; // Range 3 - 8
  GIFImageDefaultDitherMode: TDitherMode = dmNearest;

  // Default encoder compression method.
  GIFImageDefaultCompression: TGIFCompression = gcLZW;

  // Default painter thread priority
  GIFImageDefaultThreadPriority: TThreadPriority = tpNormal;

  // Default animation speed in % of normal speed (range 0 - 1000)
  GIFImageDefaultAnimationSpeed: integer = 100;

  // DoAutoDither is set to True in the initializaion section if the desktop DC
  // supports 256 colors or less.
  // It can be modified in your application to disable/enable Auto Dithering
  DoAutoDither: boolean = False;

  // Palette is set to True in the initializaion section if the desktop DC
  // supports 256 colors or less.
  // You should NOT modify it.
  PaletteDevice: boolean = False;

////////////////////////////////////////////////////////////////////////////////
//
//                      Design Time support
//
////////////////////////////////////////////////////////////////////////////////
// Dummy component registration for design time support of GIFs in TImage
procedure Register;

////////////////////////////////////////////////////////////////////////////////
//
//                      Error messages
//
////////////////////////////////////////////////////////////////////////////////
{$ifndef VER9x}
resourcestring
{$else}
const
{$endif}
  // GIF Error messages
  sOutOfData		= 'Premature end of data';
  sTooManyColors	= 'Color table overflow';
  sBadColorIndex	= 'Invalid color index';
  sBadVersion		= 'Unsupported GIF version';
  sBadSignature		= 'Invalid GIF signature';
  sScreenBadColorSize	= 'Invalid number of colors specified in Screen Descriptor';
  sImageBadColorSize	= 'Invalid number of colors specified in Image Descriptor';
  sUnknownExtension	= 'Unknown extension type';
  sBadExtensionLabel	= 'Invalid extension introducer';
  sOutOfMemDIB		= 'Failed to allocate memory for GIF DIB';
  sDIBCreate		= 'Failed to create DIB from Bitmap';
  sDecodeTooFewBits	= 'Decoder bit buffer under-run';
  sDecodeCircular	= 'Circular decoder table entry';
  sBadTrailer		= 'Invalid Image trailer';
  sBadExtensionInstance	= 'Internal error: Extension Instance does not match Extension Label';
  sBadBlockSize		= 'Unsupported Application Extension block size';
  sBadBlock		= 'Unknown GIF block type';
  sUnsupportedClass	= 'Object type not supported for operation';
  sInvalidData		= 'Invalid GIF data';
  sBadHeight		= 'Image height too small for contained frames';
  sBadWidth		= 'Image width too small for contained frames';
{$IFNDEF REGISTER_TGIFIMAGE}
  sGIFToClipboard	= 'Clipboard operations not supported for GIF objects';
{$ENDIF}
  sScreenSizeExceeded	= 'Image exceeds Logical Screen size';
  sNoColorTable		= 'No global or local color table defined';
  sBadPixelCoordinates	= 'Invalid pixel coordinates';
{$IFDEF VER9x}
  sUnsupportedBitmap	= 'Unsupported bitmap format';
{$ENDIF}
  sInvalidPixelFormat	= 'Unsupported PixelFormat';
  sBadDimension		= 'Invalid image dimensions'; // Obsolete
  sNoDIB		= 'Image has no DIB';
  sInvalidStream	= 'Invalid stream operation';
  sInvalidColor		= 'Color not in color table';
  sInvalidBitSize	= 'Invalid Bits Per Pixel value';
  sEmptyColorMap	= 'Color table is empty';
{$IFDEF VER9x}
  // From Delphi 3 consts.pas
  SOutOfResources	= 'Out of system resources';
  SInvalidBitmap	= 'Bitmap image is not valid';
  SScanLine		= 'Scan line index out of range';
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//                      Misc texts
//
////////////////////////////////////////////////////////////////////////////////
  // File filter name
  sGIFImageFile		= 'GIF Image';

  // Progress messages
  sProgressLoading	= 'Loading...';
  sProgressSaving	= 'Saving...';
  sProgressConverting	= 'Converting...';
  sProgressRendering	= 'Rendering...';
  sProgressCopying	= 'Copying...';


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//			Implementation
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

{ This makes me long for the C preprocessor... }
{$ifdef DEBUG}
  {$ifdef DEBUG_COMPRESSPERFORMANCE}
    {$define DEBUG_PERFORMANCE}
  {$else}
    {$ifdef DEBUG_DECOMPRESSPERFORMANCE}
      {$define DEBUG_PERFORMANCE}
    {$else}
      {$ifdef DEBUG_DITHERPERFORMANCE}
        {$define DEBUG_PERFORMANCE}
      {$else}
        {$ifdef DEBUG_DITHERPERFORMANCE}
          {$define DEBUG_PERFORMANCE}
        {$else}
          {$ifdef DEBUG_DRAWPERFORMANCE}
            {$define DEBUG_PERFORMANCE}
          {$endif}
        {$endif}
      {$endif}
    {$endif}
  {$endif}
{$endif}

uses
{$ifdef DEBUG}
  dialogs,
{$endif}
  mmsystem, // timeGetTime()
  messages,
  Consts;


////////////////////////////////////////////////////////////////////////////////
//
//			Misc consts
//
////////////////////////////////////////////////////////////////////////////////
const
  { Extension/block label values }
  bsPlainTextExtension		= $01;
  bsGraphicControlExtension	= $F9;
  bsCommentExtension		= $FE;
  bsApplicationExtension	= $FF;

  bsImageDescriptor		= Ord(',');
  bsExtensionIntroducer		= Ord('!');
  bsTrailer			= ord(';');

  // Thread messages - Used by TThread.Synchronize()
  CM_DESTROYWINDOW	= $8FFE; // Defined in classes.pas
  CM_EXECPROC 		= $8FFF; // Defined in classes.pas


////////////////////////////////////////////////////////////////////////////////
//
//                      Design Time support
//
////////////////////////////////////////////////////////////////////////////////
procedure Register;
begin
  // Dummy component registration to add design-time support of GIFs to TImage
  // Since TGIFImage isn't a component there's nothing to register here, but
  // since Register is only called at design time we can set the design time
  // GIF paint options here (modify as you please):

  // Don't loop animations at design-time. Animated GIFs will animate once and
  // then stop thus not using CPU resources and distracting the developer.
  Exclude(GIFImageDefaultDrawOptions, goLoop);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Utilities
//
////////////////////////////////////////////////////////////////////////////////

function WebPalette: HPalette;
type
  TLogWebPalette	= packed record
    palVersion		: word;
    palNumEntries	: word;
    PalEntries		: array[0..5,0..5,0..5] of TPaletteEntry;
  end;
var
  r, g, b		: byte;
  LogWebPalette		: TLogWebPalette;
  LogPalette		: TLogpalette absolute LogWebPalette; // Stupid typecast
begin
  with LogWebPalette do
  begin
    palVersion:= $0300;
    palNumEntries:= 216;
    for r:=0 to 5 do
      for g:=0 to 5 do
        for b:=0 to 5 do
        begin
          with PalEntries[r,g,b] do
          begin
            peRed := 51 * r;
            peGreen := 51 * g;
            peBlue := 51 * b;
            peFlags := 0;
          end;
        end;
  end;
  Result := CreatePalette(Logpalette);
end;

(*
**  GDI Error handling
**  Adapted from graphics.pas
*)
{$ifdef BCB}
function GDICheck(Value: Integer): Integer;
{$else}
function GDICheck(Value: Cardinal): Cardinal;
{$endif}
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;

  function ReturnAddr: Pointer;
  // From classes.pas
  asm
    MOV		EAX,[EBP+4] // sysutils.pas says [EBP-4], but this works !
  end;

begin
  if (Value = 0) then
  begin
    ErrorCode := GetLastError;
    if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
      ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
      raise EOutOfResources.Create(Buf) at ReturnAddr
    else
      raise EOutOfResources.Create(SOutOfResources) at ReturnAddr;
  end;
  Result := Value;
end;

(*
**  Raise error condition
*)
procedure Error(msg: string);
  function ReturnAddr: Pointer;
  // From classes.pas
  asm
    MOV		EAX,[EBP+4] // sysutils.pas says [EBP-4] !
  end;
begin
  raise GIFException.Create(msg) at ReturnAddr;
end;

(*
**  Return number bytes required to
**  hold a given number of bits.
*)
function ByteAlignBit(Bits: Cardinal): Cardinal;
begin
  Result := (Bits+7) SHR 3;
end;
// Rounded up to nearest 2
function WordAlignBit(Bits: Cardinal): Cardinal;
begin
  Result := ((Bits+15) SHR 4) SHL 1;
end;
// Rounded up to nearest 4
function DWordAlignBit(Bits: Cardinal): Cardinal;
begin
  Result := ((Bits+31) SHR 5) SHL 2;
end;
// Round to arbitrary number of bits
function AlignBit(Bits, BitsPerPixel, Alignment: Cardinal): Cardinal;
begin
  Dec(Alignment);
  Result := ((Bits * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result SHR 3;
end;

(*
**  Compute Bits per Pixel from Number of Colors
**  (Return the ceiling log of n)
*)
function Colors2bpp(val: integer): integer;
var
  i			: integer;
begin
  (*
  ** This might be faster computed by multiple if then else statements
  *)
  if (val = 0) then
    Result := 0
  else
  begin
    for i := 1 to 8 do
      if (val <= (1 SHL i)) then
      begin
        Result := i;
        exit;
      end;
    Result := 8;
  end;
end;

(*
**  Write an ordinal byte value to a stream
*)
procedure WriteByte(Stream: TStream; b: BYTE);
begin
  Stream.Write(b, 1);
end;

(*
**  Read an ordinal byte value from a stream
*)
function ReadByte(Stream: TStream): BYTE;
begin
  Stream.Read(Result, 1);
end;

(*
**  Read data from stream and raise exception of EOF
*)
procedure ReadCheck(Stream: TStream; var Buffer; Size: LongInt);
var
  ReadSize		: integer;
begin
  ReadSize := Stream.Read(Buffer, Size);
  if (ReadSize <> Size) then
    Error(sOutOfData);
end;

(*
**  Write a string list to a stream as multiple blocks
**  of max 255 characters in each.
*)
procedure WriteStrings(Stream: TStream; Text: TStrings);
var
  i			: integer;
  b			: BYTE;
  size			: integer;
  s			: string;
begin
  for i := 0 to Text.Count-1 do
  begin
    s := Text[i];
    size := length(s);
    if (size > 255) then
      b := 255
    else
      b := size;
    while (size > 0) do
    begin
      dec(size, b);
      WriteByte(Stream, b);
      Stream.Write(PChar(s)^, b);
      delete(s, 1, b);
      if (b > size) then
        b := size;
    end;
  end;
  // Terminating zero (length = 0)
  WriteByte(Stream, 0);
end;


(*
**  Read a string list from a stream as multiple blocks
**  of max 255 characters in each.
**  ***FIXME*** Replace with TGIFReader
*)
procedure ReadStrings(Stream: TStream; Text: TStrings);
var
  size			: BYTE;
  buf			: array[0..255] of char;
begin
  Text.Clear;
  if (Stream.Read(size, 1) <> 1) then
    exit;
  while (size > 0) do
  begin
    ReadCheck(Stream, buf, size);
    buf[size] := #0;
    Text.Add(Buf);
    if (Stream.Read(size, 1) <> 1) then
      exit;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//		Delphi 2.x / C++ Builder 1.x support
//
////////////////////////////////////////////////////////////////////////////////
{$IFDEF VER9x}
var
  // From Delphi 3 graphics.pas
  SystemPalette16: HPalette; // 16 color palette that maps to the system palette
{$ENDIF}

type
{$IFDEF VER9x}
  // From Delphi 3 graphics.pas
  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);
{$ENDIF}
  TPixelFormats = set of TPixelFormat;

const
  // Only pf1bit, pf4bit and pf8bit is supported since they are the only ones
  // with palettes
  SupportedPixelformats: TPixelFormats = [pf1bit, pf4bit, pf8bit];


// --------------------------
// InitializeBitmapInfoHeader
// --------------------------
// Fills a TBitmapInfoHeader with the values of a bitmap when converted to a
// DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Info		The TBitmapInfoHeader buffer that will receive the values.
// PixelFormat	The pixel format of the destination DIB.
//
{$IFDEF D4_BCB3}
  // Disable optimization to circumvent D4/BCB3 optimizer bug
  {$IFOPT O+}
    {$DEFINE O_PLUS}
    {$O-}
  {$ENDIF}
{$ENDIF}
procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var Info: TBitmapInfoHeader;
  PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  DIB		: TDIBSection;
  Bytes		: Integer;
begin
  DIB.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DIB), @DIB);
  if (Bytes = 0) then
    Error(sInvalidBitmap);

  if (Bytes >= (sizeof(DIB.dsbm) + sizeof(DIB.dsbmih))) and
    (DIB.dsbmih.biSize >= sizeof(DIB.dsbmih)) then
    Info := DIB.dsbmih
  else
  begin
    FillChar(Info, sizeof(Info), 0);
    with Info, DIB.dsbm do
    begin
      biSize := SizeOf(Info);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case PixelFormat of
    pf1bit: Info.biBitCount := 1;
    pf4bit: Info.biBitCount := 4;
    pf8bit: Info.biBitCount := 8;
    pf24bit: Info.biBitCount := 24;
  else
    Error(sInvalidPixelFormat);
    // Info.biBitCount := DIB.dsbm.bmBitsPixel * DIB.dsbm.bmPlanes;
  end;
  Info.biPlanes := 1;
  Info.biSizeImage := AlignBit(Info.biWidth, Info.biBitCount, 32) * Cardinal(abs(Info.biHeight));
end;
{$IFDEF O_PLUS}
  {$O+}
  {$UNDEF O_PLUS}
{$ENDIF}

// -------------------
// InternalGetDIBSizes
// -------------------
// Calculates the buffer sizes nescessary for convertion of a bitmap to a DIB
// of a specified PixelFormat.
// See the GetDIBSizes API function for more info.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// InfoHeaderSize
//		The returned size of a buffer that will receive the DIB's
//		TBitmapInfo structure.
// ImageSize	The returned size of a buffer that will receive the DIB's
//		pixel data.
// PixelFormat	The pixel format of the destination DIB.
//
procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
  var ImageSize: longInt; PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  Info		: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, Info, PixelFormat);
  // Check for palette device format
  if (Info.biBitCount > 8) then
  begin
    // Header but no palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if ((Info.biCompression and BI_BITFIELDS) <> 0) then
      Inc(InfoHeaderSize, 12);
  end else
    // Header and palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl Info.biBitCount);
  ImageSize := Info.biSizeImage;
end;

// --------------
// InternalGetDIB
// --------------
// Converts a bitmap to a DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Pal		The handle of the source palette.
// BitmapInfo	The buffer that will receive the DIB's TBitmapInfo structure.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// Bits		The buffer that will receive the DIB's pixel data.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// PixelFormat	The pixel format of the destination DIB.
//
// Returns:
// True on success, False on failure.
//
// Note: The InternalGetDIBSizes function can be used to calculate the
// nescessary sizes of the BitmapInfo and Bits buffers.
//
function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
// From graphics.pas, "optimized" for our use
var
  OldPal	: HPALETTE;
  DC		: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), PixelFormat);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if (Palette <> 0) then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := (GetDIBits(DC, Bitmap, 0, abs(TBitmapInfoHeader(BitmapInfo).biHeight),
      @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0);
  finally
    if (OldPal <> 0) then
      SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

// ----------
// DIBFromBit
// ----------
// Converts a bitmap to a DIB of a specified PixelFormat.
// The DIB is returned in a TMemoryStream ready for streaming to a BMP file.
//
// Note: As opposed to D2's DIBFromBit function, the returned stream also
// contains a TBitmapFileHeader at offset 0.
//
// Parameters:
// Stream	The TMemoryStream used to store the bitmap data.
//		The stream must be allocated and freed by the caller prior to
//		calling this function.
// Src		The handle of the source bitmap.
// Pal		The handle of the source palette.
// PixelFormat	The pixel format of the destination DIB.
// DIBHeader	A pointer to the DIB's TBitmapInfo (or TBitmapInfoHeader)
//		structure in the memory stream.
//		The size of the structure can either be deduced from the
//		pixel format (i.e. number of colors) or calculated by
//		subtracting the DIBHeader pointer from the DIBBits pointer.
// DIBBits	A pointer to the DIB's pixel data in the memory stream.
//
procedure DIBFromBit(Stream: TMemoryStream; Src: HBITMAP;
  Pal: HPALETTE; PixelFormat: TPixelFormat; var DIBHeader, DIBBits: Pointer);
// (From D2 graphics.pas, "optimized" for our use)
var
  HeaderSize		: integer;
  FileSize		: longInt;
  ImageSize		: longInt;
  BitmapFileHeader	: PBitmapFileHeader;
begin
  if (Src = 0) then
    Error(sInvalidBitmap);
  // Get header- and pixel data size for new pixel format
  InternalGetDIBSizes(Src, HeaderSize, ImageSize, PixelFormat);
  // Make room in stream for a TBitmapInfo and pixel data
  FileSize := sizeof(TBitmapFileHeader) + HeaderSize + ImageSize;
  Stream.SetSize(FileSize);
  // Get pointer to TBitmapFileHeader
  BitmapFileHeader := Stream.Memory;
  // Get pointer to TBitmapInfo
  DIBHeader := Pointer(Longint(BitmapFileHeader) + sizeof(TBitmapFileHeader));
  // Get pointer to pixel data
  DIBBits := Pointer(Longint(DIBHeader) + HeaderSize);
  // Initialize file header
  FillChar(BitmapFileHeader^, sizeof(TBitmapFileHeader), 0);
  with BitmapFileHeader^ do
  begin
    bfType := $4D42; // 'BM' = Windows BMP signature
    bfSize := FileSize; // File size (not needed)
    bfOffBits := sizeof(TBitmapFileHeader) + HeaderSize; // Offset of pixel data
  end;
  // Get pixel data in new pixel format
  InternalGetDIB(Src, Pal, DIBHeader^, DIBBits^, PixelFormat);
end;

// --------------
// GetPixelFormat
// --------------
// Returns the current pixel format of a bitmap.
//
// Replacement for delphi 3 TBitmap.PixelFormat getter.
//
// Parameters:
// Bitmap	The bitmap which pixel format is returned.
//
// Returns:
// The PixelFormat of the bitmap
//
{$IFDEF D4_BCB3}
  // Disable optimization to circumvent D4/BCB3 optimizer bug
  {$IFOPT O+}
    {$DEFINE O_PLUS}
    {$O-}
  {$ENDIF}
{$ENDIF}
function GetPixelFormat(Bitmap: TBitmap): TPixelFormat;
{$IFDEF VER9x}
// From graphics.pas, "optimized" for our use
var
  DIBSection		: TDIBSection;
  Bytes			: Integer;
begin
  Result := pfCustom; // This value is never returned
  if (Bitmap.Handle <> 0) then
  begin
    Bytes := GetObject(Bitmap.Handle, SizeOF(DIBSection), @DIBSection);
    if (Bytes = 0) then
      Error(sInvalidBitmap);

    with (DIBSection) do
    begin
      // Check for NT bitmap
      if (Bytes < (SizeOf(dsbm) + SizeOf(dsbmih))) or (dsbmih.biSize < SizeOf(dsbmih)) then
        DIBSection.dsBmih.biBitCount := dsbm.bmBitsPixel * dsbm.bmPlanes;

      case (dsBmih.biBitCount) of
        0: Result := pfDevice;
        1: Result := pf1bit;
        4: Result := pf4bit;
        8: Result := pf8bit;
        16: case (dsBmih.biCompression) of
              BI_RGB:
                Result := pf15Bit;
              BI_BITFIELDS:
                if (dsBitFields[1] = $07E0) then
                  Result := pf16Bit;
            end;
        24: Result := pf24Bit;
        32: if (dsBmih.biCompression = BI_RGB) then
              Result := pf32Bit;
      else
        Error(sUnsupportedBitmap);
      end;
    end;
  end else
//    Result := pfDevice;
    Error(sUnsupportedBitmap);
end;
{$ELSE}
begin
  Result := Bitmap.PixelFormat;
end;
{$ENDIF}
{$IFDEF O_PLUS}
  {$O+}
  {$UNDEF O_PLUS}
{$ENDIF}

// --------------
// SetPixelFormat
// --------------
// Changes the pixel format of a TBitmap.
//
// Replacement for delphi 3 TBitmap.PixelFormat setter.
// The returned TBitmap will always be a DIB.
//
// Note: Under Delphi 3.x this function will leak a palette handle each time it
//       converts a TBitmap to pf8bit format!
//       If possible, use SafeSetPixelFormat instead to avoid this.
//
// Parameters:
// Bitmap	The bitmap to modify.
// PixelFormat	The pixel format to convert to.
//
procedure SetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);
{$IFDEF VER9x}
var
  Stream	: TMemoryStream;
  Header	,
  Bits		: Pointer;
begin
  // Can't change anything without a handle
  if (Bitmap.Handle = 0) then
    Error(sInvalidBitmap);

  // Only convert to supported formats
  if not(PixelFormat in SupportedPixelformats) then
    Error(sInvalidPixelFormat);

  // No need to convert to same format
  if (GetPixelFormat(Bitmap) = PixelFormat) then
    exit;

  Stream := TMemoryStream.Create;
  try
    // Convert to DIB file in memory stream
    DIBFromBit(Stream, Bitmap.Handle, Bitmap.Palette, PixelFormat, Header, Bits);
    // Load DIB from stream
    Stream.Position := 0;
    Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
{$ELSE}
begin
  Bitmap.PixelFormat := PixelFormat;
end;
{$ENDIF}

{$IFNDEF VER9x}
var
  pf8BitBitmap: TBitmap = nil;
{$ENDIF}

// ------------------
// SafeSetPixelFormat
// ------------------
// Changes the pixel format of a TBitmap but doesn't preserve the contents.
//
// Replacement for delphi 3 TBitmap.PixelFormat setter.
// The returned TBitmap will always be an empty DIB of the same size as the
// original bitmap.
//
// This function is used to avoid the palette handle leak that SetPixelFormat
// and TBitmap.PixelFormat suffers from.
//
// Parameters:
// Bitmap	The bitmap to modify.
// PixelFormat	The pixel format to convert to.
//
procedure SafeSetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);
{$IFDEF VER9x}
begin
  SetPixelFormat(Bitmap, PixelFormat);
end;
{$ELSE}
{$IFDEF VER11_PLUS}
begin
  Bitmap.PixelFormat := PixelFormat;
end;
{$ELSE}
var
  Width			,
  Height	: integer;
begin
  if (PixelFormat = pf8bit) then
  begin
    // Solution to "TBitmap.PixelFormat := pf8bit" leak by Greg Chapman <glc@well.com>
    if (pf8BitBitmap = nil) then
    begin
      // Create a "template" bitmap
      // The bitmap is deleted in the finalization section of the unit.
      pf8BitBitmap:= TBitmap.Create;
      // Convert template to pf8bit format
      // This will leak 1 palette handle, but only once
      pf8BitBitmap.PixelFormat:= pf8Bit;
    end;
    // Store the size of the original bitmap
    Width := Bitmap.Width;
    Height := Bitmap.Height;
    // Convert to pf8bit format by copying template
    Bitmap.Assign(pf8BitBitmap);
    // Restore the original size
    Bitmap.Width := Width;
    Bitmap.Height := Height;
  end else
    // This is safe since only pf8bit leaks
    Bitmap.PixelFormat := PixelFormat;
end;
{$ENDIF}
{$ENDIF}


{$IFDEF VER9x}

// -----------
// CopyPalette
// -----------
// Copies a HPALETTE.
//
// Copied from D3 graphics.pas.
// This is declared private in some old versions of Delphi 2 so we have to
// implement it here to support those old versions.
//
// Parameters:
// Palette	The palette to copy.
//
// Returns:
// The handle to a new palette.
//
function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;


// TThreadList implementation from Delphi 3 classes.pas
constructor TThreadList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := TList.Create;
end;

destructor TThreadList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

procedure TThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if FList.IndexOf(Item) = -1 then
      FList.Add(Item);
  finally
    UnlockList;
  end;
end;

procedure TThreadList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TThreadList.LockList: TList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

procedure TThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TThreadList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;
// End of TThreadList implementation

// From Delphi 3 sysutils.pas
{ CompareMem performs a binary compare of Length bytes of memory referenced
  by P1 to that of P2.  CompareMem returns True if the memory referenced by
  P1 is identical to that of P2. }
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

// Dummy ASSERT procedure since ASSERT does not exist in Delphi 2.x
procedure ASSERT(Condition: boolean; Message: string);
begin
end;

{$ENDIF} // Delphi 2.x stuff

////////////////////////////////////////////////////////////////////////////////
//
//			TDIB Class
//
//  These classes gives read and write access to TBitmap's pixel data
//  independantly of the Delphi version used.
//
////////////////////////////////////////////////////////////////////////////////
type
  TDIB = class(TObject)
  private
    FBitmap		: TBitmap;
    FPixelFormat	: TPixelFormat;
  protected
    function GetScanline(Row: integer): pointer; virtual; abstract;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat); virtual;
    property Scanline[Row: integer]: pointer read GetScanline;
    property Bitmap: TBitmap read FBitmap;
  end;

  TDIBReader = class(TDIB)
  private
{$ifdef VER9x}
    FDIB		: TDIBSection;
    FDC			: HDC;
    FScanLine		: pointer;
    FLastRow		: integer;
    FInfo		: PBitmapInfo;
    FBytes		: integer;
{$endif}
  protected
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat); override;
    destructor Destroy; override;
  end;

  TDIBWriter = class(TDIB)
  private
{$ifdef PIXELFORMAT_TOO_SLOW}
    FDIBInfo		: PBitmapInfo;
    FDIBBits		: pointer;
    FDIBInfoSize	: integer;
    FDIBBitsSize	: longInt;
{$endif}
  protected
    procedure CreateDIB;
    procedure FreeDIB;
    procedure NeedDIB;
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat); override;
    destructor Destroy; override;
    procedure UpdateBitmap;
  end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIB.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
begin
  inherited Create;
  FBitmap := ABitmap;
  FPixelFormat := APixelFormat;
end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIBReader.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
begin
  inherited Create(ABitmap, APixelFormat);
{$ifndef VER9x}
  SetPixelFormat(FBitmap, FPixelFormat);
{$else}
  FDC := GetDC(0);

  GetMem(FInfo, SizeOf(TBitmapInfoHeader) + 256 * SizeOf(TRGBQuad));
  InitializeBitmapInfoHeader(ABitmap.Handle, FInfo^.bmiHeader, APixelFormat);
//  FInfo.bmiHeader.biCompression := BI_RGB;
  FLastRow := -1;

  // Allocate scan line buffer
  GetMem(FScanLine, AlignBit(FInfo^.bmiHeader.biWidth, FInfo^.bmiHeader.biBitCount, 32));
{$endif}
end;

destructor TDIBReader.Destroy;
begin
{$ifdef VER9x}
  ReleaseDC(0, FDC);
  FreeMem(FScanLine);
  FreeMem(FInfo);
{$endif}
  inherited Destroy;
end;

function TDIBReader.GetScanline(Row: integer): pointer;
begin
{$ifdef VER9x}
  if (Row < 0) or (Row >= FBitmap.Height) then
    raise EInvalidGraphicOperation.Create(SScanLine);
  GDIFlush;

  Result := FScanLine;
  if (Row = FLastRow) then
    exit;
  FLastRow := Row;

  if (FInfo^.bmiHeader.biHeight > 0) then  // bottom-up DIB
    Row := FInfo^.bmiHeader.biHeight - Row - 1;
  GetDIBits(FDC, FBitmap.Handle, Row, 1, FScanLine, FInfo^, DIB_RGB_COLORS);

{$else}
  Result := FBitmap.ScanLine[Row];
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIBWriter.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
{$ifndef PIXELFORMAT_TOO_SLOW}
var
  SavePalette		: HPalette;
{$endif}
begin
  inherited Create(ABitmap, APixelFormat);
{$ifndef PIXELFORMAT_TOO_SLOW}
  SavePalette := FBitmap.ReleasePalette;
  try
    SafeSetPixelFormat(FBitmap, FPixelFormat);
  finally
    FBitmap.Palette := SavePalette;
  end;
{$else}
  FDIBInfo := nil;
  FDIBBits := nil;
{$endif}
end;

destructor TDIBWriter.Destroy;
begin
  UpdateBitmap;
  FreeDIB;
  inherited Destroy;
end;

function TDIBWriter.GetScanline(Row: integer): pointer;
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  NeedDIB;

  if (FDIBBits = nil) then
    Error(sNoDIB);
  with FDIBInfo^.bmiHeader do
  begin
    if (Row < 0) or (Row >= FBitmap.Height) then
      raise EInvalidGraphicOperation.Create(SScanLine);
    GDIFlush;

    if biHeight > 0 then  // bottom-up DIB
      Row := biHeight - Row - 1;
    Result := PChar(Cardinal(FDIBBits) + Cardinal(Row) * AlignBit(biWidth, biBitCount, 32));
  end;
{$else}
  Result := FBitmap.ScanLine[Row];
{$endif}
end;

procedure TDIBWriter.CreateDIB;
{$IFDEF PIXELFORMAT_TOO_SLOW}
var
  SrcColors		,
  DstColors		: WORD;

  // From Delphi 3.02 graphics.pas
  // There is a bug in the ByteSwapColors from Delphi 3.0
  procedure ByteSwapColors(var Colors; Count: Integer);
  var   // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
    SysInfo: TSystemInfo;
  begin
    GetSystemInfo(SysInfo);
    asm
          MOV   EDX, Colors
          MOV   ECX, Count
          DEC   ECX
          JS    @@END
          LEA   EAX, SysInfo
          CMP   [EAX].TSystemInfo.wProcessorLevel, 3
          JE    @@386
    @@1:  MOV   EAX, [EDX+ECX*4]
          BSWAP EAX
          SHR   EAX,8
          MOV   [EDX+ECX*4],EAX
          DEC   ECX
          JNS   @@1
          JMP   @@END
    @@386:
          PUSH  EBX
    @@2:  XOR   EBX,EBX
          MOV   EAX, [EDX+ECX*4]
          MOV   BH, AL
          MOV   BL, AH
          SHR   EAX,16
          SHL   EBX,8
          MOV   BL, AL
          MOV   [EDX+ECX*4],EBX
          DEC   ECX
          JNS   @@2
          POP   EBX
      @@END:
    end;
  end;
{$ENDIF}
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  if (FBitmap.Handle = 0) then
    Error(sInvalidBitmap);

  FreeDIB;

  // Get header- and pixel data size
  InternalGetDIBSizes(FBitmap.Handle, FDIBInfoSize, FDIBBitsSize, FPixelFormat);

  // Allocate TBitmapInfo structure
  GetMem(FDIBInfo, FDIBInfoSize);
  try
    // Allocate pixel buffer
    FDIBBits := GlobalAllocPtr(GMEM_MOVEABLE, FDIBBitsSize);
    if (FDIBBits = nil) then
      raise EOutOfMemory.Create(sOutOfMemDIB);
    // Get pixel data
    if not(InternalGetDIB(FBitmap.Handle, FBitmap.Palette, FDIBInfo^, FDIBBits^, FPixelFormat)) then
      Error(sDIBCreate);

    if (FPixelFormat <= pf8bit) then
    begin
      {.$IFDEF VER9x}
      // ***FIXME***
      // Adjust for Delphi 2.x braindead palette behaviour:
      //
      // Copies the colors from a palette to a BitmapInfo structure.
      // When converting a DIB or DDB, the palette is realized and can therefore
      // not be used for our purpose; The first and last 10 palette entries are
      // allways set to the system palette colors no matter what palette we
      // attempt to define!
      // To circumvent this problem we import the palette to the DIB ourself.
      // For some strange reason this problem only occurs under Delphi 2.x.


      // Find number of colors defined by palette
      if (FBitmap.Palette = 0) or
        (GetObject(FBitmap.Palette, sizeof(SrcColors), @SrcColors) = 0) or
        (SrcColors = 0) then
        exit;
      // Determine how many colors there are room for in DIB header
      DstColors := FDIBInfo^.bmiHeader.biClrUsed;
      if (DstColors = 0) then
        DstColors := 1 SHL FDIBInfo^.bmiHeader.biBitCount;
      // Don't copy any more colors than there are room for
      if (DstColors <> 0) and (DstColors < SrcColors) then
        SrcColors := DstColors;

      // Copy all colors...
      GetPaletteEntries(FBitmap.Palette, 0, SrcColors, FDIBInfo^.bmiColors[0]);
      // ...and convert BGR to RGB
      ByteSwapColors(FDIBInfo^.bmiColors[0], SrcColors);

      // Finally zero any unused entried
      if (SrcColors < DstColors) then
        FillChar(pointer(LongInt(@FDIBInfo^.bmiColors)+SizeOf(TRGBQuad)*SrcColors)^,
          DstColors - SrcColors, 0);
     {.$ENDIF}
    end;

  except
    FreeDIB;
    raise;
  end;
{$endif}
end;

procedure TDIBWriter.FreeDIB;
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  if (FDIBInfo <> nil) then
    FreeMem(FDIBInfo);
  if (FDIBBits <> nil) then
    GlobalFreePtr(FDIBBits);
  FDIBInfo := nil;
  FDIBBits := nil;
{$endif}
end;

procedure TDIBWriter.NeedDIB;
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  if (FDIBBits = nil) then
    CreateDIB;
{$endif}
end;

// Convert the DIB created by CreateDIB back to a TBitmap
procedure TDIBWriter.UpdateBitmap;
{$ifdef PIXELFORMAT_TOO_SLOW}
var
  Stream		: TMemoryStream;
  FileSize		: longInt;
  BitmapFileHeader	: TBitmapFileHeader;
{$endif}
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  if (FDIBInfo = nil) or (FDIBBits = nil) then
    exit;
  Stream := TMemoryStream.Create;
  try
    // Make room in stream for a TBitmapInfo and pixel data
    FileSize := sizeof(TBitmapFileHeader) + FDIBInfoSize + FDIBBitsSize;
    Stream.SetSize(FileSize);
    // Initialize file header
    FillChar(BitmapFileHeader, sizeof(TBitmapFileHeader), 0);
    with BitmapFileHeader do
    begin
      bfType := $4D42; // 'BM' = Windows BMP signature
      bfSize := FileSize; // File size (not needed)
      bfOffBits := sizeof(TBitmapFileHeader) + FDIBInfoSize; // Offset of pixel data
    end;
    // Save file header
    Stream.Write(BitmapFileHeader, sizeof(TBitmapFileHeader));
    // Save TBitmapInfo structure
    Stream.Write(FDIBInfo^, FDIBInfoSize);
    // Save pixel data
    Stream.Write(FDIBBits^, FDIBBitsSize);

    // Rewind and load DIB into bitmap
    Stream.Position := 0;
    FBitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color Mapping
//
////////////////////////////////////////////////////////////////////////////////
type
  TColorLookup = class(TObject)
  private
    FColors		: integer;
  public
    constructor Create(Palette: hPalette); virtual;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; virtual; abstract;
    property Colors: integer read FColors;
  end;

  PRGBQuadArray = ^TRGBQuadArray;		// From Delphi 3 graphics.pas
  TRGBQuadArray = array[Byte] of TRGBQuad;	// From Delphi 3 graphics.pas

  BGRArray = array[0..0] of TRGBTriple;
  PBGRArray = ^BGRArray;

  PalArray =  array[byte] of TPaletteEntry;
  PPalArray = ^PalArray;

  // TFastColorLookup implements a simple but reasonably fast generic color
  // mapper. It trades precision for speed by reducing the size of the color
  // space.
  // Using a class instead of inline code results in a speed penalty of
  // approx. 15% but reduces the complexity of the color reduction routines that
  // uses it. If bitmap to GIF conversion speed is really important to you, the
  // implementation can easily be inlined again.
  TInverseLookup = array[0..1 SHL 15-1] of SmallInt;
  PInverseLookup = ^TInverseLookup;

  TFastColorLookup = class(TColorLookup)
  private
    FPaletteEntries	: PPalArray;
    FInverseLookup	: PInverseLookup;
  public
    constructor Create(Palette: hPalette); override;
    destructor Destroy; override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TSlowColorLookup implements a precise but very slow generic color mapper.
  // It uses the GetNearestPaletteIndex GDI function.
  // Note: Tests has shown TFastColorLookup to be more precise than
  // TSlowColorLookup in many cases. I can't explain why...
  TSlowColorLookup = class(TColorLookup)
  private
    FPaletteEntries	: PPalArray;
    FPalette		: hPalette;
  public
    constructor Create(Palette: hPalette); override;
    destructor Destroy; override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TNetscapeColorLookup maps colors to the netscape 6*6*6 color cube.
  TNetscapeColorLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TGrayWindowsLookup maps colors to 4 shade palette.
  TGrayWindowsLookup = class(TSlowColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TGrayScaleLookup maps colors to a uniform 256 shade palette.
  TGrayScaleLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TMonochromeLookup maps colors to a black/white palette.
  TMonochromeLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

constructor TColorLookup.Create(Palette: hPalette);
begin
  inherited Create;
end;

constructor TFastColorLookup.Create(Palette: hPalette);
var
  i			: integer;
  InverseIndex		: integer;
begin
  inherited Create(Palette);

  GetMem(FPaletteEntries, sizeof(TPaletteEntry) * 256);
  FColors := GetPaletteEntries(Palette, 0, 256, FPaletteEntries^);

  New(FInverseLookup);
  for i := low(TInverseLookup) to high(TInverseLookup) do
    FInverseLookup^[i] := -1;

  // Premap palette colors
  if (FColors > 0) then
    for i := 0 to FColors-1 do
      with FPaletteEntries^[i] do
      begin
        InverseIndex := (peRed SHR 3) OR ((peGreen AND $F8) SHL 2) OR ((peBlue AND $F8) SHL 7);
        if (FInverseLookup^[InverseIndex] = -1) then
          FInverseLookup^[InverseIndex] := i;
      end;
end;

destructor TFastColorLookup.Destroy;
begin
  if (FPaletteEntries <> nil) then
    FreeMem(FPaletteEntries);
  if (FInverseLookup <> nil) then
    Dispose(FInverseLookup);

  inherited Destroy;
end;

// Map color to arbitrary palette
function TFastColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
var
  i			: integer;
  InverseIndex		: integer;
  Delta			,
  MinDelta		,
  MinColor		: integer;
begin
  // Reduce color space with 3 bits in each dimension
  InverseIndex := (Red SHR 3) OR ((Green AND $F8) SHL 2) OR ((Blue AND $F8) SHL 7);

  if (FInverseLookup^[InverseIndex] <> -1) then
    Result := char(FInverseLookup^[InverseIndex])
  else
  begin
    // Sequential scan for nearest color to minimize euclidian distance
    MinDelta := 3 * (256 * 256);
    MinColor := 0;
    for i := 0 to FColors-1 do
      with FPaletteEntries[i] do
      begin
        Delta := ABS(peRed - Red) + ABS(peGreen - Green) + ABS(peBlue - Blue);
        if (Delta < MinDelta) then
        begin
          MinDelta := Delta;
          MinColor := i;
        end;
      end;
    Result := char(MinColor);
    FInverseLookup^[InverseIndex] := MinColor;
  end;

  with FPaletteEntries^[ord(Result)] do
  begin
    R := peRed;
    G := peGreen;
    B := peBlue;
  end;
end;

constructor TSlowColorLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FPalette := Palette;
  FColors := GetPaletteEntries(Palette, 0, 256, nil^);
  if (FColors > 0) then
  begin
    GetMem(FPaletteEntries, sizeof(TPaletteEntry) * FColors);
    FColors := GetPaletteEntries(Palette, 0, 256, FPaletteEntries^);
  end;
end;

destructor TSlowColorLookup.Destroy;
begin
  if (FPaletteEntries <> nil) then
    FreeMem(FPaletteEntries);

  inherited Destroy;
end;

// Map color to arbitrary palette
function TSlowColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  Result := char(GetNearestPaletteIndex(FPalette, Red OR (Green SHL 8) OR (Blue SHL 16)));
  if (FPaletteEntries <> nil) then
    with FPaletteEntries^[ord(Result)] do
    begin
      R := peRed;
      G := peGreen;
      B := peBlue;
    end;
end;

constructor TNetscapeColorLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 6*6*6; // This better be true or something is wrong
end;

// Map color to netscape 6*6*6 color cube
function TNetscapeColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  R := (Red+3) DIV 51;
  G := (Green+3) DIV 51;
  B := (Blue+3) DIV 51;
  Result := char(B + 6*G + 36*R);
  R := R * 51;
  G := G * 51;
  B := B * 51;
end;

constructor TGrayWindowsLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 4;
end;

// Convert color to windows grays
function TGrayWindowsLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  Result := inherited Lookup(MulDiv(Red, 77, 256),
    MulDiv(Green, 150, 256), MulDiv(Blue, 29, 256), R, G, B);
end;

constructor TGrayScaleLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 256;
end;

// Convert color to grayscale
function TGrayScaleLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  Result := char((Blue*29 + Green*150 + Red*77) DIV 256);
  R := ord(Result);
  G := ord(Result);
  B := ord(Result);
end;

constructor TMonochromeLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 2;
end;

// Convert color to black/white
function TMonochromeLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  if ((Blue*29 + Green*150 + Red*77) > 32512) then
  begin
    Result := #1;
    R := 255;
    G := 255;
    B := 255;
  end else
  begin
    Result := #0;
    R := 0;
    G := 0;
    B := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Dithering engine
//
////////////////////////////////////////////////////////////////////////////////
type
  TDitherEngine = class
  protected
    FDirection		: integer;
    FColumn		: integer;
    FLookup		: TColorLookup;
    Width		: integer;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); virtual;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; virtual;
    procedure NextLine; virtual;

    property Direction: integer read FDirection;
    property Column: integer read FColumn;
  end;

  // Note: TErrorTerm does only *need* to be 16 bits wide, but since
  // it is *much* faster to use native machine words (32 bit), we sacrifice
  // some bytes (a lot actually) to improve performance.
  TErrorTerm		= Integer;
  TErrors		= array[0..0] of TErrorTerm;
  PErrors		= ^TErrors;

  TFloydSteinbergEngine = class(TDitherEngine)
  private
    ErrorsR		,
    ErrorsG		,
    ErrorsB		: PErrors;
    ErrorR		,
    ErrorG		,
    ErrorB		: PErrors;
    CurrentErrorR	,		// Current error or pixel value
    CurrentErrorG	,
    CurrentErrorB	,
    BelowErrorR		,		// Error for pixel below current
    BelowErrorG		,
    BelowErrorB		,
    BelowPrevErrorR	,		// Error for pixel below previous pixel
    BelowPrevErrorG	,
    BelowPrevErrorB	: TErrorTerm;

  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
    procedure NextLine; override;
  end;

constructor TDitherEngine.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create;

  FLookup := Lookup;
  Width := AWidth;

  FDirection := 1;
  FColumn := 0;
end;

function TDitherEngine.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  // Map color to palette
  Result := FLookup.Lookup(Red, Green, Blue, R, G, B);
  inc(FColumn, FDirection);
end;

procedure TDitherEngine.NextLine;
begin
  FDirection := -FDirection;
  if (FDirection = 1) then
    FColumn := 0
  else
    FColumn := Width-1;
end;

constructor TFloydSteinbergEngine.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);

  // The Error arrays has (columns + 2) entries; the extra entry at
  // each end saves us from special-casing the first and last pixels.
  // We can get away with a single array (holding one row's worth of errors)
  // by using it to store the current row's errors at pixel columns not yet
  // processed, but the next row's errors at columns already processed.  We
  // need only a few extra variables to hold the errors immediately around the
  // current column.  (If we are lucky, those variables are in registers, but
  // even if not, they're probably cheaper to access than array elements are.)
  GetMem(ErrorsR, sizeof(TErrorTerm)*(Width+2));
  GetMem(ErrorsG, sizeof(TErrorTerm)*(Width+2));
  GetMem(ErrorsB, sizeof(TErrorTerm)*(Width+2));
  FillChar(ErrorsR^, sizeof(TErrorTerm)*(Width+2), 0);
  FillChar(ErrorsG^, sizeof(TErrorTerm)*(Width+2), 0);
  FillChar(ErrorsB^, sizeof(TErrorTerm)*(Width+2), 0);
  ErrorR := ErrorsR;
  ErrorG := ErrorsG;
  ErrorB := ErrorsB;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorR;
  BelowErrorR := CurrentErrorR;
  BelowErrorG := CurrentErrorR;
  BelowErrorB := CurrentErrorR;
  BelowPrevErrorR := CurrentErrorR;
  BelowPrevErrorG := CurrentErrorR;
  BelowPrevErrorB := CurrentErrorR;
end;

destructor TFloydSteinbergEngine.Destroy;
begin
  FreeMem(ErrorsR);
  FreeMem(ErrorsG);
  FreeMem(ErrorsB);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TFloydSteinbergEngine.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
var
  BelowNextError	: TErrorTerm;
  Delta			: TErrorTerm;
begin
  CurrentErrorR := Red + (CurrentErrorR + ErrorR[FDirection] + 8) DIV 16;
  if (CurrentErrorR < 0) then
    CurrentErrorR := 0
  else if (CurrentErrorR > 255) then
    CurrentErrorR := 255;

  CurrentErrorG := Green + (CurrentErrorG + ErrorG[FDirection] + 8) DIV 16;
  if (CurrentErrorG < 0) then
    CurrentErrorG := 0
  else if (CurrentErrorG > 255) then
    CurrentErrorG := 255;

  CurrentErrorB := Blue + (CurrentErrorB + ErrorB[FDirection] + 8) DIV 16;
  if (CurrentErrorB < 0) then
    CurrentErrorB := 0
  else if (CurrentErrorB > 255) then
    CurrentErrorB := 255;

  // Map color to palette
  Result := inherited Dither(CurrentErrorR, CurrentErrorG, CurrentErrorB, R, G, B);

  // Propagate Floyd-Steinberg error terms.
  // Errors are accumulated into the error arrays, at a resolution of
  // 1/16th of a pixel count.  The error at a given pixel is propagated
  // to its not-yet-processed neighbors using the standard F-S fractions,
  //		...	(here)	7/16
  //		3/16	5/16	1/16
  // We work left-to-right on even rows, right-to-left on odd rows.

  // Red component
  CurrentErrorR := CurrentErrorR - R;
  BelowNextError := CurrentErrorR;			// Error * 1

  Delta := CurrentErrorR * 2;
  CurrentErrorR := CurrentErrorR + Delta;
  ErrorR[0] := BelowPrevErrorR + CurrentErrorR;		// Error * 3

  CurrentErrorR := CurrentErrorR + Delta;
  BelowPrevErrorR := BelowErrorR + CurrentErrorR;	// Error * 5

  BelowErrorR := BelowNextError;			// Error * 1

  CurrentErrorR := CurrentErrorR + Delta;		// Error * 7

  // Green component
  CurrentErrorG := CurrentErrorG - G;
  BelowNextError := CurrentErrorG;			// Error * 1

  Delta := CurrentErrorG * 2;
  CurrentErrorG := CurrentErrorG + Delta;
  ErrorG[0] := BelowPrevErrorG + CurrentErrorG;		// Error * 3

  CurrentErrorG := CurrentErrorG + Delta;
  BelowPrevErrorG := BelowErrorG + CurrentErrorG;	// Error * 5

  BelowErrorG := BelowNextError;			// Error * 1

  CurrentErrorG := CurrentErrorG + Delta;		// Error * 7

  // Blue component
  CurrentErrorB := CurrentErrorB - B;
  BelowNextError := CurrentErrorB;			// Error * 1

  Delta := CurrentErrorB * 2;
  CurrentErrorB := CurrentErrorB + Delta;
  ErrorB[0] := BelowPrevErrorB + CurrentErrorB;		// Error * 3

  CurrentErrorB := CurrentErrorB + Delta;
  BelowPrevErrorB := BelowErrorB + CurrentErrorB;	// Error * 5

  BelowErrorB := BelowNextError;			// Error * 1

  CurrentErrorB := CurrentErrorB + Delta;		// Error * 7

  // Move on to next column
  if (FDirection = 1) then
  begin
    inc(longInt(ErrorR), sizeof(TErrorTerm));
    inc(longInt(ErrorG), sizeof(TErrorTerm));
    inc(longInt(ErrorB), sizeof(TErrorTerm));
  end else
  begin
    dec(longInt(ErrorR), sizeof(TErrorTerm));
    dec(longInt(ErrorG), sizeof(TErrorTerm));
    dec(longInt(ErrorB), sizeof(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TFloydSteinbergEngine.NextLine;
begin
  ErrorR[0] := BelowPrevErrorR;
  ErrorG[0] := BelowPrevErrorG;
  ErrorB[0] := BelowPrevErrorB;

  // Note: The optimizer produces better code for this construct:
  //   a := 0; b := a; c := a;
  // compared to this construct:
  //   a := 0; b := 0; c := 0;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorG;
  BelowErrorR := CurrentErrorG;
  BelowErrorG := CurrentErrorG;
  BelowErrorB := CurrentErrorG;
  BelowPrevErrorR := CurrentErrorG;
  BelowPrevErrorG := CurrentErrorG;
  BelowPrevErrorB := CurrentErrorG;

  inherited NextLine;

  if (FDirection = 1) then
  begin
    ErrorR := ErrorsR;
    ErrorG := ErrorsG;
    ErrorB := ErrorsB;
  end else
  begin
    ErrorR := @ErrorsR[Width+1];
    ErrorG := @ErrorsG[Width+1];
    ErrorB := @ErrorsB[Width+1];
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Engine
//
////////////////////////////////////////////////////////////////////////////////
//  Adapted from Earl F. Glynn's ColorQuantizationLibrary, March 1998
////////////////////////////////////////////////////////////////////////////////
type
  TOctreeNode = class;	// Forward definition so TReducibleNodes can be declared

  TReducibleNodes = array[0..7] of TOctreeNode;

  TOctreeNode = Class(TObject)
  public
    IsLeaf		: Boolean;
    PixelCount		: integer;
    RedSum		: integer;
    GreenSum		: integer;
    BlueSum		: integer;
    Next		: TOctreeNode;
    Child		: TReducibleNodes;

    constructor Create(Level: integer; ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);
    destructor Destroy; override;
  end;

  TColorQuantizer = class(TObject)
  private
    FTree		: TOctreeNode;
    FLeafCount		: integer;
    FReducibleNodes	: TReducibleNodes;
    FMaxColors		: integer;
    FColorBits		: integer;

  protected
    procedure AddColor(var Node: TOctreeNode; r, g, b: byte; ColorBits: integer;
      Level: integer; var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(const Node: TOctreeNode; 
      var RGBQuadArray: TRGBQuadArray; var Index: integer);
    procedure ReduceTree(ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);

  public
    constructor Create(MaxColors: integer; ColorBits: integer);
    destructor Destroy; override;

    procedure GetColorTable(var RGBQuadArray: TRGBQuadArray);
    function ProcessImage(const DIB: TDIBReader): boolean;

    property ColorCount: integer read FLeafCount;
  end;

constructor TOctreeNode.Create(Level: integer; ColorBits: integer;
  var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
var
  i			: integer;
begin
  PixelCount := 0;
  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  for i := Low(Child) to High(Child) do
    Child[i] := nil;

  IsLeaf := (Level = ColorBits);
  if (IsLeaf) then
  begin
    Next := nil;
    inc(LeafCount);
  end else
  begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := self;
  end;
end;

destructor TOctreeNode.Destroy;
var
  i			: integer;
begin
  for i := High(Child) downto Low(Child) do
    Child[i].Free;
end;

constructor TColorQuantizer.Create(MaxColors: integer; ColorBits: integer);
var
  i			: integer;
begin
  ASSERT(ColorBits <= 8, 'ColorBits must be 8 or less');

  FTree := nil;
  FLeafCount := 0;

  // Initialize all nodes even though only ColorBits+1 of them are needed
  for i := Low(FReducibleNodes) to High(FReducibleNodes) do
    FReducibleNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := ColorBits;
end;

destructor TColorQuantizer.Destroy;
begin
  if (FTree <> nil) then
    DeleteTree(FTree);
end;

procedure TColorQuantizer.GetColorTable(var RGBQuadArray: TRGBQuadArray);
var
  Index			: integer;
begin
  Index := 0;
  GetPaletteColors(FTree, RGBQuadArray, Index);
end;

// Handles passed to ProcessImage should refer to DIB sections, not DDBs.
// In certain cases, specifically when it's called upon to process 1, 4, or
// 8-bit per pixel images on systems with palettized display adapters,
// ProcessImage can produce incorrect results if it's passed a handle to a
// DDB.
function TColorQuantizer.ProcessImage(const DIB: TDIBReader): boolean;
var
  i			,
  j			: integer;
  ScanLine		: pointer;
  Pixel			: PRGBTriple;
begin
  Result := True;

  for j := 0 to DIB.Bitmap.Height-1 do
  begin
    Scanline := DIB.Scanline[j];
    Pixel := ScanLine;
    for i := 0 to DIB.Bitmap.Width-1 do
    begin
      with Pixel^ do
        AddColor(FTree, rgbtRed, rgbtGreen, rgbtBlue,
                 FColorBits, 0, FLeafCount, FReducibleNodes);

      while FLeafCount > FMaxColors do
        ReduceTree(FColorbits, FLeafCount, FReducibleNodes);
      inc(Pixel);
    end;
  end;
end;

procedure TColorQuantizer.AddColor(var Node: TOctreeNode; r,g,b: byte;
  ColorBits: integer; Level: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
const
  Mask:  array[0..7] of BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);
var
  Index			: integer;
  Shift			: integer;
begin
  // If the node doesn't exist, create it.
  if (Node = nil) then
    Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  if (Node.IsLeaf) then
  begin
    inc(Node.PixelCount);
    inc(Node.RedSum, r);
    inc(Node.GreenSum, g);
    inc(Node.BlueSum, b);
  end else
  begin
    // Recurse a level deeper if the node is not a leaf.
    Shift := 7 - Level;

    Index := (((r and mask[Level]) SHR Shift) SHL 2)  or
             (((g and mask[Level]) SHR Shift) SHL 1)  or
              ((b and mask[Level]) SHR Shift);
    AddColor(Node.Child[Index], r, g, b, ColorBits, Level+1, LeafCount, ReducibleNodes);
  end;
end;

procedure TColorQuantizer.DeleteTree(var Node: TOctreeNode);
var
  i			: integer;
begin
  for i := High(TReducibleNodes) downto Low(TReducibleNodes) do
    if (Node.Child[i] <> nil) then
      DeleteTree(Node.Child[i]);

  Node.Free;
  Node := nil;
end;

procedure TColorQuantizer.GetPaletteColors(const Node: TOctreeNode;
  var RGBQuadArray: TRGBQuadArray; var Index: integer);
var
  i			: integer;
begin
  if (Node.IsLeaf) then
  begin
    with RGBQuadArray[Index] do
    begin
      if (Node.PixelCount <> 0) then
      begin
        rgbRed   := BYTE(Node.RedSum   DIV Node.PixelCount);
        rgbGreen := BYTE(Node.GreenSum DIV Node.PixelCount);
        rgbBlue  := BYTE(Node.BlueSum  DIV Node.PixelCount);
      end else
      begin
        rgbRed := 0;
        rgbGreen := 0;
        rgbBlue := 0;
      end;
      rgbReserved := 0;
    end;
    inc(Index);
  end else
  begin
    for i := Low(Node.Child) to High(Node.Child) do
      if (Node.Child[i] <> nil) then
        GetPaletteColors(Node.Child[i], RGBQuadArray, Index);
  end;
end;

procedure TColorQuantizer.ReduceTree(ColorBits: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
var
  RedSum		,
  GreenSum		,
  BlueSum 		: integer;
  Children		: integer;
  i			: integer;
  Node			: TOctreeNode;
begin
  // Find the deepest level containing at least one reducible node
  i := Colorbits - 1;
  while (i > 0) and (ReducibleNodes[i] = nil) do
    dec(i);

  // Reduce the node most recently added to the list at level i.
  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  RedSum   := 0;
  GreenSum := 0;
  BlueSum  := 0;
  Children := 0;

  for i := Low(ReducibleNodes) to High(ReducibleNodes) do
    if (Node.Child[i] <> nil) then
    begin
      inc(RedSum, Node.Child[i].RedSum);
      inc(GreenSum, Node.Child[i].GreenSum);
      inc(BlueSum, Node.Child[i].BlueSum);
      inc(Node.PixelCount, Node.Child[i].PixelCount);
      Node.Child[i].Free;
      Node.Child[i] := nil;
      inc(Children);
    end;

  Node.IsLeaf := TRUE;
  Node.RedSum := RedSum;
  Node.GreenSum := GreenSum;
  Node.BlueSum := BlueSum;
  dec(LeafCount, Children-1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Wrapper
//
////////////////////////////////////////////////////////////////////////////////
//	Adapted from Earl F. Glynn's PaletteLibrary, March 1998
////////////////////////////////////////////////////////////////////////////////

// Wrapper for internal use - uses TDIBReader for bitmap access
function doCreateOptimizedPaletteForSingleBitmap(const DIB: TDIBReader;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  SystemPalette		: HPalette;
  ColorQuantizer	: TColorQuantizer;
  i			: integer;
  LogicalPalette	: TMaxLogPalette;
  RGBQuadArray		: TRGBQuadArray;
  Offset		: integer;
begin
  LogicalPalette.palVersion := $0300;
  LogicalPalette.palNumEntries := Colors;

  if (Windows) then
  begin
    // Get the windows 20 color system palette
    SystemPalette := GetStockObject(DEFAULT_PALETTE);
    GetPaletteEntries(SystemPalette, 0, 10, LogicalPalette.palPalEntry[0]);
    GetPaletteEntries(SystemPalette, 10, 10, LogicalPalette.palPalEntry[245]);
    Colors := 236;
    Offset := 10;
    LogicalPalette.palNumEntries := 256;
  end else
    Offset := 0;

  // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
  // use ColorBits = 8.
  ColorQuantizer := TColorQuantizer.Create(Colors, ColorBits);
  try
    ColorQuantizer.ProcessImage(DIB);
    ColorQuantizer.GetColorTable(RGBQuadArray);
  finally
    ColorQuantizer.Free;
  end;

  for i := 0 to Colors-1 do
    with LogicalPalette.palPalEntry[i+Offset] do
    begin
      peRed   := RGBQuadArray[i].rgbRed;
      peGreen := RGBQuadArray[i].rgbGreen;
      peBlue  := RGBQuadArray[i].rgbBlue;
      peFlags := RGBQuadArray[i].rgbReserved;
    end;
  Result := CreatePalette(pLogPalette(@LogicalPalette)^);
end;

function CreateOptimizedPaletteForSingleBitmap(const Bitmap: TBitmap;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  DIB			: TDIBReader;
begin
  DIB := TDIBReader.Create(Bitmap, pf24bit);
  try
    Result := doCreateOptimizedPaletteForSingleBitmap(DIB, Colors, ColorBits, Windows);
  finally
    DIB.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color reduction
//
////////////////////////////////////////////////////////////////////////////////
{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
  DitherMode: TDitherMode; ReductionBits: integer): TBitmap;
var
  Palette		: hPalette;
  ColorLookup		: TColorLookup;
  Ditherer		: TDitherEngine;
  Row			: Integer;
  DIBResult		: TDIBWriter;
  DIBSource		: TDIBReader;
  SrcScanLine		,
  Src			: PRGBTriple;
  DstScanLine		,
  Dst			: PChar;
  BGR			: TRGBTriple;
{$ifdef DEBUG_DITHERPERFORMANCE}
  TimeStart		,
  TimeStop		: DWORD;
{$endif}

  function GrayScalePalette: hPalette;
  var
    i			: integer;
    Pal			: TMaxLogPalette;
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 256;
    for i := 0 to 255 do
    begin
      with (Pal.palPalEntry[i]) do
      begin
        peRed := i;
        peGreen := i;
        peBlue  := i;
        peFlags := PC_NOCOLLAPSE;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Pal)^);
  end;

  function MonochromePalette: hPalette;
  var
    i			: integer;
    Pal			: TMaxLogPalette;
  const
    Values		: array[0..1] of byte
    			= (0, 255);
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 2;
    for i := 0 to 3 do
    begin
      with (Pal.palPalEntry[i]) do
      begin
        peRed := Values[i];
        peGreen := Values[i];
        peBlue  := Values[i];
        peFlags := PC_NOCOLLAPSE;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Pal)^);
  end;

  function WindowsGrayScalePalette: hPalette;
  var
    i			: integer;
    Pal			: TMaxLogPalette;
  const
    Values		: array[0..3] of byte
    			= (0, 128, 192, 255);
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 4;
    for i := 0 to 3 do
    begin
      with (Pal.palPalEntry[i]) do
      begin
        peRed := Values[i];
        peGreen := Values[i];
        peBlue  := Values[i];
        peFlags := PC_NOCOLLAPSE;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Pal)^);
  end;

  function WindowsHalftonePalette: hPalette;
  var
    DC			: HDC;
  begin
    DC := GDICheck(GetDC(0));
    try
      Result := CreateHalfTonePalette(DC);
    finally
      ReleaseDC(0, DC);
    end;
  end;

begin
{$ifdef DEBUG_DITHERPERFORMANCE}
  timeBeginPeriod(5);
  TimeStart := timeGetTime;
{$endif}

  Result := TBitmap.Create;
  try

    if (ColorReduction = rmNone) then
    begin
      Result.Assign(Bitmap);
      SetPixelFormat(Result, pf24bit);
      exit;
    end;

{$IFNDEF VER9x}
    if (Bitmap.Width*Bitmap.Height > BitmapAllocationThreshold) then
      SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize
{$ENDIF}

    // Set bitmap width and height
    Result.Width := Bitmap.Width;
    Result.Height := Bitmap.Height;

    // Set the bitmap pixel format
    SafeSetPixelFormat(Result, pf8bit);
    Result.Palette := 0;

    ColorLookup := nil;
    Ditherer := nil;
    DIBResult := nil;
    DIBSource := nil;
    Palette := 0;
    try // Protect above resources

      // Dithering and color mapper only supports 24 bit bitmaps,
      // so we have convert the source bitmap to the appropiate format.
      DIBSource := TDIBReader.Create(Bitmap, pf24bit);

      try
        // Create a palette based on current options
        case (ColorReduction) of
          rmQuantize:
            Palette := doCreateOptimizedPaletteForSingleBitmap(DIBSource, 1 SHL ReductionBits, 8, False);
          rmQuantizeWindows:
            Palette := CreateOptimizedPaletteForSingleBitmap(Bitmap, 256, 8, True);
          rmNetscape:
            Palette := WebPalette;
          rmGrayScale:
            Palette := GrayScalePalette;
          rmMonochrome:
            Palette := MonochromePalette;
          rmWindowsGray:
            Palette := WindowsGrayScalePalette;
          rmWindows20:
            Palette := GetStockObject(DEFAULT_PALETTE);
          rmWindows256:
            Palette := WindowsHalftonePalette;
        else
          exit;
        end;

        Result.Palette := Palette;

        // ***FIXME*** Gray scale conversion should be done
        // prior to dithering/mapping. Otherwise corrected
        // values will be converted multiple times.  
        // Create a color mapper based on current options
        case (ColorReduction) of
          // For some strange reason my fast and dirty color lookup
          // is more precise that Windows GetNearestPaletteIndex...
          // rmWindows20:
          //  ColorLookup := TSlowColorLookup.Create(Palette);
          // rmWindowsGray:
          //  ColorLookup := TGrayWindowsLookup.Create(Palette);
          rmQuantize:
            ColorLookup := TFastColorLookup.Create(Palette);
          rmNetscape:
            ColorLookup := TNetscapeColorLookup.Create(Palette);
          rmGrayScale:
            ColorLookup := TGrayScaleLookup.Create(Palette);
          rmMonochrome:
            ColorLookup := TMonochromeLookup.Create(Palette);
        else
          ColorLookup := TFastColorLookup.Create(Palette);
        end;

        // Nothing to do if palette doesn't contain any colors
        if (ColorLookup.Colors = 0) then
          exit;

        // Create a ditherer based on current options
        case (DitherMode) of
          dmNearest:
            Ditherer := TDitherEngine.Create(Bitmap.Width, ColorLookup);
          dmFloydSteinberg:
            Ditherer := TFloydSteinbergEngine.Create(Bitmap.Width, ColorLookup);
        else
          exit;
        end;

        // The processed bitmap is returned in pf8bit format
        DIBResult := TDIBWriter.Create(Result, pf8bit);

        // Process the image
        Row := 0;
        while (Row < Bitmap.Height) do
        begin
          SrcScanline := DIBSource.ScanLine[Row];
          DstScanline := DIBResult.ScanLine[Row];
          Src := pointer(longInt(SrcScanLine) + Ditherer.Column*sizeof(TRGBTriple));
          Dst := pointer(longInt(DstScanLine) + Ditherer.Column);

          while (Ditherer.Column < Ditherer.Width) and (Ditherer.Column >= 0) do
          begin
            BGR := Src^;
            // Dither and map a single pixel
            Dst^ := Ditherer.Dither(BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue,
              BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue);

            inc(Src, Ditherer.Direction);
            inc(Dst, Ditherer.Direction);
          end;

          Inc(Row);
          Ditherer.NextLine;
        end;
      except
        Result.ReleasePalette;
        if (Palette <> 0) then
          DeleteObject(Palette);
        raise;
      end;
    finally
      if (ColorLookup <> nil) then
        ColorLookup.Free;
      if (Ditherer <> nil) then
        Ditherer.Free;
      if (DIBResult <> nil) then
        DIBResult.Free;
      if (DIBSource <> nil) then
        DIBSource.Free;
    end;
  except
    Result.Free;
    raise;
  end;

{$ifdef DEBUG_DITHERPERFORMANCE}
  TimeStop := timeGetTime;
  ShowMessage(format('Dithered %d pixels in %d mS, Rate %d pixels/mS (%d pixels/S)',
    [Bitmap.Height*Bitmap.Width, TimeStop-TimeStart,
    MulDiv(Bitmap.Height, Bitmap.Width, TimeStop-TimeStart+1),
    MulDiv(Bitmap.Height, Bitmap.Width * 1000, TimeStop-TimeStart+1)]));
  timeEndPeriod(5);
{$endif}
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFColorMap
//
////////////////////////////////////////////////////////////////////////////////
const
  InitColorMapSize = 16;
  DeltaColorMapSize = 32;

constructor TGIFColorMap.Create;
begin
  inherited Create;
  FColorMap := nil;
  FCapacity := 0;
  FCount := 0;
  FOptimized := False;
end;

destructor TGIFColorMap.Destroy;
begin
  Clear;
  Changed;
  inherited Destroy;
end;

procedure TGIFColorMap.Clear;
begin
  if (FColorMap <> nil) then
    FreeMem(FColorMap);
  FColorMap := nil;
  FCapacity := 0;
  FCount := 0;
  FOptimized := False;
end;

class function TGIFColorMap.Color2RGB(Color: TColor): TGIFColor;
begin
  Result.Blue := (Color shr 16) and $FF;
  Result.Green := (Color shr 8) and $FF;
  Result.Red  := Color and $FF;
end;

class function TGIFColorMap.RGB2Color(Color: TGIFColor): TColor;
begin
  Result := (Color.Blue SHL 16) OR (Color.Green SHL 8) OR Color.Red;
end;

procedure TGIFColorMap.SaveToStream(Stream: TStream);
var
  Dummies		: integer;
  Dummy			: TGIFColor;
begin
  if (FCount = 0) then
    exit;
  Stream.WriteBuffer(FColorMap^, FCount*sizeof(TGIFColor));
  Dummies := (1 SHL BitsPerPixel)-FCount;
  Dummy.Red := 0;
  Dummy.Green := 0;
  Dummy.Blue := 0;
  while (Dummies > 0) do
  begin
    Stream.WriteBuffer(Dummy, sizeof(TGIFColor));
    dec(Dummies);
  end;
end;

procedure TGIFColorMap.LoadFromStream(Stream: TStream; Count: integer);
begin
  Clear;
  SetCapacity(Count);
  ReadCheck(Stream, FColorMap^, Count*sizeof(TGIFColor));
  FCount := Count;
end;

function TGIFColorMap.IndexOf(Color: TColor): integer;
var
  RGB			: TGIFColor;
begin
  RGB := Color2RGB(Color);
  if (FOptimized) then
  begin
    // Optimized palette has most frequently occuring entries first
    Result := 0;
    // Reverse search to (hopefully) check latest colors first
    while (Result < FCount) do
      with (FColorMap^[Result]) do
      begin
        if (RGB.Red = Red) and (RGB.Green = Green) and (RGB.Blue = Blue) then
          exit;
        Inc(Result);
      end;
    Result := -1;
  end else
  begin
    Result := FCount-1;
    // Reverse search to (hopefully) check latest colors first
    while (Result >= 0) do
      with (FColorMap^[Result]) do
      begin
        if (RGB.Red = Red) and (RGB.Green = Green) and (RGB.Blue = Blue) then
          exit;
        Dec(Result);
      end;
  end;
end;

procedure TGIFColorMap.SetCapacity(Size: integer);
begin
  if (Size >= FCapacity) then
  begin
    if (Size <= InitColorMapSize) then
      FCapacity := InitColorMapSize
    else
      FCapacity := (Size + DeltaColorMapSize - 1) DIV DeltaColorMapSize * DeltaColorMapSize;
    if (FCapacity > GIFMaxColors) then
      FCapacity := GIFMaxColors;
    ReallocMem(FColorMap, FCapacity * sizeof(TGIFColor));
  end;
end;

procedure TGIFColorMap.ImportPalette(Palette: HPalette);
type
  PalArray =  array[byte] of TPaletteEntry;
var
  Pal			: PalArray;
  NewCount		: integer;
  i			: integer;
begin
  Clear;
  NewCount := GetPaletteEntries(Palette, 0, 256, pal);
  if (NewCount = 0) then
    exit;
  SetCapacity(NewCount);
  for i := 0 to NewCount-1 do
    with FColorMap[i], Pal[i] do
    begin
      Red := peRed;
      Green := peGreen;
      Blue := peBlue;
    end;
  FCount := NewCount;
  Changed;
end;

procedure TGIFColorMap.ImportColorMap(Map: TColorMap; Count: integer);
begin
  Clear;
  if (Count = 0) then
    exit;
  SetCapacity(Count);
  FCount := Count;

  System.Move(Map, FColorMap^, FCount * sizeof(TGIFColor));

  Changed;
end;

procedure TGIFColorMap.ImportColorTable(Pal: pointer; Count: integer);
var
  i			: integer;
begin
  Clear;
  if (Count = 0) then
    exit;
  SetCapacity(Count);
  for i := 0 to Count-1 do
    with FColorMap[i], PRGBQuadArray(Pal)[i] do
    begin
      Red := rgbRed;
      Green := rgbGreen;
      Blue := rgbBlue;
    end;
  FCount := Count;
  Changed;
end;

procedure TGIFColorMap.ImportDIBColors(Handle: HDC);
var
  Pal			: Pointer;
  NewCount		: integer;
begin
  Clear;
  GetMem(Pal, sizeof(TRGBQuad) * 256);
  try
    NewCount := GetDIBColorTable(Handle, 0, 256, Pal^);
    ImportColorTable(Pal, NewCount);
  finally
    FreeMem(Pal);
  end;
  Changed;
end;

function TGIFColorMap.ExportPalette: HPalette;
var
  Pal			: TMaxLogPalette;
  i			: Integer;
begin
  if (Count = 0) then
  begin
    Result := 0;
    exit;
  end;
  Pal.palVersion := $300;
  Pal.palNumEntries := Count;
  for i := 0 to Count-1 do
    with FColorMap[i], Pal.palPalEntry[i] do
    begin
      peRed := Red;
      peGreen := Green;
      peBlue := Blue;
      peFlags := PC_NOCOLLAPSE; // ***FIXME** Not quite sure what this should be
    end;
  Result := CreatePalette(PLogPalette(@Pal)^);
end;

function TGIFColorMap.Add(Color: TColor): integer;
begin
  // Look up color before add (same as IndexOf)
  Result := IndexOf(Color);
  if (Result >= 0) then
    // Color already in map
    exit;

  if (FCount >= GIFMaxColors) then
    // Color map full
    Error(sTooManyColors);

  Result := FCount;
  if (Result >= FCapacity) then
    SetCapacity(FCount+1);
  FColorMap^[FCount] := Color2RGB(Color);
  inc(FCount);
  FOptimized := False;
  Changed;
end;

procedure TGIFColorMap.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= FCount) then
    // Color index out of range
    Error(sBadColorIndex);
  dec(FCount);
  if (Index < FCount) then
    System.Move(FColorMap^[Index + 1], FColorMap^[Index], (FCount - Index)* sizeof(TGIFColor));
  FOptimized := False;
  Changed;
end;

function TGIFColorMap.GetColor(Index: integer): TColor;
begin
  if (Index < 0) or (Index >= FCount) then
  begin
    // Color index out of range
    Warning(gsWarning, sBadColorIndex);
    // Raise an exception if the color map is empty
    if (FCount = 0) then
      Error(sEmptyColorMap);
    // Default to color index 0
    Index := 0;
  end;
  Result := RGB2Color(FColorMap^[Index]);
end;

procedure TGIFColorMap.SetColor(Index: integer; Value: TColor);
begin
  if (Index < 0) or (Index >= FCount) then
    // Color index out of range
    Error(sBadColorIndex);
  FColorMap^[Index] := Color2RGB(Value);
  Changed;
end;

type
  TUsageCount = record
    Count		: integer;	// # of pixels using color index
    Index		: integer;	// Color index
  end;

function TGIFColorMap.DoOptimize(Image: TGIFSubImage; CleanUp: Boolean): boolean;
var
  Pixel			,
  LastPixel		: PChar;
  Usage			: array[0..255] of TUsageCount;
  TempMap		: array[0..255] of TGIFColor;
  ReverseMap		: array[0..255] of BYTE;
  i			: integer;
  LastFound		: boolean;
  NewCount		: integer;
  T			: TUsageCount;
  Pivot			: integer;

  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
  begin
    repeat
      Lo := iLo;
      Hi := iHi;
      Pivot := Usage[(iLo + iHi) SHR 1].Count;
      repeat
        while (Usage[Lo].Count - Pivot > 0) do inc(Lo);
        while (Usage[Hi].Count - Pivot < 0) do dec(Hi);
        if (Lo <= Hi) then
        begin
          T := Usage[Lo];
          Usage[Lo] := Usage[Hi];
          Usage[Hi] := T;
          inc(Lo);
          dec(Hi);
        end;
      until (Lo > Hi);
      if (iLo < Hi) then
        QuickSort(iLo, Hi);
      iLo := Lo;
    until (Lo >= iHi);
  end;

begin
  if (FCount <= 1) then
  begin
    Result := False;
    exit;
  end;

  FOptimized := True;
  Result := True;

  for i := 0 to FCount-1 do
  begin
    Usage[i].Index := i;
    Usage[i].Count := 0;
  end;

  Pixel := Image.data;
  LastPixel := Pixel + Image.Width * Image.Height;

  (*
  ** Sum up usage count for each color
  *)
  while (Pixel < LastPixel) do
  begin
    inc(Usage[ord(Pixel^)].Count);
    inc(Pixel);
  end;

  (*
  **  Sort according to usage count
  *)
  QuickSort(0, FCount-1);

  (*
  ** Test for table already sorted
  *)
  for i := 0 to FCount-1 do
    if (Usage[i].Index <> i) then
      break;
  if (i = FCount) then
    exit;

  (*
  ** Build old to new map
  *)
  for i := 0 to FCount-1 do
    ReverseMap[Usage[i].Index] := i;

  (*
  **  Reorder all pixel to new map
  *)
  Pixel := Image.data;
  while (Pixel < LastPixel) do
  begin
    Pixel^ := chr(ReverseMap[ord(Pixel^)]);
    inc(Pixel);
  end;

  (*
  **  Reorder transparent colors
  *)
  if (Image.Transparent) then
    Image.GraphicControlExtension.TransparentColorIndex :=
      ReverseMap[Image.GraphicControlExtension.TransparentColorIndex];
{
  for i := 0 to Image.Extensions.Count-1 do
    if (Image.Extensions[i] is TGIFGraphicControlExtension) then
    begin
      if (TGIFGraphicControlExtension(Image.Extensions[i]).Transparent) then
        TGIFGraphicControlExtension(Image.Extensions[i]).TransparentColorIndex :=
          ReverseMap[TGIFGraphicControlExtension(Image.Extensions[i]).TransparentColorIndex];
      break;
    end;
}
  (*
  **  Reorder colormap
  *)
  LastFound := False;
  NewCount := 0;
  Move(FColorMap^, TempMap, FCount * sizeof(TGIFColor));
  for i := 0 to FCount-1 do
  begin
    FColorMap^[ReverseMap[i]] := TempMap[i];
    // Find last used color index
    if (Usage[i].Count = 0) and not(LastFound) then
    begin
      LastFound := True;
      if (CleanUp) then
        NewCount := i;
    end;
  end;

  if (CleanUp) then
    FCount := NewCount;

  Changed;
end;

function TGIFColorMap.GetBitsPerPixel: integer;
begin
  Result := Colors2bpp(FCount);
end;

procedure TGIFColorMap.Assign(Source: TPersistent);
begin
  if (Source is TGIFColorMap) then
  begin
    Clear;
    FCapacity := TGIFColorMap(Source).FCapacity;
    FCount := TGIFColorMap(Source).FCount;
    FOptimized := TGIFColorMap(Source).FOptimized;
    FColorMap := AllocMem(FCapacity * sizeof(TGIFColor));
    System.Move(TGIFColorMap(Source).FColorMap^, FColorMap^, FCount * sizeof(TGIFColor));
    Changed;
  end else
    inherited Assign(Source);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFItem
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFItem.Create(GIFImage: TGIFImage);
begin
  inherited Create;

  FGIFImage := GIFImage;
end;

procedure TGIFItem.Warning(Severity: TGIFSeverity; Message: string);
begin
  FGIFImage.Warning(self, Severity, Message);
end;

function TGIFItem.GetVersion: TGIFVersion;
begin
  Result := gv87a;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFList
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFList.Create(Image: TGIFImage);
begin
  inherited Create;
  FImage := Image;
  FItems := TList.Create;
end;

destructor TGIFList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TGIFList.GetItem(Index: Integer): TGIFItem;
begin
  Result := TGIFItem(FItems[Index]);
end;

procedure TGIFList.SetItem(Index: Integer; Item: TGIFItem);
begin
  FItems[Index] := Item;
end;

function TGIFList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGIFList.Add(Item: TGIFItem): Integer;
begin
  Result := FItems.Add(Item);
end;

procedure TGIFList.Clear;
begin
  while (FItems.Count > 0) do
    Delete(0);
end;

procedure TGIFList.Delete(Index: Integer);
begin
  TGIFItem(FItems[Index]).Free;
  FItems.Delete(Index);
end;

procedure TGIFList.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TGIFList.First: TGIFItem;
begin
  Result := TGIFItem(FItems.First);
end;

function TGIFList.IndexOf(Item: TGIFItem): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

procedure TGIFList.Insert(Index: Integer; Item: TGIFItem);
begin
  FItems.Insert(Index, Item);
end;

function TGIFList.Last: TGIFItem;
begin
  Result := TGIFItem(FItems.Last);
end;

procedure TGIFList.Move(CurIndex, NewIndex: Integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function TGIFList.Remove(Item: TGIFItem): Integer;
begin
  Result := FItems.Remove(Item);
end;

procedure TGIFList.SaveToStream(Stream: TStream);
var
  i			: integer;
begin
  for i := 0 to FItems.Count-1 do
    TGIFItem(FItems[i]).SaveToStream(Stream);
end;

procedure TGIFList.Warning(Severity: TGIFSeverity; Message: string);
begin
  Image.Warning(self, Severity, Message);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFGlobalColorMap
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFGlobalColorMap = class(TGIFColorMap)
  private
    FHeader	: TGIFHeader;
  protected
    procedure Warning(Severity: TGIFSeverity; Message: string); override;
  public
    constructor Create(HeaderItem: TGIFHeader);
    function Optimize: boolean; override;
    procedure Changed; override;
  end;

constructor TGIFGlobalColorMap.Create(HeaderItem: TGIFHeader);
begin
  Inherited Create;
  FHeader := HeaderItem;
end;

procedure TGIFGlobalColorMap.Warning(Severity: TGIFSeverity; Message: string);
begin
  FHeader.Image.Warning(self, Severity, Message);
end;

function TGIFGlobalColorMap.Optimize: boolean;
begin
  { Optimize with first image, Remove unused colors if only one image }
  if (FHeader.Image.Images.Count > 0) then
    Result := DoOptimize(TGIFSubImage(FHeader.Image.Images.First), (FHeader.Image.Images.Count = 1))
  else
    Result := False;
end;

procedure TGIFGlobalColorMap.Changed;
begin
  FHeader.Image.Palette := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFHeader
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFHeader.Create(GIFImage: TGIFImage);
begin
  inherited Create(GIFImage);
  FColorMap := TGIFGlobalColorMap.Create(self);
  FLogicalScreenDescriptor.ScreenWidth := 0;
  FLogicalScreenDescriptor.ScreenHeight := 0;
  FLogicalScreenDescriptor.PackedFields := 0;
  FLogicalScreenDescriptor.BackgroundColorIndex := 0;
  FLogicalScreenDescriptor.AspectRatio := 0;
end;

destructor TGIFHeader.Destroy;
begin
  FColorMap.Free;
  inherited Destroy;
end;

procedure TGIFHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TGIFHeader then
    with TGIFHeader(Dest) do
    begin
      ColorMap.Assign(Self.ColorMap);
      FLogicalScreenDescriptor := self.FLogicalScreenDescriptor;
    end
  else
    inherited AssignTo(Dest);
end;

type
  TGIFHeaderRec = packed record
    Signature: array[0..2] of char; { contains 'GIF' }
    Version: TGIFVersionRec;   { '87a' or '89a' }
  end;

const
  { logical screen descriptor packed field masks }
  lsdGlobalColorTable	= $80;		{ set if global color table follows L.S.D. }
  lsdColorResolution	= $70;		{ Color resolution - 3 bits }
  lsdSort		= $08;		{ set if global color table is sorted - 1 bit }
  lsdColorTableSize	= $07;		{ size of global color table - 3 bits }
  					{ Actual size = 2^value+1    - value is 3 bits }
procedure TGIFHeader.Prepare;
var
  pack			: BYTE;
begin
  Pack := $00;
  if (ColorMap.Count > 0) then
  begin
    Pack := lsdGlobalColorTable;
    if (ColorMap.Optimized) then
      Pack := Pack OR lsdSort;
  end;
  // Note: The SHL below was SHL 5 in the original source, but that looks wrong
  Pack := Pack OR ((Image.ColorResolution SHL 4) AND lsdColorResolution);
  Pack := Pack OR ((Image.BitsPerPixel-1) AND lsdColorTableSize);
  FLogicalScreenDescriptor.PackedFields := Pack;
end;

procedure TGIFHeader.SaveToStream(Stream: TStream);
var
  GifHeader		: TGIFHeaderRec;
  v			: TGIFVersion;
begin
  v := Image.Version;
  if (v = gvUnknown) then
    Error(sBadVersion);

  GifHeader.Signature := 'GIF';
  GifHeader.Version := GIFVersions[v];

  Prepare;
  Stream.Write(GifHeader, sizeof(GifHeader));
  Stream.Write(FLogicalScreenDescriptor, sizeof(FLogicalScreenDescriptor));
  if (FLogicalScreenDescriptor.PackedFields AND lsdGlobalColorTable = lsdGlobalColorTable) then
    ColorMap.SaveToStream(Stream);
end;

procedure TGIFHeader.LoadFromStream(Stream: TStream);
var
  GifHeader		: TGIFHeaderRec;
  ColorCount		: integer;
  Position		: integer;
begin
  Position := Stream.Position;
  ReadCheck(Stream, GifHeader, sizeof(GifHeader));
  if (uppercase(GifHeader.Signature) <> 'GIF') then
  begin
    // Attempt recovery in case we are reading a GIF stored in a form by rxLib
    Stream.Position := Position;
    // Seek past size stored in stream
    Stream.Seek(sizeof(longInt), soFromCurrent);
    // Attempt to read signature again
    ReadCheck(Stream, GifHeader, sizeof(GifHeader));
    if (uppercase(GifHeader.Signature) <> 'GIF') then
      Error(sBadSignature);
  end;

  ReadCheck(Stream, FLogicalScreenDescriptor, sizeof(FLogicalScreenDescriptor));

  if (FLogicalScreenDescriptor.PackedFields AND lsdGlobalColorTable = lsdGlobalColorTable) then
  begin
    ColorCount := 2 SHL (FLogicalScreenDescriptor.PackedFields AND lsdColorTableSize);
    if (ColorCount < 2) or (ColorCount > 256) then
      Error(sScreenBadColorSize);
    ColorMap.LoadFromStream(Stream, ColorCount)
  end else
    ColorMap.Clear;
end;

function TGIFHeader.GetVersion: TGIFVersion;
begin
  if (FColorMap.Optimized) or (AspectRatio <> 0) then
    Result := gv89a
  else
    Result := inherited GetVersion;
end;

function TGIFHeader.GetBackgroundColor: TColor;
begin
  Result := FColorMap[BackgroundColorIndex];
end;

procedure TGIFHeader.SetBackgroundColor(Color: TColor);
var
  Index			: integer;
begin
  Index := FColorMap.IndexOf(Color);
  if (Index = -1) then
    Index := FColorMap.Add(Color);
  BackgroundColorIndex := Index;
end;

procedure TGIFHeader.SetBackgroundColorIndex(Index: BYTE);
begin
  if ((Index >= FColorMap.Count) and (FColorMap.Count > 0)) then
  begin
    Warning(gsWarning, sBadColorIndex);
    Index := 0;
  end;
  FLogicalScreenDescriptor.BackgroundColorIndex := Index;
end;

function TGIFHeader.GetBitsPerPixel: integer;
begin
  Result := FColorMap.BitsPerPixel;
end;

function TGIFHeader.GetColorResolution: integer;
begin
  Result := FColorMap.BitsPerPixel-1;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFLocalColorMap
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFLocalColorMap = class(TGIFColorMap)
  private
    FSubImage		: TGIFSubImage;
  protected
    procedure Warning(Severity: TGIFSeverity; Message: string); override;
  public
    constructor Create(SubImage: TGIFSubImage);
    function Optimize: boolean; override;
    procedure Changed; override;
  end;

constructor TGIFLocalColorMap.Create(SubImage: TGIFSubImage);
begin
  Inherited Create;
  FSubImage := SubImage;
end;

procedure TGIFLocalColorMap.Warning(Severity: TGIFSeverity; Message: string);
begin
  FSubImage.Image.Warning(self, Severity, Message);
end;

function TGIFLocalColorMap.Optimize: boolean;
begin
  Result := DoOptimize(FSubImage, True);
end;

procedure TGIFLocalColorMap.Changed;
begin
  FSubImage.Palette := 0;
end;


////////////////////////////////////////////////////////////////////////////////
//
//			LZW Decoder
//
////////////////////////////////////////////////////////////////////////////////
const
  GIFCodeBits		= 12;			// Max number of bits per GIF token code
  GIFCodeMax		= (1 SHL GIFCodeBits)-1;// Max GIF token code
  						// 12 bits = 4095
  StackSize		= (2 SHL GIFCodeBits);	// Size of decompression stack
  TableSize		= (1 SHL GIFCodeBits);	// Size of decompression table

procedure TGIFSubImage.Decompress(Stream: TStream);
var
  table0		: array[0..TableSize-1] of integer;
  table1		: array[0..TableSize-1] of integer;
  firstcode, oldcode	: integer;
  buf			: array[0..257] of BYTE;

  Dest			: PChar;
  v			,
  xpos, ypos, pass	: integer;

  stack			: array[0..StackSize-1] of integer;
  Source			: ^integer;
  BitsPerCode		: integer;		// number of CodeTableBits/code
  InitialBitsPerCode	: BYTE;

  MaxCode		: integer;		// maximum code, given BitsPerCode
  MaxCodeSize		: integer;
  ClearCode		: integer;		// Special code to signal "Clear table"
  EOFCode		: integer;		// Special code to signal EOF
  step			: integer;
  i			: integer;

  StartBit		,			// Index of bit buffer start
  LastBit		,			// Index of last bit in buffer
  LastByte		: integer;		// Index of last byte in buffer
  get_done		,
  return_clear		,
  ZeroBlock		: boolean;
  ClearValue		: BYTE;
{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStartDecompress	,
  TimeStopDecompress	: DWORD;
{$endif}

  function nextCode(BitsPerCode: integer): integer;
  const
    masks: array[0..15] of integer =
      ($0000, $0001, $0003, $0007,
       $000f, $001f, $003f, $007f,
       $00ff, $01ff, $03ff, $07ff,
       $0fff, $1fff, $3fff, $7fff);
  var
    StartIndex, EndIndex		: integer;
    ret			: integer;
    EndBit		: integer;
    count		: BYTE;
  begin
    if (return_clear) then
    begin
      return_clear := False;
      Result := ClearCode;
      exit;
    end;

    EndBit := StartBit + BitsPerCode;

    if (EndBit >= LastBit) then
    begin
      if (get_done) then
      begin
        if (StartBit >= LastBit) then
          Warning(gsWarning, sDecodeTooFewBits);
        Result := -1;
        exit;
      end;
      buf[0] := buf[LastByte-2];
      buf[1] := buf[LastByte-1];

      if (Stream.Read(count, 1) <> 1) then
      begin
        Result := -1;
        exit;
      end;
      if (count = 0) then
      begin
        ZeroBlock := True;
        get_done := TRUE;
      end else
      begin
        // Handle premature end of file
        if (Stream.Size - Stream.Position < Count) then
        begin
          Warning(gsWarning, sOutOfData);
          // Not enough data left - Just read as much as we can get
          Count := Stream.Size - Stream.Position;
        end;
        if (Count <> 0) then
          ReadCheck(Stream, Buf[2], Count);
      end;

      LastByte := 2 + count;
      StartBit := (StartBit - LastBit) + 16;
      LastBit := LastByte * 8;

      EndBit := StartBit + BitsPerCode;
    end;

    EndIndex := EndBit DIV 8;
    StartIndex := StartBit DIV 8;

    ASSERT(StartIndex <= high(buf), 'StartIndex too large');
    if (StartIndex = EndIndex) then
      ret := buf[StartIndex]
    else
      if (StartIndex + 1 = EndIndex) then
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8)
      else
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8) OR (buf[StartIndex+2] SHL 16);

    ret := (ret SHR (StartBit AND $0007)) AND masks[BitsPerCode];

    Inc(StartBit, BitsPerCode);

    Result := ret;
  end;

  function NextLZW: integer;
  var
    code, incode	: integer;
    i			: integer;
    b			: BYTE;
  begin
    code := nextCode(BitsPerCode);
    while (code >= 0) do
    begin
      if (code = ClearCode) then
      begin
        ASSERT(ClearCode < TableSize, 'ClearCode too large');
        for i := 0 to ClearCode-1 do
        begin
          table0[i] := 0;
          table1[i] := i;
        end;
        for i := ClearCode to TableSize-1 do
        begin
          table0[i] := 0;
          table1[i] := 0;
        end;
        BitsPerCode := InitialBitsPerCode+1;
        MaxCodeSize := 2 * ClearCode;
        MaxCode := ClearCode + 2;
        Source := @stack;
        repeat
          firstcode := nextCode(BitsPerCode);
          oldcode := firstcode;
        until (firstcode <> ClearCode);

        Result := firstcode;
        exit;
      end;
      if (code = EOFCode) then
      begin
        Result := -2;
        if (ZeroBlock) then
          exit;
        // Eat rest of data blocks
        if (Stream.Read(b, 1) <> 1) then
          exit;
        while (b <> 0) do
        begin
          Stream.Seek(b, soFromCurrent);
          if (Stream.Read(b, 1) <> 1) then
            exit;
        end;
        exit;
      end;

      incode := code;

      if (code >= MaxCode) then
      begin
        Source^ := firstcode;
        Inc(Source);
        code := oldcode;
      end;

      ASSERT(Code < TableSize, 'Code too large');
      while (code >= ClearCode) do
      begin
        Source^ := table1[code];
        Inc(Source);
        if (code = table0[code]) then
          Error(sDecodeCircular);
        code := table0[code];
        ASSERT(Code < TableSize, 'Code too large');
      end;

      firstcode := table1[code];
      Source^ := firstcode;
      Inc(Source);

      code := MaxCode;
      if (code <= GIFCodeMax) then
      begin
        table0[code] := oldcode;
        table1[code] := firstcode;
        Inc(MaxCode);
        if ((MaxCode >= MaxCodeSize) and (MaxCodeSize <= GIFCodeMax)) then
        begin
          MaxCodeSize := MaxCodeSize * 2;
          Inc(BitsPerCode);
        end;
      end;

      oldcode := incode;

      if (longInt(Source) > longInt(@stack)) then
      begin
        Dec(Source);
        Result := Source^;
        exit;
      end
    end;
    Result := code;
  end;

  function readLZW: integer;
  begin
    if (longInt(Source) > longInt(@stack)) then
    begin
      Dec(Source);
      Result := Source^;
    end else
      Result := NextLZW;
  end;

begin
  NewImage;

  // Clear image data in case decompress doesn't complete
  if (Transparent) then
    // Clear to transparent color
    ClearValue := GraphicControlExtension.GetTransparentColorIndex
  else
    // Clear to first color
    ClearValue := 0;

  FillChar(FData^, FDataSize, ClearValue);

{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStartDecompress := timeGetTime;
{$endif}

  (*
  ** Read initial code size in bits from stream
  *)
  if (Stream.Read(InitialBitsPerCode, 1) <> 1) then
    exit;

  (*
  **  Initialize the Compression routines
  *)
  BitsPerCode := InitialBitsPerCode + 1;
  ClearCode := 1 SHL InitialBitsPerCode;
  EOFCode := ClearCode + 1;
  MaxCodeSize := 2 * ClearCode;
  MaxCode := ClearCode + 2;

  StartBit := 0;
  LastBit := 0;
  LastByte := 2;

  ZeroBlock := False;
  get_done := False;
  return_clear := TRUE;

  Source := @stack;

  try
    if (Interlaced) then
    begin
      ypos := 0;
      pass := 0;
      step := 8;

      for i := 0 to Height-1 do
      begin
        Dest := FData + Width * ypos;
        for xpos := 0 to width-1 do
        begin
          v := readLZW;
          if (v < 0) then
            exit;
          Dest^ := char(v);
          Inc(Dest);
        end;
        Inc(ypos, step);
        if (ypos >= height) then
          repeat
            if (pass > 0) then
              step := step DIV 2;
            Inc(pass);
            ypos := step DIV 2;
          until (ypos < height);
      end;
    end else
    begin
      Dest := FData;
      for ypos := 0 to (height * width)-1 do
      begin
        v := readLZW;
        if (v < 0) then
          exit;
        Dest^ := char(v);
        Inc(Dest);
      end;
    end;
  finally
    if (readLZW >= 0) then
      ;
//      raise GIFException.Create('Too much input data, ignoring extra...');
  end;
{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStopDecompress := timeGetTime;
  ShowMessage(format('Decompressed %d pixels in %d mS, Rate %d pixels/mS',
    [Height*Width, TimeStopDecompress-TimeStartDecompress,
    (Height*Width) DIV (TimeStopDecompress-TimeStartDecompress+1)]));
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
//
//			LZW Encoder stuff
//
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//			LZW Encoder THashTable
////////////////////////////////////////////////////////////////////////////////
const
  HashKeyBits		= 13;			// Max number of bits per Hash Key

  HashSize		= 8009;			// Size of hash table
  						// Must be prime
                                                // Must be > than HashMaxCode
                                                // Must be < than HashMaxKey

  HashKeyMax		= (1 SHL HashKeyBits)-1;// Max hash key value
  						// 13 bits = 8191

  HashKeyMask		= HashKeyMax;		// $1FFF
  GIFCodeMask		= GIFCodeMax;		// $0FFF

  HashEmpty		= $000FFFFF;		// 20 bits

type
  // A Hash Key is 20 bits wide.
  // - The lower 8 bits are the postfix character (the new pixel).
  // - The upper 12 bits are the prefix code (the GIF token).
  // A KeyInt must be able to represent the integer values -1..(2^20)-1
  KeyInt = longInt;	// 32 bits
  CodeInt = SmallInt;	// 16 bits

  THashArray = array[0..HashSize-1] of KeyInt;
  PHashArray = ^THashArray;

  THashTable = class
{$ifdef DEBUG_HASHPERFORMANCE}
    CountLookupFound	: longInt;
    CountMissFound	: longInt;
    CountLookupNotFound	: longInt;
    CountMissNotFound	: longInt;
{$endif}
    HashTable: PHashArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Insert(Key: KeyInt; Code: CodeInt);
    function Lookup(Key: KeyInt): CodeInt;
  end;

function HashKey(Key: KeyInt): CodeInt;
begin
  Result := ((Key SHR (GIFCodeBits-8)) XOR Key) MOD HashSize;
end;

function NextHashKey(HKey: CodeInt): CodeInt;
var
  disp		: CodeInt;
begin
  (*
  ** secondary hash (after G. Knott)
  *)
  disp := HashSize - HKey;
  if (HKey = 0) then
    disp := 1;
//  disp := 13;		// disp should be prime relative to HashSize, but
			// it doesn't seem to matter here...
  dec(HKey, disp);
  if (HKey < 0) then
    inc(HKey, HashSize);
  Result := HKey;
end;


constructor THashTable.Create;
begin
  ASSERT(longInt($FFFFFFFF) = -1, 'TGIFImage implementation assumes $FFFFFFFF = -1');

  inherited Create;
  GetMem(HashTable, sizeof(THashArray));
  Clear;
{$ifdef DEBUG_HASHPERFORMANCE}
  CountLookupFound := 0;
  CountMissFound := 0;
  CountLookupNotFound := 0;
  CountMissNotFound := 0;
{$endif}
end;

destructor THashTable.Destroy;
begin
{$ifdef DEBUG_HASHPERFORMANCE}
  ShowMessage(
    Format('Found: %d  HitRate: %.2f',
      [CountLookupFound, (CountLookupFound+1)/(CountMissFound+1)])+#13+
    Format('Not found: %d  HitRate: %.2f',
      [CountLookupNotFound, (CountLookupNotFound+1)/(CountMissNotFound+1)]));
{$endif}
  FreeMem(HashTable);
  inherited Destroy;
end;

// Clear hash table and fill with empty slots (doh!)
procedure THashTable.Clear;
{$ifdef DEBUG_HASHFILLFACTOR}
var
  i			,
  Count			: longInt;
{$endif}
begin
{$ifdef DEBUG_HASHFILLFACTOR}
  Count := 0;
  for i := 0 to HashSize-1 do
    if (HashTable[i] SHR GIFCodeBits <> HashEmpty) then
      inc(Count);
  ShowMessage(format('Size: %d, Filled: %d, Rate %.4f',
    [HashSize, Count, Count/HashSize]));
{$endif}

  FillChar(HashTable^, sizeof(THashArray), $FF);
end;

// Insert new key/value pair into hash table
procedure THashTable.Insert(Key: KeyInt; Code: CodeInt);
var
  HKey			: CodeInt;
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

  // Scan for empty slot
  // while (HashTable[HKey] SHR GIFCodeBits <> HashEmpty) do { Unoptimized }
  while (HashTable[HKey] AND (HashEmpty SHL GIFCodeBits) <> (HashEmpty SHL GIFCodeBits)) do { Optimized }
    HKey := NextHashKey(HKey);
  // Fill slot with key/value pair
  HashTable[HKey] := (Key SHL GIFCodeBits) OR (Code AND GIFCodeMask);
end;

// Search for key in hash table.
// Returns value if found or -1 if not
function THashTable.Lookup(Key: KeyInt): CodeInt;
var
  HKey			: CodeInt;
  HTKey			: KeyInt;
{$ifdef DEBUG_HASHPERFORMANCE}
  n			: LongInt;
{$endif}
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

{$ifdef DEBUG_HASHPERFORMANCE}
  n := 0;
{$endif}
  // Scan table for key
  // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
  Key := Key SHL GIFCodeBits; { Optimized }
  HTKey := HashTable[HKey] AND (HashEmpty SHL GIFCodeBits); { Optimized }
  // while (HTKey <> HashEmpty) do { Unoptimized }
  while (HTKey <> HashEmpty SHL GIFCodeBits) do { Optimized }
  begin
    if (Key = HTKey) then
    begin
      // Extract and return value
      Result := HashTable[HKey] AND GIFCodeMask;
{$ifdef DEBUG_HASHPERFORMANCE}
      inc(CountLookupFound);
      inc(CountMissFound, n);
{$endif}
      exit;
    end;
{$ifdef DEBUG_HASHPERFORMANCE}
    inc(n);
{$endif}
    // Try next slot
    HKey := NextHashKey(HKey);
    // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
    HTKey := HashTable[HKey] AND (HashEmpty SHL GIFCodeBits); { Optimized }
  end;
  // Found empty slot - key doesn't exist
  Result := -1;
{$ifdef DEBUG_HASHPERFORMANCE}
  inc(CountLookupNotFound);
  inc(CountMissNotFound, n);
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFStream - Abstract GIF block stream
//
// Descendants from TGIFStream either reads or writes data in blocks
// of up to 255 bytes. These blocks are organized as a leading byte
// containing the number of bytes in the block (exclusing the count
// byte itself), followed by the data (up to 254 bytes of data).
////////////////////////////////////////////////////////////////////////////////
type
  TGIFStream = class(TStream)
  private
    FOnWarning		: TGIFWarning;
    FStream		: TStream;
    FOnProgress		: TNotifyEvent;
    FBuffer		: array [BYTE] of Char;
    FBufferCount	: integer;

  protected
    constructor Create(Stream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    property Warning: TGIFWarning read FOnWarning write FOnWarning;
  end;

constructor TGIFStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FBufferCount := 1; // Reserve first byte of buffer for length
end;

procedure TGIFStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

function TGIFStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create(sInvalidStream);
end;

function TGIFStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise Exception.Create(sInvalidStream);
end;

function TGIFStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise Exception.Create(sInvalidStream);
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFReader - GIF block reader
////////////////////////////////////////////////////////////////////////////////
type
  TGIFReader = class(TGIFStream)
  public
    constructor Create(Stream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
  end;

constructor TGIFReader.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FBufferCount := 0;
end;

function TGIFReader.Read(var Buffer; Count: Longint): Longint;
var
  n			: integer;
  Dst			: PChar;
  size			: BYTE;
begin
  Dst := @Buffer;
  Result := 0;

  while (Count > 0) do
  begin
    // Get data from buffer
    while (FBufferCount > 0) and (Count > 0) do
    begin
      if (FBufferCount > Count) then
        n := Count
      else
        n := FBufferCount;
      Move(FBuffer, Dst^, n);
      dec(FBufferCount, n);
      dec(Count, n);
      inc(Result, n);
      inc(Dst, n);
    end;

    // Refill buffer when it becomes empty
    if (FBufferCount <= 0) then
    begin
      FStream.Read(size, 1);
      // ***FIXME*** Can this be handled as a warning instead?
      if (size >= 255) then
        Error('GIF block too large');
      FBufferCount := size;
      if (FBufferCount > 0) then
      begin
        n := FStream.Read(FBuffer, size);
        if (n = FBufferCount) then
        begin
          Warning(self, gsWarning, sOutOfData);
          break;
        end;
      end else
        break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFWriter - GIF block writer
////////////////////////////////////////////////////////////////////////////////
type
  TGIFWriter = class(TGIFStream)
  private
    FOutputDirty	: boolean;

  protected
    procedure FlushBuffer;

  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteByte(Value: BYTE): Longint;
  end;

constructor TGIFWriter.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FBufferCount := 1; // Reserve first byte of buffer for length
  FOutputDirty := False;
end;

destructor TGIFWriter.Destroy;
begin
  inherited Destroy;
  if (FOutputDirty) then
    FlushBuffer;
end;

procedure TGIFWriter.FlushBuffer;
begin
  if (FBufferCount <= 0) then
    exit;

  FBuffer[0] := char(FBufferCount-1); // Block size excluding the count
  FStream.WriteBuffer(FBuffer, FBufferCount);
  FBufferCount := 1; // Reserve first byte of buffer for length
  FOutputDirty := False;
end;

function TGIFWriter.Write(const Buffer; Count: Longint): Longint;
var
  n			: integer;
  Src			: PChar;
begin
  Result := Count;
  FOutputDirty := True;
  Src := @Buffer;
  while (Count > 0) do
  begin
    // Move data to the internal buffer in 255 byte chunks
    while (FBufferCount < sizeof(FBuffer)) and (Count > 0) do
    begin
      n := sizeof(FBuffer) - FBufferCount;
      if (n > Count) then
        n := Count;
      Move(Src^, FBuffer[FBufferCount], n);
      inc(Src, n);
      inc(FBufferCount, n);
      dec(Count, n);
    end;

    // Flush the buffer when it is full
    if (FBufferCount >= sizeof(FBuffer)) then
      FlushBuffer;
  end;
end;

function TGIFWriter.WriteByte(Value: BYTE): Longint;
begin
  Result := Write(Value, 1);
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFEncoder - Abstract encoder
////////////////////////////////////////////////////////////////////////////////
type
  TGIFEncoder = class(TObject)
  protected
    FOnWarning		: TGIFWarning;
    MaxColor		: integer;
    BitsPerPixel	: BYTE;		// Bits per pixel of image
    Stream		: TStream;	// Output stream
    Width		,		// Width of image in pixels
    Height		: integer;	// height of image in pixels
    Interlace		: boolean;	// Interlace flag (True = interlaced image)
    Data		: PChar;	// Pointer to pixel data
    GIFStream		: TGIFWriter;	// Output buffer

    OutputBucket	: longInt;	// Output bit bucket
    OutputBits		: integer;	// Current # of bits in bucket

    ClearFlag		: Boolean;	// True if dictionary has just been cleared
    BitsPerCode		,		// Current # of bits per code
    InitialBitsPerCode	: integer;	// Initial # of bits per code after
  					// dictionary has been cleared
    MaxCode		: CodeInt;	// maximum code, given BitsPerCode
    ClearCode		: CodeInt;	// Special output code to signal "Clear table"
    EOFCode		: CodeInt;	// Special output code to signal EOF
    BaseCode		: CodeInt;	// ...

    Pixel		: PChar;	// Pointer to current pixel

    cX			,		// Current X counter (Width - X)
    Y			: integer;	// Current Y
    Pass		: integer;	// Interlace pass

    function MaxCodesFromBits(Bits: integer): CodeInt;
    procedure Output(Value: integer); virtual;
    procedure Clear; virtual;
    function BumpPixel: boolean;
    procedure DoCompress; virtual; abstract;
  public
    procedure Compress(AStream: TStream; ABitsPerPixel: integer;
      AWidth, AHeight: integer; AInterlace: boolean; AData: PChar; AMaxColor: integer);
    property Warning: TGIFWarning read FOnWarning write FOnWarning;
  end;

// Calculate the maximum number of codes that a given number of bits can represent
// MaxCodes := (1^bits)-1
function TGIFEncoder.MaxCodesFromBits(Bits: integer): CodeInt;
begin
  Result := (CodeInt(1) SHL Bits) - 1;
end;

// Stuff bits (variable sized codes) into a buffer and output them
// a byte at a time
procedure TGIFEncoder.Output(Value: integer);
const
  BitBucketMask: array[0..16] of longInt =
    ($0000,
     $0001, $0003, $0007, $000F,
     $001F, $003F, $007F, $00FF,
     $01FF, $03FF, $07FF, $0FFF,
     $1FFF, $3FFF, $7FFF, $FFFF);
begin
  if (OutputBits > 0) then
    OutputBucket :=
      (OutputBucket AND BitBucketMask[OutputBits]) OR (longInt(Value) SHL OutputBits)
  else
    OutputBucket := Value;

  inc(OutputBits, BitsPerCode);

  while (OutputBits >= 8) do
  begin
    GIFStream.WriteByte(OutputBucket AND $FF);
    OutputBucket := OutputBucket SHR 8;
    dec(OutputBits, 8);
  end;

  if (Value = EOFCode) then
  begin
    // At EOF, write the rest of the buffer.
    while (OutputBits > 0) do
    begin
      GIFStream.WriteByte(OutputBucket AND $FF);
      OutputBucket := OutputBucket SHR 8;
      dec(OutputBits, 8);
    end;
  end;
end;

procedure TGIFEncoder.Clear;
begin
  // just_cleared = 1;
  ClearFlag := TRUE;
  Output(ClearCode);
end;

// Bump (X,Y) and data pointer to point to the next pixel
function TGIFEncoder.BumpPixel: boolean;
begin
  // Bump the current X position
  dec(cX);

  // If we are at the end of a scan line, set cX back to the beginning
  // If we are interlaced, bump Y to the appropriate spot, otherwise,
  // just increment it.
  if (cX <= 0) then
  begin

    if not(Interlace) then
    begin
      // Done - no more data
      Result := False;
      exit;
    end;

    cX := Width;
    case (Pass) of
      0:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 4;
          end;
        end;
      1:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 2;
          end;
        end;
      2:
        begin
          inc(Y, 4);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 1;
          end;
        end;
      3:
        inc(Y, 2);
    end;

    if (Y >= height) then
    begin
      // Done - No more data
      Result := False;
      exit;
    end;
    Pixel := Data + (Y * Width);
  end;
  Result := True;
end;


procedure TGIFEncoder.Compress(AStream: TStream; ABitsPerPixel: integer;
  AWidth, AHeight: integer; AInterlace: boolean; AData: PChar; AMaxColor: integer);
const
  EndBlockByte		= $00;			// End of block marker
{$ifdef DEBUG_COMPRESSPERFORMANCE}
var
  TimeStartCompress	,
  TimeStopCompress	: DWORD;
{$endif}
begin
  MaxColor := AMaxColor;
  Stream := AStream;
  BitsPerPixel := ABitsPerPixel;
  Width := AWidth;
  Height := AHeight;
  Interlace := AInterlace;
  Data := AData;

  if (BitsPerPixel <= 1) then
    BitsPerPixel := 2;

  InitialBitsPerCode := BitsPerPixel + 1;
  Stream.Write(BitsPerPixel, 1);

  // out_bits_init = init_bits;
  BitsPerCode := InitialBitsPerCode;
  MaxCode := MaxCodesFromBits(BitsPerCode);

  ClearCode := (1 SHL (InitialBitsPerCode - 1));
  EOFCode := ClearCode + 1;
  BaseCode := EOFCode + 1;

  // Clear bit bucket
  OutputBucket := 0;
  OutputBits  := 0;

  // Reset pixel counter
  if (Interlace) then
    cX := Width
  else
    cX := Width*Height;
  // Reset row counter
  Y := 0;
  Pass := 0;

  GIFStream := TGIFWriter.Create(AStream);
  try
    GIFStream.Warning := Warning;
    if (Data <> nil) and (Height > 0) and (Width > 0) then
    begin
{$ifdef DEBUG_COMPRESSPERFORMANCE}
      TimeStartCompress := timeGetTime;
{$endif}

      // Call compress implementation
      DoCompress;

{$ifdef DEBUG_COMPRESSPERFORMANCE}
      TimeStopCompress := timeGetTime;
      ShowMessage(format('Compressed %d pixels in %d mS, Rate %d pixels/mS',
        [Height*Width, TimeStopCompress-TimeStartCompress,
        DWORD(Height*Width) DIV (TimeStopCompress-TimeStartCompress+1)]));
{$endif}
      // Output the final code.
      Output(EOFCode);
    end else
      // Output the final code (and nothing else).
      TGIFEncoder(self).Output(EOFCode);
  finally
    GIFStream.Free;
  end;

  WriteByte(Stream, EndBlockByte);
end;

////////////////////////////////////////////////////////////////////////////////
//		TRLEEncoder - RLE encoder
////////////////////////////////////////////////////////////////////////////////
type
  TRLEEncoder = class(TGIFEncoder)
  private
    MaxCodes		: integer;
    OutBumpInit		,
    OutClearInit	: integer;
    Prefix		: integer;	// Current run color
    RunLengthTableMax	,
    RunLengthTablePixel	,
    OutCount		,
    OutClear		,
    OutBump		: integer;
  protected
    function ComputeTriangleCount(count: integer; nrepcodes: integer): integer;
    procedure MaxOutClear;
    procedure ResetOutClear;
    procedure FlushFromClear(Count: integer);
    procedure FlushClearOrRepeat(Count: integer);
    procedure FlushWithTable(Count: integer);
    procedure Flush(RunLengthCount: integer);
    procedure OutputPlain(Value: integer);
    procedure Clear; override;
    procedure DoCompress; override;
  end;


procedure TRLEEncoder.Clear;
begin
  OutBump := OutBumpInit;
  OutClear := OutClearInit;
  OutCount := 0;
  RunLengthTableMax := 0;

  inherited Clear;

  BitsPerCode := InitialBitsPerCode;
end;

procedure TRLEEncoder.OutputPlain(Value: integer);
begin
  ClearFlag := False;
  Output(Value);
  inc(OutCount);

  if (OutCount >= OutBump) then
  begin
    inc(BitsPerCode);
    inc(OutBump, 1 SHL (BitsPerCode - 1));
  end;

  if (OutCount >= OutClear) then
    Clear;
end;

function TRLEEncoder.ComputeTriangleCount(count: integer; nrepcodes: integer): integer;
var
  PerRepeat		: integer;
  n			: integer;

  function iSqrt(x: integer): integer;
  var
    r, v		: integer;
  begin
    if (x < 2) then
    begin
      Result := x;
      exit;
    end else
    begin
      v := x;
      r := 1;
      while (v > 0) do
      begin
        v := v DIV 4;
        r := r * 2;
      end;
    end;

    while (True) do
    begin
      v := ((x DIV r) + r) DIV 2;
      if ((v = r) or (v = r+1)) then
      begin
        Result := r;
        exit;
      end;
      r := v;
    end;
  end;

begin
  Result := 0;
  PerRepeat := (nrepcodes * (nrepcodes+1)) DIV 2;

  while (Count >= PerRepeat) do
  begin
    inc(Result, nrepcodes);
    dec(Count, PerRepeat);
  end;

  if (Count > 0) then
  begin
    n := iSqrt(Count);
    while ((n * (n+1)) >= 2*Count) do
      dec(n);
    while ((n * (n+1)) < 2*Count) do
      inc(n);
    inc(Result, n);
  end;
end;

procedure TRLEEncoder.MaxOutClear;
begin
  OutClear := MaxCodes;
end;

procedure TRLEEncoder.ResetOutClear;
begin
  OutClear := OutClearInit;
  if (OutCount >= OutClear) then
    Clear;
end;

procedure TRLEEncoder.FlushFromClear(Count: integer);
var
  n			: integer;
begin
  MaxOutClear;
  RunLengthTablePixel := Prefix;
  n := 1;
  while (Count > 0) do
  begin
    if (n = 1) then
    begin
      RunLengthTableMax := 1;
      OutputPlain(Prefix);
      dec(Count);
    end else
    if (Count >= n) then
    begin
      RunLengthTableMax := n;
      OutputPlain(BaseCode + n - 2);
      dec(Count, n);
    end else
    if (Count = 1) then
    begin
      inc(RunLengthTableMax);
      OutputPlain(Prefix);
      break;
    end else
    begin
      inc(RunLengthTableMax);
      OutputPlain(BaseCode + Count - 2);
      break;
    end;

    if (OutCount = 0) then
      n := 1
    else
      inc(n);
  end;
  ResetOutClear;
end;

procedure TRLEEncoder.FlushClearOrRepeat(Count: integer);
var
  WithClear		: integer;
begin
  WithClear := 1 + ComputeTriangleCount(Count, MaxCodes);

  if (WithClear < Count) then
  begin
    Clear;
    FlushFromClear(Count);
  end else
    while (Count > 0) do
    begin
      OutputPlain(Prefix);
      dec(Count);
    end;
end;

procedure TRLEEncoder.FlushWithTable(Count: integer);
var
  RepeatMax		,
  RepeatLeft		,
  LeftOver		: integer;
begin
  RepeatMax := Count DIV RunLengthTableMax;
  LeftOver := Count MOD RunLengthTableMax;
  if (LeftOver <> 0) then
    RepeatLeft := 1
  else
    RepeatLeft := 0;

  if (OutCount + RepeatMax + RepeatLeft > MaxCodes) then
  begin
    RepeatMax := MaxCodes - OutCount;
    LeftOver := Count - (RepeatMax * RunLengthTableMax);
    RepeatLeft := 1 + ComputeTriangleCount(LeftOver, MaxCodes);
  end;

  if (1 + ComputeTriangleCount(Count, MaxCodes) < RepeatMax + RepeatLeft) then
  begin
    Clear;
    FlushFromClear(Count);
    exit;
  end;
  MaxOutClear;

  while (RepeatMax > 0) do
  begin
    OutputPlain(BaseCode + RunLengthTableMax-2);
    dec(RepeatMax);
  end;

  if (LeftOver > 0) then
  begin
    if (ClearFlag) then
      FlushFromClear(LeftOver)
    else if (LeftOver = 1) then
      OutputPlain(Prefix)
    else
      OutputPlain(BaseCode +  LeftOver - 2);
  end;
  ResetOutClear;
end;

procedure TRLEEncoder.Flush(RunLengthCount: integer);
begin
  if (RunLengthCount = 1) then
  begin
    OutputPlain(Prefix);
    exit;
  end;

  if (ClearFlag) then
    FlushFromClear(RunLengthCount)
  else if ((RunLengthTableMax < 2) or (RunLengthTablePixel <> Prefix)) then
    FlushClearOrRepeat(RunLengthCount)
  else
    FlushWithTable(RunLengthCount);
end;

procedure TRLEEncoder.DoCompress;
var
  Color			: CodeInt;
  RunLengthCount	: integer;

begin
  OutBumpInit := ClearCode - 1;

  // For images with a lot of runs, making OutClearInit larger will
  // give better compression.
  if (BitsPerPixel <= 3) then
    OutClearInit := 9
  else
    OutClearInit := OutBumpInit - 1;

  // max_ocodes = (1 << GIFBITS) - ((1 << (out_bits_init - 1)) + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - ((1 SHL (BitsPerCode - 1)) + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - ((1 SHL (InitialBitsPerCode - 1)) + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - (ClearCode + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - (EOFCode + 2);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - (BaseCode + 1);
  // <=> MaxCodes := MaxCodesFromBits(GIFCodeBits) - BaseCode;
  MaxCodes := MaxCodesFromBits(GIFCodeBits) - BaseCode;

  Clear;
  RunLengthCount := 0;

  Pixel := Data;
  Prefix := -1; // Dummy value to make Color <> Prefix
  repeat
    // Fetch the next pixel
    Color := CodeInt(Pixel^);
    inc(Pixel);

    if (Color >= MaxColor) then
      Error(sInvalidColor);

    if (RunLengthCount > 0) and (Color <> Prefix) then
    begin
      // End of current run
      Flush(RunLengthCount);
      RunLengthCount := 0;
    end;

    if (Color = Prefix) then
      // Increment run length
      inc(RunLengthCount)
    else
    begin
      // Start new run
      Prefix := Color;
      RunLengthCount := 1;
    end;
  until not(BumpPixel);
  Flush(RunLengthCount);
end;

////////////////////////////////////////////////////////////////////////////////
//		TLZWEncoder - LZW encoder
////////////////////////////////////////////////////////////////////////////////
const
  TableMaxMaxCode	= (1 SHL GIFCodeBits);	//
  TableMaxFill		= TableMaxMaxCode-1;	// Clear table when it fills to
  						// this point.
  						// Note: Must be <= GIFCodeMax
type
  TLZWEncoder = class(TGIFEncoder)
  private
    Prefix		: CodeInt;	// Current run color
    FreeEntry		: CodeInt;	// next unused code in table
    HashTable		: THashTable;
  protected
    procedure Output(Value: integer); override;
    procedure Clear; override;
    procedure DoCompress; override;
  end;


procedure TLZWEncoder.Output(Value: integer);
begin
  inherited Output(Value);

  // If the next entry is going to be too big for the code size,
  // then increase it, if possible.
  if (FreeEntry > MaxCode) or (ClearFlag) then
  begin
    if (ClearFlag) then
    begin
      BitsPerCode := InitialBitsPerCode;
      MaxCode := MaxCodesFromBits(BitsPerCode);
      ClearFlag := False;
    end else
    begin
      inc(BitsPerCode);
      if (BitsPerCode = GIFCodeBits) then
        MaxCode := TableMaxMaxCode
      else
        MaxCode := MaxCodesFromBits(BitsPerCode);
    end;
  end;
end;

procedure TLZWEncoder.Clear;
begin
  inherited Clear;
  HashTable.Clear;
  FreeEntry := ClearCode + 2;
end;


procedure TLZWEncoder.DoCompress;
var
  Color			: char;
  NewKey		: KeyInt;
  NewCode		: CodeInt;

begin
  HashTable := THashTable.Create;
  try
    // clear hash table and sync decoder
    Clear;

    Pixel := Data;
    Prefix := CodeInt(Pixel^);
    inc(Pixel);
    if (Prefix >= MaxColor) then
      Error(sInvalidColor);
    while (BumpPixel) do
    begin
      // Fetch the next pixel
      Color := Pixel^;
      inc(Pixel);
      if (ord(Color) >= MaxColor) then
        Error(sInvalidColor);

      // Append Postfix to Prefix and lookup in table...
      NewKey := (KeyInt(Prefix) SHL 8) OR ord(Color);
      NewCode := HashTable.Lookup(NewKey);
      if (NewCode >= 0) then
      begin
        // ...if found, get next pixel
        Prefix := NewCode;
        continue;
      end;

      // ...if not found, output and start over
      Output(Prefix);
      Prefix := CodeInt(Color);

      if (FreeEntry < TableMaxFill) then
      begin
        HashTable.Insert(NewKey, FreeEntry);
        inc(FreeEntry);
      end else
        Clear;
    end;
    Output(Prefix);
  finally
    HashTable.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFSubImage
//
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////
//		TGIFSubImage.Compress
/////////////////////////////////////////////////////////////////////////
procedure TGIFSubImage.Compress(Stream: TStream);
var
  Encoder		: TGIFEncoder;
  BitsPerPixel		: BYTE;
  MaxColors		: integer;
begin
  if (ColorMap.Count > 0) then
  begin
    MaxColors := ColorMap.Count;
    BitsPerPixel := ColorMap.BitsPerPixel
  end else
  begin
    BitsPerPixel := Image.BitsPerPixel;
    MaxColors := 1 SHL BitsPerPixel;
  end;

  // Create a RLE or LZW GIF encoder
  if (Image.Compression = gcRLE) then
    Encoder := TRLEEncoder.Create
  else
    Encoder := TLZWEncoder.Create;
  try
    Encoder.Warning := Image.Warning;
    Encoder.Compress(Stream, BitsPerPixel, Width, Height, Interlaced, FData, MaxColors);
  finally
    Encoder.Free;
  end;
end;

function TGIFExtensionList.GetExtension(Index: Integer): TGIFExtension;
begin
  Result := TGIFExtension(Items[Index]);
end;

procedure TGIFExtensionList.SetExtension(Index: Integer; Extension: TGIFExtension);
begin
  Items[Index] := Extension;
end;

procedure TGIFExtensionList.LoadFromStream(Stream: TStream; Parent: TObject);
var
  b			: BYTE;
  Extension		: TGIFExtension;
  ExtensionClass	: TGIFExtensionClass;
begin
  // Peek ahead to determine block type
  if (Stream.Read(b, 1) <> 1) then
    exit;
  while not(b in [bsTrailer, bsImageDescriptor]) do
  begin
    if (b = bsExtensionIntroducer) then
    begin
      ExtensionClass := TGIFExtension.FindExtension(Stream);
      if (ExtensionClass = nil) then
        Error(sUnknownExtension);
      Stream.Seek(-1, soFromCurrent);
      Extension := ExtensionClass.Create(Parent as TGIFSubImage);
      try
        Extension.LoadFromStream(Stream);
        Add(Extension);
      except
        Extension.Free;
        raise;
      end;
    end else
    begin
      Warning(gsWarning, sBadExtensionLabel);
      break;
    end;
    if (Stream.Read(b, 1) <> 1) then
      exit;
  end;
  Stream.Seek(-1, soFromCurrent);
end;

const
  { image descriptor bit masks }
  idLocalColorTable	= $80;    { set if a local color table follows }
  idInterlaced		= $40;    { set if image is interlaced }
  idSort		= $20;    { set if color table is sorted }
  idReserved		= $0C;    { reserved - must be set to $00 }
  idColorTableSize	= $07;    { size of color table as above }

constructor TGIFSubImage.Create(GIFImage: TGIFImage);
begin
  inherited Create(GIFImage);
  FExtensions := TGIFExtensionList.Create(GIFImage);
  FColorMap := TGIFLocalColorMap.Create(self);
  FImageDescriptor.Separator := bsImageDescriptor;
  FImageDescriptor.Left := 0;
  FImageDescriptor.Top := 0;
  FImageDescriptor.Width := 0;
  FImageDescriptor.Height := 0;
  FImageDescriptor.PackedFields := 0;
  FBitmap := nil;
  FMask := 0;
  FNeedMask := True;
  FData := nil;
  FDataSize := 0;
  FTransparent := False;
  FGCE := nil;
  // Remember to synchronize with TGIFSubImage.Clear
end;

destructor TGIFSubImage.Destroy;
begin
  Clear;
  FExtensions.Free;
  FColorMap.Free;
  if (FLocalPalette <> 0) then
    DeleteObject(FLocalPalette);
  inherited Destroy;
end;

procedure TGIFSubImage.Clear;
begin
  FExtensions.Clear;
  FColorMap.Clear;
  if (FData <> nil) then
    FreeMem(FData);
  FData := nil;
  FDataSize := 0;
  Height := 0;
  Width := 0;
  FTransparent := False;
  FGCE := nil;
  FreeBitmap;
  FreeMask;
  // Remember to synchronize with TGIFSubImage.Create
end;

function TGIFSubImage.GetEmpty: Boolean;
begin
  Result := ((FData = nil) or (FDataSize = 0) or (Height = 0) or (Width = 0));
   // and (ColorMap.Count = 0); Why this?
end;

function TGIFSubImage.GetPalette: HPALETTE;
begin
  if (FBitmap <> nil) and (FBitmap.Palette <> 0) then
    // Use bitmaps own palette if possible
    Result := FBitmap.Palette
  else if (FLocalPalette <> 0) then
    // Or a previously exported local palette
    Result := FLocalPalette
  else if (Image.DoDither) then
  begin
    // or create a new dither palette
    FLocalPalette := WebPalette;
    Result := FLocalPalette;
  end
  else if (ColorMap.Count > 0) then
  begin
    // or create a new if first time
    FLocalPalette := ColorMap.ExportPalette;
    Result := FLocalPalette;
  end else
    // Use global palette if everything else fails
    Result := Image.Palette;
end;

procedure TGIFSubImage.SetPalette(Value: HPalette);
var
  NeedNewBitmap		: boolean;
begin
  if (Value <> FLocalPalette) then
  begin
    // Zap old palette
    if (FLocalPalette <> 0) then
      DeleteObject(FLocalPalette);
    // Zap bitmap unless new palette is same as bitmaps own
    NeedNewBitmap := (FBitmap <> nil) and (Value <> FBitmap.Palette);
    if (NeedNewBitmap) then
      FreeBitmap;

    // Use new palette
    FLocalPalette := Value;
    if (NeedNewBitmap) then
    begin
      // Need to create new bitmap and repaint
      Image.PaletteModified := True;
      Image.Changed(Self);
    end;
  end;
end;

procedure TGIFSubImage.NewImage;
begin
  if (FData <> nil) then
    FreeMem(FData);
  FDataSize := Height * Width;
  if (FDataSize <> 0) then
    GetMem(FData, FDataSize)
  else
    FData := nil;
end;

procedure TGIFSubImage.FreeBitmap;
begin
  if (FBitmap <> nil) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TGIFSubImage.FreeMask;
begin
  if (FMask <> 0) then
  begin
    DeleteObject(FMask);
    FMask := 0;
  end;
  FNeedMask := True;
end;

function TGIFSubImage.HasMask: boolean;
begin
  if (FNeedMask) and (Transparent) then
  begin
    // Zap old bitmap
    FreeBitmap;
    // Create new bitmap and mask
    GetBitmap;
  end;
  Result := (FMask <> 0);
end;

function TGIFSubImage.GetBounds(Index: integer): WORD;
begin
  case (Index) of
    1: Result := FImageDescriptor.Left;
    2: Result := FImageDescriptor.Top;
    3: Result := FImageDescriptor.Width;
    4: Result := FImageDescriptor.Height;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFSubImage.SetBounds(Index: integer; Value: WORD);
begin
  case (Index) of
    1: FImageDescriptor.Left := Value;
    2: FImageDescriptor.Top := Value;
    3: FImageDescriptor.Width := Value;
    4: FImageDescriptor.Height := Value;
  end;
end;

procedure TGIFSubImage.NewBitmap;
begin
  FreeBitmap;
  FBitmap := TBitmap.Create;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TGIFSubImage.DoGetDitherBitmap: TBitmap;
type
  TErrors		= array[0..0] of LongInt;
  PErrors		= ^TErrors;

const
  // Integer arithmetic scaling factor
  // All math operations are scaled by this factor to avoid using floating point
  FS_SCALE		= 1024;

var
  ScanLineRow		: Integer;
  DIBDest		: TDIB;
  DestScanLine		,
  Src			: PChar;

  ThisErrorR		,
  ThisErrorG		,
  ThisErrorB		,
  NextErrorR		,
  NextErrorG		,
  NextErrorB		: PErrors;
  Error			: LongInt;
  SR, SG, SB		: LongInt;
  R, G, B		: integer;
  Direction		: integer;
  Row, Col		: integer;
  Color			: TGIFColor;
  ColMap		: PColorMap;
  Index			: byte;
  TransparentIndex	: byte;
  IsTransparent		: boolean;
  WasTransparent	: boolean;
  i			: integer;

  MaskBits		: PChar;
  MaskDest		: PChar;
  MaskRow		: PChar;
  MaskRowWidth		,
  MaskRowBitWidth	: integer;
  Bit			,
  RightBit		: BYTE;

  procedure SwapError(var P1, P2: PErrors);
  var
    P			: PErrors;
  begin
    P:= P1;
    P1:= P2;
    P2:= P;
  end;

begin
  Result := TBitmap.Create;

{$IFNDEF VER9x}
  if (Width*Height > BitmapAllocationThreshold) then
    SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize
{$ENDIF}

  // Set bitmap width and height
  Result.Width := Width;
  Result.Height := Height;

  // Build and copy palette to bitmap
  Result.Palette := CopyPalette(Palette);

  if (Empty) then
    exit;

  // Get DIB buffer for scanline operations
  DIBDest := TDIBWriter.Create(Result, pf8bit);
  try

    // Determine if this image is transparent
    IsTransparent := FNeedMask and Transparent;
    WasTransparent := False;
    FNeedMask := False;
    TransparentIndex := 0;
    if (FMask = 0) and (IsTransparent) then
    begin
      IsTransparent := True;
      TransparentIndex := GraphicControlExtension.TransparentColorIndex;
    end;

    // Allocate bit buffer for transparency mask
    if (IsTransparent) then
    begin
      MaskRowWidth := ((Width+15) DIV 16) * 2;
      MaskRowBitWidth := (Width+7) DIV 8;
      RightBit := $01 SHL ((8 - (Width AND $0007)) AND $0007);
      GetMem(MaskBits, MaskRowWidth * Height);
      FillChar(MaskBits^, MaskRowWidth * Height, 0);
      IsTransparent := (MaskBits <> nil);
    end else
    begin
      MaskBits := nil;
      MaskRowWidth := 0;
      MaskRowBitWidth := 0;
      RightBit := $00;
    end;

    try
      (* Initialize Floyd-Steinberg error vectors. *)
      GetMem(ThisErrorR, sizeof(LongInt)*(Width+2));
      GetMem(ThisErrorG, sizeof(LongInt)*(Width+2));
      GetMem(ThisErrorB, sizeof(LongInt)*(Width+2));
      GetMem(NextErrorR, sizeof(LongInt)*(Width+2));
      GetMem(NextErrorG, sizeof(LongInt)*(Width+2));
      GetMem(NextErrorB, sizeof(LongInt)*(Width+2));
      try
        FillChar(ThisErrorR^, sizeof(LongInt)*(Width+2), 0);
        FillChar(ThisErrorG^, sizeof(LongInt)*(Width+2), 0);
        FillChar(ThisErrorB^, sizeof(LongInt)*(Width+2), 0);

        Src := FData;
        Direction := 1;
        MaskRow := MaskBits;
        ScanLineRow := 0;

        ColMap := ActiveColorMap.Data;

        while (ScanLineRow < Height) do
        begin
          DestScanline := DIBDest.ScanLine[ScanLineRow];
          if ((ScanLineRow AND $1F) = 0) then
            Image.Progress(Self, psRunning, MulDiv(ScanLineRow, 100, Height),
              False, Rect(0,0,0,0), sProgressRendering);

          FillChar(NextErrorR^, sizeof(LongInt)*(Width+2), 0);
          FillChar(NextErrorG^, sizeof(LongInt)*(Width+2), 0);
          FillChar(NextErrorB^, sizeof(LongInt)*(Width+2), 0);

          if (Direction = 1) then
          begin
            Col := 0;
            MaskDest := MaskRow;
            Bit := $80;
          end else
          begin
            Col := Width-1;
            MaskDest := MaskRow + MaskRowBitWidth-1;
            Bit := RightBit;
          end;

          while (Col < Width) and (Col >= 0) do
          begin
            Index := ord(Src[Col]);
            Color := ColMap[ord(Index)];
            if (IsTransparent and (Index = TransparentIndex)) then
            begin
              MaskDest^ := char(byte(MaskDest^) OR Bit);
              WasTransparent := True;
              DestScanline[Col] := char(Round(Color.Blue / 51.0)+6*Round(Color.Green / 51.0)+36*Round(Color.Red / 51.0));
            end else
            begin
              (* Use Floyd-Steinberg errors to adjust actual color. *)
              SR := Color.Red + ThisErrorR[col + 1] DIV FS_SCALE;
              R := round(SR / 51.0);
              if (R < 0) then
                R := 0
              else if (R > 5) then
                R := 5;

              SG := Color.Green + ThisErrorG[col + 1] DIV FS_SCALE;
              G := round(SG / 51.0);
              if (G < 0) then
                G := 0
              else if (G > 5) then
                G := 5;

              SB := Color.Blue + ThisErrorB[col + 1] DIV FS_SCALE;
              B := round(SB / 51.0);
              if (B < 0) then
                B := 0
              else if (B > 5) then
                B := 5;

              (* Map dithered pixel to netscape color cube *)
              DestScanline[Col] := char(B + 6*G + 36*R);

              (* Propagate Floyd-Steinberg error terms. *)
              if (Direction = 1) then
              begin
                Error := (SR - R*51) * FS_SCALE;
                ThisErrorR[col+2] := ThisErrorR[col+2] + (Error * 7) DIV 16;
                NextErrorR[col  ] := NextErrorR[col  ] + (Error * 3) DIV 16;
                NextErrorR[col+1] := NextErrorR[col+1] + (Error * 5)  DIV 16;
                NextErrorR[col+2] := NextErrorR[col+2] + Error DIV 16;
                Error := (SG - G*51) * FS_SCALE;
                ThisErrorG[col+2] := ThisErrorG[col+2] + (Error * 7) DIV 16;
                NextErrorG[col  ] := NextErrorG[col  ] + (Error * 3) DIV 16;
                NextErrorG[col+1] := NextErrorG[col+1] + (Error * 5)  DIV 16;
                NextErrorG[col+2] := NextErrorG[col+2] + Error DIV 16;
                Error := (SB - B*51) * FS_SCALE;
                ThisErrorB[col+2] := ThisErrorB[col+2] + (Error * 7) DIV 16;
                NextErrorB[col  ] := NextErrorB[col  ] + (Error * 3) DIV 16;
                NextErrorB[col+1] := NextErrorB[col+1] + (Error * 5)  DIV 16;
                NextErrorB[col+2] := NextErrorB[col+2] + Error DIV 16;
              end else
              begin
                Error := (SR - R*51) * FS_SCALE;
                ThisErrorR[col  ] := ThisErrorR[col  ] + (Error * 7) DIV 16;
                NextErrorR[col+2] := NextErrorR[col+2] + (Error * 3) DIV 16;
                NextErrorR[col+1] := NextErrorR[col+1] + (Error * 5)  DIV 16;
                NextErrorR[col  ] := NextErrorR[col  ] + Error DIV 16;
                Error := (SG - G*51) * FS_SCALE;
                ThisErrorG[col  ] := ThisErrorG[col  ] + (Error * 7) DIV 16;
                NextErrorG[col+2] := NextErrorG[col+2] + (Error * 3) DIV 16;
                NextErrorG[col+1] := NextErrorG[col+1] + (Error * 5)  DIV 16;
                NextErrorG[col  ] := NextErrorG[col  ] + Error DIV 16;
                Error := (SB - B*51) * FS_SCALE;
                ThisErrorB[col  ] := ThisErrorB[col  ] + (Error * 7) DIV 16;
                NextErrorB[col+2] := NextErrorB[col+2] + (Error * 3) DIV 16;
                NextErrorB[col+1] := NextErrorB[col+1] + (Error * 5)  DIV 16;
                NextErrorB[col  ] := NextErrorB[col  ] + Error DIV 16;
              end;
            end;
            if (IsTransparent) then
            begin
              if (Direction = 1) then
              begin
                Bit := Bit SHR 1;
                if (Bit = $00) then
                begin
                  Bit := $80;
                  inc(MaskDest, 1);
                end;
              end else
              begin
                Bit := Bit SHL 1;
                if (Bit = $00) then
                begin
                  Bit := $01;
                  inc(MaskDest, -1);
                end;
              end;
            end;
            Inc(Col, Direction);
          end;

          SwapError(ThisErrorR, NextErrorR);
          SwapError(ThisErrorG, NextErrorG);
          SwapError(ThisErrorB, NextErrorB);

          Direction := -Direction;

          Inc(Src, Width);
          Inc(ScanLineRow);
          if (IsTransparent) then
            Inc(MaskRow, MaskRowWidth);
        end;
      finally
        FreeMem(ThisErrorR);
        FreeMem(ThisErrorG);
        FreeMem(ThisErrorB);
        FreeMem(NextErrorR);
        FreeMem(NextErrorG);
        FreeMem(NextErrorB);
      end;
      // Transparent paint needs a mask bitmap
      if (IsTransparent) and (WasTransparent) then
        FMask := CreateBitmap(Width, Height, 1, 1, MaskBits);
    finally
      if (MaskBits <> nil) then
        FreeMem(MaskBits);
    end;
  finally
    // Free DIB buffer used for scanline operations
    DIBDest.Free;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

function TGIFSubImage.DoGetBitmap: TBitmap;
var
  ScanLineRow		: Integer;
  DIBDest		: TDIB;
  DestScanLine		,
  Src			: PChar;
  TransparentIndex	: byte;
  IsTransparent		: boolean;
  WasTransparent	: boolean;

  MaskBits		: PChar;
  MaskDest		: PChar;
  MaskRow		: PChar;
  MaskRowWidth		: integer;
  Col			: integer;
  MaskByte		: byte;
  Bit			: byte;
begin
  Result := TBitmap.Create;

{$IFNDEF VER9x}
  if (Width*Height > BitmapAllocationThreshold) then
    SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize
{$ENDIF}

  // Set bitmap width and height
  Result.Width := Width;
  Result.Height := Height;

  // Set the bitmap pixel format
  SafeSetPixelFormat(Result, pf8bit); // 8 bits per pixel/256 colors

  // Build and copy palette to bitmap
  Result.Palette := CopyPalette(Palette);

  if (Empty) then
    exit;

  // Get DIB buffer for scanline operations
  DIBDest := TDIBWriter.Create(Result, pf8bit);
  try

    // Determine if this image is transparent
    IsTransparent := FNeedMask and Transparent;
    WasTransparent := False;
    FNeedMask := False;
    TransparentIndex := 0;
    if (FMask = 0) and (IsTransparent) then
    begin
      IsTransparent := True;
      TransparentIndex := GraphicControlExtension.TransparentColorIndex;
    end;
    // Allocate bit buffer for transparency mask
    if (IsTransparent) then
    begin
      MaskRowWidth := ((Width+15) DIV 16) * 2;
      GetMem(MaskBits, MaskRowWidth * Height);
      FillChar(MaskBits^, MaskRowWidth * Height, 0);
      IsTransparent := (MaskBits <> nil);
    end else
    begin
      MaskBits := nil;
      MaskRowWidth := 0;
    end;

    try
      ScanLineRow := 0;
      Src := FData;
      MaskRow := MaskBits;
      while (ScanLineRow < Height) do
      begin
        DestScanline := DIBDest.ScanLine[ScanLineRow];

        if ((ScanLineRow AND $1F) = 0) then
          Image.Progress(Self, psRunning, MulDiv(ScanLineRow, 100, Height),
            False, Rect(0,0,0,0), sProgressRendering);

        Move(Src^, DestScanline^, Width);
        Inc(ScanLineRow);

        if (IsTransparent) then
        begin
          Bit := $80;
          MaskDest := MaskRow;
          MaskByte := 0;
          for Col := 0 to Width-1 do
          begin
            // Set a bit in the mask if the pixel is transparent
            if (Src^ = char(TransparentIndex)) then
              MaskByte := MaskByte OR Bit;

            Bit := Bit SHR 1;
            if (Bit = $00) then
            begin
              // Store a mask byte for each 8 pixels
              Bit := $80;
              WasTransparent := WasTransparent or (MaskByte <> 0);
              MaskDest^ := char(MaskByte);
              inc(MaskDest);
              MaskByte := 0;
            end;
            Inc(Src);
          end;
          // Save the last mask byte in case the width isn't divisable by 8
          if (MaskByte <> 0) then
            MaskDest^ := char(MaskByte);
          Inc(MaskRow, MaskRowWidth);
        end else
          Inc(Src, Width);
      end;

      // Transparent paint needs a mask bitmap
      if (IsTransparent) and (WasTransparent) then
        FMask := CreateBitmap(Width, Height, 1, 1, MaskBits);
    finally
      if (MaskBits <> nil) then
        FreeMem(MaskBits);
    end;
  finally
    // Free DIB buffer used for scanline operations
    DIBDest.Free;
  end;
end;

function TGIFSubImage.HasBitmap: boolean;
begin
  Result := (FBitmap <> nil);
end;

function TGIFSubImage.GetBitmap: TBitmap;
var
  n			: integer;
begin
  Result := FBitmap;
  if (Result <> nil) or (Empty) then
    Exit;

  try
    Image.Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressRendering);
    try

      if (Image.DoDither) then
        // Create dithered bitmap
        FBitmap := DoGetDitherBitmap
      else
        // Create "regular" bitmap
        FBitmap := DoGetBitmap;

      Result := FBitmap;

    finally
{$IFDEF VER93}
      // Special case for C++ Builder 1.x without "ExceptObject"
      n := 100;
{$ELSE}
      if ExceptObject = nil then
        n := 100
      else
        n := 0;
{$ENDIF}
      Image.Progress(Self, psEnding, n, Image.PaletteModified, Rect(0,0,0,0),
        sProgressRendering);
      // Make sure new palette gets realized, in case OnProgress event didn't.
//      if Image.PaletteModified then
//        Image.Changed(Self);
    end;
  except
    on EAbort do ;   // OnProgress can raise EAbort to cancel image load
  end;

end;

procedure TGIFSubImage.SetBitmap(Value: TBitmap);
begin
  FreeBitmap;
  if (Value <> nil) then
    Assign(Value);
end;

function TGIFSubImage.GetActiveColorMap: TGIFColorMap;
begin
  if (ColorMap.Count > 0) or (Image.GlobalColorMap.Count = 0) then
    Result := ColorMap
  else
    Result := Image.GlobalColorMap;
end;

function TGIFSubImage.GetInterlaced: boolean;
begin
  Result := (FImageDescriptor.PackedFields AND idInterlaced) <> 0;
end;

procedure TGIFSubImage.SetInterlaced(Value: boolean);
begin
  if (Value) then
    FImageDescriptor.PackedFields := FImageDescriptor.PackedFields OR idInterlaced
  else
    FImageDescriptor.PackedFields := FImageDescriptor.PackedFields AND NOT(idInterlaced);
end;

function TGIFSubImage.GetVersion: TGIFVersion;
var
  v			: TGIFVersion;
  i			: integer;
begin
  if (ColorMap.Optimized) then
    Result := gv89a
  else
    Result := inherited GetVersion;
  i := 0;
  while (Result < high(TGIFVersion)) and (i < FExtensions.Count) do
  begin
    v := FExtensions[i].Version;
    if (v > Result) then
      Result := v;
  end;
end;

function TGIFSubImage.GetColorResolution: integer;
begin
  Result := ColorMap.BitsPerPixel-1;
end;

function TGIFSubImage.GetBitsPerPixel: integer;
begin
  Result := ColorMap.BitsPerPixel;
end;

function TGIFSubImage.GetBoundsRect: TRect;
begin
  Result := Rect(FImageDescriptor.Left,
    FImageDescriptor.Top,
    FImageDescriptor.Left+FImageDescriptor.Width,
    FImageDescriptor.Top+FImageDescriptor.Height);
end;

function TGIFSubImage.GetClientRect: TRect;
begin
  Result := Rect(0, 0, FImageDescriptor.Width, FImageDescriptor.Height);
end;

function TGIFSubImage.GetPixel(x, y: integer): BYTE;
begin
  if (x < 0) or (x > Width-1) or (y < 0) or (y > Height-1) then
    Error(sBadPixelCoordinates);
  Result := BYTE(PChar(longInt(FData) + y * Width + x)^);
end;

procedure TGIFSubImage.Prepare;
var
  Pack			: BYTE;
begin
  Pack := FImageDescriptor.PackedFields;
  if (ColorMap.Count > 0) then
  begin
    Pack := idLocalColorTable;
    if (ColorMap.Optimized) then
      Pack := Pack OR idSort;
    Pack := (Pack AND NOT(idColorTableSize)) OR (ColorResolution AND idColorTableSize);
  end else
    Pack := Pack AND NOT(idLocalColorTable OR idSort OR idColorTableSize);
  FImageDescriptor.PackedFields := Pack;
end;

procedure TGIFSubImage.SaveToStream(Stream: TStream);
begin
  FExtensions.SaveToStream(Stream);
  if (Empty) then
    exit;
  Prepare;
  Stream.Write(FImageDescriptor, sizeof(TImageDescriptor));
  ColorMap.SaveToStream(Stream);
  Compress(Stream);
end;

procedure TGIFSubImage.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGIFSubImage.LoadFromStream(Stream: TStream);
var
  ColorCount		: integer;
  b			: BYTE;
begin
  Clear;
  FExtensions.LoadFromStream(Stream, self);
  // Check for extension without image
  if (Stream.Read(b, 1) <> 1) then
    exit;
  Stream.Seek(-1, soFromCurrent);
  if (b = bsTrailer) or (b = 0) then
    exit;

  ReadCheck(Stream, FImageDescriptor, sizeof(TImageDescriptor));

  // From Mozilla source:
  // Work around more broken GIF files that have zero image
  // width or height
  if (FImageDescriptor.Height = 0) or (FImageDescriptor.Width = 0) then
  begin
    FImageDescriptor.Height := Image.Height;
    FImageDescriptor.Width := Image.Width;
    Warning(gsWarning, sScreenSizeExceeded);
  end;

  if (FImageDescriptor.PackedFields AND idLocalColorTable = idLocalColorTable) then
  begin
    ColorCount := 2 SHL (FImageDescriptor.PackedFields AND idColorTableSize);
    if (ColorCount < 2) or (ColorCount > 256) then
      Error(sImageBadColorSize);
    ColorMap.LoadFromStream(Stream, ColorCount);
  end;

  Decompress(Stream);
end;

procedure TGIFSubImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(Bitmap)
  else
    inherited AssignTo(Dest);
end;

procedure TGIFSubImage.Assign(Source: TPersistent);
var
  MemoryStream		: TMemoryStream;
  i			: integer;
  PixelFormat		: TPixelFormat;
  DIBSource		: TDIB;

  procedure Import8Bit(Dest: PChar);
  var
    y			: integer;
  begin
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      Move(DIBSource.Scanline[y]^, Dest^, Width);
      inc(Dest, Width);
    end;
  end;

  procedure Import4Bit(Dest: PChar);
  var
    x, y		: integer;
    Scanline		: PChar;
  begin
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      for x := 0 to Width-1 do
      begin
        if (x AND $01 = 0) then
          Dest^ := chr(ord(ScanLine^) SHR 4)
        else
        begin
          Dest^ := chr(ord(ScanLine^) AND $0F);
          inc(ScanLine);
        end;
        inc(Dest);
      end;
    end;
  end;

  procedure Import1Bit(Dest: PChar);
  var
    x, y		: integer;
    Scanline		: PChar;
    Bit			: integer;
    Byte		: integer;
  begin
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      x := Width;
      Bit := 0;
      Byte := 0; // To avoid compiler warning
      while (x > 0) do
      begin
        if (Bit = 0) then
        begin
          Bit := 8;
          Byte := ord(ScanLine^);
          inc(Scanline);
        end;
        Dest^ := chr((Byte AND $80) SHR 7);
        Byte := Byte SHL 1;
        inc(Dest);
        dec(Bit);
        dec(x);
      end;
    end;
  end;

  procedure ImportAnyBit(Dest: PChar);
  type
    TCacheEntry = record
      Color		: TColor;
      Index		: integer;
    end;
  const
    // Size of palette cache. Must be 2^n.
    // The cache holds the palette index of the last "CacheSize" colors
    // processed. Hopefully the cache can speed things up a bit... Initial
    // testing shows that this is indeed the case at least for non-dithered
    // bitmaps.
    // All the same, a small hash table would probably be much better.
    CacheSize		= 8;
  var
    i			: integer;
    Cache		: array[0..CacheSize-1] of TCacheEntry;
    LastEntry		: integer;
    Pixel		: TColor;
    x, y		: integer;
  label
    NextPixel;
  begin
    for i := 0 to CacheSize-1 do
      Cache[i].Index := -1;
    LastEntry := 0;

    // Copy all pixels and build colormap
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      for x := 0 to Width-1 do
      begin
        // ***FIXME*** Should use TDIBReader in pf24bit or pf32bit mode instead
        // of TCanvas.Pixels
        Pixel := FBitmap.Canvas.Pixels[x,y];
        // Scan cache for color from most recently processed color to last
        // recently processed. This is done because TColorMap.Add is very slow.
        i := LastEntry;
        repeat
          if (Cache[i].Index = -1) then
            break;
          if (Cache[i].Color = Pixel) then
          begin
            Dest^ := chr(Cache[i].Index);
            LastEntry := i;
            goto NextPixel;
          end;
          if (i = 0) then
            i := CacheSize-1
          else
            dec(i);
        until (i = LastEntry);
        // Color not found in cache, do it the slow way instead
        Dest^ := chr(FColorMap.Add(Pixel));
        // Add color and index to cache
        LastEntry := (LastEntry + 1) AND (CacheSize-1);
        Cache[LastEntry].Color := Pixel;
        Cache[LastEntry].Index := ord(Dest^);

        NextPixel:
        Inc(Dest);
      end;
    end;
  end;

begin
  if (Source = self) then
    exit;
  if (Source is TGIFSubImage) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TGIFSubImage(Source).Empty) then
      exit;
    // Copy source data
    FImageDescriptor := TGIFSubImage(Source).FImageDescriptor;
    FTransparent := TGIFSubImage(Source).Transparent;
    // Copy image data
    NewImage;
    if (FData <> nil) and (TGIFSubImage(Source).Data <> nil) then
      Move(TGIFSubImage(Source).Data^, FData^, FDataSize);
    // Copy palette
    FColorMap.Assign(TGIFSubImage(Source).ColorMap);
    // Copy extensions
    if (TGIFSubImage(Source).Extensions.Count > 0) then
    begin
      MemoryStream := TMemoryStream.Create;
      try
        TGIFSubImage(Source).Extensions.SaveToStream(MemoryStream);
        MemoryStream.Seek(0, soFromBeginning);
        Extensions.LoadFromStream(MemoryStream, Self);
      finally
        MemoryStream.Free;
      end;
    end;

    // Copy bitmap representation
    // (Not really nescessary but improves performance if the bitmap is needed
    // later on)
    if (TGIFSubImage(Source).FBitmap <> nil) then
    begin
      NewBitmap;
      FBitmap.Assign(TGIFSubImage(Source).FBitmap);
    end;
  end else
  if (Source is TBitmap) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TBitmap(Source).Empty) then
      exit;

    PixelFormat := GetPixelFormat(TBitmap(Source));
    if (PixelFormat > pf8bit) then
    begin
      // Convert image to 8 bits/pixel or less
      FBitmap := ReduceColors(TBitmap(Source), Image.ColorReduction,
        Image.DitherMode, Image.ReductionBits);
      PixelFormat := GetPixelFormat(FBitmap);
    end else
    begin
      // Create new bitmap and copy
      NewBitmap;
      FBitmap.Assign(TBitmap(Source));
    end;

    Width := FBitmap.Width;
    Height := FBitmap.Height;
    // Allocate new buffer
    NewImage;

    Image.Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressConverting);
    try
      // ***FIXME*** It is impossible (for me at least) to import Delphi 2.x
      // DDBs in 256 color mode with acceptable results, so instead I use the
      // safe and sure (but very slow) method.
{$ifdef VER9x}
      if (PaletteDevice) and (PixelFormat in [pf1bit, pf4bit, pf8bit]) then
{$else}
      if (PixelFormat in [pf1bit, pf4bit, pf8bit]) then
{$endif}
      begin

        DIBSource := TDIBReader.Create(FBitmap, PixelFormat);
        try
          // Copy colormap
          FColorMap.ImportPalette(FBitmap.Palette);
          // Copy pixels
          case (PixelFormat) of
            pf8bit: Import8Bit(Fdata);
            pf4bit: Import4Bit(Fdata);
            pf1bit: Import1Bit(Fdata);
          end;

        finally
          DIBSource.Free;
        end;
      end else
      begin
        // Copy all pixels and build colormap
        ImportAnyBit(FData);
      end;
    finally
{$IFDEF VER93}
      // Special case for C++ Builder 1.x without "ExceptObject"
      i := 100;
{$ELSE}
      if ExceptObject = nil then
        i := 100
      else
        i := 0;
{$ENDIF}
      Image.Progress(Self, psEnding, i, Image.PaletteModified, Rect(0,0,0,0), sProgressConverting);
    end;

  end else
    inherited Assign(Source);
end;

// Copied from D3 graphics.pas
// Fixed by Brian Lowe of Acro Technology Inc. 30Jan98
function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy		= $00AA0029;
var
  MemDC			,
  OrMaskDC		: HDC;
  MemBmp		,
  OrMaskBmp		: HBITMAP;
  Save			,
  OrMaskSave		: THandle;
  crText, crBack	: TColorRef;
  SavePal		: HPALETTE;

begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;

  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(DstDC));
  try
    { Color bitmap for combining OR mask with source bitmap }
    MemBmp := GDICheck(CreateCompatibleBitmap(DstDC, SrcW, SrcH));
    try
      Save := SelectObject(MemDC, MemBmp);
      try
        { This bitmap needs the size of the source but DC of the dest }
        OrMaskDC := GDICheck(CreateCompatibleDC(DstDC));
        try
          { Need a monochrome bitmap for OR mask!! }
          OrMaskBmp := GDICheck(CreateBitmap(SrcW, SrcH, 1, 1, nil));
          try
            OrMaskSave := SelectObject(OrMaskDC, OrMaskBmp);
            try

              // OrMask := 1
              // Original: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, WHITENESS);
              // Replacement, but not needed: PatBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, WHITENESS);
              // OrMask := OrMask XOR Mask
              // Not needed: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, SrcInvert);
              // OrMask := NOT Mask
              BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, NotSrcCopy);

              // Retrieve source palette (with dummy select)
              SavePal := SelectPalette(SrcDC, SystemPalette16, False);
              // Restore source palette
              SelectPalette(SrcDC, SavePal, False);
              // Select source palette into memory buffer
              if SavePal <> 0 then
                SavePal := SelectPalette(MemDC, SavePal, True)
              else
                SavePal := SelectPalette(MemDC, SystemPalette16, True);
              RealizePalette(MemDC);

              // Mem := OrMask
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, SrcCopy);
              // Mem := Mem AND Src
{$IFNDEF GIF_TESTMASK} // Define GIF_TESTMASK if you want to know what it does...
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcAnd);
{$ELSE}
              StretchBlt(DstDC, DstX, DstY, DstW DIV 2, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              StretchBlt(DstDC, DstX+DstW DIV 2, DstY, DstW DIV 2, DstH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              exit;
{$ENDIF}
            finally
              if (OrMaskSave <> 0) then
                SelectObject(OrMaskDC, OrMaskSave);
            end;
          finally
            DeleteObject(OrMaskBmp);
          end;
        finally
          DeleteDC(OrMaskDC);
        end;

        crText := SetTextColor(DstDC, $00000000);
        crBack := SetBkColor(DstDC, $00FFFFFF);

        { All color rendering is done at 1X (no stretching),
          then final 2 masks are stretched to dest DC }
        // Neat trick!
        // Dst := Dst AND Mask
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, SrcX, SrcY, SrcW, SrcH, SrcAnd);
        // Dst := Dst OR Mem
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcPaint);

        SetTextColor(DstDC, crText);
        SetTextColor(DstDC, crBack);

      finally
        if (Save <> 0) then
          SelectObject(MemDC, Save);
      end;
    finally
      DeleteObject(MemBmp);
    end;
  finally
    if (SavePal <> 0) then
      SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;

procedure TGIFSubImage.Draw(ACanvas: TCanvas; const Rect: TRect;
  DoTransparent, DoTile: boolean);
begin
  if (DoTile) then
    StretchDraw(ACanvas, Rect, DoTransparent, DoTile)
  else
    StretchDraw(ACanvas, ScaleRect(Rect), DoTransparent, DoTile);
end;

type
  // Dummy class used to gain access to protected method TCanvas.Changed
  TChangableCanvas = class(TCanvas)
  end;

procedure TGIFSubImage.StretchDraw(ACanvas: TCanvas; const Rect: TRect;
  DoTransparent, DoTile: boolean);
var
  MaskDC		: HDC;
  Save			: THandle;
  Tile			: TRect;
{$ifdef DEBUG_DRAWPERFORMANCE}
  ImageCount		,
  TimeStart		,
  TimeStop		: DWORD;
{$endif}

begin
{$ifdef DEBUG_DRAWPERFORMANCE}
  TimeStart := timeGetTime;
  ImageCount := 0;
{$endif}
  if (DoTransparent) and (Transparent) and (HasMask) then
  begin
    // Draw transparent using mask
    Save := 0;
    MaskDC := 0;
    try
      MaskDC := GDICheck(CreateCompatibleDC(0));
      Save := SelectObject(MaskDC, FMask);

      if (DoTile) then
      begin
        Tile.Left := Rect.Left+Left;
        Tile.Right := Tile.Left + Width;
        while (Tile.Left < Rect.Right) do
        begin
          Tile.Top := Rect.Top+Top;
          Tile.Bottom := Tile.Top + Height;
          while (Tile.Top < Rect.Bottom) do
          begin
            TransparentStretchBlt(ACanvas.Handle, Tile.Left, Tile.Top, Width, Height,
              Bitmap.Canvas.Handle, 0, 0, Width, Height, MaskDC, 0, 0);
            Tile.Top := Tile.Top + Image.Height;
            Tile.Bottom := Tile.Bottom + Image.Height;
{$ifdef DEBUG_DRAWPERFORMANCE}
            inc(ImageCount);
{$endif}
          end;
          Tile.Left := Tile.Left + Image.Width;
          Tile.Right := Tile.Right + Image.Width;
        end;
      end else
        TransparentStretchBlt(ACanvas.Handle, Rect.Left, Rect.Top,
          Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
          Bitmap.Canvas.Handle, 0, 0, Width, Height, MaskDC, 0, 0);

      // Since we are not using any of the TCanvas functions (only handle)
      // we need to fire the TCanvas.Changed method "manually".
      TChangableCanvas(ACanvas).Changed;

    finally
      if (Save <> 0) then
        SelectObject(MaskDC, Save);
      if (MaskDC <> 0) then
        DeleteDC(MaskDC);
    end;
  end else
  begin
    if (DoTile) then
    begin
      Tile.Left := Rect.Left+Left;
      Tile.Right := Tile.Left + Width;
      while (Tile.Left < Rect.Right) do
      begin
        Tile.Top := Rect.Top+Top;
        Tile.Bottom := Tile.Top + Height;
        while (Tile.Top < Rect.Bottom) do
        begin
          ACanvas.StretchDraw(Tile, Bitmap);
          Tile.Top := Tile.Top + Image.Height;
          Tile.Bottom := Tile.Bottom + Image.Height;
{$ifdef DEBUG_DRAWPERFORMANCE}
          inc(ImageCount);
{$endif}
        end;
        Tile.Left := Tile.Left + Image.Width;
        Tile.Right := Tile.Right + Image.Width;
      end;
    end else
      ACanvas.StretchDraw(Rect, Bitmap);
  end;
{$ifdef DEBUG_DRAWPERFORMANCE}
  if (GetAsyncKeyState(VK_CONTROL) <> 0) then
  begin
    TimeStop := timeGetTime;
    ShowMessage(format('Draw %d images in %d mS, Rate %d images/mS (%d images/S)',
      [ImageCount, TimeStop-TimeStart,
      ImageCount DIV (TimeStop-TimeStart+1),
      MulDiv(ImageCount, 1000, TimeStop-TimeStart+1)]));
  end;
{$endif}
end;

// Given a destination rect (DestRect) calculates the
// area covered by this sub image
function TGIFSubImage.ScaleRect(DestRect: TRect): TRect;
var
  HeightMul		,
  HeightDiv		: integer;
  WidthMul		,
  WidthDiv		: integer;
begin
  HeightDiv := Image.Height;
  HeightMul := DestRect.Bottom-DestRect.Top;
  WidthDiv := Image.Width;
  WidthMul := DestRect.Right-DestRect.Left;

  Result.Left := DestRect.Left + muldiv(Left, WidthMul, WidthDiv);
  Result.Top := DestRect.Top + muldiv(Top, HeightMul, HeightDiv);
  Result.Right := DestRect.Left + muldiv(Left+Width, WidthMul, WidthDiv);
  Result.Bottom := DestRect.Top + muldiv(Top+Height, HeightMul, HeightDiv);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFTrailer
//
////////////////////////////////////////////////////////////////////////////////
procedure TGIFTrailer.SaveToStream(Stream: TStream);
begin
  WriteByte(Stream, bsTrailer);
end;

procedure TGIFTrailer.LoadFromStream(Stream: TStream);
var
  b			: BYTE;
begin
  if (Stream.Read(b, 1) <> 1) then
    exit;
  if (b <> bsTrailer) then
    Warning(gsWarning, sBadTrailer);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TGIFExtension registration database
//
////////////////////////////////////////////////////////////////////////////////
type
  TExtensionLeadIn = packed record
    Introducer: byte;      { always $21 }
    ExtensionLabel: byte;
  end;

  PExtRec = ^TExtRec;
  TExtRec = record
    ExtClass: TGIFExtensionClass;
    ExtLabel: BYTE;
  end;

  TExtensionList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(eLabel: BYTE; eClass: TGIFExtensionClass);
    function FindExt(eLabel: BYTE): TGIFExtensionClass;
    procedure Remove(eClass: TGIFExtensionClass);
  end;

constructor TExtensionList.Create;
begin
  inherited Create;
  Add(bsPlainTextExtension, TGIFTextExtension);
  Add(bsGraphicControlExtension, TGIFGraphicControlExtension);
  Add(bsCommentExtension, TGIFCommentExtension);
  Add(bsApplicationExtension, TGIFApplicationExtension);
end;

destructor TExtensionList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PExtRec(Items[I]));
  inherited Destroy;
end;

procedure TExtensionList.Add(eLabel: BYTE; eClass: TGIFExtensionClass);
var
  NewRec: PExtRec;
begin
  New(NewRec);
  with NewRec^ do
  begin
    ExtLabel := eLabel;
    ExtClass := eClass;
  end;
  inherited Add(NewRec);
end;

function TExtensionList.FindExt(eLabel: BYTE): TGIFExtensionClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    with PExtRec(Items[I])^ do
      if ExtLabel = eLabel then
      begin
        Result := ExtClass;
        Exit;
      end;
  Result := nil;
end;

procedure TExtensionList.Remove(eClass: TGIFExtensionClass);
var
  I: Integer;
  P: PExtRec;
begin
  for I := Count-1 downto 0 do
  begin
    P := PExtRec(Items[I]);
    if P^.ExtClass.InheritsFrom(eClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

var
  ExtensionList: TExtensionList = nil;

function GetExtensionList: TExtensionList;
begin
  if (ExtensionList = nil) then
    ExtensionList := TExtensionList.Create;
  Result := ExtensionList;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFExtension
//
////////////////////////////////////////////////////////////////////////////////
function TGIFExtension.GetVersion: TGIFVersion;
begin
  Result := gv89a;
end;

class procedure TGIFExtension.RegisterExtension(eLabel: BYTE; eClass: TGIFExtensionClass);
begin
  GetExtensionList.Add(eLabel, eClass);
end;

class function TGIFExtension.FindExtension(Stream: TStream): TGIFExtensionClass;
var
  eLabel		: BYTE;
  SubClass		: TGIFExtensionClass;
  Pos			: LongInt;
begin
  Pos := Stream.Position;
  if (Stream.Read(eLabel, 1) <> 1) then
  begin
    Result := nil;
    exit;
  end;
  Result := GetExtensionList.FindExt(eLabel);
  while (Result <> nil) do
  begin
    SubClass := Result.FindSubExtension(Stream);
    if (SubClass = Result) then
      break;
    Result := SubClass;
  end;
  Stream.Position := Pos;
end;

class function TGIFExtension.FindSubExtension(Stream: TStream): TGIFExtensionClass;
begin
  Result := self;
end;

constructor TGIFExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage.Image);
  FSubImage := ASubImage;
end;

procedure TGIFExtension.SaveToStream(Stream: TStream);
var
  ExtensionLeadIn	: TExtensionLeadIn;
begin
  ExtensionLeadIn.Introducer := bsExtensionIntroducer;
  ExtensionLeadIn.ExtensionLabel := ExtensionType;
  Stream.Write(ExtensionLeadIn, sizeof(ExtensionLeadIn));
end;

function TGIFExtension.DoReadFromStream(Stream: TStream): TGIFExtensionType;
var
  ExtensionLeadIn	: TExtensionLeadIn;
begin
  ReadCheck(Stream, ExtensionLeadIn, sizeof(ExtensionLeadIn));
  if (ExtensionLeadIn.Introducer <> bsExtensionIntroducer) then
    Error(sBadExtensionLabel);
  Result := ExtensionLeadIn.ExtensionLabel;
end;

procedure TGIFExtension.LoadFromStream(Stream: TStream);
begin
  // Seek past lead-in
  // Stream.Seek(sizeof(TExtensionLeadIn), soFromCurrent);
  if (DoReadFromStream(Stream) <> ExtensionType) then
    Error(sBadExtensionInstance);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFGraphicControlExtension
//
////////////////////////////////////////////////////////////////////////////////
const
  { Extension flag bit masks }
  efInputFlag		= $02;		{ 00000010 }
  efDisposal		= $1C;		{ 00011100 }
  efTransparent		= $01;		{ 00000001 }
  efReserved		= $E0;		{ 11100000 }

constructor TGIFGraphicControlExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);

  FGCExtension.BlockSize := 4;
  FGCExtension.PackedFields := $00;
  FGCExtension.DelayTime := 0;
  FGCExtension.TransparentColorIndex := 0;
  FGCExtension.Terminator := 0;
  if (ASubImage.FGCE = nil) then
    ASubImage.FGCE := self;
end;

destructor TGIFGraphicControlExtension.Destroy;
begin
  // Clear transparent flag in sub image
  if (Transparent) then
    SubImage.FTransparent := False;

  if (SubImage.FGCE = self) then
    SubImage.FGCE := nil;

  inherited Destroy;
end;

function TGIFGraphicControlExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsGraphicControlExtension;
end;

function TGIFGraphicControlExtension.GetTransparent: boolean;
begin
  Result := (FGCExtension.PackedFields AND efTransparent) <> 0;
end;

procedure TGIFGraphicControlExtension.SetTransparent(Value: boolean);
begin
  // Set transparent flag in sub image
  SubImage.FTransparent := Value;
  if (Value) then
    FGCExtension.PackedFields := FGCExtension.PackedFields OR efTransparent
  else
    FGCExtension.PackedFields := FGCExtension.PackedFields AND NOT(efTransparent);
end;

function TGIFGraphicControlExtension.GetTransparentColor: TColor;
begin
  Result := SubImage.ActiveColorMap[TransparentColorIndex];
end;

procedure TGIFGraphicControlExtension.SetTransparentColor(Color: TColor);
var
  Index			: integer;
begin
  with SubImage do
  begin
    Index := ActiveColorMap.IndexOf(Color);
    if (Index = -1) then
      Index := ActiveColorMap.Add(Color);
  end;
  FGCExtension.TransparentColorIndex := Index;
end;

function TGIFGraphicControlExtension.GetTransparentColorIndex: BYTE;
begin
  Result := FGCExtension.TransparentColorIndex;
end;

procedure TGIFGraphicControlExtension.SetTransparentColorIndex(Value: BYTE);
begin
  if ((Value >= SubImage.ActiveColorMap.Count) and (SubImage.ActiveColorMap.Count > 0)) then
  begin
    Warning(gsWarning, sBadColorIndex);
    Value := 0;
  end;
  FGCExtension.TransparentColorIndex := Value;
end;

function TGIFGraphicControlExtension.GetDelay: WORD;
begin
  Result := FGCExtension.DelayTime;
end;
procedure TGIFGraphicControlExtension.SetDelay(Value: WORD);
begin
  FGCExtension.DelayTime := Value;
end;

function TGIFGraphicControlExtension.GetUserInput: boolean;
begin
  Result := (FGCExtension.PackedFields AND efInputFlag) <> 0;
end;

procedure TGIFGraphicControlExtension.SetUserInput(Value: boolean);
begin
  if (Value) then
    FGCExtension.PackedFields := FGCExtension.PackedFields OR efInputFlag
  else
    FGCExtension.PackedFields := FGCExtension.PackedFields AND NOT(efInputFlag);
end;

function TGIFGraphicControlExtension.GetDisposal: TDisposalMethod;
begin
  Result := TDisposalMethod((FGCExtension.PackedFields AND efDisposal) SHR 2);
end;

procedure TGIFGraphicControlExtension.SetDisposal(Value: TDisposalMethod);
begin
  FGCExtension.PackedFields := FGCExtension.PackedFields AND NOT(efDisposal)
    OR ((ord(Value) SHL 2) AND efDisposal);
end;

procedure TGIFGraphicControlExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FGCExtension, sizeof(FGCExtension));
end;

procedure TGIFGraphicControlExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  if (Stream.Read(FGCExtension, sizeof(FGCExtension)) <> sizeof(FGCExtension)) then
  begin
    Warning(gsWarning, sOutOfData);
    exit;
  end;
  // Set transparent flag in sub image
  if (Transparent) then
    SubImage.FTransparent := True;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFTextExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFTextExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FText := TStringList.Create;
  FPlainTextExtension.BlockSize := 12;
  FPlainTextExtension.Left := 0;
  FPlainTextExtension.Top := 0;
  FPlainTextExtension.Width := 0;
  FPlainTextExtension.Height := 0;
  FPlainTextExtension.CellWidth := 0;
  FPlainTextExtension.CellHeight := 0;
  FPlainTextExtension.TextFGColorIndex := 0;
  FPlainTextExtension.TextBGColorIndex := 0;
end;

destructor TGIFTextExtension.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TGIFTextExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsPlainTextExtension;
end;

function TGIFTextExtension.GetForegroundColor: TColor;
begin
  Result := SubImage.ColorMap[ForegroundColorIndex];
end;

procedure TGIFTextExtension.SetForegroundColor(Color: TColor);
var
  Idx			: integer;
begin
  with SubImage do
  begin
    Idx := ActiveColorMap.IndexOf(Color);
    if (Idx = -1) then
      Idx := ActiveColorMap.Add(Color);
  end;
  ForegroundColorIndex := Idx;
end;

function TGIFTextExtension.GetBackgroundColor: TColor;
begin
  Result := SubImage.ActiveColorMap[BackgroundColorIndex];
end;

procedure TGIFTextExtension.SetBackgroundColor(Color: TColor);
var
  Idx			: integer;
begin
  with SubImage do
  begin
    Idx := ColorMap.IndexOf(Color);
    if (Idx = -1) then
      Idx := ColorMap.Add(Color);
  end;
  BackgroundColorIndex := Idx;
end;

function TGIFTextExtension.GetBounds(Index: integer): WORD;
begin
  case (Index) of
    1: Result := FPlainTextExtension.Left;
    2: Result := FPlainTextExtension.Top;
    3: Result := FPlainTextExtension.Width;
    4: Result := FPlainTextExtension.Height;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFTextExtension.SetBounds(Index: integer; Value: WORD);
begin
  case (Index) of
    1: FPlainTextExtension.Left := Value;
    2: FPlainTextExtension.Top := Value;
    3: FPlainTextExtension.Width := Value;
    4: FPlainTextExtension.Height := Value;
  end;
end;

function TGIFTextExtension.GetCharWidthHeight(Index: integer): BYTE;
begin
  case (Index) of
    1: Result := FPlainTextExtension.CellWidth;
    2: Result := FPlainTextExtension.CellHeight;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFTextExtension.SetCharWidthHeight(Index: integer; Value: BYTE);
begin
  case (Index) of
    1: FPlainTextExtension.CellWidth := Value;
    2: FPlainTextExtension.CellHeight := Value;
  end;
end;

function TGIFTextExtension.GetColorIndex(Index: integer): BYTE;
begin
  case (Index) of
    1: Result := FPlainTextExtension.TextFGColorIndex;
    2: Result := FPlainTextExtension.TextBGColorIndex;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFTextExtension.SetColorIndex(Index: integer; Value: BYTE);
begin
  case (Index) of
    1: FPlainTextExtension.TextFGColorIndex := Value;
    2: FPlainTextExtension.TextBGColorIndex := Value;
  end;
end;

procedure TGIFTextExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FPlainTextExtension, sizeof(FPlainTextExtension));
  WriteStrings(Stream, FText);
end;

procedure TGIFTextExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  ReadCheck(Stream, FPlainTextExtension, sizeof(FPlainTextExtension));
  ReadStrings(Stream, FText);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFCommentExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFCommentExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FText := TStringList.Create;
end;

destructor TGIFCommentExtension.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TGIFCommentExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsCommentExtension;
end;

procedure TGIFCommentExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteStrings(Stream, FText);
end;

procedure TGIFCommentExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  ReadStrings(Stream, FText);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TGIFApplicationExtension registration database
//
////////////////////////////////////////////////////////////////////////////////
type
  PAppExtRec = ^TAppExtRec;
  TAppExtRec = record
    AppClass: TGIFAppExtensionClass;
    Ident: TGIFApplicationRec;
  end;

  TAppExtensionList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
    function FindExt(eIdent: TGIFApplicationRec): TGIFAppExtensionClass;
    procedure Remove(eClass: TGIFAppExtensionClass);
  end;

constructor TAppExtensionList.Create;
const
  NSLoopIdent: array[0..1] of TGIFApplicationRec =
    ((Identifier: 'NETSCAPE'; Authentication: '2.0'),
     (Identifier: 'ANIMEXTS'; Authentication: '1.0'));
begin
  inherited Create;
  Add(NSLoopIdent[0], TGIFAppExtNSLoop);
  Add(NSLoopIdent[1], TGIFAppExtNSLoop);
end;

destructor TAppExtensionList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PAppExtRec(Items[I]));
  inherited Destroy;
end;

procedure TAppExtensionList.Add(eIdent: TGIFApplicationRec;
  eClass: TGIFAppExtensionClass);
var
  NewRec: PAppExtRec;
begin
  New(NewRec);
  NewRec^.Ident := eIdent;
  NewRec^.AppClass := eClass;
  inherited Add(NewRec);
end;

function TAppExtensionList.FindExt(eIdent: TGIFApplicationRec): TGIFAppExtensionClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    with PAppExtRec(Items[I])^ do
      if CompareMem(@Ident, @eIdent, sizeof(TGIFApplicationRec)) then
      begin
        Result := AppClass;
        Exit;
      end;
  Result := nil;
end;

procedure TAppExtensionList.Remove(eClass: TGIFAppExtensionClass);
var
  I: Integer;
  P: PAppExtRec;
begin
  for I := Count-1 downto 0 do
  begin
    P := PAppExtRec(Items[I]);
    if P^.AppClass.InheritsFrom(eClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

var
  AppExtensionList: TAppExtensionList = nil;

function GetAppExtensionList: TAppExtensionList;
begin
  if (AppExtensionList = nil) then
    AppExtensionList := TAppExtensionList.Create;
  Result := AppExtensionList;
end;

class procedure TGIFApplicationExtension.RegisterExtension(eIdent: TGIFApplicationRec;
  eClass: TGIFAppExtensionClass);
begin
  GetAppExtensionList.Add(eIdent, eClass);
end;

class function TGIFApplicationExtension.FindSubExtension(Stream: TStream): TGIFExtensionClass;
var
  eIdent		: TGIFApplicationRec;
  OldPos		: longInt;
  Size			: BYTE;
begin
  OldPos := Stream.Position;
  Result := nil;
  if (Stream.Read(Size, 1) <> 1) then
    exit;

  // Some old Adobe export filters mistakenly uses a value of 10
  if (Size = 10) then
  begin
    // ***FIXME*** replace with seek or...
    // read and check contents = 'Adobe'
    if (Stream.Read(eIdent, 10) <> 10) then
      exit;
    Result := TGIFUnknownAppExtension;
    exit;
  end else
  if (Size <> sizeof(TGIFApplicationRec)) or
    (Stream.Read(eIdent, sizeof(eIdent)) <> sizeof(eIdent)) then
  begin
    Stream.Position := OldPos;
    Result := inherited FindSubExtension(Stream);
  end else
  begin
    Result := GetAppExtensionList.FindExt(eIdent);
    if (Result = nil) then
      Result := TGIFUnknownAppExtension;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFApplicationExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFApplicationExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FillChar(FIdent, sizeof(FIdent), 0);
end;

destructor TGIFApplicationExtension.Destroy;
begin
  inherited Destroy;
end;

function TGIFApplicationExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsApplicationExtension;
end;

procedure TGIFApplicationExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteByte(Stream, sizeof(FIdent)); // Block size
  Stream.Write(FIdent, sizeof(FIdent));
  SaveData(Stream);
end;

procedure TGIFApplicationExtension.LoadFromStream(Stream: TStream);
var
  i			: integer;
begin
  inherited LoadFromStream(Stream);
  i := ReadByte(Stream);
  // Some old Adobe export filters mistakenly uses a value of 10
  if (i = 10) then
    FillChar(FIdent, sizeOf(FIdent), 0)
  else
    if (i < 11) then
      Error(sBadBlockSize);

  ReadCheck(Stream, FIdent, sizeof(FIdent));

  Dec(i, sizeof(FIdent));
  // Ignore extra data
  Stream.Seek(i, soFromCurrent);

  // ***FIXME***
  // If self class is TGIFApplicationExtension, this will cause an "abstract
  // error".
  // TGIFApplicationExtension.LoadData should read and ignore rest of block.
  LoadData(Stream);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFUnknownAppExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFBlock.Create(ASize: integer);
begin
  inherited Create;
  FSize := ASize;
  GetMem(FData, FSize);
end;

destructor TGIFBlock.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

procedure TGIFBlock.SaveToStream(Stream: TStream);
begin
  Stream.Write(FSize, 1);
  Stream.Write(FData^, FSize);
end;

procedure TGIFBlock.LoadFromStream(Stream: TStream);
begin
  ReadCheck(Stream, FData^, FSize);
end;

constructor TGIFUnknownAppExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FBlocks := TList.Create;
end;

destructor TGIFUnknownAppExtension.Destroy;
var
  i			: integer;
begin
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).Free;
  FBlocks.Free;
  inherited Destroy;
end;


procedure TGIFUnknownAppExtension.SaveData(Stream: TStream);
var
  i			: integer;
begin
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).SaveToStream(Stream);
  // Terminating zero
  WriteByte(Stream, 0);
end;

procedure TGIFUnknownAppExtension.LoadData(Stream: TStream);
var
  b			: BYTE;
  Block			: TGIFBlock;
  i			: integer;
begin
  // Zap old blocks
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).Free;
  FBlocks.Clear;

  // Read blocks
  if (Stream.Read(b, 1) <> 1) then
    exit;
  while (b <> 0) do
  begin
    Block := TGIFBlock.Create(b);
    try
      Block.LoadFromStream(Stream);
    except
      Block.Free;
      raise;
    end;
    FBlocks.Add(Block);
    if (Stream.Read(b, 1) <> 1) then
      exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFAppExtNSLoop
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFAppExtNSLoop.Create(ASubImage: TGIFSubImage);
const
  NSLoopIdent: TGIFApplicationRec = (Identifier: 'NETSCAPE'; Authentication: '2.0');
begin
  inherited Create(ASubImage);
  FIdent := NSLoopIdent;
end;

procedure TGIFAppExtNSLoop.SaveData(Stream: TStream);
begin
  WriteByte(Stream, 3); // Size of block
  WriteByte(Stream, 1); // Dummy - must be 1
  Stream.Write(FLoops, sizeof(FLoops)); // Loop count
  WriteByte(Stream, 0); // Terminating zero
end;

procedure TGIFAppExtNSLoop.LoadData(Stream: TStream);
begin
  if (ReadByte(Stream) <> 3) then // Size of block
    Error(sInvalidData);
  if (ReadByte(Stream) <> 1) then  // Dummy - must be 1
    Error(sInvalidData);
  ReadCheck(Stream, FLoops, sizeof(FLoops)); // Loop count
  if (ReadByte(Stream) <> 0) then // Terminating zero
    Error(sInvalidData);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TRenderer and TRenderThread
//
////////////////////////////////////////////////////////////////////////////////
type
  TRenderer = class(TObject)
  private
    FRenderQueue	: TThreadList;
    FRenderPool		: TThreadList;
    FJobSemaphore	: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    function Render(Painter: TGIFPainter): boolean;
    procedure Remove(Painter: TGIFPainter);
    property RenderQueue: TThreadList read FRenderQueue;
    property RenderPool: TThreadList read FRenderPool;
    property JobSemaphore: THandle read FJobSemaphore;
  end;

  TRenderThread = class(TThread)
  private
    FPainter		: TGIFPainter;
    FRenderer		: TRenderer;
    FIdle		: boolean;
  protected
    procedure DoRender;
    procedure Execute; override;
  public
    constructor Create(Renderer: TRenderer);
    property Idle: boolean read FIdle;
  end;

constructor TRenderer.Create;
begin
  inherited Create;
  FJobSemaphore := CreateSemaphore(nil, 0, high(LongInt), nil);
  FRenderQueue := TThreadList.Create;
  FRenderPool := TThreadList.Create;
end;

destructor TRenderer.Destroy;
var
  i			: integer;
begin
  // Terminate all worker threads
  with FRenderPool.LockList do
    try
      for i := 0 to Count-1 do
        TRenderThread(Items[i]).Terminate;
      ReleaseSemaphore(FJobSemaphore, Count, nil);
      Clear;
    finally
      FRenderPool.UnlockList;
    end;
  // Abort and zap all jobs
  with FRenderQueue.LockList do
    try
      for i := 0 to Count-1 do
        SetEvent(TGIFPainter(Items[i]).EventHandle);
      Clear;
    finally
      FRenderQueue.UnlockList;
    end;
  FRenderPool.Free;
  FRenderQueue.Free;
  CloseHandle(FJobSemaphore);
  inherited Destroy;
end;

procedure TRenderer.Remove(Painter: TGIFPainter);
var
  i			: integer;
begin
  // Remove all of painters jobs from queue
  with FRenderQueue.LockList do
    try
      for i := Count-1 downto 0 do
        if (Items[i] = Painter) then
          Delete(i);
    finally
      FRenderQueue.UnlockList;
    end;
end;

function TRenderer.Render(Painter: TGIFPainter): boolean;
var
  RenderThread		: TRenderThread;
begin
  with FRenderQueue.LockList do
    try
      // Find an idle worker thread
      with FRenderPool.LockList do
        try
          // First thread in pool should be idle if any are idle
          // If none are idle and we haven't reached the limit, create a new
          // thread.
          // Otherwise just wait for one of the existing threads to finish
          if ((Count = 0) or (not TRenderThread(First).Idle)) and
            (Count < MaxRenderThreads) then
          begin
            RenderThread := TRenderThread.Create(self);
            Add(RenderThread);
            RenderThread.Resume;
          end;
        finally
          FRenderPool.UnlockList;
        end;

      // Add job to render queue
      Add(painter);
      // Signal the threads that there are jobs to run
      ReleaseSemaphore(FJobSemaphore, 1, nil);
    finally
      FRenderQueue.UnlockList;
    end;

  // Wait for completion or abort
  Result := (WaitForSingleObject(Painter.EventHandle, MaxRenderTime) <> WAIT_TIMEOUT);
  // Attempt to remove job from queue if we timed out
  // Note: The semapahore is left at its current state since we don't know
  // if is has been decremented already
  if (not Result) then
    with FRenderQueue.LockList do
      try
        Remove(Painter);
      finally
        FRenderQueue.UnlockList;
      end;
end;

constructor TRenderThread.Create(Renderer: TRenderer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FRenderer := Renderer;
  FIdle := True;
end;

procedure TRenderThread.DoRender;
var
  i			: integer;
begin
  // Touch bitmap to render it
  i := FPainter.ActiveImage;
  while (i < FPainter.Image.Images.Count) do
  begin
    if not((FPainter.Image.Images[i].Empty) or (FPainter.Image.Images[i].HasBitmap)) then
    begin
      FPainter.Image.Images[i].Bitmap;
      break;
    end;
    inc(i);
  end;;
end;

procedure TRenderThread.Execute;
var
  JobsWaiting		: boolean;
begin
  while (not Terminated) do
  begin
    if (WaitForSingleObject(FRenderer.JobSemaphore, MaxRenderIdleTime) = WAIT_TIMEOUT) then
    begin
      // Timed out waiting for job - Remove from list and die
      with FRenderer.RenderPool.LockList do
        try
          Remove(self);
        finally
          FRenderer.RenderPool.UnlockList;
        end;
      exit;
    end;
    if (Terminated) then
      exit;

    with FRenderer.RenderQueue.LockList do
      try

        if (Terminated) then
          exit;

        // Fetch a job from the queue
        if (Count = 0) then
        begin
          FIdle := True;
          // Nothing to do - Move to start of pool and suspend
          with FRenderer.RenderPool.LockList do
            try
              Remove(self);
              Insert(0, self);
            finally
              FRenderer.RenderPool.UnlockList;
            end;
          FPainter := nil;
        end else
        begin
          FIdle := False;
          FPainter := TGIFPainter(First);
          Remove(FPainter);
        end;
      finally
        FRenderer.RenderQueue.UnlockList;
      end;

    if (Terminated) then
      exit;

    if (FPainter <> nil) then
    begin
      Priority := FPainter.Priority;
      try
        // Render the bitmap
        Synchronize(DoRender);
      finally
        Priority := tpNormal;
      end;
      // Render completed - signal job thread to resume
      SetEvent(FPainter.EventHandle);
      FIdle := True;
      // Suspend thread for a while if there are jobs waiting to avoid congestion
      with FRenderer.RenderQueue.LockList do
        try
          JobsWaiting := (Count > 0);
        finally
          FRenderer.RenderQueue.UnlockList;
        end;
      if (JobsWaiting) then
        Sleep(10);
    end;

  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImageList
//
////////////////////////////////////////////////////////////////////////////////
function TGIFImageList.GetImage(Index: Integer): TGIFSubImage;
begin
  Result := TGIFSubImage(Items[Index]);
end;

procedure TGIFImageList.SetImage(Index: Integer; SubImage: TGIFSubImage);
begin
  Items[Index] := SubImage;
end;

procedure TGIFImageList.LoadFromStream(Stream: TStream; Parent: TObject);
var
  b			: BYTE;
  SubImage		: TGIFSubImage;
begin
  // Peek ahead to determine block type
  repeat
    if (Stream.Read(b, 1) <> 1) then
      exit;
  until (b <> 0); // Ignore 0 padding (non-compliant)

  while (b <> bsTrailer) do
  begin
    Stream.Seek(-1, soFromCurrent);
    if (b in [bsExtensionIntroducer, bsImageDescriptor]) then
    begin
      SubImage := TGIFSubImage.Create(Parent as TGIFImage);
      try
        SubImage.LoadFromStream(Stream);
        Image.Progress(Self, psRunning, MulDiv(Stream.Position, 100, Stream.Size), False, Rect(0,0,0,0), sProgressLoading);
        Add(SubImage);
      except
        SubImage.Free;
        raise;
      end;
    end else
    begin
      Warning(gsWarning, sBadBlock);
      break;
    end;
    repeat
      if (Stream.Read(b, 1) <> 1) then
        exit;
    until (b <> 0); // Ignore 0 padding (non-compliant)
  end;
  Stream.Seek(-1, soFromCurrent);
end;

procedure TGIFImageList.SaveToStream(Stream: TStream);
var
  i			: integer;
begin
  for i := 0 to Count-1 do
  begin
    TGIFItem(Items[i]).SaveToStream(Stream);
    Image.Progress(Self, psRunning, MulDiv((i+1), 100, Count), False, Rect(0,0,0,0), sProgressSaving);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFPainter
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFPainter.CreateRef(Painter: PGIFPainter; AImage: TGIFImage;
  ACanvas: TCanvas; ARect: TRect; Options: TGIFDrawOptions);
begin
  Create(AImage, ACanvas, ARect, Options);
  PainterRef := Painter;
  if (PainterRef <> nil) then
    PainterRef^ := self;
end;

constructor TGIFPainter.Create(AImage: TGIFImage; ACanvas: TCanvas; ARect: TRect;
  Options: TGIFDrawOptions);
var
  i			: integer;
  BackgroundColor	: TColor;
  Disposals		: set of TDisposalMethod;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Onterminate := DoOnTerminate;
  FImage := AImage;
  FCanvas := ACanvas;
  FRect := ARect;
  FActiveImage := -1;
  FDrawOptions := Options;
  FStarted := False;
  BackupBuffer := nil;
  FrameBuffer := nil;
  Background := nil;
  FEventHandle := 0;
  // This should be a parameter, but I think I've got enough of them already...
  FAnimationSpeed := FImage.AnimationSpeed;

  // An event handle is used for animation delays
  if (FDrawOptions >= [goAnimate, goAsync]) and (FImage.Images.Count > 1) and
    (FAnimationSpeed >= 0) then
    FEventHandle := CreateEvent(nil, False, False, nil);

  // Preprocessing of extensions to determine if we need frame buffers
  Disposals := [];
  if (FImage.DrawBackgroundColor = clNone) then
  begin
    if (FImage.GlobalColorMap.Count > 0) then
      BackgroundColor := FImage.BackgroundColor
    else
      BackgroundColor := ColorToRGB(clWindow);
  end else
    BackgroundColor := ColorToRGB(FImage.DrawBackgroundColor);

  // Need background buffer to clear on loop
  if (goClearOnLoop in FDrawOptions) then
    Include(Disposals, dmBackground);

  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].GraphicControlExtension <> nil) then
      with (FImage.Images[i].GraphicControlExtension) do
        Include(Disposals, Disposal);

  // Need background buffer to draw transparent on background
  if (dmBackground in Disposals) and (goTransparent in FDrawOptions) then
  begin
    Background := TBitmap.Create;
    Background.Height := FRect.Bottom-FRect.Top;
    Background.Width := FRect.Right-FRect.Left;
    // Copy background immediately
    Background.Canvas.CopyMode := cmSrcCopy;
    Background.Canvas.CopyRect(Background.Canvas.ClipRect, FCanvas, FRect);
  end;
  // Need frame- and backup buffer to restore to previous and background
  if ((Disposals * [dmPrevious, dmBackground]) <> []) then
  begin
    BackupBuffer := TBitmap.Create;
    BackupBuffer.Height := FRect.Bottom-FRect.Top;
    BackupBuffer.Width := FRect.Right-FRect.Left;
    BackupBuffer.Canvas.CopyMode := cmSrcCopy;
    BackupBuffer.Canvas.Brush.Color := BackgroundColor;
    BackupBuffer.Canvas.Brush.Style := bsSolid;
{$IFDEF DEBUG}
    BackupBuffer.Canvas.Brush.Color := clBlack;
    BackupBuffer.Canvas.Brush.Style := bsDiagCross;
    SetBkColor(BackupBuffer.Canvas.Handle, ColorToRGB(BackgroundColor));
{$ENDIF}
    // Step 1: Copy destination to backup buffer
    //         Always executed before first frame and only once.
    BackupBuffer.Canvas.CopyRect(BackupBuffer.Canvas.ClipRect, FCanvas, FRect);
    FrameBuffer := TBitmap.Create;
    FrameBuffer.Height := FRect.Bottom-FRect.Top;
    FrameBuffer.Width := FRect.Right-FRect.Left;
    FrameBuffer.Canvas.CopyMode := cmSrcCopy;
    FrameBuffer.Canvas.Brush.Color := BackgroundColor;
    FrameBuffer.Canvas.Brush.Style := bsSolid;
{$IFDEF DEBUG}
    FrameBuffer.Canvas.Brush.Color := clBlack;
    FrameBuffer.Canvas.Brush.Style := bsDiagCross;
    SetBkColor(FrameBuffer.Canvas.Handle, ColorToRGB(BackgroundColor));
{$ENDIF}
  end;
end;

destructor TGIFPainter.Destroy;
begin
  // OnTerminate isn't called if we are running in main thread, so we must call
  // it manually
  if not(goAsync in DrawOptions) then
    DoOnTerminate(self);
  // Reraise any exptions that were eaten in the Execute method
  if (ExceptObject <> nil) then
    raise ExceptObject at ExceptAddress;
  inherited Destroy;
end;

procedure TGIFPainter.SetAnimationSpeed(Value: integer);
begin
  if (Value < 0) then
    Value := 0
  else if (Value > 1000) then
    Value := 1000;
  if (Value <> FAnimationSpeed) then
  begin
    FAnimationSpeed := Value;
    // Signal WaitForSingleObject delay to abort
    if (FEventHandle <> 0) then
      SetEvent(FEventHandle)
    else
      DoRestart := True;
  end;
end;

procedure TGIFPainter.SetActiveImage(const Value: integer);
begin
  if (Value >= 0) and (Value < FImage.Images.Count) then
    FActiveImage := Value;
end;

// Conditional Synchronize
procedure TGIFPainter.DoSynchronize(Method: TThreadMethod);
begin
  if (Terminated) then
    exit;
  if (goAsync in FDrawOptions) then
    // Execute Synchronized if requested...
    Synchronize(Method)
  else
    // ...Otherwise just execute in current thread (probably main thread)
    Method;
end;

// Delete frame buffers - Executed in main thread
procedure TGIFPainter.DoOnTerminate(Sender: TObject);
begin
  // It shouldn't really be nescessary to protect PainterRef in this manner
  // since we are running in the main thread at this point, but I'm a little
  // paranoid about the way PainterRef is being used...
  with Image.Painters.LockList do
    try
      // Zap pointer to self and remove from painter list
      if (PainterRef <> nil) and (PainterRef^ = self) then
        PainterRef^ := nil;

    finally
      Image.Painters.UnLockList;
    end;

  Image.Painters.Remove(self);
  FImage := nil;


  // Free buffers
  if (BackupBuffer <> nil) then
    BackupBuffer.Free;
  if (FrameBuffer <> nil) then
    FrameBuffer.Free;
  if (Background <> nil) then
    Background.Free;

  // Delete event handle
  if (FEventHandle <> 0) then
    CloseHandle(FEventHandle);
end;

// Event "dispatcher" - Executed in main thread
procedure TGIFPainter.DoEvent;
begin
  if (Assigned(FEvent)) then
    FEvent(self);
end;

// Non-buffered paint - Executed in main thread
procedure TGIFPainter.DoPaint;
begin
  FImage.Images[ActiveImage].Draw(FCanvas, FRect, (goTransparent in FDrawOptions),
    (goTile in FDrawOptions));
  FStarted := True;
end;

// Buffered paint - Executed in main thread
procedure TGIFPainter.DoPaintFrame;
var
  DrawDestination	: TCanvas;
  DrawRect		: TRect;
  DoStep2		,
  DoStep3		,
  DoStep5		,
  DoStep6		: boolean;
  SavePal		,
  SourcePal		: HPALETTE;

  procedure ClearBackup;
  var
    r			,
    Tile		: TRect;
    FrameTop		,
    FrameHeight		: integer;
    ImageWidth		,
    ImageHeight		: integer;
  begin

    if (goTransparent in FDrawOptions) then
    begin
      // If the frame is transparent, we must remove it by copying the
      // background buffer over it
      if (goTile in FDrawOptions) then
      begin
        FrameTop := FImage.Images[ActiveImage].Top;
        FrameHeight := FImage.Images[ActiveImage].Height;
        ImageWidth := FImage.Width;
        ImageHeight := FImage.Height;

        Tile.Left := FRect.Left + FImage.Images[ActiveImage].Left;
        Tile.Right := Tile.Left + FImage.Images[ActiveImage].Width;
        while (Tile.Left < FRect.Right) do
        begin
          Tile.Top := FRect.Top + FrameTop;
          Tile.Bottom := Tile.Top + FrameHeight;
          while (Tile.Top < FRect.Bottom) do
          begin
            BackupBuffer.Canvas.CopyRect(Tile, Background.Canvas, Tile);
            Tile.Top := Tile.Top + ImageHeight;
            Tile.Bottom := Tile.Bottom + ImageHeight;
          end;
          Tile.Left := Tile.Left + ImageWidth;
          Tile.Right := Tile.Right + ImageWidth;
        end;
      end else
      begin
        r := FImage.Images[ActiveImage].ScaleRect(BackupBuffer.Canvas.ClipRect);
        BackupBuffer.Canvas.CopyRect(r, Background.Canvas, r)
      end;
    end else
    begin
      // If the frame isn't transparent, we just clear the area covered by
      // it to the background color.
      // Tile the background unless the frame covers all of the image
      if (goTile in FDrawOptions) and
        ((FImage.Width <> FImage.Images[ActiveImage].Width) and
         (FImage.height <> FImage.Images[ActiveImage].Height)) then
      begin
        FrameTop := FImage.Images[ActiveImage].Top;
        FrameHeight := FImage.Images[ActiveImage].Height;
        ImageWidth := FImage.Width;
        ImageHeight := FImage.Height;
        BackupBuffer.Canvas.Brush.Color := clRed;

        Tile.Left := FRect.Left + FImage.Images[ActiveImage].Left;
        Tile.Right := Tile.Left + FImage.Images[ActiveImage].Width;
        while (Tile.Left < FRect.Right) do
        begin
          Tile.Top := FRect.Top + FrameTop;
          Tile.Bottom := Tile.Top + FrameHeight;
          while (Tile.Top < FRect.Bottom) do
          begin
            BackupBuffer.Canvas.FillRect(Tile);

            Tile.Top := Tile.Top + ImageHeight;
            Tile.Bottom := Tile.Bottom + ImageHeight;
          end;
          Tile.Left := Tile.Left + ImageWidth;
          Tile.Right := Tile.Right + ImageWidth;
        end;
      end else
        BackupBuffer.Canvas.FillRect(FImage.Images[ActiveImage].ScaleRect(FRect));
    end;
  end;

begin
  if (goValidateCanvas in FDrawOptions) then
    if (GetObjectType(ValidateDC) <> OBJ_DC) then
    begin
      Terminate;
      exit;
    end;

  DrawDestination := nil;
  DoStep2 := (goClearOnLoop in FDrawOptions) and (FActiveImage = 0);
  DoStep3 := False;
  DoStep5 := False;
  DoStep6 := False;
{
Disposal mode algorithm:

Step 1: Copy destination to backup buffer
        Always executed before first frame and only once.
        Done in constructor.
Step 2: Clear previous frame (implementation is same as step 6)
        Done implicitly by implementation.
        Only done explicitly on first frame if goClearOnLoop option is set.
Step 3: Copy backup buffer to frame buffer
Step 4: Draw frame
Step 5: Copy buffer to destination
Step 6: Clear frame from backup buffer
+------------+------------------+---------------------+------------------------+
|New  \  Old |  dmNone          |  dmBackground       |  dmPrevious            |
+------------+------------------+---------------------+------------------------+
|dmNone      |                  |                     |                        |
|            |4. Paint on backup|4. Paint on backup   |4. Paint on backup      |
|            |5. Restore        |5. Restore           |5. Restore              |
+------------+------------------+---------------------+------------------------+
|dmBackground|                  |                     |                        |
|            |4. Paint on backup|4. Paint on backup   |4. Paint on backup      |
|            |5. Restore        |5. Restore           |5. Restore              |
|            |6. Clear backup   |6. Clear backup      |6. Clear backup         |
+------------+------------------+---------------------+------------------------+
|dmPrevious  |                  |                     |                        |
|            |                  |3. Copy backup to buf|3. Copy backup to buf   |
|            |4. Paint on dest  |4. Paint on buf      |4. Paint on buf         |
|            |                  |5. Copy buf to dest  |5. Copy buf to dest     |
+------------+------------------+---------------------+------------------------+
}
  case (Disposal) of
    dmNone, dmNoDisposal:
    begin
      DrawDestination := BackupBuffer.Canvas;
      DrawRect := BackupBuffer.Canvas.ClipRect;
      DoStep5 := True;
    end;
    dmBackground:
    begin
      DrawDestination := BackupBuffer.Canvas;
      DrawRect := BackupBuffer.Canvas.ClipRect;
      DoStep5 := True;
      DoStep6 := True;
    end;
    dmPrevious:
      case (OldDisposal) of
        dmNone, dmNoDisposal:
        begin
          DrawDestination := FCanvas;
          DrawRect := FRect;
        end;
        dmBackground,
        dmPrevious:
        begin
          DrawDestination := FrameBuffer.Canvas;
          DrawRect := FrameBuffer.Canvas.ClipRect;
          DoStep3 := True;
          DoStep5 := True;
        end;
      end;
  end;

  // Find source palette
  SourcePal := FImage.Images[ActiveImage].Palette;
  if (SourcePal = 0) then
    SourcePal := SystemPalette16; // This should never happen

  SavePal := SelectPalette(DrawDestination.Handle, SourcePal, False);
  RealizePalette(DrawDestination.Handle);

  // Step 2: Clear previous frame
  if (DoStep2) then
   ClearBackup;

  // Step 3: Copy backup buffer to frame buffer
  if (DoStep3) then
    FrameBuffer.Canvas.CopyRect(FrameBuffer.Canvas.ClipRect,
      BackupBuffer.Canvas, BackupBuffer.Canvas.ClipRect);

  // Step 4: Draw frame
  if (DrawDestination <> nil) then
    FImage.Images[ActiveImage].Draw(DrawDestination, DrawRect,
      (goTransparent in FDrawOptions), (goTile in FDrawOptions));

  // Step 5: Copy buffer to destination
  if (DoStep5) then
  begin
    FCanvas.CopyMode := cmSrcCopy;
    FCanvas.CopyRect(FRect, DrawDestination, DrawRect);
  end;

  if (SavePal <> 0) then
    SelectPalette(DrawDestination.Handle, SavePal, False);

  // Step 6: Clear frame from backup buffer
  if (DoStep6) then
   ClearBackup;

  FStarted := True;
end;

var
  Renderer: TRenderer = nil;

// Prefetch bitmap
// Used to force the GIF image to be rendered as a bitmap
procedure TGIFPainter.PrefetchBitmap;
var
  i			: integer;
begin
  if (EventHandle = 0) or (not (goAsync in DrawOptions)) then
  begin
    // Touch current bitmap to force bitmap to be rendered
    if not((FImage.Images[ActiveImage].Empty) or (FImage.Images[ActiveImage].HasBitmap)) then
      FImage.Images[ActiveImage].Bitmap;
    exit;
  end;

  if (Renderer = nil) then
    Renderer := TRenderer.Create;

  i := ActiveImage;
  while (i < FImage.Images.Count) do
  begin
    // Only render non-empty images that hasn't already been rendered
    if not((FImage.Images[i].Empty) or (FImage.Images[i].HasBitmap)) then
    begin
      Renderer.Render(Self);
      break;
    end;
    inc(i);
  end;
end;

// Main thread execution loop - This is where it all happens...
procedure TGIFPainter.Execute;
var
  i			: integer;
  LoopCount		,
  LoopPoint		: integer;
  Looping		: boolean;
  Ext			: TGIFExtension;
  Msg			: TMsg;
  Delay			,
  OldDelay		,
  DelayUsed		,
  DelayStart		,
  NewDelayStart		: longInt;

  procedure FireEvent(Event: TNotifyEvent);
  begin
    if not(Assigned(Event)) then
      exit;
    FEvent := Event;
    try
      DoSynchronize(DoEvent);
    finally
      FEvent := nil;
    end;
  end;

begin
{
  Disposal:
    dmNone: Same as dmNodisposal
    dmNoDisposal: Do not dispose
    dmBackground: Clear with background color *)
    dmPrevious: Previous image
    *) Note: Background color should either be a BROWSER SPECIFIED Background
       color (DrawBackgroundColor) or the background image if any frames are
       transparent.
}
  try
    try
      if (goValidateCanvas in FDrawOptions) then
        ValidateDC := FCanvas.Handle;
      DoRestart := True;

      // Loop to restart paint
      while (DoRestart) and not(Terminated) do
      begin
        FActiveImage := 0;
        // Fire OnStartPaint event
        // Note: ActiveImage may be altered by the event handler
        FireEvent(FOnStartPaint);

        FStarted := False;
        DoRestart := False;
        LoopCount := 1;
        LoopPoint := FActiveImage;
        Looping := False;
        if (goAsync in DrawOptions) then
          Delay := 0
        else
          Delay := 1; // Dummy to process messages
        OldDisposal := dmNoDisposal;
        // Fetch delay start time
        DelayStart := timeGetTime;
        OldDelay := 0;

        // Loop to loop - duh!
        while ((LoopCount <> 0) or (goLoopContinously in DrawOptions)) and
          not(Terminated or DoRestart) do
        begin
          FActiveImage := LoopPoint;

          // Fire OnLoopPaint event
          // Note: ActiveImage may be altered by the event handler
          if (FStarted) then
            FireEvent(FOnLoop);

          // Loop to animate
          while (ActiveImage < FImage.Images.Count) and not(Terminated or DoRestart) do
          begin
            // Ignore empty images
            if (FImage.Images[ActiveImage].Empty) then
              break;
            // Delay from previous image
            if (Delay > 0) then
            begin
              // Prefetch frame bitmap
              PrefetchBitmap;

              // Calculate inter frame delay
              NewDelayStart := longInt(timeGetTime);
              if (FAnimationSpeed > 0) then
              begin
                // Calculate number of mS used in prefetch and display
                try
                  DelayUsed := (NewDelayStart-DelayStart)-OldDelay;
                  OldDelay := Delay * GIFDelayExp;
                  // Convert delay value to mS and...
                  // ...Adjust for time already spent converting GIF to bitmap and...
                  // ...Adjust for Animation Speed factor.
                  Delay := MulDiv(Delay * GIFDelayExp - DelayUsed, 100, FAnimationSpeed);
                except
                  Delay := GIFMaximumDelay * GIFDelayExp;
                end;
              end else
              begin
                if (goAsync in DrawOptions) then
                  Delay := longInt(INFINITE)
                else
                  Delay := GIFMaximumDelay * GIFDelayExp;
              end;
              // Fetch delay start time
              DelayStart := NewDelayStart;

              // Sleep in one chunk if we are running in a thread
              if (goAsync in DrawOptions) then
              begin
                // Use of WaitForSingleObject allows TGIFPainter.Stop to wake us up
                if (Delay > 0) or (FAnimationSpeed = 0) then
                begin
                  if (WaitForSingleObject(FEventHandle, DWORD(Delay)) <> WAIT_TIMEOUT) then
                    Delay := 0;
                end;
              end else
              begin
                if (Delay <= 0) then
                  Delay := 1;
                // Fetch start time
                DelayUsed := timeGetTime;
                // If we are not running in a thread we Sleep in small chunks
                // and give the user a chance to abort
                while (Delay > 0) and not(Terminated or DoRestart) do
                begin
                  if (Delay < 100) then
                    Sleep(Delay)
                  else
                    Sleep(100);
                  // Calculate number of mS delayed in this chunk
                  DelayUsed := longInt(timeGetTime) - DelayUsed;
                  dec(Delay, DelayUsed);
                  // Reset start time for chunk
                  DelayUsed := timeGetTime;
                  // Application.ProcessMessages wannabe
                  while (not(Terminated or DoRestart)) and
                    (PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) do
                  begin
                    if (Msg.Message <> WM_QUIT) then
                    begin
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                    end else
                    begin
                      // Put WM_QUIT back in queue and get out of here fast
                      PostQuitMessage(Msg.WParam);
                      Terminate;
                    end;
                  end;
                end;
              end;
            end else
              Sleep(0); // Yield
            if (Terminated) then
              break;

            // Fire OnPaint event
            // Note: ActiveImage may be altered by the event handler
            FireEvent(FOnPaint);
            if (Terminated) then
              break;

            // Pre-draw processing of extensions
            Disposal := dmNoDisposal;
            for i := 0 to FImage.Images[ActiveImage].Extensions.Count-1 do
            begin
              Ext := FImage.Images[ActiveImage].Extensions[i];
              if (Ext is TGIFAppExtNSLoop) then
              begin
                // Recursive loops not supported (or defined)
                if (Looping) then
                  continue;
                Looping := True;
                LoopCount := TGIFAppExtNSLoop(Ext).Loops;
                if ((LoopCount = 0) or (goLoopContinously in DrawOptions)) and
                  (goAsync in DrawOptions) then
                  LoopCount := -1; // Infinite if running in separate thread
{$IFNDEF STRICT_MOZILLA}
                // Loop from this image and on
                // Note: This is not standard behavior
                LoopPoint := ActiveImage;
{$ENDIF}
              end else
              if (Ext is TGIFGraphicControlExtension) then
                Disposal := TGIFGraphicControlExtension(Ext).Disposal;
            end;
            // Paint the image
            if (BackupBuffer <> nil) then
              DoSynchronize(DoPaintFrame)
            else
              DoSynchronize(DoPaint);
            OldDisposal := Disposal;

            // Nothing more to do unless we are animating
            if not(goAnimate in DrawOptions) then
              break;
            if (Terminated) then
              break;

            Delay := GIFDefaultDelay; // Default delay
            // Post-draw processing of extensions
            if (FImage.Images[ActiveImage].GraphicControlExtension <> nil) then
              if (FImage.Images[ActiveImage].GraphicControlExtension.Delay > 0) then
              begin
                Delay := FImage.Images[ActiveImage].GraphicControlExtension.Delay;

                // Enforce minimum animation delay in compliance with Mozilla
                if (Delay < GIFMinimumDelay) then
                  Delay := GIFMinimumDelay;

                // Do not delay more than 10 seconds if running in main thread
                if (Delay > GIFMaximumDelay) and not(goAsync in DrawOptions) then
                  Delay := GIFMaximumDelay; // Max 10 seconds
              end;
            Inc(FActiveImage);
          end;

          if (LoopCount > 0) then
            Dec(LoopCount);
          if ([goAnimate, goLoop] * DrawOptions <> [goAnimate, goLoop]) then
            break;
        end;
      end;
      FActiveImage := -1;
      // Fire OnEndPaint event
      FireEvent(FOnEndPaint);
    finally
      // If we are running in the main thread we will have to zap our self
      if not(goAsync in DrawOptions) then
        Free;
    end;
  except
    on E: Exception do
    begin
      // Eat exception and terminate thread...
      // If we allow the exception to abort the thread at this point, the
      // application will hang since the thread destructor will never be called
      // and the application will wait forever for the thread to die!
      Terminate;
      // Clone exception
      ExceptObject := E.Create(E.Message);
      ExceptAddress := ExceptAddr;
    end;
  end;
end;

procedure TGIFPainter.Start;
begin
  if (goAsync in FDrawOptions) then
    Resume;
end;

procedure TGIFPainter.Stop;
begin
  Terminate;
  if (goAsync in FDrawOptions) then
  begin
    // Signal WaitForSingleObject delay to abort
    if (FEventHandle <> 0) then
      SetEvent(FEventHandle);
    if (Suspended) then
      Resume; // Must be running before we can terminate
  end;
end;

procedure TGIFPainter.Restart;
begin
  DoRestart := True;
  if (Suspended) and (goAsync in FDrawOptions) then
    Resume; // Must be running before we can terminate
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TColorMapOptimizer
//
////////////////////////////////////////////////////////////////////////////////
// Used by TGIFImage to optimize local color maps to a single global color map.
// The following is the algorithm used:
// 1) Build a histogram for each image
// 2) Merge histograms
// 3) Sum equal colors and adjust max # of colors
// 4) Map entries > max to entries <= 256
// 5) Build new color map
// 6) Map images to new color map
////////////////////////////////////////////////////////////////////////////////

type

  POptimizeEntry	= ^TOptimizeEntry;
  TColorRec = record
  case byte of
    0: (Value: integer);
    1: (Color: TGIFColor);
    2: (SameAs: POptimizeEntry); // Used if TOptimizeEntry.Count = 0
  end;

  TOptimizeEntry = record
    Count		: integer;	// Usage count
    OldIndex		: integer;	// Color OldIndex
    NewIndex		: integer;	// NewIndex color OldIndex
    Color		: TColorRec;	// Color value
  end;

  TOptimizeEntries	= array[0..255] of TOptimizeEntry;
  POptimizeEntries	= ^TOptimizeEntries;

  THistogram = class(TObject)
  private
    PHistogram		: POptimizeEntries;
    FCount		: integer;
    FColorMap		: TGIFColorMap;
    FImageIndex		: integer;
    FList		: TList;
    FImages		: TList;
  public
    constructor Create(AIndex: integer; AColorMap: TGIFColorMap);
    destructor Destroy; override;
    procedure ProcessSubImage(Image: TGIFSubImage);
    function Prune: integer;
    procedure MapImages;
    property Count: integer read FCount;
    property ColorMap: TGIFColorMap read FColorMap;
    property ImageIndex: integer read FImageIndex;
    property List: TList read FList;
  end;

  TColorMapOptimizer = class(TObject)
  private
    FImage		: TGIFImage;
    FHistogramList	: TList;
    FHistogram		: TList;
    FColorMap		: TColorMap;
    FFinalCount		: integer;
  protected
    procedure ProcessImage;
    procedure MergeColors;
    procedure MapColors;
    procedure ReplaceColorMaps;
  public
    constructor Create(AImage: TGIFImage);
    destructor Destroy; override;
    procedure Optimize;
  end;

function CompareColor(Item1, Item2: Pointer): integer;
begin
  Result := POptimizeEntry(Item2)^.Color.Value - POptimizeEntry(Item1)^.Color.Value;
end;

function CompareCount(Item1, Item2: Pointer): integer;
begin
  Result := POptimizeEntry(Item2)^.Count - POptimizeEntry(Item1)^.Count;
end;

constructor THistogram.Create(AIndex: integer; AColorMap: TGIFColorMap);
var
  i			: integer;
begin
  inherited Create;

  FCount := AColorMap.Count;
  FImageIndex := AIndex;
  FColorMap := AColorMap;

  FImages := TList.Create;

  // Allocate memory for histogram
  GetMem(PHistogram, FCount * sizeof(TOptimizeEntry));
  FList := TList.Create;

  FList.Capacity := FCount;

  // Move data to histogram and initialize
  for i := 0 to FCount-1 do
    with PHistogram^[i] do
    begin
      FList.Add(@PHistogram^[i]);
      OldIndex := i;
      Count := 0;
      Color.Value := 0;
      Color.Color := AColorMap.Data^[i];
      NewIndex := 256; // Used to signal unmapped
    end;
end;

destructor THistogram.Destroy;
begin
  FImages.Free;
  FList.Free;
  FreeMem(PHistogram);
  inherited Destroy;
end;

procedure THistogram.ProcessSubImage(Image: TGIFSubImage);
var
  Size			: integer;
  Pixel			: PChar;
begin
  if (Image.Empty) then
    exit;

  FImages.Add(Image);

  Pixel := Image.data;
  Size := Image.Width * Image.Height;

  (*
  ** Sum up usage count for each color
  *)
  while (Size > 0) do
  begin
    if (ord(Pixel^) >= FCount) then
    begin
      Pixel^ := #0; // ***FIXME*** Isn't this an error condition?
      Image.Warning(gsWarning, sInvalidColor);
    end;

    with PHistogram^[ord(Pixel^)] do
    begin
      if (Count = high(integer)) then
        break;
      inc(Count);
      inc(Pixel);
      dec(Size);
    end;
  end;
end;

function THistogram.Prune: integer;
var
  i, j			: integer;
begin
  (*
  **  Sort by usage count
  *)
  FList.Sort(CompareCount);

  (*
  **  Determine number of used colors
  *)
  for i := 0 to FCount-1 do
    if (POptimizeEntry(FList[i])^.Count = 0) then
    begin
      // Zap unused colors
      for j := i to FCount-1 do
        POptimizeEntry(FList[j])^.Count := -1; // Use -1 to signal unused entry
      // Remove unused entries
      FCount := i;
      FList.Count := FCount;
      break;
    end;

  Result := FCount;
end;

procedure THistogram.MapImages;
var
  i			: integer;
  Size			: integer;
  Pixel			: PChar;
  ReverseMap		: array[0..255] of BYTE;
begin
  (*
  ** Build NewIndex map
  *)
  for i := 0 to List.Count-1 do
    ReverseMap[POptimizeEntry(List[i])^.OldIndex] := POptimizeEntry(List[i])^.NewIndex;

  (*
  **  Reorder all images using this color map
  *)
  for i := 0 to FImages.Count-1 do
    with TGIFSubImage(FImages[i]) do
    begin
      Pixel := Data;
      Size := Width * Height;
      // Map all pixels to new color map
      while (Size > 0) do
      begin
        Pixel^ := char(ReverseMap[ord(Pixel^)]);
        dec(size);
        inc(Pixel);
      end;
      // Map transparent color
      if (Transparent) then
        GraphicControlExtension.TransparentColorIndex :=
          ReverseMap[GraphicControlExtension.TransparentColorIndex];
    end;
end;

constructor TColorMapOptimizer.Create(AImage: TGIFImage);
begin
  inherited Create;
  FImage := AImage;
  FHistogramList := TList.Create;
  FHistogram := TList.Create;
end;

destructor TColorMapOptimizer.Destroy;
var
  i			: integer;
begin
  FHistogram.Free;

  for i := FHistogramList.Count-1 downto 0 do
    THistogram(FHistogramList[i]).Free;
  FHistogramList.Free;

  inherited Destroy;
end;

procedure TColorMapOptimizer.ProcessImage;
var
  Hist			: THistogram;
  i			: integer;
  ProcessedImage	: boolean;
begin
  // First process images using global color map
  if (FImage.GlobalColorMap.Count > 0) then
  begin
    Hist := THistogram.Create(-1, FImage.GlobalColorMap);
    ProcessedImage := False;
    // Process all images that are using the global color map
    for i := 0 to FImage.Images.Count-1 do
      if (FImage.Images[i].ColorMap.Count = 0) and (not FImage.Images[i].Empty) then
      begin
        ProcessedImage := True;
        Hist.ProcessSubImage(FImage.Images[i]);
      end;
    // Keep the histogram if any images used the global color map...
    if (ProcessedImage) then
      FHistogramList.Add(Hist)
    else // ... otherwise delete it
      Hist.Free;
  end;

  // Next process images that have a local color map
  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].ColorMap.Count > 0) and (not FImage.Images[i].Empty) then
    begin
      Hist := THistogram.Create(i, FImage.Images[i].ColorMap);
      FHistogramList.Add(Hist);
      Hist.ProcessSubImage(FImage.Images[i]);
    end;
end;

procedure TColorMapOptimizer.MergeColors;
var
  Entry, SameEntry	: POptimizeEntry;
  i			: integer;
begin
  (*
  **  Sort by color value
  *)
  FHistogram.Sort(CompareColor);

  (*
  **  Merge same colors
  *)
  SameEntry := POptimizeEntry(FHistogram[0]);
  for i := 1 to FHistogram.Count-1 do
  begin
    Entry := POptimizeEntry(FHistogram[i]);
    ASSERT(Entry^.Count > 0, 'Unused entry exported from THistogram');
    if (Entry^.Color.Value = SameEntry^.Color.Value) then
    begin
      // Transfer usage count to first entry
      inc(SameEntry^.Count, Entry^.Count);
      Entry^.Count := 0; // Use 0 to signal merged entry
      Entry^.Color.SameAs := SameEntry; // Point to master
    end else
      SameEntry := Entry;
  end;
end;

procedure TColorMapOptimizer.MapColors;
var
  i, j			: integer;
  Delta, BestDelta	: integer;
  BestIndex		: integer;
begin
  (*
  **  Sort by usage count
  *)
  FHistogram.Sort(CompareCount);

  (*
  **  Determine number of colors used (max 256)
  *)
  FFinalCount := FHistogram.Count;
  for i := 0 to FFinalCount-1 do
    if (i >= 256) or (POptimizeEntry(FHistogram[i])^.Count = 0) then
    begin
      FFinalCount := i;
      break;
    end;

  (*
  **  Build color map and reverse map for final entries
  *)
  for i := 0 to FFinalCount-1 do
  begin
    POptimizeEntry(FHistogram[i])^.NewIndex := i;
    FColorMap[i] := POptimizeEntry(FHistogram[i])^.Color.Color;
  end;

  (*
  **  Map colors > 256 to colors <= 256 and build NewIndex color map
  *)
  for i := FFinalCount to FHistogram.Count-1 do
    with POptimizeEntry(FHistogram[i])^ do
    begin
      // Entries with a usage count of -1 is unused
      ASSERT(Count <> -1, 'Internal error: Unused entry exported');
      // Entries with a usage count of 0 has been merged with another entry
      if (Count = 0) then
      begin
        // Use mapping of master entry
        ASSERT(Color.SameAs.NewIndex < 256, 'Internal error: Mapping to unmapped color');
        NewIndex := Color.SameAs.NewIndex;
      end else
      begin
        // Search for entry with nearest color value
        BestIndex := 0;
        BestDelta := 255*3;
        for j := 0 to FFinalCount-1 do
        begin
          Delta := ABS((POptimizeEntry(FHistogram[j])^.Color.Color.Red - Color.Color.Red) +
            (POptimizeEntry(FHistogram[j])^.Color.Color.Green - Color.Color.Green) +
            (POptimizeEntry(FHistogram[j])^.Color.Color.Blue - Color.Color.Blue));
          if (Delta < BestDelta) then
          begin
            BestDelta := Delta;
            BestIndex := j;
          end;
        end;
        NewIndex := POptimizeEntry(FHistogram[BestIndex])^.NewIndex;;
      end;
    end;
end;

procedure TColorMapOptimizer.ReplaceColorMaps;
var
  i			: integer;
begin
  // Zap all local color maps
  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].ColorMap <> nil) then
      FImage.Images[i].ColorMap.Clear;
  // Store optimized global color map
  FImage.GlobalColorMap.ImportColorMap(FColorMap, FFinalCount);
  FImage.GlobalColorMap.Optimized := True;
end;

procedure TColorMapOptimizer.Optimize;
var
  Total			: integer;
  i, j			: integer;
begin
  (*
  **  Process all sub images
  *)
  ProcessImage;

  // Prune histograms and calculate total number of colors
  Total := 0;
  for i := 0 to FHistogramList.Count-1 do
    inc(Total, THistogram(FHistogramList[i]).Prune);

  // Allocate global histogram
  FHistogram.Clear;
  FHistogram.Capacity := Total;

  // Move data pointers from local histograms to global histogram
  for i := 0 to FHistogramList.Count-1 do
    with THistogram(FHistogramList[i]) do
      for j := 0 to Count-1 do
      begin
        ASSERT(POptimizeEntry(List[j])^.Count > 0, 'Unused entry exported from THistogram');
        FHistogram.Add(List[j]);
      end;

  (*
  **  Merge same colors
  *)
  MergeColors;

  (*
  **  Build color map and NewIndex map for final entries
  *)
  MapColors;

  (*
  **  Process images for each color map
  *)
  for i := 0 to FHistogramList.Count-1 do
    THistogram(FHistogramList[i]).MapImages;

  (*
  **  Replace local colormaps with global color map
  *)
  ReplaceColorMaps;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImage
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFImage.Create;
begin
  inherited Create;
  FImages := TGIFImageList.Create(self);
  FHeader := TGIFHeader.Create(self);
  FPainters := TThreadList.Create;
  FGlobalPalette := 0;
  // Load defaults
  FDrawOptions := GIFImageDefaultDrawOptions;
  ColorReduction := GIFImageDefaultColorReduction;
  FReductionBits := GIFImageDefaultColorReductionBits;
  FDitherMode := GIFImageDefaultDitherMode;
  FCompression := GIFImageDefaultCompression;
  FThreadPriority := GIFImageDefaultThreadPriority;
  FAnimationSpeed := GIFImageDefaultAnimationSpeed;

  FDrawBackgroundColor := clNone;
  IsDrawing := False;
  IsInsideGetPalette := False;
  NewImage;
end;

destructor TGIFImage.Destroy;
var
  i			: integer;
begin
  PaintStop;
  with FPainters.LockList do
    try
      for i := Count-1 downto 0 do
        TGIFPainter(Items[i]).FImage := nil;
    finally
      FPainters.UnLockList;
    end;

  Clear;
  FPainters.Free;
  FImages.Free;
  FHeader.Free;
  inherited Destroy;
end;

procedure TGIFImage.Clear;
begin
  PaintStop;
  FreeBitmap;
  FImages.Clear;
  FHeader.ColorMap.Clear;
  FHeader.Height := 0;
  FHeader.Width := 0;
  FHeader.Prepare;
  Palette := 0;
end;

procedure TGIFImage.NewImage;
begin
  Clear;
end;

function TGIFImage.GetVersion: TGIFVersion;
var
  v			: TGIFVersion;
  i			: integer;
begin
  Result := gvUnknown;
  for i := 0 to FImages.Count-1 do
  begin
    v := FImages[i].Version;
    if (v > Result) then
      Result := v;
    if (v >= high(TGIFVersion)) then
      break;
  end;
end;

function TGIFImage.GetColorResolution: integer;
var
  i			: integer;
begin
  Result := FHeader.ColorResolution;
  for i := 0 to FImages.Count-1 do
    if (FImages[i].ColorResolution > Result) then
      Result := FImages[i].ColorResolution;
end;

function TGIFImage.GetBitsPerPixel: integer;
var
  i			: integer;
begin
  Result := FHeader.BitsPerPixel;
  for i := 0 to FImages.Count-1 do
    if (FImages[i].BitsPerPixel > Result) then
      Result := FImages[i].BitsPerPixel;
end;

function TGIFImage.GetBackgroundColorIndex: BYTE;
begin
  Result := FHeader.BackgroundColorIndex;
end;

function TGIFImage.GetBackgroundColor: TColor;
begin
  Result := FHeader.BackgroundColor;
end;

procedure TGIFImage.SetDrawOptions(Value: TGIFDrawOptions);
begin
  if (FDrawOptions = Value) then
    exit;

  if (DrawPainter <> nil) then
    DrawPainter.Stop;

  FDrawOptions := Value;
  // Zap all bitmaps
  Pack;
  Changed(self);
end;

procedure TGIFImage.SetAnimationSpeed(Value: integer);
begin
  if (Value < 0) then
    Value := 0
  else if (Value > 1000) then
    Value := 1000;
  if (Value <> FAnimationSpeed) then
  begin
    FAnimationSpeed := Value;
    // Use the FPainters threadlist to protect FDrawPainter from being modified
    // by the thread while we mess with it
    with FPainters.LockList do
      try
        if (FDrawPainter <> nil) then
          FDrawPainter.AnimationSpeed := FAnimationSpeed;
      finally
        // Release the lock on FPainters to let paint thread kill itself
        FPainters.UnLockList;
      end;
  end;
end;

procedure TGIFImage.SetReductionBits(Value: integer);
begin
  if (Value < 3) or (Value > 8) then
    Error(sInvalidBitSize);
  FReductionBits := Value;
end;

procedure TGIFImage.OptimizeColorMap;
var
  ColorMapOptimizer	: TColorMapOptimizer;
begin
  ColorMapOptimizer := TColorMapOptimizer.Create(self);
  try
    ColorMapOptimizer.Optimize;
  finally
    ColorMapOptimizer.Free;
  end;
end;

procedure TGIFImage.Pack;
var
  i			: integer;
begin
  // Zap bitmaps and palettes
  FreeBitmap;
  Palette := 0;
  for i := 0 to FImages.Count-1 do
  begin
    FImages[i].Bitmap := nil;
    FImages[i].Palette := 0;
  end;

  // Only pack if no global colormap and a single image
  if (FHeader.ColorMap.Count > 0) or (FImages.Count <> 1) then
    exit;

  // Copy local colormap to global
  FHeader.ColorMap.Assign(FImages[0].ColorMap);
  // Zap local colormap
  FImages[0].ColorMap.Clear;
end;

procedure TGIFImage.SaveToStream(Stream: TStream);
var
  n			: Integer;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressSaving);
  try
    // Write header
    FHeader.SaveToStream(Stream);
    // Write images
    FImages.SaveToStream(Stream);
    // Write trailer
    with TGIFTrailer.Create(self) do
      try
        SaveToStream(Stream);
      finally
        Free;
      end;
  finally
{$IFDEF VER93}
    // Special case for C++ Builder 1.x without "ExceptObject"
    n := 100;
{$ELSE}
    if ExceptObject = nil then
      n := 100
    else
      n := 0;
{$ENDIF}
    Progress(Self, psEnding, n, True, Rect(0,0,0,0), sProgressSaving);
  end;
end;

procedure TGIFImage.LoadFromStream(Stream: TStream);
var
  n			: Integer;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressLoading);
  try
    // Zap old image
    Clear;
    // Read header
    FHeader.LoadFromStream(Stream);
    // Read images
    FImages.LoadFromStream(Stream, self);
    // Read trailer
    with TGIFTrailer.Create(self) do
      try
        LoadFromStream(Stream);
      finally
        Free;
      end;
  finally
{$IFDEF VER93}
    // Special case for C++ Builder 1.x without "ExceptObject"
    n := 100;
{$ELSE}
    if ExceptObject = nil then
      n := 100
    else
      n := 0;
{$ENDIF}
    Progress(Self, psEnding, n, True, Rect(0,0,0,0), sProgressLoading);
  end;
end;

function TGIFImage.GetBitmap: TBitmap;
begin
  if not(Empty) then
  begin
    Result := FBitmap;
    if (Result <> nil) then
      exit;
    FBitmap := TBitmap.Create;
    Result := FBitmap;
    FBitmap.OnChange := Changed;
    // Use first image as default
    if (Images.Count > 0) then
    begin
      if (Images[0].Width = Width) and (Images[0].Height = Height) then
      begin
        // Use first image as it has same dimensions
        FBitmap.Assign(Images[0].Bitmap);
      end else
      begin
        // Draw first image on bitmap
        FBitmap.Palette := CopyPalette(Palette);
        FBitmap.Height := Height;
        FBitmap.Width := Width;
        Images[0].Draw(FBitmap.Canvas, FBitmap.Canvas.ClipRect, False, False);
      end;
    end;
  end else
    Result := nil
end;

// Create a new (empty) bitmap
function TGIFImage.NewBitmap: TBitmap;
begin
  Result := FBitmap;
  if (Result <> nil) then
    exit;
  FBitmap := TBitmap.Create;
  Result := FBitmap;
  FBitmap.OnChange := Changed;
  // Draw first image on bitmap
  FBitmap.Palette := CopyPalette(Palette);
  FBitmap.Height := Height;
  FBitmap.Width := Width;
end;

procedure TGIFImage.FreeBitmap;
begin
  if (DrawPainter <> nil) then
    DrawPainter.Stop;

  if (FBitmap <> nil) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

function TGIFImage.Add(Source: TPersistent): integer;
var
  Image			: TGIFSubImage;
  HasChanged		: boolean;
begin
  if not((Source is TBitmap) or (Source is TGIFSubImage)) then
    Error(sUnsupportedClass);
  if (Source is TBitmap) then
  begin
    Image := TGIFSubImage.Create(self);
    Image.Assign(Source);
  end else
    Image := TGIFSubImage(Source);
  Result := FImages.Add(Image);
  HasChanged := False;
  // Set width & height if added image is larger than existing images
{$IFDEF STRICT_MOZILLA}
  // From Mozilla source:
  // Work around broken GIF files where the logical screen
  // size has weird width or height. [...]
  if (FHeader.Width < Image.Width) or (FHeader.Height < Image.Height) then
  begin
    HasChanged := True;
    FHeader.Width := Image.Width;
    FHeader.Height := Image.Height;
    Image.Left := 0;
    Image.Top := 0;
  end;
{$ELSE}
  if (FHeader.Width < Image.Left+Image.Width) then
  begin
    HasChanged := True;
    FHeader.Width := Image.Left+Image.Width;
    Warning(self, gsWarning, sBadWidth)
  end;
  if (FHeader.Height < Image.Top+Image.Height) then
  begin
    HasChanged := True;
    FHeader.Height := Image.Top+Image.Height;
    Warning(self, gsWarning, sBadHeight)
  end;
{$ENDIF}

  if (HasChanged) then
  begin
    Warning(Image, gsWarning, sScreenSizeExceeded);
    FreeBitmap;
    Changed(self);
  end;
end;

function TGIFImage.GetEmpty: Boolean;
begin
  Result := (FImages.Count = 0);
end;

function TGIFImage.GetHeight: Integer;
begin
  Result := FHeader.Height;
end;

function TGIFImage.GetWidth: Integer;
begin
  Result := FHeader.Width;
end;

function TGIFImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := (Graphic = self);
end;

function TGIFImage.GetPalette: HPALETTE;
begin
  // Check for recursion
  // (TGIFImage.GetPalette->TGIFSubImage.GetPalette->TGIFImage.GetPalette etc...)
  if (IsInsideGetPalette) then
    Error(sNoColorTable);
  IsInsideGetPalette := True;
  try
    Result := 0;
    if (FBitmap <> nil) and (FBitmap.Palette <> 0) then
      // Use bitmaps own palette if possible
      Result := FBitmap.Palette
    else if (FGlobalPalette <> 0) then
      // Or a previously exported global palette
      Result := FGlobalPalette
    else if (DoDither) then
    begin
      // or create a new dither palette
      FGlobalPalette := WebPalette;
      Result := FGlobalPalette;
    end else
    if (FHeader.ColorMap.Count > 0) then
    begin
      // or create a new if first time
      FGlobalPalette := FHeader.ColorMap.ExportPalette;
      Result := FGlobalPalette;
    end else
    if (FImages.Count > 0) then
      // This can cause a recursion if no global palette exist and image[0]
      // hasn't got one either. Checked by the IsInsideGetPalette semaphor.
      Result := FImages[0].Palette;
  finally
    IsInsideGetPalette := False;
  end;
end;

procedure TGIFImage.SetPalette(Value: HPalette);
var
  NeedNewBitmap		: boolean;
begin
  if (Value <> FGlobalPalette) then
  begin
    // Zap old palette
    if (FGlobalPalette <> 0) then
      DeleteObject(FGlobalPalette);

    // Zap bitmap unless new palette is same as bitmaps own
    NeedNewBitmap := (FBitmap <> nil) and (Value <> FBitmap.Palette);
    if (NeedNewBitmap) then
      FreeBitmap;

    // Use new palette
    FGlobalPalette := Value;

    if (NeedNewBitmap) then
    begin
      // Need to create new bitmap and repaint
      PaletteModified := True;
      Changed(Self);
    end;
  end;
end;

// Obsolete
// procedure TGIFImage.Changed(Sender: TObject);
// begin
//  inherited Changed(Sender);
// end;

procedure TGIFImage.SetHeight(Value: Integer);
var
  i			: integer;
begin
  for i := 0 to Images.Count-1 do
    if (Images[i].Top + Images[i].Height > Value) then
      Error(sBadHeight);
  Header.Height := Value;
  FreeBitmap;
  Changed(self);
end;

procedure TGIFImage.SetWidth(Value: Integer);
var
  i			: integer;
begin
  for i := 0 to Images.Count-1 do
    if (Images[i].Left + Images[i].Width > Value) then
      Error(sBadWidth);
  Header.Width := Value;
  FreeBitmap;
  Changed(self);
end;

procedure TGIFImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(Bitmap)
  else
    inherited AssignTo(Dest);
end;

procedure TGIFImage.Assign(Source: TPersistent);
var
  i			: integer;
  Image			: TGIFSubImage;
  Bitmap		: TBitmap;

  procedure ImportViaDraw(Bitmap: TBitmap; Graphic: TGraphic);
  begin
    Bitmap.Height := Graphic.Height;
    Bitmap.Width := Graphic.Width;
    // Disable the following line to import in max number of colors with the
    // risk of having to use TCanvas.Pixels to do it (very slow)
    SafeSetPixelFormat(Bitmap, pf8bit);
    Bitmap.Canvas.Draw(0, 0, Graphic);
  end;

begin
  if (Source = self) then
    exit;
  if (Source = nil) then
  begin
    Clear;
  end else
  if (Source is TGIFImage) then
  begin
    Clear;
    // Temporarily copy event handlers to be able to generate progress events
    // during the copy and handle copy errors
    OnProgress := TGIFImage(Source).OnProgress;
    try
      FOnWarning := TGIFImage(Source).OnWarning;
      Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressCopying);
      try
        FHeader.Assign(TGIFImage(Source).Header);
        FThreadPriority := TGIFImage(Source).ThreadPriority;
        FDrawBackgroundColor := TGIFImage(Source).DrawBackgroundColor;
        FDrawOptions := TGIFImage(Source).DrawOptions;
        FColorReduction := TGIFImage(Source).ColorReduction;
        FDitherMode := TGIFImage(Source).DitherMode;

        for i := 0 to TGIFImage(Source).Images.Count-1 do
        begin
          Image := TGIFSubImage.Create(self);
          Image.Assign(TGIFImage(Source).Images[i]);
          Add(Image);
          Progress(Self, psRunning, MulDiv((i+1), 100, TGIFImage(Source).Images.Count),
            False, Rect(0,0,0,0), sProgressCopying);
        end;
      finally
{$IFDEF VER93}
        // Special case for C++ Builder 1.x without "ExceptObject"
        i := 100;
{$ELSE}
        if ExceptObject = nil then
          i := 100
        else
          i := 0;
{$ENDIF}
        Progress(Self, psEnding, i, False, Rect(0,0,0,0), sProgressCopying);
      end;
    finally
      // Reset event handlers
      FOnWarning := nil;
      OnProgress := nil;
    end;
  end else
  if (Source is TBitmap) then
  begin
    Clear;
    Add(Source);
  end else
  if (Source is TGraphic) then
  begin
    Clear;
    Bitmap := TBitmap.Create;
    try
      if (Source is TIcon) or (Source is TMetafile) then
      begin
        try
          // Make things a little easier for TGIFSubImage.Assign by converting
          // pfDevice to a more import friendly format
          if (Source is TIcon) then
            ImportViaDraw(Bitmap, TGraphic(Source))
          else
            ImportViaDraw(Bitmap, TGraphic(Source));
        except
          // If import via TCanvas.Draw fails (which it shouldn't), we try the
          // Assign mechanism instead
          Bitmap.Assign(Source);
        end;
      end else
        try
          Bitmap.Assign(Source);
        except
          // If automatic conversion to bitmap fails, we try and draw the
          // graphic on the bitmap instead
          ImportViaDraw(Bitmap, TGraphic(Source));
        end;
      // Convert the bitmap to a GIF frame
      Add(Bitmap);
    finally
      Bitmap.Free;
    end;
  end else
    inherited Assign(Source);
end;

procedure TGIFImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
{$IFDEF REGISTER_TGIFIMAGE}
var
  Data			: THandle;
  Size			: Longint;
  Buffer		: Pointer;
  Stream		: TMemoryStream;
  Bmp			: TBitmap;
begin
  Data := GetClipboardData(CF_GIF);
  if (Data <> 0) then
  begin
    // Get size and pointer to data
    Size := GlobalSize(Data);
    Buffer := GlobalLock(Data);
    try
      Stream := TMemoryStream.Create;
      try
        // Copy data to a stream
        Stream.SetSize(Size);
        Move(Buffer^, Stream.Memory^, Size);
        // Load GIF from stream
        LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    finally
      GlobalUnlock(Data);
    end;
  end else
  begin
    // No GIF on clipboard - try loading a bitmap instead
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
      Assign(Bmp);
    finally
      Bmp.Free;
    end;
  end;
end;
{$ELSE}
begin
  Error(sGIFToClipboard);
end;
{$ENDIF}

procedure TGIFImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
{$IFDEF REGISTER_TGIFIMAGE}
var
  Stream		: TMemoryStream;
  Data			: THandle;
  Buffer		: Pointer;
begin
  if (Empty) then
    exit;
  // First store a bitmap version on the clipboard...
  Bitmap.SaveToClipboardFormat(AFormat, AData, APalette);
  // ...then store a GIF
  Stream := TMemoryStream.Create;
  try
    // Save the GIF to a memory stream
    SaveToStream(Stream);
    Stream.Position := 0;
    // Allocate some memory for the GIF data
    Data := GlobalAlloc(HeapAllocFlags, Stream.Size);
    try
      if (Data <> 0) then
      begin
        Buffer := GlobalLock(Data);
        try
          // Copy the GIF data to the memory
          Move(Stream.Memory^, Buffer^, Stream.Size);
          // Put it all on the clipboard
          SetClipboardData(CF_GIF, Data);
        finally
          GlobalUnlock(Data);
        end;
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Stream.Free;
  end;
end;
{$ELSE}
begin
  Error(sGIFToClipboard);
end;
{$ENDIF}

function TGIFImage.GetColorMap: TGIFColorMap;
begin
  Result := FHeader.ColorMap;
end;

function TGIFImage.GetDoDither: boolean;
begin
  Result := (goDither in DrawOptions) and
    (((goAutoDither in DrawOptions) and DoAutoDither) or
      not(goAutoDither in DrawOptions));
end;

{$IFDEF VER9x}
procedure TGIFImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;
{$ENDIF}

procedure TGIFImage.StopDraw;
var
  Msg			: TMsg;
begin
  repeat
    // Use the FPainters threadlist to protect FDrawPainter from being modified
    // by the thread while we mess with it
    with FPainters.LockList do
      try
        if (FDrawPainter = nil) then
          break;

        // Tell thread to terminate
        FDrawPainter.Stop;

        // No need to wait for "thread" to terminate if running in main thread
        if not(goAsync in FDrawPainter.DrawOptions) then
          break;

      finally
        // Release the lock on FPainters to let paint thread kill itself
        FPainters.UnLockList;
      end;

    // Process Messages to make Synchronize work
    // (Instead of Application.ProcessMessages)
    while PeekMessage(Msg, 0, CM_DESTROYWINDOW, CM_EXECPROC, PM_REMOVE) do
    begin
      if (Msg.Message <> WM_QUIT) then
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end else
        break;
    end;
    Sleep(0); // Yield

  until (False);
  FreeBitmap;
end;

procedure TGIFImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Canvas		: TCanvas;
  Msg			: TMsg;
  DestRect		: TRect;

  procedure DrawTile(Rect: TRect; Bitmap: TBitmap);
  var
    Tile		: TRect;
  begin
    if (goTile in FDrawOptions) then
    begin
      Tile.Left := Rect.Left;
      Tile.Right := Tile.Left + Width;
      while (Tile.Left < Rect.Right) do
      begin
        Tile.Top := Rect.Top;
        Tile.Bottom := Tile.Top + Height;
        while (Tile.Top < Rect.Bottom) do
        begin
          ACanvas.StretchDraw(Tile, Bitmap);
          Tile.Top := Tile.Top + Height;
          Tile.Bottom := Tile.Top + Height;
        end;
        Tile.Left := Tile.Left + Width;
        Tile.Right := Tile.Left + Width;
      end;
    end else
      ACanvas.StretchDraw(Rect, Bitmap);
  end;

begin
  // Prevent recursion(s(s(s)))
  if (IsDrawing) or (FImages.Count = 0) then
    exit;

  IsDrawing := True;
  try
    // Copy bitmap to canvas if we are already drawing
    // (or have drawn but are finished)
    if (FImages.Count = 1) or // Only one image
      (not (goAnimate in FDrawOptions)) then // Don't animate
    begin
      FImages[0].Draw(ACanvas, Rect, (goTransparent in FDrawOptions),
        (goTile in FDrawOptions));
      exit;
    end else
    if (FBitmap <> nil) and not(goDirectDraw in FDrawOptions) then
    begin
      DrawTile(Rect, Bitmap);
      exit;
    end;

    // Use the FPainters threadlist to protect FDrawPainter from being modified
    // by the thread while we mess with it
    with FPainters.LockList do
      try
      // If we are already painting on the canvas in goDirectDraw mode
      // and at the same location, just exit and let the painter do
      // its thing when it's ready
      if (FDrawPainter <> nil) and (FDrawPainter.Canvas = ACanvas) and
        EqualRect(FDrawPainter.Rect, Rect) then
        exit;

      // Kill the current paint thread
      StopDraw;
      // Give thread a chance to die
      // Note: This is done to circumvent a thread race condition bug in
      // Delphi 4.x:
      // If the paint thread is the only thread (besides the main thread),
      // it MUST be allowed to terminate completely (and destroy the thread
      // window) before the new thread is created. Otherwise a situation
      // might arise where the thread window is deleted *after* the new
      // thread has been created.
{$IFDEF VER120}
      Sleep(0);
      while (PeekMessage(Msg, 0, CM_DESTROYWINDOW, CM_EXECPROC, PM_REMOVE)) do
        if (Msg.Message <> WM_QUIT) then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end else
          exit;
{$ENDIF}

      if not(goDirectDraw in FDrawOptions) then
      begin
        // Create a bitmap to draw on
        NewBitmap;
        Canvas := FBitmap.Canvas;
        DestRect := Canvas.ClipRect;
        // Initialize bitmap canvas with background image
        Canvas.CopyRect(DestRect, ACanvas, Rect);
      end else
      begin
        Canvas := ACanvas;
        DestRect := Rect;
      end;

      // Create new paint thread
      InternalPaint(@FDrawPainter, Canvas, DestRect, FDrawOptions);

      if (FDrawPainter <> nil) then
      begin
        // Launch thread
        FDrawPainter.Start;

        if not(goDirectDraw in FDrawOptions) then
        begin
          // Wait for thread to render first frame
          while (FDrawPainter <> nil) and (not FDrawPainter.Terminated) and
            (not FDrawPainter.Started) do
            // Process Messages to make Synchronize work
            // (Instead of Application.ProcessMessages)
            if PeekMessage(Msg, 0, CM_DESTROYWINDOW, CM_EXECPROC, PM_REMOVE) then
            begin
              if (Msg.Message <> WM_QUIT) then
              begin
                TranslateMessage(Msg);
                DispatchMessage(Msg);
              end else
                exit;
            end else
              Sleep(0); // Yield
          // Draw frame to destination
          DrawTile(Rect, Bitmap);
        end;
      end;
    finally
      FPainters.UnLockList;
    end;

  finally
    IsDrawing := False;
  end;
end;

// Internal pain(t) routine used by Draw()
function TGIFImage.InternalPaint(Painter: PGifPainter; ACanvas: TCanvas;
  const Rect: TRect; Options: TGIFDrawOptions): TGIFPainter;
begin
  if (Empty) or (Rect.Left >= Rect.Right) or (Rect.Top >= Rect.Bottom) then
  begin
    Result := nil;
    if (Painter <> nil) then
      Painter^ := Result;
    exit;
  end;

  // Draw in main thread if only one image
  if (Images.Count = 1) then
    Options := Options - [goAsync];

  Result := TGIFPainter.CreateRef(Painter, self, ACanvas, Rect, Options);
  FPainters.Add(Result);
  Result.OnStartPaint := FOnStartPaint;
  Result.OnPaint := FOnPaint;
  Result.OnLoop := FOnLoop;
  Result.OnEndPaint := FOnEndPaint;

  if not(goAsync in Options) then
  begin
    // Run in main thread
    Result.Execute;
    // Note: Painter threads executing in the main thread are freed upon exit
    // from the Execute method, so no need to do it here.
    Result := nil;
    Painter^ := Result;
  end else
    Result.Priority := FThreadPriority;
end;

function TGIFImage.Paint(ACanvas: TCanvas; const Rect: TRect;
  Options: TGIFDrawOptions): TGIFPainter;
begin
  Result := InternalPaint(nil, ACanvas, Rect, Options);
  if (Result <> nil) then
    // Run in separate thread
    Result.Start;
end;

procedure TGIFImage.PaintStart;
var
  i			: integer;
begin
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Start;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.PaintStop;
var
  Ghosts		: integer;
  i			: integer;
  Msg			: TMsg;
begin
  try
    // Loop until all have died
    repeat
      with FPainters.LockList do
        try
          if (Count = 0) then
            exit;

          // Signal painters to terminate
          // Painters will attempt to remove them self from the
          // painter list when they die
          Ghosts := Count;
          for i := Ghosts-1 downto 0 do
          begin
            TGIFPainter(Items[i]).Stop;
            if not(goAsync in TGIFPainter(Items[i]).DrawOptions) then
              dec(Ghosts);
          end;
        finally
          FPainters.UnLockList;
        end;

      // If all painters were synchronous, there's no purpose waiting for them
      // to terminate, because they are running in the main thread.
      if (Ghosts = 0) then
        exit;

      // Process Messages to make TThread.Synchronize work
      // (Instead of Application.ProcessMessages)
      while PeekMessage(Msg, 0, CM_DESTROYWINDOW, CM_EXECPROC, PM_REMOVE) do
      begin
        if (Msg.Message <> WM_QUIT) then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end else
          exit;
      end;
    until (False);
  finally
    FreeBitmap;
  end;
end;

procedure TGIFImage.PaintPause;
var
  i			: integer;
begin
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Suspend;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.PaintResume;
var
  i			: integer;
begin
  // Implementation is currently same as PaintStart, but don't call PaintStart
  // in case its implementation changes
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Start;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.PaintRestart;
var
  i			: integer;
begin
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Restart;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.Warning(Sender: TObject; Severity: TGIFSeverity; Message: string);
begin
  if (Assigned(FOnWarning)) then
    FOnWarning(Sender, Severity, Message);
end;

var
  DesktopDC: HDC;

////////////////////////////////////////////////////////////////////////////////
//
//			Initialization
//
////////////////////////////////////////////////////////////////////////////////
initialization
{$IFDEF REGISTER_TGIFIMAGE}
  TPicture.RegisterFileFormat('GIF', sGIFImageFile, TGIFImage);
  CF_GIF := RegisterClipboardFormat(PChar(sGIFImageFile));
  TPicture.RegisterClipboardFormat(CF_GIF, TGIFImage);
{$ENDIF}
  DesktopDC := GetDC(0);
  try
    PaletteDevice := (GetDeviceCaps(DesktopDC, BITSPIXEL) * GetDeviceCaps(DesktopDC, PLANES) <= 8);
    DoAutoDither := PaletteDevice;
  finally
    ReleaseDC(0, DesktopDC);
  end;

  // Use a single render work thread on Windows 95/98 since it
  // cannot schedule threads properly. More than a single thread
  // will cause congestion.
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    MaxRenderThreads := 1;

{$IFDEF VER9x}
  // Note: This doesn't return the same palette as the Delphi 3 system palette
  // since the true system palette contains 20 entries and the Delphi 3 system
  // palette only contains 16.
  // For our purpose this doesn't matter since we do not care about the actual
  // colors (or their number) in the palette.
  // Stock objects doesn't have to be deleted.
  SystemPalette16 := GetStockObject(DEFAULT_PALETTE);
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Finalization
//
////////////////////////////////////////////////////////////////////////////////
finalization
  ExtensionList.Free;
  AppExtensionList.Free;
  if (Renderer <> nil) then
    Renderer.Free;
{$IFNDEF VER9x}
  {$IFDEF REGISTER_TGIFIMAGE}
    TPicture.UnregisterGraphicClass(TGIFImage);
  {$ENDIF}
  if (pf8BitBitmap <> nil) then
    pf8BitBitmap.Free;
{$ENDIF}
end.





