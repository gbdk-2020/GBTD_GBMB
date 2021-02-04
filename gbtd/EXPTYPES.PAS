unit exptypes;

interface

uses Windows, Messages, SysUtils, classes, gbconst, gbofile, ExpBase, GBTDMain;

type

  TGBTDISASIncExport = class(TGBTDExport)
  private
    { Private declarations }
    F : TextFile;
    FName : string;

  protected
    { protected declarations }
    procedure WriteHeader(const Info : TGBOTileExport; x,y : integer ); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean ); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteEnd(const Info : TGBOTileExport ); override;
    procedure WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string ); override;

  public
    { Public declarations }
    constructor Create(const fn : string ); override;
    destructor Destroy; override;
  end;



  TGBTDISASAsmExport = class(TGBTDExport)
  private
    { Private declarations }
    F : TextFile;
    FName : string;

  protected
    { protected declarations }
    procedure WriteHeader(const Info : TGBOTileExport; x,y : integer ); override;
    procedure WriteInit(const Info : TGBOTileExport ); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean ); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer); override;
    procedure WriteEnd(const Info : TGBOTileExport ); override;

  public
    { Public declarations }
    constructor Create(const fn : string); override;
    destructor Destroy; override;
  end;


  TGBTDRGBDSAsmExport = class(TGBTDExport)
  private
    { Private declarations }
    F : TextFile;
    FName : string;

  protected
    { protected declarations }
    procedure WriteHeader(const Info : TGBOTileExport; x,y : integer ); override;
    procedure WriteInit(const Info : TGBOTileExport ); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean ); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer); override;
    procedure WriteEnd(const Info : TGBOTileExport ); override;

  public
    { Public declarations }
    constructor Create(const fn : string ); override;
    destructor Destroy; override;
  end;


  TGBTDRGBDSIncExport = class(TGBTDExport)
  private
    { Private declarations }
    F : TextFile;
    FName : string;

  protected
    { protected declarations }
    procedure WriteHeader(const Info : TGBOTileExport; x,y : integer ); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean ); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteEnd(const Info : TGBOTileExport ); override;
    procedure WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string); override;

  public
    { Public declarations }
    constructor Create(const fn : string ); override;
    destructor Destroy; override;
  end;


  TGBTDRGBDSObjExport = class(TGBTDExport)
  private
    { Private declarations }
    FStrm : TFileStream;
    Lst : TList;
    DataBuf : TPByteArray;
    Pos : integer;
    Section : string;
    Bank : integer;

  protected
    { protected declarations }
    procedure WriteInit(const Info : TGBOTileExport); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string); override;
    procedure WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer); override;
    procedure WriteEnd(const Info : TGBOTileExport); override;

  public
    { Public declarations }
    constructor Create(const fn : string ); override;
    destructor Destroy; override;
  end;


  TGBTDTASMAsmExport = class(TGBTDExport)
  private
    { Private declarations }
    F : TextFile;
    FName : string;

  protected
    { protected declarations }
    procedure WriteHeader(const Info : TGBOTileExport; x,y : integer ); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean ); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer); override;
    procedure WriteEnd(const Info : TGBOTileExport ); override;
    procedure WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string ); override;

  public
    { Public declarations }
    constructor Create(const fn : string ); override;
    destructor Destroy; override;
  end;



  TGBTDBinExport = class(TGBTDExport)
  private
    { Private declarations }
    FStrm : TFileStream;

  protected
    { protected declarations }
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer); override;

  public
    { Public declarations }
    constructor Create(const fn : string); override;
    destructor Destroy; override;
  end;


  TGBTDGBDKCExport = class(TGBTDExport)
  private
    { Private declarations }
    F : TextFile;
    FName : string;
  protected
    { protected declarations }
    procedure WriteHeader(const Info : TGBOTileExport; x,y : integer ); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean ); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer); override;
    procedure WriteEnd(const Info : TGBOTileExport ); override;

  public
    { Public declarations }
    constructor Create(const fn : string ); override;
    destructor Destroy; override;
  end;


  TGBTDGBDKHExport = class(TGBTDExport)
  private
    { Private declarations }
    F : TextFile;
    FName : string;
    IncludeName : string;
  protected
    { protected declarations }
    procedure WriteHeader(const Info : TGBOTileExport; x,y : integer ); override;
    procedure WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean ); override;
    procedure WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string ); override;
    procedure WriteEnd(const Info : TGBOTileExport ); override;
    procedure WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string ); override;

  public
    { Public declarations }
    constructor Create(const fn : string ); override;
    destructor Destroy; override;
  end;



implementation


procedure WriteAsmHeader(const F : TextFile; const Info : TGBOTileExport; x,y : integer; RGBDS : boolean );
begin
  writeln(F, '; Info:' );

  with Info do
  begin
    if RGBDS then
    begin
      writeln(F, ';   Section              : ' + SectionName );
      writeln(F, ';   Bank                 : ' + IntToStr(Bank) );
    end;
    writeln(F, ';   Form                 : ' + TileArrayDesc(TileArray) );
    writeln(F, ';   Format               : ' + FormatDesc(Format) );
    writeln(F, ';   Compression          : ' + CompressionDesc(Compression) );
    writeln(F, ';   Counter              : ' + CounterDesc(Counter) );
    writeln(F, ';   Tile size            : ' + IntToStr(x) + ' x ' + IntToStr(y) );
    writeln(F, ';   Tiles                : ' + IntToStr(From) + ' to ' + IntToStr(UpTo) );
    writeln(F, ';');
    writeln(F, ';   Palette colors       : ' + PalColDesc(IncludeColors) );
    writeln(F, ';   SGB Palette          : ' + PalFormatDesc(SGBPalettes) );
    writeln(F, ';   CGB Palette          : ' + PalFormatDesc(CGBPalettes) );
    writeln(F, ';');
    writeln(F, ';   Convert to metatiles : ' + MakeMetaTileDesc(MakeMetaTiles) );
    if MakeMetaTiles then
    begin
      writeln(F, ';   Index offset         : ' + IntToStr(MetaOffset) );
//      writeln(F, ';   Index format         : ' + MetaFormatDesc(MetaFormat) );
//      writeln(F, ';   Index layout         : ' + MetaLayoutDesc(MetaLayout) );
      writeln(F, ';   Index counter        : ' + CounterDesc(MetaCounter) );
    end;
    writeln(F, ';');
    writeln(F, '; This file was generated by GBTD v' + GBTDVersion );
    writeln(F);
  end;

end;

procedure WriteGBDKHeader(const F : TextFile; const Info : TGBOTileExport; x,y : integer );
begin
  writeln(F, ' Info:' );

  with Info do
  begin
    writeln(F, '  Form                 : ' + TileArrayDesc(TileArray) );
    writeln(F, '  Format               : ' + FormatDesc(Format) );
    writeln(F, '  Compression          : ' + CompressionDesc(Compression) );
    writeln(F, '  Counter              : ' + CounterDesc(Counter) );
    writeln(F, '  Tile size            : ' + IntToStr(x) + ' x ' + IntToStr(y) );
    writeln(F, '  Tiles                : ' + IntToStr(From) + ' to ' + IntToStr(UpTo) );
    writeln(F);
    writeln(F, '  Palette colors       : ' + PalColDesc(IncludeColors) );
    writeln(F, '  SGB Palette          : ' + PalFormatDesc(SGBPalettes) );
    writeln(F, '  CGB Palette          : ' + PalFormatDesc(CGBPalettes) );
    writeln(F);
    writeln(F, '  Convert to metatiles : ' + MakeMetaTileDesc(MakeMetaTiles) );
    if MakeMetaTiles then
    begin
      writeln(F, '  Index offset         : ' + IntToStr(MetaOffset) );
//      writeln(F, '  Index format         : ' + MetaFormatDesc(MetaFormat) );
//      writeln(F, '  Index layout         : ' + MetaLayoutDesc(MetaLayout) );
      writeln(F, '  Index counter        : ' + CounterDesc(MetaCounter) );
    end;
    writeln(F);
    writeln(F, ' This file was generated by GBTD v' + GBTDVersion );
    writeln(F);
    writeln(F, '*/');
    writeln(F);
  end;

end;



(*************************************************)
(*                                               *)
(*                   BIN output                  *)
(*                                               *)
(*************************************************)


constructor TGBTDBinExport.Create(const fn : string );
begin
  FStrm := TFileStream.Create(fn, fmCreate);
end;


destructor TGBTDBinExport.Destroy;
begin
  FStrm.Free;

  inherited;
end;


procedure TGBTDBinExport.WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer);
begin
  FStrm.Write(buf^, size);
end;


procedure TGBTDBinExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  case CType of
    gbctByte  : FStrm.Write( cnt, 1 );
    gbctWord  : FStrm.Write( cnt, 2 );
  end;
end;





(*************************************************)
(*                                               *)
(*              RGBDS OBJ output                 *)
(*                                               *)
(*************************************************)

type TGBTDRGBDSLabelType = record
  Name : ShortString;
  Location : integer;
end;



constructor TGBTDRGBDSObjExport.Create(const fn : string );
begin
  FStrm := TFileStream.Create(fn, fmCreate);
  Lst := TList.Create;
  GetMem( DataBuf, (20 * MAXTILECNT) + (2 * MAXTILECNT) );
  Pos := 0;
end;


destructor TGBTDRGBDSObjExport.Destroy;
var p : ^TGBTDRGBDSLabelType;
    i : integer;
begin
  FreeMem(DataBuf );

  (* remove labels *)
  for i :=0 to (Lst.Count-1) do
  begin
    p := Lst.Items[0];
    Lst.Delete(0);
    FreeMem(p, SizeOf( TGBTDRGBDSLabelType));
  end;
  Lst.Free;

  FStrm.Free;

  inherited;
end;


procedure TGBTDRGBDSObjExport.WriteInit( const Info : TGBOTileExport );
begin
  (* save data for later *)
  Section := Info.SectionName;
  Bank := Info.Bank;
end;


procedure TGBTDRGBDSObjExport.WriteEnd( const Info : TGBOTileExport );
var i,j : integer;
var p : ^TGBTDRGBDSLabelType;
var s : array[0..128] of char;
var l : LongInt;
begin

  (* write header *)
  FStrm.Write('RGB1', 4);

  FStrm.Write(Lst.Count, 4);                  (* Label-count *)
  l := 1;
  FStrm.Write(l, 4);                          (* Section-count *)

  (* labels *)
  j := Lst.Count-1;
  for i := 0 to j do
  begin
    p := Lst.Items[i];
    StrPcopy(s, p^.Name);
    FStrm.Write(s, StrLen(s)+1);              (* labelname *)
    l := 2;
    FStrm.Write(l, 1);                        (* EXPORT *)
    l := 0;
    FStrm.Write(l, 4);                        (* Section *)
    FStrm.Write(p^.Location, 4);              (* Location *)
  end;

  (* Section *)
  FStrm.Write(Pos, 4);                        (* Section size *)
  if (Bank = 0) then
    l := 3
  else
    l := 2;
  FStrm.Write(l, 1);                          (* HOME/CODE *)
  l := -1;
  FStrm.Write(l, 4);                          (* Org *)
  if (bank = 0) then
    l := -1
  else
    l := bank;
  FStrm.Write(l,4);                           (* Bank *)
  

  (* Export data *)
  FStrm.Write(DataBuf^, Pos);

  (* EOF ? *)
  l := 0;
  FStrm.Write(l, 4);

end;



procedure TGBTDRGBDSObjExport.WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer);
var i : integer;
begin
  (* pump over to internal buffer *)
  for i := 0 to (size-1) do
    DataBuf[Pos + i] := buf[i];

  (* save for later *)
  Pos := Pos + size;
end;


procedure TGBTDRGBDSObjExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  case CType of
    gbctByte  :
      begin
        DataBuf[pos] := cnt;
        Inc(pos);
      end;

    gbctWord  :
      begin
        DataBuf[pos] := cnt;
        DataBuf[pos+1] := (cnt div 256);
        Inc(pos, sizeof(SmallInt));
      end;
  end;
end;


procedure TGBTDRGBDSObjExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
var p : ^TGBTDRGBDSLabelType;
begin

  (* create new label *)
  GetMem(p, SizeOf( TGBTDRGBDSLabelType));
  Lst.Add(p);

  (* fill it *)
  p^.Name := lbl;
  p^.Location := pos;

end;




(*************************************************)
(*                                               *)
(*              RGBDS ASM output                 *)
(*                                               *)
(*************************************************)

constructor TGBTDRGBDSAsmExport.Create(const fn : string );
begin
  AssignFile(F, fn);
  Rewrite(F);
  FName := fn;
end;


destructor TGBTDRGBDSAsmExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TGBTDRGBDSAsmExport.WriteHeader( const Info : TGBOTileExport; x,y : integer );
begin
  (* write specific header for ASM-files *)
  writeln(F, '; ' + UpperCase(ExtractFileName(FName)));
  writeln(F, ';');
  writeln(F, '; Tile Source File.');
  writeln(F, ';');

  (* write generic header *)
  WriteAsmHeader(F, Info, x, y, True);
end;


procedure TGBTDRGBDSAsmExport.WriteInit( const Info : TGBOTileExport );
begin
  with Info do
  begin
    (* insert section *)
    write(F, ' SECTION "' + Trim(SectionName) + '", ' );
    if (Bank = 0) then
      writeln(F, 'ROM0')
    else
      writeln(F, 'ROMX, BANK[' + IntToStr(Bank) +']' );
  end;
end;


procedure TGBTDRGBDSAsmExport.WriteEnd(const Info : TGBOTileExport );
begin
  (* EOF marker *)
  writeln(F);
  writeln(F, '; End of ' + UpperCase(ExtractFileName(FName)));
end;


procedure TGBTDRGBDSAsmExport.WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer);
var i,j : integer;
begin
  i := 0;
  while (i < size) do
  begin
    write(F, 'DB ');
    for j := 0 to 7 do
      if ((i+j) < size) then
      begin
        write(F, '$' + IntToHex(buf[i+j], 2) );
        if (j < 7) and ((i+j) < (size-1)) then write(F, ',');
      end;
    writeln(F);
    i := i + j;
  end;
end;


procedure TGBTDRGBDSAsmExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  case CType of
    gbctByte  : writeln(F, 'DB ' + IntToStr(cnt));
    gbctWord  : writeln(F, 'DW ' + IntToStr(cnt));
  end;
end;


procedure TGBTDRGBDSAsmExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F, lbl + '::');
end;





(*************************************************)
(*                                               *)
(*              RGBDS INC output                 *)
(*                                               *)
(*************************************************)

constructor TGBTDRGBDSIncExport.Create(const fn : string );
begin
  AssignFile(F, fn);
  Rewrite(F);
  FName := fn;
end;


destructor TGBTDRGBDSIncExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;



procedure TGBTDRGBDSIncExport.WriteHeader( const Info : TGBOTileExport; x,y : integer );
begin
  (* write specific header for ASM-files *)
  writeln(F, '; ' + UpperCase(ExtractFileName(FName)));
  writeln(F, ';');
  writeln(F, '; Include File.');
  writeln(F, ';');

  (* write generic header *)
  WriteAsmHeader(F, Info, x, y, True);
end;


procedure TGBTDRGBDSIncExport.WriteEnd( const Info : TGBOTileExport );
begin
  (* EOF marker *)
  writeln(F);
  writeln(F, '; End of ' + UpperCase(ExtractFileName(FName)));
end;


procedure TGBTDRGBDSIncExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  if (CType = gbctEqu) then writeln(F, lbl + 'Len EQU ' + IntToStr(cnt));
end;


procedure TGBTDRGBDSIncExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F, ' GLOBAL ' + lbl );
end;


procedure TGBTDRGBDSIncExport.WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F, lbl + ' EQU ' + Value );
end;




(*************************************************)
(*                                               *)
(*                GBDK C output                  *)
(*                                               *)
(*************************************************)

constructor TGBTDGBDKCExport.Create(const fn : string );
begin
  AssignFile(F, fn);
  Rewrite(F);
  FName := fn;
end;


destructor TGBTDGBDKCExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;

procedure TGBTDGBDKCExport.WriteHeader( const Info : TGBOTileExport; x,y : integer );
begin
  (* write specific header for C-files *)
  if (Info.Bank <> 0) then begin
    writeln(F, '#pragma bank ' + IntToStr(Info.Bank) );
    writeln(F);
  end;

  writeln(F, '/*');
  writeln(F);
  writeln(F, ' ' + UpperCase(ExtractFileName(FName)));
  writeln(F);
  writeln(F, ' Tile Source File.');
  writeln(F);

  (* write generic header *)
  WriteGBDKHeader(F, Info, x, y);
end;


procedure TGBTDGBDKCExport.WriteEnd( const Info : TGBOTileExport );
begin
  (* EOF marker *)
  writeln(F);
  writeln(F, '/* End of ' + UpperCase(ExtractFileName(FName)) + ' */');
end;


procedure TGBTDGBDKCExport.WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer);
var i,j : integer;
begin
  i := 0;
  while (i < size) do
  begin
    write(F, '  ');
    for j := 0 to 7 do
      if ((i+j) < size) then
      begin
        write(F, '0x' + IntToHex(buf[i+j], 2) );
        if ((i+j) < (size-1)) then write(F, ',');
      end;
    writeln(F);
    i := i + j;
  end;
  writeln(F, '};' );
end;


procedure TGBTDGBDKCExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  case CType of
    gbctByte  : writeln(F, '  ' + IntToStr(cnt) + ',');
    gbctWord  : writeln(F, '  0x' + IntToHex(cnt mod $100,2) + ',0x' + IntToHex( cnt div $100,2) + ',' );
  end;
end;


procedure TGBTDGBDKCExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
begin
  if not MainLabel then                (* C does not support main labels *)
  begin
    if (comment <> EmptyStr) then
    begin
      writeln(F, '/* ' + comment + ' */');
    end;
    if (Info.Bank <> 0) then writeln(F, format('const void __at(%d) __bank_%s;', [Info.Bank, lbl]));
    writeln(F, 'const unsigned char ' + lbl + '[] =');
    writeln(F, '{' );
  end;
end;





(*************************************************)
(*                                               *)
(*                GBDK H output                  *)
(*                                               *)
(*************************************************)

constructor TGBTDGBDKHExport.Create(const fn : string );
begin
  AssignFile(F, fn);
  Rewrite(F);
  FName := fn;
end;


destructor TGBTDGBDKHExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TGBTDGBDKHExport.WriteHeader( const Info : TGBOTileExport; x,y : integer );
  function replacechr(const src: string): string;
  var i : integer;
  begin
    result:= src;
    for i:= 1 to length(result) do
      if not (result[i] in ['0'..'9', 'A'..'Z', 'a'..'z']) then result[i]:= '_';
  end;
begin
  (* write specific header for H-files *)
  writeln(F, '/*');
  writeln(F);
  writeln(F, ' ' + UpperCase(ExtractFileName(FName)));
  writeln(F);
  writeln(F, ' Include File.');
  writeln(F);

  (* write generic header *)
  WriteGBDKHeader(F, Info, x, y);

  IncludeName:= format('__%s_INCLUDE', [replacechr(ExtractFileName(FName))]);
  writeln(F, '#ifndef ' + IncludeName);
  writeln(F, '#define ' + IncludeName);
end;


procedure TGBTDGBDKHExport.WriteEnd( const Info : TGBOTileExport );
begin
  (* EOF marker *)
  writeln(F);
  writeln(F, '#endif');
  writeln(F);
  writeln(F, '/* End of ' + UpperCase(ExtractFileName(FName)) + ' */');
end;


procedure TGBTDGBDKHExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  if (CType = gbctEQU) then writeln(F, '#define ' + lbl + 'Len ' + IntToStr(cnt) );
end;


procedure TGBTDGBDKHExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
begin
  if not MainLabel then                (* C does not support main labels *)
  begin
    if (comment <> EmptyStr) then
    begin
      writeln(F, '/* ' + comment + ' */');
    end;

    if (Info.Bank <> 0) then writeln(F, 'extern const void __bank_' + lbl + ';');
    writeln(F, 'extern const unsigned char ' + lbl + '[];');
  end;
end;

procedure TGBTDGBDKHExport.WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '/* ' + comment + ' */' );
  end;
  writeln(F, '#define ' + lbl + ' ' + Value );
end;





(*************************************************)
(*                                               *)
(*               TASM ASM output                 *)
(*                                               *)
(*************************************************)

constructor TGBTDTASMAsmExport.Create(const fn : string );
begin
  AssignFile(F, fn);
  Rewrite(F);
  FName := fn;
end;


destructor TGBTDTASMAsmExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TGBTDTASMAsmExport.WriteHeader( const Info : TGBOTileExport; x,y : integer );
begin
  (* write specific header for ASM-files *)
  writeln(F, '; ' + UpperCase(ExtractFileName(FName)));
  writeln(F, ';');
  writeln(F, '; Tile Source File.');
  writeln(F, ';');

  (* write generic header *)
  WriteAsmHeader(F, Info, x, y, False);
end;




procedure TGBTDTASMAsmExport.WriteEnd( const Info : TGBOTileExport );
begin
  (* EOF marker *)
  writeln(F);
  writeln(F, '.end' );
  writeln(F);
  writeln(F, '; End of ' + UpperCase(ExtractFileName(FName)));
end;


procedure TGBTDTASMAsmExport.WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer);
var i,j : integer;
begin
  i := 0;
  while (i < size) do
  begin
    write(F, '.byte ');
    for j := 0 to 7 do
      if ((i+j) < size) then
      begin
        write(F, '$' + IntToHex(buf[i+j], 2) );
        if (j < 7) and ((i+j) < (size-1)) then write(F, ',');
      end;
    writeln(F);
    i := i + j;
  end;
end;


procedure TGBTDTASMAsmExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  case CType of
    gbctByte  : writeln(F, '.Byte ' + IntToStr(cnt));
    gbctWord  : writeln(F, '.Word ' + IntToStr(cnt));
    gbctEqu   : writeln(F, lbl + 'Len .equ ' + IntToStr(cnt));
  end;
end;


procedure TGBTDTASMAsmExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F, lbl + ':');
end;

procedure TGBTDTASMAsmExport.WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F, lbl + ' .equ ' + Value );
end;





(*************************************************)
(*                                               *)
(*               ISAS ASM output                 *)
(*                                               *)
(*************************************************)


{
Change [] to ( )
Change ldh to ld
Change include "xxxx.xxx" to <tab> file xxxx.xxx
Change variables created as MyVar db/dw/etc. to MyVar ds <n bytes>
Change hl+ to hli
Change <macroname>: to <macroname>
Change DB $xxxx to <tab> DB $xxxx
Assembler directives generally are tabbed in
No spaces after commas in operands
Macro parameters specified as <macroname> MACRO <parms>
No string operations
}


constructor TGBTDISASAsmExport.Create(const fn : string );
begin
  AssignFile(F, fn);
  Rewrite(F);
  FName := fn;
end;


destructor TGBTDISASAsmExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;


procedure TGBTDISASAsmExport.WriteHeader( const Info : TGBOTileExport; x,y : integer );
begin
  (* write specific header for ASM-files *)
  writeln(F, '; ' + UpperCase(ExtractFileName(FName)));
  writeln(F, ';');
  writeln(F, '; Tile Source File.');
  writeln(F, ';');

  (* write generic header *)
  WriteAsmHeader(F, Info, x, y, True);
end;


procedure TGBTDISASAsmExport.WriteInit( const Info : TGBOTileExport );
begin
  with Info do
  begin
    (* insert section *)
    writeln(F, Trim(SectionName) + ' GROUP ' + IntToStr(Bank) );
  end;
end;


procedure TGBTDISASAsmExport.WriteEnd( const Info : TGBOTileExport );
begin
  (* EOF marker *)
  writeln(F);
  writeln(F, '; End of ' + UpperCase(ExtractFileName(FName)));
end;


procedure TGBTDISASAsmExport.WriteData(const Info : TGBOTileExport; buf : TPByteArray; size : integer);
var i,j : integer;
begin
  i := 0;
  while (i < size) do
  begin
    write(F, ' DB ');
    for j := 0 to 7 do
      if ((i+j) < size) then
      begin
        write(F, '$' + IntToHex(buf[i+j], 2) );
        if (j < 7) and ((i+j) < (size-1)) then write(F, ',');
      end;
    writeln(F);
    i := i + j;
  end;
end;


procedure TGBTDISASAsmExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  case CType of
    gbctByte  : writeln(F, ' DB ' + IntToStr(cnt));
    gbctWord  : writeln(F, ' DW ' + IntToStr(cnt));
  end;
end;


procedure TGBTDISASAsmExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F,' PUBLIC ' + lbl);
  writeln(F, lbl );
end;


(*************************************************)
(*                                               *)
(*               ISAS INC output                 *)
(*                                               *)
(*************************************************)

constructor TGBTDISASIncExport.Create(const fn : string );
begin
  AssignFile(F, fn);
  Rewrite(F);
  FName := fn;
end;


destructor TGBTDISASIncExport.Destroy;
begin
  CloseFile(F);
  inherited;
end;



procedure TGBTDISASIncExport.WriteHeader(const Info : TGBOTileExport; x,y : integer );
begin
  (* write specific header for ASM-files *)
  writeln(F, '; ' + UpperCase(ExtractFileName(FName)));
  writeln(F, ';');
  writeln(F, '; Include File.');
  writeln(F, ';');

  (* write generic header *)
  WriteAsmHeader(F, Info, x, y, True);
end;


procedure TGBTDISASIncExport.WriteEnd(const Info : TGBOTileExport );
begin
  (* EOF marker *)
  writeln(F);
  writeln(F, '; End of ' + UpperCase(ExtractFileName(FName)));
end;


procedure TGBTDISASIncExport.WriteCounter(const Info : TGBOTileExport; cnt : integer; CType : TGBCounterType; const lbl : string );
begin
  if (CType = gbctEqu) then writeln(F, lbl + 'Len EQU ' + IntToStr(cnt));
end;


procedure TGBTDISASIncExport.WriteLabel(const Info : TGBOTileExport; const lbl : string; const comment : string; MainLabel : boolean );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F, ' EXTERN ' + lbl );
end;


procedure TGBTDISASIncExport.WriteConstant(const Info : TGBOTileExport; const lbl : string; const Value : string; const comment : string );
begin
  if (comment <> '') then
  begin
    writeln(F);                       (* write space *)
    writeln(F, '; ' + comment );
  end;
  writeln(F, lbl + ' EQU ' + Value );
end;

end.
