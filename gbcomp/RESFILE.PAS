unit Resfile;

interface

uses
  SysUtils, Classes;

type TRESFileTypeNum =
     ( rtNameOnly,           rtCursor,         rtBitmap,
       rtIcon,               rtMenu,           rtDialogBox,
       rtStringTable,        rtFontDirectory,  rtFont,
       rtAcceleratorTable,   rtRCData,         rtNotAllowed1,
       rtGroupCursor,        rtNotAllowed2,    rtGroupIcon,
       rtNameTable,          rtVersionInfo );

const
  rfMoveable    = $10;
  rfPure        = $20;
  rfPreLoad     = $40;
  rfDiscardable = $1000;


type
  TRESFile = class(TObject)
  (***************************************************)
  (* Opent .RES bestand en geeft de mogelijkheid tot *)
  (* inlezen van resources.                          *)
  (***************************************************)
  private
    { Private declarations }
    Strm       : TFileStream;
    DataPos    : LongInt;
    VEof       : Boolean;
    VTypeIsNum : Boolean;
    VTypeNum   : TRESFileTypeNum;
    VTypeName  : string;
    VIDIsNum   : boolean;
    VIDNum     : word;
    VIDName    : string;
    VFlags     : Word;
    VSize      : LongInt;




    procedure ReadHeader;

  public
    { Public declarations }
    constructor Create(const FileName : string);
    procedure free;

    procedure First;
    procedure Next;
    procedure GetResource(var Buffer);


    property TypeIsNum : Boolean read VTypeIsNum;
    property TypeNum   : TRESFileTypeNum read VTypeNum;
    property TypeName  : string read VTypeName;
    property IDIsNum   : Boolean read VIDIsNum;
    property IDNum     : word read VIDNum;
    property IDName    : string read VIDName;
    property Flags     : word read VFlags;
    property Size      : LongInt read VSize;
    property Eof       : Boolean read VEof;
  end;

implementation

uses OakLib;

(* Exception messages *)
const
  UnexpectedEndMsg : TMsgArray = ( 'RES file corrupted.',
                                   'RES file beschadigd.' );

  Win32ErrMsg : TMsgArray = ( 'Win32 resource found.',
                              'Win32 resource gevonden.' );

constructor TRESFile.Create(const FileName : string);
(******************)
(* Opent RES-file *)
(******************)
begin
  (* Openen van stream *)
  Strm := TFileStream.Create( FileName, fmOpenRead );
  ReadHeader;
end;


procedure TRESFile.free;
(******************)
(* Sluit RES-file *)
(******************)
begin
  if Assigned(Strm) then Strm.Destroy;
end;


procedure TRESFile.First;
(************************)
(* Naar eerste resource *)
(************************)
begin
  Strm.seek(0,0);
  ReadHeader;
end;


procedure TRESFile.Next;
(**************************)
(* Naar volgende resource *)
(**************************)
begin
  Strm.seek(DataPos + VSize, 0);
  ReadHeader;
  if VEof then
    Strm.seek(DataPos, 0); (* bestand goedzetten *)
end;


procedure TRESFile.GetResource(var Buffer);
(****************************************)
(* Plaatst resource in Buffer.          *)
(* De afmeting van Buffer moet minimaal *)
(* Size zijn.                           *)
(****************************************)
begin
  (* Inlezen *)
  if (Strm.read(Buffer, VSize) < VSize) then
    raise EStreamError.Create( GetLangMsg(UnexpectedEndMsg) );

  (* positie goedzetten *)
  Strm.Seek(DataPos, 0);
end;


procedure TRESFile.ReadHeader;
(***********************************************)
(* interne functie; leest gegevens van huidige *)
(* resource in.                                *)
(***********************************************)
var
  buf : array[0..255] of char;
  pos : LongInt;
begin
  (* Type is num of char ? *)
  VEof := (Strm.read( buf[0], 1) < 1);

  if not VEof then
  begin
    case (SmallInt(buf[0])) of
      $00 :
        (* 32-bit RES-file niet ondersteund *)
        raise EStreamError.Create( GetLangMsg(Win32ErrMsg) );

      $FF :
        begin
          (* Type is numeriek *)
          VTypeIsNum := True;
          if (Strm.read(vTypeNum, 2) < 2) then
            raise EStreamError.Create( GetLangMsg(UnexpectedEndMsg) );
          vTypeName := '';
        end;

      else
        begin
          (* Type is alfanumeriek (user-defined) *)
          VTypeIsNum := False;
          pos := Strm.Seek(0,1)-1;        (* huidige positie *)
          Strm.read(buf[1], 255);
          VTypeName := StrPas(buf);
          Strm.Seek(pos+StrLen(buf)+1, 0);
          VTypeNum := rtNameOnly;
        end;
    end;


    (* ID is num of char ? *)
    if (Strm.read( buf[0], 1) < 1) then
      raise EStreamError.Create( GetLangMsg(UnexpectedEndMsg) );

    case (SmallInt(buf[0])) of
      $FF :
        begin
          (* ID is numeriek *)
          VIDIsNum := True;
          if (Strm.read(VIDNum, 2) < 2) then
            raise EStreamError.Create( GetLangMsg(UnexpectedEndMsg) );
          vIDName := '';
        end;

      else
        begin
          (* ID is alfanumeriek (user-defined *)
          VIDIsNum := False;
          pos := Strm.Seek(0,1)-1;        (* huidige positie -1*)
          Strm.read(buf[1], 255);
          VIDName := StrPas(buf);
          Strm.Seek(pos+StrLen(buf)+1, 0);
          VIDNum := 0;
        end;
    end;

    (* Overig *)
    if (Strm.read(VFlags, 2) < 2 )then
      raise EStreamError.Create( GetLangMsg(UnexpectedEndMsg) );
    if (Strm.read(VSize, 4) < 4) then
      raise EStreamError.Create( GetLangMsg(UnexpectedEndMsg) );
    DataPos := Strm.seek(0,1);
  end;
end;


end.
