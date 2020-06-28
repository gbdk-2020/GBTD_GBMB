unit AppMem;

interface

uses Windows, SysUtils;

type
  TAppMem = class
   private
    { Private declarations }

    FHandle   : HWND;
    FMemBlock : Pointer;
    FSize     : integer;


   protected
    { Protected declarations }


   public
    { Public declarations }

    constructor Create( const ID : string; Size : integer; ReadOnly : boolean; var Existed : boolean );
    destructor Destroy; override;

    property MemBlock : Pointer read FMemBlock write FMemBlock;
    property Size     : integer read FSize;
  end;

implementation


constructor TAppMem.Create( const ID : string; Size : integer; ReadOnly : boolean; var Existed : boolean);
var s : string;
begin
  inherited Create;

  s := UpperCase(ID);
  { Convert spaces to zeroes }
  while Pos('\', s) > 0 do
  begin
    S[Pos('\', S)] := '@';
  end;

  FHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, Size, PChar(s));
  if (FHandle <> null) then
  begin
    Existed := (GetLastError = ERROR_ALREADY_EXISTS);
    if ReadOnly then
      FMemBlock := MapViewOfFile( FHandle, FILE_MAP_READ, 0, 0, Size )
    else
      FMemBlock := MapViewOfFile( FHandle, FILE_MAP_WRITE, 0, 0, Size );

    FSize := Size;
  end;
end;

destructor TAppMem.Destroy;
begin
  if Assigned(FMemBlock) then UnmapViewOfFile(FMemBlock);
  if (FHandle <> null) then CloseHandle(FHandle);

  inherited Destroy;
end;



end.
