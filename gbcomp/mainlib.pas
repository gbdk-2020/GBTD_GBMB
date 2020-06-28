unit mainlib;

interface

uses Windows, SysUtils, controls,  stdctrls, Clipbrd, Graphics, Classes,
     Forms, dialogs, Registry;


const
  MsgConfirmation = 0;
  MsgError        = 1;
  MsgInformation  = 2;


const
  AlphaSet : set of char = ['a'..'z','A'..'Z','!','@','#','$','%','^','&','*',
                            '(',')','_','-','=','+','\','|','{','[','}',']',':',
                            ';','"','''','<',',','>','.','?','/','`','~'];





type
  ByteMem = array[0..MAXINT-1] of byte;
  PByteMem = ^ByteMem;

  WordMem = array[0..(MAXINT div SizeOf(word))-1] of word;
  PWordMem = ^WordMem;

  IntMem = array[0..(MAXINT div sizeof(integer))-1] of integer;
  PIntMem = ^IntMem;

  procedure SetBuffer(buf : PByteMem; x,y : integer; Value : byte);
  function LoadString( s : string; Default : string ): string;
  procedure ForceNumbers(AEdit: TCustomEdit);

  function GetClipboardBitmapSize(var Width, Height : integer): boolean;
  procedure StreamToClipboard(const stream : tstream);
  procedure ClipboardToStream(const stream : TStream );
  procedure TextClipboardToStream(const stream : TStream );

  function LineFromStream(const stream : TStream) : string;
  procedure SeparateStrData(const s : string; const Lst : TStrings );

  function BitsNeeded( i : integer): integer;

  function CharLocBackWards( const s : string; c : char): integer;
  function MemComp(const p, q : PByteMem; Size : integer): boolean;

  function Msg(const Txt : string; dType : integer; Flags: Longint): Integer;

  procedure ClearBuf( buf : Pointer; Cnt : integer );
  procedure CopyBuf (src, dest : Pointer; Cnt : integer );

  function SaveStrToInt(const s : string): integer;
  Function GetProgramPathFromExt(Const Ext : String) : String;

  function ExpandRelativePath( const RelPath, BasePath : string): string;
implementation



function ExpandRelativePath( const RelPath, BasePath : string): string;
var
  Len,i : integer;
  BaseLst, RelLst : TStringList;

  procedure Split( const ins : string; Lst : TStringList);
  var
    i : integer;
    s : string;
    inQuotes : boolean;
  begin
    inQuotes := False;
    s := '';

    for i := 1 to Length(ins) do
    begin
      if (ins[i] = '\') and (not inQuotes) and (s <> '') then
      begin
        Lst.Add(s);
        s := '';
      end
      else
      begin
        if (ins[i] = '"') then inQuotes := not inQuotes;

        s := s + ins[i];
      end;
    end;
    if (s <> '') then Lst.Add(s);
  end;

begin
  Len := Length(RelPath);
  if (Len < 1) then
    Result := BasePath
  else
    (* check if RelPath is already absolute *)
    if (Len > 2) and (RelPath[2] = ':') then
      Result := RelPath
    else
    begin
      BaseLst := TStringList.Create;
      RelLst := TStringList.Create;
      try

        Split(RelPath, RelLst);
        Split(BasePath, BaseLst);

        (* append RelPath to BasePath *)
        for i := 0 to RelLst.Count-1 do
        begin
          if (CompareStr( RelLst.Strings[i], '.' ) <> 0) then
          begin
            if (CompareStr( RelLst.Strings[i], '..' ) = 0) then
              BaseLst.Delete(BaseLst.Count-1)
            else
              BaseLst.Add( RelLst.Strings[i]);
          end;
        end;

        (* construct new path *)
        Result := BaseLst.Strings[0];
        for i := 1 to BaseLst.Count-1 do
          Result := Result + '\' + BaseLst.Strings[i];

      finally
        RelLst.Free;
        BaseLst.Free;
      end;
    end;
end;



// Pick a program from the registry associated with a extension
// if not found, result := ''.
Function GetProgramPathFromExt(Const Ext : String) : String;
Begin
  Result := '';
  With TRegistry.Create do
  Try
    RootKey := HKEY_CLASSES_ROOT;

    If OpenKey('\'+Ext,False) Then
    Begin
      Result := ReadString('');
      If (Result <> '') Then
      Begin
        If OpenKey('\'+Result+'\shell\open\command',False) Then
          Result := ReadString('');
      end
      else
      Begin
        If OpenKey('\'+Ext+'\shell\open\command',False) Then
          Result := ReadString('');
      end;
    end;
  Finally
    Free;
  end;
end;


function SaveStrToInt(const s : string): integer;
begin
  try Result := StrToInt(s) except Result := 0; end;
end;

function Msg(const Txt : string; dType : integer; Flags: Longint): Integer;
var s : string;
begin
  if Assigned(Application) then
  begin
    case dType of
      MsgConfirmation  : begin Flags := Flags + MB_ICONQUESTION; s := 'Confirm'; end;
      MsgError         : begin Flags := Flags + MB_ICONERROR; s := 'Error'; end;
    end;
    result := Application.MessageBox(PChar(Txt), PChar(s), Flags)
  end
  else
    result := 0;
end;


function MemComp(const p, q : PByteMem; Size : integer): boolean;
var i : integer;
begin
  result := True;
  for i := 0 to Size-1 do
    if (p[i] <> q[i]) then
    begin
      Result := False;
      exit;
    end;
end;

function CharLocBackWards( const s : string; c : char): integer;
var i : integer;
begin
  Result := -1;
  i := Length(s);
  while (i > 0) and (Result = -1) do
  begin
    dec(i);
    if (s[i] = c) then Result := i;
  end;
end;

function BitsNeeded( i : integer): integer;
begin
  Result := 0;
  repeat
    i := i div 2;
    Inc(Result);
  until (i <= 0);
end;

procedure SeparateStrData(const s : string; const Lst : TStrings );
const seps : set of char = [chr(9),' ',',',';',':'];
var i : integer;
    t : string;
begin
  t := '';
  for i := 1 to Length(s) do
  begin
    if (s[i] in seps ) then
    begin
      if (t <> '') then Lst.Add(t);
      t := '';
    end
    else
      t := t + s[i];
  end;
  if (t <> '') then Lst.Add(t);
end;


function LineFromStream(const stream : TStream) : string;
var c : char;
    eol : boolean;
begin

  result := '';
  eol := False;
  with Stream do
    while (Position < Size) and (not eol) do
    begin
      Read(c,1);
      if (c = chr(10)) or (c=chr(13)) then
      begin
        eol := True;

        (* remove EOL *)
        c := chr(0);
        Read(c,1);
        if (c <> chr(10)) and (c <> chr(13)) then
          Seek(soFromBeginning, Position-1);
      end
      else
      Result := Result + c;
    end;
end;

procedure TextClipboardToStream(const stream : TStream );
(*************************************************)
(* use this one instead of ClipboardToStream for *)
(* text-objects.                                 *)
(*************************************************)
var
  p    : PChar;
  len  : integer;
  Size : integer;
begin
  (* init *)
  p    := nil;
  len  := 0;
  Size := 0;

  try
    (* determine total buffer by trying *)
    while (len = Size) do
    begin
      if Assigned(p) then FreeMem(p);        // free buffer
      Size := Size + 2048;                   // increase buffer
      GetMem(p, Size);                       // get buffer
      len := Clipboard.GetTextBuf(p, Size);  // load
    end;

    stream.seek(soFromBeginning,0);
    stream.Write(p^, len);

  finally
    if Assigned(p) then FreeMem(p);
  end;


{    (* convert from clipboard to stream *)
    GetMem(p, 128 * 1024);
    try
      len := Clipboard.GetTextBuf(p, 128*1024);
      strm.seek(soFromBeginning,0);
      strm.Write(p^, len);
    finally
      FreeMem(p);
    end;}
end;


procedure ClipboardToStream(const stream : TStream );
(***************************************************)
(* Haalt data uit het clipboard over naar          *)
(* TStream. Resultaat is een PChar (met max lengte *)
(* van longint).                                   *)
(***************************************************)
var hbuf    : THandle;
    bufptr  : Pointer;
begin
  (* Get and lock clipboard-data *)
  hbuf := Clipboard.GetAsHandle(CF_TEXT);
  if (hbuf <> null) then
  begin
    bufptr :=GlobalLock(hbuf);
    try


      (* overpompen van clipboard-data *)
      stream.seek(soFromBeginning,0);
      stream.write(bufptr^, GlobalSize(hbuf));

    finally
      (* vrijgeven van clipboard-data *)
      GlobalUnLock(hbuf);
    end;
  end;
end;



procedure StreamToClipboard(const stream : TStream);
(********************************************)
(* Versturen van TStream naar het clipboard *)
(********************************************)
var
  hbuf    : THandle;
  bufptr  : Pointer;
begin
  (* global mem alloceren *)
  hbuf := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, stream.size+1);
  bufptr := GlobalLock(hbuf);

  (* van stream naar global mem *)
  stream.seek(0,0);
  stream.read(bufptr^, stream.size+1);

  (* global mem in clipboard plaatsen *)
  GlobalUnlock(hbuf);
  Clipboard.SetAsHandle(CF_TEXT, hbuf);

  (* global mem is niet meer van ons; dus NIET vrijgeven *)
end;



procedure SetBuffer(buf : PByteMem; x,y : integer; Value : byte);
(**********************************************************)
(* Set all bytes in a buffer to value. No range checking. *)
(**********************************************************)
var i : integer;
begin
  for i := 0 to (x*y)-1 do buf[i] := Value;
end;

function GetClipboardBitmapSize(var Width, Height : integer): boolean;
(**********************************************************************)
(* bepaald afmeting van clipboard-bitmap en geeft deze in vars terug; *)
(* result = actie gelukt.                                             *)
(**********************************************************************)
var Buf : TBitmap;
begin
  Result := False;
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    Buf := TBitmap.Create;
    try
      Buf.Assign(Clipboard);
      Width := Buf.Width;
      Height := Buf.Height;

      Result := True;
    finally
      Buf.Free;
    end;
  end;
end;


function LoadString( s : string; Default : string ): string;
(*******************************************************************)
(* Geeft een getrimde s terug, tenzij deze leeg is, dan geeft deze *)
(* functie een getrimde default terug.                             *)
(*******************************************************************)
begin
  Result := trim(s);
  if ( s = EmptyStr) then Result := trim(Default);
end;





procedure ForceNumbers(AEdit: TCustomEdit);
(***********************************************************************)
(* Zorgt ervoor dat de aangeleverde control alleen maar nummers heeft. *)
(* LET OP: gebruikt de Tag-property.                                   *)
(***********************************************************************)
var s : string;
var i : integer;
var Found : boolean;
begin
  with AEdit do
  begin
    if (Tag = 0) then
    begin
      Tag := 1;

      Found := False;
      for i := 0 to Length(Text)-1 do
        if (Text[i+1] in ['0'..'9']) then
          s := s + Text[i+1]
        else
          Found := True;

      i := SelStart - 1;
      Text := s;
      if Found then SelStart := i;
    end;

    Tag := 0;
  end;
end;


procedure ClearBuf( buf : Pointer; Cnt : integer );
var p : Pointer;
    i : integer;
begin
  (* set to default *)
  p := buf;
  for i := 0 to (Cnt div 4)-1 do
  begin
    integer(p^) := 0;
    inc(integer(p), SizeOF(integer));
  end;
  for i := 0 to (Cnt mod 4)-1 do
  begin
    byte(p^) := 0;
    inc(integer(p), SizeOF(byte));
  end;
end;

procedure CopyBuf (src, dest : Pointer; Cnt : integer );
var p,q : Pointer;
    i : integer;
begin
  (* set to default *)
  p := dest;
  q := src;
  for i := 0 to (Cnt div 4)-1 do
  begin
    integer(p^) := integer(q^);
    inc(integer(p), SizeOF(integer));
    inc(integer(q), SizeOF(integer));
  end;
  for i := 0 to (Cnt mod 4)-1 do
  begin
    byte(p^) := byte(q^);
    inc(integer(p), SizeOF(byte));
    inc(integer(q), SizeOF(byte));
  end;
end;


end.
