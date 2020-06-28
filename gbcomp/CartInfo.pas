unit CartInfo;

interface

uses classes, SysUtils;

type

  THardwareStart = (hsDefault, hsGB, hsSGB, hsGBC);
  TEmuStart      = integer;


  TCartInfo = class
  private
    { Private declarations }
    FName      : string;
    FCartName  : string;         (* max 16 *)
    FFileName  : TFileName;
    FPath      : integer;
    FSGB       : boolean;
    FGBC       : boolean;
    FJapanese  : boolean;
    FPublisher : integer;
    FHStart    : THardwareStart;
    FEStart    : TEmuStart;

  protected
    { protected declarations }

    function GetName : string;
    procedure SetName(s : string);
    procedure SetCartName(s : string);
    procedure SetFileName(s : TFileName);

  public
    { Public declarations }
    property Name : string read GetName write SetName;
    property CartName : string read FCartName write SetCartName;
    property FileName : TFileName read FFileName write SetFileName;
    property Path : integer read FPath write FPath;

    property SGB      : boolean read FSGB write FSGB;
    property GBC      : boolean read FGBC write FGBC;
    property Japanese : boolean read FJapanese write FJapanese;

  end;




implementation

function CapitalCase( s : string): string;
var i,j : integer;
begin
  i := Length(s);
  if (i > 0) then
  begin
    SetLength(Result, i);
    Result[1] := UpCase(s[1]);
    for j := 2 to i do
      if (s[j] in ['A'..'Z']) then Result[j] := char(ord(s[j])-ord('A')+ord('a')) else Result[j] := s[j];
  end
  else
    Result := s;
end;



function TCartInfo.GetName : string;
begin
  Result := FName;
end;

procedure TCartInfo.SetName(s : string);
begin
  FName := Trim(s);
end;

procedure TCartInfo.SetCartName(s : string);
begin
  FCartName := Trim(s);
  if (Name = '') then Name := CapitalCase(FCartName);
end;

procedure TCartInfo.SetFileName(s : TFileName);
begin
  FFileName := Trim(s);
end;

end.
