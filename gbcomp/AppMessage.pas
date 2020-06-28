unit AppMessage;

interface

uses Controls, messages, Windows, SysUtils;

type
  TAppMessage = class
   private
    { Private declarations }
    FMsg        : integer;
    FMaster     : TControl;
    FOldHandler : TWndMethod;

    FActive     : boolean;
    FOnMessage  : TWndMethod;

   protected
    { Protected declarations }

    procedure SetActive ( b : boolean );
    procedure WindowProcHandler(var Message: TMessage);

   public
    { Public declarations }

    constructor Create( Master : TControl; const MsgDesc : string );
    destructor Destroy; override;

    function SendAppMessage(WParam : word; LParam : LongInt): LongInt;

    property Active    : boolean read FActive write SetActive;
    property OnMessage : TWndMethod read FOnMessage write FOnMessage;

   end;

implementation


constructor TAppMessage.Create(Master : TControl; const MsgDesc : string );
begin
  inherited Create;

  FMaster := Master;
  FMsg := RegisterWindowMessage(PChar(UpperCase(MsgDesc)));
end;


destructor TAppMessage.Destroy;
begin
  Active := False;

  inherited Destroy;
end;



procedure TAppMessage.SetActive ( b : boolean );
begin
  if (b <> FActive) and Assigned(FMaster) then
  begin
    if b then
    begin
      (* link handler into chain *)
      FOldHandler := FMaster.WindowProc;
      FMaster.WindowProc := WindowProcHandler;
    end
    else
      (* unlink from chain *)
      FMaster.WindowProc := FOldHandler;

    FActive := b;
  end;
end;


procedure TAppMessage.WindowProcHandler(var Message: TMessage);
begin
  if (message.Msg = FMsg) and (Assigned(FOnMessage)) then FOnMessage(Message);

  (* call next in chain *)
  if Assigned(FOldHandler) then FOldHandler(Message);
end;


function TAppMessage.SendAppMessage(WParam : word; LParam : LongInt): LongInt;
begin
  Result := SendMessage(HWND_BROADCAST, FMsg, WParam, LParam);
end;

end.
