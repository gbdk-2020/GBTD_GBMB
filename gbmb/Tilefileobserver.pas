unit TileFileObserver;

interface

uses
  Classes, Windows,SysUtils, GBMBMain, files, Repository, Dialogs, Messages, controls;

const
  CM_FILECHANGENOTIFICATION = WM_USER + 705;


type
  TTileFileObserver = class(TThread)
  private
    { Private declarations }
    hnd : THandle;
    FReceiver : TWinControl;
  protected

  public
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean; const f : string; const Receiver : TWinControl);
  end;

implementation

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TTileFileObserver.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TTileFileObserver }

uses Map;

constructor TTileFileObserver.Create(CreateSuspended: Boolean;const f : string; const Receiver : TWinControl);
var s : string;
begin
  inherited Create(True);
  Priority := tpIdle;
  s := ExtractFilePath(f);
  (* Win95 doesn't like trailing slashes.. *)
  if ((Length(s) > 0) and (s[Length(s)] = '\')) then s[Length(s)] := char(0);

  FReceiver := Receiver;
  hnd := FindFirstChangeNotification(PChar(s), False, FILE_NOTIFY_CHANGE_LAST_WRITE);
  Suspended := False;
end;



procedure TTileFileObserver.Execute;
var i : integer;
begin
  { Place thread code here }
  while (not Terminated) do
  begin
    i := WaitForSingleObject(hnd, 30);
    if (i = WAIT_OBJECT_0) then
    begin
      (* report change *)
      PostMessage(FReceiver.Handle, CM_FILECHANGENOTIFICATION, 0, 0);

      FindNextChangeNotification(hnd);
    end;
  end;
  FindCloseChangeNotification(hnd);
end;

end.
