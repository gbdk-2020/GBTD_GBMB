unit about;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Buttons, ExtCtrls, gbconst, shellapi, MainLib;

type
  TFrmAbout = class(TForm)
    BtnOk: TBitBtn;
    ImgIcon: TImage;
    Label1: TLabel;
    Label2: TLabel;
    LblVersion: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Label6: TLabel;
    LblURL: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LblURLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmAbout: TFrmAbout;

implementation

uses Repository;

{$R *.DFM}

{--$I ..\hlp\gbmb.inc}

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  LblVersion.Caption := 'Version ' + GBMBVersion;
  ImgIcon.Picture.Assign(Application.Icon);

  (* turn URl into link ? *)
  if (GetProgramPathFromExt('.html') <> '') then
  with LblURL do
  begin
    OnClick    := LblURLClick;
    Cursor     := crHandPoint;
    Font.Color := clBlue;
    Font.Style := [fsUnderline];
    ShowHint   := True;
  end;

//  HelpContext := Contacting_me_and_others; 
end;

procedure TFrmAbout.FormDestroy(Sender: TObject);
begin
  FrmAbout := nil;
end;

procedure TFrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmAbout.LblURLClick(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open', 'http://www.casema.net/~hpmulder/index.html', nil, nil, SW_SHOWNORMAL);
end;

end.
