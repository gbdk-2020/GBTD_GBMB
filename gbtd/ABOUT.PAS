unit about;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, gbconst, MainLib, ShellApi;

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
    procedure LblURLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmAbout: TFrmAbout;

implementation

{$R *.DFM}

{--$I ..\hlp\gbtd.inc}

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  LblVersion.Caption := 'Version ' + GBTDVersion;
  ImgIcon.Picture.Assign(Application.Icon);

//  HelpContext := Contacting_me_and_others;

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
end;

procedure TFrmAbout.LblURLClick(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open', 'http://www.casema.net/~hpmulder/index.html', nil, nil, SW_SHOWNORMAL);
end;

end.
