unit MapProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, MainLib, Buttons, Dialogs;

type
  TFrmMapProps = class(TForm)
    GroupBox1: TGroupBox;
    EdWidth: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EdHeight: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    EdTileFile: TEdit;
    DlgOpen: TOpenDialog;
    BtnBrowse: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EdNumChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EdTileFileExit(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnHelpClick(Sender: TObject);

  private
    { Private declarations }
    function ValidValues: boolean;

  public
    { Public declarations }
  end;

var
  FrmMapProps: TFrmMapProps;

implementation

uses Repository, GBMBMain, files;

{$R *.DFM}
{--$I ..\hlp\gbmb.inc}

procedure TFrmMapProps.FormCreate(Sender: TObject);
begin
  EdWidth.Text := IntToStr(TileMap.Width);
  EdHeight.Text := IntToStr(TileMap.Height);
  EdTileFile.Text := TileFileName;
//  HelpContext := Map_properties;
end;

procedure TFrmMapProps.FormDestroy(Sender: TObject);
begin
  FrmMapProps := nil;
end;

procedure TFrmMapProps.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  if (ModalResult = mrOk) then Modified := True;
end;

procedure TFrmMapProps.EdNumChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;

procedure TFrmMapProps.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) then
    if ValidValues then
    begin
      TileMap.SetBounds( StrToInt(EdWidth.Text), StrToInt(EdHeight.Text), True);

      TileFileName := EdTileFile.Text;
      SetTileFile(TileFilename);


      RefreshList;
      RefreshMap;

      Modified := True;
    end
    else
      CanClose := False;
end;


function TFrmMapProps.ValidValues: boolean;
var w, h : integer;
  function Test( Cond : boolean; s : string; ctrl : TWinControl ): boolean;
  begin
    Result := Cond;
    if not Result then
    begin
      Msg(s + '.', MsgError, MB_OK);
      ctrl.SetFocus;
    end;
  end;

begin
  Result :=
    Test( (EdWidth.Text <> ''), 'Width is mandatory', EdWidth) and
    Test( (EdHeight.Text <> ''), 'Height is mandatory', EdHeight) and
    Test( (EdTileFile.Text <> ''), 'FileName is mandatory', EdTileFile) and
    Test( (FileExists(ExpandRelativePath(EdTileFile.Text, BaseDir))), '"'+ EdTileFile.Text +'" does not exist', EdTileFile);

  if Result then
  begin
    w := StrToInt(EdWidth.Text);
    h := StrToInt(EdHeight.Text);

    Result :=
      Test( (w <= MAXMAPWIDTH), Format('Width should be lower or equal to %d',[MAXMAPWIDTH]), EdWidth) and
      Test( (w > 0), 'Width should be at least 1', EdWidth) and
      Test( (h <= MAXMAPHEIGHT), Format('Height should be lower or equal to %d',[MAXMAPHEIGHT]), EdHeight) and
      Test( (h > 0), 'Height should be at least 1', EdHeight);
  end;
end;

procedure TFrmMapProps.EdTileFileExit(Sender: TObject);
begin
  EdTileFile.Text := Trim(EdTileFile.Text);
end;

procedure TFrmMapProps.BtnBrowseClick(Sender: TObject);
var s : string;
begin
  s := ExtractFilePath(EdTileFile.Text);
  if (s = '') then s := BaseDir;
  if (s = '') then s := IniFile.ReadString('General', 'GBRPath', '');
  DlgOpen.InitialDir := s;

  DlgOpen.FileName := EdTileFile.Text;
  if DlgOpen.Execute then
  begin
    EdTileFile.Text := GetRelativePath( DlgOpen.FileName );

    IniFile.WriteString('General', 'GBRPath', ExtractFilePath(DlgOpen.Filename));
  end;

  (* correct possible corruption *)
  ResetBaseDir;
end;

procedure TFrmMapProps.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

procedure TFrmMapProps.BtnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
