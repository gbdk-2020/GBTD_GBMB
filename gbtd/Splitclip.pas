unit SplitClip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TileEdit, gbConst, mainlib, Clipbrd;

type
  TSplitFunction = (sfPaste, sfCopy);

  TFrmSplitClip = class(TForm)
    GroupBox1: TGroupBox;
    EdWidth: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EdHeight: TEdit;
    CBOrder: TComboBox;
    Label3: TLabel;
    BtnHelp: TButton;
    BtnCancel: TButton;
    BtnPaste: TButton;
    BtnCopy: TButton;
    procedure EdChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
  private
    { Private declarations }
    FSplitFunction : TSplitFunction;

    function GetTileWidth : integer;
    procedure SetTileWidth( i : integer );
    function GetTileHeight : integer;
    procedure SetTileHeight( i : integer );
    function GetTileOrder : TSplitOrder;
    procedure SetTileOrder( i : TSplitOrder );


  public
    { Public declarations }
    property TileWidth : integer read GetTileWidth write SetTileWidth;
    property TileHeight : integer read GetTileHeight write SetTileHeight;
    property TileOrder : TSplitOrder read GetTileOrder write SetTileOrder;
    property SplitFunction : TSplitFunction read FSplitFunction write FSplitFunction;
  end;

var
  FrmSplitClip: TFrmSplitClip;

implementation

{$R *.DFM}

{--$I ..\hlp\gbtd.inc}

function TFrmSplitClip.GetTileOrder : TSplitOrder;
begin
  result := TSplitOrder(CBOrder.ItemIndex);
end;


procedure TFrmSplitClip.SetTileOrder( i : TSplitOrder );
begin
  if (integer(i) >= 0) and (integer(i) < CBOrder.Items.Count) then
    CBOrder.ItemIndex := integer(i)
  else
    CBOrder.ItemIndex := 0;
end;


function TFrmSplitClip.GetTileWidth : integer;
begin
  try
    result := StrToInt(EdWidth.Text);
  except
    result := 0;
  end;
end;


procedure TFrmSplitClip.SetTileWidth( i : integer );
begin
  if (i > 1) then
    EdWidth.Text := IntToStr(i)
  else
    EdWidth.Text := '1';
end;


function TFrmSplitClip.GetTileHeight : integer;
begin
  try
    result := StrToInt(EdHeight.Text);
  except
    result := 0;
  end;
end;


procedure TFrmSplitClip.SetTileHeight( i : integer );
begin
  if (i > 1) then
    EdHeight.Text := IntToStr(i)
  else
    EdHeight.Text := '1';
end;


procedure TFrmSplitClip.EdChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;



procedure TFrmSplitClip.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) then
  begin
    CanClose := False;
    if (TileWidth < 1) then
    begin
      Messagedlg('Tile width should be at least one.', mtError, [mbOk],0 );
      EdWidth.SetFocus;
    end
    else
      if (TileHeight < 1) then
      begin
        Messagedlg('Tile height should be at least one.', mtError, [mbOk],0 );
        EdHeight.SetFocus;
      end
      else
        CanClose := True;
  end;
end;

procedure TFrmSplitClip.FormDestroy(Sender: TObject);
begin
  FrmSplitClip := nil;
end;

procedure TFrmSplitClip.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

procedure TFrmSplitClip.FormCreate(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    BtnPaste.Enabled := True;
    BtnPaste.Default := True;
    BtnCopy.Default := False;
  end
  else
  begin
    BtnPaste.Enabled := False;
    BtnPaste.Default := False;
    BtnCopy.Default := True;
  end;

//  HelpContext := Split_options;
end;

procedure TFrmSplitClip.BtnPasteClick(Sender: TObject);
begin
  SplitFunction := sfPaste;
end;

procedure TFrmSplitClip.BtnCopyClick(Sender: TObject);
begin
  FSplitFunction := sfCopy;
end;

end.
