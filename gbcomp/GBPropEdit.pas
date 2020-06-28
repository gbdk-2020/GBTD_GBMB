unit GBPropEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GBConst;

type
  TGBPropEdit = class(TCustomControl)
  private
    { Private declarations }
    FLabel  : TLabel;
    FEdit   : TEdit;

    FMapData  : TGBMapType;
    FMapProp : integer;

    FTotalWidth : integer;
    FMinWidth   : integer;

    procedure SetMapData( i : TGBMapType );
    procedure SetTotalWidth( i : integer );

  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure RefreshProperty;

    property MapData : TGBMapType read FMapData write SetMapData;
//    property MapData : TGBMapType read FMapData write SetMapData;

  published
    { Published declarations }

    property TotalWidth : integer read FTotalWidth write SetTotalWidth;
    property MinWidth   : integer read FMinWidth;

  end;

procedure Register;

implementation

const
  MINDISTANCE = 15;
  EDWIDTH     = 38;
  EDHEIGHT    = 19;

procedure Register;
begin
  RegisterComponents('Gameboy', [TGBPropEdit]);
end;

constructor TGBPropEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);



//  ControlStyle := ControlStyle + [csFixedWidth, csFixedHeight];

  FLabel := TLabel.Create(Self);
  with FLabel do
  begin
    Parent := Self;
    AutoSize := False;

    Top := 3;
  end;

  FEdit  := TEdit.Create(Self);
  with FEdit do
  begin
    Parent := Self;
    AutoSize := False;

    Width := EDWIDTH;
    Height := EDHEIGHT;
  end;

end;


procedure TGBPropEdit.Loaded;
begin
  Height := 30;
end;



destructor TGBPropEdit.Destroy;
begin
  FEdit.Free;

  FLabel.Free;

  inherited Destroy;
end;


procedure TGBPropEdit.SetMapData( i : TGBMapType );
begin
  FMapData := i;

  RefreshProperty;
end;

procedure TGBPropEdit.RefreshProperty;
begin
  if Assigned(FMapData) and (FMapData.PropCount > FMapProp) then
  begin
    FLabel.Caption := FMapData.PropDef[FMapProp].Name;
    FMinWidth      := MINDISTANCE + Canvas.TextWidth(FLabel.Caption) + EDWIDTH;
  end;
end;

procedure TGBPropEdit.SetTotalWidth( i : integer );
begin
  FLabel.Width := i - MINDISTANCE - EDWIDTH;
  FEdit.Left := i - EDWIDTH;
  Self.Width := i;
end;


end.
