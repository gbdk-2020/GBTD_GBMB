unit TileProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  StdCtrls, Repository, gbConst, Impstringgrid, MainLib, Grids;


type
  TFrmTileProps = class(TForm)
    GrpProps: TGroupBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    GroupBox1: TGroupBox;
    CmbRedFld: TComboBox;
    Label6: TLabel;
    CmbRedOp: TComboBox;
    EdRedValue: TEdit;
    Label4: TLabel;
    CmbGreenFld: TComboBox;
    CmbGreenOp: TComboBox;
    EdGreenValue: TEdit;
    BtnAdd: TButton;
    BtnDel: TButton;
    GrdProps: TImpStringgrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnHelpClick(Sender: TObject);
    procedure EdPropColorChange(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GrdPropsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure GrdPropsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    { Private declarations }
    FLst        : TStringList;

    procedure BuildColProps;
    procedure RowCountChanged;

    procedure SaveProps;
    function ValidProps: boolean;

    procedure InsertNewProp;
    procedure DeleteProp;

  public
    { Public declarations }
  end;

var
  FrmTileProps: TFrmTileProps;

implementation

{$R *.DFM}
{--$I ..\hlp\gbmb.inc}


procedure TFrmTileProps.DeleteProp;
var i : integer;
begin
  with GrdProps do
    if (RowCount > 2) then
    begin
      (* Move Grid *)
      for i := Row to RowCount-2 do Rows[i] := Rows[i+1];
      RowCount := RowCount-1;
      SetFocus;

      RowCountChanged;
    end
    else
    begin
      (* clean up first entry *)
      Cells[1,1] := '';
      Cells[2,1] := '0';
    end;
end;

procedure TFrmTileProps.InsertNewProp;
begin
  if (GrdProps.RowCount < (PROPCNT+2)) then
    with GrdProps do
    begin
      (* build grid *)
      RowCount := RowCount+1;
      Cells[1,RowCount-1] := '';
      Cells[2,RowCount-1] := '1';
      Cells[3,RowCount-1] := '1';
      Row := RowCount-1;
      Col := 1;
      SetFocus;

      RowCountChanged;
    end;
end;



procedure TFrmTileProps.BuildColProps;
var i : integer;

begin
  FLst.Clear;
  for i := 0 to GrdProps.RowCount-2 do
      FLst.AddObject( GrdProps.Cells[1,i+1], pointer(i) );

  with CmbRedFld do
  begin
    i := ItemIndex;
    Items.Assign(FLst);
    if (i > Items.Count-1) then i := Items.Count-1;
    ItemIndex := i;
  end;

  with CmbGreenFld do
  begin
    i := ItemIndex;
    Items.Assign(FLst);
    if (i > Items.Count-1) then i := Items.Count-1;
    ItemIndex := i;
  end;
end;

procedure TFrmTileProps.FormCreate(Sender: TObject);
var i : integer;
    //j,t : integer;
begin

  (* init Grid *)
  if (TileMap.PropCount > 1) then
    GrdProps.RowCount := TileMap.PropCount+1
  else
    GrdProps.RowCount := 2;

  with GrdProps do
  begin
    Cells[1,0] := 'Name';
    Cells[2,0] := 'Max';
    Cells[3,0] := 'Bits';

    Columns.Items[0].Alignment := taCenter;
    Columns.Items[3].ReadOnly := True;
    Columns.Items[3].Color    := clBtnFace;
  end;


  (* load data *)
  if (TileMap.PropCount > 0) then
    for i := 0 to TileMap.PropCount-1 do
    begin
      GrdProps.Cells[1,i+1] := TileMap.PropDef[i].Name;
      GrdProps.Cells[2,i+1] := IntToStr(TileMap.PropDef[i].Size);
      GrdProps.Cells[3,i+1] := IntToStr( BitsNeeded( TileMap.PropDef[i].Size ) );
    end
  else
  begin
    GrdProps.Cells[1,1] := '';
    GrdProps.Cells[2,1] := '0';
  end;

  RowCountChanged;


  (* load property colors *)
  FLst := TStringList.Create;
  BuildColProps;


  CmbRedFld.ItemIndex := CmbRedFld.Items.IndexOfObject(pointer(PropColors.Colors[0].Prop));
  CmbRedOp.ItemIndex := PropColors.Colors[0].Operator;
  EdRedValue.Text := IntToStr(PropColors.Colors[0].Value);

  CmbGreenFld.ItemIndex := CmbGreenFld.Items.IndexOfObject(pointer(PropColors.Colors[1].Prop));
  CmbGreenOp.ItemIndex := PropColors.Colors[1].Operator;
  EdGreenValue.Text := IntToStr(PropColors.Colors[1].Value);


//  HelpContext := Location_properties;
end;


procedure TFrmTileProps.SaveProps;
var i : integer;
    prp : RMapPropDef;
    DelProps,StartDelProps : integer;
begin

  (* support for empty line *)
  if (GrdProps.RowCount <= 2) and (trim(GrdProps.Cells[1,1]) = '') then
  begin
    DelProps := PROPCNT;
    StartDelProps := 0;
  end
  else
  begin
    DelProps := PROPCNT - (GrdProps.RowCount-1);
    StartDelProps := GrdProps.RowCount-1;
  end;


  (* update list *)
  for i := 0 to GrdProps.RowCount-2 do
  begin
    prp.Loc := i;
    prp.Name := trim(GrdProps.Cells[1,i+1]);
    prp.Size := StrToInt(trim(GrdProps.Cells[2,i+1]));
    TileMap.SetPropDefLoc(i, prp);
  end;

  (* remove possible remains *)
  for i := 0 to DelProps do
  begin
    prp.Loc := -1;
    TileMap.SetPropDefLoc(StartDelProps, prp);
  end;


  (* property colors *)
  with PropColors.Colors[0] do
  begin
    Prop := integer(CmbRedFld.Items.Objects[CmbRedFld.ItemIndex]);
    Operator := CmbRedOp.ItemIndex;
    try Value := StrToInt(EdRedValue.Text) except Value := 0 end;
  end;

  with PropColors.Colors[1] do
  begin
    Prop := integer(CmbGreenFld.Items.Objects[CmbGreenFld.ItemIndex]);
    Operator := CmbGreenOp.ItemIndex;
    try Value := StrToInt(EdGreenValue.Text) except Value := 0 end;
  end;

end;




procedure TFrmTileProps.FormDestroy(Sender: TObject);
begin
  FLst.Free;
  FrmTileProps := nil;
end;

procedure TFrmTileProps.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmTileProps.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

procedure TFrmTileProps.BtnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;




procedure TFrmTileProps.EdPropColorChange(Sender: TObject);
var i : integer;
begin
  i := TComponent(Sender).Tag;
  TComponent(Sender).Tag := 0;
  ForceNumbers(TCustomEdit(Sender));
  TComponent(Sender).Tag := i;
end;

procedure TFrmTileProps.BtnAddClick(Sender: TObject);
begin
  InsertNewProp;
end;

procedure TFrmTileProps.BtnDelClick(Sender: TObject);
begin
  DeleteProp;
end;


procedure TFrmTileProps.RowCountChanged;
var i : integer;
begin
  with GrdProps do
  begin
    if (((RowCount) * DefaultRowHeight) > Height) then
      ColWidths[3] := 28
    else
      ColWidths[3] := 44;

    for i := 1 to RowCount-1 do Cells[0,i] := IntToStr(i);

    BtnAdd.Enabled := (RowCount < PROPCNT+2);
    BtnDel.Enabled := (RowCount > 2);
  end;
end;


function TFrmTileProps.ValidProps: boolean;
var i : integer;

  procedure ShowError( const s : string; r, c : integer);
  begin
    Msg(s, msgError, MB_OK);
    with GrdProps do
    begin
      Row := r;
      Col := c;
      SetFocus;
    end;
  end;

begin
  Result := True;

  with GrdProps do
    if not((RowCount <= 2) and (trim(Cells[1,1]) = '')) then
      for i:= 1 to RowCount-1 do
      begin
        Result := (trim(Cells[1,i]) <> '');
        if (not Result) then ShowError( '''Name'' is mandatory.', i, 1 )
        else
        begin
          Result := (Cells[2,i] <> '');
          if (not Result) then ShowError( '''Max'' is mandatory.', i, 2 )
          else
          begin
            Result := (SaveStrToInt(Cells[2,i]) <= (1024 * 64)-1);
            if (not Result) then ShowError( '''Max'' should be lower or equal to 65535.', i, 2 )
            else
            begin
              Result := (SaveStrToInt(Cells[2,i]) > 0);
              if (not Result) then ShowError( '''Max'' should be at least 1.', i, 2 );
            end;
          end;
        end;
        if (not Result) then exit;
      end;
end;


procedure TFrmTileProps.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOk) then
    if (not ValidProps) then
      CanClose := False
    else
    begin
      SaveProps;
      Modified := True;
    end;
end;

procedure TFrmTileProps.GrdPropsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
begin
  case ACol of
    1 : BuildColProps;
    2 : with GrdProps do
          Cells[3,ARow] := IntToStr( BitsNeeded( SaveStrToInt(Cells[2,ARow]) ) );
  end;
end;

procedure TFrmTileProps.GrdPropsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
    case Key of
      VK_INSERT :
        BtnAdd.Click;

      VK_DELETE :
        if BtnDel.Enabled then
        begin
          BtnDel.SetFocus;    (* change focus to force correct update of grid *)
          BtnDel.Click;
        end;
    end;
end;

end.
