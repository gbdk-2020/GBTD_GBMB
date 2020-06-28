unit GBColorCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GBColorSetSelector,gbconst, ColorController;

type
  TGBColorCombo = class(TCustomComboBox)
  private
    { Private declarations }
//    FColorSet : PGBColorType;


    FMainSel : TGBColorSetSelector;

    FDrawFocus : boolean;
    FAddDefault : boolean;

    FOnColorSelect : TOnColorSelectEvent;

    function GetColorSet : PGBColorType;
    procedure SetColorSet(c : PGBColorType);

  protected
    { Protected declarations }
    procedure Loaded; override;

    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure Change; override;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;

//    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;

//    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
//    procedure ColorSelect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; clr: Integer);
    procedure DropDown; override;

    function GetColorSelect: TOnColorSelectEvent;
    procedure SetColorSelect( cs : TOnColorSelectEvent);

    procedure SetSelColor(i : integer);
    function GetSelColor : integer;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ColorSet : PGBColorType read GetColorSet write SetColorSet;

    procedure RePaint; override;
    procedure FillDropDown;

  published
    { Published declarations }

    property SelColor : integer read GetSelColor write SetSelColor;
    property OnColorSelect : TOnColorSelectEvent read GetColorSelect write SetColorSelect;
    property AddDefault : boolean read FAddDefault write FAddDefault;

    property OnChange;
    property ItemIndex;
    property ItemHeight;
    property Hint;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gameboy', [TGBColorCombo]);
end;


constructor TGBColorCombo.Create(AOwner: TComponent);
const
  ComboBoxStyle = [csCaptureMouse, csDoubleClicks,
    csFixedHeight, csFixedWidth, csReflector];
begin
  inherited Create( AOwner);

  ControlStyle := ComboBoxStyle;
  Style := csOwnerDrawFixed;
  DropDownCount := 9;

  FMainSel := TGBColorSetSelector.Create(Self);
  with FMainSel do
  begin
    Parent := Self;
    Top := 1;
    Left := 1;
  end;
end;


destructor TGBColorCombo.Destroy;
begin
  FMainSel.Free;

  inherited Destroy;
end;


procedure TGBColorCombo.Loaded;
var i : integer;
begin
  inherited Loaded;

  Width := 95;
  Style := csOwnerDrawFixed;

  for i := 0 to 7 do items.add('');
end;



procedure TGBColorCombo.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  i : integer;
  c : TColor;
  hgt : integer;
begin

  if (Rect.right <> Width-2) then
  begin
    if (FAddDefault) and (ItemIndex = Items.Count-1) then
    begin
      FMainSel.Visible := False;
      Canvas.TextRect( Rect, 4, Rect.Top, 'Default' )
    end
    else
    begin
      FMainSel.Visible := True;
      FMainSel.Paint;
    end;
  end
  else
  if Assigned(GBColorController) and (Rect.right = Width-2) then
    with Canvas do
    begin

      hgt := ItemHeight+3;
      Pen.Color := clBlack;

      if (FAddDefault) and (Index = Items.Count-1) then
      begin
        (* place 'Default' *)
        TextRect( Rect, 4, Rect.Top, 'Default' );
      end
      else
      begin
        (* draw blocks *)
        for i := 0 to 3 do
        begin
          (* draw color *)
          Brush.Color := GBColorController.CurColorSets[0][Index][i];
          Rectangle(Rect.Left + (i*hgt),Rect.Top, Rect.Left + (i*hgt)+hgt, Rect.Bottom);
        end;

        (* draw focus *)
        if FDrawFocus then c := clHighlight else c := clWhite;
        Brush.Color := c;
        Pen.Color   := c;
        Rectangle(Rect.Left + (4*hgt),Rect.Top, Rect.Right, Rect.Bottom);
      end;


      FDrawFocus := False;
    end;
end;


procedure TGBColorCombo.CNDrawItem(var Message: TWMDrawItem);
begin
  with Message.DrawItemStruct^ do
  begin
    if (itemState and ODA_FOCUS <> 0) then begin
      itemState := itemState and not ODA_FOCUS;
      FDrawFocus := True;
    end;
    inherited;
  end;
end;


procedure TGBColorCombo.Change;
begin
  FMainSel.ColorSet := @GBColorController.CurColorSets[0][ItemIndex];
  inherited Change;
  FMainSel.Refresh;
end;


procedure TGBColorCombo.CNCommand(var Message: TWMCommand);
begin
  inherited;
  FMainSel.Paint;
end;



procedure TGBColorCombo.RePaint;
begin
  FMainSel.ColorSet := @GBColorController.CurColorSets[0][ItemIndex];
  FMainSel.Height  := ItemHeight+3;
  FMainSel.Width   := (ItemHeight+3) * 4;
  inherited;
  FMainSel.Refresh;
end;



function TGBColorCombo.GetColorSet : PGBColorType;
begin
  Result := FMainSel.ColorSet;
end;

procedure TGBColorCombo.SetColorSet(c : PGBColorType);
begin
  FMainSel.ColorSet := c;
  FMainSel.Paint;
end;

function TGBColorCombo.GetColorSelect: TOnColorSelectEvent;
begin
  Result := FMainSel.OnColorSelect;
end;

procedure TGBColorCombo.SetColorSelect( cs : TOnColorSelectEvent);
begin
  FMainSel.OnColorSelect := cs;
end;

procedure TGBColorCombo.SetSelColor(i : integer);
begin
  FMainSel.SelColor := i;

end;

function TGBColorCombo.GetSelColor : integer;
begin
  Result := FMainSel.SelColor;
end;

procedure TGBColorCombo.FillDropDown;
var i,j,k : integer;
begin
  k := ItemIndex;

  case GBColorController.ColorMode of
    gbcmPocket,
    gbcmGameboy     : j := 0;
    gbcmSGB         : if FAddDefault then j := 4 else j := 3;
    gbcmGBC,
    gbcmGBCFiltered : if FAddDefault then j := 8 else j := 7;
    else              exit;
  end;
  Items.Clear;
  for i := 0 to j do items.add('');
  ItemIndex :=k;
end;


procedure TGBColorCombo.DropDown;
begin
  FillDropDown;
end;


end.
