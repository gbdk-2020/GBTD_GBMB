unit GBColorSetSelector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  gbconst, stdctrls, ColorController;

type


  TGBColorSetSelector = class(TCustomControl)
  private
    { Private declarations }
    FColorSet : PGBColorType;
    FSelColor : integer;

    FOnColorSelect : TOnColorSelectEvent;

    FColorSetIndex : integer;

    FGBCFilter     : boolean;

  protected
    { Protected declarations }

    procedure SetSelColor(i : integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ColorSelect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; clr: Integer);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;

    property ColorSet : PGBColorType read FColorSet write FColorSet;

  published
    { Published declarations }
    property SelColor : integer read FSelColor write SetSelColor;
    property OnColorSelect : TOnColorSelectEvent read FOnColorSelect write FOnColorSelect;
    property GBCFilter : boolean read FGBCFilter write FGBCFilter;

    property Height;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gameboy', [TGBColorSetSelector]);
end;

constructor TGBColorSetSelector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 19 * 4; //(20 * 4);
  Height := 19; //20;

  with Canvas.Font do
  begin
    Name := 'MS Sans Serif';
    Pitch := fpFixed;
    Size := 8;
  end;
end;


procedure TGBColorSetSelector.SetSelColor(i : integer);
begin
  if (i < -1) or (i > 3) then i := -1;

  if ( i <> FSelColor) then
  begin
    FSelColor := i;
    Paint;
  end;
end;


procedure TGBColorSetSelector.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var i : integer;
begin
  inherited;

  i := (x div Height); //19);//20);
  ColorSelect(Self, Button, Shift, i);
end;


procedure TGBColorSetSelector.ColorSelect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; clr: Integer);
begin
  if Assigned(FOnColorSelect) then FOnColorSelect(Sender, Button, Shift, clr);
end;


procedure TGBColorSetSelector.Paint;
const s : array[0..3] of string = ('0','1','2','3');
var i : integer;
begin

  if Assigned(FColorSet) and Assigned(GBColorController) then
  begin

    Canvas.Pen.Color := clBlack;

    for i := 0 to 3 do
      with Canvas do
      begin

        (* draw color *)
        if GBCFilter then
          Brush.Color := TranslateToGBCColor(FColorSet[i])
        else
          Brush.Color := FColorSet[i];

//        Rectangle((i*20),0,(i*20)+20,20);
        Rectangle((i*Height),0,(i*Height)+Height,Height);

        (* draw text *)
        if (((FColorSet[i] and $FF) < $20) and ((FColorSet[i] and $FF00) < $2000)) or
           (((FColorSet[i] and $FF) < $20) and ((FColorSet[i] and $FF0000) < $200000)) or
           (((FColorSet[i] and $FF00) < $2000) and ((FColorSet[i] and $FF0000) < $200000)) then
          Font.Color := clWhite
        else
          Font.Color := clBlack;
//        TextOut((i*Height)+6, 3, s[i]);
        TextOut((i*Height)+5, 2, s[i]);

        (* draw selected *)
        if (i = SelColor) then
        begin
          Brush.Color := clHighLight;
          FrameRect(Rect((i*Height)+1,1, (i*Height+(Height-1)), Height-1));
        end;
      end;
  end;
end;

end.
