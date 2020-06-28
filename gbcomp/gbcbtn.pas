unit gbcbtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons;



type
  TGBColorButton = class(TSpeedButton)
  private
    { Private declarations }
    FCaption : string;
    FColor   : TColor;

  protected
    { Protected declarations }

  public
    { Public declarations }
    procedure paint; override;

  published
    { Published declarations }
    property Caption : string read FCaption write FCaption;
    property Color : TColor read FColor write FColor;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gameboy', [TGBColorButton]);
end;


procedure TGBColorButton.Paint;
begin

 (* draw color *)
  with Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    Rectangle(0,0, Width, Height);
  end;

  (* draw text *)
  if (((FColor and $FF) < $20) and ((FColor and $FF00) < $2000)) or
     (((FColor and $FF) < $20) and ((FColor and $FF0000) < $200000)) or
     (((FColor and $FF00) < $2000) and ((FColor and $FF0000) < $200000)) then
    Canvas.Font.Color := clWhite
  else
    Canvas.Font.Color := clBlack;

  Canvas.TextOut(6,4, FCaption);
end;

end.
