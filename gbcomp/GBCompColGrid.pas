unit GBCompColGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  gbConst, GBCCBitmap, ShadowCanvas;

type
  TGBCompColGrid = class(TCustomControl)
  private
    { Private declarations }

    FBitmap : TGBCCBitmap;
    FPixelSize     : integer;




  protected
    { Protected declarations }

  public
    { Public declarations }
    property Bitmap : TGBCCBitmap read FBitmap write FBitmap;

    procedure Paint; override;


  published
    { Published declarations }

    property PixelSize : integer read FPixelSize write FPixelSize;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gameboy', [TGBCompColGrid]);
end;


procedure TGBCompColGrid.Paint;
var SC : TShadowCanvas;
  i,j : integer;
begin
  if Assigned(FBitmap) and (FBitmap.Width > 0) and (FBitmap.Height > 0) then
  begin

    SC := TShadowCanvas.Create(FBitmap.Width * FPixelSize, FBitmap.Height * FPixelSize);
    try

      for j := 0 to FBitmap.Height-1 do
        for i := 0 to FBitmap.Width-1 do
          SC.RectangleSized( (i * FPixelSize), (j * FPixelSize), FPixelSize, FPixelSize, GetShadowColor(FBitmap.Colors[i,j]));
//          SC.SetPixel(i,j, GetShadowColor(FBitmap.Colors[i,j]));


      SC.Draw(Canvas.Handle, 0, 0);

    finally
      SC.Free;
    end;
  end;
end;

end.
