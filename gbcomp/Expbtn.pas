unit ExpBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons;

type
  TExpButton = class(TSpeedButton)
  private
    { Private declarations }
    FBmpDormant  : TBitMap;
    FMainGlyph   : TBitMap;
    FKeepColor   : boolean;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;

    procedure SetMainGlyph( b : TBitmap );
    procedure BuildBitmaps;

  protected
    { Protected declarations }
    procedure Loaded; override;


  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure KludgeKeepColor;

  published
    { Published declarations }

    property KeepColor : boolean read FKeepColor write FKeepColor;
    property MainGlyph: TBitmap read FMainGlyph write SetMainGlyph;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Extensions', [TExpButton]);
end;

procedure TExpButton.SetMainGlyph( b : TBitmap );
begin
  FMainGlyph := b;

  BuildBitmaps;
end;

procedure TExpButton.CMMouseEnter(var Message: TMessage);
begin
  if Flat and Enabled and (Glyph <> FMainGlyph) and
     (not (csDesigning in ComponentState)) then Glyph := FMainGlyph;
  inherited;
end;


procedure TExpButton.CMMouseLeave(var Message: TMessage);
begin
  if Flat and Enabled and not Dragging and (Glyph <> FBmpDormant) and
  (not (csDesigning in ComponentState)) and not(Down and FKeepColor) then
   Glyph := FBmpDormant;

  inherited;
end;

procedure TExpButton.CMButtonPressed(var Message: TMessage);
begin
  inherited;

  if (Message.WParam = GroupIndex) and
     (not(csLoading in ComponentState)) then
  begin
    if (Down and FKeepColor) then
      Glyph := FMainGlyph
    else
      Glyph := FBmpDormant;
  end;
end;

procedure TExpButton.KludgeKeepColor;
begin
    if (Down and FKeepColor) then
      Glyph := FMainGlyph
    else
      Glyph := FBmpDormant;
end;

constructor TExpButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FBmpDormant := TBitmap.Create;
end;



destructor TExpButton.Destroy;
begin
  FBmpDormant.free;

  inherited Destroy;
end;


procedure TExpButton.BuildBitmaps;
var i,j   : integer;
    tc    : TColor;

begin
  if Assigned(FMainGlyph) and (not (csDesigning in ComponentState)) then
  begin

    with FMainGlyph do
    begin
      tc := Canvas.Pixels[0,0];
      TransparentColor := tc;
      FBmpDormant.Width := Width;
      FBmpDormant.Height := Height;

      for i := 0 to Width-1 do
        for j := 0 to Height-1 do
          if (Canvas.Pixels[i,j] = clBlack) or
             (Canvas.Pixels[i,j] = tc) then
            FBmpDormant.Canvas.Pixels[i,j] := Canvas.Pixels[i,j]
          else
            FBmpDormant.Canvas.Pixels[i,j] := clwhite;
    end;

    FBmpDormant.TransparentColor := tc;

    if (Down and FKeepColor) then
      Glyph := FMainGlyph
    else
      Glyph := FBmpDormant;
  end;
end;

procedure TExpButton.Loaded;
begin
  BuildBitmaps;

  inherited Loaded;
end;




end.
