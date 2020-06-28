unit GBCartList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls;

type
  TGBCartList = class(TCustomControl)
  private
    { Private declarations }
    FLstCart : TList;
    FBmpGBC  : TBitmap;
    FBmpSGB  : TBitmap;
    FScroll  : TScrollBar;

    FFlip    : TBitmap;

    FSelected : integer;

    FTop       : integer;
    FTopOffset : integer;

    FBackGround : TBitmap;
    FBackColor  : TColor;

    procedure ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure SetLstCart( lst : TList);
    procedure SetTop(i : integer);
    procedure SetSelected( i : integer );

    function GetItemsOnScreen : integer;

    procedure OnBackgroundChange(Sender: TObject);

  protected
    { Protected declarations }

    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;

//    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
//    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
//    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;


//    procedure SetBackground( b : TBitmap);



    property Top : integer read FTop write SetTop;
    property ItemsOnScreen: integer read GetItemsOnScreen;

  public
    { Public declarations }
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;

    property LstCart : TList read FLstCart write SetLstCart;
    property Selected : integer read FSelected write SetSelected;

    property BackGround : TBitmap read FBackGround;

  published
    { Published declarations }
    property TabStop;
    property taborder;
  end;

procedure Register;

implementation

{$RESOURCE GBCartIcons.res}


uses CartInfo;

procedure Register;
begin
  RegisterComponents('Gameboy', [TGBCartList]);
end;


constructor TGBCartList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

    FBmpGBC := TBitMap.Create;
  FBmpGBC.LoadFromResourceName(HInstance, 'GBCARTICONGBC');

  FBackGround := TBitmap.Create;
  FBackground.OnChange := OnBackgroundChange;

end;


destructor TGBCartList.Destroy;
begin
  FScroll.Free;

  FFlip.Free;
  FBackGround.Free;
  FBmpGBC.Free;


  inherited Destroy;
end;


procedure TGBCartList.Loaded;
begin
  inherited Loaded;

  ControlStyle := [csFramed, csDoubleClicks];


  FFlip := TBitmap.Create;
  FFlip.Width := Self.Width-2;
  FFlip.Height := Self.Height-2;


  FScroll := TScrollBar.Create(Self);
  with FScroll do
  begin
    Parent   := Self;
    Visible  := False;
    Kind     := sbVertical;
    Ctl3D    := False;
    Height   := Self.Height-2;
    Top      := 1;
    TabStop  := False;
    LargeChange := ItemsOnScreen-1;
    OnScroll := ScrollEvent;
  end;

end;

procedure TGBCartList.OnBackgroundChange(Sender: TObject);
begin
  FBackColor := FBackGround.Canvas.Pixels[0,0];
  Paint;
end;


procedure TGBCartList.ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Top := ScrollPos;
end;

procedure TGBCartList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP   : Selected := Selected - 1;
    VK_DOWN : Selected := Selected + 1;
  end;

  Key := 0;
end;


procedure TGBCartList.CNKeyDown(var Message: TWMKeyDown);
begin
//  message.Result := 0;
//  Key := 0;
end;




procedure TGBCartList.SetTop(i : integer);
begin
  if Assigned(FLstCart) and (i <> FTop) then
  begin
    if (i >= FLstCart.Count) then
      FTop := FLstCart.Count-1
    else
      if (i < 0) then
        FTop := 0
      else
        FTop := i;

    FScroll.Position := FTop;
    Paint;
  end;
end;


procedure TGBCartList.SetSelected(i : integer);
begin

  if Assigned(FLstCart) then
  begin
    if (i >= FLstCart.Count) then FSelected := FLstCart.Count-1 else
    if (i < 0) then FSelected := 0 else FSelected := i;

    (* move top to sync display *)
    if (FSelected < Top) then
      Top := FSelected
    else
      if (FSelected > (Top + ItemsOnScreen)) then
      begin
        Top := FSelected - ItemsOnScreen;
      end
      else
        Paint;
  end;
end;




procedure TGBCartList.SetLstCart( lst : TList);
begin
  FLstCart := lst;
  if (Lst.Count > 1) then
    FScroll.max := Lst.Count-1;
end;

function TGBCartList.GetItemsOnScreen : integer;
begin
  Result := (Height div - (Canvas.Font.Height-3));
end;


procedure TGBCartList.Paint;
var i : integer;
    Hgt : integer;
    x,y  : integer;
    cnt  : integer;
    tp,btm : integer;

    bmp : TBitmap;

    bw,bh,bx,by,rx,ry : integer;
begin

  with FFlip.Canvas do
  begin

    if (FBackGround.Width > 0) and (FBackGround.Height > 0) then
    begin

      if (Width > FBackground.Width) or (Height > FBackground.Height) then
      begin
        Pen.Color   := FBackColor;
        Brush.Color := FBackColor;
        Brush.Style := bsSolid;
        rectangle(0,0,Width, Height);
      end;

      if (Width >= FBackground.Width) then
      begin
        bw := FBackGround.Width;
        bx := 0;
        rx := (Width - bw) div 2;
      end
      else
      begin
        bw := Width;
        rx := 0;
        bx := (FBackGround.Width - bw) div 2;
      end;

      if (Height >= FBackground.Height) then
      begin
        bh := FBackGround.Height;
        by := 0;
        ry := (Height - bh) div 2;
      end
      else
      begin
        bh := Height;
        ry := 0;
        by := (FBackGround.Height - bh) div 2;
      end;


      CopyRect( rect(rx,ry, bw+rx, bh+ry), FBackGround.Canvas, rect(bx,by, bw+bx, bh+by) );



    end
    else
    begin
      (* clear flip *)
      Pen.Color   := clWhite;
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
      rectangle(0,0,Width, Height);
    end;
  end;



  if assigned(LstCart) and (LstCart.Count > 0) then
  with FFlip.Canvas do
  begin

    (* init *)
    Pen.Color := clBlack;
    Brush.Style := bsClear;

    Hgt := -(Canvas.Font.Height-3)+1;
    y := FTopOffset + 1;

    cnt := ItemsOnScreen;
    if (cnt > LstCart.Count-1) then
    begin
      cnt := LstCart.Count-1;
      FScroll.Visible := False;
    end
    else
    begin
      if not FScroll.visible then
      begin
        FScroll.Left := Self.Width-FScroll.Width-1;
        FScroll.Visible := True;
        FScroll.refresh;
      end;
    end;

    tp := Top;
    if ((tp + cnt) > LstCart.Count-1) then
      cnt := (LstCart.Count-1)-tp;




    for i := 0 to cnt do
    begin
      x := 2;

      if ((tp+i) = Selected) then
      begin
        Pen.Color := clHighlight;
        Brush.Color := clHighlight;
        Brush.Style := bsSolid;
        Rectangle(2, y+(i*Hgt), Width, y+(i*Hgt)+Hgt-1);
        Brush.Style := bsClear;

        Font.Color := clHighlightText;
      end
      else
        Font.Color := clBlack;



      if TCartInfo(LstCart.items[tp+i]).GBC then
        BrushCopy( Rect(x, y+(i*Hgt), x+12, y+(i*Hgt)+12), FBmpGBC, Rect(0,0,12,12), clwhite  );
      x := 2 + 12 + 2;


      TextOut(x, y+(i*Hgt), TCartInfo(LstCart.items[tp+i]).Name);


    end;


  end;

  with Canvas do
  begin
    MoveTo(0,0);
    LineTo(0,Height-1);
    LineTo(Width-1,Height-1);
    LineTo(Width-1,0);
    LineTo(0,0);

//    Draw(1,1, FFlip);
    if FScroll.visible then
      CopyRect( rect(1,1, Width-FScroll.Width-1, Height), FFlip.Canvas, rect(0,0, Width-FScroll.Width-1, Height) )
    else
      CopyRect( rect(1,1, Width, Height), FFlip.Canvas, rect(0,0, Width, Height) );

  end;

end;



procedure TGBCartList.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var i : integer;
begin

  i := (y-FTopOffset) div (-(Canvas.Font.Height-3)+1);
  Selected := i+Top;

end;


end.
