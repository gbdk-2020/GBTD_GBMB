(**************************     TGammaPanel     *******************************

 TGammaPanel is a Simple Color Panel like a Paint Shop Pro Color Panel
 
 This component can be freely used and distributed in commercial and private
 environments, provided this notice is not modified in any way.
 Feel free to contact us if you have any questions, comments or suggestions
 at TRSOFT@Menden.net

    Copyright © 1998 by TRSOFT  All Rights Reserved.
    Thomas Radtke Software Entwicklung.
    http://www.abcnet.de/TRSOFT/

  THIS SOFTWARE IS PROVIDED AS IS AND WITHOUT WARRANTY OF ANY KIND,
  EITHER EXPRESSED OR IMPLIED.

------------------------------- History --------------------------------------
 Ver 1.0 10.02.98
         Main Component Build.

 Ver 2.0 08.03.98
         OnChange property added.

 Ver 2.1 09.03.98
         FirstMouseMove and SecondMouseMove added.
         FSwapLabel.OnMouseMove added.
         New Cursor added.
         FImagePanel OnMouseMove is now AllMouseMove. (Previous
                Version was ImgMouseMove - not good...)
------------------------------------------------------------------------------
******************************************************************************)

unit GammaPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MainLib, ColorController;

type
  TGammaPanel = class(TCustomPanel)
  private
     FImagePanel    : TPanel;
     FStatePanel    : TPanel;
     FControlPanel  : TPanel;
     FViewWindow    : TPanel;
     RLabel         : TLabel;
     REd            : TEdit;
     GLabel         : TLabel;
     GEd            : TEdit;
     BLabel         : TLabel;
     BEd            : TEdit;
     FFirst         : TShape;
     FGamma         : TImage;
     FFirstColor    : TColor;
     FOnChange      : TNotifyEvent;
     PipCursor      : TCursor;
     FLastRGB       : integer;

     Freezed        : boolean;

     FSuppressChanges : boolean;
     FGBCFilter     : boolean;

     FBmpSelector   : TBitmap;
     FClrSelector   : TColor;

     procedure SetFirstColor(Value : TColor);
     procedure UpdateColor;
     procedure ChangeColor( c : TColor);

     procedure EdChange(Sender: TObject);
     procedure EdKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

     procedure SetGBCFilter( b : boolean);

     procedure SetViewColor( c : TColor );
     procedure SetFirstControls( c : TColor);

  protected
     procedure Change;
     procedure GammaClick(Sender: TObject);
     procedure FirstMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
     procedure AllMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
     procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
     procedure FirstMouseDown(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
     procedure CMColorChanged (var Message: TMessage); message CM_COLORCHANGED;
     procedure WMSize(var Message: TWMSize); message WM_SIZE;

     property ViewColor : TColor read FClrSelector write SetViewColor;

  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;



  published
     property FirstColor : TColor read FFirstColor write SetFirstColor default clBlack;
     property Font;
     property BevelInner;
     property BevelOuter;
     property BevelWidth;
     property Enabled;
     property Color;
     property ParentColor;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ShowHint;
     property Visible;
     property OnClick;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
     property OnChange: TNotifyEvent read FOnChange write FOnChange;
     property LastRGB : integer read FLastRGB write FLastRGB;

     property SuppressChanges : boolean read FSuppressChanges write FSuppressChanges;
     property GBCFilter : boolean read FGBCFilter write SetGBCFilter;

  end;


procedure Register;

implementation

{$R GammaPanel}
{$R GammaP2}

const MyCursor = 1;
      FULLWIDTH = 53;
      FULLHEIGHT = 230;

procedure Register;
begin
  RegisterComponents('Gameboy', [TGammaPanel]);
end;

constructor TGammaPanel.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FBmpSelector := TBitmap.Create;
  FBmpSelector.LoadFromResourceName(HInstance,'GAMMA');


  ControlStyle := [csCaptureMouse,csClickEvents,csOpaque,csFixedWidth,csFixedHeight];
  Width  := FULLWIDTH;
  Height := FULLHEIGHT;
  Visible:=True;
  Enabled := True;
  Align :=alNone;
  BevelInner:=bvRaised;
  BevelOuter:=bvLowered;
  BevelWidth:=1;
  BorderStyle:=bsNone;
  BorderWidth:=0;
  Screen.Cursors[MyCursor] := LoadCursor(hInstance, pChar('PIP'));
  PipCursor:=MyCursor;
  Color:=clBtnFace;
  Font.Name:='MS Serif';
  Font.Size:=7;
  FFirstColor:=clBlack;

  FImagePanel:= TPanel.Create(Self);
  FImagePanel.Parent:=Self;
  FImagePanel.SetBounds(2, 2, 49, 100);
  FImagePanel.Align :=alNone;
  FImagePanel.BevelInner:=bvLowered;
  FImagePanel.BevelOuter:=bvNone;
  FImagePanel.BevelWidth:=1;
  FImagePanel.BorderStyle:=bsNone;
  FImagePanel.BorderWidth:=0;
  FImagePanel.Color:=Color;
  FImagePanel.Caption:='';
  FImagePanel.Cursor:=PipCursor;
  FImagePanel.PopupMenu:=PopupMenu;
  FImagePanel.Visible:=True;
  FImagePanel.Enabled := True;
  FImagePanel.OnMouseMove :=AllMouseMove;

  FStatePanel:= TPanel.Create(Self);
  FStatePanel.Parent:=Self;
  FStatePanel.SetBounds(2, 136, 49, 80+12);
  FStatePanel.Align :=alNone;
  FStatePanel.BevelInner:=bvLowered;
  FStatePanel.BevelOuter:=bvNone;
  FStatePanel.BevelWidth:=1;
  FStatePanel.BorderStyle:=bsNone;
  FStatePanel.BorderWidth:=0;
  FStatePanel.ParentColor:=True;
  FStatePanel.Color:=Color;
  FStatePanel.Caption:='';
  FStatePanel.Cursor:=crDefault;
  FStatePanel.PopupMenu:=PopupMenu;
  FStatePanel.Visible:=True;
  FStatePanel.Enabled := True;

  FControlPanel:= TPanel.Create(Self);
  FControlPanel.Parent:=Self;
  FControlPanel.SetBounds(2, 104, 49, 30);
  FControlPanel.Align :=alNone;
  FControlPanel.BevelInner:=bvLowered;
  FControlPanel.BevelOuter:=bvNone;//bvRaised;
  FControlPanel.BevelWidth:=1;
  FControlPanel.BorderStyle:=bsNone;
  FControlPanel.BorderWidth:=0;
  FControlPanel.ParentColor:=True;
  FControlPanel.Color:=Color;
  FControlPanel.Caption:='';
  FControlPanel.Cursor:=crDefault;
  FControlPanel.PopupMenu:=PopupMenu;
  FControlPanel.Visible:=True;
  FControlPanel.Enabled := True;

  FViewWindow:= TPanel.Create(Self);
  FViewWindow.Parent:=FStatePanel;
  FViewWindow.SetBounds(3, 44+14, 42, 31);
  FViewWindow.Align :=alNone;
  FViewWindow.Alignment:=taCenter;
  FViewWindow.BevelInner:=bvNone;
  FViewWindow.BevelOuter:=bvLowered;
  FViewWindow.BevelWidth:=1;
  FViewWindow.BorderStyle:=bsNone;
  FViewWindow.BorderWidth:=0;
  ViewColor := clGray;
  FViewWindow.Caption:='None';
  FViewWindow.Cursor:=crDefault;
  FViewWindow.PopupMenu:=PopupMenu;
  FViewWindow.Visible:=True;
  FViewWindow.Enabled := True;

  REd := TEdit.Create(Self);
  with REd do
  begin
    SetBounds(24, 3, 18, 5);
    Parent:=FStatePanel;
    Align :=alNone;
    Alignment:=taLeftJustify;
    AutoSize:=False;
    MaxLength := 2;
    ParentColor:=True;
    Color:=clWindow;
    Font:=Font;
    Cursor:=crDefault;
    Visible:=True;
    Enabled := True;
    OnChange := EdChange;
    OnKeyUp := EdKeyUp;
  end;

  RLabel:= TLabel.Create(Self);
  with RLabel do
  begin
    SetBounds(7, 3+3, 21, 11);
    Parent:=FStatePanel;
    Align :=alNone;
    Alignment:=taLeftJustify;
    AutoSize:=True;
    ParentColor:=True;
    FocusControl := REd;
    Color:=Color;
    Font:=Font;
    Caption:='&R';
    Cursor:=crDefault;
    Visible:=True;
    Enabled := True;

  end;

  GEd := TEdit.Create(Self);
  with GEd do
  begin
    SetBounds(24, 21, 18, 5);
    Parent:=FStatePanel;
    Align :=alNone;
    Alignment:=taLeftJustify;
    AutoSize:=False;
    MaxLength := 2;
    ParentColor:=True;
    Color:=clWindow;
    Font:=Font;
    Cursor:=crDefault;
    Visible:=True;
    Enabled := True;
    OnChange := EdChange;
    OnKeyUp := EdKeyUp;
  end;

  GLabel:= TLabel.Create(Self);
  with GLabel do
  begin
    SetBounds(7, 21+3, 21, 11);
    Parent:=FStatePanel;
    Align :=alNone;
    Alignment:=taLeftJustify;
    AutoSize:=True;
    ParentColor:=True;
    FocusControl:=GEd;
    Color:=Color;
    Font:=Font;
    Caption:='&G';
    Cursor:=crDefault;
    Visible:=True;
    Enabled := True;
  end;

  BEd := TEdit.Create(Self);
  with BEd do
  begin
    SetBounds(24, 39, 18, 5);
    Parent:=FStatePanel;
    Align :=alNone;
    Alignment:=taLeftJustify;
    AutoSize:=False;
    MaxLength := 2;
    ParentColor:=True;
    Color:=clWindow;
    Font:=Font;
    Cursor:=crDefault;
    Visible:=True;
    Enabled := True;
    OnChange := EdChange;
    OnKeyUp := EdKeyUp;
  end;


  BLabel:= TLabel.Create(Self);
  with BLabel do
  begin
    SetBounds(7, 39+3, 21, 11);
    Parent:=FStatePanel;
    Align :=alNone;
    Alignment:=taLeftJustify;
    AutoSize:=True;
    ParentColor:=True;
    FocusControl:=BEd;
    Color:=Color;
    Font:=Font;
    Caption:='&B';
    Cursor:=crDefault;
    Visible:=True;
    Enabled := True;
  end;

  FFirst:= TShape.Create(Self);
  with FFirst do
  begin
    Parent:=FControlPanel;
    SetBounds(1, 1, 47, 28);
    SetFirstControls(FFirstColor);
    Visible:=True;
    Enabled := True;
    OnMouseDown := FirstMouseDown;
    OnMouseMove := FirstMouseMove;
  end;

  FGamma:= TImage.Create(Self);
  with FGamma do
  begin
    SetBounds(0, 0 , 48, 98);
    Parent:=FImagePanel;
    Align:=alClient;
    Stretch:=False;
    AutoSize:=True;
    Center:=True;
    Cursor:=PipCursor;
    Visible:=True;
    Enabled := True;
    OnClick:= GammaClick;
    OnMouseMove :=ImgMouseMove;
  end;

end;

destructor TGammaPanel.Destroy;
begin
  FGamma.Free;
  FFirst.Free;
  BEd.Free;
  BLabel.Free;
  GEd.Free;
  GLabel.Free;
  REd.Free;
  RLabel.Free;
  FControlPanel.Free;
  FViewWindow.Free;
  FStatePanel.Free;
  FImagePanel.Free;
  FBmpSelector.Free;

  inherited Destroy;
end;



procedure TGammaPanel.SetGBCFilter( b : boolean);
begin
  FGBCFilter := b;

  if b then
    FGamma.Picture.Bitmap.LoadFromResourceName(HInstance,'GBCGAMMA')
  else
    FGamma.Picture.Bitmap := FBmpSelector;
end;

procedure TGammaPanel.SetViewColor( c : TColor );
begin
  FClrSelector := c;
  if FGBCFilter then
    FViewWindow.Color := TranslateToGBCColor( FClrSelector )
  else
    FViewWindow.Color := FClrSelector;
end;


procedure TGammaPanel.AllMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Freezed := True;
  REd.Text := '';
  GEd.Text := '';
  BEd.Text := '';
  Freezed := False;
  UpdateColor;
end;

procedure TGammaPanel.ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var c : TColor;
var R,G,B : integer;
begin
  FViewWindow.Caption:='';
  c := FBmpSelector.Canvas.Pixels[x,y];
  ChangeColor(c);
end;



procedure TGammaPanel.ChangeColor( c : TColor);
var
  R,G,B : integer;
begin
  R := GetRValue(ColorToRGB(c));
  G := GetGValue(ColorToRGB(c));
  B := GetBValue(ColorToRGB(c));

  Freezed := True;
  REd.Text :=IntToStr(R shr 3);
  GEd.Text :=IntToStr(G shr 3);
  BEd.Text :=IntToStr(B shr 3);
  Freezed := False;
  UpdateColor;
end;


procedure TGammaPanel.UpdateColor;
var Ok : boolean;
var R,G,B : integer;
begin

  Ok := True;
  try
    R := StrToInt(REd.Text);
    G := StrToInt(GEd.Text);
    B := StrToInt(BEd.Text);
  except
    Ok := False;
  end;

  if Ok then
  begin
    FViewWindow.Caption:='';
    ViewColor           := RGB(R shl 3,G shl 3,B shl 3);
  end
  else
  begin
    FViewWindow.Caption := 'None';
    ViewColor           := clGray;
  end;

end;


procedure TGammaPanel.GammaClick(Sender: TObject);
begin
  FFirstColor := ViewColor;
  SetFirstControls( ViewColor );
  if Assigned(FOnChange) then FOnChange(Self);
end;


procedure TGammaPanel.FirstMouseDown(Sender: TObject;
                Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGammaPanel.FirstMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FViewWindow.Caption:='';
  ViewColor := FFirst.Brush.Color;
  REd.Text := IntToStr(GetRValue(ColorToRGB(FFirst.Brush.Color)) shr 3);
  GEd.Text := IntToStr(GetGValue(ColorToRGB(FFirst.Brush.Color)) shr 3);
  BEd.Text := IntToStr(GetBValue(ColorToRGB(FFirst.Brush.Color)) shr 3);
end;


procedure TGammaPanel.CMFontChanged (var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TGammaPanel.WMSize (var Message: TWMSize);
begin
  inherited;
  Width :=  FULLWIDTH;
  Height := FULLHEIGHT;
  Invalidate;
end;

procedure TGammaPanel.CMColorChanged (var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TGammaPanel.SetFirstColor(Value : TColor);
begin
  FFirstColor:=Value;
  SetFirstControls( FFirstColor );
  ChangeColor(FFirstColor);
  if Assigned(FOnChange) then FOnChange(Self);
end;


procedure TGammaPanel.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TGammaPanel.EdChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;


procedure TGammaPanel.EdKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  with TEdit(Sender) do
    if ((Text <> '') and (StrToInt(Text) > 31)) then TEdit(Sender).Text := '31';

  if not Freezed then UpdateColor;

  if ( FViewWindow.Caption = '') then
  begin
    FFirstColor        := ViewColor;
    SetFirstControls(ViewColor);
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TGammaPanel.SetFirstControls( c : TColor);
begin
  if FGBCFilter then
  begin
    FFirst.Brush.Color := TranslateToGBCColor(c);
    FFirst.Pen.Color   := TranslateToGBCColor(c);
  end
  else
  begin
    FFirst.Brush.Color := c;
    FFirst.Pen.Color   := c;
  end;
end;

end.
