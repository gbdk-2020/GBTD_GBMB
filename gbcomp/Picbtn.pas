{*****************************************************************************}
{                                                                             }
{  file:      PicBtn.PAS                                                      }
{  title:     Picture Button                                                  }
{  version:   2.0.32                                                          }
{  date:      2/10/98                                                         }
{  author:    Andreas Heckel                                                  }
{             Mail: andreas.heckel@wirtschaft.tu-ilmenau.de                   }
{  copyright: DELPHI STORAGE                                                  }
{             Web: http://www.wirtschaft.tu-ilmenau.de/~aeg/                  }
{                                                                             }
{  compiler:  Borland DELPHI 2.0                                              }
{  descript.: DELPHI 2.0 Visual Component                                     }
{                                                                             }
{  revision history:                                                          }
{             4/23/97    first release                                        }
{                                                                             }
{  Category: FREEWARE - demo and source included)                             }
{  Can you please put a link to the site in your list and not just the zip    }
{*****************************************************************************}

unit PicBtn;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms,DsgnIntf,ExtCtrls,StdCtrls,MyColor,propintf;

CONST  MoveTimer    =1;
       RepeateTimer =2;

type

  TFocusDrawEvent = procedure (Sender: TObject;aCanvas:TCanvas;var aRect:TRect) of object;

  TPicBtn = class(TCustomControl)
  private
    { Private-Deklarationen }
    FMouseIn       : Boolean;
    FMonitoring    : Boolean;
    FMasked        : Boolean;
    FCommand       : TNotifyEvent;
    FOnMouseEnter  : TNotifyEvent;
    FOnMouseExit   : TNotifyEvent;
    FOnDrawFocus   : TFocusDrawEvent;
    FFocusColor    : TColor;
    FDrawFocus     : Boolean;
    FFirstWait     : Integer;
    FRepeate       : Integer;
    NextTime       : LongInt;
    FBtnState      : Integer;
    FNumStates     : Integer;
    FsActive       : Integer;
    FsInActive     : Integer;
    FsDisabled     : Integer;
    FsDown         : Integer;

    PROCEDURE SetActive      (Value:Integer);

    PROCEDURE SetsActive     (Value:Integer);
    PROCEDURE SetFsInActive  (Value:Integer);
    PROCEDURE SetFsDisabled  (Value:Integer);
    PROCEDURE SetFsDown      (Value:Integer);
    PROCEDURE SetNumStates   (Value:Integer);

    PROCEDURE SetNextState(Value:Integer);
    PROCEDURE SetFocusColor(Value:TColor);
    PROCEDURE SetMasked(Value:Boolean);
    PROCEDURE SetMonitoring(Value:Boolean);

    PROCEDURE WMWINDOWPOSCHANGED(var Message: TMessage);message WM_WINDOWPOSCHANGED;
    PROCEDURE WMKeyDown(var Message: TWMKeyDown);       message WM_KEYDOWN;
    PROCEDURE WMKeyUp(var Message: TWMKeyUp);           message WM_KEYUP;
    PROCEDURE WMKeyPress(var Message: TWMChar);         message WM_Char;
    PROCEDURE WMLButtonDblclk(VAR Msg: TMessage);       message wm_LButtonDblclk;
    PROCEDURE wmLButtonDown(VAR Msg: TMessage);         message wm_LButtonDown;
    PROCEDURE WMLButtonUp(VAR Msg: TMessage);           message wm_LButtonUp;
    PROCEDURE WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    PROCEDURE WMNCHITTEST(var Message: TMessage);       message WM_NCHITTEST;


    PROCEDURE WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    PROCEDURE WMSetFocus(var Message: TWMSetFocus);     message WM_SETFOCUS;
    PROCEDURE WMKillFocus(var Message: TWMKillFocus);   message WM_KILLFOCUS;
    PROCEDURE WindowProc(Var Message:TMessage); { Timer callback }
  protected
    { Protected-Deklarationen }
    Handle                    :HWND;
    BTNParentForm             :TForm;
    fOldX,fOldY,fOldSX,fOldSY :Integer;
    fNextState                :Integer;
    PROCEDURE   DoDrawFocus(Sender: TObject;aCanvas:TCanvas;Var aRect:TRect);
    Procedure   Paint;override;
    PROCEDURE   RepTimer;
    PROCEDURE   MovTimer;
    PROCEDURE CreateWnd; override;
    FUNCTION  GetPalette: HPALETTE; override;
  public
    { Public-Deklarationen }
    FMaskBMP       : TBitmap;
    FocusRect      : TRect;
    NumCols        : Integer;
    fMaskedPicture : TMaskedPicture;
    Constructor Create(AOwner:TComponent); override;
    Destructor  Destroy; override;
    procedure ReColor;
    procedure Click; override;
    procedure StyleChanged(Sender: TObject);
  published
    { Published-Deklarationen }
    property MaskedPicture :TMaskedPicture read FMaskedPicture write FMaskedPicture;
    property DrawFocus     :Boolean read fDrawFocus        write fDrawFocus;
    property BtnState      :Integer read FBtnState         write SetActive;
    property NextState     :Integer read FNextState        write SetNextState;
    property NumStates     :Integer read FNumStates        write SetNumStates;
    property sActive       :Integer read FsActive          write SetsActive;
    property sInActive     :Integer read FsInActive        write SetFsInActive;
    property sDisabled     :Integer read FsDisabled        write SetFsDisabled;
    property sDown         :Integer read FsDown            write SetFsDown;
    property Masked        :Boolean read fMasked           write SetMasked;
    property Monitoring    :Boolean read fMonitoring       write SetMonitoring;

    property FocusColor    :TColor          read FFocusColor   write SetFocusColor;
    property OnCommand     :TNotifyEvent    read FCommand      write FCommand;
    property OnDrawFocus   :TFocusDrawEvent read FOnDrawFocus  write FOnDrawFocus;
    property OnMouseEnter  :TNotifyEvent    read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit   :TNotifyEvent    read FOnMouseExit  write FOnMouseExit;
    property FirstWait     :Integer         read FFirstWait    write FFirstWait;
    property Repeate       :Integer         read FRepeate      write FRepeate;
    property Enabled;
    property ParentFont;
    property ParentShowHint;
    property Hint;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;



procedure Register;

implementation


PROCEDURE DoFocus(Parent,Sender:TWinControl;Next:Boolean);
var l,tabList:TList;
    I,P,N,F:Integer;
    Control: TWinControl;
begin
  if Parent=nil then exit;
  if Sender=nil then exit;
  l:=tList.create;
  TabList:=tList.create;
  try
    F:=-1;
    Parent.gettabOrderList(l);
    for I:=0 to l.count-1 do
    begin
      Control:=l[I];
      if (Control.CanFocus)and(Control.TabStop)or(Control=Sender) then TabList.Add(control);
    end;
    for I:=0 to TabList.count-1 do
    begin
      Control:=TabList[I];
      if Control=Sender then F:=I;
    end;
    if F=TabList.count-1 then N:=0 else N:=F+1;
    if F=0 then P:=TabList.count-1 else P:=F-1;
    if not((F=N)or(F=P)or(F=-1))then
    begin
      if Next then Control:=TabList[N] else Control:=TabList[P];
      Control.SetFocus;
    end;
  finally
    TabList.free;
    l.free;
  end;
end;


CONSTRUCTOR TPicBtn.Create(AOwner:TComponent);
begin
  inherited create(aOwner);
  fOnDrawFocus:=DoDrawFocus;
  fMaskedPicture:=TMaskedPicture.Create;
  fMaskedPicture.OnChange:=StyleChanged;
  fOldX:=0;fOldY:=0;fOldSX:=0;fOldSY:=0;
  FMaskBMP:=TBitmap.Create;
  fMasked:=False;
  ControlStyle := [csCaptureMouse, csFixedWidth, csFixedHeight, csClickEvents, csDoubleClicks];
  FFocusColor:=clBlack;
  Handle:=AllocateHWnd(WindowProc);
  fDrawFocus:=True;
  FocusRect := Rect(0,0,0,0);
  FFirstWait:=250;
  FRepeate  :=55;
  NextTime  :=-1;
  FNumStates:=4;
  FsActive  :=1;
  FsInActive:=2;
  FsDisabled:=3;
  FsDown    :=4;
  FBtnState :=1;
  FMonitoring:=False;
end;

Destructor TPicBtn.Destroy;
begin
  try
   DeallocateHWnd(Handle);
  finally
   if FMaskBMP<>nil then FMaskBMP.Free;
   inherited destroy;
  end;
end;

PROCEDURE TPicBtn.CreateWnd;
BEGIN
  inherited CreateWnd;
  BTNParentForm:=nil;
  BTNParentForm:=(GetParentForm(self)as TForm);
  if (BTNParentForm<>nil)and(BTNParentForm.Scaled=True)then BTNParentForm.Scaled:=False;
{}  if MaskedPicture.Picture<>nil then  Height:=MaskedPicture.Picture.Height DIV NumStates;
  Paint;
END;

procedure TPicBtn.StyleChanged(Sender: TObject);
begin
  ReColor;
  Paint;
end;

FUNCTION  TPicBtn.GetPalette: HPALETTE;
begin
  if MaskedPicture.Picture<>nil then Result:=MaskedPicture.Picture.Palette else
  Result:=inherited GetPalette;
end;

PROCEDURE TPicBtn.SetActive(Value:Integer);
BEGIN
  if Value<1 then Value:=1;
  IF Value=sDisabled THEN DoFocus(BTNParentForm,self,true);
  IF Value=sDisabled THEN Enabled:=False else Enabled:=True;
  if Value > NumStates then FBtnState:=1 else FBtnState:=Value;
  invalidate;
END;

PROCEDURE TPicBtn.SetNextState(Value:Integer);
begin
  if (Value<0)or(Value>NumStates) then Value:=0;
  FNextState:=Value;
end;

PROCEDURE TPicBtn.SetNumStates   (Value:Integer);
begin
  if Value<1 then Value:=1;
  FNumStates:=Value;
  if FsActive   > FNumStates then FsActive   :=1;
  if FsInActive > FNumStates then FsInActive :=1;
  if FsDisabled > FNumStates then FsDisabled :=1;
  if FsDown     > FNumStates then FsDown     :=1;
  if BtnState   > FNumStates Then BtnState   :=1;
{}  if MaskedPicture.Picture<>nil then  Height:=MaskedPicture.Picture.Height DIV NumStates;
end;


PROCEDURE TPicBtn.SetsActive     (Value:Integer);
begin
  if Value<1 then Value:=1;
  if Value > FNumStates then FsActive   :=1 else FsActive:=Value;
end;

PROCEDURE TPicBtn.SetFsInActive  (Value:Integer);
begin
  if Value<1 then Value:=1;
  if Value > FNumStates then FsInActive   :=1 else FsInActive:=Value;
end;

PROCEDURE TPicBtn.SetFsDisabled  (Value:Integer);
begin
  if Value<1 then Value:=1;
  if Value > FNumStates then FsDisabled   :=1 else FsDisabled:=Value;
end;

PROCEDURE TPicBtn.SetFsDown      (Value:Integer);
begin
  if Value<1 then Value:=1;
  if Value > FNumStates then FsDown   :=1 else FsDown:=Value;
end;

Procedure TPicBtn.WindowProc;
 begin
  with Message do
    if Msg = WM_TIMER then
      try
        if TWMTimer(Message).TimerID=RepeateTimer then RepTimer else MovTimer;
        Result:=0;
      except
        Application.HandleException(Self);
      end else Result := DefWindowProc(Handle, Msg, wParam, lParam);
 end;

PROCEDURE TPicBtn.SetMonitoring(Value:Boolean);
begin
  fMonitoring:=Value;
  KillTimer(Handle,MoveTimer);
  if Value then SetTimer(Handle,MoveTimer,1,nil);

end;

PROCEDURE TPicBtn.MovTimer;
var MausPos,P:tPoint;  Rect:TRect;
procedure Inside;
begin
  if not FMouseIn and Assigned(FOnMouseEnter) then
  begin
    FMouseIn:=True;
    FOnMouseEnter(Self);
  end;
end;
procedure OutSide;
begin
  if FMouseIn and Assigned(FOnMouseExit) then
  begin
    FMouseIn:=False;
    FOnMouseExit(Self);
  end;
end;
begin
  if (MaskedPicture.Picture=nil)or(MaskedPicture.Picture.Empty)then OutSide else
  begin
    GetCursorPos(MausPos);
    GetWindowRect(Handle,Rect);
    P:=clienttoscreen(Rect.TopLeft);
    if (MausPos.X < P.x) or (MausPos.X > P.x+width) or
       (MausPos.Y < P.y)or(MausPos.Y > p.y+height) then OutSide else
    begin
      if Not Masked then InSide else
      begin
        P:=screentoClient(MausPos);
        if MaskedPicture.Picture.Canvas.Pixels[p.x,p.y] = MaskedPicture.MaskColor then OutSide else InSide;
      end;
    end;
  end;
end;

PROCEDURE TPicBtn.RepTimer;
var CurrentTime:LongInt;
    MausPos,P:tPoint;  Rect:TRect;
procedure Inside;
begin
    CurrentTime:=GetCurrentTime;
     if (NextTime<= CurrentTime) then
    begin
     NextTime:=NextTime+LongInt(Repeate);
     if Assigned(FCommand) and (BtnState=sDown) then FCommand(Self);
    end;
end;
procedure OutSide;
begin
{}  BtnState:=NextState;
    NextTime:=-1;
  KillTimer(Handle,RepeateTimer);
end;
begin
  if (MaskedPicture.Picture=nil)or(MaskedPicture.Picture.Empty)then OutSide else
  begin
    GetCursorPos(MausPos);
    GetWindowRect(Handle,Rect);
    P:=clienttoscreen(Rect.TopLeft);
    if (MausPos.X < P.x) or (MausPos.X > P.x+width) or
       (MausPos.Y < P.y)or(MausPos.Y > p.y+height) then OutSide else
    begin
      if Not Masked then InSide else
      begin
        P:=screentoClient(MausPos);
        if MaskedPicture.Picture.Canvas.Pixels[p.x,p.y] = MaskedPicture.MaskColor then OutSide else InSide;
      end;
    end;
  end;
end;

PROCEDURE TPicBtn.SetFocusColor(Value:TColor);
begin
  if FFocusColor = Value then EXIT;
  FFocusColor := Value;
  invalidate;
end;

PROCEDURE TPicBtn.ReColor;
var sx,sy,siy:Integer;
    p:TPoint;
    fImage:TImage;
    c,Col:TColor;
    LogPalette:PLogPalette;
    ThePalette:HPalette;

    BIP          : PBitmapInfo;
    hmem         : THandle;
    buf          : Pointer;
    BIPSize      : Integer;
    ImageSize    : DWORD;
    ColorSize    : LongInt;
    BitCount     : Word;
  begin
    buf:=nil; siy:=0; Col:=clSilver;
    if (BTNParentForm=nil)or(FMaskBMP=nil) then exit;
{}  if (MaskedPicture.Picture<>nil)and not(MaskedPicture.Picture.Empty) then
    begin
      FMaskBMP.Width:=MaskedPicture.Picture.Width;
      FMaskBMP.Height:=MaskedPicture.Picture.Height;
    end else
    begin
      FMaskBMP.Assign(nil);
      Width:=0;
      Height:=0;
      exit;
    end;
    p.x:=left;
    p.y:=top;
    fImage:=nil;
    if (BTNParentForm.controlatPos(p,True) is TImage)then fImage:= BTNParentForm.controlatPos(p,True)as TImage
    else
    begin
      if Parent <> nil then
      begin
        if parent is TPanel then  Col:=(parent as TPanel).Color else
        if parent is TForm  then  Col:=(parent as TForm).Color;
      end;
    end;
    LogPalette:=nil;ThePalette:=MaskedPicture.Picture.Palette;
    if FMaskBMP.handle=0 then exit;
    GetSizes(FMaskBMP.handle,BIPSize,ImageSize,BitCount,ColorSize);
    GetMem(BIP, BIPSize);
    HMem := GlobalAlloc(gptr, ImageSize);
    if HMem <> 0 then
    begin
      Buf := GlobalLock(HMem);
      if Buf <> NIL then
      begin
        if MyGetLogPalette(ThePalette,LogPalette,True) then
        if MyInitDIBits(FMaskBMP.Handle,BIP,Buf,LogPalette)then
        begin
          for sy:=0 to BIP^.bmiHeader.biHeight do
          begin
            if siy > height-1 then siy:=0;
            for sx:=0 to BIP^.bmiHeader.biWidth do
            begin
              C:=MaskedPicture.Picture.Canvas.Pixels[sx,sy];
              begin
                if (not Masked)or(C <> MaskedPicture.MaskColor)then
                   SetDIPixelColor(BIP,Buf,ThePalette,sx,sy,c)
                else
                begin
                  if (fImage<>nil)and(fImage.Picture.Bitmap<>nil) then
                  C:=fImage.Picture.Bitmap.Canvas.Pixels[left-fImage.left+sx,top-fImage.top+(siy)] else
                  c:=Col;
                  SetDIPixelColor(BIP,Buf,ThePalette,sx,sy,C);
                end
              end; 
            end;
            inc(siy);
          end;
          if FMaskBMP.palette<>0 then DeleteObject(FMaskBMP.palette);
          FMaskBMP.palette:=ThePalette;
          with BIP^.bmiHeader do
             SetDIBitsToDevice(FMaskBMP.Canvas.Handle,
                              0, 0,
                              biWidth, biHeight,
                              0, 0,
                              0,
                              biHeight,
                              buf,
                              BIP^,
                              dib_rgb_colors);
          if LogPalette<>nil then freemem(LogPalette,(sizeof(TLogPalette)+sizeof(TPaletteEntry)*256));
        end;
      end;
      GlobalUnlock(HMem);
      GlobalFree(HMem);
      FreeMem(BIP, BIPSize);
    end;
  end;



PROCEDURE TPicBtn.Paint;
var Brush:TBrush;
    tmp:TBitmap;
begin
  Brush:=Canvas.Brush;
    if (FMaskBMP<>nil)and(MaskedPicture.Picture<>nil)and not(MaskedPicture.Picture.Empty)then
    begin
      Width  :=MaskedPicture.Picture.Width;
      Height :=MaskedPicture.Picture.Height div NumStates;
      tmp:=TBitmap.create;
      tmp.width:=width;
      tmp.Height:=height;
      tmp.palette:=FMaskBMP.Palette;
      bitBlt(TMP.Canvas.Handle,0,0,Width,Height,FMaskBMP.Canvas.Handle,0,(BtnState-1)*Height,SRCCOPY);
      if Masked and (FMaskBMP<>nil)and(FMaskBMP.Palette<>0)then tmp.palette:=FMaskBMP.Palette;
      Canvas.Draw(0,0,TMP);
      if (Focused)and(DrawFocus) then
      begin
        if (FocusRect.Bottom=0)and(FocusRect.Right=0)then FocusRect:=Rect(0,0,Width,Height);
        if Assigned(FOnDrawFocus) then FOnDrawFocus(Self,Canvas,FocusRect);
      end;
      tmp.free;
    end else
    begin
      Width  :=0;
      Height :=0;
    end;
  Canvas.Brush:=Brush;
end;

 PROCEDURE TPicBtn.DoDrawFocus(Sender: TObject;aCanvas:TCanvas;Var aRect:TRect);
 var oldcolor:TColor;
 begin
   OldColor:=Canvas.Brush.Color;
   try
     Canvas.Brush.Color:=FFocusColor;
     if (aRect.Bottom = 0)and(aRect.right = 0) then Canvas.FrameRect(Rect(0,0,Width,Height))
     else Canvas.FrameRect(aRect);
   finally
     Canvas.Brush.Color:=oldColor;
   end;
 end;

 PROCEDURE TPicBtn.SetMasked(Value:Boolean);
 begin
  if fMasked<>Value then
  begin
    fMasked:=Value;
    ReColor;
    Paint;
  end;
 end;

 PROCEDURE TPicBtn.WMKeyPress(var Message: TWMChar);
 Var CurrentTime:LongInt;
 begin
  CurrentTime:=GetCurrentTime;
  IF (Focused)and(Message.CharCode=vk_Space) THEN
  BEGIN
    if (NextTime<= CurrentTime)then
    begin
      if Assigned(FCommand)then FCommand(Self);
      if NextTime=-1 then
        NextTime:=CurrentTime+LongInt(FirstWait)
       else
        NextTime:=CurrentTime+LongInt(Repeate);
    end;
  END;
 end;

 PROCEDURE TPicBtn.WMKeyDown(var Message: TWMKeyDown);
 Var Shift:TShiftState;
 BEGIN
  Shift:=KeyDataToShiftState(Message.KeyData);
  Message.result:=0;
  IF (Focused)and(Message.CharCode=vk_Space) THEN BTNState:=sDown
  ELSE
  IF (Focused)and (Message.CharCode=vk_TAB) THEN
  begin
    if BTNState = sDown then
    begin
      KillTimer(Handle,RepeateTimer);
      fNextState:=BTNState;
      BTNState:= sActive;
    end else  DoFocus(BTNParentForm,self,boolean(not(ssShift in Shift)));
  end;
 END;

 PROCEDURE TPicBtn.WMKeyUp(var Message: TWMKeyUp);
 BEGIN
  IF (Enabled)And(Message.CharCode=vk_Space) THEN
  BEGIN
    KillTimer(Handle,RepeateTimer);
    Message.result:=0;
    NextTime:=-1;
    Click;
  END;
 END;

 PROCEDURE TPicBtn.WMLButtonDblclk(VAR Msg: TMessage);
 BEGIN
  IF Enabled THEN msg.msg:=wm_LbuttonDown;
  wmLButtonDown(msg);
 END;

 PROCEDURE TPicBtn.wmLButtonDown(VAR Msg: TMessage);
 BEGIN
   Msg.result:=0;
   SetFocus;
   fNextState:=BTNState;
   BTNState:=sDown;
   try
     if Assigned(FCommand)then FCommand(Self);
     NextTime:=GetCurrentTime+LongInt(FirstWait);
     KillTimer(Handle,RepeateTimer);
     SetTimer(Handle,RepeateTimer,1,nil);
   except end;
 END;

 PROCEDURE TPicBtn.WMLButtonUp(VAR Msg: TMessage);
 BEGIN
  Msg.result:=0;
  IF (Enabled)And (BtnState=sDown) then
  begin
    KillTimer(Handle,RepeateTimer);
    NextTime:=-1;
    Click;
  end;
 END;

PROCEDURE TPicBtn.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  message.Result:=1;
end;

PROCEDURE TPicBtn.WMNCHITTEST(var Message: TMessage);
var P,MausPos:TPoint;
begin
  message.Result:=1;
  if Masked and (MaskedPicture.Picture<>nil)and not(MaskedPicture.Picture.Empty)then
  begin
     GetCursorPos(MausPos);
     P:=screentoClient(MausPos);
     if MaskedPicture.Picture.Canvas.Pixels[p.x,p.y] = MaskedPicture.MaskColor then message.Result:=0;
  end;
end;


procedure TPicBtn.Click;
begin
  inherited Click;
  if fNextState=0 then BTNState:=sActive else BTNState:=fNextState;
end;

PROCEDURE TPicBtn.WMWINDOWPOSCHANGED;
begin
 inherited;
 with TWMWindowPosMsg(Message).WindowPos^ do
 begin
  if (fOldX<>x)or(fOldY<>y)or(fOldSX<>cx)or(fOldSY<>cy) then
  begin
    if not(csDestroying in ComponentState)then
    begin
      Recolor;
      Paint;
    end;
  end;
  fOldX:=x;fOldY:=y;fOldSX:=cx;fOldSY:=cy;
 end;
end;


PROCEDURE TPicBtn.WMKILLFOCUS;
BEGIN
  Paint;
  inherited
END;

PROCEDURE TPicBtn.WMSETFOCUS;
BEGIN
  Paint;
  inherited
END;


procedure TPicBtn.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS +DLGC_WANTARROWS + DLGC_WANTCHARS + DLGC_WANTTAB;
end;


procedure Register;
begin
  RegisterComponents('DELPHI STORAGE', [TPicBtn]);
  RegisterComponentEditor(TPicBtn, TMaskedPictureEditor);
  RegisterPropertyEditor(TypeInfo(TMaskedPicture), nil, '', TMaskColorProperty);
end;

end.
