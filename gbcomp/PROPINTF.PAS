{*****************************************************************************}
{                                                                             }
{  file:      PROPINTF.PAS                                                    }
{  title:     Enhanced bitmap property editor                                 }
{  version:   1.0.32                                                          }
{  date:      2/10/98                                                         }
{  author:    Andreas Heckel                                                  }
{             Mail: andreas.heckel@wirtschaft.tu-ilmenau.de                   }
{  copyright: DELPHI STORAGE                                                  }
{             Web: http://www.wirtschaft.tu-ilmenau.de/~aeg/                  }
{                                                                             }
{  compiler:  Borland DELPHI 3.0                                              }
{  descript.: DELPHI 3.0 bitmap property editor                               }
{             with transparent color selection                                }
{                                                                             }
{  revision history:                                                          }
{             2/10/98    first release                                        }
{                                                                             }
{  Category: FREEWARE - demo and source included)                             }
{  Can you please put a link to the site in your list and not just the zip    }
{*****************************************************************************}
unit propintf;

interface

uses Windows, Classes, SysUtils, Graphics, Forms, Controls, Buttons, DsgnIntf,
     StdCtrls, ComCtrls,ExtCtrls, Dialogs {$IFDEF VER100}, ExtDlgs{$ENDIF};

type
  TMaskedPicture = class(TPersistent)
  private
    FPic: TBitmap;
    FCol: TColor;
    FOnChange: TNotifyEvent;
    procedure SetPic(Value: TBitmap);
    procedure SetCol(Value: TColor);
  public
    constructor Create;virtual;
    destructor  Destroy;override;
    procedure Assign(Value: TMaskedPicture);reintroduce;virtual;
    procedure Changed;
  published
    property Picture  : TBitmap read FPic write SetPic;
    property MaskColor: TColor  read FCol write SetCol;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


Type
  TFormMaskProp = Class (TForm)
    FImage:TImage;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Panel3: TPanel;
    Panel2: TPanel;
    LB_Cursor: TLabel;
    LB_Sel: TLabel;
    procedure fImageMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure fImageClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
  private
    PicDialog       :TOpenDialog;
    FMaskedPicture  :TMaskedPicture;
    FPicture        :TBitmap;
    FMaskColor      :TColor;
    procedure SetMaskedPicture(Value: TMaskedPicture);
    procedure SetMskPicture(Value:TBitmap);
    procedure SetMskColor(Value:TColor);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property EditorMaskedPicture: TMaskedPicture read FMaskedPicture write SetMaskedPicture;
  end;

  Type

  TMaskColorProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TMaskedPictureEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;




implementation

{$IFDEF VER100}
{$R intf_d3.dfm}
{$ENDIF}
{$IFNDEF VER100}
{$R intf_2c.dfm}
{$ENDIF}

{########################################################## TMaskedPicture ####}

constructor TMaskedPicture.Create;
begin
  inherited create;
  FPic:=TBitmap.Create;
end;

destructor TMaskedPicture.Destroy;
begin
  if fPic<>nil then fPic.FreeImage;
  fPic:=nil;
  inherited destroy;
end;

procedure TMaskedPicture.Assign(Value: TMaskedPicture);
begin
  MaskColor := Value.MaskColor;
  Picture.assign(Value.Picture);
end;

procedure TMaskedPicture.SetPic(Value: TBitmap);
begin
  if Value <> FPic then
  begin
    FPic.assign(Value);
    Changed;
  end;
end;

procedure TMaskedPicture.SetCol(Value: TColor);
begin
  if Value <> FCol then
  begin
    FCol := Value;
    Changed;
  end;
end;

procedure TMaskedPicture.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{############################################################ TFormMaskProp ###}

constructor TFormMaskProp.Create(AOwner: TComponent);
begin
  inherited create(aOwner);
  fPicture:=TBitmap.create;
  {$IFDEF VER100}
  PicDialog:=TOpenPictureDialog.Create(aOwner);
  {$ENDIF}
  {$IFNDEF VER100}
  PicDialog:=TOpenDialog.Create(aOwner);
  {$ENDIF}
  PicDialog.Filter:='Bitmaps (*.bmp)|*.BMP';
end;

destructor  TFormMaskProp.Destroy;
begin
  if FPicture<>nil then FPicture.Free;
  FPicture:=nil;
  PicDialog.Free;
  inherited destroy;
end;

procedure TFormMaskProp.SetMaskedPicture(Value: TMaskedPicture);
begin
  FMaskedPicture := Value;
  FPicture.assign(Value.Picture);
  FMaskColor := Value.MaskColor;
  SetMskPicture(Value.Picture);
  SetMskColor(Value.MaskColor);
  LB_Cursor.Caption:=ColorToString(Panel2.Color);
end;

procedure TFormMaskProp.SetMskPicture(Value: TBitmap);
begin
  fImage.Picture.Bitmap.assign(Value);
  if (Value<>nil)then
  begin
    if (Value.Width  > (ClientWidth-Panel4.Width)) then ClientWidth:= Value.Width+Panel4.Width;
    if (Value.Height >  ClientWidth) then ClientHeight:= Value.Height;
  end;
  FMaskedPicture.Picture.assign(Value);
end;

procedure TFormMaskProp.SetMskColor(Value: TColor);
begin
  Panel3.Color := Value;
  LB_Sel.Caption:=ColorToString(Value);
  FMaskedPicture.MaskColor := Value;
end;

procedure TFormMaskProp.fImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (fImage.Picture.Bitmap.Empty)or(fImage.Picture.Bitmap=nil) then
  begin
    Panel2.Color:=clWhite;
    LB_Cursor.Caption:='picture empty';
  end else
  begin
    if DWORD(fImage.canvas.Pixels[x,y])<>$FFFFFFFF then Panel2.Color:=fImage.canvas.Pixels[x,y];
    LB_Cursor.Caption:=ColorToString(Panel2.Color);
  end;
end;

procedure TFormMaskProp.fImageClick(Sender: TObject);
begin
  if not fImage.Picture.Bitmap.Empty then
  begin
    Panel3.Color:=Panel2.Color;
    LB_Sel.Caption:=ColorToString(Panel3.Color);
  end else messagebeep(0);
end;

procedure TFormMaskProp.Button1Click(Sender: TObject);
begin
  SetMskColor(Panel3.Color);
  SetMskPicture(fImage.Picture.Bitmap);
  if FMaskedPicture<>nil then FMaskedPicture.Changed;
end;

procedure TFormMaskProp.Button2Click(Sender: TObject);
begin
  if PicDialog.Execute then fImage.Picture.LoadFromFile(PicDialog.FileName);
  if (fImage.Picture.Bitmap.Width  > (ClientWidth-Panel4.Width)) then ClientWidth:= fImage.Picture.Bitmap.Width+Panel4.Width;
  if (fImage.Picture.Bitmap.Height >  ClientWidth) then ClientHeight:= fImage.Picture.Bitmap.Height;
  if not fImage.Picture.Bitmap.Empty then LB_Cursor.Caption:='';
end;

procedure TFormMaskProp.Button4Click(Sender: TObject);
begin
  fImage.Picture.Bitmap.Assign(nil);
  if fImage.Picture.Bitmap.Empty then
  begin
    Panel2.Color:=clWhite;
    LB_Cursor.Caption:='picture empty';
  end;
end;

procedure TFormMaskProp.Panel4Resize(Sender: TObject);
begin
  Button1.top:=Panel4.Height-28;
  Button3.top:=Panel4.Height-28;
end;

{####################################################### TMaskColorProperty ###}

Function TMaskColorProperty.GetAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

Procedure TMaskColorProperty.Edit;
var
  aMskPic : TMaskedPicture;
  Editor  : TFormMaskProp;
begin
  aMskPic := TMaskedPicture(GetOrdValue);
  Editor  := TFormMaskProp.Create(Application);
  try
    Editor.EditorMaskedPicture := aMskPic;
    Editor.ShowModal;
  finally
    Editor.Free;
  end;
end;


{##################################################### TMaskedPictureEditor ###}

procedure TMaskedPictureEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'MASKEDPICTURE') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

function TMaskedPictureEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TMaskedPictureEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Edit Masked Picture'
  else Result := '';
end;

procedure TMaskedPictureEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

end.
