unit Palette;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GammaPanel, GBColorSetSelector, gbConst, StdCtrls, Buttons,
  ColorController, ComCtrls;

type
  TfrmPalette = class(TForm)
    gbPal: TGroupBox;
    PnlGamma: TGammaPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    BtnCopyAll: TSpeedButton;
    BtnCopy: TSpeedButton;
    BtnPaste: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure PnlGammaChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnCopyAllClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FColorSets : TGBColorSets;
    FColorSetSelectors : array[0..7] of TGBColorSetSelector;
    FCurSet : TGBColorSetSelector;

    FLbl : array[0..7] of TLabel;
    FCnt : integer;

    procedure SelectorColorSelect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; clr: Integer);

    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure CopyAllToClipboard;
    procedure StorePal( Index : integer);

  public
    { Public declarations }
  end;

var
  frmPalette: TfrmPalette;

implementation

uses tileedit, GBTDMain, MainLib;


{$R *.DFM}

{--$I ..\hlp\gbtd.inc}

const SEP : PChar = chr(9);
      EOL : PChar = chr(13) + chr(10);


var PalStrm : TStream;

procedure TFrmPalette.StorePal( Index : integer);
var i,j : integer;
    s : string;
    c : integer;
    p : PChar;
begin
  for i := 0 to 3 do
  begin
    c := FColorSets[Index,i];
    s := '';
    for j := 0 to 2 do
    begin
      s := s + IntToStr( ((c and $FF) shr 3) );
      c := c shr 8;
      if (j < 2) then s := s + SEP;
    end;


    if (i < 3) then s := s + EOL;
    p := PChar(s);
    PalStrm.write(p^,Length(s));
  end;
end;


procedure TFrmPalette.PasteFromClipboard;
var i,j,k : integer;
    Lst : TStringList;
    s : string;
    c,base : TColor;

begin
  BtnOK.SetFocus;            (* fix: leave edits to stop them from reacting *)

  PalStrm := TMemoryStream.Create;
  Lst := TStringList.Create;
  try
//    ClipboardToStream(PalStrm);
    TextClipboardToStream(PalStrm);
    PalStrm.Position := 0;
    base := $FF000000;

    for i := FCurSet.Tag to FCnt do
      for j :=0 to 3 do
      begin

        (* load a color *)
        Lst.Clear;
        s := LineFromStream(PalStrm);
        SeparateStrData(s, lst);

        if (Lst.Count >= 3) then
        begin
          (* convert color *)
          c := 0;
          for k := 0 to 2 do
          begin
            c := c shr 8;
            try c := c + (StrToInt(Lst.Strings[k]) and 31) shl (16+3) except end;
          end;
          FColorSets[i,j] := c;
          if (j = 0) then base := c;
        end;
      end;

    (* reset SGB base color *)
    if (base <> $FF000000) and (GBColorController.ColorMode = gbcmSGB) then
      for i := 0 to 3 do
        FColorSets[i,0] := base;

    (* reset GammaPanel *)
    PnlGamma.FirstColor := FColorSets[FCurSet.Tag, FCurSet.SelColor];

    (* refresh *)
    for i := 0 to FCnt do
      FColorSetSelectors[i].Paint;

  finally
    Lst.Free;
    PalStrm.Free;
  end;
end;



procedure TFrmPalette.CopyToClipboard;
begin
  BtnOK.SetFocus;            (* fix: leave edits to stop them from reacting *)

  PalStrm := TMemoryStream.Create;
  try
    StorePal(FCurSet.Tag);
    StreamToClipboard(PalStrm);
  finally
    PalStrm.Free;
  end;
end;


procedure TFrmPalette.CopyAllToClipboard;
var i : integer;
begin
  BtnOK.SetFocus;            (* fix: leave edits to stop them from reacting *)

  PalStrm := TMemoryStream.Create;
  try

    for i:=0 to FCnt do
    begin
      StorePal(i);
      if (i < FCnt) then PalStrm.Write(EOL^, 2);
    end;

    StreamToClipboard(PalStrm);
  finally
    PalStrm.Free;
  end;
end;



procedure TfrmPalette.FormCreate(Sender: TObject);
var i,j : integer;
begin

  if (GBColorController.ColorMode = gbcmSGB) then
  begin
    FCnt := 3;
    gbPal.Caption := 'Super Gameboy palettes';
    PnlGamma.GBCFilter := False;
  end
  else
  begin
    FCnt := 7;
    gbPal.Caption := 'Gameboy Color palettes';
    PnlGamma.GBCFilter := (GBColorController.ColorMode = gbcmGBCFiltered);
  end;


  for i := 0 to FCnt do
  begin
    (* Create labels *)
    FLbl[i] := TLabel.Create(Self);
    with FLbl[i] do
    begin
      Parent := gbPal;
      Left := 16;
      Top := 23 + (i*28);
      Caption := IntToStr(i);
    end;

    (* Create selectors *)
    FColorSetSelectors[i] := TGBColorSetSelector.Create(Self);
    with FColorSetSelectors[i] do
    begin
      Parent := gbPal;
      Left := 16 + 20;
      Top := 19 + (i*28);
      Tag := i;
      ColorSet := @FColorSets[i];
      OnColorSelect := SelectorColorSelect;
      SelColor := -1;

      GBCFilter := (GBColorController.ColorMode = gbcmGBCFiltered);
    end;
  end;

  case GBColorController.ColorMode of
    gbcmSGB : FColorSets := GBColorController.SGBSets;
    else      FColorSets := GBColorController.CGBSets;
  end;

  FColorSetSelectors[0].SelColor := 0;
  FCurSet := FColorSetSelectors[0];

  PnlGamma.FirstColor := FColorSets[FCurSet.Tag, FCurSet.SelColor];

//  HelpContext := Palettes;
end;



procedure TfrmPalette.SelectorColorSelect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; clr: Integer);
var i : integer;
begin
  for i := 0 to FCnt do
    with FColorSetSelectors[i] do
      if (Tag <> TComponent(Sender).Tag) then
        SelColor := -1;

  TGBColorSetSelector(Sender).SelColor := clr;
  FCurSet := FColorSetSelectors[TComponent(Sender).Tag];

  if (PnlGamma.FirstColor <> FColorSets[TComponent(Sender).Tag, clr]) then
    PnlGamma.FirstColor := FColorSets[TComponent(Sender).Tag, clr];
end;



procedure TfrmPalette.PnlGammaChange(Sender: TObject);
var i : integer;
begin
  if (FColorSets[FCurSet.Tag, FCurSet.SelColor] <> PnlGamma.FirstColor) then
    FColorSets[FCurSet.Tag, FCurSet.SelColor] := PnlGamma.FirstColor;

  if (FCurSet.SelColor = 0) and (GBColorController.ColorMode = gbcmSGB) then
    for i := 0 to 3 do
    begin
      FColorSets[i,0] := PnlGamma.FirstColor;
      FColorSetSelectors[i].Paint;
    end
  else
    FCurSet.Paint;
end;



procedure TfrmPalette.FormDestroy(Sender: TObject);
begin
  frmPalette := nil;
end;

procedure TfrmPalette.FormClose(Sender: TObject; var Action: TCloseAction);
var i : integer;
begin
  Action := caFree;

  if (ModalResult = mrOk) then
  begin
    case GBColorController.ColorMode of
      gbcmSGB : GBColorController.SGBSets := FColorSets;
      else      GBColorController.CGBSets := FColorSets;
    end;

    Modified := True;
    FrmTile.GrdPixel.Paint;
    TileExchanger.ColorSetsToExchange;
  end;
end;

procedure TfrmPalette.BtnCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TfrmPalette.BtnCopyAllClick(Sender: TObject);
begin
  CopyAllToClipboard;
end;

procedure TfrmPalette.BtnPasteClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TfrmPalette.BtnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfrmPalette.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;

  if (ssCtrl in Shift) then
  begin
    if (UpCase(char(Key)) = 'C') then
    begin
      CopyToClipboard;
      Key := 0;
    end;
    if (UpCase(char(Key)) = 'V') then
    begin
      PasteFromClipboard;
      Key := 0;
    end;
  end;
end;


end.


