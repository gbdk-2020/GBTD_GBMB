unit export;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, gbofile, gbconst, ColorController, MainLib, GBTDMain, ComCtrls;

type
  TFrmExport = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    DlgFilename: TSaveDialog;
    BtnHelp: TButton;
    Tabs: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    CmbType: TComboBox;
    EdFilename: TEdit;
    BtnBrowse: TButton;
    GrpSettings: TGroupBox;
    LblLabel: TLabel;
    LblSection: TLabel;
    Label6: TLabel;
    LblBank: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    EdLabel: TEdit;
    EdSection: TEdit;
    ChkGenArray: TCheckBox;
    CmbCounter: TComboBox;
    CmbFormat: TComboBox;
    ChkCompress: TCheckBox;
    EdBank: TEdit;
    EdFrom: TEdit;
    EdTo: TEdit;
    GrpMetatiles: TGroupBox;
    LblMetaOffset: TLabel;
    LblMetaCounter: TLabel;
    ChkMetaTile: TCheckBox;
    EdMetaOffset: TEdit;
    CmbMetaCounter: TComboBox;
    GrpColor: TGroupBox;
    Label3: TLabel;
    LblCGBPal: TLabel;
    ChkColors: TCheckBox;
    CmbSGBPal: TComboBox;
    CmbCGBPal: TComboBox;
    GroupBox1: TGroupBox;
    ChkSplit: TCheckBox;
    LblBlockSize: TLabel;
    EdBlockSize: TEdit;
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CmbTypeChange(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EdChange(Sender: TObject);
    procedure ChkMetaTileClick(Sender: TObject);
    procedure ChkSplitClick(Sender: TObject);

  private
    { Private declarations }

    RGBDSAsmExt   : string;
    RGBDSIncExt   : string;
    RGBDSObjExt   : string;
    GeneralBinExt : string;
    GBDKCExt      : string;
    GBDKHExt      : string;
    TASMAsmExt    : string;
    ISASAsmExt    : string;
    ISASIncExt    : string;

    procedure ExportToRGBDSAsm( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
    procedure ExportToRGBDSObj( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
    procedure ExportToGeneralBin( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
    procedure ExportToGBDKC( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
    procedure ExportToTASMAsm( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
    procedure ExportToISASAsm( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);


    function ValidSettings: boolean;
    function IndexToExtension: string;
    procedure ForceExtension;
    procedure MetaTileChanged;

  public
    { Public declarations }
    procedure StartExport;

  end;

var
  FrmExport: TFrmExport;

implementation

uses TileEdit, ExpBase, ExpTypes;

{$R *.DFM}

{--$I ..\hlp\gbtd.inc}


procedure TFrmExport.FormCreate(Sender: TObject);
begin
  (* get extensions from ini-file *)
  with IniFile do
  begin
    RGBDSAsmExt := Lowercase(ReadString('RGBDS', 'AsmExt', 'z80'));
    RGBDSIncExt := Lowercase(ReadString('RGBDS', 'IncExt', 'inc'));
    RGBDSObjExt := Lowercase(ReadString('RGBDS', 'ObjExt', 'obj'));
    GeneralBinExt := Lowercase(ReadString('General', 'BinExt', 'bin'));
    GBDKCExt := Lowercase(ReadString('GBDK', 'CExt', 'c'));
    GBDKHExt := Lowercase(ReadString('GBDK', 'HExt', 'h'));
    TASMAsmExt := Lowercase(ReadString('TASM', 'AsmExt', 'z80'));
    ISASAsmExt := Lowercase(ReadString('ISAS', 'AsmExt', 's'));
    ISASIncExt := Lowercase(ReadString('ISAS', 'IncExt', 'inc'));
  end;

  (* setup combobox *)
  with CmbType.Items do
  begin
    Add( 'RGBDS Assembly file (*.' + RGBDSAsmExt + ')' );
    Add( 'RGBDS Object file (*.' + RGBDSObjExt + ')' );
    Add( 'TASM Assembly file (*.' + TASMAsmExt + ')' );
    Add( 'GBDK C file (*.' + GBDKCExt + ')' );
    Add( 'All-purpose binary file (*.' + GeneralBinExt + ')' );
    Add( 'ISAS Assembly file (*.' + ISASAsmExt + ')' );
  end;

  (* Init fields *)
  with TileExport do
  begin
    EdFileName.Text := LoadString(FileName, 'Export');
    EdSection.Text := LoadString(SectionName, 'Tiles');
    EdLabel.Text := LoadString(LabelName, 'TileLabel');

    EdBank.Text := IntToStr(Bank);
    ChkGenArray.Checked := TileArray;
    ChkCompress.Checked := (Compression <> 0);
    ChkColors.Checked := IncludeColors;
    CmbCounter.ItemIndex := Counter;
    CmbType.ItemIndex := FileType;
    CmbFormat.ItemIndex := Format;
    CmbSGBPal.ItemIndex := SGBPalettes;
    CmbCGBPal.ItemIndex := CGBPalettes;

    if (From <= UpTo) and (From < FrmTile.LstTiles.TileData.Count) then
      EdFrom.Text := IntToStr(From)
    else
      EdFrom.Text := '0';

    if (UpTo >= From) and (UpTo < FrmTile.LstTiles.TileData.Count) then
      EdTo.Text := IntToStr(UpTo)
    else
      EdTo.Text := '0';

    ChkMetaTile.Checked := MakeMetaTiles;
    EdMetaOffset.Text := IntToStr(MetaOffset);
    try CmbMetaCounter.ItemIndex := MetaCounter; except CmbMetaCounter.ItemIndex := 0; end;
    MetaTileChanged;

    ChkSplit.Checked := Split;
    EdBlockSize.Text := IntToStr(BlockSize);

    Tabs.ActivePage := Tabs.Pages[SelTab];

    CmbTypeChange(nil);                   (* BUG?: force direct refresh *)
    ForceExtension;

  end;

  (* setup tabs *)
  with Tabs do
  begin
    Pages[0].Caption := 'Standard';
    Pages[1].Caption := 'Advanced';
  end;


//  HelpContext := Export_options;
//  CmbType.HelpContext := Export_file_types;
//  CmbFormat.HelpContext := Export_data_formats;
//  CmbCounter.HelpContext := Export_counter_types;
//  GrpMetatiles.HelpContext := Metatiles;

end;



procedure TFrmExport.BtnBrowseClick(Sender: TObject);
var s : string;
begin
  with DlgFilename do
  begin

    case CmbType.ItemIndex of
      0    : s := 'RGBDS Assembly files';
      1    : s := 'RGBDS object files';
      2    : s := 'TASM Assembly files';
      3    : s := 'GBDK C files';
      else   s := 'All-purpose binary files';
    end;
    Filter := s + ' (*.' + IndexToExtension + ')|*.' + IndexToExtension;

    DefaultExt := IndexToExtension;
    FileName := LoadString( EdFilename.Text, '*.' + IndexToExtension );

    if Execute then
    begin
      EdFileName.Text := Filename;
      ForceExtension;
    end;
  end;
end;



procedure TFrmExport.BtnOKClick(Sender: TObject);
begin
  StartExport;
end;



procedure TFrmExport.StartExport;
var x,y : integer;
begin
  if ValidSettings then
  begin
    (* Save settings *)
    with TileExport do
    begin
      StrPCopy(FileName, EdFileName.Text);
      StrPCopy(SectionName, EdSection.Text);
      StrPCopy(LabelName, EdLabel.Text);
      From := StrToInt(Trim(EdFrom.Text));
      UpTo := StrToInt(Trim(EdTo.Text));
      Bank := StrToInt(Trim(EdBank.Text));
      TileArray := ChkGenArray.Checked;
      IncludeColors := ChkColors.Checked;
      if ChkCompress.Checked then Compression := 1 else Compression := 0;
      Counter := CmbCounter.ItemIndex;
      FileType := CmbType.ItemIndex;
      Format := CmbFormat.ItemIndex;
      SGBPalettes := CmbSGBPal.ItemIndex;
      CGBPalettes := CmbCGBPal.ItemIndex;

      MakeMetaTiles := ChkMetaTile.Checked;
      MetaOffset := SaveStrToInt(Trim(EdMetaOffset.Text));
      MetaCounter   := CmbMetaCounter.ItemIndex;

      Split         := ChkSplit.Checked;
      BlockSize     := SaveStrToInt( Trim(EdBlockSize.Text) );

      SelTab        := Tabs.ActivePage.PageIndex;

      Modified := True;
    end;


    (* Start Export *)
    case CmbType.ItemIndex of
      0   : ExportToRGBDSAsm( TileExport, CurTileSize, TileList );
      1   : ExportToRGBDSObj( TileExport, CurTileSize, TileList );
      2   : ExportToTASMAsm( TileExport, CurTileSize, TileList );
      3   : ExportToGBDKC( TileExport, CurTileSize, TileList );
      4   : ExportToGeneralBin( TileExport, CurTileSize, TileList );
      else  ExportToISASAsm( TileExport, CurTileSize, TileList );
    end;

    Close;
  end
  else
    (* Show form when in direct export-mode *)
    if not Showing then ShowModal;

end;


procedure TFrmExport.CmbTypeChange(Sender: TObject);
begin
  ForceExtension;

  (* Update mandatory fields *)
  case CmbType.ItemIndex of
    0,1,5 : begin  (* RGBDS Asm/Obj / ISAS *)
              EdLabel.Enabled := True;
              LblLabel.Enabled := True;
              EdSection.Enabled := True;
              LblSection.Enabled := True;
              EdBank.Enabled := True;
              LblBank.Enabled := True;
              ChkSplit.Enabled := True;
              EdBlockSize.Enabled := ChkSplit.Checked;
              LblBlockSize.Enabled := ChkSplit.Checked;
            end;
    2     : begin  (* TAsm / GBDK C *)
              EdLabel.Enabled := True;
              LblLabel.Enabled := True;
              EdSection.Enabled := False;
              LblSection.Enabled := False;
              EdBank.Enabled := False;
              LblBank.Enabled := False;
              ChkSplit.Enabled := True;
              EdBlockSize.Enabled := ChkSplit.Checked;
              LblBlockSize.Enabled := ChkSplit.Checked;
            end;
    3     : begin  (* TAsm / GBDK C *)
              EdLabel.Enabled := True;
              LblLabel.Enabled := True;
              EdSection.Enabled := False;
              LblSection.Enabled := False;
              EdBank.Enabled := True;
              LblBank.Enabled := True;
              ChkSplit.Enabled := True;
              EdBlockSize.Enabled := ChkSplit.Checked;
              LblBlockSize.Enabled := ChkSplit.Checked;
            end;
    else    begin  (* Binary file *)
              EdLabel.Enabled := False;
              LblLabel.Enabled := False;
              EdSection.Enabled := False;
              LblSection.Enabled := False;
              EdBank.Enabled := False;
              LblBank.Enabled := False;
              ChkSplit.Enabled := False;
              EdBlockSize.Enabled := False;
              LblBlockSize.Enabled := False;
            end;
  end;
end;



procedure TFrmExport.ForceExtension;
(****************************************)
(* zorgt ervoor dat de huidige Filename *)
(* de correcte extentie heeft.          *)
(****************************************)
var s : string;
begin
  s := EdFileName.Text;
  if (trim(s) <> EmptyStr) then
    if CharLocBackWards( s, '.') <> -1 then
      EdFileName.Text := copy( s, 0, CharLocBackWards( s, '.') ) + IndexToExtension
    else
      EdFileName.Text := s + '.' + IndexToExtension;
end;



function TFrmExport.IndexToExtension: string;
(********************************)
(* Geeft huidige extentie terug *)
(********************************)
begin
  case CmbType.ItemIndex of
    0    : Result := RGBDSAsmExt;
    1    : Result := RGBDSObjExt;
    2    : Result := TASMAsmExt;
    3    : Result := GBDKCExt;
    4    : Result := GeneralBinExt;
    else   Result := ISASAsmExt;
  end;
end;



function TFrmExport.ValidSettings: boolean;
(*************************************************)
(* Controleert of alle velden goed zijn ingevuld *)
(* voor het huidige exporttype, en plaatst een   *)
(* dialog als dit niet het geval is.             *)
(*************************************************)
var s   : string;
    f,t : integer;
    Ctrl : TWinControl;
    Page : integer;

  function ChkFileName: string;   begin if (trim(EdFileName.Text) = EmptyStr) then Result := '''Filename'; Page := 0; end;
  function ChkLabel: string;      begin if (trim(EdLabel.Text) = EmptyStr) then Result := '''Label'; Page := 0; end;
  function ChkSection: string;    begin if (trim(EdSection.Text) = EmptyStr) then Result := '''Section'; Page := 0;  end;
  function ChkBank: string;       begin if (trim(EdBank.Text) = EmptyStr) then Result := '''Bank'; Page := 0;  end;
  function ChkFrom: string;       begin if (trim(EdFrom.Text) = EmptyStr) then Result := '''From'; Page := 0;  end;
  function ChkTo: string;         begin if (trim(EdTo.Text) = EmptyStr) then Result := '''To'; Page := 0; end;
  function ChkMetaOffset: string; begin if (ChkMetaTile.Checked) and (trim(EdMetaOffset.Text) = EmptyStr) then Result := '''Index offset'; Page := 1;  end;
  function ChkBlockSize: string;  begin if (ChkSplit.Checked) and (trim(EdBlockSize.Text) = EmptyStr) then Result := '''Block size'; Page := 1; end;
begin
  ForceExtension;

  (* validate blank fields *)
  s := ChkFileName;                   (* always mandatory *)
  Ctrl := EdFileName;
  if (s = EmptyStr) then
  begin
    case CmbType.ItemIndex of
      0,1   : begin  (* RGBDS Asm/Obj *)
                s := ChkLabel; Ctrl := EdLabel;
                if (s = EmptyStr) then begin s := ChkSection; Ctrl := EdSection; end;
                if (s = EmptyStr) then begin s := ChkBank; Ctrl := EdBank; end;
                if (s = EmptyStr) then begin s := ChkFrom; Ctrl := EdFrom; end;
                if (s = EmptyStr) then begin s := ChkTo; Ctrl := EdTo; end;
                if (s = EmptyStr) then begin s := ChkMetaOffset; Ctrl := EdMetaOffset; end;
                if (s = EmptyStr) then begin s := ChkBlockSize; Ctrl := EdBlockSize; end;
              end;
      2,3   : begin  (* TAsm / GBDK C *)
                s := ChkLabel; Ctrl := EdLabel;
                if (s = EmptyStr) then begin s := ChkFrom; Ctrl := EdFrom; end;
                if (s = EmptyStr) then begin s := ChkTo; Ctrl := EdTo; end;
                if (s = EmptyStr) then begin s := ChkMetaOffset; Ctrl := EdMetaOffset; end;
                if (s = EmptyStr) then begin s := ChkBlockSize; Ctrl := EdBlockSize; end;
              end;
      else    begin  (* Binary file *)
                s := ChkFrom; Ctrl := EdTo;
                if (s = EmptyStr) then begin s := ChkTo; Ctrl := EdTo; end;
                if (s = EmptyStr) then begin s := ChkMetaOffset; Ctrl := EdMetaOffset; end;
              end;
    end;
  end;

  (* blank fields ? *)
  if (s <> EmptyStr) then
    s := s + ''' is mandatory for this export type.'
  else
  begin

    (* check for tile boundaries *)
    page := 0;
    f := StrToInt(Trim(EdFrom.Text));
    t := StrToInt(Trim(EdTo.Text));

    if (f > t) then
      begin s := '''To'' should be higher or equal to ''From''.'; Ctrl := EdTo; end
    else
      if (t > TileList.Count-1) then
      begin s := '''To'' can not exceed ' + IntToStr(TileList.Count-1) + '.'; Ctrl := EdTo; end
      else
      begin
        Page := 1;
        if (ChkSplit.Checked) and (SaveStrToInt(EdBlockSize.Text) < 1) then
        begin s := '''Block size'' should be higher or equal to 1.'; Ctrl := EdBlockSize; end;
      end;
  end;

  (* any errors ? *)
  Result := (s = EmptyStr);
  if (not Result) then
  begin
    Messagedlg(s, mtError, [mbOk],0 );
    Tabs.ActivePage := Tabs.Pages[Page];
    Ctrl.SetFocus;
  end;

end;


procedure TFrmExport.ExportToISASAsm( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
var Exp : TGBTDExport;
    s : string;
begin
  Exp := TGBTDISASAsmExport.Create( TileExport.Filename );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst, GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;

  s := copy( TileExport.Filename, 0, pos('.', TileExport.Filename)) + ISASIncExt;
  Exp := TGBTDISASIncExport.Create( s );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst,  GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;
end;


procedure TFrmExport.ExportToTASMAsm( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
var Exp : TGBTDExport;
    s : string;
begin
  Exp := TGBTDTASMAsmExport.Create( TileExport.Filename );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst, GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;
end;


procedure TFrmExport.ExportToGeneralBin( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
var Exp : TGBTDExport;
    s : string;
begin
  Exp := TGBTDBinExport.Create( TileExport.Filename );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst, GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;
end;



procedure TFrmExport.ExportToRGBDSAsm( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
var Exp : TGBTDExport;
    s : string;
begin
  Exp := TGBTDRGBDSAsmExport.Create( TileExport.Filename );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst, GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;

  s := copy( TileExport.Filename, 0, pos('.', TileExport.Filename)) + RGBDSIncExt;
  Exp := TGBTDRGBDSIncExport.Create( s );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst,  GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;
end;



procedure TFrmExport.ExportToRGBDSObj( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
var Exp : TGBTDExport;
    s : string;
begin
  Exp := TGBTDRGBDSObjExport.Create( TileExport.Filename );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst,  GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;

  s := copy( TileExport.Filename, 0, pos('.', TileExport.Filename)) + RGBDSIncExt;
  Exp := TGBTDRGBDSIncExport.Create( s );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst, GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;
end;



procedure TFrmExport.ExportToGBDKC( TileExport : TGBOTileExport; TileSize : TGBTileSize; Lst : TList);
var Exp : TGBTDExport;
    s : string;
begin
  Exp := TGBTDGBDKCExport.Create( TileExport.Filename );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst, GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;

  s := copy( TileExport.Filename, 0, pos('.', TileExport.Filename)) + GBDKHExt;
  Exp := TGBTDGBDKHExport.Create( s );
  try
    Exp.GBTDExport( TileExport, TileSize, Lst, GBColorController.SGBSets, GBColorController.CGBSets )
  finally
    Exp.free;
  end;
end;



procedure TFrmExport.FormDestroy(Sender: TObject);
begin
  FrmExport := nil;
end;

procedure TFrmExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmExport.BtnHelpClick(Sender: TObject);
begin
//  Application.HelpContext(Export_options);
end;


procedure TFrmExport.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

procedure TFrmExport.EdChange(Sender: TObject);
begin
  ForceNumbers(TCustomEdit(Sender));
end;

procedure TFrmExport.ChkMetaTileClick(Sender: TObject);
begin
  MetaTileChanged;
end;

procedure TFrmExport.MetaTileChanged;
begin
  EdMetaOffset.Enabled   := ChkMetaTile.Checked;
  LblMetaOffset.Enabled  := ChkMetaTile.Checked;
  CmbMetaCounter.Enabled := ChkMetaTile.Checked;
  LblMetaCounter.Enabled := ChkMetaTile.Checked;
end;



procedure TFrmExport.ChkSplitClick(Sender: TObject);
begin
  EdBlockSize.Enabled := ChkSplit.Checked;
  LblBlockSize.Enabled := ChkSplit.Checked;
end;

end.
