unit Export;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Repository, ExtCtrls, MainLib, gbConst, Impstringgrid,
  ComCtrls,  Grids, Dialogs, GBMBMain;



type
  TFrmExport = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    PageCtrl: TPageControl;
    TabSheet: TTabSheet;
    TabSheet2: TTabSheet;
    DlgFilename: TSaveDialog;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    CmbType: TComboBox;
    EdFilename: TEdit;
    BtnBrowse: TButton;
    GrpSettings: TGroupBox;
    LblLabel: TLabel;
    LblSection: TLabel;
    LblBank: TLabel;
    EdLabel: TEdit;
    EdSection: TEdit;
    EdBank: TEdit;
    GrpProps: TGroupBox;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    CboPlaneCnt: TComboBox;
    PnlResult: TPanel;
    Label1: TLabel;
    GrdResult: TStringGrid;
    cboOrder: TComboBox;
    cboMap: TComboBox;
    GrpSplit: TGroupBox;
    EdSplitSize: TEdit;
    LblSplitSize: TLabel;
    ChkSplit: TCheckBox;
    BtnAdd: TButton;
    BtnDel: TButton;
    GrdProp: TImpStringgrid;
    ChkSplitBank: TCheckBox;
    EdTileOffset: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure CmbTypeChange(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDelClick(Sender: TObject);
    procedure GrdPropSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure GrdPropKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GrdPropKeyPress(Sender: TObject; var Key: Char);
    procedure ChkSplitClick(Sender: TObject);
    procedure CboPlaneCntChange(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);

  private
    { Private declarations }

    FLstProps    : TStringList;

    RGBDSAsmExt   : string;
    RGBDSIncExt   : string;
    RGBDSObjExt   : string;
    GeneralBinExt : string;
    GBDKCExt      : string;
    GBDKHExt      : string;
    TASMAsmExt    : string;
    ISASAsmExt    : string;
    ISASIncExt    : string;

    function BuildGrdResult: boolean;
    procedure ForceExtension;
    function IndexToExtension: string;
    procedure LoadSettings;
    procedure SaveSettings;
    function ValidSettings: boolean;
    procedure StartExport;
    procedure SetPropRow( Row, PropNr : integer);
    procedure RowCountChanged;

  public
    { Public declarations }
    procedure DirectExport;

  end;

var
  FrmExport: TFrmExport;

implementation

uses ExportClass, Map;

{$R *.DFM}
{--$I ..\hlp\gbmb.inc}


const
  MAXSPLITSIZE = (1024 * 1024 * 2); (* 2MB *)


procedure TFrmExport.DirectExport;
begin
  if ValidSettings then
    StartExport
  else
    ShowModal;
end;

procedure TFrmExport.StartExport;
var Exp : TMapExport;
var s, fn : string;
begin

  fn := ExpandRelativePath(ExportSettings.FileName, BaseDir);


  case CmbType.ItemIndex of
    0  :
    begin
      (* RGBDS ASM Source *)
      Exp := TRGBDSMapExport.Create(fn);
      Exp.MapExport(ExportSettings, TileMap, True);
      Exp.Free;

      s := copy( fn, 0, pos('.', fn)) + RGBDSIncExt;
      Exp := TRGBDSIncMapExport.Create(PChar(s));
      Exp.MapExport(ExportSettings, TileMap, False);
      Exp.Free;
    end;

    1 :
    begin
      (* RGBDS ASM Object *)
      Exp := TRGBDSObjMapExport.Create(fn);
      Exp.MapExport(ExportSettings, TileMap, True);
      Exp.Free;

      s := copy( fn, 0, pos('.', fn)) + RGBDSIncExt;
      Exp := TRGBDSIncMapExport.Create(PChar(s));
      Exp.MapExport(ExportSettings, TileMap, False);
      Exp.Free;
    end;
    2 :
    begin
      (* TASM ASM *)
      Exp := TASMMapExport.Create(fn);
      Exp.MapExport(ExportSettings, TileMap, True);
      Exp.Free;
    end;
    3 :
    begin
      (* GBDK C *)
      Exp := TGBDKMapExport.Create(fn);
      Exp.MapExport(ExportSettings, TileMap, True);
      Exp.Free;

      s := copy( fn, 0, pos('.', fn)) + GBDKHExt;
      Exp := TGBDKIncMapExport.Create(PChar(s));
      Exp.MapExport(ExportSettings, TileMap, True);
      Exp.Free;
    end;
    4:
    begin
      (* Binary *)
      Exp := TBinMapExport.Create(fn);
      Exp.MapExport(ExportSettings, TileMap, True);
      Exp.Free;
    end;
    else
    begin
      (* ISAS ASM Source *)
      Exp := TISASMapExport.Create(fn);
      Exp.MapExport(ExportSettings, TileMap, True);
      Exp.Free;

      s := copy( fn, 0, pos('.', fn)) + ISASIncExt;
      Exp := TISASIncMapExport.Create(PChar(s));
      Exp.MapExport(ExportSettings, TileMap, False);
      Exp.Free;
    end;
  end;

end;



function TFrmExport.ValidSettings: boolean;
(*************************************************)
(* Controleert of alle velden goed zijn ingevuld *)
(* voor het huidige exporttype, en plaatst een   *)
(* dialog als dit niet het geval is.             *)
(*************************************************)
var s   : string;
    Ctrl : TWinControl;
    b : boolean;
    i : integer;
    Page : integer;

  function ChkFileName: string;   begin if (trim(EdFileName.Text) = EmptyStr) then Result := '''Filename'; end;
  function ChkLabel: string;      begin if (trim(EdLabel.Text) = EmptyStr) then Result := '''Label'; end;
  function ChkSection: string;    begin if (trim(EdSection.Text) = EmptyStr) then Result := '''Section'; end;
  function ChkBank: string;       begin if (trim(EdBank.Text) = EmptyStr) then Result := '''Bank'; end;
  function ChkSplitSize: string;  begin if ((ChkSplit.Checked) and (trim(EdSplitSize.Text) = EmptyStr)) then Result := '''Block size'; end;
  function ChkTileOffset: string; begin if (trim(EdTileOffset.Text) = EmptyStr) then Result := '''Tile offset'; end;

begin

  Page := 0;
  ForceExtension;

  (* validate blank fields *)
  s := ChkFileName;                   (* always mandatory *)
  Ctrl := EdFileName;
  if (s = EmptyStr) then
  begin
    s := ChkSplitSize;                (* always mandatory *)
    Ctrl := EdSplitSize;

    if (s = EmptyStr) then
    begin
      case CmbType.ItemIndex of
        0,1   : begin  (* RGBDS Asm/Obj *)
                  s := ChkLabel; Ctrl := EdLabel;
                  if (s = EmptyStr) then begin s := ChkSection; Ctrl := EdSection; end;
                  if (s = EmptyStr) then begin s := ChkBank; Ctrl := EdBank; end;
                end;
        2,3   : begin  (* TAsm / GBDK C *)
                  s := ChkLabel; Ctrl := EdLabel;
                end;
        4     : begin  (* Binary file *)
                end;
        else    begin  (* ISAS Asm *)
                  s := ChkLabel; Ctrl := EdLabel;
                  if (s = EmptyStr) then begin s := ChkSection; Ctrl := EdSection; end;
                  if (s = EmptyStr) then begin s := ChkBank; Ctrl := EdBank; end;
                end;
      end;
    end;
  end;

  if (s = EmptyStr) then
  begin
    Page := 1;
    s := ChkTileOffset;               (* always mandatory *)
    Ctrl := EdTileOffset;
  end;


  (* blank fields ? *)
  if (s <> EmptyStr) then
    s := s + ''' is mandatory for this export type.';

  (* Split Size is correct ? *)
  if (s = EmptyStr) then
  begin
    Page := 0;
    if (ChkSplit.Checked) then
    begin
      Ctrl := EdSplitSize;

      if (StrToInt(EdSplitSize.Text) < 1) then
        s := '''Block size'' should be at least 1.';

      if (StrToInt(EdSplitSize.Text) > MAXSPLITSIZE) then
        s := '''Block size'' should be lower or equal to ' + IntToStr(MAXSPLITSIZE) + '.';
    end;

    (* at least one property available ? *)
    if (s = EmptyStr) then
    begin
      Page := 1;

      b := False;
      with GrdProp do
        for i:= 1 to RowCount-1 do
          if ((Cells[1,i] <> '') and (Cells[2,i] <> ''))  then b := True;

      if (not b) then
      begin
        s := '''Location format'' should have at least one property selected.';
        ctrl := GrdProp;
      end;
    end;
  end;

  (* any errors ? *)
  Result := (s = EmptyStr);
  if (not Result) then
  begin
    Msg(s, msgError, MB_OK);
    if Visible then
    begin
      PageCtrl.ActivePage := PageCtrl.Pages[Page];
      Ctrl.SetFocus;
    end;
    ModalResult := mrNone;
  end;

end;



procedure TFrmExport.FormCreate(Sender: TObject);
var i : integer;
begin
//  HelpContext := Export_Options;

  cboPlaneCnt.ItemIndex := 1;
  cboOrder.ItemIndex := 0;

  with FrmMap do
  begin
    EdBank.OnChange       := GlobalNumericEdChange;
    EdSplitSize.OnChange  := GlobalNumericEdChange;
    EdTileOffset.OnChange := GlobalNumericEdChange;
  end;

  (* get extensions from ini-file *)
  with IniFile do
  begin
    RGBDSAsmExt   := Lowercase(ReadString('RGBDS', 'AsmExt', 'z80'));
    RGBDSIncExt   := Lowercase(ReadString('RGBDS', 'IncExt', 'inc'));
    RGBDSObjExt   := Lowercase(ReadString('RGBDS', 'ObjExt', 'obj'));
    GeneralBinExt := Lowercase(ReadString('General', 'BinExt', 'bin'));
    GBDKCExt      := Lowercase(ReadString('GBDK', 'CExt', 'c'));
    GBDKHExt      := Lowercase(ReadString('GBDK', 'HExt', 'h'));
    TASMAsmExt    := Lowercase(ReadString('TASM', 'AsmExt', 'z80'));
    ISASAsmExt    := Lowercase(ReadString('ISAS', 'AsmExt', 's'));
    ISASIncExt    := Lowercase(ReadString('ISAS', 'IncExt', 'inc'));
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
  CmbType.ItemIndex := 0;


  (* setup property selection-list *)
  FLstProps := TStringList.Create;
  FLstProps.Add('');
  FLstProps.Add('[Tile number]');
  FLstProps.Add('[Tile number: Low 8]');
  FLstProps.Add('[Tile number: High 9]');
  FLstProps.Add('[Vertical flip]');
  FLstProps.Add('[Horizontal flip]');
  FLstProps.Add('[GBC Palette]');
  FLstProps.Add('[SGB Palette]');
  FLstProps.Add('[GBC BG Attribute]');
  FLstProps.Add('[0 filler]');
  FLstProps.Add('[1 filler]');

  for i := 0 to TileMap.PropCount-1 do
    FLstProps.Add(TileMap.PropDef[i].Name);


  (* setup property grid *)
  with GrdProp do
  begin
    Cells[1,0] := 'Property';
    Cells[2,0] := 'Bits';

    Columns.Items[0].Alignment := taCenter;

    Columns.Items[1].ButtonStyle := cbsDatalist;
    Columns.Items[1].Picklist := FLstProps;
  end;

  (* setup tabs *)
  with PageCtrl do
  begin
    Pages[0].Caption := 'Standard';
    Pages[1].Caption := 'Location format';
  end;

  LoadSettings;
end;


procedure TFrmExport.SaveSettings;
var i : integer;
begin

  with ExportSettings do
  begin
    FileName          := EdFileName.Text;
    FileType          := CmbType.ItemIndex;
    SectionName       := EdSection.Text;
    LabelName         := EdLabel.Text;
    Bank           := SaveStrToInt(EdBank.Text);
    PlaneCnt       := CboPlaneCnt.ItemIndex;
    PlaneOrder     := CboOrder.ItemIndex;
    MapLayout      := CboMap.ItemIndex;
    SelTab         := PageCtrl.ActivePage.TabIndex;
    Split          := ChkSplit.Checked;
    SplitSize      := SaveStrToInt(EdSplitSize.Text);
    SplitBank      := ChkSplitBank.Checked;
    TileOffset     := SaveStrToInt(EdTileOffset.Text);

    ClearProps;
    for i := 0 to GrdProp.RowCount-2 do
      NewProp( FLstProps.IndexOf(GrdProp.cells[1,i+1]),
               SaveStrToInt(GrdProp.Rows[i+1].strings[2]) );
  end;

  Modified := True;
end;


procedure TFrmExport.LoadSettings;
var i : integer;
begin

    with ExportSettings do
    begin
      EdFileName.Text       := FileName;
      CmbType.ItemIndex     := FileType;
      CmbTypeChange(nil);                   (* BUG?: force direct refresh *)
      EdSection.Text        := SectionName;
      EdLabel.Text          := LabelName;
      EdBank.Text           := IntToStr(Bank);
      CboPlaneCnt.ItemIndex := PlaneCnt;
      CboOrder.ItemIndex    := PlaneOrder;
      CboMap.ItemIndex      := MapLayout;
      PageCtrl.ActivePage   := PageCtrl.Pages[SelTab];
      ChkSplit.Checked      := Split;
      ChkSplitClick(ChkSplit);              (* BUG?: force direct refresh *)
      EdSplitSize.Text      := IntToStr(SplitSize);
      ChkSplitBank.Checked  := SplitBank;
      EdTileOffset.Text     := IntToStr(TileOffset);

      (* setup prop grid *)
      i := PropCount + 1;
      if (i < 2) then i := 2;
      GrdProp.RowCount := i;

      for i := 0 to PropCount-1 do
      begin
        SetPropRow(i+1, Props[i].Nr);
        GrdProp.Cells[2,i+1] := IntToStr( Props[i].Size);
      end;

      RowCountChanged;
    end;
end;


procedure TFrmExport.SetPropRow( Row, PropNr : integer);
var i,j : integer;
begin
  i := PropNr;
  if (PropNr >= FLstProps.Count) or (PropNr < 0) then i := 0;

  GrdProp.Cells[1,Row] :=  FLstProps.Strings[i];

  (* set default size *)
  case (i) of
    0        :  j := 0;
    1        :  j := BitsNeeded(TileList.Count-1);
    2        :  j := 8;
    3        :  j := 1;
    4,5      :  j := 1;
    6        :  j := 3;
    7        :  j := 2;
    8        :  j := 8;
    9,10     :  j := 1;
    else        j := BitsNeeded(TileMap.PropDef[i-SYSPROPCNT].Size);
  end;
  GrdProp.Cells[2,Row] :=  IntToStr(j);
end;


function TFrmExport.BuildGrdResult: boolean;
var i,j : integer;
    PlaneCnt : integer;
    BitCnt   : integer;
    p,b :integer;
begin

  if (CboPlaneCnt.ItemIndex = 0) then
  begin
    PlaneCnt := 1;
    BitCnt := 4;
  end
  else
  begin
    PlaneCnt := CboPlaneCnt.ItemIndex;
    BitCnt := 8;
  end;

  (* draw fixed cols/rows *)
  for i:= 0 to 7 do
    with GrdResult.Rows[0] do
      if (i < BitCnt) then
        Strings[i+1] := IntToStr(i)
      else
        Strings[i+1] := '';

  for i:= 0 to 3 do
    with GrdResult.Cols[0] do
      if (i < PlaneCnt) then
        Strings[i+1] := IntToStr(i)
      else
        Strings[i+1] := '';


  (* fill grid *)
  p := 0;
  b := 0;
  with GrdProp do
    for i := 1 to RowCount-1 do
      if (Cells[2,i] <> '') and (StrToInt(Cells[2,i]) < 17) then
        for j := 0 to StrToInt(Cells[2,i])-1 do
          if (p < PlaneCnt) then
          begin
            GrdResult.Cells[b+1,p+1] := IntToStr(i);

            Inc(b);
            if (b >= BitCnt) then
            begin
              Inc(p);
              b := 0;
            end;
          end;

    (* clean up *)
    while (p < 4) do
    begin
      GrdResult.Cells[b+1,p+1] := '';
      Inc(b);
      if (b >= 8) then
      begin
        Inc(p);
        b := 0;
      end;
    end;

    if (CboPlaneCnt.ItemIndex = 0) then
      for i := 5 to 8 do
        GrdResult.Cells[i,1] := '';

  Result := True;
end;



procedure TFrmExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmExport.FormDestroy(Sender: TObject);
begin
  FLstProps.Free;

  FrmExport := nil;
end;

procedure TFrmExport.CmbTypeChange(Sender: TObject);

  procedure SetCtrls(l,s,b : boolean);
  begin
    LblLabel.Enabled := l;
    EdLabel.Enabled  := l;
    LblSection.Enabled := s;
    EdSection.Enabled := s;
    LblBank.Enabled := b;
    EdBank.Enabled := b;
    ChkSplitBank.Enabled := (b and ChkSplit.Checked);
  end;

begin
  ForceExtension;

  (* enable/disable fields *)
  case CmbType.ItemIndex of
    0,1  : SetCtrls(True, True, True);
    2    : SetCtrls(True, False, False );
    3    : SetCtrls(True, False, True );
    4    : SetCtrls(False, False, False );
    else   SetCtrls(True, True, True);
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
      4    : s := 'All-purpose binary files';
      else   s := 'ISAS Assembly files';
    end;
    Filter := s + ' (*.' + IndexToExtension + ')|*.' + IndexToExtension;

    DefaultExt := IndexToExtension;
    FileName := LoadString( EdFilename.Text, '*.' + IndexToExtension );

    if Execute then
    begin
      EdFileName.Text := GetRelativePath( FileName );
      ForceExtension;
    end;
  end;

  (* correct possible corruption *)
  ResetBaseDir;
end;

procedure TFrmExport.BtnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;



procedure TFrmExport.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

procedure TFrmExport.BtnAddClick(Sender: TObject);
var i : integer;
begin
  with GrdProp do
    if (RowCount < EXPORTPROPMAX+2) then
    begin
      RowCount := RowCount+1;

      (* Move Grid *)
      for i := RowCount-2 downto Row do Rows[i+1] := Rows[i];
      Cells[1,Row] := '';
      Cells[2,Row] := '0';
    end;

  RowCountChanged;
end;

procedure TFrmExport.BtnDelClick(Sender: TObject);
var i : integer;
begin
  with GrdProp do
    if (RowCount > 2) then
    begin
      (* Move Grid *)
      for i := Row to RowCount-2 do Rows[i] := Rows[i+1];
      RowCount := RowCount-1;
      SetFocus;              (* force update *)
    end;

  RowCountChanged;
end;


procedure TFrmExport.RowCountChanged;
var i : integer;
begin
  with GrdProp do
  begin
    if (((RowCount) * DefaultRowHeight) > Height) then
      ColWidths[2] := 30
    else
      ColWidths[2] := 47;

    (* rebuild prop nr's *)
    for i := 1 to RowCount-1 do Cells[0,i] := IntToStr(i);
  end;

  BtnAdd.Enabled := (GrdProp.RowCount < EXPORTPROPMAX+2);
  BtnDel.Enabled := (GrdProp.RowCount > 2);
  BuildGrdResult;
end;

procedure TFrmExport.GrdPropSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  if (ACol = 1) then
    with GrdProp do SetPropRow( Row, FLstProps.IndexOf(Cells[1, Row]) );

  BuildGrdResult;
end;

procedure TFrmExport.GrdPropKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
    case Key of
      VK_INSERT :
        BtnAdd.Click;

      VK_DELETE :
        if BtnDel.Enabled then
        begin
          BtnDel.SetFocus;    (* change focus to force correct update of grid *)
          BtnDel.Click;
        end;
    end;
end;

procedure TFrmExport.GrdPropKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key in AlphaSet) and (GrdProp.Col = 2) then Key := #0;
end;

procedure TFrmExport.ChkSplitClick(Sender: TObject);
begin
  EdSplitSize.Enabled := ChkSplit.Checked;
  LblSplitSize.Enabled := ChkSplit.Checked;
  ChkSplitBank.Enabled := (ChkSplit.Checked and EdBank.Enabled);
end;

procedure TFrmExport.CboPlaneCntChange(Sender: TObject);
begin
  BuildGrdResult;
end;

procedure TFrmExport.BtnOKClick(Sender: TObject);
begin
  if ValidSettings then
  begin
    SaveSettings;
    StartExport;
  end;
end;


end.
