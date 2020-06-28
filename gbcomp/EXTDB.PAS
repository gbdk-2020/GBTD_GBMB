unit Extdb;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, Wwdbigrd, Wwdbgrid, wwTable, wwdblook, DBTables;


  {******************}
  {*                *}
  {*   TExtDBGrid   *}
  {*                *}
  {******************}

type
  TExtDBGrid = class(TwwDBGrid)
  private
    { Private declarations }
    PInfoStatus : integer;
    FOnColumnMoved : TMovedEvent;

    procedure SetInfoStatus(infotype : integer);
    procedure DrawInfoStatus;
    procedure DrawSortMarker( ARect : TRect; level : integer);

  protected
    { Protected declarations }
    procedure DblClick; override;
{ procedure ColWidthsChanged; override;}
    {ColumnMoved(FromIndex, ToIndex : longint); }

  public
    { Public declarations }

    property InfoStatus : integer read PInfoStatus write SetInfoStatus;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

  published
    { Published declarations }
{    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
 }

  end;



  {*****************}
  {*               *}
  {*   TExtTable   *}
  {*               *}
  {*****************}

const MAXSORTFIELDS = 3;

type
  TExtTable = class(TwwTable)
  private
    { Private declarations }
    SortFields : array [0..MAXSORTFIELDS-1] of string;
    FOnSortChange : TNotifyEvent;


    procedure ResetIndex;

  protected
    { Protected declarations }


  public
    { Public declarations }
    function AddSortField(FieldName : string): integer;
    function DelSortField(FieldName : string): integer;
    function FindSortField(FieldName : string): integer;
    function GetSortField( nr : integer): string;
    procedure ClearSortFields;
    procedure AddSortList( FieldNames : TStringList);
    constructor Create(AOwner : TComponent); override;

  published
    { Published declarations }

    property OnSortChange : TNotifyEvent read FOnSortChange write FOnSortChange;

  end;



{*********************}
{*                   *}
{*   TEDistinctBox   *}
{*                   *}
{*********************}

type
  TEDistinctBox = class(TwwDBLookupCombo)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;





procedure Register;

{$R GRIDINFO.RES}

implementation

procedure Register;
begin
  RegisterComponents('Extensions', [TExtDBGrid]);
  RegisterComponents('Extensions', [TExtTable]);
  RegisterComponents('Extensions', [TEDistinctBox]);
end;



procedure TExtDBGrid.DblClick;
{***********************************************}
{* Handler voor sortering-selectie op titlebar *}
{***********************************************}
var p : TPoint;
    g : TGridCoord;
begin
  {* Default handler *}
  inherited DblClick;

  {* Bepalen van cell *}
  GetCursorPos(p);
  p := ScreenToClient(p);
  g := MouseCoord(p.x, p.y);

  {* Titlebar => Intern afhandelen *}
  if (g.y = 0) and (g.x > 0) and (DataSource.DataSet is TExtTable) then
  begin

    {* Toggle sortering *}
    with TExtTable(DataSource.DataSet) do
      if (FindSortField( Self.Fields[g.x-1].FieldName) = -1) then
        AddSortField( Self.Fields[g.x-1].FieldName )
      else
        DelSortField( Self.Fields[g.x-1].FieldName );

  end;

end;





procedure TExtDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
{*******************************************************}
{* Zorgt dat InfoStatus/ SortMarkers altijd goed staan *}
{*******************************************************}
var i: integer;
begin
  {* default handler *}
  inherited DrawCell(ACol, ARow, ARect, AState);

  {* Afhandeling van titlebar *}
  if (DataSource <> nil) and (ARow = 0) then
  begin
    {* Als de cell linksboven is verandert, dan InfoStatus opnieuw afdrukken *}
    if (ACol = 0) then
      DrawInfoStatus
    else
       {* Sort-markers *}
      if (DataSource.DataSet is TExtTable) then
      begin
        i := TExtTable(DataSource.DataSet).FindSortField(GetColField(ACol-1).fieldname);
        if (i <> -1) then
          DrawSortMarker(ARect, i);
      end;
  end;
end;



procedure TExtDBGrid.SetInfoStatus(infotype : integer);
{*******************************************}
{* Property-changer: update ook het scherm *}
{*******************************************}
begin
  {* Update property *}
  PInfoStatus := infotype;

  {* Update scherm *}
  DrawInfoStatus;
end;



procedure TExtDBGrid.DrawInfoStatus;
{*******************************}
{* Tekent InfoStatus op scherm *}
{* Mogelijkheden:              *}
{*   0 = Geen                  *}
{*   1 = Vergrootglas (filter) *}
{*******************************}
var BMP : TBitmap;
    b,p : tColor;
begin

  if (pInfoStatus = 0) then
    {* geen status : blok leeghalen *}
    with canvas do
    begin
      {* Kleuren bewaren *}
      b := Brush.Color;
      p := Pen.Color;

      {* blok tekenen *}
      Brush.Color := clBtnFace;
      Pen.Color   := clBtnFace;
      Rectangle(1,1,11,20);

      {* Kleuren terugplaatsen *}
      Brush.Color := b;
      Pen.Color   := p;
    end
  else
  begin
    {* Momenteel alleen filter als status *}
    BMP := TBitmap.Create;
    BMP.Handle := LoadBitmap(hinstance, 'LOOKGLASS');
    Canvas.Draw(1,1,BMP);
    BMP.Free;
  end;
end;



procedure TExtDBGrid.DrawSortMarker( ARect : TRect; level : integer);
{********************************}
{* Tekent SortMarkers op scherm *}
{********************************}
var BMP : TBitmap;
    p   : PChar;
begin

  {* Bepalen van naam *}
  p := StrAlloc(60);
  StrPCopy(p, 'SORT' + IntToStr(Level+1) );

  {* Afdrukken *}
  BMP := TBitmap.Create;
  BMP.Handle := LoadBitmap( hinstance, p );
  Canvas.Draw(ARect.right-BMP.Width, 0, BMP);
  BMP.Free;

  StrDispose(p);
end;


constructor TExtTable.Create(AOwner : TComponent); 
begin
  inherited Create(AOwner);

  (* Reset sortfields *)
  SortFields[0] := '';
  SortFields[1] := '';
  SortFields[2] := '';
end;



procedure TExtTable.ClearSortFields;
(******************************)
(* Verwijdert alle SortFields *)
(******************************)
begin
  SortFields[0] := '';
  SortFields[1] := '';
  SortFields[2] := '';

  ResetIndex;
end;



procedure TExtTable.AddSortList( FieldNames : TStringList);
(******************************************)
(* Vult sortering op basis van FieldNames *)
(* String #0 = Sortering #0, enz          *)
(******************************************)
begin
  (* Leeggooien huidige sortering *)
  SortFields[0] := '';
  SortFields[1] := '';
  SortFields[2] := '';

  (* Vullen *)
  with FieldNames do
  begin
    if (FieldNames.Count > 0) then
    begin
      SortFields[0] := FieldNames.Strings[0];
      if (FieldNames.Count > 1) then
      begin
        SortFields[1] := FieldNames.Strings[1];
        if (FieldNames.Count > 2) then
          SortFields[2] := FieldNames.Strings[2];
      end;
    end;
  end;

  ResetIndex;
end;


function TExtTable.AddSortField(FieldName : string): integer;
{**************************************}
{* Toevoegen van veld aan sortering.  *}
{* Wordt altijd achteraan geplaatst.  *}
{* Terug : -1 = niet gelukt           *}
{*         N  = locatie in sortering. *}
{**************************************}
var i : integer;
begin

  {* zoeken naar eerste vrije plek *}
  i := 0;
  while (SortFields[i] <> '') and (i < MAXSORTFIELDS) do Inc(i);

  {* Plaatsen van index *}
  if (i < MAXSORTFIELDS) then
  begin
    SortFields[i] := UpperCase(FieldName);
    ResetIndex;
  end
  else
    i := -1;

  {* Einde *}
  result := i;
end;



function TExtTable.FindSortField(FieldName : string): integer;
{**************************************}
{* Zoeken van veld in sortering.      *}
{* Terug : -1 = niet gevonden         *}
{*         N  = locatie in sortering. *}
{**************************************}
var i : integer;
begin
  result := -1;
  for i := 0 to MAXSORTFIELDS-1 do
    if (SortFields[i] = FieldName) then
      result := i;
end;



function TExtTable.GetSortField(nr : integer): string;
{**********************************************}
{* Geeft veldnaam van sortering #nr (vanaf 0) *}
{* Terug : '' = niet beschikbaar              *}
{*         <> = Veldnaam                      *}
{**********************************************}
begin
  if (nr < MAXSORTFIELDS) then
    result := SortFields[nr]
  else
    result := '';
end;




function TExtTable.DelSortField(FieldName : string): integer;
{*******************************************}
{* Verwijderen van veld uit sortering.     *}
{* velden worden automatisch aangeschoven. *}
{* Terug : -1 = niet gelukt                *}
{*         N  = locatie in sortering.      *}
{*******************************************}
var i : integer;
begin

  Result := -1;
  if (SortFields[0] = FieldName) then
  begin
    SortFields[0] := SortFields[1];
    SortFields[1] := SortFields[2];
    SortFields[2] := '';
    Result := 0;
    ResetIndex;
  end;

  if (SortFields[1] = FieldName) then
  begin
    SortFields[1] := SortFields[2];
    SortFields[2] := '';
    Result := 1;
    ResetIndex;
  end;

  if (SortFields[2] = FieldName) then
  begin
    SortFields[2] := '';
    Result := 2;
    ResetIndex;
  end;
end;



procedure TExtTable.ResetIndex;
{***************************************************}
{* Stelt index in op basis van SortFields.         *}
{* Indien nodig wordt een nieuwe index aangemaakt. *}
{***************************************************}
var i    : integer;
    s,t  : string;
begin

  {* Opbouwen van index-list *}
  if SortFields[0] <> '' then
    s := SortFields[0]
  else
    s := '';

  for i := 1 to MAXSORTFIELDS-1 do
    if SortFields[i] <> '' then
      s := s + ';' + SortFields[i];


  {* Aanzetten/opbouwen van index *}
  try
    IndexFieldNames := s;

  except
    {* Aanmaken van index-naam *}
    t := 'A';
    i := 0;
    for i := 0 to MAXSORTFIELDS-1 do
      if SortFields[i] <> '' then
        t := t + SortFields[i];

    {* Opbouwen van index *}
    DisableControls;
    try
      Close;
      AddIndex(t, s, [ixCaseInsensitive]);
      IndexFieldNames := s;
      Open;
    finally
      EnableControls;
    end;
  end;

  if Assigned(FOnSortChange) then
    OnSortChange(self);

end;

end.
