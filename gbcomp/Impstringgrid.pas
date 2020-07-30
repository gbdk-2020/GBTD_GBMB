unit Impstringgrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids,Stdctrls;


type
  TImpgridCellEvent = procedure (Sender: TObject; Col, Row: Longint) of object;
  TImpgriddropdownEvent = procedure (Sender: TObject; Col, Row: Longint;
                                    var Picklist:Tstrings) of object;
   TImpColumnValue = (cvColor, cvWidth, cvFont, cvAlignment, cvReadOnly, cvTitleColor,
    cvTitleCaption, cvTitleAlignment, cvTitleFont, cvImeMode, cvImeName);
  TImpColumnValues = set of TImpColumnValue;


{ TImpColumn defines internal storage for column attributes.  Values assigned
  to properties are stored in this object, the grid- or field-based default
  sources are not modified.  Values read from properties are the previously
  assigned value, if any, or the grid- or field-based default values if
  nothing has been assigned to that property. This class also publishes the
  column attribute properties for persistent storage.  }
  TImpGridColumnsState = (csDefault, csCustomized);
  TImpColumn = class;
  TImpStringgrid = class;
  TImpColumnClass = class of TImpColumn;

  TImpColumnTitle = class(TPersistent)
  private
    FColumn: TImpColumn;
    FCaption: string;
    FFont: TFont;
    FColor: TColor;
    FAlignment: TAlignment;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetCaption: string;
    function GetFont: TFont;
    function IsAlignmentStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsCaptionStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string); virtual;
  protected
    procedure RefreshDefaultFont;
  public
    constructor Create(Column: TImpColumn);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    function DefaultCaption: string;
    procedure RestoreDefaults; virtual;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment
      stored IsAlignmentStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
  end;

  TImpColumnButtonStyle = (cbsPicklist, cbsEllipsis, cbsDataList, cbsNone);

  TImpColumn = class(TCollectionItem)
  private
    FColor: TColor;
    FWidth: Integer;
    FTitle: TImpColumnTitle;
    FFont: TFont;
    FImeMode: TImeMode;
    FImeName: TImeName;
    FPickList: TStrings;
 // FPopupMenu: TPopupMenu;
    FDropDownRows: Cardinal;
    FButtonStyle: TImpColumnButtonStyle;
    FAlignment: TAlignment;
    FReadonly: Boolean;
    FAssignedValues: TImpColumnValues;
    procedure FontChanged(Sender: TObject);
    function  GetAlignment: TAlignment;
    function  GetColor: TColor;
//  function  GetField: TField;
    function  GetFont: TFont;
    function  GetImeMode: TImeMode;
    function  GetImeName: TImeName;
    function  GetPickList: TStrings;
    function  GetReadOnly: Boolean;
    function  GetWidth: Integer;
    function  IsAlignmentStored: Boolean;
    function  IsColorStored: Boolean;
    function  IsFontStored: Boolean;
    function  IsImeModeStored: Boolean;
    function  IsImeNameStored: Boolean;
    function  IsReadOnlyStored: Boolean;
    function  IsWidthStored: Boolean;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetButtonStyle(Value: TImpColumnButtonStyle);
    procedure SetColor(Value: TColor);
//  procedure SetField(Value: TField); virtual;
//  procedure SetFieldName(const Value: String);
    procedure SetFont(Value: TFont);
    procedure SetImeMode(Value: TImeMode); virtual;
    procedure SetImeName(Value: TImeName); virtual;
    procedure SetPickList(Value: TStrings);
//  procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetTitle(Value: TImpColumnTitle);
    procedure SetWidth(Value: Integer); virtual;
  protected
    function  CreateTitle: TImpColumnTitle; virtual;
    function  GetGrid: TImpStringGrid;
    function GetDisplayName: string; override;
    procedure RefreshDefaultFont;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  DefaultAlignment: TAlignment;
    function  DefaultColor: TColor;
    function  DefaultFont: TFont;
    function  DefaultImeMode: TImeMode;
    function  DefaultImeName: TImeName;
    function  DefaultReadOnly: Boolean;
    function  DefaultWidth: Integer;
    procedure RestoreDefaults; virtual;
    property  Grid: TImpStringGrid read GetGrid;
    property  AssignedValues: TImpColumnValues read FAssignedValues;
 // property  Field: TField read GetField write SetField;
  published
    property  Alignment: TAlignment read GetAlignment write SetAlignment
      stored IsAlignmentStored;
    property  ButtonStyle: TImpColumnButtonStyle read FButtonStyle write SetButtonStyle
      default cbsNone;
    property  Color: TColor read GetColor write SetColor stored IsColorStored;
    property  DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 7;
//    property  FieldName: String read FFieldName write SetFieldName;
    property  Font: TFont read GetFont write SetFont stored IsFontStored;
    property  ImeMode: TImeMode read GetImeMode write SetImeMode stored IsImeModeStored;
    property  ImeName: TImeName read GetImeName write SetImeName stored IsImeNameStored;
    property  PickList: TStrings read GetPickList write SetPickList;
  //  property  PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property  ReadOnly: Boolean read GetReadOnly write SetReadOnly
      stored IsReadOnlyStored;
    property  Title: TImpColumnTitle read FTitle write SetTitle;
    property  Width: Integer read GetWidth write SetWidth stored IsWidthStored;
  end;

TImpGridColumns = class(TCollection)
  private
    FGrid: TImpStringGrid;
    function GetCount:Integer;
    function GetState: TImpGridColumnsState;
    function GeTImpColumn(Index: Integer): TImpColumn;
    procedure SeTImpColumn(Index: Integer; Value: TImpColumn);
    procedure SetState(NewState: TImpGridColumnsState);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TImpStringGrid; ColumnClass: TImpColumnClass);
    function  Add: TImpColumn;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure RestoreDefaults;
    procedure RebuildColumns;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream);
    property State: TImpGridColumnsState read GetState write SetState;
    property Grid: TImpStringGrid read FGrid;
    property Items[Index: Integer]: TImpColumn read GeTImpColumn write SeTImpColumn; default;
    property Count: integer read GetCount;
  end;

{ TImpGridInplaceEdit }

{ TImpGridInplaceEdit adds support for a button on the in-place editor,
  which can be used to drop down a table-based lookup list, a stringlist-based
  pick list, or (if button style is esEllipsis) fire the grid event
  OnEditButtonClick.  }

  TEditStyle = (esSimple, esEllipsis, esPickList, esDataList);
  TPopupListbox = class;

  TImpStringgrid = class(TStringGrid)
  private
    fOntoomuch:TnotifyEvent;
    fOnNewRow:TnotifyEvent;
    fOnElippsisclicked:TImpgridCellEvent;
    fOnPicklistDropdown:TImpgridDropDownEvent;
    fToomuch:boolean;
    function GetColCount: Integer;
    procedure SetColCount(Col: LongInt);
    procedure Editbuttonclick;
    { Private declarations }
  protected
    { Protected declarations }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function  CreateEditor: TInplaceEdit; override;

    (* HAR : Extentions *)
    function CanEditShow: Boolean; override;
//    function CanEditAcceptKey(Key: Char): Boolean; override;

  public
    { Public declarations }
    Columns: TImpGridColumns;
    constructor Create(Owner: TComponent); override;
  published
    property OnTooMuchrows : TNotifyEvent read fOntoomuch write fOntoomuch;
    property OnNewRow : TNotifyEvent read fOnNewRow write fOnNewRow;
    property OnElippsisclicked : TImpGridCellEvent read fOnElippsisclicked write fOnElippsisclicked;
    property OnPicklistDropdown : TImpGridDropdownEvent read fOnPicklistDropdown write fOnPicklistDropdown;
    property ColCount: Longint read GetColCount write SetColCount default 5;
    { Published declarations }
  end;

  TImpGridInplaceEdit = class(TInplaceEdit)
  private
    FButtonWidth: Integer;
    //FDataList: TDBLookupListBox;
    FPickList: TPopupListbox;
    FActiveList: TWinControl;
    //FLookupSource: TDatasource;
    FEditStyle: TEditStyle;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetEditStyle(Value: TEditStyle);
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;


//    procedure WMSetText( var Message: TWMSetText); message EM_REPLACESEL;//WM_SetText;

  protected
    procedure BoundsChanged; override;
    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
    property  EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property  ActiveList: TWinControl read FActiveList write FActiveList;
    //property  DataList: TDBLookupListBox read FDataList;
    property  PickList: TPopupListbox read FPickList;



//    function EditCanModify: Boolean; override;

function GetClosestItem(s : string ) : integer;




  public
    constructor Create(Owner: TComponent); override;



  end;

{ TPopupListbox }

  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

const
  ColumnTitleValues = [cvTitleColor..cvTitleFont];
  cm_DeferLayout = WM_USER + 100;


procedure Register;

implementation
var
  DrawBitmap: TBitmap;
  UserCount: Integer;

procedure UsesBitmap;
begin
  if UserCount = 0 then
    DrawBitmap := TBitmap.Create;
  Inc(UserCount);
end;

procedure ReleaseBitmap;
begin
  Dec(UserCount);
  if UserCount = 0 then DrawBitmap.Free;
end;

function Max(X, Y: Integer): Integer;
begin
  Result := Y;
  if X > Y then Result := X;
end;

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment);
const
  AlignFlags : array [TAlignment] of Integer =
    ( DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX );
var
  B, R: TRect;
  I, Left: Integer;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if (GetNearestColor(ACanvas.Handle, I) = COLORREF(I)) then
  begin                       { Use ExtTextOut for solid colors }
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
    ExtTextOut(ACanvas.Handle, Left, ARect.Top + DY, ETO_OPAQUE or
      ETO_CLIPPED, @ARect, PChar(Text), Length(Text), nil);
  end
  else begin                  { Use FillRect and Drawtext for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin                     { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]);
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
end;

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TPopupListbox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TImpGridInplaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and
      (X < Width) and (Y < Height));
end;


constructor TImpGridInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  //FLookupSource := TDataSource.Create(Self);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle := esEllipsis;
end;

procedure TImpGridInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if FEditStyle <> esSimple then Dec(R.Right, FButtonWidth);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.Fareast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

procedure TImpGridInplaceEdit.CloseUp(Accept: Boolean);
var
  //MasterField: TField;
  ListValue: Variant;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
{    if FActiveList = FDataList then
      ListValue := FDataList.KeyValue
    else }
      if FPickList.ItemIndex <> -1 then
        ListValue := FPickList.Items[FPicklist.ItemIndex];
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept then
       if (not VarIsNull(ListValue)) and EditCanModify then
       begin
         Text := ListValue;
         (* HAR: force immediate update of grid-data *)
         Grid.Perform(WM_COMMAND, EN_CHANGE shl 16, Self.Handle);
       end;
  end;
end;

procedure TImpGridInplaceEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if FListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if FListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TImpGridInplaceEdit.DropDown;
var
  P: TPoint;
  I,J,Y: Integer;
  Column: TImpColumn;
  str : Tstrings;
begin
  if not FListVisible and Assigned(FActiveList) then
  begin
    FActiveList.Width := Width;
    with TImpStringGrid(Grid) do
      Column := Columns[col];
    begin
      FPickList.Color := Color;
      FPickList.Font := Font;
      FPickList.Items := Column.Picklist;
      if assigned(TImpStringgrid(Grid).OnPickListDropDown) then begin
        str := Tstringlist.create;
        str.assign(fpicklist.items);
        TImpStringgrid(Grid).OnPickListDropDown(Self,TImpStringgrid(Grid).col,TImpStringgrid(Grid).row,
        str);
        fpicklist.items.Assign(str);
      end;
      if FPickList.Items.Count >= Column.DropDownRows then
        FPickList.Height := Column.DropDownRows * FPickList.ItemHeight + 4
      else
        FPickList.Height := FPickList.Items.Count * FPickList.ItemHeight + 4;
//      if Column.Field.IsNull then
        FPickList.ItemIndex := -1;
//      else
//        FPickList.ItemIndex := FPickList.Items.IndexOf(Column.Field.Value);
      J := FPickList.ClientWidth;
      for I := 0 to FPickList.Items.Count - 1 do
      begin
        Y := FPickList.Canvas.TextWidth(FPickList.Items[I]);
        if Y > J then J := Y+10;
      end;
      FPickList.ClientWidth := J;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FActiveList.Height > Screen.Height then Y := P.Y - FActiveList.Height;
    SetWindowPos(FActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

type
  TWinControlCracker = class(TWinControl) end;

procedure TImpGridInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    TImpStringgrid(Grid).EditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else
    if (EditStyle = esDataList) then
    begin
      KillMessage(Handle, WM_CHAR);
    end
    else
      inherited KeyDown(Key, Shift);
end;

procedure TImpGridInplaceEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TImpGridInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(X,Y)) then
  begin
    if FListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(FActiveList) then DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TImpGridInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TImpGridInplaceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and (FEditStyle = esEllipsis) and WasPressed then
    TImpStringGrid(Grid).EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TImpGridInplaceEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W: Integer;
begin
  if FEditStyle <> esSimple then
  begin
    SetRect(R, Width - FButtonWidth, 0, Width, Height);
    Flags := 0;
    if FEditStyle in [esDataList, esPickList] then
    begin
      if FActiveList = nil then
        Flags := DFCS_INACTIVE
      else if FPressed then
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else   { esEllipsis }
    begin
      if FPressed then
        Flags := BF_FLAT;
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      Flags := ((R.Right - R.Left) shr 1) + Ord(FPressed);
      W := Height shr 3;
      if W = 0 then W := 1;
      PatBlt(DC, R.Left + Flags, R.Top + 10, 1, 1, BLACKNESS);
      PatBlt(DC, R.Left + Flags-3, R.Top + 10, 1, 1, BLACKNESS);
      PatBlt(DC, R.Left + Flags+3, R.Top + 10, 1, 1, BLACKNESS);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TImpGridInplaceEdit.SetEditStyle(Value: TEditStyle);
begin
  if Value = FEditStyle then Exit;
  FEditStyle := Value;
  case Value of
    esPickList, esDataList:
      begin
        if FPickList = nil then
        begin
          FPickList := TPopupListbox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          FPickList.IntegralHeight := True;
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
{    esDataList:
      begin
        if FDataList = nil then
        begin
          FDataList := TPopupDataList.Create(Self);
          FDataList.Visible := False;
          FDataList.Parent := Self;
          FDataList.OnMouseUp := ListMouseUp;
        end;
        FActiveList := FDataList;
      end;}
  else  { cbsNone, cbsEllipsis, or read only field }
    FActiveList := nil;
  end;
  with TImpStringGrid(Grid) do
    Self.ReadOnly := Columns[col].ReadOnly;

  Repaint;
//  if (value = esDataList) then HideCaret (Handle);
end;

procedure TImpGridInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TImpGridInplaceEdit.TrackButton(X,Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  SetRect(R, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TImpGridInplaceEdit.UpdateContents;
var
  Column: TImpColumn;
  NewStyle: TEditStyle;
  {MasterField: TField; }
begin
  with TImpStringgrid(Grid) do
    Column := Columns[col];
  NewStyle := esSimple;
  case Column.ButtonStyle of
   cbsEllipsis: NewStyle := esEllipsis;
   cbsPicklist: NewStyle := esPickList;
   cbsDatalist: NewStyle := esDataList;
 {    if Assigned(Column.Field) then
     with Column.Field do
     begin
       { Show the dropdown button only if the field is editable }
{       if FieldKind = fkLookup then
       begin
         MasterField := Dataset.FieldByName(KeyFields);
         { Column.DefaultReadonly will always be True for a lookup field.
           Test if Column.ReadOnly has been assigned a value of True }
{         if Assigned(MasterField) and MasterField.CanModify and
           not ((cvReadOnly in Column.AssignedValues) and Column.ReadOnly) then
           with TCustomDBGrid(Grid) do
             if not ReadOnly and DataLink.Active and not Datalink.ReadOnly then
               NewStyle := esDataList
       end
       else
       if Assigned(Column.Picklist) and (Column.PickList.Count > 0) and
         not Column.Readonly then
         NewStyle := esPickList;
     end;}
//  end;
  end;
  EditStyle := NewStyle;
  inherited UpdateContents;
end;

procedure TImpGridInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TImpGridInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TImpGridInplaceEdit.WMKillFocus(var Message: TMessage);
begin
  if SysLocale.FarEast then
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
  end;
  inherited;
  CloseUp(False);
end;

procedure TImpGridInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TImpGridInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TImpGridInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), ScreenToClient(P)) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TImpGridInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle in [esPickList, esDataList] then
      with TWMKey(Message) do
      begin
        //DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (CharCode <> 0) and FListVisible then
        begin
          with TMessage(Message) do
            SendMessage(FActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;
  inherited;
end;

constructor Timpstringgrid.Create(Owner: TComponent);
var i:integer;
begin
  inherited;
  Columns := TImpGridColumns.Create(self,TImpColumn);
  For i := 1 to 5 do Columns.add;
end;

procedure Timpstringgrid.Editbuttonclick;
begin
  if assigned(OnElippsisclicked) then OnElippsisclicked(self,col,row)
end;

function Timpstringgrid.GetColCount: Integer;
begin
  result := inherited colcount
end;

procedure Timpstringgrid.SetColCount(Col: LongInt);
var oldcol:Longint;
begin
  oldcol := inherited Colcount;
  if oldcol <> col then begin
    if col > oldcol then begin
      while oldcol < col do begin
        columns.add;
        inc(oldcol);
      end;
    end
      else
    begin
      while oldcol > col do begin
        columns.items[columns.count-1].free;
        dec(oldcol);
      end;
    end;
  end;
  inherited colcount := col;
end;

function TImpStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TImpGridInplaceEdit.Create(Self);
end;



procedure TImpstringgrid.KeyDown(var Key: Word; Shift: TShiftState);
var coll,roww,i:integer;
begin
  coll := col;
  roww := row;
  if key = vk_tab then begin
      if not (ssAlt in Shift) then
        if ssShift in Shift then
        begin
          Dec(coll);
          if coll < FixedCols then
          begin
            coll := ColCount - 1;
            Dec(roww);
            if roww < FixedRows then roww := RowCount - 1;
          end;
          Shift := [];
        end
        else
        begin
          Inc(coll);
          if coll >= ColCount then
          begin
            coll := FixedCols;
            Inc(roww);
            if roww >= RowCount then
            begin
              if (rowcount+1)*(Defaultrowheight+Gridlinewidth) > height then
                begin
                  if not ftoomuch then if assigned(fOntoomuch) then fOntoomuch(self);
                  ftoomuch := true;
                end else ftoomuch := false;
              rowcount := rowcount +1;
              if assigned(fOnNewRow) then fOnNewRow(self);
              if colcount <> 0 then for i := 0 to colcount-1 do begin
                cells[i,roww] := '';
              end;
            end;
          end;
        end;
   end;
   inherited keydown(Key,Shift);
end;

procedure TImpStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);

  function RowIsMultiSelected: Boolean;
  var
    Index: Integer;
  begin
    Result :=false;
    {(dgMultiSelect in Options) and Datalink.Active and
      FBookmarks.Find(Datalink.Datasource.Dataset.Bookmark, Index);}
  end;

var
  OldActive: Integer;
  Indicator: Integer;
  Highlight: Boolean;
  Value: string;
  DrawColumn: TImpColumn;
  FrameOffs: Byte;
  MultiSelected: Boolean;
begin
  if csLoading in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Exit;
  end;

 { Dec(ARow, FTitleOffset);
  Dec(ACol, FIndicatorOffset); }

  if (gdFixed in AState) then
  begin
    InflateRect(ARect, -1, -1);
    FrameOffs := 1;
  end
  else
    FrameOffs := 2;

  with Canvas do
  begin
    DrawColumn := Columns[ACol];
    if gdFixed in AState then
    begin
      Font := DrawColumn.Title.Font;
      Brush.Color := DrawColumn.Title.Color;
    end
    else
    begin
      Font := DrawColumn.Font;
      Brush.Color := DrawColumn.Color;
    end;
    with DrawColumn do
      WriteText(Canvas, ARect, FrameOffs, FrameOffs, cells[Acol,Arow], Alignment)
  end;
  if (gdFixed in AState) then
  begin
    InflateRect(ARect, 1, 1);
    if ARow < 1 then with DrawColumn.Title do
      WriteText(Canvas, ARect, FrameOffs, FrameOffs, Caption, Alignment);
    DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
    DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
  end;
end;


{ TImpColumn }

constructor TImpColumn.Create(Collection: TCollection);
var
  Grid: TImpStringGrid;
begin
  Grid := nil;
  if Assigned(Collection) and (Collection is TImpGridColumns) then
    Grid := TImpGridColumns(Collection).Grid;
  {if Assigned(Grid) then
    Grid.BeginLayout; }
  try
    inherited Create(Collection);
    FDropDownRows := 7;
    FButtonStyle := cbsNone;
    FFont := TFont.Create;
    FFont.Assign(DefaultFont);
    FFont.OnChange := FontChanged;
    FImeMode := imDontCare;
    FImeName := Screen.DefaultIme;
    FTitle := CreateTitle;
    //FPicklist := Tstrings.create;
  finally
    if Assigned(Grid) then
     // Grid.EndLayout;
  end;
end;

destructor TImpColumn.Destroy;
begin
  FTitle.Free;
  FFont.Free;
  FPickList.Free;
  inherited Destroy;
end;

procedure TImpColumn.Assign(Source: TPersistent);
begin
  if Source is TImpColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      RestoreDefaults;
      //FieldName := TImpColumn(Source).FieldName;
      if cvColor in TImpColumn(Source).AssignedValues then
        Color := TImpColumn(Source).Color;
      if cvWidth in TImpColumn(Source).AssignedValues then
        Width := TImpColumn(Source).Width;
      if cvFont in TImpColumn(Source).AssignedValues then
        Font := TImpColumn(Source).Font;
      if cvImeMode in TImpColumn(Source).AssignedValues then
        ImeMode := TImpColumn(Source).ImeMode;
      if cvImeName in TImpColumn(Source).AssignedValues then
        ImeName := TImpColumn(Source).ImeName;
      if cvAlignment in TImpColumn(Source).AssignedValues then
        Alignment := TImpColumn(Source).Alignment;
      if cvReadOnly in TImpColumn(Source).AssignedValues then
        ReadOnly := TImpColumn(Source).ReadOnly;
      Title := TImpColumn(Source).Title;
      DropDownRows := TImpColumn(Source).DropDownRows;
      ButtonStyle := TImpColumn(Source).ButtonStyle;
      PickList := TImpColumn(Source).PickList;
      //PopupMenu := TImpColumn(Source).PopupMenu;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TImpColumn.CreateTitle: TImpColumnTitle;
begin
  Result := TImpColumnTitle.Create(Self);
end;

function TImpColumn.DefaultAlignment: TAlignment;
begin
 // if Assigned(Field) then
 //   Result := FField.Alignment
 // else
    Result := taLeftJustify;
end;

function TImpColumn.DefaultColor: TColor;
var
  Grid: TImpStringGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Color
  else
    Result := clWindow;
end;

function TImpColumn.DefaultFont: TFont;
var
  Grid: TImpStringGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TImpColumn.DefaultImeMode: TImeMode;
var
  Grid: TImpStringGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeMode
  else
    Result := FImeMode;
end;

function TImpColumn.DefaultImeName: TImeName;
var
  Grid: TImpStringGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeName
  else
    Result := FImeName;
end;

function TImpColumn.DefaultReadOnly: Boolean;
//var
//  Grid: TImpStringGrid;
begin
 // Grid := GetGrid;
  Result := false;
//  Result := (Assigned(Grid) and Grid.ReadOnly) or (Assigned(Field) and FField.ReadOnly);
end;

function TImpColumn.DefaultWidth: Integer;
//var
  //W: Integer;
 // RestoreCanvas: Boolean;
//  TM: TTextMetric;
begin
  if GetGrid = nil then
  begin
    Result := 64;
    Exit;
  end;
  with GetGrid do
  begin
 {   if Assigned(Field) then
    begin
      RestoreCanvas := not HandleAllocated;
      if RestoreCanvas then
        Canvas.Handle := GetDC(0);
      try
        Canvas.Font := Self.Font;
        GetTextMetrics(Canvas.Handle, TM);
        Result := Field.DisplayWidth * (Canvas.TextWidth('0') - TM.tmOverhang)
          + TM.tmOverhang + 4;
        if dgTitles in Options then
        begin
          Canvas.Font := Title.Font;
          W := Canvas.TextWidth(Title.Caption) + 4;
          if Result < W then
            Result := W;
        end;
      finally
        if RestoreCanvas then
        begin
          ReleaseDC(0,Canvas.Handle);
          Canvas.Handle := 0;
        end;
      end;
    end
    else}
      Result := DefaultColWidth;
  end;
end;

procedure TImpColumn.FontChanged;
begin
  Include(FAssignedValues, cvFont);
  Title.RefreshDefaultFont;
  Changed(False);
end;

function TImpColumn.GetAlignment: TAlignment;
begin
  if cvAlignment in FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TImpColumn.GetColor: TColor;
begin
  if cvColor in FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

{function TImpColumn.GetField: TField;
var
  Grid: TImpStringGrid;
begin    { Returns Nil if FieldName can't be found in dataset }
{  Grid := GetGrid;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(Grid) and
    Assigned(Grid.DataLink.DataSet) then
  with Grid.Datalink.Dataset do
    if Active or (not DefaultFields) then
      SetField(FindField(FieldName));
  Result := FField;
end;}

function TImpColumn.GetFont: TFont;
var
  Save: TNotifyEvent;
begin
  if not (cvFont in FAssignedValues) and (FFont.Handle <> DefaultFont.Handle) then
  begin
    Save := FFont.OnChange;
    FFont.OnChange := nil;
    FFont.Assign(DefaultFont);
    FFont.OnChange := Save;
  end;
  Result := FFont;
end;

function TImpColumn.GetGrid: TImpStringGrid;
begin
  if Assigned(Collection) and (Collection is TImpGridColumns) then
    Result := TImpGridColumns(Collection).Grid
  else
    Result := nil;
end;

function TImpColumn.GetDisplayName: string;
begin
{  Result := FFieldName;
  if Result = '' then }Result := inherited GetDisplayName;
end;

function TImpColumn.GetImeMode: TImeMode;
begin
  if cvImeMode in FAssignedValues then
    Result := FImeMode
  else
    Result := DefaultImeMode;
end;

function TImpColumn.GetImeName: TImeName;
begin
  if cvImeName in FAssignedValues then
    Result := FImeName
  else
    Result := DefaultImeName;
end;

function TImpColumn.GetPickList: TStrings;
begin
  if FPickList = nil then
    FPickList := TStringList.Create;
  Result := FPickList;
end;

function TImpColumn.GetReadOnly: Boolean;
begin
  if cvReadOnly in FAssignedValues then
    Result := FReadOnly
  else
    Result := DefaultReadOnly;
end;

function TImpColumn.GetWidth: Integer;
begin
  if cvWidth in FAssignedValues then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

function TImpColumn.IsAlignmentStored: Boolean;
begin
  Result := (cvAlignment in FAssignedValues) and (FAlignment <> DefaultAlignment);
end;

function TImpColumn.IsColorStored: Boolean;
begin
  Result := (cvColor in FAssignedValues) and (FColor <> DefaultColor);
end;

function TImpColumn.IsFontStored: Boolean;
begin
  Result := (cvFont in FAssignedValues);
end;

function TImpColumn.IsImeModeStored: Boolean;
begin
  Result := (cvImeMode in FAssignedValues) and (FImeMode <> DefaultImeMode);
end;

function TImpColumn.IsImeNameStored: Boolean;
begin
  Result := (cvImeName in FAssignedValues) and (FImeName <> DefaultImeName);
end;

function TImpColumn.IsReadOnlyStored: Boolean;
begin
  Result := (cvReadOnly in FAssignedValues) and (FReadOnly <> DefaultReadOnly);
end;

function TImpColumn.IsWidthStored: Boolean;
begin
  Result := (cvWidth in FAssignedValues) and (FWidth <> DefaultWidth);
end;

procedure TImpColumn.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if cvFont in FAssignedValues then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TImpColumn.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvFont in FAssignedValues;
  FTitle.RestoreDefaults;
  FAssignedValues := [];
  RefreshDefaultFont;
  FPickList.Free;
  FPickList := nil;
  ButtonStyle := cbsNone;
  Changed(FontAssigned);
end;

procedure TImpColumn.SetAlignment(Value: TAlignment);
begin
  if (cvAlignment in FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FAssignedValues, cvAlignment);
  Changed(False);
end;

procedure TImpColumn.SetButtonStyle(Value: TImpColumnButtonStyle);
begin
  if Value = FButtonStyle then Exit;
  FButtonStyle := Value;
  Changed(False);
end;

procedure TImpColumn.SetColor(Value: TColor);
begin
  if (cvColor in FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FAssignedValues, cvColor);
  Changed(False);
end;

{procedure TImpColumn.SetField(Value: TField);
begin
  if FField = Value then Exit;
  FField := Value;
  if Assigned(Value) then
    FFieldName := Value.FieldName;
  Changed(False);
end; }

{procedure TImpColumn.SetFieldName(const Value: String);
var
  AField: TField;
  Grid: TImpStringGrid;
begin
  AField := nil;
  Grid := GetGrid;
  if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) and
    not (csLoading in Grid.ComponentState) and (Length(Value) > 0) then
      AField := Grid.DataLink.DataSet.FindField(Value); { no exceptions }
 { FFieldName := Value;
  SetField(AField);
  Changed(False);
end;}

procedure TImpColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, cvFont);
  Changed(False);
end;

procedure TImpColumn.SetImeMode(Value: TImeMode);
begin
  if (cvImeMode in FAssignedValues) or (Value <> DefaultImeMode) then
  begin
    FImeMode := Value;
    Include(FAssignedValues, cvImeMode);
  end;
  Changed(False);
end;

procedure TImpColumn.SetImeName(Value: TImeName);
begin
  if (cvImeName in FAssignedValues) or (Value <> DefaultImeName) then
  begin
    FImeName := Value;
    Include(FAssignedValues, cvImeName);
  end;
  Changed(False);
end;

procedure TImpColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
  begin
    FPickList.Free;
    FPickList := nil;
    Exit;
  end;
  PickList.Assign(Value);
end;

{procedure TImpColumn.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then Value.FreeNotification(GetGrid);
end;}

procedure TImpColumn.SetReadOnly(Value: Boolean);
begin
  if (cvReadOnly in FAssignedValues) and (Value = FReadOnly) then Exit;
  FReadOnly := Value;
  Include(FAssignedValues, cvReadOnly);
  Changed(False);
end;

procedure TImpColumn.SetTitle(Value: TImpColumnTitle);
begin
  FTitle.Assign(Value);
end;

procedure TImpColumn.SetWidth(Value: Integer);
begin
  if (cvWidth in FAssignedValues) or (Value <> DefaultWidth) then
  begin
    FWidth := Value;
    Include(FAssignedValues, cvWidth);
  end;
  Changed(False);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TImpStringgrid]);
end;

{ TImpGridColumns }

constructor TImpGridColumns.Create(Grid: TImpStringGrid; ColumnClass: TImpColumnClass);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TImpGridColumns.Add: TImpColumn;
begin
  Result := TImpColumn(inherited Add);
end;

function TImpGridColumns.GeTImpColumn(Index: Integer): TImpColumn;
begin
  Result := TImpColumn(inherited Items[Index]);
end;

function TImpGridColumns.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TImpGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TImpGridColumns.GetState: TImpGridColumnsState;
begin
  Result := TImpGridColumnsState((Count > 0)) ;//and not (Items[0] is TPassthroughColumn));
end;

procedure TImpGridColumns.LoadFromFile(const Filename: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

type
  TImpColumnsWrapper = class(TComponent)
  private
    FColumns: TImpGridColumns;
  published
    property Columns: TImpGridColumns read FColumns write FColumns;
  end;

procedure TImpGridColumns.LoadFromStream(S: TStream);
var
  Wrapper: TImpColumnsWrapper;
begin
 { Wrapper := TImpColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := FGrid.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;}
end;

procedure TImpGridColumns.RestoreDefaults;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count-1 do
      Items[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TImpGridColumns.RebuildColumns;
var
  I: Integer;
begin
  {if Assigned(FGrid) and Assigned(FGrid.DataSource) and
    Assigned(FGrid.Datasource.Dataset) then
  begin
    FGrid.BeginLayout;
    try
      Clear;
      with FGrid.Datasource.Dataset do
        for I := 0 to FieldCount-1 do
          Add.FieldName := Fields[I].FieldName
    finally
      FGrid.EndLayout;
    end
  end
  else
    Clear;}
end;

procedure TImpGridColumns.SaveToFile(const Filename: string);
var
  S: TStream;
begin
  S := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TImpGridColumns.SaveToStream(S: TStream);
var
  Wrapper: TImpColumnsWrapper;
begin
  Wrapper := TImpColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;

procedure TImpGridColumns.SeTImpColumn(Index: Integer; Value: TImpColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TImpGridColumns.SetState(NewState: TImpGridColumnsState);
begin
  if NewState = State then Exit;
  if NewState = csDefault then
    Clear
  else
    RebuildColumns;
end;

procedure TImpGridColumns.Update(Item: TCollectionItem);
var
  Raw: Integer;
begin
 { if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;
 { if Item = nil then
  begin
    FGrid.LayoutChanged;
  end
  else }
{  begin
    Raw := FGrid.DataToRawColumn(Item.Index);
    FGrid.InvalidateCol(Raw);
    FGrid.ColWidths[Raw] := TImpColumn(Item).Width;
  end; }
end;


{ TImpColumnTitle }
constructor TImpColumnTitle.Create(Column: TImpColumn);
begin
  inherited Create;
  FColumn := Column;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
end;

destructor TImpColumnTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TImpColumnTitle.Assign(Source: TPersistent);
begin
  if Source is TImpColumnTitle then
  begin
    if cvTitleAlignment in TImpColumnTitle(Source).FColumn.FAssignedValues then
      Alignment := TImpColumnTitle(Source).Alignment;
    if cvTitleColor in TImpColumnTitle(Source).FColumn.FAssignedValues then
      Color := TImpColumnTitle(Source).Color;
    if cvTitleCaption in TImpColumnTitle(Source).FColumn.FAssignedValues then
      Caption := TImpColumnTitle(Source).Caption;
    if cvTitleFont in TImpColumnTitle(Source).FColumn.FAssignedValues then
      Font := TImpColumnTitle(Source).Font;
  end
  else
    inherited Assign(Source);
end;

function TImpColumnTitle.DefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TImpColumnTitle.DefaultColor: TColor;
var
  Grid: TImpStringGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TImpColumnTitle.DefaultFont: TFont;
var
  Grid: TImpStringGrid;
begin
  Grid := FColumn.GetGrid;
  {if Assigned(Grid) then
    Result := Grid.TitleFont
  else }
    Result := FColumn.Font;
end;

function TImpColumnTitle.DefaultCaption: string;
//var
 { Field: TField;}
begin
 { Field := FColumn.Field;
  if Assigned(Field) then
    Result := Field.DisplayName
  else }
  {  Result := FColumn.FieldName;}
end;

procedure TImpColumnTitle.FontChanged(Sender: TObject);
begin
  Include(FColumn.FAssignedValues, cvTitleFont);
  FColumn.Changed(True);
end;

function TImpColumnTitle.GetAlignment: TAlignment;
begin
  if cvTitleAlignment in FColumn.FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TImpColumnTitle.GetColor: TColor;
begin
  if cvTitleColor in FColumn.FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TImpColumnTitle.GetCaption: string;
begin
  if cvTitleCaption in FColumn.FAssignedValues then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TImpColumnTitle.GetFont: TFont;
var
  Save: TNotifyEvent;
  Def: TFont;
begin
  if not (cvTitleFont in FColumn.FAssignedValues) then
  begin
    Def := DefaultFont;
    if (FFont.Handle <> Def.Handle) or (FFont.Color <> Def.Color) then
    begin
      Save := FFont.OnChange;
      FFont.OnChange := nil;
      FFont.Assign(DefaultFont);
      FFont.OnChange := Save;
    end;
  end;
  Result := FFont;
end;

function TImpColumnTitle.IsAlignmentStored: Boolean;
begin
  Result := (cvTitleAlignment in FColumn.FAssignedValues) and
    (FAlignment <> DefaultAlignment);
end;

function TImpColumnTitle.IsColorStored: Boolean;
begin
  Result := (cvTitleColor in FColumn.FAssignedValues) and
    (FColor <> DefaultColor);
end;

function TImpColumnTitle.IsFontStored: Boolean;
begin
  Result := (cvTitleFont in FColumn.FAssignedValues);
end;

function TImpColumnTitle.IsCaptionStored: Boolean;
begin
  Result := (cvTitleCaption in FColumn.FAssignedValues) and
    (FCaption <> DefaultCaption);
end;

procedure TImpColumnTitle.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if (cvTitleFont in FColumn.FAssignedValues) then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TImpColumnTitle.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvTitleFont in FColumn.FAssignedValues;
  FColumn.FAssignedValues := FColumn.FAssignedValues - ColumnTitleValues;
  FCaption := '';
  RefreshDefaultFont;
  { If font was assigned, changing it back to default may affect grid title
    height, and title height changes require layout and redraw of the grid. }
  FColumn.Changed(FontAssigned);
end;

procedure TImpColumnTitle.SetAlignment(Value: TAlignment);
begin
  if (cvTitleAlignment in FColumn.FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FColumn.FAssignedValues, cvTitleAlignment);
  FColumn.Changed(False);
end;

procedure TImpColumnTitle.SetColor(Value: TColor);
begin
  if (cvTitleColor in FColumn.FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FColumn.FAssignedValues, cvTitleColor);
  FColumn.Changed(False);
end;

procedure TImpColumnTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TImpColumnTitle.SetCaption(const Value: string);
begin
  if (cvTitleCaption in FColumn.FAssignedValues) and (Value = FCaption) then Exit;
  FCaption := Value;
  Include(FColumn.FAssignedValues, cvTitleCaption);
  FColumn.Changed(False);
end;


(* HAR : Extensions *)
function TImpStringgrid.CanEditShow: Boolean;
begin
  Result := (inherited CanEditShow) and (not Columns[Col].ReadOnly);
end;

{function TImpGridInplaceEdit.EditCanModify: Boolean;
begin
  with TImpStringGrid(Grid) do
    Result := (inherited EditCanModify) and (Columns[Col].ButtonStyle <> cbsPicklist);
end;
}


function TImpGridInplaceEdit.GetClosestItem(s : string ) : integer;
var i,j : integer;
    FoundLength, FoundItem : integer;

  function PartComp( s1, s2 : string) : integer;
  var i, j : integer;
  begin
    j := Length(s1);
    i := Length(s2);
    if (j > i) then j := i;

    Result := 0;
    for i := 0 to j-1 do
      if (s1[i] = s2[i]) then inc(Result) else exit;
  end;

begin

//  Len := Length(s);
  Result := -1;
  if (Assigned(FPickList) and (FPickList.Items.Count > 0)) then
  begin
    FoundLength := 0;
    for i := 0 to FPickList.Items.Count-1 do
    begin
      j := PartComp(s, FPickList.Items[i]);
      if ( j > FoundLength) then
      begin
        FoundLength := j;
        Result := i;
      end;
    end;
  end;

end;




{procedure TImpGridInplaceEdit.WMSetText( var Message: TWMSetText);
var i : integer;
begin
  i := GetClosestItem(Message.Text);
  if (i < 0) then Message.Text := '' else Message.Text := PChar(FPickList.Items[i]);
  inherited;

end;

  Perform(WM_SETTEXT, 0, Longint(Buffer));
procedure TImpGridInplaceEdit.Change;
var i : integer;
begin
  i := GetClosestItem(Text);
  if (i < 0) then Text := '' else Text := FPickList.Items[i];

  inherited Change;
end;
}
end.
