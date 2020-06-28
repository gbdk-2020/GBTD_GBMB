{ ---------------------------------------------------------------------------}
{ A Most Recently Used (MRU) File List component for Delphi.                 }
{ Copyright 1996, Brad Stowers.  All Rights Reserved.                        }
{ This component can be freely used and distributed in commercial and        }
{ private environments, provied this notice is not modified in any way.      }
{ ---------------------------------------------------------------------------}
{ Feel free to contact me if you have any questions, comments or suggestions }
{ at bstowers@pobox.com or 72733,3374 on CompuServe.                         }
{ The latest version of this component can always be found at:               }
{   http://www.pobox.com/~bstowers/delphi/                                   }
{ ---------------------------------------------------------------------------}
{ Date last modified:  Apr. 30, 1997                                         }
{ ---------------------------------------------------------------------------}

{ ---------------------------------------------------------------------------}
{ TMRUFileList v2.10                                                         }
{ ---------------------------------------------------------------------------}
{ Description:                                                               }
{   A component that greatly simplifies adding MRU file lists to menus.      }
{ Features:                                                                  }
{   Defineable maximum number of items to be maintained in the list.         }
{   Automatic removal of item when selected, or not.                         }
{   Items can exist on selected menu, or on submenu of selected menu.        }
{ ---------------------------------------------------------------------------}
{ Revision History:                                                          }
{ 1.00:  + Initial release                                                   }
{ 2.00:  + How about that?  A 1.00 release without any bug fixes!?!          }
{        + Added the ability to have MRU items appear at a specific place    }
{          on a menu.  If you select a submenu for the FileMenu property     }
{          (like the File menu), the items are appended to the end of that   }
{          menu.  If you select a menu item (like the Exit item), MRU items  }
{          will be inserted before that item.  I suggest you select a        }
{          separator because the list will create a separator of it's own,   }
{          and this will offset the entire MRU list.                         }
{        + Added automatic saving and restoring of items.  If compiled under }
{          Delphi 1.0, it saves to an INI file.  If D2, the registry.        }
{          This involves three new properties:                               }
{            AutoSave:     Should items be saved and restored.               }
{            AutoSaveName: The filename (INI) or key (registry) to save to.  }
{            AutoSaveKey:  The section to save to.                           }
{          For the registry, it would look like user\AutoSaveName\Key.       }
{        + Resource for 16 and 32-bit version.  This takes the place of a    }
{          DCR file, which can't be compatible with both.  See the $R        }
{          directive below.  To compile the resource file, use the following }
{            Delphi 1: BRCC.EXE -foMRUList.r16 -31 MRUList.rc                }
{            Delphi 2: BRCC32.EXE -foMRUList.r32 -w32 MRUList.rc             }
{ 2.01   + Added RemoveItem to remove a single item. Thanks to Peter Zehnder }
{          (email: zehnderp@dial.eunet.ch) for this.                         }
{        + RemoveItems renamed to RemoveAllItems because it will cause less  }
{          confusion with new RemoveItem method.                             }
{ 2.02   + Fixed problem with submenu not appearing in the right position.   }
{ 2.03   + Changing the MRU list during the OnMRUItemClick event could lead  }
{          to corrupted values in the MRU list.  Fixed.                      }
{ 2.04   + Added "InsertSeparator"-Property (modification by Stefan Schlott) }
{ 2.05   + Fixed nasty bug.  If you deleted the menu item that FileMenu      }
{          contained, it would cause all sorts of trouble.  See Notification }
{          method override.                                                  }
{ 2.06   + Changed AddItem to work like other TList.Add methods, i.e. adds   }
{          the item to the end of the list.  If you are like me and prefer   }
{          that new items be added to the top of the MRU list, use the new   }
{          InsertItem method with an index of 0.                             }
{        + Added ReplaceItem method to replace an existing MRU item with a   }
{          new one.  Thanks to Artem Berman (art@aber.kherson.ua) for this.  }
{ 2.07   + RemoveAllItems had a memory leak.  Plugged it.  Thanks to Robert  }
{          Galle (robert.galle@guest.arnes.si) for catching this one.        }
{ 2.10   + New property MaxCaptionWidth.  Set to 0, this performs as it      }
{          always has, however, setting it to reasonable width causes the    }
{          item to be shortened using '...' so that it does not take up more }
{          space than specified.  Experiment with it for a few minutes and   }
{          you will see how it works.                                        }
{ ---------------------------------------------------------------------------}

unit MRUList;

interface

{$IFDEF WIN32}
  {$R MRUList.r32}
{$ELSE}
  {$R MRUList.r16}
{$ENDIF}

uses
  Classes, SysUtils, Menus;

const
  { Defaults for component properties }
  DEF_MAXIMUM         = 5;
  DEF_REMOVEONCLICK   = TRUE;
  DEF_USESUBMENU      = FALSE;
  DEF_SUBMENUNAME     = 'Reopen';
  DEF_MAXCAPTIONWIDTH = 200;

type
  { A simple TMenuItem descendant to be used for RTTI }
  TMRUMenuItem = class(TMenuItem)
  private
    FFullCaption: string;
  public
    ItemNumber: byte;
    constructor Create(AOwner: TComponent); override;
    property FullCaption: string read FFullCaption write FFullCaption;
  end;

  { Event procedure for MRU item click.  Passes filename for easy us }
  TMRUClick = procedure(Sender: TObject; AFilename: TFilename) of Object;

  TMRUFileList = class(TComponent)
  private
    { Property variables }
    FMaximum: byte;
    FRemoveOnClick: boolean;
    FUseSubmenu: boolean;
    FInsertSeparator : Boolean;
    FSubmenuName: string;
    FFileMenu: TMenuItem;
    FMenuItems: TStringList;
    FAutoSave: boolean;
    FAutoSaveName: string;
    FAutoSaveKey: string;
    FMaxCaptionWidth: integer;
    { Event variables }
    FOnMRUClick: TMRUClick;
    { Internal use }
    FInhibitUpdate: boolean;

    FOnCreate: TNotifyEvent;
  protected
    procedure Loaded; override;
    { Property methods }
    procedure SetMaximum(Val: byte);
    procedure SetFileMenu(Val: TMenuItem);
    procedure SetUseSubmenu(Val: boolean);
    procedure SetInsertSeparator(Val: boolean);
    procedure SetSubmenuName(Val: string);
    procedure SetMaxCaptionWidth(Val: integer);
    { MenuItem OnClick handler }
    procedure MRUClicked(Sender: TObject);
    { Method to place items on menu }
    procedure PopulateMenu; virtual;
    { We need to know if our menu item is deleted. }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Methods to add items to the MRU list }
    procedure InsertItem(Index: integer; aFile: TFilename);
    procedure ReplaceItem(OldItem, NewItem: TFilename);
    procedure AddItem(aFile: TFilename);
    procedure AddStringList(Files: TStringList);
    procedure AddStrings(Files: TStrings);
    { Methods to load and save items. }
    function Load: boolean;
    function Save: boolean;
    { Method to remove all MRU items.  Not normally needed by an app. }
    procedure RemoveAllItems;
    procedure RemoveItem (aFile: TFilename);
    { The MRU Items.  Read Only. }
    property Items: TStringList read FMenuItems;
  published
    property Maximum: byte             { Maximum number of items on MRU list }
        read FMaximum
        write SetMaximum
        default DEF_MAXIMUM;
    property RemoveOnClick: boolean    { Remove MRU item when selected? }
        read FRemoveOnClick
        write FRemoveOnClick
        default DEF_REMOVEONCLICK;
    property UseSubmenu: boolean       { MRU items placed on a submenu? }
        read FUseSubmenu
        write SetUseSubmenu
        default DEF_USESUBMENU;
    property InsertSeparator : boolean
        read FInsertSeparator
        write SetInsertSeparator
        default True;
    property SubmenuName: string       { Caption of submenu item, if needed }
        read FSubmenuName
        write SetSubmenuName;
    property OnMRUItemClick: TMRUClick { Event for MRU item selection }
        read FOnMRUClick
        write FOnMRUClick;
    property FileMenu: TMenuItem       { Menu to place MRU items on. }
        read FFileMenu
        write SetFileMenu;
    property AutoSave: boolean         { Save and restore MRU items automatically. }
        read FAutoSave
        write FAutoSave
        default TRUE;
    property AutoSaveName: string      { The filename (INI) or key (registry) to save to.}
        read FAutoSaveName
        write FAutoSaveName;
    property AutoSaveKey: string       { The section to save to. }
        read FAutoSaveKey
        write FAutoSavekey;
    property MaxCaptionWidth: integer  { Maximum width of an MRU item, 0 = no maximum.}
        read FMaxCaptionWidth
        write SetMaxCaptionWidth
        default DEF_MAXCAPTIONWIDTH;

    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
  end;

procedure Register;

implementation

uses WinTypes, WinProcs, Graphics, FileCtrl,
     {$IFDEF HARWIN32} Registry {$ELSE} INIFiles {$ENDIF} ;

{ Make it transparent, no matter which version we are using. }
{$IFNDEF HARWIN32}
//  type
//   TRegIniFile = class(TINIFile);
{$ENDIF}

var
  MenuBmp: TBitmap;


{ Simple TMenuItem descendant mainly for RTTI, but also knows it's index     }
{ into the FMenuItems list.                                                  }
constructor TMRUMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ItemNumber := 0;
  FFullCaption := inherited Caption;
end;



{ Needs to do nothing more than initialize properties to defaults and create }
{ the list variable.                                                         }
constructor TMRUFileList.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FMaximum := DEF_MAXIMUM;
  FRemoveOnClick := DEF_REMOVEONCLICK;
  FUseSubmenu := DEF_USESUBMENU;
  FInsertSeparator:=True;
  SubmenuName := DEF_SUBMENUNAME;
  FMaxCaptionWidth := DEF_MAXCAPTIONWIDTH;
  FMenuItems := TStringList.Create;
  FMenuItems.Sorted := FALSE;
  FInhibitUpdate := FALSE;
  FOnMRUClick := NIL;
  FAutoSave := TRUE;
  {$IFDEF HARWIN32}
  FAutoSaveName := '\Software\My Application';
  {$ELSE}
  FAutoSaveName := 'MyINI.INI';
  {$ENDIF}
  FAutoSaveKey := 'MRU Items';

end;

destructor TMRUFileList.Destroy;
begin
  if not (csDesigning in ComponentState) then
    if FAutoSave then
      Save;
  { Cleanup the list variable }
  FMenuItems.Free;
  inherited Destroy;
end;

procedure TMRUFileList.Loaded;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOnCreate) then FOnCreate(Self);

    if FAutoSave then
      Load;
  end;
end;

procedure TMRUFileList.SetMaximum(Val: byte);
var
  x: integer;
begin
  if FMaximum = Val then exit;    { Value not different, do nothing. }
  if Val < FMaximum then begin    { If new less than old value, remove some. }
    for x := Val to FMenuItems.Count do
      FMenuItems.Delete(x-1);
    PopulateMenu;                 { Redo the MRU menu. }
  end;
  { Note: an ELSE clause is not needed since if new value is more than old,  }
  {       nothing needs to be done.                                          }
  FMaximum := Val;
end;

procedure TMRUFileList.SetFileMenu(Val: TMenuItem);
begin
  RemoveAllItems;           { Remove MRU items from old menu. }
  FFileMenu := Val;
  PopulateMenu;             { Add MRU items to new menu.      }
end;

procedure TMRUFileList.SetUseSubmenu(Val: boolean);
begin
  if FUseSubmenu = Val then exit; { Value not different, do nothing . }
  FUseSubmenu := Val;
  PopulateMenu;                   { Redo the menu according to new value. }
end;

procedure TMRUFileList.SetInsertSeparator(Val: boolean);
begin
 If Val=FInsertSeparator then exit;
 FInsertSeparator:=Val;
 PopulateMenu;
end;

procedure TMRUFileList.SetSubmenuName(Val: string);
begin
  if FSubmenuName = Val then exit; { Value not different, do nothing . }
  FSubmenuName := Val;
  if FUseSubmenu then         { Don't bother if we're not using the submenu. }
    PopulateMenu;             { Redo the menu according to new value. }
end;

procedure TMRUFileList.SetMaxCaptionWidth(Val: integer);
begin
  if Val = FMaxCaptionWidth then exit; { Value not different, do nothing. }
  FMaxCaptionWidth := Val;
  PopulateMenu;
end;

procedure TMRUFileList.MRUClicked(Sender: TObject);
var
  ClickItem: TFilename;
begin
  with Sender as TMRUMenuItem do begin
    if assigned(FOnMRUClick) then       { Save the clicked item's filename }
      ClickItem := FMenuItems[ItemNumber-1];
    if FRemoveOnClick then begin        { Remove the item, if desired. }
      FMenuItems.Delete(ItemNumber-1);
      PopulateMenu;
    end;
    if assigned(FOnMRUClick) then       { Call the users event handler. }
      FOnMRUClick(Sender, ClickItem);
  end;
end;

procedure TMRUFileList.InsertItem(Index: integer; aFile: TFilename);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(aFile);        { Search list for item being added. }
  if i > -1 then                         { Find it? }
    FMenuItems.Move(i, 0)                { Yes, move it to the top. }
  else begin
    FMenuItems.Insert(Index, aFile);     { No, add it. }
    while FMenuItems.Count > FMaximum do { Remove extra items. }
      FMenuItems.Delete(FMenuItems.Count-1);
  end;
  if not FInhibitUpdate then             { Should we update the menu now? }
    PopulateMenu;                        { Yes, redo the menu. }
end;

procedure TMRUFileList.ReplaceItem(OldItem, NewItem: TFileName);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(OldItem);      { Search list for item being added. }
  if i = -1 then                         { Find it? }
    exit                                 { No, get out. }
  else begin
    FMenuItems.Delete(i);                { Yes, remove it }
    FMenuItems.Insert(i, NewItem);       { and replace with the new one. }
  end;
  if not FInhibitUpdate then             { Should we update the menu now? }
    PopulateMenu;                        { Yes, redo the menu. }
end;

procedure TMRUFileList.AddItem(aFile: TFilename);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(aFile);        { Search list for item being added. }
  if i > -1 then                         { Find it? }
    FMenuItems.Move(i, 0)                { Yes, move it to the top. }
  else begin
    FMenuItems.Add(aFile);         { No, add it to the top. }
    while FMenuItems.Count > FMaximum do { Remove extra items. }
      FMenuItems.Delete(FMenuItems.Count-1);
  end;
  if not FInhibitUpdate then             { Should we update the menu now? }
    PopulateMenu;                        { Yes, redo the menu. }
end;

procedure TMRUFileList.AddStringList(Files: TStringList);
var
  x: integer;
begin
  FInhibitUpdate := TRUE;      { Don't let AddItem method call PopulateMenu. }
  for x := 0 to Files.Count - 1 do  { Add each item. }
    AddItem(Files[x]);
  FInhibitUpdate := FALSE;     { Clear inhibit flag. }
  PopulateMenu;                { Update menu now that all are added. }
end;

procedure TMRUFileList.AddStrings(Files: TStrings);
var
  x: integer;
begin
  FInhibitUpdate := TRUE;      { Don't let AddItem method call PopulateMenu. }
  for x := 0 to Files.Count - 1 do  { Add each item. }
    AddItem(Files[x]);
  FInhibitUpdate := FALSE;     { Clear inhibit flag. }
  PopulateMenu;                { Update menu now that all are added. }
end;

procedure TMRUFileList.PopulateMenu;
var
  Offset,
  x: integer;
  NewItem: TMRUMenuItem;
  ParentMenu,
  AddMenu: TMenuItem;
begin
  if (FFileMenu = NIL) then exit;        { No menu assigned, nothing to do. }
  RemoveAllItems;                        { Remove all old items. }
  if (FMenuItems.Count = 0) then exit;   { Don't have any items, we're done. }

  { If FFileMenu is an item, insert before it.  If not, it's a submenu }
  { so just add to the end of it                                       }
  if FFileMenu.Count <> 0 then begin
    Offset := FFileMenu.Count;
    ParentMenu := FFileMenu;
  end else begin
{$IFDEF WIN32}
    Offset := FFileMenu.MenuIndex;
{$ELSE}
    Offset := FFileMenu.Parent.IndexOf(FFileMenu);
{$ENDIF}
    ParentMenu := FFileMenu.Parent;
  end;

  { Create separator item. }
  If FInsertSeparator then
  Begin
   NewItem := TMRUMenuItem.Create(ParentMenu);
   NewItem.Caption := '-';
   ParentMenu.Insert(Offset, NewItem);
   inc(Offset);
  End;

  { Create submenu if needed }
  if FUseSubmenu then begin
    AddMenu := TMRUMenuItem.Create(ParentMenu);
    AddMenu.Caption := FSubmenuName;
    ParentMenu.Insert(Offset, AddMenu);
    Offset := 0;
  end else
    AddMenu := ParentMenu; { Don't need submenu, just set to the file menu. }

  { Create MRU items }
  for x := 0 to FMenuItems.Count - 1 do begin
    NewItem := TMRUMenuItem.Create(AddMenu);
    NewItem.FullCaption := '&' + IntToStr(x+1) + ' ' + FMenuItems[x];  { Caption }
    if FMaxCaptionWidth = 0 then
      NewItem.Caption := NewItem.FullCaption
    else
      NewItem.Caption := MinimizeName(FMenuItems[x], MenuBmp.Canvas, FMaxCaptionWidth);
    NewItem.ItemNumber := x + 1;                { Index into FMenuItems list }
    NewItem.OnClick := MRUClicked;              { Set event handler }
    AddMenu.Insert(Offset, NewItem);            { Add to the menu }
    inc(Offset);
  end;
end;

procedure TMRUFileList.RemoveAllItems;
var
  x: integer;
  DeleteItem,
  ParentMenu: TMenuItem;
begin
  if FFileMenu = NIL then exit;                { No menu, nothing to delete. }

  if FFileMenu.Count <> 0 then
    ParentMenu := FFileMenu
  else
    ParentMenu := FFileMenu.Parent;

  { We don't know exactly which items are ours, so we have to check them all }
  for x := ParentMenu.Count-1 downto 0 do begin
    { Use RTTI to determine if item is of our special descenadant type. }
    if (ParentMenu[x] is TMRUMenuItem) then begin
      DeleteItem := ParentMenu[x];
      ParentMenu.Delete(x);   { Yes, it is, delete it. }
      DeleteItem.Free;        { Don't forget the object, too! - RGL }
    end;
  end;
end;

procedure TMRUFileList.RemoveItem (aFile: TFilename);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(aFile);        { Search list for item being removed. }
  if i > -1 then                         { Find it? }
    begin
    FMenuItems.Delete(i);                { Yes, delete it. }
    PopulateMenu;                        { redo the menu. }
    end;
end;

function TMRUFileList.Load: boolean;
  procedure StripIdents(Items: TStringList);
  var
    p: byte;
    x: integer;
  begin
    for x := 0 to Items.Count-1 do begin
      p := Pos('=',Items[x])+1;
      Items[x] := copy(Items[x], p, Length(Items[x])-p+1);
    end;
  end;
var
  Settings: TIniFile;
begin
  Result := FALSE;
  if (FAutoSaveName = '') or (FAutoSaveKey = '') then exit;
  Settings := TIniFile.Create(FAutoSaveName);
  try
    Settings.ReadSectionValues(FAutoSaveKey, FMenuItems);
    StripIdents(FMenuItems);
    PopulateMenu;
    Result := TRUE;
  finally
    Settings.Free;
  end;
end;

function TMRUFileList.Save: boolean;
var
  Settings: TIniFile;
  x: integer;
begin
  Result := FALSE;
  if (FAutoSaveName = '') or (FAutoSaveKey = '') then exit;
  Settings := TIniFile.Create(FAutoSaveName);
  try
    Settings.EraseSection(FAutoSaveKey);
    for x := 0 to Items.Count-1 do
      Settings.WriteString(FAutoSaveKey, 'F'+IntToStr(x), Items[x]);
    Result := TRUE;
  finally
    Settings.Free;
  end;
end;

procedure TMRUFileList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFileMenu) and
     (csDesigning in ComponentState) then
    { Our placement menu item has been deleted. }
    FFileMenu := NIL;
end;


procedure Register;
begin
  RegisterComponents('My Controls', [TMRUFileList]);
end;


{$IFNDEF WIN32}
procedure FreeMemoryBmp; far;
begin
  MenuBmp.Free;
end;
{$ENDIF}

var
{$IFDEF WIN32}
  NCM: TNonClientMetrics;
{$ELSE}
  LF: TLogFont;
{$ENDIF}

initialization
  MenuBmp:= TBitmap.Create;
  {$IFDEF WIN32}
  NCM.cbSize := SizeOf(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCM, 0);
  MenuBmp.Canvas.Font.Handle := CreateFontIndirect(NCM.lfMenuFont);
  {$ELSE}
  GetObject(GetStockObject(SYSTEM_FONT), SizeOf(TLogFont), @LF);
  MenuBmp.Canvas.Font.Handle := CreateFontIndirect(LF);
  {$ENDIF}

{$IFDEF WIN32}
finalization
  MenuBmp.Free;
{$ELSE}
  AddExitProc(FreeMemoryBmp);
{$ENDIF}

end.

