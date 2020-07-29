object FrmMap: TFrmMap
  Left = 759
  Top = 336
  Width = 605
  Height = 498
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Gameboy Map Builder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpFixed
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = True
  Scaled = False
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LstTiles: TGBTileList
    Left = 496
    Top = 34
    Width = 74
    Height = 331
    TileSize = gbts16x16
    Freezed = False
    TileCount = 10
    SelTile = 0
    OnAfterSelect = LstTilesAfterSelect
    TabOrder = 0
  end
  object PnlToolbar: TPanel
    Left = 0
    Top = 0
    Width = 597
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object LneBtmShdw: TShape
      Left = 0
      Top = 30
      Width = 4000
      Height = 1
      Pen.Color = clBtnShadow
    end
    object LneBtmHgh: TShape
      Left = 0
      Top = 31
      Width = 4000
      Height = 1
      Pen.Color = clBtnHighlight
    end
    object LneTop: TShape
      Left = -1
      Top = 1
      Width = 4000
      Height = 1
      Pen.Color = clHighlightText
    end
    object LneTopShdw: TShape
      Left = -1
      Top = 0
      Width = 4000
      Height = 1
      Pen.Color = clBtnShadow
    end
    object BtnOpen: TSpeedButton
      Left = 7
      Top = 4
      Width = 24
      Height = 24
      Hint = 'Open'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
        EEEEEEEEEEEEEEEEEEEEEE0000000000EEEEE0B3B3B3B3B30EEEE03B3B3B3B3B
        30EEE003B3B3B3B3B30EE0B03B3B3B3B3B30E0BB03B3B3B3B3B0E0BBB000003B
        3B30E0BBBBBBBB00000EE0BBBBBBBBBB0EEEE0BBBBBBBBBB0EEEE0BBBB000000
        EEEEEE0000EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
      OnClick = Loadmap1Click
    end
    object BtnSave: TSpeedButton
      Left = 30
      Top = 4
      Width = 24
      Height = 24
      Hint = 'Save'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
        EEEEEE0000000000000EE0BB08088880BB0EE0BB08088880BB0EE0BB08888880
        BB0EE0BBB000000BBB0EE0BBBBBBBBBBBB0EE0B0000000000B0EE0B0FFFFFFFF
        0B0EE0B0FFFFFFFF0B0EE0B0FFFFFFFF0B0EE0B0FFFFFFFF0B0EE0B0FFFFFFFF
        0B0EE0B0FFFFFFFF0B0EE00000000000000EEEEEEEEEEEEEEEEE}
      OnClick = Savefile1Click
    end
    object Shape1: TShape
      Left = 81
      Top = 2
      Width = 1
      Height = 28
      Pen.Color = clBtnShadow
    end
    object Shape2: TShape
      Left = 82
      Top = 2
      Width = 1
      Height = 28
      Pen.Color = clBtnHighlight
    end
    object BtnExport: TSpeedButton
      Left = 53
      Top = 4
      Width = 24
      Height = 24
      Hint = 'Export'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
        EEEEE0000000000000EE0B0B0BBBBB0B0B0E0B0B0BBBBB0B0B0E0B0B0BBBBB0B
        0B0E0B0BBBBBBBBB0B0E0B0BBBBBBBBB0B0E0BB00B000B00BB0E0BBBB0BBB0BB
        BB0E0BBBB0BBB0BBBB0E0BBBB0BBB0BBBB0E0BBBBB000BBBBB0E0BBBBBBBBBBB
        BB0E0BBBBBBBBBBBBB0EE0000000000000EEEEEEEEEEEEEEEEEE}
      OnClick = Export1Click
    end
    object Shape3: TShape
      Left = 260
      Top = 2
      Width = 1
      Height = 28
      Pen.Color = clBtnShadow
    end
    object Shape4: TShape
      Left = 261
      Top = 2
      Width = 1
      Height = 28
      Pen.Color = clBtnHighlight
    end
    object BtnHelp: TSpeedButton
      Left = 265
      Top = 4
      Width = 24
      Height = 24
      Hint = 'Help topics'
      Flat = True
      Glyph.Data = {
        E6000000424DE60000000000000076000000280000000E0000000E0000000100
        0400000000007000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEE000EEEE
        EE00EEEE0BBB0EEEEE00EEEE0BBB0EEEEE00EEEEE000EEEEEE00EEEE0BBB0EEE
        EE00EEEE0BBB0EEEEE00EEEEE0BBB0EEEE00EE000E0BBB0EEE00E0BBB0E0BBB0
        EE00E0BBB0E0BBB0EE00E0BBBB0BBBB0EE00EE0BBBBBBB0EEE00EEE0BBBBB0EE
        EE00EEEE00000EEEEE00}
      OnClick = Helptopics1Click
    end
    object BtnCut: TSpeedButton
      Left = 87
      Top = 4
      Width = 24
      Height = 24
      Hint = 'Cut'
      Flat = True
      Glyph.Data = {
        E6000000424DE60000000000000076000000280000000E0000000E0000000100
        0400000000007000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000000000EE
        EE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF00E
        EE000FFFFFFFF0E0EE000FFFFFFFF0EE0E000000000000EEE0000CCCCCCCC0EE
        E0000000000000EEE000EEEE0EEEEEEEE000EEEEE0EEEEEE0E00EEEEEE0EEEE0
        EE00EEEEEEE0000EEE00}
      OnClick = Cut1Click
    end
    object BtnCopy: TSpeedButton
      Left = 110
      Top = 4
      Width = 24
      Height = 24
      Hint = 'Copy'
      Flat = True
      Glyph.Data = {
        E6000000424DE60000000000000076000000280000000E0000000E0000000100
        0400000000007000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000000000EE
        EE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF00E
        EE000FFFFFFFF0B0EE000FFFFFFFF0BB0E000000000000BBB0000CCCCCCCC0BB
        B0000000000000BBB000EEEE0BBBBBBBB000EEEEE0BBBBBB0E00EEEEEE0BBBB0
        EE00EEEEEEE0000EEE00}
      OnClick = Copy1Click
    end
    object BtnPaste: TSpeedButton
      Left = 133
      Top = 4
      Width = 24
      Height = 24
      Hint = 'Paste'
      Flat = True
      Glyph.Data = {
        E6000000424DE60000000000000076000000280000000E0000000E0000000100
        0400000000007000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEE0000EEEEE
        EE00EE0BBBB0EEEEEE00E0BBBBBB0EEEEE000BBBBBBBB0EEEE000BBBBBBBB000
        00000BBBBBBBB0FFF0000BBBBBBBB0FFF000E0BBBBBB0FFFF000EE0BBBB0FFFF
        F000EEE0000FFFFFF000EEEE0FFFFFFFF000EEEE000000000000EEEE0CCCCCCC
        C000EEEE000000000000}
      OnClick = Paste1Click
    end
    object Shape5: TShape
      Left = 161
      Top = 2
      Width = 1
      Height = 28
      Pen.Color = clBtnShadow
    end
    object Shape6: TShape
      Left = 162
      Top = 2
      Width = 1
      Height = 28
      Pen.Color = clBtnHighlight
    end
    object PnlZoom: TPanel
      Left = 167
      Top = 4
      Width = 89
      Height = 24
      Alignment = taLeftJustify
      Caption = ' Zoom'
      TabOrder = 0
      OnClick = PnlZoomClick
      object CmbZoom: TComboBox
        Left = 36
        Top = 1
        Width = 51
        Height = 21
        TabStop = False
        Style = csDropDownList
        Ctl3D = True
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 0
        OnChange = CmbZoomChange
        Items.Strings = (
          '25%'
          '50%'
          '100%'
          '150%'
          '200%')
      end
    end
  end
  object PnlMain: TPanel
    Left = 0
    Top = 34
    Width = 489
    Height = 399
    BevelOuter = bvLowered
    TabOrder = 2
    object GrdMap: TMapGrid
      Left = 33
      Top = 4
      Width = 361
      Height = 321
      ZoomFactor = 2
      TileWidth = 16
      TileHeight = 16
      Freezed = False
      Grid = False
      DoubleMarkers = False
      PropColors = False
      OnBeforeSelect = GrdMapBeforeSelect
      OnAfterSelect = GrdMapAfterSelect
    end
    object PnlBtn: TPanel
      Left = 4
      Top = 4
      Width = 26
      Height = 174
      TabOrder = 1
      object BtnPenCursor: TSpeedButton
        Left = 2
        Top = 2
        Width = 22
        Height = 22
        Hint = 'Pen'
        GroupIndex = 1
        Down = True
        Flat = True
        Glyph.Data = {
          E6000000424DE60000000000000076000000280000000E0000000E0000000100
          0400000000007000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDD000000
          DD00DDDDD0B0DDDDDD00DDDD0BBB0DDDDD00DDD0BBBBB0DDDD00DDD0000000DD
          DD00DDD0CCCC40DDDD00DDD0CCCC40DDDD00DDD0CCCC40DDDD00DDD0CCCC40DD
          DD00DDD0CCCC40DDDD00DDD0CCCC40DDDD00DDD0CCCC40DDDD00DDD0CCCC40DD
          DD00DDD0000000DDDD00}
        OnClick = Pen1Click
      end
      object BtnFloodFillCursor: TSpeedButton
        Left = 2
        Top = 24
        Width = 22
        Height = 22
        Hint = 'Flood fill'
        GroupIndex = 1
        Flat = True
        Glyph.Data = {
          E6000000424DE60000000000000076000000280000000E0000000E0000000100
          0400000000007000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000000
          DD00D0CCCCCCCC40DD00D0CCCCCCCC40DD00D0CCCCCCCC40DD00D0C0CCCCCC40
          DD00D00B0CCCCC40DD00D00B0CCC0C40DD00D00BB000B000DD00D0BBBBB0B0B0
          DD00D0000000B000DD00DDD0B0D0B0DDDD00DDD0B000B0DDDD00DDDD0BBB0DDD
          DD00DDDDD000DDDDDD00}
        OnClick = Floodfill1Click
      end
      object Bevel1: TBevel
        Left = 2
        Top = 71
        Width = 22
        Height = 82
      end
      object BtnInsRow: TSpeedButton
        Left = 3
        Top = 72
        Width = 20
        Height = 20
        Hint = 'Insert row'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEEEEEE0000EEEEEEEEEEE0BBBB0EEEEEEEEE0BBBBBB0EEEEEEE0BBBBB
          BBB0EEE0000030303030EEE0CCC030303030EEE0CCC0BBBBBBB00000CCC0000B
          BB0E0CCCCCCCCC0BB0EE0CCCCCCCCC000EEE0CCCCCCCCC0EEEEE0000CCC0000E
          EEEEEEE0CCC0EEEEEEEEEEE0CCC0EEEEEEEEEEE00000EEEEEEEE}
        OnClick = Insertrow1Click
      end
      object BtnInsertCol: TSpeedButton
        Left = 3
        Top = 92
        Width = 20
        Height = 20
        Hint = 'Insert column'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEEEEEE0000EEEEEEEEEEE0B33B0EEEEEEEEE0BB00BB0EEEEEEE0BBB33
          BBB0EEE00000BB00BBB0EEE0CCC0BB33BBB0EEE0CCC0BB00BBB00000CCC00003
          BB0E0CCCCCCCCC00B0EE0CCCCCCCCC000EEE0CCCCCCCCC0EEEEE0000CCC0000E
          EEEEEEE0CCC0EEEEEEEEEEE0CCC0EEEEEEEEEEE00000EEEEEEEE}
        OnClick = Insertcolumn1Click
      end
      object BtnDelCol: TSpeedButton
        Left = 3
        Top = 132
        Width = 20
        Height = 20
        Hint = 'Delete column'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEEEEEE0000EEEEEEEEEEE0B33B0EEEEEEEEE0BB00BB0EEEEEEE0BBB33
          BBB0EEEEEE0BBB00BBB0EEEEEE0BBB33BBB0EEEEEE0BBB00BBB0000000000003
          BB0E0CCCCCCCCC00B0EE0CCCCCCCCC000EEE0CCCCCCCCC0EEEEE00000000000E
          EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
        OnClick = Deletecolumn1Click
      end
      object BtnDelRow: TSpeedButton
        Left = 3
        Top = 112
        Width = 20
        Height = 20
        Hint = 'Delete row'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEEEEEE0000EEEEEEEEEEE0BBBB0EEEEEEEEE0BBBBBB0EEEEEEE0BBBBB
          BBB0EEEEEE0030303030EEEEEE0030303030EEEEEE0BBBBBBBB000000000000B
          BB0E0CCCCCCCCC0BB0EE0CCCCCCCCC000EEE0CCCCCCCCC0EEEEE00000000000E
          EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
        OnClick = Deleterow1Click
      end
      object BtnDropperCursor: TSpeedButton
        Left = 2
        Top = 46
        Width = 22
        Height = 22
        Hint = 'Dropper'
        GroupIndex = 1
        Flat = True
        Glyph.Data = {
          E6000000424DE60000000000000076000000280000000E0000000E0000000100
          0400000000007000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDD000DDDD
          DD00DDDDD0B0DDDDDD00DDDD0BBB0DDDDD00DDD0BBBB30DDDD00DDD0BBBB30DD
          DD00DDD0BBBB30DDDD00DDD0BBBB30DDDD00DDD0BBBB30DDDD00DD000000000D
          DD00DD0CCCCC440DDD00DDD04CCC00DDDD00DDD0CCCC40DDDD00DDD0CCCC40DD
          DD00DDDD00000DDDDD00}
        OnClick = Dropper1Click
      end
      object BtnAutoUpdate: TSpeedButton
        Left = 2
        Top = 155
        Width = 22
        Height = 17
        Hint = 'Auto update'
        AllowAllUp = True
        GroupIndex = 99
        Flat = True
        Glyph.Data = {
          96010000424D9601000000000000760000002800000040000000090000000100
          0400000000002001000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDD00DDD
          DDDDDDDDDDD00DDDDDDDDDDDDDD00DDDDDDDDDDDDDD00DDDDDDDDDDDDD0BB0DD
          DDDDDDDDDD0BB0DDDDDDDDDDDD0BB0DDDDDDDDDDDD0BB0DDDDDD0000D0BBBB0D
          DDDD0000D0BBBB0DDDDD0000D0BBBB0DDDDD0000D0BBBB0DDDDD0BBB0BBBBBB0
          DDDD0BBB0BBBBBB0DDDD0BBB0BBBBBB0DDDD0BBB0BBBBBB0DDDDD0BBBBBBBBBB
          0DDDD0BBBBBBBBBB0DDDD0BBBBBBBBBB0DDDD0BBBBBBBBBB0DDDDD0BBBB00BBB
          B0DDDD0BBBB00BBBB0DDDD0BBBB00BBBB0DDDD0BBBB00BBBB0DDDDD0BBBB000B
          BB0DDDD0BBBB000BBB0DDDD0BBBB000BBB0DDDD0BBBB000BBB0DDDDD0BB0DDD0
          0BB0DDDD0BB0DDD00BB0DDDD0BB0DDD00BB0DDDD0BB0DDD00BB0DDDDD00DDDDD
          D000DDDDD00DDDDDD000DDDDD00DDDDDD000DDDDD00DDDDDD000}
        NumGlyphs = 4
        OnClick = Autoupdate1Click
      end
    end
    object PnlInfo: TPanel
      Left = 34
      Top = 336
      Width = 449
      Height = 57
      BorderWidth = 1
      TabOrder = 2
    end
  end
  object MainMenu1: TMainMenu
    Left = 472
    Top = 424
    object File1: TMenuItem
      Caption = '&File'
      OnClick = File1Click
      object Loadmap1: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = Loadmap1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = Savefile1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        OnClick = SaveAs1Click
      end
      object NReopen: TMenuItem
        Caption = '-'
      end
      object Mapproperties1: TMenuItem
        Caption = '&Map properties...'
        OnClick = Mapproperties1Click
      end
      object Tileproperties1: TMenuItem
        Caption = '&Location properties...'
        OnClick = Tileproperties1Click
      end
      object Defaulttileproperties1: TMenuItem
        Caption = '&Default location properties...'
        OnClick = Defaulttileproperties1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Export1: TMenuItem
        Caption = '&Export'
        ShortCut = 16453
        OnClick = Export1Click
      end
      object Exportto1: TMenuItem
        Caption = 'Ex&port to...'
        OnClick = Exportto1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      OnClick = Edit1Click
      object Undo1: TMenuItem
        Caption = '&Undo'
        ShortCut = 16474
        OnClick = Undo1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Caption = 'Cu&t'
        ShortCut = 16472
        OnClick = Cut1Click
      end
      object Copy1: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
        ShortCut = 16470
        OnClick = Paste1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Copyasbitmap1: TMenuItem
        Caption = 'Copy as bit&map'
        ShortCut = 16461
        OnClick = Copyasbitmap1Click
      end
    end
    object Design1: TMenuItem
      Caption = '&Design'
      object Pen1: TMenuItem
        Caption = '&Pen'
        Checked = True
        GroupIndex = 2
        RadioItem = True
        ShortCut = 16464
        OnClick = Pen1Click
      end
      object Floodfill1: TMenuItem
        Caption = '&Flood fill'
        GroupIndex = 2
        RadioItem = True
        ShortCut = 16454
        OnClick = Floodfill1Click
      end
      object Dropper1: TMenuItem
        Caption = 'D&ropper'
        GroupIndex = 2
        RadioItem = True
        ShortCut = 16466
        OnClick = Dropper1Click
      end
      object N6: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object Insertrow1: TMenuItem
        Caption = '&Insert row'
        GroupIndex = 3
        OnClick = Insertrow1Click
      end
      object Insertcolumn1: TMenuItem
        Caption = 'I&nsert column'
        GroupIndex = 3
        OnClick = Insertcolumn1Click
      end
      object Deleterow1: TMenuItem
        Caption = '&Delete row'
        GroupIndex = 3
        OnClick = Deleterow1Click
      end
      object Deletecolumn1: TMenuItem
        Caption = 'D&elete column'
        GroupIndex = 3
        OnClick = Deletecolumn1Click
      end
      object N7: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object Clearmap1: TMenuItem
        Caption = '&Clear map'
        GroupIndex = 3
        OnClick = Clearmap1Click
      end
      object Blockfill1: TMenuItem
        Caption = '&Block fill...'
        GroupIndex = 3
        ShortCut = 16450
        OnClick = Blockfill1Click
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      OnClick = View1Click
      object Zoom1: TMenuItem
        Caption = '&Zoom'
        object N251: TMenuItem
          Caption = '25&%'
          GroupIndex = 1
          RadioItem = True
          OnClick = SetmnuZoom
        end
        object N501: TMenuItem
          Tag = 1
          Caption = '&50%'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = SetmnuZoom
        end
        object N1001: TMenuItem
          Tag = 2
          Caption = '&100%'
          GroupIndex = 1
          RadioItem = True
          OnClick = SetmnuZoom
        end
        object N1501: TMenuItem
          Tag = 3
          Caption = '15&0%'
          GroupIndex = 1
          RadioItem = True
          OnClick = SetmnuZoom
        end
        object N2001: TMenuItem
          Tag = 4
          Caption = '&200%'
          GroupIndex = 1
          RadioItem = True
          OnClick = SetmnuZoom
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Infopanel1: TMenuItem
        Caption = '&Info panel'
        Checked = True
        ShortCut = 16457
        OnClick = Infopanel1Click
      end
      object Grid1: TMenuItem
        Caption = '&Grid'
        ShortCut = 16455
        OnClick = Grid1Click
      end
      object Doublemarkers1: TMenuItem
        Caption = '&Double markers'
        ShortCut = 16452
        OnClick = Doublemarkers1Click
      end
      object Propertycolors1: TMenuItem
        Caption = 'Propert&y colors'
        ShortCut = 16473
        OnClick = Propertycolors1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Autoupdate1: TMenuItem
        Caption = 'Auto &update'
        ShortCut = 16469
        OnClick = Autoupdate1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnuColorset: TMenuItem
        Caption = 'C&olor set'
        object MnuColorsetStandard: TMenuItem
          Caption = 'Gameboy &Pocket'
          GroupIndex = 1
          RadioItem = True
          OnClick = MnuColorsetClick
        end
        object MnuColorsetGameboy: TMenuItem
          Tag = 1
          Caption = '&Gameboy'
          GroupIndex = 1
          RadioItem = True
          OnClick = MnuColorsetClick
        end
        object mnuColorSuper: TMenuItem
          Tag = 3
          Caption = '&Super Gameboy'
          GroupIndex = 1
          RadioItem = True
          OnClick = MnuColorsetClick
        end
        object MnuColorsetColor: TMenuItem
          Tag = 2
          Caption = 'Gameboy &Color'
          GroupIndex = 1
          RadioItem = True
          OnClick = MnuColorsetClick
        end
        object GameboyColorFiltered1: TMenuItem
          Tag = 4
          Caption = 'Gameboy Color (&Filtered)'
          GroupIndex = 1
          RadioItem = True
          OnClick = MnuColorsetClick
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Setbookmark1: TMenuItem
        Caption = '&Set bookmark'
        object mnuSet1: TMenuItem
          Caption = '&1'
          OnClick = SetBookmark
        end
        object mnuSet2: TMenuItem
          Tag = 1
          Caption = '&2'
          OnClick = SetBookmark
        end
        object mnuSet3: TMenuItem
          Tag = 2
          Caption = '&3'
          OnClick = SetBookmark
        end
      end
      object Gotobookmark1: TMenuItem
        Caption = 'Goto &bookmark'
        OnClick = Gotobookmark1Click
        object mnuGoto1: TMenuItem
          Caption = '&1'
          OnClick = GotoBookmark
        end
        object mnuGoto2: TMenuItem
          Tag = 1
          Caption = '&2'
          OnClick = GotoBookmark
        end
        object mnuGoto3: TMenuItem
          Tag = 2
          Caption = '&3'
          OnClick = GotoBookmark
        end
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Helptopics1: TMenuItem
        Caption = '&Help topics'
        OnClick = Helptopics1Click
      end
      object Helpindex1: TMenuItem
        Caption = 'Help &index'
        OnClick = Helpindex1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
  object DlgSave: TSaveDialog
    DefaultExt = 'gbm'
    Filter = 'GBM files (*.gbm)|*.gbm|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn]
    Left = 536
    Top = 424
  end
  object DlgOpen: TOpenDialog
    DefaultExt = 'gbm'
    Filter = 'GBM files (*.gbm)|*.gbm|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 568
    Top = 424
  end
  object MRUList: TMRUFileList
    RemoveOnClick = False
    UseSubmenu = True
    SubmenuName = '&Reopen'
    OnMRUItemClick = MRUListMRUItemClick
    FileMenu = NReopen
    AutoSaveName = 'MyINI.INI'
    AutoSaveKey = 'Recently used files'
    MaxCaptionWidth = 150
    OnCreate = MRUListCreate
    Left = 504
    Top = 424
  end
end
