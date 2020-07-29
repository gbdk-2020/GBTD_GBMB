object FrmTile: TFrmTile
  Left = 344
  Top = 189
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Gameboy Tile Designer'
  ClientHeight = 324
  ClientWidth = 485
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
  Position = poDefaultPosOnly
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PnlPixel: TPanel
    Left = 0
    Top = 40
    Width = 393
    Height = 265
    BevelOuter = bvLowered
    TabOrder = 0
    object ShpOne: TShape
      Left = 327
      Top = 7
      Width = 50
      Height = 50
    end
    object ShpMult: TShape
      Left = 328
      Top = 72
      Width = 49
      Height = 49
    end
    object ShwOne: TGBShowCase
      Left = 272
      Top = 8
      Width = 48
      Height = 48
      TileSize = gbts16x16
      TileCount = 1
      Freezed = False
    end
    object GrdPixel: TGBPixelGrid
      Left = 33
      Top = 4
      Width = 225
      Height = 225
      TileSize = gbts16x16
      PixelSize = 14
      GridLines = True
      NibbleMarkers = False
      Freezed = False
      AfterPaint = GrdPixelAfterPaint
      OnPixelClick = GrdPixelPixelClick
    end
    object ShwMult: TGBShowCase
      Left = 272
      Top = 80
      Width = 48
      Height = 48
      TileSize = gbts16x16
      TileCount = 1
      Freezed = False
    end
    object PnlEdit: TPanel
      Left = 50
      Top = 235
      Width = 191
      Height = 26
      TabOrder = 3
      object Panel1: TPanel
        Left = 2
        Top = 2
        Width = 36
        Height = 22
        BevelOuter = bvLowered
        TabOrder = 0
        object BtnLeftClick: TGBColorButton
          Left = 15
          Top = 1
          Width = 20
          Height = 20
          AllowAllUp = True
          Caption = '0'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpFixed
          Font.Style = []
          ParentFont = False
          Color = clWhite
        end
        object Label1: TLabel
          Left = 5
          Top = 5
          Width = 6
          Height = 13
          Caption = 'L'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpFixed
          Font.Style = []
          ParentFont = False
        end
      end
      object Panel2: TPanel
        Left = 39
        Top = 2
        Width = 36
        Height = 22
        BevelOuter = bvLowered
        TabOrder = 1
        object Label2: TLabel
          Left = 5
          Top = 5
          Width = 8
          Height = 13
          Caption = 'R'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpFixed
          Font.Style = []
          ParentFont = False
        end
        object BtnRightClick: TGBColorButton
          Left = 15
          Top = 1
          Width = 20
          Height = 20
          Caption = '1'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpFixed
          Font.Style = []
          ParentFont = False
          Color = clGray
        end
      end
      object SbLColor: TSpinButton
        Left = 79
        Top = 3
        Width = 16
        Height = 20
        DownGlyph.Data = {
          DE000000424DDE00000000000000360000002800000009000000060000000100
          180000000000A800000000000000000000000000000000000000008484008484
          0084840084840084840084840084840084840084840000848400848400848400
          8484000000008484008484008484008484000084840084840084840000000000
          0000000000848400848400848400008484008484000000000000000000000000
          0000000084840084840000848400000000000000000000000000000000000000
          0000008484000084840084840084840084840084840084840084840084840084
          8400}
        TabOrder = 2
        UpGlyph.Data = {
          DE000000424DDE00000000000000360000002800000009000000060000000100
          180000000000A800000000000000000000000000000000000000008484008484
          0084840084840084840084840084840084840084840000848400000000000000
          0000000000000000000000000000008484000084840084840000000000000000
          0000000000000000848400848400008484008484008484000000000000000000
          0084840084840084840000848400848400848400848400000000848400848400
          8484008484000084840084840084840084840084840084840084840084840084
          8400}
        OnDownClick = SbLColorDownClick
        OnUpClick = SbLColorUpClick
      end
      object GBColorCombo: TGBColorCombo
        Left = 94
        Top = 2
        Width = 95
        Height = 22
        SelColor = 0
        OnColorSelect = GBColorComboColorSelect
        AddDefault = False
        OnChange = GBColorComboChange
        ItemIndex = -1
        ItemHeight = 16
      end
    end
    object PnlTools: TPanel
      Left = 4
      Top = 4
      Width = 26
      Height = 217
      TabOrder = 4
      object Bevel1: TBevel
        Left = 2
        Top = 49
        Width = 22
        Height = 82
      end
      object Bevel2: TBevel
        Left = 2
        Top = 134
        Width = 22
        Height = 62
      end
      object BtnAutoUpdate: TSpeedButton
        Left = 2
        Top = 200
        Width = 22
        Height = 15
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
      object BtnVertical: TSpeedButton
        Left = 3
        Top = 135
        Width = 20
        Height = 20
        Hint = 'Flip vertically'
        Flat = True
        Glyph.Data = {
          E6000000424DE60000000000000076000000280000000E0000000E0000000100
          0400000000007000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EE00EEEEE0000EEEEE00EEEEE0BB0EEEEE00EE00E0BB0E00EE00EE0C00BB00C0
          EE00000CC0BB0CC000000CCCC0BB0CCCC0000CCCC0BB0CCCC000000CC0BB0CC0
          0000EE0C00BB00C0EE00EE00E0BB0E00EE00EEEEE0BB0EEEEE00EEEEE0000EEE
          EE00EEEEEEEEEEEEEE00}
        OnClick = BtnVerticalClick
      end
      object BtnHorizontal: TSpeedButton
        Left = 3
        Top = 155
        Width = 20
        Height = 20
        Hint = 'Flip horizontally'
        Flat = True
        Glyph.Data = {
          E6000000424DE60000000000000076000000280000000E0000000E0000000100
          0400000000007000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEE0000EEE
          EE00EEEEE0CC0EEEEE00EEE000CC000EEE00EEE0CCCCCC0EEE00EEEE0CCCC0EE
          EE00E000000000000E00E0BBBBBBBBBB0E00E0BBBBBBBBBB0E00E00000000000
          0E00EEEE0CCCC0EEEE00EEE0CCCCCC0EEE00EEE000CC000EEE00EEEEE0CC0EEE
          EE00EEEEE0000EEEEE00}
        OnClick = BtnHorizontalClick
      end
      object BtnRotate: TSpeedButton
        Left = 3
        Top = 175
        Width = 20
        Height = 20
        Hint = 'Rotate clockwise'
        Flat = True
        Glyph.Data = {
          E6000000424DE60000000000000076000000280000000E0000000E0000000100
          0400000000007000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEE0000000
          EE00EEEEE0CCCCC0EE00EEEEE0CCCC0EEE00000000CCCCC0EE000BB3B0CCCCCC
          0E000BB3B0C0CCCCC000033330030CCCC0000BB3BB3B0CCCC0000BB3BB3B0CCC
          C00003333333000000000BB3BB3BB0EEEE000BB3BB3BB0EEEE000000000000EE
          EE00EEEEEEEEEEEEEE00}
        OnClick = BtnRotateClick
      end
      object BtnScrollUp: TSpeedButton
        Left = 3
        Top = 50
        Width = 20
        Height = 20
        Hint = 'Scroll up'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEE0000000000EEEEEE0BB3BB3BB0EEEEEE0BB3BB3BB0EEEEEE0333333
          330EEEEEE0BB3BB3BB0EEEEEE0BB3BB3BB0EEE0000003333330EEE0CCCC03BB3
          BB0EEE0CCCC03BB3BB0E000CCCC00000000E0CCCCCCCC0EEEEEEE0CCCCCC0EEE
          EEEEEE0CCCC0EEEEEEEEEEE0CC0EEEEEEEEEEEEE00EEEEEEEEEE}
        OnClick = BtnScrollUpClick
      end
      object BtnScrollLeft: TSpeedButton
        Left = 3
        Top = 70
        Width = 20
        Height = 20
        Hint = 'Scroll left'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEE0000000000EEEEEE0BB3BB3BB0EEEEEE0BB3BB3BB0EEEEEE0333333
          330EEEEEE0BB3BB3BB0EEEEE00BB3BB3BB0EEEE0C0333333330EEE0CC0000BB3
          BB0EE0CCCCCC0BB3BB0E0CCCCCCC0000000E0CCCCCCC0EEEEEEEE0CCCCCC0EEE
          EEEEEE0CC0000EEEEEEEEEE0C0EEEEEEEEEEEEEE00EEEEEEEEEE}
        OnClick = BtnScrollLeftClick
      end
      object BtnScrollRight: TSpeedButton
        Left = 3
        Top = 90
        Width = 20
        Height = 20
        Hint = 'Scroll right'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEE0000000000EEEEEE0BB3BB3BB0EEEEEE0BB3BB3BB0EEEEEE0333333
          330EEEEEE0BB3BB3BB0EEEE000BB3BB3BB0EEEE0C0333333330E0000CC073BB3
          BB0E0CCCCCC03BB3BB0E0CCCCCCC0000000E0CCCCCCC0EEEEEEE0CCCCCC0EEEE
          EEEE0000CC0EEEEEEEEEEEE0C0EEEEEEEEEEEEE00EEEEEEEEEEE}
        OnClick = BtnScrollRightClick
      end
      object BtnScrollDown: TSpeedButton
        Left = 3
        Top = 110
        Width = 20
        Height = 20
        Hint = 'Scroll down'
        Flat = True
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
          EEEEEEEEE0000000000EEEEEE0BB3BB3BB0EEEEEE0BB3BB3BB0EEEEEE0333333
          330EEEEEE0BB3BB3BB0EEEEEE0BB3BB3BB0EEEEE00333333330EEEE0CC073BB3
          BB0EEE0CCCC03BB3BB0EE0CCCCCC0000000E0CCCCCCCC0EEEEEE000CCCC000EE
          EEEEEE0CCCC0EEEEEEEEEE0CCCC0EEEEEEEEEE000000EEEEEEEE}
        OnClick = BtnScrollDownClick
      end
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
        OnClick = BtnPenCursorClick
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
        OnClick = BtnFloodFillCursorClick
      end
    end
  end
  object LstTiles: TGBTileList
    Left = 400
    Top = 40
    Width = 74
    Height = 265
    TileSize = gbts16x16
    Freezed = False
    TileCount = 8
    SelTile = 0
    OnAfterSelect = LstTilesAfterSelect
    TabStop = True
    TabOrder = 1
  end
  object PnlBtn: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    OnResize = PnlBtnResize
    object LneBottom: TShape
      Left = 0
      Top = 0
      Width = 481
      Height = 1
      Pen.Color = clBtnShadow
    end
    object LneTop: TShape
      Left = -1
      Top = 1
      Width = 481
      Height = 1
      Pen.Color = clHighlightText
    end
    object LneBtmShdw: TShape
      Left = 0
      Top = 30
      Width = 481
      Height = 1
      Pen.Color = clBtnShadow
    end
    object LneBtmHgh: TShape
      Left = 0
      Top = 31
      Width = 481
      Height = 1
      Pen.Color = clBtnHighlight
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
    object Shape3: TShape
      Left = 162
      Top = 2
      Width = 1
      Height = 28
      Pen.Color = clBtnHighlight
    end
    object Shape4: TShape
      Left = 161
      Top = 2
      Width = 1
      Height = 28
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
      OnClick = Open1Click
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
      OnClick = Save1Click
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
        EE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF000
        00000FFFFFFFF0EEE0000FFFFFFFF0EEE0000000000000EEE0000CCCCCCCC0EE
        E0000000000000EEE000EEEE0EEEEEEEE000EEEE0EEEEEEEE000EEEE0EEEEEEE
        E000EEEE000000000000}
      OnClick = CutTile1Click
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
        EE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF0EEEE000FFFFFFFF000
        00000FFFFFFFF03BB0000FFFFFFFF03BB00000000000003330000CCCCCCCC03B
        B00000000000003BB000EEEE033333333000EEEE0BB3BB3BB000EEEE0BB3BB3B
        B000EEEE000000000000}
      OnClick = BtnCopyClick
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
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00E000000000EE
        EE000BB3BB3BB0EEEE000BB3BB3BB0EEEE000333333330EEEE000BB3BB3BB000
        00000BB3BB3BB0FFF0000333333330FFF0000BB3BB3BB0FFF0000BB3BB3BB0FF
        F0000000000000FFF000EEEE0FFFFFFFF000EEEE000000000000EEEE0CCCCCCC
        C000EEEE000000000000}
      OnClick = BtnPasteClick
    end
    object BtnHelp: TSpeedButton
      Left = 167
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
  end
  object MainMenu1: TMainMenu
    Left = 416
    Top = 272
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        OnClick = SaveAs1Click
      end
      object NReopen: TMenuItem
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
      object Importfrom1: TMenuItem
        Caption = '&Import from...'
        OnClick = Importfrom1Click
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
      object Undo1: TMenuItem
        Caption = '&Undo'
        ShortCut = 16474
        OnClick = Undo1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object CutTile1: TMenuItem
        Caption = 'Cu&t tile'
        ShortCut = 16472
        OnClick = CutTile1Click
      end
      object Copytile1: TMenuItem
        Caption = '&Copy tile'
        ShortCut = 16451
        OnClick = BtnCopyClick
      end
      object Pastetile1: TMenuItem
        Caption = '&Paste tile'
        ShortCut = 16470
        OnClick = BtnPasteClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object SplitCopy1: TMenuItem
        Caption = 'Split C&opy'
        OnClick = SplitCopy1Click
      end
      object SplitPaste1: TMenuItem
        Caption = 'Split P&aste'
        OnClick = SplitPaste1Click
      end
      object SplitOptions: TMenuItem
        Caption = 'Sp&lit options...'
        ShortCut = 16460
        OnClick = SplitOptionsClick
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
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object FlipVertical1: TMenuItem
        Caption = 'Flip Verticall&y'
        GroupIndex = 3
        ShortCut = 16473
        OnClick = BtnVerticalClick
      end
      object FlipHorizontal1: TMenuItem
        Caption = 'Flip &Horizontally'
        GroupIndex = 3
        ShortCut = 16456
        OnClick = FlipHorizontal1Click
      end
      object Rotateclockwise1: TMenuItem
        Caption = '&Rotate clockwise'
        GroupIndex = 3
        ShortCut = 16466
        OnClick = BtnRotateClick
      end
      object N5: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object Scrolltileleft1: TMenuItem
        Caption = 'Scroll &Left'
        GroupIndex = 3
        OnClick = BtnScrollLeftClick
      end
      object Scrolltileright1: TMenuItem
        Caption = 'Scroll Ri&ght'
        GroupIndex = 3
        OnClick = BtnScrollRightClick
      end
      object Scrolltileup1: TMenuItem
        Caption = 'Scroll &Up'
        GroupIndex = 3
        OnClick = BtnScrollUpClick
      end
      object Scrolltiledown1: TMenuItem
        Caption = 'Scroll &Down'
        GroupIndex = 3
        OnClick = BtnScrollDownClick
      end
      object N6: TMenuItem
        Caption = '-'
        GroupIndex = 3
      end
      object Cleartiles1: TMenuItem
        Caption = '&Clear tiles'
        GroupIndex = 3
        OnClick = Cleartiles1Click
      end
      object Flipcolors1: TMenuItem
        Caption = 'Fl&ip colors'
        GroupIndex = 3
        ShortCut = 16457
        OnClick = Flipcolors1Click
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      OnClick = View1Click
      object TileSize1: TMenuItem
        Caption = '&Tile size'
        object N8x81: TMenuItem
          Caption = '&8 x 8'
          RadioItem = True
          OnClick = N8x81Click
        end
        object N8x161: TMenuItem
          Caption = '8 x &16'
          RadioItem = True
          OnClick = N8x161Click
        end
        object N16x161: TMenuItem
          Caption = '1&6 x 16'
          Checked = True
          RadioItem = True
          OnClick = N16x161Click
        end
        object N32x321: TMenuItem
          Caption = '&32 x 32'
          RadioItem = True
          OnClick = N32x321Click
        end
      end
      object Tilecount1: TMenuItem
        Caption = 'Tile &count...'
        OnClick = Tilecount1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Simple1: TMenuItem
        Caption = 'Si&mple'
        ShortCut = 16461
        OnClick = Simple1Click
      end
      object Grid1: TMenuItem
        Caption = '&Grid'
        Checked = True
        ShortCut = 16455
        OnClick = Grid1Click
      end
      object Nibblemarkers1: TMenuItem
        Caption = '&Nibble markers'
        ShortCut = 16462
        OnClick = Nibblemarkers1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Autoupdate1: TMenuItem
        Caption = 'Auto &update'
        ShortCut = 16469
        OnClick = Autoupdate1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mnuColorset: TMenuItem
        Caption = 'C&olor set'
        object mnuColorSetStandard: TMenuItem
          Caption = 'Gameboy &Pocket'
          RadioItem = True
          OnClick = ColorSetClick
        end
        object mnuColorSetGameboy: TMenuItem
          Tag = 1
          Caption = '&Gameboy'
          RadioItem = True
          OnClick = ColorSetClick
        end
        object mnuColorsetSuper: TMenuItem
          Tag = 3
          Caption = '&Super Gameboy'
          RadioItem = True
          OnClick = ColorSetClick
        end
        object mnuColorsetColor: TMenuItem
          Tag = 2
          Caption = 'Gameboy &Color'
          RadioItem = True
          OnClick = ColorSetClick
        end
        object GameboyColorFiltered1: TMenuItem
          Tag = 4
          Caption = 'Gameboy Color (&Filtered)'
          RadioItem = True
          OnClick = ColorSetClick
        end
      end
      object Palettes1: TMenuItem
        Caption = 'P&alettes...'
        ShortCut = 16449
        OnClick = Palettes1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Setbookmark1: TMenuItem
        Caption = '&Set bookmark'
        object mnuSet1: TMenuItem
          Caption = '&1'
          OnClick = SetBookMark
        end
        object mnuSet2: TMenuItem
          Tag = 1
          Caption = '&2'
          OnClick = SetBookMark
        end
        object mnuSet3: TMenuItem
          Tag = 2
          Caption = '&3'
          OnClick = SetBookMark
        end
      end
      object Gotobookmark1: TMenuItem
        Caption = 'Goto &bookmark'
        OnClick = Gotobookmark1Click
        object mnuGoto1: TMenuItem
          Caption = '&1'
          OnClick = GotoBookMark
        end
        object mnuGoto2: TMenuItem
          Tag = 1
          Caption = '&2'
          OnClick = GotoBookMark
        end
        object mnuGoto3: TMenuItem
          Tag = 2
          Caption = '&3'
          OnClick = GotoBookMark
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
      object N7: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
  object DlgSave: TSaveDialog
    DefaultExt = 'gbr'
    Filter = 'GBR files (*.gbr)|*.gbr|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn]
    Left = 448
    Top = 272
  end
  object DlgOpen: TOpenDialog
    DefaultExt = 'gbr'
    Filter = 'GBR files (*.gbr)|*.gbr|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 384
    Top = 272
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
    Left = 352
    Top = 272
  end
end
