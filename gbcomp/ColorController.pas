unit ColorController;

interface

uses Classes, Forms, Windows, Graphics, gbConst, ShadowCanvas, COntrols;

const MODECNT = 4;

  type
    TOnColorSelectEvent = procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState; clr: Integer) of object;

  type TGBColorController = class
    private

      FColorMode    : TGBColorMode;
      FPalette      : TGBPaletteType;
      FCGBSets      : TGBColorSets;
      FSGBSets      : TGBColorSets;

      FColorSets    : array[0..MODECNT] of TGBPropColorSets;//TGBColorSets;
      FSelColorSets : array[0..MODECNT] of TGBPropColorSets;//TGBColorSets;

      FShadowColorSets    : array[0..MODECNT] of TGBPropColorSets;//TGBColorSets;
      FShadowSelColorSets : array[0..MODECNT] of TGBPropColorSets;//TGBColorSets;

      FColorSetIndex : integer;
//      Freezed        : boolean;

//      procedure SetColorMode( i : TGBColorMode);
      procedure SetPalette( i : TGBPaletteType);
      procedure SetCGBSets( i : TGBColorSets);
      procedure SetSGBSets( i : TGBColorSets);
      function GetCurColorSets : PGBPropColorSets;
      function GetCurSelColorSets : PGBPropColorSets;
      function GetCurShadowColorSets : PGBPropColorSets;
      function GetCurShadowSelColorSets : PGBPropColorSets;
      function GetCGBSelected : boolean;

      procedure Rebuild( Normal, Color : boolean );

//      function TranslateColor( clr : TColor) : TColor;



    public

      property ColorMode : TGBColorMode read FColorMode write FColorMode;
      property Palette : TGBPaletteType read FPalette write SetPalette;
      property CGBSets : TGBColorSets read FCGBSets write SetCGBSets;
      property SGBSets : TGBColorSets read FSGBSets write SetSGBSets;
      property ColorSetIndex : integer read FColorSetIndex write FColorSetIndex;

      property CurColorSets : PGBPropColorSets read GetCurColorSets;
      property CurSelColorSets : PGBPropColorSets read GetCurSelColorSets;
      property CurShadowColorSets : PGBPropColorSets read GetCurShadowColorSets;
      property CurShadowSelColorSets : PGBPropColorSets read GetCurShadowSelColorSets;

      property GBCSelected : boolean read GetCGBSelected;

      constructor Create( AClrForm : TCustomForm);
  end;

function TranslateToGBCColor( clr : TColor) : TColor;


var GBColorController : TGBColorController;

implementation

const FPocket  : TGBColorType = (clWhite, clLtGray, clGray, clBlack);
      FBlack   : TGBColorType = (clBlack, clBlack, clBlack, clBlack);
var   FGameboy : TGBColorType;


{-----------------}
type rgbtype = array[1..3] of byte;  { three byte array (R,G,B) }
{-----------------}
function TranslateToGBCColor( clr : TColor) : TColor;

//procedure translate(var rgb : rgbtype);
const intensity : array[0..$1f] of byte = (
 $00,$10,$20,$30,$40,$50,$5e,$6c,$7a,$88,$94,$a0,$ae,$b7,$bf,$c6,
 $ce,$d3,$d9,$df,$e3,$e7,$eb,$ef,$f3,$f6,$f9,$fb,$fd,$fe,$ff,$ff);

const influence : array[1..3,1..3] of byte = ((16,4,4),(8,16,8),(0,8,16));
var
  m   : array[1..3,1..3] of byte;
  i,j : byte;
  rgb : rgbtype;
  c : tColor;
begin
  rgb[1] := (clr and $0000FF) shr (0+3);
  rgb[2] := (clr and $00FF00) shr (8+3);
  rgb[3] := (clr and $FF0000) shr (16+3);

  for i:=1 to 3 do                
    for j:=1 to 3 do
      m[i,j] := (intensity[rgb[i]] * influence[i,j]) shr 5;

  for i:=1 to 3 do begin
    if m[1,i]>m[2,i] then begin j:=m[1,i]; m[1,i]:=m[2,i]; m[2,i]:=j; end;
    if m[2,i]>m[3,i] then begin j:=m[2,i]; m[2,i]:=m[3,i]; m[3,i]:=j; end;
    if m[1,i]>m[2,i] then begin j:=m[1,i]; m[1,i]:=m[2,i]; m[2,i]:=j; end;
    rgb[i]:=(((m[1,i]+m[2,i]*2+m[3,i]*4)*5) shr 4)+32;
  end;

//  Result := TColor( rgb[1] + (rgb[2] shl 8) + (rgb[3] shl 16) );
  c := TColor( rgb[1] + (rgb[2] shl 8) + (rgb[3] shl 16) );
  Result := c;
end;
{-----------------
var a:rgbtype;
begin
 writeln('color gameboy RGB translation procedure (c) martin korth');
 a[1]:=$1f; a[2]:=$f; a[3]:=$7;
 writeln(' CGB   Red:',a[1]:3,' - Green:',a[2]:3,' - Blue:',a[3]:3);
 translate(a);
 writeln(' VGA   Red:',a[1]:3,' - Green:',a[2]:3,' - Blue:',a[3]:3);
end.
{-----------------}



procedure GetRedColorSet( const Base : TGBColorSets; Sel : PGBColorSets);
var r,g,b : integer;
    i,j     : integer;
begin
  for j := 0 to 7 do
    for i := 0 to 3 do
    begin
      b := (Base[j,i] and $FF0000) shr 16;
      g := (Base[j,i] and $FF00) shr 8;
      r := (Base[j,i] and $FF);

      r := (r div 2) * 3;
      g := (g div 3) * 2;
      b := (b div 3) * 2;
      if (r > $FF) then r := $FF;

      Sel[j,i] := ( b shl 16) + (g shl 8) + r;
    end;
end;


procedure GetGreenColorSet( const Base : TGBColorSets; Sel : PGBColorSets);
var r,g,b : integer;
    i,j   : integer;
begin
  for j := 0 to 7 do
    for i := 0 to 3 do
    begin
      b := (Base[j,i] and $FF0000) shr 16;
      g := (Base[j,i] and $FF00) shr 8;
      r := (Base[j,i] and $FF);

      r := (r div 3) * 2;
      g := (g div 2) * 3;
      b := (b div 3) * 2;
      if (g > $FF) then g := $FF;

      Sel[j,i] := ( b shl 16) + (g shl 8) + r;
    end;
end;


procedure GetSelColorSet( const Base : TGBPropColorSets; Sel : PGBPropColorSets);
var i,j,k : integer;
var r,g,b : integer;
begin
  for k := 0 to 2 do
    for j := 0 to 7 do
      for i := 0 to 3 do
      begin
        b := (Base[k,j,i] and $FF0000) shr 16;
        g := (Base[k,j,i] and $FF00) shr 8;
        r := (Base[k,j,i] and $FF);

        r := (r div 4) * 2;
        g := (g div 4) * 2;
        b := (b div 2) * 4;
        if (b > $FF) then b := $FF;

        Sel[k,j,i] := ( b shl 16) + (g shl 8) + r;
      end;

end;


function DetermineColor( AClrForm : TCustomForm; c : TColor ): TColor;
begin
  Result := GetNearestColor( AClrForm.Canvas.Handle, c );
  if (DWORD(Result) =  CLR_INVALID) then Result := c;
end;


constructor TGBColorController.Create( AClrForm : TCustomForm);
var i : integer;
begin
  inherited Create;

  (* init gameboy colors *)
  FGameboy[0] := DetermineColor( AClrForm, $29efe0 );
  FGameboy[1] := DetermineColor( AClrForm, $42b939 );
  FGameboy[2] := DetermineColor( AClrForm, $317520 );
  FGameboy[3] := DetermineColor( AClrForm, $2e3907 );

  (* init CGB *)
  for i := 0 to 7 do FCGBSets[i] := FGameboy;

  (* init SGB *)
  for i := 0 to 3 do FSGBSets[i] := FGameboy;
  for i := 4 to 7 do FSGBSets[i] := FBlack;

  (* init palette *)
  for i := 0 to 3 do FPalette[i] := i;

  (* refresh buffers *)
  Rebuild( True, True );
end;


procedure TGBColorController.SetPalette( i : TGBPaletteType);
begin
  FPalette := i;
  ReBuild( True, True );
end;


procedure TGBColorController.SetCGBSets( i : TGBColorSets);
begin
  FCGBSets := i;
  ReBuild( False, True );
end;


procedure TGBColorController.SetSGBSets( i : TGBColorSets);
begin
  FSGBSets := i;
  ReBuild( False, True );
end;

function TGBColorController.GetCGBSelected : boolean;
begin
  result := ((FColorMode = gbcmGBC) or (FColorMode = gbcmGBCFiltered));
end;

function TGBColorController.GetCurColorSets : PGBPropColorSets;
begin
  Result := @FColorSets[integer(FColorMode)];
end;

function TGBColorController.GetCurSelColorSets : PGBPropColorSets;
begin
  Result := @FSelColorSets[integer(FColorMode)];
end;

function TGBColorController.GetCurShadowColorSets : PGBPropColorSets;
begin
  Result := @FShadowColorSets[integer(FColorMode)];
end;

function TGBColorController.GetCurShadowSelColorSets : PGBPropColorSets;
begin
  Result := @FShadowSelColorSets[integer(FColorMode)];
end;


procedure TGBColorController.Rebuild( Normal, Color : boolean );
var i,j : integer;

  procedure GetShadow( const Base : TGBPropColorSets; Res : PGBPropColorSets);
  var i,j,k : integer;
  begin
    for k := 0 to 2 do
      for i := 0 to 7 do
        for j := 0 to 3 do
          Res[k,i,j] := GetShadowColor( Base[k,i,j] );
  end;


  procedure TransColorSets ( var inS, outS : TGBColorSets);
//  const Base : TGBPropColorSets; Res : PGBPropColorSets);
  var i,j: integer;
//      k : integer;
  begin
      for i := 0 to 7 do
        for j := 0 to 3 do
//        begin
          OutS[i,j] := TranslateToGBCColor( InS[i,j] );

//          k := k + 1;
//        end;

  end;



begin

  if (Normal) then
  begin
    (* Pocket *)
    for i := 0 to 3 do FColorSets[0,0,0,i] := FPocket[FPalette[i]];
    for i := 1 to 7 do FColorSets[0,0,i] := FColorSets[0,0,0];

    GetRedColorSet( FColorSets[0,0], @FColorSets[0,1] );
    GetGreenColorSet( FColorSets[0,0], @FColorSets[0,2] );

    (* Gameboy *)
    for i := 0 to 3 do FColorSets[1,0,0,i] := FGameboy[FPalette[i]];
    for i := 1 to 7 do FColorSets[1,0,i] := FColorSets[1,0,0];

    GetRedColorSet( FColorSets[1,0], @FColorSets[1,1] );
    GetGreenColorSet( FColorSets[1,0], @FColorSets[1,2] );

    (* selected versions *)
    GetSelColorSet( FColorSets[0], @FSelColorSets[0] );
    GetSelColorSet( FColorSets[1], @FSelColorSets[1] );

    (* shadow versions *)
    GetShadow( FColorSets[0], @FShadowColorSets[0] );
    GetShadow( FSelColorSets[0], @FShadowSelColorSets[0] );
    GetShadow( FColorSets[1], @FShadowColorSets[1] );
    GetShadow( FSelColorSets[1], @FShadowSelColorSets[1] );

  end;

  if (Color ) then
  begin
    (* GBC *)
    FColorSets[2,0] := FCGBSets;
    GetRedColorSet( FColorSets[2,0], @FColorSets[2,1] );
    GetGreenColorSet( FColorSets[2,0], @FColorSets[2,2] );

    (* SGB *)
    for i := 0 to 7 do
      for j := 0 to 3 do
        FColorSets[3,0,i,j] := FSGBSets[i,FPalette[j]];

    GetRedColorSet( FColorSets[3,0], @FColorSets[3,1] );
    GetGreenColorSet( FColorSets[3,0], @FColorSets[3,2] );

    (* GBC Filtered *)
    TransColorSets( FCGBSets, FColorSets[4,0]);
    GetRedColorSet( FColorSets[4,0], @FColorSets[4,1] );
    GetGreenColorSet( FColorSets[4,0], @FColorSets[4,2] );


    (* selected versions *)
    GetSelColorSet( FColorSets[2], @FSelColorSets[2] );
    GetSelColorSet( FColorSets[3], @FSelColorSets[3] );
    GetSelColorSet( FColorSets[4], @FSelColorSets[4] );

    (* shadow versions *)
    GetShadow( FColorSets[2], @FShadowColorSets[2] );
    GetShadow( FSelColorSets[2], @FShadowSelColorSets[2] );
    GetShadow( FColorSets[3], @FShadowColorSets[3] );
    GetShadow( FSelColorSets[3], @FShadowSelColorSets[3] );
    GetShadow( FColorSets[4], @FShadowColorSets[4] );
    GetShadow( FSelColorSets[4], @FShadowSelColorSets[4] );

  end;
end;


end.
