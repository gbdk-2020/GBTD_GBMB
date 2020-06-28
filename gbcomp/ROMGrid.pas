unit ROMGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShadowCanvas, MainLib;

type
  TROMGrid = class(TCustomControl)
  private
    { Private declarations }
    FShadowCanvas : TShadowCanvas;

    FROMData      : PByteMem;
    FROMSize      : integer;
    FROMPos       : integer;
    FBlockWidth   : integer;
    FBlockHeight  : integer;

  protected
    { Protected declarations }

  public
    { Public declarations }

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;


    property ROMData : PByteMem read FROMData write FROMData;
    property ROMSize : integer read FROMSize write FROMSize;
    property ROMPos  : integer read FROMPos write FROMPos;
    property BlockWidth  : integer read FBlockWidth write FBlockWidth;
    property BlockHeight  : integer read FBlockHeight write FBlockHeight;


  published
    { Published declarations }
  end;

procedure Register;

implementation


const FPocket  : array[0..3] of TColor = (clWhite, clLtGray, clGray, clBlack);


procedure Register;
begin
  RegisterComponents('Gameboy', [TROMGrid]);
end;


constructor TROMGrid.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FShadowCanvas := TShadowCanvas.Create(320,288);

  FBlockWidth := 1;
  FBlockHeight := 1;

end;


destructor TROMGrid.Destroy;
begin
  FShadowCanvas.Free;

  inherited destroy;
end;



procedure TROMGrid.Paint;
var x,y   : integer;
    bx,by : integer;
    i,j,k,l : integer;
    Pos   : integer;
    Data  : array[0..7] of byte;
//    width : integer;
    b     : array[0..1] of byte;
    c     : integer;

begin
  if Assigned(FShadowCanvas) and Assigned(FROMData) then
  begin

    (* main init *)
    Pos := FROMPos;



      for y := 0 to ((Height div (FBlockHeight*8)) div 2)-1 do
        for x := 0 to ((Width div (FBLockWidth*8)) div 2)-1 do
        begin

          (* get data *)

          for by := 0 to FBlockHeight-1 do
            for bx := 0 to FBlockWidth-1 do
            begin

              for l := 0 to 7 do
              begin
                if ( (Pos + 1) < FROMSize) then
                begin
                  b[0] := FROMData[Pos];
                  b[1] := FROMData[Pos+1];

                  for k := 0 to 7 do
                  begin
                    c := ((b[0] and $80) + ((b[1] and $80) shl 1)) shr 7;
      //              FShadowCanvas.SetPixel( (i*8) + k, j, FPocket[c] );
                    FShadowCanvas.RectangleSized( ((x*FBlockWidth*8) + (bx*8) + k)*2,
                                                  ((y*FBlockHeight*8) + (by*8) + l)*2, 2, 2,FPocket[c] );
                    b[0] := b[0] shl 1;
                    b[1] := b[1] shl 1;
                  end;

                end;

                Inc(Pos,2);
              end;

            end;
        end;


    FShadowCanvas.Draw(Canvas.Handle, 0,0 );
  end;

end;

end.
