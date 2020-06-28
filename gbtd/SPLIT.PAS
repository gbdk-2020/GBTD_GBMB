unit split;

interface

uses Windows, Classes, graphics, Clipbrd, gbconst, mainlib,
     GBTDMain;

procedure SplitPaste( TileData : TList; Start, Width, Height, x, y : integer; Order : TSplitOrder;pal : TGBPaletteType; c : TGBColorType );
procedure SplitCopy( TileData : TList; Start, Width, Height, x, y : integer; Order : TSplitOrder;pal : TGBPaletteType; c : TGBColorType );

implementation



procedure SplitCopy( TileData : TList; Start, Width, Height, x, y : integer; Order : TSplitOrder;pal : TGBPaletteType; c : TGBColorType );
var p : PTileType;
    buf : TBitmap;
    i,j,k,l : integer;
begin

  Buf := TBitmap.Create;
  try

    (* init bitmap *)
    Buf.Width := Width * x;
    Buf.Height := Height * y;

    for j := 0 to (Height*y)-1 do
      for i := 0 to (Height*x)-1 do
        Buf.Canvas.Pixels[i,j] := clWhite;

    case Order of
    soLeftRightTopBottom :
    begin
      (* Left to right, top to bottom *)
      for k := 0 to Height-1 do
        for l := 0 to Width-1 do
        begin
          if ((Start + (k* Width) + l) < TileData.Count ) then
          begin
            p := TileData.Items[Start + (k* Width) + l];

            for j := 0 to y-1 do
              for i := 0 to x-1 do
                buf.canvas.pixels[(l*x) + i, (k*y) + j] := IntToColor(p.data[j,i], pal, @c );
          end;
        end;
    end
    else
      begin
        (* top to bottom, Left to right *)
        for k := 0 to Width-1 do
          for l := 0 to Height-1 do
          begin
            if ((Start + (k* Height) + l) < TileData.Count ) then
            begin
              p := TileData.Items[Start + (k* Height) + l];

              for j := 0 to y-1 do
                for i := 0 to x-1 do
                  buf.canvas.pixels[(k*x) + i, (l*y) + j] := IntToColor(p.data[j,i], pal, @c );
            end;
          end;
        end;
      end;

    Clipboard.Assign(Buf);
  finally
    Buf.Free;
  end;

end;



procedure SplitPaste( TileData : TList; Start, Width, Height, x, y : integer; Order : TSplitOrder; pal : TGBPaletteType; c : TGBColorType );
var p : PTileType;
    buf : TBitmap;
    i,j,k,l : integer;
begin

  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    Buf := TBitmap.Create;
    try
      Buf.Assign(Clipboard);

      case Order of
      soLeftRightTopBottom :
        begin
          (* Left to right, top to bottom *)
          for k := 0 to Height-1 do
            for l := 0 to Width-1 do
            begin
              if ((Start + (k* Width) + l) < TileData.Count ) then
              begin
                p := TileData.Items[Start + (k* Width) + l];

                for j := 0 to y-1 do
                  for i := 0 to x-1 do
                    p.data[j,i] := ColorToInt(NormalizeClipColor(buf.canvas.pixels[(l*x) + i, (k*y) + j]), pal, c );
              end;
            end;
        end
      else
        begin
          (* top to bottom, Left to right *)
          for k := 0 to Width-1 do
            for l := 0 to Height-1 do
            begin
              if ((Start + (k* Height) + l) < TileData.Count ) then
              begin
                p := TileData.Items[Start + (k* Height) + l];

                for j := 0 to y-1 do
                  for i := 0 to x-1 do
                    p.data[j,i] := ColorToInt(NormalizeClipColor(buf.canvas.pixels[(k*x) + i, (l*y) + j]), pal, c );
              end;
            end;
        end;
      end;

    finally
      Buf.Free;
      TileExchanger.ListToExchange;
    end;
  end;
end;

end.
