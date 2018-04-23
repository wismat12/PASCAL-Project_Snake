unit Wall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Tile, Option;

type
  TWall = class(TTile)
    private
    public
      procedure Draw(Canvas: TCanvas; Option: TOption);
end;

implementation

procedure TWall.Draw(Canvas: TCanvas; Option: TOption);
  begin
      Canvas.Draw(X * Option.TileWidth,
        Y * Option.TileHeight,
        Pic);
  end;
end.

