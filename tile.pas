unit Tile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TTile = class
    private
    public
    X, Y: Integer;
    Pic:  TPortableNetworkGraphic;
    constructor Create(xx, yy: integer; ppic: string);
end;

implementation

  constructor TTile.Create(xx, yy: integer; ppic: string);
  begin
    X := xx;
    Y := yy;
    Pic := TPortableNetworkGraphic.Create();
    Pic.LoadFromFile(ppic);
  end;


end.

