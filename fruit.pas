unit Fruit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Option, Tile;

type
  TFruit = class(TTile)
    private
    public
    StartDisplayTimeLeft: integer; // in ms
    StartMaxX, StartMaxY: integer;
    Kind: string;
    Visible: boolean;
    DisplayTimeLeft: integer; // in ms

    constructor Create(kkind: string; vvisible: boolean; sstartDisplayTimeLeft: integer; sstartMaxX, sstartMaxY: integer; xx, yy : integer; ppic: string);
    procedure Refresh();
    procedure RefreshPosotion();
    procedure Draw(Canvas: TCanvas; Option: TOption);
end;

implementation
  constructor TFruit.Create(kkind: string; vvisible: boolean; sstartDisplayTimeLeft: integer; sstartMaxX, sstartMaxY: integer; xx, yy : integer; ppic: string);
    begin

  inherited Create(xx, yy, ppic);

      Kind := kkind;
      Visible := vvisible;
      StartDisplayTimeLeft := sstartDisplayTimeLeft;
      DisplayTimeLeft := sstartDisplayTimeLeft;
      StartMaxX := sstartMaxX;
      StartMaxY := sstartMaxY;
    end;

  procedure TFruit.Refresh();
    begin
      DisplayTimeLeft := DisplayTimeLeft - 1;
      if(DisplayTimeLeft <= 0) then
        begin
          self.Visible:=true;
          RefreshPosotion();
          DisplayTimeLeft := StartDisplayTimeLeft;
        end;
    end;

  procedure TFruit.RefreshPosotion();
  begin
    X := Random(StartMaxX);
    Y := Random(StartMaxY);
  end;

  procedure TFruit.Draw(Canvas: TCanvas; Option: TOption);
    begin
      if(Visible = true) then
      begin
        Canvas.Draw(X * Option.TileWidth,
          Y * Option.TileHeight,
          Pic);
      end;
    end;
  end.

end.

